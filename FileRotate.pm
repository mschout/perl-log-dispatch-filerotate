package Log::Dispatch::FileRotate;

require 5.005;
use strict;

use Log::Dispatch::Output;

use base qw( Log::Dispatch::Output );

use Log::Dispatch::File;   # We are a wrapper around Log::Dispatch::File

use Date::Manip;  # For time based recurring rotations

use Params::Validate qw(validate SCALAR BOOLEAN);
Params::Validate::validation_options( allow_extra => 1 );

use vars qw[ $VERSION ];

$VERSION = sprintf "%d.%02d", q$Revision: 1.05 $ =~ /: (\d+)\.(\d+)/;

sub new
{
    my $proto = shift;
    my $class = ref $proto || $proto;

    my %p = @_;

    my $self = bless {}, $class;

	$self->{'debug'} = 0;
    $self->_basic_init(%p);
    $self->{'LDF'} =  Log::Dispatch::File->new(%p);  # Our log

	# Keep a copy of interesting stuff as well
	$self->{params} = \%p;

	# Turn ON/OFF debugging as required
	$p{'DEBUG'} ? $self->debug(1) : $self->debug(0);
	
	# Size defaults to 10meg in all failure modes, hopefully
	my $ten_meg = 1024*1024*10;
	my $two_gig = 1024*1024*1024*2;
	my $size = $p{size};
	$size = $ten_meg unless $size =~ /^\d+$/ && $size < $two_gig && $size > 0;
	$self->{size} = $size;

	# Max number of files defaults to 1. No limit enforced here. Only
	# positive whole numbers allowed
	$self->{max}  = $p{max};
	$self->{max}  = 1 unless $self->{max} =~ /^\d+$/ && $self->{max} ;

	# Get access to our Lock file
	my $lfh = do {local *LFH; *LFH;};
	my $name = $self->{params}->{filename};
	my ($dir,$f) = $name =~ m{^(.*/)(.*)$};
	$f = $name unless $f;

	my $lockfile = $dir.".".$f.".LCK";
	warn "Lock file is $lockfile\n" if $self->{'debug'};
	open $lfh ,">>$lockfile" or die "Can't open $lockfile for locking: $!";
	$self->{lfh} = $lfh;
	$self->{'lf'} = $lockfile;

	# Have we been called with a time based rotation pattern then setup
	# timebased stuff. TZ is important and must match current TZ or all
	# bets are off!
	if(defined $p{'TZ'})
	{
		Date_Init("TZ=".$p{'TZ'});  # EADT or EAST when not in daylight savings
	}
	if(defined $p{'DatePattern'})
	{
		$self->setDatePattern($p{'DatePattern'});
	}

    return $self;
}


###########################################################################
#
# Subroutine setDatePattern
#       
#       Args: a single string or ArrayRef of strings
#
#       Rtns: Nothing
#
# Description:
#     Set a recurrance for file rotation. We accept Date::Manip
#     recurrances and the log4j/DailyRollingFileAppender patterns
#       
#     Date:Manip =>              
#			0:0:0:0:5:30:0       every 5 hours and 30 minutes
#			0:0:0:2*12:30:0      every 2 days at 12:30 (each day)
#			3*1:0:2:12:0:0       every 3 years on Jan 2 at noon
#
#	  DailyRollingFileAppender =>
#			yyyy-MM
#			yyyy-ww
#			yyyy-MM-dd
#			yyyy-MM-dd-a
#			yyyy-MM-dd-HH
#			yyyy-MM-dd-HH-MM
#
# To specify multiple recurances in a single string seperate them with a
# comma: yyyy-MM-dd,0:0:0:2*12:30:0
#
sub setDatePattern
{
    my $self = shift;        # My object
    my($arg) = shift;

	local($_);               # Don't crap on $_
	my @pats = ();

	my %lookup = (
		#					 Y:M:W:D:H:M:S
		'yyyy-MM'		=> 	'0:1*0:1:0:0:0',  # Every Month
		'yyyy-ww'		=> 	'0:0:1*0:0:0:0',  # Every week
		'yyyy-dd'		=> 	'0:0:0:1*0:0:0',  # Every day 
		'yyyy-dd-a'		=> 	'0:0:0:1*12:0:0', # Every day 12noon
		'yyyy-dd-HH'	=> 	'0:0:0:0:1*0:0',  # Every hour
		'yyyy-dd-HH-MM'	=> 	'0:0:0:0:0:1*0',  # Every minute
	);

	# Convert arg to array
	if( ref($arg) eq 'ARRAY' )
	{
		@pats = @$arg;
	}
	elsif( !ref($arg) )
	{
		$arg =~ s/\s+//go;
		@pats = split(/;/,$arg);
	}
	else
	{
		die "Bad referance type argument ".ref($arg);
	}

	# Handle (possibly multiple) recurrances
	foreach my $pat (@pats)
	{
		# Convert any log4j patterns across
		if($pat =~ /^yyyy/) # Then log4j style
		{
			# Default to daily on bad pattern
			unless(grep($pat eq $_,keys %lookup))
			{
				warn "Bad Rotation pattern ($pat) using yyyy-dd\n";
				$pat = 'yyyy-dd';
			}
			$pat = $lookup{$pat};
		}

		my $abs = $self->_get_next_occurance($pat);
		warn "Adding [epoch secs,pat] =>[$abs,$pat]\n" if $self->{debug};
		my $ref = [$abs, $pat];
		push(@{$self->{'recurrance'}}, $ref);

	}

}


sub log_message
{
    my $self = shift;
    my %p = @_;

	my $max_size = $self->{size};
	my $numfiles = $self->{max};
	my $name     = $self->{params}->{filename};
	my $fh       = $self->{LDF}->{fh};

	# Prime our time based data outside the critical code area
	my ($in_time_mode,$time_to_rotate) = $self->time_to_rotate();

	# Handle critical code for logging. No changes if someone else is in
	if( !$self->lfhlock_test() )
	{
		warn "$$ waiting on lock\n" if $self->{debug};
		$self->lfhlock() || die "Can't get a lock";
	}

	my $size   = (stat($fh))[7];   # Stat the handle to get real size
	my $inode  = (stat($fh))[1];   # get real inode
	my $finode = (stat($name))[1]; # Stat the name for comparision
	warn localtime()." $$  s=$size, i=$inode, f=$finode, n=$name\n" if $self->{debug};

	# If finode and inode are the same then nobody has done a rename
	# under us and we can continue. Otherwise just close and reopen.
	# Time mode overrides Size mode
	if($in_time_mode && !$time_to_rotate && $inode == $finode)
	{
		warn localtime()." $$ In time mode: normal log\n" if $self->{debug};
		$self->logit($p{message});
	}
	elsif(!$in_time_mode && 
	      defined($size) && $size < $max_size && $inode == $finode
		  )
	{
		warn localtime()." $$ In size mode: normal log\n" if $self->{debug};
		$self->logit($p{message});
	}
	elsif($inode != $finode)
	{
		# Oops someone moved things on us. So just reopen our log
		delete $self->{LDF};  # Should get rid of current LDF
		$self->{LDF} =  Log::Dispatch::File->new(%{$self->{params}});  # Our log

		warn localtime()." $$ Someone else rotated: normal log\n" if $self->{debug};
		$self->logit($p{message});
	}
	# Need to rotate
	elsif(($in_time_mode && $time_to_rotate) || 
	      (!$in_time_mode && $size)
		 )
	{
		# Shut down the log
		delete $self->{LDF};  # Should get rid of current LDF

		my $idx = $numfiles -1;

		warn localtime() . " $$ Rotating\n" if $self->{debug};
		while($idx >= 0)
		{
			if($idx <= 0)
			{
				warn "$$ rename $name $name.1\n" if $self->{debug};
				rename($name, "$name.1");
			}
			else
			{
				warn "$$ rename $name.$idx $name.".($idx+1)."\n" if $self->{debug};
				rename("$name.$idx", "$name.".($idx+1));
			}

			$idx--;
		}
		warn localtime() . " $$ Rotating Done\n" if $self->{debug}; 

		# reopen the logfile for writing.
		$self->{LDF} =  Log::Dispatch::File->new(%{$self->{params}});  # Our log

		# Write it out
		warn localtime()." $$ rotated: normal log\n" if $self->{debug};
		$self->logit($p{message});
	}
	#else size is zero :-} just don't do anything!

	$self->lfhunlock();
}

sub DESTROY
{
    my $self = shift;

    if ( $self->{LDF} )
    {
		delete $self->{LDF};  # Should get rid of current LDF
    }

	# Clean up locks
	close $self->{lfh};
	unlink $self->{lf};
}

sub logit
{
	my $self = $_[0];

	$self->lock();
	$self->{LDF}->log_message(message => $_[1]);
	$self->unlock();
	return;
}


###########################################################################
#
# Subroutine time_to_rotate
#       
#       Args: none
#
#       Rtns: (1,n)  if we are in time mode and its time to rotate
#                    n defines the number of timers that expired
#             (1,0)  if we are in time mode but not ready to rotate
#             (0,0)  otherwise
#
# Description:
#     time_to_rotate - update internal clocks and return status as
#     defined above
#       
#       
#	my ($in_time_mode,$time_to_rotate) = $self->time_to_rotate();
sub time_to_rotate
{
    my $self   = shift;        # My object
	my $mode   = defined($self->{'recurrance'});
	my $rotate = 0;

	if($mode)
	{
		# Then do some checking and update ourselves if we think we need
		# to rotate. Wether we rotate or not is up to our caller. We
		# assume they know what their doing!

		# Check need for rotation. Loop through our recurrances looking
		# for expiration times. Any we find that have expired we update.
		my $tm    = time();
		my @recur = @{$self->{'recurrance'}};
		@{$self->{'recurrance'}} = ();
		for my $rec (@recur)
		{
			my ($abs,$pat) = @$rec;
			my $dorotate = 0;
			if($abs <= $tm)
			{
				# Then we need to rotate
				$abs = $self->_get_next_occurance($pat);
				$rotate++;
				$dorotate++;  # Just for debugging
			}
			push(@{$self->{'recurrance'}},[$abs,$pat]);
			my $next = localtime($abs);
			warn "time_to_rotate(mode,rotate,next) => ($mode,$dorotate,$next)\n" if $self->{debug};
		}
		
	}

	warn "time_to_rotate(mode,rotate) => ($mode,$rotate)\n" if $self->{debug};
	return wantarray ? ($mode,$rotate) : $rotate;
}

###########################################################################
#
# Subroutine _get_next_occurance
#       
#       Args: Date::Manip occurance pattern
#
#       Rtns: epoch seconds for next event
#
sub _get_next_occurance
{
    my $self = shift;        # My object
    my($pat) = shift;
	my $range = '';

	if($pat =~ /^0:0:0:0:0/) # Small recurrance less than 1 hour
	{
		$range = "2 hours later";
	}
	elsif($pat =~ /^0:0:0:0/) # recurrance less than 1 day
	{
		$range = "2 days later";
	}
	elsif($pat =~ /^0:0:0:/) #  recurrance less than 1 week
	{
		$range = "2 weeks later";
	}
	elsif($pat =~ /^0:0:/) #  recurrance less than 1 month
	{
		$range = "2 months later";
	}
	elsif($pat =~ /^0:/) #  recurrance less than 1 year
	{
		$range = "12 months later";
	}
	else # years
	{
		my($yrs) = m/^(\d+):/;
		$yrs = 1 unless $yrs;
		my $months = $yrs * 2 * 12;

		$range = "$months months later";
	}

	# The next date must start at least 1 second away from now other wise
	# we may rotate for every message we recieve with in this second :-(
	my $start = DateCalc("now","+ 1 second");

	my @dates = ParseRecur($pat,"now",$start,$range);

	# Just in case we have a bad parse or our assumptions are wrong.
	# We default to days
	unless(scalar @dates >= 2)
	{
		warn "Failed to parse ($pat). Going daily\n";
		@dates = ParseRecur('0:0:0:1*0:0:0',"now","now","1 months later");
	}

	die "bummer in Interval calc" unless defined $dates[0];

	warn "Next date =>".$dates[0]."\n" if $self->{debug};
	return UnixDate($dates[0],'%s');
}

# Lock and unlock routines. For when we need to write a message.
use Fcntl ':flock'; # import LOCK_* constants

sub lock 
{
   my $self = shift;
   flock($self->{LDF}->{fh},LOCK_EX);

   # Make sure we are at the EOF
   seek($self->{LDF}->{fh}, 0, 2);

   warn localtime() ." $$ Locked\n" if $self->{debug};
   return;
}

sub unlock 
{
   my $self = shift;
   flock($self->{LDF}->{fh},LOCK_UN);
   warn localtime() . " $$ unLocked\n" if $self->{debug};
}

# Lock and unlock routines. For when we need to roll the logs.
sub lfhlock_test 
{
   my $self = shift;
   if (flock($self->{lfh},LOCK_EX | LOCK_NB))
   {
		warn "$$ got lock on Lock File ".$self->{lfh}."\n" if $self->{debug};
   		return 1;
   }
   else
   {
		warn "$$ couldn't get lock on Lock File\n" if $self->{debug};
	   return 0;
   }
}

sub lfhlock
{
   my $self = shift;
   flock($self->{lfh},LOCK_EX);
}

sub lfhunlock 
{
   my $self = shift;
   flock($self->{lfh},LOCK_UN);
}

sub debug
{
	$_[0]->{'debug'} = $_[1];
}

__END__

=head1 NAME

Log::Dispatch::FileRotate - Log to files that archive/rotate themselves

=head1 SYNOPSIS

  use Log::Dispatch::FileRotate;

  my $file = Log::Dispatch::FileRotate->new( name      => 'file1',
                                       min_level => 'info',
                                       filename  => 'Somefile.log',
                                       mode      => 'append' ,
                                       size      => 10,
                                       max       => 6,
                                      );
  # or for a time based rotation

  my $file = Log::Dispatch::FileRotate->new( name      => 'file1',
                                       min_level => 'info',
                                       filename  => 'Somefile.log',
                                       mode      => 'append' ,
                                       TZ        => 'AEDT',
                                       DatePattern => 'yyyy-dd-HH',
                                      );

  $file->log( level => 'info', message => "your comment\n" );

=head1 DESCRIPTION

This module provides a simple object for logging to files under the
Log::Dispatch::* system, and automatically rotating them according to
different constraints. This is basically a Log::Dispatch::File wrapper
with additions. To that end the arguments

	name, min_level, filename and  mode

behave the same as Log::Dispatch::File. So see its man page 
(perldoc Log::Dispatch::File)

The arguments size and max specify the maximum size (in meg) and maximum
number of log files created. The size defaults to 10M and the max number
of files defaults to 1. If DatePattern is not defined then we default to
working in size mode. That is, use size values for deciding when to rotate.

Once DatePattern is defined FileRotate will move into time mode. Once
this happens file rotation ignores size constraints and uses the defined
date pattern constraints.

If you setup a config file using Log::Log4perl::init_and_watch() or the
like, you can switch between modes just by commenting out the DatePattern
line.

When using DatePattern make sure TZ is defined correctly and that the TZ
you use is understood by Date::Manip. We use Date::Manip to generate our
recurrences. Bad TZ equals bad recurrences equals surprises! Read the
Date::Manip man page for more details on TZ.

DatePattern will default to a daily rotate if your entered pattern is
incorrect. You will also get a warning message.

If you have multiple writers that were started at different times you
will find each writer will try to rotate the log file at a recurrence
calculated from its start time. To sync all the writers just use a config
file and update it after starting your last writer. This will cause
Log::Dispatch::FileRotate->new() to be called by each of the writers
close to the same time, and if your recurrences aren't too close together
all should sync up just nicely.

We handle multiple writers using flock().

=head1 DatePattern

As I said earlier we use Date::Manip for generating our recurrence
events. This means we can understand Date::Manip's recurrence patterns
and the normal log4j DatePatterns. We don't use DatePattern to define the
extension of the log file though.

DatePattern can therfore take forms like:

	
      Date::Manip style
            0:0:0:0:5:30:0       every 5 hours and 30 minutes
            0:0:0:2*12:30:0      every 2 days at 12:30 (each day)
            3*1:0:2:12:0:0       every 3 years on Jan 2 at noon

      DailyRollingFileAppender log4j style
            yyyy-MM              every month
            yyyy-ww              every week
            yyyy-MM-dd           every day
            yyyy-MM-dd-a         every day at noon
            yyyy-MM-dd-HH        every hour
            yyyy-MM-dd-HH-MM     every minute

To specify multiple recurrences in a single string separate them with a
semicolon:
        yyyy-MM-dd; 0:0:0:2*12:30:0

This says we want to rotate every day AND every 2 days at 12:30. Put in
as many as you like.

A complete description of Date::Manip recurrences is beyond us here
except to quote (from the man page):

           A recur description is a string of the format
           Y:M:W:D:H:MN:S .  Exactly one of the colons may
           optionally be replaced by an asterisk, or an asterisk
           may be prepended to the string.

           Any value "N" to the left of the asterisk refers to
           the "Nth" one.  Any value to the right of the asterisk
           refers to a value as it appears on a calendar/clock.
           Values to the right can be listed a single values,
           ranges (2 numbers separated by a dash "-"), or a comma
           separated list of values or ranges.  In a few cases,
           negative values are appropriate.

           This is best illustrated by example.

             0:0:2:1:0:0:0        every 2 weeks and 1 day
             0:0:0:0:5:30:0       every 5 hours and 30 minutes
             0:0:0:2*12:30:0      every 2 days at 12:30 (each day)
             3*1:0:2:12:0:0       every 3 years on Jan 2 at noon
             0:1*0:2:12,14:0:0    2nd of every month at 12:00 and 14:00
             1:0:0*45:0:0:0       45th day of every year
             0:1*4:2:0:0:0        4th tuesday (day 2) of every month
             0:1*-1:2:0:0:0       last tuesday of every month
             0:1:0*-2:0:0:0       2nd to last day of every month



=head1 METHODS

=over 4

=item * new(%p)

This method takes a hash of parameters.  The following options are
valid:

=item -- name ($)

The name of the object (not the filename!).  Required.

=item -- size ($)

The maxium (or close to) size the log file can grow too.

=item -- max ($)

The maxium number of log files to create.


=item -- TZ ($)

The TimeZone time based calculations should be done in. This should match
Date::Manip's concept of timezones and of course your machines timezone.
Date::Manip will normally work everything out for you. Except in my case
where EST means Eastern Standard Time in Australia not the US! I had to
use AEST or EADT instead. Here is a list of Date::Manip's timezones
straight from its man page.


       The following timezone names are currently understood (and
       can be used in parsing dates).  These are zones defined in
       RFC 822.

           Universal:  GMT, UT
           US zones :  EST, EDT, CST, CDT, MST, MDT, PST, PDT
           Military :  A to Z (except J)
           Other    :  +HHMM or -HHMM
           ISO 8601 :  +HH:MM, +HH, -HH:MM, -HH

       In addition, the following timezone abbreviations are also
       accepted.

             IDLW    -1200    International Date Line West
             NT      -1100    Nome
             HST     -1000    Hawaii Standard
             CAT     -1000    Central Alaska
             AHST    -1000    Alaska-Hawaii Standard
             AKST    -0900    Alaska Standard
             YST     -0900    Yukon Standard
             HDT     -0900    Hawaii Daylight
             AKDT    -0800    Alaska Daylight
             YDT     -0800    Yukon Daylight
             PST     -0800    Pacific Standard
             PDT     -0700    Pacific Daylight
             MST     -0700    Mountain Standard
             MDT     -0600    Mountain Daylight
             CST     -0600    Central Standard
             CDT     -0500    Central Daylight
             EST     -0500    Eastern Standard
             SAT     -0400    Chile
             EDT     -0400    Eastern Daylight
             AST     -0400    Atlantic Standard
             ADT     -0300    Atlantic Daylight
             NDT     -0230    Newfoundland Daylight
             AT      -0200    Azores
             WAT     -0100    West Africa
             GMT     +0000    Greenwich Mean
             UT      +0000    Universal (Coordinated)
             UTC     +0000    Universal (Coordinated)
             WET     +0000    Western European
             WEST    +0000    Alias for Western European
             CET     +0100    Central European
             FWT     +0100    French Winter
             MET     +0100    Middle European
             MEZ     +0100    Middle European
             MEWT    +0100    Middle European Winter
             SWT     +0100    Swedish Winter
             BST     +0100    British Summer     bst=Brazil standard  -0300
             GB      +0100    GMT with daylight savings
             CEST    +0200    Central European Summer
             EET     +0200    Eastern Europe, USSR Zone 1
             FST     +0200    French Summer
             MEST    +0200    Middle European Summer
             MESZ    +0200    Middle European Summer
             METDST  +0200    An alias for MEST used by HP-UX
             SAST    +0200    South African Standard
             SST     +0200    Swedish Summer       sst=South Sumatra    +0700
             EEST    +0300    Eastern Europe Summer
             BT      +0300    Baghdad, USSR Zone 2
             MSK     +0300    Moscow
             IT      +0330    Iran
             ZP4     +0400    USSR Zone 3
             MSD     +0300    Moscow Daylight
             ZP5     +0500    USSR Zone 4
             IST     +0530    Indian Standard
             ZP6     +0600    USSR Zone 5
             CCT     +0800    China Coast, USSR Zone 7
             AWST    +0800    West Australian Standard
             WST     +0800    West Australian Standard
             PHT     +0800    Asia Manila
             JST     +0900    Japan Standard, USSR Zone 8
             ROK     +0900    Republic of Korea
             CAST    +0930    Central Australian Standard
             EAST    +1000    Eastern Australian Standard
             GST     +1000    Guam Standard, USSR Zone 9  gst=Greenland Std
             CADT    +1030    Central Australian Daylight
             EADT    +1100    Eastern Australian Daylight
             IDLE    +1200    International Date Line East
             NZST    +1200    New Zealand Standard
             NZT     +1200    New Zealand
             NZDT    +1300    New Zealand Daylight



=item -- DatePattern ($)

The DatePattern as defined above.

=item -- min_level ($)

The minimum logging level this object will accept.  See the
Log::Dispatch documentation for more information.  Required.

=item -- max_level ($)

The maximum logging level this obejct will accept.  See the
Log::Dispatch documentation for more information.  This is not
required.  By default the maximum is the highest possible level (which
means functionally that the object has no maximum).

=item -- filename ($)

The filename to be opened for writing. This is the base name. Rotated log
files will be renamed filename.1 thru to filename.C<max>. Where max is the
paramater defined above.

=item -- mode ($)

The mode the file should be opened with.  Valid options are 'write',
'>', 'append', '>>', or the relevant constants from Fcntl.  The
default is 'write'.

=item -- autoflush ($)

Whether or not the file should be autoflushed.  This defaults to true.

=item -- callbacks( \& or [ \&, \&, ... ] )

This parameter may be a single subroutine reference or an array
reference of subroutine references.  These callbacks will be called in
the order they are given and passed a hash containing the following keys:

 ( message => $log_message, level => $log_level )

The callbacks are expected to modify the message and then return a
single scalar containing that modified message.  These callbacks will
be called when either the C<log> or C<log_to> methods are called and
will only be applied to a given message once.

=item -- DEBUG ($)

Turn on lots of warning messages to STDERR about what this module is
doing if set to 1. Really only useful to me.

=item * log_message( message => $ )

Sends a message to the appropriate output.  Generally this shouldn't
be called directly but should be called through the C<log()> method
(in Log::Dispatch::Output).

=item * setDatePattern( $ or [ $, $, ... ] )

Set a new suite of recurrances for file rotation. You can pass in a
single string or a reference to an array of strings. Multiple recurrences
can also be define within a single string by seperating them with a
semi-colon (;)

See the discussion above regarding the setDatePattern paramater for more
details.

=back

=head1 TODO

compression, signal based rotates, proper test suite

Could possibly use Logfile::Rotate as well/instead.

=head1 AUTHOR

Mark Pfeiffer, <markpf@mlp-consulting.com.au> inspired by
Dave Rolsky's, <autarch@urth.org>, code :-)

Kevin Goess <kevin@goess.org> suggested multiple writers should be
supported. He also conned me into doing the time based stuff.
Thanks Kevin! :-)

=cut

