package Log::Dispatch::FileRotate;

require 5.005;
use strict;

use Log::Dispatch::Output;

use base qw( Log::Dispatch::Output );

use Log::Dispatch::File;   # We are a wrapper around Log::Dispatch::File

use Params::Validate qw(validate SCALAR BOOLEAN);
Params::Validate::validation_options( allow_extra => 1 );

use vars qw[ $VERSION ];

$VERSION = sprintf "%d.%02d", q$Revision: 1.03 $ =~ /: (\d+)\.(\d+)/;

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

    return $self;
}

sub log_message
{
    my $self = shift;
    my %p = @_;

	my $max_size = $self->{size};
	my $numfiles = $self->{max};
	my $name     = $self->{params}->{filename};
	my $fh       = $self->{LDF}->{fh};

	# Handle critical code for logging. No changes if someone else is in
	if( !$self->lfhlock_test() )
	{
		warn "$$ waiting on lock\n" if $self->{debug};
		$self->lfhlock() || die "Can't get a lock";
	}

	my $size = (stat($fh))[7]; # Stat the handle not the name to get real size
	my $inode = (stat($fh))[1]; # Stat the handle not the name to get real inode
	my $finode = (stat($name))[1]; # Stat the name for comparision

	# If finode and inode are the same then nobody has done a rename
	# under us and we can continue. Otherwise just close and reopen.
	warn localtime() . " $$  s=$size, i=$inode, f=$finode, n=$name\n" if $self->{debug};

	if(defined($size) && $size < $max_size && $inode == $finode)
	{
		$self->logit($p{message});
	}
	elsif($inode != $finode)
	{
		# Oops someone moved things on us. So just reopen our log
		delete $self->{LDF};  # Should get rid of current LDF
		$self->{LDF} =  Log::Dispatch::File->new(%{$self->{params}});  # Our log

		$self->logit($p{message});
	}
	# Need to rotate
	elsif($size)
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

# Lock and unlock routines. For when we need to write a message.
use Fcntl ':flock'; # import LOCK_* constants

sub lock 
{
   my $self = shift;
   flock($self->{LFD}->{fh},LOCK_EX);

   # Make sure we are at the EOF
   seek($self->{LDF}->{fh}, 0, 2);

   warn localtime() ." $$ Locked\n" if $self->{debug};
   return;
}

sub unlock 
{
   my $self = shift;
   flock($self->{LFD}->{fh},LOCK_UN);
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
of files defaults to 1.

Later I'll provide time based constaints as well.

We handle multiple writers using flock().

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

=item * log_message( message => $ )

Sends a message to the appropriate output.  Generally this shouldn't
be called directly but should be called through the C<log()> method
(in Log::Dispatch::Output).


=back

=head1 TODO

compression, time based rotates, signal based rotates, proper test suite

Could possibly use Logfile::Rotate as well/instead.

=head1 AUTHOR

Mark Pfeiffer, <markpf@mlp-consulting.com.au> inspired by
Dave Rolsky's, <autarch@urth.org>, code :-)

Kevin Goess <kevin@goess.org> suggested multiple writers should be
supported. Thanks Kevin.

=cut

