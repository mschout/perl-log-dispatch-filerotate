package Log::Dispatch::FileRotate;

require 5.005;
use strict;

use Log::Dispatch::Output;

use base qw( Log::Dispatch::Output );

use Log::Dispatch::File;   # We are a wrapper around Log::Dispatch::File

use Params::Validate qw(validate SCALAR BOOLEAN);
Params::Validate::validation_options( allow_extra => 1 );

use vars qw[ $VERSION ];

$VERSION = sprintf "%d.%02d", q$Revision: 1.02 $ =~ /: (\d+)\.(\d+)/;

sub new
{
    my $proto = shift;
    my $class = ref $proto || $proto;

    my %p = @_;

    my $self = bless {}, $class;

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

    return $self;
}

sub log_message
{
    my $self = shift;
    my %p = @_;

	my $max_size = $self->{size};
	my $numfiles = $self->{max};
	my $name     = $self->{params}->{filename};

	my $size = (stat($name))[7];
	if(defined($size) && $size < $max_size)
	{
		$self->{LDF}->log_message(message => $p{message});
		return;
	}
	elsif($size)
	{
		# Shut down the log
		delete $self->{LDF};  # Should get rid of current LDF

		my $idx = $numfiles -1;
		# We use rename as we aren't moving across file systems
		while($idx >= 0)
		{
			if($idx <= 0)
			{
				rename($name, "$name.1");
			}
			else
			{
				rename("$name.$idx", "$name.".($idx+1));
			}

			$idx--;
		}
		$self->{LDF} =  Log::Dispatch::File->new(%{$self->{params}});  # Our log
	}
	#else size is zero :-} just don't do anything!
}

sub DESTROY
{
    my $self = shift;

    if ( $self->{LDF} )
    {
		delete $self->{LDF};  # Should get rid of current LDF
    }
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

=head1 AUTHOR

Mark Pfeiffer, <markpf@mlp-consulting.com.au> inspired by
Dave Rolsky's, <autarch@urth.org>, code :-)

=cut

