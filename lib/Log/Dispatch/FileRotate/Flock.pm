# COPYRIGHT

package Log::Dispatch::FileRotate::Flock;

# ABSTRACT: File Locking Functions for L<Log::Dispatch::FileRotate>

use strict;
use warnings;

use base 'Exporter';

use Fcntl ':flock';

our @EXPORT_OK = qw(safe_flock flopen);

=method safe_flock($filehandle, $flags): boolean

This is a wrapper around C<flock()> that handles things such as interruption of
the call by a signal automatically.

=cut

sub safe_flock {
    my ($fh, $flags) = @_;

    while (1) {
        unless (flock $fh, $flags) {
            # retry if we were interrupted or we are in non-blocking and the file is locked
            next if $!{EAGAIN} or $!{EWOULDBLOCK};

            return 0;
        }
        else {
            return 1;
        }
    }
}

=method flopen($path): ($filehandle, $inode)

This function is similar to BSD's C<flopen()> function.  It opens a file,
obtiains an exclusive lock on it using C<flock()>, and handles a bunch of race
conditions that can happen.  It returns the opened filehandle and the inode of
the file on success, nothing on failure.

=cut

sub flopen {
    my $path = shift;

    my $flags = LOCK_EX; my $fh;

    while (1) {
        unless (open $fh, '>>', $path) {
            return;
        }

        unless (safe_flock($fh, $flags)) {
            return;
        }

        my @path_stat = stat $path;
        unless (@path_stat) {
            # file disappeared fron under our feet
            close $fh;
            next;
        }

        my @fh_stat = stat $fh;
        unless (@fh_stat) {
            # This should never happen
            return;
        }

        unless ($^O =~ /^MSWin/) {
            # stat on a filehandle and path return different "dev" and "rdev"
            # fields on windows
            if ($path_stat[0] != $fh_stat[0]) {
                # file was changed under our feet. try again;
                close $fh;
                next;
            }
        }

        # check that inode are the same for the path and fh
        if ($path_stat[1] != $fh_stat[1])
        {
            # file was changed under our feet. try again;
            close $fh;
            next;
        }

        return ($fh, $fh_stat[1]);
    }
}

1;

__END__

=head1 SYNOPSIS

 Internal Use Only!

=head2 DESCRIPTION

Internal Use Only!

=cut
