# COPYRIGHT

package Log::Dispatch::FileRotate::Mutex;

# ABSTRACT: Flock Based File Mutex.

use strict;
use warnings;
use Carp 'croak';

use Log::Dispatch::FileRotate::Flock qw(safe_flock flopen);
use Fcntl ':flock';

my $HAS_THREADS = $INC{'threads.pm'} ? 1 : 0;
my $THREAD_ID   = $HAS_THREADS ? threads->tid() : 0;

sub CLONE {
    $THREAD_ID = threads->tid() if $HAS_THREADS;
}

sub DESTROY {
    my $self = shift;

    my $pid = $self->pid;

    if ($self->{$pid}) {
        $self->unlock;
        close(delete $self->{_fh});
    }

    return;
}

=method new($path)

Create a new mutex for the given file path.  Only one mutex per path should be
created.  The path will not actually be opened or locked until you call L<lock>.

=cut

sub new {
    my ($class, $path, %args) = @_;

    $class = ref $class || $class;

    my $self = bless {
        _path => $path,
        %args
    }, $class;

    return $self;
}

=method lock()

Obtains a lock on the path.  If the thread id or pid has changed since the path
was opened, the path will be re-opened automatically in this thread or process.

=cut

sub lock {
    my $self = shift;

    my $pid = $self->pid;

    unless (exists $self->{$pid}) {
        # we have not opened the lockfile in this thread.
        my ($fh, $inode) = flopen($self->{_path});

        $self->_set_permissions;

        unless (defined $fh) {
            return 0;
        }

        $self->{_fh}    = $fh;
        $self->{_inode} = $inode;
        $self->{$pid}   = 1;
    }
    elsif ($self->{$pid} == 0) {
        # file is open, but not locked.
        if (safe_flock($self->{_fh}, LOCK_EX)) {
            my ($inode) = (stat $self->{_path})[1];

            if ($inode != $self->{_inode}) {
                # file was removed or changed underneath us, reopen instead
                delete $self->{$pid};

                close(delete $self->{_fh});

                delete $self->{$pid};
                delete $self->{_inode};

                return $self->lock;
            }

            $self->{$pid} = 1;
        }
    }

    # otherwise this $pid is already holding the lock

    return $self->{$pid} || 0;
}

sub _set_permissions {
    my $self = shift;

    unless (defined $self->{permissions}) {
        return;
    }

    my $file = $self->{_path};

    my $current_mode = (stat $self->{_path})[2] & 07777;

    if ($current_mode ne $self->{permissions}) {
        chmod $self->{permissions}, $self->{_path}
            or croak sprintf 'Failed to chmod %s to %04o: %s',
                $self->{_path}, $self->{permissions} & 07777, $!;
    }
}

=method unlock()

Releases the lock if the current thread or process is holding it.

=cut

sub unlock {
    my $self = shift;

    my $pid = $self->pid;

    if ($self->{$pid}) {
        safe_flock($self->{_fh}, LOCK_UN);
        $self->{$pid} = 0;
    }
}

=method pid(): string

Get the current process or thread id

=cut

sub pid {
    return $HAS_THREADS
        ? join('.', $$, $THREAD_ID)
        : $$;
}

1;

__END__

=head1 SYNOPSIS

Internal Use Only!

=head1 DESCRIPTION

Internal Use Only!

=cut
