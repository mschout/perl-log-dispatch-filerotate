#!/usr/bin/perl -w

# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl test.pl'

######################### We start with some black magic to print on failure.

# Change 1..1 below to 1..last_test_to_print .
# (It may become useful if the test is moved to ./t subdirectory.)

BEGIN { $| = 1; print "1..4\n"; }
END {print "not ok 1\n" unless $loaded;}
use Log::Log4perl;
use Log::Dispatch::FileRotate;
$loaded = 1;
print "ok 1\n";

######################### End of black magic.

# Insert your test code below (better if it prints "ok 13"
# (correspondingly "not ok 13") depending on the success of chunk 13
# of the test code):

Log::Log4perl::init_and_watch("log.conf",10);
print "ok 2\n";

my $logger = Log::Log4perl->get_logger('nms.cisco.utility');
my $logger1 = Log::Log4perl->get_logger('nms');

print "ok 3\n\n";

print "while true; do clear;ls -ltr| grep myerrs; sleep 1; done\n\n";
print "Type this in another xterm in this directory to see the logs
changing. You can also edit log.conf and change params to see what will
happen to the log files.

You can also run a number of 'make test' commands to see how we behave
with multiple writers to log files.

Edit test.pl and uncomment the 'sleep 1' line if you want to
see time rotation happening
";

my $i = 4;
while ($i <= 65 )
{
 $logger->debug($$ . ' this is a debug message');
 $logger->info($$  . ' this is an info message');
 $logger->warn($$  . ' etc');
 $logger->error($$ . ' ..');
 $logger->fatal($$ . ' ..');

 $logger1->info($$ . ' this is an info message via logger1');
 $i++;
# sleep 1;
 print ".";
}
print "\n";
print "ok 4\n";

