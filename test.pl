#!/usr/bin/perl -w

# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl test.pl'

######################### We start with some black magic to print on failure.

# Change 1..1 below to 1..last_test_to_print .
# (It may become useful if the test is moved to ./t subdirectory.)

BEGIN { $| = 1; print "1..5\n"; }
END {print "not ok 1\n" unless $loaded;}
use Log::Log4perl;
use Log::Dispatch::FileRotate;
$loaded = 1;
print "ok 1\n";

######################### End of black magic.

# Insert your test code below (better if it prints "ok 13"
# (correspondingly "not ok 13") depending on the success of chunk 13
# of the test code):

# First lets build a conf file for use latter
use Date::Manip;
my $tz;
eval '$tz= Date_TimeZone();';
if($@)
{
	print "Unable to determine timezone! Lets see if it matters..\n";
	my $start = DateCalc("now","+ 1 second");
	my @dates = ParseRecur('0:0:0:0:0:1*0',"now",$start,'20 minutes later');

	# Should get about 20 in the array
	my @epochs = map {UnixDate($_,'%s')} @dates;
	shift(@epochs) while @epochs && $epochs[0] <= time();

	# If no epochs left then Timezone issue is going to bite us!
	# all bets are off.
	if( @epochs )
	{
		print "It looks like we can get by without a timezone. Lucky!\n";
		print "ok 2\n";
	}
	else
	{
		print "**** Time Zone problem: All bets are off. ****\n";
		print "not ok 2\n";
	}
	$tz = '';

}
else
{
	print "Your timezone is $tz.\n";
	$tz = "log4j.appender.FILE.TZ=$tz";
	print "ok 2\n";
}


my $config = <<EOT;

log4j.rootLogger=DEBUG, FILE
log4j.logger.nms=DEBUG, FILE

log4j.appender.S=Log::Dispatch::Screen
#log4j.appender.S.Threshold=FATAL
log4j.appender.S.layout=org.apache.log4j.PatternLayout
log4j.appender.S.layout.ConversionPattern=%d %F %-4L %-5p %c - %m%n

#log4j.appender.FILE.DEBUG=1
log4j.appender.FILE=Log::Dispatch::FileRotate
log4j.appender.FILE.filename=myerrs.log
log4j.appender.FILE.mode=append
log4j.appender.FILE.size=20000
# This is my timezone in Aus
# log4j.appender.FILE.TZ=EADT
# This is hopefully your timezone
$tz
#recurrance dates - Every Hour and Every 10mins and 1st day 4th hr of every week
log4j.appender.FILE.DatePattern=yyyy-dd-HH; 0:0:0:0:0:10*0; 0:0:1*1:4:0:0
#log4j.appender.FILE.DatePattern=0:0:0:0:0:1:0
#log4j.appender.FILE.DatePattern=yyyy-dd-HH
log4j.appender.FILE.max=5
log4j.appender.FILE.layout=org.apache.log4j.PatternLayout
log4j.appender.FILE.layout.ConversionPattern=%d %F %-4L %-5p %c - %m%n

EOT

open(CONF, "> log.conf") || die "Can't create log.conf";
print CONF $config;
close(CONF);

Log::Log4perl::init_and_watch("log.conf",10);
print "ok 3\n";

my $logger = Log::Log4perl->get_logger('nms.cisco.utility');
my $logger1 = Log::Log4perl->get_logger('nms');

print "ok 4\n\n";

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
print "ok 5\n";

