use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    'NAME'		=> 'Log::Dispatch::FileRotate',
    'VERSION_FROM'	=> 'FileRotate.pm', # finds $VERSION
    'PREREQ_PM'		=> {}, # e.g., Module::Name => 1.1
	'ABSTRACT_FROM' => 'FileRotate.pm',
	'AUTHOR'        => '"Mark Pfeiffer" <markpf@mlp-consulting.com.au>',
	'clean'         => {FILES => "*.log*"},
	 PREREQ_PM   => {
			'Log::Log4perl'	=> '0.23', # or a minimum workable version
			'Log::Dispatch::Output' => 0,
			'Date::Manip'      => 0,
			'Params::Validate' => 0,
			'Fcntl'         => 0,
    }
);
