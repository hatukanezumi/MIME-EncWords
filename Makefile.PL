use ExtUtils::MakeMaker;

WriteMakefile(
	'NAME'		=> 'MIME::EncWords',
	'ABSTRACT_FROM'	=> 'EncWords.pm',
	'VERSION_FROM'	=> 'EncWords.pm',
	'PREREQ_PM'	=> ($] >= 5.007003)? {
	    'Encode'		=> 1.98,
	    'MIME::Charset'	=> '1.008.1',
	    'MIME::Base64'	=> 2.13,
	    'Test::More'	=> 0,
	}: {
	    'MIME::Charset'	=> '1.006',
	    'MIME::Base64'	=> 2.13,
	    'Test::More'	=> 0,
	},
);

