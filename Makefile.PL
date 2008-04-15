use ExtUtils::MakeMaker;

WriteMakefile(
	'NAME'		=> 'MIME::EncWords',
	'ABSTRACT_FROM'	=> 'EncWords.pm',
	'VERSION_FROM'	=> 'EncWords.pm',
	'PREREQ_PM'	=> ($] >= 5.008001)? {
	    'Encode'		=> 1.98,
	    'MIME::Charset'	=> '1.006.1',
	    'MIME::Base64'	=> 2.13,
	    'Test'		=> 0,
	}: {
	    'MIME::Charset'	=> '1.006',
	    'MIME::Base64'	=> 2.13,
	    'Test'		=> 0,
	},
);

