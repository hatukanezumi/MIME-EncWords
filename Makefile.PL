use ExtUtils::MakeMaker;

WriteMakefile(
	'NAME'		=> 'MIME::EncWords',
	'ABSTRACT_FROM'	=> 'EncWords.pm',
	'VERSION_FROM'	=> 'EncWords.pm',
	'PREREQ_PM'	=> {
	    'Encode'		=> 2.01,
	    'MIME::Charset'	=> 0.03,
	    'MIME::Base64'	=> 3.05,
	    'Test::More'	=> 0,
	},
);

