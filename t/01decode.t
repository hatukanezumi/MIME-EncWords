use strict;
use Test::More qw(no_plan);
use MIME::EncWords qw(decode_mimewords);

{
    local($/) = '';
    open WORDS, "<testin/decode-singlebyte.txt" or die "open: $!";
    while (<WORDS>) {
	s{\A\s+|\s+\Z}{}g;    # trim

	my ($isgood, $expect, $enc) = split /\n/, $_, 3;
	$isgood = (uc($isgood) eq 'GOOD');
	$expect = eval $expect;

	my $dec = decode_mimewords($enc);
	ok((($isgood && !$@) or (!$isgood && $@)) and
           ($isgood ? ($dec eq $expect) : 1));
    }
    close WORDS;
}    

1;

