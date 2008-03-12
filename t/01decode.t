use strict;
use Test;

BEGIN { plan tests => ($^V ge v5.8.1)? 20: 10 }

my $_UNICODE = ($^V ge v5.8.1);

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
	if ($_UNICODE) {
	    $dec = decode_mimewords($enc, Charset => "utf-8");
	    Encode::from_to($expect, "iso-8859-1", "utf-8");
	    ok((($isgood && !$@) or (!$isgood && $@)) and
		($isgood ? ($dec eq $expect) : 1));
	}
    }
    close WORDS;
}    

1;

