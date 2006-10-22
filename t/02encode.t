use strict;
use Test;

BEGIN { plan tests => 9 }

use MIME::Charset qw(header_encode);
use MIME::EncWords qw(encode_mimewords);

{
    local($/) = '';
    open WORDS, "<testin/encode-singlebyte.txt" or die "open: $!";
    while (<WORDS>) {
	s{\A\s+|\s+\Z}{}g;    # trim

	my ($isgood, $dec, $expect) = split /\n/, $_, 3;
	$isgood = (uc($isgood) eq 'GOOD');
	$dec = eval $dec;

	my $enc = encode_mimewords($dec, Charset=>"ISO-8859-1", Encoding=>"A");
	ok((($isgood && !$@) or (!$isgood && $@)) and
           ($isgood ? ($enc eq $expect) : 1));
    }
    close WORDS;
}    

1;

