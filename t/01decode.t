use strict;
use Test;

BEGIN { plan tests => ($] >= 5.008001)? 48: 16 }

use MIME::EncWords qw(decode_mimewords);
$MIME::EncWords::Config = {
    Detect7bit => 'YES',
    Mapping => 'EXTENDED',
    Replacement => 'DEFAULT',
    Charset => 'ISO-8859-1',
    Encoding => 'A',
    Field => undef,
    Folding => "\n",
    MaxLineLen => 76,
    Minimal => 'YES',
};

my @testins = qw(decode-singlebyte decode-multibyte decode-ascii);

{
  local($/) = '';
  foreach my $in (@testins) {
    open WORDS, "<testin/$in.txt" or die "open: $!";
    while (<WORDS>) {
	s{\A\s+|\s+\Z}{}g;    # trim

	my ($isgood, $expect, $enc) = split /\n/, $_, 3;
	my ($charset, $ucharset);
	$isgood = (uc($isgood) eq 'GOOD');
	($expect, $charset, $ucharset) = eval $expect;

	# Convert to raw data...
	my $dec = decode_mimewords($enc);
	ok((($isgood && !$@) or (!$isgood && $@)) and
           ($isgood ? ($dec eq $expect) : 1));
	if (MIME::Charset::USE_ENCODE) {
	    my $u;
	    # Convert to other charset (or no conversion)...
	    $u = $expect;
	    Encode::from_to($u, $charset, "utf-8") if $charset;
	    $dec = decode_mimewords($enc, Charset => $charset? "utf-8": "");
	    ok((($isgood && !$@) or (!$isgood && $@)) and
		($isgood ? ($dec eq $u) : 1));
	    # Convert to Unicode...
	    $u = Encode::decode($charset || $ucharset || "us-ascii", $expect);
	    $dec = decode_mimewords($enc, Charset => "_UNICODE_");
	    ok((($isgood && !$@) or (!$isgood && $@)) and
		($isgood ? ($dec eq $u) : 1));
	}
    }
    close WORDS;
  }
}    

1;

