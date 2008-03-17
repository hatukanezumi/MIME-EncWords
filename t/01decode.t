use strict;
use Test;

BEGIN { plan tests => ($^V ge v5.8.1)? 42: 10 }

use MIME::EncWords qw(decode_mimewords);

my @testins = MIME::Charset::USE_ENCODE?
	      qw(decode-singlebyte decode-multibyte):
	      qw(decode-singlebyte);

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

	my $dec = decode_mimewords($enc, Charset => $charset,
				   Detect7bit => "YES");
	ok((($isgood && !$@) or (!$isgood && $@)) and
           ($isgood ? ($dec eq $expect) : 1));
	if (MIME::Charset::USE_ENCODE) {
	    my $u;
	    # Convert to other charset (or no conversion)...
	    $u = $expect;
	    Encode::from_to($u, $charset, "utf-8") if $charset;
	    $dec = decode_mimewords($enc, Charset => $charset? "utf-8": "",
				    Detect7bit => "YES");
	    ok((($isgood && !$@) or (!$isgood && $@)) and
		($isgood ? ($dec eq $u) : 1));
	    # Convert to Unicode...
	    $u = Encode::decode($charset || $ucharset || "us-ascii", $expect);
	    $dec = decode_mimewords($enc, Charset => "_UNICODE_",
				    Detect7bit => "YES");
	    ok((($isgood && !$@) or (!$isgood && $@)) and
		($isgood ? ($dec eq $u) : 1));
	}
    }
    close WORDS;
  }
}    

1;

