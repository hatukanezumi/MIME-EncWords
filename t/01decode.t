use strict;
use Test;

BEGIN { plan tests => ($^V ge v5.8.1)? 39: 10 }

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
	my $charset;
	$isgood = (uc($isgood) eq 'GOOD');
	($expect, $charset) = eval $expect;

	my $dec = decode_mimewords($enc, Charset => $charset);
	ok((($isgood && !$@) or (!$isgood && $@)) and
           ($isgood ? ($dec eq $expect) : 1));
	if (MIME::Charset::USE_ENCODE) {
	    my $u;
	    # Convert to other charset...
	    $u = $expect;
	    Encode::from_to($u, $charset || "us-ascii", "utf-8");
	    $dec = decode_mimewords($enc, Charset => "utf-8");
	    ok((($isgood && !$@) or (!$isgood && $@)) and
		($isgood ? ($dec eq $u) : 1));
	    # Convert to Unicode...
	    $u = Encode::decode($charset || "us-ascii", $expect);
	    $dec = decode_mimewords($enc, Charset => "_UNICODE_");
	    ok((($isgood && !$@) or (!$isgood && $@)) and
		($isgood ? ($dec eq $u) : 1));
	}
    }
    close WORDS;
  }
}    

1;

