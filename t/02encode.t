use strict;
use Test;

BEGIN { plan tests => ($^V ge v5.8.1)? 15: 10 }

use MIME::Charset qw(header_encode);
use MIME::EncWords qw(encode_mimewords);

my @testins = MIME::Charset::USE_ENCODE?
	      qw(encode-singlebyte encode-multibyte):
	      qw(encode-singlebyte);

{
  local($/) = '';
  foreach my $in (@testins) {
    open WORDS, "<testin/$in.txt" or die "open: $!";
    while (<WORDS>) {
	s{\A\s+|\s+\Z}{}g;    # trim

	my ($isgood, $dec, $expect, @params);
	my @l = split /\n/, $_;
	$isgood = shift @l;
	$dec = shift @l;
	$expect = join("\n", @l);
	$isgood = (uc($isgood) eq 'GOOD');
	@params = eval $dec;

	my $enc = encode_mimewords(@params, Encoding=>"A",
				   MaxLineLen => 76,
				   Minimal => "YES");
	ok((($isgood && !$@) or (!$isgood && $@)) and
           ($isgood ? ($enc eq $expect) : 1));
    }
    close WORDS;
  }
}    

1;

