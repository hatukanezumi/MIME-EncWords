
package MIME::EncWords;
use v5.8;

=head1 NAME

MIME::EncWords - deal with RFC-1522 encoded words (improved)

=head1 SYNOPSIS

I<L<MIME::EncWords> is aimed to be another implimentation
of L<MIME::Words> so that it will achive more exact conformance with
MIME specifications.  Additionally, it contains some improvements.
Following synopsis and descriptions are inherited from its inspirer.>

Before reading further, you should see L<MIME::Tools> to make sure that
you understand where this module fits into the grand scheme of things.
Go on, do it now.  I'll wait.

Ready?  Ok...

    use MIME::EncWords qw(:all);

    ### Decode the string into another string, forgetting the charsets:
    $decoded = decode_mimewords(
          'To: =?ISO-8859-1?Q?Keld_J=F8rn_Simonsen?= <keld@dkuug.dk>',
          );

    ### Split string into array of decoded [DATA,CHARSET] pairs:
    @decoded = decode_mimewords(
          'To: =?ISO-8859-1?Q?Keld_J=F8rn_Simonsen?= <keld@dkuug.dk>',
          );

    ### Encode a single unsafe word:
    $encoded = encode_mimeword("\xABFran\xE7ois\xBB");

    ### Encode a string, trying to find the unsafe words inside it:
    $encoded = encode_mimewords("Me and \xABFran\xE7ois\xBB in town");

=head1 DESCRIPTION

Fellow Americans, you probably won't know what the hell this module
is for.  Europeans, Russians, et al, you probably do.  C<:-)>.

For example, here's a valid MIME header you might get:

      From: =?US-ASCII?Q?Keith_Moore?= <moore@cs.utk.edu>
      To: =?ISO-8859-1?Q?Keld_J=F8rn_Simonsen?= <keld@dkuug.dk>
      CC: =?ISO-8859-1?Q?Andr=E9_?= Pirard <PIRARD@vm1.ulg.ac.be>
      Subject: =?ISO-8859-1?B?SWYgeW91IGNhbiByZWFkIHRoaXMgeW8=?=
       =?ISO-8859-2?B?dSB1bmRlcnN0YW5kIHRoZSBleGFtcGxlLg==?=
       =?US-ASCII?Q?.._cool!?=

The fields basically decode to (sorry, I can only approximate the
Latin characters with 7 bit sequences /o and 'e):

      From: Keith Moore <moore@cs.utk.edu>
      To: Keld J/orn Simonsen <keld@dkuug.dk>
      CC: Andr'e  Pirard <PIRARD@vm1.ulg.ac.be>
      Subject: If you can read this you understand the example... cool!

B<Supplement>: Fellow Americans, Europeans, you probably won't know
what the hell this module is for.  East Asians, et al, you probably do.
C<:-)>.

For example, here's a valid MIME header you might get:

      Subject: =?EUC-KR?B?sNTAuLinKGxhemluZXNzKSwgwvzB9ri7seIoaW1w?=
       =?EUC-KR?B?YXRpZW5jZSksILGzuLgoaHVicmlzKQ==?=

The fields basically decode to (sorry, I cannot approximate the
non-Latin multibyte characters with any 7 bit sequences):

      Subject: ???(laziness), ????(impatience), ??(hubris)

=head1 PUBLIC INTERFACE

=over 4

=cut

### Pragmas:
use strict;
use vars qw($VERSION @EXPORT_OK %EXPORT_TAGS @ISA);

### Exporting:
use Exporter;

%EXPORT_TAGS = (all => [qw(decode_mimewords
			   encode_mimeword
			   encode_mimewords)]);
Exporter::export_ok_tags(qw(all));

### Inheritance:
@ISA = qw(Exporter);

### Other modules:
use Carp;
use Encode;
use MIME::Base64;
use MIME::Charset qw(:trans);

#------------------------------
#
# Globals...
#
#------------------------------

### The package version, both in 1.23 style *and* usable by MakeMaker:
$VERSION = "0.01";

### Nonprintables (controls + x7F + 8bit):
#my $NONPRINT = "\\x00-\\x1F\\x7F-\\xFF";
my $NONPRINT = qr{[^\x20-\x7E]}; # Improvement: Unicode support.
my $UNSAFE = qr{[^\x01-\x7E]};

### Max line length:
my $MAXLINELEN = 76;

#------------------------------

# _decode_B STRING
#     Private: used by _decode_header() to decode "B" encoding.
#     Improvement by this module: sanity check on encoded sequence.
sub _decode_B {
    my $str = shift;
    unless ((length($str) % 4 == 0) and
	$str =~ m|^[A-Za-z0-9+/]+={0,2}$|) {
	return undef;
    }
    return decode_base64($str);
}

# _decode_Q STRING
#     Private: used by _decode_header() to decode "Q" encoding, which is
#     almost, but not exactly, quoted-printable.  :-P
sub _decode_Q {
    my $str = shift;
    $str =~ s/_/\x20/g;					# RFC-1522, Q rule 2
    $str =~ s/=([\da-fA-F]{2})/pack("C", hex($1))/ge;	# RFC-1522, Q rule 1
    $str;
}

# _encode_B STRING
#     Private: used by encode_mimeword() to encode "B" encoding.
sub _encode_B {
    my $str = shift;
    encode_base64($str, '');
}

# _encode_Q STRING
#     Private: used by encode_mimeword() to encode "Q" encoding, which is
#     almost, but not exactly, quoted-printable.  :-P
#     Improvement by this module: Spaces are escaped by ``_''.
sub _encode_Q {
    my $str = shift;
    # $str =~ s{([_\?\=$NONPRINT])}{sprintf("=%02X", ord($1))}eog;
    $str =~ s{(\x20)|([_?=]|$NONPRINT)}{
	defined $1? "_": sprintf("=%02X", ord($2))
	}eog;
    $str;
}

#------------------------------

=item decode_mimewords ENCODED, [OPTS...]

I<Function.>
Go through the string looking for RFC-1522-style "Q"
(quoted-printable, sort of) or "B" (base64) encoding, and decode them.

B<In an array context,> splits the ENCODED string into a list of decoded
C<[DATA, CHARSET]> pairs, and returns that list.  Unencoded
data are returned in a 1-element array C<[DATA]>, giving an effective
CHARSET of C<undef>.

    $enc = '=?ISO-8859-1?Q?Keld_J=F8rn_Simonsen?= <keld@dkuug.dk>';
    foreach (decode_mimewords($enc)) {
        print "", ($_[1] || 'US-ASCII'), ": ", $_[0], "\n";
    }

B<In a scalar context,> joins the "data" elements of the above
list together, and returns that.  I<Warning: this is information-lossy,>
and probably I<not> what you want, but if you know that all charsets
in the ENCODED string are identical, it might be useful to you.
(Before you use this, please see L<MIME::WordDecoder/unmime>,
which is probably what you want.)

In the event of a syntax error, $@ will be set to a description
of the error, but parsing will continue as best as possible (so as to
get I<something> back when decoding headers).
$@ will be false if no error was detected.

Any arguments past the ENCODED string are taken to define a hash of options:

=over 4

=item Field

Name of the mail field this string came from.  I<Currently ignored.>

=back

B<Improvement by this module>:
Adjacent encoded-words with same charset will be concatenated
(if that charset is supported by Encode) to handle multibyte sequences
safely.

B<Change by this module>:
Malformed base64 encoded-words will be kept encoded.

B<NOTE>:
Whitespaces surrounding unencoded data will not be stripped.

=cut

sub decode_mimewords {
    my $encstr = shift;
    my %params = @_;
    my @tokens;
    $@ = '';           ### error-return

    ### Collapse boundaries between adjacent encoded words:
    $encstr =~ s{(\?\=)\s*(\=\?)}{$1$2}gs;
    pos($encstr) = 0;
    ### print STDOUT "ENC = [", $encstr, "]\n";

    ### Decode:
    my ($word, $charset, $encoding, $enc, $dec);
    while (1) {
        last if (pos($encstr) >= length($encstr));
        my $pos = pos($encstr);               ### save it

        ### Case 1: are we looking at "=?..?..?="?
        if ($encstr =~    m{\G             # from where we left off..
                            =\?([^?]*)     # "=?" + charset +
                             \?([bq])      #  "?" + encoding +
                             \?([^?]+)     #  "?" + data maybe with spcs +
                             \?=           #  "?="
                            }xgi) {
	    ($word, $charset, $encoding, $enc) = ($&, $1, lc($2), $3);
	    if ($encoding eq 'q') {
		$dec = _decode_Q($enc);
	    } else {
		$dec = _decode_B($enc);
	    }
	    unless (defined $dec) {
		$@ .= qq|Illegal sequence in "$word" (pos $pos)\n|;
		push @tokens, [$word];
		next;
	    }

	    if (scalar(@tokens) and
		lc($charset) eq lc(${$tokens[-1]}[1]) and
		Encode::resolve_alias($charset)) { # Concat words if possible.
		${$tokens[-1]}[0] .= $dec;
	    } else {
		push @tokens, [$dec, $charset];
	    }
            next;
        }

        ### Case 2: are we looking at a bad "=?..." prefix?
        ### We need this to detect problems for case 3, which stops at "=?":
        pos($encstr) = $pos;               # reset the pointer.
        if ($encstr =~ m{\G=\?}xg) {
            $@ .= qq|unterminated "=?..?..?=" in "$encstr" (pos $pos)\n|;
            push @tokens, ['=?'];
            next;
        }

        ### Case 3: are we looking at ordinary text?
        pos($encstr) = $pos;               # reset the pointer.
        if ($encstr =~ m{\G                # from where we left off...
                         ([\x00-\xFF]*?    #   shortest possible string,
                          \n*)             #   followed by 0 or more NLs,
                         (?=(\Z|=\?))      # terminated by "=?" or EOS
                        }xg) {
            length($1) or die "MIME::EncWords: internal logic err: empty token\n";
            push @tokens, [$1];
            next;
        }

        ### Case 4: bug!
        die "MIME::EncWords: unexpected case:\n($encstr) pos $pos\n\t".
            "Please alert developer.\n";
    }
    return (wantarray ? @tokens : join('',map {$_->[0]} @tokens));
}

#------------------------------

=item encode_mimeword RAW, [ENCODING], [CHARSET]

I<Function.>
Encode a single RAW "word" that has unsafe characters.
The "word" will be encoded in its entirety.

    ### Encode "<<Franc,ois>>":
    $encoded = encode_mimeword("\xABFran\xE7ois\xBB");

You may specify the ENCODING (C<"Q"> or C<"B">), which defaults to C<"Q">.
B<Improvement by this module>:
You may also specify it as ``special'' value: C<"S"> to choose shorter
one of either C<"Q"> or C<"B">.

You may specify the CHARSET, which defaults to C<iso-8859-1>.

B<Change by this module>:
Spaces will be escaped with ``_'' by C<"Q"> encoding.

=cut

sub encode_mimeword {
    my $word = shift;
    my $encoding = uc(shift || 'Q');
    my $charset  = uc(shift || 'ISO-8859-1');

    my $encstr;
    if ($encoding eq 'Q') {
	$encstr = &_encode_Q($word);
    } elsif ($encoding eq "S") {
	if (encoded_header_len($word, "B", $charset) <
	    encoded_header_len($word, "Q", $charset)) {
	    $encoding = "B";
	    $encstr = &_encode_B($word);
	} else {
	    $encoding = "Q";
	    $encstr = &_encode_Q($word);
	}
    } else { # "B"
	$encoding = "B";
	$encstr = &_encode_B($word);
    }

    "=?$charset?$encoding?$encstr?=";
}

#------------------------------

=item encode_mimewords RAW, [OPTS]

I<Function.>
Given a RAW string, try to find and encode all "unsafe" sequences
of characters:

    ### Encode a string with some unsafe "words":
    $encoded = encode_mimewords("Me and \xABFran\xE7ois\xBB");

Returns the encoded string.

B<Improvement by this module>:
RAW may be a Unicode string.
Furthermore, RAW may be an arrayref which is reference to that returned
by decode_mimewords on array context.  In latter case Charset option
(see below) will be overridden.

Any arguments past the RAW string are taken to define a hash of options:

=over 4

=item Charset

Encode all unsafe stuff with this charset.  Default is 'ISO-8859-1',
a.k.a. "Latin-1".

=item Detect7bit

B<Improvement by this modlue>:
When L<"Encoding"> (see below) is specified as C<"a"> and Charset
option is unknown, try to detect 7-bit charset on given RAW string.
Default is C<"YES">.

=item Encoding

The encoding to use, C<"q"> or C<"b">.  The default is C<"q">.
B<Improvement by this module>:
You may also specify ``special'' values: C<"a"> will automatically choose
recommended encoding to use (with charset conversion, if alternative
charset is recommended); C<"s"> will choose shorter one of either C<"q">
or C<"b">.

=item Field

Name of the mail field this string will be used in.
B<Improvement by this module>:
Length of mail field name will be considered in the first line of
encoded header.

=back

B<Warning:> this is a quick-and-dirty solution, intended for character
sets which overlap ASCII.  B<It does not comply with the RFC-1522
rules regarding the use of encoded words in message headers>.
You may want to roll your own variant,
using C<encode_mimeword()>, for your application.
I<Thanks to Jan Kasprzak for reminding me about this problem.>

B<Change by this module>:
Encoded-words are concatenated then are splitted taking care of character
boundaries of multibyte sequences.

=cut

sub encode_mimewords  {
    my $words = shift;
    my %params = @_;
    my $charset = $params{'Charset'};
    my $detect7bit = uc($params{'Detect7bit'} || "YES");
    my $encoding = uc($params{'Encoding'});
    my $header_name = $params{'Field'};
    my $firstlinelen = $MAXLINELEN;
    if ($header_name) {
	$firstlinelen -= length($header_name.': ');
    }

    unless (ref($words) eq "ARRAY") {
	$words = [[$words, $charset]];
    }

    # Translate / concatenate words.
    my @triplets;
    foreach (@$words) {
	my ($s, $cset) = @$_;
	my $enc;

	# Unicode string should be encoded by given charset.
	# Unsupported charset will be fallbacked to UTF-8.
	if (Encode::is_utf8($s)) {
	    unless (Encode::resolve_alias($cset)) {
		if ($s !~ $UNSAFE) {
		    $cset = "US-ASCII";
		} else {
		    $cset = "UTF-8";
		}
	    }
	    $s = Encode::encode($cset, $s);
	}

	# Determine charset and encoding.
	if ($encoding eq "A") {
	    ($s, $cset, $enc) =
		header_encode($s, $cset, Detect7bit => $detect7bit);
	} else {
	    $cset ||= ($s !~ $UNSAFE)? "US-ASCII": "ISO-8859-1";
	    $enc = $encoding ||
		(($s !~ $UNSAFE and $cset eq "US-ASCII")? undef: "Q");
	}

	# Concatenate adjacent ``words'' so that multibyte sequences will
	# be handled safely.
	# Note: Encoded-word and unencoded text must not adjoin without
	# separating whitespace(s).
	if (scalar(@triplets)) {
	    my ($last, $lastenc, $lastcset) = @{$triplets[-1]};
	    if (uc($lastcset) eq uc($cset) and uc($lastenc) eq uc($enc) and
		Encode::resolve_alias($cset)) {
		${$triplets[-1]}[0] .= $s;
		next;
	    } elsif (!$lastenc and $enc and $last !~ /[\t ]$/) {
		if ($last =~ /^(.*)[\t ]([\x21-\x7E]+)$/s) {
		    ${$triplets[-1]}[0] = $1." ";
		    $s = $2.$s;
		} elsif (uc($lastcset) eq "US-ASCII") {
		    ${$triplets[-1]}[0] .= $s;
		    ${$triplets[-1]}[1] = $enc;
		    ${$triplets[-1]}[2] = $cset;
		    next;
		}
	    } elsif ($lastenc and !$enc and $s !~ /^[\t ]/) {
		if ($s =~ /^([\x21-\x7E]+)[\t ](.*)$/s) {
		    ${$triplets[-1]}[0] .= $1;
		    $s = " ".$2;
		} elsif (uc($cset) eq "US-ASCII") {
		    ${$triplets[-1]}[0] .= $s;
		    next;
		}
	    }
	}
	push @triplets, [$s, $enc, $cset];
    }

    # Split.
    my @splitted;
    my $restlen = $firstlinelen;
    my $lastlen = 0;
    foreach (@triplets) {
	my ($s, $enc, $cset) = @$_;

	my $restlen = $restlen - $lastlen - 1;
	if ($restlen < ($enc? encoded_header_len('', $enc, $cset): 1)) {
	    $restlen = $MAXLINELEN - 1;
	}

	push @splitted, &_split($s, $enc, $cset, $restlen);
	my ($last, $lastenc, $lastcset) = @{$splitted[-1]};
	if ($lastenc) {
	    $lastlen = encoded_header_len($last, $lastenc, $lastcset);
	} else {
	    $lastlen = length($last);
	}
    }

    # Do encoding.
    my @lines;
    my $linelen = $firstlinelen;
    foreach (@splitted) {
	my ($str, $encoding, $charset) = @$_;
	next unless length($str);

	my $s;
	if (!$encoding) {
	    $s = $str;
	} else {
	    $s = &encode_mimeword($str, $encoding, $charset);
	}
	my $spc = (scalar(@lines) and $lines[-1] =~ / $/)? '': ' ';

	if (!scalar(@lines)) {
	    $s =~ s/^\s+//;
	    push @lines, $s;
	} elsif (length($lines[-1]) + length($s) <= $linelen) {
	    $lines[-1] .= $spc.$s;
	} else {
	    $s =~ s/^\s+//;
	    push @lines, $s;
	    $linelen = $MAXLINELEN - 1;
	}
    }

    join("\n ", @lines);
}

#------------------------------

# _split RAW, ENCODING, CHARSET, ROOM_OF_FIRST_LINE
#     Private: used by encode_mimewords() to split a string into
#     (encoded or non-encoded) words.
#     Returns an array of arrayrefs [SUBSTRING, ENCODING, CHARSET].
sub _split {
    my $str = shift;
    my $encoding = shift;
    my $charset = shift;
    my $restlen = shift;

    if (!$charset or $charset eq '8BIT') { # Undecodable.
	return ([$str, undef, $charset]);
    }
    unless (Encode::resolve_alias($charset)) { # Unsuppoted charset.
	return ([$str, $encoding, $charset]);
    }
    if (!$encoding and $charset eq 'US-ASCII') {
	return &_split_ascii($str, $restlen);
    }

    my (@splitted, $ustr, $first);
    while (length($str)) {
	if (encoded_header_len($str, $encoding, $charset) <= $restlen) {
	    push @splitted, [$str, $encoding, $charset];
	    last;
	}
	$ustr = $str;
	$ustr = Encode::decode($charset, $ustr);
	($first, $str) = &_clip_unsafe($ustr, $encoding, $charset, $restlen);
	push @splitted, [$first, $encoding, $charset];
	$restlen = $MAXLINELEN - 1;
    }
    return @splitted;
}

# _split_ascii RAW, ROOM_OF_FIRST_LINE
#     Private: used by encode_mimewords() to split an ASCII string into
#     (encoded or non-encoded) words.
#     Returns an array of arrayrefs [SUBSTRING, ENCODING, "US-ASCII"].
sub _split_ascii {
    my $s = shift;
    my $firstlinelen = shift;
    my $restlen = $firstlinelen;

    my @splitted;
    foreach my $line (split(/[\r\n]+/, $s)) {
	$line =~ s/^[\t ]+//;

	if (length($line) < $restlen) {
	    push @splitted, [$line, undef, 'US-ASCII'];
	    $restlen = $MAXLINELEN - 1;
	    next;
	}

	my @words;
        my $spc;
	foreach my $word (split(/([\t ]+)/, $line)) {
	    unless ($word =~ /[^\t ]/) {
		$spc = $word;
		next;
	    }
	    if (scalar(@words)) {
		if (length($spc.$word) < $restlen) {
		    $words[-1] .= $spc.$word;
		    $restlen -= length($spc.$word);
		} else {
		    push @words, $word;
		    $restlen = $MAXLINELEN - length($word) - 1;
		}
	    } else {
		push @words, $word;
		$restlen -= length($word);
	    }
	}
	foreach (@words) {
	    push @splitted, [$_, undef, 'US-ASCII'];
	}
    }
    return @splitted;
}

# _clip_unsafe UNICODE, ENCODING, CHARSET, ROOM_OF_FIRST_LINE
#     Private: used by encode_mimewords() to bite off one encodable
#     ``word'' from a Unicode string.
sub _clip_unsafe {
    my $ustr = shift;
    my $encoding = shift;
    my $charset = shift;
    my $restlen = shift;
    return ("", "") unless length($ustr);

    # Seek maximal division point.
    my ($shorter, $longer) = (0, length($ustr)-1);
    while ($shorter < $longer) {
	my $cur = int(($shorter + $longer + 1) / 2);
	my $enc = Encode::encode($charset, substr($ustr, 0, $cur));
	my $elen = encoded_header_len($enc, $encoding, $charset);
	if ($elen <= $restlen) {
	    $shorter = $cur;
	} else {
	    $longer = $cur - 1;
	}
    }

    # Make sure that combined characters won't be divided.
    my ($fenc, $renc);
    my $max = length($ustr);
    while (1) {
	$@ = '';
	eval {
	    ($fenc, $renc) =
		(substr($ustr, 0, $shorter), substr($ustr, $shorter));
	    $fenc = &Encode::encode($charset, $fenc, Encode::FB_CROAK);
	    $renc = &Encode::encode($charset, $renc, Encode::FB_CROAK);
	};
	last unless ($@);

	unless ($shorter < $max) { # Unencodable characters are included.
	    return (Encode::encode($charset, $ustr), "");
	}
	$shorter++;
    }

    if (length($fenc)) {
	return ($fenc, $renc);
    } else {
	return ($renc, "");
    }
}

#------------------------------

=back

=head1 AUTHORS

The original version of function decode_mimewords() is derived from
MIME::Words module that was written by:
    Eryq (F<eryq@zeegee.com>), ZeeGee Software Inc (F<http://www.zeegee.com>).
    David F. Skoll (dfs@roaringpenguin.com) http://www.roaringpenguin.com

Other stuffs are rewritten or added by:
    Hatuka*nezumi - IKEDA Soji (F<hatuka@nezumi.nu>).

All rights reserved.  This program is free software; you can redistribute
it and/or modify it under the same terms as Perl itself.

=cut

1;
