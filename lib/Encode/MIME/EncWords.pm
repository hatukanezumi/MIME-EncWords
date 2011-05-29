# -*- perl -*-

package Encode::MIME::EncWords;
use strict;
use warnings;
no warnings 'redefine';

our $VERSION = '0.001';
use MIME::EncWords;
use Carp;

my %seed = (
    Detect7bit => 'NO',      # *
    Mapping   => 'STANDARD', # *
    #Replacement --- given by encode()/decode().
    #Charset --- "_UNICODE_" for decode().
    Encoding  => 'A',
    Field     => '',
    Folding   => "\n",
    MaxLineLen => 75,        # *
    Minimal   => 'YES',
);

$Encode::Encoding{'MIME-EncWords'} = bless {
    %seed,
    Charset  => 'UTF-8',
    Name     => 'MIME-EncWords',
} => __PACKAGE__;

$Encode::Encoding{'MIME-EncWords-B'} = bless {
    %seed,
    Charset  => 'UTF-8',
    Encoding => 'B',
    Name     => 'MIME-EncWords-B',
} => __PACKAGE__;

$Encode::Encoding{'MIME-EncWords-Q'} = bless {
    %seed,
    Charset  => 'UTF-8',
    Encoding => 'Q',
    Name     => 'MIME-EncWords-Q',
} => __PACKAGE__;

$Encode::Encoding{'MIME-EncWords-ISO_2022_JP'} = bless {
    %seed,
    Charset  => 'ISO-2022-JP',
    Name     => 'MIME-EncWords-ISO_2022_JP',
} => __PACKAGE__;

use base qw(Encode::Encoding);

sub needs_lines { 1 }
sub perlio_ok   { 0 }

sub decode($$;$) {
    use utf8;
    my ( $obj, $str, $chk ) = @_;

    $str = MIME::EncWords::decode_mimewords($str,
                                            Charset => '_UNICODE_',
                                            Detect7Bit => $obj->{'Detect7bit'},
                                            Mapping => $obj->{'Mapping'});
    $_[1] = $str if $chk;
    return $str;
}

sub encode($$;$) {
    my ( $obj, $str, $chk ) = @_;
    my @opts = map { ($_ => $obj->{$_}) } keys %seed;

    $_[1] = '' if $chk;
    return MIME::EncWords::encode_mimewords($str, @opts,
                                            Charset => $obj->{'Charset'},
                                            Replacement => ($chk || 0));
}

1;
__END__

=head1 NAME

Encode::MIME::EncWords -- MIME 'B' and 'Q' header encoding

=head1 SYNOPSIS

    use Encode::MIME::EncWords;
    use Encode qw/encode decode/;
    $utf8   = decode('MIME-EncWords', $header);
    $header = encode('MIME-EncWords', $utf8);

=head1 ABSTRACT

This module implements RFC 2047 MIME Header Encoding.  There are 4
variant encoding names: C<MIME-EncWords>, C<MIME-EncWords-B>,
C<MIME-EncWords-Q> and C<MIME-EncWords-ISO_2022_JP>.

                             decode()      encode()
  -----------------------------------------------------------------
  MIME-EncWords              Both B and Q  shorter one of -B and -Q
  MIME-EncWords-B                ditto     =?UTF-8?B?....?=
  MIME-EncWords-Q                ditto     =?UTF-8?Q?....?=
  MIME-EncWords-ISO_2022_JP      ditto     =?ISO-2022-JP?B?....?=

=head1 DESCRIPTION

To find out how to use this module in detail, see L<Encode>.

=head1 VERSION

B<This package is alpha release for experimental purpose>.

Development versions of this package may be found at
L<http://hatuka.nezumi.nu/repos/MIME-EncWords/>.

=head1 SEE ALSO

L<Encode>, L<Encode::MIME::Header>, L<MIME::EncWords>.

RFC 2047 I<MIME (Multipurpose Internet Mail Extensions) Part Three:
Message Header Extensions for Non-ASCII Text>.

=head1 AUTHOR

Hatuka*nezumi - IKEDA Soji <hatuka(at)nezumi.nu>

=head1 COPYRIGHT

Copyright (C) 2011 Hatuka*nezumi - IKEDA Soji.
This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=cut
