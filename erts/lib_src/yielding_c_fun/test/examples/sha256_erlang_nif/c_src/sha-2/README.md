# sha-2 [![Build Status](https://travis-ci.org/amosnier/sha-2.svg?branch=master)](https://travis-ci.org/amosnier/sha-2)

## Contents

SHA-2 algorithm implementations.

At the moment, only SHA-256 is implemented.

## Design criteria

- Easy to test, include in any project, compile and link.

- ANSI C with as little specific C99 as possible (e.g. extended
  integer types are used, but not bool).

- Portable. Makes no assumptions on the target system's endianess or
  word size.

- The SHA-256 implementation is a straightforward implementation of
  the algorithm specified on
  [Wikipedia](https://en.wikipedia.org/wiki/SHA-2).

## Notes

The Makefile is as minimal as possible. No effort was put into making
it general. Its purpose is mainly to ease testing for the developer's
host machine. The actual implementation is however extremely easy to
include in any project, may it use GNU make or any other build tool.

## Code review

This code has been reviewed at [Stack Exchange CODE
REVIEW](https://codereview.stackexchange.com/questions/182812/self-contained-sha-256-implementation-in-c),
and the implementation has been improved accordingly.

## Testing

Testing is continuously performed on Travis CI (see above).

Apart from that, the implementation has been successfully tested on an x86-64 machine
under Linux as well as on a 16-bit DSP. On the x86-64 machine, all the
available NIST test vectors where successfully tested ([SHA-256
examples](https://csrc.nist.gov/CSRC/media/Projects/Cryptographic-Standards-and-Guidelines/documents/examples/SHA256.pdf)
and [SHA-2 Additional
examples](https://csrc.nist.gov/CSRC/media/Projects/Cryptographic-Standards-and-Guidelines/documents/examples/SHA2_Additional.pdf),
plus a few others).

In particular:

```
Input Message: "abc"
Message Digest is BA7816BF 8F01CFEA 414140DE 5DAE2223 B00361A3 96177A9C B410FF61 F20015AD
```

```
Input Message: "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
Message Digest is 248D6A61 D20638B8 E5C02693 0C3E6039 A33CE459 64FF2167 F6ECEDD4 19DB06C1
```

```
SHA-256 Test Data
#1) 1 byte 0xbd 
68325720 aabd7c82 f30f554b 313d0570 c95accbb 7dc4b5aa e11204c0 8ffe732b
#2) 4 bytes 0xc98c8e55 
7abc22c0 ae5af26c e93dbb94 433a0e0b 2e119d01 4f8e7f65 bd56c61c cccd9504 
#3) 55 bytes of zeros 
02779466 cdec1638 11d07881 5c633f21 90141308 1449002f 24aa3e80 f0b88ef7 
#4) 56 bytes of zeros 
d4817aa5 497628e7 c77e6b60 6107042b bba31308 88c5f47a 375e6179 be789fbb 
#5) 57 bytes of zeros 
65a16cb7 861335d5 ace3c607 18b5052e 44660726 da4cd13b b745381b 235a1785 
#6) 64 bytes of zeros 
f5a5fd42 d16a2030 2798ef6e d309979b 43003d23 20d9f0e8 ea9831a9 2759fb4b 
#7) 1000 bytes of zeros 
541b3e9d aa09b20b f85fa273 e5cbd3e8 0185aa4e c298e765 db87742b 70138a53 
#8) 1000 bytes of 0x41 ‘A’ 
c2e68682 3489ced2 017f6059 b8b23931 8b6364f6 dcd835d0 a519105a 1eadd6e4 
#9) 1005 bytes of 0x55 ‘U’ 
f4d62dde c0f3dd90 ea1380fa 16a5ff8d c4c54b21 740650f2 4afc4120 903552b0 
#10) 1000000 bytes of zeros 
d29751f2 649b32ff 572b5e0a 9f541ea6 60a50f94 ff0beedf b0b692b9 24cc8025 
#11) 0x20000000 (536870912) bytes of 0x5a ‘Z’ 
15a1868c 12cc5395 1e182344 277447cd 0979536b adcc512a d24c67e9 b2d4f3dd 
#12) 0x41000000 (1090519040) bytes of zeros 
461c19a9 3bd4344f 9215f5ec 64357090 342bc66b 15a14831 7d276e31 cbc20b53 
#13) 0x6000003e (1610612798) bytes of 0x42 ‘B’ 
c23ce8a7 895f4b21 ec0daf37 920ac0a2 62a22004 5a03eb2d fed48ef9 b05aabea
```

## License

This repository is made available in the public domain. See [LICENSE
FILE](LICENSE).

## Reference implementation

I had missed that when I made this implementation but [RFC 6234, chapter 8](https://tools.ietf.org/html/rfc6234#section-8) actually includes a reference implementation in C that is (at least in ambition) broader in scope than this one. I have however neither compiled nor tested it.
