/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2000-2016. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * %CopyrightEnd%
 */
package com.ericsson.otp.erlang;

// package scope
class OtpMD5 {

    /*
     * * MD5 constants
     */
    static final long S11 = 7;
    static final long S12 = 12;
    static final long S13 = 17;
    static final long S14 = 22;
    static final long S21 = 5;
    static final long S22 = 9;
    static final long S23 = 14;
    static final long S24 = 20;
    static final long S31 = 4;
    static final long S32 = 11;
    static final long S33 = 16;
    static final long S34 = 23;
    static final long S41 = 6;
    static final long S42 = 10;
    static final long S43 = 15;
    static final long S44 = 21;

    /*
     * Has to be this large to avoid sign problems
     */

    private final long state[] = { 0x67452301L, 0xefcdab89L, 0x98badcfeL,
            0x10325476L };
    private final long count[] = { 0L, 0L };
    private final int buffer[];

    public OtpMD5() {
        buffer = new int[64];
        int i;
        for (i = 0; i < 64; ++i) {
            buffer[i] = 0;
        }
    }

    private int[] to_bytes(final String s) {
        final char tmp[] = s.toCharArray();
        final int ret[] = new int[tmp.length];
        int i;

        for (i = 0; i < tmp.length; ++i) {
            ret[i] = tmp[i] & 0xFF;
        }
        return ret;
    }

    private int[] clean_bytes(final int bytes[]) {
        final int ret[] = new int[bytes.length];
        int i;

        for (i = 0; i < bytes.length; ++i) {
            ret[i] = bytes[i] & 0xFF;
        }
        return ret;
    }

    /*
     * * A couple of operations where 32 bit over/under-flow is expected
     */

    private long shl(final long what, final int steps) {
        return what << steps & 0xFFFFFFFFL;
    }

    private long shr(final long what, final int steps) {
        return what >>> steps;
    }

    private long plus(final long a, final long b) {
        return a + b & 0xFFFFFFFFL;
    }

    private long not(final long x) {
        return ~x & 0xFFFFFFFFL;
    }

    private void to_buffer(final int to_start, final int[] from,
            final int from_start, final int num) {
        int ix = num;
        int to_ix = to_start;
        int from_ix = from_start;
        while (ix-- > 0) {
            buffer[to_ix++] = from[from_ix++];
        }
    }

    private void do_update(final int bytes[]) {
        int index = (int) (count[0] >>> 3 & 0x3F);
        final long inlen = bytes.length;
        final long addcount = shl(inlen, 3);
        final long partlen = 64 - index;
        int i;

        count[0] = plus(count[0], addcount);

        if (count[0] < addcount) {
            ++count[1];
        }

        count[1] = plus(count[1], shr(inlen, 29));

        // dumpstate();

        if (inlen >= partlen) {
            to_buffer(index, bytes, 0, (int) partlen);
            transform(buffer, 0);

            for (i = (int) partlen; i + 63 < inlen; i += 64) {
                transform(bytes, i);
            }

            index = 0;
        } else {
            i = 0;
        }

        /* dumpstate(); */

        to_buffer(index, bytes, i, (int) inlen - i);

        /* dumpstate(); */

    }

    @SuppressWarnings("unused")
    private void dumpstate() {
        System.out.println("state = {" + state[0] + ", " + state[1] + ", "
                + state[2] + ", " + state[3] + "}");
        System.out.println("count = {" + count[0] + ", " + count[1] + "}");
        System.out.print("buffer = {");
        int i;
        for (i = 0; i < 64; ++i) {
            if (i > 0) {
                System.out.print(", ");
            }
            System.out.print(buffer[i]);
        }
        System.out.println("}");
    }

    /*
     * * The transformation functions
     */

    private long F(final long x, final long y, final long z) {
        return x & y | not(x) & z;
    }

    private long G(final long x, final long y, final long z) {
        return x & z | y & not(z);
    }

    private long H(final long x, final long y, final long z) {
        return x ^ y ^ z;
    }

    private long I(final long x, final long y, final long z) {
        return y ^ (x | not(z));
    }

    private long ROTATE_LEFT(final long x, final long n) {
        return shl(x, (int) n) | shr(x, (int) (32 - n));
    }

    private long FF(final long a, final long b, final long c, final long d,
            final long x, final long s, final long ac) {
        long tmp = plus(a, plus(plus(F(b, c, d), x), ac));
        tmp = ROTATE_LEFT(tmp, s);
        return plus(tmp, b);
    }

    private long GG(final long a, final long b, final long c, final long d,
            final long x, final long s, final long ac) {
        long tmp = plus(a, plus(plus(G(b, c, d), x), ac));
        tmp = ROTATE_LEFT(tmp, s);
        return plus(tmp, b);
    }

    private long HH(final long a, final long b, final long c, final long d,
            final long x, final long s, final long ac) {
        long tmp = plus(a, plus(plus(H(b, c, d), x), ac));
        tmp = ROTATE_LEFT(tmp, s);
        return plus(tmp, b);
    }

    private long II(final long a, final long b, final long c, final long d,
            final long x, final long s, final long ac) {
        long tmp = plus(a, plus(plus(I(b, c, d), x), ac));
        tmp = ROTATE_LEFT(tmp, s);
        return plus(tmp, b);
    }

    private void decode(final long output[], final int input[],
            final int in_from, final int len) {
        int i, j;

        for (i = 0, j = 0; j < len; i++, j += 4) {
            output[i] = input[j + in_from] | shl(input[j + in_from + 1], 8)
                    | shl(input[j + in_from + 2], 16)
                    | shl(input[j + in_from + 3], 24);
        }
    }

    private void transform(final int block[], final int from) {
        long a = state[0];
        long b = state[1];
        long c = state[2];
        long d = state[3];
        final long x[] = new long[16];

        decode(x, block, from, 64);

        a = FF(a, b, c, d, x[0], S11, 0xd76aa478L); /* 1 */
        d = FF(d, a, b, c, x[1], S12, 0xe8c7b756L); /* 2 */
        c = FF(c, d, a, b, x[2], S13, 0x242070dbL); /* 3 */
        b = FF(b, c, d, a, x[3], S14, 0xc1bdceeeL); /* 4 */
        a = FF(a, b, c, d, x[4], S11, 0xf57c0fafL); /* 5 */
        d = FF(d, a, b, c, x[5], S12, 0x4787c62aL); /* 6 */
        c = FF(c, d, a, b, x[6], S13, 0xa8304613L); /* 7 */
        b = FF(b, c, d, a, x[7], S14, 0xfd469501L); /* 8 */
        a = FF(a, b, c, d, x[8], S11, 0x698098d8L); /* 9 */
        d = FF(d, a, b, c, x[9], S12, 0x8b44f7afL); /* 10 */
        c = FF(c, d, a, b, x[10], S13, 0xffff5bb1L); /* 11 */
        b = FF(b, c, d, a, x[11], S14, 0x895cd7beL); /* 12 */
        a = FF(a, b, c, d, x[12], S11, 0x6b901122L); /* 13 */
        d = FF(d, a, b, c, x[13], S12, 0xfd987193L); /* 14 */
        c = FF(c, d, a, b, x[14], S13, 0xa679438eL); /* 15 */
        b = FF(b, c, d, a, x[15], S14, 0x49b40821L); /* 16 */

        /* Round 2 */
        a = GG(a, b, c, d, x[1], S21, 0xf61e2562L); /* 17 */
        d = GG(d, a, b, c, x[6], S22, 0xc040b340L); /* 18 */
        c = GG(c, d, a, b, x[11], S23, 0x265e5a51L); /* 19 */
        b = GG(b, c, d, a, x[0], S24, 0xe9b6c7aaL); /* 20 */
        a = GG(a, b, c, d, x[5], S21, 0xd62f105dL); /* 21 */
        d = GG(d, a, b, c, x[10], S22, 0x2441453L); /* 22 */
        c = GG(c, d, a, b, x[15], S23, 0xd8a1e681L); /* 23 */
        b = GG(b, c, d, a, x[4], S24, 0xe7d3fbc8L); /* 24 */
        a = GG(a, b, c, d, x[9], S21, 0x21e1cde6L); /* 25 */
        d = GG(d, a, b, c, x[14], S22, 0xc33707d6L); /* 26 */
        c = GG(c, d, a, b, x[3], S23, 0xf4d50d87L); /* 27 */
        b = GG(b, c, d, a, x[8], S24, 0x455a14edL); /* 28 */
        a = GG(a, b, c, d, x[13], S21, 0xa9e3e905L); /* 29 */
        d = GG(d, a, b, c, x[2], S22, 0xfcefa3f8L); /* 30 */
        c = GG(c, d, a, b, x[7], S23, 0x676f02d9L); /* 31 */
        b = GG(b, c, d, a, x[12], S24, 0x8d2a4c8aL); /* 32 */

        /* Round 3 */
        a = HH(a, b, c, d, x[5], S31, 0xfffa3942L); /* 33 */
        d = HH(d, a, b, c, x[8], S32, 0x8771f681L); /* 34 */
        c = HH(c, d, a, b, x[11], S33, 0x6d9d6122L); /* 35 */
        b = HH(b, c, d, a, x[14], S34, 0xfde5380cL); /* 36 */
        a = HH(a, b, c, d, x[1], S31, 0xa4beea44L); /* 37 */
        d = HH(d, a, b, c, x[4], S32, 0x4bdecfa9L); /* 38 */
        c = HH(c, d, a, b, x[7], S33, 0xf6bb4b60L); /* 39 */
        b = HH(b, c, d, a, x[10], S34, 0xbebfbc70L); /* 40 */
        a = HH(a, b, c, d, x[13], S31, 0x289b7ec6L); /* 41 */
        d = HH(d, a, b, c, x[0], S32, 0xeaa127faL); /* 42 */
        c = HH(c, d, a, b, x[3], S33, 0xd4ef3085L); /* 43 */
        b = HH(b, c, d, a, x[6], S34, 0x4881d05L); /* 44 */
        a = HH(a, b, c, d, x[9], S31, 0xd9d4d039L); /* 45 */
        d = HH(d, a, b, c, x[12], S32, 0xe6db99e5L); /* 46 */
        c = HH(c, d, a, b, x[15], S33, 0x1fa27cf8L); /* 47 */
        b = HH(b, c, d, a, x[2], S34, 0xc4ac5665L); /* 48 */

        /* Round 4 */
        a = II(a, b, c, d, x[0], S41, 0xf4292244L); /* 49 */
        d = II(d, a, b, c, x[7], S42, 0x432aff97L); /* 50 */
        c = II(c, d, a, b, x[14], S43, 0xab9423a7L); /* 51 */
        b = II(b, c, d, a, x[5], S44, 0xfc93a039L); /* 52 */
        a = II(a, b, c, d, x[12], S41, 0x655b59c3L); /* 53 */
        d = II(d, a, b, c, x[3], S42, 0x8f0ccc92L); /* 54 */
        c = II(c, d, a, b, x[10], S43, 0xffeff47dL); /* 55 */
        b = II(b, c, d, a, x[1], S44, 0x85845dd1L); /* 56 */
        a = II(a, b, c, d, x[8], S41, 0x6fa87e4fL); /* 57 */
        d = II(d, a, b, c, x[15], S42, 0xfe2ce6e0L); /* 58 */
        c = II(c, d, a, b, x[6], S43, 0xa3014314L); /* 59 */
        b = II(b, c, d, a, x[13], S44, 0x4e0811a1L); /* 60 */
        a = II(a, b, c, d, x[4], S41, 0xf7537e82L); /* 61 */
        d = II(d, a, b, c, x[11], S42, 0xbd3af235L); /* 62 */
        c = II(c, d, a, b, x[2], S43, 0x2ad7d2bbL); /* 63 */
        b = II(b, c, d, a, x[9], S44, 0xeb86d391L); /* 64 */

        state[0] = plus(state[0], a);
        state[1] = plus(state[1], b);
        state[2] = plus(state[2], c);
        state[3] = plus(state[3], d);
    }

    public void update(final int bytes[]) {
        do_update(clean_bytes(bytes));
    }

    public void update(final String s) {
        do_update(to_bytes(s));
    }

    private int[] encode(final long[] input, final int len) {
        final int output[] = new int[len];
        int i, j;
        for (i = 0, j = 0; j < len; i++, j += 4) {
            output[j] = (int) (input[i] & 0xff);
            output[j + 1] = (int) (input[i] >>> 8 & 0xff);
            output[j + 2] = (int) (input[i] >>> 16 & 0xff);
            output[j + 3] = (int) (input[i] >>> 24 & 0xff);
        }
        return output;
    }

    public int[] final_bytes() {
        final int bits[] = encode(count, 8);
        int index, padlen;
        int padding[], i;
        int[] digest;

        index = (int) (count[0] >>> 3 & 0x3f);
        padlen = index < 56 ? 56 - index : 120 - index;
        /* padlen > 0 */
        padding = new int[padlen];
        padding[0] = 0x80;
        for (i = 1; i < padlen; ++i) {
            padding[i] = 0;
        }

        do_update(padding);

        do_update(bits);

        digest = encode(state, 16);

        return digest;
    }
}
