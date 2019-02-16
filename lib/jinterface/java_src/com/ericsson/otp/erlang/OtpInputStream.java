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

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.math.BigDecimal;
import java.util.Arrays;

/**
 * Provides a stream for decoding Erlang terms from external format.
 *
 * <p>
 * Note that this class is not synchronized, if you need synchronization you
 * must provide it yourself.
 */
public class OtpInputStream extends ByteArrayInputStream {

    public static int DECODE_INT_LISTS_AS_STRINGS = 1;

    private final int flags;

    /**
     * @param buf
     */
    public OtpInputStream(final byte[] buf) {
        this(buf, 0);
    }

    /**
     * Create a stream from a buffer containing encoded Erlang terms.
     *
     * @param flags
     */
    public OtpInputStream(final byte[] buf, final int flags) {
        super(buf);
        this.flags = flags;
    }

    /**
     * Create a stream from a buffer containing encoded Erlang terms at the
     * given offset and length.
     *
     * @param flags
     */
    public OtpInputStream(final byte[] buf, final int offset, final int length,
            final int flags) {
        super(buf, offset, length);
        this.flags = flags;
    }

    /**
     * Get the current position in the stream.
     *
     * @return the current position in the stream.
     */
    public int getPos() {
        return super.pos;
    }

    /**
     * Set the current position in the stream.
     *
     * @param pos
     *            the position to move to in the stream. If pos indicates a
     *            position beyond the end of the stream, the position is move to
     *            the end of the stream instead. If pos is negative, the
     *            position is moved to the beginning of the stream instead.
     *
     * @return the previous position in the stream.
     */
    public int setPos(final int pos) {
        final int oldpos = super.pos;

        int apos = pos;
        if (pos > super.count) {
            apos = super.count;
        } else if (pos < 0) {
            apos = 0;
        }

        super.pos = apos;

        return oldpos;
    }

    /**
     * Read an array of bytes from the stream. The method reads at most
     * buf.length bytes from the input stream.
     *
     * @return the number of bytes read.
     *
     * @exception OtpErlangDecodeException
     *                if the next byte cannot be read.
     */
    public int readN(final byte[] abuf) throws OtpErlangDecodeException {
        return this.readN(abuf, 0, abuf.length);
    }

    /**
     * Read an array of bytes from the stream. The method reads at most len
     * bytes from the input stream into offset off of the buffer.
     *
     * @return the number of bytes read.
     *
     * @exception OtpErlangDecodeException
     *                if the next byte cannot be read.
     */
    public int readN(final byte[] abuf, final int off, final int len)
            throws OtpErlangDecodeException {
        if (len == 0 && available() == 0) {
            return 0;
        }
        final int i = super.read(abuf, off, len);
        if (i < 0) {
            throw new OtpErlangDecodeException("Cannot read from input stream");
        }
        return i;
    }

    /**
     * Alias for peek1()
     */
    public int peek() throws OtpErlangDecodeException {
        return peek1();
    }

    /**
     * Look ahead one position in the stream without consuming the byte found
     * there.
     *
     * @return the next byte in the stream, as an integer.
     *
     * @exception OtpErlangDecodeException
     *                if the next byte cannot be read.
     */
    public int peek1() throws OtpErlangDecodeException {
        int i;
        try {
            i = super.buf[super.pos];
            if (i < 0) {
                i += 256;
            }

            return i;
        } catch (final Exception e) {
            throw new OtpErlangDecodeException("Cannot read from input stream");
        }
    }

    public int peek1skip_version() throws OtpErlangDecodeException {
        int i = peek1();
        if (i == OtpExternal.versionTag) {
            read1();
            i = peek1();
        }
        return i;
    }

    /**
     * Read a one byte integer from the stream.
     *
     * @return the byte read, as an integer.
     *
     * @exception OtpErlangDecodeException
     *                if the next byte cannot be read.
     */
    public int read1() throws OtpErlangDecodeException {
        int i;
        i = super.read();

        if (i < 0) {
            throw new OtpErlangDecodeException("Cannot read from input stream");
        }

        return i;
    }

    public int read1skip_version() throws OtpErlangDecodeException {
        int tag = read1();
        if (tag == OtpExternal.versionTag) {
            tag = read1();
        }
        return tag;
    }

    /**
     * Read a two byte big endian integer from the stream.
     *
     * @return the bytes read, converted from big endian to an integer.
     *
     * @exception OtpErlangDecodeException
     *                if the next byte cannot be read.
     */
    public int read2BE() throws OtpErlangDecodeException {
        final byte[] b = new byte[2];
        try {
            super.read(b);
        } catch (final IOException e) {
            throw new OtpErlangDecodeException("Cannot read from input stream");
        }
        return (b[0] << 8 & 0xff00) + (b[1] & 0xff);
    }

    /**
     * Read a four byte big endian integer from the stream.
     *
     * @return the bytes read, converted from big endian to an integer.
     *
     * @exception OtpErlangDecodeException
     *                if the next byte cannot be read.
     */
    public int read4BE() throws OtpErlangDecodeException {
        final byte[] b = new byte[4];
        try {
            super.read(b);
        } catch (final IOException e) {
            throw new OtpErlangDecodeException("Cannot read from input stream");
        }
        return (b[0] << 24 & 0xff000000) + (b[1] << 16 & 0xff0000)
                + (b[2] << 8 & 0xff00) + (b[3] & 0xff);
    }

    /**
     * Read a two byte little endian integer from the stream.
     *
     * @return the bytes read, converted from little endian to an integer.
     *
     * @exception OtpErlangDecodeException
     *                if the next byte cannot be read.
     */
    public int read2LE() throws OtpErlangDecodeException {
        final byte[] b = new byte[2];
        try {
            super.read(b);
        } catch (final IOException e) {
            throw new OtpErlangDecodeException("Cannot read from input stream");
        }
        return (b[1] << 8 & 0xff00) + (b[0] & 0xff);
    }

    /**
     * Read a four byte little endian integer from the stream.
     *
     * @return the bytes read, converted from little endian to an integer.
     *
     * @exception OtpErlangDecodeException
     *                if the next byte cannot be read.
     */
    public int read4LE() throws OtpErlangDecodeException {
        final byte[] b = new byte[4];
        try {
            super.read(b);
        } catch (final IOException e) {
            throw new OtpErlangDecodeException("Cannot read from input stream");
        }
        return (b[3] << 24 & 0xff000000) + (b[2] << 16 & 0xff0000)
                + (b[1] << 8 & 0xff00) + (b[0] & 0xff);
    }

    /**
     * Read a little endian integer from the stream.
     *
     * @param n
     *            the number of bytes to read
     *
     * @return the bytes read, converted from little endian to an integer.
     *
     * @exception OtpErlangDecodeException
     *                if the next byte cannot be read.
     */
    public long readLE(final int n) throws OtpErlangDecodeException {
        final byte[] b = new byte[n];
        try {
            super.read(b);
        } catch (final IOException e) {
            throw new OtpErlangDecodeException("Cannot read from input stream");
        }
        long v = 0;
        int i = n;
        while (i-- > 0) {
            v = v << 8 | (long) b[i] & 0xff;
        }
        return v;
    }

    /**
     * Read a bigendian integer from the stream.
     *
     * @param n
     *            the number of bytes to read
     *
     * @return the bytes read, converted from big endian to an integer.
     *
     * @exception OtpErlangDecodeException
     *                if the next byte cannot be read.
     */
    public long readBE(final int n) throws OtpErlangDecodeException {
        final byte[] b = new byte[n];
        try {
            super.read(b);
        } catch (final IOException e) {
            throw new OtpErlangDecodeException("Cannot read from input stream");
        }
        long v = 0;
        for (int i = 0; i < n; i++) {
            v = v << 8 | (long) b[i] & 0xff;
        }
        return v;
    }

    /**
     * Read an Erlang atom from the stream and interpret the value as a boolean.
     *
     * @return true if the atom at the current position in the stream contains
     *         the value 'true' (ignoring case), false otherwise.
     *
     * @exception OtpErlangDecodeException
     *                if the next term in the stream is not an atom.
     */
    public boolean read_boolean() throws OtpErlangDecodeException {
        return Boolean.valueOf(read_atom()).booleanValue();
    }

    /**
     * Read an Erlang atom from the stream.
     *
     * @return a String containing the value of the atom.
     *
     * @exception OtpErlangDecodeException
     *                if the next term in the stream is not an atom.
     */
    @SuppressWarnings("fallthrough")
    public String read_atom() throws OtpErlangDecodeException {
        int tag;
        int len = -1;
        byte[] strbuf;
        String atom;

        tag = read1skip_version();

        switch (tag) {

        case OtpExternal.atomTag:
            len = read2BE();
            strbuf = new byte[len];
            this.readN(strbuf);
            try {
                atom = new String(strbuf, "ISO-8859-1");
            } catch (final java.io.UnsupportedEncodingException e) {
                throw new OtpErlangDecodeException(
                        "Failed to decode ISO-8859-1 atom");
            }
            if (atom.length() > OtpExternal.maxAtomLength) {
                /*
                 * Throwing an exception would be better I think, but truncation
                 * seems to be the way it has been done in other parts of OTP...
                 */
                atom = atom.substring(0, OtpExternal.maxAtomLength);
            }
            break;

        case OtpExternal.smallAtomUtf8Tag:
            len = read1();
            // fall-through
        case OtpExternal.atomUtf8Tag:
            if (len < 0) {
                len = read2BE();
            }
            strbuf = new byte[len];
            this.readN(strbuf);
            try {
                atom = new String(strbuf, "UTF-8");
            } catch (final java.io.UnsupportedEncodingException e) {
                throw new OtpErlangDecodeException(
                        "Failed to decode UTF-8 atom");
            }
            if (atom.codePointCount(0, atom.length()) > OtpExternal.maxAtomLength) {
                /*
                 * Throwing an exception would be better I think, but truncation
                 * seems to be the way it has been done in other parts of OTP...
                 */
                final int[] cps = OtpErlangString.stringToCodePoints(atom);
                atom = new String(cps, 0, OtpExternal.maxAtomLength);
            }
            break;

        default:
            throw new OtpErlangDecodeException(
                    "wrong tag encountered, expected " + OtpExternal.atomTag
                            + ", or " + OtpExternal.atomUtf8Tag + ", got "
                            + tag);
        }

        return atom;
    }

    /**
     * Read an Erlang binary from the stream.
     *
     * @return a byte array containing the value of the binary.
     *
     * @exception OtpErlangDecodeException
     *                if the next term in the stream is not a binary.
     */
    public byte[] read_binary() throws OtpErlangDecodeException {
        int tag;
        int len;
        byte[] bin;

        tag = read1skip_version();

        if (tag != OtpExternal.binTag) {
            throw new OtpErlangDecodeException(
                    "Wrong tag encountered, expected " + OtpExternal.binTag
                            + ", got " + tag);
        }

        len = read4BE();

        bin = new byte[len];
        this.readN(bin);

        return bin;
    }

    /**
     * Read an Erlang bitstr from the stream.
     *
     * @param pad_bits
     *            an int array whose first element will be set to the number of
     *            pad bits in the last byte.
     *
     * @return a byte array containing the value of the bitstr.
     *
     * @exception OtpErlangDecodeException
     *                if the next term in the stream is not a bitstr.
     */
    public byte[] read_bitstr(final int pad_bits[])
            throws OtpErlangDecodeException {
        int tag;
        int len;
        byte[] bin;

        tag = read1skip_version();

        if (tag != OtpExternal.bitBinTag) {
            throw new OtpErlangDecodeException(
                    "Wrong tag encountered, expected " + OtpExternal.bitBinTag
                            + ", got " + tag);
        }

        len = read4BE();
        bin = new byte[len];
        final int tail_bits = read1();
        if (tail_bits < 0 || 7 < tail_bits) {
            throw new OtpErlangDecodeException(
                    "Wrong tail bit count in bitstr: " + tail_bits);
        }
        if (len == 0 && tail_bits != 0) {
            throw new OtpErlangDecodeException(
                    "Length 0 on bitstr with tail bit count: " + tail_bits);
        }
        this.readN(bin);

        pad_bits[0] = 8 - tail_bits;
        return bin;
    }

    /**
     * Read an Erlang float from the stream.
     *
     * @return the float value.
     *
     * @exception OtpErlangDecodeException
     *                if the next term in the stream is not a float.
     */
    public float read_float() throws OtpErlangDecodeException {
        final double d = read_double();
        return (float) d;
    }

    /**
     * Read an Erlang float from the stream.
     *
     * @return the float value, as a double.
     *
     * @exception OtpErlangDecodeException
     *                if the next term in the stream is not a float.
     */
    public double read_double() throws OtpErlangDecodeException {
        int tag;

        // parse the stream
        tag = read1skip_version();

        switch (tag) {
        case OtpExternal.newFloatTag: {
            return Double.longBitsToDouble(readBE(8));
        }
        case OtpExternal.floatTag: {
            BigDecimal val;
            int epos;
            int exp;
            final byte[] strbuf = new byte[31];
            String str;

            // get the string
            this.readN(strbuf);
            str = OtpErlangString.newString(strbuf);

            // find the exponent prefix 'e' in the string
            epos = str.indexOf('e', 0);

            if (epos < 0) {
                throw new OtpErlangDecodeException("Invalid float format: '"
                        + str + "'");
            }

            // remove the sign from the exponent, if positive
            String estr = str.substring(epos + 1).trim();

            if (estr.substring(0, 1).equals("+")) {
                estr = estr.substring(1);
            }

            // now put the mantissa and exponent together
            exp = Integer.valueOf(estr).intValue();
            val = new BigDecimal(str.substring(0, epos)).movePointRight(exp);

            return val.doubleValue();
        }
        default:
            throw new OtpErlangDecodeException(
                    "Wrong tag encountered, expected "
                            + OtpExternal.newFloatTag + ", got " + tag);
        }
    }

    /**
     * Read one byte from the stream.
     *
     * @return the byte read.
     *
     * @exception OtpErlangDecodeException
     *                if the next byte cannot be read.
     */
    public byte read_byte() throws OtpErlangDecodeException {
        final long l = this.read_long(false);
        final byte i = (byte) l;

        if (l != i) {
            throw new OtpErlangDecodeException("Value does not fit in byte: "
                    + l);
        }

        return i;
    }

    /**
     * Read a character from the stream.
     *
     * @return the character value.
     *
     * @exception OtpErlangDecodeException
     *                if the next term in the stream is not an integer that can
     *                be represented as a char.
     */
    public char read_char() throws OtpErlangDecodeException {
        final long l = this.read_long(true);
        final char i = (char) l;

        if (l != (i & 0xffffL)) {
            throw new OtpErlangDecodeException("Value does not fit in char: "
                    + l);
        }

        return i;
    }

    /**
     * Read an unsigned integer from the stream.
     *
     * @return the integer value.
     *
     * @exception OtpErlangDecodeException
     *                if the next term in the stream cannot be represented as a
     *                positive integer.
     */
    public int read_uint() throws OtpErlangDecodeException {
        final long l = this.read_long(true);
        final int i = (int) l;

        if (l != (i & 0xFFFFffffL)) {
            throw new OtpErlangDecodeException("Value does not fit in uint: "
                    + l);
        }

        return i;
    }

    /**
     * Read an integer from the stream.
     *
     * @return the integer value.
     *
     * @exception OtpErlangDecodeException
     *                if the next term in the stream cannot be represented as
     *                an integer.
     */
    public int read_int() throws OtpErlangDecodeException {
        final long l = this.read_long(false);
        final int i = (int) l;

        if (l != i) {
            throw new OtpErlangDecodeException("Value does not fit in int: "
                    + l);
        }

        return i;
    }

    /**
     * Read an unsigned short from the stream.
     *
     * @return the short value.
     *
     * @exception OtpErlangDecodeException
     *                if the next term in the stream cannot be represented as a
     *                positive short.
     */
    public short read_ushort() throws OtpErlangDecodeException {
        final long l = this.read_long(true);
        final short i = (short) l;

        if (l != (i & 0xffffL)) {
            throw new OtpErlangDecodeException("Value does not fit in ushort: "
                    + l);
        }

        return i;
    }

    /**
     * Read a short from the stream.
     *
     * @return the short value.
     *
     * @exception OtpErlangDecodeException
     *                if the next term in the stream cannot be represented as a
     *                short.
     */
    public short read_short() throws OtpErlangDecodeException {
        final long l = this.read_long(false);
        final short i = (short) l;

        if (l != i) {
            throw new OtpErlangDecodeException("Value does not fit in short: "
                    + l);
        }

        return i;
    }

    /**
     * Read an unsigned long from the stream.
     *
     * @return the long value.
     *
     * @exception OtpErlangDecodeException
     *                if the next term in the stream cannot be represented as a
     *                positive long.
     */
    public long read_ulong() throws OtpErlangDecodeException {
        return this.read_long(true);
    }

    /**
     * Read a long from the stream.
     *
     * @return the long value.
     *
     * @exception OtpErlangDecodeException
     *                if the next term in the stream cannot be represented as a
     *                long.
     */
    public long read_long() throws OtpErlangDecodeException {
        return this.read_long(false);
    }

    public long read_long(final boolean unsigned)
            throws OtpErlangDecodeException {
        final byte[] b = read_integer_byte_array();
        return OtpInputStream.byte_array_to_long(b, unsigned);
    }

    /**
     * Read an integer from the stream.
     *
     * @return the value as a big endian 2's complement byte array.
     *
     * @exception OtpErlangDecodeException
     *                if the next term in the stream is not an integer.
     */
    public byte[] read_integer_byte_array() throws OtpErlangDecodeException {
        int tag;
        byte[] nb;

        tag = read1skip_version();

        switch (tag) {
        case OtpExternal.smallIntTag:
            nb = new byte[2];
            nb[0] = 0;
            nb[1] = (byte) read1();
            break;

        case OtpExternal.intTag:
            nb = new byte[4];
            if (this.readN(nb) != 4) { // Big endian
                throw new OtpErlangDecodeException(
                        "Cannot read from intput stream");
            }
            break;

        case OtpExternal.smallBigTag:
        case OtpExternal.largeBigTag:
            int arity;
            int sign;
            if (tag == OtpExternal.smallBigTag) {
                arity = read1();
                sign = read1();
            } else {
                arity = read4BE();
                sign = read1();
                if (arity + 1 < 0) {
                    throw new OtpErlangDecodeException(
                            "Value of largeBig does not fit in BigInteger, arity "
                                    + arity + " sign " + sign);
                }
            }
            nb = new byte[arity + 1];
            // Value is read as little endian. The big end is augumented
            // with one zero byte to make the value 2's complement positive.
            if (this.readN(nb, 0, arity) != arity) {
                throw new OtpErlangDecodeException(
                        "Cannot read from intput stream");
            }
            // Reverse the array to make it big endian.
            for (int i = 0, j = nb.length; i < j--; i++) {
                // Swap [i] with [j]
                final byte b = nb[i];
                nb[i] = nb[j];
                nb[j] = b;
            }
            if (sign != 0) {
                // 2's complement negate the big endian value in the array
                int c = 1; // Carry
                for (int j = nb.length; j-- > 0;) {
                    c = (~nb[j] & 0xFF) + c;
                    nb[j] = (byte) c;
                    c >>= 8;
                }
            }
            break;

        default:
            throw new OtpErlangDecodeException("Not valid integer tag: " + tag);
        }

        return nb;
    }

    public static long byte_array_to_long(final byte[] b, final boolean unsigned)
            throws OtpErlangDecodeException {
        long v;
        switch (b.length) {
        case 0:
            v = 0;
            break;
        case 2:
            v = ((b[0] & 0xFF) << 8) + (b[1] & 0xFF);
            v = (short) v; // Sign extend
            if (v < 0 && unsigned) {
                throw new OtpErlangDecodeException("Value not unsigned: " + v);
            }
            break;
        case 4:
            v = ((b[0] & 0xFF) << 24) + ((b[1] & 0xFF) << 16)
                    + ((b[2] & 0xFF) << 8) + (b[3] & 0xFF);
            v = (int) v; // Sign extend
            if (v < 0 && unsigned) {
                throw new OtpErlangDecodeException("Value not unsigned: " + v);
            }
            break;
        default:
            int i = 0;
            final byte c = b[i];
            // Skip non-essential leading bytes
            if (unsigned) {
                if (c < 0) {
                    throw new OtpErlangDecodeException("Value not unsigned: "
                            + Arrays.toString(b));
                }
                while (b[i] == 0) {
                    i++; // Skip leading zero sign bytes
                }
            } else {
                if (c == 0 || c == -1) { // Leading sign byte
                    i = 1;
                    // Skip all leading sign bytes
                    while (i < b.length && b[i] == c) {
                        i++;
                    }
                    if (i < b.length) {
                        // Check first non-sign byte to see if its sign
                        // matches the whole number's sign. If not one more
                        // byte is needed to represent the value.
                        if (((c ^ b[i]) & 0x80) != 0) {
                            i--;
                        }
                    }
                }
            }
            if (b.length - i > 8) {
                // More than 64 bits of value
                throw new OtpErlangDecodeException(
                        "Value does not fit in long: " + Arrays.toString(b));
            }
            // Convert the necessary bytes
            for (v = c < 0 ? -1 : 0; i < b.length; i++) {
                v = v << 8 | b[i] & 0xFF;
            }
        }
        return v;
    }

    /**
     * Read a list header from the stream.
     *
     * @return the arity of the list.
     *
     * @exception OtpErlangDecodeException
     *                if the next term in the stream is not a list.
     */
    public int read_list_head() throws OtpErlangDecodeException {
        int arity = 0;
        final int tag = read1skip_version();

        switch (tag) {
        case OtpExternal.nilTag:
            arity = 0;
            break;

        case OtpExternal.stringTag:
            arity = read2BE();
            break;

        case OtpExternal.listTag:
            arity = read4BE();
            break;

        default:
            throw new OtpErlangDecodeException("Not valid list tag: " + tag);
        }

        return arity;
    }

    /**
     * Read a tuple header from the stream.
     *
     * @return the arity of the tuple.
     *
     * @exception OtpErlangDecodeException
     *                if the next term in the stream is not a tuple.
     */
    public int read_tuple_head() throws OtpErlangDecodeException {
        int arity = 0;
        final int tag = read1skip_version();

        // decode the tuple header and get arity
        switch (tag) {
        case OtpExternal.smallTupleTag:
            arity = read1();
            break;

        case OtpExternal.largeTupleTag:
            arity = read4BE();
            break;

        default:
            throw new OtpErlangDecodeException("Not valid tuple tag: " + tag);
        }

        return arity;
    }

    /**
     * Read an empty list from the stream.
     *
     * @return zero (the arity of the list).
     *
     * @exception OtpErlangDecodeException
     *                if the next term in the stream is not an empty list.
     */
    public int read_nil() throws OtpErlangDecodeException {
        int arity = 0;
        final int tag = read1skip_version();

        switch (tag) {
        case OtpExternal.nilTag:
            arity = 0;
            break;

        default:
            throw new OtpErlangDecodeException("Not valid nil tag: " + tag);
        }

        return arity;
    }

    /**
     * Read an Erlang PID from the stream.
     *
     * @return the value of the PID.
     *
     * @exception OtpErlangDecodeException
     *                if the next term in the stream is not an Erlang PID.
     */
    public OtpErlangPid read_pid() throws OtpErlangDecodeException {
        String node;
        int id;
        int serial;
        int creation;
        int tag;

        tag = read1skip_version();

        if (tag != OtpExternal.pidTag &&
	    tag != OtpExternal.newPidTag) {
            throw new OtpErlangDecodeException(
                    "Wrong tag encountered, expected " + OtpExternal.pidTag
		    + " or " + OtpExternal.newPidTag
                            + ", got " + tag);
        }

        node = read_atom();
        id = read4BE();
        serial = read4BE();
	if (tag == OtpExternal.pidTag)
	    creation = read1();
	else
	    creation = read4BE();

        return new OtpErlangPid(tag, node, id, serial, creation);
    }

    /**
     * Read an Erlang port from the stream.
     *
     * @return the value of the port.
     *
     * @exception OtpErlangDecodeException
     *                if the next term in the stream is not an Erlang port.
     */
    public OtpErlangPort read_port() throws OtpErlangDecodeException {
        String node;
        int id;
        int creation;
        int tag;

        tag = read1skip_version();

        if (tag != OtpExternal.portTag &&
	    tag != OtpExternal.newPortTag) {
            throw new OtpErlangDecodeException(
                    "Wrong tag encountered, expected " + OtpExternal.portTag
		    + " or " + OtpExternal.newPortTag
                            + ", got " + tag);
        }

        node = read_atom();
        id = read4BE();
	if (tag == OtpExternal.portTag)
	    creation = read1();
	else
	    creation = read4BE();

        return new OtpErlangPort(tag, node, id, creation);
    }

    /**
     * Read an Erlang reference from the stream.
     *
     * @return the value of the reference
     *
     * @exception OtpErlangDecodeException
     *                if the next term in the stream is not an Erlang reference.
     */
    public OtpErlangRef read_ref() throws OtpErlangDecodeException {
        String node;
        int id;
        int creation;
        int tag;

        tag = read1skip_version();

        switch (tag) {
        case OtpExternal.refTag:
            node = read_atom();
            id = read4BE() & 0x3ffff; // 18 bits
            creation = read1() & 0x03; // 2 bits
            return new OtpErlangRef(node, id, creation);

        case OtpExternal.newRefTag:
        case OtpExternal.newerRefTag:
            final int arity = read2BE();
            if (arity > 3) {
		throw new OtpErlangDecodeException(
		    "Ref arity " + arity + " too large ");
	    }
            node = read_atom();
	    if (tag == OtpExternal.newRefTag)
		creation = read1();
	    else
		creation = read4BE();

            final int[] ids = new int[arity];
            for (int i = 0; i < arity; i++) {
                ids[i] = read4BE();
            }
            return new OtpErlangRef(tag, node, ids, creation);

        default:
            throw new OtpErlangDecodeException(
                    "Wrong tag encountered, expected ref, got " + tag);
        }
    }

    public OtpErlangFun read_fun() throws OtpErlangDecodeException {
        final int tag = read1skip_version();
        if (tag == OtpExternal.funTag) {
            final int nFreeVars = read4BE();
            final OtpErlangPid pid = read_pid();
            final String module = read_atom();
            final long index = read_long();
            final long uniq = read_long();
            final OtpErlangObject[] freeVars = new OtpErlangObject[nFreeVars];
            for (int i = 0; i < nFreeVars; ++i) {
                freeVars[i] = read_any();
            }
            return new OtpErlangFun(pid, module, index, uniq, freeVars);
        } else if (tag == OtpExternal.newFunTag) {
            read4BE();
            final int arity = read1();
            final byte[] md5 = new byte[16];
            readN(md5);
            final int index = read4BE();
            final int nFreeVars = read4BE();
            final String module = read_atom();
            final long oldIndex = read_long();
            final long uniq = read_long();
            final OtpErlangPid pid = read_pid();
            final OtpErlangObject[] freeVars = new OtpErlangObject[nFreeVars];
            for (int i = 0; i < nFreeVars; ++i) {
                freeVars[i] = read_any();
            }
            return new OtpErlangFun(pid, module, arity, md5, index, oldIndex,
                    uniq, freeVars);
        } else {
            throw new OtpErlangDecodeException(
                    "Wrong tag encountered, expected fun, got " + tag);
        }
    }

    public OtpErlangExternalFun read_external_fun()
            throws OtpErlangDecodeException {
        final int tag = read1skip_version();
        if (tag != OtpExternal.externalFunTag) {
            throw new OtpErlangDecodeException(
                    "Wrong tag encountered, expected external fun, got " + tag);
        }
        final String module = read_atom();
        final String function = read_atom();
        final int arity = (int) read_long();
        return new OtpErlangExternalFun(module, function, arity);
    }

    /**
     * Read a string from the stream.
     *
     * @return the value of the string.
     *
     * @exception OtpErlangDecodeException
     *                if the next term in the stream is not a string.
     */
    public String read_string() throws OtpErlangDecodeException {
        int tag;
        int len;
        byte[] strbuf;
        int[] intbuf;
        tag = read1skip_version();
        switch (tag) {
        case OtpExternal.stringTag:
            len = read2BE();
            strbuf = new byte[len];
            this.readN(strbuf);
            return OtpErlangString.newString(strbuf);
        case OtpExternal.nilTag:
            return "";
        case OtpExternal.listTag: // List when unicode +
            len = read4BE();
            intbuf = new int[len];
            for (int i = 0; i < len; i++) {
                intbuf[i] = read_int();
                if (!OtpErlangString.isValidCodePoint(intbuf[i])) {
                    throw new OtpErlangDecodeException("Invalid CodePoint: "
                            + intbuf[i]);
                }
            }
            read_nil();
            return new String(intbuf, 0, intbuf.length);
        default:
            throw new OtpErlangDecodeException(
                    "Wrong tag encountered, expected " + OtpExternal.stringTag
                            + " or " + OtpExternal.listTag + ", got " + tag);
        }
    }

    /**
     * Read a compressed term from the stream
     *
     * @return the resulting uncompressed term.
     *
     * @exception OtpErlangDecodeException
     *                if the next term in the stream is not a compressed term.
     */
    public OtpErlangObject read_compressed() throws OtpErlangDecodeException {
        final int tag = read1skip_version();

        if (tag != OtpExternal.compressedTag) {
            throw new OtpErlangDecodeException(
                    "Wrong tag encountered, expected "
                            + OtpExternal.compressedTag + ", got " + tag);
        }

        final int size = read4BE();
        final byte[] abuf = new byte[size];
        final java.util.zip.InflaterInputStream is = new java.util.zip.InflaterInputStream(
                this, new java.util.zip.Inflater(), size);
        int curPos = 0;
        try {
            int curRead;
            while (curPos < size
                    && (curRead = is.read(abuf, curPos, size - curPos)) != -1) {
                curPos += curRead;
            }
            if (curPos != size) {
                throw new OtpErlangDecodeException("Decompression gave "
                        + curPos + " bytes, not " + size);
            }
        } catch (final IOException e) {
            throw new OtpErlangDecodeException("Cannot read from input stream");
        }

        @SuppressWarnings("resource")
        final OtpInputStream ois = new OtpInputStream(abuf, flags);
        return ois.read_any();
    }

    /**
     * Read an arbitrary Erlang term from the stream.
     *
     * @return the Erlang term.
     *
     * @exception OtpErlangDecodeException
     *                if the stream does not contain a known Erlang type at the
     *                next position.
     */
    public OtpErlangObject read_any() throws OtpErlangDecodeException {
        // calls one of the above functions, depending on o
        final int tag = peek1skip_version();

        switch (tag) {
        case OtpExternal.smallIntTag:
        case OtpExternal.intTag:
        case OtpExternal.smallBigTag:
        case OtpExternal.largeBigTag:
            return new OtpErlangLong(this);

        case OtpExternal.atomTag:
        case OtpExternal.smallAtomUtf8Tag:
        case OtpExternal.atomUtf8Tag:
            return new OtpErlangAtom(this);

        case OtpExternal.floatTag:
        case OtpExternal.newFloatTag:
            return new OtpErlangDouble(this);

        case OtpExternal.refTag:
        case OtpExternal.newRefTag:
        case OtpExternal.newerRefTag:
            return new OtpErlangRef(this);

        case OtpExternal.mapTag:
            return new OtpErlangMap(this);

        case OtpExternal.portTag:
        case OtpExternal.newPortTag:
            return new OtpErlangPort(this);

        case OtpExternal.pidTag:
        case OtpExternal.newPidTag:
            return new OtpErlangPid(this);

        case OtpExternal.stringTag:
            return new OtpErlangString(this);

        case OtpExternal.listTag:
        case OtpExternal.nilTag:
            if ((flags & DECODE_INT_LISTS_AS_STRINGS) != 0) {
                final int savePos = getPos();
                try {
                    return new OtpErlangString(this);
                } catch (final OtpErlangDecodeException e) {
                }
                setPos(savePos);
            }
            return new OtpErlangList(this);

        case OtpExternal.smallTupleTag:
        case OtpExternal.largeTupleTag:
            return new OtpErlangTuple(this);

        case OtpExternal.binTag:
            return new OtpErlangBinary(this);

        case OtpExternal.bitBinTag:
            return new OtpErlangBitstr(this);

        case OtpExternal.compressedTag:
            return read_compressed();

        case OtpExternal.newFunTag:
        case OtpExternal.funTag:
            return new OtpErlangFun(this);

	case OtpExternal.externalFunTag:
	    return new OtpErlangExternalFun(this);

        default:
            throw new OtpErlangDecodeException("Uknown data type: " + tag);
        }
    }

    public int read_map_head() throws OtpErlangDecodeException {
        int arity = 0;
        final int tag = read1skip_version();

        // decode the map header and get arity
        switch (tag) {
        case OtpExternal.mapTag:
            arity = read4BE();
            break;

        default:
            throw new OtpErlangDecodeException("Not valid map tag: " + tag);
        }

        return arity;
    }
}
