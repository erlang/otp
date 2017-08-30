/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2007-2016. All Rights Reserved.
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

import java.io.ByteArrayOutputStream;
import java.io.IOException;

/**
 * Provides a Java representation of Erlang bitstrs. An Erlang bitstr is an
 * Erlang binary with a length not an integral number of bytes (8-bit). Anything
 * can be represented as a sequence of bytes can be made into an Erlang bitstr.
 */
public class OtpErlangBitstr extends OtpErlangObject {
    // don't change this!
    private static final long serialVersionUID = -3781009633593609217L;

    protected byte[] bin;
    protected int pad_bits;

    /**
     * Create a bitstr from a byte array
     *
     * @param bin
     *            the array of bytes from which to create the bitstr.
     */
    public OtpErlangBitstr(final byte[] bin) {
        this.bin = new byte[bin.length];
        System.arraycopy(bin, 0, this.bin, 0, bin.length);
        pad_bits = 0;
    }

    /**
     * Create a bitstr with pad bits from a byte array.
     *
     * @param bin
     *            the array of bytes from which to create the bitstr.
     * @param pad_bits
     *            the number of unused bits in the low end of the last byte.
     */
    public OtpErlangBitstr(final byte[] bin, final int pad_bits) {
        this.bin = new byte[bin.length];
        System.arraycopy(bin, 0, this.bin, 0, bin.length);
        this.pad_bits = pad_bits;

        check_bitstr(this.bin, this.pad_bits);
    }

    private void check_bitstr(final byte[] abin, final int a_pad_bits) {
        if (a_pad_bits < 0 || 7 < a_pad_bits) {
            throw new java.lang.IllegalArgumentException(
                    "Padding must be in range 0..7");
        }
        if (a_pad_bits != 0 && abin.length == 0) {
            throw new java.lang.IllegalArgumentException(
                    "Padding on zero length bitstr");
        }
        if (abin.length != 0) {
            // Make sure padding is zero
            abin[abin.length - 1] &= ~((1 << a_pad_bits) - 1);
        }
    }

    /**
     * Create a bitstr from a stream containing a bitstr encoded in Erlang
     * external format.
     *
     * @param buf
     *            the stream containing the encoded bitstr.
     *
     * @exception OtpErlangDecodeException
     *                if the buffer does not contain a valid external
     *                representation of an Erlang bitstr.
     */
    public OtpErlangBitstr(final OtpInputStream buf)
            throws OtpErlangDecodeException {
        final int pbs[] = { 0 }; // This is ugly just to get a value-result
        // parameter
        bin = buf.read_bitstr(pbs);
        pad_bits = pbs[0];

        check_bitstr(bin, pad_bits);
    }

    /**
     * Create a bitstr from an arbitrary Java Object. The object must implement
     * java.io.Serializable or java.io.Externalizable.
     *
     * @param o
     *            the object to serialize and create this bitstr from.
     */
    public OtpErlangBitstr(final Object o) {
        try {
            bin = toByteArray(o);
            pad_bits = 0;
        } catch (final IOException e) {
            throw new java.lang.IllegalArgumentException(
                    "Object must implement Serializable");
        }
    }

    private static byte[] toByteArray(final Object o)
            throws java.io.IOException {

        if (o == null) {
            return null;
        }

        /* need to synchronize use of the shared baos */
        final java.io.ByteArrayOutputStream baos = new ByteArrayOutputStream();
        final java.io.ObjectOutputStream oos = new java.io.ObjectOutputStream(
                baos);

        oos.writeObject(o);
        oos.flush();

        return baos.toByteArray();
    }

    private static Object fromByteArray(final byte[] buf) {
        if (buf == null) {
            return null;
        }

        try {
            final java.io.ByteArrayInputStream bais = new java.io.ByteArrayInputStream(
                    buf);
            final java.io.ObjectInputStream ois = new java.io.ObjectInputStream(
                    bais);
            return ois.readObject();
        } catch (final java.lang.ClassNotFoundException e) {
        } catch (final java.io.IOException e) {
        }

        return null;
    }

    /**
     * Get the byte array from a bitstr, padded with zero bits in the little end
     * of the last byte.
     *
     * @return the byte array containing the bytes for this bitstr.
     */
    public byte[] binaryValue() {
        return bin;
    }

    /**
     * Get the size in whole bytes of the bitstr, rest bits in the last byte not
     * counted.
     *
     * @return the number of bytes contained in the bintstr.
     */
    public int size() {
        if (pad_bits == 0) {
            return bin.length;
        }
        if (bin.length == 0) {
            throw new java.lang.IllegalStateException("Impossible length");
        }
        return bin.length - 1;
    }

    /**
     * Get the number of pad bits in the last byte of the bitstr. The pad bits
     * are zero and in the little end.
     *
     * @return the number of pad bits in the bitstr.
     */
    public int pad_bits() {
        return pad_bits;
    }

    /**
     * Get the java Object from the bitstr. If the bitstr contains a serialized
     * Java object, then this method will recreate the object.
     *
     *
     * @return the java Object represented by this bitstr, or null if the bitstr
     *         does not represent a Java Object.
     */
    public Object getObject() {
        if (pad_bits != 0) {
            return null;
        }
        return fromByteArray(bin);
    }

    /**
     * Get the string representation of this bitstr object. A bitstr is printed
     * as #Bin&lt;N&gt;, where N is the number of bytes contained in the object
     * or #bin&lt;N-M&gt; if there are M pad bits.
     *
     * @return the Erlang string representation of this bitstr.
     */
    @Override
    public String toString() {
        if (pad_bits == 0) {
            return "#Bin<" + bin.length + ">";
        }
        if (bin.length == 0) {
            throw new java.lang.IllegalStateException("Impossible length");
        }
        return "#Bin<" + bin.length + "-" + pad_bits + ">";
    }

    /**
     * Convert this bitstr to the equivalent Erlang external representation.
     *
     * @param buf
     *            an output stream to which the encoded bitstr should be
     *            written.
     */
    @Override
    public void encode(final OtpOutputStream buf) {
        buf.write_bitstr(bin, pad_bits);
    }

    /**
     * Determine if two bitstrs are equal. Bitstrs are equal if they have the
     * same byte length and tail length, and the array of bytes is identical.
     *
     * @param o
     *            the bitstr to compare to.
     *
     * @return true if the bitstrs contain the same bits, false otherwise.
     */
    @Override
    public boolean equals(final Object o) {
        if (!(o instanceof OtpErlangBitstr)) {
            return false;
        }

        final OtpErlangBitstr that = (OtpErlangBitstr) o;
        if (pad_bits != that.pad_bits) {
            return false;
        }

        final int len = bin.length;
        if (len != that.bin.length) {
            return false;
        }

        for (int i = 0; i < len; i++) {
            if (bin[i] != that.bin[i]) {
                return false; // early exit
            }
        }

        return true;
    }

    @Override
    protected int doHashCode() {
        final OtpErlangObject.Hash hash = new OtpErlangObject.Hash(15);
        hash.combine(bin);
        hash.combine(pad_bits);
        return hash.valueOf();
    }

    @Override
    public Object clone() {
        final OtpErlangBitstr that = (OtpErlangBitstr) super.clone();
        that.bin = bin.clone();
        that.pad_bits = pad_bits;
        return that;
    }
}
