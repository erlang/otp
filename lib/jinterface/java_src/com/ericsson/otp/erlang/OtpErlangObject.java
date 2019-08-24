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

import java.io.Serializable;

/**
 * Base class of the Erlang data type classes. This class is used to represent
 * an arbitrary Erlang term.
 */
public abstract class OtpErlangObject implements Serializable, Cloneable {
    protected int hashCodeValue = 0;

    // don't change this!
    static final long serialVersionUID = -8435938572339430044L;

    /**
     * @return the printable representation of the object. This is usually
     *         similar to the representation used by Erlang for the same type of
     *         object.
     */
    @Override
    public abstract String toString();

    /**
     * Convert the object according to the rules of the Erlang external format.
     * This is mainly used for sending Erlang terms in messages, however it can
     * also be used for storing terms to disk.
     *
     * @param buf
     *            an output stream to which the encoded term should be written.
     */
    public abstract void encode(OtpOutputStream buf);

    /**
     * Read binary data in the Erlang external format, and produce a
     * corresponding Erlang data type object. This method is normally used when
     * Erlang terms are received in messages, however it can also be used for
     * reading terms from disk.
     *
     * @param buf
     *            an input stream containing one or more encoded Erlang terms.
     *
     * @return an object representing one of the Erlang data types.
     *
     * @exception OtpErlangDecodeException
     *                if the stream does not contain a valid representation of
     *                an Erlang term.
     */
    public static OtpErlangObject decode(final OtpInputStream buf)
            throws OtpErlangDecodeException {
        return buf.read_any();
    }

    /**
     * Determine if two Erlang objects are equal. In general, Erlang objects are
     * equal if the components they consist of are equal.
     *
     * @param o
     *            the object to compare to.
     *
     * @return true if the objects are identical.
     */
    @Override
    public abstract boolean equals(Object o);

    /**
     * Perform match operation against given term.
     *
     * @param term
     *            the object to match
     * @param binds
     *            variable bindings
     * @return true if match succeeded
     */
    public <T> boolean match(final OtpErlangObject term, final T binds) {
        return equals(term);
    }

    /**
     * Make new Erlang term replacing variables with the respective values from
     * bindings argument(s).
     *
     * @param binds
     *            variable bindings
     * @return new term
     * @throws OtpErlangException
     */
    public <T> OtpErlangObject bind(final T binds) throws OtpErlangException {
        return this;
    }

    @Override
    public int hashCode() {
        if (hashCodeValue == 0) {
            hashCodeValue = doHashCode();
        }
        return hashCodeValue;
    }

    protected int doHashCode() {
        return super.hashCode();
    }

    @Override
    public Object clone() {
        try {
            return super.clone();
        } catch (final CloneNotSupportedException e) {
            /* cannot happen */
            throw new InternalError(e.toString());
        }
    }

    protected final static class Hash {
        int abc[] = { 0, 0, 0 };

        /*
         * Hash function suggested by Bob Jenkins. The same as in the Erlang VM
         * (beam); utils.c.
         */

        private final static int HASH_CONST[] = { 0, // not used
                0x9e3779b9, // the golden ratio; an arbitrary value
                0x3c6ef372, // (hashHConst[1] * 2) % (1<<32)
                0xdaa66d2b, // 1 3
                0x78dde6e4, // 1 4
                0x1715609d, // 1 5
                0xb54cda56, // 1 6
                0x5384540f, // 1 7
                0xf1bbcdc8, // 1 8
                0x8ff34781, // 1 9
                0x2e2ac13a, // 1 10
                0xcc623af3, // 1 11
                0x6a99b4ac, // 1 12
                0x08d12e65, // 1 13
                0xa708a81e, // 1 14
                0x454021d7, // 1 15
        };

        protected Hash(final int i) {
            abc[0] = abc[1] = HASH_CONST[i];
            abc[2] = 0;
        }

        // protected Hash() {
        // Hash(1);
        // }

        private void mix() {
            abc[0] -= abc[1];
            abc[0] -= abc[2];
            abc[0] ^= abc[2] >>> 13;
            abc[1] -= abc[2];
            abc[1] -= abc[0];
            abc[1] ^= abc[0] << 8;
            abc[2] -= abc[0];
            abc[2] -= abc[1];
            abc[2] ^= abc[1] >>> 13;
            abc[0] -= abc[1];
            abc[0] -= abc[2];
            abc[0] ^= abc[2] >>> 12;
            abc[1] -= abc[2];
            abc[1] -= abc[0];
            abc[1] ^= abc[0] << 16;
            abc[2] -= abc[0];
            abc[2] -= abc[1];
            abc[2] ^= abc[1] >>> 5;
            abc[0] -= abc[1];
            abc[0] -= abc[2];
            abc[0] ^= abc[2] >>> 3;
            abc[1] -= abc[2];
            abc[1] -= abc[0];
            abc[1] ^= abc[0] << 10;
            abc[2] -= abc[0];
            abc[2] -= abc[1];
            abc[2] ^= abc[1] >>> 15;
        }

        protected void combine(final int a) {
            abc[0] += a;
            mix();
        }

        protected void combine(final long a) {
            combine((int) (a >>> 32), (int) a);
        }

        protected void combine(final int a, final int b) {
            abc[0] += a;
            abc[1] += b;
            mix();
        }

        protected void combine(final byte b[]) {
            int j, k;
            for (j = 0, k = 0; j + 4 < b.length; j += 4, k += 1, k %= 3) {
                abc[k] += (b[j + 0] & 0xFF) + (b[j + 1] << 8 & 0xFF00)
                        + (b[j + 2] << 16 & 0xFF0000) + (b[j + 3] << 24);
                mix();
            }
            for (int n = 0, m = 0xFF; j < b.length; j++, n += 8, m <<= 8) {
                abc[k] += b[j] << n & m;
            }
            mix();
        }

        protected int valueOf() {
            return abc[2];
        }
    }
}
