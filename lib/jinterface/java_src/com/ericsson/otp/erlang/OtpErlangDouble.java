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

/**
 * Provides a Java representation of Erlang floats and doubles. Erlang defines
 * only one floating point numeric type, however this class and its subclass
 * {@link OtpErlangFloat} are used to provide representations corresponding to
 * the Java types Double and Float.
 */
public class OtpErlangDouble extends OtpErlangObject {
    // don't change this!
    private static final long serialVersionUID = 132947104811974021L;

    private final double d;

    /**
     * Create an Erlang float from the given double value.
     */
    public OtpErlangDouble(final double d) {
        this.d = d;
    }

    /**
     * Create an Erlang float from a stream containing a double encoded in
     * Erlang external format.
     *
     * @param buf
     *            the stream containing the encoded value.
     *
     * @exception OtpErlangDecodeException
     *                if the buffer does not contain a valid external
     *                representation of an Erlang float.
     */
    public OtpErlangDouble(final OtpInputStream buf)
            throws OtpErlangDecodeException {
        d = buf.read_double();
    }

    /**
     * Get the value, as a double.
     *
     * @return the value of this object, as a double.
     */
    public double doubleValue() {
        return d;
    }

    /**
     * Get the value, as a float.
     *
     * @return the value of this object, as a float.
     *
     * @exception OtpErlangRangeException
     *                if the value cannot be represented as a float.
     */
    public float floatValue() throws OtpErlangRangeException {
        final float f = (float) d;

        if (f != d) {
            throw new OtpErlangRangeException("Value too large for float: " + d);
        }

        return f;
    }

    /**
     * Get the string representation of this double.
     *
     * @return the string representation of this double.
     */
    @Override
    public String toString() {
        return "" + d;
    }

    /**
     * Convert this double to the equivalent Erlang external representation.
     *
     * @param buf
     *            an output stream to which the encoded value should be written.
     */
    @Override
    public void encode(final OtpOutputStream buf) {
        buf.write_double(d);
    }

    /**
     * Determine if two floats are equal. Floats are equal if they contain the
     * same value.
     *
     * @param o
     *            the float to compare to.
     *
     * @return true if the floats have the same value.
     */
    @Override
    public boolean equals(final Object o) {
        if (!(o instanceof OtpErlangDouble)) {
            return false;
        }

        final OtpErlangDouble other = (OtpErlangDouble) o;
        return d == other.d;
    }

    @Override
    protected int doHashCode() {
        final Double v = new Double(d);
        return v.hashCode();
    }
}
