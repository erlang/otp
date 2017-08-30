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

import java.io.UnsupportedEncodingException;

/**
 * Provides a Java representation of Erlang strings.
 */
public class OtpErlangString extends OtpErlangObject {
    // don't change this!
    private static final long serialVersionUID = -7053595217604929233L;

    private final String str;

    /**
     * Create an Erlang string from the given string.
     */
    public OtpErlangString(final String str) {
        this.str = str;
    }

    /**
     * Create an Erlang string from a list of integers.
     *
     * @throws OtpErlangException
     *             for non-proper and non-integer lists.
     * @throws OtpErlangRangeException
     *             if an integer in the list is not a valid Unicode code point
     *             according to Erlang.
     */
    public OtpErlangString(final OtpErlangList list) throws OtpErlangException {
        final String s = list.stringValue();
        final int n = s.length();
        for (int i = 0; i < n; i = s.offsetByCodePoints(i, 1)) {
            final int cp = s.codePointAt(i);
            if (!isValidCodePoint(cp)) {
                throw new OtpErlangRangeException("Invalid CodePoint: " + cp);
            }
        }
        str = s;
    }

    /**
     * Create an Erlang string from a stream containing a string encoded in
     * Erlang external format.
     *
     * @param buf
     *            the stream containing the encoded string.
     *
     * @exception OtpErlangDecodeException
     *                if the buffer does not contain a valid external
     *                representation of an Erlang string.
     */
    public OtpErlangString(final OtpInputStream buf)
            throws OtpErlangDecodeException {
        str = buf.read_string();
    }

    /**
     * Get the actual string contained in this object.
     *
     * @return the raw string contained in this object, without regard to Erlang
     *         quoting rules.
     *
     * @see #toString
     */
    public String stringValue() {
        return str;
    }

    /**
     * Get the printable version of the string contained in this object.
     *
     * @return the string contained in this object, quoted.
     *
     * @see #stringValue
     */

    @Override
    public String toString() {
        return "\"" + str + "\"";
    }

    /**
     * Convert this string to the equivalent Erlang external representation.
     *
     * @param buf
     *            an output stream to which the encoded string should be
     *            written.
     */

    @Override
    public void encode(final OtpOutputStream buf) {
        buf.write_string(str);
    }

    /**
     * Determine if two strings are equal. They are equal if they represent the
     * same sequence of characters. This method can be used to compare
     * OtpErlangStrings with each other and with Strings.
     *
     * @param o
     *            the OtpErlangString or String to compare to.
     *
     * @return true if the strings consist of the same sequence of characters,
     *         false otherwise.
     */

    @Override
    public boolean equals(final Object o) {
        if (o instanceof String) {
            return str.compareTo((String) o) == 0;
        } else if (o instanceof OtpErlangString) {
            return str.compareTo(((OtpErlangString) o).str) == 0;
        }

        return false;
    }

    @Override
    protected int doHashCode() {
        return str.hashCode();
    }

    /**
     * Create Unicode code points from a String.
     *
     * @param s
     *            a String to convert to an Unicode code point array
     *
     * @return the corresponding array of integers representing Unicode code
     *         points
     */

    public static int[] stringToCodePoints(final String s) {
        final int m = s.codePointCount(0, s.length());
        final int[] codePoints = new int[m];
        int j = 0;
        for (int offset = 0; offset < s.length();) {
            final int codepoint = s.codePointAt(offset);
            codePoints[j++] = codepoint;
            offset += Character.charCount(codepoint);
        }
        return codePoints;
    }

    /**
     * Validate a code point according to Erlang definition; Unicode 3.0. That
     * is; valid in the range U+0..U+10FFFF, but not in the range U+D800..U+DFFF
     * (surrogat pairs).
     *
     * @param cp
     *            the code point value to validate
     *
     * @return true if the code point is valid, false otherwise.
     */

    public static boolean isValidCodePoint(final int cp) {
        // Erlang definition of valid Unicode code points;
        // Unicode 3.0, XML, et.al.
        return cp >>> 16 <= 0x10 // in 0..10FFFF; Unicode range
                && (cp & ~0x7FF) != 0xD800; // not in D800..DFFF; surrogate
                                            // range
    }

    /**
     * Construct a String from a Latin-1 (ISO-8859-1) encoded byte array, if
     * Latin-1 is available, otherwise use the default encoding.
     *
     */
    public static String newString(final byte[] bytes) {
        try {
            return new String(bytes, "ISO-8859-1");
        } catch (final UnsupportedEncodingException e) {
        }
        return new String(bytes);
    }
}
