/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2000-2009. All Rights Reserved.
 * 
 * The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved online at http://www.erlang.org/.
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 * 
 * %CopyrightEnd%
 */
package com.ericsson.otp.erlang;

import java.io.Serializable;
import java.lang.Character;
import java.io.UnsupportedEncodingException;

/**
 * Provides a Java representation of Erlang strings.
 */
public class OtpErlangString extends OtpErlangObject implements Serializable,
	Cloneable {
    // don't change this!
    static final long serialVersionUID = -7053595217604929233L;

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
     * @return an Erlang string with Unicode code units.
     *
     * @throws OtpErlangException
     *                for non-proper and non-integer lists.
     * @throws OtpErlangRangeException
     *                if an integer in the list is not
     *                a valid Unicode code point according to Erlang.
     */
    public OtpErlangString(final OtpErlangList list)
	    throws OtpErlangException {
	String s = list.stringValue();
	final int n = s.length();
	for (int i = 0;  i < n;  i = s.offsetByCodePoints(i, 1)) {
	    int cp = s.codePointAt(i);
	    if (! isValidCodePoint(cp)) {
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
    
    protected int doHashCode() {
	return str.hashCode();
    }

    /**
     * Create Unicode code points from a String.
     * 
     * @param  s
     *             a String to convert to an Unicode code point array
     *
     * @return the corresponding array of integers representing
     *         Unicode code points
     */

    public static int[] stringToCodePoints(final String s) {
	final int m = s.codePointCount(0, s.length());
	final int [] codePoints = new int[m];
	for (int i = 0, j = 0;  j < m;  i = s.offsetByCodePoints(i, 1), j++) {
	    codePoints[j] = s.codePointAt(i);
	}
	return codePoints;
    }

    /**
     * Validate a code point according to Erlang definition; Unicode 3.0.
     * That is; valid in the range U+0..U+10FFFF, but not in the range
     * U+D800..U+DFFF (surrogat pairs), nor U+FFFE..U+FFFF (non-characters).
     *
     * @param  cp
     *             the code point value to validate
     *
     * @return true if the code point is valid,
     *         false otherwise.
     */

    public static boolean isValidCodePoint(final int cp) {
	// Erlang definition of valid Unicode code points; 
	// Unicode 3.0, XML, et.al.
	return (cp>>>16) <= 0x10 // in 0..10FFFF; Unicode range
	    && (cp & ~0x7FF) != 0xD800 // not in D800..DFFF; surrogate range
	    && (cp & ~1) != 0xFFFE; // not in FFFE..FFFF; non-characters
    }

    /**
     * Construct a String from a Latin-1 (ISO-8859-1) encoded byte array,
     * if Latin-1 is available, otherwise use the default encoding. 
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
