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
 * Provides a Java representation of Erlang atoms. Atoms can be created from
 * strings whose length is not more than {@link #maxAtomLength maxAtomLength}
 * characters.
 */
public class OtpErlangAtom extends OtpErlangObject {
    // don't change this!
    private static final long serialVersionUID = -3204386396807876641L;

    /** The maximun allowed length of an atom, in characters */
    public static final int maxAtomLength = 0xff; // one byte length

    private final String atom;

    /**
     * Create an atom from the given string.
     *
     * @param atom
     *            the string to create the atom from.
     *
     * @exception java.lang.IllegalArgumentException
     *                if the string is null or contains more than
     *                {@link #maxAtomLength maxAtomLength} characters.
     */
    public OtpErlangAtom(final String atom) {
        if (atom == null) {
            throw new java.lang.IllegalArgumentException("null string value");
        }

        if (atom.codePointCount(0, atom.length()) > maxAtomLength) {
            throw new java.lang.IllegalArgumentException("Atom may not exceed "
                    + maxAtomLength + " characters: " + atom);
        }
        this.atom = atom;
    }

    /**
     * Create an atom from a stream containing an atom encoded in Erlang
     * external format.
     *
     * @param buf
     *            the stream containing the encoded atom.
     *
     * @exception OtpErlangDecodeException
     *                if the buffer does not contain a valid external
     *                representation of an Erlang atom.
     */
    public OtpErlangAtom(final OtpInputStream buf)
            throws OtpErlangDecodeException {
        atom = buf.read_atom();
    }

    /**
     * Create an atom whose value is "true" or "false".
     */
    public OtpErlangAtom(final boolean t) {
        atom = String.valueOf(t);
    }

    /**
     * Get the actual string contained in this object.
     *
     * @return the raw string contained in this object, without regard to Erlang
     *         quoting rules.
     *
     * @see #toString
     */
    public String atomValue() {
        return atom;
    }

    /**
     * The boolean value of this atom.
     *
     * @return the value of this atom expressed as a boolean value. If the atom
     *         consists of the characters "true" (independent of case) the value
     *         will be true. For any other values, the value will be false.
     *
     */
    public boolean booleanValue() {
        return Boolean.valueOf(atomValue()).booleanValue();
    }

    /**
     * Get the printname of the atom represented by this object. The difference
     * between this method and {link #atomValue atomValue()} is that the
     * printname is quoted and escaped where necessary, according to the Erlang
     * rules for atom naming.
     *
     * @return the printname representation of this atom object.
     *
     * @see #atomValue
     */
    @Override
    public String toString() {
        if (atomNeedsQuoting(atom)) {
            return "'" + escapeSpecialChars(atom) + "'";
        }
        return atom;
    }

    /**
     * Determine if two atoms are equal.
     *
     * @param o
     *            the other object to compare to.
     *
     * @return true if the atoms are equal, false otherwise.
     */
    @Override
    public boolean equals(final Object o) {

        if (!(o instanceof OtpErlangAtom)) {
            return false;
        }

        final OtpErlangAtom other = (OtpErlangAtom) o;
        return atom.compareTo(other.atom) == 0;
    }

    @Override
    protected int doHashCode() {
        return atom.hashCode();
    }

    /**
     * Convert this atom to the equivalent Erlang external representation.
     *
     * @param buf
     *            an output stream to which the encoded atom should be written.
     */
    @Override
    public void encode(final OtpOutputStream buf) {
        buf.write_atom(atom);
    }

    /* the following four predicates are helpers for the toString() method */
    private boolean isErlangDigit(final char c) {
        return c >= '0' && c <= '9';
    }

    private boolean isErlangUpper(final char c) {
        return c >= 'A' && c <= 'Z' || c == '_';
    }

    private boolean isErlangLower(final char c) {
        return c >= 'a' && c <= 'z';
    }

    private boolean isErlangLetter(final char c) {
        return isErlangLower(c) || isErlangUpper(c);
    }

    // true if the atom should be displayed with quotation marks
    private boolean atomNeedsQuoting(final String s) {
        char c;

        if (s.length() == 0) {
            return true;
        }
        if (!isErlangLower(s.charAt(0))) {
            return true;
        }

        final int len = s.length();
        for (int i = 1; i < len; i++) {
            c = s.charAt(i);

            if (!isErlangLetter(c) && !isErlangDigit(c) && c != '@') {
                return true;
            }
        }
        return false;
    }

    /*
     * Get the atom string, with special characters escaped. Note that this
     * function currently does not consider any characters above 127 to be
     * printable.
     */
    private String escapeSpecialChars(final String s) {
        char c;
        final StringBuffer so = new StringBuffer();

        final int len = s.length();
        for (int i = 0; i < len; i++) {
            c = s.charAt(i);

            /*
             * note that some of these escape sequences are unique to Erlang,
             * which is why the corresponding 'case' values use octal. The
             * resulting string is, of course, in Erlang format.
             */

            switch (c) {
            // some special escape sequences
            case '\b':
                so.append("\\b");
                break;

            case 0177:
                so.append("\\d");
                break;

            case 033:
                so.append("\\e");
                break;

            case '\f':
                so.append("\\f");
                break;

            case '\n':
                so.append("\\n");
                break;

            case '\r':
                so.append("\\r");
                break;

            case '\t':
                so.append("\\t");
                break;

            case 013:
                so.append("\\v");
                break;

            case '\\':
                so.append("\\\\");
                break;

            case '\'':
                so.append("\\'");
                break;

            case '\"':
                so.append("\\\"");
                break;

            default:
                // some other character classes
                if (c < 027) {
                    // control chars show as "\^@", "\^A" etc
                    so.append("\\^" + (char) ('A' - 1 + c));
                } else if (c > 126) {
                    // 8-bit chars show as \345 \344 \366 etc
                    so.append("\\" + Integer.toOctalString(c));
                } else {
                    // character is printable without modification!
                    so.append(c);
                }
            }
        }
        return new String(so);
    }

}
