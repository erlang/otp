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
 * Provides a Java representation of Erlang tuples. Tuples are created from one
 * or more arbitrary Erlang terms.
 *
 * <p>
 * The arity of the tuple is the number of elements it contains. Elements are
 * indexed from 0 to (arity-1) and can be retrieved individually by using the
 * appropriate index.
 */
public class OtpErlangTuple extends OtpErlangObject {
    // don't change this!
    private static final long serialVersionUID = 9163498658004915935L;

    private static final OtpErlangObject[] NO_ELEMENTS = new OtpErlangObject[0];

    private OtpErlangObject[] elems = NO_ELEMENTS;

    /**
     * Create a unary tuple containing the given element.
     *
     * @param elem
     *            the element to create the tuple from.
     *
     * @exception java.lang.IllegalArgumentException
     *                if the element is null.
     */
    public OtpErlangTuple(final OtpErlangObject elem) {
        if (elem == null) {
            throw new java.lang.IllegalArgumentException(
                    "Tuple element cannot be null");
        }
        elems = new OtpErlangObject[] { elem };
    }

    /**
     * Create a tuple from an array of terms.
     *
     * @param elems
     *            the array of terms to create the tuple from.
     *
     * @exception java.lang.IllegalArgumentException
     *                if the array is empty (null) or contains null elements.
     */
    public OtpErlangTuple(final OtpErlangObject[] elems) {
        this(elems, 0, elems.length);
    }

    /**
     * Create a tuple from an array of terms.
     *
     * @param elems
     *            the array of terms to create the tuple from.
     * @param start
     *            the offset of the first term to insert.
     * @param count
     *            the number of terms to insert.
     *
     * @exception java.lang.IllegalArgumentException
     *                if the array is empty (null) or contains null elements.
     */
    public OtpErlangTuple(final OtpErlangObject[] elems, final int start,
            final int count) {
        if (elems == null) {
            throw new java.lang.IllegalArgumentException(
                    "Tuple content can't be null");
        } else if (count < 1) {
            this.elems = NO_ELEMENTS;
        } else {
            this.elems = new OtpErlangObject[count];
            for (int i = 0; i < count; i++) {
                if (elems[start + i] != null) {
                    this.elems[i] = elems[start + i];
                } else {
                    throw new java.lang.IllegalArgumentException(
                            "Tuple element cannot be null (element"
                                    + (start + i) + ")");
                }
            }
        }
    }

    /**
     * Create a tuple from a stream containing an tuple encoded in Erlang
     * external format.
     *
     * @param buf
     *            the stream containing the encoded tuple.
     *
     * @exception OtpErlangDecodeException
     *                if the buffer does not contain a valid external
     *                representation of an Erlang tuple.
     */
    public OtpErlangTuple(final OtpInputStream buf)
            throws OtpErlangDecodeException {
        final int arity = buf.read_tuple_head();

        if (arity > 0) {
            elems = new OtpErlangObject[arity];

            for (int i = 0; i < arity; i++) {
                elems[i] = buf.read_any();
            }
        } else {
            elems = NO_ELEMENTS;
        }
    }

    /**
     * Get the arity of the tuple.
     *
     * @return the number of elements contained in the tuple.
     */
    public int arity() {
        return elems.length;
    }

    /**
     * Get the specified element from the tuple.
     *
     * @param i
     *            the index of the requested element. Tuple elements are
     *            numbered as array elements, starting at 0.
     *
     * @return the requested element, of null if i is not a valid element index.
     */
    public OtpErlangObject elementAt(final int i) {
        if (i >= arity() || i < 0) {
            return null;
        }
        return elems[i];
    }

    /**
     * Get all the elements from the tuple as an array.
     *
     * @return an array containing all of the tuple's elements.
     */
    public OtpErlangObject[] elements() {
        final OtpErlangObject[] res = new OtpErlangObject[arity()];
        System.arraycopy(elems, 0, res, 0, res.length);
        return res;
    }

    /**
     * Get the string representation of the tuple.
     *
     * @return the string representation of the tuple.
     */
    @Override
    public String toString() {
        int i;
        final StringBuffer s = new StringBuffer();
        final int arity = elems.length;

        s.append("{");

        for (i = 0; i < arity; i++) {
            if (i > 0) {
                s.append(",");
            }
            s.append(elems[i].toString());
        }

        s.append("}");

        return s.toString();
    }

    /**
     * Convert this tuple to the equivalent Erlang external representation.
     *
     * @param buf
     *            an output stream to which the encoded tuple should be written.
     */
    @Override
    public void encode(final OtpOutputStream buf) {
        final int arity = elems.length;

        buf.write_tuple_head(arity);

        for (int i = 0; i < arity; i++) {
            buf.write_any(elems[i]);
        }
    }

    /**
     * Determine if two tuples are equal. Tuples are equal if they have the same
     * arity and all of the elements are equal.
     *
     * @param o
     *            the tuple to compare to.
     *
     * @return true if the tuples have the same arity and all the elements are
     *         equal.
     */
    @Override
    public boolean equals(final Object o) {
        if (!(o instanceof OtpErlangTuple)) {
            return false;
        }

        final OtpErlangTuple t = (OtpErlangTuple) o;
        final int a = arity();

        if (a != t.arity()) {
            return false;
        }

        for (int i = 0; i < a; i++) {
            if (!elems[i].equals(t.elems[i])) {
                return false; // early exit
            }
        }

        return true;
    }

    @Override
    public <T> boolean match(final OtpErlangObject term, final T bindings) {
        if (!(term instanceof OtpErlangTuple)) {
            return false;
        }
        final OtpErlangTuple t = (OtpErlangTuple) term;
        final int a = elems.length;
        if (a != t.elems.length) {
            return false;
        }
        for (int i = 0; i < a; i++) {
            if (!elems[i].match(t.elems[i], bindings)) {
                return false;
            }
        }
        return true;
    }

    @Override
    public <T> OtpErlangObject bind(final T binds) throws OtpErlangException {
        final OtpErlangTuple tuple = (OtpErlangTuple) this.clone();
        final int a = tuple.elems.length;
        for (int i = 0; i < a; i++) {
            final OtpErlangObject e = tuple.elems[i];
            tuple.elems[i] = e.bind(binds);
        }
        return tuple;
    }

    @Override
    protected int doHashCode() {
        final OtpErlangObject.Hash hash = new OtpErlangObject.Hash(9);
        final int a = arity();
        hash.combine(a);
        for (int i = 0; i < a; i++) {
            hash.combine(elems[i].hashCode());
        }
        return hash.valueOf();
    }

    @Override
    public Object clone() {
        final OtpErlangTuple newTuple = (OtpErlangTuple) super.clone();
        newTuple.elems = elems.clone();
        return newTuple;
    }
}
