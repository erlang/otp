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

import java.util.Iterator;
import java.util.NoSuchElementException;

/**
 * Provides a Java representation of Erlang lists. Lists are created from zero
 * or more arbitrary Erlang terms.
 *
 * <p>
 * The arity of the list is the number of elements it contains.
 */
public class OtpErlangList extends OtpErlangObject implements
        Iterable<OtpErlangObject> {
    // don't change this!
    private static final long serialVersionUID = 5999112769036676548L;

    private static final OtpErlangObject[] NO_ELEMENTS = new OtpErlangObject[0];

    final private OtpErlangObject[] elems;

    private OtpErlangObject lastTail = null;

    /**
     * Create an empty list.
     */
    public OtpErlangList() {
        elems = NO_ELEMENTS;
    }

    /**
     * Create a list of Erlang integers representing Unicode codePoints. This
     * method does not check if the string contains valid code points.
     *
     * @param str
     *            the characters from which to create the list.
     */
    public OtpErlangList(final String str) {
        if (str == null || str.length() == 0) {
            elems = NO_ELEMENTS;
        } else {
            final int[] codePoints = OtpErlangString.stringToCodePoints(str);
            elems = new OtpErlangObject[codePoints.length];
            for (int i = 0; i < elems.length; i++) {
                elems[i] = new OtpErlangInt(codePoints[i]);
            }
        }
    }

    /**
     * Create a list containing one element.
     *
     * @param elem
     *            the elememet to make the list from.
     */
    public OtpErlangList(final OtpErlangObject elem) {
        elems = new OtpErlangObject[] { elem };
    }

    /**
     * Create a list from an array of arbitrary Erlang terms.
     *
     * @param elems
     *            the array of terms from which to create the list.
     */
    public OtpErlangList(final OtpErlangObject[] elems) {
        this(elems, 0, elems.length);
    }

    /**
     * Create a list from an array of arbitrary Erlang terms. Tail can be
     * specified, if not null, the list will not be proper.
     *
     * @param elems
     *            array of terms from which to create the list
     * @param lastTail
     * @throws OtpErlangException
     */
    public OtpErlangList(final OtpErlangObject[] elems,
            final OtpErlangObject lastTail) throws OtpErlangException {
        this(elems, 0, elems.length);
        if (elems.length == 0 && lastTail != null) {
            throw new OtpErlangException("Bad list, empty head, non-empty tail");
        }
        this.lastTail = lastTail;
    }

    /**
     * Create a list from an array of arbitrary Erlang terms.
     *
     * @param elems
     *            the array of terms from which to create the list.
     * @param start
     *            the offset of the first term to insert.
     * @param count
     *            the number of terms to insert.
     */
    public OtpErlangList(final OtpErlangObject[] elems, final int start,
            final int count) {
        if (elems != null && count > 0) {
            this.elems = new OtpErlangObject[count];
            System.arraycopy(elems, start, this.elems, 0, count);
        } else {
            this.elems = NO_ELEMENTS;
        }
    }

    /**
     * Create a list from a stream containing an list encoded in Erlang external
     * format.
     *
     * @param buf
     *            the stream containing the encoded list.
     *
     * @exception OtpErlangDecodeException
     *                if the buffer does not contain a valid external
     *                representation of an Erlang list.
     */
    public OtpErlangList(final OtpInputStream buf)
            throws OtpErlangDecodeException {
        final int arity = buf.read_list_head();
        if (arity > 0) {
            elems = new OtpErlangObject[arity];
            for (int i = 0; i < arity; i++) {
                elems[i] = buf.read_any();
            }
            /* discard the terminating nil (empty list) or read tail */
            if (buf.peek1() == OtpExternal.nilTag) {
                buf.read_nil();
            } else {
                lastTail = buf.read_any();
            }
        } else {
            elems = NO_ELEMENTS;
        }
    }

    /**
     * Get the arity of the list.
     *
     * @return the number of elements contained in the list.
     */
    public int arity() {
        return elems.length;
    }

    /**
     * Get the specified element from the list.
     *
     * @param i
     *            the index of the requested element. List elements are numbered
     *            as array elements, starting at 0.
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
     * Get all the elements from the list as an array.
     *
     * @return an array containing all of the list's elements.
     */
    public OtpErlangObject[] elements() {
        if (arity() == 0) {
            return NO_ELEMENTS;
        }
        final OtpErlangObject[] res = new OtpErlangObject[arity()];
        System.arraycopy(elems, 0, res, 0, res.length);
        return res;
    }

    /**
     * Get the string representation of the list.
     *
     * @return the string representation of the list.
     */

    @Override
    public String toString() {
        return toString(0);
    }

    protected String toString(final int start) {
        final StringBuffer s = new StringBuffer();
        s.append("[");

        for (int i = start; i < arity(); i++) {
            if (i > start) {
                s.append(",");
            }
            s.append(elems[i].toString());
        }
        if (lastTail != null) {
            s.append("|").append(lastTail.toString());
        }
        s.append("]");

        return s.toString();
    }

    /**
     * Convert this list to the equivalent Erlang external representation. Note
     * that this method never encodes lists as strings, even when it is possible
     * to do so.
     *
     * @param buf
     *            An output stream to which the encoded list should be written.
     *
     */

    @Override
    public void encode(final OtpOutputStream buf) {
        encode(buf, 0);
    }

    protected void encode(final OtpOutputStream buf, final int start) {
        final int arity = arity() - start;

        if (arity > 0) {
            buf.write_list_head(arity);

            for (int i = start; i < arity + start; i++) {
                buf.write_any(elems[i]);
            }
        }
        if (lastTail == null) {
            buf.write_nil();
        } else {
            buf.write_any(lastTail);
        }
    }

    /**
     * Determine if two lists are equal. Lists are equal if they have the same
     * arity and all of the elements are equal.
     *
     * @param o
     *            the list to compare to.
     *
     * @return true if the lists have the same arity and all the elements are
     *         equal.
     */

    @Override
    public boolean equals(final Object o) {

        /*
         * Be careful to use methods even for "this", so that equals work also
         * for sublists
         */

        if (!(o instanceof OtpErlangList)) {
            return false;
        }

        final OtpErlangList l = (OtpErlangList) o;

        final int a = arity();
        if (a != l.arity()) {
            return false;
        }
        for (int i = 0; i < a; i++) {
            if (!elementAt(i).equals(l.elementAt(i))) {
                return false; // early exit
            }
        }
        final OtpErlangObject otherTail = l.getLastTail();
        if (getLastTail() == null && otherTail == null) {
            return true;
        }
        if (getLastTail() == null) {
            return false;
        }
        return getLastTail().equals(l.getLastTail());
    }

    @Override
    public <T> boolean match(final OtpErlangObject term, final T bindings) {
        if (!(term instanceof OtpErlangList)) {
            return false;
        }
        final OtpErlangList that = (OtpErlangList) term;

        final int thisArity = this.arity();
        final int thatArity = that.arity();
        final OtpErlangObject thisTail = this.getLastTail();
        final OtpErlangObject thatTail = that.getLastTail();

        if (thisTail == null) {
            if (thisArity != thatArity || thatTail != null) {
                return false;
            }
        } else {
            if (thisArity > thatArity) {
                return false;
            }
        }
        for (int i = 0; i < thisArity; i++) {
            if (!elementAt(i).match(that.elementAt(i), bindings)) {
                return false;
            }
        }
        if (thisTail == null) {
            return true;
        }
        return thisTail.match(that.getNthTail(thisArity), bindings);
    }

    @Override
    public <T> OtpErlangObject bind(final T binds) throws OtpErlangException {
        final OtpErlangList list = (OtpErlangList) this.clone();

        final int a = list.elems.length;
        for (int i = 0; i < a; i++) {
            list.elems[i] = list.elems[i].bind(binds);
        }

        if (list.lastTail != null) {
            list.lastTail = list.lastTail.bind(binds);
        }

        return list;
    }

    public OtpErlangObject getLastTail() {
        return lastTail;
    }

    @Override
    protected int doHashCode() {
        final OtpErlangObject.Hash hash = new OtpErlangObject.Hash(4);
        final int a = arity();
        if (a == 0) {
            return (int) 3468870702L;
        }
        for (int i = 0; i < a; i++) {
            hash.combine(elementAt(i).hashCode());
        }
        final OtpErlangObject t = getLastTail();
        if (t != null) {
            final int h = t.hashCode();
            hash.combine(h, h);
        }
        return hash.valueOf();
    }

    @Override
    public Object clone() {
        try {
            return new OtpErlangList(elements(), getLastTail());
        } catch (final OtpErlangException e) {
            throw new AssertionError(this);
        }
    }

    public Iterator<OtpErlangObject> iterator() {
        return iterator(0);
    }

    private Iterator<OtpErlangObject> iterator(final int start) {
        return new Itr(start);
    }

    /**
     * @return true if the list is proper, i.e. the last tail is nil
     */
    public boolean isProper() {
        return lastTail == null;
    }

    public OtpErlangObject getHead() {
        if (arity() > 0) {
            return elems[0];
        }
        return null;
    }

    public OtpErlangObject getTail() {
        return getNthTail(1);
    }

    public OtpErlangObject getNthTail(final int n) {
        final int arity = arity();
        if (arity >= n) {
            if (arity == n && lastTail != null) {
                return lastTail;
            }
            return new SubList(this, n);
        }
        return null;
    }

    /**
     * Convert a list of integers into a Unicode string, interpreting each
     * integer as a Unicode code point value.
     *
     * @return A java.lang.String object created through its constructor
     *         String(int[], int, int).
     *
     * @exception OtpErlangException
     *                for non-proper and non-integer lists.
     *
     * @exception OtpErlangRangeException
     *                if any integer does not fit into a Java int.
     *
     * @exception java.security.InvalidParameterException
     *                if any integer is not within the Unicode range.
     *
     * @see String#String(int[], int, int)
     *
     */

    public String stringValue() throws OtpErlangException {
        if (!isProper()) {
            throw new OtpErlangException("Non-proper list: " + this);
        }
        final int[] values = new int[arity()];
        for (int i = 0; i < values.length; ++i) {
            final OtpErlangObject o = elementAt(i);
            if (!(o instanceof OtpErlangLong)) {
                throw new OtpErlangException("Non-integer term: " + o);
            }
            final OtpErlangLong l = (OtpErlangLong) o;
            values[i] = l.intValue();
        }
        return new String(values, 0, values.length);
    }

    public static class SubList extends OtpErlangList {
        private static final long serialVersionUID = OtpErlangList.serialVersionUID;

        private final int start;

        private final OtpErlangList parent;

        private SubList(final OtpErlangList parent, final int start) {
            super();
            this.parent = parent;
            this.start = start;
        }

        @Override
        public int arity() {
            return parent.arity() - start;
        }

        @Override
        public OtpErlangObject elementAt(final int i) {
            return parent.elementAt(i + start);
        }

        @Override
        public OtpErlangObject[] elements() {
            final int n = parent.arity() - start;
            final OtpErlangObject[] res = new OtpErlangObject[n];
            for (int i = 0; i < res.length; i++) {
                res[i] = parent.elementAt(i + start);
            }
            return res;
        }

        @Override
        public boolean isProper() {
            return parent.isProper();
        }

        @Override
        public OtpErlangObject getHead() {
            return parent.elementAt(start);
        }

        @Override
        public OtpErlangObject getNthTail(final int n) {
            return parent.getNthTail(n + start);
        }

        @Override
        public String toString() {
            return parent.toString(start);
        }

        @Override
        public void encode(final OtpOutputStream stream) {
            parent.encode(stream, start);
        }

        @Override
        public OtpErlangObject getLastTail() {
            return parent.getLastTail();
        }

        @Override
        public Iterator<OtpErlangObject> iterator() {
            return parent.iterator(start);
        }
    }

    private class Itr implements Iterator<OtpErlangObject> {
        /**
         * Index of element to be returned by subsequent call to next.
         */
        private int cursor;

        private Itr(final int cursor) {
            this.cursor = cursor;
        }

        public boolean hasNext() {
            return cursor < elems.length;
        }

        public OtpErlangObject next() {
            try {
                return elems[cursor++];
            } catch (final IndexOutOfBoundsException e) {
                throw new NoSuchElementException();
            }
        }

        public void remove() {
            throw new UnsupportedOperationException(
                    "OtpErlangList cannot be modified!");
        }
    }
}
