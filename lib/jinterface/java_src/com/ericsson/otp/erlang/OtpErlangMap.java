/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2000-2013. All Rights Reserved.
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


/**
 * Provides a Java representation of Erlang maps. Maps are created from one or
 * more arbitrary Erlang terms.
 * 
 * <p>
 * The arity of the map is the number of elements it contains. The keys and
 * values can be retrieved as arrays and the value for a key can be queried.
 * 
 */
public class OtpErlangMap extends OtpErlangObject {
    // don't change this!
    private static final long serialVersionUID = -6410770117696198497L;

    private static final OtpErlangObject[] NO_ELEMENTS = new OtpErlangObject[0];

    private OtpErlangObject[] keys = NO_ELEMENTS;
    private OtpErlangObject[] values = NO_ELEMENTS;

    /**
     * Create a map from an array of keys and an array of values.
     * 
     * @param keys
     *            the array of terms to create the map keys from.
     * @param values
     *            the array of terms to create the map values from.
     * 
     * @exception java.lang.IllegalArgumentException
     *                if any array is empty (null) or contains null elements.
     */
    public OtpErlangMap(final OtpErlangObject[] keys,
	    final OtpErlangObject[] values) {
	this(keys, 0, keys.length, values, 0, values.length);
    }

    /**
     * Create a map from an array of terms.
     * 
     * @param keys
     *            the array of terms to create the map from.
     * @param kstart
     *            the offset of the first key to insert.
     * @param kcount
     *            the number of keys to insert.
     * @param values
     *            the array of values to create the map from.
     * @param vstart
     *            the offset of the first value to insert.
     * @param vcount
     *            the number of values to insert.
     * 
     * @exception java.lang.IllegalArgumentException
     *                if any array is empty (null) or contains null elements.
     * @exception java.lang.IllegalArgumentException
     *                if kcount and vcount differ.
     */
    public OtpErlangMap(final OtpErlangObject[] keys, final int kstart,
	    final int kcount, final OtpErlangObject[] values, final int vstart,
	    final int vcount) {
	if (keys == null || values == null) {
	    throw new java.lang.IllegalArgumentException(
		    "Map content can't be null");
	} else if (kcount != vcount) {
	    throw new java.lang.IllegalArgumentException(
		    "Map keys and values must have same arity");
	} else if (vcount < 1) {
	    this.keys = NO_ELEMENTS;
	    this.values = NO_ELEMENTS;
	} else {
	    this.keys = new OtpErlangObject[vcount];
	    for (int i = 0; i < vcount; i++) {
		if (keys[kstart + i] != null) {
		    this.keys[i] = keys[kstart + i];
		} else {
		    throw new java.lang.IllegalArgumentException(
			    "Map key cannot be null (element" + (kstart + i)
				    + ")");
		}
	    }
	    this.values = new OtpErlangObject[vcount];
	    for (int i = 0; i < vcount; i++) {
		if (values[vstart + i] != null) {
		    this.values[i] = values[vstart + i];
		} else {
		    throw new java.lang.IllegalArgumentException(
			    "Map value cannot be null (element" + (vstart + i)
				    + ")");
		}
	    }
	}
    }

    /**
     * Create a map from a stream containing a map encoded in Erlang external
     * format.
     * 
     * @param buf
     *            the stream containing the encoded map.
     * 
     * @exception OtpErlangDecodeException
     *                if the buffer does not contain a valid external
     *                representation of an Erlang map.
     */
    public OtpErlangMap(final OtpInputStream buf)
	    throws OtpErlangDecodeException {
	final int arity = buf.read_map_head();

	if (arity > 0) {
	    keys = new OtpErlangObject[arity];
	    values = new OtpErlangObject[arity];

	    for (int i = 0; i < arity; i++) {
		keys[i] = buf.read_any();
		values[i] = buf.read_any();
	    }
	} else {
	    keys = NO_ELEMENTS;
	    values = NO_ELEMENTS;
	}
    }

    /**
     * Get the arity of the map.
     * 
     * @return the number of elements contained in the map.
     */
    public int arity() {
	return keys.length;
    }

    /**
     * Get the specified value from the map.
     * 
     * @param key
     *            the key of the requested value.
     * 
     * @return the requested value, of null if key is not a valid key.
     */
    public OtpErlangObject get(final OtpErlangObject key) {
	if (key == null) {
	    return null;
	}
	for (int i = 0; i < keys.length; i++) {
	    if (key.equals(keys[i])) {
		return values[i];
	    }
	}
	return null;
    }

    /**
     * Get all the keys from the map as an array.
     * 
     * @return an array containing all of the map's keys.
     */
    public OtpErlangObject[] keys() {
	final OtpErlangObject[] res = new OtpErlangObject[arity()];
	System.arraycopy(keys, 0, res, 0, res.length);
	return res;
    }

    /**
     * Get all the values from the map as an array.
     * 
     * @return an array containing all of the map's values.
     */
    public OtpErlangObject[] values() {
	final OtpErlangObject[] res = new OtpErlangObject[arity()];
	System.arraycopy(values, 0, res, 0, res.length);
	return res;
    }

    /**
     * Get the string representation of the map.
     * 
     * @return the string representation of the map.
     */
    @Override
    public String toString() {
	int i;
	final StringBuffer s = new StringBuffer();
	final int arity = values.length;

	s.append("#{");

	for (i = 0; i < arity; i++) {
	    if (i > 0) {
		s.append(",");
	    }
	    s.append(keys[i].toString());
	    s.append(" => ");
	    s.append(values[i].toString());
	}

	s.append("}");

	return s.toString();
    }

    /**
     * Convert this map to the equivalent Erlang external representation.
     * 
     * @param buf
     *            an output stream to which the encoded map should be written.
     */
    @Override
    public void encode(final OtpOutputStream buf) {
	final int arity = values.length;

	buf.write_map_head(arity);

	for (int i = 0; i < arity; i++) {
	    buf.write_any(keys[i]);
	    buf.write_any(values[i]);
	}
    }

    /**
     * Determine if two maps are equal. Maps are equal if they have the same
     * arity and all of the elements are equal.
     * 
     * @param o
     *            the map to compare to.
     * 
     * @return true if the maps have the same arity and all the elements are
     *         equal.
     */
    @Override
    public boolean equals(final Object o) {
	if (!(o instanceof OtpErlangMap)) {
	    return false;
	}

	final OtpErlangMap t = (OtpErlangMap) o;
	final int a = arity();

	if (a != t.arity()) {
	    return false;
	}

	for (int i = 0; i < a; i++) {
	    if (!keys[i].equals(t.keys[i])) {
		return false; // early exit
	    }
	}
	for (int i = 0; i < a; i++) {
	    if (!values[i].equals(t.values[i])) {
		return false; // early exit
	    }
	}

	return true;
    }

    @Override
    protected int doHashCode() {
	final OtpErlangObject.Hash hash = new OtpErlangObject.Hash(9);
	final int a = arity();
	hash.combine(a);
	for (int i = 0; i < a; i++) {
	    hash.combine(keys[i].hashCode());
	}
	for (int i = 0; i < a; i++) {
	    hash.combine(values[i].hashCode());
	}
	return hash.valueOf();
    }

    @Override
    public Object clone() {
	final OtpErlangMap newMap = (OtpErlangMap) super.clone();
	newMap.values = values.clone();
	return newMap;
    }
}
