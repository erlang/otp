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

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

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

    private OtpMap map;

    private static class OtpMap
            extends LinkedHashMap<OtpErlangObject, OtpErlangObject> {
        private static final long serialVersionUID = -2666505810905455082L;

        public OtpMap() {
            super();
        }
    }

    /**
     * Create an empty map.
     */
    public OtpErlangMap() {
        map = new OtpMap();
    }

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
        }
        map = new OtpMap();
        OtpErlangObject key, val;
        for (int i = 0; i < vcount; i++) {
            if ((key = keys[kstart + i]) == null) {
                throw new java.lang.IllegalArgumentException(
                        "Map key cannot be null (element" + (kstart + i) + ")");
            }
            if ((val = values[vstart + i]) == null) {
                throw new java.lang.IllegalArgumentException(
                        "Map value cannot be null (element" + (vstart + i)
                                + ")");
            }
            put(key, val);
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
            map = new OtpMap();
            for (int i = 0; i < arity; i++) {
                OtpErlangObject key, val;
                key = buf.read_any();
                val = buf.read_any();
                put(key, val);
            }
        } else {
            map = new OtpMap();
        }
    }

    /**
     * Get the arity of the map.
     *
     * @return the number of elements contained in the map.
     */
    public int arity() {
        return map.size();
    }

    /**
     * Put value corresponding to key into the map. For detailed behavior
     * description see {@link Map#put(Object, Object)}.
     *
     * @param key
     *            key to associate value with
     * @param value
     *            value to associate with key
     * @return previous value associated with key or null
     */
    public OtpErlangObject put(final OtpErlangObject key,
            final OtpErlangObject value) {
        return map.put(key, value);
    }

    /**
     * removes mapping for the key if present.
     *
     * @param key
     *            key for which mapping is to be remove
     * @return value associated with key or null
     */
    public OtpErlangObject remove(final OtpErlangObject key) {
        return map.remove(key);
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
        return map.get(key);
    }

    /**
     * Get all the keys from the map as an array.
     *
     * @return an array containing all of the map's keys.
     */
    public OtpErlangObject[] keys() {
        return map.keySet().toArray(new OtpErlangObject[arity()]);
    }

    /**
     * Get all the values from the map as an array.
     *
     * @return an array containing all of the map's values.
     */
    public OtpErlangObject[] values() {
        return map.values().toArray(new OtpErlangObject[arity()]);
    }

    /**
     * make Set view of the map key-value pairs
     *
     * @return a set containing key-value pairs
     */
    public Set<Entry<OtpErlangObject, OtpErlangObject>> entrySet() {
        return map.entrySet();
    }

    /**
     * Get the string representation of the map.
     *
     * @return the string representation of the map.
     */
    @Override
    public String toString() {
        final StringBuffer s = new StringBuffer();

        s.append("#{");

        boolean first = true;
        for (final Map.Entry<OtpErlangObject, OtpErlangObject> e : entrySet()) {
            if (first) {
                first = false;
            } else {
                s.append(",");
            }
            s.append(e.getKey().toString());
            s.append(" => ");
            s.append(e.getValue().toString());
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
        final int arity = arity();

        buf.write_map_head(arity);

        for (final Map.Entry<OtpErlangObject, OtpErlangObject> e : entrySet()) {
            buf.write_any(e.getKey());
            buf.write_any(e.getValue());
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
        if (a == 0) {
            return true;
        }

        OtpErlangObject key, val;
        for (final Map.Entry<OtpErlangObject, OtpErlangObject> e : entrySet()) {
            key = e.getKey();
            val = e.getValue();
            final OtpErlangObject v = t.get(key);
            if (v == null || !val.equals(v)) {
                return false;
            }
        }

        return true;
    }

    @Override
    public <T> boolean match(final OtpErlangObject term, final T binds) {
        if (!(term instanceof OtpErlangMap)) {
            return false;
        }

        final OtpErlangMap t = (OtpErlangMap) term;
        final int a = arity();

        if (a > t.arity()) {
            return false;
        }
        if (a == 0) {
            return true;
        }

        OtpErlangObject key, val;
        for (final Map.Entry<OtpErlangObject, OtpErlangObject> e : entrySet()) {
            key = e.getKey();
            val = e.getValue();
            final OtpErlangObject v = t.get(key);
            if (v == null || !val.match(v, binds)) {
                return false;
            }
        }

        return true;
    }

    @Override
    public <T> OtpErlangObject bind(final T binds) throws OtpErlangException {
        final OtpErlangMap ret = new OtpErlangMap();

        OtpErlangObject key, val;
        for (final Map.Entry<OtpErlangObject, OtpErlangObject> e : entrySet()) {
            key = e.getKey();
            val = e.getValue();
            ret.put(key, val.bind(binds));
        }

        return ret;
    }

    @Override
    protected int doHashCode() {
        final OtpErlangObject.Hash hash = new OtpErlangObject.Hash(9);
        hash.combine(map.hashCode());
        return hash.valueOf();
    }

    @Override
    @SuppressWarnings("unchecked")
    public Object clone() {
        final OtpErlangMap newMap = (OtpErlangMap) super.clone();
        newMap.map = (OtpMap) map.clone();
        return newMap;
    }
}
