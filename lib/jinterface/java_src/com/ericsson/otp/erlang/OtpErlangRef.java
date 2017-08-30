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
 * Provides a Java representation of Erlang refs. There are two styles of Erlang
 * refs, old style (one id value) and new style (array of id values). This class
 * manages both types.
 */
public class OtpErlangRef extends OtpErlangObject {
    // don't change this!
    private static final long serialVersionUID = -7022666480768586521L;

    private final int tag;
    private final String node;
    private final int creation;

    // old style refs have one 18-bit id
    // r6 "new" refs have array of ids, first one is only 18 bits however
    // 19 "newer" refs have full 32-bits for creation and for ids[0]
    private int ids[] = null;

    /**
     * Create a unique Erlang ref belonging to the local node.
     *
     * @param self
     *            the local node.
     *
     * @deprecated use OtpLocalNode:createRef() instead
     */
    @Deprecated
    public OtpErlangRef(final OtpLocalNode self) {
        final OtpErlangRef r = self.createRef();

	tag = r.tag;
        ids = r.ids;
        creation = r.creation;
        node = r.node;
    }

    /**
     * Create an Erlang ref from a stream containing a ref encoded in Erlang
     * external format.
     *
     * @param buf
     *            the stream containing the encoded ref.
     *
     * @exception OtpErlangDecodeException
     *                if the buffer does not contain a valid external
     *                representation of an Erlang ref.
     */
    public OtpErlangRef(final OtpInputStream buf)
            throws OtpErlangDecodeException {
        final OtpErlangRef r = buf.read_ref();

	tag = r.tag;
        node = r.node();
        creation = r.creation();

        ids = r.ids();
    }

    /**
     * Create an old style Erlang ref from its components.
     *
     * @param node
     *            the nodename.
     *
     * @param id
     *            an arbitrary number. Only the low order 18 bits will be used.
     *
     * @param creation
     *            another arbitrary number.
     */
    public OtpErlangRef(final String node, final int id, final int creation) {
	this.tag = OtpExternal.newRefTag;
        this.node = node;
        ids = new int[1];
        ids[0] = id & 0x3ffff; // 18 bits
        this.creation = creation & 0x03; // 2 bits
    }

    /**
     * Create a new style Erlang ref from its components.
     *
     * @param node
     *            the nodename.
     *
     * @param ids
     *            an array of arbitrary numbers. Only the low order 18 bits of
     *            the first number will be used. If the array contains only one
     *            number, an old style ref will be written instead. At most
     *            three numbers will be read from the array.
     *
     * @param creation
     *            another arbitrary number. Only the low order 2 bits will be
     *            used.
     */
    public OtpErlangRef(final String node, final int[] ids, final int creation) {
	this(OtpExternal.newRefTag, node, ids, creation);
    }

    /**
     * Create a new(er) style Erlang ref from its components.
     *
     * @param tag
     *            the external format to be compliant with.
     *            OtpExternal.newRefTag where only a subset of the bits are used (see other constructor)
     *            OtpExternal.newerRefTag where all bits of ids and creation are used.
     *            newerPortTag can only be decoded by OTP-19 and newer.
     *
     * @param node
     *            the nodename.
     *
     * @param ids
     *            an array of arbitrary numbers. At most three numbers
     *            will be read from the array.
     *
     * @param creation
     *            another arbitrary number.
     */
    public OtpErlangRef(final int tag, final String node, final int[] ids,
			final int creation) {
	this.tag = tag;
        this.node = node;

        // use at most 3 words
        int len = ids.length;
        this.ids = new int[3];
        this.ids[0] = 0;
        this.ids[1] = 0;
        this.ids[2] = 0;

        if (len > 3) {
            len = 3;
        }
        System.arraycopy(ids, 0, this.ids, 0, len);
	if (tag == OtpExternal.newRefTag) {
	    this.creation = creation & 0x3;
	    this.ids[0] &= 0x3ffff; // only 18 significant bits in first number
	}
	else {
	    this.creation = creation;
	}
    }

    protected int tag() {
        return tag;
    }

    /**
     * Get the id number from the ref. Old style refs have only one id number.
     * If this is a new style ref, the first id number is returned.
     *
     * @return the id number from the ref.
     */
    public int id() {
        return ids[0];
    }

    /**
     * Get the array of id numbers from the ref. If this is an old style ref,
     * the array is of length 1. If this is a new style ref, the array has
     * length 3.
     *
     * @return the array of id numbers from the ref.
     */
    public int[] ids() {
        return ids;
    }

    /**
     * Determine whether this is a new style ref.
     *
     * @return true if this ref is a new style ref, false otherwise.
     */
    public boolean isNewRef() {
        return ids.length > 1;
    }

    /**
     * Get the creation number from the ref.
     *
     * @return the creation number from the ref.
     */
    public int creation() {
        return creation;
    }

    /**
     * Get the node name from the ref.
     *
     * @return the node name from the ref.
     */
    public String node() {
        return node;
    }

    /**
     * Get the string representation of the ref. Erlang refs are printed as
     * #Ref&lt;node.id&gt;
     *
     * @return the string representation of the ref.
     */
    @Override
    public String toString() {
        String s = "#Ref<" + node;

        for (int i = 0; i < ids.length; i++) {
            s += "." + ids[i];
        }

        s += ">";

        return s;
    }

    /**
     * Convert this ref to the equivalent Erlang external representation.
     *
     * @param buf
     *            an output stream to which the encoded ref should be written.
     */
    @Override
    public void encode(final OtpOutputStream buf) {
        buf.write_ref(this);
    }

    /**
     * Determine if two refs are equal. Refs are equal if their components are
     * equal. New refs and old refs are considered equal if the node, creation
     * and first id numnber are equal.
     *
     * @param o
     *            the other ref to compare to.
     *
     * @return true if the refs are equal, false otherwise.
     */
    @Override
    public boolean equals(final Object o) {
        if (!(o instanceof OtpErlangRef)) {
            return false;
        }

        final OtpErlangRef ref = (OtpErlangRef) o;

        if (!(node.equals(ref.node()) && creation == ref.creation())) {
            return false;
        }

        if (isNewRef() && ref.isNewRef()) {
            return ids[0] == ref.ids[0] && ids[1] == ref.ids[1]
                    && ids[2] == ref.ids[2];
        }
        return ids[0] == ref.ids[0];
    }

    /**
     * Compute the hashCode value for a given ref. This function is compatible
     * with equal.
     *
     * @return the hashCode of the node.
     **/

    @Override
    protected int doHashCode() {
        final OtpErlangObject.Hash hash = new OtpErlangObject.Hash(7);
        hash.combine(creation, ids[0]);
        if (isNewRef()) {
            hash.combine(ids[1], ids[2]);
        }
        return hash.valueOf();
    }

    @Override
    public Object clone() {
        final OtpErlangRef newRef = (OtpErlangRef) super.clone();
        newRef.ids = ids.clone();
        return newRef;
    }
}
