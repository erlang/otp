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
 * Provides a Java representation of Erlang ports.
 */
public class OtpErlangPort extends OtpErlangObject {
    // don't change this!
    private static final long serialVersionUID = 4037115468007644704L;

    private final int tag;
    private final String node;
    private final int id;
    private final int creation;

    /*
     * Create a unique Erlang port belonging to the local node. Since it isn't
     * meaninful to do so, this constructor is private...
     *
     * @param self the local node.
     *
     * @deprecated use OtpLocalNode:createPort() instead
     */
    @SuppressWarnings("unused")
    private OtpErlangPort(final OtpSelf self) {
        final OtpErlangPort p = self.createPort();

	tag = p.tag;
        id = p.id;
        creation = p.creation;
        node = p.node;
    }

    /**
     * Create an Erlang port from a stream containing a port encoded in Erlang
     * external format.
     *
     * @param buf
     *            the stream containing the encoded port.
     *
     * @exception OtpErlangDecodeException
     *                if the buffer does not contain a valid external
     *                representation of an Erlang port.
     */
    public OtpErlangPort(final OtpInputStream buf)
            throws OtpErlangDecodeException {
        final OtpErlangPort p = buf.read_port();

	tag = p.tag;
        node = p.node();
        id = p.id();
        creation = p.creation();
    }

    /**
     * Create an Erlang port from its components.
     *
     * @param node
     *            the nodename.
     *
     * @param id
     *            an arbitrary number. Only the low order 28 bits will be used.
     *
     * @param creation
     *            another arbitrary number. Only the low order 2 bits will be used.
     */
    public OtpErlangPort(final String node, final int id, final int creation) {
        this(OtpExternal.portTag, node, id, creation);
    }

    /**
     * Create an Erlang port from its components.
     *
     * @param tag
     *            the external format to be compliant with.
     *            OtpExternal.portTag where only a subset of the bits are used (see other constructor)
     *            OtpExternal.newPortTag where all 32 bits of id and creation are significant.
     *            newPortTag can only be decoded by OTP-19 and newer.
     * @param node
     *            the nodename.
     *
     * @param id
     *            an arbitrary number. Only the low order 28 bits will be used.
     *
     * @param creation
     *            another arbitrary number.
     */
    public OtpErlangPort(final int tag, final String node, final int id,
			 final int creation) {
	this.tag = tag;
	this.node = node;
	if (tag == OtpExternal.portTag) {
	    this.id = id & 0xfffffff; // 28 bits
	    this.creation = creation & 0x3; // 2 bits
	}
	else {
	    this.id = id;
	    this.creation = creation;
	}
    }

    protected int tag() {
        return tag;
    }

    /**
     * Get the id number from the port.
     *
     * @return the id number from the port.
     */
    public int id() {
        return id;
    }

    /**
     * Get the creation number from the port.
     *
     * @return the creation number from the port.
     */
    public int creation() {
        return creation;
    }

    /**
     * Get the node name from the port.
     *
     * @return the node name from the port.
     */
    public String node() {
        return node;
    }

    /**
     * Get the string representation of the port. Erlang ports are printed as
     * #Port&lt;node.id&gt;.
     *
     * @return the string representation of the port.
     */
    @Override
    public String toString() {
        return "#Port<" + node + "." + id + ">";
    }

    /**
     * Convert this port to the equivalent Erlang external representation.
     *
     * @param buf
     *            an output stream to which the encoded port should be written.
     */
    @Override
    public void encode(final OtpOutputStream buf) {
        buf.write_port(this);
    }

    /**
     * Determine if two ports are equal. Ports are equal if their components are
     * equal.
     *
     * @param o
     *            the other port to compare to.
     *
     * @return true if the ports are equal, false otherwise.
     */
    @Override
    public boolean equals(final Object o) {
        if (!(o instanceof OtpErlangPort)) {
            return false;
        }

        final OtpErlangPort port = (OtpErlangPort) o;

        return creation == port.creation && id == port.id
                && node.compareTo(port.node) == 0;
    }

    @Override
    protected int doHashCode() {
        final OtpErlangObject.Hash hash = new OtpErlangObject.Hash(6);
        hash.combine(creation);
        hash.combine(id, node.hashCode());
        return hash.valueOf();
    }
}
