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
 * Provides a Java representation of Erlang PIDs. PIDs represent Erlang
 * processes and consist of a nodename and a number of integers.
 */
public class OtpErlangPid extends OtpErlangObject implements Comparable<Object> {
    // don't change this!
    private static final long serialVersionUID = 1664394142301803659L;

    private final int tag;
    private final String node;
    private final int id;
    private final int serial;
    private final int creation;

    /**
     * Create a unique Erlang PID belonging to the local node.
     *
     * @param self
     *            the local node.
     *
     * @deprecated use OtpLocalNode:createPid() instead
     */
    @Deprecated
    public OtpErlangPid(final OtpLocalNode self) {
        final OtpErlangPid p = self.createPid();

	tag = p.tag;
        id = p.id;
        serial = p.serial;
        creation = p.creation;
        node = p.node;
    }

    /**
     * Create an Erlang PID from a stream containing a PID encoded in Erlang
     * external format.
     *
     * @param buf
     *            the stream containing the encoded PID.
     *
     * @exception OtpErlangDecodeException
     *                if the buffer does not contain a valid external
     *                representation of an Erlang PID.
     */
    public OtpErlangPid(final OtpInputStream buf)
            throws OtpErlangDecodeException {
        final OtpErlangPid p = buf.read_pid();

	tag = p.tag;
        node = p.node();
        id = p.id();
        serial = p.serial();
        creation = p.creation();
    }

    /**
     * Create an Erlang pid from its components.
     *
     * @param node
     *            the nodename.
     *
     * @param id
     *            an arbitrary number. Only the low order 15 bits will be used.
     *
     * @param serial
     *            another arbitrary number. Only the low order 13 bits will be
     *            used.
     *
     * @param creation
     *            yet another arbitrary number. Ony the low order 2 bits will
     *            be used.
     */
    public OtpErlangPid(final String node, final int id, final int serial,
			final int creation) {
	this(OtpExternal.pidTag, node, id, serial, creation);
    }

    /**
     * Create an Erlang pid from its components.
     *
     * @param tag
     *            the external format to be compliant with
     *            OtpExternal.pidTag where only a subset of the bits are significant (see other constructor).
     *            OtpExternal.newPidTag where all 32 bits of id,serial and creation are significant.
     *            newPidTag can only be decoded by OTP-19 and newer.
     * @param node
     *            the nodename.
     *
     * @param id
     *            an arbitrary number.
     *
     * @param serial
     *            another arbitrary number.
     *
     * @param creation
     *            yet another arbitrary number.
     */
    protected OtpErlangPid(final int tag, final String node, final int id,
			   final int serial, final int creation) {
	this.tag = tag;
	this.node = node;
	if (tag == OtpExternal.pidTag) {
	    this.id = id & 0x7fff; // 15 bits
	    this.serial = serial & 0x1fff; // 13 bits
	    this.creation = creation & 0x03; // 2 bits
	}
	else {  // allow all 32 bits for newPidTag
	    this.id = id;
	    this.serial = serial;
	    this.creation = creation;
	}
    }

    protected int tag() {
	return tag;
    }

    /**
     * Get the serial number from the PID.
     *
     * @return the serial number from the PID.
     */
    public int serial() {
        return serial;
    }

    /**
     * Get the id number from the PID.
     *
     * @return the id number from the PID.
     */
    public int id() {
        return id;
    }

    /**
     * Get the creation number from the PID.
     *
     * @return the creation number from the PID.
     */
    public int creation() {
        return creation;
    }

    /**
     * Get the node name from the PID.
     *
     * @return the node name from the PID.
     */
    public String node() {
        return node;
    }

    /**
     * Get the string representation of the PID. Erlang PIDs are printed as
     * #Pid&lt;node.id.serial&gt;
     *
     * @return the string representation of the PID.
     */
    @Override
    public String toString() {
        return "#Pid<" + node.toString() + "." + id + "." + serial + ">";
    }

    /**
     * Convert this PID to the equivalent Erlang external representation.
     *
     * @param buf
     *            an output stream to which the encoded PID should be written.
     */
    @Override
    public void encode(final OtpOutputStream buf) {
        buf.write_pid(this);
    }

    /**
     * Determine if two PIDs are equal. PIDs are equal if their components are
     * equal.
     *
     * @param o
     *            the other PID to compare to.
     *
     * @return true if the PIDs are equal, false otherwise.
     */
    @Override
    public boolean equals(final Object o) {
        if (!(o instanceof OtpErlangPid)) {
            return false;
        }

        final OtpErlangPid pid = (OtpErlangPid) o;

        return creation == pid.creation && serial == pid.serial && id == pid.id
                && node.compareTo(pid.node) == 0;
    }

    @Override
    protected int doHashCode() {
        final OtpErlangObject.Hash hash = new OtpErlangObject.Hash(5);
        hash.combine(creation, serial);
        hash.combine(id, node.hashCode());
        return hash.valueOf();
    }

    public int compareTo(final Object o) {
        if (!(o instanceof OtpErlangPid)) {
            return -1;
        }

        final OtpErlangPid pid = (OtpErlangPid) o;
        if (creation == pid.creation) {
            if (serial == pid.serial) {
                if (id == pid.id) {
                    return node.compareTo(pid.node);
                }
                return id - pid.id;
            }
            return serial - pid.serial;
        }
        return creation - pid.creation;
    }
}
