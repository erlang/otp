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

/**
 * This class represents local node types. It is used to group the node types
 * {@link OtpNode OtpNode} and {@link OtpSelf OtpSelf}.
 */
public class OtpLocalNode extends AbstractNode {
    private int serial = 0;
    private int pidCount = 1;
    private int portCount = 1;
    private int refId[];

    protected int port;
    protected java.net.Socket epmd;

    protected OtpLocalNode() {
	super();
	init();
    }

    /**
     * Create a node with the given name and the default cookie.
     */
    protected OtpLocalNode(final String node) {
	super(node);
	init();
    }

    /**
     * Create a node with the given name and cookie.
     */
    protected OtpLocalNode(final String node, final String cookie) {
	super(node, cookie);
	init();
    }

    private void init() {
	serial = 0;
	pidCount = 1;
	portCount = 1;
	refId = new int[3];
	refId[0] = 1;
	refId[1] = 0;
	refId[2] = 0;
    }

    /**
     * Get the port number used by this node.
     * 
     * @return the port number this server node is accepting connections on.
     */
    public int port() {
	return port;
    }

    /**
     * Set the Epmd socket after publishing this nodes listen port to Epmd.
     * 
     * @param s
     *                The socket connecting this node to Epmd.
     */
    protected void setEpmd(final java.net.Socket s) {
	epmd = s;
    }

    /**
     * Get the Epmd socket.
     * 
     * @return The socket connecting this node to Epmd.
     */
    protected java.net.Socket getEpmd() {
	return epmd;
    }

    /**
     * Create an Erlang {@link OtpErlangPid pid}. Erlang pids are based upon
     * some node specific information; this method creates a pid using the
     * information in this node. Each call to this method produces a unique pid.
     * 
     * @return an Erlang pid.
     */
    public synchronized OtpErlangPid createPid() {
	final OtpErlangPid p = new OtpErlangPid(node, pidCount, serial,
		creation);

	pidCount++;
	if (pidCount > 0x7fff) {
	    pidCount = 0;

	    serial++;
	    if (serial > 0x1fff) { /* 13 bits */
		serial = 0;
	    }
	}

	return p;
    }

    /**
     * Create an Erlang {@link OtpErlangPort port}. Erlang ports are based upon
     * some node specific information; this method creates a port using the
     * information in this node. Each call to this method produces a unique
     * port. It may not be meaningful to create a port in a non-Erlang
     * environment, but this method is provided for completeness.
     * 
     * @return an Erlang port.
     */
    public synchronized OtpErlangPort createPort() {
	final OtpErlangPort p = new OtpErlangPort(node, portCount, creation);

	portCount++;
	if (portCount > 0xfffffff) { /* 28 bits */
	    portCount = 0;
	}

	return p;
    }

    /**
     * Create an Erlang {@link OtpErlangRef reference}. Erlang references are
     * based upon some node specific information; this method creates a
     * reference using the information in this node. Each call to this method
     * produces a unique reference.
     * 
     * @return an Erlang reference.
     */
    public synchronized OtpErlangRef createRef() {
	final OtpErlangRef r = new OtpErlangRef(node, refId, creation);

	// increment ref ids (3 ints: 18 + 32 + 32 bits)
	refId[0]++;
	if (refId[0] > 0x3ffff) {
	    refId[0] = 0;

	    refId[1]++;
	    if (refId[1] == 0) {
		refId[2]++;
	    }
	}

	return r;
    }
}
