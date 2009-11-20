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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.net.InetAddress;
import java.net.UnknownHostException;

/**
 * <p>
 * Represents an OTP node.
 * </p>
 * 
 * <p>
 * About nodenames: Erlang nodenames consist of two components, an alivename and
 * a hostname separated by '@'. Additionally, there are two nodename formats:
 * short and long. Short names are of the form "alive@hostname", while long
 * names are of the form "alive@host.fully.qualified.domainname". Erlang has
 * special requirements regarding the use of the short and long formats, in
 * particular they cannot be mixed freely in a network of communicating nodes,
 * however Jinterface makes no distinction. See the Erlang documentation for
 * more information about nodenames.
 * </p>
 * 
 * <p>
 * The constructors for the AbstractNode classes will create names exactly as
 * you provide them as long as the name contains '@'. If the string you provide
 * contains no '@', it will be treated as an alivename and the name of the local
 * host will be appended, resulting in a shortname. Nodenames longer than 255
 * characters will be truncated without warning.
 * </p>
 * 
 * <p>
 * Upon initialization, this class attempts to read the file .erlang.cookie in
 * the user's home directory, and uses the trimmed first line of the file as the
 * default cookie by those constructors lacking a cookie argument. If for any
 * reason the file cannot be found or read, the default cookie will be set to
 * the empty string (""). The location of a user's home directory is determined
 * using the system property "user.home", which may not be automatically set on
 * all platforms.
 * </p>
 * 
 * <p>
 * Instances of this class cannot be created directly, use one of the subclasses
 * instead.
 * </p>
 */
public class AbstractNode {
    static String localHost = null;
    String node;
    String host;
    String alive;
    String cookie;
    static String defaultCookie = null;

    // Node types
    static final int NTYPE_R6 = 110; // 'n' post-r5, all nodes
    static final int NTYPE_R4_ERLANG = 109; // 'm' Only for source compatibility
    static final int NTYPE_R4_HIDDEN = 104; // 'h' Only for source compatibility

    // Node capability flags
    static final int dFlagPublished = 1;
    static final int dFlagAtomCache = 2;
    static final int dFlagExtendedReferences = 4;
    static final int dFlagDistMonitor = 8;
    static final int dFlagFunTags = 0x10;
    static final int dFlagDistMonitorName = 0x20; // NOT USED
    static final int dFlagHiddenAtomCache = 0x40; // NOT SUPPORTED
    static final int dflagNewFunTags = 0x80;
    static final int dFlagExtendedPidsPorts = 0x100;
    static final int dFlagExportPtrTag = 0x200; // NOT SUPPORTED
    static final int dFlagBitBinaries = 0x400;
    static final int dFlagNewFloats = 0x800;

    int ntype = NTYPE_R6;
    int proto = 0; // tcp/ip
    int distHigh = 5; // Cannot talk to nodes before R6
    int distLow = 5; // Cannot talk to nodes before R6
    int creation = 0;
    int flags = dFlagExtendedReferences | dFlagExtendedPidsPorts
	    | dFlagBitBinaries | dFlagNewFloats | dFlagFunTags
	    | dflagNewFunTags;

    /* initialize hostname and default cookie */
    static {
	try {
	    localHost = InetAddress.getLocalHost().getHostName();
	    /*
	     * Make sure it's a short name, i.e. strip of everything after first
	     * '.'
	     */
	    final int dot = localHost.indexOf(".");
	    if (dot != -1) {
		localHost = localHost.substring(0, dot);
	    }
	} catch (final UnknownHostException e) {
	    localHost = "localhost";
	}

	final String dotCookieFilename = System.getProperty("user.home")
		+ File.separator + ".erlang.cookie";
	BufferedReader br = null;

	try {
	    final File dotCookieFile = new File(dotCookieFilename);

	    br = new BufferedReader(new FileReader(dotCookieFile));
	    defaultCookie = br.readLine().trim();
	} catch (final IOException e) {
	    defaultCookie = "";
	} finally {
	    try {
		if (br != null) {
		    br.close();
		}
	    } catch (final IOException e) {
	    }
	}
    }

    protected AbstractNode() {
    }

    /**
     * Create a node with the given name and the default cookie.
     */
    protected AbstractNode(final String node) {
	this(node, defaultCookie);
    }

    /**
     * Create a node with the given name and cookie.
     */
    protected AbstractNode(final String name, final String cookie) {
	this.cookie = cookie;

	final int i = name.indexOf('@', 0);
	if (i < 0) {
	    alive = name;
	    host = localHost;
	} else {
	    alive = name.substring(0, i);
	    host = name.substring(i + 1, name.length());
	}

	if (alive.length() > 0xff) {
	    alive = alive.substring(0, 0xff);
	}

	node = alive + "@" + host;
    }

    /**
     * Get the name of this node.
     * 
     * @return the name of the node represented by this object.
     */
    public String node() {
	return node;
    }

    /**
     * Get the hostname part of the nodename. Nodenames are composed of two
     * parts, an alivename and a hostname, separated by '@'. This method returns
     * the part of the nodename following the '@'.
     * 
     * @return the hostname component of the nodename.
     */
    public String host() {
	return host;
    }

    /**
     * Get the alivename part of the hostname. Nodenames are composed of two
     * parts, an alivename and a hostname, separated by '@'. This method returns
     * the part of the nodename preceding the '@'.
     * 
     * @return the alivename component of the nodename.
     */
    public String alive() {
	return alive;
    }

    /**
     * Get the authorization cookie used by this node.
     * 
     * @return the authorization cookie used by this node.
     */
    public String cookie() {
	return cookie;
    }

    // package scope
    int type() {
	return ntype;
    }

    // package scope
    int distHigh() {
	return distHigh;
    }

    // package scope
    int distLow() {
	return distLow;
    }

    // package scope: useless information?
    int proto() {
	return proto;
    }

    // package scope
    int creation() {
	return creation;
    }

    /**
     * Set the authorization cookie used by this node.
     * 
     * @return the previous authorization cookie used by this node.
     */
    public String setCookie(final String cookie) {
	final String prev = this.cookie;
	this.cookie = cookie;
	return prev;
    }

    @Override
    public String toString() {
	return node();
    }
}
