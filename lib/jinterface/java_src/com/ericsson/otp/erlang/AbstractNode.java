/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2000-2022. All Rights Reserved.
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
public class AbstractNode implements OtpTransportFactory {
    static String localHost = null;
    String node;
    String host;
    String alive;
    String cookie;
    static String defaultCookie = null;
    final OtpTransportFactory transportFactory;

    static final int NTYPE_R6 = 110; // 'n' post-r5, all nodes

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
    static final int dFlagExportPtrTag = 0x200;
    static final int dFlagBitBinaries = 0x400;
    static final int dFlagNewFloats = 0x800;
    static final int dFlagUnicodeIo = 0x1000;
    static final int dFlagUtf8Atoms = 0x10000;
    static final int dFlagMapTag = 0x20000;
    static final int dFlagBigCreation = 0x40000;
    static final int dFlagHandshake23 = 0x1000000;
    static final int dFlagUnlinkId = 0x2000000;
    static final int dFlagMandatory25Digest = 0x4000000;
    static final long dFlagV4PidsRefs = 0x4L << 32;

    /* Mandatory flags in OTP 25. */
    static final long mandatoryFlags25 = dFlagExtendedReferences
        | dFlagFunTags
        | dFlagExtendedPidsPorts
        | dFlagUtf8Atoms
        | dflagNewFunTags
        | dFlagBigCreation
        | dFlagNewFloats
        | dFlagMapTag
        | dFlagExportPtrTag
        | dFlagBitBinaries
        | dFlagHandshake23;

    /* New mandatory flags in OTP 26 */
    static final long mandatoryFlags26 = dFlagV4PidsRefs
        | dFlagUnlinkId;
    
    /* Mandatory flags for distribution. Keep them in sync with
       DFLAG_DIST_MANDATORY in erts/emulator/beam/dist.h. */
    static final long mandatoryFlags = mandatoryFlags25
        | mandatoryFlags26;

    int ntype = NTYPE_R6;
    int proto = 0; // tcp/ip
    int distHigh = 6;
    int distLow = 6; // Cannot talk to nodes before OTP 23
    private int creation = 0x710000;
    long flags = mandatoryFlags
        | dFlagMandatory25Digest;

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

        final String homeDir = getHomeDir();
        final String dotCookieFilename = homeDir + File.separator
                + ".erlang.cookie";
        BufferedReader br = null;

        try {
            final File dotCookieFile = new File(dotCookieFilename);

            br = new BufferedReader(new FileReader(dotCookieFile));
            final String line = br.readLine();
            if (line == null) {
                defaultCookie = "";
            } else {
                defaultCookie = line.trim();
            }
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

    protected AbstractNode(final OtpTransportFactory transportFactory) {
        this.transportFactory = transportFactory;
    }

    /**
     * Create a node with the given name and default cookie and transport
     * factory.
     */
    protected AbstractNode(final String node) {
        this(node, defaultCookie, new OtpSocketTransportFactory());
    }

    /**
     * Create a node with the given name, transport factory and the default
     * cookie.
     */
    protected AbstractNode(final String node,
            final OtpTransportFactory transportFactory) {
        this(node, defaultCookie, transportFactory);
    }

    /**
     * Create a node with the given name, cookie and default transport factory.
     */
    protected AbstractNode(final String name, final String cookie) {
        this(name, cookie, new OtpSocketTransportFactory());
    }

    /**
     * Create a node with the given name, cookie and transport factory.
     */
    protected AbstractNode(final String name, final String cookie,
            final OtpTransportFactory transportFactory) {
        this.cookie = cookie;
        this.transportFactory = transportFactory;

        final int i = name.indexOf('@');
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

    void setCreation(int cr) throws OtpErlangDecodeException {
        if (cr == 0) {
            throw new OtpErlangDecodeException("Node creation 0 not allowed");
        }
        this.creation = cr;
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

    private static String getHomeDir() {
        final String home = System.getProperty("user.home");
        if (System.getProperty("os.name").toLowerCase().contains("windows")) {
            final String drive = System.getenv("HOMEDRIVE");
            final String path = System.getenv("HOMEPATH");
            return drive != null && path != null ? drive + path : home;
        }
        return home;
    }

    public OtpTransport createTransport(final String addr, final int port)
            throws IOException {
        return transportFactory.createTransport(addr, port);
    }

    public OtpTransport createTransport(final InetAddress addr, final int port)
            throws IOException {
        return transportFactory.createTransport(addr, port);
    }

    public OtpServerTransport createServerTransport(final int port)
            throws IOException {
        return transportFactory.createServerTransport(port);
    }

    /**
     * Create a client-side transport for alternative distribution protocols
     * using a transport factory extending the OtpGenericTransportFactory
     * abstract class. Connect it to the specified server.
     *
     * @param peer
     *            the peer identifying the server to connect to
     *
     */
    public OtpTransport createTransport(final OtpPeer peer)
            throws IOException {
        if (transportFactory instanceof OtpGenericTransportFactory) {
            return ((OtpGenericTransportFactory) transportFactory)
                .createTransport(peer);
        }
        throw new IOException("Method createTransport(OtpPeer) " +
                              "applicable only for Nodes with a transport " +
                              "factory instance of OtpGenericTransportFactory");
    }

    /**
     * Create a server-side transport for alternative distribution protocols
     * using a transport factory extending the OtpGenericTransportFactory
     * abstract class.
     *
     * @param node
     *            the local node identifying the transport to create server-side
     *
     */
    public OtpServerTransport createServerTransport(final OtpLocalNode node)
            throws IOException {
        if (transportFactory instanceof OtpGenericTransportFactory) {
            return ((OtpGenericTransportFactory) transportFactory)
                .createServerTransport(node);
        }
        throw new IOException("Method createServerTransport(OtpLocalNode) " +
                              "applicable only for Nodes with a transport " +
                              "factory instance of OtpGenericTransportFactory");
    }
}
