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

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.net.InetAddress;

/**
 * Provides methods for registering, unregistering and looking up nodes with the
 * Erlang portmapper daemon (Epmd). For each registered node, Epmd maintains
 * information about the port on which incoming connections are accepted, as
 * well as which versions of the Erlang communication protocol the node
 * supports.
 *
 * <p>
 * Nodes wishing to contact other nodes must first request information from Epmd
 * before a connection can be set up, however this is done automatically by
 * {@link OtpSelf#connect(OtpPeer) OtpSelf.connect()} when necessary.
 *
 * <p>
 * The methods {@link #publishPort(OtpLocalNode) publishPort()} and
 * {@link #unPublishPort(OtpLocalNode) unPublishPort()} will fail if an Epmd
 * process is not running on the localhost. Additionally
 * {@link #lookupPort(AbstractNode) lookupPort()} will fail if there is no Epmd
 * process running on the host where the specified node is running. See the
 * Erlang documentation for information about starting Epmd.
 *
 * <p>
 * This class contains only static methods, there are no constructors.
 */
public class OtpEpmd {

    private static class EpmdPort {
        private static int epmdPort = 0;

        public static int get() {
            if (epmdPort == 0) {
                String env;
                try {
                    env = System.getenv("ERL_EPMD_PORT");
                } catch (final java.lang.SecurityException e) {
                    env = null;
                }
                epmdPort = env != null ? Integer.parseInt(env) : 4369;
            }
            return epmdPort;
        }

        public static void set(final int port) {
            epmdPort = port;
        }
    }

    // common values
    private static final byte stopReq = (byte) 115;

    private static final byte port4req = (byte) 122;
    private static final byte port4resp = (byte) 119;
    private static final byte publish4req = (byte) 120;
    private static final byte publish4resp = (byte) 121;
    private static final byte names4req = (byte) 110;

    private static int traceLevel = 0;
    private static final int traceThreshold = 4;

    static {
        // debug this connection?
        final String trace = System.getProperties().getProperty(
                "OtpConnection.trace");
        try {
            if (trace != null) {
                traceLevel = Integer.valueOf(trace).intValue();
            }
        } catch (final NumberFormatException e) {
            traceLevel = 0;
        }
    }

    // only static methods: no public constructors
    // hmm, idea: singleton constructor could spawn epmd process
    private OtpEpmd() {
    }

    /**
     * Set the port number to be used to contact the epmd process. Only needed
     * when the default port is not desired and system environment variable
     * ERL_EPMD_PORT can not be read (applet).
     */
    public static void useEpmdPort(final int port) {
        EpmdPort.set(port);
    }

    /**
     * Determine what port a node listens for incoming connections on.
     *
     * @return the listen port for the specified node, or 0 if the node was not
     *         registered with Epmd.
     *
     * @exception java.io.IOException
     *                if there was no response from the name server.
     */
    public static int lookupPort(final AbstractNode node) throws IOException {
        return r4_lookupPort(node);
    }

    /**
     * Register with Epmd, so that other nodes are able to find and connect to
     * it.
     *
     * @param node
     *            the server node that should be registered with Epmd.
     *
     * @return true if the operation was successful. False if the node was
     *         already registered.
     *
     * @exception java.io.IOException
     *                if there was no response from the name server.
     */
    public static boolean publishPort(final OtpLocalNode node)
            throws IOException {
        OtpTransport s = null;

        s = r4_publish(node);

        node.setEpmd(s);

        return s != null;
    }

    // Ask epmd to close his end of the connection.
    // Caller should close his epmd socket as well.
    // This method is pretty forgiving...
    /**
     * Unregister from Epmd. Other nodes wishing to connect will no longer be
     * able to.
     *
     * <p>
     * This method does not report any failures.
     */
    public static void unPublishPort(final OtpLocalNode node) {
        OtpTransport s = null;

        try {
            s = node.createTransport((String) null, EpmdPort.get());
            @SuppressWarnings("resource")
            final OtpOutputStream obuf = new OtpOutputStream();
            obuf.write2BE(node.alive().length() + 1);
            obuf.write1(stopReq);
            obuf.writeN(node.alive().getBytes());
            obuf.writeToAndFlush(s.getOutputStream());
            // don't even wait for a response (is there one?)
            if (traceLevel >= traceThreshold) {
                System.out.println("-> UNPUBLISH " + node + " port="
                        + node.port());
                System.out.println("<- OK (assumed)");
            }
        } catch (final Exception e) {/* ignore all failures */
        } finally {
            try {
                if (s != null) {
                    s.close();
                }
            } catch (final IOException e) { /* ignore close failure */
            }
            s = null;
        }
    }

    private static int r4_lookupPort(final AbstractNode node)
            throws IOException {
        int port = 0;
        OtpTransport s = null;

        try {
            @SuppressWarnings("resource")
            final OtpOutputStream obuf = new OtpOutputStream();
            s = node.createTransport(node.host(), EpmdPort.get());

            // build and send epmd request
            // length[2], tag[1], alivename[n] (length = n+1)
            obuf.write2BE(node.alive().length() + 1);
            obuf.write1(port4req);
            obuf.writeN(node.alive().getBytes());

            // send request
            obuf.writeToAndFlush(s.getOutputStream());

            if (traceLevel >= traceThreshold) {
                System.out.println("-> LOOKUP (r4) " + node);
            }

            // receive and decode reply
            // resptag[1], result[1], port[2], ntype[1], proto[1],
            // disthigh[2], distlow[2], nlen[2], alivename[n],
            // elen[2], edata[m]
            final byte[] tmpbuf = new byte[100];

            final int n = s.getInputStream().read(tmpbuf);

            if (n < 0) {
                s.close();
                throw new IOException("Nameserver not responding on "
                        + node.host() + " when looking up " + node.alive());
            }

            @SuppressWarnings("resource")
            final OtpInputStream ibuf = new OtpInputStream(tmpbuf, 0);

            final int response = ibuf.read1();
            if (response == port4resp) {
                final int result = ibuf.read1();
                if (result == 0) {
                    port = ibuf.read2BE();

                    node.ntype = ibuf.read1();
                    node.proto = ibuf.read1();
                    node.distHigh = ibuf.read2BE();
                    node.distLow = ibuf.read2BE();
                    // ignore rest of fields
                }
            }
        } catch (final IOException e) {
            if (traceLevel >= traceThreshold) {
                System.out.println("<- (no response)");
            }
            throw new IOException("Nameserver not responding on " + node.host()
                    + " when looking up " + node.alive(), e);
        } catch (final OtpErlangDecodeException e) {
            if (traceLevel >= traceThreshold) {
                System.out.println("<- (invalid response)");
            }
            throw new IOException("Nameserver not responding on " + node.host()
                    + " when looking up " + node.alive());
        } finally {
            try {
                if (s != null) {
                    s.close();
                }
            } catch (final IOException e) { /* ignore close errors */
            }
            s = null;
        }

        if (traceLevel >= traceThreshold) {
            if (port == 0) {
                System.out.println("<- NOT FOUND");
            } else {
                System.out.println("<- PORT " + port);
            }
        }
        return port;
    }

    /*
     * this function will get an exception if it tries to talk to a very old
     * epmd, or if something else happens that it cannot forsee. In both cases
     * we return an exception. We no longer support r3, so the exception is
     * fatal. If we manage to successfully communicate with an r4 epmd, we
     * return either the socket, or null, depending on the result.
     */
    private static OtpTransport r4_publish(final OtpLocalNode node)
            throws IOException {
        OtpTransport s = null;

        try {
            @SuppressWarnings("resource")
            final OtpOutputStream obuf = new OtpOutputStream();
            s = node.createTransport((String) null, EpmdPort.get());

            obuf.write2BE(node.alive().length() + 13);

            obuf.write1(publish4req);
            obuf.write2BE(node.port());

            obuf.write1(node.type());

            obuf.write1(node.proto());
            obuf.write2BE(node.distHigh());
            obuf.write2BE(node.distLow());

            obuf.write2BE(node.alive().length());
            obuf.writeN(node.alive().getBytes());
            obuf.write2BE(0); // No extra

            // send request
            obuf.writeToAndFlush(s.getOutputStream());

            if (traceLevel >= traceThreshold) {
                System.out.println("-> PUBLISH (r4) " + node + " port="
                        + node.port());
            }

            // get reply
            final byte[] tmpbuf = new byte[100];
            final int n = s.getInputStream().read(tmpbuf);

            if (n < 0) {
                s.close();
                throw new IOException("Nameserver not responding on "
                        + node.host() + " when publishing " + node.alive());
            }

            @SuppressWarnings("resource")
            final OtpInputStream ibuf = new OtpInputStream(tmpbuf, 0);

            final int response = ibuf.read1();
            if (response == publish4resp) {
                final int result = ibuf.read1();
                if (result == 0) {
                    node.creation = ibuf.read2BE();
                    if (traceLevel >= traceThreshold) {
                        System.out.println("<- OK");
                    }
                    return s; // success
                }
            }
        } catch (final IOException e) {
            // epmd closed the connection = fail
            if (s != null) {
                s.close();
            }
            if (traceLevel >= traceThreshold) {
                System.out.println("<- (no response)");
            }
            throw new IOException("Nameserver not responding on " + node.host()
                    + " when publishing " + node.alive());
        } catch (final OtpErlangDecodeException e) {
            s.close();
            if (traceLevel >= traceThreshold) {
                System.out.println("<- (invalid response)");
            }
            throw new IOException("Nameserver not responding on " + node.host()
                    + " when publishing " + node.alive());
        }

        s.close();
        return null;
    }

    public static String[] lookupNames() throws IOException {
        return lookupNames(InetAddress.getByName(null),
                new OtpSocketTransportFactory());
    }

    public static String[] lookupNames(
            final OtpTransportFactory transportFactory) throws IOException {
        return lookupNames(InetAddress.getByName(null), transportFactory);
    }

    public static String[] lookupNames(final InetAddress address)
            throws IOException {
        return lookupNames(address, new OtpSocketTransportFactory());
    }

    public static String[] lookupNames(final InetAddress address,
            final OtpTransportFactory transportFactory) throws IOException {
        OtpTransport s = null;

        try {
            @SuppressWarnings("resource")
            final OtpOutputStream obuf = new OtpOutputStream();
            try {
                s = transportFactory.createTransport(address, EpmdPort.get());

                obuf.write2BE(1);
                obuf.write1(names4req);
                // send request
                obuf.writeToAndFlush(s.getOutputStream());

                if (traceLevel >= traceThreshold) {
                    System.out.println("-> NAMES (r4) ");
                }

                // get reply
                final byte[] buffer = new byte[256];
                final ByteArrayOutputStream out = new ByteArrayOutputStream(256);
                while (true) {
                    final int bytesRead = s.getInputStream().read(buffer);
                    if (bytesRead == -1) {
                        break;
                    }
                    out.write(buffer, 0, bytesRead);
                }
                final byte[] tmpbuf = out.toByteArray();
                @SuppressWarnings("resource")
                final OtpInputStream ibuf = new OtpInputStream(tmpbuf, 0);
                ibuf.read4BE(); // read port int
                // final int port = ibuf.read4BE();
                // check if port = epmdPort

                final int n = tmpbuf.length;
                final byte[] buf = new byte[n - 4];
                System.arraycopy(tmpbuf, 4, buf, 0, n - 4);
                final String all = OtpErlangString.newString(buf);
                return all.split("\n");
            } finally {
                if (s != null) {
                    s.close();
                }
            }

        } catch (final IOException e) {
            if (traceLevel >= traceThreshold) {
                System.out.println("<- (no response)");
            }
            throw new IOException(
                    "Nameserver not responding when requesting names");
        } catch (final OtpErlangDecodeException e) {
            if (traceLevel >= traceThreshold) {
                System.out.println("<- (invalid response)");
            }
            throw new IOException(
                    "Nameserver not responding when requesting names");
        }
    }

}
