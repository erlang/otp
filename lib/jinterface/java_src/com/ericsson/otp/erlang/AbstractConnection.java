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

import java.io.IOException;
import java.io.OutputStream;
import java.util.Random;

/**
 * Maintains a connection between a Java process and a remote Erlang, Java or C
 * node. The object maintains connection state and allows data to be sent to and
 * received from the peer.
 *
 * <p>
 * This abstract class provides the neccesary methods to maintain the actual
 * connection and encode the messages and headers in the proper format according
 * to the Erlang distribution protocol. Subclasses can use these methods to
 * provide a more or less transparent communication channel as desired.
 * </p>
 *
 * <p>
 * Note that no receive methods are provided. Subclasses must provide methods
 * for message delivery, and may implement their own receive methods.
 * <p>
 *
 * <p>
 * If an exception occurs in any of the methods in this class, the connection
 * will be closed and must be reopened in order to resume communication with the
 * peer. This will be indicated to the subclass by passing the exception to its
 * delivery() method.
 * </p>
 *
 * <p>
 * The System property OtpConnection.trace can be used to change the initial
 * trace level setting for all connections. Normally the initial trace level is
 * 0 and connections are not traced unless {@link #setTraceLevel
 * setTraceLevel()} is used to change the setting for a particular connection.
 * OtpConnection.trace can be used to turn on tracing by default for all
 * connections.
 * </p>
 */
public abstract class AbstractConnection extends Thread {
    protected static final int headerLen = 2048; // more than enough

    protected static final byte passThrough = (byte) 0x70;
    protected static final byte version = (byte) 0x83;

    // Erlang message header tags
    protected static final int linkTag = 1;
    protected static final int sendTag = 2;
    protected static final int exitTag = 3;
    protected static final int unlinkTag = 4;
    protected static final int regSendTag = 6;
    protected static final int groupLeaderTag = 7;
    protected static final int exit2Tag = 8;

    protected static final int sendTTTag = 12;
    protected static final int exitTTTag = 13;
    protected static final int regSendTTTag = 16;
    protected static final int exit2TTTag = 18;

    // MD5 challenge messsage tags
    protected static final int ChallengeReply = 'r';
    protected static final int ChallengeAck = 'a';
    protected static final int ChallengeStatus = 's';

    private volatile boolean done = false;

    protected boolean connected = false; // connection status
    protected OtpTransport socket; // communication channel
    protected OtpPeer peer; // who are we connected to
    protected OtpLocalNode localNode; // this nodes id
    String name; // local name of this connection

    protected boolean cookieOk = false; // already checked the cookie for this
    // connection
    protected boolean sendCookie = true; // Send cookies in messages?

    // tracelevel constants
    protected int traceLevel = 0;

    protected static int defaultLevel = 0;
    protected static int sendThreshold = 1;
    protected static int ctrlThreshold = 2;
    protected static int handshakeThreshold = 3;

    protected static Random random = null;

    private int flags = 0;

    static {
        // trace this connection?
        final String trace = System.getProperties().getProperty(
                "OtpConnection.trace");
        try {
            if (trace != null) {
                defaultLevel = Integer.valueOf(trace).intValue();
            }
        } catch (final NumberFormatException e) {
            defaultLevel = 0;
        }
        random = new Random();
    }

    // private AbstractConnection() {
    // }

    /**
     * Accept an incoming connection from a remote node. Used by
     * {@link OtpSelf#accept() OtpSelf.accept()} to create a connection based on
     * data received when handshaking with the peer node, when the remote node
     * is the connection initiator.
     *
     * @exception java.io.IOException
     *                if it was not possible to connect to the peer.
     *
     * @exception OtpAuthException
     *                if handshake resulted in an authentication error
     */
    protected AbstractConnection(final OtpLocalNode self, final OtpTransport s)
            throws IOException, OtpAuthException {
        localNode = self;
        peer = new OtpPeer(self.transportFactory);
        socket = s;

        traceLevel = defaultLevel;
        setDaemon(true);

        if (traceLevel >= handshakeThreshold) {
            System.out.println("<- ACCEPT FROM " + s);
        }

        // get his info
        recvName(peer);

        // now find highest common dist value
        if (peer.proto != self.proto || self.distHigh < peer.distLow
                || self.distLow > peer.distHigh) {
            close();
            throw new IOException(
                    "No common protocol found - cannot accept connection");
        }
        // highest common version: min(peer.distHigh, self.distHigh)
        peer.distChoose = peer.distHigh > self.distHigh ? self.distHigh
                : peer.distHigh;

        doAccept();
        name = peer.node();
    }

    /**
     * Intiate and open a connection to a remote node.
     *
     * @exception java.io.IOException
     *                if it was not possible to connect to the peer.
     *
     * @exception OtpAuthException
     *                if handshake resulted in an authentication error.
     */
    protected AbstractConnection(final OtpLocalNode self, final OtpPeer other)
            throws IOException, OtpAuthException {
        peer = other;
        localNode = self;
        socket = null;
        int port;

        traceLevel = defaultLevel;
        setDaemon(true);

        // now get a connection between the two...
        port = OtpEpmd.lookupPort(peer);
        if (port == 0)
            throw new IOException("No remote node found - cannot connect");

        // now find highest common dist value
        if (peer.proto != self.proto || self.distHigh < peer.distLow
                || self.distLow > peer.distHigh) {
            throw new IOException("No common protocol found - cannot connect");
        }

        // highest common version: min(peer.distHigh, self.distHigh)
        peer.distChoose = peer.distHigh > self.distHigh ? self.distHigh
                : peer.distHigh;

        doConnect(port);

        name = peer.node();
        connected = true;
    }

    /**
     * Deliver communication exceptions to the recipient.
     */
    public abstract void deliver(Exception e);

    /**
     * Deliver messages to the recipient.
     */
    public abstract void deliver(OtpMsg msg);

    /**
     * Send a pre-encoded message to a named process on a remote node.
     *
     * @param dest
     *            the name of the remote process.
     * @param payload
     *            the encoded message to send.
     *
     * @exception java.io.IOException
     *                if the connection is not active or a communication error
     *                occurs.
     */
    protected void sendBuf(final OtpErlangPid from, final String dest,
            final OtpOutputStream payload) throws IOException {
        if (!connected) {
            throw new IOException("Not connected");
        }
        @SuppressWarnings("resource")
        final OtpOutputStream header = new OtpOutputStream(headerLen);

        // preamble: 4 byte length + "passthrough" tag + version
        header.write4BE(0); // reserve space for length
        header.write1(passThrough);
        header.write1(version);

        // header info
        header.write_tuple_head(4);
        header.write_long(regSendTag);
        header.write_any(from);
        if (sendCookie) {
            header.write_atom(localNode.cookie());
        } else {
            header.write_atom("");
        }
        header.write_atom(dest);

        // version for payload
        header.write1(version);

        // fix up length in preamble
        header.poke4BE(0, header.size() + payload.size() - 4);

        do_send(header, payload);
    }

    /**
     * Send a pre-encoded message to a process on a remote node.
     *
     * @param dest
     *            the Erlang PID of the remote process.
     * @param payload
     *            the encoded message to send.
     *
     * @exception java.io.IOException
     *                if the connection is not active or a communication error
     *                occurs.
     */
    protected void sendBuf(final OtpErlangPid from, final OtpErlangPid dest,
            final OtpOutputStream payload) throws IOException {
        if (!connected) {
            throw new IOException("Not connected");
        }
        @SuppressWarnings("resource")
        final OtpOutputStream header = new OtpOutputStream(headerLen);

        // preamble: 4 byte length + "passthrough" tag + version
        header.write4BE(0); // reserve space for length
        header.write1(passThrough);
        header.write1(version);

        // header info
        header.write_tuple_head(3);
        header.write_long(sendTag);
        if (sendCookie) {
            header.write_atom(localNode.cookie());
        } else {
            header.write_atom("");
        }
        header.write_any(dest);

        // version for payload
        header.write1(version);

        // fix up length in preamble
        header.poke4BE(0, header.size() + payload.size() - 4);

        do_send(header, payload);
    }

    /*
     * Send an auth error to peer because he sent a bad cookie. The auth error
     * uses his cookie (not revealing ours). This is just like send_reg
     * otherwise
     */
    private void cookieError(final OtpLocalNode local,
            final OtpErlangAtom cookie) throws OtpAuthException {
        try {
            @SuppressWarnings("resource")
            final OtpOutputStream header = new OtpOutputStream(headerLen);

            // preamble: 4 byte length + "passthrough" tag + version
            header.write4BE(0); // reserve space for length
            header.write1(passThrough);
            header.write1(version);

            header.write_tuple_head(4);
            header.write_long(regSendTag);
            header.write_any(local.createPid()); // disposable pid
            header.write_atom(cookie.atomValue()); // important: his cookie,
            // not mine...
            header.write_atom("auth");

            // version for payload
            header.write1(version);

            // the payload

            // the no_auth message (copied from Erlang) Don't change this
            // (Erlang will crash)
            // {$gen_cast, {print, "~n** Unauthorized cookie ~w **~n",
            // [foo@aule]}}
            final OtpErlangObject[] msg = new OtpErlangObject[2];
            final OtpErlangObject[] msgbody = new OtpErlangObject[3];

            msgbody[0] = new OtpErlangAtom("print");
            msgbody[1] = new OtpErlangString("~n** Bad cookie sent to " + local
                    + " **~n");
            // Erlang will crash and burn if there is no third argument here...
            msgbody[2] = new OtpErlangList(); // empty list

            msg[0] = new OtpErlangAtom("$gen_cast");
            msg[1] = new OtpErlangTuple(msgbody);

            @SuppressWarnings("resource")
            final OtpOutputStream payload = new OtpOutputStream(
                    new OtpErlangTuple(msg));

            // fix up length in preamble
            header.poke4BE(0, header.size() + payload.size() - 4);

            try {
                do_send(header, payload);
            } catch (final IOException e) {
            } // ignore
        } finally {
            close();
        }
        throw new OtpAuthException("Remote cookie not authorized: "
                + cookie.atomValue());
    }

    // link to pid

    /**
     * Create a link between the local node and the specified process on the
     * remote node. If the link is still active when the remote process
     * terminates, an exit signal will be sent to this connection. Use
     * {@link #sendUnlink unlink()} to remove the link.
     *
     * @param dest
     *            the Erlang PID of the remote process.
     *
     * @exception java.io.IOException
     *                if the connection is not active or a communication error
     *                occurs.
     */
    protected void sendLink(final OtpErlangPid from, final OtpErlangPid dest)
            throws IOException {
        if (!connected) {
            throw new IOException("Not connected");
        }
        @SuppressWarnings("resource")
        final OtpOutputStream header = new OtpOutputStream(headerLen);

        // preamble: 4 byte length + "passthrough" tag
        header.write4BE(0); // reserve space for length
        header.write1(passThrough);
        header.write1(version);

        // header
        header.write_tuple_head(3);
        header.write_long(linkTag);
        header.write_any(from);
        header.write_any(dest);

        // fix up length in preamble
        header.poke4BE(0, header.size() - 4);

        do_send(header);
    }

    /**
     * Remove a link between the local node and the specified process on the
     * remote node. This method deactivates links created with {@link #sendLink
     * link()}.
     *
     * @param dest
     *            the Erlang PID of the remote process.
     *
     * @exception java.io.IOException
     *                if the connection is not active or a communication error
     *                occurs.
     */
    protected void sendUnlink(final OtpErlangPid from, final OtpErlangPid dest)
            throws IOException {
        if (!connected) {
            throw new IOException("Not connected");
        }
        @SuppressWarnings("resource")
        final OtpOutputStream header = new OtpOutputStream(headerLen);

        // preamble: 4 byte length + "passthrough" tag
        header.write4BE(0); // reserve space for length
        header.write1(passThrough);
        header.write1(version);

        // header
        header.write_tuple_head(3);
        header.write_long(unlinkTag);
        header.write_any(from);
        header.write_any(dest);

        // fix up length in preamble
        header.poke4BE(0, header.size() - 4);

        do_send(header);
    }

    /* used internally when "processes" terminate */
    protected void sendExit(final OtpErlangPid from, final OtpErlangPid dest,
            final OtpErlangObject reason) throws IOException {
        sendExit(exitTag, from, dest, reason);
    }

    /**
     * Send an exit signal to a remote process.
     *
     * @param dest
     *            the Erlang PID of the remote process.
     * @param reason
     *            an Erlang term describing the exit reason.
     *
     * @exception java.io.IOException
     *                if the connection is not active or a communication error
     *                occurs.
     */
    protected void sendExit2(final OtpErlangPid from, final OtpErlangPid dest,
            final OtpErlangObject reason) throws IOException {
        sendExit(exit2Tag, from, dest, reason);
    }

    private void sendExit(final int tag, final OtpErlangPid from,
            final OtpErlangPid dest, final OtpErlangObject reason)
            throws IOException {
        if (!connected) {
            throw new IOException("Not connected");
        }
        @SuppressWarnings("resource")
        final OtpOutputStream header = new OtpOutputStream(headerLen);

        // preamble: 4 byte length + "passthrough" tag
        header.write4BE(0); // reserve space for length
        header.write1(passThrough);
        header.write1(version);

        // header
        header.write_tuple_head(4);
        header.write_long(tag);
        header.write_any(from);
        header.write_any(dest);
        header.write_any(reason);

        // fix up length in preamble
        header.poke4BE(0, header.size() - 4);

        do_send(header);
    }

    @SuppressWarnings("resource")
    @Override
    public void run() {
        if (!connected) {
            deliver(new IOException("Not connected"));
            return;
        }

        final byte[] lbuf = new byte[4];
        OtpInputStream ibuf;
        OtpErlangObject traceobj;
        int len;
        final byte[] tock = { 0, 0, 0, 0 };

        try {
            receive_loop: while (!done) {
                // don't return until we get a real message
                // or a failure of some kind (e.g. EXIT)
                // read length and read buffer must be atomic!
                do {
                    // read 4 bytes - get length of incoming packet
                    // socket.getInputStream().read(lbuf);
                    readSock(socket, lbuf);
                    ibuf = new OtpInputStream(lbuf, flags);
                    len = ibuf.read4BE();

                    // received tick? send tock!
                    if (len == 0) {
                        synchronized (this) {
                            OutputStream out = socket.getOutputStream();
                            out.write(tock);
                            out.flush();
                        }
                    }

                } while (len == 0); // tick_loop

                // got a real message (maybe) - read len bytes
                final byte[] tmpbuf = new byte[len];
                // i = socket.getInputStream().read(tmpbuf);
                readSock(socket, tmpbuf);
                ibuf.close();
                ibuf = new OtpInputStream(tmpbuf, flags);

                if (ibuf.read1() != passThrough) {
                    break receive_loop;
                }

                // got a real message (really)
                OtpErlangObject reason = null;
                OtpErlangAtom cookie = null;
                OtpErlangObject tmp = null;
                OtpErlangTuple head = null;
                OtpErlangAtom toName;
                OtpErlangPid to;
                OtpErlangPid from;
                int tag;

                // decode the header
                tmp = ibuf.read_any();
                if (!(tmp instanceof OtpErlangTuple)) {
                    break receive_loop;
                }

                head = (OtpErlangTuple) tmp;
                if (!(head.elementAt(0) instanceof OtpErlangLong)) {
                    break receive_loop;
                }

                // lets see what kind of message this is
                tag = (int) ((OtpErlangLong) head.elementAt(0)).longValue();

                switch (tag) {
                case sendTag: // { SEND, Cookie, ToPid }
                case sendTTTag: // { SEND, Cookie, ToPid, TraceToken }
                    if (!cookieOk) {
                        // we only check this once, he can send us bad cookies
                        // later if he likes
                        if (!(head.elementAt(1) instanceof OtpErlangAtom)) {
                            break receive_loop;
                        }
                        cookie = (OtpErlangAtom) head.elementAt(1);
                        if (sendCookie) {
                            if (!cookie.atomValue().equals(localNode.cookie())) {
                                cookieError(localNode, cookie);
                            }
                        } else {
                            if (!cookie.atomValue().equals("")) {
                                cookieError(localNode, cookie);
                            }
                        }
                        cookieOk = true;
                    }

                    if (traceLevel >= sendThreshold) {
                        System.out.println("<- " + headerType(head) + " "
                                + head);

                        /* show received payload too */
                        ibuf.mark(0);
                        traceobj = ibuf.read_any();

                        if (traceobj != null) {
                            System.out.println("   " + traceobj);
                        } else {
                            System.out.println("   (null)");
                        }
                        ibuf.reset();
                    }

                    to = (OtpErlangPid) head.elementAt(2);

                    deliver(new OtpMsg(to, ibuf));
                    break;

                case regSendTag: // { REG_SEND, FromPid, Cookie, ToName }
                case regSendTTTag: // { REG_SEND, FromPid, Cookie, ToName,
                    // TraceToken }
                    if (!cookieOk) {
                        // we only check this once, he can send us bad cookies
                        // later if he likes
                        if (!(head.elementAt(2) instanceof OtpErlangAtom)) {
                            break receive_loop;
                        }
                        cookie = (OtpErlangAtom) head.elementAt(2);
                        if (sendCookie) {
                            if (!cookie.atomValue().equals(localNode.cookie())) {
                                cookieError(localNode, cookie);
                            }
                        } else {
                            if (!cookie.atomValue().equals("")) {
                                cookieError(localNode, cookie);
                            }
                        }
                        cookieOk = true;
                    }

                    if (traceLevel >= sendThreshold) {
                        System.out.println("<- " + headerType(head) + " "
                                + head);

                        /* show received payload too */
                        ibuf.mark(0);
                        traceobj = ibuf.read_any();

                        if (traceobj != null) {
                            System.out.println("   " + traceobj);
                        } else {
                            System.out.println("   (null)");
                        }
                        ibuf.reset();
                    }

                    from = (OtpErlangPid) head.elementAt(1);
                    toName = (OtpErlangAtom) head.elementAt(3);

                    deliver(new OtpMsg(from, toName.atomValue(), ibuf));
                    break;

                case exitTag: // { EXIT, FromPid, ToPid, Reason }
                case exit2Tag: // { EXIT2, FromPid, ToPid, Reason }
                    if (head.elementAt(3) == null) {
                        break receive_loop;
                    }
                    if (traceLevel >= ctrlThreshold) {
                        System.out.println("<- " + headerType(head) + " "
                                + head);
                    }

                    from = (OtpErlangPid) head.elementAt(1);
                    to = (OtpErlangPid) head.elementAt(2);
                    reason = head.elementAt(3);

                    deliver(new OtpMsg(tag, from, to, reason));
                    break;

                case exitTTTag: // { EXIT, FromPid, ToPid, TraceToken, Reason }
                case exit2TTTag: // { EXIT2, FromPid, ToPid, TraceToken,
                    // Reason
                    // }
                    // as above, but bifferent element number
                    if (head.elementAt(4) == null) {
                        break receive_loop;
                    }
                    if (traceLevel >= ctrlThreshold) {
                        System.out.println("<- " + headerType(head) + " "
                                + head);
                    }

                    from = (OtpErlangPid) head.elementAt(1);
                    to = (OtpErlangPid) head.elementAt(2);
                    reason = head.elementAt(4);

                    deliver(new OtpMsg(tag, from, to, reason));
                    break;

                case linkTag: // { LINK, FromPid, ToPid}
                case unlinkTag: // { UNLINK, FromPid, ToPid}
                    if (traceLevel >= ctrlThreshold) {
                        System.out.println("<- " + headerType(head) + " "
                                + head);
                    }

                    from = (OtpErlangPid) head.elementAt(1);
                    to = (OtpErlangPid) head.elementAt(2);

                    deliver(new OtpMsg(tag, from, to));
                    break;

                // absolutely no idea what to do with these, so we ignore
                // them...
                case groupLeaderTag: // { GROUPLEADER, FromPid, ToPid}
                    // (just show trace)
                    if (traceLevel >= ctrlThreshold) {
                        System.out.println("<- " + headerType(head) + " "
                                + head);
                    }
                    break;

                default:
                    // garbage?
                    break receive_loop;
                }
            } // end receive_loop

            // this section reachable only with break
            // we have received garbage from peer
            deliver(new OtpErlangExit("Remote is sending garbage"));

        } // try

        catch (final OtpAuthException e) {
            deliver(e);
        } catch (final OtpErlangDecodeException e) {
            deliver(new OtpErlangExit("Remote is sending garbage"));
        } catch (final IOException e) {
            deliver(new OtpErlangExit("Remote has closed connection"));
        } finally {
            close();
        }
    }

    /**
     * <p>
     * Set the trace level for this connection. Normally tracing is off by
     * default unless System property OtpConnection.trace was set.
     * </p>
     *
     * <p>
     * The following levels are valid: 0 turns off tracing completely, 1 shows
     * ordinary send and receive messages, 2 shows control messages such as link
     * and unlink, 3 shows handshaking at connection setup, and 4 shows
     * communication with Epmd. Each level includes the information shown by the
     * lower ones.
     * </p>
     *
     * @param level
     *            the level to set.
     *
     * @return the previous trace level.
     */
    public int setTraceLevel(final int level) {
        final int oldLevel = traceLevel;

        // pin the value
        int theLevel = level;
        if (level < 0) {
            theLevel = 0;
        } else if (level > 4) {
            theLevel = 4;
        }

        traceLevel = theLevel;

        return oldLevel;
    }

    /**
     * Get the trace level for this connection.
     *
     * @return the current trace level.
     */
    public int getTraceLevel() {
        return traceLevel;
    }

    /**
     * Close the connection to the remote node.
     */
    public void close() {
        done = true;
        connected = false;
        synchronized (this) {
            try {
                if (socket != null) {
                    if (traceLevel >= ctrlThreshold) {
                        System.out.println("-> CLOSE");
                    }
                    socket.close();
                }
            } catch (final IOException e) { /* ignore socket close errors */
            } finally {
                socket = null;
            }
        }
    }

    @Override
    protected void finalize() {
        close();
    }

    /**
     * Determine if the connection is still alive. Note that this method only
     * reports the status of the connection, and that it is possible that there
     * are unread messages waiting in the receive queue.
     *
     * @return true if the connection is alive.
     */
    public boolean isConnected() {
        return connected;
    }

    // used by send and send_reg (message types with payload)
    protected synchronized void do_send(final OtpOutputStream header,
            final OtpOutputStream payload) throws IOException {
        try {
            if (traceLevel >= sendThreshold) {
                // Need to decode header and output buffer to show trace
                // message!
                // First make OtpInputStream, then decode.
                try {
                    final OtpErlangObject h = header.getOtpInputStream(5)
                            .read_any();
                    System.out.println("-> " + headerType(h) + " " + h);

                    OtpErlangObject o = payload.getOtpInputStream(0).read_any();
                    System.out.println("   " + o);
                    o = null;
                } catch (final OtpErlangDecodeException e) {
                    System.out.println("   " + "can't decode output buffer:"
                            + e);
                }
            }

            // group flush op in favour of possible ssh-tunneled stream
            OutputStream out = socket.getOutputStream();
            header.writeTo(out);
            payload.writeTo(out);
            out.flush();
        } catch (final IOException e) {
            close();
            throw e;
        }
    }

    // used by the other message types
    protected synchronized void do_send(final OtpOutputStream header)
            throws IOException {
        try {
            if (traceLevel >= ctrlThreshold) {
                try {
                    final OtpErlangObject h = header.getOtpInputStream(5)
                            .read_any();
                    System.out.println("-> " + headerType(h) + " " + h);
                } catch (final OtpErlangDecodeException e) {
                    System.out.println("   " + "can't decode output buffer: "
                            + e);
                }
            }
            header.writeToAndFlush(socket.getOutputStream());
        } catch (final IOException e) {
            close();
            throw e;
        }
    }

    protected String headerType(final OtpErlangObject h) {
        int tag = -1;

        if (h instanceof OtpErlangTuple) {
            tag = (int) ((OtpErlangLong) ((OtpErlangTuple) h).elementAt(0))
                    .longValue();
        }

        switch (tag) {
        case linkTag:
            return "LINK";

        case sendTag:
            return "SEND";

        case exitTag:
            return "EXIT";

        case unlinkTag:
            return "UNLINK";

        case regSendTag:
            return "REG_SEND";

        case groupLeaderTag:
            return "GROUP_LEADER";

        case exit2Tag:
            return "EXIT2";

        case sendTTTag:
            return "SEND_TT";

        case exitTTTag:
            return "EXIT_TT";

        case regSendTTTag:
            return "REG_SEND_TT";

        case exit2TTTag:
            return "EXIT2_TT";
        }

        return "(unknown type)";
    }

    /* this method now throws exception if we don't get full read */
    protected int readSock(final OtpTransport s, final byte[] b)
            throws IOException {
        int got = 0;
        final int len = b.length;
        int i;

        synchronized (this) {
            if (s == null) {
                throw new IOException("expected " + len
                        + " bytes, socket was closed");
            }
        }

        while (got < len) {
            i = s.getInputStream().read(b, got, len - got);

            if (i < 0) {
                throw new IOException("expected " + len
                        + " bytes, got EOF after " + got + " bytes");
            } else if (i == 0 && len != 0) {
                /*
                 * This is a corner case. According to
                 * http://java.sun.com/j2se/1.4.2/docs/api/ class InputStream
                 * is.read(,,l) can only return 0 if l==0. In other words it
                 * should not happen, but apparently did.
                 */
                throw new IOException("Remote connection closed");
            } else {
                got += i;
            }
        }
        return got;
    }

    protected void doAccept() throws IOException, OtpAuthException {
        try {
            sendStatus("ok");
            final int our_challenge = genChallenge();
            sendChallenge(peer.distChoose, localNode.flags, our_challenge);
            final int her_challenge = recvChallengeReply(our_challenge);
            final byte[] our_digest = genDigest(her_challenge,
                    localNode.cookie());
            sendChallengeAck(our_digest);
            connected = true;
            cookieOk = true;
            sendCookie = false;
        } catch (final IOException ie) {
            close();
            throw ie;
        } catch (final OtpAuthException ae) {
            close();
            throw ae;
        } catch (final Exception e) {
            final String nn = peer.node();
            close();
            final IOException ioe = new IOException(
                    "Error accepting connection from " + nn);
            ioe.initCause(e);
            throw ioe;
        }
        if (traceLevel >= handshakeThreshold) {
            System.out.println("<- MD5 ACCEPTED " + peer.host());
        }
    }

    protected void doConnect(final int port) throws IOException,
            OtpAuthException {
        try {
            socket = peer.createTransport(peer.host(), port);

            if (traceLevel >= handshakeThreshold) {
                System.out.println("-> MD5 CONNECT TO " + peer.host() + ":"
                        + port);
            }
            sendName(peer.distChoose, localNode.flags);
            recvStatus();
            final int her_challenge = recvChallenge();
            final byte[] our_digest = genDigest(her_challenge,
                    localNode.cookie());
            final int our_challenge = genChallenge();
            sendChallengeReply(our_challenge, our_digest);
            recvChallengeAck(our_challenge);
            cookieOk = true;
            sendCookie = false;
        } catch (final OtpAuthException ae) {
            close();
            throw ae;
        } catch (final Exception e) {
            close();
            final IOException ioe = new IOException(
                    "Cannot connect to peer node");
            ioe.initCause(e);
            throw ioe;
        }
    }

    // This is nooo good as a challenge,
    // XXX fix me.
    static protected int genChallenge() {
        return random.nextInt();
    }

    // Used to debug print a message digest
    static String hex0(final byte x) {
        final char tab[] = { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
                'a', 'b', 'c', 'd', 'e', 'f' };
        int uint;
        if (x < 0) {
            uint = x & 0x7F;
            uint |= 1 << 7;
        } else {
            uint = x;
        }
        return "" + tab[uint >>> 4] + tab[uint & 0xF];
    }

    static String hex(final byte[] b) {
        final StringBuffer sb = new StringBuffer();
        try {
            int i;
            for (i = 0; i < b.length; ++i) {
                sb.append(hex0(b[i]));
            }
        } catch (final Exception e) {
            // Debug function, ignore errors.
        }
        return sb.toString();

    }

    protected byte[] genDigest(final int challenge, final String cookie) {
        int i;
        long ch2;

        if (challenge < 0) {
            ch2 = 1L << 31;
            ch2 |= challenge & 0x7FFFFFFF;
        } else {
            ch2 = challenge;
        }
        final OtpMD5 context = new OtpMD5();
        context.update(cookie);
        context.update("" + ch2);

        final int[] tmp = context.final_bytes();
        final byte[] res = new byte[tmp.length];
        for (i = 0; i < tmp.length; ++i) {
            res[i] = (byte) (tmp[i] & 0xFF);
        }
        return res;
    }

    protected void sendName(final int dist, final int aflags)
            throws IOException {

        @SuppressWarnings("resource")
        final OtpOutputStream obuf = new OtpOutputStream();
        final String str = localNode.node();
        obuf.write2BE(str.length() + 7); // 7 bytes + nodename
        obuf.write1(AbstractNode.NTYPE_R6);
        obuf.write2BE(dist);
        obuf.write4BE(aflags);
        obuf.write(str.getBytes());

        obuf.writeToAndFlush(socket.getOutputStream());

        if (traceLevel >= handshakeThreshold) {
            System.out.println("-> " + "HANDSHAKE sendName" + " flags="
                    + aflags + " dist=" + dist + " local=" + localNode);
        }
    }

    protected void sendChallenge(final int dist, final int aflags,
            final int challenge) throws IOException {

        @SuppressWarnings("resource")
        final OtpOutputStream obuf = new OtpOutputStream();
        final String str = localNode.node();
        obuf.write2BE(str.length() + 11); // 11 bytes + nodename
        obuf.write1(AbstractNode.NTYPE_R6);
        obuf.write2BE(dist);
        obuf.write4BE(aflags);
        obuf.write4BE(challenge);
        obuf.write(str.getBytes());

        obuf.writeToAndFlush(socket.getOutputStream());

        if (traceLevel >= handshakeThreshold) {
            System.out.println("-> " + "HANDSHAKE sendChallenge" + " flags="
                    + aflags + " dist=" + dist + " challenge=" + challenge
                    + " local=" + localNode);
        }
    }

    protected byte[] read2BytePackage() throws IOException,
            OtpErlangDecodeException {

        final byte[] lbuf = new byte[2];
        byte[] tmpbuf;

        readSock(socket, lbuf);
        @SuppressWarnings("resource")
        final OtpInputStream ibuf = new OtpInputStream(lbuf, 0);
        final int len = ibuf.read2BE();
        tmpbuf = new byte[len];
        readSock(socket, tmpbuf);
        return tmpbuf;
    }

    protected void recvName(final OtpPeer apeer) throws IOException {

        String hisname = "";

        try {
            final byte[] tmpbuf = read2BytePackage();
            @SuppressWarnings("resource")
            final OtpInputStream ibuf = new OtpInputStream(tmpbuf, 0);
            byte[] tmpname;
            final int len = tmpbuf.length;
            apeer.ntype = ibuf.read1();
            if (apeer.ntype != AbstractNode.NTYPE_R6) {
                throw new IOException("Unknown remote node type");
            }
            apeer.distLow = apeer.distHigh = ibuf.read2BE();
            if (apeer.distLow < 5) {
                throw new IOException("Unknown remote node type");
            }
            apeer.flags = ibuf.read4BE();
            tmpname = new byte[len - 7];
            ibuf.readN(tmpname);
            hisname = OtpErlangString.newString(tmpname);
            // Set the old nodetype parameter to indicate hidden/normal status
            // When the old handshake is removed, the ntype should also be.
            if ((apeer.flags & AbstractNode.dFlagPublished) != 0) {
                apeer.ntype = AbstractNode.NTYPE_R4_ERLANG;
            } else {
                apeer.ntype = AbstractNode.NTYPE_R4_HIDDEN;
            }

            if ((apeer.flags & AbstractNode.dFlagExtendedReferences) == 0) {
                throw new IOException(
                        "Handshake failed - peer cannot handle extended references");
            }

            if ((apeer.flags & AbstractNode.dFlagExtendedPidsPorts) == 0) {
                throw new IOException(
                        "Handshake failed - peer cannot handle extended pids and ports");
            }

        } catch (final OtpErlangDecodeException e) {
            throw new IOException("Handshake failed - not enough data");
        }

        final int i = hisname.indexOf('@', 0);
        apeer.node = hisname;
        apeer.alive = hisname.substring(0, i);
        apeer.host = hisname.substring(i + 1, hisname.length());

        if (traceLevel >= handshakeThreshold) {
            System.out.println("<- " + "HANDSHAKE" + " ntype=" + apeer.ntype
                    + " dist=" + apeer.distHigh + " remote=" + apeer);
        }
    }

    protected int recvChallenge() throws IOException {

        int challenge;

        try {
            final byte[] buf = read2BytePackage();
            @SuppressWarnings("resource")
            final OtpInputStream ibuf = new OtpInputStream(buf, 0);
            peer.ntype = ibuf.read1();
            if (peer.ntype != AbstractNode.NTYPE_R6) {
                throw new IOException("Unexpected peer type");
            }
            peer.distLow = peer.distHigh = ibuf.read2BE();
            peer.flags = ibuf.read4BE();
            challenge = ibuf.read4BE();
            final byte[] tmpname = new byte[buf.length - 11];
            ibuf.readN(tmpname);
            final String hisname = OtpErlangString.newString(tmpname);
            if (!hisname.equals(peer.node)) {
                throw new IOException(
                        "Handshake failed - peer has wrong name: " + hisname);
            }

            if ((peer.flags & AbstractNode.dFlagExtendedReferences) == 0) {
                throw new IOException(
                        "Handshake failed - peer cannot handle extended references");
            }

            if ((peer.flags & AbstractNode.dFlagExtendedPidsPorts) == 0) {
                throw new IOException(
                        "Handshake failed - peer cannot handle extended pids and ports");
            }

        } catch (final OtpErlangDecodeException e) {
            throw new IOException("Handshake failed - not enough data");
        }

        if (traceLevel >= handshakeThreshold) {
            System.out.println("<- " + "HANDSHAKE recvChallenge" + " from="
                    + peer.node + " challenge=" + challenge + " local="
                    + localNode);
        }

        return challenge;
    }

    protected void sendChallengeReply(final int challenge, final byte[] digest)
            throws IOException {

        @SuppressWarnings("resource")
        final OtpOutputStream obuf = new OtpOutputStream();
        obuf.write2BE(21);
        obuf.write1(ChallengeReply);
        obuf.write4BE(challenge);
        obuf.write(digest);
        obuf.writeToAndFlush(socket.getOutputStream());

        if (traceLevel >= handshakeThreshold) {
            System.out.println("-> " + "HANDSHAKE sendChallengeReply"
                    + " challenge=" + challenge + " digest=" + hex(digest)
                    + " local=" + localNode);
        }
    }

    // Would use Array.equals in newer JDK...
    private boolean digests_equals(final byte[] a, final byte[] b) {
        int i;
        for (i = 0; i < 16; ++i) {
            if (a[i] != b[i]) {
                return false;
            }
        }
        return true;
    }

    protected int recvChallengeReply(final int our_challenge)
            throws IOException, OtpAuthException {

        int challenge;
        final byte[] her_digest = new byte[16];

        try {
            final byte[] buf = read2BytePackage();
            @SuppressWarnings("resource")
            final OtpInputStream ibuf = new OtpInputStream(buf, 0);
            final int tag = ibuf.read1();
            if (tag != ChallengeReply) {
                throw new IOException("Handshake protocol error");
            }
            challenge = ibuf.read4BE();
            ibuf.readN(her_digest);
            final byte[] our_digest = genDigest(our_challenge,
                    localNode.cookie());
            if (!digests_equals(her_digest, our_digest)) {
                throw new OtpAuthException("Peer authentication error.");
            }
        } catch (final OtpErlangDecodeException e) {
            throw new IOException("Handshake failed - not enough data");
        }

        if (traceLevel >= handshakeThreshold) {
            System.out.println("<- " + "HANDSHAKE recvChallengeReply"
                    + " from=" + peer.node + " challenge=" + challenge
                    + " digest=" + hex(her_digest) + " local=" + localNode);
        }

        return challenge;
    }

    protected void sendChallengeAck(final byte[] digest) throws IOException {

        @SuppressWarnings("resource")
        final OtpOutputStream obuf = new OtpOutputStream();
        obuf.write2BE(17);
        obuf.write1(ChallengeAck);
        obuf.write(digest);

        obuf.writeToAndFlush(socket.getOutputStream());

        if (traceLevel >= handshakeThreshold) {
            System.out.println("-> " + "HANDSHAKE sendChallengeAck"
                    + " digest=" + hex(digest) + " local=" + localNode);
        }
    }

    protected void recvChallengeAck(final int our_challenge)
            throws IOException, OtpAuthException {

        final byte[] her_digest = new byte[16];
        try {
            final byte[] buf = read2BytePackage();
            @SuppressWarnings("resource")
            final OtpInputStream ibuf = new OtpInputStream(buf, 0);
            final int tag = ibuf.read1();
            if (tag != ChallengeAck) {
                throw new IOException("Handshake protocol error");
            }
            ibuf.readN(her_digest);
            final byte[] our_digest = genDigest(our_challenge,
                    localNode.cookie());
            if (!digests_equals(her_digest, our_digest)) {
                throw new OtpAuthException("Peer authentication error.");
            }
        } catch (final OtpErlangDecodeException e) {
            throw new IOException("Handshake failed - not enough data");
        } catch (final Exception e) {
            throw new OtpAuthException("Peer authentication error.");
        }

        if (traceLevel >= handshakeThreshold) {
            System.out.println("<- " + "HANDSHAKE recvChallengeAck" + " from="
                    + peer.node + " digest=" + hex(her_digest) + " local="
                    + localNode);
        }
    }

    protected void sendStatus(final String status) throws IOException {

        @SuppressWarnings("resource")
        final OtpOutputStream obuf = new OtpOutputStream();
        obuf.write2BE(status.length() + 1);
        obuf.write1(ChallengeStatus);
        obuf.write(status.getBytes());

        obuf.writeToAndFlush(socket.getOutputStream());

        if (traceLevel >= handshakeThreshold) {
            System.out.println("-> " + "HANDSHAKE sendStatus" + " status="
                    + status + " local=" + localNode);
        }
    }

    protected void recvStatus() throws IOException {

        try {
            final byte[] buf = read2BytePackage();
            @SuppressWarnings("resource")
            final OtpInputStream ibuf = new OtpInputStream(buf, 0);
            final int tag = ibuf.read1();
            if (tag != ChallengeStatus) {
                throw new IOException("Handshake protocol error");
            }
            final byte[] tmpbuf = new byte[buf.length - 1];
            ibuf.readN(tmpbuf);
            final String status = OtpErlangString.newString(tmpbuf);

            if (status.compareTo("ok") != 0) {
                throw new IOException("Peer replied with status '" + status
                        + "' instead of 'ok'");
            }
        } catch (final OtpErlangDecodeException e) {
            throw new IOException("Handshake failed - not enough data");
        }
        if (traceLevel >= handshakeThreshold) {
            System.out.println("<- " + "HANDSHAKE recvStatus (ok)" + " local="
                    + localNode);
        }
    }

    public void setFlags(final int flags) {
        this.flags = flags;
    }

    public int getFlags() {
        return flags;
    }
}
