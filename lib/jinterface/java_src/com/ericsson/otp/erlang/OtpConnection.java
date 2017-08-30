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

/**
 * Maintains a connection between a Java process and a remote Erlang, Java or C
 * node. The object maintains connection state and allows data to be sent to and
 * received from the peer.
 *
 * <p>
 * Once a connection is established between the local node and a remote node,
 * the connection object can be used to send and receive messages between the
 * nodes and make rpc calls (assuming that the remote node is a real Erlang
 * node).
 *
 * <p>
 * The various receive methods are all blocking and will return only when a
 * valid message has been received or an exception is raised.
 *
 * <p>
 * If an exception occurs in any of the methods in this class, the connection
 * will be closed and must be explicitely reopened in order to resume
 * communication with the peer.
 *
 * <p>
 * It is not possible to create an instance of this class directly.
 * OtpConnection objects are returned by {@link OtpSelf#connect(OtpPeer)
 * OtpSelf.connect()} and {@link OtpSelf#accept() OtpSelf.accept()}.
 */
public class OtpConnection extends AbstractConnection {
    protected OtpSelf self;
    protected GenericQueue queue; // messages get delivered here

    /*
     * Accept an incoming connection from a remote node. Used by {@link
     * OtpSelf#accept() OtpSelf.accept()} to create a connection based on data
     * received when handshaking with the peer node, when the remote node is the
     * connection intitiator.
     *
     * @exception java.io.IOException if it was not possible to connect to the
     * peer.
     *
     * @exception OtpAuthException if handshake resulted in an authentication
     * error
     */
    // package scope
    OtpConnection(final OtpSelf self, final OtpTransport s)
            throws IOException, OtpAuthException {
        super(self, s);
        this.self = self;
        queue = new GenericQueue();
        start();
    }

    /*
     * Intiate and open a connection to a remote node.
     *
     * @exception java.io.IOException if it was not possible to connect to the
     * peer.
     *
     * @exception OtpAuthException if handshake resulted in an authentication
     * error.
     */
    // package scope
    OtpConnection(final OtpSelf self, final OtpPeer other) throws IOException,
            OtpAuthException {
        super(self, other);
        this.self = self;
        queue = new GenericQueue();
        start();
    }

    @Override
    public void deliver(final Exception e) {
        queue.put(e);
    }

    @Override
    public void deliver(final OtpMsg msg) {
        queue.put(msg);
    }

    /**
     * Get information about the node at the peer end of this connection.
     *
     * @return the {@link OtpPeer Node} representing the peer node.
     */
    public OtpPeer peer() {
        return peer;
    }

    /**
     * Get information about the node at the local end of this connection.
     *
     * @return the {@link OtpSelf Node} representing the local node.
     */
    public OtpSelf self() {
        return self;
    }

    /**
     * Return the number of messages currently waiting in the receive queue for
     * this connection.
     */
    public int msgCount() {
        return queue.getCount();
    }

    /**
     * Receive a message from a remote process. This method blocks until a valid
     * message is received or an exception is raised.
     *
     * <p>
     * If the remote node sends a message that cannot be decoded properly, the
     * connection is closed and the method throws an exception.
     *
     * @return an object containing a single Erlang term.
     *
     * @exception java.io.IOException
     *                if the connection is not active or a communication error
     *                occurs.
     *
     * @exception OtpErlangExit
     *                if an exit signal is received from a process on the peer
     *                node.
     *
     * @exception OtpAuthException
     *                if the remote node sends a message containing an invalid
     *                cookie.
     */
    public OtpErlangObject receive() throws IOException, OtpErlangExit,
            OtpAuthException {
        try {
            return receiveMsg().getMsg();
        } catch (final OtpErlangDecodeException e) {
            close();
            throw new IOException(e.getMessage());
        }
    }

    /**
     * Receive a message from a remote process. This method blocks at most for
     * the specified time, until a valid message is received or an exception is
     * raised.
     *
     * <p>
     * If the remote node sends a message that cannot be decoded properly, the
     * connection is closed and the method throws an exception.
     *
     * @param timeout
     *            the time in milliseconds that this operation will block.
     *            Specify 0 to poll the queue.
     *
     * @return an object containing a single Erlang term.
     *
     * @exception java.io.IOException
     *                if the connection is not active or a communication error
     *                occurs.
     *
     * @exception OtpErlangExit
     *                if an exit signal is received from a process on the peer
     *                node.
     *
     * @exception OtpAuthException
     *                if the remote node sends a message containing an invalid
     *                cookie.
     *
     * @exception InterruptedException
     *                if no message if the method times out before a message
     *                becomes available.
     */
    public OtpErlangObject receive(final long timeout)
            throws InterruptedException, IOException, OtpErlangExit,
            OtpAuthException {
        try {
            return receiveMsg(timeout).getMsg();
        } catch (final OtpErlangDecodeException e) {
            close();
            throw new IOException(e.getMessage());
        }
    }

    /**
     * Receive a raw (still encoded) message from a remote process. This message
     * blocks until a valid message is received or an exception is raised.
     *
     * <p>
     * If the remote node sends a message that cannot be decoded properly, the
     * connection is closed and the method throws an exception.
     *
     * @return an object containing a raw (still encoded) Erlang term.
     *
     * @exception java.io.IOException
     *                if the connection is not active or a communication error
     *                occurs.
     *
     * @exception OtpErlangExit
     *                if an exit signal is received from a process on the peer
     *                node, or if the connection is lost for any reason.
     *
     * @exception OtpAuthException
     *                if the remote node sends a message containing an invalid
     *                cookie.
     */
    public OtpInputStream receiveBuf() throws IOException, OtpErlangExit,
            OtpAuthException {
        return receiveMsg().getMsgBuf();
    }

    /**
     * Receive a raw (still encoded) message from a remote process. This message
     * blocks at most for the specified time until a valid message is received
     * or an exception is raised.
     *
     * <p>
     * If the remote node sends a message that cannot be decoded properly, the
     * connection is closed and the method throws an exception.
     *
     * @param timeout
     *            the time in milliseconds that this operation will block.
     *            Specify 0 to poll the queue.
     *
     * @return an object containing a raw (still encoded) Erlang term.
     *
     * @exception java.io.IOException
     *                if the connection is not active or a communication error
     *                occurs.
     *
     * @exception OtpErlangExit
     *                if an exit signal is received from a process on the peer
     *                node, or if the connection is lost for any reason.
     *
     * @exception OtpAuthException
     *                if the remote node sends a message containing an invalid
     *                cookie.
     *
     * @exception InterruptedException
     *                if no message if the method times out before a message
     *                becomes available.
     */
    public OtpInputStream receiveBuf(final long timeout)
            throws InterruptedException, IOException, OtpErlangExit,
            OtpAuthException {
        return receiveMsg(timeout).getMsgBuf();
    }

    /**
     * Receive a messge complete with sender and recipient information.
     *
     * @return an {@link OtpMsg OtpMsg} containing the header information about
     *         the sender and recipient, as well as the actual message contents.
     *
     * @exception java.io.IOException
     *                if the connection is not active or a communication error
     *                occurs.
     *
     * @exception OtpErlangExit
     *                if an exit signal is received from a process on the peer
     *                node, or if the connection is lost for any reason.
     *
     * @exception OtpAuthException
     *                if the remote node sends a message containing an invalid
     *                cookie.
     */
    public OtpMsg receiveMsg() throws IOException, OtpErlangExit,
            OtpAuthException {
        final Object o = queue.get();

        if (o instanceof OtpMsg) {
            return (OtpMsg) o;
        } else if (o instanceof IOException) {
            throw (IOException) o;
        } else if (o instanceof OtpErlangExit) {
            throw (OtpErlangExit) o;
        } else if (o instanceof OtpAuthException) {
            throw (OtpAuthException) o;
        }

        return null;
    }

    /**
     * Receive a messge complete with sender and recipient information. This
     * method blocks at most for the specified time.
     *
     * @param timeout
     *            the time in milliseconds that this operation will block.
     *            Specify 0 to poll the queue.
     *
     * @return an {@link OtpMsg OtpMsg} containing the header information about
     *         the sender and recipient, as well as the actual message contents.
     *
     * @exception java.io.IOException
     *                if the connection is not active or a communication error
     *                occurs.
     *
     * @exception OtpErlangExit
     *                if an exit signal is received from a process on the peer
     *                node, or if the connection is lost for any reason.
     *
     * @exception OtpAuthException
     *                if the remote node sends a message containing an invalid
     *                cookie.
     *
     * @exception InterruptedException
     *                if no message if the method times out before a message
     *                becomes available.
     */
    public OtpMsg receiveMsg(final long timeout) throws InterruptedException,
            IOException, OtpErlangExit, OtpAuthException {
        final Object o = queue.get(timeout);

        if (o instanceof OtpMsg) {
            return (OtpMsg) o;
        } else if (o instanceof IOException) {
            throw (IOException) o;
        } else if (o instanceof OtpErlangExit) {
            throw (OtpErlangExit) o;
        } else if (o instanceof OtpAuthException) {
            throw (OtpAuthException) o;
        }

        return null;
    }

    /**
     * Send a message to a process on a remote node.
     *
     * @param dest
     *            the Erlang PID of the remote process.
     * @param msg
     *            the message to send.
     *
     * @exception java.io.IOException
     *                if the connection is not active or a communication error
     *                occurs.
     */
    @SuppressWarnings("resource")
    public void send(final OtpErlangPid dest, final OtpErlangObject msg)
            throws IOException {
        // encode and send the message
        super.sendBuf(self.pid(), dest, new OtpOutputStream(msg));
    }

    /**
     * Send a message to a named process on a remote node.
     *
     * @param dest
     *            the name of the remote process.
     * @param msg
     *            the message to send.
     *
     * @exception java.io.IOException
     *                if the connection is not active or a communication error
     *                occurs.
     */
    @SuppressWarnings("resource")
    public void send(final String dest, final OtpErlangObject msg)
            throws IOException {
        // encode and send the message
        super.sendBuf(self.pid(), dest, new OtpOutputStream(msg));
    }

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
    public void sendBuf(final String dest, final OtpOutputStream payload)
            throws IOException {
        super.sendBuf(self.pid(), dest, payload);
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
    public void sendBuf(final OtpErlangPid dest, final OtpOutputStream payload)
            throws IOException {
        super.sendBuf(self.pid(), dest, payload);
    }

    /**
     * Send an RPC request to the remote Erlang node. This convenience function
     * creates the following message and sends it to 'rex' on the remote node:
     *
     * <pre>
     * { self, { call, Mod, Fun, Args, user } }
     * </pre>
     *
     * <p>
     * Note that this method has unpredicatble results if the remote node is not
     * an Erlang node.
     * </p>
     *
     * @param mod
     *            the name of the Erlang module containing the function to be
     *            called.
     * @param fun
     *            the name of the function to call.
     * @param args
     *            an array of Erlang terms, to be used as arguments to the
     *            function.
     *
     * @exception java.io.IOException
     *                if the connection is not active or a communication error
     *                occurs.
     */
    public void sendRPC(final String mod, final String fun,
            final OtpErlangObject[] args) throws IOException {
        sendRPC(mod, fun, new OtpErlangList(args));
    }

    /**
     * Send an RPC request to the remote Erlang node. This convenience function
     * creates the following message and sends it to 'rex' on the remote node:
     *
     * <pre>
     * { self, { call, Mod, Fun, Args, user } }
     * </pre>
     *
     * <p>
     * Note that this method has unpredicatble results if the remote node is not
     * an Erlang node.
     * </p>
     *
     * @param mod
     *            the name of the Erlang module containing the function to be
     *            called.
     * @param fun
     *            the name of the function to call.
     * @param args
     *            a list of Erlang terms, to be used as arguments to the
     *            function.
     *
     * @exception java.io.IOException
     *                if the connection is not active or a communication error
     *                occurs.
     */
    public void sendRPC(final String mod, final String fun,
            final OtpErlangList args) throws IOException {
        final OtpErlangObject[] rpc = new OtpErlangObject[2];
        final OtpErlangObject[] call = new OtpErlangObject[5];

        /* {self, { call, Mod, Fun, Args, user}} */

        call[0] = new OtpErlangAtom("call");
        call[1] = new OtpErlangAtom(mod);
        call[2] = new OtpErlangAtom(fun);
        call[3] = args;
        call[4] = new OtpErlangAtom("user");

        rpc[0] = self.pid();
        rpc[1] = new OtpErlangTuple(call);

        send("rex", new OtpErlangTuple(rpc));
    }

    /**
     * Receive an RPC reply from the remote Erlang node. This convenience
     * function receives a message from the remote node, and expects it to have
     * the following format:
     *
     * <pre>
     * { rex, Term }
     * </pre>
     *
     * @return the second element of the tuple if the received message is a
     *         two-tuple, otherwise null. No further error checking is
     *         performed.
     *
     * @exception java.io.IOException
     *                if the connection is not active or a communication error
     *                occurs.
     *
     * @exception OtpErlangExit
     *                if an exit signal is received from a process on the peer
     *                node.
     *
     * @exception OtpAuthException
     *                if the remote node sends a message containing an invalid
     *                cookie.
     */
    public OtpErlangObject receiveRPC() throws IOException, OtpErlangExit,
            OtpAuthException {

        final OtpErlangObject msg = receive();

        if (msg instanceof OtpErlangTuple) {
            final OtpErlangTuple t = (OtpErlangTuple) msg;
            if (t.arity() == 2) {
                return t.elementAt(1); // obs: second element
            }
        }

        return null;
    }

    /**
     * Create a link between the local node and the specified process on the
     * remote node. If the link is still active when the remote process
     * terminates, an exit signal will be sent to this connection. Use
     * {@link #unlink unlink()} to remove the link.
     *
     * @param dest
     *            the Erlang PID of the remote process.
     *
     * @exception java.io.IOException
     *                if the connection is not active or a communication error
     *                occurs.
     */
    public void link(final OtpErlangPid dest) throws IOException {
        super.sendLink(self.pid(), dest);
    }

    /**
     * Remove a link between the local node and the specified process on the
     * remote node. This method deactivates links created with {@link #link
     * link()}.
     *
     * @param dest
     *            the Erlang PID of the remote process.
     *
     * @exception java.io.IOException
     *                if the connection is not active or a communication error
     *                occurs.
     */
    public void unlink(final OtpErlangPid dest) throws IOException {
        super.sendUnlink(self.pid(), dest);
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
    public void exit(final OtpErlangPid dest, final OtpErlangObject reason)
            throws IOException {
        super.sendExit2(self.pid(), dest, reason);
    }
}
