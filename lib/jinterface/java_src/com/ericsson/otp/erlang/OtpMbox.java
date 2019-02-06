/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2000-2017. All Rights Reserved.
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
 * <p>
 * Provides a simple mechanism for exchanging messages with Erlang processes or
 * other instances of this class.
 * </p>
 *
 * <p>
 * Each mailbox is associated with a unique {@link OtpErlangPid pid} that
 * contains information necessary for delivery of messages. When sending
 * messages to named processes or mailboxes, the sender pid is made available to
 * the recipient of the message. When sending messages to other mailboxes, the
 * recipient can only respond if the sender includes the pid as part of the
 * message contents. The sender can determine his own pid by calling
 * {@link #self() self()}.
 * </p>
 *
 * <p>
 * Mailboxes can be named, either at creation or later. Messages can be sent to
 * named mailboxes and named Erlang processes without knowing the
 * {@link OtpErlangPid pid} that identifies the mailbox. This is necessary in
 * order to set up initial communication between parts of an application. Each
 * mailbox can have at most one name.
 * </p>
 *
 * <p>
 * Since this class was intended for communication with Erlang, all of the send
 * methods take {@link OtpErlangObject OtpErlangObject} arguments. However this
 * class can also be used to transmit arbitrary Java objects (as long as they
 * implement one of java.io.Serializable or java.io.Externalizable) by
 * encapsulating the object in a {@link OtpErlangBinary OtpErlangBinary}.
 * </p>
 *
 * <p>
 * Messages to remote nodes are externalized for transmission, and as a result
 * the recipient receives a <b>copy</b> of the original Java object. To ensure
 * consistent behaviour when messages are sent between local mailboxes, such
 * messages are cloned before delivery.
 * </p>
 *
 * <p>
 * Additionally, mailboxes can be linked in much the same way as Erlang
 * processes. If a link is active when a mailbox is {@link #close closed}, any
 * linked Erlang processes or OtpMboxes will be sent an exit signal. As well,
 * exit signals will be (eventually) sent if a mailbox goes out of scope and its
 * {@link #finalize finalize()} method called. However due to the nature of
 * finalization (i.e. Java makes no guarantees about when {@link #finalize
 * finalize()} will be called) it is recommended that you always explicitly
 * close mailboxes if you are using links instead of relying on finalization to
 * notify other parties in a timely manner.
 * </p>
 *
 * <p>
 * When retrieving messages from a mailbox that has received an exit signal, an
 * {@link OtpErlangExit OtpErlangExit} exception will be raised. Note that the
 * exception is queued in the mailbox along with other messages, and will not be
 * raised until it reaches the head of the queue and is about to be retrieved.
 * </p>
 *
 */
public class OtpMbox {
    OtpNode home;
    OtpErlangPid self;
    GenericQueue queue;
    String name;
    Links links;

    // package constructor: called by OtpNode:createMbox(name)
    // to create a named mbox
    OtpMbox(final OtpNode home, final OtpErlangPid self, final String name) {
        this.self = self;
        this.home = home;
        this.name = name;
        queue = new GenericQueue();
        links = new Links(10);
    }

    // package constructor: called by OtpNode:createMbox()
    // to create an anonymous
    OtpMbox(final OtpNode home, final OtpErlangPid self) {
        this(home, self, null);
    }

    /**
     * <p>
     * Get the identifying {@link OtpErlangPid pid} associated with this
     * mailbox.
     * </p>
     *
     * <p>
     * The {@link OtpErlangPid pid} associated with this mailbox uniquely
     * identifies the mailbox and can be used to address the mailbox. You can
     * send the {@link OtpErlangPid pid} to a remote communicating part so that
     * he can know where to send his response.
     * </p>
     *
     * @return the self pid for this mailbox.
     */
    public OtpErlangPid self() {
        return self;
    }

    /**
     * <p>
     * Register or remove a name for this mailbox. Registering a name for a
     * mailbox enables others to send messages without knowing the
     * {@link OtpErlangPid pid} of the mailbox. A mailbox can have at most one
     * name; if the mailbox already had a name, calling this method will
     * supercede that name.
     * </p>
     *
     * @param aname
     *            the name to register for the mailbox. Specify null to
     *            unregister the existing name from this mailbox.
     *
     * @return true if the name was available, or false otherwise.
     */
    public synchronized boolean registerName(final String aname) {
        return home.registerName(aname, this);
    }

    /**
     * Get the registered name of this mailbox.
     *
     * @return the registered name of this mailbox, or null if the mailbox had
     *         no registered name.
     */
    public String getName() {
        return name;
    }

    /**
     * Block until a message arrives for this mailbox.
     *
     * @return an {@link OtpErlangObject OtpErlangObject} representing the body
     *         of the next message waiting in this mailbox.
     *
     * @exception OtpErlangDecodeException
     *                if the message can not be decoded.
     *
     * @exception OtpErlangExit
     *                if a linked {@link OtpErlangPid pid} has exited or has
     *                sent an exit signal to this mailbox.
     */
    public OtpErlangObject receive() throws OtpErlangExit,
            OtpErlangDecodeException {
        try {
            return receiveMsg().getMsg();
        } catch (final OtpErlangExit e) {
            throw e;
        } catch (final OtpErlangDecodeException f) {
            throw f;
        }
    }

    /**
     * Wait for a message to arrive for this mailbox.
     *
     * @param timeout
     *            the time, in milliseconds, to wait for a message before
     *            returning null.
     *
     * @return an {@link OtpErlangObject OtpErlangObject} representing the body
     *         of the next message waiting in this mailbox.
     *
     * @exception OtpErlangDecodeException
     *                if the message can not be decoded.
     *
     * @exception OtpErlangExit
     *                if a linked {@link OtpErlangPid pid} has exited or has
     *                sent an exit signal to this mailbox.
     */
    public OtpErlangObject receive(final long timeout) throws OtpErlangExit,
            OtpErlangDecodeException {
        try {
            final OtpMsg m = receiveMsg(timeout);
            if (m != null) {
                return m.getMsg();
            }
        } catch (final OtpErlangExit e) {
            throw e;
        } catch (final OtpErlangDecodeException f) {
            throw f;
        } catch (final InterruptedException g) {
        }
        return null;
    }

    /**
     * Block until a message arrives for this mailbox.
     *
     * @return a byte array representing the still-encoded body of the next
     *         message waiting in this mailbox.
     *
     * @exception OtpErlangExit
     *                if a linked {@link OtpErlangPid pid} has exited or has
     *                sent an exit signal to this mailbox.
     *
     */
    public OtpInputStream receiveBuf() throws OtpErlangExit {
        return receiveMsg().getMsgBuf();
    }

    /**
     * Wait for a message to arrive for this mailbox.
     *
     * @param timeout
     *            the time, in milliseconds, to wait for a message before
     *            returning null.
     *
     * @return a byte array representing the still-encoded body of the next
     *         message waiting in this mailbox.
     *
     * @exception OtpErlangExit
     *                if a linked {@link OtpErlangPid pid} has exited or has
     *                sent an exit signal to this mailbox.
     *
     * @exception InterruptedException
     *                if no message if the method times out before a message
     *                becomes available.
     */
    public OtpInputStream receiveBuf(final long timeout)
            throws InterruptedException, OtpErlangExit {
        final OtpMsg m = receiveMsg(timeout);
        if (m != null) {
            return m.getMsgBuf();
        }

        return null;
    }

    /**
     * Block until a message arrives for this mailbox.
     *
     * @return an {@link OtpMsg OtpMsg} containing the header information as
     *         well as the body of the next message waiting in this mailbox.
     *
     * @exception OtpErlangExit
     *                if a linked {@link OtpErlangPid pid} has exited or has
     *                sent an exit signal to this mailbox.
     *
     */
    public OtpMsg receiveMsg() throws OtpErlangExit {

        final OtpMsg m = (OtpMsg) queue.get();

        switch (m.type()) {
        case OtpMsg.exitTag:
        case OtpMsg.exit2Tag:
            try {
                final OtpErlangObject o = m.getMsg();
                throw new OtpErlangExit(o, m.getSenderPid());
            } catch (final OtpErlangDecodeException e) {
                throw new OtpErlangExit("unknown", m.getSenderPid());
            }

        default:
            return m;
        }
    }

    /**
     * Wait for a message to arrive for this mailbox.
     *
     * @param timeout
     *            the time, in milliseconds, to wait for a message.
     *
     * @return an {@link OtpMsg OtpMsg} containing the header information as
     *         well as the body of the next message waiting in this mailbox.
     *
     * @exception OtpErlangExit
     *                if a linked {@link OtpErlangPid pid} has exited or has
     *                sent an exit signal to this mailbox.
     *
     * @exception InterruptedException
     *                if no message if the method times out before a message
     *                becomes available.
     */
    public OtpMsg receiveMsg(final long timeout) throws InterruptedException,
            OtpErlangExit {
        final OtpMsg m = (OtpMsg) queue.get(timeout);

        if (m == null) {
            return null;
        }

        switch (m.type()) {
        case OtpMsg.exitTag:
        case OtpMsg.exit2Tag:
            try {
                final OtpErlangObject o = m.getMsg();
                throw new OtpErlangExit(o, m.getSenderPid());
            } catch (final OtpErlangDecodeException e) {
                throw new OtpErlangExit("unknown", m.getSenderPid());
            }

        default:
            return m;
        }
    }

    /**
     * Send a message to a remote {@link OtpErlangPid pid}, representing either
     * another {@link OtpMbox mailbox} or an Erlang process.
     *
     * @param to
     *            the {@link OtpErlangPid pid} identifying the intended
     *            recipient of the message.
     *
     * @param msg
     *            the body of the message to send.
     *
     */
    public void send(final OtpErlangPid to, final OtpErlangObject msg) {
        try {
            final String node = to.node();
            if (node.equals(home.node())) {
                home.deliver(new OtpMsg(to, (OtpErlangObject) msg.clone()));
            } else {
                final OtpCookedConnection conn = home.getConnection(node);
                if (conn == null) {
                    return;
                }
                conn.send(self, to, msg);
            }
        } catch (final Exception e) {
        }
    }

    /**
     * Send a message to a named mailbox created from the same node as this
     * mailbox.
     *
     * @param aname
     *            the registered name of recipient mailbox.
     *
     * @param msg
     *            the body of the message to send.
     *
     */
    public void send(final String aname, final OtpErlangObject msg) {
        home.deliver(new OtpMsg(self, aname, (OtpErlangObject) msg.clone()));
    }

    /**
     * Send a message to a named mailbox created from another node.
     *
     * @param aname
     *            the registered name of recipient mailbox.
     *
     * @param node
     *            the name of the remote node where the recipient mailbox is
     *            registered.
     *
     * @param msg
     *            the body of the message to send.
     *
     */
    public void send(final String aname, final String node,
            final OtpErlangObject msg) {
        try {
            final String currentNode = home.node();
            if (node.equals(currentNode)) {
                send(aname, msg);
            } else if (node.indexOf('@', 0) < 0
                    && node.equals(currentNode.substring(0,
                            currentNode.indexOf('@', 0)))) {
                send(aname, msg);
            } else {
                // other node
                final OtpCookedConnection conn = home.getConnection(node);
                if (conn == null) {
                    return;
                }
                conn.send(self, aname, msg);
            }
        } catch (final Exception e) {
        }
    }

    /**
     * Close this mailbox with the given reason.
     *
     * <p>
     * After this operation, the mailbox will no longer be able to receive
     * messages. Any delivered but as yet unretrieved messages can still be
     * retrieved however.
     * </p>
     *
     * <p>
     * If there are links from this mailbox to other {@link OtpErlangPid pids},
     * they will be broken when this method is called and exit signals will be
     * sent.
     * </p>
     *
     * @param reason
     *            an Erlang term describing the reason for the exit.
     */
    public void exit(final OtpErlangObject reason) {
        home.closeMbox(this, reason);
    }

    /**
     * Equivalent to <code>exit(new OtpErlangAtom(reason))</code>.
     *
     * @see #exit(OtpErlangObject)
     */
    public void exit(final String reason) {
        exit(new OtpErlangAtom(reason));
    }

    /**
     * <p>
     * Send an exit signal to a remote {@link OtpErlangPid pid}. This method
     * does not cause any links to be broken, except indirectly if the remote
     * {@link OtpErlangPid pid} exits as a result of this exit signal.
     * </p>
     *
     * @param to
     *            the {@link OtpErlangPid pid} to which the exit signal should
     *            be sent.
     *
     * @param reason
     *            an Erlang term indicating the reason for the exit.
     */
    // it's called exit, but it sends exit2
    public void exit(final OtpErlangPid to, final OtpErlangObject reason) {
        exit(2, to, reason);
    }

    /**
     * <p>
     * Equivalent to <code>exit(to, new
     * OtpErlangAtom(reason))</code>.
     * </p>
     *
     * @see #exit(OtpErlangPid, OtpErlangObject)
     */
    public void exit(final OtpErlangPid to, final String reason) {
        exit(to, new OtpErlangAtom(reason));
    }

    // this function used internally when "process" dies
    // since Erlang discerns between exit and exit/2.
    private void exit(final int arity, final OtpErlangPid to,
            final OtpErlangObject reason) {
        try {
            final String node = to.node();
            if (node.equals(home.node())) {
                home.deliver(new OtpMsg(OtpMsg.exitTag, self, to, reason));
            } else {
                final OtpCookedConnection conn = home.getConnection(node);
                if (conn == null) {
                    return;
                }
                switch (arity) {
                case 1:
                    conn.exit(self, to, reason);
                    break;

                case 2:
                    conn.exit2(self, to, reason);
                    break;
                }
            }
        } catch (final Exception e) {
        }
    }

    /**
     * <p>
     * Link to a remote mailbox or Erlang process. Links are idempotent, calling
     * this method multiple times will not result in more than one link being
     * created.
     * </p>
     *
     * <p>
     * If the remote process subsequently exits or the mailbox is closed, a
     * subsequent attempt to retrieve a message through this mailbox will cause
     * an {@link OtpErlangExit OtpErlangExit} exception to be raised. Similarly,
     * if the sending mailbox is closed, the linked mailbox or process will
     * receive an exit signal.
     * </p>
     *
     * <p>
     * If the remote process cannot be reached in order to set the link, the
     * exception is raised immediately.
     * </p>
     *
     * @param to
     *            the {@link OtpErlangPid pid} representing the object to link
     *            to.
     *
     * @exception OtpErlangExit
     *                if the {@link OtpErlangPid pid} referred to does not exist
     *                or could not be reached.
     *
     */
    public void link(final OtpErlangPid to) throws OtpErlangExit {
        try {
            final String node = to.node();
            if (node.equals(home.node())) {
                if (!home.deliver(new OtpMsg(OtpMsg.linkTag, self, to))) {
                    throw new OtpErlangExit("noproc", to);
                }
            } else {
                final OtpCookedConnection conn = home.getConnection(node);
                if (conn != null) {
                    conn.link(self, to);
                } else {
                    throw new OtpErlangExit("noproc", to);
                }
            }
        } catch (final OtpErlangExit e) {
            throw e;
        } catch (final Exception e) {
        }

        links.addLink(self, to);
    }

    /**
     * <p>
     * Remove a link to a remote mailbox or Erlang process. This method removes
     * a link created with {@link #link link()}. Links are idempotent; calling
     * this method once will remove all links between this mailbox and the
     * remote {@link OtpErlangPid pid}.
     * </p>
     *
     * @param to
     *            the {@link OtpErlangPid pid} representing the object to unlink
     *            from.
     *
     */
    public void unlink(final OtpErlangPid to) {
        links.removeLink(self, to);

        try {
            final String node = to.node();
            if (node.equals(home.node())) {
                home.deliver(new OtpMsg(OtpMsg.unlinkTag, self, to));
            } else {
                final OtpCookedConnection conn = home.getConnection(node);
                if (conn != null) {
                    conn.unlink(self, to);
                }
            }
        } catch (final Exception e) {
        }
    }

    /**
     * <p>
     * Create a connection to a remote node.
     * </p>
     *
     * <p>
     * Strictly speaking, this method is not necessary simply to set up a
     * connection, since connections are created automatically first time a
     * message is sent to a {@link OtpErlangPid pid} on the remote node.
     * </p>
     *
     * <p>
     * This method makes it possible to wait for a node to come up, however, or
     * check that a node is still alive.
     * </p>
     *
     * <p>
     * This method calls a method with the same name in {@link OtpNode#ping
     * Otpnode} but is provided here for convenience.
     * </p>
     *
     * @param node
     *            the name of the node to ping.
     *
     * @param timeout
     *            the time, in milliseconds, before reporting failure.
     */
    public boolean ping(final String node, final long timeout) {
        return home.ping(node, timeout);
    }

    /**
     * <p>
     * Get a list of all known registered names on the same {@link OtpNode node}
     * as this mailbox.
     * </p>
     *
     * <p>
     * This method calls a method with the same name in {@link OtpNode#getNames
     * Otpnode} but is provided here for convenience.
     * </p>
     *
     * @return an array of Strings containing all registered names on this
     *         {@link OtpNode node}.
     */
    public String[] getNames() {
        return home.getNames();
    }

    /**
     * Determine the {@link OtpErlangPid pid} corresponding to a registered name
     * on this {@link OtpNode node}.
     *
     * <p>
     * This method calls a method with the same name in {@link OtpNode#whereis
     * Otpnode} but is provided here for convenience.
     * </p>
     *
     * @return the {@link OtpErlangPid pid} corresponding to the registered
     *         name, or null if the name is not known on this node.
     */
    public OtpErlangPid whereis(final String aname) {
        return home.whereis(aname);
    }

    /**
     * Close this mailbox.
     *
     * <p>
     * After this operation, the mailbox will no longer be able to receive
     * messages. Any delivered but as yet unretrieved messages can still be
     * retrieved however.
     * </p>
     *
     * <p>
     * If there are links from this mailbox to other {@link OtpErlangPid pids},
     * they will be broken when this method is called and exit signals with
     * reason 'normal' will be sent.
     * </p>
     *
     * <p>
     * This is equivalent to {@link #exit(String) exit("normal")}.
     * </p>
     */
    public void close() {
        home.closeMbox(this);
    }

    @Override
    protected void finalize() {
        close();
        queue.flush();
    }

    /**
     * Determine if two mailboxes are equal.
     *
     * @return true if both Objects are mailboxes with the same identifying
     *         {@link OtpErlangPid pids}.
     */
    @Override
    public boolean equals(final Object o) {
        if (!(o instanceof OtpMbox)) {
            return false;
        }

        final OtpMbox m = (OtpMbox) o;
        return m.self.equals(self);
    }

    @Override
    public int hashCode() {
        return self.hashCode();
    }

    /*
     * called by OtpNode to deliver message to this mailbox.
     *
     * About exit and exit2: both cause exception to be raised upon receive().
     * However exit (not 2) causes any link to be removed as well, while exit2
     * leaves any links intact.
     */
    void deliver(final OtpMsg m) {
        switch (m.type()) {
        case OtpMsg.linkTag:
            links.addLink(self, m.getSenderPid());
            break;

        case OtpMsg.unlinkTag:
            links.removeLink(self, m.getSenderPid());
            break;

        case OtpMsg.exitTag:
            links.removeLink(self, m.getSenderPid());
            queue.put(m);
            break;

        case OtpMsg.exit2Tag:
        default:
            queue.put(m);
            break;
        }
    }

    // used to break all known links to this mbox
    void breakLinks(final OtpErlangObject reason) {
        final Link[] l = links.clearLinks();

        if (l != null) {
            final int len = l.length;

            for (int i = 0; i < len; i++) {
                exit(1, l[i].remote(), reason);
            }
        }
    }
}
