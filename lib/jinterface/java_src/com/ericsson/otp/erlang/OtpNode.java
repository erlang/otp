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
import java.lang.ref.WeakReference;
import java.util.Collection;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Iterator;

/**
 * <p>
 * Represents a local OTP node. This class is used when you do not wish to
 * manage connections yourself - outgoing connections are established as needed,
 * and incoming connections accepted automatically. This class supports the use
 * of a mailbox API for communication, while management of the underlying
 * communication mechanism is automatic and hidden from the application
 * programmer.
 * </p>
 *
 * <p>
 * Once an instance of this class has been created, obtain one or more mailboxes
 * in order to send or receive messages. The first message sent to a given node
 * will cause a connection to be set up to that node. Any messages received will
 * be delivered to the appropriate mailboxes.
 * </p>
 *
 * <p>
 * To shut down the node, call {@link #close close()}. This will prevent the
 * node from accepting additional connections and it will cause all existing
 * connections to be closed. Any unread messages in existing mailboxes can still
 * be read, however no new messages will be delivered to the mailboxes.
 * </p>
 *
 * <p>
 * Note that the use of this class requires that Epmd (Erlang Port Mapper
 * Daemon) is running on each cooperating host. This class does not start Epmd
 * automatically as Erlang does, you must start it manually or through some
 * other means. See the Erlang documentation for more information about this.
 * </p>
 */
public class OtpNode extends OtpLocalNode {
    private boolean initDone = false;

    // thread to manage incoming connections
    private Acceptor acceptor = null;

    // keep track of all connections
    Hashtable<String, OtpCookedConnection> connections = null;

    // keep track of all mailboxes
    Mailboxes mboxes = null;

    // handle status changes
    OtpNodeStatus handler;

    // flags
    private int connFlags = 0;

    /**
     * <p>
     * Create a node using the default cookie. The default cookie is found by
     * reading the first line of the .erlang.cookie file in the user's home
     * directory. The home directory is obtained from the System property
     * "user.home".
     * </p>
     *
     * <p>
     * If the file does not exist, an empty string is used. This method makes no
     * attempt to create the file.
     * </p>
     *
     * @param node
     *            the name of this node.
     *
     * @exception IOException
     *                if communication could not be initialized.
     *
     */
    public OtpNode(final String node) throws IOException {
        super(node);

        init(0);
    }

    /**
     * <p>
     * Create a node using the default cookie. The default cookie is found by
     * reading the first line of the .erlang.cookie file in the user's home
     * directory. The home directory is obtained from the System property
     * "user.home".
     * </p>
     *
     * <p>
     * If the file does not exist, an empty string is used. This method makes no
     * attempt to create the file.
     * </p>
     *
     * @param node
     *            the name of this node.
     *
     * @param transportFactory
     *            the transport factory to use when creating connections.
     *
     * @exception IOException
     *                if communication could not be initialized.
     *
     */
    public OtpNode(final String node,
            final OtpTransportFactory transportFactory) throws IOException {
        super(node, transportFactory);

        init(0);
    }

    /**
     * Create a node.
     *
     * @param node
     *            the name of this node.
     *
     * @param cookie
     *            the authorization cookie that will be used by this node when
     *            it communicates with other nodes.
     *
     * @exception IOException
     *                if communication could not be initialized.
     *
     */
    public OtpNode(final String node, final String cookie) throws IOException {
        this(node, cookie, 0);
    }

    /**
     * Create a node.
     *
     * @param node
     *            the name of this node.
     *
     * @param cookie
     *            the authorization cookie that will be used by this node when
     *            it communicates with other nodes.
     *
     * @param transportFactory
     *            the transport factory to use when creating connections.
     *
     * @exception IOException
     *                if communication could not be initialized.
     *
     */
    public OtpNode(final String node, final String cookie,
            final OtpTransportFactory transportFactory) throws IOException {
        this(node, cookie, 0, transportFactory);
    }

    /**
     * Create a node.
     *
     * @param node
     *            the name of this node.
     *
     * @param cookie
     *            the authorization cookie that will be used by this node when
     *            it communicates with other nodes.
     *
     * @param port
     *            the port number you wish to use for incoming connections.
     *            Specifying 0 lets the system choose an available port.
     *
     * @exception IOException
     *                if communication could not be initialized.
     *
     */
    public OtpNode(final String node, final String cookie, final int port)
            throws IOException {
        super(node, cookie);

        init(port);
    }

    /**
     * Create a node.
     *
     * @param node
     *            the name of this node.
     *
     * @param cookie
     *            the authorization cookie that will be used by this node when
     *            it communicates with other nodes.
     *
     * @param port
     *            the port number you wish to use for incoming connections.
     *            Specifying 0 lets the system choose an available port.
     *
     * @param transportFactory
     *            the transport factory to use when creating connections.
     *
     * @exception IOException
     *                if communication could not be initialized.
     *
     */
    public OtpNode(final String node, final String cookie, final int port,
            final OtpTransportFactory transportFactory) throws IOException {
        super(node, cookie, transportFactory);

        init(port);
    }

    private synchronized void init(final int aport) throws IOException {
        if (!initDone) {
            connections = new Hashtable<String, OtpCookedConnection>(17,
                    (float) 0.95);
            mboxes = new Mailboxes();
            acceptor = new Acceptor(aport);
            initDone = true;
        }
    }

    /**
     * Close the node. Unpublish the node from Epmd (preventing new connections)
     * and close all existing connections.
     */
    public synchronized void close() {
        acceptor.quit();
        OtpCookedConnection conn;
        final Collection<OtpCookedConnection> coll = connections.values();
        final Iterator<OtpCookedConnection> it = coll.iterator();

        mboxes.clear();

        while (it.hasNext()) {
            conn = it.next();
            it.remove();
            conn.close();
        }
        initDone = false;
    }

    @Override
    protected void finalize() {
        close();
    }

    /**
     * Create an unnamed {@link OtpMbox mailbox} that can be used to send and
     * receive messages with other, similar mailboxes and with Erlang processes.
     * Messages can be sent to this mailbox by using its associated
     * {@link OtpMbox#self() pid}.
     *
     * @return a mailbox.
     */
    public OtpMbox createMbox() {
        return mboxes.create();
    }

    /**
     * Close the specified mailbox with reason 'normal'.
     *
     * @param mbox
     *            the mailbox to close.
     *
     *            <p>
     *            After this operation, the mailbox will no longer be able to
     *            receive messages. Any delivered but as yet unretrieved
     *            messages can still be retrieved however.
     *            </p>
     *
     *            <p>
     *            If there are links from the mailbox to other
     *            {@link OtpErlangPid pids}, they will be broken when this
     *            method is called and exit signals with reason 'normal' will be
     *            sent.
     *            </p>
     *
     */
    public void closeMbox(final OtpMbox mbox) {
        closeMbox(mbox, new OtpErlangAtom("normal"));
    }

    /**
     * Close the specified mailbox with the given reason.
     *
     * @param mbox
     *            the mailbox to close.
     * @param reason
     *            an Erlang term describing the reason for the termination.
     *
     *            <p>
     *            After this operation, the mailbox will no longer be able to
     *            receive messages. Any delivered but as yet unretrieved
     *            messages can still be retrieved however.
     *            </p>
     *
     *            <p>
     *            If there are links from the mailbox to other
     *            {@link OtpErlangPid pids}, they will be broken when this
     *            method is called and exit signals with the given reason will
     *            be sent.
     *            </p>
     *
     */
    public void closeMbox(final OtpMbox mbox, final OtpErlangObject reason) {
        if (mbox != null) {
            mboxes.remove(mbox);
            mbox.name = null;
            mbox.breakLinks(reason);
        }
    }

    /**
     * Create an named mailbox that can be used to send and receive messages
     * with other, similar mailboxes and with Erlang processes. Messages can be
     * sent to this mailbox by using its registered name or the associated
     * {@link OtpMbox#self() pid}.
     *
     * @param name
     *            a name to register for this mailbox. The name must be unique
     *            within this OtpNode.
     *
     * @return a mailbox, or null if the name was already in use.
     *
     */
    public OtpMbox createMbox(final String name) {
        return mboxes.create(name);
    }

    /**
     * <p>
     * Register or remove a name for the given mailbox. Registering a name for a
     * mailbox enables others to send messages without knowing the
     * {@link OtpErlangPid pid} of the mailbox. A mailbox can have at most one
     * name; if the mailbox already had a name, calling this method will
     * supercede that name.
     * </p>
     *
     * @param name
     *            the name to register for the mailbox. Specify null to
     *            unregister the existing name from this mailbox.
     *
     * @param mbox
     *            the mailbox to associate with the name.
     *
     * @return true if the name was available, or false otherwise.
     */
    public boolean registerName(final String name, final OtpMbox mbox) {
        return mboxes.register(name, mbox);
    }

    /**
     * Get a list of all known registered names on this node.
     *
     * @return an array of Strings, containins all known registered names on
     *         this node.
     */

    public String[] getNames() {
        return mboxes.names();
    }

    /**
     * Determine the {@link OtpErlangPid pid} corresponding to a registered name
     * on this node.
     *
     * @return the {@link OtpErlangPid pid} corresponding to the registered
     *         name, or null if the name is not known on this node.
     */
    public OtpErlangPid whereis(final String name) {
        final OtpMbox m = mboxes.get(name);
        if (m != null) {
            return m.self();
        }
        return null;
    }

    /**
     * Register interest in certain system events. The {@link OtpNodeStatus
     * OtpNodeStatus} handler object contains callback methods, that will be
     * called when certain events occur.
     *
     * @param ahandler
     *            the callback object to register. To clear the handler, specify
     *            null as the handler to use.
     *
     */
    public synchronized void registerStatusHandler(final OtpNodeStatus ahandler) {
        handler = ahandler;
    }

    /**
     * <p>
     * Determine if another node is alive. This method has the side effect of
     * setting up a connection to the remote node (if possible). Only a single
     * outgoing message is sent; the timeout is how long to wait for a response.
     * </p>
     *
     * <p>
     * Only a single attempt is made to connect to the remote node, so for
     * example it is not possible to specify an extremely long timeout and
     * expect to be notified when the node eventually comes up. If you wish to
     * wait for a remote node to be started, the following construction may be
     * useful:
     * </p>
     *
     * <pre>
     * // ping every 2 seconds until positive response
     * while (!me.ping(him, 2000))
     *     ;
     * </pre>
     *
     * @param anode
     *            the name of the node to ping.
     *
     * @param timeout
     *            the time, in milliseconds, to wait for response before
     *            returning false.
     *
     * @return true if the node was alive and the correct ping response was
     *         returned. false if the correct response was not returned on time.
     */
    /*
     * internal info about the message formats...
     *
     * the request: -> REG_SEND {6,#Pid<bingo@aule.1.0>,'',net_kernel}
     * {'$gen_call',{#Pid<bingo@aule.1.0>,#Ref<bingo@aule.2>},{is_auth,bingo@aule}}
     *
     * the reply: <- SEND {2,'',#Pid<bingo@aule.1.0>} {#Ref<bingo@aule.2>,yes}
     */
    public boolean ping(final String anode, final long timeout) {
        if (anode.equals(node)) {
            return true;
        } else if (anode.indexOf('@', 0) < 0
                && anode.equals(node.substring(0, node.indexOf('@', 0)))) {
            return true;
        }

        // other node
        OtpMbox mbox = null;
        try {
            mbox = createMbox();
            mbox.send("net_kernel", anode, getPingTuple(mbox));
            final OtpErlangObject reply = mbox.receive(timeout);

            final OtpErlangTuple t = (OtpErlangTuple) reply;
            final OtpErlangAtom a = (OtpErlangAtom) t.elementAt(1);
            return "yes".equals(a.atomValue());
        } catch (final Exception e) {
        } finally {
            closeMbox(mbox);
        }
        return false;
    }

    /* create the outgoing ping message */
    private OtpErlangTuple getPingTuple(final OtpMbox mbox) {
        final OtpErlangObject[] ping = new OtpErlangObject[3];
        final OtpErlangObject[] pid = new OtpErlangObject[2];
        final OtpErlangObject[] anode = new OtpErlangObject[2];

        pid[0] = mbox.self();
        pid[1] = createRef();

        anode[0] = new OtpErlangAtom("is_auth");
        anode[1] = new OtpErlangAtom(node());

        ping[0] = new OtpErlangAtom("$gen_call");
        ping[1] = new OtpErlangTuple(pid);
        ping[2] = new OtpErlangTuple(anode);

        return new OtpErlangTuple(ping);
    }

    /*
     * this method simulates net_kernel only for the purpose of replying to
     * pings.
     */
    private boolean netKernel(final OtpMsg m) {
        OtpMbox mbox = null;
        try {
            final OtpErlangTuple t = (OtpErlangTuple) m.getMsg();
            final OtpErlangTuple req = (OtpErlangTuple) t.elementAt(1); // actual
            // request

            final OtpErlangPid pid = (OtpErlangPid) req.elementAt(0); // originating
            // pid

            final OtpErlangObject[] pong = new OtpErlangObject[2];
            pong[0] = req.elementAt(1); // his #Ref
            pong[1] = new OtpErlangAtom("yes");

            mbox = createMbox();
            mbox.send(pid, new OtpErlangTuple(pong));
            return true;
        } catch (final Exception e) {
        } finally {
            closeMbox(mbox);
        }
        return false;
    }

    /*
     * OtpCookedConnection delivers messages here return true if message was
     * delivered successfully, or false otherwise.
     */
    boolean deliver(final OtpMsg m) {
        OtpMbox mbox = null;

        try {
            final int t = m.type();

            if (t == OtpMsg.regSendTag) {
                final String name = m.getRecipientName();
                /* special case for netKernel requests */
                if (name.equals("net_kernel")) {
                    return netKernel(m);
                }
                mbox = mboxes.get(name);
            } else {
                mbox = mboxes.get(m.getRecipientPid());
            }

            if (mbox == null) {
                return false;
            }
            mbox.deliver(m);
        } catch (final Exception e) {
            return false;
        }

        return true;
    }

    /*
     * OtpCookedConnection delivers errors here, we send them on to the handler
     * specified by the application
     */
    void deliverError(final OtpCookedConnection conn, final Exception e) {
        removeConnection(conn);
        remoteStatus(conn.name, false, e);
    }

    /*
     * find or create a connection to the given node
     */
    OtpCookedConnection getConnection(final String anode) {
        OtpPeer peer = null;
        OtpCookedConnection conn = null;

        synchronized (connections) {
            // first just try looking up the name as-is
            conn = connections.get(anode);

            if (conn == null) {
                // in case node had no '@' add localhost info and try again
                peer = new OtpPeer(anode);
                conn = connections.get(peer.node());

                if (conn == null) {
                    try {
                        conn = new OtpCookedConnection(this, peer);
                        conn.setFlags(connFlags);
                        addConnection(conn);
                    } catch (final Exception e) {
                        /* false = outgoing */
                        connAttempt(peer.node(), false, e);
                    }
                }
            }
            return conn;
        }
    }

    void addConnection(final OtpCookedConnection conn) {
        if (conn != null && conn.name != null) {
            connections.put(conn.name, conn);
            remoteStatus(conn.name, true, null);
        }
    }

    private void removeConnection(final OtpCookedConnection conn) {
        if (conn != null && conn.name != null) {
            connections.remove(conn.name);
        }
    }

    /* use these wrappers to call handler functions */
    private synchronized void remoteStatus(final String anode,
            final boolean up, final Object info) {
        if (handler == null) {
            return;
        }
        try {
            handler.remoteStatus(anode, up, info);
        } catch (final Exception e) {
        }
    }

    synchronized void localStatus(final String anode, final boolean up,
            final Object info) {
        if (handler == null) {
            return;
        }
        try {
            handler.localStatus(anode, up, info);
        } catch (final Exception e) {
        }
    }

    synchronized void connAttempt(final String anode, final boolean incoming,
            final Object info) {
        if (handler == null) {
            return;
        }
        try {
            handler.connAttempt(anode, incoming, info);
        } catch (final Exception e) {
        }
    }

    /*
     * this class used to wrap the mailbox hashtables so we can use weak
     * references
     */
    public class Mailboxes {
        // mbox pids here
        private Hashtable<OtpErlangPid, WeakReference<OtpMbox>> byPid = null;
        // mbox names here
        private Hashtable<String, WeakReference<OtpMbox>> byName = null;

        public Mailboxes() {
            byPid = new Hashtable<OtpErlangPid, WeakReference<OtpMbox>>(17,
                    (float) 0.95);
            byName = new Hashtable<String, WeakReference<OtpMbox>>(17,
                    (float) 0.95);
        }

        public OtpMbox create(final String name) {
            OtpMbox m = null;

            synchronized (byName) {
                if (get(name) != null) {
                    return null;
                }
                final OtpErlangPid pid = createPid();
                m = new OtpMbox(OtpNode.this, pid, name);
                byPid.put(pid, new WeakReference<OtpMbox>(m));
                byName.put(name, new WeakReference<OtpMbox>(m));
            }
            return m;
        }

        public OtpMbox create() {
            final OtpErlangPid pid = createPid();
            final OtpMbox m = new OtpMbox(OtpNode.this, pid);
            byPid.put(pid, new WeakReference<OtpMbox>(m));
            return m;
        }

        public void clear() {
            byPid.clear();
            byName.clear();
        }

        public String[] names() {
            String allnames[] = null;

            synchronized (byName) {
                final int n = byName.size();
                final Enumeration<String> keys = byName.keys();
                allnames = new String[n];

                int i = 0;
                while (keys.hasMoreElements()) {
                    allnames[i++] = keys.nextElement();
                }
            }
            return allnames;
        }

        public boolean register(final String name, final OtpMbox mbox) {
            if (name == null) {
                if (mbox.name != null) {
                    byName.remove(mbox.name);
                    mbox.name = null;
                }
            } else {
                synchronized (byName) {
                    if (get(name) != null) {
                        return false;
                    }
                    byName.put(name, new WeakReference<OtpMbox>(mbox));
                    mbox.name = name;
                }
            }
            return true;
        }

        /*
         * look up a mailbox based on its name. If the mailbox has gone out of
         * scope we also remove the reference from the hashtable so we don't
         * find it again.
         */
        public OtpMbox get(final String name) {
            final WeakReference<OtpMbox> wr = byName.get(name);

            if (wr != null) {
                final OtpMbox m = wr.get();

                if (m != null) {
                    return m;
                }
                byName.remove(name);
            }
            return null;
        }

        /*
         * look up a mailbox based on its pid. If the mailbox has gone out of
         * scope we also remove the reference from the hashtable so we don't
         * find it again.
         */
        public OtpMbox get(final OtpErlangPid pid) {
            final WeakReference<OtpMbox> wr = byPid.get(pid);

            if (wr != null) {
                final OtpMbox m = wr.get();

                if (m != null) {
                    return m;
                }
                byPid.remove(pid);
            }
            return null;
        }

        public void remove(final OtpMbox mbox) {
            byPid.remove(mbox.self);
            if (mbox.name != null) {
                byName.remove(mbox.name);
            }
        }
    }

    /*
     * this thread simply listens for incoming connections
     */
    public class Acceptor extends Thread {
        private final OtpServerTransport sock;
        private final int acceptorPort;
        private volatile boolean done = false;

        Acceptor(final int port) throws IOException {
            sock = createServerTransport(port);
            acceptorPort = sock.getLocalPort();
            OtpNode.this.port = acceptorPort;

            setDaemon(true);
            setName("acceptor");
            publishPort();
            start();
        }

        private boolean publishPort() throws IOException {
            if (getEpmd() != null) {
                return false; // already published
            }
            OtpEpmd.publishPort(OtpNode.this);
            return true;
        }

        private void unPublishPort() {
            // unregister with epmd
            OtpEpmd.unPublishPort(OtpNode.this);

            // close the local descriptor (if we have one)
            closeSock(epmd);
            epmd = null;
        }

        public void quit() {
            unPublishPort();
            done = true;
            closeSock(sock);
            localStatus(node, false, null);
        }

        private void closeSock(final OtpServerTransport s) {
            try {
                if (s != null) {
                    s.close();
                }
            } catch (final Exception e) {
            }
        }

        private void closeSock(final OtpTransport s) {
            try {
                if (s != null) {
                    s.close();
                }
            } catch (final Exception e) {
            }
        }

        public int port() {
            return acceptorPort;
        }

        @Override
        public void run() {
            OtpTransport newsock = null;
            OtpCookedConnection conn = null;

            localStatus(node, true, null);

            accept_loop: while (!done) {
                conn = null;

                try {
                    newsock = sock.accept();
                } catch (final Exception e) {
                    // Problem in java1.2.2: accept throws SocketException
                    // when socket is closed. This will happen when
                    // acceptor.quit()
                    // is called. acceptor.quit() will call localStatus(...), so
                    // we have to check if that's where we come from.
                    if (!done) {
                        localStatus(node, false, e);
                    }
                    break accept_loop;
                }

                try {
                    synchronized (connections) {
                        conn = new OtpCookedConnection(OtpNode.this, newsock);
                        conn.setFlags(connFlags);
                        addConnection(conn);
                    }
                } catch (final OtpAuthException e) {
                    if (conn != null && conn.name != null) {
                        connAttempt(conn.name, true, e);
                    } else {
                        connAttempt("unknown", true, e);
                    }
                    closeSock(newsock);
                } catch (final IOException e) {
                    if (conn != null && conn.name != null) {
                        connAttempt(conn.name, true, e);
                    } else {
                        connAttempt("unknown", true, e);
                    }
                    closeSock(newsock);
                } catch (final Exception e) {
                    closeSock(newsock);
                    closeSock(sock);
                    localStatus(node, false, e);
                    break accept_loop;
                }
            } // while

            // if we have exited loop we must do this too
            unPublishPort();
        }
    }

    public void setFlags(final int flags) {
        connFlags = flags;
    }
}
