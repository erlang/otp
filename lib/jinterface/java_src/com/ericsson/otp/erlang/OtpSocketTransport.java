/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2015. All Rights Reserved.
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

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.Socket;
import java.net.UnknownHostException;

/**
 * Default socket-based client transport
 * 
 * @author Dmitriy Kargapolov
 */
public class OtpSocketTransport implements OtpTransport {

    /**
     * Underlying socket
     */
    private final Socket socket;

    /**
     * @see Socket#Socket(String, int)
     */
    public OtpSocketTransport(final String addr, final int port)
            throws UnknownHostException, IOException {
        socket = new Socket(addr, port);
        socket.setTcpNoDelay(true);
    }

    /**
     * @see Socket#Socket(InetAddress, int)
     */
    public OtpSocketTransport(final InetAddress addr, final int port)
            throws UnknownHostException, IOException {
        socket = new Socket(addr, port);
        socket.setTcpNoDelay(true);
    }

    /**
     * Socket wrapping constructor
     * 
     * @param s
     *            socket to wrap
     */
    public OtpSocketTransport(final Socket s) {
        socket = s;
    }

    /**
     * @see Socket#getInputStream()
     */
    public InputStream getInputStream() throws IOException {
        return socket.getInputStream();
    }

    /**
     * @see Socket#getOutputStream()
     */
    public OutputStream getOutputStream() throws IOException {
        return socket.getOutputStream();
    }

    /**
     * @see Socket#close()
     */
    public void close() throws IOException {
        socket.close();
    }
}
