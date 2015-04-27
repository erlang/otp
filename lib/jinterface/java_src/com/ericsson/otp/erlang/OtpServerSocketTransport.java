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
import java.net.ServerSocket;
import java.net.Socket;

/**
 * Default socket-based server transport
 * 
 * @author Dmitriy Kargapolov
 */
public class OtpServerSocketTransport implements OtpServerTransport {

    /**
     * Underlying server socket
     */
    private final ServerSocket socket;

    /**
     * @see ServerSocket#ServerSocket(int)
     */
    public OtpServerSocketTransport(final int port) throws IOException {
        socket = new ServerSocket(port);
    }

    /**
     * @see ServerSocket#getLocalPort()
     */
    public int getLocalPort() {
        return socket.getLocalPort();
    }

    /**
     * @see ServerSocket#accept()
     */
    public OtpTransport accept() throws IOException {
        final Socket sock = socket.accept();
        sock.setTcpNoDelay(true);
        return new OtpSocketTransport(sock);
    }

    /**
     * @see ServerSocket#close()
     */
    public void close() throws IOException {
        socket.close();
    }

}
