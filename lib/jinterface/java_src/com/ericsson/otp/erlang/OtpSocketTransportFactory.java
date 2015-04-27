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
import java.net.InetAddress;

/**
 * Default socket-based transport factory
 * 
 * @author Dmitriy Kargapolov
 */
public class OtpSocketTransportFactory implements OtpTransportFactory {

    /**
     * @see OtpTransportFactory#createTransport(String, int)
     */
    public OtpTransport createTransport(final String addr, final int port)
            throws IOException {
        return new OtpSocketTransport(addr, port);
    }

    /**
     * @see OtpTransportFactory#createTransport(InetAddress, int)
     */
    public OtpTransport createTransport(final InetAddress addr, final int port)
            throws IOException {
        return new OtpSocketTransport(addr, port);
    }

    /**
     * @see OtpTransportFactory#createServerTransport(int)
     */
    public OtpServerTransport createServerTransport(final int port)
            throws IOException {
        return new OtpServerSocketTransport(port);
    }

}
