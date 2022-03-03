/*
 * Copyright 2022 Jérôme de Bretagne
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
 * %ExternalCopyright%
 */

package com.ericsson.otp.erlang;

import java.io.IOException;
import java.net.InetAddress;

/**
 * Transport factory abstract class used to create client-side and server-side
 * transport instances defined using generic peers and local nodes (instead of
 * a host + port combination as expected in the base OtpTransportFactory).
 *
 * It allows the creation of a transport using Unix Domain Sockets for example.
 *
 * OtpGenericTransportFactory is created as a subclass of OtpTransportFactory
 * to keep backwards compatibility and ease the integration within existing
 * Jinterface code, but in practice it doesn't support the 3 original methods.
 */
public abstract class OtpGenericTransportFactory implements OtpTransportFactory {

    /**
     * Create an instance of a client-side {@link OtpTransport}
     *
     * @param peer
     *            the peer identifying the server to connect to
     *
     * @return a new transport object
     *
     * @throws IOException
     */
    public abstract OtpTransport createTransport(final OtpPeer peer)
            throws IOException;

    /**
     * Create an instance of a server-side {@link OtpServerTransport}
     *
     * @param node
     *            the local node identifying the transport to create server-side
     *
     * @return a new transport object
     *
     * @throws IOException
     */
    public abstract OtpServerTransport createServerTransport(final OtpLocalNode node)
            throws IOException;


    /**
     * Implement the 3 original methods by throwing an exception as the usage
     * of a port is not supported by this subclass of OtpTransportFactory.
     */
    @Override public OtpTransport createTransport(String addr, int port)
            throws IOException {
        throw new IOException("Method createTransport(String, int) " +
                              "not applicable for OtpGenericTransportFactory");
    }

    @Override public OtpTransport createTransport(final InetAddress addr, final int port)
            throws IOException {
        throw new IOException("Method createTransport(InetAddress, int) " +
                              "not applicable for OtpGenericTransportFactory");
    }

    @Override public OtpServerTransport createServerTransport(final int port)
            throws IOException {
        throw new IOException("Method createServerTransport(int) " +
                              "not applicable for OtpGenericTransportFactory");
    }

}

