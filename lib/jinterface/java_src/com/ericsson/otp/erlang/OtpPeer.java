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
import java.net.UnknownHostException;

/**
 * Represents a remote OTP node. It acts only as a container for the nodename
 * and other node-specific information that is needed by the
 * {@link OtpConnection} class.
 */
public class OtpPeer extends AbstractNode {
    int distChoose = 0; /*
                         * this is set by OtpConnection and is the highest
                         * common protocol version we both support
                         */

    OtpPeer(final OtpTransportFactory transportFactory) {
        super(transportFactory);
    }

    /**
     * Create a peer node.
     *
     * @param node
     *            the name of the node.
     */
    public OtpPeer(final String node) {
        super(node);
    }

    /**
     * Create a peer node with custom transport factory.
     *
     * @param node
     *            the name of the node.
     * @param transportFactory
     *            custom transport factory
     */
    public OtpPeer(final String node, final OtpTransportFactory
            transportFactory) {
        super(node, transportFactory);
    }

    /**
     * Create a connection to a remote node.
     *
     * @param self
     *            the local node from which you wish to connect.
     *
     * @return a connection to the remote node.
     *
     * @exception java.net.UnknownHostException
     *                if the remote host could not be found.
     *
     * @exception java.io.IOException
     *                if it was not possible to connect to the remote node.
     *
     * @exception OtpAuthException
     *                if the connection was refused by the remote node.
     *
     * @deprecated Use the corresponding method in {@link OtpSelf} instead.
     */
    @Deprecated
    public OtpConnection connect(final OtpSelf self) throws IOException,
            UnknownHostException, OtpAuthException {
        return new OtpConnection(self, this);
    }

    // package
    /*
     * Get the port number used by the remote node.
     *
     * @return the port number used by the remote node, or 0 if the node was not
     * registered with the port mapper.
     *
     * @exception java.io.IOException if the port mapper could not be contacted.
     */
    int port() throws IOException {
        return OtpEpmd.lookupPort(this);
    }
}
