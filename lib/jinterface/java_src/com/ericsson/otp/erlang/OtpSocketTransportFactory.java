/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2015. All Rights Reserved.
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
