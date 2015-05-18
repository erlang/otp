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

import java.io.IOException;
import java.net.InetAddress;

import com.ericsson.otp.erlang.OtpSelf;
import com.ericsson.otp.erlang.OtpServerTransport;
import com.ericsson.otp.erlang.OtpSocketTransportFactory;
import com.ericsson.otp.erlang.OtpTransport;
import com.ericsson.otp.erlang.OtpTransportFactory;

/**
 * @author Dmitriy Kargapolov
 */
public class TransportFactoryTest {

    /**
     * example of custom transport factory wrapping default one
     */
    public static class TransportFactory implements OtpTransportFactory {

        OtpSocketTransportFactory tf = new OtpSocketTransportFactory();

        public OtpTransport createTransport(final String addr, final int port)
                throws IOException {
            clientOk = true;
            System.out.println("creating transport to " + addr + ", " + port);
            return tf.createTransport(addr, port);
        }

        public OtpTransport createTransport(final InetAddress addr,
                final int port) throws IOException {
            clientOk = true;
            System.out.println("creating transport to " + addr + ", " + port);
            return tf.createTransport(addr, port);
        }

        public OtpServerTransport createServerTransport(final int port)
                throws IOException {
            serverOk = true;
            System.out.println("creating server transport to " + port);
            return tf.createServerTransport(port);
        }

    }

    static boolean serverOk = false;
    static boolean clientOk = false;

    public static void main(final String[] args) throws IOException {

        // check server transport
        final OtpSelf self = new OtpSelf("local", new TransportFactory());
        if (!serverOk) {
            fail("custom server transport was not created");
        }
        System.out.println("accepting connections on " + self.port());

        // check client transport
        try {
            self.publishPort();
        } catch (final Exception e) {
        }
        if (!clientOk) {
            fail("custom client transport was not created");
        }
    }

    private static void fail(final String string) {
        System.err.println(string);
        System.exit(1);
    }
}
