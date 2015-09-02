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
