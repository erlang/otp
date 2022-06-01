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

import java.io.IOException;

import com.ericsson.otp.erlang.OtpGenericTransportFactory;
import com.ericsson.otp.erlang.OtpLocalNode;
import com.ericsson.otp.erlang.OtpNode;
import com.ericsson.otp.erlang.OtpPeer;
import com.ericsson.otp.erlang.OtpServerTransport;
import com.ericsson.otp.erlang.OtpSocketTransportFactory;
import com.ericsson.otp.erlang.OtpTransport;
import com.ericsson.otp.erlang.OtpTransportFactory;

public class GenericTransportFactoryTest {

    static boolean serverOk = false;

    /**
     * A minimalist custom generic transport factory working without epmd,
     * using the node name as the identifier of the listening socket port.
     */
    public static class TestFactory extends OtpGenericTransportFactory {

        OtpSocketTransportFactory tf = new OtpSocketTransportFactory();

        public OtpTransport createTransport(final OtpPeer peer)
                throws IOException {
            String addr = "localhost";
            String peerName = peer.alive();
            int port = 0;
            try {
                port = Integer.parseInt(peerName);
            } catch (NumberFormatException e) {
            }
            System.out.println("Creating transport to " + addr + ", " + port);
            return tf.createTransport(addr, port);
        }

        public OtpServerTransport createServerTransport(final OtpLocalNode node)
                throws IOException {
            String nodeName = node.alive();
            int port = 0;
            try {
                port = Integer.parseInt(nodeName);
                serverOk = true;
            } catch (NumberFormatException e) {
                serverOk = false;
            }
            System.out.println("Creating server transport to " + port);
            return tf.createServerTransport(port);
        }

    }

    public static void main(final String[] args) throws IOException {

        OtpTransportFactory customFactory = new TestFactory();

        // The 2 test nodes with arbitrary socket port numbers
        String nodeName1 = "65432";
        String nodeName2 = "54321";

        // Create the first node
        final OtpNode node1 = new OtpNode(nodeName1, customFactory);
        if (!serverOk) {
            fail("Custom server transport 1 was not created");
        }
        System.out.println("Node 1 accepting connections on " + node1.port());

        // Create the second node
        final OtpNode node2 = new OtpNode(nodeName2, customFactory);
        if (!serverOk) {
            fail("Custom server transport 2 was not created");
        }
        System.out.println("Node 2 accepting connections on " + node2.port());

        // Finally test the connection between the 2 nodes
        if (!node1.ping(nodeName2, 2000)) {
            fail("The 2 nodes couldn't connect");
        }

    }

    private static void fail(final String string) {
        System.err.println(string);
        System.exit(1);
    }
}
