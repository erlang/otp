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

    /**
     * A minimalist custom generic transport factory working without epmd,
     * using the node name as the identifier of the listening socket port.
     */
    public static class TestFactory extends OtpGenericTransportFactory {

        OtpSocketTransportFactory tf = new OtpSocketTransportFactory();
		String fake_epmd_name;
		int fake_epmd_port;

        public OtpTransport createTransport(final OtpPeer peer)
                throws IOException {
            String addr = "localhost";
            String peerName = peer.alive();
            int port = 0;
			if (peerName == fake_epmd_name) {
				port = fake_epmd_port;
			}
			else {
				fail("Fake epmd don't know port nr of " + peerName);
			}
			System.out.println("Creating transport to " + addr + ", " + port);
            return tf.createTransport(addr, port);
        }

        public OtpServerTransport createServerTransport(final OtpLocalNode node)
                throws IOException {
			  String nodeName = node.alive();
			  OtpServerTransport transport = tf.createServerTransport(0);
			  System.out.println("Creating server transport for node "
								 + nodeName + " to port "
								 + transport.getLocalPort());

			  // Store last created server node in our fake "epmd database".
			  fake_epmd_name = nodeName;
			  fake_epmd_port = transport.getLocalPort();

			  return transport;
        }

    }

    public static void main(final String[] args) throws IOException {

        OtpTransportFactory customFactory = new TestFactory();

        // The 2 test nodes with arbitrary socket port numbers
        String nodeName1 = "Alice";
        String nodeName2 = "Bob";

        // Create the first node
        final OtpNode node1 = new OtpNode(nodeName1, customFactory);

        // Create the second node
        final OtpNode node2 = new OtpNode(nodeName2, customFactory);

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
