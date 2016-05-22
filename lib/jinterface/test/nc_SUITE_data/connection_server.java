/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2004-2016. All Rights Reserved.
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

import com.ericsson.otp.erlang.*;

public class connection_server {
    static java.lang.String Name = "connection_server";

    public static void main(String[] argv) {
	try {
	    System.out.println("connection_server booting...");

	    for (int j = 0; j < argv.length; j++)
		System.out.println("argv[" + j + "] = \"" + argv[j] + "\"");

	    if (argv.length != 4) {
		System.out.println("Wrong number of arguments!");
		System.exit(1);
	    }

	    // Start node and mbox
	    OtpNode node = new OtpNode(argv[0], argv[1]);
	    OtpMbox mbox = node.createMbox();
	    if (! mbox.registerName(Name)) {
		System.out.println("Could not register name " + Name);
		System.exit(3);
	    }

	    // Announce our presence
	    OtpErlangObject[] amsg = new OtpErlangObject[3];
	    amsg[0] = new OtpErlangAtom(Name);
	    amsg[1] = new OtpErlangAtom(argv[0]);
	    amsg[2] = mbox.self();
	    OtpErlangTuple atuple = new OtpErlangTuple(amsg);
	    mbox.send(argv[3], argv[2], atuple);

	    // Do connects ...
	    while (true) {
		OtpErlangObject o = mbox.receive();
		if (o == null)
		    continue;
		if (o instanceof OtpErlangTuple) {
		    OtpErlangTuple msg = (OtpErlangTuple) o;
		    OtpErlangPid from = (OtpErlangPid)(msg.elementAt(0));
		    OtpErlangAtom conn_node = (OtpErlangAtom) msg.elementAt(1);

		    System.out.println("Got request to connect to: "
				       + conn_node);
		    OtpErlangObject[] rmsg = new OtpErlangObject[3];
		    rmsg[0] = mbox.self();
		    rmsg[1] = conn_node;
		    if (node.ping(conn_node.atomValue(), 1000)) {
			System.out.println("Successfully connected to "
					   + conn_node.toString());
			rmsg[2] = new OtpErlangAtom("true");
		    }
		    else {
			System.out.println("Failed to connect to "
					   + conn_node.toString());
			rmsg[2] = new OtpErlangAtom("false");
		    }

		    OtpErlangTuple rtuple = new OtpErlangTuple(rmsg);

		    mbox.send(from, rtuple);
		}
		else { // probably 'bye'
		    System.out.println("connection_server halting...");
		    System.exit(0);
		}
	    }
	}
	catch (Exception e) {
	    System.out.println("" + e);
	    System.exit(2);
	}

    }

}
