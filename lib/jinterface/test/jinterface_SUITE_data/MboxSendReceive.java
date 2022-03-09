/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2004-2022. All Rights Reserved.
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

import java.util.Arrays;
import com.ericsson.otp.erlang.*;

class MboxSendReceive {

    /*
      Implements test case jinterface_SUITE:mbox_send_receive/1

      Test OtpMbox.send(...) and OtpMbox.receive(...)
     */

    private static final boolean dbg = true;
    private static final int recTime = 2000;

    private static final int java_erlang_send_receive = 1;
    private static final int java_internal_send_receive_same_node = 2;
    private static final int java_internal_send_receive_different_nodes = 3;
    private static final int java_internal_send_receive_self = 4;

    @SuppressWarnings("null")
    public static void main(String argv[]) {

	String cookie = argv[0];
	String erlNode = argv[1];

	OtpErlangObject[] msgArray = new OtpErlangObject[2];
	msgArray[1] = new OtpErlangAtom("hello world");
	OtpErlangTuple msg = null;

	try {
	    // Initiate: create javanode and mboxes
	    OtpNode node = new OtpNode("javanode",cookie);
	    OtpMbox mbox = node.createMbox();
	    OtpMbox mbox2 = node.createMbox("java_echo_server2");

	    // Send the pid of mbox to erlang and wait for test case
	    // instruction: {TestCaseTag, Pid}
	    mbox.send("erl_send_receive_server", erlNode, mbox.self());
	    OtpErlangObject obj = mbox.receive(recTime);
	    if (obj == null) System.exit(1);
	    OtpErlangTuple testCase = (OtpErlangTuple)obj;
	    dbg("mbox received " + testCase);
	    int tag = (int)((OtpErlangLong)testCase.elementAt(0)).longValue();
	    OtpErlangPid erlangPid = (OtpErlangPid)testCase.elementAt(1);

	    switch (tag) {

	    case java_erlang_send_receive:

		// Test1 (happened during initiation):
		// Send mbox pid to erlang process with registered name.
		// Erlang process sent back its pid to the mbox pid.

		// Test2: Register name and sent it to the erlang pid. Erlang
		// process shall send message back to my registered name.

		mbox.registerName("java_echo_server");
		msgArray[0] = getNameNode("java_echo_server",node);
		msg = new OtpErlangTuple(msgArray);

		dbg("java_echo_server sending " + msg);
		mbox.send(erlangPid,msg);

		obj = mbox.receive(recTime);
		dbg("java_echo_server received " + obj);
		if (obj == null) System.exit(2);
		if (!obj.equals(msgArray[1])) System.exit(3);

		// Test3: Same as Test2, but using a new mbox2 which
		// got its name already when it is created - i.e. not
		// using mbox.registerName
		msgArray[0] = getNameNode("java_echo_server2",node);
		msg = new OtpErlangTuple(msgArray);

		dbg("java_echo_server2 sending " + msg);
		mbox2.send(erlangPid,msg);

		obj = mbox2.receive(recTime);
		dbg("java_echo_server received " + obj);
		if (obj == null) System.exit(4);
		if (!obj.equals(msgArray[1])) System.exit(5);

                // Test4: Test all term types
                byte[] bytes = {1,2,3};
                int[] ints = {11,22,33};
                OtpErlangObject[] elements = {
                    new OtpErlangAtom("atom"),
                    new OtpErlangString("string"),
                    new OtpErlangLong(17),
                };
                OtpErlangObject[] terms = {
                    new OtpErlangBitstr(bytes, 5),
                    new OtpErlangDouble(3.141592),
                    new OtpErlangExternalFun("lists", "length", 1),
                    //new OtpErlangFun(...),
                    new OtpErlangList(elements),
                    new OtpErlangLong(-1742),
                    new OtpErlangMap(Arrays.copyOfRange(elements,0,elements.length-1),
                                     Arrays.copyOfRange(elements,1,elements.length)),
                    new OtpErlangPid(node.node(), 1372, 1742, 98765),
                    new OtpErlangPort(node.node(), 1372, 87654),
                    new OtpErlangRef(node.node(), ints, 76543),
                    new OtpErlangString("This is an OtpErlangString"),
                    new OtpErlangTuple(elements),
                };
                for (int i=0; i < terms.length; i++) {
                    msgArray[1] = terms[i];
                    msg = new OtpErlangTuple(msgArray);

                    dbg("java_echo_server2 sending " + msg);
                    mbox2.send(erlangPid,msg);

                    obj = mbox2.receive(recTime);
                    dbg("java_echo_server received " + obj);
                    if (obj == null) System.exit(4);
                    if (!obj.equals(msgArray[1])) System.exit(5);
                }


		break;

	    case java_internal_send_receive_same_node:

		// Test1: Sending message between mboxes on same node
		// given registered name and node without host.
		mbox.send("java_echo_server2","javanode",msgArray[1]);
		obj = mbox2.receive(recTime);
		dbg("Mbox at same node: " + obj);
		if (obj == null) System.exit(6);
		if(!((OtpErlangAtom)obj).equals(msgArray[1])) System.exit(7);

		// Test2: Sending message between mboxes on same node
		// given registered name and node with host.
		mbox.send("java_echo_server2",mbox2.self().node(),msgArray[1]);
		obj = mbox2.receive(recTime);
		dbg("Mbox at same node: " + obj);
		if (obj == null) System.exit(8);
		if(!((OtpErlangAtom)obj).equals(msgArray[1])) System.exit(9);

		// Test3: Sending message between mboxes on same node
		// given registered name but not node.
		mbox.send("java_echo_server2",msgArray[1]);
		obj = mbox2.receive(recTime);
		dbg("Mbox at same node: " + obj);
		if (obj == null) System.exit(10);
		if(!((OtpErlangAtom)obj).equals(msgArray[1])) System.exit(11);

		// Test4: Sending message between mboxes on same node
		// given pid.
		mbox.send(mbox2.self(),msgArray[1]);
		obj = mbox2.receive(recTime);
		dbg("Mbox at same node: " + obj);
		if (obj == null) System.exit(12);
		if(!((OtpErlangAtom)obj).equals(msgArray[1])) System.exit(13);

		break;

	    case java_internal_send_receive_different_nodes:

		OtpNode node2 = new OtpNode("javanode2", cookie);
		OtpMbox mboxOtherNode = node2.createMbox("mboxOtherNode");

		// Test1: Sending message between mboxes on different
		// nodes given registered name and node without host.
		mbox.send("mboxOtherNode","javanode2",msgArray[1]);
		obj = mboxOtherNode.receive(recTime);
		dbg("Mbox at same node: " + obj);
		if (obj == null) System.exit(14);
		if(!((OtpErlangAtom)obj).equals(msgArray[1])) System.exit(15);

		// Test2: Sending message between mboxes on different
		// nodes given registered name and node with host.
		mbox.send("mboxOtherNode",mboxOtherNode.self().node(),
			  msgArray[1]);
		obj = mboxOtherNode.receive(recTime);
		dbg("Mbox at same node: " + obj);
		if (obj == null) System.exit(16);
		if(!((OtpErlangAtom)obj).equals(msgArray[1])) System.exit(17);

		// Test3: Sending message between mboxes on different
		// nodes given pid.
		mbox.send(mboxOtherNode.self(),msgArray[1]);
		obj = mboxOtherNode.receive(recTime);
		dbg("Mbox at same node: " + obj);
		if (obj == null) System.exit(18);
		if(!((OtpErlangAtom)obj).equals(msgArray[1])) System.exit(19);

		break;


	    case java_internal_send_receive_self:

		// Test1: Sending message to myself given registered
		// name and node without host.
		mbox2.send("java_echo_server2","javanode",msgArray[1]);
		obj = mbox2.receive(recTime);
		dbg("Self: " + obj);
		if (obj == null) System.exit(18);
		if(!((OtpErlangAtom)obj).equals(msgArray[1])) System.exit(19);

		// Test2: Sending message to myself given registered
		// name and node with host.
		mbox2.send("java_echo_server2",mbox2.self().node(),msgArray[1]);
		obj = mbox2.receive(recTime);
		dbg("Self: " + obj);
		if (obj == null) System.exit(20);
		if(!((OtpErlangAtom)obj).equals(msgArray[1])) System.exit(21);

		// Test3: Sending message to myself given registered
		// name but not host.
		mbox2.send("java_echo_server2",msgArray[1]);
		obj = mbox2.receive(recTime);
		dbg("Self: " + obj);
		if (obj == null) System.exit(22);
		if(!((OtpErlangAtom)obj).equals(msgArray[1])) System.exit(23);

		// Test4: Sending message to myself given pid.
		mbox2.send(mbox2.self(),msgArray[1]);
		obj = mbox2.receive(recTime);
		dbg("Self: " + obj);
		if (obj == null) System.exit(24);
		if(!((OtpErlangAtom)obj).equals(msgArray[1])) System.exit(25);

		break;

	    }

	    // Closing erl_send_receive_server by sending the atom 'done' to it.
	    mbox.send(erlangPid,new OtpErlangAtom("done"));
	}
	catch (Exception e) {
	    System.out.println("" + e);
	    System.exit(26);
	}
    }

    private static OtpErlangTuple getNameNode(String mboxName,OtpNode node) {
	OtpErlangObject[] array = {new OtpErlangAtom(mboxName),
				   new OtpErlangAtom(node.node())};
	return new OtpErlangTuple(array);

    }

    private static void dbg(String str) {
	if (dbg) System.out.println(str);
    }


}
