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

class NodePing {

    /*
      Implements test case jinterface_SUITE:node_ping/1

      Creates three OtpNode objects. One with default cookie, one with
      specified same cookie as the node running the test case and one
      with a faulty cookie. From each OtpNode object the test_server
      node is pinged.

      Also the default cookie node pings itself, and the node with the
      specified cookie pings the node with default cookie.
    */

    public static void main(String argv[]) {

	String cookie = argv[0];
	String erlNode = argv[1];

	try {
	    OtpNode node1 = new OtpNode("javanode1");
	    ping(node1,erlNode,"Default cookie:",true,1);
	    ping(node1,node1.node(),"Self:",true,2);
	    ping(node1,"javanode1","Self:",true,3);

	    OtpNode node2 = new OtpNode("javanode2",cookie);
	    ping(node2,erlNode,"Specified cookie:",true,4);
	    ping(node2,"javanode1","Javanode (no host):",true,5);
	    ping(node2,node1.node(),"Javanode:",true,6);

	    OtpNode node3 = new OtpNode("javanode3","faultycookie");
	    ping(node3,erlNode,"Faulty cookie:",false,7);

	    // Test OtpNode.cookie() and OtpNode.setCookie(cookie) as well
	    if (!node3.cookie().equals("faultycookie"))
		fail("Testing OtpNode.cookie()",8);
	    String old = node3.setCookie(cookie);
	    if (!old.equals("faultycookie"))
		fail("Checking return of OtpNode.setCookie(cookie)",9);
	    ping(node3,erlNode,"setCookie:",true,10);
	}
	catch (Exception e) {
	    fail("" + e, 11);
	}
    }

    private static void ping(OtpNode node, String remote, String descr,
			     boolean expected, int reason) {
	if ( node.ping(remote,2000) == expected ) {
	    System.out.println(descr + " ping(" + remote + ") -> " + expected);
	}
	else {
	    fail("ERROR: " + descr + " ping(" + remote +") -> " + !expected,
		 reason);
	}
    }

    private static void fail(String str, int reason) {
	System.out.println(str);
	System.exit(reason);
    }
}
