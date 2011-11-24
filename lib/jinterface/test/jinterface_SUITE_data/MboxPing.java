/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2004-2010. All Rights Reserved.
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

import com.ericsson.otp.erlang.*;

class MboxPing {

    /*
      Implements test case jinterface_SUITE:mbox_ping/1

      Creates an OtpNode object with an OtpMbox object. The test_server
      node is pinged from the OtpMbox.
    */

    public static void main(String argv[]) {

	try {
	    OtpNode node = new OtpNode("javanode",argv[0]);
	    OtpMbox mbox = node.createMbox();
	    if (mbox.ping(argv[1],2000)) {
		System.out.println("OtpMbox.ping(" + argv[1] + ") -> true");
	    }
	    else {
		System.out.println("ERROR: OtpMbox.ping(" + argv[1] +
				   ") -> false");
		System.exit(1);
	    }
	}
	catch (Exception e) {
	    System.out.println("" + e);
	    System.exit(2);
	}
    }
}
