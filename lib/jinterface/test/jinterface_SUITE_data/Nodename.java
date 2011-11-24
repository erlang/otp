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

class Nodename {

    /*
      Implements test case jinterface_SUITE:nodename/1

    */

    public static void main(String argv[]) {

	String host = argv[0];

	try {
	    OtpNode node = new OtpNode("javanode");
	    System.out.println("Given host: " + host +
			       " Host: " + node.host() +
			       " Alive: " + node.alive() +
			       " Node: " + node.node());

	    if (!node.host().equals(host)) fail(1);
	    if (!node.alive().equals("javanode")) fail(2);
	    if (!node.node().equals("javanode@" + host)) fail(3);
	}
	catch (Exception e) {
	    System.out.println("" + e);
	    fail(4);
	}
    }

    private static void fail(int reason) {
	System.exit(reason);
    }

}
