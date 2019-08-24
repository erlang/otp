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
