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

class RegisterAndWhereis {

    /*
      Implements test case jinterface_SUITE:register_and_whereis/1

    */

    public static void main(String argv[]) {

	try {
	    OtpNode node = new OtpNode("javanode");
	    OtpMbox mbox1 = node.createMbox();
	    mbox1.registerName("mbox1");
	    OtpMbox mbox2 = node.createMbox("mbox2");
	    OtpMbox mbox3 = node.createMbox();
	    node.registerName("mbox3",mbox3);

	    OtpErlangPid pid1 = mbox1.self();
	    OtpErlangPid pid2 = mbox2.self();
	    OtpErlangPid pid3 = mbox3.self();

	    if (!pid1.equals(node.whereis("mbox1"))) fail(1);
	    if (!pid1.equals(mbox1.whereis("mbox1"))) fail(2);
	    if (!pid1.equals(mbox2.whereis("mbox1"))) fail(3);
	    if (!pid1.equals(mbox3.whereis("mbox1"))) fail(4);
	    if (!pid2.equals(node.whereis("mbox2"))) fail(5);
	    if (!pid2.equals(mbox2.whereis("mbox2"))) fail(6);
	    if (!pid3.equals(node.whereis("mbox3"))) fail(7);
	    if (!pid3.equals(mbox3.whereis("mbox3"))) fail(8);

	    node.closeMbox(mbox1);
	    mbox2.close();

	    if (node.whereis("mbox1") != null) fail(9);
	    if (node.whereis("mbox2") != null) fail(10);
	    if (mbox3.whereis("mbox1") != null) fail(11);
	    if (mbox3.whereis("mbox2") != null) fail(12);

	    mbox3.close();
	    if (mbox2.whereis("mbox3") != null) fail(13);



	}
	catch (Exception e) {
	    fail("" + e, 14);
	}
    }

    private static void fail(int reason) {
	System.exit(reason);
    }

    private static void fail(String str, int reason) {
	System.out.println(str);
	System.exit(reason);
    }
}
