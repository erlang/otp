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

import java.util.ArrayList;

import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;

class GetNames {

    /*
      Implements test case jinterface_SUITE:get_names/1

    */

    public static void main(String argv[]) {

	try {
	    OtpNode node = new OtpNode("javanode");
	    OtpMbox mbox1 = node.createMbox();
	    mbox1.registerName("mbox1");
	    node.createMbox("mbox2");
	    OtpMbox mbox3 = node.createMbox();
	    node.registerName("mbox3",mbox3);

        ArrayList<String> existing_names = new ArrayList<String>();
	    existing_names.add("mbox3");
	    existing_names.add("mbox2");
	    existing_names.add("mbox1");

	    String[] names = node.getNames();
	    if (names.length != existing_names.size()) fail(1);

	    for(int i=0; i<names.length; i++) {
		System.out.println("" + names[i]);
		existing_names.remove(names[i]);
	    }

	    if (!existing_names.isEmpty()) fail(2);
	}
	catch (Exception e) {
	    fail("" + e, 3);
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
