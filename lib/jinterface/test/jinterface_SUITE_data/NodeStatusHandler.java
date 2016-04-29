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

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBoolean;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;
import com.ericsson.otp.erlang.OtpNodeStatus;

public class NodeStatusHandler extends OtpNodeStatus {
    /*
      Implements java side of test cases in jinterface_SUITE.erl

      Test OtpNode.registerStatusHandler(...) and class OtpNodeStatus.
     */

    private static final boolean dbg = true;
    private static final int recTime = 2000;

    private static String erlNode = null;
    private static String cookie = null;
    private static OtpMbox mbox = null;

    private static final int status_handler_localStatus = 1;
    private static final int status_handler_remoteStatus = 2;
    private static final int status_handler_connAttempt = 3;

    public static void main(String argv[]) {

	cookie = argv[0];
	erlNode = argv[1];

	try {
	  OtpNode javaNode = new OtpNode("javanode", cookie);
	  mbox = javaNode.createMbox();
	}
	catch (Exception e) {
	    dbg("EXCEPTION when creating javanode: " + e);
	    System.exit(1);
	}

	try {
	    OtpNode node1 = new OtpNode("javanode1", cookie);
	    node1.registerStatusHandler(new NodeStatusHandler());

	    switch (Integer.parseInt(argv[2])) {

	    case status_handler_localStatus:
		dbg("java running test case \"status_handler_localStatus\"");

		Thread.sleep(200); // Give 'nodeup' message a chance
		                   // before closing
		node1.close();
		Thread.sleep(500);
		break;

	    case status_handler_remoteStatus:
		dbg("java running test case \"status_handler_remoteStatus\"");

		OtpNode node2 = new OtpNode("javanode2", cookie);
		node2.ping(node1.node(),2000);
		node2.close();
		Thread.sleep(500);
		break;

	    case status_handler_connAttempt:
		dbg("java running test case \"status_handler_connAttempt\"");

		OtpNode node3 = new OtpNode("javanode3","othercookie");
		node3.ping(node1.node(),2000);
		node1.ping(node3.node(),2000);
		break;

	    }

	    OtpErlangObject o = mbox.receive(recTime);
        if (o == null) {
            System.exit(2);
            return;
        }
	    if (! ((OtpErlangAtom)o).atomValue().equals("done"))
		System.exit(3);

	}
	catch (Exception e) {
	    dbg("EXCEPTION: " + e);
	    System.exit(4);
	}

    }



  @Override
  public void remoteStatus(String node, boolean up, Object info) {
      try {
	  dbg("Got remoteStatus: " + node + " " + up + " "  + info);
	  OtpErlangObject[] msgArray = new OtpErlangObject[4];
	  msgArray[0] = new OtpErlangAtom("remoteStatus");
	  msgArray[1] = new OtpErlangString(node);
	  msgArray[2] = new OtpErlangBoolean(up);
	  msgArray[3] = mbox.self();
	  OtpErlangTuple msg = new OtpErlangTuple(msgArray);
	  mbox.send("erl_status_server", erlNode, msg);

      }
      catch (Exception e) {
	  dbg("EXCEPTION in remoteStatus: " + e + "\nArgs:  " +
			     node + " " + up + " "  + info);
	  System.exit(5);
      }
  }


  @Override
  public void localStatus(String node, boolean up, Object info) {
      try {
	  dbg("Got localStatus: " + node + " " + up + " "  + info);
	  OtpErlangObject[] msgArray = new OtpErlangObject[4];
	  msgArray[0] = new OtpErlangAtom("localStatus");
	  msgArray[1] = new OtpErlangString(node);
	  msgArray[2] = new OtpErlangBoolean(up);
	  msgArray[3] = mbox.self();
	  OtpErlangTuple msg = new OtpErlangTuple(msgArray);
	  mbox.send("erl_status_server", erlNode, msg);

      }
      catch (Exception e) {
	  dbg("EXCEPTION in localStatus: " + e + "\nArgs:  " +
			     node + " " + up + " "  + info);
	  System.exit(6);
      }
  }



@Override
  public void connAttempt(String node, boolean incoming, Object info) {
      try {
	  dbg("Got connAttempt: " + node + " " + incoming + " "  + info);
	  OtpErlangObject[] msgArray = new OtpErlangObject[4];
	  msgArray[0] = new OtpErlangAtom("connAttempt");
	  msgArray[1] = new OtpErlangString(node);
	  msgArray[2] = new OtpErlangBoolean(incoming);
	  msgArray[3] = mbox.self();
	  OtpErlangTuple msg = new OtpErlangTuple(msgArray);
	  mbox.send("erl_status_server", erlNode, msg);

      }
      catch (Exception e) {
	  dbg("EXCEPTION in connAttempt: " + e + "\nArgs:  " +
			     node + " " + incoming + " "  + info);
	  System.exit(7);
      }
  }


    private static void dbg(String str) {
	if (dbg) System.out.println(str);
    }

}
