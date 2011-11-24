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
	    if (o == null) System.exit(2);
	    if (! ((OtpErlangAtom)o).atomValue().equals("done"))
		System.exit(3);

	}
	catch (Exception e) {
	    dbg("EXCEPTION: " + e);
	    System.exit(4);
	}

    }



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
