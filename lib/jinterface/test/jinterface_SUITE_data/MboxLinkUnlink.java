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

class MboxLinkUnlink {

    /*
      Implements test case jinterface_SUITE:mbox_link_unlink/1



    */

    private static final int java_link_and_exit = 1;
    private static final int erl_link_and_exit = 2;
    private static final int erl_link_java_exit = 3;
    private static final int java_link_erl_exit = 4;
    private static final int internal_link_linking_exits = 5;
    private static final int internal_link_linked_exits = 6;
    private static final int internal_unlink_linking_exits = 7;
    private static final int internal_unlink_linked_exits = 8;
    private static final int normal_exit = 9;
    private static final int kill_mbox = 10;
    private static final int kill_erl_proc_from_java = 11;
    private static final int kill_mbox_from_erlang = 12;
    private static final int erl_exit_with_reason_any_term = 13;
    private static final int java_exit_with_reason_any_term = 14;

    private static boolean dbg = true;


    public static void main(String argv[]) {


	//String cookie = argv[0];
	String erlNode = argv[1];
	OtpErlangObject expected = null;
	boolean waiting = false;

	try { //
	    OtpNode node = new OtpNode("javanode");
	    OtpMbox mainMbox = node.createMbox();

	    try {
		// Initiate and set up connection to erlang process
		OtpMbox mbox = node.createMbox();
		OtpMbox mbox2;

		OtpErlangObject[] msg = {mainMbox.self(),mbox.self()};
		mbox.send("erl_link_server", erlNode, new OtpErlangTuple(msg));
		OtpErlangObject o = mbox.receive(1000);
		if (o == null) System.exit(1);
		OtpErlangTuple tuple = (OtpErlangTuple)o;
		int tag = (int)((OtpErlangLong)tuple.elementAt(0)).longValue();

		switch (tag) {

		case java_exit_with_reason_any_term:
		case java_link_and_exit:
		    dbg("Java got \"java_link_and_exit\" or " +
			"\"java_exit_with_reason_any_term\"");
		    mbox.link((OtpErlangPid)tuple.elementAt(1));
		    mbox.send((OtpErlangPid)tuple.elementAt(1),
			      new OtpErlangAtom("ok"));
		    mbox.exit(tuple.elementAt(2));
		    break;
		case erl_exit_with_reason_any_term:
		case erl_link_and_exit:
		    dbg("Java got \"erl_link_and_exit\" or " +
			"\"erl_exit_with_reason_any_term\"");
		    mbox.send((OtpErlangPid)tuple.elementAt(1),
			      new OtpErlangAtom("ok"));
		    waiting = true;
		    expected = tuple.elementAt(2);
		    mbox.receive(1000);
		    System.exit(2);
		case erl_link_java_exit:
		    dbg("Java got \"erl_link_java_exit\"");
		    mbox.exit(tuple.elementAt(2));
		    break;
		case java_link_erl_exit:
		    dbg("Java got \"java_link_erl_exit\"");
		    mbox.link((OtpErlangPid)tuple.elementAt(1));
		    mbox.send((OtpErlangPid)tuple.elementAt(1),
			      new OtpErlangAtom("ok"));
		    waiting = true;
		    expected = tuple.elementAt(2);
		    mbox.receive(1000);
		    System.exit(3);
		case internal_link_linking_exits:
		    dbg("Java got \"internal_link_linking_exits\"");
		    mbox2 = node.createMbox();
		    mbox.link(mbox2.self());
		    mbox.exit(tuple.elementAt(2));
		    waiting = true;
		    expected = tuple.elementAt(2);
		    mbox2.receive(1000); // hanging waiting for exit
		    System.exit(4);  // got someting other than exit
		case internal_link_linked_exits:
		    dbg("Java got \"internal_link_linked_exits\"");
		    mbox2 = node.createMbox();
		    mbox.link(mbox2.self());
		    mbox2.exit(tuple.elementAt(2));
		    waiting = true;
		    expected = tuple.elementAt(2);
		    mbox.receive(1000); // hanging waiting for exit
		    System.exit(5);  // got someting other than exit
		case internal_unlink_linking_exits:
		    dbg("Java got \"internal_unlink_linking_exits\"");
		    mbox2 = node.createMbox();
		    mbox.link(mbox2.self());
		    mbox.unlink(mbox2.self());
		    mbox.link(mbox2.self());
		    mbox2.unlink(mbox.self());
		    mbox2.exit(tuple.elementAt(2));
		    if (mbox.receive(500)!=null) System.exit(6);
		    break;
		case internal_unlink_linked_exits:
		    dbg("Java got \"internal_unlink_linked_exits\"");
		    mbox2 = node.createMbox();
		    mbox.link(mbox2.self());
		    mbox.unlink(mbox2.self());
		    mbox.link(mbox2.self());
		    mbox2.unlink(mbox.self());
		    mbox.exit(tuple.elementAt(2));
		    if (mbox2.receive(500)!=null) System.exit(7);
		    break;
		case normal_exit:
		    dbg("Java got \"normal_exit\"");
		    mbox.close();
		    break;
		case kill_mbox:
		    dbg("Java got \"kill_mbox\"");
		    mbox.exit("kill");
		    break;
		case kill_erl_proc_from_java:
		    dbg("Java got \"kill_erl_proc_from_java\"");
		    mbox.exit((OtpErlangPid)tuple.elementAt(1),"kill");
		    break;
		case kill_mbox_from_erlang:
		    dbg("Java got \"kill_mbox_from_erlang\"");
		    /* This will make the testcase successful, but it is
		       not the correct way to do it...
		       Mbox should probably just die when the kill signal is
		       received from erlang (or other mbox).

		    try {
			mbox.receive(1000);
			System.exit(8);
		    }
		    catch (OtpErlangExit exit) {
			if(!(exit.reason().equals(new OtpErlangAtom("kill"))))
			    System.exit(9);
			mbox.exit("killed");
		    }
		    */
		    break;
		}
	    }
	    catch (OtpErlangExit exit) {
		dbg("Java got exit: " + exit.reason());
		if(!(waiting && exit.reason().equals(expected)))
		    System.exit(10);
	    }

	    OtpErlangAtom done = new OtpErlangAtom("done");
	    mainMbox.send("erl_link_server", erlNode, done);
	    OtpErlangObject o = mainMbox.receive(1000);
	    if (o == null) System.exit(11);
	    else if (!((OtpErlangAtom)o).equals(done))
		System.exit(12);

	}
	catch (Exception e) {
	    System.out.println("EXCEPTION: " + e);
	    System.exit(13);
	}
    }

    private static void dbg(String str) {
	if (dbg) System.out.println(str);
    }

}
