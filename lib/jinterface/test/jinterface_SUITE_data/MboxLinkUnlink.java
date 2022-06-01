/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2004-2021. All Rights Reserved.
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
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;

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
    private static final int erl_link_unlink_link_and_exit = 15;
    private static final int erl_link_java_unlink_link_and_exit = 16;
    private static final int simultaneous_erl_link_java_link_unlink = 17;
    private static final int simultaneous_erl_link_unlink_java_link = 18;

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
        if (o == null) {
            System.exit(1);
            return;
        }
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
		case erl_link_unlink_link_and_exit:
		    dbg("Java got \"erl_link_and_exit\" or " +
			"\"erl_exit_with_reason_any_term\" or " +
                        "\"erl_link_unlink_link_and_exit\"");
                    if (!is_linked(mbox, (OtpErlangPid)tuple.elementAt(1)))
                        System.exit(17);
		    mbox.send((OtpErlangPid)tuple.elementAt(1),
			      new OtpErlangAtom("ok"));
		    waiting = true;
		    expected = tuple.elementAt(2);
		    mbox.receive(1000);
		    System.exit(2);
                    break;
		case erl_link_java_unlink_link_and_exit:
		    dbg("Java got \"erl_link_java_unlink_link_and_exit\"");
		    mbox.unlink((OtpErlangPid)tuple.elementAt(1));
		    mbox.unlink((OtpErlangPid)tuple.elementAt(1));
		    mbox.link((OtpErlangPid)tuple.elementAt(1));
                    if (!is_linked(mbox, (OtpErlangPid)tuple.elementAt(1)))
                        System.exit(16);
		    mbox.send((OtpErlangPid)tuple.elementAt(1),
			      new OtpErlangAtom("ok"));
		    waiting = true;
		    expected = tuple.elementAt(2);
		    mbox.receive(1000);
		    System.exit(2);
                    break;
                case simultaneous_erl_link_java_link_unlink: {
		    dbg("Java got \"simultaneous_erl_link_java_link_unlink\"");
                    OtpErlangPid remote = (OtpErlangPid) tuple.elementAt(1);
                    long go_time = ((OtpErlangLong) tuple.elementAt(2)).longValue();
                    spin_wait_until(go_time);
		    mbox.link(remote);
		    mbox.unlink(remote);
                    OtpErlangAtom check_link = new OtpErlangAtom("check_link");
                    mbox.send(remote, check_link);
                    OtpErlangObject chk_msg = mbox.receive(2000);
                    if (chk_msg == null)
                        System.exit(14);
                    else if (!((OtpErlangAtom) chk_msg).equals(check_link))
                        System.exit(15);
                    mbox.send(remote, new OtpErlangAtom(is_linked(mbox, remote)
                                                        ? "linked"
                                                        : "not_linked"));
                    mbox.close();
                    break;
                }
                case simultaneous_erl_link_unlink_java_link: {
		    dbg("Java got \"simultaneous_erl_link_unlink_java_link\"");
                    OtpErlangPid remote = (OtpErlangPid) tuple.elementAt(1);
                    long go_time = ((OtpErlangLong) tuple.elementAt(2)).longValue();
                    spin_wait_until(go_time);
		    mbox.link(remote);
                    OtpErlangAtom check_link = new OtpErlangAtom("check_link");
                    OtpErlangObject chk_msg = mbox.receive(2000);
                    if (chk_msg == null)
                        System.exit(14);
                    else if (!((OtpErlangAtom) chk_msg).equals(check_link))
                        System.exit(15);
                    // We now know the unlink should have reached us and we
                    // should have sent the unlink ack...
                    mbox.send(remote, check_link);
                    mbox.send(remote, new OtpErlangAtom(is_linked(mbox, remote)
                                                        ? "linked"
                                                        : "not_linked"));
                    mbox.close();
                    break;
                }
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
            break;
		case internal_link_linking_exits:
		    dbg("Java got \"internal_link_linking_exits\"");
		    mbox2 = node.createMbox();
		    mbox.link(mbox2.self());
		    mbox.exit(tuple.elementAt(2));
		    waiting = true;
		    expected = tuple.elementAt(2);
		    mbox2.receive(1000); // hanging waiting for exit
		    System.exit(4);  // got something other than exit
            break;
		case internal_link_linked_exits:
		    dbg("Java got \"internal_link_linked_exits\"");
		    mbox2 = node.createMbox();
		    mbox.link(mbox2.self());
		    mbox2.exit(tuple.elementAt(2));
		    waiting = true;
		    expected = tuple.elementAt(2);
		    mbox.receive(1000); // hanging waiting for exit
		    System.exit(5);  // got something other than exit
            break;
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

    private static void spin_wait_until(final long ms) {
        long time;
        do {
            time = System.currentTimeMillis();
        } while (time < ms);
    }

    private static boolean is_linked(OtpMbox mbox, OtpErlangPid pid) {
        final OtpErlangPid[] linked = mbox.linked();
        if (linked != null) {
            final int len = linked.length;
            for (int i = 0; i < len; i++) {
                if (pid.equals(linked[i]))
                    return true;
            }
        }
        return false;
    }

}
