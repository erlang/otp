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

import java.io.UnsupportedEncodingException;
import java.util.HashSet;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangBoolean;
import com.ericsson.otp.erlang.OtpErlangException;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpExternal;
import com.ericsson.otp.erlang.OtpInputStream;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;
import com.ericsson.otp.erlang.OtpOutputStream;

public class echo_server {
    private static boolean debug = false;

    public static void main(final String[] argv) {
	try {
	    System.out.println("echo_server booting...");

	    for (int j = 0; j < argv.length; j++) {
		System.out.println("argv[" + j + "] = \"" + argv[j] + "\"");
	    }

	    if (argv.length != 4 && argv.length != 5) {
		System.out.println("Wrong number of arguments!");
		System.exit(1);
	    }

	    // Start node and mbox
	    final OtpNode node = new OtpNode(argv[0], argv[1]);
	    if (argv.length == 5 && argv[4].equals("unicode")) {
		System.out.println("Setting unicode string mode.");
		node.setFlags(OtpInputStream.DECODE_INT_LISTS_AS_STRINGS);
	    }
	    final OtpMbox mbox = node.createMbox();

	    // Announce our presence
	    final OtpErlangObject[] amsg = new OtpErlangObject[3];
	    amsg[0] = new OtpErlangAtom("echo_server");
	    amsg[1] = new OtpErlangAtom(argv[0]);
	    amsg[2] = mbox.self();
	    final OtpErlangTuple atuple = new OtpErlangTuple(amsg);
	    mbox.send(argv[3], argv[2], atuple);

	    // Do echoing...
	    while (true) {
		final OtpErlangObject o = mbox.receive();
		if (o == null) {
		    continue;
		}
		if (o instanceof OtpErlangTuple) {
		    final OtpErlangTuple msg = (OtpErlangTuple) o;
		    final int arity = msg.arity();
		    if (arity < 2) {
			System.out
				.println("Arity < 2; echo_server aborting...");
			System.exit(2);
		    } else if (arity == 2) {
			final OtpErlangPid from = (OtpErlangPid) msg
				.elementAt(0);
			if (debug) System.out.println("Echoing: "
						      + msg.elementAt(1));

			final OtpErlangObject[] rmsg = new OtpErlangObject[2];
			rmsg[0] = mbox.self();
			rmsg[1] = msg.elementAt(1);
			final OtpErlangTuple rtuple = new OtpErlangTuple(rmsg);

			mbox.send(from, rtuple);
			continue;
		    } else if (arity == 3) {
			echoTwisted(mbox, msg);
			continue;
		    } else {
			System.out
				.println("Arity > 3; echo_server aborting...");
			System.exit(2);
		    }
		} else if (o instanceof OtpErlangAtom) {
		    OtpErlangAtom a = (OtpErlangAtom) o;
		    if (a.atomValue().equals("debug")) {
			debug = true;
		    } else if (a.atomValue().equals("bye")) {
			System.out.println("echo_server halting...");
			System.exit(0);
		    }
		} else { // probably 'bye'
		}
		System.out.println("Unexpected: " + o +
				   " echo_server aborting...");
		System.exit(2);
	    }
	} catch (final Exception e) {
	    System.out.println("" + e);
	    System.exit(2);
	}
    }

    private static void echoTwisted(final OtpMbox mbox,
				    final OtpErlangTuple msg)
	    throws OtpErlangException {
	final OtpErlangPid from = (OtpErlangPid) msg.elementAt(0);

	final OtpErlangObject[] rmsg = new OtpErlangObject[3];
	if (debug) System.out.println("Echo in: " + msg);
	rmsg[0] = mbox.self();
	rmsg[1] = twist(msg.elementAt(1), rmsg[2] = msg.elementAt(2));
	final OtpErlangTuple rtuple = new OtpErlangTuple(rmsg);
	if (debug) System.out.println("Echo out: " + rtuple);

	mbox.send(from, rtuple);
    }

    private static HashSet<OtpErlangObject> hash_set =
	new HashSet<OtpErlangObject>();

    private static OtpErlangObject twist(final OtpErlangObject i,
	    final OtpErlangObject t) throws OtpErlangException {
	hash_set.add(i);
	if (t instanceof OtpErlangAtom) {
	    final String atomValue = ((OtpErlangAtom) t).atomValue();
	    if (atomValue.equals("binary") && i instanceof OtpErlangBinary) {
		final OtpErlangBinary b = (OtpErlangBinary) i;
        @SuppressWarnings("resource")
		final OtpInputStream bis = new OtpInputStream(b.binaryValue(),
			0);
		final OtpErlangObject o = bis.read_any();
		return o;
	    } else if (atomValue.equals("compress")) {
        @SuppressWarnings("resource")
		final OtpOutputStream oos = new OtpOutputStream();
		oos.write1(OtpExternal.versionTag);
		oos.write_compressed(i);
		final OtpErlangBinary o =
		    new OtpErlangBinary(oos.toByteArray());
		return o;
	    } else if (atomValue.equals("bigint")
		       && i instanceof OtpErlangLong) {
		final OtpErlangLong l = (OtpErlangLong) i;
		final int w = l.signum() * l.bitLength();
		final OtpErlangLong x = new OtpErlangLong(l.longValue());
		final java.math.BigInteger b = l.bigIntegerValue();
		System.out.println("long: " + l + ": " + w + ": " + b.signum()
			* b.bitLength() + ": " + x + ": " + l.isLong() + ": "
			+ l.isULong());
		return new OtpErlangTuple(new OtpErlangObject[] { l,
			new OtpErlangInt(w), x,
			new OtpErlangInt(l.isLong() ? 1 : 0),
			new OtpErlangInt(l.isULong() ? 1 : 0) });
	    } else if (atomValue.equals("tail")
		       && i instanceof OtpErlangList) {
		final OtpErlangObject o = ((OtpErlangList) i).getTail();
		if (o == null) {
		    return new OtpErlangAtom("null");
		}
		return o;
	    } else if (atomValue.equals("tail3")
		       && i instanceof OtpErlangList) {
		final OtpErlangObject o = ((OtpErlangList) i).getNthTail(3);
		if (o == null) {
		    return new OtpErlangAtom("null");
		}
		return o;
	    } else if (atomValue.equals("strcat")
		       && i instanceof OtpErlangList) {
		final java.lang.StringBuffer b = new java.lang.StringBuffer();
		final OtpErlangList l = (OtpErlangList) i;
		for (final OtpErlangObject j : l) {
		    final OtpErlangString k = (OtpErlangString) j;
		    b.append(k.stringValue());
		}
		final OtpErlangObject o = new OtpErlangString(b.toString());
		return o;
	    } else if (atomValue.equals("sub3atom")
		    && i instanceof OtpErlangString) {
		final OtpErlangString s = (OtpErlangString) i;
		final OtpErlangAtom o = new OtpErlangAtom(s.stringValue()
			.substring(3));
		return o;
	    } else if (atomValue.equals("codepointBug")
		    && i instanceof OtpErlangString) {
		final OtpErlangString s = (OtpErlangString) i;
		final String ss = s.stringValue().substring(3, 6);
        @SuppressWarnings("unused")
		final int[] cps = OtpErlangString.stringToCodePoints(ss);
		return s;
	    } else if (atomValue.equals("utf8")) {
		if (i instanceof OtpErlangString) {
		    final OtpErlangString s = (OtpErlangString) i;
		    byte[] bytes;
		    try {
			bytes = s.stringValue().getBytes("UTF-8");
		    } catch (final UnsupportedEncodingException e) {
			bytes = new byte[] { 'e', 'r', 'r', 'o', 'r' };
		    }
		    final OtpErlangBinary b = new OtpErlangBinary(bytes);
		    return b;
		}
	    } else if(atomValue.equals("to_string_neg_int_list")) {
		OtpErlangString oes = null;
		if (i instanceof OtpErlangString) {
		    oes = (OtpErlangString) i;
		} else if (i instanceof OtpErlangList) {
		    OtpErlangList oel = (OtpErlangList) i;
		    try {
			oes = new OtpErlangString(oel);
		    } catch (final Exception e) {
		    }
		}
		if (oes != null) {
		    String s = oes.stringValue();
		    int n = s.length();
		    OtpErlangObject l[] = new OtpErlangObject[n];
		    for (int j = 0;  j < n;  j++) {
			int c = s.charAt(j);
			l[j] = new OtpErlangInt(-c);
		    }
		    return new OtpErlangList(l);
		}
	    } else if(atomValue.equals("to_neg_int_list")) {
		if (i instanceof OtpErlangString) {
		    OtpErlangString oes = (OtpErlangString) i;
		    OtpErlangList oel = new OtpErlangList(oes.stringValue());
		    int n = oel.arity();
		    OtpErlangObject l[] = new OtpErlangObject[n];
		    for (int j = 0;  j < n;  j++) {
			OtpErlangLong c = (OtpErlangLong) oel.elementAt(j);
			l[j] = new OtpErlangInt(-c.intValue());
		    }
		    return new OtpErlangList(l);
		}
	    } else if (atomValue.equals("hash_lookup")) {
		final boolean exists = hash_set.contains(i);
		final OtpErlangBoolean b = new OtpErlangBoolean(exists);
		return b;
	    } else if (atomValue.equals("hash_clear")) {
		hash_set.clear();
		return new OtpErlangAtom("hash_cleared");
	    }
	}
	return i;
    }
}
