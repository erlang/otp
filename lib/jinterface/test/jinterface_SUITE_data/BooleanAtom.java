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

class BooleanAtom {

    /*
      Implements test case jinterface_SUITE:boolean_atom/1

      Test the function OtpErlangAtom.booleanValue()
    */

    public static void main(String argv[]) {

	OtpErlangAtom atom = new OtpErlangAtom("true");
	if (!atom.booleanValue()) fail(1);

	atom = new OtpErlangAtom("false");
	if (atom.booleanValue()) fail(2);

	atom = new OtpErlangAtom("somethingelse");
	if (atom.booleanValue()) fail(3);

    }

    private static void fail(int reason) {
	System.exit(reason);
    }
}
