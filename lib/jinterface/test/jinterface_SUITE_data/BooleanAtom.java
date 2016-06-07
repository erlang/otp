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
