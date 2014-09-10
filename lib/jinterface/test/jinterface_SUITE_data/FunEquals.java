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

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangFun;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;

public class FunEquals {

    /*
      Implements test case jinterface_SUITE:fun_equals/1

      Test the function OtpErlangFun.equals()
    */

    public static void main(String argv[]) {

        OtpErlangPid pid = new OtpErlangPid("here", 4, 5, 0);
        String module = "mod";
        int arity = 2;
        byte[] md5 = new byte[]{3,5,7};
        int index = 2;
        long old_index = 1;
        long uniq= 2;
        OtpErlangObject[] freeVars = new OtpErlangObject[]{
                new OtpErlangAtom("hej"), new OtpErlangLong(9)
            };

        OtpErlangFun f1 = new OtpErlangFun(pid, module, arity, md5,
                index, old_index, uniq, freeVars);
        OtpErlangFun f2 = new OtpErlangFun(pid, module, arity, copyArray(md5),
                index, old_index, uniq, copyArray(freeVars));

        if(!f1.equals(f2))
            fail(1);

    }

    private static void fail(int reason) {
        System.exit(reason);
    }

    private static byte[] copyArray(byte[] source) {
        byte[] result = new byte[source.length];
        System.arraycopy(source, 0, result, 0, source.length);
        return result;
    }

    private static OtpErlangObject[] copyArray(OtpErlangObject[] source) {
        OtpErlangObject[] result = new OtpErlangObject[source.length];
        System.arraycopy(source, 0, result, 0, source.length);
        return result;
    }

}
