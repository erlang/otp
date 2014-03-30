import java.util.Arrays;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangMap;
import com.ericsson.otp.erlang.OtpInputStream;
import com.ericsson.otp.erlang.OtpOutputStream;

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

class Maps {

    /*
     * Implements test case jinterface_SUITE:maps/1
     * 
     * Test the class OtpErlangMap
     */

    @SuppressWarnings("resource")
    public static void main(final String argv[]) {

	runTest(new byte[] { (byte) 131, 116, 0, 0, 0, 0 }, "#{}", 1);
	runTest(new byte[] { (byte) 131, 116, 0, 0, 0, 1, 100, 0, 1, 97, 100,
		0, 1, 98 }, "#{a => b}", 2);
	// make sure keys are sorted here, jinterface doesn't reorder them
	runTest(new byte[] { (byte) 131, 116, 0, 0, 0, 2, 97, 2, 106,
			     100, 0, 1, 97, 97, 1 }, "#{2 => [],a => 1}", 3);
	runTest(new byte[] { (byte) 131, 116, 0, 0, 0, 1, 104, 1, 97, 3, 108,
		0, 0, 0, 1, 100, 0, 1, 114, 106 }, "#{{3} => [r]}", 4);

	try {
	    // #{2 => [],a => 1}
	    final OtpErlangMap map = new OtpErlangMap(new OtpInputStream(
		  new byte[] { (byte) 131, 116, 0, 0, 0, 2, 97, 2, 106,
			       100, 0, 1, 97, 97, 1 }));

	    if (map.arity() != 2) {
		fail(5);
	    }
	    if (!new OtpErlangLong(1).equals(map.get(new OtpErlangAtom("a")))) {
		fail(6);
	    }
	    if (!new OtpErlangList().equals(map.get(new OtpErlangLong(2)))) {
		fail(7);
	    }
	    if (map.get(new OtpErlangLong(1)) != null) {
		fail(8);
	    }
	} catch (final OtpErlangDecodeException e) {
	    fail(99);
	}

    }

    @SuppressWarnings("resource")
    private static void runTest(final byte[] in, final String out, final int err) {
	try {
	    final OtpInputStream is = new OtpInputStream(in);

	    final OtpErlangMap map = new OtpErlangMap(is);
	    final String output = map.toString();
	    if (!output.equals(out)) {
		fail("toString mismatch " + output + " <> " + out, err);
	    }

	    final OtpOutputStream os = new OtpOutputStream(map);
	    final byte[] outArray0 = os.toByteArray();
	    final byte[] outArray = new byte[outArray0.length + 1];
	    System.arraycopy(outArray0, 0, outArray, 1, outArray0.length);
	    outArray[0] = (byte) 131;
	    if (!Arrays.equals(in, outArray)) {
		fail("encode error " + Arrays.toString(outArray), err);
	    }
	} catch (final OtpErlangDecodeException e) {
	    fail("decode error " + e.getMessage(), err);
	} catch (final Exception e) {
	    fail("error " + e.getMessage(), err);
	}
    }

    private static void fail(final int reason) {
	System.exit(reason);
    }

    private static void fail(final String str, final int reason) {
	System.out.println(str);
	System.exit(reason);
    }
}
