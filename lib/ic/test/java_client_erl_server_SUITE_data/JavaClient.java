/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2003-2016. All Rights Reserved.
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
 *
 */
public class JavaClient {

    public static void main(String[] argv) 
    {
	System.out.println("Hello World!");
	if (argv.length < 4) {
	    System.out.println("Too few arguments!");
	    System.exit(1);
	}
	//      for (int j = 0; j < argv.length; j++)
	//        System.out.println(argv[j]);
	try {
	    if (argv[3].equals("marshal_ll")) {
		System.out.println("marshal_ll");
		marshal_ll(argv[0], argv[1], argv[2], argv[3]);
	    } 
	    else if (argv[3].equals("marshal_ull")) {
		marshal_ull(argv[0], argv[1], argv[2], argv[3]);
	    } 
	    else if (argv[3].equals("marshal_l")) {
		marshal_l(argv[0], argv[1], argv[2], argv[3]);
	    } 
	    else if (argv[3].equals("marshal_ul")) {
		marshal_ul(argv[0], argv[1], argv[2], argv[3]);
	    } 
	    else if (argv[3].equals("marshal_s")) {
		marshal_s(argv[0], argv[1], argv[2], argv[3]);
	    } 
	    else if (argv[3].equals("marshal_us")) {
		marshal_us(argv[0], argv[1], argv[2], argv[3]);
	    } 
	    else if (argv[3].equals("marshal_c")) {
		marshal_c(argv[0], argv[1], argv[2], argv[3]);
	    } 
	    else if (argv[3].equals("marshal_wc")) {
		marshal_wc(argv[0], argv[1], argv[2], argv[3]);
	    } 
	    else if (argv[3].equals("marshal_str")) {
		marshal_str(argv[0], argv[1], argv[2], argv[3]);
	    } 
	    else if (argv[3].equals("marshal_any_3")) {
		marshal_any_3(argv[0], argv[1], argv[2], argv[3]);
	    } 
	    else if (argv[3].equals("marshal_any_2")) {
		marshal_any_2(argv[0], argv[1], argv[2], argv[3]);
	    } 
	    else {
		System.out.println("Unknown test: "+argv[3]);
		System.exit(2);
	    }
	} catch (java.lang.Exception e) {
	    System.out.println("Exception!: "+e);
	    System.exit(3);
	}
	System.exit(0);
    }



    static void marshal_ll(String selfNode, String peerNode,
			   String cookie, String serverName)
	throws java.lang.Exception
    {
	m._iStub i = new m._iStub(selfNode, peerNode, cookie, serverName);
	// Just warming up..
	System.out.println("Just warming up.."+i);
	verify_ll(i, 3, 2, 1);
	verify_ll(i, 5, 4, 3);
	verify_ll(i, -128, 0, 1);
	// The small integer border
	verify_ll(i, 255, 0, 1);
	verify_ll(i, 256, 0, 1);
	// The integer border
	verify_ll(i, (1L<<26)-1L, 0L, 1);
	verify_ll(i, 1L<<26, 0L, 1);
	verify_ll(i, -(1L<<26), 0L, 1);
	verify_ll(i, (1L<<27)-1L, 0L, 1);
	verify_ll(i, 1L<<27, 0L, 1);
	verify_ll(i, -(1L<<27), 0L, 1);
	// Bignum byte borders
	verify_ll(i, (1L<<32)-1L, 0L, 1);
	verify_ll(i, 1L<<32, 0L, 1);
	verify_ll(i, -(1L<<32)+1L, 0L, 1);
	verify_ll(i, -(1L<<32), 0L, 1);
	verify_ll(i, (1L<<40)-1L, 0L, 1);
	verify_ll(i, 1L<<40, 0L, 1);
	verify_ll(i, -(1L<<40)+1L, 0L, 1);
	verify_ll(i, -(1L<<40), 0L, 1);
	verify_ll(i, (1L<<48)-1L, 0L, 1);
	verify_ll(i, 1L<<48, 0L, 1);
	verify_ll(i, -(1L<<48)+1L, 0L, 1);
	verify_ll(i, -(1L<<48), 0L, 1);
	// Java long border
	verify_ll(i, java.lang.Long.MAX_VALUE, 0L, 1);
	verify_ll(i, java.lang.Long.MIN_VALUE, 0L, 1);
	verify_ll(i, -1L, 0L, 1);
	// Impossible decodes
	verify_ll_bad(i, java.lang.Long.MAX_VALUE, -1L, 1);
	verify_ll_bad(i, java.lang.Long.MIN_VALUE, 1L, 1);
	verify_ll_bad(i, java.lang.Long.MIN_VALUE, 0L, -1);
	verify_ll_bad(i, java.lang.Long.MAX_VALUE, -1L, 2);
	verify_ll_bad(i, java.lang.Long.MIN_VALUE, 0L, 2);
    }

    static void marshal_ull(String selfNode, String peerNode,
			    String cookie, String serverName)
	throws java.lang.Exception
    {
	m._iStub i = new m._iStub(selfNode, peerNode, cookie, serverName);
	// Just warming up..
	verify_ull(i, 3, 2, 1);
	verify_ull(i, 5, 4, 3);
	// The small integer border
	verify_ull(i, 255, 0, 1);
	verify_ull(i, 256, 0, 1);
	// The integer border
	verify_ull(i, (1L<<26)-1L, 0L, 1);
	verify_ull(i, 1L<<26, 0L, 1);
	verify_ull(i, (1L<<27)-1L, 0L, 1);
	verify_ull(i, 1L<<27, 0L, 1);
	// Bignum byte borders
	verify_ull(i, (1L<<32)-1L, 0L, 1);
	verify_ull(i, 1L<<32, 0L, 1);
	verify_ull(i, (1L<<40)-1L, 0L, 1);
	verify_ull(i, 1L<<40, 0L, 1);
	verify_ull(i, (1L<<48)-1L, 0L, 1);
	verify_ull(i, 1L<<48, 0L, 1);
	// Java long border
	verify_ull(i, java.lang.Long.MAX_VALUE, 0L, 1);
	verify_ull(i, java.lang.Long.MIN_VALUE, 0L, 1);
	verify_ull(i, -1L, 0L, 1);
	verify_ull(i, java.lang.Long.MAX_VALUE, 
		   java.lang.Long.MIN_VALUE, 1);
	// Impossible decodes
	verify_ull_bad(i, -1L, -1L, 1);
	verify_ull_bad(i, java.lang.Long.MAX_VALUE, -1L, 2);
    }

    static void marshal_l(String selfNode, String peerNode,
			  String cookie, String serverName)
	throws java.lang.Exception
    {
	m._iStub i = new m._iStub(selfNode, peerNode, cookie, serverName);
	// Just warming up..
	verify_l(i, 3, 2, 1);
	verify_l(i, 5, 4, 3);
	verify_l(i, -128, 0, 1);
	// The small integer border
	verify_l(i, 255, 0, 1);
	verify_l(i, 256, 0, 1);
	// The integer border
	verify_l(i, (1<<26)-1, 0, 1);
	verify_l(i, 1<<26, 0, 1);
	verify_l(i, -(1<<26), 0, 1);
	verify_l(i, (1<<27)-1, 0, 1);
	verify_l(i, 1<<27, 0, 1);
	verify_l(i, -(1<<27), 0, 1);
	// Java int border
	verify_l(i, java.lang.Integer.MAX_VALUE, 0, 1);
	verify_l(i, java.lang.Integer.MIN_VALUE, 0, 1);
	// Impossible decodes
	verify_l_bad(i, java.lang.Integer.MAX_VALUE, -1, 1);
	verify_l_bad(i, java.lang.Integer.MIN_VALUE, 1, 1);
	verify_l_bad(i, java.lang.Integer.MIN_VALUE, 0, -1);
    }

    static void marshal_ul(String selfNode, String peerNode,
			   String cookie, String serverName)
	throws java.lang.Exception
    {
	m._iStub i = new m._iStub(selfNode, peerNode, cookie, serverName);
	// Just warming up..
	verify_ul(i, 3, 2, 1);
	verify_ul(i, 5, 4, 3);
	// The small integer border
	verify_ul(i, 255, 0, 1);
	verify_ul(i, 256, 0, 1);
	// The integer border
	verify_ul(i, (1<<26)-1, 0, 1);
	verify_ul(i, 1<<26, 0, 1);
	verify_ul(i, (1<<27)-1, 0, 1);
	verify_ul(i, 1<<27, 0, 1);
	// Java int border
	verify_ul(i, java.lang.Integer.MAX_VALUE, 0, 1);
	verify_ul(i, java.lang.Integer.MIN_VALUE, 0, 1);
	verify_ul(i, -1, 0, 1);
	verify_ul(i, java.lang.Integer.MAX_VALUE, 
		  java.lang.Integer.MIN_VALUE, 1);
	// Impossible decodes
	verify_ul_bad(i, -1, -1, 1);
    }

    static void marshal_s(String selfNode, String peerNode,
			  String cookie, String serverName)
	throws java.lang.Exception
    {
	m._iStub i = new m._iStub(selfNode, peerNode, cookie, serverName);
	// Just warming up..
	verify_s(i, 3, 2, 1);
	verify_s(i, 5, 4, 3);
	verify_s(i, -128, 0, 1);
	// The small integer border
	verify_s(i, 255, 0, 1);
	verify_s(i, 256, 0, 1);
	// Java short border
	verify_s(i, java.lang.Short.MAX_VALUE, 0, 1);
	verify_s(i, java.lang.Short.MIN_VALUE, 0, 1);
	// Impossible decodes
	verify_s_bad(i, java.lang.Short.MAX_VALUE, -1, 1);
	verify_s_bad(i, java.lang.Short.MIN_VALUE, 1, 1);
	verify_s_bad(i, java.lang.Short.MIN_VALUE, 0, -1);
    }

    static void marshal_us(String selfNode, String peerNode,
			   String cookie, String serverName)
	throws java.lang.Exception
    {
	m._iStub i = new m._iStub(selfNode, peerNode, cookie, serverName);
	// Just warming up..
	verify_us(i, 3, 2, 1);
	verify_us(i, 5, 4, 3);
	// The small integer border
	verify_us(i, 255, 0, 1);
	verify_us(i, 256, 0, 1);
	// Java short border
	verify_us(i, java.lang.Short.MAX_VALUE, 0, 1);
	verify_us(i, java.lang.Short.MIN_VALUE, 0, 1);
	verify_us(i, -1, 0, 1);
	verify_us(i, java.lang.Short.MAX_VALUE, 
		  java.lang.Short.MIN_VALUE, 1);
	// Impossible decodes
	verify_us_bad(i, -1, -1, 1);
    }

    static void marshal_c(String selfNode, String peerNode,
			   String cookie, String serverName)
	throws java.lang.Exception
    {
	m._iStub i = new m._iStub(selfNode, peerNode, cookie, serverName);
	// Just warming up..
	verify_c(i, '\3', '\2', 1);
	verify_c(i, '\5', '\4', 3);
	// The small integer border
	verify_c(i, '\u00FF', '\0', 1);
	verify_c(i, '\u0100', '\0', 1);
	// Java char border
	verify_c(i, java.lang.Character.MAX_VALUE, '\0', 1);
	verify_c(i, java.lang.Character.MIN_VALUE, '\0', 1);
	verify_c(i, java.lang.Character.MAX_VALUE, 
		  java.lang.Character.MIN_VALUE, 1);
	// Impossible decodes
	verify_c_bad(i, '\u8000', '\0', 2);
    }

    static void marshal_wc(String selfNode, String peerNode,
			   String cookie, String serverName)
	throws java.lang.Exception
    {
	m._iStub i = new m._iStub(selfNode, peerNode, cookie, serverName);
	// Just warming up..
	verify_wc(i, '\3', '\2', 1);
	verify_wc(i, '\5', '\4', 3);
	// The small integer border
	verify_wc(i, '\u00FF', '\0', 1);
	verify_wc(i, '\u0100', '\0', 1);
	// Java char border
	verify_wc(i, java.lang.Character.MAX_VALUE, '\0', 1);
	verify_wc(i, java.lang.Character.MIN_VALUE, '\0', 1);
	verify_wc(i, java.lang.Character.MAX_VALUE, 
		  java.lang.Character.MIN_VALUE, 1);
	// Impossible decodes
	verify_c_bad(i, '\u8000', '\0', 2);
    }

    static void marshal_str(String selfNode, String peerNode,
			    String cookie, String serverName)
	throws java.lang.Exception
    {
	m._iStub i = new m._iStub(selfNode, peerNode, cookie, serverName);
	// Just warming up..
	verify_str(i, 100, 100);
	verify_str(i, 100, 1);
	// Erlang string border
	verify_str(i, 65535, 1);
	verify_str(i, 2, 65535);
	// Erlang string border out
	verify_str(i, 65536, 1);
	verify_str(i, 65536, 65536);
    }

    static void marshal_any_3(String selfNode, String peerNode,
			      String cookie, String serverName)
	throws java.lang.Exception
    {
	m._iStub i = new m._iStub(selfNode, peerNode, cookie, serverName);
	com.ericsson.otp.ic.Any x = new com.ericsson.otp.ic.Any();
	com.ericsson.otp.ic.Any y = new com.ericsson.otp.ic.Any();
	com.ericsson.otp.ic.Any z = new com.ericsson.otp.ic.Any();
	
	x.insert_longlong(java.lang.Long.MAX_VALUE);
	y.insert_longlong(1L);
	z.insert_longlong(java.lang.Long.MAX_VALUE-1L);
	System.out.println("verify_any_3 longlong max");
	verify_any_3(i, x, y, 1, z);
	
	x.insert_longlong(java.lang.Long.MIN_VALUE);
	y.insert_longlong(-1L);
	z.insert_longlong(java.lang.Long.MIN_VALUE+1L);
	System.out.println("verify_any_3 longlong min");
	verify_any_3(i, x, y, 1, z);
	
	x.insert_ulonglong(-1L);
	y.insert_longlong(1L);
	z.insert_ulonglong(-2L);
	System.out.println("verify_any_3 ulonglong max");
	verify_any_3(i, x, y, 1, z);
	
	x.insert_ulonglong(0L);
	y.insert_longlong(-1L);
	z.insert_ulonglong(1L);
	System.out.println("verify_any_3 ulonglong min");
	verify_any_3(i, x, y, 1, z);
	
	x.insert_long(java.lang.Integer.MAX_VALUE);
	y.insert_long(1);
	z.insert_long(java.lang.Integer.MAX_VALUE-1);
	System.out.println("verify_any_3 long max");
	verify_any_3(i, x, y, 1, z);
	
	x.insert_long(java.lang.Integer.MIN_VALUE);
	y.insert_long(-1);
	z.insert_long(java.lang.Integer.MIN_VALUE+1);
	System.out.println("verify_any_3 long min");
	verify_any_3(i, x, y, 1, z);
	
	x.insert_ulong(-1);
	y.insert_long(1);
	z.insert_ulong(-2);
	System.out.println("verify_any_3 ulong max");
	verify_any_3(i, x, y, 1, z);
	
	x.insert_ulong(0);
	y.insert_long(-1);
	z.insert_ulong(1);
	System.out.println("verify_any_3 ulong min");
	verify_any_3(i, x, y, 1, z);
	
	x.insert_short(java.lang.Short.MAX_VALUE);
	y.insert_short((short)1);
	z.insert_short((short)(java.lang.Short.MAX_VALUE-1));
	System.out.println("verify_any_3 short max");
	verify_any_3(i, x, y, 1, z);
	
	x.insert_short(java.lang.Short.MIN_VALUE);
	y.insert_short((short)-1);
	z.insert_short((short)(java.lang.Short.MIN_VALUE+1));
	System.out.println("verify_any_3 short min");
	verify_any_3(i, x, y, 1, z);
	
	x.insert_ushort((short)-1);
	y.insert_short((short)1);
	z.insert_ushort((short)-2);
	System.out.println("verify_any_3 ushort max");
	verify_any_3(i, x, y, 1, z);
	
	x.insert_ushort((short)0);
	y.insert_short((short)-1);
	z.insert_ushort((short)1);
	System.out.println("verify_any_3 ushort min");
	verify_any_3(i, x, y, 1, z);
	
	x.insert_char('\377');
	y.insert_char('\1');
	z.insert_char('\376');
	System.out.println("verify_any_3 char max");
	verify_any_3(i, x, y, 1, z);
	
	x.insert_wchar('\uFFFF');
	y.insert_wchar('\u0001');
	z.insert_wchar('\uFFFE');
	System.out.println("verify_any_3 char max");
	verify_any_3(i, x, y, 1, z);
    }

    static void marshal_any_2(String selfNode, String peerNode,
			      String cookie, String serverName)
	throws java.lang.Exception
    {
	m._iStub i = new m._iStub(selfNode, peerNode, cookie, serverName);
	m.s s = new m.s();
	com.ericsson.otp.ic.Any a = new com.ericsson.otp.ic.Any();
	//
	s.ull_x = -1L;
	s.ll_x = java.lang.Long.MAX_VALUE;
	s.ll_y = 1L;
	s.ull_z = -2L;
	s.ll_z = java.lang.Long.MAX_VALUE-1L;
	//
	s.ul_x = -1;
	s.l_x = java.lang.Integer.MAX_VALUE;
	s.l_y = 1;
	s.ul_z = -2;
	s.l_z = java.lang.Integer.MAX_VALUE-1;
	//
	s.us_x = (short)-1;
	s.s_x = java.lang.Short.MAX_VALUE;
	s.s_y = (short)1;
	s.us_z = (short)-2;
	s.s_z = (short)(java.lang.Short.MAX_VALUE-1);
	//
	s.c_x = '\377';
	s.c_y = '\1';
	s.c_z = '\376';
	s.wc_x = '\uFFFF';
	s.wc_y = '\u0001';
	s.wc_z = '\uFFFE';
	m.sHelper.insert(a, s);
	verify_any_2(i, a, 1);
	
	s.ull_x = 0L;
	s.ll_x = java.lang.Long.MIN_VALUE;
	s.ll_y = -1L;
	s.ull_z = 1L;
	s.ll_z = java.lang.Long.MIN_VALUE+1L;
	//
	s.ul_x = 0;
	s.l_x = java.lang.Integer.MIN_VALUE;
	s.l_y = -1;
	s.ul_z = 1;
	s.l_z = java.lang.Integer.MIN_VALUE+1;
	//
	s.us_x = (short)0;
	s.s_x = java.lang.Short.MIN_VALUE;
	s.s_y = (short)-1;
	s.us_z = (short)1;
	s.s_z = (short)(java.lang.Short.MIN_VALUE+1);
	//
	s.c_x = '\0';
	s.c_y = '\0';
	s.c_z = '\0';
	s.wc_x = '\u0000';
	s.wc_y = '\u0000';
	s.wc_z = '\u0000';
	m.sHelper.insert(a, s);
	verify_any_2(i, a, 1);
    }


    static void verify_ll(m._iStub i, long x, long y, int b) 
	throws java.lang.Exception 
    {
	m.s a = new m.s();
	System.out.println("verify_ll "+a);
	a.ll_x = x;
	a.ll_y = y;
	long expected = (x - y)*(short)b;
	long result = i.marshal_ll(a, (short)b);
	if (result == expected) {
	    System.out.println("verify_ll("+x+", "+y+", "+b+") => "
			       +result);
	} else {
	    System.out.println("verify_ll("+x+", "+y+", "+b+") => "
			       +result+" != "+expected);
	    System.exit(4);
	}
    }

    static void verify_ull(m._iStub i, long x, long y, int b) 
	throws java.lang.Exception 
    {
	m.s a = new m.s();
	a.ull_x = x;
	a.ll_y = y;
	long expected = (x - y)*(short)b;
	long result = i.marshal_ull(a, (short)b);
	if (result == expected) {
	    System.out.println("verify_ull("+x+", "+y+", "+b+") => "
			       +result);
	} else {
	    System.out.println("verify_ull("+x+", "+y+", "+b+") => "
			       +result+" != "+expected);
	    System.exit(4);
	}
    }

    static void verify_l(m._iStub i, int x, int y, int b) 
	throws java.lang.Exception 
    {
	m.s a = new m.s();
	a.l_x = x;
	a.l_y = y;
	int expected = (x - y)*(short)b;
	int result = i.marshal_l(a, (short)b);
	if (result == expected) {
	    System.out.println("verify_l("+x+", "+y+", "+b+") => "
			       +result);
	} else {
	    System.out.println("verify_l("+x+", "+y+", "+b+") => "
			       +result+" != "+expected);
	    System.exit(4);
	}
    }

    static void verify_ul(m._iStub i, int x, int y, int b) 
	throws java.lang.Exception 
    {
	m.s a = new m.s();
	a.ul_x = x;
	a.l_y = y;
	int expected = (x - y)*(short)b;
	int result = i.marshal_ul(a, (short)b);
	if (result == expected) {
	    System.out.println("verify_ul("+x+", "+y+", "+b+") => "
			       +result);
	} else {
	    System.out.println("verify_ul("+x+", "+y+", "+b+") => "
			       +result+" != "+expected);
	    System.exit(4);
	}
    }

    static void verify_s(m._iStub i, int x, int y, int b) 
	throws java.lang.Exception 
    {
	m.s a = new m.s();
	a.s_x = (short)x;
	a.s_y = (short)y;
	short expected = (short)((x - y)*(short)b);
	short result = i.marshal_s(a, (short)b);
	if (result == expected) {
	    System.out.println("verify_s("+x+", "+y+", "+b+") => "
			       +result);
	} else {
	    System.out.println("verify_s("+x+", "+y+", "+b+") => "
			       +result+" != "+expected);
	    System.exit(4);
	}
    }

    static void verify_us(m._iStub i, int x, int y, int b) 
	throws java.lang.Exception 
    {
	m.s a = new m.s();
	a.us_x = (short)x;
	a.s_y = (short)y;
	short expected = (short)((x - y)*(short)b);
	short result = i.marshal_us(a, (short)b);
	if (result == expected) {
	    System.out.println("verify_us("+x+", "+y+", "+b+") => "
			       +result);
	} else {
	    System.out.println("verify_us("+x+", "+y+", "+b+") => "
			       +result+" != "+expected);
	    System.exit(4);
	}
    }

    static void verify_c(m._iStub i, char x, char y, int b) 
	throws java.lang.Exception 
    {
	m.s a = new m.s();
	a.c_x = x;
	a.c_y = y;
	char expected = (char)(((int)x - (int)y)*(short)b);
	char result = i.marshal_c(a, (short)b);
	if (result == expected) {
	    System.out.println("verify_c("+x+", "+y+", "+b+") => "
			       +result);
	} else {
	    System.out.println("verify_c("+x+", "+y+", "+b+") => "
			       +result+" != "+expected);
	    System.exit(4);
	}
    }

    static void verify_wc(m._iStub i, char x, char y, int b) 
	throws java.lang.Exception 
    {
	m.s a = new m.s();
	a.wc_x = x;
	a.wc_y = y;
	char expected = (char)(((int)x - (int)y)*(short)b);
	char result = i.marshal_wc(a, (short)b);
	if (result == expected) {
	    System.out.println("verify_wc("+x+", "+y+", "+b+") => "
			       +result);
	} else {
	    System.out.println("verify_wc("+x+", "+y+", "+b+") => "
			       +result+" != "+expected);
	    System.exit(4);
	}
    }

    static void verify_str(m._iStub i, int a_len, int b_len) 
	throws java.lang.Exception 
    {
	String a = mk_str(a_len);
	String b = mk_str(b_len);
	String expected = a + b;
	String result = i.strcat(a, b);
	if (result.equals(expected)) {
	    System.out.println("verify_str(\""+a+"\", \""+b+"\") => \""
			       +result+"\"");
	} else {
	    System.out.println("verify_str(\""+a+"\", \""+b+"\") => \""
			       +result+"\" != \""+expected.length()+"\"");
	    System.exit(4);
	}
    }

    static String mk_str(int len) 
	throws StringIndexOutOfBoundsException
    {
	StringBuffer s = new StringBuffer();
	// 17 characters is prime relative all bases of two - on purpose
	do s.append("qwertyuiopasdfghj"); while (s.length() < len);
	return s.substring(0, len);
    }

    static void verify_any_3(m._iStub i, 
			   com.ericsson.otp.ic.Any x, 
			   com.ericsson.otp.ic.Any y, 
			   int b, 
			   com.ericsson.otp.ic.Any expected) 
	throws java.lang.Exception 
    {
	com.ericsson.otp.ic.Any result = i.marshal_any_3(x, y, (short)b);
	if (! expected.equal(result)) {
	    System.exit(4);
	}
    }

    static void verify_any_2(m._iStub i, com.ericsson.otp.ic.Any a, int b)
	throws java.lang.Exception 
    {
	com.ericsson.otp.ic.Any result = i.marshal_any_2(a, (short)b);
	if (! a.equal(result)) {
	    System.exit(4);
	}
    }



    static void verify_ll_bad(m._iStub i, long x, long y, int b)
	throws java.lang.Exception
    {
	try {
	    verify_ll(i, x, y, b);
	    System.out.println("Expected exception missing!");
	    System.exit(5);
	} catch (com.ericsson.otp.erlang.OtpErlangDecodeException e) {
	    System.out.println("Expected exception: "+e);
	}
    }

    static void verify_ull_bad(m._iStub i, long x, long y, int b)
	throws java.lang.Exception
    {
	try {
	    verify_ull(i, x, y, b);
	    System.out.println("Expected exception missing!");
	    System.exit(5);
	} catch (com.ericsson.otp.erlang.OtpErlangDecodeException e) {
	    System.out.println("Expected exception: "+e);
	}
    }

    static void verify_l_bad(m._iStub i, int x, int y, int b)
	throws java.lang.Exception
    {
	try {
	    verify_l(i, x, y, b);
	    System.out.println("Expected exception missing!");
	    System.exit(5);
	} catch (com.ericsson.otp.erlang.OtpErlangDecodeException e) {
	    System.out.println("Expected exception: "+e);
	}
    }

    static void verify_ul_bad(m._iStub i, int x, int y, int b)
	throws java.lang.Exception
    {
	try {
	    verify_ul(i, x, y, b);
	    System.out.println("Expected exception missing!");
	    System.exit(5);
	} catch (com.ericsson.otp.erlang.OtpErlangDecodeException e) {
	    System.out.println("Expected exception: "+e);
	}
    }

    static void verify_s_bad(m._iStub i, int x, int y, int b)
	throws java.lang.Exception
    {
	try {
	    verify_s(i, x, y, b);
	    System.out.println("Expected exception missing!");
	    System.exit(5);
	} catch (com.ericsson.otp.erlang.OtpErlangDecodeException e) {
	    System.out.println("Expected exception: "+e);
	}
    }

    static void verify_us_bad(m._iStub i, int x, int y, int b)
	throws java.lang.Exception
    {
	try {
	    verify_us(i, x, y, b);
	    System.out.println("Expected exception missing!");
	    System.exit(5);
	} catch (com.ericsson.otp.erlang.OtpErlangDecodeException e) {
	    System.out.println("Expected exception: "+e);
	}
    }

    static void verify_c_bad(m._iStub i, char x, char y, int b)
	throws java.lang.Exception
    {
	try {
	    verify_c(i, x, y, b);
	    System.out.println("Expected exception missing!");
	    System.exit(5);
	} catch (com.ericsson.otp.erlang.OtpErlangDecodeException e) {
	    System.out.println("Expected exception: "+e);
	}
    }

    static void verify_wc_bad(m._iStub i, char x, char y, int b)
	throws java.lang.Exception
    {
	try {
	    verify_wc(i, x, y, b);
	    System.out.println("Expected exception missing!");
	    System.exit(5);
	} catch (com.ericsson.otp.erlang.OtpErlangDecodeException e) {
	    System.out.println("Expected exception: "+e);
	}
    }

}
