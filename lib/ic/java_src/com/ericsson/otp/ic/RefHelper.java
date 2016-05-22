/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1999-2016. All Rights Reserved.
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
package com.ericsson.otp.ic;

/**
  Helper class for Ref.
  **/

public class RefHelper {

   // constructors
   private RefHelper() {}

   // methods
  /**
    Marshal method for the Ref class, encodes the Ref object to the output stream.
    **/
   public static void marshal(com.ericsson.otp.erlang.OtpOutputStream _out, Ref _value)
     throws java.lang.Exception {

     _out.write_ref(_value.node(),_value.id(),_value.creation());
   }
  
  /**
    Unmarshal method for the Ref class, decodes a Ref object from the stream.
    @return Ref, read from the input stream
    **/
   public static Ref unmarshal(com.ericsson.otp.erlang.OtpInputStream _in)
     throws java.lang.Exception {

       // Double job is done here, there should be 
       // a function returning a Ref instead of an
       // OtpErlangRef
       com.ericsson.otp.erlang.OtpErlangRef oer = _in.read_ref(); 
       
       if (oer.isNewRef())
	 return new Ref(oer.node(),oer.ids(),oer.creation());
       else
	 return new Ref(oer.node(),oer.id(),oer.creation());      
   }
  
  /**
    Standard method that returns the interface repository identity.
    @return String containing the interface repository identity of Ref
    **/
   public static String id() {
      return "IDL:com/ericsson/otp/ic/Ref:1.0";
   }

  /**
    Standard method that returns the Ref class name.
    @return String containing the class name of Ref
    **/
   public static String name() {
      return "Ref";
   }
  
  /**
    Holds the TypeCode
    **/
  private static com.ericsson.otp.ic.TypeCode _tc;
  
  /**
    Standard TypeCode accessor method.
    @return the TypeCode for Ref
    **/
  synchronized public static com.ericsson.otp.ic.TypeCode type() {

     if (_tc != null)
       return _tc;

     com.ericsson.otp.ic.TypeCode _tc0 =
       new com.ericsson.otp.ic.TypeCode();
     _tc0.kind(com.ericsson.otp.ic.TCKind.tk_struct);
     _tc0.id("IDL:com/ericsson/otp/ic/Ref:1.0");
     _tc0.name("Ref");
     _tc0.member_count(3);
     _tc0.member_name(0,"node");
     com.ericsson.otp.ic.TypeCode _tc1 =
       new com.ericsson.otp.ic.TypeCode();
     _tc1.kind(com.ericsson.otp.ic.TCKind.tk_string);
     _tc1.length(256);
     _tc0.member_type(0,_tc1);
     _tc0.member_name(1,"id");
     com.ericsson.otp.ic.TypeCode _tc2 =
       new com.ericsson.otp.ic.TypeCode();
     _tc2.kind(com.ericsson.otp.ic.TCKind.tk_ulong);
     _tc0.member_type(1,_tc2);
     _tc0.member_name(2,"creation");
     com.ericsson.otp.ic.TypeCode _tc3 =
       new com.ericsson.otp.ic.TypeCode();
     _tc3.kind(com.ericsson.otp.ic.TCKind.tk_ulong);
     _tc0.member_type(2,_tc3);

     _tc = _tc0;

     return _tc0;
   }

  /**
    Standard method for inserting a Ref to an Any.
    **/
   public static void insert(com.ericsson.otp.ic.Any _any, Ref _this)
     throws java.lang.Exception {

     com.ericsson.otp.erlang.OtpOutputStream _os = 
       new com.ericsson.otp.erlang.OtpOutputStream();

     _any.type(type());
     marshal(_os, _this);
     _any.insert_Streamable(_os);
   }

  /**
    Standard method for extracting a Ref from an Any.
    @return Ref, the value found in an Any contained stream.
    **/
   public static Ref extract(com.ericsson.otp.ic.Any _any)
     throws java.lang.Exception {

     return unmarshal(_any.extract_Streamable());
   }

}
