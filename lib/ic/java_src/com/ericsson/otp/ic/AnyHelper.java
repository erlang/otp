/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1999-2009. All Rights Reserved.
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
 *
 */
package com.ericsson.otp.ic;

/**

Helper class for Any, according to OMG-IDL java mapping.
<p>Instead for write,read methods, the methods marshal respective 
unmarshal are used to denote the implementation difference.

**/  


public class AnyHelper {

   // Constructors
   private AnyHelper() {}
 
   // Methods
  /**
    Marshal method for the Any class, encodes the Any object to the output stream.
    **/
   public static void marshal(com.ericsson.otp.erlang.OtpOutputStream _out, Any _any)
     throws java.lang.Exception {
             
       TypeCode _tc = _any.type(); 
       
       _out.write_tuple_head(3);
       _out.write_atom("any");
       
       TypeCode.marshal(_out, _tc);
       _any.write_value(_out);

   }

  /**
    Unmarshal method for the Any class, decodes an Any object from the stream.
    @return Any, read from the input stream
    **/
  public static Any unmarshal(com.ericsson.otp.erlang.OtpInputStream _in)
    throws java.lang.Exception {

      Any _value;
      TypeCode _tc;
      
      _in.read_tuple_head();
      
      if ((_in.read_atom()).compareTo("any") != 0)
        throw new java.lang.Exception("");
      
      _tc = TypeCode.unmarshal(_in);
      _value = new Any(); 
      _value.read_value(_in,_tc);

      return _value;
  }
  
}



