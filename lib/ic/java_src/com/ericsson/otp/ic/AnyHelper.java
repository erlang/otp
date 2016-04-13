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



