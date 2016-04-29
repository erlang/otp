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
  Helper class for Term.
  **/

public class TermHelper {

   // Constructors
   private TermHelper() {}
 
   // Methods
  /**
    Marshal method for the Term class, encodes the Term object to the output stream.
    **/
   public static void marshal(com.ericsson.otp.erlang.OtpOutputStream _out, Term _any)
     throws java.lang.Exception {
             
       _any.write_value(_out);
   }

  /**
    Unmarshal method for the Term class, decodes a Term object from the stream.
    @return Term, read from the input stream
    **/
  public static Term unmarshal(com.ericsson.otp.erlang.OtpInputStream _in)
    throws java.lang.Exception {

      Term _value = new Term();
      
      int tag = _in.peek();
      if (tag == com.ericsson.otp.erlang.OtpExternal.versionTag) {
	_in.read1();
	tag = _in.peek();      
      }    
      _value.tag = tag;


      // Allways save the object in OtpErlangObject form
      _in.mark(0);
      com.ericsson.otp.erlang.OtpErlangObject _obj = _in.read_any();
      _value.insert_Object(_obj);
      
      switch (tag) {
      case com.ericsson.otp.erlang.OtpExternal.smallIntTag:
      case com.ericsson.otp.erlang.OtpExternal.intTag:
      case com.ericsson.otp.erlang.OtpExternal.smallBigTag:
	_in.reset();
	_value.longV = _in.read_long();
	break;

      case com.ericsson.otp.erlang.OtpExternal.atomTag:
      case com.ericsson.otp.erlang.OtpExternal.atomUtf8Tag:
      case com.ericsson.otp.erlang.OtpExternal.smallAtomUtf8Tag:
	_in.reset();
	_value.atomV = _in.read_atom();
	break;

      case com.ericsson.otp.erlang.OtpExternal.floatTag:
	_in.reset();
	_value.doubleV = _in.read_double();
	break;

      case com.ericsson.otp.erlang.OtpExternal.refTag:
      case com.ericsson.otp.erlang.OtpExternal.newRefTag:
	_in.reset();
	com.ericsson.otp.erlang.OtpErlangRef _eref = 
	  _in.read_ref();  
	
	if (_eref.isNewRef())
	  _value.RefV = new Ref(_eref.node(),_eref.ids(),_eref.creation());
	else
	  _value.RefV = new Ref(_eref.node(),_eref.id(),_eref.creation());      

	break;

      case com.ericsson.otp.erlang.OtpExternal.portTag:
	_in.reset();
	com.ericsson.otp.erlang.OtpErlangPort _eport = 
	  _in.read_port(); 
	
	_value.PortV = new Port(_eport.node(),_eport.id(),_eport.creation());  
	break;

      case com.ericsson.otp.erlang.OtpExternal.pidTag:
	_in.reset();
	com.ericsson.otp.erlang.OtpErlangPid _epid = 
	  _in.read_pid(); 
	
	_value.PidV = new Pid(_epid.node(),_epid.id(),_epid.serial(),_epid.creation());  
	break;

      case com.ericsson.otp.erlang.OtpExternal.stringTag:
	_in.reset();
	_value.stringV = _in.read_string();
	break;

      case com.ericsson.otp.erlang.OtpExternal.listTag:
      case com.ericsson.otp.erlang.OtpExternal.nilTag:
      case com.ericsson.otp.erlang.OtpExternal.smallTupleTag:
      case com.ericsson.otp.erlang.OtpExternal.largeTupleTag:
      case com.ericsson.otp.erlang.OtpExternal.binTag:

	com.ericsson.otp.erlang.OtpOutputStream _os = 
	  new com.ericsson.otp.erlang.OtpOutputStream();

	_obj.encode(_os);
	_value.insert_Streamable(_os);
	break;
	
      case com.ericsson.otp.erlang.OtpExternal.largeBigTag:
      default:
	throw new com.ericsson.otp.erlang.OtpErlangDecodeException("Uknown data type: " + tag);
      }
      
      return _value;
  }
  
}



