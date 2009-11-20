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

Holder class for Any, according to OMG-IDL java mapping.
<p>Instead for _write,_read methods, the methods _marshal respective 
_unmarshal are used to denote the implementation difference.

**/ 

final public class AnyHolder {
  
  // Instance variables
  public Any value;
  
  // Constructors
  public AnyHolder() {}
  
  public AnyHolder(Any initial) {
    value = initial;
  }
  
  // Methods
  /**
    Marshal method for the Any class, encodes the Any object to the output stream.
    **/
  public void _marshal(com.ericsson.otp.erlang.OtpOutputStream out) 
    throws java.lang.Exception {
      AnyHelper.marshal(out, value);
  }
  
  /**
    Unmarshal method for the Any class, decodes an Any object from the stream and
    assigns it to the Holder value.
    **/
  public void _unmarshal(com.ericsson.otp.erlang.OtpInputStream in) 
    throws java.lang.Exception {
      value = AnyHelper.unmarshal(in);
  }
  
}
