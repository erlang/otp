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
