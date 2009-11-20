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

Ref class mapps the built-in erlang type Ref, a message reference.

**/ 

final public class Ref extends com.ericsson.otp.erlang.OtpErlangRef {
   
  public Ref(com.ericsson.otp.erlang.OtpSelf self) {
    super(self);
  }
  
  
  public Ref(com.ericsson.otp.erlang.OtpInputStream buf) 
    throws com.ericsson.otp.erlang.OtpErlangDecodeException {
      super(buf);
  }
  
  /** 
    Old style Ref costructor. Costructs an Ref that coresponds to the
    old erlang Ref type.
    **/
  public Ref(String node, int id, int creation) {
    super(node,id,creation);
  }
  
  public Ref(String node, int[] ids, int creation) {
    super(node,ids,creation);
  }

  /**
    Comparisson method for Ref.
    @return true if the input Ref value equals the value of the current object, false otherwize
    **/
  public boolean equal(Ref _ref) {
    return super.equals(_ref);
  }

}
