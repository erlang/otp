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

Pid class mapps the built-in erlang type pid, a process identity.

**/ 


final public class Pid extends com.ericsson.otp.erlang.OtpErlangPid {
  
  public Pid(com.ericsson.otp.erlang.OtpSelf self) {
    super(self);
  }

  public Pid(com.ericsson.otp.erlang.OtpInputStream buf) 
    throws com.ericsson.otp.erlang.OtpErlangDecodeException {
      super(buf);
  }

 
  public Pid(String node, int id, int serial, int creation) {
    super(node,id,serial,creation);
  }


  /**
    Comparisson method for Pid.
    @return true if the input Pid value equals the value of the current object, false otherwize
    **/
  public boolean equal(Pid _pid) {
    return super.equals(_pid);
  }

}
