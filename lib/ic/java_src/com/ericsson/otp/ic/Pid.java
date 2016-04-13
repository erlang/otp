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
