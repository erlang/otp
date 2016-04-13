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

Port class mapps the built-in erlang type port, a process port.

**/ 

final public class Port extends com.ericsson.otp.erlang.OtpErlangPort {
  
  public Port(com.ericsson.otp.erlang.OtpInputStream buf) 
    throws com.ericsson.otp.erlang.OtpErlangDecodeException {
      super(buf);
  }
  
  public Port(String node, int id, int creation) {
    super(node,id,creation);
  }

   /**
    Comparisson method for Port.
    @return true if the input Port value equals the value of the current object, false otherwize
    **/
  public boolean equal(Port _port) {
    return super.equals(_port);
  }


}
