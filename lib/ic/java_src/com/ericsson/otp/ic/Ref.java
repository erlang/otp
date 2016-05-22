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
