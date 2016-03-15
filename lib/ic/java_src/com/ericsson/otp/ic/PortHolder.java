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
  Holder class for Port.
  **/

final public class PortHolder {

  /**
    Port instance variable.
    **/
   public Port value;

   // constructors
   public PortHolder() {}
   public PortHolder(Port initial) {
      value = initial;
   }

   // methods
  /**
    Marshal method for the PortHolder class, encodes the Port object value to the output stream.
    **/
   public void _marshal(com.ericsson.otp.erlang.OtpOutputStream out) 
     throws java.lang.Exception {
      PortHelper.marshal(out, value);
   }

  /**
    Unmarshal method for the PortHolder class, decodes a Port object from the output stream
    and assigns it to the Holder value field.
    **/
   public void _unmarshal(com.ericsson.otp.erlang.OtpInputStream in) 
     throws java.lang.Exception {
      value = PortHelper.unmarshal(in);
   }
}
