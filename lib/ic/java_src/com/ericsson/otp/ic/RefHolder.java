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
  Holder class for Ref.
  **/

final public class RefHolder {
   
  /**
    Ref instance variable.
    **/
   public Ref value;

   // constructors
   public RefHolder() {}
   public RefHolder(Ref initial) {
      value = initial;
   }

   // methods
  /**
    Marshal method for the RefHolder class, encodes the Ref object value to the output stream.
    **/
   public void _marshal(com.ericsson.otp.erlang.OtpOutputStream out) throws java.lang.Exception {
      RefHelper.marshal(out, value);
   }

  /**
    Unmarshal method for the RefHolder class, decodes a Ref object from the output stream
    and assigns it to the Holder value field.
    **/
   public void _unmarshal(com.ericsson.otp.erlang.OtpInputStream in) throws java.lang.Exception {
      value = RefHelper.unmarshal(in);
   }
}
