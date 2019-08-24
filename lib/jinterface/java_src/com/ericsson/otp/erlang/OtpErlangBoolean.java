/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2000-2016. All Rights Reserved.
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
 */
package com.ericsson.otp.erlang;

/**
 * Provides a Java representation of Erlang booleans, which are special cases of
 * atoms with values 'true' and 'false'.
 */
public class OtpErlangBoolean extends OtpErlangAtom {
    // don't change this!
    static final long serialVersionUID = 1087178844844988393L;

    /**
     * Create a boolean from the given value
     *
     * @param t
     *            the boolean value to represent as an atom.
     */
    public OtpErlangBoolean(final boolean t) {
        super(t);
    }

    /**
     * Create a boolean from a stream containing an atom encoded in Erlang
     * external format. The value of the boolean will be true if the atom
     * represented by the stream is "true" without regard to case. For other
     * atom values, the boolean will have the value false.
     *
     * @exception OtpErlangDecodeException
     *                if the buffer does not contain a valid external
     *                representation of an Erlang atom.
     */
    public OtpErlangBoolean(final OtpInputStream buf)
            throws OtpErlangDecodeException {
        super(buf);
    }
}
