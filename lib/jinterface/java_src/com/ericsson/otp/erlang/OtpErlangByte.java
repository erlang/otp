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
 * Provides a Java representation of Erlang integral types.
 */
public class OtpErlangByte extends OtpErlangLong {
    // don't change this!
    private static final long serialVersionUID = 5778019796466613446L;

    /**
     * Create an Erlang integer from the given value.
     *
     * @param b
     *            the byte value to use.
     */
    public OtpErlangByte(final byte b) {
        super(b);
    }

    /**
     * Create an Erlang integer from a stream containing an integer encoded in
     * Erlang external format.
     *
     * @param buf
     *            the stream containing the encoded value.
     *
     * @exception OtpErlangDecodeException
     *                if the buffer does not contain a valid external
     *                representation of an Erlang integer.
     *
     * @exception OtpErlangRangeException
     *                if the value is too large to be represented as a byte.
     */
    public OtpErlangByte(final OtpInputStream buf)
            throws OtpErlangRangeException, OtpErlangDecodeException {
        super(buf);

        byteValue();
    }
}
