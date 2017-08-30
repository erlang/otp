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
 * Provides a Java representation of Erlang floats and doubles.
 */
public class OtpErlangFloat extends OtpErlangDouble {
    // don't change this!
    private static final long serialVersionUID = -2231546377289456934L;

    /**
     * Create an Erlang float from the given float value.
     */
    public OtpErlangFloat(final float f) {
        super(f);
    }

    /**
     * Create an Erlang float from a stream containing a float encoded in Erlang
     * external format.
     *
     * @param buf
     *            the stream containing the encoded value.
     *
     * @exception OtpErlangDecodeException
     *                if the buffer does not contain a valid external
     *                representation of an Erlang float.
     *
     * @exception OtpErlangRangeException
     *                if the value cannot be represented as a Java float.
     */
    public OtpErlangFloat(final OtpInputStream buf)
            throws OtpErlangDecodeException, OtpErlangRangeException {
        super(buf);

        floatValue();
    }
}
