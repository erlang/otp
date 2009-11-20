/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2000-2009. All Rights Reserved.
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
 */
package com.ericsson.otp.erlang;

import java.io.Serializable;

/**
 * Provides a Java representation of Erlang floats and doubles.
 */
public class OtpErlangFloat extends OtpErlangDouble implements Serializable,
	Cloneable {
    // don't change this!
    static final long serialVersionUID = -2231546377289456934L;

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
     *                the stream containing the encoded value.
     * 
     * @exception OtpErlangDecodeException
     *                    if the buffer does not contain a valid external
     *                    representation of an Erlang float.
     * 
     * @exception OtpErlangRangeException
     *                    if the value cannot be represented as a Java float.
     */
    public OtpErlangFloat(final OtpInputStream buf)
	    throws OtpErlangDecodeException, OtpErlangRangeException {
	super(buf);

	final float f = floatValue();
    }
}
