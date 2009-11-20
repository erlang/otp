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

/**
 * Base class for the other OTP erlang exception classes.
 */
public class OtpErlangException extends OtpException {
    private static final long serialVersionUID = 1L;

    /**
     * Provides no message.
     */
    public OtpErlangException() {
	super();
    }

    /**
     * Provides a detailed message.
     */
    public OtpErlangException(final String msg) {
	super(msg);
    }
}
