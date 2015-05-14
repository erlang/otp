/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2015. All Rights Reserved.
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

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.Socket;

/**
 * Client-side connection-oriented transport interface.
 * 
 * @author Dmitriy Kargapolov
 */
public interface OtpTransport {

    /**
     * @see Socket#getInputStream()
     */
    public abstract InputStream getInputStream() throws IOException;

    /**
     * @see Socket#getOutputStream()
     */
    public abstract OutputStream getOutputStream() throws IOException;

    /**
     * @see Socket#close()
     */
    public abstract void close() throws IOException;

}
