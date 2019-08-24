/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2015. All Rights Reserved.
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
