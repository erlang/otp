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
 * Exception raised when a node attempts to establish a communication channel
 * when it is not authorized to do so, or when a node sends a message containing
 * an invalid cookie on an established channel.
 *
 * @see OtpConnection
 */
public class OtpAuthException extends OtpException {
    private static final long serialVersionUID = 1L;

    /**
     * Provides a detailed message.
     */
    public OtpAuthException(final String s) {
        super(s);
    }
}
