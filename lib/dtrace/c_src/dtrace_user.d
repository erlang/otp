/*
 * %CopyrightBegin%
 *
 * Copyright Scott Lystig Fritchie 2011.
 * All Rights Reserved.
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

provider erlang {
    /**
     * Send a single string to a probe.
     *
     * @param NUL-terminated string
     */
    probe user_trace__s1(char* message);

    /**
     * Multi-purpose probe: up to 4 NUL-terminated strings and 4
     * 64-bit integer arguments.
     *
     * @param proc, the PID (string form) of the sending process
     * @param user_tag, the user tag of the sender
     * @param i1, integer
     * @param i2, integer
     * @param i3, integer
     * @param i4, integer
     * @param s1, string/iolist. D's arg6 is NULL if not given by Erlang
     * @param s2, string/iolist. D's arg7 is NULL if not given by Erlang
     * @param s3, string/iolist. D's arg8 is NULL if not given by Erlang
     * @param s4, string/iolist. D's arg9 is NULL if not given by Erlang
     */
    probe user_trace__i4s4(char *proc, char *user_tag,
                           int i1, int i2, int i3, int i4,
                           char *s1, char *s2, char *s3, char *s4);
};

#pragma D attributes Evolving/Evolving/Common provider erlang provider
#pragma D attributes Private/Private/Common provider erlang module
#pragma D attributes Private/Private/Common provider erlang function
#pragma D attributes Evolving/Evolving/Common provider erlang name
#pragma D attributes Evolving/Evolving/Common provider erlang args
