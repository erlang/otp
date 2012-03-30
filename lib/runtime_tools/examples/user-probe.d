/* example usage: dtrace -q -s /path/to/user-probe.d */
/*
 * %CopyrightBegin%
 *
 * Copyright Scott Lystig Fritchie 2011-2012. All Rights Reserved.
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

erlang*:::user_trace-s1
{
    printf("%s\n", copyinstr(arg0));
}

erlang*:::user_trace-i4s4
{
    printf("%s %s %d %d %d %d '%s' '%s' '%s' '%s'\n",
           copyinstr(arg0),
           arg1 == NULL ? "" : copyinstr(arg1),
           arg2, arg3, arg4, arg5,
           arg6 == NULL ? "" : copyinstr(arg6),
           arg7 == NULL ? "" : copyinstr(arg7),
           arg8 == NULL ? "" : copyinstr(arg8),
           arg9 == NULL ? "" : copyinstr(arg9));
}
