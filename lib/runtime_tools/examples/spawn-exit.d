/* example usage: dtrace -q -s /path/to/spawn-exit.d */
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

erlang*:::process-spawn
{
    printf("pid %s mfa %s\n", copyinstr(arg0), copyinstr(arg1));
}

erlang*:::process-exit
{
    printf("pid %s reason %s\n", copyinstr(arg0), copyinstr(arg1));
}

erlang*:::process-exit_signal
{
    printf("sender %s -> pid %s reason %s\n",
	   copyinstr(arg0), copyinstr(arg1), copyinstr(arg2));
}

erlang*:::process-exit_signal-remote
{
    printf("sender %s -> node %s pid %s reason %s\n",
	   copyinstr(arg0), copyinstr(arg1), copyinstr(arg2), copyinstr(arg3));
}
