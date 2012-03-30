/* example usage: dtrace -q -s /path/to/function-calls.d */
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

erlang*:::local-function-entry
{
    printf("pid %s enter (local)  %s depth %d\n",
	   copyinstr(arg0), copyinstr(arg1), arg2);
}

erlang*:::global-function-entry
{
    printf("pid %s enter (global)  %s depth %d\n",
	   copyinstr(arg0), copyinstr(arg1), arg2);
}

erlang*:::function-return
{
    printf("pid %s return %s depth %d\n",
	   copyinstr(arg0), copyinstr(arg1), arg2);
}

erlang*:::bif-entry
{
    printf("pid %s BIF entry  mfa %s\n", copyinstr(arg0), copyinstr(arg1));
}

erlang*:::bif-return
{
    printf("pid %s BIF return mfa %s\n", copyinstr(arg0), copyinstr(arg1));
}

erlang*:::nif-entry
{
    printf("pid %s NIF entry  mfa %s\n", copyinstr(arg0), copyinstr(arg1));
}

erlang*:::nif-return
{
    printf("pid %s NIF return mfa %s\n", copyinstr(arg0), copyinstr(arg1));
}
