/* example usage: dtrace -q -s /path/to/messages.d */
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

BEGIN
{
    printf("\n");
    printf("NOTE: message-queue message size 4294967295 means an external\n");
    printf("      message that the code isn't smart enough to determine\n");
    printf("      the actual size.\n");
    printf("\n");
}

erlang*:::message-send
/arg3 == 0 && arg4 == 0 && arg5 == 0/
{
    printf("send:   %s -> %s: %d words\n",
           copyinstr(arg0), copyinstr(arg1), arg2);
}

erlang*:::message-send
/arg3 != 0 || arg4 != 0 || arg5 != 0/
{
    printf("send:   %s label %d token {%d,%d} -> %s: %d words\n",
           copyinstr(arg0),
           arg3, arg4, arg5,
           copyinstr(arg1), arg2);
}

/*
 * TODO:
 * Weird, on my OS X box, beam says arg6 = 0 but this script says 4294967296.
 */

erlang*:::message-send-remote
/arg4 == 0 && arg5 == 0 && (arg6 == 0 || arg6 >= 4294967296)/
{
    printf("send :  %s -> %s %s: %d words\n",
           copyinstr(arg0), copyinstr(arg1), copyinstr(arg2), arg3);
}

erlang*:::message-send-remote
/arg4 != 0 || arg5 != 0 || arg6 < 4294967296/
{
    printf("send :  %s label %d token {%d,%d} -> %s %s: %d words\n",
           copyinstr(arg0),
           arg4, arg5, arg6,
           copyinstr(arg1), copyinstr(arg2), arg3);
}

erlang*:::message-queued
/arg3 == 0 && arg4 == 0 && arg5 == 0/
{
    printf("queued: %s: %d words, queue len %d\n", copyinstr(arg0), arg1, arg2);
}

erlang*:::message-queued
/arg3 != 0 || arg4 != 0 || arg5 != 0/
{
    printf("queued: %s label %d token {%d,%d}: %d words, queue len %d\n",
           copyinstr(arg0), arg3, arg4, arg5,
           arg1, arg2);
}

erlang*:::message-receive
/arg3 == 0 && arg4 == 0 && arg5 == 0/
{
    printf("receive: %s: %d words, queue len %d\n",
           copyinstr(arg0), arg1, arg2);
}

erlang*:::message-receive
/arg3 != 0 || arg4 != 0 || arg5 != 0/
{
    printf("receive: %s label %d token {%d,%d}: %d words, queue len %d\n",
           copyinstr(arg0), arg3, arg4, arg5,
           arg1, arg2);
}
