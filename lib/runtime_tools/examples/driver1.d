/* example usage: dtrace -q -s /path/to/driver1.d */
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

erlang*:::driver-init
{
    printf("driver init name %s major %d minor %d flags %d\n",
	   copyinstr(arg0), arg1, arg2, arg3);
}

erlang*:::driver-start
{
    printf("driver start pid %s driver name %s port %s\n",
	   copyinstr(arg0), copyinstr(arg1), copyinstr(arg2));
}

erlang*:::driver-stop
{
    printf("driver stop pid %s driver name %s port %s\n",
	   copyinstr(arg0), copyinstr(arg1), copyinstr(arg2));
}

erlang*:::driver-finish
{
    printf("driver finish driver name %s port %s\n",
           copyinstr(arg0), copyinstr(arg1));
}

erlang*:::driver-flush
{
    printf("driver flush pid %s port %s port name %s\n",
	   copyinstr(arg0), copyinstr(arg1), copyinstr(arg2));
}

erlang*:::driver-output
{
    printf("driver output pid %s port %s port name %s bytes %d\n",
	   copyinstr(arg0), copyinstr(arg1), copyinstr(arg2), arg3);
}

erlang*:::driver-outputv
{
    printf("driver outputv pid %s port %s port name %s bytes %d\n",
	   copyinstr(arg0), copyinstr(arg1), copyinstr(arg2), arg3);
}

erlang*:::driver-control
{
    printf("driver control pid %s port %s port name %s command %d bytes %d\n",
	   copyinstr(arg0), copyinstr(arg1), copyinstr(arg2), arg3, arg4);
}

erlang*:::driver-call
{
    printf("driver call pid %s port %s port name %s command %d bytes %d\n",
	   copyinstr(arg0), copyinstr(arg1), copyinstr(arg2), arg3, arg4);
}

erlang*:::driver-event
{
    printf("driver event pid %s port %s port name %s\n",
	   copyinstr(arg0), copyinstr(arg1), copyinstr(arg2));
}

erlang*:::driver-ready_input
{
    printf("driver ready_input pid %s port %s port name %s\n",
	   copyinstr(arg0), copyinstr(arg1), copyinstr(arg2));
}

erlang*:::driver-ready_output
{
    printf("driver ready_output pid %s port %s port name %s\n",
	   copyinstr(arg0), copyinstr(arg1), copyinstr(arg2));
}

erlang*:::driver-timeout
{
    printf("driver timeout pid %s port %s port name %s\n",
	   copyinstr(arg0), copyinstr(arg1), copyinstr(arg2));
}

erlang*:::driver-ready_async
{
    printf("driver ready_async pid %s port %s port name %s\n",
	   copyinstr(arg0), copyinstr(arg1), copyinstr(arg2));
}

erlang*:::driver-process_exit
{
    printf("driver process_exit pid %s port %s port name %s\n",
	   copyinstr(arg0), copyinstr(arg1), copyinstr(arg2));
}

erlang*:::driver-stop_select
{
    printf("driver stop_select driver name %s\n", copyinstr(arg0));
}
