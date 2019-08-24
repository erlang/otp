/* example usage: dtrace -q -s /path/to/port1.d */
/*
 * %CopyrightBegin%
 *
 * Copyright Scott Lystig Fritchie 2011-2016. All Rights Reserved.
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

BEGIN
{
        driver_map["tcp_inet", 1] = "OPEN";
        driver_map["tcp_inet", 2] = "CLOSE";
        driver_map["tcp_inet", 3] = "CONNECT";
        driver_map["tcp_inet", 4] = "PEER";
        driver_map["tcp_inet", 5] = "NAME";
        driver_map["tcp_inet", 6] = "BIND";
        driver_map["tcp_inet", 7] = "SETOPTS";
        driver_map["tcp_inet", 8] = "GETOPTS";
        driver_map["tcp_inet", 11] = "GETSTAT";
        driver_map["tcp_inet", 12] = "GETHOSTNAME";
        driver_map["tcp_inet", 13] = "FDOPEN";
        driver_map["tcp_inet", 14] = "GETFD";
        driver_map["tcp_inet", 15] = "GETTYPE";
        driver_map["tcp_inet", 16] = "GETSTATUS";
        driver_map["tcp_inet", 17] = "GETSERVBYNAME";
        driver_map["tcp_inet", 18] = "GETSERVBYPORT";
        driver_map["tcp_inet", 19] = "SETNAME";
        driver_map["tcp_inet", 20] = "SETPEER";
        driver_map["tcp_inet", 21] = "GETIFLIST";
        driver_map["tcp_inet", 22] = "IFGET";
        driver_map["tcp_inet", 23] = "IFSET";
        driver_map["tcp_inet", 24] = "SUBSCRIBE";
        driver_map["tcp_inet", 25] = "GETIFADDRS";
        driver_map["tcp_inet", 40] = "ACCEPT";
        driver_map["tcp_inet", 41] = "LISTEN";
        driver_map["tcp_inet", 42] = "RECV";
        driver_map["tcp_inet", 43] = "UNRECV";
        driver_map["tcp_inet", 44] = "SHUTDOWN";
        driver_map["tcp_inet", 60] = "RECV";
        driver_map["tcp_inet", 61] = "LISTEN";
        driver_map["tcp_inet", 62] = "BINDX";
        /* No looping constructs, so repeat for udp_inet */
        driver_map["udp_inet", 1] = "OPEN";
        driver_map["udp_inet", 2] = "CLOSE";
        driver_map["udp_inet", 3] = "CONNECT";
        driver_map["udp_inet", 4] = "PEER";
        driver_map["udp_inet", 5] = "NAME";
        driver_map["udp_inet", 6] = "BIND";
        driver_map["udp_inet", 7] = "SETOPTS";
        driver_map["udp_inet", 8] = "GETOPTS";
        driver_map["udp_inet", 11] = "GETSTAT";
        driver_map["udp_inet", 12] = "GETHOSTNAME";
        driver_map["udp_inet", 13] = "FDOPEN";
        driver_map["udp_inet", 14] = "GETFD";
        driver_map["udp_inet", 15] = "GETTYPE";
        driver_map["udp_inet", 16] = "GETSTATUS";
        driver_map["udp_inet", 17] = "GETSERVBYNAME";
        driver_map["udp_inet", 18] = "GETSERVBYPORT";
        driver_map["udp_inet", 19] = "SETNAME";
        driver_map["udp_inet", 20] = "SETPEER";
        driver_map["udp_inet", 21] = "GETIFLIST";
        driver_map["udp_inet", 22] = "IFGET";
        driver_map["udp_inet", 23] = "IFSET";
        driver_map["udp_inet", 24] = "SUBSCRIBE";
        driver_map["udp_inet", 25] = "GETIFADDRS";
        driver_map["udp_inet", 40] = "ACCEPT";
        driver_map["udp_inet", 41] = "LISTEN";
        driver_map["udp_inet", 42] = "RECV";
        driver_map["udp_inet", 43] = "UNRECV";
        driver_map["udp_inet", 44] = "SHUTDOWN";
        driver_map["udp_inet", 60] = "RECV";
        driver_map["udp_inet", 61] = "LISTEN";
        driver_map["udp_inet", 62] = "BINDX";
}

erlang*:::port-open
{
    printf("port open pid %s port name %s port %s\n",
           copyinstr(arg0), copyinstr(arg1), copyinstr(arg2));
}

erlang*:::port-command
{
    printf("port command pid %s port %s port name %s command type %s\n",
	   copyinstr(arg0), copyinstr(arg1), copyinstr(arg2), copyinstr(arg3));
}

erlang*:::port-control
{
    /* http://dtrace.org/blogs/brendan/2011/11/25/dtrace-variable-types/ */
    this->cmd = driver_map[copyinstr(arg2), arg3];
    this->cmd_str = (this->cmd == 0) ? "unknown" : this->cmd;
    printf("port control pid %s port %s port name %s command %d %s\n",
	   copyinstr(arg0), copyinstr(arg1), copyinstr(arg2), arg3,
           this->cmd_str);
}

/* port-exit is fired as a result of port_close() or exit signal */

erlang*:::port-exit
{
    printf("port exit pid %s port %s port name %s reason %s\n",
	   copyinstr(arg0), copyinstr(arg1), copyinstr(arg2), copyinstr(arg3));
}

erlang*:::port-connect
{
    printf("port connect pid %s port %s port name %s new pid %s\n",
	   copyinstr(arg0), copyinstr(arg1), copyinstr(arg2), copyinstr(arg3));
}

erlang*:::port-busy
{
    printf("port busy %s\n", copyinstr(arg0));
}

erlang*:::port-not_busy
{
    printf("port not busy %s\n", copyinstr(arg0));
}

erlang*:::aio_pool-add
{
    printf("async I/O pool add thread %d queue len %d\n", arg0, arg1);
}

erlang*:::aio_pool-get
{
    printf("async I/O pool get thread %d queue len %d\n", arg0, arg1);
}
