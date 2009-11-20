%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2009. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%

%%

%% op codes commands are in capital and reply codes in lower case 

-define(CONNECT,	1).
-define(CONNECT_WAIT,	2).
-define(CONNECT_REP,	3).
-define(CONNECT_ERR,	4).

-define(TERMINATE,	5).
-define(CLOSE,		6).

-define(LISTEN,		7).
-define(LISTEN_REP,	8).
-define(LISTEN_ERR,	9).

-define(TRANSPORT_ACCEPT, 10).
-define(NOACCEPT,	11).
-define(TRANSPORT_ACCEPT_REP, 12).
-define(TRANSPORT_ACCEPT_ERR, 13).

-define(FROMNET_CLOSE,	14).

-define(CONNECT_SYNC_ERR, 15).
-define(LISTEN_SYNC_ERR, 16).

-define(PROXY_PORT,	23).
-define(PROXY_JOIN,	24).
-define(PROXY_JOIN_REP,	25).
-define(PROXY_JOIN_ERR,	26).

-define(SET_SOCK_OPT,	27).
-define(IOCTL_OK,	28).
-define(IOCTL_ERR,	29).

-define(GETPEERNAME,	30).
-define(GETPEERNAME_REP, 31).
-define(GETPEERNAME_ERR, 32).

-define(GETSOCKNAME,	33).
-define(GETSOCKNAME_REP, 34).
-define(GETSOCKNAME_ERR, 35).

-define(GETPEERCERT,	36).
-define(GETPEERCERT_REP, 37).
-define(GETPEERCERT_ERR, 38).

-define(GETVERSION, 39).
-define(GETVERSION_REP, 40).

-define(SET_SEED, 41).

-define(GETCONNINFO, 42).
-define(GETCONNINFO_REP, 43).
-define(GETCONNINFO_ERR, 44).

-define(SSL_ACCEPT, 45).
-define(SSL_ACCEPT_REP, 46).
-define(SSL_ACCEPT_ERR, 47).

-define(DUMP_CMD,       48).
-define(DEBUG_CMD,      49).
-define(DEBUGMSG_CMD,   50).

%% --------------

-define(SSLv2, 1).
-define(SSLv3, 2).
-define(TLSv1, 4).


%% Set socket options codes  'SET_SOCK_OPT' 
-define(SET_TCP_NODELAY, 1).

-define(DEF_BACKLOG, 128).

-define(DEF_TIMEOUT, 10000).

-record(sslsocket, { fd = nil, pid = nil}).

