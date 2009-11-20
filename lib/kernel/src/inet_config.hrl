%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2009. All Rights Reserved.
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

%% Configuration constants

-define(DEFAULT_ETC,       "/etc").
-define(DEFAULT_SERVICES,  "services").
-define(DEFAULT_RPC,       "rpc").
-define(DEFAULT_HOSTS,     "hosts").
-define(DEFAULT_RESOLV,    "resolv.conf").
-define(DEFAULT_PROTOCOLS, "protocols").
-define(DEFAULT_NETMASKS,  "netmasks").
-define(DEFAULT_NETWORKS,  "networks").

-define(DEFAULT_UDP_MODULE,  inet_udp).
-define(DEFAULT_TCP_MODULE,  inet_tcp).
-define(DEFAULT_SCTP_MODULE, inet_sctp).

