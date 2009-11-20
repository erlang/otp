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
%% This record is returned by inet:gethostbyaddr/2 and inet:gethostbyname/2.


-type hostname() :: atom() | string().
-type ip4_address() :: {0..255,0..255,0..255,0..255}.
-type ip6_address() :: {0..65535,0..65535,0..65535,0..65535,
			0..65535,0..65535,0..65535,0..65535}.
-type ip_address() :: ip4_address() | ip6_address().
-type ip_port() :: 0..65535.

-record(hostent,
	{
	 h_name		  :: hostname(),	%% offical name of host
	 h_aliases = []   :: [hostname()],	%% alias list
	 h_addrtype	  :: 'inet' | 'inet6',	%% host address type
	 h_length	  :: non_neg_integer(),	%% length of address
	 h_addr_list = [] :: [ip_address()]	%% list of addresses from name server
	}).
