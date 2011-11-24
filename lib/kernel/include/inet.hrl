%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2011. All Rights Reserved.
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


-record(hostent,
	{
	 h_name		  :: inet:hostname(),	%% offical name of host
	 h_aliases = []   :: [inet:hostname()],	%% alias list
	 h_addrtype	  :: 'inet' | 'inet6',	%% host address type
	 h_length	  :: non_neg_integer(),	%% length of address
	 h_addr_list = [] :: [inet:ip_address()]%% list of addresses from name server
	}).
