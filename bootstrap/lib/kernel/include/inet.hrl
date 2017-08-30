%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
%% 
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
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
