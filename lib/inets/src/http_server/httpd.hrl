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
%%

-include_lib("kernel/include/file.hrl").

-ifndef(SERVER_SOFTWARE).
-define(SERVER_SOFTWARE,"inets/develop").	% Define in Makefile!
-endif.
-define(SERVER_PROTOCOL,"HTTP/1.1").
-define(DEFAULT_MODS, [mod_alias, mod_auth, mod_esi, mod_actions, mod_cgi,
		       mod_dir, mod_get, mod_head, mod_log, mod_disk_log]).
-define(SOCKET_CHUNK_SIZE,8192).
-define(SOCKET_MAX_POLL,25).
-define(FILE_CHUNK_SIZE,64*1024).
-define(GATEWAY_INTERFACE,"CGI/1.1").
-define(NICE(Reason),lists:flatten(atom_to_list(?MODULE)++": "++Reason)).
-define(DEFAULT_CONTEXT,
	[{errmsg,"[an error occurred while processing this directive]"},
	 {timefmt,"%A, %d-%b-%y %T %Z"},
	 {sizefmt,"abbrev"}]).


-ifdef(inets_error).
-define(ERROR(Format, Args), io:format("E(~p:~p:~p) : "++Format++"~n",
				       [self(),?MODULE,?LINE]++Args)).
-else.
-define(ERROR(F,A),[]).
-endif.

-ifdef(inets_log).
-define(LOG(Format, Args), io:format("L(~p:~p:~p) : "++Format++"~n",
				     [self(),?MODULE,?LINE]++Args)).
-else.
-define(LOG(F,A),[]).
-endif.

-ifdef(inets_debug).
-define(DEBUG(Format, Args), io:format("D(~p:~p:~p) : "++Format++"~n",
				       [self(),?MODULE,?LINE]++Args)).
-else.
-define(DEBUG(F,A),[]).
-endif.

-ifdef(inets_cdebug).
-define(CDEBUG(Format, Args), io:format("C(~p:~p:~p) : "++Format++"~n",
				       [self(),?MODULE,?LINE]++Args)).
-else.
-define(CDEBUG(F,A),[]).
-endif.


-record(init_data,{peername,resolve}).
-record(mod,{init_data,
	     data=[],
	     socket_type=ip_comm,
	     socket,
	     config_db,
	     method,
	     absolute_uri=[],
	     request_uri,
	     http_version,
	     request_line,
	     parsed_header=[],
	     entity_body,
	     connection}).
