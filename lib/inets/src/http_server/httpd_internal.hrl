%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2010. All Rights Reserved.
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
%%

-ifndef(httpd_internal_hrl).
-define(httpd_internal_hrl, true).

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
-define(DEFAULT_PROFILE, default).

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

-define(SERVICE, httpd).
-define(hdri(Label, Content), ?report_important(Label, ?SERVICE, Content)).
-define(hdrv(Label, Content), ?report_verbose(Label,   ?SERVICE, Content)).
-define(hdrd(Label, Content), ?report_debug(Label,     ?SERVICE, Content)).
-define(hdrt(Label, Content), ?report_trace(Label,     ?SERVICE, Content)).

-endif. % -ifdef(httpd_internal_hrl).
