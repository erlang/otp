%% 
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2006-2016. All Rights Reserved.
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

-ifndef(snmp_internal).
-define(snmp_internal, true).

-ifndef(APPLICATION).
-define(APPLICATION, snmp).
-endif.

-define(STACK(), erlang:get_stacktrace()).

-define(snmp_info(C, F, A),    ?snmp_msg(info_msg, C, F, A)).
-define(snmp_warning(C, F, A), ?snmp_msg(warning_msg, C, F, A)).
-define(snmp_error(C, F, A),   ?snmp_msg(error_msg, C, F, A)).

-define(snmp_msg(Func, Component, Format, Args),
%% 	io:format("[ ~w : ~s : ~w : ~p ] ~n" ++ Format ++ "~n",
%% 		  [?APPLICATION, Component, ?MODULE, self() | Args]),
	(catch error_logger:Func("[ ~w : ~s : ~w : ~p ] ~n" ++ Format ++ "~n",
		[?APPLICATION, Component, ?MODULE, self() | Args]))).

-endif. % -ifdef(snmp_internal).



