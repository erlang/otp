%% 
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2006-2009. All Rights Reserved.
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

-ifndef(snmp_internal).
-define(snmp_internal, true).

-ifndef(APPLICATION).
-define(APPLICATION, snmp).
-endif.

-define(snmp_info(C, F, A),    ?snmp_msg(info_msg, C, F, A)).
-define(snmp_warning(C, F, A), ?snmp_msg(warning_msg, C, F, A)).
-define(snmp_error(C, F, A),   ?snmp_msg(error_msg, C, F, A)).

-define(snmp_msg(Func, Component, Format, Args),
%% 	io:format("[ ~w : ~s : ~w : ~p ] ~n" ++ Format ++ "~n",
%% 		  [?APPLICATION, Component, ?MODULE, self() | Args]),
	(catch error_logger:Func("[ ~w : ~s : ~w : ~p ] ~n" ++ Format ++ "~n",
		[?APPLICATION, Component, ?MODULE, self() | Args]))).

-endif. % -ifdef(snmp_internal).



