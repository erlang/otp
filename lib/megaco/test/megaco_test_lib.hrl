%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2023. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% Purpose: Define common macros for testing
%%----------------------------------------------------------------------

-ifndef(APPLICATION).
-define(APPLICATION, megaco).
-endif.

-define(LIB, megaco_test_lib).

-define(PCALL(F, T, D),      ?LIB:proxy_call(F, T, D)).

-define(APPLY(Proxy, Fun),
	Proxy ! {apply, Fun}).

-define(LOG(Format, Args),
	?LIB:log(Format, Args, ?MODULE, ?LINE)).

-define(ERROR(Reason),
	?LIB:error(Reason, ?MODULE, ?LINE)).

-define(OS_BASED_SKIP(Skippable),
	?LIB:os_based_skip(Skippable)).

-define(NON_PC_TC_MAYBE_SKIP(Config, Condition),
	?LIB:non_pc_tc_maybe_skip(Config, Condition, ?MODULE, ?LINE)).

-define(SKIP(Reason),
	?LIB:skip(Reason, ?MODULE, ?LINE)).

-define(FAIL(Reason),
	exit({Reason, ?MODULE, ?LINE})).

-define(VERIFYL(Expected, Expr),
	fun(A,B) when list(A), list(B) ->
		A1 = lists:sort(A),
		B1 = lists:sort(B),
		case A1 of
		    B1 -> ?LOG("Ok, ~p~n", [B]);
		    _  -> ?ERROR(B)
		end,
		B;
	   (A,A) ->
		?LOG("Ok, ~p~n", [A]),
		A;
	   (A,B) ->
		?ERROR(B),
		B
	end(Expected, (catch Expr))).

-define(VERIFY(Expected, Expr),
	fun() ->
		AcTuAlReS = (catch (Expr)),
		case AcTuAlReS of
		    Expected -> ?LOG("Ok, ~p~n", [AcTuAlReS]);
		    _        ->	?ERROR(AcTuAlReS)
		end,
		AcTuAlReS
	end()).

-define(RECEIVE(Expected),
	?VERIFY(Expected, ?LIB:flush())).

-define(MULTI_RECEIVE(Expected),
	?VERIFY(lists:sort(Expected), lists:sort(?LIB:flush()))).

-define(EXEC(F),     ?LIB:executor(F)).
-define(EXEC(F, TO), ?LIB:executor(F, TO)).

-define(TRY_TC(TCN, N, V, PRE, CASE, POST),
        ?LIB:try_tc(TCN, N, V, PRE, CASE, POST)).
-define(TRY_TC(TCN, N, V, COND, PRE, CASE, POST),
        ?LIB:try_tc(TCN, N, V, COND, PRE, CASE, POST)).

-define(ACQUIRE_NODES(N, Config),
	?LIB:prepare_test_case([init, {stop_app, megaco}],
                               N, Config, ?FILE, ?LINE)).

-define(MK_NODES(N),               ?LIB:mk_nodes(N)).
-define(START_NODE(Node, Force),   ?LIB:start_node(Node, Force, ?FILE, ?LINE)).
-define(START_NODE(Node),          ?START_NODE(Node, false)).
-define(START_NODES(Nodes, Force), ?LIB:start_nodes(Nodes, Force, ?FILE, ?LINE)).
-define(START_NODES(Nodes),        ?START_NODES(Nodes, false)).
-define(STOP_NODE(Node),           ?LIB:stop_node(Node,   ?FILE, ?LINE)).
-define(STOP_NODES(Nodes),         ?LIB:stop_nodes(Nodes, ?FILE, ?LINE)).

-define(SLEEP(MSEC),    ?LIB:sleep(MSEC)).
-define(HOURS(T),       ?LIB:hours(T)).
-define(MINS(T),        ?LIB:minutes(T)).
-define(MINUTES(T),     ?MINS(T)).
-define(SECS(T),        ?LIB:seconds(T)).
-define(SECONDS(T),     ?SECS(T)).
-define(FTS(),          megaco:format_timestamp(erlang:timestamp())).
-define(FTS(TS),        megaco:format_timestamp(TS)).
-define(F(F,A),         lists:flatten(io_lib:format(F, A))).

-define(INET_BACKEND_OPTS(C),    ?LIB:inet_backend_opts(C)).
-define(EXPLICIT_INET_BACKEND(), ?LIB:explicit_inet_backend()).
-define(TEST_INET_BACKENDS(),    ?LIB:test_inet_backends()).
-define(WHICH_INET_BACKEND(C),   ?LIB:which_inet_backend(C)).
-define(IS_SOCKET_BACKEND(C),    ?LIB:is_socket_backend(C)).

-define(OPEN(C, P, O),           ?LIB:open(C, P, O)).
-define(LISTEN(C, P, O),         ?LIB:listen(C, P, O)).
-define(CONNECT(C, R, O),        ?LIB:connect(C, R, O)).


-define(ANNOUNCE_SUITE_INIT(),
	io:format(user, "~n*** ~s *** suite ~w init~n~n", [?FTS(), ?MODULE])).
-define(ANNOUNCE_SUITE_END(),
	io:format(user, "~n*** ~s *** suite ~w end~n~n", [?FTS(), ?MODULE])).
-define(ANNOUNCE_GROUP_INIT(GR),
	io:format(user, "~n*** ~s *** group ~w:~w init~n~n", 
		  [?FTS(), ?MODULE, GR])).
-define(ANNOUNCE_GROUP_END(GR),
	io:format(user, "~n*** ~s *** group ~w:~w end~n~n", 
		  [?FTS(), ?MODULE, GR])).
-define(ANNOUNCE_CASE_INIT(C),
	io:format(user, "~n*** ~s *** case ~w:~w init~n~n", 
		  [?FTS(), ?MODULE, C])).
-define(ANNOUNCE_CASE_END(C),
	io:format(user, "~n*** ~s *** case ~w:~w end~n~n", 
		  [?FTS(), ?MODULE, C])).

-define(UNIQUE(PreName),
        list_to_atom(
          ?F("~w_~w", [(PreName), erlang:system_time(millisecond)]))).

-define(MEGACO_TRACE(C, D),    ?LIB:megaco_trace((C), (D))).
-define(ENABLE_TRACE(C, L, D), ?LIB:enable_trace((C), (L), (D))).
