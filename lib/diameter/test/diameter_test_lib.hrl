%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% Purpose: Define common macros for testing
%%----------------------------------------------------------------------
%% 

-define(FLUSH(),        diameter_test_lib:flush()).

-define(SLEEP(MSEC),    diameter_test_lib:sleep(MSEC)).
-define(M(),            diameter_test_lib:millis()).
-define(MDIFF(A,B),     diameter_test_lib:millis_diff(A,B)).

-define(HOURS(T),       diameter_test_lib:hours(T)).
-define(MINUTES(T),     diameter_test_lib:minutes(T)).
-define(SECONDS(T),     diameter_test_lib:seconds(T)).

-define(KEY1SEARCH(Key, L), diameter_test_lib:key1search(Key, L)).


-define(APPLY(Proxy, Fun),
	Proxy ! {apply, Fun}).

-define(LOG(Format, Args),
	diameter_test_lib:log(Format, Args, ?MODULE, ?LINE)).

-define(ERROR(Reason),
	diameter_test_lib:error(Reason, ?MODULE, ?LINE)).

-define(OS_BASED_SKIP(Skippable),
	diameter_test_lib:os_based_skip(Skippable)).

-define(NON_PC_TC_MAYBE_SKIP(Config, Condition),
	diameter_test_lib:non_pc_tc_maybe_skip(Config, Condition, ?MODULE, ?LINE)).

-define(FAIL(Reason),
	diameter_test_lib:fail(Reason, ?MODULE, ?LINE)).

-define(SKIP(Reason),
	diameter_test_lib:skip(Reason, ?MODULE, ?LINE)).

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
	?VERIFY(Expected, ?FLUSH())).

-define(MULTI_RECEIVE(Expected),
	?VERIFY(lists:sort(Expected), lists:sort(?FLUSH()))).

-define(ACQUIRE_NODES(N, Config),
	diameter_test_lib:prepare_test_case([init, {stop_app, diameter}],
					    N, Config, ?FILE, ?LINE)).


-define(REPORT_IMPORTANT(Label, Content), ?REPORT_EVENT(20, Label, Content)).
-define(REPORT_VERBOSE(Label, Content),   ?REPORT_EVENT(40, Label, Content)).
-define(REPORT_DEBUG(Label, Content),     ?REPORT_EVENT(60, Label, Content)).
-define(REPORT_TRACE(Label, Content),     ?REPORT_EVENT(80, Label, Content)).

-define(REPORT_EVENT(Severity, Label, Content),
	diameter_test_lib:report_event(Severity, Label, 
				       [{line, ?MODULE, ?LINE} | Content])).

