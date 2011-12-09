%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2011. All Rights Reserved.
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
-define(log(Format,Args),mnesia_test_lib:log(Format,Args,?FILE,?LINE)).
-define(warning(Format,Args),?log("<>WARNING<>~n " ++ Format,Args)).
-define(error(Format,Args),
	mnesia_test_lib:error(Format,Args,?FILE,?LINE)).
-define(verbose(Format,Args),mnesia_test_lib:verbose(Format,Args,?FILE,?LINE)).

-define(fatal(Format,Args),
	?error(Format, Args),
	exit({test_case_fatal, Format, Args, ?FILE, ?LINE})).

-define(skip(Format,Args),
	?warning(Format, Args),
	exit({skipped, ?flat_format(Format, Args)})).

-define(flat_format(Format,Args),
        lists:flatten(io_lib:format(Format, Args))).

-define(sort(What), mnesia_test_lib:sort(What)).

-define(ignore(Expr),
	fun() ->
		AcTuAlReS = (catch (Expr)),
		?verbose("ok, ~n Result as expected:~p~n",[AcTuAlReS]),
		AcTuAlReS
	end()).

-define(match(ExpectedRes,Expr),
	fun() ->
		AcTuAlReS = (catch (Expr)),
		case AcTuAlReS of
		    ExpectedRes ->
			?verbose("ok, ~n Result as expected:~p~n",[AcTuAlReS]),
			{success,AcTuAlReS};
		    _ ->
			?error("Not Matching Actual result was:~n ~p~n",
			       [AcTuAlReS]),
			{fail,AcTuAlReS}
		end
	end()).

-define(match_inverse(NotExpectedRes,Expr),
	fun() ->
		AcTuAlReS = (catch (Expr)),
		case AcTuAlReS of
		    NotExpectedRes ->
			?error("Not matching Actual result was:~n ~p~n",
			       [AcTuAlReS]),
			{fail,AcTuAlReS};
		    _ ->
			?verbose("ok, ~n Result as expected: ~p~n",[AcTuAlReS]),
			{success,AcTuAlReS}
		end
	end()).

-define(match_receive(ExpectedMsg),
	?match(ExpectedMsg,mnesia_test_lib:pick_msg())).

%% ExpectedMsgs must be completely bound
-define(match_multi_receive(ExpectedMsgs),
	fun() ->
		TmPeXpCtEdMsGs = lists:sort(ExpectedMsgs),
		?match(TmPeXpCtEdMsGs,
		       lists:sort(lists:map(fun(_) ->
						    mnesia_test_lib:pick_msg()
					    end,
					    TmPeXpCtEdMsGs)))
	end()).

-define(start_activities(Nodes),
	mnesia_test_lib:start_activities(Nodes)).

-define(start_transactions(Pids),
	mnesia_test_lib:start_transactions(Pids)).

-define(acquire_nodes(N, Config),
	mnesia_test_lib:prepare_test_case([{init_test_case, [mnesia]},
					   delete_schema,
					   create_schema,
					   {start_appls, [mnesia]}],
					  N, Config, ?FILE, ?LINE)).

-define(activate_debug_fun(I, F, C),
	mnesia_lib:activate_debug_fun(I, F, C, ?FILE, ?LINE)).

-define(remote_activate_debug_fun(N, I, F, C),
	?match(ok, mnesia_test_lib:remote_activate_debug_fun(N, I, F, C,
							     ?FILE, ?LINE))).

-define(deactivate_debug_fun(I),
	mnesia_lib:deactivate_debug_fun(I, ?FILE, ?LINE)).

-define(remote_deactivate_debug_fun(N, I),
	rpc:call(N, mnesia_lib, deactivate_debug_fun, [I, ?FILE, ?LINE])).

-define(is_debug_compiled,
	case mnesia_lib:is_debug_compiled() of
	    false ->
		?skip("Mnesia is not debug compiled, test case ignored.~n", []);
	    _OhTeR ->
		ok
	end).

-define(needs_disc(Config),
	case mnesia_test_lib:diskless(Config) of
	    false ->
		ok;
	    true ->
		?skip("Must have disc, test case ignored.~n", [])
	end).

-define(verify_mnesia(Ups, Downs),
	mnesia_test_lib:verify_mnesia(Ups, Downs, ?FILE, ?LINE)).
