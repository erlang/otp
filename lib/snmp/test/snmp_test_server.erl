%% 
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2016. All Rights Reserved.
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

%%----------------------------------------------------------------------
%% Purpose: Lightweight test server
%%----------------------------------------------------------------------

-module(snmp_test_server).

-compile(export_all).

-export([
	 run/1, run/2,

	 error/3,
	 skip/3,
	 fatal_skip/3,

	 init_per_testcase/2,
	 end_per_testcase/2
	]).

-include("snmp_test_lib.hrl").

-define(GLOBAL_LOGGER, snmp_global_logger).
-define(TEST_CASE_SUP, snmp_test_case_supervisor).

-define(d(F,A),d(F,A,?LINE)).

-ifndef(snmp_priv_dir).
-define(snmp_priv_dir, "run-" ++ timestamp()).
-endif.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Evaluates a test case or test suite
%% Returns a list of failing test cases:
%% 
%%     {Mod, Fun, ExpectedRes, ActualRes}
%%----------------------------------------------------------------------

run([Mod, Fun]) when is_atom(Mod) andalso is_atom(Fun) ->
    Res = run({Mod, Fun}, default_config(Mod)),
    display_result(Res),
    Res;
run({Mod, _Fun} = Case) when is_atom(Mod) ->
    io:format("~n", []),
    Res = run(Case, default_config(Mod)),
    display_result(Res),
    Res;
run(Mod) when is_atom(Mod) ->
    io:format("~n", []),
    Res = run(Mod, default_config(Mod)),
    display_result(Res),
    Res;
run([Mod]) when is_atom(Mod) ->
    io:format("~n", []),
    Res = run(Mod, default_config(Mod)),
    display_result(Res),
    Res.
    

run({Mod, Fun}, Config) when is_atom(Mod) andalso 
			     is_atom(Fun) andalso 
			     is_list(Config) ->
    ?d("run(~p,~p) -> entry", [Mod, Fun]),
    case (catch apply(Mod, Fun, [suite])) of
	[] ->
	    io:format("~n~n*** Eval: ~p ***************~n", 
		      [{Mod, Fun}]),
	    case eval(Mod, Fun, Config) of
		{ok, _, _} ->
		    [];
		Other ->
		    [Other]
	    end;

	Cases when is_list(Cases) ->
	    io:format("~n*** Expand: ~p ...~n", [{Mod, Fun}]),
	    Map = fun(Case) when is_atom(Case) -> {Mod, Case};
		     (Case) -> Case
		  end,
	    run(lists:map(Map, Cases), Config);

        {conf, InitSuite, Cases, FinishSuite} when is_atom(InitSuite) andalso 
						   is_list(Cases) andalso 
						   is_atom(FinishSuite) ->
	    ?d("run -> conf: "
	       "~n   InitSuite:   ~p"
	       "~n   Cases:       ~p"
	       "~n   FinishSuite: ~p", [InitSuite, Cases, FinishSuite]),
	    do_suite(Mod, InitSuite, Cases, FinishSuite, Config);
                    
        {req, _, SubCases} when is_list(SubCases) ->
	    ?d("run -> req: "
	       "~n   SubCases: ~p", [SubCases]),
	    do_subcases(Mod, Fun, SubCases, Config, []);
                    
        {req, _, Conf} ->
	    ?d("run -> req: "
	       "~n   Conf: ~p", [Conf]),
	    do_subcases(Mod, Fun, [Conf], Config, []);
                    
        {'EXIT', {undef, _}} ->
            io:format("~n*** Undefined:   ~p~n", [{Mod, Fun}]),
            [{nyi, {Mod, Fun}, ok}];
                    
        Error ->
            io:format("~n*** Ignoring:   ~p: ~p~n", [{Mod, Fun}, Error]),
            [{failed, {Mod, Fun}, Error}]
    end;

run(Mod, Config) when is_atom(Mod) andalso is_list(Config) ->
    run({Mod, all}, Config);

run(Cases, Config) when is_list(Cases) andalso is_list(Config) ->
    Errors = [run(Case, Config) || Case <- Cases],
    lists:append(Errors);

run(Bad, _Config) ->
    [{badarg, Bad, ok}].


do_suite(Mod, Init, Cases, Finish, Config0) ->
    ?d("do_suite -> entry with"
       "~n   Mod:     ~p"
       "~n   Init:    ~p"
       "~n   Cases:   ~p"
       "~n   Finish:  ~p"
       "~n   Config0: ~p", [Mod, Init, Cases, Finish, Config0]),
    case (catch apply(Mod, Init, [Config0])) of
	Config when is_list(Config) ->
	    io:format("~n*** Expand: ~p ...~n", [Mod]),
	    Map = fun(Case) when is_atom(Case) -> {Mod, Case};
		     (Case) -> Case
		  end,
	    Res = run(lists:map(Map, Cases), Config),
	    (catch apply(Mod, Finish, [Config])),
	    Res;

	{'EXIT', {skipped, Reason}} ->
	    io:format(" => skipping: ~p~n", [Reason]),
	    SkippedCases = 
		[{skipped, {Mod, Case}, suite_init_skipped} || Case <- Cases],
	    lists:flatten([[{skipped, {Mod, Init}, Reason}],
			   SkippedCases, 
			   [{skipped, {Mod, Finish}, suite_init_skipped}]]);

	Error ->
	    io:format(" => init (~p) failed: ~n~p~n", [Init, Error]),
	    InitResult = 
		[{failed, {Mod, Init}, Error}],
	    SkippedCases = 
		[{skipped, {Mod, Case}, suite_init_failed} || Case <- Cases],
	    FinResult = 
		case (catch apply(Mod, Finish, [Config0])) of
		    ok ->
			[];
		    FinConfig when is_list(FinConfig) ->
			[];
		    FinError ->
			[{failed, {Mod, Finish}, FinError}]
		end,
	    lists:flatten([InitResult, SkippedCases, FinResult])

    end.


do_subcases(_Mod, _Fun, [], _Config, Acc) ->
    lists:flatten(lists:reverse(Acc));
do_subcases(Mod, Fun, [{conf, Init, Cases, Finish}|SubCases], Config, Acc) 
  when is_atom(Init) andalso is_list(Cases) andalso is_atom(Finish) ->
    R = case (catch apply(Mod, Init, [Config])) of
	    Conf when is_list(Conf) ->
		io:format("~n*** Expand: ~p ...~n", [{Mod, Fun}]),
		Map = fun(Case) when is_atom(Case) -> {Mod, Case};
			 (Case) -> Case
		      end,
		Res = run(lists:map(Map, Cases), Conf),
		(catch apply(Mod, Finish, [Conf])),
		Res;
	    
	    {'EXIT', {skipped, Reason}} ->
		io:format(" => skipping: ~p~n", [Reason]),
		[{skipped, {Mod, Fun}, Reason}];
	    
	    Error ->
		io:format(" => init (~p) failed: ~n~p~n", [Init, Error]),
		(catch apply(Mod, Finish, [Config])),
		[{failed, {Mod, Fun}, Error}]
	end,
    do_subcases(Mod, Fun, SubCases, Config, [R|Acc]);
do_subcases(Mod, Fun, [SubCase|SubCases], Config, Acc) when atom(SubCase) ->
    R = do_case(Mod, SubCase, Config),
    do_subcases(Mod, Fun, SubCases,Config, [R|Acc]).


do_case(M, F, C) ->
    io:format("~n~n*** Eval: ~p ***************~n", [{M, F}]),
    case eval(M, F, C) of
	{ok, _, _} ->
	    [];
	Other ->
	    [Other]
    end.


eval(Mod, Fun, Config) ->
    Flag    = process_flag(trap_exit, true),
    global:register_name(?TEST_CASE_SUP, self()),
    Config2 = Mod:init_per_testcase(Fun, Config),
    Self    = self(), 
    Eval    = fun() -> do_eval(Self, Mod, Fun, Config2) end,
    Pid     = spawn_link(Eval),
    R       = wait_for_evaluator(Pid, Mod, Fun, Config2, []),
    Mod:end_per_testcase(Fun, Config2),
    global:unregister_name(?TEST_CASE_SUP),
    process_flag(trap_exit, Flag),
    R.

wait_for_evaluator(Pid, Mod, Fun, Config, Errors) ->
    Pre = lists:concat(["TEST CASE: ", Fun]),
    receive
	{'EXIT', _Watchdog, watchdog_timeout} ->
	    io:format("*** ~s WATCHDOG TIMEOUT~n", [Pre]), 
	    exit(Pid, kill),
	    {failed, {Mod, Fun}, watchdog_timeout};
	{done, Pid, ok} when Errors =:= [] ->
	    io:format("*** ~s OK~n", [Pre]),
	    {ok, {Mod, Fun}, Errors};
	{done, Pid, {ok, _}} when Errors =:= [] ->
	    io:format("*** ~s OK~n", [Pre]),
	    {ok, {Mod, Fun}, Errors};
	{done, Pid, Fail} ->
	    io:format("*** ~s FAILED~n~p~n", [Pre, Fail]),
	    {failed, {Mod, Fun}, Fail};
	{'EXIT', Pid, {skipped, Reason}} -> 
	    io:format("*** ~s SKIPPED~n~p~n", [Pre, Reason]),
	    {skipped, {Mod, Fun}, Errors};
	{'EXIT', Pid, Reason} -> 
	    io:format("*** ~s CRASHED~n~p~n", [Pre, Reason]),
	    {crashed, {Mod, Fun}, [{'EXIT', Reason} | Errors]};
	{fail, Pid, Reason} ->
	    io:format("*** ~s FAILED~n~p~n", [Pre, Reason]),
	    wait_for_evaluator(Pid, Mod, Fun, Config, Errors ++ [Reason])
   end.

do_eval(ReplyTo, Mod, Fun, Config) ->
    case (catch apply(Mod, Fun, [Config])) of
	{'EXIT', {skipped, Reason}} ->
	    ReplyTo ! {'EXIT', self(), {skipped, Reason}};
	Other ->
	    ReplyTo ! {done, self(), Other}
    end,
    unlink(ReplyTo),
    exit(shutdown).


display_result([]) ->    
    io:format("TEST OK~n", []);

display_result(Errors) when is_list(Errors) ->
    Nyi     = [MF || {nyi, MF, _} <- Errors],
    Skipped = [{MF, Reason} || {skipped, MF, Reason} <- Errors],
    Crashed = [{MF, Reason} || {crashed, MF, Reason} <- Errors],
    Failed  = [{MF, Reason} || {failed,  MF, Reason} <- Errors],
    display_skipped(Skipped),
    display_crashed(Crashed),
    display_failed(Failed),
    display_summery(Nyi, Skipped, Crashed, Failed).

display_summery(Nyi, Skipped, Crashed, Failed) ->
    io:format("~nTest case summery:~n", []),
    display_summery(Nyi, "not yet implemented"),
    display_summery(Skipped, "skipped"),
    display_summery(Crashed, "crashed"),
    display_summery(Failed, "failed"),
    io:format("~n", []).
   
display_summery([], _) ->
    ok;
display_summery(Res, Info) ->
    io:format("  ~w test cases ~s~n", [length(Res), Info]).
    
display_skipped([]) ->
    io:format("Skipped test cases: -~n", []);
display_skipped(Skipped) ->
    io:format("Skipped test cases:~n", []),
    [io:format("  ~p => ~p~n", [MF, Reason]) || {MF, Reason} <- Skipped],
    io:format("~n", []).
    
display_crashed([]) ->
    io:format("Crashed test cases: -~n", []);
display_crashed(Crashed) ->
    io:format("Crashed test cases:~n", []),
    [io:format("  ~p => ~p~n", [MF, Reason]) || {MF, Reason} <- Crashed],
    io:format("~n", []).
    
display_failed([]) ->
    io:format("Failed test cases: -~n", []);
display_failed(Failed) ->
    io:format("Failed test cases:~n", []),
    [io:format("  ~p => ~p~n", [MF, Reason]) || {MF, Reason} <- Failed],
    io:format("~n", []).
        
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Verify that the actual result of a test case matches the exected one
%% Returns the actual result
%% Stores the result in the process dictionary if mismatch

error(Actual, Mod, Line) ->
    global:send(?GLOBAL_LOGGER, {failed, Mod, Line}),
    log("<ERROR> Bad result: ~p~n", [Actual], Mod, Line),
    case global:whereis_name(?TEST_CASE_SUP) of
	undefined -> 
	    ignore;
	Pid -> 
	    Pid ! {fail, self(), {Actual, Mod, Line}}
    end,
    Actual.

skip(Actual, Mod, Line) ->
    log("Skipping test case~n", [], Mod, Line),
    exit({skipped, {Actual, Mod, Line}}).

fatal_skip(Actual, Mod, Line) ->
    error(Actual, Mod, Line),
    exit(shutdown).


log(Format, Args, Mod, Line) ->
    case global:whereis_name(?GLOBAL_LOGGER) of
	undefined ->
	    io:format(user, "~p(~p): " ++ Format, [Mod, Line] ++ Args);
	Pid ->
	    io:format(Pid, "~p(~p): " ++ Format, [Mod, Line] ++ Args)
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test server callbacks

init_per_testcase(_Case, Config) ->
    global:register_name(?GLOBAL_LOGGER, group_leader()),
    Config.

end_per_testcase(_Case, _Config) ->
    global:unregister_name(?GLOBAL_LOGGER),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal utility functions

default_config(Mod) ->
    PrivDir0 = ?snmp_priv_dir,
    case filename:pathtype(PrivDir0) of
	absolute ->
	    ok;
	_ ->
	    case file:make_dir(Mod) of
		ok ->
		    ok;
		{error, eexist} ->
		    ok
	    end,
	    PrivDir = filename:join(Mod, PrivDir0), 
	    case file:make_dir(PrivDir) of
		ok ->
		    ok;
		{error, eexist} ->
		    ok;
		Error ->
		    ?FAIL({failed_creating_subsuite_top_dir, Error})
	    end,
	    [{priv_dir, PrivDir}]
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

d(F, A, L) ->
    d(true, F, A, L).
    %% d(get(dbg), F, A, L).

d(true, F, A, L) ->
    io:format("STS[~w] ~p " ++ F ++ "~n", [L,self()|A]);
d(_, _, _, _) ->
    ok.

timestamp() ->
    {Date, Time}     = calendar:now_to_datetime( now() ),
    {YYYY, MM, DD}   = Date,
    {Hour, Min, Sec} = Time,
    FormatDate =
        io_lib:format("~.4w-~.2.0w-~.2.0w_~.2.0w.~.2.0w.~.2.0w",
                      [YYYY,MM,DD,Hour,Min,Sec]),
    lists:flatten(FormatDate).
    
