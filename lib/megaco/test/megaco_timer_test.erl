%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2019. All Rights Reserved.
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
%% Purpose: Verify the application specifics of the Megaco application
%%----------------------------------------------------------------------
-module(megaco_timer_test).

-compile({no_auto_import,[error/1]}).

-export([
	 t/0, t/1,
	 init_per_testcase/2, end_per_testcase/2,
	 all/0,groups/0,init_per_group/2,end_per_group/2,
	 simple_init/1,
	 simple_usage/1,
	 integer_timer_start_and_expire/1, 
	 integer_timer_start_and_stop/1%% ,
%%	 incr_timer/1
	]).

-export([
	 timeout/3
	]).

-include("megaco_test_lib.hrl").
-include_lib("megaco/include/megaco.hrl").
-include_lib("megaco/include/megaco_message_v1.hrl").


-define(TEST_VERBOSITY, info). % silence | info | debug


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


t()     -> megaco_test_lib:t(?MODULE).
t(Case) -> megaco_test_lib:t({?MODULE, Case}).

%% Test server callbacks
%% init_per_testcase(multi_user_extreme_load = Case, Config) ->
%%     C = lists:keydelete(tc_timeout, 1, Config),
%%     do_init_per_testcase(Case, [{tc_timeout, min(20)}|C]);
init_per_testcase(Case, Config) ->
    do_init_per_testcase(Case, Config).

do_init_per_testcase(Case, Config) ->
    process_flag(trap_exit, true),
    {ok, _Pid} = megaco_monitor:start_link(),
    megaco_test_lib:init_per_testcase(Case, [{monitor_running, true}|Config]).
    
end_per_testcase(Case, Config) ->
    io:format("end_per_testcase -> entry with"
	       "~n   Case:   ~p"
	       "~n   Config: ~p"
	       "~n", [Case, Config]),
    process_flag(trap_exit, false),
    case lists:keydelete(monitor_running, 1, Config) of
	Config ->
	    megaco_test_lib:end_per_testcase(Case, Config);
	Config2 ->
	    megaco_monitor:stop(),
	    megaco_test_lib:end_per_testcase(Case, Config2)
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all() -> 
    [{group, simple}, {group, integer_timer}].

groups() -> 
    [{simple, [],
      [simple_init, simple_usage]},
%, incr_timer
     {integer_timer, [],
      [integer_timer_start_and_expire,
       integer_timer_start_and_stop]}].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


%% incr_timer(suite) ->
%%     Cases = 
%% 	[
%% 	],
%%     Cases.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

simple_init(suite) ->
    [];
simple_init(doc) ->
    [];
simple_init(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(tc,        si),
    put(sname,     "TEST"),
    put(verbosity, info),
    i("starting"),

    Init = 
	fun(Tmr) ->
		case (catch megaco_timer:init(Tmr)) of
		    {WaitFor, {NewTmr, _}} when 
		        (((WaitFor == infinity) or is_integer(WaitFor)) andalso 
			 is_record(NewTmr, megaco_incr_timer) andalso
			 (is_record(Tmr, megaco_incr_timer) andalso 
			  (Tmr#megaco_incr_timer.max_retries == infinity_restartable))) ->
			ok;
		    {WaitFor, NewTmr} when 
			 (((WaitFor == infinity) or is_integer(WaitFor)) andalso 
			  ((NewTmr == timeout) or is_record(NewTmr, megaco_incr_timer))) ->
			ok;
		    X ->
			d("initiation of timer failed: "
			  "~n   X: ~p", [X]),
			{error, X}
		end
	end,
    Verify = 
	fun(A, A) ->
		ok;
	   (error, {error, Reason}) ->
		d("Error reason: ~p", [Reason]),
		ok;
	   (A, B) ->
		d("unexpected result: "
		  "~n   Expected: ~p"
		  "~n   Actual:   ~p", [A, B]),
		error({unexpected_result, A, B})
	end,
    VerifyTMR = 
	fun(false, Tmr) -> 
		not megaco_timer:verify(Tmr);
	   (true, Tmr) ->
		megaco_timer:verify(Tmr)
	end,

    d(" 1) verify infinity timer"), 
    TMR01 = infinity,
    Verify(true,  VerifyTMR(true, TMR01)),
    Verify(ok,    Init(TMR01)),

    d(" 2) verify integer (2007) timer"), 
    TMR02 = 2007,
    Verify(true,  VerifyTMR(true, TMR02)),
    Verify(ok,    Init(TMR02)),

    d(" 3) verify default megaco incr timer timer"), 
    TMR03 = #megaco_incr_timer{},
    Verify(true,  VerifyTMR(true, TMR03)),
    Verify(ok,    Init(TMR03)),

    d(" 4) verify megaco incr timer timer"), 
    TMR04 = #megaco_incr_timer{max_retries = infinity_restartable},
    Verify(true,  VerifyTMR(true, TMR04)),
    Verify(ok,    Init(TMR04)),

    d(" 5) verify megaco incr timer timer"), 
    TMR05 = #megaco_incr_timer{incr = -1}, %% This is new
    Verify(true,  VerifyTMR(true, TMR05)),
    Verify(ok,    Init(TMR05)), 

    d(" 6) verify invalid timer"), 
    TMR06 = infinit,
    Verify(true,  VerifyTMR(false, TMR06)),
    Verify(error, Init(TMR06)),

    d(" 7) verify invalid timer"), 
    TMR07 = -2007,
    Verify(true,  VerifyTMR(false, TMR07)),
    Verify(error, Init(TMR07)),

    d(" 8) verify invalid timer"), 
    TMR08 = 20.33,
    Verify(true,  VerifyTMR(false, TMR08)),
    Verify(error, Init(TMR08)),

    d(" 9) verify invalid timer"), 
    TMR09 = -20.33,
    Verify(true,  VerifyTMR(false, TMR09)),
    Verify(error, Init(TMR09)),

    d("10) verify invalid timer"), 
    TMR10 = "kalle anka", 
    Verify(true,  VerifyTMR(false, TMR10)),
    Verify(error, Init(TMR10)),

    d("11) verify invalid timer"), 
    TMR11 = #megaco_incr_timer{wait_for    = 10,
			       factor      = 1,
			       incr        = 0,
			       max_retries = infinit},
    Verify(true,  VerifyTMR(false, TMR11)),
    Verify(error, Init(TMR11)),

    i("done", []),
    ok.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

simple_usage(suite) ->
    [];
simple_usage(doc) ->
    [];
simple_usage(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(tc,        su),
    put(sname,     "TEST"),
    i("starting"),

    Verify  = fun(Tmr) -> megaco_timer:verify(Tmr)  end,
    Init    = fun(Tmr) -> megaco_timer:init(Tmr)    end,
    Restart = fun(Tmr) -> megaco_timer:restart(Tmr) end,

    VerifyRes = 
	fun(A, A) ->
		ok;
	   (A, B) ->
		error({unexpected_result, A, B})
	end,


    %% Timer 1
    d(" 1) verify (infinity) timer"), 
    TMR01 = infinity,
    VerifyRes(true,  Verify(TMR01)), 
    d(" 1) init (infinity) timer"), 
    VerifyRes({TMR01, timeout}, Init(TMR01)),

    %% Timer 2
    d(" 2) verify (integer) timer"), 
    TMR02 = 1000,
    VerifyRes(true,  Verify(TMR02)), 
    d(" 2) init (integer) timer"), 
    VerifyRes({TMR02, timeout}, Init(TMR02)),

    %% Timer 3
    d(" 3) verify (megaco_incr_timer) timer"), 
    TMR03 = #megaco_incr_timer{wait_for    = TMR02,
			       factor      = 1,
			       incr        = 0,
			       max_retries = infinity}, 
    VerifyRes(true,  Verify(TMR03)), 
    d(" 3) init (megaco_incr_timer) timer"), 
    {TMR02, NewTMR03_1} = Init(TMR03),
    d(" 3) restart (megaco_incr_timer) timer"), 
    {TMR02, _}          = Restart(NewTMR03_1),

    %% Timer 4
    d(" 4) verify (megaco_incr_timer) timer"), 
    TMR04 = #megaco_incr_timer{wait_for    = 1000,
			       factor      = 1,
			       incr        = 0,
			       max_retries = 2}, 
    VerifyRes(true,  Verify(TMR04)), 
    d(" 4) init (megaco_incr_timer) timer"), 
    {TMR02, NewTMR04_1} = Init(TMR04),
    d(" 4) restart (megaco_incr_timer) timer"), 
    {TMR02, NewTMR04_2} = Restart(NewTMR04_1),
    d(" 4) last restart (megaco_incr_timer) timer"), 
    {TMR02, timeout}    = Restart(NewTMR04_2),

    %% Timer 5
    d(" 5) verify (megaco_incr_timer) timer"), 
    TMR05 = #megaco_incr_timer{wait_for    = 1000,
			       factor      = 1,
			       incr        = -300,
			       max_retries = infinity}, 
    VerifyRes(true,  Verify(TMR05)), 
    d(" 5) init (megaco_incr_timer) timer"), 
    {TMR02, NewTMR05_1}     = Init(TMR05),
    d(" 5) restart (1) (megaco_incr_timer) timer"), 
    TMR05_1 = TMR02-300,
    {TMR05_1, NewTMR05_2} = Restart(NewTMR05_1),
    d(" 5) restart (2) (megaco_incr_timer) timer"), 
    TMR05_2 = TMR05_1-300,
    {TMR05_2, NewTMR05_3} = Restart(NewTMR05_2),
    d(" 5) restart (3) (megaco_incr_timer) timer"), 
    TMR05_3 = TMR05_2-300,
    {TMR05_3, NewTMR05_4} = Restart(NewTMR05_3),
    d(" 5) restart (4) (megaco_incr_timer) timer"), 
    {0, NewTMR05_5}         = Restart(NewTMR05_4),
    d(" 5) restart (5) (megaco_incr_timer) timer"), 
    {0, _}                  = Restart(NewTMR05_5),

    i("done", []),
    ok.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

integer_timer_start_and_expire(suite) ->
    [];
integer_timer_start_and_expire(doc) ->
    [];
integer_timer_start_and_expire(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(tc,        itsae),
    put(sname,     "TEST"),
    i("starting"),

    Timeout = 5000,
    Ref = tmr_start(Timeout),
    receive
	{timeout, Timeout} ->
	    ok
    after Timeout + 500 ->
	    tmr_stop(Ref),
	    error(no_timeout)
    end,

    i("done", []),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

integer_timer_start_and_stop(suite) ->
    [];
integer_timer_start_and_stop(doc) ->
    [];
integer_timer_start_and_stop(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(tc,        itsas),
    put(sname,     "TEST"),
    i("starting"),

    Timeout = 5000,
    i("try start (~w msec) timer", [Timeout]),
    Ref     = tmr_start(Timeout),
    i("timer started "),
    receive
	{timeout, Timeout} ->
            i("unexpected premature timer expire"),
	    error(bad_timeout)
    after Timeout - 100 ->
            i("try stop timer"),
	    case tmr_stop(Ref) of
                {ok, Rem} ->
                    i("timer stopped with ~w msec remaining", [Rem]),
                    ok;
                CancelRes ->
                    i("failed stop timer: "
                      "~n   ~p", [CancelRes]),
                    ?SKIP({cancel_failed, CancelRes}) % Race - not our problem
            end
    end,

    %% Make sure it does not reach us after we attempted to stop it.
    receive
	{timeout, Timeout} ->
	    error(unexpected_timeout)
    after Timeout ->
	    ok
    end,

    i("done", []),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tmr_start(Timeout) ->
    Pid = self(), 
    megaco_monitor:apply_after(?MODULE, timeout, [Pid, Timeout, get(tc)], Timeout).

tmr_stop(Ref) ->
    megaco_monitor:cancel_apply_after(Ref).

timeout(Pid, Timeout, Tc) ->
    put(sname, timer),
    put(tc, Tc), 
    print("DBG",
	  "timeout -> entry with"
	  "~n   Pid:     ~p"
	  "~n   Timeout: ~p", [Pid, Timeout]),
    Pid ! {timeout, Timeout}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% tim() ->
%%     {A,B,C} = erlang:now(),
%%     A*1000000000+B*1000+(C div 1000).

%% min(M) -> timer:minutes(M).

%% sleep(X) -> receive after X -> ok end.

error(Reason) -> throw({error, Reason}).

%% error_msg(F,A) -> error_logger:error_msg(F ++ "~n",A).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

i(F) ->
    i(F, []).

i(F, A) ->
    print(info, "INF", F, A).

d(F) ->
    d(F, []).

d(F, A) ->
    print(debug, "DBG", F, A).

printable(_, debug)   -> true;
printable(info, info) -> true;
printable(_,_)        -> false.


print(Severity, Prefix, F, A) ->
    print1(printable(Severity, get(verbosity)), Prefix, F, A).

print1(true, Prefix, F, A) ->
    print(Prefix, F, A);
print1(_, _, _, _) ->
    ok.

print(Prefix, F, A) ->
    io:format("*** [~s] ~s ~p ~s:~w ***"
              "~n   " ++ F ++ "~n", 
              [?FTS(), Prefix, self(), get(sname), get(tc) | A]).


