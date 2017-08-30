%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012-2016. All Rights Reserved.
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
-module(test_server_break_SUITE).

-export([all/1, init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).
-export([break_in_init_tc/1,
	 break_in_tc/1,
	 break_in_end_tc/1,
	 break_in_end_tc_after_fail/1,
	 break_in_end_tc_after_abort/1,
	 check_all_breaks/1]).

-include_lib("common_test/include/ct.hrl").

all(suite) ->
    [break_in_init_tc,
     break_in_tc,
     break_in_end_tc,
     break_in_end_tc_after_fail,
     break_in_end_tc_after_abort,
     check_all_breaks]. %must be the last test - checks result of previous tests

init_per_suite(Config) ->
    spawn(fun break_and_continue_sup/0),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(Case,Config) when Case==break_in_init_tc ->
    Config1 = init_timetrap(500,Config),
    break_and_check(Case),
    Config1;
init_per_testcase(Case,Config) when Case==check_all_breaks ->
    init_timetrap({seconds,20},Config);
init_per_testcase(_Case,Config) ->
    init_timetrap(500,Config).

init_timetrap(T,Config) ->
    Dog = ?t:timetrap(T),
    [{watchdog, Dog}|Config].

end_per_testcase(Case,Config) when Case==break_in_end_tc;
				   Case==break_in_end_tc_after_fail;
				   Case==break_in_end_tc_after_abort ->
    break_and_check(Case),
    cancel_timetrap(Config);
end_per_testcase(_Case,Config) ->
    cancel_timetrap(Config).

cancel_timetrap(Config) ->
    Dog=?config(watchdog, Config),
    ?t:timetrap_cancel(Dog),
    ok.


%%%-----------------------------------------------------------------
%%% Test cases

break_in_init_tc(Config) when is_list(Config) ->
    ok.

break_in_tc(Config) when is_list(Config) ->
    break_and_check(break_in_tc),
    ok.

break_in_end_tc(Config) when is_list(Config) ->
    ok.

break_in_end_tc_after_fail(Config) when is_list(Config) ->
    ?t:fail(test_case_should_fail).

break_in_end_tc_after_abort(Config) when is_list(Config) ->
    ?t:adjusted_sleep(2000). % will cause a timetrap timeout

%% This test case checks that all breaks in previous test cases was
%% also continued, and that the break lasted as long as expected.
%% The reason for this is that some of the breaks above are in
%% end_per_testcase, and failures there will only produce a warning,
%% not an error - so this is to catch the error for real.
check_all_breaks(Config) when is_list(Config) ->
    break_and_continue_sup ! {done,self()},
    receive {Breaks,Continued} ->
	    check_all_breaks(Breaks,Continued)
    end.
%%%-----------------------------------------------------------------
%%% Internal functions


check_all_breaks([{From,Case,T,Start}|Breaks],[{From,End}|Continued]) ->
    Diff = timer:now_diff(End,Start),
    DiffSec = round(Diff/1000000),
    TSec = round(T/1000000),
    if DiffSec==TSec ->
	    ?t:format("Break in ~p successfully continued after ~p second(s)~n",
		      [Case,DiffSec]),
	    check_all_breaks(Breaks,Continued);
       true ->
	    ?t:format("Faulty duration of break in ~p: continued after ~p second(s)~n",
		      [Case,DiffSec]),
	    ?t:fail({faulty_diff,Case,DiffSec,TSec})
    end;
check_all_breaks([],[]) ->
    ok;
check_all_breaks(Breaks,Continued) ->
    %% This is probably a case of a missing continue - i.e. a break
    %% has been started, but it was never continued.
    ?t:fail({no_match_in_breaks_and_continued,Breaks,Continued}).

break_and_check(Case) ->
    break_and_continue_sup ! {break,Case,1000,self()},
    ?t:break(atom_to_list(Case)),
    break_and_continue_sup ! {continued,self()},
    ok.

break_and_continue_sup() ->
    register(break_and_continue_sup,self()),
    break_and_continue_loop([],[]).

break_and_continue_loop(Breaks,Continued) ->
    receive
	{break,Case,T,From} ->
	    Start = now(),
	    {RealT,_} = timer:tc(?t,adjusted_sleep,[T]),
	    ?t:continue(),
	    break_and_continue_loop([{From,Case,RealT,Start}|Breaks],Continued);
	{continued,From} ->
	    break_and_continue_loop(Breaks,[{From,now()}|Continued]);
	{done,From} ->
	    From ! {lists:reverse(Breaks),lists:reverse(Continued)}
    end.
