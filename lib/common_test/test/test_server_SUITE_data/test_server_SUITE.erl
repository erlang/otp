%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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

%%%------------------------------------------------------------------
%%% Test Server self test. 
%%%------------------------------------------------------------------
-module(test_server_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/file.hrl").
-export([all/1]).

-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2, fin_per_testcase/2]).
-export([config/1, comment/1, timetrap/1, timetrap_cancel/1, multiply_timetrap/1,
	 init_per_s/1, init_per_tc/1, end_per_tc/1,
	 timeconv/1, msgs/1, capture/1, timecall/1,
	 do_times/1, do_times_mfa/1, do_times_fun/1,
	 skip_cases/1, skip_case1/1, skip_case2/1, skip_case3/1, 
	 skip_case4/1, skip_case5/1, skip_case6/1, skip_case7/1,
	 skip_case8/1, skip_case9/1,
	 conf_init/1, check_new_conf/1, conf_cleanup/1,
	 check_old_conf/1, conf_init_fail/1, start_stop_node/1,
	 cleanup_nodes_init/1, check_survive_nodes/1, cleanup_nodes_fin/1,
	 commercial/1,
	 io_invalid_data/1, print_unexpected/1]).

-export([dummy_function/0,dummy_function/1,doer/1]).

all(doc) -> ["Test Server self test"];
all(suite) ->
    [config, comment, timetrap, timetrap_cancel, multiply_timetrap,
     init_per_s, init_per_tc, end_per_tc,
     timeconv, msgs, capture, timecall, do_times, skip_cases,
     commercial, io_invalid_data, print_unexpected,
     {conf, conf_init, [check_new_conf], conf_cleanup},
     check_old_conf,
     {conf, conf_init_fail,[conf_member_skip],conf_cleanup_skip},
     start_stop_node,
     {conf, cleanup_nodes_init,[check_survive_nodes],cleanup_nodes_fin},
     config
    ].


init_per_suite(Config) ->
    [{init_per_suite_var,ok}|Config].

end_per_suite(_Config) ->
    ok.

init_per_testcase(Func, Config) when is_atom(Func), is_list(Config) ->
    Dog = ?t:timetrap(?t:minutes(2)),
    Config1 = [{watchdog, Dog}|Config],
    case Func of
	init_per_tc ->
	    [{strange_var, 1}|Config1];
	skip_case8 -> 
	    {skipped, "This case should be noted as `Skipped'"};
	skip_case9 ->
	    {skip, "This case should be noted as `Skipped'"};
	_ ->
	    Config1
    end;
init_per_testcase(Func, Config) ->
    io:format("Func:~p",[Func]),
    io:format("Config:~p",[Config]),
    ?t:fail("Arguments to init_per_testcase not correct").

end_per_testcase(Func, Config) when is_atom(Func), is_list(Config) ->
    Dog=?config(watchdog, Config),
    ?t:timetrap_cancel(Dog),
    case Func of
	end_per_tc -> io:format("CLEANUP => this test case is ok\n");
	_Other -> ok
    end;
end_per_testcase(Func, Config) ->
    io:format("Func:~p",[Func]),
    io:format("Config:~p",[Config]),
    ?t:fail("Arguments to end_per_testcase not correct").

fin_per_testcase(Func, Config) ->
    io:format("Func:~p",[Func]),
    io:format("Config:~p",[Config]),
    ?t:fail("fin_per_testcase/2 called, should have called end_per_testcase/2").
    

config(suite) -> [];
config(doc) -> ["Test that the Config variable is decent, ",
		"and that the std config variables are correct ",
		"(check that data/priv dir exists)."
		"Also check that ?config macro works."];
config(Config) when is_list(Config) ->
    is_tuplelist(Config),
    {value,{data_dir,Dd}}=lists:keysearch(data_dir,1,Config),
    {value,{priv_dir,Dp}}=lists:keysearch(priv_dir,1,Config),
    true=is_dir(Dd),
    {ok, _Bin}=file:read_file(filename:join(Dd, "dummy_file")),
    true=is_dir(Dp),

    Dd = ?config(data_dir,Config),
    Dp = ?config(priv_dir,Config),
    ok;
config(_Config) ->
    ?t:fail("Config variable is not a list.").

is_tuplelist([]) ->
    true;
is_tuplelist([{_A,_B}|Rest]) ->
    is_tuplelist(Rest);
is_tuplelist(_) ->
    false.

is_dir(Dir) ->
    case file:read_file_info(Dir) of
	{ok, #file_info{type=directory}} ->
	    true;
	_ ->
	    false
    end.

comment(suite) -> [];
comment(doc) -> ["Print a comment in the HTML log"];
comment(Config) when is_list(Config) ->
    ?t:comment("This comment should not occur in the HTML log because a later"
	       " comment shall overwrite it"),
    ?t:comment("This comment is printed with the comment/1 function."
	       " It should occur in the HTML log").



timetrap(suite) -> [];
timetrap(doc) -> ["Test that timetrap works."];
timetrap(Config) when is_list(Config) ->
    TrapAfter = 3000,
    Dog=?t:timetrap(TrapAfter),
    process_flag(trap_exit, true),
    TimeOut = TrapAfter * test_server:timetrap_scale_factor() + 1000,
    receive
	{'EXIT', Dog, {timetrap_timeout, _, _}} ->
	    ok;
	{'EXIT', _OtherPid, {timetrap_timeout, _, _}} ->
	    ?t:fail("EXIT signal from wrong process")
    after
	TimeOut ->
	    ?t:fail("Timetrap is not working.")
    end,
    ?t:timetrap_cancel(Dog),
    ok.


timetrap_cancel(suite) -> [];
timetrap_cancel(doc) -> ["Test that timetrap_cancel works."];
timetrap_cancel(Config) when is_list(Config) ->
    Dog=?t:timetrap(1000),
    receive
    after
	500 ->
	    ok
    end,
    ?t:timetrap_cancel(Dog),
    receive
    after 1000 ->
	    ok
    end,
    ok.

multiply_timetrap(suite) -> [];
multiply_timetrap(doc) -> ["Test multiply timetrap"];
multiply_timetrap(Config) when is_list(Config) ->
    %% This simulates the call to test_server_ctrl:multiply_timetraps/1:
    put(test_server_multiply_timetraps,{2,true}),

    Dog = ?t:timetrap(500),
    timer:sleep(800),
    ?t:timetrap_cancel(Dog),

    %% Reset
    put(test_server_multiply_timetraps,1),
    ok.


init_per_s(suite) -> [];
init_per_s(doc) -> ["Test that a Config that is altered in ",
		     "init_per_suite gets through to the testcases."];
init_per_s(Config) ->
    %% Check that the config var sent from init_per_suite
    %% really exists.
    {value, {init_per_suite_var, ok}} = 
	lists:keysearch(init_per_suite_var,1,Config),

    %% Check that the other variables still exist.
    {value,{data_dir,_Dd}}=lists:keysearch(data_dir,1,Config),
    {value,{priv_dir,_Dp}}=lists:keysearch(priv_dir,1,Config),
    ok.

init_per_tc(suite) -> [];
init_per_tc(doc) -> ["Test that a Config that is altered in ",
		     "init_per_testcase gets through to the ",
		     "actual testcase."];
init_per_tc(Config) ->
    %% Check that the config var sent from init_per_testcase
    %% really exists.
    {value, {strange_var, 1}} = lists:keysearch(strange_var,1,Config),

    %% Check that the other variables still exist.
    {value,{data_dir,_Dd}}=lists:keysearch(data_dir,1,Config),
    {value,{priv_dir,_Dp}}=lists:keysearch(priv_dir,1,Config),
    ok.

end_per_tc(suite) -> [];
end_per_tc(doc) -> ["Test that end_per_testcase/2 is called even if"
		    " test case fails"];
end_per_tc(Config) when is_list(Config) ->
    ?t:fail("This case should fail! Check that \"CLEANUP\" is"
	    " printed in the minor log file.").


timeconv(suite) -> [];
timeconv(doc) -> ["Test that the time unit conversion functions ",
		  "works."];
timeconv(Config) when is_list(Config) ->
    Val=2,
    Secs=Val*1000,
    Mins=Secs*60,
    Hrs=Mins*60,
    Secs=?t:seconds(2),
    Mins=?t:minutes(2),
    Hrs=?t:hours(2),
    ok.


msgs(suite) -> [];
msgs(doc) -> ["Tests the messages_get function."];
msgs(Config) when is_list(Config) ->
    self() ! {hej, du},
    self() ! {lite, "data"},
    self() ! en_atom,
    [{hej, du}, {lite, "data"}, en_atom] = ?t:messages_get(),
    ok.

capture(suite) -> [];
capture(doc) -> ["Test that the capture functions work properly."];
capture(Config) when is_list(Config) ->
    String1="abcedfghjiklmnopqrstuvwxyz",
    String2="0123456789",
    ?t:capture_start(),
    io:format(String1),
    [String1]=?t:capture_get(),
    io:format(String2),
    [String2]=?t:capture_get(),
    ?t:capture_stop(),
    []=?t:capture_get(),
    io:format(String2),
    []=?t:capture_get(),
    ok.

timecall(suite) -> [];
timecall(doc) -> ["Tests that timed calls work."];
timecall(Config) when is_list(Config) ->
    {_Time1, liten_apa_e_oxo_farlig} = ?t:timecall(?MODULE, dummy_function, []),
    {Time2, jag_ar_en_gorilla} = ?t:timecall(?MODULE, dummy_function, [gorilla]),
    DTime=round(Time2),
    if
	DTime<1 ->
	    ?t:fail("Timecall reported a too low time.");
	DTime==1 ->
	    ok;
	DTime>1 ->
	    ?t:fail("Timecall reported a too high time.")
    end,
    ok.

dummy_function() ->
    liten_apa_e_oxo_farlig.
dummy_function(gorilla) ->
    receive after 1000 -> ok end,
    jag_ar_en_gorilla.


do_times(suite) -> [do_times_mfa, do_times_fun];
do_times(doc) -> ["Test the do_times function."].

do_times_mfa(suite) -> [];
do_times_mfa(doc) -> ["Test the do_times function with M,F,A given."];
do_times_mfa(Config) when is_list(Config) ->
    ?t:do_times(100, ?MODULE, doer, [self()]),
    100=length(?t:messages_get()),
    ok.

do_times_fun(suite) -> [];
do_times_fun(doc) -> ["Test the do_times function with fun given."];
do_times_fun(Config) when is_list(Config) ->
    Self = self(),
    ?t:do_times(100, fun() -> doer(Self) end),
    100=length(?t:messages_get()),
    ok.

doer(From) ->
    From ! a,
    ok.

skip_cases(doc) -> ["Test all possible ways to skip a test case."];
skip_cases(suite) -> [skip_case1, skip_case2, skip_case3, skip_case4,
		      skip_case5, skip_case6, skip_case7, skip_case8,
		      skip_case9].

skip_case1(suite) -> [];
skip_case1(doc) -> ["Test that you can return {skipped, Reason},"
		    " and that Reason is in the comment field in the HTML log"];
skip_case1(Config) when is_list(Config) ->
    %% If this comment shows, the case failed!!
    ?t:comment("ERROR: This case should have been noted as `Skipped'"),
    %% The Reason in {skipped, Reason} should overwrite a 'comment'
    {skipped, "This case should be noted as `Skipped'"}.

skip_case2(suite) -> [];
skip_case2(doc) -> ["Test that you can return {skipped, Reason},"
		    " and that Reason is in the comment field in the HTML log"];
skip_case2(Config) when is_list(Config) ->
    %% If this comment shows, the case failed!!
    ?t:comment("ERROR: This case should have been noted as `Skipped'"),
    %% The Reason in {skipped, Reason} should overwrite a 'comment'
    exit({skipped, "This case should be noted as `Skipped'"}).    

skip_case3(suite) -> [];
skip_case3(doc) -> ["Test that you can return {skip, Reason},"
		    " and that Reason is in the comment field in the HTML log"];
skip_case3(Config) when is_list(Config) ->
    %% If this comment shows, the case failed!!
    ?t:comment("ERROR: This case should have been noted as `Skipped'"),
    %% The Reason in {skip, Reason} should overwrite a 'comment'
    {skip, "This case should be noted as `Skipped'"}.

skip_case4(suite) -> [];
skip_case4(doc) -> ["Test that you can return {skip, Reason},"
		    " and that Reason is in the comment field in the HTML log"];
skip_case4(Config) when is_list(Config) ->
    %% If this comment shows, the case failed!!
    ?t:comment("ERROR: This case should have been noted as `Skipped'"),
    %% The Reason in {skip, Reason} should overwrite a 'comment'
    exit({skip, "This case should be noted as `Skipped'"}).    

skip_case5(suite) -> {skipped, "This case should be noted as `Skipped'"};
skip_case5(doc) -> ["Test that you can return {skipped, Reason}"
		    " from the specification clause"].

skip_case6(suite) -> {skip, "This case should be noted as `Skipped'"};
skip_case6(doc) -> ["Test that you can return {skip, Reason}"
		    " from the specification clause"].

skip_case7(suite) -> [];
skip_case7(doc) -> ["Test that skip works from a test specification file"];
skip_case7(Config) when is_list(Config) ->
    %% This case shall be skipped by adding 
    %% {skip, {test_server_SUITE, skip_case7, Reason}}. 
    %% to the test specification file.
    ?t:fail("This case should have been Skipped by the .spec file").

skip_case8(suite) -> [];
skip_case8(doc) -> ["Test that {skipped, Reason} works from"
		    " init_per_testcase/2"];
skip_case8(Config) when is_list(Config) ->
    %% This case shall be skipped by adding a specific clause to 
    %% returning {skipped, Reason} from init_per_testcase/2 for this case. 
    ?t:fail("This case should have been Skipped by init_per_testcase/2").

skip_case9(suite) -> [];
skip_case9(doc) -> ["Test that {skip, Reason} works from a init_per_testcase/2"];
skip_case9(Config) when is_list(Config) ->
    %% This case shall be skipped by adding a specific clause to 
    %% returning {skip, Reason} from init_per_testcase/2 for this case. 
    ?t:fail("This case should have been Skipped by init_per_testcase/2").

conf_init(doc) -> ["Test successful conf case: Change Config parameter"];
conf_init(Config) when is_list(Config) ->
    [{conf_init_var,1389}|Config].

check_new_conf(suite) -> [];
check_new_conf(doc) -> ["Check that Config parameter changed by"
			" conf_init is used"];
check_new_conf(Config) when is_list(Config) ->
    1389 = ?config(conf_init_var,Config),
    ok.

conf_cleanup(doc) -> ["Test successful conf case: Restore Config parameter"];
conf_cleanup(Config) when is_list(Config) ->
    lists:keydelete(conf_init_var,1,Config).

check_old_conf(suite) -> [];
check_old_conf(doc) -> ["Test that the restored Config is used after a"
			" conf cleanup"];
check_old_conf(Config) when is_list(Config) ->
    undefined = ?config(conf_init_var,Config),
    ok.

conf_init_fail(doc) -> ["Test that config members are skipped if"
			" conf init function fails."];
conf_init_fail(Config) when is_list(Config) -> 
    ?t:fail("This case should fail! Check that conf_member_skip and"
	    " conf_cleanup_skip are skipped.").



start_stop_node(suite) -> [];
start_stop_node(doc) -> ["Test start and stop of slave and peer nodes"];
start_stop_node(Config) when is_list(Config) ->
    {ok,Node2} = ?t:start_node(node2,peer,[]),
    {error, _} = ?t:start_node(node2,peer,[{fail_on_error,false}]),
    true = lists:member(Node2,nodes()),

    {ok,Node3} = ?t:start_node(node3,slave,[]),
    {error, _} = ?t:start_node(node3,slave,[]),
    true = lists:member(Node3,nodes()),

    {ok,Node4} = ?t:start_node(node4,peer,[{wait,false}]),
    case lists:member(Node4,nodes()) of
	true -> 
	    ?t:comment("WARNING: Node started with {wait,false}"
			     " is up faster than expected...");
	false ->
	    test_server:wait_for_node(Node4),
	    true = lists:member(Node4,nodes())
    end,

    true = ?t:stop_node(Node2),
    false = lists:member(Node2,nodes()),

    true = ?t:stop_node(Node3),
    false = lists:member(Node3,nodes()),
    
    true = ?t:stop_node(Node4),
    false = lists:member(Node4,nodes()),
    timer:sleep(2000),
    false = ?t:stop_node(Node4),

    ok.

cleanup_nodes_init(doc) -> ["Test that nodes are terminated when test case"
			    " is finished unless {cleanup,false} is given."];
cleanup_nodes_init(Config) when is_list(Config) ->
    {ok,DieSlave} = ?t:start_node(die_slave, slave, []),
    {ok,SurviveSlave} = ?t:start_node(survive_slave, slave, [{cleanup,false}]),
    {ok,DiePeer} = ?t:start_node(die_peer, peer, []),
    {ok,SurvivePeer} = ?t:start_node(survive_peer, peer, [{cleanup,false}]),
    [{die_slave,DieSlave},
     {survive_slave,SurviveSlave},
     {die_peer,DiePeer},
     {survive_peer,SurvivePeer} | Config].



check_survive_nodes(suite) -> [];
check_survive_nodes(doc) -> ["Test that nodes with {cleanup,false} survived"];
check_survive_nodes(Config) when is_list(Config) ->
    timer:sleep(1000),
    false = lists:member(?config(die_slave,Config),nodes()),
    true = lists:member(?config(survive_slave,Config),nodes()),
    false = lists:member(?config(die_peer,Config),nodes()),
    true = lists:member(?config(survive_peer,Config),nodes()),
    ok.


cleanup_nodes_fin(doc) -> ["Test that nodes started with {cleanup,false}"
			   " can be stopped"];
cleanup_nodes_fin(Config) when is_list(Config) ->
    Slave = ?config(survive_slave,Config),
    Peer = ?config(survive_peer,Config),
    
    true = ?t:stop_node(Slave),
    false = lists:member(Slave,nodes()),
    true = ?t:stop_node(Peer),
    false = lists:member(Peer,nodes()),
    
    C1 = lists:keydelete(die_slave,1,Config),
    C2 = lists:keydelete(survive_slave,1,C1),
    C3 = lists:keydelete(die_peer,1,C2),
    lists:keydelete(survive_peer,1,C3).

commercial(Config) when is_list(Config) ->
    case ?t:is_commercial() of
	false -> {comment,"Open-source build"};
	true -> {comment,"Commercial build"}
    end.

io_invalid_data(Config) when is_list(Config) ->
    ok = io:put_chars("valid: " ++ [42]),
    %% OTP-10991 caused this to hang and produce a timetrap timeout:
    {'EXIT',{badarg,_}} = (catch io:put_chars("invalid: " ++ [42.0])),
    ok.

print_unexpected(Config) when is_list(Config) ->
    Str = "-x-x-x- test_server_SUITE:print_unexpected -> Unexpected data -x-x-x-",
    test_server_io:print_unexpected(Str),
    UnexpectedLog = filename:join(filename:dirname(?config(tc_logfile,Config)),
				  "unexpected_io.log.html"),
    {ok,Bin} = file:read_file(UnexpectedLog),
    match = re:run(Bin, Str, [global,{capture,none}]),
    ok.
