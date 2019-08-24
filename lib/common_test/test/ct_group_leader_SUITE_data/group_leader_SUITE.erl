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
-module(group_leader_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% @spec suite() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{seconds,10}}].

%%--------------------------------------------------------------------
%% @spec init_per_suite(Config0) ->
%%     Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    start_my_io_server(),
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_suite(Config0) -> void() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    my_io_server ! die,
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_group(GroupName, Config0) ->
%%               void() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_testcase(TestCase, Config0) ->
%%               void() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec groups() -> [Group]
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%% N = integer() | forever
%% @end
%%--------------------------------------------------------------------
groups() ->
    [{p,[parallel],[p1,p2]},
     {p_restart,[parallel],[p_restart_my_io_server]},
     {seq,[],[s1,s2,s3]},
     {seq2,[],[s4,s5]},
     {seq_in_par,[parallel],[p10,p11,{group,seq},p12,{group,seq2},p13]},
     {capture_io,[parallel],[cap1,cap2]},
     {unexpected_io,[parallel],[unexp1,unexp2]}].

%%--------------------------------------------------------------------
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
all() ->
    [tc1,{group,p},{group,p_restart},p3,
     {group,seq_in_par},
     cap1,cap2,
     {group,capture_io},
     {group,unexpected_io}].

tc1(_C) ->
    ok.

p1(_) ->
    %% OTP-10101:
    %%
    %% External apps/processes started by init_per_suite (common operation),
    %% will inherit the group leader of the init_per_suite process, i.e. the
    %% test_server test case control process (executing run_test_case_msgloop/7).
    %% If, later, a parallel test case triggers the external app to print with
    %% e.g. io:format() (also common operation), the calling process will hang!
    %% The reason for this is that a parallel test case has a dedicated IO
    %% server process, other than the central test case control process. The
    %% latter process is not executing run_test_case_msgloop/7 and will not
    %% respond to IO messages. The process is still group leader for the
    %% external app, however, which is wrong. It's the IO process for the
    %% parallel test case that should be group leader - but only for the
    %% particular invokation, since other parallel test cases could be
    %% invoking the external app too.
    print("hej\n").

p2(_) ->
    print("hopp\n").

p_restart_my_io_server(_) ->
    %% Restart the IO server and change its group leader. This used
    %% to set to the group leader to a process that would soon die.
    Ref = erlang:monitor(process, my_io_server),
    my_io_server ! die,
    receive
	{'DOWN',Ref,_,_,_} ->
	    start_my_io_server()
    end.

p3(_) ->
    %% OTP-10125. This would crash since the group leader process
    %% for the my_io_server had died.
    print("hoppsan\n").

print(String) ->
    my_io_server ! {print,self(),String},
    receive
	{printed,String} ->
	    ok
    end.

start_my_io_server() ->
    Parent = self(),
    Pid = spawn(fun() -> my_io_server(Parent) end),
    receive
	{Pid,started} ->
	    io:format("~p\n", [process_info(Pid)]),
	    ok
    end.

my_io_server(Parent) ->
    register(my_io_server, self()),
    Parent ! {self(),started},
    my_io_server_loop().

my_io_server_loop() ->
    receive
	{print,From,String} ->
	    io:put_chars(String),
	    From ! {printed,String},
	    my_io_server_loop();
	die ->
	    ok
    end.

p10(_) ->
    receive after 1 -> ok end.

p11(_) ->
    ok.

p12(_) ->
    ok.

p13(_) ->
    ok.

s1(_) ->
    ok.

s2(_) ->
    ok.

s3(_) ->
    ok.

s4(_) ->
    ok.

s5(_) ->
    ok.

cap1(_) ->
    ct:capture_start(),
    IO = gen_io(cap1, 10, []),
    ct:capture_stop(),
    IO = ct:capture_get(),
    ok.

cap2(_) ->
    ct:capture_start(),
    {Pid,Ref} = spawn_monitor(fun() ->
				      exit(gen_io(cap2, 42, []))
			      end),
    receive
	{'DOWN',Ref,process,Pid,IO} ->
	    ct:capture_stop(),
	    IO = ct:capture_get(),
	    ok
    end.

gen_io(_, 0, Acc) ->
    lists:reverse(Acc);
gen_io(Label, N, Acc) ->
    S = lists:flatten(io_lib:format("~s: ~p\n", [Label,N])),
    io:put_chars(S),
    gen_io(Label, N-1, [S|Acc]).

%% Test that unexpected I/O is sent to test_server's unexpeced_io log.
%% To trigger this, run two test cases in parallel and send a printout
%% (via ct logging functions) from an external process which has a
%% different group leader than the test cases.
unexp1(Config) ->
    ct:sleep(1000),
    gen_unexp_io(),
    ct:sleep(1000),
    check_unexp_io(Config),
    ok.

unexp2(_) ->
    ct:sleep(2000),
    ok.

gen_unexp_io() ->
    spawn(fun() ->
		  group_leader(whereis(user),self()),
		  ct:log("-x- Unexpected io ct:log -x-",[]),
		  ct:pal("-x- Unexpected io ct:pal -x-",[]),
		  ok
	  end).

check_unexp_io(Config) ->
    SuiteLog = ?config(tc_logfile,Config),
    Dir = filename:dirname(SuiteLog),
    UnexpLog = filename:join(Dir,"unexpected_io.log.html"),
    {ok,SuiteBin} = file:read_file(SuiteLog),
    nomatch = re:run(SuiteBin,"-x- Unexpected io ",[global,{capture,none}]),
    {ok,UnexpBin} = file:read_file(UnexpLog),
    {match,[_,_]} = re:run(UnexpBin,"-x- Unexpected io ",[global]),
    ok.
