%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2018. All Rights Reserved.
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
-module(proc_lib_SUITE).

%%
%% Define to run outside of test server
%%
%%-define(STANDALONE,1).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, 
	 crash/1, stacktrace/1, sync_start_nolink/1, sync_start_link/1,
         spawn_opt/1, sp1/0, sp2/0, sp3/1, sp4/2, sp5/1, '\x{447}'/0,
	 hibernate/1, stop/1, t_format/1, t_format_arbitrary/1]).
-export([ otp_6345/1, init_dont_hang/1]).

-export([hib_loop/1, awaken/1]).

-export([init/1,
	 handle_event/2, handle_call/2, handle_info/2,
	 terminate/2]).

-export([otp_6345_init/1, init_dont_hang_init/1]).

-export([system_terminate/4]).

-ifdef(STANDALONE).
-define(line, noop, ).
-else.
-include_lib("common_test/include/ct.hrl").
-endif.

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [crash, stacktrace, {group, sync_start}, spawn_opt, hibernate,
     {group, tickets}, stop, t_format, t_format_arbitrary].

groups() -> 
    [{tickets, [], [otp_6345, init_dont_hang]},
     {sync_start, [], [sync_start_nolink, sync_start_link]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.



%%-----------------------------------------------------------------
%% We don't have to test that spwn and spawn_link actually spawns
%% new processes - if they don't we can't run this suite!
%% But we want to test that start and start_link really is
%% synchronous, and we want to test that the crash report is ok.
%%-----------------------------------------------------------------
crash(Config) when is_list(Config) ->
    ok = application:unset_env(kernel, error_logger_format_depth),
    crash_1(Config),
    ok = application:set_env(kernel, error_logger_format_depth, 30),
    crash_1(Config),
    ok = application:unset_env(kernel, error_logger_format_depth),
    ok.

crash_1(_Config) ->
    error_logger:add_report_handler(?MODULE, self()),

    %% Make sure that we don't get a crash report if a process
    %% terminates with reason 'shutdown' or reason {shutdown,Reason}.
    process_flag(trap_exit, true),
    Pid0 = proc_lib:spawn_link(erlang, apply,
			       [fun() -> exit(shutdown) end,[]]),
    Pid1 = proc_lib:spawn_link(erlang, apply,
			       [fun() -> exit({shutdown,{a,b,c}}) end,[]]),

    receive {'EXIT',Pid0,shutdown} -> ok end,
    receive {'EXIT',Pid1,{shutdown,{a,b,c}}} -> ok end,
    process_flag(trap_exit, false),
    %% We expect any unexpected messages to be caught below,
    %% so we don't have explicitly wait some time to be sure.

    %% Spawn export function.
    Pid2 = proc_lib:spawn(?MODULE, sp1, []),
    Pid2 ! die,
    Exp2 = [{initial_call,{?MODULE,sp1,[]}},
	    {ancestors,[self()]},
	    {error_info,{exit,die,{stacktrace}}}],
    analyse_crash(Pid2, Exp2, []),

    %% Spawn fun.
    F = fun sp1/0,
    Pid3 = proc_lib:spawn(node(), F),
    Pid3 ! die,
    {module,?MODULE} = erlang:fun_info(F, module),
    {name,Fname} = erlang:fun_info(F, name),
    Exp3 = [{initial_call,{?MODULE,Fname,[]}},
	    {ancestors,[self()]},
	    {error_info,{exit,die,{stacktrace}}}],
    analyse_crash(Pid3, Exp3, []),

    %% Spawn function with neighbour.
    Pid4 = proc_lib:spawn(?MODULE, sp2, []),
    ct:sleep(100),
    {?MODULE,sp2,[]} = proc_lib:initial_call(Pid4),
    {?MODULE,sp2,0} = proc_lib:translate_initial_call(Pid4),
    Pid4 ! die,
    Exp4 = [{initial_call,{?MODULE,sp2,[]}},
	    {ancestors,[self()]},
	    {error_info,{exit,die,{stacktrace}}}],
    Links4 = [[{initial_call,{?MODULE,sp1,[]}},
	       {ancestors,[Pid4,self()]}]],
    analyse_crash(Pid4, Exp4, Links4),

    %% Make sure that we still get a crash report if the
    %% process dictionary have been tampered with.

    Pid5 = proc_lib:spawn(erlang, apply,
			  [fun() ->
				   erase(),
				   exit(abnormal)
			   end,[]]),
    Exp5 = [{initial_call,absent},
	    {ancestors,[]},
	    {error_info,{exit,abnormal,{stacktrace}}}],
    analyse_crash(Pid5, Exp5, []),

    %% Unicode atom
    Pid6 = proc_lib:spawn(?MODULE, '\x{447}', []),
    Pid6 ! die,
    Exp6 = [{initial_call,{?MODULE,'\x{447}',[]}},
	    {ancestors,[self()]},
	    {error_info,{exit,die,{stacktrace}}}],
    analyse_crash(Pid6, Exp6, []),

    error_logger:delete_report_handler(?MODULE),
    ok.

analyse_crash(Pid, Expected0, ExpLinks) ->
    Expected = [{pid,Pid}|Expected0],
    receive
	{crash_report, Pid, Report} ->
	    _ = proc_lib:format(Report),	%Smoke test.
	    [Crash,Links] = Report,
	    analyse_crash_1(Expected, Crash),
	    analyse_links(ExpLinks, Links);
	Unexpected ->
	    io:format("~p\n", [Unexpected]),
	    ct:fail(unexpected_message)
    after 5000 ->
	    ct:fail(no_crash_report)
    end.

analyse_links([H|Es], [{neighbour,N}|Links]) ->
    analyse_crash_1(H, N),
    analyse_links(Es, Links);
analyse_links([], []) ->
    ok.

analyse_crash_1([{Key,absent}|T], Report) ->
    false = lists:keymember(Key, 1, Report),
    analyse_crash_1(T, Report);
analyse_crash_1([{Key,Pattern}|T], Report) ->
    case lists:keyfind(Key, 1, Report) of
	false ->
	    io:format("~p", [Report]),
	    ct:fail({missing_key,Key});
	{Key,Info} ->
	    try
		match_info(Pattern, Info)
	    catch
		no_match ->
		    io:format("key: ~p", [Key]),
		    io:format("pattern: ~p", [Pattern]),
		    io:format("actual: ~p", [Report]),
		    ct:fail(no_match)
	    end,
	    analyse_crash_1(T, Report)
    end;
analyse_crash_1([], _Report) ->
    [].

match_info(T, T) ->
    ok;
match_info({stacktrace}, Stk) when is_list(Stk) ->
    ok;
match_info([H1|T1], [H2|T2]) ->
    match_info(H1, H2),
    match_info(T1, T2);
match_info(Tuple1, Tuple2) when tuple_size(Tuple1) =:= tuple_size(Tuple2) ->
    match_info(tuple_to_list(Tuple1), tuple_to_list(Tuple2));
match_info(_, _) ->
    throw(no_match).

stacktrace(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    %% Errors.
    Pid1 = proc_lib:spawn_link(fun() -> 1 = 2 end),
    receive
	{'EXIT',Pid1,{{badmatch,2},_Stack1}} -> ok
    after 500 ->
	ct:fail(error)
    end,
    %% Exits.
    Pid2 = proc_lib:spawn_link(fun() -> exit(bye) end),
    receive
	{'EXIT',Pid2,bye} -> ok
    after 500 ->
	ct:fail(exit)
    end,
    %% Throws.
    Pid3 = proc_lib:spawn_link(fun() -> throw(ball) end),
    receive
	{'EXIT',Pid3,{{nocatch,ball},_Stack3}} -> ok
    after 500 ->
	ct:fail(throw)
    end,
    ok.

sync_start_nolink(Config) when is_list(Config) ->
    _Pid = spawn_link(?MODULE, sp5, [self()]),
    receive
	{sync_started, F} -> 
	    exit(F, kill),
	    ct:fail(async_start)
    after 1000 -> ok
    end,
    receive
	{Pid2, init} ->
	    Pid2 ! go_on
    end,
    receive
	{sync_started, _} -> ok
    after 1000 ->
	    exit(Pid2, kill),
	    ct:fail(no_sync_start)
    end,
    ok.

sync_start_link(Config) when is_list(Config) ->
    _Pid = spawn_link(?MODULE, sp3, [self()]),
    receive
	{sync_started, _} -> ct:fail(async_start)
    after 1000 -> ok
    end,
    receive
	{Pid2, init} ->
	    Pid2 ! go_on
    end,
    receive
	{sync_started, _} -> ok
    after 1000 -> ct:fail(no_sync_start)
    end,
    ok.

spawn_opt(Config) when is_list(Config) ->
    F = fun sp1/0,
    {name,Fname} = erlang:fun_info(F, name),
    FunMFArgs = {?MODULE,Fname,[]},
    FunMFArity = {?MODULE,Fname,0},
    Pid1 = proc_lib:spawn_opt(node(), F, [{priority,low}]),
    Pid = proc_lib:spawn_opt(F, [{priority,low}]),
    ct:sleep(100),
    FunMFArgs = proc_lib:initial_call(Pid),
    FunMFArity = proc_lib:translate_initial_call(Pid),
    Pid ! die,
    FunMFArgs = proc_lib:initial_call(Pid1),
    FunMFArity = proc_lib:translate_initial_call(Pid1),
    Pid1 ! die,
    ok.


sp1() ->
    receive 
	die -> exit(die);
	_ -> sp1()
    end.

sp2() ->
    _Pid = proc_lib:spawn_link(?MODULE, sp1, []),
    receive 
	die -> exit(die);
	_ -> sp1()
    end.

sp3(Tester) ->
    Pid = proc_lib:start_link(?MODULE, sp4, [self(), Tester]),
    Tester ! {sync_started, Pid}.

sp5(Tester) ->
    Pid = proc_lib:start(?MODULE, sp4, [self(), Tester]),
    Tester ! {sync_started, Pid}.

sp4(Parent, Tester) ->
    Tester ! {self(), init},
    receive
	go_on -> ok
    end,
    proc_lib:init_ack(Parent, self()).

'\x{447}'() ->
    receive
	die -> exit(die);
	_ -> sp1()
    end.

hibernate(Config) when is_list(Config) ->
    Ref = make_ref(),
    Self = self(),
    LoopData = {Ref,Self},
    Pid = proc_lib:spawn_link(?MODULE, hib_loop, [LoopData]),

    %% Just check that the child process can process and answer messages.
    Pid ! {Self,loop_data},
    receive
	{loop_data,LoopData} -> ok;
	Unexpected0 ->
	    io:format("Unexpected: ~p\n", [Unexpected0]),
	    ct:fail(failed)
    after 1000 ->
	    io:format("Timeout"),
	    ct:fail(failed)
    end,

    %% Hibernate the process.
    Pid ! hibernate,
    erlang:yield(),
    io:format("~p\n", [process_info(Pid, heap_size)]),


    %% Send a message to the process...

    Pid ! {Self,loop_data},

    %% ... expect first a wake up message from the process...
    receive
	{awaken,LoopData} -> ok;
	Unexpected1 ->
	    io:format("Unexpected: ~p\n", [Unexpected1]),
	    ct:fail(failed)
    after 1000 ->
	    io:format("Timeout"),
	    ct:fail(failed)
    end,

    %% ... followed by the answer to the actual request.
    receive
	{loop_data,LoopData} -> ok;
	Unexpected2 ->
	    io:format("Unexpected: ~p\n", [Unexpected2]),
	    ct:fail(failed)
    after 1000 ->
	    io:format("Timeout"),
	    ct:fail(failed)
    end,

    %% Test that errors are handled correctly after wake up from hibernation...

    process_flag(trap_exit, true),
    error_logger:add_report_handler(?MODULE, self()),
    Pid ! crash,

    %% We should receive two messages. Especially in the SMP emulator,
    %% we can't be sure of the message order, so sort the messages before
    %% matching.

    Messages = lists:sort(hib_receive_messages(2)),
    io:format("~p", [Messages]),
    [{'EXIT',Pid,i_crashed},{crash_report,Pid,[Report,[]]}] = Messages,

    %% Check that the initial_call has the expected format.
    {value,{initial_call,{?MODULE,hib_loop,[_]}}} =
	lists:keysearch(initial_call, 1, Report),

    error_logger:delete_report_handler(?MODULE),
    ok.

hib_loop(LoopData) ->
    receive
	hibernate ->
	    proc_lib:hibernate(?MODULE, awaken, [LoopData]);
	{Pid,loop_data} ->
	    Pid ! {loop_data,LoopData};
	crash ->
	    exit(i_crashed)
    end,
    hib_loop(LoopData).

awaken({_,Parent}=LoopData) ->
    Parent ! {awaken,LoopData},
    hib_loop(LoopData).

hib_receive_messages(0) -> [];
hib_receive_messages(N) ->
    receive
	Any -> [Any|hib_receive_messages(N-1)]
    end.

%% 'monitor' spawn_opt option.
otp_6345(Config) when is_list(Config) ->
    Opts = [link,monitor],
    {'EXIT', {badarg,[{proc_lib,check_for_monitor,_,_}|_Stack]}} =
	(catch proc_lib:start(?MODULE, otp_6345_init, [self()],
			      1000, Opts)),
    ok.

otp_6345_init(Parent) ->
    proc_lib:init_ack(Parent, {ok, self()}),
    otp_6345_loop().

otp_6345_loop() ->
    receive
	_Msg ->
	    otp_6345_loop()
    end.

%% OTP-9803. Check that proc_lib:start() doesn't hang if spawned process
%% crashes before proc_lib:init_ack/2.
init_dont_hang(Config) when is_list(Config) ->
    %% Start should behave as start_link
    process_flag(trap_exit, true),
    StartLinkRes = proc_lib:start_link(?MODULE, init_dont_hang_init, [self()]),
    try
	StartLinkRes = proc_lib:start(?MODULE, init_dont_hang_init, [self()], 1000),
	StartLinkRes = proc_lib:start(?MODULE, init_dont_hang_init, [self()], 1000, []),
	ok
    catch _:Error:Stacktrace ->
	    io:format("Error ~p /= ~p ~n",[Stacktrace, StartLinkRes]),
	    exit(Error)
    end.

init_dont_hang_init(_Parent) ->
    error(bad_init).

%% Test proc_lib:stop/1,3
stop(_Config) ->
    Parent = self(),
    SysMsgProc =
	fun() ->
		receive
		    {system,From,Request} ->
			sys:handle_system_msg(Request,From,Parent,?MODULE,[],[])
		end
	end,

    %% Normal case:
    %% Process handles system message and terminated with given reason
    Pid1 = proc_lib:spawn(SysMsgProc),
    ok = proc_lib:stop(Pid1),
    false = erlang:is_process_alive(Pid1),

    %% Process does not exit
    {'EXIT',noproc} = (catch proc_lib:stop(Pid1)),

    %% Badly handled system message
    DieProc =
	fun() ->
		receive
		    {system,_From,_Request} ->
			exit(die)
		end
	end,
    Pid2 = proc_lib:spawn(DieProc),
    {'EXIT',{die,_}} = (catch proc_lib:stop(Pid2)),

    %% Hanging process => timeout
    HangProc =
	fun() ->
		receive
		    {system,_From,_Request} ->
			timer:sleep(5000)
		end
	end,
    Pid3 = proc_lib:spawn(HangProc),
    {'EXIT',timeout} = (catch proc_lib:stop(Pid3,normal,1000)),

    %% Success case with other reason than 'normal'
    Pid4 = proc_lib:spawn(SysMsgProc),
    ok = proc_lib:stop(Pid4,other_reason,infinity),
    false = erlang:is_process_alive(Pid4),

    %% System message is handled, but process dies with other reason
    %% than the given (in system_terminate/4 below)
    Pid5 = proc_lib:spawn(SysMsgProc),
    {'EXIT',{{badmatch,2},_Stacktrace}} = (catch proc_lib:stop(Pid5,crash,infinity)),
    false = erlang:is_process_alive(Pid5),

    %% Local registered name
    Pid6 = proc_lib:spawn(SysMsgProc),
    register(to_stop,Pid6),
    ok = proc_lib:stop(to_stop),
    undefined = whereis(to_stop),
    false = erlang:is_process_alive(Pid6),

    %% Remote registered name
    {ok,Node} = test_server:start_node(proc_lib_SUITE_stop,slave,[]),
    Dir = filename:dirname(code:which(?MODULE)),
    rpc:call(Node,code,add_path,[Dir]),
    Pid7 = spawn(Node,SysMsgProc),
    true = rpc:call(Node,erlang,register,[to_stop,Pid7]),
    Pid7 = rpc:call(Node,erlang,whereis,[to_stop]),
    ok = proc_lib:stop({to_stop,Node}),
    undefined = rpc:call(Node,erlang,whereis,[to_stop]),
    false = rpc:call(Node,erlang,is_process_alive,[Pid7]),

    %% Local and remote registered name, but non-existing
    {'EXIT',noproc} = (catch proc_lib:stop(to_stop)),
    {'EXIT',noproc} = (catch proc_lib:stop({to_stop,Node})),

    true = test_server:stop_node(Node),

    %% Remote registered name, but non-existing node
    {'EXIT',{{nodedown,Node},_}} = (catch proc_lib:stop({to_stop,Node})),
    ok.

system_terminate(crash,_Parent,_Deb,_State) ->
    error({badmatch,2});
system_terminate(Reason,_Parent,_Deb,_State) ->
    exit(Reason).


t_format(_Config) ->
    {ok,#{level:=Level}} = logger:get_handler_config(default),
    logger:set_handler_config(default,level,none),
    error_logger:add_report_handler(?MODULE, self()),
    try
	t_format()
    after
        error_logger:delete_report_handler(?MODULE),
        logger:set_handler_config(default,level,Level)
    end,
    ok.

t_format() ->
    Pid = proc_lib:spawn(fun '\x{aaa}t_format_looper'/0),
    HugeData = gb_sets:from_list(lists:seq(1, 100)),
    SomeData1 = list_to_atom([246]),
    SomeData2 = list_to_atom([1024]),
    Pid ! {SomeData1,SomeData2},
    Pid ! {die,{HugeData,SomeData1,SomeData2}},
    Report = receive
		 {crash_report, Pid, Report0} -> Report0
	     end,
    Usz = do_test_format(Report, latin1, unlimited),
    Tsz = do_test_format(Report, latin1, 20),

    if
	Tsz >= Usz ->
	    ct:fail(failed);
	true ->
	    ok
    end,

    UszU = do_test_format(Report, unicode, unlimited),
    TszU = do_test_format(Report, unicode, 20),

    if
	TszU >= UszU ->
	    ct:fail(failed);
	true ->
	    ok
    end,

    ok.

t_format_arbitrary(_Config) ->
    {ok,#{level:=Level}} = logger:get_handler_config(default),
    logger:set_handler_config(default,level,none),
    try
        t_format_arbitrary()
    after
        logger:set_handler_config(default,level,Level)
    end,
    ok.

t_format_arbitrary() ->
    A = list_to_atom([1024]),
    do_test_format([fake_report, A], unlimited),
    do_test_format([fake_report, A], 20),

    do_test_format([fake_report, foo], unlimited),
    do_test_format([fake_report, foo], 20),
    do_test_format([fake_report, []], unlimited),
    do_test_format([fake_report, []], 20).

do_test_format(Report, Depth) ->
    do_test_format(Report, latin1, Depth),
    do_test_format(Report, unicode, Depth).

do_test_format(Report, Encoding, Depth) ->
    io:format("*** Depth = ~p, Encoding = ~p", [Depth, Encoding]),
    S0 = proc_lib:format(Report, Encoding, Depth),
    S = lists:flatten(S0),
    case Encoding of
        latin1 -> io:format("~s\n", [S]);
        _ -> io:format("~ts\n", [S])
    end,
    length(S).

'\x{aaa}t_format_looper'() ->
    receive
	{die,Data} ->
	    exit(Data);
	M ->
            put(M, M),
	    '\x{aaa}t_format_looper'()
    end.

%%-----------------------------------------------------------------
%% The error_logger handler used.
%%-----------------------------------------------------------------
init(Tester) ->
    {ok, Tester}.

handle_event({error_report, _GL, {Pid, crash_report, Report}}, Tester) ->
    io:format("~ts\n", [proc_lib:format(Report)]),
    Tester ! {crash_report, Pid, Report},
    {ok, Tester};
handle_event(_Event, State) ->
    {ok, State}.

handle_info(_, State) ->
    {ok, State}.

handle_call(_Query, State) -> {ok, {error, bad_query}, State}.

terminate(_Reason, State) ->
    State.
