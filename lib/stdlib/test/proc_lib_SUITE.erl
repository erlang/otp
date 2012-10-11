%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2012. All Rights Reserved.
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
-module(proc_lib_SUITE).

%%
%% Define to run outside of test server
%%
%%-define(STANDALONE,1).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, 
	 crash/1, sync_start_nolink/1, sync_start_link/1,
         spawn_opt/1, sp1/0, sp2/0, sp3/1, sp4/2, sp5/1,
	 hibernate/1]).
-export([ otp_6345/1, init_dont_hang/1]).

-export([hib_loop/1, awaken/1]).

-export([init/1,
	 handle_event/2, handle_call/2, handle_info/2,
	 terminate/2]).

-export([otp_6345_init/1, init_dont_hang_init/1]).


-ifdef(STANDALONE).
-define(line, noop, ).
-else.
-include_lib("test_server/include/test_server.hrl").
-endif.

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [crash, {group, sync_start}, spawn_opt, hibernate,
     {group, tickets}].

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
    error_logger:add_report_handler(?MODULE, self()),

    Pid = proc_lib:spawn(?MODULE, sp1, []),
    Pid ! die,
    ?line Report = receive
		       {crash_report, Pid, Report0} -> Report0
		   after 2000 -> test_server:fail(no_crash_report)
		   end,
    ?line proc_lib:format(Report),
    ?line [PidRep, []] = Report,
    ?line {value, {initial_call,{?MODULE,sp1,[]}}} =
	lists:keysearch(initial_call, 1, PidRep),
    Self = self(),
    ?line {value, {ancestors,[Self]}} =
	lists:keysearch(ancestors, 1, PidRep),
    ?line {value, {error_info,{exit,die,_StackTrace1}}} =
	lists:keysearch(error_info, 1, PidRep),

    F = fun sp1/0,
    Pid1 = proc_lib:spawn(node(), F),
    Pid1 ! die,
    ?line [PidRep1, []] = receive
		       {crash_report, Pid1, Report1} -> Report1
		   after 2000 -> test_server:fail(no_crash_report)
		   end,
    ?line {value, {initial_call,{Fmod,Fname,[]}}} =
	lists:keysearch(initial_call, 1, PidRep1),
    ?line {module,Fmod} = erlang:fun_info(F, module),
    ?line {name,Fname} = erlang:fun_info(F, name),
    ?line {value, {ancestors,[Self]}} =
	lists:keysearch(ancestors, 1, PidRep1),
    ?line {value, {error_info,{exit,die,_StackTrace2}}} =
	lists:keysearch(error_info, 1, PidRep1),

    Pid2 = proc_lib:spawn(?MODULE, sp2, []),
    test_server:sleep(100),
    ?line {?MODULE,sp2,[]} = proc_lib:initial_call(Pid2),
    ?line {?MODULE,sp2,0} = proc_lib:translate_initial_call(Pid2),
    Pid2 ! die,
    ?line [Pid2Rep, [{neighbour, LinkRep}]] =
	receive
	    {crash_report, Pid2, Report2} -> Report2
	after 2000 -> test_server:fail(no_crash_report)
	end,
    ?line {value, {initial_call,{?MODULE,sp2,[]}}} =
	lists:keysearch(initial_call, 1, Pid2Rep),
    ?line {value, {ancestors,[Self]}} =
	lists:keysearch(ancestors, 1, Pid2Rep),
    ?line {value, {error_info,{exit,die,_StackTrace3}}} =
	lists:keysearch(error_info, 1, Pid2Rep),
    ?line {value, {initial_call,{?MODULE,sp1,[]}}} =
	lists:keysearch(initial_call, 1, LinkRep),

    %% Make sure that we don't get a crash report if a process
    %% terminates with reason 'shutdown' or reason {shutdown,Reason}.
    ?line process_flag(trap_exit, true),
    ?line Pid3 = proc_lib:spawn_link(erlang, apply,
				     [fun() -> exit(shutdown) end,[]]),

    ?line Pid4 = proc_lib:spawn_link(erlang, apply,
				     [fun() -> exit({shutdown,{a,b,c}}) end,[]]),

    ?line receive {'EXIT',Pid3,shutdown} -> ok end,
    ?line receive {'EXIT',Pid4,{shutdown,{a,b,c}}} -> ok end,
    ?line process_flag(trap_exit, false),

    receive
	Any ->
	    ?line ?t:fail({unexpected_message,Any})
	after 2000 ->
		ok
	end.


sync_start_nolink(Config) when is_list(Config) ->
    _Pid = spawn_link(?MODULE, sp5, [self()]),
    receive
	{sync_started, F} -> 
	    exit(F, kill),
	    test_server:fail(async_start)
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
	    test_server:fail(no_sync_start)
    end,
    ok.
    
sync_start_link(Config) when is_list(Config) ->
    _Pid = spawn_link(?MODULE, sp3, [self()]),
    receive
	{sync_started, _} -> test_server:fail(async_start)
    after 1000 -> ok
    end,
    receive
	{Pid2, init} ->
	    Pid2 ! go_on
    end,
    receive
	{sync_started, _} -> ok
    after 1000 -> test_server:fail(no_sync_start)
    end,
    ok.
    
spawn_opt(Config) when is_list(Config) ->
    F = fun sp1/0,
    {name,Fname} = erlang:fun_info(F, name),
    FunMFArgs = {?MODULE,Fname,[]},
    FunMFArity = {?MODULE,Fname,0},
    ?line Pid1 = proc_lib:spawn_opt(node(), F, [{priority,low}]),
    ?line Pid = proc_lib:spawn_opt(F, [{priority,low}]),
    ?line test_server:sleep(100),
    ?line FunMFArgs = proc_lib:initial_call(Pid),
    ?line FunMFArity = proc_lib:translate_initial_call(Pid),
    ?line Pid ! die,
    ?line FunMFArgs = proc_lib:initial_call(Pid1),
    ?line FunMFArity = proc_lib:translate_initial_call(Pid1),
    ?line Pid1 ! die,
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

hibernate(Config) when is_list(Config) ->
    Ref = make_ref(),
    Self = self(),
    LoopData = {Ref,Self},
    ?line Pid = proc_lib:spawn_link(?MODULE, hib_loop, [LoopData]),

    %% Just check that the child process can process and answer messages.
    ?line Pid ! {Self,loop_data},
    receive
	{loop_data,LoopData} -> ok;
	Unexpected0 ->
	    ?line io:format("Unexpected: ~p\n", [Unexpected0]),
	    ?line ?t:fail()
    after 1000 ->
	    ?line io:format("Timeout"),
	    ?line ?t:fail()
    end,

    %% Hibernate the process.
    ?line Pid ! hibernate,
    erlang:yield(),
    io:format("~p\n", [process_info(Pid, heap_size)]),


    %% Send a message to the process...

    ?line Pid ! {Self,loop_data},

    %% ... expect first a wake up message from the process...
    receive
	{awaken,LoopData} -> ok;
	Unexpected1 ->
	    ?line io:format("Unexpected: ~p\n", [Unexpected1]),
	    ?line ?t:fail()
    after 1000 ->
	    ?line io:format("Timeout"),
	    ?line ?t:fail()
    end,

    %% ... followed by the answer to the actual request.
    receive
	{loop_data,LoopData} -> ok;
	Unexpected2 ->
	    ?line io:format("Unexpected: ~p\n", [Unexpected2]),
	    ?line ?t:fail()
    after 1000 ->
	    ?line io:format("Timeout"),
	    ?line ?t:fail()
    end,

    %% Test that errors are handled correctly after wake up from hibernation...

    ?line process_flag(trap_exit, true),
    ?line error_logger:add_report_handler(?MODULE, self()),
    ?line Pid ! crash,

    %% We should receive two messages. Especially in the SMP emulator,
    %% we can't be sure of the message order, so sort the messages before
    %% matching.

    Messages = lists:sort(hib_receive_messages(2)),
    io:format("~p", [Messages]),
    ?line [{'EXIT',Pid,i_crashed},{crash_report,Pid,[Report,[]]}] = Messages,

    %% Check that the initial_call has the expected format.
    ?line {value,{initial_call,{?MODULE,hib_loop,[_]}}} =
	lists:keysearch(initial_call, 1, Report),

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

otp_6345(suite) ->
    [];
otp_6345(doc) ->
    ["'monitor' spawn_opt option"];
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

%% OTP-9803
init_dont_hang(suite) ->
    [];
init_dont_hang(doc) ->
    ["Check that proc_lib:start don't hang if spawned process crashes before proc_lib:init_ack/2"];
init_dont_hang(Config) when is_list(Config) ->
    %% Start should behave as start_link
    process_flag(trap_exit, true),
    StartLinkRes = proc_lib:start_link(?MODULE, init_dont_hang_init, [self()]),
    try
	StartLinkRes = proc_lib:start(?MODULE, init_dont_hang_init, [self()], 1000),
	StartLinkRes = proc_lib:start(?MODULE, init_dont_hang_init, [self()], 1000, []),
	ok
    catch _:Error ->
	    io:format("Error ~p /= ~p ~n",[erlang:get_stacktrace(), StartLinkRes]),
	    exit(Error)
    end.

init_dont_hang_init(Parent) ->
    1 = 2.



%%-----------------------------------------------------------------
%% The error_logger handler used.
%%-----------------------------------------------------------------
init(Tester) ->
    {ok, Tester}.
    
handle_event({error_report, _GL, {Pid, crash_report, Report}}, Tester) ->
    io:format("~s\n", [proc_lib:format(Report)]),
    Tester ! {crash_report, Pid, Report},
    {ok, Tester};
handle_event(_Event, State) ->
    {ok, State}.

handle_info(_, State) ->
    {ok, State}.

handle_call(_Query, State) -> {ok, {error, bad_query}, State}.

terminate(_Reason, State) ->
    State.
