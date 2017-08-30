%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2016. All Rights Reserved.
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
-module(gen_statem_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile(export_all).
-behaviour(gen_statem).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() ->
    [{group, start},
     {group, start_handle_event},
     {group, stop},
     {group, stop_handle_event},
     {group, abnormal},
     {group, abnormal_handle_event},
     shutdown, stop_and_reply, event_order,
     {group, sys},
     hibernate, enter_loop].

groups() ->
    [{start, [],
      [start1, start2, start3, start4, start5, start6, start7,
       start8, start9, start10, start11, start12, next_events]},
     {start_handle_event, [],
      [start1, start2, start3, start4, start5, start6, start7,
       start8, start9, start10, start11, start12, next_events]},
     {stop, [],
      [stop1, stop2, stop3, stop4, stop5, stop6, stop7, stop8, stop9, stop10]},
     {stop_handle_event, [],
      [stop1, stop2, stop3, stop4, stop5, stop6, stop7, stop8, stop9, stop10]},
     {abnormal, [], [abnormal1, abnormal2]},
     {abnormal_handle_event, [], [abnormal1, abnormal2]},
     {sys, [],
      [sys1, code_change,
       call_format_status,
       error_format_status, terminate_crash_format,
       get_state, replace_state]},
     {sys_handle_event, [],
      [sys1,
       call_format_status,
       error_format_status, terminate_crash_format,
       get_state, replace_state]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(GroupName, Config)
  when GroupName =:= start_handle_event;
       GroupName =:= stop_handle_event;
       GroupName =:= abnormal_handle_event;
       GroupName =:= sys_handle_event ->
    [{callback_mode,handle_event_function}|Config];
init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(_CaseName, Config) ->
    flush(),
%%%    dbg:tracer(),
%%%    dbg:p(all, c),
%%%    dbg:tpl(gen_statem, cx),
%%%    dbg:tpl(proc_lib, cx),
%%%    dbg:tpl(gen, cx),
%%%    dbg:tpl(sys, cx),
    Config.

end_per_testcase(_CaseName, Config) ->
%%%    dbg:stop(),
    Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(EXPECT_FAILURE(Code, Reason),
	try begin Code end of
	    Reason ->
		ct:fail({unexpected,Reason})
	catch
	    error:Reason -> Reason;
	    exit:Reason -> Reason
	end).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% anonymous
start1(Config) ->
    %%OldFl = process_flag(trap_exit, true),

    {ok,Pid0} = gen_statem:start_link(?MODULE, start_arg(Config, []), []),
    ok = do_func_test(Pid0),
    ok = do_sync_func_test(Pid0),
    stop_it(Pid0),
%%    stopped = gen_statem:call(Pid0, stop),
%%    timeout =
%%	?EXPECT_FAILURE(gen_statem:call(Pid0, hej), Reason),

    %%process_flag(trap_exit, OldFl),
    ok = verify_empty_msgq().

%% anonymous w. shutdown
start2(Config) ->
    %% Dont link when shutdown
    {ok,Pid0} = gen_statem:start(?MODULE, start_arg(Config, []), []),
    ok = do_func_test(Pid0),
    ok = do_sync_func_test(Pid0),
    stopped = gen_statem:call(Pid0, {stop,shutdown}),
    check_stopped(Pid0),
    ok = verify_empty_msgq().

%% anonymous with timeout
start3(Config) ->
    %%OldFl = process_flag(trap_exit, true),

    {ok,Pid0} =
	gen_statem:start(?MODULE, start_arg(Config, []), [{timeout,5}]),
    ok = do_func_test(Pid0),
    ok = do_sync_func_test(Pid0),
    stop_it(Pid0),

    {error,timeout} =
	gen_statem:start(
	  ?MODULE, start_arg(Config, sleep), [{timeout,5}]),

    %%process_flag(trap_exit, OldFl),
    ok = verify_empty_msgq().

%% anonymous with ignore
start4(Config) ->
    OldFl = process_flag(trap_exit, true),

    ignore = gen_statem:start(?MODULE, start_arg(Config, ignore), []),

    process_flag(trap_exit, OldFl),
    ok = verify_empty_msgq().

%% anonymous with stop
start5(Config) ->
    OldFl = process_flag(trap_exit, true),

    {error,stopped} = gen_statem:start(?MODULE, start_arg(Config, stop), []),

    process_flag(trap_exit, OldFl),
    ok = verify_empty_msgq().

%% anonymous linked
start6(Config) ->
    {ok,Pid} = gen_statem:start_link(?MODULE, start_arg(Config, []), []),
    ok = do_func_test(Pid),
    ok = do_sync_func_test(Pid),
    stop_it(Pid),

    ok = verify_empty_msgq().

%% global register linked
start7(Config) ->
    STM = {global,my_stm},

    {ok,Pid} =
	gen_statem:start_link(STM, ?MODULE, start_arg(Config, []), []),
    {error,{already_started,Pid}} =
	gen_statem:start_link(STM, ?MODULE, start_arg(Config, []), []),
    {error,{already_started,Pid}} =
	gen_statem:start(STM, ?MODULE, start_arg(Config, []), []),

    ok = do_func_test(Pid),
    ok = do_sync_func_test(Pid),
    ok = do_func_test(STM),
    ok = do_sync_func_test(STM),
    stop_it(STM),

    ok = verify_empty_msgq().


%% local register
start8(Config) ->
    %%OldFl = process_flag(trap_exit, true),
    Name = my_stm,
    STM = {local,Name},

    {ok,Pid} =
	gen_statem:start(STM, ?MODULE, start_arg(Config, []), []),
    {error,{already_started,Pid}} =
	gen_statem:start(STM, ?MODULE, start_arg(Config, []), []),

    ok = do_func_test(Pid),
    ok = do_sync_func_test(Pid),
    ok = do_func_test(Name),
    ok = do_sync_func_test(Name),
    stop_it(Pid),

    %%process_flag(trap_exit, OldFl),
    ok = verify_empty_msgq().

%% local register linked
start9(Config) ->
    %%OldFl = process_flag(trap_exit, true),
    Name = my_stm,
    STM = {local,Name},

    {ok,Pid} =
	gen_statem:start_link(STM, ?MODULE, start_arg(Config, []), []),
    {error,{already_started,Pid}} =
	gen_statem:start(STM, ?MODULE, start_arg(Config, []), []),

    ok = do_func_test(Pid),
    ok = do_sync_func_test(Pid),
    ok = do_func_test(Name),
    ok = do_sync_func_test(Name),
    stop_it(Pid),

    %%process_flag(trap_exit, OldFl),
    ok = verify_empty_msgq().

%% global register
start10(Config) ->
    STM = {global,my_stm},

    {ok,Pid} =
	gen_statem:start(STM, ?MODULE, start_arg(Config, []), []),
    {error,{already_started,Pid}} =
	gen_statem:start(STM, ?MODULE, start_arg(Config, []), []),
    {error,{already_started,Pid}} =
	gen_statem:start_link(STM, ?MODULE, start_arg(Config, []), []),

    ok = do_func_test(Pid),
    ok = do_sync_func_test(Pid),
    ok = do_func_test(STM),
    ok = do_sync_func_test(STM),
    stop_it(STM),

    ok = verify_empty_msgq().

%% Stop registered processes
start11(Config) ->
    Name = my_stm,
    LocalSTM = {local,Name},
    GlobalSTM = {global,Name},

    {ok,Pid} =
	gen_statem:start_link(LocalSTM, ?MODULE, start_arg(Config, []), []),
    stop_it(Pid),

    {ok,_Pid1} =
	gen_statem:start_link(LocalSTM, ?MODULE, start_arg(Config, []), []),
    stop_it(Name),

    {ok,Pid2} =
	gen_statem:start(GlobalSTM, ?MODULE, start_arg(Config, []), []),
    stop_it(Pid2),
    receive after 1 -> true end,
    Result =
	gen_statem:start(GlobalSTM, ?MODULE, start_arg(Config, []), []),
    ct:log("Result = ~p~n",[Result]),
    {ok,_Pid3} = Result,
    stop_it(GlobalSTM),

    ok = verify_empty_msgq().

%% Via register linked
start12(Config) ->
    dummy_via:reset(),
    VIA = {via,dummy_via,my_stm},

    {ok,Pid} =
	gen_statem:start_link(VIA, ?MODULE, start_arg(Config, []), []),
    {error,{already_started,Pid}} =
	gen_statem:start_link(VIA, ?MODULE, start_arg(Config, []), []),
    {error,{already_started,Pid}} =
	gen_statem:start(VIA, ?MODULE, start_arg(Config, []), []),

    ok = do_func_test(Pid),
    ok = do_sync_func_test(Pid),
    ok = do_func_test(VIA),
    ok = do_sync_func_test(VIA),
    stop_it(VIA),

    ok = verify_empty_msgq().


%% Anonymous, reason 'normal'
stop1(Config) ->
    {ok,Pid} = gen_statem:start(?MODULE, start_arg(Config, []), []),
    ok = gen_statem:stop(Pid),
    false = erlang:is_process_alive(Pid),
    noproc =
	?EXPECT_FAILURE(gen_statem:stop(Pid), Reason).

%% Anonymous, other reason
stop2(Config) ->
    {ok,Pid} = gen_statem:start(?MODULE, start_arg(Config, []), []),
    ok = gen_statem:stop(Pid, other_reason, infinity),
    false = erlang:is_process_alive(Pid),
    ok.

%% Anonymous, invalid timeout
stop3(Config) ->
    {ok,Pid} = gen_statem:start(?MODULE, start_arg(Config, []), []),
    _ =
	?EXPECT_FAILURE(
	   gen_statem:stop(Pid, other_reason, invalid_timeout),
	   Reason),
    true = erlang:is_process_alive(Pid),
    ok = gen_statem:stop(Pid),
    false = erlang:is_process_alive(Pid),
    ok.

%% Registered name
stop4(Config) ->
    {ok,Pid} =
	gen_statem:start(
	  {local,to_stop},?MODULE, start_arg(Config, []), []),
    ok = gen_statem:stop(to_stop),
    false = erlang:is_process_alive(Pid),
    noproc =
	?EXPECT_FAILURE(gen_statem:stop(to_stop), Reason),
    ok.

%% Registered name and local node
stop5(Config) ->
    Name = to_stop,
    {ok,Pid} =
	gen_statem:start(
	  {local,Name},?MODULE, start_arg(Config, []), []),
    ok = gen_statem:stop({Name,node()}),
    false = erlang:is_process_alive(Pid),
    noproc =
	?EXPECT_FAILURE(gen_statem:stop({Name,node()}), Reason),
    ok.

%% Globally registered name
stop6(Config) ->
    STM = {global,to_stop},
    {ok,Pid} = gen_statem:start(STM, ?MODULE, start_arg(Config, []), []),
    ok = gen_statem:stop(STM),
    false = erlang:is_process_alive(Pid),
    noproc =
	?EXPECT_FAILURE(gen_statem:stop(STM), Reason),
    ok.

%% 'via' registered name
stop7(Config) ->
    VIA = {via,dummy_via,to_stop},
    dummy_via:reset(),
    {ok,Pid} = gen_statem:start(VIA, ?MODULE, start_arg(Config, []), []),
    ok = gen_statem:stop(VIA),
    false = erlang:is_process_alive(Pid),
    noproc =
	?EXPECT_FAILURE(gen_statem:stop(VIA), Reason),
    ok.

%% Anonymous on remote node
stop8(Config) ->
    Node = gen_statem_stop8,
    {ok,NodeName} = ct_slave:start(Node),
    Dir = filename:dirname(code:which(?MODULE)),
    rpc:call(NodeName, code, add_path, [Dir]),
    {ok,Pid} =
	rpc:call(
	  NodeName, gen_statem,start,
	  [?MODULE,start_arg(Config, []),[]]),
    ok = gen_statem:stop(Pid),
    false = rpc:call(NodeName, erlang, is_process_alive, [Pid]),
    noproc =
	?EXPECT_FAILURE(gen_statem:stop(Pid), Reason1),
    {ok,NodeName} = ct_slave:stop(Node),
    {{nodedown,NodeName},{sys,terminate,_}} =
	?EXPECT_FAILURE(gen_statem:stop(Pid), Reason2),
    ok.

%% Registered name on remote node
stop9(Config) ->
    Name = to_stop,
    LocalSTM = {local,Name},
    Node = gen_statem__stop9,
    {ok,NodeName} = ct_slave:start(Node),
    STM = {Name,NodeName},
    Dir = filename:dirname(code:which(?MODULE)),
    rpc:call(NodeName, code, add_path, [Dir]),
    {ok,Pid} =
	rpc:call(
	  NodeName, gen_statem, start,
	  [LocalSTM,?MODULE,start_arg(Config, []),[]]),
    ok = gen_statem:stop(STM),
    undefined = rpc:call(NodeName,erlang,whereis,[Name]),
    false = rpc:call(NodeName,erlang,is_process_alive,[Pid]),
    noproc =
	?EXPECT_FAILURE(gen_statem:stop(STM), Reason1),
    {ok,NodeName} = ct_slave:stop(Node),
    {{nodedown,NodeName},{sys,terminate,_}} =
	?EXPECT_FAILURE(gen_statem:stop(STM), Reason2),
    ok.

%% Globally registered name on remote node
stop10(Config) ->
    Node = gen_statem_stop10,
    STM = {global,to_stop},
    {ok,NodeName} = ct_slave:start(Node),
    Dir = filename:dirname(code:which(?MODULE)),
    rpc:call(NodeName,code,add_path,[Dir]),
    {ok,Pid} =
	rpc:call(
	  NodeName, gen_statem, start,
	  [STM,?MODULE,start_arg(Config, []),[]]),
    global:sync(),
    ok = gen_statem:stop(STM),
    false = rpc:call(NodeName, erlang, is_process_alive, [Pid]),
    noproc =
	?EXPECT_FAILURE(gen_statem:stop(STM), Reason1),
    {ok,NodeName} = ct_slave:stop(Node),
    noproc =
	?EXPECT_FAILURE(gen_statem:stop(STM), Reason2),
    ok.

%% Check that time outs in calls work
abnormal1(Config) ->
    Name = abnormal1,
    LocalSTM = {local,Name},

    {ok, _Pid} =
	gen_statem:start(LocalSTM, ?MODULE, start_arg(Config, []), []),

    %% timeout call.
    delayed = gen_statem:call(Name, {delayed_answer,1}, 100),
    {timeout,_} =
	?EXPECT_FAILURE(
	   gen_statem:call(Name, {delayed_answer,1000}, 10),
	   Reason),
    ok = gen_statem:stop(Name),
    ok = verify_empty_msgq().

%% Check that bad return values makes the stm crash. Note that we must
%% trap exit since we must link to get the real bad_return_ error
abnormal2(Config) ->
    OldFl = process_flag(trap_exit, true),
    {ok,Pid} = gen_statem:start_link(?MODULE, start_arg(Config, []), []),

    %% bad return value in the gen_statem loop
    {{bad_return_value,badreturn},_} =
	?EXPECT_FAILURE(gen_statem:call(Pid, badreturn), Reason),
    receive
	{'EXIT',Pid,{bad_return_value,badreturn}} -> ok
    after 5000 ->
	    ct:fail(gen_statem_did_not_die)
    end,

    process_flag(trap_exit, OldFl),
    ok = verify_empty_msgq().

shutdown(Config) ->
    process_flag(trap_exit, true),

    {ok,Pid0} = gen_statem:start_link(?MODULE, start_arg(Config, []), []),
    ok = do_func_test(Pid0),
    ok = do_sync_func_test(Pid0),
    stopped = gen_statem:call(Pid0, {stop,{shutdown,reason}}),
    receive {'EXIT',Pid0,{shutdown,reason}} -> ok end,
    process_flag(trap_exit, false),

    {noproc,_} =
	?EXPECT_FAILURE(gen_statem:call(Pid0, hej), Reason),

    receive
	Any ->
	    ct:log("Unexpected: ~p", [Any]),
	    ct:fail({unexpected,Any})
    after 500 ->
	    ok
    end.



stop_and_reply(_Config) ->
    process_flag(trap_exit, true),

    Machine =
	%% Abusing the internal format of From...
	#{init =>
	      fun () ->
		      {ok,start,undefined}
	      end,
	  start =>
	      fun (cast, {echo,From1,Reply1}, undefined) ->
		      {next_state,wait,{reply,From1,Reply1}}
	      end,
	  wait =>
	      fun (cast, {stop_and_reply,Reason,From2,Reply2},R1) ->
		      {stop_and_reply,Reason,
		       [R1,{reply,From2,Reply2}]}
	      end},
    {ok,STM} = gen_statem:start_link(?MODULE, {map_statem,Machine}, []),

    Self = self(),
    Tag1 = make_ref(),
    gen_statem:cast(STM, {echo,{Self,Tag1},reply1}),
    Tag2 = make_ref(),
    gen_statem:cast(STM, {stop_and_reply,reason,{Self,Tag2},reply2}),
    case flush() of
	[{Tag1,reply1},{Tag2,reply2},{'EXIT',STM,reason}] ->
	    ok;
	Other1 ->
	    ct:fail({unexpected,Other1})
    end,

    {noproc,_} =
	?EXPECT_FAILURE(gen_statem:call(STM, hej), Reason),
    case flush() of
	[] ->
	    ok;
	Other2 ->
	    ct:fail({unexpected,Other2})
    end.



event_order(_Config) ->
    process_flag(trap_exit, true),

    Machine =
	%% Abusing the internal format of From...
	#{init =>
	      fun () ->
		      {ok,start,undefined}
	      end,
	  start =>
	      fun (cast, _, _) ->
		      {keep_state_and_data,postpone}; %% Handled in 'buffer'
		  ({call,From}, {buffer,Pid,[Tag3,Tag4,Tag5]},
		   undefined) ->
		      {next_state,buffer,[],
		       [{next_event,internal,{reply,{Pid,Tag3},ok3}},
			{next_event,internal,{reply,{Pid,Tag4},ok4}},
			{timeout,0,{reply,{Pid,Tag5},ok5}},
			%% The timeout should not happen since there
			%% are events that cancel it i.e next_event
			%% and postponed
			{reply,From,ok}]}
	      end,
	  buffer =>
	      fun (internal, Reply, Replies) ->
		      {keep_state,[Reply|Replies]};
		  (timeout, Reply, Replies) ->
		      {keep_state,[Reply|Replies]};
		  (cast, Reply, Replies) ->
		      {keep_state,[Reply|Replies]};
		  ({call,From}, {stop,Reason}, Replies) ->
		      {next_state,stop,undefined,
		       lists:reverse(
			 Replies,
			 [{reply,From,ok},
			  {next_event,internal,{stop,Reason}}])}
	      end,
	  stop =>
	      fun (internal, Result, undefined) ->
		      Result
	      end},

    {ok,STM} = gen_statem:start_link(?MODULE, {map_statem,Machine}, []),
    Self = self(),
    Tag1 = make_ref(),
    gen_statem:cast(STM, {reply,{Self,Tag1},ok1}),
    Tag2 = make_ref(),
    gen_statem:cast(STM, {reply,{Self,Tag2},ok2}),
    Tag3 = make_ref(),
    Tag4 = make_ref(),
    Tag5 = make_ref(),
    ok = gen_statem:call(STM, {buffer,Self,[Tag3,Tag4,Tag5]}),
    ok = gen_statem:call(STM, {stop,reason}),
    case flush() of
	[{Tag3,ok3},{Tag4,ok4},{Tag1,ok1},{Tag2,ok2},
	 {'EXIT',STM,reason}] ->
	    ok;
	Other1 ->
	    ct:fail({unexpected,Other1})
    end,

    {noproc,_} =
	?EXPECT_FAILURE(gen_statem:call(STM, hej), Reason),
    case flush() of
	[] ->
	    ok;
	Other2 ->
	    ct:fail({unexpected,Other2})
    end.



sys1(Config) ->
    {ok,Pid} = gen_statem:start(?MODULE, start_arg(Config, []), []),
    {status, Pid, {module,gen_statem}, _} = sys:get_status(Pid),
    sys:suspend(Pid),
    Parent = self(),
    Tag = make_ref(),
    Caller =
	spawn(
	  fun () ->
		  Parent ! {Tag,gen_statem:call(Pid, hej)}
	  end),
    receive
	{Tag,_} ->
	    ct:fail(should_be_suspended)
    after 3000 ->
	    exit(Caller, ok)
    end,

    %% {timeout,_} =
    %% 	?EXPECT_FAILURE(gen_statem:call(Pid, hej), Reason),
    sys:resume(Pid),
    stop_it(Pid).

code_change(Config) ->
    Mode = handle_event_function,
    {ok,Pid} = gen_statem:start(?MODULE, start_arg(Config, []), []),
    {idle,data} = sys:get_state(Pid),
    sys:suspend(Pid),
    sys:change_code(Pid, ?MODULE, old_vsn, Mode),
    sys:resume(Pid),
    {idle,{old_vsn,data,Mode}} = sys:get_state(Pid),
    Mode = gen_statem:call(Pid, get_callback_mode),
    stop_it(Pid).

call_format_status(Config) ->
    {ok,Pid} = gen_statem:start(?MODULE, start_arg(Config, []), []),
    Status = sys:get_status(Pid),
    {status,Pid,_Mod,[_PDict,running,_,_, Data]} = Status,
    [format_status_called|_] = lists:reverse(Data),
    stop_it(Pid),

    %% check that format_status can handle a name being an atom (pid is
    %% already checked by the previous test)
    {ok, Pid2} =
	gen_statem:start(
	  {local, gstm}, ?MODULE, start_arg(Config, []), []),
    Status2 = sys:get_status(gstm),
    {status,Pid2,_Mod,[_PDict2,running,_,_,Data2]} = Status2,
    [format_status_called|_] = lists:reverse(Data2),
    stop_it(Pid2),

    %% check that format_status can handle a name being a term other than a
    %% pid or atom
    GlobalName1 = {global,"CallFormatStatus"},
    {ok,Pid3} =
	gen_statem:start(
	  GlobalName1, ?MODULE, start_arg(Config, []), []),
    Status3 = sys:get_status(GlobalName1),
    {status,Pid3,_Mod,[_PDict3,running,_,_,Data3]} = Status3,
    [format_status_called|_] = lists:reverse(Data3),
    stop_it(Pid3),
    GlobalName2 = {global,{name, "term"}},
    {ok,Pid4} =
	gen_statem:start(
	  GlobalName2, ?MODULE, start_arg(Config, []), []),
    Status4 = sys:get_status(GlobalName2),
    {status,Pid4,_Mod,[_PDict4,running,_,_, Data4]} = Status4,
    [format_status_called|_] = lists:reverse(Data4),
    stop_it(Pid4),

    %% check that format_status can handle a name being a term other than a
    %% pid or atom
    dummy_via:reset(),
    ViaName1 = {via,dummy_via,"CallFormatStatus"},
    {ok,Pid5} = gen_statem:start(ViaName1, ?MODULE, start_arg(Config, []), []),
    Status5 = sys:get_status(ViaName1),
    {status,Pid5,_Mod, [_PDict5,running,_,_, Data5]} = Status5,
    [format_status_called|_] = lists:reverse(Data5),
    stop_it(Pid5),
    ViaName2 = {via,dummy_via,{name,"term"}},
    {ok, Pid6} =
	gen_statem:start(
	  ViaName2, ?MODULE, start_arg(Config, []), []),
    Status6 = sys:get_status(ViaName2),
    {status,Pid6,_Mod,[_PDict6,running,_,_,Data6]} = Status6,
    [format_status_called|_] = lists:reverse(Data6),
    stop_it(Pid6).



error_format_status(Config) ->
    error_logger_forwarder:register(),
    OldFl = process_flag(trap_exit, true),
    Data = "called format_status",
    {ok,Pid} =
	gen_statem:start(
	  ?MODULE, start_arg(Config, {data,Data}), []),
    %% bad return value in the gen_statem loop
    {{bad_return_value,badreturn},_} =
	?EXPECT_FAILURE(gen_statem:call(Pid, badreturn), Reason),
    receive
	{error,_,
	 {Pid,
	  "** State machine"++_,
	  [Pid,{{call,_},badreturn},
	   {formatted,idle,Data},
	   error,{bad_return_value,badreturn}|_]}} ->
	    ok;
	Other when is_tuple(Other), element(1, Other) =:= error ->
	    error_logger_forwarder:unregister(),
	    ct:fail({unexpected,Other})
    after 1000 ->
	    error_logger_forwarder:unregister(),
	    ct:fail(timeout)
    end,
    process_flag(trap_exit, OldFl),
    error_logger_forwarder:unregister(),
    receive
	%% Comes with SASL
	{error_report,_,{Pid,crash_report,_}} ->
	    ok
    after 500 ->
	    ok
    end,
    ok = verify_empty_msgq().

terminate_crash_format(Config) ->
    error_logger_forwarder:register(),
    OldFl = process_flag(trap_exit, true),
    Data = crash_terminate,
    {ok,Pid} =
	gen_statem:start(
	  ?MODULE, start_arg(Config, {data,Data}), []),
    stop_it(Pid),
    Self = self(),
    receive
	{error,_GroupLeader,
	 {Pid,
	  "** State machine"++_,
	  [Pid,
	   {{call,{Self,_}},stop},
	   {formatted,idle,Data},
	   exit,{crash,terminate}|_]}} ->
	    ok;
	Other when is_tuple(Other), element(1, Other) =:= error ->
	    error_logger_forwarder:unregister(),
	    ct:fail({unexpected,Other})
    after 1000 ->
	    error_logger_forwarder:unregister(),
	    ct:fail(timeout)
    end,
    process_flag(trap_exit, OldFl),
    error_logger_forwarder:unregister(),
    receive
	%% Comes with SASL
	{error_report,_,{Pid,crash_report,_}} ->
	    ok
    after 500 ->
	    ok
    end,
    ok = verify_empty_msgq().


get_state(Config) ->
    State = self(),
    {ok,Pid} =
	gen_statem:start(
	  ?MODULE, start_arg(Config, {data,State}), []),
    {idle,State} = sys:get_state(Pid),
    {idle,State} = sys:get_state(Pid, 5000),
    stop_it(Pid),

    %% check that get_state can handle a name being an atom (pid is
    %% already checked by the previous test)
    {ok,Pid2} =
	gen_statem:start(
	  {local,gstm}, ?MODULE, start_arg(Config, {data,State}), []),
    {idle,State} = sys:get_state(gstm),
    {idle,State} = sys:get_state(gstm, 5000),
    stop_it(Pid2),

    %% check that get_state works when pid is sys suspended
    {ok,Pid3} =
	gen_statem:start(
	  ?MODULE, start_arg(Config, {data,State}), []),
    {idle,State} = sys:get_state(Pid3),
    ok = sys:suspend(Pid3),
    {idle,State} = sys:get_state(Pid3, 5000),
    ok = sys:resume(Pid3),
    stop_it(Pid3),
    ok = verify_empty_msgq().

replace_state(Config) ->
    State = self(),
    {ok, Pid} =
	gen_statem:start(
	  ?MODULE, start_arg(Config, {data,State}), []),
    {idle,State} = sys:get_state(Pid),
    NState1 = "replaced",
    Replace1 = fun({StateName, _}) -> {StateName,NState1} end,
    {idle,NState1} = sys:replace_state(Pid, Replace1),
    {idle,NState1} = sys:get_state(Pid),
    NState2 = "replaced again",
    Replace2 = fun({idle, _}) -> {state0,NState2} end,
    {state0,NState2} = sys:replace_state(Pid, Replace2, 5000),
    {state0,NState2} = sys:get_state(Pid),
    %% verify no change in state if replace function crashes
    Replace3 = fun(_) -> error(fail) end,
    {callback_failed,
     {gen_statem,system_replace_state},{error,fail}} =
	?EXPECT_FAILURE(sys:replace_state(Pid, Replace3), Reason),
    {state0, NState2} = sys:get_state(Pid),
    %% verify state replaced if process sys suspended
    ok = sys:suspend(Pid),
    Suffix2 = " and again",
    NState3 = NState2 ++ Suffix2,
    Replace4 = fun({StateName, _}) -> {StateName, NState3} end,
    {state0,NState3} = sys:replace_state(Pid, Replace4),
    ok = sys:resume(Pid),
    {state0,NState3} = sys:get_state(Pid, 5000),
    stop_it(Pid),
    ok = verify_empty_msgq().

%% Hibernation
hibernate(Config) ->
    OldFl = process_flag(trap_exit, true),

    {ok,Pid0} =
	gen_statem:start_link(
	  ?MODULE, start_arg(Config, hiber_now), []),
    is_in_erlang_hibernate(Pid0),
    stop_it(Pid0),
    receive
	{'EXIT',Pid0,normal} -> ok
    after 5000 ->
	    ct:fail(gen_statem_did_not_die)
    end,

    {ok,Pid} =
	gen_statem:start_link(?MODULE, start_arg(Config, hiber), []),
    true = ({current_function,{erlang,hibernate,3}} =/=
		erlang:process_info(Pid,current_function)),
    hibernating = gen_statem:call(Pid, hibernate_sync),
    is_in_erlang_hibernate(Pid),
    good_morning = gen_statem:call(Pid, wakeup_sync),
    is_not_in_erlang_hibernate(Pid),
    hibernating = gen_statem:call(Pid, hibernate_sync),
    is_in_erlang_hibernate(Pid),
    please_just_five_more = gen_statem:call(Pid, snooze_sync),
    is_in_erlang_hibernate(Pid),
    good_morning = gen_statem:call(Pid, wakeup_sync),
    is_not_in_erlang_hibernate(Pid),
    ok = gen_statem:cast(Pid, hibernate_async),
    is_in_erlang_hibernate(Pid),
    ok = gen_statem:cast(Pid, wakeup_async),
    is_not_in_erlang_hibernate(Pid),
    ok = gen_statem:cast(Pid, hibernate_async),
    is_in_erlang_hibernate(Pid),
    ok = gen_statem:cast(Pid, snooze_async),
    is_in_erlang_hibernate(Pid),
    ok = gen_statem:cast(Pid, wakeup_async),
    is_not_in_erlang_hibernate(Pid),

    Pid ! hibernate_later,
    true =
	({current_function,{erlang,hibernate,3}} =/=
	     erlang:process_info(Pid, current_function)),
    is_in_erlang_hibernate(Pid),

    'alive!' = gen_statem:call(Pid, 'alive?'),
    true =
	({current_function,{erlang,hibernate,3}} =/=
	     erlang:process_info(Pid, current_function)),
    Pid ! hibernate_now,
    is_in_erlang_hibernate(Pid),

    'alive!' = gen_statem:call(Pid, 'alive?'),
    true =
	({current_function,{erlang,hibernate,3}} =/=
	     erlang:process_info(Pid, current_function)),

    hibernating = gen_statem:call(Pid, hibernate_sync),
    is_in_erlang_hibernate(Pid),
    good_morning = gen_statem:call(Pid, wakeup_sync),
    is_not_in_erlang_hibernate(Pid),
    hibernating = gen_statem:call(Pid, hibernate_sync),
    is_in_erlang_hibernate(Pid),
    please_just_five_more = gen_statem:call(Pid, snooze_sync),
    is_in_erlang_hibernate(Pid),
    good_morning = gen_statem:call(Pid, wakeup_sync),
    is_not_in_erlang_hibernate(Pid),
    ok = gen_statem:cast(Pid, hibernate_async),
    is_in_erlang_hibernate(Pid),
    ok  = gen_statem:cast(Pid, wakeup_async),
    is_not_in_erlang_hibernate(Pid),
    ok = gen_statem:cast(Pid, hibernate_async),
    is_in_erlang_hibernate(Pid),
    ok = gen_statem:cast(Pid, snooze_async),
    is_in_erlang_hibernate(Pid),
    ok = gen_statem:cast(Pid, wakeup_async),
    is_not_in_erlang_hibernate(Pid),

    hibernating = gen_statem:call(Pid, hibernate_sync),
    is_in_erlang_hibernate(Pid),
    sys:suspend(Pid),
    is_in_erlang_hibernate(Pid),
    sys:resume(Pid),
    is_in_erlang_hibernate(Pid),
    receive after 1000 -> ok end,
    is_in_erlang_hibernate(Pid),

    good_morning  = gen_statem:call(Pid, wakeup_sync),
    is_not_in_erlang_hibernate(Pid),
    stop_it(Pid),
    process_flag(trap_exit, OldFl),
    receive
	{'EXIT',Pid,normal} -> ok
    after 5000 ->
	    ct:fail(gen_statem_did_not_die)
    end,
    ok = verify_empty_msgq().

is_in_erlang_hibernate(Pid) ->
    receive after 1 -> ok end,
    is_in_erlang_hibernate_1(200, Pid).

is_in_erlang_hibernate_1(0, Pid) ->
    ct:log("~p\n", [erlang:process_info(Pid, current_function)]),
    ct:fail(not_in_erlang_hibernate_3);
is_in_erlang_hibernate_1(N, Pid) ->
    {current_function,MFA} = erlang:process_info(Pid, current_function),
    case MFA of
	{erlang,hibernate,3} ->
	    ok;
	_ ->
	    receive after 10 -> ok end,
	    is_in_erlang_hibernate_1(N-1, Pid)
    end.

is_not_in_erlang_hibernate(Pid) ->
    receive after 1 -> ok end,
    is_not_in_erlang_hibernate_1(200, Pid).

is_not_in_erlang_hibernate_1(0, Pid) ->
    ct:log("~p\n", [erlang:process_info(Pid, current_function)]),
    ct:fail(not_in_erlang_hibernate_3);
is_not_in_erlang_hibernate_1(N, Pid) ->
    {current_function,MFA} = erlang:process_info(Pid, current_function),
    case MFA of
	{erlang,hibernate,3} ->
	    receive after 10 -> ok end,
	    is_not_in_erlang_hibernate_1(N-1, Pid);
	_ ->
	    ok
    end.


enter_loop(_Config) ->
    OldFlag = process_flag(trap_exit, true),

    dummy_via:reset(),

    %% Locally registered process + {local,Name}
    {ok,Pid1a} =
	proc_lib:start_link(?MODULE, enter_loop, [local,local]),
    yes = gen_statem:call(Pid1a, 'alive?'),
    stopped = gen_statem:call(Pid1a, stop),
    receive
	{'EXIT',Pid1a,normal} ->
	    ok
    after 5000 ->
	    ct:fail(gen_statem_did_not_die)
    end,

    %% Unregistered process + {local,Name}
    {ok,Pid1b} =
	proc_lib:start_link(?MODULE, enter_loop, [anon,local]),
    receive
	{'EXIT',Pid1b,process_not_registered} ->
	    ok
    after 5000 ->
	    ct:fail(gen_statem_did_not_die)
    end,

    %% Globally registered process + {global,Name}
    {ok,Pid2a} =
	proc_lib:start_link(?MODULE, enter_loop, [global,global]),
    yes = gen_statem:call(Pid2a, 'alive?'),
    stopped = gen_statem:call(Pid2a, stop),
    receive
	{'EXIT',Pid2a,normal} ->
	    ok
    after 5000 ->
	    ct:fail(gen_statem_did_not_die)
    end,

    %% Unregistered process + {global,Name}
    {ok,Pid2b} =
	proc_lib:start_link(?MODULE, enter_loop, [anon,global]),
    receive
	{'EXIT',Pid2b,process_not_registered_globally} ->
	    ok
    after 5000 ->
	    ct:fail(gen_statem_did_not_die)
    end,

    %% Unregistered process + no name
    {ok,Pid3} =
	proc_lib:start_link(?MODULE, enter_loop, [anon,anon]),
    yes = gen_statem:call(Pid3, 'alive?'),
    stopped = gen_statem:call(Pid3, stop),
    receive
	{'EXIT',Pid3,normal} ->
	    ok
    after 5000 ->
	    ct:fail(gen_statem_did_not_die)
    end,

    %% Process not started using proc_lib
    CallbackMode = state_functions,
    Pid4 =
	spawn_link(
	  gen_statem, enter_loop,
	  [?MODULE,[],CallbackMode,state0,[]]),
    receive
	{'EXIT',Pid4,process_was_not_started_by_proc_lib} ->
	    ok
    after 5000 ->
	    ct:fail(gen_statem_did_not_die)
    end,

    %% Make sure I am the parent, ie that ordering a shutdown will
    %% result in the process terminating with Reason==shutdown
    {ok,Pid5} =
	proc_lib:start_link(?MODULE, enter_loop, [anon,anon]),
    yes = gen_statem:call(Pid5, 'alive?'),
    exit(Pid5, shutdown),
    receive
	{'EXIT',Pid5,shutdown} ->
	    ok
    after 5000 ->
	    ct:fail(gen_statem_did_not_die)
    end,

    %% Make sure gen_statem:enter_loop does not accept {local,Name}
    %% when it's another process than the calling one which is
    %% registered under that name
    register(armitage, self()),
    {ok,Pid6a} =
	proc_lib:start_link(?MODULE, enter_loop, [anon,local]),
    receive
	{'EXIT',Pid6a,process_not_registered} ->
	    ok
    after 1000 ->
	    ct:fail(gen_statem_started)
    end,
    unregister(armitage),

    %% Make sure gen_statem:enter_loop does not accept {global,Name}
    %% when it's another process than the calling one which is
    %% registered under that name
    global:register_name(armitage, self()),
    {ok,Pid6b} =
	proc_lib:start_link(?MODULE, enter_loop, [anon,global]),
    receive
	{'EXIT',Pid6b,process_not_registered_globally} ->
	    ok
    after 1000 ->
	    ct:fail(gen_statem_started)
    end,
    global:unregister_name(armitage),

    dummy_via:register_name(armitage, self()),
    {ok,Pid6c} =
	proc_lib:start_link(?MODULE, enter_loop, [anon,via]),
    receive
	{'EXIT',Pid6c,{process_not_registered_via,dummy_via}} ->
	    ok
    after 1000 ->
	    ct:fail(
	      {gen_statem_started,
	       process_info(self(), messages)})
    end,
    dummy_via:unregister_name(armitage),

    process_flag(trap_exit, OldFlag),
    ok = verify_empty_msgq().

enter_loop(Reg1, Reg2) ->
    process_flag(trap_exit, true),
    case Reg1 of
	local -> register(armitage, self());
	global -> global:register_name(armitage, self());
	via -> dummy_via:register_name(armitage, self());
	anon -> ignore
    end,
    proc_lib:init_ack({ok, self()}),
    CallbackMode = state_functions,
    case Reg2 of
	local ->
	    gen_statem:enter_loop(
	      ?MODULE, [], CallbackMode, state0, [], {local,armitage});
	global ->
	    gen_statem:enter_loop(
	      ?MODULE, [], CallbackMode, state0, [], {global,armitage});
	via ->
	    gen_statem:enter_loop(
	      ?MODULE, [], CallbackMode, state0, [],
	      {via, dummy_via, armitage});
	anon ->
	    gen_statem:enter_loop(
	      ?MODULE, [], CallbackMode, state0, [])
    end.


%% Test the order for multiple {next_event,T,C}
next_events(Config) ->
    {ok,Pid} = gen_statem:start(?MODULE, start_arg(Config, []), []),
    ok = gen_statem:cast(Pid, next_event),
    {state,next_events,[]} = gen_statem:call(Pid, get),
    ok = gen_statem:stop(Pid),
    false = erlang:is_process_alive(Pid),
    noproc =
	?EXPECT_FAILURE(gen_statem:stop(Pid), Reason).


%%
%% Functionality check
%%

wfor(Msg) ->
    receive
	Msg -> ok
    after 5000 ->
	    error(timeout)
    end.


stop_it(STM) ->
    stopped = gen_statem:call(STM, stop),
    check_stopped(STM).


check_stopped(STM) ->
    Call = there_you_are,
    {_,{gen_statem,call,[_,Call,infinity]}} =
	?EXPECT_FAILURE(gen_statem:call(STM, Call), Reason),
    ok.


do_func_test(STM) ->
    ok = gen_statem:cast(STM, {'alive?',self()}),
    wfor(yes),
    ok = do_connect(STM),
    ok = gen_statem:cast(STM, {'alive?',self()}),
    wfor(yes),
    ?t:do_times(3, ?MODULE, do_msg, [STM]),
    ok = gen_statem:cast(STM, {'alive?',self()}),
    wfor(yes),
    ok = do_disconnect(STM),
    ok = gen_statem:cast(STM, {'alive?',self()}),
    wfor(yes),
    ok.


do_connect(STM) ->
    check_state(STM, idle),
    gen_statem:cast(STM, {connect,self()}),
    wfor(accept),
    check_state(STM, wfor_conf),
    Tag = make_ref(),
    gen_statem:cast(STM, {ping,self(),Tag}),
    gen_statem:cast(STM, confirm),
    wfor({pong,Tag}),
    check_state(STM, connected),
    ok.

do_msg(STM) ->
    check_state(STM, connected),
    R = make_ref(),
    ok = gen_statem:cast(STM, {msg,self(),R}),
    wfor({ack,R}).


do_disconnect(STM) ->
    ok = gen_statem:cast(STM, disconnect),
    check_state(STM, idle).

check_state(STM, State) ->
    case gen_statem:call(STM, get) of
	{state, State, _} -> ok
    end.

do_sync_func_test(STM) ->
    yes = gen_statem:call(STM, 'alive?'),
    ok = do_sync_connect(STM),
    yes = gen_statem:call(STM, 'alive?'),
    ?t:do_times(3, ?MODULE, do_sync_msg, [STM]),
    yes = gen_statem:call(STM, 'alive?'),
    ok = do_sync_disconnect(STM),
    yes = gen_statem:call(STM, 'alive?'),
    check_state(STM, idle),
    ok = gen_statem:call(STM, {timeout,200}),
    yes = gen_statem:call(STM, 'alive?'),
    check_state(STM, idle),
    ok.


do_sync_connect(STM) ->
    check_state(STM, idle),
    accept = gen_statem:call(STM, connect),
    check_state(STM, wfor_conf),
    Tag = make_ref(),
    gen_statem:cast(STM, {ping,self(),Tag}),
    yes = gen_statem:call(STM, confirm),
    wfor({pong,Tag}),
    check_state(STM, connected),
    ok.

do_sync_msg(STM) ->
    check_state(STM, connected),
    R = make_ref(),
    {ack,R} = gen_statem:call(STM, {msg,R}),
    ok.

do_sync_disconnect(STM) ->
    yes = gen_statem:call(STM, disconnect),
    check_state(STM, idle).


verify_empty_msgq() ->
    [] = flush(),
    ok.

start_arg(Config, Arg) ->
    case lists:keyfind(callback_mode, 1, Config) of
	{_,CallbackMode} ->
	    {callback_mode,CallbackMode,Arg};
	false ->
	    Arg
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% The State Machine
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(ignore) ->
    ignore;
init(stop) ->
    {stop,stopped};
init(stop_shutdown) ->
    {stop,shutdown};
init(sleep) ->
    ?t:sleep(1000),
    {state_functions,idle,data};
init(hiber) ->
    {state_functions,hiber_idle,[]};
init(hiber_now) ->
    {state_functions,hiber_idle,[],[hibernate]};
init({data, Data}) ->
    {state_functions,idle,Data};
init({callback_mode,CallbackMode,Arg}) ->
    case init(Arg) of
	{_,State,Data,Ops} ->
	    {CallbackMode,State,Data,Ops};
	{_,State,Data} ->
	    {CallbackMode,State,Data};
	Other ->
	    Other
    end;
init({map_statem,#{init := Init}=Machine}) ->
    case Init() of
	{ok,State,Data,Ops} ->
	    {handle_event_function,State,[Data|Machine],Ops};
	{ok,State,Data} ->
	    {handle_event_function,State,[Data|Machine]};
	Other ->
	    Other
    end;
init([]) ->
    {state_functions,idle,data}.

terminate(_, _State, crash_terminate) ->
    exit({crash,terminate});
terminate({From,stopped}, State, _Data) ->
    From ! {self(),{stopped,State}},
    ok;
terminate(_Reason, _State, _Data) ->
    ok.


%% State functions

idle(cast, {connect,Pid}, Data) ->
    Pid ! accept,
    {next_state,wfor_conf,Data,infinity}; % NoOp timeout just to test API
idle({call,From}, connect, Data) ->
    gen_statem:reply(From, accept),
    {next_state,wfor_conf,Data,infinity}; % NoOp timeout just to test API
idle(cast, badreturn, _Data) ->
    badreturn;
idle({call,_From}, badreturn, _Data) ->
    badreturn;
idle({call,From}, {delayed_answer,T}, Data) ->
    receive
    after T ->
	    gen_statem:reply({reply,From,delayed}),
	    throw({keep_state,Data})
    end;
idle({call,From}, {timeout,Time}, _Data) ->
    {next_state,timeout,{From,Time},
     {timeout,Time,idle}};
idle(cast, next_event, _Data) ->
    {next_state,next_events,[a,b,c],
     [{next_event,internal,a},
      {next_event,internal,b},
      {next_event,internal,c}]};
idle(Type, Content, Data) ->
    case handle_common_events(Type, Content, idle, Data) of
	undefined ->
	    case Type of
		{call,From} ->
		    throw({keep_state,Data,[{reply,From,'eh?'}]});
		_ ->
		    throw(
		      {stop,{unexpected,idle,Type,Content}})
	    end;
	Result ->
	    Result
    end.

timeout(timeout, idle, {From,Time}) ->
    TRef = erlang:start_timer(Time, self(), ok),
    {keep_state,{From,TRef},0}; % Immediate timeout 0
timeout(timeout, 0, {From,TRef}) ->
    {next_state,timeout2,{From,TRef},
     [{timeout,1,should_be_cancelled},
      postpone]}; % Should cancel state timeout
timeout(_, _, _) ->
    keep_state_and_data.

timeout2(timeout, 0, _) ->
    keep_state_and_data;
timeout2(timeout, Reason, _) ->
    {stop,Reason};
timeout2(info, {timeout,TRef,Result}, {From,TRef}) ->
    gen_statem:reply([{reply,From,Result}]),
    {next_state,idle,state};
timeout2(_, _, _) ->
    {keep_state_and_data,[]}.

wfor_conf({call,From}, confirm, Data) ->
    {next_state,connected,Data,
     {reply,From,yes}};
wfor_conf(cast, {ping,_,_}, _) ->
    {keep_state_and_data,[postpone]};
wfor_conf(cast, confirm, Data) ->
    {next_state,connected,Data};
wfor_conf(Type, Content, Data) ->
    case handle_common_events(Type, Content, wfor_conf, Data) of
	undefined ->
	    case Type of
		{call,From} ->
		    {next_state,idle,Data,
		     [{reply,From,'eh?'}]};
		_ ->
		    throw(keep_state_and_data)
	    end;
	Result ->
	    Result
    end.

connected({call,From}, {msg,Ref}, Data) ->
    {keep_state,Data,
     {reply,From,{ack,Ref}}};
connected(cast, {msg,From,Ref}, Data) ->
    From ! {ack,Ref},
    {keep_state,Data};
connected({call,From}, disconnect, Data) ->
    {next_state,idle,Data,
     [{reply,From,yes}]};
connected(cast, disconnect, Data) ->
    {next_state,idle,Data};
connected(cast, {ping,Pid,Tag}, Data) ->
    Pid ! {pong,Tag},
    {keep_state,Data};
connected(Type, Content, Data) ->
    case handle_common_events(Type, Content, connected, Data) of
	undefined ->
	    case Type of
		{call,From} ->
		    {keep_state,Data,
		     [{reply,From,'eh?'}]};
		_ ->
		    {keep_state,Data}
	    end;
	Result ->
	    Result
    end.

state0({call,From}, stop, Data) ->
    {stop_and_reply,normal,[{reply,From,stopped}],Data};
state0(Type, Content, Data) ->
    case handle_common_events(Type, Content, state0, Data) of
	undefined ->
	    {keep_state,Data};
	Result ->
	    Result
    end.

hiber_idle({call,From}, 'alive?', Data) ->
    {keep_state,Data,
     [{reply,From,'alive!'}]};
hiber_idle({call,From}, hibernate_sync, Data) ->
    {next_state,hiber_wakeup,Data,
     [{reply,From,hibernating},
      hibernate]};
hiber_idle(info, hibernate_later, _) ->
    Tref = erlang:start_timer(1000, self(), hibernate),
    {keep_state,Tref};
hiber_idle(info, hibernate_now, Data) ->
    {keep_state,Data,
     [hibernate]};
hiber_idle(info, {timeout,Tref,hibernate}, Tref) ->
    {keep_state,[],
     [hibernate]};
hiber_idle(cast, hibernate_async, Data) ->
    {next_state,hiber_wakeup,Data,
     [hibernate]};
hiber_idle(Type, Content, Data) ->
    case handle_common_events(Type, Content, hiber_idle, Data) of
	undefined ->
	    {keep_state,Data};
	Result ->
	    Result
    end.

hiber_wakeup({call,From}, wakeup_sync, Data) ->
    {next_state,hiber_idle,Data,
     [{reply,From,good_morning}]};
hiber_wakeup({call,From}, snooze_sync, Data) ->
    {keep_state,Data,
     [{reply,From,please_just_five_more},
      hibernate]};
hiber_wakeup(cast, wakeup_async, Data) ->
    {next_state,hiber_idle,Data};
hiber_wakeup(cast, snooze_async, Data) ->
    {keep_state,Data,
     [hibernate]};
hiber_wakeup(Type, Content, Data) ->
    case handle_common_events(Type, Content, hiber_wakeup, Data) of
	undefined ->
	    {keep_state,Data};
	Result ->
	    Result
    end.

next_events(internal, Msg, [Msg|Msgs]) ->
    {keep_state,Msgs};
next_events(Type, Content, Data) ->
    case handle_common_events(Type, Content, next_events, Data) of
	undefined ->
	    {keep_state,Data};
	Result ->
	    Result
    end.


handle_common_events({call,From}, get_callback_mode, _, _) ->
    {keep_state_and_data,{reply,From,state_functions}};
handle_common_events({call,From}, get, State, Data) ->
    {keep_state,Data,
     [{reply,From,{state,State,Data}}]};
handle_common_events(cast, {get,Pid}, State, Data) ->
    Pid ! {state,State,Data},
    {keep_state,Data};
handle_common_events({call,From}, stop, _, Data) ->
    {stop_and_reply,normal,[{reply,From,stopped}],Data};
handle_common_events(cast, stop, _, _) ->
    stop;
handle_common_events({call,From}, {stop,Reason}, _, Data) ->
    {stop_and_reply,Reason,{reply,From,stopped},Data};
handle_common_events(cast, {stop,Reason}, _, _) ->
    {stop,Reason};
handle_common_events({call,From}, 'alive?', _, Data) ->
    {keep_state,Data,
     [{reply,From,yes}]};
handle_common_events(cast, {'alive?',Pid}, _, Data) ->
    Pid ! yes,
    {keep_state,Data};
handle_common_events(_, _, _, _) ->
    undefined.

handle_event({call,From}, get_callback_mode, _, _) ->
    {keep_state_and_data,{reply,From,handle_event_function}};
%% Wrapper state machine that uses a map state machine spec
handle_event(
  Type, Event, State, [Data|Machine])
  when is_map(Machine) ->
    #{State := HandleEvent} = Machine,
    case
	try HandleEvent(Type, Event, Data) of
	    Result ->
		Result
	catch
	    Result ->
		Result
	end of
	{stop,Reason,NewData} ->
	    {stop,Reason,[NewData|Machine]};
	{next_state,NewState,NewData} ->
	    {next_state,NewState,[NewData|Machine]};
	{next_state,NewState,NewData,Ops} ->
	    {next_state,NewState,[NewData|Machine],Ops};
	{keep_state,NewData} ->
	    {keep_state,[NewData|Machine]};
	{keep_state,NewData,Ops} ->
	    {keep_state,[NewData|Machine],Ops};
	Other ->
	    Other
    end;
%%
%% Dispatcher to test callback_mode handle_event_function
%%
%% Wrap the state in a 1 element list just to test non-atom
%% states.  Note that the state from init/1 is not wrapped
%% so both atom and non-atom states are tested.
handle_event(Type, Event, State, Data) ->
    StateName = unwrap_state(State),
    try ?MODULE:StateName(Type, Event, Data) of
	Result ->
	    wrap_result(Result)
    catch
	throw:Result ->
	    erlang:raise(
	      throw, wrap_result(Result), erlang:get_stacktrace())
    end.

unwrap_state([State]) ->
    State;
unwrap_state(State) ->
    State.

wrap_result(Result) ->
    case Result of
	{next_state,NewState,NewData} ->
	    {next_state,[NewState],NewData};
	{next_state,NewState,NewData,StateOps} ->
	    {next_state,[NewState],NewData,StateOps};
	Other ->
	    Other
    end.



code_change(OldVsn, State, Data, CallbackMode) ->
    {CallbackMode,State,{OldVsn,Data,CallbackMode}}.

format_status(terminate, [_Pdict,State,Data]) ->
    {formatted,State,Data};
format_status(normal, [_Pdict,_State,_Data]) ->
    [format_status_called].

flush() ->
    receive
	Msg ->
	    [Msg|flush()]
    after 500 ->
	    []
    end.
