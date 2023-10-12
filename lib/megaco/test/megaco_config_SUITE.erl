%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2000-2022. All Rights Reserved.
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
%% Purpose: Test application config
%%----------------------------------------------------------------------

-module(megaco_config_SUITE).

-export([
         suite/0, all/0, groups/0,
	 init_per_suite/1,    end_per_suite/1, 
	 init_per_group/2,    end_per_group/2, 
	 init_per_testcase/2, end_per_testcase/2, 

         config/1,
         transaction_id_counter_mg/1,
         transaction_id_counter_mgc/1,
         otp_7216/1,
         otp_8167/1,
         otp_8183/1
        ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("megaco/include/megaco.hrl").
-include_lib("megaco/src/app/megaco_internal.hrl").
-include("megaco_test_lib.hrl").

-record(command, {id, desc, cmd, verify}).

-define(TEST_VERBOSITY,     debug).
-define(CNT_PROCS_NUM,      100).
-define(CNT_PROCS_CHUNK,    10).
-define(MAX_TRANS_ID,       1000).
-define(MAX_TRANS_ID_CHUNK, 100).


%%======================================================================
%% Common Test interface functions
%%======================================================================

suite() -> 
    [{ct_hooks, [ts_install_cth]}].

all() -> 
    [
     config,
     {group, transaction_id_counter},
     {group, tickets}
    ].

groups() -> 
    [
     {transaction_id_counter, [], transaction_id_counter_cases()},
     {tickets,                [], tickets_cases()}
    ].



transaction_id_counter_cases() ->
    [
     transaction_id_counter_mg,
     transaction_id_counter_mgc
    ].

tickets_cases() ->
    [
     otp_7216,
     otp_8167,
     otp_8183
    ].



%%
%% -----
%%

init_per_suite(Config0) when is_list(Config0) ->

    p("init_per_suite -> entry with"
      "~n      Config: ~p"
      "~n      Nodes:  ~p", [Config0, erlang:nodes()]),

    case ?LIB:init_per_suite(Config0) of
        {skip, _} = SKIP ->
            SKIP;

        Config1 when is_list(Config1) ->

            %% We need a (local) monitor on this node also
            megaco_test_sys_monitor:start(),

            p("init_per_suite -> end when"
              "~n      Config: ~p"
              "~n      Nodes:  ~p", [Config1, erlang:nodes()]),
            
            Config1
    end.

end_per_suite(Config0) when is_list(Config0) ->

    p("end_per_suite -> entry with"
      "~n      Config0: ~p"
      "~n      Nodes:  ~p", [Config0, erlang:nodes()]),

    megaco_test_sys_monitor:stop(),
    Config1 = ?LIB:end_per_suite(Config0),

    p("end_per_suite -> end when"
      "~n      Nodes:  ~p", [erlang:nodes()]),
    Config1.



%%
%% -----
%%

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


%%
%% -----
%%

%% Test server callbacks
init_per_testcase(Case, Config) when (Case =/= config) ->
    i("init_per_testcase -> entry with"
      "~n   Config: ~p"
      "~n   Nodes:  ~p", [Config, erlang:nodes()]),

    megaco_test_global_sys_monitor:reset_events(),

    i("try starting megaco_config"),
    case megaco_config:start_link() of
        {ok, _} ->
            C = lists:keydelete(tc_timeout, 1, Config),
            do_init_per_testcase(Case, [{tc_timeout, min(3)}|C]);
        {error, Reason} ->
            i("Failed starting megaco_config: "
              "~n   ~p", [Reason]),
            {skip, ?F("Failed starting config: ~p", [Reason])}
    end;
init_per_testcase(Case, Config) ->
    i("init_per_testcase -> entry with"
      "~n   Config: ~p"
      "~n   Nodes:  ~p", [Config, erlang:nodes()]),

    megaco_test_global_sys_monitor:reset_events(),

    C = lists:keydelete(tc_timeout, 1, Config),
    do_init_per_testcase(Case, [{tc_timeout, min(3)}|C]).

do_init_per_testcase(Case, Config) ->
    process_flag(trap_exit, true),
    megaco_test_lib:init_per_testcase(Case, Config).

min(M) -> timer:minutes(M).


end_per_testcase(Case, Config) when (Case =/= config) ->
    p("end_per_testcase -> entry with"
      "~n   Config: ~p"
      "~n   Nodes:  ~p", [Config, erlang:nodes()]),

    p("system events during test: "
      "~n   ~p", [megaco_test_global_sys_monitor:events()]),

    (catch megaco_config:stop()),
    process_flag(trap_exit, false),
    megaco_test_lib:end_per_testcase(Case, Config);
end_per_testcase(Case, Config) ->
    p("end_per_testcase -> entry with"
      "~n   Config: ~p"
      "~n   Nodes:  ~p", [Config, erlang:nodes()]),

    p("system events during test: "
      "~n   ~p", [megaco_test_global_sys_monitor:events()]),

    process_flag(trap_exit, false),
    megaco_test_lib:end_per_testcase(Case, Config).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Config test case

config(suite) ->
    [];
config(Config) when is_list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Mid = fake_mid,
    
    %% Nice values
    Int = 3,
    IT  = #megaco_incr_timer{max_retries = Int},

    %% Evil values
    NonInt = non_int,
    IT2 = #megaco_incr_timer{wait_for = NonInt},
    IT3 = #megaco_incr_timer{factor = NonInt},
    IT4 = #megaco_incr_timer{max_retries = NonInt},
    IT5 = #megaco_incr_timer{max_retries = non_infinity},

    %% Command range values
    Initial = 100,
    Verify  = 200,
    Nice    = 300,
    Evil    = 400,
    End     = 500,

    InitialCmd = 
	fun(No, Desc, Cmd, VerifyVal) ->
		initial_command(Initial + No, Desc, Cmd, VerifyVal)
	end,

    VerifyCmd = 
	fun(M, No, Key, V) ->
		verify_user_default_command(M, Verify + No, Key, V)
	end,

    NiceCmd = 
	fun(M, No, Key, Val) ->
		nice_user_update_command(M, Nice + No, Key, Val)
	end,

    EvilCmd = 
	fun(M, No, Key, Val) ->
		evil_user_update_command(M, Evil + No, Key, Val)
	end,
    
    %% End commands
    ExitCmd = 
	fun(No, Desc, Cmd) ->
		exit_command(End + No, Desc, Cmd)
	end,
    ErrorCmd = 
	fun(No, Desc, Cmd, MainReason, TS) ->
		error_command(End + No, Desc, Cmd, MainReason, TS)
	end,
    PlainCmd = 
	fun(No, Desc, Cmd, V) ->
		command(End + No, Desc, Cmd, V)
	end,

    Commands = 
	[
	 %% Initial commands
	 InitialCmd(0, 
		    "enable trace", 
		    fun() -> megaco:enable_trace(100, io) end, 
		    ok),
	 InitialCmd(1, 
		    "start", 
		    fun() -> megaco:start() end, 
		    ok),
	 InitialCmd(2, 
		    "Verify no active requests", 
		    fun() -> megaco:system_info(n_active_requests) end,
		    0),
	 InitialCmd(3, 
		    "Verify no active replies", 
		    fun() -> megaco:system_info(n_active_replies) end,
		    0),
	 InitialCmd(4, 
		    "Verify no active connections", 
		    fun() -> megaco:system_info(n_active_connections) end,
		    0), 
	 InitialCmd(5, 
		    "Verify no connections", 
		    fun() -> megaco:system_info(connections) end, 
		    []),
	 InitialCmd(6, 
		    "Verify no users", 
		    fun() -> megaco:system_info(users) end, 
		    []), 
	 InitialCmd(7, 
		    "Start user", 
		    fun() -> megaco:start_user(Mid, []) end, 
		    ok),
	 

	 %% Verify user defaults
	 VerifyCmd(Mid,  1, connections, []),
	 VerifyCmd(Mid,  2, min_trans_id, 1), 
	 VerifyCmd(Mid,  3, max_trans_id, infinity), 
	 VerifyCmd(Mid,  4, request_timer, #megaco_incr_timer{}), 
	 VerifyCmd(Mid,  5, long_request_timer, timer:seconds(60)), 
	 VerifyCmd(Mid,  6, auto_ack, false), 
	 VerifyCmd(Mid,  7, pending_timer, 30000), 
	 VerifyCmd(Mid,  8, reply_timer, 30000), 
	 VerifyCmd(Mid,  9, send_mod, megaco_tcp), 
	 VerifyCmd(Mid, 10, encoding_mod, megaco_pretty_text_encoder), 
	 VerifyCmd(Mid, 11, encoding_config, []), 
	 VerifyCmd(Mid, 12, protocol_version, 1), 
	 VerifyCmd(Mid, 13, reply_data, undefined), 
	 VerifyCmd(Mid, 14, receive_handle, 
		   fun(H) when is_record(H, megaco_receive_handle) -> 
			   {ok, H};
		      (R)  -> 
			   {error, R}
		   end), 


	 %% Nice update
	 NiceCmd(Mid,  1, min_trans_id, Int), 
	 NiceCmd(Mid,  2, max_trans_id, Int), 
	 NiceCmd(Mid,  3, max_trans_id, infinity), 
	 NiceCmd(Mid,  4, request_timer, Int), 
	 NiceCmd(Mid,  5, request_timer, infinity), 
	 NiceCmd(Mid,  6, request_timer, IT), 
	 NiceCmd(Mid,  7, long_request_timer, Int), 
	 NiceCmd(Mid,  8, long_request_timer, infinity), 
	 NiceCmd(Mid,  9, long_request_timer, IT), 
	 NiceCmd(Mid, 10, auto_ack, true), 
	 NiceCmd(Mid, 11, auto_ack, false), 
	 NiceCmd(Mid, 12, pending_timer, Int), 
	 NiceCmd(Mid, 13, pending_timer, infinity), 
	 NiceCmd(Mid, 14, pending_timer, IT), 
	 NiceCmd(Mid, 15, reply_timer, Int), 
	 NiceCmd(Mid, 16, reply_timer, infinity), 
	 NiceCmd(Mid, 17, reply_timer, IT), 
	 NiceCmd(Mid, 18, send_mod, an_atom), 
	 NiceCmd(Mid, 19, encoding_mod, an_atom), 
	 NiceCmd(Mid, 20, encoding_config, []), 
	 NiceCmd(Mid, 21, protocol_version, Int), 
	 NiceCmd(Mid, 23, reply_data, IT), 
	 NiceCmd(Mid, 23, resend_indication, true), 
	 NiceCmd(Mid, 24, resend_indication, false), 
	 NiceCmd(Mid, 25, resend_indication, flag),


	 %% Evil update
	 EvilCmd(Mid,  1, min_trans_id, NonInt), 
	 EvilCmd(Mid,  2, max_trans_id, NonInt), 
	 EvilCmd(Mid,  3, max_trans_id, non_infinity), 
	 EvilCmd(Mid,  4, request_timer, NonInt), 
	 EvilCmd(Mid,  5, request_timer, non_infinity), 
	 EvilCmd(Mid,  6, request_timer, IT2), 
	 EvilCmd(Mid,  7, request_timer, IT3), 
	 EvilCmd(Mid,  8, request_timer, IT4), 
	 EvilCmd(Mid,  9, request_timer, IT5), 
	 EvilCmd(Mid, 10, long_request_timer, NonInt), 
	 EvilCmd(Mid, 11, long_request_timer, non_infinity), 
	 EvilCmd(Mid, 12, long_request_timer, IT2), 
	 EvilCmd(Mid, 13, long_request_timer, IT3), 
	 EvilCmd(Mid, 14, long_request_timer, IT4), 
	 EvilCmd(Mid, 15, long_request_timer, IT5), 
	 EvilCmd(Mid, 16, auto_ack, non_bool), 
	 EvilCmd(Mid, 17, pending_timer, NonInt), 
	 EvilCmd(Mid, 18, pending_timer, non_infinity), 
	 EvilCmd(Mid, 19, pending_timer, IT2), 
	 EvilCmd(Mid, 20, pending_timer, IT3), 
	 EvilCmd(Mid, 21, pending_timer, IT4), 
	 EvilCmd(Mid, 22, pending_timer, IT5), 
	 EvilCmd(Mid, 23, reply_timer, NonInt), 
	 EvilCmd(Mid, 24, reply_timer, non_infinity), 
	 EvilCmd(Mid, 25, reply_timer, IT2), 
	 EvilCmd(Mid, 26, reply_timer, IT3), 
	 EvilCmd(Mid, 27, reply_timer, IT4), 
	 EvilCmd(Mid, 28, reply_timer, IT5), 
	 EvilCmd(Mid, 29, send_mod, {non_atom}), 
	 EvilCmd(Mid, 30, encoding_mod, {non_atom}), 
	 EvilCmd(Mid, 31, encoding_config, non_list), 
	 EvilCmd(Mid, 32, protocol_version, NonInt),
	 EvilCmd(Mid, 33, resend_indication, flagg),


	 %% End 
	 ExitCmd(1, "Verify non-existing system info", 
		 fun() -> megaco:system_info(non_exist) end),
	 ExitCmd(2, "Verify non-existing user user info", 
		 fun() -> megaco:user_info(non_exist, trans_id) end),
	 ExitCmd(3, "Verify non-existing user info", 
		 fun() -> megaco:user_info(Mid, non_exist) end),

	 ErrorCmd(4, "Try updating user info for non-existing user", 
		  fun() -> 
			  megaco:update_user_info(non_exist, trans_id, 1) 
		  end,
		  no_such_user, 2),
	 ErrorCmd(11, "Try updating non-existing user info", 
		  fun() -> 
			  megaco:update_user_info(Mid, trans_id, 4711) 
		  end,
		  bad_user_val, 4),
	 ErrorCmd(12, "Try start already started user", 
		  fun() -> megaco:start_user(Mid, []) end,
		  user_already_exists, 2),

	 PlainCmd(13, "Verify started users", 
		  fun() -> megaco:system_info(users) end, [Mid]),
	 PlainCmd(14, "Stop user", fun() -> megaco:stop_user(Mid) end, ok),
	 PlainCmd(15, "Verify started users", 
		  fun() -> megaco:system_info(users) end, []),
	 ErrorCmd(16, "Try stop not started user",
		  fun() -> megaco:stop_user(Mid) end, no_such_user, 2),
	 ErrorCmd(17, "Try start megaco (it's already started)",
		  fun() -> megaco:start() end, already_started, 2),
	 PlainCmd(18, "Stop megaco", fun() -> megaco:stop() end, ok),
	 ErrorCmd(19, "Try stop megaco (it's not running)",
		  fun() -> megaco:stop() end, not_started, 2)
	],

    
    exec(Commands).
    


exec([]) ->
    ok;
exec([#command{id     = No, 
	       desc   = Desc, 
	       cmd    = Cmd, 
	       verify = Verify}|Commands]) ->
    io:format("Executing command ~3w: ~s: ", [No, Desc]),
    case (catch Verify((catch Cmd()))) of
	{ok, OK} ->
	    io:format("ok => ~p~n", [OK]),
	    exec(Commands);
	{error, Reason} ->
	    io:format("error => ~p~n", [Reason]),
	    {error, {bad_result, No, Reason}};
	Error ->
	    io:format("exit => ~p~n", [Error]),
	    {error, {unexpected_result, No, Error}}
    end.

initial_command(No, Desc0, Cmd, VerifyVal) when is_function(Cmd) ->
    Desc = lists:flatten(io_lib:format("Initial - ~s", [Desc0])),
    command(No, Desc, Cmd, VerifyVal).
    
verify_user_default_command(Mid, No, Key, Verify) ->
    Desc = lists:flatten(io_lib:format("Defaults - Verify ~w", [Key])),
    Cmd = fun() -> megaco:user_info(Mid, Key) end,
    command(No, Desc, Cmd, Verify).
		     
nice_user_update_command(Mid, No, Key, Val) ->
    Desc = lists:flatten(io_lib:format("Nice - Update ~w", [Key])),
    Cmd = fun() -> megaco:update_user_info(Mid, Key, Val) end,
    Verify = fun(ok) ->
		     case (catch megaco:user_info(Mid, Key)) of
			 {'EXIT', R} ->
			     {error, {value_retreival_failed, R}};
			 Val ->
			     {ok, Val};
			 Invalid ->
			     {error, {value_update_failed, Val, Invalid}}
		     end;
		(R)  -> 
		     {error, R}
	     end,
    command(No, Desc, Cmd, Verify).


evil_user_update_command(Mid, No, Key, Val) ->
    Desc = lists:flatten(io_lib:format("Evil - Update ~w", [Key])),
    Cmd = fun() ->
		  case (catch megaco:user_info(Mid, Key)) of
		      {'EXIT', R} ->
			  {{error, {old_value_retreival_failed, R}}, 
			   ignore};
		      OldVal ->
			  {OldVal,
			   (catch megaco:update_user_info(Mid, Key, Val))}
		  end
	  end,
    Verify = fun({{error, _} = Error, ignore}) ->
		     Error;
		({OldVal, {error, {bad_user_val, _, _, _}}}) ->
		     case (catch megaco:user_info(Mid, Key)) of
			 {'EXIT', R} ->
			     {error, {value_retreival_failed, R}};
			 OldVal ->
			     {ok, OldVal};
			 Invalid ->
			     {error, {value_update_failed, OldVal, Invalid}}
		     end;
		(R) ->
		     {error, R}
	     end,
    command(No, Desc, Cmd, Verify).

exit_command(No, Desc, Cmd) when is_function(Cmd) ->    
    Verify = fun({'EXIT', _} = E) ->
		     {ok, E};
		(R) ->
		     {error, R}
	     end,
    command(No, Desc, Cmd, Verify).

error_command(No, Desc, Cmd, MainReason, TS) when is_function(Cmd) ->
    Verify = fun({error, Reason}) ->
		     io:format("verify -> Reason: ~n~p~n", [Reason]), 
		     case Reason of
			 {MainReason, _} when TS == 2 ->
			     {ok, MainReason};
			 {MainReason, _, _, _} when TS == 4 ->
			     {ok, MainReason};
			 _ ->
			     {error, Reason}
		     end;
		(R) ->
		     {error, R}
	     end,
    command(No, Desc, Cmd, Verify).

command(No, Desc, Cmd, Verify) 
  when (is_integer(No) andalso 
	is_list(Desc) andalso 
	is_function(Cmd) andalso 
	is_function(Verify)) ->
    #command{id     = No, 
	     desc   = Desc,
	     cmd    = Cmd,
	     verify = Verify};
command(No, Desc, Cmd, VerifyVal) 
  when (is_integer(No) andalso 
	is_list(Desc) andalso 
	is_function(Cmd)) ->
    Verify = fun(Val) ->
		     case Val of
			 VerifyVal ->
			     {ok, Val};
			 _ ->
			     {error, Val}
		     end
	     end,
    #command{id     = No, 
	     desc   = Desc,
	     cmd    = Cmd,
	     verify = Verify}.
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

calc_max_trans_id(Config) ->
    calc_number(Config, ?MAX_TRANS_ID_CHUNK, ?MAX_TRANS_ID).

calc_num_cnt_procs(Config) ->
    calc_number(Config, ?CNT_PROCS_CHUNK, ?CNT_PROCS_NUM).

calc_number(Config, Chunk, Max) ->
    Factor = ?config(megaco_factor, Config),
    if
        (Factor =:= 1) ->
            Max;
        (Factor < 10) ->
            calc_number2(Factor, Chunk, Max);
        true ->
            calc_number2(10, Chunk, Max)
    end.

calc_number2(Factor, Chunk, Max) ->
    Max - ((Factor-1) * Chunk).


transaction_id_counter_mg(suite) ->
    [];
transaction_id_counter_mg(doc) ->
    ["This test case is intended to test and verify the "
     "transaction counter handling of the application "
     "in with one connection (MG). "];
transaction_id_counter_mg(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        ?FUNCTION_NAME),
    
    process_flag(trap_exit, true),

    MaxTransID  = calc_max_trans_id(Config),
    NumCntProcs = calc_num_cnt_procs(Config),

    i("starting with"
      "~n      Max Transaction ID:      ~w"
      "~n      Number Of Counter Procs: ~w", [MaxTransID, NumCntProcs]),

    %% Basic user data
    UserMid = {deviceName, "mg"},
    UserConfig = [
		  {min_trans_id, 1}
		 ],

    %% Basic connection data
    RemoteMid = {deviceName, "mgc"},
    RecvHandle = #megaco_receive_handle{local_mid       = UserMid,
					encoding_mod    = ?MODULE,
					encoding_config = [],
					send_mod        = ?MODULE},
    SendHandle = dummy_send_handle,
    ControlPid = self(), 

    %% Start user
    i("start user"),
    ok = megaco_config:start_user(UserMid, UserConfig),

    %% Create connection
    i("create connection"),
    {ok, CD} = 
	megaco_config:connect(RecvHandle, RemoteMid, SendHandle, ControlPid),

    %% Set counter limits
    i("set counter max limit"),
    CH = CD#conn_data.conn_handle, 
    megaco_config:update_conn_info(CH, max_trans_id, MaxTransID),

    %% Create the counter worker procs
    i("create ~w counter working procs", [NumCntProcs]),
    Pids = create_counter_working_procs(CH, NumCntProcs, []),

    %% Start the counter worker procs
    i("release the counter working procs"),
    start_counter_working_procs(Pids),

    %% Await the counter worker procs termination
    i("await the counter working procs completion"),
    ok = await_completion_counter_working_procs(Pids),

    %% Verify result
    i("verify counter result"),
    TransId = megaco_config:conn_info(CH, trans_id),
    1 = TransId,

    %% Stop test
    i("disconnect"),
    {ok, _, _} = megaco_config:disconnect(CH),
    i("stop user"),
    ok = megaco_config:stop_user(UserMid),

    i("done"),
    ok.



create_counter_working_procs(_CH, 0, Pids) ->
    Pids;
create_counter_working_procs(CH, N, Pids) ->
    TC = get(tc),
    Pid = erlang:spawn_link(fun() -> counter_init(CH, TC) end),
    create_counter_working_procs(CH, N-1, [Pid | Pids]).

counter_init(CH, TC) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     lists:flatten(io_lib:format("CNT-~p", [self()]))),
    put(tc,        TC),    
    UserMid = CH#megaco_conn_handle.local_mid,
    Min = megaco_config:user_info(UserMid, min_trans_id),
    Max = megaco_config:conn_info(CH, max_trans_id),
    Num = Max - Min + 1,
    receive
	start ->
	    %% i("received start command (~p)", [Num]),
	    ok
    end,
    counter_loop(CH, Num).

counter_loop(_CH, 0) ->
    %% i("done"),
    exit(normal);
counter_loop(CH, Num) when (Num > 0) ->
    megaco_config:incr_trans_id_counter(CH, 1),
    counter_loop(CH, Num-1).

start_counter_working_procs([]) ->
    %% i("released"),
    ok;
start_counter_working_procs([Pid | Pids]) ->
    Pid ! start,
    start_counter_working_procs(Pids).

await_completion_counter_working_procs(Pids) ->
    await_completion_counter_working_procs(0, ts(), Pids, [], []).

await_completion_counter_working_procs(MaxTSD, _TS, [], _OKs, [] = _ERRs) ->
    i("done when ok with max TS-diff: ~p", [MaxTSD]),
    ok;
await_completion_counter_working_procs(MaxTSD, _TS, [], _OKs, ERRs) ->
    i("done when error (~w) with max TS-diff: ~p", [length(ERRs), MaxTSD]),
    {error, ERRs};
await_completion_counter_working_procs(MaxTSD, TS, Pids, OKs, ERRs) ->
    receive
	{'EXIT', Pid, normal} ->
            %% i("counter working process completion[~w, ~w, ~w] -> "
            %%   "Expected exit from counter process: "
            %%   "~n      Time since last event: ~s"
            %%   "~n      Pid: ~p",
            %%   [length(Pids), length(OKs), length(ERRs), tsd(TS), Pid]),
            TSD = ts() - TS,
            MaxTSD2 =
                if (TSD > MaxTSD) ->
                        i("counter working process completion[~w, ~w, ~w] -> "
                          "Expected exit from counter process: "
                          "~n      New max time since last event: ~s"
                          "~n      Pid: ~p",
                          [length(Pids),
                           length(OKs),
                           length(ERRs),
                           tsd(TSD),
                           Pid]),
                        TSD;
                   true ->
                        MaxTSD
                end,
	    Pids2 = lists:delete(Pid, Pids),
	    await_completion_counter_working_procs(MaxTSD2, ts(),
                                                   Pids2, [Pid | OKs], ERRs);

	{'EXIT', Pid, {timetrap_timeout, _Timeout, Stack} = _Reason} ->
            TSD = ts() - TS,
            MaxTSD2 =
                if (TSD > MaxTSD) ->
                        TSD;
                   true ->
                        MaxTSD
                end,
            e("counter working process completion[~w, ~w, ~w] -> "
              "Test case timeout timetrap"
              "~n      Time since last event: ~s (max ~s)"
              "~n      Pid:                   ~p (~p)"
              "~n      Stack:                 ~p",
              [length(Pids), length(OKs), length(ERRs),
               tsd(TSD), tsd(MaxTSD2),
               Pid, lists:member(Pid, Pids),
               Stack]),
            %% The test case (timetrap) has timed out, which either means
            %% we are running on very slow hw or some system functions
            %% are slowing us down (this test case should never normally
            %% time out).
            case megaco_test_global_sys_monitor:events(?SECS(5)) of
                {error, timeout} ->
                    i("counter working process completion[~w, ~w, ~w] -> "
                      "idle:sys-mon timeout",
                      [length(Pids), length(OKs), length(ERRs)]),
                    ?SKIP("TC idle; sys-monitor evs timeout");
                [] ->
                    i("counter working process completion[~w, ~w, ~w] -> idle",
                      [length(Pids), length(OKs), length(ERRs)]),
                    ?SKIP("TC idle");
                SysEvs ->
                    e("counter working process completion[~w, ~w, ~w] -> "
                      "system event(s): "
                      "~n      ~p",
                      [length(Pids), length(OKs), length(ERRs), SysEvs]),
                    ?SKIP("TC system events")
            end;

	{'EXIT', Pid, Reason} ->
            TSD = ts() - TS,
            MaxTSD2 =
                if (TSD > MaxTSD) ->
                        TSD;
                   true ->
                        MaxTSD
                end,
            e("counter working process completion[~w, ~w, ~w] -> "
              "Unexpected exit from counter process: "
              "~n      Time since last event: ~s (max ~s)"
              "~n      Pid:                   ~p"
              "~n      Reason:                ~p",
              [length(Pids), length(OKs), length(ERRs),
               tsd(TSD), tsd(MaxTSD2),
               Pid, Reason]),
	    Pids2 = lists:delete(Pid, Pids),
	    await_completion_counter_working_procs(MaxTSD2, ts(),
                                                   Pids2, OKs, [Pid | ERRs]);

	Any ->
            TSD = ts() - TS,
            MaxTSD2 =
                if (TSD > MaxTSD) ->
                        TSD;
                   true ->
                        MaxTSD
                end,
            e("counter working process completion[~w, ~w, ~w] -> "
              "Unexpected message: "
              "~n      Time since last event: ~s (max ~s)"
              "~n      ~p",
              [length(Pids), length(OKs), length(ERRs),
               tsd(TSD), tsd(MaxTSD2),
               Any]),
	    await_completion_counter_working_procs(MaxTSD2, TS,
                                                   Pids, OKs, ERRs)

    after 1000 ->
            %% If nothing has happened for this long, something is wrong:
            %% Check system events
            TS2 = ts(),
            TSD = TS2 - TS,
            case megaco_test_global_sys_monitor:events() of
                [] ->
                    TS3 = ts(),
                    i("counter working process completion[~w, ~w, ~w] -> idle"
                      "~n      Time since last event:       ~s"
                      "~n      (global) sys monitor events: ~s",
                      [length(Pids), length(OKs), length(ERRs),
                       tsd(TSD), tsd(TS3 - TS2)]),
                    await_completion_counter_working_procs(MaxTSD, TS,
                                                           Pids, OKs, ERRs);
                SysEvs ->
                    TS3 = ts(),
                    e("counter working process completion[~w, ~w, ~w] -> "
                      "system event(s): "
                      "~n      Time since last event:       ~s"
                      "~n      (global) sys monitor events: ~s"
                      "~n      ~p",
                      [length(Pids), length(OKs), length(ERRs),
                       tsd(TSD), tsd(TS3 - TS2),
                       SysEvs]),
                    ?SKIP("TC idle with system events")
            end
    end.
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

transaction_id_counter_mgc(suite) ->
    [];
transaction_id_counter_mgc(doc) ->
    ["This test case is intended to test and verify the "
     "transaction counter handling of the application "
     "in with several connections (MGC). "];
transaction_id_counter_mgc(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        ?FUNCTION_NAME),
    
    Name        = ?FUNCTION_NAME,
    MaxTransID  = calc_max_trans_id(Config),
    NumCntProcs = calc_num_cnt_procs(Config),                   

    i("starting with"
      "~n      Max Transaction ID:      ~w"
      "~n      Number Of Counter Procs: ~w", [MaxTransID, NumCntProcs]),

    Pre = fun() ->
                  %% Basic user data
                  UserMid = {deviceName, "mgc"},
                  UserConfig = [
                                {min_trans_id, 1}
                               ],

                  %% Basic connection data
                  RemoteMids = 
                      [
                       {deviceName, "mg01"},
                       {deviceName, "mg02"},
                       {deviceName, "mg03"},
                       {deviceName, "mg04"},
                       {deviceName, "mg05"},
                       {deviceName, "mg06"},
                       {deviceName, "mg07"},
                       {deviceName, "mg08"},
                       {deviceName, "mg09"},
                       {deviceName, "mg10"}
                      ], 
                  RecvHandles = 
                      [
                       #megaco_receive_handle{local_mid     = UserMid,
                                              encoding_mod    = ?MODULE,
                                              encoding_config = [],
                                              send_mod        = ?MODULE},
                       #megaco_receive_handle{local_mid     = UserMid,
                                              encoding_mod    = ?MODULE,
                                              encoding_config = [],
                                              send_mod        = ?MODULE},
                       #megaco_receive_handle{local_mid     = UserMid,
                                              encoding_mod    = ?MODULE,
                                              encoding_config = [],
                                              send_mod        = ?MODULE},
                       #megaco_receive_handle{local_mid     = UserMid,
                                              encoding_mod    = ?MODULE,
                                              encoding_config = [],
                                              send_mod        = ?MODULE},
                       #megaco_receive_handle{local_mid     = UserMid,
                                              encoding_mod    = ?MODULE,
                                              encoding_config = [],
                                              send_mod        = ?MODULE},
                       #megaco_receive_handle{local_mid     = UserMid,
                                              encoding_mod    = ?MODULE,
                                              encoding_config = [],
                                              send_mod        = ?MODULE},
                       #megaco_receive_handle{local_mid     = UserMid,
                                              encoding_mod    = ?MODULE,
                                              encoding_config = [],
                                              send_mod        = ?MODULE},
                       #megaco_receive_handle{local_mid     = UserMid,
                                              encoding_mod    = ?MODULE,
                                              encoding_config = [],
                                              send_mod        = ?MODULE},
                       #megaco_receive_handle{local_mid     = UserMid,
                                              encoding_mod    = ?MODULE,
                                              encoding_config = [],
                                              send_mod        = ?MODULE},
                       #megaco_receive_handle{local_mid     = UserMid,
                                              encoding_mod    = ?MODULE,
                                              encoding_config = [],
                                              send_mod        = ?MODULE}
                      ],
                  SendHandle = dummy_send_handle,
                  ControlPid = self(),
    
                  %% Start user
                  i("start user"),
                  ok = megaco_config:start_user(UserMid, UserConfig),

                  %% Create connection
                  i("create connection(s)"),
                  CDs = create_connections(RecvHandles,
                                           RemoteMids,
                                           SendHandle,
                                           ControlPid),

                  %% Set counter limits
                  i("set counter max limit(s)"),
                  set_counter_max_limits(CDs, MaxTransID),

                  {UserMid, CDs}
          end,

    Case = fun({_, CDs}) ->
                   %% Create the counter worker procs
                   i("create ~w counter working procs", [NumCntProcs]),
                   Pids = create_counter_working_procs(CDs, NumCntProcs),

                   %% Start the counter worker procs
                   i("release the counter working procs"),
                   start_counter_working_procs(Pids),

                   %% Await the counter worker procs termination
                   i("await the counter working procs completion"),
                   ok = await_completion_counter_working_procs(Pids),

                   %% Verify result
                   i("verify counter result"),
                   verify_counter_results(CDs)
           end,

    Post = fun({UserMid, CDs}) ->
                   %% Stop test
                   i("disconnect"),
                   delete_connections(CDs), 
                   i("stop user"),
                   ok = megaco_config:stop_user(UserMid),
                   i("done"),
                   ok
           end,
    try_tc(Name, Pre, Case, Post).


create_connections(RecvHandles, RemoteMids, SendHandle, ControlPid) ->
    create_connections(RecvHandles, RemoteMids, SendHandle, ControlPid, []).

create_connections([], [], _SendHandle, _ControlPid, Acc) ->
    lists:reverse(Acc);
create_connections([RecvHandle | RecvHandles], 
		   [RemoteMid  | RemoteMids], 
		   SendHandle, ControlPid, Acc) ->
    {ok, CD} = 
	megaco_config:connect(RecvHandle, RemoteMid, SendHandle, ControlPid),
    create_connections(RecvHandles, RemoteMids, 
		       SendHandle, ControlPid, [CD | Acc]).


set_counter_max_limits([], _MaxTransId) ->
    ok;
set_counter_max_limits([#conn_data{conn_handle = CH} | CDs], MaxTransId) ->
    megaco_config:update_conn_info(CH, max_trans_id, MaxTransId),
    set_counter_max_limits(CDs, MaxTransId).
    

create_counter_working_procs(CDs, NumCntProcs) ->
    lists:flatten(create_counter_working_procs2(CDs, NumCntProcs)).

create_counter_working_procs2([], _NumCntProcs) ->
    [];
create_counter_working_procs2([#conn_data{conn_handle = CH} | CDs], 
			      NumCntProcs) ->
    [create_counter_working_procs(CH, NumCntProcs, []) |
     create_counter_working_procs2(CDs, NumCntProcs)].


verify_counter_results(CDs) ->
    verify_counter_results(CDs, [], []).

verify_counter_results([], _OKs, [] = _Errors) ->
    i("counter verification success"),
    ok;
verify_counter_results([], OKs, Errors) ->
    e("counter verification failed: "
      "~n      Num OKs:    ~p"
      "~n      Num Errors: ~p", [length(OKs), length(Errors)]),
    error;
verify_counter_results([#conn_data{conn_handle = CH} = CD| CDs], OKs, Errors) ->
    TransId = megaco_config:conn_info(CH, trans_id),
    if
	(TransId =:= 1) ->
	    verify_counter_results(CDs, [CD|OKs], Errors);
	true ->
            e("invalid transaction is for connection: "
              "~n      CD:      ~p"
              "~n      TransId: ~p", [CD, TransId]),
            verify_counter_results(CDs, OKs, [{CD, TransId}|Errors])
    end.


delete_connections([]) ->
    ok;
delete_connections([#conn_data{conn_handle = CH} | CDs]) ->
    {ok, _, _} = megaco_config:disconnect(CH),
    delete_connections(CDs).

    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

otp_7216(suite) ->
    [];
otp_7216(Config) when is_list(Config) ->
    put(tc, otp_7216),
    p("start"),

    LocalMid1 = {deviceName, "local-mid-1"},
    %% LocalMid2 = {deviceName, "local-mid-2"},
    RemoteMid1 = {deviceName, "remote-mid-1"},
    %% RemoteMid2 = {deviceName, "remote-mid-2"},
    RH = #megaco_receive_handle{local_mid       = LocalMid1,
				encoding_mod    = dummy_codec_module,
				encoding_config = [],
				send_mod        = dummy_transport_module},
    MinTransId = 7216,
    MaxTransId = MinTransId + 10,
    User1Config = [{min_trans_id, MinTransId},
		   {max_trans_id, MaxTransId}], 

    VerifySerial = 
	fun(Actual, Expected) ->
		if
		    Actual == Expected ->
			ok;
		    true ->
			throw({error, {invalid_counter_value, Actual}})
		end
	end,

    p("start local user: ~p", [LocalMid1]),
    ok = megaco_config:start_user(LocalMid1, User1Config),

    p("connect"),
    {ok, CD} = megaco_config:connect(RH, RemoteMid1, 
				     dummy_send_handle, self()),
    p("connect ok: CD = ~n~p", [CD]),
    CH = CD#conn_data.conn_handle,

    
    p("*** make the first counter increment ***"),
    {ok, CD01} = megaco_config:incr_trans_id_counter(CH, 1),
    Serial01   = CD01#conn_data.serial,
    p("serial: ~p", [Serial01]),
    VerifySerial(Serial01, MinTransId),
    p("counter increment 1 ok"),    


    p("*** make two more counter increments ***"),
    {ok, _} = megaco_config:incr_trans_id_counter(CH, 1),
    {ok, CD02} = megaco_config:incr_trans_id_counter(CH, 1),
    Serial02   = CD02#conn_data.serial,
    p("serial: ~p", [Serial02]),
    VerifySerial(Serial02, MinTransId+2),
    p("counter increment 2 ok"), 

    
    p("*** make a big counter increment ***"),
    {ok, CD03} = megaco_config:incr_trans_id_counter(CH, 8),
    Serial03   = CD03#conn_data.serial,
    p("serial: ~p", [Serial03]),
    VerifySerial(Serial03, MinTransId+2+8),
    p("counter increment 3 ok"), 

    
    p("*** make a wrap-around counter increment ***"),
    {ok, CD04} = megaco_config:incr_trans_id_counter(CH, 1),
    Serial04   = CD04#conn_data.serial,
    p("serial: ~p", [Serial04]),
    VerifySerial(Serial04, MinTransId),
    p("counter increment 4 ok"), 


    p("*** make a big counter increment ***"),
    {ok, CD05} = megaco_config:incr_trans_id_counter(CH, 10),
    Serial05   = CD05#conn_data.serial,
    p("serial: ~p", [Serial05]),
    VerifySerial(Serial05, MinTransId+10),
    p("counter increment 5 ok"), 

    
    p("*** make a big wrap-around counter increment ***"),
    {ok, CD06} = megaco_config:incr_trans_id_counter(CH, 3),
    Serial06   = CD06#conn_data.serial,
    p("serial: ~p", [Serial06]),
    VerifySerial(Serial06, MinTransId+(3-1)),
    p("counter increment 6 ok"), 


    p("*** make a big counter increment ***"),
    {ok, CD07} = megaco_config:incr_trans_id_counter(CH, 7),
    Serial07   = CD07#conn_data.serial,
    p("serial: ~p", [Serial07]),
    VerifySerial(Serial07, MinTransId+(3-1)+7),
    p("counter increment 7 ok"), 


    p("*** make a big wrap-around counter increment ***"),
    {ok, CD08} = megaco_config:incr_trans_id_counter(CH, 5),
    Serial08   = CD08#conn_data.serial,
    p("serial: ~p", [Serial08]),
    VerifySerial(Serial08, MinTransId+(5-1-1)),
    p("counter increment 8 ok"), 


    p("disconnect"),
    {ok, CD, RCD} = megaco_config:disconnect(CH),
    p("disconnect ok: RCD = ~n~p", [RCD]),

    p("stop user"),
    ok = megaco_config:stop_user(LocalMid1),

    p("stop megaco config process"),
    megaco_config:stop(),

    p("done"),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

otp_8167(suite) ->
    [];
otp_8167(Config) when is_list(Config) ->
    put(tc, otp8167),
    p("start"),

    LocalMid1  = {deviceName, "local-mid-1"},
    LocalMid2  = {deviceName, "local-mid-2"},
    RemoteMid1 = {deviceName, "remote-mid-1"},
    %% RemoteMid2 = {deviceName, "remote-mid-2"},
    RH1 = #megaco_receive_handle{local_mid       = LocalMid1,
				 encoding_mod    = dummy_codec_module,
				 encoding_config = [],
				 send_mod        = dummy_transport_module},
%%     RH2 = #megaco_receive_handle{local_mid       = LocalMid2,
%% 				 encoding_mod    = dummy_codec_module,
%% 				 encoding_config = [],
%% 				 send_mod        = dummy_transport_module},

    User1ConfigA = [{call_proxy_gc_timeout, 1}], 
    User1ConfigB = [{call_proxy_gc_timeout, 0}], 
    User2ConfigA = [{call_proxy_gc_timeout, -1}], 
    User2ConfigB = [{call_proxy_gc_timeout, infinity}], 
    User2ConfigC = [{call_proxy_gc_timeout, "1"}], 
    User2ConfigD = [{call_proxy_gc_timeout, 1.0}], 

    p("start local user (1A): ~p", [LocalMid1]),
    ok = megaco_config:start_user(LocalMid1, User1ConfigA),
    p("stop local user (1A): ~p", [LocalMid1]),
    ok = megaco_config:stop_user(LocalMid1),

    p("start local user (1B): ~p", [LocalMid1]),
    ok = megaco_config:start_user(LocalMid1, User1ConfigB),
    p("try (and fail) change value for item call_proxy_gc_timeout for local user: ~p -> ~p", 
      [LocalMid1, -1]),
    {error, {bad_user_val, LocalMid1, call_proxy_gc_timeout, -1}} = 
	megaco_config:update_user_info(LocalMid1, call_proxy_gc_timeout, -1),
    p("try (and fail) change value for item call_proxy_gc_timeout for local user: ~p -> ~p", 
      [LocalMid1, infinity]),
    {error, {bad_user_val, LocalMid1, call_proxy_gc_timeout, infinity}} = 
	megaco_config:update_user_info(LocalMid1, call_proxy_gc_timeout, infinity),
    p("try (and fail) change value for item call_proxy_gc_timeout for local user: ~p -> ~p", 
      [LocalMid1, "1"]),
    {error, {bad_user_val, LocalMid1, call_proxy_gc_timeout, "1"}} = 
	megaco_config:update_user_info(LocalMid1, call_proxy_gc_timeout, "1"),
    p("try (and fail) change value for item call_proxy_gc_timeout for local user: ~p -> ~p", 
      [LocalMid1, 1.0]),
    {error, {bad_user_val, LocalMid1, call_proxy_gc_timeout, 1.0}} = 
	megaco_config:update_user_info(LocalMid1, call_proxy_gc_timeout, 1.0),
    p("change value for item call_proxy_gc_timeout for local user: ~p", [LocalMid1]),
    ok = megaco_config:update_user_info(LocalMid1, call_proxy_gc_timeout, 10101),
    
    p("connect"),
    {ok, CD} = megaco_config:connect(RH1, RemoteMid1, 
				     dummy_send_handle, self()),
    p("connect ok: CD = ~n~p", [CD]),
    CH = CD#conn_data.conn_handle,

    p("get value for item cancel from connection: ~p", [CH]),
    false = megaco_config:conn_info(CH, cancel),

    p("get value for item cancel from connection data", []),
    false = megaco_config:conn_info(CD, cancel),

    p("get value for item call_proxy_gc_timeout for connection: ~p", [CH]),
    10101 = megaco_config:conn_info(CH, call_proxy_gc_timeout),

    p("change value for item call_proxy_gc_timeout for connection: ~p -> ~p", 
      [CH, 20202]),
    ok = megaco_config:update_conn_info(CH, call_proxy_gc_timeout, 20202),

    p("try (and fail) change value for item call_proxy_gc_timeout for connection: ~p -> ~p", 
      [CH, -1]),
    {error, {bad_user_val, LocalMid1, call_proxy_gc_timeout, -1}} = 
	megaco_config:update_conn_info(CH, call_proxy_gc_timeout, -1),

    p("try (and fail) change value for item call_proxy_gc_timeout for connection: ~p -> ~p", 
      [CH, infinity]),
    {error, {bad_user_val, LocalMid1, call_proxy_gc_timeout, infinity}} = 
	megaco_config:update_conn_info(CH, call_proxy_gc_timeout, infinity),

    p("try (and fail) change value for item call_proxy_gc_timeout for connection: ~p -> ~p", 
      [CH, "1"]),
    {error, {bad_user_val, LocalMid1, call_proxy_gc_timeout, "1"}} = 
	megaco_config:update_conn_info(CH, call_proxy_gc_timeout, "1"),

    p("try (and fail) change value for item call_proxy_gc_timeout for connection: ~p -> ~p", 
      [CH, 1.0]),
    {error, {bad_user_val, LocalMid1, call_proxy_gc_timeout, 1.0}} = 
	megaco_config:update_conn_info(CH, call_proxy_gc_timeout, 1.0),

    p("disconnect: ~p", [CH]),
    {ok, _, _} = megaco_config:disconnect(CH),

    p("stop local user (1B): ~p", [LocalMid1]),
    ok = megaco_config:stop_user(LocalMid1),

    p("try (and fail) start local user (2A): ~p", [LocalMid2]),
    {error, {bad_user_val, LocalMid2, call_proxy_gc_timeout, -1}} = 
	megaco_config:start_user(LocalMid2, User2ConfigA),

    p("try (and fail) start local user (2B): ~p", [LocalMid2]),
    {error, {bad_user_val, LocalMid2, call_proxy_gc_timeout, infinity}} = 
	megaco_config:start_user(LocalMid2, User2ConfigB),

    p("try (and fail) start local user (2C): ~p", [LocalMid2]),
    {error, {bad_user_val, LocalMid2, call_proxy_gc_timeout, "1"}} = 
	megaco_config:start_user(LocalMid2, User2ConfigC),

    p("try (and fail) start local user (2D): ~p", [LocalMid2]),
    {error, {bad_user_val, LocalMid2, call_proxy_gc_timeout, 1.0}} = 
	megaco_config:start_user(LocalMid2, User2ConfigD),

    p("done"),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

otp_8183(suite) ->
    [];
otp_8183(Config) when is_list(Config) ->
    put(tc, otp8183),
    p("start"),

    LocalMid1  = {deviceName, "local-mid-1"},
    LocalMid2  = {deviceName, "local-mid-2"},
    RemoteMid1 = {deviceName, "remote-mid-1"},
%%     RemoteMid2 = {deviceName, "remote-mid-2"},
    RH1 = #megaco_receive_handle{local_mid       = LocalMid1,
				 encoding_mod    = dummy_codec_module,
				 encoding_config = [],
				 send_mod        = dummy_transport_module},
%%     RH2 = #megaco_receive_handle{local_mid       = LocalMid2,
%% 				 encoding_mod    = dummy_codec_module,
%% 				 encoding_config = [],
%% 				 send_mod        = dummy_transport_module},

    OkValA = 100,
    OkValB = 0,
    OkValC = plain,
    OkValD = 10101,
    OkValE = 20202,
    BadValA = -1,
    BadValB = pain,
    BadValC = "1",
    BadValD = 1.0,
    User1ConfigA = [{request_keep_alive_timeout, OkValA}], 
    User1ConfigB = [{request_keep_alive_timeout, OkValB}], 
    User1ConfigC = [{request_keep_alive_timeout, OkValC}], 
    User2ConfigA = [{request_keep_alive_timeout, BadValA}], 
    User2ConfigB = [{request_keep_alive_timeout, BadValB}], 
    User2ConfigC = [{request_keep_alive_timeout, BadValC}], 
    User2ConfigD = [{request_keep_alive_timeout, BadValD}], 

    p("start local user (1A): ~p", [LocalMid1]),
    ok = megaco_config:start_user(LocalMid1, User1ConfigA),
    p("stop local user (1A): ~p", [LocalMid1]),
    ok = megaco_config:stop_user(LocalMid1),

    p("start local user (1B): ~p", [LocalMid1]),
    ok = megaco_config:start_user(LocalMid1, User1ConfigB),
    p("stop local user (1B): ~p", [LocalMid1]),
    ok = megaco_config:stop_user(LocalMid1),

    p("start local user (1C): ~p", [LocalMid1]),
    ok = megaco_config:start_user(LocalMid1, User1ConfigC),

    p("try (and fail) change value for item call_proxy_gc_timeout for local user: ~p -> ~p", 
      [LocalMid1, BadValA]),
    {error, {bad_user_val, LocalMid1, request_keep_alive_timeout, BadValA}} = 
	megaco_config:update_user_info(LocalMid1, request_keep_alive_timeout, BadValA),

    p("try (and fail) change value for item request_keep_alive_timeout for local user: ~p -> ~p", 
      [LocalMid1, BadValB]),
    {error, {bad_user_val, LocalMid1, request_keep_alive_timeout, BadValB}} = 
	megaco_config:update_user_info(LocalMid1, request_keep_alive_timeout, BadValB),

    p("try (and fail) change value for item request_keep_alive_timeout for local user: ~p -> ~p", 
      [LocalMid1, BadValC]),
    {error, {bad_user_val, LocalMid1, request_keep_alive_timeout, BadValC}} = 
	megaco_config:update_user_info(LocalMid1, request_keep_alive_timeout, BadValC),

    p("try (and fail) change value for item request_keep_alive_timeout for local user: ~p -> ~p", 
      [LocalMid1, BadValD]),
    {error, {bad_user_val, LocalMid1, request_keep_alive_timeout, BadValD}} = 
	megaco_config:update_user_info(LocalMid1, request_keep_alive_timeout, BadValD),

    p("change value for item request_keep_alive_timeout for local user: ~p", [LocalMid1]),
    ok = megaco_config:update_user_info(LocalMid1, request_keep_alive_timeout, OkValD),
    
    p("connect"),
    {ok, CD} = megaco_config:connect(RH1, RemoteMid1, 
				     dummy_send_handle, self()),
    p("connect ok: CD = ~n~p", [CD]),
    CH = CD#conn_data.conn_handle,

    p("get value for item request_keep_alive_timeout for connection: ~p", [CH]),
    OkValD = megaco_config:conn_info(CH, request_keep_alive_timeout),

    p("change value for item request_keep_alive_timeout for connection: ~p -> ~p", 
      [CH, OkValE]),
    ok = megaco_config:update_conn_info(CH, request_keep_alive_timeout, OkValE),

    p("try (and fail) change value for item request_keep_alive_timeout for connection: ~p -> ~p", 
      [CH, BadValA]),
    {error, {bad_user_val, LocalMid1, request_keep_alive_timeout, BadValA}} = 
	megaco_config:update_conn_info(CH, request_keep_alive_timeout, BadValA),

    p("try (and fail) change value for item request_keep_alive_timeout for connection: ~p -> ~p", 
      [CH, BadValB]),
    {error, {bad_user_val, LocalMid1, request_keep_alive_timeout, BadValB}} = 
	megaco_config:update_conn_info(CH, request_keep_alive_timeout, BadValB),

    p("try (and fail) change value for item request_keep_alive_timeout for connection: ~p -> ~p", 
      [CH, BadValC]),
    {error, {bad_user_val, LocalMid1, request_keep_alive_timeout, BadValC}} = 
	megaco_config:update_conn_info(CH, request_keep_alive_timeout, BadValC),

    p("try (and fail) change value for item request_keep_alive_timeout for connection: ~p -> ~p", 
      [CH, BadValD]),
    {error, {bad_user_val, LocalMid1, request_keep_alive_timeout, BadValD}} = 
	megaco_config:update_conn_info(CH, request_keep_alive_timeout, BadValD),

    p("disconnect: ~p", [CH]),
    {ok, _, _} = megaco_config:disconnect(CH),

    p("stop local user (1B): ~p", [LocalMid1]),
    ok = megaco_config:stop_user(LocalMid1),

    p("try (and fail) start local user (2A): ~p", [LocalMid2]),
    {error, {bad_user_val, LocalMid2, request_keep_alive_timeout, BadValA}} = 
	megaco_config:start_user(LocalMid2, User2ConfigA),

    p("try (and fail) start local user (2B): ~p", [LocalMid2]),
    {error, {bad_user_val, LocalMid2, request_keep_alive_timeout, BadValB}} = 
	megaco_config:start_user(LocalMid2, User2ConfigB),

    p("try (and fail) start local user (2C): ~p", [LocalMid2]),
    {error, {bad_user_val, LocalMid2, request_keep_alive_timeout, BadValC}} = 
	megaco_config:start_user(LocalMid2, User2ConfigC),

    p("try (and fail) start local user (2D): ~p", [LocalMid2]),
    {error, {bad_user_val, LocalMid2, request_keep_alive_timeout, BadValD}} = 
	megaco_config:start_user(LocalMid2, User2ConfigD),

    p("done"),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

try_tc(TCName, Pre, Case, Post) ->
    try_tc(TCName, "TEST", ?TEST_VERBOSITY, Pre, Case, Post).

try_tc(TCName, Name, Verbosity, Pre, Case, Post) ->
    ?TRY_TC(TCName, Name, Verbosity, Pre, Case, Post).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ts() ->
    erlang:monotonic_time(milli_seconds).

tsd(TSD) ->
    if (TSD < 1000) ->
            ?F("~w ms", [TSD]);
       (TSD < 60000) ->
            ?F("~w secs", [TSD div 1000]);
       true ->
            ?F("~w mins", [TSD div 60000])
    end.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

p(F) ->
    p(F, []).

p(F, A) ->
    case get(tc) of
        undefined ->
            io:format(F ++ "~n", A);
        TC ->
            io:format("[~w] " ++ F ++ "~n", [TC|A])
    end.


i(F) ->
    i(F, []).

i(F, A) ->
    print(info, get(verbosity), get(tc), "INFO", F, A).

e(F, A) ->
    print(info, get(verbosity), get(tc), "ERROR", F, A).

printable(_, debug)   -> true;
printable(info, info) -> true;
printable(_,_)        -> false.

print(Severity, Verbosity, Tc, P, F, A) ->
    print(printable(Severity,Verbosity), Tc, P, F, A).

print(true, Tc, P, F, A) ->
    io:format("*** [~s] ~s ~p ~s:~w ***"
              "~n   " ++ F ++ "~n",
              [?FTS(), P, self(), get(sname), Tc | A]);
print(_, _, _, _, _) ->
    ok.
