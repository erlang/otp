%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2017-2018. All Rights Reserved.
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
-module(logger_olp).
-behaviour(gen_server).

-include("logger_h_common.hrl").
-include("logger_internal.hrl").

%% API
-export([start_link/4, load/2, info/1, reset/1, stop/1, restart/1,
         set_opts/2, get_opts/1, get_default_opts/0, is_alive/1,
         call/2, cast/2]).

%% gen_server and proc_lib callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%%-----------------------------------------------------------------
%% -define(CONFIG_KEYS,[sync_mode_qlen,
%%                      drop_mode_qlen,
%%                      flush_qlen,
%%                      burst_limit_enable,
%%                      burst_limit_max_count,
%%                      burst_limit_window_time,
%%                      overload_kill_enable,
%%                      overload_kill_qlen,
%%                      overload_kill_mem_size,
%%                      overload_kill_restart_after]).

%%%-----------------------------------------------------------------
%%% API

%-spec start_link(Name,Module,Args,Options) -> {ok,Pid,Olp} | {error,Reason}.
start_link(Name,Module,Args,Options0) when is_map(Options0) ->
    Options = maps:merge(get_default_opts(),Options0),
    case check_opts(Options) of
        ok ->
            case proc_lib:start_link(?MODULE,init,
                                     [[Name,Module,Args,Options]]) of
                {ok,Pid,Olp} ->
                    {ok,Pid,{Olp,Options}};
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

is_alive({_Name,Pid,_ModeRef}) ->
    is_process_alive(Pid).

load({_Name,Pid,ModeRef},Msg) ->
    %% If the process is getting overloaded, the message will be
    %% synchronous instead of asynchronous (slows down the tempo of a
    %% process causing much load). If the process is choked, drop mode
    %% is set and no message is sent.
    try ?get_mode(ModeRef) of
        async ->
            gen_server:cast(Pid, {'$olp_load',Msg});
        sync ->
            case call(Pid, {'$olp_load',Msg}) of
                ok ->
                    ok;
                _Other ->
                    %% dropped or {error,busy}
                    ?observe(_Name,{dropped,1}),
                    ok
            end;
        drop ->
            ?observe(_Name,{dropped,1})
    catch
        %% if the ETS table doesn't exist (maybe because of a
        %% process restart), we can only drop the event
        _:_ -> ?observe(_Name,{dropped,1})
    end,
    ok.    

info(Olp) ->
    call(Olp, info).

reset(Olp) ->
    call(Olp, reset).

stop({_Name,Pid,_ModRef}) ->
    _ = gen_server:call(Pid, stop),
    ok.

set_opts({_Name,Pid,_ModRef}, Opts) ->
    gen_server:call(Pid, {set_opts,Opts}).

get_opts({_Name,Pid,_ModRef}) ->
    gen_server:call(Pid, get_opts).

get_default_opts() ->
    #{sync_mode_qlen              => ?SYNC_MODE_QLEN,
      drop_mode_qlen              => ?DROP_MODE_QLEN,
      flush_qlen                  => ?FLUSH_QLEN,
      burst_limit_enable          => ?BURST_LIMIT_ENABLE,
      burst_limit_max_count       => ?BURST_LIMIT_MAX_COUNT,
      burst_limit_window_time     => ?BURST_LIMIT_WINDOW_TIME,
      overload_kill_enable        => ?OVERLOAD_KILL_ENABLE,
      overload_kill_qlen          => ?OVERLOAD_KILL_QLEN,
      overload_kill_mem_size      => ?OVERLOAD_KILL_MEM_SIZE,
      overload_kill_restart_after => ?OVERLOAD_KILL_RESTART_AFTER}.

restart(Fun) ->
    erlang:display(restarting),
    erlang:display(_ = Fun()),
    ok.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Name,Module,Args,Options]) ->
    register(Name, self()),
    process_flag(message_queue_data, off_heap),

    ?init_test_hooks(),
    ?start_observation(Name),

    try Module:init(Args) of
        {ok,CBState} ->
            try ets:new(Name, [public]) of
                ModeRef ->
                    ?set_mode(ModeRef, async),
                    T0 = ?timestamp(),
                    proc_lib:init_ack({ok,self(),{Name,self(),ModeRef}}),
                    %% Storing options in state to avoid copying
                    %% (sending) the option data with each message
                    State0 = ?merge_with_stats(
                                Options#{id => Name,
                                         module => Module,
                                         mode_ref => ModeRef,
                                         mode => async,
                                         last_qlen => 0,
                                         last_load_ts => T0,
                                         burst_win_ts => T0,
                                         burst_msg_count => 0,
                                         cb_state => CBState}),
                    State = reset_restart_flag(State0),
                    gen_server:enter_loop(?MODULE, [], State)
            catch
                _:Error ->
                    unregister(Name),
                    proc_lib:init_ack(Error)
            end;
        Error ->
            unregister(Name),
            proc_lib:init_ack(Error)
    catch
        _:Error ->
            unregister(Name),
            proc_lib:init_ack(Error)
    end.

%% This is the synchronous load event.
handle_call({'$olp_load', Msg}, _From, State) ->
    {Result,State1} = do_load(Msg, call, State),
    %% Result == ok | dropped
    {reply,Result, State1};

handle_call({set_opts,Opts0},_From,State) ->
    Opts = maps:merge(get_default_opts(),Opts0),
    case check_opts(Opts) of
        ok ->
            {reply, ok, maps:merge(State,Opts)};
        Error ->
            {reply, Error, State}
    end;

handle_call(info, _From, State) ->
    {reply, State, State};

handle_call(reset, _From, #{module:=Module,cb_state:=CBState}=State) ->
    State1 = ?merge_with_stats(State),
    CBState1 = try_callback_call(Module,reset_state,[CBState],CBState),
    {reply, ok, State1#{last_qlen => 0,
                        last_load_ts => ?timestamp(),
                        cb_state => CBState1}};

handle_call(stop, _From, State) ->
    {stop, {shutdown,stopped}, ok, State};

handle_call(Msg, From, #{module:=Module,cb_state:=CBState}=State) ->
    case try_callback_call(Module,handle_call,[Msg, From, CBState]) of
        {reply,Reply,CBState1} ->
            {reply,Reply,State#{cb_state=>CBState1}};
        {reply,Reply,CBState1,Timeout}->
            {reply,Reply,State#{cb_state=>CBState1},Timeout};
        {noreply,CBState1} ->
            {noreply,State#{cb_state=>CBState1}};
        {noreply,CBState1,Timeout} ->
            {noreply,State#{cb_state=>CBState1},Timeout}
    end.

%% This is the asynchronous load event.
handle_cast({'$olp_load', Msg}, State) ->
    {_Result,State1} = do_load(Msg, cast, State),
    %% Result == ok | dropped
    {noreply,State1};

handle_cast(Msg, #{module:=Module, cb_state:=CBState} = State) ->
    case try_callback_call(Module,handle_cast,[Msg, CBState]) of
        {noreply,CBState1} ->
            {noreply,State#{cb_state=>CBState1}};
        {noreply,CBState1,Timeout} ->
            {noreply,State#{cb_state=>CBState1},Timeout}
    end.

handle_info(Msg, #{module := Module, cb_state := CBState} = State) ->
    case try_callback_call(Module,handle_info,[Msg, CBState]) of
        {noreply,CBState1} ->
            {noreply,State#{cb_state=>CBState1}};
        {noreply,CBState1,Timeout} ->
            {noreply,State#{cb_state=>CBState1},Timeout}
    end.

terminate({shutdown,{overloaded,_QLen,_Mem}},
          #{id:=Name, module := Module, cb_state := CBState,
            overload_kill_restart_after := RestartAfter} = State) ->
    %% We're terminating because of an overload situation (see
    %% kill_if_choked/3).
    unregister(Name), %%!!!! to avoid error printout of callback crashed on stop
    case try_callback_call(Module,terminate,[overloaded,CBState],ok) of
        {ok,Fun} when is_function(Fun,0), is_integer(RestartAfter) ->
            set_restart_flag(State),
            timer:apply_after(RestartAfter,?MODULE,restart,[Fun]),
            ok;
        _ ->
            ok
    end,
    ok;
terminate(Reason, #{id:=Name, module:=Module, cb_state:=CBState}) ->
    _ = try_callback_call(Module,terminate,[Reason,CBState],ok),
    unregister(Name),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%-----------------------------------------------------------------
%%% Internal functions
call({_Name, Pid, _ModeRef},Msg) ->
    call(Pid, Msg);
call(Server, Msg) ->
    try
        gen_server:call(Server, Msg, ?DEFAULT_CALL_TIMEOUT)
    catch
        _:{timeout,_} -> {error,busy}
    end.

cast({_Name, Pid, _ModeRef},Msg) ->
    gen_server:cast(Pid, Msg).

%% check for overload between every event (and set Mode to async,
%% sync or drop accordingly), but never flush the whole mailbox
%% before LogWindowSize events have been handled
do_load(Msg, CallOrCast, State) ->
    T1 = ?timestamp(),

    %% check if the process is getting overloaded, or if it's
    %% recovering from overload (the check must be done for each
    %% event to react quickly to large bursts of events and
    %% to ensure that the handler can never end up in drop mode
    %% with an empty mailbox, which would stop operation)
    {Mode1,QLen,Mem,State1} = check_load(State),

    %% kill the handler if it can't keep up with the load
    kill_if_choked(QLen, Mem, State1),

    if Mode1 == flush ->
            flush(T1, State1);
       true ->
            handle_load(Mode1, T1, Msg, CallOrCast, State1)
    end.

%% this function is called by do_load/3 after an overload check
%% has been performed, where QLen > FlushQLen
flush(T1, State=#{id := _Name, last_load_ts := T0, mode_ref := ModeRef}) ->
    %% flush load messages in the mailbox (a limited number in order
    %% to not cause long delays)
    NewFlushed = flush_load(?FLUSH_MAX_N),

    %% write info in log about flushed messages
    State1=notify({flushed,NewFlushed},State),

    %% because of the receive loop when flushing messages, the
    %% handler will be scheduled out often and the mailbox could
    %% grow very large, so we'd better check the queue again here
    {_,_QLen1} = process_info(self(), message_queue_len),
    ?observe(_Name,{max_qlen,_QLen1}),

    %% Add 1 for the current log event
    ?observe(_Name,{flushed,NewFlushed+1}),

    State2 = ?update_max_time(?diff_time(T1,T0),State1),
    State3 = ?update_max_qlen(_QLen1,State2),
    {dropped,?update_other(flushed,FLUSHED,NewFlushed,
                           State3#{mode => ?set_mode(ModeRef,async),
                                   last_qlen => 0,
                                   last_load_ts => T1})}.

%% this function is called to actually handle the message
handle_load(Mode, T1, Msg, _CallOrCast,
      State = #{id := _Name,
                module := Module,
                cb_state := CBState,
                mode_ref := ModeRef,
                last_qlen := LastQLen,
                last_load_ts := T0}) ->
    %% check if we need to limit the number of writes
    %% during a burst of log events
    {DoWrite,State1} = limit_burst(State),

    {Result,LastQLen1,CBState1} =
        if DoWrite ->
                ?observe(_Name,{_CallOrCast,1}),
                {ok,CBS} = try_callback_call(Module,handle_load,[Msg,CBState]),
                {ok,element(2, process_info(self(), message_queue_len)),CBS};
           true ->
                ?observe(_Name,{flushed,1}),
                {dropped,LastQLen,CBState}
        end,
    State2 = State1#{cb_state=>CBState1},

    %% Check if the time since the previous load message is long
    %% enough - and the queue length small enough - to assume the
    %% mailbox has been emptied, and if so, reset mode to async. Note
    %% that this is the best we can do to detect an idle handler
    %% without setting a timer after each log call/cast. If the time
    %% between two consecutive log events is fast and no new event
    %% comes in after the last one, idle state won't be detected!
    Time = ?diff_time(T1,T0),
    State3 =
        if (LastQLen1 < ?FILESYNC_OK_QLEN) andalso
           (Time > ?IDLE_DETECT_TIME_USEC) ->
                S = notify(idle,State2),
                S#{mode => ?change_mode(ModeRef, Mode, async),
                   burst_msg_count => 0};
           true ->
                State2#{mode => Mode}
        end,
    State4 = ?update_calls_or_casts(_CallOrCast,1,State3),
    State5 = ?update_max_qlen(LastQLen1,State4),
    State6 =
        ?update_max_time(Time,
                         State5#{last_qlen := LastQLen1,
                                 last_load_ts => T1}),
    {Result,State6}.


%%%-----------------------------------------------------------------
%%% Check that the options are valid
check_opts(Options) when is_map(Options) ->
    case do_check_opts(maps:to_list(Options)) of
        ok ->
            case overload_levels_ok(Options) of
                true ->
                    ok;
                false ->
                    Faulty = maps:with([sync_mode_qlen,
                                        drop_mode_qlen,
                                        flush_qlen],Options),
                    {error,{invalid_olp_levels,Faulty}}
            end;
        {error,Key,Value} ->
            {error,{invalid_olp_config,#{Key=>Value}}}
    end.

do_check_opts([{sync_mode_qlen,N}|Options]) when is_integer(N) ->
    do_check_opts(Options);
do_check_opts([{drop_mode_qlen,N}|Options]) when is_integer(N) ->
    do_check_opts(Options);
do_check_opts([{flush_qlen,N}|Options]) when is_integer(N) ->
    do_check_opts(Options);
do_check_opts([{burst_limit_enable,Bool}|Options]) when is_boolean(Bool) ->
    do_check_opts(Options);
do_check_opts([{burst_limit_max_count,N}|Options]) when is_integer(N) ->
    do_check_opts(Options);
do_check_opts([{burst_limit_window_time,N}|Options]) when is_integer(N) ->
    do_check_opts(Options);
do_check_opts([{overload_kill_enable,Bool}|Options]) when is_boolean(Bool) ->
    do_check_opts(Options);
do_check_opts([{overload_kill_qlen,N}|Options]) when is_integer(N) ->
    do_check_opts(Options);
do_check_opts([{overload_kill_mem_size,N}|Options]) when is_integer(N) ->
    do_check_opts(Options);
do_check_opts([{overload_kill_restart_after,NorA}|Options])
  when is_integer(NorA); NorA == infinity ->
    do_check_opts(Options);
do_check_opts([{Key,Value}|_]) ->
    {error,Key,Value};
do_check_opts([]) ->
    ok.

set_restart_flag(#{id := Name, module := Module}) ->
    Flag = list_to_atom(lists:concat([Module,"_",Name,"_restarting"])),
    spawn(fun() ->
                  register(Flag, self()),
                  timer:sleep(infinity)
          end),
    ok.

reset_restart_flag(#{id := Name, module := Module} = State) ->
    Flag = list_to_atom(lists:concat([Module,"_",Name,"_restarting"])),
    case whereis(Flag) of
        undefined ->
            State;
        Pid ->
            exit(Pid, kill),
            notify(restart,State)
    end.

check_load(State = #{id:=_Name, mode_ref := ModeRef, mode := Mode,
                     sync_mode_qlen := SyncModeQLen,
                     drop_mode_qlen := DropModeQLen,
                     flush_qlen := FlushQLen}) ->
    {_,Mem} = process_info(self(), memory),
    ?observe(_Name,{max_mem,Mem}),
    {_,QLen} = process_info(self(), message_queue_len),
    ?observe(_Name,{max_qlen,QLen}),
    %% When the handler process gets scheduled in, it's impossible
    %% to predict the QLen. We could jump "up" arbitrarily from say
    %% async to sync, async to drop, sync to flush, etc. However, when
    %% the handler process manages the log events (without flushing),
    %% one after the other, we will move "down" from drop to sync and
    %% from sync to async. This way we don't risk getting stuck in
    %% drop or sync mode with an empty mailbox.
    {Mode1,_NewDrops,_NewFlushes} =
        if
            QLen >= FlushQLen ->
                {flush, 0,1};
            QLen >= DropModeQLen ->
                %% Note that drop mode will force load messages to
                %% be dropped on the client side (never sent to
                %% the handler).
                IncDrops = if Mode == drop -> 0; true -> 1 end,
                {?change_mode(ModeRef, Mode, drop), IncDrops,0};
            QLen >= SyncModeQLen ->
                {?change_mode(ModeRef, Mode, sync), 0,0};
            true ->
                {?change_mode(ModeRef, Mode, async), 0,0}
        end,
    State1 = ?update_other(drops,DROPS,_NewDrops,State),
    State2 = maybe_notify_mode_change(Mode1,State1),
    {Mode1, QLen, Mem,
     ?update_other(flushes,FLUSHES,_NewFlushes,
                   State2#{last_qlen => QLen})}.

limit_burst(#{burst_limit_enable := false}=State) ->
     {true,State};
limit_burst(#{burst_win_ts := BurstWinT0,
              burst_msg_count := BurstMsgCount,
              burst_limit_window_time := BurstLimitWinTime,
              burst_limit_max_count := BurstLimitMaxCnt} = State) ->
    if (BurstMsgCount >= BurstLimitMaxCnt) -> 
            %% the limit for allowed messages has been reached
            BurstWinT1 = ?timestamp(),
            case ?diff_time(BurstWinT1,BurstWinT0) of
                BurstCheckTime when BurstCheckTime < (BurstLimitWinTime*1000) ->
                    %% we're still within the burst time frame
                    {false,?update_other(burst_drops,BURSTS,1,State)};
                _BurstCheckTime ->
                    %% burst time frame passed, reset counters
                    {true,State#{burst_win_ts => BurstWinT1,
                                 burst_msg_count => 0}}
            end;
       true ->
            %% the limit for allowed messages not yet reached
            {true,State#{burst_win_ts => BurstWinT0,
                         burst_msg_count => BurstMsgCount+1}}
    end.

kill_if_choked(QLen, Mem, #{overload_kill_enable   := KillIfOL,
                            overload_kill_qlen     := OLKillQLen,
                            overload_kill_mem_size := OLKillMem}) ->
    if KillIfOL andalso
       ((QLen > OLKillQLen) orelse (Mem > OLKillMem)) ->
            exit({shutdown,{overloaded,QLen,Mem}});
       true ->
            ok
    end.

flush_load(Limit) ->
    process_flag(priority, high),
    Flushed = flush_load(0, Limit),
    process_flag(priority, normal),
    Flushed.

flush_load(Limit, Limit) ->
    Limit;
flush_load(N, Limit) ->
    %% flush log events but leave other events, such as info, reset
    %% and stop, so that these have a chance to be processed even
    %% under heavy load
    receive
        {'$gen_cast',{'$olp_load',_}} ->
            flush_load(N+1, Limit);
        {'$gen_call',{Pid,MRef},{'$olp_load',_}} ->
            Pid ! {MRef, dropped},
            flush_load(N+1, Limit)
    after
        0 -> N
    end.

overload_levels_ok(Options) ->
    SMQL = maps:get(sync_mode_qlen, Options, ?SYNC_MODE_QLEN),
    DMQL = maps:get(drop_mode_qlen, Options, ?DROP_MODE_QLEN),
    FQL = maps:get(flush_qlen, Options, ?FLUSH_QLEN),
    (DMQL > 1) andalso (SMQL =< DMQL) andalso (DMQL =< FQL).

maybe_notify_mode_change(drop,#{mode:=Mode0}=State)
  when Mode0=/=drop ->
    notify({mode_change,Mode0,drop},State);
maybe_notify_mode_change(Mode1,#{mode:=drop}=State)
  when Mode1==async; Mode1==sync ->
    notify({mode_change,drop,Mode1},State);
maybe_notify_mode_change(_,State) ->
    State.

notify(Note,#{module:=Module,cb_state:=CBState}=State) ->
    CBState1 = try_callback_call(Module,notify,[Note,CBState],CBState),
    State#{cb_state=>CBState1}.

try_callback_call(Module, Function, Args) ->
    try_callback_call(Module, Function, Args, '$no_default_return').

try_callback_call(Module, Function, Args, DefRet) ->
    try apply(Module, Function, Args)
    catch
        throw:R -> R;
        error:undef:S when DefRet=/='$no_default_return' ->
            case S of
                [{Module,Function,Args,_}|_] ->
                    DefRet;
                _ ->
                    erlang:raise(error,undef,S)
            end
    end.
