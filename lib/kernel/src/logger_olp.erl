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

-include("logger_olp.hrl").
-include("logger_internal.hrl").

%% API
-export([start_link/4, load/2, info/1, reset/1, stop/1, restart/1,
         set_opts/2, get_opts/1, get_default_opts/0, get_pid/1,
         call/2, cast/2, get_ref/0, get_ref/1]).

%% gen_server and proc_lib callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(OPT_KEYS,[sync_mode_qlen,
                  drop_mode_qlen,
                  flush_qlen,
                  burst_limit_enable,
                  burst_limit_max_count,
                  burst_limit_window_time,
                  overload_kill_enable,
                  overload_kill_qlen,
                  overload_kill_mem_size,
                  overload_kill_restart_after]).

-export_type([olp_ref/0, options/0]).

-opaque olp_ref() :: {atom(),pid(),ets:tid()}.

-type options() :: logger:olp_config().

%%%-----------------------------------------------------------------
%%% API

-spec start_link(Name,Module,Args,Options) -> {ok,Pid,Olp} | {error,Reason} when
      Name :: atom(),
      Module :: module(),
      Args :: term(),
      Options :: options(),
      Pid :: pid(),
      Olp :: olp_ref(),
      Reason :: term().
start_link(Name,Module,Args,Options0) when is_map(Options0) ->
    Options = maps:merge(get_default_opts(),Options0),
    case check_opts(Options) of
        ok ->
            proc_lib:start_link(?MODULE,init,[[Name,Module,Args,Options]]);
        Error ->
            Error
    end.

-spec load(Olp, Msg) -> ok when
      Olp :: olp_ref(),
      Msg :: term().
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

-spec info(Olp) -> map() | {error, busy} when
      Olp :: atom() | pid() | olp_ref().
info(Olp) ->
    call(Olp, info).

-spec reset(Olp) -> ok | {error, busy} when
      Olp :: atom() | pid() | olp_ref().
reset(Olp) ->
    call(Olp, reset).

-spec stop(Olp) -> ok when
      Olp :: atom() | pid() | olp_ref().
stop({_Name,Pid,_ModRef}) ->
    stop(Pid);
stop(Pid) ->
    _ = gen_server:call(Pid, stop),
    ok.

-spec set_opts(Olp, Opts) -> ok | {error,term()} | {error, busy} when
      Olp :: atom() | pid() | olp_ref(),
      Opts :: options().
set_opts(Olp, Opts) ->
    call(Olp, {set_opts,Opts}).

-spec get_opts(Olp) -> options() | {error, busy} when
      Olp :: atom() | pid() | olp_ref().
get_opts(Olp) ->
    call(Olp, get_opts).

-spec get_default_opts() -> options().
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

-spec restart(fun(() -> any())) -> ok.
restart(Fun) ->
    Result =
        try Fun()
        catch C:R:S ->
                {error,{restart_failed,Fun,C,R,S}}
        end,
    ?LOG_INTERNAL(debug,[{logger_olp,restart},
                         {result,Result}]),
    ok.

-spec get_ref() -> olp_ref().
get_ref() ->
    get(olp_ref).

-spec get_ref(PidOrName) -> olp_ref() | {error, busy} when
      PidOrName :: pid() | atom().
get_ref(PidOrName) ->
    call(PidOrName,get_ref).

-spec get_pid(olp_ref()) -> pid().
get_pid({_Name,Pid,_ModeRef}) ->
    Pid.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Name,Module,Args,Options]) ->
    register(Name, self()),
    process_flag(message_queue_data, off_heap),

    ?start_observation(Name),

    try ets:new(Name, [public]) of
        ModeRef ->
            OlpRef = {Name,self(),ModeRef},
            put(olp_ref,OlpRef),
            try Module:init(Args) of
                {ok,CBState} ->
                    ?set_mode(ModeRef, async),
                    T0 = ?timestamp(),
                    proc_lib:init_ack({ok,self(),OlpRef}),
                    %% Storing options in state to avoid copying
                    %% (sending) the option data with each message
                    State0 = ?merge_with_stats(
                                Options#{id => Name,
                                         idle=> true,
                                         module => Module,
                                         mode_ref => ModeRef,
                                         mode => async,
                                         last_qlen => 0,
                                         last_load_ts => T0,
                                         burst_win_ts => T0,
                                         burst_msg_count => 0,
                                         cb_state => CBState}),
                    State = reset_restart_flag(State0),
                    gen_server:enter_loop(?MODULE, [], State);
                Error ->
                    _ = ets:delete(ModeRef),
                    unregister(Name),
                    proc_lib:init_ack(Error)
            catch
                _:Error ->
                    _ = ets:delete(ModeRef),
                    unregister(Name),
                    proc_lib:init_ack(Error)
            end
    catch
        _:Error ->
            unregister(Name),
            proc_lib:init_ack(Error)
    end.

%% This is the synchronous load event.
handle_call({'$olp_load', Msg}, _From, State) ->
    {Result,State1} = do_load(Msg, call, State#{idle=>false}),
    %% Result == ok | dropped
    reply_return(Result,State1);

handle_call(get_ref,_From,#{id:=Name,mode_ref:=ModeRef}=State) ->
    reply_return({Name,self(),ModeRef},State);

handle_call({set_opts,Opts0},_From,State) ->
    Opts = maps:merge(maps:with(?OPT_KEYS,State),Opts0),
    case check_opts(Opts) of
        ok ->
            reply_return(ok, maps:merge(State,Opts));
        Error ->
            reply_return(Error, State)
    end;

handle_call(get_opts,_From,State) ->
    reply_return(maps:with(?OPT_KEYS,State), State);

handle_call(info, _From, State) ->
    reply_return(State, State);

handle_call(reset, _From, #{module:=Module,cb_state:=CBState}=State) ->
    State1 = ?merge_with_stats(State),
    CBState1 = try_callback_call(Module,reset_state,[CBState],CBState),
    reply_return(ok, State1#{idle => true,
                             last_qlen => 0,
                             last_load_ts => ?timestamp(),
                             cb_state => CBState1});

handle_call(stop, _From, State) ->
    {stop, {shutdown,stopped}, ok, State};

handle_call(Msg, From, #{module:=Module,cb_state:=CBState}=State) ->
    case try_callback_call(Module,handle_call,[Msg, From, CBState]) of
        {reply,Reply,CBState1} ->
            reply_return(Reply,State#{cb_state=>CBState1});
        {noreply,CBState1} ->
            noreply_return(State#{cb_state=>CBState1});
        {stop, Reason, Reply, CBState1} ->
            {stop, Reason, Reply, State#{cb_state=>CBState1}};
        {stop, Reason, CBState1} ->
            {stop, Reason, State#{cb_state=>CBState1}}
    end.

%% This is the asynchronous load event.
handle_cast({'$olp_load', Msg}, State) ->
    {_Result,State1} = do_load(Msg, cast, State#{idle=>false}),
    noreply_return(State1);

handle_cast(Msg, #{module:=Module, cb_state:=CBState} = State) ->
    case try_callback_call(Module,handle_cast,[Msg, CBState]) of
        {noreply,CBState1} ->
            noreply_return(State#{cb_state=>CBState1});
        {stop, Reason, CBState1} ->
            {stop, Reason, State#{cb_state=>CBState1}}
    end.

handle_info(timeout, #{mode_ref:=_ModeRef, mode:=Mode} = State) ->
    State1 = notify(idle,State),
    State2 = maybe_notify_mode_change(async,State1),
    {noreply, State2#{idle => true,
                      mode => ?change_mode(_ModeRef, Mode, async),
                      burst_msg_count => 0}};
handle_info(Msg, #{module := Module, cb_state := CBState} = State) ->
    case try_callback_call(Module,handle_info,[Msg, CBState]) of
        {noreply,CBState1} ->
            noreply_return(State#{cb_state=>CBState1});
        {stop, Reason, CBState1} ->
            {stop, Reason, State#{cb_state=>CBState1}};
        {load,CBState1} ->
            {_,State1} = do_load(Msg, cast, State#{idle=>false,
                                                   cb_state=>CBState1}),
            noreply_return(State1)
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
            _ = timer:apply_after(RestartAfter,?MODULE,restart,[Fun]),
            ok;
        _ ->
            ok
    end;
terminate(Reason, #{id:=Name, module:=Module, cb_state:=CBState}) ->
    _ = try_callback_call(Module,terminate,[Reason,CBState],ok),
    unregister(Name),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%-----------------------------------------------------------------
%%% Internal functions
-spec call(Olp, term()) -> term() | {error,busy} when
      Olp :: atom() | pid() | olp_ref().
call({_Name, Pid, _ModeRef},Msg) ->
    call(Pid, Msg);
call(Server, Msg) ->
    try
        gen_server:call(Server, Msg)
    catch
        _:{timeout,_} -> {error,busy}
    end.

-spec cast(olp_ref(),term()) -> ok.
cast({_Name, Pid, _ModeRef},Msg) ->
    gen_server:cast(Pid, Msg).

%% check for overload between every event (and set Mode to async,
%% sync or drop accordingly), but never flush the whole mailbox
%% before LogWindowSize events have been handled
do_load(Msg, CallOrCast, State) ->
    T1 = ?timestamp(),
    State1 = ?update_time(T1,State),

    %% check if the process is getting overloaded, or if it's
    %% recovering from overload (the check must be done for each
    %% event to react quickly to large bursts of events and
    %% to ensure that the handler can never end up in drop mode
    %% with an empty mailbox, which would stop operation)
    {Mode1,QLen,Mem,State2} = check_load(State1),

    %% kill the handler if it can't keep up with the load
    kill_if_choked(QLen, Mem, State2),

    if Mode1 == flush ->
            flush(T1, State2);
       true ->
            handle_load(Mode1, T1, Msg, CallOrCast, State2)
    end.

%% this function is called by do_load/3 after an overload check
%% has been performed, where QLen > FlushQLen
flush(T1, State=#{id := _Name, mode := Mode, last_load_ts := _T0, mode_ref := ModeRef}) ->
    %% flush load messages in the mailbox (a limited number in order
    %% to not cause long delays)
    NewFlushed = flush_load(?FLUSH_MAX_N),

    %% write info in log about flushed messages
    State1=notify({flushed,NewFlushed},State),

    %% because of the receive loop when flushing messages, the
    %% handler will be scheduled out often and the mailbox could
    %% grow very large, so we'd better check the queue again here
    {_,QLen1} = process_info(self(), message_queue_len),
    ?observe(_Name,{max_qlen,QLen1}),

    %% Add 1 for the current log event
    ?observe(_Name,{flushed,NewFlushed+1}),

    State2 = ?update_max_time(?diff_time(T1,_T0),State1),
    State3 = ?update_max_qlen(QLen1,State2),
    State4 = maybe_notify_mode_change(async,State3),
    {dropped,?update_other(flushed,FLUSHED,NewFlushed,
                           State4#{mode => ?change_mode(ModeRef,Mode,async),
                                   last_qlen => QLen1,
                                   last_load_ts => T1})}.

%% this function is called to actually handle the message
handle_load(Mode, T1, Msg, _CallOrCast,
      State = #{id := _Name,
                module := Module,
                cb_state := CBState,
                last_qlen := LastQLen,
                last_load_ts := _T0}) ->
    %% check if we need to limit the number of writes
    %% during a burst of log events
    {DoWrite,State1} = limit_burst(State),

    {Result,LastQLen1,CBState1} =
        if DoWrite ->
                ?observe(_Name,{_CallOrCast,1}),
                CBS = try_callback_call(Module,handle_load,[Msg,CBState]),
                {ok,element(2, process_info(self(), message_queue_len)),CBS};
           true ->
                ?observe(_Name,{flushed,1}),
                {dropped,LastQLen,CBState}
        end,
    State2 = State1#{cb_state=>CBState1},

    State3 = State2#{mode => Mode},
    State4 = ?update_calls_or_casts(_CallOrCast,1,State3),
    State5 = ?update_max_qlen(LastQLen1,State4),
    State6 =
        ?update_max_time(?diff_time(T1,_T0),
                         State5#{last_qlen := LastQLen1,
                                 last_load_ts => T1}),
    State7 = case Result of
                 ok ->
                     S = ?update_freq(T1,State6),
                     ?update_other(writes,WRITES,1,S);
                 _ ->
                     State6
             end,
    {Result,State7}.


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
                %% the olp process).
                IncDrops = if Mode == drop -> 0; true -> 1 end,
                {?change_mode(ModeRef, Mode, drop), IncDrops,0};
            QLen >= SyncModeQLen ->
                {?change_mode(ModeRef, Mode, sync), 0,0};
            true ->
                {?change_mode(ModeRef, Mode, async), 0,0}
        end,
    State1 = ?update_other(drops,DROPS,_NewDrops,State),
    State2 = ?update_max_qlen(QLen,State1),
    State3 = ?update_max_mem(Mem,State2),
    State4 = maybe_notify_mode_change(Mode1,State3),
    {Mode1, QLen, Mem,
     ?update_other(flushes,FLUSHES,_NewFlushes,
                   State4#{last_qlen => QLen})}.

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
            flush_load(N+1, Limit);
        {log,_,_,_,_} ->
            flush_load(N+1, Limit);
        {log,_,_,_} ->
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

noreply_return(#{idle:=true}=State) ->
    {noreply,State};
noreply_return(#{idle:=false}=State) ->
    {noreply,State,?IDLE_DETECT_TIME}.

reply_return(Reply,#{idle:=true}=State) ->
    {reply,Reply,State};
reply_return(Reply,#{idle:=false}=State) ->
    {reply,Reply,State,?IDLE_DETECT_TIME}.
