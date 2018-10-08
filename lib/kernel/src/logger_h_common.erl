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
-module(logger_h_common).
-behaviour(gen_server).

-include("logger_h_common.hrl").
-include("logger_internal.hrl").

%% API
-export([start_link/3, info/2, filesync/2, reset/2]).

%% gen_server and proc_lib callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% logger callbacks
-export([log/2, adding_handler/1, removing_handler/1, changing_config/3,
         filter_config/1]).

%% Library functions for handlers
-export([error_notify/1]).

%%%-----------------------------------------------------------------
-define(CONFIG_KEYS,[sync_mode_qlen,
                     drop_mode_qlen,
                     flush_qlen,
                     burst_limit_enable,
                     burst_limit_max_count,
                     burst_limit_window_time,
                     overload_kill_enable,
                     overload_kill_qlen,
                     overload_kill_mem_size,
                     overload_kill_restart_after,
                     filesync_repeat_interval]).
-define(READ_ONLY_KEYS,[handler_pid,mode_tab]).

%%%-----------------------------------------------------------------
%%%
filesync(Module, Name) when is_atom(Name) ->
    try
        gen_server:call(?name_to_reg_name(Module,Name),
                        filesync, ?DEFAULT_CALL_TIMEOUT)
    catch
        _:{timeout,_} -> {error,handler_busy}
    end;
filesync(_, Name) ->
    {error,{badarg,{filesync,[Name]}}}.

info(Module, Name) when is_atom(Name) ->
    try
        gen_server:call(?name_to_reg_name(Module,Name),
                        info, ?DEFAULT_CALL_TIMEOUT)
    catch
        _:{timeout,_} -> {error,handler_busy}
    end;
info(_, Name) ->
    {error,{badarg,{info,[Name]}}}.

reset(Module, Name) when is_atom(Name) ->
    try
        gen_server:call(?name_to_reg_name(Module,Name),
                        reset, ?DEFAULT_CALL_TIMEOUT)
    catch
        _:{timeout,_} -> {error,handler_busy}
    end;
reset(_, Name) ->
    {error,{badarg,{reset,[Name]}}}.



%%%-----------------------------------------------------------------
%%% Handler being added
adding_handler(#{id:=Name,module:=Module}=Config) ->
    HConfig = maps:get(config, Config, #{}),
    HandlerConfig0 = maps:without(?CONFIG_KEYS,HConfig),
    case Module:check_config(Name,set,undefined,HandlerConfig0) of
        {ok,HandlerConfig} ->
            ModifiedCommon = maps:with(?CONFIG_KEYS,HandlerConfig),
            CommonConfig0 = maps:with(?CONFIG_KEYS,HConfig),
            CommonConfig = maps:merge(
                             maps:merge(get_default_config(), CommonConfig0),
                             ModifiedCommon),
            case check_config(CommonConfig) of
                ok ->
                    State = maps:merge(get_init_state(), CommonConfig),
                    HConfig1 = maps:merge(CommonConfig,HandlerConfig),
                    Config1 = Config#{config=>HConfig1},
                    case overload_levels_ok(State) of
                        true ->
                            start(Name, Config1, State);
                        false ->
                            #{sync_mode_qlen := SMQL,
                              drop_mode_qlen := DMQL,
                              flush_qlen     := FQL} = State,
                            {error,{invalid_levels,{SMQL,DMQL,FQL}}}
                    end;
                {error,Faulty} ->
                    {error,{invalid_config,Module,Faulty}}
            end;
        Error ->
            Error
    end.

%%%-----------------------------------------------------------------
%%% Handler being removed
removing_handler(#{id:=Name, module:=Module}) ->
    case whereis(?name_to_reg_name(Module,Name)) of
        undefined ->
            ok;
        Pid ->
            %% We don't want to do supervisor:terminate_child here
            %% since we need to distinguish this explicit stop from a
            %% system termination in order to avoid circular attempts
            %% at removing the handler (implying deadlocks and
            %% timeouts).
            %% And we don't need to do supervisor:delete_child, since
            %% the restart type is temporary, which means that the
            %% child specification is automatically removed from the
            %% supervisor when the process dies.
            _ = gen_server:call(Pid, stop),
            ok
    end.

%%%-----------------------------------------------------------------
%%% Updating handler config
changing_config(SetOrUpdate,
                OldConfig=#{id:=Name,config:=OldHConfig,module:=Module},
                NewConfig0) ->
    NewHConfig0 = maps:get(config, NewConfig0, #{}),
    OldHandlerConfig = maps:without(?CONFIG_KEYS++?READ_ONLY_KEYS,OldHConfig),
    NewHandlerConfig0 = maps:without(?CONFIG_KEYS++?READ_ONLY_KEYS,NewHConfig0),
    case Module:check_config(Name, SetOrUpdate,
                             OldHandlerConfig,NewHandlerConfig0) of
        {ok, NewHandlerConfig} ->
            ModifiedCommon = maps:with(?CONFIG_KEYS,NewHandlerConfig),
            NewCommonConfig0 = maps:with(?CONFIG_KEYS,NewHConfig0),
            CommonDefault =
                case SetOrUpdate of
                    set ->
                        get_default_config();
                    update ->
                        maps:with(?CONFIG_KEYS,OldHConfig)
                end,
            NewCommonConfig = maps:merge(
                                maps:merge(CommonDefault,NewCommonConfig0),
                                ModifiedCommon),
            case check_config(NewCommonConfig) of
                ok ->
                    ReadOnly = maps:with(?READ_ONLY_KEYS,OldHConfig),
                    NewHConfig = maps:merge(
                                   maps:merge(NewCommonConfig,NewHandlerConfig),
                                   ReadOnly),
                    NewConfig = NewConfig0#{config=>NewHConfig},
                    HPid = maps:get(handler_pid,OldHConfig),
                    try gen_server:call(HPid, {change_config,OldConfig,NewConfig},
                                        ?DEFAULT_CALL_TIMEOUT) of
                        ok      -> {ok,NewConfig};
                        Error  -> Error
                    catch
                        _:{timeout,_} -> {error,handler_busy}
                    end;
                {error,Faulty} ->
                    {error,{invalid_config,Module,Faulty}}
            end;
        Error ->
            Error
    end.

%%%-----------------------------------------------------------------
%%% Log a string or report
-spec log(LogEvent, Config) -> ok when
      LogEvent :: logger:log_event(),
      Config :: logger:handler_config().

log(LogEvent, Config = #{id := Name,
                         config := #{handler_pid := HPid,
                                     mode_tab := ModeTab}}) ->
    %% if the handler has crashed, we must drop this event
    %% and hope the handler restarts so we can try again
    true = is_process_alive(HPid),
    Bin = log_to_binary(LogEvent, Config),
    call_cast_or_drop(Name, HPid, ModeTab, Bin).

%%%-----------------------------------------------------------------
%%% Remove internal fields from configuration
filter_config(#{config:=HConfig}=Config) ->
    Config#{config=>maps:without(?READ_ONLY_KEYS,HConfig)}.

%%%-----------------------------------------------------------------
%%% Add a standard handler to the logger.
%%% This starts a dedicated handler process which should always
%%% exist if the handler is registered with logger (and should not
%%% exist if the handler is not registered).
%%%
%%% Handler specific config should be provided with a sub map associated
%%% with a key named 'config', e.g:
%%%
%%% Config = #{config => #{sync_mode_qlen => 50}
%%%
%%% The standard handler process is linked to logger_sup, which is
%%% part of the kernel application's supervision tree.
start(Name, Config, HandlerState) ->
    ChildSpec =
        #{id       => Name,
          start    => {?MODULE, start_link, [Name,Config,HandlerState]},
          restart  => temporary,
          shutdown => 2000,
          type     => worker,
          modules  => [?MODULE]},
    case supervisor:start_child(logger_sup, ChildSpec) of
        {ok,Pid,Config1} ->
            ok = logger_handler_watcher:register_handler(Name,Pid),
            {ok,Config1};
        Error ->
            Error
    end.

%%%-----------------------------------------------------------------
%%% Start a standard handler process and link to caller.
%%% This function is called by the kernel supervisor when this
%%% handler process gets added
-spec start_link(Name, Config, HandlerState) -> {ok,Pid} | {error,Reason} when
      Name :: atom(),
      Config :: logger:handler_config(),
      HandlerState :: map(),
      Pid :: pid(),
      Reason :: term().

start_link(Name, Config, HandlerState) ->
    proc_lib:start_link(?MODULE,init,[[Name,Config,HandlerState]]).

%%%-----------------------------------------------------------------
%%%
get_init_state() ->
     #{ctrl_sync_int               => ?CONTROLLER_SYNC_INTERVAL,
       filesync_ok_qlen            => ?FILESYNC_OK_QLEN}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Name, Config = #{config := HConfig, module := Module},
      State = #{filesync_repeat_interval := FSyncInt,
                ctrl_sync_int := CtrlSyncInt}]) ->
    RegName = ?name_to_reg_name(Module,Name),
    register(RegName, self()),
    process_flag(trap_exit, true),
    process_flag(message_queue_data, off_heap),

    ?init_test_hooks(),
    ?start_observation(Name),

    case Module:init(Name, HConfig) of
        {ok,HState} ->
            try ets:new(Name, [public]) of
                ModeTab ->
                    ?set_mode(ModeTab, async),
                    T0 = ?timestamp(),
                    State1 =
                        ?merge_with_stats(State#{id => Name,
                                                 module => Module,
                                                 mode_tab => ModeTab,
                                                 mode => async,
                                                 ctrl_sync_count => CtrlSyncInt,
                                                 last_qlen => 0,
                                                 last_log_ts => T0,
                                                 last_op => sync,
                                                 burst_win_ts => T0,
                                                 burst_msg_count => 0,
                                                 handler_state => HState}),
                    Config1 =
                        Config#{config => HConfig#{handler_pid => self(),
                                                   mode_tab => ModeTab}},
                    proc_lib:init_ack({ok,self(),Config1}),
                    if is_integer(FSyncInt) ->
                            gen_server:cast(self(), repeated_filesync);
                       true ->
                            ok
                    end,
                    case unset_restart_flag(Name, Module) of
                        true ->
                            %% inform about restart
                            gen_server:cast(self(), {log_handler_info,
                                                     "Handler ~p restarted",
                                                     [Name]});
                        false ->
                            %% initial start
                            ok
                    end,
                    gen_server:enter_loop(?MODULE, [], State1)
            catch
                _:Error ->
                    unregister(RegName),
                    error_notify({init_handler,Name,Error}),
                    proc_lib:init_ack(Error)
            end;
        Error ->
            unregister(RegName),
            error_notify({init_handler,Name,Error}),
            proc_lib:init_ack(Error)
    end.

%% This is the synchronous log event.
handle_call({log, Bin}, _From, State) ->
    {Result,State1} = do_log(Bin, call, State),
    %% Result == ok | dropped
    {reply,Result, State1};

handle_call(filesync, _From, State = #{id := Name,
                                       module := Module,
                                       handler_state := HandlerState}) ->
    {Result,HandlerState1} = Module:sync_filesync(Name,HandlerState),
    {reply, Result, State#{handler_state=>HandlerState1, last_op=>sync}};

handle_call({change_config,_OldConfig,NewConfig}, _From,
            State = #{filesync_repeat_interval := FSyncInt0}) ->
    HConfig = maps:get(config, NewConfig, #{}),
    State1 = maps:merge(State, HConfig),
    case overload_levels_ok(State1) of
        true ->
            _ =
                case maps:get(filesync_repeat_interval, HConfig, undefined) of
                    undefined ->
                        ok;
                    no_repeat ->
                        _ = logger_h_common:cancel_timer(maps:get(rep_sync_tref,
                                                                  State,
                                                                  undefined));
                    FSyncInt0 ->
                        ok;
                    _FSyncInt1 ->
                        _ = logger_h_common:cancel_timer(maps:get(rep_sync_tref,
                                                                  State,
                                                                  undefined)),
                        gen_server:cast(self(), repeated_filesync)
                end,
            {reply, ok, State1};
        false ->
            #{sync_mode_qlen := SMQL,
              drop_mode_qlen := DMQL,
              flush_qlen     := FQL} = State1,
            {reply, {error,{invalid_levels,{SMQL,DMQL,FQL}}}, State}
    end;

handle_call(info, _From, State) ->
    {reply, State, State};

handle_call(reset, _From, #{module:=Module,handler_state:=HandlerState}=State) ->
    State1 = ?merge_with_stats(State),
    {reply, ok, State1#{last_qlen => 0,
                        last_log_ts => ?timestamp(),
                        handler_state => Module:reset_state(HandlerState)}};

handle_call(stop, _From, State) ->
    {stop, {shutdown,stopped}, ok, State}.

%% This is the asynchronous log event.
handle_cast({log, Bin}, State) ->
    {_,State1} = do_log(Bin, cast, State),
    {noreply, State1};

handle_cast({log_handler_info, Format, Args}, State = #{id:=Name}) ->
    log_handler_info(Name, Format, Args, State),
    {noreply, State};

%% If FILESYNC_REPEAT_INTERVAL is set to a millisec value, this
%% clause gets called repeatedly by the handler. In order to
%% guarantee that a filesync *always* happens after the last log
%% event, the repeat operation must be active!
handle_cast(repeated_filesync,State = #{filesync_repeat_interval := no_repeat}) ->
    {noreply,State};
handle_cast(repeated_filesync,
            State = #{id := Name,
                      module := Module,
                      handler_state := HandlerState,
                      filesync_repeat_interval := FSyncInt,
                      last_op := LastOp}) ->
    HandlerState1 =
        if LastOp == sync ->
                HandlerState;
           true ->
                Module:async_filesync(Name,HandlerState)
        end,
    {ok,TRef} = timer:apply_after(FSyncInt, gen_server,cast,
                                  [self(),repeated_filesync]),
    {noreply,State#{handler_state=>HandlerState1,
                    rep_sync_tref => TRef,
                    last_op => sync}}.

handle_info(Info, #{module := Module, handler_state := HandlerState} = State) ->
    {noreply,State#{handler_state => Module:handle_info(Info,HandlerState)}}.

terminate(Reason, State = #{id := Name,
                            module := Module,
                            handler_state := HandlerState}) ->
    _ = cancel_timer(maps:get(rep_sync_tref, State, undefined)),
    _ = Module:terminate(Name, Reason, HandlerState),
    ok = stop_or_restart(Name, Reason, State),
    unregister(?name_to_reg_name(Module, Name)),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% check for overload between every event (and set Mode to async,
%% sync or drop accordingly), but never flush the whole mailbox
%% before LogWindowSize events have been handled
do_log(Bin, CallOrCast, State = #{id:=Name, module:=Module, mode:=Mode0}) ->
    T1 = ?timestamp(),

    %% check if the handler is getting overloaded, or if it's
    %% recovering from overload (the check must be done for each
    %% event to react quickly to large bursts of events and
    %% to ensure that the handler can never end up in drop mode
    %% with an empty mailbox, which would stop operation)
    {Mode1,QLen,Mem,State1} = check_load(State),

    if (Mode1 == drop) andalso (Mode0 =/= drop) ->
            log_handler_info(Name, "Handler ~p switched to drop mode",
                             [Name], State);
       (Mode0 == drop) andalso ((Mode1 == async) orelse (Mode1 == sync)) ->
            log_handler_info(Name, "Handler ~p switched to ~w mode",
                             [Name,Mode1], State);
       true ->
            ok
    end,

    %% kill the handler if it can't keep up with the load
    kill_if_choked(Name, Module, QLen, Mem, State),

    if Mode1 == flush ->
            flush(Name, QLen, T1, State1);
       true ->
            write(Name, Mode1, T1, Bin, CallOrCast, State1)
    end.

%% this clause is called by do_log/3 after an overload check
%% has been performed, where QLen > FlushQLen
flush(Name, _QLen0, T1, State=#{last_log_ts := _T0, mode_tab := ModeTab}) ->
    %% flush messages in the mailbox (a limited number in
    %% order to not cause long delays)
    NewFlushed = flush_log_events(?FLUSH_MAX_N),

    %% write info in log about flushed messages
    log_handler_info(Name, "Handler ~p flushed ~w log events",
                     [Name,NewFlushed], State),

    %% because of the receive loop when flushing messages, the
    %% handler will be scheduled out often and the mailbox could
    %% grow very large, so we'd better check the queue again here
    {_,_QLen1} = process_info(self(), message_queue_len),
    ?observe(Name,{max_qlen,_QLen1}),

    %% Add 1 for the current log event
    ?observe(Name,{flushed,NewFlushed+1}),

    State1 = ?update_max_time(?diff_time(T1,_T0),State),
    {dropped,?update_other(flushed,FLUSHED,NewFlushed,
                           State1#{mode => ?set_mode(ModeTab,async),
                                   last_qlen => 0,
                                   last_log_ts => T1})}.

%% this clause is called to write to file
write(Name, Mode, T1, Bin, _CallOrCast,
      State = #{module := Module,
                handler_state := HandlerState,
                mode_tab := ModeTab,
                ctrl_sync_count := CtrlSync,
                ctrl_sync_int := CtrlSyncInt,
                last_qlen := LastQLen,
                last_log_ts := T0}) ->
    %% check if we need to limit the number of writes
    %% during a burst of log events
    {DoWrite,BurstWinT,BurstMsgCount} = limit_burst(State),

    %% only log synhrounously every CtrlSyncInt time, to give the
    %% handler time between writes so it can keep up with incoming
    %% messages
    {Result,LastQLen1,HandlerState1} =
        if DoWrite, CtrlSync == 0 ->
                ?observe(Name,{_CallOrCast,1}),
                {_,HS1} = Module:sync_write(Name, Bin, HandlerState),
                {ok,element(2, process_info(self(), message_queue_len)),HS1};
           DoWrite ->
                ?observe(Name,{_CallOrCast,1}),
                HS1 = Module:async_write(Name, Bin, HandlerState),
                {ok,LastQLen,HS1};
           not DoWrite ->
                ?observe(Name,{flushed,1}),
                {dropped,LastQLen,HandlerState}
        end,

    %% Check if the time since the previous log event is long enough -
    %% and the queue length small enough - to assume the mailbox has
    %% been emptied, and if so, do filesync operation and reset mode to
    %% async. Note that this is the best we can do to detect an idle
    %% handler without setting a timer after each log call/cast. If the
    %% time between two consecutive log events is fast and no new
    %% event comes in after the last one, idle state won't be detected!
    Time = ?diff_time(T1,T0),
    {Mode1,BurstMsgCount1,HandlerState2} =
        if (LastQLen1 < ?FILESYNC_OK_QLEN) andalso
           (Time > ?IDLE_DETECT_TIME_USEC) ->
                HS2 = Module:async_filesync(Name,HandlerState),
                {?change_mode(ModeTab, Mode, async),0,HS2};
           true ->
                {Mode,BurstMsgCount,HandlerState1}
        end,
    State1 =
        ?update_calls_or_casts(_CallOrCast,1,State),
    State2 =
        ?update_max_time(Time,
                         State1#{mode => Mode1,
                                 last_qlen := LastQLen1,
                                 last_log_ts => T1,
                                 last_op => write,
                                 burst_win_ts => BurstWinT,
                                 burst_msg_count => BurstMsgCount1,
                                 ctrl_sync_count => if CtrlSync==0 -> CtrlSyncInt;
                                                       true -> CtrlSync-1
                                                    end,
                                 handler_state => HandlerState2}),
    {Result,State2}.

log_handler_info(Name, Format, Args, #{module:=Module,
                                       handler_state:=HandlerState}) ->
    Config =
        case logger:get_handler_config(Name) of
            {ok,Conf} -> Conf;
            _ -> #{formatter=>{?DEFAULT_FORMATTER,?DEFAULT_FORMAT_CONFIG}}
        end,
    Meta = #{time=>erlang:system_time(microsecond)},
    Bin = log_to_binary(#{level => notice,
                          msg => {Format,Args},
                          meta => Meta}, Config),
    _ = Module:async_write(Name, Bin, HandlerState),
    ok.

%%%-----------------------------------------------------------------
%%% Convert log data on any form to binary
-spec log_to_binary(LogEvent,Config) -> LogString when
      LogEvent :: logger:log_event(),
      Config :: logger:handler_config(),
      LogString :: binary().
log_to_binary(#{msg:={report,_},meta:=#{report_cb:=_}}=Log,Config) ->
    do_log_to_binary(Log,Config);
log_to_binary(#{msg:={report,_},meta:=Meta}=Log,Config) ->
    DefaultReportCb = fun logger:format_otp_report/1,
    do_log_to_binary(Log#{meta=>Meta#{report_cb=>DefaultReportCb}},Config);
log_to_binary(Log,Config) ->
    do_log_to_binary(Log,Config).

do_log_to_binary(Log,Config) ->
    {Formatter,FormatterConfig} =
        maps:get(formatter,Config,{?DEFAULT_FORMATTER,?DEFAULT_FORMAT_CONFIG}),
    String = try_format(Log,Formatter,FormatterConfig),
    try string_to_binary(String)
    catch C2:R2:S2 ->
            ?LOG_INTERNAL(debug,[{formatter_error,Formatter},
                                 {config,FormatterConfig},
                                 {log_event,Log},
                                 {bad_return_value,String},
                                 {catched,{C2,R2,S2}}]),
            <<"FORMATTER ERROR: bad return value">>
    end.

try_format(Log,Formatter,FormatterConfig) ->
    try Formatter:format(Log,FormatterConfig)
    catch
        C:R:S ->
            ?LOG_INTERNAL(debug,[{formatter_crashed,Formatter},
                                 {config,FormatterConfig},
                                 {log_event,Log},
                                 {reason,
                                  {C,R,logger:filter_stacktrace(?MODULE,S)}}]),
            case {?DEFAULT_FORMATTER,#{}} of
                {Formatter,FormatterConfig} ->
                    "DEFAULT FORMATTER CRASHED";
                {DefaultFormatter,DefaultConfig} ->
                    try_format(Log#{msg=>{"FORMATTER CRASH: ~tp",
                                          [maps:get(msg,Log)]}},
                              DefaultFormatter,DefaultConfig)
            end
    end.

string_to_binary(String) ->
    case unicode:characters_to_binary(String) of
        Binary when is_binary(Binary) ->
            Binary;
        Error ->
            throw(Error)
    end.

%%%-----------------------------------------------------------------
%%% Check that the configuration term is valid
check_config(Config) when is_map(Config) ->
    check_common_config(maps:to_list(Config)).

check_common_config([{sync_mode_qlen,N}|Config]) when is_integer(N) ->
    check_common_config(Config);
check_common_config([{drop_mode_qlen,N}|Config]) when is_integer(N) ->
    check_common_config(Config);
check_common_config([{flush_qlen,N}|Config]) when is_integer(N) ->
    check_common_config(Config);
check_common_config([{burst_limit_enable,Bool}|Config]) when is_boolean(Bool) ->
    check_common_config(Config);
check_common_config([{burst_limit_max_count,N}|Config]) when is_integer(N) ->
    check_common_config(Config);
check_common_config([{burst_limit_window_time,N}|Config]) when is_integer(N) ->
    check_common_config(Config);
check_common_config([{overload_kill_enable,Bool}|Config]) when is_boolean(Bool) ->
    check_common_config(Config);
check_common_config([{overload_kill_qlen,N}|Config]) when is_integer(N) ->
    check_common_config(Config);
check_common_config([{overload_kill_mem_size,N}|Config]) when is_integer(N) ->
    check_common_config(Config);
check_common_config([{overload_kill_restart_after,NorA}|Config])
  when is_integer(NorA); NorA == infinity ->
    check_common_config(Config);
check_common_config([{filesync_repeat_interval,NorA}|Config])
  when is_integer(NorA); NorA == no_repeat ->
    check_common_config(Config);
check_common_config([{Key,Value}|_]) ->
    {error,#{Key=>Value}};
check_common_config([]) ->
    ok.

get_default_config() ->
    #{sync_mode_qlen              => ?SYNC_MODE_QLEN,
      drop_mode_qlen              => ?DROP_MODE_QLEN,
      flush_qlen                  => ?FLUSH_QLEN,
      burst_limit_enable          => ?BURST_LIMIT_ENABLE,
      burst_limit_max_count       => ?BURST_LIMIT_MAX_COUNT,
      burst_limit_window_time     => ?BURST_LIMIT_WINDOW_TIME,
      overload_kill_enable        => ?OVERLOAD_KILL_ENABLE,
      overload_kill_qlen          => ?OVERLOAD_KILL_QLEN,
      overload_kill_mem_size      => ?OVERLOAD_KILL_MEM_SIZE,
      overload_kill_restart_after => ?OVERLOAD_KILL_RESTART_AFTER,
      filesync_repeat_interval    => ?FILESYNC_REPEAT_INTERVAL}.

%%%-----------------------------------------------------------------
%%% Overload Protection
call_cast_or_drop(_Name, HandlerPid, ModeTab, Bin) ->
    %% If the handler process is getting overloaded, the log event
    %% will be synchronous instead of asynchronous (slows down the
    %% logging tempo of a process doing lots of logging. If the
    %% handler is choked, drop mode is set and no event will be sent.
    try ?get_mode(ModeTab) of
        async ->
            gen_server:cast(HandlerPid, {log,Bin});
        sync ->
            try gen_server:call(HandlerPid, {log,Bin}, ?DEFAULT_CALL_TIMEOUT) of
                %% if return value from call == dropped, the
                %% message has been flushed by handler and should
                %% therefore not be counted as dropped in stats
                ok      -> ok;
                dropped -> ok
            catch
                _:{timeout,_} ->
                    ?observe(_Name,{dropped,1})
            end;
        drop ->
            ?observe(_Name,{dropped,1})
    catch
        %% if the ETS table doesn't exist (maybe because of a
        %% handler restart), we can only drop the event
        _:_ -> ?observe(_Name,{dropped,1})
    end,
    ok.

handler_exit(_Name, Reason) ->
    exit(Reason).

set_restart_flag(Name, Module) ->
    Flag = list_to_atom(lists:concat([Module,"_",Name,"_restarting"])),
    spawn(fun() ->
                  register(Flag, self()),
                  timer:sleep(infinity)
          end),
    ok.

unset_restart_flag(Name, Module) ->
    Flag = list_to_atom(lists:concat([Module,"_",Name,"_restarting"])),
    case whereis(Flag) of
        undefined ->
            false;
        Pid ->
            exit(Pid, kill),
            true
    end.

check_load(State = #{id:=_Name, mode_tab := ModeTab, mode := Mode,
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
                %% Note that drop mode will force log events to
                %% be dropped on the client side (never sent get to
                %% the handler).
                IncDrops = if Mode == drop -> 0; true -> 1 end,
                {?change_mode(ModeTab, Mode, drop), IncDrops,0};
            QLen >= SyncModeQLen ->
                {?change_mode(ModeTab, Mode, sync), 0,0};
            true ->
                {?change_mode(ModeTab, Mode, async), 0,0}
        end,
    State1 = ?update_other(drops,DROPS,_NewDrops,State),
    {Mode1, QLen, Mem,
     ?update_other(flushes,FLUSHES,_NewFlushes,
                   State1#{last_qlen => QLen})}.

limit_burst(#{burst_limit_enable := false}) ->
     {true,0,0};
limit_burst(#{burst_win_ts := BurstWinT0,
              burst_msg_count := BurstMsgCount,
              burst_limit_window_time := BurstLimitWinTime,
              burst_limit_max_count := BurstLimitMaxCnt}) ->
    if (BurstMsgCount >= BurstLimitMaxCnt) -> 
            %% the limit for allowed messages has been reached
            BurstWinT1 = ?timestamp(),
            case ?diff_time(BurstWinT1,BurstWinT0) of
                BurstCheckTime when BurstCheckTime < (BurstLimitWinTime*1000) ->
                    %% we're still within the burst time frame
                    {false,BurstWinT0,BurstMsgCount};
                _BurstCheckTime ->
                    %% burst time frame passed, reset counters
                    {true,BurstWinT1,0}
            end;
       true ->
            %% the limit for allowed messages not yet reached
            {true,BurstWinT0,BurstMsgCount+1}
    end.

kill_if_choked(Name, Module, QLen, Mem,
               State = #{overload_kill_enable   := KillIfOL,
                         overload_kill_qlen     := OLKillQLen,
                         overload_kill_mem_size := OLKillMem}) ->
    if KillIfOL andalso
       ((QLen > OLKillQLen) orelse (Mem > OLKillMem)) ->
            log_handler_info(Name,
                             "Handler ~p overloaded and stopping",
                             [Name], State),
            set_restart_flag(Name, Module),
            handler_exit(Name, {shutdown,{overloaded,Name,QLen,Mem}});
       true ->
            ok
    end.

flush_log_events() ->
    flush_log_events(-1).

flush_log_events(Limit) ->
    process_flag(priority, high),
    Flushed = flush_log_events(0, Limit),
    process_flag(priority, normal),
    Flushed.

flush_log_events(Limit, Limit) ->
    Limit;
flush_log_events(N, Limit) ->
    %% flush log events but leave other events, such as
    %% filesync, info and change_config, so that these
    %% have a chance to be processed even under heavy load
    receive
        {'$gen_cast',{log,_}} ->
            flush_log_events(N+1, Limit);
        {'$gen_call',{Pid,MRef},{log,_}} ->
            Pid ! {MRef, dropped},
            flush_log_events(N+1, Limit)
    after
        0 -> N
    end.

cancel_timer(TRef) when is_atom(TRef) -> ok;
cancel_timer(TRef) -> timer:cancel(TRef).


stop_or_restart(Name, {shutdown,Reason={overloaded,_Name,_QLen,_Mem}},
                #{overload_kill_restart_after := RestartAfter}) ->
    %% If we're terminating because of an overload situation (see
    %% kill_if_choked/5), we need to remove the handler and set a
    %% restart timer. A separate process must perform this in order to
    %% avoid deadlock.
    HandlerPid = self(),
    ConfigResult = logger:get_handler_config(Name),
    RemoveAndRestart =
        fun() ->
                MRef = erlang:monitor(process, HandlerPid),
                receive
                    {'DOWN',MRef,_,_,_} ->
                        ok
                after 30000 ->
                        error_notify(Reason),
                        exit(HandlerPid, kill)
                end,
                case ConfigResult of
                    {ok,#{module:=HMod}=HConfig0} when is_integer(RestartAfter) ->
                        _ = logger:remove_handler(Name),
                        HConfig = try HMod:filter_config(HConfig0)
                                  catch _:_ -> HConfig0
                                  end,
                        _ = timer:apply_after(RestartAfter, logger, add_handler,
                                              [Name,HMod,HConfig]);
                    {ok,_} ->
                        _ = logger:remove_handler(Name);
                    {error,CfgReason} when is_integer(RestartAfter) ->
                        error_notify({Name,restart_impossible,CfgReason});
                    {error,_} ->
                        ok
                end
        end,
    spawn(RemoveAndRestart),
    ok;
stop_or_restart(_Name, _Reason, _State) ->
    ok.

overload_levels_ok(HandlerConfig) ->
    SMQL = maps:get(sync_mode_qlen, HandlerConfig, ?SYNC_MODE_QLEN),
    DMQL = maps:get(drop_mode_qlen, HandlerConfig, ?DROP_MODE_QLEN),
    FQL = maps:get(flush_qlen, HandlerConfig, ?FLUSH_QLEN),
    (DMQL > 1) andalso (SMQL =< DMQL) andalso (DMQL =< FQL).

error_notify(Term) ->
    ?internal_log(error, Term).
