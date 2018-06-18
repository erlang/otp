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
-module(logger_disk_log_h).

-behaviour(gen_server).

-include("logger.hrl").
-include("logger_internal.hrl").
-include("logger_h_common.hrl").

%%% API
-export([start_link/3, info/1, filesync/1, reset/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% logger callbacks
-export([log/2, adding_handler/1, removing_handler/1, changing_config/2]).

%% handler internal
-export([log_handler_info/4]).

%%%===================================================================
%%% API
%%%===================================================================

%%%-----------------------------------------------------------------
%%% Start a disk_log handler process and link to caller.
%%% This function is called by the kernel supervisor when this
%%% handler process gets added (as a result of calling add/3).
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
-spec filesync(Name) -> ok | {error,Reason} when
      Name :: atom(),
      Reason :: handler_busy | {badarg,term()}.

filesync(Name) when is_atom(Name) ->
    try
        gen_server:call(?name_to_reg_name(?MODULE,Name),
                        disk_log_sync, ?DEFAULT_CALL_TIMEOUT)
    catch
        _:{timeout,_} -> {error,handler_busy}
    end;
filesync(Name) ->
    {error,{badarg,{filesync,[Name]}}}.

%%%-----------------------------------------------------------------
%%%
-spec info(Name) -> Info | {error,Reason} when
      Name :: atom(),
      Info :: term(),
      Reason :: handler_busy | {badarg,term()}.

info(Name) when is_atom(Name) ->
    try
        gen_server:call(?name_to_reg_name(?MODULE,Name),
                        info, ?DEFAULT_CALL_TIMEOUT)
    catch
        _:{timeout,_} -> {error,handler_busy}
    end;
info(Name) ->
    {error,{badarg,{info,[Name]}}}.

%%%-----------------------------------------------------------------
%%%
-spec reset(Name) -> ok | {error,Reason} when
      Name :: atom(),
      Reason :: handler_busy | {badarg,term()}.

reset(Name) when is_atom(Name) ->
    try
        gen_server:call(?name_to_reg_name(?MODULE,Name),
                        reset, ?DEFAULT_CALL_TIMEOUT)
    catch
        _:{timeout,_} -> {error,handler_busy}
    end;      
reset(Name) ->
    {error,{badarg,{reset,[Name]}}}.


%%%===================================================================
%%% logger callbacks
%%%===================================================================

%%%-----------------------------------------------------------------
%%% Handler being added
adding_handler(#{id:=Name}=Config) ->
    case check_config(adding, Config) of
        {ok, Config1} ->
            %% create initial handler state by merging defaults with config
            HConfig = maps:get(config, Config1, #{}),
            HState = maps:merge(get_init_state(), HConfig),
            case logger_h_common:overload_levels_ok(HState) of
                true ->
                    start(Name, Config1, HState);
                false ->
                    #{sync_mode_qlen := SMQL,
                      drop_mode_qlen := DMQL,
                      flush_qlen     := FQL} = HState, 
                    {error,{invalid_levels,{SMQL,DMQL,FQL}}}
            end;
        Error ->
            Error
    end.

%%%-----------------------------------------------------------------
%%% Updating handler config
changing_config(OldConfig = #{id:=Name, config:=OldHConfig},
                NewConfig = #{id:=Name, config:=NewHConfig}) ->
    #{type:=Type, file:=File, max_no_files:=MaxFs,
      max_no_bytes:=MaxBytes} = OldHConfig,
    case NewHConfig of
        #{type:=Type, file:=File, max_no_files:=MaxFs,
          max_no_bytes:=MaxBytes} ->
            changing_config1(OldConfig, NewConfig);
        _ ->
            {error,{illegal_config_change,OldConfig,NewConfig}}
    end;
changing_config(OldConfig, NewConfig) ->
    {error,{illegal_config_change,OldConfig,NewConfig}}.

changing_config1(OldConfig=#{config:=OldHConfig}, NewConfig) ->
    case check_config(changing, NewConfig) of
        {ok,NewConfig1 = #{config:=NewHConfig}} ->
            #{handler_pid:=HPid,
              mode_tab:=ModeTab} = OldHConfig,
            NewHConfig1 = NewHConfig#{handler_pid=>HPid,
                                      mode_tab=>ModeTab},
            NewConfig2 = NewConfig1#{config=>NewHConfig1},
            try gen_server:call(HPid, {change_config,OldConfig,NewConfig2},
                                ?DEFAULT_CALL_TIMEOUT) of
                ok      -> {ok,NewConfig2};
                HError  -> HError
            catch
                _:{timeout,_} -> {error,handler_busy}
            end;
        Error ->
            Error
    end.

check_config(adding, #{id:=Name}=Config) ->
    %% merge handler specific config data
    HConfig = merge_default_logopts(Name, maps:get(config, Config, #{})),
    case check_h_config(maps:to_list(HConfig)) of
        ok ->
            {ok,Config#{config=>HConfig}};
        Error ->
            Error
    end;
check_config(changing, Config) ->
    HConfig = maps:get(config, Config, #{}),
    case check_h_config(maps:to_list(HConfig)) of
        ok    -> {ok,Config};
        Error -> Error
    end.

merge_default_logopts(Name, HConfig) ->
    Type = maps:get(type, HConfig, wrap),
    {DefaultNoFiles,DefaultNoBytes} =
        case Type of
            halt -> {undefined,infinity};
            _wrap -> {10,1048576}
        end,
    {ok,Dir} = file:get_cwd(),
    Defaults = #{file => filename:join(Dir,Name),
                 max_no_files => DefaultNoFiles,
                 max_no_bytes => DefaultNoBytes,
                 type => Type},
    maps:merge(Defaults, HConfig).

check_h_config([{file,File}|Config]) when is_list(File) ->
    check_h_config(Config);
check_h_config([{max_no_files,undefined}|Config]) ->
    check_h_config(Config);
check_h_config([{max_no_files,N}|Config]) when is_integer(N), N>0 ->
    check_h_config(Config);
check_h_config([{max_no_bytes,infinity}|Config]) ->
    check_h_config(Config);
check_h_config([{max_no_bytes,N}|Config]) when is_integer(N), N>0 ->
    check_h_config(Config);
check_h_config([{type,Type}|Config]) when Type==wrap; Type==halt ->
    check_h_config(Config);
check_h_config([Other | Config]) ->
    case logger_h_common:check_common_config(Other) of
        valid ->
            check_h_config(Config);
        invalid ->
            {error,{invalid_config,?MODULE,Other}}
    end;
check_h_config([]) ->
    ok.

%%%-----------------------------------------------------------------
%%% Handler being removed
removing_handler(#{id:=Name}) ->
    stop(Name).

%%%-----------------------------------------------------------------
%%% Log a string or report
-spec log(LogEvent, Config) -> ok | dropped when
      LogEvent :: logger:log_event(),
      Config :: logger:handler_config().

log(LogEvent, Config = #{id := Name,
                         config := #{handler_pid := HPid,
                                     mode_tab := ModeTab}}) ->
    %% if the handler has crashed, we must drop this event
    %% and hope the handler restarts so we can try again
    true = is_process_alive(HPid),
    Bin = logger_h_common:log_to_binary(LogEvent, Config),
    logger_h_common:call_cast_or_drop(Name, HPid, ModeTab, Bin).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Name,
      Config = #{config := HConfig = #{file:=File,
                                       type:=Type,
                                       max_no_bytes:=MNB,
                                       max_no_files:=MNF}},
      State = #{dl_sync_int := DLSyncInt}]) ->

    RegName = ?name_to_reg_name(?MODULE,Name),
    register(RegName, self()),
    process_flag(trap_exit, true),
    process_flag(message_queue_data, off_heap),

    ?init_test_hooks(),
    ?start_observation(Name),
    
    LogOpts = #{file=>File, type=>Type, max_no_bytes=>MNB, max_no_files=>MNF},
    case open_disk_log(Name, File, Type, MNB, MNF) of
        ok ->
            try ets:new(Name, [public]) of
                ModeTab ->
                    ?set_mode(ModeTab, async),
                    T0 = ?timestamp(),
                    State1 =
                        ?merge_with_stats(State#{
                                            id => Name,
                                            mode_tab => ModeTab,
                                            mode => async,
                                            dl_sync => DLSyncInt,
                                            log_opts => LogOpts,
                                            last_qlen => 0,
                                            last_log_ts => T0,
                                            burst_win_ts => T0,
                                            burst_msg_count => 0,
                                            last_op => sync,
                                            prev_log_result => ok,
                                            prev_sync_result => ok,
                                            prev_disk_log_info => undefined}),
                    Config1 =
                        Config#{config => HConfig#{handler_pid => self(),
                                                   mode_tab => ModeTab}},
                    proc_lib:init_ack({ok,self(),Config1}),
                    gen_server:cast(self(), repeated_disk_log_sync),
                    case logger_h_common:unset_restart_flag(Name, ?MODULE) of
                        true ->
                            %% inform about restart
                            gen_server:cast(self(), {log_handler_info,
                                                     "Handler ~p restarted",
                                                     [Name]});
                        false ->
                            %% initial start
                            ok
                    end,
                    enter_loop(Config1, State1)
            catch
                _:Error ->
                    unregister(RegName),
                    logger_h_common:error_notify({open_disk_log,Name,Error}),
                    proc_lib:init_ack(Error)
            end;
        Error ->
            unregister(RegName),
            logger_h_common:error_notify({open_disk_log,Name,Error}),
            proc_lib:init_ack(Error)
    end.

enter_loop(_Config,State) ->
    gen_server:enter_loop(?MODULE,[],State).

%% This is the synchronous log event.
handle_call({log, Bin}, _From, State) ->
    {Result,State1} = do_log(Bin, call, State),
    %% Result == ok | dropped
    {reply, Result, State1};

handle_call(disk_log_sync, _From, State = #{id := Name}) ->
    State1 = #{prev_sync_result := Result} = disk_log_sync(Name, State),
    {reply, Result, State1};

handle_call({change_config,_OldConfig,NewConfig}, _From,
            State = #{filesync_repeat_interval := FSyncInt0}) ->
    HConfig = maps:get(config, NewConfig, #{}),
    State1 = #{sync_mode_qlen := SMQL,
               drop_mode_qlen := DMQL,
               flush_qlen     := FQL} = maps:merge(State, HConfig),
    case logger_h_common:overload_levels_ok(State1) of
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
                        _ = gen_server:cast(self(), repeated_disk_log_sync)
                end,
            {reply, ok, State1};
        false ->
            {reply, {error,{invalid_levels,{SMQL,DMQL,FQL}}}, State}
    end;

handle_call(info, _From, State) ->
    {reply, State, State};

handle_call(reset, _From, State) ->
    State1 = ?merge_with_stats(State),
    {reply, ok, State1#{last_qlen => 0,
                        last_log_ts => ?timestamp(),
                        prev_log_result => ok,
                        prev_sync_result => ok,
                        prev_disk_log_info => undefined}};

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
handle_cast(repeated_disk_log_sync,
            State = #{id := Name,
                      filesync_repeat_interval := FSyncInt,
                      last_op := LastOp}) ->
    State1 =
        if is_integer(FSyncInt) ->
                %% only do filesync if something has been
                %% written since last time we checked
                NewState = if LastOp == sync ->
                                   State;
                              true ->
                                   disk_log_sync(Name, State)
                           end,
                {ok,TRef} =
                    timer:apply_after(FSyncInt, gen_server,cast,
                                      [self(),repeated_disk_log_sync]),
                NewState#{rep_sync_tref => TRef, last_op => sync};
           true ->
                State
        end,
    {noreply,State1}.

%% The disk log owner must handle status messages from disk_log.
handle_info({disk_log, _Node, _Log, {wrap,_NoLostItems}}, State) ->
    {noreply, State};
handle_info({disk_log, _Node, Log, Info = {truncated,_NoLostItems}},
            State = #{id := Name, prev_disk_log_info := PrevInfo}) ->
    error_notify_new(Info, PrevInfo, {disk_log,Name,Log,Info}),
    {noreply, State#{prev_disk_log_info => Info}};
handle_info({disk_log, _Node, Log, Info = {blocked_log,_Items}},
            State = #{id := Name, prev_disk_log_info := PrevInfo}) ->
    error_notify_new(Info, PrevInfo, {disk_log,Name,Log,Info}),
    {noreply, State#{prev_disk_log_info => Info}};
handle_info({disk_log, _Node, Log, full},
            State = #{id := Name, prev_disk_log_info := PrevInfo}) ->
    error_notify_new(full, PrevInfo, {disk_log,Name,Log,full}),
    {noreply, State#{prev_disk_log_info => full}};
handle_info({disk_log, _Node, Log, Info = {error_status,_Status}},
            State = #{id := Name, prev_disk_log_info := PrevInfo}) ->
    error_notify_new(Info, PrevInfo, {disk_log,Name,Log,Info}),
    {noreply, State#{prev_disk_log_info => Info}};

handle_info({'EXIT',_Pid,_Why}, State = #{id := _Name}) ->
    {noreply, State};

handle_info(_, State) ->
    {noreply, State}.

terminate(Reason, State = #{id := Name}) ->
    _ = logger_h_common:cancel_timer(maps:get(rep_sync_tref, State,
                                              undefined)),
    _ = close_disk_log(Name, normal),
    unregister(?name_to_reg_name(?MODULE, Name)),
    logger_h_common:stop_or_restart(Name, Reason, State).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%-----------------------------------------------------------------
%%% Internal functions

%%%-----------------------------------------------------------------
%%%
get_init_state() ->
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
       dl_sync_int                 => ?CONTROLLER_SYNC_INTERVAL,
       filesync_ok_qlen            => ?FILESYNC_OK_QLEN,
       filesync_repeat_interval    => ?FILESYNC_REPEAT_INTERVAL}.

%%%-----------------------------------------------------------------
%%% Add a disk_log handler to the logger.
%%% This starts a dedicated handler process which should always
%%% exist if the handler is registered with logger (and should not
%%% exist if the handler is not registered).
%%%
%%% Config is the logger:handler_config() map. Handler specific parameters
%%% should be provided with a sub map associated with a key named
%%% 'config', e.g:
%%%
%%% Config = #{config => #{sync_mode_qlen => 50}
%%%
%%% The 'config' sub map will also contain parameters for configuring
%%% the disk_log:
%%%
%%% Config = #{config => #{file          => file:filename(),
%%%                        max_no_bytes  => integer(),
%%%                        max_no_files  => integer(),
%%%                        type          => wrap | halt}}.
%%%
%%% If type == halt, then max_no_files is ignored.
%%%
%%% The disk_log handler process is linked to logger_sup, which is
%%% part of the kernel application's supervision tree.
start(Name, Config, HandlerState) ->
    LoggerDLH =
        #{id       => Name,
          start    => {?MODULE, start_link, [Name,Config,HandlerState]},
          restart  => temporary,
          shutdown => 2000,
          type     => worker,
          modules  => [?MODULE]},
    case supervisor:start_child(logger_sup, LoggerDLH) of
        {ok,_Pid,Config1} ->
            {ok,Config1};
        Error ->
            Error
    end.

%%%-----------------------------------------------------------------
%%% Stop and remove the handler.
stop(Name) ->
    case whereis(?name_to_reg_name(?MODULE,Name)) of
        undefined ->
            ok;
        Pid ->
            %% We don't want to do supervisor:terminate_child here
            %% since we need to distinguish this explicit stop from a
            %% system termination in order to avoid circular attempts
            %% at removing the handler (implying deadlocks and
            %% timeouts).
            _ = gen_server:call(Pid, stop),
            _ = supervisor:delete_child(logger_sup, Name),
            ok
    end.

%%%-----------------------------------------------------------------
%%% Logging and overload control.
-define(update_dl_sync(C, Interval),
        if C == 0 -> Interval;
           true -> C-1 end).

%% check for overload between every event (and set Mode to async,
%% sync or drop accordingly), but never flush the whole mailbox
%% before LogWindowSize events have been handled
do_log(Bin, CallOrCast, State = #{id:=Name, mode := Mode0}) ->
    T1 = ?timestamp(),

    %% check if the handler is getting overloaded, or if it's
    %% recovering from overload (the check must be done for each
    %% event to react quickly to large bursts of events and
    %% to ensure that the handler can never end up in drop mode
    %% with an empty mailbox, which would stop operation)
    {Mode1,QLen,Mem,State1} = logger_h_common:check_load(State),

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
    logger_h_common:kill_if_choked(Name, QLen, Mem, ?MODULE, State),

    if Mode1 == flush ->            
            flush(Name, QLen, T1, State1);
       true ->
            write(Name, Mode1, T1, Bin, CallOrCast, State1)
    end.

%% this function is called by do_log/3 after an overload check
%% has been performed, where QLen > FlushQLen
flush(Name, _QLen0, T1, State=#{last_log_ts := _T0, mode_tab := ModeTab}) ->
    %% flush messages in the mailbox (a limited number in
    %% order to not cause long delays)
    NewFlushed = logger_h_common:flush_log_events(?FLUSH_MAX_N),

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

%% this function is called to write to disk_log
write(Name, Mode, T1, Bin, _CallOrCast,
      State = #{mode_tab := ModeTab,
                dl_sync := DLSync,
                dl_sync_int := DLSyncInt,
                last_qlen := LastQLen,
                last_log_ts := T0}) ->
    %% check if we need to limit the number of writes
    %% during a burst of log events
    {DoWrite,BurstWinT,BurstMsgCount} = logger_h_common:limit_burst(State),

    %% only send a synhrounous event to the disk_log process
    %% every DLSyncInt time, to give the handler time between
    %% writes so it can keep up with incoming messages
    {Status,LastQLen1,State1} =
        if DoWrite, DLSync == 0 ->
                ?observe(Name,{_CallOrCast,1}),
                NewState = disk_log_write(Name, Bin, State),
                {ok, element(2,process_info(self(),message_queue_len)),
                 NewState};
           DoWrite ->
                ?observe(Name,{_CallOrCast,1}),
                NewState = disk_log_write(Name, Bin, State),
                {ok, LastQLen, NewState};
           not DoWrite ->
                ?observe(Name,{flushed,1}),
                {dropped, LastQLen, State}
        end,

    %% Check if the time since the previous log event is long enough -
    %% and the queue length small enough - to assume the mailbox has
    %% been emptied, and if so, do filesync operation and reset mode to
    %% async. Note that this is the best we can do to detect an idle
    %% handler without setting a timer after each log call/cast. If the
    %% time between two consecutive log events is fast and no new
    %% event comes in after the last one, idle state won't be detected!
    Time = ?diff_time(T1,T0),
    {Mode1,BurstMsgCount1,State2} =
        if (LastQLen1 < ?FILESYNC_OK_QLEN) andalso
           (Time > ?IDLE_DETECT_TIME_USEC) ->
                {?change_mode(ModeTab,Mode,async), 0, disk_log_sync(Name,State1)};
           true ->
                {Mode, BurstMsgCount,State1}
        end,
    
    State3 =
        ?update_calls_or_casts(_CallOrCast,1,State2),
    State4 =
        ?update_max_time(Time,
                         State3#{mode => Mode1,
                                 last_qlen := LastQLen1,
                                 last_log_ts => T1,
                                 burst_win_ts => BurstWinT,
                                 burst_msg_count => BurstMsgCount1,
                                 dl_sync => ?update_dl_sync(DLSync,DLSyncInt)}),
    {Status,State4}.


log_handler_info(Name, Format, Args, State) ->
    Config =
        case logger:get_handler_config(Name) of
            {ok,Conf} -> Conf;
            _ -> #{formatter=>{?DEFAULT_FORMATTER,?DEFAULT_FORMAT_CONFIG}}
        end,
    Meta = #{time=>erlang:system_time(microsecond)},
    Bin = logger_h_common:log_to_binary(#{level => notice,
                                          msg => {Format,Args},
                                          meta => Meta}, Config),
    _ = disk_log_write(Name, Bin, State),
    ok.


open_disk_log(Name, File, Type, MaxNoBytes, MaxNoFiles) ->
    case filelib:ensure_dir(File) of
        ok ->
            Size =
                if Type == halt -> MaxNoBytes;
                   Type == wrap -> {MaxNoBytes,MaxNoFiles}
                end,
            Opts = [{name,   Name},
                    {file,   File},
                    {size,   Size},
                    {type,   Type},
                    {linkto, self()},
                    {repair, false},
                    {format, external},
                    {notify, true},
                    {quiet,  true},
                    {mode,   read_write}],            
            case disk_log:open(Opts) of
                {ok,Name} ->
                    ok;
                Error = {error,_Reason} ->            
                    Error
            end;
        Error ->
            Error
    end.

close_disk_log(Name, _) ->
    _ = ?disk_log_sync(Name),
    _ = disk_log:lclose(Name),
    ok.

disk_log_write(Name, Bin, State) ->
        case ?disk_log_blog(Name, Bin) of
            ok ->
                State#{prev_log_result => ok, last_op => write};
            LogError ->
                _ = case maps:get(prev_log_result, State) of
                        LogError ->
                            %% don't report same error twice
                            ok;
                        _ ->
                            LogOpts = maps:get(log_opts, State),
                            logger_h_common:error_notify({Name,log,
                                                          LogOpts,
                                                          LogError})
                    end,
                State#{prev_log_result => LogError}
        end.

disk_log_sync(Name, State) ->
    case ?disk_log_sync(Name) of
        ok ->
            State#{prev_sync_result => ok, last_op => sync};
        SyncError ->
            _ = case maps:get(prev_sync_result, State) of
                    SyncError ->
                        %% don't report same error twice
                        ok;
                    _ ->
                        LogOpts = maps:get(log_opts, State),
                        logger_h_common:error_notify({Name,filesync,
                                                      LogOpts,
                                                      SyncError})
                end,
            State#{prev_sync_result => SyncError}
    end.

error_notify_new(Info,Info, _Term) ->
    ok;
error_notify_new(_Info0,_Info1, Term) ->
    logger_h_common:error_notify(Term).
