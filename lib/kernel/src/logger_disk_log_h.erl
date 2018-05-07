%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2017. All Rights Reserved.
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
-export([start_link/3, info/1, disk_log_sync/1, reset/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% logger callbacks
-export([log/2,
         adding_handler/2, removing_handler/2,
         changing_config/3, swap_buffer/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%%-----------------------------------------------------------------
%%% Start a disk_log handler process and link to caller.
%%% This function is called by the kernel supervisor when this
%%% handler process gets added (as a result of calling add/3).
-spec start_link(Name, Config, HandlerState) -> {ok,Pid} | {error,Reason} when
      Name :: atom(),
      Config :: logger:config(),
      HandlerState :: map(),
      Pid :: pid(),
      Reason :: term().

start_link(Name, Config, HandlerState) ->
    proc_lib:start_link(?MODULE,init,[[Name,Config,HandlerState]]).

%%%-----------------------------------------------------------------
%%%
-spec disk_log_sync(Name) -> ok | {error,Reason} when
      Name :: atom(),
      Reason :: handler_busy | {badarg,term()}.

disk_log_sync(Name) when is_atom(Name) ->
    try
        gen_server:call(Name, disk_log_sync, ?DEFAULT_CALL_TIMEOUT)
    catch
        _:{timeout,_} -> {error,handler_busy}
    end;
disk_log_sync(Name) ->
    {error,{badarg,{disk_log_sync,[Name]}}}.

%%%-----------------------------------------------------------------
%%%
-spec info(Name) -> Info | {error,Reason} when
      Name :: atom(),
      Info :: term(),
      Reason :: handler_busy | {badarg,term()}.

info(Name) when is_atom(Name) ->
    try
        gen_server:call(Name, info, ?DEFAULT_CALL_TIMEOUT)
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
        gen_server:call(Name, reset, ?DEFAULT_CALL_TIMEOUT)
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
adding_handler(Name, Config) ->
    case check_config(adding, Name, Config) of
        {ok, Config1} ->
            %% create initial handler state by merging defaults with config
            HConfig = maps:get(?MODULE, Config1, #{}),
            HState = maps:merge(get_init_state(), HConfig),
            case logger_h_common:overload_levels_ok(HState) of
                true ->
                    case start(Name, Config1, HState) of
                        ok ->
                            %% Make sure wait_for_buffer is not stored, so we
                            %% won't hang and wait for buffer on a restart
                            {ok, maps:remove(wait_for_buffer,Config1)};
                        Error ->
                            Error
                    end;
                false ->
                    #{toggle_sync_qlen   := TSQL,
                      drop_new_reqs_qlen := DNRQL,
                      flush_reqs_qlen    := FRQL} = HState, 
                    {error,{invalid_levels,{TSQL,DNRQL,FRQL}}}
            end;
        Error ->
            Error
    end.

%%%-----------------------------------------------------------------
%%% Updating handler config
changing_config(Name,
                OldConfig=#{id:=Id, disk_log_opts:=DLOpts},
                NewConfig=#{id:=Id, disk_log_opts:=DLOpts}) ->
    case check_config(changing, Name, NewConfig) of
        Result = {ok,NewConfig1} ->
            try gen_server:call(Name, {change_config,OldConfig,NewConfig1},
                                ?DEFAULT_CALL_TIMEOUT) of
                ok      -> Result;
                HError  -> HError
            catch
                _:{timeout,_} -> {error,handler_busy}
            end;
        Error ->
            Error
    end;
changing_config(_Name, OldConfig, NewConfig) ->
    {error,{illegal_config_change,OldConfig,NewConfig}}.

check_config(adding, Name, Config0) ->
    %% Merge in defaults on top level
    Config = maps:merge(#{id => Name}, Config0),
    %% Merge in defaults on handler level
    LogOpts0 = maps:get(disk_log_opts, Config, #{}),
    LogOpts = merge_default_logopts(Name, LogOpts0),
    case check_log_opts(maps:to_list(LogOpts)) of
        ok ->
            MyConfig = maps:get(?MODULE, Config, #{}),
            case check_my_config(maps:to_list(MyConfig)) of
                ok ->
                    {ok,Config#{disk_log_opts=>LogOpts,
                                ?MODULE=>MyConfig}};
                Error ->
                    Error
            end;
        Error ->
            Error
    end;
check_config(changing, _Name, Config) ->
    MyConfig = maps:get(?MODULE, Config, #{}),
    case check_my_config(maps:to_list(MyConfig)) of
        ok    -> {ok,Config};
        Error -> Error
    end.

merge_default_logopts(Name, LogOpts) ->
    Type = maps:get(type, LogOpts, wrap),
    {DefaultNoFiles,DefaultNoBytes} =
        case Type of
            halt -> {undefined,infinity};
            _wrap -> {10,1048576}
        end,
    {ok,Dir} = file:get_cwd(),
    Default = #{file => filename:join(Dir,Name),
                max_no_files => DefaultNoFiles,
                max_no_bytes => DefaultNoBytes,
                type => Type},
    maps:merge(Default,LogOpts).

check_log_opts([{file,File}|Opts]) when is_list(File) ->
    check_log_opts(Opts);
check_log_opts([{max_no_files,undefined}|Opts]) ->
    check_log_opts(Opts);
check_log_opts([{max_no_files,N}|Opts]) when is_integer(N), N>0 ->
    check_log_opts(Opts);
check_log_opts([{max_no_bytes,infinity}|Opts]) ->
    check_log_opts(Opts);
check_log_opts([{max_no_bytes,N}|Opts]) when is_integer(N), N>0 ->
    check_log_opts(Opts);
check_log_opts([{type,Type}|Opts]) when Type==wrap; Type==halt ->
    check_log_opts(Opts);
check_log_opts([Invalid|_]) ->
    {error,{invalid_config,disk_log_opt,Invalid}};
check_log_opts([]) ->
    ok.

check_my_config([Other | Config]) ->
    case logger_h_common:check_common_config(Other) of
        valid ->
            check_my_config(Config);
        invalid ->
            {error,{invalid_config,?MODULE,Other}}
    end;
check_my_config([]) ->
    ok.

%%%-----------------------------------------------------------------
%%% Handler being removed
removing_handler(Name, _Config) ->
    stop(Name).

%%%-----------------------------------------------------------------
%%% Get buffer when swapping from simple handler
swap_buffer(Name,Buffer) ->
    case whereis(Name) of
        undefined ->
            ok;
        _ ->
            Name ! {buffer,Buffer}
    end.

%%%-----------------------------------------------------------------
%%% Log a string or report
-spec log(Log, Config) -> ok | dropped when
      Log :: logger:log(),
      Config :: logger:config().

log(Log,Config=#{id:=Name}) ->
    %% if the handler has crashed, we must drop this request
    %% and hope the handler restarts so we can try again
    true = is_pid(whereis(Name)),
    Bin = logger_h_common:log_to_binary(Log,Config),
    logger_h_common:call_cast_or_drop(Name, Bin).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Name, Config = #{disk_log_opts := LogOpts},
      State = #{dl_sync_int := DLSyncInt}]) ->
    register(Name, self()),
    process_flag(trap_exit, true),
    process_flag(message_queue_data, off_heap),

    ?init_test_hooks(),
    ?start_observation(Name),

    case open_disk_log(Name, LogOpts) of
        ok ->
            catch ets:new(Name, [public, named_table]),
            ?set_mode(Name, async),
            proc_lib:init_ack({ok,self()}),
            T0 = ?timestamp(),
            State1 =
                ?merge_with_stats(State#{id => Name,
                                         mode => async,
                                         dl_sync => DLSyncInt,
                                         log_opts => LogOpts,
                                         last_qlen => 0,
                                         last_log_ts => T0,
                                         burst_win_ts => T0,
                                         burst_msg_count => 0,
                                         prev_log_result => ok,
                                         prev_sync_result => ok,
                                         prev_disk_log_info => undefined}),
            gen_server:cast(self(), {repeated_disk_log_sync,T0}),
            enter_loop(Config, State1);
        Error ->
            logger_h_common:error_notify({open_disk_log,Name,Error}),
            proc_lib:init_ack(Error)
    end.

enter_loop(#{wait_for_buffer:=true}=Config,State) ->
    State1 =
        receive
            {buffer,Buffer} ->
                lists:foldl(
                  fun(Log,S) ->
                          Bin = logger_h_common:log_to_binary(Log,Config),
                          {_,S1} = do_log(Bin,cast,S),
                          S1
                  end,
                  State,
                  Buffer)
        end,
    gen_server:enter_loop(?MODULE,[],State1);
enter_loop(_Config,State) ->
    gen_server:enter_loop(?MODULE,[],State).

%% This is the synchronous log request.
handle_call({log, Bin}, _From, State) ->
    {Result,State1} = do_log(Bin, call, State),
    %% Result == ok | dropped
    {reply, Result, State1};

handle_call(disk_log_sync, _From, State = #{id := Name}) ->
    State1 = #{prev_sync_result := Result} = disk_log_sync(Name, State),
    {reply, Result, State1};

handle_call({change_config,_OldConfig,NewConfig}, _From,
            State = #{filesync_repeat_interval := FSyncInt0,
                      last_log_ts := LastLogTS}) ->
    HConfig = maps:get(?MODULE, NewConfig, #{}),
    State1 = #{toggle_sync_qlen   := TSQL,
               drop_new_reqs_qlen := DNRQL,
               flush_reqs_qlen    := FRQL} = maps:merge(State, HConfig),
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
                        _ = gen_server:cast(self(), {repeated_disk_log_sync,
                                                     LastLogTS})
                end,            
            {reply, ok, State1};
        false ->
            {reply, {error,{invalid_levels,{TSQL,DNRQL,FRQL}}}, State}
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


%% This is the asynchronous log request.
handle_cast({log, Bin}, State) ->
    {_,State1} = do_log(Bin, cast, State),
    {noreply, State1};

%% If FILESYNC_REPEAT_INTERVAL is set to a millisec value, this
%% clause gets called repeatedly by the handler. In order to
%% guarantee that a filesync *always* happens after the last log
%% request, the repeat operation must be active!
handle_cast({repeated_disk_log_sync,LastLogTS0},
            State = #{id := Name,
                      filesync_repeat_interval := FSyncInt,
                      last_log_ts := LastLogTS1}) ->
    State1 =
        if is_integer(FSyncInt) ->
                %% only do filesync if something has been
                %% written since last time we checked
                NewState = if LastLogTS1 == LastLogTS0 ->
                                   State;
                              true ->
                                   disk_log_sync(Name, State)
                           end,
                {ok,TRef} =
                    timer:apply_after(FSyncInt, gen_server,cast,
                                      [self(),
                                       {repeated_disk_log_sync,LastLogTS1}]),
                NewState#{rep_sync_tref => TRef};
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
    logger_h_common:stop_or_restart(Name, Reason, State).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%-----------------------------------------------------------------
%%% Internal functions

%%%-----------------------------------------------------------------
%%%
get_init_state() ->
     #{toggle_sync_qlen           => ?TOGGLE_SYNC_QLEN,
       drop_new_reqs_qlen         => ?DROP_NEW_REQS_QLEN,
       flush_reqs_qlen            => ?FLUSH_REQS_QLEN,
       enable_burst_limit         => ?ENABLE_BURST_LIMIT,
       burst_limit_size           => ?BURST_LIMIT_SIZE,
       burst_window_time          => ?BURST_WINDOW_TIME,
       enable_kill_overloaded     => ?ENABLE_KILL_OVERLOADED,
       handler_overloaded_qlen    => ?HANDLER_OVERLOADED_QLEN,
       handler_overloaded_mem     => ?HANDLER_OVERLOADED_MEM,
       handler_restart_after      => ?HANDLER_RESTART_AFTER,
       dl_sync_int                => ?CONTROLLER_SYNC_INTERVAL,
       filesync_ok_qlen           => ?FILESYNC_OK_QLEN,
       filesync_repeat_interval   => ?FILESYNC_REPEAT_INTERVAL}.

%%%-----------------------------------------------------------------
%%% Add a disk_log handler to the logger.
%%% This starts a dedicated handler process which should always
%%% exist if the handler is registered with logger (and should not
%%% exist if the handler is not registered).
%%%
%%% Config is the logger:config() map containing a sub map with any of
%%% the following associations:
%%%
%%% Config = #{disk_log_opts => #{file          => file:filename(),
%%%                               max_no_bytes  => integer(),
%%%                               max_no_files  => integer(),
%%%                               type          => wrap | halt}}.
%%%
%%% This map will be merged with the logger configuration data for
%%% the disk_log LogName. If type == halt, then max_no_files is
%%% ignored.
%%%
%%% Handler specific config should be provided with a sub map associated
%%% with a key named the same as this module, e.g:
%%%
%%% Config = #{logger_disk_log_h => #{toggle_sync_qlen => 50}
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
        {ok,_} ->
            ok;
        Error ->
            Error
    end.

%%%-----------------------------------------------------------------
%%% Stop and remove the handler.
stop(Name) ->
    case whereis(Name) of
        undefined ->
            ok;
        _ ->
            %% We don't want to do supervisor:terminate_child here
            %% since we need to distinguish this explicit stop from a
            %% system termination in order to avoid circular attempts
            %% at removing the handler (implying deadlocks and
            %% timeouts).
            _ = gen_server:call(Name,stop),
            _ = supervisor:delete_child(logger_sup, Name),
            ok
    end.

%%%-----------------------------------------------------------------
%%% Logging and overload control.
-define(update_dl_sync(C, Interval),
        if C == 0 -> Interval;
           true -> C-1 end).

%% check for overload between every request (and set Mode to async,
%% sync or drop accordingly), but never flush the whole mailbox
%% before LogWindowSize requests have been handled
do_log(Bin, CallOrCast, State = #{id:=Name, mode := _Mode0}) ->
    T1 = ?timestamp(),

    %% check if the handler is getting overloaded, or if it's
    %% recovering from overload (the check must be done for each
    %% request to react quickly to large bursts of requests and
    %% to ensure that the handler can never end up in drop mode
    %% with an empty mailbox, which would stop operation)
    {Mode1,QLen,Mem,State1} = logger_h_common:check_load(State),

    %% kill the handler if it can't keep up with the load
    logger_h_common:kill_if_choked(Name, QLen, Mem, State),

    if Mode1 == flush ->            
            flush(Name, QLen, T1, State1);
       true ->
            write(Name, Mode1, T1, Bin, CallOrCast, State1)
    end.

%% this function is called by do_log/3 after an overload check
%% has been performed, where QLen > FlushQLen
flush(Name, _QLen0, T1, State=#{last_log_ts := _T0}) ->
    %% flush messages in the mailbox (a limited number in
    %% order to not cause long delays)
    _NewFlushed = logger_h_common:flush_log_requests(?FLUSH_MAX_N),

    %% because of the receive loop when flushing messages, the
    %% handler will be scheduled out often and the mailbox could
    %% grow very large, so we'd better check the queue again here
    {_,_QLen1} = process_info(self(), message_queue_len),
    ?observe(Name,{max_qlen,_QLen1}),

    %% Add 1 for the current log request
    ?observe(Name,{flushed,_NewFlushed+1}),

    State1 = ?update_max_time(?diff_time(T1,_T0),State),
    {dropped,?update_other(flushed,FLUSHED,_NewFlushed,
                           State1#{mode => ?set_mode(Name,async),
                                   last_qlen => 0,
                                   last_log_ts => T1})}.

%% this function is called to write to disk_log
write(Name, Mode, T1, Bin, _CallOrCast,
      State = #{dl_sync := DLSync,
                dl_sync_int := DLSyncInt,
                last_qlen := LastQLen,
                last_log_ts := T0}) ->
    %% check if we need to limit the number of writes
    %% during a burst of log requests
    {DoWrite,BurstWinT,BurstMsgCount} = logger_h_common:limit_burst(State),

    %% only send a synhrounous request to the disk_log process
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

    %% Check if the time since the previous log request is long enough -
    %% and the queue length small enough - to assume the mailbox has
    %% been emptied, and if so, do filesync operation and reset mode to
    %% async. Note that this is the best we can do to detect an idle
    %% handler without setting a timer after each log call/cast. If the
    %% time between two consecutive log requests is fast and no new
    %% request comes in after the last one, idle state won't be detected!
    Time = ?diff_time(T1,T0),
    {Mode1,BurstMsgCount1,State2} =
        if (LastQLen1 < ?FILESYNC_OK_QLEN) andalso
           (Time > ?IDLE_DETECT_TIME_USEC) ->
                {?change_mode(Name,Mode,async), 0, disk_log_sync(Name,State1)};
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


open_disk_log(Name, LogOpts) ->
    #{file          := File,
      max_no_bytes  := MaxNoBytes,
      max_no_files  := MaxNoFiles,
      type          := Type} = LogOpts,
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
    Result =
        case ?disk_log_blog(Name, Bin) of
            ok ->
                ok;
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
                LogError
        end,
    State#{prev_log_result => Result}.

disk_log_sync(Name, State) ->
    Result =
        case ?disk_log_sync(Name) of
            ok ->
                ok;
            SyncError ->
                _ = case maps:get(prev_sync_result, State) of
                        SyncError ->
                            %% don't report same error twice
                            ok;
                        _ ->
                            LogOpts = maps:get(log_opts, State),
                            logger_h_common:error_notify({Name,sync,
                                                          LogOpts,
                                                          SyncError})
                    end,
                SyncError
        end,
    State#{prev_sync_result => Result}. 

error_notify_new(Info,Info, _Term) ->
    ok;
error_notify_new(_Info0,_Info1, Term) ->
    logger_h_common:error_notify(Term).
