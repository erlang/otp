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
-module(logger_std_h).

-behaviour(gen_server).

-include("logger.hrl").
-include("logger_internal.hrl").
-include("logger_h_common.hrl").

-include_lib("kernel/include/file.hrl").

%% API
-export([start_link/3, info/1, filesync/1, reset/1]).

%% gen_server and proc_lib callbacks
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
-spec filesync(Name) -> ok | {error,Reason} when
      Name :: atom(),
      Reason :: handler_busy | {badarg,term()}.

filesync(Name) when is_atom(Name) ->
    try
        gen_server:call(?name_to_reg_name(?MODULE,Name),
                        filesync, ?DEFAULT_CALL_TIMEOUT)
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
changing_config(OldConfig=#{id:=Name, config:=OldHConfig},
                NewConfig=#{id:=Name}) ->
    #{type:=Type, handler_pid:=HPid, mode_tab:=ModeTab} = OldHConfig,
    NewHConfig = maps:get(config, NewConfig, #{}),
    case maps:get(type, NewHConfig, Type) of
        Type ->
            NewHConfig1 = NewHConfig#{type=>Type,
                                      handler_pid=>HPid,
                                      mode_tab=>ModeTab},
            changing_config1(HPid, OldConfig,
                             NewConfig#{config=>NewHConfig1});
        _ ->
            {error,{illegal_config_change,OldConfig,NewConfig}}
    end;
changing_config(OldConfig, NewConfig) ->
    {error,{illegal_config_change,OldConfig,NewConfig}}.

changing_config1(HPid, OldConfig, NewConfig) ->
    case check_config(changing, NewConfig) of
        Result = {ok,NewConfig1} ->
            try gen_server:call(HPid, {change_config,OldConfig,NewConfig1},
                                ?DEFAULT_CALL_TIMEOUT) of
                ok      -> Result;
                HError  -> HError
            catch
                _:{timeout,_} -> {error,handler_busy}
            end;
        Error ->
            Error
    end.    

check_config(adding, Config) ->
    %% Merge in defaults on handler level
    HConfig0 = maps:get(config, Config, #{}),
    HConfig = maps:merge(#{type => standard_io},
                         HConfig0),
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

check_h_config([{type,Type} | Config]) when Type == standard_io;
                                            Type == standard_error ->
    check_h_config(Config);
check_h_config([{type,{file,File}} | Config]) when is_list(File) ->
    check_h_config(Config);
check_h_config([{type,{file,File,Modes}} | Config]) when is_list(File),
                                                         is_list(Modes) ->
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

init([Name, Config = #{config := HConfig},
      State0 = #{type := Type, file_ctrl_sync_int := FileCtrlSyncInt}]) ->    
    RegName = ?name_to_reg_name(?MODULE,Name),
    register(RegName, self()),
    process_flag(trap_exit, true),
    process_flag(message_queue_data, off_heap),

    ?init_test_hooks(),
    ?start_observation(Name),

    case do_init(Name, Type) of
        {ok,InitState} ->
            try ets:new(Name, [public]) of
                ModeTab ->
                    ?set_mode(ModeTab, async),
                    State = maps:merge(State0, InitState),
                    T0 = ?timestamp(),
                    State1 =
                        ?merge_with_stats(State#{
                                            mode_tab => ModeTab,
                                            mode => async,
                                            file_ctrl_sync => FileCtrlSyncInt,
                                            last_qlen => 0,
                                            last_log_ts => T0,
                                            last_op => sync,
                                            burst_win_ts => T0,
                                            burst_msg_count => 0}),
                    Config1 =
                        Config#{config => HConfig#{handler_pid => self(),
                                                   mode_tab => ModeTab}},
                    proc_lib:init_ack({ok,self(),Config1}),
                    gen_server:cast(self(), repeated_filesync),
                    enter_loop(Config1, State1)
            catch
                _:Error ->
                    unregister(RegName),
                    logger_h_common:error_notify({init_handler,Name,Error}),
                    proc_lib:init_ack(Error)
            end;
        Error ->
            unregister(RegName),
            logger_h_common:error_notify({init_handler,Name,Error}),
            proc_lib:init_ack(Error)
    end.

do_init(Name, Type) ->
    case open_log_file(Name, Type) of
        {ok,FileCtrlPid} ->
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
            {ok,#{id=>Name,type=>Type,file_ctrl_pid=>FileCtrlPid}};
        Error ->
            Error
    end.

enter_loop(_Config,State) ->
    gen_server:enter_loop(?MODULE,[],State).

%% This is the synchronous log event.
handle_call({log, Bin}, _From, State) ->
    {Result,State1} = do_log(Bin, call, State),
    %% Result == ok | dropped
    {reply,Result, State1};

handle_call(filesync, _From, State = #{type := Type,
                                       file_ctrl_pid := FileCtrlPid}) ->
    if is_atom(Type) ->
            {reply, ok, State};
       true ->
            {reply, file_ctrl_filesync_sync(FileCtrlPid), State#{last_op=>sync}}
    end;

handle_call({change_config,_OldConfig,NewConfig}, _From,
            State = #{filesync_repeat_interval := FSyncInt0}) ->
    HConfig = maps:get(config, NewConfig, #{}),
    State1 = maps:merge(State, HConfig),
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

handle_call(reset, _From, State) ->
    State1 = ?merge_with_stats(State),
    {reply, ok, State1#{last_qlen => 0,
                        last_log_ts => ?timestamp()}};

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
handle_cast(repeated_filesync,
            State = #{type := Type,
                      file_ctrl_pid := FileCtrlPid,
                      filesync_repeat_interval := FSyncInt,
                      last_op := LastOp}) ->
    State1 =
        if not is_atom(Type), is_integer(FSyncInt) ->
                %% only do filesync if something has been
                %% written since last time we checked
                if LastOp == sync ->
                        ok;
                   true ->
                        file_ctrl_filesync_async(FileCtrlPid)
                end,
                {ok,TRef} =
                    timer:apply_after(FSyncInt, gen_server,cast,
                                      [self(),repeated_filesync]),
                State#{rep_sync_tref => TRef, last_op => sync};
           true ->
                State
        end,
    {noreply,State1}.

handle_info({'EXIT',Pid,Why}, State = #{id := Name, type := FileInfo}) ->
    case maps:get(file_ctrl_pid, State, undefined) of
        Pid ->
            %% file error, terminate handler
            logger_h_common:handler_exit(Name,
                                         {error,{write_failed,FileInfo,Why}});
        _Other ->
            %% ignore EXIT
            ok
    end,
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, State = #{id:=Name, file_ctrl_pid:=FWPid,
                            type:=_FileInfo}) ->
    _ = logger_h_common:cancel_timer(maps:get(rep_sync_tref, State,
                                              undefined)),
    case is_process_alive(FWPid) of
        true ->
            unlink(FWPid),
            _ = file_ctrl_stop(FWPid),
            MRef = erlang:monitor(process, FWPid),
            receive
                {'DOWN',MRef,_,_,_} ->
                    ok
            after
                ?DEFAULT_CALL_TIMEOUT ->
                    exit(FWPid, kill)
            end;
        false ->
            ok
    end,
    unregister(?name_to_reg_name(?MODULE, Name)),
    logger_h_common:stop_or_restart(Name, Reason, State).
                                                  
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

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
       file_ctrl_sync_int          => ?CONTROLLER_SYNC_INTERVAL,
       filesync_ok_qlen            => ?FILESYNC_OK_QLEN,
       filesync_repeat_interval    => ?FILESYNC_REPEAT_INTERVAL}.

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
    LoggerStdH =
        #{id       => Name,
          start    => {?MODULE, start_link, [Name,Config,HandlerState]},
          restart  => temporary,
          shutdown => 2000,
          type     => worker,
          modules  => [?MODULE]},
    case supervisor:start_child(logger_sup, LoggerStdH) of
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
-define(update_file_ctrl_sync(C, Interval),
        if C == 0 -> Interval;
           true -> C-1 end).

%% check for overload between every event (and set Mode to async,
%% sync or drop accordingly), but never flush the whole mailbox
%% before LogWindowSize events have been handled
do_log(Bin, CallOrCast, State = #{id:=Name, mode:=Mode0}) ->
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

%% this clause is called by do_log/3 after an overload check
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

%% this clause is called to write to file
write(_Name, Mode, T1, Bin, _CallOrCast,
      State = #{mode_tab := ModeTab,
                file_ctrl_pid := FileCtrlPid,
                file_ctrl_sync := FileCtrlSync,
                last_qlen := LastQLen,
                last_log_ts := T0,
                file_ctrl_sync_int := FileCtrlSyncInt}) ->
    %% check if we need to limit the number of writes
    %% during a burst of log events
    {DoWrite,BurstWinT,BurstMsgCount} = logger_h_common:limit_burst(State),

    %% only send a synhrounous event to the file controller process
    %% every FileCtrlSyncInt time, to give the handler time between
    %% file writes so it can keep up with incoming messages
    {Result,LastQLen1} =
        if DoWrite, FileCtrlSync == 0 ->
                ?observe(_Name,{_CallOrCast,1}),
                file_write_sync(FileCtrlPid, Bin, false),
                {ok,element(2, process_info(self(), message_queue_len))};
           DoWrite ->
                ?observe(_Name,{_CallOrCast,1}),
                file_write_async(FileCtrlPid, Bin),
                {ok,LastQLen};
           not DoWrite ->
                ?observe(_Name,{flushed,1}),
                {dropped,LastQLen}
        end,
    
    %% Check if the time since the previous log event is long enough -
    %% and the queue length small enough - to assume the mailbox has
    %% been emptied, and if so, do filesync operation and reset mode to
    %% async. Note that this is the best we can do to detect an idle
    %% handler without setting a timer after each log call/cast. If the
    %% time between two consecutive log events is fast and no new
    %% event comes in after the last one, idle state won't be detected!
    Time = ?diff_time(T1,T0),
    {Mode1,BurstMsgCount1} =
        if (LastQLen1 < ?FILESYNC_OK_QLEN) andalso
           (Time > ?IDLE_DETECT_TIME_USEC) ->
                %% do filesync if necessary
                case maps:get(type, State) of
                    Std when is_atom(Std) ->
                        ok;
                    _File ->
                        file_ctrl_filesync_async(FileCtrlPid)
                end,
                {?change_mode(ModeTab, Mode, async),0};
           true ->
                {Mode,BurstMsgCount}
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
                                 file_ctrl_sync =>
                                     ?update_file_ctrl_sync(FileCtrlSync,
                                                            FileCtrlSyncInt)}),
    {Result,State2}.

open_log_file(HandlerName, FileInfo) ->
    case file_ctrl_start(HandlerName, FileInfo) of
        OK = {ok,_FileCtrlPid} -> OK;
        Error -> Error
    end.

do_open_log_file({file,File}) ->
    do_open_log_file({file,File,[raw,append,delayed_write]});

do_open_log_file({file,File,[]}) ->
    do_open_log_file({file,File,[raw,append,delayed_write]});

do_open_log_file({file,File,Modes}) ->
    try
        case filelib:ensure_dir(File) of
            ok ->
                file:open(File, Modes);
            Error ->
                Error
        end
    catch
        _:Reason -> {error,Reason}
    end.

close_log_file(Std) when Std == standard_io; Std == standard_error ->
    ok;
close_log_file(Fd) ->
    _ = file:datasync(Fd),
    _ = file:close(Fd).


log_handler_info(Name, Format, Args, #{file_ctrl_pid := FileCtrlPid}) ->
    Config =
        case logger:get_handler_config(Name) of
            {ok,Conf} -> Conf;
            _ -> #{formatter=>{?DEFAULT_FORMATTER,?DEFAULT_FORMAT_CONFIG}}
        end,
    Meta = #{time=>erlang:system_time(microsecond)},
    Bin = logger_h_common:log_to_binary(#{level => notice,
                                          msg => {Format,Args},
                                          meta => Meta}, Config),
    _ = file_write_async(FileCtrlPid, Bin),
    ok.

%%%-----------------------------------------------------------------
%%% File control process

file_ctrl_start(HandlerName, FileInfo) ->
    Starter = self(),
    FileCtrlPid =
        spawn_link(fun() ->
                           file_ctrl_init(HandlerName, FileInfo, Starter)
                   end),
    receive
        {FileCtrlPid,ok} ->
            {ok,FileCtrlPid};
        {FileCtrlPid,Error} ->
            Error
    after
        ?DEFAULT_CALL_TIMEOUT ->
            {error,file_ctrl_process_not_started}
    end.

file_ctrl_stop(Pid) ->
    Pid ! stop.

file_write_async(Pid, Bin) ->
    Pid ! {log,Bin},
    ok.

file_write_sync(Pid, Bin, FileSync) ->
    case file_ctrl_call(Pid, {log,self(),Bin,FileSync}) of
        {error,Reason} ->
            {error,{write_failed,Bin,Reason}};
        Result ->
            Result
    end.

file_ctrl_filesync_async(Pid) ->
    Pid ! filesync,
    ok.

file_ctrl_filesync_sync(Pid) ->
    file_ctrl_call(Pid, {filesync,self()}).

file_ctrl_call(Pid, Msg) ->
    MRef = monitor(process, Pid),
    Pid ! {Msg,MRef},
    receive
        {MRef,Result} ->
            demonitor(MRef, [flush]),
            Result;
        {'DOWN',MRef,_Type,_Object,Reason} ->
            {error,Reason}
    after
        ?DEFAULT_CALL_TIMEOUT ->
            {error,{no_response,Pid}}
    end.    

file_ctrl_init(HandlerName, FileInfo, Starter) when is_tuple(FileInfo) ->
    process_flag(message_queue_data, off_heap),
    FileName = element(2, FileInfo),
    case do_open_log_file(FileInfo) of
        {ok,Fd} ->
            Starter ! {self(),ok},
            file_ctrl_loop(Fd, file, FileName, false, ok, ok, HandlerName);
        {error,Reason} ->
            Starter ! {self(),{error,{open_failed,FileName,Reason}}}
    end;
file_ctrl_init(HandlerName, StdDev, Starter) ->
    Starter ! {self(),ok},
    file_ctrl_loop(StdDev, standard_io, StdDev, false, ok, ok, HandlerName).

file_ctrl_loop(Fd, Type, DevName, Synced,
               PrevWriteResult, PrevSyncResult, HandlerName) ->
    receive
        %% asynchronous event
        {log,Bin} ->
            Result = if Type == file ->
                             write_to_dev(Fd, Bin, DevName,
                                          PrevWriteResult, HandlerName);
                        true ->
                             io:put_chars(Fd, Bin)
                     end,
            file_ctrl_loop(Fd, Type, DevName, false,
                           Result, PrevSyncResult, HandlerName);

        %% synchronous event
        {{log,From,Bin,FileSync},MRef} ->
            if Type == file ->
                    %% check that file hasn't been deleted
                    CheckFile =
                        fun() -> {ok,_} = file:read_file_info(DevName) end,
                    spawn_link(CheckFile),
                    WResult = write_to_dev(Fd, Bin, DevName,
                                           PrevWriteResult, HandlerName),
                    {Synced1,SResult} =
                        if not FileSync ->
                                {false,PrevSyncResult};
                           true ->
                                case sync_dev(Fd, DevName,
                                              PrevSyncResult, HandlerName) of
                                    ok     -> {true,ok};
                                    Error  -> {false,Error}
                                end
                        end,
                    From ! {MRef,ok},
                    file_ctrl_loop(Fd, Type, DevName, Synced1,
                                   WResult, SResult, HandlerName);
               true ->
                    _ = io:put_chars(Fd, Bin),
                    From ! {MRef,ok},
                    file_ctrl_loop(Fd, Type, DevName, false,
                                   ok, PrevSyncResult, HandlerName)
            end;

        filesync when not Synced ->
            Result = sync_dev(Fd, DevName, PrevSyncResult, HandlerName),
            file_ctrl_loop(Fd, Type, DevName, true,
                           PrevWriteResult, Result, HandlerName);

        filesync ->
            file_ctrl_loop(Fd, Type, DevName, true,
                           PrevWriteResult, PrevSyncResult, HandlerName);

        {{filesync,From},MRef} ->
            Result = if not Synced ->
                             sync_dev(Fd, DevName, PrevSyncResult, HandlerName);
                        true ->
                             ok
                     end,
            From ! {MRef,ok},
            file_ctrl_loop(Fd, Type, DevName, true,
                           PrevWriteResult, Result, HandlerName);

        stop ->
            _ = close_log_file(Fd),
            stopped
    end.

write_to_dev(Fd, Bin, FileName, PrevWriteResult, HandlerName) ->
    case ?file_write(Fd, Bin) of
        ok ->
            ok;
        PrevWriteResult ->
            %% don't report same error twice
            PrevWriteResult;
        Error ->
            logger_h_common:error_notify({HandlerName,write,FileName,Error}),
            Error
    end.

sync_dev(Fd, DevName, PrevSyncResult, HandlerName) ->
    case ?file_datasync(Fd) of
        ok ->
            ok;
        PrevSyncResult ->
            %% don't report same error twice
            PrevSyncResult;
        Error ->
            logger_h_common:error_notify({HandlerName,filesync,DevName,Error}),
            Error
    end.

