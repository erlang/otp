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
-export([log/2, adding_handler/2, removing_handler/2,
         changing_config/3, swap_buffer/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%%-----------------------------------------------------------------
%%% Start a standard handler process and link to caller.
%%% This function is called by the kernel supervisor when this
%%% handler process gets added
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
-spec filesync(Name) -> ok | {error,Reason} when
      Name :: atom(),
      Reason :: handler_busy | {badarg,term()}.

filesync(Name) when is_atom(Name) ->
    try
        gen_server:call(Name, filesync, ?DEFAULT_CALL_TIMEOUT)
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
                OldConfig=#{id:=Id, ?MODULE:=#{type:=Type}},
                NewConfig=#{id:=Id}) ->
    MyConfig = maps:get(?MODULE, NewConfig, #{}),
    case maps:get(type, MyConfig, Type) of
        Type ->
            MyConfig1 = MyConfig#{type=>Type},
            changing_config1(Name, OldConfig,
                             NewConfig#{?MODULE=>MyConfig1});
        _ ->
            {error,{illegal_config_change,OldConfig,NewConfig}}
    end;
changing_config(_Name, OldConfig, NewConfig) ->
    {error,{illegal_config_change,OldConfig,NewConfig}}.

changing_config1(Name, OldConfig, NewConfig) ->
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
    end.    

check_config(adding, Name, Config0) ->
    %% Merge in defaults on top level
    Config = maps:merge(#{id => Name}, Config0),
    %% Merge in defaults on handler level
    MyConfig0 = maps:get(?MODULE, Config, #{}),
    MyConfig = maps:merge(#{type => standard_io},
                          MyConfig0),
    case check_my_config(maps:to_list(MyConfig)) of
        ok ->
            {ok,Config#{?MODULE=>MyConfig}};
        Error ->
            Error
    end;
check_config(changing, _Name, Config) ->
    MyConfig = maps:get(?MODULE, Config, #{}),
    case check_my_config(maps:to_list(MyConfig)) of
        ok    -> {ok,Config};
        Error -> Error
    end.

check_my_config([{type,Type} | Config]) when Type == standard_io;
                                             Type == standard_error ->
    check_my_config(Config);
check_my_config([{type,{file,File}} | Config]) when is_list(File) ->
    check_my_config(Config);
check_my_config([{type,{file,File,Modes}} | Config]) when is_list(File),
                                                          is_list(Modes) ->
    check_my_config(Config);
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
removing_handler(Name,_Config) ->
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

init([Name, Config,
      State0 = #{type := Type, file_ctrl_sync_int := FileCtrlSyncInt}]) ->
    register(Name, self()),
    process_flag(trap_exit, true),
    process_flag(message_queue_data, off_heap),

    ?init_test_hooks(),
    ?start_observation(Name),

    case do_init(Name, Type) of
        {ok,InitState} ->
            catch ets:new(Name, [public, named_table]),
            ?set_mode(Name, async),
            State = maps:merge(State0, InitState),
            T0 = ?timestamp(),
            State1 =
                ?merge_with_stats(State#{mode => async,
                                         file_ctrl_sync => FileCtrlSyncInt,
                                         last_qlen => 0,
                                         last_log_ts => T0,
                                         burst_win_ts => T0,
                                         burst_msg_count => 0}),
            proc_lib:init_ack({ok,self()}),
            gen_server:cast(self(), {repeated_filesync,T0}),
            enter_loop(Config, State1);
        Error ->
            logger_h_common:error_notify({init_handler,Name,Error}),
            proc_lib:init_ack(Error)
    end.

do_init(Name, Std) when Std=:=standard_io; Std=:=standard_error ->
    case open_log_file(Name, Std) of
        {ok,FileCtrlPid} ->
            {ok,#{id=>Name,type=>Std,file_ctrl_pid=>FileCtrlPid}};
        Error ->
            Error
    end;
do_init(Name, FileInfo) when is_tuple(FileInfo) ->
    case open_log_file(Name, FileInfo) of
        {ok,FileCtrlPid} ->
            {ok,#{id=>Name,type=>FileInfo,file_ctrl_pid=>FileCtrlPid}};
        Error ->
            Error
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
    {reply,Result, State1};

handle_call(filesync, _From, State = #{type := Type,
                                       file_ctrl_pid := FileCtrlPid}) ->
    if is_atom(Type) ->
            {reply, ok, State};
       true ->
            {reply, file_ctrl_filesync_sync(FileCtrlPid), State}
    end;

handle_call({change_config,_OldConfig,NewConfig}, _From,
            State = #{filesync_repeat_interval := FSyncInt0,
                      last_log_ts := LastLogTS}) ->
    HConfig = maps:get(?MODULE, NewConfig, #{}),
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
                        gen_server:cast(self(), {repeated_filesync,
                                                 LastLogTS})
                end,
            {reply, ok, State1};
        false ->
            #{toggle_sync_qlen   := TSQL,
              drop_new_reqs_qlen := DNRQL,
              flush_reqs_qlen    := FRQL} = State1,
            {reply, {error,{invalid_levels,{TSQL,DNRQL,FRQL}}}, State}
    end;

handle_call(info, _From, State) ->
    {reply, State, State};

handle_call(reset, _From, State) ->
    State1 = ?merge_with_stats(State),
    {reply, ok, State1#{last_qlen => 0,
                        last_log_ts => ?timestamp()}};

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
handle_cast({repeated_filesync,LastLogTS0},
            State = #{type := Type,
                      file_ctrl_pid := FileCtrlPid,
                      filesync_repeat_interval := FSyncInt,
                      last_log_ts := LastLogTS1}) ->
    State1 =
        if not is_atom(Type), is_integer(FSyncInt) ->
                %% only do filesync if something has been
                %% written since last time we checked
                if LastLogTS1 == LastLogTS0 ->
                        ok;
                   true ->
                        file_ctrl_filesync_async(FileCtrlPid)
                end,
                {ok,TRef} =
                    timer:apply_after(FSyncInt, gen_server,cast,
                                      [self(),{repeated_filesync,LastLogTS1}]),
                State#{rep_sync_tref => TRef};
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
    logger_h_common:stop_or_restart(Name, Reason, State).
                                                  
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

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
       file_ctrl_sync_int         => ?CONTROLLER_SYNC_INTERVAL,
       filesync_ok_qlen           => ?FILESYNC_OK_QLEN,
       filesync_repeat_interval   => ?FILESYNC_REPEAT_INTERVAL}.

%%%-----------------------------------------------------------------
%%% Add a standard handler to the logger.
%%% This starts a dedicated handler process which should always
%%% exist if the handler is registered with logger (and should not
%%% exist if the handler is not registered).
%%%
%%% Handler specific config should be provided with a sub map associated
%%% with a key named the same as this module, e.g:
%%%
%%% Config = #{logger_std_h => #{toggle_sync_qlen => 50}
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
        {ok,_Pid} ->
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
-define(update_file_ctrl_sync(C, Interval),
        if C == 0 -> Interval;
           true -> C-1 end).

%% check for overload between every request (and set Mode to async,
%% sync or drop accordingly), but never flush the whole mailbox
%% before LogWindowSize requests have been handled
do_log(Bin, CallOrCast, State = #{id:=Name}) ->
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

%% this clause is called by do_log/3 after an overload check
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

%% this clause is called to write to file
write(Name, Mode, T1, Bin, _CallOrCast,
      State = #{file_ctrl_pid := FileCtrlPid,
                file_ctrl_sync := FileCtrlSync,
                last_qlen := LastQLen,
                last_log_ts := T0,
                file_ctrl_sync_int := FileCtrlSyncInt}) ->
    %% check if we need to limit the number of writes
    %% during a burst of log requests
    {DoWrite,BurstWinT,BurstMsgCount} = logger_h_common:limit_burst(State),

    %% only send a synhrounous request to the file controller process
    %% every FileCtrlSyncInt time, to give the handler time between
    %% file writes so it can keep up with incoming messages
    {Result,LastQLen1} =
        if DoWrite, FileCtrlSync == 0 ->
                ?observe(Name,{_CallOrCast,1}),
                file_write_sync(FileCtrlPid, Bin, false),
                {ok,element(2, process_info(self(), message_queue_len))};
           DoWrite ->
                ?observe(Name,{_CallOrCast,1}),
                file_write_async(FileCtrlPid, Bin),
                {ok,LastQLen};
           not DoWrite ->
                ?observe(Name,{flushed,1}),
                {dropped,LastQLen}
        end,
    
    %% Check if the time since the previous log request is long enough -
    %% and the queue length small enough - to assume the mailbox has
    %% been emptied, and if so, do filesync operation and reset mode to
    %% async. Note that this is the best we can do to detect an idle
    %% handler without setting a timer after each log call/cast. If the
    %% time between two consecutive log requests is fast and no new
    %% request comes in after the last one, idle state won't be detected!
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
                {?change_mode(Name, Mode, async),0};
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
        %% asynchronous request
        {log,Bin} ->
            Result = if Type == file ->
                             write_to_dev(Fd, Bin, DevName,
                                          PrevWriteResult, HandlerName);
                        true ->
                             io:put_chars(Fd, Bin)
                     end,
            file_ctrl_loop(Fd, Type, DevName, false,
                           Result, PrevSyncResult, HandlerName);

        %% synchronous request
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

