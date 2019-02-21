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

-include("logger.hrl").
-include("logger_internal.hrl").
-include("logger_h_common.hrl").

-include_lib("kernel/include/file.hrl").

%% API
-export([filesync/1]).

%% logger_h_common callbacks
-export([init/2, check_config/4, reset_state/2,
         filesync/3, write/4, handle_info/3, terminate/3]).

%% logger callbacks
-export([log/2, adding_handler/1, removing_handler/1, changing_config/3,
         filter_config/1]).

-define(DEFAULT_CALL_TIMEOUT, 5000).

%%%===================================================================
%%% API
%%%===================================================================

%%%-----------------------------------------------------------------
%%%
-spec filesync(Name) -> ok | {error,Reason} when
      Name :: atom(),
      Reason :: handler_busy | {badarg,term()}.

filesync(Name) ->
    logger_h_common:filesync(?MODULE,Name).

%%%===================================================================
%%% logger callbacks - just forward to logger_h_common
%%%===================================================================

%%%-----------------------------------------------------------------
%%% Handler being added
-spec adding_handler(Config) -> {ok,Config} | {error,Reason} when
      Config :: logger:handler_config(),
      Reason :: term().

adding_handler(Config) ->
    logger_h_common:adding_handler(Config).

%%%-----------------------------------------------------------------
%%% Updating handler config
-spec changing_config(SetOrUpdate, OldConfig, NewConfig) ->
                              {ok,Config} | {error,Reason} when
      SetOrUpdate :: set | update,
      OldConfig :: logger:handler_config(),
      NewConfig :: logger:handler_config(),
      Config :: logger:handler_config(),
      Reason :: term().

changing_config(SetOrUpdate, OldConfig, NewConfig) ->
    logger_h_common:changing_config(SetOrUpdate, OldConfig, NewConfig).

%%%-----------------------------------------------------------------
%%% Handler being removed
-spec removing_handler(Config) -> ok when
      Config :: logger:handler_config().

removing_handler(Config) ->
    logger_h_common:removing_handler(Config).

%%%-----------------------------------------------------------------
%%% Log a string or report
-spec log(LogEvent, Config) -> ok when
      LogEvent :: logger:log_event(),
      Config :: logger:handler_config().

log(LogEvent, Config) ->
    logger_h_common:log(LogEvent, Config).

%%%-----------------------------------------------------------------
%%% Remove internal fields from configuration
-spec filter_config(Config) -> Config when
      Config :: logger:handler_config().

filter_config(Config) ->
    logger_h_common:filter_config(Config).

%%%===================================================================
%%% logger_h_common callbacks
%%%===================================================================
init(Name, #{type := Type}) ->
    case open_log_file(Name, Type) of
        {ok,FileCtrlPid} ->
            {ok,#{type=>Type,file_ctrl_pid=>FileCtrlPid}};
        Error ->
            Error
    end.

check_config(_Name,set,undefined,NewHConfig) ->
    check_config(maps:merge(get_default_config(),NewHConfig));
check_config(_Name,SetOrUpdate,OldHConfig,NewHConfig0) ->
    WriteOnce = maps:with([type],OldHConfig),
    Default =
        case SetOrUpdate of
            set ->
                %% Do not reset write-once fields to defaults
                maps:merge(get_default_config(),WriteOnce);
            update ->
                OldHConfig
        end,

    NewHConfig = maps:merge(Default, NewHConfig0),

    %% Fail if write-once fields are changed
    case maps:with([type],NewHConfig) of
        WriteOnce ->
            check_config(NewHConfig);
        Other ->
            {error,{illegal_config_change,?MODULE,WriteOnce,Other}}
    end.

check_config(#{type:=Type}=HConfig) ->
    case check_h_config(maps:to_list(HConfig)) of
        ok when is_atom(Type) ->
            {ok,HConfig#{filesync_repeat_interval=>no_repeat}};
        ok ->
            {ok,HConfig};
        {error,{Key,Value}} ->
            {error,{invalid_config,?MODULE,#{Key=>Value}}}
    end.

check_h_config([{type,Type} | Config]) when Type == standard_io;
                                            Type == standard_error ->
    check_h_config(Config);
check_h_config([{type,{file,File}} | Config]) when is_list(File) ->
    check_h_config(Config);
check_h_config([{type,{file,File,Modes}} | Config]) when is_list(File),
                                                         is_list(Modes) ->
    check_h_config(Config);
check_h_config([Other | _]) ->
    {error,Other};
check_h_config([]) ->
    ok.

get_default_config() ->
     #{type => standard_io}.

filesync(_Name, _Mode, #{type := Type}=State) when is_atom(Type) ->
    {ok,State};
filesync(_Name, async, #{file_ctrl_pid := FileCtrlPid} = State) ->
    ok = file_ctrl_filesync_async(FileCtrlPid),
    {ok,State};
filesync(_Name, sync, #{file_ctrl_pid := FileCtrlPid} = State) ->
    Result = file_ctrl_filesync_sync(FileCtrlPid),
    {Result,State}.

write(_Name, async, Bin, #{file_ctrl_pid:=FileCtrlPid} = State) ->
    ok = file_write_async(FileCtrlPid, Bin),
    {ok,State};
write(_Name, sync, Bin, #{file_ctrl_pid:=FileCtrlPid} = State) ->
    Result = file_write_sync(FileCtrlPid, Bin),
    {Result,State}.

reset_state(_Name, State) ->
    State.

handle_info(_Name, {'EXIT',Pid,Why}, #{type := FileInfo, file_ctrl_pid := Pid}) ->
    %% file_ctrl_pid died, file error, terminate handler
    exit({error,{write_failed,FileInfo,Why}});
handle_info(_, _, State) ->
    State.

terminate(_Name, _Reason, #{file_ctrl_pid:=FWPid}) ->
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
                    exit(FWPid, kill),
                    ok
            end;
        false ->
            ok
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%-----------------------------------------------------------------
%%%
open_log_file(HandlerName, FileInfo) ->
    case file_ctrl_start(HandlerName, FileInfo) of
        OK = {ok,_FileCtrlPid} -> OK;
        Error -> Error
    end.

do_open_log_file({file,FileName}) ->
    do_open_log_file({file,FileName,[raw,append,delayed_write]});

do_open_log_file({file,FileName,[]}) ->
    do_open_log_file({file,FileName,[raw,append,delayed_write]});

do_open_log_file({file,FileName,Modes}) ->
    try
        case filelib:ensure_dir(FileName) of
            ok ->
                case file:open(FileName, Modes) of
                    {ok, Fd} ->
                        {ok,#file_info{inode=INode}} =
                            file:read_file_info(FileName),
                        {ok, {Fd, INode}};
                    Error ->
                        Error
                end;
            Error ->
                Error
        end
    catch
        _:Reason -> {error,Reason}
    end.

close_log_file(Std) when Std == standard_io; Std == standard_error ->
    ok;
close_log_file({Fd,_}) ->
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

file_write_sync(Pid, Bin) ->
    file_ctrl_call(Pid, {log,Bin}).

file_ctrl_filesync_async(Pid) ->
    Pid ! filesync,
    ok.

file_ctrl_filesync_sync(Pid) ->
    file_ctrl_call(Pid, filesync).

file_ctrl_call(Pid, Msg) ->
    MRef = monitor(process, Pid),
    Pid ! {Msg,{self(),MRef}},
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
        {ok,File} ->
            Starter ! {self(),ok},
            file_ctrl_loop(File, FileName, false, ok, ok, HandlerName);
        {error,Reason} ->
            Starter ! {self(),{error,{open_failed,FileName,Reason}}}
    end;
file_ctrl_init(HandlerName, StdDev, Starter) ->
    Starter ! {self(),ok},
    file_ctrl_loop(StdDev, StdDev, false, ok, ok, HandlerName).

file_ctrl_loop(File, DevName, Synced,
               PrevWriteResult, PrevSyncResult, HandlerName) ->
    receive
        %% asynchronous event
        {log,Bin} ->
            File1 = ensure(File, DevName),
            Result = write_to_dev(File1, Bin, DevName,
                                  PrevWriteResult, HandlerName),
            file_ctrl_loop(File1, DevName, false,
                           Result, PrevSyncResult, HandlerName);

        %% synchronous event
        {{log,Bin},{From,MRef}} ->
            File1 = ensure(File, DevName),
            Result = write_to_dev(File1, Bin, DevName,
                                  PrevWriteResult, HandlerName),
            From ! {MRef,ok},
            file_ctrl_loop(File1, DevName, false,
                           Result, PrevSyncResult, HandlerName);

        filesync ->
            File1 = ensure(File, DevName),
            Result = sync_dev(File1, DevName, Synced,
                              PrevSyncResult, HandlerName),
            file_ctrl_loop(File1, DevName, true,
                           PrevWriteResult, Result, HandlerName);

        {filesync,{From,MRef}} ->
            File1 = ensure(File, DevName),
            Result = sync_dev(File1, DevName, Synced,
                              PrevSyncResult, HandlerName),
            From ! {MRef,ok},
            file_ctrl_loop(File1, DevName, true,
                           PrevWriteResult, Result, HandlerName);

        stop ->
            _ = close_log_file(File),
            stopped
    end.

%% In order to play well with tools like logrotate, we need to be able
%% to re-create the file if it has disappeared (e.g. if rotated by
%% logrotate)
ensure(Fd,DevName) when is_atom(DevName) ->
    Fd;
ensure({Fd,INode},FileName) ->
    case file:read_file_info(FileName) of
        {ok,#file_info{inode=INode}} ->
            {Fd,INode};
        _ ->
            _ = file:close(Fd),
            _ = file:close(Fd), % delayed_write cause close not to close
            case do_open_log_file({file,FileName}) of
                {ok,File} ->
                    File;
                Error ->
                    exit({could_not_reopen_file,Error})
            end
    end.

write_to_dev(DevName, Bin, _DevName, _PrevWriteResult, _HandlerName)
  when is_atom(DevName) ->
    io:put_chars(DevName, Bin);
write_to_dev({Fd,_}, Bin, FileName, PrevWriteResult, HandlerName) ->
    Result = ?file_write(Fd, Bin),
    maybe_notify_error(write,Result,PrevWriteResult,FileName,HandlerName).

sync_dev(_, _FileName, true, PrevSyncResult, _HandlerName) ->
    PrevSyncResult;
sync_dev({Fd,_}, FileName, false, PrevSyncResult, HandlerName) ->
    Result = ?file_datasync(Fd),
    maybe_notify_error(filesync,Result,PrevSyncResult,FileName,HandlerName).

maybe_notify_error(_Op, ok, _PrevResult, _FileName, _HandlerName) ->
    ok;
maybe_notify_error(_Op, PrevResult, PrevResult, _FileName, _HandlerName) ->
    %% don't report same error twice
    PrevResult;
maybe_notify_error(Op, Error, _PrevResult, FileName, HandlerName) ->
    logger_h_common:error_notify({HandlerName,Op,FileName,Error}),
    Error.
