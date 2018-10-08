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
-export([info/1, filesync/1, reset/1]).

%% logger_h_common callbacks
-export([init/2, check_config/4, reset_state/1,
         async_filesync/2, sync_filesync/2,
         async_write/3, sync_write/3,
         handle_info/2, terminate/3]).

%% logger callbacks
-export([log/2, adding_handler/1, removing_handler/1, changing_config/3,
         filter_config/1]).

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

%%%-----------------------------------------------------------------
%%%
-spec info(Name) -> Info | {error,Reason} when
      Name :: atom(),
      Info :: term(),
      Reason :: handler_busy | {badarg,term()}.

info(Name) ->
    logger_h_common:info(?MODULE,Name).

%%%-----------------------------------------------------------------
%%%
-spec reset(Name) -> ok | {error,Reason} when
      Name :: atom(),
      Reason :: handler_busy | {badarg,term()}.

reset(Name) ->
    logger_h_common:reset(?MODULE,Name).

%%%===================================================================
%%% logger callbacks
%%%===================================================================

%%%-----------------------------------------------------------------
%%% Handler being added
adding_handler(Config) ->
    logger_h_common:adding_handler(Config).

%%%-----------------------------------------------------------------
%%% Updating handler config
changing_config(SetOrUpdate, OldConfig, NewConfig) ->
    logger_h_common:changing_config(SetOrUpdate, OldConfig, NewConfig).

%%%-----------------------------------------------------------------
%%% Handler being removed
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

async_filesync(_Name,#{type := Type}=State) when is_atom(Type) ->
    State;
async_filesync(_Name,#{file_ctrl_pid := FileCtrlPid}=State) ->
    ok = file_ctrl_filesync_async(FileCtrlPid),
    State.

sync_filesync(_Name,#{type := Type}=State) when is_atom(Type) ->
    {ok,State};
sync_filesync(_Name,#{file_ctrl_pid := FileCtrlPid}=State) ->
    Result = file_ctrl_filesync_sync(FileCtrlPid),
    {Result,State}.

async_write(_Name, Bin, #{file_ctrl_pid:=FileCtrlPid} = State) ->
    ok = file_write_async(FileCtrlPid, Bin),
    State.

sync_write(_Name, Bin, #{file_ctrl_pid:=FileCtrlPid} = State) ->
    Result = file_write_sync(FileCtrlPid, Bin),
    {Result,State}.

reset_state(State) ->
    State.

handle_info({'EXIT',Pid,Why}, #{type := FileInfo, file_ctrl_pid := Pid}) ->
    %% file_ctrl_pid died, file error, terminate handler
    exit({error,{write_failed,FileInfo,Why}});
handle_info(_, State) ->
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
                    exit(FWPid, kill)
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

file_write_sync(Pid, Bin) ->
    file_ctrl_call(Pid, {log,self(),Bin}).

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
        {{log,From,Bin},MRef} ->
            WResult =
                if Type == file ->
                        %% check that file hasn't been deleted
                        CheckFile =
                            fun() -> {ok,_} = file:read_file_info(DevName) end,
                        spawn_link(CheckFile),
                        write_to_dev(Fd, Bin, DevName,
                                     PrevWriteResult, HandlerName);
                   true ->
                        _ = io:put_chars(Fd, Bin),
                        ok
                end,
            From ! {MRef,ok},
            file_ctrl_loop(Fd, Type, DevName, false,
                           WResult, PrevSyncResult, HandlerName);

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
