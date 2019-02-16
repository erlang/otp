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

-include("logger.hrl").
-include("logger_internal.hrl").
-include("logger_h_common.hrl").

%%% API
-export([filesync/1]).

%% logger_h_common callbacks
-export([init/2, check_config/4, reset_state/2,
         filesync/3, write/4, handle_info/3, terminate/3]).

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
init(Name, #{file:=File,type:=Type,max_no_bytes:=MNB,max_no_files:=MNF}) ->
    case open_disk_log(Name, File, Type, MNB, MNF) of
        ok ->
            {ok,#{log_opts => #{file => File,
                                type => Type,
                                max_no_bytes => MNB,
                                max_no_files => MNF},
                  prev_log_result => ok,
                  prev_sync_result => ok,
                  prev_disk_log_info => undefined}};
        Error ->
            Error
    end.

check_config(Name,set,undefined,HConfig0) ->
    HConfig=merge_default_logopts(Name,maps:merge(get_default_config(),HConfig0)),
    check_config(HConfig);
check_config(_Name,SetOrUpdate,OldHConfig,NewHConfig0) ->
    WriteOnce = maps:with([type,file,max_no_files,max_no_bytes],OldHConfig),
    Default =
        case SetOrUpdate of
            set ->
                %% Do not reset write-once fields to defaults
                maps:merge(get_default_config(),WriteOnce);
            update ->
                OldHConfig
        end,

    NewHConfig = maps:merge(Default,NewHConfig0),

    %% Fail if write-once fields are changed
    case maps:with([type,file,max_no_files,max_no_bytes],NewHConfig) of
        WriteOnce ->
            check_config(NewHConfig);
        Other ->
            {Old,New} = logger_server:diff_maps(WriteOnce,Other),
            {error,{illegal_config_change,?MODULE,Old,New}}
    end.

check_config(HConfig) ->
    case check_h_config(maps:to_list(HConfig)) of
        ok ->
            {ok,HConfig};
        {error,{Key,Value}} ->
            {error,{invalid_config,?MODULE,#{Key=>Value}}}
    end.

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
check_h_config([Other | _]) ->
    {error,Other};
check_h_config([]) ->
    ok.

get_default_config() ->
     #{}.

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

filesync(Name,_Mode,State) ->
    Result = ?disk_log_sync(Name),
    maybe_notify_error(Name, filesync, Result, prev_sync_result, State).

write(Name, Mode, Bin, State) ->
    Result = ?disk_log_write(Name, Mode, Bin),
    maybe_notify_error(Name, log, Result, prev_log_result, State).

reset_state(_Name, State) ->
    State#{prev_log_result => ok,
           prev_sync_result => ok,
           prev_disk_log_info => undefined}.

%% The disk log owner must handle status messages from disk_log.
handle_info(Name, {disk_log, _Node, Log, Info={truncated,_NoLostItems}}, State) ->
    maybe_notify_status(Name, Log, Info, prev_disk_log_info, State);
handle_info(Name, {disk_log, _Node, Log, Info = {blocked_log,_Items}}, State) ->
    maybe_notify_status(Name, Log, Info, prev_disk_log_info, State);
handle_info(Name, {disk_log, _Node, Log, Info = full}, State) ->
    maybe_notify_status(Name, Log, Info, prev_disk_log_info, State);
handle_info(Name, {disk_log, _Node, Log, Info = {error_status,_Status}}, State) ->
    maybe_notify_status(Name, Log, Info, prev_disk_log_info, State);
handle_info(_, _, State) ->
    State.

terminate(Name, _Reason, _State) ->
    _ = close_disk_log(Name, normal),
    ok.

%%%-----------------------------------------------------------------
%%% Internal functions
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

disk_log_write(Name, sync, Bin) ->
    disk_log:blog(Name, Bin);
disk_log_write(Name, async, Bin) ->
    disk_log:balog(Name, Bin).

%%%-----------------------------------------------------------------
%%% Print error messages, but don't repeat the same message
maybe_notify_error(Name, Op, Result, Key, #{log_opts:=LogOpts}=State) ->
    {Result,error_notify_new({Name, Op, LogOpts, Result}, Result, Key, State)}.

maybe_notify_status(Name, Log, Info, Key, State) ->
    error_notify_new({disk_log, Name, Log, Info}, Info, Key, State).

error_notify_new(Term, What, Key, State) ->
    error_notify_new(What, maps:get(Key,State), Term),
    State#{Key => What}.

error_notify_new(ok,_Prev,_Term) ->
    ok;
error_notify_new(Same,Same,_Term) ->
    ok;
error_notify_new(_New,_Prev,Term) ->
    logger_h_common:error_notify(Term).
