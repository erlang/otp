%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2017-2025. All Rights Reserved.
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
-moduledoc """
A disk_log based handler for Logger

This is a handler for Logger that offers circular (wrapped) logs by using
`m:disk_log`. Multiple instances of this handler can be added to Logger, and
each instance prints to its own disk log file, created with the name and
settings specified in the handler configuration.

The default standard handler, `m:logger_std_h`, can be replaced by a disk_log
handler at startup of the Kernel application. See an example of this below.

The handler has an overload protection mechanism that keeps the handler process
and the Kernel application alive during high loads of log events. How overload
protection works, and how to configure it, is described in the
[`User's Guide`](logger_chapter.md#overload_protection).

To add a new instance of the disk_log handler, use
[`logger:add_handler/3`](`logger:add_handler/3`). The handler configuration
argument is a map which can contain general configuration parameters, as
documented in the [`User's Guide`](logger_chapter.md#handler-configuration),
and handler specific parameters. The specific data is stored in a sub map with
the key `config`, and can contain the following parameters:

- **`file`** - This is the full name of the disk log file. The option
  corresponds to the `name` property in the [`dlog_option()`](`disk_log:open/1`)
  datatype.

  The value is set when the handler is added, and it cannot be changed in
  runtime.

  Defaults to the same name as the handler identity, in the current directory.

- **`type`** - This is the disk log type, `wrap` or `halt`. The option
  corresponds to the `type` property in the [`dlog_option()`](`disk_log:open/1`)
  datatype.

  The value is set when the handler is added, and it cannot be changed in
  runtime.

  Defaults to `wrap`.

- **`max_no_files`** - This is the maximum number of files that disk_log uses
  for its circular logging. The option corresponds to the `MaxNoFiles` element
  in the `size` property in the [`dlog_option()`](`disk_log:open/1`) datatype.

  The value is set when the handler is added, and it cannot be changed in
  runtime.

  Defaults to `10`.

  The setting has no effect on a halt log.

- **`max_no_bytes`** - This is the maximum number of bytes that is written to a
  log file before disk_log proceeds with the next file in order, or generates an
  error in case of a full halt log. The option corresponds to the `MaxNoBytes`
  element in the `size` property in the [`dlog_option()`](`disk_log:open/1`)
  datatype.

  The value is set when the handler is added, and it cannot be changed in
  runtime.

  Defaults to `1048576` bytes for a wrap log, and `infinity` for a halt log.

- **`filesync_repeat_interval`** - This value, in milliseconds, specifies how
  often the handler does a disk_log sync operation to write buffered data to
  disk. The handler attempts the operation repeatedly, but only performs a new
  sync if something has actually been logged.

  Defaults to `5000` milliseconds.

  If `no_repeat` is set as value, the repeated sync operation is disabled. The
  user can also call the [`filesync/1`](`filesync/1`) function to perform a
  disk_log sync.

Other configuration parameters exist, to be used for customizing the overload
protection behaviour. The same parameters are used both in the standard handler
and the disk_log handler, and are documented in the
[`User's Guide`](logger_chapter.md#overload_protection).

Notice that when changing the configuration of the handler in runtime, the
disk_log options (`file`, `type`, `max_no_files`, `max_no_bytes`) must not be
modified.

Example of adding a disk_log handler:

```erlang
logger:add_handler(my_disk_log_h, logger_disk_log_h,
                   #{config => #{file => "./my_disk_log",
                                 type => wrap,
                                 max_no_files => 4,
                                 max_no_bytes => 10000,
                                 filesync_repeat_interval => 1000}}).
```

To use the disk_log handler instead of the default standard handler when
starting an Erlang node, change the Kernel default logger to use
`logger_disk_log_h`. Example:

```text
erl -kernel logger '[{handler,default,logger_disk_log_h,
                      #{config => #{file => "./system_disk_log"}}}]'
```

## See Also

`m:logger`, `m:logger_std_h`, `m:disk_log`
""".
-moduledoc(#{since => "OTP 21.0"}).

-include("logger.hrl").
-include("logger_internal.hrl").
-include("logger_h_common.hrl").

%%% API
-export([filesync/1]).

-behaviour(logger_handler).

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
-doc "Write buffered data to disk.".
-doc(#{since => <<"OTP 21.0">>}).
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
-doc false.
adding_handler(Config) ->
    logger_h_common:adding_handler(Config).

%%%-----------------------------------------------------------------
%%% Updating handler config
-doc false.
changing_config(SetOrUpdate,OldConfig,NewConfig) ->
    logger_h_common:changing_config(SetOrUpdate,OldConfig,NewConfig).

%%%-----------------------------------------------------------------
%%% Handler being removed
-doc false.
removing_handler(Config) ->
    logger_h_common:removing_handler(Config).

%%%-----------------------------------------------------------------
%%% Log a string or report
-doc false.
-spec log(LogEvent,Config) -> ok when
      LogEvent :: logger:log_event(),
      Config :: logger_handler:config().

log(LogEvent,Config) ->
    logger_h_common:log(LogEvent,Config).

%%%-----------------------------------------------------------------
%%% Remove internal fields from configuration
-doc false.
filter_config(Config) ->
    logger_h_common:filter_config(Config).

%%%===================================================================
%%% logger_h_common callbacks
%%%===================================================================
-doc false.
init(Name, #{file:=File,type:=Type,max_no_bytes:=MNB,max_no_files:=MNF}) ->
    case open_disk_log(Name, File, Type, MNB, MNF) of
        ok ->
            {ok,#{log_opts=>#{file=>File,
                              type=>Type,
                              max_no_bytes=>MNB,
                              max_no_files=>MNF},
                  prev_log_result=>ok,
                  prev_sync_result=>ok,
                  prev_disk_log_info=>undefined}};
        Error ->
            Error
    end.

-doc false.
check_config(Name,set,undefined,HConfig0) ->
    HConfig=merge_default_logopts(Name,HConfig0),
    check_config(HConfig);
check_config(_Name,SetOrUpdate,OldHConfig,NewHConfig0) ->
    WriteOnce = maps:with([type,file,max_no_files,max_no_bytes],OldHConfig),
    Default =
        case SetOrUpdate of
            set ->
                %% Do not reset write-once fields to defaults
                WriteOnce;
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

merge_default_logopts(Name,HConfig) ->
    Type = maps:get(type,HConfig,wrap),
    {DefaultNoFiles,DefaultNoBytes} =
        case Type of
            halt -> {undefined,infinity};
            _wrap -> {10,1048576}
        end,
    {ok,Dir} = file:get_cwd(),
    Defaults = #{file=>filename:join(Dir,Name),
                 max_no_files=>DefaultNoFiles,
                 max_no_bytes=>DefaultNoBytes,
                 type=>Type},
    maps:merge(Defaults,HConfig).

-doc false.
filesync(Name,_Mode,State) ->
    Result = ?disk_log_sync(Name),
    maybe_notify_error(Name,filesync,Result,prev_sync_result,State).

-doc false.
write(Name,Mode,Bin,State) ->
    Result = ?disk_log_write(Name,Mode,Bin),
    maybe_notify_error(Name,log,Result,prev_log_result,State).

-doc false.
reset_state(_Name,State) ->
    State#{prev_log_result=>ok,
           prev_sync_result=>ok,
           prev_disk_log_info=>undefined}.

%% The disk log owner must handle status messages from disk_log.
-doc false.
handle_info(Name,{disk_log,_Node,Log,Info={truncated,_NoLostItems}},State) ->
    maybe_notify_status(Name,Log,Info,prev_disk_log_info,State);
handle_info(Name,{disk_log,_Node,Log,Info={blocked_log,_Items}},State) ->
    maybe_notify_status(Name,Log,Info,prev_disk_log_info,State);
handle_info(Name,{disk_log,_Node,Log,Info=full}, State) ->
    maybe_notify_status(Name,Log,Info,prev_disk_log_info,State);
handle_info(Name,{disk_log,_Node,Log,Info={error_status,_Status}},State) ->
    maybe_notify_status(Name,Log,Info,prev_disk_log_info,State);
handle_info(_,_,State) ->
    State.

-doc false.
terminate(Name,_Reason,_State) ->
    _ = close_disk_log(Name,normal),
    ok.

%%%-----------------------------------------------------------------
%%% Internal functions
open_disk_log(Name,File,Type,MaxNoBytes,MaxNoFiles) ->
    case filelib:ensure_dir(File) of
        ok ->
            Size =
                if Type==halt -> MaxNoBytes;
                   Type==wrap -> {MaxNoBytes,MaxNoFiles}
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

close_disk_log(Name,_) ->
    _ = ?disk_log_sync(Name),
    _ = disk_log:close(Name),
    ok.

disk_log_write(Name,sync,Bin) ->
    disk_log:blog(Name,Bin);
disk_log_write(Name,async,Bin) ->
    disk_log:balog(Name,Bin).

%%%-----------------------------------------------------------------
%%% Print error messages, but don't repeat the same message
maybe_notify_error(Name,Op,Result,Key,#{log_opts:=LogOpts}=State) ->
    {Result,error_notify_new({Name,Op,LogOpts,Result},Result,Key,State)}.

maybe_notify_status(Name,Log,Info,Key,State) ->
    error_notify_new({disk_log,Name,Log,Info},Info,Key,State).

error_notify_new(Term,What,Key,State) ->
    error_notify_new(What,maps:get(Key,State),Term),
    State#{Key=>What}.

error_notify_new(ok,_Prev,_Term) ->
    ok;
error_notify_new(Same,Same,_Term) ->
    ok;
error_notify_new(_New,_Prev,Term) ->
    logger_h_common:error_notify(Term).
