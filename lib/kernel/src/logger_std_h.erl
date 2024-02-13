%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2017-2024. All Rights Reserved.
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
-moduledoc """
Standard handler for Logger.

This is the standard handler for Logger. Multiple instances of this handler can
be added to Logger, and each instance prints logs to
[`standard_io`](`t:io:standard_io/0`),
[`standard_error`](`t:io:standard_error/0`), or to file.

The handler has an overload protection mechanism that keeps the handler process
and the Kernel application alive during high loads of log events. How overload
protection works, and how to configure it, is described in the
[`User's Guide` ](logger_chapter.md#overload_protection).

To add a new instance of the standard handler, use
[`logger:add_handler/3` ](`logger:add_handler/3`). The handler configuration
argument is a map which can contain general configuration parameters, as
documented in the [`User's Guide` ](logger_chapter.md#handler_configuration),
and handler specific parameters. The specific data is stored in a sub map with
the key `config`, and can contain the following parameters:

- **`type = `{: #type
  }`t:io:standard_io/0`` | ``t:io:standard_error/0`` | file | {device, ``t:io:device/0``}`** -
  Specifies the log destination.

  The value is set when the handler is added, and it cannot be changed in
  runtime.

  Defaults to [`standard_io`](`t:io:standard_io/0`), unless parameter
  [`file`](`m:logger_std_h#file`) is given, in which case it defaults to `file`.

- **`file = `{: #file }`t:file:filename/0`** - This specifies the name of the
  log file when the handler is of type `file`.

  The value is set when the handler is added, and it cannot be changed in
  runtime.

  Defaults to the same name as the handler identity, in the current directory.

- **`modes = [`{: #modes }`t:file:mode/0``]`** - This specifies the file modes
  to use when opening the log file, see `file:open/2`. If `modes` are not
  specified, the default list used is `[raw,append,delayed_write]`. If `modes`
  are specified, the list replaces the default modes list with the following
  adjustments:

  - If `raw` is not found in the list, it is added.
  - If none of `write`, `append` or `exclusive` is found in the list, `append`
    is added.
  - If none of `delayed_write` or `{delayed_write,Size,Delay}` is found in the
    list, `delayed_write` is added.

  Log files are always UTF-8 encoded. The encoding cannot be changed by setting
  the mode `{encoding,Encoding}`.

  The value is set when the handler is added, and it cannot be changed in
  runtime.

  Defaults to `[raw,append,delayed_write]`.

- **`max_no_bytes = pos_integer() | infinity`{: #max_no_bytes }** - This
  parameter specifies if the log file should be rotated or not. The value
  `infinity` means the log file will grow indefinitely, while an integer value
  specifies at which file size (bytes) the file is rotated.

  Defaults to `infinity`.

- **`max_no_files = non_neg_integer()`{: #max_no_files }** - This parameter
  specifies the number of rotated log file archives to keep. This has meaning
  only if [`max_no_bytes`](`m:logger_std_h#max_no_bytes`) is set to an integer
  value.

  The log archives are named `FileName.0`, `FileName.1`, ... `FileName.N`, where
  `FileName` is the name of the current log file. `FileName.0` is the newest of
  the archives. The maximum value for `N` is the value of `max_no_files`
  minus 1.

  Notice that setting this value to `0` does not turn off rotation. It only
  specifies that no archives are kept.

  Defaults to `0`.

- **`compress_on_rotate = boolean()`{: #compress_on_rotate }** - This parameter
  specifies if the rotated log file archives shall be compressed or not. If set
  to `true`, all archives are compressed with `gzip`, and renamed to
  `FileName.N.gz`

  `compress_on_rotate` has no meaning if
  [`max_no_bytes`](`m:logger_std_h#max_no_bytes`) has the value `infinity`.

  Defaults to `false`.

- **`file_check = non_neg_integer()`{: #file_check }** - When `logger_std_h`
  logs to a file, it reads the file information of the log file prior to each
  write operation. This is to make sure the file still exists and has the same
  inode as when it was opened. This implies some performance loss, but ensures
  that no log events are lost in the case when the file has been removed or
  renamed by an external actor.

  In order to allow minimizing the performance loss, the `file_check` parameter
  can be set to a positive integer value, `N`. The handler will then skip
  reading the file information prior to writing, as long as no more than `N`
  milliseconds have passed since it was last read.

  Notice that the risk of losing log events grows when the `file_check` value
  grows.

  Defaults to 0.

- **`filesync_repeat_interval = pos_integer() | no_repeat`** - This value, in
  milliseconds, specifies how often the handler does a file sync operation to
  write buffered data to disk. The handler attempts the operation repeatedly,
  but only performs a new sync if something has actually been logged.

  If `no_repeat` is set as value, the repeated file sync operation is disabled,
  and it is the operating system settings that determine how quickly or slowly
  data is written to disk. The user can also call the `filesync/1` function to
  perform a file sync.

  Defaults to `5000` milliseconds.

Other configuration parameters exist, to be used for customizing the overload
protection behaviour. The same parameters are used both in the standard handler
and the disk_log handler, and are documented in the
[`User's Guide` ](logger_chapter.md#overload_protection).

Notice that if changing the configuration of the handler in runtime, the `type`,
`file`, or `modes` parameters must not be modified.

Example of adding a standard handler:

```erlang
logger:add_handler(my_standard_h, logger_std_h,
                   #{config => #{file => "./system_info.log",
                                 filesync_repeat_interval => 1000}}).
```

To set the default handler, that starts initially with the Kernel application,
to log to file instead of [`standard_io`](`t:io:standard_io/0`), change the
Kernel default logger configuration. Example:

```text
erl -kernel logger '[{handler,default,logger_std_h,
                      #{config => #{file => "./log.log"}}}]'
```

An example of how to replace the standard handler with a disk_log handler at
startup is found in the `m:logger_disk_log_h` manual.

## See Also

`m:logger`, `m:logger_disk_log_h`
""".
-moduledoc(#{since => "OTP 21.0"}).

-include("logger.hrl").
-include("logger_internal.hrl").
-include("logger_h_common.hrl").

-include_lib("kernel/include/file.hrl").

%% API
-export([filesync/1]).

-behaviour(logger_handler).

%% logger_h_common callbacks
-export([init/2, check_config/4, config_changed/3, reset_state/2,
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
-doc "Write buffered data to disk.".
-doc(#{since => <<"OTP 21.0">>}).
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
-doc false.
-spec adding_handler(Config) -> {ok,Config} | {error,Reason} when
      Config :: logger_handler:config(),
      Reason :: term().

adding_handler(Config) ->
    logger_h_common:adding_handler(Config).

%%%-----------------------------------------------------------------
%%% Updating handler config
-doc false.
-spec changing_config(SetOrUpdate, OldConfig, NewConfig) ->
                              {ok,Config} | {error,Reason} when
      SetOrUpdate :: set | update,
      OldConfig :: logger_handler:config(),
      NewConfig :: logger_handler:config(),
      Config :: logger_handler:config(),
      Reason :: term().

changing_config(SetOrUpdate, OldConfig, NewConfig) ->
    logger_h_common:changing_config(SetOrUpdate, OldConfig, NewConfig).

%%%-----------------------------------------------------------------
%%% Handler being removed
-doc false.
-spec removing_handler(Config) -> ok when
      Config :: logger_handler:config().

removing_handler(Config) ->
    logger_h_common:removing_handler(Config).

%%%-----------------------------------------------------------------
%%% Log a string or report
-doc false.
-spec log(LogEvent, Config) -> ok when
      LogEvent :: logger:log_event(),
      Config :: logger_handler:config().

log(LogEvent, Config) ->
    logger_h_common:log(LogEvent, Config).

%%%-----------------------------------------------------------------
%%% Remove internal fields from configuration
-doc false.
-spec filter_config(Config) -> Config when
      Config :: logger_handler:config().

filter_config(Config) ->
    logger_h_common:filter_config(Config).

%%%===================================================================
%%% logger_h_common callbacks
%%%===================================================================
-doc false.
init(Name, Config) ->
    MyConfig = maps:with([type,file,modes,file_check,max_no_bytes,
                          max_no_files,compress_on_rotate],Config),
    case file_ctrl_start(Name, MyConfig) of
        {ok,FileCtrlPid} ->
            {ok,MyConfig#{file_ctrl_pid=>FileCtrlPid}};
        Error ->
            Error
    end.

-doc false.
check_config(Name,set,undefined,NewHConfig) ->
    check_h_config(merge_default_config(Name,normalize_config(NewHConfig)));
check_config(Name,SetOrUpdate,OldHConfig,NewHConfig0) ->
    WriteOnce = maps:with([type,file,modes],OldHConfig),
    Default =
        case SetOrUpdate of
            set ->
                %% Do not reset write-once fields to defaults
                merge_default_config(Name,WriteOnce);
            update ->
                OldHConfig
        end,

    NewHConfig = maps:merge(Default, normalize_config(NewHConfig0)),

    %% Fail if write-once fields are changed
    case maps:with([type,file,modes],NewHConfig) of
        WriteOnce ->
            check_h_config(NewHConfig);
        Other ->
            {error,{illegal_config_change,?MODULE,WriteOnce,Other}}
    end.

check_h_config(HConfig) ->
    case check_h_config(maps:get(type,HConfig),maps:to_list(HConfig)) of
        ok ->
            {ok,fix_file_opts(HConfig)};
        {error,{Key,Value}} ->
            {error,{invalid_config,?MODULE,#{Key=>Value}}}
    end.

check_h_config(Type,[{type,Type} | Config]) when Type =:= standard_io;
                                                 Type =:= standard_error;
                                                 Type =:= file ->
    check_h_config(Type,Config);
check_h_config({device,Device},[{type,{device,Device}} | Config]) ->
    check_h_config({device,Device},Config);
check_h_config(file,[{file,File} | Config]) when is_list(File) ->
    check_h_config(file,Config);
check_h_config(file,[{modes,Modes} | Config]) when is_list(Modes) ->
    check_h_config(file,Config);
check_h_config(file,[{max_no_bytes,Size} | Config])
  when (is_integer(Size) andalso Size>0) orelse Size=:=infinity ->
    check_h_config(file,Config);
check_h_config(file,[{max_no_files,Num} | Config]) when is_integer(Num), Num>=0 ->
    check_h_config(file,Config);
check_h_config(file,[{compress_on_rotate,Bool} | Config]) when is_boolean(Bool) ->
    check_h_config(file,Config);
check_h_config(file,[{file_check,FileCheck} | Config])
  when is_integer(FileCheck), FileCheck>=0 ->
    check_h_config(file,Config);
check_h_config(_Type,[Other | _]) ->
    {error,Other};
check_h_config(_Type,[]) ->
    ok.

normalize_config(#{type:={file,File}}=HConfig) ->
    normalize_config(HConfig#{type=>file,file=>File});
normalize_config(#{type:={file,File,Modes}}=HConfig) ->
    normalize_config(HConfig#{type=>file,file=>File,modes=>Modes});
normalize_config(#{file:=File}=HConfig) ->
    HConfig#{file=>filename:absname(File)};
normalize_config(HConfig) ->
    HConfig.

merge_default_config(Name,#{type:=Type}=HConfig) ->
    merge_default_config(Name,Type,HConfig);
merge_default_config(Name,#{file:=_}=HConfig) ->
    merge_default_config(Name,file,HConfig);
merge_default_config(Name,HConfig) ->
    merge_default_config(Name,standard_io,HConfig).

merge_default_config(Name,Type,HConfig) ->
    maps:merge(get_default_config(Name,Type),HConfig).

get_default_config(Name,file) ->
     #{type => file,
       file => filename:absname(atom_to_list(Name)),
       modes => [raw,append],
       file_check => 0,
       max_no_bytes => infinity,
       max_no_files => 0,
       compress_on_rotate => false};
get_default_config(_Name,Type) ->
     #{type => Type}.

fix_file_opts(#{modes:=Modes}=HConfig) ->
    HConfig#{modes=>fix_modes(Modes)};
fix_file_opts(HConfig) ->
    HConfig#{filesync_repeat_interval=>no_repeat}.

fix_modes(Modes) ->
    %% Ensure write|append|exclusive
    Modes1 =
        case [M || M <- Modes,
                   lists:member(M,[write,append,exclusive])] of
            [] -> [append|Modes];
            _ -> Modes
        end,
    %% Ensure raw
    Modes2 =
        case lists:member(raw,Modes) of
            false -> [raw|Modes1];
            true -> Modes1
        end,
    %% Ensure delayed_write
    case lists:partition(fun(delayed_write) -> true;
                            ({delayed_write,_,_}) -> true;
                            (_) -> false
                         end, Modes2) of
        {[],_} ->
            [delayed_write|Modes2];
        _ ->
            Modes2
    end.

-doc false.
config_changed(_Name,
               #{file_check:=FileCheck,
                 max_no_bytes:=Size,
                 max_no_files:=Count,
                 compress_on_rotate:=Compress},
               #{file_check:=FileCheck,
                 max_no_bytes:=Size,
                 max_no_files:=Count,
                 compress_on_rotate:=Compress}=State) ->
    State;
config_changed(_Name,
               #{file_check:=FileCheck,
                 max_no_bytes:=Size,
                 max_no_files:=Count,
                 compress_on_rotate:=Compress},
               #{file_ctrl_pid := FileCtrlPid} = State) ->
    FileCtrlPid ! {update_config,#{file_check=>FileCheck,
                                   max_no_bytes=>Size,
                                   max_no_files=>Count,
                                   compress_on_rotate=>Compress}},
    State#{file_check:=FileCheck,
           max_no_bytes:=Size,
           max_no_files:=Count,
           compress_on_rotate:=Compress};
config_changed(_Name,_NewHConfig,State) ->
    State.

-doc false.
filesync(_Name, SyncAsync, #{file_ctrl_pid := FileCtrlPid} = State) ->
    Result = file_ctrl_filesync(SyncAsync, FileCtrlPid),
    {Result,State}.

-doc false.
write(_Name, SyncAsync, Bin, #{file_ctrl_pid:=FileCtrlPid} = State) ->
    Result = file_write(SyncAsync, FileCtrlPid, Bin),
    {Result,State}.

-doc false.
reset_state(_Name, State) ->
    State.

-doc false.
handle_info(_Name, {'EXIT',Pid,Why}, #{file_ctrl_pid := Pid}=State) ->
    %% file_ctrl_pid died, file error, terminate handler
    exit({error,{write_failed,maps:with([type,file,modes],State),Why}});
handle_info(_, _, State) ->
    State.

-doc false.
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
open_log_file(HandlerName,#{type:=file,
                            file:=FileName,
                            modes:=Modes,
                            file_check:=FileCheck}) ->
    try
        case filelib:ensure_dir(FileName) of
            ok ->
                case file:open(FileName, Modes) of
                    {ok, Fd} ->
                        {ok,#file_info{inode=INode}} =
                            file:read_file_info(FileName,[raw]),
                        UpdateModes = [append | Modes--[write,append,exclusive]],
                        {ok,#{handler_name=>HandlerName,
                              file_name=>FileName,
                              modes=>UpdateModes,
                              file_check=>FileCheck,
                              fd=>Fd,
                              inode=>INode,
                              last_check=>timestamp(),
                              synced=>false,
                              write_res=>ok,
                              sync_res=>ok}};
                    Error ->
                        Error
                end;
            Error ->
                Error
        end
    catch
        _:Reason -> {error,Reason}
    end.

close_log_file(#{fd:=Fd}) ->
    _ = file:datasync(Fd), %% file:datasync may return error as it will flush the delayed_write buffer
    _ = file:close(Fd),
    ok;
close_log_file(_) ->
    ok.

%% A special close that closes the FD properly when the delayed write close failed
delayed_write_close(#{fd:=Fd}) ->
    case file:close(Fd) of
        %% We got an error while closing, could be a delayed write failing
        %% So we close again in order to make sure the file is closed.
        {error, _} ->
            file:close(Fd);
        Res ->
            Res
    end.

%%%-----------------------------------------------------------------
%%% File control process

file_ctrl_start(HandlerName, HConfig) ->
    Starter = self(),
    FileCtrlPid =
        spawn_link(fun() ->
                           file_ctrl_init(HandlerName, HConfig, Starter)
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

file_write(async, Pid, Bin) ->
    Pid ! {log,Bin},
    ok;
file_write(sync, Pid, Bin) ->
    file_ctrl_call(Pid, {log,Bin}).

file_ctrl_filesync(async, Pid) ->
    Pid ! filesync,
    ok;
file_ctrl_filesync(sync, Pid) ->
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
            %% If this timeout triggers we will get a stray
            %% reply message in our mailbox eventually.
            %% That does not really matter though as it will
            %% end up in this module's handle_info and be ignored
            demonitor(MRef, [flush]),
            {error,{no_response,Pid}}
    end.

file_ctrl_init(HandlerName,
               #{type:=file,
                 max_no_bytes:=Size,
                 max_no_files:=Count,
                 compress_on_rotate:=Compress,
                 file:=FileName} = HConfig,
               Starter) ->
    process_flag(message_queue_data, off_heap),
    case open_log_file(HandlerName,HConfig) of
        {ok,State} ->
            Starter ! {self(),ok},
            %% Do the initial rotate (if any) after we ack the starting
            %% process as otherwise startup of the system will be
            %% delayed/crash
            RotState = update_rotation({Size,Count,Compress},State),
            file_ctrl_loop(RotState);
        {error,Reason} ->
            Starter ! {self(),{error,{open_failed,FileName,Reason}}}
    end;
file_ctrl_init(HandlerName, #{type:={device,Dev}}, Starter) ->
    Starter ! {self(),ok},
    file_ctrl_loop(#{handler_name=>HandlerName,dev=>Dev});
file_ctrl_init(HandlerName, #{type:=StdDev}, Starter) ->
    Starter ! {self(),ok},
    file_ctrl_loop(#{handler_name=>HandlerName,dev=>StdDev}).

file_ctrl_loop(State) ->
    receive
        %% asynchronous event
        {log,Bin} ->
            State1 = write_to_dev(Bin,State),
            file_ctrl_loop(State1);

        %% synchronous event
        {{log,Bin},{From,MRef}} ->
            State1 = ensure_file(State),
            State2 = write_to_dev(Bin,State1),
            From ! {MRef,ok},
            file_ctrl_loop(State2);

        filesync ->
            State1 = sync_dev(State),
            file_ctrl_loop(State1);

        {filesync,{From,MRef}} ->
            State1 = ensure_file(State),
            State2 = sync_dev(State1),
            From ! {MRef,ok},
            file_ctrl_loop(State2);

        {update_config,#{file_check:=FileCheck,
                         max_no_bytes:=Size,
                         max_no_files:=Count,
                         compress_on_rotate:=Compress}} ->
            State1 = update_rotation({Size,Count,Compress},State),
            file_ctrl_loop(State1#{file_check=>FileCheck});

        stop ->
            close_log_file(State),
            stopped
    end.

maybe_ensure_file(#{file_check:=0}=State) ->
    ensure_file(State);
maybe_ensure_file(#{last_check:=T0,file_check:=CheckInt}=State)
  when is_integer(CheckInt) ->
    T = timestamp(),
    if T-T0 > CheckInt -> ensure_file(State);
       true -> State
    end;
maybe_ensure_file(State) ->
    State.

%% In order to play well with tools like logrotate, we need to be able
%% to re-create the file if it has disappeared (e.g. if rotated by
%% logrotate)
ensure_file(#{inode:=INode0,file_name:=FileName,modes:=Modes}=State) ->
    case file:read_file_info(FileName,[raw]) of
        {ok,#file_info{inode=INode0}} ->
            State#{last_check=>timestamp()};
        _ ->
            close_log_file(State),
            {ok, Fd} = ensure_open(FileName, Modes),
            {ok,#file_info{inode=INode}} =
                file:read_file_info(FileName,[raw]),
            State#{fd=>Fd,inode=>INode,
                   last_check=>timestamp(),
                   synced=>true,sync_res=>ok}
    end;
ensure_file(State) ->
    State.

ensure_open(Filename, Modes) ->
    case filelib:ensure_dir(Filename) of
        ok ->
            case file:open(Filename, Modes) of
                {ok, Fd} ->
                    {ok, Fd};
                Error ->
                    exit({could_not_reopen_file,Error})
            end;
        Error ->
            exit({could_not_create_dir_for_file,Error})
    end.

write_to_dev(Bin,#{dev:=standard_io}=State) ->
    try
        io:put_chars(user, Bin)
    catch _E:_R ->
            io:put_chars(
              standard_error, "Failed to write log message to stdout, trying stderr\n"),
            io:put_chars(standard_error, Bin)
    end,
    State;
write_to_dev(Bin,#{dev:=DevName}=State) ->
    io:put_chars(DevName, Bin),
    State;
write_to_dev(Bin, State) ->
    State1 = #{fd:=Fd} = maybe_ensure_file(State),
    Result = ?file_write(Fd, Bin),
    State2 = maybe_rotate_file(Bin,State1),
    maybe_notify_error(write,Result,State2),
    State2#{synced=>false,write_res=>Result}.

sync_dev(#{synced:=false}=State) ->
    State1 = #{fd:=Fd} = maybe_ensure_file(State),
    Result = ?file_datasync(Fd),
    maybe_notify_error(filesync,Result,State1),
    State1#{synced=>true,sync_res=>Result};
sync_dev(State) ->
    State.

update_rotation({infinity,_,_},State) ->
    maybe_remove_archives(0,State),
    maps:remove(rotation,State);
update_rotation({Size,Count,Compress},#{file_name:=FileName} = State) ->
    maybe_remove_archives(Count,State),
    {ok,#file_info{size=CurrSize}} = file:read_file_info(FileName,[raw]),
    State1 = State#{rotation=>#{size=>Size,
                                count=>Count,
                                compress=>Compress,
                                curr_size=>CurrSize}},
    maybe_update_compress(0,State1),
    maybe_rotate_file(0,State1).

maybe_remove_archives(Count,#{file_name:=FileName}=State) ->
    Archive = rot_file_name(FileName,Count,false),
    CompressedArchive = rot_file_name(FileName,Count,true),
    case {file:read_file_info(Archive,[raw]),
          file:read_file_info(CompressedArchive,[raw])} of
        {{error,enoent},{error,enoent}} ->
            ok;
        _ ->
            _ = file:delete(Archive),
            _ = file:delete(CompressedArchive),
            maybe_remove_archives(Count+1,State)
    end.

maybe_update_compress(Count,#{rotation:=#{count:=Count}}) ->
    ok;
maybe_update_compress(N,#{file_name:=FileName,
                          rotation:=#{compress:=Compress}}=State) ->
    Archive = rot_file_name(FileName,N,not Compress),
    case file:read_file_info(Archive,[raw]) of
        {ok,_} when Compress ->
            compress_file(Archive);
        {ok,_} ->
            decompress_file(Archive);
        _ ->
            ok
    end,
    maybe_update_compress(N+1,State).

maybe_rotate_file(Bin,#{rotation:=_}=State) when is_binary(Bin) ->
    maybe_rotate_file(byte_size(Bin),State);
maybe_rotate_file(AddSize,#{rotation:=#{size:=RotSize,
                                        curr_size:=CurrSize}=Rotation}=State) ->
    NewSize = CurrSize + AddSize,
    if NewSize>RotSize ->
            rotate_file(State#{rotation=>Rotation#{curr_size=>NewSize}});
       true ->
            State#{rotation=>Rotation#{curr_size=>NewSize}}
    end;
maybe_rotate_file(_Bin,State) ->
    State.

rotate_file(#{file_name:=FileName,modes:=Modes,rotation:=Rotation}=State) ->
    State1 = sync_dev(State),
    _ = delayed_write_close(State),
    rotate_files(FileName,maps:get(count,Rotation),maps:get(compress,Rotation)),
    {ok, Fd} = ensure_open(FileName,Modes),
    {ok,#file_info{inode=INode}} = file:read_file_info(FileName,[raw]),
    State1#{fd=>Fd,inode=>INode,rotation=>Rotation#{curr_size=>0}}.

rotate_files(FileName,0,_Compress) ->
    _ = file:delete(FileName),
    ok;
rotate_files(FileName,1,Compress) ->
    FileName0 = FileName++".0",
    Rename = file:rename(FileName,FileName0),
    %% Rename may fail if file has been deleted. If it has, then
    %% we do not need to compress it...
    if Rename =:= ok, Compress -> compress_file(FileName0);
       true -> ok
    end,
    ok;
rotate_files(FileName,Count,Compress) ->
    _ = file:rename(rot_file_name(FileName,Count-2,Compress),
                    rot_file_name(FileName,Count-1,Compress)),
    rotate_files(FileName,Count-1,Compress).

rot_file_name(FileName,Count,false) ->
    FileName ++ "." ++ integer_to_list(Count);
rot_file_name(FileName,Count,true) ->
    rot_file_name(FileName,Count,false) ++ ".gz".

compress_file(FileName) ->
    {ok,In} = file:open(FileName,[read,binary]),
    {ok,Out} = file:open(FileName++".gz",[write]),
    Z = zlib:open(),
    zlib:deflateInit(Z, default, deflated, 31, 8, default),
    compress_data(Z,In,Out),
    zlib:deflateEnd(Z),
    zlib:close(Z),
    _ = file:close(In),
    _ = file:close(Out),
    _ = file:delete(FileName),
    ok.

compress_data(Z,In,Out) ->
    case file:read(In,100000) of
        {ok,Data} ->
            Compressed = zlib:deflate(Z, Data),
            _ = file:write(Out,Compressed),
            compress_data(Z,In,Out);
        eof ->
            Compressed = zlib:deflate(Z, <<>>, finish),
            _ = file:write(Out,Compressed),
            ok
    end.

decompress_file(FileName) ->
    {ok,In} = file:open(FileName,[read,binary]),
    {ok,Out} = file:open(filename:rootname(FileName,".gz"),[write]),
    Z = zlib:open(),
    zlib:inflateInit(Z, 31),
    decompress_data(Z,In,Out),
    zlib:inflateEnd(Z),
    zlib:close(Z),
    _ = file:close(In),
    _ = file:close(Out),
    _ = file:delete(FileName),
    ok.

decompress_data(Z,In,Out) ->
    case file:read(In,1000) of
        {ok,Data} ->
            Decompressed = zlib:inflate(Z, Data),
            _ = file:write(Out,Decompressed),
            decompress_data(Z,In,Out);
        eof ->
            ok
    end.

maybe_notify_error(_Op, ok, _State) ->
    ok;
maybe_notify_error(Op, Result, #{write_res:=WR,sync_res:=SR})
  when (Op==write andalso Result==WR) orelse
       (Op==filesync andalso Result==SR) ->
    %% don't report same error twice
    ok;
maybe_notify_error(Op, Error, #{handler_name:=HandlerName,file_name:=FileName}) ->
    logger_h_common:error_notify({HandlerName,Op,FileName,Error}),
    ok.

timestamp() ->
    erlang:monotonic_time(millisecond).
