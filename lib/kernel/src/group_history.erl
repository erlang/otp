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
-module(group_history).
-export([load/0, add/1]).

%% Make a minimal size that should encompass set of lines and then make
%% a file rotation for N files of this size.
-define(DEFAULT_HISTORY_FILE, "erlang-shell-log").
-define(MAX_HISTORY_FILES, 10).
-define(DEFAULT_SIZE, 1024*512). % 512 kb total default
-define(DEFAULT_STATUS, disabled).
-define(MIN_HISTORY_SIZE, (50*1024)). % 50 kb, in bytes
-define(DEFAULT_DROP, []).
-define(DISK_LOG_FORMAT, internal). % since we want repairs
-define(LOG_NAME, '$#group_history').
-define(VSN, {0,1,0}).

%%%%%%%%%%%%%%
%%% PUBLIC %%%
%%%%%%%%%%%%%%

%% @doc Loads the shell history from memory. This function should only be
%% called from group:server/3 to inject itself in the previous commands
%% stack.
-spec load() -> [string()].
load() ->
    wait_for_kernel_safe_sup(),
    case history_status() of
        enabled ->
            case open_log() of
                {ok, ?LOG_NAME} ->
                    read_full_log(?LOG_NAME);
                {repaired, ?LOG_NAME, {recovered, Good}, {badbytes, Bad}} ->
                    report_repairs(?LOG_NAME, Good, Bad),
                    read_full_log(?LOG_NAME);
                {error, {need_repair, _FileName}} ->
                    repair_log(?LOG_NAME);
                {error, {arg_mismatch, repair, true, false}} ->
                    repair_log(?LOG_NAME);
                {error, {name_already_open, _}} ->
                    show_rename_warning(),
                    read_full_log(?LOG_NAME);
                {error, {size_mismatch, Current, New}} ->
                    show_size_warning(Current, New),
                    resize_log(?LOG_NAME, Current, New),
                    load();
                {error, {invalid_header, {vsn, Version}}} ->
                    upgrade_version(?LOG_NAME, Version),
                    load();
                {error, Reason} ->
                    handle_open_error(Reason),
                    disable_history(),
                    []
            end;
        _ ->
            []
    end.

%% @doc adds a log line to the erlang history log, if configured to do so.
-spec add(iodata()) -> ok.
add(Line) -> add(Line, history_status()).

add(Line, enabled) ->
    case lists:member(Line, to_drop()) of
        false ->
            case disk_log:log(?LOG_NAME, Line) of
                ok ->
                    ok;
                {error, no_such_log} ->
                    _ = open_log(), % a wild attempt we hope works!
                    disk_log:log(?LOG_NAME, Line);
                {error, _Other} ->
                    % just ignore, we're too late
                    ok
            end;
        true ->
            ok
    end;
add(_Line, disabled) ->
    ok.

%%%%%%%%%%%%%%%
%%% PRIVATE %%%
%%%%%%%%%%%%%%%

%% Because loading the shell happens really damn early, processes we depend on
%% might not be there yet. Luckily, the load function is called from the shell
%% after a new process has been spawned, so we can block in here
wait_for_kernel_safe_sup() ->
    case whereis(kernel_safe_sup) of
        undefined ->
            timer:sleep(50),
            wait_for_kernel_safe_sup();
        _ ->
            ok
    end.

%% Repair the log out of band
repair_log(Name) ->
    Opts = lists:keydelete(size, 1, log_options()),
    case disk_log:open(Opts) of
        {repaired, ?LOG_NAME, {recovered, Good}, {badbytes, Bad}} ->
            report_repairs(?LOG_NAME, Good, Bad);
        _ ->
            ok
    end,
    _ = disk_log:close(Name),
    load().

%% Return whether the shell history is enabled or not
-spec history_status() -> enabled | disabled.
history_status() ->
    case is_user() orelse application:get_env(kernel, shell_history) of
        true -> disabled; % don't run for user proc
        {ok, enabled} -> enabled;
        undefined -> ?DEFAULT_STATUS;
        _ -> disabled
    end.

%% Return whether the user process is running this
-spec is_user() -> boolean().
is_user() ->
    case process_info(self(), registered_name) of
        {registered_name, user} -> true;
        _ -> false
    end.

%% Open a disk_log file while ensuring the required path is there.
open_log() ->
    Opts = log_options(),
    _ = ensure_path(Opts),
    disk_log:open(Opts).

%% Return logger options
log_options() ->
    Path = find_path(),
    File = filename:join([Path, ?DEFAULT_HISTORY_FILE]),
    Size = find_wrap_values(),
    [{name, ?LOG_NAME},
     {file, File},
     {repair, true},
     {format, internal},
     {type, wrap},
     {size, Size},
     {distributed, []},
     {notify, false},
     {head, {vsn, ?VSN}},
     {quiet, true},
     {mode, read_write}].

-spec ensure_path([{file, string()} | {atom(), _}, ...]) -> ok | {error, term()}.
ensure_path(Opts) ->
    {file, Path} = lists:keyfind(file, 1, Opts),
    filelib:ensure_dir(Path).

%% @private read the logs from an already open file. Treat closed files
%% as wrong and returns an empty list to avoid crash loops in the shell.
-spec read_full_log(term()) -> [string()].
read_full_log(Name) ->
    case disk_log:chunk(Name, start) of
        {error, no_such_log} ->
            show_unexpected_close_warning(),
            [];
        eof ->
            [];
        {Cont, Logs} ->
            lists:reverse(maybe_drop_header(Logs) ++ read_full_log(Name, Cont))
    end.

read_full_log(Name, Cont) ->
    case disk_log:chunk(Name, Cont) of
        {error, no_such_log} ->
            show_unexpected_close_warning(),
            [];
        eof ->
            [];
        {NextCont, Logs} ->
            maybe_drop_header(Logs) ++ read_full_log(Name, NextCont)
    end.

maybe_drop_header([{vsn, _} | Rest]) -> Rest;
maybe_drop_header(Logs) -> Logs.

-spec handle_open_error(_) -> ok.
handle_open_error({arg_mismatch, OptName, CurrentVal, NewVal}) ->
    show('$#erlang-history-arg-mismatch',
         "Log file argument ~p changed value from ~p to ~p "
         "and cannot be automatically updated. Please clear the "
         "history files and try again.~n",
         [OptName, CurrentVal, NewVal]);
handle_open_error({not_a_log_file, FileName}) ->
    show_invalid_file_warning(FileName);
handle_open_error({invalid_index_file, FileName}) ->
    show_invalid_file_warning(FileName);
handle_open_error({invalid_header, Term}) ->
    show('$#erlang-history-invalid-header',
         "Shell history expects to be able to use the log files "
         "which currently have unknown headers (~p) and may belong to "
         "another mechanism. History logging will be "
         "disabled.~n",
         [Term]);
handle_open_error({file_error, FileName, Reason}) ->
    show('$#erlang-history-file-error',
         "Error handling File ~ts. Reason: ~p~n"
         "History logging will be disabled.~n",
         [FileName, Reason]);
handle_open_error(Err) ->
    show_unexpected_warning({disk_log, open, 1}, Err).

find_wrap_values() ->
    ConfSize = case application:get_env(kernel, shell_history_file_bytes) of
        undefined -> ?DEFAULT_SIZE;
        {ok, S} -> S
    end,
    SizePerFile = max(?MIN_HISTORY_SIZE, ConfSize div ?MAX_HISTORY_FILES),
    FileCount = if SizePerFile > ?MIN_HISTORY_SIZE ->
                       ?MAX_HISTORY_FILES
                 ; SizePerFile =< ?MIN_HISTORY_SIZE ->
                       max(1, ConfSize div SizePerFile)
                end,
    {SizePerFile, FileCount}.

report_repairs(_, _, 0) ->
    %% just a regular close repair
    ok;
report_repairs(_, Good, Bad) ->
    show('$#erlang-history-report-repairs',
         "The shell history log file was corrupted and was repaired. "
         "~p bytes were recovered and ~p were lost.~n", [Good, Bad]).

resize_log(Name, _OldSize, NewSize) ->
    show('$#erlang-history-resize-attempt',
         "Attempting to resize the log history file to ~p...", [NewSize]),
    Opts = lists:keydelete(size, 1, log_options()),
    _ = case disk_log:open(Opts) of
        {error, {need_repair, _}} ->
            _ = repair_log(Name),
            disk_log:open(Opts);
        _ ->
            ok
    end,
    case disk_log:change_size(Name, NewSize) of
        ok ->
            show('$#erlang-history-resize-result',
                 "ok~n", []);
        {error, {new_size_too_small, _, _}} ->
            show('$#erlang-history-resize-result',
                 "failed (new size is too small)~n", []),
            disable_history();
        {error, Reason} ->
            show('$#erlang-history-resize-result',
                 "failed (~p)~n", [Reason]),
            disable_history()
    end.

upgrade_version(_Name, Unsupported) ->
    %% We only know of one version and can't support a newer one
    show('$#erlang-history-upgrade',
         "The version for the shell logs found on disk (~p) is "
         "not supported by the current version (~p)~n",
         [Unsupported, ?VSN]),
    disable_history().

disable_history() ->
    show('$#erlang-history-disable', "Disabling shell history logging.~n", []),
    application:set_env(kernel, shell_history, force_disabled).

find_path() ->
    case application:get_env(kernel, shell_history_path) of
        undefined -> filename:basedir(user_cache, "erlang-history");
        {ok, Path} -> Path
    end.

to_drop() ->
    case application:get_env(kernel, shell_history_drop) of
        undefined ->
            application:set_env(kernel, shell_history_drop, ?DEFAULT_DROP),
            ?DEFAULT_DROP;
        {ok, V} when is_list(V) -> [Ln++"\n" || Ln <- V];
        {ok, _} -> ?DEFAULT_DROP
    end.

%%%%%%%%%%%%%%%%%%%%%%%%
%%% Output functions %%%
%%%%%%%%%%%%%%%%%%%%%%%%
show_rename_warning() ->
    show('$#erlang-history-rename-warn',
         "A history file with a different path has already "
         "been started for the shell of this node. The old "
         "name will keep being used for this session.~n",
         []).

show_invalid_file_warning(FileName) ->
    show('$#erlang-history-invalid-file',
         "Shell history expects to be able to use the file ~ts "
         "which currently exists and is not a file usable for "
         "history logging purposes. History logging will be "
         "disabled.~n", [FileName]).

show_unexpected_warning({M,F,A}, Term) ->
    show('$#erlang-history-unexpected-return',
         "unexpected return value from ~p:~p/~p: ~p~n"
         "shell history will be disabled for this session.~n",
         [M,F,A,Term]).

show_unexpected_close_warning() ->
    show('$#erlang-history-unexpected-close',
         "The shell log file has mysteriousy closed. Ignoring "
         "currently unread history.~n", []).

show_size_warning(_Current, _New) ->
    show('$#erlang-history-size',
         "The configured log history file size is different from "
         "the size of the log file on disk.~n", []).

show(Key, Format, Args) ->
    case get(Key) of
        undefined ->
            io:format(standard_error, Format, Args),
            put(Key, true),
            ok;
        true ->
            ok
    end.
