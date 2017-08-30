%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2016. All Rights Reserved.
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

-module(reltool).

%% Public
-export([
         start/0, start/1, start_link/1, debug/0, % GUI
         start_server/1, get_server/1, get_status/1, stop/1,
         get_config/1, get_config/3, get_rel/2, get_script/2,
         create_target/2, get_target_spec/1, eval_target_spec/3,
         install/2
        ]).

-include("reltool.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Start main window process
-spec start() -> {ok, window_pid()} | {error, reason()}.
start() ->
    start([]).

%% Start main window process
-spec start(options()) -> {ok, window_pid()} | {error, reason()}.
start(Options) when is_list(Options) ->
    case start_link(Options) of
	{ok, WinPid} = OK ->
	    unlink(WinPid),
	    OK;
	{error, _Reason} = Error ->
	    Error
    end.

%% Start main window process with wx debugging enabled
-spec debug() -> {ok, window_pid()} | {error, reason()}.
debug() ->
    start([{wx_debug, 2}]).

%% Start main window process with options
-spec start_link(options()) -> {ok, window_pid()} | {error, reason()}.
start_link(Options) when is_list(Options) ->
    case reltool_sys_win:start_link(Options) of
        {ok, _WinPid} = OK ->
            OK;
        {error, Reason} ->
            {error, Reason}
    end.

%% Start server process with options
-spec start_server(options()) -> {ok, server_pid()} | {error, reason()}.
start_server(Options) ->
    case reltool_server:start_link(Options) of
        {ok, ServerPid, _Common, _Sys} ->
            {ok, ServerPid};
        {error, Reason} ->
            {error, Reason}
    end.

%% Start server process with options
-spec get_server(window_pid()) -> {ok, server_pid()} | {error, reason()}.
get_server(WinPid) ->
    case reltool_sys_win:get_server(WinPid) of
        {ok, _ServerPid} = OK ->
            OK;
        {error, Reason} ->
            {error, lists:flatten(io_lib:format("~p", [Reason]))}
    end.

%% Stop a server or window process
-spec stop(server_pid() | window_pid()) -> ok | {error, reason()}.
stop(Pid) when is_pid(Pid) ->
    Ref = erlang:monitor(process, Pid),
    unlink(Pid),
    exit(Pid, shutdown),
    receive
        {'DOWN', Ref, _, _, shutdown} ->
            ok;
        {'DOWN', Ref, _, _, Reason} ->
            {error, lists:flatten(io_lib:format("~p", [Reason]))}
    end.

%% Internal library function
-spec eval_server(server(), boolean(), fun((server_pid()) -> Ret)) ->
	 Ret | {error, reason()} when Ret :: term().
eval_server(Pid, _DisplayWarnings, Fun)
  when is_pid(Pid) ->
    Fun(Pid);
eval_server(Options, DisplayWarnings, Fun)
  when is_list(Options) ->
    TrapExit = process_flag(trap_exit, true),
    Res = case start_server(Options) of
	      {ok, Pid} ->
		  apply_fun(Pid, DisplayWarnings, Fun);
	      {error, _Reason} = Error ->
		  Error
	  end,
    process_flag(trap_exit, TrapExit),
    Res.

apply_fun(Pid, false, Fun) ->
    Res = Fun(Pid),
    stop(Pid),
    Res;
apply_fun(Pid, true, Fun) ->
    case get_status(Pid) of
	{ok, Warnings} ->
	    [io:format("~w: ~ts\n", [?APPLICATION, W]) || W <- Warnings],
	    apply_fun(Pid, false, Fun);
	{error, _Reason} = Error ->
	    stop(Pid),
	    Error
    end.

%% Get status about the configuration
-type warning() :: string().
-spec get_status(server()) -> {ok, [warning()]} | {error, reason()}.
get_status(PidOrOptions)
  when is_pid(PidOrOptions); is_list(PidOrOptions) ->
    eval_server(PidOrOptions, false,
		fun(Pid) -> reltool_server:get_status(Pid) end).

%% Get reltool configuration
-spec get_config(server()) -> {ok, config()} | {error, reason()}.
get_config(PidOrOption) ->
    get_config(PidOrOption, false, false).

-spec get_config(server(), incl_defaults(), incl_derived()) ->
			{ok, config()} | {error, reason()}.
get_config(PidOrOptions, InclDef, InclDeriv)
  when is_pid(PidOrOptions); is_list(PidOrOptions) ->
    eval_server(PidOrOptions, true,
		fun(Pid) ->
			reltool_server:get_config(Pid, InclDef, InclDeriv)
		end).

%% Get contents of release file
-spec get_rel(server(), rel_name()) -> {ok, rel_file()} | {error, reason()}.
get_rel(PidOrOptions, RelName)
  when is_pid(PidOrOptions); is_list(PidOrOptions) ->
    eval_server(PidOrOptions, true,
		fun(Pid) -> reltool_server:get_rel(Pid, RelName) end).

%% Get contents of boot script file
-spec get_script(server(), rel_name()) ->
			{ok, script_file()} | {error, reason()}.
get_script(PidOrOptions, RelName)
  when is_pid(PidOrOptions); is_list(PidOrOptions) ->
   eval_server(PidOrOptions, true,
	       fun(Pid) -> reltool_server:get_script(Pid, RelName) end).

%% Generate a target system
-spec create_target(server(), target_dir()) -> ok | {error, reason()}.
create_target(PidOrOptions, TargetDir)
  when is_pid(PidOrOptions); is_list(PidOrOptions) ->
    eval_server(PidOrOptions, true,
		fun(Pid) -> reltool_server:gen_target(Pid, TargetDir) end).

%% Generate a target system
-spec get_target_spec(server()) -> {ok, target_spec()} | {error, reason()}.
get_target_spec(PidOrOptions)
  when is_pid(PidOrOptions); is_list(PidOrOptions) ->
    eval_server(PidOrOptions, true,
		fun(Pid) -> reltool_server:gen_spec(Pid) end).

%% Generate a target system
-spec eval_target_spec(target_spec(), root_dir(), target_dir()) ->
			      ok | {error, reason()}.
eval_target_spec(Spec, SourceDir, TargetDir)
  when is_list(SourceDir), is_list(TargetDir) ->
    reltool_target:eval_spec(Spec, SourceDir, TargetDir).

%% Install a target system
-spec install(rel_name(), dir()) -> ok | {error, reason()}.
install(RelName, TargetDir) ->
    reltool_target:install(RelName, TargetDir).
