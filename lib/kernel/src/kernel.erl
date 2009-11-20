%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2009. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%
-module(kernel).

-behaviour(supervisor).

%% External exports
-export([start/2, init/1, stop/1]).
-export([config_change/3]).

%%%-----------------------------------------------------------------
%%% The kernel is the first application started.
%%% Callback functions for the kernel application.
%%%-----------------------------------------------------------------
start(_, []) ->
    case supervisor:start_link({local, kernel_sup}, kernel, []) of
	{ok, Pid} ->
	    Type = get_error_logger_type(),
	    error_logger:swap_handler(Type),
	    {ok, Pid, []};
	Error -> Error
    end.

stop(_State) ->
    ok.

%%-------------------------------------------------------------------
%% Some configuration parameters for kernel are changed
%%-------------------------------------------------------------------
config_change(Changed, New, Removed) ->
    do_distribution_change(Changed, New, Removed),
    do_global_groups_change(Changed, New, Removed),
    ok.

get_error_logger_type() ->
    case application:get_env(kernel, error_logger) of
	{ok, tty} -> tty;
	{ok, {file, File}} when is_list(File) -> {logfile, File};
	{ok, false} -> false;
	{ok, silent} -> silent;
	undefined -> tty; % default value
	{ok, Bad} -> exit({bad_config, {kernel, {error_logger, Bad}}})
    end.

%%%-----------------------------------------------------------------
%%% The process structure in kernel is as shown in the figure.
%%%
%%%               ---------------
%%%              | kernel_sup (A)|
%%%	          ---------------
%%%                      |
%%%        -------------------------------
%%%       |              |                |
%%%  <std services> -------------   -------------
%%%   (file,code,  | erl_dist (A)| | safe_sup (1)|
%%%    rpc, ...)    -------------   -------------
%%%		          |               |
%%%                  (net_kernel,  (disk_log, pg2,
%%%          	      auth, ...)     ...)
%%%
%%% The rectangular boxes are supervisors.  All supervisors except
%%% for kernel_safe_sup terminates the enitre erlang node if any of
%%% their children dies.  Any child that can't be restarted in case
%%% of failure must be placed under one of these supervisors.  Any
%%% other child must be placed under safe_sup.  These children may
%%% be restarted. Be aware that if a child is restarted the old state
%%% and all data will be lost.
%%%-----------------------------------------------------------------
%%% Callback functions for the kernel_sup supervisor.
%%%-----------------------------------------------------------------

init([]) ->
    SupFlags = {one_for_all, 0, 1},

    Config = {kernel_config,
	      {kernel_config, start_link, []},
	      permanent, 2000, worker, [kernel_config]},
    Code = {code_server,
	    {code, start_link, get_code_args()},
	    permanent, 2000, worker, [code]},
    File = {file_server_2,
	    {file_server, start_link, []},
	    permanent, 2000, worker, 
	    [file, file_server, file_io_server, prim_file]},
    StdError = {standard_error,
		{standard_error, start_link, []},
		temporary, 2000, supervisor, [user_sup]},
    User = {user,
	    {user_sup, start, []},
	    temporary, 2000, supervisor, [user_sup]},
    
    case init:get_argument(mode) of
	{ok, [["minimal"]]} ->
	    SafeSupervisor = {kernel_safe_sup,
			      {supervisor, start_link,
			       [{local, kernel_safe_sup}, ?MODULE, safe]},
			      permanent, infinity, supervisor, [?MODULE]},
	    {ok, {SupFlags,
		  [File, Code, StdError, User,
		   Config, SafeSupervisor]}};
	_ ->
	    Rpc = {rex, {rpc, start_link, []}, 
		   permanent, 2000, worker, [rpc]},
	    Global = {global_name_server, {global, start_link, []}, 
		      permanent, 2000, worker, [global]},
	    Glo_grp = {global_group, {global_group,start_link,[]},
		       permanent, 2000, worker, [global_group]},
	    InetDb = {inet_db, {inet_db, start_link, []},
		      permanent, 2000, worker, [inet_db]},
	    NetSup = {net_sup, {erl_distribution, start_link, []}, 
		      permanent, infinity, supervisor,[erl_distribution]},
	    DistAC = start_dist_ac(),

	    Timer = start_timer(),

	    SafeSupervisor = {kernel_safe_sup,
			      {supervisor, start_link,
			       [{local, kernel_safe_sup}, ?MODULE, safe]},
			      permanent, infinity, supervisor, [?MODULE]},
	    {ok, {SupFlags,
		  [Rpc, Global, InetDb | DistAC] ++ 
		  [NetSup, Glo_grp, File, Code, 
		   StdError, User, Config, SafeSupervisor] ++ Timer}}
    end;
init(safe) ->
    SupFlags = {one_for_one, 4, 3600},
    Boot = start_boot_server(),
    DiskLog = start_disk_log(),
    Pg2 = start_pg2(),
    {ok, {SupFlags, Boot ++ DiskLog ++ Pg2}}.

get_code_args() ->
    case init:get_argument(nostick) of
	{ok, [[]]} -> [[nostick]];
	_ -> []
    end.

start_dist_ac() ->
    Spec = [{dist_ac,{dist_ac,start_link,[]},permanent,2000,worker,[dist_ac]}],
    case application:get_env(kernel, start_dist_ac) of
	{ok, true} -> Spec;
	{ok, false} -> [];
	undefined ->
	    case application:get_env(kernel, distributed) of
		{ok, _} -> Spec;
		_ -> []
	    end
    end.

start_boot_server() ->
    case application:get_env(kernel, start_boot_server) of
	{ok, true} ->
	    Args = get_boot_args(),
	    [{boot_server, {erl_boot_server, start_link, [Args]}, permanent,
	      1000, worker, [erl_boot_server]}];
	_ ->
	    []
    end.

get_boot_args() ->
    case application:get_env(kernel, boot_server_slaves) of
	{ok, Slaves} -> Slaves;
	_            -> []
    end.

start_disk_log() ->
    case application:get_env(kernel, start_disk_log) of
	{ok, true} ->
	    [{disk_log_server,
	      {disk_log_server, start_link, []},
	      permanent, 2000, worker, [disk_log_server]},
	     {disk_log_sup, {disk_log_sup, start_link, []}, permanent,
	      1000, supervisor, [disk_log_sup]}];
	_ ->
	    []
    end.

start_pg2() ->
    case application:get_env(kernel, start_pg2) of
	{ok, true} ->
	    [{pg2, {pg2, start_link, []}, permanent, 1000, worker, [pg2]}];
	_ ->
	    []
    end.

start_timer() ->
    case application:get_env(kernel, start_timer) of
	{ok, true} -> 
	    [{timer_server, {timer, start_link, []}, permanent, 1000, worker, 
	      [timer]}];
	_ ->
	    []
    end.

%%-----------------------------------------------------------------
%% The change of the distributed parameter is taken care of here
%%-----------------------------------------------------------------
do_distribution_change(Changed, New, Removed) ->
    %% check if the distributed parameter is changed. It is not allowed
    %% to make a local application to a distributed one, or vice versa.
    case is_dist_changed(Changed, New, Removed) of
	%%{changed, new, removed}
	{false, false, false} ->
	    ok;
	{C, false, false} ->
	    %% At last, update the parameter.
	    gen_server:call(dist_ac, {distribution_changed, C}, infinity);
	{false, _, false} ->
	    error_logger:error_report("Distribution not changed: "
				      "Not allowed to add the 'distributed' "
				      "parameter."),
	    {error, {distribution_not_changed, "Not allowed to add the "
		     "'distributed' parameter"}};
	{false, false, _} ->
	    error_logger:error_report("Distribution not changed: "
				      "Not allowed to remove the "
				      "distribution parameter."),
	    {error, {distribution_not_changed, "Not allowed to remove the "
		     "'distributed' parameter"}}
    end.

%%-----------------------------------------------------------------
%% Check if distribution is changed in someway.
%%-----------------------------------------------------------------
is_dist_changed(Changed, New, Removed) ->
    C = case lists:keyfind(distributed, 1, Changed) of
	    false ->
		false;
	    {distributed, NewDistC} ->
		NewDistC
	end,
    N = case lists:keyfind(distributed, 1, New) of
	    false ->
		false;
	    {distributed, NewDistN} ->
		NewDistN
	end,
    R = lists:member(distributed, Removed),
    {C, N, R}.

%%-----------------------------------------------------------------
%% The change of the global_groups parameter is taken care of here
%%-----------------------------------------------------------------
do_global_groups_change(Changed, New, Removed) ->
    %% check if the global_groups parameter is changed.
    case is_gg_changed(Changed, New, Removed) of
	%%{changed, new, removed}
	{false, false, false} ->
	    ok;
	{C, false, false} ->
	    %% At last, update the parameter.
	    global_group:global_groups_changed(C);
	{false, N, false} ->
	    global_group:global_groups_added(N);
	{false, false, R} ->
	    global_group:global_groups_removed(R)
    end.

%%-----------------------------------------------------------------
%% Check if global_groups is changed in someway.
%%-----------------------------------------------------------------
is_gg_changed(Changed, New, Removed) ->
    C = case lists:keyfind(global_groups, 1, Changed) of
	    false ->
		false;
	    {global_groups, NewDistC} ->
		NewDistC
	end,
    N = case lists:keyfind(global_groups, 1, New) of
	    false ->
		false;
	    {global_groups, NewDistN} ->
		NewDistN
	end,
    R = lists:member(global_groups, Removed),
    {C, N, R}.
