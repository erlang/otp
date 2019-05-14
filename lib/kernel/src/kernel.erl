%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2018. All Rights Reserved.
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
    %% Setup the logger and configure the kernel logger environment
    ok = logger:internal_init_logger(),
    case supervisor:start_link({local, kernel_sup}, kernel, []) of
	{ok, Pid} ->
            ok = erl_signal_handler:start(),
            ok = logger:add_handlers(kernel),
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
%%% for kernel_safe_sup terminates the entire erlang node if any of
%%% their children dies.  Any child that can't be restarted in case
%%% of failure must be placed under one of these supervisors.  Any
%%% other child must be placed under safe_sup.  These children may
%%% be restarted. Be aware that if a child is restarted the old state
%%% and all data will be lost.
%%%-----------------------------------------------------------------
%%% Callback functions for the kernel_sup supervisor.
%%%-----------------------------------------------------------------

init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},

    Config = #{id => kernel_config,
               start => {kernel_config, start_link, []},
               restart => permanent,
               shutdown => 2000,
               type => worker,
               modules => [kernel_config]},

    RefC = #{id => kernel_refc,
             start => {kernel_refc, start_link, []},
             restart => permanent,
             shutdown => 2000,
             type => worker,
             modules => [kernel_refc]},

    Code = #{id => code_server,
             start => {code, start_link, []},
             restart => permanent,
             shutdown => 2000,
             type => worker,
             modules => [code]},

    File = #{id => file_server_2,
             start => {file_server, start_link, []},
             restart => permanent,
             shutdown => 2000,
             type => worker,
             modules => [file, file_server, file_io_server, prim_file]},

    StdError = #{id => standard_error,
                 start => {standard_error, start_link, []},
                 restart => temporary,
                 shutdown => 2000,
                 type => supervisor,
                 modules => [standard_error]},

    User = #{id => user,
             start => {user_sup, start, []},
             restart => temporary,
             shutdown => 2000,
             type => supervisor,
             modules => [user_sup]},

    SafeSup = #{id => kernel_safe_sup,
                start =>{supervisor, start_link, [{local, kernel_safe_sup}, ?MODULE, safe]},
                restart => permanent,
                shutdown => infinity,
                type => supervisor,
                modules => [?MODULE]},


    LoggerSup = #{id => logger_sup,
                  start => {logger_sup, start_link, []},
                  restart => permanent,
                  shutdown => infinity,
                  type => supervisor,
                  modules => [logger_sup]},

    case init:get_argument(mode) of
        {ok, [["minimal"]|_]} ->
            {ok, {SupFlags,
                  [Code, File, StdError, User, LoggerSup, Config, RefC, SafeSup]}};
        _ ->
            DistChildren =
		case application:get_env(kernel, start_distribution) of
		    {ok, false} -> [];
		    _ -> start_distribution()
		end,

            InetDb = #{id => inet_db,
                       start => {inet_db, start_link, []},
                       restart => permanent,
                       shutdown => 2000,
                       type => worker,
                       modules => [inet_db]},

            SigSrv = #{id => erl_signal_server,
                       start => {gen_event, start_link, [{local, erl_signal_server}]},
                       restart => permanent,
                       shutdown => 2000,
                       type => worker,
                       modules => dynamic},

            Timer = start_timer(),
            CompileServer = start_compile_server(),

            {ok, {SupFlags,
                  [Code, InetDb | DistChildren] ++
                      [File, SigSrv, StdError, User, Config, RefC, SafeSup, LoggerSup] ++
                      Timer ++ CompileServer}}
    end;
init(safe) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 4,
                 period => 3600},

    Boot = start_boot_server(),
    DiskLog = start_disk_log(),
    Pg2 = start_pg2(),

    %% Run the on_load handlers for all modules that have been
    %% loaded so far. Running them at this point means that
    %% on_load handlers can safely call kernel processes
    %% (and in particular call code:priv_dir/1 or code:lib_dir/1).
    init:run_on_load_handlers(),

    {ok, {SupFlags, Boot ++ DiskLog ++ Pg2}}.

start_distribution() ->
    Rpc = #{id => rex,
            start => {rpc, start_link, []},
            restart => permanent,
            shutdown => 2000,
            type => worker,
            modules => [rpc]},

    Global = #{id => global_name_server,
               start => {global, start_link, []},
               restart => permanent,
               shutdown => 2000,
               type => worker,
               modules => [global]},

    DistAC = start_dist_ac(),

    NetSup = #{id => net_sup,
               start => {erl_distribution, start_link, []},
               restart => permanent,
               shutdown => infinity,
               type => supervisor,
               modules => [erl_distribution]},

    GlGroup = #{id => global_group,
                start => {global_group,start_link,[]},
                restart => permanent,
                shutdown => 2000,
                type => worker,
                modules => [global_group]},

    [Rpc, Global | DistAC] ++ [NetSup, GlGroup].

start_dist_ac() ->
    Spec = [#{id => dist_ac,
              start => {dist_ac,start_link,[]},
              restart => permanent,
              shutdown => 2000,
              type => worker,
              modules => [dist_ac]}],
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
            [#{id => boot_server,
               start => {erl_boot_server, start_link, [Args]},
               restart => permanent,
               shutdown => 1000,
               type => worker,
               modules => [erl_boot_server]}];
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
            [#{id => disk_log_server,
               start => {disk_log_server, start_link, []},
               restart => permanent,
               shutdown => 2000,
               type => worker,
               modules => [disk_log_server]},
             #{id => disk_log_sup,
               start => {disk_log_sup, start_link, []},
               restart => permanent,
               shutdown => 1000,
               type => supervisor,
               modules => [disk_log_sup]}];
        _ ->
            []
    end.

start_pg2() ->
    case application:get_env(kernel, start_pg2) of
        {ok, true} ->
            [#{id => pg2,
               start => {pg2, start_link, []},
               restart => permanent,
               shutdown => 1000,
               type => worker,
               modules => [pg2]}];
        _ ->
            []
    end.

start_timer() ->
    case application:get_env(kernel, start_timer) of
        {ok, true} ->
            [#{id => timer_server,
               start => {timer, start_link, []},
               restart => permanent,
               shutdown => 1000,
               type => worker,
               modules => [timer]}];
        _ ->
            []
    end.

start_compile_server() ->
    case application:get_env(kernel, start_compile_server) of
        {ok, true} ->
            [#{id => erl_compile_server,
               start => {erl_compile_server, start_link, []},
               restart => permanent,
               shutdown => 2000,
               type => worker,
               modules => [erl_compile_server]}];
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
