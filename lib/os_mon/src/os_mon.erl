%% 
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2016. All Rights Reserved.
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
-module(os_mon).

-behaviour(application).
-behaviour(supervisor).

%% API
-export([call/2, call/3, get_env/2, open_port/2]).

%% Application callbacks
-export([start/2, stop/1]).

%% Supervisor callbacks
-export([init/1]).

%%%-----------------------------------------------------------------
%%% API
%%%-----------------------------------------------------------------

call(Service, Request) ->
    call(Service, Request, 5000).

call(Service, Request, Timeout) ->
    try gen_server:call(server_name(Service), Request, Timeout)
    catch
	exit:{noproc, Call} ->
	    case lists:keysearch(os_mon, 1,
				 application:which_applications()) of
		{value, _AppInfo} ->
		    case startp(Service) of
			true ->
			    erlang:exit({noproc, Call});
			false ->
			    String = "OS_MON (~p) called by ~p, "
				     "unavailable~n",
			    error_logger:warning_msg(String,
						     [Service, self()]),
			    Service:dummy_reply(Request)
		    end;
		false ->
		    String = "OS_MON (~p) called by ~p, not started~n",
		    error_logger:warning_msg(String, [Service, self()]),
		    Service:dummy_reply(Request)
	    end
    end.

get_env(Service, Param) ->
    case application:get_env(os_mon, Param) of
	{ok, Value} ->
	    case Service:param_type(Param, Value) of
		true ->
		    Value;
		false ->
		    String = "OS_MON (~p), ignoring "
			     "bad configuration parameter (~p=~p)~n"
	                     "Using default value instead~n",
		    error_logger:warning_msg(String,
					     [Service, Param, Value]),
		    Service:param_default(Param)
	    end;
	undefined ->
	    Service:param_default(Param)
    end.

open_port(Name, Opts) ->
    PrivDir = code:priv_dir(os_mon),
    ReleasedPath = filename:join([PrivDir,"bin",Name]),
    %% Check os_mon*/priv/bin/Name
    case filelib:is_regular(ReleasedPath) of
	true ->
	    erlang:open_port({spawn, "\""++ReleasedPath++"\""}, Opts);
	false ->
	    %% Use os_mon*/priv/bin/Arch/Name
	    ArchPath =
		filename:join(
		  [PrivDir,"bin",erlang:system_info(system_architecture),Name]),
	    erlang:open_port({spawn, "\""++ArchPath++"\""}, Opts)
    end.


%%%-----------------------------------------------------------------
%%% Application callbacks
%%%-----------------------------------------------------------------

start(_, _) ->
    supervisor:start_link({local, os_mon_sup}, os_mon, []).

stop(_) ->
    ok.

%%%-----------------------------------------------------------------
%%% Supervisor callbacks
%%%-----------------------------------------------------------------

init([]) ->
    SupFlags = case os:type() of
		   {win32, _} ->
		       {one_for_one, 5, 3600};
		   _ ->
		       {one_for_one, 4, 3600}
	       end,
    SysInf = childspec(sysinfo, startp(sysinfo)),
    DskSup = childspec(disksup, startp(disksup)),
    MemSup = childspec(memsup,  startp(memsup)),
    CpuSup = childspec(cpu_sup, startp(cpu_sup)),
    OsSup  = childspec(os_sup,  startp(os_sup)),
    {ok, {SupFlags, SysInf ++ DskSup ++ MemSup ++ CpuSup ++ OsSup}}.

childspec(_Service, false) ->
    [];
childspec(cpu_sup, true) ->
    [{cpu_sup, {cpu_sup, start_link, []},
      permanent, 2000, worker, [cpu_sup]}];
childspec(disksup, true) ->
    [{disksup, {disksup, start_link, []},
      permanent, 2000, worker, [disksup]}];
childspec(memsup, true) ->
    [{memsup, {memsup, start_link, []},
      permanent, 2000, worker, [memsup]}];
childspec(os_sup, true) ->
    OS = os:type(),
    Mod = case OS of
	      {win32, _} -> nteventlog; % windows
	      _ -> os_sup % solaris
	  end,
    [{os_sup, {os_sup, start_link, [OS]},
      permanent, 10000, worker, [Mod]}];
childspec(sysinfo, true) ->
    [{os_mon_sysinfo, {os_mon_sysinfo, start_link, []},
      permanent, 2000, worker, [os_mon_sysinfo]}].

%%%-----------------------------------------------------------------
%%% Internal functions (OS_Mon configuration)
%%%-----------------------------------------------------------------

startp(Service) ->
    %% Available for this platform?
    case lists:member(Service, services(os:type())) of
	true ->
	    %% Is there a start configuration parameter?
	    case start_param(Service) of
		none ->
		    true;
		Param ->
		    %% Is the start configuration parameter 'true'?
		    case application:get_env(os_mon, Param) of
			{ok, true} ->
			    true;
			_ ->
			    false
		    end
	    end;
	false ->
	    false
    end.

services({unix, sunos}) ->
    [cpu_sup, disksup, memsup, os_sup];
services({unix, _}) -> % Other unix.
    [cpu_sup, disksup, memsup];
services({win32, _}) ->
    [disksup, memsup, os_sup, sysinfo].

server_name(cpu_sup) -> cpu_sup;
server_name(disksup) -> disksup;
server_name(memsup) ->  memsup;
server_name(os_sup) ->  os_sup_server;
server_name(sysinfo) -> os_mon_sysinfo.

start_param(cpu_sup) -> start_cpu_sup;
start_param(disksup) -> start_disksup;
start_param(memsup) ->  start_memsup;
start_param(os_sup) ->  start_os_sup;
start_param(sysinfo) -> none.
