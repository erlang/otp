%% ``Licensed under the Apache License, Version 2.0 (the "License");
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
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%%
%%     $Id: httpd_sup.erl,v 1.1 2008/12/17 09:53:34 mikpe Exp $
%%----------------------------------------------------------------------
%% Purpose: The top supervisor for the inets application
%%----------------------------------------------------------------------

-module(httpd_sup).

-behaviour(supervisor).

-include("httpd_verbosity.hrl").

%% public
-export([start/2, start_link/2, start2/2, start_link2/2, stop/1, stop/2, stop2/1]).
-export([init/1]).


-define(D(F, A), io:format("~p:" ++ F ++ "~n", [?MODULE|A])).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% supervisor callback functions

start(ConfigFile, Verbosity) ->
    case start_link(ConfigFile, Verbosity) of
	{ok, Pid} ->
	    unlink(Pid),
	    {ok, Pid};

	Else ->
	    Else
    end.


start_link(ConfigFile, Verbosity) ->
    case get_addr_and_port(ConfigFile) of
	{ok, ConfigList, Addr, Port} ->
	    Name    = make_name(Addr, Port),
	    SupName = {local, Name},
	    supervisor:start_link(SupName, ?MODULE,
				  [ConfigFile, ConfigList,
				   Verbosity, Addr, Port]);

	{error, Reason} ->
	    error_logger:error_report(Reason),
	    {stop, Reason};

	Else ->
	    error_logger:error_report(Else),
	    {stop, Else}
    end.


start2(ConfigList, Verbosity) ->
    case start_link2(ConfigList, Verbosity) of
	{ok, Pid} ->
	    unlink(Pid),
	    {ok, Pid};

	Else ->
	    Else
    end.


start_link2(ConfigList, Verbosity) ->
    case get_addr_and_port2(ConfigList) of
	{ok, Addr, Port} ->
	    Name    = make_name(Addr, Port),
	    SupName = {local, Name},
	    supervisor:start_link(SupName, ?MODULE,
				  [undefined, ConfigList, Verbosity, Addr, Port]);

	{error, Reason} ->
	    error_logger:error_report(Reason),
	    {stop, Reason};

	Else ->
	    error_logger:error_report(Else),
	    {stop, Else}
    end.



stop(Pid) when pid(Pid) ->
    do_stop(Pid);
stop(ConfigFile) when list(ConfigFile) ->
    case get_addr_and_port(ConfigFile) of
	{ok, _, Addr, Port} ->
	    stop(Addr, Port);

	Error ->
	    Error
    end;
stop(StartArgs) ->
    ok.


stop(Addr, Port) when integer(Port) ->
    Name = make_name(Addr, Port),
    case whereis(Name) of
	Pid when pid(Pid) ->
	    do_stop(Pid),
	    ok;
	_ ->
	    not_started
    end.

stop2(ConfigList) when list(ConfigList) ->
    {ok, Addr, Port} = get_addr_and_port2(ConfigList),
    stop(Addr, Port).


do_stop(Pid) ->
    exit(Pid, shutdown).


init([ConfigFile, ConfigList, Verbosity, Addr, Port]) ->
    init(ConfigFile, ConfigList, Verbosity, Addr, Port);
init(BadArg) ->
    {error, {badarg, BadArg}}.

init(ConfigFile, ConfigList, Verbosity, Addr, Port) ->
    Flags = {one_for_one, 0, 1},
    AccSupVerbosity  = get_acc_sup_verbosity(Verbosity),
    MiscSupVerbosity = get_misc_sup_verbosity(Verbosity),
    Sups  = [sup_spec(httpd_acceptor_sup, Addr, Port, AccSupVerbosity),
	     sup_spec(httpd_misc_sup, Addr, Port, MiscSupVerbosity),
	     worker_spec(httpd_manager, Addr, Port, ConfigFile, ConfigList,
			 Verbosity, [gen_server])],
    {ok, {Flags, Sups}}.


sup_spec(Name, Addr, Port, Verbosity) ->
    {{Name, Addr, Port},
     {Name, start, [Addr, Port, Verbosity]},
     permanent, 2000, supervisor, [Name, supervisor]}.

worker_spec(Name, Addr, Port, ConfigFile, ConfigList, Verbosity, Modules) ->
    {{Name, Addr, Port},
     {Name, start_link, [ConfigFile, ConfigList, Verbosity]},
     permanent, 2000, worker, [Name] ++ Modules}.


make_name(Addr,Port) ->
    httpd_util:make_name("httpd_sup",Addr,Port).


%% get_addr_and_port

get_addr_and_port(ConfigFile) ->
    case httpd_conf:load(ConfigFile) of
	{ok, ConfigList} ->
	    {ok, Addr, Port} = get_addr_and_port2(ConfigList),
	    {ok, ConfigList, Addr, Port};
	Error ->
	    Error
    end.


get_addr_and_port2(ConfigList) ->
    Port = httpd_util:key1search(ConfigList, port, 80),
    Addr = httpd_util:key1search(ConfigList, bind_address),
    {ok, Addr, Port}.

get_acc_sup_verbosity(V) ->
    case key1search(V, all) of
	undefined ->
	    key1search(V, acceptor_sup_verbosity, ?default_verbosity);
	Verbosity ->
	    Verbosity
    end.


get_misc_sup_verbosity(V) ->
    case key1search(V, all) of
	undefined ->
	    key1search(V, misc_sup_verbosity, ?default_verbosity);
	Verbosity ->
	    Verbosity
    end.


key1search(L, K) ->
    httpd_util:key1search(L, K).

key1search(L, K, D) ->
    httpd_util:key1search(L, K, D).
