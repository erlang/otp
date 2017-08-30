%% 
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2016. All Rights Reserved.
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
-module(snmp_app).

-behaviour(application).

-include("snmp_debug.hrl").


%%%-----------------------------------------------------------------
%%%  This module implements the SNMP application.
%%%-----------------------------------------------------------------
-export([start/2, stop/0, stop/1, config_change/3]).
-export([start_agent/0,   start_agent/1,   start_agent/2]).
-export([start_manager/0, start_manager/1, start_manager/2]).

start(Type, []) ->
    ?d("start -> entry with"
      "~n   Type. ~p", [Type]),
    %% This is the new snmp application config format
    %% First start the (new) central supervisor,
    {ok, Pid} = snmp_app_sup:start_link(),
    Entities = entities(),
    ok = start_entities(Type, Entities),
    {ok, Pid}.

entities() ->
    entities([agent, manager], []).

entities([], []) ->
    ?d("entities -> entry when no entities", []),

    %% Could be old style snmp (agent) application config format
    %% but could also be a skeleton start, which means that neither
    %% agent nor manager config has been specified
    
    case get_env() of
	[] ->
	    %% Skeleton start
	    ?d("entities -> skeleton start", []),
	    [];
	OldConf when is_list(OldConf) ->
	    ?d("entities -> old style config: ~n~p", [OldConf]),
	    %% Old style snmp (agent) application config format
	    Conf = snmpa_app:convert_config(OldConf),
	    ?d("entities -> converted config: ~n~p", [Conf]),
	    [{agent, Conf}]
    end;
entities([], Acc) ->
    ?d("entities -> done", []),
    lists:reverse(Acc);
entities([Ent|Ents], Acc) ->
    ?d("entities -> entry with"
       "~n   Ent: ~p", [Ent]),
    case application:get_env(snmp, Ent) of
	{ok, Conf} ->
	    entities(Ents, [{Ent, Conf}|Acc]);
	_ ->
	    entities(Ents, Acc)
    end.
	
start_entities(_Type, []) ->
    ok;
start_entities(Type, [{agent, Opts}|Entities]) ->
    case start_agent(Type, Opts) of
	ok ->
	    start_entities(Type, Entities);
	Error ->
	    Error
    end;
start_entities(Type, [{manager, Opts}|Entities]) ->
    case start_manager(Type, Opts) of
	ok ->
	    start_entities(Type, Entities);
	Error ->
	    Error
    end;
start_entities(Type, [BadEntity|Entities]) ->
    error_msg("Bad snmp configuration: ~n: ~p", [BadEntity]), 
    start_entities(Type, Entities).


start_agent() ->
    start_agent(normal).

start_agent(Type) when is_atom(Type) ->
    case application:get_env(snmp, agent) of
	{ok, Opts} ->
	    start_agent(Type, Opts);
	_ ->
	    {error, missing_config}
    end;
start_agent(Opts) when is_list(Opts) ->
    start_agent(normal, Opts);
start_agent(BadArg) ->
    {error, {bad_arg, BadArg}}.

start_agent(Type, Opts) ->
    ?d("start_agent -> entry", []),
    case snmp_app_sup:start_agent(Type, Opts) of
	{ok, _} ->
	    ok;
	Error ->
	    Error
    end.

start_manager() ->
    start_manager(normal).

start_manager(Type) when is_atom(Type) ->
    case application:get_env(snmp, manager) of
	{ok, Opts} ->
	    start_manager(Type, Opts);
	_ ->
	    {error, missing_config}
    end;
start_manager(Opts) when is_list(Opts) ->
    start_manager(normal, Opts);
start_manager(BadArg) ->
    {error, {bad_arg, BadArg}}.

start_manager(Type, Opts) ->
    ?d("start manager -> entry", []),
    case snmp_app_sup:start_manager(Type, Opts) of
	{ok, _} ->
	    ok;
	Error ->
	    Error
    end.    
    

stop(_) ->
    ok.

stop() ->
    snmp_app_sup:stop().


get_env() ->
    Env        = application:get_all_env(snmp),
    DeleteElem = [included_applications],
    F = fun({Key, _}) -> lists:member(Key, DeleteElem) end,
    lists:dropwhile(F, Env).


%%-----------------------------------------------------------------
%% The presence of this function means that we will accept changes
%% in the configuration parameters.  However, we won't react upon
%% those changes until the app is restarted.  So we just return
%% ok.
%%-----------------------------------------------------------------
config_change(_Changed, _New, _Removed) ->
    ok.

%% ---------------------------------------------------------------------
                        
error_msg(F, A) ->
    error_logger:error_msg("~w: " ++ F ++ "~n", [?MODULE|A]).

