%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2017. All Rights Reserved.
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

%%
%% The top supervisor for the diameter application.
%%

-module(diameter_sup).

-behaviour(supervisor).

%% interface
-export([start_link/0,  %% supervisor start
         tree/0]).      %% supervision tree

%% supervisor callback
-export([init/1]).

-define(CHILDREN, [diameter_misc_sup,
                   diameter_config_sup,
                   diameter_watchdog_sup,
                   diameter_peer_fsm_sup,
                   diameter_transport_sup,
                   diameter_service_sup]).

-define(TABLES, [{diameter_sequence, [set]},
                 {diameter_service,  [set, {keypos, 3}]},
                 {diameter_request,  [set]},
                 {diameter_config,   [bag, {keypos, 2}]}]).

%% start_link/0

start_link() ->
    SupName = {local, ?MODULE},
    supervisor:start_link(SupName, ?MODULE, []).

%% init/1

init([]) ->
    ets_new(?TABLES),
    diameter_session:init(),
    Flags = {one_for_one, 1, 5},
    ChildSpecs = lists:map(fun spec/1, ?CHILDREN),
    {ok, {Flags, ChildSpecs}}.

%% spec/1

spec(Mod) ->
    {Mod,
     {Mod, start_link, []},
     permanent,
     infinity,
     supervisor,
     [Mod]}.

%% ets_new/1

ets_new(List)
  when is_list(List) ->
    lists:foreach(fun ets_new/1, List);

ets_new({Table, Opts}) ->
    ets:new(Table, [named_table, public | Opts]).

%% tree/0

tree() ->
    [{?MODULE, whereis(?MODULE), tree(?MODULE)}].

tree(Sup) ->
    lists:map(fun t/1, supervisor:which_children(Sup)).

t({Name, Pid, supervisor, _}) ->
    t(Name, Pid, tree(Pid));
t({Name, Pid, worker, _}) ->
    t(Name, Pid).

t(undefined, Pid, Children) ->
    {Pid, Children};
t(Name, Pid, Children) ->
    {Name, Pid, Children}.

t(undefined, Pid) ->
    Pid;
t(Name, Pid) ->
    {Name, Pid}.
