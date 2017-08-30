%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% Purpose: Ssh connection supervisor.
%%----------------------------------------------------------------------

-module(httpd_connection_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).
-export([start_child/2, connection_sup/2]).

%% Supervisor callback
-export([init/1]).

%%%=========================================================================
%%%  API
%%%=========================================================================
start_link(Args) ->
    supervisor:start_link(?MODULE, [Args]).

start_child(Sup, Args) ->
    supervisor:start_child(Sup, Args).

connection_sup(Addr, Port) ->
    httpd_util:make_name("httpd_connection_sup", Addr, Port).

%%%=========================================================================
%%%  Supervisor callback
%%%=========================================================================
init([[Addr, Port]]) ->
    RegName = connection_sup(Addr, Port),
    register(RegName, self()),
    RestartStrategy = simple_one_for_one,
    MaxR = 0,
    MaxT = 3600,

    Name = undefined, % As simple_one_for_one is used.
    StartFunc = {httpd_request_handler, start_link, []},
    Restart = temporary, % E.g. should not be restarted
    Shutdown = 4000,
    Modules = [httpd_request_handler],
    Type = worker,

    ChildSpec = {Name, StartFunc, Restart, Shutdown, Type, Modules},
    {ok, {{RestartStrategy, MaxR, MaxT}, [ChildSpec]}}.



