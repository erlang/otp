%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2009. All Rights Reserved.
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
%% Purpose: The top supervisor for the ftp hangs under inets_sup.
%%----------------------------------------------------------------------
-module(ftp_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([start_child/1]).

%% Supervisor callback
-export([init/1]).

%%%=========================================================================
%%%  API
%%%=========================================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Args) ->
    supervisor:start_child(?MODULE, Args).
    
%%%=========================================================================
%%%  Supervisor callback
%%%=========================================================================
init(_) ->
    RestartStrategy = simple_one_for_one,
    MaxR = 0,
    MaxT = 3600,
   
    Name = undefined, % As simple_one_for_one is used.
    StartFunc = {ftp, start_link, []},
    Restart = temporary, % E.g. should not be restarted
    Shutdown = 4000,
    Modules = [ftp],
    Type = worker,
    
    ChildSpec = {Name, StartFunc, Restart, Shutdown, Type, Modules},
    {ok, {{RestartStrategy, MaxR, MaxT}, [ChildSpec]}}.
