%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2015. All Rights Reserved.
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

-module(erlpmd_sup).

-behaviour(supervisor).

-include("erlpmd_internal.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(_) ->
	{ok, Argv} = application:get_env(erlpmd,argv),
	ErlPMD = {erlpmd, {erlpmd, start_link, [Argv]}, transient, 5000, worker, [erlpmd]},
	Listeners = [{{ip, Ip}, {erlpmd_tcp_listener, start_link, [[Ip,Argv#argv.port]]}, transient, 5000, worker, [erlpmd_tcp_listener]} || Ip <- Argv#argv.address],
	{ok, {{one_for_one, 5, 10}, [ErlPMD | Listeners]}}.

