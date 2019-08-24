%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2002-2018. All Rights Reserved.
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

%%%-------------------------------------------------------------------
%% @doc ftp top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(ftp_sup).

-behaviour(supervisor).

%% API
-export([start_child/1, start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Args) ->
    supervisor:start_child(?MODULE, Args).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
init(_) ->
    SupFlags = #{strategy  => simple_one_for_one,
                 intensity => 0,
                 period    => 3600},
    {ok, {SupFlags, child_specs()}}.


%%====================================================================
%% Internal functions
%%====================================================================
child_specs() ->
    [#{id => undefined,
       start => {ftp, start_link, []},
       restart => temporary,
       shutdown => 4000,
       type => worker,
       modules => [ftp]}].
