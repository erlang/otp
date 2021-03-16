%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2016. All Rights Reserved.
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

%%%=========================================================================
%%% Purpose : Application master and top supervisors for SSH.
%%%
%%%  -----> ssh_sup -----+-----> sshc_sup --+--> "system sup" (etc)
%%%                      |                  |
%%%                      |                  +--> "system sup" (etc)
%%%                      |                  :
%%%                      |                  +--> "system sup" (etc)
%%%                      |
%%%                      +-----> sshc_sup --+--> "system sup" (etc)
%%%                                         |
%%%                                         +--> "system sup" (etc)
%%%                                         :
%%%                                         +--> "system sup" (etc)

-module(ssh_app).

-behaviour(application).
-behaviour(supervisor).

%% 'application' export:
-export([start/2, stop/1]).

%% 'supervisor' export:
-export([init/1]).


%%%=========================================================================
%%%  Application callback
%%%=========================================================================
start(_Type, _State) ->
    supervisor:start_link({local,ssh_sup}, ?MODULE, [ssh_sup]).

stop(_State) ->
    ok.

%%%=========================================================================
%%%  Supervisor callback
%%%=========================================================================
init([ssh_sup]) ->
    SupFlags = #{strategy  => one_for_one,
                 intensity =>   10,
                 period    => 3600
                },
    ChildSpecs = [#{id       => SupName,
                    start    => {supervisor, start_link,
                                 [{local,SupName}, ?MODULE, [sshX_sup]]},
                    type     => supervisor}
                  || SupName <- [sshd_sup, sshc_sup]
                 ],
    {ok, {SupFlags,ChildSpecs}};

init([sshX_sup]) ->
    SupFlags = #{strategy  => one_for_one,
                 intensity =>   10,
                 period    => 3600
                },
    ChildSpecs = [],
    {ok, {SupFlags,ChildSpecs}}.
