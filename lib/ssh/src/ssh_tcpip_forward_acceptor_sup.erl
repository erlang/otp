%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2008-2025. All Rights Reserved.
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
%% Purpose: The supervisor for tcpip-forwarding acceptor
%%----------------------------------------------------------------------

-module(ssh_tcpip_forward_acceptor_sup).
-moduledoc false.
-behaviour(supervisor).

-include("ssh.hrl").

-export([start_link/0, start_child/7]).

%% Supervisor callback
-export([init/1]).

%%%=========================================================================
%%%  API
%%%=========================================================================
start_link() ->
    supervisor:start_link(?MODULE, []).

start_child(Sup, LSock, ListenAddr, ConnectToAddr, ChanType, ChanCB, ConnPid) ->
    Args = [LSock, ListenAddr, ConnectToAddr, ChanType, ChanCB, ConnPid],
    supervisor:start_child(Sup, 
                           #{id     => {ListenAddr,ConnectToAddr},
                             start  => {ssh_tcpip_forward_acceptor, start_link, Args}
                            }).
    

%%%=========================================================================
%%%  Supervisor callback
%%%=========================================================================
init([]) ->
    ssh_lib:set_label(tcpip_fw_acceptor_sup),
    SupFlags = #{strategy  => one_for_one, 
                 intensity =>   10,
                 period    => 3600
                },
    ChildSpecs = [],
    {ok, {SupFlags,ChildSpecs}}.

%%%=========================================================================
%%%  Internal functions
%%%=========================================================================
