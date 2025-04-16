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
%% Purpose: The acceptor supervisor for ssh servers hangs under
%%          ssh_system_sup.
%%----------------------------------------------------------------------

-module(ssh_acceptor_sup).
-moduledoc false.
-behaviour(supervisor).

-include("ssh.hrl").

-export([start_link/3,
         restart_child/2
        ]).

%% Supervisor callback
-export([init/1]).

-behaviour(ssh_dbg).
-export([ssh_dbg_trace_points/0, ssh_dbg_flags/1, ssh_dbg_on/1, ssh_dbg_off/1,
         ssh_dbg_format/2]).

%%%=========================================================================
%%%  API
%%%=========================================================================
start_link(SystemSup, Address, Options) ->
    case supervisor:start_link(?MODULE, [SystemSup, Address, Options]) of
        {error, {shutdown, {failed_to_start_child, _, Error}}} ->
            {error,Error};
        Other ->
            Other
    end.

restart_child(AccSup, Address) ->
    supervisor:restart_child(AccSup, {?MODULE,Address}).

%%%=========================================================================
%%%  Supervisor callback
%%%=========================================================================
init([SystemSup, Address, Options]) ->
    ssh_lib:set_label(server, acceptor_sup),
    %% Initial start of ssh_acceptor_sup for this port
    {LSocket, _LHost, _LPort, ProviderPid} =
        ?GET_INTERNAL_OPT(lsocket, Options, undefined),
    request_ownership(LSocket, ProviderPid),
    SupFlags = #{strategy  => one_for_one,
                 intensity =>   10,
                 period    => 3600
                },
    ChildSpecs = [#{id       => {?MODULE,Address},
                    start    => {ssh_acceptor, start_link, [SystemSup, Address, Options]},
                    restart  => transient % because a crashed listener could be replaced by a new one
                   }
                 ],
    {ok, {SupFlags,ChildSpecs}}.

%%%=========================================================================
%%%  Internal functions
%%%=========================================================================
request_ownership(LSocket, SockProvider) ->
    SockProvider ! {request_control,LSocket,self()},
    receive
	{its_yours,LSocket} ->
            ok
    after ?DEFAULT_TIMEOUT ->
            no_response_from_socket_provider
    end.

%%%################################################################
%%%#
%%%# Tracing
%%%#

ssh_dbg_trace_points() -> [connections].

ssh_dbg_flags(connections) -> [c].

ssh_dbg_on(connections) ->
    dbg:tpl(?MODULE, start_link, 3, x),
    dbg:tpl(?MODULE, restart_child, 2, x),
    dbg:tpl(?MODULE, request_ownership, 2, x),
    dbg:tpl(?MODULE, init, 1, x).

ssh_dbg_off(connections) ->
    dbg:ctpl(?MODULE, start_link, 3),
    dbg:ctpl(?MODULE, restart_child, 2),
    dbg:ctpl(?MODULE, request_ownership, 2),
    dbg:ctpl(?MODULE, init, 1).

ssh_dbg_format(Tracepoint, Event = {call, {?MODULE, Function, Args}}) ->
    [io_lib:format("~w:~w/~w> ~s", [?MODULE, Function, length(Args)] ++
                       ssh_dbg_comment(Tracepoint, Event))];
ssh_dbg_format(Tracepoint, Event = {return_from, {?MODULE,Function,Arity}, Ret}) ->
    [io_lib:format("~w:~w/~w returned ~W> ~s", [?MODULE, Function, Arity, Ret, 2] ++
                  ssh_dbg_comment(Tracepoint, Event))].

ssh_dbg_comment(connections, {call, {?MODULE, init, [[LSocket | _]]}}) ->
    [io_lib:format("LSocket ~p", [LSocket])];
ssh_dbg_comment(connections, {call, {?MODULE, request_ownership, [LSocket, SockProvider]}}) ->
    [io_lib:format("LSocket ~p SockProvider ~p", [LSocket, SockProvider])];
ssh_dbg_comment(_, _) ->
    [""].

