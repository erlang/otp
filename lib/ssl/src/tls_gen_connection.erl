%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2020. All Rights Reserved.
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
%% Purpose: Provid help function to handle generic parts of TLS
%% connection fsms
%%----------------------------------------------------------------------

-module(tls_gen_connection).

-include("ssl_internal.hrl").

%% Initial setup
-export([start_link/8, init/1]).

%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
-spec start_link(client| server, pid(), ssl:host(), inet:port_number(), port(), list(), pid(), tuple()) ->
    {ok, pid()} | ignore |  {error, reason()}.
%%
%% Description: Creates a process which calls Module:init/1 to
%% choose appropriat gen_statem and initialize.
%%--------------------------------------------------------------------
start_link(Role, Sender, Host, Port, Socket, Options, User, CbInfo) ->
    {ok, proc_lib:spawn_link(?MODULE, init, [[Role, Sender, Host, Port, Socket, Options, User, CbInfo]])}.

%%--------------------------------------------------------------------
-spec init(list()) -> no_return().
%% Description: Initialization
%%--------------------------------------------------------------------
init([_Role, Sender, _Host, _Port, _Socket, {#{erl_dist := ErlDist} = TLSOpts, _, _},  _User, _CbInfo] = InitArgs) ->
    process_flag(trap_exit, true),
    link(Sender),
    case ErlDist of
        true ->
            process_flag(priority, max);
        _ ->
            ok
    end,
    ConnectionFsm = connection_fsm(TLSOpts),
    ConnectionFsm:init(InitArgs).

connection_fsm(#{versions := ['tlsv1.3']}) ->
    tls_connection_1_3;
connection_fsm(_) ->
    tls_connection.
