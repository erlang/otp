%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2010-2025. All Rights Reserved.
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

-module(server).

%%
%% An example Diameter server that answers the base protocol ACR sent
%% by the client example.
%%
%% Simplest usage to listen on TCP at 127.0.0.1:3868:
%%
%%   server:start().
%%   server:listen(tcp).
%%


%% Interface.
-export([start/1,    %% start a service
         start/2,    %%
         listen/2,   %% add a listening transport
         stop/1]).   %% stop a service

%% Convenience functions using the default service name.
-export([start/0,
         listen/1,
         stop/0]).

%% Internal callback.
-export([message/3]).

-define(DEF_SVC_NAME, ?MODULE).

%% The service configuration. In a server supporting multiple Diameter
%% applications each application may have its own, although they could all
%% be configured with a common callback module.
-define(SERVICE(Name), [{'Origin-Host', atom_to_list(Name) ++ ".example.com"},
                        {'Origin-Realm', "example.com"},
                        {'Vendor-Id', 193},
                        {'Product-Name', "Server"},
                        {'Auth-Application-Id', [0]},
                        {decode_format, map},
                        {restrict_connections, false},
                        {strict_mbit, false},
                        {string_decode, false},
                        {application, [{alias, common},
                                       {dictionary, diameter_gen_base_rfc6733},
                                       {module, server_cb},
                                       {call_mutates_state, false}]}]).

%% start/2

start(Name, Opts) ->
    Defaults = [T || {K,_} = T <- ?SERVICE(Name),
                     not lists:keymember(K, 1, Opts)],
    diameter:start_service(Name, Opts ++ Defaults).

%% start/1

start(Opts) ->
    start(?DEF_SVC_NAME, Opts).

%% start/0

start() ->
    start(?DEF_SVC_NAME, []).

%% listen/2

listen(Name, Opts)
  when is_list(Opts) ->
    diameter:add_transport(Name, {listen, lists:flatmap(fun opts/1, Opts)});

%% backwards compatibility with old config
listen(Name, {T, Opts}) ->
    listen(Name, [T | Opts]);
listen(Name, T) ->
    listen(Name, [T]).

%% listen/1

listen(Opts) ->
    listen(?DEF_SVC_NAME, Opts).

%% stop/1

stop(Name) ->
    diameter:stop_service(Name).

%% stop/0

stop() ->
    stop(?DEF_SVC_NAME).

%% ===========================================================================

%% opts/1
%%
%% Map a 3-tuple a transport_module/transport_config pair as a
%% convenience, pass everything else unmodified.

opts(T)
  when T == tcp;
       T == sctp ->
    opts({T, loopback, default});

opts({tcp, Addr, Port}) ->
    opts({diameter_tcp, Addr, Port});

opts({sctp, Addr, Port}) ->
    opts({diameter_sctp, Addr, Port});

opts({Mod, loopback, Port}) ->
    opts({Mod, {127,0,0,1}, Port});

opts({Mod, Addr, default}) ->
    opts({Mod, Addr, 3868});

opts({Mod, Addr, Port}) ->
    [{transport_module, Mod},
     {transport_config, [{reuseaddr, true},
                         {sender, true},
                         {message_cb, {?MODULE, message, [0]}},
                         {ip, Addr},
                         {port, Port}]}];
opts(T) ->
    [T].

%% message/3
%%
%% Simple message callback that limits the number of concurrent
%% requests on the peer connection in question.

%% Incoming request.
message(recv, <<_:32, 1:1, _/bits>> = Bin, N) ->
    [Bin, N < 32, {?MODULE, message, [N+1]}];

%% Outgoing request.
message(ack, <<_:32, 1:1, _/bits>>, _) ->
    [];

%% Incoming answer or request discarded.
message(ack, _, N) ->
    [N =< 32, {?MODULE, message, [N-1]}];

%% Outgoing message or incoming answer.
message(_, Bin, _) ->
    [Bin].
