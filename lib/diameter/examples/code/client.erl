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

-module(client).

%%
%% An example Diameter client that can sends base protocol ACR
%% requests to a connected peer.
%%
%% Simplest usage to connect to a server listening on TCP at
%% 127.0.0.1:3868:
%%
%%   client:start().
%%   client:connect(tcp).
%%   client:call().
%%

-export([start/1,     %% start a service
         start/2,     %%
         connect/2,   %% add a connecting transport
         call/2,      %% send a request
         stop/1]).    %% stop a service

%% Convenience functions using the default service name.
-export([start/0,
         connect/1,
         stop/0,
         call/0,
         call/1]).

-define(DEF_SVC_NAME, ?MODULE).
-define(L, atom_to_list).
-define(LOOPBACK, {127,0,0,1}).

%% Service configuration.
-define(SERVICE(Name), [{'Origin-Host', ?L(Name) ++ ".example.com"},
                        {'Origin-Realm', "example.com"},
                        {'Vendor-Id', 0},
                        {'Product-Name', "Client"},
                        {'Auth-Application-Id', [0]},
                        {decode_format, map},
                        {restrict_connections, false},
                        {strict_mbit, false},
                        {string_decode, false},
                        {application, [{alias, common},
                                       {dictionary, diameter_gen_base_rfc6733},
                                       {module, client_cb},
                                       {answer_errors, callback},
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

%% connect/1

connect(Opts) ->
    connect(?DEF_SVC_NAME, Opts).

%% connect/2

connect(Name, Opts)
  when is_list(Opts) ->
    diameter:add_transport(Name, {connect, lists:flatmap(fun opts/1, Opts)});

%% backwards compatibility with old config
connect(Name, {T, Opts}) ->
    connect(Name, [T | Opts]);
connect(Name, T) ->
    connect(Name, [T]).

%% call/2

call(Name, #{'Session-Id' := _} = Avps) ->
    Defaults = #{'Destination-Realm' => "example.com",
                 'Accounting-Record-Type' => 1,  %% EVENT_RECORD
                 'Accounting-Record-Number' => 0},
    ACR = ['ACR' | maps:merge(Defaults, Avps)],
    diameter:call(Name, common, ACR, []);

call(Name, #{} = Avps) ->
    call(Name, Avps#{'Session-Id' => diameter:session_id(?L(Name))});

call(Name, Avps) ->
    call(Name, maps:from_list(Avps)).

%% call/1

call(Avps) ->
    call(?DEF_SVC_NAME, Avps).

%% call/0

call() ->
    call(?DEF_SVC_NAME, #{}).

%% stop/1

stop(Name) ->
    diameter:stop_service(Name).

stop() ->
    stop(?DEF_SVC_NAME).

%% ===========================================================================

%% opts/1
%%
%% Map some terms to transport_module/transport_config pairs as a
%% convenience, pass everything else unmodified.

opts(T)
  when T == any;
       T == tcp;
       T == sctp ->
   opts({T, loopback, 3868});

opts({T, RA, RP}) ->
    opts({T, [], RA, RP});

opts({T, loopback, RA, RP}) ->
    opts({T, ?LOOPBACK, RA, RP});

opts({T, LA, RA, RP})
  when is_tuple(LA) ->
    opts({T, [{ip, LA}], RA, RP});

opts({any, Opts, RA, RP}) ->
    All = Opts ++ opts(RA, RP),
    [{transport_module, diameter_sctp},
     {transport_config, All, 2000},
     {transport_module, diameter_tcp},
     {transport_config, All}];

opts({tcp, Opts, RA, RP}) ->
    opts({diameter_tcp, Opts, RA, RP});

opts({sctp, Opts, RA, RP}) ->
    opts({diameter_sctp, Opts, RA, RP});

opts({Mod, Opts, loopback, RP}) ->
    opts({Mod, Opts, ?LOOPBACK, RP});

opts({Mod, Opts, RA, default}) ->
    opts({Mod, Opts, RA, 3868});

opts({Mod, Opts, RA, RP}) ->
    [{transport_module, Mod},
     {transport_config, opts(RA, RP) ++ Opts}];

opts(T) ->
    [T].

%% opts/2

opts(loopback, RP) ->
    opts(?LOOPBACK, RP);

opts(RA, RP) ->
    [{raddr, RA}, {rport, RP}, {reuseaddr, true}].
