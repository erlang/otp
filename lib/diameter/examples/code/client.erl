%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2017. All Rights Reserved.
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
%% An example Diameter client that can sends base protocol RAR
%% requests to a connected peer.
%%
%% The simplest usage is as follows this to connect to a server
%% listening on the default port on the local host, assuming diameter
%% is already started (eg. diameter:start()).
%%
%%   client:start().
%%   client:connect(tcp).
%%   client:call().
%%
%% The first call starts the a service with the default name of
%% ?MODULE, the second defines a connecting transport that results in
%% a connection to the peer (if it's listening), the third sends it a
%% RAR and returns the answer.
%%

-module(client).

-include_lib("diameter/include/diameter.hrl").

-export([start/1,     %% start a service
         start/2,     %%
         connect/2,   %% add a connecting transport
         call/1,      %% send using the record encoding
         cast/1,      %% send using the list encoding and detached
         stop/1]).    %% stop a service
%% A real application would typically choose an encoding and whether
%% they want the call to return the answer or not. Sending with
%% both the record and list encoding here, one detached and one not,
%% is just for demonstration purposes.

%% Convenience functions using the default service name.
-export([start/0,
         connect/1,
         stop/0,
         call/0,
         cast/0]).

-define(DEF_SVC_NAME, ?MODULE).
-define(L, atom_to_list).

%% The service configuration. As in the server example, a client
%% supporting multiple Diameter applications may or may not want to
%% configure a common callback module on all applications.
-define(SERVICE(Name), [{'Origin-Host', ?L(Name) ++ ".example.com"},
                        {'Origin-Realm', "example.com"},
                        {'Vendor-Id', 0},
                        {'Product-Name', "Client"},
                        {'Auth-Application-Id', [0]},
                        {string_decode, false},
                        {decode_format, map},
                        {application, [{alias, common},
                                       {dictionary, diameter_gen_base_rfc6733},
                                       {module, client_cb}]}]).

%% start/1

start(Name)
  when is_atom(Name) ->
    start(Name, []);

start(Opts)
  when is_list(Opts) ->
    start(?DEF_SVC_NAME, Opts).

%% start/0

start() ->
    start(?DEF_SVC_NAME).

%% start/2

start(Name, Opts) ->
    node:start(Name, Opts ++ [T || {K,_} = T <- ?SERVICE(Name),
                                   false == lists:keymember(K, 1, Opts)]).

%% connect/2

connect(Name, T) ->
    node:connect(Name, T).

connect(T) ->
    connect(?DEF_SVC_NAME, T).

%% call/1

call(Name) ->
    SId = diameter:session_id(?L(Name)),
    RAR = ['RAR' | #{'Session-Id' => SId,
                     'Auth-Application-Id' => 0,
                     'Re-Auth-Request-Type' => 0}],
    diameter:call(Name, common, RAR, []).

call() ->
    call(?DEF_SVC_NAME).

%% cast/1

cast(Name) ->
    SId = diameter:session_id(?L(Name)),
    RAR = ['RAR', {'Session-Id', SId},
                  {'Auth-Application-Id', 0},
                  {'Re-Auth-Request-Type', 1}],
    diameter:call(Name, common, RAR, [detach]).

cast() ->
    cast(?DEF_SVC_NAME).

%% stop/1

stop(Name) ->
    node:stop(Name).

stop() ->
    stop(?DEF_SVC_NAME).
