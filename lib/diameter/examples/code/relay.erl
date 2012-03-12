%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2012. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%

%%
%% An example Diameter relay agent.
%%
%% Usage to connect to a server listening on the default port over TCP
%% and to listen on the default port over SCTP is as follows, assuming
%% diameter is already started (eg. diameter:start()).
%%
%% Eg.  relay:start().
%%      relay:connect(tcp).
%%      relay:listen(sctp).
%%

-module(relay).

-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc3588.hrl").

-export([start/1,
         listen/2,
         connect/2,
         stop/1]).

-export([start/0,
         listen/1,
         connect/1,
         stop/0]).

-define(APP_ALIAS,    ?MODULE).
-define(SVC_NAME,     ?MODULE).
-define(CALLBACK_MOD, relay_cb).

%% The service configuration.
-define(SERVICE(Name), [{'Origin-Host', atom_to_list(Name) ++ ".example.com"},
                        {'Origin-Realm', "example.com"},
                        {'Vendor-Id', 193},
                        {'Product-Name', "RelayAgent"},
                        {'Auth-Application-Id', [?DIAMETER_APP_ID_RELAY]},
                        {application, [{alias, ?MODULE},
                                       {dictionary, ?DIAMETER_DICT_RELAY},
                                       {module, ?CALLBACK_MOD}]}]).

%% start/1

start(Name)
  when is_atom(Name) ->
    peer:start(Name, ?SERVICE(Name)).

start() ->
    start(?SVC_NAME).

%% listen/2

listen(Name, T) ->
    peer:listen(Name, T).

listen(T) ->
    listen(?SVC_NAME, T).

%% connect/2

connect(Name, T) ->
    peer:connect(Name, T).

connect(T) ->
    connect(?SVC_NAME, T).

%% stop/1

stop(Name) ->
    peer:stop(Name).

stop() ->
    stop(?SVC_NAME).
