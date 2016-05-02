%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2016. All Rights Reserved.
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

-module(redirect).

-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc3588.hrl").

-export([start/1,
         listen/2,
         stop/1]).

-export([start/0,
         listen/1,
         stop/0]).

-define(APP_ALIAS,    ?MODULE).
-define(SVC_NAME,     ?MODULE).
-define(CALLBACK_MOD, redirect_mod).

%% The service configuration.
-define(SERVICE(Name), [{'Origin-Host', atom_to_list(Name) ++ ".example.com"},
                        {'Origin-Realm', "example.com"},
                        {'Vendor-Id', 193},
                        {'Product-Name', "RedirectAgent"},
                        {'Auth-Application-Id', [?DIAMETER_APP_ID_RELAY]},
                        {application, [{alias, ?APP_ALIAS},
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

%% stop/1

stop(Name) ->
    peer:stop(Name).

stop() ->
    stop(?SVC_NAME).
