%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2022. All Rights Reserved.
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

-module(ssl_session_cache_api_SUITE).

-behaviour(ct_suite).

-include_lib("common_test/include/ct.hrl").
-include("tls_handshake.hrl").

%% Callback functions
-export([all/0,
        init_per_suite/1,
         end_per_suite/1]).

%% Testcases
-export([server_cb/0,
         server_cb/1,
         client_cb/0,
         client_cb/1
        ]).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------
all() ->
    [server_cb,
     client_cb].

init_per_suite(Config0) ->
    catch crypto:stop(),
    try crypto:start() of
	ok ->
            ssl_test_lib:clean_start(),
            Config0
    catch _:_ ->
	    {skip, "Crypto did not start"}
    end.

end_per_suite(_Config) ->
    ssl:stop(),
    application:stop(crypto).

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------
server_cb() ->
    [{doc, "Test ssl_session_cache_api server callback implementation"
      "Note that default implementation is treated special to avoid"
      "an extra process to handle the functional db structure used"
      "The callback is provided for the main purpose of having a"
      "session table that may survive node restart an assumes
      a db reference that is not updated"
     }].

server_cb(Config) when is_list(Config) ->
    Cb = ssl_server_session_cache_db,
    Db0 = Cb:init([]),
    Id0 = crypto:strong_rand_bytes(32),
    DummySession0 = #session{session_id = Id0},
    0 = Cb:size(Db0),

    Db1 = Cb:update(Db0, Id0, DummySession0),
    1 = Cb:size(Db1),

    Id1 = crypto:strong_rand_bytes(32),
    DummySession1 = #session{session_id = Id1},
    Db2 = Cb:update(Db1, Id1, DummySession1),
    2 = Cb:size(Db2),

    DummySession0 = Cb:lookup(Db2, Id0),
    DummySession1 = Cb:lookup(Db2, Id1),

    Db3 = Cb:delete(Db2, Id1),
    1 = Cb:size(Db3),
    undefined = Cb:lookup(Db3, crypto:strong_rand_bytes(32)),
    Cb:terminate(Db3).

client_cb() ->
    [{doc, "Test ssl_session_cache_api client callback implementation"}].

client_cb(Config) when is_list(Config) ->
    Cb = ssl_client_session_cache_db,
    Db = Cb:init([]),
    Id0 = crypto:strong_rand_bytes(32),
    DummySession0 = #session{session_id = Id0},
    0 = Cb:size(Db),

    Cb:update(Db, Id0, DummySession0),
    1 = Cb:size(Db),

    Id1 = crypto:strong_rand_bytes(32),
    DummySession1 = #session{session_id = Id1},
    Cb:update(Db, Id1, DummySession1),
    2 = Cb:size(Db),

    DummySession0 = Cb:lookup(Db, Id0),
    DummySession1 = Cb:lookup(Db, Id1),
    undefined = Cb:lookup(Db, crypto:strong_rand_bytes(32)),

    Cb:delete(Db, Id1),
    1 = Cb:size(Db),
    Cb:terminate(Db).
