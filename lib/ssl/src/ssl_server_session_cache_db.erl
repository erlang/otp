%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2020-2024. All Rights Reserved.
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

%%----------------------------------------------------------------------
%% Purpose: Handle server side TLS-1.2 session storage
%%----------------------------------------------------------------------

-module(ssl_server_session_cache_db).
-moduledoc false.

-behaviour(ssl_session_cache_api).

%% API
-export([init/1,
         terminate/1,
         lookup/2,
         update/3,
         delete/2,
         size/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% Description: Return table reference.
%%--------------------------------------------------------------------
init(_Options) ->
    gb_trees:empty().

%%--------------------------------------------------------------------
%% Description: Looks up a cache entry. Should be callable from any
%% process.
%%--------------------------------------------------------------------
lookup(Cache, Key) ->
    case gb_trees:lookup(Key, Cache) of
	{value, Session} ->
	    Session;
	none ->
	    undefined
    end.

%%--------------------------------------------------------------------
%% Description: Caches a new session or updates a already cached one.
%% Will only be called from the ssl_server_cache process.
%%--------------------------------------------------------------------
update(Cache, Key, Session) ->
    gb_trees:insert(Key, Session, Cache).

%%--------------------------------------------------------------------
%% Description: Deletes a cache entry.
%% Will only be called from the ssl_server_cache process.
%%--------------------------------------------------------------------
delete(Cache, Key) ->
    gb_trees:delete(Key, Cache).

%%--------------------------------------------------------------
%% Description: Returns the cache size
%%--------------------------------------------------------------------
size(Cache) ->
    gb_trees:size(Cache).

terminate(_) ->
    ok.
