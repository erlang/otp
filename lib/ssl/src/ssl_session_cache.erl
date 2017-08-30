%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
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
-module(ssl_session_cache).

-behaviour(ssl_session_cache_api).

-include("ssl_handshake.hrl").
-include("ssl_internal.hrl").

-export([init/1, terminate/1, lookup/2, update/3, delete/2, foldl/3, 
	 select_session/2, size/1]). 

%%--------------------------------------------------------------------
%% Description: Return table reference. Called by ssl_manager process. 
%%--------------------------------------------------------------------
init(Options) ->
    ets:new(cache_name(proplists:get_value(role, Options)), [ordered_set, protected]).

%%--------------------------------------------------------------------
%% Description: Handles cache table at termination of ssl manager. 
%%--------------------------------------------------------------------
terminate(Cache) ->
    ets:delete(Cache).

%%--------------------------------------------------------------------
%% Description: Looks up a cach entry. Should be callable from any
%% process.
%%--------------------------------------------------------------------
lookup(Cache, Key) ->
    case ets:lookup(Cache, Key) of
	[{Key, Session}] ->
	    Session;
	[] ->
	    undefined
    end.

%%--------------------------------------------------------------------
%% Description: Caches a new session or updates a already cached one.
%% Will only be called from the ssl_manager process.
%%--------------------------------------------------------------------
update(Cache, Key, Session) ->
    ets:insert(Cache, {Key, Session}).

%%--------------------------------------------------------------------
%% Description: Delets a cache entry.
%% Will only be called from the ssl_manager process.
%%--------------------------------------------------------------------
delete(Cache, Key) ->
    ets:delete(Cache, Key).

%%--------------------------------------------------------------------
%% Description: Calls Fun(Elem, AccIn) on successive elements of the
%% cache, starting with AccIn == Acc0. Fun/2 must return a new
%% accumulator which is passed to the next call. The function returns
%% the final value of the accumulator. Acc0 is returned if the cache
%% is empty.Should be callable from any process
%%--------------------------------------------------------------------
foldl(Fun, Acc0, Cache) ->
    ets:foldl(Fun, Acc0, Cache).
  
%%--------------------------------------------------------------------
%% Description: Selects a session that could be reused. Should be callable
%% from any process.
%%--------------------------------------------------------------------
select_session(Cache, PartialKey) ->    
    ets:select(Cache, 
	       [{{{PartialKey,'_'}, '$1'},[],['$1']}]).

%%--------------------------------------------------------------------
%% Description: Returns the cache size
%%--------------------------------------------------------------------
size(Cache) ->
    ets:info(Cache, size).

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
cache_name(Name) ->
    list_to_atom(atom_to_list(Name) ++ "_ssl_otp_session_cache").
