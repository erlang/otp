%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2011. All Rights Reserved.
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
-module(ssl_session_cache).

-behaviour(ssl_session_cache_api).

-include("ssl_handshake.hrl").
-include("ssl_internal.hrl").

-export([init/1, terminate/1, lookup/2, update/3, delete/2, foldl/3, 
	 select_session/2]). 

-type key() :: {{host(), inet:port_number()}, session_id()} |  {inet:port_number(), session_id()}.

%%--------------------------------------------------------------------
-spec init(list()) -> db_handle(). %% Returns reference to the cache (opaque)
%%
%% Description: Return table reference. Called by ssl_manager process. 
%%--------------------------------------------------------------------
init(_) ->
    ets:new(cache_name(), [set, protected]).

%%--------------------------------------------------------------------
-spec terminate(db_handle()) -> any().
%%
%% Description: Handles cache table at termination of ssl manager. 
%%--------------------------------------------------------------------
terminate(Cache) ->
    ets:delete(Cache).

%%--------------------------------------------------------------------
-spec lookup(db_handle(), key()) -> #session{} | undefined.
%%
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
-spec update(db_handle(), key(), #session{}) -> any().
%%
%% Description: Caches a new session or updates a already cached one.
%% Will only be called from the ssl_manager process.
%%--------------------------------------------------------------------
update(Cache, Key, Session) ->
    ets:insert(Cache, {Key, Session}).

%%--------------------------------------------------------------------
-spec delete(db_handle(), key()) -> any().
%%
%% Description: Delets a cache entry.
%% Will only be called from the ssl_manager process.
%%--------------------------------------------------------------------
delete(Cache, Key) ->
    ets:delete(Cache, Key).

%%--------------------------------------------------------------------
-spec foldl(fun(), term(), db_handle()) -> term().
%%
%% Description: Calls Fun(Elem, AccIn) on successive elements of the
%% cache, starting with AccIn == Acc0. Fun/2 must return a new
%% accumulator which is passed to the next call. The function returns
%% the final value of the accumulator. Acc0 is returned if the cache
%% is empty.Should be callable from any process
%%--------------------------------------------------------------------
foldl(Fun, Acc0, Cache) ->
    ets:foldl(Fun, Acc0, Cache).
  
%%--------------------------------------------------------------------
-spec select_session(db_handle(), {host(), inet:port_number()} | inet:port_number()) -> [#session{}].
%%
%% Description: Selects a session that could be reused. Should be callable
%% from any process.
%%--------------------------------------------------------------------
select_session(Cache, PartialKey) ->    
    ets:select(Cache, 
	       [{{{PartialKey,'$1'}, '$2'},[],['$$']}]).

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
cache_name() ->
    ssl_otp_session_cache.
