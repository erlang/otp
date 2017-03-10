%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 20016-2017. All Rights Reserved.
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
%% Purpose: Manages ssl sessions and trusted certifacates
%%----------------------------------------------------------------------

-module(ssl_pem_cache).
-behaviour(gen_server).

%% Internal application API
-export([start_link/1, 
	 start_link_dist/1,
	 name/1,
	 insert/1,
	 clear/0]).

% Spawn export
-export([init_pem_cache_validator/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("ssl_handshake.hrl").
-include("ssl_internal.hrl").
-include_lib("kernel/include/file.hrl").

-record(state, {
	  pem_cache,                  
	  last_pem_check             :: erlang:timestamp(),
	  clear            :: integer()
	 }).

-define(CLEAR_PEM_CACHE, 120000).
-define(DEFAULT_MAX_SESSION_CACHE, 1000).

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
-spec name(normal | dist) -> atom().
%%
%% Description: Returns the registered name of the ssl cache process
%% in the operation modes 'normal' and 'dist'.
%%--------------------------------------------------------------------
name(normal) ->
    ?MODULE;
name(dist) ->
    list_to_atom(atom_to_list(?MODULE) ++ "dist").

%%--------------------------------------------------------------------
-spec start_link(list()) -> {ok, pid()} | ignore | {error, term()}.
%%
%% Description: Starts the ssl pem cache handler 
%%--------------------------------------------------------------------
start_link(_) ->
    CacheName = name(normal),
    gen_server:start_link({local, CacheName}, 
			  ?MODULE, [CacheName], []).

%%--------------------------------------------------------------------
-spec start_link_dist(list()) -> {ok, pid()} | ignore | {error, term()}.
%%
%% Description: Starts a special instance of the ssl manager to
%% be used by the erlang distribution. Note disables soft upgrade!
%%--------------------------------------------------------------------
start_link_dist(_) ->
    DistCacheName = name(dist),
    gen_server:start_link({local, DistCacheName}, 
			  ?MODULE, [DistCacheName], []).


%%--------------------------------------------------------------------
-spec insert(binary()) -> {ok, term()} | {error, reason()}.
%%		    
%% Description: Cache a pem file and return its content.
%%--------------------------------------------------------------------
insert(File) ->    
    {ok, PemBin} = file:read_file(File),
    Content = public_key:pem_decode(PemBin),
    case bypass_cache() of
	true ->
	    {ok, Content};
	false ->
	    cast({cache_pem, File, Content}),
	    {ok, Content}
    end.

%%--------------------------------------------------------------------
-spec clear() -> ok.
%%
%% Description: Clear the PEM cache
%%--------------------------------------------------------------------
clear() ->
    %% Not supported for distribution at the moement, should it be?
    put(ssl_pem_cache, name(normal)),
    call(unconditionally_clear_pem_cache).

-spec invalidate_pem(File::binary()) -> ok.
invalidate_pem(File) ->
    cast({invalidate_pem, File}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
-spec init(list()) -> {ok, #state{}}.
%% Possible return values not used now. 
%% |  {ok, #state{}, timeout()} | ignore | {stop, term()}.		  
%%
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Name]) ->
    put(ssl_pem_cache, Name),
    process_flag(trap_exit, true),
    PemCache = ssl_pkix_db:create_pem_cache(Name),
    Interval = pem_check_interval(),
    erlang:send_after(Interval, self(), clear_pem_cache),
    {ok, #state{pem_cache = PemCache,
		last_pem_check =  os:timestamp(),
		clear = Interval 	
	       }}.

%%--------------------------------------------------------------------
-spec handle_call(msg(), from(), #state{}) -> {reply, reply(), #state{}}. 
%% Possible return values not used now.  
%%					      {reply, reply(), #state{}, timeout()} |
%%					      {noreply, #state{}} |
%%					      {noreply, #state{}, timeout()} |
%%					      {stop, reason(), reply(), #state{}} |
%%					      {stop, reason(), #state{}}.
%%
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({unconditionally_clear_pem_cache, _},_, 
	    #state{pem_cache = PemCache} = State) ->
    ssl_pkix_db:clear(PemCache),
    {reply, ok,  State}.

%%--------------------------------------------------------------------
-spec  handle_cast(msg(), #state{}) -> {noreply, #state{}}.
%% Possible return values not used now.  
%%				      | {noreply, #state{}, timeout()} |
%%				       {stop, reason(), #state{}}.
%%
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({cache_pem, File, Content}, #state{pem_cache = Db} = State) ->
    ssl_pkix_db:insert(File, Content, Db), 
    {noreply, State};

handle_cast({invalidate_pem, File}, #state{pem_cache = Db} = State) ->
    ssl_pkix_db:remove(File, Db),
    {noreply, State}.


%%--------------------------------------------------------------------
-spec handle_info(msg(), #state{}) -> {noreply, #state{}}.
%% Possible return values not used now.
%%				      |{noreply, #state{}, timeout()} |
%%				      {stop, reason(), #state{}}.
%%
%% Description: Handling all non call/cast messages
%%-------------------------------------------------------------------
handle_info(clear_pem_cache, #state{pem_cache = PemCache,
				    clear = Interval,
				    last_pem_check = CheckPoint} = State) ->
    NewCheckPoint = os:timestamp(),
    start_pem_cache_validator(PemCache, CheckPoint),
    erlang:send_after(Interval, self(), clear_pem_cache),
    {noreply, State#state{last_pem_check = NewCheckPoint}};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
-spec terminate(reason(), #state{}) -> ok.
%%		       
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, #state{}) ->
    ok.

%%--------------------------------------------------------------------
-spec code_change(term(), #state{}, list()) -> {ok, #state{}}.			 
%%
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
call(Msg) ->
    gen_server:call(get(ssl_pem_cache), {Msg, self()}, infinity).

cast(Msg) ->
    gen_server:cast(get(ssl_pem_cache), Msg).

start_pem_cache_validator(PemCache, CheckPoint) ->
    spawn_link(?MODULE, init_pem_cache_validator, 
	       [[get(ssl_pem_cache), PemCache, CheckPoint]]).

init_pem_cache_validator([CacheName, PemCache, CheckPoint]) ->
    put(ssl_pem_cache, CacheName),
    ssl_pkix_db:foldl(fun pem_cache_validate/2,
		      CheckPoint, PemCache).

pem_cache_validate({File, _}, CheckPoint) ->
    case file:read_file_info(File, []) of
	{ok, #file_info{mtime = Time}} ->
	    case is_before_checkpoint(Time, CheckPoint) of
		true ->
		    ok;
		false ->
		    invalidate_pem(File)
	    end;
	_  ->
	    invalidate_pem(File)
    end,
    CheckPoint.

is_before_checkpoint(Time, CheckPoint) ->
    calendar:datetime_to_gregorian_seconds(
      calendar:now_to_datetime(CheckPoint)) -
	calendar:datetime_to_gregorian_seconds(Time) > 0.

pem_check_interval() ->
    case application:get_env(ssl, ssl_pem_cache_clean) of
	{ok, Interval} when is_integer(Interval) ->
	    Interval;
	_  ->
	    ?CLEAR_PEM_CACHE
    end.

bypass_cache() ->
    case application:get_env(ssl, bypass_pem_cache) of
	{ok, Bool} when is_boolean(Bool) ->
	    Bool;
	_ ->
	    false
    end.
