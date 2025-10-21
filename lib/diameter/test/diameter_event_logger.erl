%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2025-2025. All Rights Reserved.
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
%% Tests of traffic between two Diameter nodes, the client being
%% spread across three Erlang nodes.
%%

-module(diameter_event_logger).

-behaviour(gen_server).

%% External exports
-export([start_link/0,
	 stop/0,
	 register_service/1,
	 unregister_service/1]).

%% Internal (gen_server) exports
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


-include("diameter.hrl").
-include("diameter_util.hrl").

-define(SERVER, ?MODULE).

-record(state, {clients}).


%%-----------------------------------------------------------------
%% Interface functions.
%%-----------------------------------------------------------------

start_link() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    call(?FUNCTION_NAME).

register_service(SvcName) ->
    call({?FUNCTION_NAME, SvcName}).

unregister_service(SvcName) ->
    call({?FUNCTION_NAME, SvcName}).


%%-----------------------------------------------------------------
%% Callback functions for the gen_server
%%-----------------------------------------------------------------

init([]) ->
    {ok, #state{clients = #{}}}.


handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call({register_service, SvcName},
	    {Client, _Tag} = _From,
	    #state{clients = Clients} = State) ->
    Clients2 = handle_register_service(SvcName, Client, Clients),
    {reply, ok, State#state{clients = Clients2}};

handle_call({unregister_service, SvcName},
	    {Client, _Tag} = _From,
	    #state{clients = Clients} = State) ->
    Clients2 = handle_unregister_service(SvcName, Client, Clients),
    {reply, ok, State#state{clients = Clients2}};

handle_call(Req, From, State) ->
    warning_msg("received unexpected request from ~p: ~n   ~p",
		[From, Req]),
    {reply, {error, {unknown_request, Req}}, State}.


handle_cast(Msg, State) ->
    warning_msg("received unexpected message: ~n   ~p", [Msg]),
    {noreply, State}.


handle_info({'DOWN', MRef, process, Pid, Info},
	    #state{clients = Clients} = State) ->
    Clients2 = handle_down(MRef, Pid, Info, Clients),
    {noreply, State#state{clients = Clients2}};

handle_info(#diameter_event{} = DEvent, State) ->
    handle_diameter_event(DEvent),
    {noreply, State};

handle_info(Info, State) ->
    warning_msg("received unexpected info: ~n   ~p",
		[Info]),
    {noreply, State}.


terminate(Reason, #state{clients = Clients} = _State) ->
    if
	(Reason =/= normal) ->
	    error_msg("Terminating: "
		      "~n   ~p", [Reason]);
	true ->
	    ok
    end,
    Unsubscribe = fun({Pid, SvcName}) ->
			  info_msg("Unsubscribing from service ~p~n"
				   "on behalf of client ~p",
				   [SvcName, Pid]),
			  diameter:unsubscribe(SvcName)
		  end,
    ClientIterator = fun(Client, {MRef, Services}) ->
			     erlang:demonitor(MRef),
			     lists:foreach(fun(SvcName) ->
						   Unsubscribe(Client, SvcName)
					   end, Services)
	       end,
    maps:foreach(ClientIterator, Clients),
    ok.


%% downgrade
code_change({down, _Vsn}, State, _Extra) ->
    {ok, State};

%% upgrade
code_change(_Vsn, State, _Extra) ->
    {ok, State}.


%% ===========================================================================

handle_register_service(SvcName, Client, Clients) ->
    case maps:find(Client, Clients) of
	error ->
	    %% First time for this client
	    MRef     = erlang:monitor(process, Client),
	    Clients2 = Clients#{Client => {MRef, [SvcName]}},
	    info_msg("Subscribing to service ~p~n"
		     "on behalf of new client ~p",
		     [SvcName, Client]),
	    diameter:subscribe(SvcName),
	    Clients2;
	{ok, {MRef, Services}} ->
	    %% This client already has atleast one service
	    %% Check if this client already handle *this* service
	    case lists:member(SvcName, Services) of
		true ->
		    %% Already handle this service
		    warning_msg("Ignore attempt to subscribe to "
				"known service ~p~n"
				"on behalf of client ~p",
				[SvcName, Client]),
		    Clients;
		false ->
		    Services2 = [SvcName|Services],
		    Clients2  = Clients#{Client => {MRef, Services2}},
		    info_msg("Subscribing to service ~p~n"
			     "on behalf of client ~p",
			     [SvcName, Client]),
		    diameter:subscribe(SvcName),
		    Clients2
	    end
    end.
    

handle_unregister_service(SvcName, Client, Clients) ->
    case maps:find(Client, Clients) of
	error ->
	    warning_msg("Ignore attempt to unsubscribe from service ~p ~n"
			"on behalf of unknown client ~p",
			[SvcName, Client]),
	    Clients;
	{ok, {MRef, Services}} ->
	    case lists:delete(SvcName, Services) of
		Services -> % Not subscribed - race?
		    warning_msg("Ignore attempt to unsubscribe from unknown "
				"service ~p~n"
				"on behalf of client ~p",
				[SvcName, Client]),
		    Client;
		[] ->
		    %% Last one removed - demonitor
		    info_msg("Unsubscribing from (last) service ~p~n"
			     "on behalf of client ~p",
			     [SvcName, Client]),
		    diameter:subscribe(SvcName),
		    erlang:demonitor(MRef, [flush]),
		    maps:remove(Client, Clients);
		Services2 ->
		    info_msg("Unsubscribing from service ~p~n"
			     "on behalf of client ~p",
			     [SvcName, Client]),
		    diameter:subscribe(SvcName),
		    Clients#{Client => {MRef, Services2}}
	    end
    end.


handle_down(MRef, Pid, Info, Clients) ->
    case maps:find(Pid, Clients) of
	{ok, {MRef, Services}} ->
	    warning_msg("Received unexpected DOWN from client ~p: "
			"~n   ~p", [Pid, Info]),
	    Unsubscribe =
		fun(SvcName) ->
			info_msg("Unsubscribing from service ~p~n"
				 "on behalf of (downed) client ~p",
				 [SvcName, Pid]),
			%% In this case we do not really know what
			%% happened; diameter stopped?
			(catch diameter:unsubscribe(SvcName))
		end,
	    lists:foreach(Unsubscribe, Services),
	    maps:remove(Pid, Clients);
	error -> % Race
	    Clients
    end.


handle_diameter_event(#diameter_event{service = SvcName,
				      info    = Info}) ->
    io:format("=== ~s === DIAMETER EVENT - ~p ===~n"
	      "~s~n",
	      [?FS(), SvcName, format_diameter_event("   ", Info)]).

format_diameter_event(Indent, Event)
  when (Event =:= start) orelse (Event =:= stop) ->
    ?F("~s~w", [Indent, Event]);
format_diameter_event(Indent,
		      {up, Ref, Peer, _Config, _Pkt}) ->
    ?F("~sup: "
       "~n~s   Ref:  ~p"
       "~n~s   Peer: ~p",
       [Indent,
        Indent, Ref,
        Indent, Peer]);
format_diameter_event(Indent,
		      {up, Ref, Peer, _Config}) ->
    ?F("~sup: "
       "~n~s   Ref:  ~p"
       "~n~s   Peer: ~p",
       [Indent,
        Indent, Ref,
        Indent, Peer]);
format_diameter_event(Indent,
		      {down, Ref, Peer, _Config}) ->
    ?F("~sdown: "
       "~n~s   Ref:  ~p"
       "~n~s   Peer: ~p",
       [Indent,
        Indent, Ref,
        Indent, Peer]);
format_diameter_event(Indent,
		      {reconnect, Ref, _Opts}) ->
    ?F("~sreconnect: "
       "~n~s   Ref: ~p",
       [Indent,
        Indent, Ref]);
format_diameter_event(Indent,
		      {closed, Ref, Reason, _Config}) ->
    ?F("~sclosed: "
       "~n~s   Ref:    ~p"
       "~n~s   Reason: ~p",
       [Indent,
        Indent, Ref,
        Indent, Reason]);
format_diameter_event(Indent,
		      {watchdog, Ref, PeerRef, {From, To}, _Config}) ->
    ?F("~swatchdog: ~w -> ~w"
       "~n~s   Ref:     ~p"
       "~n~s   PeerRef: ~p",
       [Indent, From, To,
        Indent, Ref,
        Indent, PeerRef]);
format_diameter_event(Indent, Event) ->
    ?F("~s~p", [Indent, Event]).

    

%%-----------------------------------------------------------------

call(Req) ->
    call(Req, infinity).

call(Req, Timeout) ->
    gen_server:call(?SERVER, Req, Timeout).

%% cast(Msg) ->
%%     gen_server:cast(?SERVER, Msg).


%% ---------------------------------------------------------------------------

info_msg(F, A) ->
    error_logger:info_msg("=== DIAMETER EVENT LOGGER ===~n" ++ F, A).

warning_msg(F, A) ->
    error_logger:warning_msg("=== DIAMETER EVENT LOGGER ===~n" ++ F, A).

error_msg(F, A) ->
    error_logger:error_msg("=== DIAMETER EVENT LOGGER ===~n" ++ F, A).

