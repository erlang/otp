%%--------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
%%-----------------------------------------------------------------
%% File: orber_iiop_inproxy.erl
%% 
%% Description:
%%    This file contains the IIOP "proxy" for incomming connections
%%
%%-----------------------------------------------------------------
-module(orber_iiop_inproxy).

-behaviour(gen_server).

-include_lib("orber/src/orber_iiop.hrl").
-include_lib("orber/include/corba.hrl").

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([start/0, start/1]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 code_change/3, terminate/2, post_accept/3, stop/1]).

%%-----------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------
-define(DEBUG_LEVEL, 7).

-record(state, {stype, socket, db, timeout, max_fragments, 
		max_requests, request_counter = 1, giop_env, peer}).

%%-----------------------------------------------------------------
%% External interface functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: start/0
%%-----------------------------------------------------------------
start() ->
    ignore.

%%-----------------------------------------------------------------
%% Func: start/1
%%-----------------------------------------------------------------
start(Opts) ->
    gen_server:start_link(orber_iiop_inproxy, Opts, []).

post_accept(Pid, ssl, Socket) ->
    (catch gen_server:cast(Pid, {post_accept, ssl, Socket})),
    ok;
post_accept(_, _, _) ->
    ok.

%%-----------------------------------------------------------------
%% Internal interface functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: stop/1
%%-----------------------------------------------------------------
stop(Pid) ->
    gen_server:cast(Pid, stop).

%%-----------------------------------------------------------------
%% Server functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: init/1
%%-----------------------------------------------------------------
init({connect, Type, Socket, Ref, Options}) ->
    process_flag(trap_exit, true),
    Flags = orber_tb:keysearch(flags, Options, orber_env:get_flags()),
    {Address, Port} = PeerData = orber_socket:peerdata(Type, Socket),
    {LAddress, LPort} = LocalData = orber_socket:sockdata(Type, Socket),
    case {?ORB_FLAG_TEST(Flags, ?ORB_ENV_LOCAL_INTERFACE), LPort} of
	{true, 0} ->
	    orber_tb:info("Unable to lookup the local address and port number.~n"
			  "Closing the incoming connection.", []),
	    ignore;
	_ ->
	    orber_iiop_net:add_connection(Socket, Type, PeerData, LocalData, Ref),
	    Interceptors = 
		case orber_tb:keysearch(interceptors, Options,
					orber_env:get_interceptors()) of
		    {native, PIs} ->
			{native, orber_pi:new_in_connection(PIs, Address, Port, 
							    LAddress, LPort), PIs};
		    Other ->
			Other
		end,
	    Env = 
		case ?ORB_FLAG_TEST(Flags, ?ORB_ENV_LOCAL_INTERFACE) of
		    true when Type == ssl ->
			#giop_env{interceptors = Interceptors, 
				  flags = Flags, host = [LAddress], 
				  iiop_port = 
				  orber_tb:keysearch(iiop_port, Options, 
						     orber_env:iiop_port()),
				  iiop_ssl_port = LPort,
				  domain = orber:domain(),
				  partial_security = orber:partial_security()};
		    true ->
			#giop_env{interceptors = Interceptors, 
				  flags = Flags, host = [LAddress], 
				  iiop_port = LPort,
				  iiop_ssl_port = 
				  orber_tb:keysearch(iiop_ssl_port, Options, 
						     orber_env:iiop_ssl_port()),
				  domain = orber:domain(),
				  partial_security = orber:partial_security()};
		    false ->
			case ?ORB_FLAG_TEST(Flags, ?ORB_ENV_ENABLE_NAT) of
			    false ->
				#giop_env{interceptors = Interceptors, 
					  flags = Flags, host = orber:host(), 
					  iiop_port = orber:iiop_port(),
					  iiop_ssl_port = orber:iiop_ssl_port(),
					  domain = orber:domain(),
					  partial_security = orber:partial_security()};
			    true ->
				#giop_env{interceptors = Interceptors, 
					  flags = Flags, 
					  host = 
					  orber_tb:keysearch(nat_ip_address, Options,
							     orber_env:nat_host()), 
					  iiop_port = 
					  orber_tb:keysearch(nat_iiop_port, Options,
							     orber_env:nat_iiop_port()),
					  iiop_ssl_port = 
					  orber_tb:keysearch(nat_iiop_ssl_port, Options,
							     orber_env:nat_iiop_ssl_port()),
					  domain = orber:domain(),
					  partial_security = orber:partial_security()}
			end
		end,
	    Timeout = orber_tb:keysearch(iiop_in_connection_timeout, Options,
					 orber_env:iiop_in_connection_timeout()),
	    MaxFrags = orber_tb:keysearch(iiop_max_fragments, Options,
					  orber_env:iiop_max_fragments()),
	    MaxRequests = orber_tb:keysearch(iiop_max_in_requests, Options,
					     orber_env:iiop_max_in_requests()),
	    {ok, #state{stype = Type, 
			socket = Socket, 
			db =  ets:new(orber_incoming_requests, [set]), 
			timeout = Timeout,
			max_fragments = MaxFrags,
			max_requests = MaxRequests,
			giop_env = Env, peer = PeerData}, Timeout}
    end.


%%-----------------------------------------------------------------
%% Func: terminate/2
%%-----------------------------------------------------------------
%% We may want to kill all proxies before terminating, but the best
%% option should be to let the requests complete (especially for one-way
%% functions it's a better alternative.
terminate(_Reason, #state{db = IncRequests, giop_env = Env}) ->
    ets:delete(IncRequests),
    case Env#giop_env.interceptors of 
	false ->
	    ok;
	{native, Ref, PIs} ->
	    orber_pi:closed_in_connection(PIs, Ref);
	{_Type, _PIs} ->
	    ok
    end.

%%-----------------------------------------------------------------
%% Func: handle_call/3
%%-----------------------------------------------------------------
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_, _, State) ->
    {noreply, State, State#state.timeout}.

%%-----------------------------------------------------------------
%% Func: handle_cast/2
%%-----------------------------------------------------------------
handle_cast({post_accept, Type, Socket}, State) ->
    Timeout = orber_env:iiop_ssl_accept_timeout(),
    case catch orber_socket:post_accept(Type, Socket, Timeout) of
	ok ->
	    {noreply, State};
	_Failed ->
	    orber_socket:close(Type, Socket),
	    {stop, normal, State}
    end;
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_, State) ->
    {noreply, State, State#state.timeout}.

%%-----------------------------------------------------------------
%% Func: handle_info/2
%%-----------------------------------------------------------------
%% Normal invocation
handle_info({tcp, Socket, Bytes}, State) ->
    handle_msg(normal, Socket, Bytes, State);
handle_info({ssl, Socket, Bytes}, State) ->
    handle_msg(ssl, Socket, Bytes, State);
%% Errors, closed connection
handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};
handle_info({tcp_error, _Socket, _Reason}, State) ->
    {stop, normal, State};
handle_info({ssl_closed, _Socket}, State) ->
    {stop, normal, State};
handle_info({ssl_error, _Socket, _Reason}, State) ->
    {stop, normal, State};
%% Servant termination.
handle_info({'EXIT', Pid, normal}, State) ->
    ets:delete(State#state.db, Pid),
    {noreply, decrease_counter(State), State#state.timeout};
handle_info({message_error, _Pid, ReqId}, State) ->
    ets:delete(State#state.db, ReqId),
    {noreply, State, State#state.timeout};
handle_info(timeout, State) ->
    case ets:info(State#state.db, size) of
	0 ->
	    %% No pending requests, close the connection.
	    {stop, normal, State};
	_Amount ->
	    %% Still pending request, cannot close the connection.
	    {noreply, State, State#state.timeout}
    end;
handle_info({reconfigure, Options}, State) ->
    {noreply, update_state(State, Options), State#state.timeout};
handle_info(_X,State) ->
    {noreply, State, State#state.timeout}.

handle_msg(Type, Socket, Bytes, #state{stype = Type, socket = Socket, 
				       giop_env = Env} = State) ->
    case catch cdr_decode:dec_giop_message_header(Bytes) of
	%% Only when using IIOP-1.2 may the client send this message. 
	%% Introduced in CORBA-2.6
	#giop_message{message_type = ?GIOP_MSG_CLOSE_CONNECTION, 
		      giop_version = {1,2}} ->
	    {stop, normal, State};
	#giop_message{message_type = ?GIOP_MSG_CLOSE_CONNECTION} ->
	    {noreply, State, State#state.timeout};
	#giop_message{message_type = ?GIOP_MSG_CANCEL_REQUEST} = GIOPHdr ->
	    ReqId = cdr_decode:peek_request_id(GIOPHdr#giop_message.byte_order,
					       GIOPHdr#giop_message.message),
	    case ets:lookup(State#state.db, ReqId) of
		[{RId, PPid}] ->
		    ets:delete(State#state.db, RId),
		    PPid ! {self(), cancel_request_header};
		[] ->
		    send_msg_error(Type, Socket, Bytes, 
				   Env#giop_env{version = 
						GIOPHdr#giop_message.giop_version},
				   "No such request id")
	    end,
	    {noreply, State, State#state.timeout};
	%% A fragment; we must have received a Request or LocateRequest
	%% with fragment-flag set to true.
	%% We need to decode the header to get the request-id.
	#giop_message{message_type = ?GIOP_MSG_FRAGMENT,
		      giop_version = {1,2}} = GIOPHdr ->
	    ReqId = cdr_decode:peek_request_id(GIOPHdr#giop_message.byte_order,
					       GIOPHdr#giop_message.message),
	    case ets:lookup(State#state.db, ReqId) of
		[{_RId, PPid}] when GIOPHdr#giop_message.fragments == true ->
		    PPid ! {self(), GIOPHdr};
		[{RId, PPid}] ->
		    ets:delete(State#state.db, RId),
		    PPid ! {self(), GIOPHdr};
		[] ->
		    send_msg_error(Type, Socket, Bytes, 
				   Env#giop_env{version = 
						GIOPHdr#giop_message.giop_version},
				   "No such fragment id")
	    end,
	    {noreply, State, State#state.timeout};
	%% Must be a Request or LocateRequest which have been fragmented.
	%% We need to decode the header to get the request-id.
	#giop_message{fragments = true,
		      giop_version = {1,2}} = GIOPHdr ->
	    ReqId = cdr_decode:peek_request_id(GIOPHdr#giop_message.byte_order,
					       GIOPHdr#giop_message.message),
	    Pid = 
		orber_iiop_inrequest:
		start_fragment_collector(GIOPHdr, Bytes, 
					 Type, Socket, 
					 ReqId, self(),
					 State#state.max_fragments,
					 Env#giop_env{version = {1,2},
						      request_id = ReqId}),
	    ets:insert(State#state.db, {Pid, ReqId}),
	    ets:insert(State#state.db, {ReqId, Pid}),
	    {noreply, increase_counter(State), State#state.timeout};
	GIOPHdr when is_record(GIOPHdr, giop_message) ->
	    Pid = orber_iiop_inrequest:start(GIOPHdr, Bytes, Type, Socket, 
					     Env#giop_env{version = 
							  GIOPHdr#giop_message.giop_version}),
	    ets:insert(State#state.db, {Pid, undefined}),
	    {noreply, increase_counter(State), State#state.timeout};
	{'EXIT', message_error} ->
	    send_msg_error(Type, Socket, Bytes, 
			   Env#giop_env{version = orber_env:giop_version()},
			   "Unable to decode the GIOP-header"),
	    {noreply, State, State#state.timeout}
    end;
handle_msg(Type, _, Bytes, State) ->
    orber:dbg("[~p] orber_iiop_inproxy:handle_msg(~p);~n"
	      "Received a message from a socket of a different type.~n"
	      "Should be ~p but was ~p.", 
	      [?LINE, Bytes, State#state.stype, Type], ?DEBUG_LEVEL),
    {noreply, State, State#state.timeout}.

send_msg_error(Type, Socket, Data, Env, Msg) ->
    orber:dbg("[~p] orber_iiop_inproxy:handle_msg(~p); ~p.", 
	      [?LINE, Data, Msg], ?DEBUG_LEVEL),
    Reply = cdr_encode:enc_message_error(Env),
    orber_socket:write(Type, Socket, Reply).

increase_counter(#state{max_requests = infinity} = State) ->
    State;
increase_counter(#state{max_requests = Max, 
			request_counter = Counter} = State) when Max > Counter ->
    orber_socket:setopts(State#state.stype, State#state.socket, [{active, once}]),
    State#state{request_counter = Counter + 1};
increase_counter(State) ->
    State#state{request_counter = State#state.request_counter + 1}.

decrease_counter(#state{max_requests = infinity} = State) ->
    State;
decrease_counter(#state{max_requests = Max, 
			request_counter = Counter} = State) when Max =< Counter ->
    orber_socket:setopts(State#state.stype, State#state.socket, [{active, once}]),
    State#state{request_counter = Counter - 1};
decrease_counter(State) ->
    State#state{request_counter = State#state.request_counter - 1}.

update_state(#state{giop_env = Env} = State, 
	     [{interceptors, false}|Options]) ->
    update_state(State#state{giop_env = 
			     Env#giop_env{interceptors = false}}, Options);
update_state(#state{giop_env = #giop_env{interceptors = false, host = [SH], 
					 iiop_port = SP} = Env, 
		    peer = {PH, PP}, stype = normal} = State, 
	     [{interceptors, {native, LPIs}}|Options]) ->
    %% No Interceptor(s). Add the same Ref used by the built in interceptors.
    update_state(State#state{giop_env = 
			     Env#giop_env{interceptors = 
					  {native, {PH, PP, SH, SP}, LPIs}}},
		 Options);
update_state(#state{giop_env = #giop_env{interceptors = false, host = [SH], 
					 iiop_ssl_port = SP} = Env, 
		    peer = {PH, PP}, stype = ssl} = State, 
	     [{interceptors, {native, LPIs}}|Options]) ->
    %% No Interceptor(s). Add the same Ref used by the built in interceptors.
    update_state(State#state{giop_env = 
			     Env#giop_env{interceptors = 
					  {native, {PH, PP, SH, SP}, LPIs}}},
		 Options);
update_state(#state{giop_env = #giop_env{interceptors = {native, Ref, _}} = Env} = 
	     State, 
	     [{interceptors, {native, LPIs}}|Options]) ->
    %% Interceptor(s) already in use. We must use the same Ref as before.
    update_state(State#state{giop_env = 
			     Env#giop_env{interceptors = {native, Ref, LPIs}}},
		 Options);
update_state(State, [H|T]) ->
    orber:dbg("[~p] orber_iiop_inproxy:update_state(~p, ~p)~n"
	      "Couldn't change the state.", 
	      [?LINE, H, State], ?DEBUG_LEVEL),
    update_state(State, T);
update_state(State, []) ->
    State.

%%-----------------------------------------------------------------
%% Func: code_change/3
%%-----------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

