%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2013. All Rights Reserved.
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
-module(dtls_connection).

%% Internal application API

%%====================================================================
%% Internal application API
%%====================================================================



%% %%====================================================================
%% %% State functions
%% %%====================================================================

%% -spec hello(start | #hello_request{} | #client_hello{} | #server_hello{} | term(),
%% 	    #state{}) -> gen_fsm_state_return().
%% %%--------------------------------------------------------------------
%% hello(start, #state{host = Host, port = Port, role = client,
%% 			      ssl_options = SslOpts,
%% 			      session = #session{own_certificate = Cert} = Session0,
%% 			      session_cache = Cache, session_cache_cb = CacheCb,
%% 			      connection_states = ConnectionStates0,
%% 			      renegotiation = {Renegotiation, _},
%% 			      client_cookie = Cookie} = State0) ->
%%     Hello = dtls_handshake:client_hello(Host, Port, Cookie, ConnectionStates0, SslOpts,
%% 				       Cache, CacheCb, Renegotiation, Cert),

%%     Version = Hello#client_hello.client_version,
%%     State1 = State0#state{negotiated_version = Version, %% Requested version
%% 			  session =
%% 			      Session0#session{session_id = Hello#client_hello.session_id},
%% 			  dtls_handshake_history = ssl_handshake:init_handshake_history()},

%%     State2 = send_flight(Hello, waiting, State1),

%%     {Record, State} = next_record(State2),
%%     next_state(hello, hello, Record, State);

%% hello(start, #state{role = server} = State0) ->
%%     {Record, State} = next_record(State0),
%%     next_state(hello, hello, Record, State);

%% hello(#hello_request{}, #state{role = client} = State0) ->
%%     {Record, State} = next_record(State0),
%%     next_state(hello, hello, Record, State);

%% hello(#server_hello{cipher_suite = CipherSuite,
%% 		    compression_method = Compression} = Hello,
%%       #state{session = #session{session_id = OldId},
%% 	     connection_states = ConnectionStates0,
%% 	     role = client,
%% 	     negotiated_version = ReqVersion,
%% 	     renegotiation = {Renegotiation, _},
%% 	     ssl_options = SslOptions} = State1) ->
%%     State0 = flight_done(State1),
%%     case ssl_handshake:hello(Hello, SslOptions, ConnectionStates0, Renegotiation) of
%% 	#alert{} = Alert ->
%% 	    handle_own_alert(Alert, ReqVersion, hello, State0);
%% 	{Version, NewId, ConnectionStates, NextProtocol} ->
%% 	    {KeyAlgorithm, _, _, _} =
%% 		ssl_cipher:suite_definition(CipherSuite),

%% 	    PremasterSecret = make_premaster_secret(ReqVersion, KeyAlgorithm),

%% 	    NewNextProtocol = case NextProtocol of
%% 				  undefined ->
%% 				      State0#state.next_protocol;
%% 				  _ ->
%% 				      NextProtocol
%% 			      end,

%% 	    State = State0#state{key_algorithm = KeyAlgorithm,
%% 				 hashsign_algorithm = default_hashsign(Version, KeyAlgorithm),
%% 				 negotiated_version = Version,
%% 				 connection_states = ConnectionStates,
%% 				 premaster_secret = PremasterSecret,
%% 				 expecting_next_protocol_negotiation = NextProtocol =/= undefined,
%% 				 next_protocol = NewNextProtocol},

%% 	    case ssl_session:is_new(OldId, NewId) of
%% 		true ->
%% 		    handle_new_session(NewId, CipherSuite, Compression,
%% 				       State#state{connection_states = ConnectionStates});
%% 		false ->
%% 		    handle_resumed_session(NewId, State#state{connection_states = ConnectionStates})
%% 	    end
%%     end;

%% hello(#hello_verify_request{cookie = Cookie},
%%       #state{host = Host, port = Port,
%% 	     session = #session{own_certificate = Cert},
%% 	     session_cache = Cache, session_cache_cb = CacheCb,
%% 	     ssl_options = SslOpts,
%% 	     connection_states = ConnectionStates0,
%% 	     renegotiation = {Renegotiation, _}} = State0) ->
%%     Hello = ssl_handshake:client_hello(Host, Port, Cookie, ConnectionStates0, SslOpts,
%% 				       Cache, CacheCb, Renegotiation, Cert),
%%     State1 = State0#state{
%% 	       tls_handshake_history = ssl_handshake:init_handshake_history(),
%% 	       client_cookie = Cookie},
%%     State2 = send_flight(Hello, waiting, State1),

%%     {Record, State} = next_record(State2),
%%     next_state(hello, hello, Record, State);


%% %%--------------------------------------------------------------------
%% -spec abbreviated(#hello_request{} | #finished{} | term(),
%% 		  #state{}) -> gen_fsm_state_return().
%% %%--------------------------------------------------------------------

%% abbreviated(timeout, State) ->
%%     { next_state, abbreviated, State, hibernate };

%% abbreviated(Msg, State) ->
%%     handle_unexpected_message(Msg, abbreviated, State).

%% %%--------------------------------------------------------------------
%% -spec certify(#hello_request{} | #certificate{} |  #server_key_exchange{} |
%% 	      #certificate_request{} | #server_hello_done{} | #client_key_exchange{} | term(),
%% 	      #state{}) -> gen_fsm_state_return().
%% %%--------------------------------------------------------------------


%% certify(timeout, State) ->
%%     { next_state, certify, State, hibernate };

%% certify(Msg, State) ->
%%     handle_unexpected_message(Msg, certify, State).


%% %%--------------------------------------------------------------------
%% -spec cipher(#hello_request{} | #certificate_verify{} | #finished{} | term(),
%% 	     #state{}) -> gen_fsm_state_return().
%% %%--------------------------------------------------------------------

%% cipher(timeout, State) ->
%%     { next_state, cipher, State, hibernate };

%% cipher(Msg, State) ->
%%     handle_unexpected_message(Msg, cipher, State).

%% %%--------------------------------------------------------------------
%% -spec connection(#hello_request{} | #client_hello{} | term(),
%% 		 #state{}) -> gen_fsm_state_return().
%% %%--------------------------------------------------------------------

%% connection(timeout, State) ->
%%     {next_state, connection, State, hibernate};

%% connection(Msg, State) ->
%%     handle_unexpected_message(Msg, connection, State).

%% %%--------------------------------------------------------------------
%% %%% Internal functions
%% %%--------------------------------------------------------------------
%% handle_unexpected_message(Msg, Info, #state{negotiated_version = Version} = State) ->
%%     Alert =  ?ALERT_REC(?FATAL,?UNEXPECTED_MESSAGE),
%%     handle_own_alert(Alert, Version, {Info, Msg}, State).

%% send_flight(HandshakeRec, FlightState, State) ->
%%     send_flight(FlightState, buffer_flight(HandshakeRec, State)).

%% send_flight(FlightState, State = #state{negotiated_version = Version,
%% 					flight_buffer = Buffer}) ->

%%     State1 = do_send_flight(queue:to_list(Buffer), [], State),
%%     finish_send_flight(Version, FlightState, State1).

%% resend_flight(State = #state{negotiated_version = Version,
%% 			     flight_state = FlightState,
%% 			     flight_buffer = Buffer})
%%   when FlightState == finished; FlightState == waiting ->
%%     State1 = do_send_flight(queue:to_list(Buffer), [], State),
%%     finish_send_flight(Version, FlightState, State1);

%% resend_flight(State) ->
%%     State.

%% flight_done(State) ->
%%     cancel_dtls_retransmit_timer(State#state{flight_state = done,
%% 					     flight_buffer = undefined}).

%% do_send_flight([], BinMsgs, State = #state{transport_cb = Transport, socket = Socket}) ->
%%     Transport:send(Socket, lists:reverse(BinMsgs)),
%%     State;
%% do_send_flight([{Epoch, MsgSeq, HandshakeRec}|T], BinMsgs0,
%% 	       State = #state{negotiated_version = Version,
%% 			      connection_states = ConnectionStates0}) ->
%%     CS0 = ssl_record:connection_state_by_epoch(ConnectionStates0, Epoch, write),
%%     {BinMsgs, CS1} = encode_handshake_rec(HandshakeRec, Version, MsgSeq, BinMsgs0, CS0),
%%     ConnectionStates1 = ssl_record:set_connection_state_by_epoch(ConnectionStates0, CS1, write),
%%     do_send_flight(T, BinMsgs, State#state{connection_states = ConnectionStates1}).

%% cancel_dtls_retransmit_timer(State = #state{dtls_retransmit_timer = TimerRef}) ->
%%     cancel_timer(TimerRef),
%%     State#state{dtls_retransmit_timer = undefined}.

%% rearm_dtls_retransmit_timer(State = #state{dtls_retransmit_timer = undefined}) ->
%%     TimerRef = erlang:start_timer(1000, self(), dtls_retransmit),
%%     State#state{dtls_retransmit_timer = TimerRef};
%% rearm_dtls_retransmit_timer(State) ->
%%     State.

%% finish_send_flight({254, _}, waiting, State) ->
%%     TimerRef = erlang:start_timer(1000, self(), dtls_retransmit),
%%     State#state{
%%       dtls_retransmit_timer = TimerRef,
%%       last_retransmit = timestamp(),
%%       flight_state = waiting};

%% finish_send_flight(_, FlightState, State) ->
%%     State#state{flight_state = FlightState}.

%% timestamp() ->
%%     {Mega, Sec, Micro} = erlang:now(),
%%     Mega * 1000000 * 1000 + Sec * 1000 + (Micro div 1000).

%% encode_handshake_rec(HandshakeRec, Version, MsgSeq, BinMsgs0, CS0) ->
%%     {_, Fragments} = ssl_handshake:encode_handshake(HandshakeRec, Version, MsgSeq, 1400),
%%     lists:foldl(fun(F, {Bin, C0}) ->
%% 			{B, C1} = ssl_record:encode_handshake(F, Version, C0),
%% 		{[B|Bin], C1} end, {BinMsgs0, CS0}, Fragments).
