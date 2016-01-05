%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2015. All Rights Reserved.
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
-module(dtls_connection).

%% Internal application API

-behaviour(gen_statem).

-include("dtls_connection.hrl").
-include("dtls_handshake.hrl").
-include("ssl_alert.hrl").
-include("dtls_record.hrl").
-include("ssl_cipher.hrl").
-include("ssl_api.hrl").
-include("ssl_internal.hrl").
-include("ssl_srp.hrl").
-include_lib("public_key/include/public_key.hrl"). 

%% Internal application API

%% Setup
-export([start_fsm/8, start_link/7, init/1]).

%% State transition handling	 
-export([next_record/1, next_event/3]).

%% Handshake handling
-export([renegotiate/2, send_handshake/2, queue_handshake/2, queue_change_cipher/2]).

%% Alert and close handling
-export([send_alert/2, handle_own_alert/4, handle_close_alert/3,
	 handle_normal_shutdown/3,
	 close/5, alert_user/6, alert_user/9
	]).

%% Data handling
-export([write_application_data/3, read_application_data/2,
	 passive_receive/2,  next_record_if_active/1, handle_common_event/4,
	 handle_packet/3]).

%% gen_statem state functions
-export([init/3, error/3, downgrade/3, %% Initiation and take down states
	 hello/3, certify/3, cipher/3, abbreviated/3, %% Handshake states 
	 connection/3]). 
%% gen_statem callbacks
-export([terminate/3, code_change/4, format_status/2]).

-define(GEN_STATEM_CB_MODE, state_functions).

%%====================================================================
%% Internal application API
%%====================================================================	     
start_fsm(Role, Host, Port, Socket, {#ssl_options{erl_dist = false},_, Tracker} = Opts,
	  User, {CbModule, _,_, _} = CbInfo, 
	  Timeout) -> 
    try 
	{ok, Pid} = dtls_connection_sup:start_child([Role, Host, Port, Socket, 
						     Opts, User, CbInfo]), 
	{ok, SslSocket} = ssl_connection:socket_control(?MODULE, Socket, Pid, CbModule, Tracker),
	ok = ssl_connection:handshake(SslSocket, Timeout),
	{ok, SslSocket} 
    catch
	error:{badmatch, {error, _} = Error} ->
	    Error
    end;

start_fsm(Role, Host, Port, Socket, {#ssl_options{erl_dist = true},_, Tracker} = Opts,
	  User, {CbModule, _,_, _} = CbInfo, 
	  Timeout) -> 
    try 
	{ok, Pid} = dtls_connection_sup:start_child_dist([Role, Host, Port, Socket, 
							  Opts, User, CbInfo]), 
	{ok, SslSocket} = ssl_connection:socket_control(?MODULE, Socket, Pid, CbModule, Tracker),
	ok = ssl_connection:handshake(SslSocket, Timeout),
	{ok, SslSocket} 
    catch
	error:{badmatch, {error, _} = Error} ->
	    Error
    end.

send_handshake(Handshake, State) ->
    send_handshake_flight(queue_handshake(Handshake, State)).

queue_flight_buffer(Msg, #state{negotiated_version = Version,
				connection_states = #connection_states{
				  current_write =
				      #connection_state{epoch = Epoch}},
				flight_buffer = Flight} = State) ->
    State#state{flight_buffer = Flight ++ [{Version, Epoch, Msg}]}.

queue_handshake(Handshake, #state{negotiated_version = Version,
				  tls_handshake_history = Hist0,
				  connection_states = ConnectionStates0} = State0) ->
    {Frag, ConnectionStates, Hist} =
	encode_handshake(Handshake, Version, ConnectionStates0, Hist0),
    queue_flight_buffer(Frag, State0#state{connection_states = ConnectionStates,
					   tls_handshake_history = Hist}).

send_handshake_flight(#state{socket = Socket,
			     transport_cb = Transport,
			     flight_buffer = Flight,
			     connection_states = ConnectionStates0} = State0) ->

    {Encoded, ConnectionStates} =
	encode_handshake_flight(Flight, ConnectionStates0),

    Transport:send(Socket, Encoded),
    State0#state{flight_buffer = [], connection_states = ConnectionStates}.

queue_change_cipher(Msg, State) ->
    queue_flight_buffer(Msg, State).

send_alert(Alert, #state{negotiated_version = Version,
			 socket = Socket,
			 transport_cb = Transport,
			 connection_states = ConnectionStates0} = State0) ->
    {BinMsg, ConnectionStates} =
	ssl_alert:encode(Alert, Version, ConnectionStates0),
    Transport:send(Socket, BinMsg),
    State0#state{connection_states = ConnectionStates}.

%%====================================================================
%% tls_connection_sup API
%%====================================================================

%%--------------------------------------------------------------------
-spec start_link(atom(), host(), inet:port_number(), port(), list(), pid(), tuple()) ->
    {ok, pid()} | ignore |  {error, reason()}.
%%
%% Description: Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this function
%% does not return until Module:init/1 has returned.  
%%--------------------------------------------------------------------
start_link(Role, Host, Port, Socket, Options, User, CbInfo) ->
    {ok, proc_lib:spawn_link(?MODULE, init, [[Role, Host, Port, Socket, Options, User, CbInfo]])}.

init([Role, Host, Port, Socket, Options,  User, CbInfo]) ->
    process_flag(trap_exit, true),
    State0 =  initial_state(Role, Host, Port, Socket, Options, User, CbInfo),
    try
	State = ssl_connection:ssl_config(State0#state.ssl_options, Role, State0),
	gen_statem:enter_loop(?MODULE, [], ?GEN_STATEM_CB_MODE, init, State)
    catch
	throw:Error ->
	    gen_statem:enter_loop(?MODULE, [], ?GEN_STATEM_CB_MODE, error, {Error,State0})
    end.

%%--------------------------------------------------------------------
%% State functionsconnection/2
%%--------------------------------------------------------------------

init({call, From}, {start, Timeout}, 
     #state{host = Host, port = Port, role = client,
	    ssl_options = SslOpts,
	    session = #session{own_certificate = Cert} = Session0,
	    connection_states = ConnectionStates0,
	    renegotiation = {Renegotiation, _},
	    session_cache = Cache,
	    session_cache_cb = CacheCb
	   } = State0) ->
    Timer = ssl_connection:start_or_recv_cancel_timer(Timeout, From),
    Hello = dtls_handshake:client_hello(Host, Port, ConnectionStates0, SslOpts,
				       Cache, CacheCb, Renegotiation, Cert),

    Version = Hello#client_hello.client_version,
    _HelloVersion = dtls_record:lowest_protocol_version(SslOpts#ssl_options.versions),
    %%
    %% TODO: HelloVersion handling is not correct
    %%
    State1 = State0#state{
	       negotiated_version = Version, %% Requested version
	       session =
		   Session0#session{session_id = Hello#client_hello.session_id},
	       tls_handshake_history = ssl_handshake:init_handshake_history(),
	       start_or_recv_from = From,
	       timer = Timer},
    State2 = send_handshake(Hello, State1),
    {Record, State} = next_record(State2),
    next_event(hello, Record, State);
init(Type, Event, State) ->
    ssl_connection:init(Type, Event, State, ?MODULE).
 
error({call, From}, {start, _Timeout}, {Error, State}) ->
    {stop_and_reply, normal, {reply, From, {error, Error}}, State};
error({call, From}, Msg, State) ->
    handle_call(Msg, From, error, State);
error(_, _, _) ->
     {keep_state_and_data, [postpone]}.

%%--------------------------------------------------------------------
-spec hello(gen_statem:event_type(),
	    #hello_request{} | #client_hello{} | #server_hello{} | term(),
	    #state{}) ->
		   gen_statem:state_function_result().
%%--------------------------------------------------------------------
hello(internal, #client_hello{client_version = ClientVersion,
			      extensions = #hello_extensions{ec_point_formats = EcPointFormats,
							     elliptic_curves = EllipticCurves}} = Hello,
      State = #state{connection_states = ConnectionStates0,
		     port = Port, session = #session{own_certificate = Cert} = Session0,
		     renegotiation = {Renegotiation, _},
		     session_cache = Cache,
		     session_cache_cb = CacheCb,
		     negotiated_protocol = CurrentProtocol,
		     key_algorithm = KeyExAlg,
		     ssl_options = SslOpts}) ->

    case dtls_handshake:hello(Hello, SslOpts, {Port, Session0, Cache, CacheCb,
					      ConnectionStates0, Cert, KeyExAlg}, Renegotiation) of
	#alert{} = Alert ->
	    handle_own_alert(Alert, ClientVersion, hello, State);
	{Version, {Type, Session},
	 ConnectionStates, Protocol0, ServerHelloExt, HashSign} ->
	    Protocol = case Protocol0 of
			   undefined -> CurrentProtocol;
			   _ -> Protocol0
		       end,

	    ssl_connection:hello(internal, {common_client_hello, Type, ServerHelloExt},
				 State#state{connection_states	= ConnectionStates,
					     negotiated_version = Version,
					     hashsign_algorithm = HashSign,
					     session = Session,
					     client_ecc = {EllipticCurves, EcPointFormats},
					     negotiated_protocol = Protocol}, ?MODULE)
    end;

hello(internal, #hello_verify_request{cookie = Cookie},
      #state{host = Host, port = Port, role = client,
	     ssl_options = SslOpts,
	     session = #session{own_certificate = Cert},
	     connection_states = ConnectionStates0,
	     renegotiation = {Renegotiation, _},
	     session_cache = Cache,
	     session_cache_cb = CacheCb
	    } = State0) ->
    Hello = dtls_handshake:client_hello(Host, Port, Cookie, ConnectionStates0, SslOpts,
					Cache, CacheCb, Renegotiation, Cert),
    State1 = State0#state{
	       tls_handshake_history = ssl_handshake:init_handshake_history()
	       %%	       client_cookie = Cookie
	      },
    State2 = send_handshake(Hello, State1),
    {Record, State} = next_record(State2),
    next_event(hello, Record, State);

hello(internal, #server_hello{} = Hello,
      #state{connection_states = ConnectionStates0,
	     negotiated_version = ReqVersion,
	     role = client,
	     renegotiation = {Renegotiation, _},
	     ssl_options = SslOptions} = State) ->
    case dtls_handshake:hello(Hello, SslOptions, ConnectionStates0, Renegotiation) of
	#alert{} = Alert ->
	    handle_own_alert(Alert, ReqVersion, hello, State);
	{Version, NewId, ConnectionStates, ProtoExt, Protocol} ->
	    ssl_connection:handle_session(Hello, 
					  Version, NewId, ConnectionStates, ProtoExt, Protocol, State)
    end;
hello(info, Event, State) ->
    handle_info(Event, hello, State);

hello(Type, Event, State) ->
    ssl_connection:hello(Type, Event, State, ?MODULE).

abbreviated(info, Event, State) ->
    handle_info(Event, abbreviated, State);
abbreviated(Type, Event, State) ->
    ssl_connection:abbreviated(Type, Event, State, ?MODULE).

certify(info, Event, State) ->
    handle_info(Event, certify, State);
certify(Type, Event, State) ->
    ssl_connection:certify(Type, Event, State, ?MODULE).

cipher(info, Event, State) ->
    handle_info(Event, cipher, State);
cipher(Type, Event, State) ->
     ssl_connection:cipher(Type, Event, State, ?MODULE).

connection(info, Event, State) ->
    handle_info(Event, connection, State);
connection(internal, #hello_request{}, #state{host = Host, port = Port,
				    session = #session{own_certificate = Cert} = Session0,
				    session_cache = Cache, session_cache_cb = CacheCb,
				    ssl_options = SslOpts,
				    connection_states = ConnectionStates0,
				    renegotiation = {Renegotiation, _}} = State0) ->
    Hello = dtls_handshake:client_hello(Host, Port, ConnectionStates0, SslOpts,
					Cache, CacheCb, Renegotiation, Cert),
    %% TODO DTLS version State1 = send_handshake(Hello, State0),
    State1 = State0,
    {Record, State} =
	next_record(
	  State1#state{session = Session0#session{session_id
						  = Hello#client_hello.session_id}}),
    next_event(hello, Record, State);

connection(internal, #client_hello{} = Hello, #state{role = server, allow_renegotiate = true} = State) ->
    %% Mitigate Computational DoS attack
    %% http://www.educatedguesswork.org/2011/10/ssltls_and_computational_dos.html
    %% http://www.thc.org/thc-ssl-dos/ Rather than disabling client
    %% initiated renegotiation we will disallow many client initiated
    %% renegotiations immediately after each other.
    erlang:send_after(?WAIT_TO_ALLOW_RENEGOTIATION, self(), allow_renegotiate),
    {next_state, hello, State#state{allow_renegotiate = false}, [{next_event, internal, Hello}]};


connection(internal, #client_hello{}, #state{role = server, allow_renegotiate = false} = State0) ->
    Alert = ?ALERT_REC(?WARNING, ?NO_RENEGOTIATION),
    State1 = send_alert(Alert, State0),
    {Record, State} = ssl_connection:prepare_connection(State1, ?MODULE),
    next_event(connection, Record, State);
  
connection(Type, Event, State) ->
     ssl_connection:connection(Type, Event, State, ?MODULE).

downgrade(Type, Event, State) ->
     ssl_connection:downgrade(Type, Event, State, ?MODULE).


%%--------------------------------------------------------------------
%% Event handling functions called by state functions to handle
%% common or unexpected events for the state.
%%--------------------------------------------------------------------
handle_call(Event, From, StateName, State) ->
    ssl_connection:handle_call(Event, From, StateName, State, ?MODULE).

%% raw data from socket, unpack records
handle_info({Protocol, _, Data}, StateName,
            #state{data_tag = Protocol} = State0) ->
     case next_tls_record(Data, State0) of
	{Record, State} ->
	    next_event(StateName, Record, State);
	#alert{} = Alert ->
	    handle_normal_shutdown(Alert, StateName, State0), 
	    {stop, {shutdown, own_alert}}
     end;
handle_info({CloseTag, Socket}, StateName,
	    #state{socket = Socket, close_tag = CloseTag,
		   negotiated_version = Version} = State) ->
    %% Note that as of DTLS 1.2 (TLS 1.1),
    %% failure to properly close a connection no longer requires that a
    %% session not be resumed.	This is a change from DTLS 1.0 to conform
    %% with widespread implementation practice.
    case Version of
	{254, N} when N =< 253 ->
	    ok;
	_ ->
	    %% As invalidate_sessions here causes performance issues,
	    %% we will conform to the widespread implementation
	    %% practice and go aginst the spec
	    %%invalidate_session(Role, Host, Port, Session)
	    ok
    end,
    handle_normal_shutdown(?ALERT_REC(?FATAL, ?CLOSE_NOTIFY), StateName, State),
    {stop, {shutdown, transport_closed}};
handle_info(Msg, StateName, State) ->
    ssl_connection:handle_info(Msg, StateName, State).

handle_common_event(internal, #alert{} = Alert, StateName,
		    #state{negotiated_version = Version} = State) ->
    handle_own_alert(Alert, Version, StateName, State);

%%% TLS record protocol level handshake messages
handle_common_event(internal, Record = #ssl_tls{type = ?HANDSHAKE},
		    StateName, #state{protocol_buffers =
					  #protocol_buffers{dtls_packets = Packets0,
							    dtls_fragment_state = HsState0} = Buffers,
				      negotiated_version = Version} = State0) ->
%%
%% TODO: It might be clearer if the Handle function was elevate to gen_statem event function.
%%       The get_dtls_handshake chould simply return a list of Events (which might be empty)
%%       and handle_common_event would match on the request type.
%%
    Handle =
	fun({#hello_request{} = Packet, _}, {connection, HState}) ->
		%% This message should not be included in handshake
		%% message hashes. Starts new handshake (renegotiation)
		Hs0 = ssl_handshake:init_handshake_history(),
		{HState#state{tls_handshake_history = Hs0,
			      renegotiation = {true, peer}},
		 {next_event, internal, Packet}};
	   ({#hello_request{}, _}, {next_state, _SName, HState}) ->
		%% This message should not be included in handshake
		%% message hashes. Already in negotiation so it will be ignored!
		{HState, []};
	   ({#client_hello{} = Packet, Raw}, {connection, HState0}) ->
		HState = handle_sni_extension(Packet, HState0),
		Hs0 = ssl_handshake:init_handshake_history(),
		Hs1 = ssl_handshake:update_handshake_history(Hs0, Raw, false),
		{HState#state{tls_handshake_history = Hs1,
			      renegotiation = {true, peer}},
		 {next_event, internal, Packet}};

	   ({Packet, Raw}, {_SName, HState0 = #state{tls_handshake_history=Hs0}}) ->
		HState = handle_sni_extension(Packet, HState0),
		Hs1 = ssl_handshake:update_handshake_history(Hs0, Raw, false),
		{HState#state{tls_handshake_history=Hs1}, {next_event, internal, Packet}}
	end,
    try
	{Packets1, HsState} = dtls_handshake:get_dtls_handshake(Record, HsState0),
	Packets = Packets0 ++ Packets1,
	State1 = State0#state{protocol_buffers =
				  Buffers#protocol_buffers{dtls_packets = Packets,
							   dtls_fragment_state = HsState}},
	case Packets of
	    [] ->
		{NextRecord, State} = next_record(State1),
		next_event(StateName, NextRecord, State);
	    _ ->
		{State, Events} = dtls_handshake_events(Handle, StateName, State1, []),
		case StateName of
		    connection ->
			ssl_connection:hibernate_after(StateName, State, Events);
		    _ ->
			{next_state, StateName, State, Events}
		end
	end
    catch throw:#alert{} = Alert ->
	    handle_own_alert(Alert, Version, StateName, State0)
    end;

%%% TLS record protocol level application data messages
handle_common_event(internal, #ssl_tls{type = ?APPLICATION_DATA, fragment = Data}, StateName, State) ->
    {next_state, StateName, State, [{next_event, internal, {application_data, Data}}]};
%%% TLS record protocol level change cipher messages
handle_common_event(internal, #ssl_tls{type = ?CHANGE_CIPHER_SPEC, fragment = Data}, StateName, State) ->
    {next_state, StateName, State, [{next_event, internal, #change_cipher_spec{type = Data}}]};
%%% TLS record protocol level Alert messages
handle_common_event(internal, #ssl_tls{type = ?ALERT, fragment = EncAlerts}, StateName,
		    #state{negotiated_version = Version} = State) ->
    case decode_alerts(EncAlerts) of
	Alerts = [_|_] ->
	    handle_alerts(Alerts,  {next_state, StateName, State});
	#alert{} = Alert ->
	    handle_own_alert(Alert, Version, StateName, State)
    end;
%% Ignore unknown TLS record level protocol messages
handle_common_event(internal, #ssl_tls{type = _Unknown}, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% Description:This function is called by a gen_fsm when it is about
%% to terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%--------------------------------------------------------------------
terminate(Reason, StateName, State) ->
    ssl_connection:terminate(Reason, StateName, State).

%%--------------------------------------------------------------------
%% code_change(OldVsn, StateName, State, Extra) -> {ok, StateName, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {?GEN_STATEM_CB_MODE, StateName, State}.

format_status(Type, Data) ->
    ssl_connection:format_status(Type, Data).

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
encode_handshake(Handshake, Version, ConnectionStates0, Hist0) ->
    {Seq, ConnectionStates} = sequence(ConnectionStates0),
    {EncHandshake, Frag} = dtls_handshake:encode_handshake(Handshake, Version, Seq),
    Hist = ssl_handshake:update_handshake_history(Hist0, EncHandshake, false),
    {Frag, ConnectionStates, Hist}.

encode_change_cipher(#change_cipher_spec{}, Version, Epoch, ConnectionStates) ->
    dtls_record:encode_plain_text(?CHANGE_CIPHER_SPEC, Version, {Epoch, <<1:8>>}, ConnectionStates).

encode_handshake_flight(Flight, ConnectionStates) ->
    MSS = 1400,
    encode_handshake_records(Flight, ConnectionStates, MSS, init_pack_records()).

encode_handshake_records([], CS, _MSS, Recs) ->
    {finish_pack_records(Recs), CS};

encode_handshake_records([{Version, Epoch, Frag = #change_cipher_spec{}}|Tail], ConnectionStates0, MSS, Recs0) ->
    {Encoded, ConnectionStates} =
        encode_change_cipher(Frag, Version, Epoch,ConnectionStates0),
    Recs = append_pack_records([Encoded], MSS, Recs0),
    encode_handshake_records(Tail, ConnectionStates, MSS, Recs);

encode_handshake_records([{Version, Epoch, {MsgType, MsgSeq, Bin}}|Tail], CS0, MSS, Recs0 = {Buf0, _}) ->
    Space = MSS - iolist_size(Buf0),
    Len = byte_size(Bin),
    {Encoded, CS} =
	encode_handshake_record(Version, Epoch, Space, MsgType, MsgSeq, Len, Bin, 0, MSS, [], CS0),
    Recs = append_pack_records(Encoded, MSS, Recs0),
    encode_handshake_records(Tail, CS, MSS, Recs).

%% TODO: move to dtls_handshake????
encode_handshake_record(_Version, _Epoch, _Space, _MsgType, _MsgSeq, _Len, <<>>, _Offset, _MRS, Encoded, CS)
  when length(Encoded) > 0 ->
    %% make sure we encode at least one segment (for empty messages like Server Hello Done
    {lists:reverse(Encoded), CS};

encode_handshake_record(Version, Epoch, Space, MsgType, MsgSeq, Len, Bin,
			Offset, MRS, Encoded0, CS0) ->
    MaxFragmentLen = Space - 25,
    case Bin of
	<<BinFragment:MaxFragmentLen/bytes, Rest/binary>> ->
	    ok;
	_ ->
	    BinFragment = Bin,
	    Rest = <<>>
    end,
    FragLength = byte_size(BinFragment),
    Frag = [MsgType, ?uint24(Len), ?uint16(MsgSeq), ?uint24(Offset), ?uint24(FragLength), BinFragment],
    {Encoded, CS} = ssl_record:encode_handshake({Epoch, Frag}, Version, CS0),
    encode_handshake_record(Version, Epoch, MRS, MsgType, MsgSeq, Len, Rest, Offset + FragLength, MRS, [Encoded|Encoded0], CS).

init_pack_records() ->
    {[], []}.

append_pack_records([], MSS, Recs = {Buf0, Acc0}) ->
    Remaining = MSS - iolist_size(Buf0),
    if Remaining < 12 ->
	    {[], [lists:reverse(Buf0)|Acc0]};
       true ->
	    Recs
    end;
append_pack_records([Head|Tail], MSS, {Buf0, Acc0}) ->
    TotLen = iolist_size(Buf0) + iolist_size(Head),
    if TotLen > MSS ->
	    append_pack_records(Tail, MSS, {[Head], [lists:reverse(Buf0)|Acc0]});
       true ->
	    append_pack_records(Tail, MSS, {[Head|Buf0], Acc0})
    end.

finish_pack_records({[], Acc}) ->
    lists:reverse(Acc);
finish_pack_records({Buf, Acc}) ->
    lists:reverse([lists:reverse(Buf)|Acc]).

decode_alerts(Bin) ->
    ssl_alert:decode(Bin).

initial_state(Role, Host, Port, Socket, {SSLOptions, SocketOptions, Tracker}, User,
	      {CbModule, DataTag, CloseTag, ErrorTag}) ->
    ConnectionStates = init_connection_states(Role, SSLOptions#ssl_options.hello_verify),

    SessionCacheCb = case application:get_env(ssl, session_cb) of
			 {ok, Cb} when is_atom(Cb) ->
			    Cb;
			 _  ->
			     ssl_session_cache
		     end,

    Monitor = erlang:monitor(process, User),

    #state{socket_options = SocketOptions,
	   ssl_options = SSLOptions,
	   session = #session{is_resumable = new},
	   transport_cb = CbModule,
	   data_tag = DataTag,
	   close_tag = CloseTag,
	   error_tag = ErrorTag,
	   role = Role,
	   host = Host,
	   port = Port,
	   socket = Socket,
	   connection_states = ConnectionStates,
	   protocol_buffers = #protocol_buffers{},
	   user_application = {Monitor, User},
	   user_data_buffer = <<>>,
	   session_cache_cb = SessionCacheCb,
	   renegotiation = {false, first},
	   allow_renegotiate = SSLOptions#ssl_options.client_renegotiation,
	   start_or_recv_from = undefined,
	   protocol_cb = ?MODULE,
	   tracker = Tracker,
	   flight_state = waiting,
	   flight_buffer = []
	  }.

init_connection_states(server, true) ->
    ssl_record:init_connection_states(server, 1);
init_connection_states(Role, _) ->
    ssl_record:init_connection_states(Role, 0).

update_ssl_options_from_sni(OrigSSLOptions, SNIHostname) ->
    SSLOption =
	case OrigSSLOptions#ssl_options.sni_fun of
	    undefined ->
		proplists:get_value(SNIHostname,
				    OrigSSLOptions#ssl_options.sni_hosts);
	    SNIFun ->
		SNIFun(SNIHostname)
	end,
    case SSLOption of
	undefined ->
	    undefined;
	_ ->
	    ssl:handle_options(SSLOption, OrigSSLOptions)
    end.

next_tls_record(Data, #state{protocol_buffers = #protocol_buffers{
						   dtls_record_buffer = Buf0,
						   dtls_cipher_texts = CT0} = Buffers} = State0) ->
    case dtls_record:get_dtls_records(Data, Buf0) of
	{Records, Buf1} ->
	    CT1 = CT0 ++ Records,
	    next_record(State0#state{protocol_buffers =
					 Buffers#protocol_buffers{dtls_record_buffer = Buf1,
								  dtls_cipher_texts = CT1}});

	#alert{} = Alert ->
	    Alert
   end.

next_record(#state{%%flight = #flight{state = finished}, 
		   protocol_buffers =
		       #protocol_buffers{dtls_packets = [], dtls_cipher_texts = [CT | Rest]}
		   = Buffers,
		   connection_states = ConnStates0} = State) ->
    case dtls_record:decode_cipher_text(CT, ConnStates0) of
	{Plain, ConnStates} ->		      
	    {Plain, State#state{protocol_buffers =
				    Buffers#protocol_buffers{dtls_cipher_texts = Rest},
				connection_states = ConnStates}};
	#alert{} = Alert ->
	    {Alert, State}
    end;
next_record(#state{socket = Socket,
		   transport_cb = Transport} = State) -> %% when FlightState =/= finished
    ssl_socket:setopts(Transport, Socket, [{active,once}]),
    {no_record, State};
next_record(State) ->
    {no_record, State}.

next_record_if_active(State =
		      #state{socket_options =
			     #socket_options{active = false}}) ->
    {no_record ,State};

next_record_if_active(State) ->
    next_record(State).

passive_receive(State0 = #state{user_data_buffer = Buffer}, StateName) ->
    case Buffer of
	<<>> ->
	    {Record, State} = next_record(State0),
	    next_event(StateName, Record, State);
	_ ->
	    case read_application_data(<<>>, State0) of
		Stop = {stop, _} ->
		    Stop;
		{Record, State} ->
		    next_event(StateName, Record, State)
	    end
    end.

next_event(StateName, Record, State) ->
    next_event(StateName, Record, State, []).

next_event(connection = StateName, no_record, State0, Actions) ->
    case next_record_if_active(State0) of
	{no_record, State} ->
	    ssl_connection:hibernate_after(StateName, State, Actions);
	{#ssl_tls{} = Record, State} ->
	    {next_state, StateName, State, [{next_event, internal, {dtls_record, Record}} | Actions]};
	{#alert{} = Alert, State} ->
	    {next_state, StateName, State, [{next_event, internal, Alert} | Actions]}
    end;
next_event(StateName, Record, State, Actions) ->
    case Record of
	no_record ->
	    {next_state, StateName, State, Actions};
	#ssl_tls{} = Record ->
	    {next_state, StateName, State, [{next_event, internal, {dtls_record, Record}} | Actions]};
	#alert{} = Alert ->
	    {next_state, StateName, State, [{next_event, internal, Alert} | Actions]}
    end.

read_application_data(Data, #state{user_application = {_Mon, Pid},
				   socket = Socket,
				   transport_cb = Transport,
				   socket_options = SOpts,
				   bytes_to_read = BytesToRead,
				   start_or_recv_from = RecvFrom,
				   timer = Timer,
				   user_data_buffer = Buffer0,
				   tracker = Tracker} = State0) ->
    Buffer1 = if
		  Buffer0 =:= <<>> -> Data;
		  Data =:= <<>> -> Buffer0;
		  true -> <<Buffer0/binary, Data/binary>>
	      end,
    case get_data(SOpts, BytesToRead, Buffer1) of
	{ok, ClientData, Buffer} -> % Send data
	    SocketOpt = deliver_app_data(Transport, Socket, SOpts, ClientData, Pid, RecvFrom, Tracker),
	    cancel_timer(Timer),
	    State = State0#state{user_data_buffer = Buffer,
				 start_or_recv_from = undefined,
				 timer = undefined,
				 bytes_to_read = undefined,
				 socket_options = SocketOpt
				},
	    if
		SocketOpt#socket_options.active =:= false; Buffer =:= <<>> ->
		    %% Passive mode, wait for active once or recv
		    %% Active and empty, get more data
		    next_record_if_active(State);
		true -> %% We have more data
		    read_application_data(<<>>, State)
	    end;
	{more, Buffer} -> % no reply, we need more data
	    next_record(State0#state{user_data_buffer = Buffer});
	{passive, Buffer} ->
	    next_record_if_active(State0#state{user_data_buffer = Buffer});
	{error,_Reason} -> %% Invalid packet in packet mode
	    deliver_packet_error(Transport, Socket, SOpts, Buffer1, Pid, RecvFrom, Tracker),
	    {stop, normal, State0}
    end.

%% Picks ClientData
get_data(_, _, <<>>) ->
    {more, <<>>};
%% Recv timed out save buffer data until next recv
get_data(#socket_options{active=false}, undefined, Buffer) ->
    {passive, Buffer};
get_data(#socket_options{active=Active, packet=Raw}, BytesToRead, Buffer)
  when Raw =:= raw; Raw =:= 0 ->   %% Raw Mode
    if
	Active =/= false orelse BytesToRead =:= 0  ->
	    %% Active true or once, or passive mode recv(0)
	    {ok, Buffer, <<>>};
	byte_size(Buffer) >= BytesToRead ->
	    %% Passive Mode, recv(Bytes)
	    <<Data:BytesToRead/binary, Rest/binary>> = Buffer,
	    {ok, Data, Rest};
	true ->
	    %% Passive Mode not enough data
	    {more, Buffer}
    end;
get_data(#socket_options{packet=Type, packet_size=Size}, _, Buffer) ->
    PacketOpts = [{packet_size, Size}],
    case decode_packet(Type, Buffer, PacketOpts) of
	{more, _} ->
	    {more, Buffer};
	Decoded ->
	    Decoded
    end.

decode_packet(Type, Buffer, PacketOpts) ->
    erlang:decode_packet(Type, Buffer, PacketOpts).

deliver_app_data(Transport, Socket, SOpts = #socket_options{active=Active},
		 Data, Pid, From, Tracker) ->
    send_or_reply(Active, Pid, From, format_reply(Transport, Socket, SOpts, Data, Tracker)),
    case Active of
	once ->
	    SOpts#socket_options{active=false};
	_ ->
	    SOpts
    end.

format_reply(_, _,#socket_options{active = false, mode = Mode, packet = Packet,
				  header = Header}, Data, _) ->
    {ok, do_format_reply(Mode, Packet, Header, Data)};
format_reply(Transport, Socket, #socket_options{active = _, mode = Mode, packet = Packet,
						header = Header}, Data, Tracker) ->
    {ssl, ssl_socket:socket(self(), Transport, Socket, ?MODULE, Tracker),
     do_format_reply(Mode, Packet, Header, Data)}.

deliver_packet_error(Transport, Socket, SO= #socket_options{active = Active}, Data, Pid, From, Tracker) ->
    send_or_reply(Active, Pid, From, format_packet_error(Transport, Socket, SO, Data, Tracker)).

format_packet_error(_, _,#socket_options{active = false, mode = Mode}, Data, _) ->
    {error, {invalid_packet, do_format_reply(Mode, raw, 0, Data)}};
format_packet_error(Transport, Socket, #socket_options{active = _, mode = Mode}, Data, Tracker) ->
    {ssl_error, ssl_socket:socket(self(), Transport, Socket, ?MODULE, Tracker),
     {invalid_packet, do_format_reply(Mode, raw, 0, Data)}}.

do_format_reply(binary, _, N, Data) when N > 0 ->  % Header mode
    header(N, Data);
do_format_reply(binary, _, _, Data) ->
    Data;
do_format_reply(list, Packet, _, Data)
  when Packet == http; Packet == {http, headers};
       Packet == http_bin; Packet == {http_bin, headers};
       Packet == httph; Packet == httph_bin ->
    Data;
do_format_reply(list, _,_, Data) ->
    binary_to_list(Data).

header(0, <<>>) ->
    <<>>;
header(_, <<>>) ->
    [];
header(0, Binary) ->
    Binary;
header(N, Binary) ->
    <<?BYTE(ByteN), NewBinary/binary>> = Binary,
    [ByteN | header(N-1, NewBinary)].

send_or_reply(false, _Pid, From, Data) when From =/= undefined ->
    gen_statem:reply(From, Data);
%% Can happen when handling own alert or udp error/close and there is
%% no outstanding gen_fsm sync events
send_or_reply(false, no_pid, _, _) ->
    ok;
send_or_reply(_, Pid, _From, Data) ->
    send_user(Pid, Data).

send_user(Pid, Msg) ->
    Pid ! Msg.

dtls_handshake_events(Handle, StateName,
		     #state{protocol_buffers =
				#protocol_buffers{dtls_packets = [Packet]} = Buffers} = State0, Acc) ->
    {State, Event} = Handle(Packet, {StateName,
				     State0#state{protocol_buffers =
						      Buffers#protocol_buffers{dtls_packets = []}}}),
    {State, lists:reverse([Event |Acc])};
dtls_handshake_events(Handle, StateName,
		     #state{protocol_buffers =
				#protocol_buffers{dtls_packets =
						      [Packet | Packets]} = Buffers} = State0, Acc) ->
    {State, Event} = Handle(Packet, {StateName, State0#state{protocol_buffers =
								 Buffers#protocol_buffers{dtls_packets =
											      Packets}}}),
    dtls_handshake_events(Handle, StateName, State, [Event | Acc]);

dtls_handshake_events(_Handle, _, #state{}, _) ->
     throw(?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE)).

write_application_data(Data0, From,
		       #state{socket = Socket,
			      negotiated_version = Version,
			      transport_cb = Transport,
			      connection_states = ConnectionStates0,
			      socket_options = SockOpts,
			      ssl_options = #ssl_options{renegotiate_at = RenegotiateAt}} = State) ->
    Data = encode_packet(Data0, SockOpts),

    case time_to_renegotiate(Data, ConnectionStates0, RenegotiateAt) of
	true ->
	    renegotiate(State#state{renegotiation = {true, internal}},
			[{next_event, {call, From}, {application_data, Data0}}]);
	false ->
	    {Msgs, ConnectionStates} = ssl_record:encode_data(Data, Version, ConnectionStates0),
	    Result = Transport:send(Socket, Msgs),
		ssl_connection:hibernate_after(connection, State#state{connection_states = ConnectionStates},
					       [{reply, From, Result}])
    end.

encode_packet(Data, #socket_options{packet=Packet}) ->
    case Packet of
	1 -> encode_size_packet(Data, 8,  (1 bsl 8) - 1);
	2 -> encode_size_packet(Data, 16, (1 bsl 16) - 1);
	4 -> encode_size_packet(Data, 32, (1 bsl 32) - 1);
	_ -> Data
    end.

encode_size_packet(Bin, Size, Max) ->
    Len = erlang:byte_size(Bin),
    case Len > Max of
	true  -> throw({error, {badarg, {packet_to_large, Len, Max}}});
	false -> <<Len:Size, Bin/binary>>
    end.

time_to_renegotiate(_Data,
		    #connection_states{current_write =
					   #connection_state{sequence_number = Num}},
		    RenegotiateAt) ->

    %% We could do test:
    %% is_time_to_renegotiate((erlang:byte_size(_Data) div ?MAX_PLAIN_TEXT_LENGTH) + 1, RenegotiateAt),
    %% but we chose to have a some what lower renegotiateAt and a much cheaper test
    is_time_to_renegotiate(Num, RenegotiateAt).

is_time_to_renegotiate(N, M) when N < M->
    false;
is_time_to_renegotiate(_,_) ->
    true.
renegotiate(#state{role = client} = State, Actions) ->
    %% Handle same way as if server requested
    %% the renegotiation
    Hs0 = ssl_handshake:init_handshake_history(),
    {next_state, connection, State#state{tls_handshake_history = Hs0,
					 protocol_buffers = #protocol_buffers{}},
     [{next_event, internal, #hello_request{}} | Actions]};

renegotiate(#state{role = server,
		   connection_states = CS0} = State0, Actions) ->
    HelloRequest = ssl_handshake:hello_request(),
    State1 = send_handshake(HelloRequest, State0#state{connection_states =
							    CS0#connection_states{dtls_write_msg_seq = 0}}),
    Hs0 = ssl_handshake:init_handshake_history(),
    {Record, State} = next_record(State1#state{tls_handshake_history = Hs0,
					       protocol_buffers = #protocol_buffers{}}),
    next_event(hello, Record, State, Actions).

handle_alerts([], Result) ->
    Result;
handle_alerts(_, {stop,_} = Stop) ->
    Stop;
handle_alerts(_, {stop,_,_} = Stop) ->
    Stop;
handle_alerts([Alert | Alerts], {next_state, StateName, State}) ->
     handle_alerts(Alerts, handle_alert(Alert, StateName, State));
handle_alerts([Alert | Alerts], {next_state, StateName, State, _Actions}) ->
     handle_alerts(Alerts, handle_alert(Alert, StateName, State)).

handle_alert(#alert{level = ?FATAL} = Alert, StateName,
	     #state{socket = Socket, transport_cb = Transport,
		    ssl_options = SslOpts, start_or_recv_from = From, host = Host,
		    port = Port, session = Session, user_application = {_Mon, Pid},
		    role = Role, socket_options = Opts, tracker = Tracker}) ->
    invalidate_session(Role, Host, Port, Session),
    log_alert(SslOpts#ssl_options.log_alert, StateName, Alert),
    alert_user(Transport, Tracker, Socket, StateName, Opts, Pid, From, Alert, Role),
    {stop, normal};

handle_alert(#alert{level = ?WARNING, description = ?CLOSE_NOTIFY} = Alert,
	     StateName, State) ->
    handle_normal_shutdown(Alert, StateName, State),
    {stop, {shutdown, peer_close}};

handle_alert(#alert{level = ?WARNING, description = ?NO_RENEGOTIATION} = Alert, StateName,
	     #state{ssl_options = SslOpts, renegotiation = {true, internal}} = State) ->
    log_alert(SslOpts#ssl_options.log_alert, StateName, Alert),
    handle_normal_shutdown(Alert, StateName, State),
    {stop, {shutdown, peer_close}};

handle_alert(#alert{level = ?WARNING, description = ?NO_RENEGOTIATION} = Alert, StateName,
	     #state{ssl_options = SslOpts, renegotiation = {true, From}} = State0) ->
    log_alert(SslOpts#ssl_options.log_alert, StateName, Alert),
    gen_statem:reply(From, {error, renegotiation_rejected}),
    {Record, State} = next_record(State0),
    %% Go back to connection!
    next_event(connection, Record, State);

%% Gracefully log and ignore all other warning alerts
handle_alert(#alert{level = ?WARNING} = Alert, StateName,
	     #state{ssl_options = SslOpts} = State0) ->
    log_alert(SslOpts#ssl_options.log_alert, StateName, Alert),
    {Record, State} = next_record(State0),
    next_event(StateName, Record, State).

alert_user(Transport, Tracker, Socket, connection, Opts, Pid, From, Alert, Role) ->
    alert_user(Transport, Tracker, Socket, Opts#socket_options.active, Pid, From, Alert, Role);
alert_user(Transport, Tracker, Socket,_, _, _, From, Alert, Role) ->
    alert_user(Transport, Tracker, Socket, From, Alert, Role).

alert_user(Transport, Tracker, Socket, From, Alert, Role) ->
    alert_user(Transport, Tracker, Socket, false, no_pid, From, Alert, Role).

alert_user(_, _, _, false = Active, Pid, From, Alert, Role) ->
    %% If there is an outstanding ssl_accept | recv
    %% From will be defined and send_or_reply will
    %% send the appropriate error message.
    ReasonCode = ssl_alert:reason_code(Alert, Role),
    send_or_reply(Active, Pid, From, {error, ReasonCode});
alert_user(Transport, Tracker, Socket, Active, Pid, From, Alert, Role) ->
    case ssl_alert:reason_code(Alert, Role) of
	closed ->
	    send_or_reply(Active, Pid, From,
			  {ssl_closed, ssl_socket:socket(self(),
							 Transport, Socket, ?MODULE, Tracker)});
	ReasonCode ->
	    send_or_reply(Active, Pid, From,
			  {ssl_error, ssl_socket:socket(self(),
							Transport, Socket, ?MODULE, Tracker), ReasonCode})
    end.

log_alert(true, Info, Alert) ->
    Txt = ssl_alert:alert_txt(Alert),
    error_logger:format("SSL: ~p: ~s\n", [Info, Txt]);
log_alert(false, _, _) ->
    ok.

handle_own_alert(Alert, Version, StateName,
		 #state{transport_cb = Transport,
			socket = Socket,
			connection_states = ConnectionStates,
			ssl_options = SslOpts} = State) ->
    try %% Try to tell the other side
	{BinMsg, _} =
	ssl_alert:encode(Alert, Version, ConnectionStates),
	Transport:send(Socket, BinMsg)
    catch _:_ ->  %% Can crash if we are in a uninitialized state
	    ignore
    end,
    try %% Try to tell the local user
	log_alert(SslOpts#ssl_options.log_alert, StateName, Alert),
	handle_normal_shutdown(Alert,StateName, State)
    catch _:_ ->
	    ok
    end,
    {stop, {shutdown, own_alert}}.

handle_normal_shutdown(Alert, _, #state{socket = Socket,
					transport_cb = Transport,
					start_or_recv_from = StartFrom,
					tracker = Tracker,
					role = Role, renegotiation = {false, first}}) ->
    alert_user(Transport, Tracker,Socket, StartFrom, Alert, Role);

handle_normal_shutdown(Alert, StateName, #state{socket = Socket,
						socket_options = Opts,
						transport_cb = Transport,
						user_application = {_Mon, Pid},
						tracker = Tracker,
						start_or_recv_from = RecvFrom, role = Role}) ->
    alert_user(Transport, Tracker, Socket, StateName, Opts, Pid, RecvFrom, Alert, Role).

handle_close_alert(Data, StateName, State0) ->
    case next_tls_record(Data, State0) of
	{#ssl_tls{type = ?ALERT, fragment = EncAlerts}, State} ->
	    [Alert|_] = decode_alerts(EncAlerts),
	    handle_normal_shutdown(Alert, StateName, State);
	_ ->
	    ok
    end.

cancel_timer(undefined) ->
    ok;
cancel_timer(Timer) ->
    erlang:cancel_timer(Timer),
    ok.

invalidate_session(client, Host, Port, Session) ->
    ssl_manager:invalidate_session(Host, Port, Session);
invalidate_session(server, _, Port, Session) ->
    ssl_manager:invalidate_session(Port, Session).

%% User closes or recursive call!
close({close, Timeout}, Socket, Transport, _,_) ->
    ssl_socket:setopts(Transport, Socket, [{active, false}]),
    Transport:shutdown(Socket, write),
    _ = Transport:recv(Socket, 0, Timeout),
    ok;
%% Peer closed socket
close({shutdown, transport_closed}, Socket, Transport, ConnectionStates, Check) ->
    close({close, 0}, Socket, Transport, ConnectionStates, Check);
%% We generate fatal alert
close({shutdown, own_alert}, Socket, Transport, ConnectionStates, Check) ->
    close({close, ?DEFAULT_TIMEOUT}, Socket, Transport, ConnectionStates, Check);
close(downgrade, _,_,_,_) ->
    ok;
%% Other
close(_, Socket, Transport, _,_) ->
    Transport:close(Socket).

handle_sni_extension(#client_hello{extensions = HelloExtensions}, State0) ->
    case HelloExtensions#hello_extensions.sni of
	undefined ->
	    State0;
	#sni{hostname = Hostname} ->
	    NewOptions = update_ssl_options_from_sni(State0#state.ssl_options, Hostname),
	    case NewOptions of
		undefined ->
		    State0;
		_ ->
		    {ok, Ref, CertDbHandle, FileRefHandle, CacheHandle, CRLDbHandle, OwnCert, Key, DHParams} =
			ssl_config:init(NewOptions, State0#state.role),
		    State0#state{
		      session = State0#state.session#session{own_certificate = OwnCert},
		      file_ref_db = FileRefHandle,
		      cert_db_ref = Ref,
		      cert_db = CertDbHandle,
		      crl_db = CRLDbHandle,
		      session_cache = CacheHandle,
		      private_key = Key,
		      diffie_hellman_params = DHParams,
		      ssl_options = NewOptions,
		      sni_hostname = Hostname
		     }
	    end
    end;
handle_sni_extension(_, State0) ->
    State0.

sequence(#connection_states{dtls_write_msg_seq = Seq} = CS) ->
    {Seq, CS#connection_states{dtls_write_msg_seq = Seq + 1}}.

handle_packet(Address, Port, Packet) ->
    try dtls_record:get_dtls_records(Packet, <<>>) of
	%% expect client hello
	{[#ssl_tls{type = ?HANDSHAKE, version = {254, _}} = Record], <<>>} ->
	    handle_dtls_client_hello(Address, Port, Record);
	_Other ->
	    {error, not_dtls}
    catch
	_Class:_Error ->
	    {error, not_dtls}
    end.

handle_dtls_client_hello(Address, Port,
			 #ssl_tls{epoch = Epoch, sequence_number = Seq,
				  version = Version} = Record) ->
    {[{Hello, _}], _} =
	dtls_handshake:get_dtls_handshake(Record,
					 dtls_handshake:dtls_handshake_new_flight(undefined)),
    #client_hello{client_version = {Major, Minor},
		  random = Random,
		  session_id = SessionId,
		  cipher_suites = CipherSuites,
		  compression_methods = CompressionMethods} = Hello,
    CookieData = [address_to_bin(Address, Port),
		  <<?BYTE(Major), ?BYTE(Minor)>>,
		  Random, SessionId, CipherSuites, CompressionMethods],
    Cookie = crypto:hmac(sha, <<"secret">>, CookieData),

    case Hello of
	#client_hello{cookie = Cookie} ->
	    accept;

	_ ->
	    %% generate HelloVerifyRequest
	    {RequestFragment, _} = dtls_handshake:encode_handshake(
				     dtls_handshake:hello_verify_request(Cookie),
				     Version, 0),
	    HelloVerifyRequest =
		dtls_record:encode_tls_cipher_text(?HANDSHAKE, Version, Epoch, Seq, RequestFragment),
	    {reply, HelloVerifyRequest}
    end.

address_to_bin({A,B,C,D}, Port) ->
    <<0:80,16#ffff:16,A,B,C,D,Port:16>>;
address_to_bin({A,B,C,D,E,F,G,H}, Port) ->
    <<A:16,B:16,C:16,D:16,E:16,F:16,G:16,H:16,Port:16>>.
