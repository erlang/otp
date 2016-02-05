%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2015. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% Purpose: Handles an ssl connection, e.i. both the setup
%% e.i. SSL-Handshake, SSL-Alert and SSL-Cipher protocols and delivering
%% data to the application. All data on the connectinon is received and 
%% sent according to the SSL-record protocol.  
%%----------------------------------------------------------------------

-module(tls_connection).

-behaviour(gen_fsm).

-include("tls_connection.hrl").
-include("tls_handshake.hrl").
-include("ssl_alert.hrl").
-include("tls_record.hrl").
-include("ssl_cipher.hrl").
-include("ssl_api.hrl").
-include("ssl_internal.hrl").
-include("ssl_srp.hrl").
-include_lib("public_key/include/public_key.hrl"). 

%% Internal application API

%% Setup
-export([start_fsm/8]).

%% State transition handling	 
-export([next_record/1, next_state/4, next_state_connection/2]).

%% Handshake handling
-export([renegotiate/1, send_handshake/2, send_change_cipher/2]).

%% Alert and close handling
-export([send_alert/2, handle_own_alert/4, handle_close_alert/3,
	 handle_normal_shutdown/3, handle_unexpected_message/3,
	 close/5, alert_user/6, alert_user/9
	]).

%% Data handling
-export([write_application_data/3, read_application_data/2,
	 passive_receive/2,  next_record_if_active/1]).

%% Called by tls_connection_sup
-export([start_link/7]). 

%% gen_fsm callbacks
-export([init/1, hello/2, certify/2, cipher/2,
	 abbreviated/2, connection/2, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4, format_status/2]).

%%====================================================================
%% Internal application API
%%====================================================================	     
start_fsm(Role, Host, Port, Socket, {#ssl_options{erl_dist = false},_, Tracker} = Opts,
	  User, {CbModule, _,_, _} = CbInfo, 
	  Timeout) -> 
    try 
	{ok, Pid} = tls_connection_sup:start_child([Role, Host, Port, Socket, 
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
	{ok, Pid} = tls_connection_sup:start_child_dist([Role, Host, Port, Socket, 
							 Opts, User, CbInfo]), 
	{ok, SslSocket} = ssl_connection:socket_control(?MODULE, Socket, Pid, CbModule, Tracker),
	ok = ssl_connection:handshake(SslSocket, Timeout),
	{ok, SslSocket} 
    catch
	error:{badmatch, {error, _} = Error} ->
	    Error
    end.

send_handshake(Handshake, #state{negotiated_version = Version,
				 socket = Socket,
				 transport_cb = Transport,
				 tls_handshake_history = Hist0,
				 connection_states = ConnectionStates0} = State0) ->
    {BinHandshake, ConnectionStates, Hist} =
	encode_handshake(Handshake, Version, ConnectionStates0, Hist0),
    Transport:send(Socket, BinHandshake),
    State0#state{connection_states = ConnectionStates,
		tls_handshake_history = Hist
	       }.

send_alert(Alert, #state{negotiated_version = Version,
			 socket = Socket,
			 transport_cb = Transport,
			 connection_states = ConnectionStates0} = State0) ->
    {BinMsg, ConnectionStates} =
	ssl_alert:encode(Alert, Version, ConnectionStates0),
    Transport:send(Socket, BinMsg),
    State0#state{connection_states = ConnectionStates}.

send_change_cipher(Msg, #state{connection_states = ConnectionStates0,
			       socket = Socket,
			       negotiated_version = Version,
			       transport_cb = Transport} = State0) ->
    {BinChangeCipher, ConnectionStates} =
	encode_change_cipher(Msg, Version, ConnectionStates0),
    Transport:send(Socket, BinChangeCipher),
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
    State = initial_state(Role, Host, Port, Socket, Options, User, CbInfo),
    gen_fsm:enter_loop(?MODULE, [], hello, State, get_timeout(State)).

%%--------------------------------------------------------------------
%% Description:There should be one instance of this function for each
%% possible state name. Whenever a gen_fsm receives an event sent
%% using gen_fsm:send_event/2, the instance of this function with the
%% same name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
hello(start, #state{host = Host, port = Port, role = client,
		    ssl_options = SslOpts,
		    session = #session{own_certificate = Cert} = Session0,
		    session_cache = Cache, session_cache_cb = CacheCb,
		    transport_cb = Transport, socket = Socket,
		    connection_states = ConnectionStates0,
		    renegotiation = {Renegotiation, _}} = State0) ->
    Hello = tls_handshake:client_hello(Host, Port, ConnectionStates0, SslOpts,
				       Cache, CacheCb, Renegotiation, Cert),
    
    Version = Hello#client_hello.client_version,
    HelloVersion = tls_record:lowest_protocol_version(SslOpts#ssl_options.versions),
    Handshake0 = ssl_handshake:init_handshake_history(),
    {BinMsg, ConnectionStates, Handshake} =
        encode_handshake(Hello,  HelloVersion, ConnectionStates0, Handshake0),
    Transport:send(Socket, BinMsg),
    State1 = State0#state{connection_states = ConnectionStates,
			  negotiated_version = Version, %% Requested version
			  session =
			      Session0#session{session_id = Hello#client_hello.session_id},
			  tls_handshake_history = Handshake},
    {Record, State} = next_record(State1),
    next_state(hello, hello, Record, State);

hello(Hello = #client_hello{client_version = ClientVersion,
			    extensions = #hello_extensions{ec_point_formats = EcPointFormats,
							   elliptic_curves = EllipticCurves}},
      State = #state{connection_states = ConnectionStates0,
		     port = Port, session = #session{own_certificate = Cert} = Session0,
		     renegotiation = {Renegotiation, _},
		     session_cache = Cache,
		     session_cache_cb = CacheCb,
		     negotiated_protocol = CurrentProtocol,
		     key_algorithm = KeyExAlg,
		     ssl_options = SslOpts}) ->

    case tls_handshake:hello(Hello, SslOpts, {Port, Session0, Cache, CacheCb,
					      ConnectionStates0, Cert, KeyExAlg}, Renegotiation) of
        #alert{} = Alert ->
            handle_own_alert(Alert, ClientVersion, hello, State);
        {Version, {Type, Session},
	 ConnectionStates, Protocol0, ServerHelloExt, HashSign} ->
	    Protocol = case Protocol0 of
			   undefined -> CurrentProtocol;
			   _ -> Protocol0
		       end,
	    ssl_connection:hello({common_client_hello, Type, ServerHelloExt},
				 State#state{connection_states  = ConnectionStates,
					     negotiated_version = Version,
					     hashsign_algorithm = HashSign,
					     session = Session,
					     client_ecc = {EllipticCurves, EcPointFormats},
					     negotiated_protocol = Protocol}, ?MODULE)
    end;

hello(Hello = #server_hello{},
      #state{connection_states = ConnectionStates0,
	     negotiated_version = ReqVersion,
	     role = client,
	     renegotiation = {Renegotiation, _},
	     ssl_options = SslOptions} = State) ->
    case tls_handshake:hello(Hello, SslOptions, ConnectionStates0, Renegotiation) of
	#alert{} = Alert ->
	    handle_own_alert(Alert, ReqVersion, hello, State);
	{Version, NewId, ConnectionStates, ProtoExt, Protocol} ->
	    ssl_connection:handle_session(Hello, 
					  Version, NewId, ConnectionStates, ProtoExt, Protocol, State)
    end;

hello(Msg, State) ->
    ssl_connection:hello(Msg, State, ?MODULE).

abbreviated(Msg, State) ->
    ssl_connection:abbreviated(Msg, State, ?MODULE).

certify(Msg, State) ->
    ssl_connection:certify(Msg, State, ?MODULE).

cipher(Msg, State) ->
     ssl_connection:cipher(Msg, State, ?MODULE).

connection(#hello_request{}, #state{host = Host, port = Port,
				    session = #session{own_certificate = Cert} = Session0,
				    session_cache = Cache, session_cache_cb = CacheCb,
				    ssl_options = SslOpts,
				    connection_states = ConnectionStates0,
				    renegotiation = {Renegotiation, _}} = State0) ->
    Hello = tls_handshake:client_hello(Host, Port, ConnectionStates0, SslOpts,
				       Cache, CacheCb, Renegotiation, Cert),
    State1 = send_handshake(Hello, State0),
    {Record, State} =
	next_record(
	  State1#state{session = Session0#session{session_id
						  = Hello#client_hello.session_id}}),
    next_state(connection, hello, Record, State);

connection(#client_hello{} = Hello, #state{role = server, allow_renegotiate = true} = State) ->
    %% Mitigate Computational DoS attack
    %% http://www.educatedguesswork.org/2011/10/ssltls_and_computational_dos.html
    %% http://www.thc.org/thc-ssl-dos/ Rather than disabling client
    %% initiated renegotiation we will disallow many client initiated
    %% renegotiations immediately after each other.
    erlang:send_after(?WAIT_TO_ALLOW_RENEGOTIATION, self(), allow_renegotiate),
    hello(Hello, State#state{allow_renegotiate = false});

connection(#client_hello{}, #state{role = server, allow_renegotiate = false} = State0) ->
    Alert = ?ALERT_REC(?WARNING, ?NO_RENEGOTIATION),
    State = send_alert(Alert, State0),
    next_state_connection(connection, State);
  
connection(Msg, State) ->
     ssl_connection:connection(Msg, State, tls_connection).

%%--------------------------------------------------------------------
%% Description: Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event. Not currently used!
%%--------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State, get_timeout(State)}.

%%--------------------------------------------------------------------
%% Description: Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/2,3, this function is called to handle
%% the event.
%%--------------------------------------------------------------------
handle_sync_event(Event, From, StateName, State) ->
    ssl_connection:handle_sync_event(Event, From, StateName, State).

%%--------------------------------------------------------------------
%% Description: This function is called by a gen_fsm when it receives any
%% other message than a synchronous or asynchronous event
%% (or a system message).
%%--------------------------------------------------------------------

%% raw data from socket, unpack records
handle_info({Protocol, _, Data}, StateName,
            #state{data_tag = Protocol} = State0) ->
    case next_tls_record(Data, State0) of
	{Record, State} ->
	    next_state(StateName, StateName, Record, State);
	#alert{} = Alert ->
	    handle_normal_shutdown(Alert, StateName, State0), 
	    {stop, {shutdown, own_alert}, State0}
    end;

handle_info({CloseTag, Socket}, StateName,
            #state{socket = Socket, close_tag = CloseTag,
		   negotiated_version = Version} = State) ->
    %% Note that as of TLS 1.1,
    %% failure to properly close a connection no longer requires that a
    %% session not be resumed.  This is a change from TLS 1.0 to conform
    %% with widespread implementation practice.
    case Version of
	{1, N} when N >= 1 ->
	    ok;
	_ ->
	    %% As invalidate_sessions here causes performance issues,
	    %% we will conform to the widespread implementation
	    %% practice and go aginst the spec
	    %%invalidate_session(Role, Host, Port, Session)
	    ok
    end,
    handle_normal_shutdown(?ALERT_REC(?FATAL, ?CLOSE_NOTIFY), StateName, State),
    {stop, {shutdown, transport_closed}, State};

handle_info(Msg, StateName, State) ->
    ssl_connection:handle_info(Msg, StateName, State).

%%--------------------------------------------------------------------
%% Description:This function is called by a gen_fsm when it is about
%% to terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%--------------------------------------------------------------------
terminate(Reason, StateName, State) ->
    catch ssl_connection:terminate(Reason, StateName, State).

%%--------------------------------------------------------------------
%% code_change(OldVsn, StateName, State, Extra) -> {ok, StateName, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State0, {Direction, From, To}) ->
    State = convert_state(State0, Direction, From, To),
    {ok, StateName, State};
code_change(_OldVsn, StateName, State, _) ->
    {ok, StateName, State}.

format_status(Type, Data) ->
    ssl_connection:format_status(Type, Data).

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
encode_handshake(Handshake, Version, ConnectionStates0, Hist0) ->
    Frag = tls_handshake:encode_handshake(Handshake, Version),
    Hist = ssl_handshake:update_handshake_history(Hist0, Frag),
    {Encoded, ConnectionStates} =
        ssl_record:encode_handshake(Frag, Version, ConnectionStates0),
    {Encoded, ConnectionStates, Hist}.

encode_change_cipher(#change_cipher_spec{}, Version, ConnectionStates) ->
    ssl_record:encode_change_cipher_spec(Version, ConnectionStates).

decode_alerts(Bin) ->
    ssl_alert:decode(Bin).

initial_state(Role, Host, Port, Socket, {SSLOptions, SocketOptions, Tracker}, User,
	      {CbModule, DataTag, CloseTag, ErrorTag}) ->
    ConnectionStates = ssl_record:init_connection_states(Role),
    
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
	   send_queue = queue:new(),
	   protocol_cb = ?MODULE,
	   tracker = Tracker
	  }.


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

next_state(Current,_, #alert{} = Alert, #state{negotiated_version = Version} = State) ->
    handle_own_alert(Alert, Version, Current, State);

next_state(_,Next, no_record, State) ->
    {next_state, Next, State, get_timeout(State)};

next_state(Current, Next, #ssl_tls{type = ?ALERT, fragment = EncAlerts}, #state{negotiated_version = Version} = State) ->
    case decode_alerts(EncAlerts) of
	Alerts = [_|_] ->
	    handle_alerts(Alerts,  {next_state, Next, State, get_timeout(State)});
	#alert{} = Alert ->
	    handle_own_alert(Alert, Version, Current, State)
    end;
next_state(Current, Next, #ssl_tls{type = ?HANDSHAKE, fragment = Data},
	   State0 = #state{protocol_buffers =
			       #protocol_buffers{tls_handshake_buffer = Buf0} = Buffers,
			   negotiated_version = Version}) ->
    Handle = 
   	fun({#hello_request{} = Packet, _}, {next_state, connection = SName, State}) ->
   		%% This message should not be included in handshake
   		%% message hashes. Starts new handshake (renegotiation)
		Hs0 = ssl_handshake:init_handshake_history(),
		?MODULE:SName(Packet, State#state{tls_handshake_history=Hs0,
   						  renegotiation = {true, peer}});
   	   ({#hello_request{} = Packet, _}, {next_state, SName, State}) ->
   		%% This message should not be included in handshake
   		%% message hashes. Already in negotiation so it will be ignored!
   		?MODULE:SName(Packet, State);
	   ({#client_hello{} = Packet, Raw}, {next_state, connection = SName, HState0}) ->
		HState = handle_sni_extension(Packet, HState0),
		Version = Packet#client_hello.client_version,
		Hs0 = ssl_handshake:init_handshake_history(),
		Hs1 = ssl_handshake:update_handshake_history(Hs0, Raw),
		?MODULE:SName(Packet, HState#state{tls_handshake_history=Hs1,
						   renegotiation = {true, peer}});
	   ({Packet, Raw}, {next_state, SName, HState0 = #state{tls_handshake_history=Hs0}}) ->
		HState = handle_sni_extension(Packet, HState0),
		Hs1 = ssl_handshake:update_handshake_history(Hs0, Raw),
		?MODULE:SName(Packet, HState#state{tls_handshake_history=Hs1});
   	   (_, StopState) -> StopState
   	end,
    try
	{Packets, Buf} = tls_handshake:get_tls_handshake(Version,Data,Buf0),
	State = State0#state{protocol_buffers =
				 Buffers#protocol_buffers{tls_packets = Packets,
							  tls_handshake_buffer = Buf}},
	handle_tls_handshake(Handle, Next, State)
    catch throw:#alert{} = Alert ->
	    handle_own_alert(Alert, Version, Current, State0)
    end;

next_state(_, StateName, #ssl_tls{type = ?APPLICATION_DATA, fragment = Data}, State0) ->
    case read_application_data(Data, State0) of
	Stop = {stop,_,_} ->
   	    Stop;
	{Record, State} ->
   	    next_state(StateName, StateName, Record, State)
    end;
next_state(Current, Next, #ssl_tls{type = ?CHANGE_CIPHER_SPEC, fragment = <<1>>} = 
	       _ChangeCipher, 
 	   #state{connection_states = ConnectionStates0} = State0) 
  when Next == cipher; Next == abbreviated ->
    ConnectionStates1 =
	ssl_record:activate_pending_connection_state(ConnectionStates0, read),
    {Record, State} = next_record(State0#state{connection_states = ConnectionStates1}),
    next_state(Current, Next, Record, State#state{expecting_finished = true});
next_state(Current, _Next, #ssl_tls{type = ?CHANGE_CIPHER_SPEC, fragment = <<1>>} = 
 	       _ChangeCipher, #state{negotiated_version = Version} = State) ->
    handle_own_alert(?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE), Version, Current, State);
next_state(Current, Next, #ssl_tls{type = _Unknown}, State0) ->
    %% Ignore unknown type 
    {Record, State} = next_record(State0),
    next_state(Current, Next, Record, State).

next_tls_record(Data, #state{protocol_buffers = #protocol_buffers{tls_record_buffer = Buf0,
						tls_cipher_texts = CT0} = Buffers} = State0) ->
    case tls_record:get_tls_records(Data, Buf0) of
	{Records, Buf1} ->
	    CT1 = CT0 ++ Records,
	    next_record(State0#state{protocol_buffers =
					 Buffers#protocol_buffers{tls_record_buffer = Buf1,
								  tls_cipher_texts = CT1}});
	#alert{} = Alert ->
	    Alert
    end.

next_record(#state{protocol_buffers = #protocol_buffers{tls_packets = [], tls_cipher_texts = []},
		   socket = Socket,
		   transport_cb = Transport} = State) ->
    ssl_socket:setopts(Transport, Socket, [{active,once}]),
    {no_record, State};
next_record(#state{protocol_buffers =
		       #protocol_buffers{tls_packets = [], tls_cipher_texts = [CT | Rest]}
		   = Buffers,
		   connection_states = ConnStates0,
		   ssl_options = #ssl_options{padding_check = Check}} = State) ->
    case tls_record:decode_cipher_text(CT, ConnStates0, Check) of
	{Plain, ConnStates} ->		      
	    {Plain, State#state{protocol_buffers =
				    Buffers#protocol_buffers{tls_cipher_texts = Rest},
				connection_states = ConnStates}};
	#alert{} = Alert ->
	    {Alert, State}
    end;
next_record(State) ->
    {no_record, State}.

next_record_if_active(State = 
		      #state{socket_options = 
			     #socket_options{active = false}}) -> 
    {no_record ,State};

next_record_if_active(State) ->
    next_record(State).

next_state_connection(StateName, #state{send_queue = Queue0,
					negotiated_version = Version,
					socket = Socket,
					transport_cb = Transport,
					connection_states = ConnectionStates0
				       } = State) ->     
    %% Send queued up data that was queued while renegotiating
    case queue:out(Queue0) of
	{{value, {From, Data}}, Queue} ->
	    {Msgs, ConnectionStates} = 
		ssl_record:encode_data(Data, Version, ConnectionStates0),
	    Result = Transport:send(Socket, Msgs),
	    gen_fsm:reply(From, Result),
	    next_state_connection(StateName,
				  State#state{connection_states = ConnectionStates,
						      send_queue = Queue});		
	{empty, Queue0} ->
	    next_state_is_connection(StateName, State)
    end.

%% In next_state_is_connection/1: clear tls_handshake,
%% premaster_secret and public_key_info (only needed during handshake)
%% to reduce memory foot print of a connection.
next_state_is_connection(_, State = 
		      #state{start_or_recv_from = RecvFrom,
			     socket_options =
			     #socket_options{active = false}}) when RecvFrom =/= undefined ->
    passive_receive(State#state{premaster_secret = undefined,
				public_key_info = undefined,
				tls_handshake_history = ssl_handshake:init_handshake_history()}, connection);

next_state_is_connection(StateName, State0) ->
    {Record, State} = next_record_if_active(State0),
    next_state(StateName, connection, Record, State#state{premaster_secret = undefined,
							  public_key_info = undefined,
							  tls_handshake_history = ssl_handshake:init_handshake_history()}).

passive_receive(State0 = #state{user_data_buffer = Buffer}, StateName) -> 
    case Buffer of
	<<>> ->
	    {Record, State} = next_record(State0),
	    next_state(StateName, StateName, Record, State);
	_ ->
	    case read_application_data(<<>>, State0) of
		Stop = {stop, _, _} ->
		    Stop;
		{Record, State} ->
		    next_state(StateName, StateName, Record, State)
	    end
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

get_timeout(#state{ssl_options=#ssl_options{hibernate_after = undefined}}) ->
    infinity;
get_timeout(#state{ssl_options=#ssl_options{hibernate_after = HibernateAfter}}) ->
    HibernateAfter.

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

decode_packet({http, headers}, Buffer, PacketOpts) ->
    decode_packet(httph, Buffer, PacketOpts);
decode_packet({http_bin, headers}, Buffer, PacketOpts) ->
    decode_packet(httph_bin, Buffer, PacketOpts);
decode_packet(Type, Buffer, PacketOpts) ->
    erlang:decode_packet(Type, Buffer, PacketOpts).

%% Just like with gen_tcp sockets, an ssl socket that has been configured with
%% {packet, http} (or {packet, http_bin}) will automatically switch to expect
%% HTTP headers after it sees a HTTP Request or HTTP Response line. We
%% represent the current state as follows:
%%    #socket_options.packet =:= http: Expect a HTTP Request/Response line
%%    #socket_options.packet =:= {http, headers}: Expect HTTP Headers
%% Note that if the user has explicitly configured the socket to expect
%% HTTP headers using the {packet, httph} option, we don't do any automatic
%% switching of states.
deliver_app_data(Transport, Socket, SOpts = #socket_options{active=Active, packet=Type},
		 Data, Pid, From, Tracker) ->
    send_or_reply(Active, Pid, From, format_reply(Transport, Socket, SOpts, Data, Tracker)),
    SO = case Data of
	     {P, _, _, _} when ((P =:= http_request) or (P =:= http_response)),
			       ((Type =:= http) or (Type =:= http_bin)) ->
	         SOpts#socket_options{packet={Type, headers}};
	     http_eoh when tuple_size(Type) =:= 2 ->
                 % End of headers - expect another Request/Response line
	         {Type1, headers} = Type,
	         SOpts#socket_options{packet=Type1};
	     _ ->
	         SOpts
	 end,
    case Active of
        once ->
            SO#socket_options{active=false};
	_ ->
	    SO
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
    gen_fsm:reply(From, Data);
%% Can happen when handling own alert or tcp error/close and there is
%% no outstanding gen_fsm sync events
send_or_reply(false, no_pid, _, _) ->
    ok;
send_or_reply(_, Pid, _From, Data) ->
    send_user(Pid, Data).

send_user(Pid, Msg) ->
    Pid ! Msg.

handle_tls_handshake(Handle, StateName,
		     #state{protocol_buffers =
				#protocol_buffers{tls_packets = [Packet]} = Buffers} = State) ->
    FsmReturn = {next_state, StateName, State#state{protocol_buffers =
							Buffers#protocol_buffers{tls_packets = []}}},
    Handle(Packet, FsmReturn);

handle_tls_handshake(Handle, StateName,
		     #state{protocol_buffers =
				#protocol_buffers{tls_packets = [Packet | Packets]} = Buffers} =
			 State0) ->
    FsmReturn = {next_state, StateName, State0#state{protocol_buffers =
							 Buffers#protocol_buffers{tls_packets =
										      Packets}}},
    case Handle(Packet, FsmReturn) of
	{next_state, NextStateName, State, _Timeout} ->
	    handle_tls_handshake(Handle, NextStateName, State);
	{next_state, NextStateName, State} ->
	    handle_tls_handshake(Handle, NextStateName, State);
	{stop, _,_} = Stop ->
	    Stop
    end;

handle_tls_handshake(_Handle, _StateName, #state{}) ->
    throw(?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE)).

write_application_data(Data0, From, 
		       #state{socket = Socket,
			      negotiated_version = Version,
			      transport_cb = Transport,
			      connection_states = ConnectionStates0,
			      send_queue = SendQueue,
			      socket_options = SockOpts,
			      ssl_options = #ssl_options{renegotiate_at = RenegotiateAt}} = State) ->
    Data = encode_packet(Data0, SockOpts),
    
    case time_to_renegotiate(Data, ConnectionStates0, RenegotiateAt) of
	true ->
	    renegotiate(State#state{send_queue = queue:in_r({From, Data}, SendQueue),
				    renegotiation = {true, internal}});
	false ->
	    {Msgs, ConnectionStates} = ssl_record:encode_data(Data, Version, ConnectionStates0),
	    Result = Transport:send(Socket, Msgs),
	    {reply, Result,
	     connection, State#state{connection_states = ConnectionStates}, get_timeout(State)}
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
renegotiate(#state{role = client} = State) ->
    %% Handle same way as if server requested
    %% the renegotiation
    Hs0 = ssl_handshake:init_handshake_history(),
    connection(#hello_request{}, State#state{tls_handshake_history = Hs0});
renegotiate(#state{role = server,
		   socket = Socket,
		   transport_cb = Transport,
		   negotiated_version = Version,
		   connection_states = ConnectionStates0} = State0) ->
    HelloRequest = ssl_handshake:hello_request(),
    Frag = tls_handshake:encode_handshake(HelloRequest, Version),
    Hs0 = ssl_handshake:init_handshake_history(),
    {BinMsg, ConnectionStates} = 
	ssl_record:encode_handshake(Frag, Version, ConnectionStates0),
    Transport:send(Socket, BinMsg),
    {Record, State} = next_record(State0#state{connection_states = 
					       ConnectionStates,
					       tls_handshake_history = Hs0}),
    next_state(connection, hello, Record, State#state{allow_renegotiate = true}).

handle_alerts([], Result) ->
    Result;
handle_alerts(_, {stop, _, _} = Stop) ->
    %% If it is a fatal alert immediately close 
    Stop;
handle_alerts([Alert | Alerts], {next_state, StateName, State, _Timeout}) ->
    handle_alerts(Alerts, handle_alert(Alert, StateName, State)).

handle_alert(#alert{level = ?FATAL} = Alert, StateName,
	     #state{socket = Socket, transport_cb = Transport, 
		    ssl_options = SslOpts, start_or_recv_from = From, host = Host,
		    port = Port, session = Session, user_application = {_Mon, Pid},
		    role = Role, socket_options = Opts, tracker = Tracker} = State) ->
    invalidate_session(Role, Host, Port, Session),
    log_alert(SslOpts#ssl_options.log_alert, StateName, Alert),
    alert_user(Transport, Tracker, Socket, StateName, Opts, Pid, From, Alert, Role),
    {stop, normal, State};

handle_alert(#alert{level = ?WARNING, description = ?CLOSE_NOTIFY} = Alert, 
	     StateName, State) -> 
    handle_normal_shutdown(Alert, StateName, State),
    {stop, {shutdown, peer_close}, State};

handle_alert(#alert{level = ?WARNING, description = ?NO_RENEGOTIATION} = Alert, StateName, 
	     #state{ssl_options = SslOpts, renegotiation = {true, internal}} = State) ->
    log_alert(SslOpts#ssl_options.log_alert, StateName, Alert),
    handle_normal_shutdown(Alert, StateName, State),
    {stop, {shutdown, peer_close}, State};

handle_alert(#alert{level = ?WARNING, description = ?NO_RENEGOTIATION} = Alert, StateName, 
	     #state{ssl_options = SslOpts, renegotiation = {true, From}} = State0) ->
    log_alert(SslOpts#ssl_options.log_alert, StateName, Alert),
    gen_fsm:reply(From, {error, renegotiation_rejected}),
    {Record, State} = next_record(State0),
    next_state(StateName, connection, Record, State);

%% Gracefully log and ignore all other warning alerts
handle_alert(#alert{level = ?WARNING} = Alert, StateName,
	     #state{ssl_options = SslOpts} = State0) ->
    log_alert(SslOpts#ssl_options.log_alert, StateName, Alert),
    {Record, State} = next_record(State0),
    next_state(StateName, StateName, Record, State).

alert_user(Transport, Tracker, Socket, connection, Opts, Pid, From, Alert, Role) ->
    alert_user(Transport, Tracker, Socket, Opts#socket_options.active, Pid, From, Alert, Role);
alert_user(Transport, Tracker, Socket,_, _, _, From, Alert, Role) ->
    alert_user(Transport, Tracker, Socket, From, Alert, Role).

alert_user(Transport, Tracker, Socket, From, Alert, Role) ->
    alert_user(Transport, Tracker, Socket, false, no_pid, From, Alert, Role).

alert_user(_, _, _, false = Active, Pid, From,  Alert, Role) ->
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
    {stop, {shutdown, own_alert}, State}.

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

handle_unexpected_message(Msg, Info, #state{negotiated_version = Version} = State) ->
    Alert =  ?ALERT_REC(?FATAL,?UNEXPECTED_MESSAGE),
    handle_own_alert(Alert, Version, {Info, Msg}, State).


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

%% User downgrades connection
%% When downgrading an TLS connection to a transport connection
%% we must recive the close message before releasing the 
%% transport socket.
close({close, {Pid, Timeout}}, Socket, Transport, ConnectionStates, Check) when is_pid(Pid) -> 
    ssl_socket:setopts(Transport, Socket, [{active, false}, {packet, ssl_tls}]),
    case Transport:recv(Socket, 0, Timeout) of
	{ok, {ssl_tls, Socket, ?ALERT, Version,  Fragment}} ->
	    case tls_record:decode_cipher_text(#ssl_tls{type = ?ALERT,
							version = Version,
							fragment = Fragment
						       }, ConnectionStates, Check) of
		{#ssl_tls{fragment = Plain}, _} ->
		    [Alert| _] = decode_alerts(Plain),
		    downgrade(Alert, Transport, Socket, Pid)
	    end;
	{error, timeout} ->
	    {error, timeout};
	_ ->
	    {error, no_tls_close}
    end;
%% User closes or recursive call!
close({close, Timeout}, Socket, Transport = gen_tcp, _,_) ->
    ssl_socket:setopts(Transport, Socket, [{active, false}]),
    Transport:shutdown(Socket, write),
    _ = Transport:recv(Socket, 0, Timeout),
    ok;
%% Peer closed socket
close({shutdown, transport_closed}, Socket, Transport = gen_tcp, ConnectionStates, Check) ->
    close({close, 0}, Socket, Transport, ConnectionStates, Check);
%% We generate fatal alert
close({shutdown, own_alert}, Socket, Transport = gen_tcp, ConnectionStates, Check) ->
    %% Standard trick to try to make sure all
    %% data sent to the tcp port is really delivered to the
    %% peer application before tcp port is closed so that the peer will
    %% get the correct TLS alert message and not only a transport close.
    %% Will return when other side has closed or after timout millisec
    %% e.g. we do not want to hang if something goes wrong
    %% with the network but we want to maximise the odds that
    %% peer application gets all data sent on the tcp connection.
    close({close, ?DEFAULT_TIMEOUT}, Socket, Transport, ConnectionStates, Check);
%% Other
close(_, Socket, Transport, _,_) -> 
    Transport:close(Socket).
downgrade(#alert{description = ?CLOSE_NOTIFY}, Transport, Socket, Pid) ->
    ssl_socket:setopts(Transport, Socket, [{active, false}, {packet, 0}, {mode, binary}]),
    Transport:controlling_process(Socket, Pid),
    {ok, Socket};
downgrade(_, _,_,_) ->
    {error, no_tls_close}.
	       
convert_state(#state{ssl_options = Options} = State, up, "5.3.5", "5.3.6") ->
    State#state{ssl_options = convert_options_partial_chain(Options, up)};
convert_state(#state{ssl_options = Options} = State, down, "5.3.6", "5.3.5") ->
    State#state{ssl_options = convert_options_partial_chain(Options, down)}.

convert_options_partial_chain(Options, up) ->
    {Head, Tail} = lists:split(5, tuple_to_list(Options)),
    list_to_tuple(Head ++ [{partial_chain, fun(_) -> unknown_ca end}] ++ Tail);
convert_options_partial_chain(Options, down) ->
    list_to_tuple(proplists:delete(partial_chain, tuple_to_list(Options))).

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

