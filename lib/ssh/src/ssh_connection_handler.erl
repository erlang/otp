%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2013. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% Purpose: Handles the setup of an ssh connection, e.i. both the
%% setup SSH Transport Layer Protocol (RFC 4253) and Authentication
%% Protocol (RFC 4252). Details of the different protocols are
%% implemented in ssh_transport.erl, ssh_auth.erl
%% ----------------------------------------------------------------------

-module(ssh_connection_handler).

-behaviour(gen_fsm).

-include("ssh.hrl").
-include("ssh_transport.hrl").
-include("ssh_auth.hrl").
-include("ssh_connect.hrl").

-export([start_link/4, send/2, renegotiate/1, send_event/2,
	 connection_info/3,
	 peer_address/1,
	 renegotiate_data/1]).

%% gen_fsm callbacks
-export([hello/2, kexinit/2, key_exchange/2, new_keys/2,
	 userauth/2, connected/2]).

-export([init/1, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% spawn export
-export([ssh_info_handler/3]).

-record(state, {
	  transport_protocol,      % ex: tcp
	  transport_cb,
	  transport_close_tag,
	  ssh_params,              %  #ssh{} - from ssh.hrl
	  socket,                  %  socket()
	  decoded_data_buffer,     %  binary()
	  encoded_data_buffer,     %  binary()
	  undecoded_packet_length, %  integer()
	  key_exchange_init_msg,   %  #ssh_msg_kexinit{}
	  renegotiate = false,     %  boolean() 
	  manager,                  % pid()
	  connection_queue,
	  address,
	  port,
	  opts
	 }). 

-define(DBG_MESSAGE, true).

%%====================================================================
%% Internal application API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> ok,Pid} | ignore | {error,Error}
%% Description:Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this function
%% does not return until Module:init/1 has returned.  
%%--------------------------------------------------------------------
start_link(Role, Manager, Socket, Options) ->
    gen_fsm:start_link(?MODULE, [Role, Manager, Socket, Options], []).

send(ConnectionHandler, Data) ->
    send_all_state_event(ConnectionHandler, {send, Data}).

renegotiate(ConnectionHandler) ->
    send_all_state_event(ConnectionHandler, renegotiate).
 
renegotiate_data(ConnectionHandler) ->
    send_all_state_event(ConnectionHandler, data_size).
connection_info(ConnectionHandler, From, Options) ->
     send_all_state_event(ConnectionHandler, {info, From, Options}).

%% Replaced with option to connection_info/3. For now keep 
%% for backwards compatibility
peer_address(ConnectionHandler) ->
    sync_send_all_state_event(ConnectionHandler, peer_address).

%%====================================================================
%% gen_fsm callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, StateName, State} |
%%                         {ok, StateName, State, Timeout} |
%%                         ignore                              |
%%                         {stop, StopReason}                   
%% Description:Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/3,4, this function is called by the new process to 
%% initialize. 
%%--------------------------------------------------------------------
init([Role, Manager, Socket, SshOpts]) ->
    process_flag(trap_exit, true),
    {NumVsn, StrVsn} = ssh_transport:versions(Role, SshOpts),
    ssh_bits:install_messages(ssh_transport:transport_messages(NumVsn)),
    {Protocol, Callback, CloseTag} = 
	proplists:get_value(transport, SshOpts, {tcp, gen_tcp, tcp_closed}),
    try init_ssh(Role, NumVsn, StrVsn, SshOpts, Socket) of
	Ssh ->
	    {ok, hello, #state{ssh_params =
				   Ssh#ssh{send_sequence = 0, recv_sequence = 0},
			       socket = Socket,
			       decoded_data_buffer = <<>>,
			       encoded_data_buffer = <<>>,
			       transport_protocol = Protocol,
			       transport_cb = Callback,
			       transport_close_tag = CloseTag,
			       manager = Manager,
			       opts = SshOpts
			      }}
    catch
	exit:Reason ->
	    {stop, {shutdown, Reason}}
    end.
%%--------------------------------------------------------------------
%% Function: 
%% state_name(Event, State) -> {next_state, NextStateName, NextState}|
%%                             {next_state, NextStateName, 
%%                                NextState, Timeout} |
%%                             {stop, Reason, NewState}
%% Description:There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same name as
%% the current state name StateName is called to handle the event. It is also 
%% called if a timeout occurs. 
%%--------------------------------------------------------------------
hello(socket_control, #state{socket = Socket, ssh_params = Ssh} = State) ->
    VsnMsg = ssh_transport:hello_version_msg(string_version(Ssh)),
    send_msg(VsnMsg, State),
    inet:setopts(Socket, [{packet, line}]),
    {next_state, hello, next_packet(State)};

hello({info_line, _Line}, State) ->
    {next_state, hello, next_packet(State)};

hello({version_exchange, Version}, #state{ssh_params = Ssh0,
					  socket = Socket} = State) ->
    {NumVsn, StrVsn} = ssh_transport:handle_hello_version(Version),
    case handle_version(NumVsn, StrVsn, Ssh0) of
	{ok, Ssh1} ->
	    inet:setopts(Socket, [{packet,0}, {mode,binary}]),
	    {KeyInitMsg, SshPacket, Ssh} = ssh_transport:key_exchange_init_msg(Ssh1),
	    send_msg(SshPacket, State),
	    {next_state, kexinit, next_packet(State#state{ssh_params = Ssh,
							  key_exchange_init_msg = 
							  KeyInitMsg})};
	not_supported ->
	   DisconnectMsg =
		#ssh_msg_disconnect{code = 
				    ?SSH_DISCONNECT_PROTOCOL_VERSION_NOT_SUPPORTED,
				    description = "Protocol version " ++  StrVsn 
				    ++ " not supported",
				    language = "en"},
	    handle_disconnect(DisconnectMsg, State)
    end.

kexinit({#ssh_msg_kexinit{} = Kex, Payload},
	#state{ssh_params = #ssh{role = Role} = Ssh0,
					 key_exchange_init_msg = OwnKex} = 
	State) ->
    Ssh1 = ssh_transport:key_init(opposite_role(Role), Ssh0, Payload), 
    try ssh_transport:handle_kexinit_msg(Kex, OwnKex, Ssh1) of
	{ok, NextKexMsg, Ssh} when Role == client ->
	    send_msg(NextKexMsg, State),
	    {next_state, key_exchange, 
	     next_packet(State#state{ssh_params = Ssh})};
	{ok, Ssh} when Role == server ->
	    {next_state, key_exchange, 
	     next_packet(State#state{ssh_params = Ssh})}
    catch
	#ssh_msg_disconnect{} = DisconnectMsg ->
	    handle_disconnect(DisconnectMsg, State);
	_:Error ->
	    Desc = log_error(Error),
	    handle_disconnect(#ssh_msg_disconnect{code = ?SSH_DISCONNECT_KEY_EXCHANGE_FAILED,
						  description = Desc,
						  language = "en"}, State)
    end.
    
key_exchange(#ssh_msg_kexdh_init{} = Msg, 
	     #state{ssh_params = #ssh{role = server} =Ssh0} = State) ->
    try ssh_transport:handle_kexdh_init(Msg, Ssh0) of
	{ok, KexdhReply, Ssh1} ->
	    send_msg(KexdhReply, State),
	    {ok, NewKeys, Ssh} = ssh_transport:new_keys_message(Ssh1),
	    send_msg(NewKeys, State),
	    {next_state, new_keys, next_packet(State#state{ssh_params = Ssh})}
    catch
	#ssh_msg_disconnect{} = DisconnectMsg ->
	    handle_disconnect(DisconnectMsg, State);
	_:Error ->
	    Desc = log_error(Error),
	    handle_disconnect(#ssh_msg_disconnect{code = ?SSH_DISCONNECT_KEY_EXCHANGE_FAILED,
						  description = Desc,
						  language = "en"}, State)
    end;

key_exchange({#ssh_msg_kexinit{} = Kex, Payload},
	#state{ssh_params = #ssh{role = Role} = Ssh0,
					 key_exchange_init_msg = OwnKex} =
	State) ->
    Ssh1 = ssh_transport:key_init(opposite_role(Role), Ssh0, Payload),
    try ssh_transport:handle_kexinit_msg(Kex, OwnKex, Ssh1) of
	{ok, NextKexMsg, Ssh} when Role == client ->
	    send_msg(NextKexMsg, State),
	    {next_state, key_exchange,
	     next_packet(State#state{ssh_params = Ssh})};
	{ok, Ssh} when Role == server ->
	    {next_state, key_exchange,
	     next_packet(State#state{ssh_params = Ssh})}
    catch
	#ssh_msg_disconnect{} = DisconnectMsg ->
	    handle_disconnect(DisconnectMsg, State);
	_:Error ->
	    Desc = log_error(Error),
	    handle_disconnect(#ssh_msg_disconnect{code = ?SSH_DISCONNECT_KEY_EXCHANGE_FAILED,
						  description = Desc,
						  language = "en"}, State)
    end;
  
key_exchange(#ssh_msg_kexdh_reply{} = Msg, 
	     #state{ssh_params = #ssh{role = client} = Ssh0} = State) -> 
    try ssh_transport:handle_kexdh_reply(Msg, Ssh0) of
	{ok, NewKeys, Ssh} -> 
	    send_msg(NewKeys, State),
	    {next_state, new_keys, next_packet(State#state{ssh_params = Ssh})}
    catch
	#ssh_msg_disconnect{} = DisconnectMsg ->
	    handle_disconnect(DisconnectMsg, State);
	{ErrorToDisplay, #ssh_msg_disconnect{} = DisconnectMsg} ->
	    handle_disconnect(DisconnectMsg, State, ErrorToDisplay);
	_:Error ->
	    Desc = log_error(Error),
	    handle_disconnect(#ssh_msg_disconnect{code = ?SSH_DISCONNECT_KEY_EXCHANGE_FAILED,
							  description = Desc,
							  language = "en"}, State)
    end;

key_exchange(#ssh_msg_kex_dh_gex_group{} = Msg, 
	     #state{ssh_params = #ssh{role = server} = Ssh0} = State) ->
    try ssh_transport:handle_kex_dh_gex_group(Msg, Ssh0) of 
	{ok, NextKexMsg, Ssh1} ->
	    send_msg(NextKexMsg, State),
	    {ok, NewKeys, Ssh} = ssh_transport:new_keys_message(Ssh1),
	    send_msg(NewKeys, State),
	    {next_state, new_keys, next_packet(State#state{ssh_params = Ssh})}
    catch
	#ssh_msg_disconnect{} = DisconnectMsg ->
	    handle_disconnect(DisconnectMsg, State);
	_:Error ->
	    Desc = log_error(Error),
	    handle_disconnect(#ssh_msg_disconnect{code = ?SSH_DISCONNECT_KEY_EXCHANGE_FAILED,
						  description = Desc,
						  language = "en"}, State)
    end;

key_exchange(#ssh_msg_kex_dh_gex_request{} = Msg, 
	     #state{ssh_params = #ssh{role = client} = Ssh0} = State) ->
    try ssh_transport:handle_kex_dh_gex_request(Msg, Ssh0) of 
	{ok, NextKexMsg, Ssh} ->
	    send_msg(NextKexMsg, State),
	    {next_state, new_keys, next_packet(State#state{ssh_params = Ssh})}
    catch
	#ssh_msg_disconnect{} = DisconnectMsg ->
	    handle_disconnect(DisconnectMsg, State);
	_:Error ->
	    Desc = log_error(Error),
	    handle_disconnect(#ssh_msg_disconnect{code = ?SSH_DISCONNECT_KEY_EXCHANGE_FAILED,
						  description = Desc,
						  language = "en"}, State)
    end;
key_exchange(#ssh_msg_kex_dh_gex_reply{} = Msg, 
	     #state{ssh_params = #ssh{role = client} = Ssh0} = State) ->
    try ssh_transport:handle_kex_dh_gex_reply(Msg, Ssh0) of 
	{ok, NewKeys, Ssh} ->
	    send_msg(NewKeys, State),
	    {next_state, new_keys, next_packet(State#state{ssh_params = Ssh})}
    catch
	#ssh_msg_disconnect{} = DisconnectMsg ->
	    handle_disconnect(DisconnectMsg, State);
	_:Error ->
	    Desc = log_error(Error),
	    handle_disconnect(#ssh_msg_disconnect{code = ?SSH_DISCONNECT_KEY_EXCHANGE_FAILED,
						  description = Desc,
						  language = "en"}, State)
    end.

new_keys(#ssh_msg_newkeys{} = Msg, #state{ssh_params = Ssh0} = State0) ->
    try ssh_transport:handle_new_keys(Msg, Ssh0) of
	{ok, Ssh} ->
	    {NextStateName, State} = 
		after_new_keys(State0#state{ssh_params = Ssh}),
	    {next_state, NextStateName, next_packet(State)}
    catch
	#ssh_msg_disconnect{} = DisconnectMsg ->
	    handle_disconnect(DisconnectMsg, State0);
	_:Error ->
	    Desc = log_error(Error),
	    handle_disconnect(#ssh_msg_disconnect{code = ?SSH_DISCONNECT_KEY_EXCHANGE_FAILED,
						  description = Desc,
						  language = "en"}, State0)
    end.

userauth(#ssh_msg_service_request{name = "ssh-userauth"} = Msg, 
	 #state{ssh_params = #ssh{role = server, 
				  session_id = SessionId} = Ssh0} = State) ->
    ssh_bits:install_messages(ssh_auth:userauth_messages()),
    try ssh_auth:handle_userauth_request(Msg, SessionId, Ssh0) of
	{ok, {Reply, Ssh}} ->
	    send_msg(Reply, State),
	    {next_state, userauth, next_packet(State#state{ssh_params = Ssh})}
    catch 	
	#ssh_msg_disconnect{} = DisconnectMsg ->
	    handle_disconnect(DisconnectMsg, State);
	_:Error ->
	    Desc = log_error(Error),
	    handle_disconnect(#ssh_msg_disconnect{code = ?SSH_DISCONNECT_SERVICE_NOT_AVAILABLE,
						  description = Desc,
						  language = "en"}, State)
    end;

userauth(#ssh_msg_service_accept{name = "ssh-userauth"},  
	 #state{ssh_params = #ssh{role = client,
				  service = "ssh-userauth"} = Ssh0} = 
	 State) ->
    {Msg, Ssh} = ssh_auth:init_userauth_request_msg(Ssh0),
    send_msg(Msg, State),
    {next_state, userauth, next_packet(State#state{ssh_params = Ssh})};

userauth(#ssh_msg_userauth_request{service = "ssh-connection",
                                  method = "none"} = Msg, 
        #state{ssh_params = #ssh{session_id = SessionId, role = server, 
                                 service = "ssh-connection"} = Ssh0
              } = State) -> 
    try ssh_auth:handle_userauth_request(Msg, SessionId, Ssh0) of
       {not_authorized, {_User, _Reason}, {Reply, Ssh}} ->
           send_msg(Reply, State),
           {next_state, userauth, next_packet(State#state{ssh_params = Ssh})}
    catch 
	#ssh_msg_disconnect{} = DisconnectMsg ->
	    handle_disconnect(DisconnectMsg, State);
	_:Error ->
	    Desc = log_error(Error),
	    handle_disconnect(#ssh_msg_disconnect{code = ?SSH_DISCONNECT_SERVICE_NOT_AVAILABLE,
						  description = Desc,
						  language = "en"}, State)
    end;

userauth(#ssh_msg_userauth_request{service = "ssh-connection",
				   method = Method} = Msg, 
	 #state{ssh_params = #ssh{session_id = SessionId, role = server, 
				  service = "ssh-connection",
				  peer = {_, Address}} = Ssh0,
		opts = Opts, manager = Pid} = State) -> 
    try ssh_auth:handle_userauth_request(Msg, SessionId, Ssh0) of
	{authorized, User, {Reply, Ssh}} ->
	    send_msg(Reply, State),
	    ssh_userreg:register_user(User, Pid),
	    Pid ! ssh_connected,
	    connected_fun(User, Address, Method, Opts),
	    {next_state, connected, 
	     next_packet(State#state{ssh_params = Ssh})};
	{not_authorized, {User, Reason}, {Reply, Ssh}} ->
	    retry_fun(User, Reason, Opts),
	    send_msg(Reply, State),
	    {next_state, userauth, next_packet(State#state{ssh_params = Ssh})} 
    catch 	
	#ssh_msg_disconnect{} = DisconnectMsg ->
	    handle_disconnect(DisconnectMsg, State);
	_:Error ->
	    Desc = log_error(Error),
	    handle_disconnect(#ssh_msg_disconnect{code = ?SSH_DISCONNECT_SERVICE_NOT_AVAILABLE,
						  description = Desc,
						  language = "en"}, State)
    end;

userauth(#ssh_msg_userauth_info_request{} = Msg, 
	 #state{ssh_params = #ssh{role = client, 
				  io_cb = IoCb} = Ssh0} = State) ->
    try ssh_auth:handle_userauth_info_request(Msg, IoCb, Ssh0) of
	{ok, {Reply, Ssh}} ->
	    send_msg(Reply, State),
	    {next_state, userauth, next_packet(State#state{ssh_params = Ssh})}
    catch 	
	#ssh_msg_disconnect{} = DisconnectMsg ->
	    handle_disconnect(DisconnectMsg, State);
	_:Error ->
	    Desc = log_error(Error),
	    handle_disconnect(#ssh_msg_disconnect{code = ?SSH_DISCONNECT_SERVICE_NOT_AVAILABLE,
						  description = Desc,
						  language = "en"}, State)
    end;

userauth(#ssh_msg_userauth_info_response{} = Msg, 
	 #state{ssh_params = #ssh{role = server} = Ssh0} = State) ->
    try ssh_auth:handle_userauth_info_response(Msg, Ssh0) of
	{ok, {Reply, Ssh}} ->
	    send_msg(Reply, State),
	    {next_state, userauth, next_packet(State#state{ssh_params = Ssh})}
    catch 	
	#ssh_msg_disconnect{} = DisconnectMsg ->
	    handle_disconnect(DisconnectMsg, State);
	_:Error ->
	    Desc = log_error(Error),
	    handle_disconnect(#ssh_msg_disconnect{code = ?SSH_DISCONNECT_SERVICE_NOT_AVAILABLE,
						  description = Desc,
						  language = "en"}, State)
    end;
			
userauth(#ssh_msg_userauth_success{}, #state{ssh_params = #ssh{role = client} = Ssh,
					     manager = Pid} = State) ->
    Pid ! ssh_connected,
    {next_state, connected, next_packet(State#state{ssh_params = Ssh#ssh{authenticated = true}})};

userauth(#ssh_msg_userauth_failure{},  
	 #state{ssh_params = #ssh{role = client,
				  userauth_methods = []}} 
	 = State) ->
    Msg = #ssh_msg_disconnect{code = 
			      ?SSH_DISCONNECT_NO_MORE_AUTH_METHODS_AVAILABLE,
			      description = "Unable to connect using the available"
			      " authentication methods",
			      language = "en"},
    handle_disconnect(Msg, State); 

%% Server tells us which authentication methods that are allowed
userauth(#ssh_msg_userauth_failure{authentications = Methodes},  
	 #state{ssh_params = #ssh{role = client,
				  userauth_methods = none} = Ssh0} = State) ->
    AuthMethods = string:tokens(Methodes, ","),
    Ssh1 = Ssh0#ssh{userauth_methods = AuthMethods},
    case ssh_auth:userauth_request_msg(Ssh1) of
	{disconnect, DisconnectMsg, {Msg, Ssh}} ->
	    send_msg(Msg, State),
	    handle_disconnect(DisconnectMsg, State#state{ssh_params = Ssh});
	{Msg, Ssh} ->
	    send_msg(Msg, State),
	    {next_state, userauth, next_packet(State#state{ssh_params = Ssh})}
    end;

%% The prefered authentication method failed try next method 
userauth(#ssh_msg_userauth_failure{},  
	 #state{ssh_params = #ssh{role = client} = Ssh0} = State) ->
    case ssh_auth:userauth_request_msg(Ssh0) of
	{disconnect,  DisconnectMsg,{Msg, Ssh}} ->
	    send_msg(Msg, State),
	    handle_disconnect(DisconnectMsg, State#state{ssh_params = Ssh}); 
	{Msg, Ssh} ->	  
	    send_msg(Msg, State),
	    {next_state, userauth, next_packet(State#state{ssh_params = Ssh})}
    end;

userauth(#ssh_msg_userauth_banner{}, 
	 #state{ssh_params = #ssh{userauth_quiet_mode = true, 
				  role = client}} = State) ->
    {next_state, userauth, next_packet(State)};
userauth(#ssh_msg_userauth_banner{message = Msg}, 
	 #state{ssh_params = 
		#ssh{userauth_quiet_mode = false, role = client}} = State) ->
    io:format("~s", [Msg]),
    {next_state, userauth, next_packet(State)}.

connected({#ssh_msg_kexinit{}, _Payload} = Event, State) ->
    kexinit(Event, State#state{renegotiate = true});
connected({#ssh_msg_kexdh_init{}, _Payload} = Event, State) ->
    key_exchange(Event, State#state{renegotiate = true}).

%%--------------------------------------------------------------------
%% Function: 
%% handle_event(Event, StateName, State) -> {next_state, NextStateName, 
%%						  NextState} |
%%                                          {next_state, NextStateName, 
%%					          NextState, Timeout} |
%%                                          {stop, Reason, NewState}
%% Description: Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%--------------------------------------------------------------------
handle_event({send, Data}, StateName, #state{ssh_params = Ssh0} = State) ->
    {Packet, Ssh} = ssh_transport:pack(Data, Ssh0),
    send_msg(Packet, State),
    {next_state, StateName, next_packet(State#state{ssh_params = Ssh})};

handle_event(#ssh_msg_disconnect{} = Msg, _StateName, 
	     #state{manager = Pid} = State) ->
    (catch ssh_connection_manager:event(Pid, Msg)),
    {stop, normal, State};

handle_event(#ssh_msg_ignore{}, StateName, State) ->
    {next_state, StateName, next_packet(State)};

handle_event(#ssh_msg_debug{always_display = true, message = DbgMsg}, 
	     StateName, State) ->
    io:format("DEBUG: ~p\n", [DbgMsg]),
    {next_state, StateName, next_packet(State)};

handle_event(#ssh_msg_debug{}, StateName, State) ->
    {next_state, StateName, next_packet(State)};

handle_event(#ssh_msg_unimplemented{}, StateName, State) ->
    {next_state, StateName, next_packet(State)};

handle_event(renegotiate, connected, #state{ssh_params = Ssh0} 
	     = State) ->
    {KeyInitMsg, SshPacket, Ssh} = ssh_transport:key_exchange_init_msg(Ssh0),
    send_msg(SshPacket, State),
    {next_state, connected, 
     next_packet(State#state{ssh_params = Ssh,
			     key_exchange_init_msg = KeyInitMsg,
			     renegotiate = true})};

handle_event(renegotiate, StateName, State) ->
    %% Allready in keyexcahange so ignore
    {next_state, StateName, State};

handle_event({info, From, Options}, StateName,  #state{ssh_params = Ssh} = State) ->
    spawn(?MODULE, ssh_info_handler, [Options, Ssh, From]), 
    {next_state, StateName, State};
handle_event(data_size, connected, #state{ssh_params = Ssh0} = State) ->
    {ok, [{send_oct,Sent}]} = inet:getstat(State#state.socket, [send_oct]),
    MaxSent = proplists:get_value(rekey_limit, State#state.opts, 1024000000),
    case Sent >= MaxSent of
	true ->
	    {KeyInitMsg, SshPacket, Ssh} = ssh_transport:key_exchange_init_msg(Ssh0),
	    send_msg(SshPacket, State),
	    {next_state, connected, 
	     next_packet(State#state{ssh_params = Ssh,
				     key_exchange_init_msg = KeyInitMsg,
				     renegotiate = true})};
	_ ->
	    {next_state, connected, next_packet(State)}
    end;
handle_event(data_size, StateName, State) ->
    {next_state, StateName, State};
handle_event({unknown, Data}, StateName, State) ->
    Msg = #ssh_msg_unimplemented{sequence = Data},
    send_msg(Msg, State),
    {next_state, StateName, next_packet(State)}.
%%--------------------------------------------------------------------
%% Function: 
%% handle_sync_event(Event, From, StateName, 
%%                   State) -> {next_state, NextStateName, NextState} |
%%                             {next_state, NextStateName, NextState, 
%%                              Timeout} |
%%                             {reply, Reply, NextStateName, NextState}|
%%                             {reply, Reply, NextStateName, NextState, 
%%                              Timeout} |
%%                             {stop, Reason, NewState} |
%%                             {stop, Reason, Reply, NewState}
%% Description: Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/2,3, this function is called to handle
%% the event.
%%--------------------------------------------------------------------

%% Replaced with option to connection_info/3. For now keep 
%% for backwards compatibility
handle_sync_event(peer_address, _From, StateName, 
		  #state{ssh_params = #ssh{peer = {_, Address}}} = State) ->
    {reply, {ok, Address}, StateName, State}.

%%--------------------------------------------------------------------
%% Function: 
%% handle_info(Info,StateName,State)-> {next_state, NextStateName, NextState}|
%%                                     {next_state, NextStateName, NextState, 
%%                                       Timeout} |
%%                                     {stop, Reason, NewState}
%% Description: This function is called by a gen_fsm when it receives any
%% other message than a synchronous or asynchronous event
%% (or a system message).
%%--------------------------------------------------------------------
handle_info({Protocol, Socket, "SSH-" ++ _ = Version}, hello, 
	    #state{socket = Socket,
		   transport_protocol = Protocol} = State ) -> 
    event({version_exchange, Version}, hello, State);

handle_info({Protocol, Socket, Info}, hello, 
	    #state{socket = Socket,
		   transport_protocol = Protocol} = State) -> 
    event({info_line, Info}, hello, State);

handle_info({Protocol, Socket, Data}, Statename, 
	    #state{socket = Socket,
		   transport_protocol = Protocol,
		   ssh_params = #ssh{decrypt_block_size = BlockSize,
				     recv_mac_size = MacSize} = Ssh0,
		   decoded_data_buffer = <<>>,
		   encoded_data_buffer = EncData0} = State0) ->

    %% Implementations SHOULD decrypt the length after receiving the
    %% first 8 (or cipher block size, whichever is larger) bytes of a
    %% packet. (RFC 4253: Section 6 - Binary Packet Protocol)
    case size(EncData0) + size(Data) >= erlang:max(8, BlockSize) of
	true ->
	    {Ssh, SshPacketLen, DecData, EncData} = 

		ssh_transport:decrypt_first_block(<<EncData0/binary, 
						   Data/binary>>, Ssh0),
	     case SshPacketLen > ?SSH_MAX_PACKET_SIZE of
 		true ->
  		    DisconnectMsg = 
  			#ssh_msg_disconnect{code = 
					    ?SSH_DISCONNECT_PROTOCOL_ERROR,
  					    description = "Bad packet length " 
					    ++ integer_to_list(SshPacketLen),
  					    language = "en"},
  		    handle_disconnect(DisconnectMsg, State0);
  		false ->
		    RemainingSshPacketLen = 
			(SshPacketLen + ?SSH_LENGHT_INDICATOR_SIZE) - 
			BlockSize + MacSize,
		    State = State0#state{ssh_params = Ssh},
		    handle_ssh_packet_data(RemainingSshPacketLen, 
					   DecData, EncData, Statename,
					   State)
	     end;
	false  ->
	    {next_state, Statename, 
	     next_packet(State0#state{encoded_data_buffer = 
				      <<EncData0/binary, Data/binary>>})}
    end;

handle_info({Protocol, Socket, Data}, Statename, 
	    #state{socket = Socket,
		   transport_protocol = Protocol,
		   decoded_data_buffer = DecData, 
		   encoded_data_buffer = EncData,
		   undecoded_packet_length = Len} = 
	    State) when is_integer(Len) ->
    handle_ssh_packet_data(Len, DecData, <<EncData/binary, Data/binary>>, 
			   Statename, State);

handle_info({CloseTag, _Socket}, _StateName, 
	    #state{transport_close_tag = CloseTag,
		   ssh_params = #ssh{role = _Role, opts = _Opts}} = State) ->
    DisconnectMsg =
	#ssh_msg_disconnect{code = ?SSH_DISCONNECT_CONNECTION_LOST,
			    description = "Connection Lost",
			    language = "en"},
    {stop, {shutdown, DisconnectMsg}, State};

%%% So that terminate will be run when supervisor is shutdown
handle_info({'EXIT', _Sup, Reason}, _StateName, State) ->
    {stop, Reason, State};

handle_info(UnexpectedMessage, StateName, #state{ssh_params = SshParams} = State) ->
    Msg = lists:flatten(io_lib:format(
           "Unexpected message '~p' received in state '~p'\n"
           "Role: ~p\n"
           "Peer: ~p\n"
           "Local Address: ~p\n", [UnexpectedMessage, StateName,
               SshParams#ssh.role, SshParams#ssh.peer,
               proplists:get_value(address, SshParams#ssh.opts)])),
    error_logger:info_report(Msg),
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, StateName, State) -> void()
%% Description:This function is called by a gen_fsm when it is about
%% to terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%--------------------------------------------------------------------
terminate(normal, _, #state{transport_cb = Transport,
			    socket = Socket,
			    manager = Pid}) ->
    (catch ssh_userreg:delete_user(Pid)),
    (catch Transport:close(Socket)),
    ok;

%% Terminated as manager terminated
terminate(shutdown, StateName, #state{ssh_params = Ssh0} = State) ->
    DisconnectMsg = 
	#ssh_msg_disconnect{code = ?SSH_DISCONNECT_BY_APPLICATION,
			    description = "Application shutdown",
			    language = "en"},
    {SshPacket, Ssh} = ssh_transport:ssh_packet(DisconnectMsg, Ssh0),
    send_msg(SshPacket, State),
    terminate(normal, StateName, State#state{ssh_params = Ssh});

terminate({shutdown, #ssh_msg_disconnect{} = Msg}, StateName, #state{ssh_params = Ssh0, manager = Pid} = State) ->
    {SshPacket, Ssh} = ssh_transport:ssh_packet(Msg, Ssh0),
    send_msg(SshPacket, State),
    ssh_connection_manager:event(Pid, Msg),
    terminate(normal, StateName, State#state{ssh_params = Ssh});
terminate({shutdown, {#ssh_msg_disconnect{} = Msg, ErrorMsg}}, StateName, #state{ssh_params = Ssh0, manager = Pid} = State) ->
    {SshPacket, Ssh} = ssh_transport:ssh_packet(Msg, Ssh0),
    send_msg(SshPacket, State),
    ssh_connection_manager:event(Pid, Msg, ErrorMsg),
    terminate(normal, StateName, State#state{ssh_params = Ssh});
terminate(Reason, StateName, #state{ssh_params = Ssh0, manager = Pid} = State) ->
    log_error(Reason),
    DisconnectMsg = 
	#ssh_msg_disconnect{code = ?SSH_DISCONNECT_BY_APPLICATION,
			    description = "Internal error",
			    language = "en"},
    {SshPacket, Ssh} = ssh_transport:ssh_packet(DisconnectMsg, Ssh0),
    ssh_connection_manager:event(Pid, DisconnectMsg),
    send_msg(SshPacket, State),
    terminate(normal, StateName, State#state{ssh_params = Ssh}).

%%--------------------------------------------------------------------
%% Function:
%% code_change(OldVsn, StateName, State, Extra) -> {ok, StateName, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
init_ssh(client = Role, Vsn, Version, Options, Socket) ->
    IOCb = case proplists:get_value(user_interaction, Options, true) of
	       true -> 
		   ssh_io;
	       false -> 
		   ssh_no_io
	   end,

    AuthMethods = proplists:get_value(auth_methods, Options, 
				      ?SUPPORTED_AUTH_METHODS),
    {ok, PeerAddr} = inet:peername(Socket),
    
    PeerName =  proplists:get_value(host, Options),
    KeyCb =  proplists:get_value(key_cb, Options, ssh_file),

    #ssh{role = Role,
	 c_vsn = Vsn,
	 c_version = Version,
	 key_cb = KeyCb,
	 io_cb = IOCb,
	 userauth_quiet_mode = proplists:get_value(quiet_mode, Options, false),
	 opts = Options,
	 userauth_supported_methods = AuthMethods,
	 peer = {PeerName, PeerAddr},
	 available_host_keys = supported_host_keys(Role, KeyCb, Options)
	};

init_ssh(server = Role, Vsn, Version, Options, Socket) ->

    AuthMethods = proplists:get_value(auth_methods, Options, 
				      ?SUPPORTED_AUTH_METHODS),
    {ok, PeerAddr} = inet:peername(Socket),
    KeyCb =  proplists:get_value(key_cb, Options, ssh_file),

    #ssh{role = Role,
	 s_vsn = Vsn,
	 s_version = Version,
	 key_cb = KeyCb,
	 io_cb = proplists:get_value(io_cb, Options, ssh_io),
	 opts = Options,
	 userauth_supported_methods = AuthMethods,
	 peer = {undefined, PeerAddr},
	 available_host_keys = supported_host_keys(Role, KeyCb, Options)
	 }.

supported_host_keys(client, _, Options) ->
    try
	case extract_algs(proplists:get_value(pref_public_key_algs, Options, false), []) of
	    false ->
		["ssh-rsa", "ssh-dss"];
	    Algs ->
		Algs
	end
    catch
	exit:Reason ->
	    {stop, {shutdown, Reason}}
    end;
supported_host_keys(server, KeyCb, Options) ->
    lists:foldl(fun(Type, Acc) ->
			case available_host_key(KeyCb, Type, Options) of
			    {error, _} ->
				Acc;
			    Alg ->
				[Alg | Acc]
			end
		end, [],
		%% Prefered alg last so no need to reverse
		["ssh-dss", "ssh-rsa"]).
extract_algs(false, _) ->
    false;
extract_algs([],[]) ->
    false;
extract_algs([], NewList) ->
    lists:reverse(NewList);
extract_algs([H|T], NewList) ->
    case H of
	'ssh-dss' ->
	    extract_algs(T, ["ssh-dss"|NewList]);
	'ssh-rsa' ->
	    extract_algs(T, ["ssh-rsa"|NewList])
    end.
available_host_key(KeyCb, "ssh-dss"= Alg, Opts) ->
    case KeyCb:host_key('ssh-dss', Opts) of
	{ok, _} ->
	    Alg;
	Other ->
	    Other
    end;
available_host_key(KeyCb, "ssh-rsa" = Alg, Opts) ->
    case KeyCb:host_key('ssh-rsa', Opts) of
	{ok, _} ->
	    Alg;
	Other ->
	    Other
    end.

send_msg(Msg, #state{socket = Socket, transport_cb = Transport}) ->
    Transport:send(Socket, Msg).

handle_version({2, 0} = NumVsn, StrVsn, Ssh0) -> 
    Ssh = counterpart_versions(NumVsn, StrVsn, Ssh0),
    {ok, Ssh};
handle_version(_,_,_) ->
    not_supported.

string_version(#ssh{role = client, c_version = Vsn}) ->
    Vsn;
string_version(#ssh{role = server, s_version = Vsn}) ->
    Vsn.

send_event(FsmPid, Event) ->
    gen_fsm:send_event(FsmPid, Event).

send_all_state_event(FsmPid, Event) ->
    gen_fsm:send_all_state_event(FsmPid, Event).

sync_send_all_state_event(FsmPid, Event) ->
    gen_fsm:sync_send_all_state_event(FsmPid, Event).

%% simulate send_all_state_event(self(), Event) 
event(#ssh_msg_disconnect{} = Event, StateName, State) ->
    handle_event(Event, StateName, State);
event(#ssh_msg_ignore{} = Event, StateName, State) ->
    handle_event(Event, StateName, State);
event(#ssh_msg_debug{} = Event, StateName, State) ->
    handle_event(Event, StateName, State);
event(#ssh_msg_unimplemented{} = Event, StateName, State) ->
    handle_event(Event, StateName, State);
%% simulate send_event(self(), Event)
event(Event, StateName, State) ->
    ?MODULE:StateName(Event, State).

generate_event(<<?BYTE(Byte), _/binary>> = Msg, StateName,
	       #state{manager = Pid} = State0, EncData) 
  when  Byte == ?SSH_MSG_GLOBAL_REQUEST;
	Byte == ?SSH_MSG_REQUEST_SUCCESS;
	Byte == ?SSH_MSG_REQUEST_FAILURE;
	Byte == ?SSH_MSG_CHANNEL_OPEN;
	Byte == ?SSH_MSG_CHANNEL_OPEN_CONFIRMATION;
	Byte == ?SSH_MSG_CHANNEL_OPEN_FAILURE;
	Byte == ?SSH_MSG_CHANNEL_WINDOW_ADJUST;
	Byte == ?SSH_MSG_CHANNEL_DATA;
	Byte == ?SSH_MSG_CHANNEL_EXTENDED_DATA;
	Byte == ?SSH_MSG_CHANNEL_EOF;
	Byte == ?SSH_MSG_CHANNEL_CLOSE;
	Byte == ?SSH_MSG_CHANNEL_REQUEST;
	Byte == ?SSH_MSG_CHANNEL_SUCCESS;
	Byte == ?SSH_MSG_CHANNEL_FAILURE ->

    try 
	ssh_connection_manager:event(Pid, Msg),
	State = generate_event_new_state(State0, EncData),
	next_packet(State),
	{next_state, StateName, State}
    catch
	exit:{noproc, Reason} ->
	    {stop, {shutdown, Reason}, State0}
    end;
generate_event(Msg, StateName, State0, EncData) ->
    Event = ssh_bits:decode(Msg),
    State = generate_event_new_state(State0, EncData),
    case Event of
	#ssh_msg_kexinit{} ->
	    %% We need payload for verification later.
	    event({Event, Msg}, StateName, State);
	_ ->
	    event(Event, StateName, State)
    end.

generate_event_new_state(#state{ssh_params = 
				#ssh{recv_sequence = SeqNum0} 
				= Ssh} = State, EncData) ->
    SeqNum = ssh_transport:next_seqnum(SeqNum0),
    State#state{ssh_params = Ssh#ssh{recv_sequence = SeqNum},
		decoded_data_buffer = <<>>,
		encoded_data_buffer = EncData, 
		undecoded_packet_length = undefined}.


next_packet(#state{decoded_data_buffer = <<>>,
		   encoded_data_buffer = Buff,
		   ssh_params = #ssh{decrypt_block_size = BlockSize},
		   socket = Socket,
		   transport_protocol = Protocol} = State) when Buff =/= <<>> ->
    case  size(Buff) >= erlang:max(8, BlockSize) of
	true ->
	    %% Enough data from the next packet has been received to
	    %% decode the length indicator, fake a socket-recive
	    %% message so that the data will be processed
	    self() ! {Protocol, Socket, <<>>};
	false ->
	    inet:setopts(Socket, [{active, once}])
    end,
    State;

next_packet(#state{socket = Socket} = State) ->
    inet:setopts(Socket, [{active, once}]),
    State.

after_new_keys(#state{renegotiate = true} = State) ->
    {connected, State#state{renegotiate = false}};
after_new_keys(#state{renegotiate = false, 
		      ssh_params = #ssh{role = client} = Ssh0} = State) ->
    ssh_bits:install_messages(ssh_auth:userauth_messages()),
    {Msg, Ssh} = ssh_auth:service_request_msg(Ssh0),
    send_msg(Msg, State),
    {userauth, State#state{ssh_params = Ssh}};
after_new_keys(#state{renegotiate = false,  
		      ssh_params = #ssh{role = server}} = State) ->
    {userauth, State}.

handle_ssh_packet_data(RemainingSshPacketLen, DecData, EncData, StateName, 
		       State) ->
    EncSize =  size(EncData), 
    case RemainingSshPacketLen > EncSize of
	true ->
	    {next_state, StateName, 
	     next_packet(State#state{decoded_data_buffer = DecData,
				     encoded_data_buffer = EncData,
				     undecoded_packet_length = 
				     RemainingSshPacketLen})};
	false ->
	    handle_ssh_packet(RemainingSshPacketLen, StateName,
			      State#state{decoded_data_buffer = DecData,
					  encoded_data_buffer = EncData})
    
    end.    

handle_ssh_packet(Length, StateName, #state{decoded_data_buffer = DecData0,
					    encoded_data_buffer = EncData0,
					    ssh_params = Ssh0,
					    transport_protocol = _Protocol,
					    socket = _Socket} = State0) ->
    {Ssh1, DecData, EncData, Mac} = 
	ssh_transport:unpack(EncData0, Length, Ssh0),
    SshPacket = <<DecData0/binary, DecData/binary>>,
    case ssh_transport:is_valid_mac(Mac, SshPacket, Ssh1) of
	true ->
	    PacketData = ssh_transport:msg_data(SshPacket),
	    {Ssh1, Msg} = ssh_transport:decompress(Ssh1, PacketData),
	    generate_event(Msg, StateName, 
			   State0#state{ssh_params = Ssh1,
					%% Important to be set for
					%% next_packet
					decoded_data_buffer = <<>>}, EncData);
	false ->
	    DisconnectMsg = 
		#ssh_msg_disconnect{code = ?SSH_DISCONNECT_PROTOCOL_ERROR,
				    description = "Bad mac",
				    language = "en"},
	    handle_disconnect(DisconnectMsg, State0)
    end.

handle_disconnect(#ssh_msg_disconnect{} = Msg, State) ->
    {stop, {shutdown, Msg}, State}.
handle_disconnect(#ssh_msg_disconnect{} = Msg, State, ErrorMsg) ->
    {stop, {shutdown, {Msg, ErrorMsg}}, State}.

counterpart_versions(NumVsn, StrVsn, #ssh{role = server} = Ssh) ->
    Ssh#ssh{c_vsn = NumVsn , c_version = StrVsn};
counterpart_versions(NumVsn, StrVsn, #ssh{role = client} = Ssh) ->
    Ssh#ssh{s_vsn = NumVsn , s_version = StrVsn}.

opposite_role(client) ->
    server;
opposite_role(server) ->
    client.
connected_fun(User, PeerAddr, Method, Opts) ->
    case proplists:get_value(connectfun, Opts) of
	undefined ->
	    ok;
	Fun ->
	    catch Fun(User, PeerAddr, Method)
    end.

retry_fun(_, undefined, _) ->
    ok;

retry_fun(User, {error, Reason}, Opts) ->
    case proplists:get_value(failfun, Opts) of
	undefined ->
	    ok;
	Fun ->
	    catch Fun(User, Reason)
    end;

retry_fun(User, Reason, Opts) ->
    case proplists:get_value(infofun, Opts) of
	undefined ->
	    ok;
	Fun ->
	    catch Fun(User, Reason)
    end.

ssh_info_handler(Options, Ssh, From) ->
    Info = ssh_info(Options, Ssh, []),
    ssh_connection_manager:send_msg({channel_requst_reply, From, Info}).

ssh_info([], _, Acc) ->
    Acc;

ssh_info([client_version | Rest], #ssh{c_vsn = IntVsn,
				       c_version = StringVsn} = SshParams, Acc) ->
    ssh_info(Rest, SshParams, [{client_version, {IntVsn, StringVsn}} | Acc]);

ssh_info([server_version | Rest], #ssh{s_vsn = IntVsn,
				       s_version = StringVsn} = SshParams, Acc) ->
    ssh_info(Rest, SshParams, [{server_version, {IntVsn, StringVsn}} | Acc]);

ssh_info([peer | Rest], #ssh{peer = Peer} = SshParams, Acc) ->
    ssh_info(Rest, SshParams, [{peer, Peer} | Acc]);

ssh_info([ _ | Rest], SshParams, Acc) ->
    ssh_info(Rest, SshParams, Acc).

log_error(Reason) ->
    Report = io_lib:format("Erlang ssh connection handler failed with reason: "
			   "~p ~n, Stacktace: ~p ~n"
			   "please report this to erlang-bugs@erlang.org \n",
			   [Reason,  erlang:get_stacktrace()]),
    error_logger:error_report(Report),
    "Internal error".
