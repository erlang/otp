%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2018. All Rights Reserved.
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
%% Purpose: Handles an ssh connection, e.i. both the
%% setup SSH Transport Layer Protocol (RFC 4253), Authentication
%% Protocol (RFC 4252) and SSH connection Protocol (RFC 4255)
%% Details of the different protocols are
%% implemented in ssh_transport.erl, ssh_auth.erl and ssh_connection.erl
%% ----------------------------------------------------------------------

-module(ssh_connection_handler).

-behaviour(gen_statem).

-include("ssh.hrl").
-include("ssh_transport.hrl").
-include("ssh_auth.hrl").
-include("ssh_connect.hrl").

%%====================================================================
%%% Exports
%%====================================================================

%%% Start and stop
-export([start_link/3,
	 stop/1
	]).

%%% Internal application API
-export([start_connection/4,
         available_hkey_algorithms/2,
	 open_channel/6,
	 request/6, request/7,
	 reply_request/3, 
	 send/5,
	 send_eof/2,
	 info/1, info/2,
	 connection_info/2,
	 channel_info/3,
	 adjust_window/3, close/2,
	 disconnect/4,
	 get_print_info/1
	]).

-type connection_ref() :: ssh:connection_ref().
-type channel_id()     :: ssh:channel_id().

%%% Behaviour callbacks
-export([init/1, callback_mode/0, handle_event/4, terminate/3,
	 format_status/2, code_change/4]).

%%% Exports not intended to be used :). They are used for spawning and tests
-export([init_connection_handler/3,	   % proc_lib:spawn needs this
	 init_ssh_record/3,		   % Export of this internal function
					   % intended for low-level protocol test suites
	 renegotiate/1, alg/1 % Export intended for test cases
	]).

-export([dbg_trace/3]).


-define(send_disconnect(Code, DetailedText, StateName, State),
        send_disconnect(Code, DetailedText, ?MODULE, ?LINE, StateName, State)).

-define(send_disconnect(Code, Reason, DetailedText, StateName, State),
        send_disconnect(Code, Reason, DetailedText, ?MODULE, ?LINE, StateName, State)).

-define(call_disconnectfun_and_log_cond(LogMsg, DetailedText, StateName, D),
        call_disconnectfun_and_log_cond(LogMsg, DetailedText, ?MODULE, ?LINE, StateName, D)).

%%====================================================================
%% Start / stop
%%====================================================================
%%--------------------------------------------------------------------
-spec start_link(role(),
		 gen_tcp:socket(),
                 internal_options()
		) -> {ok, pid()}.
%% . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
start_link(Role, Socket, Options) ->
    {ok, proc_lib:spawn_opt(?MODULE, 
                            init_connection_handler, 
                            [Role, Socket, Options],
                            [link, {message_queue_data,off_heap}]
                           )}.


%%--------------------------------------------------------------------
-spec stop(connection_ref()
	  ) -> ok | {error, term()}.
%% . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
stop(ConnectionHandler)->
    case call(ConnectionHandler, stop) of
       {error, closed} ->
	    ok;
	Other ->
	    Other
    end.

%%====================================================================
%% Internal application API
%%====================================================================

%%--------------------------------------------------------------------
-spec start_connection(role(),
		       gen_tcp:socket(),
                       internal_options(),
		       timeout()
		      ) -> {ok, connection_ref()} | {error, term()}.
%% . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
start_connection(client = Role, Socket, Options, Timeout) ->
    try
	{ok, Pid} = sshc_sup:start_child([Role, Socket, Options]),
	ok = socket_control(Socket, Pid, Options),
	handshake(Pid, erlang:monitor(process,Pid), Timeout)
    catch
	exit:{noproc, _} ->
	    {error, ssh_not_started};
	_:Error ->
	    {error, Error}
    end;

start_connection(server = Role, Socket, Options, Timeout) ->
    try
	case ?GET_OPT(parallel_login, Options) of
	    true ->
		HandshakerPid =
		    spawn_link(fun() ->
				       receive
					   {do_handshake, Pid} ->
					       handshake(Pid, erlang:monitor(process,Pid), Timeout)
				       end
			       end),
		ChildPid = start_the_connection_child(HandshakerPid, Role, Socket, Options),
		HandshakerPid ! {do_handshake, ChildPid};
	    false ->
		ChildPid = start_the_connection_child(self(), Role, Socket, Options),
		handshake(ChildPid, erlang:monitor(process,ChildPid), Timeout)
	end
    catch
	exit:{noproc, _} ->
	    {error, ssh_not_started};
	_:Error ->
	    {error, Error}
    end.

%%--------------------------------------------------------------------
%%% Some other module has decided to disconnect.

-spec disconnect(Code::integer(), Details::iodata(),
                      Module::atom(), Line::integer()) -> no_return().
%% . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

% Preferable called with the macro ?DISCONNECT

disconnect(Code, DetailedText, Module, Line) ->
    throw({keep_state_and_data,
	   [{next_event, internal, {send_disconnect, Code, DetailedText, Module, Line}}]}).

%%--------------------------------------------------------------------
-spec open_channel(connection_ref(), 
		   string(),
		   iodata(),
		   pos_integer(),
		   pos_integer(),
		   timeout()
		  ) -> {open, channel_id()} | {error, term()}.
		   
%% . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
open_channel(ConnectionHandler, 
	     ChannelType, ChannelSpecificData, InitialWindowSize, MaxPacketSize, 
	     Timeout) ->
    call(ConnectionHandler,
	 {open, 
	  self(), 
	  ChannelType, InitialWindowSize, MaxPacketSize, ChannelSpecificData,
	  Timeout}).

%%--------------------------------------------------------------------
-spec request(connection_ref(),
	      pid(),
	      channel_id(),
	      string(),
	      boolean(),
	      iodata(),
	      timeout()
	     ) -> success | failure | ok | {error,timeout}.

-spec request(connection_ref(),
	      channel_id(),
	      string(),
	      boolean(),
	      iodata(),
	      timeout()
	     ) -> success | failure | ok | {error,timeout}.
%% . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
request(ConnectionHandler, ChannelPid, ChannelId, Type, true, Data, Timeout) ->
    call(ConnectionHandler, {request, ChannelPid, ChannelId, Type, Data, Timeout});
request(ConnectionHandler, ChannelPid, ChannelId, Type, false, Data, _) ->
    cast(ConnectionHandler, {request, ChannelPid, ChannelId, Type, Data}).

request(ConnectionHandler, ChannelId, Type, true, Data, Timeout) ->
    call(ConnectionHandler, {request, ChannelId, Type, Data, Timeout});
request(ConnectionHandler, ChannelId, Type, false, Data, _) ->
    cast(ConnectionHandler, {request, ChannelId, Type, Data}).

%%--------------------------------------------------------------------
-spec reply_request(connection_ref(),
		    success | failure,
		    channel_id()
		   ) -> ok.
			   
%% . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
reply_request(ConnectionHandler, Status, ChannelId) ->
    cast(ConnectionHandler, {reply_request, Status, ChannelId}).

%%--------------------------------------------------------------------
-spec send(connection_ref(),
	   channel_id(),
	   non_neg_integer(),
	   iodata(),
	   timeout()
	  ) -> ok | {error, timeout|closed}.
%% . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
send(ConnectionHandler, ChannelId, Type, Data, Timeout) ->
    call(ConnectionHandler, {data, ChannelId, Type, Data, Timeout}).

%%--------------------------------------------------------------------
-spec send_eof(connection_ref(),
	       channel_id()
	      ) -> ok | {error,closed}.
%% . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
send_eof(ConnectionHandler, ChannelId) ->
    call(ConnectionHandler, {eof, ChannelId}).

%%--------------------------------------------------------------------
-spec info(connection_ref()
	  ) -> {ok, [#channel{}]} .

-spec info(connection_ref(),
	   pid() | all
	  ) -> {ok, [#channel{}]} .
%% . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
info(ConnectionHandler) ->
    info(ConnectionHandler, all).

info(ConnectionHandler, ChannelProcess) ->
    call(ConnectionHandler, {info, ChannelProcess}).

%%--------------------------------------------------------------------
-type local_sock_info() :: {inet:ip_address(), non_neg_integer()} | string().
-type peer_sock_info()  :: {inet:ip_address(), non_neg_integer()} | string().
-type state_info() :: iolist().

-spec get_print_info(connection_ref()
		    ) -> {{local_sock_info(), peer_sock_info()},
			  state_info()
			 }.
%% . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
get_print_info(ConnectionHandler) ->
    call(ConnectionHandler, get_print_info, 1000).

%%--------------------------------------------------------------------
connection_info(ConnectionHandler, []) ->
    connection_info(ConnectionHandler, conn_info_keys());
connection_info(ConnectionHandler, Key) when is_atom(Key) ->
    case connection_info(ConnectionHandler, [Key]) of
        [{Key,Val}] -> {Key,Val};
        Other -> Other
    end;
connection_info(ConnectionHandler, Options) ->
    call(ConnectionHandler, {connection_info, Options}).

%%--------------------------------------------------------------------
-spec channel_info(connection_ref(),
		   channel_id(),
		   [atom()]
		  ) -> proplists:proplist().
%% . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
channel_info(ConnectionHandler, ChannelId, Options) ->
    call(ConnectionHandler, {channel_info, ChannelId, Options}).

%%--------------------------------------------------------------------
-spec adjust_window(connection_ref(),
		    channel_id(),
		    integer()
		   ) -> ok.
%% . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
adjust_window(ConnectionHandler, Channel, Bytes) ->
    cast(ConnectionHandler, {adjust_window, Channel, Bytes}).

%%--------------------------------------------------------------------
-spec close(connection_ref(),
	    channel_id()
	   ) -> ok.
%% . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
close(ConnectionHandler, ChannelId) ->
    case call(ConnectionHandler, {close, ChannelId}) of
	ok ->
	    ok;
	{error, closed} ->
	    ok
    end.

%%====================================================================
%% Test support
%%====================================================================
%%--------------------------------------------------------------------
-spec renegotiate(connection_ref()
		 ) -> ok.
%% . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
renegotiate(ConnectionHandler) ->
    cast(ConnectionHandler, force_renegotiate).

%%--------------------------------------------------------------------
alg(ConnectionHandler) ->
    call(ConnectionHandler, get_alg).

%%====================================================================
%% Internal process state
%%====================================================================
-record(data, {
	  starter                               :: pid()
						 | undefined,
	  auth_user                             :: string()
						 | undefined,
	  connection_state                      :: #connection{},
	  latest_channel_id         = 0         :: non_neg_integer()
                                                 | undefined,
	  transport_protocol                    :: atom()
                                                 | undefined,	% ex: tcp
	  transport_cb                          :: atom()
                                                 | undefined,	% ex: gen_tcp
	  transport_close_tag                   :: atom()
                                                 | undefined,	% ex: tcp_closed
	  ssh_params                            :: #ssh{}
                                                 | undefined,
	  socket                                :: gen_tcp:socket()
                                                 | undefined,
	  decrypted_data_buffer     = <<>>      :: binary()
                                                 | undefined,
	  encrypted_data_buffer     = <<>>      :: binary()
                                                 | undefined,
	  aead_data                 = <<>>      :: binary()
                                                 | undefined,
	  undecrypted_packet_length             :: undefined | non_neg_integer(),
	  key_exchange_init_msg                 :: #ssh_msg_kexinit{}
						 | undefined,
	  last_size_rekey           = 0         :: non_neg_integer(),
	  event_queue               = []        :: list(),
	  inet_initial_recbuf_size              :: pos_integer()
						 | undefined
	 }).

%%====================================================================
%% Intitialisation
%%====================================================================
%%--------------------------------------------------------------------
-spec init_connection_handler(role(),
			      gen_tcp:socket(),
			      internal_options()
			     ) -> no_return().
%% . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
init_connection_handler(Role, Socket, Opts) ->
    case init([Role, Socket, Opts]) of
        {ok, StartState, D} ->
            process_flag(trap_exit, true),
            gen_statem:enter_loop(?MODULE,
                                  [], %%[{debug,[trace,log,statistics,debug]} ], %% []
                                  StartState,
                                  D);

        {stop, Error} ->
            D = try
                    %% Only servers have supervisorts defined in Opts
                    Sups = ?GET_INTERNAL_OPT(supervisors, Opts),
                    #connection{system_supervisor =     proplists:get_value(system_sup,     Sups),
                                sub_system_supervisor = proplists:get_value(subsystem_sup,  Sups),
                                connection_supervisor = proplists:get_value(connection_sup, Sups)
                               }
                of
                    C ->
                        #data{connection_state=C}
                catch
                    _:_ ->
                        #data{connection_state=#connection{}}
                end,
            gen_statem:enter_loop(?MODULE,
                                  [],
                                  {init_error,Error},
                                  D#data{socket=Socket})
    end.



init([Role,Socket,Opts]) ->
    case inet:peername(Socket) of
        {ok, PeerAddr} ->
            {Protocol, Callback, CloseTag} = ?GET_OPT(transport, Opts),
            C = #connection{channel_cache = ssh_client_channel:cache_create(),
                            channel_id_seed = 0,
                            requests = [],
                            options = Opts},
            D0 = #data{starter = ?GET_INTERNAL_OPT(user_pid, Opts),
                       connection_state = C,
                       socket = Socket,
                       transport_protocol = Protocol,
                       transport_cb = Callback,
                       transport_close_tag = CloseTag,
                       ssh_params = init_ssh_record(Role, Socket, PeerAddr, Opts)
              },
            D = case Role of
                    client ->
                        D0;
                    server ->
                        Sups = ?GET_INTERNAL_OPT(supervisors, Opts),
                        D0#data{connection_state = 
                                    C#connection{cli_spec = ?GET_OPT(ssh_cli, Opts, {ssh_cli,[?GET_OPT(shell, Opts)]}),
                                                 exec =     ?GET_OPT(exec,    Opts),
                                                 system_supervisor =     proplists:get_value(system_sup,     Sups),
                                                 sub_system_supervisor = proplists:get_value(subsystem_sup,  Sups),
                                                 connection_supervisor = proplists:get_value(connection_sup, Sups)
                                                }}
                end,
            {ok, {hello,Role}, D};
        
        {error,Error} ->
            {stop, Error}
    end.



init_ssh_record(Role, Socket, Opts) ->
    %% Export of this internal function is
    %% intended for low-level protocol test suites
    {ok,PeerAddr} = inet:peername(Socket),
    init_ssh_record(Role, Socket, PeerAddr, Opts).

init_ssh_record(Role, Socket, PeerAddr, Opts) ->
    AuthMethods = ?GET_OPT(auth_methods, Opts),
    S0 = #ssh{role = Role,
	      opts = Opts,
	      userauth_supported_methods = AuthMethods,
	      available_host_keys = available_hkey_algorithms(Role, Opts),
	      random_length_padding = ?GET_OPT(max_random_length_padding, Opts)
	   },

    {Vsn, Version} = ssh_transport:versions(Role, Opts),
    LocalName = case inet:sockname(Socket) of
                    {ok,Local} -> Local;
                    _ -> undefined
                end,
    case Role of
	client ->
	    PeerName = case ?GET_INTERNAL_OPT(host, Opts) of
                           PeerIP when is_tuple(PeerIP) ->
                               inet_parse:ntoa(PeerIP);
                           PeerName0 when is_atom(PeerName0) ->
                               atom_to_list(PeerName0);
                           PeerName0 when is_list(PeerName0) ->
                               PeerName0
                       end,
            S1 =
                S0#ssh{c_vsn = Vsn,
                       c_version = Version,
                       opts = ?PUT_INTERNAL_OPT({io_cb, case ?GET_OPT(user_interaction, Opts) of
                                                            true ->  ssh_io;
                                                            false -> ssh_no_io
                                                        end},
                                                Opts),
                       userauth_quiet_mode = ?GET_OPT(quiet_mode, Opts),
                       peer = {PeerName, PeerAddr},
                       local = LocalName
                      },
            S1#ssh{userauth_pubkeys = [K || K <- ?GET_OPT(pref_public_key_algs, Opts),
                                            is_usable_user_pubkey(K, S1)
                                      ]
                  };

	server ->
	    S0#ssh{s_vsn = Vsn,
		   s_version = Version,
		   userauth_methods = string:tokens(AuthMethods, ","),
		   kb_tries_left = 3,
		   peer = {undefined, PeerAddr},
                   local = LocalName
		  }
    end.



%%====================================================================
%% gen_statem callbacks
%%====================================================================
%%--------------------------------------------------------------------
-type event_content() ::  any().

-type renegotiate_flag() :: init | renegotiate.

-type state_name() :: 
        {hello,                     role()                    }
      | {kexinit,                   role(), renegotiate_flag()}
      | {key_exchange,              role(), renegotiate_flag()}
      | {key_exchange_dh_gex_init,  server, renegotiate_flag()}
      | {key_exchange_dh_gex_reply, client, renegotiate_flag()}
      | {new_keys,                  role(), renegotiate_flag()}
      | {ext_info,                  role(), renegotiate_flag()}
      | {service_request,           role()                    }
      | {userauth,                  role()                    }
      | {userauth_keyboard_interactive,       role()          }
      | {userauth_keyboard_interactive_extra, server          }
      | {userauth_keyboard_interactive_info_response, client  }
      | {connected,                 role()                    }
	.

%% The state names must fulfill some rules regarding
%% where the role() and the renegotiate_flag() is placed:

-spec role(state_name()) -> role().
role({_,Role}) -> Role;
role({_,Role,_}) -> Role.

-spec renegotiation(state_name()) -> boolean().
renegotiation({_,_,ReNeg}) -> ReNeg == renegotiate;
renegotiation(_) -> false.


-define(CONNECTED(StateName), 
        (element(1,StateName) == connected orelse
         element(1,StateName) == ext_info ) ).

-spec handle_event(gen_statem:event_type(),
		   event_content(),
		   state_name(),
		   #data{}
		  ) -> gen_statem:event_handler_result(state_name()) .
		   
-define(CONNECTION_MSG(Msg),
        [{next_event, internal, prepare_next_packet},
         {next_event,internal,{conn_msg,Msg}}]).

%% . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

callback_mode() ->
    [handle_event_function,
     state_enter].


handle_event(_, _Event, {init_error,Error}=StateName, D) ->
    case Error of
        enotconn ->
           %% Handles the abnormal sequence:
           %%    SYN->
           %%            <-SYNACK
           %%    ACK->
           %%    RST->
            ?call_disconnectfun_and_log_cond("Protocol Error",
                                             "TCP connenction to server was prematurely closed by the client",
                                             StateName, D),
            {stop, {shutdown,"TCP connenction to server was prematurely closed by the client"}};

        OtherError ->
            {stop, {shutdown,{init,OtherError}}}
    end;

%%% ######## {hello, client|server} ####
%% The very first event that is sent when the we are set as controlling process of Socket
handle_event(_, socket_control, {hello,_}=StateName, D) ->
    VsnMsg = ssh_transport:hello_version_msg(string_version(D#data.ssh_params)),
    send_bytes(VsnMsg, D),
    case inet:getopts(Socket=D#data.socket, [recbuf]) of
	{ok, [{recbuf,Size}]} ->
	    %% Set the socket to the hello text line handling mode:
	    inet:setopts(Socket, [{packet, line},
				  {active, once},
				  % Expecting the version string which might
				  % be max ?MAX_PROTO_VERSION bytes:
				  {recbuf, ?MAX_PROTO_VERSION},
				  {nodelay,true}]),
	    {keep_state, D#data{inet_initial_recbuf_size=Size}};

	Other ->
            ?call_disconnectfun_and_log_cond("Option return", 
                                             io_lib:format("Unexpected getopts return:~n  ~p",[Other]),
                                             StateName, D),
	    {stop, {shutdown,{unexpected_getopts_return, Other}}}
    end;

handle_event(_, {info_line,Line}, {hello,Role}=StateName, D) ->
    case Role of
	client ->
	    %% The server may send info lines to the client before the version_exchange
	    %% RFC4253/4.2
	    inet:setopts(D#data.socket, [{active, once}]),
	    keep_state_and_data;
	server ->
	    %% But the client may NOT send them to the server. Openssh answers with cleartext,
	    %% and so do we
	    send_bytes("Protocol mismatch.", D),
            Msg = io_lib:format("Protocol mismatch in version exchange. Client sent info lines.~n~s",
                                [ssh_dbg:hex_dump(Line, 64)]),
            ?call_disconnectfun_and_log_cond("Protocol mismatch.", Msg, StateName, D),
	    {stop, {shutdown,"Protocol mismatch in version exchange. Client sent info lines."}}
    end;

handle_event(_, {version_exchange,Version}, {hello,Role}, D0) ->
    {NumVsn, StrVsn} = ssh_transport:handle_hello_version(Version),
    case handle_version(NumVsn, StrVsn, D0#data.ssh_params) of
	{ok, Ssh1} ->
	    %% Since the hello part is finnished correctly, we set the
	    %% socket to the packet handling mode (including recbuf size):
	    inet:setopts(D0#data.socket, [{packet,0},
					 {mode,binary},
					 {active, once},
					 {recbuf, D0#data.inet_initial_recbuf_size}]),
	    {KeyInitMsg, SshPacket, Ssh} = ssh_transport:key_exchange_init_msg(Ssh1),
	    send_bytes(SshPacket, D0),
	    {next_state, {kexinit,Role,init}, D0#data{ssh_params = Ssh,
						     key_exchange_init_msg = KeyInitMsg}};
	not_supported ->
            {Shutdown, D} =  
                ?send_disconnect(?SSH_DISCONNECT_PROTOCOL_VERSION_NOT_SUPPORTED,
                                 io_lib:format("Offending version is ~p",[string:chomp(Version)]),
                                 {hello,Role},
                                 D0),
	    {stop, Shutdown, D}
    end;

		  
%%% ######## {kexinit, client|server, init|renegotiate} ####

handle_event(_, {#ssh_msg_kexinit{}=Kex, Payload}, {kexinit,Role,ReNeg},
	     D = #data{key_exchange_init_msg = OwnKex}) ->
    Ssh1 = ssh_transport:key_init(peer_role(Role), D#data.ssh_params, Payload),
    Ssh = case ssh_transport:handle_kexinit_msg(Kex, OwnKex, Ssh1) of
	      {ok, NextKexMsg, Ssh2} when Role==client ->
		  send_bytes(NextKexMsg, D),
		  Ssh2;
	      {ok, Ssh2} when Role==server ->
		  Ssh2
	  end,
    {next_state, {key_exchange,Role,ReNeg}, D#data{ssh_params=Ssh}};


%%% ######## {key_exchange, client|server, init|renegotiate} ####

%%%---- diffie-hellman
handle_event(_, #ssh_msg_kexdh_init{} = Msg, {key_exchange,server,ReNeg}, D) ->
    {ok, KexdhReply, Ssh1} = ssh_transport:handle_kexdh_init(Msg, D#data.ssh_params),
    send_bytes(KexdhReply, D),
    {ok, NewKeys, Ssh2} = ssh_transport:new_keys_message(Ssh1),
    send_bytes(NewKeys, D),
    {ok, ExtInfo, Ssh} = ssh_transport:ext_info_message(Ssh2),
    send_bytes(ExtInfo, D),
    {next_state, {new_keys,server,ReNeg}, D#data{ssh_params=Ssh}};

handle_event(_, #ssh_msg_kexdh_reply{} = Msg, {key_exchange,client,ReNeg}, D) ->
    {ok, NewKeys, Ssh1} = ssh_transport:handle_kexdh_reply(Msg, D#data.ssh_params),
    send_bytes(NewKeys, D),
    {ok, ExtInfo, Ssh} = ssh_transport:ext_info_message(Ssh1),
    send_bytes(ExtInfo, D),
    {next_state, {new_keys,client,ReNeg}, D#data{ssh_params=Ssh}};

%%%---- diffie-hellman group exchange
handle_event(_, #ssh_msg_kex_dh_gex_request{} = Msg, {key_exchange,server,ReNeg}, D) ->
    {ok, GexGroup, Ssh1} = ssh_transport:handle_kex_dh_gex_request(Msg, D#data.ssh_params),
    send_bytes(GexGroup, D),
    Ssh = ssh_transport:parallell_gen_key(Ssh1),
    {next_state, {key_exchange_dh_gex_init,server,ReNeg}, D#data{ssh_params=Ssh}};

handle_event(_, #ssh_msg_kex_dh_gex_request_old{} = Msg, {key_exchange,server,ReNeg}, D) ->
    {ok, GexGroup, Ssh1} = ssh_transport:handle_kex_dh_gex_request(Msg, D#data.ssh_params),
    send_bytes(GexGroup, D),
    Ssh = ssh_transport:parallell_gen_key(Ssh1),
    {next_state, {key_exchange_dh_gex_init,server,ReNeg}, D#data{ssh_params=Ssh}};

handle_event(_, #ssh_msg_kex_dh_gex_group{} = Msg, {key_exchange,client,ReNeg}, D) ->
    {ok, KexGexInit, Ssh} = ssh_transport:handle_kex_dh_gex_group(Msg, D#data.ssh_params),
    send_bytes(KexGexInit, D),
    {next_state, {key_exchange_dh_gex_reply,client,ReNeg}, D#data{ssh_params=Ssh}};

%%%---- elliptic curve diffie-hellman
handle_event(_, #ssh_msg_kex_ecdh_init{} = Msg, {key_exchange,server,ReNeg}, D) ->
    {ok, KexEcdhReply, Ssh1} = ssh_transport:handle_kex_ecdh_init(Msg, D#data.ssh_params),
    send_bytes(KexEcdhReply, D),
    {ok, NewKeys, Ssh2} = ssh_transport:new_keys_message(Ssh1),
    send_bytes(NewKeys, D),
    {ok, ExtInfo, Ssh} = ssh_transport:ext_info_message(Ssh2),
    send_bytes(ExtInfo, D),
    {next_state, {new_keys,server,ReNeg}, D#data{ssh_params=Ssh}};

handle_event(_, #ssh_msg_kex_ecdh_reply{} = Msg, {key_exchange,client,ReNeg}, D) ->
    {ok, NewKeys, Ssh1} = ssh_transport:handle_kex_ecdh_reply(Msg, D#data.ssh_params),
    send_bytes(NewKeys, D),
    {ok, ExtInfo, Ssh} = ssh_transport:ext_info_message(Ssh1),
    send_bytes(ExtInfo, D),
    {next_state, {new_keys,client,ReNeg}, D#data{ssh_params=Ssh}};


%%% ######## {key_exchange_dh_gex_init, server, init|renegotiate} ####

handle_event(_, #ssh_msg_kex_dh_gex_init{} = Msg, {key_exchange_dh_gex_init,server,ReNeg}, D) ->
    {ok, KexGexReply, Ssh1} =  ssh_transport:handle_kex_dh_gex_init(Msg, D#data.ssh_params),
    send_bytes(KexGexReply, D),
    {ok, NewKeys, Ssh2} = ssh_transport:new_keys_message(Ssh1),
    send_bytes(NewKeys, D),
    {ok, ExtInfo, Ssh} = ssh_transport:ext_info_message(Ssh2),
    send_bytes(ExtInfo, D),
    {next_state, {new_keys,server,ReNeg}, D#data{ssh_params=Ssh}};


%%% ######## {key_exchange_dh_gex_reply, client, init|renegotiate} ####

handle_event(_, #ssh_msg_kex_dh_gex_reply{} = Msg, {key_exchange_dh_gex_reply,client,ReNeg}, D) ->
    {ok, NewKeys, Ssh1} = ssh_transport:handle_kex_dh_gex_reply(Msg, D#data.ssh_params),
    send_bytes(NewKeys, D),
    {ok, ExtInfo, Ssh} = ssh_transport:ext_info_message(Ssh1),
    send_bytes(ExtInfo, D),
    {next_state, {new_keys,client,ReNeg}, D#data{ssh_params=Ssh}};


%%% ######## {new_keys, client|server} ####

%% First key exchange round:
handle_event(_, #ssh_msg_newkeys{} = Msg, {new_keys,client,init}, D) ->
    {ok, Ssh1} = ssh_transport:handle_new_keys(Msg, D#data.ssh_params),
    %% {ok, ExtInfo, Ssh2} = ssh_transport:ext_info_message(Ssh1),
    %% send_bytes(ExtInfo, D),
    {MsgReq, Ssh} = ssh_auth:service_request_msg(Ssh1),
    send_bytes(MsgReq, D),
    {next_state, {ext_info,client,init}, D#data{ssh_params=Ssh}};

handle_event(_, #ssh_msg_newkeys{} = Msg, {new_keys,server,init}, D) ->
    {ok, Ssh} = ssh_transport:handle_new_keys(Msg, D#data.ssh_params),
    %% {ok, ExtInfo, Ssh} = ssh_transport:ext_info_message(Ssh1),
    %% send_bytes(ExtInfo, D),
    {next_state, {ext_info,server,init}, D#data{ssh_params=Ssh}};

%% Subsequent key exchange rounds (renegotiation):
handle_event(_, #ssh_msg_newkeys{} = Msg, {new_keys,Role,renegotiate}, D) ->
    {ok, Ssh} = ssh_transport:handle_new_keys(Msg, D#data.ssh_params),
    %% {ok, ExtInfo, Ssh} = ssh_transport:ext_info_message(Ssh1),
    %% send_bytes(ExtInfo, D),
    {next_state, {ext_info,Role,renegotiate}, D#data{ssh_params=Ssh}};


%%% ######## {ext_info, client|server, init|renegotiate} ####

handle_event(_, #ssh_msg_ext_info{}=Msg, {ext_info,Role,init}, D0) ->
    D = handle_ssh_msg_ext_info(Msg, D0),
    {next_state, {service_request,Role}, D};

handle_event(_, #ssh_msg_ext_info{}=Msg, {ext_info,Role,renegotiate}, D0) ->
    D = handle_ssh_msg_ext_info(Msg, D0),
    {next_state, {connected,Role}, D};

handle_event(_, #ssh_msg_newkeys{}=Msg, {ext_info,_Role,renegotiate}, D) ->
    {ok, Ssh} = ssh_transport:handle_new_keys(Msg, D#data.ssh_params),
    {keep_state, D#data{ssh_params = Ssh}};
    

handle_event(internal, Msg, {ext_info,Role,init}, D) when is_tuple(Msg) ->
    %% If something else arrives, goto next state and handle the event in that one
    {next_state, {service_request,Role}, D, [postpone]};

handle_event(internal, Msg, {ext_info,Role,_ReNegFlag}, D) when is_tuple(Msg) ->
    %% If something else arrives, goto next state and handle the event in that one
    {next_state, {connected,Role}, D, [postpone]};

%%% ######## {service_request, client|server} ####

handle_event(_, Msg = #ssh_msg_service_request{name=ServiceName}, StateName = {service_request,server}, D0) ->
    case ServiceName of
	"ssh-userauth" ->
	    Ssh0 = #ssh{session_id=SessionId} = D0#data.ssh_params,
	    {ok, {Reply, Ssh}} = ssh_auth:handle_userauth_request(Msg, SessionId, Ssh0),
	    send_bytes(Reply, D0),
	    {next_state, {userauth,server}, D0#data{ssh_params = Ssh}};

	_ ->
            {Shutdown, D} =  
                ?send_disconnect(?SSH_DISCONNECT_SERVICE_NOT_AVAILABLE,
                                 io_lib:format("Unknown service: ~p",[ServiceName]),
                                 StateName, D0),
            {stop, Shutdown, D}
    end;

handle_event(_, #ssh_msg_service_accept{name = "ssh-userauth"}, {service_request,client},
	     #data{ssh_params = #ssh{service="ssh-userauth"} = Ssh0} = State) ->
    {Msg, Ssh} = ssh_auth:init_userauth_request_msg(Ssh0),
    send_bytes(Msg, State),
    {next_state, {userauth,client}, State#data{auth_user = Ssh#ssh.user, ssh_params = Ssh}};


%%% ######## {userauth, client|server} ####

%%---- userauth request to server
handle_event(_, 
	     Msg = #ssh_msg_userauth_request{service = ServiceName, method = Method},
	     StateName = {userauth,server},
	     D0 = #data{ssh_params=Ssh0}) ->

    case {ServiceName, Ssh0#ssh.service, Method} of
	{"ssh-connection", "ssh-connection", "none"} ->
	    %% Probably the very first userauth_request but we deny unauthorized login
	    {not_authorized, _, {Reply,Ssh}} =
		ssh_auth:handle_userauth_request(Msg, Ssh0#ssh.session_id, Ssh0),
	    send_bytes(Reply, D0),
	    {keep_state, D0#data{ssh_params = Ssh}};
	
	{"ssh-connection", "ssh-connection", Method} ->
	    %% Userauth request with a method like "password" or so
	    case lists:member(Method, Ssh0#ssh.userauth_methods) of
		true ->
		    %% Yepp! we support this method
		    case ssh_auth:handle_userauth_request(Msg, Ssh0#ssh.session_id, Ssh0) of
			{authorized, User, {Reply, Ssh}} ->
			    send_bytes(Reply, D0),
			    D0#data.starter ! ssh_connected,
			    connected_fun(User, Method, D0),
			    {next_state, {connected,server},
			     D0#data{auth_user = User, 
				    ssh_params = Ssh#ssh{authenticated = true}}};
			{not_authorized, {User, Reason}, {Reply, Ssh}} when Method == "keyboard-interactive" ->
			    retry_fun(User, Reason, D0),
			    send_bytes(Reply, D0),
			    {next_state, {userauth_keyboard_interactive,server}, D0#data{ssh_params = Ssh}};
			{not_authorized, {User, Reason}, {Reply, Ssh}} ->
			    retry_fun(User, Reason, D0),
			    send_bytes(Reply, D0),
			    {keep_state, D0#data{ssh_params = Ssh}}
		    end;
		false ->
		    %% No we do not support this method (=/= none)
		    %% At least one non-erlang client does like this. Retry as the next event
		    {keep_state_and_data,
		     [{next_event, internal, Msg#ssh_msg_userauth_request{method="none"}}]
		    }
	    end;

	%% {"ssh-connection", Expected, Method} when Expected =/= ServiceName -> Do what?
	%% {ServiceName,      Expected, Method} when Expected =/= ServiceName -> Do what?

	{ServiceName, _, _} when ServiceName =/= "ssh-connection" ->
            {Shutdown, D} =  
                ?send_disconnect(?SSH_DISCONNECT_SERVICE_NOT_AVAILABLE,
                                 io_lib:format("Unknown service: ~p",[ServiceName]),
                                 StateName, D0),
            {stop, Shutdown, D}
    end;

%%---- userauth success to client
handle_event(_, #ssh_msg_ext_info{}=Msg, {userauth,client}, D0) ->
    %% FIXME: need new state to receive this msg!
    D = handle_ssh_msg_ext_info(Msg, D0),
    {keep_state, D};

handle_event(_, #ssh_msg_userauth_success{}, {userauth,client}, D=#data{ssh_params = Ssh}) ->
    D#data.starter ! ssh_connected,
    {next_state, {connected,client}, D#data{ssh_params=Ssh#ssh{authenticated = true}}};


%%---- userauth failure response to client
handle_event(_, #ssh_msg_userauth_failure{}, {userauth,client}=StateName,
	     #data{ssh_params = #ssh{userauth_methods = []}} = D0) ->
    {Shutdown, D} =
        ?send_disconnect(?SSH_DISCONNECT_NO_MORE_AUTH_METHODS_AVAILABLE, 
                         io_lib:format("User auth failed for: ~p",[D0#data.auth_user]),
                         StateName, D0),
    {stop, Shutdown, D};
handle_event(_, #ssh_msg_userauth_failure{authentications = Methods}, StateName={userauth,client},
	     D0 = #data{ssh_params = Ssh0}) ->
    %% The prefered authentication method failed try next method
    Ssh1 = case Ssh0#ssh.userauth_methods of
	       none ->
		   %% Server tells us which authentication methods that are allowed
		   Ssh0#ssh{userauth_methods = string:tokens(Methods, ",")};
	       _ ->
		   %% We already know...
		   Ssh0
	   end,
    case ssh_auth:userauth_request_msg(Ssh1) of
        {send_disconnect, Code, Ssh} ->
            {Shutdown, D} =
                ?send_disconnect(Code, 
                                 io_lib:format("User auth failed for: ~p",[D0#data.auth_user]),
                                 StateName, D0#data{ssh_params = Ssh}),
	    {stop, Shutdown, D};
	{"keyboard-interactive", {Msg, Ssh}} ->
	    send_bytes(Msg, D0),
	    {next_state, {userauth_keyboard_interactive,client}, D0#data{ssh_params = Ssh}};
	{_Method, {Msg, Ssh}} ->
	    send_bytes(Msg, D0),
	    {keep_state, D0#data{ssh_params = Ssh}}
    end;

%%---- banner to client
handle_event(_, #ssh_msg_userauth_banner{message = Msg}, {userauth,client}, D) ->
    case D#data.ssh_params#ssh.userauth_quiet_mode of
	false -> io:format("~s", [Msg]);
	true -> ok
    end,
    keep_state_and_data;


%%% ######## {userauth_keyboard_interactive, client|server}

handle_event(_, #ssh_msg_userauth_info_request{} = Msg, {userauth_keyboard_interactive, client},
	     #data{ssh_params = Ssh0} = D) ->
    case ssh_auth:handle_userauth_info_request(Msg, Ssh0) of
	{ok, {Reply, Ssh}} ->
	    send_bytes(Reply, D),
	    {next_state, {userauth_keyboard_interactive_info_response,client}, D#data{ssh_params = Ssh}};
	not_ok ->
	    {next_state, {userauth,client}, D, [postpone]}
    end;

handle_event(_, #ssh_msg_userauth_info_response{} = Msg, {userauth_keyboard_interactive, server}, D) ->
    case ssh_auth:handle_userauth_info_response(Msg, D#data.ssh_params) of
	{authorized, User, {Reply, Ssh}} ->
	    send_bytes(Reply, D),
	    D#data.starter ! ssh_connected,
	    connected_fun(User, "keyboard-interactive", D),
	    {next_state, {connected,server}, D#data{auth_user = User,
						    ssh_params = Ssh#ssh{authenticated = true}}};
	{not_authorized, {User, Reason}, {Reply, Ssh}} ->
	    retry_fun(User, Reason, D),
	    send_bytes(Reply, D),
	    {next_state, {userauth,server}, D#data{ssh_params = Ssh}};

	{authorized_but_one_more, _User,  {Reply, Ssh}} ->
	    send_bytes(Reply, D),
	    {next_state, {userauth_keyboard_interactive_extra,server}, D#data{ssh_params = Ssh}}
    end;

handle_event(_, #ssh_msg_userauth_info_response{} = Msg, {userauth_keyboard_interactive_extra, server}, D) ->
    {authorized, User, {Reply, Ssh}} = ssh_auth:handle_userauth_info_response({extra,Msg}, D#data.ssh_params),
    send_bytes(Reply, D),
    D#data.starter ! ssh_connected,
    connected_fun(User, "keyboard-interactive", D),
    {next_state, {connected,server}, D#data{auth_user = User,
					    ssh_params = Ssh#ssh{authenticated = true}}};

handle_event(_, #ssh_msg_userauth_failure{}, {userauth_keyboard_interactive, client},
	     #data{ssh_params = Ssh0} = D0) ->
    Prefs = [{Method,M,F,A} || {Method,M,F,A} <- Ssh0#ssh.userauth_preference,
			       Method =/= "keyboard-interactive"],
    D = D0#data{ssh_params = Ssh0#ssh{userauth_preference=Prefs}},
    {next_state, {userauth,client}, D, [postpone]};

handle_event(_, #ssh_msg_userauth_failure{}, {userauth_keyboard_interactive_info_response, client},
	     #data{ssh_params = Ssh0} = D0) ->
    Opts = Ssh0#ssh.opts,
    D = case ?GET_OPT(password, Opts) of
	    undefined ->
		D0;
	    _ ->
		D0#data{ssh_params =
			    Ssh0#ssh{opts = ?PUT_OPT({password,not_ok}, Opts)}} % FIXME:intermodule dependency
	end,
    {next_state, {userauth,client}, D, [postpone]};

handle_event(_, #ssh_msg_ext_info{}=Msg, {userauth_keyboard_interactive_info_response, client}, D0) ->
    %% FIXME: need new state to receive this msg!
    D = handle_ssh_msg_ext_info(Msg, D0),
    {keep_state, D};

handle_event(_, #ssh_msg_userauth_success{}, {userauth_keyboard_interactive_info_response, client}, D) ->
    {next_state, {userauth,client}, D, [postpone]};

handle_event(_, #ssh_msg_userauth_info_request{}, {userauth_keyboard_interactive_info_response, client}, D) ->
    {next_state, {userauth_keyboard_interactive,client}, D, [postpone]};


%%% ######## {connected, client|server} ####

%% Skip ext_info messages in connected state (for example from OpenSSH >= 7.7)
handle_event(_, #ssh_msg_ext_info{}, {connected,_Role}, D) ->
    {keep_state, D};

handle_event(_, {#ssh_msg_kexinit{},_}, {connected,Role}, D0) ->
    {KeyInitMsg, SshPacket, Ssh} = ssh_transport:key_exchange_init_msg(D0#data.ssh_params),
    D = D0#data{ssh_params = Ssh,
		key_exchange_init_msg = KeyInitMsg},
    send_bytes(SshPacket, D),
    {next_state, {kexinit,Role,renegotiate}, D, [postpone]};

handle_event(_, #ssh_msg_disconnect{description=Desc} = Msg, StateName, D0) ->
    {disconnect, _, RepliesCon} =
	ssh_connection:handle_msg(Msg, D0#data.connection_state, role(StateName)),
    {Actions,D} = send_replies(RepliesCon, D0),
    disconnect_fun("Received disconnect: "++Desc, D),
    {stop_and_reply, {shutdown,Desc}, Actions, D};

handle_event(_, #ssh_msg_ignore{}, _, _) ->
    keep_state_and_data;

handle_event(_, #ssh_msg_unimplemented{}, _, _) ->
    keep_state_and_data;

handle_event(_, #ssh_msg_debug{} = Msg, _, D) ->
    debug_fun(Msg, D),
    keep_state_and_data;

handle_event(internal, {conn_msg,Msg}, StateName, #data{starter = User,
                                                        connection_state = Connection0,
                                                        event_queue = Qev0} = D0) ->
    Role = role(StateName),
    Rengotation = renegotiation(StateName),
    try ssh_connection:handle_msg(Msg, Connection0, Role) of
	{disconnect, Reason0, RepliesConn} ->
            {Repls, D} = send_replies(RepliesConn, D0),
            case {Reason0,Role} of
                {{_, Reason}, client} when ((StateName =/= {connected,client})
                                            and (not Rengotation)) ->
		   User ! {self(), not_connected, Reason};
                _ ->
                    ok
            end,
            {stop_and_reply, {shutdown,normal}, Repls, D};

	{Replies, Connection} when is_list(Replies) ->
	    {Repls, D} = 
		case StateName of
		    {connected,_} ->
			send_replies(Replies, D0#data{connection_state=Connection});
		    _ ->
			{ConnReplies, NonConnReplies} = lists:splitwith(fun not_connected_filter/1, Replies),
			send_replies(NonConnReplies, D0#data{event_queue = Qev0 ++ ConnReplies})
		end,
            case {Msg, StateName} of
                {#ssh_msg_channel_close{}, {connected,_}} ->
                    {keep_state, D, [cond_set_idle_timer(D)|Repls]};
                {#ssh_msg_channel_success{}, _} ->
                    update_inet_buffers(D#data.socket),
                    {keep_state, D, Repls};
                _ ->
                    {keep_state, D, Repls}
            end

    catch
	Class:Error ->
            {Repls, D1} = send_replies(ssh_connection:handle_stop(Connection0), D0),
            {Shutdown, D} = ?send_disconnect(?SSH_DISCONNECT_BY_APPLICATION,
                                             io_lib:format("Internal error: ~p:~p",[Class,Error]),
                                             StateName, D1),
            {stop_and_reply, Shutdown, Repls, D}
    end;


handle_event(enter, _OldState, {connected,_}=State, D) ->
    %% Entering the state where re-negotiation is possible
    init_renegotiate_timers(State, D);
    
handle_event(enter, _OldState, {ext_info,_,renegotiate}=State, D) ->
    %% Could be hanging in exit_info state if nothing else arrives
    init_renegotiate_timers(State, D);

handle_event(enter, {connected,_}, State, D) ->
    %% Exiting the state where re-negotiation is possible
    pause_renegotiate_timers(State, D);

handle_event(cast, force_renegotiate, StateName, D) ->
    handle_event({timeout,renegotiate}, undefined, StateName, D);

handle_event({timeout,renegotiate}, _, StateName, D0) ->
    case StateName of
        {connected,Role} ->
            start_rekeying(Role, D0);
        {ext_info,Role,renegotiate} ->
            start_rekeying(Role, D0);
        _ ->
            %% Wrong state for starting a renegotiation, must be in re-negotiation
            keep_state_and_data
    end;

handle_event({timeout,check_data_size}, _, StateName, D0) ->
    %% Rekey due to sent data limit reached? (Can't be in {ext_info,...} if data is sent)
    case StateName of
        {connected,Role} ->
            check_data_rekeying(Role, D0);
        _ ->
            %% Wrong state for starting a renegotiation, must be in re-negotiation
            keep_state_and_data
    end;

handle_event({call,From}, get_alg, _, D) ->
    #ssh{algorithms=Algs} = D#data.ssh_params,
    {keep_state_and_data, [{reply,From,Algs}]};

handle_event(cast, _, StateName, _) when not ?CONNECTED(StateName) ->
    {keep_state_and_data, [postpone]};

handle_event(cast, {adjust_window,ChannelId,Bytes}, StateName, D) when ?CONNECTED(StateName) ->
    case ssh_client_channel:cache_lookup(cache(D), ChannelId) of
	#channel{recv_window_size = WinSize,
		 recv_window_pending = Pending,
		 recv_packet_size = PktSize} = Channel
	  when (WinSize-Bytes) >= 2*PktSize ->
	    %% The peer can send at least two more *full* packet, no hurry.
	    ssh_client_channel:cache_update(cache(D),
				     Channel#channel{recv_window_pending = Pending + Bytes}),
	    keep_state_and_data;

	#channel{recv_window_size = WinSize,
		 recv_window_pending = Pending,
		 remote_id = Id} = Channel ->
	    %% Now we have to update the window - we can't receive so many more pkts
	    ssh_client_channel:cache_update(cache(D),
				     Channel#channel{recv_window_size =
							 WinSize + Bytes + Pending,
						     recv_window_pending = 0}),
	    Msg = ssh_connection:channel_adjust_window_msg(Id, Bytes + Pending),
	    {keep_state, send_msg(Msg,D)};

	undefined ->
	    keep_state_and_data
    end;

handle_event(cast, {reply_request,success,ChannelId}, StateName, D) when ?CONNECTED(StateName) ->
    case ssh_client_channel:cache_lookup(cache(D), ChannelId) of
	#channel{remote_id = RemoteId} ->
	    Msg = ssh_connection:channel_success_msg(RemoteId),
	    update_inet_buffers(D#data.socket),
	    {keep_state, send_msg(Msg,D)};

	undefined ->
	    keep_state_and_data
    end;

handle_event(cast, {request,ChannelPid, ChannelId, Type, Data}, StateName, D) when ?CONNECTED(StateName) ->
    {keep_state,  handle_request(ChannelPid, ChannelId, Type, Data, false, none, D)};

handle_event(cast, {request,ChannelId,Type,Data}, StateName, D) when ?CONNECTED(StateName) ->
    {keep_state,  handle_request(ChannelId, Type, Data, false, none, D)};

handle_event(cast, {unknown,Data}, StateName, D) when ?CONNECTED(StateName) ->
    Msg = #ssh_msg_unimplemented{sequence = Data},
    {keep_state, send_msg(Msg,D)};

%%% Previously handle_sync_event began here
handle_event({call,From}, get_print_info, StateName, D) ->
    Reply =
	try
	    {inet:sockname(D#data.socket),
	     inet:peername(D#data.socket)
	    }
	of
	    {{ok,Local}, {ok,Remote}} -> 
		{{Local,Remote},io_lib:format("statename=~p",[StateName])};
	    _ ->
		{{"-",0},"-"}
	catch
	    _:_ ->
		{{"?",0},"?"}
	end,
    {keep_state_and_data, [{reply,From,Reply}]};

handle_event({call,From}, {connection_info, Options}, _, D) ->
    Info = fold_keys(Options, fun conn_info/2, D),
    {keep_state_and_data, [{reply,From,Info}]};

handle_event({call,From}, {channel_info,ChannelId,Options}, _, D) ->
    case ssh_client_channel:cache_lookup(cache(D), ChannelId) of
	#channel{} = Channel ->
	    Info = fold_keys(Options, fun chann_info/2, Channel),
	    {keep_state_and_data, [{reply,From,Info}]};
	undefined ->
	    {keep_state_and_data, [{reply,From,[]}]}
    end;


handle_event({call,From}, {info, all}, _, D) ->
    Result = ssh_client_channel:cache_foldl(fun(Channel, Acc) ->
					     [Channel | Acc]
				     end,
				     [], cache(D)),
    {keep_state_and_data, [{reply, From, {ok,Result}}]};
    
handle_event({call,From}, {info, ChannelPid}, _, D) ->
    Result = ssh_client_channel:cache_foldl(
	       fun(Channel, Acc) when Channel#channel.user == ChannelPid ->
		       [Channel | Acc];
		  (_, Acc) ->
		       Acc
	       end, [], cache(D)),
    {keep_state_and_data, [{reply, From, {ok,Result}}]};

handle_event({call,From}, stop, _StateName, D0) ->
    {Repls,D} = send_replies(ssh_connection:handle_stop(D0#data.connection_state), D0),
    {stop_and_reply, normal, [{reply,From,ok}|Repls], D};

handle_event({call,_}, _, StateName, _) when not ?CONNECTED(StateName) ->
    {keep_state_and_data, [postpone]};

handle_event({call,From}, {request, ChannelPid, ChannelId, Type, Data, Timeout}, StateName, D0) 
  when ?CONNECTED(StateName) ->
    case handle_request(ChannelPid, ChannelId, Type, Data, true, From, D0) of
        {error,Error} ->
            {keep_state, D0, {reply,From,{error,Error}}};
        D ->
            %% Note reply to channel will happen later when reply is recived from peer on the socket
            start_channel_request_timer(ChannelId, From, Timeout),
            {keep_state, D, cond_set_idle_timer(D)}
    end;

handle_event({call,From}, {request, ChannelId, Type, Data, Timeout}, StateName, D0) 
  when ?CONNECTED(StateName) ->
    case handle_request(ChannelId, Type, Data, true, From, D0) of
        {error,Error} ->
            {keep_state, D0, {reply,From,{error,Error}}};
        D ->
            %% Note reply to channel will happen later when reply is recived from peer on the socket
            start_channel_request_timer(ChannelId, From, Timeout),
            {keep_state, D, cond_set_idle_timer(D)}
    end;

handle_event({call,From}, {data, ChannelId, Type, Data, Timeout}, StateName, D0) 
  when ?CONNECTED(StateName) ->
    {Repls,D} = send_replies(ssh_connection:channel_data(ChannelId, Type, Data, D0#data.connection_state, From),
                             D0),
    start_channel_request_timer(ChannelId, From, Timeout), % FIXME: No message exchange so why?
    {keep_state, D, Repls};

handle_event({call,From}, {eof, ChannelId}, StateName, D0) 
  when ?CONNECTED(StateName) ->
    case ssh_client_channel:cache_lookup(cache(D0), ChannelId) of
	#channel{remote_id = Id, sent_close = false} ->
	    D = send_msg(ssh_connection:channel_eof_msg(Id), D0),
	    {keep_state, D, [{reply,From,ok}]};
	_ ->
	    {keep_state, D0, [{reply,From,{error,closed}}]}
    end;

handle_event({call,From},
	     {open, ChannelPid, Type, InitialWindowSize, MaxPacketSize, Data, Timeout},
	     StateName,
	     D0) when ?CONNECTED(StateName) ->
    erlang:monitor(process, ChannelPid),
    {ChannelId, D1} = new_channel_id(D0),
    D2 = send_msg(ssh_connection:channel_open_msg(Type, ChannelId,
						  InitialWindowSize,
						  MaxPacketSize, Data),
		  D1),
    ssh_client_channel:cache_update(cache(D2), 
			     #channel{type = Type,
				      sys = "none",
				      user = ChannelPid,
				      local_id = ChannelId,
				      recv_window_size = InitialWindowSize,
				      recv_packet_size = MaxPacketSize,
				      send_buf = queue:new()
				     }),
    D = add_request(true, ChannelId, From, D2),
    start_channel_request_timer(ChannelId, From, Timeout),
    {keep_state, D, cond_set_idle_timer(D)};

handle_event({call,From}, {send_window, ChannelId}, StateName, D) 
  when ?CONNECTED(StateName) ->
    Reply = case ssh_client_channel:cache_lookup(cache(D), ChannelId) of
		#channel{send_window_size = WinSize,
			 send_packet_size = Packsize} ->
		    {ok, {WinSize, Packsize}};
		undefined ->
		    {error, einval}
	    end,
    {keep_state_and_data, [{reply,From,Reply}]};

handle_event({call,From}, {recv_window, ChannelId}, StateName, D) 
  when ?CONNECTED(StateName) ->
    Reply = case ssh_client_channel:cache_lookup(cache(D), ChannelId) of
		#channel{recv_window_size = WinSize,
			 recv_packet_size = Packsize} ->
		    {ok, {WinSize, Packsize}};
		undefined ->
		    {error, einval}
	    end,
    {keep_state_and_data, [{reply,From,Reply}]};

handle_event({call,From}, {close, ChannelId}, StateName, D0) 
  when ?CONNECTED(StateName) ->
    case ssh_client_channel:cache_lookup(cache(D0), ChannelId) of
	#channel{remote_id = Id} = Channel ->
	    D1 = send_msg(ssh_connection:channel_close_msg(Id), D0),
	    ssh_client_channel:cache_update(cache(D1), Channel#channel{sent_close = true}),
	    {keep_state, D1, [cond_set_idle_timer(D1), {reply,From,ok}]};
	undefined ->
	    {keep_state_and_data, [{reply,From,ok}]}
    end;


%%===== Reception of encrypted bytes, decryption and framing
handle_event(info, {Proto, Sock, Info}, {hello,_}, #data{socket = Sock,
							 transport_protocol = Proto}) ->
    case Info of
	"SSH-" ++ _ ->  
	    {keep_state_and_data, [{next_event, internal, {version_exchange,Info}}]};
	_ ->
	    {keep_state_and_data, [{next_event, internal, {info_line,Info}}]}
    end;


handle_event(info, {Proto, Sock, NewData}, StateName, D0 = #data{socket = Sock,
								 transport_protocol = Proto}) ->
    try ssh_transport:handle_packet_part(
	  D0#data.decrypted_data_buffer,
	  <<(D0#data.encrypted_data_buffer)/binary, NewData/binary>>,
          D0#data.aead_data,
          D0#data.undecrypted_packet_length,
	  D0#data.ssh_params)
    of
	{packet_decrypted, DecryptedBytes, EncryptedDataRest, Ssh1} ->
	    D1 = D0#data{ssh_params =
			    Ssh1#ssh{recv_sequence = ssh_transport:next_seqnum(Ssh1#ssh.recv_sequence)},
			decrypted_data_buffer = <<>>,
                        undecrypted_packet_length = undefined,
                        aead_data = <<>>,
			encrypted_data_buffer = EncryptedDataRest},
	    try
		ssh_message:decode(set_kex_overload_prefix(DecryptedBytes,D1))
	    of
		#ssh_msg_kexinit{} = Msg ->
		    {keep_state, D1, [{next_event, internal, prepare_next_packet},
				     {next_event, internal, {Msg,DecryptedBytes}}
				    ]};

                #ssh_msg_global_request{}            = Msg -> {keep_state, D1, ?CONNECTION_MSG(Msg)};
                #ssh_msg_request_success{}           = Msg -> {keep_state, D1, ?CONNECTION_MSG(Msg)};
                #ssh_msg_request_failure{}           = Msg -> {keep_state, D1, ?CONNECTION_MSG(Msg)};
                #ssh_msg_channel_open{}              = Msg -> {keep_state, D1, ?CONNECTION_MSG(Msg)};
                #ssh_msg_channel_open_confirmation{} = Msg -> {keep_state, D1, ?CONNECTION_MSG(Msg)};
                #ssh_msg_channel_open_failure{}      = Msg -> {keep_state, D1, ?CONNECTION_MSG(Msg)};
                #ssh_msg_channel_window_adjust{}     = Msg -> {keep_state, D1, ?CONNECTION_MSG(Msg)};
                #ssh_msg_channel_data{}              = Msg -> {keep_state, D1, ?CONNECTION_MSG(Msg)};
                #ssh_msg_channel_extended_data{}     = Msg -> {keep_state, D1, ?CONNECTION_MSG(Msg)};
                #ssh_msg_channel_eof{}               = Msg -> {keep_state, D1, ?CONNECTION_MSG(Msg)};
                #ssh_msg_channel_close{}             = Msg -> {keep_state, D1, ?CONNECTION_MSG(Msg)};
                #ssh_msg_channel_request{}           = Msg -> {keep_state, D1, ?CONNECTION_MSG(Msg)};
                #ssh_msg_channel_failure{}           = Msg -> {keep_state, D1, ?CONNECTION_MSG(Msg)};
                #ssh_msg_channel_success{}           = Msg -> {keep_state, D1, ?CONNECTION_MSG(Msg)};

		Msg ->
		    {keep_state, D1, [{next_event, internal, prepare_next_packet},
                                      {next_event, internal, Msg}
				    ]}
	    catch
		C:E:ST  ->
                    {Shutdown, D} =  
                        ?send_disconnect(?SSH_DISCONNECT_PROTOCOL_ERROR,
                                         io_lib:format("Bad packet: Decrypted, but can't decode~n~p:~p~n~p",
                                                       [C,E,ST]),
                                         StateName, D1),
                    {stop, Shutdown, D}
	    end;

	{get_more, DecryptedBytes, EncryptedDataRest, AeadData, RemainingSshPacketLen, Ssh1} ->
	    %% Here we know that there are not enough bytes in
	    %% EncryptedDataRest to use. We must wait for more.
	    inet:setopts(Sock, [{active, once}]),
	    {keep_state, D0#data{encrypted_data_buffer = EncryptedDataRest,
				 decrypted_data_buffer = DecryptedBytes,
                                 undecrypted_packet_length = RemainingSshPacketLen,
                                 aead_data = AeadData,
				 ssh_params = Ssh1}};

	{bad_mac, Ssh1} ->
            {Shutdown, D} =  
                ?send_disconnect(?SSH_DISCONNECT_PROTOCOL_ERROR,
                                 "Bad packet: bad mac",
                                 StateName, D0#data{ssh_params=Ssh1}),
            {stop, Shutdown, D};

	{error, {exceeds_max_size,PacketLen}} ->
            {Shutdown, D} =  
                ?send_disconnect(?SSH_DISCONNECT_PROTOCOL_ERROR,
                                 io_lib:format("Bad packet: Size (~p bytes) exceeds max size",
                                               [PacketLen]),
                                 StateName, D0),
            {stop, Shutdown, D}
    catch
	C:E:ST ->
            {Shutdown, D} =  
                ?send_disconnect(?SSH_DISCONNECT_PROTOCOL_ERROR,
                                 io_lib:format("Bad packet: Couldn't decrypt~n~p:~p~n~p",[C,E,ST]),
                                 StateName, D0),
            {stop, Shutdown, D}
    end;


%%%==== 
handle_event(internal, prepare_next_packet, _, D) ->
    Enough =  erlang:max(8, D#data.ssh_params#ssh.decrypt_block_size),
    case size(D#data.encrypted_data_buffer) of
	Sz when Sz >= Enough ->
	    self() ! {D#data.transport_protocol, D#data.socket, <<>>};
	_ ->
	    ok
    end,
    inet:setopts(D#data.socket, [{active, once}]),
    keep_state_and_data;

handle_event(info, {CloseTag,Socket}, _StateName,
	     D0 = #data{socket = Socket,
                        transport_close_tag = CloseTag,
                        connection_state = C0}) ->
    {Repls, D} = send_replies(ssh_connection:handle_stop(C0), D0),
    disconnect_fun("Received a transport close", D),
    {stop_and_reply, {shutdown,"Connection closed"}, Repls, D};

handle_event(info, {timeout, {_, From} = Request}, _,
	     #data{connection_state = #connection{requests = Requests} = C0} = D) ->
    case lists:member(Request, Requests) of
	true ->
	    %% A channel request is not answered in time. Answer {error,timeout}
	    %% to the caller
	    C = C0#connection{requests = lists:delete(Request, Requests)},
	    {keep_state, D#data{connection_state=C}, [{reply,From,{error,timeout}}]};
	false ->
	    %% The request is answered - just ignore the timeout
	    keep_state_and_data
    end;

%%% Handle that ssh channels user process goes down
handle_event(info, {'DOWN', _Ref, process, ChannelPid, _Reason}, _, D) ->
    Cache = cache(D),
    ssh_client_channel:cache_foldl(
      fun(#channel{user=U,
                   local_id=Id}, Acc) when U == ChannelPid ->
              ssh_client_channel:cache_delete(Cache, Id),
              Acc;
         (_,Acc) ->
              Acc
      end, [], Cache),
    {keep_state, D, cond_set_idle_timer(D)};

handle_event({timeout,idle_time}, _Data,  _StateName, _D) ->
    {stop, {shutdown, "Timeout"}};

%%% So that terminate will be run when supervisor is shutdown
handle_event(info, {'EXIT', _Sup, Reason}, StateName, _) ->
    Role = role(StateName),
    if
	Role == client ->
	    %% OTP-8111 tells this function clause fixes a problem in
	    %% clients, but there were no check for that role.
	    {stop, {shutdown, Reason}};

	Reason == normal ->
	    %% An exit normal should not cause a server to crash. This has happend...
	    keep_state_and_data;

	true ->
	    {stop, {shutdown, Reason}}
    end;

handle_event(info, check_cache, _, D) ->
    {keep_state, D, cond_set_idle_timer(D)};

handle_event(info, UnexpectedMessage, StateName, D = #data{ssh_params = Ssh}) ->
    case unexpected_fun(UnexpectedMessage, D) of
	report ->
	    Msg = lists:flatten(
		    io_lib:format(
                      "*** SSH: "
		      "Unexpected message '~p' received in state '~p'\n"
		      "Role: ~p\n"
		      "Peer: ~p\n"
		      "Local Address: ~p\n",
                      [UnexpectedMessage,
                       StateName,
                       Ssh#ssh.role, 
                       Ssh#ssh.peer,
                       ?GET_INTERNAL_OPT(address, Ssh#ssh.opts, undefined)])),
	    error_logger:info_report(Msg),
	    keep_state_and_data;

	skip ->
	    keep_state_and_data;

	Other ->
	    Msg = lists:flatten(
		    io_lib:format("*** SSH: "
                                  "Call to fun in 'unexpectedfun' failed:~n"
				  "Return: ~p\n"
				  "Message: ~p\n"
				  "Role: ~p\n"
				  "Peer: ~p\n"
				  "Local Address: ~p\n",
                                  [Other,
                                   UnexpectedMessage,
                                   Ssh#ssh.role,
                                   Ssh#ssh.peer,
                                   ?GET_INTERNAL_OPT(address, Ssh#ssh.opts, undefined)]
				 )),
	    error_logger:error_report(Msg),
	    keep_state_and_data
    end;

handle_event(internal, {send_disconnect,Code,DetailedText,Module,Line}, StateName, D0) ->
    {Shutdown, D} =  
        send_disconnect(Code, DetailedText, Module, Line, StateName, D0),
    {stop, Shutdown, D};


handle_event(enter, _OldState, State, D) ->
    %% Just skip
    {next_state, State, D};

handle_event(_Type, _Msg, {ext_info,Role,_ReNegFlag}, D) ->
    %% If something else arrives, goto next state and handle the event in that one
    {next_state, {connected,Role}, D, [postpone]};

handle_event(Type, Ev, StateName, D0) ->
    Details =
	case catch atom_to_list(element(1,Ev)) of
	    "ssh_msg_" ++_ when Type==internal ->
                lists:flatten(io_lib:format("Message ~p in wrong state (~p)", [element(1,Ev), StateName]));
	    _ ->
		io_lib:format("Unhandled event in state ~p:~n~p", [StateName,Ev])
	end,
    {Shutdown, D} =  
        ?send_disconnect(?SSH_DISCONNECT_PROTOCOL_ERROR, Details, StateName, D0),
    {stop, Shutdown, D}.


%%--------------------------------------------------------------------
-spec terminate(any(),
		state_name(),
		#data{}
	       ) -> term().
			
%% . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

terminate(normal, _StateName, D) ->
    stop_subsystem(D),
    close_transport(D);

terminate({shutdown,"Connection closed"}, _StateName, D) ->
    %% Normal: terminated by a sent by peer
    stop_subsystem(D),
    close_transport(D);

terminate({shutdown,{init,Reason}}, StateName, D) ->
    %% Error in initiation. "This error should not occur".
    log(error, D, "Shutdown in init (StateName=~p): ~p~n", [StateName,Reason]),
    stop_subsystem(D),
    close_transport(D);

terminate({shutdown,_R}, _StateName, D) ->
    %% Internal termination, usually already reported via ?send_disconnect resulting in a log entry
    stop_subsystem(D),
    close_transport(D);

terminate(shutdown, _StateName, D0) ->
    %% Terminated by supervisor
    %% Use send_msg directly instead of ?send_disconnect to avoid filling the log
    D = send_msg(#ssh_msg_disconnect{code = ?SSH_DISCONNECT_BY_APPLICATION,
                                     description = "Terminated (shutdown) by supervisor"},
                 D0),
    close_transport(D);

terminate(kill, _StateName, D) ->
    %% Got a kill signal
    stop_subsystem(D),
    close_transport(D);

terminate(Reason, StateName, D0) ->
    %% Others, e.g  undef, {badmatch,_}, ...
    log(error, D0, Reason),
    {_ShutdownReason, D} = ?send_disconnect(?SSH_DISCONNECT_BY_APPLICATION,
                                            "Internal error",
                                            io_lib:format("Reason: ~p",[Reason]),
                                            StateName, D0),
    stop_subsystem(D),
    close_transport(D).

%%--------------------------------------------------------------------

%% . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

format_status(normal, [_, _StateName, D]) ->
    [{data, [{"State", D}]}];
format_status(terminate, [_, _StateName, D]) ->
    [{data, [{"State", state_data2proplist(D)}]}].


state_data2proplist(D) ->
    DataPropList0 =
        fmt_stat_rec(record_info(fields, data), D,
                     [decrypted_data_buffer,
                      encrypted_data_buffer,
                      key_exchange_init_msg,
                      user_passwords,
                      opts,
                      inet_initial_recbuf_size]),
    SshPropList =
        fmt_stat_rec(record_info(fields, ssh), D#data.ssh_params,
                     [c_keyinit,
                      s_keyinit,
                      send_mac_key,
                      send_mac_size,
                      recv_mac_key,
                      recv_mac_size,
                      encrypt_keys,
                      encrypt_ctx,
                      decrypt_keys,
                      decrypt_ctx,
                      compress_ctx,
                      decompress_ctx,
                      shared_secret,
                      exchanged_hash,
                      session_id,
                      keyex_key,
                      keyex_info,
                      available_host_keys]),
    lists:keyreplace(ssh_params, 1, DataPropList0,
                     {ssh_params,SshPropList}).
    

fmt_stat_rec(FieldNames, Rec, Exclude) ->
    Values = tl(tuple_to_list(Rec)),
    [P || {K,_} = P <- lists:zip(FieldNames, Values),
	  not lists:member(K, Exclude)].

%%--------------------------------------------------------------------
-spec code_change(term() | {down,term()},
		  state_name(),
		  #data{},
		  term()
		 ) -> {ok, state_name(), #data{}}.

%% . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.


%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% Starting

start_the_connection_child(UserPid, Role, Socket, Options0) ->
    Sups = ?GET_INTERNAL_OPT(supervisors, Options0),
    ConnectionSup = proplists:get_value(connection_sup, Sups),
    Options = ?PUT_INTERNAL_OPT({user_pid,UserPid}, Options0),
    {ok, Pid} = ssh_connection_sup:start_child(ConnectionSup, [Role, Socket, Options]),
    ok = socket_control(Socket, Pid, Options),
    Pid.

%%--------------------------------------------------------------------
%% Stopping

stop_subsystem(#data{connection_state =
                         #connection{system_supervisor = SysSup,
                                     sub_system_supervisor = SubSysSup}}) when is_pid(SubSysSup) ->
    ssh_system_sup:stop_subsystem(SysSup, SubSysSup);
stop_subsystem(_) ->
    ok.


close_transport(#data{transport_cb = Transport,
                      socket = Socket}) ->
    try
        Transport:close(Socket)
    of
        _ -> ok
    catch
        _:_ -> ok
    end.

%%--------------------------------------------------------------------
%% "Invert" the Role
peer_role(client) -> server;
peer_role(server) -> client.

%%--------------------------------------------------------------------
available_hkey_algorithms(client, Options) ->
    case available_hkey_algos(Options) of
        [] ->
            error({shutdown, "No public key algs"});
        Algs ->
	    [atom_to_list(A) || A<-Algs]
    end;

available_hkey_algorithms(server, Options) ->
    case [A || A <- available_hkey_algos(Options),
               is_usable_host_key(A, Options)] of
        [] ->
            error({shutdown, "No host key available"});
	Algs ->
	    [atom_to_list(A) || A<-Algs]
    end.


available_hkey_algos(Options) ->
    SupAlgos = ssh_transport:supported_algorithms(public_key),
    HKeys = proplists:get_value(public_key,
                                ?GET_OPT(preferred_algorithms,Options)
                               ),
    NonSupported =  HKeys -- SupAlgos,
    AvailableAndSupported = HKeys -- NonSupported,
    AvailableAndSupported.


send_msg(Msg, State=#data{ssh_params=Ssh0}) when is_tuple(Msg) ->
    {Bytes, Ssh} = ssh_transport:ssh_packet(Msg, Ssh0),
    send_bytes(Bytes, State),
    State#data{ssh_params=Ssh}.

send_bytes("", _D) ->
    ok;
send_bytes(Bytes, #data{socket = Socket, transport_cb = Transport}) ->
    _ = Transport:send(Socket, Bytes),
    ok.

handle_version({2, 0} = NumVsn, StrVsn, Ssh0) ->
    Ssh = counterpart_versions(NumVsn, StrVsn, Ssh0),
    {ok, Ssh};
handle_version(_,_,_) ->
    not_supported.

string_version(#ssh{role = client, c_version = Vsn}) ->
    Vsn;
string_version(#ssh{role = server, s_version = Vsn}) ->
    Vsn.


cast(FsmPid, Event) ->
    gen_statem:cast(FsmPid, Event).

call(FsmPid, Event) ->
    call(FsmPid, Event, infinity).

call(FsmPid, Event, Timeout) ->
    try gen_statem:call(FsmPid, Event, Timeout) of
	{closed, _R} ->
	    {error, closed};
	{killed, _R} ->
	    {error, closed};
	Result ->
	    Result
    catch
	exit:{noproc, _R} ->
	    {error, closed};
	exit:{normal, _R} ->
	    {error, closed};
	exit:{{shutdown, _R},_} ->
	    {error, closed}
    end.


set_kex_overload_prefix(Msg = <<?BYTE(Op),_/binary>>, #data{ssh_params=SshParams})
  when Op == 30;
       Op == 31
       ->
    case catch atom_to_list(kex(SshParams)) of
	"ecdh-sha2-" ++ _ ->
	    <<"ecdh",Msg/binary>>;
        "curve25519-" ++ _ ->
	    <<"ecdh",Msg/binary>>;
        "curve448-" ++ _ ->
	    <<"ecdh",Msg/binary>>;
	"diffie-hellman-group-exchange-" ++ _ ->
	    <<"dh_gex",Msg/binary>>;
	"diffie-hellman-group" ++ _ ->
	    <<"dh",Msg/binary>>;
	_ ->
	    Msg
    end;
set_kex_overload_prefix(Msg, _) ->
    Msg.

kex(#ssh{algorithms=#alg{kex=Kex}}) -> Kex;
kex(_) -> undefined.

cache(#data{connection_state=C}) -> C#connection.channel_cache.
    

%%%----------------------------------------------------------------
handle_ssh_msg_ext_info(#ssh_msg_ext_info{}, D=#data{ssh_params = #ssh{recv_ext_info=false}} ) ->
    % The peer sent this although we didn't allow it!
    D;

handle_ssh_msg_ext_info(#ssh_msg_ext_info{data=Data}, D0) ->
    lists:foldl(fun ext_info/2, D0, Data).


ext_info({"server-sig-algs",SigAlgsStr},
         D0 = #data{ssh_params=#ssh{role=client,
                                    userauth_pubkeys=ClientSigAlgs}=Ssh0}) ->
    %% ClientSigAlgs are the pub_key algortithms that:
    %%  1) is usable, that is, the user has such a public key and
    %%  2) is either the default list or set by the caller
    %%     with the client option 'pref_public_key_algs'
    %%
    %% The list is already checked for duplicates.

    SigAlgs = [A || Astr <- string:tokens(SigAlgsStr, ","),
                    A <- try [list_to_existing_atom(Astr)]
                              %% list_to_existing_atom will fail for unknown algorithms
                         catch _:_ -> []
                         end],

    CommonAlgs = [A || A <- SigAlgs,
                       lists:member(A, ClientSigAlgs)],

    %% Re-arrange the client supported public-key algorithms so that the server
    %% preferred ones are tried first.
    %% Trying algorithms not mentioned by the server is ok, since the server can't know
    %% if the client supports 'server-sig-algs' or not.

    D0#data{
      ssh_params =
          Ssh0#ssh{
            userauth_pubkeys =
                CommonAlgs ++ (ClientSigAlgs -- CommonAlgs)
           }};

ext_info(_, D0) ->
    %% Not implemented
    D0.

%%%----------------------------------------------------------------
is_usable_user_pubkey(Alg, Ssh) ->
    try ssh_auth:get_public_key(Alg, Ssh) of
        {ok,_} -> true;
        _ -> false
    catch
        _:_ -> false
    end.

%%%----------------------------------------------------------------
is_usable_host_key(Alg, Opts) ->
    try ssh_transport:get_host_key(Alg, Opts)
    of
        _PrivHostKey -> true
    catch
        _:_ -> false
    end.

%%%----------------------------------------------------------------
handle_request(ChannelPid, ChannelId, Type, Data, WantReply, From, D) ->
    case ssh_client_channel:cache_lookup(cache(D), ChannelId) of
	#channel{remote_id = Id,
                 sent_close = false} = Channel ->
	    update_sys(cache(D), Channel, Type, ChannelPid),
	    send_msg(ssh_connection:channel_request_msg(Id, Type, WantReply, Data),
		     add_request(WantReply, ChannelId, From, D));

        _ when WantReply==true ->
            {error,closed};

        _ ->
            D
    end.

handle_request(ChannelId, Type, Data, WantReply, From, D) ->
    case ssh_client_channel:cache_lookup(cache(D), ChannelId) of
	#channel{remote_id = Id,
                 sent_close = false} ->
	    send_msg(ssh_connection:channel_request_msg(Id, Type, WantReply, Data),
		     add_request(WantReply, ChannelId, From, D));

	_ when WantReply==true ->
            {error,closed};
        
        _ ->
            D
    end.

%%%----------------------------------------------------------------
update_sys(Cache, Channel, Type, ChannelPid) ->
    ssh_client_channel:cache_update(Cache,
			     Channel#channel{sys = Type, user = ChannelPid}).

add_request(false, _ChannelId, _From, State) ->
    State;
add_request(true, ChannelId, From, #data{connection_state =
					     #connection{requests = Requests0} =
					     Connection} = State) ->
    Requests = [{ChannelId, From} | Requests0],
    State#data{connection_state = Connection#connection{requests = Requests}}.

new_channel_id(#data{connection_state = #connection{channel_id_seed = Id} =
			 Connection}
	       = State) ->
    {Id, State#data{connection_state =
			Connection#connection{channel_id_seed = Id + 1}}}.


%%%----------------------------------------------------------------
start_rekeying(Role, D0) ->
    {KeyInitMsg, SshPacket, Ssh} = ssh_transport:key_exchange_init_msg(D0#data.ssh_params),
    send_bytes(SshPacket, D0),
    D = D0#data{ssh_params = Ssh,
                key_exchange_init_msg = KeyInitMsg},
    {next_state, {kexinit,Role,renegotiate}, D}.


init_renegotiate_timers(State, D) ->
    {RekeyTimeout,_MaxSent} = ?GET_OPT(rekey_limit, (D#data.ssh_params)#ssh.opts),
    {next_state, State, D, [{{timeout,renegotiate},     RekeyTimeout,       none},
                            {{timeout,check_data_size}, ?REKEY_DATA_TIMOUT, none} ]}.


pause_renegotiate_timers(State, D) ->
    {next_state, State, D, [{{timeout,renegotiate},     infinity, none},
                            {{timeout,check_data_size}, infinity, none} ]}.

check_data_rekeying(Role, D) ->
    {ok, [{send_oct,SocketSentTotal}]} = inet:getstat(D#data.socket, [send_oct]),
    SentSinceRekey = SocketSentTotal - D#data.last_size_rekey,
    {_RekeyTimeout,MaxSent} = ?GET_OPT(rekey_limit, (D#data.ssh_params)#ssh.opts),
    case check_data_rekeying_dbg(SentSinceRekey, MaxSent) of
        true ->
            start_rekeying(Role, D#data{last_size_rekey = SocketSentTotal});
        _ ->
            %% Not enough data sent for a re-negotiation. Restart timer.
            {keep_state, D, {{timeout,check_data_size}, ?REKEY_DATA_TIMOUT, none}}
    end.

check_data_rekeying_dbg(SentSinceRekey, MaxSent) ->
    %% This function is for the ssh_dbg to trace on. See dbg_trace/3 at the end.
    SentSinceRekey >= MaxSent.

%%%----------------------------------------------------------------
%%% This server/client has decided to disconnect via the state machine:
%%% The unused arguments are for debugging.

send_disconnect(Code, DetailedText, Module, Line, StateName, D) ->
    send_disconnect(Code, default_text(Code), DetailedText, Module, Line, StateName, D).

send_disconnect(Code, Reason, DetailedText, Module, Line, StateName, D0) ->
    Msg = #ssh_msg_disconnect{code = Code,
                              description = Reason},
    D = send_msg(Msg, D0),
    LogMsg = io_lib:format("Disconnects with code = ~p [RFC4253 11.1]: ~s",[Code,Reason]),
    call_disconnectfun_and_log_cond(LogMsg, DetailedText, Module, Line, StateName, D),
    {{shutdown,Reason}, D}.

call_disconnectfun_and_log_cond(LogMsg, DetailedText, Module, Line, StateName, D) ->
    case disconnect_fun(LogMsg, D) of
        void ->
            log(info, D,
                "~s~n"
                "State = ~p~n"
                "Module = ~p, Line = ~p.~n"
                "Details:~n  ~s~n",
                [LogMsg, StateName, Module, Line, DetailedText]);
        _ ->
            ok
    end.


default_text(?SSH_DISCONNECT_HOST_NOT_ALLOWED_TO_CONNECT) -> "Host not allowed to connect";
default_text(?SSH_DISCONNECT_PROTOCOL_ERROR) -> "Protocol error";
default_text(?SSH_DISCONNECT_KEY_EXCHANGE_FAILED) -> "Key exchange failed";
default_text(?SSH_DISCONNECT_RESERVED) -> "Reserved";
default_text(?SSH_DISCONNECT_MAC_ERROR) -> "Mac error";
default_text(?SSH_DISCONNECT_COMPRESSION_ERROR) -> "Compression error";
default_text(?SSH_DISCONNECT_SERVICE_NOT_AVAILABLE) -> "Service not available";
default_text(?SSH_DISCONNECT_PROTOCOL_VERSION_NOT_SUPPORTED) -> "Protocol version not supported";
default_text(?SSH_DISCONNECT_HOST_KEY_NOT_VERIFIABLE) -> "Host key not verifiable";
default_text(?SSH_DISCONNECT_CONNECTION_LOST) -> "Connection lost";
default_text(?SSH_DISCONNECT_BY_APPLICATION) -> "By application";
default_text(?SSH_DISCONNECT_TOO_MANY_CONNECTIONS) -> "Too many connections";
default_text(?SSH_DISCONNECT_AUTH_CANCELLED_BY_USER) -> "Auth cancelled by user";
default_text(?SSH_DISCONNECT_NO_MORE_AUTH_METHODS_AVAILABLE) -> "Unable to connect using the available authentication methods";
default_text(?SSH_DISCONNECT_ILLEGAL_USER_NAME) -> "Illegal user name".

%%%----------------------------------------------------------------
counterpart_versions(NumVsn, StrVsn, #ssh{role = server} = Ssh) ->
    Ssh#ssh{c_vsn = NumVsn , c_version = StrVsn};
counterpart_versions(NumVsn, StrVsn, #ssh{role = client} = Ssh) ->
    Ssh#ssh{s_vsn = NumVsn , s_version = StrVsn}.

%%%----------------------------------------------------------------
conn_info_keys() ->
    [client_version,
     server_version,
     peer,
     user,
     sockname,
     options,
     algorithms,
     channels
    ].

conn_info(client_version, #data{ssh_params=S}) -> {S#ssh.c_vsn, S#ssh.c_version};
conn_info(server_version, #data{ssh_params=S}) -> {S#ssh.s_vsn, S#ssh.s_version};
conn_info(peer,           #data{ssh_params=S}) -> S#ssh.peer;
conn_info(user,                             D) -> D#data.auth_user;
conn_info(sockname,       #data{ssh_params=S}) -> S#ssh.local;
conn_info(options,        #data{ssh_params=#ssh{opts=Opts}})    -> lists:sort(
                                                                     maps:to_list(
                                                                       ssh_options:keep_set_options(
                                                                         client,
                                                                         ssh_options:keep_user_options(client,Opts))));
conn_info(algorithms,     #data{ssh_params=#ssh{algorithms=A}}) -> conn_info_alg(A);
conn_info(channels, D) -> try conn_info_chans(ets:tab2list(cache(D)))
                          catch _:_ -> undefined
                          end;
%% dbg options ( = not documented):
conn_info(socket, D) ->   D#data.socket;
conn_info(chan_ids, D) -> 
    ssh_client_channel:cache_foldl(fun(#channel{local_id=Id}, Acc) ->
				    [Id | Acc]
			    end, [], cache(D)).

conn_info_chans(Chs) ->
    Fs = record_info(fields, channel),
    [lists:zip(Fs, tl(tuple_to_list(Ch))) || Ch=#channel{} <- Chs].

conn_info_alg(AlgTup) ->
    [alg|Vs] = tuple_to_list(AlgTup),
    Fs = record_info(fields, alg),
    [{K,V} || {K,V} <- lists:zip(Fs,Vs),
              lists:member(K,[kex,
                              hkey,
                              encrypt,
                              decrypt,
                              send_mac,
                              recv_mac,
                              compress,
                              decompress,
                              send_ext_info,
                              recv_ext_info])].

%%%----------------------------------------------------------------
chann_info(recv_window, C) ->
    {{win_size,    C#channel.recv_window_size},
     {packet_size, C#channel.recv_packet_size}};
chann_info(send_window, C) ->
    {{win_size,    C#channel.send_window_size},
     {packet_size, C#channel.send_packet_size}};
%% dbg options ( = not documented):
chann_info(pid, C) ->
    C#channel.user.

%%%----------------------------------------------------------------
%% Assisting meta function for the *_info functions
fold_keys(Keys, Fun, Extra) ->
    lists:foldr(fun(Key, Acc) ->
			try Fun(Key, Extra) of
			    Value -> [{Key,Value}|Acc]
			catch
			    _:_ -> Acc
			end
		end, [], Keys).

%%%----------------------------------------------------------------
log(Tag, D, Format, Args) ->
    log(Tag, D, io_lib:format(Format,Args)).

log(Tag, D, Reason) ->
    case atom_to_list(Tag) of                   % Dialyzer-technical reasons...
        "error"   -> do_log(error_msg,   Reason, D);
        "warning" -> do_log(warning_msg, Reason, D);
        "info"    -> do_log(info_msg,    Reason, D)
    end.


do_log(F, Reason0, #data{ssh_params = S}) ->
    Reason =
        try io_lib:format("~s",[Reason0])
        of _ -> Reason0
        catch
            _:_ -> io_lib:format("~p",[Reason0])
        end,
    case S of
        #ssh{role = Role} when Role==server ;
                               Role==client ->
            {PeerRole,PeerVersion} =
                case Role of
                    server -> {"Client", S#ssh.c_version};
                    client -> {"Server", S#ssh.s_version}
                end,
            error_logger:F("Erlang SSH ~p ~s ~s.~n"
                           "~s: ~p~n"
                           "~s~n",
                           [Role,
                            ssh_log_version(), crypto_log_info(), 
                            PeerRole, PeerVersion,
                            Reason]);
        _ ->
            error_logger:F("Erlang SSH ~s ~s.~n"
                           "~s~n",
                           [ssh_log_version(), crypto_log_info(), 
                            Reason])
    end.

crypto_log_info() ->
    try 
        [{_,_,CI}] = crypto:info_lib(),
        case crypto:info_fips() of
            enabled ->
                <<"(",CI/binary,". FIPS enabled)">>;
            not_enabled ->
                <<"(",CI/binary,". FIPS available but not enabled)">>;
            _ ->
                <<"(",CI/binary,")">>
        end
    catch
        _:_ -> ""
    end.

ssh_log_version() ->
    case application:get_key(ssh,vsn) of
        {ok,Vsn} -> Vsn;
        undefined -> ""
    end.

%%%----------------------------------------------------------------
not_connected_filter({connection_reply, _Data}) -> true;
not_connected_filter(_) -> false.

%%%----------------------------------------------------------------

send_replies({Repls,C = #connection{}}, D) when is_list(Repls) ->
    send_replies(Repls, D#data{connection_state=C});
send_replies(Repls, State) ->
    lists:foldl(fun get_repl/2, {[],State}, Repls).

get_repl({connection_reply,Msg}, {CallRepls,S}) ->
    if is_record(Msg, ssh_msg_channel_success) ->
	    update_inet_buffers(S#data.socket);
       true ->
	    ok
    end,
    {CallRepls, send_msg(Msg,S)};
get_repl({channel_data,undefined,_Data}, Acc) ->
    Acc;
get_repl({channel_data,Pid,Data}, Acc) ->
    Pid ! {ssh_cm, self(), Data},
    Acc;
get_repl({channel_request_reply,From,Data}, {CallRepls,S}) ->
    {[{reply,From,Data}|CallRepls], S};
get_repl({flow_control,Cache,Channel,From,Msg}, {CallRepls,S}) ->
    ssh_client_channel:cache_update(Cache, Channel#channel{flow_control = undefined}),
    {[{reply,From,Msg}|CallRepls], S};
get_repl({flow_control,From,Msg}, {CallRepls,S}) ->
    {[{reply,From,Msg}|CallRepls], S};
%% get_repl(noreply, Acc) ->
%%     Acc;
%% get_repl([], Acc) ->
%%     Acc;
get_repl(X, Acc) ->
    exit({get_repl,X,Acc}).

%%%----------------------------------------------------------------
-define(CALL_FUN(Key,D), catch (?GET_OPT(Key, (D#data.ssh_params)#ssh.opts)) ).

%%disconnect_fun({disconnect,Msg}, D) -> ?CALL_FUN(disconnectfun,D)(Msg);
disconnect_fun(Reason, D)           -> ?CALL_FUN(disconnectfun,D)(Reason).

unexpected_fun(UnexpectedMessage, #data{ssh_params = #ssh{peer = {_,Peer} }} = D) ->
    ?CALL_FUN(unexpectedfun,D)(UnexpectedMessage, Peer).

debug_fun(#ssh_msg_debug{always_display = Display,
			 message = DbgMsg,
			 language = Lang},
	  D) ->
    ?CALL_FUN(ssh_msg_debug_fun,D)(self(), Display, DbgMsg, Lang).


connected_fun(User, Method, #data{ssh_params = #ssh{peer = {_,Peer}}} = D) ->
    ?CALL_FUN(connectfun,D)(User, Peer, Method).


retry_fun(_, undefined, _) ->
    ok;
retry_fun(User, Reason, #data{ssh_params = #ssh{opts = Opts,
						peer = {_,Peer}
					       }}) ->
    {Tag,Info} =
	case Reason of
	    {error, Error} ->
		{failfun, Error};
	    _ ->
		{infofun, Reason}
	end,
    Fun = ?GET_OPT(Tag, Opts),
    try erlang:fun_info(Fun, arity)
    of
	{arity, 2} -> %% Backwards compatible
	    catch Fun(User, Info);
	{arity, 3} ->
	    catch Fun(User, Peer, Info);
	_ ->
	    ok
    catch
	_:_ ->
	    ok
    end.

%%%----------------------------------------------------------------
%%% Cache idle timer that closes the connection if there are no
%%% channels open for a while.

cond_set_idle_timer(D) ->
    case ssh_client_channel:cache_info(num_entries, cache(D)) of
        0 -> {{timeout,idle_time}, ?GET_OPT(idle_time, (D#data.ssh_params)#ssh.opts), none};
        _ -> {{timeout,idle_time}, infinity, none}
    end.

%%%----------------------------------------------------------------
start_channel_request_timer(_,_, infinity) ->
    ok;
start_channel_request_timer(Channel, From, Time) ->
    erlang:send_after(Time, self(), {timeout, {Channel, From}}).

%%%----------------------------------------------------------------
%%% Connection start and initalization helpers

socket_control(Socket, Pid, Options) ->
    {_, Callback, _} =	?GET_OPT(transport, Options),
    case Callback:controlling_process(Socket, Pid) of
	ok ->
	    gen_statem:cast(Pid, socket_control);
	{error, Reason}	->
	    {error, Reason}
    end.

handshake(Pid, Ref, Timeout) ->
    receive
	ssh_connected ->
	    erlang:demonitor(Ref),
	    {ok, Pid};
	{Pid, not_connected, Reason} ->
	    {error, Reason};
	{Pid, user_password} ->
	    Pass = io:get_password(),
	    Pid ! Pass,
	    handshake(Pid, Ref, Timeout);
	{Pid, question} ->
	    Answer = io:get_line(""),
	    Pid ! Answer,
	    handshake(Pid, Ref, Timeout);
	{'DOWN', _, process, Pid, {shutdown, Reason}} ->
	    {error, Reason};
	{'DOWN', _, process, Pid, Reason} ->
	    {error, Reason}
    after Timeout ->
	    stop(Pid),
	    {error, timeout}
    end.

update_inet_buffers(Socket) ->
    try
        {ok, BufSzs0} = inet:getopts(Socket, [sndbuf,recbuf]),
        MinVal = 655360,
        [{Tag,MinVal} || {Tag,Val} <- BufSzs0,
                         Val < MinVal]
    of
	[] -> ok;
	NewOpts -> inet:setopts(Socket, NewOpts)
    catch
        _:_ -> ok
    end.

%%%################################################################
%%%#
%%%# Tracing
%%%#

dbg_trace(points,         _,  _) -> [terminate, disconnect, connections, connection_events, renegotiation];

dbg_trace(flags,  connections,  A) -> [c] ++ dbg_trace(flags, terminate, A);
dbg_trace(on,     connections,  A) -> dbg:tp(?MODULE,  init_connection_handler, 3, x),
                                      dbg_trace(on, terminate, A);
dbg_trace(off,    connections,  A) -> dbg:ctpg(?MODULE, init_connection_handler, 3),
                                      dbg_trace(off, terminate, A);
dbg_trace(format, connections, {call, {?MODULE,init_connection_handler, [Role, Sock, Opts]}}) ->
    DefaultOpts = ssh_options:handle_options(Role,[]),
    ExcludedKeys = [internal_options, user_options],
    NonDefaultOpts =
        maps:filter(fun(K,V) ->
                            case lists:member(K,ExcludedKeys) of
                                true ->
                                    false;
                                false ->
                                    V =/= (catch maps:get(K,DefaultOpts))
                            end
                    end,
                    Opts),
    {ok, {IPp,Portp}} = inet:peername(Sock),
    {ok, {IPs,Ports}} = inet:sockname(Sock),
    [io_lib:format("Starting ~p connection:\n",[Role]),
     io_lib:format("Socket = ~p, Peer = ~s:~p, Local = ~s:~p,~n"
                   "Non-default options:~n~p",
                   [Sock,inet:ntoa(IPp),Portp,inet:ntoa(IPs),Ports,
                    NonDefaultOpts])
    ];
dbg_trace(format, connections, F) ->
    dbg_trace(format, terminate, F);

dbg_trace(flags,  connection_events,  _) -> [c];
dbg_trace(on,     connection_events,  _) -> dbg:tp(?MODULE,   handle_event, 4, x);
dbg_trace(off,    connection_events,  _) -> dbg:ctpg(?MODULE, handle_event, 4);
dbg_trace(format, connection_events, {call, {?MODULE,handle_event, [EventType, EventContent, State, _Data]}}) ->
    ["Connection event\n",
     io_lib:format("EventType: ~p~nEventContent: ~p~nState: ~p~n", [EventType, EventContent, State])
    ];
dbg_trace(format, connection_events, {return_from, {?MODULE,handle_event,4}, Ret}) ->
    ["Connection event result\n",
     io_lib:format("~p~n", [event_handler_result(Ret)])
    ];

dbg_trace(flags,  renegotiation,  _) -> [c];
dbg_trace(on,     renegotiation,  _) -> dbg:tpl(?MODULE,   init_renegotiate_timers, 2, x),
                                        dbg:tpl(?MODULE,   pause_renegotiate_timers, 2, x),
                                        dbg:tpl(?MODULE,   check_data_rekeying_dbg, 2, x),
                                        dbg:tpl(?MODULE,   start_rekeying, 2, x);
dbg_trace(off,    renegotiation,  _) -> dbg:ctpl(?MODULE,   init_renegotiate_timers, 2),
                                        dbg:ctpl(?MODULE,   pause_renegotiate_timers, 2),
                                        dbg:ctpl(?MODULE,   check_data_rekeying_dbg, 2),
                                        dbg:ctpl(?MODULE,   start_rekeying, 2);
dbg_trace(format, renegotiation, {call, {?MODULE,init_renegotiate_timers,[_State,D]}}) ->
    ["Renegotiation init\n",
     io_lib:format("rekey_limit: ~p ({ms,bytes})~ncheck_data_size: ~p (ms)~n", 
                   [?GET_OPT(rekey_limit, (D#data.ssh_params)#ssh.opts),
                    ?REKEY_DATA_TIMOUT])
    ];
dbg_trace(format, renegotiation, {call, {?MODULE,pause_renegotiate_timers,[_State,_D]}}) ->
    ["Renegotiation pause\n"];
dbg_trace(format, renegotiation, {call, {?MODULE,start_rekeying,[_Role,_D]}}) ->
    ["Renegotiation start rekeying\n"];
dbg_trace(format, renegotiation, {call, {?MODULE,check_data_rekeying_dbg,[SentSinceRekey, MaxSent]}}) ->
    ["Renegotiation check data sent\n",
     io_lib:format("TotalSentSinceRekey: ~p~nMaxBeforeRekey: ~p~nStartRekey: ~p~n",
                   [SentSinceRekey, MaxSent, SentSinceRekey >= MaxSent])
    ];



dbg_trace(flags,  terminate,  _) -> [c];
dbg_trace(on,     terminate,  _) -> dbg:tp(?MODULE,  terminate, 3, x);
dbg_trace(off,    terminate,  _) -> dbg:ctpg(?MODULE, terminate, 3);
dbg_trace(format, terminate, {call, {?MODULE,terminate, [Reason, StateName, D]}}) ->
    ExtraInfo =
        try
            {conn_info(peer,D),
             conn_info(user,D),
             conn_info(sockname,D)}
        of
            {{_,{IPp,Portp}}, Usr, {IPs,Ports}} when is_tuple(IPp), is_tuple(IPs),
                                                     is_integer(Portp), is_integer(Ports) ->
                io_lib:format("Peer=~s:~p, Local=~s:~p, User=~p",
                              [inet:ntoa(IPp),Portp,inet:ntoa(IPs),Ports,Usr]);
            {Peer,Usr,Sockname} ->
                io_lib:format("Peer=~p, Local=~p, User=~p",[Peer,Sockname,Usr])
        catch
            _:_ ->
                ""
        end,
    if
        Reason == normal ;
        Reason == shutdown ;
        element(1,Reason) == shutdown
        ->
            ["Connection Terminating:\n",
             io_lib:format("Reason: ~p, StateName: ~p~n~s", [Reason, StateName, ExtraInfo])
            ];

        true ->
            ["Connection Terminating:\n",
             io_lib:format("Reason: ~p, StateName: ~p~n~s~nStateData = ~p",
                           [Reason, StateName, ExtraInfo, state_data2proplist(D)])
            ]
    end;

dbg_trace(flags,  disconnect, _) -> [c];
dbg_trace(on,     disconnect, _) -> dbg:tpl(?MODULE,  send_disconnect, 7, x);
dbg_trace(off,    disconnect, _) -> dbg:ctpl(?MODULE, send_disconnect, 7);
dbg_trace(format, disconnect, {call,{?MODULE,send_disconnect,
                                    [Code, Reason, DetailedText, Module, Line, StateName, _D]}}) ->
    ["Disconnecting:\n",
     io_lib:format(" Module = ~p, Line = ~p, StateName = ~p,~n"
                   " Code = ~p, Reason = ~p,~n"
                   " DetailedText =~n"
                   " ~p",
                   [Module, Line, StateName, Code, Reason, lists:flatten(DetailedText)])
    ].


event_handler_result({next_state, NextState, _NewData}) ->
    {next_state, NextState, "#data{}"};
event_handler_result({next_state, NextState, _NewData, Actions}) ->
    {next_state, NextState, "#data{}", Actions};
event_handler_result(R) ->
    state_callback_result(R).

state_callback_result({keep_state, _NewData}) ->
    {keep_state, "#data{}"};
state_callback_result({keep_state, _NewData, Actions}) ->
    {keep_state, "#data{}", Actions};
state_callback_result(keep_state_and_data) ->
    keep_state_and_data;
state_callback_result({keep_state_and_data, Actions}) ->
    {keep_state_and_data, Actions};
state_callback_result({repeat_state, _NewData}) ->
    {repeat_state, "#data{}"};
state_callback_result({repeat_state, _NewData, Actions}) ->
    {repeat_state, "#data{}", Actions};
state_callback_result(repeat_state_and_data) ->
    repeat_state_and_data;
state_callback_result({repeat_state_and_data, Actions}) ->
    {repeat_state_and_data, Actions};
state_callback_result(stop) ->
    stop;
state_callback_result({stop, Reason}) ->
    {stop, Reason};
state_callback_result({stop, Reason, _NewData}) ->
    {stop, Reason, "#data{}"};
state_callback_result({stop_and_reply, Reason,  Replies}) ->
    {stop_and_reply, Reason,  Replies};
state_callback_result({stop_and_reply, Reason,  Replies, _NewData}) ->
    {stop_and_reply, Reason,  Replies, "#data{}"};
state_callback_result(R) ->
    R.
