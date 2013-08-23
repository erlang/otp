%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2013. All Rights Reserved.
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

%%% Purpose : Main API module for SSL.

-module(tls).

-export([start/0, start/1, stop/0, transport_accept/1,
	 transport_accept/2, ssl_accept/1, ssl_accept/2, ssl_accept/3,
	 cipher_suites/0, cipher_suites/1, suite_definition/1,
	 close/1, shutdown/2,
	 connect/3, connect/2, connect/4, connection_info/1,
	 controlling_process/2, listen/2, peername/1, peercert/1,
	 recv/2, recv/3, send/2, getopts/2, setopts/2, sockname/1,
	 versions/0, session_info/1, format_error/1,
	 renegotiate/1, prf/5, clear_pem_cache/0, random_bytes/1, negotiated_next_protocol/1]).

-include("ssl_internal.hrl").
-include("ssl_record.hrl").
-include("ssl_cipher.hrl").
-include("ssl_handshake.hrl").
-include("ssl_srp.hrl").

-include_lib("public_key/include/public_key.hrl"). 

%% Visible in API
-export_type([connect_option/0, listen_option/0, ssl_option/0, transport_option/0,
	      erl_cipher_suite/0, %% From ssl_cipher.hrl 
	      tls_atom_version/0, %% From ssl_internal.hrl
	      prf_random/0, sslsocket/0]).

-record(config, {ssl,               %% SSL parameters
		 inet_user,         %% User set inet options
		 emulated,          %% #socket_option{} emulated
		 inet_ssl,          %% inet options for internal ssl socket 
		 cb                 %% Callback info
		}).

-type sslsocket()                :: #sslsocket{}.
-type connect_option()           :: socket_connect_option() | ssl_option() | transport_option().
-type socket_connect_option()    :: gen_tcp:connect_option().
-type listen_option()            :: socket_listen_option() | ssl_option() | transport_option().
-type socket_listen_option()     :: gen_tcp:listen_option().

-type ssl_option()    :: {verify, verify_type()} |
			{verify_fun, {fun(), InitialUserState::term()}} |
                        {fail_if_no_peer_cert, boolean()} | {depth, integer()} |
                        {cert, Der::binary()} | {certfile, path()} | {key, Der::binary()} |
                        {keyfile, path()} | {password, string()} | {cacerts, [Der::binary()]} |
                        {cacertfile, path()} | {dh, Der::binary()} | {dhfile, path()} |
                        {user_lookup_fun, {fun(), InitialUserState::term()}} |
                        {psk_identity, string()} |
                        {srp_identity, {string(), string()}} |
                        {ciphers, ciphers()} | {ssl_imp, ssl_imp()} | {reuse_sessions, boolean()} |
                        {reuse_session, fun()} | {hibernate_after, integer()|undefined} |
                        {next_protocols_advertised, list(binary())} |
                        {client_preferred_next_protocols, binary(), client | server, list(binary())}.

-type verify_type()  :: verify_none | verify_peer.
-type path()         :: string().
-type ciphers()      :: [erl_cipher_suite()] |
			string(). % (according to old API)
-type ssl_imp()      :: new | old.

-type transport_option() :: {cb_info, {CallbackModule::atom(), DataTag::atom(), 
				       ClosedTag::atom(), ErrTag::atom()}}.
-type prf_random() :: client_random | server_random.

%%--------------------------------------------------------------------
-spec start() -> ok  | {error, reason()}.
-spec start(permanent | transient | temporary) -> ok | {error, reason()}.
%%
%% Description: Utility function that starts the ssl, 
%% crypto and public_key applications. Default type
%% is temporary. see application(3)
%%--------------------------------------------------------------------
start() ->
    application:start(crypto),
    application:start(asn1),
    application:start(public_key),
    application:start(ssl).

start(Type) ->
    application:start(crypto, Type),
    application:start(asn1),
    application:start(public_key, Type),
    application:start(ssl, Type).

%%--------------------------------------------------------------------
-spec stop() -> ok.
%%
%% Description: Stops the ssl application.
%%--------------------------------------------------------------------
stop() ->
    application:stop(ssl).

%%--------------------------------------------------------------------
-spec connect(host() | port(), [connect_option()]) -> {ok, #sslsocket{}} |
					      {error, reason()}.
-spec connect(host() | port(), [connect_option()] | inet:port_number(), 
	      timeout() | list()) ->
		     {ok, #sslsocket{}} | {error, reason()}.
-spec connect(host() | port(), inet:port_number(), list(), timeout()) ->
		     {ok, #sslsocket{}} | {error, reason()}.

%%
%% Description: Connect to an ssl server.
%%--------------------------------------------------------------------
connect(Socket, SslOptions) when is_port(Socket) ->
    connect(Socket, SslOptions, infinity).

connect(Socket, SslOptions0, Timeout) when is_port(Socket) ->
    {Transport,_,_,_} = proplists:get_value(cb_info, SslOptions0, 
					      {gen_tcp, tcp, tcp_closed, tcp_error}),    
    EmulatedOptions = emulated_options(),
    {ok, SocketValues} = ssl_socket:getopts(Transport, Socket, EmulatedOptions),
    try handle_options(SslOptions0 ++ SocketValues, client) of
	{ok, #config{cb = CbInfo, ssl = SslOptions, emulated = EmOpts}} ->
	   
	    ok = ssl_socket:setopts(Transport, Socket, internal_inet_values()),
	    case ssl_socket:peername(Transport, Socket) of
		{ok, {Address, Port}} ->
		    tls_connection:connect(Address, Port, Socket, 
					   {SslOptions, EmOpts},
					   self(), CbInfo, Timeout);
		{error, Error} ->
		    {error, Error}
	    end
    catch 
	_:{error, Reason} ->
            {error, Reason}
    end;

connect(Host, Port, Options) ->
    connect(Host, Port, Options, infinity).

connect(Host, Port, Options, Timeout) ->
    try handle_options(Options, client) of
	{ok, Config} ->
	    do_connect(Host,Port,Config,Timeout)
    catch
	throw:Error ->
	    Error
    end.

%%--------------------------------------------------------------------
-spec listen(inet:port_number(), [listen_option()]) ->{ok, #sslsocket{}} | {error, reason()}.
		    
%%
%% Description: Creates an ssl listen socket.
%%--------------------------------------------------------------------
listen(_Port, []) ->
    {error, nooptions};
listen(Port, Options0) ->
    try
	{ok, Config} = handle_options(Options0, server),
	#config{cb = {Transport, _, _, _}, inet_user = Options} = Config,
	case Transport:listen(Port, Options) of
	    {ok, ListenSocket} ->
		{ok, #sslsocket{pid = {ListenSocket, Config}}};
	    Err = {error, _} ->
		Err
	end
    catch
	Error = {error, _} ->
	    Error
    end.
%%--------------------------------------------------------------------
-spec transport_accept(#sslsocket{}) -> {ok, #sslsocket{}} |
					{error, reason()}.
-spec transport_accept(#sslsocket{}, timeout()) -> {ok, #sslsocket{}} |
						   {error, reason()}.
%%
%% Description: Performs transport accept on an ssl listen socket
%%--------------------------------------------------------------------
transport_accept(ListenSocket) ->
    transport_accept(ListenSocket, infinity).

transport_accept(#sslsocket{pid = {ListenSocket, #config{cb = CbInfo, ssl = SslOpts}}}, Timeout) ->
    
    %% The setopt could have been invoked on the listen socket
    %% and options should be inherited.
    EmOptions = emulated_options(),
    {Transport,_,_, _} = CbInfo,    
    {ok, SocketValues} = ssl_socket:getopts(Transport, ListenSocket, EmOptions),
    ok = ssl_socket:setopts(Transport, ListenSocket, internal_inet_values()),
    case Transport:accept(ListenSocket, Timeout) of
	{ok, Socket} ->
	    ok = ssl_socket:setopts(Transport, ListenSocket, SocketValues),
	    {ok, Port} = ssl_socket:port(Transport, Socket),
	    ConnArgs = [server, "localhost", Port, Socket,
			{SslOpts, socket_options(SocketValues)}, self(), CbInfo],
	    case ssl_connection_sup:start_child(ConnArgs) of
		{ok, Pid} ->
		    tls_connection:socket_control(Socket, Pid, Transport);
		{error, Reason} ->
		    {error, Reason}
	    end;
	{error, Reason} ->
	    {error, Reason}
    end.

%%--------------------------------------------------------------------
-spec ssl_accept(#sslsocket{}) -> ok | {error, reason()}.
-spec ssl_accept(#sslsocket{} | port(), timeout()| [ssl_option() 
						    | transport_option()]) ->
			ok | {ok, #sslsocket{}} | {error, reason()}.
-spec ssl_accept(port(), [ssl_option()| transport_option()], timeout()) -> 
			{ok, #sslsocket{}} | {error, reason()}.
%%
%% Description: Performs accept on an ssl listen socket. e.i. performs
%%              ssl handshake. 
%%--------------------------------------------------------------------
ssl_accept(ListenSocket) ->
    ssl_accept(ListenSocket, infinity).

ssl_accept(#sslsocket{} = Socket, Timeout) ->
    tls_connection:handshake(Socket, Timeout);
    
ssl_accept(ListenSocket, SslOptions)  when is_port(ListenSocket) -> 
    ssl_accept(ListenSocket, SslOptions, infinity).

ssl_accept(Socket, SslOptions, Timeout) when is_port(Socket) -> 
    {Transport,_,_,_} = 
	proplists:get_value(cb_info, SslOptions, {gen_tcp, tcp, tcp_closed, tcp_error}),    
    EmulatedOptions = emulated_options(),
    {ok, SocketValues} = ssl_socket:getopts(Transport, Socket, EmulatedOptions),
    try handle_options(SslOptions ++ SocketValues, server) of
	{ok, #config{cb = CbInfo, ssl = SslOpts, emulated = EmOpts}} ->
	    ok = ssl_socket:setopts(Transport, Socket, internal_inet_values()),
	    {ok, Port} = ssl_socket:port(Transport, Socket),
	    tls_connection:ssl_accept(Port, Socket,
				      {SslOpts, EmOpts},
				      self(), CbInfo, Timeout)
    catch 
	Error = {error, _Reason} -> Error
    end.

%%--------------------------------------------------------------------
-spec  close(#sslsocket{}) -> term().
%%
%% Description: Close an ssl connection
%%--------------------------------------------------------------------  
close(#sslsocket{pid = Pid}) when is_pid(Pid) ->
    tls_connection:close(Pid);
close(#sslsocket{pid = {ListenSocket, #config{cb={Transport,_, _, _}}}}) ->
    Transport:close(ListenSocket).

%%--------------------------------------------------------------------
-spec send(#sslsocket{}, iodata()) -> ok | {error, reason()}.
%% 
%% Description: Sends data over the ssl connection
%%--------------------------------------------------------------------
send(#sslsocket{pid = Pid}, Data) when is_pid(Pid) ->
    tls_connection:send(Pid, Data);
send(#sslsocket{pid = {ListenSocket, #config{cb={Transport, _, _, _}}}}, Data) ->
    Transport:send(ListenSocket, Data). %% {error,enotconn}

%%--------------------------------------------------------------------
-spec recv(#sslsocket{}, integer()) -> {ok, binary()| list()} | {error, reason()}.
-spec recv(#sslsocket{}, integer(), timeout()) -> {ok, binary()| list()} | {error, reason()}.
%%
%% Description: Receives data when active = false
%%--------------------------------------------------------------------
recv(Socket, Length) ->
    recv(Socket, Length, infinity).
recv(#sslsocket{pid = Pid}, Length, Timeout) when is_pid(Pid) ->
    tls_connection:recv(Pid, Length, Timeout);
recv(#sslsocket{pid = {Listen, 
		       #config{cb={Transport, _, _, _}}}}, _,_) when is_port(Listen)->
    Transport:recv(Listen, 0). %% {error,enotconn}

%%--------------------------------------------------------------------
-spec controlling_process(#sslsocket{}, pid()) -> ok | {error, reason()}.
%%				 
%% Description: Changes process that receives the messages when active = true
%% or once. 
%%--------------------------------------------------------------------
controlling_process(#sslsocket{pid = Pid}, NewOwner) when is_pid(Pid), is_pid(NewOwner) ->
    tls_connection:new_user(Pid, NewOwner);
controlling_process(#sslsocket{pid = {Listen,
				      #config{cb={Transport, _, _, _}}}}, 
		    NewOwner) when is_port(Listen),
				   is_pid(NewOwner) ->
    Transport:controlling_process(Listen, NewOwner).

%%--------------------------------------------------------------------
-spec connection_info(#sslsocket{}) -> 	{ok, {tls_atom_version(), erl_cipher_suite()}} | 
					{error, reason()}.
%%
%% Description: Returns ssl protocol and cipher used for the connection
%%--------------------------------------------------------------------
connection_info(#sslsocket{pid = Pid}) when is_pid(Pid) ->
    tls_connection:info(Pid);
connection_info(#sslsocket{pid = {Listen, _}}) when is_port(Listen) ->
    {error, enotconn}.

%%--------------------------------------------------------------------
-spec peername(#sslsocket{}) -> {ok, {inet:ip_address(), inet:port_number()}} | {error, reason()}.
%%
%% Description: same as inet:peername/1.
%%--------------------------------------------------------------------
peername(#sslsocket{pid = Pid, fd = {Transport, Socket}}) when is_pid(Pid)->
    ssl_socket:peername(Transport, Socket);
peername(#sslsocket{pid = {ListenSocket,  #config{cb = {Transport,_,_,_}}}}) ->
    ssl_socket:peername(Transport, ListenSocket). %% Will return {error, enotconn}

%%--------------------------------------------------------------------
-spec peercert(#sslsocket{}) ->{ok, DerCert::binary()} | {error, reason()}.
%%
%% Description: Returns the peercert.
%%--------------------------------------------------------------------
peercert(#sslsocket{pid = Pid}) when is_pid(Pid) ->
    case tls_connection:peer_certificate(Pid) of
	{ok, undefined} ->
	    {error, no_peercert};
        Result ->
	    Result
    end;
peercert(#sslsocket{pid = {Listen, _}}) when is_port(Listen) ->
    {error, enotconn}.

%%--------------------------------------------------------------------
-spec suite_definition(cipher_suite()) -> erl_cipher_suite().
%%
%% Description: Return erlang cipher suite definition.
%%--------------------------------------------------------------------
suite_definition(S) ->
    {KeyExchange, Cipher, Hash, _} = ssl_cipher:suite_definition(S),
    {KeyExchange, Cipher, Hash}.

%%--------------------------------------------------------------------
-spec negotiated_next_protocol(#sslsocket{}) -> {ok, binary()} | {error, reason()}.
%%
%% Description: Returns the next protocol that has been negotiated. If no
%% protocol has been negotiated will return {error, next_protocol_not_negotiated}
%%--------------------------------------------------------------------
negotiated_next_protocol(#sslsocket{pid = Pid}) ->
    tls_connection:negotiated_next_protocol(Pid).

-spec cipher_suites() -> [erl_cipher_suite()].
-spec cipher_suites(erlang | openssl | all) -> [erl_cipher_suite()] | [string()].
			   
%% Description: Returns all supported cipher suites.
%%--------------------------------------------------------------------
cipher_suites() ->
    cipher_suites(erlang).
  
cipher_suites(erlang) ->
    Version = tls_record:highest_protocol_version([]),
    [suite_definition(S) || S <- ssl_cipher:suites(Version)];

cipher_suites(openssl) ->
    Version = tls_record:highest_protocol_version([]),
    [ssl_cipher:openssl_suite_name(S) || S <- ssl_cipher:suites(Version)];
cipher_suites(all) ->
    Version = tls_record:highest_protocol_version([]),
    Supported = ssl_cipher:suites(Version)
	++ ssl_cipher:anonymous_suites()
	++ ssl_cipher:psk_suites(Version)
	++ ssl_cipher:srp_suites(),
    [suite_definition(S) || S <- Supported].

%%--------------------------------------------------------------------
-spec getopts(#sslsocket{}, [gen_tcp:option_name()]) ->
		     {ok, [gen_tcp:option()]} | {error, reason()}.
%% 
%% Description: Gets options
%%--------------------------------------------------------------------
getopts(#sslsocket{pid = Pid}, OptionTags) when is_pid(Pid), is_list(OptionTags) ->
    tls_connection:get_opts(Pid, OptionTags);
getopts(#sslsocket{pid = {ListenSocket,  #config{cb = {Transport,_,_,_}}}}, 
	OptionTags) when is_list(OptionTags) ->
    try ssl_socket:getopts(Transport, ListenSocket, OptionTags) of
	{ok, _} = Result ->
	    Result;
	{error, InetError} ->
	    {error, {options, {socket_options, OptionTags, InetError}}}
    catch
	_:_ ->
	    {error, {options, {socket_options, OptionTags}}}
    end;
getopts(#sslsocket{}, OptionTags) ->
    {error, {options, {socket_options, OptionTags}}}.

%%--------------------------------------------------------------------
-spec setopts(#sslsocket{},  [gen_tcp:option()]) -> ok | {error, reason()}.
%% 
%% Description: Sets options
%%--------------------------------------------------------------------
setopts(#sslsocket{pid = Pid}, Options0) when is_pid(Pid), is_list(Options0)  ->
    try proplists:expand([{binary, [{mode, binary}]},
			  {list, [{mode, list}]}], Options0) of
	Options ->
	    tls_connection:set_opts(Pid, Options)
    catch
	_:_ ->
	    {error, {options, {not_a_proplist, Options0}}}
    end;

setopts(#sslsocket{pid = {ListenSocket, #config{cb = {Transport,_,_,_}}}}, Options) when is_list(Options) ->
    try ssl_socket:setopts(Transport, ListenSocket, Options) of
	ok ->
	    ok;
	{error, InetError} ->
	    {error, {options, {socket_options, Options, InetError}}}
    catch
	_:Error ->
	    {error, {options, {socket_options, Options, Error}}}
    end;
setopts(#sslsocket{}, Options) ->
    {error, {options,{not_a_proplist, Options}}}.

%%---------------------------------------------------------------
-spec shutdown(#sslsocket{}, read | write | read_write) ->  ok | {error, reason()}.
%%		      
%% Description: Same as gen_tcp:shutdown/2
%%--------------------------------------------------------------------
shutdown(#sslsocket{pid = {Listen, #config{cb={Transport,_, _, _}}}},
	 How) when is_port(Listen) ->
    Transport:shutdown(Listen, How);
shutdown(#sslsocket{pid = Pid}, How) ->
    tls_connection:shutdown(Pid, How).

%%--------------------------------------------------------------------
-spec sockname(#sslsocket{}) -> {ok, {inet:ip_address(), inet:port_number()}} | {error, reason()}.
%%		     
%% Description: Same as inet:sockname/1
%%--------------------------------------------------------------------
sockname(#sslsocket{pid = {Listen,  #config{cb={Transport,_, _, _}}}}) when is_port(Listen) ->
    ssl_socket:sockname(Transport, Listen);

sockname(#sslsocket{pid = Pid, fd = {Transport, Socket}}) when is_pid(Pid) ->
    ssl_socket:sockname(Transport, Socket).

%%---------------------------------------------------------------
-spec session_info(#sslsocket{}) -> {ok, list()} | {error, reason()}.
%% 
%% Description: Returns list of session info currently [{session_id, session_id(),
%% {cipher_suite, cipher_suite()}]
%%--------------------------------------------------------------------
session_info(#sslsocket{pid = Pid}) when is_pid(Pid) ->
    tls_connection:session_info(Pid);
session_info(#sslsocket{pid = {Listen,_}}) when is_port(Listen) ->
    {error, enotconn}.

%%---------------------------------------------------------------
-spec versions() -> [{ssl_app, string()} | {supported, [tls_atom_version()]} | 
		     {available, [tls_atom_version()]}].
%%
%% Description: Returns a list of relevant versions.
%%--------------------------------------------------------------------
versions() ->
    Vsns = tls_record:supported_protocol_versions(),
    SupportedVsns = [tls_record:protocol_version(Vsn) || Vsn <- Vsns],
    AvailableVsns = ?ALL_SUPPORTED_VERSIONS,
    [{ssl_app, ?VSN}, {supported, SupportedVsns}, {available, AvailableVsns}].


%%---------------------------------------------------------------
-spec renegotiate(#sslsocket{}) -> ok | {error, reason()}.
%% 
%% Description: Initiates a renegotiation.
%%--------------------------------------------------------------------
renegotiate(#sslsocket{pid = Pid}) when is_pid(Pid) ->
    tls_connection:renegotiation(Pid);
renegotiate(#sslsocket{pid = {Listen,_}}) when is_port(Listen) ->
    {error, enotconn}.

%%--------------------------------------------------------------------
-spec prf(#sslsocket{}, binary() | 'master_secret', binary(),
	  binary() | prf_random(), non_neg_integer()) ->
		 {ok, binary()} | {error, reason()}.
%%
%% Description: use a ssl sessions TLS PRF to generate key material
%%--------------------------------------------------------------------
prf(#sslsocket{pid = Pid},
    Secret, Label, Seed, WantedLength) when is_pid(Pid) ->
    tls_connection:prf(Pid, Secret, Label, Seed, WantedLength);
prf(#sslsocket{pid = {Listen,_}}, _,_,_,_) when is_port(Listen) ->
    {error, enotconn}.

%%--------------------------------------------------------------------
-spec clear_pem_cache() -> ok.
%%
%% Description: Clear the PEM cache
%%--------------------------------------------------------------------
clear_pem_cache() ->
    ssl_manager:clear_pem_cache().

%%---------------------------------------------------------------
-spec format_error({error, term()}) -> list().
%%
%% Description: Creates error string.
%%--------------------------------------------------------------------
format_error({error, Reason}) ->
    format_error(Reason);
format_error(Reason) when is_list(Reason) ->
    Reason;
format_error(closed) ->
    "TLS connection is closed";
format_error({tls_alert, Description}) ->
    "TLS Alert: " ++ Description;
format_error({options,{FileType, File, Reason}}) when FileType == cacertfile;
						      FileType == certfile;
						      FileType == keyfile;
						      FileType == dhfile ->
    Error = file_error_format(Reason),
    file_desc(FileType) ++ File ++ ": " ++ Error;
format_error({options, {socket_options, Option, Error}}) ->
    lists:flatten(io_lib:format("Invalid transport socket option ~p: ~s", [Option, format_error(Error)]));
format_error({options, {socket_options, Option}}) ->
    lists:flatten(io_lib:format("Invalid socket option: ~p", [Option]));
format_error({options, Options}) ->
    lists:flatten(io_lib:format("Invalid TLS option: ~p", [Options]));

format_error(Error) ->
    case inet:format_error(Error) of
        "unknown POSIX" ++ _ ->
            unexpected_format(Error);
        Other ->
            Other
    end.

%%--------------------------------------------------------------------
-spec random_bytes(integer()) -> binary().

%%
%% Description: Generates cryptographically secure random sequence if possible
%% fallbacks on pseudo random function
%%--------------------------------------------------------------------
random_bytes(N) ->
    try crypto:strong_rand_bytes(N) of
	RandBytes ->
	    RandBytes
    catch
	error:low_entropy ->
	    crypto:rand_bytes(N)
    end.

%%%--------------------------------------------------------------
%%% Internal functions
%%%--------------------------------------------------------------------
do_connect(Address, Port,
	       #config{cb=CbInfo, inet_user=UserOpts, ssl=SslOpts,
		       emulated=EmOpts,inet_ssl=SocketOpts},
	       Timeout) ->
    {Transport, _, _, _} = CbInfo,    
    try Transport:connect(Address, Port,  SocketOpts, Timeout) of
	{ok, Socket} ->
	    tls_connection:connect(Address, Port, Socket, {SslOpts,EmOpts},
				   self(), CbInfo, Timeout);
	{error, Reason} ->
	    {error, Reason}
    catch
	exit:{function_clause, _} ->
	    {error, {options, {cb_info, CbInfo}}};
	exit:badarg ->
	    {error, {options, {socket_options, UserOpts}}};
	exit:{badarg, _} ->
	    {error, {options, {socket_options, UserOpts}}}
    end.

handle_options(Opts0, _Role) ->
    Opts = proplists:expand([{binary, [{mode, binary}]},
			     {list, [{mode, list}]}], Opts0),
    ReuseSessionFun = fun(_, _, _, _) -> true end,

    DefaultVerifyNoneFun =
	{fun(_,{bad_cert, _}, UserState) ->
		 {valid, UserState};
	    (_,{extension, _}, UserState) ->
		 {unknown, UserState};
	    (_, valid, UserState) ->
		 {valid, UserState};
	    (_, valid_peer, UserState) ->
		 {valid, UserState}
	 end, []},

    VerifyNoneFun = handle_option(verify_fun, Opts, DefaultVerifyNoneFun),

    UserFailIfNoPeerCert = handle_option(fail_if_no_peer_cert, Opts, false),
    UserVerifyFun = handle_option(verify_fun, Opts, undefined),
    CaCerts = handle_option(cacerts, Opts, undefined),

    {Verify, FailIfNoPeerCert, CaCertDefault, VerifyFun} =
	%% Handle 0, 1, 2 for backwards compatibility
	case proplists:get_value(verify, Opts, verify_none) of
	    0 ->
		{verify_none, false,
		 ca_cert_default(verify_none, VerifyNoneFun, CaCerts), VerifyNoneFun};
	    1  ->
		{verify_peer, false,
		 ca_cert_default(verify_peer, UserVerifyFun, CaCerts), UserVerifyFun};
	    2 ->
		{verify_peer, true,
		 ca_cert_default(verify_peer, UserVerifyFun, CaCerts), UserVerifyFun};
	    verify_none ->
		{verify_none, false,
		 ca_cert_default(verify_none, VerifyNoneFun, CaCerts), VerifyNoneFun};
	    verify_peer ->
		{verify_peer, UserFailIfNoPeerCert,
		 ca_cert_default(verify_peer, UserVerifyFun, CaCerts), UserVerifyFun};
	    Value ->
		throw({error, {options, {verify, Value}}})
	end,

    CertFile = handle_option(certfile, Opts, <<>>),
    
    Versions = case handle_option(versions, Opts, []) of
		   [] ->
		       tls_record:supported_protocol_versions();  
		   Vsns  ->
		       [tls_record:protocol_version(Vsn) || Vsn <- Vsns]
	       end,  

    SSLOptions = #ssl_options{
      versions   = Versions,
      verify     = validate_option(verify, Verify),
      verify_fun = VerifyFun,
      fail_if_no_peer_cert = FailIfNoPeerCert,
      verify_client_once =  handle_option(verify_client_once, Opts, false),
      depth      = handle_option(depth,  Opts, 1),
      cert       = handle_option(cert, Opts, undefined),
      certfile   = CertFile,
      key        = handle_option(key, Opts, undefined),
      keyfile    = handle_option(keyfile,  Opts, CertFile),
      password   = handle_option(password, Opts, ""),
      cacerts    = CaCerts,
      cacertfile = handle_option(cacertfile, Opts, CaCertDefault),
      dh         = handle_option(dh, Opts, undefined),
      dhfile     = handle_option(dhfile, Opts, undefined),
      user_lookup_fun = handle_option(user_lookup_fun, Opts, undefined),
      psk_identity = handle_option(psk_identity, Opts, undefined),
      srp_identity = handle_option(srp_identity, Opts, undefined),
      ciphers    = handle_option(ciphers, Opts, []),
      %% Server side option
      reuse_session = handle_option(reuse_session, Opts, ReuseSessionFun),
      reuse_sessions = handle_option(reuse_sessions, Opts, true),
      secure_renegotiate = handle_option(secure_renegotiate, Opts, false),
      renegotiate_at = handle_option(renegotiate_at, Opts, ?DEFAULT_RENEGOTIATE_AT),
      hibernate_after = handle_option(hibernate_after, Opts, undefined),
      erl_dist = handle_option(erl_dist, Opts, false),
      next_protocols_advertised = 
			handle_option(next_protocols_advertised, Opts, undefined),
      next_protocol_selector = 
			make_next_protocol_selector(
			  handle_option(client_preferred_next_protocols, Opts, undefined)),
      log_alert = handle_option(log_alert, Opts, true)
     },

    CbInfo  = proplists:get_value(cb_info, Opts, {gen_tcp, tcp, tcp_closed, tcp_error}),    
    SslOptions = [versions, verify, verify_fun,
		  fail_if_no_peer_cert, verify_client_once,
		  depth, cert, certfile, key, keyfile,
		  password, cacerts, cacertfile, dh, dhfile, 
		  user_lookup_fun, psk_identity, srp_identity, ciphers,
		  reuse_session, reuse_sessions, ssl_imp,
		  cb_info, renegotiate_at, secure_renegotiate, hibernate_after, 
		  erl_dist, next_protocols_advertised,
		  client_preferred_next_protocols, log_alert],
    
    SockOpts = lists:foldl(fun(Key, PropList) -> 
				   proplists:delete(Key, PropList)
			   end, Opts, SslOptions),
    
    {SSLsock, Emulated} = emulated_options(SockOpts),
    {ok, #config{ssl=SSLOptions, emulated=Emulated, inet_ssl=SSLsock,
		 inet_user=SockOpts, cb=CbInfo}}.

handle_option(OptionName, Opts, Default) ->
    validate_option(OptionName, 
		    proplists:get_value(OptionName, Opts, Default)).


validate_option(versions, Versions)  ->
    validate_versions(Versions, Versions);
validate_option(verify, Value) 
  when Value == verify_none; Value == verify_peer ->
    Value;
validate_option(verify_fun, undefined)  ->
    undefined;
%% Backwards compatibility
validate_option(verify_fun, Fun) when is_function(Fun) ->
    {fun(_,{bad_cert, _} = Reason, OldFun) ->
	     case OldFun([Reason]) of
		 true ->
		     {valid, OldFun};
		 false ->
		     {fail, Reason}
	     end;
	(_,{extension, _}, UserState) ->
	     {unknown, UserState};
	(_, valid, UserState) ->
	     {valid, UserState};
	(_, valid_peer, UserState) ->
	     {valid, UserState}
     end, Fun};
validate_option(verify_fun, {Fun, _} = Value) when is_function(Fun) ->
   Value;
validate_option(fail_if_no_peer_cert, Value) 
  when Value == true; Value == false ->
    Value;
validate_option(verify_client_once, Value) 
  when Value == true; Value == false ->
    Value;
validate_option(depth, Value) when is_integer(Value), 
                                   Value >= 0, Value =< 255->
    Value;
validate_option(cert, Value) when Value == undefined;
                                 is_binary(Value) ->
    Value;
validate_option(certfile, undefined = Value) ->
    Value;
validate_option(certfile, Value) when is_binary(Value) ->
    Value;
validate_option(certfile, Value) when is_list(Value) ->
    list_to_binary(Value);

validate_option(key, undefined) ->
    undefined;
validate_option(key, {KeyType, Value}) when is_binary(Value),
					    KeyType == rsa; %% Backwards compatibility
					    KeyType == dsa; %% Backwards compatibility
					    KeyType == 'RSAPrivateKey';
					    KeyType == 'DSAPrivateKey';
					    KeyType == 'PrivateKeyInfo' ->
    {KeyType, Value};

validate_option(keyfile, undefined) ->
   <<>>;
validate_option(keyfile, Value) when is_binary(Value) ->
    Value;
validate_option(keyfile, Value) when is_list(Value), Value =/= "" ->
    list_to_binary(Value);
validate_option(password, Value) when is_list(Value) ->
    Value;

validate_option(cacerts, Value) when Value == undefined;
				     is_list(Value) ->
    Value;
%% certfile must be present in some cases otherwhise it can be set
%% to the empty string.
validate_option(cacertfile, undefined) ->
   <<>>;
validate_option(cacertfile, Value) when is_binary(Value) ->
    Value;
validate_option(cacertfile, Value) when is_list(Value), Value =/= ""->
    list_to_binary(Value);
validate_option(dh, Value) when Value == undefined;
				is_binary(Value) ->
    Value;
validate_option(dhfile, undefined = Value)  ->
    Value;
validate_option(dhfile, Value) when is_binary(Value) ->
    Value;
validate_option(dhfile, Value) when is_list(Value), Value =/= "" ->
    list_to_binary(Value);
validate_option(psk_identity, undefined) ->
    undefined;
validate_option(psk_identity, Identity)
  when is_list(Identity), Identity =/= "", length(Identity) =< 65535 ->
    list_to_binary(Identity);
validate_option(user_lookup_fun, undefined) ->
    undefined;
validate_option(user_lookup_fun, {Fun, _} = Value) when is_function(Fun, 3) ->
   Value;
validate_option(srp_identity, undefined) ->
    undefined;
validate_option(srp_identity, {Username, Password})
  when is_list(Username), is_list(Password), Username =/= "", length(Username) =< 255 ->
    {list_to_binary(Username), list_to_binary(Password)};

validate_option(ciphers, Value)  when is_list(Value) ->
    Version = tls_record:highest_protocol_version([]),
    try cipher_suites(Version, Value)
    catch
	exit:_ ->
	    throw({error, {options, {ciphers, Value}}});
	error:_->
	    throw({error, {options, {ciphers, Value}}})
    end;
validate_option(reuse_session, Value) when is_function(Value) ->
    Value;
validate_option(reuse_sessions, Value) when Value == true; 
					    Value == false ->
    Value;

validate_option(secure_renegotiate, Value) when Value == true; 
						Value == false ->
    Value;
validate_option(renegotiate_at, Value) when is_integer(Value) ->
    erlang:min(Value, ?DEFAULT_RENEGOTIATE_AT);

validate_option(hibernate_after, undefined) ->
    undefined;
validate_option(hibernate_after, Value) when is_integer(Value), Value >= 0 ->
    Value;
validate_option(erl_dist,Value) when Value == true;
				     Value == false ->
    Value;
validate_option(client_preferred_next_protocols = Opt, {Precedence, PreferredProtocols} = Value)
  when is_list(PreferredProtocols) ->
    case tls_record:highest_protocol_version([]) of
	{3,0} ->
	    throw({error, {options, {not_supported_in_sslv3, {Opt, Value}}}});
	_ ->
	    validate_binary_list(client_preferred_next_protocols, PreferredProtocols),
	    validate_npn_ordering(Precedence),
	    {Precedence, PreferredProtocols, ?NO_PROTOCOL}
    end;
validate_option(client_preferred_next_protocols = Opt, {Precedence, PreferredProtocols, Default} = Value)
      when is_list(PreferredProtocols), is_binary(Default),
           byte_size(Default) > 0, byte_size(Default) < 256 ->
    case tls_record:highest_protocol_version([]) of
	{3,0} ->
	    throw({error, {options, {not_supported_in_sslv3, {Opt, Value}}}});
	_ ->
	    validate_binary_list(client_preferred_next_protocols, PreferredProtocols),
	    validate_npn_ordering(Precedence),
	    Value
    end;
	
validate_option(client_preferred_next_protocols, undefined) ->
    undefined;
validate_option(log_alert, Value) when Value == true;
				       Value == false ->
    Value;
validate_option(next_protocols_advertised = Opt, Value) when is_list(Value) ->
    case tls_record:highest_protocol_version([]) of
	{3,0} ->
	    throw({error, {options, {not_supported_in_sslv3, {Opt, Value}}}});
	_ ->
	    validate_binary_list(next_protocols_advertised, Value),
	    Value
    end;

validate_option(next_protocols_advertised, undefined) ->
    undefined;
validate_option(Opt, Value) ->
    throw({error, {options, {Opt, Value}}}).

validate_npn_ordering(client) ->
    ok;
validate_npn_ordering(server) ->
    ok;
validate_npn_ordering(Value) ->
    throw({error, {options, {client_preferred_next_protocols, {invalid_precedence, Value}}}}).

validate_binary_list(Opt, List) ->
    lists:foreach(
        fun(Bin) when is_binary(Bin),
                      byte_size(Bin) > 0,
                      byte_size(Bin) < 256 ->
            ok;
           (Bin) ->
            throw({error, {options, {Opt, {invalid_protocol, Bin}}}})
        end, List).

validate_versions([], Versions) ->
    Versions;
validate_versions([Version | Rest], Versions) when Version == 'tlsv1.2';
                                                   Version == 'tlsv1.1';
                                                   Version == tlsv1; 
                                                   Version == sslv3 ->
    validate_versions(Rest, Versions);					   
validate_versions([Ver| _], Versions) ->
    throw({error, {options, {Ver, {versions, Versions}}}}).

validate_inet_option(mode, Value)
  when Value =/= list, Value =/= binary ->
    throw({error, {options, {mode,Value}}});
validate_inet_option(packet, Value)
  when not (is_atom(Value) orelse is_integer(Value)) ->
    throw({error, {options, {packet,Value}}});
validate_inet_option(packet_size, Value)
  when not is_integer(Value) ->
    throw({error, {options, {packet_size,Value}}});
validate_inet_option(header, Value)
  when not is_integer(Value) ->
    throw({error, {options, {header,Value}}});
validate_inet_option(active, Value)
  when Value =/= true, Value =/= false, Value =/= once ->
    throw({error, {options, {active,Value}}});
validate_inet_option(_, _) ->
    ok.

%% The option cacerts overrides cacertsfile
ca_cert_default(_,_, [_|_]) ->
    undefined;
ca_cert_default(verify_none, _, _) ->
    undefined;
ca_cert_default(verify_peer, {Fun,_}, _) when is_function(Fun) ->
    undefined;
%% Server that wants to verify_peer and has no verify_fun must have
%% some trusted certs.
ca_cert_default(verify_peer, undefined, _) ->
    "".

emulated_options() ->
    [mode, packet, active, header, packet_size].

internal_inet_values() ->
    [{packet_size,0},{packet, 0},{header, 0},{active, false},{mode,binary}].

socket_options(InetValues) ->
    #socket_options{
		mode   = proplists:get_value(mode, InetValues, lists),
		header = proplists:get_value(header, InetValues, 0),
		active = proplists:get_value(active, InetValues, active),
		packet = proplists:get_value(packet, InetValues, 0),
		packet_size = proplists:get_value(packet_size, InetValues)	     
	       }.

emulated_options(Opts) ->
    emulated_options(Opts, internal_inet_values(), #socket_options{}).

emulated_options([{mode,Opt}|Opts], Inet, Emulated) ->
    validate_inet_option(mode,Opt),
    emulated_options(Opts, Inet, Emulated#socket_options{mode=Opt});
emulated_options([{header,Opt}|Opts], Inet, Emulated) ->
    validate_inet_option(header,Opt),
    emulated_options(Opts, Inet, Emulated#socket_options{header=Opt});
emulated_options([{active,Opt}|Opts], Inet, Emulated) ->
    validate_inet_option(active,Opt),
    emulated_options(Opts, Inet, Emulated#socket_options{active=Opt});
emulated_options([{packet,Opt}|Opts], Inet, Emulated) ->
    validate_inet_option(packet,Opt),
    emulated_options(Opts, Inet, Emulated#socket_options{packet=Opt});
emulated_options([{packet_size,Opt}|Opts], Inet, Emulated) ->
    validate_inet_option(packet_size,Opt),
    emulated_options(Opts, Inet, Emulated#socket_options{packet_size=Opt});
emulated_options([Opt|Opts], Inet, Emulated) ->
    emulated_options(Opts, [Opt|Inet], Emulated);
emulated_options([], Inet,Emulated) ->
    {Inet, Emulated}.

cipher_suites(Version, []) ->
    ssl_cipher:suites(Version);
cipher_suites(Version, [{_,_,_,_}| _] = Ciphers0) -> %% Backwards compatibility
    Ciphers = [{KeyExchange, Cipher, Hash} || {KeyExchange, Cipher, Hash, _} <- Ciphers0],
    cipher_suites(Version, Ciphers);
cipher_suites(Version, [{_,_,_}| _] = Ciphers0) ->
    Ciphers = [ssl_cipher:suite(C) || C <- Ciphers0],
    cipher_suites(Version, Ciphers);

cipher_suites(Version, [Cipher0 | _] = Ciphers0) when is_binary(Cipher0) ->
    Supported0 = ssl_cipher:suites(Version)  
	++ ssl_cipher:anonymous_suites() 
	++ ssl_cipher:psk_suites(Version)
	++ ssl_cipher:srp_suites(),
    Supported = ssl_cipher:filter_suites(Supported0),
    case [Cipher || Cipher <- Ciphers0, lists:member(Cipher, Supported)] of
	[] ->
	    Supported;
	Ciphers ->
	    Ciphers
    end;
cipher_suites(Version, [Head | _] = Ciphers0) when is_list(Head) ->
    %% Format: ["RC4-SHA","RC4-MD5"]
    Ciphers = [ssl_cipher:openssl_suite(C) || C <- Ciphers0],
    cipher_suites(Version, Ciphers); 
cipher_suites(Version, Ciphers0)  ->
    %% Format: "RC4-SHA:RC4-MD5"
    Ciphers = [ssl_cipher:openssl_suite(C) || C <- string:tokens(Ciphers0, ":")],
    cipher_suites(Version, Ciphers).

unexpected_format(Error) ->    
    lists:flatten(io_lib:format("Unexpected error: ~p", [Error])).

file_error_format({error, Error})->
    case file:format_error(Error) of 
	"unknown POSIX error" ->
	    "decoding error";
	Str ->
	    Str
    end;
file_error_format(_) ->
    "decoding error".
    
file_desc(cacertfile) ->
    "Invalid CA certificate file ";
file_desc(certfile) ->
    "Invalid certificate file ";
file_desc(keyfile) ->
    "Invalid key file "; 
file_desc(dhfile) ->
    "Invalid DH params file ".

detect(_Pred, []) ->
    undefined;
detect(Pred, [H|T]) ->
    case Pred(H) of
        true ->
            H;
        _ ->
            detect(Pred, T)
    end.

make_next_protocol_selector(undefined) ->
    undefined;
make_next_protocol_selector({client, AllProtocols, DefaultProtocol}) ->
    fun(AdvertisedProtocols) ->
        case detect(fun(PreferredProtocol) -> 
			    lists:member(PreferredProtocol, AdvertisedProtocols) 
		    end, AllProtocols) of
            undefined -> 
		DefaultProtocol;
            PreferredProtocol -> 
		PreferredProtocol
        end
    end;

make_next_protocol_selector({server, AllProtocols, DefaultProtocol}) ->
    fun(AdvertisedProtocols) ->
	    case detect(fun(PreferredProtocol) -> 
				lists:member(PreferredProtocol, AllProtocols) 
			end, 
			AdvertisedProtocols) of
		undefined -> 
		    DefaultProtocol;
            PreferredProtocol -> 
		    PreferredProtocol
	    end
    end.
