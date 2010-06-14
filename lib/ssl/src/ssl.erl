%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2010. All Rights Reserved.
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

-module(ssl).

-export([start/0, start/1, stop/0, transport_accept/1,
	 transport_accept/2, ssl_accept/1, ssl_accept/2, ssl_accept/3,
	 ciphers/0, cipher_suites/0, cipher_suites/1, close/1, shutdown/2,
	 connect/3, connect/2, connect/4, connection_info/1,
	 controlling_process/2, listen/2, pid/1, peername/1, recv/2, recv/3,
	 send/2, getopts/2, setopts/2, seed/1, sockname/1, peercert/1,
	 peercert/2, version/0, versions/0, session_info/1, format_error/1,
	 renegotiate/1]).

%% Should be deprecated as soon as old ssl is removed
%%-deprecated({pid, 1, next_major_release}).

-include("ssl_int.hrl").
-include("ssl_internal.hrl").
-include("ssl_record.hrl").

-include_lib("public_key/include/public_key.hrl"). 

-record(config, {ssl,               %% SSL parameters
		 inet_user,         %% User set inet options
		 emulated,          %% #socket_option{} emulated
		 inet_ssl,          %% inet options for internal ssl socket 
		 cb                 %% Callback info
		}).

%%--------------------------------------------------------------------
%% Function: start([, Type]) -> ok
%%
%%  Type =  permanent | transient | temporary
%%
%% Description: Starts the ssl application. Default type
%% is temporary. see application(3)
%%--------------------------------------------------------------------
start() ->
    application:start(crypto),
    application:start(public_key),
    application:start(ssl).

start(Type) ->
    application:start(crypto, Type),
    application:start(public_key, Type),
    application:start(ssl, Type).

%%--------------------------------------------------------------------
%% Function: stop() -> ok
%%
%% Description: Stops the ssl application.
%%--------------------------------------------------------------------
stop() ->
    application:stop(ssl).

%%--------------------------------------------------------------------
%% Function: connect(Address, Port, Options[, Timeout]) -> {ok, Socket}
%%
%% Description: Connect to a ssl server.
%%--------------------------------------------------------------------
connect(Socket, SslOptions) when is_port(Socket) ->
    connect(Socket, SslOptions, infinity).

connect(Socket, SslOptions0, Timeout) when is_port(Socket) ->
    EmulatedOptions = emulated_options(),
    {ok, InetValues} = inet:getopts(Socket, EmulatedOptions),
    inet:setopts(Socket, internal_inet_values()), 
    try handle_options(SslOptions0 ++ InetValues, client) of
	{ok, #config{cb=CbInfo, ssl=SslOptions, emulated=EmOpts}} ->
	    case inet:peername(Socket) of
		{ok, {Address, Port}} ->
		    ssl_connection:connect(Address, Port, Socket, 
					   {SslOptions, EmOpts},
					   self(), CbInfo, Timeout);
		{error, Error} ->
		    {error, Error}
	    end
    catch 
	_:{error, Reason} ->
            {error, Reason}
    end;

connect(Address, Port, Options) ->
    connect(Address, Port, Options, infinity).

connect(Address, Port, Options0, Timeout) ->
    case proplists:get_value(ssl_imp, Options0, new) of
        new ->
            new_connect(Address, Port, Options0, Timeout);
        old ->
	    %% Allow the option reuseaddr to be present
	    %% so that new and old ssl can be run by the same
	    %% code, however the option will be ignored by old ssl
	    %% that hardcodes reuseaddr to true in its portprogram.
	    Options1 = proplists:delete(reuseaddr, Options0),
	    Options  = proplists:delete(ssl_imp, Options1),
            old_connect(Address, Port, Options, Timeout);
	Value ->
	    {error, {eoptions, {ssl_imp, Value}}}
    end.

%%--------------------------------------------------------------------
%% Function: listen(Port, Options) -> {ok, ListenSock} | {error, Reason}
%%
%% Description: Creates a ssl listen socket.
%%--------------------------------------------------------------------
listen(_Port, []) ->
    {error, enooptions};
listen(Port, Options0) ->
    case proplists:get_value(ssl_imp, Options0, new) of
	new ->
	    new_listen(Port, Options0);
	old ->
	    %% Allow the option reuseaddr to be present
	    %% so that new and old ssl can be run by the same
	    %% code, however the option will be ignored by old ssl
	    %% that hardcodes reuseaddr to true in its portprogram.
	    Options1 = proplists:delete(reuseaddr, Options0),
	    Options  = proplists:delete(ssl_imp, Options1),
	    old_listen(Port, Options);
	Value ->
	    {error, {eoptions, {ssl_imp, Value}}}
    end.

%%--------------------------------------------------------------------
%% Function: transport_accept(ListenSocket[, Timeout]) -> {ok, Socket}.
%%
%% Description: Performs transport accept on a ssl listen socket 
%%--------------------------------------------------------------------
transport_accept(ListenSocket) ->
    transport_accept(ListenSocket, infinity).

transport_accept(#sslsocket{pid = {ListenSocket, #config{cb=CbInfo, ssl=SslOpts}},
                            fd = new_ssl}, Timeout) ->
    
    %% The setopt could have been invoked on the listen socket
    %% and options should be inherited.
    EmOptions = emulated_options(),
    {ok, InetValues} = inet:getopts(ListenSocket, EmOptions),
    ok = inet:setopts(ListenSocket, internal_inet_values()),
    {CbModule,_,_, _} = CbInfo,    
    case CbModule:accept(ListenSocket, Timeout) of
	{ok, Socket} ->
	    ok = inet:setopts(ListenSocket, InetValues),
	    {ok, Port} = inet:port(Socket),
	    ConnArgs = [server, "localhost", Port, Socket,
			{SslOpts, socket_options(InetValues)}, self(), CbInfo],
	    case ssl_connection_sup:start_child(ConnArgs) of
		{ok, Pid} ->
		    ssl_connection:socket_control(Socket, Pid, CbModule);
		{error, Reason} ->
		    {error, Reason}
	    end;
	{error, Reason} ->
	    {error, Reason}
    end;

transport_accept(#sslsocket{} = ListenSocket, Timeout) ->
    ensure_old_ssl_started(),
    {ok, Pid} = ssl_broker:start_broker(acceptor),
    ssl_broker:transport_accept(Pid, ListenSocket, Timeout).

%%--------------------------------------------------------------------
%% Function: ssl_accept(ListenSocket[, Timeout]) -> {ok, Socket} | 
%%                                                  {error, Reason}
%%
%% Description: Performs accept on a ssl listen socket. e.i. performs
%%              ssl handshake. 
%%--------------------------------------------------------------------
ssl_accept(ListenSocket) ->
    ssl_accept(ListenSocket, infinity).

ssl_accept(#sslsocket{fd = new_ssl} = Socket, Timeout) ->
    ssl_connection:handshake(Socket, Timeout);
    
ssl_accept(ListenSocket, SslOptions)  when is_port(ListenSocket) -> 
    ssl_accept(ListenSocket, SslOptions, infinity);

%% Old ssl
ssl_accept(#sslsocket{} = Socket, Timeout)  ->
    ensure_old_ssl_started(),
    ssl_broker:ssl_accept(Socket, Timeout).

ssl_accept(Socket, SslOptions, Timeout) when is_port(Socket) -> 
    EmulatedOptions = emulated_options(),
    {ok, InetValues} = inet:getopts(Socket, EmulatedOptions),
    inet:setopts(Socket, internal_inet_values()), 
    try handle_options(SslOptions ++ InetValues, server) of
	{ok, #config{cb=CbInfo,ssl=SslOpts, emulated=EmOpts}} ->
	    {ok, Port} = inet:port(Socket),
	    ssl_connection:ssl_accept(Port, Socket,
				  {SslOpts, EmOpts},
				  self(), CbInfo, Timeout)
    catch 
	Error = {error, _Reason} -> Error
    end.

%%--------------------------------------------------------------------
%% Function: close() -> ok 
%%
%% Description: Close a ssl connection
%%--------------------------------------------------------------------  
close(#sslsocket{pid = {ListenSocket, #config{cb={CbMod,_, _, _}}}, fd = new_ssl}) ->
    CbMod:close(ListenSocket);
close(#sslsocket{pid = Pid, fd = new_ssl}) ->
    ssl_connection:close(Pid);
close(Socket = #sslsocket{}) ->
    ensure_old_ssl_started(),
    ssl_broker:close(Socket).

%%--------------------------------------------------------------------
%% Function:  send(Socket, Data) -> ok | {error, Reason}
%% 
%% Description: Sends data over the ssl connection
%%--------------------------------------------------------------------
send(#sslsocket{pid = Pid, fd = new_ssl}, Data) ->
    ssl_connection:send(Pid, Data);

send(#sslsocket{} = Socket, Data) -> 
    ensure_old_ssl_started(),
    ssl_broker:send(Socket, Data).

%%--------------------------------------------------------------------
%% Function: recv(Socket, Length [,Timeout]) -> {ok, Data} | {error, reason}
%%
%% Description: Receives data when active = false
%%--------------------------------------------------------------------
recv(Socket, Length) ->
    recv(Socket, Length, infinity).
recv(#sslsocket{pid = Pid, fd = new_ssl}, Length, Timeout) ->
    ssl_connection:recv(Pid, Length, Timeout);

recv(Socket = #sslsocket{}, Length, Timeout) ->
    ensure_old_ssl_started(),
    ssl_broker:recv(Socket, Length, Timeout).

%%--------------------------------------------------------------------
%% Function: controlling_process(Socket, NewOwner) -> ok | {error, Reason}
%%
%% Description: Changes process that receives the messages when active = true
%% or once. 
%%--------------------------------------------------------------------
controlling_process(#sslsocket{pid = Pid, fd = new_ssl}, NewOwner) 
  when is_pid(Pid) ->
    ssl_connection:new_user(Pid, NewOwner);

controlling_process(Socket, NewOwner) when is_pid(NewOwner) ->
    ensure_old_ssl_started(),
    ssl_broker:controlling_process(Socket, NewOwner).

%%--------------------------------------------------------------------
%% Function: connection_info(Socket) -> {ok, {Protocol, CipherSuite}} | 
%%                                      {error, Reason}
%% Protocol = sslv3 | tlsv1 | tlsv1.1
%% CipherSuite = {KeyExchange, Chipher, Hash, Exportable}
%% 
%%
%% Description: Returns ssl protocol and cipher used for the connection
%%--------------------------------------------------------------------
connection_info(#sslsocket{pid = Pid, fd = new_ssl}) ->
    ssl_connection:info(Pid);

connection_info(#sslsocket{} = Socket) -> 
    ensure_old_ssl_started(),
    ssl_broker:connection_info(Socket).

%%--------------------------------------------------------------------
%% Function: peercert(Socket[, Opts]) -> {ok, Cert} | {error, Reason}
%%
%% Description: 
%%--------------------------------------------------------------------
peercert(Socket) ->
    peercert(Socket, []).

peercert(#sslsocket{pid = Pid, fd = new_ssl}, Opts) ->
    case ssl_connection:peer_certificate(Pid) of
	{ok, undefined} ->
	    {error, no_peercert};
        {ok, BinCert} ->
	    decode_peercert(BinCert, Opts);
        {error, Reason}  ->
            {error, Reason}
    end;

peercert(#sslsocket{} = Socket, Opts) ->
    ensure_old_ssl_started(),
    case ssl_broker:peercert(Socket) of
        {ok, Bin} ->
	    decode_peercert(Bin, Opts);
        {error, Reason}  ->
            {error, Reason}
    end.


decode_peercert(BinCert, Opts) ->
    PKOpts = [case Opt of ssl -> otp; pkix -> plain end || 
		 Opt <- Opts, Opt =:= ssl orelse Opt =:= pkix],
    case PKOpts of
	[Opt] ->
	    select_part(Opt, public_key:pkix_decode_cert(BinCert, Opt), Opts);
	[] ->
	    {ok, BinCert}
    end.

select_part(otp, {ok, Cert}, Opts) ->
    case lists:member(subject, Opts) of 
	true ->
	    TBS = Cert#'OTPCertificate'.tbsCertificate,
	    {ok, TBS#'OTPTBSCertificate'.subject};
	false ->
	    {ok, Cert}
    end;

select_part(plain, {ok, Cert}, Opts) ->
    case lists:member(subject, Opts) of 
	true ->
	    TBS = Cert#'Certificate'.tbsCertificate,
	    {ok,  TBS#'TBSCertificate'.subject};
	false ->
	    {ok, Cert}
    end.

%%--------------------------------------------------------------------
%% Function: peername(Socket) -> {ok, {Address, Port}} | {error, Reason}
%%
%% Description:
%%--------------------------------------------------------------------
peername(#sslsocket{fd = new_ssl, pid = Pid}) ->
    ssl_connection:peername(Pid);

peername(#sslsocket{} = Socket) ->
    ensure_old_ssl_started(),
    ssl_broker:peername(Socket).

%%--------------------------------------------------------------------
%% Function: cipher_suites() -> 
%%
%% Description:
%%--------------------------------------------------------------------
cipher_suites() ->
    cipher_suites(erlang).
  
cipher_suites(erlang) ->
    Version = ssl_record:highest_protocol_version([]),
    [ssl_cipher:suite_definition(S) || S <- ssl_cipher:suites(Version)];    

cipher_suites(openssl) ->
    Version = ssl_record:highest_protocol_version([]),
    [ssl_cipher:openssl_suite_name(S) || S <- ssl_cipher:suites(Version)].

%%--------------------------------------------------------------------
%% Function: getopts(Socket, OptTags) -> {ok, Options} | {error, Reason}
%% 
%% Description:
%%--------------------------------------------------------------------
getopts(#sslsocket{fd = new_ssl, pid = Pid}, OptTags) when is_pid(Pid) ->
    ssl_connection:get_opts(Pid, OptTags);
getopts(#sslsocket{fd = new_ssl, pid = {ListenSocket, _}}, OptTags) ->
    inet:getopts(ListenSocket, OptTags);
getopts(#sslsocket{} = Socket, Options) ->
    ensure_old_ssl_started(),
    ssl_broker:getopts(Socket, Options).

%%--------------------------------------------------------------------
%% Function: setopts(Socket, Options) -> ok | {error, Reason}
%% 
%% Description:
%%--------------------------------------------------------------------
setopts(#sslsocket{fd = new_ssl, pid = Pid}, Opts0) when is_pid(Pid) ->
    Opts = proplists:expand([{binary, [{mode, binary}]},
			     {list, [{mode, list}]}], Opts0),
    ssl_connection:set_opts(Pid, Opts);
setopts(#sslsocket{fd = new_ssl, pid = {ListenSocket, _}}, OptTags) ->
    inet:setopts(ListenSocket, OptTags);
setopts(#sslsocket{} = Socket, Options) ->
    ensure_old_ssl_started(),
    ssl_broker:setopts(Socket, Options).

%%---------------------------------------------------------------
%% Function: shutdown(Socket, How) -> ok | {error, Reason}
%% 
%% Description: Same as gen_tcp:shutdown/2
%%--------------------------------------------------------------------
shutdown(#sslsocket{pid = {ListenSocket, #config{cb={CbMod,_, _, _}}}, fd = new_ssl}, How) ->
    CbMod:shutdown(ListenSocket, How);
shutdown(#sslsocket{pid = Pid, fd = new_ssl}, How) ->
    ssl_connection:shutdown(Pid, How).

%%--------------------------------------------------------------------
%% Function: sockname(Socket) -> {ok, {Address, Port}} | {error, Reason}
%% 
%% Description: Same as inet:sockname/1
%%--------------------------------------------------------------------
sockname(#sslsocket{fd = new_ssl, pid = {ListenSocket, _}}) ->
    inet:sockname(ListenSocket);

sockname(#sslsocket{fd = new_ssl, pid = Pid}) ->
    ssl_connection:sockname(Pid);

sockname(#sslsocket{} = Socket) ->
    ensure_old_ssl_started(),
    ssl_broker:sockname(Socket).

%%---------------------------------------------------------------
%% Function: seed(Data) -> ok | {error, edata}
%% 
%% Description:
%%--------------------------------------------------------------------
%% TODO: crypto:seed ?
seed(Data) ->
    ensure_old_ssl_started(),
    ssl_server:seed(Data).

%%---------------------------------------------------------------
%% Function: session_id(Socket) -> {ok, PropList} | {error, Reason} 
%% 
%% Description:
%%--------------------------------------------------------------------
session_info(#sslsocket{pid = Pid, fd = new_ssl}) ->
    ssl_connection:session_info(Pid).

%%---------------------------------------------------------------
%% Function: versions() -> [{SslAppVer, SupportedSslVer, AvailableSslVsn}]
%% 
%% SslAppVer = string()  - t.ex: ssl-4.0
%% SupportedSslVer = [SslVer]
%% AvailableSslVsn = [SSLVer]
%% SSLVer = sslv3 | tlsv1  | 'tlsv1.1'
%%
%% Description: Returns a list of relevant versions.
%%--------------------------------------------------------------------
versions() ->
    Vsns = ssl_record:supported_protocol_versions(),
    SupportedVsns = [ssl_record:protocol_version(Vsn) || Vsn <- Vsns],
    AvailableVsns = ?DEFAULT_SUPPORTED_VERSIONS,
    [{ssl_app, ?VSN}, {supported, SupportedVsns}, {available, AvailableVsns}].


renegotiate(#sslsocket{pid = Pid, fd = new_ssl}) ->
    ssl_connection:renegotiation(Pid).

%%%--------------------------------------------------------------
%%% Internal functions
%%%--------------------------------------------------------------------
new_connect(Address, Port, Options, Timeout) when is_list(Options) ->
    try handle_options(Options, client) of
	{ok, Config} ->
	    do_new_connect(Address,Port,Config,Timeout)
    catch 
	throw:Error ->
	    Error
    end.

do_new_connect(Address, Port,
	       #config{cb=CbInfo, inet_user=UserOpts, ssl=SslOpts,
		       emulated=EmOpts,inet_ssl=SocketOpts},
	       Timeout) ->
    {CbModule, _, _, _} = CbInfo,    
    try CbModule:connect(Address, Port,  SocketOpts, Timeout) of
	{ok, Socket} ->
	    ssl_connection:connect(Address, Port, Socket, {SslOpts,EmOpts},
				   self(), CbInfo, Timeout);
	{error, Reason} ->
	    {error, Reason}
    catch
	exit:{function_clause, _} ->
	    {error, {eoptions, {cb_info, CbInfo}}};
	exit:{badarg, _} ->
	    {error,{eoptions, {inet_options, UserOpts}}}
    end.

old_connect(Address, Port, Options, Timeout) ->
    ensure_old_ssl_started(),
    {ok, Pid} = ssl_broker:start_broker(connector),
    ssl_broker:connect(Pid, Address, Port, Options, Timeout).

new_listen(Port, Options0) ->
    try 
	{ok, Config} = handle_options(Options0, server),
	#config{cb={CbModule, _, _, _},inet_user=Options} = Config,
	case CbModule:listen(Port, Options) of
	    {ok, ListenSocket} ->
		{ok, #sslsocket{pid = {ListenSocket, Config}, fd = new_ssl}};
	    Err = {error, _} ->
		Err
	end
    catch 
	Error = {error, _} ->
	    Error
    end.
	    
old_listen(Port, Options) ->
    ensure_old_ssl_started(),
    {ok, Pid} = ssl_broker:start_broker(listener),
    ssl_broker:listen(Pid, Port, Options).

handle_options(Opts0, Role) ->
    Opts = proplists:expand([{binary, [{mode, binary}]},
			     {list, [{mode, list}]}], Opts0),
    
    ReuseSessionFun = fun(_, _, _, _) -> true end,

    AcceptBadCa = fun({bad_cert,unknown_ca}, Acc) ->  Acc;
		     (Other, Acc) -> [Other | Acc]
		  end,
    
    VerifyFun =
	fun(ErrorList) ->
		case lists:foldl(AcceptBadCa, [], ErrorList) of
		    [] ->    true;
		    [_|_] -> false
		end
	end,

    UserFailIfNoPeerCert = validate_option(fail_if_no_peer_cert, 
					   proplists:get_value(fail_if_no_peer_cert, Opts, false)),

    {Verify, FailIfNoPeerCert, CaCertDefault} = 
	%% Handle 0, 1, 2 for backwards compatibility
	case proplists:get_value(verify, Opts, verify_none) of
	    0 ->
		{verify_none, false, ca_cert_default(verify_none, Role)};
	    1  ->
		{verify_peer, false, ca_cert_default(verify_peer, Role)};
	    2 ->
		{verify_peer, true,  ca_cert_default(verify_peer, Role)};
	    verify_none ->
		{verify_none, false, ca_cert_default(verify_none, Role)};
	    verify_peer ->
		{verify_peer, UserFailIfNoPeerCert, ca_cert_default(verify_peer, Role)};
	    Value ->
		throw({error, {eoptions, {verify, Value}}})
	end,   

    CertFile = handle_option(certfile, Opts, ""),
    
    SSLOptions = #ssl_options{
      versions   = handle_option(versions, Opts, []),
      verify     = validate_option(verify, Verify),
      verify_fun = handle_option(verify_fun, Opts, VerifyFun),
      fail_if_no_peer_cert = FailIfNoPeerCert,
      verify_client_once =  handle_option(verify_client_once, Opts, false),
      validate_extensions_fun = handle_option(validate_extensions_fun, Opts, undefined),
      depth      = handle_option(depth,  Opts, 1),
      certfile   = CertFile,
      keyfile    = handle_option(keyfile,  Opts, CertFile),
      key        = handle_option(key, Opts, undefined),
      password   = handle_option(password, Opts, ""),
      cacertfile = handle_option(cacertfile, Opts, CaCertDefault),
      dhfile     = handle_option(dhfile, Opts, undefined),
      ciphers    = handle_option(ciphers, Opts, []),
      %% Server side option
      reuse_session = handle_option(reuse_session, Opts, ReuseSessionFun),
      reuse_sessions = handle_option(reuse_sessions, Opts, true),
      secure_renegotiate = handle_option(secure_renegotiate, Opts, false),
      renegotiate_at = handle_option(renegotiate_at, Opts, ?DEFAULT_RENEGOTIATE_AT),
      debug      = handle_option(debug, Opts, [])
     },

    CbInfo  = proplists:get_value(cb_info, Opts, {gen_tcp, tcp, tcp_closed, tcp_error}),    
    SslOptions = [versions, verify, verify_fun, validate_extensions_fun, 
		  fail_if_no_peer_cert, verify_client_once,
		  depth, certfile, keyfile,
		  key, password, cacertfile, dhfile, ciphers,
		  debug, reuse_session, reuse_sessions, ssl_imp,
		  cb_info, renegotiate_at, secure_renegotiate],
    
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
validate_option(ssl_imp, Value) when Value == new; Value == old ->
    Value;
validate_option(verify, Value) 
  when Value == verify_none; Value == verify_peer ->
    Value;
validate_option(verify_fun, Value) when is_function(Value) ->
   Value;
validate_option(fail_if_no_peer_cert, Value) 
  when Value == true; Value == false ->
    Value;
validate_option(verify_client_once, Value) 
  when Value == true; Value == false ->
    Value;

validate_option(validate_extensions_fun, Value) when Value == undefined; is_function(Value) ->
    Value;
validate_option(depth, Value) when is_integer(Value), 
                                   Value >= 0, Value =< 255->
    Value;
validate_option(certfile, Value) when is_list(Value) ->
    Value;
validate_option(keyfile, Value) when is_list(Value) ->
    Value;
validate_option(key, Value) when Value == undefined;
                                 is_tuple(Value) ->
    %% element(1, Value)=='RSAPrivateKey' ->
    Value;
validate_option(password, Value) when is_list(Value) ->
    Value;

%% certfile must be present in some cases otherwhise it can be set
%% to the empty string.
validate_option(cacertfile, undefined) ->
    "";
validate_option(cacertfile, Value) when is_list(Value), Value =/= "" ->
    Value;
validate_option(dhfile, undefined = Value)  ->
    Value;
validate_option(dhfile, Value) when is_list(Value), Value =/= "" ->
    Value;
validate_option(ciphers, Value)  when is_list(Value) ->
    Version = ssl_record:highest_protocol_version([]),
    try cipher_suites(Version, Value)
    catch
	exit:_ ->
	    throw({error, {eoptions, {ciphers, Value}}});
	error:_->
	    throw({error, {eoptions, {ciphers, Value}}})
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
    min(Value, ?DEFAULT_RENEGOTIATE_AT);

validate_option(debug, Value) when is_list(Value); Value == true ->
    Value;
validate_option(Opt, Value) ->
    throw({error, {eoptions, {Opt, Value}}}).
    
validate_versions([], Versions) ->
    Versions;
validate_versions([Version | Rest], Versions) when Version == 'tlsv1.1'; 
                                                   Version == tlsv1; 
                                                   Version == sslv3 ->
    validate_versions(Rest, Versions);					   
validate_versions([Ver| _], Versions) ->
    throw({error, {eoptions, {Ver, {versions, Versions}}}}).

validate_inet_option(mode, Value)
  when Value =/= list, Value =/= binary ->
    throw({error, {eoptions, {mode,Value}}});
validate_inet_option(packet, Value)
  when not (is_atom(Value) orelse is_integer(Value)) ->
    throw({error, {eoptions, {packet,Value}}});
validate_inet_option(packet_size, Value)
  when not is_integer(Value) ->
    throw({error, {eoptions, {packet_size,Value}}});
validate_inet_option(header, Value)
  when not is_integer(Value) ->
    throw({error, {eoptions, {header,Value}}});
validate_inet_option(active, Value)
  when Value =/= true, Value =/= false, Value =/= once ->
    throw({error, {eoptions, {active,Value}}});
validate_inet_option(_, _) ->
    ok.

ca_cert_default(verify_none, _) ->
    undefined;
%% Client may leave verification up to the user
ca_cert_default(verify_peer, client) ->
    undefined;
%% Server that wants to verify_peer must have
%% some trusted certs.
ca_cert_default(verify_peer, server) ->
    "".

emulated_options() ->
    [mode, packet, active, header, packet_size].

internal_inet_values() ->
    [{packet_size,0},{packet, 0},{header, 0},{active, false},{mode,binary}].
    %%[{packet, ssl},{header, 0},{active, false},{mode,binary}].

socket_options(InetValues) ->
    #socket_options{
		mode   = proplists:get_value(mode, InetValues),
		header = proplists:get_value(header, InetValues),
		active = proplists:get_value(active, InetValues),
		packet = proplists:get_value(packet, InetValues),
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
    Supported = ssl_cipher:suites(Version),
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

format_error({error, Reason}) ->
    format_error(Reason);
format_error(Reason) when is_list(Reason) ->
    Reason;
format_error(closed) ->
    "The connection is closed";
format_error(ecacertfile) ->
    "Own CA certificate file is invalid.";
format_error(ecertfile) ->
    "Own certificate file is invalid.";
format_error(ekeyfile) ->
    "Own private key file is invalid.";
format_error(esslaccept) ->
    "Server SSL handshake procedure between client and server failed.";
format_error(esslconnect) ->
    "Client SSL handshake procedure between client and server failed.";
format_error({eoptions, Options}) ->
    lists:flatten(io_lib:format("Error in options list: ~p~n", [Options]));

%%%%%%%%%%%%  START OLD SSL format_error %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
format_error(ebadsocket) ->
    "Connection not found (internal error).";
format_error(ebadstate) ->
    "Connection not in connect state (internal error).";
format_error(ebrokertype) ->
    "Wrong broker type (internal error).";
format_error(echaintoolong) ->
    "The chain of certificates provided by peer is too long.";
format_error(ecipher) ->
    "Own list of specified ciphers is invalid.";
format_error(ekeymismatch) ->
    "Own private key does not match own certificate.";
format_error(enoissuercert) ->
    "Cannot find certificate of issuer of certificate provided by peer.";
format_error(enoservercert) ->
    "Attempt to do accept without having set own certificate.";
format_error(enotlistener) ->
    "Attempt to accept on a non-listening socket.";
format_error(enoproxysocket) ->
    "No proxy socket found (internal error or max number of file "
        "descriptors exceeded).";
format_error(enooptions) ->
    "List of options is empty.";
format_error(enotstarted) ->
    "The SSL application has not been started.";
format_error(eoptions) ->
    "Invalid list of options.";
format_error(epeercert) ->
    "Certificate provided by peer is in error.";
format_error(epeercertexpired) ->
    "Certificate provided by peer has expired.";
format_error(epeercertinvalid) ->
    "Certificate provided by peer is invalid.";
format_error(eselfsignedcert) ->
    "Certificate provided by peer is self signed.";
format_error(esslerrssl) ->
    "SSL protocol failure. Typically because of a fatal alert from peer.";
format_error(ewantconnect) ->
    "Protocol wants to connect, which is not supported in this "
        "version of the SSL application.";
format_error(ex509lookup) ->
    "Protocol wants X.509 lookup, which is not supported in this "
        "version of the SSL application.";
format_error({badcall, _Call}) ->
    "Call not recognized for current mode (active or passive) and state "
        "of socket.";
format_error({badcast, _Cast}) ->
    "Call not recognized for current mode (active or passive) and state "
        "of socket."; 

format_error({badinfo, _Info}) ->
    "Call not recognized for current mode (active or passive) and state "
        "of socket.";

%%%%%%%%%%%%%%%%%% END OLD SSL format_error %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

format_error(Error) ->
    case (catch inet:format_error(Error)) of
        "unkknown POSIX" ++ _ ->
            no_format(Error);
        {'EXIT', _} ->
            no_format(Error);
        Other ->
            Other
    end.

no_format(Error) ->    
    lists:flatten(io_lib:format("No format string for error: \"~p\" available.", [Error])).

%% Start old ssl port program if needed.
ensure_old_ssl_started() ->
    case whereis(ssl_server) of
	undefined ->
	    (catch supervisor:start_child(ssl_sup, 
				   {ssl_server, {ssl_server, start_link, []},
				    permanent, 2000, worker, [ssl_server]}));
	_ ->
	    ok
    end.

%%%%%%%%%%%%%%%% Deprecated %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ciphers() -> 
    ensure_old_ssl_started(),
    case (catch ssl_server:ciphers()) of
        {'EXIT', _} ->
            {error, enotstarted};
        Res = {ok, _}  ->
            Res
    end.

version() -> 
    ensure_old_ssl_started(),
    SSLVsn = ?VSN,
    {CompVsn, LibVsn} = case (catch ssl_server:version()) of
                            {'EXIT', _} ->
                                {"", ""};
                            {ok, Vsns}  ->
                                Vsns
                        end,
    {ok, {SSLVsn, CompVsn, LibVsn}}.

min(N,M) when N < M ->
    N;
min(_, M) ->
    M.
                                
%% Only used to remove exit messages from old ssl
%% First is a nonsense clause to provide some
%% backward compability for orber that uses this
%% function in a none recommended way, but will
%% work correctly if a valid pid is returned.
pid(#sslsocket{fd = new_ssl}) ->
    whereis(ssl_connection_sup);
pid(#sslsocket{pid = Pid}) ->
    Pid.
