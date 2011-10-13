%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2011. All Rights Reserved.
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
	 cipher_suites/0, cipher_suites/1, close/1, shutdown/2,
	 connect/3, connect/2, connect/4, connection_info/1,
	 controlling_process/2, listen/2, pid/1, peername/1, peercert/1,
	 recv/2, recv/3, send/2, getopts/2, setopts/2, sockname/1,
	 versions/0, session_info/1, format_error/1,
	 renegotiate/1]).

-deprecated({pid, 1, next_major_release}).

-include("ssl_internal.hrl").
-include("ssl_record.hrl").
-include("ssl_cipher.hrl").

-include_lib("public_key/include/public_key.hrl"). 

-record(config, {ssl,               %% SSL parameters
		 inet_user,         %% User set inet options
		 emulated,          %% #socket_option{} emulated
		 inet_ssl,          %% inet options for internal ssl socket 
		 cb                 %% Callback info
		}).
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
                        {ciphers, ciphers()} | {ssl_imp, ssl_imp()} | {reuse_sessions, boolean()} |
                        {reuse_session, fun()} | {hibernate_after, integer()|undefined}.

-type verify_type()  :: verify_none | verify_peer.
-type path()         :: string().
-type ciphers()      :: [erl_cipher_suite()] |
			string(). % (according to old API)
-type ssl_imp()      :: new | old.

-type transport_option() :: {cb_info, {CallbackModule::atom(), DataTag::atom(), ClosedTag::atom()}}.


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
    application:start(public_key),
    application:start(ssl).

start(Type) ->
    application:start(crypto, Type),
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
-spec connect(host() | port(), [connect_option()] | inet:port_number(), timeout() | list()) ->
		     {ok, #sslsocket{}} | {error, reason()}.
-spec connect(host() | port(), inet:port_number(), list(), timeout()) ->
		     {ok, #sslsocket{}} | {error, reason()}.

%%
%% Description: Connect to an ssl server.
%%--------------------------------------------------------------------
connect(Socket, SslOptions) when is_port(Socket) ->
    connect(Socket, SslOptions, infinity).

connect(Socket, SslOptions0, Timeout) when is_port(Socket) ->
    EmulatedOptions = emulated_options(),
    {ok, InetValues} = inet:getopts(Socket, EmulatedOptions),
    ok = inet:setopts(Socket, internal_inet_values()),
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
    {error, enooptions};
listen(Port, Options0) ->
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

transport_accept(#sslsocket{pid = {ListenSocket, #config{cb=CbInfo, ssl=SslOpts}}}, Timeout) ->
    
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
    end.

%%--------------------------------------------------------------------
-spec ssl_accept(#sslsocket{}) -> ok | {error, reason()}.
-spec ssl_accept(#sslsocket{} | port(), timeout()| [ssl_option() | transport_option()]) ->
			ok | {ok, #sslsocket{}} | {error, reason()}.
-spec ssl_accept(port(), [ssl_option()| transport_option()], timeout()) -> {ok, #sslsocket{}} | {error, reason()}.
%%
%% Description: Performs accept on an ssl listen socket. e.i. performs
%%              ssl handshake. 
%%--------------------------------------------------------------------
ssl_accept(ListenSocket) ->
    ssl_accept(ListenSocket, infinity).

ssl_accept(#sslsocket{} = Socket, Timeout) ->
    ssl_connection:handshake(Socket, Timeout);
    
ssl_accept(ListenSocket, SslOptions)  when is_port(ListenSocket) -> 
    ssl_accept(ListenSocket, SslOptions, infinity).

ssl_accept(Socket, SslOptions, Timeout) when is_port(Socket) -> 
    EmulatedOptions = emulated_options(),
    {ok, InetValues} = inet:getopts(Socket, EmulatedOptions),
    ok = inet:setopts(Socket, internal_inet_values()),
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
-spec  close(#sslsocket{}) -> term().
%%
%% Description: Close an ssl connection
%%--------------------------------------------------------------------  
close(#sslsocket{pid = {ListenSocket, #config{cb={CbMod,_, _, _}}}}) ->
    CbMod:close(ListenSocket);
close(#sslsocket{pid = Pid}) ->
    ssl_connection:close(Pid).

%%--------------------------------------------------------------------
-spec send(#sslsocket{}, iodata()) -> ok | {error, reason()}.
%% 
%% Description: Sends data over the ssl connection
%%--------------------------------------------------------------------
send(#sslsocket{pid = Pid}, Data) ->
    ssl_connection:send(Pid, Data).

%%--------------------------------------------------------------------
-spec recv(#sslsocket{}, integer()) -> {ok, binary()| list()} | {error, reason()}.
-spec recv(#sslsocket{}, integer(), timeout()) -> {ok, binary()| list()} | {error, reason()}.
%%
%% Description: Receives data when active = false
%%--------------------------------------------------------------------
recv(Socket, Length) ->
    recv(Socket, Length, infinity).
recv(#sslsocket{pid = Pid, fd = new_ssl}, Length, Timeout) ->
    ssl_connection:recv(Pid, Length, Timeout).

%%--------------------------------------------------------------------
-spec controlling_process(#sslsocket{}, pid()) -> ok | {error, reason()}.
%%				 
%% Description: Changes process that receives the messages when active = true
%% or once. 
%%--------------------------------------------------------------------
controlling_process(#sslsocket{pid = Pid}, NewOwner) when is_pid(Pid) ->
    ssl_connection:new_user(Pid, NewOwner).

%%--------------------------------------------------------------------
-spec connection_info(#sslsocket{}) -> 	{ok, {tls_atom_version(), erl_cipher_suite()}} | 
					{error, reason()}.
%%
%% Description: Returns ssl protocol and cipher used for the connection
%%--------------------------------------------------------------------
connection_info(#sslsocket{pid = Pid}) ->
    ssl_connection:info(Pid).

%%--------------------------------------------------------------------
-spec peername(#sslsocket{}) -> {ok, {inet:ip_address(), inet:port_number()}} | {error, reason()}.
%%
%% Description: same as inet:peername/1.
%%--------------------------------------------------------------------
peername(#sslsocket{pid = Pid}) ->
    ssl_connection:peername(Pid).

%%--------------------------------------------------------------------
-spec peercert(#sslsocket{}) ->{ok, DerCert::binary()} | {error, reason()}.
%%
%% Description: Returns the peercert.
%%--------------------------------------------------------------------
peercert(#sslsocket{pid = Pid}) ->
    case ssl_connection:peer_certificate(Pid) of
	{ok, undefined} ->
	    {error, no_peercert};
        Result ->
	    Result
    end.

%%--------------------------------------------------------------------
-spec cipher_suites() -> [erl_cipher_suite()].
-spec cipher_suites(erlang | openssl) -> [erl_cipher_suite()] | [string()].
			   
%% Description: Returns all supported cipher suites.
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
-spec getopts(#sslsocket{}, [gen_tcp:option_name()]) ->
		     {ok, [gen_tcp:option()]} | {error, reason()}.
%% 
%% Description: Gets options
%%--------------------------------------------------------------------
getopts(#sslsocket{pid = Pid}, OptionTags) when is_pid(Pid), is_list(OptionTags) ->
    ssl_connection:get_opts(Pid, OptionTags);
getopts(#sslsocket{pid = {ListenSocket, _}}, OptionTags) when is_list(OptionTags) ->
    try inet:getopts(ListenSocket, OptionTags) of
	{ok, _} = Result ->
	    Result;
	{error, InetError} ->
	    {error, {eoptions, {inet_options, OptionTags, InetError}}}
    catch
	_:_ ->
	    {error, {eoptions, {inet_options, OptionTags}}}
    end;
getopts(#sslsocket{}, OptionTags) ->
    {error, {eoptions, {inet_options, OptionTags}}}.

%%--------------------------------------------------------------------
-spec setopts(#sslsocket{},  [gen_tcp:option()]) -> ok | {error, reason()}.
%% 
%% Description: Sets options
%%--------------------------------------------------------------------
setopts(#sslsocket{pid = Pid}, Options0) when is_pid(Pid), is_list(Options0)  ->
    try proplists:expand([{binary, [{mode, binary}]},
			  {list, [{mode, list}]}], Options0) of
	Options ->
	    ssl_connection:set_opts(Pid, Options)
    catch
	_:_ ->
	    {error, {eoptions, {not_a_proplist, Options0}}}
    end;

setopts(#sslsocket{pid = {ListenSocket, _}}, Options) when is_list(Options) ->
    try inet:setopts(ListenSocket, Options) of
	ok ->
	    ok;
	{error, InetError} ->
	    {error, {eoptions, {inet_options, Options, InetError}}}
    catch
	_:Error ->
	    {error, {eoptions, {inet_options, Options, Error}}}
    end;
setopts(#sslsocket{}, Options) ->
    {error, {eoptions,{not_a_proplist, Options}}}.

%%---------------------------------------------------------------
-spec shutdown(#sslsocket{}, read | write | read_write) ->  ok | {error, reason()}.
%%		      
%% Description: Same as gen_tcp:shutdown/2
%%--------------------------------------------------------------------
shutdown(#sslsocket{pid = {ListenSocket, #config{cb={CbMod,_, _, _}}}}, How) ->
    CbMod:shutdown(ListenSocket, How);
shutdown(#sslsocket{pid = Pid}, How) ->
    ssl_connection:shutdown(Pid, How).

%%--------------------------------------------------------------------
-spec sockname(#sslsocket{}) -> {ok, {inet:ip_address(), inet:port_number()}} | {error, reason()}.
%%		     
%% Description: Same as inet:sockname/1
%%--------------------------------------------------------------------
sockname(#sslsocket{pid = {ListenSocket, _}}) ->
    inet:sockname(ListenSocket);

sockname(#sslsocket{pid = Pid}) ->
    ssl_connection:sockname(Pid).

%%---------------------------------------------------------------
-spec session_info(#sslsocket{}) -> {ok, list()} | {error, reason()}.
%% 
%% Description: Returns list of session info currently [{session_id, session_id(),
%% {cipher_suite, cipher_suite()}]
%%--------------------------------------------------------------------
session_info(#sslsocket{pid = Pid, fd = new_ssl}) ->
    ssl_connection:session_info(Pid).

%%---------------------------------------------------------------
-spec versions() -> [{ssl_app, string()} | {supported, [tls_atom_version()]} | 
		      {available, [tls_atom_version()]}]. 
%%
%% Description: Returns a list of relevant versions.
%%--------------------------------------------------------------------
versions() ->
    Vsns = ssl_record:supported_protocol_versions(),
    SupportedVsns = [ssl_record:protocol_version(Vsn) || Vsn <- Vsns],
    AvailableVsns = ?DEFAULT_SUPPORTED_VERSIONS,
    [{ssl_app, ?VSN}, {supported, SupportedVsns}, {available, AvailableVsns}].


%%---------------------------------------------------------------
-spec renegotiate(#sslsocket{}) -> ok | {error, reason()}.
%% 
%% Description: Initiates a renegotiation.
%%--------------------------------------------------------------------
renegotiate(#sslsocket{pid = Pid, fd = new_ssl}) ->
    ssl_connection:renegotiation(Pid).

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

format_error(Error) ->
    case (catch inet:format_error(Error)) of
        "unkknown POSIX" ++ _ ->
            no_format(Error);
        {'EXIT', _} ->
            no_format(Error);
        Other ->
            Other
    end.

%%%--------------------------------------------------------------
%%% Internal functions
%%%--------------------------------------------------------------------
do_connect(Address, Port,
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
	exit:badarg ->
	    {error, {eoptions, {inet_options, UserOpts}}};
	exit:{badarg, _} ->
	    {error, {eoptions, {inet_options, UserOpts}}}
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
		throw({error, {eoptions, {verify, Value}}})
	end,

    CertFile = handle_option(certfile, Opts, ""),
    
    SSLOptions = #ssl_options{
      versions   = handle_option(versions, Opts, []),
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
      ciphers    = handle_option(ciphers, Opts, []),
      %% Server side option
      reuse_session = handle_option(reuse_session, Opts, ReuseSessionFun),
      reuse_sessions = handle_option(reuse_sessions, Opts, true),
      secure_renegotiate = handle_option(secure_renegotiate, Opts, false),
      renegotiate_at = handle_option(renegotiate_at, Opts, ?DEFAULT_RENEGOTIATE_AT),
      debug      = handle_option(debug, Opts, []),
      hibernate_after = handle_option(hibernate_after, Opts, undefined),
      erl_dist = handle_option(erl_dist, Opts, false)
     },

    CbInfo  = proplists:get_value(cb_info, Opts, {gen_tcp, tcp, tcp_closed, tcp_error}),    
    SslOptions = [versions, verify, verify_fun,
		  fail_if_no_peer_cert, verify_client_once,
		  depth, cert, certfile, key, keyfile,
		  password, cacerts, cacertfile, dh, dhfile, ciphers,
		  debug, reuse_session, reuse_sessions, ssl_imp,
		  cb_info, renegotiate_at, secure_renegotiate, hibernate_after, erl_dist],
    
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
validate_option(certfile, Value) when Value == undefined; is_list(Value) ->
    Value;

validate_option(key, undefined) ->
    undefined;
validate_option(key, {KeyType, Value}) when is_binary(Value),
					    KeyType == rsa;
					    KeyType == dsa ->
    {KeyType, Value};
validate_option(keyfile, Value) when is_list(Value) ->
    Value;
validate_option(password, Value) when is_list(Value) ->
    Value;

validate_option(cacerts, Value) when Value == undefined;
				     is_list(Value) ->
    Value;
%% certfile must be present in some cases otherwhise it can be set
%% to the empty string.
validate_option(cacertfile, undefined) ->
    "";
validate_option(cacertfile, Value) when is_list(Value), Value =/= "" ->
    Value;
validate_option(dh, Value) when Value == undefined;
				is_binary(Value) ->
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
    erlang:min(Value, ?DEFAULT_RENEGOTIATE_AT);

validate_option(debug, Value) when is_list(Value); Value == true ->
    Value;
validate_option(hibernate_after, undefined) ->
    undefined;
validate_option(hibernate_after, Value) when is_integer(Value), Value >= 0 ->
    Value;
validate_option(erl_dist,Value) when Value == true; 
				     Value == false ->
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
    Supported = ssl_cipher:suites(Version) ++ ssl_cipher:anonymous_suites(),
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

no_format(Error) ->    
    lists:flatten(io_lib:format("No format string for error: \"~p\" available.", [Error])).
                                
%% Only used to remove exit messages from old ssl
%% First is a nonsense clause to provide some
%% backward compatibility for orber that uses this
%% function in a none recommended way, but will
%% work correctly if a valid pid is returned.
%% Deprcated to be removed in r16
pid(#sslsocket{fd = new_ssl}) ->
     whereis(ssl_connection_sup);
pid(#sslsocket{pid = Pid}) ->
     Pid.
