%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2016. All Rights Reserved.
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

%%% Purpose : Main API module for SSL see also tls.erl and dtls.erl

-module(ssl).
-include("ssl_internal.hrl").
-include_lib("public_key/include/public_key.hrl").

%% Application handling
-export([start/0, start/1, stop/0, clear_pem_cache/0]).

%% Socket handling
-export([connect/3, connect/2, connect/4,
	 listen/2, transport_accept/1, transport_accept/2,
	 ssl_accept/1, ssl_accept/2, ssl_accept/3,
	 controlling_process/2, peername/1, peercert/1, sockname/1,
	 close/1, close/2, shutdown/2, recv/2, recv/3, send/2,
	 getopts/2, setopts/2, getstat/1, getstat/2
	]).
%% SSL/TLS protocol handling
-export([cipher_suites/0, cipher_suites/1,
	 connection_info/1, versions/0, session_info/1, format_error/1,
     renegotiate/1, prf/5, negotiated_protocol/1, negotiated_next_protocol/1,
	 connection_information/1, connection_information/2]).
%% Misc
-export([handle_options/2]).

-deprecated({negotiated_next_protocol, 1, next_major_release}).
-deprecated({connection_info, 1, next_major_release}).

-include("ssl_api.hrl").
-include("ssl_internal.hrl").
-include("ssl_record.hrl").
-include("ssl_cipher.hrl").
-include("ssl_handshake.hrl").
-include("ssl_srp.hrl").

-include_lib("public_key/include/public_key.hrl"). 

%%--------------------------------------------------------------------
-spec start() -> ok  | {error, reason()}.
-spec start(permanent | transient | temporary) -> ok | {error, reason()}.
%%
%% Description: Utility function that starts the ssl and applications
%% that it depends on.
%% see application(3)
%%--------------------------------------------------------------------
start() ->
    start(temporary).
start(Type) ->
    case application:ensure_all_started(ssl, Type) of
	{ok, _} ->
	    ok;
	Other ->
	    Other
    end.
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

connect(Socket, SslOptions0, Timeout) when is_port(Socket),
					    (is_integer(Timeout) andalso Timeout >= 0) or (Timeout == infinity) ->
    {Transport,_,_,_} = proplists:get_value(cb_info, SslOptions0,
					      {gen_tcp, tcp, tcp_closed, tcp_error}),
    EmulatedOptions = ssl_socket:emulated_options(),
    {ok, SocketValues} = ssl_socket:getopts(Transport, Socket, EmulatedOptions),
    try handle_options(SslOptions0 ++ SocketValues, client) of
	{ok, #config{transport_info = CbInfo, ssl = SslOptions, emulated = EmOpts,
		     connection_cb = ConnectionCb}} ->

	    ok = ssl_socket:setopts(Transport, Socket, ssl_socket:internal_inet_values()),
	    case ssl_socket:peername(Transport, Socket) of
		{ok, {Address, Port}} ->
		    ssl_connection:connect(ConnectionCb, Address, Port, Socket,
					   {SslOptions, emulated_socket_options(EmOpts, #socket_options{}), undefined},
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

connect(Host, Port, Options, Timeout) when (is_integer(Timeout) andalso Timeout >= 0) or (Timeout == infinity) ->
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
	ConnectionCb = connection_cb(Options0),
	#config{transport_info = {Transport, _, _, _}, inet_user = Options, connection_cb = ConnectionCb,
		ssl = SslOpts, emulated = EmOpts} = Config,
	case Transport:listen(Port, Options) of
	    {ok, ListenSocket} ->
		ok = ssl_socket:setopts(Transport, ListenSocket, ssl_socket:internal_inet_values()),
		{ok, Tracker} = ssl_socket:inherit_tracker(ListenSocket, EmOpts, SslOpts),
		{ok, #sslsocket{pid = {ListenSocket, Config#config{emulated = Tracker}}}};
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

transport_accept(#sslsocket{pid = {ListenSocket,
				   #config{transport_info =  {Transport,_,_, _} =CbInfo,
					   connection_cb = ConnectionCb,
					   ssl = SslOpts,
					   emulated = Tracker}}}, Timeout) when (is_integer(Timeout) andalso Timeout >= 0) or (Timeout == infinity) ->
    case Transport:accept(ListenSocket, Timeout) of
	{ok, Socket} ->
	    {ok, EmOpts} = ssl_socket:get_emulated_opts(Tracker),
	    {ok, Port} = ssl_socket:port(Transport, Socket),
	    ConnArgs = [server, "localhost", Port, Socket,
			{SslOpts, emulated_socket_options(EmOpts, #socket_options{}), Tracker}, self(), CbInfo],
	    ConnectionSup = connection_sup(ConnectionCb),
	    case ConnectionSup:start_child(ConnArgs) of
		{ok, Pid} ->
		    ssl_connection:socket_control(ConnectionCb, Socket, Pid, Transport, Tracker);
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

-spec ssl_accept(#sslsocket{} | port(), [ssl_option()] | [ssl_option()| transport_option()], timeout()) ->
			{ok, #sslsocket{}} | {error, reason()}.
%%
%% Description: Performs accept on an ssl listen socket. e.i. performs
%%              ssl handshake.
%%--------------------------------------------------------------------
ssl_accept(ListenSocket) ->
    ssl_accept(ListenSocket, infinity).

ssl_accept(#sslsocket{} = Socket, Timeout) when  (is_integer(Timeout) andalso Timeout >= 0) or (Timeout == infinity) ->
    ssl_connection:handshake(Socket, Timeout);

ssl_accept(ListenSocket, SslOptions)  when is_port(ListenSocket) ->
    ssl_accept(ListenSocket, SslOptions, infinity).

ssl_accept(#sslsocket{} = Socket, [], Timeout) when (is_integer(Timeout) andalso Timeout >= 0) or (Timeout == infinity)->
    ssl_accept(#sslsocket{} = Socket, Timeout);
ssl_accept(#sslsocket{fd = {_, _, _, Tracker}} = Socket, SslOpts0, Timeout) when
      (is_integer(Timeout) andalso Timeout >= 0) or (Timeout == infinity)->
    try
	{ok, EmOpts, InheritedSslOpts} = ssl_socket:get_all_opts(Tracker),
	SslOpts = handle_options(SslOpts0, InheritedSslOpts),
	ssl_connection:handshake(Socket, {SslOpts, emulated_socket_options(EmOpts, #socket_options{})}, Timeout)
    catch
	Error = {error, _Reason} -> Error
    end;
ssl_accept(Socket, SslOptions, Timeout) when is_port(Socket),
					     (is_integer(Timeout) andalso Timeout >= 0) or (Timeout == infinity) ->
    {Transport,_,_,_} =
	proplists:get_value(cb_info, SslOptions, {gen_tcp, tcp, tcp_closed, tcp_error}),
    EmulatedOptions = ssl_socket:emulated_options(),
    {ok, SocketValues} = ssl_socket:getopts(Transport, Socket, EmulatedOptions),
    ConnetionCb = connection_cb(SslOptions),
    try handle_options(SslOptions ++ SocketValues, server) of
	{ok, #config{transport_info = CbInfo, ssl = SslOpts, emulated = EmOpts}} ->
	    ok = ssl_socket:setopts(Transport, Socket, ssl_socket:internal_inet_values()),
	    {ok, Port} = ssl_socket:port(Transport, Socket),
	    ssl_connection:ssl_accept(ConnetionCb, Port, Socket,
				      {SslOpts, emulated_socket_options(EmOpts, #socket_options{}), undefined},
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
    ssl_connection:close(Pid, {close, ?DEFAULT_TIMEOUT});
close(#sslsocket{pid = {ListenSocket, #config{transport_info={Transport,_, _, _}}}}) ->
    Transport:close(ListenSocket).

%%--------------------------------------------------------------------
-spec  close(#sslsocket{}, timeout() | {pid(), integer()}) -> term().
%%
%% Description: Close an ssl connection
%%--------------------------------------------------------------------
close(#sslsocket{pid = TLSPid},
      {Pid, Timeout} = DownGrade) when is_pid(TLSPid),
				       is_pid(Pid),
				       (is_integer(Timeout) andalso Timeout >= 0) or (Timeout == infinity) ->
    ssl_connection:close(TLSPid, {close, DownGrade});
close(#sslsocket{pid = TLSPid}, Timeout) when is_pid(TLSPid),
					      (is_integer(Timeout) andalso Timeout >= 0) or (Timeout == infinity) ->
    ssl_connection:close(TLSPid, {close, Timeout});
close(#sslsocket{pid = {ListenSocket, #config{transport_info={Transport,_, _, _}}}}, _) ->
    Transport:close(ListenSocket).

%%--------------------------------------------------------------------
-spec send(#sslsocket{}, iodata()) -> ok | {error, reason()}.
%%
%% Description: Sends data over the ssl connection
%%--------------------------------------------------------------------
send(#sslsocket{pid = Pid}, Data) when is_pid(Pid) ->
    ssl_connection:send(Pid, Data);
send(#sslsocket{pid = {ListenSocket, #config{transport_info={Transport, _, _, _}}}}, Data) ->
    Transport:send(ListenSocket, Data). %% {error,enotconn}

%%--------------------------------------------------------------------
-spec recv(#sslsocket{}, integer()) -> {ok, binary()| list()} | {error, reason()}.
-spec recv(#sslsocket{}, integer(), timeout()) -> {ok, binary()| list()} | {error, reason()}.
%%
%% Description: Receives data when active = false
%%--------------------------------------------------------------------
recv(Socket, Length) ->
    recv(Socket, Length, infinity).
recv(#sslsocket{pid = Pid}, Length, Timeout) when is_pid(Pid),
						  (is_integer(Timeout) andalso Timeout >= 0) or (Timeout == infinity)->
    ssl_connection:recv(Pid, Length, Timeout);
recv(#sslsocket{pid = {Listen,
		       #config{transport_info = {Transport, _, _, _}}}}, _,_) when is_port(Listen)->
    Transport:recv(Listen, 0). %% {error,enotconn}

%%--------------------------------------------------------------------
-spec controlling_process(#sslsocket{}, pid()) -> ok | {error, reason()}.
%%
%% Description: Changes process that receives the messages when active = true
%% or once.
%%--------------------------------------------------------------------
controlling_process(#sslsocket{pid = Pid}, NewOwner) when is_pid(Pid), is_pid(NewOwner) ->
    ssl_connection:new_user(Pid, NewOwner);
controlling_process(#sslsocket{pid = {Listen,
				      #config{transport_info = {Transport, _, _, _}}}},
		    NewOwner) when is_port(Listen),
				   is_pid(NewOwner) ->
    Transport:controlling_process(Listen, NewOwner).


%%--------------------------------------------------------------------
-spec connection_information(#sslsocket{}) -> {ok, list()} | {error, reason()}.
%%
%% Description: Return SSL information for the connection
%%--------------------------------------------------------------------
connection_information(#sslsocket{pid = Pid}) when is_pid(Pid) -> 
    case ssl_connection:connection_information(Pid) of
	{ok, Info} ->
	    {ok, [Item || Item = {_Key, Value} <- Info,  Value =/= undefined]};
	Error ->
            Error
    end;
connection_information(#sslsocket{pid = {Listen, _}}) when is_port(Listen) -> 
    {error, enotconn}.

%%--------------------------------------------------------------------
-spec connection_information(#sslsocket{}, [atom()]) -> {ok, list()} | {error, reason()}.
%%
%% Description: Return SSL information for the connection
%%--------------------------------------------------------------------
connection_information(#sslsocket{} = SSLSocket, Items) -> 
    case connection_information(SSLSocket) of
        {ok, Info} ->
            {ok, [Item || Item = {Key, Value} <- Info,  lists:member(Key, Items),
			  Value =/= undefined]};
	Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% Deprecated
-spec connection_info(#sslsocket{}) -> 	{ok, {tls_record:tls_atom_version(), ssl_cipher:erl_cipher_suite()}} |
					{error, reason()}.
%%
%% Description: Returns ssl protocol and cipher used for the connection
%%--------------------------------------------------------------------
connection_info(#sslsocket{} = SSLSocket) ->
    case connection_information(SSLSocket) of
        {ok, Result} ->
            {ok, {proplists:get_value(protocol, Result), proplists:get_value(cipher_suite, Result)}};
        Error ->
            Error
    end.

%%--------------------------------------------------------------------
-spec peername(#sslsocket{}) -> {ok, {inet:ip_address(), inet:port_number()}} | {error, reason()}.
%%
%% Description: same as inet:peername/1.
%%--------------------------------------------------------------------
peername(#sslsocket{pid = Pid, fd = {Transport, Socket, _, _}}) when is_pid(Pid)->
    ssl_socket:peername(Transport, Socket);
peername(#sslsocket{pid = {ListenSocket,  #config{transport_info = {Transport,_,_,_}}}}) ->
    ssl_socket:peername(Transport, ListenSocket). %% Will return {error, enotconn}

%%--------------------------------------------------------------------
-spec peercert(#sslsocket{}) ->{ok, DerCert::binary()} | {error, reason()}.
%%
%% Description: Returns the peercert.
%%--------------------------------------------------------------------
peercert(#sslsocket{pid = Pid}) when is_pid(Pid) ->
    case ssl_connection:peer_certificate(Pid) of
	{ok, undefined} ->
	    {error, no_peercert};
        Result ->
	    Result
    end;
peercert(#sslsocket{pid = {Listen, _}}) when is_port(Listen) ->
    {error, enotconn}.

%%--------------------------------------------------------------------
-spec negotiated_protocol(#sslsocket{}) -> {ok, binary()} | {error, reason()}.
%%
%% Description: Returns the protocol that has been negotiated. If no
%% protocol has been negotiated will return {error, protocol_not_negotiated}
%%--------------------------------------------------------------------
negotiated_protocol(#sslsocket{pid = Pid}) ->
    ssl_connection:negotiated_protocol(Pid).

%%--------------------------------------------------------------------
-spec negotiated_next_protocol(#sslsocket{}) -> {ok, binary()} | {error, reason()}.
%%
%% Description: Returns the next protocol that has been negotiated. If no
%% protocol has been negotiated will return {error, next_protocol_not_negotiated}
%%--------------------------------------------------------------------
negotiated_next_protocol(Socket) ->
    case negotiated_protocol(Socket) of
        {error, protocol_not_negotiated} ->
            {error, next_protocol_not_negotiated};
        Res ->
            Res
    end.

%%--------------------------------------------------------------------
-spec cipher_suites() -> [ssl_cipher:erl_cipher_suite()] | [string()].
%%--------------------------------------------------------------------
cipher_suites() ->
    cipher_suites(erlang).
%%--------------------------------------------------------------------
-spec cipher_suites(erlang | openssl | all) -> [ssl_cipher:erl_cipher_suite()] |
					       [string()].
%% Description: Returns all supported cipher suites.
%%--------------------------------------------------------------------
cipher_suites(erlang) ->
    [ssl_cipher:erl_suite_definition(Suite) || Suite <- available_suites(default)];

cipher_suites(openssl) ->
    [ssl_cipher:openssl_suite_name(Suite) || Suite <- available_suites(default)];

cipher_suites(all) ->
    [ssl_cipher:erl_suite_definition(Suite) || Suite <- available_suites(all)].

%%--------------------------------------------------------------------
-spec getopts(#sslsocket{}, [gen_tcp:option_name()]) ->
		     {ok, [gen_tcp:option()]} | {error, reason()}.
%%
%% Description: Gets options
%%--------------------------------------------------------------------
getopts(#sslsocket{pid = Pid}, OptionTags) when is_pid(Pid), is_list(OptionTags) ->
    ssl_connection:get_opts(Pid, OptionTags);
getopts(#sslsocket{pid = {_,  #config{transport_info = {Transport,_,_,_}}}} = ListenSocket,
	OptionTags) when is_list(OptionTags) ->
    try ssl_socket:getopts(Transport, ListenSocket, OptionTags) of
	{ok, _} = Result ->
	    Result;
	{error, InetError} ->
	    {error, {options, {socket_options, OptionTags, InetError}}}
    catch
	_:Error ->
	    {error, {options, {socket_options, OptionTags, Error}}}
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
	    ssl_connection:set_opts(Pid, Options)
    catch
	_:_ ->
	    {error, {options, {not_a_proplist, Options0}}}
    end;

setopts(#sslsocket{pid = {_, #config{transport_info = {Transport,_,_,_}}}} = ListenSocket, Options) when is_list(Options) ->
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
-spec getstat(Socket) ->
	{ok, OptionValues} | {error, inet:posix()} when
      Socket :: #sslsocket{},
      OptionValues :: [{inet:stat_option(), integer()}].
%%
%% Description: Get all statistic options for a socket.
%%--------------------------------------------------------------------
getstat(Socket) ->
	getstat(Socket, inet:stats()).

%%---------------------------------------------------------------
-spec getstat(Socket, Options) ->
	{ok, OptionValues} | {error, inet:posix()} when
      Socket :: #sslsocket{},
      Options :: [inet:stat_option()],
      OptionValues :: [{inet:stat_option(), integer()}].
%%
%% Description: Get one or more statistic options for a socket.
%%--------------------------------------------------------------------
getstat(#sslsocket{pid = {Listen,  #config{transport_info = {Transport, _, _, _}}}}, Options) when is_port(Listen), is_list(Options) ->
    ssl_socket:getstat(Transport, Listen, Options);

getstat(#sslsocket{pid = Pid, fd = {Transport, Socket, _, _}}, Options) when is_pid(Pid), is_list(Options) ->
    ssl_socket:getstat(Transport, Socket, Options).

%%---------------------------------------------------------------
-spec shutdown(#sslsocket{}, read | write | read_write) ->  ok | {error, reason()}.
%%
%% Description: Same as gen_tcp:shutdown/2
%%--------------------------------------------------------------------
shutdown(#sslsocket{pid = {Listen, #config{transport_info = {Transport,_, _, _}}}},
	 How) when is_port(Listen) ->
    Transport:shutdown(Listen, How);
shutdown(#sslsocket{pid = Pid}, How) ->
    ssl_connection:shutdown(Pid, How).

%%--------------------------------------------------------------------
-spec sockname(#sslsocket{}) -> {ok, {inet:ip_address(), inet:port_number()}} | {error, reason()}.
%%
%% Description: Same as inet:sockname/1
%%--------------------------------------------------------------------
sockname(#sslsocket{pid = {Listen,  #config{transport_info = {Transport, _, _, _}}}}) when is_port(Listen) ->
    ssl_socket:sockname(Transport, Listen);

sockname(#sslsocket{pid = Pid, fd = {Transport, Socket, _, _}}) when is_pid(Pid) ->
    ssl_socket:sockname(Transport, Socket).

%%---------------------------------------------------------------
-spec session_info(#sslsocket{}) -> {ok, list()} | {error, reason()}.
%%
%% Description: Returns list of session info currently [{session_id, session_id(),
%% {cipher_suite, cipher_suite()}]
%%--------------------------------------------------------------------
session_info(#sslsocket{pid = Pid}) when is_pid(Pid) ->
    ssl_connection:session_info(Pid);
session_info(#sslsocket{pid = {Listen,_}}) when is_port(Listen) ->
    {error, enotconn}.

%%---------------------------------------------------------------
-spec versions() -> [{ssl_app, string()} | {supported, [tls_record:tls_atom_version()]} |
		     {available, [tls_record:tls_atom_version()]}].
%%
%% Description: Returns a list of relevant versions.
%%--------------------------------------------------------------------
versions() ->
    Vsns = tls_record:supported_protocol_versions(),
    SupportedVsns = [tls_record:protocol_version(Vsn) || Vsn <- Vsns],
    AvailableVsns = ?ALL_AVAILABLE_VERSIONS,
    %% TODO Add DTLS versions when supported
    [{ssl_app, ?VSN}, {supported, SupportedVsns}, {available, AvailableVsns}].


%%---------------------------------------------------------------
-spec renegotiate(#sslsocket{}) -> ok | {error, reason()}.
%%
%% Description: Initiates a renegotiation.
%%--------------------------------------------------------------------
renegotiate(#sslsocket{pid = Pid}) when is_pid(Pid) ->
    ssl_connection:renegotiation(Pid);
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
    ssl_connection:prf(Pid, Secret, Label, Seed, WantedLength);
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

%%%--------------------------------------------------------------
%%% Internal functions
%%%--------------------------------------------------------------------

%% Possible filters out suites not supported by crypto 
available_suites(default) ->  
    Version = tls_record:highest_protocol_version([]),			  
    ssl_cipher:filter_suites(ssl_cipher:suites(Version));

available_suites(all) ->  
    Version = tls_record:highest_protocol_version([]),			  
    ssl_cipher:filter_suites(ssl_cipher:all_suites(Version)).

do_connect(Address, Port,
	   #config{transport_info = CbInfo, inet_user = UserOpts, ssl = SslOpts,
		   emulated = EmOpts, inet_ssl = SocketOpts, connection_cb = ConnetionCb},
	   Timeout) ->
    {Transport, _, _, _} = CbInfo,
    try Transport:connect(Address, Port,  SocketOpts, Timeout) of
	{ok, Socket} ->
	    ssl_connection:connect(ConnetionCb, Address, Port, Socket, 
				   {SslOpts, emulated_socket_options(EmOpts, #socket_options{}), undefined},
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

%% Handle extra ssl options given to ssl_accept
handle_options(Opts0, #ssl_options{protocol = Protocol, cacerts = CaCerts0,
				   cacertfile = CaCertFile0} = InheritedSslOpts) ->
    RecordCB = record_cb(Protocol),
    CaCerts = handle_option(cacerts, Opts0, CaCerts0),
    {Verify, FailIfNoPeerCert, CaCertDefault, VerifyFun, PartialChainHanlder,
     VerifyClientOnce} = handle_verify_options(Opts0, CaCerts),
    CaCertFile = case proplists:get_value(cacertfile, Opts0, CaCertFile0) of
		     undefined ->
			 CaCertDefault;
		     CAFile ->
			 CAFile
		 end,

    NewVerifyOpts = InheritedSslOpts#ssl_options{cacerts = CaCerts,
						 cacertfile = CaCertFile,
						 verify = Verify,
						 verify_fun = VerifyFun,
						 partial_chain = PartialChainHanlder,
						 fail_if_no_peer_cert = FailIfNoPeerCert,
						 verify_client_once = VerifyClientOnce},
    SslOpts1 = lists:foldl(fun(Key, PropList) ->
				   proplists:delete(Key, PropList)
			   end, Opts0, [cacerts, cacertfile, verify, verify_fun, partial_chain,
					fail_if_no_peer_cert, verify_client_once]),
    case handle_option(versions, SslOpts1, []) of
	[] ->
	    new_ssl_options(SslOpts1, NewVerifyOpts, RecordCB);
	Value ->
	    Versions = [RecordCB:protocol_version(Vsn) || Vsn <- Value],
	    new_ssl_options(proplists:delete(versions, SslOpts1), 
			    NewVerifyOpts#ssl_options{versions = Versions}, record_cb(Protocol))
    end;

%% Handle all options in listen and connect
handle_options(Opts0, Role) ->
    Opts = proplists:expand([{binary, [{mode, binary}]},
			     {list, [{mode, list}]}], Opts0),
    assert_proplist(Opts),
    RecordCb = record_cb(Opts),

    ReuseSessionFun = fun(_, _, _, _) -> true end,
    CaCerts = handle_option(cacerts, Opts, undefined),

    {Verify, FailIfNoPeerCert, CaCertDefault, VerifyFun, PartialChainHanlder, VerifyClientOnce} =
	handle_verify_options(Opts, CaCerts),
    
    CertFile = handle_option(certfile, Opts, <<>>),
    RecordCb = record_cb(Opts),
    
    Versions = case handle_option(versions, Opts, []) of
		   [] ->
		       RecordCb:supported_protocol_versions();
		   Vsns  ->
		       [RecordCb:protocol_version(Vsn) || Vsn <- Vsns]
	       end,

    SSLOptions = #ssl_options{
		    versions   = Versions,
		    verify     = validate_option(verify, Verify),
		    verify_fun = VerifyFun,
		    partial_chain = PartialChainHanlder,
		    fail_if_no_peer_cert = FailIfNoPeerCert,
		    verify_client_once = VerifyClientOnce,
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
		    ciphers    = handle_cipher_option(proplists:get_value(ciphers, Opts, []), 
						      RecordCb:highest_protocol_version(Versions)),
		    signature_algs = handle_hashsigns_option(proplists:get_value(signature_algs, Opts, 
									     default_option_role(server, 
												 tls_v1:default_signature_algs(Versions), Role)),
							 RecordCb:highest_protocol_version(Versions)), 
		    %% Server side option
		    reuse_session = handle_option(reuse_session, Opts, ReuseSessionFun),
		    reuse_sessions = handle_option(reuse_sessions, Opts, true),
		    secure_renegotiate = handle_option(secure_renegotiate, Opts, false),
		    client_renegotiation = handle_option(client_renegotiation, Opts, 
							 default_option_role(server, true, Role), 
							 server, Role),
		    renegotiate_at = handle_option(renegotiate_at, Opts, ?DEFAULT_RENEGOTIATE_AT),
		    hibernate_after = handle_option(hibernate_after, Opts, infinity),
		    erl_dist = handle_option(erl_dist, Opts, false),
		    alpn_advertised_protocols =
			handle_option(alpn_advertised_protocols, Opts, undefined),
		    alpn_preferred_protocols =
			handle_option(alpn_preferred_protocols, Opts, undefined),
		    next_protocols_advertised =
			handle_option(next_protocols_advertised, Opts, undefined),
		    next_protocol_selector =
			make_next_protocol_selector(
			  handle_option(client_preferred_next_protocols, Opts, undefined)),
		    log_alert = handle_option(log_alert, Opts, true),
		    server_name_indication = handle_option(server_name_indication, Opts, undefined),
		    sni_hosts = handle_option(sni_hosts, Opts, []),
		    sni_fun = handle_option(sni_fun, Opts, undefined),
		    honor_cipher_order = handle_option(honor_cipher_order, Opts, 
						       default_option_role(server, false, Role), 
						       server, Role),
		    protocol = proplists:get_value(protocol, Opts, tls),
		    padding_check =  proplists:get_value(padding_check, Opts, true),
		    beast_mitigation = handle_option(beast_mitigation, Opts, one_n_minus_one),
		    fallback = handle_option(fallback, Opts,
					     proplists:get_value(fallback, Opts,    
								 default_option_role(client, 
										     false, Role)),
					     client, Role),
		    crl_check = handle_option(crl_check, Opts, false),
		    crl_cache = handle_option(crl_cache, Opts, {ssl_crl_cache, {internal, []}}),
		    v2_hello_compatible = handle_option(v2_hello_compatible, Opts, false)
		   },

    CbInfo  = proplists:get_value(cb_info, Opts, {gen_tcp, tcp, tcp_closed, tcp_error}),
    SslOptions = [protocol, versions, verify, verify_fun, partial_chain,
		  fail_if_no_peer_cert, verify_client_once,
		  depth, cert, certfile, key, keyfile,
		  password, cacerts, cacertfile, dh, dhfile,
		  user_lookup_fun, psk_identity, srp_identity, ciphers,
		  reuse_session, reuse_sessions, ssl_imp, client_renegotiation,
		  cb_info, renegotiate_at, secure_renegotiate, hibernate_after,
		  erl_dist, alpn_advertised_protocols, sni_hosts, sni_fun,
		  alpn_preferred_protocols, next_protocols_advertised,
		  client_preferred_next_protocols, log_alert,
		  server_name_indication, honor_cipher_order, padding_check, crl_check, crl_cache,
		  fallback, signature_algs, beast_mitigation, v2_hello_compatible],

    SockOpts = lists:foldl(fun(Key, PropList) ->
				   proplists:delete(Key, PropList)
			   end, Opts, SslOptions),

    {Sock, Emulated} = emulated_options(SockOpts),
    ConnetionCb = connection_cb(Opts),

    {ok, #config{ssl = SSLOptions, emulated = Emulated, inet_ssl = Sock,
		 inet_user = SockOpts, transport_info = CbInfo, connection_cb = ConnetionCb
		}}.



handle_option(OptionName, Opts, Default, Role, Role) ->
    handle_option(OptionName, Opts, Default);
handle_option(_, _, undefined = Value, _, _) ->
    Value.

handle_option(sni_fun, Opts, Default) ->
    OptFun = validate_option(sni_fun,
                             proplists:get_value(sni_fun, Opts, Default)),
    OptHosts = proplists:get_value(sni_hosts, Opts, undefined),
    case {OptFun, OptHosts} of
        {Default, _} ->
            Default;
        {_, undefined} ->
            OptFun;
        _ ->
            throw({error, {conflict_options, [sni_fun, sni_hosts]}})
    end;
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
validate_option(partial_chain, Value) when is_function(Value) ->
    Value;
validate_option(fail_if_no_peer_cert, Value) when is_boolean(Value) ->
    Value;
validate_option(verify_client_once, Value) when is_boolean(Value) ->
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
    binary_filename(Value);

validate_option(key, undefined) ->
    undefined;
validate_option(key, {KeyType, Value}) when is_binary(Value),
					    KeyType == rsa; %% Backwards compatibility
					    KeyType == dsa; %% Backwards compatibility
					    KeyType == 'RSAPrivateKey';
					    KeyType == 'DSAPrivateKey';
					    KeyType == 'ECPrivateKey';
					    KeyType == 'PrivateKeyInfo' ->
    {KeyType, Value};

validate_option(keyfile, undefined) ->
   <<>>;
validate_option(keyfile, Value) when is_binary(Value) ->
    Value;
validate_option(keyfile, Value) when is_list(Value), Value =/= "" ->
    binary_filename(Value);
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
    binary_filename(Value);
validate_option(dh, Value) when Value == undefined;
				is_binary(Value) ->
    Value;
validate_option(dhfile, undefined = Value)  ->
    Value;
validate_option(dhfile, Value) when is_binary(Value) ->
    Value;
validate_option(dhfile, Value) when is_list(Value), Value =/= "" ->
    binary_filename(Value);
validate_option(psk_identity, undefined) ->
    undefined;
validate_option(psk_identity, Identity)
  when is_list(Identity), Identity =/= "", length(Identity) =< 65535 ->
    binary_filename(Identity);
validate_option(user_lookup_fun, undefined) ->
    undefined;
validate_option(user_lookup_fun, {Fun, _} = Value) when is_function(Fun, 3) ->
   Value;
validate_option(srp_identity, undefined) ->
    undefined;
validate_option(srp_identity, {Username, Password})
  when is_list(Username), is_list(Password), Username =/= "", length(Username) =< 255 ->
    {unicode:characters_to_binary(Username),
     unicode:characters_to_binary(Password)};

validate_option(reuse_session, Value) when is_function(Value) ->
    Value;
validate_option(reuse_sessions, Value) when is_boolean(Value) ->
    Value;

validate_option(secure_renegotiate, Value) when is_boolean(Value) ->
    Value;
validate_option(client_renegotiation, Value) when is_boolean(Value) ->
    Value;
validate_option(renegotiate_at, Value) when is_integer(Value) ->
    erlang:min(Value, ?DEFAULT_RENEGOTIATE_AT);

validate_option(hibernate_after, undefined) -> %% Backwards compatibility
    infinity;
validate_option(hibernate_after, infinity) ->
    infinity;
validate_option(hibernate_after, Value) when is_integer(Value), Value >= 0 ->
    Value;

validate_option(erl_dist,Value) when is_boolean(Value) ->
    Value;
validate_option(Opt, Value)
  when Opt =:= alpn_advertised_protocols orelse Opt =:= alpn_preferred_protocols,
       is_list(Value) ->
    case tls_record:highest_protocol_version([]) of
	{3,0} ->
	    throw({error, {options, {not_supported_in_sslv3, {Opt, Value}}}});
	_ ->
	    validate_binary_list(Opt, Value),
	    Value
    end;
validate_option(Opt, Value)
  when Opt =:= alpn_advertised_protocols orelse Opt =:= alpn_preferred_protocols,
       Value =:= undefined ->
    undefined;
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
validate_option(log_alert, Value) when is_boolean(Value) ->
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
validate_option(server_name_indication, Value) when is_list(Value) ->
    Value;
validate_option(server_name_indication, disable) ->
    disable;
validate_option(server_name_indication, undefined) ->
    undefined;
validate_option(sni_hosts, []) ->
    [];
validate_option(sni_hosts, [{Hostname, SSLOptions} | Tail]) when is_list(Hostname) ->
	RecursiveSNIOptions = proplists:get_value(sni_hosts, SSLOptions, undefined),
	case RecursiveSNIOptions of
		undefined ->
			[{Hostname, validate_options(SSLOptions)} | validate_option(sni_hosts, Tail)];
		_ ->
			throw({error, {options, {sni_hosts, RecursiveSNIOptions}}})
	end;
validate_option(sni_fun, undefined) ->
    undefined;
validate_option(sni_fun, Fun) when is_function(Fun) ->
    Fun;
validate_option(honor_cipher_order, Value) when is_boolean(Value) ->
    Value;
validate_option(padding_check, Value) when is_boolean(Value) ->
    Value;
validate_option(fallback, Value) when is_boolean(Value) ->
    Value;
validate_option(crl_check, Value) when is_boolean(Value)  ->
    Value;
validate_option(crl_check, Value) when (Value == best_effort) or (Value == peer) -> 
    Value;
validate_option(crl_cache, {Cb, {_Handle, Options}} = Value) when is_atom(Cb) and is_list(Options) ->
    Value;
validate_option(beast_mitigation, Value) when Value == one_n_minus_one orelse
                                              Value == zero_n orelse
                                              Value == disabled ->
  Value;
validate_option(v2_hello_compatible, Value) when is_boolean(Value)  ->
    Value;
validate_option(Opt, Value) ->
    throw({error, {options, {Opt, Value}}}).

handle_hashsigns_option(Value, {Major, Minor} = Version) when is_list(Value) 
							      andalso Major >= 3 andalso Minor >= 3->
    case tls_v1:signature_algs(Version, Value) of
	[] ->
	    throw({error, {options, no_supported_algorithms, {signature_algs, Value}}});
	_ ->	
	    Value
    end;
handle_hashsigns_option(_, {Major, Minor} = Version) when Major >= 3 andalso Minor >= 3->
    handle_hashsigns_option(tls_v1:default_signature_algs(Version), Version);
handle_hashsigns_option(_, _Version) ->
    undefined.

validate_options([]) ->
	[];
validate_options([{Opt, Value} | Tail]) ->
	[{Opt, validate_option(Opt, Value)} | validate_options(Tail)].

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
emulated_options(Opts) ->
    emulated_options(Opts, ssl_socket:internal_inet_values(), ssl_socket:default_inet_values()).

emulated_options([{mode, Value} = Opt |Opts], Inet, Emulated) ->
    validate_inet_option(mode, Value),
    emulated_options(Opts, Inet, [Opt | proplists:delete(mode, Emulated)]);
emulated_options([{header, Value} = Opt | Opts], Inet, Emulated) ->
    validate_inet_option(header, Value),
    emulated_options(Opts, Inet,  [Opt | proplists:delete(header, Emulated)]);
emulated_options([{active, Value} = Opt |Opts], Inet, Emulated) ->
    validate_inet_option(active, Value),
    emulated_options(Opts, Inet, [Opt | proplists:delete(active, Emulated)]);
emulated_options([{packet, Value} = Opt |Opts], Inet, Emulated) ->
    validate_inet_option(packet, Value),
    emulated_options(Opts, Inet, [Opt | proplists:delete(packet, Emulated)]);
emulated_options([{packet_size, Value} = Opt | Opts], Inet, Emulated) ->
    validate_inet_option(packet_size, Value),
    emulated_options(Opts, Inet, [Opt | proplists:delete(packet_size, Emulated)]);
emulated_options([Opt|Opts], Inet, Emulated) ->
    emulated_options(Opts, [Opt|Inet], Emulated);
emulated_options([], Inet,Emulated) ->
    {Inet, Emulated}.

handle_cipher_option(Value, Version)  when is_list(Value) ->
    try binary_cipher_suites(Version, Value) of
	Suites ->
	    Suites
    catch
	exit:_ ->
	    throw({error, {options, {ciphers, Value}}});
	error:_->
	    throw({error, {options, {ciphers, Value}}})
    end.

binary_cipher_suites(Version, []) -> 
    %% Defaults to all supported suites that does
    %% not require explicit configuration
    ssl_cipher:filter_suites(ssl_cipher:suites(Version));
binary_cipher_suites(Version, [Tuple|_] = Ciphers0) when is_tuple(Tuple) ->
    Ciphers = [ssl_cipher:suite(C) || C <- Ciphers0],
    binary_cipher_suites(Version, Ciphers);

binary_cipher_suites(Version, [Cipher0 | _] = Ciphers0) when is_binary(Cipher0) ->
    All = ssl_cipher:all_suites(Version),
    case [Cipher || Cipher <- Ciphers0, lists:member(Cipher, All)] of
	[] ->
	    %% Defaults to all supported suites that does
	    %% not require explicit configuration
	    ssl_cipher:filter_suites(ssl_cipher:suites(Version));
	Ciphers ->
	    Ciphers
    end;
binary_cipher_suites(Version, [Head | _] = Ciphers0) when is_list(Head) ->
    %% Format: ["RC4-SHA","RC4-MD5"]
    Ciphers = [ssl_cipher:openssl_suite(C) || C <- Ciphers0],
    binary_cipher_suites(Version, Ciphers);
binary_cipher_suites(Version, Ciphers0)  ->
    %% Format: "RC4-SHA:RC4-MD5"
    Ciphers = [ssl_cipher:openssl_suite(C) || C <- string:tokens(Ciphers0, ":")],
    binary_cipher_suites(Version, Ciphers).

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

connection_cb(tls) ->
    tls_connection;
connection_cb(dtls) ->
    dtls_connection;
connection_cb(Opts) ->
   connection_cb(proplists:get_value(protocol, Opts, tls)).

record_cb(tls) ->
    tls_record;
record_cb(dtls) ->
    dtls_record;
record_cb(Opts) ->
    record_cb(proplists:get_value(protocol, Opts, tls)).

connection_sup(tls_connection) ->
    tls_connection_sup;
connection_sup(dtls_connection) ->
    dtls_connection_sup.

binary_filename(FileName) ->
    Enc = file:native_name_encoding(),
    unicode:characters_to_binary(FileName, unicode, Enc).

assert_proplist([]) ->
    true;
assert_proplist([{Key,_} | Rest]) when is_atom(Key) ->
    assert_proplist(Rest);
%% Handle exceptions 
assert_proplist([{raw,_,_,_} | Rest]) ->
    assert_proplist(Rest);
assert_proplist([inet | Rest]) ->
    assert_proplist(Rest);
assert_proplist([inet6 | Rest]) ->
    assert_proplist(Rest);
assert_proplist([Value | _]) ->
    throw({option_not_a_key_value_tuple, Value}).

emulated_socket_options(InetValues, #socket_options{
				       mode   = Mode,
				       header = Header,
				       active = Active,
				       packet = Packet,
				       packet_size = Size}) ->
    #socket_options{
       mode   = proplists:get_value(mode, InetValues, Mode),
       header = proplists:get_value(header, InetValues, Header),
       active = proplists:get_value(active, InetValues, Active),
       packet = proplists:get_value(packet, InetValues, Packet),
       packet_size = proplists:get_value(packet_size, InetValues, Size)
      }.

new_ssl_options([], #ssl_options{} = Opts, _) -> 
    Opts;
new_ssl_options([{verify_client_once, Value} | Rest], #ssl_options{} = Opts, RecordCB) -> 
    new_ssl_options(Rest, Opts#ssl_options{verify_client_once = 
					       validate_option(verify_client_once, Value)}, RecordCB); 
new_ssl_options([{depth, Value} | Rest], #ssl_options{} = Opts, RecordCB) -> 
    new_ssl_options(Rest, Opts#ssl_options{depth = validate_option(depth, Value)}, RecordCB);
new_ssl_options([{cert, Value} | Rest], #ssl_options{} = Opts, RecordCB) -> 
    new_ssl_options(Rest, Opts#ssl_options{cert = validate_option(cert, Value)}, RecordCB);
new_ssl_options([{certfile, Value} | Rest], #ssl_options{} = Opts, RecordCB) -> 
    new_ssl_options(Rest, Opts#ssl_options{certfile = validate_option(certfile, Value)}, RecordCB);
new_ssl_options([{key, Value} | Rest], #ssl_options{} = Opts, RecordCB) -> 
    new_ssl_options(Rest, Opts#ssl_options{key = validate_option(key, Value)}, RecordCB);
new_ssl_options([{keyfile, Value} | Rest], #ssl_options{} = Opts, RecordCB) -> 
    new_ssl_options(Rest, Opts#ssl_options{keyfile = validate_option(keyfile, Value)}, RecordCB);
new_ssl_options([{password, Value} | Rest], #ssl_options{} = Opts, RecordCB) -> 
    new_ssl_options(Rest, Opts#ssl_options{password = validate_option(password, Value)}, RecordCB);
new_ssl_options([{dh, Value} | Rest], #ssl_options{} = Opts, RecordCB) -> 
    new_ssl_options(Rest, Opts#ssl_options{dh = validate_option(dh, Value)}, RecordCB);
new_ssl_options([{dhfile, Value} | Rest], #ssl_options{} = Opts, RecordCB) -> 
    new_ssl_options(Rest, Opts#ssl_options{dhfile = validate_option(dhfile, Value)}, RecordCB); 
new_ssl_options([{user_lookup_fun, Value} | Rest], #ssl_options{} = Opts, RecordCB) -> 
    new_ssl_options(Rest, Opts#ssl_options{user_lookup_fun = validate_option(user_lookup_fun, Value)}, RecordCB);
new_ssl_options([{psk_identity, Value} | Rest], #ssl_options{} = Opts, RecordCB) -> 
    new_ssl_options(Rest, Opts#ssl_options{psk_identity = validate_option(psk_identity, Value)}, RecordCB);
new_ssl_options([{srp_identity, Value} | Rest], #ssl_options{} = Opts, RecordCB) -> 
    new_ssl_options(Rest, Opts#ssl_options{srp_identity = validate_option(srp_identity, Value)}, RecordCB);
new_ssl_options([{ciphers, Value} | Rest], #ssl_options{versions = Versions} = Opts, RecordCB) -> 
    Ciphers = handle_cipher_option(Value, RecordCB:highest_protocol_version(Versions)),
    new_ssl_options(Rest, 
		    Opts#ssl_options{ciphers = Ciphers}, RecordCB);
new_ssl_options([{reuse_session, Value} | Rest], #ssl_options{} = Opts, RecordCB) -> 
    new_ssl_options(Rest, Opts#ssl_options{reuse_session = validate_option(reuse_session, Value)}, RecordCB);
new_ssl_options([{reuse_sessions, Value} | Rest], #ssl_options{} = Opts, RecordCB) -> 
    new_ssl_options(Rest, Opts#ssl_options{reuse_sessions = validate_option(reuse_sessions, Value)}, RecordCB);
new_ssl_options([{ssl_imp, _Value} | Rest], #ssl_options{} = Opts, RecordCB) -> %% Not used backwards compatibility
    new_ssl_options(Rest, Opts, RecordCB);
new_ssl_options([{renegotiate_at, Value} | Rest], #ssl_options{} = Opts, RecordCB) -> 
    new_ssl_options(Rest, Opts#ssl_options{ renegotiate_at = validate_option(renegotiate_at, Value)}, RecordCB);
new_ssl_options([{secure_renegotiate, Value} | Rest], #ssl_options{} = Opts, RecordCB) -> 
    new_ssl_options(Rest, Opts#ssl_options{secure_renegotiate = validate_option(secure_renegotiate, Value)}, RecordCB); 
new_ssl_options([{client_renegotiation, Value} | Rest], #ssl_options{} = Opts, RecordCB) -> 
    new_ssl_options(Rest, Opts#ssl_options{client_renegotiation = validate_option(client_renegotiation, Value)}, RecordCB); 
new_ssl_options([{hibernate_after, Value} | Rest], #ssl_options{} = Opts, RecordCB) -> 
    new_ssl_options(Rest, Opts#ssl_options{hibernate_after = validate_option(hibernate_after, Value)}, RecordCB);
new_ssl_options([{alpn_advertised_protocols, Value} | Rest], #ssl_options{} = Opts, RecordCB) ->
	new_ssl_options(Rest, Opts#ssl_options{alpn_advertised_protocols = validate_option(alpn_advertised_protocols, Value)}, RecordCB);
new_ssl_options([{alpn_preferred_protocols, Value} | Rest], #ssl_options{} = Opts, RecordCB) ->
	new_ssl_options(Rest, Opts#ssl_options{alpn_preferred_protocols = validate_option(alpn_preferred_protocols, Value)}, RecordCB);
new_ssl_options([{next_protocols_advertised, Value} | Rest], #ssl_options{} = Opts, RecordCB) -> 
    new_ssl_options(Rest, Opts#ssl_options{next_protocols_advertised = validate_option(next_protocols_advertised, Value)}, RecordCB);
new_ssl_options([{client_preferred_next_protocols, Value} | Rest], #ssl_options{} = Opts, RecordCB) -> 
    new_ssl_options(Rest, Opts#ssl_options{next_protocol_selector = 
					       make_next_protocol_selector(validate_option(client_preferred_next_protocols, Value))}, RecordCB);
new_ssl_options([{log_alert, Value} | Rest], #ssl_options{} = Opts, RecordCB) -> 
    new_ssl_options(Rest, Opts#ssl_options{log_alert = validate_option(log_alert, Value)}, RecordCB);
new_ssl_options([{server_name_indication, Value} | Rest], #ssl_options{} = Opts, RecordCB) -> 
    new_ssl_options(Rest, Opts#ssl_options{server_name_indication = validate_option(server_name_indication, Value)}, RecordCB);
new_ssl_options([{honor_cipher_order, Value} | Rest], #ssl_options{} = Opts, RecordCB) -> 
    new_ssl_options(Rest, Opts#ssl_options{honor_cipher_order = validate_option(honor_cipher_order, Value)}, RecordCB);
new_ssl_options([{signature_algs, Value} | Rest], #ssl_options{} = Opts, RecordCB) -> 
    new_ssl_options(Rest, 
		    Opts#ssl_options{signature_algs = 
					 handle_hashsigns_option(Value, 
								 RecordCB:highest_protocol_version())}, 
		    RecordCB);

new_ssl_options([{Key, Value} | _Rest], #ssl_options{}, _) -> 
    throw({error, {options, {Key, Value}}}).


handle_verify_options(Opts, CaCerts) ->
    DefaultVerifyNoneFun =
	{fun(_,{bad_cert, _}, UserState) ->
		 {valid, UserState};
	    (_,{extension, #'Extension'{critical = true}}, UserState) ->
		 %% This extension is marked as critical, so
		 %% certificate verification should fail if we don't
		 %% understand the extension.  However, this is
		 %% `verify_none', so let's accept it anyway.
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
    
    PartialChainHanlder = handle_option(partial_chain, Opts,
					fun(_) -> unknown_ca end),

    VerifyClientOnce = handle_option(verify_client_once, Opts, false),

    %% Handle 0, 1, 2 for backwards compatibility
    case proplists:get_value(verify, Opts, verify_none) of
	0 ->
	    {verify_none, false,
		 ca_cert_default(verify_none, VerifyNoneFun, CaCerts),
	     VerifyNoneFun, PartialChainHanlder, VerifyClientOnce};
	1  ->
	    {verify_peer, false,
	     ca_cert_default(verify_peer, UserVerifyFun, CaCerts),
	     UserVerifyFun, PartialChainHanlder, VerifyClientOnce};
	2 ->
	    {verify_peer, true,
	     ca_cert_default(verify_peer, UserVerifyFun, CaCerts),
	     UserVerifyFun, PartialChainHanlder, VerifyClientOnce};
	verify_none ->
	    {verify_none, false,
	     ca_cert_default(verify_none, VerifyNoneFun, CaCerts),
	     VerifyNoneFun, PartialChainHanlder, VerifyClientOnce};
	verify_peer ->
	    {verify_peer, UserFailIfNoPeerCert,
	     ca_cert_default(verify_peer, UserVerifyFun, CaCerts),
	     UserVerifyFun, PartialChainHanlder, VerifyClientOnce};
	Value ->
	    throw({error, {options, {verify, Value}}})
    end.

default_option_role(Role, Value, Role) ->
    Value;
default_option_role(_,_,_) ->
    undefined.
