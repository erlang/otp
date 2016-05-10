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

%%----------------------------------------------------------------------
%% Purpose: Help funtions for handling the TLS (specific parts of)
%%% SSL/TLS/DTLS handshake protocol
%%----------------------------------------------------------------------

-module(tls_handshake).

-include("tls_handshake.hrl").
-include("tls_record.hrl").
-include("ssl_alert.hrl").
-include("ssl_internal.hrl").
-include("ssl_cipher.hrl").
-include_lib("public_key/include/public_key.hrl").

-export([client_hello/8, hello/4,
	 get_tls_handshake/3, encode_handshake/2, decode_handshake/3]).

-type tls_handshake() :: #client_hello{} | ssl_handshake:ssl_handshake().

%%====================================================================
%% Internal application API
%%====================================================================
%%--------------------------------------------------------------------
-spec client_hello(host(), inet:port_number(), #connection_states{},
		   #ssl_options{}, integer(), atom(), boolean(), der_cert()) ->
			  #client_hello{}.
%%
%% Description: Creates a client hello message.
%%--------------------------------------------------------------------
client_hello(Host, Port, ConnectionStates,
	     #ssl_options{versions = Versions,
			  ciphers = UserSuites,
			  fallback = Fallback
			 } = SslOpts,
	     Cache, CacheCb, Renegotiation, OwnCert) ->
    Version = tls_record:highest_protocol_version(Versions),
    Pending = ssl_record:pending_connection_state(ConnectionStates, read),
    SecParams = Pending#connection_state.security_parameters,
    AvailableCipherSuites = ssl_handshake:available_suites(UserSuites, Version),     
    Extensions = ssl_handshake:client_hello_extensions(Host, Version, 
						       AvailableCipherSuites,
						       SslOpts, ConnectionStates, Renegotiation),
    CipherSuites = 
	case Fallback of
	    true ->
	        [?TLS_FALLBACK_SCSV | ssl_handshake:cipher_suites(AvailableCipherSuites, Renegotiation)];
	    false ->
		ssl_handshake:cipher_suites(AvailableCipherSuites, Renegotiation)
	end,
    Id = ssl_session:client_id({Host, Port, SslOpts}, Cache, CacheCb, OwnCert),    
    #client_hello{session_id = Id,
		  client_version = Version,
		  cipher_suites = CipherSuites,
		  compression_methods = ssl_record:compressions(),
		  random = SecParams#security_parameters.client_random,
		  extensions = Extensions
		 }.

%%--------------------------------------------------------------------
-spec hello(#server_hello{} | #client_hello{}, #ssl_options{},
	    #connection_states{} | {inet:port_number(), #session{}, db_handle(),
				    atom(), #connection_states{}, 
				    binary() | undefined, ssl_cipher:key_algo()},
	    boolean()) ->
		   {tls_record:tls_version(), session_id(), 
		    #connection_states{}, alpn | npn, binary() | undefined}|
		   {tls_record:tls_version(), {resumed | new, #session{}}, 
		    #connection_states{}, binary() | undefined, 
		    #hello_extensions{}, {ssl_cipher:hash(), ssl_cipher:sign_algo()} | undefined} |
		   #alert{}.
%%
%% Description: Handles a recieved hello message
%%--------------------------------------------------------------------
hello(#server_hello{server_version = Version, random = Random,
		    cipher_suite = CipherSuite,
		    compression_method = Compression,
		    session_id = SessionId, extensions = HelloExt},
      #ssl_options{versions = SupportedVersions} = SslOpt,
      ConnectionStates0, Renegotiation) ->
    case tls_record:is_acceptable_version(Version, SupportedVersions) of
	true ->
	    handle_server_hello_extensions(Version, SessionId, Random, CipherSuite,
					   Compression, HelloExt, SslOpt, ConnectionStates0, Renegotiation);
	false ->
	    ?ALERT_REC(?FATAL, ?PROTOCOL_VERSION)
    end;
			       
hello(#client_hello{client_version = ClientVersion,
		    cipher_suites = CipherSuites} = Hello,
      #ssl_options{versions = Versions} = SslOpts,
      Info, Renegotiation) ->
    Version = ssl_handshake:select_version(tls_record, ClientVersion, Versions),
    case ssl_cipher:is_fallback(CipherSuites) of
	true -> 
	    Highest = tls_record:highest_protocol_version(Versions),
	    case tls_record:is_higher(Highest, Version) of
		true ->
		    ?ALERT_REC(?FATAL, ?INAPPROPRIATE_FALLBACK);
		false ->				     
		    handle_client_hello(Version, Hello, SslOpts, Info, Renegotiation)
	    end;
	false ->
	    handle_client_hello(Version, Hello, SslOpts, Info, Renegotiation)
    end.
%%--------------------------------------------------------------------
-spec encode_handshake(tls_handshake(), tls_record:tls_version()) -> iolist().
%%     
%% Description: Encode a handshake packet
%%--------------------------------------------------------------------x
encode_handshake(Package, Version) ->
    {MsgType, Bin} = enc_handshake(Package, Version),
    Len = byte_size(Bin),
    [MsgType, ?uint24(Len), Bin].

%%--------------------------------------------------------------------
-spec get_tls_handshake(tls_record:tls_version(), binary(), binary() | iolist()) ->
     {[tls_handshake()], binary()}.
%%
%% Description: Given buffered and new data from ssl_record, collects
%% and returns it as a list of handshake messages, also returns leftover
%% data.
%%--------------------------------------------------------------------
get_tls_handshake(Version, Data, <<>>) ->
    get_tls_handshake_aux(Version, Data, []);
get_tls_handshake(Version, Data, Buffer) ->
    get_tls_handshake_aux(Version, list_to_binary([Buffer, Data]), []).

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
handle_client_hello(Version, #client_hello{session_id = SugesstedId,
					   cipher_suites = CipherSuites,
					   compression_methods = Compressions,
					   random = Random,
					   extensions = #hello_extensions{elliptic_curves = Curves,
									  signature_algs = ClientHashSigns} = HelloExt},
		    #ssl_options{versions = Versions,
				 signature_algs = SupportedHashSigns} = SslOpts,
		    {Port, Session0, Cache, CacheCb, ConnectionStates0, Cert, _}, Renegotiation) ->
    case tls_record:is_acceptable_version(Version, Versions) of
	true ->
	    AvailableHashSigns = available_signature_algs(ClientHashSigns, SupportedHashSigns, Cert, Version),
	    ECCCurve = ssl_handshake:select_curve(Curves, ssl_handshake:supported_ecc(Version)),
	    {Type, #session{cipher_suite = CipherSuite} = Session1}
		= ssl_handshake:select_session(SugesstedId, CipherSuites, AvailableHashSigns, Compressions,
					       Port, Session0#session{ecc = ECCCurve}, Version,
					       SslOpts, Cache, CacheCb, Cert),
	    case CipherSuite of 
		no_suite ->
		    ?ALERT_REC(?FATAL, ?INSUFFICIENT_SECURITY);
		_ ->
		    {KeyExAlg,_,_,_} = ssl_cipher:suite_definition(CipherSuite),
		    case ssl_handshake:select_hashsign(ClientHashSigns, Cert, KeyExAlg, SupportedHashSigns, Version) of
			#alert{} = Alert ->
			    Alert;
			HashSign ->
			    handle_client_hello_extensions(Version, Type, Random, CipherSuites, HelloExt,
							   SslOpts, Session1, ConnectionStates0,
							   Renegotiation, HashSign)
		    end
	    end;
	false ->
	    ?ALERT_REC(?FATAL, ?PROTOCOL_VERSION)
    end.

get_tls_handshake_aux(Version, <<?BYTE(Type), ?UINT24(Length),
			Body:Length/binary,Rest/binary>>, Acc) ->
    Raw = <<?BYTE(Type), ?UINT24(Length), Body/binary>>,
    Handshake = decode_handshake(Version, Type, Body),
    get_tls_handshake_aux(Version, Rest, [{Handshake,Raw} | Acc]);
get_tls_handshake_aux(_Version, Data, Acc) ->
    {lists:reverse(Acc), Data}.

decode_handshake(_, ?HELLO_REQUEST, <<>>) ->
    #hello_request{};

%% Client hello v2.
%% The server must be able to receive such messages, from clients that
%% are willing to use ssl v3 or higher, but have ssl v2 compatibility.
decode_handshake(_Version, ?CLIENT_HELLO, <<?BYTE(Major), ?BYTE(Minor),
				  ?UINT16(CSLength), ?UINT16(0),
				  ?UINT16(CDLength),
				  CipherSuites:CSLength/binary,
				  ChallengeData:CDLength/binary>>) ->
    #client_hello{client_version = {Major, Minor},
		  random = ssl_v2:client_random(ChallengeData, CDLength),
		  session_id = 0,
		  cipher_suites =  ssl_handshake:decode_suites('3_bytes', CipherSuites),
		  compression_methods = [?NULL],
		  extensions = #hello_extensions{}
		 };
decode_handshake(_Version, ?CLIENT_HELLO, <<?BYTE(Major), ?BYTE(Minor), Random:32/binary,
		       ?BYTE(SID_length), Session_ID:SID_length/binary,
		       ?UINT16(Cs_length), CipherSuites:Cs_length/binary,
		       ?BYTE(Cm_length), Comp_methods:Cm_length/binary,
		       Extensions/binary>>) ->

    DecodedExtensions = ssl_handshake:decode_hello_extensions({client, Extensions}),

    #client_hello{
       client_version = {Major,Minor},
       random = Random,
       session_id = Session_ID,
       cipher_suites = ssl_handshake:decode_suites('2_bytes', CipherSuites),
       compression_methods = Comp_methods,
       extensions = DecodedExtensions
      };

decode_handshake(Version, Tag, Msg) ->
    ssl_handshake:decode_handshake(Version, Tag, Msg).

enc_handshake(#hello_request{}, _Version) ->
    {?HELLO_REQUEST, <<>>};
enc_handshake(#client_hello{client_version = {Major, Minor},
		     random = Random,
		     session_id = SessionID,
		     cipher_suites = CipherSuites,
		     compression_methods = CompMethods, 
		     extensions = HelloExtensions}, _Version) ->
    SIDLength = byte_size(SessionID),
    BinCompMethods = list_to_binary(CompMethods),
    CmLength = byte_size(BinCompMethods),
    BinCipherSuites = list_to_binary(CipherSuites),
    CsLength = byte_size(BinCipherSuites),
    ExtensionsBin = ssl_handshake:encode_hello_extensions(HelloExtensions),

    {?CLIENT_HELLO, <<?BYTE(Major), ?BYTE(Minor), Random:32/binary,
		      ?BYTE(SIDLength), SessionID/binary,
		      ?UINT16(CsLength), BinCipherSuites/binary,
		      ?BYTE(CmLength), BinCompMethods/binary, ExtensionsBin/binary>>};

enc_handshake(HandshakeMsg, Version) ->
    ssl_handshake:encode_handshake(HandshakeMsg, Version).


handle_client_hello_extensions(Version, Type, Random, CipherSuites,
			HelloExt, SslOpts, Session0, ConnectionStates0, Renegotiation, HashSign) ->
    try ssl_handshake:handle_client_hello_extensions(tls_record, Random, CipherSuites,
						     HelloExt, Version, SslOpts,
						     Session0, ConnectionStates0, Renegotiation) of
	#alert{} = Alert ->
	    Alert;
	{Session, ConnectionStates, Protocol, ServerHelloExt} ->
	    {Version, {Type, Session}, ConnectionStates, Protocol, ServerHelloExt, HashSign}
    catch throw:Alert ->
	    Alert
    end.


handle_server_hello_extensions(Version, SessionId, Random, CipherSuite,
			Compression, HelloExt, SslOpt, ConnectionStates0, Renegotiation) ->
    case ssl_handshake:handle_server_hello_extensions(tls_record, Random, CipherSuite,
						      Compression, HelloExt, Version,
						      SslOpt, ConnectionStates0, Renegotiation) of
	#alert{} = Alert ->
	    Alert;
	{ConnectionStates, ProtoExt, Protocol} ->
	    {Version, SessionId, ConnectionStates, ProtoExt, Protocol}
    end.

available_signature_algs(undefined, SupportedHashSigns, _, {Major, Minor}) when 
      (Major >= 3) andalso (Minor >= 3) ->
    SupportedHashSigns;
available_signature_algs(#hash_sign_algos{hash_sign_algos = ClientHashSigns}, SupportedHashSigns, 
		     _, {Major, Minor}) when (Major >= 3) andalso (Minor >= 3) ->
    sets:to_list(sets:intersection(sets:from_list(ClientHashSigns), 
				   sets:from_list(SupportedHashSigns)));
available_signature_algs(_, _, _, _) -> 
    undefined.


