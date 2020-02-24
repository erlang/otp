%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2020. All Rights Reserved.
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

%%----------------------------------------------------------------------
%% Purpose: Help funtions for handling the DTLS (specific parts of)
%%% SSL/TLS/DTLS handshake protocol
%%----------------------------------------------------------------------
-module(dtls_handshake).

-include("dtls_connection.hrl").
-include("dtls_handshake.hrl").
-include("dtls_record.hrl").
-include("ssl_internal.hrl").
-include("ssl_alert.hrl").

%% Handshake handling
-export([client_hello/7, client_hello/9, cookie/4, hello/5, hello/4,
	 hello_verify_request/2]).

%% Handshake encoding
-export([fragment_handshake/2, encode_handshake/3]).

%% Handshake decodeing
-export([get_dtls_handshake/4]).

-type dtls_handshake() :: #client_hello{} | #hello_verify_request{} |
			  ssl_handshake:ssl_handshake().

%%====================================================================
%% Handshake handling
%%====================================================================
%%--------------------------------------------------------------------
-spec client_hello(ssl:host(), inet:port_number(), ssl_record:connection_states(),
		   ssl_options(), binary(), boolean(), der_cert()) ->
			  #client_hello{}.
%%
%% Description: Creates a client hello message.
%%--------------------------------------------------------------------
client_hello(Host, Port, ConnectionStates, SslOpts,
	     Id, Renegotiation, OwnCert) ->
    %% First client hello (two sent in DTLS ) uses empty Cookie
    client_hello(Host, Port, <<>>, ConnectionStates, SslOpts,
		 Id, Renegotiation, OwnCert, undefined).

%%--------------------------------------------------------------------
-spec client_hello(ssl:host(), inet:port_number(), term(), ssl_record:connection_states(),
		   ssl_options(), binary(),boolean(), der_cert(), binary() | undefined) ->
			  #client_hello{}.
%%
%% Description: Creates a client hello message.
%%--------------------------------------------------------------------
client_hello(_Host, _Port, Cookie, ConnectionStates,
	     #{versions := Versions,
               ciphers := UserSuites,
               fallback := Fallback} = SslOpts,
	     Id, Renegotiation, _OwnCert, OcspNonce) ->
    Version =  dtls_record:highest_protocol_version(Versions),
    Pending = ssl_record:pending_connection_state(ConnectionStates, read),
    SecParams = maps:get(security_parameters, Pending),
    TLSVersion = dtls_v1:corresponding_tls_version(Version),
    CipherSuites = ssl_handshake:available_suites(UserSuites, TLSVersion),

    Extensions = ssl_handshake:client_hello_extensions(TLSVersion, CipherSuites,
                                                       SslOpts, ConnectionStates, 
                                                       Renegotiation, undefined,
                                                       undefined, OcspNonce),

    #client_hello{session_id = Id,
		  client_version = Version,
		  cipher_suites = 
                      ssl_handshake:cipher_suites(CipherSuites, 
                                                  Renegotiation, Fallback),
		  compression_methods = ssl_record:compressions(),
		  random = SecParams#security_parameters.client_random,
		  cookie = Cookie,
		  extensions = Extensions
		 }.

hello(#server_hello{server_version = Version, random = Random,
		    cipher_suite = CipherSuite,
		    compression_method = Compression,
		    session_id = SessionId, extensions = HelloExt},
      #{versions := SupportedVersions} = SslOpt,
      ConnectionStates0, Renegotiation, OldId) ->
    IsNew = ssl_session:is_new(OldId, SessionId),
    case dtls_record:is_acceptable_version(Version, SupportedVersions) of
	true ->
	    handle_server_hello_extensions(Version, SessionId, Random, CipherSuite,
					   Compression, HelloExt, SslOpt, 
                                           ConnectionStates0, Renegotiation, IsNew);
	false ->
	    ?ALERT_REC(?FATAL, ?PROTOCOL_VERSION)
    end.
hello(#client_hello{client_version = ClientVersion} = Hello,
      #{versions := Versions} = SslOpts,
      Info, Renegotiation) ->
    Version = ssl_handshake:select_version(dtls_record, ClientVersion, Versions),
    handle_client_hello(Version, Hello, SslOpts, Info, Renegotiation).

cookie(Key, Address, Port, #client_hello{client_version = {Major, Minor},
					 random = Random,
					 session_id = SessionId,
					 cipher_suites = CipherSuites,
					 compression_methods = CompressionMethods}) ->
    CookieData = [address_to_bin(Address, Port),
		  <<?BYTE(Major), ?BYTE(Minor)>>,
		  Random, SessionId, CipherSuites, CompressionMethods],
    crypto:mac(hmac, sha, Key, CookieData).
%%--------------------------------------------------------------------
-spec hello_verify_request(binary(),  ssl_record:ssl_version()) -> #hello_verify_request{}.
%%
%% Description: Creates a hello verify request message sent by server to
%% verify client
%%--------------------------------------------------------------------
hello_verify_request(Cookie, Version) ->
    #hello_verify_request{protocol_version = Version, cookie = Cookie}.

%%--------------------------------------------------------------------
%%% Handshake encoding
%%--------------------------------------------------------------------

fragment_handshake(Bin, _) when is_binary(Bin)-> 
    %% This is the change_cipher_spec not a "real handshake" but part of the flight
    Bin;
fragment_handshake([MsgType, Len, Seq, _, Len, Bin], Size) ->
    Bins = bin_fragments(Bin, Size),
    handshake_fragments(MsgType, Seq, Len, Bins, []).
encode_handshake(Handshake, Version, Seq) ->
    {MsgType, Bin} = enc_handshake(Handshake, Version),
    Len = byte_size(Bin),
    [MsgType, ?uint24(Len), ?uint16(Seq), ?uint24(0), ?uint24(Len), Bin].
  
%%--------------------------------------------------------------------
%%% Handshake decodeing
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
-spec get_dtls_handshake(ssl_record:ssl_version(), binary(), #protocol_buffers{}, ssl_options()) ->
                                {[dtls_handshake()], #protocol_buffers{}}.                
%%
%% Description:  Given buffered and new data from dtls_record, collects
%% and returns it as a list of handshake messages, also returns 
%% possible leftover data in the new "protocol_buffers".
%%--------------------------------------------------------------------
get_dtls_handshake(Version, Fragment, ProtocolBuffers, Options) ->
    handle_fragments(Version, Fragment, ProtocolBuffers, Options, []).

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
handle_client_hello(Version, 
                    #client_hello{session_id = SugesstedId,
                                  cipher_suites = CipherSuites,
                                  compression_methods = Compressions,
                                  random = Random,
                                  extensions = HelloExt},
		    #{versions := Versions,
                      signature_algs := SupportedHashSigns,
                      eccs := SupportedECCs,
                      honor_ecc_order := ECCOrder} = SslOpts,
		    {Port, Session0, Cache, CacheCb, ConnectionStates0, Cert, _}, 
                    Renegotiation) ->
    case dtls_record:is_acceptable_version(Version, Versions) of
	true ->
            Curves = maps:get(elliptic_curves, HelloExt, undefined),
            ClientHashSigns = maps:get(signature_algs, HelloExt, undefined),
	    TLSVersion = dtls_v1:corresponding_tls_version(Version),
	    AvailableHashSigns = ssl_handshake:available_signature_algs(
				   ClientHashSigns, SupportedHashSigns, Cert,TLSVersion),
	    ECCCurve = ssl_handshake:select_curve(Curves, SupportedECCs, ECCOrder),
	    {Type, #session{cipher_suite = CipherSuite} = Session1}
		= ssl_handshake:select_session(SugesstedId, CipherSuites, 
                                               AvailableHashSigns, Compressions,
					       Port, Session0#session{ecc = ECCCurve}, TLSVersion,
					       SslOpts, Cache, CacheCb, Cert),
	    case CipherSuite of
		no_suite ->
		    ?ALERT_REC(?FATAL, ?INSUFFICIENT_SECURITY);
		_ ->
		    #{key_exchange := KeyExAlg} = ssl_cipher_format:suite_bin_to_map(CipherSuite),
		    case ssl_handshake:select_hashsign({ClientHashSigns, undefined}, Cert, KeyExAlg,
						       SupportedHashSigns, TLSVersion) of
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

handle_client_hello_extensions(Version, Type, Random, CipherSuites,
			HelloExt, SslOpts, Session0, ConnectionStates0, Renegotiation, HashSign) ->
    try ssl_handshake:handle_client_hello_extensions(dtls_record, Random, CipherSuites,
						     HelloExt, dtls_v1:corresponding_tls_version(Version),
						     SslOpts, Session0, 
                                                     ConnectionStates0, Renegotiation,
                                                     Session0#session.is_resumable) of
	{Session, ConnectionStates, Protocol, ServerHelloExt} ->
	    {Version, {Type, Session}, ConnectionStates, Protocol, ServerHelloExt, HashSign}
    catch throw:Alert ->
	    Alert
    end.

handle_server_hello_extensions(Version, SessionId, Random, CipherSuite,
			       Compression, HelloExt, SslOpt, ConnectionStates0, Renegotiation, IsNew) ->
    try ssl_handshake:handle_server_hello_extensions(dtls_record, Random, CipherSuite,
                                                     Compression, HelloExt,
                                                     dtls_v1:corresponding_tls_version(Version),
                                                     SslOpt, ConnectionStates0, Renegotiation, IsNew) of
	{ConnectionStates, ProtoExt, Protocol} ->
	    {Version, SessionId, ConnectionStates, ProtoExt, Protocol}
    catch throw:Alert ->
	    Alert
    end.

%%--------------------------------------------------------------------

enc_handshake(#hello_verify_request{protocol_version = {Major, Minor},
 				       cookie = Cookie}, _Version) ->
    CookieLength = byte_size(Cookie),
    {?HELLO_VERIFY_REQUEST, <<?BYTE(Major), ?BYTE(Minor),
 			      ?BYTE(CookieLength),
 			      Cookie:CookieLength/binary>>};
enc_handshake(#hello_request{}, _Version) ->
    {?HELLO_REQUEST, <<>>};
enc_handshake(#client_hello{client_version = {Major, Minor},
			       random = Random,
			       session_id = SessionID,
			       cookie = Cookie,
			       cipher_suites = CipherSuites,
			       compression_methods = CompMethods,
			       extensions = HelloExtensions}, _Version) ->
    SIDLength = byte_size(SessionID),
    CookieLength = byte_size(Cookie),
    BinCompMethods = list_to_binary(CompMethods),
    CmLength = byte_size(BinCompMethods),
    BinCipherSuites = list_to_binary(CipherSuites),
    CsLength = byte_size(BinCipherSuites),
    ExtensionsBin = ssl_handshake:encode_hello_extensions(HelloExtensions, 
                                                          dtls_v1:corresponding_tls_version({Major, Minor})),

    {?CLIENT_HELLO, <<?BYTE(Major), ?BYTE(Minor), Random:32/binary,
 		      ?BYTE(SIDLength), SessionID/binary,
		      ?BYTE(CookieLength), Cookie/binary,
		      ?UINT16(CsLength), BinCipherSuites/binary,
 		      ?BYTE(CmLength), BinCompMethods/binary, ExtensionsBin/binary>>};
enc_handshake(#server_hello{} = HandshakeMsg, Version) ->
    {Type, <<?BYTE(Major), ?BYTE(Minor), Rest/binary>>} = 
	ssl_handshake:encode_handshake(HandshakeMsg, Version),
    {DTLSMajor, DTLSMinor} = dtls_v1:corresponding_dtls_version({Major, Minor}),
    {Type,  <<?BYTE(DTLSMajor), ?BYTE(DTLSMinor), Rest/binary>>};
enc_handshake(HandshakeMsg, Version) ->
    ssl_handshake:encode_handshake(HandshakeMsg, dtls_v1:corresponding_tls_version(Version)).

handshake_bin(#handshake_fragment{
		 type = Type,
		 length = Len, 
		 message_seq = Seq,
		 fragment_length = Len,
		 fragment_offset = 0,
		 fragment = Fragment}) ->	    
    handshake_bin(Type, Len, Seq, Fragment).
handshake_bin(Type, Length, Seq, FragmentData) -> 
    <<?BYTE(Type), ?UINT24(Length),
      ?UINT16(Seq), ?UINT24(0), ?UINT24(Length),
      FragmentData:Length/binary>>.  
  
bin_fragments(Bin, Size) ->
     bin_fragments(Bin, size(Bin), Size, 0, []).
bin_fragments(Bin, BinSize,  FragSize, Offset, Fragments) ->
    case (BinSize - Offset - FragSize)  > 0 of
	true ->
	    Frag = binary:part(Bin, {Offset, FragSize}),
	    bin_fragments(Bin, BinSize, FragSize, Offset + FragSize, [{Frag, Offset} | Fragments]);
	false ->
	    Frag = binary:part(Bin, {Offset, BinSize-Offset}),
	    lists:reverse([{Frag, Offset} | Fragments])
    end.

handshake_fragments(_, _, _, [], Acc) ->
    lists:reverse(Acc);
handshake_fragments(MsgType, Seq, Len, [{Bin, Offset} | Bins], Acc) ->
    FragLen = size(Bin),
    handshake_fragments(MsgType, Seq, Len, Bins, 
      [<<?BYTE(MsgType), Len/binary, Seq/binary, ?UINT24(Offset),
	 ?UINT24(FragLen), Bin/binary>> | Acc]).

address_to_bin({A,B,C,D}, Port) ->
    <<0:80,16#ffff:16,A,B,C,D,Port:16>>;
address_to_bin({A,B,C,D,E,F,G,H}, Port) ->
    <<A:16,B:16,C:16,D:16,E:16,F:16,G:16,H:16,Port:16>>.

%%--------------------------------------------------------------------

handle_fragments(Version, FragmentData, Buffers0, Options, Acc) ->
    Fragments = decode_handshake_fragments(FragmentData),
    do_handle_fragments(Version, Fragments, Buffers0, Options, Acc).

do_handle_fragments(_, [], Buffers, _Options, Acc) ->
    {lists:reverse(Acc), Buffers};
do_handle_fragments(Version, [Fragment | Fragments], Buffers0, #{log_level := LogLevel} = Options, Acc) ->
    case reassemble(Version, Fragment, Buffers0) of
	{more_data, Buffers} when Fragments == [] ->
	    {lists:reverse(Acc), Buffers};
	{more_data, Buffers} ->
	    do_handle_fragments(Version, Fragments, Buffers, Options, Acc);
	{{Handshake, _} = HsPacket, Buffers} ->
            ssl_logger:debug(LogLevel, inbound, 'handshake', Handshake),
	    do_handle_fragments(Version, Fragments, Buffers, Options, [HsPacket | Acc])
    end.

decode_handshake(Version, <<?BYTE(Type), Bin/binary>>) ->
    decode_handshake(Version, Type, Bin).

decode_handshake(_, ?HELLO_REQUEST, <<>>) ->
    #hello_request{};
decode_handshake(Version, ?CLIENT_HELLO, <<?UINT24(_), ?UINT16(_),
					    ?UINT24(_),  ?UINT24(_), 
					    ?BYTE(Major), ?BYTE(Minor), Random:32/binary,
					    ?BYTE(SID_length), Session_ID:SID_length/binary,
					    ?BYTE(CookieLength), Cookie:CookieLength/binary,
					    ?UINT16(Cs_length), CipherSuites:Cs_length/binary,
					    ?BYTE(Cm_length), Comp_methods:Cm_length/binary,
					    Extensions/binary>>) ->
    TLSVersion = dtls_v1:corresponding_tls_version(Version),
    LegacyVersion = dtls_v1:corresponding_tls_version({Major, Minor}),
    Exts = ssl_handshake:decode_vector(Extensions),
    DecodedExtensions = ssl_handshake:decode_hello_extensions(Exts, TLSVersion, LegacyVersion, client),

    #client_hello{
       client_version = {Major,Minor},
       random = Random,
       cookie = Cookie,
       session_id = Session_ID,
       cipher_suites = ssl_handshake:decode_suites('2_bytes', CipherSuites),
       compression_methods = Comp_methods,
       extensions = DecodedExtensions
      };
decode_handshake(_Version, ?HELLO_VERIFY_REQUEST, <<?UINT24(_), ?UINT16(_),
						    ?UINT24(_),  ?UINT24(_),
						    ?BYTE(Major), ?BYTE(Minor),
						    ?BYTE(CookieLength),
						    Cookie:CookieLength/binary>>) ->
    #hello_verify_request{protocol_version = {Major, Minor},
			  cookie = Cookie};
decode_handshake(Version, Tag,  <<?UINT24(_), ?UINT16(_),
				  ?UINT24(_),  ?UINT24(_), Msg/binary>>) -> 
    %% DTLS specifics stripped
    decode_tls_handshake(Version, Tag, Msg).

decode_tls_handshake(Version, Tag, Msg) ->
    TLSVersion = dtls_v1:corresponding_tls_version(Version),
    ssl_handshake:decode_handshake(TLSVersion, Tag, Msg).

decode_handshake_fragments(<<>>) ->
    [<<>>];
decode_handshake_fragments(<<?BYTE(Type), ?UINT24(Length),
			     ?UINT16(MessageSeq),
			     ?UINT24(FragmentOffset), ?UINT24(FragmentLength),
			    Fragment:FragmentLength/binary, Rest/binary>>) ->
    [#handshake_fragment{type = Type, 
			length = Length,
			message_seq = MessageSeq,
			fragment_offset = FragmentOffset,
			fragment_length = FragmentLength,
			fragment = Fragment} | decode_handshake_fragments(Rest)].

reassemble(Version,  #handshake_fragment{message_seq = Seq} = Fragment, 
	   #protocol_buffers{dtls_handshake_next_seq = Seq,
			     dtls_handshake_next_fragments = Fragments0,
			     dtls_handshake_later_fragments = LaterFragments0} = 
	       Buffers0)-> 
    case reassemble_fragments(Fragment, Fragments0) of
	{more_data, Fragments} ->
	    {more_data,  Buffers0#protocol_buffers{dtls_handshake_next_fragments = Fragments}};
	{raw, RawHandshake} ->
	    Handshake = decode_handshake(Version, RawHandshake),
	    {NextFragments, LaterFragments} = next_fragments(LaterFragments0),
	    {{Handshake, RawHandshake}, Buffers0#protocol_buffers{dtls_handshake_next_seq = Seq + 1,
						  dtls_handshake_next_fragments = NextFragments,
						  dtls_handshake_later_fragments = LaterFragments}}
    end;
reassemble(_,  #handshake_fragment{message_seq = FragSeq} = Fragment, 
	   #protocol_buffers{dtls_handshake_next_seq = Seq,
			     dtls_handshake_later_fragments = LaterFragments} 
           = Buffers0) when FragSeq > Seq-> 
    {more_data,
     Buffers0#protocol_buffers{dtls_handshake_later_fragments = [Fragment | LaterFragments]}};
reassemble(_, _, Buffers) -> 
    %% Disregard fragments FragSeq < Seq
    {more_data, Buffers}.

reassemble_fragments(Current, Fragments0) ->
    [Frag1 | Frags] = lists:keysort(#handshake_fragment.fragment_offset, [Current | Fragments0]),
    [Fragment | _] = Fragments = merge_fragment(Frag1, Frags),
    case is_complete_handshake(Fragment) of
	true ->
	    {raw, handshake_bin(Fragment)};
	false ->
	    {more_data, Fragments}
    end.

merge_fragment(Frag0, []) ->
    [Frag0];
merge_fragment(Frag0, [Frag1 | Rest]) ->
    case merge_fragments(Frag0, Frag1) of
	[_|_] = Frags ->
	    Frags ++ Rest;
	Frag ->
	    merge_fragment(Frag, Rest)
    end.


%% Duplicate (fully contained fragment)
%% 2,5 _ _ P P P P P
%% 2,5 _ _ C C C C C
merge_fragments(#handshake_fragment{
		   fragment_offset = PreviousOffSet,
		   fragment_length = PreviousLen,
		   fragment = PreviousData
		  } = Previous,
		#handshake_fragment{
		   fragment_offset = PreviousOffSet,
		   fragment_length = PreviousLen,
		   fragment = PreviousData}) ->
    Previous;

%% Duplicate (fully contained fragment)
%% 2,5 _ _ P P P P P
%% 2,2 _ _ C C
%% 0,3 X X X
%% 5,3 _ _ _ _ _ X X X
merge_fragments(#handshake_fragment{
                   fragment_offset = PreviousOffset,
                   fragment_length = PreviousLen
                  } = Previous,
                #handshake_fragment{
                   fragment_offset = CurrentOffset,
                   fragment_length = CurrentLen})
  when PreviousOffset =< CurrentOffset andalso
       CurrentOffset =< PreviousOffset + PreviousLen andalso
       CurrentOffset + CurrentLen =< PreviousOffset + PreviousLen ->
    Previous;

%% Fully overlapping fragments
%% 2,5 _ _ P P P P P
%% 0,8 C C C C C C C C
merge_fragments(#handshake_fragment{
                   fragment_offset = PreviousOffset,
                   fragment_length = PreviousLen
                  },
                #handshake_fragment{
                   fragment_offset = CurrentOffset,
                   fragment_length = CurrentLen} = Current)
  when CurrentOffset =< PreviousOffset andalso
       CurrentOffset + CurrentLen >= PreviousOffset + PreviousLen ->
    Current;

%% Overlapping fragments
%% 2,5 _ _ P P P P P
%% 0,3 C C C
merge_fragments(#handshake_fragment{
                   fragment_offset = PreviousOffset,
                   fragment_length = PreviousLen,
		   fragment = PreviousData
                  } = Previous,
                #handshake_fragment{
                   fragment_offset = CurrentOffset,
                   fragment_length = CurrentLen,
                   fragment = CurrentData})
  when CurrentOffset < PreviousOffset andalso
       CurrentOffset + CurrentLen < PreviousOffset + PreviousLen ->
    NewDataLen = PreviousOffset - CurrentOffset,
    <<NewData:NewDataLen/binary, _/binary>> = CurrentData,
    Previous#handshake_fragment{
      fragment_length = PreviousLen + NewDataLen,
      fragment = <<NewData/binary, PreviousData/binary>>
     };

%% Overlapping fragments
%% 2,5 _ _ P P P P P
%% 5,3 _ _ _ _ _ C C C
merge_fragments(#handshake_fragment{
                   fragment_offset = PreviousOffset,
                   fragment_length = PreviousLen,
		   fragment = PreviousData
                  } = Previous,
                #handshake_fragment{
                   fragment_offset = CurrentOffset,
                   fragment_length = CurrentLen,
                   fragment = CurrentData})
  when CurrentOffset > PreviousOffset andalso
       CurrentOffset < PreviousOffset + PreviousLen ->
    NewDataLen = CurrentOffset + CurrentLen - (PreviousOffset + PreviousLen),
    DropLen = CurrentLen - NewDataLen,
    <<_:DropLen/binary, NewData/binary>> = CurrentData,
    Previous#handshake_fragment{
      fragment_length = PreviousLen + NewDataLen,
      fragment = <<PreviousData/binary, NewData/binary>>
     };

%% Adjacent fragments
%% 2,5 _ _ P P P P P
%% 7,3 _ _ _ _ _ _ _ C C C
merge_fragments(#handshake_fragment{
                   fragment_offset = PreviousOffset,
                   fragment_length = PreviousLen,
		   fragment = PreviousData
                  } = Previous,
                #handshake_fragment{
                   fragment_offset = CurrentOffset,
                   fragment_length = CurrentLen,
                   fragment = CurrentData})
  when CurrentOffset =:= PreviousOffset + PreviousLen ->
    Previous#handshake_fragment{
      fragment_length = PreviousLen + CurrentLen,
      fragment = <<PreviousData/binary, CurrentData/binary>>
     };

%% Adjacent fragments
%% 2,5 _ _ P P P P P
%% 0,2 C C
merge_fragments(#handshake_fragment{
                   fragment_offset = PreviousOffset,
                   fragment_length = PreviousLen,
		   fragment = PreviousData
                  } = Previous,
                #handshake_fragment{
                   fragment_offset = CurrentOffset,
                   fragment_length = CurrentLen,
                   fragment = CurrentData})
  when PreviousOffset =:= CurrentOffset + CurrentLen ->
    Previous#handshake_fragment{
      fragment_length = PreviousLen + CurrentLen,
      fragment = <<CurrentData/binary, PreviousData/binary>>
     };

%% No merge there is a gap
%% 3,5 _ _ _ P P P P
%% 0,2 C C
merge_fragments(Previous, Current) ->
    [Previous, Current].

next_fragments(LaterFragments) ->
    case lists:keysort(#handshake_fragment.message_seq, LaterFragments) of
	[] ->
	    {[], []}; 
	[#handshake_fragment{message_seq = Seq} | _] = Fragments ->
	    split_frags(Fragments, Seq, [])
    end.

split_frags([#handshake_fragment{message_seq = Seq} = Frag | Rest], Seq, Acc) ->
    split_frags(Rest, Seq, [Frag | Acc]);
split_frags(Frags, _, Acc) ->
    {lists:reverse(Acc), Frags}.

is_complete_handshake(#handshake_fragment{length = Length, fragment_length = Length}) ->
    true;
is_complete_handshake(_) ->
    false.




	    
