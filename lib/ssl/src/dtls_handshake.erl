%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2016. All Rights Reserved.
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
-module(dtls_handshake).

-include("dtls_handshake.hrl").
-include("dtls_record.hrl").
-include("ssl_internal.hrl").
-include("ssl_alert.hrl").

-export([client_hello/8, client_hello/9, hello/4,
	 hello_verify_request/1, get_dtls_handshake/2,
	 dtls_handshake_new_flight/1, dtls_handshake_new_epoch/1,
	 encode_handshake/3]).

-type dtls_handshake() :: #client_hello{} | #hello_verify_request{} | 
			  ssl_handshake:ssl_handshake().

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
client_hello(Host, Port, ConnectionStates, SslOpts,
	     Cache, CacheCb, Renegotiation, OwnCert) ->
    %% First client hello (two sent in DTLS ) uses empty Cookie
    client_hello(Host, Port, <<>>, ConnectionStates, SslOpts,
		 Cache, CacheCb, Renegotiation, OwnCert).

%%--------------------------------------------------------------------
-spec client_hello(host(), inet:port_number(), term(), #connection_states{},
		   #ssl_options{}, integer(), atom(), boolean(), der_cert()) ->
			  #client_hello{}.
%%
%% Description: Creates a client hello message.
%%--------------------------------------------------------------------
client_hello(Host, Port, Cookie, ConnectionStates,
	     #ssl_options{versions = Versions,
			  ciphers = UserSuites
			 } = SslOpts,
	     Cache, CacheCb, Renegotiation, OwnCert) ->
    Version =  dtls_record:highest_protocol_version(Versions),
    Pending = ssl_record:pending_connection_state(ConnectionStates, read),
    SecParams = Pending#connection_state.security_parameters,
    CipherSuites = ssl_handshake:available_suites(UserSuites, Version),

    Extensions = ssl_handshake:client_hello_extensions(Host, dtls_v1:corresponding_tls_version(Version), CipherSuites,
						SslOpts, ConnectionStates, Renegotiation),

    Id = ssl_session:client_id({Host, Port, SslOpts}, Cache, CacheCb, OwnCert),

    #client_hello{session_id = Id,
		  client_version = Version,
		  cipher_suites = ssl_handshake:cipher_suites(CipherSuites, Renegotiation),
		  compression_methods = ssl_record:compressions(),
		  random = SecParams#security_parameters.client_random,
		  cookie = Cookie,
		  extensions = Extensions
		 }.

hello(#server_hello{server_version = Version, random = Random,
		    cipher_suite = CipherSuite,
		    compression_method = Compression,
		    session_id = SessionId, extensions = HelloExt},
      #ssl_options{versions = SupportedVersions} = SslOpt,
      ConnectionStates0, Renegotiation) ->
    case dtls_record:is_acceptable_version(Version, SupportedVersions) of
	true ->
	    handle_server_hello_extensions(Version, SessionId, Random, CipherSuite,
					   Compression, HelloExt, SslOpt, ConnectionStates0, Renegotiation);
	false ->
	    ?ALERT_REC(?FATAL, ?PROTOCOL_VERSION)
    end;

hello(#client_hello{client_version = ClientVersion} = Hello,
      #ssl_options{versions = Versions} = SslOpts,
      Info, Renegotiation) ->
    Version = ssl_handshake:select_version(dtls_record, ClientVersion, Versions),
    %%
    %% TODO: handle Cipher Fallback
    %%
    handle_client_hello(Version, Hello, SslOpts, Info, Renegotiation).

-spec hello_verify_request(binary()) -> #hello_verify_request{}.
%%
%% Description: Creates a hello verify request message sent by server to
%% verify client
%%--------------------------------------------------------------------
hello_verify_request(Cookie) ->
    %% TODO: DTLS Versions?????
    #hello_verify_request{protocol_version = {254, 255}, cookie = Cookie}.

%%--------------------------------------------------------------------

%% %%--------------------------------------------------------------------
encode_handshake(Handshake, Version, MsgSeq) ->
    {MsgType, Bin} = enc_handshake(Handshake, Version),
    Len = byte_size(Bin),
    Enc = [MsgType, ?uint24(Len), ?uint16(MsgSeq), ?uint24(0), ?uint24(Len), Bin],
    Frag = {MsgType, MsgSeq, Bin},
    {Enc, Frag}.

%%--------------------------------------------------------------------
-spec get_dtls_handshake(#ssl_tls{}, #dtls_hs_state{} | undefined) ->
				{[dtls_handshake()], #dtls_hs_state{}} | {retransmit, #dtls_hs_state{}}.
%%
%% Description: Given a DTLS state and new data from ssl_record, collects
%% and returns it as a list of handshake messages, also returns a new
%% DTLS state
%%--------------------------------------------------------------------
get_dtls_handshake(Records, undefined) ->
    HsState = #dtls_hs_state{highest_record_seq = 0,
			     starting_read_seq = 0,
			     fragments = gb_trees:empty(),
			     completed = []},
    get_dtls_handshake(Records, HsState);
get_dtls_handshake(Records, HsState0) when is_list(Records) ->
    HsState1 = lists:foldr(fun get_dtls_handshake_aux/2, HsState0, Records),
    get_dtls_handshake_completed(HsState1);
get_dtls_handshake(Record, HsState0) when is_record(Record, ssl_tls) ->
    HsState1 = get_dtls_handshake_aux(Record, HsState0),
    get_dtls_handshake_completed(HsState1).

%%--------------------------------------------------------------------
-spec dtls_handshake_new_epoch(#dtls_hs_state{}) -> #dtls_hs_state{}.
%%
%% Description: Reset the DTLS decoder state for a new Epoch
%%--------------------------------------------------------------------
%% dtls_handshake_new_epoch(<<>>) ->
%%     dtls_hs_state_init();
dtls_handshake_new_epoch(HsState) ->
    HsState#dtls_hs_state{highest_record_seq = 0,
			  starting_read_seq = HsState#dtls_hs_state.current_read_seq,
			  fragments = gb_trees:empty(), completed = []}.

%--------------------------------------------------------------------
-spec dtls_handshake_new_flight(integer() | undefined) -> #dtls_hs_state{}.
%
% Description: Init the DTLS decoder state for a new Flight
dtls_handshake_new_flight(ExpectedReadReq) ->
    #dtls_hs_state{current_read_seq = ExpectedReadReq,
		   highest_record_seq = 0,
		   starting_read_seq = 0,
		   fragments = gb_trees:empty(), completed = []}.

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
    case dtls_record:is_acceptable_version(Version, Versions) of
	true ->
	    AvailableHashSigns = ssl_handshake:available_signature_algs(
				   ClientHashSigns, SupportedHashSigns, Cert,
				   dtls_v1:corresponding_tls_version(Version)),
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

handle_client_hello_extensions(Version, Type, Random, CipherSuites,
			HelloExt, SslOpts, Session0, ConnectionStates0, Renegotiation, HashSign) ->
    try ssl_handshake:handle_client_hello_extensions(dtls_record, Random, CipherSuites,
						     HelloExt, dtls_v1:corresponding_tls_version(Version),
						     SslOpts, Session0, ConnectionStates0, Renegotiation) of
	#alert{} = Alert ->
	    Alert;
	{Session, ConnectionStates, Protocol, ServerHelloExt} ->
	    {Version, {Type, Session}, ConnectionStates, Protocol, ServerHelloExt, HashSign}
    catch throw:Alert ->
	    Alert
    end.

handle_server_hello_extensions(Version, SessionId, Random, CipherSuite,
			       Compression, HelloExt, SslOpt, ConnectionStates0, Renegotiation) ->
    case ssl_handshake:handle_server_hello_extensions(dtls_record, Random, CipherSuite,
						      Compression, HelloExt,
						      dtls_v1:corresponding_tls_version(Version),
						      SslOpt, ConnectionStates0, Renegotiation) of
	#alert{} = Alert ->
	    Alert;
	{ConnectionStates, ProtoExt, Protocol} ->
	    {Version, SessionId, ConnectionStates, ProtoExt, Protocol}
    end.

get_dtls_handshake_completed(HsState = #dtls_hs_state{completed = Completed}) ->
    {lists:reverse(Completed), HsState#dtls_hs_state{completed = []}}.

get_dtls_handshake_aux(#ssl_tls{version = Version,
 				sequence_number = SeqNo,
 				fragment = Data}, HsState) ->
    get_dtls_handshake_aux(Version, SeqNo, Data, HsState).

get_dtls_handshake_aux(Version, SeqNo,
 		       <<?BYTE(Type), ?UINT24(Length),
 			 ?UINT16(MessageSeq),
 			 ?UINT24(FragmentOffset), ?UINT24(FragmentLength),
 			 Body:FragmentLength/binary, Rest/binary>>,
 		       HsState0) ->
    case reassemble_dtls_fragment(SeqNo, Type, Length, MessageSeq,
				  FragmentOffset, FragmentLength,
				  Body, HsState0) of
 	{HsState1, HighestSeqNo, MsgBody} ->
 	    HsState2 = dec_dtls_fragment(Version, HighestSeqNo, Type, Length, MessageSeq, MsgBody, HsState1),
 	    HsState3 = process_dtls_fragments(Version, HsState2),
 	    get_dtls_handshake_aux(Version, SeqNo, Rest, HsState3);

 	HsState2 ->
 	    HsState3 = process_dtls_fragments(Version, HsState2),
 	    get_dtls_handshake_aux(Version, SeqNo, Rest, HsState3)
     end;

get_dtls_handshake_aux(_Version, _SeqNo, <<>>, HsState) ->
    HsState.

dec_dtls_fragment(Version, SeqNo, Type, Length, MessageSeq, MsgBody,
 		  HsState = #dtls_hs_state{highest_record_seq = HighestSeqNo, completed = Acc}) ->
    Raw = <<?BYTE(Type), ?UINT24(Length), ?UINT16(MessageSeq), ?UINT24(0), ?UINT24(Length), MsgBody/binary>>,
    H = decode_handshake(Version, Type, MsgBody),
    HsState#dtls_hs_state{completed = [{H,Raw}|Acc], highest_record_seq = erlang:max(HighestSeqNo, SeqNo)}.

process_dtls_fragments(Version,
 		       HsState0 = #dtls_hs_state{current_read_seq = CurrentReadSeq,
 						 fragments = Fragments0}) ->
    case gb_trees:is_empty(Fragments0) of
 	true ->
 	    HsState0;
 	_ ->
 	    case gb_trees:smallest(Fragments0) of
 		{CurrentReadSeq, {SeqNo, Type, Length, CurrentReadSeq, {Length, [{0, Length}], MsgBody}}} ->
 		    HsState1 = dtls_hs_state_process_seq(HsState0),
 		    HsState2 = dec_dtls_fragment(Version, SeqNo, Type, Length, CurrentReadSeq, MsgBody, HsState1),
 		    process_dtls_fragments(Version, HsState2);
 		_ ->
 		    HsState0
 	    end
     end.

dtls_hs_state_process_seq(HsState0 = #dtls_hs_state{current_read_seq = CurrentReadSeq,
 						    fragments = Fragments0}) ->
    Fragments1 = gb_trees:delete_any(CurrentReadSeq, Fragments0),
    HsState0#dtls_hs_state{current_read_seq = CurrentReadSeq + 1,
 			   fragments = Fragments1}.

dtls_hs_state_add_fragment(MessageSeq, Fragment, HsState0 = #dtls_hs_state{fragments = Fragments0}) ->
    Fragments1 = gb_trees:enter(MessageSeq, Fragment, Fragments0),
    HsState0#dtls_hs_state{fragments = Fragments1}.

reassemble_dtls_fragment(SeqNo, Type, Length, MessageSeq, 0, Length,
 			 Body, HsState0 = #dtls_hs_state{current_read_seq = undefined})
  when Type == ?CLIENT_HELLO;
       Type == ?SERVER_HELLO;
        Type == ?HELLO_VERIFY_REQUEST ->
    %% First message, should be client hello
    %% return the current message and set the next expected Sequence
    %%
    %% Note: this could (should?) be restricted further, ClientHello and
    %%       HelloVerifyRequest have to have message_seq = 0, ServerHello
    %%       can have a message_seq of 0 or 1
    %%
    {HsState0#dtls_hs_state{current_read_seq = MessageSeq + 1}, SeqNo, Body};

reassemble_dtls_fragment(_SeqNo, _Type, Length, _MessageSeq, _, Length,
			 _Body, HsState = #dtls_hs_state{current_read_seq = undefined}) ->
    %% not what we expected, drop it
    HsState;

reassemble_dtls_fragment(SeqNo, _Type, Length, MessageSeq, 0, Length,
 			 Body, HsState0 =
 			     #dtls_hs_state{starting_read_seq = StartingReadSeq})
  when MessageSeq < StartingReadSeq ->
    %% this has to be the start of a new flight, let it through
    %%
    %% Note: this could (should?) be restricted further, the first message of a
    %%       new flight has to have message_seq = 0
    %%
    HsState = dtls_hs_state_process_seq(HsState0),
    {HsState, SeqNo, Body};

reassemble_dtls_fragment(_SeqNo, _Type, Length, MessageSeq, 0, Length,
 			 _Body, HsState = #dtls_hs_state{current_read_seq = CurrentReadSeq})
  when MessageSeq < CurrentReadSeq ->
    HsState;

reassemble_dtls_fragment(SeqNo, _Type, Length, MessageSeq, 0, Length,
 			 Body, HsState0 = #dtls_hs_state{current_read_seq = MessageSeq}) ->
    %% Message fully contained and it's the current seq
    HsState1 = dtls_hs_state_process_seq(HsState0),
    {HsState1, SeqNo, Body};

reassemble_dtls_fragment(SeqNo, Type, Length, MessageSeq, 0, Length,
 			 Body, HsState) ->
    %% Message fully contained and it's the NOT the current seq -> buffer
    Fragment = {SeqNo, Type, Length, MessageSeq,
 		dtls_fragment_init(Length, 0, Length, Body)},
    dtls_hs_state_add_fragment(MessageSeq, Fragment, HsState);

reassemble_dtls_fragment(_SeqNo, _Type, Length, MessageSeq, FragmentOffset, FragmentLength,
 			 _Body,
 			 HsState = #dtls_hs_state{current_read_seq = CurrentReadSeq})
  when FragmentOffset + FragmentLength == Length andalso MessageSeq == (CurrentReadSeq - 1) ->
    {retransmit, HsState};

reassemble_dtls_fragment(_SeqNo, _Type, _Length, MessageSeq, _FragmentOffset, _FragmentLength,
 			 _Body,
 			 HsState = #dtls_hs_state{current_read_seq = CurrentReadSeq})
  when MessageSeq < CurrentReadSeq ->
    HsState;

reassemble_dtls_fragment(SeqNo, Type, Length, MessageSeq,
 			 FragmentOffset, FragmentLength,
 			 Body,
 			 HsState = #dtls_hs_state{fragments = Fragments0}) ->
    case gb_trees:lookup(MessageSeq, Fragments0) of
 	{value, Fragment} ->
 	    dtls_fragment_reassemble(SeqNo, Type, Length, MessageSeq,
 				     FragmentOffset, FragmentLength,
 				     Body, Fragment, HsState);
 	none ->
 	    dtls_fragment_start(SeqNo, Type, Length, MessageSeq,
 				FragmentOffset, FragmentLength,
 				Body, HsState)
    end.

dtls_fragment_start(SeqNo, Type, Length, MessageSeq,
		    FragmentOffset, FragmentLength,
		    Body, HsState = #dtls_hs_state{fragments = Fragments0}) ->
    Fragment = {SeqNo, Type, Length, MessageSeq,
 		dtls_fragment_init(Length, FragmentOffset, FragmentLength, Body)},
     Fragments1 = gb_trees:insert(MessageSeq, Fragment, Fragments0),
    HsState#dtls_hs_state{fragments = Fragments1}.

dtls_fragment_reassemble(SeqNo, Type, Length, MessageSeq,
			 FragmentOffset, FragmentLength,
 			 Body,
 			 {LastSeqNo, Type, Length, MessageSeq, FragBuffer0},
 			 HsState = #dtls_hs_state{fragments = Fragments0}) ->
    FragBuffer1 = dtls_fragment_add(FragBuffer0, FragmentOffset, FragmentLength, Body),
    Fragment = {erlang:max(SeqNo, LastSeqNo), Type, Length, MessageSeq, FragBuffer1},
    Fragments1 = gb_trees:enter(MessageSeq, Fragment, Fragments0),
    HsState#dtls_hs_state{fragments = Fragments1};

%% Type, Length or Seq mismatch, drop everything...
%% Note: the RFC is not clear on how to handle this...
dtls_fragment_reassemble(_SeqNo, _Type, _Length, MessageSeq,
 			 _FragmentOffset, _FragmentLength, _Body, _Fragment,
 			 HsState = #dtls_hs_state{fragments = Fragments0}) ->
    Fragments1 = gb_trees:delete_any(MessageSeq, Fragments0),
    HsState#dtls_hs_state{fragments = Fragments1}.

dtls_fragment_add({Length, FragmentList0, Bin0}, FragmentOffset, FragmentLength, Body) ->
    Bin1 = dtls_fragment_bin_add(FragmentOffset, FragmentLength, Body, Bin0),
    FragmentList1 = add_fragment(FragmentList0, {FragmentOffset, FragmentLength}),
    {Length, FragmentList1, Bin1}.

dtls_fragment_init(Length, 0, Length, Body) ->
    {Length, [{0, Length}], Body};
dtls_fragment_init(Length, FragmentOffset, FragmentLength, Body) ->
    Bin = dtls_fragment_bin_add(FragmentOffset, FragmentLength, Body, <<0:(Length*8)>>),
    {Length, [{FragmentOffset, FragmentOffset + FragmentLength}], Bin}.

dtls_fragment_bin_add(FragmentOffset, FragmentLength, Add, Buffer) ->
    <<First:FragmentOffset/bytes, _:FragmentLength/bytes, Rest/binary>> = Buffer,
    <<First/binary, Add/binary, Rest/binary>>.

merge_fragment_list([], Fragment, Acc) ->
    lists:reverse([Fragment|Acc]);

merge_fragment_list([H = {_, HEnd}|Rest], Frag = {FStart, _}, Acc)
  when FStart > HEnd ->
    merge_fragment_list(Rest, Frag, [H|Acc]);

merge_fragment_list(Rest = [{HStart, _HEnd}|_], Frag = {_FStart, FEnd}, Acc)
  when FEnd < HStart ->
    lists:reverse(Acc) ++ [Frag|Rest];

merge_fragment_list([{HStart, HEnd}|Rest], _Frag = {FStart, FEnd}, Acc)
   when
      FStart =< HEnd orelse FEnd >= HStart ->
    Start = erlang:min(HStart, FStart),
    End = erlang:max(HEnd, FEnd),
    NewFrag = {Start, End},
    merge_fragment_list(Rest, NewFrag, Acc).

add_fragment(List, {FragmentOffset, FragmentLength}) ->
    merge_fragment_list(List, {FragmentOffset, FragmentOffset + FragmentLength}, []).

enc_handshake(#hello_verify_request{protocol_version = {Major, Minor},
 				       cookie = Cookie}, _Version) ->
     CookieLength = byte_size(Cookie),
    {?HELLO_VERIFY_REQUEST, <<?BYTE(Major), ?BYTE(Minor),
 			      ?BYTE(CookieLength),
 			      Cookie/binary>>};

enc_handshake(#hello_request{}, _Version) ->
    {?HELLO_REQUEST, <<>>};
enc_handshake(#client_hello{client_version = {Major, Minor},
			       random = Random,
			       session_id = SessionID,
			       cookie = Cookie,
			       cipher_suites = CipherSuites,
			       compression_methods = CompMethods,
			       extensions = HelloExtensions}, Version) ->
    SIDLength = byte_size(SessionID),
    BinCookie = enc_client_hello_cookie(Version, Cookie),
    BinCompMethods = list_to_binary(CompMethods),
    CmLength = byte_size(BinCompMethods),
    BinCipherSuites = list_to_binary(CipherSuites),
    CsLength = byte_size(BinCipherSuites),
    ExtensionsBin = ssl_handshake:encode_hello_extensions(HelloExtensions),
    
    {?CLIENT_HELLO, <<?BYTE(Major), ?BYTE(Minor), Random:32/binary,
 		      ?BYTE(SIDLength), SessionID/binary,
 		      BinCookie/binary,
		      ?UINT16(CsLength), BinCipherSuites/binary,
 		      ?BYTE(CmLength), BinCompMethods/binary, ExtensionsBin/binary>>};
enc_handshake(HandshakeMsg, Version) ->
    ssl_handshake:encode_handshake(HandshakeMsg, Version).

enc_client_hello_cookie(_, <<>>) ->
    <<>>;
enc_client_hello_cookie(_, Cookie) ->
    CookieLength = byte_size(Cookie),
    <<?BYTE(CookieLength), Cookie/binary>>.

decode_handshake(_Version, ?CLIENT_HELLO, <<?BYTE(Major), ?BYTE(Minor), Random:32/binary,
					    ?BYTE(SID_length), Session_ID:SID_length/binary,
					    ?BYTE(Cookie_length), Cookie:Cookie_length/binary,
					    ?UINT16(Cs_length), CipherSuites:Cs_length/binary,
					    ?BYTE(Cm_length), Comp_methods:Cm_length/binary,
					    Extensions/binary>>) ->
    
    DecodedExtensions = ssl_handshake:decode_hello_extensions(Extensions),
    
    #client_hello{
       client_version = {Major,Minor},
       random = Random,
        session_id = Session_ID,
       cookie = Cookie,
       cipher_suites = ssl_handshake:decode_suites('2_bytes', CipherSuites),
       compression_methods = Comp_methods,
       extensions = DecodedExtensions
       };

decode_handshake(_Version, ?HELLO_VERIFY_REQUEST, <<?BYTE(Major), ?BYTE(Minor),
						    ?BYTE(CookieLength), Cookie:CookieLength/binary>>) ->

    #hello_verify_request{
       protocol_version = {Major,Minor},
       cookie = Cookie};
decode_handshake(Version, Tag, Msg) ->
    ssl_handshake:decode_handshake(Version, Tag, Msg).

%% address_to_bin({A,B,C,D}, Port) ->
%%     <<0:80,16#ffff:16,A,B,C,D,Port:16>>;
%% address_to_bin({A,B,C,D,E,F,G,H}, Port) ->
%%     <<A:16,B:16,C:16,D:16,E:16,F:16,G:16,H:16,Port:16>>.
