%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2013. All Rights Reserved.
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
-module(dtls_handshake).

-include("dtls_handshake.hrl").
-include("dtls_record.hrl").
-include("ssl_internal.hrl").

-export([get_dtls_handshake/2,
	 dtls_handshake_new_flight/1, dtls_handshake_new_epoch/1,
	 encode_handshake/4]).

-record(dtls_hs_state, {current_read_seq, starting_read_seq, highest_record_seq, fragments, completed}).

-type dtls_handshake() :: #client_hello{} | #server_hello{} |   #hello_verify_request{} |
			 #server_hello_done{} | #certificate{} | #certificate_request{} |
			 #client_key_exchange{} | #finished{} | #certificate_verify{} |
			 #hello_request{} | #next_protocol{}.

%%====================================================================
%% Internal application API
%%====================================================================
encode_handshake(Package, Version, MsgSeq, Mss) ->
    {MsgType, Bin} = enc_hs(Package, Version),
    Len = byte_size(Bin),
    HsHistory = [MsgType, ?uint24(Len), ?uint16(MsgSeq), ?uint24(0), ?uint24(Len), Bin],
    BinMsg = dtls_split_handshake(Mss, MsgType, Len, MsgSeq, Bin, 0, []),
    {HsHistory, BinMsg}.

%--------------------------------------------------------------------
-spec get_dtls_handshake(#ssl_tls{}, #dtls_hs_state{} | binary()) ->
     {[dtls_handshake()], #ssl_tls{}}.
%
% Description: Given a DTLS state and new data from ssl_record, collects
% and returns it as a list of handshake messages, also returns a new
% DTLS state
%--------------------------------------------------------------------
% get_dtls_handshake(Record, <<>>) ->
%     get_dtls_handshake_aux(Record, dtls_hs_state_init());
get_dtls_handshake(Record, HsState) ->
    get_dtls_handshake_aux(Record, HsState).

%--------------------------------------------------------------------
-spec dtls_handshake_new_epoch(#dtls_hs_state{}) -> #dtls_hs_state{}.
%
% Description: Reset the DTLS decoder state for a new Epoch
%--------------------------------------------------------------------
% dtls_handshake_new_epoch(<<>>) ->
%     dtls_hs_state_init();
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

dtls_split_handshake(Mss, MsgType, Len, MsgSeq, Bin, Offset, Acc)
  when byte_size(Bin) + 12 < Mss ->
    FragmentLen = byte_size(Bin),
    BinMsg = [MsgType, ?uint24(Len), ?uint16(MsgSeq), ?uint24(Offset), ?uint24(FragmentLen), Bin],
    lists:reverse([BinMsg|Acc]);
dtls_split_handshake(Mss, MsgType, Len, MsgSeq, Bin, Offset, Acc) ->
    FragmentLen = Mss - 12,
    <<Fragment:FragmentLen/bytes, Rest/binary>> = Bin,
    BinMsg = [MsgType, ?uint24(Len), ?uint16(MsgSeq), ?uint24(Offset), ?uint24(FragmentLen), Fragment],
    dtls_split_handshake(Mss, MsgType, Len, MsgSeq, Rest, Offset + FragmentLen, [BinMsg|Acc]).

get_dtls_handshake_aux(#ssl_tls{version = Version,
				record_seq = SeqNo,
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
	{retransmit, HsState1} ->
	    case Rest of
		<<>> ->
		    {retransmit, HsState1};
		_ ->
		    get_dtls_handshake_aux(Version, SeqNo, Rest, HsState1)
	    end;
	{HsState1, HighestSeqNo, MsgBody} ->
	    HsState2 = dec_dtls_fragment(Version, HighestSeqNo, Type, Length, MessageSeq, MsgBody, HsState1),
	    HsState3 = process_dtls_fragments(Version, HsState2),
	    get_dtls_handshake_aux(Version, SeqNo, Rest, HsState3);
	HsState2 ->
	    HsState3 = process_dtls_fragments(Version, HsState2),
	    get_dtls_handshake_aux(Version, SeqNo, Rest, HsState3)
    end;

get_dtls_handshake_aux(_Version, _SeqNo, <<>>, HsState) ->
    {lists:reverse(HsState#dtls_hs_state.completed),
     HsState#dtls_hs_state.highest_record_seq,
     HsState#dtls_hs_state{completed = []}}.

dec_dtls_fragment(Version, SeqNo, Type, Length, MessageSeq, MsgBody,
		  HsState = #dtls_hs_state{highest_record_seq = HighestSeqNo, completed = Acc}) ->
    Raw = <<?BYTE(Type), ?UINT24(Length), ?UINT16(MessageSeq), ?UINT24(0), ?UINT24(Length), MsgBody/binary>>,
    H = dec_hs(Version, Type, MsgBody),
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
			 _Body, HsState =
			     #dtls_hs_state{current_read_seq = CurrentReadSeq})
  when MessageSeq < CurrentReadSeq ->
    {retransmit, HsState};

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
    {Length, [{FragmentOffset, FragmentLength}], Bin}.

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


dec_hs(_Version, ?HELLO_VERIFY_REQUEST, <<?BYTE(Major), ?BYTE(Minor),
		       ?BYTE(CookieLength), Cookie:CookieLength/binary>>)
  when Major >= 128 ->

    #hello_verify_request{
	protocol_version = {Major, Minor},
        cookie = Cookie};

dec_hs(_Version, ?CLIENT_HELLO, <<?BYTE(Major), ?BYTE(Minor), Random:32/binary,
		       ?BYTE(SID_length), Session_ID:SID_length/binary,
		       ?BYTE(Cookie_length), Cookie:Cookie_length/binary,
		       ?UINT16(Cs_length), CipherSuites:Cs_length/binary,
		       ?BYTE(Cm_length), Comp_methods:Cm_length/binary,
		       Extensions/binary>>)
  when Major >= 128 ->

    DecodedExtensions = tls_handshake:dec_hello_extensions(Extensions),
    RenegotiationInfo = proplists:get_value(renegotiation_info, DecodedExtensions, undefined),
    SRP = proplists:get_value(srp, DecodedExtensions, undefined),
    HashSigns = proplists:get_value(hash_signs, DecodedExtensions, undefined),
    EllipticCurves = proplists:get_value(elliptic_curves, DecodedExtensions,
					 undefined),
    NextProtocolNegotiation = proplists:get_value(next_protocol_negotiation, DecodedExtensions, undefined),

    #client_hello{
       client_version = {Major,Minor},
       random = Random,
       session_id = Session_ID,
       cookie = Cookie,
       cipher_suites = tls_handshake:decode_suites('2_bytes', CipherSuites),
       compression_methods = Comp_methods,
       renegotiation_info = RenegotiationInfo,
       srp = SRP,
       hash_signs = HashSigns,
       elliptic_curves = EllipticCurves,
       next_protocol_negotiation = NextProtocolNegotiation
      }.

enc_hs(#hello_verify_request{protocol_version = {Major, Minor},
			     cookie = Cookie}, _Version) ->
    CookieLength = byte_size(Cookie),
    {?HELLO_VERIFY_REQUEST, <<?BYTE(Major), ?BYTE(Minor),
			      ?BYTE(CookieLength),
			      Cookie/binary>>};

enc_hs(#client_hello{client_version = {Major, Minor},
		     random = Random,
		     session_id = SessionID,
		     cookie = Cookie,
		     cipher_suites = CipherSuites,
		     compression_methods = CompMethods,
		     renegotiation_info = RenegotiationInfo,
		     srp = SRP,
		     hash_signs = HashSigns,
		     ec_point_formats = EcPointFormats,
		     elliptic_curves = EllipticCurves,
		     next_protocol_negotiation = NextProtocolNegotiation}, Version) ->
    SIDLength = byte_size(SessionID),
    BinCookie = enc_client_hello_cookie(Version, Cookie),
    BinCompMethods = list_to_binary(CompMethods),
    CmLength = byte_size(BinCompMethods),
    BinCipherSuites = list_to_binary(CipherSuites),
    CsLength = byte_size(BinCipherSuites),
    Extensions = tls_handshake:hello_extensions(RenegotiationInfo, SRP, NextProtocolNegotiation)
	++ tls_handshake:ec_hello_extensions(lists:map(fun ssl_cipher:suite_definition/1, CipherSuites), EcPointFormats)
	++ tls_handshake:ec_hello_extensions(lists:map(fun ssl_cipher:suite_definition/1, CipherSuites), EllipticCurves)
	++ tls_handshake:hello_extensions(HashSigns),
    ExtensionsBin = tls_handshake:enc_hello_extensions(Extensions),

    {?CLIENT_HELLO, <<?BYTE(Major), ?BYTE(Minor), Random:32/binary,
		      ?BYTE(SIDLength), SessionID/binary,
		      BinCookie/binary,
		      ?UINT16(CsLength), BinCipherSuites/binary,
		     ?BYTE(CmLength), BinCompMethods/binary, ExtensionsBin/binary>>}.

enc_client_hello_cookie(_, Cookie) ->
    CookieLength = byte_size(Cookie),
    <<?BYTE(CookieLength), Cookie/binary>>;
enc_client_hello_cookie(_, _) ->
    <<>>.

dec_hs(_Version, ?CLIENT_HELLO, <<?BYTE(Major), ?BYTE(Minor), Random:32/binary,
		       ?BYTE(SID_length), Session_ID:SID_length/binary,
		       ?BYTE(Cookie_length), Cookie:Cookie_length/binary,
		       ?UINT16(Cs_length), CipherSuites:Cs_length/binary,
		       ?BYTE(Cm_length), Comp_methods:Cm_length/binary,
		       Extensions/binary>>) ->

    DecodedExtensions = dec_hello_extensions(Extensions),
    RenegotiationInfo = proplists:get_value(renegotiation_info, DecodedExtensions, undefined),
    SRP = proplists:get_value(srp, DecodedExtensions, undefined),
    HashSigns = proplists:get_value(hash_signs, DecodedExtensions, undefined),
    EllipticCurves = proplists:get_value(elliptic_curves, DecodedExtensions,
					 undefined),
    NextProtocolNegotiation = proplists:get_value(next_protocol_negotiation, DecodedExtensions, undefined),

    #client_hello{
       client_version = {Major,Minor},
       random = Random,
       session_id = Session_ID,
       cookie = Cookie,
       cipher_suites = from_2bytes(CipherSuites),
       compression_methods = Comp_methods,
       renegotiation_info = RenegotiationInfo,
	srp = SRP,
       hash_signs = HashSigns,
       elliptic_curves = EllipticCurves,
       next_protocol_negotiation = NextProtocolNegotiation
      };

dec_hs(_Version, ?HELLO_VERIFY_REQUEST, <<?BYTE(Major), ?BYTE(Minor),
		       ?BYTE(CookieLength), Cookie:CookieLength/binary>>) ->

    #hello_verify_request{
	server_version = {Major,Minor},
       cookie = Cookie};
