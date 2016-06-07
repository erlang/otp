%% 
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2016. All Rights Reserved.
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

-module(snmp_pdus).

-define(SNMP_USE_V3, true).
-include("snmp_types.hrl").

-define(VMODULE,"PDUS").
-include("snmp_verbosity.hrl").

%% See RFC1155, RFC1157, RFC1901, RFC1902, RFC1905, RFC2272


%% API
-export([enc_message/1, enc_message_only/1, enc_pdu/1,
	 enc_varbind/1, 
	 enc_oct_str_tag/1, enc_scoped_pdu/1,
	 enc_usm_security_parameters/1,
	 dec_message/1, dec_message_only/1, dec_pdu/1,
	 dec_scoped_pdu_data/1, dec_scoped_pdu/1,
	 dec_usm_security_parameters/1,
	 strip_encrypted_scoped_pdu_data/1,
	 octet_str_to_bits/1, bits_to_str/1,
	 get_encoded_length/1,
	 enc_value/2, dec_value/1]).

%% -compile(export_all).

%% Returns the number of octets required to encode Length.
get_encoded_length(Length) ->
    length(elength(Length)).

dec_message([48 | Bytes]) ->
    Bytes2 = get_data_bytes(Bytes),
    case dec_snmp_version(Bytes2) of
	{'version-3', Rest} ->
	    dec_rest_v3_msg(Rest);
	{Vsn, Rest} -> % 1 or 2
	    dec_rest_v1_v2_msg(Vsn, Rest)
    end.

dec_message_only([48 | Bytes]) ->
    Bytes2 = get_data_bytes(Bytes),
    case dec_snmp_version(Bytes2) of
	{'version-3', Rest} ->
	    dec_rest_v3_msg_only(Rest);
	{Vsn, Rest} -> % 1 or 2
	    dec_rest_v1_v2_msg_only(Vsn, Rest)
    end.

dec_snmp_version(Bytes) ->
    case (catch dec_int_tag(Bytes, 10)) of
	{error, {bad_integer, BadInt}} ->
	    exit({bad_version, BadInt});
	{SNMPversion, Rest} when is_integer(SNMPversion) andalso is_list(Rest) ->
	    {dec_snmp_ver(SNMPversion), Rest};
	{'EXIT', Reason} ->
	    exit(Reason)
    end.
	    
	    
dec_snmp_ver(0) ->
    'version-1';
dec_snmp_ver(1) ->
    'version-2';
dec_snmp_ver(3) ->
    'version-3';
dec_snmp_ver(Vsn) ->
    exit({bad_version, Vsn}).

dec_rest_v1_v2_msg(Vsn, Rest1) ->
    {Community, Rest2} = dec_oct_str_tag(Rest1),
    PDU = dec_pdu(Rest2),
    #message{version = Vsn, vsn_hdr = Community, data = PDU}.

dec_rest_v1_v2_msg_only(Vsn, Rest1) ->
    {Community, Rest2} = dec_oct_str_tag(Rest1),
    #message{version = Vsn, vsn_hdr = Community, data = Rest2}.

dec_rest_v3_msg_only([48 | Bytes]) -> % starts with header data sequence
    {Size, Tail} = dec_len(Bytes),
    {HBytes, Bytes1} = split_at(Tail, Size, []),
    %% Decode HeaderData
    {MsgID, HBytes1} = dec_int_tag(HBytes),
    chk_msg_id(MsgID),
    {MsgMaxSize, HBytes2} = dec_int_tag(HBytes1),
    chk_msg_max_size(MsgMaxSize),
    {MsgFlags, HBytes3} = dec_oct_str_tag(HBytes2),
    {MsgSecurityModel, []} = dec_int_tag(HBytes3),
    chk_msg_sec_model(MsgSecurityModel),
    %% Continue with Message
%    {MsgSecurityParameters, Bytes2} = dec_oct_str_tag(Bytes1),

    [4 | Bytes1a] = Bytes1,
    {Size1a, Tail1a} = dec_len(Bytes1a),
    {MsgSecurityParameters, Bytes2} = split_at(Tail1a, Size1a, []),

    %% [48 , HdrDataLen, HdrData, 4, MsgSecLen, MsgSec, ...]
    %% NOTE: HdrDataLen is always so small that one octet is enough to
    %%         encode its length.
    %%       MsgSecLen is worse... but for USM, it is small enough for
    %%         one octet.  USM is currently the only secmodel.
    %% 1 + 1 + Size + 1 + 1 + Size1a
    HdrSize = Size + Size1a + 4,
    V3Hdr = #v3_hdr{msgID = MsgID,
		    msgMaxSize = MsgMaxSize,
		    msgFlags = MsgFlags, %dec_msg_flags(MsgFlags),
		    msgSecurityModel = MsgSecurityModel,
		    msgSecurityParameters = MsgSecurityParameters,
		    hdr_size = HdrSize},
    #message{version = 'version-3', vsn_hdr = V3Hdr, data = Bytes2}.

dec_rest_v3_msg(Bytes) -> 
    Message = dec_rest_v3_msg_only(Bytes),
    Data = Message#message.data,
    Message#message{data = dec_scoped_pdu_data(Data)}.

dec_scoped_pdu_data([48 | Bytes]) -> % plaintext
    {ScopedPdu, []} = dec_scoped_pdu_notag(Bytes),
    ScopedPdu;
dec_scoped_pdu_data([4 | Bytes]) -> % encryptedPDU
    {EncryptedPDU, []} = dec_oct_str_notag(Bytes),
    EncryptedPDU.

    
dec_scoped_pdu([48 | Bytes]) ->
    element(1, dec_scoped_pdu_notag(Bytes)).

dec_scoped_pdu_notag(Bytes) ->
    Bytes1 = get_data_bytes(Bytes),
    {ContextEngineID, Bytes2} = dec_oct_str_tag(Bytes1),
    {ContextName, Bytes3} = dec_oct_str_tag(Bytes2),
    Pdu = dec_pdu(Bytes3),
    {#scopedPdu{contextEngineID = ContextEngineID,
		contextName = ContextName,
		data = Pdu},
     []}.

dec_pdu_tag(160) ->
    'get-request';
dec_pdu_tag(161) ->
    'get-next-request';
dec_pdu_tag(162) ->
    'get-response';
dec_pdu_tag(163) ->
    'set-request';
%% 164 SNMPv1 Trap
%% 165 Bulk
dec_pdu_tag(166) ->
    'inform-request';
dec_pdu_tag(167) ->
    'snmpv2-trap';
dec_pdu_tag(168) ->
    report.


dec_pdu([164 | Bytes]) ->      % It's a trap
    Bytes2 = get_data_bytes(Bytes),
    {Enterprise, Rest1} = dec_oid_tag(Bytes2),
    {{'IpAddress', [_, _, _, _] = AgentAddr}, Rest2} = dec_value(Rest1),
    {GenericTrap, Rest3} = dec_int_tag(Rest2),
    {SpecificTrap, Rest4} = dec_int_tag(Rest3),
    {{'TimeTicks', TimeStamp}, VBBytes} = dec_value(Rest4),
    VBs = dec_VBs(VBBytes),
    #trappdu{enterprise = Enterprise, agent_addr = AgentAddr,
	     generic_trap = GenericTrap, specific_trap = SpecificTrap,
	     time_stamp = TimeStamp, varbinds = VBs};

dec_pdu([165 | Bytes]) ->    % Bulk
    Bytes2 = get_data_bytes(Bytes),
    {RequestID, Rest1} = dec_int_tag(Bytes2),
    {NonRepeaters, Rest2} = dec_int_tag(Rest1),
    {MaxRepetitions,VBbytes} = dec_int_tag(Rest2),
    VBs = dec_VBs(VBbytes),
    #pdu{type = 'get-bulk-request', request_id = RequestID,
	 error_status = NonRepeaters, error_index = MaxRepetitions,
	 varbinds = VBs};

dec_pdu([PduTag | Bytes]) ->
    Type = dec_pdu_tag(PduTag),
    Bytes2 = get_data_bytes(Bytes),
    {RequestID, Rest1} = dec_int_tag(Bytes2),
    {ErrStat, Rest2} = dec_int_tag(Rest1),
    ErrStatus = case lists:keysearch(ErrStat, 2, errMsgs()) of
		    {value, {ErrStatName, _ErrStat}} ->
			ErrStatName;
		    false ->
			ErrStat
		end,
    {ErrIndex, VarbindsBytes} = dec_int_tag(Rest2),
    VBs = dec_VBs(VarbindsBytes),
    #pdu{type = Type, request_id = RequestID, error_status = ErrStatus,
	 error_index = ErrIndex, varbinds = VBs}.

dec_VBs([48 | Bytes]) ->
    Bytes1 = get_data_bytes(Bytes),
    dec_individual_VBs(Bytes1, 1, []).

dec_individual_VBs([], _No, VBs) ->
    lists:reverse(VBs);
dec_individual_VBs([48 | Bytes], OrgIndex, AccVBs) ->
    {_SizeOfThisVB, Bytes2} = dec_len(Bytes),
    {Oid, Rest} = dec_oid_tag(Bytes2),
    {{Type, Value}, Rest2} = dec_value(Rest),
    % perhaps we should check that we have eaten SizeOfThisVB bytes, but we
    % don't consider ourselves to have time for such list traversing stuff.
    dec_individual_VBs(Rest2, OrgIndex + 1, [#varbind{oid = Oid,
						      variabletype = Type,
						      value = Value,
						      org_index = OrgIndex}
					     | AccVBs]).

dec_usm_security_parameters([48 | Bytes1]) ->
    {_Len, Bytes2} = dec_len(Bytes1),
    {MsgAuthEngineID, Bytes3} = dec_oct_str_tag(Bytes2),
    {MsgAuthEngineBoots, Bytes4} = dec_int_tag(Bytes3),
    {MsgAuthEngineTime, Bytes5} = dec_int_tag(Bytes4),
    {MsgUserName, Bytes6} = dec_oct_str_tag(Bytes5),
    {MsgAuthParams, Bytes7} = dec_oct_str_tag(Bytes6),
    {MsgPrivParams, []} = dec_oct_str_tag(Bytes7),
    #usmSecurityParameters{msgAuthoritativeEngineID = MsgAuthEngineID,
			   msgAuthoritativeEngineBoots = MsgAuthEngineBoots,
			   msgAuthoritativeEngineTime = MsgAuthEngineTime,
			   msgUserName = MsgUserName,
			   msgAuthenticationParameters = MsgAuthParams,
			   msgPrivacyParameters = MsgPrivParams}.

strip_encrypted_scoped_pdu_data([48 | Bytes]) ->
    {Size, Tail} = dec_len(Bytes),
    [48 | elength(Size)] ++ strip(Size, Tail).

strip(N, [H|T]) when N > 0 -> [H | strip(N-1, T)];
strip(0, _Tail) ->
    [].


%%----------------------------------------------------------------------
%% Returns:{Type, Value}
%%----------------------------------------------------------------------

%% OBJECT IDENTIFIER
dec_value([6 | Bytes]) ->
    {Value, Rest} = dec_oid_notag(Bytes),
    {{'OBJECT IDENTIFIER', Value}, Rest};
dec_value([5,0 | T]) ->
    {{'NULL', 'NULL'}, T};

%% INTEGER
dec_value([2 | Bytes]) ->
    {Value, Rest} = dec_integer_notag(Bytes),
    {{'INTEGER', Value}, Rest};

%% OCTET STRING
dec_value([4 | Bytes]) ->
    {Value, Rest} = dec_oct_str_notag(Bytes),
    {{'OCTET STRING', Value}, Rest};

%% IpAddress
dec_value([64 | Bytes]) -> 
    {Value, Rest} = dec_oct_str_notag(Bytes),
    {{'IpAddress', Value}, Rest};

%% Counter32
dec_value([65 | Bytes]) ->
    %% Counter32 is an unsigned 32 but is actually encoded as 
    %% a signed integer 32 (INTEGER).
    {Value, Rest} = dec_integer_notag(Bytes),
    Value2 = 
    	if
    	    (Value >= 0) andalso (Value =< 16#ffffffff) ->
    		%% We accept value above 16#7fffffff
    		%% in order to be backward bug-compatible
    		Value;
    	    (Value < 0) ->
    		16#ffffffff + Value + 1;
    	    true ->
    		exit({error, {bad_counter32, Value}})	
    	end,
    {{'Counter32', Value2}, Rest};

%% Unsigned32
dec_value([66 | Bytes]) ->
    {Value, Rest} = dec_integer_notag(Bytes),
    Value2 = 
	if 
	    (Value >= 0) andalso (Value =< 16#ffffffff) ->
		Value;
	    (Value < 0) ->
		16#ffffffff + Value + 1;
	    true ->
		exit({error, {bad_unsigned32, Value}})
	end,
    {{'Unsigned32', Value2}, Rest};

%% TimeTicks
dec_value([67 | Bytes]) ->
    {Value, Rest} = dec_integer_notag(Bytes),
    Value2 = 
	if
	    (Value >= 0) andalso (Value =< 16#ffffffff) ->
		Value;
	    (Value < 0) ->
		16#ffffffff + Value + 1;
	true ->
	    exit({error, {bad_timeticks, Value}})
	end,
    {{'TimeTicks', Value2}, Rest};

%% Opaque
dec_value([68 | Bytes]) ->
    {Value, Rest} = dec_oct_str_notag(Bytes),
    {{'Opaque', Value}, Rest};

%% Counter64
dec_value([70 | Bytes]) ->
    %% Counter64 is an unsigned 64 but is actually encoded as 
    %% a signed integer 64.
    {Value, Rest} = dec_integer_notag(Bytes),
    Value2 = 
    	if
    	    (Value >= 0) andalso (Value < 16#8000000000000000) ->
    		Value;
    	    (Value < 0) ->
    		16#ffffffffffffffff + Value + 1;
    	    true ->
    		exit({error, {bad_counter64, Value}})	end,
    {{'Counter64', Value2}, Rest};

dec_value([128,0|T]) ->
    {{'NULL', noSuchObject}, T};
dec_value([129,0|T]) ->
    {{'NULL', noSuchInstance}, T};
dec_value([130,0|T]) ->
    {{'NULL', endOfMibView}, T}.


%%----------------------------------------------------------------------
%% Purpose: Uses the beginning length bytes to return the actual data.
%% If data has the wrong length, the program is exited.
%% Pre: Tag is removed.
%%----------------------------------------------------------------------
get_data_bytes(Bytes) ->
    {Size, Tail} = dec_len(Bytes),
    if
	length(Tail) =:= Size ->
	    Tail;
	true ->
	    exit({error, {wrong_length, Bytes}})
    end.

split_at(L, 0, Acc) -> 
    {lists:reverse(Acc), L};
split_at([H|T], N, Acc) ->
    split_at(T, N-1, [H|Acc]).

%%----------------------------------------------------------------------
%% All decoding routines return: {Data, RestBytes}
%%----------------------------------------------------------------------

dec_int_tag([2 | Bytes]) ->
    dec_integer_notag(Bytes).
dec_int_tag([2 | Bytes], SizeLimit) ->
    dec_integer_notag(Bytes, SizeLimit).

dec_integer_notag(Ints) ->
    dec_integer_notag(Ints, infinity).
dec_integer_notag(Ints, SizeLimit) ->
    case dec_len(Ints) of
	{Size, Ints2} when SizeLimit =:= infinity ->
	    do_dec_integer_notag(Size, Ints2);
	{Size, Ints2} when (is_integer(SizeLimit) andalso 
			    (Size =< SizeLimit)) ->
	    do_dec_integer_notag(Size, Ints2);
	{BadSize, _BadInts2} ->
	    throw({error, {bad_integer, {BadSize, SizeLimit}}})
    end.

do_dec_integer_notag(Size, Ints) ->
    if hd(Ints) band 128 == 0 -> %% Positive number
	    dec_pos_int(Ints, Size, 8 * (Size - 1));
       true -> %% Negative
	    dec_neg_int(Ints, Size, 8 * (Size - 1))
    end.


dec_pos_int(T, 0, _) -> {0, T};
dec_pos_int([Byte|Tail], Size, Shift) ->
    {Int, Rest} = dec_pos_int(Tail, Size - 1, Shift - 8),
    {(Byte bsl Shift) bor Int, Rest}.

dec_neg_int(T, 0, _) -> {0, T};
dec_neg_int([Byte|Tail], Size, Shift) ->
    {Int, Rest} = dec_pos_int(Tail, Size - 1, Shift-8),
    {(-128 + (Byte band 127) bsl Shift) bor Int, Rest}.

dec_oct_str_tag([4 | Bytes]) ->
    dec_oct_str_notag(Bytes).

dec_oct_str_notag(Bytes) ->
    {Size, Tail} = dec_len(Bytes),
    split_at(Tail, Size, []).

dec_oid_tag([6 | Bytes]) ->
    dec_oid_notag(Bytes).

dec_oid_notag(Bytes) ->
    {Size, [H | Tail]} = dec_len(Bytes),
    {Oid, Rest} = dec_oid_elements(Tail, Size - 1, []),
    {[H div 40, H rem 40 | Oid], Rest}.

dec_oid_elements(L, 0, Acc) ->
    {lists:reverse(Acc), L};
dec_oid_elements([Dig|Tail], Size, Acc) when Dig < 128 ->
    dec_oid_elements(Tail, Size - 1, [Dig | Acc]);
dec_oid_elements([Dig|Tail], Size, Acc) ->
    {Num, Neaten, Tl} = dec_oid_element(Tail,1,Dig band 127),
    dec_oid_elements(Tl, Size - Neaten, [Num|Acc]).

dec_oid_element([Dig|Tail], Neaten, Num) when Dig < 128 ->
    {Num*128+Dig,Neaten+1,Tail};
dec_oid_element([Dig|Tail],Neaten, Num) ->
    dec_oid_element(Tail, Neaten+1, Num*128 + (Dig band 127)).

chk_msg_id(MsgId) when (MsgId >= 0) andalso (MsgId =< 2147483647) -> ok;
chk_msg_id(MsgId) -> exit({bad_msg_id, MsgId}).
    
chk_msg_max_size(MMS) when (MMS >= 484) andalso (MMS =< 2147483647) -> ok;
chk_msg_max_size(MMS) -> exit({bad_msg_max_size, MMS}).
    
chk_msg_sec_model(MsgSecurityModel) when MsgSecurityModel >= 0,
					 MsgSecurityModel =< 2147483647 -> ok;
chk_msg_sec_model(MsgSecurityModel) -> 
    exit({bad_msg_sec_model, MsgSecurityModel}).
    
%%----------------------------------------------------------------------
%% Code copied from the original ASN.1 compiler written by 
%% klacke@erix.ericsson.se
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Returns: {Len, Tail}
%%----------------------------------------------------------------------
dec_len([128|_Tail]) ->
    %% indefinite form - not allowed in SNMP
    exit({asn1_error, indefinite_length});

dec_len([Hd|Tl]) when Hd >= 0 ->
    %% definite form
    if
	Hd < 128 -> % 8th bit is cleared
	    %% Short form (actually, we can remove this test, since snmp_pdus
	    %% performs this test _before_ calling this function)
	    {Hd,Tl};
	true ->
	    %% Long form
	    No = Hd band 127,  % clear 8th bit
	    {DigList, Rest} = head(No, Tl),
	    Size = dec_integer_len(DigList),
	    {Size, Rest}
    end.

dec_integer_len([D]) ->
    D;
dec_integer_len([A,B]) ->
    (A bsl 8) bor B;
dec_integer_len([A,B,C]) ->
    (A bsl 16) bor (B bsl 8) bor C;
%% More than 3 elements for length => either *very* long packet
%% (which we don't handle), or the length is encoded with more octets
%% than necessary (in which case the first octet must be 0).
dec_integer_len([0 | T]) ->
    dec_integer_len(T).

%%-----------------------------------------------------------------
%% head(N, List) -> {List1, List2}
%%   List == List1 ++ List2
%%   length(List1) == N
%%-----------------------------------------------------------------
head(L,List) ->
    head(L,List,[]).

head(0,L,Res) ->
    {lists:reverse(Res),L};

head(Int,[H|Tail],Res) ->
    head(Int-1,Tail,[H|Res]);
head(Int, [], _Res) ->
    exit({asn1_error, {bad_length, Int}}).

%%%----------------------------------------------------------------------
%%% ENCODING ENCODING ENCODING ENCODING ENCODING ENCODING ENCODING ENCODING 
%%%----------------------------------------------------------------------

enc_message(#message{version = Ver, vsn_hdr = VsnHdr, data = Data}) ->
    VerBytes = enc_version(Ver),
    Bytes = 
	case Ver of
	    'version-3' ->
		V3HeaderBytes = enc_v3_header(VsnHdr),
		DataBytes = enc_scoped_pdu(Data),
		V3HeaderBytes ++ DataBytes;
	    _ ->
		ComBytes = enc_community(VsnHdr),
		DataBytes = enc_pdu(Data),
		ComBytes ++ DataBytes
	end,
    Bytes2 = VerBytes ++ Bytes,
    Len = elength(length(Bytes2)),
    [48 | Len] ++ Bytes2.

enc_message_only(#message{version = Ver, vsn_hdr = VsnHdr, data = DataBytes}) ->
    VerBytes = enc_version(Ver),
    Bytes = 
	case Ver of
	    'version-3' ->
		V3HeaderBytes = enc_v3_header(VsnHdr),
		V3HeaderBytes ++ DataBytes;
	    _ ->
		ComBytes = enc_community(VsnHdr),
		ComBytes ++ DataBytes
	end,
    Bytes2 = VerBytes ++ Bytes,
    Len = elength(length(Bytes2)),
    [48 | Len] ++ Bytes2.

enc_version('version-1') ->
    [2,1,0];
enc_version('version-2') ->
    [2,1,1];
enc_version('version-3') ->
    [2,1,3].

enc_community(Com) ->
    enc_oct_str_tag(Com).

enc_v3_header(#v3_hdr{msgID = MsgID,
		      msgMaxSize = MsgMaxSize,
		      msgFlags = MsgFlags,
		      msgSecurityModel = MsgSecurityModel,
		      msgSecurityParameters = MsgSecurityParameters}) ->
    Bytes = lists:append([enc_integer_tag(MsgID),
			  enc_integer_tag(MsgMaxSize),
			  enc_oct_str_tag(MsgFlags),
			  enc_integer_tag(MsgSecurityModel)]),
    Len = elength(length(Bytes)),
    lists:append([[48 | Len], Bytes, enc_oct_str_tag(MsgSecurityParameters)]).
    
enc_scoped_pdu(#scopedPdu{contextEngineID = ContextEngineID,
			  contextName = ContextName,
			  data = Data}) ->
    Bytes = lists:append([enc_oct_str_tag(ContextEngineID),
			  enc_oct_str_tag(ContextName),
			  enc_pdu(Data)]),
    Len = elength(length(Bytes)),
    [48 | Len] ++ Bytes.


enc_pdu(PDU) when PDU#pdu.type =:= 'get-request' ->
    enc_pdu(160, PDU);
enc_pdu(PDU) when PDU#pdu.type =:= 'get-next-request' ->
    enc_pdu(161, PDU);
enc_pdu(PDU) when PDU#pdu.type =:= 'get-response' ->
    enc_pdu(162, PDU);
enc_pdu(PDU) when PDU#pdu.type =:= 'set-request' ->
    enc_pdu(163, PDU);
enc_pdu(PDU) when PDU#pdu.type =:= 'get-bulk-request' ->
    enc_pdu(165, PDU);
enc_pdu(PDU) when PDU#pdu.type =:= 'inform-request' ->
    enc_pdu(166, PDU);
enc_pdu(PDU) when PDU#pdu.type =:= 'snmpv2-trap' ->
    enc_pdu(167, PDU);
enc_pdu(PDU) when PDU#pdu.type =:= report ->
    enc_pdu(168, PDU);
enc_pdu(TrapPDU) when is_record(TrapPDU, trappdu) ->
    enc_Trap(TrapPDU).


enc_pdu(Tag,PDU) ->
    Bytes2 = enc_pdu2(PDU),
    Len2 = elength(length(Bytes2)),
    lists:append([Tag | Len2], Bytes2).

enc_pdu2(#pdu{type = Type, request_id = ReqId, error_index = ErrIndex,
	      error_status = ErrStat, varbinds = VBs}) ->
    ReqBytes = enc_integer_tag(ReqId),
    Val = err_val(ErrStat,Type),
    ErrStatBytes = enc_integer_tag(Val),
    ErrIndexBytes = enc_integer_tag(ErrIndex),
    VBsBytes = enc_VarBindList(VBs),
    lists:append([ReqBytes, ErrStatBytes, ErrIndexBytes, VBsBytes]).

enc_usm_security_parameters(
  #usmSecurityParameters{msgAuthoritativeEngineID = MsgAuthEngineID,
			 msgAuthoritativeEngineBoots = MsgAuthEngineBoots,
			 msgAuthoritativeEngineTime = MsgAuthEngineTime,
			 msgUserName = MsgUserName,
			 msgAuthenticationParameters = MsgAuthParams,
			 msgPrivacyParameters = MsgPrivParams}) ->
    Bytes1 = enc_oct_str_tag(MsgAuthEngineID),
    Bytes2 = enc_integer_tag(MsgAuthEngineBoots),
    Bytes3 = enc_integer_tag(MsgAuthEngineTime),
    Bytes4 = enc_oct_str_tag(MsgUserName),
    Bytes5 = enc_oct_str_tag(MsgAuthParams),
    Bytes6 = enc_oct_str_tag(MsgPrivParams),
    Bytes7 = lists:append([Bytes1, Bytes2, Bytes3, Bytes4, Bytes5, Bytes6]),
    Len = elength(length(Bytes7)),
    [48 | Len] ++ Bytes7.

err_val(Int,'get-bulk-request') when is_integer(Int) -> Int;
err_val(ErrStat, _) ->
    {value, {_ErrStat, Val}} = lists:keysearch(ErrStat, 1, errMsgs()),
    Val.

errMsgs() ->
    [{noError,0},{tooBig,1},{noSuchName,2},
     {badValue,3},{readOnly,4},{genErr,5},
     %% v2
     {noAccess,6},{wrongType,7},{wrongLength,8},{wrongEncoding,9},
     {wrongValue,10},{noCreation,11},{inconsistentValue,12},
     {resourceUnavailable,13},{commitFailed,14},{undoFailed,15},
     {authorizationError,16},{notWritable,17},{inconsistentName,18}].

enc_VarBindList(EncodedVBs) when is_integer(hd(EncodedVBs)) ->
    Len1 = elength(length(EncodedVBs)),
    lists:append([48 | Len1],EncodedVBs);
enc_VarBindList(VBs) ->
    Bytes1 = lists:append(lists:map(fun enc_varbind/1, VBs)),
    Len1 = elength(length(Bytes1)),
    lists:append([48 | Len1],Bytes1).

enc_varbind(Varbind) ->
    Bytes1 = enc_VarBind_attributes(Varbind),
    Len1 = elength(length(Bytes1)),
    lists:append([48 | Len1],Bytes1).


enc_VarBind_attributes(#varbind{oid = Oid, variabletype = Type,value = Val}) ->
    OidBytes = enc_oid_tag(Oid),
    ValueBytes = enc_value(Type, Val),
    lists:append(OidBytes, ValueBytes).

enc_value('INTEGER', Val) ->
    enc_integer_tag(Val);
enc_value('OCTET STRING', Val) ->
    enc_oct_str_tag(Val);
enc_value('BITS', Val) ->
    enc_oct_str_tag(bits_to_str(Val));
enc_value('OBJECT IDENTIFIER', Val) ->
    enc_oid_tag(Val);
enc_value('IpAddress', {A, B, C, D}) ->
    enc_value('IpAddress', [A,B,C,D]);
enc_value('IpAddress', Val) when is_list(Val) ->
    Bytes2 = enc_oct_str_notag(Val),
    Len2 = elength(length(Bytes2)),
    lists:append([64 | Len2],Bytes2);
enc_value('Opaque', Val) ->
    Bytes2 = enc_oct_str_notag(Val),
    Len2 = elength(length(Bytes2)),
    lists:append([68 | Len2],Bytes2);
enc_value(_Type, noSuchObject) ->
    [128,0];
enc_value(_Type, noSuchInstance) ->
    [129,0];
enc_value(_Type, endOfMibView) ->
    [130,0];
enc_value('NULL', _Val) ->
    [5,0];
enc_value('Counter32', Val) ->
    Val2 = 
	if
	    Val > 16#ffffffff ->
		exit({error, {bad_counter32, Val}});
	    Val >= 16#80000000 ->
		(Val band 16#7fffffff) - 16#80000000;
	    Val >= 0 ->
		Val;
	    true ->
		exit({error, {bad_counter32, Val}}) 
	end,
    Bytes2 = enc_integer_notag(Val2),
    Len2 = elength(length(Bytes2)),
    lists:append([65 | Len2],Bytes2);
enc_value('Unsigned32', Val) ->
    if
	(Val >= 0) andalso (Val =< 4294967295) ->
	    Bytes2 = enc_integer_notag(Val),
	    Len2 = elength(length(Bytes2)),
	    lists:append([66 | Len2], Bytes2);
	true ->
	    exit({error, {bad_counter32, Val}}) 
    end;
enc_value('TimeTicks', Val) ->
    if
	(Val >= 0) andalso (Val =< 4294967295) ->
	    Bytes2 = enc_integer_notag(Val),
	    Len2 = elength(length(Bytes2)),
	    lists:append([67 | Len2], Bytes2);
	true ->
	    exit({error, {bad_timeticks, Val}}) 
    end;
enc_value('Counter64', Val) ->
    Val2 = 
	if
	    Val > 16#ffffffffffffffff ->
		exit({error, {bad_counter64, Val}});
	    Val >= 16#8000000000000000 ->
		(Val band 16#7fffffffffffffff) - 16#8000000000000000;
	    Val >= 0 ->
		Val;
	    true ->
		exit({error, {bad_counter64, Val}}) 
	end,
    Bytes2 = enc_integer_notag(Val2),
    Len2 = elength(length(Bytes2)),
    lists:append([70 | Len2],Bytes2).


%%----------------------------------------------------------------------
%% Impl according to RFC1906, section 8
%% For example: the number 1010 0000 (=160)   0100 0001 (=65) is represented as
%% the octet string: 1000 0010, 0000 0101 (=[130,5])
%%----------------------------------------------------------------------
bits_to_str(0) -> "";
bits_to_str(Int) ->
    [rev_int8(Int band 255) | bits_to_str(Int div 256)].

rev_int8(Val) ->
    rev_int(Val,0,1,128).

rev_int(_Val,Res,256,0) -> Res;
rev_int(Val,Res,OldBit,NewBit) when Val band OldBit =/= 0 ->
    rev_int(Val,Res+NewBit,OldBit*2,NewBit div 2);
rev_int(Val,Res,OldBit,NewBit) ->
    rev_int(Val,Res,OldBit*2,NewBit div 2).

octet_str_to_bits(Str) ->
    octet_str_to_bits(Str,1).

octet_str_to_bits("",_) -> 0;
octet_str_to_bits([Byte|Bytes],Mul) ->
    Mul*rev_int8(Byte)+octet_str_to_bits(Bytes,Mul*256).
    

enc_Trap(TrapPdu) when is_record(TrapPdu, trappdu) ->
    Bytes1 = enc_trap_data(TrapPdu),
    Len1 = elength(length(Bytes1)),
    lists:append([164 | Len1],Bytes1).


enc_trap_data(#trappdu{enterprise = Enterprise,
		       agent_addr = AgentAddr,
		       generic_trap = GenericTrap,
		       specific_trap = SpecificTrap,
		       time_stamp = TimeStamp,
		       varbinds = VBs}) ->
    L1 = enc_oid_tag(Enterprise),
    L2 = enc_value('IpAddress', AgentAddr),
    L3 = enc_integer_tag(GenericTrap),
    L4 = enc_integer_tag(SpecificTrap),
    L5 = enc_value('TimeTicks', TimeStamp),
    L6 = enc_VarBindList(VBs),
    lists:append([L1,L2,L3,L4,L5,L6]).

enc_oid_tag([E1,E2|RestOid]) when E1 * 40 + E2 =< 255 ->
    Head = 40*E1 + E2,  % weird
    Res = e_object_elements(RestOid, []),
    lists:append([6 | elength(length(Res) + 1)],[Head|Res]).

e_object_elements([Num | T], Res)  ->
    e_object_elements(T, lists:append(e_object_element(Num),Res));

e_object_elements([], Res) -> lists:reverse(Res).

%%----------------------------------------------------------------------
%% The reversed encoding for an oid-element
%%----------------------------------------------------------------------
e_object_element(Num) when Num > 0 ->
    [Last|T] = e_object_element2(Num),
    [Last-128|T];
e_object_element(0) -> [0].

e_object_element2(Num) when Num > 0 ->
    Byte = (Num rem 128),
    [128+Byte | e_object_element2((Num-Byte) div 128)];
e_object_element2(0) -> [].

enc_integer_tag(Val) when Val >= 0 ->  %% stdcase positive ints
    Bytes = eint(Val,[]),
    [2 | elength(length(Bytes))] ++ Bytes;

enc_integer_tag(Val) ->  %% It's a negative number
    Bytes = enint(Val,[]),
    [2 | elength(length(Bytes))] ++ Bytes.

enc_integer_notag(Val) when Val >= 0 ->  %% stdcase positive ints
    eint(Val,[]);
    
enc_integer_notag(Val) -> %% It's a negative number
    enint(Val,[]).

eint(0, [B|Acc]) when B < 128 ->
    [B|Acc];
eint(N, Acc) ->
    eint(N bsr 8, [N band 16#ff| Acc]).

enint(-1, [B1|T]) when B1 > 127 ->
    [B1|T];
enint(N, Acc) ->
    enint(N bsr 8, [N band 16#ff|Acc]).
 
enc_oct_str_tag(OStr) when is_list(OStr) ->
    lists:append([4|elength(length(OStr))],OStr);
enc_oct_str_tag(OBin) ->
    [4 | elength(size(OBin))] ++ binary_to_list(OBin).


enc_oct_str_notag(OStr) -> OStr.

%%-----------------------------------------------------------------
%% Always use definite form
%%-----------------------------------------------------------------
%% Short
elength(L) when L < 127 ->
    [L];

%% 3 cases of long form
elength(L) when L =< 16#FF ->
    [2#10000001,L];

elength(L) when L  =< 16#FFFF ->
    [2#10000010,(L bsr 8),(L band 16#FF)];

elength(L) when L =< 16#7FFFFF ->
    [2#10000011,(L bsr 16),((L band 16#FF00) bsr 8), (L band 16#FF)].


