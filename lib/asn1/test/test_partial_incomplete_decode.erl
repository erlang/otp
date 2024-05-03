%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2003-2024. All Rights Reserved.
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
-module(test_partial_incomplete_decode).

-export([test/1]).

-include_lib("common_test/include/ct.hrl").

test(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    test_PartialDecSeq(),
    test_PartialDecSeq2(),
    test_PartialDecSeq3(),
    test_MyHTTPMsg(),
    test_megaco(DataDir),
    test_OCSP(DataDir),
    ok.

test_PartialDecSeq() ->
    M = 'PartialDecSeq',

    FMsg = msg('F'),
    test_exclusive(fun M:decode_F_fb_incomplete/1, 'F', FMsg),

    DMsg = msg('D'),
    test_exclusive(fun M:decode_D_incomplete/1, 'D', DMsg),

    F3Msg = msg('F3'),
    test_exclusive(fun M:decode_F_fb_exclusive3/1, 'F', F3Msg),

    EMsg = msg('E'),
    test_exclusive(fun M:decode_E_b_incomplete/1, 'E', EMsg),

    ok.

test_PartialDecSeq2() ->
    M = 'PartialDecSeq2',

    %% Test DEFAULT value.
    AMsg1 = msg('A_1'),
    AMsg1Encoded = roundtrip(M, 'A', AMsg1),
    {ok,AMsg1} = M:decode_A_a_incomplete(AMsg1Encoded),

    AMsg2 = msg('A_2'),
    test_exclusive(fun M:decode_A_a_incomplete/1, 'A', AMsg2),
    test_exclusive(fun M:decode_A_c_b_incomplete/1, 'A', AMsg2),

    SMsg = {'S',true,false},
    BextMsg = {c,SMsg},

    test_exclusive(fun M:decode_Bext_c_incomplete/1, 'Bext', BextMsg),
    test_exclusive(fun M:decode_Bext_c_b_incomplete/1, 'Bext', BextMsg),

    T = 'SeqChoice',

    SeqChoiceMsg1 = {'SeqChoice',{b,true},<<"abc">>},
    test_exclusive(fun M:decode_SeqChoice_c_b_d_incomplete/1, T, SeqChoiceMsg1),

    test_exclusive(fun M:decode_SeqChoice_c_bis_incomplete/1, T, SeqChoiceMsg1),

    SeqChoiceMsg2 = {'SeqChoice',{i,42},<<"cde">>},
    test_exclusive(fun M:decode_SeqChoice_c_bis_incomplete/1, T, SeqChoiceMsg2),

    SeqChoiceMsg3 = {'SeqChoice',{s,"xyz"},<<"fgh">>},
    test_exclusive(fun M:decode_SeqChoice_c_bis_incomplete/1, T, SeqChoiceMsg3),

    ok.

test_PartialDecSeq3() ->
    M = 'PartialDecSeq3',

    MsgS1_1 = msg('S1_1'),
    test_exclusive(fun M:decode_S1_incomplete/1, 'S1', MsgS1_1),
    test_exclusive(fun M:decode_S1_b_incomplete/1, 'S1', MsgS1_1),

    MsgS1_2 = msg('S1_2'),
    test_exclusive(fun M:decode_S1_incomplete/1, 'S1', MsgS1_2),

    MsgS3 = msg('S3'),
    test_exclusive(fun M:decode_S3_second/1, 'S3', MsgS3),

    ok.

test_MyHTTPMsg() ->
    MyHTTPMsg = msg('GetRequest'),
    Bytes1 = roundtrip('PartialDecMyHTTP', 'GetRequest', MyHTTPMsg),
    {ok,IncFMsg4} = 'PartialDecMyHTTP':decode_GetRequest_incomplete(Bytes1),
    decode_parts('GetRequest', IncFMsg4),

    ok.

test_megaco(DataDir) ->
    Files = filelib:wildcard(filename:join([DataDir,megacomessages,"*.val"])),
    Mod = 'MEDIA-GATEWAY-CONTROL',
    lists:foreach(fun(File) ->
			  {ok,Bin} = file:read_file(File),
			  V = binary_to_term(Bin),
			  T = element(1, V),
			  Enc = roundtrip(Mod, T, V),
			  exclusive_decode(Enc, File)
		  end, Files).

exclusive_decode(Bin,F) ->
    Mod='MEDIA-GATEWAY-CONTROL',
    io:format("Encoding message: ~p~n",[F]),
    {ok,{_,_,{_,_VsnNo,{MsgMidKey,MsgMid},{MsgMBodyKey,MsgMBody}}}}=
	Mod:decode_MegacoMessage_exclusive(Bin),
    {ok,_} = Mod:decode_part(MsgMidKey,MsgMid),
    {ok,_} = Mod:decode_part(MsgMBodyKey,MsgMBody),
    ok.

test_OCSP(DataDir) ->
    Mod = 'OCSP-2013-88',

    ResponseData = {'ResponseData',
                    v1,                          %Version
                    {byKey,<<"key hash">>},
                    "factory",
                    [],
                    asn1_NOVALUE},

    Type = 'BasicOCSPResponse',

    BasicMsg = {Type,
                ResponseData,
                {'AlgorithmIdentifier',Mod:'id-pkix-ocsp-basic'(),asn1_NOVALUE},
                <<"signature">>,
                []},

    test_exclusive(fun Mod:decode_version_undec/1, Type, BasicMsg),
    test_exclusive(fun Mod:decode_responderID_undec/1, Type, BasicMsg),
    test_exclusive(fun Mod:decode_producedAt_undec/1, Type, BasicMsg),
    test_exclusive(fun Mod:decode_responses_undec/1, Type, BasicMsg),
    test_exclusive(fun Mod:decode_responses_parts/1, Type, BasicMsg),
    test_exclusive(fun Mod:decode_tbsResponseData_undec/1, Type, BasicMsg),
    test_exclusive(fun Mod:decode_BasicOCSPResponse_signature_undec/1, Type, BasicMsg),
    test_exclusive(fun Mod:decode_BasicOCSPResponse_certs_undec/1, Type, BasicMsg),
    test_exclusive(fun Mod:decode_BasicOCSPResponse_certs_parts/1, Type, BasicMsg),

    %% Test undecoded/parts for an absent element.
    MsgWithoutCerts =
        {Type,
         ResponseData,
         {'AlgorithmIdentifier',Mod:'id-pkix-ocsp-basic'(),asn1_NOVALUE},
         <<"signature">>,
         asn1_NOVALUE},
    {ok,Enc} = Mod:encode(Type, MsgWithoutCerts),
    {ok,MsgWithoutCerts} = Mod:decode_BasicOCSPResponse_certs_undec(Enc),
    {ok,MsgWithoutCerts} = Mod:decode_BasicOCSPResponse_certs_parts(Enc),

    DataFileName = filename:join(DataDir, "BasicOCSPResponse.ber"),
    {ok,CannedData} = file:read_file(DataFileName),
    {ok,HugeMsg} = Mod:decode('BasicOCSPResponse', CannedData),

    %% Decode version with a default value.
    {ok,HugeMsg} = Mod:decode_version_undec(CannedData),

    test_exclusive(fun Mod:decode_responderID_undec/1, Type, HugeMsg),
    test_exclusive(fun Mod:decode_producedAt_undec/1, Type, HugeMsg),
    test_exclusive(fun Mod:decode_responses_undec/1, Type, HugeMsg),
    test_exclusive(fun Mod:decode_responses_parts/1, Type, HugeMsg),
    test_exclusive(fun Mod:decode_tbsResponseData_undec/1, Type, HugeMsg),
    test_exclusive(fun Mod:decode_BasicOCSPResponse_signature_undec/1, Type, HugeMsg),
    test_exclusive(fun Mod:decode_BasicOCSPResponse_certs_undec/1, Type, HugeMsg),
    test_exclusive(fun Mod:decode_BasicOCSPResponse_certs_parts/1, Type, HugeMsg),

    ok.

decode_parts('F3',PartDecMsg) ->
    {fb,{'E',10,{E_bkey,E_b},false,{dc,{'E_d_dc',13,true,{E_d_dc_dcckey,E_d_dc_dcc}}}}} = PartDecMsg,
    {ok,[{'D',11,true},{'D',12,false}]} = 'PartialDecSeq':decode_part(E_bkey,E_b),
    {ok,{'E_d_dc_dcc',14,15}} = 'PartialDecSeq':decode_part(E_d_dc_dcckey,E_d_dc_dcc);
decode_parts('GetRequest',PartDecMsg) ->
    {'GetRequest',true,false,
	   {'AcceptTypes',[html,'plain-text',gif,jpeg],
	    {NameAcceptTypes_others,ListBinAcceptTypes_others}},
	   "IamfineThankYOu"} = PartDecMsg,
    {ok,["hell","othe","reho","peyo","uare","fine"]} =
	'PartialDecMyHTTP':decode_part(NameAcceptTypes_others,
				       ListBinAcceptTypes_others),
    {ok,"hell"} =
	'PartialDecMyHTTP':decode_part(NameAcceptTypes_others,
				       hd(ListBinAcceptTypes_others)),
    ok.

msg('E') ->
    {'E',35,msg('D_many'),false,{da,[{'A',16,{'D',17,true}}]}};

msg('D_many') ->
    [{'D',3,true},{'D',4,false},{'D',5,true},{'D',6,true},{'D',7,false},{'D',8,true},{'D',9,true},
     {'D',10,false},{'D',11,true},{'D',12,true},{'D',13,false},{'D',14,true}];

msg('F') ->
    {fb,msg('E')};

msg('F3') ->
    {fb,{'E',10,[{'D',11,true},{'D',12,false}],false,{dc,{'E_d_dc',13,true,{'E_d_dc_dcc',14,15}}}}};

msg('D') ->
    {'D',123,true};

msg('A_1') ->
    {'A',15,{c,{'S',true,false}},{b,{'A_c_b',false,false}}};
msg('A_2') ->
    {'A',42,{c,{'S',true,false}},{b,{'A_c_b',false,false}}};

msg('GetRequest') ->
    {'GetRequest',true,false,
     {'AcceptTypes',[html,'plain-text',gif,jpeg],
      ["hell","othe","reho","peyo","uare","fine"]},
     "IamfineThankYOu"};

msg('S1_1') ->
    {'S1',14,msg('S2'),msg('C1_a'),msg('SO1')};
msg('S1_2') ->
    {'S1',14,msg('S2'),msg('C1_b'),msg('SO1')};
msg('S2') ->
    {'S2',false,12,[msg('S3'),msg('S3'),msg('S3')]};
msg('C1_a') ->
    {a,[msg('S3'),msg('S3'),msg('S3')]};
msg('C1_b') ->
    {b,{'C1_b',11,true,msg('S4')}};
msg('S3') ->
    {'S3',10,"PrintableString",<<"OCTETSTRING">>,[one,two,three,four]};
msg('S4') ->
    {'S4',msg('Name'),"MSc"};
msg('SO1') ->
    [msg('Name'),msg('Name'),msg('Name')];
msg('Name') ->
    {'Name',"Hans","HCA","Andersen"}.

roundtrip(M, T, V) ->
    asn1_test_lib:roundtrip_enc(M, T, V).

test_exclusive(DecodeFun, Type, Msg) ->
    {module,Mod} = erlang:fun_info(DecodeFun, module),
    Encoded = roundtrip(Mod, Type, Msg),
    case DecodeFun(Encoded) of
        {ok,Msg} ->
            error({should_be_different,Msg});
        {ok,Decoded} ->
            case dec_parts(Decoded, Msg, Mod) of
                Msg ->
                    ok;
                OtherMsg ->
                    io:format("""
                              Partial decoding:
                              ~p

                              Expected:
                              ~p

                              Got:
                              ~p
                              """, [Decoded,Msg,OtherMsg]),
                    error(full_and_partial_decode_differ)
            end
    end.

dec_parts(Same, Same, _Mod) ->
    Same;
dec_parts({Name,Parts}, Expected, Mod) when is_atom(Name), is_list(Parts), is_list(Expected) ->
    [begin
         {ok,Dec} = Mod:decode_part(Name, Bin),
         Dec
     end || Bin <- Parts];
dec_parts({Name,Undec}, _Expected, Mod) when is_atom(Name), is_binary(Undec) ->
    {ok,Dec} = Mod:decode_part(Name, Undec),
    Dec;
dec_parts({Name,{Tag,_}=Undec}, _Expected, Mod) when is_atom(Name), is_integer(Tag) ->
    {ok,Dec} = Mod:decode_part(Name, Undec),
    Dec;
dec_parts(Tuple0, Expected, Mod) when is_tuple(Tuple0), is_tuple(Expected) ->
    Tuple = dec_parts_list(tuple_to_list(Tuple0), tuple_to_list(Expected), Mod),
    list_to_tuple(Tuple);
dec_parts(List, Expected, Mod) when is_list(List), is_list(Expected) ->
    dec_parts_list(List, Expected, Mod).

dec_parts_list([H1|T1], [H2|T2], Mod) ->
    [dec_parts(H1, H2, Mod) | dec_parts_list(T1, T2, Mod)];
dec_parts_list([], [], _Mod) ->
    [].
