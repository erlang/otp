%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2003-2012. All Rights Reserved.
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
-module(test_partial_incomplete_decode).

-export([test/1]).

-include_lib("test_server/include/test_server.hrl").

test(Config) ->
    FMsg = msg('F'),
    Bytes1 = roundtrip('PartialDecSeq', 'F', FMsg),
    {ok,IncFMsg} = 'PartialDecSeq':decode_F_fb_incomplete(Bytes1),
    decode_parts('F', IncFMsg),
    {ok,IncF2Msg} = 'PartialDecSeq':decode_F_fb_exclusive2(Bytes1),
    decode_parts('F2', IncF2Msg),
    
    DMsg = msg('D'),
    Bytes2 = roundtrip('PartialDecSeq', 'D', DMsg),
    {ok,IncDMsg} = 'PartialDecSeq':decode_D_incomplete(Bytes2),
    decode_parts('D', IncDMsg),

    F3Msg = msg('F3'),
    BytesF3 = roundtrip('PartialDecSeq', 'F', F3Msg),
    {ok,IncF3Msg} = 'PartialDecSeq':decode_F_fb_exclusive3(BytesF3),
    decode_parts('F3', IncF3Msg),

    AMsg = msg('A'),
    Bytes3 = roundtrip('PartialDecSeq2', 'A', AMsg),
    {ok,IncFMsg3} = 'PartialDecSeq2':decode_A_c_b_incomplete(Bytes3),
    decode_parts('A', IncFMsg3),
    
    MyHTTPMsg = msg('GetRequest'),
    Bytes4 = roundtrip('PartialDecMyHTTP', 'GetRequest', MyHTTPMsg),
    {ok,IncFMsg4} = 'PartialDecMyHTTP':decode_GetRequest_incomplete(Bytes4),
    decode_parts('GetRequest', IncFMsg4),
    
    MsgS1_1 = msg('S1_1'),
    Bytes5 = roundtrip('PartialDecSeq3', 'S1', MsgS1_1),
    {ok,IncFMsg5} = 'PartialDecSeq3':decode_S1_incomplete(Bytes5),
    decode_parts('S1_1', IncFMsg5),

    MsgS1_2 = msg('S1_2'),
    Bytes6 = roundtrip('PartialDecSeq3', 'S1', MsgS1_2),
    {ok,IncFMsg6} = 'PartialDecSeq3':decode_S1_incomplete(Bytes6),
    decode_parts('S1_2', IncFMsg6),

    %% test of MEDIA-GATEWAY-CONTROL
    test_megaco(Config),
    ok.

test_megaco(Config) ->
    DataDir = ?config(data_dir, Config),
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
    ?line {ok,{_,_,{_,_VsnNo,{MsgMidKey,MsgMid},{MsgMBodyKey,MsgMBody}}}}=
	Mod:decode_MegacoMessage_exclusive(Bin),
    ?line {ok,_} = Mod:decode_part(MsgMidKey,MsgMid),
    ?line {ok,_} = Mod:decode_part(MsgMBodyKey,MsgMBody),
    ok.

decode_parts('F',PartDecMsg) ->
    ?line {fb,{'E',35,{NameE_b,ListBinE_b},false,{NameE_d,BinE_d}}} = PartDecMsg,
    ?line {ok,[{'D',3,true}|_]} = 'PartialDecSeq':decode_part(NameE_b,ListBinE_b),
    ?line {ok,{'D',3,true}} = 'PartialDecSeq':decode_part(NameE_b,
							  hd(ListBinE_b)),
    ?line {ok,{da,[{'A',16,{'D',17,true}}]}} = 
	'PartialDecSeq':decode_part(NameE_d,BinE_d),
    ok;
decode_parts('F2',PartDecMsg) ->
    ?line {fb,{'E',35,{E_bkey,E_b},false,{da,{E_d_akey,E_d_a}}}} = PartDecMsg,
    ?line {ok,[{'D',3,true},{'D',4,false},{'D',5,true},{'D',6,true},{'D',7,false},{'D',8,true},{'D',9,true},{'D',10,false},{'D',11,true},{'D',12,true},{'D',13,false},{'D',14,true}]} = 'PartialDecSeq':decode_part(E_bkey,E_b),
    ?line {ok,[{'A',16,{'D',17,true}}]} = 'PartialDecSeq':decode_part(E_d_akey,E_d_a);

decode_parts('F3',PartDecMsg) ->
    ?line {fb,{'E',10,{E_bkey,E_b},false,{dc,{'E_d_dc',13,true,{E_d_dc_dcckey,E_d_dc_dcc}}}}} = PartDecMsg,
    ?line {ok,[{'D',11,true},{'D',12,false}]} = 'PartialDecSeq':decode_part(E_bkey,E_b),
    ?line {ok,{'E_d_dc_dcc',14,15}} = 'PartialDecSeq':decode_part(E_d_dc_dcckey,E_d_dc_dcc);


decode_parts('D',PartDecMsg) ->
    ?line {'D',{NameD_a,BinD_a},true} = PartDecMsg,
    ?line {ok,123} = 'PartialDecSeq':decode_part(NameD_a,BinD_a),
    ok;
decode_parts('A',PartDecMsg) ->
    ?line {'A',12,{c,{'S',true,false}},{b,{NameA_c_b,BinA_c_b}}} = PartDecMsg,
    ?line {ok,{'A_c_b',false,false}} = 
	'PartialDecSeq2':decode_part(NameA_c_b,BinA_c_b),
    ok;
decode_parts('GetRequest',PartDecMsg) ->
    ?line {'GetRequest',true,false,
	   {'AcceptTypes',[html,'plain-text',gif,jpeg],
	    {NameAcceptTypes_others,ListBinAcceptTypes_others}},
	   "IamfineThankYOu"} = PartDecMsg,
    ?line {ok,["hell","othe","reho","peyo","uare","fine"]} = 
	'PartialDecMyHTTP':decode_part(NameAcceptTypes_others,
				       ListBinAcceptTypes_others),
    ?line {ok,"hell"} = 
	'PartialDecMyHTTP':decode_part(NameAcceptTypes_others,
				       hd(ListBinAcceptTypes_others)),
    ok;
decode_parts('S1_1',PartDecMsg) ->
    ?line {'S1',14,{'S2',false,12,{NameS2c,BinS2c}},
	   {_,{NameS1c_a,ListBinS1c_a}},{NameS1d,BinS1d}} = PartDecMsg,
    ?line {ok,[{'S3',10,"PrintableString","OCTETSTRING",
		[one,two,three,four]}|_Rest1]} = 
	'PartialDecSeq3':decode_part(NameS2c,BinS2c),
    ?line {ok,[{'S3',10,"PrintableString","OCTETSTRING",
		[one,two,three,four]}|_Rest2]} = 
	'PartialDecSeq3':decode_part(NameS1c_a,ListBinS1c_a),
    ?line {ok,{'S3',10,"PrintableString","OCTETSTRING",
	       [one,two,three,four]}} =
	'PartialDecSeq3':decode_part(NameS1c_a,hd(ListBinS1c_a)),
    ?line {ok,[{'Name',"Hans","HCA","Andersen"}|_Rest3]} =
	'PartialDecSeq3':decode_part(NameS1d,BinS1d),
    ok;
decode_parts('S1_2',PartDecMsg) ->
    ?line {'S1',14,{'S2',false,12,_S2c},S1c_b,{NameS1d,BinS1d}} = PartDecMsg,
    ?line {b,{'C1_b',11,true,
	      {'S4',{'Name',"Hans","HCA","Andersen"},"MSc"}}}=S1c_b,
    ?line {ok,[{'Name',"Hans","HCA","Andersen"}|_Rest3]} =
	'PartialDecSeq3':decode_part(NameS1d,BinS1d),
    ok.
    

    
msg('F') ->
    {fb,{'E',35,[{'D',3,true},{'D',4,false},{'D',5,true},{'D',6,true},{'D',7,false},{'D',8,true},{'D',9,true},{'D',10,false},{'D',11,true},{'D',12,true},{'D',13,false},{'D',14,true}],false,{da,[{'A',16,{'D',17,true}}]}}};

msg('F3') ->
    {fb,{'E',10,[{'D',11,true},{'D',12,false}],false,{dc,{'E_d_dc',13,true,{'E_d_dc_dcc',14,15}}}}};

msg('D') ->
    {'D',123,true};

msg('A') ->
    {'A',12,{c,{'S',true,false}},{b,{'A_c_b',false,false}}};

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
    {'S3',10,"PrintableString","OCTETSTRING",[one,two,three,four]};
msg('S4') ->
    {'S4',msg('Name'),"MSc"};
msg('SO1') ->
    [msg('Name'),msg('Name'),msg('Name')];
msg('Name') ->
    {'Name',"Hans","HCA","Andersen"}.

roundtrip(M, T, V) ->
    asn1_test_lib:roundtrip_enc(M, T, V).
