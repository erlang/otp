%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2009. All Rights Reserved.
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

-export([compile/3,test/2]).

-include_lib("test_server/include/test_server.hrl").



compile(Config,Rule,Opt) when Rule == ber_bin_v2 ->

    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),
    
    ?line ok = asn1ct:compile(DataDir ++ "PartialDecSeq.asn",
			      [Rule,{outdir,OutDir},{i,DataDir},
			       asn1config]++Opt),
    ?line ok = asn1ct:compile(DataDir ++ "PartialDecSeq2.asn",
			      [Rule,{outdir,OutDir},{i,DataDir},
			       asn1config]++Opt),
    ?line ok = asn1ct:compile(DataDir ++ "PartialDecSeq3.asn",
			      [Rule,{outdir,OutDir},{i,DataDir},
			      asn1config]++Opt),
    ?line ok = asn1ct:compile(DataDir ++ "PartialDecMyHTTP.asn",
			      [Rule,{outdir,OutDir},{i,DataDir},
			       asn1config]++Opt),
    ?line ok = asn1ct:compile(DataDir ++ "MEDIA-GATEWAY-CONTROL.asn",
			      [Rule,{outdir,OutDir},{i,DataDir},
			       asn1config]++Opt),
    ?line ok = asn1ct:compile(DataDir ++ "P-Record",
			      [Rule,{outdir,OutDir},{i,DataDir},
			       asn1config]++Opt);
compile(_,Rule,_) ->
    {skip,lists:concat(["not implemented yet for version: ",Rule])}.

test(ber_bin_v2,Config) ->
    FMsg = msg('F'),
    ?line {ok,Bytes} = asn1_wrapper:encode('PartialDecSeq','F',FMsg),
    ?line {ok,_} = asn1_wrapper:decode('PartialDecSeq','F',Bytes),
    ?line {ok,IncFMsg} =
	'PartialDecSeq':decode_F_fb_incomplete(list_to_binary(Bytes)),
    ?line decode_parts('F',IncFMsg),
    
    DMsg = msg('D'),
    ?line {ok,Bytes2} = asn1_wrapper:encode('PartialDecSeq','D',DMsg),
    ?line {ok,_} = asn1_wrapper:decode('PartialDecSeq','D',Bytes2),
    ?line {ok,IncDMsg} =
	'PartialDecSeq':decode_D_incomplete(list_to_binary(Bytes2)),
    ?line decode_parts('D',IncDMsg),
    
    ?line {ok,IncF2Msg} =
	'PartialDecSeq':decode_F_fb_exclusive2(list_to_binary(Bytes)),
    ?line decode_parts('F2',IncF2Msg),

    F3Msg = msg('F3'),
    ?line {ok,BytesF3} = asn1_wrapper:encode('PartialDecSeq','F',F3Msg),
    ?line {ok,_} = asn1_wrapper:decode('PartialDecSeq','F',BytesF3),
    ?line {ok,IncF3Msg} =
	'PartialDecSeq':decode_F_fb_exclusive3(list_to_binary(BytesF3)),
    ?line decode_parts('F3',IncF3Msg),


    AMsg =msg('A'),
    ?line {ok,Bytes3} = asn1_wrapper:encode('PartialDecSeq2','A',AMsg),
    ?line {ok,_} = asn1_wrapper:decode('PartialDecSeq2','A',Bytes3),
    ?line {ok,IncFMsg3} =
	'PartialDecSeq2':decode_A_c_b_incomplete(list_to_binary(Bytes3)),
    ?line decode_parts('A',IncFMsg3),
    
    MyHTTPMsg = msg('GetRequest'),
    ?line {ok,Bytes4} = asn1_wrapper:encode('PartialDecMyHTTP',
					    'GetRequest',MyHTTPMsg),
    ?line {ok,_} = asn1_wrapper:decode('PartialDecMyHTTP','GetRequest',
				       Bytes4),
    ?line {ok,IncFMsg4} =
	'PartialDecMyHTTP':decode_GetRequest_incomplete(list_to_binary(Bytes4)),
    ?line decode_parts('GetRequest',IncFMsg4),
    
    MsgS1_1 = msg('S1_1'),
    ?line {ok,Bytes5} = asn1_wrapper:encode('PartialDecSeq3','S1',MsgS1_1),
    ?line {ok,_} = asn1_wrapper:decode('PartialDecSeq3','S1',Bytes5),
    ?line {ok,IncFMsg5} =
	'PartialDecSeq3':decode_S1_incomplete(list_to_binary(Bytes5)),
    ?line decode_parts('S1_1',IncFMsg5),

    MsgS1_2 = msg('S1_2'),
    ?line {ok,Bytes6} = asn1_wrapper:encode('PartialDecSeq3','S1',MsgS1_2),
    ?line {ok,IncFMsg6} =
	'PartialDecSeq3':decode_S1_incomplete(list_to_binary(Bytes6)),
    ?line ok = decode_parts('S1_2',IncFMsg6),

    %% test of MEDIA-GATEWAY-CONTROL
    test_megaco(Config),
    ok;
test(Erule,_) ->
    {skip,lists:concat(["not implemented yet for version: ",Erule])}.

test_megaco(Config) ->
    ?line DataDir = ?config(data_dir,Config),
    Mod='MEDIA-GATEWAY-CONTROL',
    ?line {ok,FilenameList} = file:list_dir(filename:join([DataDir,
							   megacomessages])),
    %% remove any junk files that may be in the megacomessage directory
    Pred = fun(X) ->
		   case lists:reverse(X) of
		       [$l,$a,$v,$.|_R] ->true;
		       _ -> false
		   end
	   end,
    MegacoMsgFilenameList = lists:filter(Pred,FilenameList),
    Fun = fun(F) ->
                  M = read_msg(filename:join([DataDir,megacomessages,F])),
		  ?line {ok,B} = asn1_wrapper:encode(Mod,element(1,M),M),
		  ?line exclusive_decode(list_to_binary(B),F)
	  end,
    ?line lists:foreach(Fun,MegacoMsgFilenameList),
    ok.

exclusive_decode(Bin,F) ->
    Mod='MEDIA-GATEWAY-CONTROL',
    io:format("Encoding message: ~p~n",[F]),
    ?line {ok,{_,_,{_,_VsnNo,{MsgMidKey,MsgMid},{MsgMBodyKey,MsgMBody}}}}=
	Mod:decode_MegacoMessage_exclusive(Bin),
    ?line {ok,_} = Mod:decode_part(MsgMidKey,MsgMid),
    ?line {ok,_} = Mod:decode_part(MsgMBodyKey,MsgMBody),
    ok.


read_msg(File) ->
    case file:read_file(File) of
        {ok,Bin} ->
            binary_to_term(Bin);
        _ -> 
	    io:format("couldn't read file ~p~n",[File])
    end.

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
    {'F',{fb,{'E',35,[{'D',3,true},{'D',4,false},{'D',5,true},{'D',6,true},{'D',7,false},{'D',8,true},{'D',9,true},{'D',10,false},{'D',11,true},{'D',12,true},{'D',13,false},{'D',14,true}],false,{da,[{'A',16,{'D',17,true}}]}}}};

msg('F3') ->
    {fb,{'E',10,[{'D',11,true},{'D',12,false}],false,{dc,{'E_d_dc',13,true,{'E_d_dc_dcc',14,15}}}}};

msg('D') ->
    {'D',123,true};

msg('A') ->
    {'A',12,{c,{'S',true,false}},{b,{'A_c_b',false,false}}};

msg('GetRequest') ->
    {'GetRequest',true,false,{'AcceptTypes',[1,1,1,1],["hell","othe","reho","peyo","uare","fine"]},"IamfineThankYOu"};

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
    {'S3',10,"PrintableString","OCTETSTRING",[1,1,1,1]};
msg('S4') ->
    {'S4',msg('Name'),"MSc"};
msg('SO1') ->
    [msg('Name'),msg('Name'),msg('Name')];
msg('Name') ->
    {'Name',"Hans","HCA","Andersen"}.
