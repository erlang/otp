%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2010. All Rights Reserved.
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
-module(asn1_test_lib).

-export([ticket_7407_compile/2,ticket_7407_code/1, ticket_7678/2,
	ticket_7708/2, ticket_7763/1, ticket_7876/3]).

-include_lib("test_server/include/test_server.hrl").


ticket_7407_compile(Config,Option) ->

    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),

    ?line ok = asn1ct:compile(DataDir ++ "EUTRA-extract-7407",
			      [uper_bin, {outdir,OutDir}]++Option).

ticket_7708(Config,Option) ->
    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    
    ?line ok = asn1ct:compile(DataDir ++ "EUTRA-extract-55",
			      [uper_bin, {outdir,OutDir}]++Option).
    

ticket_7407_code(FinalPadding) ->
    Msg1 = {Type1,_} = eutra1(msg),
    ?line {ok,B1} = 'EUTRA-extract-7407':encode(Type1,Msg1),
    ?line B1 = eutra1(result,FinalPadding),
    
    Msg2 = {Type2,_} = eutra2(msg),
    ?line {ok,B2} = 'EUTRA-extract-7407':encode(Type2,Msg2),
    ?line B2 = eutra2(result,FinalPadding),
    ok.

eutra1(msg) ->
    {'BCCH-BCH-Message',{'MasterInformationBlock',[0,1,0,1],[1,0,1,0],{'PHICH-Configuration',short,ffs},[1,0,1,0,0,0,0,0]}}.
eutra1(result,true) ->
    <<90,80,0>>;
eutra1(result,false) ->
    <<90,80,0:1>>.

eutra2(msg) ->
    {'BCCH-DL-SCH-Message',
     {c1,
      {systemInformation1,
       {'SystemInformationBlockType1',
	{'SystemInformationBlockType1_cellAccessRelatedInformation',
	 [{'SystemInformationBlockType1_cellAccessRelatedInformation_plmn-IdentityList_SEQOF',{'PLMN-Identity'},true},
	  {'SystemInformationBlockType1_cellAccessRelatedInformation_plmn-IdentityList_SEQOF',{'PLMN-Identity'},false},
	  {'SystemInformationBlockType1_cellAccessRelatedInformation_plmn-IdentityList_SEQOF',{'PLMN-Identity'},true}],
	 {'TrackingAreaCode'},
	 {'CellIdentity'},
	 false,
	 true,
	 true,
	 true
	},
	{'SystemInformationBlockType1_cellSelectionInfo',-50},
	24,
	[{'SystemInformationBlockType1_schedulinInformation_SEQOF',
	  {'SystemInformationBlockType1_schedulinInformation_SEQOF_si-MessageType'},
	  ms320,
	  {'SystemInformationBlockType1_schedulinInformation_SEQOF_sib-MappingInfo'}}],
	0
       }
      }
     }
    }.
eutra2(result,true) ->
%% 55 5C A5 E0
    <<85,92,165,224>>;
eutra2(result,false) ->
    <<85,92,165,14:4>>.



ticket_7678(Config, Option) ->
    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),

    ?line ok = asn1ct:compile(DataDir ++ "UPERDefault",
			      [uper_bin, {outdir,OutDir}]++Option),
    
    ?line Val = 'UPERDefault':seq(),
    ?line {ok,<<0,6,0>>} = 'UPERDefault':encode('Seq',Val),
    ?line {ok,Val} = 'UPERDefault':decode('Seq',<<0,6,0>>),
    ok.


ticket_7763(Config) ->
    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),

    ?line ok = asn1ct:compile(DataDir ++ "EUTRA-extract-55",
			      [uper_bin, {outdir,OutDir}]),
    Val = {'Seq',15,lists:duplicate(8,0),[0],lists:duplicate(28,0),15,true},
    ?line {ok,Bin} = 'EUTRA-extract-55':encode('Seq',Val),

    ?line ok = asn1ct:compile(DataDir ++ "EUTRA-extract-55",
			      [uper_bin,compact_bit_string,{outdir,OutDir}]),
    CompactVal = {'Seq',15,{0,<<0>>},{7,<<0>>},{4,<<0,0,0,0>>},15,true},
    {ok,CompactBin} = 'EUTRA-extract-55':encode('Seq',CompactVal),

    ?line Bin = CompactBin,

    io:format("CompactBin:~n~p~nBin:~n~p~nCompactBin == Bin is ~p~n",[CompactBin,Bin,CompactBin == Bin]).
    

ticket_7876(Config,Erule,Options) ->
    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),

    ?line ok = asn1ct:compile(DataDir ++ "S1AP-CommonDataTypes",
			      [Erule,{outdir,OutDir}|Options]),
    ?line ok = asn1ct:compile(DataDir ++ "S1AP-Constants",
			      [Erule,{outdir,OutDir}|Options]),
?line ok = asn1ct:compile(DataDir ++ "S1AP-Containers",
			      [Erule,{outdir,OutDir}|Options]),
?line ok = asn1ct:compile(DataDir ++ "S1AP-IEs",
			      [Erule,{outdir,OutDir}|Options]),
?line ok = asn1ct:compile(DataDir ++ "S1AP-PDU-Contents",
			      [Erule,{outdir,OutDir}|Options]),
?line ok = asn1ct:compile(DataDir ++ "S1AP-PDU-Descriptions",
			      [Erule,{outdir,OutDir}|Options]),

    ticket_7876_encdec(Erule),
    ok.

ticket_7876_encdec(per) ->
    ?line {ok,{initiatingMessage,_}} = 'S1AP-PDU-Descriptions':decode('S1AP-PDU', [0,2,64,49,0,0,5,0,0,0,4,128,106,56,197,0,8,0,3,64,2,134,0,100,64,8,0,66,240,153,0,7,192,16,0,67,64,6,0,66,240,153,70,1,0,107,64,5,0,0,0,0,0]);
ticket_7876_encdec(_) ->
    ?line {ok,{initiatingMessage,_}} = 'S1AP-PDU-Descriptions':decode('S1AP-PDU', <<0,2,64,49,0,0,5,0,0,0,4,128,106,56,197,0,8,0,3,64,2,134,0,100,64,8,0,66,240,153,0,7,192,16,0,67,64,6,0,66,240,153,70,1,0,107,64,5,0,0,0,0,0>>).
