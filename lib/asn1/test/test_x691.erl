%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2008-2009. All Rights Reserved.
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
-module(test_x691).

-export([compile/3]).
-export([cases/2]).

-include_lib("test_server/include/test_server.hrl").


compile(Config,Rules,Option) ->

    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),
    ?line ok = asn1ct:compile(DataDir ++ "P-RecordA1",
			      [Rules, {outdir,OutDir}]++Option),
    ?line ok = asn1ct:compile(DataDir ++ "P-RecordA2",
			      [Rules, {outdir,OutDir}]++Option),
    ?line ok = asn1ct:compile(DataDir ++ "P-RecordA3",
			      [Rules, {outdir,OutDir}]++Option).


cases(Erule,Variant) ->
    MsgA1 = a1(),
    ?line {ok,B1} = asn1_wrapper:encode('P-RecordA1','PersonnelRecord',MsgA1),
    ?line {ok,MsgA1} = asn1_wrapper:decode('P-RecordA1','PersonnelRecord',B1),
    io:format("compare_format(~p,B1) ->~p~nencval(a1,~p,binary) ->~p~n",
	      [Erule,
	       compare_format(Erule,B1),
	       Variant,
	       encval(a1,Variant,binary)]),
    ?line true = (compare_format(Erule,B1) == encval(a1,Variant,binary)),
    
    MsgA2 = a2(),
    ?line {ok,B2} = asn1_wrapper:encode('P-RecordA2','PersonnelRecord',MsgA2),
    ?line {ok,MsgA2} = asn1_wrapper:decode('P-RecordA2','PersonnelRecord',B2),
    io:format("compare_format(~p,B2) ->~p~nencval(a2,~p,binary) ->~p~n",
	      [Erule,
	       compare_format(Erule,B2),
	       Variant,
	       encval(a2,Variant,binary)]),
    ?line true = (compare_format(Erule,B2) == encval(a2,Variant,binary)),

    MsgA3 = a3(),
    ?line {ok,B3} = asn1_wrapper:encode('P-RecordA3','PersonnelRecord',MsgA3),
    ?line {ok,MsgA3} = asn1_wrapper:decode('P-RecordA3','PersonnelRecord',B3),
    io:format("compare_format(~p,B3) ->~p~nencval(a3,~p,binary) ->~p~n",
	      [Erule,
	       compare_format(Erule,B3),
	       Variant,
	       encval(a3,Variant,binary)]),
    ?line true = (compare_format(Erule,B3) == encval(a3,Variant,binary)).

compare_format(Erule,Val) when is_list(Val) ->
    compare_format(Erule,list_to_binary(Val));
%% compare_format(per,Val) ->
%%     binary_to_list(Val);
compare_format(_,Val) ->
    Val.

a1() ->
    {'PersonnelRecord', 
     {'Name',"John", "P", "Smith"}, 
      "Director",
      51, 
      "19710917", 
      {'Name', "Mary", "T", "Smith"},
      [{'ChildInformation',
	{'Name',  "Ralph", "T", "Smith"},  
	"19571111"}, 
       {'ChildInformation',
	{'Name', "Susan", "B", "Jones"}, 
	"19590717"}]}.

a2() ->
    a1().

a3() ->
    {'PersonnelRecord',
     {'Name',"John", "P", "Smith"},
     "Director",
     51,
     "19710917",
     {'Name', "Mary", "T", "Smith"},
     [{'ChildInformation',
       {'Name', "Ralph", "T", "Smith"}, 
       "19571111",
       asn1_NOVALUE},
      {'ChildInformation',
       {'Name', "Susan", "B", "Jones"},
       "19590717",
       female}]}.

encval(An,Variant,Encoding) when Encoding == hex; Encoding == binary ->
    Msg = encval(An,Variant),
    encoding(Encoding,Msg).

encval(a1,aligned) ->
    "80044A6F 686E0150 05536D69 74680133 08446972 6563746F 72083139 37313039 3137044D 61727901 5405536D 69746802 0552616C 70680154 05536D69 74680831 39353731 31313105 53757361 6E014205 4A6F6E65 73083139 35393037 3137";
encval(a1,unaligned) ->
    "824ADFA3 700D005A 7B74F4D0 02661113 4F2CB8FA 6FE410C5 CB762C1C B16E0937 0F2F2035 0169EDD3 D340102D 2C3B3868 01A80B4F 6E9E9A02 18B96ADD 8B162C41 69F5E787 700C2059 5BF765E6 10C5CB57 2C1BB16E";
encval(a2,aligned) ->
    "864A6F68 6E501053 6D697468 01330844 69726563 746F7219 7109170C 4D617279 5410536D 69746802 1052616C 70685410 536D6974 68195711 11105375 73616E42 104A6F6E 65731959 0717";
encval(a2,unaligned) ->
    "865D51D2 888A5125 F1809984 44D3CB2E 3E9BF90C B8848B86 7396E8A8 8A5125F1 81089B93 D71AA229 4497C632 AE222222 985CE521 885D54C1 70CAC838 B8";
encval(a3,aligned) ->
    "40C04A6F 686E5008 536D6974 68000033 08446972 6563746F 72001971 0917034D 61727954 08536D69 74680100 52616C70 68540853 6D697468 00195711 11820053 7573616E 42084A6F 6E657300 19590717 010140";
encval(a3,unaligned) ->
    "40CBAA3A 5108A512 5F180330 889A7965 C7D37F20 CB8848B8 19CE5BA2 A114A24B E3011372 7AE35422 94497C61 95711118 22985CE5 21842EAA 60B832B2 0E2E0202 80".

encoding(binary,Msg) ->
    list_to_binary(bin(Msg));
encoding(hex,Msg) ->
    hex(Msg).

bin(Msg) ->
    HexList = hex(Msg),
    Fun = fun([H1,H2|Rest],F) -> [(H1 bsl 4) + H2|F(Rest,F)];([],_) -> [] end,
    Fun(HexList,Fun).

hex(Msg) ->
    [to_hex(X)||X <- Msg,X /= $ ].

to_hex(I) when I >= $0, I =< $9 ->
    I-48;
to_hex(C) when C >= $A,C =< $F ->
    C - 55.

%% ex('EUTRA','BCCH-DL-SCH-Message',1) ->
%%     {'BCCH-DL-SCH-Message',
%%      {c1,
%%       {systemInformation1,
%%        {'SystemInformationBlockType1',
%% 	{'SystemInformationBlockType1_cellAccessRelatedInformation',
%% 	 [{'SystemInformationBlockType1_cellAccessRelatedInformation_SOF',
%% 	   {'PLMN-Identity'},
%% 	   true},
%% 	  {'SystemInformationBlockType1_cellAccessRelatedInformation_SOF',
%% 	   {'PLMN-Identity'},
%% 	   false},
%% 	  {'SystemInformationBlockType1_cellAccessRelatedInformation_SOF',
%% 	   {'PLMN-Identity'},
%% 	   true}],
%% 	 {'TrackingAreaCode'},
%% 	 {'CellIdentity'},
%% 	 false,
%% 	 true,
%% 	 true,
%% 	 true},
%% 	{'SystemInformationBlockType1_cellSelectionInfo',
%% 	 -50},
%% 	24,
%% 	[{'SystemInformationBlockType1_schedulinInformation_SOF',
%% 	  {'SystemInformationBlockType1_schedulinInformation_SOF_si-MessageType'},
%% 	  ms320,
%% 	  {'SystemInformationBlockType1_schedulinInformation_SOF_sib-MappingInfo'}
%% 	 }],
%% 	0
%% 	}
%%        }
%%       }
%%      }.

%% eutra1(msg) ->
%%     {'BCCH-BCH-Message',{'MasterInformationBlock',[0,1,0,1],[1,0,1,0],{'PHICH-Configuration',short,ffs},[1,0,1,0,0,0,0,0]}};
%% eutra1(result) ->
%%     <<90,80,0>>.

%% eutra2(msg) ->
%%     {'BCCH-DL-SCH-Message',
%%      {c1,
%%       {systemInformation1,
%%        {'SystemInformationBlockType1',
%% 	{'SystemInformationBlockType1_cellAccessRelatedInformation',
%% 	 [{'SystemInformationBlockType1_cellAccessRelatedInformation_plmn-IdentityList_SEQOF',{'PLMN-Identity'},true},
%% 	  {'SystemInformationBlockType1_cellAccessRelatedInformation_plmn-IdentityList_SEQOF',{'PLMN-Identity'},false},
%% 	  {'SystemInformationBlockType1_cellAccessRelatedInformation_plmn-IdentityList_SEQOF',{'PLMN-Identity'},true}],
%% 	 {'TrackingAreaCode'},
%% 	 {'CellIdentity'},
%% 	 false,
%% 	 true,
%% 	 true,
%% 	 true
%% 	},
%% 	{'SystemInformationBlockType1_cellSelectionInfo',-50},
%% 	24,
%% 	[{'SystemInformationBlockType1_schedulinInformation_SEQOF',
%% 	  {'SystemInformationBlockType1_schedulinInformation_SEQOF_si-MessageType'},
%% 	  ms320,
%% 	  {'SystemInformationBlockType1_schedulinInformation_SEQOF_sib-MappingInfo'}}],
%% 	0
%%        }
%%       }
%%      }
%%     };
%% eutra2(result) ->
%% %% 55 5C A5 E0
%%     <<85,92,165,224>>.



%% compare([H|T1],[H|T2],Acc) ->
%%     compare(T1,T2,[H|Acc]);
%% compare([],[],_Acc) ->
%%     ok;
%% compare(L1,L2,Acc) ->
%%     {miss_match,L1,L2,lists:reverse(Acc)}.


