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
-module(test_special_decode_performance).

-export([compile/2,go/1,loop2/4,loop1/5]).

-include_lib("test_server/include/test_server.hrl").


compile(Config,Rule) when Rule==ber_bin_v2 ->
    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),
    
    ?line asn1ct:compile(DataDir++"MEDIA-GATEWAY-CONTROL",
			 [ber_bin,optimize,asn1config,{outdir,OutDir},
			  {i,DataDir}]),
    ?line asn1ct:compile(DataDir++"PartialDecSeq",
			 [ber_bin,optimize,asn1config,{outdir,OutDir},
			  {i,DataDir}]);
compile(_,Rule) ->
    {skip,lists:concat(["not implemented yet for version: ",Rule])}.

go(all) ->
    {Time_S_s,Time_S_e,Time_S_c}=go(10000,'PartialDecSeq'),
    {Time_MGC_s,Time_MGC_e,Time_MGC_c}=go(10000,'MEDIA-GATEWAY-CONTROL'),
    ?line do_comment({Time_S_s,Time_MGC_s},
		     {Time_S_e,Time_MGC_e},
		     {Time_S_c,Time_MGC_c}).

go(N,Mod) ->
    ?line Val = val(Mod),
    ?line {ok,B} = Mod:encode(element(1,Val),Val),
    ?line go(Mod,list_to_binary(B),N).

go(Mod,Bin,N) ->
    ?line FsS = get_selective_funcs(Mod),
    ?line FsE = get_exclusive_funcs(Mod),
    ?line io:format("~nSize of value for module ~p: ~p bytes.~n~n",[Mod,size(Bin)]),
    ?line Time_s=go1(selective,Mod,FsS,Bin,N,0),
    ?line Time_e=go1(exclusive,Mod,FsE,Bin,N,0),
    ?line Time_c=go1(common,Mod,[decode],Bin,N,0),
    ?line {Time_s/length(FsS),Time_e/length(FsE),Time_c}.

go1(_,_,[],_,_,AccTime) ->
    ?line AccTime;
%% go1 for common decode
go1(common,Mod,_,Bin,N,_) ->
    ?line TT=get_top_type(Mod),
    ?line {Time,Result}=timer:tc(?MODULE,loop1,[Mod,decode,TT,Bin,N]),
    case Result of
	{ok,_R1} ->
	    io:format("common Decode ~p:decode, ~p times on time ~p~n",
		      [Mod,N,Time]);
	Err ->
	    io:format("common Decode ~p:decode failed: ~w~n~n",[Mod,Err])
    end,
    Time;
go1(Dec,Mod,[F|Fs],Bin,N,AccTime) ->
    ?line {Time,Result}=timer:tc(?MODULE,loop2,[Mod,F,Bin,N]),
    case Result of
	{ok,_R1} ->
	    io:format("~p Decode ~p:~p, ~p times on time ~p~n",[Dec,Mod,F,N,Time]);
	Err ->
	    io:format("~p Decode ~p:~p failed: ~w~n~n",[Dec,Mod,F,Err])
    end,
    go1(Dec,Mod,Fs,Bin,N,AccTime+Time).

do_comment({Time_S_s,Time_MGC_s},
	   {Time_S_e,Time_MGC_e},
	   {Time_S_c,Time_MGC_c}) ->
%    io:format("Time_s: ~w, Time_e: ~w, Time_c: ~w~n",[Time_s,Time_e,Time_c]),
    Time_sofc1 = Time_S_s/Time_S_c,
    Time_sofc2 = Time_MGC_s/Time_MGC_c,
    Time_eofc1 = Time_S_e/Time_S_c,
    Time_eofc2 = Time_MGC_e/Time_MGC_c,
    Av_proc_sofc = 
	integer_to_list(round(((100*Time_sofc1) + (100*Time_sofc2))/2)),
    Av_proc_eofc =
	integer_to_list(round(((100*Time_eofc1) + (100*Time_eofc2))/2)),
    io:format("Av_proc_sofc = ~w, Av_proc_eofc = ~w~n",
	      [Av_proc_sofc,Av_proc_eofc]),
    Comment = ["selective decode takes "++
	       Av_proc_sofc ++" % of common decode time",
	       "exclusive decode takes "++ Av_proc_eofc++
	       " % of common decode time"],
    {comment,Comment}.
    
val('PartialDecSeq') ->
    {'F',{fb,{'E',12,[{'D',13,true},{'D',14,false},{'D',15,true},{'D',16,false},{'D',13,true},{'D',14,false},{'D',15,true},{'D',16,false},{'D',13,true},{'D',14,false},{'D',15,true},{'D',16,false}],true,{da,[{'A',17,{'D',18,false}},{'A',19,{'D',20,true}},{'A',21,{'D',22,false}},{'A',17,{'D',18,false}},{'A',19,{'D',20,true}},{'A',21,{'D',22,false}},{'A',17,{'D',18,false}},{'A',19,{'D',20,true}},{'A',21,{'D',22,false}},{'A',17,{'D',18,false}},{'A',19,{'D',20,true}},{'A',21,{'D',22,false}},{'A',17,{'D',18,false}},{'A',19,{'D',20,true}},{'A',21,{'D',22,false}},{'A',17,{'D',18,false}},{'A',19,{'D',20,true}},{'A',21,{'D',22,false}}]}}}};

val('MEDIA-GATEWAY-CONTROL') ->
    {'MegacoMessage',asn1_NOVALUE,{'Message',1,{ip4Address,{'IP4Address',[125,125,125,111],55555}},{transactions,[{transactionReply,{'TransactionReply',50007,asn1_NOVALUE,{actionReplies,[{'ActionReply',0,asn1_NOVALUE,asn1_NOVALUE,[{auditValueReply,{auditResult,{'AuditResult',{'TerminationID',[],[255,255,255]},[{mediaDescriptor,{'MediaDescriptor',asn1_NOVALUE,{multiStream,[{'StreamDescriptor',1,{'StreamParms',{'LocalControlDescriptor',sendRecv,asn1_NOVALUE,asn1_NOVALUE,[{'PropertyParm',[0,11,0,7],[[52,48]],asn1_NOVALUE}]},{'LocalRemoteDescriptor',[[{'PropertyParm',[0,0,176,1],[[48]],asn1_NOVALUE},{'PropertyParm',[0,0,176,8],[[73,78,32,73,80,52,32,49,50,53,46,49,50,53,46,49,50,53,46,49,49,49]],asn1_NOVALUE},{'PropertyParm',[0,0,176,15],[[97,117,100,105,111,32,49,49,49,49,32,82,84,80,47,65,86,80,32,32,52]],asn1_NOVALUE},{'PropertyParm',[0,0,176,12],[[112,116,105,109,101,58,51,48]],asn1_NOVALUE}]]},{'LocalRemoteDescriptor',[[{'PropertyParm',[0,0,176,1],[[48]],asn1_NOVALUE},{'PropertyParm',[0,0,176,8],[[73,78,32,73,80,52,32,49,50,52,46,49,50,52,46,49,50,52,46,50,50,50]],asn1_NOVALUE},{'PropertyParm',[0,0,176,15],[[97,117,100,105,111,32,50,50,50,50,32,82,84,80,47,65,86,80,32,32,52]],asn1_NOVALUE},{'PropertyParm',[0,0,176,12],[[112,116,105,109,101,58,51,48]],asn1_NOVALUE}]]}}}]}}},{packagesDescriptor,[{'PackagesItem',[0,11],1},{'PackagesItem',[0,11],1}]},{statisticsDescriptor,[{'StatisticsParameter',[0,12,0,4],[[49,50,48,48]]},{'StatisticsParameter',[0,11,0,2],[[54,50,51,48,48]]},{'StatisticsParameter',[0,12,0,5],[[55,48,48]]},{'StatisticsParameter',[0,11,0,3],[[52,53,49,48,48]]},{'StatisticsParameter',[0,12,0,6],[[48,46,50]]},{'StatisticsParameter',[0,12,0,7],[[50,48]]},{'StatisticsParameter',[0,12,0,8],[[52,48]]}]}]}}}]}]}}}]}}}.

%% val('PartialDecSeq') ->
%%     {'F',{fb,{'E',35,[{'D',3,true},{'D',4,false},{'D',5,true},{'D',6,true},{'D',7,false},{'D',8,true},{'D',9,true},{'D',10,false},{'D',11,true},{'D',12,true},{'D',13,false},{'D',14,true}],false,{dc,{'E_d_dc',15,true,{'E_d_dc_dcc',17,4711}}}}}}.

loop1(Mod,decode,TT,Bin,1) ->
    {ok,_Msg}=Mod:decode(TT,Bin);
loop1(Mod,decode,TT,Bin,N) ->
    {ok,_Msg}=Mod:decode(TT,Bin),
    loop1(Mod,decode,TT,Bin,N-1).

loop2(Mod,FS,Bin,1) ->
    {ok,_Msg}=Mod:FS(Bin);
loop2(Mod,FS,Bin,N) ->
    {ok,_Msg}=Mod:FS(Bin),
    loop2(Mod,FS,Bin,N-1).

%% loop3(Mod,F,Bin,1) ->
%%     {ok,Msg}=Mod:F(Bin),
%%     decode_parts(Mod,F,Msg);
%% loop3(Mod,F,Bin,N) ->
%%     {ok,Msg}=Mod:F(Bin),
%%     decode_parts(Mod,F,Msg),
%%     loop3(Mod,F,Bin,N-1).

get_selective_funcs('PartialDecSeq') ->
%    [selected_decode_F1,selected_decode_F2,selected_decode_F3,selected_decode_F4];
    [selected_decode_F1,selected_decode_F3,selected_decode_F4];
get_selective_funcs('MEDIA-GATEWAY-CONTROL') ->
    [decode_MegacoMessage_selective].

get_exclusive_funcs('PartialDecSeq') ->
    [decode_F_fb_incomplete,decode_F_fb_exclusive2,decode_F_fb_exclusive3];
get_exclusive_funcs('MEDIA-GATEWAY-CONTROL') ->
    [decode_MegacoMessage_exclusive].

get_top_type('PartialDecSeq') ->
    'F';
get_top_type('MEDIA-GATEWAY-CONTROL') ->
    'MegacoMessage'.

%% decode_parts('PartialDecSeq',decode_F_fb_incomplete,Msg) ->
%%     {fb,{'E',12,{E_bKey,E_bMsg},true,{E_dKey,E_dMsg}}}=Msg,
%%     {ok,_}='Seq':decode_part(E_bKey,E_bMsg),
%%     {ok,_}='Seq':decode_part(E_dKey,E_dMsg);
%% decode_parts('PartialDecSeq',decode_F_fb_exclusive2,Msg) ->
%%     {fb,{'E',12,{E_bKey,E_bMsg},true,{d,{E_dKey,E_dMsg}}}} = Msg,
%%     {ok,_}='Seq':decode_part(E_bKey,E_bMsg),
%%     {ok,_}='Seq':decode_part(E_dKey,E_dMsg);
%% decode_parts('MEDIA-GATEWAY-CONTROL',decode_MegacoMessage_exclusive,Msg) ->
%%     {'MegacoMessage',asn1_NOVALUE,{'Message',1,{M_MidKey,M_MidMsg},
%% 				   {M_mBKey,M_mBMsg}}} = Msg,
%%     {ok,_}='MEDIA-GATEWAY-CONTROL':decode_part(M_MidKey,M_MidMsg),
%%     {ok,_}='MEDIA-GATEWAY-CONTROL':decode_part(M_mBKey,M_mBMsg).
    
    
