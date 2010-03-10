%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2009. All Rights Reserved.
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
-module(test_inline).

-export([compile/3,main/1,inline1/3,performance/1,performance2/0]).
-export([mvrasn_inlined_encdec/2,mvrasn_encdec/2,
	 mi_encdec/2,m_encdec/2]).

-include_lib("test_server/include/test_server.hrl").
-define(times, 5000).
-define(times2, 50000).

compile(Config,_Rules,Opt) ->
    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),
    ?line true = code:add_patha(DataDir),
    
    ?line ok=asn1ct:compile(DataDir++"Mvrasn.set.asn",[{inline,mvrasn_inlined},{outdir,OutDir}]++Opt),
    ?line ok=asn1ct:compile(DataDir++"Mvrasn-11-6.asn",[{outdir,OutDir}]++Opt),

    ?line ok=asn1ct:compile(DataDir++"Mod.set.asn",[{inline,m},{outdir,OutDir}]++Opt),
    ?line ok=remove_inlined_files(OutDir,[filename:join([OutDir,X])||X<-["m.erl","m.beam"]]),
    ?line ok=asn1ct:compile(DataDir++"Mod.set.asn",[inline,{outdir,OutDir}]++Opt),
    ?line ok=remove_inlined_files(OutDir,[]).
    
inline1(Config,Rule,Opt) ->
    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    
    ?line ok=asn1ct:compile(DataDir++"P-Record",
			    [{inline,'inlined_P_Record'},
			     {outdir,OutDir}]++Opt),
    ?line test_inline1(),
    
    ?line ok=remove_inlined_files2(OutDir,ber_bin_v2),

    case Rule of
	ber_bin_v2 ->
	    ?line ok=asn1ct:compile(DataDir++"P-Record",
				    [inline,asn1config,ber_bin,optimize,
				     {outdir,OutDir}]++Opt),
	    ?line test_inline2(Rule,'P-Record'),
	    ?line remove_inlined_files3(OutDir,Rule),
	    io:format("compiling ~p~nwith ~p~n",
		      [DataDir ++ "p_record.set.asn",
		       [inline,asn1config,ber_bin,optimize,{outdir,OutDir}]++Opt]),
	    ?line ok = asn1ct:compile(DataDir ++ "p_record.set.asn",
				      [inline,asn1config,ber_bin,optimize,
				       {outdir,OutDir}]++Opt),
	    ?line test_inline2(Rule,'p_record'),
	    ?line remove_inlined_files4(OutDir,Rule);
	_ ->
	    ok
    end.

main(_Erule) ->
    ?line Val = val(),
    ?line {ok,Bytes}=asn1_wrapper:encode(mvrasn_inlined,'InsertSubscriberDataArg',Val),
    ?line {ok,_Val2}=asn1_wrapper:decode(mvrasn_inlined,'InsertSubscriberDataArg',Bytes).

test_inline1() ->
     PRecMsg = {'PersonnelRecord',{'Name',"Sven","S","Svensson"},
	       "manager",123,"20000202",{'Name',"Inga","K","Svensson"},
		asn1_DEFAULT},
    ?line {ok,Bytes}=asn1_wrapper:encode('inlined_P_Record','PersonnelRecord',
				   PRecMsg),
    ?line {ok,_}=asn1_wrapper:decode('inlined_P_Record',
				     'PersonnelRecord',Bytes).

test_inline2(ber_bin_v2,Mod) ->
    PRecMsg = {'PersonnelRecord',{'Name',"Sven","S","Svensson"},
	       "manager",123,"20000202",{'Name',"Inga","K","Svensson"},
	       asn1_DEFAULT},
    ?line {ok,Bytes} = Mod:encode('PersonnelRecord',PRecMsg),
    ?line {ok,_} = Mod:sel_dec(list_to_binary(Bytes));
test_inline2(_,_) ->
    ok.

val() ->
    ?line {ok,Val} = asn1ct:value('Mvrasn-11-6','InsertSubscriberDataArg'),
    Val.

performance(Config) ->
    ?line true = code:add_patha(?config(priv_dir,Config)),
    ?line true = code:add_patha(?config(data_dir,Config)),
    ?line Val = val(),
    %% warm up
    timer:tc(?MODULE,mvrasn_inlined_encdec,[2,Val]),
    %% performance test
    ?line {Time1,ok}=timer:tc(?MODULE,mvrasn_inlined_encdec,[?times,Val]),
    %% warm up
    timer:tc(?MODULE,mvrasn_encdec,[2,Val]),
    %% performance test
    ?line {Time2,ok}=timer:tc(?MODULE,mvrasn_encdec,[?times,Val]),

    ?line Comment = "inlined_code: "++
	integer_to_list(round(Time1/?times))++
	" micro,<br>original_code: "++
	integer_to_list(round(Time2/?times))++
% 	" micro,~ninlined_code[inline]: "++
% 	integer_to_list(round(Time3/?times))++
	" micro",
    {comment,Comment}.


mvrasn_inlined_encdec(0,_) ->
    ok;
mvrasn_inlined_encdec(N,V) ->
    ?line {ok,B}=mvrasn_inlined:encode('InsertSubscriberDataArg',V),
    ?line {ok,_R}=mvrasn_inlined:decode('InsertSubscriberDataArg',B),
    mvrasn_inlined_encdec(N-1,V).

mvrasn_encdec(0,_) ->
    ok;
mvrasn_encdec(N,V) ->
    ?line {ok,B}='Mvrasn-11-6':encode('InsertSubscriberDataArg',V),
    ?line {ok,_R}='Mvrasn-11-6':decode('InsertSubscriberDataArg',B),
    mvrasn_encdec(N-1,V).

%% mvrasn_inlined_i_encdec(0,_) ->
%%     ok;
%% mvrasn_inlined_i_encdec(N,V) ->
%%     {ok,B}=mvrasn_inlined_i:encode('InsertSubscriberDataArg',V),
%%     {ok,_R}=mvrasn_inlined_i:decode('InsertSubscriberDataArg',B),
%%     mvrasn_inlined_i_encdec(N-1,V).

performance2() ->
    Val = mval(),
    %% warm up
    timer:tc(?MODULE,mi_encdec,[?times,Val]),
    %% performance test
    {Time1,_R1}=timer:tc(?MODULE,mi_encdec,[?times2,Val]),
    %% warm up
    timer:tc(?MODULE,m_encdec,[?times,Val]),
    %% performance test
    {Time2,_R2}=timer:tc(?MODULE,m_encdec,[?times2,Val]),
    ?line Comment = "inlined_code: "++
	integer_to_list(round(Time1/?times2))++
	" micro,<br>original_code: "++
	integer_to_list(round(Time2/?times2))++
	" micro<br>"++
	"The inlined code was "++
	integer_to_list(round(((Time2-Time1)/Time2)*100))++
	" % faster than the original code.",
    {comment,Comment}.

mi_encdec(0,_) ->
    ok;
mi_encdec(N,Val) ->
    {ok,B}=m:encode('L',Val),
    {ok,_R}=m:decode('L',B),
%    io:format("a"),
    mi_encdec(N-1,Val).

m_encdec(0,_) ->
    ok;
m_encdec(N,Val) ->
    {ok,B}='Mod1':encode('L',Val),
    {ok,_R}='Mod1':decode('L',B),
    m_encdec(N-1,Val).


-record('L', {country, region, name}).
-record('OtherName', {locationName, thingName}).
-record('FamilyName', {prefix, secondname}).
-record('Lang', {l}).
-record('Inhabitant', {name, country}).
-record('Country', {name, language}).
-record('PersonName', {name1, name2}).
-record('LocName', {region, name}).
-record('Reg', {name, inhabitants}).


mval() ->
    'L'().
'L'() ->
    #'L'{
     country='Co'(),
     region='Reg'(),
     name='Name'(othername)}.
'Co'() ->
    'Country'().
'Country'()->
    #'Country'{name='Name'(othername),
	       language='Lang'()}.
'Lang'()->
    #'Lang'{l="englsh"}.
'Reg'() ->
    #'Reg'{
       name='Name'(othername),
       inhabitants='Inhabitants'()}.
'Inhabitants'()->
    lists:duplicate(5,'Inhabitant'()).
'Inhabitant'()->
    #'Inhabitant'{name='Name'(person),
		 country='Country'()}.
'Name'(person) ->
    {person,'PersonName'()};
'Name'(othername) ->
    {othername,'OtherName'()}.
'PersonName'()->
    #'PersonName'{name1='FirstName'(firstname),
		  name2='FamilyName'()}.
'OtherName'()->
    #'OtherName'{locationName='LocName'(),
		 thingName='ThingName'()}.
'FirstName'(firstname)->
    {firstname,"Henry"};
'FirstName'(nickname) ->
    {nickname,"nick"}.
'FamilyName'() ->
    #'FamilyName'{prefix=none,
		  secondname="Lloyd"}.
'ThingName'()->
    "Enkoping".
'LocName'()->
    #'LocName'{
	   region=svealand,
	   name="Enkoping"}.

remove_inlined_files(Dir,Files) ->
    ModList=[filename:join([Dir,X])||X<-["Mod"]],
    FileList=Files++ mods2files(ModList,".asn1db")++
	mods2files(ModList,".beam")++
	mods2files(ModList,".erl")++mods2files(ModList,".hrl"),
    lists:foreach(fun(X) ->
			  io:format("X: ~p~n",[X]),
			  ?line ok=file:delete(X)
		  end,FileList),
    ok.
mods2files(ModList,Extension) ->
    [X++Extension||X<-ModList].

    
remove_inlined_files2(Dir,Rule) ->
    ?line ok=remove_inlined_files3(Dir,Rule),
    TargetErl=filename:join([Dir,"inlined_P_Record.erl"]),
    TargetBeam=filename:join([Dir,"inlined_P_Record.beam"]),
    lists:foreach(fun(X) ->
			  ?line ok=file:delete(X)
		  end,[TargetErl,TargetBeam]),
    ok.
remove_inlined_files3(Dir,ber_bin_v2) ->
    Erl=filename:join([Dir,"P-Record.erl"]),
    Beam=filename:join([Dir,"P-Record.beam"]),
    Asn1DB=filename:join([Dir,"P-Record.asn1db"]),
    Hrl=filename:join([Dir,"P-Record.hrl"]),
    lists:foreach(fun(X) ->
			  ?line ok=file:delete(X)
		  end,[Erl,Beam,Asn1DB,Hrl]),
    ok;
remove_inlined_files3(_,_) ->
    ok.

remove_inlined_files4(Dir,ber_bin_v2) ->
    Erl=filename:join([Dir,"p_record.erl"]),
    Beam=filename:join([Dir,"p_record.beam"]),
    Asn1DB=filename:join([Dir,"p_record.asn1db"]),
    Hrl=filename:join([Dir,"p_record.hrl"]),
    ErlBak=filename:join([Dir,"p_record.erl.bak"]),
    file:delete(ErlBak),
    lists:foreach(fun(X) ->
			  ?line ok=file:delete(X)
		  end,[Erl,Beam,Asn1DB,Hrl]),
    ok;
remove_inlined_files4(_,_) ->
    ok.
