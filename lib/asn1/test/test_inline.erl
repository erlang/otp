%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2012. All Rights Reserved.
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

-export([compile/2,main/2,inline1/3,performance/1,performance2/0]).
-export([mvrasn_inlined_encdec/2,mvrasn_encdec/2,
	 mi_encdec/2,m_encdec/2]).

-include_lib("test_server/include/test_server.hrl").
-define(times, 5000).
-define(times2, 50000).

compile(Config, Options) ->
    CaseDir = ?config(case_dir, Config),
    asn1_test_lib:compile("Mvrasn.set.asn", Config, [{inline, mvrasn_inlined}|Options]),
    asn1_test_lib:compile("Mod.set.asn", Config, [{inline, m}|Options]),
    ok = remove_inlined_files(CaseDir, [filename:join([CaseDir, X])||X<-["m.erl", "m.beam"]]),
    asn1_test_lib:compile("Mod.set.asn", Config, [inline|Options]),
    ok = remove_inlined_files(CaseDir, []).

inline1(Config, Rule, Opt) ->
    CaseDir = ?config(case_dir, Config),

    asn1_test_lib:compile("P-Record", Config, [{inline, 'inlined_P_Record'}|Opt]),
    test_inline1(),

    ok=remove_inlined_files2(CaseDir, ber_bin_v2),

    case Rule of
        ber_bin_v2 ->
            asn1_test_lib:compile("P-Record", Config,
                                  [ber_bin, inline, asn1config, optimize|Opt]),
            test_inline2(Rule, 'P-Record'),
            remove_inlined_files3(CaseDir, Rule),
            asn1_test_lib:compile("p_record.set.asn", Config,
                                  [ber_bin, inline, asn1config, optimize|Opt]),
            test_inline2(Rule, 'p_record'),
            remove_inlined_files4(CaseDir, Rule);
        _ ->
            ok
    end.

main(Config, _Erule) ->
    Val = val(Config),
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

val(Config) ->
    {ok,Val} = asn1ct:value('Mvrasn','InsertSubscriberDataArg',
                            [{i, ?config(case_dir, Config)}]),
    Val.

performance(Config) ->
    Val = val(Config),
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
    {ok,B}='Mod':encode('L',Val),
    {ok,_R}='Mod':decode('L',B),
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
