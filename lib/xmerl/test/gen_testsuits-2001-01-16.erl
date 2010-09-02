%%%-------------------------------------------------------------------
%%% File    : test2.erl
%%% Author  : Bertil Karlsson <bertil@finrod>
%%% Description : 
%%%
%%% Created : 26 Sep 2006 by Bertil Karlsson <bertil@finrod>
%%%-------------------------------------------------------------------
-module(gen_testsuits-2001-01-16).

-compile(export_all).
-include_lib("xmerl/include/xmerl.hrl").


%% generate(Suite) generates test suite modules. In those modules are
%% all test cases extracted from the corresponding ".testSet"
%% file. This program must be in the xmlSchema2002-01-16 directory of
%% the unpacked xmlSchema2002-01-16.tar file. The test suite files are
%% created in this directory.
generate(all) ->
    generate(nist),
    generate(sun),
    generate(msx);
generate(nist) -> 
    io:format("generating test suite source ~p~n",
	      ["NISTXMLSchema1-0-20020116.testSet"]),
    generate("NISTXMLSchema1-0-20020116.testSet"),
    io:format("compiling test suite source~n",[]),
    {ok,_}=compile:file("xmerl_xsd_NIST2002-01-16_SUITE.erl",[{i,"/view/bertil_xmerl/clearcase/otp/erts/lib/test_server/include/"},{i,"/view/bertil_xmerl/clearcase/otp/erts/lib/xmerl/include/"}]);
generate(sun) ->
    io:format("generating test suite source ~p~n",
	      ["SunXMLSchema1-0-20020116.testSet"]),
    generate("SunXMLSchema1-0-20020116.testSet"),
    io:format("compiling test suite source~n",[]),
    {ok,_}=compile:file("xmerl_xsd_Sun2002-01-16_SUITE.erl",[{i,"/view/bertil_xmerl/clearcase/otp/erts/lib/test_server/include/"},{i,"/view/bertil_xmerl/clearcase/otp/erts/lib/xmerl/include/"}]);
generate(msx) ->
    io:format("generating test suite source ~p~n",
	      ["MSXMLSchema1-0-20020116.testSet"]),
    generate("MSXMLSchema1-0-20020116.testSet"),
    io:format("compiling test suite source~n",[]),
    {ok,_}=compile:file("xmerl_xsd_MS2002-01-16_SUITE.erl",[{i,"/view/bertil_xmerl/clearcase/otp/erts/lib/test_server/include/"},{i,"/view/bertil_xmerl/clearcase/otp/erts/lib/xmerl/include/"}]);
generate(Suite) ->
    {E,_} = xmerl_scan:file(Suite),
    {ok,FileName} = create_suite_name(xmerl_xpath:string("./@name",E)),
    case create_suite_file(FileName) of
	{ok,IO} ->
	    emit_licens_text(IO),
	    emit_module_header(IO,list_to_atom(filename:rootname(FileName))),
	    generate2(abbrev(Suite),E,IO);
	_ ->
	    failed
    end.
generate2(Suite,E,IO) ->
    %% Each testGroup name is a function in the test suite.
    TestGroupNames = test_case_names(Suite),
    emit_all_function(TestGroupNames,IO),
    emit_init_per_suite(IO,Suite),
    emit_init_per_testcase(IO),
    TestGroups=[X||X=#xmlElement{}<-E#xmlElement.content],
    TestGroupGroups = group_testGroups(test_case_prefix(Suite),TestGroups),
    generate_test_case_functions(Suite,TestGroupGroups,IO).
    
group_testGroups(Prefs,TGs) ->
    group_testGroups(Prefs,TGs,[]).
group_testGroups([],[],Acc) ->
    lists:reverse(Acc);
group_testGroups(Prefs,TGs,Acc) ->
    SplitPrefs = fun([{_,[]}|T],F) ->
			 F(T,F);
		    ([{N,[H|T1]}|T2],_) ->
			 {H,N,[{N,T1}|T2]};
		    ([H|T],_) ->
			 {H,H,T};
		    (A,B) ->
			 io:format("SplitRefs: ~nA: ~p~nB: ~p~nTGs: ~p~n",[A,B,hd(TGs)]),
			 exit(dummy)
		 end,
    {Pref,TCName,Prefs2} = SplitPrefs(Prefs,SplitPrefs),
    Pred = 
	fun(E) ->
		case xmerl_xpath:string("@name",E) of
		    [#xmlAttribute{value=V}] -> 
			lists:prefix(Pref,V);
		    _ -> false
		end
	end,
    {TGG,TGs2}=lists:splitwith(Pred,TGs),
%%     case TGG of
%% 	[] ->
%% 	    io:format("hd(Prefs): ~p~n",[hd(Prefs)]);
%% 	_ ->
%% 	    ok
%%     end,
    group_testGroups(Prefs2,TGs2,acc_group({TCName,TGG},Acc)).

acc_group({Name,TGG},[{Name,AccP}|Acc]) ->
    [{Name,AccP++TGG}|Acc];
acc_group(TGG,Acc) ->
    [TGG|Acc].

generate_test_case_functions(_Suite,[],IO)  ->
    %%emit_schema_test(IO),
    %%emit_instance_test(IO),
    file:close(IO);
generate_test_case_functions(Suite,[{TCName,TGG}|TGGs],IO) ->
    emit_test_case_func(true,TCName,comment(TCName,TGG),IO),
    generate_test_cases(Suite,TGG,0,0,IO),
    generate_test_case_functions(Suite,TGGs,IO).

generate_test_cases(_,[],SIndex,EIndex,IO) ->
    emit_result_list_test(IO,SIndex,EIndex),
%%     emit_schema_result_list(IO,SIndex,EIndex),
%%     emit_instance_result_list(IO,SIndex,EIndex),
    ok;
generate_test_cases(Suite,[El|Els],SIndex,EIndex,IO) ->
    SchemaTest = xmerl_xpath:string("schemaTest",El),
    InstanceTest = xmerl_xpath:string("instanceTest",El),
    SIndex2 = generate_schema_tests(Suite,SchemaTest,IO,SIndex,
				    any_instance_tests(InstanceTest)),

    EIndex2=generate_instance_tests(InstanceTest,mk_state(SIndex),EIndex,IO),
    case {Els,any_tests(SchemaTest,InstanceTest)} of
%% 	{[],true} ->
%% 	    emit(IO,[".",nl]);
%%	{_,true} -> 
%%	    emit(IO,[",",nl]);
	_ ->
	    ok
    end,
    emit(IO,[nl,nl]),
    generate_test_cases(Suite,Els,SIndex2,EIndex2,IO).

    

%% 0 or 1 schemaTest/ testGroup, but a schemaTest may reference many schemas
generate_schema_tests(_Suite,[],_IO,SIndex,_) ->
    SIndex;
generate_schema_tests(Suite,[SchemaTest],IO,SIndex,AnyInstanceTests) ->
    case xmerl_xpath:string("current[@status=\"accepted\" or @status=\"stable\"]",SchemaTest) of
	[] ->
	    SIndex;
	_ ->
	    case exclude_case(Suite,xmerl_xpath:string("@name",SchemaTest)) of
		true ->
		    SIndex;
		_ ->
		    Refs = xmerl_xpath:string("schemaDocument/@xlink:href",
					      SchemaTest),
		    ExpectedValue = xmerl_xpath:string("expected/@validity",
						       SchemaTest),
		    ExpectedReturnValue = 
			expected_return_value_st(ExpectedValue,SIndex),
		    generate_schema_validation_call(ExpectedReturnValue,
						    Refs,0,SIndex,
						    IO,length(Refs)>1,
						    AnyInstanceTests)
	    end
    end.


generate_schema_validation_call(_RetVal,[],_Num,Sindex,_IO,_,false) ->
%%    emit(IO,[RetVal]),
    %% emit(IO,[".",nl]),
    Sindex;
generate_schema_validation_call(_RetVal,[],_Num,Sindex,_IO,_,true) ->
 %%   emit(IO,[RetVal]),
%    emit(IO,[",",nl]),
    Sindex;
generate_schema_validation_call(RetVal,[#xmlAttribute{value=Link}|Refs],
				Num,Sindex,IO,ManySchemas,AnyInstanceTests) ->
    XsdBase = filename:dirname(Link),
%%    FileName = lists:flatten(io_lib:format("filename:join([?config(data_dir,Config), ~p])", [list_to_atom(Link)])),
    AccState =
	case Num of
	    0 ->
		"";
	    _ ->
		",{state," ++ mk_state(Sindex - 1) ++ "}"
	end,
    if
	Sindex == 0 ->
	    emit(IO,[indent(2),mk_STResList(0)," = [],",nl,nl]);
	true ->
	    ok
    end,
    RetState =
	if
	    ManySchemas;AnyInstanceTests ->
		mk_state(Sindex);
	    true ->
		"_"
	end,
%%     emit(IO,[indent(2),"?line ",mk_state(Num)," = xmerl_xsd:process_schema(",nl,
%% 	     indent(25),FileName,",",nl,
%% 	     indent(25),"[{xsdbase,filename:join([?config(data_dir,Config),'",
%% 	     XsdBase,"'])}",AccState,"]),",nl]),
    emit(IO,[indent(2),"?line {",mk_STRes(Sindex),",",RetState,
				  "} = xmerl_xsd_lib:schema_test(Config,",
	     list_to_atom(Link),",",list_to_atom(XsdBase),",",RetVal,AccState,"),",nl]),
    emit(IO,[indent(2),mk_STResList(Sindex+1)," = [",mk_STRes(Sindex),"|",
	     mk_STResList(Sindex),"],",nl]),
    generate_schema_validation_call(RetVal,Refs,Num+1,Sindex+1,IO,
				    ManySchemas,AnyInstanceTests).

%% 0 or many instanceTests / testGroup
generate_instance_tests([],_,EI,_IO) ->
    EI;
generate_instance_tests([InstanceTest|ITs],State,Num,IO) ->
    case xmerl_xpath:string("current[@status=\"accepted\" or @status=\"stable\"]",InstanceTest) of
	[] ->
	    ok;
	_ ->
	    if
		Num == 0 ->
		    emit(IO,[indent(2),mk_ITResList(0)," = [],",nl]);
		true ->
		    ok
	    end,
	    [Ref] = xmerl_xpath:string("instanceDocument/@xlink:href",InstanceTest),
	    ExpectedValue = xmerl_xpath:string("expected/@validity",
					       InstanceTest),
	    ExpectedReturnValue = expected_return_value_it(ExpectedValue),
	    generate_instance_validation_call(ExpectedReturnValue,Ref,
					      State,Num,IO)
    end,
    case ITs of
	[] -> ok; %%emit(IO,[".",nl]);
	_ ->ok% emit(IO,[",",nl])
    end,
    generate_instance_tests(ITs,State,Num+1,IO).
    
generate_instance_validation_call(ExpectedReturnValue,
				  #xmlAttribute{value=Link},State,Num,IO) ->
    XMLBase = filename:dirname(Link),
    %%FileName = filename:basename(Link),
%%    FileName = lists:flatten(io_lib:format("filename:join([?config(data_dir,Config), ~p])", [list_to_atom(Link)])),

%%    E = mk_E(Num),

    Res = mk_ITRes(Num),

%%     emit(IO,[indent(2),"?line {",E,",_} = xmerl_scan:file(",FileName,",",nl,
%% 	     indent(32),"[{xmlbase,filename:join([?config(data_dir,Config),'",
%% 	     XMLBase,"'])}]),",nl]),

    emit(IO,[indent(2),"?line ",Res," = xmerl_xsd_lib:instance_test(Config,",list_to_atom(Link),",",list_to_atom(XMLBase),",",ExpectedReturnValue,",",State,"),",nl]),
    emit(IO,[indent(2),mk_ITResList(Num+1)," = [",Res,"|",mk_ITResList(Num),"],",nl]).

%%      emit(IO,[indent(2),"?line ",ExpectedReturnValue,
%% 	     " = xmerl_xsd:validate(",E,",[{state,",State,"}])"]).

any_tests([],[]) ->
    false;
any_tests(_,_) ->
    true.

any_instance_tests([]) ->
    false;
any_instance_tests(_) ->
    true.

expected_return_value_it([#xmlAttribute{value=Validity}]) ->
    case Validity of
	"valid" ->
	    valid;
	"invalid" ->
	    invalid;
	_ ->
	    notKnown
    end;
expected_return_value_it([]) ->
    "_".

expected_return_value_st([#xmlAttribute{value=Validity}],_Num) ->
%%    RetVal =
    case Validity of
%% 	"valid" -> "#xsd_state{errors=[]}";
%% 	"invalid" -> "#xsd_state{errors=[_Err|_Errs]}";
%% 	_ -> "#xsd_state{}"
%%     end,
%%     "  ?line " ++ RetVal ++ " = " ++ mk_state(Num).
	"valid" ->
	    valid;
	"invalid" ->
	    invalid;
	_ ->
	    notKnown
    end.

create_suite_name([#xmlAttribute{value=Name}]) ->
    FileName = lists:concat(['xmerl_','xsd_',Name,'_SUITE.erl']),
    {ok,FileName};
create_suite_name(_) ->
    error.

create_suite_file(FileName)  ->
    file:delete(FileName),
    case file:open(FileName,[append]) of
	{ok,IO} -> {ok,IO};
	_ -> failed
    end.

emit_licens_text(IO) ->
    emit(IO,[
"%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2006-2010. All Rights Reserved.
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
%%",nl,
"%%",nl,
"%%% Purpose : Test suite for the xmerl application",nl,nl,
"%% Do NOT edit this file. It is generated by the generate_xsd_suite module",nl,"%% For more info read the comments in the header of that file.",nl,nl]).

emit_module_header(IO,Module) ->
    emit(IO,["-module(",{asis,Module},").",nl,nl]),
    emit(IO,["-compile(export_all).",nl,nl]),
    emit(IO,["-include(",{asis,"test_server.hrl"},").",nl]),
    emit(IO,["-include_lib(",{asis,"xmerl/include/xmerl.hrl"},").",nl]),
    emit(IO,["-include_lib(",{asis,"xmerl/include/xmerl_xsd.hrl"},").",nl,nl,nl]).

emit_all_function(TestGroupNames,IO) ->
    
    emit(IO,["all(suite) -> [",nl]),
    emit(IO,[list_to_atom(X)||X<-indent_all(TestGroupNames)]),
    emit(IO,["].",nl,nl,nl]).

emit_init_per_suite(IO,Suite) ->
    emit(IO,["%% initialization before the test suite",nl]),
    emit(IO,["init_per_suite(Config) ->",nl,
	     "  Dog=test_server:timetrap({minutes,10}),",nl,
	     "  xmerl_xsd_lib:unpack(Config,",Suite,"),",nl,
	     "  {ok,LogFile} = xmerl_xsd_lib:create_error_log_file(Config,",Suite,"),",nl,
	     "  test_server:timetrap_cancel(Dog),",nl,
	     "  [{suite,",Suite,"},{xmerl_error_log,LogFile}|Config].",nl,nl]),
    emit(IO,["end_per_suite(Config) ->",nl,
	     "  xmerl_xsd_lib:rmdir(Config,",Suite,"),",nl,
	     "  xmerl_xsd_lib:close_error_log_file(Config),",nl,
	     "  ok.",nl,nl]).
    
emit_init_per_testcase(IO) ->
    emit(IO,["%% initialization before each testcase",nl]),
    emit(IO,["init_per_testcase(TestCase,Config) ->",nl,
	     indent(2),"Dog=test_server:timetrap({minutes,3}),",nl,
	     indent(2),"[{testcase,TestCase},{watchdog, Dog}|Config].",nl,nl]),
    emit(IO,["%% clean up after each testcase",nl,
	     "end_per_testcase(_Func,Config) ->",nl,
	     indent(2),"Dog=?config(watchdog, Config),",nl,
	     indent(2),"test_server:timetrap_cancel(Dog),",nl,
	     indent(2),"ok.",nl,nl]).

emit_test_case_func(false,TestCaseName,_Doc,IO) ->
    emit(IO,["%% skipped testcase ",{asis,TestCaseName},".",nl,
	     "%% There were no schemaTest case or instanceTest case.",nl,nl]);
emit_test_case_func(true,TestCaseName,Doc,IO) ->
%%    emit(IO,indent_comment(Doc)),
    emit(IO,Doc),
    emit(IO,[nl,{asis,list_to_atom(TestCaseName)},"(Config) when is_list(Config) ->",nl]).


emit_result_list_test(IO,0,0) ->
    emit(IO,["  xmerl_xsd_lib:compare_test_results(Config,[],[]).",nl,nl]);
emit_result_list_test(IO,STI,0) ->
    emit(IO,["  xmerl_xsd_lib:compare_test_results(Config,",mk_STResList(STI),
	     ",[]).",nl,nl]);
emit_result_list_test(IO,0,ITI) ->
    emit(IO,["  xmerl_xsd_lib:compare_test_results(Config,[],",mk_ITResList(ITI),
	     ").",nl,nl]);
emit_result_list_test(IO,STI,ITI) ->
    emit(IO,["  xmerl_xsd_lib:compare_test_results(Config,",mk_STResList(STI),
	     ",",mk_ITResList(ITI),").",nl,nl]).

emit_schema_result_list(_IO,0,_) ->
    ok;
emit_schema_result_list(IO,SIndex,0) ->
    emit(IO,[indent(2),"STResults = lists:reverse(",mk_STResList(SIndex),"),",nl]);
emit_schema_result_list(IO,SIndex,_) ->
    emit(IO,[indent(2),"STResults = lists:reverse(",mk_STResList(SIndex),"),",nl]).

emit_instance_result_list(_IO,0,0) ->
    ok;
emit_instance_result_list(IO,_,0) ->
    emit(IO,[indent(2),"xmerl_xsd_lib:compare_test_results(Config,STResults,[])).",nl]);
emit_instance_result_list(IO,0,I) ->
    emit(IO,[indent(2),"xmerl_xsd_lib:compare_test_results(Config,[],lists:reverse(",mk_ITResList(I),")).",nl]);
emit_instance_result_list(IO,_,I) ->
%%    emit(IO,indent_all(res_list(0,I))).
%%    emit(IO,["ITResults =",nl,"["]++res_list(0,I-1,fun mk_ITRes/1)++["].",nl]).
    emit(IO,[indent(2),"xmerl_xsd_lib:compare_test_results(Config,STResults,lists:reverse(",mk_ITResList(I),")).",nl]).
    

emit(IO,[nl|T]) ->
    io:format(IO,"~n",[]),
    emit(IO,T);
emit(IO,[comma|T]) ->
    io:format(IO,",",[]),
    emit(IO,T);
emit(IO,[H|T]) ->
    print(IO,H),
    emit(IO,T);
emit(_,[]) ->
    ok.

print(IO,Str) when is_list(Str) ->
    io:format(IO,"~s",[Str]);
print(IO,{asis,Sym}) ->
    io:format(IO,"~p",[Sym]);
print(IO,Term) ->
    io:format(IO,"~p",[Term]).

mk_state(I) ->
    mk_VarNum("S",I).

mk_E(Num) ->
    mk_VarNum("E",Num).

mk_VarNum(Var,Num) ->
    lists:concat([Var,Num]).

mk_ITRes(Num) ->
    mk_VarNum("ITRes",Num).
mk_STRes(Num) ->
    mk_VarNum("STRes",Num).
mk_STResList(Num) ->
    mk_VarNum("STResList",Num).
mk_ITResList(Num) ->
    mk_VarNum("ITResList",Num).

res_list(N,N,VariableFun) ->
    [VariableFun(N)];
res_list(M,N,VariableFun) ->
    [VariableFun(M),comma|res_list(M+1,N,VariableFun)].

indent(I) ->
    lists:flatten(lists:duplicate(I," ")).

indent_all([H|T]) ->
    indent_all(T,length(H),[H]).
indent_all([],_Col,Acc) ->
    lists:reverse(Acc);
indent_all([H|T],Col,Acc) when is_list(H) ->
    case length(H)+Col of
	I when I < 80 ->
	    indent_all(T,I+1,[H,"comma"|Acc]);
	_I ->
	    indent_all(T,length(H),[H,"nl","comma"|Acc])
    end.

indent_comment(Comment) ->
    indent_comment(string:tokens(Comment,"\n\t\s"),3,["%% "]).
indent_comment([],_,Acc) ->
    lists:reverse(Acc);
indent_comment([H|T],Col,Acc) ->
    case length(H) + Col of
	I when I > 80 ->
	    indent_comment(T,length(H) + 3,[H,"%% ",nl|Acc]);
	I ->
	    indent_comment(T,I + 1,[H," "|Acc])
    end.

test_case_names(nist) ->
    [X||X<-test_case_prefix(nist)];
test_case_names(msx) ->
    [X||X<-lists:map(fun({N,_})->N;(N)when is_list(N) -> N end,test_case_prefix(msx))];
test_case_names(sun) ->
    test_case_prefix(sun).

abbrev("NISTXMLSchema1-0-20020116.testSet") ->
    nist;
abbrev("MSXMLSchema1-0-20020116.testSet") ->
    msx;
abbrev(_) ->
    sun.
test_case_prefix(msx) ->
    ["att","ct","elem","group","idc_","id",
     {"mgABCD",["mgA","mgB","mgC","mgD"]},
     {"mgEFG",["mgE","mgF","mgG"]},{"mgHIJ",["mgH","mgI","mgJ"]},
     "mgK",{"mgLM",["mgL","mgM"]},"mgN",{"mgOP",["mgO","mgP"]},
     {"mgQR",["mgQ","mgR"]},"mgS",
     {"particlesAB",["particlesA","particlesB"]},
     {"particlesCDE",["particlesC","particlesD","particlesE"]},
     {"particlesFHI",["particlesF","particlesH","particlesI"]},
     "particlesJ",
     {"particlesKOSRTQUVW",
      ["particlesK","particlesO","particlesS","particlesR","particlesT",
       "particlesQ","particlesU","particlesV","particlesW"]},
     {"stABCDE",["stA","stB","stC","stD","stE"]},
     {"stFGH",["stF","stG","stH"]},{"stIJK",["stI","stJ","stK"]},
     "stZ",{"wildABCDEF",["wildA","wildB","wildC","wildD","wildE","wildF"]},
     {"wildGHI",["wildG","wildH","wildI"]},
     {"wildJKLMNQOP",["wildJ","wildK","wildL","wildM","wildN",
			    "wildQ","wildO","wildP"]},"wildZ"];
test_case_prefix(nist) ->
    ["NISTSchema-anyURI","NISTSchema-base64Binary","NISTSchema-boolean",
     "NISTSchema-byte","NISTSchema-date-","NISTSchema-dateTime",
     "NISTSchema-decimal","NISTSchema-double","NISTSchema-duration",
     "NISTSchema-float","NISTSchema-gDay","NISTSchema-gMonth-",
     "NISTSchema-gMonthDay","NISTSchema-gYear-","NISTSchema-gYearMonth",
     "NISTSchema-hexBinary","NISTSchema-ID","NISTSchema-int-",
     "NISTSchema-integer","NISTSchema-language","NISTSchema-long",
     "NISTSchema-Name","NISTSchema-NCName","NISTSchema-negativeInteger",
     "NISTSchema-NMTOKEN","NISTSchema-nonNegativeInteger",
     "NISTSchema-nonPositiveInteger","NISTSchema-normalizedString",
     "NISTSchema-positiveInteger","NISTSchema-QName","NISTSchema-short",
     "NISTSchema-string","NISTSchema-time","NISTSchema-token",
     "NISTSchema-unsignedByte","NISTSchema-unsignedInt",
     "NISTSchema-unsignedLong","NISTSchema-unsignedShort"];
test_case_prefix(sun) ->
    ["Sun-idc001.nogen", "Sun-idc002.e", "Sun-idc002b.e", "Sun-idc003.e",
     "Sun-idc004.nogen", "Sun-idc004a.e", "Sun-idc005.nogen",
     "Sun-idc006.nogen", "Sun-xsd001", "Sun-xsd002", "Sun-xsd003-1.e",
     "Sun-xsd003-2.e", "Sun-xsd003a", "Sun-xsd003b", "Sun-xsd004",
     "Sun-xsd005", "Sun-xsd006", "Sun-xsd008", "Sun-xsd011", "Sun-xsd012",
     "Sun-xsd013.e", "Sun-xsd014.e", "Sun-xsd015.e", "Sun-xsd016.e",
     "Sun-xsd017.e", "Sun-xsd018.e", "Sun-xsd019.e", "Sun-xsd020.e",
     "Sun-xsd020-2.e", "Sun-xsd020-3.e", "Sun-xsd020-4.e", "Sun-xsd021",
     "Sun-xsd022", "Sun-xsd023.e", "Sun-xsiType1", "Sun-xsiType-block-1",
     "Sun-xsiType-block-2", "Sun-xsiType-block-3", "Sun-xsiType-block-4",
     "Sun-type-and-subst-1"].

comment([$S,$u,$n|_],[TG]) ->
    case xmerl_xpath:string("annotation/documentation/text()",TG) of
	L=[#xmlText{}|_] ->
	    ["%% "|[X||#xmlText{value=X}<-L]];
	_ ->
	    ["%%"]
    end;
comment("att",_) ->
    ["%% Syntax Checking for Attribute Declaration",nl];
comment("ct",_) ->
    ["%% Syntax Checking for top level complexType Declaration.",nl,
    "%% Syntax Checking for simpleContent complexType Declaration.",nl,
    "%% Syntax Checking for comlexContent complexType Declaration",nl,
    "%% complexType Validation checking",nl,
    "%% complexType Schema Component Constraints",nl];
comment("elem",_) ->
    ["%% 3.3.2 XML Representation of Element Declaration.",nl,
     "%% 3.3.4 Element Declaration Validation Rules.",nl,
     "%% element Validation checking.",nl,
     "%% Regular Expression Validation checking.",nl,
     "%% Bug Regressions Specs section: 3.3.4",nl];
comment("group",_) ->
    ["%% Syntax Checking Model Group Tests.",nl,
     "%% Content Checking Model Group Tests.",nl];
comment("idc_",_) ->
    ["%% 3.11.1 The Identity-constraint Definition Schema Component.",nl];
comment("id",_) ->
    ["%% Identity-constraint Definition Schema Component.",nl,
     "%% Identity-constraint Validation Rules.",nl,
     "%% Selector identity-constraint xpath bnf.",nl,
     "%% Field identity-constraint xpath bnf.",nl,
     "%% XPath validation.",nl,
     "%% Bug Regressions",nl];
comment("mgABCD",_) ->
    ["%% model groups (ALL).",nl];
comment("mgEFG",_) ->
    ["%% model groups ( sequence ).",nl];
comment("mgHIJ",_) ->
    ["%% model groups ( choice ).",nl];
comment("mgK",_) ->
    ["%% model group validation checking (sequence).",nl];
comment("mgLM",_) ->
    ["%% model group validation checking (choice, all).",nl];
comment("mgN",_) ->
    ["%% Element Sequence Valid.",nl];
comment("mgOP",_) ->
    ["%% All Group Limited.",nl];
comment("mgQR",_) ->
    ["%% Element Declarations Consistent, 3.8.6",nl];
comment("mgS",_) ->
    ["%% Deterministic Sequences.",nl];
comment("particlesAB",_) ->
    ["%% 3.9.1 The Particle Schema Component.",nl];
comment("particlesCDE",_) ->
    ["%% 3.9.4 Particle Validation Rules: Element Sequence Locally Valid.",nl];
comment("particlesFHI",_) ->
    ["%% 3.9.6 Schema Component Constraint: Particle ....",nl];
comment("particlesJ",_) ->
    ["%% 3.9.6 Particle Derivation.",nl];
comment("particlesKOSRTQUVW",_) ->
    ["%% 3.9.6 Particle Restriction.",nl];
comment("stABCDE",_) ->
    ["%% Syntax Checking for simpleType Declaration.",nl];
comment("stFGH",_) ->
    ["%% simpleType Validation checking.",nl];
comment("stIJK",_) ->
    ["%% simpleType Schema Component Constraints.",nl];
comment("stZ",_) ->
    ["%% Bug Regressions.",nl];
comment("wildABCDEF",_) ->
    ["%% Syntax Validation - any.",nl];
comment("wildGHI",_) ->
    ["%% 3.10.4 Wildcard Validation Rules - any.",nl];
comment("wildJKLMNQOP",_) ->
    ["%% Syntax Validation - anyAttribute.",nl];
comment("wildZ",_) ->
    ["%% Bugs - Wildcards.",nl];
comment("NISTSchema-anyURI",_) ->
    ["%% Data type derived by restriction of anyURI by facets",nl];
comment("NISTSchema-base64Binary",_) ->
    ["%% Data type derived by restriction of base64binary by facets",nl];
comment("NISTSchema-boolean",_) ->
    ["%% Data type derived by restriction of boolean by facets",nl];
comment("NISTSchema-byte",_) ->
    ["%% Data type derived by restriction of byte by facets",nl];
comment("NISTSchema-date-",_) ->
    ["%% Data type derived by restriction of date by facets",nl];
comment("NISTSchema-dateTime",_) ->
    ["%% Data type derived by restriction of dateTime by facets",nl];
comment("NISTSchema-decimal",_) ->
    ["%% Data type derived by restriction of decimal by facets",nl];
comment("NISTSchema-double",_) ->
    ["%% Data type derived by restriction of double by facets",nl];
comment("NISTSchema-duration",_) ->
    ["%% Data type derived by restriction of duration by facets",nl];
comment("NISTSchema-float",_) ->
    ["%% Data type derived by restriction of float by facets",nl];
comment("NISTSchema-gDay",_) ->
    ["%% Data type derived by restriction of gDay by facets",nl];
comment("NISTSchema-gMonth-",_) ->
    ["%% Data type derived by restriction of gMonth by facets",nl];
comment("NISTSchema-gMonthDay",_) ->
    ["%% Data type derived by restriction of gMonthDay by facets",nl];
comment("NISTSchema-gYear-",_) ->
    ["%% Data type derived by restriction of gYear by facets",nl];
comment("NISTSchema-gYearMonth",_) ->
    ["%% Data type derived by restriction of gYearMonth by facets",nl];
comment("NISTSchema-hexBinary",_) ->
    ["%% Data type derived by restriction of hexBinary by facets",nl];
comment("NISTSchema-ID",_) ->
    ["%% Data type derived by restriction of ID by facets",nl];
comment("NISTSchema-int-",_) ->
    ["%% Data type derived by restriction of int by facets",nl];
comment("NISTSchema-integer",_) ->
    ["%% Data type derived by restriction of integer by facets",nl];
comment("NISTSchema-language",_) ->
    ["%% Data type derived by restriction of language by facets",nl];
comment("NISTSchema-long",_) ->
    ["%% Data type derived by restriction of long by facets",nl];
comment("NISTSchema-Name",_) ->
    ["%% Data type derived by restriction of Name by facets",nl];
comment("NISTSchema-NCName",_) ->
    ["%% Data type derived by restriction of NCName by facets",nl];
comment("NISTSchema-negativeInteger",_) ->
    ["%% Data type derived by restriction of negativeInteger by facets",nl];
comment("NISTSchema-NMTOKEN",_) ->
    ["%% Data type derived by restriction of NMTOKEN by facets",nl];
comment("NISTSchema-nonNegativeInteger",_) ->
    ["%% Data type derived by restriction of nonNegativeInteger by facets",nl];
comment("NISTSchema-nonPositiveInteger",_) ->
    ["%% Data type derived by restriction of nonPositiveInteger by facets",nl];
comment("NISTSchema-normalizedString",_) ->
    ["%% Data type derived by restriction of normalizedString by facets",nl];
comment("NISTSchema-positiveInteger",_) ->
    ["%% Data type derived by restriction of positiveInteger by facets",nl];
comment("NISTSchema-QName",_) ->
    ["%% Data type derived by restriction of QName by facets",nl];
comment("NISTSchema-short",_) ->
    ["%% Data type derived by restriction of short by facets",nl];
comment("NISTSchema-string",_) ->
    ["%% Data type derived by restriction of string by facets",nl];
comment("NISTSchema-time",_) ->
    ["%% Data type derived by restriction of time by facets",nl];
comment("NISTSchema-token",_) ->
    ["%% Data type derived by restriction of token by facets",nl];
comment("NISTSchema-unsignedByte",_) ->
    ["%% Data type derived by restriction of unsignedByte by facets",nl];
comment("NISTSchema-unsignedInt",_) ->
    ["%% Data type derived by restriction of unsignedInt by facets",nl];
comment("NISTSchema-unsignedLong",_) ->
    ["%% Data type derived by restriction of unsignedLong by facets",nl];
comment("NISTSchema-unsignedShort",_) ->
    ["%% Data type derived by restriction of unsignedShort by facets",nl].    


emit_schema_test(IO) ->
    emit(IO,["schema_test(Config,FileName,XsdBase,Validity) ->",nl,
	     indent(3),"{Ok,S} = xmerl_xsd:process_schema(",
	     "filename:join([?config(data_dir,Config),FileName]),",nl,
	     indent(25),
	     "[{xsdbase,filename:join([?config(data_dir,Config),XsdBase])}]),",nl,
	     indent(3),"case Validity of",nl,
	     indent(6),valid ," when Ok == ok ->",nl,
	     indent(9),"{{filename:basename(FileName),S#xsd_state.errors == []},S};", nl,
	     indent(6),invalid," when Ok == error ->",nl,
	     indent(9), "{{filename:basename(FileName),true},S};",nl,
	     indent(6),notKnown," ->",nl,
	     indent(9),"{{filename:basename(FileName),true},S};",nl,
	     indent(6),"_ -> {{filename:basename(FileName),false},S}",nl,
	     indent(3),"end.",nl]),
    emit(IO,["schema_test(Config,FileName,XsdBase,Validity,AccState) ->",nl,
	     indent(3),"{Ok,S2} = xmerl_xsd:process_schema(",
	     "filename:join([?config(data_dir,Config),FileName]),",nl,
	     indent(25),
	     "[{xsdbase,filename:join([?config(data_dir,Config),XsdBase])}, AccState]),",nl,
	     indent(3),"case Validity of",nl,
	     indent(6),valid," when Ok == ok ->",nl,
	     indent(9),"{{filename:basename(FileName),S2#xsd_state.errors == []},S2};", nl,
	     indent(6),invalid," when Ok == error ->",nl,
	     indent(9), "{{filename:basename(FileName),true},S2};",nl,
	     indent(6),notKnown," ->",nl,
	     indent(9),"{{filename:basename(FileName),true},S2};",nl,
	     indent(6),"_ -> {{filename:basename(FileName),false},S2}",nl,
	     indent(3),"end.",nl]).

emit_instance_test(IO) ->
    emit(IO,["instance_test(Config,FileName,XMLBase,Validity,State) ->",nl]),    
    emit(IO,[indent(3),"{E,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),FileName]),",nl,
	     indent(19),"[{xmlbase,filename:join([?config(data_dir,Config),",nl,
	     indent(19),"XMLBase])}]),",nl]),
    emit(IO,[indent(3),"{VE,S2} = xmerl_xsd:validate(E,[{state,State}]),",nl]),
    emit(IO,[indent(3),"case Validity of",nl,
	     indent(6),"valid when is_record(VE,xmlElement) ->",nl,
	     indent(9),"{filename:basename(FileName),S2#xsd_state.errors == []};",nl,
	     indent(6),"invalid when VE == error ->",nl,
	     indent(9),"{filename:basename(FileName),true};",nl,
	     indent(6),"notKnown ->",nl,
	     indent(9),"{filename:basename(FileName),true};",nl,
	     indent(6),"_ ->",nl,
	     indent(9),"{filename:basename(FileName),false}",nl,
	     indent(3),"end.",nl]).
%%     emit(IO,[indent(3),"{VE,_} = xmerl_xsd:validate(E,[{state,State}]),",nl]),
%%     emit(IO,[indent(3),"{filename:basename(FileName),VE == ExpectedRetVal}."]).

exclude_case(_Suite,[]) ->
    false;
exclude_case(Suite,[#xmlAttribute{value=Name}]) ->
    exlude_case(Suite,Name).

exlude_case(msx,Case) ->
    lists:member(Case,["attC002","mgS002","mgS003","mgS004","mgS005"]);
exlude_case(_,_) ->
    false.
