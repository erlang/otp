%%%-------------------------------------------------------------------
%%% File    : test_xmerl.erl
%%% Author  : Bertil Karlsson <bertil@finrod>
%%% Description : 
%%%
%%% Created :  2 Dec 2003 by Bertil Karlsson <bertil@finrod>
%%%-------------------------------------------------------------------
-module(test_xmerl).

-compile(export_all).

-define(xmerl_test_root,"/ldisk/xml/xml-test-suite/xmlconf").
-define(jclark_subdir,"/ldisk/xml/xml-test-suite/xmlconf/xmltest").
-define(sun_subdir,"/ldisk/xml/xml-test-suite/xmlconf/sun").
-define(xerox_subdir,"/ldisk/xml/xml-test-suite/xmlconf/japanese").
-define(oasis_subdir,"/ldisk/xml/xml-test-suite/xmlconf/oasis").
-define(ibm_subdir,"/ldisk/xml/xml-test-suite/xmlconf/ibm").


get_xmlconf() ->
    FileName = filename:join(?xmerl_test_root,"xmlconf.xml"),
    {ok,L} = read_file(FileName),
    L.

read_file(FileName) ->
    case file:read_file(FileName) of
	{ok,Binary} ->
	    {ok,binary_to_list(Binary)};
	Err ->
	    exit({error,Err})
    end.

get_file(F="sun"++_Rest) ->
    get_sun_xml(F);
get_file(F="jclark-xmltest") ->
    get_jclark_xml(F);
get_file(F="xerox-japanese") ->
    get_xerox_xml(F);
get_file(F="nist-oasis") ->
    get_oasis_xml(F);
get_file("ibm-"++Rest) ->
    get_ibm_xml(Rest).
  

get_sun_xml(F) ->
    FileName = filename:join([?sun_subdir,F++".xml"]),
    {ok,L} = read_file(FileName),
    L.
get_jclark_xml(_F) ->
    FileName = filename:join([?jclark_subdir,"xmltest.xml"]),
    {ok,L} = read_file(FileName),
    L.
get_xerox_xml(_F) ->
    FileName = filename:join([?xerox_subdir,"japanese.xml"]),
    {ok,L} = read_file(FileName),
    L.
get_oasis_xml(_F) ->
    FileName = filename:join([?oasis_subdir,"oasis.xml"]),
    {ok,L} = read_file(FileName),
    L.
get_ibm_xml(F) ->
    FileName = filename:join([?ibm_subdir,"ibm_oasis_"++F++".xml"]),
    {ok,L} = read_file(FileName),
    L.

%% The generated xml file must have a unique name: concatenate the 
%% sub directory name and the ID of the TEST tag.
%% In each file the start of xmlconf.xml is included. Then a TEST
%% follows. It must be properly finished with end tags.
extract_TESTSUITEs() ->
    TestSuites = ["sun-valid","sun-invalid","sun-not-wf","sun-error",
		 "jclark-xmltest","xerox-japanese","nist-oasis",
		 "ibm-invalid","ibm-not-wf","ibm-valid"],
    Pid=spawn_link(?MODULE,ticker,[]),
    lists:foreach(fun extract_TESTCASES/1,TestSuites),
    generate_testfuncs(),
    xmerl_ticker ! finished.

extract_TESTCASES(Suite) ->
%    io:format("extract_TESTCASES:1~n",[]),
    L = get_file(Suite), % fetch xml file in sub directory
%    io:format("Reading Suite: ~p~n",[Suite]),
    Prol = xmlconf_prolog(),
%    io:format("Reading xmlconf.xml~n",[]),
    xmerl_ticker ! {suite,Suite},
    extract_TESTs(Suite,L,Prol,[]).

extract_TESTs(Suite,TCfile,Prol,TCAcc) ->
    case find_start_tag(Suite,TCfile,[],TCAcc) of
	{[],_,_,_} -> ok;
%%	    print_filenames();
	{T,Rest,TC,ID} ->
	    generate_TEST(Suite,ID,Prol,TC,T),
	    extract_TESTs(Suite,Rest,Prol,TC)
%	    file_output("out.xml",L1)
    end.

generate_TEST(Suite,ID,Prolog,TC,T) ->
%    GenDir = filename:join(["test",sub_dir(Suite)]),
    GenDir = sub_dir(Suite),
    Extension = extension(Suite),
%    Filename = filename:join([GenDir,ID ++ Extension]),
    Filename = mk_filename(Suite,GenDir,ID ++ Extension),
    save_filename(Suite,ID),
    {ok,IOF}=file:open(Filename,[write]),
    TSEnding = testsuite_end(),
    TCEnding = testcases_end(length(TC)),
    file:write(IOF,Prolog ++ "\n\n" ++ TC ++ "\n\n" ++ T ++ "\n\n" ++
	       TCEnding++ "\n\n" ++ TSEnding),
    file:close(IOF).

save_filename("xerox-japanese",Name) ->
    NewName = "japanese-"++Name,
    save_filename1(NewName);
save_filename(_,Name) ->
    save_filename1(Name).

save_filename1(Name) ->
    Saves = case get(filenames) of
		undefined ->
		    [];
		L -> L
	    end,
    put(filenames,[Name|Saves]).

generate_testfuncs() ->
    Filenames = get(filenames),
    Res = (catch generate_testfuncs(lists:reverse(Filenames))),
    io:format("~p~n",[Res]).
generate_testfuncs(Filenames) ->
    file:delete(testfuncs.erl),
    {ok,IOF} = file:open(testfuncs.erl,[append]),
    lists:foreach(fun(X) -> 
			  RN = filename:rootname(X),
			  TestDirPath = "filename:join([?config(data_dir,Config),"++xmerl_SUITE:testcase_dir(list_to_atom(RN)),
			  file:write(IOF,"'"++RN++"'(suite) -> [];\n'"++RN++
				     "'(Config) ->\n"++
				     "  ?line file:set_cwd(?config(data_dir,Config)),\n"++
				     "  ?line {A,_} = xmerl_scan:file("++
				     TestDirPath++",\""++RN++".xml\"])"++
				     ",[]),\n"++
				     "  ?line C = xmerl:export([A],xmerl_test)."++
				     "\n\n") end,Filenames),
    file:close(IOF),
    io:format("~ngenerated ~w testcases.erl.~n",[length(Filenames)]).

print_filenames() ->
    Filenames = get(filenames),
    io:format("~n,~w files generated.~n[",[length(Filenames)]),
    lists:foreach(fun(X) ->
			  io:format("~p,",[list_to_atom(X)]) end,
		  Filenames),
    io:format("]~n"),
    put(filenames,[]).


mk_filename("xerox-japanese",GenDir,Name) ->
    filename:join([GenDir,"japanese-"++Name]);
mk_filename(_,GenDir,Name) ->
    filename:join([GenDir,Name]).

file_output(Filename,Content) ->
    {ok,IOF}=file:open(Filename,[write]),
    C1 = xmlconf_prolog(),
    C2 = xmlconf_end(),
    file:write(IOF,C1 ++ "\n\n" ++ Content ++ "\n\n" ++ C2),
    file:close(IOF).
		       

find_start_tag(Suite,"<TESTCASES"++Rest,TAcc,TCAcc) ->
    {L,Rest2}=parse_until_end_TCs_skip_base(Rest,[]),
    find_start_tag(Suite,Rest2,TAcc,["<TESTCASES"++L|TCAcc]);
find_start_tag(Suite,"</TESTCASES>"++Rest,TAcc,[H|T]) ->
    find_start_tag(Suite,Rest,TAcc,T);
find_start_tag(Suite,"<TEST"++Rest,TAcc,TCAcc) ->
    Id = extract_ID(Rest),
    {L,Rest2} = parse_until_end_T(Suite,Rest,[]),
    {"<TEST" ++ L,Rest2,TCAcc,Id};
find_start_tag(Suite,[H|T],TAcc,TCAcc) ->
    find_start_tag(Suite,T,TAcc,TCAcc);
find_start_tag(_,[],TAcc,TCAcc) -> % no more tests
    {TAcc,[],TCAcc,[]}.

parse_until_end_T(Suite,"URI"++Rest,Acc) ->
    {Up2URI,Rest2}=parse_upto_URI_val(Suite,Rest,["URI"]),
    parse_until_end_T(Suite,Rest2,[Up2URI|Acc]);
% parse_until_end_T(Suite,"</TEST>"++Rest,Acc) ->
%     {lists:flatten(lists:reverse(["</TEST>"|Acc])),Rest};
parse_until_end_T(Suite,"OUTPUT"++Rest,Acc) ->
    {Up2OUTPUT,Rest2}=parse_upto_URI_val(Suite,Rest,["OUTPUT"]),
    parse_until_end_T(Suite,Rest2,[Up2OUTPUT|Acc]);
parse_until_end_T(_Suite,"</TEST>"++Rest,Acc) ->
    {lists:flatten(lists:reverse(["</TEST>"|Acc])),Rest};
parse_until_end_T(Suite,[H|T],Acc) ->
    parse_until_end_T(Suite,T,[H|Acc]);
parse_until_end_T(_,[],Acc) ->
    exit({error,{"unexpected end",lists:reverse(Acc)}}).

parse_upto_URI_val(Suite,[H|Rest],Acc) when H==$\t;H==$\n;H==$\s ->
    parse_upto_URI_val(Suite,Rest,[H|Acc]);
parse_upto_URI_val(Suite,[H|Rest],Acc) when H/="=" ->
    {lists:flatten(lists:reverse([H|Acc])),Rest};
parse_upto_URI_val(Suite,[H|Rest],Acc) ->
    parse_upto_URI_val2(Suite,Rest,[H|Acc]).

parse_upto_URI_val2(Suite,[H|Rest],Acc) when H==$"; H==$' ->
    SubDir = sub_dir(Suite),
    {lists:flatten(lists:reverse([$/,SubDir,H|Acc])),Rest};
parse_upto_URI_val2(Suite,[H|T],Acc) ->
    parse_upto_URI_val2(Suite,T,[H|Acc]).

parse_until_end_TCs_skip_base(">"++Rest,Acc) ->
    {lists:flatten(lists:reverse(["\n",">"|Acc])),Rest};
parse_until_end_TCs_skip_base("xml:base="++Rest,Acc) ->
    Rest2=skip_base_def(Rest),
    parse_until_end_TCs_skip_base(Rest2,Acc) ->
parse_until_end_TCs_skip_base([H|T],Acc) ->
    parse_until_end_TCs_skip_base(T,[H|Acc]).

skip_base_def([Del|R]) ->
    skip_base_def(R,Del).
skip_base_def([Del|R],Del) ->
    R;
skip_base_def([_H|R],Del) ->
    skip_base_def(R,Del).

% parse_end_T("</TEST>"++Rest,Acc) ->
%     {lists:flatten(lists:reverse(["</TEST>"|Acc])),Rest};
% parse_end_T([H|T],Acc) ->
%     parse_end_T(T,[H|Acc]).


xmlconf_prolog() ->
    FC = get_xmlconf(),
    parse_xmlconf_prolog(FC,[]).


%% 
extract_ID("ID"++Rest) ->
    extract_ID2(Rest);
extract_ID([H|T]) ->
    extract_ID(T).

extract_ID2([H|Rest]) when H==$";H==$' ->
    extract_IDval(Rest,[]);
extract_ID2([H|T]) -> %skip '=' and white space
    extract_ID2(T).

extract_IDval([H|Rest],Acc) when H==$";H==$' ->
    lists:flatten(lists:reverse(Acc));
extract_IDval([H|T],Acc) ->
    extract_IDval(T,[H|Acc]).

    
testsuite_end() ->
    "</TESTSUITE>".

testcases_end(0) ->
    "";
testcases_end(N) ->
    "</TESTCASES>" ++ "\n" ++testcases_end(N-1).

xmlconf_end() ->
    "</TESTSUITE>".


parse_xmlconf_prolog("href=\"xmlconformance.xsl\""++Rest,Acc) ->
    HRef = lists:reverse("href=\"../../xmlconformance.xsl\""),
    parse_xmlconf_prolog(Rest,HRef++Acc);
parse_xmlconf_prolog("<!DOCTYPE TESTSUITE SYSTEM \"testcases.dtd\""++Rest,Acc) -> 
    Rest2 = skip_entity_defs(Rest),
    DocType = lists:reverse("<!DOCTYPE TESTSUITE SYSTEM \"../../testcases.dtd\">"),
    parse_xmlconf_prolog(Rest2,DocType++Acc);
parse_xmlconf_prolog("<TESTSUITE"++Rest,Acc) ->
    Acc2 = parse_until_TESTSUITE_tag(Rest,[]),
    lists:flatten(lists:reverse([Acc2,"<TESTSUITE"|Acc]));
parse_xmlconf_prolog([H|T],Acc) ->
    parse_xmlconf_prolog(T,[H|Acc]);
parse_xmlconf_prolog([],Acc) ->
    exit({error,{"unexpected end",lists:reverse(Acc)}}).

parse_until_TESTSUITE_tag(">"++_Rest,Acc) ->
    lists:flatten(lists:reverse([">"|Acc]));
parse_until_TESTSUITE_tag([H|T],Acc) ->
    parse_until_TESTSUITE_tag(T,[H|Acc]);
parse_until_TESTSUITE_tag([],Acc) ->
    exit({error,{"unexpected end",lists:reverse(Acc)}}).

skip_entity_defs("]>"++Rest) ->
    Rest;
skip_entity_defs([H|T]) ->
    skip_entity_defs(T).
    
sub_dir("jclark-xmltest") ->
    "xmerl_SUITE_data/xmltest";
sub_dir("xerox-japanese") ->
    "xmerl_SUITE_data/japanese";
sub_dir("sun"++_Rest) ->
    "xmerl_SUITE_data/sun";
sub_dir("nist-oasis") ->
    "xmerl_SUITE_data/oasis";
sub_dir("ibm"++_Rest) ->
    "xmerl_SUITE_data/ibm".

extension("ibm"++_R) ->
    "";
extension(_) ->
    ".xml".

%*****************************
ticker() ->
    register(xmerl_ticker,self()),
    receive
	{suite,Name} ->
	    io:format("~nGenerating suite ~p",[Name])
    end,
    ticker_loop().

ticker_loop() ->
    receive
	{suite,Name} ->
	    io:format("~nGenerating suite ~p",[Name]),
	    ticker_loop();
	finished ->
	    ok
    after 400 ->
%	    io:format(".",[]),
	    ticker_loop()
    end.
