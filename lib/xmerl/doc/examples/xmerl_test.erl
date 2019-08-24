-module(xmerl_test).

-compile(export_all).
%%-export([Function/Arity, ...]).

-define(XMERL_APP,).

-include("xmerl.hrl").

%% Export to HTML from "simple" format
test1() ->
    xmerl:export_simple(simple(), xmerl_html, [{title, "Doc Title"}]).


%% Export to XML from "simple" format
test2() ->
    xmerl:export_simple(simple(), xmerl_xml, [{title, "Doc Title"}]).


%% Parse XHTML, and export result to HTML and text
test3() ->
    FetchFun = fun(_DTDSpec, S) -> {ok, not_fetched,S} end,
    {A, _} = xmerl_scan:string(html(),
			    [{fetch_fun,FetchFun}]),
    io:format("From xmerl_scan:string/2~n ~p~n", [A]),
    B = xmerl:export([A], xmerl_html),
    io:format("From xmerl:export/2 xmerl_html filter~n ~p~n", [B]),
    C = xmerl:export([A], xmerl_text),
    io:format("From xmerl:export/2 xmerl_text filter~n ~p~n", [C]).
    

test4() ->
    FetchFun = fun(_DTDSpec, S) -> {ok, not_fetched, S} end,
    {A,_} = xmerl_scan:string(xml_namespace(),
			    [{fetch_fun,FetchFun},
			     {namespace_conformant,true}]),
    io:format("From xmerl_scan:string/2~n ~p~n", [A]).

test5() ->
    {ok, Cwd} = file:get_cwd(), % Assume we are in the examples dir...
    File = Cwd ++ "/xml/xmerl.xml",
    FetchFun = fun(_DTDSpec, S) -> {ok, not_fetched, S} end,
%    {Resp0,Rest0}=xmerl_eventp:stream(File,[]),
%    io:format("Tree: ~p~n",[Resp0]),
    {Resp1, _Rest1}=xmerl_eventp:file_sax(File, ?MODULE, undefined,
					[{fetch_fun, FetchFun}]),
    io:format("Using file_sax: counted ~p  paragraphs~n", [Resp1]),
    {Resp2, _Rest2} = xmerl_eventp:stream_sax(File, ?MODULE, undefined, []),
    io:format("Using stream_sax: counted ~p paragraphs~n", [Resp2]).

test6() ->
    FetchFun = fun(_DTDSpec, S) -> {ok, {string,""}, S} end,
    {Doc, _} = xmerl_scan:string(xml_namespace(),
				 [{fetch_fun, FetchFun},
				  {namespace_conformant, true}]),
    E = xmerl_xpath:string("child::title[position()=1]", Doc),
    io:format("From xmerl_scan:string/2~n E=~p~n", [E]).


simple() ->
    [{document, 
      [{title, ["Doc Title"]},
       {author, ["Ulf Wiger"]},
       {section,[{heading, ["heading1"]},
		 {'P', ["This is a paragraph of text."]},
		 {section,[{heading, ["heading2"]},
			   {'P', ["This is another paragraph."]},
			   {table,[{border, ["1"]}, 
				   {heading,[{col, ["head1"]},
					     {col, ["head2"]}]},
				   {row, [{col, ["col11"]},
					  {col, ["col12"]}]},
				   {row, [{col, ["col21"]},
					  {col, ["col22"]}]}
				  ]}
			  ]}
		]}
      ]}
    ].


html() ->
    "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"DTD/xhtml1-strict.dtd\"><html>"
	"<head><title>Doc Title</title><author>Ulf Wiger</author></head>"
	"<h1>heading1</h1>"
	"<p>This is a paragraph of text.</p>"
	"<h2>heading2</h2>"
	"<p>This is another paragraph.</p>"
	"<table>"
	"<thead><tr><td>head1</td><td>head2</td></tr></thead>"
	"<tr><td>col11</td><td>col122</td></tr>"
	"<tr><td>col21</td><td>col122</td></tr>"
	"</table>"
	"</html>".

xml_namespace() ->
    "<?xml version=\"1.0\"?>"
	"<!-- initially, the default namespace is \"books\" -->"
	"<book xmlns='urn:loc.gov:books' xmlns:isbn='urn:ISBN:0-395-36341-6'>"
	"<title>Cheaper by the Dozen</title>"
	"<isbn:number>1568491379</isbn:number>"
	"<notes>"
	"<!-- make HTML the default namespace for some comments -->"
	"<p xmlns='urn:w3-org-ns:HTML'>"
	"This is a <i>funny</i> book!"
	"</p>"
	"</notes>"
	"</book>".


%%% ============================================================================
%%% Generic callbacks

%'#text#'(Text) ->
%    [].

'#root#'(Data, Attrs, [], _E) ->
    io:format("root... Data=~p Attrs=~p E=~p~n",[Data,Attrs,_E]),
    [].

'#element#'(Tag, Data, Attrs, _Parents, _E) ->
    io:format("Tag=~p~n Data=~p~n Attrs=~p~n Parents=~p~n E=~p~n",
	      [Tag, Data, Attrs, _Parents, _E]),
    [].

'#element#'(_Tag, _Data, _Attrs, CBstate) ->
%    io:format("Tag=~p~n Data=~p~n Attrs=~p~n CBstate=~p~n",
%	      [Tag, Data, Attrs, CBstate]),
    CBstate.

'#text#'(Text, CBstate) ->
    io:format("Text=~p~n CBstate=~p~n",
	      [Text, CBstate]),
    CBstate.


'#xml-inheritance#'() ->
    [xmerl_html].




%%% ============================================================================
%%% To run these tests you must first download the testsuite from www.w3c.org
%%% xmlconf.xml is the main test file that contains references to all the tests.
%%% Thus parse this, export result and execute tests in the call-back functions.
%%% Note:
%%% - xmerl assumes all characters are represented with a single integer.
w3cvalidate() ->
    Tests = filename:join(filename:dirname(filename:absname(code:which(xmerl))),
			  "../w3c/xmlconf/xmlconf.xml"),
    TestDir = filename:dirname(Tests),
    io:format("Looking for W3C tests at ~p~n", [Tests]),
    {ok, Bin} = file:read_file(Tests),

%    String = ucs:to_unicode(binary_to_list(Bin), 'utf-8'),
%    case xmerl_scan:string(String, [{xmlbase, TestDir}]) of
    case xmerl_scan:string(binary_to_list(Bin), [{xmlbase, TestDir}]) of
	{error, Reason} ->
	    io:format("ERROR xmerl:scan_file/2 Reason=~w~n", [Reason]);
	{A, _Res} ->
%     io:format("From xmerl:scan_file/2 ~n A=~p~n Res=~w~n", [A,Res]),
	    C = xmerl:export([A], xmerl_test),
	    io:format("From xmerl:export/2 xmerl_text filter~n ~p~n", [C])
    end.
    

'TESTSUITE'(_Data, Attrs, _Parents, _E) ->
    _Profile = find_attribute('PROFILE', Attrs),
%    io:format("testsuite Profile=~p~n", [Profile]),
    [].

'TESTCASES'(_Data, Attrs, _Parents, _E) ->
    Profile = find_attribute('PROFILE', Attrs),
    XMLbase = find_attribute('xml:base', Attrs),
    io:format("testsuite Profile=~p~n xml:base=~p~n", [Profile, XMLbase]),
    [].

%% More info on Canonical Forms can be found at:
%%  http://dev.w3.org/cvsweb/~checkout~/2001/XML-Test-Suite/xmlconf/sun/cxml.html?content-type=text/html;%20charset=iso-8859-1
'TEST'(Data, Attrs, _Parents, E) ->
%    io:format("test Attrs=~p~n Parents=~p~n E=~p~n",[Attrs, _Parents, E]),
    Id = find_attribute('ID', Attrs),
    io:format("Test: ~p ",[Id]),
    Entities = find_attribute('ENTITIES', Attrs), % Always handle all entities
    Output1 = find_attribute('OUTPUT', Attrs), % 
    Output3 = find_attribute('OUTPUT3', Attrs), % FIXME!
    Sections = find_attribute('SECTIONS', Attrs),
    Recommendation = find_attribute('RECOMMENDATION', Attrs), % FIXME!
    Type = find_attribute('TYPE', Attrs), % Always handle all entities
    Version = find_attribute('VERSION', Attrs), % FIXME!
    URI = find_attribute('URI', Attrs),
    Namespace = find_attribute('NAMESPACE', Attrs), % FIXME!

    OutputForm=
	if
	    Output1 =/= undefined -> Output1;
	    true -> Output3
	end,
    Test = filename:join(E#xmlElement.xmlbase, URI),
%    io:format("TEST URI=~p~n E=~p~n",[Test,E]),
    case Type of
	"valid" ->
%	    io:format("Data=~p~n Attrs=~p~n Parents=~p~n Path=~p~n",
%		      [Data, Attrs, _Parents, Test]),
	    test_valid(Test, Data, Sections, Entities, OutputForm, Recommendation,
		       Version, Namespace);
	"invalid" ->
	    test_invalid(Test, Data, Sections, Entities, OutputForm, Recommendation,
			 Version, Namespace);
	"not-wf" ->
	    test_notwf(Test, Data, Sections, Entities, OutputForm, Recommendation,
		       Version, Namespace);
	"error" ->
	    test_error(Test, Data, Sections, Entities, OutputForm, Recommendation,
		       Version, Namespace)
    end,
    [].

%% Really basic HTML font tweaks, to support highlighting
%% some aspects of test descriptions ...
'EM'(Data, _Attrs, _Parents, _E) ->
    [$" |Data ++ [$"]].

'B'(Data, _Attrs, _Parents, _E) ->
   [$" |Data ++ [$"]].



find_attribute(Tag,Attrs) ->
    case xmerl_lib:find_attribute(Tag, Attrs) of
	{value, Id} -> Id;
	false -> undefined
    end.


-define(CONT, false).

%%% All parsers must accept "valid" testcases.
test_valid(URI, Data, Sections, Entities, OutputForm, Recommendation, Version,
	   Namespace) ->
    io:format("nonvalidating ", []),
    case nonvalidating_parser_q(URI) of
	{Res, Tail} when is_record(Res, xmlElement) ->
	    case is_whitespace(Tail) of
		true ->
		    io:format("OK ", []),
		    ok;
		false ->
		    print_error({Res, Tail}, URI, Sections, Entities, OutputForm,
				Recommendation,
				Version, Namespace, Data),
		    if
			?CONT == false -> throw({'EXIT', failed_test});
			true -> error
		    end
	    end;
	Error ->
	    print_error(Error, URI, Sections, Entities, OutputForm, Recommendation,
			Version, Namespace, Data),
	    if
		?CONT == false -> throw({'EXIT', failed_test});
		true -> error
	    end
    end,
    io:format("validating ", []),
    case validating_parser_q(URI) of
	{Res2, Tail2} when is_record(Res2, xmlElement) ->
	    case is_whitespace(Tail2) of
		true ->
		    io:format("OK~n", []),
		    ok;
		false ->
		    print_error({Res2, Tail2}, URI, Sections, Entities, OutputForm,
				Recommendation,
				Version, Namespace, Data),
		    if
			?CONT == false -> throw({'EXIT', failed_test});
			true -> error
		    end
	    end;
	Error2 ->
	    print_error(Error2, URI, Sections, Entities, OutputForm, Recommendation,
			Version, Namespace, Data),
	    if
		?CONT == false -> throw({'EXIT', failed_test});
		true -> error
	    end
    end.


%%% Nonvalidating parsers must accept "invalid" testcases, but validating ones
%%% must reject them.
test_invalid(URI, Data, Sections, Entities, OutputForm, Recommendation, Version,
	     Namespace) ->
    io:format("nonvalidating ", []),
    case nonvalidating_parser_q(URI) of
	{Res,Tail} when is_record(Res, xmlElement) ->
	    case is_whitespace(Tail) of
		true ->
		    io:format("OK ", []),
		    ok;
		false ->
		    print_error({Res, Tail}, URI, Sections, Entities, OutputForm,
				Recommendation,
				Version, Namespace, Data),
		    if
			?CONT == false -> throw({'EXIT', failed_test});
			true -> error
		    end
	    end;
	Error ->
	    print_error(Error, URI, Sections, Entities, OutputForm, Recommendation,
			Version, Namespace, Data),
	    if
		?CONT == false -> throw({'EXIT', failed_test});
		true -> error
	    end
    end,
    io:format("validating ", []),
    case validating_parser_q(URI) of
	{Res2, Tail2} when is_record(Res2, xmlElement) ->
	    case is_whitespace(Tail2) of
		false ->
		    io:format("OK~n", []),
		    ok;
		true ->
		    print_error({Res2, Tail2}, URI, Sections, Entities, OutputForm,
				Recommendation,
				Version, Namespace, Data),
		    if
			?CONT == false -> throw({'EXIT', failed_test});
			true -> error
		    end
	    end;
	{error, enoent} ->
	    print_error("Testfile not found", URI, Sections, Entities, OutputForm,
			Recommendation, Version, Namespace, Data),
	    if
		?CONT == false -> throw({'EXIT', failed_test});
		true -> error
	    end;
	_Error2 ->
	    io:format("OK~n", []),
	    ok
    end.

%%% No parser should accept a "not-wf" testcase unless it's a nonvalidating
%%% parser and the test contains external entities that the parser doesn't read
test_notwf(URI, Data, Sections, Entities, OutputForm, Recommendation, Version,
	   Namespace) ->
    io:format("nonvalidating ", []),
    case nonvalidating_parser_q(URI) of
	{Res, Tail} when is_record(Res, xmlElement) ->
	    case is_whitespace(Tail) of
		false ->
		    io:format("OK ", []),
		    ok;
		true ->
		    print_error({Res, Tail}, URI, Sections, Entities, OutputForm,
				Recommendation,
				Version, Namespace, Data),
		    if
			?CONT == false -> throw({'EXIT', failed_test});
			true -> error
		    end
	    end;
	{error,enoent} ->
	    print_error("Testfile not found",URI,Sections,Entities,OutputForm,
			Recommendation,Version,Namespace,Data),
	    if
		?CONT==false -> throw({'EXIT', failed_test});
		true -> error
	    end;
	_Error ->
	    io:format("OK ",[]),
	    ok
    end,
    io:format("validating ",[]),
    case validating_parser_q(URI) of
	{Res2, Tail2} when is_record(Res2, xmlElement) ->
	    case is_whitespace(Tail2) of
		false ->
		    io:format("OK~n", []),
		    ok;
		true ->
		    print_error({Res2, Tail2}, URI, Sections, Entities, OutputForm,
				Recommendation,
				Version, Namespace, Data),
		    if
			?CONT == false -> throw({'EXIT', failed_test});
			true -> error
		    end
	    end;
	{error,enoent} ->
	    print_error("Testfile not found", URI, Sections, Entities, OutputForm,
			Recommendation, Version, Namespace, Data),
	    if
		?CONT == false -> throw({'EXIT', failed_test});
		true -> error
	    end;
	_Error2 ->
	    io:format("OK~n", []),
	    ok
    end.

%%% Parsers are not required to report "errors", but xmerl will always...
test_error(URI, Data, Sections, Entities, OutputForm, Recommendation, Version,
	   Namespace) ->
    io:format("nonvalidating ", []),
    case nonvalidating_parser_q(URI) of
	{'EXIT', _Reason} ->
	    io:format("OK ", []),
	    ok;
	{error, enoent} ->
	    print_error("Testfile not found", URI, Sections, Entities, OutputForm,
			Recommendation, Version, Namespace, Data),
	    if
		?CONT == false -> throw({'EXIT', failed_test});
		true -> error
	    end;
	Res ->
	    print_error(Res, URI, Sections, Entities, OutputForm, Recommendation,
			Version, Namespace, Data),
	    if
		?CONT == false -> throw({'EXIT', failed_test});
		true -> error
	    end
    end,
    io:format("validating ", []),
    case validating_parser_q(URI) of
	{'EXIT', _Reason2} ->
	    io:format("OK~n", []),
	    ok;
	{error, enoent} ->
	    print_error("Testfile not found", URI, Sections, Entities, OutputForm,
			Recommendation, Version, Namespace, Data),
	    if
		?CONT == false -> throw({'EXIT', failed_test});
		true -> error
	    end;
	Res2 ->
	    print_error(Res2, URI, Sections, Entities, OutputForm, Recommendation,
			Version, Namespace, Data),
	    if
		?CONT == false -> throw({'EXIT', failed_test});
		true -> error
	    end
    end.


%%% Use xmerl as nonvalidating XML parser
nonvalidating_parser(URI) ->
    (catch xmerl_scan:file(URI, [])).


%%% Use xmerl as nonvalidating XML parser
nonvalidating_parser_q(URI) ->
    (catch xmerl_scan:file(URI, [{quiet, true}])).


%%% Use xmerl as validating XML parser
validating_parser(URI) ->
    (catch xmerl_scan:file(URI, [{validation, true}])).


%%% Use xmerl as validating XML parser
validating_parser_q(URI) ->
    (catch xmerl_scan:file(URI, [{validation, true}, {quiet, true}])).


is_whitespace([]) ->
    true;
is_whitespace([H |Rest]) when ?whitespace(H) ->
    is_whitespace(Rest);
is_whitespace(_) ->
    false.


print_error(Error, URI, Sections, Entities, OutputForm, Recommendation, Version,
	    Namespace, Data) ->
    io:format("ERROR ~p~n URI=~p~n See Section ~s~n",[Error, URI, Sections]),
    if
	Entities == undefined -> ok;
	true -> io:format(" Entities  =~s~n",[Entities])
    end,
    if
	OutputForm == undefined -> ok;
	true -> io:format(" OutputForm=~s FIXME!~n",[OutputForm])
    end,
    if
	Recommendation == undefined -> ok;
	true -> io:format(" Recommendation=~s~n",[Recommendation])
    end,
    if
	Version == undefined -> ok;
	true -> io:format(" Version   =~s~n",[Version])
    end,
    if
	Namespace == undefined -> ok;
	true -> io:format(" Namespace =~s~n",[Namespace])
    end,
    io:format(Data).


    
    
    
	



%%% ============================================================================
%%% Callbacks for parsing of Simplified DocBook XML

para(_Data, _Attrs, US) ->
    case US of
	Int when is_integer(Int) -> Int+1;
	undefined -> 1
    end.


