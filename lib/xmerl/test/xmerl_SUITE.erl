%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2013. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%
%%
%%% Purpose : Test suite for the ASN.1 application

-module(xmerl_SUITE).

-compile(export_all).

%%----------------------------------------------------------------------
%% Include files
%%----------------------------------------------------------------------
-include_lib("test_server/include/test_server.hrl").
%%-include("xmerl.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-include_lib("kernel/include/file.hrl").


%%======================================================================
%% Tests
%%======================================================================

%%----------------------------------------------------------------------
%% Test groups
%%----------------------------------------------------------------------
all() -> 
    [{group, cpd_tests}, xpath_text1, xpath_main,
     xpath_abbreviated_syntax, xpath_functions, xpath_namespaces,
     {group, misc}, {group, eventp_tests},
     {group, ticket_tests}, {group, app_test},
     {group, appup_test}].

groups() -> 
    [{cpd_tests, [],
      [cpd_invalid1, cpd_invalid1_index, cpd_invalid2_index,
       cpd_invalid_index3, cpd_invalid_is_layer,
       cpd_expl_provided_DTD]},
     {misc, [],
      [latin1_alias, syntax_bug1, syntax_bug2, syntax_bug3,
       pe_ref1, copyright, testXSEIF, export_simple1, export]},
     {eventp_tests, [], [sax_parse_and_export]},
     {ticket_tests, [],
      [ticket_5998, ticket_7211, ticket_7214, ticket_7430,
       ticket_6873, ticket_7496, ticket_8156, ticket_8697,
       ticket_9411, ticket_9457, ticket_9664_schema, ticket_9664_dtd]},
     {app_test, [], [{xmerl_app_test, all}]},
     {appup_test, [], [{xmerl_appup_test, all}]}].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

%%----------------------------------------------------------------------
%% Initializations
%%----------------------------------------------------------------------
init_per_suite(doc) ->
    ["Starts the test suite"];
init_per_suite(Config) ->
    Dog=test_server:timetrap({minutes,10}),
    ?line file:set_cwd(?config(data_dir,Config)),
    ?line ok=erl_tar:extract("cpd.tar.gz",[compressed]),
    ?line ok=erl_tar:extract("misc.tar.gz",[compressed]),
    ?line ok = change_mode(["cpd", "misc"]),
    ?line file:set_cwd(filename:join(?config(data_dir,Config),xpath)),
    TestServerIncludeDir=filename:join(filename:dirname(code:priv_dir(test_server)), "include"),
    ?line {ok, xpath_lib} = compile:file(xpath_lib, [{i, TestServerIncludeDir}]),
    ?line {ok, xpath_text} = compile:file(xpath_text, [{i, TestServerIncludeDir}]),
    ?line {ok, xpath_abbrev} = compile:file(xpath_abbrev, [{i, TestServerIncludeDir}]),
    [{watchdog, Dog}|Config].


-ifndef(dont_rm_test_dirs).
end_per_suite(doc) ->
    ["Stops the test suite"];
end_per_suite(Config) ->
    ?line file:set_cwd(?config(data_dir,Config)),
    ?line ok=rm_files(["cpd", "misc"]),
    Dog=?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    lists:keydelete(watchdog,1,Config).

-else.
end_per_suite(doc) ->
    ["Stops the test suite"];
end_per_suite(Config) ->
    Dog=?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    lists:keydelete(watchdog,1,Config).
-endif.


%% initialization before each testcase
init_per_testcase(_TestCase,Config) ->
    io:format("Config:~n~p",[Config]),
    ?line {ok, _} = file:read_file_info(filename:join([?config(priv_dir,Config)])),
    ?line code:add_patha(?config(priv_dir,Config)),
    Dog=test_server:timetrap({minutes,10}),
    [{watchdog, Dog}|Config].


%% clean up after each testcase
end_per_testcase(_Func,Config) ->
    Dog=?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.


%%----------------------------------------------------------------------
%% Test cases
%%----------------------------------------------------------------------
cpd_invalid1(suite) -> [];
cpd_invalid1(Config) ->
    ?line file:set_cwd(?config(data_dir,Config)),
    ?line case catch xmerl_scan:file(filename:join([?config(data_dir,Config), cpd,"cpd_test.xml"]),[]) of
	      {'EXIT',{fatal,Reason}} ->
		  case Reason of
		      {expected_markup,_Path,28,32} -> ok;
		      _ -> {comment,"parsing changed behaviour"}
		  end
	  end.

cpd_invalid1_index(suite) -> [];
cpd_invalid1_index(Config) ->
    ?line file:set_cwd(?config(data_dir,Config)),
    ?line case catch xmerl_scan:file(filename:join([?config(data_dir,Config), cpd,"cpd_index.xml"]),[]) of
	      {'EXIT',{fatal,Reason}} ->
		  case Reason of
		      {{error,{whitespace_was_expected}},_Path,1,19} -> ok;
		      _ -> {comment,"parsing changed behaviour"}
		  end
	  end.

cpd_invalid2_index(suite) -> [];
cpd_invalid2_index(Config) ->
    ?line file:set_cwd(?config(data_dir,Config)),
    ?line case catch xmerl_scan:file(filename:join([?config(data_dir,Config), cpd,"cpd_index2.xml"]),[]) of
	      {'EXIT',{fatal,Reason}} ->
		  case Reason of
		      {{invalid_target_name,_Ver},_Path,2,3} ->ok;
		      _ -> {comment,"parsing changed behaviour"}
		  end
	  end.

cpd_invalid_index3(suite) -> [];
cpd_invalid_index3(Config) ->
    ?line file:set_cwd(?config(data_dir,Config)),
    ?line case catch xmerl_scan:file(filename:join([?config(data_dir,Config), cpd,"cpd_index3.xml"]),[]) of
	      {'EXIT',{fatal,Reason}} ->
		  case Reason of
		      {expected_markup,_Path,1,2} -> ok;
		      _ -> {comment,"parsing changed behaviour"}
		  end
	  end.

cpd_invalid_is_layer(suite) -> [];
cpd_invalid_is_layer(Config) ->
    ?line file:set_cwd(?config(data_dir,Config)),
    ?line case catch xmerl_scan:file(filename:join([?config(data_dir,Config), cpd,"is_layer2.xml"]),[]) of
	      {'EXIT',{fatal,_Reason}} -> ok
	  end.

cpd_expl_provided_DTD(suite) -> [];
cpd_expl_provided_DTD(Config) ->
    ?line file:set_cwd(?config(data_dir,Config)),
    ?line {#xmlElement{},[]} = xmerl_scan:file(filename:join([?config(data_dir,Config), cpd,"file_wo_DTD.xml"]),[{validation,true},{doctype_DTD,"separate_DTD.dtd"}]).

%%----------------------------------------------------------------------

xpath_text1(suite) -> [];
xpath_text1(Config) ->
    ?line file:set_cwd(filename:join(?config(data_dir,Config),xpath)),
    ?line ok = xpath_text:one().

xpath_main(suite) -> [];
xpath_main(Config) ->
    ?line file:set_cwd(filename:join(?config(data_dir,Config),xpath)),
    ?line ok = xpath_lib:test().

xpath_abbreviated_syntax(suite) -> [];
xpath_abbreviated_syntax(Config) ->
    ?line file:set_cwd(filename:join(?config(data_dir,Config),xpath)),
    ?line ok = xpath_abbrev:test().

xpath_functions(suite) -> [];
xpath_functions(Config) ->
    ?line file:set_cwd(filename:join(?config(data_dir,Config),xpath)),
    ?line ok = xpath_abbrev:functions().

xpath_namespaces(suite) -> [];
xpath_namespaces(Config) ->
    ?line file:set_cwd(filename:join(?config(data_dir,Config),xpath)),
    ?line ok = xpath_abbrev:namespaces().

%%----------------------------------------------------------------------

latin1_alias(suite) -> [];
latin1_alias(Config) ->
%    ?line file:set_cwd(filename:join(?config(data_dir,Config),misc)),
    ?line file:set_cwd(?config(data_dir,Config)),
    ?line {_Elements,[]}=
	xmerl_scan:file(filename:join([?config(data_dir,Config),
				       misc,"motorcycles.xml"]),
			[{validation,true},
			 {encoding,'iso-8859-1'}]).

syntax_bug1(suite) -> [];
syntax_bug1(Config) ->
    ?line file:set_cwd(?config(data_dir,Config)),
    ?line {fatal,{"expected one of: ?>, standalone, encoding",
		  {file,'misc/syntax_bug1.xml'},{line,1},{col,21}}} =
	case catch xmerl_scan:file('misc/syntax_bug1.xml') of
	    {'EXIT',Reason} ->
		Reason;
	    Err -> Err
	end.

syntax_bug2(suite) -> [];
syntax_bug2(Config) ->
    ?line file:set_cwd(?config(data_dir,Config)),
    ?line {fatal,{"expected one of: ?>, whitespace_character",
		  {file,'misc/syntax_bug2.xml'},{line,1},{col,20}}} =
	case catch xmerl_scan:file('misc/syntax_bug2.xml') of
	    {'EXIT',Reason} ->
		Reason;
	    Err -> Err
	end.

syntax_bug3(suite) -> [];
syntax_bug3(Config) ->
    ?line file:set_cwd(?config(data_dir,Config)),
    ?line {fatal,{{endtag_does_not_match,{was,obj,should_have_been,int}},
			  {file,'misc/syntax_bug3.xml'},{line,4},{col,3}}} =
	case catch xmerl_scan:file('misc/syntax_bug3.xml') of
	    {'EXIT',Reason} ->
		Reason;
	    Err -> Err
	end.

pe_ref1(suite) -> [];
pe_ref1(Config) ->
    ?line file:set_cwd(?config(data_dir,Config)),
    ?line {#xmlElement{},[]} = xmerl_scan:file(filename:join([?config(data_dir,Config), misc,"PE_ref1.xml"]),[{validation,true}]).

copyright(suite) -> [];
copyright(Config) ->
    ?line file:set_cwd(?config(data_dir,Config)),
    ?line {#xmlElement{},[]} = xmerl_scan:file(filename:join([?config(data_dir,Config), misc,"cprght.xml"]),[{validation,true}]).

testXSEIF(suite) -> [];
testXSEIF(Config) ->
    ?line file:set_cwd(?config(data_dir,Config)),
    ?line {#xmlElement{},[]} = xmerl_scan:file(filename:join([?config(data_dir,Config), misc,"ReplBoard_1_1543-CNA11313Uen.xml"]),[{validation,true}]).

export_simple1(suite) -> [];
export_simple1(Config) ->
    Simple = simple(),
    Res = xmerl:export_simple(Simple,xmerl_xml,[{title, "Doc Title"}]),
    ?line "<?xml version="++_ = lists:flatten(Res),

    %% Use of fun in simple content OTP-6679
    Simple2 = simple2(),
    Res2 = xmerl:export_simple(Simple2,xmerl_xml,[{title,"Doc Title"}]),
    ?line true = (Res2 =:= Res),
    ok.

export(suite) -> [];
export(Config) ->
    DataDir = ?config(data_dir,Config),
    Prolog = ["<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n<!DOCTYPE motorcycles SYSTEM \"motorcycles.dtd\">\n"],
    TestFile = filename:join([DataDir,"misc","motorcycles.xml"]),
    ?line {E,_} = xmerl_scan:file(TestFile),
    ?line Exported = xmerl:export([E],xmerl_xml,[{prolog,Prolog}]),
    B = list_to_binary(Exported++"\n"),
    ?line {ok, B} = file:read_file(TestFile),
    ok.

%%----------------------------------------------------------------------

sax_parse_and_export(suite) -> [];
sax_parse_and_export(Config) ->
    ?line ok = sax_parse_export_xml_big(Config),
    ?line ok = sax_parse_export_xml_small(Config).

%%----------------------------------------------------------------------

sax_parse_export_xml_big(Config) ->
    DataDir = ?config(data_dir,Config),
    OutDir = ?config(priv_dir,Config),
    io:format("DataDir: ~p~n,OutDir:~p~n",[DataDir,OutDir]),
    CMOMxml = filename:join([DataDir,"eventp","CMOM.xml"]),
    ?line {Ex,[]} = xmerl_eventp:file_sax(CMOMxml, xmerl_xml,[],[]),
    OutFile = filename:join([OutDir,"cmom"]),
    file:delete(OutFile),
    StubFile = filename:join([DataDir,"eventp","CelloMOM.stub"]),
    ?line {ok,Bin} = file:read_file(StubFile),
    ?line {ok,IO} = file:open(OutFile,[write,append]),
    ?line ok = file:write(IO,Bin),
    ?line ok = io:format(IO,"~s~n~n",[lists:flatten(Ex)]),
    Cmd = lists:flatten(io_lib:format("cmp ~s ~s",[OutFile,CMOMxml])),
    ?line [] = os:cmd(Cmd),
    ok.

sax_parse_export_xml_small(Config) ->
    DataDir = ?config(data_dir,Config),
    OutDir = ?config(priv_dir,Config),
    Wurfl_xml = filename:join([DataDir,"eventp","wurfl.xml"]),
    ?line {Ex,[]} = xmerl_eventp:file_sax(Wurfl_xml, xmerl_xml,[],[]),
    OutFile = filename:join([OutDir,"wrfl"]),
    file:delete(OutFile),
    StubFile = filename:join([DataDir,"eventp","wurfl.stub"]),
    ?line {ok,Bin} = file:read_file(StubFile),
    ?line {ok,IO} = file:open(OutFile,[write,append]),
    ?line ok = file:write(IO,Bin),
    ?line ok = io:format(IO,"~s~n",[lists:flatten(Ex)]),
    Cmd = lists:flatten(io_lib:format("cmp ~s ~s",[OutFile,Wurfl_xml])),
    ?line [] = os:cmd(Cmd),
    ok.


simple() ->
    [{document, 
      [{title, "Doc Title"}, {author, "Ulf Wiger"}],
      [{section, 
	[{heading, "heading1"}],
	[{'P', ["This is a paragraph of text."]},
	 {section, 
	  [{heading, "heading2"}],
	  [{'P', ["This is another paragraph."]},
	   {table, 
	    [{border, 1}],
	    [{heading,
	      [{col, ["head1"]},
	       {col, ["head2"]}]},
	     {row,
	      [{col, ["col11"]},
	       {col, ["col12"]}]},
	     {row,
	      [{col, ["col21"]},
	       {col, ["col22"]}]}]}]}]}]}].


simple2() ->
    GenC = fun ?MODULE:generate_section/1,
    GenA = fun ?MODULE:generate_attr_title/1,
    [{document,[{GenA,2}],[{GenC,1}]}].

generate_attr_title(0) ->
    done;
generate_attr_title(1) ->
    {{title,"Doc Title"},0};
generate_attr_title(2) ->
    {{author, "Ulf Wiger"},1}.
generate_section(0) ->
    done;
generate_section(N) ->
    GenC = fun ?MODULE:generate_section_content/1,
    GenA = fun ?MODULE:generate_section_attribute/1,
    {{section,[{GenA,1}],[{GenC,2}]},N-1}.

generate_section_attribute(0) ->
    done;
generate_section_attribute(N) ->
    {{heading, "heading1"},N-1}.

    
generate_subsection_content(0) ->
    done;
generate_subsection_content(1) ->
    {{'P',["This is another paragraph."]},0};
generate_subsection_content(N) ->
    {{table,[{fun ?MODULE:generate_border_attribute/1,1}],
      [{fun ?MODULE:generate_table_content/1,2}]},N-1}.
generate_section_content(0) ->
    done;
generate_section_content(1) ->
    {{'P',["This is a paragraph of text."]},0};
generate_section_content(N) ->
    {{section,[{heading,"heading2"}],
      [{fun ?MODULE:generate_subsection_content/1,2}]},N-1}.
generate_border_attribute(0) ->
    done;
generate_border_attribute(N) ->
    {{border,N},N-1}.
generate_table_content(0) ->
    done;
generate_table_content(1) ->
    {{fun ?MODULE:generate_heading/1,1},0};
generate_table_content(N) ->
    {{fun ?MODULE:generate_row/1, {2,2}},N-1}.
generate_row({0,_}) ->
    done;
generate_row(N) ->
    UpdateS = fun({2,_}) -> {1,2};(_) -> {0,0} end,
    {{row,[{fun ?MODULE:generate_row_col/1, N}]},UpdateS(N)}.
generate_row_col({_,0}) ->
    done;
generate_row_col(N={C,R}) ->
    UpdateS = fun({X,Y}) -> {X,Y-1} end,
    {{col,[lists:concat(["col",C,R])]},UpdateS(N)}.
generate_heading(0) ->
    done;
generate_heading(N) ->
    {{heading,[{fun ?MODULE:generate_heading_col/1,2}]},N-1}.
generate_heading_col(0) ->
    done;
generate_heading_col(N) ->
    {{col,[lists:concat(["head",N])]},N-1}.

%%----------------------------------------------------------------------
%% Ticket Tests
%%----------------------------------------------------------------------

%%
%% ticket_5998
%%
%% A Kleene Closure child in a sequence consumed all following
%% childs. This problem has been fixed.
%%
ticket_5998(suite) -> [];
ticket_5998(Config) ->
    DataDir = ?config(data_dir,Config),
    %% First fix is tested by case syntax_bug2.
    
    ?line case catch xmerl_scan:file(filename:join([DataDir,misc,
						    "ticket_5998_2.xml"])) of
	      {'EXIT',{fatal,Reason1}} ->
		  case Reason1 of
		      {{endtag_does_not_match,
			{was,obj,should_have_been,int}},
		       _,_,_} -> ok;
		      _ -> {comment,"parsing changed behaviour"}
		  end
	  end,

    ?line case catch xmerl_scan:file(filename:join([DataDir,misc,
						    "ticket_5998_3.xml"])) of
	      {'EXIT',{fatal,Reason2}} ->
		  case Reason2 of
		      {"expected one of: ?>, standalone, encoding",
		       _,_,_} -> ok;
		      _ -> {comment,"parsing changed behaviour"}
		  end
	  end.    


%%
%% ticket_7211
%%
%% A Kleene Closure child in a sequence consumed all following
%% childs. This problem has been fixed.
%%
ticket_7211(suite) -> [];
ticket_7211(Config) ->
    DataDir = ?config(data_dir,Config),
    ?line {E,[]} = 
	xmerl_scan:file(filename:join([DataDir,misc,"notes2.xml"]),
			[{fetch_path,[filename:join([DataDir,misc,erlang_docs_dtd])]},
			 {validation,dtd}]),
    
    ?line ok = case E of
		   Rec when is_record(Rec,xmlElement) ->
		       ok;
		   _ ->
		       E
	       end,
		       
    ?line {E2,[]} = 
	xmerl_scan:file(filename:join([DataDir,misc,"XS.xml"]),
			[{fetch_path,[filename:join([DataDir,misc,erlang_docs_dtd])]},
			 {validation,dtd}]),
    
    ?line ok = case E2 of
		   Rec2 when is_record(Rec2,xmlElement) ->
		       ok;
		   _ ->
		       E2
	       end.

%%
%% ticket_7214
%%
%% Now validating xhtml1-transitional.dtd.
%% A certain contentspec with a succeding choice, that didn't match
%% all content, followed by other child elements caused a
%% failure. This is now corrected.
%%
ticket_7214(suite) -> [];
ticket_7214(Config) ->
    DataDir = ?config(data_dir,Config),

    ?line {E,[]} = 
	xmerl_scan:file(filename:join([DataDir,misc,'block_tags.html']),
			[{validation,dtd},
			 {fetch_path,[filename:join([DataDir,misc,erlang_docs_dtd])]}]),
    
    ?line ok = case E of
		   Rec when is_record(Rec,xmlElement) ->
		       ok;
		   _ ->
		       E
	       end.

%%
%% ticket_7430
%%
%% Problem with contents of numeric character references followed by 
%% UTF-8 characters..
%%
ticket_7430(suite) -> [];
ticket_7430(Config) ->
    DataDir = ?config(data_dir,Config),

    ?line {E,[]} = 
	xmerl_scan:string("<a>\303\251&#xD;\303\251</a>",
			  [{encoding, 'utf-8'}]),

    ?line ok = case E of
		   {xmlElement,a,a,[],
		    {xmlNamespace,[],[]},
		    [],1,[],
		    [{xmlText,[{a,1}],1,[],"é",text},
		     {xmlText,[{a,1}],2,[],"\né",text}],
		    [],_,undeclared} ->
		       ok;
		   _ ->
		       E
	       end.

ticket_6873(suite) -> [];
ticket_6873(Config) ->
    ?line file:set_cwd(filename:join(?config(data_dir,Config),xpath)),    
    ?line ok = xpath_abbrev:ticket_6873(),
    ?line ok = xpath_lib:ticket_6873().

ticket_7496(suite) -> [];
ticket_7496(Config) ->
    ?line file:set_cwd(filename:join(?config(data_dir,Config),xpath)),
    ?line ok = xpath_abbrev:ticket_7496().

ticket_8156(suite) -> [];
ticket_8156(Config) ->
    ?line {ftp,{[],[]},"testmachine1",21,"/w.erl"} = xmerl_uri:parse("ftp://testmachine1/w.erl"),
    ?line {ftp,{"user",[]},"testmachine1",21,"/w.erl"} = xmerl_uri:parse("ftp://user@testmachine1/w.erl"),
    ?line {ftp,{"user","hello"},"testmachine1",21,"/w.erl"} = 
	xmerl_uri:parse("ftp://user:hello@testmachine1/w.erl"),
    ?line {ftp,{[],[]},"testmachine1",3000,"/w.erl"} = xmerl_uri:parse("ftp://testmachine1:3000/w.erl"),
    ?line {ftp,{"user","hello"},"testmachine1",3000,"/w.erl"} = 
	xmerl_uri:parse("ftp://user:hello@testmachine1:3000/w.erl"),
    ok.

ticket_8697(suite) -> [];
ticket_8697(doc) -> 
    ["Test that xmerl_scan can decode unicode entities properly"];
ticket_8697(Config) ->
    ?line {UTF8Output, []} = xmerl_scan:string("<?xml version=\"1\" ?>\n<text>" ++ [229, 145, 156] ++ "</text>"),
    ?line #xmlElement{content = [#xmlText{value = UTF8Text}]} = UTF8Output,
    ?line [16#545C] = UTF8Text,
    ?line {DecEntityOutput, []} = xmerl_scan:string("<?xml version=\"1\" ?>\n<text>&#21596;</text>"),
    ?line #xmlElement{content = [#xmlText{value = DecEntityText}]} = DecEntityOutput,
    ?line [21596] = DecEntityText,
    ?line {HexEntityOutput, []} = xmerl_scan:string("<?xml version=\"1\" ?>\n<text>&#x545C;</text>"),
    ?line #xmlElement{content = [#xmlText{value = HexEntityText}]} = HexEntityOutput,
    ?line [16#545C] = HexEntityText,
    ok.

ticket_9411(suite) -> [];
ticket_9411(doc) -> 
    ["Test that xmerl_scan handles attribute that contains for example &quot"];
ticket_9411(Config) ->
    DataDir = ?config(data_dir,Config),

    ?line {ok, Schema} = xmerl_xsd:process_schema(filename:join([DataDir,"misc/ticket_9411.xsd"])),
    ?line {ok, Bin} = file:read_file(filename:join([DataDir,"misc/ticket_9411.xml"])),
    ?line Xml = erlang:binary_to_list(Bin),
    ?line {E, _} = xmerl_scan:string(Xml),
    ?line {E, _} = xmerl_xsd:validate(E, Schema).

ticket_9457(suite) -> [];
ticket_9457(doc) -> 
    ["Test that xmerl_scan handles continuation correct when current input runs out at the end of an attribute value"];
ticket_9457(Config) ->
    Opts = [{continuation_fun, fun ticket_9457_cont/3, start}, {space, normalize}],
    ?line {E, _} = xmerl_scan:string([], Opts).

ticket_9457_cont(Continue, Exception, GlobalState) ->
    case xmerl_scan:cont_state(GlobalState) of
	start ->
	    G1 = xmerl_scan:cont_state(next, GlobalState),
	    Bytes = "<?xml version=\"1.0\" ?>\r\n<item a=\"b\"",
	    Continue(Bytes, G1);
	next ->
	    G1 = xmerl_scan:cont_state(last, GlobalState),
	    Bytes = ">blah</item>\r\n",
	    Continue(Bytes, G1);
	_ ->
	    Exception(GlobalState)
    end.


ticket_9664_schema(suite) -> [];
ticket_9664_schema(doc) -> 
    ["Test that comments are handled correct whith"];
ticket_9664_schema(Config) ->

    ?line {E, _} = xmerl_scan:file(filename:join([?config(data_dir, Config), misc,
						  "ticket_9664_schema.xml"]),[]),
    ?line {ok, S} = xmerl_xsd:process_schema(filename:join([?config(data_dir, Config), misc,
							    "motorcycles.xsd"])),
    ?line {E1, _} = xmerl_xsd:validate(E, S),

    ?line {E1,_} = xmerl_xsd:process_validate(filename:join([?config(data_dir,Config), misc,
							    "motorcycles.xsd"]),E,[]),

    ?line {E1,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),  misc,
						 "ticket_9664_schema.xml"]), 
				  [{schemaLocation, [{"mc", "motorcycles.xsd"}]},
				   {validation, schema}]),
    ok.

ticket_9664_dtd(suite) -> [];
ticket_9664_dtd(doc) -> 
    ["Test that comments are handled correct whith"];
ticket_9664_dtd(Config) ->
    ?line {E, _} = xmerl_scan:file(filename:join([?config(data_dir, Config),  misc,
						  "ticket_9664_dtd.xml"]),[]),
    ?line {E, _} = xmerl_scan:file(filename:join([?config(data_dir, Config),  misc,
						  "ticket_9664_dtd.xml"]),[{validation, true}]),
    ok.


%%======================================================================
%% Support Functions
%%======================================================================

%% Dir is a directory
rm_f_(Dir) ->
    ?line {ok,CWD} = file:get_cwd(),
    ?line {ok,FileList} = file:list_dir(Dir),
    ?line file:set_cwd(filename:join([CWD,Dir])),
    rm_files(FileList),
    ?line file:set_cwd(CWD),
    ? line ok = file:del_dir(Dir).

rm_files([])->
    ok;
rm_files([F|Fs]) ->
    case filelib:is_dir(F) of
	true ->
	    rm_f_(F);
	_ ->
	    ?line ok = file:delete(F)
    end,
    rm_files(Fs).

change_mode(Files) ->
    change_mode3(Files).
change_mode2(Dir)->
    ?line {ok,CWD} = file:get_cwd(),
    ?line {ok,FileList} = file:list_dir(Dir),
    ?line file:set_cwd(filename:join([CWD,Dir])),
    change_mode3(FileList),
    ?line file:set_cwd(CWD).
change_mode3([]) ->
    ok;
change_mode3([F|Fs]) ->
    case filelib:is_dir(F) of
	true ->
	    chmod(F),
	    change_mode2(F);
	_ ->
	    chmod(F)
    end,
    change_mode3(Fs).
    
chmod(F) ->
    case file:read_file_info(F) of
	{ok,FileInfo} ->
	    Mode= FileInfo#file_info.mode,
	    file:write_file_info(F,FileInfo#file_info{mode=8#00777 bor Mode});
	_ ->
	    ok
    end.
    
