%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
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
-include_lib("common_test/include/ct.hrl").
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
       pe_ref1, copyright, testXSEIF, export_simple1, export,
       default_attrs_bug, xml_ns, scan_splits_string_bug]},
     {eventp_tests, [], [sax_parse_and_export]},
     {ticket_tests, [],
      [ticket_5998, ticket_7211, ticket_7214, ticket_7430,
       ticket_6873, ticket_7496, ticket_8156, ticket_8697,
       ticket_9411, ticket_9457, ticket_9664_schema, ticket_9664_dtd]},
     {app_test, [], [{xmerl_app_test, all}]},
     {appup_test, [], [{xmerl_appup_test, all}]}].

suite() ->
    [{timetrap,{minutes,10}}].

%%----------------------------------------------------------------------
%% Initializations
%%----------------------------------------------------------------------

init_per_suite(Config) ->
    file:set_cwd(datadir(Config)),
    ok=erl_tar:extract("cpd.tar.gz",[compressed]),
    ok=erl_tar:extract("misc.tar.gz",[compressed]),
    ok = change_mode(["cpd", "misc"]),
    file:set_cwd(filename:join(datadir(Config),xpath)),
    TestServerIncludeDir=filename:join(filename:dirname(code:priv_dir(test_server)), "include"),
    {ok, xpath_lib} = compile:file(xpath_lib, [{i, TestServerIncludeDir}]),
    {ok, xpath_text} = compile:file(xpath_text, [{i, TestServerIncludeDir}]),
    {ok, xpath_abbrev} = compile:file(xpath_abbrev, [{i, TestServerIncludeDir}]),
    Config.


-ifndef(dont_rm_test_dirs).
end_per_suite(Config) ->
    file:set_cwd(datadir(Config)),
    ok = rm_files(["cpd", "misc"]),
    ok.

-else.
end_per_suite(_Config) ->
    ok.
-endif.


%% initialization before each testcase
init_per_testcase(_TestCase,Config) ->
    io:format("Config:~n~p",[Config]),
    {ok, _} = file:read_file_info(filename:join([privdir(Config)])),
    code:add_patha(privdir(Config)),
    Config.


%% clean up after each testcase
end_per_testcase(_Func,_Config) ->
    ok.


%%----------------------------------------------------------------------
%% Test cases
%%----------------------------------------------------------------------
cpd_invalid1(Config) ->
    file:set_cwd(datadir(Config)),
    case catch xmerl_scan:file(datadir_join(Config,[cpd,"cpd_test.xml"]),[]) of
        {'EXIT',{fatal,Reason}} ->
            case Reason of
                {expected_markup,_Path,28,32} -> ok;
                _ -> {comment,"parsing changed behaviour"}
            end
    end.

cpd_invalid1_index(Config) ->
    file:set_cwd(datadir(Config)),
    case catch xmerl_scan:file(datadir_join(Config,[cpd,"cpd_index.xml"]),[]) of
        {'EXIT',{fatal,Reason}} ->
            case Reason of
                {{error,{whitespace_was_expected}},_Path,1,19} -> ok;
                _ -> {comment,"parsing changed behaviour"}
            end
    end.

cpd_invalid2_index(Config) ->
    file:set_cwd(datadir(Config)),
    case catch xmerl_scan:file(datadir_join(Config,[cpd,"cpd_index2.xml"]),[]) of
        {'EXIT',{fatal,Reason}} ->
            case Reason of
                {{invalid_target_name,_Ver},_Path,2,3} ->ok;
                _ -> {comment,"parsing changed behaviour"}
            end
    end.

cpd_invalid_index3(Config) ->
    file:set_cwd(datadir(Config)),
    case catch xmerl_scan:file(datadir_join(Config,[cpd,"cpd_index3.xml"]),[]) of
        {'EXIT',{fatal,Reason}} ->
            case Reason of
                {expected_markup,_Path,1,2} -> ok;
                _ -> {comment,"parsing changed behaviour"}
            end
    end.

cpd_invalid_is_layer(Config) ->
    file:set_cwd(datadir(Config)),
    case catch xmerl_scan:file(datadir_join(Config,[cpd,"is_layer2.xml"]),[]) of
        {'EXIT',{fatal,_Reason}} -> ok
    end.

cpd_expl_provided_DTD(Config) ->
    file:set_cwd(datadir(Config)),
    {#xmlElement{},[]} = xmerl_scan:file(datadir_join(Config,[cpd,"file_wo_DTD.xml"]),
                                         [{validation,true},{doctype_DTD,"separate_DTD.dtd"}]).

%%----------------------------------------------------------------------

xpath_text1(Config) ->
    file:set_cwd(filename:join(datadir(Config),xpath)),
    ok = xpath_text:one().

xpath_main(Config) ->
    file:set_cwd(filename:join(datadir(Config),xpath)),
    ok = xpath_lib:test().

xpath_abbreviated_syntax(Config) ->
    file:set_cwd(filename:join(datadir(Config),xpath)),
    ok = xpath_abbrev:test().

xpath_functions(Config) ->
    file:set_cwd(filename:join(datadir(Config),xpath)),
    ok = xpath_abbrev:functions().

xpath_namespaces(Config) ->
    file:set_cwd(filename:join(datadir(Config),xpath)),
    ok = xpath_abbrev:namespaces().

%%----------------------------------------------------------------------

latin1_alias(Config) ->
%    file:set_cwd(filename:join(datadir(Config),misc)),
    file:set_cwd(datadir(Config)),
    {_Elements,[]} = xmerl_scan:file(datadir_join(Config,[misc,"motorcycles.xml"]),
                                     [{validation,true},
                                      {encoding,'iso-8859-1'}]).

syntax_bug1(Config) ->
    file:set_cwd(datadir(Config)),
    {fatal,{"expected one of: ?>, standalone, encoding",
            {file,'misc/syntax_bug1.xml'},{line,1},{col,21}}
    } = case catch xmerl_scan:file('misc/syntax_bug1.xml') of
            {'EXIT',Reason} ->
                Reason;
            Err -> Err
        end.

syntax_bug2(Config) ->
    file:set_cwd(datadir(Config)),
    {fatal,{"expected one of: ?>, whitespace_character",
            {file,'misc/syntax_bug2.xml'},{line,1},{col,20}}
    } = case catch xmerl_scan:file('misc/syntax_bug2.xml') of
            {'EXIT',Reason} ->
                Reason;
            Err -> Err
        end.

syntax_bug3(Config) ->
    file:set_cwd(datadir(Config)),
    {fatal,{{endtag_does_not_match,{was,obj,should_have_been,int}},
            {file,'misc/syntax_bug3.xml'},{line,4},{col,3}}
    } = case catch xmerl_scan:file('misc/syntax_bug3.xml') of
            {'EXIT',Reason} ->
                Reason;
            Err -> Err
        end.

default_attrs_bug(Config) ->
    file:set_cwd(datadir(Config)),
    Doc = "<!DOCTYPE doc [<!ATTLIST doc b CDATA \"default\">]>\n"
          "<doc a=\"explicit\"/>",
    {#xmlElement{attributes = [#xmlAttribute{name = a, value = "explicit"},
                               #xmlAttribute{name = b, value = "default"}]},
     []
    } = xmerl_scan:string(Doc, [{default_attrs, true}]),
    Doc2 = "<!DOCTYPE doc [<!ATTLIST doc b CDATA \"default\">]>\n"
           "<doc b=\"also explicit\" a=\"explicit\"/>",
    {#xmlElement{attributes = [#xmlAttribute{name = b, value = "also explicit"},
                               #xmlAttribute{name = a, value = "explicit"}]},
     []
    } = xmerl_scan:string(Doc2, [{default_attrs, true}]),
    ok.


xml_ns(Config) ->
    Doc = "<?xml version='1.0'?>\n"
        "<doc xml:attr1=\"implicit xml ns\"/>",
    {#xmlElement{namespace=#xmlNamespace{default = [], nodes = []},
                 attributes = [#xmlAttribute{name = 'xml:attr1',
                                             expanded_name = {'http://www.w3.org/XML/1998/namespace',attr1},
                                             nsinfo = {"xml","attr1"},
                                             namespace = #xmlNamespace{default = [], nodes = []}}]},
     []
    } = xmerl_scan:string(Doc, [{namespace_conformant, true}]),
    Doc2 = "<?xml version='1.0'?>\n"
        "<doc xmlns:xml=\"http://www.w3.org/XML/1998/namespace\" xml:attr1=\"explicit xml ns\"/>",
    {#xmlElement{namespace=#xmlNamespace{default = [], nodes = [{"xml",'http://www.w3.org/XML/1998/namespace'}]},
                 attributes = [#xmlAttribute{name = 'xmlns:xml',
                                             expanded_name = {"xmlns","xml"},
                                             nsinfo = {"xmlns","xml"},
                                             namespace = #xmlNamespace{default = [], 
                                                                       nodes = [{"xml",'http://www.w3.org/XML/1998/namespace'}]}},
                               #xmlAttribute{name = 'xml:attr1',
                                             expanded_name = {'http://www.w3.org/XML/1998/namespace',attr1},
                                             nsinfo = {"xml","attr1"},
                                             namespace = #xmlNamespace{default = [], 
                                                                       nodes = [{"xml",'http://www.w3.org/XML/1998/namespace'}]}}]},
     []
    } = xmerl_scan:string(Doc2, [{namespace_conformant, true}]),
    ok.

scan_splits_string_bug(_Config) ->
    {#xmlElement{ content = [#xmlText{ value = "Jimmy Zöger" }] }, []}
        = xmerl_scan:string("<name>Jimmy Z&#246;ger</name>").

pe_ref1(Config) ->
    file:set_cwd(datadir(Config)),
    {#xmlElement{},[]} = xmerl_scan:file(datadir_join(Config,[misc,"PE_ref1.xml"]),[{validation,true}]).

copyright(Config) ->
    file:set_cwd(datadir(Config)),
    {#xmlElement{},[]} = xmerl_scan:file(datadir_join(Config,[misc,"cprght.xml"]),[{validation,true}]).

testXSEIF(Config) ->
    file:set_cwd(datadir(Config)),
    {#xmlElement{},[]} = xmerl_scan:file(datadir_join(Config,[misc,"ReplBoard_1_1543-CNA11313Uen.xml"]),[{validation,true}]).

export_simple1(_Config) ->
    Simple = simple(),
    Res = xmerl:export_simple(Simple,xmerl_xml,[{title, "Doc Title"}]),
    "<?xml version="++_ = lists:flatten(Res),

    %% Use of fun in simple content OTP-6679
    Simple2 = simple2(),
    Res2 = xmerl:export_simple(Simple2,xmerl_xml,[{title,"Doc Title"}]),
    true = (Res2 =:= Res),
    ok.

export(Config) ->
    DataDir = datadir(Config),
    Prolog = ["<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n<!DOCTYPE motorcycles SYSTEM \"motorcycles.dtd\">\n"],
    TestFile = filename:join([DataDir,"misc","motorcycles.xml"]),
    {E,_} = xmerl_scan:file(TestFile),
    Exported = xmerl:export([E],xmerl_xml,[{prolog,Prolog}]),
    B = list_to_binary(Exported++"\n"),
    {ok, B} = file:read_file(TestFile),
    ok.

%%----------------------------------------------------------------------

sax_parse_and_export(Config) ->
    ok = sax_parse_export_xml_big(Config),
    ok = sax_parse_export_xml_small(Config).

%%----------------------------------------------------------------------

sax_parse_export_xml_big(Config) ->
    DataDir = datadir(Config),
    OutDir = privdir(Config),
    io:format("DataDir: ~p~n,OutDir:~p~n",[DataDir,OutDir]),
    CMOMxml = filename:join([DataDir,"eventp","CMOM.xml"]),
    {Ex,[]} = xmerl_eventp:file_sax(CMOMxml, xmerl_xml,[],[]),
    OutFile = filename:join([OutDir,"cmom"]),
    file:delete(OutFile),
    StubFile = filename:join([DataDir,"eventp","CelloMOM.stub"]),
    {ok,Bin} = file:read_file(StubFile),
    {ok,IO} = file:open(OutFile,[write,append]),
    ok = file:write(IO,Bin),
    ok = io:format(IO,"~s~n~n",[lists:flatten(Ex)]),
    Cmd = lists:flatten(io_lib:format("cmp ~s ~s",[OutFile,CMOMxml])),
    [] = os:cmd(Cmd),
    ok.

sax_parse_export_xml_small(Config) ->
    DataDir = datadir(Config),
    OutDir = privdir(Config),
    Wurfl_xml = filename:join([DataDir,"eventp","wurfl.xml"]),
    {Ex,[]} = xmerl_eventp:file_sax(Wurfl_xml, xmerl_xml,[],[]),
    OutFile = filename:join([OutDir,"wrfl"]),
    file:delete(OutFile),
    StubFile = filename:join([DataDir,"eventp","wurfl.stub"]),
    {ok,Bin} = file:read_file(StubFile),
    {ok,IO} = file:open(OutFile,[write,append]),
    ok = file:write(IO,Bin),
    ok = io:format(IO,"~s~n",[lists:flatten(Ex)]),
    Cmd = lists:flatten(io_lib:format("cmp ~s ~s",[OutFile,Wurfl_xml])),
    [] = os:cmd(Cmd),
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
ticket_5998(Config) ->
    DataDir = datadir(Config),
    %% First fix is tested by case syntax_bug2.
    
    case catch xmerl_scan:file(filename:join([DataDir,misc,"ticket_5998_2.xml"])) of
        {'EXIT',{fatal,Reason1}} ->
            case Reason1 of
                {{endtag_does_not_match,
                  {was,obj,should_have_been,int}},
                 _,_,_} -> ok;
                _ -> {comment,"parsing changed behaviour"}
            end
    end,

    case catch xmerl_scan:file(filename:join([DataDir,misc,"ticket_5998_3.xml"])) of
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
ticket_7211(Config) ->
    DataDir = datadir(Config),
    {E,[]} = xmerl_scan:file(filename:join([DataDir,misc,"notes2.xml"]),
                             [{fetch_path,[filename:join([DataDir,misc,erlang_docs_dtd])]},
                              {validation,dtd}]),
    
    ok = case E of
             Rec when is_record(Rec,xmlElement) ->
                 ok;
             _ ->
                 E
         end,
		       
    {E2,[]} = xmerl_scan:file(filename:join([DataDir,misc,"XS.xml"]),
                              [{fetch_path,[filename:join([DataDir,misc,erlang_docs_dtd])]},
                               {validation,dtd}]),
    
    ok = case E2 of
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
ticket_7214(Config) ->
    DataDir = datadir(Config),

    {E,[]} = xmerl_scan:file(filename:join([DataDir,misc,'block_tags.html']),
                             [{validation,dtd},
                              {fetch_path,[filename:join([DataDir,misc,erlang_docs_dtd])]}]),
    
    ok = case E of
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
ticket_7430(Config) ->
    DataDir = datadir(Config),

    {E,[]} = xmerl_scan:string("<a>\303\251&#xD;\303\251</a>",[{encoding,'utf-8'}]),

    ok = case E of
             {xmlElement,a,a,[],
              {xmlNamespace,[],[]},
              [],1,[],
              [{xmlText,[{a,1}],1,[],"é\né",text}],
              [],_,undeclared} ->
                 ok;
             _ ->
                 E
         end.

ticket_6873(Config) ->
    file:set_cwd(filename:join(datadir(Config),xpath)),
    ok = xpath_abbrev:ticket_6873(),
    ok = xpath_lib:ticket_6873().

ticket_7496(Config) ->
    file:set_cwd(filename:join(datadir(Config),xpath)),
    ok = xpath_abbrev:ticket_7496().

ticket_8156(Config) ->
    {ftp,{[],[]},"testmachine1",21,"/w.erl"} = xmerl_uri:parse("ftp://testmachine1/w.erl"),
    {ftp,{"user",[]},"testmachine1",21,"/w.erl"} = xmerl_uri:parse("ftp://user@testmachine1/w.erl"),
    {ftp,{"user","hello"},"testmachine1",21,"/w.erl"} = xmerl_uri:parse("ftp://user:hello@testmachine1/w.erl"),
    {ftp,{[],[]},"testmachine1",3000,"/w.erl"} = xmerl_uri:parse("ftp://testmachine1:3000/w.erl"),
    {ftp,{"user","hello"},"testmachine1",3000,"/w.erl"} = xmerl_uri:parse("ftp://user:hello@testmachine1:3000/w.erl"),
    ok.

%% Test that xmerl_scan can decode unicode entities properly
ticket_8697(Config) ->
    {UTF8Output, []} = xmerl_scan:string("<?xml version=\"1\" ?>\n<text>" ++ [229, 145, 156] ++ "</text>"),
    #xmlElement{content = [#xmlText{value = UTF8Text}]} = UTF8Output,
    [16#545C] = UTF8Text,
    {DecEntityOutput, []} = xmerl_scan:string("<?xml version=\"1\" ?>\n<text>&#21596;</text>"),
    #xmlElement{content = [#xmlText{value = DecEntityText}]} = DecEntityOutput,
    [21596] = DecEntityText,
    {HexEntityOutput, []} = xmerl_scan:string("<?xml version=\"1\" ?>\n<text>&#x545C;</text>"),
    #xmlElement{content = [#xmlText{value = HexEntityText}]} = HexEntityOutput,
    [16#545C] = HexEntityText,
    ok.

%% Test that xmerl_scan handles attribute that contains for example &quot
ticket_9411(Config) ->
    DataDir = datadir(Config),

    {ok, Schema} = xmerl_xsd:process_schema(filename:join([DataDir,"misc/ticket_9411.xsd"])),
    {ok, Bin} = file:read_file(filename:join([DataDir,"misc/ticket_9411.xml"])),
    Xml = erlang:binary_to_list(Bin),
    {E, _} = xmerl_scan:string(Xml),
    {E, _} = xmerl_xsd:validate(E, Schema).

%% Test that xmerl_scan handles continuation correct when current input runs out at the end of an attribute value
ticket_9457(Config) ->
    Opts = [{continuation_fun, fun ticket_9457_cont/3, start}, {space, normalize}],
    {E, _} = xmerl_scan:string([], Opts).

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


%% Test that comments are handled correct whith
ticket_9664_schema(Config) ->
    {E, _} = xmerl_scan:file(datadir_join(Config,[misc,"ticket_9664_schema.xml"]),[]),
    {ok, S} = xmerl_xsd:process_schema(datadir_join(Config,[misc,"motorcycles.xsd"])),
    {E1, _} = xmerl_xsd:validate(E, S),

    {E1,_} = xmerl_xsd:process_validate(datadir_join(Config,[misc,"motorcycles.xsd"]),E,[]),

    {E1,_} = xmerl_scan:file(datadir_join(Config,[misc,"ticket_9664_schema.xml"]),
                             [{schemaLocation, [{"mc", "motorcycles.xsd"}]},
                              {validation, schema}]),
    ok.

%% Test that comments are handled correct whith
ticket_9664_dtd(Config) ->
    {E, _} = xmerl_scan:file(datadir_join(Config,[misc,"ticket_9664_dtd.xml"]),[]),
    {E, _} = xmerl_scan:file(datadir_join(Config,[misc,"ticket_9664_dtd.xml"]),[{validation, true}]),
    ok.


%%======================================================================
%% Support Functions
%%======================================================================

%% Dir is a directory
rm_f_(Dir) ->
    {ok,CWD} = file:get_cwd(),
    {ok,FileList} = file:list_dir(Dir),
    file:set_cwd(filename:join([CWD,Dir])),
    rm_files(FileList),
    file:set_cwd(CWD),
    ? line ok = file:del_dir(Dir).

rm_files([])->
    ok;
rm_files([F|Fs]) ->
    case filelib:is_dir(F) of
	true ->
	    rm_f_(F);
	_ ->
	    ok = file:delete(F)
    end,
    rm_files(Fs).

change_mode(Files) ->
    change_mode3(Files).
change_mode2(Dir)->
    {ok,CWD} = file:get_cwd(),
    {ok,FileList} = file:list_dir(Dir),
    file:set_cwd(filename:join([CWD,Dir])),
    change_mode3(FileList),
    file:set_cwd(CWD).
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

privdir(Config) ->
    proplists:get_value(priv_dir, Config).
datadir(Config) ->
    proplists:get_value(data_dir, Config).

datadir_join(Config,Files) ->
    filename:join([datadir(Config)|Files]).
