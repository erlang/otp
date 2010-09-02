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
     xpath_abbreviated_syntax, xpath_functions,
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
       ticket_6873, ticket_7496, ticket_8156, ticket_8697]},
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
    ?line ok = xmerl_test_lib:export_simple1(Config).

export(suite) -> [];
export(Config) ->
    ?line ok = xmerl_test_lib:export(Config).

%%----------------------------------------------------------------------

sax_parse_and_export(suite) -> [];
sax_parse_and_export(Config) ->
    ?line ok = xmerl_test_lib:sax_parse_export_xml_big(Config),
    ?line ok = xmerl_test_lib:sax_parse_export_xml_small(Config).

%%----------------------------------------------------------------------

ticket_5998(suite) -> [];
ticket_5998(Config) ->
    ?line ok = xmerl_test_lib:ticket_5998(Config).

ticket_7211(suite) -> [];
ticket_7211(Config) ->
    ?line ok = xmerl_test_lib:ticket_7211(Config).

ticket_7214(suite) -> [];
ticket_7214(Config) ->
    ?line ok = xmerl_test_lib:ticket_7214(Config).


ticket_7430(suite) -> [];
ticket_7430(Config) ->
    ?line ok = xmerl_test_lib:ticket_7430(Config).

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

%%----------------------------------------------------------------------

% app_test(Config) ->
%     ?line xmerl_app_test:app().





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
    
