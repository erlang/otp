%%
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
%%
%%%%% Purpose : Test suite for the xmerl application, xmerl_eventp module
%%%-------------------------------------------------------------------
%%% File    : xmerl_sax_lib.erl
%%% Author  : Bertil Karlsson <bertil@finrod>
%%% Description : 
%%%
%%% Created : 28 Apr 2006 by Bertil Karlsson <bertil@finrod>
%%%-------------------------------------------------------------------
-module(xmerl_test_lib).

-compile(export_all).

-include("test_server.hrl").
-include("xmerl.hrl").

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

export_simple1(_Config) ->
    Simple = simple(),
    Res = xmerl:export_simple(Simple,xmerl_xml,[{title, "Doc Title"}]),
    ?line "<?xml version="++_ = lists:flatten(Res),

    %% Use of fun in simple content OTP-6679
    Simple2 = simple2(),
    Res2 = xmerl:export_simple(Simple2,xmerl_xml,[{title,"Doc Title"}]),
    ?line true = (Res2 =:= Res),
    ok.

export(Config) ->
    DataDir = ?config(data_dir,Config),
    Prolog = ["<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n<!DOCTYPE motorcycles SYSTEM \"motorcycles.dtd\">\n"],
    TestFile = filename:join([DataDir,"misc","motorcycles.xml"]),
    ?line {E,_} = xmerl_scan:file(TestFile),
    ?line Exported = xmerl:export([E],xmerl_xml,[{prolog,Prolog}]),
    B = list_to_binary(Exported++"\n"),
    ?line {ok,B} = file:read_file(TestFile),
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

%% cmp_element/2
%% First argument result after parsing
%% Second argument result after validation
cmp_element(E,E) ->
    ok;
cmp_element(#xmlElement{name=N,attributes=A1,content=C1},
	    #xmlElement{name=N,attributes=A2,content=C2}) ->
    case cmp_attributes(A1,A2) of
	ok ->
	    cmp_elements(C1,C2);
	Err -> Err
    end;
cmp_element(#xmlText{},#xmlText{}) ->
    ok;
cmp_element(A,B) ->
    {error,{A,does_not_match,B}}.

cmp_elements([H1|T1],[H2|T2]) ->
    case cmp_element(H1,H2) of
	ok ->
	    cmp_elements(T1,T2);
	Err ->
	    Err
    end;
cmp_elements([],[]) ->
    ok.

%% All attributes in argument 1 must be present in 2
cmp_attributes([A1|T1],Atts2) ->
    case keysearch_delete(A1#xmlAttribute.name,#xmlAttribute.name,Atts2) of
	{A2,NewAtts2} ->
	    case A1#xmlAttribute.value == A2#xmlAttribute.value of
		true ->
		    cmp_attributes(T1,NewAtts2);
		_ ->
		    {error,{mismatching_values_in_attsibutes,A1,A2}}
	    end;
	_ ->
	    {error,{no_matching_attsibute,A1,in,Atts2}}
    end;
cmp_attributes([],_) ->
   ok.

keysearch_delete(Key,N,List) ->
    case lists:keysearch(Key,N,List) of
	{value,Res} ->
	    {Res,lists:keydelete(Key,N,List)};
	_ ->
	    false
    end.

%%-------------------------------------------------------
%% TICKET tests
%%-------------------------------------------------------

%%
%% ticket_5998
%%
%% A Kleene Closure child in a sequence consumed all following
%% childs. This problem has been fixed.
%%
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

