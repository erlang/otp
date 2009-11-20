%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999-2000, Ericsson 
%% Utvecklings AB. All Rights Reserved.''
%% 
%%     $Id$
%%
-module(docb_tr_report2html).

-export([extension/0, transform/3, rule/2, rule/3]).

%%
%% File extension
%%

extension() ->
  ".html".

transform(File, {report,_,[Header|Rest]}, Opts) ->
    Data = [{[], [], docb_html_util:all_header_data(Header)}],
    Tree = {report, Data, [{header,[],[]}|Rest]},
    ChapterLevel = case docb_util:lookup_option(number, Opts) of
		       false -> none;
		       Value -> Value
		   end,
    NumberTree = docb_html_util:number(Tree, ChapterLevel, File),
    options(NumberTree, Opts).

options(Tree, []) ->
    Tree;
options(Tree, [_|Rest]) ->
    options(Tree, Rest).

rule([header|_], _) ->
    {drop, ""};

rule([toc|_], {_,_,ToC}) ->
    {drop, "\n<h3>Table of Contents</h3>\n" ++
     docb_html_util:format_toc(ToC) ++ "\n"};

rule([section|_], _) ->
    {"", ""};

rule([title|Rest], {_,[Number,_File], [{pcdata,_,Title}]}) ->
    N = integer_to_list(docb_html_util:count_sections(Rest)+1),
    {drop, "\n<h" ++ N ++ ">" ++ Number ++ " " ++
     docb_html_util:pcdata_to_html(Title) ++ "</h" ++ N ++ ">\n"};

rule([erlinclude|_], {_,[File,Tag],_}) ->
    docb_html_util:erl_include(File, Tag);

rule(TagHistory, TagBody) ->
    docb_html:rule(TagHistory, TagBody).

rule([report|_], {_,[Data],_}, Opts) ->
    {{docb_html_layout:report_top(Data, Opts),
      docb_html_layout:report_bot(Opts)}, Opts};

rule(TagHistory, TagBody, Opts) ->
    docb_html:rule(TagHistory, TagBody, Opts).
