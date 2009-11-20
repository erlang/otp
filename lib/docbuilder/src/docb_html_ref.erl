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
-module(docb_html_ref).

-export([rule/2, rule/3]).

rule([description|_],_) ->
    {"\n<h3>DESCRIPTION</h3>\n<div class=\"REFBODY\">\n","\n</div>\n"};

rule([funcs|_],_) ->
    {"\n<h3>EXPORTS</h3>\n",""};

rule([func|_],_) ->
    {"\n<p>",""};

rule([name, func, funcs, RefType|_], {_,_,[{pcdata,[],Name0}]}) ->
    Name1 = docb_html_util:make_anchor_name_short(Name0, RefType),
    {"<a name=\"" ++ Name1 ++ "\"><span class=\"bold_code\">",
     "</span></a><br/>\n"};

rule([fsummary|_],_) ->
    {drop, "\n</p>\n"};

rule([type|_], _) ->
    {"\n<div class=\"REFBODY\"><p>Types:</p>\n  <div class=\"REFTYPES\">\n<p>\n",
     "\n </p> </div>\n</div>\n"};

rule([v|_], _) ->
    {"<span class=\"bold_code\">","</span><br/>\n"};

rule([d|_], _) ->
    {"\n<div class=\"REFBODY\">\n","\n</div>\n"};

rule([desc|_], _) ->
    {"\n<div class=\"REFBODY\">\n","\n</div>\n"};

rule([authors|_], _) -> 
    {"\n<h3>AUTHORS</h3>\n<div class=\"REFBODY\">\n","\n</div>\n"};

rule([aname|_], _) ->
    {"", " - "};

rule([section|_], {1,_,_}) ->
    {"", ""};
rule([section|_], {_N,_,_}) ->
    {"", "\n</div>\n"};

rule([title|_], _) ->
    {"\n<h3>", "</h3>\n<div class=\"REFBODY\">\n"};

rule(TagHistory, TagBody) ->
    docb_html:rule(TagHistory, TagBody).

rule([email|_], _, Opts) ->
    case docb_util:html_snippet(email, Opts) of
	"" ->
	    {{"","<br/>\n"}, Opts};
	Email ->
	    {{drop, Email++"<br/>\n"}, Opts}
    end;

rule(TagHistory, TagBody, Opts) ->
    docb_html:rule(TagHistory, TagBody, Opts).

