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
-module(docb_tr_cref2html).

-export([extension/0, transform/3, rule/2, rule/3]).

extension() ->
  ".html".

transform(_File, {cref,_,[Header|Rest]}, _Opts) ->
    Data = [{[], [], docb_html_util:all_header_data(Header)}],
    {cref, Data, [{header,[],[]}|Rest]}.

rule([header|_],_) ->
    {drop, ""};

rule([ret|_],_) ->
    {"",""};

rule([nametext|_],_) ->
    {" ",""};

rule([name|_], {_,_,[_Ret,{nametext,[],[{pcdata,[],Name}]}]}) ->
    FName = lists:flatten(docb_html_util:pcdata_to_html(Name)),
    TName = docb_util:trim(FName), 
    CAnchor = docb_util:fknidx(TName, "/"),
    {"<A NAME=\"" ++ CAnchor ++ "\"><STRONG><CODE>",
     "</CODE></STRONG></A><BR>\n"};
rule([name|T], {I,As,[Ret,{pcdata,[],Name}]}) -> % For SGML DTD
    rule([name|T], {I,As,[Ret,{nametext,[],[{pcdata,[],Name}]}]});

rule([lib|_],_) ->
    {"\n<H3>C LIBRARY</H3>\n<DIV CLASS=REFBODY>\n","\n</DIV>\n"};

rule([libsummary|_],_) ->
    {"\n<H3>C LIBRARY SUMMARY</H3>\n<DIV CLASS=REFBODY>\n","\n</DIV>\n"};

rule(TagHistory, TagBody) ->
    docb_html_ref:rule(TagHistory, TagBody).

rule([cref|_], {_,[Data],_}, Opts) ->
    {{docb_html_layout:ref_top(Data, Opts),
      docb_html_layout:ref_bot(Opts)}, Opts};

rule(TagHistory, TagBody, Opts) ->
    docb_html_ref:rule(TagHistory, TagBody, Opts).
