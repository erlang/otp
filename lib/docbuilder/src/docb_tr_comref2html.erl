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
-module(docb_tr_comref2html).

-export([extension/0, transform/3, rule/2, rule/3]).

extension() ->
    ".html".

transform(_File, {comref,_,[Header|Rest]}, _Opts) ->
    Data = [{[], [], docb_html_util:all_header_data(Header)}],
    {comref, Data, [{header,[],[]}|Rest]}.

rule([header|_],_) ->
    {drop,""};

rule([com|_],_) ->
    {"\n<h3>COMMAND</h3>\n<div class=\"REFBODY\">\n","\n</div>\n"};

rule([comsummary|_],_) ->
    {"\n<h3>COMMAND SUMMARY</h3>\n<div class=\"REFBODY\">\n","\n</div>\n"};

rule(TagHistory, TagBody) ->
    docb_html_ref:rule(TagHistory, TagBody).

rule([comref|_], {_,[Data],_}, Opts) ->
    {{docb_html_layout:ref_top(Data, Opts),
      docb_html_layout:ref_bot(Opts)}, Opts};

rule(TagHistory, TagBody, Opts) ->
    docb_html_ref:rule(TagHistory, TagBody, Opts).
