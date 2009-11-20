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
-module(docb_tr_first2html).

-export([extension/0, transform/3, rule/2, rule/3]).

extension() ->
    ".html".

transform(_File, Tree, _Opts) ->
    Tree.

rule([header|_], _) ->
    {drop, ""};

rule([description|_], _) ->
    {"", ""};

rule([include|_], _) ->
    {drop, ""};

rule(TagHistory, TagBody) ->
    docb_html:rule(TagHistory, TagBody).

rule([first|_], {_,[],[Header|_]}, Opts) ->
    HeaderData = docb_html_util:all_header_data(Header),
    {{docb_html_layout:first_top(HeaderData, Opts),
      docb_html_layout:first_bot(Opts)}, Opts};

rule(TagHistory, TagBody, Opts) ->
    docb_html:rule(TagHistory, TagBody, Opts).
