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
-module(docb_tr_term2html).

-export([extension/0, transform/3, rule/2, rule/3]).

extension() ->
    ".html".

transform(_File, Tree, Opts) ->
    purge(Tree, Opts).
  
purge({Tag, Attrs, [Header|Body]}, Opts) ->
    TermList = case docb_util:lookup_option({defs,term}, Opts) of
		   false -> [];
		   Value  -> Value
	       end,
    B1 = purge_body(Body, TermList),
    B2 = lists:ukeysort(2, B1),
    {Tag, Attrs, [Header|B2]}.

purge_body([], _) ->
    [];
purge_body([{pcdata,_Attrs,_More}|Rest], TermList) ->
    purge_body(Rest, TermList);
purge_body([{term,[{"ID","CDATA",ID}],More}|Rest], TermList) ->
    case lists:keysearch(ID, 1, TermList) of
	false ->
	    [{term,[{"NAME","CDATA",ID},{"ID","CDATA",ID}],More}|
	     purge_body(Rest, TermList)];
	{value, {ID, Name, _Description, _Responsible}} ->
	    [{term,[{"NAME","CDATA",Name},{"ID","CDATA",ID}],More}|
	     purge_body(Rest, TermList)];
	{value, {ID, Name, _Description}} ->
	    [{term,[{"NAME","CDATA",Name},{"ID","CDATA",ID}],More}|
	     purge_body(Rest, TermList)]
    end;
purge_body([{_Tag,_Attrs,More}|Rest], TermList) ->
    lists:append(purge_body(More, TermList),
		 purge_body(Rest, TermList)).

rule([header|_], _) ->
    {drop, ""};

rule(_, _) ->
    {drop, ""}.

rule([term|_], {_,[],[Header]}, Opts) ->
    {{docb_html_layout:chapter_top(
	docb_html_util:all_header_data(Header), Opts) ++
      "\n<center><h1>Glossary</h1></center>\n",
      docb_html_layout:chapter_bot(Opts)}, Opts};

rule([term|_], {_,[],[Header|_]},Opts) ->
    {{docb_html_layout:chapter_top(
	docb_html_util:all_header_data(Header), Opts) ++
      "\n<center><h1>Glossary</h1></center>\n<dl>\n",
      "\n</dl>\n" ++ docb_html_layout:chapter_bot(Opts)}, Opts};

rule([term|_], {_,[Data],_}, Opts) ->
    {{docb_html_layout:chapter_top(Data, Opts) ++
      "\n<center><h1>Bibliography</h1></center>\n<dl>\n",
      "\n</dl>\n" ++ docb_html_layout:chapter_bot(Opts)}, Opts};

rule([term|_], {_,[Name,ID],[{termdef,[],[{pcdata,[],Def}]}]}, Opts) ->
    TermList = case docb_util:lookup_option({defs,term}, Opts) of
		   false -> [];
		   Value  -> Value
	       end,
    case lists:keysearch(ID, 1, TermList) of
	false ->
	    {{drop,"\n<dt><a name=\"" ++ ID ++ "\">" ++ 
	      "<strong>" ++ ID ++ "</strong></a>\n</dt>\n<dd>" ++
	      docb_html_util:pcdata_to_html(Def) ++ "\n</dd>\n"}, Opts};
	{value, {ID, Name, Description, _Responsible}} ->
	    docb_util:message(warning,
			      "Global term ~s overriding local", [ID]),
	    {{drop,"\n<dt><a name=\"" ++ ID ++ "\">" ++
	      "<strong>" ++ Name ++ "</strong></a></dt>\n<dd>" ++
	      docb_html_util:pcdata_to_html(Description) ++ "\n</dd>\n"},
	     Opts};
	{value, {ID, Name, Description}} ->
	    docb_util:message(warning,
			      "Global term ~s overriding local", [ID]),
	    {{drop, "\n<dt><a name=\"" ++ ID ++ "\">" ++ 
	      "<strong>" ++ Name ++ "</strong></a></dt>\n<dd>" ++
	      docb_html_util:pcdata_to_html(Description) ++ "\n</dd>\n"}, Opts}
    end;

rule([term|_], {_,[Name,ID],_}, Opts) ->
    TermList = case docb_util:lookup_option({defs,term}, Opts) of
		   false -> [];
		   Value  -> Value
	       end,
    case lists:keysearch(ID, 1, TermList) of
	false ->
	    docb_util:message(error,
			      "The term ~s has no definition", [ID]),
	    {{drop, "\n<dt><a name=\"" ++ ID ++ "\">" ++ 
	      "<strong>" ++ ID ++ "</strong></a></dt>\n<dd>" ++
	      "??" ++ "\n</dd>\n"}, Opts};
	{value, {ID, Name, Description, _Responsible}} ->
	    {{drop, "\n<dt><a name=\"" ++ ID ++ "\">" ++
	      "<strong>" ++ Name ++ "</strong></a></dt>\n<dd>" ++
	      docb_html_util:pcdata_to_html(Description) ++ "\n</dd>\n"},
	     Opts};
	{value, {ID, Name, Description}} ->
	    {{drop, "\n<dt><a name=\"" ++ ID ++ "\">" ++ 
	      "<strong>" ++ Name ++ "</strong></a></dt>\n<dd>" ++
	      docb_html_util:pcdata_to_html(Description) ++ "\n</dd>\n"}, Opts}
    end.
