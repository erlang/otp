%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%% The Original Code is xmerl-0.7
%%%
%%% The Initial Developer of the Original Code is Ericsson Telecom
%%% AB. Portions created by Ericsson are Copyright (C), 1998, Ericsson
%%% Telecom AB. All Rights Reserved.
%%%
%%% Contributor(s): ______________________________________.
%%%
%%%----------------------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%%----------------------------------------------------------------------
%%% File:        test_html.erl
%%% Author       : Ulf Wiger <ulf.wiger@ericsson.com>

%%% Description  : Callback module for exporting XML to HTML with support
%%%                for special Erlang-related tags. (Experimental)
%%% 
%%% Modules used : lists, io_lib
%%% 
%%%----------------------------------------------------------------------

-module(test_html).
-author('ulf.wiger@ericsson.com').


-export(['#xml-inheritance#'/0]).

%%% special Erlang forms
-export(['EXIT'/4,
	 'tuple_list'/4]).

-export(['#root#'/4,
	 title/4,
	 heading/4,
	 section/4,
	 table/4,
	 row/4,
	 col/4,
	 data/4,
	 p/4, para/4, 'P'/4,
	 emphasis/4]).

-include("xmerl.hrl").


'#xml-inheritance#'() -> [xmerl_xml].



%% The '#root#' tag is called when the entire structure has been exported.
%% It does not appear in the structure itself.
'#root#'(Data, Attrs, [], E) ->
    Title = 
	case find_attribute(title, Attrs) of
	    {value, T} ->
		["<title>", T, "</title>"];
	    false ->
		[]
	end,
    ["<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML//EN\">\n"
     "<html>\n",
     "<head>\n", Title, "</head>\n"
     "<body>\n", Data, "</body>\n"].




%%% Special token: EXIT
'EXIT'(Reason, Attrs = [], Parents = [], E) ->
    %% This happens e.g. if a request function crashes completely.
    ["<pre>\n", mk_string({'EXIT', Reason}), "</pre>"].


title(Str, Attrs, Parents, E) ->
    ["<h1>", Str, "</h1>\n"].


%%% section/3 is to be used instead of headings.
section(Data, Attrs, [{section,_}, {section,_}, {section,_} | _], E) ->
    opt_heading(Attrs, "<h4>", "</h4>", Data);
section(Data, Attrs, [{section,_}, {section,_} | _], E) ->
    opt_heading(Attrs, "<h3>", "</h3>", Data);
section(Data, Attrs, [{section,_} | _], E) ->
    opt_heading(Attrs, "<h2>", "</h2>", Data);
section(Data, Attrs, Parents, E) ->
    opt_heading(Attrs, "<h1>", "</h1>", Data).

opt_heading(Attrs, StartTag, EndTag, Data) ->
    case find_attribute(heading, Attrs) of
	{value, Text} ->
	    [StartTag, Text, EndTag, "\n" | Data];
	false ->
	    Data
    end.


%% tables
%% e.g. {table, [{heading, [{col, H1}, {col, H2}]},
%%		 {row, [{col, C11}, {col, C12}]},
%%		 {row, [{col, C21}, {col, C22}]}]}.
                             
table(Data, Attrs, Parents, E) ->
    Border = case find_attribute(border, Attrs) of
		 false ->
		     " border=1";
		 {value, N} ->
		     [" border=", mk_string(N)]
	     end,
    ["<table", Border, ">\n", Data, "\n</table>\n"].

row(Data, Attrs, [{table,_}|_], E) ->
    ["<tr>", Data, "</tr>\n"].

heading(Data, Attrs, [{table,_}|_], E) ->
    ["<tr>", Data, "</tr>\n"].


%% Context-sensitive columns (heading- or row columns)
col(Data, Attrs, [{heading,_}, {table,_} | _], E) ->
    ["<th>", nbsp_if_empty(Data), "</th>\n"];
col(Data, Attrs, [{row,_}, {table,_} | _], E) ->
    ["<td>", nbsp_if_empty(Data), "</td>\n"].


tuple_list(List, Attrs, Parents, E) ->
    Elems = case find_attribute(elements, Attrs) of
		{value, Es} -> 
		    Es;
		false ->
		    case List of
			[H|_] ->
			    lists:seq(1,size(H));
			[] ->
			    []
		    end
	    end,
    TableData = [{row, [{col, {element(P, Rec)}} || P <- Elems]} ||
		    Rec <- List],
    Table = case find_attribute(heading, Attrs) of
		{value, Cols} ->
		    Head = {heading, [{col, C} || C <- Cols]},
		    {table, [Head | TableData]};
		false ->
		    {table, TableData}
	    end,
    {'#xml-redefine#', Table}.


data(Data, Pos, Attrs, Parents) ->
    mk_string(Data).



p(Data, Pos, Attrs, Parents) ->
    {'#xml-alias#', 'P'}.

para(Data, Pos, Attrs, Parents) ->
    {'#xml-alias#', 'P'}.

'P'(Data, Pos, Attrs, Parents) ->
    ["<p>", mk_string(Data), "</p>\n"].


emphasis(Str, Pos, Attrs, Parents) ->
    ["<strong>", Str, "</strong>"].


nbsp_if_empty(Data) when binary(Data), size(Data) == 0 ->
    "&nbsp;";
nbsp_if_empty(Data) when list(Data) ->
    case catch list_to_binary(Data) of
	{'EXIT', _} ->
	    nbsp_if_empty_term(Data);
	B when size(B) == 0 ->
	    "&nbsp;";
	_ ->
	    Data
    end;
nbsp_if_empty(Data) ->
    nbsp_if_empty_term(Data).

nbsp_if_empty_term(Data) ->
    Str = io_lib:format("~p", [Data]),
    case list_to_binary(Str) of
	B when size(B) == 0 ->
	    "&nbsp;";
	_ ->
	    Str
    end.


mk_string(I) when integer(I) ->
    integer_to_list(I);
mk_string(A) when atom(A) ->
    atom_to_list(A);
mk_string(L) when list(L) ->
    %% again, we can't regognize a string without "parsing" it
    case catch list_to_binary(L) of
	{'EXIT',_} ->
	    io_lib:format("~p", [L]);
	_ ->
	    L
    end;
mk_string(Term) ->
    io_lib:format("~p", [Term]).



find_attribute(Name, Attrs) ->
    case lists:keysearch(Name, #xmlAttribute.name, Attrs) of
	{value, #xmlAttribute{value = V}} ->
	    {value, V};
	false ->
	    false
    end.
