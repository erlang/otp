%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2016. All Rights Reserved.
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
-module(mnesia_text).

%% Avoid warning for local function error/1 clashing with autoimported BIF.
-compile({no_auto_import,[error/1]}).
-export([parse/1, file/1, load_textfile/1, dump_to_textfile/1]).

load_textfile(File) ->
    ensure_started(),
    case parse(File) of
	{ok, {Tabs, Data}} ->
	    Badtabs = make_tabs(lists:map(fun validate_tab/1, Tabs)),
	    load_data(del_data(Badtabs, Data, []));
	Other ->
	    Other
    end.

dump_to_textfile(File) ->
    dump_to_textfile(mnesia_lib:is_running(), file:open(File, [write])).
dump_to_textfile(yes, {ok, F}) ->
    Tabs = lists:delete(schema, mnesia_lib:local_active_tables()),
    Defs = lists:map(fun(T) -> {T, [{record_name, mnesia_lib:val({T, record_name})},
				    {attributes, mnesia_lib:val({T, attributes})}]} 
		     end,
		     Tabs),
    io:format(F, "~p.~n", [{tables, Defs}]),
    lists:foreach(fun(T) -> dump_tab(F, T) end, Tabs),
    file:close(F);
dump_to_textfile(_,_) -> error.

    
dump_tab(F, T) ->
    W = mnesia_lib:val({T, wild_pattern}),
    {atomic,All} = mnesia:transaction(fun() -> mnesia:match_object(T, W, read) end),
    lists:foreach(fun(Term) -> io:format(F,"~p.~n", [setelement(1, Term, T)]) end, All).


ensure_started() ->
    case mnesia_lib:is_running() of
	yes ->
	    yes;
	no ->
	    case mnesia_lib:exists(mnesia_lib:dir("schema.DAT")) of
		true ->
		    mnesia:start();
		false ->
		    mnesia:create_schema([node()]),
		    mnesia:start()
	    end
    end.

del_data(Bad, [H|T], Ack) ->
    case lists:member(element(1, H), Bad) of
	true -> del_data(Bad, T, Ack);
	false -> del_data(Bad, T, [H|Ack])
    end;
del_data(_Bad, [], Ack) ->
    lists:reverse(Ack).
		
%% Tis the place to call the validate func in mnesia_schema
validate_tab({Tabname, List}) ->
    {Tabname, List};
validate_tab({Tabname, RecName, List}) ->
    {Tabname, RecName, List};
validate_tab(_) -> error(badtab).

make_tabs([{Tab, Def} | Tail]) ->
    try mnesia:table_info(Tab, where_to_read) of
	Node ->
	    io:format("** Table ~w already exists on ~p, just entering data~n",
		      [Tab, Node]),
	    make_tabs(Tail)
    catch exit:_ -> %% non-existing table
	    case mnesia:create_table(Tab, Def) of
		{aborted, Reason} ->
		    io:format("** Failed to create table ~w ~n"
			      "** Reason = ~w, Args = ~p~n", 
			      [Tab, Reason, Def]),
		    [Tab | make_tabs(Tail)];
		_ -> 
		    io:format("New table ~w~n", [Tab]),
		    make_tabs(Tail)
	    end
    end;

make_tabs([]) -> 
    [].

load_data(L) ->
    mnesia:transaction(fun() ->
			       F = fun(X) -> 
					   Tab = element(1, X),
					   RN = mnesia:table_info(Tab, record_name),
					   Rec = setelement(1, X, RN),
					   mnesia:write(Tab, Rec, write) end,
			       lists:foreach(F, L)
		       end).

parse(File) ->
    case file(File) of
	{ok, Terms} ->
	    try collect(Terms) of
		Other -> {ok, Other}
	    catch throw:Error -> Error
	    end;
	Other ->
	    Other
    end.

collect([{_, {tables, Tabs}}|L]) ->
    {Tabs, collect_data(Tabs, L)};

collect(_) ->
    io:format("No tables found\n", []),
    error(bad_header).

collect_data(Tabs, [{Line, Term} | Tail]) when is_tuple(Term) ->
    case lists:keysearch(element(1, Term), 1, Tabs) of
	{value, _} ->
	    [Term | collect_data(Tabs, Tail)];
	_Other ->
	    io:format("Object:~p at line ~w unknown\n", [Term,Line]),
	    error(undefined_object)
    end;
collect_data(_Tabs, []) -> [];
collect_data(_Tabs, [H|_T]) ->
    io:format("Object:~p unknown\n", [H]),
    error(undefined_object).

error(What) -> throw({error, What}).

file(File) ->
    case file:open(File, [read]) of
	{ok, Stream} ->
	    Res = read_terms(Stream, File, 1, []),
	    file:close(Stream),
	    Res;
	_Other ->
	    {error, open}
    end.

read_terms(Stream, File, Line, L) ->
    case read_term_from_stream(Stream, File, Line) of
	{ok, Term, NextLine} ->
	    read_terms(Stream, File, NextLine, [Term|L]);
	error ->
	    {error, read};
	eof ->
	    {ok, lists:reverse(L)}
    end.

read_term_from_stream(Stream, File, Line) ->
    R = io:request(Stream, {get_until,'',erl_scan,tokens,[Line]}),
    case R of
	{ok,Toks,EndLine} ->
	    case erl_parse:parse_term(Toks) of
		{ok, Term} ->
		    {ok, {Line, Term}, EndLine};
		{error, {NewLine,Mod,What}} ->
		    Str = Mod:format_error(What),
		    io:format("Error in line:~p of:~p ~s\n",
			      [NewLine, File, Str]),
		    error
	    end;
	{eof,_EndLine} ->
	    eof;
	Other ->
	    io:format("Error1 **~p~n",[Other]),
	    error
    end.

			       
