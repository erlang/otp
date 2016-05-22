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
-module(format_lib_supp).

%%%---------------------------------------------------------------------
%%% Description:
%%% This module contains generic formatting functions for the SUPPort
%%% tools. 
%%% The main parts are:
%%% 1) print_info.  Prints information tagged by 'header', 'data',
%%%    'table', 'items' and 'newline'.
%%%---------------------------------------------------------------------

%% intermodule exports
-export([print_info/2, print_info/3]).

%% exports for use within module
-export([maxcol/2]).

%%---------------------------------------------------------------------
%% Format is an ordered list of:
%%   {header, HeaderString}
%%   {data, List_Of_KeyValue_tuples}
%%        The KeyValues_tuples will be printed on one line
%%        (if possible); 'Key:     Value'.
%%        Elements in the list may also be single terms, which are
%%        printed as they are.
%%   {table, {TableName, ColumnNames, Columns}}
%%        ColumnNames is a tuple of names for the columns, and
%%        Columns is a list, where each element is a tuple of
%%        data for that column.
%%   {items, {Name, Items}}
%%        Items is a list of KeyValue_tuples. Will be printed as:
%%        'Name:
%%              Key1:     Value1
%%              KeyN:     ValueN'
%%   {newline, N}
%%   Any other format will be ignored.
%% This list is printed in order. If the header clause is present,
%% it must be the first element in the format list.
%% ------------------------------------------------------------------
print_info(Device, Format) ->
    print_info(Device, 79, Format).
print_info(Device, Line, Format) ->
    print_header(Device, Line, Format),
    print_format(Device, Line, Format).

print_header(Device, Line, [{header, Header}|_]) ->
    print_header2(Device, Line, Header);
print_header(Device, Line, _) ->
    print_header2(Device, Line, "").
print_header2(Device, Line, Header) ->
    Format1 = lists:concat(["~n~", Line, ".", Line, "s~n"]),
    Format2 = lists:concat(["~", Line, "c~n"]),
    io:format(Device, Format1, [Header]),
    io:format(Device, Format2, [$=]).
    
print_format(Device, _Line, []) ->
    io:format(Device, '~n', []);
print_format(Device, Line, [{data, Data}|T]) ->
    print_data(Device, Line, Data),
    print_format(Device, Line, T);
print_format(Device, Line, [{table, Table}|T]) ->
    _ = print_table(Device, Line, Table),
    print_format(Device, Line, T);
print_format(Device, Line, [{items, Items}|T]) ->
    print_items(Device, Line, Items),
    print_format(Device, Line, T);
print_format(Device, Line, [{newline, N}|T]) ->
    print_newlines(Device, N),
    print_format(Device, Line, T);
print_format(Device, Line, [_|T]) ->  % ignore any erroneous format.
    print_format(Device, Line, T).

print_data(_Device, _Line, []) -> ok;
print_data(Device, Line, [{Key, Value}|T]) ->
    print_one_line(Device, Line, Key, Value),
    print_data(Device, Line, T);
print_data(Device, Line, [Value|T]) ->
    io:format(Device, "~p~n", [Value]),
    print_data(Device, Line, T).

print_items(Device, Line, {Name, Items}) ->
    print_items(Device, Line, Name, Items).

print_table(Device, Line, {TableName, ColumnNames, Columns}) ->
    print_table(Device, Line, TableName, ColumnNames, Columns).

print_newlines(_Device, 0) -> ok;
print_newlines(Device, N) when N > 0 ->
    io:format(Device, '~n', []),
    print_newlines(Device, N-1).

print_one_line(Device, Line, Key, Value) ->
    StrKey = term_to_string(Key),
    KeyLen = lists:min([length(StrKey), Line]),
    ValueLen = Line - KeyLen,
    Format1 = lists:concat(["~-", KeyLen, s]),
    Format2 = lists:concat(["~", ValueLen, "s~n"]),
    io:format(Device, Format1, [StrKey]),
    Try = term_to_string(Value),
    Length = length(Try),
    if
	Length < ValueLen ->
	    io:format(Device, Format2, [Try]);
	true ->
	    io:format(Device, "~n         ", []),
	    Format3 = lists:concat(["~", Line, ".9p~n"]),
	    io:format(Device, Format3, [Value])
    end.

term_to_string(Value) ->
    lists:flatten(io_lib:format(get_format(Value), [Value])).

get_format(Value) ->
    case misc_supp:is_string(Value) of
	true -> "~s";
	false -> "~p"
    end.

make_list(0, _Elem) -> [];
make_list(N, Elem) -> [Elem|make_list(N-1, Elem)].


%%-----------------------------------------------------------------
%% Items
%%-----------------------------------------------------------------
print_items(Device, Line, Name, Items) ->
    print_one_line(Device, Line, Name, " "),
    print_item_elements(Device, Line, Items).

print_item_elements(_Device, _Line, []) -> ok;
print_item_elements(Device, Line, [{Key, Value}|T]) ->
    print_one_line(Device, Line, lists:concat(["   ", Key]), Value),
    print_item_elements(Device, Line, T).

%%-----------------------------------------------------------------
%% Table handling
%%-----------------------------------------------------------------
extra_space_between_columns() -> 3.

find_max_col([Row | T], ColumnSizes) ->
    find_max_col(T, misc_supp:multi_map({format_lib_supp, maxcol},
					[Row, ColumnSizes]));

find_max_col([], ColumnSizes) -> ColumnSizes.

maxcol(Term, OldMax) ->
    lists:max([length(term_to_string(Term)), OldMax]).

make_column_format(With) ->
    lists:concat(["~", With + extra_space_between_columns(), s]).

is_correct_column_length(_Length, []) -> true;
is_correct_column_length(Length, [Tuple|T]) ->
    case size(Tuple) of
	Length -> is_correct_column_length(Length, T);
	_ -> false
    end;
is_correct_column_length(_, _) -> false.

print_table(Device, Line, TableName, _TupleOfColumnNames, []) ->
    print_one_line(Device, Line, TableName, "<empty table>"),
    io:format(Device, "~n", []);

print_table(Device, Line, TableName, TupleOfColumnNames, ListOfTuples) 
                when is_list(ListOfTuples), is_tuple(TupleOfColumnNames) ->
    case is_correct_column_length(size(TupleOfColumnNames),
				  ListOfTuples) of
	true -> 
	    print_one_line(Device, Line, TableName, " "),
	    ListOfColumnNames = tuple_to_list(TupleOfColumnNames),
	    ListOfLists = lists:map(fun(Tuple) ->
					    tuple_to_list(Tuple)
				    end,
				    ListOfTuples),
	    ColWidths = find_max_col([ListOfColumnNames |
				      ListOfLists],
				     make_list(length(ListOfColumnNames),0)),
	    Format = lists:flatten([lists:map(fun(CW) ->
						      make_column_format(CW)
					      end,
					      ColWidths), "~n"]),
	    io:format(Device, Format, ListOfColumnNames),
	    io:format(Device,
		      lists:concat(['~', extra_space_between_columns(),
				    'c', '~', lists:sum(ColWidths)
				    + (length(ColWidths) - 1)
				    * extra_space_between_columns(),
				    'c~n']), [$ , $-]),
	    lists:foreach(fun(List) ->
				  print_row(List, Device, Format)
			  end,
			  ListOfLists),
	    io:format(Device, '~n', []),
	    true;
	false ->
	    {error, {'a tuple has wrong size',
		     {TableName, TupleOfColumnNames, ListOfTuples}}}
    end.

%%--------------------------------------------------
%% Device MUST be 2nd arg because of extraarg ni foreach...
%%--------------------------------------------------
print_row(Row, Device, Format) ->
    io:format(Device, Format,
	      lists:map(fun(Term) -> term_to_string(Term) end,
			Row)).
