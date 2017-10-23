%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2017. All Rights Reserved.
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
%%%    'items' and 'newline'.
%%%---------------------------------------------------------------------

%% intermodule exports
-export([print_info/2, print_info/3]).

%%---------------------------------------------------------------------
%% Format is an ordered list of:
%%   {header, HeaderString}
%%   {data, List_Of_KeyValue_tuples}
%%        The KeyValues_tuples will be printed on one line
%%        (if possible); 'Key:     Value'.
%%        Elements in the list may also be single terms, which are
%%        printed as they are.
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
    Modifier = misc_supp:modifier(Device),
    io:format(Device, "~"++Modifier++"p~n", [Value]),
    print_data(Device, Line, T);
print_data(Device, _Line, Value) ->
    Modifier = misc_supp:modifier(Device),
    io:format(Device, "~"++Modifier++"p~n", [Value]).

print_items(Device, Line, {Name, Items}) ->
    print_items(Device, Line, Name, Items).

print_newlines(_Device, 0) -> ok;
print_newlines(Device, N) when N > 0 ->
    io:format(Device, '~n', []),
    print_newlines(Device, N-1).

print_one_line(Device, Line, Key, Value) ->
    Modifier = misc_supp:modifier(Device),
    StrKey = term_to_string(Key,Modifier),
    KeyLen = lists:min([length(StrKey), Line]),
    ValueLen = Line - KeyLen,
    Format1 = lists:concat(["~-", KeyLen, Modifier, "s"]),
    Format2 = lists:concat(["~", ValueLen, Modifier, "s~n"]),
    io:format(Device, Format1, [StrKey]),
    Try = term_to_string(Value,Modifier),
    Length = length(Try),
    if
	Length < ValueLen ->
	    io:format(Device, Format2, [Try]);
	true ->
	    io:format(Device, "~n         ", []),
	    Format3 = lists:concat(["~", Line, ".9", Modifier, "p~n"]),
	    io:format(Device, Format3, [Value])
    end.

term_to_string(Value,Modifier) ->
    lists:flatten(io_lib:format(get_format(Value,Modifier), [Value])).

get_format([],_) ->
    "~p";
get_format(Value,Modifier) ->
    case io_lib:printable_list(Value) of
	true -> "~"++Modifier++"s";
	false -> "~"++Modifier++"p"
    end.

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
