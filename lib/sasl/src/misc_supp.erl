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
-module(misc_supp).

%%%---------------------------------------------------------------------
%%% Description:
%%% This module contains MISCellaneous routines for the SUPPort tools.
%%% 1) The function format_pdict/3 is called by every process that
%%%    wants to format its process dictionary.  
%%% 2) Very generic functions such as, multi_map, is_string...
%%%
%%% This module is a part of the BOS.  (format_pdict is called from
%%% init, memsup, disksup (but not the fileserver since it
%%% formats its pdict its own way).)
%%%---------------------------------------------------------------------

-export([format_pdict/3, format_tuples/2, assq/2, passq/2, is_string/1, 
	 multi_map/2]).

%%-----------------------------------------------------------------
%% Uses format_tuples to format the data in process dictionary.
%% This function is called from format_status_info by several modules
%% that want to format its process dictionary.
%% Args: Exclude is: list of atoms to exclude
%%-----------------------------------------------------------------
format_pdict(normal, _PDict, _Exclude) ->
    [];
format_pdict(all, PDict, Exclude) ->
    case format_tuples(PDict, ['$sys_dict$' | Exclude]) of
        [] -> [];
        Data -> [{newline, 1} | Data]
    end.


%%-----------------------------------------------------------------
%% Format all Key value tuples except for the Keys in the
%% Exclude list.
%%-----------------------------------------------------------------
format_tuples(KeyValues, Exclude) ->
    case format_tuples(KeyValues, Exclude, []) of
	[] -> [];
	Data -> [{data, Data}]
    end.
format_tuples([], _Exclude, Res) -> Res;
format_tuples([{Key, Value} | T], Exclude, Res) ->
    case lists:member(Key, Exclude) of
	true ->
	    format_tuples(T, Exclude, Res);
	false ->
	    format_tuples(T, Exclude, [{Key, Value} | Res])
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% "Very" generic misc stuff:
%%--------------------------------------------------

assq(Key, List) ->
    case lists:keysearch(Key, 1, List) of
	{value, {Key, Val}} -> {value, Val};
	_ -> false
    end.

%% Primitive assq. Use to get items from a process dictionary list.
passq(Key, List) ->
    case lists:keysearch(Key, 1, List) of
	{value, {Key, Val}} -> Val;
	_ -> undefined
    end.

%% This one doesn't treat [] as a string (as io_lib:char_list)
is_string([]) -> false;
is_string(X) -> is_string_2(X).

is_string_2([]) -> true;
is_string_2([H|T]) when is_integer(H), H >= $ , H =< 255 ->
    is_string_2(T);
is_string_2(_) -> false.

%%-----------------------------------------------------------------
%% Pre: ListOfLists is a list of N lists, each of length M.
%%      Func is a function of arity N.
%% Returns: A list of length M where element Y is the result of
%%          applying Func on [Elem(Y, List1), ..., Elem(Y, ListN)].
%%-----------------------------------------------------------------
multi_map(_Func, [[] | _ListOfLists]) -> [];
multi_map(Func, ListOfLists) ->
    [apply(Func, lists:map(fun(List) -> hd(List) end, ListOfLists)) |
     multi_map(Func,
	       lists:map(fun(List) -> tl(List) end, ListOfLists))].
