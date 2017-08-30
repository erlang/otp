%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2016. All Rights Reserved.
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
-module(edlin_expand).

%% a default expand function for edlin, expanding modules and functions

-export([expand/1, format_matches/1]).

-import(lists, [reverse/1, nthtail/2, prefix/2]).

%% expand(CurrentBefore) ->
%%	{yes, Expansion, Matches} | {no, Matches}
%%  Try to expand the word before as either a module name or a function
%%  name. We can handle white space around the seperating ':' but the
%%  function name must be on the same line. CurrentBefore is reversed
%%  and over_word/3 reverses the characters it finds. In certain cases
%%  possible expansions are printed.
expand(Bef0) ->
    {Bef1,Word,_} = edlin:over_word(Bef0, [], 0),
    case over_white(Bef1, [], 0) of
 	{[$:|Bef2],_White,_Nwh} ->
 	    {Bef3,_White1,_Nwh1} = over_white(Bef2, [], 0),
 	    {_,Mod,_Nm} = edlin:over_word(Bef3, [], 0),
 	    expand_function_name(Mod, Word);
 	{_,_,_} ->
 	    expand_module_name(Word)
    end.

expand_module_name(Prefix) ->
    match(Prefix, code:all_loaded(), ":").

expand_function_name(ModStr, FuncPrefix) ->
    case to_atom(ModStr) of
	{ok, Mod} ->
	    case erlang:module_loaded(Mod) of
		true ->
		    L = Mod:module_info(),
		    case lists:keyfind(exports, 1, L) of
			{_, Exports} ->
			    match(FuncPrefix, Exports, "(");
			_ ->
			    {no, [], []}
		    end;
		false ->
		    {no, [], []}
	    end;
	error ->
	    {no, [], []}
    end.

%% if it's a quoted atom, atom_to_list/1 will do the wrong thing.
to_atom(Str) ->
    case erl_scan:string(Str) of
	{ok, [{atom,_,A}], _} ->
	    {ok, A};
	_ ->
	    error
    end.

match(Prefix, Alts, Extra0) ->
    Len = length(Prefix),
    Matches = lists:sort(
		[{S, A} || {H, A} <- Alts,
			   prefix(Prefix, S=hd(io_lib:fwrite("~w",[H])))]),
    case longest_common_head([N || {N, _} <- Matches]) of
 	{partial, []} ->
 	    {no, [], Matches}; % format_matches(Matches)};
 	{partial, Str} ->
 	    case nthtail(Len, Str) of
 		[] ->
		    {yes, [], Matches}; % format_matches(Matches)};
 		Remain ->
 		    {yes, Remain, []}
 	    end;
 	{complete, Str} ->
	    Extra = case {Extra0,Matches} of
			{"(",[{Str,0}]} -> "()";
			{_,_} -> Extra0
		    end,
	    {yes, nthtail(Len, Str) ++ Extra, []};
 	no ->
 	    {no, [], []}
    end.

%% Return the list of names L in multiple columns.
format_matches(L) ->
    S = format_col(lists:sort(L), []),
    ["\n" | S].

format_col([], _) -> [];
format_col(L, Acc) -> format_col(L, field_width(L), 0, Acc).

format_col(X, Width, Len, Acc) when Width + Len > 79 ->
    format_col(X, Width, 0, ["\n" | Acc]);
format_col([A|T], Width, Len, Acc0) ->
    H = case A of
 	    %% If it's a tuple {string(), integer()}, we assume it's an
 	    %% arity, and meant to be printed.
	    {H0, I} when is_integer(I) ->
		H0 ++ "/" ++ integer_to_list(I);
	    {H1, _} -> H1;
 	    H2 -> H2
 	end,
    Acc = [io_lib:format("~-*s", [Width,H]) | Acc0],
    format_col(T, Width, Len+Width, Acc);
format_col([], _, _, Acc) ->
    lists:reverse(Acc, "\n").

field_width(L) -> field_width(L, 0).

field_width([{H,_}|T], W) ->
    case length(H) of
 	L when L > W -> field_width(T, L);
 	_ -> field_width(T, W)
    end;
field_width([H|T], W) ->
    case length(H) of
 	L when L > W -> field_width(T, L);
 	_ -> field_width(T, W)
    end;
field_width([], W) when W < 40 ->
    W + 4;
field_width([], _) ->
    40.

longest_common_head([]) ->
    no;
longest_common_head(LL) ->
    longest_common_head(LL, []).

longest_common_head([[]|_], L) ->
    {partial, reverse(L)};
longest_common_head(LL, L) ->
    case same_head(LL) of
 	true ->
 	    [[H|_]|_] = LL,
 	    LL1 = all_tails(LL),
 	    case all_nil(LL1) of
 		false ->
 		    longest_common_head(LL1, [H|L]);
 		true ->
 		    {complete, reverse([H|L])}
 	    end;
 	false ->
 	    {partial, reverse(L)}
    end.

same_head([[H|_]|T1]) -> same_head(H, T1).

same_head(H, [[H|_]|T]) -> same_head(H, T);
same_head(_, [])        -> true;
same_head(_, _)         -> false.

all_tails(LL) -> all_tails(LL, []).

all_tails([[_|T]|T1], L) -> all_tails(T1, [T|L]);
all_tails([], L)         -> L.

all_nil([]) -> true;
all_nil([[] | Rest]) -> all_nil(Rest);
all_nil(_) -> false.

%% over_white(Chars, InitialStack, InitialCount) ->
%%    {RemainingChars,CharStack,Count}.

over_white([$\s|Cs], Stack, N) ->
    over_white(Cs, [$\s|Stack], N+1);
over_white([$\t|Cs], Stack, N) ->
    over_white(Cs, [$\t|Stack], N+1);
over_white(Cs, Stack, N) when is_list(Cs) ->
    {Cs,Stack,N}.
