%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2011. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%

-module(diameter_spec_scan).

%%
%% Functions used by the spec file parser in diameter_spec_util.
%%

-export([split/1,
         split/2,
         parse/1]).

%%% -----------------------------------------------------------
%%% # parse/1
%%%
%%% Output: list of Token
%%%
%%%         Token = '{' | '}' | '<' | '>' | '[' | ']'
%%%               | '*' | '::=' | ':' | ',' | '-'
%%%               | {name, string()}
%%%               | {tag, atom()}
%%%               | {number, integer() >= 0}
%%%
%%% Tokenize a string. Fails if the string does not parse.
%%% -----------------------------------------------------------

parse(S) ->
    parse(S, []).

%% parse/2

parse(S, Acc) ->
    acc(split(S), Acc).

acc({T, Rest}, Acc) ->
    parse(Rest, [T | Acc]);
acc("", Acc) ->
    lists:reverse(Acc).

%%% -----------------------------------------------------------
%%% # split/2
%%%
%%% Output: {list() of Token, Rest}
%%%
%%% Extract a specified number of tokens from a string. Returns a list
%%% of length less than the specified number if there are less than
%%% this number of tokens to be parsed.
%%% -----------------------------------------------------------

split(Str, N)
  when N >= 0 ->
    split(N, Str, []).

split(0, Str, Acc) ->
    {lists:reverse(Acc), Str};

split(N, Str, Acc) ->
    case split(Str) of
        {T, Rest} ->
            split(N-1, Rest, [T|Acc]);
        "" = Rest ->
            {lists:reverse(Acc), Rest}
    end.

%%% -----------------------------------------------------------
%%% # split/1
%%%
%%% Output: {Token, Rest} | ""
%%%
%%% Extract the next token from a string.
%%% -----------------------------------------------------------

split("" = Rest) ->
    Rest;

split("::=" ++ T) ->
    {'::=', T};

split([H|T])
  when H == ${; H == $};
       H == $<; H == $>;
       H == $[; H == $];
       H == $*; H == $:; H == $,; H == $- ->
    {list_to_atom([H]), T};

split([H|T]) when $A =< H, H =< $Z;
                  $0 =< H, H =< $9 ->
    {P, Rest} = splitwith(fun is_name_ch/1, [H], T),
    Tok = try
              {number, read_int(P)}
          catch
              error:_ ->
                  {name, P}
          end,
    {Tok, Rest};

split([H|T]) when $a =< H, H =< $z ->
    {P, Rest} = splitwith(fun is_name_ch/1, [H], T),
    {{tag, list_to_atom(P)}, Rest};

split([H|T]) when H == $\t;
                  H == $\s;
                  H == $\n ->
    split(T).

%% read_int/1

read_int([$0,X|S])
  when X == $X;
       X == $x ->
    {ok, [N], []} = io_lib:fread("~16u", S),
    N;

read_int(S) ->
    list_to_integer(S).

%% splitwith/3

splitwith(Fun, Acc, S) ->
    split([] /= S andalso Fun(hd(S)), Fun, Acc, S).

split(true, Fun, Acc, [H|T]) ->
    splitwith(Fun, [H|Acc], T);
split(false, _, Acc, S) ->
    {lists:reverse(Acc), S}.

is_name_ch(C) ->
    is_alphanum(C) orelse C == $- orelse C == $_.

is_alphanum(C) ->
    is_lower(C) orelse is_upper(C) orelse is_digit(C).

is_lower(C) ->
    $a =< C andalso C =< $z.

is_upper(C) ->
    $A =< C andalso C =< $Z.

is_digit(C) ->
    $0 =< C andalso C =< $9.
