%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2016. All Rights Reserved.
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

-module(diameter_dict_scanner).

%%
%% A scanner for dictionary files of the form expected by yecc.
%%

-export([scan/1,
         format_error/1]).

-export([is_name/1]).

%% -----------------------------------------------------------
%% # scan/1
%% -----------------------------------------------------------

-spec scan(string() | binary())
   -> {ok, [Token]}
    | {error, {string(), string(), Lineno}}
 when Token  :: {word, Lineno, string()}
              | {number, Lineno, non_neg_integer()}
              | {Symbol, Lineno},
      Lineno   :: pos_integer(),
      Symbol   :: '{' | '}'   | '<' | '>' | '[' | ']'
                | '*' | '::=' | ':' | ',' | '-'
                | avp_types
                | avp_vendor_id
                | codecs
                | custom_types
                | define
                | grouped
                | id
                | inherits
                | messages
                | name
                | prefix
                | vendor
                | '$end'
                | code
                | 'answer-message'
                | 'AVP'
                | 'AVP-Header'
                | 'Diameter'
                | 'Diameter-Header'
                | 'Header'
                | 'REQ'
                | 'PXY'
                | 'ERR'.

scan(B)
  when is_binary(B) ->
    scan(binary_to_list(B));
scan(S) ->
    scan(S, {1, []}).

scan(S, {Lineno, Acc}) ->
    case split(S) of
        '$end' = E ->
            {ok, lists:reverse([{E, Lineno} | Acc])};
        {Tok, Rest} ->
            scan(Rest, acc(Tok, Lineno, Acc));
        Reason when is_list(Reason) ->
            {error, {Reason, S, Lineno}}
    end.

%% format_error/1

format_error({Reason, Input, Lineno}) ->
    io_lib:format("~s at line ~p: ~s",
                  [Reason, Lineno, head(Input, [], 20, true)]).

%% is_name/1

is_name([H|T]) ->
    is_alphanum(H) andalso lists:all(fun is_name_ch/1, T).

%% ===========================================================================

head(Str, Acc, N, _)
  when [] == Str;
       0 == N;
       $\r == hd(Str);
       $\n == hd(Str) ->
    lists:reverse(Acc);
head([C|Rest], Acc, N, true = T)  %% skip leading whitespace
  when C == $\s;
       C == $\t;
       C == $\f;
       C == $\v ->
    head(Rest, Acc, N, T);
head([C|Rest], Acc, N, _) ->
    head(Rest, [C|Acc], N-1, false).

acc(endline, Lineno, Acc) ->
    {Lineno + 1, Acc};
acc(T, Lineno, Acc) ->
    {Lineno, [tok(T, Lineno) | Acc]}.

tok({Cat, Sym}, Lineno) ->
    {Cat, Lineno, Sym};
tok(Sym, Lineno) ->
    {Sym, Lineno}.

%% # split/1
%%
%% Output: {Token, Rest} | atom()

%% Finito.
split("") ->
    '$end';

%% Skip comments. This precludes using semicolon for any other purpose.
split([$;|T]) ->
    split(lists:dropwhile(fun(C) -> not is_eol_ch(C) end, T));

%% Beginning of a section.
split([$@|T]) ->
    {Name, Rest} = lists:splitwith(fun is_name_ch/1, T),
    case section(Name) of
        false ->
            "Unknown section";
        'end' ->
            '$end';
        A ->
            {A, Rest}
    end;

split("::=" ++ T) ->
    {'::=', T};

split([H|T])
  when H == ${; H == $};
       H == $<; H == $>;
       H == $[; H == $];
       H == $*; H == $:; H == $,; H == $- ->
    {list_to_atom([H]), T};

%% RFC 3588 requires various names to begin with a letter but 3GPP (for
%% one) abuses this. (eg 3GPP-Charging-Id in TS32.299.)
split([H|_] = L) when $0 =< H, H =< $9 ->
    {P, Rest} = splitwith(fun is_name_ch/1, L),
    Tok = try
              {number, read_int(P)}
          catch
              error:_ ->
                  word(P)
          end,
    {Tok, Rest};

split([H|_] = L) when $a =< H, H =< $z;
                      $A =< H, H =< $Z ->
    {P, Rest} = splitwith(fun is_name_ch/1, L),
    {word(P), Rest};

split([$'|T]) ->
    case lists:splitwith(fun(C) -> not lists:member(C, "'\r\n") end, T) of
        {[_|_] = A, [$'|Rest]} ->
            {{word, A}, Rest};
        {[], [$'|_]} ->
            "Empty string";
        _ -> %% not terminated on same line
            "Unterminated string"
    end;

%% Line ending of various forms.
split([$\r,$\n|T]) ->
    {endline, T};
split([C|T])
  when C == $\r;
       C == $\n ->
    {endline, T};

%% Ignore whitespace.
split([C|T])
  when C == $\s;
       C == $\t;
       C == $\f;
       C == $\v ->
    split(T);

split(_) ->
    "Unexpected character".

%% word/1

%% Reserved words significant in parsing ...
word(S)
  when S == "answer-message";
       S == "code";
       S == "AVP";
       S == "AVP-Header";
       S == "Diameter";
       S == "Diameter-Header";
       S == "Header";
       S == "REQ";
       S == "PXY";
       S == "ERR" ->
    list_to_atom(S);

%% ... or not.
word(S) ->
    {word, S}.

%% section/1

section(N)
  when N == "avp_types";
       N == "avp_vendor_id";
       N == "codecs";
       N == "custom_types";
       N == "define";
       N == "end";
       N == "enum";
       N == "grouped";
       N == "id";
       N == "inherits";
       N == "messages";
       N == "name";
       N == "prefix";
       N == "vendor" ->
    list_to_atom(N);
section(_) ->
    false.

%% read_int/1

read_int([$0,X|S])
  when X == $X;
       X == $x ->
    {ok, [N], []} = io_lib:fread("~16u", S),
    N;

read_int(S) ->
    list_to_integer(S).

%% splitwith/3

splitwith(Fun, [H|T]) ->
    {SH, ST} = lists:splitwith(Fun, T),
    {[H|SH], ST}.

is_eol_ch(C) ->
    C == $\n orelse C == $\r.

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
