%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2003-2025. All Rights Reserved.
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

-module(xmerl_xsd_re).
-moduledoc false.

-export([map/1]). %% api

-export([scan/1]).  %% test

%% map/1
%%
%% Map an XSD 1.0 regular expression to an equivalent PCRE regular
%% expression as understood by re(3).

-spec map(binary()) -> iodata().

map(Bin) ->
    case xmerl_xsd_re_parse:parse(scan(Bin)) of
        {ok, RE} ->
            %% io:format("map RE: ~p\n\n", [RE]),
            RE;
        {error, Reason} ->
            %% io:format("map error reason: \n~p\n\n", [Reason]),
            error({?MODULE, Reason})
    end.

%% scan/1
%%
%% Scanner for XSD 1.0 regular expressions as required by yecc. Just
%% breaks the input into metacharacters, escapes (SingleCharEsc,
%% MultiCharEsc, CatEsc/ComplEsc), digits, and other characters. Scan
%% the entire input in one go since regular expressions aren't
%% expected to be overly huge.

-spec scan(binary()) -> [Tok]
 when Tok :: {Sym, Pos}
           | {Cat, Pos, Chr},
      Sym :: eof
           | '.' | '?' | '*' | '+' | '(' | ')' | '|' | '[' | ']'
           | '{' | '}' | ',' | '-'
           | '^' | '$' | ':',
      Cat :: digit | multi | single | property | other,
      Pos :: non_neg_integer(),
      Chr :: pos_integer().

scan(Bin) ->
    scan(Bin, 0).

%% scan/2

scan(<<>>, N) ->
    [{eof, N}];

scan(<<$\\, C, B/binary>>, N)  %% SingleCharEsc
  when C == $n;
       C == $r;
       C == $t;
       C == $\\;
       C == $|;
       C == $.;
       C == $?;
       C == $*;
       C == $+;
       C == $(;
       C == $);
       C == ${;
       C == $};
       C == $-;
       C == $[;
       C == $];
       C == $^ ->
    [{single, N, C} | scan(B, N+2)];

scan(<<$\\, C, B/binary>>, N)  %% MultiCharEsc
  when C == $s;
       C == $S;
       C == $i;
       C == $I;
       C == $c;
       C == $C;
       C == $d;
       C == $D;
       C == $w;
       C == $W ->
    [{multi, N, C} | scan(B, N+2)];

scan(<<$\\, C, B/binary>>, N)
  when C == $p;
       C == $P ->
    [{property, N, C} | scan(B, N+2)];

scan(<<C/utf8, B/binary>>, N)
  when C /= $\\ ->
    [chr(C, N) | scan(B, N+1)];

scan(B, N) ->
    error({?MODULE, N, B}).

%% chr/2

chr(C, N)
  when C == $.;
       C == $?;
       C == $*;
       C == $+;
       C == $(;
       C == $);
       C == $|;
       C == $[;
       C == $];
       C == ${;
       C == $};
       C == $,;
       C == $-;
       C == $^;
       C == $$;
       C == $: ->
    {list_to_atom([C]), N};

chr(C, N) when $0 =< C, C =< $9 ->
    {digit, N, C};

chr(C, N) ->
    {other, N, C}.
