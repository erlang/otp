%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2009. All Rights Reserved.
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

%%

%%% Purpose : Base 64 encoding and decoding.

-module(ssl_base64).

-export([encode/1, encode_split/1, decode/1, join_decode/1]).

-define(st(X,A), ((X-A+256) div 256)).
-define(CHARS, 64).

%% A PEM encoding consists of characters A-Z, a-z, 0-9, +, / and
%% =. Each character encodes a 6 bits value from 0 to 63 (A = 0, / =
%% 63); = is a padding character.
%%

%%
%% encode(Bytes|Binary) -> Chars
%%
%% Take 3 bytes a time (3 x 8 = 24 bits), and make 4 characters out of
%% them (4 x 6 = 24 bits).
%%
encode(Bs) when is_list(Bs) ->
    encode(list_to_binary(Bs));
encode(<<B:3/binary, Bs/binary>>) ->
    <<C1:6, C2:6, C3:6, C4:6>> = B,
    [enc(C1), enc(C2), enc(C3), enc(C4)| encode(Bs)];
encode(<<B:2/binary>>) ->
    <<C1:6, C2:6, C3:6, _:6>> = <<B/binary, 0>>,
    [enc(C1), enc(C2), enc(C3), $=];
encode(<<B:1/binary>>) ->
    <<C1:6, C2:6, _:12>> = <<B/binary, 0, 0>>,
    [enc(C1), enc(C2), $=, $=];
encode(<<>>) ->
    [].

%% 
%% encode_split(Bytes|Binary) -> Lines
%%
%% The encoding is divided into lines separated by <NL>, and each line
%% is precisely 64 characters long (excluding the <NL> characters,
%% except the last line which 64 characters long or shorter. <NL> may
%% follow the last line.
%% 
encode_split(Bs) ->
    split(encode(Bs)).

%%
%% decode(Chars) -> Binary
%%
decode(Cs) ->
    list_to_binary(decode1(Cs)).

decode1([C1, C2, $=, $=]) ->
    <<B1, _:16>> = <<(dec(C1)):6, (dec(C2)):6, 0:12>>, 
    [B1];
decode1([C1, C2, C3, $=]) ->
    <<B1, B2, _:8>> = <<(dec(C1)):6, (dec(C2)):6, (dec(C3)):6, (dec(0)):6>>, 
    [B1, B2];
decode1([C1, C2, C3, C4| Cs]) ->
    Bin = <<(dec(C1)):6, (dec(C2)):6, (dec(C3)):6, (dec(C4)):6>>, 
    [Bin| decode1(Cs)];
decode1([]) ->
    [].

%% 
%% join_decode(Lines) -> Binary
%%
%% Remove <NL> before decoding.
%%
join_decode(Cs) ->    
    decode(join(Cs)).

%% 
%% Locals
%%    

%% enc/1 and dec/1
%%
%% Mapping: 0-25 -> A-Z, 26-51 -> a-z, 52-61 -> 0-9, 62 -> +, 63 -> /
%%
enc(C) ->
    65 + C + 6*?st(C,26) - 75*?st(C,52) -15*?st(C,62) + 3*?st(C,63).

dec(C) ->
    62*?st(C,43) + ?st(C,47) + (C-59)*?st(C,48) - 69*?st(C,65) - 6*?st(C,97).

%% split encoding into lines
%%
split(Cs) ->
    split(Cs, ?CHARS).

split([], _N) ->
    [$\n];
split(Cs, 0) ->
    [$\n| split(Cs, ?CHARS)];
split([C| Cs], N) ->
    [C| split(Cs, N-1)].

%% join lines of encodings
%%
join([$\r, $\n| Cs]) ->
    join(Cs);
join([$\n| Cs]) ->
    join(Cs);
join([C| Cs]) ->
    [C| join(Cs)];
join([]) ->
    [].

