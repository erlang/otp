%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2009. All Rights Reserved.
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

%%% Purpose : some debug utilities

-module(ssl_debug).

-export([unhex/1, hexd/1, hex_data/2, term_data/2, hex_data/4, term_data/4, make_binary/1]).

%% external

hex_data(Name, Data) ->
    io:format("~s\n~s", [Name, hex(Data)]).

term_data(Name, Term) ->
    io:format("~s\n~p\n", [Name, Term]).

hex_data(Name, Data, Mod, Line) ->
    io:format("~w:~p ~s\n~s", [Mod, Line, Name, hex(Data)]).

term_data(Name, Term, Mod, Line) ->
    io:format("~w:~p ~s\n~p\n", [Mod, Line, Name, Term]).

unhex(S) ->
    Lines = string:tokens(S, "\n"),
    H = [unhex(L, []) || L <- Lines],
    list_to_binary(H).

make_binary(Size) ->
    crypto:rand_bytes(Size).

%% internal

is_hex_digit(C) when C >= $0, C =< $9 -> true;
is_hex_digit(C) when C >= $A, C =< $F -> true;
is_hex_digit(C) when C >= $a, C =< $f -> true;
is_hex_digit(_) -> false.

unhex([], Acc) ->
    list_to_binary(lists:reverse(Acc));
unhex([_], Acc) ->
    unhex([], Acc);
unhex([$  | Tl], Acc) ->
    unhex(Tl, Acc);
unhex([D1, D2 | Tl], Acc) ->
    case {is_hex_digit(D1), is_hex_digit(D2)} of
        {true, true} ->
            unhex(Tl, [erlang:list_to_integer([D1, D2], 16) | Acc]);
        _ ->
            unhex([], Acc)
    end.

hexd(B) ->
    io:format("~s\n", [hex(B)]).

hex(B) -> hex(erlang:iolist_to_binary(B), []).

hex_asc(B) ->
    L = binary_to_list(B),
    {hexify(L), asciify(L)}.

hex(<<B:16/binary, Rest/binary>>, Acc) ->
    {HS, AS} = hex_asc(B),
    hex(Rest, ["\n", AS, "  ", HS | Acc]);
hex(<<>>, Acc) ->
    lists:reverse(Acc);
hex(B, Acc) ->
    {HS, AS} = hex_asc(B),
    L = erlang:iolist_size(HS),
    lists:flatten(lists:reverse(Acc, [HS, lists:duplicate(3*16 - L, $ ), "  ", AS, "\n"])).

hexify(L) -> [[hex_byte(B), " "] || B <- L].

hex_byte(B) when B < 16#10 -> ["0", erlang:integer_to_list(B, 16)];
hex_byte(B) -> erlang:integer_to_list(B, 16).

asciify(L) -> [ascii_byte(C) || C <- L].

ascii_byte($") -> $.;
ascii_byte(C) when C < 32; C >= 127 -> $.;
ascii_byte(C) -> C.
