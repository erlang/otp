%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2012. All Rights Reserved.
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
-module(h323test).

-export([run/1]).
-include_lib("test_server/include/test_server.hrl").

run(per_bin) -> run();
run(per)     -> run();
run(_Rules)  -> ok.

run() ->
    alerting(),
    connect(),
    ok.

dec_alerting() ->
    Cs = "0380060008914a0002020120110000000000000000000000000000000000",
    ByteList = hexstr2bytes(Cs),
    asn1_wrapper:decode('H323-MESSAGES','H323-UserInformation',ByteList).

enc_alerting(V) ->
    asn1_wrapper:encode('H323-MESSAGES','H323-UserInformation',V).

alerting() ->
    {ok,V} = dec_alerting(),
    {ok,B} = enc_alerting(V),
    ByteList = lists:flatten(B),
    {ok,V} = asn1_wrapper:decode('H323-MESSAGES','H323-UserInformation',ByteList).


dec_connect() ->
    Cs = "02c0060008914a00020088e1293a04a322c0b500534c164d6963726f736f6674ae204e65744d656474696e67ae0003332e3000001689edc5bf23d3118c2d00c04f4b1cd00900110000000000000000000000000000000000",
    ByteList = hexstr2bytes(Cs),
    asn1_wrapper:decode('H323-MESSAGES','H323-UserInformation',ByteList).

enc_connect(V) ->
    asn1_wrapper:encode('H323-MESSAGES','H323-UserInformation',V).

connect() ->
    {ok,V} = dec_connect(),
    {ok,B} = enc_connect(V),
    ByteList = lists:flatten(B),
    {ok,V} = asn1_wrapper:decode('H323-MESSAGES','H323-UserInformation',ByteList).

hexstr2bytes([D1,D2|T]) ->
    [dig2num(D1)*16+dig2num(D2)|hexstr2bytes(T)];
hexstr2bytes([]) ->
    [].

dig2num(D) when D >= $0, D =< $9 -> D - $0;
dig2num(D) when D >= $a, D =< $f -> 10 + D - $a;
dig2num(D) when D >= $A, D =< $F -> 10 + D - $A.
