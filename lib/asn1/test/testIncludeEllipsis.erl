%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2017. All Rights Reserved.
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
%%
-module(testIncludeEllipsis).

-export([main/1]).

main(_Erule) ->
    %% test the case where there are no extension data in the encoded bytes
    roundtrip_nv('Message', {'Message',3,high}),
    map_roundtrip('Message', #{id=>4,priority=>high}),
    roundtrip_ed('Message', {'Message',3,high}, <<4, 1, 99>>),
    roundtrip_ed('Message', {'Message',3,high}, <<4, 1, 99, 4, 1, 98>>),
    map_roundtrip_ed('Message', #{id=>4,priority=>high}, <<4, 1, 99>>),
    map_roundtrip_ed('Message', #{id=>4,priority=>high}, <<4, 1, 99, 4, 1, 98>>),
    ok.

roundtrip_nv(Type, Value) ->
    roundtrip(Type, Value, undefined, erlang:append_element(Value,asn1_NOVALUE)).

roundtrip_ed(Type, Value, ED) ->
    DecED = dec_ed(ED),
    roundtrip(Type, Value, ED, erlang:append_element(Value,DecED)).

roundtrip(Type, Value, ED, Expected) ->
    %% asn1_test_lib:roundtrip/3 will invoke map_roundtrip/3, which will
    %% not work in this case. Therefore, implement the roundtrip ourselves.
    M = 'IncludeEllipsis',
    {ok,Enc} = M:encode(Type, Value),
    EncEd = if ED == undefined -> Enc;
               true ->
                    <<Tag, Len, Data/binary>> = Enc,
                    <<Tag, (Len + size(ED)), Data/binary, ED/binary>>
            end,
    {ok,Expected} = M:decode(Type, EncEd),
    ok.

map_roundtrip(Type, Value) ->
    map_roundtrip(Type, Value, undefined, Value).

map_roundtrip_ed(Type, Value, ED) ->
    DecED = dec_ed(ED),
    map_roundtrip(Type, Value, ED, Value#{'$ellipsis' => DecED}).

map_roundtrip(Type, Value, ED, Expected) ->
    M = 'maps_IncludeEllipsis',
    Enc = M:encode(Type, Value),
    EncEd = if ED == undefined -> Enc;
               true ->
                    <<Tag, Len, Data/binary>> = Enc,
                    <<Tag, (Len + size(ED)), Data/binary, ED/binary>>
            end,
    Expected = M:decode(Type, EncEd),
    ok.

dec_ed(<<>>) ->
    [];
dec_ed(Bin) ->
    {Dec,Rem} = asn1rt_nif:decode_ber_tlv(Bin),
    [Dec | dec_ed(Rem)].

