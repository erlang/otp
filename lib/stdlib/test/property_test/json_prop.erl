%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2024. All Rights Reserved.
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
-module(json_prop).

-include_lib("common_test/include/ct_property_test.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%

prop_string_roundtrip() ->
    ?FORALL(Str, printable_string(), begin
        equals(Str, decode_io(json:encode(Str)))
    end).

prop_integer_roundtrip() ->
    ?FORALL(Int, integer(), begin
        equals(Int, decode_io(json:encode(Int)))
    end).

prop_float_roundtrip() ->
    ?FORALL(Float, float(), begin
        equals(Float, decode_io(json:encode(Float)))
    end).

prop_object_roundtrip() ->
    ?FORALL(Obj, object(), begin
        equals(Obj, decode_io(json:encode(Obj)))
    end).

prop_escape_all() ->
    ?FORALL(Str, printable_string(), begin
        Encoded = iolist_to_binary(encode_escape_all(Str)),
        all_ascii(Encoded) andalso equals(Str, json:decode(Encoded))
    end).

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%

printable_string() ->
    Chars = oneof([
        oneof("\n\r\t\v\b\f\e\d"),
        range(16#20, 16#7E),
        range(16#A0, 16#D7FF),
        range(16#E000, 16#FFFD),
        range(16#10000, 16#10FFFF)
    ]),
    ?LET(L, list(Chars), unicode:characters_to_binary(L)).

object() -> ?SIZED(Size, object(Size)).

object(Size) ->
    FlatTypes = oneof([integer(), float(), printable_string(), boolean(), null]),
    case Size =:= 0 of
        true ->
            FlatTypes;
        false ->
            frequency([
                {50, FlatTypes},
                {1, list(?LAZY(object(Size - 1)))},
                {1, map(printable_string(), ?LAZY(object(Size - 1)))}
            ])
    end.

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

all_ascii(Bin) ->
    lists:all(fun(C) -> C < 128 end, binary_to_list(Bin)).

decode_io(IOList) -> json:decode(iolist_to_binary(IOList)).

encode_escape_all(Term) ->
    Encode = fun
        (Binary, _) when is_binary(Binary) -> json:encode_binary_escape_all(Binary);
        (Other, Encode) -> json:encode_value(Other, Encode)
    end,
    json:encode(Term, Encode).
