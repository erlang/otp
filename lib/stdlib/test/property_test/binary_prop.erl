-module(binary_prop).

-export([prop_hex_encode_2/0]).

-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct_property_test.hrl").

prop_hex_encode_2() ->
    ?FORALL(Data, data(),
        begin
            UpperHex = binary:encode_hex(Data, uppercase),
            LowerHex = binary:encode_hex(Data, lowercase),
            binary:decode_hex(LowerHex) =:= Data andalso
            binary:decode_hex(UpperHex) =:= Data andalso
            check_hex_encoded(Data, UpperHex, LowerHex)
        end).

data() ->
    ?SIZED(Size, resize(Size * 2, binary())).


%% @doc Credit to the <a href="https://github.com/erlang/otp/pull/6297#discussion_r1034771450">comment</a> of @Maria-12648430
-spec check_hex_encoded(Data :: binary(), UpperHex :: binary(), LoweHex :: binary()) -> boolean().
check_hex_encoded(<<I1:4, I2:4, Ins/binary>>, <<U1:8, U2:8, UCs/binary>>, <<L1:8, L2:8, LCs/binary>>) ->
    check_hex_chars_match(I1, U1, L1) andalso
    check_hex_chars_match(I2, U2, L2) andalso
    check_hex_encoded(Ins, UCs, LCs);
check_hex_encoded(<<>>, <<>>, <<>>) ->
    true;
check_hex_encoded(_, _, _) ->
    false.

check_hex_chars_match(X, U, L) when X < 10 ->
    (U =:= $0 + X) andalso (L =:= $0 + X);
check_hex_chars_match(X, U, L) ->
    (U =:= $A + X -10) andalso (L =:= $a + X -10).
