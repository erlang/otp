-module(iolist).

%% A small part of beam_asm.

-export([encode/2]).

-spec encode(non_neg_integer(), integer()) -> iolist().

encode(Tag, N) when Tag >= 0, N < 0 ->
    encode1(Tag, negative_to_bytes(N));
encode(Tag, N) when Tag >= 0, N < 16 ->
    (N bsl 4) bor Tag; % not in the specification
encode(Tag, N) when Tag >= 0, N < 16#800  ->
    [((N bsr 3) band 2#11100000) bor Tag bor 2#00001000, N band 16#ff];
encode(Tag, N) when Tag >= 0 ->
    encode1(Tag, to_bytes(N)).

encode1(Tag, Bytes) ->
    case iolist_size(Bytes) of
	Num when 2 =< Num, Num =< 8 ->
	    [((Num-2) bsl 5) bor 2#00011000 bor Tag| Bytes];
	Num when 8 < Num ->
	    [2#11111000 bor Tag, encode(0, Num-9)| Bytes]
    end.

to_bytes(N) ->
    Bin = binary:encode_unsigned(N),
    case Bin of
	<<0:1,_/bits>> -> Bin;
	<<1:1,_/bits>> -> [0,Bin]
    end.

negative_to_bytes(N) when N >= -16#8000 ->
    <<N:16>>;
negative_to_bytes(N) ->
    Bytes = byte_size(binary:encode_unsigned(-N)),
    Bin = <<N:Bytes/unit:8>>,
    case Bin of
	<<0:1,_/bits>> -> [16#ff,Bin];
	<<1:1,_/bits>> -> Bin
    end.
