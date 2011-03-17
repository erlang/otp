%%--------------------------------------------------------------------
%% Test case that exposed a bug (bogus warning) in dialyzer_dataflow
%% when refining binaries containing UTF-based segments. Reported by
%% Patrik Nyblom on 4/3/2009 and fixed by Kostis Sagonas on 31/3/2009.
%%--------------------------------------------------------------------

-module(bs_utf8).

-export([doit/2]).

doit(N, Bin) when is_integer(N), N > 0 ->
    count_and_find(Bin, N).

count_and_find(Bin, N) when is_binary(Bin) ->
    cafu(Bin, N, 0, 0, no_pos).

cafu(<<>>, _N, Count, _ByteCount, SavePos) ->
    {Count, SavePos};
cafu(<<_/utf8, Rest/binary>>, 0, Count, ByteCount, _SavePos) ->
    cafu(Rest, -1, Count+1, 0, ByteCount);
cafu(<<_/utf8, Rest/binary>>, N, Count, _ByteCount, SavePos) when N < 0 ->
    cafu(Rest, -1, Count+1, 0, SavePos);
cafu(<<_/utf8, Rest/binary>> = Whole, N, Count, ByteCount, SavePos) ->
    Delta = byte_size(Whole) - byte_size(Rest),
    cafu(Rest, N-1, Count+1, ByteCount+Delta, SavePos);
cafu(_Other, _N, Count, ByteCount, _SavePos) -> % Non Unicode character at end
    {Count, ByteCount}.
