%%--------------------------------------------------------------------
%% Test case that exposed a bug (bogus warning) in dialyzer_dataflow
%% when refining binaries containing UTF-based segments. Reported by
%% Patrik Nyblom on 4/3/2009 and fixed by Kostis Sagonas on 31/3/2009.
%%--------------------------------------------------------------------

-module(bs_utf8).

-export([doit/2, gh_8383/1]).

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

%% GH-8383: UTF-8 segments did not update the type for the next segment.
%%
%% In the case below, this left the type at the start of the `Rest` segment as
%% the incoming <<_:32>>, causing `X` to be inferred to <<_:32>> and the
%% subsequent match on the empty binary to fail.
gh_8383(_) -> <<>> == gh_8383_1(<<"exit">>).

gh_8383_1(<<_/utf8, Rest/binary>>) -> gh_8383_1(Rest);
gh_8383_1(X) -> X.
