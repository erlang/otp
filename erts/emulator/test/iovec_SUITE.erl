%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2017-2018. All Rights Reserved.
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

-module(iovec_SUITE).

-export([all/0, suite/0, init_per_suite/1, end_per_suite/1]).

-export([integer_lists/1, binary_lists/1, empty_lists/1, empty_binary_lists/1,
         mixed_lists/1, improper_lists/1, illegal_lists/1, cons_bomb/1,
         sub_binary_lists/1, iolist_to_iovec_idempotence/1,
         iolist_to_iovec_correctness/1, unaligned_sub_binaries/1,
         direct_binary_arg/1]).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 2}}].

all() ->
    [integer_lists, binary_lists, empty_lists, empty_binary_lists, mixed_lists,
     sub_binary_lists, illegal_lists, improper_lists, cons_bomb,
     iolist_to_iovec_idempotence, iolist_to_iovec_correctness,
     unaligned_sub_binaries, direct_binary_arg].

init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    application:stop(os_mon),
    Config.

integer_lists(Config) when is_list(Config) ->
    Variations = gen_variations([I || I <- lists:seq(1, 255)]),
    equivalence_test(fun erlang:iolist_to_iovec/1, Variations).

sub_binary_lists(Config) when is_list(Config) ->
    Parent = <<0:256/unit:8, "gazurka">>,
    <<0:196/unit:8, Child/binary>> = Parent,
    equivalence_test(fun erlang:iolist_to_iovec/1, gen_variations(Child)).

binary_lists(Config) when is_list(Config) ->
    Variations = gen_variations([<<I:8>> || I <- lists:seq(1, 255)]),
    equivalence_test(fun erlang:iolist_to_iovec/1, Variations).

empty_lists(Config) when is_list(Config) ->
    Variations = gen_variations([[] || _ <- lists:seq(1, 256)]),
    equivalence_test(fun erlang:iolist_to_iovec/1, Variations),
    [] = erlang:iolist_to_iovec([]),
    ok.

empty_binary_lists(Config) when is_list(Config) ->
    Variations = gen_variations([<<>> || _ <- lists:seq(1, 8192)]),
    equivalence_test(fun erlang:iolist_to_iovec/1, Variations),
    [] = erlang:iolist_to_iovec(Variations),
    ok.

mixed_lists(Config) when is_list(Config) ->
    Variations = gen_variations([<<>>, lists:seq(1, 40), <<12, 45, 78>>]),
    equivalence_test(fun erlang:iolist_to_iovec/1, Variations).

illegal_lists(Config) when is_list(Config) ->
    BitStrs = gen_variations(["gurka", <<1:1>>, "gaffel"]),
    BadInts = gen_variations(["gurka", 890, "gaffel"]),
    Atoms = gen_variations([gurka, "gaffel"]),
    BadTails = [["test" | 0], ["gurka" | gaffel], ["gaffel" | <<1:1>>]],

    Variations =
        BitStrs ++ BadInts ++ Atoms ++ BadTails,

    illegality_test(fun erlang:iolist_to_iovec/1, Variations).

improper_lists(Config) when is_list(Config) ->
    Variations = [
        [[[[1 | <<2>>] | <<3>>] | <<4>>] | <<5>>],
        [[<<1>>, 2] | <<3, 4, 5>>],
        [1, 2, 3 | <<4, 5>>]
    ],
    equivalence_test(fun erlang:iolist_to_iovec/1, Variations).

cons_bomb(Config) when is_list(Config) ->
    IntBase = gen_variations([I || I <- lists:seq(1, 255)]),
    BinBase = gen_variations([<<I:8>> || I <- lists:seq(1, 255)]),
    MixBase = gen_variations([<<12, 45, 78>>, lists:seq(1, 255)]),

    Variations = gen_variations([IntBase, BinBase, MixBase], 16),
    equivalence_test(fun erlang:iolist_to_iovec/1, Variations).

iolist_to_iovec_idempotence(Config) when is_list(Config) ->
    IntVariations = gen_variations([I || I <- lists:seq(1, 255)]),
    BinVariations = gen_variations([<<I:8>> || I <- lists:seq(1, 255)]),
    MixVariations = gen_variations([<<12, 45, 78>>, lists:seq(1, 255)]),

    Variations = [IntVariations, BinVariations, MixVariations],
    Optimized = erlang:iolist_to_iovec(Variations),

    true = Optimized =:= erlang:iolist_to_iovec(Optimized),
    ok.

iolist_to_iovec_correctness(Config) when is_list(Config) ->
    IntVariations = gen_variations([I || I <- lists:seq(1, 255)]),
    BinVariations = gen_variations([<<I:8>> || I <- lists:seq(1, 255)]),
    MixVariations = gen_variations([<<12, 45, 78>>, lists:seq(1, 255)]),

    Variations = [IntVariations, BinVariations, MixVariations],
    Optimized = erlang:iolist_to_iovec(Variations),

    true = is_iolist_equal(Optimized, Variations),
    ok.

unaligned_sub_binaries(Config) when is_list(Config) ->
    UnalignedBins = [gen_unaligned_binary(I) || I <- lists:seq(32, 4 bsl 10, 512)],
    UnalignedVariations = gen_variations(UnalignedBins),

    Optimized = erlang:iolist_to_iovec(UnalignedVariations),

    true = is_iolist_equal(Optimized, UnalignedVariations),
    ok.

direct_binary_arg(Config) when is_list(Config) ->
    {'EXIT',{badarg, _}} = (catch erlang:iolist_to_iovec(<<1:1>>)),
    [<<1>>] = erlang:iolist_to_iovec(<<1>>),
    [] = erlang:iolist_to_iovec(<<>>),
    ok.

illegality_test(Fun, Variations) ->
    [{'EXIT',{badarg, _}} = (catch Fun(Variation)) || Variation <- Variations],
    ok.

equivalence_test(Fun, [Head | _] = Variations) ->
    %% Check that each variation is equal to the others, and that the sum of
    %% them is equal to the input.
    Comparand = Fun(Head),
    [true = is_iolist_equal(Comparand, Fun(V)) || V <- Variations],
    true = is_iolist_equal(Variations, Fun(Variations)),
    ok.

is_iolist_equal(A, B) ->
    iolist_to_binary(A) =:= iolist_to_binary(B).

gen_unaligned_binary(Size) ->
    Bin0 = << <<I>> || I <- lists:seq(1, Size) >>,
    <<0:3,Bin:Size/binary,31:5>> = id(<<0:3,Bin0/binary,31:5>>),
    Bin.

id(I) -> I.

%% Generates a bunch of lists whose contents will be equal to Base repeated a
%% few times. The lists only differ by their structure, so their reduction to
%% a simpler format should yield the same result.
gen_variations(Base) ->
    gen_variations(Base, 12).
gen_variations(Base, N) ->
    [gen_flat_list(Base, N),
     gen_nested_list(Base, N),
     gen_nasty_list(Base, N)].

gen_flat_list(Base, N) ->
    lists:flatten(gen_nested_list(Base, N)).

gen_nested_list(Base, N) ->
    [Base || _ <- lists:seq(1, N)].

gen_nasty_list(Base, N) ->
    gen_nasty_list_1(gen_nested_list(Base, N), []).
gen_nasty_list_1([], Result) ->
    Result;
gen_nasty_list_1([Head | Base], Result) when is_list(Head) ->
    gen_nasty_list_1(Base, [[Result], [gen_nasty_list_1(Head, [])]]);
gen_nasty_list_1([Head | Base], Result) ->
    gen_nasty_list_1(Base, [[Result], [Head]]).
