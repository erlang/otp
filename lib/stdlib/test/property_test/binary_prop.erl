%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2021-2023. All Rights Reserved.
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
-module(binary_prop).

-include_lib("common_test/include/ct_property_test.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%

%% --- at/2 -----------------------------------------------------------
prop_at() ->
    ?FORALL(
        {Bin, Pos, Byte},
        ?LET(
            {B1, Bt, B2},
            {binary(), byte(), binary()},
            {<<B1/binary, Bt, B2/binary>>, byte_size(B1), Bt}
        ),
        Byte =:= binary:at(Bin, Pos)
    ).

prop_at_invalid_index() ->
    ?FORALL(
        {Bin, Pos},
        ?LET(
            B,
            binary(),
            {B, gen_index_invalid(B)}
        ),
        expect_error(fun binary:at/2, [Bin, Pos])
    ).

prop_at_invalid_subject() ->
    ?FORALL(
        Bin,
        gen_subject_invalid(),
        expect_error(fun binary:at/2, [Bin, 0])
    ).

%% --- bin_to_list/1 --------------------------------------------------
prop_bin_to_list_1() ->
    ?FORALL(
        {Bin, List},
        ?LET(
            L,
            list(byte()),
            {<< <<Bt>> || Bt <- L >>, L}
        ),
        List =:= binary:bin_to_list(Bin)
    ).

%% --- bin_to_list/2,3 ------------------------------------------------
prop_bin_to_list_2_3() ->
    ?FORALL(
        {Bin, List, {Pos, Len}=PosLen},
        ?LET(
            {L1, L, L2},
            {list(byte()), list(byte()), list(byte())},
            {<< <<Bt>> || Bt <- L1 ++ L ++ L2 >>, L, {length(L1), length(L)}}
        ),
        List =:= binary:bin_to_list(Bin, PosLen) andalso
        List =:= binary:bin_to_list(Bin, Pos, Len)
    ).

prop_bin_to_list_2_3_invalid_range() ->
    ?FORALL(
        {Bin, {Pos, Len}=PosLen},
        ?LET(
            B,
            binary(),
            {B, gen_part_invalid(B)}
        ),
        expect_error(fun binary:bin_to_list/2, [Bin, PosLen]) andalso
        expect_error(fun binary:bin_to_list/3, [Bin, Pos, Len])
    ).

prop_bin_to_list_invalid_subject() ->
    ?FORALL(
        Bin,
        gen_subject_invalid(),
        expect_error(fun binary:bin_to_list/1, [Bin]) andalso
        expect_error(fun binary:bin_to_list/2, [Bin, {0, 0}]) andalso
        expect_error(fun binary:bin_to_list/3, [Bin, 0, 0])
    ).

%% --- compile_pattern/1 ----------------------------------------------
prop_compile_pattern() ->
    ?FORALL(
        P,
        gen_patterns(),
        is_tuple(binary:compile_pattern(P))
    ).

prop_compile_pattern_invalid_pattern() ->
    ?FORALL(
        P,
        gen_patterns_invalid(),
        expect_error(fun binary:compile_pattern/1, [P])
    ).

%% --- copy/1,2 -------------------------------------------------------
prop_copy() ->
    ?FORALL(
        {Bin, N},
        {binary(), ?SUCHTHAT(N, non_neg_integer(), N =/= 1)},
        begin
            Copy1 = binary:copy(Bin),
            Copy2 = binary:copy(Bin, 1),
            Bin =:= Copy1 andalso
            not erts_debug:same(Bin, Copy1) andalso
            Bin =:= Copy2 andalso
            not erts_debug:same(Bin, Copy2) andalso
            << <<Bin/binary>> || _ <- lists:seq(1, N) >> =:= binary:copy(Bin, N)
        end
    ).

prop_copy_2_invalid_n() ->
    ?FORALL(
        {Bin, N},
        {binary(), neg_integer()},
        expect_error(fun binary:copy/2, [Bin, N])
    ).

prop_copy_invalid_subject() ->
    ?FORALL(
        {Bin, N},
        {gen_subject_invalid(), non_neg_integer()},
        expect_error(fun binary:copy/1, [Bin]) andalso
        expect_error(fun binary:copy/2, [Bin, N])
    ).

%% --- decode_hex/1 ---------------------------------------------------
prop_decode_hex() ->
    ?FORALL(
        {BR, BE},
        ?LET(
            L,
            list({oneof([lower, upper]), range(16#0, 16#f)}),
            lists:foldl(
                fun
                    ({_, Nib}, {AccR, AccE}) when Nib < 10 ->
                        {<<AccR/bitstring, Nib:4>>, <<AccE/binary, ($0 + Nib)>>};
                    ({lower, Nib}, {AccR, AccE}) ->
                        {<<AccR/bitstring, Nib:4>>, <<AccE/binary, ($a + Nib - 10)>>};
                    ({upper, Nib}, {AccR, AccE}) ->
                        {<<AccR/bitstring, Nib:4>>, <<AccE/binary, ($A + Nib - 10)>>}
                end,
                {<<>>, <<>>},
                case length(L) rem 2 of
                    0 -> L;
                    1 -> tl(L)
                end
            )
        ),
        BR =:= binary:decode_hex(BE)
    ).

prop_decode_hex_invalid_chars() ->
    ?FORALL(
        Bin,
        ?SUCHTHAT(B, binary(), not is_hex_bin(B)),
        expect_error(fun binary:decode_hex/1, [Bin])
    ).

prop_decode_hex_invalid_subject() ->
    ?FORALL(
        Bin,
        gen_subject_invalid(),
        expect_error(fun binary:decode_hex/1, [Bin])
    ).

%% --- decode_unsigned/1,2 --------------------------------------------
prop_decode_unsigned() ->
    ?FORALL(
        Bin,
        binary(),
        begin
            Size = bit_size(Bin),
            <<Big:Size/integer-unsigned-big>> = Bin,
            <<Little:Size/integer-unsigned-little>> = Bin,
            Big =:= binary:decode_unsigned(Bin) andalso
            Big =:= binary:decode_unsigned(Bin, big) andalso
            Little =:= binary:decode_unsigned(Bin, little)
        end
    ).

prop_decode_unsigned_2_invalid_endianness() ->
    ?FORALL(
        {Bin, Endianness},
        {binary(), ?SUCHTHAT(E, ct_proper_ext:safe_any(), E =/= big andalso E =/= little)},
        expect_error(fun binary:decode_unsigned/2, [Bin, Endianness])
    ).

prop_decode_unsigned_invalid_subject() ->
    ?FORALL(
        Bin,
        gen_subject_invalid(),
        expect_error(fun binary:decode_unsigned/1, [Bin]) andalso
        expect_error(fun binary:decode_unsigned/2, [Bin, big]) andalso
        expect_error(fun binary:decode_unsigned/2, [Bin, little])
    ).

%% --- encode_hex/1,2 -------------------------------------------------
prop_encode_hex() ->
    ?FORALL(
        Bin,
        binary(),
        begin
            LowerHex = binary:encode_hex(Bin, lowercase),
            UpperHex = binary:encode_hex(Bin, uppercase),
            UpperHex =:= binary:encode_hex(Bin) andalso
            Bin =:= binary:decode_hex(LowerHex) andalso
            Bin =:= binary:decode_hex(UpperHex) andalso
            check_hex_encoded(Bin, UpperHex, LowerHex)
        end
    ).

prop_encode_hex_2_invalid_case() ->
    ?FORALL(
        {Bin, Case},
        {binary(), ?SUCHTHAT(C, ct_proper_ext:safe_any(), C =/= lowercase andalso C =/= uppercase)},
        expect_error(fun binary:encode_hex/2, [Bin, Case])
    ).

prop_encode_hex_invalid_subject() ->
    ?FORALL(
        Bin,
        gen_subject_invalid(),
        expect_error(fun binary:encode_hex/1, [Bin]) andalso
        expect_error(fun binary:encode_hex/2, [Bin, lowercase]) andalso
        expect_error(fun binary:encode_hex/2, [Bin, uppercase])
    ).

%% --- encode_unsigned/1,2 --------------------------------------------
prop_encode_unsigned() ->
    ?FORALL(
        I,
        non_neg_integer(),
        begin
            Size = max(8, int_bitsize(I)),
            Big = <<I:Size/integer-unsigned-big>>,
            Little = <<I:Size/integer-unsigned-little>>,
            Big =:= binary:encode_unsigned(I) andalso
            Big =:= binary:encode_unsigned(I, big) andalso
            Little =:= binary:encode_unsigned(I, little)
        end
    ).

prop_encode_unsigned_invalid_integer() ->
    ?FORALL(
        I,
        neg_integer(),
        expect_error(fun binary:encode_unsigned/1, [I]) andalso
        expect_error(fun binary:encode_unsigned/2, [I, big]) andalso
        expect_error(fun binary:encode_unsigned/2, [I, little])
    ).

prop_encode_unsigned_2_invalid_endianness() ->
    ?FORALL(
        {I, Endianness},
        {non_neg_integer(), ?SUCHTHAT(E, ct_proper_ext:safe_any(), E =/= big andalso E =/= little)},
        expect_error(fun binary:encode_unsigned/2, [I, Endianness])
    ).

%% --- first/1 --------------------------------------------------------
prop_first() ->
    ?FORALL(
        {Bin, Byte},
        ?LET(
            {B, Bt},
            {binary(), byte()},
            {<<Bt, B/binary>>, Bt}
        ),
        Byte =:= binary:first(Bin)
    ).

prop_first_invalid_subject() ->
    ?FORALL(
        Bin,
        oneof([<<>>, gen_subject_invalid()]),
        expect_error(fun binary:first/1, [Bin])
    ).

%% --- last/1 ---------------------------------------------------------
prop_last() ->
    ?FORALL(
        {Bin, Byte},
        ?LET(
            {B, Bt},
            {binary(), byte()},
            {<<B/binary, Bt>>, Bt}
        ),
        Byte =:= binary:last(Bin)
    ).

prop_last_invalid_subject() ->
    ?FORALL(
        Bin,
        oneof([<<>>, gen_subject_invalid()]),
        expect_error(fun binary:last/1, [Bin])
    ).

%% --- list_to_bin/1 --------------------------------------------------
prop_list_to_bin() ->
    ?FORALL(
        {List, Bin},
        ?LET(
            B,
            binary(),
            {[Bt || <<Bt>> <= B], B}
        ),
        Bin =:= binary:list_to_bin(List)
    ).

prop_list_to_bin_invalid_bytes() ->
    ?FORALL(
        List,
        ?SUCHTHAT(
            L,
            non_empty(list(integer())),
            lists:any(fun(I) -> I < 16#00 orelse I > 16#ff end, L)
        ),
        expect_error(fun binary:list_to_bin/1, [List])
    ).

%% --- longest_common_prefix/1 ----------------------------------------
prop_longest_common_prefix() ->
    ?FORALL(
        Bins,
        ?LET(
            {C, Bs},
            {binary(), non_empty(list(binary()))},
            [<<C/binary, B/binary>> || B <- Bs]
        ),
        find_longest_common_prefix(Bins) =:= binary:longest_common_prefix(Bins)
    ).

prop_longest_common_prefix_invalid_subject() ->
    ?FORALL(
        Bins,
        gen_subjects_invalid(),
        expect_error(fun binary:longest_common_prefix/1, [Bins])
    ).

%% --- longest_common_suffix/1 ----------------------------------------
prop_longest_common_suffix() ->
    ?FORALL(
        Bins,
        ?LET(
            {C, Bs},
            {binary(), non_empty(list(binary()))},
            [<<B/binary, C/binary>> || B <- Bs]
        ),
        find_longest_common_suffix(Bins) =:= binary:longest_common_suffix(Bins)
    ).

prop_longest_common_suffix_invalid_subject() ->
    ?FORALL(
        Bins,
        gen_subjects_invalid(),
        expect_error(fun binary:longest_common_suffix/1, [Bins])
    ).

%% --- match/2,3 --------------------------------------------------------
prop_match() ->
    ?FORALL(
        {Bin, Pattern, Opts},
        ?LET(
            B,
            binary(),
            {B, gen_patterns(B), gen_opts([], [gen_scope_opt(B)])}
        ),
        begin
            Match1 = binary:match(Bin, Pattern),
            Match2 = binary:match(Bin, Pattern, Opts),
            Match1 =:= binary:match(Bin, binary:compile_pattern(Pattern)) andalso
            Match1 =:= find_match(Bin, Pattern) andalso
            Match2 =:= binary:match(Bin, binary:compile_pattern(Pattern), Opts) andalso
            Match2 =:= find_match(Bin, Pattern, Opts)
        end
    ).

prop_match_3_invalid_scope() ->
    ?FORALL(
        {Bin, Pattern, Opts},
        ?LET(
            B,
            binary(),
            {B, gen_patterns(), gen_opts([gen_scope_invalid_opt(B)], [])}
        ),
        expect_error(fun binary:match/3, [Bin, Pattern, Opts]) andalso
        expect_error(fun binary:match/3, [Bin, binary:compile_pattern(Pattern), Opts])
    ).

prop_match_invalid_pattern() ->
    ?FORALL(
        {Bin, Pattern, Opts},
        ?LET(
            B,
            binary(),
            {B, gen_patterns_invalid(), gen_opts([], [gen_scope_opt(B)])}
        ),
        expect_error(fun binary:match/2, [Bin, Pattern]) andalso
        expect_error(fun binary:match/3, [Bin, Pattern, Opts])
    ).

prop_match_invalid_subject() ->
    ?FORALL(
        {Bin, Pattern, Opts},
        {gen_subject_invalid(), gen_patterns(), gen_opts([], [gen_scope_opt(<<>>)])},
        expect_error(fun binary:match/2, [Bin, Pattern]) andalso
        expect_error(fun binary:match/3, [Bin, Pattern, Opts])
    ).

%% --- matches/2,3 ------------------------------------------------------
prop_matches() ->
    ?FORALL(
        {Bin, Pattern, Opts},
        ?LET(
            B,
            binary(),
            {B, gen_patterns(B), gen_opts([], [gen_scope_opt(B)])}
        ),
        begin
            Match1 = lists:sort(binary:matches(Bin, Pattern)),
            Match2 = lists:sort(binary:matches(Bin, Pattern, Opts)),
            Match1 =:= lists:sort(binary:matches(Bin, binary:compile_pattern(Pattern))) andalso
            Match1 =:= lists:sort(find_matches(Bin, Pattern)) andalso
            Match2 =:= lists:sort(binary:matches(Bin, binary:compile_pattern(Pattern), Opts)) andalso
            Match2 =:= lists:sort(find_matches(Bin, Pattern, Opts))
        end
    ).

prop_matches_3_invalid_scope() ->
    ?FORALL(
        {Bin, Pattern, Opts},
        ?LET(
            B,
            binary(),
            {B, gen_patterns(), gen_opts([gen_scope_invalid_opt(B)], [])}
        ),
        expect_error(fun binary:matches/3, [Bin, Pattern, Opts]) andalso
        expect_error(fun binary:matches/3, [Bin, binary:compile_pattern(Pattern), Opts])
    ).

prop_matches_invalid_pattern() ->
    ?FORALL(
        {Bin, Pattern, Opts},
        ?LET(
            B,
            binary(),
            {B, gen_patterns_invalid(), gen_opts([], [gen_scope_opt(B)])}
        ),
        expect_error(fun binary:matches/2, [Bin, Pattern]) andalso
        expect_error(fun binary:matches/3, [Bin, Pattern, Opts])
    ).

prop_matches_invalid_subject() ->
    ?FORALL(
        {Bin, Pattern, Opts},
        {gen_subject_invalid(), gen_patterns(), gen_opts([], [gen_scope_opt(<<>>)])},
        expect_error(fun binary:matches/2, [Bin, Pattern]) andalso
        expect_error(fun binary:matches/3, [Bin, Pattern, Opts])
    ).

%% --- part/2,3 -------------------------------------------------------
prop_part() ->
    ?FORALL(
        {Bin, {Pos, Len}=PosLen},
        ?LET(
            B,
            binary(),
            {B, gen_part(B)}
        ),
        begin
            {_, Part, _} = part_split(Bin, PosLen),
            Part =:= binary:part(Bin, PosLen) andalso
            Part =:= binary:part(Bin, Pos, Len) andalso
            Part =:= erlang:binary_part(Bin, PosLen) andalso
            Part =:= erlang:binary_part(Bin, Pos, Len)
        end
    ).

prop_part_invalid_range() ->
    ?FORALL(
        {Bin, {Pos, Len}=PosLen},
        ?LET(
            B,
            binary(),
            {B, gen_part_invalid(B)}
        ),
        expect_error(fun binary:part/2, [Bin, PosLen]) andalso
        expect_error(fun binary:part/3, [Bin, Pos, Len]) andalso
	expect_error(fun erlang:binary_part/2, [Bin, PosLen]) andalso
	expect_error(fun erlang:binary_part/3, [Bin, Pos, Len])
    ).

prop_part_invalid_subject() ->
    ?FORALL(
        {Bin, {Pos, Len}=PosLen},
        {gen_subject_invalid(), gen_part(<<>>)},
        expect_error(fun binary:part/2, [Bin, PosLen]) andalso
        expect_error(fun binary:part/3, [Bin, Pos, Len]) andalso
	expect_error(fun erlang:binary_part/2, [Bin, PosLen]) andalso
	expect_error(fun erlang:binary_part/3, [Bin, Pos, Len])
    ).

%% --- replace/3,4 ------------------------------------------------------
prop_replace() ->
    ?FORALL(
        {Bin, Pattern, Replacement, Opts},
        ?LET(
            {B, R},
            {binary(), gen_replacement()},
            {B, gen_patterns(B), R, gen_opts([], [global, gen_scope_opt(B), gen_insert_replaced_opt(R)])}
        ),
        begin
            Replaced1 = binary:replace(Bin, Pattern, Replacement),
            Replaced2 = binary:replace(Bin, Pattern, Replacement, Opts),
            Replaced1 =:= binary:replace(Bin, binary:compile_pattern(Pattern), Replacement) andalso
            Replaced1 =:= do_replace(Bin, Pattern, Replacement) andalso
            Replaced2 =:= binary:replace(Bin, binary:compile_pattern(Pattern), Replacement, Opts) andalso
            Replaced2 =:= do_replace(Bin, Pattern, Replacement, Opts)
        end
    ).

prop_replace_4_invalid_scope() ->
    ?FORALL(
        {Bin, Pattern, Replacement, Opts},
        ?LET(
            {B, R},
            {binary(), gen_replacement()},
            {B, gen_patterns(B), R, gen_opts([gen_scope_invalid_opt(B)], [global, gen_insert_replaced_opt(R)])}
        ),
        expect_error(fun binary:replace/4, [Bin, Pattern, Replacement, Opts]) andalso
        expect_error(fun binary:replace/4, [Bin, binary:compile_pattern(Pattern), Replacement, Opts])
    ).

prop_replace_4_invalid_insert_replaced() ->
    ?FORALL(
        {Bin, Pattern, Replacement, Opts},
        ?LET(
            {B, R},
            {binary(), binary()},
            {B, gen_patterns(B), R, gen_opts([gen_insert_replaced_invalid_opt(R)], [global, gen_scope_opt(B)])}
        ),
        expect_error(fun binary:replace/4, [Bin, Pattern, Replacement, Opts]) andalso
        expect_error(fun binary:replace/4, [Bin, binary:compile_pattern(Pattern), Replacement, Opts])
    ).

prop_replace_invalid_pattern() ->
    ?FORALL(
        {Bin, Pattern, Replacement, Opts},
        ?LET(
            {B, R},
            {binary(), gen_replacement()},
            {B, gen_patterns_invalid(), R, gen_opts([], [global, gen_scope_opt(B), gen_insert_replaced_opt(R)])}
        ),
        expect_error(fun binary:replace/3, [Bin, Pattern, Replacement]) andalso
        expect_error(fun binary:replace/4, [Bin, Pattern, Replacement, Opts])
    ).

prop_replace_invalid_replacement() ->
    ?FORALL(
        {Bin, Pattern, Replacement, Opts},
        ?LET(
            B,
            binary(),
            {B, gen_patterns(B), gen_replacement_invalid(), gen_opts([], [global, gen_scope_opt(B), gen_insert_replaced_opt(<<>>)])}
        ),
        expect_error(fun binary:replace/3, [Bin, Pattern, Replacement]) andalso
        expect_error(fun binary:replace/4, [Bin, Pattern, Replacement, Opts])
    ).

prop_replace_invalid_subject() ->
    ?FORALL(
        {Bin, Pattern, Replacement, Opts},
        {gen_subject_invalid(), gen_patterns(), gen_replacement(), gen_opts([], [global, gen_scope_opt(<<>>), gen_insert_replaced_opt(<<>>)])},
        expect_error(fun binary:replace/3, [Bin, Pattern, Replacement]) andalso
        expect_error(fun binary:replace/4, [Bin, Pattern, Replacement, Opts])
    ).

%% --- split/2,3 --------------------------------------------------------
prop_split() ->
    ?FORALL(
        {Bin, Pattern, Opts},
        ?LET(
            B,
            binary(),
            {B, gen_patterns(B), gen_opts([], [global, trim, trim_all, gen_scope_opt(B)])}
        ),
        begin
            Split1 = binary:split(Bin, Pattern),
            Split2 = binary:split(Bin, Pattern, Opts),
            Split1 =:= binary:split(Bin, binary:compile_pattern(Pattern)) andalso
            Split1 =:= do_split(Bin, Pattern) andalso
            Split2 =:= binary:split(Bin, binary:compile_pattern(Pattern), Opts) andalso
            Split2 =:= do_split(Bin, Pattern, Opts)
        end
    ).

prop_split_3_invalid_scope() ->
    ?FORALL(
        {Bin, Pattern, Opts},
        ?LET(
            B,
            binary(),
            {B, gen_patterns(B), gen_opts([gen_scope_invalid_opt(B)], [global, trim, trim_all])}
        ),
        expect_error(fun binary:split/3, [Bin, Pattern, Opts]) andalso
        expect_error(fun binary:split/3, [Bin, binary:compile_pattern(Pattern), Opts])
    ).

prop_split_invalid_pattern() ->
    ?FORALL(
        {Bin, Pattern, Opts},
        ?LET(
            B,
            binary(),
            {B, gen_patterns_invalid(), gen_opts([], [global, trim, trim_all, gen_scope_opt(B)])}
        ),
        expect_error(fun binary:split/2, [Bin, Pattern]) andalso
        expect_error(fun binary:split/3, [Bin, Pattern, Opts])
    ).

prop_split_invalid_subject() ->
    ?FORALL(
        {Bin, Pattern, Opts},
        {gen_subject_invalid(), gen_patterns(), gen_opts([], [global, trim, trim_all, gen_scope_opt(<<>>)])},
        expect_error(fun binary:split/2, [Bin, Pattern]) andalso
        expect_error(fun binary:split/3, [Bin, Pattern, Opts])
    ).


%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%

%% Generator for lists of options, in random order.
%% The options given in RequiredOpts will be present.
%% The options given in MaybeOpts may or may not be present.
gen_opts(RequiredOpts, MaybeOpts) ->
    ?LET(
        {UseOpts, Shuffles},
        {vector(length(MaybeOpts), boolean()), vector(length(RequiredOpts) + length(MaybeOpts), integer())},
        begin
            UsedMaybeOpts = [Opt || {true, Opt} <- lists:zip(UseOpts, MaybeOpts)],
            UsedOpts = RequiredOpts ++ UsedMaybeOpts,
            ShuffledOpts = lists:sort(lists:zip(Shuffles, UsedOpts, trim)),
            [Opt || {_, Opt} <- ShuffledOpts]
        end
    ).

%% Generator for scope options, valid for the binary given in Bin
gen_scope_opt(Bin) ->
    {scope, gen_part(Bin)}.

%% Generator for scope options, invalid for the binary given in Bin
gen_scope_invalid_opt(Bin) ->
    {scope, gen_part_invalid(Bin)}.

%% Generator for a part range, valid for the binary given in Bin
gen_part(Bin) ->
    Size = byte_size(Bin),
    ?LET(
        S,
        range(0, Size),
        {S, range(-S, Size-S)}
    ).

%% Generator for a part range, invalid for the binary given in Bin
gen_part_invalid(Bin) ->
    Size = byte_size(Bin),
    ?SUCHTHAT(
        {S, L},
        {integer(), integer()},
        S < 0 orelse S > Size orelse S + L < 0 orelse S + L > Size
    ).

%% Generator for an index within the binary given in Bin
gen_index(Bin) ->
    range(0, byte_size(Bin)).

%% Generator for an index outside the binary given in Bin
gen_index_invalid(Bin) ->
    oneof([neg_integer(), range(byte_size(Bin) + 1, inf)]).

%% Generator for insert_replaced options, valid for the binary given in Replacement
gen_insert_replaced_opt(Replacement) when is_binary(Replacement) ->
    {insert_replaced, oneof([gen_index(Replacement), list(gen_index(Replacement))])};
gen_insert_replaced_opt(_Replacement) ->
    {insert_replaced, oneof([non_neg_integer(), list(non_neg_integer())])}.

%% Generator for insert_replaced options, invalid for the binary given in Replacement
gen_insert_replaced_invalid_opt(Replacement) when is_binary(Replacement) ->
    {insert_replaced, oneof([gen_index_invalid(Replacement), non_empty(list(gen_index_invalid(Replacement)))])};
gen_insert_replaced_invalid_opt(_Replacement) ->
    {insert_replaced, oneof([neg_integer(), non_empty(list(neg_integer()))])}.

%% Generator for patterns, that is a single or a list of pattern binaries,
%% which may or may not be present in the optionally binary given in Bin.
gen_patterns() ->
    oneof([gen_pattern(), non_empty(list(gen_pattern()))]).

gen_patterns(Bin) ->
    oneof([gen_pattern(Bin), non_empty(list(gen_pattern(Bin)))]).

%% Generator for a single pattern.
%% The pattern may or may not be present in the optionally binary given in Bin.
gen_pattern() ->
    non_empty(binary()).

gen_pattern(<<>>) ->
    non_empty(binary());
gen_pattern(Bin) ->
    oneof([non_empty(binary()),
           ?LET(
               S,
               range(0, byte_size(Bin) - 1),
               ?LET(
                   L,
                   range(1, byte_size(Bin) - S),
                   begin
                       <<_:S/binary, P:L/binary, _/binary>> = Bin,
                       P
                   end
                )
            )]).

%% Generator for invalid patterns
gen_patterns_invalid() ->
    oneof([
        ?SUCHTHAT(T, ct_proper_ext:safe_any(), T =:= <<>> orelse not is_binary(T) andalso not is_list(T)),
        ?LET(
            L,
            list(binary()),
            case L =:= [] orelse lists:any(fun(E) -> E =:= <<>> end, L) of
                true ->
                    L;
                false ->
                    [<<>> | L]
            end
        ),
        ?LET(
            L,
            list(ct_proper_ext:safe_any()),
            case L =:= [] orelse lists:any(fun(<<_, _/binary>>) -> false; (_) -> true end, L) of
                true ->
                        L;
                false ->
                    [?SUCHTHAT(T, ct_proper_ext:safe_any(), T =:= <<>> orelse not is_binary(T)) | L]
            end
        )
    ]).

%% Generator for invalid subjects
gen_subject_invalid() ->
    oneof([
        ?LET(
            {B, FillInt, FillSize},
            {bitstring(), range(0, 16#7f), range(1, 7)},
            case bit_size(B) rem 8 of
                0 ->
                    <<B/binary, FillInt:FillSize/integer>>;
                _ ->
                    B
            end
        ),
        ?SUCHTHAT(T, ct_proper_ext:safe_any(), not is_binary(T))
    ]).

%% Generator for invalid subjects or lists containing at least one
%% invalid subject
gen_subjects_invalid() ->
    ?LET(
        T,
        ct_proper_ext:safe_any(),
        case T of
            [_|_] ->
                case lists:all(fun erlang:is_binary/1, T) of
                    true ->
                        [gen_subject_invalid() | T];
                    false ->
                        T
                end;
            _ ->
                T
        end
    ).

%% Generator for replacements
gen_replacement() ->
    oneof([binary(), function1(binary())]).

%% Generator for invalid replacements
gen_replacement_invalid() ->
    ?SUCHTHAT(T, ct_proper_ext:safe_any(), not is_binary(T) andalso not is_function(T, 1)).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

%% Determine the number of bits required to store the given integer in full bytes
int_bitsize(0) ->
    0;
int_bitsize(I) ->
    8 + int_bitsize(I div 16#100).

%% Extract an indicator for whether or not to perform a global search from the given options list
opts_to_how(Opts) ->
    case lists:member(global, Opts) of
        true ->
            all;
        false ->
            first
    end.

%% Divide the given binary in the parts before, in, and after the scope specified in the given options list
opts_to_scope_parts(Bin, Opts) ->
    case lists:keyfind(scope, 1, Opts) of
        false ->
            {<<>>, Bin, <<>>};
        {scope, PosLen} ->
            part_split(Bin, PosLen)
    end.

%% Decide the trimming mode from the trim or trim_all options specified in the given options list
opts_to_trim(Opts) ->
    case {lists:member(trim_all, Opts), lists:member(trim, Opts)} of
        {true, _} ->
            all;
        {_, true} ->
            rear;
        _ ->
            none
    end.

%% Process a replacement based on the insert_replaced options specified in the given options list
opts_to_replacement(Replacement, _Opts) when is_function(Replacement) ->
    Replacement;
opts_to_replacement(Replacement, Opts) ->
    case lists:keyfind(insert_replaced, 1, Opts) of
        false ->
            Replacement;
        {insert_replaced, []} ->
            Replacement;
        {insert_replaced, [_|_] = Positions} ->
            split_replacement(Positions, Replacement);
        {insert_replaced, Position} ->
            <<Front:Position/binary, Rear/binary>> = Replacement,
            [Front, Rear]
    end.

split_replacement(Inserts, Bin) ->
    split_replacement1(Bin, lists:sort(Inserts), 0, []).

split_replacement1(Bin, [], L, Acc) ->
    lists:reverse([extract_part(Bin, L, byte_size(Bin))|Acc]);
split_replacement1(Bin, [P|Ps], L, Acc) ->
    split_replacement1(Bin, Ps, P, [extract_part(Bin, L, P)|Acc]).

extract_part(Bin, S, E) ->
    <<_:S/binary, P:(E-S)/binary, _/binary>> = Bin,
    P.

join_replacement(_Join, []) ->
    <<>>;
join_replacement(_Join, [P]) ->
    P;
join_replacement(Join, [P|Ps]) ->
    <<P/binary, Join/binary, (join_replacement(Join, Ps))/binary>>.

replace_match(Replacement, Match) when is_function(Replacement) ->
    Replacement(Match);
replace_match(Replacement, Match) when is_list(Replacement) ->
    join_replacement(Match, Replacement);
replace_match(Replacement, _Match) ->
    Replacement.

%% Split the given binary in the parts before, within, and after the scope
%% defined by Pos and Len
part_split(Bin, {Pos, Len}) when Len >= 0 ->
    <<Front:Pos/binary, Middle:Len/binary, Rear/binary>> = Bin,
    {Front, Middle, Rear};
part_split(Bin, {Pos, Len}) ->
    <<Front:(Pos + Len)/binary, Middle:(-Len)/binary, Rear/binary>> = Bin,
    {Front, Middle, Rear}.

%% Sort the given list of patterns by descending size.
%% If a single pattern is given, wraps it in a list.
sort_patterns(Patterns) when is_list(Patterns) ->
    lists:sort(fun(P1, P2) -> byte_size(P1) >= byte_size(P2) end, Patterns);
sort_patterns(Pattern) ->
    [Pattern].

%% Control implementation for longest_common_prefix/1
find_longest_common_prefix([B|_]=Bs) ->
    find_longest_common_prefix1(Bs, byte_size(B)).

find_longest_common_prefix1(_Bs, 0) ->
    0;
find_longest_common_prefix1([_B], N) ->
    N;
find_longest_common_prefix1([B1|[B2|_]=More], N) ->
    find_longest_common_prefix1(More, min(N, find_longest_common_prefix2(B1, B2, 0))).

find_longest_common_prefix2(<<C, B1/binary>>, <<C, B2/binary>>, N) ->
    find_longest_common_prefix2(B1, B2, N + 1);
find_longest_common_prefix2(_B1, _B2, N) ->
    N.

%% Control implementation for longest_common_suffix/1
find_longest_common_suffix([B|_]=Bs) ->
    find_longest_common_suffix1(Bs, byte_size(B)).

find_longest_common_suffix1(_Bs, 0) ->
    0;
find_longest_common_suffix1([_B], N) ->
    N;
find_longest_common_suffix1([B1|[B2|_]=More], N) ->
    S1 = byte_size(B1),
    S2 = byte_size(B2),
    <<_:(max(0, S1 - S2))/bytes, B1_1/binary>> = B1,
    <<_:(max(0, S2 - S1))/bytes, B2_1/binary>> = B2,
    find_longest_common_suffix1(More, min(N, find_longest_common_suffix2(B1_1, B2_1, 0))).

find_longest_common_suffix2(<<>>, <<>>, N) ->
    N;
find_longest_common_suffix2(<<C, B1/binary>>, <<C, B2/binary>>, N) ->
    find_longest_common_suffix2(B1, B2, N + 1);
find_longest_common_suffix2(<<_, B1/binary>>, <<_, B2/binary>>, _N) ->
    find_longest_common_suffix2(B1, B2, 0).

%% Control implementation for match/2
find_match(Bin, Patterns) ->
    find_match(Bin, Patterns, []).

%% Control implementation for match/3
find_match(Bin, Patterns, Opts) ->
    case find_matches(Bin, Patterns, Opts) of
        [] ->
            nomatch;
        [Match|_] ->
            Match
    end.

%% Control implementation for matches/2
find_matches(Bin, Patterns) ->
    find_matches(Bin, Patterns, []).

%% Control implementation for matches/3
find_matches(Bin, Patterns, Opts) ->
    {Front, SearchBin, _} = opts_to_scope_parts(Bin, Opts),
    SortedPatterns = sort_patterns(Patterns),
    Matches = find_matches1(SearchBin, SortedPatterns, SortedPatterns, 0, []),
    Offset = byte_size(Front),
    [{Pos + Offset, Len} || {Pos, Len} <- Matches].

find_matches1(<<>>, _Patterns, _AllPatterns, _Pos, Acc) ->
    lists:reverse(Acc);
find_matches1(<<_, Bin/binary>>, [], AllPatterns, Pos, Acc) ->
    find_matches1(Bin, AllPatterns, AllPatterns, Pos + 1, Acc);
find_matches1(Bin, [Pattern|Patterns], AllPatterns, Pos, Acc) ->
    case Bin of
        <<Pattern:(byte_size(Pattern))/binary, Bin1/binary>> ->
            find_matches1(Bin1, AllPatterns, AllPatterns, Pos + byte_size(Pattern), [{Pos, byte_size(Pattern)}|Acc]);
        _ ->
            find_matches1(Bin, Patterns, AllPatterns, Pos, Acc)
    end.

%% Control implementation for replace/3
do_replace(Bin, Patterns, Replacement) ->
    do_replace(Bin, Patterns, Replacement, []).

%% Control implementation for replace/4
do_replace(Bin, Patterns, Replacement, Opts) ->
    How = opts_to_how(Opts),
    {Front, ReplaceBin, Rear} = opts_to_scope_parts(Bin, Opts),
    Replacement1 = opts_to_replacement(Replacement, Opts),
    SortedPatterns = sort_patterns(Patterns),
    Replaced = do_replace1(How, ReplaceBin, SortedPatterns, SortedPatterns, Replacement1, <<>>),
    <<Front/binary, Replaced/binary, Rear/binary>>.

do_replace1(_How, <<>>, _Patterns, _AllPatterns, _Replacement, Acc) ->
    Acc;
do_replace1(How, <<C, Bin/binary>>, [], AllPatterns, Replacement, Acc) ->
    do_replace1(How, Bin, AllPatterns, AllPatterns, Replacement, <<Acc/binary, C>>);
do_replace1(How, Bin, [Pattern|Patterns], AllPatterns, Replacement, Acc) ->
    case Bin of
        <<Pattern:(byte_size(Pattern))/binary, Bin1/binary>> ->
            case How of
                first ->
                    <<Acc/binary, (replace_match(Replacement, Pattern))/binary, Bin1/binary>>;
                all ->
                    do_replace1(How, Bin1, AllPatterns, AllPatterns, Replacement, <<Acc/binary, (replace_match(Replacement, Pattern))/binary>>)
            end;
        _ ->
            do_replace1(How, Bin, Patterns, AllPatterns, Replacement, Acc)
    end.

%% Control implementation for split/2
do_split(Bin, Patterns) ->
    do_split(Bin, Patterns, []).

%% Control implementation for split/3
do_split(Bin, Patterns, Opts) ->
    How = opts_to_how(Opts),
    {Front, SplitBin, Rear} = opts_to_scope_parts(Bin, Opts),
    Trim = opts_to_trim(Opts),
    SortedPatterns = sort_patterns(Patterns),
    Splitted0 = do_split1(How, SplitBin, SortedPatterns, SortedPatterns, <<>>, []),
    Splitted1 = [<<Front/binary, (hd(Splitted0))/binary>> | tl(Splitted0)],
    Splitted2 = lists:droplast(Splitted1) ++ [<<(lists:last(Splitted1))/binary, Rear/binary>>],
    case Trim of
        none ->
            Splitted2;
        all ->
            lists:filter(fun(Part) -> Part =/= <<>> end, Splitted2);
        rear ->
            lists:reverse(lists:dropwhile(fun(Part) -> Part =:= <<>> end, lists:reverse(Splitted2)))
    end.

do_split1(_How, <<>>, _Patterns, _AllPatterns, AccB, AccL) ->
    lists:reverse([AccB | AccL]);
do_split1(How, <<C, Bin/binary>>, [], AllPatterns, AccB, AccL) ->
    do_split1(How, Bin, AllPatterns, AllPatterns, <<AccB/binary, C>>, AccL);
do_split1(How, Bin, [Pattern|Patterns], AllPatterns, AccB, AccL) ->
    case Bin of
        <<Pattern:(byte_size(Pattern))/binary, Bin1/binary>> ->
            case How of
                first ->
                    [AccB, Bin1];
                all ->
                    do_split1(How, Bin1, AllPatterns, AllPatterns, <<>>, [AccB | AccL])
            end;
        _ ->
            do_split1(How, Bin, Patterns, AllPatterns, AccB, AccL)
    end.

%% @doc Credit to the <a href="https://github.com/erlang/otp/pull/6297#discussion_r1034771450">comment</a> of @Maria-12648430
-spec check_hex_encoded(Data :: binary(), UpperHex :: binary(), LowerHex :: binary()) -> boolean().
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
    (U =:= $A + X - 10) andalso (L =:= $a + X - 10).

%% Determine whether the given binary consist of an even number of
%% bytes, with each byte in one of the ranges of $0..$9, $a..$f, $A..$F
is_hex_bin(<<>>) ->
    true;
is_hex_bin(<<C1, C2, Bin/binary>>) ->
    is_hex_char(C1) andalso
    is_hex_char(C2) andalso
    is_hex_bin(Bin);
is_hex_bin(_) ->
    false.

%% Check if the given byte is a hex digit
is_hex_char(C) when C >= $0, C =< $9 ->
    true;
is_hex_char(C) when C >= $a, C =< $f ->
    true;
is_hex_char(C) when C >= $A, C =< $F ->
    true;
is_hex_char(_) ->
    false.

%% Returns whether an error exception was raised when the
%% given function is applied to the given arguments
expect_error(Fun, Args) when is_function(Fun, length(Args)) ->
    try
        erlang:apply(Fun, Args)
    of
        _ ->
            false
    catch
        error:_ ->
            true;
        _:_ ->
            false
    end.

