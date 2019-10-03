%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2018. All Rights Reserved.
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

-module(bs_match_SUITE).
-compile(nowarn_shadow_vars).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 init_per_testcase/2,end_per_testcase/2,
         verify_highest_opcode/1, expand_and_squeeze/1,
	 size_shadow/1,int_float/1,otp_5269/1,null_fields/1,wiger/1,
	 bin_tail/1,save_restore/1,
	 partitioned_bs_match/1,function_clause/1,
	 unit/1,shared_sub_bins/1,bin_and_float/1,
	 dec_subidentifiers/1,skip_optional_tag/1,decode_integer/1,
	 wfbm/1,degenerated_match/1,bs_sum/1,coverage/1,
	 multiple_uses/1,zero_label/1,followed_by_catch/1,
	 matching_meets_construction/1,simon/1,matching_and_andalso/1,
	 otp_7188/1,otp_7233/1,otp_7240/1,otp_7498/1,
	 match_string/1,zero_width/1,bad_size/1,haystack/1,
	 cover_beam_bool/1,matched_out_size/1,follow_fail_branch/1,
	 no_partition/1,calling_a_binary/1,binary_in_map/1,
	 match_string_opt/1,select_on_integer/1,
	 map_and_binary/1,unsafe_branch_caching/1,
	 bad_literals/1,good_literals/1,constant_propagation/1,
	 parse_xml/1,get_payload/1,escape/1,num_slots_different/1,
         beam_bsm/1,guard/1,is_ascii/1,non_opt_eq/1,
         expression_before_match/1,erl_689/1,restore_on_call/1,
         restore_after_catch/1,matches_on_parameter/1,big_positions/1,
         matching_meets_apply/1,bs_start_match2_defs/1,
         exceptions_after_match_failure/1,
         bad_phi_paths/1,many_clauses/1]).

-export([coverage_id/1,coverage_external_ignore/2]).

-include_lib("common_test/include/ct.hrl").
-include_lib("syntax_tools/include/merl.hrl").


suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() -> 
    [{group,p}].

groups() -> 
    [{p,test_lib:parallel(),
      [verify_highest_opcode,
       size_shadow,int_float,otp_5269,null_fields,wiger,
       bin_tail,save_restore,expand_and_squeeze,
       partitioned_bs_match,function_clause,unit,
       shared_sub_bins,bin_and_float,dec_subidentifiers,
       skip_optional_tag,decode_integer,wfbm,degenerated_match,bs_sum,
       coverage,multiple_uses,zero_label,followed_by_catch,
       matching_meets_construction,simon,
       matching_and_andalso,otp_7188,otp_7233,otp_7240,
       otp_7498,match_string,zero_width,bad_size,haystack,
       cover_beam_bool,matched_out_size,follow_fail_branch,
       no_partition,calling_a_binary,binary_in_map,
       match_string_opt,select_on_integer,
       map_and_binary,unsafe_branch_caching,
       bad_literals,good_literals,constant_propagation,parse_xml,
       get_payload,escape,num_slots_different,
       beam_bsm,guard,is_ascii,non_opt_eq,
       expression_before_match,erl_689,restore_on_call,
       matches_on_parameter,big_positions,
       matching_meets_apply,bs_start_match2_defs,
       exceptions_after_match_failure,bad_phi_paths,
       many_clauses]}].

init_per_suite(Config) ->
    test_lib:recompile(?MODULE),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


init_per_testcase(Case, Config) when is_atom(Case), is_list(Config) ->
    Config.

end_per_testcase(Case, Config) when is_atom(Case), is_list(Config) ->
    ok.

verify_highest_opcode(_Config) ->
    case ?MODULE of
        bs_match_r21_SUITE ->
            {ok,Beam} = file:read_file(code:which(?MODULE)),
            case test_lib:highest_opcode(Beam) of
                Highest when Highest =< 163 ->
                    ok;
                TooHigh ->
                    ct:fail({too_high_opcode_for_21,TooHigh})
            end;
        _ ->
            ok
    end.

size_shadow(Config) when is_list(Config) ->
    %% Originally OTP-5270.
    7 = size_shadow_1(),
    7 = size_shadow_2(8),
    7 = size_shadow_3(),
    no = size_shadow_4(8),
    Any = {any,term,goes},
    {2577,Any,-175,whatever} =
	(size_shadow_5(Any, 12))(<<2577:12>>, -175, whatever),
    {7777,Any,42,whatever} =
	(size_shadow_6(Any, 13))(42, <<7777:13>>, whatever),
    {<<45>>,<<>>} = size_shadow_7({int,1}, <<1:16,45>>),
    {'EXIT',{function_clause,_}} =
	(catch size_shadow_7({int,42}, <<1:16,45>>)),
    ok.

size_shadow_1() ->
    L = 8,
    Fs = [fun(<<L:L,B:L>>) -> B end,
          fun(A) ->
                  (fun([<<L:L,B:L>>]) -> B end)([A])
          end,
          fun(A) ->
                  (fun([<<L:L,B:L>>,<<L:L,B:L>>]) -> B end)([A,A])
          end,
          fun(A) ->
                  <<Size:L,_/bits>> = A,
                  Inner = fun([L], {#{key1 := <<L:L,B:L>>,
                                    key2 := <<L:L,B:L>>}, L}) -> B end,
                  Inner([Size], {#{key1 => A,key2 => A},Size})
          end],
    size_shadow_apply(Fs, <<16:8, 7:16>>).

size_shadow_2(L) ->
    Fs = [fun(<<L:L,B:L>>) -> B end,
          fun(A) ->
                  (fun([<<L:L,B:L>>]) -> B end)([A])
          end,
          fun(A) ->
                  (fun({<<L:L,B:L>>,<<L:L,B:L>>}) -> B end)({A,A})
          end],
    size_shadow_apply(Fs, <<16:8, 7:16>>).

size_shadow_3() ->
    L = 8,
    Fs = [fun(<<L:L,B:L,L:L>>) -> B end,
          fun(A) ->
                  (fun({tag,[<<L:L,B:L,L:L>>]}) -> B end)({tag,[A]})
          end,
          fun(A) ->
                  (fun({tag,<<L:L,B:L,L:L>>,<<L:L,B:L,L:L>>}) -> B end)({tag,A,A})
          end],
    size_shadow_apply(Fs, <<16:8, 7:16,16:16>>).

size_shadow_4(L) ->
    Fs = [fun(<<L:L,B:L,L:L>>) -> B;
             (_) -> no
          end,
          fun(A) ->
                  Inner = fun([<<L:L,B:L,L:L>>]) -> B;
                             (_) -> no
                          end,
                  Inner([A])
          end,
          fun(A) ->
                  Inner = fun({<<L:L,B:L,L:L>>,<<L:L,B:L,L:L>>}) -> B;
                             (_) -> no
                          end,
                  Inner({A,A})
          end],
    size_shadow_apply(Fs, <<16:8, 7:16,15:16>>).

size_shadow_5(X, Y) ->
    fun (<< A:Y >>, Y, B) -> fum(A, X, Y, B) end.

size_shadow_6(X, Y) ->
    fun (Y, << A:Y >>, B) -> fum(A, X, Y, B) end.

fum(A, B, C, D) ->
    {A,B,C,D}.

size_shadow_7({int,N}, <<N:16,B:N/binary,T/binary>>) ->
    {B,T}.

size_shadow_apply([F|Fs], Arg) when is_function(F, 1) ->
    size_shadow_apply(Fs, Arg, F(Arg)).

size_shadow_apply([F|Fs], Arg, Res) when is_function(F, 1) ->
    Res = F(Arg),
    size_shadow_apply(Fs, Arg, Res);
size_shadow_apply([], _, Res) ->
    Res.

int_float(Config) when is_list(Config) ->
    %% OTP-5323
    <<103133.0:64/float>> = <<103133:64/float>>,
    <<103133:64/float>> = <<103133:64/float>>,

    %% Coverage of error cases in sys_pre_expand:coerce_to_float/2.
    case id(default) of
	<<(1 bsl 1024):64/float>> ->
	    ct:fail(should_not_match);
	default ->
	    ok
    end.

%% Stolen from erl_eval_SUITE and modified.
%% OTP-5269. Bugs in the bit syntax.
otp_5269(Config) when is_list(Config) ->
    check(fun() -> L = 8,
		   F = fun(<<A:L,B:A>>) -> B end,
		   F(<<16:8, 7:16>>)
                end,
                7),
    check(fun() -> L = 8, <<A:L,B:A>> = <<16:8, 7:16>>, B end,
	  7),
    check(fun() -> U = 8, (fun(<<U:U>>) -> U end)(<<32:8>>) end,
	  32),
    check(fun() -> U = 8, [U || <<U:U>> <- [<<32:8>>]] end,
	  [32]),
    check(fun() -> [X || <<A:8,
			   B:A>> <- [<<16:8,19:16>>],
			 <<X:8>> <- [<<B:8>>]] end,
	  [19]),
    check(fun() -> A = 4, B = 28, bit_size(<<13:(A+(X=B))>>), X end,
	  28),
    check(fun() ->
		  <<Size,B:Size/binary,Rest/binary>> = <<2,"AB","CD">>,
		  {Size,B,Rest}
	  end,
	  {2,<<"AB">>,<<"CD">>}),
    check(fun() -> X = 32,
		   [X || <<X:X>> <- [<<1:32>>,<<2:32>>,<<3:8>>]] end,
    %% "binsize variable"    ^
	  [1,2]),
    check(fun() ->
		  (fun (<<A:1/binary, B:8/integer, _C:B/binary>>) ->
			   case A of
			       B -> wrong;
			       _ -> ok
			   end
		   end)(<<1,2,3,4>>) end,
	  ok),
    ok.

null_fields(Config) when is_list(Config) ->
    check(fun() ->
		  W = id(0),
		  F = fun(<<_:W>>) -> tail;
			 (<<>>) -> empty
		      end,
		  F(<<>>)
	  end, tail),
    check(fun() ->
		  F = fun(<<_/binary>>) -> tail;
			 (<<>>) -> empty
		      end,
		  F(<<>>)
	  end, tail),
    ok.

wiger(Config) when is_list(Config) ->
    ok1 = wcheck(<<3>>),
    ok2 = wcheck(<<1,2,3>>),
    ok3 = wcheck(<<4>>),
    {error,<<1,2,3,4>>} = wcheck(<<1,2,3,4>>),
    {error,<<>>} = wcheck(<<>>),
    ok.

wcheck(<<A>>) when A==3->
    ok1;
wcheck(<<_,_:2/binary>>) ->
    ok2;
wcheck(<<_>>) ->
    ok3;
wcheck(Other) ->
    {error,Other}.

bin_tail(Config) when is_list(Config) ->
    S = <<"abcde">>,
    $a = bin_tail_c(S, 0),
    $c = bin_tail_c(S, 2),
    $e = bin_tail_c(S, 4),
    {'EXIT',_} = (catch bin_tail_c(S, 5)),
    {'EXIT',_} = (catch bin_tail_c_var(S, 5)),

    $a = bin_tail_d(S, 0),
    $b = bin_tail_d(S, 8),
    $d = bin_tail_d(S, 3*8),
    {'EXIT',_} = (catch bin_tail_d_dead(S, 1)),
    {'EXIT',_} = (catch bin_tail_d_dead(S, 9)),
    {'EXIT',_} = (catch bin_tail_d_dead(S, 5*8)),
    {'EXIT',_} = (catch bin_tail_d_var(S, 1)),

    ok = bin_tail_e(<<2:2,0:1,1:5>>),
    ok = bin_tail_e(<<2:2,1:1,1:5,42:64>>),
    error = bin_tail_e(<<3:2,1:1,1:5,42:64>>),
    error = bin_tail_e(<<>>),

    MD5 = erlang:md5(<<42>>),
    <<"abc">> = bin_tail_f(<<MD5/binary,"abc">>, MD5, 3),
    error = bin_tail_f(<<MD5/binary,"abc">>, MD5, 999),
    {'EXIT',{_,_}} = (catch bin_tail_f(<<0:16/unit:8>>, MD5, 0)),

    ok.

bin_tail_c(Bin, Offset) ->
    Res = bin_tail_c_dead(Bin, Offset),
    <<_:Offset/binary,_,Tail/binary>> = Bin,
    {Res,Tail} = bin_tail_c_var(Bin, Offset),
    Res.

bin_tail_c_dead(Bin, Offset) ->
    <<_:Offset/binary,C,_/binary>> = Bin,
    C.

bin_tail_c_var(Bin, Offset) ->
    <<_:Offset/binary,C,Tail/binary>> = Bin,
    {C,Tail}.


bin_tail_d(Bin, BitOffset) ->
    Res = bin_tail_d_dead(Bin, BitOffset),
    <<_:BitOffset,_:8,Tail/binary>> = Bin,
    {Res,Tail} = bin_tail_d_var(Bin, BitOffset),
    Res.

bin_tail_d_dead(Bin, BitOffset) ->
    <<_:BitOffset,C,_/binary>> = Bin,
    C.

bin_tail_d_var(Bin, BitOffset) ->
    <<_:BitOffset,C,Tail/binary>> = Bin,
    {C,Tail}.

bin_tail_e(Bin) ->
    case bin_tail_e_dead(Bin) of
	ok ->
	    <<_,Tail/binary>> = Bin,
	    Tail = bin_tail_e_var(Bin),
	    ok;
	error ->
	    bin_tail_e_var(Bin)
    end.

bin_tail_e_dead(Bin) ->
    case Bin of
	%% The binary is aligned at the end; neither the bs_skip_bits2 nor
	%% bs_test_tail2 instructions are needed.
	<<2:2,_:1,1:5,_/binary>> -> ok;
	_ -> error
    end.

bin_tail_e_var(Bin) ->
    case Bin of
	%% The binary is aligned at the end; neither the bs_skip_bits2 nor
	%% bs_test_tail2 instructions are needed.
	<<2:2,_:1,1:5,Tail/binary>> -> Tail;
	_ -> error
    end.

bin_tail_f(Bin, MD5, Size) ->
    case Bin of
        <<MD5:16/binary, Tail:Size/binary>> ->
            Tail;
        <<MD5:16/binary, _/binary>> ->
            error
    end.
	    
save_restore(Config) when is_list(Config) ->
    0 = save_restore_1(<<0:2,42:6>>),
    {1,3456} = save_restore_1(<<1:2,3456:14>>),
    {2,7981234} = save_restore_1(<<2:2,7981234:30>>),
    {3,763967493838} = save_restore_1(<<0:2,763967493838:62>>),

    A = <<" x">>,
    B = <<".x">>,
    C = <<"-x">>,

    {" ",<<"x">>} = lll(A),
    {" ",<<"x">>} = mmm(A),
    {" ",<<"x">>} = nnn(A),
    {" ",<<"x">>} = ooo(A),

    {".",<<"x">>} = lll(B),
    {".",<<"x">>} = mmm(B),
    {".",<<"x">>} = nnn(B),
    {".",<<"x">>} = ooo(B),

    {"-",<<"x">>} = lll(C),
    {"-",<<"x">>} = mmm(C),
    {"-",<<"x">>} = nnn(C),
    {"-",<<"x">>} = ooo(C),

    a = multiple_matches(<<777:16>>, <<777:16>>),
    b = multiple_matches(<<777:16>>, <<999:16>>),
    c = multiple_matches(<<777:16>>, <<57:8>>),
    d = multiple_matches(<<17:8>>, <<1111:16>>),

    Bin = <<-1:64>>,
    case bad_float_unpack_match(Bin) of
	-1 -> ok;
	_Other -> ct:fail(bad_return_value_probably_NaN)
    end.

save_restore_1(Bin) ->
    case Bin of
	<<0:2,_:6>> -> 0;
	<<1:2,A:14>> -> {1,A};
	<<2:2,A:30>> -> {2,A};
	<<A:64>> -> {3,A}
    end.

lll(<<Char,         Tail/binary>>) -> {[Char],Tail}.

mmm(<<$.,$.,$.,     Tail/binary>>) -> Tail;
mmm(<<$\s,$-,$\s,   Tail/binary>>) -> Tail;
mmm(<<Char,         Tail/binary>>) -> {[Char],Tail}. %% Buggy Tail!

nnn(<<"...",        Tail/binary>>) -> Tail;
nnn(<<" - ",        Tail/binary>>) -> Tail;
nnn(<<Char,         Tail/binary>>) -> {[Char],Tail}. %% Buggy Tail!

ooo(<<" - ",        Tail/binary>>) -> Tail;
ooo(<<Char,         Tail/binary>>) -> {[Char],Tail}.

multiple_matches(<<Y:16>>, <<Y:16>>) -> a;
multiple_matches(<<_:16>>, <<_:16>>) -> b;
multiple_matches(<<_:16>>, <<_:8>>) -> c;
multiple_matches(<<_:8>>, <<_:16>>) -> d.

bad_float_unpack_match(<<F:64/float>>) -> F;
bad_float_unpack_match(<<I:64/integer-signed>>) -> I.


partitioned_bs_match(Config) when is_list(Config) ->
    <<1,2,3>> = partitioned_bs_match(blurf, <<42,1,2,3>>),
    error = partitioned_bs_match(10, <<7,8,15,13>>),
    error = partitioned_bs_match(100, {a,tuple,is,'not',a,binary}),
    ok = partitioned_bs_match(0, <<>>),
    fc(partitioned_bs_match, [-1,blurf],
	     catch partitioned_bs_match(-1, blurf)),
    fc(partitioned_bs_match, [-1,<<1,2,3>>],
	     catch partitioned_bs_match(-1, <<1,2,3>>)),
    {17,<<1,2,3>>} = partitioned_bs_match_2(1, <<17,1,2,3>>),
    {7,<<1,2,3>>} = partitioned_bs_match_2(7, <<17,1,2,3>>),

    fc(partitioned_bs_match_2, [4,<<0:17>>],
	     catch partitioned_bs_match_2(4, <<0:17>>)),

    anything = partitioned_bs_match_3(anything, <<42>>),
    ok = partitioned_bs_match_3(1, 2),

    ok.

partitioned_bs_match(_, <<42:8,T/binary>>) ->
    T;
partitioned_bs_match(N, _) when N > 0 ->
    error;
partitioned_bs_match(_, <<>>) ->
    ok.

partitioned_bs_match_2(1, <<B:8,T/binary>>) ->
    {B,T};
partitioned_bs_match_2(Len, <<_:8,T/binary>>) ->
    {Len,T}.

partitioned_bs_match_3(Var, <<_>>) -> Var;
partitioned_bs_match_3(1, 2) -> ok.

function_clause(Config) when is_list(Config)  ->
    ok = function_clause_1(<<0,7,0,7,42>>),
    fc(function_clause_1, [<<0,1,2,3>>],
       catch function_clause_1(<<0,1,2,3>>)),
    fc(function_clause_1, [<<0,1,2,3>>],
       catch function_clause_1(<<0,7,0,1,2,3>>)),

    ok = function_clause_2(<<0,7,0,7,42>>),
    ok = function_clause_2(<<255>>),
    ok = function_clause_2(<<13:4>>),
    fc(function_clause_2, [<<0,1,2,3>>],
       catch function_clause_2(<<0,1,2,3>>)),
    fc(function_clause_2, [<<0,1,2,3>>],
       catch function_clause_2(<<0,7,0,1,2,3>>)),

    ok.

function_clause_1(<<0:8,7:8,T/binary>>) ->
    function_clause_1(T);
function_clause_1(<<_:8>>) ->
    ok.

function_clause_2(<<0:8,7:8,T/binary>>) ->
    function_clause_2(T);
function_clause_2(<<_:8>>) ->
    ok;
function_clause_2(<<_:4>>) ->
    ok.

unit(Config) when is_list(Config) ->
    42 = peek1(<<42>>),
    43 = peek1(<<43,1,2>>),
    43 = peek1(<<43,1,2,(-1):1>>),
    43 = peek1(<<43,1,2,(-1):2>>),
    43 = peek1(<<43,1,2,(-1):7>>),

    99 = peek8(<<99>>),
    100 = peek8(<<100,101>>),
    fc(peek8, [<<100,101,0:1>>], catch peek8(<<100,101,0:1>>)),

    37484 = peek16(<<37484:16>>),
    37489 = peek16(<<37489:16,5566:16>>),
    fc(peek16, [<<8>>], catch peek16(<<8>>)),
    fc(peek16, [<<42:15>>], catch peek16(<<42:15>>)),
    fc(peek16, [<<1,2,3,4,5>>], catch peek16(<<1,2,3,4,5>>)),

    127 = peek7(<<127:7>>),
    100 = peek7(<<100:7,19:7>>),
    fc(peek7, [<<1,2>>], catch peek7(<<1,2>>)),

    1 = unit_opt(1, -1),
    8 = unit_opt(8, -1),

    <<1:32,"abc">> = unit_opt_2(<<1:32,"abc">>),
    <<"def">> = unit_opt_2(<<2:32,"def">>),
    {'EXIT',_} = (catch unit_opt_2(<<1:32,33:7>>)),
    {'EXIT',_} = (catch unit_opt_2(<<2:32,55:7>>)),

    <<0:64>> = unit_opt_3(<<1:128>>),
    <<1:64>> = unit_opt_3(<<1:64>>),

    ok.

peek1(<<B:8,_/bitstring>>) -> B.

peek7(<<B:7,_/binary-unit:7>>) -> B.

peek8(<<B:8,_/binary>>) -> B.

peek16(<<B:16,_/binary-unit:16>>) -> B.

unit_opt(U, X) ->
    %% Cover type analysis in beam_ssa_type.
    Bin = case U of
              1 -> <<X:7>>;
              8 -> <<X>>
          end,
    %% The type of Bin will be set to {binary,gcd(1, 8)}.
    case Bin of
        <<_/binary-unit:8>> -> 8;
        <<_/binary-unit:1>> -> 1
    end.

unit_opt_2(<<St:32,KO/binary>> = Bin0) ->
    Bin = if
              St =:= 1 ->
                  Bin0;
              St =:= 2 ->
                  <<KO/binary>>
          end,
    id(Bin).

unit_opt_3(A) when is_binary(A) ->
    %% There should be no test_unit instruction after the first segment, since
    %% we already know A is a binary and its tail will still be a binary after
    %% matching 8 bytes from it.
    <<Bin:8/binary, _/binary>> = A,
    Bin.

shared_sub_bins(Config) when is_list(Config) ->
    {15,[<<>>,<<5>>,<<4,5>>,<<3,4,5>>,<<2,3,4,5>>]} = sum(<<1,2,3,4,5>>, [], 0),
    ok.

sum(<<B,T/binary>>, Acc, Sum) ->
    sum(T, [T|Acc], Sum+B);
sum(<<>>, Last, Sum) -> {Sum,Last}.


bin_and_float(Config) when is_list(Config) ->
    14.0 = bin_and_float(<<1.0/float,2.0/float,3.0/float>>, 0.0),
    ok.

bin_and_float(<<X/float,Y/float,Z/float,T/binary>>, Sum) when is_float(X),
							      is_float(Y),
							      is_float(Z) ->
    bin_and_float(T, Sum+X*X+Y*Y+Z*Z);
bin_and_float(<<>>, Sum) -> Sum.

dec_subidentifiers(Config) when is_list(Config) ->
    {[],<<1,2,3>>} =
	do_dec_subidentifiers(<<1:1,42:7,1:1,99:7,1,2,3>>, 0, [], 2),
    {[5389],<<1,2,3>>} = do_dec_subidentifiers(<<1:1,42:7,0:1,13:7,1,2,3>>, 0, [], 2),
    {[3,2,1],not_a_binary} = dec_subidentifiers(not_a_binary, any, [1,2,3], 0),
    ok.

do_dec_subidentifiers(Buffer, Av, Al, Len) -> 
    Res = dec_subidentifiers(Buffer, Av, Al, Len),
    Res = dec_subidentifiers2(Buffer, Av, Al, Len),
    Res = dec_subidentifiers4(Buffer, Av, Al, Len),
    Res = dec_subidentifiers3(Buffer, Av, Al, Len).

dec_subidentifiers(Buffer, _Av, Al, 0) -> 
    {lists:reverse(Al),Buffer}; 
dec_subidentifiers(<<1:1,H:7,T/binary>>, Av, Al, Len) -> 
    dec_subidentifiers(T, (Av bsl 7) bor H, Al, Len-1);
dec_subidentifiers(<<H,T/binary>>, Av, Al, Len) -> 
    dec_subidentifiers(T, 0, [((Av bsl 7) bor H)|Al], Len-1).

dec_subidentifiers2(<<Buffer/binary>>, _Av, Al, 0) ->
    {lists:reverse(Al),Buffer}; 
dec_subidentifiers2(<<1:1,H:7,T/binary>>, Av, Al, Len) -> 
    dec_subidentifiers2(T, (Av bsl 7) bor H, Al, Len-1); 
dec_subidentifiers2(<<H,T/binary>>, Av, Al, Len) -> 
    dec_subidentifiers2(T, 0, [((Av bsl 7) bor H)|Al], Len-1).

dec_subidentifiers3(Buffer, _Av, Al, 0) when is_binary(Buffer) ->
    {lists:reverse(Al),Buffer}; 
dec_subidentifiers3(<<1:1,H:7,T/binary>>, Av, Al, Len) -> 
    dec_subidentifiers3(T, (Av bsl 7) bor H, Al, Len-1); 
dec_subidentifiers3(<<H,T/binary>>, Av, Al, Len) -> 
    dec_subidentifiers3(T, 0, [((Av bsl 7) bor H)|Al], Len-1).

dec_subidentifiers4(<<1:1,H:7,T/binary>>, Av, Al, Len) when Len =/= 0 -> 
    dec_subidentifiers4(T, (Av bsl 7) bor H, Al, Len-1); 
dec_subidentifiers4(<<H,T/binary>>, Av, Al, Len) when Len =/= 0 -> 
    dec_subidentifiers4(T, 0, [((Av bsl 7) bor H)|Al], Len-1);
dec_subidentifiers4(Buffer, _Av, Al, 0) -> 
    {lists:reverse(Al),Buffer}.


skip_optional_tag(Config) when is_list(Config) ->
    {ok,<<>>} = skip_optional_tag(<<42>>, <<42>>),
    {ok,<<>>} = skip_optional_tag(<<42,1>>, <<42,1>>),
    {ok,<<1,2,3>>} = skip_optional_tag(<<42>>, <<42,1,2,3>>),
    missing = skip_optional_tag(<<2:3>>, blurf),
    ok.

skip_optional_tag(<<>>, Binary) ->
    {ok,Binary};
skip_optional_tag(<<Tag,RestTag/binary>>, <<Tag,Rest/binary>>) ->
    skip_optional_tag(RestTag, Rest);
skip_optional_tag(_, _) -> missing.

decode_integer(_Config) ->
    {10795,<<43>>,whatever} = decode_integer(1, <<42,43>>, whatever),
    {-28909,<<19>>,whatever} = decode_integer(1, <<143,19>>, whatever),
    ok.

decode_integer(Len, <<B1:1,B2:7,Bs/binary>>, RemovedBytes) when B1 == 0 ->
    Bin = <<_Skip:Len/unit:8, Buffer2/binary>> = <<B1:1,B2:7,Bs/binary>>,
    Size = byte_size(Bin),
    <<Int:Size/unit:8>> = Bin,
    {Int,Buffer2,RemovedBytes};
decode_integer(Len, <<B1:1,B2:7,Bs/binary>>, RemovedBytes)  ->
    Bin = <<_Skip:Len/unit:8,Buffer2/binary>> = <<B1:1,B2:7,Bs/binary>>,
    Size = byte_size(Bin),
    <<N:Size/unit:8>> = <<B2,Bs/binary>>,
    Int = N - (1 bsl (8 * size(Bin) -1)),
    {Int,Buffer2,RemovedBytes}.

-define(DATELEN, 16).

wfbm(Config) when is_list(Config) ->
    %% check_for_dot_or_space and get_tail is from wfbm4 by Steve Vinoski,
    %% with modifications.
    {nomatch,0} = check_for_dot_or_space(<<" ">>),
    {nomatch,0} = check_for_dot_or_space(<<" abc">>),
    {ok,<<"abcde">>} = check_for_dot_or_space(<<"abcde 34555">>),
    {nomatch,0} = check_for_dot_or_space(<<".gurka">>),
    {nomatch,1} = check_for_dot_or_space(<<"g.urka">>),

    nomatch = get_tail(<<>>),
    {ok,<<"2007/10/23/blurf">>} = get_tail(<<"200x/2007/10/23/blurf ">>),
    {skip,?DATELEN+5} = get_tail(<<"200x/2007/10/23/blurf.">>),
    nomatch = get_tail(<<"200y.2007.10.23.blurf ">>),
    {'EXIT',_} = (catch get_tail({no,binary,at,all})),
    {'EXIT',_} = (catch get_tail(no_binary)),
    ok.

check_for_dot_or_space(Bin) ->
    check_for_dot_or_space(Bin, 0).

check_for_dot_or_space(<<$\s, _/binary>>, 0) ->
    {nomatch,0};
check_for_dot_or_space(Bin, Len) ->
    case Bin of
        <<Front:Len/binary, $\s, _/binary>> ->
            {ok,Front};
        <<_:Len/binary, $., _/binary>> ->
            {nomatch,Len};
	_ ->
            check_for_dot_or_space(Bin, Len+1)
    end.

get_tail(<<>>) ->
    nomatch;
get_tail(Bin) ->
    <<Front:?DATELEN/binary, Tail/binary>> = Bin,
    case Front of
        <<_:3/binary,"x/",Y:4/binary,$/,M:2/binary,$/,D:2/binary,$/>> ->
	    case check_for_dot_or_space(Tail) of
                {ok,Match} ->
                    {ok,<<Y/binary,$/,M/binary,$/,D/binary,$/, Match/binary>>};
                {nomatch,Skip} -> {skip,?DATELEN + Skip}
            end;
        _ -> nomatch
    end.

degenerated_match(Config) when is_list(Config) ->
    error = degenerated_match_1(<<>>),
    1 = degenerated_match_1(<<1:1>>),
    2 = degenerated_match_1(<<42,43>>),

    error = degenerated_match_2(<<>>),
    no_split = degenerated_match_2(<<1,2>>),
    {<<1,2,3,4>>,<<5>>} = degenerated_match_2(<<1,2,3,4,5>>),
    
    ok.

degenerated_match_1(<<>>) -> error;
degenerated_match_1(Bin) -> byte_size(Bin).

degenerated_match_2(<<>>) -> error;
degenerated_match_2(Bin) ->
    case byte_size(Bin) > 4 of
	true ->
	    split_binary(Bin, 4);
	false ->
	    no_split
    end.

bs_sum(Config) when is_list(Config) ->
    0 = bs_sum_1([]),
    0 = bs_sum_1(<<>>),
    42 = bs_sum_1([42]),
    1 = bs_sum_1(<<1>>),
    10 = bs_sum_1([1,2,3,4]),
    15 = bs_sum_1(<<1,2,3,4,5>>),
    21 = bs_sum_1([1,2,3|<<4,5,6>>]),
    15 = bs_sum_1([1,2,3|{4,5}]),
    6 = bs_sum_1([1,2,3|zero]),
    6 = bs_sum_1([1,2,3|0]),
    7 = bs_sum_1([1,2,3|one]),

    fc(catch bs_sum_1({too,big,tuple})),
    fc(catch bs_sum_1([1,2,3|{too,big,tuple}])),

    [] = sneaky_alias(<<>>),
    [559,387655] = sneaky_alias(id(<<559:32,387655:32>>)),
    fc(sneaky_alias, [<<1>>], catch sneaky_alias(id(<<1>>))),
    fc(sneaky_alias, [[1,2,3,4]], catch sneaky_alias(lists:seq(1, 4))),
    ok.

bs_sum_1(<<H,T/binary>>) -> H+bs_sum_1(T);
bs_sum_1([H|T]) -> H+bs_sum_1(T);
bs_sum_1({A,B}=_Tuple=_AliasForNoGoodReason) -> A+B;
bs_sum_1(0) -> 0;
bs_sum_1(zero=_Zero) -> 0;
bs_sum_1(one) -> 1;
bs_sum_1([]) -> 0;
bs_sum_1(<<>>) -> 0.

sneaky_alias(<<>>=L) -> binary_to_list(L);
sneaky_alias(<<From:32,L/binary>>) -> [From|sneaky_alias(L)].

coverage(Config) when is_list(Config) ->
    0 = coverage_fold(fun(B, A) -> A+B end, 0, <<>>),
    6 = coverage_fold(fun(B, A) -> A+B end, 0, <<1,2,3>>),
    fc(catch coverage_fold(fun(B, A) ->
					 A+B
				 end, 0, [a,b,c])),

    {<<42.0:64/float>>,float} = coverage_build(<<>>, <<42>>, float),
    {<<>>,not_a_tuple} = coverage_build(<<>>, <<>>, not_a_tuple),
    {<<16#76,"abc",16#A9,"abc">>,{x,42,43}} =
	coverage_build(<<>>, <<16#7,16#A>>, {x,y,z}),

    [<<2>>,<<1>>] = coverage_bc(<<1,2>>, []),

    {x,<<"abc">>,z} = coverage_setelement(<<2,"abc">>, {x,y,z}),

    [42] = coverage_apply(<<42>>, [coverage_id]),
    42 = coverage_external(<<42>>),

    do_coverage_bin_to_term_list([]),
    do_coverage_bin_to_term_list([lists:seq(0, 10),{a,b,c},<<23:42>>]),
    fc(coverage_bin_to_term_list, [<<0,0,0,7>>],
	     catch do_coverage_bin_to_term_list_1(<<7:32>>)),

    <<>> = coverage_per_key(<<4:32>>),
    <<$a,$b,$c>> = coverage_per_key(<<7:32,"abc">>),

    binary = coverage_bitstring(<<>>),
    binary = coverage_bitstring(<<7>>),
    bitstring = coverage_bitstring(<<7:4>>),
    other = coverage_bitstring([a]),

    %% Cover code in beam_trim.

    {done,<<17,53>>,[253,155,200]} =
        coverage_trim(<<253,155,200,17,53>>, e0, e1, e2, e3, []),

    <<"(right|linux)">> = coverage_trim_1(<<"">>, <<"right">>, <<"linux">>),
    <<"/(right|linux)">> = coverage_trim_1(<<"/">>, <<"right">>, <<"linux">>),
    <<"(left|linux)/(right|linux)">> =
        coverage_trim_1(<<"left">>, <<"right">>, <<"linux">>),

    {10,<<"-">>,""} = coverage_trim_2(<<"-">>, 10, []),
    {8,<<"-">>,"aa"} = coverage_trim_2(<<"aa-">>, 10, []),

    ok.

coverage_fold(Fun, Acc, <<H,T/binary>>) ->
    IdFun = fun id/1,
    coverage_fold(Fun, Fun(IdFun(H), IdFun(Acc)), T);
coverage_fold(Fun, Acc, <<>>) when is_function(Fun, 2) -> Acc.

coverage_build(Acc0, <<H,T/binary>>, float) ->
    Float = id(<<H:64/float>>),
    Acc = <<Acc0/binary,Float/binary>>,
    coverage_build(Acc, T, float);
coverage_build(Acc0, <<H,T/binary>>, Tuple0) ->
    Str = id(<<H:(id(4)),(H-1):4,"abc">>),
    Acc = id(<<Acc0/bitstring,Str/bitstring>>),
    Tuple = setelement(2, setelement(3, Tuple0, 43), 42),
    if
	byte_size(Acc) > 0 ->
	    coverage_build(Acc, T, Tuple)
    end;
coverage_build(Acc, <<>>, Tuple) -> {Acc,Tuple}.

coverage_bc(<<H,T/binary>>, Acc) ->
    B = << <<C:8>> || C <- [H] >>,
    coverage_bc(T, [B|Acc]);
coverage_bc(<<>>, Acc) -> Acc.

coverage_setelement(<<H,T1/binary>>, Tuple) when element(1, Tuple) =:= x ->
    setelement(H, Tuple, T1).

coverage_apply(<<H,T/binary>>, [F|Fs]) ->
    [?MODULE:F(H)|coverage_apply(T, Fs)];
coverage_apply(<<>>, []) -> [].

coverage_external(<<H,T/binary>>) ->
    ?MODULE:coverage_external_ignore(T, T),
    H.

coverage_external_ignore(_, _) ->
    ok.

coverage_id(I) -> id(I).

do_coverage_bin_to_term_list(L) ->
    Bin = << <<(begin BinTerm = term_to_binary(Term),
		      <<(byte_size(BinTerm)):32,BinTerm/binary>> end)/binary>> ||
	      Term <- L >>,
    L = do_coverage_bin_to_term_list_1(Bin),
    L = do_coverage_bin_to_term_list_1(<<Bin/binary,7:32,"garbage">>),
    L = do_coverage_bin_to_term_list_1(<<7:32,"garbage",Bin/binary>>).   

do_coverage_bin_to_term_list_1(Bin) ->
    Res = coverage_bin_to_term_list(Bin),
    Res = coverage_bin_to_term_list(Bin, []),
    Res = coverage_bin_to_term_list_catch(Bin),
    Res = coverage_bin_to_term_list_catch(Bin, []).

coverage_bin_to_term_list(<<Sz:32,BinTerm:Sz/binary,T/binary>>) ->
    try binary_to_term(BinTerm) of
	Term -> [Term|coverage_bin_to_term_list(T)]
    catch
	error:badarg -> coverage_bin_to_term_list(T)
    end;
coverage_bin_to_term_list(<<>>) -> [].

coverage_bin_to_term_list(<<Sz:32,BinTerm:Sz/binary,T/binary>>, Acc) ->
    try binary_to_term(BinTerm) of
	Term -> coverage_bin_to_term_list(T, [Term|Acc])
    catch
	error:badarg -> coverage_bin_to_term_list(T, Acc)
    end;
coverage_bin_to_term_list(<<>>, Acc) -> lists:reverse(Acc).

coverage_bin_to_term_list_catch(<<Sz:32,BinTerm:Sz/binary,T/binary>>) ->
    case catch binary_to_term(BinTerm) of
	{'EXIT',_} -> coverage_bin_to_term_list_catch(T);
	Term -> [Term|coverage_bin_to_term_list_catch(T)]
    end;
coverage_bin_to_term_list_catch(<<>>) -> [].

coverage_bin_to_term_list_catch(<<Sz:32,BinTerm:Sz/binary,T/binary>>, Acc) ->
    case catch binary_to_term(BinTerm) of
	{'EXIT',_} -> coverage_bin_to_term_list_catch(T, Acc);
	Term -> coverage_bin_to_term_list_catch(T, [Term|Acc])
    end;
coverage_bin_to_term_list_catch(<<>>, Acc) -> lists:reverse(Acc).

coverage_per_key(<<BinSize:32,Bin/binary>> = B) ->
    true = (byte_size(B) =:= BinSize),
    Bin.

coverage_bitstring(Bin) when is_binary(Bin) -> binary;
coverage_bitstring(<<_/bitstring>>) -> bitstring;
coverage_bitstring(_) -> other.

coverage_trim(<<C:8,T/binary>> = Bin, E0, E1, E2, E3, Acc) ->
    case id(C > 128) of
        true ->
            coverage_trim(T, E0, E1, E2, E3, [C|Acc]);
        false ->
            {done,Bin,lists:reverse(Acc)}
    end.

coverage_trim_1(<<>>, Right, OsType) ->
    do_coverage_trim_1(Right, OsType);
coverage_trim_1(<<"/">>, Right, OsType) ->
    <<"/",(do_coverage_trim_1(Right, OsType))/binary>>;
coverage_trim_1(Left, Right, OsType) ->
    <<(do_coverage_trim_1(Left, OsType))/binary,
      "/",
      (do_coverage_trim_1(Right, OsType))/binary>>.

do_coverage_trim_1(A, OsType) ->
    <<"(",A/binary,"|",OsType/binary,")">>.

coverage_trim_2(<<C/utf8,R/binary>> = Bin, I, L) ->
    case printable_char(C) of
        true ->
            coverage_trim_2(R, I - 1, [C | L]);
        false ->
            {I,Bin,lists:reverse(L)}
    end.

printable_char($a) -> true;
printable_char(_) -> false.

multiple_uses(Config) when is_list(Config) ->
    {344,62879,345,<<245,159,1,89>>} = multiple_uses_1(<<1,88,245,159,1,89>>),
    true = multiple_uses_2(<<0,0,197,18>>),
    <<42,43>> = multiple_uses_3(<<0,0,42,43>>, fun id/1),

    ok = first_after(<<>>, 42),
    <<1>> = first_after(<<1,2,3>>, 0),
    <<2>> = first_after(<<1,2,3>>, 1),

    ok.

multiple_uses_1(<<X:16,Tail/binary>>) ->
    %% NOT OPTIMIZED: sub binary is matched or used in more than one place
    {Y,Z} = multiple_uses_match(Tail),
    {X,Y,Z,Tail}.

multiple_uses_2(<<_:16,Tail/binary>>) ->
    %% NOT OPTIMIZED: sub binary is matched or used in more than one place
    multiple_uses_cmp(Tail, Tail).

multiple_uses_3(<<_:16,Tail/binary>>, Fun) ->
    %% NOT OPTIMIZED: sub binary is used or returned
    Fun(Tail).

multiple_uses_match(<<Y:16,Z:16>>) ->
    {Y,Z}.

multiple_uses_cmp(<<Y:16>>, <<Y:16>>) -> true;
multiple_uses_cmp(<<_:16>>, <<_:16>>) -> false.

first_after(Data, Offset) ->
    case byte_size(Data) > Offset of
	false ->
	    {_First, _Rest} = {ok, ok},
	    ok;
	true ->
	    <<_:Offset/binary, Rest/binary>> = Data,
	    %% 'Rest' saved in y(0) before the call.
            {First, _} = match_first(Data, Rest),
            %% When beam_bsm sees the code, the following line
            %% which uses y(0) has been optimized away.
	    {First, Rest} = {First, Rest},
	    First
    end.

match_first(_, <<First:1/binary, Rest/binary>>) ->
    {First, Rest}.

zero_label(Config) when is_list(Config) ->
    <<"nosemouth">> = read_pols(<<"FACE","nose","mouth">>),
    <<"CE">> = read_pols(<<"noFACE">>),
    ok.

read_pols(Data) ->
    <<PolygonType:4/binary,Rest/binary>> = Data,
    %% Intentional warning.
    (PolygonType == <<"FACE">>) or (PolygonType == <<"PTCH">>),
    Rest.

followed_by_catch(Config) when is_list(Config) ->
    ok = handle(<<0,1,2,3,4,5>>).

-record(rec,{field}).
handle(<<>>) ->  ok;
handle(Msg) ->
    <<_DataLen:16, Rest/binary>> = Msg,
    case catch fooX:func() of
	[X] ->
	    X#rec.field;
	_ ->
	    ok
    end,
    handle(Rest).

matching_meets_construction(Config) when is_list(Config) ->
    Bin = id(<<"abc">>),
    Len = id(2),
    Tail0 = id(<<1,2,3,4,5>>),
    <<_:Len/binary,Tail/binary>> = Tail0,
    Res = <<Tail/binary,Bin/binary>>,
    <<3,4,5,"abc">> = Res,
    {'EXIT',{badarg,_}} = (catch matching_meets_construction_1(<<"Abc">>)),
    {'EXIT',{badarg,_}} = (catch matching_meets_construction_2(<<"Abc">>)),
    <<"Bbc">> = matching_meets_construction_3(<<"Abc">>),

    <<1,2>> = encode_octet_string(<<1,2,3>>, 2),
    ok.

matching_meets_construction_1(<<"A",H/binary>>) -> <<"B",H>>.

matching_meets_construction_2(<<"A",H/binary>>) -> <<"B",H/float>>.

matching_meets_construction_3(<<"A",H/binary>>) -> <<"B",H/binary>>.

encode_octet_string(<<OctetString/binary>>, Len) ->
    <<OctetString:Len/binary-unit:8>>.

simon(Config) when is_list(Config) ->
    one = simon(blurf, <<>>),
    two = simon(0, <<42>>),
    fc(simon, [17,<<1>>], catch simon(17, <<1>>)),
    fc(simon, [0,<<1,2,3>>], catch simon(0, <<1,2,3>>)),

    one = simon2(blurf, <<9>>),
    two = simon2(0, <<9,1>>),
    fc(simon2, [0,<<9,10,11>>], catch simon2(0, <<9,10,11>>)),
    ok.

simon(_, <<>>) -> one;
simon(0, <<_>>) -> two.

simon2(_, <<9>>) -> one;
simon2(0, <<_:16>>) -> two.


%% OTP-7113: Crash in v3_codegen.
matching_and_andalso(Config) when is_list(Config) ->
    ok = matching_and_andalso_1(<<1,2,3>>, 3),
    {'EXIT',{function_clause,_}} = (catch matching_and_andalso_1(<<1,2,3>>, -8)),
    {'EXIT',{function_clause,_}} = (catch matching_and_andalso_1(<<1,2,3>>, blurf)),
    {'EXIT',{function_clause,_}} = (catch matching_and_andalso_1(<<1,2,3>>, 19)),

    {"abc",<<"xyz">>} = matching_and_andalso_23("abc", <<"-xyz">>),
    {"abc",<<"">>} = matching_and_andalso_23("abc", <<($a-1)>>),
    {"abc",<<"">>} = matching_and_andalso_23("abc", <<($z+1)>>),
    {"abc",<<"">>} = matching_and_andalso_23("abc", <<($A-1)>>),
    {"abc",<<"">>} = matching_and_andalso_23("abc", <<($Z+1)>>),
    error = matching_and_andalso_23([], <<>>),
    error = matching_and_andalso_23([], <<$A>>),
    error = matching_and_andalso_23([], <<$Z>>),
    error = matching_and_andalso_23([], <<$a>>),
    error = matching_and_andalso_23([], <<$z>>),
    ok.

matching_and_andalso_1(<<Bitmap/binary>>, K)
  when is_integer(K) andalso size(Bitmap) >= K andalso 0 < K ->
    ok.

matching_and_andalso_23(Datetime, Bin) ->
    Res = matching_and_andalso_2(Datetime, Bin),
    Res = matching_and_andalso_3(Datetime, Bin),
    Res.

matching_and_andalso_2(Datetime, <<H,T/binary>>)
  when not ((H >= $a) andalso (H =< $z)) andalso
       not ((H >= $A) andalso (H =< $Z)) ->
    {Datetime,T};
matching_and_andalso_2(_, _) -> error.

%% Contrived example to ensure we cover the handling of 'call' instructions
%% in v3_codegen:bsm_rename_ctx/4.
matching_and_andalso_3(Datetime, <<H,T/binary>>)
  when not ((abs(H) >= $a) andalso (abs(H) =< $z)) andalso
       not ((abs(H) >= $A) andalso (abs(H) =< $Z)) ->
    {Datetime,T};
matching_and_andalso_3(_, _) -> error.

%% Thanks to Tomas Stejskal.
otp_7188(Config) when is_list(Config) ->
    MP3 = <<84,65,71,68,117,154,105,232,107,121,0,0,0,0,0,0,0,0,0,0,
	   0,0,0,0,0,0,0,0,0,0,0,0,0,68,97,110,105,101,108,32,76,
	   97,110,100,97,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,66,
	   101,115,116,32,79,102,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	   0,0,0,0,0,0,50,48,48,48,50,48,48,48,32,45,32,66,101,115,
	   116,32,79,102,32,32,32,32,32,32,32,32,32,32,32,32,32,32,
	   32,32,12>>,
    {ok,{"ID3v1",
	       [{title,<<68,117,154,105,232,107,121>>},
		{artist,<<"Daniel Landa">>},
		{album,<<"Best Of">>}]}} = parse_v1_or_v11_tag(MP3).

parse_v1_or_v11_tag(<<"TAG", Title:30/binary,
		     Artist:30/binary, Album:30/binary,
		     _Year:4/binary, _Comment:28/binary,
		     0:8, Track:8, _Genre:8>>) ->
    {ok,
     {"ID3v1.1",
      [{track, Track}, {title, trim(Title)},
       {artist, trim(Artist)}, {album, trim(Album)}]}};
parse_v1_or_v11_tag(<<"TAG", Title:30/binary,
		     Artist:30/binary, Album:30/binary,
		     _Year:4/binary, _Comment:30/binary,
		     _Genre:8>>) ->
    {ok,
     {"ID3v1",
      [{title, trim(Title)},
       {artist, trim(Artist)},
       {album, trim(Album)}]}};
parse_v1_or_v11_tag(_) ->
    error.

trim(Bin) ->
    list_to_binary(trim_blanks(binary_to_list(Bin))).

trim_blanks(L) ->
    lists:reverse(skip_blanks_and_zero(lists:reverse(L))).

skip_blanks_and_zero([$\s|T]) ->
    skip_blanks_and_zero(T);
skip_blanks_and_zero([0|T]) ->
    skip_blanks_and_zero(T);
skip_blanks_and_zero(L) ->
    L.

%% OTP-7233. Record and binary matching optimizations clashed.
%% Thanks to Vladimir Klebansky.

-record(rec_otp_7233, {key, val}).

otp_7233(Config) when is_list(Config) ->
    otp_7233_1(#rec_otp_7233{key = <<"XXabcde">>,val=[{"xxxxxxxx",42}]}),
    [<<"XXabcde">>,{"xxxxxxxx",42}] = get(io_format),
    erase(io_format),
    otp_7233_1(#rec_otp_7233{key = <<"XXabcde">>,val=[]}),
    undefined = get(io_format),
    ok.

otp_7233_1(Rec) ->
    <<K:2/binary,_Rest:5/binary>> = Rec#rec_otp_7233.key,
    case K of
	<<"XX">> ->
	    Value = Rec#rec_otp_7233.val,
	    case lists:keyfind("xxxxxxxx", 1, Value) of
		false ->
		    ok;
		T ->
		    put(io_format, [Rec#rec_otp_7233.key,T])
	    end;
	_ -> ok
    end.


otp_7240(Config) when is_list(Config) ->
    a = otp_7240_a(0, <<>>),
    b = otp_7240_a(1, 2),

    a = otp_7240_b(anything, <<>>),
    b = otp_7240_b(1, {x,y}),

    a = otp_7240_c(anything, <<>>),
    b = otp_7240_c(1, <<2>>),

    a = otp_7240_d(anything, <<>>),
    b = otp_7240_d(again, <<2>>),

    a = otp_7240_e(anything, <<>>),
    b = otp_7240_e(1, 41),

    a = otp_7240_f(anything, <<>>),
    b = otp_7240_f(1, {}),
    
    ok.

otp_7240_a(_, <<>>) -> a;
otp_7240_a(1, 2) -> b.

otp_7240_b(_, <<>>) -> a;
otp_7240_b(1, {_,_}) -> b.

otp_7240_c(_, <<>>) -> a;
otp_7240_c(1, <<2>>) -> b.

otp_7240_d(_, <<>>) -> a;
otp_7240_d(_, <<2>>) -> b.

otp_7240_e(_, <<>>) -> a;
otp_7240_e(1, B) when B < 42 -> b.

otp_7240_f(_, <<>>) -> a;
otp_7240_f(1, B) when is_tuple(B) -> b.

otp_7498(Config) when is_list(Config) ->
    <<1,2,3>> = otp_7498_foo(<<1,2,3>>, 0),
    <<2,3>> = otp_7498_foo(<<1,2,3>>, 1),
    <<1,2,3>> = otp_7498_foo(<<1,2,3>>, 2),

    <<1,2,3>> = otp_7498_bar(<<1,2,3>>, 0),
    <<2,3>> = otp_7498_bar(<<1,2,3>>, 1),
    <<1,2,3>> = otp_7498_bar(<<1,2,3>>, 2),
    <<>> = otp_7498_bar(<<>>, 2),
    <<1,2,3>> = otp_7498_bar(<<1,2,3>>, 3),

    ok.

otp_7498_foo(Bin, 0) ->
   otp_7498_foo(Bin, 42);
otp_7498_foo(<<_A, Rest/bitstring>>, 1) ->
   otp_7498_foo(Rest, 43);
otp_7498_foo(Bin, _I)  ->
   Bin.

otp_7498_bar(Bin, 0) ->
   otp_7498_bar(Bin, 42);
otp_7498_bar(<<_A, Rest/bitstring>>, 1) ->
   otp_7498_bar(Rest, 43);
otp_7498_bar(<<>>, 2) ->
   otp_7498_bar(<<>>, 44);
otp_7498_bar(Bin, _I)  ->
   Bin.


match_string(Config) when is_list(Config) ->
    %% To make sure that native endian really is handled correctly
    %% (i.e. that the compiler does not attempt to use bs_match_string/4
    %% instructions for native segments), running this test is not enough.
    %% Either examine the generated for do_match_string_native/1 or
    %% check the coverage for the v3_kernel module.
    case erlang:system_info(endian) of
	little ->
	    do_match_string_native(<<$a,0,$b,0>>);
	big ->
	    do_match_string_native(<<0,$a,0,$b>>)
    end,

    do_match_string_big(<<0,$a,0,$b>>),
    do_match_string_little(<<$a,0,$b,0>>),

    do_match_string_big_signed(<<255,255>>),
    do_match_string_little_signed(<<255,255>>),

    plain = no_match_string_opt(<<"abc">>),
    strange = no_match_string_opt(<<$a:9,$b:9,$c:9>>),

    ok.

do_match_string_native(<<$a:16/native,$b:16/native>>) -> ok.

do_match_string_big(<<$a:16/big,$b:16/big>>) -> ok.

do_match_string_little(<<$a:16/little,$b:16/little>>) -> ok.

do_match_string_big_signed(<<(-1):16/signed>>) -> ok.

do_match_string_little_signed(<<(-1):16/little-signed>>) -> ok.

no_match_string_opt(<<"abc">>) -> plain;
no_match_string_opt(<<$a:9,$b:9,$c:9>>) -> strange.
    

%% OTP-7591: A zero-width segment in matching would crash the compiler.

zero_width(Config) when is_list(Config) ->
    <<Len:16/little, Str:Len/binary, 0:0>> = <<2, 0, $h, $i, 0:0>>,
    2 = Len,
    Str = <<"hi">>,

    %% Match sure that values that cannot fit in a segment will not match.
    case id(<<0:8>>) of
	<<256:8>> -> ct:fail(should_not_match);
	_ -> ok
    end,
    ok.


%% OTP_7650: A invalid size for binary segments could crash the compiler.
bad_size(Config) when is_list(Config) ->
    Tuple = {a,b,c},
    Binary = <<1,2,3>>,
    Atom = an_atom,

    {'EXIT',{{badmatch,<<>>},_}} = (catch <<32:Tuple>> = id(<<>>)),
    {'EXIT',{{badmatch,<<>>},_}} = (catch <<32:Binary>> = id(<<>>)),
    {'EXIT',{{badmatch,<<>>},_}} = (catch <<32:Atom>> = id(<<>>)),

    {'EXIT',{{badmatch,<<>>},_}} = (catch <<42.0:Tuple/float>> = id(<<>>)),
    {'EXIT',{{badmatch,<<>>},_}} = (catch <<42.0:Binary/float>> = id(<<>>)),
    {'EXIT',{{badmatch,<<>>},_}} = (catch <<42.0:Atom/float>> = id(<<>>)),

    %% Matched out value is ignored.
    {'EXIT',{{badmatch,<<>>},_}} = (catch <<_:Binary>> = id(<<>>)),
    {'EXIT',{{badmatch,<<>>},_}} = (catch <<_:Tuple>> = id(<<>>)),
    {'EXIT',{{badmatch,<<>>},_}} = (catch <<_:Atom>> = id(<<>>)),

    no_match = bad_all_size(<<>>),
    no_match = bad_all_size(<<1,2,3>>),

    ok.

bad_all_size(Bin) ->
    Res = bad_all_size_1(Bin),
    Res = bad_all_size_2(Bin),
    Res = bad_all_size_3(Bin),
    Res = bad_all_size_4(Bin),
    Res = bad_all_size_5(Bin),
    Res = bad_all_size_6(Bin),
    Res.

bad_all_size_1(Bin) ->
    case Bin of
        <<B:all/binary>> -> B;
        _ -> no_match
    end.

bad_all_size_2(Bin) ->
    case Bin of
        <<_:all/binary>> -> ok;
        _ -> no_match
    end.

bad_all_size_3(Bin) ->
    All = all,
    case Bin of
        <<B:All/binary>> -> B;
        _ -> no_match
    end.

bad_all_size_4(Bin) ->
    All = all,
    case Bin of
        <<_:All/binary>> -> ok;
        _ -> no_match
    end.

bad_all_size_5(Bin) ->
    All = case 0 of
              0 -> all
          end,
    case Bin of
        <<B:All/binary>> -> B;
        _ -> no_match
    end.

bad_all_size_6(Bin) ->
    All = case 0 of
              0 -> all
          end,
    case Bin of
        <<_:All/binary>> -> ok;
        _ -> no_match
    end.

haystack(Config) when is_list(Config) ->
    <<0:10/unit:8>> = haystack_1(<<0:10/unit:8>>),
    [<<0:10/unit:8>>,
	   <<0:20/unit:8>>] = haystack_2(<<1:8192>>),
    ok.

%% Used to crash the compiler.
haystack_1(Haystack) ->
    Subs = [10],
    [begin
	 <<B:Y/binary>> = Haystack,
	 B
     end || Y <- Subs],
    Haystack.

%% There would be an incorrect badmatch exception.
haystack_2(Haystack) ->
    Subs = [{687,10},{369,20}],
    [begin
	 <<_:X/binary,B:Y/binary,_/binary>> = Haystack,
	 B
     end || {X,Y} <- Subs ].

fc({'EXIT',{function_clause,_}}) -> ok;
fc({'EXIT',{{case_clause,_},_}}) when ?MODULE =:= bs_match_inline_SUITE -> ok.

fc(Name, Args, {'EXIT',{function_clause,[{?MODULE,Name,Args,_}|_]}}) -> ok;
fc(Name, Args, {'EXIT',{function_clause,[{?MODULE,Name,Arity,_}|_]}})
  when length(Args) =:= Arity ->
    true = test_server:is_native(?MODULE);
fc(_, Args, {'EXIT',{{case_clause,ActualArgs},_}})
  when ?MODULE =:= bs_match_inline_SUITE ->
    Args = tuple_to_list(ActualArgs).

%% Cover the clause handling bs_context to binary in
%% beam_block:initialized_regs/2.
cover_beam_bool(Config) when is_list(Config) ->
    ok = do_cover_beam_bool(<<>>, 3),
    <<19>> = do_cover_beam_bool(<<19>>, 2),
    <<42>> = do_cover_beam_bool(<<42>>, 1),
    <<17>> = do_cover_beam_bool(<<13,17>>, 0),
    ok.

do_cover_beam_bool(Bin, X) when X > 0 ->
    if
	X =:= 1; X =:= 2 ->
	    Bin;
	true ->
	    ok
    end;
do_cover_beam_bool(<<_,Bin/binary>>, X) ->
    do_cover_beam_bool(Bin, X+1).

matched_out_size(Config) when is_list(Config) ->
    {253,16#DEADBEEF} = mos_int(<<8,253,16#DEADBEEF:32>>),
    {6,16#BEEFDEAD} = mos_int(<<3,6:3,16#BEEFDEAD:32>>),
    {53,16#CAFEDEADBEEFCAFE} = mos_int(<<16,53:16,16#CAFEDEADBEEFCAFE:64>>),
    {23,16#CAFEDEADBEEFCAFE} = mos_int(<<5,23:5,16#CAFEDEADBEEFCAFE:64>>),

    {<<1,2,3>>,4} = mos_bin(<<3,1,2,3,4,3>>),
    {<<1,2,3,7>>,19,42} = mos_bin(<<4,1,2,3,7,19,4,42>>),
    <<1,2,3,7>> = mos_bin(<<4,1,2,3,7,"abcdefghij">>),

    false = mos_verify_sig(not_a_binary),
    false = mos_verify_sig(<<>>),
    false = mos_verify_sig(<<42:32>>),
    <<"123456789">> = mos_verify_sig(<<77:32,0:77/unit:8,9:32,"123456789">>),

    ok.

mos_int(B) ->
    Res = mos_int_plain(B),
    Res = mos_int_list([B]),
    Res = mos_int_tuple({a,[B],z}),

    Res = mos_int_mixed([B]),
    Res = mos_int_mixed({a,[B],z}),
    42 = mos_int_mixed({30,12}),
    no_match = mos_int_mixed([B,B,B]),

    Res = mos_int_pats1({tag,[B]}, {0,1,2,3,4,5,6,7,8,9}),
    Res = mos_int_pats2({tag,[B]}, {a,a,a,a,a,a,a,a,a,a}, [z]),
    {I,X} = Res,
    Res = mos_int_pats3({tag,[B]}, [I,{X,B,X},I]),
    Res = mos_int_map(#{key => [B]}),
    Key = {my,key},
    Res = mos_int_map(Key, #{Key => [B]}),
    {I,X,B} = mos_int_alias([[B]]),
    Res = {I,X},
    Res = mos_int_try([B]),
    Res = mos_int_receive(B),
    Res = mos_int_fun([B]),
    Res = mos_int_exported(B),
    Res = mos_int_utf(B),
    Res.

mos_int_plain(<<L,I:L,X:32>>) ->
    {I,X};
mos_int_plain(<<L,I:L,X:64>>) ->
    {I,X}.

mos_int_list([<<L,I:L,X:32>>]) ->
    {I,X};
mos_int_list([<<L,I:L,X:64>>]) ->
    {I,X}.

mos_int_tuple({a,[<<L,I:L,X:32>>],z}) ->
    {I,X};
mos_int_tuple({a,[<<L,I:L,X:64>>],z}) ->
    {I,X}.

mos_int_mixed({a,[<<L,I:L,X:32>>],z}) ->
    {I,X};
mos_int_mixed({a,[<<L,I:L,X:64>>],z}) ->
    {I,X};
mos_int_mixed([<<L,I:L,X:32>>]) ->
    {I,X};
mos_int_mixed([<<L,I:L,X:64>>]) ->
    {I,X};
mos_int_mixed({A,B}) when is_integer(A), is_integer(B) ->
    A + B;
mos_int_mixed(_) ->
    no_match.

mos_int_pats1({tag,[<<L,I:L,X:32>>]}, {_,_,_,_,_,_,_,_,_,_}) ->
    {I,X};
mos_int_pats1({tag,[<<L,I:L,X:64>>]}, {_,_,_,_,_,_,_,_,_,_}) ->
    {I,X}.

mos_int_pats2({tag,[<<L,I:L,X:32>>]}, {S,S,S,S,S,S,S,S,S,S}, [_|_]) ->
    {I,X};
mos_int_pats2({tag,[<<L,I:L,X:64>>]}, {S,S,S,S,S,S,S,S,S,S}, [_|_]) ->
    {I,X}.

mos_int_pats3({tag,[<<L,I:L,X:32>>]}, [I,{X,<<L,I:L,X:32>>,X},I]) ->
    {I,X};
mos_int_pats3({tag,[<<L,I:L,X:64>>]}, [I,{X,<<L,I:L,X:64>>,X},I]) ->
    {I,X}.

mos_int_map(#{key := [<<L,I:L,X:32>>]}) ->
    {I,X};
mos_int_map(#{key := [<<L,I:L,X:64>>]}) ->
    {I,X}.

mos_int_map(Key, Map) ->
    case Map of
        #{Key := [<<L,I:L,X:32>>]} -> {I,X};
        #{Key := [<<L,I:L,X:64>>]} -> {I,X}
    end.

mos_int_alias([[<<L,I:L,X:32>> = B]]) ->
    {I,X,B};
mos_int_alias([[<<L,I:L,X:64>> = B]]) ->
    {I,X,B}.

mos_int_try(B) ->
    try id(B) of
        [<<L,I:L,X:32>>] -> {I,X};
        [<<L,I:L,X:64>>] -> {I,X}
    after
        ok
    end.

mos_int_receive(Msg) ->
    Res = (fun() ->
                  self() ! Msg,
                  receive
                      <<L,I:L,X:32>> -> {I,X};
                      <<L,I:L,X:64>> -> {I,X}
                  end
           end)(),
    self() ! Msg,
    Res = receive
              <<L,I:L,X:32>> -> {I,X};
              <<L,I:L,X:64>> -> {I,X}
          end,
    self() ! {tag,[Msg]},
    Res = receive
              {tag,[<<L,I:L,X:32>>]} -> {I,X};
              {tag,[<<L,I:L,X:64>>]} -> {I,X}
          end,
    Res.

mos_int_fun(B) ->
    L = ignore_me,
    F = fun ([<<L,I:L,X:32>>]) -> {I,X};
            ([<<L,I:L,X:64>>]) -> {I,X}
        end,
    F(B).

mos_int_exported(B) ->
    case B of
        <<L,I:L,X:32>> -> ok;
        <<L,I:L,X:64>> -> ok
    end,
    {I,X}.

mos_int_utf(B0) ->
    B = id(<<B0/bits,777/utf8,7777/utf16,9999/utf32>>),
    case B of
        <<L,I:L,X:32,777/utf8,7777/utf16,9999/utf32>> -> {I,X};
        <<L,I:L,X:64,777/utf8,7777/utf16,9999/utf32>> -> {I,X}
    end.

mos_bin(B) ->
    Res = mos_bin_plain(B),
    Res = mos_bin_tuple({outer,{inner,B}}),
    Res.

mos_bin_plain(<<L,Bin:L/binary,X:8,L>>) ->
    L = byte_size(Bin),
    {Bin,X};
mos_bin_plain(<<L,Bin:L/binary,X:8,L,Y:8>>) ->
    L = byte_size(Bin),
    {Bin,X,Y};
mos_bin_plain(<<L,Bin:L/binary,"abcdefghij">>) ->
    L = byte_size(Bin),
    Bin.

mos_bin_tuple({outer,{inner,<<L,Bin:L/binary,X:8,L>>}}) ->
    L = byte_size(Bin),
    {Bin,X};
mos_bin_tuple({outer,{inner,<<L,Bin:L/binary,X:8,L,Y:8>>}}) ->
    L = byte_size(Bin),
    {Bin,X,Y};
mos_bin_tuple({outer,{inner,<<L,Bin:L/binary,"abcdefghij">>}}) ->
    L = byte_size(Bin),
    Bin.

mos_verify_sig(AlgSig) ->
    try
        <<AlgLen:32, _Alg:AlgLen/binary,
          SigLen:32, Sig:SigLen/binary>> = AlgSig,
        Sig
    catch
        _:_ ->
            false
    end.

follow_fail_branch(_) ->
    42 = ffb_1(<<0,1>>, <<0>>),
    8 = ffb_1(<<0,1>>, [a]),
    42 = ffb_2(<<0,1>>, <<0>>, 17),
    8 = ffb_2(<<0,1>>, [a], 0),
    ok.

ffb_1(<<_,T/bitstring>>, List) ->
    case List of
	<<_>> ->
	    42;
	[_|_] ->
	    %% The fail branch of the bs_start_match2 instruction
	    %% pointing to here would be ignored, making the compiler
	    %% incorrectly assume that the delayed sub-binary
	    %% optimization was safe.
	    bit_size(T)
    end.

ffb_2(<<_,T/bitstring>>, List, A) ->
    case List of
	<<_>> when A =:= 17 -> 42;
	[_|_] -> bit_size(T)
    end.

no_partition(_) ->
    one = no_partition_1(<<"string">>, a1),
    {two,<<"string">>} = no_partition_1(<<"string">>, a2),
    {two,<<>>} = no_partition_1(<<>>, a2),
    {two,a} = no_partition_1(a, a2),
    three = no_partition_1(undefined, a3),
    {four,a,[]} = no_partition_1([a], a4),
    {five,a,b} = no_partition_1({a,b}, a5),

    one = no_partition_2(<<"string">>, a1),
    two = no_partition_2(<<"string">>, a2),
    two = no_partition_2(<<>>, a2),
    two = no_partition_2(a, a2),
    three = no_partition_2(undefined, a3),
    four = no_partition_2(42, a4),
    five = no_partition_2([], a5),
    six = no_partition_2(42.0, a6),
    ok.

no_partition_1(<<"string">>, a1) ->
    one;
no_partition_1(V, a2) ->
    {two,V};
no_partition_1(undefined, a3) ->
    three;
no_partition_1([H|T], a4) ->
    {four,H,T};
no_partition_1({A,B}, a5) ->
    {five,A,B}.

no_partition_2(<<"string">>, a1) ->
    one;
no_partition_2(_, a2) ->
    two;
no_partition_2(undefined, a3) ->
    three;
no_partition_2(42, a4) ->
    four;
no_partition_2([], a5) ->
    five;
no_partition_2(42.0, a6) ->
    six.

calling_a_binary(Config) when is_list(Config) ->
    [] = call_binary(<<>>, []),
    {'EXIT',{badarg,_}} = (catch call_binary(<<1>>, [])),
    {'EXIT',{badarg,_}} = (catch call_binary(<<1,2,3>>, [])),
    ok.

call_binary(<<>>, Acc) ->
    Acc;
call_binary(<<H,T/bits>>, Acc) ->
    T(<<Acc/binary,H>>).

binary_in_map(Config) when is_list(Config) ->
    ok = match_binary_in_map(#{key => <<42:8>>}),
    {'EXIT',{{badmatch,#{key := 1}},_}} =
	(catch match_binary_in_map(#{key => 1})),
    {'EXIT',{{badmatch,#{key := <<1023:16>>}},_}} =
	(catch match_binary_in_map(#{key => <<1023:16>>})),
    {'EXIT',{{badmatch,#{key := <<1:8>>}},_}} =
	(catch match_binary_in_map(#{key => <<1:8>>})),
    {'EXIT',{{badmatch,not_a_map},_}} =
	(catch match_binary_in_map(not_a_map)),
    ok.

match_binary_in_map(Map) ->
    case 8 of
	N ->
	    #{key := <<42:N>>} = Map,
	    ok
    end.

match_string_opt(Config) when is_list(Config) ->
    {x,<<1,2,3>>,{<<1>>,{v,<<1,2,3>>}}} =
	do_match_string_opt({<<1>>,{v,<<1,2,3>>}}),
    ok.

do_match_string_opt({<<1>>,{v,V}}=T) ->
    {x,V,T}.

select_on_integer(Config) when is_list(Config) ->
    42 = do_select_on_integer(<<42>>),
    <<"abc">> = do_select_on_integer(<<128,"abc">>),

    {'EXIT',_} = (catch do_select_on_integer(<<0:1>>)),
    {'EXIT',_} = (catch do_select_on_integer(<<1:1>>)),
    {'EXIT',_} = (catch do_select_on_integer(<<0:1,0:15>>)),
    ok.

%% The ASN.1 compiler frequently generates code like this.
do_select_on_integer(<<0:1,I:7>>) ->
    I;
do_select_on_integer(<<1:1,_:7,Bin/binary>>) ->
    Bin.

%% If 'bin_opt_info' was given the warning would lack filename
%% and line number.

map_and_binary(_Config) ->
    {<<"10">>,<<"37">>,<<"am">>} = do_map_and_binary(<<"10:37am">>),
    Map1 = #{time => "noon"},
    {ok,Map1} = do_map_and_binary(Map1),
    Map2 = #{hour => 8, min => 42},
    {8,42,Map2} = do_map_and_binary(Map2),
    ok.

do_map_and_binary(<<Hour:2/bytes, $:, Min:2/bytes, Rest/binary>>) ->
    {Hour, Min, Rest};
do_map_and_binary(#{time := _} = T) ->
    {ok, T};
do_map_and_binary(#{hour := Hour, min := Min} = T) ->
    {Hour, Min, T}.

%% Unsafe caching of branch outcomes in beam_bsm would cause the
%% delayed creation of sub-binaries optimization to be applied even
%% when it was unsafe.

unsafe_branch_caching(_Config) ->
    <<>> = do_unsafe_branch_caching(<<42,1>>),
    <<>> = do_unsafe_branch_caching(<<42,2>>),
    <<>> = do_unsafe_branch_caching(<<42,3>>),
    <<17,18>> = do_unsafe_branch_caching(<<42,3,17,18>>),
    <<>> = do_unsafe_branch_caching(<<1,3,42,2>>),

    ok.

do_unsafe_branch_caching(<<Code/integer, Bin/binary>>) ->
    <<C1/integer, B1/binary>> = Bin,
    case C1 of
	X when X =:= 1 orelse X =:= 2 ->
	    Bin2 = <<>>;
	_ ->
	    Bin2 = B1
    end,
    case Code of
	1 -> do_unsafe_branch_caching(Bin2);
	_ -> Bin2
    end.

bad_literals(_Config) ->
    Mod = list_to_atom(?MODULE_STRING ++ "_" ++
			   atom_to_list(?FUNCTION_NAME)),
    S = [signed_lit_match(V, Sz) || V <- lists:seq(-8, 8),
				    Sz <- [0,1,2,3]] ++
	[unsigned_lit_match(V, Sz) || V <- lists:seq(-2, 8),
				      Sz <- [0,1,2]] ++
	[unicode_match(V) ||
	    V <- [-100,-1,0,1,2|lists:seq(16#10FFFC, 16#110004)]],
    Code = ?Q(["-module('@Mod@').\n"
	       "-export([f/0]).\n"
	       "f() ->\n"
	       "_@S,\n"
	       "ok.\n"]),
    merl:print(Code),
    Opts = test_lib:opt_opts(?MODULE),
    {ok,_} = merl:compile_and_load(Code, Opts),
    Mod:f(),

    {'EXIT',<<42>>} = (catch bad_literals_1()),

    Sz = id(8),
    {'EXIT',{{badmatch,_},_}} = (catch <<-1:Sz>> = <<-1>>),
    ok.

bad_literals_1() ->
    BadSz = bad,
    case case <<42>> of
	     <<42:BadSz>> -> ok;
	     Val -> exit(Val)
	 end of
	ok -> ok;
	error -> error
    end.

signed_lit_match(V, Sz) ->
    case <<V:Sz>> of
	<<V:Sz/signed>> ->
	    ?Q("<<_@V@:_@Sz@/signed>> = <<_@V@:_@Sz@>>");
	_ ->
	    ?Q(["case <<_@V@:_@Sz@>> of\n",
		" <<_@V@:_@Sz@/signed>> ->\n",
		"  ct:fail(should_not_match);\n",
		" _ ->\n",
		"  ok\n",
		"end\n"])
    end.

unsigned_lit_match(V, Sz) ->
    case <<V:Sz>> of
	<<V:Sz/unsigned>> ->
	    ?Q("<<_@V@:_@Sz@>> = <<_@V@:_@Sz@>>");
	_ ->
	    ?Q(["case <<_@V@:_@Sz@>> of\n",
		" <<_@V@:_@Sz@/unsigned>> ->\n",
		"  ct:fail(should_not_match);\n",
		" _ ->\n",
		"  ok\n",
		"end\n"])
    end.

unicode_match(V) ->
    try <<V/utf8>> of
	<<V/utf8>> ->
	    ?Q(["<<_@V@/utf8>> = <<_@V@/utf8>>,\n",
		"<<_@V@/utf16>> = <<_@V@/utf16>>,\n",
		"<<_@V@/utf32>> = <<_@V@/utf32>>\n"])
    catch
	error:badarg ->
	    ?Q(["case <<_@V@:32>> of\n",
		" <<_@V@/utf32>> ->\n",
		"  ct:fail(should_not_match);\n",
		" _ ->\n",
		"  ok\n",
		"end\n"])
    end.

%% Test a few legal but rare cases.

good_literals(_Config) ->
    Sz = id(64),

    %% Variable size.
    <<42:Sz>> = id(<<42:Sz>>),
    <<42.0:Sz/float>> = id(<<42:Sz/float>>),

    %% unit > 1
    <<16#cafebeef:4/unit:8>> = id(<<16#cafebeef:32>>),
    ok.

constant_propagation(_Config) ->
    <<5>> = constant_propagation_a(a, <<5>>),
    {'EXIT',{{case_clause,b},_}} = (catch constant_propagation_a(b, <<5>>)),
    258 = constant_propagation_b(<<1,2>>),
    F = constant_propagation_c(),
    259 = F(<<1,3>>),
    ok.

constant_propagation_a(X, Y) ->
    case X of
	a -> Y2 = 8
    end,
    <<5:Y2>> = Y.

constant_propagation_b(B) ->
    Sz = 16,
    <<X:Sz/integer>> = B,
    X.

constant_propagation_c() ->
    Size = 16,
    fun(Bin) ->
	    <<X:Size/integer>> = Bin,
	    X
    end.

parse_xml(_Config) ->
    <<"<?xmlX">> = do_parse_xml(<<"<?xmlX">>),
    <<" ">> = do_parse_xml(<<"<?xml ">>),
    ok.

do_parse_xml(<<"<?xml"/utf8,Rest/binary>> = Bytes) ->
    %% Delayed sub-binary creation is not safe. A buggy (development)
    %% version of check_liveness_everywhere() in beam_utils would turn
    %% on the optimization.
    Rest1 = case is_next_char_whitespace(Rest) of
		false ->
		    Bytes;
		true ->
		    id(Rest)
	    end,
    id(Rest1).

is_next_char_whitespace(<<C/utf8,_/binary>>) ->
    C =:= $\s.

-record(ext_header,
        {this_hdr = 17,
         ext_hdr_opts}).

get_payload(_Config) ->
    <<3445:48>> = do_get_payload(#ext_header{ext_hdr_opts = <<3445:48>>}),
    {'EXIT',_} = (catch do_get_payload(#ext_header{})),
    ok.

do_get_payload(ExtHdr) ->
    _ = ExtHdr#ext_header.this_hdr,
    ExtHdrOptions = ExtHdr#ext_header.ext_hdr_opts,
    <<_:13,_:35>> = ExtHdr#ext_header.ext_hdr_opts,
    ExtHdrOptions.

escape(_Config) ->
    0 = escape(<<>>, 0),
    1 = escape(<<128>>, 0),
    2 = escape(<<128,255>>, 0),
    42 = escape(<<42>>, 0),
    50 = escape(<<42,8>>, 0),
    ok.

escape(<<Byte, Rest/bits>>, Pos) when Byte >= 127 ->
    escape(Rest, Pos + 1);
escape(<<Byte, Rest/bits>>, Pos) ->
    escape(Rest, Pos + Byte);
escape(<<_Rest/bits>>, Pos) ->
    Pos.

%% ERL-490
num_slots_different(_Config) ->
    Ts = [{<<"de">>, <<"default">>, <<"Remove">>, <<"a">>},
          {<<"de">>, <<"default">>, <<"Remove from list">>, <<"a">>},
          {<<"de">>, <<"default">>, <<"Remove from the list">>, <<"a">>},
          {<<"de">>, <<"default">>, <<"Results">>, <<"Ergebnisse">>},
          {<<"de">>, <<"default">>, <<"Reservatio">>, <<"a">>},
          {<<"de">>, <<"navigation">>, <<"Results">>, <<"Ergebnisse">>},
          {<<"de">>, <<"navigation">>, <<"Resources">>, <<"Ressourcen">>}],
    _ = [{ok,Res} = lgettext(A, B, C) || {A,B,C,Res} <- Ts],

    {'EXIT',_} = (catch lgettext(<<"d">>, <<"default">>, <<"Remove">>)),
    {'EXIT',_} = (catch lgettext("", <<"default">>, <<"Remove">>)),
    {'EXIT',_} = (catch lgettext(<<"de">>, <<"def">>, <<"Remove">>)),
    {'EXIT',_} = (catch lgettext(<<"de">>, <<"default">>, <<"Res">>)),
    ok.


lgettext(<<"de">>, <<"default">>, <<"Remove">>) ->
    {ok, <<"a">>};
lgettext(<<"de">>, <<"default">>, <<"Remove from list">>) ->
    {ok, <<"a">>};
lgettext(<<"de">>, <<"default">>, <<"Remove from the list">>) ->
    {ok, <<"a">>};
lgettext(<<"de">>, <<"default">>, <<"Results">>) ->
    {ok, <<"Ergebnisse">>};
lgettext(<<"de">>, <<"default">>, <<"Reservatio">>) ->
    {ok, <<"a">>};
lgettext(<<"de">>, <<"navigation">>, <<"Results">>) ->
    {ok, <<"Ergebnisse">>};
lgettext(<<"de">>, <<"navigation">>, <<"Resources">>) ->
    {ok, <<"Ressourcen">>}.

%% Test more code in beam_bsm.
beam_bsm(_Config) ->
    true = check_bitstring_list(<<1:1,0:1,1:1,1:1>>, [1,0,1,1]),
    false = check_bitstring_list(<<1:1,0:1,1:1,1:1>>, [0]),

    true = bsm_validate_scheme(<<>>),
    true = bsm_validate_scheme(<<5,10>>),
    false = bsm_validate_scheme(<<5,10,11,12>>),
    true = bsm_validate_scheme([]),
    true = bsm_validate_scheme([5,10]),
    false = bsm_validate_scheme([5,6,7]),

    <<1,2,3>> = bsm_must_save_and_not_save(<<1,2,3>>, []),
    D = fun(N) -> 2*N end,
    [2,4|<<3>>] = bsm_must_save_and_not_save(<<1,2,3>>, [D,D]),

    ok.

check_bitstring_list(<<H:1,T1/bitstring>>, [H|T2]) ->
    check_bitstring_list(T1, T2);
check_bitstring_list(<<>>, []) ->
    true;
check_bitstring_list(_, _) ->
    false.

bsm_validate_scheme([]) -> true;
bsm_validate_scheme([H|T]) ->
    case bsm_is_scheme(H) of
        true -> bsm_validate_scheme(T);
        false -> false
    end;
bsm_validate_scheme(<<>>) -> true;
bsm_validate_scheme(<<H, Rest/binary>>) ->
    case bsm_is_scheme(H) of
        true -> bsm_validate_scheme(Rest);
        false -> false
    end.

bsm_is_scheme(Int) ->
    Int rem 5 =:= 0.

%% NOT OPTIMIZED: different control paths use different positions in the binary
bsm_must_save_and_not_save(Bin, []) ->
    Bin;
bsm_must_save_and_not_save(<<H,T/binary>>, [F|Fs]) ->
    [F(H)|bsm_must_save_and_not_save(T, Fs)];
bsm_must_save_and_not_save(<<>>, []) ->
    [].

guard(_Config) ->
    _Tuple = id({a,b}),
    ok = guard_1(<<1,2,3>>, {1,2,3}),
    ok = guard_2(<<42>>, #{}),
    ok.

%% Cover handling of #k_put{} in v3_codegen:bsm_rename_ctx/4.
guard_1(<<A,B,C>>, Tuple) when Tuple =:= {A,B,C} ->
    ok.

%% Cover handling of #k_call{} in v3_codegen:bsm_rename_ctx/4.
guard_2(<<_>>, Healing) when Healing#{[] => Healing} =:= #{[] => #{}} ->
    ok.

is_ascii(_Config) ->
    true = do_is_ascii(<<>>),
    true = do_is_ascii(<<"string">>),
    false = do_is_ascii(<<1024/utf8>>),
    {'EXIT',{function_clause,_}} = (catch do_is_ascii(<<$A,0:3>>)),
    {'EXIT',{function_clause,_}} = (catch do_is_ascii(<<16#80,0:3>>)),
    ok.

do_is_ascii(<<>>) ->
    true;
do_is_ascii(<<C,_/binary>>) when C >= 16#80 ->
    %% This clause must fail to match if the size of the argument in
    %% bits is not divisible by 8. Beware of unsafe optimizations.
    false;
do_is_ascii(<<_, T/binary>>) ->
    do_is_ascii(T).

non_opt_eq(_Config) ->
    true = non_opt_eq([], <<>>),
    true = non_opt_eq([$a], <<$a>>),
    false = non_opt_eq([$a], <<$b>>),
    ok.

%% An example from the Efficiency Guide. It used to be not optimized,
%% but now it can be optimized.

non_opt_eq([H|T1], <<H,T2/binary>>) ->
    non_opt_eq(T1, T2);
non_opt_eq([_|_], <<_,_/binary>>) ->
    false;
non_opt_eq([], <<>>) ->
    true.

%% ERL-689

erl_689(_Config) ->
    {{0, 0, 0}, <<>>} = do_erl_689_1(<<0>>, ?MODULE),
    {{2018, 8, 7}, <<>>} = do_erl_689_1(<<4,2018:16/little,8,7>>, ?MODULE),
    {{0, 0, 0}, <<>>} = do_erl_689_2(?MODULE, <<0>>),
    {{2018, 8, 7}, <<>>} = do_erl_689_2(?MODULE, <<4,2018:16/little,8,7>>),
    ok.

do_erl_689_1(Arg1, Arg2) ->
    Res = do_erl_689_1a(Arg1, Arg2),
    Res = do_erl_689_1b(Arg1, Arg2).

do_erl_689_2(Arg1, Arg2) ->
    Res = do_erl_689_2a(Arg1, Arg2),
    Res = do_erl_689_2b(Arg1, Arg2).

do_erl_689_1a(<<Length, Data/binary>>, _) ->
    case {Data, Length} of
        {_, 0} ->
            %% bs_context_to_binary would incorrectly set Data to the original
            %% binary (before matching in the function head).
            {{0, 0, 0}, Data};
        {<<Y:16/little, M, D, Rest/binary>>, 4} ->
            {{Y, M, D}, Rest}
    end.

do_erl_689_1b(<<Length, Data/binary>>, _) ->
    case {Data, Length} of
        {_, 0} ->
            %% bs_context_to_binary would incorrectly set Data to the original
            %% binary (before matching in the function head).
            id(0),
            {{0, 0, 0}, Data};
        {<<Y:16/little, M, D, Rest/binary>>, 4} ->
            id(1),
            {{Y, M, D}, Rest}
    end.

do_erl_689_2a(_, <<Length, Data/binary>>) ->
    case {Length, Data} of
        {0, _} ->
            %% bs_context_to_binary would incorrectly set Data to the original
            %% binary (before matching in the function head).
            {{0, 0, 0}, Data};
        {4, <<Y:16/little, M, D, Rest/binary>>} ->
            {{Y, M, D}, Rest}
    end.

do_erl_689_2b(_, <<Length, Data/binary>>) ->
    case {Length, Data} of
        {0, _} ->
            %% bs_context_to_binary would incorrectly set Data to the original
            %% binary (before matching in the function head).
            id(0),
            {{0, 0, 0}, Data};
        {4, <<Y:16/little, M, D, Rest/binary>>} ->
            id(1),
            {{Y, M, D}, Rest}
    end.

%% ERL-753

bs_start_match2_defs(_Config) ->
    {<<"http://127.0.0.1:1234/vsaas/hello">>} = api_url(<<"hello">>),
    {"https://127.0.0.1:4321/vsaas/hello"} = api_url({https, "hello"}).

api_url(URL) ->
    case URL of
        <<_/binary>> -> {<<"http://127.0.0.1:1234/vsaas/",URL/binary>>};
        {https, [_|_] = URL1} -> {"https://127.0.0.1:4321/vsaas/"++URL1}
    end.

check(F, R) ->
    R = F().

%% Make sure that an expression that comes between function start and a match
%% expression passes validation.
expression_before_match(Config) when is_list(Config) ->
    <<_,R/binary>> = id(<<0,1,2,3>>),
    {1, <<2,3>>} = expression_before_match_1(R),
    ok.

expression_before_match_1(R) ->
    A = id(1),
    case R of
        <<1,Bar/binary>> -> {A, Bar};
        <<>> -> {A, baz}
    end.

%% Make sure that context positions are updated on calls.
restore_on_call(Config) when is_list(Config) ->
    ok = restore_on_call_plain(<<0, 1, 2>>),
    <<"x">> = restore_on_call_match(<<0, "x">>),
    ok.

restore_on_call_plain(<<0, Rest/binary>>) ->
    <<2>> = restore_on_call_plain_1(Rest),
    %% {badmatch, <<>>} on missing restore.
    <<2>> = restore_on_call_plain_1(Rest),
    ok.

restore_on_call_plain_1(<<1, Rest/binary>>) -> Rest;
restore_on_call_plain_1(Other) -> Other.

%% Calls a function that moves the match context passed to it, and then matches
%% on its result to confuse the reposition algorithm's success/fail logic.
restore_on_call_match(<<0, Bin/binary>>) ->
    case skip_until_zero(Bin) of
        {skipped, Rest} ->
            Rest;
        not_found ->
            %% The match context did not get repositioned before the
            %% bs_get_tail instruction here.
            Bin
    end.

skip_until_zero(<<0,Rest/binary>>) ->
    {skipped, Rest};
skip_until_zero(<<_C,Rest/binary>>) ->
    skip_until_zero(Rest);
skip_until_zero(_) ->
    not_found.

%% 'catch' must invalidate positions.
restore_after_catch(Config) when is_list(Config) ->
    <<0, 1>> = restore_after_catch_1(<<0, 1>>),
    ok.

restore_after_catch_1(<<A/binary>>) ->
    try throw_after_byte(A) of
        _ -> impossible
    catch
        throw:_Any ->
            %% Will equal <<1>> if the bug is present.
            A
    end.

throw_after_byte(<<_,_/binary>>) ->
    throw(away).

matches_on_parameter(Config) when is_list(Config) ->
    %% This improves coverage for matching on "naked" parameters.
    {<<"urka">>, <<"a">>} = matches_on_parameter_1(<<"gurka">>),
    ok = (catch matches_on_parameter_2(<<"10001110101">>, 0)).

matches_on_parameter_1(Bin) ->
    <<"g", A/binary>> = Bin,
    <<_,_,"rk", B/binary>> = Bin,
    {A, B}.

matches_on_parameter_2(Bin, Offset) ->
    <<_:Offset, Bit:1, Rest/bits>> = Bin,
    case bit_size(Rest) of
        0 -> throw(ok);
        _ -> [Bit | matches_on_parameter_2(Bin, Offset + 1)]
    end.

big_positions(Config) when is_list(Config) ->
    %% This provides coverage for when match context positions no longer fit
    %% into an immediate on 32-bit platforms.

    A = <<0:((1 bsl 27) - 8), $A, 1:1, "gurka", $A>>,
    B = <<0:((1 bsl 27) - 8), $B, "hello", $B>>,

    {a,$A} = bp_start_match(A),
    {b,$B} = bp_start_match(B),
    {a,$A} = bp_getpos(A),
    {b,$B} = bp_getpos(B),

    ok.

%% After the first iteration the context's position will no longer fit into an
%% immediate. To improve performance the bs_start_match3 instruction will
%% return a new context with an updated base position so that we won't have to
%% resort to using bigints.
bp_start_match(<<_:(1 bsl 27),T/bits>>) -> bp_start_match(T);
bp_start_match(<<1:1,"gurka",A>>) -> {a,A};
bp_start_match(<<"hello",B>>) -> {b,B}.

%% This is a corner case where the above didn't work perfectly; if the position
%% was _just_ small enough to fit into an immediate when bs_start_match3 was
%% hit, but too large at bs_get_position, then it must be saved as a bigint.
bp_getpos(<<_:((1 bsl 27) - 8),T/bits>>) -> bp_getpos(T);
bp_getpos(<<A,1:1,"gurka",A>>) -> {a,A};
bp_getpos(<<B,"hello",B>>) -> {b,B}.

matching_meets_apply(_Config) ->
    <<"abc">> = do_matching_meets_apply(<<"/abc">>, []),
    42 = do_matching_meets_apply(<<"">>, {erlang,-42}),
    100 = do_matching_meets_apply(no_binary, {erlang,-100}),
    ok.

do_matching_meets_apply(<<$/, Rest/binary>>, _Handler) ->
    id(Rest);
do_matching_meets_apply(<<_/binary>>=Name, never_matches_a) ->
    %% Used to crash the compiler because variables in a remote
    %% were not handled properly by beam_ssa_bsm.
    Name:foo(gurka);
do_matching_meets_apply(<<_/binary>>=Name, never_matches_b) ->
    %% Another case of the above.
    foo:Name(gurka);
do_matching_meets_apply(_Bin, {Handler, State}) ->
    %% Another case of the above.
    Handler:abs(State).

%% Exception handling was broken on the failure path of bs_start_match as
%% beam_ssa_bsm accidentally cloned and renamed the ?BADARG_BLOCK.
exceptions_after_match_failure(_Config) ->
    {'EXIT', {badarith, _}} = (catch do_exceptions_after_match_failure(atom)),
    ok = do_exceptions_after_match_failure(<<0, 1, "gurka">>),
    ok = do_exceptions_after_match_failure(2.0).

do_exceptions_after_match_failure(<<_A, _B, "gurka">>) ->
    ok;
do_exceptions_after_match_failure(Other) ->
    Other / 2.0,
    ok.

%% ERL-1050: After copying successors, phi nodes on the *original* path could
%% refer to blocks that were only reachable from the copied path.
bad_phi_paths(_Config) ->
    <<"gurka">> = bad_phi_paths_1(id(<<"gurka">>)),
    ok.

bad_phi_paths_1(Arg) ->
    B = case Arg of
            <<_/binary>> -> Arg;
            #{} -> id(Arg)
        end,
    id(B).

id(I) -> I.

expand_and_squeeze(Config) when is_list(Config) ->
    %% UTF8 literals are expanded and then squeezed into integer16
    [
	{test,bs_get_integer2,_,_,[_,{integer,16}|_],_}
	| _
    ] = binary_match_to_asm([
	?Q("<<$á/utf8,_/binary>>"),
	?Q("<<$é/utf8,_/binary>>")
    ]),

    %% Sized integers are expanded and then squeezed into integer16
    [
	{test,bs_get_integer2,_,_,[_,{integer,16}|_],_}
	| _
    ] = binary_match_to_asm([
	?Q("<<0:32,_/binary>>"),
	?Q("<<\"bbbb\",_/binary>>")
    ]),

    %% Groups of 8 bits are squeezed into integer16
    [
	{test,bs_get_integer2,_,_,[_,{integer,16}|_],_}
	| _
    ] = binary_match_to_asm([
	?Q("<<\"aaaa\",_/binary>>"),
	?Q("<<\"bbbb\",_/binary>>")
    ]),

    %% Groups of 8 bits with empty binary are also squeezed
    [
	{test,bs_get_integer2,_,_,[_,{integer,16}|_],_}
	| _
    ] = binary_match_to_asm([
	?Q("<<\"aaaa\",_/binary>>"),
	?Q("<<\"bbbb\",_/binary>>"),
	?Q("<<>>")
    ]),

    %% Groups of 8 bits with float lookup are not squeezed
    [
	{test,bs_get_integer2,_,_,[_,{integer,8}|_],_}
	| _
    ] = binary_match_to_asm([
	?Q("<<\"aaaa\",_/binary>>"),
	?Q("<<\"bbbb\",_/binary>>"),
	?Q("<<_/float>>")
    ]),

    %% Groups of diverse bits go with minimum possible
    [
	{test,bs_get_integer2,_,_,[_,{integer,8}|_],_}
	| _
    ] = binary_match_to_asm([
	?Q("<<\"aa\",_/binary>>"),
	?Q("<<\"bb\",_/binary>>"),
	?Q("<<\"c\",_/binary>>")
    ]),

    %% Groups of diverse bits go with minimum possible but are recursive...
    [
	{test,bs_get_integer2,_,_,[_,{integer,8}|_],_}
	| RestDiverse
    ] = binary_match_to_asm([
	?Q("<<\"aaa\",_/binary>>"),
	?Q("<<\"abb\",_/binary>>"),
	?Q("<<\"c\",_/binary>>")
    ]),

    %% so we still perform a 16 bits lookup for the remaining
    true = lists:any(fun({test,bs_get_integer2,_,_,[_,{integer,16}|_],_}) -> true;
			(_) -> false end, RestDiverse),

    %% Large match is kept as is if there is a sized match later
    [
	{test,bs_get_integer2,_,_,[_,{integer,64}|_],_}
	| _
    ] = binary_match_to_asm([
	?Q("<<255,255,255,255,255,255,255,255>>"),
	?Q("<<_:64>>")
    ]),

    %% Large match is kept as is with large matches before and after
    [
	{test,bs_get_integer2,_,_,[_,{integer,32}|_],_}
	| _
    ] = binary_match_to_asm([
	?Q("<<A:32,_:A>>"),
	?Q("<<0:32>>"),
	?Q("<<_:32>>")
    ]),

    %% Large match is kept as is with large matches before and after
    [
	{test,bs_get_integer2,_,_,[_,{integer,32}|_],_}
	| _
    ] = binary_match_to_asm([
	?Q("<<A:32,_:A>>"),
	?Q("<<0,0,0,0>>"),
	?Q("<<_:32>>")
    ]),

    %% Large match is kept as is with smaller but still large matches before and after
    [
	{test,bs_get_integer2,_,_,[_,{integer,32}|_],_}
	| _
    ] = binary_match_to_asm([
	?Q("<<A:32, _:A>>"),
	?Q("<<0:64>>"),
	?Q("<<_:32>>")
    ]),

    %% There is no squeezing for groups with more than 16 matches
    [
	{test,bs_get_integer2,_,_,[_,{integer,8}|_],_}
	| _
    ] = binary_match_to_asm([
	?Q("<<\"aa\", _/binary>>"),
	?Q("<<\"bb\", _/binary>>"),
	?Q("<<\"cc\", _/binary>>"),
	?Q("<<\"dd\", _/binary>>"),
	?Q("<<\"ee\", _/binary>>"),
	?Q("<<\"ff\", _/binary>>"),
	?Q("<<\"gg\", _/binary>>"),
	?Q("<<\"hh\", _/binary>>"),
	?Q("<<\"ii\", _/binary>>"),
	?Q("<<\"jj\", _/binary>>"),
	?Q("<<\"kk\", _/binary>>"),
	?Q("<<\"ll\", _/binary>>"),
	?Q("<<\"mm\", _/binary>>"),
	?Q("<<\"nn\", _/binary>>"),
	?Q("<<\"oo\", _/binary>>"),
	?Q("<<\"pp\", _/binary>>")
    ]),

    ok.

binary_match_to_asm(Matches) ->
    Clauses = [
	begin
	    Ann = element(2, Match),
	    {clause,Ann,[Match],[],[{integer,Ann,Return}]}
	end || {Match,Return} <- lists:zip(Matches, lists:seq(1, length(Matches)))
    ],

    Module = [
	{attribute,1,module,match_to_asm},
	{attribute,2,export,[{example,1}]},
	{function,3,example,1,Clauses}
    ],

    {ok,match_to_asm,{match_to_asm,_Exports,_Attrs,Funs,_},_} =
	compile:forms(Module, [return, to_asm]),

    [{function,example,1,2,AllInstructions}|_] = Funs,
    [{label,_},{line,_},{func_info,_,_,_},{label,_},{'%',_},
     {test,bs_start_match3,_,_,_,_},{bs_get_position,_,_,_}|Instructions] = AllInstructions,
    Instructions.

many_clauses(_Config) ->
    Mod = list_to_atom(?MODULE_STRING ++ "_" ++
			   atom_to_list(?FUNCTION_NAME)),
    Seq = lists:seq(1, 200),
    S = [one_clause(I) || I <- Seq],
    Code = ?Q(["-module('@Mod@').\n"
	       "-export([f/1]).\n"
	       "f(Bin) ->\n"
	       "case Bin of\n"
               "  dummy -> _@_@S\n"
               "end.\n"]),
    %% merl:print(Code),
    Opts = test_lib:opt_opts(?MODULE),
    {ok,_} = merl:compile_and_load(Code, Opts),
    _ = [begin
             H = erlang:phash2(I),
             Sz = 16,
             <<Res0:Sz>> = <<H:Sz>>,
             Res = I + Res0,
             Res = Mod:f({I,<<Sz:8,H:Sz>>})
         end || I <- Seq],
    ok.

one_clause(I) ->
    ?Q(<<"{_@I@,<<L:8,Val:L>>} -> _@I@ + Val">>).
