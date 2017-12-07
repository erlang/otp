%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2001-2017. All Rights Reserved.
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
-module(guard_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 misc/1,const_cond/1,basic_not/1,complex_not/1,nested_nots/1,
	 semicolon/1,complex_semicolon/1,comma/1,
	 or_guard/1,more_or_guards/1,
	 complex_or_guards/1,and_guard/1,
	 xor_guard/1,more_xor_guards/1,
	 old_guard_tests/1,complex_guard/1,
	 build_in_guard/1,gbif/1,
	 t_is_boolean/1,is_function_2/1,
	 tricky/1,rel_ops/1,rel_op_combinations/1,literal_type_tests/1,
	 basic_andalso_orelse/1,traverse_dcd/1,
	 check_qlc_hrl/1,andalso_semi/1,t_tuple_size/1,binary_part/1,
	 bad_constants/1,bad_guards/1,
         guard_in_catch/1,beam_bool_SUITE/1,
         cover_beam_dead/1]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    test_lib:recompile(?MODULE),
    [{group,p}].

groups() -> 
    [{p,[parallel],
      [misc,const_cond,basic_not,complex_not,nested_nots,
       semicolon,complex_semicolon,comma,or_guard,
       more_or_guards,complex_or_guards,and_guard,xor_guard,
       more_xor_guards,build_in_guard,
       old_guard_tests,complex_guard,gbif,
       t_is_boolean,is_function_2,tricky,
       rel_ops,rel_op_combinations,
       literal_type_tests,basic_andalso_orelse,traverse_dcd,
       check_qlc_hrl,andalso_semi,t_tuple_size,binary_part,
       bad_constants,bad_guards,guard_in_catch,beam_bool_SUITE,
       cover_beam_dead]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


misc(Config) when is_list(Config) ->
    42 = case id(42) of
	     X when -X -> ok;
	     X -> X
	 end,
    {a,b,c} = misc_1([{{a,b,c}},{[4]},{[3]},{-2}]),
    none = misc_1([{{a,b,c}},{[4]},{[3]},{-3}]),
    none = misc_1([{{a,b,c}},{[4]},{[7]},{-2}]),
    none = misc_1([{{a,b,c}},{[4]},{[3]},{[1,2,3]}]),

    {ok,buf,<<>>} = get_data({o,true,raw}, 0, buf),
    {ok,buf,<<>>} = get_data({o,true,raw}, 42, buf),
    {ok,buf,<<>>} = get_data({o,false,raw}, 0, buf),
    error = get_data({o,false,raw}, 42, buf),
    {ok,buf,<<>>} = get_data({o,true,0}, 0, buf),
    {ok,buf,<<>>} = get_data({o,true,0}, 42, buf),
    {ok,buf,<<>>} = get_data({o,false,0}, 0, buf),
    error = get_data({o,false,0}, 42, buf),

    relief = misc_2(0),
    error = misc_2(1),
    error = misc_2(true),

    if
	is_integer(Config) =/= true ->
	    ok
    end,

    true = misc_3(1, 0),
    true = misc_3(0, 0),
    false = misc_3(0, 2),

    %% Abuse of boolean values.

    Zero = id(0),
    One = id(1),
    ok = if (Zero == 0) > false -> ok end,
    ok = if (Zero == 0) =:= (One == 1) -> ok end,
    ok = if (Zero == 0) =:= (One == 1) -> ok end,
    ok = if is_atom(Zero > One) -> ok end,
    error = if abs(Zero > One) -> ok; true -> error end,
    ok = if is_integer(Zero) >= is_integer(One) -> ok end,

    ok.

misc_1([{W},{X},{Y},{Z}]) ->
	      if
	X > Y andalso abs(Z) =:= 2 ->
	    id(W);
	true ->
	    none
    end.

misc_2(0) -> relief;
misc_2(Adapter = 1) when Adapter -> franklin;
misc_2(_) -> error.

misc_3(LenUp, LenDw) ->
    if
	%% Cover handling of #k_alt{}.
	LenUp >= 1 orelse ((LenDw >= 2) xor true) -> true;
	true -> false
    end.

get_data({o,Active,Raw}, BytesToRead, Buffer) 
  when Raw =:= raw; Raw =:= 0 ->
    if 
	Active =/= false orelse BytesToRead =:= 0  ->
	    {ok,Buffer,<<>>};
	true ->
	    error
    end.

const_cond(Config) when is_list(Config) ->
    ok = const_cond({}, 0),
    ok = const_cond({a}, 1),
    error = const_cond({a,b}, 3),
    error = const_cond({a}, 0),
    error = const_cond({a,b}, 1),
    ok.

const_cond(T, Sz) ->
    case T of
	_X when false -> never;
	_X when is_tuple(T), eq == eq, tuple_size(T) == Sz -> ok;
	_X when is_tuple(T), eq == leq, tuple_size(T) =< Sz -> ok;
	_X -> error
    end.

basic_not(Config) when is_list(Config) ->
    True = id(true),
    False = id(false),
    Glurf = id(glurf),
    A = id(5),
    B = id(37.5),
    C = id(-1),
    D = id(5),
    ATuple = {False,True,Glurf},

    check(fun() -> if not false -> ok; true -> error end end, ok),
    check(fun() -> if not true -> ok; true -> error end end, error),
    check(fun() -> if not False -> ok; true -> error end end, ok),
    check(fun() -> if not True -> ok; true -> error end end, error),

    check(fun() -> if A > B -> gt; A < B -> lt; A == B -> eq end end, lt),
    check(fun() -> if A > C -> gt; A < C -> lt; A == C -> eq end end, gt),
    check(fun() -> if A > D -> gt; A < D -> lt; A == D -> eq end end, eq),

    check(fun() -> if not (7 > 453) -> le; not (7 < 453) -> ge;
		      not (7 == 453) -> ne; true -> eq end end, le),
    check(fun() -> if not (7 > -8) -> le; not (7 < -8) -> ge;
		      not (7 == -8) -> ne; true -> eq end end, ge),
    check(fun() -> if not (7 > 7) -> le; not (7 < 7) -> ge;
		      not (7 == 7) -> ne; true -> eq end end, le),

    check(fun() -> if not (A > B) -> le; not (A < B) -> ge;
		      not (A == B) -> ne; true -> eq end end, le),
    check(fun() -> if not (A > C) -> le; not (A < C) -> ge;
		      not (A == C) -> ne; true -> eq end end, ge),
    check(fun() -> if not (A > D) -> le; not (A < D) -> ge;
		      not (A == D) -> ne; true -> eq end end, le),

    check(fun() -> if not element(1, ATuple) -> ok; true -> error end end, ok),
    check(fun() -> if not element(2, ATuple) -> ok; true -> error end end, error),
    check(fun() -> if not element(3, ATuple) -> ok; true -> error end end, error),

    check(fun() -> if not glurf -> ok; true -> error end end, error),
    check(fun() -> if not Glurf -> ok; true -> error end end, error),

    check(fun() -> if not (not true) -> broken end end, broken),

    check(fun() -> if not (True xor True) -> ok end end, ok),
    check(fun() -> if not (True xor False) -> ok;
		      true -> error end end, error),

    ok.

complex_not(Config) when is_list(Config) ->
    ATuple = id({false,true,gurka}),
    check(fun() -> if not(element(1, ATuple)) -> ok; true -> error end end, ok),
    check(fun() -> if not(element(2, ATuple)) -> ok; true -> error end end, error),

    check(fun() -> if not(element(3, ATuple) == gurka) -> ok;
		      true -> error end end, error),
    check(fun() -> if not(element(3, ATuple) =/= gurka) -> ok;
		      true -> error end end, ok),

    check(fun() -> if {a,not(element(2, ATuple))} == {a,false} -> ok;
		      true -> error end end, ok),
    check(fun() -> if {a,not(element(1, ATuple))} == {a,false} -> ok;
		      true -> error end end, error),

    check(fun() -> if not(element(1, ATuple) or element(3, ATuple)) -> ok;
		      true -> error end end, error),

    %% orelse
    check(fun() -> if not(element(1, ATuple) orelse element(3, ATuple)) -> ok;
		      true -> error end end, error),

    %% complex_not_1/4
    ok = complex_not_1(1, 1, 1, a),
    error = complex_not_1(1, 1, 1, []),
    error = complex_not_1(1, 1, 3, a),
    error = complex_not_1(1, 1, 3, []),
    error = complex_not_1(1, 2, 1, a),
    error = complex_not_1(1, 2, 1, []),
    error = complex_not_1(1, 2, 3, a),
    error = complex_not_1(1, 2, 3, []),

    %% complex_not_2/4
    ok = complex_not_2(1, 2, 0, x),
    error = complex_not_2(1, 2, 0, []),
    error = complex_not_2(1, 2, 3, x),
    error = complex_not_2(1, 2, 3, []),
    error = complex_not_2(1, 1, 0, x),
    error = complex_not_2(1, 1, 0, []),
    error = complex_not_2(1, 1, 3, x),
    error = complex_not_2(1, 1, 3, []),

    ok.

complex_not_1(A, B, C, D) ->
    Res = complex_not_1a(A, B, C, D),
    Res = complex_not_1b(A, B, C, D).

complex_not_1a(A, B, C, D)
  when (not (A < B)) andalso (not (B < C)) andalso (not is_list(D)) ->
    ok;
complex_not_1a(_, _, _, _) ->
    error.

complex_not_1b(A, B, C, D)
  when (not (A < B)) and (not (B < C)) and (not is_list(D)) ->
    ok;
complex_not_1b(_, _, _, _) ->
    error.

complex_not_2(A, B, C, D) ->
    Res = complex_not_2a(A, B, C, D),
    Res = complex_not_2b(A, B, C, D).

complex_not_2a(A, B, C, D)
  when A < B andalso not (B < C) andalso not is_list(D) ->
    ok;
complex_not_2a(_, _, _, _) ->
    error.

complex_not_2b(A, B, C, D)
  when A < B, not (B < C), not is_list(D) ->
    ok;
complex_not_2b(_, _, _, _) ->
    error.

nested_nots(Config) when is_list(Config) ->
    true = nested_not_1(0, 0),
    true = nested_not_1(0, 1),
    true = nested_not_1(a, b),
    true = nested_not_1(10, 0),
    false = nested_not_1(z, a),
    false = nested_not_1(3.4, {anything,goes}),
    false = nested_not_1(3.4, atom),
    true = nested_not_1(3.0, [list]),

    true = nested_not_2(false, false, 42),
    true = nested_not_2(false, true, 42),
    true = nested_not_2(true, false, 42),
    true = nested_not_2(true, true, 42),
    true = nested_not_2(false, false, atom),
    false = nested_not_2(false, true, atom),
    false = nested_not_2(true, false, atom),
    false = nested_not_2(true, true, atom),
    ok.

nested_not_1(X, Y) ->
    Res = nested_not_1a(X, Y),
    Res = nested_not_1b(X, Y).

nested_not_1a(X, Y) when not (((X>Y) or not(is_atom(X))) and
			     (is_atom(Y) or (X==3.4))) ->
    true;
nested_not_1a(_, _) ->
    false.

nested_not_1b(X, Y) when not (((X>Y) orelse not(is_atom(X))) andalso
			     (is_atom(Y) orelse (X==3.4))) ->
    true;
nested_not_1b(_, _) ->
    false.

nested_not_2(X, Y, Z) ->
    Res = nested_not_2a(X, Y, Z, true),
    Res = nested_not_2b(X, Y, Z, true).

nested_not_2a(X, Y, Z, True)
  when not(True and not((not(X) and not(Y)) or not(is_atom(Z)))) ->
    true;
nested_not_2a(_, _, _, _) ->
    false.

nested_not_2b(X, Y, Z, True)
  when not(True andalso not((not(X) andalso not(Y)) orelse not(is_atom(Z)))) ->
    true;
nested_not_2b(_, _, _, _) ->
    false.

semicolon(Config) when is_list(Config) ->

    %% True/false combined using ';' (literal atoms).

    check(fun() -> if true; false -> ok end end, ok),
    check(fun() -> if false; true -> ok end end, ok),
    check(fun() -> if true; true -> ok end end, ok),
    check(fun() -> if false; false -> ok; true -> error end end, error),
    check(fun() ->
		  {'EXIT',{if_clause,_}} = (catch if false; false -> ok end),
		  exit
	  end, exit),

    %% True/false combined used ';'.

    True = id(true),
    False = id(false),

    check(fun() -> if True; False -> ok end end, ok),
    check(fun() -> if False; True -> ok end end, ok),
    check(fun() -> if True; True -> ok end end, ok),
    check(fun() -> if False; False -> ok; true -> error end end, error),
    check(fun() ->
		  {'EXIT',{if_clause,_}} = (catch if False; False -> ok end),
		  exit
	  end, exit),

    %% Combine true/false with a non-boolean value.
    Glurf = id(glurf),


    check(fun() -> if True; Glurf -> ok end end, ok),
    check(fun() -> if Glurf; True -> ok end end, ok),
    check(fun() -> if Glurf; Glurf -> ok; true -> error end end, error),
    check(fun() -> if False; Glurf -> ok; true -> error end end, error),
    check(fun() -> if Glurf; False -> ok; true -> error end end, error),
    check(fun() ->
		  {'EXIT',{if_clause,_}} = (catch if Glurf; Glurf -> ok end),
		  exit
	  end, exit),

    %% Combine true/false with errors.

    ATuple = id({false,true,gurka}),

    check(fun() -> if True; element(42, ATuple) -> ok end end, ok),
    check(fun() -> if element(42, ATuple); True -> ok end end, ok),
    check(fun() -> if element(42, ATuple); element(42, ATuple) -> ok;
		      true -> error end end, error),
    check(fun() -> if False; element(42, ATuple) -> ok;
		      true -> error end end, error),
    check(fun() -> if element(42, ATuple);
		      False -> ok; true -> error end end, error),
    check(fun() ->
		  {'EXIT',{if_clause,_}} =
		      (catch if element(42, ATuple);
				element(42, ATuple) -> ok end),
		  exit
	  end, exit),

    ok.

complex_semicolon(Config) when is_list(Config) ->
    ok = csemi1(int, {blurf}),
    ok = csemi1(string, {blurf}),
    ok = csemi1(float, [a]),
    error = csemi1(35, 42),

    %% 2
    ok = csemi2({}, {a,b,c}),
    ok = csemi2({1,3.5}, {a,b,c}),
    ok = csemi2(dum, {a,b,c}),

    ok = csemi2({45,-19.3}, {}),
    ok = csemi2({45,-19.3}, {dum}),
    ok = csemi2({45,-19.3}, {dum,dum}),

    error = csemi2({45}, {dum}),
    error = csemi2([], {dum}),
    error = csemi2({dum}, []),
    error = csemi2([], []),

    %% 3
    csemi3(fun csemi3a/4),
    csemi3(fun csemi3b/4),
    csemi3(fun csemi3c/4),

    %% 4
    csemi4(fun csemi4a/4),
    csemi4(fun csemi4b/4),
    csemi4(fun csemi4c/4),
    csemi4(fun csemi4d/4),

    %% 4, 'orelse' instead of 'or'
    csemi4_orelse(fun csemi4_orelse_a/4),
    csemi4_orelse(fun csemi4_orelse_b/4),
    csemi4_orelse(fun csemi4_orelse_c/4),
    csemi4_orelse(fun csemi4_orelse_d/4),

    %% 5
    error = csemi5(0, 0),
    ok = csemi5(5, 0),
    ok = csemi5(4, -4),
    ok = csemi5(10, -4),

    %% 6
    error = csemi6({a}, 0),
    ok = csemi6({a,b}, 0),
    ok = csemi6({}, 3),
    ok = csemi6({a,b,c}, 3),

    %% 7
    error = csemi7(#{a=>1}, 1, 0),
    error = csemi7(<<>>, 1, 0),
    ok = csemi7(#{a=>1}, 3, 0),
    ok = csemi7(#{a=>1}, 0, 3),
    ok = csemi7(#{a=>1}, 3, 3),
    ok = csemi7(#{a=>1, b=>3}, 0, 0),

    %% 8: Make sure that funs cannot be copied into guards.
    ok = csemi8(true),
    error = csemi8(false),
    error = csemi8(42),

    ok.

csemi1(Type, Val) when is_list(Val), Type == float;
		       Type == int; Type == string -> ok;
csemi1(_, _) -> error.

csemi2(A, B) when tuple_size(A) > 1; tuple_size(B) > 2 -> ok;
csemi2(_, _) -> error.

csemi3(Csemi3) ->
    ok = Csemi3({}, {a,b,c}, [0], [0]),
    ok = Csemi3({1,3.5}, {a,b,c}, -1, -1),
    ok = Csemi3(dum, {a,b,c}, 0.0, 0.0),
    ok = Csemi3(dum, {c}, b, a),
    ok = Csemi3(dum, <<1,2,3>>, 0.0, 0.0),
    ok = Csemi3(<<3.5/float>>, {a,b,c}, -1, -1),

    ok = Csemi3({45,-19.3}, {}, [], []),
    ok = Csemi3({45,-19.3}, {dum}, 42, 42),
    ok = Csemi3({45,-19.3}, {dum,dum}, 33, 33),

    ok = Csemi3({45}, {dum}, 1.0, 0),
    ok = Csemi3([a], {dum}, 1.0, 0),
    ok = Csemi3({dum}, [], 1.0, 0),
    ok = Csemi3([], [], 1.0, 0),
    ok = Csemi3(blurf, {dum}, 1.0, 0),
    ok = Csemi3({a}, blurf, 1.0, 0),
    ok = Csemi3([a], [dum], 1.0, 0),
    ok = Csemi3({dum}, [], 1.0, 0),
    ok = Csemi3([], [], 1.0, 0),

    error = Csemi3({45}, {dum}, 0, 0),
    error = Csemi3([a], {dum}, 0, 0),
    error = Csemi3({dum}, [], 0, 0),
    error = Csemi3([], [], 0, 0),

    ok.

csemi3a(A, B, X, Y) when X > Y; size(A) > 1; size(B) > 2 -> ok;
csemi3a(_, _, _, _) -> error.

csemi3b(A, B, X, Y) when size(A) > 1; X > Y; size(B) > 2 -> ok;
csemi3b(_, _, _, _) -> error.

csemi3c(A, B, X, Y) when size(A) > 1; size(B) > 2; X > Y -> ok;
csemi3c(_, _, _, _) -> error.


csemi4(Test) ->
    ok = Test({a,b}, 2, {c,d}, 2),
    ok = Test({1,2,3}, 0, [], 0),
    ok = Test({}, 2, blurf, 0),
    ok = Test({}, 2, {1}, 2),

    error = Test([], 4, {}, 0),
    error = Test({}, 0, [a,b], 4),
    error = Test({}, 0, [a,b], 0),
    error = Test([], 0, {}, 0),
    error = Test({}, 0, {}, 0),

    ok.

csemi4a(A, X, B, Y) when (tuple_size(A) > 1) or (X > 1);
			 (tuple_size(B) > 1) or (Y > 1) -> ok;
csemi4a(_, _, _, _) -> error.

csemi4b(A, X, B, Y) when (X > 1) or (tuple_size(A) > 1);
			 (tuple_size(B) > 1) or (Y > 1) -> ok;
csemi4b(_, _, _, _) -> error.

csemi4c(A, X, B, Y) when (tuple_size(A) > 1) or (X > 1);
			 (Y > 1) or (tuple_size(B) > 1) -> ok;
csemi4c(_, _, _, _) -> error.
    
csemi4d(A, X, B, Y) when (X > 1) or (tuple_size(A) > 1);
			 (Y > 1) or (tuple_size(B) > 1) -> ok;
csemi4d(_, _, _, _) -> error.


csemi4_orelse(Test) ->
    ok = Test({a,b}, 2, {c,d}, 2),
    ok = Test({1,2,3}, 0, [], 0),
    ok = Test({}, 2, blurf, 0),
    ok = Test({}, 2, {1}, 2),

    error = Test([], 1, {}, 0),

    ok.

csemi4_orelse_a(A, X, B, Y) when (tuple_size(A) > 1) orelse (X > 1);
			 (tuple_size(B) > 1) orelse (Y > 1) -> ok;
csemi4_orelse_a(_, _, _, _) -> error.

csemi4_orelse_b(A, X, B, Y) when (X > 1) orelse (tuple_size(A) > 1);
			 (tuple_size(B) > 1) orelse (Y > 1) -> ok;
csemi4_orelse_b(_, _, _, _) -> error.

csemi4_orelse_c(A, X, B, Y) when (tuple_size(A) > 1) orelse (X > 1);
                           (Y > 1) orelse (tuple_size(B) > 1) -> ok;
csemi4_orelse_c(_, _, _, _) -> error.
    
csemi4_orelse_d(A, X, B, Y) when (X > 1) or (tuple_size(A) > 1);
			 (Y > 1) or (tuple_size(B) > 1) -> ok;
csemi4_orelse_d(_, _, _, _) -> error.

csemi5(A, B) when hd([A+B]) > 1; abs(B) > 2 -> ok;
csemi5(_, _) -> error.

csemi6(A, B) when hd([tuple_size(A)]) > 1; abs(B) > 2 -> ok;
csemi6(_, _) -> error.
    
csemi7(A, B, C) when A#{a:=B} > #{a=>1}; abs(C) > 2 -> ok;
csemi7(_, _, _) -> error.

csemi8(Together) ->
  case fun csemi8/1 of
      Typically when Together; Typically, Together -> ok;
      _ -> error
  end.


comma(Config) when is_list(Config) ->

    %% ',' combinations of literal true/false.

    check(fun() -> if true, false -> ok; true -> error end end, error),
    check(fun() -> if false, true -> ok; true -> error end end, error),
    check(fun() -> if true, true -> ok end end, ok),
    check(fun() -> if false, false -> ok; true -> error end end, error),
    check(fun() ->
		  {'EXIT',{if_clause,_}} =
		      (catch if true, false -> ok;
				false, true -> ok;
				false, false -> ok
			     end),
		  exit
	  end, exit),

    %% ',' combinations of true/false in variables.

    True = id(true),
    False = id(false),

    check(fun() -> if True, False -> ok; true -> error end end, error),
    check(fun() -> if False, True -> ok; true -> error end end, error),
    check(fun() -> if True, True -> ok end end, ok),
    check(fun() -> if False, False -> ok; true -> error end end, error),
    check(fun() ->
		  {'EXIT',{if_clause,_}} =
		      (catch if True, False -> ok;
				False, True -> ok;
				False, False -> ok
			     end),
		  exit
	  end, exit),

    %% ',' combinations of true/false, and non-boolean in variables.

    Glurf = id(glurf),

    check(fun() -> if True, Glurf -> ok; true -> error end end, error),
    check(fun() -> if Glurf, True -> ok; true -> error end end, error),
    check(fun() -> if True, True -> ok end end, ok),
    check(fun() -> if Glurf, Glurf -> ok; true -> error end end, error),
    check(fun() ->
		  {'EXIT',{if_clause,_}} =
		      (catch if True, Glurf -> ok;
				Glurf, True -> ok;
				Glurf, Glurf -> ok
			     end),
		  exit
	  end, exit),

    %% ',' combinations of true/false with errors.
    ATuple = id({a,b,c}),

    check(fun() -> if True, element(42, ATuple) -> ok;
		      true -> error end end, error),
    check(fun() -> if element(42, ATuple), True -> ok;
		      true -> error end end, error),
    check(fun() -> if True, True -> ok end end, ok),
    check(fun() -> if element(42, ATuple), element(42, ATuple) -> ok;
		      true -> error end end, error),
    check(fun() ->
		  {'EXIT',{if_clause,_}} =
		      (catch if True, element(42, ATuple) -> ok;
				element(42, ATuple), True -> ok;
				element(42, ATuple), element(42, ATuple) -> ok
			     end),
		  exit
	  end, exit),
    
    ok.

or_guard(Config) when is_list(Config) ->
    True = id(true),
    False = id(false),
    Glurf = id(glurf),

    %% 'or' combinations of literal true/false.
    check(fun() -> if true or false -> ok end end, ok),
    check(fun() -> if false or true -> ok end end, ok),
    check(fun() -> if true or true -> ok end end, ok),
    check(fun() -> if false or false -> ok; true -> error end end, error),

    check(fun() -> if glurf or true -> ok; true -> error end end, error),
    check(fun() -> if true or glurf -> ok; true -> error end end, error),
    check(fun() -> if glurf or glurf -> ok; true -> error end end, error),

    check(fun() ->
		  {'EXIT',{if_clause,_}} = (catch if false or false -> ok end),
		  exit
	  end, exit),


    %% 'or' combinations using variables containing true/false.
    check(fun() -> if True or False -> ok end end, ok),
    check(fun() -> if False or True -> ok end end, ok),
    check(fun() -> if True or True -> ok end end, ok),
    check(fun() -> if False or False -> ok; true -> error end end, error),

    check(fun() -> if True or Glurf -> ok; true -> error end end, error),
    check(fun() -> if Glurf or True -> ok; true -> error end end, error),
    check(fun() -> if Glurf or Glurf -> ok; true -> error end end, error),

    check(fun() ->
		  {'EXIT',{if_clause,_}} = (catch if False or False -> ok end),
		  exit
	  end, exit),

    ok.

more_or_guards(Config) when is_list(Config) ->
    True = id(true),
    False = id(false),
    ATuple = id({false,true,gurka}),

    check(fun() ->
		  if element(42, ATuple) or False -> ok;
		     true -> error end
	  end, error),

    check(fun() ->
		  if False or element(42, ATuple) -> ok;
		     true -> error end
	  end, error),

    check(fun() ->
		  if element(18, ATuple) or element(42, ATuple) -> ok;
		     true -> error end
	  end, error),

    check(fun() ->
		  if True or element(42, ATuple) -> ok;
		     true -> error end
	  end, error),

    check(fun() ->
		  if element(42, ATuple) or True -> ok;
		     true -> error end
	  end, error),

    check(fun() ->
		  if element(1, ATuple) or element(42, ATuple) or True -> ok;
		     true -> error end
	  end, error),

    check(fun() ->
		  if element(1, ATuple) or True or element(42, ATuple) -> ok;
		     true -> error end
	  end, error),

    check(fun() ->
		  if
		      (<<False:8>> == <<0>>) or element(2, ATuple) -> ok;
		      true -> error end
	  end, error),

    check(fun() ->
		  if
		      element(2, ATuple) or (<<True:8>> == <<1>>) -> ok;
		      true -> error end
	  end, error),

    check(fun() ->
		  if element(2, ATuple) or element(42, ATuple) -> ok;
		     true -> error end
	  end, error),

    check(fun() ->
		  if
		      element(1, ATuple) or
		      element(2, ATuple) or
		      element(19, ATuple) -> ok;
		      true -> error end
	  end, error),
    ok.

complex_or_guards(Config) when is_list(Config) ->
    %% complex_or_1/2
    ok = complex_or_1({a,b,c,d}, {1,2,3}),
    ok = complex_or_1({a,b,c,d}, {1}),
    ok = complex_or_1({a}, {1,2,3}),
    error = complex_or_1({a}, {1}),

    error = complex_or_1(1, 2),
    error = complex_or_1([], {a,b,c,d}),
    error = complex_or_1({a,b,c,d}, []),


    %% complex_or_2/1
    ok = complex_or_2({true,{}}),
    ok = complex_or_2({false,{a}}),
    ok = complex_or_2({false,{a,b,c}}),
    ok = complex_or_2({true,{a,b,c,d}}),

    error = complex_or_2({blurf,{a,b,c}}),
    
    error = complex_or_2({true}),
    error = complex_or_2({true,no_tuple}),
    error = complex_or_2({true,[]}),

    %% complex_or_3/2
    ok = complex_or_3({true}, {}),
    ok = complex_or_3({false}, {a}),
    ok = complex_or_3({false}, {a,b,c}),
    ok = complex_or_3({true}, {a,b,c,d}),
    ok = complex_or_3({false}, <<1,2,3>>),
    ok = complex_or_3({true}, <<1,2,3,4>>),

    error = complex_or_3(blurf, {a,b,c}),

    error = complex_or_3({false}, <<1,2,3,4>>),
    error = complex_or_3([], <<1,2>>),
    error = complex_or_3({true}, 45),
    error = complex_or_3(<<>>, <<>>),

    %% complex_or_4/2
    ok = complex_or_4(<<1,2,3>>, {true}),
    ok = complex_or_4(<<1,2,3>>, {false}),
    ok = complex_or_4(<<1,2,3>>, {true}),
    ok = complex_or_4({1,2,3}, {true}),
    error = complex_or_4({1,2,3,4}, {false}),

    error = complex_or_4(<<1,2,3,4>>, []),
    error = complex_or_4([], {true}),

    %% complex_or_5/2
    ok = complex_or_5(<<1>>, {false}),
    ok = complex_or_5(<<1,2,3>>, {true}),
    ok = complex_or_5(<<1,2,3,4>>, {false}),
    ok = complex_or_5({1,2,3}, {false}),
    ok = complex_or_5({1,2,3,4}, {false}),

    error = complex_or_5(blurf, {false}),
    error = complex_or_5(<<1>>, klarf),
    error = complex_or_5(blurf, klarf),

    %% complex_or_6/2
    ok = complex_or_6({true,true}, {1,2,3,4}),
    ok = complex_or_6({true,true}, <<1,2,3,4>>),
    ok = complex_or_6({false,false}, <<1,2,3,4>>),
    ok = complex_or_6({false,true}, <<1>>),
    ok = complex_or_6({true,false}, {1}),
    ok = complex_or_6({true,true}, {1}),

    error = complex_or_6({false,false}, {1}),

    error = complex_or_6({true}, {1,2,3,4}),
    error = complex_or_6({}, {1,2,3,4}),
    error = complex_or_6([], {1,2,3,4}),
    error = complex_or_6([], {1,2,3,4}),
    error = complex_or_6({true,false}, klurf),

    ok.

complex_or_1(A, B) ->
    if
	((3 < tuple_size(A)) and (tuple_size(A) < 9)) or
	((2 < tuple_size(B)) and (tuple_size(B) < 7)) -> ok;
	true -> error
    end.

complex_or_2(Tuple) ->
    if
	element(1, Tuple) or not (tuple_size(element(2, Tuple)) > 3) -> ok;
	true -> error
    end.

complex_or_3(A, B) ->
    if
	not (size(B) > 3) or element(1, A) -> ok;
	true -> error
    end.

complex_or_4(A, B) ->
    if
	not (is_tuple(A) and (size(A) > 3)) or element(1, B) -> ok;
	true -> error
    end.

complex_or_5(A, B) ->
    if
	not (is_tuple(A) or (size(A) > 3)) or not element(1, B) -> ok;
	true -> error
    end.

complex_or_6(A, B) ->
    if
	not (not element(1, A) and not element(2, A)) or
	not (not (size(B) > 3)) -> ok;
	true -> error
    end.

and_guard(Config) when is_list(Config) ->

    %% 'and' combinations of literal true/false.

    check(fun() -> if true and false -> ok; true -> error end end, error),
    check(fun() -> if false and true -> ok; true -> error end end, error),
    check(fun() -> if true and true -> ok end end, ok),
    check(fun() -> if false and false -> ok; true -> error end end, error),

    check(fun() -> if glurf and true -> ok; true -> error end end, error),
    check(fun() -> if true and glurf -> ok; true -> error end end, error),
    check(fun() -> if glurf and glurf -> ok; true -> error end end, error),

    check(fun() ->
		  {'EXIT',{if_clause,_}} =
		      (catch if true and false -> ok;
				false and true -> ok;
				false and false -> ok
			     end),
		  exit
	  end, exit),

    %% 'and' combinations of true/false in variables.

    True = id(true),
    False = id(false),

    check(fun() -> if True and False -> ok; true -> error end end, error),
    check(fun() -> if False and True -> ok; true -> error end end, error),
    check(fun() -> if True and True -> ok end end, ok),
    check(fun() -> if False and False -> ok; true -> error end end, error),
    check(fun() ->
		  {'EXIT',{if_clause,_}} =
		      (catch if True and False -> ok;
				False and True -> ok;
				False and False -> ok
			     end),
		  exit
	  end, exit),

    %% 'and' combinations of true/false and a non-boolean in variables.

    Glurf = id(glurf),

    check(fun() -> if True and Glurf -> ok; true -> error end end, error),
    check(fun() -> if Glurf and True -> ok; true -> error end end, error),
    check(fun() -> if True and True -> ok end end, ok),
    check(fun() -> if Glurf and Glurf -> ok; true -> error end end, error),
    check(fun() ->
		  {'EXIT',{if_clause,_}} =
		      (catch if True and Glurf -> ok;
				Glurf and True -> ok;
				Glurf and Glurf -> ok
			     end),
		  exit
	  end, exit),

    %% 'and' combinations of true/false with errors.
    ATuple = id({a,b,c}),

    check(fun() -> if True and element(42, ATuple) -> ok;
		      true -> error end end, error),
    check(fun() -> if element(42, ATuple) and True -> ok;
		      true -> error end end, error),
    check(fun() -> if True and True -> ok end end, ok),
    check(fun() -> if element(42, ATuple) and element(42, ATuple) -> ok;
		      true -> error end end, error),
    check(fun() ->
		  {'EXIT',{if_clause,_}} =
		      (catch if True and element(42, ATuple) -> ok;
				element(42, ATuple) and True -> ok;
				element(42, ATuple) and element(42, ATuple) -> ok
			     end),
			exit
		end, exit),

    ok = relprod({'Set',a,b}, {'Set',a,b}),
    
    ok = and_same_var(42),
    {'EXIT',{if_clause,_}} = (catch and_same_var(x)),
    ok.

and_same_var(V) ->
    B = is_integer(V),
    if
	B or B -> ok
    end.

relprod(R1, R2) when (erlang:size(R1) =:= 3) and (erlang:element(1,R1) =:= 'Set'), (erlang:size(R2) =:= 3) and (erlang:element(1,R2) =:= 'Set') ->
    ok.


xor_guard(Config) when is_list(Config) ->

    %% 'xor' combinations of literal true/false.
    check(fun() -> if true xor false -> ok end end, ok),
    check(fun() -> if false xor true -> ok end end, ok),
    check(fun() -> if true xor true -> ok; true -> error end end, error),
    check(fun() -> if false xor false -> ok; true -> error end end, error),
    check(fun() ->
		  {'EXIT',{if_clause,_}} = (catch if false xor false -> ok end),
		  exit
	  end, exit),
    check(fun() ->
		  {'EXIT',{if_clause,_}} = (catch if true xor true -> ok end),
		  exit
	  end, exit),


    %% 'xor' combinations using variables containing true/false.

    True = id(true),
    False = id(false),

    check(fun() -> if True xor False -> ok end end, ok),
    check(fun() -> if False xor True -> ok end end, ok),
    check(fun() -> if True xor True -> ok; true -> error end end, error),
    check(fun() -> if False xor False -> ok; true -> error end end, error),
    check(fun() ->
		  {'EXIT',{if_clause,_}} = (catch if False xor False -> ok end),
		  exit
	  end, exit),
    check(fun() ->
		  {'EXIT',{if_clause,_}} = (catch if True xor True -> ok end),
		  exit
	  end, exit),

    ok.

more_xor_guards(Config) when is_list(Config) ->
    True = id(true),
    False = id(false),
    ATuple = id({false,true,gurka}),

    check(fun() ->
		  if element(42, ATuple) xor False -> ok;
		     true -> error end
	  end, error),

    check(fun() ->
		  if False xor element(42, ATuple) xor False -> ok;
		     true -> error end
	  end, error),

    check(fun() ->
		  if element(18, ATuple) xor element(42, ATuple) -> ok;
		     true -> error end
	  end, error),

    check(fun() ->
		  if True xor element(42, ATuple) -> ok;
		     true -> error end
	  end, error),

    check(fun() ->
		  if element(42, ATuple) xor True -> ok;
		     true -> error end
	  end, error),
    ok.

build_in_guard(Config) when is_list(Config) ->
    SubBin = <<5.0/float>>,
    B = <<1,SubBin/binary,3.5/float>>,
    if
	B =:= <<1,SubBin/binary,3.5/float>> -> ok
    end.

old_guard_tests(Config) when list(Config) ->
    %% Check that all the old guard tests are still recognized.
    list = og(Config),
    atom = og(an_atom),
    binary = og(<<1,2>>),
    float = og(3.14),
    integer = og(43),
    a_function = og(fun() -> ok end),
    pid = og(self()),
    reference = og(make_ref()),
    tuple = og({}),

    number = on(45.333),
    number = on(-19),
    ok.

og(V) when atom(V) -> atom;
og(V) when binary(V) -> binary;
og(V) when float(V) -> float;
og(V) when integer(V) -> integer;
og(V) when function(V) -> a_function;
og(V) when list(V) -> list;
og(V) when pid(V) -> pid;
og(V) when port(V) -> port;
og(V) when reference(V) -> reference;
og(V) when tuple(V) -> tuple;
og(_) -> what.

on(V) when number(V) -> number;
on(_) -> not_number.

complex_guard(_Config) ->
    _ = [true = do_complex_guard(X, Y, Z) ||
	    X <- [4,5], Y <- [4,5], Z <- [4,5]],
    _ = [true = do_complex_guard(X, Y, Z) ||
	    X <- [1,2,3], Y <- [1,2,3], Z <- [1,2,3]],
    _ = [catch do_complex_guard(X, Y, Z) ||
	    X <- [1,2,3,4,5], Y <- [0,6], Z <- [1,2,3,4,5]],
    ok.

do_complex_guard(X1, Y1, Z1) ->
    if
	((X1 =:= 4) or (X1 =:= 5)) and
	((Y1 =:= 4) or (Y1 =:= 5)) and
	((Z1 =:= 4) or (Z1 =:= 5)) or
	((X1 =:= 1) or (X1 =:= 2) or (X1 =:= 3)) and
	((Y1 =:= 1) or (Y1 =:= 2) or (Y1 =:= 3)) and
	((Z1 =:= 1) or (Z1 =:= 2) or (Z1 =:= 3)) ->
	    true
    end.

gbif(Config) when is_list(Config) ->
    error = gbif_1(1, {false,true}),
    ok = gbif_1(2, {false,true}),
    ok.

gbif_1(P, T) when element(P, T) -> ok;
gbif_1(_, _) -> error.


t_is_boolean(Config) when is_list(Config) ->
    true = is_boolean(true),
    true = is_boolean(false),
    true = is_boolean(id(true)),
    true = is_boolean(id(false)),

    false = is_boolean(glurf),
    false = is_boolean(id(glurf)),

    false = is_boolean([]),
    false = is_boolean(id([])),
    false = is_boolean(42),
    false = is_boolean(id(-42)),

    false = is_boolean(math:pi()),
    false = is_boolean(384793478934378924978439789873478934897),

    false = is_boolean(id(self())),
    false = is_boolean(id({x,y,z})),
    false = is_boolean(id([a,b,c])),
    false = is_boolean(id(make_ref())),
    false = is_boolean(id(<<1,2,3>>)),
    false = is_boolean({id(x),y,z}),
    false = is_boolean([id(a),b,c]),

    ok = bool(true),
    ok = bool(false),
    ok = bool(id(true)),
    ok = bool(id(false)),

    error = bool(glurf),
    error = bool(id(glurf)),

    error = bool([]),
    error = bool(id([])),
    error = bool(42),
    error = bool(id(-42)),

    error = bool(math:pi()),
    error = bool(384793478934378924978439789873478934897),

    error = bool(id(self())),
    error = bool(id({x,y,z})),
    error = bool(id([a,b,c])),
    error = bool(id(make_ref())),
    error = bool(id(<<1,2,3>>)),

    true = my_is_bool(true),
    true = my_is_bool(false),
    false = my_is_bool([]),
    false = my_is_bool([1,2,3,4]),
    false = my_is_bool({a,b,c}),

    ok.

bool(X) when is_boolean(X) -> ok;
bool(_) -> error.

my_is_bool(V) ->
    Res = my_is_bool_a(V),
    Res = my_is_bool_b(V).

my_is_bool_a(V) ->
    case V of
	true -> true;
	false -> true;
	_ -> false
    end.

my_is_bool_b(V) ->
    case V of
	false -> true;
	true -> true;
	_ -> false
    end.

is_function_2(Config) when is_list(Config) ->
    true = is_function(id(fun ?MODULE:all/1), 1),
    true = is_function(id(fun() -> ok end), 0),
    false = is_function(id(fun ?MODULE:all/1), 0),
    false = is_function(id(fun() -> ok end), 1),
    {'EXIT',{badarg,_}} =
        (catch is_function(id(fun() -> ok end), -1) orelse error),
    {'EXIT',{badarg,_}} =
        (catch is_function(id(fun() -> ok end), '') orelse error),

    F = fun(_) -> ok end,
    if
	is_function(F, 1) -> ok
    end.

tricky(Config) when is_list(Config) ->
    not_ok = tricky_1(1, 2),
    not_ok = tricky_1(1, blurf),
    not_ok = tricky_1(foo, 2),
    not_ok = tricky_1(a, b),

    error = tricky_2(0.5),
    error = tricky_2(a),
    error = tricky_2({a,b,c}),

    false = rb(100000, [1], 42),
    true = rb(100000, [], 42),
    true = rb(555, [a,b,c], 19),

    error = tricky_3(42),
    error = tricky_3(42.0),
    error = tricky_3(<<>>),
    error = tricky_3(#{}),
    error = tricky_3({a,b}),

    ok.

tricky_1(X, Y) when abs((X == 1) or (Y == 2)) -> ok;
tricky_1(_, _) -> not_ok.

tricky_2(X) when float(X) or float(X) -> ok;
tricky_2(_) -> error.

tricky_3(X)
  when abs(X) or bit_size(X) or byte_size(X) or ceil(X) or
       float(X) or floor(X) or length(X) or
       map_size(X) or node() or node(X) or round(X) or
       self() or size(X) or tl(X) or trunc(X) or tuple_size(X) ->
    ok;
tricky_3(_) ->
    error.

%% From dets_v9:read_buckets/11, simplified.

rb(Size, ToRead, SoFar) when SoFar + Size < 81920; ToRead == [] -> true;
rb(_, _, _) -> false.


-define(T(Op,A,B),
	ok = if A Op B -> ok; true -> error end,
	ok = if not (A Op B) -> error; true -> ok end,
	(fun(X, Y, True, False) -> 
		 ok = if X Op Y -> ok; true -> error end,
		 ok = if False; X Op Y; False -> ok; true -> error end,
		 ok = if X Op Y, True -> ok; true -> error end,
		 ok = if not (X Op Y) -> error; true -> ok end,
		 ok = if False; not (X Op Y); False -> error; true -> ok end
	 end)(id(A), id(B), id(true), id(false))).

-define(F(Op,A,B),
	ok = if A Op B -> error; true -> ok end,
	ok = if not (A Op B) -> ok; true -> error end,
	(fun(X, Y, True, False) ->
		 ok = if X Op Y -> error; true -> ok end,
		 ok = if False; X Op Y; False -> error; true -> ok end,
		 ok = if not (X Op Y); False -> ok; true -> error end,
		 ok = if not (X Op Y), True -> ok; true -> error end
	 end)(id(A), id(B), id(true), id(false))).


rel_ops(Config) when is_list(Config) ->
    ?T(=/=, 1, 1.0),
    ?F(=/=, 2, 2),
    ?F(=/=, {a}, {a}),

    ?F(/=, a, a),
    ?F(/=, 0, 0.0),
    ?T(/=, 0, 1),
    ?F(/=, {a}, {a}),

    ?T(==, 1, 1.0),
    ?F(==, a, {}),

    ?F(=:=, 1, 1.0),
    ?T(=:=, 42.0, 42.0),

    ?F(>, a, b),
    ?T(>, 42, 1.0),
    ?F(>, 42, 42.0),

    ?T(<, a, b),
    ?F(<, 42, 1.0),
    ?F(<, 42, 42.0),

    ?T(=<, 1.5, 5),
    ?F(=<, -9, -100.344),
    ?T(=<, 42, 42.0),

    ?T(>=, 42, 42.0),
    ?F(>=, a, b),
    ?T(>=, 1.0, 0),

    %% Coverage of beam_block:is_exact_eq_ok/1 and collect/1.
    true = any_atom /= id(42),
    true = [] /= id(42),

    %% Coverage of beam_utils:bif_to_test/3
    Empty = id([]),
    ?T(==, [], Empty),

    ok.

-undef(TestOp).

rel_op_combinations(Config) when is_list(Config) ->
    Digits0 = lists:seq(16#0030, 16#0039) ++
	lists:seq(16#0660, 16#0669) ++
	lists:seq(16#06F0, 16#06F9),
    Digits = gb_sets:from_list(Digits0),
    rel_op_combinations_1(16#0700, Digits),

    BrokenRange0 = lists:seq(3, 5) ++
	lists:seq(10, 12) ++ lists:seq(14, 20),
    BrokenRange = gb_sets:from_list(BrokenRange0),
    rel_op_combinations_2(30, BrokenRange),

    Red0 = [{I,2*I} || I <- lists:seq(0, 50)] ++
	[{I,5*I} || I <- lists:seq(51, 80)],
    Red = gb_trees:from_orddict(Red0),
    rel_op_combinations_3(100, Red).

rel_op_combinations_1(0, _) ->
    ok;
rel_op_combinations_1(N, Digits) ->
    Bool = gb_sets:is_member(N, Digits),
    Bool = is_digit_1(N),
    Bool = is_digit_2(N),
    Bool = is_digit_3(N),
    Bool = is_digit_4(N),
    Bool = is_digit_5(N),
    Bool = is_digit_6(N),
    Bool = is_digit_7(N),
    Bool = is_digit_8(N),
    rel_op_combinations_1(N-1, Digits).

is_digit_1(X) when 16#0660 =< X, X =< 16#0669 -> true;
is_digit_1(X) when 16#0030 =< X, X =< 16#0039 -> true;
is_digit_1(X) when 16#06F0 =< X, X =< 16#06F9 -> true;
is_digit_1(_) -> false.

is_digit_2(X) when (16#0030-1) < X, X =< 16#0039 -> true;
is_digit_2(X) when (16#0660-1) < X, X =< 16#0669 -> true;
is_digit_2(X) when (16#06F0-1) < X, X =< 16#06F9 -> true;
is_digit_2(_) -> false.

is_digit_3(X) when 16#0660 =< X, X < (16#0669+1) -> true;
is_digit_3(X) when 16#0030 =< X, X < (16#0039+1) -> true;
is_digit_3(X) when 16#06F0 =< X, X < (16#06F9+1) -> true;
is_digit_3(_) -> false.

is_digit_4(X) when (16#0660-1) < X, X < (16#0669+1) -> true;
is_digit_4(X) when (16#0030-1) < X, X < (16#0039+1) -> true;
is_digit_4(X) when (16#06F0-1) < X, X < (16#06F9+1) -> true;
is_digit_4(_) -> false.

is_digit_5(X) when X >= 16#0660, X =< 16#0669 -> true;
is_digit_5(X) when X >= 16#0030, X =< 16#0039 -> true;
is_digit_5(X) when X >= 16#06F0, X =< 16#06F9 -> true;
is_digit_5(_) -> false.

is_digit_6(X) when X > (16#0660-1), X =< 16#0669 -> true;
is_digit_6(X) when X > (16#0030-1), X =< 16#0039 -> true;
is_digit_6(X) when X > (16#06F0-1), X =< 16#06F9 -> true;
is_digit_6(_) -> false.

is_digit_7(X) when 16#0660 =< X, X =< 16#0669 -> true;
is_digit_7(X) when 16#0030 =< X, X =< 16#003A, X =/= 16#003A -> true;
is_digit_7(X) when 16#06F0 =< X, X =< 16#06F9 -> true;
is_digit_7(_) -> false.

is_digit_8(X) when X =< 16#0039, X > (16#0030-1) -> true;
is_digit_8(X) when X =< 16#06F9, X > (16#06F0-1) -> true;
is_digit_8(X) when X =< 16#0669, X > (16#0660-1) -> true;
is_digit_8(16#0670) -> false;
is_digit_8(_) -> false.

rel_op_combinations_2(0, _) ->
    ok;
rel_op_combinations_2(N, Range) ->
    Bool = gb_sets:is_member(N, Range),
    Bool = broken_range_1(N),
    Bool = broken_range_2(N),
    Bool = broken_range_3(N),
    Bool = broken_range_4(N),
    Bool = broken_range_5(N),
    Bool = broken_range_6(N),
    Bool = broken_range_7(N),
    Bool = broken_range_8(N),
    Bool = broken_range_9(N),
    Bool = broken_range_10(N),
    Bool = broken_range_11(N),
    Bool = broken_range_12(N),
    Bool = broken_range_13(N),
    rel_op_combinations_2(N-1, Range).

broken_range_1(X) when X >= 10, X =< 20, X =/= 13 -> true;
broken_range_1(X) when X >= 3, X =< 5 -> true;
broken_range_1(_) -> false.

broken_range_2(X) when X >= 10, X =< 12 -> true;
broken_range_2(X) when X >= 14, X =< 20 -> true;
broken_range_2(X) when X >= 3, X =< 5 -> true;
broken_range_2(_) -> false.

broken_range_3(X) when X >= 10, X =< 12 -> true;
broken_range_3(X) when X >= 14, X < 21 -> true;
broken_range_3(3) -> true;
broken_range_3(4) -> true;
broken_range_3(5) -> true;
broken_range_3(_) -> false.

broken_range_4(X) when X =< 5, X >= 3 -> true;
broken_range_4(X) when X >= 10, X =< 20, X =/= 13 -> true;
broken_range_4(X) when X =< 100 -> false;
broken_range_4(_) -> false.

broken_range_5(X) when X >= 10, X =< 20, X =/= 13 -> true;
broken_range_5(X) when X > 2, X =< 5 -> true;
broken_range_5(_) -> false.

broken_range_6(X) when X >= 10, X =< 20, X =/= 13 -> true;
broken_range_6(X) when X > 2, X < 6 -> true;
broken_range_6(_) -> false.

broken_range_7(X) when X > 2, X < 6 -> true;
broken_range_7(X) when X >= 10, X =< 20, X =/= 13 -> true;
broken_range_7(X) when X > 30 -> false;
broken_range_7(_) -> false.

broken_range_8(X) when X >= 10, X =< 20, X =/= 13 -> true;
broken_range_8(X) when X =:= 3 -> true;
broken_range_8(X) when X >= 3, X =< 5 -> true;
broken_range_8(_) -> false.

broken_range_9(X) when X >= 10, X =< 20, X =/= 13 -> true;
broken_range_9(X) when X =:= 13 -> false;
broken_range_9(X) when X >= 3, X =< 5 -> true;
broken_range_9(_) -> false.

broken_range_10(X) when X >= 3, X =< 5 -> true;
broken_range_10(X) when X >= 10, X =< 20, X =/= 13 -> true;
broken_range_10(X) when X =/= 13 -> false;
broken_range_10(_) -> false.

broken_range_11(X) when X >= 10, X =< 20, X =/= 13 -> true;
broken_range_11(X) when is_tuple(X), X =:= 10 -> true;
broken_range_11(X) when X >= 3, X =< 5 -> true;
broken_range_11(_) -> false.

broken_range_12(X) when X >= 3, X =< 5 -> true;
broken_range_12(X) when X >= 10, X =< 20, X =/= 13 -> true;
broken_range_12(X) when X < 30, X > 20 -> false;
broken_range_12(_) -> false.

broken_range_13(X) when X >= 10, X =< 20, 13 =/= X -> true;
broken_range_13(X) when X >= 3, X =< 5 -> true;
broken_range_13(_) -> false.

rel_op_combinations_3(0, _) ->
    ok;
rel_op_combinations_3(N, Red) ->
    Val = case gb_trees:lookup(N, Red) of
	      none -> none;
	      {value,V} -> V
	  end,
    Val = redundant_1(N),
    Val = redundant_2(N),
    Val = redundant_3(N),
    Val = redundant_4(N),
    Val = redundant_5(N),
    Val = redundant_6(N),
    Val = redundant_7(N),
    Val = redundant_8(N),
    Val = redundant_9(N),
    Val = redundant_10(N),
    Val = redundant_11(N),
    rel_op_combinations_3(N-1, Red).

redundant_1(X) when X >= 51, X =< 80 -> 5*X;
redundant_1(X) when X < 51 -> 2*X;
redundant_1(_) -> none.

redundant_2(X) when X < 51 -> 2*X;
redundant_2(X) when X >= 51, X =< 80 -> 5*X;
redundant_2(_) -> none.

redundant_3(X) when X < 51 -> 2*X;
redundant_3(X) when X =< 80, X >= 51 -> 5*X;
redundant_3(X) when X =/= 100 -> none;
redundant_3(_) -> none.

redundant_4(X) when X < 51 -> 2*X;
redundant_4(X) when X =< 80, X > 50 -> 5*X;
redundant_4(X) when X =/= 100 -> none;
redundant_4(_) -> none.

redundant_5(X) when X < 51 -> 2*X;
redundant_5(X) when X > 50, X < 81 -> 5*X;
redundant_5(X) when X =< 10 -> none;
redundant_5(_) -> none.

redundant_6(X) when X > 50, X =< 80 -> 5*X;
redundant_6(X) when X < 51 -> 2*X;
redundant_6(_) -> none.

redundant_7(X) when is_integer(X), X >= 51, X =< 80 -> 5*X;
redundant_7(X) when is_integer(X), X < 51 -> 2*X;
redundant_7(_) -> none.

redundant_8(X) when X >= 51, X =< 80 -> 5*X;
redundant_8(X) when X < 51 -> 2*X;
redundant_8(_) -> none.

redundant_9(X) when X >= 51, X =< 80 -> 5*X;
redundant_9(X) when X < 51 -> 2*X;
redundant_9(90) -> none;
redundant_9(X) when X =/= 90 -> none;
redundant_9(_) -> none.

redundant_10(X) when X >= 51, X =< 80 -> 5*X;
redundant_10(X) when X < 51 -> 2*X;
redundant_10(90) -> none;
redundant_10(X) when X =:= 90 -> none;
redundant_10(_) -> none.

redundant_11(X) when X < 51 -> 2*X;
redundant_11(X) when X =:= 10 -> 2*X;
redundant_11(X) when X >= 51, X =< 80 -> 5*X;
redundant_11(_) -> none.

%% Test type tests on literal values. (From emulator test suites.)
literal_type_tests(Config) when is_list(Config) ->
    case ?MODULE of
	guard_SUITE -> literal_type_tests_1(Config);
	_ -> {skip,"Enough to run this case once."}
    end.

literal_type_tests_1(Config) ->
    %% Generate an Erlang module with all different type of type tests.
    Tests = make_test([{T,L} || T <- type_tests(), L <- literals()] ++
			    [{is_function,L1,L2} || 
				L1 <- literals(), L2 <- literals()]),
    Mod = literal_test,
    Anno = erl_anno:new(0),
    Func = {function, Anno, test, 0, [{clause,Anno,[],[],Tests}]},
    Form = [{attribute,Anno,module,Mod},
            {attribute,Anno,compile,export_all},
            Func, {eof,999}],

    %% Print generated code for inspection.
    lists:foreach(fun (F) -> io:put_chars([erl_pp:form(F),"\n"]) end, Form),

    %% Test compile:form/1.  This implies full optimization (default).
    {ok,Mod,Code1} = compile:forms(Form),
    smoke_disasm(Config, Mod, Code1),
    {module,Mod} = code:load_binary(Mod, Mod, Code1),
    Mod:test(),
    true = code:delete(Mod),
    code:purge(Mod),
			       
    %% Test compile:form/2.  Turn off all optimizations.
    {ok,Mod,Code2} = compile:forms(Form, [binary,report,time,
						no_copt,no_postopt]),
    smoke_disasm(Config, Mod, Code2),
    {module,Mod} = code:load_binary(Mod, Mod, Code2),
    Mod:test(),
    true = code:delete(Mod),
    code:purge(Mod),
    ok.

make_test([{T,L1,L2}|Ts]) ->
    [test(T, L1, L2)|make_test(Ts)];
make_test([{T,L}|Ts]) ->
    [test(T, L)|make_test(Ts)];
make_test([]) -> [].

test(T, L) ->
    S0 = io_lib:format("begin io:format(\"~~p~n\", [{~p,~p}]), if ~w(~w) -> true; true -> false end end. ", [T,L,T,L]),
    S = lists:flatten(S0),
    {ok,Toks,_Line} = erl_scan:string(S),
    {ok,E} = erl_parse:parse_exprs(Toks),
    {value,Val,_Bs} = erl_eval:exprs(E, []),
    Anno = erl_anno:new(0),
    {match,Anno,{atom,Anno,Val},hd(E)}.

test(T, L1, L2) ->
    S0 = io_lib:format("begin io:format(\"~~p~n\", [{~p,~p,~p}]), if ~w(~w, ~w) -> true; true -> false end end. ", [T,L1,L2,T,L1,L2]),
    S = lists:flatten(S0),
    {ok,Toks,_Line} = erl_scan:string(S),
    {ok,E} = erl_parse:parse_exprs(Toks),
    {value,Val,_Bs} = erl_eval:exprs(E, []),
    Anno = erl_anno:new(0),
    {match,Anno,{atom,Anno,Val},hd(E)}.

smoke_disasm(Config, Mod, Bin) ->
    Priv = proplists:get_value(priv_dir, Config),
    File = filename:join(Priv, atom_to_list(Mod)++".beam"),
    ok = file:write_file(File, Bin),
    test_lib:smoke_disasm(File).

literals() ->
    [42,
     3.14,
     -3,
     32982724987789283473473838474,
     [],
     xxxx,
     {a,b,c},
     [a,list],
     <<1,2,3>>,
     <<42:17>>].

type_tests() ->
    [is_boolean,
     is_integer,
     is_float,
     is_number,
     is_atom,
     is_list,
     is_tuple,
     is_pid,
     is_reference,
     is_port,
     is_binary,
     is_bitstring,
     is_function,
     is_map].

basic_andalso_orelse(Config) when is_list(Config) ->
    T = id({type,integers,23,42}),
    65 = if
	     ((element(1, T) =:= type) andalso (tuple_size(T) =:= 4) andalso
	      element(2, T)) == integers ->
		 element(3, T) + element(4, T);
	     true -> error
	 end,
    65 = case [] of
	     [] when ((element(1, T) =:= type) andalso (tuple_size(T) =:= 4) andalso
		      element(2, T)) == integers ->
		 element(3, T) + element(4, T)
	 end,

    42 = basic_rt({type,integers,40,2}),
    5.0 = basic_rt({vector,{3.0,4.0}}),
    20 = basic_rt(['+',3,7]),
    {'Set',a,b} = basic_rt({{'Set',a,b},{'Set',a,b}}),
    12 = basic_rt({klurf,4}),

    error = basic_rt({type,integers,40,2,3}),
    error = basic_rt({kalle,integers,40,2}),
    error = basic_rt({kalle,integers,40,2}),
    error = basic_rt({1,2}),
    error = basic_rt([]),

    RelProdBody =
	fun(R1, R2) ->
		if
		    (erlang:size(R1) =:= 3) andalso (erlang:element(1,R1) =:= 'Set'),
		    (erlang:size(R2) =:= 3) andalso (erlang:element(1,R2) =:= 'Set') ->
			ok
		end
	end,

    ok = RelProdBody({'Set',a,b}, {'Set',a,b}),

    %% 'andalso'/'orelse' with calls known to fail already at compile time.
    %% Used to crash the code generator.
    error = (fun() ->
		     R = {vars,true},
		     if
			 is_record(R, vars, 2) andalso element(99, R) -> ok;
			 true -> error
		     end
	     end)(),
    error = (fun(X) ->
		     L = {a,b,c},
		     if
			 is_list(X) andalso length(L) > 4 -> ok;
			 true -> error
		     end
	     end)([]),
    ok.

basic_rt(T) when is_tuple(T) andalso tuple_size(T) =:= 4 andalso element(1, T) =:= type andalso
		 element(2, T) == integers ->
    element(3, T) + element(4, T);
basic_rt(T) when is_tuple(T) andalso tuple_size(T) =:= 2 andalso element(1, T) =:= vector ->
    {X,Y} = element(2, T),
    if
	is_float(X), is_float(Y) ->
	    math:sqrt(X*X+Y*Y)
    end;
basic_rt(['+',A,B]) ->
    2*id(A+B);
basic_rt({R1,R2}) when erlang:size(R1) =:= 3 andalso erlang:element(1,R1) =:= 'Set',
		       erlang:size(R2) =:= 3 andalso erlang:element(1,R2) =:= 'Set' ->
    R1 = id(R1),
    R2 = id(R2),
    R1;
basic_rt(T) when is_tuple(T) andalso tuple_size(T) =:= 2 andalso element(1, T) =:= klurf ->
    3*id(element(2, T));
basic_rt(_) ->
    error.

traverse_dcd(Config) when is_list(Config) ->
    L0 = [{log_header,dcd_log,"1.0",a,b,c},{log_header,dcd_log,"2.0",a,b,c},
	  {log_header,dcd_log,"0.0",a,b,c},blurf],
    {cont,[{log_header,dcd_log,"0.0",a,b,c},blurf],log,funny} = 
	traverse_dcd({cont,L0}, log, funny),
    L1 = [{log_header,dcd_log,"1.0"}],
    {cont,L1,log,funny} = traverse_dcd({cont,L1}, log, funny),
    L2 = [{a,tuple}],
    {cont,L2,log,funny} = traverse_dcd({cont,L2}, log, funny),
    ok.

%% The function starts out with 3 arguments in {x,0}, {x,1}, {x,2}.
%% The outer match of a two tuple will places the first element in {x,3} and
%% second in {x,4}. The guard for the first clause must make ensure that all of those
%% registers are restored before entering the second clause.
%%
%% (From mnesia_checkpoint.erl, modified.)

traverse_dcd({Cont,[LogH|Rest]},Log,Fun)
  when is_tuple(LogH) andalso tuple_size(LogH) =:= 6 andalso element(1, LogH) =:= log_header
andalso erlang:element(2,LogH) == dcd_log,
is_tuple(LogH) andalso tuple_size(LogH) =:= 6 andalso element(1, LogH) =:= log_header
andalso erlang:element(3,LogH) >= "1.0" ->
    traverse_dcd({Cont,Rest},Log,Fun);
traverse_dcd({Cont,Recs},Log,Fun) ->
    {Cont,Recs,Log,Fun}.


check_qlc_hrl(Config) when is_list(Config) ->
    St = {r1,false,dum},
    foo = cqlc(qlc, q, [{lc,1,2,3}], St),
    foo = cqlc(qlc, q, [{lc,1,2,3},b], St),
    St = cqlc(qlc, q, [], St),
    St = cqlc(qlc, blurf, [{lc,1,2,3},b], St),
    St = cqlc(q, q, [{lc,1,2,3},b], St),
    St = cqlc(qlc, q, [{lc,1,2,3},b,c], St),
    St = cqlc(qlc, q, [a,b], St),
    {r1,true,kalle} = cqlc(qlc, q, [{lc,1,2,3},b], {r1,true,kalle}),
    ok.

%% From erl_lint.erl; original name was check_qlc_hrl/4.
cqlc(M, F, As, St) ->
    Arity = length(As),
    case As of
        [{lc,_L,_E,_Qs}|_] when M =:= qlc, F =:= q, 
                                Arity < 3, 
                                not (((element(1, St) =:= r1) orelse fail) and (tuple_size(St) =:= 3) and element(2, St)) ->
            foo;
        _ ->
            St
    end.

%% OTP-7679: Thanks to Hunter Morris.
andalso_semi(Config) when is_list(Config) ->
    ok = andalso_semi_foo(0),
    ok = andalso_semi_foo(1),
    fc(catch andalso_semi_foo(2)),

    ok = andalso_semi_bar([a,b,c]),
    ok = andalso_semi_bar(1),
    fc(catch andalso_semi_bar([a,b])),
    ok.

andalso_semi_foo(Bar) when is_integer(Bar) andalso Bar =:= 0; Bar =:= 1 ->
   ok.

andalso_semi_bar(Bar) when is_list(Bar) andalso length(Bar) =:= 3; Bar =:= 1 ->
   ok.


t_tuple_size(Config) when is_list(Config) ->
    10 = do_tuple_size({1,2,3,4}),
    fc(catch do_tuple_size({1,2,3})),
    fc(catch do_tuple_size(42)),

    error = ludicrous_tuple_size({a,b,c}),
    error = ludicrous_tuple_size([a,b,c]),

    %% Test the "unsafe case" - the register assigned the tuple size is
    %% not killed.
    DataDir = test_lib:get_data_dir(Config),
    File = filename:join(DataDir, "guard_SUITE_tuple_size"),
    {ok,Mod,Code} = compile:file(File, [from_asm,binary]),
    code:load_binary(Mod, File, Code),
    14 = Mod:t({1,2,3,4}),
    _ = code:delete(Mod),
    _ = code:purge(Mod),
    
    ok.

do_tuple_size(T) when tuple_size(T) =:= 4 ->
    {A,B,C,D} = T,
    A+B+C+D.

ludicrous_tuple_size(T)
  when tuple_size(T) =:= 16#7777777777777777777777777777777777 -> ok;
ludicrous_tuple_size(T)
  when tuple_size(T) =:= 16#10000000000000000 -> ok;
ludicrous_tuple_size(T)
  when tuple_size(T) =:= (1 bsl 64) - 1 -> ok;
ludicrous_tuple_size(T)
  when tuple_size(T) =:= 16#FFFFFFFFFFFFFFFF -> ok;
ludicrous_tuple_size(_) -> error.

%%
%% The binary_part/2,3 guard BIFs
%%
-define(MASK_ERROR(EXPR),mask_error((catch (EXPR)))).
mask_error({'EXIT',{Err,_}}) ->
    Err;
mask_error(Else) ->
    Else.

%% Test the binary_part/2,3 guard (GC) BIFs.
binary_part(Config) when is_list(Config) ->
    %% This is more or less a copy of what the guard_SUITE in emulator
    %% does to cover the guard bif's
    1 = bptest(<<1,2,3>>),
    2 = bptest(<<2,1,3>>),
    error = bptest(<<1>>),
    error = bptest(<<>>),
    error = bptest(apa),
    3 = bptest(<<2,3,3>>),
    % With one variable (pos)
    1 = bptest(<<1,2,3>>,1),
    2 = bptest(<<2,1,3>>,1),
    error = bptest(<<1>>,1),
    error = bptest(<<>>,1),
    error = bptest(apa,1),
    3 = bptest(<<2,3,3>>,1),
    % With one variable (length)
    1 = bptesty(<<1,2,3>>,1),
    2 = bptesty(<<2,1,3>>,1),
    error = bptesty(<<1>>,1),
    error = bptesty(<<>>,1),
    error = bptesty(apa,1),
    3 = bptesty(<<2,3,3>>,2),
    % With one variable (whole tuple)
    1 = bptestx(<<1,2,3>>,{1,1}),
    2 = bptestx(<<2,1,3>>,{1,1}),
    error = bptestx(<<1>>,{1,1}),
    error = bptestx(<<>>,{1,1}),
    error = bptestx(apa,{1,1}),
    3 = bptestx(<<2,3,3>>,{1,2}),
    % With two variables
    1 = bptest(<<1,2,3>>,1,1),
    2 = bptest(<<2,1,3>>,1,1),
    error = bptest(<<1>>,1,1),
    error = bptest(<<>>,1,1),
    error = bptest(apa,1,1),
    3 = bptest(<<2,3,3>>,1,2),
    % Direct (autoimported) call, these will be evaluated by the compiler...
    <<2>> = binary_part(<<1,2,3>>,1,1),
    <<1>> = binary_part(<<2,1,3>>,1,1),
    % Compiler warnings due to constant evaluation expected (3)
    badarg = ?MASK_ERROR(binary_part(<<1>>,1,1)),
    badarg = ?MASK_ERROR(binary_part(<<>>,1,1)),
    badarg = ?MASK_ERROR(binary_part(apa,1,1)),
    <<3,3>> = binary_part(<<2,3,3>>,1,2),
    % Direct call through apply
    <<2>> = apply(erlang,binary_part,[<<1,2,3>>,1,1]),
    <<1>> = apply(erlang,binary_part,[<<2,1,3>>,1,1]),
    % Compiler warnings due to constant evaluation expected (3)
    badarg = ?MASK_ERROR(apply(erlang,binary_part,[<<1>>,1,1])),
    badarg = ?MASK_ERROR(apply(erlang,binary_part,[<<>>,1,1])),
    badarg = ?MASK_ERROR(apply(erlang,binary_part,[apa,1,1])),
    <<3,3>> = apply(erlang,binary_part,[<<2,3,3>>,1,2]),
    % Constant propagation
     Bin = <<1,2,3>>,
     ok = if
		    binary_part(Bin,1,1) =:= <<2>> ->
			ok;
		    %% Compiler warning, clause cannot match (expected)
		    true ->
			error
		end,
     ok = if
		    binary_part(Bin,{1,1}) =:= <<2>> ->
			ok;
		    %% Compiler warning, clause cannot match (expected)
		    true ->
			error
		end,
    ok.


bptest(B) when length(B) =:= 1337 ->
    1;
bptest(B) when binary_part(B,{1,1}) =:= <<2>> ->
    1;
bptest(B) when erlang:binary_part(B,1,1) =:= <<1>> ->
    2;
bptest(B)  when erlang:binary_part(B,{1,2}) =:= <<3,3>> ->
    3;
bptest(_) ->
    error.

bptest(B,A) when length(B) =:= A ->
    1;
bptest(B,A) when binary_part(B,{A,1}) =:= <<2>> ->
    1;
bptest(B,A) when erlang:binary_part(B,A,1) =:= <<1>> ->
    2;
bptest(B,A)  when erlang:binary_part(B,{A,2}) =:= <<3,3>> ->
    3;
bptest(_,_) ->
    error.

bptestx(B,A) when length(B) =:= A ->
    1;
bptestx(B,A) when binary_part(B,A) =:= <<2>> ->
    1;
bptestx(B,A) when erlang:binary_part(B,A) =:= <<1>> ->
    2;
bptestx(B,A)  when erlang:binary_part(B,A) =:= <<3,3>> ->
    3;
bptestx(_,_) ->
    error.

bptesty(B,A) when length(B) =:= A ->
    1;
bptesty(B,A) when binary_part(B,{1,A}) =:= <<2>> ->
    1;
bptesty(B,A) when erlang:binary_part(B,1,A) =:= <<1>> ->
    2;
bptesty(B,A)  when erlang:binary_part(B,{1,A}) =:= <<3,3>> ->
    3;
bptesty(_,_) ->
    error.

bptest(B,A,_C) when length(B) =:= A ->
    1;
bptest(B,A,C) when binary_part(B,{A,C}) =:= <<2>> ->
    1;
bptest(B,A,C) when erlang:binary_part(B,A,C) =:= <<1>> ->
    2;
bptest(B,A,C)  when erlang:binary_part(B,{A,C}) =:= <<3,3>> ->
    3;
bptest(_,_,_) ->
    error.

-define(FAILING(C),
	if
	    C -> ct:fail(should_fail);
	    true -> ok
	end,
	if
	    true, C -> ct:fail(should_fail);
	    true -> ok
	end).

bad_constants(Config) when is_list(Config) ->
    ?FAILING(false),
    ?FAILING([]),
    ?FAILING([a]),
    ?FAILING([Config]),
    ?FAILING({a,b}),
    ?FAILING({a,Config}),
    ?FAILING(<<1>>),
    ?FAILING(42),
    ?FAILING(3.14),
    ok.

bad_guards(Config) when is_list(Config) ->
    if erlang:float(self()); true -> ok end,

    fc(catch bad_guards_1(1, [])),
    fc(catch bad_guards_1(1, [2])),
    fc(catch bad_guards_1(atom, [2])),

    fc(catch bad_guards_2(#{a=>0,b=>0}, [])),
    fc(catch bad_guards_2(#{a=>0,b=>0}, [x])),
    fc(catch bad_guards_2(not_a_map, [x])),
    fc(catch bad_guards_2(42, [x])),

    fc(catch bad_guards_3(#{a=>0,b=>0}, [])),
    fc(catch bad_guards_3(#{a=>0,b=>0}, [x])),
    fc(catch bad_guards_3(not_a_map, [x])),
    fc(catch bad_guards_3(42, [x])),

    fc(catch bad_guards_4()),

    ok.

%% beam_bool used to produce GC BIF instructions whose
%% Live operands included uninitialized registers.

bad_guards_1(X, [_]) when {{X}}, -X ->
    ok.

bad_guards_2(M, [_]) when M#{a := 0, b => 0}, map_size(M) ->
    ok.

%% beam_type used to produce an GC BIF instruction whose Live operand
%% included uninitialized registers.

bad_guards_3(M, [_]) when is_map(M) andalso M#{a := 0, b => 0}, length(M) ->
    ok.

%% v3_codegen would generate a jump to the failure label, but
%% without initializing x(0). The code at the failure label expected
%% x(0) to be initialized.

bad_guards_4() when not (error#{}); {not 0.0} -> freedom.

%% Building maps in a guard in a 'catch' would crash v3_codegen.

guard_in_catch(_Config) ->
    {'EXIT',{if_clause,_}} = do_guard_in_catch_map_1(#{}),
    {'EXIT',{if_clause,_}} = do_guard_in_catch_map_1(#{a=>b}),
    {'EXIT',{if_clause,_}} = do_guard_in_catch_map_1(atom),

    {'EXIT',{if_clause,_}} = do_guard_in_catch_map_2(#{}),
    {'EXIT',{if_clause,_}} = do_guard_in_catch_map_2(#{a=>b}),
    {'EXIT',{if_clause,_}} = do_guard_in_catch_map_2(atom),

    {'EXIT',{if_clause,_}} = (catch do_guard_in_catch_map_3()),

    {'EXIT',{if_clause,_}} = do_guard_in_catch_bin(42),
    {'EXIT',{if_clause,_}} = do_guard_in_catch_bin(<<1,2,3>>),
    {'EXIT',{if_clause,_}} = do_guard_in_catch_bin(atom),
    {'EXIT',{if_clause,_}} = do_guard_in_catch_bin(#{}),

    ok.

do_guard_in_catch_map_1(From) ->
    catch
	if
	    From#{[] => sufficient} ->
		saint
	end.

do_guard_in_catch_map_2(From) ->
    catch
	if
	    From#{From => sufficient} ->
		saint
	end.

do_guard_in_catch_map_3() ->
    try
	if [] -> solo end
    catch
	Friendly when Friendly#{0 => []} -> minutes
    after
	membership
    end.

do_guard_in_catch_bin(From) ->
    %% Would not crash v3_codegen, but there would be an unnecessary
    %% 'move' to a Y register.
    catch
	if
	    <<From:32>> ->
		saint
	end.

%%%
%%% The beam_bool pass has been eliminated. Here are the tests from
%%% beam_bool_SUITE.
%%%

beam_bool_SUITE(_Config) ->
    before_and_inside_if(),
    scotland(),
    y_registers(),
    protected(),
    maps(),
    ok.

before_and_inside_if() ->
    no = before_and_inside_if([a], [b], delete),
    no = before_and_inside_if([a], [b], x),
    no = before_and_inside_if([a], [], delete),
    no = before_and_inside_if([a], [], x),
    no = before_and_inside_if([], [], delete),
    yes = before_and_inside_if([], [], x),
    yes = before_and_inside_if([], [b], delete),
    yes = before_and_inside_if([], [b], x),

    {ch1,ch2} = before_and_inside_if_2([a], [b], blah),
    {ch1,ch2} = before_and_inside_if_2([a], [b], xx),
    {ch1,ch2} = before_and_inside_if_2([a], [], blah),
    {ch1,ch2} = before_and_inside_if_2([a], [], xx),
    {no,no} = before_and_inside_if_2([], [b], blah),
    {no,no} = before_and_inside_if_2([], [b], xx),
    {ch1,no} = before_and_inside_if_2([], [], blah),
    {no,ch2} = before_and_inside_if_2([], [], xx),
    ok.

%% Thanks to Simon Cornish and Kostis Sagonas.
%% Used to crash beam_bool.
before_and_inside_if(XDo1, XDo2, Do3) ->
    Do1 = (XDo1 =/= []),
    Do2 = (XDo2 =/= []),
    if
	%% This expression occurs in a try/catch (protected)
	%% block, which cannot refer to variables outside of
	%% the block that are boolean expressions.
	Do1 =:= true;
	Do1 =:= false, Do2 =:= false, Do3 =:= delete ->
	    no;
       true ->
	    yes
    end.

%% Thanks to Simon Cornish.
%% Used to generate code that would not set {y,0} on
%% all paths before its use (and therefore fail
%% validation by the beam_validator).
before_and_inside_if_2(XDo1, XDo2, Do3) ->
    Do1    = (XDo1 =/= []),
    Do2    = (XDo2 =/= []),
    CH1 = if Do1 == true;
	     Do1 == false,Do2==false,Do3 == blah ->
		  ch1;
	     true ->
		  no
	  end,
    CH2 = if Do1 == true;
	     Do1 == false,Do2==false,Do3 == xx ->
		  ch2;
	     true ->
		  no
	  end,
    {CH1,CH2}.


%% beam_bool would remove the initialization of {y,0}.
%% (Thanks to Thomas Arts and QuickCheck.)

scotland() ->
    million = do_scotland(placed),
    {'EXIT',{{badmatch,placed},_}} = (catch do_scotland(false)),
    {'EXIT',{{badmatch,placed},_}} = (catch do_scotland(true)),
    {'EXIT',{{badmatch,placed},_}} = (catch do_scotland(echo)),
    ok.

do_scotland(Echo) ->
  found(case Echo of
	    Echo when true; Echo, Echo, Echo ->
		Echo;
	    echo ->
		[]
	end,
	Echo = placed).

found(_, _) -> million.


%% ERL-143: beam_bool could not handle Y registers as a destination.
y_registers() ->
    {'EXIT',{badarith,[_|_]}} = (catch baker(valentine)),
    {'EXIT',{badarith,[_|_]}} = (catch baker(clementine)),

    {not_ok,true} = potter([]),
    {ok,false} = potter([{encoding,any}]),

    ok.

%% Thanks to Quickcheck.
baker(Baker) ->
    (valentine == Baker) +
	case Baker of
	    Baker when Baker; Baker ->
		Baker;
	    Baker ->
		[]
	end.

%% Thanks to Jose Valim.
potter(Modes) ->
    Raw = lists:keyfind(encoding, 1, Modes) == false,
    Final = case Raw of
		X when X == false; X == nil -> ok;
		_ -> not_ok
	    end,
    {Final,Raw}.

protected() ->
    {'EXIT',{if_clause,_}} = (catch photographs({1, surprise, true}, opinions)),

    {{true}} = welcome({perfect, true}),
    {'EXIT',{if_clause,_}} = (catch welcome({perfect, false})),
    ok.

photographs({_Violation, surprise, Deep}, opinions) ->
    {if
	 0; "here", Deep ->
	     Deep = Deep
     end}.

welcome({perfect, Profit}) ->
    if
	Profit, Profit, Profit; 0 ->
	    {id({Profit})}
    end.

maps() ->
    ok = evidence(#{0 => 42}).

%% Cover handling of put_map in in split_block_label_used/2.
evidence(#{0 := Charge}) when 0; #{[] => Charge} == #{[] => 42} ->
    ok.

cover_beam_dead(_Config) ->
    Mod = ?FUNCTION_NAME,
    Attr = [],
    Fs = [{function,test,1,2,
           [{label,1},
            {line,[]},
            {func_info,{atom,Mod},{atom,test},1},
            {label,2},
            %% Cover beam_dead:turn_op/1 using swapped operand order.
            {test,is_ne_exact,{f,3},[{integer,1},{x,0}]},
            {test,is_eq_exact,{f,1},[{atom,a},{x,0}]},
            {label,3},
            {move,{atom,ok},{x,0}},
            return]}],
    Exp = [{test,1}],
    Asm = {Mod,Exp,Attr,Fs,3},
    {ok,Mod,Beam} = compile:forms(Asm, [from_asm,binary,report]),
    {module,Mod} = code:load_binary(Mod, Mod, Beam),
    ok = Mod:test(1),
    ok = Mod:test(a),
    {'EXIT',_} = (catch Mod:test(other)),
    true = code:delete(Mod),
    _ = code:purge(Mod),

    ok.

%% Call this function to turn off constant propagation.
id(I) -> I.

check(F, Result) ->
    case F() of
	Result -> ok;
	Other ->
	    io:format("Expected: ~p\n", [Result]),
	    io:format("     Got: ~p\n", [Other]),
	    ct:fail(check_failed)
    end.

fc({'EXIT',{function_clause,_}}) -> ok;
fc({'EXIT',{{case_clause,_},_}}) when ?MODULE =:= guard_inline_SUITE -> ok.
