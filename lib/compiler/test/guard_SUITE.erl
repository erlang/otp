%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2001-2013. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%
-module(guard_SUITE).

-include_lib("test_server/include/test_server.hrl").

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 misc/1,const_cond/1,basic_not/1,complex_not/1,nested_nots/1,
	 semicolon/1,complex_semicolon/1,comma/1,
	 or_guard/1,more_or_guards/1,
	 complex_or_guards/1,and_guard/1,
	 xor_guard/1,more_xor_guards/1,
	 old_guard_tests/1,
	 build_in_guard/1,gbif/1,
	 t_is_boolean/1,is_function_2/1,
	 tricky/1,rel_ops/1,literal_type_tests/1,
	 basic_andalso_orelse/1,traverse_dcd/1,
	 check_qlc_hrl/1,andalso_semi/1,t_tuple_size/1,binary_part/1,
	 bad_constants/1,bad_guards/1]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    test_lib:recompile(?MODULE),
    [{group,p}].

groups() -> 
    [{p,test_lib:parallel(),
      [misc,const_cond,basic_not,complex_not,nested_nots,
       semicolon,complex_semicolon,comma,or_guard,
       more_or_guards,complex_or_guards,and_guard,xor_guard,
       more_xor_guards,build_in_guard,old_guard_tests,gbif,
       t_is_boolean,is_function_2,tricky,rel_ops,
       literal_type_tests,basic_andalso_orelse,traverse_dcd,
       check_qlc_hrl,andalso_semi,t_tuple_size,binary_part,
       bad_constants,bad_guards]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


misc(Config) when is_list(Config) ->
    ?line 42 = case id(42) of
		   X when -X -> ok;
		   X -> X
	       end,
    ?line {a,b,c} = misc_1([{{a,b,c}},{[4]},{[3]},{-2}]),
    ?line none = misc_1([{{a,b,c}},{[4]},{[3]},{-3}]),
    ?line none = misc_1([{{a,b,c}},{[4]},{[7]},{-2}]),
    ?line none = misc_1([{{a,b,c}},{[4]},{[3]},{[1,2,3]}]),

    ?line {ok,buf,<<>>} = get_data({o,true,raw}, 0, buf),
    ?line {ok,buf,<<>>} = get_data({o,true,raw}, 42, buf),
    ?line {ok,buf,<<>>} = get_data({o,false,raw}, 0, buf),
    ?line error = get_data({o,false,raw}, 42, buf),
    ?line {ok,buf,<<>>} = get_data({o,true,0}, 0, buf),
    ?line {ok,buf,<<>>} = get_data({o,true,0}, 42, buf),
    ?line {ok,buf,<<>>} = get_data({o,false,0}, 0, buf),
    ?line error = get_data({o,false,0}, 42, buf),
    ok.
    

misc_1([{W},{X},{Y},{Z}]) ->
	      if
	X > Y andalso abs(Z) =:= 2 ->
	    id(W);
	true ->
	    none
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
    ?line ok = const_cond({}, 0),
    ?line ok = const_cond({a}, 1),
    ?line error = const_cond({a,b}, 3),
    ?line error = const_cond({a}, 0),
    ?line error = const_cond({a,b}, 1),
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

    ?line check(fun() -> if not false -> ok; true -> error end end, ok),
    ?line check(fun() -> if not true -> ok; true -> error end end, error),
    ?line check(fun() -> if not False -> ok; true -> error end end, ok),
    ?line check(fun() -> if not True -> ok; true -> error end end, error),

    ?line check(fun() -> if A > B -> gt; A < B -> lt; A == B -> eq end end, lt),
    ?line check(fun() -> if A > C -> gt; A < C -> lt; A == C -> eq end end, gt),
    ?line check(fun() -> if A > D -> gt; A < D -> lt; A == D -> eq end end, eq),

    ?line check(fun() -> if not (7 > 453) -> le; not (7 < 453) -> ge;
			    not (7 == 453) -> ne; true -> eq end end, le),
    ?line check(fun() -> if not (7 > -8) -> le; not (7 < -8) -> ge;
			    not (7 == -8) -> ne; true -> eq end end, ge),
    ?line check(fun() -> if not (7 > 7) -> le; not (7 < 7) -> ge;
			    not (7 == 7) -> ne; true -> eq end end, le),

    ?line check(fun() -> if not (A > B) -> le; not (A < B) -> ge;
			    not (A == B) -> ne; true -> eq end end, le),
    ?line check(fun() -> if not (A > C) -> le; not (A < C) -> ge;
			    not (A == C) -> ne; true -> eq end end, ge),
    ?line check(fun() -> if not (A > D) -> le; not (A < D) -> ge;
			    not (A == D) -> ne; true -> eq end end, le),

    ?line check(fun() -> if not element(1, ATuple) -> ok; true -> error end end, ok),
    ?line check(fun() -> if not element(2, ATuple) -> ok; true -> error end end, error),
    ?line check(fun() -> if not element(3, ATuple) -> ok; true -> error end end, error),

    ?line check(fun() -> if not glurf -> ok; true -> error end end, error),
    ?line check(fun() -> if not Glurf -> ok; true -> error end end, error),

    ok.

complex_not(Config) when is_list(Config) ->
    ATuple = id({false,true,gurka}),
    ?line check(fun() -> if not(element(1, ATuple)) -> ok; true -> error end end, ok),
    ?line check(fun() -> if not(element(2, ATuple)) -> ok; true -> error end end, error),

    ?line check(fun() -> if not(element(3, ATuple) == gurka) -> ok;
			    true -> error end end, error),
    ?line check(fun() -> if not(element(3, ATuple) =/= gurka) -> ok;
			    true -> error end end, ok),

    ?line check(fun() -> if {a,not(element(2, ATuple))} == {a,false} -> ok;
			    true -> error end end, ok),
    ?line check(fun() -> if {a,not(element(1, ATuple))} == {a,false} -> ok;
			    true -> error end end, error),

    ?line check(fun() -> if not(element(1, ATuple) or element(3, ATuple)) -> ok;
			    true -> error end end, error),

    %% orelse
    ?line check(fun() -> if not(element(1, ATuple) orelse element(3, ATuple)) -> ok;
			    true -> error end end, error),

    ok.

nested_nots(Config) when is_list(Config) ->
    ?line true = nested_not_1(0, 0),
    ?line true = nested_not_1(0, 1),
    ?line true = nested_not_1(a, b),
    ?line true = nested_not_1(10, 0),
    ?line false = nested_not_1(z, a),
    ?line false = nested_not_1(3.4, {anything,goes}),
    ?line false = nested_not_1(3.4, atom),
    ?line true = nested_not_1(3.0, [list]),

    ?line true = nested_not_2(false, false, 42),
    ?line true = nested_not_2(false, true, 42),
    ?line true = nested_not_2(true, false, 42),
    ?line true = nested_not_2(true, true, 42),
    ?line true = nested_not_2(false, false, atom),
    ?line false = nested_not_2(false, true, atom),
    ?line false = nested_not_2(true, false, atom),
    ?line false = nested_not_2(true, true, atom),
    ok.

nested_not_1(X, Y) when not (((X>Y) or not(is_atom(X))) and
			     (is_atom(Y) or (X==3.4))) ->
    true;
nested_not_1(_, _) ->
    false.

nested_not_2(X, Y, Z) ->
    nested_not_2(X, Y, Z, true).

nested_not_2(X, Y, Z, True)
  when not(True and not((not(X) and not(Y)) or not(is_atom(Z)))) ->
    true;
nested_not_2(_, _, _, _) ->
    false.

semicolon(Config) when is_list(Config) ->

    %% True/false combined using ';' (literal atoms).

    ?line check(fun() -> if true; false -> ok end end, ok),
    ?line check(fun() -> if false; true -> ok end end, ok),
    ?line check(fun() -> if true; true -> ok end end, ok),
    ?line check(fun() -> if false; false -> ok; true -> error end end, error),
    ?line check(fun() ->
			{'EXIT',{if_clause,_}} = (catch if false; false -> ok end),
			exit
		end, exit),

    %% True/false combined used ';'.

    True = id(true),
    False = id(false),

    ?line check(fun() -> if True; False -> ok end end, ok),
    ?line check(fun() -> if False; True -> ok end end, ok),
    ?line check(fun() -> if True; True -> ok end end, ok),
    ?line check(fun() -> if False; False -> ok; true -> error end end, error),
    ?line check(fun() ->
			{'EXIT',{if_clause,_}} = (catch if False; False -> ok end),
			exit
		end, exit),

    %% Combine true/false with a non-boolean value.
    Glurf = id(glurf),


    ?line check(fun() -> if True; Glurf -> ok end end, ok),
    ?line check(fun() -> if Glurf; True -> ok end end, ok),
    ?line check(fun() -> if Glurf; Glurf -> ok; true -> error end end, error),
    ?line check(fun() -> if False; Glurf -> ok; true -> error end end, error),
    ?line check(fun() -> if Glurf; False -> ok; true -> error end end, error),
    ?line check(fun() ->
			{'EXIT',{if_clause,_}} = (catch if Glurf; Glurf -> ok end),
			exit
		end, exit),

    %% Combine true/false with errors.

    ATuple = id({false,true,gurka}),

    ?line check(fun() -> if True; element(42, ATuple) -> ok end end, ok),
    ?line check(fun() -> if element(42, ATuple); True -> ok end end, ok),
    ?line check(fun() -> if element(42, ATuple); element(42, ATuple) -> ok;
			    true -> error end end, error),
    ?line check(fun() -> if False; element(42, ATuple) -> ok;
			    true -> error end end, error),
    ?line check(fun() -> if element(42, ATuple);
			    False -> ok; true -> error end end, error),
    ?line check(fun() ->
			{'EXIT',{if_clause,_}} =
			    (catch if element(42, ATuple);
				      element(42, ATuple) -> ok end),
			exit
		end, exit),

    ok.

complex_semicolon(Config) when is_list(Config) ->
    ?line ok = csemi1(int, {blurf}),
    ?line ok = csemi1(string, {blurf}),
    ?line ok = csemi1(float, [a]),
    ?line error = csemi1(35, 42),

    %% 2
    ?line ok = csemi2({}, {a,b,c}),
    ?line ok = csemi2({1,3.5}, {a,b,c}),
    ?line ok = csemi2(dum, {a,b,c}),

    ?line ok = csemi2({45,-19.3}, {}),
    ?line ok = csemi2({45,-19.3}, {dum}),
    ?line ok = csemi2({45,-19.3}, {dum,dum}),

    ?line error = csemi2({45}, {dum}),
    ?line error = csemi2([], {dum}),
    ?line error = csemi2({dum}, []),
    ?line error = csemi2([], []),

    %% 3
    ?line csemi3(fun csemi3a/4),
    ?line csemi3(fun csemi3b/4),
    ?line csemi3(fun csemi3c/4),

    %% 4
    ?line csemi4(fun csemi4a/4),
    ?line csemi4(fun csemi4b/4),
    ?line csemi4(fun csemi4c/4),
    ?line csemi4(fun csemi4d/4),

    %% 4, 'orelse' instead of 'or'
    ?line csemi4_orelse(fun csemi4_orelse_a/4),
    ?line csemi4_orelse(fun csemi4_orelse_b/4),
    ?line csemi4_orelse(fun csemi4_orelse_c/4),
    ?line csemi4_orelse(fun csemi4_orelse_d/4),

    %% 5
    ?line error = csemi5(0, 0),
    ?line ok = csemi5(5, 0),
    ?line ok = csemi5(4, -4),
    ?line ok = csemi5(10, -4),

    %% 6
    ?line error = csemi6({a}, 0),
    ?line ok = csemi6({a,b}, 0),
    ?line ok = csemi6({}, 3),
    ?line ok = csemi6({a,b,c}, 3),
    
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

    ?line error = Test([], 1, {}, 0),

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
    
comma(Config) when is_list(Config) ->

    %% ',' combinations of literal true/false.

    ?line check(fun() -> if true, false -> ok; true -> error end end, error),
    ?line check(fun() -> if false, true -> ok; true -> error end end, error),
    ?line check(fun() -> if true, true -> ok end end, ok),
    ?line check(fun() -> if false, false -> ok; true -> error end end, error),
    ?line check(fun() ->
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

    ?line check(fun() -> if True, False -> ok; true -> error end end, error),
    ?line check(fun() -> if False, True -> ok; true -> error end end, error),
    ?line check(fun() -> if True, True -> ok end end, ok),
    ?line check(fun() -> if False, False -> ok; true -> error end end, error),
    ?line check(fun() ->
			{'EXIT',{if_clause,_}} =
			    (catch if True, False -> ok;
				      False, True -> ok;
				      False, False -> ok
				   end),
			exit
		end, exit),

    %% ',' combinations of true/false, and non-boolean in variables.

    Glurf = id(glurf),

    ?line check(fun() -> if True, Glurf -> ok; true -> error end end, error),
    ?line check(fun() -> if Glurf, True -> ok; true -> error end end, error),
    ?line check(fun() -> if True, True -> ok end end, ok),
    ?line check(fun() -> if Glurf, Glurf -> ok; true -> error end end, error),
    ?line check(fun() ->
			{'EXIT',{if_clause,_}} =
			    (catch if True, Glurf -> ok;
				      Glurf, True -> ok;
				      Glurf, Glurf -> ok
				   end),
			exit
		end, exit),

    %% ',' combinations of true/false with errors.
    ATuple = id({a,b,c}),

    ?line check(fun() -> if True, element(42, ATuple) -> ok;
			    true -> error end end, error),
    ?line check(fun() -> if element(42, ATuple), True -> ok;
			    true -> error end end, error),
    ?line check(fun() -> if True, True -> ok end end, ok),
    ?line check(fun() -> if element(42, ATuple), element(42, ATuple) -> ok;
			    true -> error end end, error),
    ?line check(fun() ->
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
    ?line check(fun() -> if true or false -> ok end end, ok),
    ?line check(fun() -> if false or true -> ok end end, ok),
    ?line check(fun() -> if true or true -> ok end end, ok),
    ?line check(fun() -> if false or false -> ok; true -> error end end, error),

    ?line check(fun() -> if glurf or true -> ok; true -> error end end, error),
    ?line check(fun() -> if true or glurf -> ok; true -> error end end, error),
    ?line check(fun() -> if glurf or glurf -> ok; true -> error end end, error),

    ?line check(fun() ->
			{'EXIT',{if_clause,_}} = (catch if false or false -> ok end),
			exit
		end, exit),


    %% 'or' combinations using variables containing true/false.
    ?line check(fun() -> if True or False -> ok end end, ok),
    ?line check(fun() -> if False or True -> ok end end, ok),
    ?line check(fun() -> if True or True -> ok end end, ok),
    ?line check(fun() -> if False or False -> ok; true -> error end end, error),

    ?line check(fun() -> if True or Glurf -> ok; true -> error end end, error),
    ?line check(fun() -> if Glurf or True -> ok; true -> error end end, error),
    ?line check(fun() -> if Glurf or Glurf -> ok; true -> error end end, error),

    ?line check(fun() ->
			{'EXIT',{if_clause,_}} = (catch if False or False -> ok end),
			exit
		end, exit),

    ok.

more_or_guards(Config) when is_list(Config) ->
    True = id(true),
    False = id(false),
    ATuple = id({false,true,gurka}),

    ?line check(fun() ->
			if element(42, ATuple) or False -> ok;
			   true -> error end
		end, error),

    ?line check(fun() ->
			if False or element(42, ATuple) -> ok;
			   true -> error end
		end, error),

    ?line check(fun() ->
			if element(18, ATuple) or element(42, ATuple) -> ok;
			   true -> error end
		end, error),

    ?line check(fun() ->
			if True or element(42, ATuple) -> ok;
			   true -> error end
		end, error),

    ?line check(fun() ->
			if element(42, ATuple) or True -> ok;
			   true -> error end
		end, error),

    ?line check(fun() ->
			if element(1, ATuple) or element(42, ATuple) or True -> ok;
			   true -> error end
		end, error),

    ?line check(fun() ->
			if element(1, ATuple) or True or element(42, ATuple) -> ok;
			   true -> error end
		end, error),

    ?line check(fun() ->
			if
			    (<<False:8>> == <<0>>) or element(2, ATuple) -> ok;
			    true -> error end
		end, error),

    ?line check(fun() ->
			if
			    element(2, ATuple) or (<<True:8>> == <<1>>) -> ok;
			    true -> error end
		end, error),

    ?line check(fun() ->
			if element(2, ATuple) or element(42, ATuple) -> ok;
			   true -> error end
		end, error),

    ?line check(fun() ->
			if
			    element(1, ATuple) or
			    element(2, ATuple) or
			    element(19, ATuple) -> ok;
			    true -> error end
		end, error),
    ok.

complex_or_guards(Config) when is_list(Config) ->
    %% complex_or_1/2
    ?line ok = complex_or_1({a,b,c,d}, {1,2,3}),
    ?line ok = complex_or_1({a,b,c,d}, {1}),
    ?line ok = complex_or_1({a}, {1,2,3}),
    ?line error = complex_or_1({a}, {1}),

    ?line error = complex_or_1(1, 2),
    ?line error = complex_or_1([], {a,b,c,d}),
    ?line error = complex_or_1({a,b,c,d}, []),


    %% complex_or_2/1
    ?line ok = complex_or_2({true,{}}),
    ?line ok = complex_or_2({false,{a}}),
    ?line ok = complex_or_2({false,{a,b,c}}),
    ?line ok = complex_or_2({true,{a,b,c,d}}),

    ?line error = complex_or_2({blurf,{a,b,c}}),
    
    ?line error = complex_or_2({true}),
    ?line error = complex_or_2({true,no_tuple}),
    ?line error = complex_or_2({true,[]}),

    %% complex_or_3/2
    ?line ok = complex_or_3({true}, {}),
    ?line ok = complex_or_3({false}, {a}),
    ?line ok = complex_or_3({false}, {a,b,c}),
    ?line ok = complex_or_3({true}, {a,b,c,d}),
    ?line ok = complex_or_3({false}, <<1,2,3>>),
    ?line ok = complex_or_3({true}, <<1,2,3,4>>),

    ?line error = complex_or_3(blurf, {a,b,c}),

    ?line error = complex_or_3({false}, <<1,2,3,4>>),
    ?line error = complex_or_3([], <<1,2>>),
    ?line error = complex_or_3({true}, 45),
    ?line error = complex_or_3(<<>>, <<>>),

    %% complex_or_4/2
    ?line ok = complex_or_4(<<1,2,3>>, {true}),
    ?line ok = complex_or_4(<<1,2,3>>, {false}),
    ?line ok = complex_or_4(<<1,2,3>>, {true}),
    ?line ok = complex_or_4({1,2,3}, {true}),
    ?line error = complex_or_4({1,2,3,4}, {false}),

    ?line error = complex_or_4(<<1,2,3,4>>, []),
    ?line error = complex_or_4([], {true}),

    %% complex_or_5/2
    ?line ok = complex_or_5(<<1>>, {false}),
    ?line ok = complex_or_5(<<1,2,3>>, {true}),
    ?line ok = complex_or_5(<<1,2,3,4>>, {false}),
    ?line ok = complex_or_5({1,2,3}, {false}),
    ?line ok = complex_or_5({1,2,3,4}, {false}),

    ?line error = complex_or_5(blurf, {false}),
    ?line error = complex_or_5(<<1>>, klarf),
    ?line error = complex_or_5(blurf, klarf),

    %% complex_or_6/2
    ?line ok = complex_or_6({true,true}, {1,2,3,4}),
    ?line ok = complex_or_6({true,true}, <<1,2,3,4>>),
    ?line ok = complex_or_6({false,false}, <<1,2,3,4>>),
    ?line ok = complex_or_6({false,true}, <<1>>),
    ?line ok = complex_or_6({true,false}, {1}),
    ?line ok = complex_or_6({true,true}, {1}),

    ?line error = complex_or_6({false,false}, {1}),

    ?line error = complex_or_6({true}, {1,2,3,4}),
    ?line error = complex_or_6({}, {1,2,3,4}),
    ?line error = complex_or_6([], {1,2,3,4}),
    ?line error = complex_or_6([], {1,2,3,4}),
    ?line error = complex_or_6({true,false}, klurf),

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

    ?line check(fun() -> if true and false -> ok; true -> error end end, error),
    ?line check(fun() -> if false and true -> ok; true -> error end end, error),
    ?line check(fun() -> if true and true -> ok end end, ok),
    ?line check(fun() -> if false and false -> ok; true -> error end end, error),

    ?line check(fun() -> if glurf and true -> ok; true -> error end end, error),
    ?line check(fun() -> if true and glurf -> ok; true -> error end end, error),
    ?line check(fun() -> if glurf and glurf -> ok; true -> error end end, error),

    ?line check(fun() ->
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

    ?line check(fun() -> if True and False -> ok; true -> error end end, error),
    ?line check(fun() -> if False and True -> ok; true -> error end end, error),
    ?line check(fun() -> if True and True -> ok end end, ok),
    ?line check(fun() -> if False and False -> ok; true -> error end end, error),
    ?line check(fun() ->
			{'EXIT',{if_clause,_}} =
			    (catch if True and False -> ok;
				      False and True -> ok;
				      False and False -> ok
				   end),
			exit
		end, exit),

    %% 'and' combinations of true/false and a non-boolean in variables.

    Glurf = id(glurf),

    ?line check(fun() -> if True and Glurf -> ok; true -> error end end, error),
    ?line check(fun() -> if Glurf and True -> ok; true -> error end end, error),
    ?line check(fun() -> if True and True -> ok end end, ok),
    ?line check(fun() -> if Glurf and Glurf -> ok; true -> error end end, error),
    ?line check(fun() ->
			{'EXIT',{if_clause,_}} =
			    (catch if True and Glurf -> ok;
				      Glurf and True -> ok;
				      Glurf and Glurf -> ok
				   end),
			exit
		end, exit),

    %% 'and' combinations of true/false with errors.
    ATuple = id({a,b,c}),

    ?line check(fun() -> if True and element(42, ATuple) -> ok;
			    true -> error end end, error),
    ?line check(fun() -> if element(42, ATuple) and True -> ok;
			    true -> error end end, error),
    ?line check(fun() -> if True and True -> ok end end, ok),
    ?line check(fun() -> if element(42, ATuple) and element(42, ATuple) -> ok;
			    true -> error end end, error),
    ?line check(fun() ->
			{'EXIT',{if_clause,_}} =
			    (catch if True and element(42, ATuple) -> ok;
				      element(42, ATuple) and True -> ok;
				      element(42, ATuple) and element(42, ATuple) -> ok
				   end),
			exit
		end, exit),

    ?line ok = relprod({'Set',a,b}, {'Set',a,b}),
    
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
    ?line check(fun() -> if true xor false -> ok end end, ok),
    ?line check(fun() -> if false xor true -> ok end end, ok),
    ?line check(fun() -> if true xor true -> ok; true -> error end end, error),
    ?line check(fun() -> if false xor false -> ok; true -> error end end, error),
    ?line check(fun() ->
			{'EXIT',{if_clause,_}} = (catch if false xor false -> ok end),
			exit
		end, exit),
    ?line check(fun() ->
			{'EXIT',{if_clause,_}} = (catch if true xor true -> ok end),
			exit
		end, exit),


    %% 'xor' combinations using variables containing true/false.

    True = id(true),
    False = id(false),

    ?line check(fun() -> if True xor False -> ok end end, ok),
    ?line check(fun() -> if False xor True -> ok end end, ok),
    ?line check(fun() -> if True xor True -> ok; true -> error end end, error),
    ?line check(fun() -> if False xor False -> ok; true -> error end end, error),
    ?line check(fun() ->
			{'EXIT',{if_clause,_}} = (catch if False xor False -> ok end),
			exit
		end, exit),
    ?line check(fun() ->
			{'EXIT',{if_clause,_}} = (catch if True xor True -> ok end),
			exit
		end, exit),

    ok.

more_xor_guards(Config) when is_list(Config) ->
    True = id(true),
    False = id(false),
    ATuple = id({false,true,gurka}),

    ?line check(fun() ->
			if element(42, ATuple) xor False -> ok;
			   true -> error end
		end, error),

    ?line check(fun() ->
			if False xor element(42, ATuple) xor False -> ok;
			   true -> error end
		end, error),

    ?line check(fun() ->
			if element(18, ATuple) xor element(42, ATuple) -> ok;
			   true -> error end
		end, error),

    ?line check(fun() ->
			if True xor element(42, ATuple) -> ok;
			   true -> error end
		end, error),

    ?line check(fun() ->
			if element(42, ATuple) xor True -> ok;
			   true -> error end
		end, error),
    ok.

build_in_guard(Config) when is_list(Config) ->
    SubBin = <<5.0/float>>,
    ?line B = <<1,SubBin/binary,3.5/float>>,
    ?line if
	      B =:= <<1,SubBin/binary,3.5/float>> -> ok
	  end.

old_guard_tests(Config) when list(Config) ->
    %% Check that all the old guard tests are still recognized.
    ?line list = og(Config),
    ?line atom = og(an_atom),
    ?line binary = og(<<1,2>>),
    ?line float = og(3.14),
    ?line integer = og(43),
    ?line a_function = og(fun() -> ok end),
    ?line pid = og(self()),
    ?line reference = og(make_ref()),
    ?line tuple = og({}),

    ?line number = on(45.333),
    ?line number = on(-19),
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

gbif(Config) when is_list(Config) ->
    ?line error = gbif_1(1, {false,true}),
    ?line ok = gbif_1(2, {false,true}),
    ok.

gbif_1(P, T) when element(P, T) -> ok;
gbif_1(_, _) -> error.


t_is_boolean(Config) when is_list(Config) ->
    ?line true = is_boolean(true),
    ?line true = is_boolean(false),
    ?line true = is_boolean(id(true)),
    ?line true = is_boolean(id(false)),

    ?line false = is_boolean(glurf),
    ?line false = is_boolean(id(glurf)),

    ?line false = is_boolean([]),
    ?line false = is_boolean(id([])),
    ?line false = is_boolean(42),
    ?line false = is_boolean(id(-42)),

    ?line false = is_boolean(math:pi()),
    ?line false = is_boolean(384793478934378924978439789873478934897),

    ?line false = is_boolean(id(self())),
    ?line false = is_boolean(id({x,y,z})),
    ?line false = is_boolean(id([a,b,c])),
    ?line false = is_boolean(id(make_ref())),
    ?line false = is_boolean(id(<<1,2,3>>)),
    ?line false = is_boolean({id(x),y,z}),
    ?line false = is_boolean([id(a),b,c]),

    ?line ok = bool(true),
    ?line ok = bool(false),
    ?line ok = bool(id(true)),
    ?line ok = bool(id(false)),

    ?line error = bool(glurf),
    ?line error = bool(id(glurf)),

    ?line error = bool([]),
    ?line error = bool(id([])),
    ?line error = bool(42),
    ?line error = bool(id(-42)),

    ?line error = bool(math:pi()),
    ?line error = bool(384793478934378924978439789873478934897),

    ?line error = bool(id(self())),
    ?line error = bool(id({x,y,z})),
    ?line error = bool(id([a,b,c])),
    ?line error = bool(id(make_ref())),
    ?line error = bool(id(<<1,2,3>>)),

    ?line true = my_is_bool(true),
    ?line true = my_is_bool(false),
    ?line false = my_is_bool([]),
    ?line false = my_is_bool([1,2,3,4]),
    ?line false = my_is_bool({a,b,c}),

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
    ?line not_ok = tricky_1(1, 2),
    ?line not_ok = tricky_1(1, blurf),
    ?line not_ok = tricky_1(foo, 2),
    ?line not_ok = tricky_1(a, b),

    ?line error = tricky_2(0.5),
    ?line error = tricky_2(a),
    ?line error = tricky_2({a,b,c}),

    ?line false = rb(100000, [1], 42),
    ?line true = rb(100000, [], 42),
    ?line true = rb(555, [a,b,c], 19),
    ok.

tricky_1(X, Y) when abs((X == 1) or (Y == 2)) -> ok;
tricky_1(_, _) -> not_ok.

tricky_2(X) when float(X) or float(X) -> ok;
tricky_2(_) -> error.

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
    ?line ?T(=/=, 1, 1.0),
    ?line ?F(=/=, 2, 2),
    ?line ?F(=/=, {a}, {a}),

    ?line ?F(/=, a, a),
    ?line ?F(/=, 0, 0.0),
    ?line ?T(/=, 0, 1),
    ?line ?F(/=, {a}, {a}),

    ?line ?T(==, 1, 1.0),
    ?line ?F(==, a, {}),

    ?line ?F(=:=, 1, 1.0),
    ?line ?T(=:=, 42.0, 42.0),

    ?line ?F(>, a, b),
    ?line ?T(>, 42, 1.0),
    ?line ?F(>, 42, 42.0),

    ?line ?T(<, a, b),
    ?line ?F(<, 42, 1.0),
    ?line ?F(<, 42, 42.0),

    ?line ?T(=<, 1.5, 5),
    ?line ?F(=<, -9, -100.344),
    ?line ?T(=<, 42, 42.0),

    ?line ?T(>=, 42, 42.0),
    ?line ?F(>=, a, b),
    ?line ?T(>=, 1.0, 0),

    %% Coverage of beam_block:is_exact_eq_ok/1 and collect/1.
    ?line true = any_atom /= id(42),
    ?line true = [] /= id(42),

    ok.

-undef(TestOp).


%% Test type tests on literal values. (From emulator test suites.)
literal_type_tests(Config) when is_list(Config) ->
    case ?MODULE of
	guard_SUITE -> literal_type_tests_1(Config);
	_ -> {skip,"Enough to run this case once."}
    end.

literal_type_tests_1(Config) ->
    %% Generate an Erlang module with all different type of type tests.
    ?line Tests = make_test([{T,L} || T <- type_tests(), L <- literals()] ++
			    [{is_function,L1,L2} || 
				L1 <- literals(), L2 <- literals()]),
    ?line Mod = literal_test,
    ?line Func = {function, 0, test, 0, [{clause,0,[],[],Tests}]},
    ?line Form = [{attribute,0,module,Mod},
		  {attribute,0,compile,export_all},
		  Func, {eof,0}],

    %% Print generated code for inspection.
    ?line lists:foreach(fun (F) -> io:put_chars([erl_pp:form(F),"\n"]) end, Form),

    %% Test compile:form/1.  This implies full optimization (default).
    ?line {ok,Mod,Code1} = compile:forms(Form),
    ?line smoke_disasm(Config, Mod, Code1),
    ?line {module,Mod} = code:load_binary(Mod, Mod, Code1),
    ?line Mod:test(),
    ?line true = code:delete(Mod),
    ?line code:purge(Mod),
			       
    %% Test compile:form/2.  Turn off all optimizations.
    ?line {ok,Mod,Code2} = compile:forms(Form, [binary,report,time,
						no_copt,no_postopt]),
    ?line smoke_disasm(Config, Mod, Code2),
    ?line {module,Mod} = code:load_binary(Mod, Mod, Code2),
    ?line Mod:test(),
    ?line true = code:delete(Mod),
    ?line code:purge(Mod),
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
    {match,0,{atom,0,Val},hd(E)}.

test(T, L1, L2) ->
    S0 = io_lib:format("begin io:format(\"~~p~n\", [{~p,~p,~p}]), if ~w(~w, ~w) -> true; true -> false end end. ", [T,L1,L2,T,L1,L2]),
    S = lists:flatten(S0),
    {ok,Toks,_Line} = erl_scan:string(S),
    {ok,E} = erl_parse:parse_exprs(Toks),
    {value,Val,_Bs} = erl_eval:exprs(E, []),
    {match,0,{atom,0,Val},hd(E)}.

smoke_disasm(Config, Mod, Bin) ->
    Priv = ?config(priv_dir, Config),
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
     is_function].

basic_andalso_orelse(Config) when is_list(Config) ->
    ?line T = id({type,integers,23,42}),
    ?line 65 = if
		   ((element(1, T) =:= type) andalso (tuple_size(T) =:= 4) andalso
		    element(2, T)) == integers ->
		       element(3, T) + element(4, T);
		   true -> error
	       end,
    ?line 65 = case [] of
		   [] when ((element(1, T) =:= type) andalso (tuple_size(T) =:= 4) andalso
			    element(2, T)) == integers ->
		       element(3, T) + element(4, T)
	       end,

    ?line 42 = basic_rt({type,integers,40,2}),
    ?line 5.0 = basic_rt({vector,{3.0,4.0}}),
    ?line 20 = basic_rt(['+',3,7]),
    ?line {'Set',a,b} = basic_rt({{'Set',a,b},{'Set',a,b}}),
    ?line 12 = basic_rt({klurf,4}),

    ?line error = basic_rt({type,integers,40,2,3}),
    ?line error = basic_rt({kalle,integers,40,2}),
    ?line error = basic_rt({kalle,integers,40,2}),
    ?line error = basic_rt({1,2}),
    ?line error = basic_rt([]),

    RelProdBody =
	fun(R1, R2) ->
		if
		    (erlang:size(R1) =:= 3) andalso (erlang:element(1,R1) =:= 'Set'),
		    (erlang:size(R2) =:= 3) andalso (erlang:element(1,R2) =:= 'Set') ->
			ok
		end
	end,

    ?line ok = RelProdBody({'Set',a,b}, {'Set',a,b}),

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
    ?line foo = cqlc(qlc, q, [{lc,1,2,3}], St),
    ?line foo = cqlc(qlc, q, [{lc,1,2,3},b], St),
    ?line St = cqlc(qlc, q, [], St),
    ?line St = cqlc(qlc, blurf, [{lc,1,2,3},b], St),
    ?line St = cqlc(q, q, [{lc,1,2,3},b], St),
    ?line St = cqlc(qlc, q, [{lc,1,2,3},b,c], St),
    ?line St = cqlc(qlc, q, [a,b], St),
    ?line {r1,true,kalle} = cqlc(qlc, q, [{lc,1,2,3},b], {r1,true,kalle}),
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
    ?line ok = andalso_semi_foo(0),
    ?line ok = andalso_semi_foo(1),
    ?line fc(catch andalso_semi_foo(2)),

    ?line ok = andalso_semi_bar([a,b,c]),
    ?line ok = andalso_semi_bar(1),
    ?line fc(catch andalso_semi_bar([a,b])),
    ok.

andalso_semi_foo(Bar) when is_integer(Bar) andalso Bar =:= 0; Bar =:= 1 ->
   ok.

andalso_semi_bar(Bar) when is_list(Bar) andalso length(Bar) =:= 3; Bar =:= 1 ->
   ok.


t_tuple_size(Config) when is_list(Config) ->
    ?line 10 = do_tuple_size({1,2,3,4}),
    ?line fc(catch do_tuple_size({1,2,3})),
    ?line fc(catch do_tuple_size(42)),

    ?line error = ludicrous_tuple_size({a,b,c}),
    ?line error = ludicrous_tuple_size([a,b,c]),

    %% Test the "unsafe case" - the register assigned the tuple size is
    %% not killed.
    ?line DataDir = test_lib:get_data_dir(Config),
    ?line File = filename:join(DataDir, "guard_SUITE_tuple_size"),
    ?line {ok,Mod,Code} = compile:file(File, [from_asm,binary]),
    ?line code:load_binary(Mod, File, Code),
    ?line 14 = Mod:t({1,2,3,4}),
    
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

binary_part(doc) ->
    ["Tests the binary_part/2,3 guard (GC) bif's"];
binary_part(Config) when is_list(Config) ->
    %% This is more or less a copy of what the guard_SUITE in emulator
    %% does to cover the guard bif's
    ?line 1 = bptest(<<1,2,3>>),
    ?line 2 = bptest(<<2,1,3>>),
    ?line error = bptest(<<1>>),
    ?line error = bptest(<<>>),
    ?line error = bptest(apa),
    ?line 3 = bptest(<<2,3,3>>),
    % With one variable (pos)
    ?line 1 = bptest(<<1,2,3>>,1),
    ?line 2 = bptest(<<2,1,3>>,1),
    ?line error = bptest(<<1>>,1),
    ?line error = bptest(<<>>,1),
    ?line error = bptest(apa,1),
    ?line 3 = bptest(<<2,3,3>>,1),
    % With one variable (length)
    ?line 1 = bptesty(<<1,2,3>>,1),
    ?line 2 = bptesty(<<2,1,3>>,1),
    ?line error = bptesty(<<1>>,1),
    ?line error = bptesty(<<>>,1),
    ?line error = bptesty(apa,1),
    ?line 3 = bptesty(<<2,3,3>>,2),
    % With one variable (whole tuple)
    ?line 1 = bptestx(<<1,2,3>>,{1,1}),
    ?line 2 = bptestx(<<2,1,3>>,{1,1}),
    ?line error = bptestx(<<1>>,{1,1}),
    ?line error = bptestx(<<>>,{1,1}),
    ?line error = bptestx(apa,{1,1}),
    ?line 3 = bptestx(<<2,3,3>>,{1,2}),
    % With two variables
    ?line 1 = bptest(<<1,2,3>>,1,1),
    ?line 2 = bptest(<<2,1,3>>,1,1),
    ?line error = bptest(<<1>>,1,1),
    ?line error = bptest(<<>>,1,1),
    ?line error = bptest(apa,1,1),
    ?line 3 = bptest(<<2,3,3>>,1,2),
    % Direct (autoimported) call, these will be evaluated by the compiler...
    ?line <<2>> = binary_part(<<1,2,3>>,1,1),
    ?line <<1>> = binary_part(<<2,1,3>>,1,1),
    % Compiler warnings due to constant evaluation expected (3)
    ?line badarg = ?MASK_ERROR(binary_part(<<1>>,1,1)),
    ?line badarg = ?MASK_ERROR(binary_part(<<>>,1,1)),
    ?line badarg = ?MASK_ERROR(binary_part(apa,1,1)),
    ?line <<3,3>> = binary_part(<<2,3,3>>,1,2),
    % Direct call through apply
    ?line <<2>> = apply(erlang,binary_part,[<<1,2,3>>,1,1]),
    ?line <<1>> = apply(erlang,binary_part,[<<2,1,3>>,1,1]),
    % Compiler warnings due to constant evaluation expected (3)
    ?line badarg = ?MASK_ERROR(apply(erlang,binary_part,[<<1>>,1,1])),
    ?line badarg = ?MASK_ERROR(apply(erlang,binary_part,[<<>>,1,1])),
    ?line badarg = ?MASK_ERROR(apply(erlang,binary_part,[apa,1,1])),
    ?line <<3,3>> = apply(erlang,binary_part,[<<2,3,3>>,1,2]),
    % Constant propagation
    ?line  Bin = <<1,2,3>>,
    ?line  ok = if
		    binary_part(Bin,1,1) =:= <<2>> ->
			ok;
		    %% Compiler warning, clause cannot match (expected)
		    true ->
			error
		end,
    ?line  ok = if
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
	    C -> ?t:fail(should_fail);
	    true -> ok
	end,
	if
	    true, C -> ?t:fail(should_fail);
	    true -> ok
	end).

bad_constants(Config) when is_list(Config) ->
    ?line ?FAILING(false),
    ?line ?FAILING([]),
    ?line ?FAILING([a]),
    ?line ?FAILING([Config]),
    ?line ?FAILING({a,b}),
    ?line ?FAILING({a,Config}),
    ?line ?FAILING(<<1>>),
    ?line ?FAILING(42),
    ?line ?FAILING(3.14),
    ok.

bad_guards(Config) when is_list(Config) ->
    if erlang:float(self()); true -> ok end,
    ok.

%% Call this function to turn off constant propagation.
id(I) -> I.

check(F, Result) ->
    case F() of
	Result -> ok;
	Other ->
	    io:format("Expected: ~p\n", [Result]),
	    io:format("     Got: ~p\n", [Other]),
	    test_server:fail()
    end.

fc({'EXIT',{function_clause,_}}) -> ok;
fc({'EXIT',{{case_clause,_},_}}) when ?MODULE =:= guard_inline_SUITE -> ok.
