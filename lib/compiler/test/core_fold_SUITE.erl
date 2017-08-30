%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2016. All Rights Reserved.
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
-module(core_fold_SUITE).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 t_element/1,setelement/1,t_length/1,append/1,t_apply/1,bifs/1,
	 eq/1,nested_call_in_case/1,guard_try_catch/1,coverage/1,
	 unused_multiple_values_error/1,unused_multiple_values/1,
	 multiple_aliases/1,redundant_boolean_clauses/1,
	 mixed_matching_clauses/1,unnecessary_building/1,
	 no_no_file/1]).

-export([foo/0,foo/1,foo/2,foo/3]).

-include_lib("common_test/include/ct.hrl").

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    test_lib:recompile(?MODULE),
    [{group,p}].

groups() -> 
    [{p,[parallel],
      [t_element,setelement,t_length,append,t_apply,bifs,
       eq,nested_call_in_case,guard_try_catch,coverage,
       unused_multiple_values_error,unused_multiple_values,
       multiple_aliases,redundant_boolean_clauses,
       mixed_matching_clauses,unnecessary_building,
       no_no_file]}].


init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


t_element(Config) when is_list(Config) ->
    X = make_ref(),
    X = id(element(1, {X,y,z})),
    b = id(element(2, {a,b,c,d})),
    (fun() ->
	    case {a,#{k=>X}} of
		{a,#{k:=X}}=Tuple ->
		    #{k:=X} = id(element(2, Tuple))
	    end
    end)(),

    %% No optimization, but should work.
    Tuple = id({x,y,z}),
    Pos = id(3),
    x = id(element(1, Tuple)),
    c = id(element(Pos, {a,b,c,d})),
    X = id(element(Pos, {a,b,X,d})),
    z = id(element(Pos, Tuple)),

    %% Calls that will fail.
    {'EXIT',{badarg,_}} = (catch element(5, {a,b,c,d})),
    {'EXIT',{badarg,_}} = (catch element(5, {a,b,X,d})),
    {'EXIT',{badarg,_}} = (catch element(5.0, {a,b,X,d})),
    {'EXIT',{badarg,_}} = (catch element(2, not_a_tuple)),
    {'EXIT',{badarg,_}} = (catch element(2, [])),
    {'EXIT',{badarg,_}} = (catch element(2, Tuple == 3)),
    case id({a,b,c}) of
	{_,_,_}=Tup ->
	    {'EXIT',{badarg,_}} = (catch element(4, Tup))
    end,
    {'EXIT',{badarg,_}} = (catch element(1, tuple_size(Tuple))),

    ok.

setelement(Config) when is_list(Config) ->
    X = id(b),
    New = id([1,2,3]),
    {y,b,c} = id(setelement(1, {a,b,c}, y)),
    {y,b,c} = id(setelement(1, {a,X,c}, y)),
    {a,y,c} = id(setelement(2, {a,X,c}, y)),
    {a,[1,2,3],c} = id(setelement(2, {a,b,c}, New)),
    {a,[1,2,3],c} = id(setelement(2, {a,X,c}, New)),
    {a,b,[1,2,3]} = id(setelement(3, {a,b,c}, New)),
    {a,b,[1,2,3]} = id(setelement(3, {a,X,c}, New)),

    {'EXIT',{badarg,_}} = (catch setelement_crash({a,b,c,d,e,f})),
    error = setelement_crash_2({a,b,c,d,e,f}, <<42>>),

    {'EXIT',{badarg,_}} = (catch setelement(1, not_a_tuple, New)),
    {'EXIT',{badarg,_}} = (catch setelement(3, {a,b}, New)),

    ok.

setelement_crash(Tuple) ->
    %% Used to crash the compiler because sys_core_dsetel did not notice that
    %% X1 was used in bit syntax construction.
    X1 = setelement(5, Tuple, new),
    X2 = setelement(3, X1, new),
    {X2,<<X1>>}.

setelement_crash_2(Tuple, Bin) ->
    %% Used to crash the compiler because sys_core_dsetel did not notice that
    %% X1 was used as a size field in bit syntax matching.
    X1 = setelement(5, Tuple, new),
    X2 = setelement(3, X1, new),
    case Bin of
	<<42:X1>> -> X2;
	_ -> error
    end.

t_length(Config) when is_list(Config) ->
    Blurf = id({blurf,a,b}),
    Tail = id([42,43,44,45]),
    0 = id(length([])),
    1 = id(length([x])),
    2 = id(length([x,Blurf])),
    4 = id(length([x,Blurf,a,b])),

    %% No or partial optimization.
    4 = length(Tail),
    5 = id(length([x|Tail])),

    %% Will fail.
    {'EXIT',{badarg,_}} = (catch id(length([a,b|c]))),
    {'EXIT',{badarg,_}} = (catch id(length([a,Blurf|c]))),
    {'EXIT',{badarg,_}} = (catch id(length(atom))),

    ok.

-define(APPEND(A, B), (fun(Res) ->
			       Res = lists:append(A, B),
			       Res = erlang:append(A, B),
			       Res = erlang:'++'(A, B)
		       end)(A++B)).

append(Config) when is_list(Config) ->
    A = id(0),
    [a,b,c,d,e,f,g,h,i,j,k] = id(?APPEND([a,b,c,d,e,f],[g,h,i,j,k])),
    [a,b,c,d,e] = id(?APPEND([a,b,c],id([d,e]))),
    [0,1,2,3,4,5,6] = id(?APPEND([A,1,2,3],[4,5,6])),
    {'EXIT',{badarg,_}} = (catch id(?APPEND([A|blurf],[4,5,6]))),
    ok.

t_apply(Config) when is_list(Config) ->
    ok = apply(?MODULE, foo, []),
    4 = apply(?MODULE, foo, [3]),
    7 = apply(?MODULE, foo, [3,4]),
    12 = apply(?MODULE, foo, [id(8),4]),
    21 = apply(?MODULE, foo, [8,id(9),4]),
    20 = apply(?MODULE, foo, [8,8,id(4)]),
    24 = apply(?MODULE, foo, [id(10),10,4]),

    M = id(?MODULE),
    ok = apply(M, foo, []),
    4 = apply(M, foo, [3]),
    16.0 = apply(M, foo, [12.0,4]),

    %% Will fail.
    {'EXIT',{badarg,_}} = (catch apply([a,b,c], foo, [])),
    {'EXIT',{badarg,_}} = (catch apply(42, foo, [])),
    {'EXIT',{badarg,_}} = (catch apply(?MODULE, 45, [xx])),
    {'EXIT',{badarg,_}} = (catch apply(?MODULE, foo, {a,b})),
    {'EXIT',{badarg,_}} = (catch apply(M, M, [1009|10010])),
    {'EXIT',{badarg,_}} = (catch apply(?MODULE, foo, [10000|9999])),
    {'EXIT',{badarg,_}} = (catch apply(?MODULE, foo, a)),

    ok.

foo() ->
    ok.

foo(A) ->
    A+1.

foo(A, B) ->
    A + B.

foo(A, B, C) ->
    A + B + C.

bifs(Config) when is_list(Config) ->
    <<1,2,3,4>> = id(list_to_binary([1,2,3,4])),
    K = {a,key},
    V = {a,value},
    {ok,#{K:=V}} = id(list_to_tuple([ok,#{K=>V}])),
    ok.

-define(CMP_SAME(A0, B), (fun(A) -> true = A == B, false = A /= B end)(id(A0))).
-define(CMP_DIFF(A0, B), (fun(A) -> false = A == B, true = A /= B end)(id(A0))).
	       
eq(Config) when is_list(Config) ->
    ?CMP_SAME([a,b,c], [a,b,c]),
    ?CMP_SAME([42.0], [42.0]),
    ?CMP_SAME([42], [42]),
    ?CMP_SAME([42.0], [42]),

    ?CMP_DIFF(a, [a]),
    ?CMP_DIFF(a, {1,2,3}),

    ?CMP_SAME(#{a=>1.0,b=>2}, #{b=>2.0,a=>1}),
    ?CMP_SAME(#{a=>[1.0],b=>[2]}, #{b=>[2.0],a=>[1]}),

    %% The rule for comparing keys are different in 17.x and 18.x.
    %% Just test that the results are consistent.
    Bool = id(#{1=>a}) == id(#{1.0=>a}),	%Unoptimizable.
    Bool = id(#{1=>a}) == #{1.0=>a},		%Optimizable.
    Bool = #{1=>a} == #{1.0=>a},		%Optimizable.
    io:format("Bool = ~p\n", [Bool]),

    ok.

%% OTP-7117.
nested_call_in_case(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    Dir = test_lib:get_data_dir(Config),
    Core = filename:join(Dir, "nested_call_in_case"),
    Opts = [from_core,{outdir,PrivDir}|test_lib:opt_opts(?MODULE)],
    io:format("~p", [Opts]),
    {ok,Mod} = c:c(Core, Opts),
    yes = Mod:a([1,2,3], 2),
    no = Mod:a([1,2,3], 4),
    {'EXIT',_} = (catch Mod:a(not_a_list, 42)),
    _ = code:delete(Mod),
    _ = code:purge(Mod),
    ok.

guard_try_catch(_Config) ->
    false = do_guard_try_catch(key, value),
    value = get(key),
    ok.

do_guard_try_catch(K, V) ->
    %% This try...catch block looks like a guard.
    %% Make sure that it is not optimized like a guard
    %% (the put/2 call must not be optimized away).
    try
	put(K, V),
	false
    catch
	_:_ ->
	    false
    end.

-record(cover_opt_guard_try, {list=[]}).

coverage(Config) when is_list(Config) ->
    {'EXIT',{{case_clause,{a,b,c}},_}} =
	(catch cover_will_match_list_type({a,b,c})),
    {'EXIT',{{case_clause,{a,b,c,d}},_}} =
	(catch cover_will_match_list_type({a,b,c,d})),
    a = cover_remove_non_vars_alias({a,b,c}),
    error = cover_will_match_lit_list(),
    {ok,[a]} = cover_is_safe_bool_expr(a),

    ok = cover_opt_guard_try(#cover_opt_guard_try{list=[a]}),
    error = cover_opt_guard_try(#cover_opt_guard_try{list=[]}),

    %% Make sure that we don't attempt to make literals
    %% out of pids. (Putting a pid into a #c_literal{}
    %% would crash later compiler passes.)
    case list_to_pid("<0.42.0>") of
	Pid when is_pid(Pid) -> ok
    end,

    %% Cover the non-variable case in bsm_do_an/4.
    ok = bsm_an_inlined(<<1>>, Config),
    error = bsm_an_inlined(<<1,2,3>>, Config),
    error = bsm_an_inlined([], Config),

    %% Cover eval_rel_op/4.
    Tuple = id({a,b}),
    false = case Tuple of
		{_,_} ->
		    Tuple =:= true
	    end,
    ok.

cover_will_match_list_type(A) ->
    case A of
	{a,_,_} ->				%Set type of A to {a,_,_}.
	    case A of
		{a,_,_,_} -> ok			%Compare type and pattern.
	    end
    end.

%% Make sure the remove_non_vars/4 can handle aliases in the type argument.
cover_remove_non_vars_alias(X) ->
    case X of
	{a=Y,_,_} ->				%Set type of A to {a=Y,_,_}.
	    case X of
		{_,_,_} ->			%Compare type and pattern.
		    Y
	    end
    end.

cover_will_match_lit_list() ->
    case {1,2,3} of				%Literal case expression.
	{_,$A,$A} ->				%Pattern that does not match.
	    ok;
	_ ->
	    error
    end.

cover_is_safe_bool_expr(X) ->
    %% Use a try...catch that looks like a try...catch in a guard.
    try
	%% let V = [X] in {ok,V}
	%%    is_safe_simple([X]) ==> true
	%%    is_safe_bool_expr([X]) ==> false
	V = [X],
	{ok,V}
    catch
	_:_ ->
	    false
    end.

cover_opt_guard_try(Msg) ->
    if
	length(Msg#cover_opt_guard_try.list) =/= 1 ->
	    error;
	true ->
	    ok
    end.

bsm_an_inlined(<<_:8>>, _) -> ok;
bsm_an_inlined(_, _) -> error.

unused_multiple_values_error(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    Dir = test_lib:get_data_dir(Config),
    Core = filename:join(Dir, "unused_multiple_values_error"),
    Opts = [no_copt,clint,return,from_core,{outdir,PrivDir}
	   |test_lib:opt_opts(?MODULE)],
    {error,[{unused_multiple_values_error,
	     [{none,core_lint,{return_mismatch,{hello,1}}}]}],
     []} = c:c(Core, Opts),
    ok.

unused_multiple_values(Config) when is_list(Config) ->
    put(unused_multiple_values, []),
    [false] = test_unused_multiple_values(false),
    [b,a,{a,b},false] = test_unused_multiple_values({a,b}),
    ok.

test_unused_multiple_values(X) ->
    ok = do_unused_multiple_values(X),
    get(unused_multiple_values).

do_unused_multiple_values(X) ->
    case do_something(X) of
        false ->
            A = false;
        Res ->
            {A,B} = Res,
            do_something(A),
            do_something(B)
    end,
    _ThisShouldNotFail = A,
    ok.

do_something(I) ->
    put(unused_multiple_values,
	[I|get(unused_multiple_values)]),
    I.


%% Make sure that multiple aliases does not cause
%% the case expression to be evaluated twice.
multiple_aliases(Config) when is_list(Config) ->
    do_ma(fun() ->
		  X = Y = run_once(),
		  {X,Y}
	  end, {ok,ok}),
    do_ma(fun() ->
		  case {true,run_once()} of
		      {true=A=B,ok=X=Y} ->
			  {A,B,X,Y}
		  end
	  end, {true,true,ok,ok}),
    ok.

do_ma(Fun, Expected) when is_function(Fun, 0) ->
    Expected = Fun(),
    ran_once = erase(run_once),
    ok.

run_once() ->
    undefined = put(run_once, ran_once),
    ok.


redundant_boolean_clauses(Config) when is_list(Config) ->
  X = id(0),
  yes = case X == 0 of
            false -> no;
            false -> no;
            true -> yes
        end.

mixed_matching_clauses(Config) when is_list(Config) ->
  0 = case #{} of
          #{} -> 0;
          a -> 1
      end,
  0 = case <<>> of
          <<>> -> 0;
          a -> 1
      end,
  ok.

unnecessary_building(Config) when is_list(Config) ->
    Term1 = do_unnecessary_building_1(test_lib:id(a)),
    [{a,a},{a,a}] = Term1,
    7 = erts_debug:size(Term1),

    %% The Input term should not be rebuilt (thus, it should
    %% only be counted once in the size of the combined term).
    Input = test_lib:id({a,b,c}),
    Term2 = test_lib:id(do_unnecessary_building_2(Input)),
    {b,[{a,b,c},none],x} = Term2,
    4+4+4+2 = erts_debug:size([Term2|Input]),

    ok.

do_unnecessary_building_1(S) ->
    %% The tuple must only be built once.
    F0 = F1 = {S,S},
    [F0,F1].

do_unnecessary_building_2({a,_,_}=T) ->
    %% The T term should not be rebuilt.
    {b,
     [_,_] = [T,none],
     x}.

%% This test tests that v3_core has provided annotations and that
%% sys_core_fold retains them, so that warnings produced by
%% sys_core_fold will have proper filenames and line numbers. Thus, no
%% "no_file" warnings.
no_no_file(_Config) ->
    {'EXIT',{{case_clause,0},_}} = (catch source(true, any)),
    surgery = (tim(#{reduction => any}))(),

    false = soul(#{[] => true}),
    {'EXIT',{{case_clause,true},_}} = (catch soul(#{[] => false})),

    ok = experiment(),
    ok.

source(true, Activities) ->
    case 0 of
	Activities when [] ->
	    Activities
    end.

tim(#{reduction := Emergency}) ->
    try
	fun() -> surgery end
    catch
	_ when [] ->
	    planet
    end.

soul(#{[] := Properly}) ->
    not case true of
	    Properly -> true;
	    Properly -> 0
	end.

experiment() ->
    case kingdom of
	_ ->
	    +case "map" of
		 _ -> 0.0
	     end
    end,
    ok.

id(I) -> I.
