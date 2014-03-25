%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2013. All Rights Reserved.
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
-module(core_fold_SUITE).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 t_element/1,setelement/1,t_length/1,append/1,t_apply/1,bifs/1,
	 eq/1,nested_call_in_case/1,guard_try_catch/1,coverage/1,
	 unused_multiple_values_error/1,unused_multiple_values/1,
	 multiple_aliases/1,redundant_boolean_clauses/1,mixed_matching_clauses/1]).

-export([foo/0,foo/1,foo/2,foo/3]).

-include_lib("test_server/include/test_server.hrl").

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    test_lib:recompile(?MODULE),
    [{group,p}].

groups() -> 
    [{p,test_lib:parallel(),
      [t_element,setelement,t_length,append,t_apply,bifs,
       eq,nested_call_in_case,guard_try_catch,coverage,
       unused_multiple_values_error,unused_multiple_values,
       multiple_aliases,redundant_boolean_clauses,mixed_matching_clauses]}].


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
    ?line X = id(element(1, {X,y,z})),
    ?line b = id(element(2, {a,b,c,d})),

    %% No optimization, but should work.
    Tuple = id({x,y,z}),
    Pos = id(3),
    ?line x = id(element(1, Tuple)),
    ?line c = id(element(Pos, {a,b,c,d})),
    ?line X = id(element(Pos, {a,b,X,d})),
    ?line z = id(element(Pos, Tuple)),

    %% Calls that will fail.
    ?line {'EXIT',{badarg,_}} = (catch element(5, {a,b,c,d})),
    ?line {'EXIT',{badarg,_}} = (catch element(5, {a,b,X,d})),
    ?line {'EXIT',{badarg,_}} = (catch element(5.0, {a,b,X,d})),
    {'EXIT',{badarg,_}} = (catch element(2, not_a_tuple)),
    {'EXIT',{badarg,_}} = (catch element(2, [])),
    {'EXIT',{badarg,_}} = (catch element(2, Tuple == 3)),
    case id({a,b,c}) of
	{_,_,_}=Tup ->
	    ?line {'EXIT',{badarg,_}} = (catch element(4, Tup))
    end,

    ok.

setelement(Config) when is_list(Config) ->
    X = id(b),
    New = id([1,2,3]),
    ?line {y,b,c} = id(setelement(1, {a,b,c}, y)),
    ?line {y,b,c} = id(setelement(1, {a,X,c}, y)),
    ?line {a,y,c} = id(setelement(2, {a,X,c}, y)),
    ?line {a,[1,2,3],c} = id(setelement(2, {a,b,c}, New)),
    ?line {a,[1,2,3],c} = id(setelement(2, {a,X,c}, New)),
    ?line {a,b,[1,2,3]} = id(setelement(3, {a,b,c}, New)),
    ?line {a,b,[1,2,3]} = id(setelement(3, {a,X,c}, New)),

    ?line {'EXIT',{badarg,_}} = (catch setelement_crash({a,b,c,d,e,f})),
    ?line error = setelement_crash_2({a,b,c,d,e,f}, <<42>>),

    {'EXIT',{badarg,_}} = (catch setelement(1, not_a_tuple, New)),

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
    ?line 0 = id(length([])),
    ?line 1 = id(length([x])),
    ?line 2 = id(length([x,Blurf])),
    ?line 4 = id(length([x,Blurf,a,b])),

    %% No or partial optimization.
    ?line 4 = length(Tail),
    ?line 5 = id(length([x|Tail])),

    %% Will fail.
    ?line {'EXIT',{badarg,_}} = (catch id(length([a,b|c]))),
    ?line {'EXIT',{badarg,_}} = (catch id(length([a,Blurf|c]))),
    ?line {'EXIT',{badarg,_}} = (catch id(length(atom))),

    ok.

-define(APPEND(A, B), (fun(Res) ->
			       Res = lists:append(A, B),
			       Res = erlang:append(A, B),
			       Res = erlang:'++'(A, B)
		       end)(A++B)).

append(Config) when is_list(Config) ->
    A = id(0),
    ?line [a,b,c,d,e,f,g,h,i,j,k] = id(?APPEND([a,b,c,d,e,f],[g,h,i,j,k])),
    ?line [a,b,c,d,e] = id(?APPEND([a,b,c],id([d,e]))),
    ?line [0,1,2,3,4,5,6] = id(?APPEND([A,1,2,3],[4,5,6])),
    ?line {'EXIT',{badarg,_}} = (catch id(?APPEND([A|blurf],[4,5,6]))),
    ok.

t_apply(Config) when is_list(Config) ->
    ?line ok = apply(?MODULE, foo, []),
    ?line 4 = apply(?MODULE, foo, [3]),
    ?line 7 = apply(?MODULE, foo, [3,4]),
    ?line 12 = apply(?MODULE, foo, [id(8),4]),
    ?line 21 = apply(?MODULE, foo, [8,id(9),4]),
    ?line 20 = apply(?MODULE, foo, [8,8,id(4)]),
    ?line 24 = apply(?MODULE, foo, [id(10),10,4]),

    M = id(?MODULE),
    ?line ok = apply(M, foo, []),
    ?line 4 = apply(M, foo, [3]),
    ?line 16.0 = apply(M, foo, [12.0,4]),

    %% Will fail.
    ?line {'EXIT',{badarg,_}} = (catch apply([a,b,c], foo, [])),
    ?line {'EXIT',{badarg,_}} = (catch apply(42, foo, [])),
    ?line {'EXIT',{badarg,_}} = (catch apply(?MODULE, 45, [xx])),
    ?line {'EXIT',{badarg,_}} = (catch apply(?MODULE, foo, {a,b})),
    ?line {'EXIT',{badarg,_}} = (catch apply(M, M, [1009|10010])),
    ?line {'EXIT',{badarg,_}} = (catch apply(?MODULE, foo, [10000|9999])),
    ?line {'EXIT',{badarg,_}} = (catch apply(?MODULE, foo, a)),

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
    ?line <<1,2,3,4>> = id(list_to_binary([1,2,3,4])),
    ok.

-define(CMP_SAME(A0, B), (fun(A) -> true = A == B, false = A /= B end)(id(A0))).
-define(CMP_DIFF(A0, B), (fun(A) -> false = A == B, true = A /= B end)(id(A0))).
	       
eq(Config) when is_list(Config) ->
    ?line ?CMP_SAME([a,b,c], [a,b,c]),
    ?line ?CMP_SAME([42.0], [42.0]),
    ?line ?CMP_SAME([42], [42]),
    ?line ?CMP_SAME([42.0], [42]),

    ?line ?CMP_DIFF(a, [a]),
    ?line ?CMP_DIFF(a, {1,2,3}),

    ok.

%% OTP-7117.
nested_call_in_case(Config) when is_list(Config) ->
    ?line PrivDir = ?config(priv_dir, Config),
    ?line Dir = filename:dirname(code:which(?MODULE)),
    ?line Core = filename:join(Dir, "nested_call_in_case"),
    ?line Opts = [from_core,{outdir,PrivDir}|test_lib:opt_opts(?MODULE)],
    ?line io:format("~p", [Opts]),
    ?line {ok,Mod} = c:c(Core, Opts),
    ?line yes = Mod:a([1,2,3], 2),
    ?line no = Mod:a([1,2,3], 4),
    ?line {'EXIT',_} = (catch Mod:a(not_a_list, 42)),
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

coverage(Config) when is_list(Config) ->
    ?line {'EXIT',{{case_clause,{a,b,c}},_}} =
	(catch cover_will_match_list_type({a,b,c})),
    ?line {'EXIT',{{case_clause,{a,b,c,d}},_}} =
	(catch cover_will_match_list_type({a,b,c,d})),
    ?line a = cover_remove_non_vars_alias({a,b,c}),
    ?line error = cover_will_match_lit_list(),
    {ok,[a]} = cover_is_safe_bool_expr(a),

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

bsm_an_inlined(<<_:8>>, _) -> ok;
bsm_an_inlined(_, _) -> error.

unused_multiple_values_error(Config) when is_list(Config) ->
    PrivDir = ?config(priv_dir, Config),
    Dir = filename:dirname(code:which(?MODULE)),
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

id(I) -> I.
