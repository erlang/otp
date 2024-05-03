%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2022. All Rights Reserved.
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

-module(sanity_checks).

-compile(no_ssa_opt_destructive_update).

-export([check_fail/0,
         check_wrong_pass/0,
         check_xfail/0,
         check_prefix1/0,
         check_prefix2/0,
         check_prefix3/0,

         t0/0, t1/0, t2/0, t3/0, t4/0,
         t5/0, t6/0, t7/0, t8/0, t9/0,
         t10/0, t11/0, t12/0, t13/0, t14/0,
         t15/0, t16/0, t17/0, t18/0, t19/0,
         t20/0, t21/0, t22/0, t23/0, t24/0,
         t25/0, t26/0, t27/0, t28/0, t29/0,
         t30/0, t31/0, t32/1, t33/1, t34/1,
         t35/1, t36/0, t37/0, t38/0, t39/1,
         t40/0, t41/0, t42/0, t43/0, t44/0,

         check_env/0]).

%% Check that we do not trigger on the wrong pass
check_wrong_pass() ->
%ssa% (_) when this_pass_does_not_exist ->
%ssa% _ = this_instruction_does_not_exist().
    ok.

%% Check that failures work
check_fail() ->
%ssa% fail (_) when post_ssa_opt ->
%ssa% _ = this_instruction_does_not_exist().
    ok.

check_xfail() ->
%ssa% xfail (_) when post_ssa_opt ->
%ssa% _ = this_instruction_does_not_exist().
    ok.

check_prefix1() ->
%ssa% fail (_) when post_ssa_opt ->
%ssa% _ = this_instruction_does_not_exist().
    ok.

check_prefix2() ->
%%ssa% fail (_) when post_ssa_opt ->
%ssa% _ = this_instruction_does_not_exist().
    ok.

check_prefix3() ->
%%%ssa% fail (_) when post_ssa_opt ->
%ssa% _ = this_instruction_does_not_exist().
    ok.

%% Check map literals with any type which can be a key
t0() ->
%ssa% () when post_ssa_opt ->
%ssa% ret(#{}).
    #{}.

t1() ->
%ssa% () when post_ssa_opt ->
%ssa% ret(#{a=>b}).
    #{a=>b}.

t2() ->
%ssa% () when post_ssa_opt ->
%ssa% ret(#{a=>b,c=>d}).
    #{a=>b,c=>d}.

t3() ->
%ssa% () when post_ssa_opt ->
%ssa% ret(#{1=>1}).
    #{1=>1}.

t4() ->
%ssa% () when post_ssa_opt ->
%ssa% ret(#{3.14=>3.14}).
    #{3.14=>3.14}.

t5() ->
%ssa% () when post_ssa_opt ->
%ssa% ret(#{#{}=>1}).
    #{#{}=>1}.

t6() ->
%ssa% () when post_ssa_opt ->
%ssa% ret(#{<<17,15>> => 1}).
    #{<<17,15>> => 1}.

t7() ->
%ssa% () when post_ssa_opt ->
%ssa% ret(#{{} => 1}).
    #{{} => 1}.

t8() ->
%ssa% () when post_ssa_opt ->
%ssa% ret(#{{1,2} => 1}).
    #{{1,2} => 1}.

t9() ->
%ssa% () when post_ssa_opt ->
%ssa% ret(#{[] => 1}).
    #{[] => 1}.

t10() ->
%ssa% () when post_ssa_opt ->
%ssa% ret(#{[1,2] => 1}).
    #{[1,2] => 1}.


%% Check literals
t11() ->
%ssa% () when post_ssa_opt ->
%ssa% ret(atom).
    atom.

t12() ->
%ssa% () when post_ssa_opt ->
%ssa% ret(17).
    17.

t13() ->
%ssa% () when post_ssa_opt ->
%ssa% ret(3.14).
    3.14.

t14() ->
%ssa% () when post_ssa_opt ->
%ssa% ret(3.141(1.0e-3)).
    3.1415.

t15() ->
%ssa% () when post_ssa_opt ->
%ssa% _ = make_fun(fun t15/0).
    fun t15/0.

t16() ->
%ssa% () when post_ssa_opt ->
%ssa% ret(fun sanity_checks:t16/0).
    fun sanity_checks:t16/0.

t17() ->
%ssa% () when post_ssa_opt ->
%ssa% ret({}).
    {}.

t18() ->
%ssa% () when post_ssa_opt ->
%ssa% ret({1}).
    {1}.

t19() ->
%ssa% () when post_ssa_opt ->
%ssa% ret({1,2}).
    {1,2}.

t20() ->
%ssa% () when post_ssa_opt ->
%ssa% ret({1,2,3}).
    {1,2,3}.

t21() ->
%ssa% () when post_ssa_opt ->
%ssa% ret({1,...}).
    {1,2,3}.

t22() ->
%ssa% () when post_ssa_opt ->
%ssa% ret(<<>>).
    <<>>.

t23() ->
%ssa% () when post_ssa_opt ->
%ssa% ret(<<1>>).
    <<1>>.

t24() ->
%ssa% () when post_ssa_opt ->
%ssa% ret(<<1,2>>).
    <<1,2>>.

t25() ->
%ssa% () when post_ssa_opt ->
%ssa% ret(<<1,2,3>>).
    <<1,2,3>>.

t26() ->
%ssa% () when post_ssa_opt ->
%ssa% ret([]).
    [].

t27() ->
%ssa% () when post_ssa_opt ->
%ssa% ret([1]).
    [1].

t28() ->
%ssa% () when post_ssa_opt ->
%ssa% ret([1,2]).
    [1,2].

t29() ->
%ssa% () when post_ssa_opt ->
%ssa% ret([1,2,3]).
    [1,2,3].

t30() ->
%ssa% () when post_ssa_opt ->
%ssa% ret([1,2|3]).
    [1,2|3].

t31() ->
%ssa% () when post_ssa_opt ->
%ssa% ret([1,2,...]).
    [1,2,3].

%% Check that we handle a bif
t32(X) ->
%ssa% (X) when post_ssa_opt ->
%ssa% A = bif:'=='(X, 1).
    true = X == 1.

%% Check that we handle a br
t33(X) ->
%ssa% (X) when post_ssa_opt ->
%ssa% A = bif:'=='(X, 1),
%ssa% br(A, 9, 8).
    true = X == 1.

%% Check that we handle a branch and variable labels
t34(X) ->
%ssa% (X) when post_ssa_opt ->
%ssa% A = bif:'=='(X, 1),
%ssa% br(A, Succ, Fail),
%ssa% label Succ,
%ssa% ret(true),
%ssa% label Fail,
%ssa% _ = match_fail(badmatch, ...).
    true = X == 1.

%% Check that we handle a switch
t35(X) ->
%ssa% (X) when post_ssa_opt ->
%ssa% switch(X, Fail, [{1,One},{2,Two},...]),
%ssa% label Two,
%ssa% ret(b),
%ssa% label One,
%ssa% ret(a),
%ssa% label Fail,
%ssa% _ = match_fail(case_clause, ...).
    case X of
	1 ->
	    a;
	2 ->
	    b;
	3 ->
	    c
    end.

%% Check a complex pattern
t36() ->
%ssa% () when post_ssa_opt ->
%ssa% ret({{}, 4,a,{[45|a], 17.0, 3.141(1.0e-3), #{a=>b}}, [1,2,...], ...}).
    {{}, 4,a,{[45|a], 17.0, 3.14,#{a=>b}}, [1,2,3,4], 3.1415, 5, 7}.

%% Check '...' when used for instruction arguments.
t37() ->
%ssa% () when post_ssa_opt ->
%ssa% _ = call(fun e:f0/4, _, _, ...),
%ssa% _ = call(fun e:f1/4, ...).
    e:f0(1, 2, 3, 4),
    e:f1(1, 2, 3, 4).

%% Check that we fail if we specify too many arguments.
t38() ->
%ssa% xfail (_) when post_ssa_opt ->
%ssa% _ = call(fun e:f0/4, ...).
    e:f0(1, 2, 3, 4).

%% Check that we fail if we specify too few arguments.
t39(_) ->
%ssa% xfail () when post_ssa_opt ->
%ssa% _ = call(fun e:f0/4, ...).
    e:f0(1, 2, 3, 4).


%% Check ... when used to match part of a tuple.
t40() ->
%ssa% () when post_ssa_opt ->
%ssa% _ = call(fun e:f0/1, {...}).
    e:f0({1, 2, 3, 4}).

t41() ->
%ssa% () when post_ssa_opt ->
%ssa% _ = call(fun e:f0/1, {1, ...}).
    e:f0({1, 2, 3, 4}).

t42() ->
%ssa% () when post_ssa_opt ->
%ssa% _ = call(fun e:f0/1, {1, 2, ...}).
    e:f0({1, 2, 3, 4}).

t43() ->
%ssa% fail () when post_ssa_opt ->
%ssa% _ = call(fun e:f0/1, {1, 2, ...}).
    e:f0({1}).

t44() ->
%ssa% () when post_ssa_opt ->
%ssa% _ = call(fun e:f0/1, {...}).
    e:f0({}).

%% Ensure bug which trashed the environment after matching a literal
%% bitstring stays fixed.
check_env() ->
%ssa% () when post_ssa_opt ->
%ssa% X = bs_create_bin(append, _, <<>>, ...),
%ssa% ret(X).
    A = <<>>,
    B = ex:f(),
    <<A/binary, B/binary>>.
