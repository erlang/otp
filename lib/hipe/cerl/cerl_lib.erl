%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2016. All Rights Reserved.
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

%% @doc Utility functions for Core Erlang abstract syntax trees.
%%
%% <p>Syntax trees are defined in the module <a
%% href=""><code>cerl</code></a>.</p>
%%
%% @type cerl() = cerl:cerl()

-module(cerl_lib).

-define(NO_UNUSED, true).

-export([is_safe_expr/2, reduce_expr/1, is_simple_clause/1,
	 is_bool_switch/1, bool_switch_cases/1]).
-ifndef(NO_UNUSED).
-export([is_safe_expr/1, is_pure_expr/1, is_pure_expr/2,
	 make_bool_switch/3]).
-endif.


%% Test if a clause has a single pattern and an always-true guard.

-spec is_simple_clause(cerl:c_clause()) -> boolean().

is_simple_clause(C) ->
    case cerl:clause_pats(C) of
	[_P] ->
	    G = cerl:clause_guard(C),
	    case cerl_clauses:eval_guard(G) of
		{value, true} -> true;
		_ -> false
	    end;
	_ -> false
    end.

%% Creating an if-then-else construct that can be recognized as such.
%% `Test' *must* be guaranteed to return a boolean.

-ifndef(NO_UNUSED).
make_bool_switch(Test, True, False) ->
    Cs = [cerl:c_clause([cerl:c_atom(true)], True),
	  cerl:c_clause([cerl:c_atom(false)], False)],
    cerl:c_case(Test, Cs).
-endif.

%% A boolean switch cannot have a catch-all; only true/false branches.

-spec is_bool_switch([cerl:c_clause()]) -> boolean().

is_bool_switch([C1, C2]) ->
    case is_simple_clause(C1) andalso is_simple_clause(C2) of
	true ->
	    [P1] = cerl:clause_pats(C1),
	    [P2] = cerl:clause_pats(C2),
	    case cerl:is_c_atom(P1) andalso cerl:is_c_atom(P2) of
		true ->
		    A1 = cerl:concrete(P1),
		    A2 = cerl:concrete(P2),
		    is_boolean(A1) andalso is_boolean(A2)
			andalso A1 =/= A2;
		false ->
		    false
	    end;
	false ->
	    false
    end;
is_bool_switch(_) ->
    false.

%% Returns the true-body and the false-body for boolean switch clauses.

-spec bool_switch_cases([cerl:c_clause()]) -> {cerl:cerl(), cerl:cerl()}.

bool_switch_cases([C1, C2]) ->
    B1 = cerl:clause_body(C1),
    B2 = cerl:clause_body(C2),
    [P1] = cerl:clause_pats(C1),
    case cerl:concrete(P1) of
	true ->
	    {B1, B2};
	false ->
	    {B2, B1}
    end.

%%
%% The type of the check functions like the default check below - XXX: refine
%%
-type check_fun() :: fun((_, _) -> boolean()).

%% The default function property check always returns `false':

default_check(_Property, _Function) -> false.


%% @spec is_safe_expr(Expr::cerl()) -> boolean()
%%
%% @doc Returns `true' if `Expr' represents a "safe" Core Erlang
%% expression, otherwise `false'. An expression is safe if it always
%% completes normally and does not modify the state (although the return
%% value may depend on the state).
%%
%% Expressions of type `apply', `case', `receive' and `binary' are
%% always considered unsafe by this function.

%% TODO: update cerl_inline to use these functions instead.

-ifndef(NO_UNUSED).
is_safe_expr(E) ->
    Check = fun default_check/2,
    is_safe_expr(E, Check).
-endif.
%% @clear

-spec is_safe_expr(cerl:cerl(), check_fun()) -> boolean().

is_safe_expr(E, Check) ->
    case cerl:type(E) of
	literal ->
	    true;
	var ->
	    true;
	'fun' ->
	    true;
	values ->
	    is_safe_expr_list(cerl:values_es(E), Check);
	tuple ->
	    is_safe_expr_list(cerl:tuple_es(E), Check);
	cons ->
	    case is_safe_expr(cerl:cons_hd(E), Check) of
		true ->
		    is_safe_expr(cerl:cons_tl(E), Check);
		false ->
		    false
	    end;
	'let' ->
	    case is_safe_expr(cerl:let_arg(E), Check) of
		true ->
		    is_safe_expr(cerl:let_body(E), Check);
		false ->
		    false
	    end;
	letrec ->
	    is_safe_expr(cerl:letrec_body(E), Check);
	seq ->
	    case is_safe_expr(cerl:seq_arg(E), Check) of
		true ->
		    is_safe_expr(cerl:seq_body(E), Check);
		false ->
		    false
	    end;
	'catch' ->
	    is_safe_expr(cerl:catch_body(E), Check);
	'try' ->
	    %% If the guarded expression is safe, the try-handler will
	    %% never be evaluated, so we need only check the body.  If
	    %% the guarded expression is pure, but could fail, we also
	    %% have to check the handler.
	    case is_safe_expr(cerl:try_arg(E), Check) of
		true ->
		    is_safe_expr(cerl:try_body(E), Check);
		false ->
		    case is_pure_expr(cerl:try_arg(E), Check) of
			true ->
			    case is_safe_expr(cerl:try_body(E), Check) of
				true ->
				    is_safe_expr(cerl:try_handler(E), Check);
				false ->
				    false
			    end;
			false ->
			    false
		    end
	    end;
	primop ->
	    Name = cerl:atom_val(cerl:primop_name(E)),
	    As = cerl:primop_args(E),
	    case Check(safe, {Name, length(As)}) of
		true ->
		    is_safe_expr_list(As, Check);
		false ->
		    false
	    end;
	call ->
	    Module = cerl:call_module(E),
	    Name = cerl:call_name(E),
	    case cerl:is_c_atom(Module) and cerl:is_c_atom(Name) of
		true ->
		    M = cerl:atom_val(Module),
		    F = cerl:atom_val(Name),
		    As = cerl:call_args(E),
		    case Check(safe, {M, F, length(As)}) of
			true ->
			    is_safe_expr_list(As, Check);
			false ->
			    false
		    end;
		false ->
		    false    % Call to unknown function
	    end;
	_ ->
	    false
    end.

is_safe_expr_list([E | Es], Check) ->
    case is_safe_expr(E, Check) of
	true ->
	    is_safe_expr_list(Es, Check);
	false ->
	    false
    end;
is_safe_expr_list([], _Check) ->
    true.


%% @spec (Expr::cerl()) -> bool()
%%
%% @doc Returns `true' if `Expr' represents a "pure" Core Erlang
%% expression, otherwise `false'. An expression is pure if it does not
%% affect the state, nor depend on the state, although its evaluation is
%% not guaranteed to complete normally for all input.
%%
%% Expressions of type `apply', `case', `receive' and `binary' are
%% always considered impure by this function.

-ifndef(NO_UNUSED).
is_pure_expr(E) ->
    Check = fun default_check/2,
    is_pure_expr(E, Check).
-endif.
%% @clear

is_pure_expr(E, Check) ->
    case cerl:type(E) of
	literal ->
	    true;
	var ->
	    true;
	'fun' ->
	    true;
	values ->
	    is_pure_expr_list(cerl:values_es(E), Check);
	tuple ->
	    is_pure_expr_list(cerl:tuple_es(E), Check);
	cons ->
	    case is_pure_expr(cerl:cons_hd(E), Check) of
		true ->
		    is_pure_expr(cerl:cons_tl(E), Check);
		false ->
		    false
	    end;
	'let' ->
	    case is_pure_expr(cerl:let_arg(E), Check) of
		true ->
		    is_pure_expr(cerl:let_body(E), Check);
		false ->
		    false
	    end;
	letrec ->
	    is_pure_expr(cerl:letrec_body(E), Check);
	seq ->
	    case is_pure_expr(cerl:seq_arg(E), Check) of
		true ->
		    is_pure_expr(cerl:seq_body(E), Check);
		false ->
		    false
	    end;
	'catch' ->
	    is_pure_expr(cerl:catch_body(E), Check);
	'try' ->
	    case is_pure_expr(cerl:try_arg(E), Check) of
		true ->
		    case is_pure_expr(cerl:try_body(E), Check) of
			true ->
			    is_pure_expr(cerl:try_handler(E), Check);
			false ->
			    false
		    end;
		false ->
		    false
	    end;
	primop ->
	    Name = cerl:atom_val(cerl:primop_name(E)),
	    As = cerl:primop_args(E),
	    case Check(pure, {Name, length(As)}) of
		true ->
		    is_pure_expr_list(As, Check);
		false ->
		    false
	    end;
	call ->
	    Module = cerl:call_module(E),
	    Name = cerl:call_name(E),
	    case cerl:is_c_atom(Module) and cerl:is_c_atom(Name) of
		true ->
		    M = cerl:atom_val(Module),
		    F = cerl:atom_val(Name),
		    As = cerl:call_args(E),
		    case Check(pure, {M, F, length(As)}) of
			true ->
			    is_pure_expr_list(As, Check);
			false ->
			    false
		    end;
		false ->
		    false    % Call to unknown function
	    end;
	_ ->
	    false
    end.

is_pure_expr_list([E | Es], Check) ->
    case is_pure_expr(E, Check) of
	true ->
	    is_pure_expr_list(Es, Check);
	false ->
	    false
    end;
is_pure_expr_list([], _Check) ->
    true.


%% Peephole optimizations
%%
%% This is only intended to be a light-weight cleanup optimizer,
%% removing small things that may e.g. have been generated by other
%% optimization passes or in the translation from higher-level code.
%% It is not recursive in general - it only descends until it can do no
%% more work in the current context.
%%
%% To expose hidden cases of final expressions (enabling last call
%% optimization), we try to remove all trivial let-bindings (`let X = Y
%% in X', `let X = Y in Y', `let X = Y in let ... in ...', `let X = let
%% ... in ... in ...', etc.). We do not, however, try to recognize any
%% other similar cases, even for simple `case'-expressions like `case E
%% of X -> X end', or simultaneous multiple-value bindings.

-spec reduce_expr(cerl:cerl()) -> cerl:cerl().

reduce_expr(E) ->
    Check = fun default_check/2,
    reduce_expr(E, Check).

-spec reduce_expr(cerl:cerl(), check_fun()) -> cerl:cerl().

reduce_expr(E, Check) ->
    case cerl:type(E) of
	values ->
	    case cerl:values_es(E) of
		[E1] ->
		    %% Not really an "optimization" in itself, but
		    %% enables other rewritings by removing the wrapper.
		    reduce_expr(E1, Check);
		_ ->
		    E
	    end;
	'seq' ->
	    A = reduce_expr(cerl:seq_arg(E), Check),
	    B = reduce_expr(cerl:seq_body(E), Check),
	    %% `do <E1> <E2>' is equivalent to `<E2>' if `<E1>' is
	    %% "safe" (cannot effect the behaviour in any way).
	    case is_safe_expr(A, Check) of
		true ->
		    B;
		false ->
		    case cerl:is_c_seq(B) of
			true ->
			    %% Rewrite `do <E1> do <E2> <E3>' to `do do
			    %% <E1> <E2> <E3>' so that the "body" of the
			    %% outermost seq-operator is the expression
			    %% which produces the final result (i.e.,
			    %% E3). This can make other optimizations
			    %% easier; see `let'.
			    B1 = cerl:seq_arg(B),
			    B2 = cerl:seq_body(B),
			    cerl:c_seq(cerl:c_seq(A, B1), B2);
			false ->
			    cerl:c_seq(A, B)
		    end
	    end;
	'let' ->
	    A = reduce_expr(cerl:let_arg(E), Check),
	    case cerl:is_c_seq(A) of
		true ->
		    %% `let X = do <E1> <E2> in Y' is equivalent to `do
		    %% <E1> let X = <E2> in Y'. Note that `<E2>' cannot
		    %% be a seq-operator, due to the `seq' optimization.
		    A1 = cerl:seq_arg(A),
		    A2 = cerl:seq_body(A),
		    E1 = cerl:update_c_let(E, cerl:let_vars(E),
					   A2, cerl:let_body(E)),
		    cerl:c_seq(A1, reduce_expr(E1, Check));
		false ->
		    B = reduce_expr(cerl:let_body(E), Check),
		    Vs = cerl:let_vars(E),
		    %% We give up if the body does not reduce to a
		    %% single variable. This is not a generic copy
		    %% propagation.
		    case cerl:type(B) of
			var when length(Vs) =:= 1 ->
			    %% We have `let <V1> = <E> in <V2>':
			    [V] = Vs,
			    N1 = cerl:var_name(V),
			    N2 = cerl:var_name(B),
			    if N1 =:= N2 ->
				    %% `let X = <E> in X' equals `<E>'
				    A;
			       true ->
				    %% `let X = <E> in Y' when X and Y
				    %% are different variables is
				    %% equivalent to `do <E> Y'.
				    reduce_expr(cerl:c_seq(A, B), Check)
			    end;
			literal ->
			    %% `let X = <E> in T' when T is a literal
			    %% term is equivalent to `do <E> T'.
			    reduce_expr(cerl:c_seq(A, B), Check);
			_ ->
			    cerl:update_c_let(E, Vs, A, B)
		    end
	    end;
	'try' ->
	    %% Get rid of unnecessary try-expressions.
	    A = reduce_expr(cerl:try_arg(E), Check),
	    B = reduce_expr(cerl:try_body(E), Check),
	    case is_safe_expr(A, Check) of
		true ->
		    B;
		false ->
		    cerl:update_c_try(E, A, cerl:try_vars(E), B,
				      cerl:try_evars(E),
				      cerl:try_handler(E))
	    end;
	'catch' ->
	    %% Just a simpler form of try-expressions.
	    B = reduce_expr(cerl:catch_body(E), Check),
	    case is_safe_expr(B, Check) of
		true ->
		    B;
		false ->
		    cerl:update_c_catch(E, B)
	    end;
	_ ->
	    E
    end.
