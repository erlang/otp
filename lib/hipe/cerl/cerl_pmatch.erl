%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2016. All Rights Reserved.
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
%% @author Richard Carlsson <richardc@it.uu.se>
%% @copyright 2000-2006 Richard Carlsson
%%
%% @doc Core Erlang pattern matching compiler.
%%
%% <p>For reference, see Simon L. Peyton Jones "The Implementation of
%% Functional Programming Languages", chapter 5 (by Phil Wadler).</p>
%%
%% @type cerl() = cerl:cerl().
%%     Abstract Core Erlang syntax trees.
%% @type cerl_records() = cerl:cerl_records().
%%     An explicit record representation of Core Erlang syntax trees.

-module(cerl_pmatch).

%%-define(NO_UNUSED, true).

-export([clauses/2]).
-ifndef(NO_UNUSED).
-export([transform/2, core_transform/2, expr/2]).
-endif.

-import(lists, [all/2, splitwith/2, foldr/3, keysort/2, foldl/3,
		mapfoldl/3]).

-define(binary_id, {binary}).
-define(cons_id, {cons}).
-define(tuple_id, {tuple}).
-define(literal_id(V), V).


%% @spec core_transform(Module::cerl_records(), Options::[term()]) ->
%%           cerl_records()
%%
%% @doc Transforms a module represented by records. See
%% <code>transform/2</code> for details.
%%
%% <p>Use the compiler option <code>{core_transform, cerl_pmatch}</code>
%% to insert this function as a compilation pass.</p>
%%
%% @see transform/2

-ifndef(NO_UNUSED).
-spec core_transform(cerl:c_module(), [_]) -> cerl:c_module().

core_transform(M, Opts) ->
    cerl:to_records(transform(cerl:from_records(M), Opts)).
-endif.	% NO_UNUSED
%% @clear


%% @spec transform(Module::cerl(), Options::[term()]) -> cerl()
%%
%% @doc Rewrites all <code>case</code>-clauses in <code>Module</code>.
%% <code>receive</code>-clauses are not affected. Currently, no options
%% are available.
%%
%% @see clauses/2
%% @see expr/2
%% @see core_transform/2

-ifndef(NO_UNUSED).
-spec transform(cerl:cerl(), [_]) -> cerl:cerl().

transform(M, _Opts) ->
  expr(M, env__empty()).
-endif.	% NO_UNUSED
%% @clear


%% @spec clauses(Clauses::[Clause], Env) -> {Expr, Vars}
%%    Clause = cerl()
%%    Expr = cerl()
%%    Vars = [cerl()]
%%    Env = rec_env:environment()
%%
%% @doc Rewrites a sequence of clauses to an equivalent expression,
%% removing as much repeated testing as possible. Returns a pair
%% <code>{Expr, Vars}</code>, where <code>Expr</code> is the resulting
%% expression, and <code>Vars</code> is a list of new variables (i.e.,
%% not already in the given environment) to be bound to the arguments to
%% the switch. The following is a typical example (assuming
%% <code>E</code> is a Core Erlang case expression):
%% <pre>
%%   handle_case(E, Env) ->
%%       Cs = case_clauses(E),
%%       {E1, Vs} = cerl_pmatch(Cs, Env),
%%       c_let(Vs, case_arg(E), E1).
%% </pre>
%% 
%% <p>The environment is used for generating new variables which do not
%% shadow existing bindings.</p>
%% 
%% @see rec_env
%% @see expr/2
%% @see transform/2

-spec clauses([cerl:cerl(),...], rec_env:environment()) ->
          {cerl:cerl(), [cerl:cerl()]}.

clauses(Cs, Env) ->
    clauses(Cs, none, Env).

clauses([C | _] = Cs, Else, Env) ->
    Vs = new_vars(cerl:clause_arity(C), Env),
    E = match(Vs, Cs, Else, add_vars(Vs, Env)),
    {E, Vs}.

%% The implementation very closely follows that described in the book.

match([], Cs, Else, _Env) ->
    %% If the "default action" is the atom 'none', it is simply not
    %% added; otherwise it is put in the body of a final catch-all
    %% clause (which is often removed by the below optimization).
    Cs1 = if Else =:= none -> Cs;
	     true -> Cs ++ [cerl:c_clause([], Else)]
	  end,
    %% This clause reduction is an important optimization. It selects a
    %% clause body if possible, and otherwise just removes dead clauses.
    case cerl_clauses:reduce(Cs1) of
 	{true, {C, []}} ->    % if we get bindings, something is wrong!
 	    cerl:clause_body(C);
 	{false, Cs2} ->
	    %% This happens when guards are nontrivial.
 	    cerl:c_case(cerl:c_values([]), Cs2)
    end;
match([V | _] = Vs, Cs, Else, Env) ->
    foldr(fun (CsF, ElseF) ->
		  match_var_con(Vs, CsF, ElseF, Env)
	  end,
	  Else,
	  group([unalias(C, V) || C <- Cs], fun is_var_clause/1)).

group([], _F) ->
    [];
group([X | _] = Xs, F) ->
    group(Xs, F, F(X)).

group(Xs, F, P) ->
    {First, Rest} = splitwith(fun (X) -> F(X) =:= P end, Xs),
    [First | group(Rest, F)].

is_var_clause(C) ->
    cerl:is_c_var(hd(cerl:clause_pats(C))).

%% To avoid code duplication, if the 'Else' expression is too big, we
%% put it in a local function definition instead, and replace it with a
%% call. (Note that it is important that 'is_lightweight' does not yield
%% 'true' for a simple function application, or we will create a lot of
%% unnecessary extra functions.)

match_var_con(Vs, Cs, none = Else, Env) ->
    match_var_con_1(Vs, Cs, Else, Env);
match_var_con(Vs, Cs, Else, Env) ->
    case is_lightweight(Else) of
	true ->
	    match_var_con_1(Vs, Cs, Else, Env);
	false ->
	    F = new_fvar("match_", 0, Env),
	    Else1 = cerl:c_apply(F, []),
	    Env1 = add_vars([F], Env),
	    cerl:c_letrec([{F, cerl:c_fun([], Else)}],
			  match_var_con_1(Vs, Cs, Else1, Env1))
    end.

match_var_con_1(Vs, Cs, Else, Env) ->
    case is_var_clause(hd(Cs)) of
	true ->
	    match_var(Vs, Cs, Else, Env);
	false ->
	    match_con(Vs, Cs, Else, Env)
    end.

match_var([V | Vs], Cs, Else, Env) ->
    Cs1 = [begin
	       [P | Ps] = cerl:clause_pats(C),
	       G = make_let([P], V, cerl:clause_guard(C)),
	       B = make_let([P], V, cerl:clause_body(C)),
	       cerl:update_c_clause(C, Ps, G, B)
	   end
	   || C <- Cs],
    match(Vs, Cs1, Else, Env).

%% Since Erlang is dynamically typed, we must include the possibility
%% that none of the constructors in the group will match, and in that
%% case the "Else" code will be executed (unless it is 'none'), in the
%% body of a final catch-all clause.

match_con([V | Vs], Cs, Else, Env) ->
    case group_con(Cs) of 
      [{_, _, Gs}] ->
 	    %% Don't create a group type switch if there is only one
 	    %% such group
	    make_switch(V, [match_congroup(DG, Vs, CsG, Else, Env)
 			    || {DG, _, CsG} <- Gs],
 			Else, Env);
	Ts ->
	    Cs1 = [match_typegroup(T, V, Vs, Gs, Else, Env)
		   || {T, _, Gs} <- Ts],
	    make_switch(V, Cs1, Else, Env)
    end.


match_typegroup(_T, _V, Vs, [{D, _, Cs}], Else, Env) when element(1, D) /= ?binary_id ->
    %% Don't create a group type switch if there is only one constructor
    %% in the group. (Note that this always happens for '[]'.)  
    %% Special case for binaries which always get a group switch
    match_congroup(D, Vs, Cs, Else, Env);
match_typegroup(T, V, Vs, Gs, Else, Env) ->
    Body = make_switch(V, [match_congroup(D, Vs, Cs, Else, Env)
			 ||  {D, _, Cs} <- Gs],
		       Else, Env),
    typetest_clause(T, V, Body, Env).

match_congroup({?binary_id, Segs}, Vs, Cs, Else, Env) ->
    Body = match(Vs, Cs, Else, Env),
    cerl:c_clause([make_pat(?binary_id, Segs)], Body);

match_congroup({D, A}, Vs, Cs, Else, Env) ->
    Vs1 = new_vars(A, Env),
    Body = match(Vs1 ++ Vs, Cs, Else, add_vars(Vs1, Env)),
    cerl:c_clause([make_pat(D, Vs1)], Body).

make_switch(V, Cs, Else, Env) ->
    cerl:c_case(V, if Else =:= none -> Cs;
		      true -> Cs ++ [cerl:c_clause([new_var(Env)],
						   Else)]
		   end).

%% We preserve the relative order of different-type constructors as they
%% were originally listed. This is done by tracking the clause numbers.

group_con(Cs) ->
    {Cs1, _} = mapfoldl(fun (C, N) ->
				[P | Ps] = cerl:clause_pats(C),
				Ps1 = sub_pats(P) ++ Ps,
				G = cerl:clause_guard(C),
				B = cerl:clause_body(C),
				C1 = cerl:update_c_clause(C, Ps1, G, B),
				D = con_desc(P),
				{{D, N, C1}, N + 1}
			end,
			0, Cs),
    %% Sort and group constructors.
    Css = group(keysort(1, Cs1), fun ({D,_,_}) -> D end),
    %% Sort each group "back" by line number, and move the descriptor
    %% and line number to the wrapper for the group.
    Gs = [finalize_congroup(C) || C <- Css],
    %% Group by type only (put e.g. different-arity tuples together).
    Gss = group(Gs, fun ({D,_,_}) -> con_desc_type(D) end),
    %% Sort and wrap the type groups.
    Ts = [finalize_typegroup(G) || G <- Gss],
    %% Sort type-groups by first clause order
    keysort(2, Ts).

finalize_congroup(Cs) ->
    [{D,N,_}|_] = Cs1 = keysort(2, Cs),
    {D, N, [C || {_,_,C} <- Cs1]}.

finalize_typegroup(Gs) ->
    [{D,N,_}|_] = Gs1 = keysort(2, Gs),
    {con_desc_type(D), N, Gs1}.

%% Since Erlang clause patterns can contain "alias patterns", we must
%% eliminate these, by turning them into let-definitions in the guards
%% and bodies of the clauses.

unalias(C, V) -> 
    [P | Ps] = cerl:clause_pats(C),
    B = cerl:clause_body(C),
    G = cerl:clause_guard(C),
    unalias(P, V, Ps, B, G, C).

unalias(P, V, Ps, B, G, C) ->
    case cerl:type(P) of
	alias ->
	    V1 = cerl:alias_var(P),
	    B1 = make_let([V1], V, B),
	    G1 = make_let([V1], V, G),
	    unalias(cerl:alias_pat(P), V, Ps, B1, G1, C);
	_ ->
	    cerl:update_c_clause(C, [P | Ps], G, B)
    end.

%% Generating a type-switch clause

typetest_clause([], _V, E, _Env) ->
    cerl:c_clause([cerl:c_nil()], E);
typetest_clause(atom, V, E, _Env) ->
    typetest_clause_1(is_atom, V, E);
typetest_clause(integer, V, E, _Env) ->
    typetest_clause_1(is_integer, V, E);
typetest_clause(float, V, E, _Env) ->
    typetest_clause_1(is_float, V, E);
typetest_clause(cons, _V, E, Env) ->
    [V1, V2] = new_vars(2, Env),
    cerl:c_clause([cerl:c_cons(V1, V2)], E);  % there is no 'is cons'
typetest_clause(tuple, V, E, _Env) ->
    typetest_clause_1(is_tuple, V, E);
typetest_clause(binary, V, E, _Env) ->
    typetest_clause_1(is_binary, V, E).

typetest_clause_1(T, V, E) ->
    cerl:c_clause([V], cerl:c_call(cerl:c_atom('erlang'),
				   cerl:c_atom(T), [V]), E).

%% This returns a constructor descriptor, to be used for grouping and
%% pattern generation. It consists of an identifier term and the arity.

con_desc(E) ->
    case cerl:type(E) of
	cons -> {?cons_id, 2};
	tuple -> {?tuple_id, cerl:tuple_arity(E)};
	binary -> {?binary_id, cerl:binary_segments(E)};
	literal ->
	    case cerl:concrete(E) of
		[_|_] -> {?cons_id, 2};
		T when is_tuple(T) -> {?tuple_id, tuple_size(T)};
		V -> {?literal_id(V), 0}
	    end;
	_ ->
	    throw({bad_constructor, E})
    end.

%% This returns the type class for a constructor descriptor, for 
%% grouping of clauses. It does not distinguish between tuples of
%% different arity, nor between different values of atoms, integers and
%% floats.

con_desc_type({?literal_id([]), _}) -> [];
con_desc_type({?literal_id(V), _}) when is_atom(V) -> atom;
con_desc_type({?literal_id(V), _}) when is_integer(V) -> integer;
con_desc_type({?literal_id(V), _}) when is_float(V) -> float;
con_desc_type({?cons_id, 2}) -> cons;
con_desc_type({?tuple_id, _}) -> tuple;
con_desc_type({?binary_id, _}) -> binary.

%% This creates a new constructor pattern from a type descriptor and a
%% list of variables.

make_pat(?cons_id, [V1, V2]) -> cerl:c_cons(V1, V2);
make_pat(?tuple_id, Vs) -> cerl:c_tuple(Vs);
make_pat(?binary_id, Segs) -> cerl:c_binary(Segs);
make_pat(?literal_id(Val), []) -> cerl:abstract(Val).

%% This returns the list of subpatterns of a constructor pattern.

sub_pats(E) ->
    case cerl:type(E) of
	cons ->
	    [cerl:cons_hd(E), cerl:cons_tl(E)];
	tuple ->
	    cerl:tuple_es(E);
	binary ->
	    [];
	literal ->
	    case cerl:concrete(E) of
		[H|T] -> [cerl:abstract(H), cerl:abstract(T)];
		T when is_tuple(T) -> [cerl:abstract(X)
				       || X <- tuple_to_list(T)];
		_ -> []
	    end;
	_ ->
	    throw({bad_constructor_pattern, E})
    end.

%% This avoids generating stupid things like "let X = ... in 'true'",
%% and "let X = Y in X", keeping the generated code cleaner. It also
%% prevents expressions from being considered "non-lightweight" when
%% code duplication is disallowed (see is_lightweight for details).

make_let(Vs, A, B) ->
    cerl_lib:reduce_expr(cerl:c_let(Vs, A, B)).

%% ---------------------------------------------------------------------
%% Rewriting a module or other expression:

%% @spec expr(Expression::cerl(), Env) -> cerl()
%%    Env = rec_env:environment()
%%
%% @doc Rewrites all <code>case</code>-clauses in
%% <code>Expression</code>. <code>receive</code>-clauses are not
%% affected.
%%
%% <p>The environment is used for generating new variables which do not
%% shadow existing bindings.</p>
%% 
%% @see clauses/2
%% @see rec_env

-ifndef(NO_UNUSED).
-spec expr(cerl:cerl(), rec_env:environment()) -> cerl:cerl().

expr(E, Env) ->
    case cerl:type(E) of
        binary ->
            Es = expr_list(cerl:binary_segments(E), Env),
            cerl:update_c_binary(E, Es);
        bitstr ->
            V = expr(cerl:bitstr_val(E), Env),
            Sz = expr(cerl:bitstr_size(E), Env),
            Unit = expr(cerl:bitstr_unit(E), Env),
            Type = expr(cerl:bitstr_type(E), Env),
            cerl:update_c_bitstr(E, V, Sz, Unit, Type, cerl:bitstr_flags(E));
 	literal ->
	    E;
	var ->
	    E;
	values ->
	    Es = expr_list(cerl:values_es(E), Env),
 	    cerl:update_c_values(E, Es);
	cons ->
	    H = expr(cerl:cons_hd(E), Env),
	    T = expr(cerl:cons_tl(E), Env),
	    cerl:update_c_cons(E, H, T);
 	tuple ->
	    Es = expr_list(cerl:tuple_es(E), Env),
	    cerl:update_c_tuple(E, Es);
 	'let' ->
	    A = expr(cerl:let_arg(E), Env),
	    Vs = cerl:let_vars(E),
	    Env1 = add_vars(Vs, Env),
	    B = expr(cerl:let_body(E), Env1),
	    cerl:update_c_let(E, Vs, A, B);
	seq ->
	    A = expr(cerl:seq_arg(E), Env),
	    B = expr(cerl:seq_body(E), Env),
 	    cerl:update_c_seq(E, A, B);
 	apply ->
	    Op = expr(cerl:apply_op(E), Env),
	    As = expr_list(cerl:apply_args(E), Env),
 	    cerl:update_c_apply(E, Op, As);
 	call ->
	    M = expr(cerl:call_module(E), Env),
	    N = expr(cerl:call_name(E), Env),
	    As = expr_list(cerl:call_args(E), Env),
 	    cerl:update_c_call(E, M, N, As);
 	primop ->
	    As = expr_list(cerl:primop_args(E), Env),
	    cerl:update_c_primop(E, cerl:primop_name(E), As);
 	'case' ->
	    A = expr(cerl:case_arg(E), Env),
	    Cs = expr_list(cerl:case_clauses(E), Env),
	    {E1, Vs} = clauses(Cs, Env),
 	    make_let(Vs, A, E1);
 	clause ->
	    Vs = cerl:clause_vars(E),
	    Env1 = add_vars(Vs, Env),
	    G = expr(cerl:clause_guard(E), Env1),
	    B = expr(cerl:clause_body(E), Env1),
	    cerl:update_c_clause(E, cerl:clause_pats(E), G, B);
 	'fun' ->
	    Vs = cerl:fun_vars(E),
	    Env1 = add_vars(Vs, Env),
	    B = expr(cerl:fun_body(E), Env1),
	    cerl:update_c_fun(E, Vs, B);
 	'receive' ->
	    %% NOTE: No pattern matching compilation is done here! The
	    %% receive-clauses and patterns cannot be staged as long as
	    %% we are working with "normal" Core Erlang.
	    Cs = expr_list(cerl:receive_clauses(E), Env),
	    T = expr(cerl:receive_timeout(E), Env),
	    A = expr(cerl:receive_action(E), Env),
	    cerl:update_c_receive(E, Cs, T, A);
	'try' ->
	    A = expr(cerl:try_arg(E), Env),
	    Vs = cerl:try_vars(E),
	    B = expr(cerl:try_body(E), add_vars(Vs, Env)),
	    Evs = cerl:try_evars(E),
	    H = expr(cerl:try_handler(E), add_vars(Evs, Env)),
	    cerl:update_c_try(E, A, Vs, B, Evs, H);
 	'catch' ->
	    B = expr(cerl:catch_body(E), Env),
	    cerl:update_c_catch(E, B);
	letrec ->
	    Ds = cerl:letrec_defs(E),
	    Env1 = add_defs(Ds, Env),
	    Ds1 = defs(Ds, Env1),
	    B = expr(cerl:letrec_body(E), Env1),
	    cerl:update_c_letrec(E, Ds1, B);
	module ->
	    Ds = cerl:module_defs(E),
	    Env1 = add_defs(Ds, Env),
	    Ds1 = defs(Ds, Env1),
	    cerl:update_c_module(E, cerl:module_name(E),
				 cerl:module_exports(E),
				 cerl:module_attrs(E), Ds1)
    end.

expr_list(Es, Env) ->
    [expr(E, Env) || E <- Es].

defs(Ds, Env) ->
    [{V, expr(F, Env)} || {V, F} <- Ds].
-endif.	% NO_UNUSED
%% @clear

%% ---------------------------------------------------------------------
%%	Support functions

new_var(Env) ->
    Name = env__new_vname(Env),
    cerl:c_var(Name).

new_vars(N, Env) ->
    [cerl:c_var(V) || V <- env__new_vnames(N, Env)].

new_fvar(A, N, Env) ->
    Name = env__new_fname(A, N, Env),
    cerl:c_var(Name).

add_vars(Vs, Env) ->
    foldl(fun (V, E) -> env__bind(cerl:var_name(V), [], E) end, Env, Vs).

-ifndef(NO_UNUSED).
add_defs(Ds, Env) ->
    foldl(fun ({V, _F}, E) ->
		  env__bind(cerl:var_name(V), [], E)
	  end, Env, Ds).
-endif.	% NO_UNUSED

%% This decides whether an expression is worth lifting out to a separate
%% function instead of duplicating the code. In other words, whether its
%% cost is about the same or smaller than that of a local function call.
%% Note that variables must always be "lightweight"; otherwise, they may
%% get lifted out of the case switch that introduces them.

is_lightweight(E) ->
    case get('cerl_pmatch_duplicate_code') of
	never -> cerl:type(E) =:= var;    % Avoids all code duplication
	always -> true;    % Does not lift code to new functions
	_ -> is_lightweight_1(E)
    end.

is_lightweight_1(E) ->
    case cerl:type(E) of
	var -> true;
   	literal -> true;
   	'fun' -> true;
   	values -> all(fun is_simple/1, cerl:values_es(E));
   	cons -> is_simple(cerl:cons_hd(E))
   		    andalso is_simple(cerl:cons_tl(E));
   	tuple -> all(fun is_simple/1, cerl:tuple_es(E));
   	'let' -> (is_simple(cerl:let_arg(E)) andalso
   		  is_lightweight_1(cerl:let_body(E)));
   	seq -> (is_simple(cerl:seq_arg(E)) andalso
   		is_lightweight_1(cerl:seq_body(E)));
   	primop ->
   	    all(fun is_simple/1, cerl:primop_args(E));
   	apply ->
   	    is_simple(cerl:apply_op(E))
   		andalso all(fun is_simple/1, cerl:apply_args(E));
   	call ->
   	    is_simple(cerl:call_module(E))
   		andalso is_simple(cerl:call_name(E))
   		andalso all(fun is_simple/1, cerl:call_args(E));    
   	_ ->
	    %% The default is to lift the code to a new function.
	    false
    end.

%% "Simple" things have no (or negligible) runtime cost and are free
%% from side effects.

is_simple(E) ->
    case cerl:type(E) of
	var -> true;
	literal -> true;
	values -> all(fun is_simple/1, cerl:values_es(E));
	_ -> false
    end.


%% ---------------------------------------------------------------------
%% Abstract datatype: environment()

env__bind(Key, Val, Env) ->
    rec_env:bind(Key, Val, Env).

-ifndef(NO_UNUSED).
%% env__bind_recursive(Ks, Vs, F, Env) ->
%%     rec_env:bind_recursive(Ks, Vs, F, Env).

%% env__lookup(Key, Env) ->
%%     rec_env:lookup(Key, Env).

%% env__get(Key, Env) ->
%%     rec_env:get(Key, Env).

%% env__is_defined(Key, Env) ->
%%     rec_env:is_defined(Key, Env).

env__empty() ->
    rec_env:empty().
-endif.	% NO_UNUSED

env__new_vname(Env) ->
    rec_env:new_key(Env).

env__new_vnames(N, Env) ->
    rec_env:new_keys(N, Env).

env__new_fname(F, A, Env) ->
    rec_env:new_key(fun (X) ->
			    S = integer_to_list(X),
			    {list_to_atom(F ++ S), A}
		    end,
		    Env).
