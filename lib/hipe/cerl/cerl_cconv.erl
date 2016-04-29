%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2015. All Rights Reserved.
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
%% @copyright 2000-2004 Richard Carlsson
%% @doc Closure conversion of Core Erlang modules. This is done as a
%% step in the translation from Core Erlang down to HiPE Icode, and is
%% very much tied to the calling conventions used in HiPE native code.
%% @see cerl_to_icode

%% Some information about function closures in Beam and HiPE:
%%
%% - In Beam, each fun-expression is lifted to a top-level function such
%%   that the arity of the new function is equal to the arity of the fun
%%   *plus* the number of free variables. The original fun-expression is
%%   replaced by a call to 'make_fun' which takes the *label* of the new
%%   function and the number of free variables as arguments (the arity
%%   of the fun can be found via the label). When a call is made through
%%   the closure, the free variables are extracted from the closure by
%%   the 'call_fun' operation and are placed in the X registers
%%   following the ones used for the normal parameters; then the call is
%%   made to the function label.
%%
%% - In HiPE (when compiling from Beam bytecode), the Beam-to-Icode
%%   translation rewrites the fun-functions (those referenced by
%%   'make_fun' operations) so that the code expects only the normal
%%   parameters, plus *one* extra parameter containing the closure
%%   itself, and then immediately extracts the free variables from the
%%   closure - the code knows how many free variables it expects.
%%   However, the arity part of the function name is *not* changed;
%%   thus, the native code and the Beam code still use the same
%%   fun-table entry. The arity value used in native-code 'make_fun'
%%   operations should therefore be the same as in Beam, i.e., the sum
%%   of the number of parameters and the number of free variables.

-module(cerl_cconv).

-export([transform/2]).
-export([core_transform/2]).

-include("cerl_hipe_primops.hrl").

%% A descriptor for top-level and letrec-bound functions. (Top-level
%% functions always have an empty list of free variables.) The 'name'
%% field is the name of the lifted function, and is thus unique over the
%% whole module.

-record(function, {name :: {atom(), arity()}, free}).

%% A record for holding fun-information (if such information is attached
%% as an annotation on a fun, it should preferably be preserved).

-record(fun_info, {name     :: atom(),
		   id   = 0 :: integer(),
		   hash = 0 :: integer()}).

%% @spec core_transform(Module::cerl_records(), Options::[term()]) ->
%%           cerl_records()
%%
%% @doc Transforms a module represented by records. See
%% <code>transform/2</code> for details.
%%
%% <p>Use the compiler option <code>{core_transform, cerl_cconv}</code>
%% to insert this function as a compilation pass.</p>
%%
%% @see transform/2

-spec core_transform(cerl:cerl(), [term()]) -> cerl:cerl().

core_transform(M, Opts) ->
    cerl:to_records(transform(cerl:from_records(M), Opts)).


%% @spec transform(Module::cerl(), Options::[term()]) -> cerl()
%%
%%    cerl() = cerl:cerl()
%%
%% @doc Rewrites a Core Erlang module so that all fun-expressions
%% (lambda expressions) in the code are in top level function
%% definitions, and the operators of all `apply'-expressions are names
%% of such top-level functions. The primitive operations `make_fun' and
%% `call_fun' are inserted in the code to create and apply functional
%% values; this transformation is known as "Closure Conversion"
%%
%% <p>See the module {@link cerl_to_icode} for details.</p>

-spec transform(cerl:c_module(), [term()]) -> cerl:c_module().

transform(E, _Options) ->
    M = cerl:module_name(E),
    S0 = s__new(cerl:atom_val(M)),
    {Defs1, S1} = module_defs(cerl:module_defs(E), env__new(),
			      ren__new(), S0),
    Defs2 = lists:reverse(s__get_defs(S1) ++ Defs1),
    cerl:update_c_module(E, M, cerl:module_exports(E),
			 cerl:module_attrs(E), Defs2).

%% Note that the environment is defined on the renamed variables.

expr(E, Env, Ren, S0) ->
    case cerl:type(E) of
 	literal ->
	    {E, S0};
	var ->
	    var(E, Env, Ren, S0);
	values ->
	    {Es, S1} = expr_list(cerl:values_es(E), Env, Ren, S0),
 	    {cerl:update_c_values(E, Es), S1};
	cons ->
	    {E1, S1} = expr(cerl:cons_hd(E), Env, Ren, S0),
	    {E2, S2} = expr(cerl:cons_tl(E), Env, Ren, S1),
	    {cerl:update_c_cons(E, E1, E2), S2};
 	tuple ->
	    {Es, S1} = expr_list(cerl:tuple_es(E), Env, Ren, S0),
	    {cerl:update_c_tuple(E, Es), S1};
 	'let' ->
	    {A, S1} = expr(cerl:let_arg(E), Env, Ren, S0),
	    Vs = cerl:let_vars(E),
	    {Vs1, Env1, Ren1} = bind_vars(Vs, Env, Ren),
	    {B, S2} = expr(cerl:let_body(E), Env1, Ren1, S1),
	    {cerl:update_c_let(E, Vs1, A, B), S2};
	seq ->
	    {A, S1} = expr(cerl:seq_arg(E), Env, Ren, S0),
	    {B, S2} = expr(cerl:seq_body(E), Env, Ren, S1),
 	    {cerl:update_c_seq(E, A, B), S2};
 	apply ->
	    apply_expr(E, Env, Ren, S0);
 	call ->
	    {M, S1} = expr(cerl:call_module(E), Env, Ren, S0),
	    {N, S2} = expr(cerl:call_name(E), Env, Ren, S1),
	    {As, S3} = expr_list(cerl:call_args(E), Env, Ren, S2),
 	    {cerl:update_c_call(E, M, N, As), S3};
 	primop ->
	    {As, S1} = expr_list(cerl:primop_args(E), Env, Ren, S0),
	    N = cerl:primop_name(E),
	    {cerl:update_c_primop(E, N, As), S1};
 	'case' ->
	    {A, S1} = expr(cerl:case_arg(E), Env, Ren, S0),
	    {Cs, S2} = expr_list(cerl:case_clauses(E), Env, Ren, S1),
 	    {cerl:update_c_case(E, A, Cs), S2};
 	clause ->
	    Vs = cerl:clause_vars(E),
	    {_, Env1, Ren1} = bind_vars(Vs, Env, Ren),
	    %% Visit patterns to rename variables.
	    Ps = pattern_list(cerl:clause_pats(E), Env1, Ren1),
	    {G, S1} = expr(cerl:clause_guard(E), Env1, Ren1, S0),
	    {B, S2} = expr(cerl:clause_body(E), Env1, Ren1, S1),
	    {cerl:update_c_clause(E, Ps, G, B), S2};
 	'fun' ->
	    fun_expr(E, Env, Ren, S0);
 	'receive' ->
	    {Cs, S1} = expr_list(cerl:receive_clauses(E), Env, Ren, S0),
	    {T, S2} = expr(cerl:receive_timeout(E), Env, Ren, S1),
	    {A, S3} = expr(cerl:receive_action(E), Env, Ren, S2),
	    {cerl:update_c_receive(E, Cs, T, A), S3};
 	'try' ->
	    {A, S1} = expr(cerl:try_arg(E), Env, Ren, S0),
	    Vs = cerl:try_vars(E),
	    {Vs1, Env1, Ren1} = bind_vars(Vs, Env, Ren),
	    {B, S2} = expr(cerl:try_body(E), Env1, Ren1, S1),
	    Evs = cerl:try_evars(E),
	    {Evs1, Env2, Ren2} = bind_vars(Evs, Env, Ren),
	    {H, S3} = expr(cerl:try_handler(E), Env2, Ren2, S2),
	    {cerl:update_c_try(E, A, Vs1, B, Evs1, H), S3};
 	'catch' ->
	    {B, S1} = expr(cerl:catch_body(E), Env, Ren, S0),
	    {cerl:update_c_catch(E, B), S1};
	letrec ->
	    {Env1, Ren1, S1} = letrec_defs(cerl:letrec_defs(E), Env,
					   Ren, S0),
	    expr(cerl:letrec_body(E), Env1, Ren1, S1);
        binary ->
	    {Segs, S1} = expr_list(cerl:binary_segments(E), Env, Ren, S0),
	    {cerl:update_c_binary(E, Segs),S1};
	bitstr ->
	    {E1,S1} = expr(cerl:bitstr_val(E), Env, Ren, S0),
	    {E2,S2} = expr(cerl:bitstr_size(E), Env, Ren, S1),	
	    E3 = cerl:bitstr_unit(E), 
	    E4 = cerl:bitstr_type(E),
	    E5 = cerl:bitstr_flags(E),
	    {cerl:update_c_bitstr(E, E1, E2, E3, E4, E5), S2}
    end.

expr_list([E | Es], Env, Ren, S0) ->
    {E1, S1} = expr(E, Env, Ren, S0),
    {Es1, S2} = expr_list(Es, Env, Ren, S1),
    {[E1 | Es1], S2};
expr_list([], _, _, S) ->
    {[], S}.

pattern(E, Env, Ren) ->
    case cerl:type(E) of
 	literal ->
	    E;
	var ->
	    cerl:update_c_var(E, ren__map(cerl:var_name(E), Ren));
	values ->
	    Es = pattern_list(cerl:values_es(E), Env, Ren),
 	    cerl:update_c_values(E, Es);
	cons ->
	    E1 = pattern(cerl:cons_hd(E), Env, Ren),
	    E2 = pattern(cerl:cons_tl(E), Env, Ren),
	    cerl:update_c_cons(E, E1, E2);
 	tuple ->
	    Es = pattern_list(cerl:tuple_es(E), Env, Ren),
	    cerl:update_c_tuple(E, Es);
	binary ->
	    Es = pattern_list(cerl:binary_segments(E), Env, Ren),
	    cerl:update_c_binary(E, Es);
	bitstr ->
	    E1 = pattern(cerl:bitstr_val(E), Env, Ren),
	    E2 = pattern(cerl:bitstr_size(E), Env, Ren),	
	    E3 = cerl:bitstr_unit(E), 
	    E4 = cerl:bitstr_type(E),
	    E5 = cerl:bitstr_flags(E),
	    cerl:update_c_bitstr(E, E1, E2, E3, E4, E5);
	alias ->
	    V = pattern(cerl:alias_var(E), Env, Ren),
	    P = pattern(cerl:alias_pat(E), Env, Ren),
	    cerl:update_c_alias(E, V, P)
    end.

pattern_list([E | Es], Env, Ren) ->
    [pattern(E, Env, Ren) | pattern_list(Es, Env, Ren)];
pattern_list([], _, _) ->
    [].

%% First we set up the environment, binding the function names to the
%% corresponding descriptors. (For the top level functions, we don't
%% want to cause renaming.) After that, we can visit each function body
%% and return the new function definitions and the final state.

module_defs(Ds, Env, Ren, S) ->
    {Env1, S1} = bind_module_defs(Ds, Env, S),
    module_defs_1(Ds, [], Env1, Ren, S1).

bind_module_defs([{V, _F} | Ds], Env, S) ->
    Name = cerl:var_name(V),
    check_function_name(Name, S),
    S1 = s__add_function_name(Name, S),
    Info = #function{name = Name, free = []},
    Env1 = env__bind(Name, Info, Env),
    bind_module_defs(Ds, Env1, S1);
bind_module_defs([], Env, S) ->
    {Env, S}.

%% Checking that top-level function names are not reused

check_function_name(Name, S) ->
    case s__is_function_name(Name, S) of
	true ->
	    error_msg("multiple definitions of function `~w'.", [Name]),
	    exit(error);
	false ->
	    ok
    end.

%% We must track which top-level function we are in, for name generation
%% purposes.

module_defs_1([{V, F} | Ds], Ds1, Env, Ren, S) ->
    S1 = s__enter_function(cerl:var_name(V), S),
    %% The parameters should never need renaming, but this is easiest.
    {Vs, Env1, Ren1} = bind_vars(cerl:fun_vars(F), Env, Ren),
    {B, S2} = expr(cerl:fun_body(F), Env1, Ren1, S1),
    F1 = cerl:update_c_fun(F, Vs, B),
    module_defs_1(Ds, [{V, F1} | Ds1], Env, Ren, S2);
module_defs_1([], Ds, _, _, S) ->
    {Ds, S}.

%% First we must create the new function names and set up the
%% environment with descriptors for the letrec-bound functions.
%%
%% Since we never shadow variables, the free variables of any
%% letrec-bound fun can always be referenced directly wherever the
%% fun-variable itself is referenced - this is important when we create
%% direct calls to lifted letrec-bound functions, and is the main reason
%% why we do renaming. For example:
%%
%%   'f'/0 = fun () ->
%%             let X = 42 in
%%               letrec 'g'/1 = fun (Y) -> {X, Y} in
%%                 let X = 17 in
%%                   apply 'g'/1(X)
%%
%% will become something like
%%
%%   'f'/0 = fun () ->
%%             let X = 42 in
%%               let X1 = 17 in
%%                 apply 'g'/2(X1, X)
%%   'g'/2 = fun (Y, X) -> {X, Y}
%%
%% where the innermost X has been renamed so that the outermost X can be
%% referenced in the call to the lifted function 'g'/2. (Renaming must
%% of course also be applied also to letrec-bound function variables.)
%%
%% Furthermore, if some variable X occurs free in a fun 'f'/N, and 'f'/N
%% it its turn occurs free in a fun 'g'/M, then we transitively count X
%% as free in 'g'/M, even if it has no occurrence there. This allows us
%% to rewrite code such as the following:
%%
%%   'f'/0 = fun () ->
%%             let X = 42 in
%%               letrec 'g'/1 = fun (Y) -> {X, Y}
%%                      'h'/1 = fun (Z) -> {'bar', apply 'g'/1(Z)}
%%               in let X = 17 in
%%                    apply 'h'/1(X)
%%
%% into something like:
%%
%%   'f'/0 = fun () ->
%%             let X = 42 in
%%               let X1 = 17 in
%%                 apply 'h'/2(X1, X)
%%   'g'/2 = fun (Y, X) -> {X, Y}
%%   'h'/2 = fun (Z, X) -> {'bar', apply 'g'/2(Z, X)}
%%
%% which uses only direct calls. The drawback is that if the occurrence
%% of 'f'/N in 'g'/M instead would cause a closure to be created, then
%% that closure could have been formed earlier (at the point where 'f'/N
%% was defined), rather than passing on all the free variables of 'f'/N
%% into 'g'/M. Since we must know the interface to 'g'/M (i.e., the
%% total number of parameters) before we begin processing its body, and
%% the interface depends on what we do to the body (and functions can be
%% mutually recursive), this problem can only be solved by finding out
%% _what_ we are going to do before we can even define the interfaces of
%% the functions, by looking at _how_ variables are being referenced
%% when we look for free variables. Currently, we don't do that.

letrec_defs(Ds, Env, Ren, S) ->
    {Env1, Ren1, S1} = bind_letrec_defs(Ds, Env, Ren, S),
    {Env1, Ren1, lift_letrec_defs(Ds, Env1, Ren1, S1)}.

%% Note: it is important that we store the *renamed* free variables for
%% each function to be lifted.

bind_letrec_defs(Ds, Env, Ren, S) ->
    bind_letrec_defs(Ds, free_in_defs(Ds, Env, Ren), Env, Ren, S).

bind_letrec_defs([{V, _F} | Ds], Free, Env, Ren, S) ->
    Name = cerl:var_name(V),
    {Env1, Ren1, S1} = bind_letrec_fun(Name, Free, Env, Ren, S),
    bind_letrec_defs(Ds, Free, Env1, Ren1, S1);
bind_letrec_defs([], _Free, Env, Ren, S) ->
    {Env, Ren, S}.

bind_letrec_fun(Name = {_,A}, Free, Env, Ren, S) ->
    A1 = A + length(Free),
    {Name1, Ren1, S1} = rename_letrec_fun(Name, A1, Env, Ren, S),
    Info = #function{name = Name1, free = Free},
    {env__bind(Name1, Info, Env), Ren1, S1}.

%% Creating a new name for the lifted function that is informative, is
%% not in the environment, and is not already used for some other lifted
%% function.

rename_letrec_fun(Name, NewArity, Env, Ren, S) ->
    {New, S1} = new_letrec_fun_name(Name, NewArity, Env, S),
    {New, ren__add(Name, New, Ren), s__add_function_name(New, S1)}.

new_letrec_fun_name({N,_}, Arity, Env, S) ->
    {FName, FArity} = s__get_function(S),
    Base = fun_name_base(FName, FArity)
	++ "-letrec-" ++ atom_to_list(N) ++ "-",
    %% We try the base as name first. This will usually work.
    Name = {list_to_atom(Base), Arity},
    case env__is_defined(Name, Env) of
	true ->
	    new_fun_name(Base, Arity, Env, S);
	false ->
	    case s__is_function_name(Name, S) of
		true ->
		    new_fun_name(Base, Arity, Env, S);
		false ->
		    {Name, S}
	    end
    end.

%% Processing the actual functions of a letrec

lift_letrec_defs([{V, F} | Ds], Env, Ren, S) ->
    Info = env__get(ren__map(cerl:var_name(V), Ren), Env),
    S1 = lift_letrec_fun(F, Info, Env, Ren, S),
    lift_letrec_defs(Ds, Env, Ren, S1);
lift_letrec_defs([], _, _, S) ->
    S.

%% The direct calling convention for letrec-defined functions is to pass
%% the free variables as additional parameters. Note that the free
%% variables (if any) are already in the environment when we get here.
%% We only have to append them to the parameter list so that they are in
%% scope in the lifted function; they are already renamed.
%%
%% It should not be possible for the original parameters to clash with
%% the free ones (in that case they cannot be free), but we do the full
%% bind-and-rename anyway, since it's easiest.

lift_letrec_fun(F, Info, Env, Ren, S) ->
    {Vs, Env1, Ren1} = bind_vars(cerl:fun_vars(F), Env, Ren),
    {B, S1} = expr(cerl:fun_body(F), Env1, Ren1, S),
    Fs = [cerl:c_var(V) || V <- Info#function.free],
    F1 = cerl:c_fun(Vs ++ Fs, B),
    s__add_def(cerl:c_var(Info#function.name), F1, S1).

%% This is a simple way of handling mutual recursion in a group of
%% letrec-definitions: classify a variable as free in all the functions
%% if it is free in any of them. (The preferred way would be to actually
%% take the transitive closure for each function.)

free_in_defs(Ds, Env, Ren) ->
    {Vs, Fs} = free_in_defs(Ds, [], [], Ren),
    closure_vars(ordsets:subtract(Fs, Vs), Env, Ren).

free_in_defs([{V, F} | Ds], Vs, Free, Ren) ->
    Fs = cerl_trees:free_variables(F),
    free_in_defs(Ds, [ren__map(cerl:var_name(V), Ren) | Vs], Fs ++ Free,
		 Ren);
free_in_defs([], Vs, Free, _Ren) ->
    {ordsets:from_list(Vs), ordsets:from_list(Free)}.

%% Replacing function variables with the free variables of the function

closure_vars(Vs, Env, Ren) ->
    closure_vars(Vs, [], Env, Ren).

closure_vars([V = {_, _} | Vs], As, Env, Ren) ->
    V1 = ren__map(V, Ren),
    case env__lookup(V1, Env) of
	{ok, #function{free = Vs1}} ->
	    closure_vars(Vs, Vs1 ++ As, Env, Ren);
	_ ->
	    closure_vars(Vs, As, Env, Ren)
    end;
closure_vars([V | Vs], As, Env, Ren) ->
    closure_vars(Vs, [V | As], Env, Ren);
closure_vars([], As, _Env, _Ren) ->
    ordsets:from_list(As).

%% We use the no-shadowing strategy, renaming variables on the fly and
%% only when necessary to uphold the invariant.

bind_vars(Vs, Env, Ren) -> 
    bind_vars(Vs, [], Env, Ren).

bind_vars([V | Vs], Vs1, Env, Ren) ->
    Name = cerl:var_name(V),
    {Name1, Ren1} = rename_var(Name, Env, Ren),
    bind_vars(Vs, [cerl:update_c_var(V, Name1) | Vs1],
	      env__bind(Name1, variable, Env), Ren1);
bind_vars([], Vs, Env, Ren) ->
    {lists:reverse(Vs), Env, Ren}.

rename_var(Name, Env, Ren) ->
    case env__is_defined(Name, Env) of
	false ->
	    {Name, Ren};
	true ->
	    New = env__new_name(Env),
	    {New, ren__add(Name, New, Ren)}
    end.

%% This handles variable references *except* in function application
%% operator positions (see apply_expr/4).
%%
%% The Beam compiler annotates function-variable references with 'id'
%% info, eventually transforming a direct reference such as "fun f/2"
%% into a new fun-expression "fun (X1,X2) -> apply f/2(X1,X2)" for which
%% the info is used to create the lifted function as for any other fun.
%% We do the same thing for function-bound variables.

var(V, Env, Ren, S) ->
    Name = ren__map(cerl:var_name(V), Ren),
    case lookup_var(Name, Env) of
	#function{name = F, free = Vs} ->
	    {_, Arity} = F,
	    Vs1 = make_vars(Arity),
	    C = cerl:c_apply(cerl:c_var(F), Vs1),
	    E = cerl:ann_c_fun(cerl:get_ann(V), Vs1, C),
	    fun_expr_1(E, Vs, Env, Ren, S);
	variable ->
	    {cerl:update_c_var(V, Name), S}
    end.

lookup_var(V, Env) ->
    case env__lookup(V, Env) of
	{ok, X} ->
	    X;
	error ->
	    error_msg("unbound variable `~P'.", [V, 5]),
	    exit(error)
    end.

make_vars(N) when N > 0 ->
    [cerl:c_var(list_to_atom("X" ++ integer_to_list(N)))
     | make_vars(N - 1)];
make_vars(0) ->
    [].

%% All funs that are not bound by module or letrec definitions will be
%% rewritten to create explicit closures using "make fun". We don't
%% currently track ordinary let-bindings of funs, as in "let F = fun
%% ... in ...apply F(...)...".
%%
%% Note that we (currently) follow the Beam naming convention, including
%% the free variables in the arity of the name, even though the actual
%% function typically expects a different number of parameters.

fun_expr(F, Env, Ren, S) ->
    Free = closure_vars(cerl_trees:free_variables(F), Env, Ren),
    Vs = [cerl:c_var(V) || V <- Free],
    fun_expr_1(F, Vs, Env, Ren, S).
    
fun_expr_1(F, Vs, Env, Ren, S) ->
    Arity = cerl:fun_arity(F) + length(Vs),    % for the name only
    {Info, S1} = fun_info(F, Env, S),
    Name = {Info#fun_info.name, Arity},
    S2 = lift_fun(Name, F, Vs, Env, Ren, S1),
    {make_fun_primop(Name, Vs, Info, F, S2), S2}.

make_fun_primop({Name, Arity}, Free, #fun_info{id = Id, hash = Hash},
		F, S) ->
    Module = s__get_module_name(S),
    cerl:update_c_primop(F, cerl:c_atom(?PRIMOP_MAKE_FUN),
			 [cerl:c_atom(Module),
			  cerl:c_atom(Name),
			  cerl:c_int(Arity),
			  cerl:c_int(Hash),
			  cerl:c_int(Id),
			  cerl:make_list(Free)]).

%% Getting attached fun-info, if present; otherwise making it up.

fun_info(E, Env, S) ->
    case lists:keyfind(id, 1, cerl:get_ann(E)) of
	{id, {Id, H, Name}} ->
	    %% io:fwrite("Got fun-info: ~w: {~w,~w}.\n", [Name,Id,H]),
	    {#fun_info{name = Name, id = Id, hash = H}, S};
	_ ->
	    io:fwrite("Warning - fun not annotated: "
		      "making up new name.\n"), % for now
 	    {{Name,_Arity}, S1} = new_fun_name(E, Env, S),
 	    {#fun_info{name = Name, id = 0, hash = 0}, S1}
    end.

fun_name_base(FName, FArity) ->
    "-" ++ atom_to_list(FName) ++ "/" ++ integer_to_list(FArity).

%% Generate a name for the new function, using a the same convention
%% that is used by the Beam compiler.
new_fun_name(F, Env, S) ->
    {FName, FArity} = s__get_function(S),
    Base = fun_name_base(FName, FArity) ++ "-fun-",
    Arity = cerl:fun_arity(F),
    new_fun_name(Base, Arity, Env, S).

%% Creating a new function name that is not in the environment and is
%% not already used for some other lifted function.

new_fun_name(Base, Arity, Env, S) ->
    F = fun (N) ->
		{list_to_atom(Base ++ integer_to_list(N)), Arity}
	end,
    new_fun_name(Base, Arity, Env, S, F).

new_fun_name(Base, Arity, Env, S, F) ->
    %% Note that repeated calls to env__new_function_name/2 will yield
    %% different names even though Env and F are the same.
    Name = env__new_function_name(F, Env),
    case s__is_function_name(Name, S) of
	true ->
	    new_fun_name(Base, Arity, Env, S, F);
	false ->
	    {Name, S}
    end.

%% This lifts the fun to a new top-level function which uses the calling
%% convention for closures, with the closure itself as the final
%% parameter. Note that the free variables (if any) are already in the
%% environment.
%%
%% It should not be possible for the original parameters to clash with
%% the free ones (in that case they cannot be free), but we do the full
%% bind-and-rename anyway, since it's easiest.

lift_fun(Name, F, Free, Env, Ren, S) ->
    %% If the name is already in the list of top-level definitions, we
    %% assume we have already generated this function, and do not need
    %% to do it again (typically, this happens for 'fun f/n'-variables
    %% that have been duplicated before being rewritten to actual
    %% fun-expressions, and the name is taken from their annotations).
    %% Otherwise, we add the name to the list.
    case s__is_function_name(Name, S) of
	true ->
	    S;
	false ->
	    S1 = s__add_function_name(Name, S),
	    lift_fun_1(Name, F, Free, Env, Ren, S1)
    end.

lift_fun_1(Name, F, Free, Env, Ren, S) ->
    %% (The original parameters must be added to the environment before
    %% we generate the new variable for the closure parameter.)
    {Vs, Env1, Ren1} = bind_vars(cerl:fun_vars(F), Env, Ren),
    V = env__new_name(Env1),
    Env2 = env__bind(V, variable, Env1),
    {B, S1} = expr(cerl:fun_body(F), Env2, Ren1, S),
    %% We unpack all free variables from the closure upon entering.
    %% (Adding this to the body before we process it would introduce
    %% unnecessary, although harmless, renaming of the free variables.)
    Es = closure_elements(length(Free), cerl:c_var(V)),
    B1 = cerl:c_let(Free, cerl:c_values(Es), B),
    %% The closure itself is passed as the last argument. The new
    %% function is annotated as being a closure-call entry point.
    E = cerl:ann_c_fun([closure, {closure_orig_arity, cerl:fun_arity(F)}], Vs ++ [cerl:c_var(V)], B1),
    s__add_def(cerl:c_var(Name), E, S1).

closure_elements(N, V) ->
    closure_elements(N, N + 1, V).

closure_elements(0, _, _) -> [];
closure_elements(N, M, V) ->
    [cerl:c_primop(cerl:c_atom(?PRIMOP_FUN_ELEMENT),
		   [cerl:c_int(M - N), V])
     | closure_elements(N - 1, M, V)].


%% Function applications must be rewritten depending on the
%% operator. For a call to a known top-level function or letrec-bound
%% function, we make a direct call, passing the free variables as extra
%% parameters (we know they are in scope, since variables may not be
%% shadowed). Otherwise, we create an "apply fun" primop call that
%% expects a closure.

apply_expr(E, Env, Ren, S) ->
    {As, S1} = expr_list(cerl:apply_args(E), Env, Ren, S),
    Op = cerl:apply_op(E),
    case cerl:is_c_var(Op) of
	true ->
	    Name = ren__map(cerl:var_name(Op), Ren),
	    case lookup_var(Name, Env) of
		#function{name = F, free = Vs} ->
		    Vs1 = As ++ [cerl:c_var(V) || V <- Vs],
		    {cerl:update_c_apply(E, cerl:c_var(F), Vs1), S1};
		variable ->
		    apply_expr_1(E, Op, As, Env, Ren, S1)
	    end;
	_ ->
	    apply_expr_1(E, Op, As, Env, Ren, S1)
    end.

%% Note that this primop call only communicates the necessary
%% information to the core-to-icode stage, which rewrites it to use the
%% real calling convention for funs.

apply_expr_1(E, Op, As, Env, Ren, S) ->
    {Op1, S1} = expr(Op, Env, Ren, S),
    Call = cerl:update_c_primop(E, cerl:c_atom(?PRIMOP_APPLY_FUN),
				[Op1, cerl:make_list(As)]),
    {Call, S1}.


%% ---------------------------------------------------------------------
%% Environment

env__new() ->
    rec_env:empty().

env__bind(Key, Value, Env) ->
    rec_env:bind(Key, Value, Env).

env__lookup(Key, Env) ->
    rec_env:lookup(Key, Env).

env__get(Key, Env) ->
    rec_env:get(Key, Env).

env__is_defined(Key, Env) ->
    rec_env:is_defined(Key, Env).

env__new_name(Env) ->
    rec_env:new_key(Env).

env__new_function_name(F, Env) ->
    rec_env:new_key(F, Env).


%% ---------------------------------------------------------------------
%% Renaming

ren__new() ->
    dict:new().

ren__add(Key, Value, Ren) ->
    dict:store(Key, Value, Ren).

ren__map(Key, Ren) ->
    case dict:find(Key, Ren) of
	{ok, Value} ->
	    Value;
	error ->
	    Key
    end.


%% ---------------------------------------------------------------------
%% State

-record(state, {module             :: module(),
		function           :: {atom(), arity()} | 'undefined',
		names = sets:new() :: sets:set(),  %% XXX: refine
		refs  = dict:new() :: dict:dict(), %% XXX: refine
		defs  = []}).

s__new(Module) ->
    #state{module = Module}.

s__add_function_name(Name, S) ->
    S#state{names = sets:add_element(Name, S#state.names)}.

s__is_function_name(Name, S) ->
    sets:is_element(Name, S#state.names).

s__get_module_name(S) ->
    S#state.module.

s__enter_function(F, S) ->
    S#state{function = F}.

s__get_function(S) ->
    S#state.function.

s__add_def(V, F, S) ->
    S#state{defs = [{V, F} | S#state.defs]}.

s__get_defs(S) ->
    S#state.defs.


%% ---------------------------------------------------------------------
%% Reporting

%% internal_error_msg(S) ->
%%     internal_error_msg(S, []).

%% internal_error_msg(S, Vs) ->
%%     error_msg(lists:concat(["Internal error: ", S]), Vs).

%% error_msg(S) ->
%%     error_msg(S, []).

error_msg(S, Vs) ->
    error_logger:error_msg(lists:concat([?MODULE, ": ", S, "\n"]), Vs).

%% warning_msg(S) ->
%%     warning_msg(S, []).

%% warning_msg(S, Vs) ->
%%     info_msg(lists:concat(["warning: ", S]), Vs).

%% info_msg(S) ->
%%     info_msg(S, []).

%% info_msg(S, Vs) ->
%%     error_logger:info_msg(lists:concat([?MODULE, ": ", S, "\n"]), Vs).
