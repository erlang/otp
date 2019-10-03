%% =====================================================================
%%
%%
%%
%%
%%
%%
%%
%%
%%
%%
%%
%%
%%
%%
%%
%% $Id: cerl_hipeify.erl,v 1.1 2008/12/17 09:53:49 mikpe Exp $
%%
%% @author Richard Carlsson <richardc@csd.uu.se>
%% @copyright 2000-2004 Richard Carlsson
%% @doc HiPE-ification of Core Erlang code. Prepares Core Erlang code
%% for translation to ICode.
%% @see cerl_to_icode

-module(cerl_hipeify).

-export([transform/2]).

-define(PRIMOP_IDENTITY, identity).		% arity 1
-define(PRIMOP_NOT, 'not').			% arity 1
-define(PRIMOP_AND, 'and').			% arity 2
-define(PRIMOP_OR, 'or').			% arity 2
-define(PRIMOP_XOR, 'xor').			% arity 2
-define(PRIMOP_ADD, '+').			% arity 2
-define(PRIMOP_SUB, '-').			% arity 2
-define(PRIMOP_NEG, neg).			% arity 1
-define(PRIMOP_MUL, '*').			% arity 2
-define(PRIMOP_DIV, '/').			% arity 2
-define(PRIMOP_INTDIV, 'div').			% arity 2
-define(PRIMOP_REM, 'rem').			% arity 2
-define(PRIMOP_BAND, 'band').			% arity 2
-define(PRIMOP_BOR, 'bor').			% arity 2
-define(PRIMOP_BXOR, 'bxor').			% arity 2
-define(PRIMOP_BNOT, 'bnot').			% arity 1
-define(PRIMOP_BSL, 'bsl').			% arity 2
-define(PRIMOP_BSR, 'bsr').			% arity 2
-define(PRIMOP_EQ, '==').			% arity 2
-define(PRIMOP_NE, '/=').			% arity 2
-define(PRIMOP_EXACT_EQ, '=:=').		% arity 2
-define(PRIMOP_EXACT_NE, '=/=').		% arity 2
-define(PRIMOP_LT, '<').			% arity 2
-define(PRIMOP_GT, '>').			% arity 2
-define(PRIMOP_LE, '=<').			% arity 2
-define(PRIMOP_GE, '>=').			% arity 2
-define(PRIMOP_IS_ATOM, 'is_atom').		% arity 1
-define(PRIMOP_IS_BIGNUM, 'is_bignum').		% arity 1
-define(PRIMOP_IS_BINARY, 'is_binary').		% arity 1
-define(PRIMOP_IS_CONSTANT, 'is_constant').	% arity 1
-define(PRIMOP_IS_FIXNUM, 'is_fixnum').		% arity 1
-define(PRIMOP_IS_FLOAT, 'is_float').		% arity 1
-define(PRIMOP_IS_FUNCTION, 'is_function').	% arity 1
-define(PRIMOP_IS_INTEGER, 'is_integer').	% arity 1
-define(PRIMOP_IS_LIST, 'is_list').		% arity 1
-define(PRIMOP_IS_NUMBER, 'is_number').		% arity 1
-define(PRIMOP_IS_PID, 'is_pid').		% arity 1
-define(PRIMOP_IS_PORT, 'is_port').		% arity 1
-define(PRIMOP_IS_REFERENCE, 'is_reference').	% arity 1
-define(PRIMOP_IS_TUPLE, 'is_tuple').		% arity 1
-define(PRIMOP_IS_RECORD, 'is_record').		% arity 3
-define(PRIMOP_EXIT, exit).			% arity 1
-define(PRIMOP_THROW, throw).			% arity 1
-define(PRIMOP_ERROR, error).			% arity 1,2
-define(PRIMOP_RETHROW, raise).			% arity 2
-define(PRIMOP_RECEIVE_SELECT, receive_select).	% arity 0
-define(PRIMOP_RECEIVE_NEXT, receive_next).	% arity 0
-define(PRIMOP_ELEMENT, element).		% arity 2
-define(PRIMOP_DSETELEMENT, dsetelement).	% arity 3
-define(PRIMOP_MAKE_FUN, make_fun).		% arity 6
-define(PRIMOP_APPLY_FUN, apply_fun).		% arity 2
-define(PRIMOP_FUN_ELEMENT, closure_element).	% arity 2
-define(PRIMOP_SET_LABEL, set_label).           % arity 1
-define(PRIMOP_GOTO_LABEL, goto_label).         % arity 1
-define(PRIMOP_REDUCTION_TEST, reduction_test). % arity 0

-record(ctxt, {class = expr}).


%% @spec transform(Module::cerl(), Options::[term()]) -> cerl()
%%
%%    cerl() = cerl:cerl()
%%
%% @doc Rewrites a Core Erlang module to a form suitable for further
%% translation to HiPE Icode. See module <code>cerl_to_icode</code> for
%% details.
%%
%% @see cerl_to_icode
%% @see cerl_cconv

transform(E, Opts) ->
    %% Start by closure converting the code
    module(cerl_cconv:transform(E, Opts), Opts).

module(E, Opts) ->
    {Ds, Env, Ren} = add_defs(cerl:module_defs(E), env__new(),
			      ren__new()),
    M = cerl:module_name(E),
    S0 = s__new(cerl:atom_val(M)),
    S = s__set_pmatch(proplists:get_value(pmatch, Opts), S0),
    {Ds1, _} = defs(Ds, true, Env, Ren, S),
    cerl:update_c_module(E, M, cerl:module_exports(E),
			 cerl:module_attrs(E), Ds1).

%% Note that the environment is defined on the renamed variables.

expr(E0, Env, Ren, Ctxt, S0) ->
    %% Do peephole optimizations as we traverse the code.
    E = cerl_lib:reduce_expr(E0),
    case cerl:type(E) of
	literal ->
	    {E, S0};
	var ->
	    variable(E, Env, Ren, Ctxt, S0);
	values ->
	    {Es, S1} = expr_list(cerl:values_es(E), Env, Ren, Ctxt, S0),
	    {cerl:update_c_values(E, Es), S1};
	cons ->
	    {E1, S1} = expr(cerl:cons_hd(E), Env, Ren, Ctxt, S0),
	    {E2, S2} = expr(cerl:cons_tl(E), Env, Ren, Ctxt, S1),
	    {cerl:update_c_cons(E, E1, E2), S2};
	tuple ->
	    {Es, S1} = expr_list(cerl:tuple_es(E), Env, Ren, Ctxt, S0),
	    {cerl:update_c_tuple(E, Es), S1};
	'let' ->
	    let_expr(E, Env, Ren, Ctxt, S0);
	seq ->
	    {A, S1} = expr(cerl:seq_arg(E), Env, Ren, Ctxt, S0),
	    {B, S2} = expr(cerl:seq_body(E), Env, Ren, Ctxt, S1),
	    {cerl:update_c_seq(E, A, B), S2};
	apply ->
	    {Op, S1} = expr(cerl:apply_op(E), Env, Ren, Ctxt, S0),
	    {As, S2} = expr_list(cerl:apply_args(E), Env, Ren, Ctxt, S1),
	    {cerl:update_c_apply(E, Op, As), S2};
	call ->
	    {M, S1} = expr(cerl:call_module(E), Env, Ren, Ctxt, S0),
	    {N, S2} = expr(cerl:call_name(E), Env, Ren, Ctxt, S1),
	    {As, S3} = expr_list(cerl:call_args(E), Env, Ren, Ctxt, S2),
	    {rewrite_call(E, M, N, As, S3), S3};
	primop ->
	    {As, S1} = expr_list(cerl:primop_args(E), Env, Ren, Ctxt, S0),
	    N = cerl:primop_name(E),
	    {rewrite_primop(E, N, As, S1), S1};
	'case' ->
	    {A, S1} = expr(cerl:case_arg(E), Env, Ren, Ctxt, S0),
	    {E1, Vs, S2} = clauses(cerl:case_clauses(E), Env, Ren, Ctxt, S1),
	    {cerl:c_let(Vs, A, E1), S2};
	'fun' ->
	    Vs = cerl:fun_vars(E),
	    {Vs1, Env1, Ren1} = add_vars(Vs, Env, Ren),
	    {B, S1} = expr(cerl:fun_body(E), Env1, Ren1, Ctxt, S0),
	    {cerl:update_c_fun(E, Vs1, B), S1};
	'receive' ->
	    receive_expr(E, Env, Ren, Ctxt, S0);
	'try' ->
	    {A, S1} = expr(cerl:try_arg(E), Env, Ren, Ctxt, S0),
	    Vs = cerl:try_vars(E),
	    {Vs1, Env1, Ren1} = add_vars(Vs, Env, Ren),
	    {B, S2} = expr(cerl:try_body(E), Env1, Ren1, Ctxt, S1),
	    Evs = cerl:try_evars(E),
	    {Evs1, Env2, Ren2} = add_vars(Evs, Env, Ren),
	    {H, S3} = expr(cerl:try_handler(E), Env2, Ren2, Ctxt, S2),
	    {cerl:update_c_try(E, A, Vs1, B, Evs1, H), S3};
	'catch' ->
	    catch_expr(E, Env, Ren, Ctxt, S0);
	letrec ->
	    {Ds, Env1, Ren1} = add_defs(cerl:letrec_defs(E), Env, Ren),
	    {Ds1, S1} = defs(Ds, false, Env1, Ren1, S0),
	    {B, S2} = expr(cerl:letrec_body(E), Env1, Ren1, Ctxt, S1),
	    {cerl:update_c_letrec(E, Ds1, B), S2};
	binary ->
	    {Segs, S1}=expr_list(cerl:binary_segments(E), Env, Ren,
				 Ctxt, S0),
	    {cerl:update_c_binary(E, Segs), S1};
	bitstr ->
	    {E1,S1} = expr(cerl:bitstr_val(E), Env, Ren, Ctxt, S0),
	    {E2,S2} = expr(cerl:bitstr_size(E), Env, Ren, Ctxt, S1),
	    E3 = cerl:bitstr_unit(E),
	    E4 = cerl:bitstr_type(E),
	    E5 = cerl:bitstr_flags(E),
	    {cerl:update_c_bitstr(E, E1, E2, E3, E4, E5), S2}
    end.

guard_expr(E, Env, Ren, Ctxt, S) ->
    expr(E, Env, Ren, Ctxt#ctxt{class = guard}, S).

expr_list(Es, Env, Ren, Ctxt, S0) ->
    list(Es, Env, Ren, Ctxt, S0, fun expr/5).

list([E | Es], Env, Ren, Ctxt, S0, F) ->
    {E1, S1} = F(E, Env, Ren, Ctxt, S0),
    {Es1, S2} = list(Es, Env, Ren, Ctxt, S1, F),
    {[E1 | Es1], S2};
list([], _, _, _, S, _) ->
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
	alias ->
	    V = pattern(cerl:alias_var(E), Env, Ren),
	    P = pattern(cerl:alias_pat(E), Env, Ren),
	    cerl:update_c_alias(E, V, P);
	binary ->
	    Segs=pattern_list(cerl:binary_segments(E), Env, Ren),
	    cerl:update_c_binary(E, Segs);
	bitstr ->
	    E1 = pattern(cerl:bitstr_val(E), Env, Ren),
	    E2 = pattern(cerl:bitstr_size(E), Env, Ren),
	    E3 = cerl:bitstr_unit(E),
	    E4 = cerl:bitstr_type(E),
	    E5 = cerl:bitstr_flags(E),
	    cerl:update_c_bitstr(E, E1, E2, E3, E4, E5)
    end.



pattern_list([E | Es], Env, Ren) ->
    [pattern(E, Env, Ren) | pattern_list(Es, Env, Ren)];
pattern_list([], _, _) ->
    [].

%% Visit the function body of each definition. We insert an explicit
%% reduction test at the start of each function.

defs(Ds, Top, Env, Ren, S) ->
    defs(Ds, [], Top, Env, Ren, S).

defs([{V, F} | Ds], Ds1, Top, Env, Ren, S0) ->
    S1 = case Top of
	     true -> s__enter_function(cerl:var_name(V), S0);
	     false -> S0
	 end,
    {B, S2} = expr(cerl:fun_body(F), Env, Ren, #ctxt{}, S1),
    B1 = cerl:c_seq(cerl:c_primop(cerl:c_atom(?PRIMOP_REDUCTION_TEST),
				  []),
		    B),
    F1 = cerl:update_c_fun(F, cerl:fun_vars(F), B1),
    defs(Ds, [{V, F1} | Ds1], Top, Env, Ren, S2);
defs([], Ds, _Top, _Env, _Ren, S) ->
    {lists:reverse(Ds), S}.

clauses([C|_]=Cs, Env, Ren, Ctxt, S) ->
    {Cs1, S1} = clause_list(Cs, Env, Ren, Ctxt, S),
    %% Perform pattern matching compilation on the clauses.
    {E, Vs} = case s__get_pmatch(S) of
		  true ->
		      cerl_pmatch:clauses(Cs1, Env);
		  no_duplicates ->
		      put('cerl_pmatch_duplicate_code', never),
		      cerl_pmatch:clauses(Cs1, Env);
		  duplicate_all ->
		      put('cerl_pmatch_duplicate_code', always),
		      cerl_pmatch:clauses(Cs1, Env);
		  Other when Other == false; Other == undefined ->
		      Vs0 = new_vars(cerl:clause_arity(C), Env),
		      {cerl:c_case(cerl:c_values(Vs0), Cs1), Vs0}
	      end,
    %% We must make sure that we also visit any clause guards generated
    %% by the pattern matching compilation. We pass an empty renaming,
    %% so we do not rename any variables twice.
    {E1, S2} = revisit_expr(E, Env, ren__new(), Ctxt, S1),
    {E1, Vs, S2}.

clause_list(Cs, Env, Ren, Ctxt, S) ->
    list(Cs, Env, Ren, Ctxt, S, fun clause/5).

clause(E, Env, Ren, Ctxt, S0) ->
    Vs = cerl:clause_vars(E),
    {_, Env1, Ren1} = add_vars(Vs, Env, Ren),
    %% Visit patterns to rename variables.
    Ps = pattern_list(cerl:clause_pats(E), Env1, Ren1),
    {G, S1} = guard_expr(cerl:clause_guard(E), Env1, Ren1, Ctxt, S0),
    {B, S2} = expr(cerl:clause_body(E), Env1, Ren1, Ctxt, S1),
    {cerl:update_c_clause(E, Ps, G, B), S2}.

%% This does what 'expr' does, but only recurses into clause guard
%% expressions, 'case'-expressions, and the bodies of lets and letrecs.
%% Note that revisiting should not add further renamings, and we simply
%% ignore making any bindings at all at this level.

revisit_expr(E, Env, Ren, Ctxt, S0) ->
    %% Also enable peephole optimizations here.
    revisit_expr_1(cerl_lib:reduce_expr(E), Env, Ren, Ctxt, S0).

revisit_expr_1(E, Env, Ren, Ctxt, S0) ->
    case cerl:type(E) of
	'case' ->
	    {Cs, S1} = revisit_clause_list(cerl:case_clauses(E), Env,
					   Ren, Ctxt, S0),
	    {cerl:update_c_case(E, cerl:case_arg(E), Cs), S1};
	'let' ->
	    {B, S1} = revisit_expr(cerl:let_body(E), Env, Ren, Ctxt, S0),
	    {cerl:update_c_let(E, cerl:let_vars(E), cerl:let_arg(E), B),
	     S1};
	'letrec' ->
	    {B, S1} = revisit_expr(cerl:letrec_body(E), Env, Ren, Ctxt, S0),
	    {cerl:update_c_letrec(E, cerl:letrec_defs(E), B), S1};
	_ ->
	    {E, S0}
    end.

revisit_clause_list(Cs, Env, Ren, Ctxt, S) ->
    list(Cs, Env, Ren, Ctxt, S, fun revisit_clause/5).

revisit_clause(E, Env, Ren, Ctxt, S0) ->
    %% Ignore the bindings.
    {G, S1} = guard_expr(cerl:clause_guard(E), Env, Ren, Ctxt, S0),
    {B, S2} = revisit_expr(cerl:clause_body(E), Env, Ren, Ctxt, S1),
    {cerl:update_c_clause(E, cerl:clause_pats(E), G, B), S2}.

%% We use the no-shadowing strategy, renaming variables on the fly and
%% only when necessary to uphold the invariant.

add_vars(Vs, Env, Ren) ->
    add_vars(Vs, [], Env, Ren).

add_vars([V | Vs], Vs1, Env, Ren) ->
    Name = cerl:var_name(V),
    {Name1, Ren1} = rename(Name, Env, Ren),
    add_vars(Vs, [cerl:update_c_var(V, Name1) | Vs1],
	     env__bind(Name1, variable, Env), Ren1);
add_vars([], Vs, Env, Ren) ->
    {lists:reverse(Vs), Env, Ren}.

rename(Name, Env, Ren) ->
    case env__is_defined(Name, Env) of
	false ->
	    {Name, Ren};
	true ->
	    New = env__new_name(Env),
	    {New, ren__add(Name, New, Ren)}
    end.

%% Setting up the environment for a list of letrec-bound definitions.

add_defs(Ds, Env, Ren) ->
    add_defs(Ds, [], Env, Ren).

add_defs([{V, F} | Ds], Ds1, Env, Ren) ->
    Name = cerl:var_name(V),
    {Name1, Ren1} =
	case env__is_defined(Name, Env) of
	    false ->
		{Name, Ren};
	    true ->
		{N, A} = Name,
		S = atom_to_list(N) ++ "_",
		F = fun (Num) ->	%% XXX: BUG: This should be F1
			    {list_to_atom(S ++ integer_to_list(Num)), A}
		    end,
		New = env__new_function_name(F, Env),
		{New, ren__add(Name, New, Ren)}
	end,
    add_defs(Ds, [{cerl:update_c_var(V, Name1), F} | Ds1],
	     env__bind(Name1, function, Env), Ren1);
add_defs([], Ds, Env, Ren) ->
    {lists:reverse(Ds), Env, Ren}.

%% We change remote calls to important built-in functions into primop
%% calls. In some cases (e.g., for the boolean operators), this is
%% mainly to allow the cerl_to_icode module to handle them more
%% straightforwardly. In most cases however, it is simply because they
%% are supposed to be represented as primop calls on the Icode level.

rewrite_call(E, M, F, As, S) ->
    case cerl:is_c_atom(M) and cerl:is_c_atom(F) of
	true ->
	    case call_to_primop(cerl:atom_val(M),
				cerl:atom_val(F),
				length(As))
		of
		{yes, N} ->
		    %% The primop might need further handling
		    N1 = cerl:c_atom(N),
		    E1 = cerl:update_c_primop(E, N1, As),
		    rewrite_primop(E1, N1, As, S);
		no ->
		    cerl:update_c_call(E, M, F, As)
	    end;
	false ->
	    cerl:update_c_call(E, M, F, As)
    end.

call_to_primop(erlang, 'not', 1) -> {yes, ?PRIMOP_NOT};
call_to_primop(erlang, 'and', 2) -> {yes, ?PRIMOP_AND};
call_to_primop(erlang, 'or', 2) -> {yes, ?PRIMOP_OR};
call_to_primop(erlang, 'xor', 2) -> {yes, ?PRIMOP_XOR};
call_to_primop(erlang, '+', 2) -> {yes, ?PRIMOP_ADD};
call_to_primop(erlang, '+', 1) -> {yes, ?PRIMOP_IDENTITY};
call_to_primop(erlang, '-', 2) -> {yes, ?PRIMOP_SUB};
call_to_primop(erlang, '-', 1) -> {yes, ?PRIMOP_NEG};
call_to_primop(erlang, '*', 2) -> {yes, ?PRIMOP_MUL};
call_to_primop(erlang, '/', 2) -> {yes, ?PRIMOP_DIV};
call_to_primop(erlang, 'div', 2) -> {yes, ?PRIMOP_INTDIV};
call_to_primop(erlang, 'rem', 2) -> {yes, ?PRIMOP_REM};
call_to_primop(erlang, 'band', 2) -> {yes, ?PRIMOP_BAND};
call_to_primop(erlang, 'bor', 2) -> {yes, ?PRIMOP_BOR};
call_to_primop(erlang, 'bxor', 2) -> {yes, ?PRIMOP_BXOR};
call_to_primop(erlang, 'bnot', 1) -> {yes, ?PRIMOP_BNOT};
call_to_primop(erlang, 'bsl', 2) -> {yes, ?PRIMOP_BSL};
call_to_primop(erlang, 'bsr', 2) -> {yes, ?PRIMOP_BSR};
call_to_primop(erlang, '==', 2) -> {yes, ?PRIMOP_EQ};
call_to_primop(erlang, '/=', 2) -> {yes, ?PRIMOP_NE};
call_to_primop(erlang, '=:=', 2) -> {yes, ?PRIMOP_EXACT_EQ};
call_to_primop(erlang, '=/=', 2) -> {yes, ?PRIMOP_EXACT_NE};
call_to_primop(erlang, '<', 2) -> {yes, ?PRIMOP_LT};
call_to_primop(erlang, '>', 2) -> {yes, ?PRIMOP_GT};
call_to_primop(erlang, '=<', 2) -> {yes, ?PRIMOP_LE};
call_to_primop(erlang, '>=', 2) -> {yes, ?PRIMOP_GE};
call_to_primop(erlang, is_atom, 1) -> {yes, ?PRIMOP_IS_ATOM};
call_to_primop(erlang, is_binary, 1) -> {yes, ?PRIMOP_IS_BINARY};
call_to_primop(erlang, is_constant, 1) -> {yes, ?PRIMOP_IS_CONSTANT};
call_to_primop(erlang, is_float, 1) -> {yes, ?PRIMOP_IS_FLOAT};
call_to_primop(erlang, is_function, 1) -> {yes, ?PRIMOP_IS_FUNCTION};
call_to_primop(erlang, is_integer, 1) -> {yes, ?PRIMOP_IS_INTEGER};
call_to_primop(erlang, is_list, 1) -> {yes, ?PRIMOP_IS_LIST};
call_to_primop(erlang, is_number, 1) -> {yes, ?PRIMOP_IS_NUMBER};
call_to_primop(erlang, is_pid, 1) -> {yes, ?PRIMOP_IS_PID};
call_to_primop(erlang, is_port, 1) -> {yes, ?PRIMOP_IS_PORT};
call_to_primop(erlang, is_reference, 1) -> {yes, ?PRIMOP_IS_REFERENCE};
call_to_primop(erlang, is_tuple, 1) -> {yes, ?PRIMOP_IS_TUPLE};
call_to_primop(erlang, internal_is_record, 3) -> {yes, ?PRIMOP_IS_RECORD};
call_to_primop(erlang, element, 2) -> {yes, ?PRIMOP_ELEMENT};
call_to_primop(erlang, exit, 1) -> {yes, ?PRIMOP_EXIT};
call_to_primop(erlang, throw, 1) -> {yes, ?PRIMOP_THROW};
call_to_primop(erlang, error, 1) -> {yes, ?PRIMOP_ERROR};
call_to_primop(erlang, error, 2) -> {yes, ?PRIMOP_ERROR};
call_to_primop(erlang, fault, 1) -> {yes, ?PRIMOP_ERROR};
call_to_primop(erlang, fault, 2) -> {yes, ?PRIMOP_ERROR};
call_to_primop(_, _, _) -> no.

%% Also, some primops (introduced by Erlang to Core Erlang translation
%% and possibly other stages) must be recognized and rewritten.

rewrite_primop(E, N, As, S) ->
    case {cerl:atom_val(N), As} of
	{match_fail, [R]} ->
	    M = s__get_module_name(S),
	    {F, A} = s__get_function_name(S),
	    Stack = cerl:abstract([{M, F, A}]),
	    case cerl:type(R) of
		tuple ->
		    %% Function clause failures have a special encoding
		    %% as '{function_clause, Arg1, ..., ArgN}'.
		    case cerl:tuple_es(R) of
			[X | Xs] ->
			    case cerl:is_c_atom(X) of
				true ->
				    case cerl:atom_val(X) of
					function_clause ->
					    FStack = cerl:make_list(
						       [cerl:c_tuple(
							  [cerl:c_atom(M),
							   cerl:c_atom(F),
							   cerl:make_list(Xs)])]),
					    match_fail(E, X, FStack);
					_ ->
					    match_fail(E, R, Stack)
				    end;
				false ->
				    match_fail(E, R, Stack)
			    end;
			_ ->
			    match_fail(E, R, Stack)
		    end;
		_ ->
		    match_fail(E, R, Stack)
	    end;
	_ ->
	    cerl:update_c_primop(E, N, As)
    end.

match_fail(E, R, Stack) ->
    cerl:update_c_primop(E, cerl:c_atom(?PRIMOP_ERROR), [R, Stack]).

%% Simple let-definitions (of degree 1) in guard context are always
%% inline expanded. This is allowable, since they cannot have side
%% effects, and it makes it easy to generate good code for boolean
%% expressions. It could cause repeated evaluations, but typically,
%% local definitions within guards are used exactly once.

let_expr(E, Env, Ren, Ctxt, S) ->
    if Ctxt#ctxt.class == guard ->
	    case cerl:let_vars(E) of
		[V] ->
		    {Name, Ren1} = rename(cerl:var_name(V), Env, Ren),
		    Env1 = env__bind(Name, {expr, cerl:let_arg(E)}, Env),
		    expr(cerl:let_body(E), Env1, Ren1, Ctxt, S);
		_ ->
		    let_expr_1(E, Env, Ren, Ctxt, S)
	    end;
       true ->
	    let_expr_1(E, Env, Ren, Ctxt, S)
    end.

let_expr_1(E, Env, Ren, Ctxt, S0) ->
    {A, S1} = expr(cerl:let_arg(E), Env, Ren, Ctxt, S0),
    Vs = cerl:let_vars(E),
    {Vs1, Env1, Ren1} = add_vars(Vs, Env, Ren),
    {B, S2} = expr(cerl:let_body(E), Env1, Ren1, Ctxt, S1),
    {cerl:update_c_let(E, Vs1, A, B), S2}.

variable(E, Env, Ren, Ctxt, S) ->
    V = ren__map(cerl:var_name(E), Ren),
    if Ctxt#ctxt.class == guard ->
	    case env__lookup(V, Env) of
		{ok, {expr, E1}} ->
		    expr(E1, Env, Ren, Ctxt, S);   % inline
		_ ->
		    %% Since we don't track all bindings when we revisit
		    %% guards, some names will not be in the environment.
		    variable_1(E, V, S)
	    end;
       true ->
	    variable_1(E, V, S)
    end.

variable_1(E, V, S) ->
    {cerl:update_c_var(E, V), S}.

%% A catch-expression 'catch Expr' is rewritten as:
%%
%%	try Expr
%%	of (V) -> V
%%	catch (T, V, E) ->
%%	    letrec 'wrap'/1 = fun (V) -> {'EXIT', V}
%%	    in case T of
%%	         'throw' when 'true' -> V
%%	         'exit' when 'true' -> 'wrap'/1(V)
%%	         V when 'true' ->
%%	             'wrap'/1({V, erlang:get_stacktrace()})
%%	       end

catch_expr(E, Env, Ren, Ctxt, S) ->
    T = cerl:c_var('T'),
    V = cerl:c_var('V'),
    X = cerl:c_var('X'),
    W = cerl:c_var({wrap,1}),
    G = cerl:c_call(cerl:c_atom('erlang'),cerl:c_atom('get_stacktrace'),[]),
    Cs = [cerl:c_clause([cerl:c_atom('throw')], V),
	  cerl:c_clause([cerl:c_atom('exit')], cerl:c_apply(W, [V])),
	  cerl:c_clause([T], cerl:c_apply(W, [cerl:c_tuple([V,G])]))
	 ],
    C = cerl:c_case(T, Cs),
    F = cerl:c_fun([V], cerl:c_tuple([cerl:c_atom('EXIT'), V])),
    H = cerl:c_letrec([{W,F}], C),
    As = cerl:get_ann(E),
    {B, S1} = expr(cerl:catch_body(E),Env, Ren, Ctxt, S),
    {cerl:ann_c_try(As, B, [V], V, [T,V,X], H), S1}.

%% Receive-expressions are rewritten as follows:
%%
%%	receive
%%	    P1 when G1 -> B1
%%	      ...
%%	    Pn when Gn -> Bn
%%	after T -> A end
%% becomes:
%%	receive
%%	    M when 'true' ->
%%	      case M of
%%	        P1 when G1 -> do primop RECEIVE_SELECT B1
%%	          ...
%%	        Pn when Gn -> do primop RECEIVE_SELECT Bn
%%	        Pn+1 when 'true' -> primop RECEIVE_NEXT()
%%	    end
%%	after T -> A end

receive_expr(E, Env, Ren, Ctxt, S0) ->
    Cs = cerl:receive_clauses(E),
    {B, Vs, S1} = clauses(receive_clauses(Cs), Env, Ren, Ctxt, S0),
    {T, S2} = expr(cerl:receive_timeout(E), Env, Ren, Ctxt, S1),
    {A, S3} = expr(cerl:receive_action(E), Env, Ren, Ctxt, S2),
    Cs1 = [cerl:c_clause(Vs, B)],
    {cerl:update_c_receive(E, Cs1, T, A), S3}.

receive_clauses([C | Cs]) ->
    Call = cerl:c_primop(cerl:c_atom(?PRIMOP_RECEIVE_SELECT),
			 []),
    B = cerl:c_seq(Call, cerl:clause_body(C)),
    C1 =  cerl:update_c_clause(C, cerl:clause_pats(C),
			       cerl:clause_guard(C), B),
    [C1 | receive_clauses(Cs)];
receive_clauses([]) ->
    Call = cerl:c_primop(cerl:c_atom(?PRIMOP_RECEIVE_NEXT),
			 []),
    V = cerl:c_var('X'),    % any name is ok
    [cerl:c_clause([V], Call)].


new_vars(N, Env) ->
    [cerl:c_var(V) || V <- env__new_names(N, Env)].


%% ---------------------------------------------------------------------
%% Environment

env__new() ->
    rec_env:empty().

env__bind(Key, Value, Env) ->
    rec_env:bind(Key, Value, Env).

%% env__get(Key, Env) ->
%%     rec_env:get(Key, Env).

env__lookup(Key, Env) ->
    rec_env:lookup(Key, Env).

env__is_defined(Key, Env) ->
    rec_env:is_defined(Key, Env).

env__new_name(Env) ->
    rec_env:new_key(Env).

env__new_names(N, Env) ->
    rec_env:new_keys(N, Env).

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

-record(state, {module, function, pmatch=true}).

s__new(Module) ->
    #state{module = Module}.

s__get_module_name(S) ->
    S#state.module.

s__enter_function(F, S) ->
    S#state{function = F}.

s__get_function_name(S) ->
    S#state.function.

s__set_pmatch(V, S) ->
    S#state{pmatch = V}.

s__get_pmatch(S) ->
    S#state.pmatch.
