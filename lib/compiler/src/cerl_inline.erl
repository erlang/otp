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
%% @copyright 1999-2002 Richard Carlsson
%% @author Richard Carlsson <carlsson.richard@gmail.com>
%% @doc Core Erlang inliner.

%% =====================================================================
%%
%% This is an implementation of the algorithm by Waddell and Dybvig
%% ("Fast and Effective Procedure Inlining", International Static
%% Analysis Symposium 1997), adapted to the Core Erlang language.
%%
%% Instead of always renaming variables and function variables, this
%% implementation uses the "no-shadowing strategy" of Peyton Jones and
%% Marlow ("Secrets of the Glasgow Haskell Compiler Inliner", 1999).
%%
%% =====================================================================

%% TODO: inline single-source-reference operands without size limit.

-module(cerl_inline).

-export([core_transform/2, transform/1, transform/2]).

-import(cerl, [abstract/1, alias_pat/1, alias_var/1, apply_args/1,
	       apply_op/1, atom_name/1, atom_val/1, bitstr_val/1,
	       bitstr_size/1, bitstr_unit/1, bitstr_type/1,
	       bitstr_flags/1, binary_segments/1, update_c_alias/3,
	       update_c_apply/3, update_c_binary/2, update_c_bitstr/6,
	       update_c_call/4, update_c_case/3, update_c_catch/2,
	       update_c_clause/4, c_fun/2, c_int/1, c_let/3, ann_c_let/4,
	       update_c_let/4, update_c_letrec/3, update_c_module/5,
	       update_c_primop/3, update_c_receive/4, update_c_seq/3,
	       c_seq/2, update_c_try/6, c_tuple/1, update_c_values/2,
	       c_values/1, c_var/1, call_args/1, call_module/1,
	       call_name/1, case_arity/1, case_arg/1, case_clauses/1,
	       catch_body/1, clause_body/1, clause_guard/1,
	       clause_pats/1, clause_vars/1, concrete/1, cons_hd/1,
	       cons_tl/1, data_arity/1, data_es/1, data_type/1,
	       fname_arity/1, fun_body/1, fun_vars/1, get_ann/1, int_val/1,
	       is_c_atom/1, is_c_cons/1, is_c_fname/1, is_c_int/1,
	       is_c_list/1, is_c_seq/1, is_c_tuple/1, is_c_var/1,
	       is_data/1, is_literal/1, is_literal_term/1, let_arg/1,
	       let_body/1, let_vars/1, letrec_body/1, letrec_defs/1,
	       list_length/1, list_elements/1, update_data/3,
	       make_list/1, make_data_skel/2, module_attrs/1,
	       module_defs/1, module_exports/1, module_name/1,
	       primop_args/1, primop_name/1, receive_action/1,
	       receive_clauses/1, receive_timeout/1, seq_arg/1,
	       seq_body/1, set_ann/2, try_arg/1, try_body/1, try_vars/1,
	       try_evars/1, try_handler/1, tuple_es/1, tuple_arity/1,
	       type/1, values_es/1, var_name/1,
	       map_arg/1, map_es/1, update_c_map/3,
	       update_c_map_pair/4,
	       map_pair_op/1, map_pair_key/1, map_pair_val/1
	   ]).

-import(lists, [foldl/3, foldr/3, member/2, mapfoldl/3, reverse/1]).

%%
%% Constants
%%

debug_runtime() -> false.
debug_counters() -> false.

%% Normal execution times for inlining are between 0.1 and 0.3 seconds
%% (on the author's current equipment). The default effort limit of 150
%% is high enough that most normal programs never hit the limit even
%% once, and for difficult programs, it generally keeps the execution
%% times below 2-5 seconds. Using an effort counter of 1000 will thus
%% have no further effect on most programs, but some programs may take
%% as much as 10 seconds or more. Effort counts larger than 2500 have
%% never been observed even on very ill-conditioned programs.
%%
%% Size limits between 6 and 18 tend to actually shrink the code,
%% because of the simplifications made possible by inlining. A limit of
%% 16 seems to be optimal for this purpose, often shrinking the
%% executable code by up to 10%. Size limits between 18 and 30 generally
%% give the same code size as if no inlining was done (i.e., code
%% duplication balances out the simplifications at these levels). A size
%% limit between 1 and 5 tends to inline small functions and propagate
%% constants, but does not cause much simplifications do be done, so the
%% net effect will be a slight increase in code size. For size limits
%% above 30, the executable code size tends to increase with about 10%
%% per 100 units, with some variations depending on the sizes of
%% functions in the source code.
%%
%% Typically, about 90% of the maximum speedup achievable is already
%% reached using a size limit of 30, and 98% is reached at limits around
%% 100-150; there is rarely any point in letting the code size increase
%% by more than 10-15%. If too large functions are inlined, cache
%% effects will slow the program down.

default_effort() -> 150.
default_size() -> 24.
default_unroll() -> 1.

%% Base costs/weights for different kinds of expressions. If these are
%% modified, the size limits above may have to be adjusted.

weight(var) -> 0;	% We count no cost for variable accesses.
weight(values) -> 0;	% Value aggregates have no cost in themselves.
weight(literal) -> 1;	% We assume efficient handling of constants.
weight(data) -> 1;	% Base cost; add 1 per element.
weight(element) -> 1;   % Cost of storing/fetching an element.
weight(argument) -> 1;  % Cost of passing a function argument.
weight('fun') -> 6;	% Base cost + average number of free vars.
weight('let') -> 0;	% Count no cost for let-bindings.
weight(letrec) -> 0;    % Like a let-binding.
weight('case') -> 0;	% Case switches have no base cost.
weight(clause) -> 1;    % Count one jump at the end of each clause body.
weight('receive') -> 9;	% Initialization/cleanup cost.
weight('try') -> 1;	% Assume efficient implementation.
weight('catch') -> 1;	% See `try'.
weight(apply) -> 3;     % Average base cost: call/return.
weight(call) -> 3;      % Assume remote-calls as efficient as `apply'.
weight(primop) -> 2;    % Assume more efficient than `apply'.
weight(binary) -> 4;    % Initialisation base cost.
weight(bitstr) -> 3;    % Coding/decoding a value; like a primop.
weight(map) -> 4;       % Initialisation base cost.
weight(map_pair) -> 3;  % Coding/decoding a value; like a primop.
weight(module) -> 1.    % Like a letrec with a constant body

%% These "reference" structures are used for variables and function
%% variables. They keep track of the variable name, any bound operand,
%% and the associated store location.

-record(ref, {name, opnd, loc}).

%% Operand structures contain the operand expression, the renaming and
%% environment, the state location, and the effort counter at the call
%% site (cf. `visit').

-record(opnd, {expr, ren, env, loc, effort, no_inline}).

%% Since expressions are only visited in `effect' context when they are
%% not bound to a referenced variable, only expressions visited in
%% 'value' context are cached.

-record(cache, {expr, size}).

%% The context flags for an application structure are kept separate from
%% the structure itself. Note that the original algorithm had exactly
%% one operand in each application context structure, while we can have
%% several, or none.

-record(app, {opnds, ctxt, loc}).


%%
%% Interface functions
%%

%% Use compile option `{core_transform, inline}' to insert this as a
%% compilation pass.

-spec core_transform(cerl:cerl(), [compile:option()]) -> cerl:cerl().

core_transform(Code, Opts) ->
    cerl:to_records(transform(cerl:from_records(Code), Opts)).

-spec transform(cerl:cerl()) -> cerl:cerl().

transform(Tree) ->
    transform(Tree, []).

-spec transform(cerl:cerl(), [compile:option()]) -> cerl:cerl().

transform(Tree, Opts) ->
    main(Tree, value, Opts).

main(Tree, Ctxt, Opts) ->
    %% We spawn a new process to do the work, so we don't have to worry
    %% about cluttering the process dictionary with debugging info, or
    %% proper deallocation of ets-tables.
    Opts1 = Opts ++ [{inline_size, default_size()},
		     {inline_effort, default_effort()},
		     {inline_unroll, default_unroll()}],
    Reply = self(),
    Pid = spawn_link(fun () -> start(Reply, Tree, Ctxt, Opts1) end),
    receive
        {Pid, Tree1} -> Tree1
    end.

start(Reply, Tree, Ctxt, Opts) ->
    init_debug(),
    case debug_runtime() of
        %% true ->
        %%     put(inline_start_time,
        %%         element(1, erlang:statistics(runtime)));
        false ->
            ok
    end,
    Size = erlang:max(1, proplists:get_value(inline_size, Opts)),
    Effort = erlang:max(1, proplists:get_value(inline_effort, Opts)),
    Unroll = erlang:max(1, proplists:get_value(inline_unroll, Opts)),
    case proplists:get_bool(verbose, Opts) of
	true ->
	    io:fwrite("Inlining: inline_size=~w inline_effort=~w\n",
		      [Size, Effort]);
	false ->
	    ok
    end,

    %% Note that the counters of the new state are passive.
    S = st__new(Effort, Size, Unroll),

%%% Initialization is not needed at present. Note that the code in
%%% `inline_init' is not up-to-date with this module.
%%%     {Tree1, S1} = inline_init:init(Tree, S),
%%%     {Tree2, _S2} = i(Tree1, Ctxt, S1),
    {Tree2, _S2} = i(Tree, Ctxt, S),
    report_debug(),
    Reply ! {self(), Tree2}.

init_debug() ->
    case debug_counters() of
        %% true ->
        %%     put(counter_effort_triggers, 0),
        %%     put(counter_effort_max, 0),
        %%     put(counter_size_triggers, 0),
        %%     put(counter_size_max, 0);
        false ->
            ok
    end.

report_debug() ->
    case debug_runtime() of
        %% true ->
        %%     {Time, _} = erlang:statistics(runtime),
        %%     report("Total run time for inlining: ~.2.0f s.\n",
	%% 	   [(Time - get(inline_start_time))/1000]);
        false ->
            ok
    end,
    case debug_counters() of
        %% true ->
        %%     counter_stats();
        false ->
            ok
    end.

%% counter_stats() ->
%%     T1 = get(counter_effort_triggers),
%%     T2 = get(counter_size_triggers),
%%     E = get(counter_effort_max),
%%     S = get(counter_size_max),
%%     M1 = io_lib:fwrite("\tNumber of triggered "
%%                        "effort counters: ~p.\n", [T1]),
%%     M2 = io_lib:fwrite("\tNumber of triggered "
%%                        "size counters: ~p.\n", [T2]),
%%     M3 = io_lib:fwrite("\tLargest active effort counter: ~p.\n",
%%                        [E]),
%%     M4 = io_lib:fwrite("\tLargest active size counter: ~p.\n",
%%                        [S]),
%%     report("Counter statistics:\n~s", [[M1, M2, M3, M4]]).


%% =====================================================================
%% The main inlining function
%%
%% i(E :: coreErlang(),
%%   Ctxt :: value | effect | #app{}
%%   Ren :: renaming(),
%%   Env :: environment(),
%%   S :: state())
%%   -> {E', S'}
%%
%% Note: It is expected that the input source code ('E') does not
%% contain free variables. If it does, there is a risk of accidental
%% name capture, in case a generated "new" variable name happens to be
%% the same as the name of a variable that is free further below in the
%% tree; the algorithm only consults the current environment to check if
%% a name already exists.
%%
%% The renaming maps names of source-code variable and function
%% variables to new names as necessary to avoid clashes, according to
%% the "no-shadowing" strategy. The environment maps *residual-code*
%% variables and function variables to operands and global information.
%% Separating the renaming from the environment, and using the
%% residual-code variables instead of the source-code variables as its
%% domain, improves the behaviour of the algorithm when code needs to be
%% traversed more than once.
%%
%% Note that there is no such thing as a `test' context for expressions
%% in (Core) Erlang (see `i_case' below for details).

i(E, Ctxt, S) ->
    i(E, Ctxt, ren__identity(), env__empty(), S).

i(E, Ctxt, Ren, Env, S0) ->
    %% Count one unit of effort on each pass.
    S = count_effort(1, S0),
    case is_data(E) of
        true ->
            i_data(E, Ctxt, Ren, Env, S);
        false ->
            case type(E) of
                var ->
                    i_var(E, Ctxt, Ren, Env, S);
                values ->
                    i_values(E, Ctxt, Ren, Env, S);
                'fun' ->
                    i_fun(E, Ctxt, Ren, Env, S);
                seq ->
                    i_seq(E, Ctxt, Ren, Env, S);
                'let' ->
                    i_let(E, Ctxt, Ren, Env, S);
                letrec ->
                    i_letrec(E, Ctxt, Ren, Env, S);
                'case' ->
                    i_case(E, Ctxt, Ren, Env, S);
                'receive' ->
                    i_receive(E, Ctxt, Ren, Env, S);
                apply ->
                    i_apply(E, Ctxt, Ren, Env, S);
                call ->
                    i_call(E, Ctxt, Ren, Env, S);
                primop ->
                    i_primop(E, Ren, Env, S);
                'try' ->
                    i_try(E, Ctxt, Ren, Env, S);
                'catch' ->
                    i_catch(E, Ctxt, Ren, Env, S);
		binary ->
		    i_binary(E, Ren, Env, S);
		map ->
		    i_map(E, Ctxt, Ren, Env, S);
                module ->
                    i_module(E, Ctxt, Ren, Env, S)
            end
    end.

i_data(E, Ctxt, Ren, Env, S) ->
    case is_literal(E) of
        true ->
            %% This is the `(const c)' case of the original algorithm:
            %% literal terms which (regardless of size) do not need to
            %% be constructed dynamically at runtime - boldly assuming
            %% that the compiler/runtime system can handle this.
            case Ctxt of
                effect ->
                    %% Reduce useless constants to a simple value.
                    {void(), count_size(weight(literal), S)};
                _ ->
                    %% (In Erlang, we cannot set all non-`false'
                    %% constants to `true' in a `test' context, like we
                    %% could do in Lisp or C, so the above is the only
                    %% special case to be handled here.)
                    {E, count_size(weight(literal), S)}
            end;
        false ->
            %% Data constructors are like to calls to safe built-in
            %% functions, for which we can "decide to inline"
            %% immediately; there is no need to create operand
            %% structures. In `effect' context, we can simply make a
            %% sequence of the argument expressions, also visited in
            %% `effect' context. In all other cases, the arguments are
            %% visited for value.
            case Ctxt of
                effect ->
                    %% Note that this will count the sizes of the
                    %% subexpressions, even though some or all of them
                    %% might be discarded by the sequencing afterwards.
                    {Es1, S1} = mapfoldl(fun (E, S) ->
						 i(E, effect, Ren, Env,
						   S)
					 end,
					 S, data_es(E)),
                    E1 = foldl(fun (E1, E2) -> make_seq(E1, E2) end,
			       void(), Es1),
                    {E1, S1};
                _ ->
                    {Es1, S1} = mapfoldl(fun (E, S) ->
						 i(E, value, Ren, Env,
						   S)
					 end,
					 S, data_es(E)),
                    %% The total size/cost is the base cost for a data
                    %% constructor plus the cost for storing each
                    %% element.
                    N = weight(data) + length(Es1) * weight(element),
                    S2 = count_size(N, S1),
                    {update_data(E, data_type(E), Es1), S2}
            end
    end.

%% This is the `(ref x)' (variable use) case of the original algorithm.
%% Note that binding occurrences are always handled in the respective
%% cases of the binding constructs.

i_var(E, Ctxt, Ren, Env, S) ->
    case Ctxt of
        effect ->
            %% Reduce useless variable references to a simple constant.
	    %% This also avoids useless visiting of bound operands.
            {void(), count_size(weight(literal), S)};
        _ ->
	    Name = var_name(E),
            case env__lookup(ren__map(Name, Ren), Env) of
                {ok, R} ->
                    case R#ref.opnd of
                        undefined ->
                            %% The variable is not associated with an
                            %% argument expression; just residualize it.
                            residualize_var(R, S);
                        Opnd ->
			    i_var_1(R, Opnd, Ctxt, Env, S)
                    end;
                error ->
                    %% The variable is unbound. (It has not been
                    %% accidentally captured, however, or it would have
                    %% been in the environment.) We leave it as it is,
                    %% without any warning.
		    {E, count_size(weight(var), S)}
            end
    end.

%% This first visits the bound operand and then does copy propagation.
%% Note that we must first set the "inner-pending" flag, and clear the
%% flag afterwards.

i_var_1(R, Opnd, Ctxt, Env, S) ->
    %% If the operand is already "inner-pending", it is residualised.
    %% (In Lisp/C, if the variable might be assigned to, it should also
    %% be residualised.)
    L = Opnd#opnd.loc,
    case st__test_inner_pending(L, S) of
	true ->
	    residualize_var(R, S);
	false ->
	    S1 = st__mark_inner_pending(L, S),
	    try visit(Opnd, S1) of
		{E, S2} ->
		    %% Note that we pass the current environment and
		    %% context to `copy', but not the current renaming.
		    S3 = st__clear_inner_pending(L, S2),
		    copy(R, Opnd, E, Ctxt, Env, S3)
	    catch
		throw:X ->
 		    %% If we use destructive update for the
 		    %% `inner-pending' flag, we must make sure to clear
 		    %% it also if we make a nonlocal return.
		    _S2 = st__clear_inner_pending(Opnd#opnd.loc, S1),
		    throw(X)
	    end
    end.

%% A multiple-value aggregate `<e1, ..., en>'. This is very much like a
%% tuple data constructor `{e1, ..., en}'; cf. `i_data' for details.

i_values(E, Ctxt, Ren, Env, S) ->
    case values_es(E) of
	[E1] ->
	    %% Single-value aggregates can be dropped; they are simply
	    %% notation.
	    i(E1, Ctxt, Ren, Env, S);
	Es ->
	    %% In `effect' context, we can simply make a sequence of the
	    %% argument expressions, also visited in `effect' context.
	    %% In all other cases, the arguments are visited for value.
	    case Ctxt of
		effect ->
		    {Es1, S1} =
			mapfoldl(fun (E, S) ->
					 i(E, effect, Ren, Env, S)
				 end,
				 S, Es),
		    E1 = foldl(fun (E1, E2) ->
				       make_seq(E1, E2)
			       end,
			       void(), Es1),
		    {E1, S1};    % drop annotations on E
		_ ->
		    {Es1, S1} = mapfoldl(fun (E, S) ->
						 i(E, value, Ren, Env,
						   S)
					 end,
					 S, Es),
		    %% Aggregating values does not write them to memory,
		    %% so we count no extra cost per element.
		    S2 = count_size(weight(values), S1),
		    {update_c_values(E, Es1), S2}
	    end
    end.

%% A let-expression `let <v1,...,vn> = e0 in e1' is semantically
%% equivalent to a case-expression `case e0 of <v1,...,vn> when 'true'
%% -> e1 end'. As a special case, `let <v> = e0 in e1' is also
%% equivalent to `apply fun (v) -> e0 (e1)'. However, for efficiency,
%% and in order to allow the handling of `case' clauses to introduce new
%% let-expressions without entering an infinite rewrite loop, we handle
%% these directly.

%%% %% Rewriting a `let' to an equivalent expression.
%%% i_let(E, Ctxt, Ren, Env, S) ->
%%%     case let_vars(E) of
%%% 	[V] ->
%%%  	    E1 = update_c_apply(E, c_fun([V], let_body(E)), [let_arg(E)]),
%%%  	    i(E1, Ctxt, Ren, Env, S);
%%% 	Vs ->
%%%  	    C = c_clause(Vs, abstract(true), let_body(E)),
%%%  	    E1 = update_c_case(E, let_arg(E), [C]),
%%%  	    i(E1, Ctxt, Ren, Env, S)
%%%     end.

i_let(E, Ctxt, Ren, Env, S) ->
    case let_vars(E) of
 	[V] ->
 	    i_let_1(V, E, Ctxt, Ren, Env, S);
	Vs ->
	    %% Visit the argument expression in `value' context, to
	    %% simplify it as far as possible.
	    {A, S1} = i(let_arg(E), value, Ren, Env, S),
	    case get_components(length(Vs), result(A)) of
		{true, As} ->
		    %% Note that only the components of the result of
		    %% `A' are passed on; any effects are hoisted.
		    {E1, S2} = i_let_2(Vs, As, E, Ctxt, Ren, Env, S1),
		    {hoist_effects(A, E1), S2};
		false ->
		    %% We cannot do anything with this `let', since the
		    %% variables cannot be matched against the argument
		    %% components. Just visit the variables for renaming
		    %% and visit the body for value (cf. `i_fun').
		    {_, Ren1, Env1, S2} = bind_locals(Vs, Ren, Env, S1),
		    Vs1 = i_params(Vs, Ren1, Env1),
		    %% The body is always visited for value here.
		    {B, S3} = i(let_body(E), value, Ren1, Env1, S2),
		    S4 = count_size(weight('let'), S3),
		    {update_c_let(E, Vs1, A, B), S4}
	    end
    end.

%% Single-variable `let' binding.

i_let_1(V, E, Ctxt, Ren, Env, S) ->
    %% Make an operand structure for the argument expression, create a
    %% local binding from the parameter to the operand structure, and
    %% visit the body. Finally create necessary bindings and/or set
    %% flags.
    {Opnd, S1} = make_opnd(let_arg(E), Ren, Env, S),
    {[R], Ren1, Env1, S2} = bind_locals([V], [Opnd], Ren, Env, S1),
    {E1, S3} = i(let_body(E), Ctxt, Ren1, Env1, S2),
    i_let_3([R], [Opnd], E1, S3).

%% Multi-variable `let' binding.

i_let_2(Vs, As, E, Ctxt, Ren, Env, S) ->
    %% Make operand structures for the argument components. Note that
    %% since the argument has already been visited at this point, we use
    %% the identity renaming for the operands.
    {Opnds, S1} = mapfoldl(fun (E, S) ->
                                   make_opnd(E, ren__identity(), Env, S)
                           end,
                           S, As),
    %% Create local bindings from the parameters to their respective
    %% operand structures, and visit the body.
    {Rs, Ren1, Env1, S2} = bind_locals(Vs, Opnds, Ren, Env, S1),
    {E1, S3} = i(let_body(E), Ctxt, Ren1, Env1, S2),
    i_let_3(Rs, Opnds, E1, S3).

i_let_3(Rs, Opnds, E, S) ->
    %% Create necessary bindings and/or set flags.
    {E1, S1} = make_let_bindings(Rs, E, S),
    
    %% We must also create evaluation for effect, for any unused
    %% operands, as after an application expression.
    residualize_operands(Opnds, E1, S1).

%% A sequence `do e1 e2', written `(seq e1 e2)' in the original
%% algorithm, where `e1' is evaluated for effect only (since its value
%% is not used), and `e2' yields the final value. Note that we use
%% `make_seq' to recompose the sequence after visiting the parts.

i_seq(E, Ctxt, Ren, Env, S) ->
    {E1, S1} = i(seq_arg(E), effect, Ren, Env, S),
    {E2, S2} = i(seq_body(E), Ctxt, Ren, Env, S1),
    %% A sequence has no cost in itself.
    {make_seq(E1, E2), S2}.


%% The `case' switch of Core Erlang is rather different from the boolean
%% `(if e1 e2 e3)' case of the original algorithm, but the central idea
%% is the same: if, given the simplified switch expression (which is
%% visited in `value' context - a boolean `test' context would not be
%% generally useful), there is a clause which could definitely be
%% selected, such that no clause before it can possibly be selected,
%% then we can eliminate all other clauses. (And even if this is not the
%% case, some clauses can often be eliminated.) Furthermore, if a clause
%% can be selected, we can replace the case-expression (including the
%% switch expression) with the body of the clause and a set of zero or
%% more let-bindings of subexpressions of the switch expression. (In the
%% simplest case, the switch expression is evaluated only for effect.)

i_case(E, Ctxt, Ren, Env, S) ->
    %% First visit the switch expression in `value' context, to simplify
    %% it as far as possible. Note that only the result part is passed
    %% on to the clause matching below; any effects are hoisted.
    {A, S1} = i(case_arg(E), value, Ren, Env, S),
    A1 = result(A),

    %% Propagating an application context into the branches could cause
    %% the arguments of the application to be evaluated *after* the
    %% switch expression, but *before* the body of the selected clause.
    %% Such interleaving is not allowed in general, and it does not seem
    %% worthwile to make a more powerful transformation here. Therefore,
    %% the clause bodies are conservatively visited for value if the
    %% context is `application'.
    Ctxt1 = safe_context(Ctxt),
    {E1, S2} = case get_components(case_arity(E), A1) of
		   {true, As} ->
		       i_case_1(As, E, Ctxt1, Ren, Env, S1);
		   false ->
		       i_case_1([], E, Ctxt1, Ren, Env, S1)
	       end,
    {hoist_effects(A, E1), S2}.

i_case_1(As, E, Ctxt, Ren, Env, S) ->
    case i_clauses(As, case_clauses(E), Ctxt, Ren, Env, S) of
        {false, {As1, Vs, Env1, Cs}, S1} ->
            %% We still have a list of clauses. Sanity check:
            if Cs =:= [] ->
                    report_warning("empty list of clauses "
				   "in residual program!.\n");
               true ->
                    ok
            end,
	    {A, S2} = i(c_values(As1), value, ren__identity(), Env1,
			S1),
	    {E1, S3} = i_case_2(Cs, A, E, S2),
	    i_case_3(Vs, Env1, E1, S3);
        {true, {_, Vs, Env1, [C]}, S1} ->
            %% A single clause was selected; we just take the body.
	    i_case_3(Vs, Env1, clause_body(C), S1)
    end.

%% Check if all clause bodies are actually equivalent expressions that
%% do not depent on pattern variables (this sometimes occurs as a
%% consequence of inlining, e.g., all branches might yield 'true'), and
%% if so, replace the `case' with a sequence, first evaluating the
%% clause selection for effect, then evaluating one of the clause bodies
%% for its value. (Unless the switch contains a catch-all clause, the
%% clause selection must be evaluated for effect, since there is no
%% guarantee that any of the clauses will actually match. Assuming that
%% some clause always matches could make an undefined program produce a
%% value.) This makes the final size less than what was accounted for
%% when visiting the clauses, but currently we don't try to adjust for
%% this.

i_case_2(Cs, A, E, S) ->
    case equivalent_clauses(Cs) of
	false ->
	    %% Count the base sizes for the remaining clauses; pattern
	    %% and guard sizes are already counted.
	    N = weight('case') + weight(clause) * length(Cs),
	    S1 = count_size(N, S),
	    {update_c_case(E, A, Cs), S1};
	true ->
	    case cerl_clauses:any_catchall(Cs) of
		true ->
		    %% We know that some clause must be selected, so we
		    %% can drop all the testing as well.
		    E1 = make_seq(A, clause_body(hd(Cs))),
		    {E1, S};
		false ->
		    %% The clause selection must be performed for
		    %% effect.
		    E1 = update_c_case(E, A,
				       set_clause_bodies(Cs, void())),
		    {make_seq(E1, clause_body(hd(Cs))), S}
	    end
    end.

i_case_3(Vs, Env, E, S) ->
    %% For the variables bound to the switch expression subexpressions,
    %% make let bindings or create evaluation for effect.
    Rs = [env__get(var_name(V), Env) || V <- Vs],
    {E1, S1} = make_let_bindings(Rs, E, S),
    Opnds = [R#ref.opnd || R <- Rs],
    residualize_operands(Opnds, E1, S1).

%% This function takes a sequence of switch expressions `Es' (which can
%% be the empty list if these are unknown) and a list `Cs' of clauses,
%% and returns `{Match, {As, Vs, Env1, Cs1}, S1}' where `As' is a list
%% of residual switch expressions, `Vs' the list of variables used in
%% the templates, `Env1' the environment for the templates, and `Cs1'
%% the list of residual clauses. `Match' is `true' if some clause could
%% be shown to definitely match (in this case, `Cs1' contains exactly
%% one element), and `false' otherwise. `S1' is the new state. The given
%% `Ctxt' is the context to be used for visiting the body of clauses.
%%
%% Visiting a clause basically amounts to extending the environment for
%% all variables in the pattern, as for a `fun' (cf. `i_fun'),
%% propagating match information if possible, and visiting the guard and
%% body in the new environment.
%%
%% To make it cheaper to do handle a set of clauses, and to avoid
%% unnecessarily exceeding the size limit, we avoid visiting the bodies
%% of clauses which are subsequently removed, by dividing the visiting
%% of a clause into two stages: first construct the environment(s) and
%% visit the pattern (for renaming) and the guard (for value), then
%% reduce the switch as much as possible, and lastly visit the body.

i_clauses(Cs, Ctxt, Ren, Env, S) ->
    i_clauses([], Cs, Ctxt, Ren, Env, S).

i_clauses(Es, Cs, Ctxt, Ren, Env, S) ->
    %% Create templates for the switch expressions.
    {Ts, {Vs, Env0}} = mapfoldl(fun (E, {Vs, Env}) ->
					{T, Vs1, Env1} =
					    make_template(E, Env),
					{T, {Vs1 ++ Vs, Env1}}
				end,
				{[], Env}, Es),
    
    %% Make operand structures for the switch subexpression templates
    %% (found in `Env0') and add proper ref-structure bindings to the
    %% environment. Since the subexpressions in general can be
    %% interdependent (Vs is in reverse-dependency order), the
    %% environment (and renaming) must be created incrementally. Note
    %% that since the switch expressions have been visited already, the
    %% identity renaming is used for the operands.
    Vs1 = lists:reverse(Vs),
    {Ren1, Env1, S1} =
	foldl(fun (V, {Ren, Env, S}) ->
		      E = env__get(var_name(V), Env0),
		      {Opnd, S_1} = make_opnd(E, ren__identity(), Env,
					      S),
		      {_, Ren1, Env1, S_2} = bind_locals([V], [Opnd],
							 Ren, Env, S_1),
		      {Ren1, Env1, S_2}
	      end,
	      {Ren, Env, S}, Vs1),
    
    %% First we visit the head of each individual clause, renaming
    %% pattern variables, inserting let-bindings in the guard and body,
    %% and visiting the guard. The information used for visiting the
    %% clause body will be prefixed to the clause annotations.
    {Cs1, S2} = mapfoldl(fun (C, S) ->
				 i_clause_head(C, Ts, Ren1, Env1, S)
			 end,
			 S1, Cs),
    
    %% Now that the clause guards have been reduced as far as possible,
    %% we can attempt to reduce the clauses.
    As = [hd(get_ann(T)) || T <- Ts],
    case cerl_clauses:reduce(Cs1, Ts) of
        {false, Cs2} ->
            %% We still have one or more clauses (with associated
            %% extended environments). Their bodies have not yet been
            %% visited, so we do that (in the respective safe
            %% environments, adding the sizes of the visited heads to
            %% the current size counter) and return the final list of
            %% clauses.
            {Cs3, S3} = mapfoldl(
                          fun (C, S) ->
                                  i_clause_body(C, Ctxt, S)
                          end,
                          S2, Cs2),
            {false, {As, Vs1, Env1, Cs3}, S3};
        {true, {C, _}} ->
            %% A clause C could be selected (the bindings have already
            %% been added to the guard/body). Note that since the clause
            %% head will probably be discarded, its size is not counted.
	    {C1, Ren2, Env2, _} = get_clause_extras(C),
	    {B, S3} = i(clause_body(C), Ctxt, Ren2, Env2, S2),
	    C2 = update_c_clause(C1, clause_pats(C1), clause_guard(C1), B),
	    {true, {As, Vs1, Env1, [C2]}, S3}
    end.

%% This visits the head of a clause, renames pattern variables, inserts
%% let-bindings in the guard and body, and does inlining on the guard
%% expression. Returns a list of pairs `{NewClause, Data}', where `Data'
%% is `{Renaming, Environment, Size}' used for visiting the body of the
%% new clause.

i_clause_head(C, Ts, Ren, Env, S) ->
    %% Match the templates against the (non-renamed) patterns to get the
    %% available information about matching subexpressions. We don't
    %% care at this point whether an exact match/nomatch is detected.
    Ps = clause_pats(C),
    Bs = case cerl_clauses:match_list(Ps, Ts) of
	     {_, Bs1} -> Bs1;
	     none -> []
	 end,

    %% The patterns must be visited for renaming; cf. `i_pattern'. We
    %% use a passive size counter for visiting the patterns and the
    %% guard (cf. `visit'), because we do not know at this stage whether
    %% the clause will be kept or not; the final value of the counter is
    %% included in the returned value below.
    {_, Ren1, Env1, S1} = bind_locals(clause_vars(C), Ren, Env, S),
    S2 = new_passive_size(get_size_limit(S1), S1),
    {Ps1, S3} = mapfoldl(fun (P, S) ->
				 i_pattern(P, Ren1, Env1, Ren, Env, S)
			 end,
			 S2, Ps),
    
    %% Rewrite guard and body and visit the guard for value. Discard the
    %% latter size count if the guard turns out to be a constant.
    G = add_match_bindings(Bs, clause_guard(C)),
    B = add_match_bindings(Bs, clause_body(C)),
    {G1, S4} = i(G, value, Ren1, Env1, S3),
    S5 = case is_literal(G1) of
	     true ->
		 revert_size(S3, S4);
	     false ->
		 S4
	 end,

    %% Revert to the size counter we had on entry to this function. The
    %% environment and renaming, together with the size of the clause
    %% head, are prefixed to the annotations for later use.
    Size = get_size_value(S5),
    C1 = update_c_clause(C, Ps1, G1, B),
    {set_clause_extras(C1, Ren1, Env1, Size), revert_size(S, S5)}.

add_match_bindings(Bs, E) ->
    %% Don't waste time if the variables definitely cannot be used.
    %% (Most guards are simply `true'.)
    case is_literal(E) of
	true ->
	    E;
	false ->
	    Vs = [V || {V, E} <- Bs, E =/= any],
	    Es = [hd(get_ann(E)) || {_V, E} <- Bs, E =/= any],
	    c_let(Vs, c_values(Es), E)
    end.

i_clause_body(C0, Ctxt, S) ->
    {C, Ren, Env, Size} = get_clause_extras(C0),
    S1 = count_size(Size, S),
    {B, S2} = i(clause_body(C), Ctxt, Ren, Env, S1),
    C1 = update_c_clause(C, clause_pats(C), clause_guard(C), B),
    {C1, S2}.

get_clause_extras(C) ->
    [{Ren, Env, Size} | As] = get_ann(C),
    {set_ann(C, As), Ren, Env, Size}.

set_clause_extras(C, Ren, Env, Size) ->
    As = [{Ren, Env, Size} | get_ann(C)],
    set_ann(C, As).

%% This is the `(lambda x e)' case of the original algorithm. A
%% `fun' is like a lambda expression, but with a varying number of
%% parameters; possibly zero.

i_fun(E, Ctxt, Ren, Env, S) ->
    case Ctxt of
        effect ->
            %% Reduce useless `fun' expressions to a simple constant;
	    %% visiting the body would be a waste of time, and could
	    %% needlessly mark variables as referenced.
            {void(), count_size(weight(literal), S)};
        value ->
            %% Note that the variables are visited as patterns.
            Vs = fun_vars(E),
            {_, Ren1, Env1, S1} = bind_locals(Vs, Ren, Env, S),
            Vs1 = i_params(Vs, Ren1, Env1),

            %% The body is always visited for value.
            {B, S2} = i(fun_body(E), value, Ren1, Env1, S1),

	    %% We don't bother to include the exact number of free
	    %% variables in the cost for creating a fun-value.
            S3 = count_size(weight('fun'), S2),

	    %% Inlining might have duplicated code, so we must remove
	    %% any 'id'-annotations from the original fun-expression.
	    %% (This forces a later stage to invent new id:s.) This is
	    %% necessary as long as fun:s may still need to be
	    %% identified the old way. Function variables that are not
	    %% in application context also have such annotations, but
	    %% the inlining will currently lose all annotations on
	    %% variable references (I think), so that's not a problem.
            {set_ann(c_fun(Vs1, B), kill_id_anns(get_ann(E))), S3};
        #app{} ->
            %% An application of a fun-expression (in the original
            %% source code) is handled by going directly to `inline'.
            %% This is never residualised unless there is an arity
            %% mismatch, so we don't set up new counters here. Note that
            %% inlining of copy-propagated fun-expressions is done in
            %% `copy'; not here!
            inline(E, Ctxt, Ren, Env, S)
    end.

%% A `letrec' requires a circular environment, but is otherwise like a
%% `let', i.e. like a direct lambda application. Note that only
%% fun-expressions (lambda abstractions) may occur in the right-hand
%% side of each definition.

i_letrec(E, Ctxt, Ren, Env, S) ->
    %% We must turn off inlining if this `letrec' is specially
    %% implemented.
    NoInline = member(letrec_goto, get_ann(E)),

    %% Note that we pass an empty list for the auto-referenced
    %% (exported) functions here.
    {Es, B, _, S1} = i_letrec(letrec_defs(E), letrec_body(E), [], Ctxt,
			      Ren, Env, NoInline, S),

    %% If no bindings remain, only the body is returned.
    case Es of
        [] ->
            {B, S1};    % drop annotations on E
        _ ->
            S2 = count_size(weight(letrec), S1),
            {update_c_letrec(E, Es, B), S2}
    end.

%% The major part of this is shared by letrec-expressions and module
%% definitions alike.

i_letrec(Es, B, Xs, Ctxt, Ren, Env, NoInline, S) ->
    %% First, we create operands with dummy renamings and environments,
    %% and with fresh store locations for cached expressions and operand
    %% info.
    {Opnds, S1} = mapfoldl(fun ({_, E}, S) ->
                                   make_opnd(E, undefined, undefined,
                                             NoInline, S)
                           end,
                           S, Es),

    %% Then we make recursive bindings for the definitions.
    {Rs, Ren1, Env1, S2} = bind_recursive([F || {F, _} <- Es],
                                          Opnds, Ren, Env, S1),
    
    %% For the function variables listed in Xs (none for a
    %% letrec-expression), we must make sure that the corresponding
    %% operand expressions are visited and that the definitions are
    %% marked as referenced; we also need to return the possibly renamed
    %% function variables.
    {Xs1, S3} =
        mapfoldl(
          fun (X, S) ->
                  Name = ren__map(var_name(X), Ren1),
                  case env__lookup(Name, Env1) of
                      {ok, R} ->
                          S_1 = i_letrec_export(R, S),
                          {ref_to_var(R), S_1};
                      error ->
                          %% We just skip any exports that are not
                          %% actually defined here, and generate a
                          %% warning message.
                          {N, A} = var_name(X),
                          report_warning("export `~w'/~w "
					 "not defined.\n", [N, A]),
                          {X, S}
                  end
          end,
          S2, Xs),

    %% At last, we can then visit the body.
    {B1, S4} = i(B, Ctxt, Ren1, Env1, S3),

    %% Finally, we create new letrec-bindings for any and all
    %% residualised definitions. All referenced functions should have
    %% been visited; the call to `visit' below is expected to retreive a
    %% cached expression.
    Rs1 = keep_referenced(Rs, S4),
    {Es1, S5} = mapfoldl(fun (R, S) ->
				 {E_1, S_1} = visit(R#ref.opnd, S),
				 {{ref_to_var(R), E_1}, S_1}
			 end,
			 S4, Rs1),
    {Es1, B1, Xs1, S5}.

%% This visits the operand for a function definition exported by a
%% `letrec' (which is really a `module' module definition, since normal
%% letrecs have no export declarations). Only the updated state is
%% returned. We must handle the "inner-pending" flag when doing this;
%% cf. `i_var'.

i_letrec_export(R, S) ->
    Opnd = R#ref.opnd,
    S1 = st__mark_inner_pending(Opnd#opnd.loc, S),
    {_, S2} = visit(Opnd, S1),
    {_, S3} = residualize_var(R, st__clear_inner_pending(Opnd#opnd.loc,
							 S2)),
    S3.

%% This is the `(call e1 e2)' case of the original algorithm. The only
%% difference is that we must handle multiple (or no) operand
%% expressions.

i_apply(E, Ctxt, Ren, Env, S) ->
    {Opnds, S1} = mapfoldl(fun (E, S) ->
                                   make_opnd(E, Ren, Env, S)
                           end,
                           S, apply_args(E)),

    %% Allocate a new app-context location and set up an application
    %% context structure containing the surrounding context.
    {L, S2} = st__new_app_loc(S1),
    Ctxt1 = #app{opnds = Opnds, ctxt = Ctxt, loc = L},

    %% Visit the operator expression in the new call context.
    {E1, S3} = i(apply_op(E), Ctxt1, Ren, Env, S2),

    %% Check the "inlined" flag to find out what to do next. (The store
    %% location could be recycled after the flag has been tested, but
    %% there is no real advantage to that, because in practice, only
    %% 4-5% of all created store locations will ever be reused, while
    %% there will be a noticable overhead for managing the free list.)
    case st__get_app_inlined(L, S3) of
        true ->
            %% The application was inlined, so we have the final
            %% expression in `E1'. We just have to handle any operands
            %% that need to be residualized for effect only (i.e., those
            %% the values of which are not used).
            residualize_operands(Opnds, E1, S3);
        false ->
            %% Otherwise, `E1' is the residual operator expression. We
            %% make sure all operands are visited, and rebuild the
            %% application.
            {Es, S4} = mapfoldl(fun (Opnd, S) ->
					visit_and_count_size(Opnd, S)
				end,
				S3, Opnds),
            Arity = length(Es),
            E2 = case is_c_fname(E1) andalso length(Es) =/= fname_arity(E1) of
                     true ->
                         V = new_var(Env),
                         ann_c_let(get_ann(E), [V], E1,
				   update_c_apply(E, V, Es));
                     false ->
                         update_c_apply(E, E1, Es)
                 end,
            N = apply_size(Arity),
            {E2, count_size(N, S4)}
    end.

apply_size(A) ->
    weight(apply) + weight(argument) * A.

%% Since it is not the task of this transformation to handle
%% cross-module inlining, all inter-module calls are handled by visiting
%% the components (the module and function name, and the arguments of
%% the call) for value. In `effect' context, if the function itself is
%% known to be completely effect free, the call can be discarded and the
%% arguments evaluated for effect. Otherwise, if all the visited
%% arguments are to constants, and the function is known to be safe to
%% execute at compile time, then we try to evaluate the call. If
%% evaluation completes normally, the call is replaced by the result;
%% otherwise the call is residualised.

i_call(E, Ctxt, Ren, Env, S) ->
    {M, S1} = i(call_module(E), value, Ren, Env, S),
    {F, S2} = i(call_name(E), value, Ren, Env, S1),
    As = call_args(E),
    Arity = length(As),

    %% Check if the name of the called function is static. If so,
    %% discard the size counts performed above, since the values will
    %% not cause any runtime cost.
    Static =  is_c_atom(M) and is_c_atom(F),
    S3 = case Static of
	     true ->
		 revert_size(S, S2);
	     false ->
		 S2
	 end,
    case Ctxt of
        effect when Static =:= true ->
            case is_safe_call(atom_val(M), atom_val(F), Arity) of
                true ->
                    %% The result will not be used, and the call is
                    %% effect free, so we create a multiple-value
                    %% aggregate containing the (not yet visited)
                    %% arguments and process that instead.
                    i(c_values(As), effect, Ren, Env, S3);
                false ->
                    %% We are not allowed to simply discard the call,
                    %% but we can try to evaluate it.
                    i_call_1(Static, M, F, Arity, As, E, Ctxt, Ren, Env,
                             S3)
            end;
        _ ->
	    i_call_1(Static, M, F, Arity, As, E, Ctxt, Ren, Env, S3)
    end.

i_call_1(Static, M, F, Arity, As, E, Ctxt, Ren, Env, S) ->
    %% Visit the arguments for value.
    {As1, S1} = mapfoldl(fun (X, A) -> i(X, value, Ren, Env, A) end, 
			 S, As),
    case Static of
	true ->
	    case erl_bifs:is_pure(atom_val(M), atom_val(F), Arity) of
		true ->
		    %% It is allowed to evaluate this at compile time.
		    case all_static(As1) of
			true ->
			    i_call_3(M, F, As1, E, Ctxt, Env, S1);
			false ->
			    %% See if the call can be rewritten instead.
			    i_call_4(M, F, As1, E, Ctxt, Env, S1)
		    end;
		false ->
		    i_call_2(M, F, As1, E, S1)
	    end;
	false ->
	    i_call_2(M, F, As1, E, S1)
    end.

%% Residualise the call.

i_call_2(M, F, As, E, S) ->
    N = weight(call) + weight(argument) * length(As),
    {update_c_call(E, M, F, As), count_size(N, S)}.

%% Attempt to evaluate the call to yield a literal; if that fails, try
%% to rewrite the expression.

i_call_3(M, F, As, E, Ctxt, Env, S) ->
    %% Note that we extract the results of argument expessions here; the
    %% expressions could still be sequences with side effects.
    Vs = [concrete(result(A)) || A <- As],
    try apply(atom_val(M), atom_val(F), Vs) of
	V ->
	    %% Evaluation completed normally - try to turn the result
	    %% back into a syntax tree (representing a literal).
	    case is_literal_term(V) of
		true ->
		    %% Make a sequence of the arguments (as a
		    %% multiple-value aggregate) and the final value.
		    S1 = count_size(weight(values), S),
		    S2 = count_size(weight(literal), S1),
		    {make_seq(c_values(As), abstract(V)), S2};
		false ->
		    %% The result could not be represented as a literal.
		    i_call_4(M, F, As, E, Ctxt, Env, S)
	    end
    catch
	error:_ ->
	    %% The evaluation attempt did not complete normally.
	    i_call_4(M, F, As, E, Ctxt, Env, S)
    end.

%% Rewrite the expression, if possible, otherwise residualise it.

i_call_4(M, F, As, E, Ctxt, Env, S) ->
    case reduce_bif_call(atom_val(M), atom_val(F), As, Env) of
        false ->
            %% Nothing more to be done - residualise the call.
            i_call_2(M, F, As, E, S);
        {true, E1} ->
            %% We revisit the result, because the rewriting might have
            %% opened possibilities for further inlining. Since the
            %% parts have already been visited once, we use the identity
            %% renaming here.
            i(E1, Ctxt, ren__identity(), Env, S)
    end.

%% For now, we assume that primops cannot be evaluated at compile time,
%% probably being too special. Also, we have no knowledge about their
%% side effects.

i_primop(E, Ren, Env, S) ->
    %% Visit the arguments for value.
    {As, S1} = mapfoldl(fun (E, S) ->
				i(E, value, Ren, Env, S)
			end,
			S, primop_args(E)),
    N = weight(primop) + weight(argument) * length(As),
    {update_c_primop(E, primop_name(E), As), count_size(N, S1)}.

%% This is like having an expression with an extra fun-expression
%% attached for "exceptional cases"; actually, there are exactly two
%% parameter variables for the body, but they are easiest handled as if
%% their number might vary, just as for a `fun'.

i_try(E, Ctxt, Ren, Env, S) ->
    %% The argument expression is evaluated in `value' context, and the
    %% surrounding context is propagated into both branches. We do not
    %% try to recognize cases when the protected expression will
    %% actually raise an exception. Note that the variables are visited
    %% as patterns.
    {A, S1} = i(try_arg(E), value, Ren, Env, S),
    Vs = try_vars(E),
    {_, Ren1, Env1, S2} = bind_locals(Vs, Ren, Env, S1),
    Vs1 = i_params(Vs, Ren1, Env1),
    {B, S3} = i(try_body(E), Ctxt, Ren1, Env1, S2),
    case is_safe(A) of
	true ->
	    %% The `try' wrapper can be dropped in this case. Since the
	    %% expressions have been visited already, the identity
	    %% renaming is used when we revisit the new let-expression.
	    i(c_let(Vs1, A, B), Ctxt, ren__identity(), Env, S3);
	false ->
	    Evs = try_evars(E),
	    {_, Ren2, Env2, S4} = bind_locals(Evs, Ren, Env, S3),
	    Evs1 = i_params(Evs, Ren2, Env2),
	    {H, S5} = i(try_handler(E), Ctxt, Ren2, Env2, S4),
	    S6 = count_size(weight('try'), S5),
	    {update_c_try(E, A, Vs1, B, Evs1, H), S6}
    end.

%% A special case of try-expressions:

i_catch(E, Ctxt, Ren, Env, S) ->
    %% We cannot propagate application contexts into the catch.
    {E1, S1} = ES1 = i(catch_body(E), safe_context(Ctxt), Ren, Env, S),
    case is_safe(E1) of
	true ->
	    %% The `catch' wrapper can be dropped in this case.
	    ES1;
	false ->
	    S2 = count_size(weight('catch'), S1),
	    {update_c_catch(E, E1), S2}
    end.

%% A receive-expression is very much like a case-expression, with the
%% difference that we do not have access to a switch expression, since
%% the value being switched on is taken from the mailbox. The fact that
%% the receive-expression may iterate over an arbitrary number of
%% messages is not of interest to us. All we can do here is to visit its
%% subexpressions, and possibly eliminate definitely unselectable
%% clauses.

i_receive(E, Ctxt, Ren, Env, S) ->
    %% We first visit the expiry expression (for value) and the expiry
    %% body (in the surrounding context).
    {T, S1} = i(receive_timeout(E), value, Ren, Env, S),
    {B, S2} = i(receive_action(E), Ctxt, Ren, Env, S1),

    %% Then we visit the clauses. Note that application contexts may not
    %% in general be propagated into the branches (and the expiry body),
    %% because the execution of the `receive' may remove a message from
    %% the mailbox as a side effect; the situation is thus analogous to
    %% that in a `case' expression.
    Ctxt1 = safe_context(Ctxt),
    case i_clauses(receive_clauses(E), Ctxt1, Ren, Env, S2) of
        {false, {[], _, _, Cs}, S3} ->
            %% We still have a list of clauses. If the list is empty,
            %% and the expiry expression is the integer zero, the
            %% expression reduces to the expiry body.
	    if Cs =:= [] ->
		    case is_c_int(T) andalso (int_val(T) =:= 0) of
			true ->
			    {B, S3};
			false ->
			    i_receive_1(E, Cs, T, B, S3)
		    end;
	       true ->
		    i_receive_1(E, Cs, T, B, S3)
	    end;
        {true, {_, _, _, Cs}, S3} ->
	    %% Cs is a single clause that will always be matched (if a
	    %% message exists), but we must keep the `receive' statement
	    %% in order to fetch the message from the mailbox.
	    i_receive_1(E, Cs, T, B, S3)
    end.

i_receive_1(E, Cs, T, B, S) ->
    %% Here, we just add the base sizes for the receive-expression
    %% itself and for each remaining clause; cf. `case'.
    N = weight('receive') + weight(clause) * length(Cs),
    {update_c_receive(E, Cs, T, B), count_size(N, S)}.

%% A module definition is like a `letrec', with some add-ons (export and
%% attribute declarations) but without an explicit body. Actually, the
%% exporting of function names has the same effect as if there was a
%% body consisting of the list of references to the exported functions.
%% Thus, the exported functions are exactly those which can be
%% referenced from outside the module.

i_module(E, Ctxt, Ren, Env, S) ->
    %% Cf. `i_letrec'. Note that we pass a dummy constant value for the
    %% "body" parameter.
    Exps = i_module_exports(E),
    {Es, _, Xs1, S1} = i_letrec(module_defs(E), void(),
                                Exps, Ctxt, Ren, Env, false, S),
    %% Sanity check:
    case Es of
        [] ->
            report_warning("no function definitions remaining "
			   "in module `~s'.\n",
			   [atom_name(module_name(E))]);
        _ ->
            ok
    end,
    E1 = update_c_module(E, module_name(E), Xs1, module_attrs(E), Es),
    {E1, count_size(weight(module), S1)}.

i_module_exports(E) ->
    %% If a function is named in an `on_load' attribute, we will
    %% pretend that it is exported to ensure that it will not be removed.
    Exps = module_exports(E),
    Attrs = module_attrs(E),
    case i_module_on_load(Attrs) of
	none ->
	    Exps;
	[{_,_}=FA] ->
	    ordsets:add_element(c_var(FA), Exps)
    end.

i_module_on_load([{Key,Val}|T]) ->    
    case concrete(Key) of
	on_load ->
	    concrete(Val);
	_ ->
	    i_module_on_load(T)
    end;
i_module_on_load([]) -> none.

%% Binary-syntax expressions are too complicated to do anything
%% interesting with here - that is beyond the scope of this program;
%% also, their construction could have side effects, so even in effect
%% context we can't remove them. (We don't bother to identify cases of
%% "safe" unused binaries which could be removed.)

i_binary(E, Ren, Env, S) ->
    %% Visit the segments for value.
    {Es, S1} = mapfoldl(fun (E, S) ->
				i_bitstr(E, Ren, Env, S)
			end,
			S, binary_segments(E)),
    S2 = count_size(weight(binary), S1),
    {update_c_binary(E, Es), S2}.

i_bitstr(E, Ren, Env, S) ->
    %% It is not necessary to visit the Unit, Type and Flags fields,
    %% since these are always literals.
    {Val, S1} = i(bitstr_val(E), value, Ren, Env, S),
    {Size, S2} = i(bitstr_size(E), value, Ren, Env, S1),
    Unit = bitstr_unit(E),
    Type = bitstr_type(E),
    Flags = bitstr_flags(E),
    S3 = count_size(weight(bitstr), S2),
    {update_c_bitstr(E, Val, Size, Unit, Type, Flags), S3}.

i_map(E, Ctx, Ren, Env, S0) ->
    %% Visit the segments for value.
    {M1, S1} = i(map_arg(E), value, Ren, Env, S0),
    {Es, S2} = mapfoldl(fun (E, S) ->
		i_map_pair(E, Ctx, Ren, Env, S)
	end, S1, map_es(E)),
    S3 = count_size(weight(map), S2),
    {update_c_map(E, M1,Es), S3}.

i_map_pair(E, Ctx, Ren, Env, S0) ->
    %% It is not necessary to visit the Op field
    %% since it is always a literal.
    {Key, S1} = i(map_pair_key(E), value, Ren, Env, S0),
    {Val, S2} = i(map_pair_val(E), Ctx, Ren, Env, S1),
    Op = map_pair_op(E),
    S3 = count_size(weight(map_pair), S2),
    {update_c_map_pair(E, Op, Key, Val), S3}.


%% This is a simplified version of `i_pattern', for lists of parameter
%% variables only. It does not modify the state.

i_params([V | Vs], Ren, Env) ->
    Name = ren__map(var_name(V), Ren),
    case env__lookup(Name, Env) of
	{ok, R} ->
	    [ref_to_var(R) | i_params(Vs, Ren, Env)];
	error ->
	    report_internal_error("variable `~w' not bound "
				  "in pattern.\n", [Name]),
	    exit(error)
    end;
i_params([], _, _) ->
    [].

%% For ordinary patterns, we just visit to rename variables and count
%% the size/cost. All occurring binding instances of variables should
%% already have been added to the renaming and environment; however, to
%% handle the size expressions of binary-syntax patterns, we must pass
%% the renaming and environment of the containing expression

i_pattern(E, Ren, Env, Ren0, Env0, S) ->
    case type(E) of
	var ->
	    %% Count no size.
            Name = ren__map(var_name(E), Ren),
            case env__lookup(Name, Env) of
                {ok, R} ->
                    {ref_to_var(R), S};
                error ->
                    report_internal_error("variable `~w' not bound "
					  "in pattern.\n", [Name]),
		    exit(error)
            end;
	alias ->
	    %% Count no size.
	    V = alias_var(E),
	    Name = ren__map(var_name(V), Ren),
	    case env__lookup(Name, Env) of
		{ok, R} ->
		    %% Visit the subpattern and recompose.
		    V1 = ref_to_var(R),
		    {P, S1} = i_pattern(alias_pat(E), Ren, Env, Ren0,
					Env0, S),
		    {update_c_alias(E, V1, P), S1};
		error ->
		    report_internal_error("variable `~w' not bound "
					  "in pattern.\n", [Name]),
		    exit(error)
	    end;
	binary ->
	    {Es, S1} = mapfoldl(fun (E, S) ->
					i_bitstr_pattern(E, Ren, Env,
							  Ren0, Env0, S)
				end,
				S, binary_segments(E)),
	    S2 = count_size(weight(binary), S1),
	    {update_c_binary(E, Es), S2};
	map ->
	    {Es, S1} = mapfoldl(fun (E, S) ->
			i_map_pair_pattern(E, Ren, Env, Ren0, Env0, S)
		end, S, map_es(E)),
	    S2 = count_size(weight(map), S1),
	    {update_c_map(E, map_arg(E), Es), S2};
	_ ->
	    case is_literal(E) of
		true ->
                    {E, count_size(weight(literal), S)};
		false ->
		    {Es1, S1} = mapfoldl(fun (E, S) ->
						 i_pattern(E, Ren, Env,
							   Ren0, Env0,
							   S)
					 end,
					 S, data_es(E)),
		    %% We assume that in general, the elements of the
		    %% constructor will all be fetched.
		    N = weight(data) + length(Es1) * weight(element),
		    S2 = count_size(N, S1),
		    {update_data(E, data_type(E), Es1), S2}
	    end
    end.

i_bitstr_pattern(E, Ren, Env, Ren0, Env0, S) ->
    %% It is not necessary to visit the Unit, Type and Flags fields,
    %% since these are always literals. The Value field is a limited
    %% pattern - either a literal or an unbound variable. The Size field
    %% is a limited expression - either a literal or a variable bound in
    %% the environment of the containing expression.
    {Val, S1} = i_pattern(bitstr_val(E), Ren, Env, Ren0, Env0, S),
    {Size, S2} = i(bitstr_size(E), value, Ren0, Env0, S1),
    Unit = bitstr_unit(E),
    Type = bitstr_type(E),
    Flags = bitstr_flags(E),
    S3 = count_size(weight(bitstr), S2),
    {update_c_bitstr(E, Val, Size, Unit, Type, Flags), S3}.

i_map_pair_pattern(E, Ren, Env, Ren0, Env0, S) ->
    %% It is not necessary to visit the Op it is always a literal.
    %% Key is an expression
    {Key, S1} = i(map_pair_key(E), value, Ren0, Env0, S),
    {Val, S2} = i_pattern(map_pair_val(E), Ren, Env, Ren0, Env0, S1),
    Op = map_pair_op(E), %% should be 'exact' literal
    S3 = count_size(weight(map_pair), S2),
    {update_c_map_pair(E, Op, Key, Val), S3}.


%% ---------------------------------------------------------------------
%% Other central inlining functions

%% The following function assumes that `E' is a fun-expression and the
%% context is an app-structure. If the inlining could be aborted, a
%% corresponding catch should be set up before entering the function.
%%
%% Note: if the inlined body is some lambda abstraction, and the
%% surrounding context of the app-context is also an app-context, the
%% `inlined' flag of the outermost context will be set before that of
%% the inner context is set. E.g.: `let F = fun (X) -> fun (Y) -> E in
%% apply apply F(A)(B)' will propagate the body of F, which is a lambda
%% abstraction, into the outer application context, which will be
%% inlined to produce expression `E', and the flag of the outer context
%% will be set. Upon return, the flag of the inner context will also be
%% set. However, the flags are then tested in innermost-first order.
%% Thus, if some inlining attempt is aborted, the `inlined' flags of any
%% nested app-contexts must be cleared.
%%
%% This implementation does nothing to handle inlining of calls to
%% recursive functions in a smart way. This means that as long as the
%% size and effort counters do not prevent it, the function body will be
%% inlined (i.e., the first iteration will be unrolled), and the
%% recursive calls will be residualized.

inline(E, #app{opnds = Opnds, ctxt = Ctxt, loc = L}, Ren, Env, S) ->
    %% Check that the arities match:
    Vs = fun_vars(E),
    if length(Opnds) =/= length(Vs) ->
	    %% Arity mismatch: the call will be residualized
	    {E, S};
       true ->
	    %% Create local bindings for the parameters to their
	    %% respective operand structures from the app-structure.
	    {Rs, Ren1, Env1, S1} = bind_locals(Vs, Opnds, Ren, Env, S),

	    %% Visit the body in the context saved in the structure.
	    {E1, S2} = i(fun_body(E), Ctxt, Ren1, Env1, S1),

	    %% Create necessary bindings and/or set flags.
	    {E2, S3} = make_let_bindings(Rs, E1, S2),

	    %% Lastly, flag the application as inlined, since the inlining
	    %% attempt was not aborted before we reached this point.
	    {E2, st__set_app_inlined(L, S3)}
    end.

%% For the (possibly renamed) argument variables to an inlined call,
%% either create `let' bindings for them, if they are still referenced
%% in the residual expression (in C/Lisp, also if they are assigned to),
%% or otherwise (if they are not referenced or assigned) mark them for
%% evaluation for side effects.

make_let_bindings([R | Rs], E, S) ->
    {E1, S1} = make_let_bindings(Rs, E, S),
    make_let_binding(R, E1, S1);
make_let_bindings([], E, S) ->
    {E, S}.

make_let_binding(R, E, S) ->
    %% The `referenced' flag is conservatively computed. We therefore
    %% first check some simple cases where parameter R is definitely not
    %% referenced in the resulting body E.
    case is_literal(E) of
        true ->
            %% A constant contains no variable references.
            make_let_binding_1(R, E, S);
        false ->
            case is_c_var(E) of
                true ->
                    case var_name(E) =:= R#ref.name of
                        true ->
                            %% The body is simply the parameter variable
                            %% itself. Visit the operand for value and
                            %% substitute the result for the body.
                            visit_and_count_size(R#ref.opnd, S);
                        false ->
                            %% Not the same variable, so the parameter
                            %% is not referenced at all.
                            make_let_binding_1(R, E, S)
                    end;
                false ->
		    %% Proceed to check the `referenced' flag.
		    case st__get_var_referenced(R#ref.loc, S) of
			true ->
			    %% The parameter is probably referenced in
			    %% the residual code (although it might not
			    %% be). Visit the operand for value and
			    %% create a let-binding.
			    {E1, S1} = visit_and_count_size(R#ref.opnd,
							    S),
			    S2 = count_size(weight('let'), S1),
			    {c_let([ref_to_var(R)], E1, E), S2};
			false ->
			    %% The parameter is definitely not
			    %% referenced.
			    make_let_binding_1(R, E, S)
		    end
	    end
    end.

%% This marks the operand for evaluation for effect.

make_let_binding_1(R, E, S) ->
    Opnd = R#ref.opnd,
    {E, st__set_opnd_effect(Opnd#opnd.loc, S)}.

%% Here, `R' is the ref-structure which is the target of the copy
%% propagation, and `Opnd' is a visited operand structure, to be
%% propagated through `R' if possible - if not, `R' is residualised.
%% `Opnd' is normally the operand that `R' is bound to, and `E' is the
%% result of visiting `Opnd' for value; we pass this as an argument so
%% we don't have to fetch it multiple times (because we don't have
%% constant time access).
%%
%% We also pass the environment of the site of the variable reference,
%% for use when inlining a propagated fun-expression. In the original
%% algorithm by Waddell, the environment used for inlining such cases is
%% the identity mapping, because the fun-expression body has already
%% been visited for value, and their algorithm combines renaming of
%% source-code variables with the looking up of information about
%% residual-code variables. We, however, need to check the environment
%% of the call site when creating new non-shadowed variables, but we
%% must avoid repeated renaming. We therefore separate the renaming and
%% the environment (as in the renaming algorithm of Peyton-Jones and
%% Marlow). This also makes our implementation more general, compared to
%% the original algorithm, because we do not give up on propagating
%% variables that were free in the fun-body.
%%
%%  Example:
%%
%%	let F = fun (X) -> {'foo', X} in
%%	let G = fun (H) -> apply H(F)        % F is free in the fun G
%%	in apply G(fun (F) -> apply F(42))
%%	  =>
%%	let F = fun (X) -> {'foo', X} in
%%	apply (fun (H) -> apply H(F))(fun (F) -> apply F(42))
%%	  =>
%%	let F = fun (X) -> {'foo', X} in
%%	apply (fun (F) -> apply F(42))(F)
%%	  =>
%%	let F = fun (X) -> {'foo', X} in
%%	apply F(42)
%%	  =>
%%	apply (fun (X) -> {'foo', X})(2)
%%	  =>
%%	{'foo', 42}
%%
%%  The original algorithm would give up at stage 4, because F was free
%%  in the propagated fun-expression. Our version inlines this example
%%  completely.

copy(R, Opnd, E, Ctxt, Env, S) ->
    case is_c_var(E) andalso not is_c_fname(E) of
        true ->
	    %% The operand reduces to another variable - get its
	    %% ref-structure and attempt to propagate further.
            copy_var(env__get(var_name(E), Opnd#opnd.env), Ctxt, Env,
                     S);
        false ->
            %% Apart from variables and functional values (the latter
            %% are handled by `copy_1' below), only constant literals
            %% are copyable in general; other things, including e.g.
            %% tuples `{foo, X}', could cause duplication of work, and
            %% are not copy propagated.
            case is_literal(E) of
                true ->
                    {E, count_size(weight(literal), S)};
                false ->
                    copy_1(R, Opnd, E, Ctxt, Env, S)
            end
    end.

copy_var(R, Ctxt, Env, S) ->
    %% (In Lisp or C, if this other variable might be assigned to, we
    %% should residualize the "parent" instead, so we don't bypass any
    %% destructive updates.)
    case R#ref.opnd of
        undefined ->
            %% This variable is not bound to an expression, so just
            %% residualize it.
            residualize_var(R, S);
        Opnd ->
	    %% Note that because operands are always visited before
	    %% copied, all copyable operand expressions will be
	    %% propagated through any number of bindings. If `R' was
	    %% bound to a constant literal, we would never have reached
	    %% this point.
            case st__lookup_opnd_cache(Opnd#opnd.loc, S) of
                error ->
                    %% The result for this operand is not yet ready
                    %% (which should mean that it is a recursive
                    %% reference). Thus, we must residualise the
                    %% variable.
                    residualize_var(R, S);
                {ok, #cache{expr = E1}} ->
                    %% The result for the operand is ready, so we can
                    %% proceed to propagate it.
                    copy_1(R, Opnd, E1, Ctxt, Env, S)
            end
    end.

copy_1(R, #opnd{no_inline = true}, _E, _Ctxt, _Env, S) ->
    residualize_var(R, S);
copy_1(R, Opnd, E, Ctxt, Env, S) ->
    case type(E) of
        'fun' ->
            %% Fun-expression (lambdas) are a bit special; they are copyable,
            %% but should preferably not be duplicated, so they should not be
            %% copy propagated except into application contexts, where they can
            %% be inlined.
            case Ctxt of
                #app{} ->
                    %% First test if the operand is "outer-pending"; if
                    %% so, don't inline.
                    case st__test_outer_pending(Opnd#opnd.loc, S) of
                        false ->
                            copy_inline(R, Opnd, E, Ctxt, Env, S);
                        true ->
                            %% Cyclic reference forced inlining to stop
                            %% (avoiding infinite unfolding).
                            residualize_var(R, S)
                    end;
                _ ->
                    residualize_var(R, S)
            end;
        var ->
            %% Variables at this point only refer to local functions; they are
            %% copyable but can't appear in guards, so they should not be
            %% copy propagated except into application contexts, where they can
            %% be inlined.
            case Ctxt of
                #app{} ->
                    %% First test if the operand is "outer-pending"; if
                    %% so, don't inline.
                    case st__test_outer_pending(Opnd#opnd.loc, S) of
                        false ->
                            R1 = env__get(var_name(E), Opnd#opnd.env),
                            copy_var(R1, Ctxt, Env, S);
                        true ->
                            %% Cyclic reference forced inlining to stop
                            %% (avoiding infinite unfolding).
                            residualize_var(R, S)
                    end;
                _ ->
                    residualize_var(R, S)
            end;
        _ ->
            %% We have no other cases to handle here
            residualize_var(R, S)
    end.

%% This inlines a function value that was propagated to an application
%% context. The inlining is done with an identity renaming (since the
%% expression is already visited) but in the environment of the call
%% site (which is OK because of the no-shadowing strategy for renaming,
%% and because the domain of our environments are the residual-program
%% variables instead of the source-program variables). Note that we must
%% first set the "outer-pending" flag, and clear it afterwards.

copy_inline(R, Opnd, E, Ctxt, Env, S) ->
    S1 = st__mark_outer_pending(Opnd#opnd.loc, S),
    try copy_inline_1(R, E, Ctxt, Env, S1) of
        {E1, S2} ->
            {E1, st__clear_outer_pending(Opnd#opnd.loc, S2)}
    catch
        throw:X ->
 	    %% If we use destructive update for the `outer-pending'
 	    %% flag, we must make sure to clear it upon a nonlocal
 	    %% return.
	    _S2 = st__clear_outer_pending(Opnd#opnd.loc, S1),
            throw(X)
    end.

%% If the current effort counter was passive, we use a new active effort
%% counter with the inherited limit for this particular inlining.

copy_inline_1(R, E, Ctxt, Env, S) ->
    case effort_is_active(S) of
        true ->
            copy_inline_2(R, E, Ctxt, Env, S);
        false ->
            S1 = new_active_effort(get_effort_limit(S), S),
            try copy_inline_2(R, E, Ctxt, Env, S1) of
                {E1, S2} ->
                    %% Revert to the old effort counter.
                    {E1, revert_effort(S, S2)}
	    catch
                throw:{counter_exceeded, effort, _} ->
                    %% Aborted this inlining attempt because too much
                    %% effort was spent. Residualize the variable and
                    %% revert to the previous state.
                    residualize_var(R, S)
            end
    end.

%% Regardless of whether the current size counter is active or not, we
%% use a new active size counter for each inlining. If the current
%% counter was passive, the new counter gets the inherited size limit;
%% if it was active, the size limit of the new counter will be equal to
%% the remaining budget of the current counter (which itself is not
%% affected by the inlining). This distributes the size budget more
%% evenly over "inlinings within inlinings", so that the whole size
%% budget is not spent on the first few call sites (in an inlined
%% function body) forcing the remaining call sites to be residualised.

copy_inline_2(R, E, Ctxt, Env, S) ->
    Limit = case size_is_active(S) of
                true ->
                    get_size_limit(S) - get_size_value(S);
                false ->
                    get_size_limit(S)
            end,
    %% Add the cost of the application to the new size limit, so we
    %% always inline functions that are small enough, even if `Limit' is
    %% close to zero at this point. (This is an extension to the
    %% original algorithm.)
    S1 = new_active_size(Limit + apply_size(length(Ctxt#app.opnds)), S),
    try inline(E, Ctxt, ren__identity(), Env, S1) of
        {E1, S2} ->
            %% Revert to the old size counter.
            {E1, revert_size(S, S2)}
    catch
        throw:{counter_exceeded, size, S2} ->
            %% Aborted this inlining attempt because it got too big.
            %% Residualize the variable and revert to the old size
            %% counter. (It is important that we do not also revert the
            %% effort counter here. Because the effort and size counters
            %% are always set up together, we know that the effort
            %% counter returned in S2 is the same that was passed to
            %% `inline'.)
	    S3 = revert_size(S, S2),
  	    %% If we use destructive update for the `inlined' flag, we
  	    %% must make sure to clear the flags of any nested
  	    %% app-contexts upon aborting; see `inline' for details.
 	    S4 = reset_nested_apps(Ctxt, S3),    % for effect
            residualize_var(R, S4)
    end.

reset_nested_apps(#app{ctxt = Ctxt, loc = L}, S) ->
    reset_nested_apps(Ctxt, st__clear_app_inlined(L, S));
reset_nested_apps(_, S) ->
    S.


%% ---------------------------------------------------------------------
%%	Support functions

new_var(Env) ->
    Name = env__new_vname(Env),
    c_var(Name).

%% The way a template variable is used makes it necessary
%% to make sure that it is unique in the entire function.
%% Therefore, template variables are atoms with the prefix "@i".

new_template_var(Env) ->
    Name = env__new_tname(Env),
    c_var(Name).

residualize_var(R, S) ->
    S1 = count_size(weight(var), S),
    {ref_to_var(R), st__set_var_referenced(R#ref.loc, S1)}.

%% This function returns the value-producing subexpression of any
%% expression. (Except for sequencing expressions, this is the
%% expression itself.)

result(E) ->
    case is_c_seq(E) of
        true ->
            %% Also see `make_seq', which is used in all places to build
            %% sequences so that they are always nested in the first
            %% position.
            seq_body(E);
        false ->
            E
    end.

%% This function rewrites E to `do A1 E' if A is `do A1 A2', and
%% otherwise returns E unchanged.

hoist_effects(A, E) ->
    case type(A) of
	seq -> make_seq(seq_arg(A), E);
	_ -> E
    end.

%% This "build sequencing expression" operation assures that sequences
%% are always nested in the first position, which makes it easy to find
%% the actual value-producing expression of a sequence (cf. `result').

make_seq(E1, E2) ->
    case is_safe(E1) of
        true ->
            %% The first expression can safely be dropped.
            E2;
        false ->
            %% If `E1' is a sequence whose final expression has no side
            %% effects, then we can lose *that* expression when we
            %% compose the new sequence, since its value will not be
            %% used.
            E3 = case is_c_seq(E1) of
                     true ->
                         case is_safe(seq_body(E1)) of
                             true ->
                                 %% Drop the final expression.
                                 seq_arg(E1);
                             false ->
                                 E1
                         end;
                     false ->
                         E1
                 end,
            case is_c_seq(E2) of
                true ->
                    %% `E2' is a sequence (E2' E2''), so we must
                    %% rearrange the nesting to ((E1, E2') E2''), to
                    %% preserve the invariant. Annotations on `E2' are
                    %% lost.
                    c_seq(c_seq(E3, seq_arg(E2)), seq_body(E2));
                false ->
                    c_seq(E3, E2)
            end
    end.

%% Currently, safe expressions include variables, lambda expressions,
%% constructors with safe subexpressions (this includes atoms, integers,
%% empty lists, etc.), seq-, let- and letrec-expressions with safe
%% subexpressions, try- and catch-expressions with safe subexpressions
%% and calls to safe functions with safe argument subexpressions.
%% Binaries seem too tricky to be considered.

is_safe(E) ->
    case is_data(E) of
        true ->
	    is_safe_list(data_es(E));
        false ->
            case type(E) of
                var ->
                    true;
                'fun' ->
                    true;
		values ->
		    is_safe_list(values_es(E));
                'seq' ->
                    is_safe(seq_arg(E)) andalso is_safe(seq_body(E));
                'let' ->
                    is_safe(let_arg(E)) andalso is_safe(let_body(E));
                letrec ->
                    is_safe(letrec_body(E));
		'try' ->
		    %% If the argument expression is not safe, it could
		    %% be modifying the state; thus, even if the body is
		    %% safe, the try-expression as a whole would not be.
		    %% If the argument is safe, the handler is not used.
                    is_safe(try_arg(E)) andalso is_safe(try_body(E));
		'catch' ->
                    is_safe(catch_body(E));
		call ->
		    M = call_module(E),
		    F = call_name(E),
		    case is_c_atom(M) andalso is_c_atom(F) of
			true ->
			    As = call_args(E),
			    is_safe_list(As) andalso
				is_safe_call(atom_val(M),
					     atom_val(F),
					     length(As));
			false ->
			    false
		    end;
                _ ->
                    false
            end
    end.

is_safe_list([E | Es]) ->
    case is_safe(E) of
	true ->
	    is_safe_list(Es); 
	false ->
	    false
    end;
is_safe_list([]) ->
    true.

is_safe_call(M, F, A) ->
    erl_bifs:is_safe(M, F, A).

%% When setting up local variables, we only create new names if we have
%% to, according to the "no-shadowing" strategy.

make_locals(Vs, Ren, Env) ->
    make_locals(Vs, [], Ren, Env).

make_locals([V | Vs], As, Ren, Env) ->
    Name = var_name(V),
    case env__is_defined(Name, Env) of
        false ->
            %% The variable need not be renamed. Just make sure that the
            %% renaming will map it to itself.
            Name1 = Name,
            Ren1 = ren__add_identity(Name, Ren);
        true ->
            %% The variable must be renamed to maintain the no-shadowing
            %% invariant. Do the right thing for function variables.
            Name1 = case Name of
			{A, N} ->
			    env__new_fname(A, N, Env);
			_ ->
			    env__new_vname(Env)
		    end,
            Ren1 = ren__add(Name, Name1, Ren)
    end,
    %% This temporary binding is added for correct new-key generation.
    Env1 = env__bind(Name1, dummy, Env),
    make_locals(Vs, [Name1 | As], Ren1, Env1);
make_locals([], As, Ren, Env) ->
    {reverse(As), Ren, Env}.

%% This adds let-bindings for the source code variables in `Es' to the
%% environment `Env'.
%%
%% Note that we always assign a new state location for the
%% residual-program variable, since we cannot know when a location for a
%% particular variable in the source code can be reused.

bind_locals(Vs, Ren, Env, S) ->
    Opnds = [undefined || _ <- Vs],
    bind_locals(Vs, Opnds, Ren, Env, S).

bind_locals(Vs, Opnds, Ren, Env, S) ->
    {Ns, Ren1, Env1} = make_locals(Vs, Ren, Env),
    {Rs, Env2, S1} = bind_locals_1(Ns, Opnds, [], Env1, S),
    {Rs, Ren1, Env2, S1}.

%% Note that the `Vs' are currently not used for anything except the
%% number of variables. If we were maintaining "source-referenced"
%% flags, then the flag in the new variable should be initialized to the
%% current value of the (residual-) referenced-flag of the "parent".

bind_locals_1([N | Ns], [Opnd | Opnds], Rs, Env, S) ->
    {R, S1} = new_ref(N, Opnd, S),
    Env1 = env__bind(N, R, Env),
    bind_locals_1(Ns, Opnds, [R | Rs], Env1, S1);
bind_locals_1([], [], Rs, Env, S) ->
    {lists:reverse(Rs), Env, S}.

new_refs(Ns, Opnds, S) ->
    new_refs(Ns, Opnds, [], S).

new_refs([N | Ns], [Opnd | Opnds], Rs, S) ->
    {R, S1} = new_ref(N, Opnd, S),
    new_refs(Ns, Opnds, [R | Rs], S1);
new_refs([], [], Rs, S) ->
    {lists:reverse(Rs), S}.

new_ref(N, Opnd, S) ->
    {L, S1} = st__new_ref_loc(S),
    {#ref{name = N, opnd = Opnd, loc = L}, S1}.

%% This adds recursive bindings for the source code variables in `Es' to
%% the environment `Env'. Note that recursive binding of a set of
%% variables is an atomic operation on the environment - they cannot be
%% added one at a time.

bind_recursive(Vs, Opnds, Ren, Env, S) ->
    {Ns, Ren1, Env1} = make_locals(Vs, Ren, Env),
    {Rs, S1} = new_refs(Ns, Opnds, S),

    %% When this fun-expression is evaluated, it updates the operand
    %% structure in the ref-structure to contain the recursively defined
    %% environment and the correct renaming.
    Fun = fun (R, Env) ->
		  Opnd = R#ref.opnd,
		  R#ref{opnd = Opnd#opnd{ren = Ren1, env = Env}}
	  end,
    {Rs, Ren1, env__bind_recursive(Ns, Rs, Fun, Env1), S1}.

safe_context(Ctxt) ->
    case Ctxt of
        #app{} ->
            value;
        _ ->
            Ctxt
    end.

%% Note that the name of a variable encodes its type: a "plain" variable
%% or a function variable. The latter kind also contains an arity number
%% which should be preserved upon renaming.

ref_to_var(#ref{name = Name}) ->
    %% If we were maintaining "source-referenced" flags, the annotation
    %% `add_ann([#source_ref{loc = L}], E)' should also be done here, to
    %% make the algorithm reapplicable. This is however not necessary
    %% since there are no destructive variable assignments in Erlang.
    c_var(Name).

%% Including the effort counter of the call site assures that the cost
%% of processing an operand via `visit' is charged to the correct
%% counter. In particular, if the effort counter of the call site was
%% passive, the operands will also be processed with a passive counter.

make_opnd(E, Ren, Env, S) ->
    make_opnd(E, Ren, Env, false, S).

make_opnd(E, Ren, Env, NoInline, S) ->
    {L, S1} = st__new_opnd_loc(S),
    C = st__get_effort(S1),
    Opnd = #opnd{expr = E, ren = Ren, env = Env, loc = L,
                 effort = C, no_inline = NoInline},
    {Opnd, S1}.

keep_referenced(Rs, S) ->
    [R || R <- Rs, st__get_var_referenced(R#ref.loc, S)].

residualize_operands(Opnds, E, S) ->
    foldr(fun (Opnd, {E, S}) -> residualize_operand(Opnd, E, S) end,
          {E, S}, Opnds).

%% This is the only case where an operand expression can be visited in
%% `effect' context instead of `value' context.

residualize_operand(Opnd, E, S) ->
    case st__get_opnd_effect(Opnd#opnd.loc, S) of
        true ->
            %% The operand has not been visited, so we do that now, but
            %% in `effect' context. (Waddell's algoritm does some stuff
            %% here to account specially for the operand size, which
            %% appears unnecessary.)
            {E1, S1} = i(Opnd#opnd.expr, effect, Opnd#opnd.ren,
                         Opnd#opnd.env, S),
            {make_seq(E1, E), S1};
        false ->
            {E, S}
    end.

%% The `visit' function always visits the operand expression in `value'
%% context (`residualize_operand' visits an unreferenced operand
%% expression in `effect' context when necessary). A new passive size
%% counter is used for visiting the operand, the final value of which is
%% then cached along with the resulting expression.
%%
%% Note that the effort counter of the call site, included in the
%% operand structure, is not a shared object. Thus, the effort budget is
%% actually reused over all occurrences of the operands of a single
%% application. This does not appear to be a problem; just a
%% modification of the algorithm.

visit(Opnd, S) ->
    {C, S1} = visit_1(Opnd, S),
    {C#cache.expr, S1}.

visit_and_count_size(Opnd, S) ->
    {C, S1} = visit_1(Opnd, S),
    {C#cache.expr, count_size(C#cache.size, S1)}.

visit_1(Opnd, S) ->
    case st__lookup_opnd_cache(Opnd#opnd.loc, S) of
        error ->
            %% Use a new, passive, size counter for visiting operands,
            %% and use the effort counter of the context of the operand.
            %% It turns out that if the latter is active, it must be the
            %% same object as the one currently used, and if it is
            %% passive, it does not matter if it is the same object as
            %% any other counter.
	    Effort = Opnd#opnd.effort,
	    Active = counter__is_active(Effort),
	    S1 = case Active of
		     true ->
			 S;    % don't change effort counter
		     false ->
			 st__set_effort(Effort, S)
		 end,
	    S2 = new_passive_size(get_size_limit(S1), S1),
	    
            %% Visit the expression and cache the result, along with the
            %% final value of the size counter.
            {E, S3} = i(Opnd#opnd.expr, value, Opnd#opnd.ren,
                        Opnd#opnd.env, S2),
            Size = get_size_value(S3),
            C = #cache{expr = E, size = Size},
            S4 = revert_size(S, st__set_opnd_cache(Opnd#opnd.loc, C,
						   S3)),
	    case Active of
		true ->
		    {C, S4};  % keep using the same effort counter
		false ->
		    {C, revert_effort(S, S4)}
	    end;
	{ok, C} ->
            {C, S}
    end.

%% Create a pattern matching template for an expression. A template
%% contains only data constructors (including atomic ones) and
%% variables, and compound literals are not folded into a single node.
%% Each node in the template is annotated with the variable which holds
%% the corresponding subexpression; these are new, unique variables not
%% existing in the given `Env'. Returns `{Template, Variables, NewEnv}',
%% where `Variables' is the list of all variables corresponding to nodes
%% in the template *listed in reverse dependency order*, and `NewEnv' is
%% `Env' augmented with mappings from the variable names to
%% subexpressions of `E' (not #ref{} structures!) rewritten so that no
%% computations are duplicated. `Variables' is guaranteed to be nonempty
%% - at least the root node will always be bound to a new variable.

make_template(E, Env) ->
    make_template(E, [], Env).

make_template(E, Vs0, Env0) ->
    case is_data(E) of
	true ->
	    {Ts, {Vs1, Env1}} = mapfoldl(
				  fun (E, {Vs0, Env0}) ->
					  {T, Vs1, Env1} =
					      make_template(E, Vs0,
							    Env0),
					  {T, {Vs1, Env1}}
				  end,
				  {Vs0, Env0}, data_es(E)),
	    T = make_data_skel(data_type(E), Ts),
	    E1 = update_data(E, data_type(E),
			     [hd(get_ann(T)) || T <- Ts]),
	    V = new_template_var(Env1),
	    Env2 = env__bind(var_name(V), E1, Env1),
	    {set_ann(T, [V]), [V | Vs1], Env2};
	false ->
	    case type(E) of
		seq ->
		    %% For a sequencing, we can rebind the variable used
		    %% for the body, and pass on the template as it is.
		    {T, Vs1, Env1} = make_template(seq_body(E), Vs0,
						   Env0),
		    V = var_name(hd(get_ann(T))),
		    E1 = update_c_seq(E, seq_arg(E), env__get(V, Env1)),
		    Env2 = env__bind(V, E1, Env1),
		    {T, Vs1, Env2};
		_ ->
		    V = new_template_var(Env0),
		    Env1 = env__bind(var_name(V), E, Env0),
		    {set_ann(V, [V]), [V | Vs0], Env1}
	    end
    end.

%% Two clauses are equivalent if their bodies are equivalent expressions
%% given that the respective pattern variables are local.

equivalent_clauses([]) ->
    true;
equivalent_clauses([C | Cs]) ->
    Env = cerl_trees:variables(c_values(clause_pats(C))),
    equivalent_clauses_1(clause_body(C), Cs, Env).

equivalent_clauses_1(E, [C | Cs], Env) ->
    Env1 = cerl_trees:variables(c_values(clause_pats(C))),
    case equivalent(E, clause_body(C), ordsets:union(Env, Env1)) of
	true ->
	    equivalent_clauses_1(E, Cs, Env);
	false ->
	    false
    end;
equivalent_clauses_1(_, [], _Env) ->
    true.

%% Two expressions are equivalent if and only if they yield the same
%% value and has the same side effects in the same order. Currently, we
%% only accept equality between constructors (constants) and nonlocal
%% variables, since this should cover most cases of interest. If a
%% variable is locally bound in one expression, it cannot be equivalent
%% to one with the same name in the other expression, so we need not
%% keep track of two environments.

equivalent(E1, E2, Env) ->
    case is_data(E1) of
        true ->
            case is_data(E2) of
                true ->
                    T1 = {data_type(E1), data_arity(E1)},
                    T2 = {data_type(E2), data_arity(E2)},
                    %% Note that we must test for exact equality.
                    T1 =:= T2 andalso
			equivalent_lists(data_es(E1), data_es(E2), Env);
                false ->
                    false
            end;
        false ->
	    case type(E1) of
		var ->
		    case is_c_var(E2) of
			true ->
			    N1 = var_name(E1),
			    N2 = var_name(E2),
			    N1 =:= N2 andalso not ordsets:is_element(N1, Env);
			false ->
			    false
		    end;
		_ ->
		    %% Other constructs are not being considered.
		    false
	    end
    end.

equivalent_lists([E1 | Es1], [E2 | Es2], Env) ->
    equivalent(E1, E2, Env) and equivalent_lists(Es1, Es2, Env);
equivalent_lists([], [], _) ->
    true;
equivalent_lists(_, _, _) ->
    false.

%% Return `false' or `{true, EffectExpr, ValueExpr}'. The environment is
%% passed for new-variable generation.

reduce_bif_call(M, F, As, Env) ->
    reduce_bif_call_1(M, F, length(As), As, Env).

reduce_bif_call_1(erlang, element, 2, [X, Y], _Env) ->
    case is_c_int(X) and is_c_tuple(Y) of
	true ->
	    %% We are free to change the relative evaluation order of
	    %% the elements, so lifting out a particular element is OK.
	    T = list_to_tuple(tuple_es(Y)),
	    N = int_val(X),
	    if is_integer(N), N > 0, N =< tuple_size(T) ->
		    E = element(N, T),
		    Es = tuple_to_list(setelement(N, T, void())),
		    {true, make_seq(c_tuple(Es), E)};
	       true ->
		    false
	    end;
	false ->
	    false
    end;
reduce_bif_call_1(erlang, hd, 1, [X], _Env) ->
    case is_c_cons(X) of
	true ->
	    %% Cf. `element/2' above.
	    {true, make_seq(cons_tl(X), cons_hd(X))};
	false ->
	    false
    end;
reduce_bif_call_1(erlang, length, 1, [X], _Env) ->
    case is_c_list(X) of
	true ->
	    %% Cf. `erlang:size/1' below.
	    {true, make_seq(X, c_int(list_length(X)))};
	false ->
	    false
    end;
reduce_bif_call_1(erlang, list_to_tuple, 1, [X], _Env) ->
    case is_c_list(X) of
	true ->
	    %% This does not actually preserve all the evaluation order
	    %% constraints of the list, but I don't imagine that it will
	    %% be a problem.
	    {true, c_tuple(list_elements(X))};
	false ->
	    false
    end;
reduce_bif_call_1(erlang, setelement, 3, [X, Y, Z], Env) ->
    case is_c_int(X) and is_c_tuple(Y) of
	true ->
	    %% Here, unless `Z' is a simple expression, we must bind it
	    %% to a new variable, because in that case, `Z' must be
	    %% evaluated before any part of `Y'.
	    T = list_to_tuple(tuple_es(Y)),
	    N = int_val(X),
	    if is_integer(N), N > 0, N =< tuple_size(T) ->
		    E = element(N, T),
		    case is_simple(Z) of
			true ->
			    Es = tuple_to_list(setelement(N, T, Z)),
			    {true, make_seq(E, c_tuple(Es))};
			false ->
			    V = new_var(Env),
			    Es = tuple_to_list(setelement(N, T, V)),
			    E1 = make_seq(E, c_tuple(Es)),
			    {true, c_let([V], Z, E1)}
		    end;
	       true ->
		    false
	    end;
	false ->
	    false
    end;
reduce_bif_call_1(erlang, size, 1, [X], Env) ->
    case is_c_tuple(X) of
	true ->
	    reduce_bif_call_1(erlang, tuple_size, 1, [X], Env);
	false ->
	    false
    end;
reduce_bif_call_1(erlang, tl, 1, [X], _Env) ->
    case is_c_cons(X) of
	true ->
	    %% Cf. `element/2' above.
	    {true, make_seq(cons_hd(X), cons_tl(X))};
	false ->
	    false
    end;
reduce_bif_call_1(erlang, tuple_size, 1, [X], _Env) ->
    case is_c_tuple(X) of
	true ->
	    %% Just evaluate the tuple for effect and use the size (the
	    %% arity) as the result.
	    {true, make_seq(X, c_int(tuple_arity(X)))};
	false ->
	    false
    end;
reduce_bif_call_1(erlang, tuple_to_list, 1, [X], _Env) ->
    case is_c_tuple(X) of
	true ->
	    %% This actually introduces slightly stronger constraints on
	    %% the evaluation order of the subexpressions.
	    {true, make_list(tuple_es(X))};
	false ->
	    false
    end;
reduce_bif_call_1(_M, _F, _A, _As, _Env) ->
    false.

effort_is_active(S) ->
    counter__is_active(st__get_effort(S)).

size_is_active(S) ->
    counter__is_active(st__get_size(S)).

get_effort_limit(S) ->
    counter__limit(st__get_effort(S)).

new_active_effort(Limit, S) ->
    st__set_effort(counter__new_active(Limit), S).

revert_effort(S1, S2) ->
    st__set_effort(st__get_effort(S1), S2).

new_active_size(Limit, S) ->
    st__set_size(counter__new_active(Limit), S).

new_passive_size(Limit, S) ->
    st__set_size(counter__new_passive(Limit), S).

revert_size(S1, S2) ->
    st__set_size(st__get_size(S1), S2).

count_effort(N, S) ->
    C = st__get_effort(S),
    C1 = counter__add(N, C, effort, S),
    case debug_counters() of
        %% true ->
        %%     case counter__is_active(C1) of
        %%         true ->
        %%             V = counter__value(C1),
        %%             case V > get(counter_effort_max) of
        %%                 true ->
        %%                     put(counter_effort_max, V);
        %%                 false ->
        %%                     ok
        %%             end;
        %%         false ->
        %%             ok
        %%     end;
        false ->
            ok
    end,
    st__set_effort(C1, S).

count_size(N, S) ->
    C = st__get_size(S),
    C1 = counter__add(N, C, size, S),
    case debug_counters() of
        %% true ->
        %%     case counter__is_active(C1) of
        %%         true ->
        %%             V = counter__value(C1),
        %%             case V > get(counter_size_max) of
        %%                 true ->
        %%                     put(counter_size_max, V);
        %%                 false ->
        %%                     ok
        %%             end;
        %%         false ->
        %%             ok
        %%     end;
        false ->
            ok
    end,
    st__set_size(C1, S).

get_size_value(S) ->
    counter__value(st__get_size(S)).

get_size_limit(S) ->
    counter__limit(st__get_size(S)).

kill_id_anns([{'id',_} | As]) ->
    kill_id_anns(As);
kill_id_anns([A | As]) ->
    [A | kill_id_anns(As)];
kill_id_anns([]) ->
    [].


%% =====================================================================
%% General utilities

%% The atom `ok', is widely used in Erlang for "void" values.

void() -> abstract(ok).

is_simple(E) ->
    case type(E) of
	literal -> true;
	var -> true;
	'fun' -> true;
	_ -> false
    end.

get_components(N, E) ->
    case type(E) of
	values ->
	    Es = values_es(E),
	    if length(Es) =:= N ->
		    {true, Es};
	       true ->
		    false
	    end;
	_ when N =:= 1 ->
	    {true, [E]};
	_ ->
	    false
    end.

all_static(Es) ->
    lists:all(fun (E) -> is_literal(result(E)) end, Es).

set_clause_bodies([C | Cs], B) ->
    [update_c_clause(C, clause_pats(C), clause_guard(C), B)
     | set_clause_bodies(Cs, B)];
set_clause_bodies([], _) ->
    [].

%% =====================================================================
%% Abstract datatype: renaming()

ren__identity() ->
    #{}.

ren__add(X, Y, Ren) ->
    Ren#{X=>Y}.

ren__map(X, Ren) ->
    case Ren of
        #{X:=Y} -> Y;
        #{} -> X
    end.

ren__add_identity(X, Ren) ->
    maps:remove(X, Ren).


%% =====================================================================
%% Abstract datatype: environment()

env__empty() ->
    rec_env:empty().

env__bind(Key, Val, Env) ->
    rec_env:bind(Key, Val, Env).

%% `Es' should have type `[{Key, Val}]', and `Fun' should have type
%% `(Val, Env) -> T', mapping a value together with the recursive
%% environment itself to some term `T' to be returned when the entry is
%% looked up.

env__bind_recursive(Ks, Vs, F, Env) ->
    rec_env:bind_recursive(Ks, Vs, F, Env).

env__lookup(Key, Env) ->
    rec_env:lookup(Key, Env).

env__get(Key, Env) ->
    rec_env:get(Key, Env).

env__is_defined(Key, Env) ->
    rec_env:is_defined(Key, Env).

env__new_vname(Env) ->
    rec_env:new_key(Env).

env__new_tname(Env) ->
    rec_env:new_key(fun(I) ->
                            list_to_atom("@i"++integer_to_list(I))
                    end, Env).

env__new_fname(A, N, Env) ->
    rec_env:new_key(fun (X) ->
			S = integer_to_list(X),
			{list_to_atom(atom_to_list(A) ++ "_" ++ S),
			 N}
		    end, Env).


%% =====================================================================
%% Abstract datatype: state()

-record(state, {free,		% next free location
		size,		% size counter
		effort,		% effort counter
		unroll,		% inner/outer-pending initial value
		cache,		% operand expression cache
		var_flags,	% flags for variables (#ref-structures)
		opnd_flags,	% flags for operands
		app_flags}).	% flags for #app-structures

%% Note that we do not have a `var_assigned' flag, since there is no
%% destructive assignment in Erlang. In the original algorithm, the
%% "residual-referenced"-flags of the previous inlining pass (or
%% initialization pass) are used as the "source-referenced"-flags for
%% the subsequent pass. The latter may then be used as a safe
%% approximation whenever we need to base a decision on whether or not a
%% particular variable or function variable could be referenced in the
%% program being generated, and computation of the new
%% "residual-referenced" flag for that variable is not yet finished. In
%% the present algorithm, this can only happen in the presence of
%% variable assignments, which do not exist in Erlang. Therefore, we do
%% not keep "source-referenced" flags for residual-code references in
%% our implementation.
%%
%% The "inner-pending" flag tells us whether we are already in the
%% process of visiting a particular operand, and the "outer-pending"
%% flag whether we are in the process of inlining a propagated
%% functional value. The "pending flags" are really counters limiting
%% the number of times an operand may be inlined recursively, causing
%% loop unrolling. Note that the initial value must be greater than zero
%% in order for any inlining at all to be done.

%% Flags are stored in ETS-tables, one table for each class. The second
%% element in each stored tuple is the key (the "label").

-record(var_flags, {lab, referenced = false}).
-record(opnd_flags, {lab, inner_pending = 1, outer_pending = 1,
		     effect = false}).
-record(app_flags, {lab, inlined = false}).

st__new(Effort, Size, Unroll) ->
    EtsOpts = [set, private, {keypos, 2}],
    #state{free = 0,
	   size = counter__new_passive(Size),
	   effort = counter__new_passive(Effort),
	   unroll = Unroll,
	   cache = maps:new(),
 	   var_flags = ets:new(var, EtsOpts),
	   opnd_flags = ets:new(opnd, EtsOpts),
	   app_flags = ets:new(app, EtsOpts)}.

st__new_loc(S) ->
    N = S#state.free,
    {N, S#state{free = N + 1}}.

st__get_effort(S) ->
    S#state.effort.

st__set_effort(C, S) ->
    S#state{effort = C}.

st__get_size(S) ->
    S#state.size.

st__set_size(C, S) ->
    S#state{size = C}.

st__set_var_referenced(L, S) ->
    T = S#state.var_flags,
    [F] = ets:lookup(T, L),
    ets:insert(T, F#var_flags{referenced = true}),
    S.

st__get_var_referenced(L, S) ->
    ets:lookup_element(S#state.var_flags, L, #var_flags.referenced).

st__lookup_opnd_cache(L, S) ->
    maps:find(L, S#state.cache).

%% Note that setting the cache should only be done once.

st__set_opnd_cache(L, C, S) ->
    S#state{cache = maps:put(L, C, S#state.cache)}.

st__set_opnd_effect(L, S) ->
    T = S#state.opnd_flags,
    [F] = ets:lookup(T, L),
    ets:insert(T, F#opnd_flags{effect = true}),
    S.

st__get_opnd_effect(L, S) ->
    ets:lookup_element(S#state.opnd_flags, L, #opnd_flags.effect).

st__set_app_inlined(L, S) ->
    T = S#state.app_flags,
    [F] = ets:lookup(T, L),
    ets:insert(T, F#app_flags{inlined = true}),
    S.

st__clear_app_inlined(L, S) ->
    T = S#state.app_flags,
    [F] = ets:lookup(T, L),
    ets:insert(T, F#app_flags{inlined = false}),
    S.

st__get_app_inlined(L, S) ->
    ets:lookup_element(S#state.app_flags, L, #app_flags.inlined).

%% The pending-flags are initialized by `st__new_opnd_loc' below.

st__test_inner_pending(L, S) ->
    T = S#state.opnd_flags,
    P = ets:lookup_element(T, L, #opnd_flags.inner_pending),
    P =< 0.

st__mark_inner_pending(L, S) ->
    ets:update_counter(S#state.opnd_flags, L,
		       {#opnd_flags.inner_pending, -1}),
    S.

st__clear_inner_pending(L, S) ->
    ets:update_counter(S#state.opnd_flags, L,
		       {#opnd_flags.inner_pending, 1}),
    S.

st__test_outer_pending(L, S) ->
    T = S#state.opnd_flags,
    P = ets:lookup_element(T, L, #opnd_flags.outer_pending),
    P =< 0.

st__mark_outer_pending(L, S) ->
    ets:update_counter(S#state.opnd_flags, L,
		       {#opnd_flags.outer_pending, -1}),
    S.

st__clear_outer_pending(L, S) ->
    ets:update_counter(S#state.opnd_flags, L,
		       {#opnd_flags.outer_pending, 1}),
    S.

st__new_app_loc(S) ->
    V = {L, _S1} = st__new_loc(S),
    ets:insert(S#state.app_flags, #app_flags{lab = L}),
    V.

st__new_ref_loc(S) ->
    V = {L, _S1} = st__new_loc(S),
    ets:insert(S#state.var_flags, #var_flags{lab = L}),
    V.

st__new_opnd_loc(S) ->
    V = {L, _S1} = st__new_loc(S),
    N = S#state.unroll,
    ets:insert(S#state.opnd_flags,
	       #opnd_flags{lab = L,
			   inner_pending = N,
			   outer_pending = N}),
    V.


%% =====================================================================
%% Abstract datatype: counter()
%%
%% `counter__add' throws `{counter_exceeded, Type, Data}' if the
%% resulting counter value would exceed the limit for the counter in
%% question (`Type' and `Data' are given by the user).

counter__new_passive(Limit) when Limit > 0 ->
    {0, Limit}.

counter__new_active(Limit) when Limit > 0 ->
    {Limit, Limit}.

%% Active counters have values > 0 internally; passive counters start at
%% zero. The 'limit' field is only accessed by the 'counter__limit'
%% function.

counter__is_active({C, _}) ->
    C > 0.

counter__limit({_, L}) ->
    L.

counter__value({N, L}) ->
    if N > 0 ->
	    L - N;
       true ->
            -N
    end.

counter__add(N, {V, L}, Type, Data) ->
    N1 = V - N,
    if V > 0, N1 =< 0 ->
	    case debug_counters() of
		%% true ->
		%%     case Type of
		%% 	effort ->
		%% 	    put(counter_effort_triggers,
		%% 		get(counter_effort_triggers) + 1);
		%% 	size ->
		%% 	    put(counter_size_triggers,
		%% 		get(counter_size_triggers) + 1)
		%%     end;
		false ->
		    ok
	    end,
	    throw({counter_exceeded, Type, Data});
       true ->
	    {N1, L}
    end.


%% =====================================================================
%% Reporting

% report_internal_error(S) ->
%     report_internal_error(S, []).

report_internal_error(S, Vs) ->
    report_error("internal error: " ++ S, Vs).

%% report_error(D) ->
%%     report_error(D, []).
    
report_error(D, Vs) ->
    report({error, D}, Vs).

report_warning(D) ->
    report_warning(D, []).

report_warning(D, Vs) ->
    report({warning, D}, Vs).

report(D, Vs) ->
    io:put_chars(format(D, Vs)).

format({error, D}, Vs) ->
    ["error: ", format(D, Vs)];
format({warning, D}, Vs) ->
    ["warning: ", format(D, Vs)];
format(S, Vs) when is_list(S) ->
    [io_lib:fwrite(S, Vs), $\n].


%% =====================================================================
