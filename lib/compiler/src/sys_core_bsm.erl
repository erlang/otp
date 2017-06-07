%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2017. All Rights Reserved.
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
%% Purpose : Optimize bit syntax matching.


-module(sys_core_bsm).
-export([module/2,format_error/1]).

-include("core_parse.hrl").
-import(lists, [member/2,nth/2,reverse/1,usort/1]).

-spec module(cerl:c_module(), [compile:option()]) -> {'ok', cerl:c_module()}.

module(#c_module{defs=Ds0}=Mod, Opts) ->
    {Ds,Ws0} = function(Ds0, [], []),
    case member(bin_opt_info, Opts) of
        false ->
            {ok,Mod#c_module{defs=Ds}};
        true ->
            Ws1 = [make_warning(Where, What) || {Where,What} <- Ws0],
            Ws = usort(Ws1),
            {ok,Mod#c_module{defs=Ds},Ws}
    end.

function([{#c_var{name={F,Arity}}=Name,B0}|Fs], FsAcc, Ws0) ->
    try cerl_trees:mapfold(fun bsm_an/2, Ws0, B0) of
        {B,Ws} ->
            function(Fs, [{Name,B}|FsAcc], Ws)
    catch
	Class:Error ->
	    Stack = erlang:get_stacktrace(),
	    io:fwrite("Function: ~w/~w\n", [F,Arity]),
	    erlang:raise(Class, Error, Stack)
    end;
function([], Fs, Ws) ->
    {reverse(Fs),Ws}.

-type error() :: atom().
-spec format_error(error()) -> nonempty_string().

format_error(bin_opt_alias) ->
    "INFO: the '=' operator will prevent delayed sub binary optimization";
format_error(bin_partition) ->
    "INFO: matching non-variables after a previous clause matching a variable "
	"will prevent delayed sub binary optimization";
format_error(bin_left_var_used_in_guard) ->
    "INFO: a variable to the left of the binary pattern is used in a guard; "
	"will prevent delayed sub binary optimization";
format_error(bin_argument_order) ->
    "INFO: matching anything else but a plain variable to the left of "
	"binary pattern will prevent delayed sub binary optimization; "
	"SUGGEST changing argument order";
format_error(bin_var_used) ->
    "INFO: using a matched out sub binary will prevent "
	"delayed sub binary optimization";
format_error(orig_bin_var_used_in_guard) ->
    "INFO: using the original binary variable in a guard will prevent "
	"delayed sub binary optimization";
format_error(bin_var_used_in_guard) ->
    "INFO: using a matched out sub binary in a guard will prevent "
	"delayed sub binary optimization".


%%%
%%% Annotate bit syntax matching to faciliate optimization in further passes.
%%%

bsm_an(Core0, Ws0) ->
    case bsm_an(Core0) of
        {ok,Core} ->
            {Core,Ws0};
        {ok,Core,W} ->
            {Core,[W|Ws0]}
    end.

bsm_an(#c_case{arg=#c_var{}=V}=Case) ->
    bsm_an_1([V], Case);
bsm_an(#c_case{arg=#c_values{es=Es}}=Case) ->
    bsm_an_1(Es, Case);
bsm_an(Other) ->
    {ok,Other}.

bsm_an_1(Vs, #c_case{clauses=Cs}=Case) ->
    case bsm_leftmost(Cs) of
	none -> {ok,Case};
	Pos -> bsm_an_2(Vs, Cs, Case, Pos)
    end.

bsm_an_2(Vs, Cs, Case, Pos) ->
    case bsm_nonempty(Cs, Pos) of
	true -> bsm_an_3(Vs, Cs, Case, Pos);
	false -> {ok,Case}
    end.

bsm_an_3(Vs, Cs, Case, Pos) ->
    try
	bsm_ensure_no_partition(Cs, Pos),
	{ok,bsm_do_an(Vs, Pos, Cs, Case)}
    catch
	throw:{problem,Where,What} ->
	    {ok,Case,{Where,What}}
    end.

bsm_do_an(Vs0, Pos, Cs0, Case) ->
    case nth(Pos, Vs0) of
	#c_var{name=Vname}=V0 ->
	    Cs = bsm_do_an_var(Vname, Pos, Cs0, []),
	    V = bsm_annotate_for_reuse(V0),
	    Bef = lists:sublist(Vs0, Pos-1),
	    Aft = lists:nthtail(Pos, Vs0),
	    case Bef ++ [V|Aft] of
		[_] ->
		    Case#c_case{arg=V,clauses=Cs};
		Vs ->
		    Case#c_case{arg=#c_values{es=Vs},clauses=Cs}
	    end;
	_ ->
	    Case
    end.

bsm_do_an_var(V, S, [#c_clause{pats=Ps,guard=G,body=B0}=C0|Cs], Acc) ->
    case nth(S, Ps) of
	#c_var{name=VarName} ->
	    case core_lib:is_var_used(V, G) of
		true -> bsm_problem(C0, orig_bin_var_used_in_guard);
		false -> ok
	    end,
	    case core_lib:is_var_used(VarName, G) of
		true -> bsm_problem(C0, bin_var_used_in_guard);
		false -> ok
	    end,
	    B1 = bsm_maybe_ctx_to_binary(VarName, B0),
	    B = bsm_maybe_ctx_to_binary(V, B1),
	    C = C0#c_clause{body=B},
	    bsm_do_an_var(V, S, Cs, [C|Acc]);
	#c_alias{}=P ->
	    case bsm_could_match_binary(P) of
		false ->
		    bsm_do_an_var(V, S, Cs, [C0|Acc]);
		true ->
		    bsm_problem(C0, bin_opt_alias)
	    end;
	P ->
	    case bsm_could_match_binary(P) andalso bsm_is_var_used(V, G, B0) of
		false ->
		    bsm_do_an_var(V, S, Cs, [C0|Acc]);
		true ->
		    bsm_problem(C0, bin_var_used)
	    end
    end;
bsm_do_an_var(_, _, [], Acc) -> reverse(Acc).

bsm_annotate_for_reuse(#c_var{anno=Anno}=Var) ->
    Var#c_var{anno=[reuse_for_context|Anno]}.

bsm_is_var_used(V, G, B) ->
    core_lib:is_var_used(V, G) orelse core_lib:is_var_used(V, B).

bsm_maybe_ctx_to_binary(V, B) ->
    case core_lib:is_var_used(V, B) andalso not previous_ctx_to_binary(V, B) of
	false ->
	    B;
	true ->
	    #c_seq{arg=#c_primop{name=#c_literal{val=bs_context_to_binary},
				 args=[#c_var{name=V}]},
		   body=B}
    end.

previous_ctx_to_binary(V, Core) ->
    case Core of
	#c_seq{arg=#c_primop{name=#c_literal{val=bs_context_to_binary},
			     args=[#c_var{name=V}]}} ->
	    true;
	_ ->
	    false
    end.

%% bsm_leftmost(Cs) -> none | ArgumentNumber
%%  Find the leftmost argument that does binary matching. Return
%%  the number of the argument (1-N).

bsm_leftmost(Cs) ->
    bsm_leftmost_1(Cs, none).

bsm_leftmost_1([#c_clause{pats=Ps}|Cs], Pos) ->
    bsm_leftmost_2(Ps, Cs, 1, Pos);
bsm_leftmost_1([], Pos) -> Pos.

bsm_leftmost_2(_, Cs, Pos, Pos) ->
    bsm_leftmost_1(Cs, Pos);
bsm_leftmost_2([#c_binary{}|_], Cs, N, _) ->
    bsm_leftmost_1(Cs, N);
bsm_leftmost_2([_|Ps], Cs, N, Pos) ->
    bsm_leftmost_2(Ps, Cs, N+1, Pos);
bsm_leftmost_2([], Cs, _, Pos) ->
    bsm_leftmost_1(Cs, Pos).

%% bsm_nonempty(Cs, Pos) -> true|false
%%  Check if at least one of the clauses matches a non-empty
%%  binary in the given argument position.
%%
bsm_nonempty([#c_clause{pats=Ps}|Cs], Pos) ->
    case nth(Pos, Ps) of
	#c_binary{segments=[_|_]} ->
	    true;
	_ ->
	    bsm_nonempty(Cs, Pos)
    end;
bsm_nonempty([], _ ) -> false.

%% bsm_ensure_no_partition(Cs, Pos) -> ok     (exception if problem)
%%  We must make sure that matching is not partitioned between
%%  variables like this:
%%             foo(<<...>>) -> ...
%%             foo(<Variable>) when ... -> ...
%%             foo(<Any non-variable pattern>) ->
%%  If there is such partition, we are not allowed to reuse the binary variable
%%  for the match context.
%%
%%  Also, arguments to the left of the argument that is matched
%%  against a binary, are only allowed to be simple variables, not
%%  used in guards. The reason is that we must know that the binary is
%%  only matched in one place (i.e. there must be only one bs_start_match2
%%  instruction emitted).

bsm_ensure_no_partition(Cs, Pos) ->
    bsm_ensure_no_partition_1(Cs, Pos, before).

%% Loop through each clause.
bsm_ensure_no_partition_1([#c_clause{pats=Ps,guard=G}|Cs], Pos, State0) ->
    State = bsm_ensure_no_partition_2(Ps, Pos, G, simple_vars, State0),
    case State of
	'after' ->
	    bsm_ensure_no_partition_after(Cs, Pos);
	_ ->
	    ok
    end,
    bsm_ensure_no_partition_1(Cs, Pos, State);
bsm_ensure_no_partition_1([], _, _) -> ok.

%% Loop through each pattern for this clause.
bsm_ensure_no_partition_2([#c_binary{}=Where|_], 1, _, Vstate, State) ->
    case State of
	before when Vstate =:= simple_vars -> within;
	before -> bsm_problem(Where, Vstate);
	within when Vstate =:= simple_vars -> within;
	within -> bsm_problem(Where, Vstate)
    end;
bsm_ensure_no_partition_2([#c_alias{}=Alias|_], 1, N, Vstate, State) ->
    %% Retrieve the real pattern that the alias refers to and check that.
    P = bsm_real_pattern(Alias),
    bsm_ensure_no_partition_2([P], 1, N, Vstate, State);
bsm_ensure_no_partition_2([_|_], 1, _, _Vstate, before=State) ->
    %% No binary matching yet - therefore no partition.
    State;
bsm_ensure_no_partition_2([P|_], 1, _, Vstate, State) ->
    case bsm_could_match_binary(P) of
	false ->
	    %% If clauses can be freely arranged (Vstate =:= simple_vars),
	    %% a clause that cannot match a binary will not partition the clause.
	    %% Example:
	    %%
	    %% a(Var, <<>>) -> ...
	    %% a(Var, []) -> ...
	    %% a(Var, <<B>>) -> ...
	    %%
	    %% But if the clauses can't be freely rearranged, as in
	    %%
	    %% b(Var, <<X>>) -> ...
	    %% b(1, 2) -> ...
	    %%
	    %% we do have a problem.
	    %%
	    case Vstate of
		simple_vars -> State;
		_ -> bsm_problem(P, Vstate)
	    end;
	true ->
	    %% The pattern P *may* match a binary, so we must update the state.
	    %% (P must be a variable.)
	    case State of
		within -> 'after';
		'after' -> 'after'
	    end
    end;
bsm_ensure_no_partition_2([#c_var{name=V}|Ps], N, G, Vstate, S) ->
    case core_lib:is_var_used(V, G) of
	false ->
	    bsm_ensure_no_partition_2(Ps, N-1, G, Vstate, S);
	true ->
	    bsm_ensure_no_partition_2(Ps, N-1, G, bin_left_var_used_in_guard, S)
    end;
bsm_ensure_no_partition_2([_|Ps], N, G, _, S) ->
    bsm_ensure_no_partition_2(Ps, N-1, G, bin_argument_order, S).

bsm_ensure_no_partition_after([#c_clause{pats=Ps}=C|Cs], Pos) ->
    case nth(Pos, Ps) of
	#c_var{} ->
	    bsm_ensure_no_partition_after(Cs, Pos);
	_ ->
	    bsm_problem(C, bin_partition)
    end;
bsm_ensure_no_partition_after([], _) -> ok.

bsm_could_match_binary(#c_alias{pat=P}) -> bsm_could_match_binary(P);
bsm_could_match_binary(#c_cons{}) -> false;
bsm_could_match_binary(#c_tuple{}) -> false;
bsm_could_match_binary(#c_literal{val=Lit}) -> is_bitstring(Lit);
bsm_could_match_binary(_) -> true.

bsm_real_pattern(#c_alias{pat=P}) -> bsm_real_pattern(P);
bsm_real_pattern(P) -> P.

bsm_problem(Where, What) ->
    throw({problem,Where,What}).

make_warning(Core, Term) ->
    case should_suppress_warning(Core) of
	true ->
	    ok;
	false ->
            Anno = cerl:get_ann(Core),
            Line = get_line(Anno),
            File = get_file(Anno),
            {File,[{Line,?MODULE,Term}]}
    end.

should_suppress_warning(Core) ->
    Ann = cerl:get_ann(Core),
    member(compiler_generated, Ann).

get_line([Line|_]) when is_integer(Line) -> Line;
get_line([_|T]) -> get_line(T);
get_line([]) -> none.

get_file([{file,File}|_]) -> File;
get_file([_|T]) -> get_file(T);
get_file([]) -> "no_file". % should not happen
