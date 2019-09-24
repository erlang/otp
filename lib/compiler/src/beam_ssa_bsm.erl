%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2018. All Rights Reserved.
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

%%%
%%% This pass optimizes bit syntax matching, and is centered around the concept
%%% of "match context reuse" which is best explained through example. To put it
%%% shortly we attempt to turn this:
%%%
%%%    <<0,B/bits>> = A,
%%%    <<1,C/bits>> = B,
%%%    <<D,_/bits>> = C,
%%%    D.
%%%
%%% ... Into this:
%%%
%%%    <<0,1,D,_/bits>>=A,
%%%    D.
%%%
%%% Which is much faster as it avoids the creation of intermediate terms. This
%%% is especially noticeable in loops where such garbage is generated on each
%%% iteration.
%%%
%%% The optimization itself is very simple and can be applied whenever there's
%%% matching on the tail end of a binary; instead of creating a new binary and
%%% starting a new match context on it, we reuse the match context used to
%%% extract the tail and avoid the creation of both objects.
%%%
%%% The catch is that a match context isn't a proper type and nothing outside
%%% of bit syntax match operations can handle them. We therefore need to make
%%% sure that they never "leak" into other instructions, and most of the pass
%%% revolves around getting around that limitation.
%%%
%%% Unlike most other passes we look at the whole module so we can combine
%%% matches across function boundaries, greatly increasing the performance of
%%% complex matches and loops.
%%%

-module(beam_ssa_bsm).

-export([module/2, format_error/1]).

-include("beam_ssa.hrl").

-import(lists, [member/2, reverse/1, reverse/2, splitwith/2, map/2, foldl/3,
                mapfoldl/3, nth/2, max/1, unzip/1]).

-spec format_error(term()) -> nonempty_string().

format_error(OptInfo) ->
    format_opt_info(OptInfo).

-spec module(Module, Options) -> Result when
      Module :: beam_ssa:b_module(),
      Options :: [compile:option()],
      Result :: {ok, beam_ssa:b_module(), list()}.

-define(PASS(N), {N,fun N/1}).

module(#b_module{body=Fs0}=Module, Opts) ->
    ModInfo = analyze_module(Module),

    %% combine_matches is repeated after accept_context_args as the control
    %% flow changes can enable further optimizations, as in the example below:
    %%
    %%    a(<<0,X/binary>>) -> a(X);
    %%    a(A) when bit_size(A) =:= 52 -> bar;
    %%    a(<<1,X/binary>>) -> X. %% Match context will be reused here when
    %%                            %% when repeated.

    {Fs, _} = compile:run_sub_passes(
                [?PASS(combine_matches),
                 ?PASS(accept_context_args),
                 ?PASS(combine_matches),
                 ?PASS(allow_context_passthrough),
                 ?PASS(skip_outgoing_tail_extraction),
                 ?PASS(annotate_context_parameters)],
                {Fs0, ModInfo}),

    Ws = case proplists:get_bool(bin_opt_info, Opts) of
             true -> collect_opt_info(Fs);
             false -> []
         end,

    {ok, Module#b_module{body=Fs}, Ws}.

-type module_info() :: #{ func_id() => func_info() }.

-type func_id() :: {Name :: atom(), Arity :: non_neg_integer()}.

-type func_info() :: #{ has_bsm_ops => boolean(),
                        parameters => [#b_var{}],
                        parameter_info => #{ #b_var{} => param_info() } }.

-type param_info() :: suitable_for_reuse |
                      {Problem :: atom(), Where :: term()}.

-spec analyze_module(#b_module{}) -> module_info().

analyze_module(#b_module{body=Fs}) ->
    foldl(fun(#b_function{args=Parameters}=F, I) ->
                  FuncInfo = #{ has_bsm_ops => has_bsm_ops(F),
                                parameters => Parameters,
                                parameter_info => #{} },
                  FuncId = get_fa(F),
                  I#{ FuncId => FuncInfo }
          end, #{}, Fs).

has_bsm_ops(#b_function{bs=Blocks}) ->
    hbo_blocks(maps:to_list(Blocks)).

hbo_blocks([{_,#b_blk{is=Is}} | Blocks]) ->
    case hbo_is(Is) of
        false -> hbo_blocks(Blocks);
        true -> true
    end;
hbo_blocks([]) ->
    false.

hbo_is([#b_set{op=bs_start_match} | _]) -> true;
hbo_is([_I | Is]) -> hbo_is(Is);
hbo_is([]) -> false.

%% Checks whether it's legal to make a call with the given argument as a match
%% context, returning the param_info() of the relevant parameter.
-spec check_context_call(#b_set{}, Arg, CtxChain, ModInfo) -> param_info() when
      Arg :: #b_var{},
      CtxChain :: [#b_var{}],
      ModInfo :: module_info().
check_context_call(#b_set{args=Args}, Arg, CtxChain, ModInfo) ->
    Aliases = [Arg | CtxChain],
    ccc_1(Args, Arg, Aliases, ModInfo).

ccc_1([#b_local{}=Call | Args], Ctx, Aliases, ModInfo) ->
    %% Matching operations assume that their context isn't aliased (as in
    %% pointer aliasing), so we must reject calls whose arguments contain more
    %% than one reference to the context.
    %%
    %% TODO: Try to fall back to passing binaries in these cases. Partial reuse
    %% is better than nothing.
    UseCount = foldl(fun(Arg, C) ->
                             case member(Arg, Aliases) of
                                 true -> C + 1;
                                 false -> C
                             end
                     end, 0, Args),
    if
        UseCount =:= 1 ->
            #b_local{name=#b_literal{val=Name},arity=Arity} = Call,
            Callee = {Name, Arity},

            ParamInfo = funcinfo_get(Callee, parameter_info, ModInfo),
            Parameters = funcinfo_get(Callee, parameters, ModInfo),
            Parameter = nth(1 + arg_index(Ctx, Args), Parameters),

            case maps:find(Parameter, ParamInfo) of
                {ok, suitable_for_reuse} ->
                    suitable_for_reuse;
                {ok, Other} ->
                    {unsuitable_call, {Call, Other}};
                error ->
                    {no_match_on_entry, Call}
            end;
        UseCount > 1 ->
            {multiple_uses_in_call, Call}
    end;
ccc_1([#b_remote{}=Call | _Args], _Ctx, _CtxChain, _ModInfo) ->
    {remote_call, Call};
ccc_1([Fun | _Args], _Ctx, _CtxChain, _ModInfo) ->
    %% TODO: It may be possible to support this in the future for locally
    %% defined funs, including ones with free variables.
    {fun_call, Fun}.

%% Returns the index of Var in Args.
arg_index(Var, Args) -> arg_index_1(Var, Args, 0).

arg_index_1(Var, [Var | _Args], Index) -> Index;
arg_index_1(Var, [_Arg | Args], Index) -> arg_index_1(Var, Args, Index + 1).

is_tail_binary(#b_set{op=bs_match,args=[#b_literal{val=binary} | Rest]}) ->
    member(#b_literal{val=all}, Rest);
is_tail_binary(#b_set{op=bs_get_tail}) ->
    true;
is_tail_binary(_) ->
    false.

is_tail_binary(#b_var{}=Var, Defs) ->
    case find_match_definition(Var, Defs) of
        {ok, Def} -> is_tail_binary(Def);
        _ -> false
    end;
is_tail_binary(_Literal, _Defs) ->
    false.

assert_match_context(#b_var{}=Var, Defs) ->
    case maps:find(Var, Defs) of
        {ok, #b_set{op=bs_match,args=[_,#b_var{}=Ctx|_]}} ->
            assert_match_context(Ctx, Defs);
        {ok, #b_set{op=bs_start_match}} ->
            ok
    end.

find_match_definition(#b_var{}=Var, Defs) ->
    case maps:find(Var, Defs) of
        {ok, #b_set{op=bs_extract,args=[Ctx]}} -> maps:find(Ctx, Defs);
        {ok, #b_set{op=bs_get_tail}=Def} -> {ok, Def};
        _ -> error
    end.

%% Returns a list of all contexts that were used to extract Var.
context_chain_of(#b_var{}=Var, Defs) ->
    case maps:find(Var, Defs) of
        {ok, #b_set{op=bs_match,args=[_,#b_var{}=Ctx|_]}} ->
            [Ctx | context_chain_of(Ctx, Defs)];
        {ok, #b_set{op=bs_get_tail,args=[Ctx]}} ->
            [Ctx | context_chain_of(Ctx, Defs)];
        {ok, #b_set{op=bs_extract,args=[Ctx]}} ->
            [Ctx | context_chain_of(Ctx, Defs)];
        _ ->
            []
    end.

%% Grabs the match context used to produce the given variable.
match_context_of(#b_var{}=Var, Defs) ->
    Ctx = match_context_of_1(Var, Defs),
    assert_match_context(Ctx, Defs),
    Ctx.

match_context_of_1(Var, Defs) ->
    case maps:get(Var, Defs) of
        #b_set{op=bs_extract,args=[#b_var{}=Ctx0]} ->
            #b_set{op=bs_match,
                   args=[_,#b_var{}=Ctx|_]} = maps:get(Ctx0, Defs),
            Ctx;
        #b_set{op=bs_get_tail,args=[#b_var{}=Ctx]} ->
            Ctx
    end.

funcinfo_get(#b_function{}=F, Attribute, ModInfo) ->
    funcinfo_get(get_fa(F), Attribute, ModInfo);
funcinfo_get({_,_}=Key, Attribute, ModInfo) ->
    FuncInfo = maps:get(Key, ModInfo),
    maps:get(Attribute, FuncInfo).

funcinfo_set(#b_function{}=F, Attribute, Value, ModInfo) ->
    funcinfo_set(get_fa(F), Attribute, Value, ModInfo);
funcinfo_set(Key, Attribute, Value, ModInfo) ->
    FuncInfo = maps:put(Attribute, Value, maps:get(Key, ModInfo, #{})),
    maps:put(Key, FuncInfo, ModInfo).

get_fa(#b_function{ anno = Anno }) ->
    {_,Name,Arity} = maps:get(func_info, Anno),
    {Name,Arity}.

%% Replaces matched-out binaries with aliases that are lazily converted to
%% binary form when used, allowing us to keep the "match path" free of binary
%% creation.

-spec alias_matched_binaries(Blocks, Counter, AliasMap) -> Result when
      Blocks :: beam_ssa:block_map(),
      Counter :: non_neg_integer(),
      AliasMap :: match_alias_map(),
      Result :: {Blocks, Counter}.

-type match_alias_map() ::
        #{ Binary :: #b_var{} =>
                     { %% Replace all uses of Binary with an alias after this
                       %% label.
                       AliasAfter :: beam_ssa:label(),
                       %% The match context whose tail is equal to Binary.
                       Context :: #b_var{} } }.

%% Keeps track of the promotions we need to insert. They're partially keyed by
%% location because they may not be valid on all execution paths and we may
%% need to add redundant promotions in some cases.
-type promotion_map() ::
        #{ { PromoteAt :: beam_ssa:label(),
             Variable :: #b_var{} } =>
               Instruction :: #b_set{} }.

-record(amb, { dominators :: beam_ssa:dominator_map(),
               match_aliases :: match_alias_map(),
               cnt :: non_neg_integer(),
               promotions = #{} :: promotion_map() }).

alias_matched_binaries(Blocks0, Counter, AliasMap) when AliasMap =/= #{} ->
    {Dominators, _} = beam_ssa:dominators(Blocks0),
    State0 = #amb{ dominators = Dominators,
                   match_aliases = AliasMap,
                   cnt = Counter },
    {Blocks, State} = beam_ssa:mapfold_blocks_rpo(fun amb_1/3, [0], State0,
                                                  Blocks0),
    {amb_insert_promotions(Blocks, State), State#amb.cnt};
alias_matched_binaries(Blocks, Counter, _AliasMap) ->
    {Blocks, Counter}.

amb_1(Lbl, #b_blk{is=Is0,last=Last0}=Block, State0) ->
    {Is, State1} = mapfoldl(fun(I, State) ->
                                    amb_assign_set(I, Lbl, State)
                            end, State0, Is0),
    {Last, State} = amb_assign_last(Last0, Lbl, State1),
    {Block#b_blk{is=Is,last=Last}, State}.

amb_assign_set(#b_set{op=phi,args=Args0}=I, _Lbl, State0) ->
    %% Phi node aliases are relative to their source block, not their
    %% containing block.
    {Args, State} =
        mapfoldl(fun({Arg0, Lbl}, Acc) ->
                         {Arg, State} = amb_get_alias(Arg0, Lbl, Acc),
                         {{Arg, Lbl}, State}
                 end, State0, Args0),
    {I#b_set{args=Args}, State};
amb_assign_set(#b_set{args=Args0}=I, Lbl, State0) ->
    {Args, State} = mapfoldl(fun(Arg0, Acc) ->
                                     amb_get_alias(Arg0, Lbl, Acc)
                             end, State0, Args0),
    {I#b_set{args=Args}, State}.

amb_assign_last(#b_ret{arg=Arg0}=T, Lbl, State0) ->
    {Arg, State} = amb_get_alias(Arg0, Lbl, State0),
    {T#b_ret{arg=Arg}, State};
amb_assign_last(#b_switch{arg=Arg0}=T, Lbl, State0) ->
    {Arg, State} = amb_get_alias(Arg0, Lbl, State0),
    {T#b_switch{arg=Arg}, State};
amb_assign_last(#b_br{bool=Arg0}=T, Lbl, State0) ->
    {Arg, State} = amb_get_alias(Arg0, Lbl, State0),
    {T#b_br{bool=Arg}, State}.

amb_get_alias(#b_var{}=Arg, Lbl, State) ->
    case maps:find(Arg, State#amb.match_aliases) of
        {ok, {AliasAfter, Context}} ->
            %% Our context may not have been created yet, so we skip assigning
            %% an alias unless the given block is among our dominators.
            Dominators = maps:get(Lbl, State#amb.dominators),
            case member(AliasAfter, Dominators) of
                true -> amb_create_alias(Arg, Context, Lbl, State);
                false -> {Arg, State}
            end;
        error ->
            {Arg, State}
    end;
amb_get_alias(#b_remote{mod=Mod0,name=Name0}=Arg0, Lbl, State0) ->
    {Mod, State1} = amb_get_alias(Mod0, Lbl, State0),
    {Name, State} = amb_get_alias(Name0, Lbl, State1),
    Arg = Arg0#b_remote{mod=Mod,name=Name},
    {Arg, State};
amb_get_alias(Arg, _Lbl, State) ->
    {Arg, State}.

amb_create_alias(#b_var{}=Arg0, Context, Lbl, State0) ->
    Dominators = maps:get(Lbl, State0#amb.dominators),
    Promotions0 = State0#amb.promotions,

    PrevPromotions =
        [maps:get({Dom, Arg0}, Promotions0)
         || Dom <- Dominators, is_map_key({Dom, Arg0}, Promotions0)],

    case PrevPromotions of
        [_|_] ->
            %% We've already created an alias prior to this block, so we'll
            %% grab the most recent one to minimize stack use.

            #b_set{dst=Alias} = max(PrevPromotions),
            {Alias, State0};
        [] ->
            %% If we haven't created an alias we need to do so now. The
            %% promotion will be inserted later by amb_insert_promotions/2.

            Counter = State0#amb.cnt,
            Alias = #b_var{name={'@ssa_bsm_alias', Counter}},
            Promotion = #b_set{op=bs_get_tail,dst=Alias,args=[Context]},

            Promotions = maps:put({Lbl, Arg0}, Promotion, Promotions0),
            State = State0#amb{ promotions=Promotions, cnt=Counter+1 },

            {Alias, State}
    end.

amb_insert_promotions(Blocks0, State) ->
    F = fun({Lbl, #b_var{}}, Promotion, Blocks) ->
                Block = maps:get(Lbl, Blocks),

                Alias = Promotion#b_set.dst,
                {Before, After} = splitwith(
                                    fun(#b_set{args=Args}) ->
                                            not is_var_in_args(Alias, Args)
                                    end, Block#b_blk.is),
                Is = Before ++ [Promotion | After],

                maps:put(Lbl, Block#b_blk{is=Is}, Blocks)
        end,
    maps:fold(F, Blocks0, State#amb.promotions).

is_var_in_args(Var, [Var | _]) -> true;
is_var_in_args(Var, [#b_remote{name=Var} | _]) -> true;
is_var_in_args(Var, [#b_remote{mod=Var} | _]) -> true;
is_var_in_args(Var, [_ | Args]) -> is_var_in_args(Var, Args);
is_var_in_args(_Var, []) -> false.

%%%
%%% Subpasses
%%%

%% Removes superflous chained bs_start_match instructions in the same
%% function. When matching on an extracted tail binary, or on a binary we've
%% already matched on, we reuse the original match context.
%%
%% This pass runs first since it makes subsequent optimizations more effective
%% by removing spots where promotion would be required.

-type prior_match_map() ::
        #{ Binary :: #b_var{} =>
                     [{ %% The context and success label of a previous
                        %% bs_start_match made on this binary.
                        ValidAfter :: beam_ssa:label(),
                        Context :: #b_var{} }] }.

-record(cm, { definitions :: beam_ssa:definition_map(),
              dominators :: beam_ssa:dominator_map(),
              blocks :: beam_ssa:block_map(),
              match_aliases = #{} :: match_alias_map(),
              prior_matches = #{} :: prior_match_map(),
              renames = #{} :: beam_ssa:rename_map() }).

combine_matches({Fs0, ModInfo}) ->
    Fs = map(fun(F) -> combine_matches(F, ModInfo) end, Fs0),
    {Fs, ModInfo}.

combine_matches(#b_function{bs=Blocks0,cnt=Counter0}=F, ModInfo) ->
    case funcinfo_get(F, has_bsm_ops, ModInfo) of
        true ->
            {Dominators, _} = beam_ssa:dominators(Blocks0),
            {Blocks1, State} =
                beam_ssa:mapfold_blocks_rpo(
                  fun(Lbl, #b_blk{is=Is0}=Block0, State0) ->
                          {Is, State} = cm_1(Is0, [], Lbl, State0),
                          {Block0#b_blk{is=Is}, State}
                  end, [0],
                  #cm{ definitions = beam_ssa:definitions(Blocks0),
                       dominators = Dominators,
                       blocks = Blocks0 },
                  Blocks0),

            Blocks2 = beam_ssa:rename_vars(State#cm.renames, [0], Blocks1),

            {Blocks, Counter} = alias_matched_binaries(Blocks2, Counter0,
                                                       State#cm.match_aliases),

            F#b_function{ bs=Blocks, cnt=Counter };
        false ->
            F
    end.

cm_1([#b_set{ op=bs_start_match,
              dst=Ctx,
              args=[Src] },
      #b_set{ op=succeeded,
              dst=Bool,
              args=[Ctx] }]=MatchSeq, Acc0, Lbl, State0) ->
    Acc = reverse(Acc0),
    case is_tail_binary(Src, State0#cm.definitions) of
        true -> cm_combine_tail(Src, Ctx, Bool, Acc, State0);
        false -> cm_handle_priors(Src, Ctx, Bool, Acc, MatchSeq, Lbl, State0)
    end;
cm_1([I | Is], Acc, Lbl, State) ->
    cm_1(Is, [I | Acc], Lbl, State);
cm_1([], Acc, _Lbl, State) ->
    {reverse(Acc), State}.

%% If we're dominated by at least one match on the same source, we can reuse
%% the context created by that match.
cm_handle_priors(Src, DstCtx, Bool, Acc, MatchSeq, Lbl, State0) ->
    PriorCtxs = case maps:find(Src, State0#cm.prior_matches) of
                    {ok, Priors} ->
                        %% We've seen other match contexts on this source, but
                        %% we can only consider the ones whose success path
                        %% dominate us.
                        Dominators = maps:get(Lbl, State0#cm.dominators, []),
                        [Ctx || {ValidAfter, Ctx} <- Priors,
                                member(ValidAfter, Dominators)];
                    error ->
                        []
                end,
    case PriorCtxs of
        [Ctx|_] ->
            Renames0 = State0#cm.renames,
            Renames = Renames0#{ Bool => #b_literal{val=true}, DstCtx => Ctx },
            {Acc, State0#cm{ renames = Renames }};
        [] ->
            %% Since we lack a prior match, we need to register this one in
            %% case we dominate another.
            State = cm_register_prior(Src, DstCtx, Lbl, State0),
            {Acc ++ MatchSeq, State}
    end.

cm_register_prior(Src, DstCtx, Lbl, State) ->
    Block = maps:get(Lbl, State#cm.blocks),
    #b_br{succ=ValidAfter} = Block#b_blk.last,

    Priors0 = maps:get(Src, State#cm.prior_matches, []),
    Priors = [{ValidAfter, DstCtx} | Priors0],

    PriorMatches = maps:put(Src, Priors, State#cm.prior_matches),
    State#cm{ prior_matches = PriorMatches }.

cm_combine_tail(Src, DstCtx, Bool, Acc, State0) ->
    SrcCtx = match_context_of(Src, State0#cm.definitions),

    %% We replace the source with a context alias as it normally won't be used
    %% on the happy path after being matched, and the added cost of conversion
    %% is negligible if it is.
    Aliases = maps:put(Src, {0, SrcCtx}, State0#cm.match_aliases),

    Renames0 = State0#cm.renames,
    Renames = Renames0#{ Bool => #b_literal{val=true}, DstCtx => SrcCtx },

    State = State0#cm{ match_aliases = Aliases, renames = Renames },

    {Acc, State}.

%% Lets functions accept match contexts as arguments. The parameter must be
%% unused before the bs_start_match instruction, and it must be matched in the
%% first block.

-record(aca, { unused_parameters :: ordsets:ordset(#b_var{}),
               counter :: non_neg_integer(),
               parameter_info = #{} :: #{ #b_var{} => param_info() },
               match_aliases = #{} :: match_alias_map() }).

accept_context_args({Fs, ModInfo}) ->
    mapfoldl(fun accept_context_args/2, ModInfo, Fs).

accept_context_args(#b_function{bs=Blocks0}=F, ModInfo0) ->
    case funcinfo_get(F, has_bsm_ops, ModInfo0) of
        true ->
            Parameters = ordsets:from_list(funcinfo_get(F, parameters, ModInfo0)),
            State0 = #aca{ unused_parameters = Parameters,
                           counter = F#b_function.cnt },

            {Blocks1, State} = aca_1(Blocks0, State0),
            {Blocks, Counter} = alias_matched_binaries(Blocks1,
                                                       State#aca.counter,
                                                       State#aca.match_aliases),

            ModInfo = funcinfo_set(F, parameter_info, State#aca.parameter_info,
                                   ModInfo0),

            {F#b_function{bs=Blocks,cnt=Counter}, ModInfo};
        false ->
            {F, ModInfo0}
    end.

aca_1(Blocks, State) ->
    %% We only handle block 0 as we don't yet support starting a match after a
    %% test. This is generally good enough as the sys_core_bsm pass makes the
    %% match instruction come first if possible, and it's rare for a function
    %% to binary-match several parameters at once.
    EntryBlock = maps:get(0, Blocks),
    aca_enable_reuse(EntryBlock#b_blk.is, EntryBlock, Blocks, [], State).

aca_enable_reuse([#b_set{op=bs_start_match,args=[Src]}=I0 | Rest],
                 EntryBlock, Blocks0, Acc, State0) ->
    case aca_is_reuse_safe(Src, State0) of
        true ->
            {I, Last, Blocks1, State} =
                aca_reuse_context(I0, EntryBlock, Blocks0, State0),

            Is = reverse([I | Acc], Rest),
            Blocks = maps:put(0, EntryBlock#b_blk{is=Is,last=Last}, Blocks1),

            %% Copying (and thus renaming) the successors of a block may cause
            %% them to become unreachable under their original label, breaking
            %% the phi nodes on the original path that refer to them, so we
            %% remove unreachable blocks to make sure they aren't referenced.
            {beam_ssa:trim_unreachable(Blocks), State};
        false ->
            {Blocks0, State0}
    end;
aca_enable_reuse([I | Is], EntryBlock, Blocks, Acc, State0) ->
    UnusedParams0 = State0#aca.unused_parameters,
    case ordsets:intersection(UnusedParams0, beam_ssa:used(I)) of
        [] ->
            aca_enable_reuse(Is, EntryBlock, Blocks, [I | Acc], State0);
        PrematureUses ->
            UnusedParams = ordsets:subtract(UnusedParams0, PrematureUses),

            %% Mark the offending parameters as unsuitable for context reuse.
            ParamInfo = foldl(fun(A, Ps) ->
                                      maps:put(A, {used_before_match, I}, Ps)
                              end, State0#aca.parameter_info, PrematureUses),

            State = State0#aca{ unused_parameters = UnusedParams,
                                parameter_info = ParamInfo },
            aca_enable_reuse(Is, EntryBlock, Blocks, [I | Acc], State)
    end;
aca_enable_reuse([], _EntryBlock, Blocks, _Acc, State) ->
    {Blocks, State}.

aca_is_reuse_safe(Src, State) ->
    %% Context reuse is unsafe unless all uses are dominated by the start_match
    %% instruction. Since we only process block 0 it's enough to check if
    %% they're unused so far.
    ordsets:is_element(Src, State#aca.unused_parameters).

aca_reuse_context(#b_set{dst=Dst, args=[Src]}=I0, Block, Blocks0, State0) ->
    %% When matching fails on a reused context it needs to be converted back
    %% to a binary. We only need to do this on the success path since it can't
    %% be a context on the type failure path, but it's very common for these
    %% to converge which requires special handling.
    {State1, Last, Blocks} =
        aca_handle_convergence(Src, State0, Block#b_blk.last, Blocks0),

    Aliases = maps:put(Src, {Last#b_br.succ, Dst}, State1#aca.match_aliases),
    ParamInfo = maps:put(Src, suitable_for_reuse, State1#aca.parameter_info),

    State = State1#aca{ match_aliases = Aliases,
                        parameter_info = ParamInfo },

    I = beam_ssa:add_anno(accepts_match_contexts, true, I0),

    {I, Last, Blocks, State}.

aca_handle_convergence(Src, State0, Last0, Blocks0) ->
    #b_br{fail=Fail0,succ=Succ0} = Last0,

    SuccPath = beam_ssa:rpo([Succ0], Blocks0),
    FailPath = beam_ssa:rpo([Fail0], Blocks0),

    %% The promotion logic in alias_matched_binaries breaks down if the source
    %% is used after the fail/success paths converge, as we have no way to tell
    %% whether the source is a match context or something else past that point.
    %%
    %% We could handle this through clever insertion of phi nodes but it's
    %% far simpler to copy either branch in its entirety. It doesn't matter
    %% which one as long as they become disjoint.
    ConvergedPaths = ordsets:intersection(
                       ordsets:from_list(SuccPath),
                       ordsets:from_list(FailPath)),

    case maps:is_key(Src, beam_ssa:uses(ConvergedPaths, Blocks0)) of
        true ->
            case shortest(SuccPath, FailPath) of
                left ->
                    {Succ, Blocks, Counter} =
                        aca_copy_successors(Succ0, Blocks0, State0#aca.counter),
                    State = State0#aca{ counter = Counter },
                    {State, Last0#b_br{succ=Succ}, Blocks};
                right ->
                    {Fail, Blocks, Counter} =
                        aca_copy_successors(Fail0, Blocks0, State0#aca.counter),
                    State = State0#aca{ counter = Counter },
                    {State, Last0#b_br{fail=Fail}, Blocks}
            end;
        false ->
            {State0, Last0, Blocks0}
    end.

shortest([_|As], [_|Bs]) -> shortest(As, Bs);
shortest([], _) -> left;
shortest(_, []) -> right.

%% Copies all successor blocks of Lbl, returning the label to the entry block
%% of this copy. Since the copied blocks aren't referenced anywhere else, they
%% are all guaranteed to be dominated by Lbl.
aca_copy_successors(Lbl0, Blocks0, Counter0) ->
    %% Building the block rename map up front greatly simplifies phi node
    %% handling.
    Path = beam_ssa:rpo([Lbl0], Blocks0),
    {BRs, Counter1} = aca_cs_build_brs(Path, Counter0, #{}),
    {Blocks, Counter} = aca_cs_1(Path, Blocks0, Counter1, #{}, BRs, #{}),
    Lbl = maps:get(Lbl0, BRs),
    {Lbl, Blocks, Counter}.

aca_cs_build_brs([?BADARG_BLOCK=Lbl | Path], Counter, Acc) ->
    %% ?BADARG_BLOCK is a marker and not an actual block, so renaming it will
    %% break exception handling.
    aca_cs_build_brs(Path, Counter, Acc#{ Lbl => Lbl });
aca_cs_build_brs([Lbl | Path], Counter0, Acc) ->
    aca_cs_build_brs(Path, Counter0 + 1, Acc#{ Lbl => Counter0 });
aca_cs_build_brs([], Counter, Acc) ->
    {Acc, Counter}.

aca_cs_1([Lbl0 | Path], Blocks, Counter0, VRs0, BRs, Acc0) ->
    Block0 = maps:get(Lbl0, Blocks),
    Lbl = maps:get(Lbl0, BRs),
    {VRs, Block, Counter} = aca_cs_block(Block0, Counter0, VRs0, BRs),
    Acc = maps:put(Lbl, Block, Acc0),
    aca_cs_1(Path, Blocks, Counter, VRs, BRs, Acc);
aca_cs_1([], Blocks, Counter, _VRs, _BRs, Acc) ->
    {maps:merge(Blocks, Acc), Counter}.

aca_cs_block(#b_blk{is=Is0,last=Last0}=Block0, Counter0, VRs0, BRs) ->
    {VRs, Is, Counter} = aca_cs_is(Is0, Counter0, VRs0, BRs, []),
    Last = aca_cs_last(Last0, VRs, BRs),
    Block = Block0#b_blk{is=Is,last=Last},
    {VRs, Block, Counter}.

aca_cs_is([#b_set{op=Op,
                  dst=Dst0,
                  args=Args0}=I0 | Is],
          Counter0, VRs0, BRs, Acc) ->
    Args = case Op of
               phi -> aca_cs_args_phi(Args0, VRs0, BRs);
               _ -> aca_cs_args(Args0, VRs0)
           end,
    Counter = Counter0 + 1,
    Dst = #b_var{name={'@ssa_bsm_aca',Counter}},
    I = I0#b_set{dst=Dst,args=Args},
    VRs = maps:put(Dst0, Dst, VRs0),
    aca_cs_is(Is, Counter, VRs, BRs, [I | Acc]);
aca_cs_is([], Counter, VRs, _BRs, Acc) ->
    {VRs, reverse(Acc), Counter}.

aca_cs_last(#b_switch{arg=Arg0,list=Switch0,fail=Fail0}=Sw, VRs, BRs) ->
    Switch = [{Literal, maps:get(Lbl, BRs)} || {Literal, Lbl} <- Switch0],
    Sw#b_switch{arg=aca_cs_arg(Arg0, VRs),
                fail=maps:get(Fail0, BRs),
                list=Switch};
aca_cs_last(#b_br{bool=Arg0,succ=Succ0,fail=Fail0}=Br, VRs, BRs) ->
    Br#b_br{bool=aca_cs_arg(Arg0, VRs),
            succ=maps:get(Succ0, BRs),
            fail=maps:get(Fail0, BRs)};
aca_cs_last(#b_ret{arg=Arg0}=Ret, VRs, _BRs) ->
    Ret#b_ret{arg=aca_cs_arg(Arg0, VRs)}.

aca_cs_args_phi([{Arg, Lbl} | Args], VRs, BRs) ->
    case BRs of
        #{ Lbl := New } ->
            [{aca_cs_arg(Arg, VRs), New} | aca_cs_args_phi(Args, VRs, BRs)];
        #{} ->
            aca_cs_args_phi(Args, VRs, BRs)
    end;
aca_cs_args_phi([], _VRs, _BRs) ->
    [].

aca_cs_args([Arg | Args], VRs) ->
    [aca_cs_arg(Arg, VRs) | aca_cs_args(Args, VRs)];
aca_cs_args([], _VRs) ->
    [].

aca_cs_arg(#b_remote{mod=Mod0,name=Name0}=Rem, VRs) ->
    Mod = aca_cs_arg(Mod0, VRs),
    Name = aca_cs_arg(Name0, VRs),
    Rem#b_remote{mod=Mod,name=Name};
aca_cs_arg(Arg, VRs) ->
    case VRs of
        #{ Arg := New } -> New;
        #{} -> Arg
    end.

%% Allows contexts to pass through "wrapper functions" where the context is
%% passed directly to a function that accepts match contexts (including other
%% wrappers).
%%
%% This does not alter the function in any way, it only changes parameter info
%% so that skip_outgoing_tail_extraction is aware that it's safe to pass
%% contexts to us.

allow_context_passthrough({Fs, ModInfo0}) ->
    ModInfo =
        acp_forward_params([{F, beam_ssa:uses(F#b_function.bs)} || F <- Fs],
                           ModInfo0),
    {Fs, ModInfo}.

acp_forward_params(FsUses, ModInfo0) ->
    F = fun({#b_function{args=Parameters}=Func, UseMap}, ModInfo) ->
                ParamInfo =
                    foldl(fun(Param, ParamInfo) ->
                                  Uses = maps:get(Param, UseMap, []),
                                  acp_1(Param, Uses, ModInfo, ParamInfo)
                          end,
                          funcinfo_get(Func, parameter_info, ModInfo),
                          Parameters),
                funcinfo_set(Func, parameter_info, ParamInfo, ModInfo)
        end,
    %% Allowing context passthrough on one function may make it possible to
    %% enable it on another, so it needs to be repeated for maximum effect.
    case foldl(F, ModInfo0, FsUses) of
        ModInfo0 -> ModInfo0;
        Changed -> acp_forward_params(FsUses, Changed)
    end.

%% We have no way to know if an argument is a context, so it's only safe to
%% forward them if they're passed exactly once in the first block. Any other
%% uses are unsafe, including function_clause errors.
acp_1(Param, [{0, #b_set{op=call}=I}], ModInfo, ParamInfo) ->
    %% We don't need to provide a context chain as our callers make sure that
    %% multiple arguments never reference the same context.
    case check_context_call(I, Param, [], ModInfo) of
        {no_match_on_entry, _} -> ParamInfo;
        Other -> maps:put(Param, Other, ParamInfo)
    end;
acp_1(_Param, _Uses, _ModInfo, ParamInfo) ->
    ParamInfo.

%% This is conceptually similar to combine_matches but operates across
%% functions. Whenever a tail binary is passed to a parameter that accepts
%% match contexts we'll pass the context instead, improving performance by
%% avoiding the creation of a new match context in the callee.
%%
%% We also create an alias to delay extraction until it's needed as an actual
%% binary, which is often rare on the happy path. The cost of being wrong is
%% negligible (`bs_test_unit + bs_get_tail` vs `bs_get_binary`) so we're
%% applying it unconditionally to keep things simple.

-record(sote, { definitions :: beam_ssa:definition_map(),
                mod_info :: module_info(),
                match_aliases = #{} :: match_alias_map() }).

skip_outgoing_tail_extraction({Fs0, ModInfo}) ->
    Fs = map(fun(F) -> skip_outgoing_tail_extraction(F, ModInfo) end, Fs0),
    {Fs, ModInfo}.

skip_outgoing_tail_extraction(#b_function{bs=Blocks0}=F, ModInfo) ->
    case funcinfo_get(F, has_bsm_ops, ModInfo) of
        true ->
            State0 = #sote{ definitions = beam_ssa:definitions(Blocks0),
                            mod_info = ModInfo },

            {Blocks1, State} = beam_ssa:mapfold_instrs_rpo(
                                 fun sote_rewrite_calls/2, [0], State0, Blocks0),

            {Blocks, Counter} = alias_matched_binaries(Blocks1,
                                                       F#b_function.cnt,
                                                       State#sote.match_aliases),

            F#b_function{bs=Blocks,cnt=Counter};
        false ->
            F
    end.

sote_rewrite_calls(#b_set{op=call,args=Args}=Call, State) ->
    sote_rewrite_call(Call, Args, [], State);
sote_rewrite_calls(I, State) ->
    {I, State}.

sote_rewrite_call(Call, [], ArgsOut, State) ->
    {Call#b_set{args=reverse(ArgsOut)}, State};
sote_rewrite_call(Call0, [Arg | ArgsIn], ArgsOut, State0) ->
    case is_tail_binary(Arg, State0#sote.definitions) of
        true ->
            CtxChain = context_chain_of(Arg, State0#sote.definitions),
            case check_context_call(Call0, Arg, CtxChain, State0#sote.mod_info) of
                suitable_for_reuse ->
                    Ctx = match_context_of(Arg, State0#sote.definitions),

                    MatchAliases0 = State0#sote.match_aliases,
                    MatchAliases = maps:put(Arg, {0, Ctx}, MatchAliases0),
                    State = State0#sote{ match_aliases = MatchAliases },

                    Call = beam_ssa:add_anno(bsm_info, context_reused, Call0),
                    sote_rewrite_call(Call, ArgsIn, [Ctx | ArgsOut], State);
                Other ->
                    Call = beam_ssa:add_anno(bsm_info, Other, Call0),
                    sote_rewrite_call(Call, ArgsIn, [Arg | ArgsOut], State0)
            end;
        false ->
            sote_rewrite_call(Call0, ArgsIn, [Arg | ArgsOut], State0)
    end.

%% Adds parameter_type_info annotations to help the validator determine whether
%% our optimizations were safe.

annotate_context_parameters({Fs, ModInfo}) ->
    mapfoldl(fun annotate_context_parameters/2, ModInfo, Fs).

annotate_context_parameters(F, ModInfo) ->
    ParamInfo = funcinfo_get(F, parameter_info, ModInfo),
    TypeAnno0 = beam_ssa:get_anno(parameter_type_info, F, #{}),
    TypeAnno = maps:fold(fun(K, _V, Acc) when is_map_key(K, Acc) ->
                                 %% Assertion.
                                 error(conflicting_parameter_types);
                            (K, suitable_for_reuse, Acc) ->
                                 T = beam_validator:type_anno(match_context),
                                 Acc#{ K => T };
                            (_K, _V, Acc) ->
                                 Acc
                         end, TypeAnno0, ParamInfo),
    {beam_ssa:add_anno(parameter_type_info, TypeAnno, F), ModInfo}.

%%%
%%% +bin_opt_info
%%%

collect_opt_info(Fs) ->
    foldl(fun(#b_function{bs=Blocks}=F, Acc0) ->
                  UseMap = beam_ssa:uses(Blocks),
                  Where = beam_ssa:get_anno(location, F, []),
                  beam_ssa:fold_instrs_rpo(
                    fun(I, Acc) ->
                            collect_opt_info_1(I, Where, UseMap, Acc)
                    end, [0], Acc0, Blocks)
          end, [], Fs).

collect_opt_info_1(#b_set{op=Op,anno=Anno,dst=Dst}=I, Where, UseMap, Acc0) ->
    case is_tail_binary(I) of
        true when Op =:= bs_match ->
            %% The uses include when the context is passed raw, so we discard
            %% everything but the bs_extract instruction to limit warnings to
            %% unoptimized uses.
            Uses0 = maps:get(Dst, UseMap, []),
            case [E || {_, #b_set{op=bs_extract}=E} <- Uses0] of
                [Use] -> add_unopt_binary_info(Use, false, Where, UseMap, Acc0);
                [] -> Acc0
            end;
        true ->
            %% Add a warning for each use. Note that we don't do anything
            %% special if unused as a later pass will remove this instruction
            %% anyway.
            Uses = maps:get(Dst, UseMap, []),
            foldl(fun({_Lbl, Use}, Acc) ->
                          add_unopt_binary_info(Use, false, Where, UseMap, Acc)
                  end, Acc0, Uses);
        false ->
            add_opt_info(Anno, Where, Acc0)
    end;
collect_opt_info_1(#b_ret{anno=Anno}, Where, _UseMap, Acc) ->
    add_opt_info(Anno, Where, Acc);
collect_opt_info_1(_I, _Where, _Uses, Acc) ->
    Acc.

add_opt_info(Anno, Where, Acc) ->
    case maps:find(bsm_info, Anno) of
        {ok, Term} -> [make_warning(Term, Anno, Where) | Acc];
        error -> Acc
    end.

%% When an alias is promoted we need to figure out where it goes to ignore
%% warnings for compiler-generated things, and provide more useful warnings in
%% general.
%%
%% We track whether the binary has been used to build another term because it
%% can be helpful when there's no line information.

add_unopt_binary_info(#b_set{op=Follow,dst=Dst}, _Nested, Where, UseMap, Acc0)
  when Follow =:= put_tuple;
       Follow =:= put_list;
       Follow =:= put_map ->
    %% Term-building instructions.
    {_, Uses} = unzip(maps:get(Dst, UseMap, [])),
    foldl(fun(Use, Acc) ->
                  add_unopt_binary_info(Use, true, Where, UseMap, Acc)
          end, Acc0, Uses);
add_unopt_binary_info(#b_set{op=Follow,dst=Dst}, Nested, Where, UseMap, Acc0)
  when Follow =:= bs_extract;
       Follow =:= phi ->
    %% Non-building instructions that need to be followed.
    {_, Uses} = unzip(maps:get(Dst, UseMap, [])),
    foldl(fun(Use, Acc) ->
                  add_unopt_binary_info(Use, Nested, Where, UseMap, Acc)
          end, Acc0, Uses);
add_unopt_binary_info(#b_set{op=call,
                             args=[#b_remote{mod=#b_literal{val=erlang},
                                             name=#b_literal{val=error}} |
                                   _Ignored]},
                      _Nested, _Where, _UseMap, Acc) ->
    %% There's no nice way to tell compiler-generated exceptions apart from
    %% user ones so we ignore them all. I doubt anyone cares.
    Acc;
add_unopt_binary_info(#b_switch{anno=Anno}=I, Nested, Where, _UseMap, Acc) ->
    [make_promotion_warning(I, Nested, Anno, Where) | Acc];
add_unopt_binary_info(#b_set{anno=Anno}=I, Nested, Where, _UseMap, Acc) ->
    [make_promotion_warning(I, Nested, Anno, Where) | Acc];
add_unopt_binary_info(#b_ret{anno=Anno}=I, Nested, Where, _UseMap, Acc) ->
    [make_promotion_warning(I, Nested, Anno, Where) | Acc];
add_unopt_binary_info(#b_br{anno=Anno}=I, Nested, Where, _UseMap, Acc) ->
    [make_promotion_warning(I, Nested, Anno, Where) | Acc].

make_promotion_warning(I, Nested, Anno, Where) ->
    make_warning({binary_created, I, Nested}, Anno, Where).

make_warning(Term, Anno, Where) ->
    {File, Line} = maps:get(location, Anno, Where),
    {File,[{Line,?MODULE,Term}]}.

format_opt_info(context_reused) ->
    "OPTIMIZED: match context reused";
format_opt_info({binary_created, _, _}=Promotion) ->
    io_lib:format("BINARY CREATED: ~s", [format_opt_info_1(Promotion)]);
format_opt_info(Other) ->
    io_lib:format("NOT OPTIMIZED: ~s", [format_opt_info_1(Other)]).

format_opt_info_1({binary_created, #b_set{op=call,args=[Call|_]}, false}) ->
    io_lib:format("binary is used in call to ~s which doesn't support "
                  "context reuse", [format_call(Call)]);
format_opt_info_1({binary_created, #b_set{op=call,args=[Call|_]}, true}) ->
    io_lib:format("binary is used in term passed to ~s",
                  [format_call(Call)]);
format_opt_info_1({binary_created, #b_set{op={bif, BIF},args=Args}, false}) ->
    io_lib:format("binary is used in ~p/~p which doesn't support context "
                  "reuse", [BIF, length(Args)]);
format_opt_info_1({binary_created, #b_set{op={bif, BIF},args=Args}, true}) ->
    io_lib:format("binary is used in term passed to ~p/~p",
                  [BIF, length(Args)]);
format_opt_info_1({binary_created, #b_set{op=Op}, false}) ->
    io_lib:format("binary is used in '~p' which doesn't support context "
                  "reuse", [Op]);
format_opt_info_1({binary_created, #b_set{op=Op}, true}) ->
    io_lib:format("binary is used in term passed to '~p'", [Op]);
format_opt_info_1({binary_created, #b_ret{}, false}) ->
    io_lib:format("binary is returned from the function", []);
format_opt_info_1({binary_created, #b_ret{}, true}) ->
    io_lib:format("binary is used in a term that is returned from the "
                  "function", []);
format_opt_info_1({unsuitable_call, {Call, Inner}}) ->
    io_lib:format("binary used in call to ~s, where ~s",
                  [format_call(Call), format_opt_info_1(Inner)]);
format_opt_info_1({remote_call, Call}) ->
    io_lib:format("binary is used in remote call to ~s", [format_call(Call)]);
format_opt_info_1({fun_call, Call}) ->
    io_lib:format("binary is used in fun call (~s)",
                  [format_call(Call)]);
format_opt_info_1({multiple_uses_in_call, Call}) ->
    io_lib:format("binary is passed as multiple arguments to ~s",
                  [format_call(Call)]);
format_opt_info_1({no_match_on_entry, Call}) ->
    io_lib:format("binary is used in call to ~s which does not begin with a "
                  "suitable binary match", [format_call(Call)]);
format_opt_info_1({used_before_match, #b_set{op=call,args=[Call|_]}}) ->
    io_lib:format("binary is used in call to ~s before being matched",
                  [format_call(Call)]);
format_opt_info_1({used_before_match, #b_set{op={bif, BIF},args=Args}}) ->
    io_lib:format("binary is used in ~p/~p before being matched",
                  [BIF, length(Args)]);
format_opt_info_1({used_before_match, #b_set{op=phi}}) ->
    io_lib:format("binary is returned from an expression before being "
                  "matched", []);
format_opt_info_1({used_before_match, #b_set{op=Op}}) ->
    io_lib:format("binary is used in '~p' before being matched",[Op]);
format_opt_info_1(Term) ->
    io_lib:format("~w", [Term]).

format_call(#b_local{name=#b_literal{val=F},arity=A}) ->
    io_lib:format("~p/~p", [F, A]);
format_call(#b_remote{mod=#b_literal{val=M},name=#b_literal{val=F},arity=A}) ->
    io_lib:format("~p:~p/~p", [M, F, A]);
format_call(Fun) ->
    io_lib:format("~p", [Fun]).
