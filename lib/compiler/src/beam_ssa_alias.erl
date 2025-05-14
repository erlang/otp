%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2024-2025. All Rights Reserved.
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
%%

-module(beam_ssa_alias).
-moduledoc false.

-export([opt/2]).

-import(lists, [foldl/3, reverse/1]).

%% The maximum number of iterations when calculating alias
%% information.
-define(MAX_REPETITIONS, 16).

-include("beam_ssa_opt.hrl").
-include("beam_types.hrl").
-include("beam_ssa_alias_debug.hrl").

-ifdef(DEBUG_ALIAS).
-define(DP(FMT, ARGS), io:format(FMT, ARGS)).
-define(DP(FMT), io:format(FMT)).
-define(DBG(STMT), STMT).

fn(#b_local{name=#b_literal{val=N},arity=A}) ->
    io_lib:format("~p/~p", [N, A]).

-else.
-define(DP(FMT, ARGS), skip).
-define(DP(FMT), skip).
-define(DBG(STMT), skip).
-endif.

%% Alias analysis state
-record(aas, {
              caller :: func_id() | 'undefined',
              call_args = #{},
              alias_map = #{} :: alias_map(),
              func_db :: func_info_db(),
              kills :: kills_map(),
              st_map :: st_map(),
              orig_st_map :: st_map(),
              repeats = sets:new() :: sets:set(func_id()),
              %% The next unused variable name in caller
              cnt = 0 :: non_neg_integer(),
              %% Functions which have been analyzed at least once.
              analyzed = sets:new() :: sets:set(func_id()),
              run_count = #{} :: #{ func_id() => non_neg_integer() },
              prune_strategy = #{} :: #{ func_id() =>
                                             #{beam_ssa:label() =>
                                                   'add' | 'del'} },
              forced_aliases :: #{ func_id() => var_set() }
             }).

%% A code location refering to either the #b_set{} defining a variable
%% or the terminator of a block.
-type kill_loc() :: #b_var{}
                  | {terminator, beam_ssa:label()}
                  | {live_outs, beam_ssa:label()}
                  | {killed_in_block, beam_ssa:label()}.

-type var_set() :: sets:set(#b_var{}).

%% Map a code location to the set of variables which die at that
%% location.
-type kill_set() :: #{ kill_loc() => var_set() }.

%% Map a label to the set of variables which are live in to that block.
-type live_in_set() :: #{ beam_ssa:label() => var_set() }.

%% Map a flow-control edge to the set of variables which are live in
%% to the destination block due to phis.
-type phi_live_set() :: #{ {beam_ssa:label(), beam_ssa:label()} => var_set() }.

-type kills_map() :: #{ func_id() => {live_in_set(),
                                      kill_set(),
                                      phi_live_set()} }.

-type alias_map() :: #{ func_id() => lbl2ss() }.

-type lbl2ss() :: #{ beam_ssa:label() => sharing_state() }.

-type sharing_state() :: any(). % A beam_digraph graph.

-type type_db() :: #{ beam_ssa:b_var() := type() }.

%%%
%%% Optimization pass which calculates the alias status of values and
%%% uses the results to transform the code.
%%%
-spec opt(st_map(), func_info_db()) -> {st_map(), func_info_db()}.

opt(StMap0, FuncDb0) ->
    %% Ignore functions which are not in the function db (never
    %% called).
    try
        Funs = [ F || F <- maps:keys(StMap0), is_map_key(F, FuncDb0)],
        StMap1 = #{ F=>expand_record_update(OptSt) || F:=OptSt <- StMap0},
        ForcedAliases = foldl(fun(F, Acc) ->
                                      #opt_st{ssa=Is} = map_get(F, StMap1),
                                      Acc#{F=>forced_aliasing(Is)}
                              end, #{}, Funs),
        KillsMap = killsets(Funs, StMap1),
        {StMap2, FuncDb} = aa(Funs, KillsMap, StMap1, FuncDb0, ForcedAliases),
        StMap = #{ F=>restore_update_record(OptSt) || F:=OptSt <- StMap2},
        {StMap, FuncDb}
    catch
        throw:too_deep ->
            %% Give up and leave the module unmodified. Thrown by
            %% beam_ssa_ss:prune/2 when the depth of sharing state
            %% value chains exceeds a fixed limit.
            {StMap0,FuncDb0}
    end.

%%%
%%% Calculate the set of variables killed at each instruction. The
%%% algorithm traverses the basic blocks of the CFG post-order. It
%%% traverses the instructions within a basic block in reverse order,
%%% starting with the terminator. When starting the traversal of a
%%% basic block, the set of variables that are live is initialized to
%%% the variables that are live in to the block's successors. When a
%%% def for a variable is found, it is pruned from the live set. When
%%% a use which is not in the live-set is found, it is a kill. The
%%% killed variable is added to the kill set for the current
%%% instruction and added to the live set.
%%%
%%% As the only back-edges occuring in BEAM are for receives and
%%% constructing terms are not allowed within the receive loop,
%%% back-edges can be safely ignored as they won't change the alias
%%% status of any variable.
%%%

killsets(Funs, StMap) ->
    OptStates = [{F,map_get(F, StMap)} || F <- Funs],
    #{ F=>killsets_fun(reverse(SSA)) || {F,#opt_st{ssa=SSA}} <:- OptStates }.

killsets_fun(Blocks) ->
    %% Pre-calculate the live-ins due to Phi-instructions.
    PhiLiveIns = killsets_phi_live_ins(Blocks),
    killsets_blks(Blocks, #{}, #{}, PhiLiveIns).

killsets_blks([{Lbl,Blk}|Blocks], LiveIns0, Kills0, PhiLiveIns) ->
    {LiveIns,Kills} = killsets_blk(Lbl, Blk, LiveIns0, Kills0, PhiLiveIns),
    killsets_blks(Blocks, LiveIns, Kills, PhiLiveIns);
killsets_blks([], LiveIns, Kills, PhiLiveIns) ->
    {LiveIns,Kills,PhiLiveIns}.

killsets_blk(Lbl, #b_blk{is=Is0,last=L}=Blk, LiveIns0, Kills0, PhiLiveIns) ->
    Successors = beam_ssa:successors(Blk),
    Live1 = killsets_blk_live_outs(Successors, Lbl, LiveIns0, PhiLiveIns),
    Kills1 = Kills0#{{live_outs,Lbl}=>Live1},
    Is = [L|reverse(Is0)],
    {Live,Kills} = killsets_is(Is, Live1, Kills1, Lbl, sets:new()),
    LiveIns = LiveIns0#{Lbl=>Live},
    {LiveIns, Kills}.

killsets_is([#b_set{op=phi,dst=Dst}=I|Is], Live, Kills0, Lbl, KilledInBlock0) ->
    %% The Phi uses are logically located in the predecessors, so we
    %% don't want them live in to this block. But to correctly
    %% calculate the aliasing of the arguments to the Phi in this
    %% block, we need to know if the arguments live past the Phi. The
    %% kill set is stored with the key {phi,Dst}.
    Uses = beam_ssa:used(I),
    {_,LastUses} = killsets_update_live_and_last_use(Live, Uses),
    Kills = killsets_add_kills({phi,Dst}, LastUses, Kills0),
    KilledInBlock = sets:union(LastUses, KilledInBlock0),
    killsets_is(Is, sets:del_element(Dst, Live), Kills, Lbl, KilledInBlock);
killsets_is([I|Is], Live0, Kills0, Lbl, KilledInBlock0) ->
    Uses = beam_ssa:used(I),
    {Live,LastUses} = killsets_update_live_and_last_use(Live0, Uses),
    KilledInBlock = sets:union(LastUses, KilledInBlock0),
    case I of
        #b_set{dst=Dst} ->
            killsets_is(Is, sets:del_element(Dst, Live),
                        killsets_add_kills(Dst, LastUses, Kills0),
                        Lbl, KilledInBlock);
        _ ->
            killsets_is(Is, Live,
                        killsets_add_kills({terminator,Lbl}, LastUses, Kills0),
                        Lbl, KilledInBlock)
    end;
killsets_is([], Live, Kills, Lbl, KilledInBlock) ->
    {Live,killsets_add_kills({killed_in_block,Lbl}, KilledInBlock, Kills)}.

killsets_update_live_and_last_use(Live0, Uses) ->
    foldl(fun(Use, {LiveAcc,LastAcc}=Acc) ->
                  case sets:is_element(Use, LiveAcc) of
                      true ->
                          Acc;
                      false ->
                          {sets:add_element(Use, LiveAcc),
                           sets:add_element(Use, LastAcc)}
                  end
          end, {Live0,sets:new()}, Uses).

killsets_add_kills(Dst, LastUses, Kills) ->
    Kills#{Dst=>LastUses}.

%%%
%%% Pre-calculate the live-ins due to Phi-instructions in order to
%%% avoid having to repeatedly scan the first instruction(s) of a
%%% basic block in order to find them when calculating live-in sets.
%%%
killsets_phi_live_ins(Blocks) ->
    killsets_phi_live_ins(Blocks, #{}).

killsets_phi_live_ins([{Lbl,#b_blk{is=Is}}|Blocks], PhiLiveIns0) ->
    killsets_phi_live_ins(Blocks,
                          killsets_phi_uses_in_block(Lbl, Is, PhiLiveIns0));
killsets_phi_live_ins([], PhiLiveIns) ->
    PhiLiveIns.

killsets_phi_uses_in_block(Lbl, [#b_set{op=phi,args=Args}|Is], PhiLiveIns0) ->
    PhiLiveIns = foldl(fun({#b_var{}=Var,From}, Acc) ->
                               Key = {From,Lbl},
                               Old = case Acc of
                                         #{Key:=O} -> O;
                                         #{} -> sets:new()
                                     end,
                               Acc#{Key=>sets:add_element(Var, Old)};
                          ({#b_literal{},_},Acc) ->
                               Acc
                       end, PhiLiveIns0, Args),
    killsets_phi_uses_in_block(Lbl, Is, PhiLiveIns);
killsets_phi_uses_in_block(_Lbl, _, PhiLiveIns) ->
    %% No more phis.
    PhiLiveIns.

%% Create a set of variables which are live out from this block.
killsets_blk_live_outs(Successors, ThisBlock, LiveIns, PhiLiveIns) ->
    killsets_blk_live_outs(Successors, ThisBlock, LiveIns,
                           PhiLiveIns, sets:new()).

killsets_blk_live_outs([Successor|Successors],
                       ThisBlock, LiveIns, PhiLiveIns, Acc0) ->
    Acc = case LiveIns of
              #{Successor:=LI} ->
                  Tmp = sets:union(Acc0, LI),
                  case PhiLiveIns of
                      #{{ThisBlock,Successor}:=PhiUses} ->
                          sets:union(Tmp, PhiUses);
                      #{} ->
                          Tmp
                  end;
              #{} ->
                  %% This is a back edge, we can ignore it as it only occurs
                  %% in combination with a receive.
                  Acc0
          end,
    killsets_blk_live_outs(Successors, ThisBlock, LiveIns, PhiLiveIns, Acc);
killsets_blk_live_outs([], _, _, _, Acc) ->
    Acc.

%%%
%%% Perform an alias analysis of the given functions, alias
%%% information is added as annotations on the SSA code.
%%%
%%% Alias analysis is done by an algorithm inspired by Kotzmann and
%%% Mössenböck's 2005 algorithm for Escape Analysis
%%% (https://www.usenix.org/events/vee05/full_papers/p111-kotzmann.pdf),
%%% particularly their escape equivalent sets. But in contrast to
%%% Kotzmann and Mössenböck, instead of just tracking escaping values
%%% we track if a value in a variable is unique and/or aliased.
%%%
%%% A variable is said to be unique if it currently is the only live
%%% variable pointing to a particular term on the heap. Literals and
%%% non-boxed terms are considered unique.
%%%
%%% A variable is said to be aliased if it points to a heap term which
%%% can be reached by other means than the boxed pointer in the
%%% variable.
%%%
%%% For this pass, a data structure called a Sharing State (in the
%%% code the variable holding it is usually called SS) is used to
%%% describe the alias status of SSA variables. The Sharing State
%%% maintains information about:
%%%
%%% * Whether a variable is unique or aliased.
%%%
%%% * For variables which are unique and derived from other variables
%%%   (that is, contained in or contains another term), it makes it
%%%   possible to identify the variable for the contained/containing
%%%   term. This includes information making it possible to avoid
%%%   false aliasing when a tuple is deconstructed although
%%%   technically, the variable holding tuple itself aliases its
%%%   elements until all elements are extracted.
%%%
%%% The alias analysis is performed by traversing the functions in the
%%% module and their code. The basic blocks of a function are
%%% traversed in reverse post order. When the end of a block is
%%% reached, the current sharing state is propagated to the block's
%%% successors by merging the sharing state of all the successor
%%% block's predecessors' sharing states.
%%%
%%% When all blocks have been visited and the sharing states at the
%%% end of each block are known, information about aliased variables
%%% are propagated along the edges of the execution graph during a
%%% post order traversal of the basic blocks.

-spec aa([func_id()], kills_map(), st_map(), func_info_db(),
         #{func_id() => var_set()}) ->
          {st_map(), func_info_db()}.

aa(Funs, KillsMap, StMap, FuncDb, ForcedAliases) ->
    %% Set up the argument info to make all incoming arguments to
    %% exported functions aliased and all non-exported functions
    %% unique.
    ArgsInfoIn =
        foldl(
          fun(F=#b_local{}, Acc) ->
                  #func_info{exported=E,arg_types=AT} = map_get(F, FuncDb),
                  S = case E of
                          true -> aliased;
                          false -> no_info
                      end,
                  Acc#{F=>beam_ssa_ss:initialize_in_args([S || _ <- AT])}
          end, #{}, Funs),
    AAS = #aas{call_args=ArgsInfoIn,
               func_db=FuncDb,kills=KillsMap,
               st_map=StMap, orig_st_map=StMap,
               forced_aliases=ForcedAliases},
    aa_fixpoint(Funs, AAS).

%%%
%%% Alias analysis works on the whole module and uses its own fixpoint
%%% loop instead of the fixpoint abstraction in beam_ssa_opt. The
%%% reason for this is three-fold:
%%%
%%% * The termination condition is simpler: the alias map hasn't
%%%   changed.
%%%
%%% * Adapting the alias analysis to fit into the beam_ssa_opt
%%%   fixpoint framework would require it to be expressed as three
%%%   passes in the style of ssa_opt_type_start,
%%%   ssa_opt_type_continue, and ssa_opt_type_finish in order to
%%%   create and maintain the state which is now kept in #aas{}.
%%%
%%% * As the beam_ssa_opt fixpoint framework doesn't provide a way for
%%%   an optimization to be informed that the fixpoint calculation
%%%   didn't converge within the interation limit and it is unsafe to
%%%   do optimizations on incomplete unique/aliased information it is
%%%   much simpler to explicitly handle it locally instead of trying
%%%   to detect incomplete information in a hypothetical
%%%   ssa_opt_alias_finish pass.
%%%
aa_fixpoint(Funs, AAS=#aas{func_db=FuncDb}) ->
    Order = aa_order(Funs, FuncDb),
    ?DP("Traversal order:~n  ~s~n",
        [string:join([fn(F) || F <- Order], ",\n  ")]),
    aa_fixpoint(Order, reverse(Order), AAS, 1).

aa_fixpoint([F|Fs], Order,
            AAS0=#aas{func_db=FuncDb,st_map=StMap,
                      repeats=Repeats,run_count=RC}, NoofIters) ->

    ?DP("-= ~s =-~n", [fn(F)]),
    %% If, when analysing another function, this function has been
    %% scheduled for revisiting, we can remove it, as we otherwise
    %% revisit it again in the next iteration.
    AAS1 = AAS0#aas{caller=F,repeats=sets:del_element(F, Repeats)},

    St = #opt_st{ssa=_Is} = map_get(F, StMap),
    ?DP("code:~n~p.~n", [_Is]),
    AAS = aa_fun(F, St, AAS1),
    ?DP("Done ~s~n", [fn(F)]),
    case maps:get(F, RC, ?MAX_REPETITIONS) of
        0 ->
            ?DP("**** End of iteration, too many iterations ****~n"),
            {StMap, FuncDb};
        N ->
            aa_fixpoint(Fs, Order,
                        AAS#aas{run_count=RC#{F => N - 1}}, NoofIters)
    end;
aa_fixpoint([], Order, #aas{func_db=FuncDb,repeats=Repeats}=AAS, NoofIters) ->
    %% Following the depth first order, select those in Repeats.
    case [Id || Id <- Order, sets:is_element(Id, Repeats)] of
        [] ->
            ?DP("**** Fixpoint reached after ~p traversals ****~n",
                [NoofIters]),
            {StMap,_} = aa_update_annotations(Order, AAS),
            {StMap, FuncDb};
        NewOrder ->
            ?DP("**** Starting traversal ~p ****~n", [NoofIters + 1]),
            aa_fixpoint(NewOrder, reverse(Order),
                        AAS#aas{repeats=sets:new()}, NoofIters + 1)
    end.

aa_fun(F, #opt_st{ssa=Linear0,args=Args},
       AAS0=#aas{alias_map=AliasMap0,analyzed=Analyzed,kills=KillsMap,
                 prune_strategy=StrategyMap0,forced_aliases=ForcedAliases}) ->
    %% Initially assume all formal parameters are unique for a
    %% non-exported function, if we have call argument info in the
    %% AAS, we use it. For an exported function, all arguments are
    %% assumed to be aliased.
    {SS0,Cnt} = aa_init_fun_ss(Args, F, AAS0),
    #{F:={LiveIns,Kills,PhiLiveIns}} = KillsMap,
    Strategy0 = maps:get(F, StrategyMap0, #{}),
    {SS,Strategy,AAS1} =
        aa_blocks(Linear0, LiveIns, PhiLiveIns,
                  Kills, #{0=>SS0}, Strategy0, AAS0#aas{cnt=Cnt},
                  maps:get(F, ForcedAliases)),
    Lbl2SS0 = maps:get(F, AliasMap0, #{}),
    Type2Status0 = maps:get(returns, Lbl2SS0, #{}),
    Type2Status = maps:get(returns, SS, #{}),
    AAS = case Type2Status0 =/= Type2Status of
              true ->
                  aa_schedule_revisit_callers(F, AAS1);
              false ->
                  AAS1
          end,
    AliasMap = AliasMap0#{ F => SS },
    StrategyMap = StrategyMap0#{F => Strategy},
    AAS#aas{alias_map=AliasMap,analyzed=sets:add_element(F, Analyzed),
            prune_strategy=StrategyMap}.

%% Main entry point for the alias analysis
aa_blocks([{?EXCEPTION_BLOCK,_}|Bs],
          LiveIns, PhiLiveIns, Kills, Lbl2SS, Strategy, AAS, ForcedAliases) ->
    %% Nothing happening in the exception block can propagate to the
    %% other block.
    aa_blocks(Bs, LiveIns, PhiLiveIns, Kills, Lbl2SS,
              Strategy, AAS, ForcedAliases);
aa_blocks([{L,#b_blk{is=Is0,last=T}}|Bs0],
          LiveIns, PhiLiveIns, Kills, Lbl2SS0, Strategy0, AAS0,
          ForcedAliases) ->
    #{L:=SS0} = Lbl2SS0,
    ?DP("Block: ~p~nSS:~n~s~n", [L, beam_ssa_ss:dump(SS0)]),
    {FullSS,AAS1} = aa_is(Is0, SS0, AAS0, ForcedAliases),
    #{{live_outs,L}:=LiveOut} = Kills,
    {Lbl2SS1,Successors} = aa_terminator(T, FullSS, Lbl2SS0),
    %% In around 80% of the cases when prune is called, more than half
    %% of the nodes in the sharing state database survive. Therefore
    %% we default to a pruning strategy which removes nodes from the
    %% database. But if only a few nodes survive it is faster to
    %% recreate the pruned state from scratch. We therefore track the
    %% result of a previous prune for the current basic block and
    %% select the, hopefully, best pruning strategy.
    Before = beam_ssa_ss:size(FullSS),
    S = maps:get(L, Strategy0, del),
    PrunedSS =
        case S of
            del ->
                #{{killed_in_block,L}:=KilledInBlock} = Kills,
                beam_ssa_ss:prune(LiveOut, KilledInBlock, FullSS);
            add ->
                beam_ssa_ss:prune_by_add(LiveOut, FullSS)
        end,
    After = beam_ssa_ss:size(PrunedSS),
    Strategy = case After < (Before div 2) of
                   true when S =:= add ->
                       Strategy0;
                   false when S =:= del ->
                       Strategy0;
                   true ->
                       Strategy0#{L => add};
                   false ->
                       Strategy0#{L => del}
               end,
    ?DP("Live out from ~p: ~p~n", [L, sets:to_list(LiveOut)]),
    Lbl2SS2 = aa_add_block_entry_ss(Successors, L, PrunedSS,
                                    LiveOut, LiveIns, PhiLiveIns, Lbl2SS1),
    Lbl2SS = aa_set_block_exit_ss(L, FullSS, Lbl2SS2),
    aa_blocks(Bs0, LiveIns, PhiLiveIns, Kills, Lbl2SS,
              Strategy, AAS1, ForcedAliases);
aa_blocks([], _LiveIns, _PhiLiveIns, _Kills, Lbl2SS,
          Strategy, AAS, _ForcedAliases) ->
    {Lbl2SS, Strategy, AAS}.

aa_is([_I=#b_set{dst=Dst,op=Op,args=Args,anno=Anno0}|Is], SS0,
      AAS0, ForcedAliases) ->
    ?DP("I: ~p~n", [_I]),
    {SS3, AAS} =
        case Op of
            %% Instructions changing the alias status.
            {bif,Bif} ->
                Types = maps:get(arg_types, Anno0, #{}),
                {aa_bif(Dst, Bif, Args, Types, SS0, AAS0), AAS0};
            bs_create_bin ->
                case Args of
                    [#b_literal{val=Flag},_,Arg|_] when
                          Flag =:= private_append ; Flag =:= append ->
                        case aa_all_dies([Arg], Dst, AAS0) of
                            true ->
                                %% Inherit the status of the argument
                                {aa_derive_from(Dst, Arg, SS0), AAS0};
                            false ->
                                %% We alias with the surviving arg
                                {aa_set_aliased([Dst|Args], SS0), AAS0}
                        end;
                    _ ->
                        %% TODO: Too conservative?
                        {aa_set_aliased([Dst|Args], SS0), AAS0}
                end;
            bs_extract ->
                {aa_set_aliased([Dst|Args], SS0), AAS0};
            bs_get_tail ->
                {aa_set_aliased([Dst|Args], SS0), AAS0};
            bs_match ->
                {aa_set_aliased([Dst|Args], SS0), AAS0};
            bs_start_match ->
                [_,Bin] = Args,
                {aa_set_aliased([Dst,Bin], SS0), AAS0};
            build_stacktrace ->
                SS1 = beam_ssa_ss:add_var(Dst, unique, SS0),
                %% build_stacktrace can potentially alias anything
                %% live at this point in the code. We handle it by
                %% aliasing everything known to us. Touching
                %% variables which are dead is harmless.
                {aa_alias_all(SS1), AAS0};
            call ->
                SS1 = beam_ssa_ss:add_var(Dst, unique, SS0),
                aa_call(Dst, Args, Anno0, SS1, AAS0);
            'catch_end' ->
                [_Tag,Arg] = Args,
                {aa_derive_from(Dst, Arg, SS0), AAS0};
            extract ->
                [Arg,_] = Args,
                {aa_derive_from(Dst, Arg, SS0), AAS0};
            get_hd ->
                [Arg] = Args,
                Type = maps:get(0, maps:get(arg_types, Anno0, #{0=>any}), any),
                {aa_pair_extraction(Dst, Arg, hd, Type, SS0), AAS0};
            get_map_element ->
                [Map,_Key] = Args,
                {aa_map_extraction(Dst, Map, SS0, AAS0), AAS0};
            get_tl ->
                [Arg] = Args,
                Type = maps:get(0, maps:get(arg_types, Anno0, #{0=>any}), any),
                {aa_pair_extraction(Dst, Arg, tl, Type, SS0), AAS0};
            get_tuple_element ->
                [Arg,Idx] = Args,
                Types = maps:get(arg_types, Anno0, #{}),
                {aa_tuple_extraction(Dst, Arg, Idx, Types, SS0), AAS0};
            landingpad ->
                {aa_set_aliased(Dst, SS0), AAS0};
            make_fun ->
                SS1 = beam_ssa_ss:add_var(Dst, unique, SS0),
                [Callee|Env] = Args,
                aa_make_fun(Dst, Callee, Env, SS1, AAS0);
            peek_message ->
                {aa_set_aliased(Dst, SS0), AAS0};
            phi ->
                aa_phi(Dst, Args, SS0, AAS0);
            put_list ->
                SS1 = beam_ssa_ss:add_var(Dst, unique, SS0),
                Types =
                    aa_map_arg_to_type(Args, maps:get(arg_types, Anno0, #{})),
                {aa_construct_pair(Dst, Args, Types, SS1, AAS0), AAS0};
            put_map ->
                {aa_construct_term(Dst, Args, SS0, AAS0), AAS0};
            put_tuple ->
                SS1 = beam_ssa_ss:add_var(Dst, unique, SS0),
                Types = aa_map_arg_to_type(Args,
                                           maps:get(arg_types, Anno0, #{})),
                Values = lists:enumerate(0, Args),
                {aa_construct_tuple(Dst, Values, Types, SS1, AAS0), AAS0};
            update_tuple ->
                {aa_construct_term(Dst, Args, SS0, AAS0), AAS0};
            update_record ->
                SS1 = beam_ssa_ss:add_var(Dst, unique, SS0),
                [#b_literal{val=Hint},_Size,Src|Updates] = Args,
                RecordType = maps:get(arg_types, Anno0, #{}),
                ?DP("UPDATE RECORD dst: ~p, src: ~p, type:~p~n",
                    [Dst,Src,RecordType]),
                Values = aa_update_record_get_vars(Updates),
                ?DP("values: ~p~n", [Values]),
                Types = aa_map_arg_to_type(Args, RecordType),
                ?DP("updates: ~p~n", [Updates]),
                ?DP("type-mapping: ~p~n", [Types]),
                SS2 = aa_construct_tuple(Dst, Values, Types, SS1, AAS0),
                case Hint of
                    reuse ->
                        %% If the reuse hint is set and the source
                        %% doesn't die here, both Src and Dst become
                        %% aliased, as the VM could just leave Src
                        %% unchanged and move it to Dst.
                        KillSet = aa_killset_for_instr(Dst, AAS0),
                        case sets:is_element(Src, KillSet) of
                            true ->
                                {SS2,AAS0};
                            false ->
                                {aa_set_status([Dst,Src], aliased,  SS2), AAS0}
                        end;
                    copy ->
                        {SS2,AAS0}
                end;

            %% Instructions which don't change the alias status
            {float,_} ->
                {SS0, AAS0};
            {succeeded,_} ->
                {SS0, AAS0};
            bs_init_writable ->
                {SS0, AAS0};
            bs_test_tail ->
                {SS0, AAS0};
            debug_line ->
                {SS0, AAS0};
            executable_line ->
                {SS0, AAS0};
            has_map_field ->
                {SS0, AAS0};
            is_nonempty_list ->
                {SS0, AAS0};
            is_tagged_tuple ->
                {SS0, AAS0};
            kill_try_tag ->
                {SS0, AAS0};
            match_fail ->
                {SS0, AAS0};
            new_try_tag ->
                {SS0, AAS0};
            nif_start ->
                {SS0, AAS0};
            raw_raise ->
                {SS0, AAS0};
            recv_marker_bind ->
                {SS0, AAS0};
            recv_marker_clear ->
                {SS0, AAS0};
            recv_marker_reserve ->
                {SS0, AAS0};
            recv_next ->
                {SS0, AAS0};
            remove_message ->
                {SS0, AAS0};
            resume ->
                {SS0, AAS0};
            wait_timeout ->
                {SS0, AAS0}
        end,
    SS = case sets:is_element(Dst, ForcedAliases) of
             true ->
                 aa_set_aliased(Dst, SS3);
             false ->
                 SS3
         end,
    ?DP("Post I: ~p.~n      ~p~n", [_I, SS]),
    aa_is(Is, SS, AAS, ForcedAliases);
aa_is([], SS, AAS, _ForcedAliases) ->
    {SS, AAS}.

aa_terminator(#b_br{succ=S,fail=S}, _SS, Lbl2SS) ->
    {Lbl2SS,[S]};
aa_terminator(#b_br{succ=S,fail=F}, _SS, Lbl2SS) ->
    {Lbl2SS,[S,F]};
aa_terminator(#b_ret{arg=Arg,anno=Anno0}, SS, Lbl2SS0) ->
    Type = maps:get(result_type, Anno0, any),
    Type2Status0 = maps:get(returns, Lbl2SS0, #{}),
    Status0 = case Type2Status0 of
                  #{ Type := OtherStatus } ->
                      OtherStatus;
                  #{ } ->
                      no_info
              end,
    [Status] = beam_ssa_ss:merge_in_args([Arg], [Status0], SS),
    Type2Status = Type2Status0#{ Type => Status },
    ?DP("Returned ~p:~p:~p~n", [Arg, Status, Type]),
    ?DP("New status map: ~p~n", [Type2Status]),
    Lbl2SS = Lbl2SS0#{ returns => Type2Status },
    {Lbl2SS, []};
aa_terminator(#b_switch{fail=F,list=Ls}, _SS, Lbl2SS) ->
    {Lbl2SS,[F|[L || {_,L} <:- Ls]]}.

%% Store the updated SS for the point where execution leaves the
%% block.
aa_set_block_exit_ss(ThisBlockLbl, SS, Lbl2SS) ->
    Lbl2SS#{ThisBlockLbl=>SS}.

%% Extend the SS valid on entry to the blocks in the list with NewSS.
aa_add_block_entry_ss([?EXCEPTION_BLOCK|BlockLabels], From, NewSS,
                      LiveOut, LiveIns, PhiLiveIns, Lbl2SS) ->
    aa_add_block_entry_ss(BlockLabels, From, NewSS, LiveOut,
                          LiveIns, PhiLiveIns, Lbl2SS);
aa_add_block_entry_ss([L|BlockLabels], From,
                      NewSS, LiveOut, LiveIns, PhiLiveIns, Lbl2SS) ->
    #{L:=LiveIn} = LiveIns,
    PhiLiveIn = case PhiLiveIns of
                    #{{From,L}:=Vs} -> Vs;
                    #{} -> sets:new()
                end,
    AllLiveIn = sets:union(LiveIn, PhiLiveIn),
    KilledOnEdge = sets:subtract(LiveOut, AllLiveIn),
    ?DP("Killed on edge to ~p: ~p~n", [L, sets:to_list(KilledOnEdge)]),
    ?DP("Live on edge to ~p: ~p~n", [L, sets:to_list(AllLiveIn)]),
    Pruned = beam_ssa_ss:prune(AllLiveIn, KilledOnEdge, NewSS),
    aa_add_block_entry_ss(BlockLabels, From, NewSS, LiveOut, LiveIns,
                          PhiLiveIns, aa_merge_ss(L, Pruned, Lbl2SS));
aa_add_block_entry_ss([], _, _, _, _, _, Lbl2SS) ->
    Lbl2SS.

%% Merge two sharing states when traversing the execution graph
%% reverse post order.
aa_merge_ss(BlockLbl, NewSS, Lbl2SS) when is_map_key(BlockLbl, Lbl2SS) ->
    Lbl2SS#{BlockLbl=>beam_ssa_ss:merge(NewSS, map_get(BlockLbl, Lbl2SS))};
aa_merge_ss(BlockLbl, NewSS, Lbl2SS) ->
    Lbl2SS#{BlockLbl=>NewSS}.

%% Merge two sharing states when traversing the execution graph post
%% order. The only thing the successor merging needs to to is to check
%% if variables in the original SS have become aliased.
aa_merge_ss_successor(BlockLbl, NewSS, Lbl2SS) ->
    #{BlockLbl:=OrigSS} = Lbl2SS,
    Lbl2SS#{BlockLbl=>beam_ssa_ss:forward_status(OrigSS, NewSS)}.

aa_get_status(V=#b_var{}, State) ->
    beam_ssa_ss:get_status(V, State);
aa_get_status(#b_literal{}, _State) ->
    unique.

aa_get_status(V, State, Types) ->
    case aa_is_plain_value(V, Types) of
        true ->
            unique;
        false ->
            aa_get_status(V, State)
    end.

%% aa_get_status but for instructions extracting values from pairs and
%% tuples.
aa_get_element_extraction_status(V=#b_var{}, State) ->
    aa_get_status(V, State);
aa_get_element_extraction_status(#b_literal{}, _State) ->
    unique.

aa_set_status(V=#b_var{}, Status, State) ->
    ?DP("Setting ~p to ~p.~n", [V, Status]),
    beam_ssa_ss:set_status(V, Status, State);
aa_set_status(#b_literal{}, _Status, State) ->
    State;
aa_set_status(plain, _Status, State) ->
    State;
aa_set_status([X|T], Status, State) ->
    aa_set_status(X, Status, aa_set_status(T, Status, State));
aa_set_status([], _, State) ->
    State.

aa_derive_from(Dst, Parents, State0) ->
    aa_derive_from(Dst, Parents, #{}, State0).

aa_derive_from(Dst, [Parent|Parents], Types, State0) ->
    aa_derive_from(Dst, Parents, Types,
                   aa_derive_from1(Dst, Parent, Types, State0));
aa_derive_from(_Dst, [], _, State0) ->
    State0;
aa_derive_from(Dst, Parent, Types, State0) ->
    aa_derive_from1(Dst, Parent, Types, State0).

aa_derive_from1(#b_var{}=Dst, #b_literal{val=Val}, _, State) when is_map(Val) ->
    aa_set_aliased(Dst, State);
aa_derive_from1(#b_var{}, #b_literal{}, _, State) ->
    State;
aa_derive_from1(Dst, Parent, Types, State) ->
    false = aa_is_plain_value(Parent, Types), %% Assertion
    beam_ssa_ss:derive_from(#b_var{}=Dst, #b_var{}=Parent, State).

aa_update_annotations(Funs, #aas{alias_map=AliasMap0,st_map=StMap0}=AAS) ->
    foldl(fun(F, {StMapAcc,AliasMapAcc}) ->
                  #{F:=Lbl2SS0} = AliasMapAcc,
                  #{F:=OptSt0} = StMapAcc,
                  ?DP("Updating annotations for ~s~n", [fn(F)]),
                  {OptSt,Lbl2SS} =
                      aa_update_fun_annotation(OptSt0, Lbl2SS0,
                                               AAS#aas{caller=F}),
                  {StMapAcc#{F=>OptSt},AliasMapAcc#{F=>Lbl2SS}}
          end, {StMap0,AliasMap0}, Funs).

aa_update_fun_annotation(#opt_st{ssa=SSA0}=OptSt0, Lbl2SS0, AAS) ->
    %% Propagate alias information from successor to predecessor by
    %% traversing the code post-order.
    {SSA,Lbl2SS} = aa_update_annotation_blocks(reverse(SSA0), [], Lbl2SS0, AAS),
    {OptSt0#opt_st{ssa=SSA},Lbl2SS}.

aa_update_annotation_blocks([{?EXCEPTION_BLOCK,_}=Block|Blocks],
                            Acc, Lbl2SS, AAS) ->
    %% There is no point in touching the exception block.
    aa_update_annotation_blocks(Blocks, [Block|Acc], Lbl2SS, AAS);
aa_update_annotation_blocks([{Lbl, Block0}|Blocks], Acc, Lbl2SS0, AAS) ->
    Successors = beam_ssa:successors(Block0),
    ?DP("Block ~p, successors: ~p.~n", [Lbl, Successors]),
    Lbl2SS = foldl(fun(?EXCEPTION_BLOCK, Lbl2SSAcc) ->
                           %% What happens in the exception block
                           %% can't influence anything in any of the
                           %% parents.
                           Lbl2SSAcc;
                      (Successor, Lbl2SSAcc) ->
                           #{Successor:=OtherSS} = Lbl2SSAcc,
                           aa_merge_ss_successor(Lbl, OtherSS, Lbl2SSAcc)
                   end, Lbl2SS0, Successors),
    #{Lbl:=SS} = Lbl2SS,
    Block = aa_update_annotation_block(Block0, SS, AAS),
    ?DP("Block ~p done.~n", [Lbl]),
    aa_update_annotation_blocks(Blocks, [{Lbl,Block}|Acc], Lbl2SS, AAS);
aa_update_annotation_blocks([], Acc, Lbl2SS, _AAS) ->
    {Acc,Lbl2SS}.

aa_update_annotation_block(#b_blk{is=Linear,last=Last}=Blk, SS, AAS) ->
    Blk#b_blk{is=[aa_update_annotation(I, SS, AAS) || I <- Linear],
              last=aa_update_annotation(Last, SS, AAS)}.

aa_update_annotation(I=#b_set{args=[Tuple,Idx],op=get_tuple_element},
                     SS, AAS) ->
    Args = [{Tuple,aa_get_element_extraction_status(Tuple, SS)},
            {Idx,aa_get_status(Idx, SS)}],
    aa_update_annotation1(Args, I, AAS);
aa_update_annotation(I=#b_set{args=[Idx,Tuple],op={bif,element}}, SS, AAS) ->
    Args = [{Idx,aa_get_status(Idx, SS)},
            {Tuple,aa_get_element_extraction_status(Tuple, SS)}],
    aa_update_annotation1(Args, I, AAS);
aa_update_annotation(I=#b_set{args=[Pair],op=get_hd}, SS, AAS) ->
    Args = [{Pair,aa_get_element_extraction_status(Pair, SS)}],
    aa_update_annotation1(Args, I, AAS);
aa_update_annotation(I=#b_set{args=[Pair],op=get_tl}, SS, AAS) ->
    Args = [{Pair,aa_get_element_extraction_status(Pair, SS)}],
    aa_update_annotation1(Args, I, AAS);
aa_update_annotation(I=#b_set{args=[Pair],op={bif,hd}}, SS, AAS) ->
    Args = [{Pair,aa_get_element_extraction_status(Pair, SS)}],
    aa_update_annotation1(Args, I, AAS);
aa_update_annotation(I=#b_set{args=[Pair],op={bif,tl}}, SS, AAS) ->
    Args = [{Pair,aa_get_element_extraction_status(Pair, SS)}],
    aa_update_annotation1(Args, I, AAS);
aa_update_annotation(I=#b_set{args=Args0,anno=Anno,dst=_Dst}, SS, AAS) ->
    Types = maps:get(arg_types, Anno, #{}),
    Arg2Type = #{V=>maps:get(Idx, Types, any)
                 || {Idx,#b_var{}=V} <- lists:enumerate(0, 1, Args0)},
    Args = [{V,aa_get_status(V, SS, Arg2Type)} || #b_var{}=V <- Args0],
    ?DP("Args with status for ~p: ~p~n", [_Dst, Args]),
    aa_update_annotation1(Args, I, AAS);
aa_update_annotation(I=#b_ret{arg=#b_var{}=V}, SS, AAS) ->
    aa_update_annotation1(aa_get_status(V, SS), I, AAS);
aa_update_annotation(I, _SS, _AAS) ->
    %% For now we don't care about the other terminators.
    I.

aa_update_annotation1(ArgsStatus,
                      I=#b_set{anno=Anno0,args=Args,op=Op}, AAS) ->
    Anno1 = foldl(fun({#b_var{}=V,S}, Acc) ->
                          aa_update_annotation_for_var(V, S, Acc);
                     (_, Acc) ->
                          Acc
                  end, Anno0, ArgsStatus),
    %% Alias analysis indicate the alias status of the instruction
    %% arguments before the instruction is executed. For transforms in
    %% later stages, we need to know if a particular argument dies
    %% with this instruction or not. As we have the kill map available
    %% during this analysis pass, it is more efficient to add an
    %% annotation now, instead of trying to reconstruct the
    %% kill map during the later transform pass.
    Anno = case {Op,Args} of
               {bs_create_bin,[#b_literal{val=append},_,Var|_]} ->
                   %% For the private-append optimization we need to
                   %% know if the first fragment dies.
                   Anno1#{first_fragment_dies => dies_at(Var, I, AAS)};
               {update_record,[_Hint,_Size,Src|_Updates]} ->
                   %% One of the requirements for valid destructive
                   %% record updates is that the source tuple dies
                   %% with the update.
                   Anno1#{source_dies => dies_at(Src, I, AAS)};
               _ ->
                   Anno1
           end,
    I#b_set{anno=Anno};
aa_update_annotation1(Status, I=#b_ret{arg=#b_var{}=V,anno=Anno0}, _AAS) ->
    Anno = aa_update_annotation_for_var(V, Status, Anno0),
    I#b_ret{anno=Anno}.

aa_update_annotation_for_var(Var, Status, Anno0) ->
    Aliased0 = maps:get(aliased, Anno0, []),
    Unique0 = maps:get(unique, Anno0, []),
    {Aliased, Unique} = case Status of
                            aliased ->
                                {ordsets:add_element(Var, Aliased0),
                                 ordsets:del_element(Var, Unique0)};
                            unique ->
                                {ordsets:del_element(Var, Aliased0),
                                 ordsets:add_element(Var, Unique0)};
                            no_info ->
                                {ordsets:del_element(Var, Aliased0),
                                 ordsets:del_element(Var, Unique0)}
                        end,
    Anno1 = case Aliased of
                [] ->
                    maps:remove(aliased, Anno0);
                _ ->
                    Anno0#{aliased=>Aliased}
            end,
    case Unique of
        [] ->
            maps:remove(unique, Anno1);
        _ ->
            Anno1#{unique=>Unique}
    end.

%% Return true if Var dies with its use (assumed, not checked) in the
%% instruction.
dies_at(Var, #b_set{dst=Dst}, AAS) ->
    #aas{caller=Caller,kills=KillsMap} = AAS,
    {_LiveIns,KillMap,_PhiLiveIns} = map_get(Caller, KillsMap),
    sets:is_element(Var, map_get(Dst, KillMap)).

aa_set_aliased(Args, SS) ->
    aa_set_status(Args, aliased, SS).

aa_alias_all(SS) ->
    aa_set_aliased(beam_ssa_ss:variables(SS), SS).

%%
%% Type is always less specific or exactly the same as one of the
%% types in StatusByType, so we need to meet all possible statuses for
%% the call site.
%%
aa_get_status_by_type(none, _StatusByType) ->
    %% The function did not return, conservatively report the status
    %% as aliased.
    aliased;
aa_get_status_by_type(Type, StatusByType) ->
    Statuses = [Status || Candidate := Status <- StatusByType,
                          beam_types:meet(Type, Candidate) =/= none],
    case Statuses of
        [] ->
            %% No matching type was found, this can happen when the
            %% returned type, for example, is a #t_union{}. For now,
            %% conservatively return a status of aliased.
            aliased;
        _ ->
            beam_ssa_ss:meet_in_args(Statuses)
    end.

aa_alias_surviving_phi_args(Args, Call, SS, AAS) ->
    KillSet = aa_killset_for_instr(Call, AAS),
    aa_alias_surviving_phi_args1(Args, SS, KillSet).

aa_alias_surviving_phi_args1([#b_var{}=A|Args], SS0, KillSet) ->
    SS = case sets:is_element(A, KillSet) of
             true ->
                 SS0;
             false ->
                 aa_set_status(A, aliased, SS0)
         end,
    aa_alias_surviving_phi_args1(Args, SS, KillSet);
aa_alias_surviving_phi_args1([_|Args], SS, KillSet) ->
    aa_alias_surviving_phi_args1(Args, SS, KillSet);
aa_alias_surviving_phi_args1([], SS, _KillSet) ->
    SS.

aa_alias_surviving_args(Args, Call, SS0, AAS) ->
    SS = aa_alias_surviving_phi_args(Args, Call, SS0, AAS),
    %% Compared to phi arguments, function arguments are all used, and
    %% if the same variable is used more than once, it becomes
    %% aliased.
    aa_alias_repeated_args(Args, SS, sets:new()).

aa_alias_repeated_args([#b_var{}=A|Args], SS0, Seen) ->
    SS = case sets:is_element(A, Seen) of
             true ->
                 aa_set_status(A, aliased, SS0);
             false ->
                 SS0
         end,
    aa_alias_repeated_args(Args, SS, sets:add_element(A, Seen));
aa_alias_repeated_args([_|Args], SS, Seen) ->
    aa_alias_repeated_args(Args, SS, Seen);
aa_alias_repeated_args([], SS, _Seen) ->
    SS.

%% Return the kill-set for the instruction defining Dst.
aa_killset_for_instr(Dst, #aas{caller=Caller,kills=Kills}) ->
    {_LiveIns,KillMap,_PhiLiveIns} = map_get(Caller, Kills),
    map_get(Dst, KillMap).

%% Predicate to check if all variables in `Vars` dies at `Where`.
-spec aa_all_dies([#b_var{}], kill_loc(), type_db(), #aas{}) -> boolean().
aa_all_dies(Vars, Where, Types, AAS) ->
    KillSet = aa_killset_for_instr(Where, AAS),
    aa_all_dies1(Vars, Types, KillSet).

%% As aa_all_dies/4 but without type information.
aa_all_dies(Vars, Where, AAS) ->
    aa_all_dies(Vars, Where, #{}, AAS).

aa_all_dies1([#b_literal{}|Vars], Types, KillSet) ->
    aa_all_dies1(Vars, Types, KillSet);
aa_all_dies1([#b_var{}=V|Vars], Types, KillSet) ->
    case aa_dies(V, Types, KillSet) of
        true ->
            aa_all_dies1(Vars, Types, KillSet);
        false ->
            false
    end;
aa_all_dies1([], _, _) ->
    true.

aa_dies(V, Types, KillSet) ->
    sets:is_element(V, KillSet) orelse aa_is_plain_value(V, Types).

aa_alias_if_args_dont_die(Args, Where, SS, AAS) ->
    case aa_all_dies(Args, Where, AAS) of
        true ->
            SS;
        false ->
            aa_set_aliased([Where|Args], SS)
    end.

%% Dst inherits the alias status of Arg, if Arg doesn't die here, it
%% becomes aliased by default.
aa_alias_inherit_and_alias_if_arg_does_not_die(Dst, Arg, SS0, AAS) ->
    SS1 = aa_alias_if_args_dont_die([Arg], Dst, SS0, AAS),
    aa_set_status(Dst, aa_get_status(Arg, SS1), SS1).

%% Check that a variable in Args only occurs once and that it is not
%% aliased, literals, values of types which fit into a register are
%% ignored.
aa_all_vars_unique(Args, Types, SS) ->
    aa_all_vars_unique(Args, #{}, Types, SS).

aa_all_vars_unique([#b_literal{}|Args], Seen, Types, SS) ->
    aa_all_vars_unique(Args, Seen, Types, SS);
aa_all_vars_unique([#b_var{}=V|Args], Seen, Types, SS) ->
    aa_get_status(V, SS) =:= unique andalso
        case Seen of
            #{ V := _ } ->
                false;
            #{} ->
                aa_all_vars_unique(Args, Seen#{V => true }, Types, SS)
        end;
aa_all_vars_unique([], _, _, _) ->
    true.

%% Predicate to test whether a variable is of a type which is just a
%% value or behaves as it was (for example pid, ports and references).
aa_is_plain_value(V, Types) ->
    case Types of
        #{V:=Type} ->
            aa_is_plain_type(Type);
        #{} ->
            false
    end.

aa_is_plain_type(Type) ->
    case Type of
        #t_atom{} ->
            true;
        #t_number{} ->
            true;
        #t_integer{} ->
            true;
        #t_float{} ->
            true;
        'identifier' ->
            true;
        'pid' ->
            true;
        'port' ->
            true;
        'reference' ->
            true;
        _ ->
            false
    end.

aa_map_arg_to_type(Args, Types) ->
    aa_map_arg_to_type(Args, Types, #{}, 0).

aa_map_arg_to_type([A|Args], Types, Acc0, Idx) ->
    Acc = case Types of
              #{Idx:=T} ->
                  Acc0#{A=>T};
              #{} ->
                  Acc0
          end,
    aa_map_arg_to_type(Args, Types, Acc, Idx+1);
aa_map_arg_to_type([], _, Acc, _) ->
    Acc.

aa_construct_term(Dst, Values, SS, AAS) ->
    aa_construct_term(Dst, Values, #{}, SS, AAS).

aa_construct_term(Dst, Values, Types, SS, AAS) ->
    ?DP("Constructing term in ~p~n values: ~p~n  types: ~p~n  au: ~p, ad: ~p~n",
        [Dst, Values, Types, aa_all_vars_unique(Values, Types, SS),
         aa_all_dies(Values, Dst, Types, AAS)]),
    case aa_all_vars_unique(Values, Types, SS)
        andalso aa_all_dies(Values, Dst, Types, AAS) of
        true ->
            ?DP("  deriving ~p from ~p~n", [Dst, Values]),
            aa_derive_from(Dst, Values, Types, SS);
        false ->
            Alias = [V || V <- [Dst|Values], not aa_is_plain_value(V, Types)],
            ?DP("  aliasing ~p~n", [Alias]),
            aa_set_aliased(Alias, SS)
    end.

aa_construct_tuple(Dst, IdxValues, Types, SS, AAS) ->
    KillSet = aa_killset_for_instr(Dst, AAS),
    ?DP("Constructing tuple in ~p~n from: ~p~n",
        [Dst, [#{idx=>Idx,v=>V,status=>aa_get_status(V, SS, Types),
                 killed=>aa_dies(V, Types, KillSet),
                 plain=>aa_is_plain_value(V, Types)}
               || {Idx,V} <:- IdxValues]]),
    ?DP("~s~n", [beam_ssa_ss:dump(SS)]),
    aa_build_tuple_or_pair(Dst, IdxValues, Types, KillSet, SS, []).

aa_build_tuple_or_pair(Dst, [{Idx,#b_literal{val=Lit}}|IdxValues], Types,
                       KillSet, SS0, Sources)
  when is_atom(Lit); is_number(Lit); is_map(Lit);
       is_bitstring(Lit); is_function(Lit); Lit =:= [] ->
    aa_build_tuple_or_pair(Dst, IdxValues, Types, KillSet,
                           SS0, [{Idx,plain}|Sources]);
aa_build_tuple_or_pair(Dst, [{Idx,V}=IdxVar|IdxValues], Types,
                       KillSet, SS0, Sources) ->
    case aa_is_plain_value(V, Types) of
        true ->
            %% Does not need to be tracked.
            aa_build_tuple_or_pair(Dst, IdxValues, Types,
                                   KillSet, SS0, [{Idx,plain}|Sources]);
        false ->
            SS = case aa_dies(V, Types, KillSet) of
                     true ->
                         SS0;
                     false ->
                         aa_set_aliased(V, SS0)
                 end,
            aa_build_tuple_or_pair(Dst, IdxValues, Types,
                                   KillSet, SS, [IdxVar|Sources])
    end;
aa_build_tuple_or_pair(Dst, [], _Types, _KillSet, SS, Sources) ->
    ?DP("  embedding ~p~n", [Sources]),
    R = beam_ssa_ss:embed_in(Dst, Sources, SS),
    R.

aa_construct_pair(Dst, Args0, Types, SS, AAS) ->
    KillSet = aa_killset_for_instr(Dst, AAS),
    [Hd,Tl] = Args0,
    ?DP("Constructing pair in ~p~n from ~p and ~p~n~s~n",
        [Dst, Hd, Tl, beam_ssa_ss:dump(SS)]),
    Args = [{hd,Hd},{tl,Tl}],
    aa_build_tuple_or_pair(Dst, Args, Types, KillSet, SS, []).

aa_update_record_get_vars([#b_literal{val=I}, Value|Updates]) ->
    [{I-1,Value}|aa_update_record_get_vars(Updates)];
aa_update_record_get_vars([]) ->
    [].

aa_bif(Dst, element, [#b_literal{val=Idx},Tuple], _Types, SS, _AAS)
  when is_integer(Idx), Idx > 0 ->
    %% The element bif is always rewritten to a get_tuple_element
    %% instruction when the index is an integer and the second
    %% argument is a known to be a tuple. Therefore this code is only
    %% reached when the type of Tuple is unknown, thus there is no
    %% point in trying to provide aa_tuple_extraction/5 with type
    %% information.
    aa_tuple_extraction(Dst, Tuple, #b_literal{val=Idx-1}, #{}, SS);
aa_bif(Dst, element, [#b_literal{},Tuple], _Types, SS, _AAS) ->
    %% This BIF will fail, but in order to avoid any later transforms
    %% making use of uniqueness, conservatively alias.
    aa_set_aliased([Dst,Tuple], SS);
aa_bif(Dst, element, [#b_var{},Tuple], _Types, SS, _AAS) ->
    aa_set_aliased([Dst,Tuple], SS);
aa_bif(Dst, hd, [Pair], _Types, SS, _AAS) ->
    %% The hd bif is always rewritten to a get_hd instruction when the
    %% argument is known to be a pair. Therefore this code is only
    %% reached when the type of Pair is unknown, thus there is no
    %% point in trying to provide aa_pair_extraction/5 with type
    %% information.
    aa_pair_extraction(Dst, Pair, hd, SS);
aa_bif(Dst, tl, [Pair], _Types, SS, _AAS) ->
    %% The tl bif is always rewritten to a get_tl instruction when the
    %% argument is known to be a pair. Therefore this code is only
    %% reached when the type of Pair is unknown, thus there is no
    %% point in trying to provide aa_pair_extraction/5 with type
    %% information.
    aa_pair_extraction(Dst, Pair, tl, SS);
aa_bif(Dst, map_get, [_Key,Map], _Types, SS, AAS) ->
    aa_map_extraction(Dst, Map, SS, AAS);
aa_bif(Dst, binary_part, Args, _Types, SS0, _AAS) ->
    %% bif:binary_part/{2,3} is the only guard bif which could lead to
    %% aliasing, it extracts a sub-binary with a reference to its
    %% argument.
    SS = beam_ssa_ss:add_var(Dst, unique, SS0),
    aa_set_aliased([Dst|Args], SS);
aa_bif(Dst, Bif, Args, Types, SS, _AAS) ->
    Arity = length(Args),
    case erl_internal:guard_bif(Bif, Arity)
        orelse erl_internal:bool_op(Bif, Arity)
        orelse (erl_internal:comp_op(Bif, Arity)
                andalso comp_bif_can_avoid_aliasing(Args, Types))
        orelse erl_internal:arith_op(Bif, Arity)
        orelse erl_internal:new_type_test(Bif, Arity) of
        true ->
            SS;
        false ->
            %% Assume anything else shares the arguments and returns an
            %% aliased result.
            aa_set_aliased([Dst|Args], SS)
    end.

comp_bif_can_avoid_aliasing([LHS, RHS], Types) ->
    %% A comparison can see in-place mutated terms if it has to do
    %% anything except compare tags.
    LHSTy = to_general_type(maps:get(0, Types, any), LHS),
    RHSTy = to_general_type(maps:get(1, Types, any), RHS),
    case {LHSTy,RHSTy} of
        %% We compare a non-aggregate with an aggregate, this is a tag
        %% test, so it cannot "see" destructively updated terms.
        {non_aggregate,_} ->
            true;
        {_,non_aggregate} ->
            true;

        %% For anything else, we trigger aliasing as we could be
        %% traversing an aggregate term which has destructively
        %% updated elements.
        _ ->
            false
    end.

to_general_type(any, #b_literal{val=V}) ->
    to_general_type(beam_types:make_type_from_value(V));
to_general_type(T, _) ->
    to_general_type(T).

to_general_type(any) ->
    any;
to_general_type(T=#t_bs_matchable{}) ->
    T;
to_general_type(T=#t_bitstring{}) ->
    T;
to_general_type(T=#t_tuple{}) ->
    T;
to_general_type(T=#t_list{}) ->
    T;
to_general_type(T=#t_cons{}) ->
    T;
to_general_type(#t_union{tuple_set=none,list=none,other=none}) ->
    non_aggregate;
to_general_type(#t_union{}) ->
    any;
to_general_type(_) ->
    non_aggregate.

aa_phi(Dst, Args0, SS0, #aas{cnt=Cnt0}=AAS) ->
    %% TODO: Use type info?
    Args = [V || {V,_} <:- Args0],
    ?DP("Phi~n"),
    SS1 = aa_alias_surviving_phi_args(Args, {phi,Dst}, SS0, AAS),
    ?DP("  after aa_alias_surviving_args:~n~s.~n", [beam_ssa_ss:dump(SS1)]),
    {SS,Cnt} = beam_ssa_ss:phi(Dst, Args, SS1, Cnt0),
    {SS,AAS#aas{cnt=Cnt}}.

aa_call(Dst, [#b_local{}=Callee|Args], Anno, SS0,
        #aas{alias_map=AliasMap,analyzed=Analyzed,
             st_map=StMap,cnt=Cnt0}=AAS0) ->
    ?DP("A Call~n  callee: ~s~n  args: ~p~n", [fn(Callee), Args]),
    ?DP("  caller args: ~p~n", [Args]),
    SS1 = aa_alias_surviving_args(Args, Dst, SS0, AAS0),
    ?DP("  caller ss before call:~n~s.~n", [beam_ssa_ss:dump(SS1)]),
    #aas{alias_map=AliasMap} = AAS =
        aa_add_call_info(Callee, Args, SS1, AAS0),
    case sets:is_element(Callee, Analyzed) of
        true ->
            ?DP("  The callee has been analyzed~n"),
            #opt_st{args=_CalleeArgs} = map_get(Callee, StMap),
            ?DP("  callee args: ~p~n", [_CalleeArgs]),
            #{Callee:=#{0:=_CalleeSS}=Lbl2SS} = AliasMap,
            ?DP("  callee ss:~n~s~n", [beam_ssa_ss:dump(_CalleeSS)]),
            ?DP("  caller ss after call:~n~s~n", [beam_ssa_ss:dump(SS1)]),

            ReturnStatusByType = maps:get(returns, Lbl2SS, #{}),
            ?DP("  status by type: ~p~n", [ReturnStatusByType]),
            ReturnedType = maps:get(result_type, Anno, any),
            %% ReturnedType is always less specific or exactly the
            %% same as one of the types in ReturnStatusByType.
            ?DP("  returned type: ~s~n",
                [beam_ssa_pp:format_type(ReturnedType)]),
            ResultStatus = aa_get_status_by_type(ReturnedType,
                                                 ReturnStatusByType),
            ?DP("  result status: ~p~n", [ResultStatus]),
            {SS,Cnt} =
                beam_ssa_ss:set_call_result(Dst, ResultStatus, SS1, Cnt0),
            ?DP("~s~n", [beam_ssa_ss:dump(SS)]),
            {SS, AAS#aas{cnt=Cnt}};
        false ->
            ?DP("  The callee has not been analyzed~n"),
            %% We don't know anything about the function, so
            %% explicitly mark that we don't know anything about the
            %% result.
            {beam_ssa_ss:set_status(Dst, no_info, SS0), AAS}
    end;
aa_call(_Dst, [#b_remote{mod=#b_literal{val=erlang},
                         name=#b_literal{val=exit},
                         arity=1}|_], _Anno, SS, AAS) ->
    %% The function will never return, so nothing that happens after
    %% this can influence the aliasing status.
    {SS, AAS};
aa_call(Dst, [_Callee|Args], _Anno, SS0, AAS) ->
    %% This is either a call to a fun or to an external function,
    %% assume that result always escapes and
    %% all arguments escape, if they survive.
    SS = aa_alias_surviving_args(Args, Dst, SS0, AAS),
    {aa_set_aliased([Dst], SS), AAS}.

%% Incorporate aliasing information for the arguments to a call when
%% analysing the body of a function into the global state.
aa_add_call_info(Callee, Args, SS0,
                 #aas{call_args=InInfo0,caller=_Caller}=AAS) ->
    #{Callee := InStatus0} = InInfo0,
    ?DP("Adding call info for ~s when called by ~s~n"
        "  args: ~p.~n  ss:~n~s.~n",
        [fn(Callee), fn(_Caller), Args, beam_ssa_ss:dump(SS0)]),
    InStatus = beam_ssa_ss:merge_in_args(Args, InStatus0, SS0),
    ?DP("  orig in-info:~n  ~p.~n", [InStatus0]),
    ?DP("  updated in-info for ~s:~n    ~p.~n", [fn(Callee), InStatus]),
    case InStatus0 =/= InStatus of
        true ->
            InInfo = InInfo0#{Callee => InStatus},
            aa_schedule_revisit(Callee, AAS#aas{call_args=InInfo});
        false ->
            AAS
    end.

aa_init_fun_ss(Args, FunId, #aas{call_args=Info,st_map=StMap}) ->
    #{FunId:=ArgsStatus} = Info,
    #{FunId:=#opt_st{cnt=Cnt}} = StMap,
    ?DP("aa_init_fun_ss: ~s~n  args: ~p~n  status: ~p~n  cnt: ~p~n",
        [fn(FunId), Args, ArgsStatus, Cnt]),
    beam_ssa_ss:new(Args, ArgsStatus, Cnt).

%% Pair extraction.
aa_pair_extraction(Dst, Pair, Element, SS) ->
    aa_pair_extraction(Dst, Pair, Element, any, SS).

aa_pair_extraction(Dst, #b_var{}=Pair, Element, Type, SS) ->
    IsPlainValue = case {Type,Element} of
                       {#t_cons{terminator=Ty},tl} ->
                           aa_is_plain_type(Ty);
                       {#t_cons{type=Ty},hd} ->
                           aa_is_plain_type(Ty);
                       _ ->
                           %% There is no type information,
                           %% conservatively assume this isn't a plain
                           %% value.
                           false
                   end,
    case IsPlainValue of
        true ->
            %% A plain value was extracted, it doesn't change the
            %% alias status of Dst nor the pair.
            SS;
        false ->
            beam_ssa_ss:extract(Dst, Pair, Element, SS)
    end;
aa_pair_extraction(_Dst, #b_literal{}, _Element, _, SS) ->
    SS.

aa_map_extraction(Dst, Map, SS, AAS) ->
    aa_derive_from(
      Dst, Map,
      aa_alias_inherit_and_alias_if_arg_does_not_die(Dst, Map, SS, AAS)).

%% Extracting elements from a tuple.
aa_tuple_extraction(Dst, #b_var{}=Tuple, #b_literal{val=I}, Types, SS) ->
    TupleType = maps:get(0, Types, any),
    TypeIdx = I+1, %% In types tuple indices starting at zero.
    IsPlainValue = case TupleType of
                       #t_tuple{elements=#{TypeIdx:=T}} when T =/= any ->
                           aa_is_plain_type(T);
                       _ ->
                           %% There is no type information,
                           %% conservatively assume this isn't a plain
                           %% value.
                           false
                   end,
    ?DP("tuple-extraction dst:~p, tuple: ~p, idx: ~p,~n"
        "  type: ~p,~n  plain: ~p~n",
        [Dst, Tuple, I, TupleType, IsPlainValue]),
    case IsPlainValue of
        true ->
            %% A plain value was extracted, it doesn't change the
            %% alias status of Dst nor the tuple.
            SS;
        false ->
            beam_ssa_ss:extract(Dst, Tuple, I, SS)
    end;
aa_tuple_extraction(_, #b_literal{}, _, _, SS) ->
    SS.

aa_make_fun(Dst, Callee=#b_local{name=#b_literal{}},
            Env0, SS0, AAS0=#aas{call_args=Info0}) ->
    %% When a value is copied into the environment of a fun we assume
    %% that it has been aliased as there is no obvious way to track
    %% and ensure that the value is only used once, even if the
    %% argument dies at this location.
    %%
    %% We also assume that all arguments are aliased because someone
    %% could have stolen the function through tracing and called it
    %% with unexpected arguments, which may be aliased.
    SS = aa_set_aliased([Dst|Env0], SS0),
    #{ Callee := Status0 } = Info0,
    Status = [aliased || _ <- Status0],
    #{ Callee := PrevStatus } = Info0,
    Info = Info0#{ Callee := Status },
    AAS1 = case PrevStatus =/= Status of
               true ->
                   %% We have new information for the callee, we
                   %% have to revisit it.
                   aa_schedule_revisit(Callee, AAS0);
               false ->
                   AAS0
              end,
    {SS, AAS1#aas{call_args=Info}}.

aa_order(Funs, FuncDb) ->
    %% Information about the alias status flows from callers to
    %% callees and then back through the status of the result.  In
    %% order to avoid the most pessimistic estimate of the aliasing
    %% status we want to process callers before callees with one
    %% exception: zero-arity functions are always processed before
    %% their callers as they are likely to give the most precise alias
    %% information to their callers.
    IsExportedNoLocalCallers =
        fun (F) ->
                #{ F := #func_info{exported=E,in=In} } = FuncDb,
                E andalso In =:= []
        end,
    ExportedNoLocalCallers =
        lists:sort([ F || F <- Funs, IsExportedNoLocalCallers(F)]),
    IsExportedLocalCallers =
        fun (F) ->
                #{ F := #func_info{exported=E,in=In} } = FuncDb,
                E andalso In =/= []
        end,

    ZeroArityFunctions = lists:sort([ F || #b_local{arity=0}=F <- Funs]),
    ExportedLocalCallers =
        lists:sort([ F || F <- Funs, IsExportedLocalCallers(F)]),
    aa_reverse_post_order(ZeroArityFunctions ++ ExportedNoLocalCallers,
                          ExportedLocalCallers,
                          sets:new(), FuncDb).

aa_reverse_post_order([F|Work], Next, Seen, FuncDb) ->
    case sets:is_element(F, Seen) of
        true ->
            aa_reverse_post_order(Work, Next, Seen, FuncDb);
        false ->
            case FuncDb of
                #{ F := #func_info{out=Children} } ->
                    [F|aa_reverse_post_order(
                         Work, Children ++ Next,
                         sets:add_element(F, Seen), FuncDb)];
                #{} ->
                    %% Other optimization steps can have determined
                    %% that the function is not called and removed it
                    %% from the funcdb, but it still remains in the
                    %% #func_info{} of the (at the syntax-level)
                    %% caller.
                    aa_reverse_post_order(Work, Next, Seen, FuncDb)
            end
    end;
aa_reverse_post_order([], [], _Seen, _FuncDb) ->
    [];
aa_reverse_post_order([], Next, Seen, FuncDb) ->
    aa_reverse_post_order(Next, [], Seen, FuncDb).

%% Schedule a function for revisting.
aa_schedule_revisit(FuncId,
                    #aas{func_db=FuncDb,st_map=StMap,repeats=Repeats}=AAS)
  when is_map_key(FuncId, FuncDb), is_map_key(FuncId, StMap) ->
    ?DP("Scheduling ~s for revisit~n", [fn(FuncId)]),
    AAS#aas{repeats=sets:add_element(FuncId, Repeats)};
aa_schedule_revisit(_, AAS) ->
    AAS.

%% Schedule the callers of a function for revisting.
aa_schedule_revisit_callers(FuncId, #aas{func_db=FuncDb}=AAS) ->
    #{FuncId:=#func_info{in=In}} = FuncDb,
    ?DP("Scheduling callers of ~s for revisit: ~s.~n",
        [fn(FuncId), string:join([fn(I) || I <- In], ", ")]),
    foldl(fun aa_schedule_revisit/2, AAS, In).

expand_record_update(#opt_st{ssa=Linear0,cnt=First,anno=Anno0}=OptSt) ->
    {Linear,Cnt} = eru_blocks(Linear0, First),
    Anno = Anno0#{orig_cnt=>First},
    OptSt#opt_st{ssa=Linear,cnt=Cnt,anno=Anno}.

eru_blocks(Linear, First) ->
    eru_blocks(Linear, First, []).

eru_blocks([{Lbl,#b_blk{is=Is0}=Blk}|Rest], First, Acc) ->
    {Is,Next} = eru_is(Is0, First, []),
    eru_blocks(Rest, Next, [{Lbl,Blk#b_blk{is=Is}}|Acc]);
eru_blocks([], Cnt, Acc) ->
    {reverse(Acc),Cnt}.

eru_is([#b_set{op=update_record,
               args=[_Hint,#b_literal{val=Size},Src|Updates]=Args,
               anno=#{arg_types:=ArgTypes0}=Anno0}=I0|Rest], First, Acc) ->
    TupleType = maps:get(2, ArgTypes0, any),
    {Extracts,ExtraArgs,Next,ArgTypes} =
        eru_args(Updates, First, Src, Size, TupleType, ArgTypes0),
    I = I0#b_set{args=Args++ExtraArgs,anno=Anno0#{arg_types=>ArgTypes}},
    eru_is(Rest, Next, [I|Extracts]++Acc);
eru_is([I|Rest], First, Acc) ->
    eru_is(Rest, First, [I|Acc]);
eru_is([], First, Acc) ->
    {reverse(Acc), First}.

eru_args(Updates, First, Src, Size, TupleType, ArgTypes) ->
    eru_args1(Updates, sets:from_list(lists:seq(1, Size)),
              4, First, Src, TupleType, ArgTypes).

eru_args1([#b_literal{val=Idx},_Val|Updates],
          Remaining, ArgIdx, First, Src, TupleType, ArgTypes) ->
    eru_args1(Updates, sets:del_element(Idx, Remaining), ArgIdx+2,
              First, Src, TupleType, ArgTypes);
eru_args1([], Remaining, ArgIdx, First, Src, TupleType, ArgTypes) ->
    eru_args2(sets:to_list(Remaining), [], [], ArgIdx,
              First, Src, TupleType, ArgTypes).

eru_args2([Idx|Remaining], Extracts, Args0, ArgIdx, First,
          Src, TupleType, ArgTypes0) ->
    Dst = #b_var{name=First},
    I = #b_set{dst=Dst,op=get_tuple_element,
               args=[Src,#b_literal{val=Idx-1}],
               anno=#{arg_types=>#{0=>TupleType}}},
    ArgTypes = case TupleType of
                   #t_tuple{elements=#{Idx:=ET}} ->
                       ArgTypes0#{ArgIdx=>ET};
                   _ ->
                       ArgTypes0
               end,
    %% built in reverse to make argument indexes end up in the right
    %% order after the final reverse.
    Args = [Dst,#b_literal{val=Idx}|Args0],
    eru_args2(Remaining, [I|Extracts], Args,
              ArgIdx+2, First+1, Src, TupleType, ArgTypes);
eru_args2([], Extracts, Args, _, First, _, _, ArgTypes) ->
    {Extracts,reverse(Args),First,ArgTypes}.

restore_update_record(#opt_st{ssa=Linear,anno=Anno}=OptSt) ->
    Limit = map_get(orig_cnt, Anno),
    OptSt#opt_st{ssa=rur_blocks(Linear, Limit),
                 cnt=Limit,anno=maps:remove(orig_cnt, Anno)}.

rur_blocks([{Lbl,#b_blk{is=Is}=Blk}|Rest], Limit) ->
    [{Lbl,Blk#b_blk{is=rur_is(Is, Limit)}}|rur_blocks(Rest, Limit)];
rur_blocks([], _) ->
    [].

rur_is([#b_set{dst=#b_var{name=Name},op=get_tuple_element}|Rest], Limit)
  when is_integer(Name), Name >= Limit ->
    rur_is(Rest, Limit);
rur_is([#b_set{op=update_record,
               args=[Hint,Size,Src|Updates],
               anno=Anno0}=I0|Rest], Limit) ->
    Anno = rur_filter_anno(
             rur_filter_anno(Anno0, unique, Limit),
             aliased, Limit),
    Args = [Hint,Size,Src] ++ rur_args(Updates, Limit),
    I = I0#b_set{args=Args,anno=Anno},
    [I|rur_is(Rest, Limit)];
rur_is([I|Rest], Limit) ->
    [I|rur_is(Rest, Limit)];
rur_is([], _) ->
    [].

rur_filter_anno(Anno, Key, Limit) ->
    Vars = maps:get(Key, Anno, []),
    case rur_filter_synthetic(Vars, Limit) of
        [] ->
            maps:remove(Key, Anno);
        Vs ->
            Anno#{Key=>Vs}
    end.

rur_filter_synthetic([#b_var{name=N}|Rest], Limit)
  when is_integer(N), N >= Limit ->
    rur_filter_synthetic(Rest, Limit);
rur_filter_synthetic([V|Rest], Limit) ->
    [V|rur_filter_synthetic(Rest, Limit)];
rur_filter_synthetic([], _) ->
    [].

rur_args([_,#b_var{name=Name}|Updates], Limit)
  when is_integer(Name), Name >= Limit ->
    rur_args(Updates, Limit);
rur_args([Idx,V|Updates], Limit) ->
    [Idx,V|rur_args(Updates, Limit)];
rur_args([], _) ->
    [].

%%
%% Detect when the same element is extracted from a tuple multiple
%% times in a function. Normally the CSE pass ensures that this is
%% only done once, but sometimes it decides that it is more efficient
%% to keep the tuple around and extract the element again. This
%% interacts badly with the alias analysis which takes care to
%% minimize the database it keeps about aliasing status to variables
%% that are live, and can therefore in rare cases fail to detect
%% aliasing.
%%
%% Instead of complicating and slowing down the main alias analysis,
%% we do a once over on all functions and detect when the same field
%% is extracted twice and store the afflicted variables in a
%% set. During the main alias analysis pass we consult the set and
%% forcibly alias the variable when it is defined.
%%
forced_aliasing(Linear) ->
    forced_aliasing(Linear, #{0=>#{}}, sets:new()).

forced_aliasing([{Lbl,#b_blk{last=Last,is=Is}}|Rest], SeenDb0, ToExtend0) ->
    #{Lbl:=Seen0} = SeenDb0,
    Successors = fa_successors(Last),
    {Seen,ToExtend} = forced_aliasing_is(Is, Seen0, ToExtend0),
    SeenDb = foldl(fun(Succ, Acc) -> fa_merge(Seen, Succ, Acc) end,
                   SeenDb0, Successors),
    forced_aliasing(Rest, SeenDb, ToExtend);
forced_aliasing([], _Seen, ToExtend) ->
    ToExtend.

forced_aliasing_is([#b_set{op=get_tuple_element,dst=Dst,args=[Src,Idx]}|Is],
                   Seen, ToExtend0) ->
    Aliases = forced_aliasing_get_aliases(Src, Idx, Seen),
    ToExtend = forced_aliasing_extend_to(Dst, Aliases, ToExtend0),
    forced_aliasing_is(Is,
                       forced_aliasing_add_seen(Src, Idx, Dst, Seen),
                       ToExtend);
forced_aliasing_is([#b_set{op=phi,dst=Dst,args=Args}|Is], Seen0, ToExtend) ->
    %% If elements are extracted from the Phi-value, behave as if the
    %% same elements were extracted from the in-values.
    Seen =
        foldl(
          fun({Src,_}, Acc0) ->
                  ExtractedIdxs = maps:get(Src, Acc0, []),
                  foldl(
                    fun(Idx, Acc1) ->
                            forced_aliasing_add_seen(Src, Idx, Dst, Acc1)
                    end, Acc0, ExtractedIdxs)
          end, Seen0, Args),
    forced_aliasing_is(Is, Seen, ToExtend);
forced_aliasing_is([_|Is], Seen, ToExtend) ->
    forced_aliasing_is(Is, Seen, ToExtend);
forced_aliasing_is([], Seen, ToExtend) ->
    {Seen,ToExtend}.

forced_aliasing_get_aliases(Src, Idx, Seen) ->
    Key = {extracts,Src,Idx},
    case Seen of
        #{Key:=Aliases} ->
            Aliases;
        #{} ->
            []
    end.

forced_aliasing_add_seen(Src, Idx, Dst, Seen0) ->
    Key = {extracts,Src,Idx},
    Seen0#{Key=>ordsets:add_element(Dst, maps:get(Key, Seen0, [])),
           Src=>ordsets:add_element(Idx, maps:get(Src, Seen0, []))}.

forced_aliasing_extend_to(_, [], ToExtend) ->
    ToExtend;
forced_aliasing_extend_to(Dst, Aliases, ToExtend) ->
    foldl(fun sets:add_element/2,
          sets:add_element(Dst, ToExtend), Aliases).

fa_successors(#b_ret{}) ->
    [];
fa_successors(#b_br{succ=S,fail=F}) ->
    [S,F];
fa_successors(#b_switch{list=Ls,fail=F}) ->
    [F|[L || {_,L} <:- Ls]].

fa_merge(Seen, Succ, SeenDb) ->
    Other = maps:get(Succ, SeenDb, #{}),
    SeenDb#{Succ=>maps:merge_with(
                    fun(_, A, B) ->
                            ordsets:union(A, B)
                    end,
                    Seen, Other)}.

