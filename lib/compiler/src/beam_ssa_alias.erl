%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2023. All Rights Reserved.
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

-import(lists, [foldl/3, reverse/1, zip/2]).

%% The maximum number of iterations when calculating alias
%% information.
-define(MAX_REPETITIONS, 16).

-include("beam_ssa_opt.hrl").
-include("beam_types.hrl").

%% Uncomment the following to get trace printouts.

%% -define(DEBUG, true).

-ifdef(DEBUG).
-define(DP(FMT, ARGS), io:format(FMT, ARGS)).
-define(DP(FMT), io:format(FMT)).
-else.
-define(DP(FMT, ARGS), skip).
-define(DP(FMT), skip).
-endif.

%% Uncomment the following to get trace printouts when states are
%% merged.

%% -define(TRACE_MERGE, true).

-ifdef(TRACE_MERGE).
-define(TM_DP(FMT, ARGS), io:format(FMT, ARGS)).
-define(TM_DP(FMT), io:format(FMT)).
-else.
-define(TM_DP(FMT, ARGS), skip).
-define(TM_DP(FMT), skip).
-endif.

%% Uncomment the following to check that all invariants for the state
%% hold when a state are and has been updated. These checks are
%% expensive and not enabled by default.

%% -define(EXTRA_ASSERTS, true).

-ifdef(EXTRA_ASSERTS).
-define(aa_assert_ss(SS), aa_assert_ss(SS)).
-define(ASSERT(Assert), Assert).
-else.
-define(aa_assert_ss(SS), SS).
-define(ASSERT(Assert), skip).
-endif.

-type call_args_status_map() :: #{ #b_local{} => ['aliased' | 'unique'] }.

%% Alias analysis state
-record(aas, {
              caller :: func_id() | 'undefined',
              call_args = #{} :: call_args_status_map(),
              alias_map = #{} :: alias_map(),
              func_db :: func_info_db(),
              kills :: kills_map(),
              st_map :: st_map(),
              orig_st_map :: st_map(),
              repeats = sets:new([{version,2}]) :: sets:set(func_id())
             }).

%% A code location refering to either the #b_set{} defining a variable
%% or the terminator of a block.
-type kill_loc() :: #b_var{}
                  | {terminator, beam_ssa:label()}
                  | {live_outs, beam_ssa:label()}.

%% Map a code location to the set of variables which die at that
%% location.
-type kill_set() :: #{ kill_loc() => sets:set(#b_var{}) }.

-type kills_map() :: #{ func_id() => kill_set() }.

-type alias_map() :: #{ func_id() => lbl2ss() }.

-type lbl2ss() :: #{ beam_ssa:label() => sharing_state() }.

%% The sharing state for a variable.
-record(vas, {
              status :: 'unique' | 'aliased' | 'as_parent',
              parents = [] :: ordsets:ordset(#b_var{}),
              child = none :: #b_var{} | 'none',
              extracted = [] :: ordsets:ordset(#b_var{}),
              tuple_elems = [] :: ordsets:ordset({non_neg_integer(),#b_var{}}),
              pair_elems = none :: 'none'
                                 | {'hd',#b_var{}}
                                 | {'tl',#b_var{}}
                                 | {'both',#b_var{},#b_var{}}
             }).

-type sharing_state() :: #{ #b_var{} => #vas{} }.

%%%
%%% Optimization pass which calculates the alias status of values and
%%% uses the results to transform the code.
%%%
-spec opt(st_map(), func_info_db()) -> {st_map(), func_info_db()}.

opt(StMap0, FuncDb0) ->
    %% Ignore functions which are not in the function db (never
    %% called).
    Funs = [ F || F <- maps:keys(StMap0), is_map_key(F, FuncDb0)],
    KillsMap = killsets(Funs, StMap0),
    aa(Funs, KillsMap, StMap0, FuncDb0).

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
    #{ F=>killsets_fun(reverse(SSA)) || {F,#opt_st{ssa=SSA}} <- OptStates }.

killsets_fun(Blocks) ->
    %% Pre-calculate the live-ins due to Phi-instructions.
    PhiLiveIns = killsets_phi_live_ins(Blocks),
    killsets_blks(Blocks, #{}, #{}, PhiLiveIns).

killsets_blks([{Lbl,Blk}|Blocks], LiveIns0, Kills0, PhiLiveIns) ->
    {LiveIns,Kills} = killsets_blk(Lbl, Blk, LiveIns0, Kills0, PhiLiveIns),
    killsets_blks(Blocks, LiveIns, Kills, PhiLiveIns);
killsets_blks([], _LiveIns0, Kills, _PhiLiveIns) ->
    Kills.

killsets_blk(Lbl, #b_blk{is=Is0,last=L}=Blk, LiveIns0, Kills0, PhiLiveIns) ->
    Successors = beam_ssa:successors(Blk),
    Live1 = killsets_blk_live_outs(Successors, Lbl, LiveIns0, PhiLiveIns),
    Kills1 = Kills0#{{live_outs,Lbl}=>Live1},
    Is = [L|reverse(Is0)],
    {Live,Kills} = killsets_is(Is, Live1, Kills1, Lbl),
    LiveIns = LiveIns0#{Lbl=>Live},
    {LiveIns, Kills}.

killsets_is([#b_set{op=phi,dst=Dst}|Is], Live, Kills, Lbl) ->
    %% The Phi uses are logically located in the predecessors.
    killsets_is(Is, sets:del_element(Dst, Live), Kills, Lbl);
killsets_is([I|Is], Live0, Kills0, Lbl) ->
    Uses = beam_ssa:used(I),
    {Live,LastUses} =
        foldl(fun(Use, {LiveAcc,LastAcc}=Acc) ->
                      case sets:is_element(Use, LiveAcc) of
                          true ->
                              Acc;
                          false ->
                              {sets:add_element(Use, LiveAcc),
                               sets:add_element(Use, LastAcc)}
                      end
              end, {Live0,sets:new([{version,2}])}, Uses),
    case I of
        #b_set{dst=Dst} ->
            killsets_is(Is, sets:del_element(Dst, Live),
                        killsets_add_kills(Dst, LastUses, Kills0), Lbl);
        _ ->
            killsets_is(Is, Live,
                        killsets_add_kills({terminator,Lbl}, LastUses, Kills0),
                        Lbl)
    end;
killsets_is([], Live, Kills, _) ->
    {Live,Kills}.

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
                                         #{} -> sets:new([{version,2}])
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
                           PhiLiveIns, sets:new([{version,2}])).

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

-spec aa([func_id()], kills_map(), st_map(), func_info_db()) ->
          {st_map(), func_info_db()}.

aa(Funs, KillsMap, StMap, FuncDb) ->
    %% Set up the argument info to make all incoming arguments to
    %% exported functions aliased and all non-exported functions
    %% unique.
    ArgsInfo =
        foldl(
          fun(F=#b_local{}, Acc) ->
                  #func_info{exported=E,arg_types=AT} = map_get(F, FuncDb),
                  S = case E of
                          true -> aliased;
                          false -> unique
                      end,
                  Acc#{F=>[S || _ <- AT]}
          end, #{}, Funs),
    AAS = #aas{call_args=ArgsInfo,func_db=FuncDb,kills=KillsMap,
               st_map=StMap, orig_st_map=StMap},
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
aa_fixpoint(Funs, AAS=#aas{alias_map=AliasMap,call_args=CallArgs,
                           func_db=FuncDb}) ->
    Order = aa_breadth_first(Funs, FuncDb),
    aa_fixpoint(Order, Order, AliasMap, CallArgs, AAS, ?MAX_REPETITIONS).

aa_fixpoint([F|Fs], Order, OldAliasMap, OldCallArgs, AAS0=#aas{st_map=StMap},
            Limit) ->
    #b_local{name=#b_literal{val=_N},arity=_A} = F,
    AAS1 = AAS0#aas{caller=F},
    ?DP("-= ~p/~p =-~n", [_N, _A]),
    AAS = aa_fun(F, map_get(F, StMap), AAS1),
    aa_fixpoint(Fs, Order, OldAliasMap, OldCallArgs, AAS, Limit);
aa_fixpoint([], Order, OldAliasMap, OldCallArgs,
            #aas{alias_map=OldAliasMap,call_args=OldCallArgs,
                 func_db=FuncDb}=AAS, _Limit) ->
    ?DP("**** End of iteration ~p ****~n", [_Limit]),
    {StMap,_} = aa_update_annotations(Order, AAS),
    {StMap, FuncDb};
aa_fixpoint([], _, _, _, #aas{func_db=FuncDb,orig_st_map=StMap}, 0) ->
    ?DP("**** End of iteration, too many iterations ****~n"),
    {StMap, FuncDb};
aa_fixpoint([], Order, _OldAliasMap, _OldCallArgs,
            #aas{alias_map=AliasMap,call_args=CallArgs,repeats=Repeats}=AAS,
            Limit) ->
    ?DP("**** Things have changed, starting next iteration ****~n"),
    %% Following the depth first order, select those in Repeats.
    NewOrder = [Id || Id <- Order, sets:is_element(Id, Repeats)],
    aa_fixpoint(NewOrder, Order, AliasMap, CallArgs,
                AAS#aas{repeats=sets:new([{version,2}])}, Limit - 1).

aa_fun(F, #opt_st{ssa=Linear0,args=Args},
       AAS0=#aas{alias_map=AliasMap0,call_args=CallArgs0,
                 func_db=FuncDb,kills=KillsMap,repeats=Repeats0}) ->
    %% Initially assume all formal parameters are unique for a
    %% non-exported function, if we have call argument info in the
    %% AAS, we use it. For an exported function, all arguments are
    %% assumed to be aliased.
    ArgsStatus = aa_get_call_args_status(Args, F, AAS0),
    SS0 = foldl(fun({Var, Status}, Acc) ->
                        aa_new_ssa_var(Var, Status, Acc)
                end, #{}, ArgsStatus),
    ?DP("Args: ~p~n", [ArgsStatus]),
    #{F:=Kills} = KillsMap,
    {SS,#aas{call_args=CallArgs}=AAS} =
        aa_blocks(Linear0, Kills, #{0=>SS0}, AAS0),
    ?DP("SS:~n~p~n~n", [SS]),

    AliasMap = AliasMap0#{ F => SS },
    PrevSS = maps:get(F, AliasMap0, #{}),
    Repeats = case PrevSS =/= SS orelse CallArgs0 =/= CallArgs of
                  true ->
                      %% Alias status has changed, so schedule both
                      %% our callers and callees for renewed analysis.
                      #{ F := #func_info{in=In,out=Out} } = FuncDb,
                      foldl(fun sets:add_element/2,
                            foldl(fun sets:add_element/2, Repeats0, Out), In);
                  false ->
                      Repeats0
              end,
    AAS#aas{alias_map=AliasMap,repeats=Repeats}.

%% Main entry point for the alias analysis
aa_blocks([{?EXCEPTION_BLOCK,_}|Bs], Kills, Lbl2SS, AAS) ->
    %% Nothing happening in the exception block can propagate to the
    %% other block.
    aa_blocks(Bs, Kills, Lbl2SS, AAS);
aa_blocks([{L,#b_blk{is=Is0,last=T}}|Bs0], Kills, Lbl2SS0, AAS0) ->
    #{L:=SS0} = Lbl2SS0,
    {FullSS,AAS1} = aa_is(Is0, SS0, AAS0),
    #{{live_outs,L}:=LiveOut} = Kills,
    {Lbl2SS1,Successors} = aa_terminator(T, FullSS, Lbl2SS0),
    PrunedSS = aa_prune_ss(FullSS, LiveOut),
    Lbl2SS2 = aa_add_block_entry_ss(Successors, PrunedSS, Lbl2SS1),
    Lbl2SS = aa_set_block_exit_ss(L, FullSS, Lbl2SS2),
    aa_blocks(Bs0, Kills, Lbl2SS, AAS1);
aa_blocks([], _Kills, Lbl2SS, AAS) ->
    {Lbl2SS,AAS}.

aa_is([I=#b_set{dst=Dst,op=Op,args=Args,anno=Anno0}|Is], SS0, AAS0) ->
    SS1 = aa_new_ssa_var(Dst, unique, SS0),
    {SS, AAS} =
        case Op of
            %% Instructions changing the alias status.
            {bif,Bif} ->
                {aa_bif(Dst, Bif, Args, SS1, AAS0), AAS0};
            bs_create_bin ->
                case Args of
                    [#b_literal{val=Flag},_,Arg|_] when
                          Flag =:= private_append ; Flag =:= append ->
                        case aa_all_dies([Arg], Dst, AAS0) of
                            true ->
                                %% Inherit the status of the argument
                                {aa_derive_from(Dst, Arg, SS1), AAS0};
                            false ->
                                %% We alias with the surviving arg
                                {aa_set_aliased([Dst|Args], SS1), AAS0}
                        end;
                    _ ->
                        %% TODO: Too conservative?
                        {aa_set_aliased([Dst|Args], SS1), AAS0}
                end;
            bs_extract ->
                {aa_set_aliased([Dst|Args], SS1), AAS0};
            bs_get_tail ->
                {aa_set_aliased([Dst|Args], SS1), AAS0};
            bs_match ->
                {aa_set_aliased([Dst|Args], SS1), AAS0};
            bs_start_match ->
                [_,Bin] = Args,
                {aa_set_aliased([Dst,Bin], SS1), AAS0};
            build_stacktrace ->
                %% build_stacktrace can potentially alias anything
                %% live at this point in the code. We handle it by
                %% aliasing everything known to us. Touching
                %% variables which are dead is harmless.
                {aa_alias_all(SS1), AAS0};
            call ->
                aa_call(Dst, Args, Anno0, SS1, AAS0);
            'catch_end' ->
                [_Tag,Arg] = Args,
                {aa_derive_from(Dst, Arg, SS1), AAS0};
            extract ->
                [Arg,_] = Args,
                {aa_derive_from(Dst, Arg, SS1), AAS0};
            get_hd ->
                [Arg] = Args,
                {aa_pair_extraction(Dst, Arg, hd, SS1), AAS0};
            get_map_element ->
                [Map,_Key] = Args,
                {aa_map_extraction(Dst, Map, SS1, AAS0), AAS0};
            get_tl ->
                [Arg] = Args,
                {aa_pair_extraction(Dst, Arg, tl, SS1), AAS0};
            get_tuple_element ->
                [Arg,Idx] = Args,
                {aa_tuple_extraction(Dst, Arg, Idx, SS1), AAS0};
            landingpad ->
                {aa_set_aliased(Dst, SS1), AAS0};
            make_fun ->
                [Callee|Env] = Args,
                aa_make_fun(Dst, Callee, Env, SS1, AAS0);
            peek_message ->
                {aa_set_aliased(Dst, SS1), AAS0};
            phi ->
                {aa_phi(Dst, Args, SS1), AAS0};
            put_list ->
                {aa_construct_term(Dst, Args, SS1, AAS0), AAS0};
            put_map ->
                {aa_construct_term(Dst, Args, SS1, AAS0), AAS0};
            put_tuple ->
                {aa_construct_term(Dst, Args, SS1, AAS0), AAS0};
            update_tuple ->
                {aa_construct_term(Dst, Args, SS1, AAS0), AAS0};
            update_record ->
                [_Hint,_Size,Src|Updates] = Args,
                Values = [Src|aa_update_record_get_vars(Updates)],
                {aa_construct_term(Dst, Values, SS1, AAS0), AAS0};

            %% Instructions which don't change the alias status
            {float,_} ->
                {SS1, AAS0};
            {succeeded,_} ->
                {SS1, AAS0};
            bs_init_writable ->
                {SS1, AAS0};
            bs_test_tail ->
                {SS1, AAS0};
            executable_line ->
                {SS1, AAS0};
            has_map_field ->
                {SS1, AAS0};
            is_nonempty_list ->
                {SS1, AAS0};
            is_tagged_tuple ->
                {SS1, AAS0};
            kill_try_tag ->
                {SS1, AAS0};
            match_fail ->
                {SS1, AAS0};
            new_try_tag ->
                {SS1, AAS0};
            nif_start ->
                {SS1, AAS0};
            raw_raise ->
                {SS1, AAS0};
            recv_marker_bind ->
                {SS1, AAS0};
            recv_marker_clear ->
                {SS1, AAS0};
            recv_marker_reserve ->
                {SS1, AAS0};
            recv_next ->
                {SS1, AAS0};
            remove_message ->
                {SS1, AAS0};
            resume ->
                {SS1, AAS0};
            wait_timeout ->
                {SS1, AAS0};
            _ ->
                exit({unknown_instruction, I})
        end,
    aa_is(Is, SS, AAS);
aa_is([], SS, AAS) ->
    {SS, AAS}.

aa_terminator(#b_br{succ=S,fail=S}, _SS, Lbl2SS) ->
    {Lbl2SS,[S]};
aa_terminator(#b_br{succ=S,fail=F}, _SS, Lbl2SS) ->
    {Lbl2SS,[S,F]};
aa_terminator(#b_ret{arg=Arg,anno=Anno0}, SS, Lbl2SS0) ->
    Type = maps:get(result_type, Anno0, any),
    Status0 = aa_get_status(Arg, SS),
    ?DP("Returned ~p:~p:~p~n", [Arg, Status0, Type]),
    Type2Status0 = maps:get(returns, Lbl2SS0, #{}),
    Status = case Type2Status0 of
                 #{ Type := OtherStatus } ->
                     aa_meet(Status0, OtherStatus);
                 #{ } ->
                     Status0
             end,
    Type2Status = Type2Status0#{ Type => Status },
    ?DP("New status map: ~p~n", [Type2Status]),
    Lbl2SS = Lbl2SS0#{ returns => Type2Status},
    {Lbl2SS, []};
aa_terminator(#b_switch{fail=F,list=Ls}, _SS, Lbl2SS) ->
    {Lbl2SS,[F|[L || {_,L} <- Ls]]}.

%% Store the updated SS for the point where execution leaves the
%% block.
aa_set_block_exit_ss(ThisBlockLbl, SS, Lbl2SS) ->
    Lbl2SS#{ThisBlockLbl=>SS}.

%% Extend the SS valid on entry to the blocks in the list with NewSS.
aa_add_block_entry_ss([?EXCEPTION_BLOCK|BlockLabels], NewSS, Lbl2SS) ->
    aa_add_block_entry_ss(BlockLabels, NewSS, Lbl2SS);
aa_add_block_entry_ss([L|BlockLabels], NewSS, Lbl2SS) ->
    aa_add_block_entry_ss(BlockLabels, NewSS, aa_merge_ss(L, NewSS, Lbl2SS));
aa_add_block_entry_ss([], _, Lbl2SS) ->
    Lbl2SS.

%% Merge two sharing states when traversing the execution graph
%% reverse post order.
aa_merge_ss(BlockLbl, NewSS, Lbl2SS)
  when is_map_key(BlockLbl, Lbl2SS) ->
    #{BlockLbl:=OrigSS} = Lbl2SS,
    NewSize = maps:size(NewSS),
    OrigSize = maps:size(OrigSS),
    _ = ?aa_assert_ss(OrigSS),
    _ = ?aa_assert_ss(NewSS),

    %% Always merge the smaller state into the larger.
    Tmp = if NewSize < OrigSize ->
                  ?TM_DP("merging block ~p~n~p.~n~p.~n",
                         [BlockLbl, OrigSS, NewSS]),
                  aa_merge_continue(OrigSS, NewSS, maps:keys(NewSS), [], []);
             true ->
                  ?TM_DP("merging block ~p~n~p.~n~p.~n",
                         [BlockLbl, NewSS, OrigSS]),
                  aa_merge_continue(NewSS, OrigSS, maps:keys(OrigSS), [], [])
          end,
    Lbl2SS#{BlockLbl=>Tmp};
aa_merge_ss(BlockLbl, NewSS, Lbl2SS) ->
    Lbl2SS#{BlockLbl=>NewSS}.

aa_merge_continue(A, B, [V|Vars], ParentFixups, AliasFixups) ->
    #{V:=BVas} = B,
    case A of
        #{V:=AVas} ->
            ?TM_DP("merge ~p~n", [V]),
            aa_merge_1(V, AVas, BVas, A, B, Vars, ParentFixups, AliasFixups);
        #{} ->
            ?TM_DP("not in dest ~p~n", [V]),
            %% V isn't in A, nothing to merge, add it.
            aa_merge_continue(A#{V=>BVas}, B, Vars, ParentFixups, AliasFixups)
    end;
aa_merge_continue(A0, _, [], ParentFixups, AliasFixups) ->
    A = aa_merge_parent_fixups(A0, ParentFixups),
    ?aa_assert_ss(aa_merge_alias_fixups(A, AliasFixups)).

aa_merge_1(_V, Vas, Vas, A, B, Vars, ParentFixups, AliasFixups) ->
    %% They are both the same, no change.
    ?TM_DP("same~n"),
    aa_merge_continue(A, B, Vars, ParentFixups, AliasFixups);
aa_merge_1(_V, #vas{status=aliased}, BVas, A, B, Vars,
           ParentFixups, AliasFixups) ->
    %% V is aliased in A, anything related to B becomes aliased.
    ?TM_DP("force aliasB of ~p~n", [aa_related(BVas)]),
    aa_merge_continue(A, B, Vars, ParentFixups,
                      aa_related(BVas)++AliasFixups);
aa_merge_1(V, AVas, #vas{status=aliased}, A, B, Vars,
           ParentFixups, AliasFixups) ->
    %% V is aliased in B, anything related to A becomes aliased.
    ?TM_DP("force aliasA of ~p~n", [aa_related(AVas)]),
    aa_merge_continue(A#{V=>#vas{status=aliased}}, B, Vars,
                      ParentFixups,
                      aa_related(AVas)++AliasFixups);
aa_merge_1(V, #vas{status=S}=AVas, #vas{status=S}=BVas, A, B, Vars,
           ParentFixups, AliasFixups)
  when S == unique ; S == as_parent ->
    aa_merge_child(V, AVas, BVas, A, B, Vars, ParentFixups, AliasFixups).

aa_merge_child(V, #vas{child=Child}=AVas, #vas{child=Child}=BVas,
               A, B, Vars, ParentFixups, AliasFixups) ->
    ?TM_DP("child ~p, same~n", [Child]),
    aa_merge_tuple(V, AVas, BVas, A, B, Vars, ParentFixups, AliasFixups);
aa_merge_child(V, #vas{child=none}=AVas, #vas{child=Child}=BVas,
               A, B, Vars, ParentFixups, AliasFixups) ->
    %% BVas has aquired a derivation from a Phi, no conflict, but the
    %% A side has to be updated with new parent information.
    ?TM_DP("new child in B, ~p~n", [Child]),
    aa_merge_tuple(V, AVas#vas{child=Child}, BVas, A#{V=>BVas},
                   B, Vars, [{Child,V}|ParentFixups], AliasFixups);
aa_merge_child(V, AVas, #vas{child=none}=BVas, A, B, Vars,
               ParentFixups, AliasFixups) ->
    %% AVas has aquired a derivation from a Phi, no conflict, no
    %% update of the state necessary.
    ?TM_DP("no child in B~n"),
    aa_merge_tuple(V, AVas, BVas, A, B, Vars, ParentFixups, AliasFixups);
aa_merge_child(V, AVas, BVas, A, B, Vars, ParentFixups, AliasFixups) ->
    %% Different children, this leads to aliasing.
    ?TM_DP("different children, force alias of ~p~n",
           [aa_related(AVas)++aa_related(BVas)]),
    aa_merge_continue(
      A#{V=>#vas{status=aliased}}, B, Vars,
      ParentFixups,
      aa_related(AVas)++aa_related(BVas)++AliasFixups).

aa_merge_tuple(V, #vas{tuple_elems=Es}=AVas, #vas{tuple_elems=Es}=BVas,
               A, B, Vars, ParentFixups, AliasFixups) ->
    %% The same tuple elements are extracted, no conflict.
    ?TM_DP("same tuple elements~n"),
    aa_merge_pair(V, AVas, BVas, A, B, Vars, ParentFixups, AliasFixups);
aa_merge_tuple(V, #vas{tuple_elems=AEs}=AVas, #vas{tuple_elems=BEs}=BVas,
               A, B, Vars, ParentFixups, AliasFixups) ->
    %% This won't lead to aliasing if all elements are unique.
    case aa_non_aliasing_tuple_elements(AEs++BEs) of
        true ->
            %% No aliasing, the elements are unique
            ?TM_DP("different tuple elements, no aliasing~n"),
            Elements = ordsets:union(AEs, BEs),
            Vas = AVas#vas{tuple_elems=Elements},
            aa_merge_pair(V, Vas, BVas, A#{V=>Vas}, B, Vars,
                          ParentFixups, AliasFixups);
        false ->
            %% Aliasing occurred.
            ?TM_DP("aliasing tuple elements, force ~p~n",
                   aa_related(AVas)++aa_related(BVas)),
            aa_merge_continue(A#{V=>#vas{status=aliased}}, B, Vars,
                              ParentFixups,
                              aa_related(AVas)++aa_related(BVas)++AliasFixups)
    end.

aa_merge_pair(V, #vas{pair_elems=Es}=AVas, #vas{pair_elems=Es}=BVas,
              A, B, Vars, ParentFixups, AliasFixups) ->
    %% The same pair elements are extracted, no conflict.
    ?TM_DP("same pairs~n"),
    aa_merge_extracted(V, AVas, BVas, A, B, Vars, ParentFixups, AliasFixups);
aa_merge_pair(V, #vas{pair_elems=AEs}=AVas, #vas{pair_elems=BEs}=BVas,
              A, B, Vars, ParentFixups, AliasFixups) ->
    R = case {AEs,BEs} of
            {{hd,H},{tl,T}} ->
                {both,H,T};
            {{tl,T},{hd,H}} ->
                {both,H,T};
            {E,none} ->
                E;
            {none,E} ->
                E;
            _ ->
                alias
        end,
    case R of
        alias ->
            ?TM_DP("aliasing pair elements: ~p~n", [R]),
            aa_merge_continue(A#{V=>#vas{status=aliased}}, B, Vars,
                              ParentFixups,
                              aa_related(AVas)++aa_related(BVas)++AliasFixups);
        Pair ->
            ?TM_DP("different pair elements, no aliasing~n"),
            Vas = AVas#vas{pair_elems=Pair},
            aa_merge_extracted(V, Vas, BVas, A#{V=>Vas},
                               B, Vars, ParentFixups, AliasFixups)
    end.

aa_merge_extracted(V, #vas{extracted=AEs}=AVas, #vas{extracted=BEs},
                   A, B, Vars, ParentFixups, AliasFixups) ->
    Extracted = ordsets:union(AEs, BEs),
    aa_merge_continue(A#{V=>AVas#vas{extracted=Extracted}}, B, Vars,
                      ParentFixups, AliasFixups).

aa_related(#vas{parents=Ps,child=Child,extracted=Ex}) ->
    case Child of none ->
            [];
        Child ->
            [Child]
    end ++ Ps ++ Ex.

aa_non_aliasing_tuple_elements(Elems) ->
    aa_non_aliasing_tuple_elements(Elems, #{}).

aa_non_aliasing_tuple_elements([{I,V}|Es], Seen) ->
    case Seen of
        #{I:=X} when X =/= V ->
            false;
        #{} ->
            aa_non_aliasing_tuple_elements(Es, Seen#{I=>V})
    end;
aa_non_aliasing_tuple_elements([], _) ->
    true.

aa_merge_alias_fixups(SS, Fixups) ->
    ?TM_DP("fixup: Forcing aliasing ~p~n", [Fixups]),
    aa_set_status_1(Fixups, none, SS).

aa_merge_parent_fixups(SS0, [{Child,Parent}|Fixups]) ->
    ?TM_DP("fixup: Forcing parents ~p->~p~n", [Child,Parent]),
    #{Child:=#vas{parents=Parents}=Vas} = SS0,
    SS = SS0#{Child=>Vas#vas{parents=ordsets:add_element(Parent, Parents)}},
    aa_merge_parent_fixups(SS, Fixups);
aa_merge_parent_fixups(SS, []) ->
    ?TM_DP("Parent fixups executed~n"),
    SS.

%% Merge two sharing states when traversing the execution graph post
%% order. The only thing the successor merging needs to to is to check
%% if variables in the original SS have become aliased.
aa_merge_ss_successor(BlockLbl, NewSS, Lbl2SS) ->
    #{BlockLbl:=OrigSS} = Lbl2SS,
    Lbl2SS#{BlockLbl=>aa_merge_ss_successor(OrigSS, NewSS)}.

aa_merge_ss_successor(Orig, New) ->
    maps:fold(fun(V, Vas, Acc) ->
                      case New of
                          #{V:=Vas} ->
                              %% Nothing has changed for V.
                              Acc;
                          #{V:=#vas{status=aliased}} ->
                              aa_set_aliased(V, Acc);
                          #{} ->
                              %% V did not exist in New.
                              Acc
                      end
              end, Orig, Orig).

%% Add a new ssa variable to the sharing state and set its status.
aa_new_ssa_var(Var, Status, State) ->
    ?ASSERT(false = maps:get(Var, State, false)),
    State#{Var=>#vas{status=Status}}.

aa_get_status(V=#b_var{}, State) ->
    case State of
        #{V:=#vas{status=as_parent,parents=Ps}} ->
            aa_get_status(Ps, State);
        #{V:=#vas{status=Status}} ->
            Status
    end;
aa_get_status(#b_literal{}, _State) ->
    unique;
aa_get_status([V=#b_var{}], State) ->
    aa_get_status(V, State);
aa_get_status([V=#b_var{}|Parents], State) ->
    aa_meet(aa_get_status(V, State), aa_get_status(Parents, State)).


%% aa_get_status but for instructions extracting values from pairs and
%% tuples.
aa_get_element_extraction_status(V=#b_var{}, State) ->
    case State of
        #{V:=#vas{status=aliased}} ->
            aliased;
        #{V:=#vas{tuple_elems=Elems}} when Elems =/= [] ->
            unique;
        #{V:=#vas{pair_elems=Elems}} when Elems =/= none ->
            unique
    end;
aa_get_element_extraction_status(#b_literal{}, _State) ->
    unique.

aa_set_status(V=#b_var{}, aliased, State) ->
    ?DP("Setting ~p to aliased.~n", [V]),
    case State of
        #{V:=#vas{status=unique,parents=[]}} ->
            %% This is the initial value.
            aa_set_status_1(V, none, State);
        #{V:=#vas{status=aliased}} ->
            %% No change
            State;
        #{V:=#vas{parents=Parents}} ->
            %% V is derived from another value, so the status has to
            %% be propagated to the parent(s).
            aa_set_status(Parents, aliased, State)
    end;
aa_set_status(_V=#b_var{}, unique, State) ->
    ?ASSERT(true = case State of
                       #{_V:=#vas{status=unique}} -> true;
                       #{_V:=#vas{parents=Parents}} ->
                           [unique = aa_get_status(P, State) || P <- Parents],
                           true
                   end),
    State;
aa_set_status(#b_literal{}, _Status, State) ->
    State;
aa_set_status([X|T], Status, State) ->
    aa_set_status(X, Status, aa_set_status(T, Status, State));
aa_set_status([], _, State) ->
    State.

%% Propagate the aliased status to the children.
aa_set_status_1(#b_var{}=V, Parent, State0) ->
    ?DP("aa_set_status_1: ~p, parent:~p~n~p.~n", [V,Parent,State0]),
    #{V:=#vas{child=Child,extracted=Extracted,parents=Parents}} = State0,
    State = State0#{V=>#vas{status=aliased}},
    Work = case Child of
               none ->
                   [];
               _ ->
                   [Child]
           end ++ ordsets:del_element(Parent, Parents) ++ Extracted,
    aa_set_status_1(Work, V, State);
aa_set_status_1([#b_var{}=V|Rest], Parent, State) ->
    aa_set_status_1(Rest, Parent, aa_set_status_1(V, Parent, State));
aa_set_status_1([], _Parent, State) ->
    State.

aa_derive_from(Dst, [Parent|Parents], State0) ->
    aa_derive_from(Dst, Parents, aa_derive_from(Dst, Parent, State0));
aa_derive_from(_Dst, [], State0) ->
    State0;
aa_derive_from(#b_var{}, #b_literal{}, State) ->
    State;
aa_derive_from(#b_var{}=Dst, #b_var{}=Parent, State) ->
    ?DP("Deriving ~p from ~p~n~p.~n", [Dst,Parent,State]),
    case State of
        #{Dst:=#vas{status=aliased}} ->
            %% Nothing to do, already aliased. This can happen when
            %% handling Phis, no propagation to the parent should be
            %% done.
            ?aa_assert_ss(State);
        #{Parent:=#vas{status=aliased}} ->
            %% The parent is aliased, the child will become aliased.
            ?aa_assert_ss(aa_set_aliased(Dst, State));
        #{Parent:=#vas{child=Child}} when Child =/= none ->
            %% There already is a child, this will alias both Dst and Parent.
            ?aa_assert_ss(aa_set_aliased([Dst,Parent], State));
        #{Parent:=#vas{child=none,tuple_elems=Elems}} when Elems =/= [] ->
            %% There already is a child, this will alias both Dst and Parent.
            ?aa_assert_ss(aa_set_aliased([Dst,Parent], State));
        #{Parent:=#vas{child=none,pair_elems=Elems}} when Elems =/= none ->
            %% There already is a child, this will alias both Dst and Parent.
            ?aa_assert_ss(aa_set_aliased([Dst,Parent], State));
        #{Dst:=#vas{parents=Parents}=ChildVas0,
          Parent:=#vas{child=none}=ParentVas0} ->
            %% Inherit the status of the parent.
            ChildVas =
                ChildVas0#vas{parents=ordsets:add_element(Parent, Parents),
                              status=as_parent},
            ParentVas = ParentVas0#vas{child=Dst},
            ?aa_assert_ss(State#{Dst=>ChildVas,Parent=>ParentVas})
    end.

aa_prune_ss(SS, Live) ->
    aa_prune_ss(SS, sets:to_list(Live), Live, #{}).
aa_prune_ss(SS, [V|Wanted], Live, Pruned) ->
    case is_map_key(V, Pruned) of
        false ->
            %% This variable has to be kept, copy it, add it to the
            %% set of live nodes and add the parents to the work list.
            #{V:=#vas{parents=Ps}=Vas} = SS,
            aa_prune_ss(SS, Ps++Wanted,
                        sets:add_element(V, Live),
                        Pruned#{V=>Vas});
        true ->
            %% This variable is alread added.
            aa_prune_ss(SS, Wanted, Live, Pruned)
    end;
aa_prune_ss(_SS, [], Live, Pruned) ->
    %% Now strip all references to variables not in the live set.
    PruneRefs = fun(#vas{parents=Ps0,child=Child0,extracted=Es0,
                         tuple_elems=Ts0,pair_elems=Pes0}=Vas) ->
                        Ps = [P || P <- Ps0, sets:is_element(P, Live)],
                        Child = case sets:is_element(Child0, Live) of
                                    true ->
                                        Child0;
                                    false ->
                                        none
                                end,
                        Es = [E || E <- Es0, sets:is_element(E, Live)],
                        Ts = [E
                              || {_,Var}=E <- Ts0, sets:is_element(Var, Live)],
                        Pes = case Pes0 of
                                  {_,X}=P ->
                                      case sets:is_element(X, Live) of
                                          true ->
                                              P;
                                          _ ->
                                              none
                                      end;
                                  {both,X,Y}=P ->
                                      case {sets:is_element(X, Live),
                                            sets:is_element(Y, Live)} of
                                          {true,true} ->
                                              P;
                                          {true,false} ->
                                              {hd,X};
                                          {false,true} ->
                                              {tl,Y};
                                          _ ->
                                              none
                                      end;
                                  none ->
                                      none
                              end,
                        Vas#vas{parents=Ps,child=Child,extracted=Es,
                                tuple_elems=Ts,pair_elems=Pes}
                end,
    #{V=>PruneRefs(Vas) || V:=Vas <- Pruned}.

aa_update_annotations(Funs, #aas{alias_map=AliasMap0,st_map=StMap0}=AAS) ->
    foldl(fun(F, {StMapAcc,AliasMapAcc}) ->
                  #{F:=Lbl2SS0} = AliasMapAcc,
                  #{F:=OptSt0} = StMapAcc,
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
aa_update_annotation(I=#b_set{args=Args0}, SS, AAS) ->
    Args = [{V,aa_get_status(V, SS)} || #b_var{}=V <- Args0],
    aa_update_annotation1(Args, I, AAS);
aa_update_annotation(I=#b_ret{arg=#b_var{}=V}, SS, AAS) ->
    aa_update_annotation1(aa_get_status(V, SS), I, AAS);
aa_update_annotation(I, _SS, _AAS) ->
    %% For now we don't care about the other terminators.
    I.

aa_update_annotation1(ArgsStatus,
                      I=#b_set{anno=Anno0,args=Args,op=Op}, AAS) ->
    {Aliased,Unique} =
        foldl(fun({#b_var{}=V,aliased}, {As,Us}) ->
                      {ordsets:add_element(V, As), Us};
                 ({#b_var{}=V,unique}, {As,Us}) ->
                      {As, ordsets:add_element(V, Us)};
                 (_, S) ->
                      S
              end, {ordsets:new(),ordsets:new()}, ArgsStatus),
    Anno1 = case Aliased of
                [] -> maps:remove(aliased, Anno0);
                _ -> Anno0#{aliased => Aliased}
            end,
    Anno2 = case Unique of
                [] -> maps:remove(unique, Anno1);
                _ -> Anno1#{unique => Unique}
            end,
    Anno = case {Op,Args} of
               {bs_create_bin,[#b_literal{val=append},_,Var|_]} ->
                   %% Alias analysis indicate the alias status of the
                   %% instruction arguments before the instruction is
                   %% executed. For the private-append optimization we
                   %% need to know if the first fragment dies with
                   %% this instruction or not. Adding an annotation
                   %% here, during alias analysis, is more efficient
                   %% than trying to reconstruct information in the
                   %% kill map during the private-append pass.
                   #aas{caller=Caller,kills=KillsMap} = AAS,
                   #b_set{dst=Dst} = I,
                   KillMap = maps:get(Caller, KillsMap),
                   Dies = sets:is_element(Var, map_get(Dst, KillMap)),
                   Anno2#{first_fragment_dies => Dies};
               _ ->
                   Anno2
           end,
    I#b_set{anno=Anno};
aa_update_annotation1(Status, I=#b_ret{arg=#b_var{}=V,anno=Anno0}, _AAS) ->
    Anno = case Status of
               aliased ->
                   maps:remove(unique, Anno0#{aliased=>[V]});
               unique ->
                   maps:remove(aliased, Anno0#{unique=>[V]})
           end,
    I#b_ret{anno=Anno}.

aa_set_aliased(Args, SS) ->
    aa_set_status(Args, aliased, SS).

aa_alias_all(SS) ->
    aa_set_aliased(maps:keys(SS), SS).

aa_register_extracted(Extracted, Aggregate, State) ->
    ?DP("REGISTER ~p: ~p~n", [Aggregate,Extracted]),
    #{Aggregate:=#vas{extracted=ExVars}=AggVas0,
      Extracted:=#vas{parents=Parents}=ExVas0} = State,
    AggVas = AggVas0#vas{extracted=ordsets:add_element(Extracted, ExVars)},
    ExVas = ExVas0#vas{status=as_parent,
                       parents=ordsets:add_element(Aggregate, Parents)},
    State#{Aggregate=>AggVas, Extracted=>ExVas}.

aa_meet(#b_var{}=Var, OtherStatus, State) ->
    Status = aa_get_status(Var, State),
    aa_set_status(Var, aa_meet(OtherStatus, Status), State);
aa_meet(#b_literal{}, _SetStatus, State) ->
    State;
aa_meet([Var|Vars], [Status|Statuses], State) ->
    aa_meet(Vars, Statuses, aa_meet(Var, Status, State));
aa_meet([], [], State) ->
    State.

aa_meet(StatusA, StatusB) ->
    case {StatusA, StatusB} of
        {_,aliased} -> aliased;
        {aliased, _} -> aliased;
        {unique, unique} -> unique
    end.

aa_meet([H|T]) ->
    aa_meet(H, aa_meet(T));
aa_meet([]) ->
    unique.

%%
%% Type is always less specific or exactly the same as one of the
%% types in StatusByType, so we need to meet all possible statuses for
%% the call site.
%%
aa_get_status_by_type(Type, StatusByType) ->
    Statuses = [Status || Candidate := Status <- StatusByType,
                          beam_types:meet(Type, Candidate) =/= none],
    aa_meet(Statuses).

%% Predicate to check if all variables in `Vars` dies at `Where`.
-spec aa_all_dies([#b_var{}], kill_loc(), #aas{}) -> boolean().
aa_all_dies(Vars, Where, #aas{caller=Caller,kills=Kills}) ->
    KillMap = map_get(Caller, Kills),
    KillSet = map_get(Where, KillMap),
    aa_all_dies(Vars, KillSet).

aa_all_dies([#b_literal{}|Vars], KillSet) ->
    aa_all_dies(Vars, KillSet);
aa_all_dies([#b_var{}=V|Vars], KillSet) ->
    case sets:is_element(V, KillSet) of
        true ->
            aa_all_dies(Vars, KillSet);
        false ->
            false
    end;
aa_all_dies([], _) ->
    true.

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
%% aliased, literals are ignored.
aa_all_vars_unique(Args, SS) ->
    aa_all_vars_unique(Args, #{}, SS).

aa_all_vars_unique([#b_literal{}|Args], Seen,SS) ->
    aa_all_vars_unique(Args, Seen, SS);
aa_all_vars_unique([#b_var{}=V|Args], Seen, SS) ->
    aa_get_status(V, SS) =:= unique andalso
        case Seen of
            #{ V := _ } ->
                false;
            #{} ->
                aa_all_vars_unique(Args, Seen#{V => true }, SS)
        end;
aa_all_vars_unique([], _, _) ->
    true.

aa_construct_term(Dst, Values, SS, AAS) ->
    case aa_all_vars_unique(Values, SS)
        andalso aa_all_dies(Values, Dst, AAS) of
        true ->
            aa_derive_from(Dst, Values, SS);
        false ->
            aa_set_aliased([Dst|Values], SS)
    end.

aa_update_record_get_vars([#b_literal{}, Value|Updates]) ->
    [Value|aa_update_record_get_vars(Updates)];
aa_update_record_get_vars([]) ->
    [].

aa_bif(Dst, element, [#b_literal{val=Idx},Tuple], SS, _AAS)
  when is_integer(Idx), Idx > 0 ->
    aa_tuple_extraction(Dst, Tuple, #b_literal{val=Idx-1}, SS);
aa_bif(Dst, element, [#b_literal{},Tuple], SS, _AAS) ->
    %% This BIF will fail, but in order to avoid any later transforms
    %% making use of uniqueness, conservatively alias.
    aa_set_aliased([Dst,Tuple], SS);
aa_bif(Dst, element, [#b_var{},Tuple], SS, _AAS) ->
    aa_set_aliased([Dst,Tuple], SS);
aa_bif(Dst, hd, [Pair], SS, _AAS) ->
    aa_pair_extraction(Dst, Pair, hd, SS);
aa_bif(Dst, tl, [Pair], SS, _AAS) ->
    aa_pair_extraction(Dst, Pair, tl, SS);
aa_bif(Dst, map_get, [_Key,Map], SS, AAS) ->
    aa_map_extraction(Dst, Map, SS, AAS);
aa_bif(Dst, binary_part, Args, SS, _AAS) ->
    %% bif:binary_part/{2,3} is the only guard bif which could lead to
    %% aliasing, it extracts a sub-binary with a reference to its
    %% argument.
    aa_set_aliased([Dst|Args], SS);
aa_bif(Dst, Bif, Args, SS, _AAS) ->
    Arity = length(Args),
    case erl_internal:guard_bif(Bif, Arity)
        orelse erl_internal:bool_op(Bif, Arity)
        orelse erl_internal:comp_op(Bif, Arity)
        orelse erl_internal:arith_op(Bif, Arity)
        orelse erl_internal:new_type_test(Bif, Arity) of
        true ->
            SS;
        false ->
            %% Assume anything else shares the arguments and returns an
            %% aliased result.
            aa_set_aliased([Dst|Args], SS)
    end.

aa_phi(Dst, Args0, SS) ->
    Args = [V || {V,_} <- Args0],
    aa_derive_from(Dst, Args, SS).

aa_call(Dst, [#b_local{}=Callee|Args], Anno, SS0,
        #aas{alias_map=AliasMap,st_map=StMap}=AAS0) ->
    #b_local{name=#b_literal{val=_N},arity=_A} = Callee,
    ?DP("A Call~n  callee: ~p/~p~n  args: ~p~n", [_N, _A, Args]),
    case AliasMap of
        #{Callee:=#{0:=CalleeSS}=Lbl2SS} ->
            ?DP("  The callee is known~n"),
            #opt_st{args=CalleeArgs} = map_get(Callee, StMap),
            ?DP("  callee args: ~p~n", [CalleeArgs]),
            ?DP("  caller args: ~p~n", [Args]),
            ?DP("  args in caller: ~p~n",
                [[{Arg, aa_get_status(Arg, SS0)} || Arg <- Args]]),
            ArgStates = [ aa_get_status(Arg, CalleeSS) || Arg <- CalleeArgs],
            ?DP("  callee arg states: ~p~n", [ArgStates]),
            AAS = aa_add_call_info(Callee, Args, SS0, AAS0),
            SS = aa_meet(Args, ArgStates, SS0),
            ?DP("  meet: ~p~n",
                [[{Arg, aa_get_status(Arg, SS)} || Arg <- Args]]),
            ?DP("  callee-ss ~p~n", [CalleeSS]),
            ReturnStatusByType = maps:get(returns, Lbl2SS, #{}),
            ?DP("  status by type: ~p~n", [ReturnStatusByType]),
            ReturnedType = case Anno of
                               #{ result_type := ResultType } ->
                                   ResultType;
                               #{} ->
                                   any
                           end,
            %% ReturnedType is always less specific or exactly the
            %% same as one of the types in ReturnStatusByType.
            ?DP("  returned type: ~s~n",
                [beam_ssa_pp:format_type(ReturnedType)]),
            ResultStatus = aa_get_status_by_type(ReturnedType,
                                                 ReturnStatusByType),
            ?DP("  result status: ~p~n", [ResultStatus]),
            {aa_set_status(Dst, ResultStatus, SS), AAS};
        #{} ->
            %% We don't know anything about the function, don't change
            %% the status of any variables
            {SS0, AAS0}
    end;
aa_call(_Dst, [#b_remote{mod=#b_literal{val=erlang},
                         name=#b_literal{val=exit},
                         arity=1}|_], _Anno, SS, AAS) ->
    %% The function will never return, so nothing that happens after
    %% this can influence the aliasing status.
    {SS, AAS};
aa_call(Dst, [_Callee|Args], _Anno, SS, AAS) ->
    %% This is either a call to a fun or to an external function,
    %% assume that all arguments and the result escape.
    {aa_set_aliased([Dst|Args], SS), AAS}.

%% Incorporate aliasing information for the arguments to a call when
%% analysing the body of a function into the global state.
aa_add_call_info(Callee, Args, SS0, #aas{call_args=Info0}=AAS) ->
    ArgStats = [aa_get_status(Arg, SS0) || Arg <- Args],
    #{Callee := Stats} = Info0,
    NewStats = [aa_meet(A, B) || {A,B} <- zip(Stats, ArgStats)],
    Info = Info0#{Callee => NewStats},
    AAS#aas{call_args=Info}.

aa_get_call_args_status(Args, Callee, #aas{call_args=Info}) ->
    #{ Callee := Status } = Info,
    zip(Args, Status).

%% Pair extraction.
aa_pair_extraction(Dst, #b_var{}=Pair, Element, SS) ->
    case SS of
        #{Pair:=#vas{status=aliased}} ->
            %% The pair is aliased, so what is extracted will be aliased.
            aa_set_aliased(Dst, SS);
        #{Pair:=#vas{pair_elems={both,_,_}}} ->
            %% Both elements have already been extracted.
            aa_set_aliased([Dst,Pair], SS);
        #{Pair:=#vas{pair_elems=none}=Vas} ->
            %% Nothing has been extracted from this pair yet.
            aa_register_extracted(
              Dst, Pair,
              SS#{Pair=>Vas#vas{pair_elems={Element,Dst}}});
        #{Pair:=#vas{pair_elems={Element,_}}} ->
            %% This element has already been extracted.
            aa_set_aliased([Dst,Pair], SS);
        #{Pair:=#vas{pair_elems={tl,T}}=Vas} when Element =:= hd ->
            %% Both elements have now been extracted, but no aliasing.
            aa_register_extracted(Dst, Pair,
                                  SS#{Pair=>Vas#vas{pair_elems={both,Dst,T}}});
        #{Pair:=#vas{pair_elems={hd,H}}=Vas} when Element =:= tl ->
            %% Both elements have now been extracted, but no aliasing.
            aa_register_extracted(Dst, Pair,
                                  SS#{Pair=>Vas#vas{pair_elems={both,H,Dst}}})
    end;
aa_pair_extraction(_Dst, #b_literal{}, _Element, SS) ->
    SS.

aa_map_extraction(Dst, Map, SS, AAS) ->
    aa_derive_from(
      Dst, Map,
      aa_alias_inherit_and_alias_if_arg_does_not_die(Dst, Map, SS, AAS)).

%% Extracting elements from a tuple.
aa_tuple_extraction(Dst, #b_var{}=Tuple, #b_literal{val=I}, SS) ->
    case SS of
        #{Tuple:=#vas{status=aliased}} ->
            %% The tuple is aliased, so what is extracted will be
            %% aliased.
            aa_set_aliased(Dst, SS);
        #{Tuple:=#vas{child=Child}} when Child =/= none ->
            %% Something has already been derived from the tuple.
            aa_set_aliased([Dst,Tuple], SS);
        #{Tuple:=#vas{tuple_elems=[]}=TupleVas} ->
            %% Nothing has been extracted from this tuple yet.
            aa_register_extracted(
              Dst, Tuple, SS#{Tuple=>TupleVas#vas{tuple_elems=[{I,Dst}]}});
        #{Tuple:=#vas{tuple_elems=Elems0}=TupleVas} ->
            case [ Idx || {Idx,_} <- Elems0, I =:= Idx] of
                [] ->
                    %% This element has not been extracted.
                    Elems = ordsets:add_element({I,Dst}, Elems0),
                    aa_register_extracted(
                      Dst, Tuple, SS#{Tuple=>TupleVas#vas{tuple_elems=Elems}});
                _ ->
                    %% This element is already extracted -> aliasing
                    aa_set_aliased([Dst,Tuple], SS)
            end
    end;
aa_tuple_extraction(_, #b_literal{}, _, SS) ->
    SS.

aa_make_fun(Dst, Callee=#b_local{name=#b_literal{}},
            Env0, SS0,
            AAS0=#aas{call_args=Info0,repeats=Repeats0}) ->
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
    Repeats = case PrevStatus =/= Status of
                  true ->
                      %% We have new information for the callee, we
                      %% have to revisit it.
                      sets:add_element(Callee, Repeats0);
                  false ->
                      Repeats0
              end,
    AAS = AAS0#aas{call_args=Info,repeats=Repeats},
    {SS, AAS}.

aa_breadth_first(Funs, FuncDb) ->
    IsExported = fun (F) ->
                         #{ F := #func_info{exported=E} } = FuncDb,
                         E
                 end,
    Exported = [ F || F <- Funs, IsExported(F)],
    aa_breadth_first(Exported, [], sets:new([{version,2}]), FuncDb).

aa_breadth_first([F|Work], Next, Seen, FuncDb) ->
    case sets:is_element(F, Seen) of
        true ->
            aa_breadth_first(Work, Next, Seen, FuncDb);
        false ->
            case FuncDb of
                #{ F := #func_info{out=Children} } ->
                    [F|aa_breadth_first(Work, Children ++ Next,
                                        sets:add_element(F, Seen), FuncDb)];
                #{} ->
                    %% Other optimization steps can have determined
                    %% that the function is not called and removed it
                    %% from the funcdb, but it still remains in the
                    %% #func_info{} of the (at the syntax-level)
                    %% caller.
                    aa_breadth_first(Work, Next, Seen, FuncDb)
            end
    end;
aa_breadth_first([], [], _Seen, _FuncDb) ->
    [];
aa_breadth_first([], Next, Seen, FuncDb) ->
    aa_breadth_first(Next, [], Seen, FuncDb).

-ifdef(EXTRA_ASSERTS).

-spec aa_assert_ss(sharing_state()) -> sharing_state().

aa_assert_ss(SS) ->
    try
        maps:foreach(
          fun(_V, #vas{status=aliased}=Vas) ->
                  %% An aliased variable should not have extra info.
                  [] = Vas#vas.parents,
                  none = Vas#vas.child,
                  [] = Vas#vas.extracted,
                  [] = Vas#vas.tuple_elems,
                  none = Vas#vas.pair_elems,
                  ok;
             (V, #vas{status=unique,child=Child,extracted=Es,
                      tuple_elems=Ts,pair_elems=Pair}=Vas) ->
                  [] = Vas#vas.parents,
                  aa_assert_extracted(Es, Ts, Pair, V),
                  aa_assert_parent_of(V, Child, SS),
                  aa_assert_parent_of(V, Es, SS),
                  aa_assert_pair(Pair, V, SS),
                  aa_assert_tuple_elems(Ts, V, SS);
             (V, #vas{status=as_parent,parents=Ps,child=Child,extracted=Es,
                      tuple_elems=Ts,pair_elems=Pair}) ->
                  aa_assert_not_aliased(
                    Ps, SS,
                    io_lib:format("as parent of ~p should not be aliased.",
                                  [V])),
                  aa_assert_extracted(Es, Ts, Pair, V),
                  aa_assert_parent_of(Ps, V, SS),
                  aa_assert_parent_of(V, Child, SS),
                  aa_assert_parent_of(V, Es, SS),
                  aa_assert_pair(Pair, V, SS),
                  aa_assert_tuple_elems(Ts, V, SS)
          end, SS)
    of
        _ -> SS
    catch {assertion_failure, V, Desc} ->
            io:format("Malformed SS~n~p~n~p ~s~n", [SS, V, Desc]),
            exit(assertion_failure)
    end.

%% Check that V is a parent of Child
aa_assert_parent_of(_V, none, _SS) ->
    ok;
aa_assert_parent_of(#b_var{}=V, #b_var{}=Child, SS) ->
    case SS of
        #{Child:=#vas{status=as_parent,parents=Ps}} ->
            case ordsets:is_element(V, Ps) of
                true ->
                    ok;
                false ->
                    throw({assertion_failure, V,
                           io_lib:format(
                             "child ~p does not have ~p as parent",
                             [Child, V])})
            end;
        #{} ->
            throw({assertion_failure, V,
                   io_lib:format(
                     "child ~p does not have status as_parent", [Child])})
    end;
aa_assert_parent_of(#b_var{}=V, [P|Ps], SS) ->
    aa_assert_parent_of(V, P, SS),
    aa_assert_parent_of(V, Ps, SS);
aa_assert_parent_of([V|Vs], Child, SS) ->
    aa_assert_parent_of(V, Child, SS),
    aa_assert_parent_of(Vs, Child, SS);
aa_assert_parent_of(_, [], _) ->
    true;
aa_assert_parent_of([], _, _) ->
    true.

aa_assert_pair(none, _V, _SS) ->
    ok;
aa_assert_pair({Elem,X}, V, SS) when Elem =:= hd; Elem =:= tl ->
    case SS of
        #{X:=#vas{status=as_parent}} ->
            aa_assert_parent_of(V, X, SS);
        #{} ->
            throw({assertion_failure, V,
                   io_lib:format("extracted pair and ~p does not"
                                 " have status as_parent", [X])})
    end;
aa_assert_pair({both,X,Y}, V, SS) ->
    case SS of
        #{X:=#vas{status=as_parent},
          Y:=#vas{status=as_parent}} ->
            aa_assert_parent_of(V, X, SS),
            aa_assert_parent_of(V, Y, SS);
        #{} ->
            throw({assertion_failure, V,
                   io_lib:format("extracted pairs ~p and ~p do not"
                                 " have status as_parent", [X, Y])})
    end.

aa_assert_tuple_elems([{_,X}|Ts], V, SS) ->
    case SS of
        #{X:=#vas{status=as_parent}} ->
            aa_assert_parent_of(V, X, SS),
            aa_assert_tuple_elems(Ts, V, SS);
        #{} ->
            throw({assertion_failure, V,
                   io_lib:format(
                     "child ~p does not have status as_parent", [X])})
    end;
aa_assert_tuple_elems([], _, _) ->
    ok.

aa_assert_extracted(Es, Ts, Pair, Var) ->
    Actual = ordsets:union(ordsets:from_list([V || {_,V} <- Ts]),
                           ordsets:from_list(case Pair of
                                                 none -> [];
                                                 {_, X} -> [X];
                                                 {both,X,Y} -> [X,Y]
                                             end)),
    case Es of
        Actual ->
            true;
        _ ->
            throw({assertion_failure, Var,
                   "has inconsistent extracted set"})
    end.

aa_assert_not_aliased([V|Vs], SS, Desc) ->
    #{V:=#vas{status=S}} = SS,

    case S of
        unique -> ok;
        as_parent -> ok;
        _ ->
            throw({assertion_failure, V, Desc})
    end,
    aa_assert_not_aliased(Vs, SS, Desc);
aa_assert_not_aliased([], _SS, _) ->
    true.
-endif.
