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

-export([opt/2]).

-import(lists, [foldl/3, reverse/1, zip/2]).

%% The maximum number of iterations when calculating alias
%% information.
-define(MAX_REPETITIONS, 16).

-include("beam_ssa_opt.hrl").
-include("beam_types.hrl").

%% -define(DEBUG, true).

-ifdef(DEBUG).
-define(DP(FMT, ARGS), io:format(FMT, ARGS)).
-define(DP(FMT), io:format(FMT)).
-else.
-define(DP(FMT, ARGS), skip).
-define(DP(FMT), skip).
-endif.

-type call_args_status_map() :: #{ #b_local{} => ['aliased' | 'unique'] }.

%% Alias analysis state
-record(aas, {
              caller :: func_id() | 'undefined',
              call_args = #{} :: call_args_status_map(),
              alias_map = #{},
              func_db :: func_info_db(),
              kills :: kills_map(),
              st_map :: st_map(),
              orig_st_map :: st_map()
             }).

%% A code location refering to either the #b_set{} defining a variable
%% or the terminator of a block.
-type kill_loc() :: #b_var{} | {terminator, beam_ssa:label()}.

%% Map a code location to the set of variables which die at that
%% location.
-type kill_set() :: #{ kill_loc() => sets:set(#b_var{}) }.

-type kills_map() :: #{ func_id() => kill_set() }.

%% Record holding the liveness information for a code location.
-record(liveness_st, {
                      in = sets:new([{version,2}]) :: sets:set(#b_var{}),
                      out = sets:new([{version,2}]) :: sets:set(#b_var{})
                     }).

%%%
%%% Optimization pass which calculates the alias status of values and
%%% uses the results to transform the code.
%%%
-spec opt(st_map(), func_info_db()) -> {st_map(), func_info_db()}.

opt(StMap0, FuncDb0) ->
    %% Ignore functions which are not in the function db (never
    %% called) or are stubs for nifs.
    Funs = [ F || F <- maps:keys(StMap0),
                  is_map_key(F, FuncDb0), not is_nif(F, StMap0)],
    Liveness = liveness(Funs, StMap0),
    KillsMap = killsets(Liveness, StMap0),

    aa(Funs, KillsMap, StMap0, FuncDb0).

%%%
%%% Calculate liveness for each function using the standard iterative
%%% fixpoint method.
%%%

-spec liveness([func_id()], st_map()) ->
          [{func_id(), #{func_id() => {beam_ssa:label(), #liveness_st{}}}}].

liveness([F|Funs], StMap) ->
    Liveness = liveness_fun(F, StMap),
    [{F,Liveness}|liveness(Funs, StMap)];
liveness([], _StMap) ->
    [].

liveness_fun(F, StMap0) ->
    #opt_st{ssa=SSA} = map_get(F, StMap0),
    State0 = maps:from_list([ {Lbl, #liveness_st{}} || {Lbl,_} <- SSA]),
    liveness_blks_fixp(reverse(SSA), State0, false).

liveness_blks_fixp(_SSA, State0, State0) ->
    State0;
liveness_blks_fixp(SSA, State0, _Old) ->
    State = liveness_blks(SSA, State0),
    liveness_blks_fixp(SSA, State, State0).

liveness_blks([{Lbl,Blk}|Blocks], State0) ->
    OutOld = get_live_out(Lbl, State0),
    Defs = blk_defs(Blk),
    Uses = blk_effective_uses(Blk),
    In = sets:union(Uses, sets:subtract(OutOld, Defs)),
    Out = successor_live_ins(Blk, State0),
    liveness_blks(Blocks, set_block_liveness(Lbl, In, Out, State0));
liveness_blks([], State0) ->
    State0.

get_live_in(Lbl, State) ->
    #liveness_st{in=In} = map_get(Lbl, State),
    In.

get_live_out(Lbl, State) ->
    #liveness_st{out=Out} = map_get(Lbl, State),
    Out.

set_block_liveness(Lbl, In, Out, State) ->
    L = map_get(Lbl, State),
    State#{Lbl => L#liveness_st{in=In,out=Out}}.

successor_live_ins(Blk, State) ->
    foldl(fun(Lbl, Acc) ->
                  sets:union(Acc, get_live_in(Lbl, State))
          end, sets:new([{version,2}]), beam_ssa:successors(Blk)).

blk_defs(#b_blk{is=Is}) ->
    foldl(fun(#b_set{dst=Dst}, Acc) ->
                  sets:add_element(Dst, Acc)
          end, sets:new([{version,2}]), Is).

blk_effective_uses(#b_blk{is=Is,last=Last}) ->
    %% We can't use beam_ssa:used/1 on the whole block as it considers
    %% a use after a def a use and that will derail the liveness
    %% calculation.
    blk_effective_uses([Last|reverse(Is)], sets:new([{version,2}])).

blk_effective_uses([I|Is], Uses0) ->
    Uses = case I of
               #b_set{dst=Dst} ->
                   %% The uses after the def do not count
                   sets:del_element(Dst, Uses0);
               _ -> % A terminator, no defs
                   Uses0
           end,
    LocalUses = sets:from_list(beam_ssa:used(I), [{version,2}]),
    blk_effective_uses(Is, sets:union(Uses, LocalUses));
blk_effective_uses([], Uses) ->
    Uses.

%%%
%%% Predicate to check if a function is the stub for a nif.
%%%
-spec is_nif(func_id(), st_map()) -> boolean().

is_nif(F, StMap) ->
    #opt_st{ssa=[{0,#b_blk{is=Is}}|_]} = map_get(F, StMap),
    case Is of
        [#b_set{op=nif_start}|_] ->
            true;
        _ -> false
    end.

%%%
%%% Calculate the killset for all functions in the liveness
%%% information.
%%%
-spec killsets([{func_id(),
                 #{func_id() => {beam_ssa:label(), #liveness_st{}}}}],
               st_map()) -> kills_map().

killsets(Liveness, StMap) ->
    maps:from_list([{F, kills_fun(F, StMap, Live)} || {F, Live} <- Liveness]).

%%%
%%% Calculate the killset for a function. The killset allows us to
%%% look up the variables that die at a code location.
%%%
kills_fun(Fun, StMap, Liveness) ->
    #opt_st{ssa=SSA} = map_get(Fun, StMap),
    kills_fun1(SSA, #{}, Liveness).

kills_fun1([{Lbl,Blk}|Blocks], KillsMap0, Liveness) ->
    KillsMap = kills_block(Lbl, Blk, map_get(Lbl, Liveness), KillsMap0),
    kills_fun1(Blocks, KillsMap, Liveness);
kills_fun1([], KillsMap, _) ->
    KillsMap.

kills_block(Lbl, #b_blk{is=Is,last=Last}, #liveness_st{out=Out}, KillsMap0) ->
    kills_is([Last|reverse(Is)], Out, KillsMap0, Lbl).

kills_is([I|Is], Live0, KillsMap0, Blk) ->
    {Live, Key} = case I of
                      #b_set{dst=Dst} ->
                          {sets:del_element(Dst, Live0), Dst};
                      _ ->
                          {Live0, {terminator, Blk}}
                  end,
    Uses = sets:from_list(beam_ssa:used(I), [{version,2}]),
    RemainingUses = sets:union(Live0, Uses),
    Killed = sets:subtract(RemainingUses, Live0),
    KillsMap = KillsMap0#{Key => Killed},
    kills_is(Is, sets:union(Live, Killed), KillsMap, Blk);
kills_is([], _, KillsMap, _) ->
    KillsMap.

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
%%% The alias analysis is performed by traversing the functions in the
%%% module and their code. For each operation the uniqueness and alias
%%% status are updated. The unique/aliased status is maintained in a
%%% map which maps a variable to a either a status or another
%%% variable. Thus constructing equivalent sets in the same way a
%%% Kotzmann and Mössenböck.
%%%
%%% When the analysis finishes each instruction is annotated with
%%% information about which of its arguments are unique or aliased.
%%%
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
aa_fixpoint(Order, AAS) ->
    ?DP("**** Starting fixpoint iteration ****~n"),
    aa_fixpoint(Order, Order, AAS#aas.alias_map, AAS, ?MAX_REPETITIONS).

aa_fixpoint([F|Fs], Order, OldAliasMap, AAS0=#aas{st_map=StMap}, Limit) ->
    #b_local{name=#b_literal{val=_N},arity=_A} = F,
    AAS1 = AAS0#aas{caller=F},
    ?DP("-= ~p/~p =-~n", [_N, _A]),
    {OptSt,AAS2} = aa_fun(F, map_get(F, StMap), AAS1),
    AAS = AAS2#aas{st_map=StMap#{F => OptSt}},
    aa_fixpoint(Fs, Order, OldAliasMap, AAS, Limit);
aa_fixpoint([], _Order, OldAliasMap,
            #aas{alias_map=OldAliasMap,func_db=FuncDb,st_map=StMap}, _) ->
    ?DP("**** End of iteration ****~n"),
    {StMap, FuncDb};
aa_fixpoint([], _, _, #aas{func_db=FuncDb,orig_st_map=StMap}, 0) ->
    ?DP("**** End of iteration, too many iterations ****~n"),
    {StMap, FuncDb};
aa_fixpoint([], Order, _OldAliasMap, AAS=#aas{alias_map=AliasMap}, Limit) ->
    ?DP("**** Things have changed, starting next iteration ****~n"),
    aa_fixpoint(Order, Order, AliasMap, AAS, Limit - 1).

aa_fun(F, #opt_st{ssa=Linear0,anno=_Anno,args=Args}=St,
       AAS0=#aas{alias_map=AliasMap0}) ->
    %% Initially assume all formal parameters are unique for a
    %% non-exported function, if we have call argument info in the
    %% AAS, we use it. For an exported function, all arguments are
    %% assumed to be aliased.
    ArgsStatus = aa_get_call_args_status(Args, F, AAS0),
    SS0 = foldl(fun({Var, Status}, Acc) ->
                        aa_new_ssa_var(Var, Status, Acc)
                end, #{}, ArgsStatus),
    ?DP("@@ Args: ~p~n", [ArgsStatus]),
    {Linear1,SS} = aa_blocks(Linear0, SS0, AAS0),
    ?DP("SS:~n~s~n~n", [aa_format(SS)]),
    AAS = aa_merge_call_args_status(SS, AAS0),

    AliasMap = AliasMap0#{ F => SS },
    {St#opt_st{ssa=Linear1}, AAS#aas{alias_map=AliasMap}}.

%% Main entry point for the alias analysis
aa_blocks([{L,#b_blk{is=Is0,last=T0}=Blk}|Bs0], SS0, AAS) ->
    {Is,SS1} = aa_is(Is0, SS0, [], AAS),
    {T,SS2} = aa_terminator(T0, SS1),
    {Bs,SS} = aa_blocks(Bs0, SS2, AAS),
    {[{L,Blk#b_blk{is=Is,last=T}}|Bs],SS};
aa_blocks([], SS, _) ->
    {[],SS}.

aa_is([I=#b_set{dst=Dst,op=Op,args=Args,anno=Anno0}|Is], SS0, Acc, AAS) ->
    SS1 = aa_new_ssa_var(Dst, unique, SS0),
    SS = case Op of
             %% Instructions changing the alias status.
             {bif,Bif} ->
                 aa_bif(Dst, Bif, Args, SS1, AAS);
             bs_create_bin ->
                 case Args of
                     [#b_literal{val=Flag},_,Arg|_] when
                           Flag =:= private_append ; Flag =:= append ->
                         case aa_all_dies([Arg], Dst, AAS) of
                             true ->
                                 %% Inherit the status of the argument
                                 aa_join(Dst, Arg, SS1);
                             false ->
                                 %% We alias with the surviving arg
                                 aa_set_aliased([Dst|Args], SS1)
                         end;
                     _ ->
                         %% TODO: Too conservative?
                         aa_set_aliased([Dst|Args], SS1)
                 end;
             bs_extract ->
                 aa_set_aliased([Dst|Args], SS1);
             bs_get_tail ->
                 aa_set_aliased([Dst|Args], SS1);
             bs_match ->
                 aa_set_aliased([Dst|Args], SS1);
             bs_start_match ->
                 [_,Bin] = Args,
                 aa_set_aliased([Dst,Bin], SS1);
             build_stacktrace ->
                 %% build_stacktrace can potentially alias anything
                 %% live at this point in the code. We handle it by
                 %% aliasing everything known to us. Touching
                 %% variables which are dead is harmless.
                 aa_alias_all(SS1);
             call ->
                 aa_call(Dst, Args, Anno0, SS1, AAS);
             'catch_end' ->
                 [_Tag,Arg] = Args,
                 aa_join(Dst, Arg, SS1);
             extract ->
                 [Arg,_] = Args,
                 aa_join(Dst, Arg, SS1);
             get_hd ->
                 [Arg] = Args,
                 aa_pair_extraction(Dst, Arg, hd, SS1);
             get_map_element ->
                 [Map,_Key] = Args,
                 aa_join(Dst, Map, SS1);
             get_tl ->
                 [Arg] = Args,
                 aa_pair_extraction(Dst, Arg, tl, SS1);
             get_tuple_element ->
                 [Arg,Idx] = Args,
                 aa_tuple_extraction(Dst, Arg, Idx, SS1);
             landingpad ->
                 aa_set_aliased(Dst, SS1);
             make_fun ->
                 [_|Env] = Args,
                 aa_join_ls(Dst, Env, SS1);
             old_make_fun ->
                 [_|Env] = Args,
                 aa_join_ls(Dst, Env, SS1);
             peek_message ->
                 aa_set_aliased(Dst, SS1);
             phi ->
                 aa_phi(Dst, Args, SS1);
             put_list ->
                 aa_construct_term(Dst, Args, SS1, AAS);
             put_map ->
                 aa_construct_term(Dst, Args, SS1, AAS);
             put_tuple ->
                 aa_construct_term(Dst, Args, SS1, AAS);
             update_tuple ->
                 aa_construct_term(Dst, Args, SS1, AAS);
             update_record ->
                 [_Hint,_Size,Src|Updates] = Args,
                 Values = [Src|aa_update_record_get_vars(Updates)],
                 aa_construct_term(Dst, Values, SS1, AAS);

             %% Instructions which don't change the alias status
             {float,_} ->
                 SS1;
             {succeeded,_} ->
                 SS1;
             bs_init_writable ->
                 SS1;
             bs_test_tail ->
                 SS1;
             has_map_field ->
                 SS1;
             is_nonempty_list ->
                 SS1;
             is_tagged_tuple ->
                 SS1;
             kill_try_tag ->
                 SS1;
             match_fail ->
                 SS1;
             new_try_tag ->
                 SS1;
             nif_start ->
                 SS1;
             raw_raise ->
                 SS1;
             recv_marker_bind ->
                 SS1;
             recv_marker_clear ->
                 SS1;
             recv_marker_reserve ->
                 SS1;
             recv_next ->
                 SS1;
             remove_message ->
                 SS1;
             resume ->
                 SS1;
             wait_timeout ->
                 SS1;
             _ ->
                 exit({unknown_instruction, I})
         end,
    aa_is(Is, SS, [aa_update_annotation(I, SS1)|Acc],
          AAS);
aa_is([], SS, Acc, _AAS) ->
    {reverse(Acc), SS}.

aa_terminator(T=#b_br{anno=Anno0}, SS0) ->
    Anno = aa_update_annotation(Anno0, SS0),
    {T#b_br{anno=Anno}, SS0};
aa_terminator(T=#b_ret{arg=Arg,anno=Anno0}, SS0) ->
    Type = maps:get(result_type, Anno0, any),
    Status0 = aa_get_status(Arg, SS0),
    ?DP("Returned ~p:~p:~p~n", [Arg, Status0, Type]),
    Type2Status0 = maps:get(returns, SS0, #{}),
    Status = case Type2Status0 of
                 #{ Type := OtherStatus } ->
                     aa_meet(Status0, OtherStatus);
                 #{ } ->
                     Status0
             end,
    Type2Status = Type2Status0#{ Type => Status },
    ?DP("new status map: ~p~n", [Type2Status]),
    SS = SS0#{ returns => Type2Status},
    {aa_update_annotation(T, SS), SS};
aa_terminator(T=#b_switch{anno=Anno0}, SS0) ->
    Anno = aa_update_annotation(Anno0, SS0),
    {T#b_switch{anno=Anno}, SS0}.

%% Add a new ssa variable to the alias state and set its status.
aa_new_ssa_var(Var, Status, State) ->
    false = maps:get(Var, State, false), % Assertion
    State#{Var => {status, Status}}.

aa_get_representative(Var, State) ->
    %% TODO: Consider path compression
    case State of
        #{ Var := {status, _} } ->
            Var;
        #{ Var := Parent } ->
            aa_get_representative(Parent, State)
    end.

aa_get_status(V=#b_var{}, State) ->
    Repr = aa_get_representative(V, State),
    #{ Repr := {status, S} } = State,
    S;
aa_get_status(#b_literal{}, _State) ->
    unique.

aa_set_status(V=#b_var{}, Status, State) ->
    Repr = aa_get_representative(V, State),
    State#{ Repr => {status, Status} };
aa_set_status(#b_literal{}, _Status, State) ->
    State;
aa_set_status([X|T], Status, State) ->
    aa_set_status(X, Status, aa_set_status(T, Status, State));
aa_set_status([], _, State) ->
    State.

aa_update_annotation(I=#b_set{anno=Anno0,args=Args}, SS) ->
    {Aliased,Unique} =
        foldl(fun(#b_var{}=V, {As,Us}) ->
                      case aa_get_status(V, SS) of
                          aliased ->
                              {ordsets:add_element(V, As), Us};
                          unique ->
                              {As, ordsets:add_element(V, Us)}
                      end;
                 (_, A) ->
                      A
              end, {ordsets:new(),ordsets:new()}, Args),
    Anno1 = case Aliased of
                [] -> maps:remove(aliased, Anno0);
                _ -> Anno0#{aliased => Aliased}
            end,
    Anno = case Unique of
               [] -> maps:remove(unique, Anno1);
               _ -> Anno1#{unique => Unique}
           end,
    I#b_set{anno=Anno};
aa_update_annotation(I=#b_ret{arg=#b_var{}=V,anno=Anno0}, SS) ->
    Anno = case aa_get_status(V, SS) of
               aliased ->
                   maps:remove(unique, Anno0#{aliased=>[V]});
               unique ->
                   maps:remove(aliased, Anno0#{unique=>[V]})
           end,
    I#b_ret{anno=Anno};
aa_update_annotation(I, _SS) ->
    %% For now we don't care about the other terminators.
    I.

aa_set_aliased(Args, SS) ->
    aa_set_status(Args, aliased, SS).

aa_alias_all(SS0) ->
    maps:map(fun(#b_var{}, _) ->
                     {status,aliased};
                (returns, Types) ->
                     #{ T => aliased || T := _ <- Types};
                (_, V) ->
                     V
             end, SS0).

aa_join_ls(VarA, [#b_var{}=VarB|Vars], State) ->
    aa_join_ls(VarB, Vars, aa_join(VarA, VarB, State));
aa_join_ls(VarA, [_|Vars], State) ->
    aa_join_ls(VarA, Vars, State);
aa_join_ls(_, [], State) ->
    State.

aa_join(#b_var{}=VarA, #b_var{}=VarB, State) ->
    ARepr = aa_get_representative(VarA, State),
    BRepr = aa_get_representative(VarB, State),
    case {ARepr, BRepr} of
        {Repr, Repr} ->
            State;
        _ ->
            {status, A} = map_get(ARepr, State),
            {status, B} = map_get(BRepr, State),
            State#{ ARepr => {status, aa_meet(A, B)}, BRepr => ARepr }
    end;
aa_join(_, _, State) ->
    State.

aa_meet(#b_var{}=Var, SetStatus, State) ->
    Repr = aa_get_representative(Var, State),
    {status, Status} = map_get(Repr, State),
    State#{ Repr => {status, aa_meet(SetStatus, Status)} };
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

%% Check that a variable in Args only occurs once, literals are
%% ignored.
aa_all_vars_unique(Args) ->
    aa_all_vars_unique(Args, #{}).

aa_all_vars_unique([#b_literal{}|Args], Seen) ->
    aa_all_vars_unique(Args, Seen);
aa_all_vars_unique([#b_var{}=V|Args], Seen) ->
    case Seen of
        #{ V := _ } ->
            false;
        #{} ->
            aa_all_vars_unique(Args, Seen#{V => true })
    end;
aa_all_vars_unique([], _) ->
    true.

aa_construct_term(Dst, Values, SS, AAS) ->
    case aa_all_vars_unique(Values)
        andalso aa_all_dies(Values, Dst, AAS) of
        true ->
            aa_join_ls(Dst, Values, SS);
        false ->
            aa_set_aliased([Dst|Values], SS)
    end.

aa_update_record_get_vars([#b_literal{}, Value|Updates]) ->
    [Value|aa_update_record_get_vars(Updates)];
aa_update_record_get_vars([]) ->
    [].

aa_bif(Dst, element, [_Idx,Tuple], SS, AAS) ->
    %% If we extract a value and the aggregate dies and wasn't aliased,
    %% we should not consider this an aliasing operation.
    aa_alias_if_args_dont_die([Tuple], Dst, SS, AAS);
aa_bif(Dst, hd, Args, SS, AAS) ->
    %% If we extract a value and the aggregate dies and wasn't aliased,
    %% we should not consider this an aliasing operation.
    aa_alias_if_args_dont_die(Args, Dst, SS, AAS);
aa_bif(Dst, tl, Args, SS, AAS) ->
    %% If we extract a value and the aggregate dies and wasn't aliased,
    %% we should not consider this an aliasing operation.
    aa_alias_if_args_dont_die(Args, Dst, SS, AAS);
%% TODO: Ignored for now, as we don't track what's inside maps.
%% aa_bif(_Dst, map_get, _Args, SS, _AAS) ->
%%     SS;
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
    aa_join_ls(Dst, Args, SS).

aa_call(Dst, [#b_local{}=Callee|Args], Anno, SS0,
        #aas{alias_map=AliasMap,st_map=StMap}) ->
    #b_local{name=#b_literal{val=_N},arity=_A} = Callee,
    ?DP("A Call~n  callee: ~p/~p~n  args: ~p~n", [_N, _A, Args]),
    IsNif = is_nif(Callee, StMap),
    case AliasMap of
        #{ Callee := CalleeSS } when not IsNif ->
            ?DP("  The callee is known~n"),
            #opt_st{args=CalleeArgs} = map_get(Callee, StMap),
            ?DP("  args in caller: ~p~n",
                [[{Arg, aa_get_status(Arg, SS0)} || Arg <- Args]]),
            ArgStates = [ aa_get_status(Arg, CalleeSS) || Arg <- CalleeArgs],
            ?DP("  callee arg states: ~p~n", [ArgStates]),
            SS1 = aa_add_call_info(Callee, Args, SS0),
            SS = aa_meet(Args, ArgStates, SS1),
            ?DP("  meet: ~p~n",
                [[{Arg, aa_get_status(Arg, SS1)} || Arg <- Args]]),
            ReturnStatusByType = maps:get(returns, CalleeSS, #{}),
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
            aa_set_status(Dst, ResultStatus, SS);
        _ when IsNif ->
            %% This is a nif, assume that all arguments will be
            %% aliased and that the result is aliased.
            aa_set_aliased([Dst|Args], SS0);
        #{} ->
            %% We don't know anything about the function, don't change
            %% the status of any variables
            SS0
    end;
aa_call(Dst, [_Callee|Args], _Anno, SS, _AAS) ->
    %% This is either a call to a fun or to an external function,
    %% assume that all arguments and the result escape.
    aa_set_aliased([Dst|Args], SS).

%% Add info about the aliasing status of the arguments to the call
aa_add_call_info(Callee, Args, SS0) ->
    ArgStats = [aa_get_status(Arg, SS0) || Arg <- Args],
    NewStats = case SS0 of
                   #{{call_info, Callee} := Stats} ->
                       [aa_meet(A, B) || {A,B} <- zip(Stats, ArgStats)];
                   #{} ->
                       ArgStats
               end,
    SS0#{{call_info, Callee} => NewStats}.

%% Incorporate aliasing information derived when analysing the body of
%% a function into the module-global state.
aa_merge_call_args_status(SS, AAS=#aas{call_args=Info0}) ->
    Info =
        maps:fold(fun({call_info,Callee}, NewArgs, Acc) ->
                          #{ Callee := OldArgs } = Acc,
                          Args = [aa_meet(A, B)
                                  || {A,B} <- zip(NewArgs, OldArgs)],
                          Acc#{Callee => Args};
                     (_, _, Acc) ->
                          Acc
                  end, Info0, SS),
    AAS#aas{call_args=Info}.

aa_get_call_args_status(Args, Callee, #aas{call_args=Info}) ->
    #{ Callee := Status } = Info,
    zip(Args, Status).

aa_pair_extraction(Dst, Pair, Element, SS0) ->
    case SS0 of
        #{{pair,Pair}:=both} ->
            %% Both elements have already been extracted
            aa_set_aliased([Dst,Pair], SS0);
        #{{pair,Pair}:=Element} ->
            %% This element has already been extracted
            aa_set_aliased([Dst,Pair], SS0);
        #{{pair,Pair}:=_Other} ->
            %% Both elements have now been extracted
            aa_join(Dst, Pair, SS0#{{pair,Pair}=>both});
        _ ->
            %% Nothing has been extracted from this pair
            aa_join(Dst, Pair, SS0#{{pair,Pair}=>Element})
    end.

aa_tuple_extraction(Dst, Tuple, #b_literal{val=I}, SS1) ->
    case SS1 of
        #{{tuple_element,Tuple}:=OrdSet0} ->
            case ordsets:is_element(I, OrdSet0) of
                true ->
                    aa_set_aliased([Dst,Tuple], SS1);
                false ->
                    OrdSet = ordsets:add_element(I, OrdSet0),
                    aa_join(Dst, Tuple, SS1#{{tuple_element,Tuple}=>OrdSet})
            end;
        _ ->
            %% There are no aliases yet.
            aa_join(Dst, Tuple, SS1#{{tuple_element,Tuple}=>[I]})
    end.
