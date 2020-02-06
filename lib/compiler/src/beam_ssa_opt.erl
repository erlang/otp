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
%%% This is a collection of various optimizations that don't need a separate
%%% pass by themselves and/or are mutually beneficial to other passes.
%%%
%%% The optimizations are applied in "phases," each with a list of sub-passes
%%% to run. These sub-passes are applied on all functions in a module before
%%% moving on to the next phase, which lets us gather module-level information
%%% in one phase and then apply it in the next without having to risk working
%%% with incomplete information.
%%%
%%% Each sub-pass operates on a #opt_st{} record and a func_info_db(), where
%%% the former is just a #b_function{} whose blocks can be represented either
%%% in linear or map form, and the latter is a map with information about all
%%% functions in the module (see beam_ssa_opt.hrl for more details).
%%%

-module(beam_ssa_opt).
-export([module/2]).

-include("beam_ssa_opt.hrl").

-import(lists, [all/2,append/1,duplicate/2,flatten/1,foldl/3,keyfind/3,
                member/2,reverse/1,reverse/2,splitwith/2,sort/1,
                takewhile/2,unzip/1]).

-define(MAX_REPETITIONS, 16).

-spec module(beam_ssa:b_module(), [compile:option()]) ->
                    {'ok',beam_ssa:b_module()}.

module(Module, Opts) ->
    FuncDb = case proplists:get_value(no_module_opt, Opts, false) of
                 false -> build_func_db(Module);
                 true -> #{}
             end,

    %% Passes that perform module-level optimizations are often aided by
    %% optimizing callers before callees and vice versa, so we optimize all
    %% functions in call order, alternating the order every time.
    StMap0 = build_st_map(Module),
    Order = get_call_order_po(StMap0, FuncDb),

    Phases = [{once, Order, prologue_passes(Opts)},
              {module, module_passes(Opts)},
              {fixpoint, Order, repeated_passes(Opts)},
              {once, Order, epilogue_passes(Opts)}],

    StMap = run_phases(Phases, StMap0, FuncDb),
    {ok, finish(Module, StMap)}.

run_phases([{module, Passes} | Phases], StMap0, FuncDb0) ->
    {StMap, FuncDb} = compile:run_sub_passes(Passes, {StMap0, FuncDb0}),
    run_phases(Phases, StMap, FuncDb);
run_phases([{once, FuncIds0, Passes} | Phases], StMap0, FuncDb0) ->
    FuncIds = skip_removed(FuncIds0, StMap0),
    {StMap, FuncDb} = phase(FuncIds, Passes, StMap0, FuncDb0),
    run_phases(Phases, StMap, FuncDb);
run_phases([{fixpoint, FuncIds0, Passes} | Phases], StMap0, FuncDb0) ->
    FuncIds = skip_removed(FuncIds0, StMap0),
    RevFuncIds = reverse(FuncIds),
    Order = {FuncIds, RevFuncIds},
    {StMap, FuncDb} = fixpoint(RevFuncIds, Order, Passes,
                               StMap0, FuncDb0, ?MAX_REPETITIONS),
    run_phases(Phases, StMap, FuncDb);
run_phases([], StMap, _FuncDb) ->
    StMap.

skip_removed(FuncIds, StMap) ->
    [F || F <- FuncIds, is_map_key(F, StMap)].

%% Run the given passes until a fixpoint is reached.
fixpoint(_FuncIds, _Order, _Passes, StMap, FuncDb, 0) ->
    %% Too many repetitions. Give up and return what we have.
    {StMap, FuncDb};
fixpoint(FuncIds0, Order0, Passes, StMap0, FuncDb0, N) ->
    {StMap, FuncDb} = phase(FuncIds0, Passes, StMap0, FuncDb0),
    Repeat = changed(FuncIds0, FuncDb0, FuncDb, StMap0, StMap),
    case cerl_sets:size(Repeat) of
        0 ->
            %% No change. Fixpoint reached.
            {StMap, FuncDb};
        _ ->
            %% Repeat the optimizations for functions whose code has
            %% changed or for which there is potentially updated type
            %% information.
            {OrderA, OrderB} = Order0,
            Order = {OrderB, OrderA},
            FuncIds = [Id || Id <- OrderA, cerl_sets:is_element(Id, Repeat)],
            fixpoint(FuncIds, Order, Passes, StMap, FuncDb, N - 1)
    end.

phase([FuncId | Ids], Ps, StMap, FuncDb0) ->
    try compile:run_sub_passes(Ps, {map_get(FuncId, StMap), FuncDb0}) of
        {St, FuncDb} ->
            phase(Ids, Ps, StMap#{ FuncId => St }, FuncDb)
    catch
        Class:Error:Stack ->
            #b_local{name=#b_literal{val=Name},arity=Arity} = FuncId,
            io:fwrite("Function: ~w/~w\n", [Name,Arity]),
            erlang:raise(Class, Error, Stack)
    end;
phase([], _Ps, StMap, FuncDb) ->
    {StMap, FuncDb}.

changed(PrevIds, FuncDb0, FuncDb, StMap0, StMap) ->
    %% Find all functions in FuncDb that can be reached by changes
    %% of argument and/or return types. Those are the functions that
    %% may gain from running the optimization passes again.
    %%
    %% Note that we examine all functions in FuncDb, not only functions
    %% optimized in the previous run, because the argument types can
    %% have been updated for functions not included in the previous run.

    F = fun(Id, A) ->
                case cerl_sets:is_element(Id, A) of
                    true ->
                        A;
                    false ->
                        {#func_info{arg_types=ATs0,succ_types=ST0},
                         #func_info{arg_types=ATs1,succ_types=ST1}} =
                            {map_get(Id, FuncDb0),map_get(Id, FuncDb)},

                        %% If the argument types have changed for this
                        %% function, re-optimize this function and all
                        %% functions it calls directly or indirectly.
                        %%
                        %% If the return type has changed, re-optimize
                        %% this function and all functions that call
                        %% this function directly or indirectly.
                        Opts = case ATs0 =:= ATs1 of
                                    true -> [];
                                    false -> [called]
                                end ++
                            case ST0 =:= ST1 of
                                true -> [];
                                false -> [callers]
                            end,
                        case Opts of
                            [] -> A;
                            [_|_] -> add_changed([Id], Opts, FuncDb, A)
                        end
                end
        end,
    Ids = foldl(F, cerl_sets:new(), maps:keys(FuncDb)),

    %% From all functions that were optimized in the previous run,
    %% find the functions that had any change in the SSA code. Those
    %% functions might gain from being optimized again. (For example,
    %% when beam_ssa_dead has shortcut branches, the types for some
    %% variables could become narrower, giving beam_ssa_type new
    %% opportunities for optimization.)
    %%
    %% Note that the functions examined could be functions with module-level
    %% optimization turned off (and thus not included in FuncDb).

    foldl(fun(Id, A) ->
                  case cerl_sets:is_element(Id, A) of
                      true ->
                          %% Already scheduled for another optimization.
                          %% No need to compare the SSA code.
                          A;
                      false ->
                          %% Compare the SSA code before and after optimization.
                          case {map_get(Id, StMap0),map_get(Id, StMap)} of
                              {Same,Same} -> A;
                              {_,_} -> cerl_sets:add_element(Id, A)
                          end
                  end
          end, Ids, PrevIds).

add_changed([Id|Ids], Opts, FuncDb, S0) when is_map_key(Id, FuncDb) ->
    case cerl_sets:is_element(Id, S0) of
        true ->
            add_changed(Ids, Opts, FuncDb, S0);
        false ->
            S1 = cerl_sets:add_element(Id, S0),
            #func_info{in=In,out=Out} = map_get(Id, FuncDb),
            S2 = case member(callers, Opts) of
                     true -> add_changed(In, Opts, FuncDb, S1);
                     false -> S1
                 end,
            S = case member(called, Opts) of
                    true -> add_changed(Out, Opts, FuncDb, S2);
                    false -> S2
                end,
            add_changed(Ids, Opts, FuncDb, S)
    end;
add_changed([_|Ids], Opts, FuncDb, S) ->
    %% This function is exempt from module-level optimization and will not
    %% provide any more information.
    add_changed(Ids, Opts, FuncDb, S);
add_changed([], _, _, S) -> S.

%%

get_func_id(F) ->
    {_Mod, Name, Arity} = beam_ssa:get_anno(func_info, F),
    #b_local{name=#b_literal{val=Name}, arity=Arity}.

-spec build_st_map(#b_module{}) -> st_map().
build_st_map(#b_module{body=Fs}) ->
    build_st_map_1(Fs, #{}).

build_st_map_1([F | Fs], Map) ->
    #b_function{anno=Anno,args=Args,cnt=Counter,bs=Bs} = F,
    St = #opt_st{anno=Anno,args=Args,cnt=Counter,ssa=Bs},
    build_st_map_1(Fs, Map#{ get_func_id(F) => St });
build_st_map_1([], Map) ->
    Map.

-spec finish(#b_module{}, st_map()) -> #b_module{}.
finish(#b_module{body=Fs0}=Module, StMap) ->
    Module#b_module{body=finish_1(Fs0, StMap)}.

finish_1([F0 | Fs], StMap) ->
    FuncId = get_func_id(F0),
    case StMap of
        #{ FuncId := #opt_st{anno=Anno,cnt=Counter,ssa=Blocks} } ->
            F = F0#b_function{anno=Anno,bs=Blocks,cnt=Counter},
            [F | finish_1(Fs, StMap)];
        #{} ->
            finish_1(Fs, StMap)
    end;
finish_1([], _StMap) ->
    [].

%%

-define(PASS(N), {N,fun N/1}).

prologue_passes(Opts) ->
    Ps = [?PASS(ssa_opt_split_blocks),
          ?PASS(ssa_opt_coalesce_phis),
          ?PASS(ssa_opt_tail_phis),
          ?PASS(ssa_opt_element),
          ?PASS(ssa_opt_linearize),
          ?PASS(ssa_opt_tuple_size),
          ?PASS(ssa_opt_record),
          ?PASS(ssa_opt_cse),                   % Helps the first type pass.
          ?PASS(ssa_opt_live)],                 % ...
    passes_1(Ps, Opts).

module_passes(Opts) ->
    Ps0 = [{ssa_opt_type_start,
            fun({StMap, FuncDb}) ->
                    beam_ssa_type:opt_start(StMap, FuncDb)
            end}],
    passes_1(Ps0, Opts).

%% These passes all benefit from each other (in roughly this order), so they
%% are repeated as required.
repeated_passes(Opts) ->
    Ps = [?PASS(ssa_opt_live),
          ?PASS(ssa_opt_bs_puts),
          ?PASS(ssa_opt_dead),
          ?PASS(ssa_opt_cse),
          ?PASS(ssa_opt_tail_phis),
          ?PASS(ssa_opt_sink),
          ?PASS(ssa_opt_tuple_size),
          ?PASS(ssa_opt_record),
          ?PASS(ssa_opt_type_continue)],        %Must run after ssa_opt_dead to
                                                %clean up phi nodes.
    passes_1(Ps, Opts).

epilogue_passes(Opts) ->
    Ps = [?PASS(ssa_opt_type_finish),
          ?PASS(ssa_opt_float),
          ?PASS(ssa_opt_sw),

          %% Run live one more time to clean up after the float and sw
          %% passes.
          ?PASS(ssa_opt_live),
          ?PASS(ssa_opt_try),
          ?PASS(ssa_opt_bsm),
          ?PASS(ssa_opt_bsm_shortcut),
          ?PASS(ssa_opt_sink),
          ?PASS(ssa_opt_blockify),
          ?PASS(ssa_opt_merge_blocks),
          ?PASS(ssa_opt_get_tuple_element),
          ?PASS(ssa_opt_trim_unreachable),
          ?PASS(ssa_opt_unfold_literals)],
    passes_1(Ps, Opts).

passes_1(Ps, Opts0) ->
    Negations = [{list_to_atom("no_"++atom_to_list(N)),N} ||
                    {N,_} <- Ps],
    Opts = proplists:substitute_negations(Negations, Opts0),
    [case proplists:get_value(Name, Opts, true) of
         true ->
             P;
         false ->
             {NoName,Name} = keyfind(Name, 2, Negations),
             {NoName,fun(S) -> S end}
     end || {Name,_}=P <- Ps].

%% Builds a function information map with basic information about incoming and
%% outgoing local calls, as well as whether the function is exported.
-spec build_func_db(#b_module{}) -> func_info_db().
build_func_db(#b_module{body=Fs,attributes=Attr,exports=Exports0}) ->
    Exports = fdb_exports(Attr, Exports0),
    try
        fdb_fs(Fs, Exports, #{})
    catch
        %% All module-level optimizations are invalid when a NIF can override a
        %% function, so we have to bail out.
        throw:load_nif -> #{}
    end.

fdb_exports([{on_load, L} | Attrs], Exports) ->
    %% Functions marked with on_load must be treated as exported to prevent
    %% them from being optimized away when unused.
    fdb_exports(Attrs, flatten(L) ++ Exports);
fdb_exports([_Attr | Attrs], Exports) ->
    fdb_exports(Attrs, Exports);
fdb_exports([], Exports) ->
    gb_sets:from_list(Exports).

fdb_fs([#b_function{ args=Args,bs=Bs }=F | Fs], Exports, FuncDb0) ->
    Id = get_func_id(F),

    #b_local{name=#b_literal{val=Name}, arity=Arity} = Id,
    Exported = gb_sets:is_element({Name, Arity}, Exports),
    ArgTypes = duplicate(length(Args), #{}),

    FuncDb1 = case FuncDb0 of
                  %% We may have an entry already if someone's called us.
                  #{ Id := Info } ->
                      FuncDb0#{ Id := Info#func_info{ exported=Exported,
                                                      arg_types=ArgTypes }};
                  #{} ->
                      FuncDb0#{ Id => #func_info{ exported=Exported,
                                                  arg_types=ArgTypes }}
              end,

    FuncDb = beam_ssa:fold_rpo(fun(_L, #b_blk{is=Is}, FuncDb) ->
                                       fdb_is(Is, Id, FuncDb)
                               end, FuncDb1, Bs),

    fdb_fs(Fs, Exports, FuncDb);
fdb_fs([], _Exports, FuncDb) ->
    FuncDb.

fdb_is([#b_set{op=call,
               args=[#b_local{}=Callee | _]} | Is],
       Caller, FuncDb) ->
    fdb_is(Is, Caller, fdb_update(Caller, Callee, FuncDb));
fdb_is([#b_set{op=call,
               args=[#b_remote{mod=#b_literal{val=erlang},
                               name=#b_literal{val=load_nif}},
                     _Path, _LoadInfo]} | _Is], _Caller, _FuncDb) ->
    throw(load_nif);
fdb_is([#b_set{op=make_fun,
               args=[#b_local{}=Callee | _]} | Is],
       Caller, FuncDb) ->
    %% The make_fun instruction's type depends on the return type of the
    %% function in question, so we treat this as a function call.
    fdb_is(Is, Caller, fdb_update(Caller, Callee, FuncDb));
fdb_is([_ | Is], Caller, FuncDb) ->
    fdb_is(Is, Caller, FuncDb);
fdb_is([], _Caller, FuncDb) ->
    FuncDb.

fdb_update(Caller, Callee, FuncDb) ->
    CallerVertex = maps:get(Caller, FuncDb, #func_info{}),
    CalleeVertex = maps:get(Callee, FuncDb, #func_info{}),

    Calls = ordsets:add_element(Callee, CallerVertex#func_info.out),
    CalledBy = ordsets:add_element(Caller, CalleeVertex#func_info.in),

    FuncDb#{ Caller => CallerVertex#func_info{out=Calls},
             Callee => CalleeVertex#func_info{in=CalledBy} }.

%% Returns the post-order of all local calls in this module. That is,
%% called functions will be ordered before the functions calling them.
%%
%% Functions where module-level optimization is disabled are added last in
%% arbitrary order.

get_call_order_po(StMap, FuncDb) ->
    Order = gco_po(FuncDb),
    Order ++ maps:fold(fun(K, _V, Acc) ->
                               case is_map_key(K, FuncDb) of
                                   false -> [K | Acc];
                                   true -> Acc
                               end
                       end, [], StMap).

gco_po(FuncDb) ->
    All = sort(maps:keys(FuncDb)),
    {RPO,_} = gco_rpo(All, FuncDb, cerl_sets:new(), []),
    reverse(RPO).

gco_rpo([Id|Ids], FuncDb, Seen0, Acc0) ->
    case cerl_sets:is_element(Id, Seen0) of
        true ->
            gco_rpo(Ids, FuncDb, Seen0, Acc0);
        false ->
            #func_info{out=Successors} = map_get(Id, FuncDb),
            Seen1 = cerl_sets:add_element(Id, Seen0),
            {Acc,Seen} = gco_rpo(Successors, FuncDb, Seen1, Acc0),
            gco_rpo(Ids, FuncDb, Seen, [Id|Acc])
    end;
gco_rpo([], _, Seen, Acc) ->
    {Acc,Seen}.

%%%
%%% Trivial sub passes.
%%%

ssa_opt_dead({#opt_st{ssa=Linear}=St, FuncDb}) ->
    {St#opt_st{ssa=beam_ssa_dead:opt(Linear)}, FuncDb}.

ssa_opt_linearize({#opt_st{ssa=Blocks}=St, FuncDb}) ->
    {St#opt_st{ssa=beam_ssa:linearize(Blocks)}, FuncDb}.

ssa_opt_type_continue({#opt_st{ssa=Linear0,args=Args,anno=Anno}=St0, FuncDb0}) ->
    {Linear, FuncDb} = beam_ssa_type:opt_continue(Linear0, Args, Anno, FuncDb0),
    {St0#opt_st{ssa=Linear}, FuncDb}.

ssa_opt_type_finish({#opt_st{args=Args,anno=Anno0}=St0, FuncDb0}) ->
    {Anno, FuncDb} = beam_ssa_type:opt_finish(Args, Anno0, FuncDb0),
    {St0#opt_st{anno=Anno}, FuncDb}.

ssa_opt_blockify({#opt_st{ssa=Linear}=St, FuncDb}) ->
    {St#opt_st{ssa=maps:from_list(Linear)}, FuncDb}.

ssa_opt_trim_unreachable({#opt_st{ssa=Blocks}=St, FuncDb}) ->
    {St#opt_st{ssa=beam_ssa:trim_unreachable(Blocks)}, FuncDb}.

%%%
%%% Split blocks before certain instructions to enable more optimizations.
%%%
%%% Splitting before element/2 enables the optimization that swaps
%%% element/2 instructions.
%%%
%%% Splitting before call and make_fun instructions gives more opportunities
%%% for sinking get_tuple_element instructions.
%%%

ssa_opt_split_blocks({#opt_st{ssa=Blocks0,cnt=Count0}=St, FuncDb}) ->
    P = fun(#b_set{op={bif,element}}) -> true;
           (#b_set{op=call}) -> true;
           (#b_set{op=make_fun}) -> true;
           (_) -> false
        end,
    {Blocks,Count} = beam_ssa:split_blocks(P, Blocks0, Count0),
    {St#opt_st{ssa=Blocks,cnt=Count}, FuncDb}.

%%%
%%% Coalesce phi nodes.
%%%
%%% Nested cases can led to code such as this:
%%%
%%%     10:
%%%       _1 = phi {literal value1, label 8}, {Var, label 9}
%%%       br 11
%%%
%%%     11:
%%%       _2 = phi {_1, label 10}, {literal false, label 3}
%%%
%%% The phi nodes can be coalesced like this:
%%%
%%%     11:
%%%       _2 = phi {literal value1, label 8}, {Var, label 9}, {literal false, label 3}
%%%
%%% Coalescing can help other optimizations, and can in some cases reduce register
%%% shuffling (if the phi variables for two phi nodes happens to be allocated to
%%% different registers).
%%%

ssa_opt_coalesce_phis({#opt_st{ssa=Blocks0}=St, FuncDb}) ->
    Ls = beam_ssa:rpo(Blocks0),
    Blocks = c_phis_1(Ls, Blocks0),
    {St#opt_st{ssa=Blocks}, FuncDb}.

c_phis_1([L|Ls], Blocks0) ->
    case map_get(L, Blocks0) of
        #b_blk{is=[#b_set{op=phi}|_]}=Blk ->
            Blocks = c_phis_2(L, Blk, Blocks0),
            c_phis_1(Ls, Blocks);
        #b_blk{} ->
            c_phis_1(Ls, Blocks0)
    end;
c_phis_1([], Blocks) -> Blocks.

c_phis_2(L, #b_blk{is=Is0}=Blk0, Blocks0) ->
    case c_phis_args(Is0, Blocks0) of
        none ->
            Blocks0;
        {_,_,Preds}=Info ->
            Is = c_rewrite_phis(Is0, Info),
            Blk = Blk0#b_blk{is=Is},
            Blocks = Blocks0#{L:=Blk},
            c_fix_branches(Preds, L, Blocks)
    end.

c_phis_args([#b_set{op=phi,args=Args0}|Is], Blocks) ->
    case c_phis_args_1(Args0, Blocks) of
        none ->
            c_phis_args(Is, Blocks);
        Res ->
            Res
    end;
c_phis_args(_, _Blocks) -> none.

c_phis_args_1([{Var,Pred}|As], Blocks) ->
    case c_get_pred_vars(Var, Pred, Blocks) of
        none ->
            c_phis_args_1(As, Blocks);
        Result ->
            Result
    end;
c_phis_args_1([], _Blocks) -> none.

c_get_pred_vars(Var, Pred, Blocks) ->
    case map_get(Pred, Blocks) of
        #b_blk{is=[#b_set{op=phi,dst=Var,args=Args}]} ->
            {Var,Pred,Args};
        #b_blk{} ->
            none
    end.

c_rewrite_phis([#b_set{op=phi,args=Args0}=I|Is], Info) ->
    Args = c_rewrite_phi(Args0, Info),
    [I#b_set{args=Args}|c_rewrite_phis(Is, Info)];
c_rewrite_phis(Is, _Info) -> Is.

c_rewrite_phi([{Var,Pred}|As], {Var,Pred,Values}) ->
    Values ++ As;
c_rewrite_phi([{Value,Pred}|As], {_,Pred,Values}) ->
    [{Value,P} || {_,P} <- Values] ++ As;
c_rewrite_phi([A|As], Info) ->
    [A|c_rewrite_phi(As, Info)];
c_rewrite_phi([], _Info) -> [].

c_fix_branches([{_,Pred}|As], L, Blocks0) ->
    #b_blk{last=Last0} = Blk0 = map_get(Pred, Blocks0),
    #b_br{bool=#b_literal{val=true}} = Last0,   %Assertion.
    Last = Last0#b_br{bool=#b_literal{val=true},succ=L,fail=L},
    Blk = Blk0#b_blk{last=Last},
    Blocks = Blocks0#{Pred:=Blk},
    c_fix_branches(As, L, Blocks);
c_fix_branches([], _, Blocks) -> Blocks.

%%%
%%% Eliminate phi nodes in the tail of a function.
%%%
%%% Try to eliminate short blocks that starts with a phi node
%%% and end in a return. For example:
%%%
%%%    Result = phi { Res1, 4 }, { literal true, 5 }
%%%    Ret = put_tuple literal ok, Result
%%%    ret Ret
%%%
%%% The code in this block can be inserted at the end blocks 4 and
%%% 5. Thus, the following code can be inserted into block 4:
%%%
%%%    Ret:1 = put_tuple literal ok, Res1
%%%    ret Ret:1
%%%
%%% And the following code into block 5:
%%%
%%%    Ret:2 = put_tuple literal ok, literal true
%%%    ret Ret:2
%%%
%%% Which can be further simplified to:
%%%
%%%    ret literal {ok, true}
%%%
%%% This transformation may lead to more code improvements:
%%%
%%%   - Stack trimming
%%%   - Fewer test_heap instructions
%%%   - Smaller stack frames
%%%

ssa_opt_tail_phis({#opt_st{ssa=SSA0,cnt=Count0}=St, FuncDb}) ->
    {SSA,Count} = opt_tail_phis(SSA0, Count0),
    {St#opt_st{ssa=SSA,cnt=Count}, FuncDb}.

opt_tail_phis(Blocks, Count) when is_map(Blocks) ->
    opt_tail_phis(maps:values(Blocks), Blocks, Count);
opt_tail_phis(Linear0, Count0) when is_list(Linear0) ->
    Blocks0 = maps:from_list(Linear0),
    {Blocks,Count} = opt_tail_phis(Blocks0, Count0),
    {beam_ssa:linearize(Blocks),Count}.

opt_tail_phis([#b_blk{is=Is0,last=Last}|Bs], Blocks0, Count0) ->
    case {Is0,Last} of
        {[#b_set{op=phi,args=[_,_|_]}|_],#b_ret{arg=#b_var{}}=Ret} ->
            {Phis,Is} = splitwith(fun(#b_set{op=Op}) -> Op =:= phi end, Is0),
            case suitable_tail_ops(Is) of
                true ->
                    {Blocks,Count} = opt_tail_phi(Phis, Is, Ret,
                                                  Blocks0, Count0),
                    opt_tail_phis(Bs, Blocks, Count);
                false ->
                    opt_tail_phis(Bs, Blocks0, Count0)
            end;
        {_,_} ->
            opt_tail_phis(Bs, Blocks0, Count0)
    end;
opt_tail_phis([], Blocks, Count) ->
    {Blocks,Count}.

opt_tail_phi(Phis0, Is, Ret, Blocks0, Count0) ->
    Phis = rel2fam(reduce_phis(Phis0)),
    {Blocks,Count,Cost} =
        foldl(fun(PhiArg, Acc) ->
                      opt_tail_phi_arg(PhiArg, Is, Ret, Acc)
              end, {Blocks0,Count0,0}, Phis),
    MaxCost = length(Phis) * 3 + 2,
    if
        Cost =< MaxCost ->
            %% The transformation would cause at most a slight
            %% increase in code size if no more optimizations
            %% can be applied.
            {Blocks,Count};
        true ->
            %% The code size would be increased too much.
            {Blocks0,Count0}
    end.

reduce_phis([#b_set{dst=PhiDst,args=PhiArgs}|Is]) ->
    [{L,{PhiDst,Val}} || {Val,L} <- PhiArgs] ++ reduce_phis(Is);
reduce_phis([]) -> [].

opt_tail_phi_arg({PredL,Sub0}, Is0, Ret0, {Blocks0,Count0,Cost0}) ->
    Blk0 = map_get(PredL, Blocks0),
    #b_blk{is=IsPrefix,last=#b_br{succ=Next,fail=Next}} = Blk0,
    case is_exit_bif(IsPrefix) of
        false ->
            Sub1 = maps:from_list(Sub0),
            {Is1,Count,Sub} = new_names(Is0, Sub1, Count0, []),
            Is2 = [sub(I, Sub) || I <- Is1],
            Cost = build_cost(Is2, Cost0),
            Is = IsPrefix ++ Is2,
            Ret = sub(Ret0, Sub),
            Blk = Blk0#b_blk{is=Is,last=Ret},
            Blocks = Blocks0#{PredL:=Blk},
            {Blocks,Count,Cost};
        true ->
            %% The block ends in a call to a function that
            %% will cause an exception.
            {Blocks0,Count0,Cost0+3}
    end.

is_exit_bif([#b_set{op=call,
                    args=[#b_remote{mod=#b_literal{val=Mod},
                                    name=#b_literal{val=Name}}|Args]}]) ->
    erl_bifs:is_exit_bif(Mod, Name, length(Args));
is_exit_bif(_) -> false.

new_names([#b_set{dst=Dst}=I|Is], Sub0, Count0, Acc) ->
    {NewDst,Count} = new_var(Dst, Count0),
    Sub = Sub0#{Dst=>NewDst},
    new_names(Is, Sub, Count, [I#b_set{dst=NewDst}|Acc]);
new_names([], Sub, Count, Acc) ->
    {reverse(Acc),Count,Sub}.

suitable_tail_ops(Is) ->
    all(fun(#b_set{op=Op}) ->
                is_suitable_tail_op(Op)
        end, Is).

is_suitable_tail_op({bif,_}) -> true;
is_suitable_tail_op(put_list) -> true;
is_suitable_tail_op(put_tuple) -> true;
is_suitable_tail_op(_) -> false.

build_cost([#b_set{op=put_list,args=Args}|Is], Cost) ->
    case are_all_literals(Args) of
        true ->
            build_cost(Is, Cost);
        false ->
            build_cost(Is, Cost + 1)
    end;
build_cost([#b_set{op=put_tuple,args=Args}|Is], Cost) ->
    case are_all_literals(Args) of
        true ->
            build_cost(Is, Cost);
        false ->
            build_cost(Is, Cost + length(Args) + 1)
    end;
build_cost([#b_set{op={bif,_},args=Args}|Is], Cost) ->
    case are_all_literals(Args) of
        true ->
            build_cost(Is, Cost);
        false ->
            build_cost(Is, Cost + 1)
    end;
build_cost([], Cost) -> Cost.

are_all_literals(Args) ->
    all(fun(#b_literal{}) -> true;
                (_) -> false
             end, Args).

%%%
%%% Order element/2 calls.
%%%
%%% Order an unbroken chain of element/2 calls for the same tuple
%%% with the same failure label so that the highest element is
%%% retrieved first. That will allow the other element/2 calls to
%%% be replaced with get_tuple_element/3 instructions.
%%%

ssa_opt_element({#opt_st{ssa=Blocks}=St, FuncDb}) ->
    %% Collect the information about element instructions in this
    %% function.
    GetEls = collect_element_calls(beam_ssa:linearize(Blocks)),

    %% Collect the element instructions into chains. The
    %% element calls in each chain are ordered in reverse
    %% execution order.
    Chains = collect_chains(GetEls, []),

    %% For each chain, swap the first element call with the
    %% element call with the highest index.
    {St#opt_st{ssa=swap_element_calls(Chains, Blocks)}, FuncDb}.

collect_element_calls([{L,#b_blk{is=Is0,last=Last}}|Bs]) ->
    case {Is0,Last} of
        {[#b_set{op={bif,element},dst=Element,
                 args=[#b_literal{val=N},#b_var{}=Tuple]},
          #b_set{op=succeeded,dst=Bool,args=[Element]}],
         #b_br{bool=Bool,succ=Succ,fail=Fail}} ->
            Info = {L,Succ,{Tuple,Fail},N},
            [Info|collect_element_calls(Bs)];
        {_,_} ->
            collect_element_calls(Bs)
    end;
collect_element_calls([]) -> [].

collect_chains([{This,_,V,_}=El|Els], [{_,This,V,_}|_]=Chain) ->
    %% Add to the previous chain.
    collect_chains(Els, [El|Chain]);
collect_chains([El|Els], [_,_|_]=Chain) ->
    %% Save the previous chain and start a new chain.
    [Chain|collect_chains(Els, [El])];
collect_chains([El|Els], _Chain) ->
    %% The previous chain is too short; discard it and start a new.
    collect_chains(Els, [El]);
collect_chains([], [_,_|_]=Chain) ->
    %% Save the last chain.
    [Chain];
collect_chains([], _) ->  [].

swap_element_calls([[{L,_,_,N}|_]=Chain|Chains], Blocks0) ->
    Blocks = swap_element_calls_1(Chain, {N,L}, Blocks0),
    swap_element_calls(Chains, Blocks);
swap_element_calls([], Blocks) -> Blocks.

swap_element_calls_1([{L1,_,_,N1}], {N2,L2}, Blocks) when N2 > N1 ->
    %% We have reached the end of the chain, and the first
    %% element instrution to be executed. Its index is lower
    %% than the maximum index found while traversing the chain,
    %% so we will need to swap the instructions.
    #{L1:=Blk1,L2:=Blk2} = Blocks,
    [#b_set{dst=Dst1}=GetEl1,Succ1] = Blk1#b_blk.is,
    [#b_set{dst=Dst2}=GetEl2,Succ2] = Blk2#b_blk.is,
    Is1 = [GetEl2,Succ1#b_set{args=[Dst2]}],
    Is2 = [GetEl1,Succ2#b_set{args=[Dst1]}],
    Blocks#{L1:=Blk1#b_blk{is=Is1},L2:=Blk2#b_blk{is=Is2}};
swap_element_calls_1([{L,_,_,N1}|Els], {N2,_}, Blocks) when N1 > N2 ->
    swap_element_calls_1(Els, {N2,L}, Blocks);
swap_element_calls_1([_|Els], Highest, Blocks) ->
    swap_element_calls_1(Els, Highest, Blocks);
swap_element_calls_1([], _, Blocks) ->
    %% Nothing to do. The element call with highest index
    %% is already the first one to be executed.
    Blocks.

%%%
%%% Record optimization.
%%%
%%% Replace tuple matching with an is_tagged_tuple instruction
%%% when applicable.
%%%

ssa_opt_record({#opt_st{ssa=Linear}=St, FuncDb}) ->
    Blocks = maps:from_list(Linear),
    {St#opt_st{ssa=record_opt(Linear, Blocks)}, FuncDb}.

record_opt([{L,#b_blk{is=Is0,last=Last}=Blk0}|Bs], Blocks) ->
    Is = record_opt_is(Is0, Last, Blocks),
    Blk = Blk0#b_blk{is=Is},
    [{L,Blk}|record_opt(Bs, Blocks)];
record_opt([], _Blocks) -> [].

record_opt_is([#b_set{op={bif,is_tuple},dst=Bool,args=[Tuple]}=Set],
              Last, Blocks) ->
    case is_tagged_tuple(Tuple, Bool, Last, Blocks) of
        {yes,Size,Tag} ->
            Args = [Tuple,Size,Tag],
            [Set#b_set{op=is_tagged_tuple,args=Args}];
        no ->
            [Set]
    end;
record_opt_is([I|Is]=Is0, #b_br{bool=Bool}=Last, Blocks) ->
    case is_tagged_tuple_1(Is0, Last, Blocks) of
        {yes,_Fail,Tuple,Arity,Tag} ->
            Args = [Tuple,Arity,Tag],
            [I#b_set{op=is_tagged_tuple,dst=Bool,args=Args}];
        no ->
            [I|record_opt_is(Is, Last, Blocks)]
    end;
record_opt_is([I|Is], Last, Blocks) ->
    [I|record_opt_is(Is, Last, Blocks)];
record_opt_is([], _Last, _Blocks) -> [].

is_tagged_tuple(#b_var{}=Tuple, Bool,
                #b_br{bool=Bool,succ=Succ,fail=Fail},
                Blocks) ->
    #b_blk{is=Is,last=Last} = map_get(Succ, Blocks),
    case is_tagged_tuple_1(Is, Last, Blocks) of
        {yes,Fail,Tuple,Arity,Tag} ->
            {yes,Arity,Tag};
        _ ->
            no
    end;
is_tagged_tuple(_, _, _, _) -> no.

is_tagged_tuple_1(Is, Last, Blocks) ->
    case {Is,Last} of
        {[#b_set{op={bif,tuple_size},dst=ArityVar,
                 args=[#b_var{}=Tuple]},
          #b_set{op={bif,'=:='},
                 dst=Bool,
                 args=[ArityVar, #b_literal{val=ArityVal}=Arity]}],
         #b_br{bool=Bool,succ=Succ,fail=Fail}}
          when is_integer(ArityVal) ->
            SuccBlk = map_get(Succ, Blocks),
            case is_tagged_tuple_2(SuccBlk, Tuple, Fail) of
                no ->
                    no;
                {yes,Tag} ->
                    {yes,Fail,Tuple,Arity,Tag}
            end;
        _ ->
            no
    end.

is_tagged_tuple_2(#b_blk{is=Is,
                         last=#b_br{bool=#b_var{}=Bool,fail=Fail}},
                  Tuple, Fail) ->
    is_tagged_tuple_3(Is, Bool, Tuple);
is_tagged_tuple_2(#b_blk{}, _, _) -> no.

is_tagged_tuple_3([#b_set{op=get_tuple_element,
                          dst=TagVar,
                          args=[#b_var{}=Tuple,#b_literal{val=0}]}|Is],
                  Bool, Tuple) ->
    is_tagged_tuple_4(Is, Bool, TagVar);
is_tagged_tuple_3([_|Is], Bool, Tuple) ->
    is_tagged_tuple_3(Is, Bool, Tuple);
is_tagged_tuple_3([], _, _) -> no.

is_tagged_tuple_4([#b_set{op={bif,'=:='},dst=Bool,
                          args=[#b_var{}=TagVar,
                                #b_literal{val=TagVal}=Tag]}],
                 Bool, TagVar) when is_atom(TagVal) ->
    {yes,Tag};
is_tagged_tuple_4([_|Is], Bool, TagVar) ->
    is_tagged_tuple_4(Is, Bool, TagVar);
is_tagged_tuple_4([], _, _) -> no.

%%%
%%% Common subexpression elimination (CSE).
%%%
%%% Eliminate repeated evaluation of identical expressions. To avoid
%%% increasing the size of the stack frame, we don't eliminate
%%% subexpressions across instructions that clobber the X registers.
%%%

ssa_opt_cse({#opt_st{ssa=Linear}=St, FuncDb}) ->
    M = #{0=>#{}},
    {St#opt_st{ssa=cse(Linear, #{}, M)}, FuncDb}.

cse([{L,#b_blk{is=Is0,last=Last0}=Blk}|Bs], Sub0, M0) ->
    Es0 = map_get(L, M0),
    {Is1,Es,Sub} = cse_is(Is0, Es0, Sub0, []),
    Last = sub(Last0, Sub),
    M = cse_successors(Is1, Blk, Es, M0),
    Is = reverse(Is1),
    [{L,Blk#b_blk{is=Is,last=Last}}|cse(Bs, Sub, M)];
cse([], _, _) -> [].

cse_successors([#b_set{op=succeeded,args=[Src]},Bif|_], Blk, EsSucc, M0) ->
    case cse_suitable(Bif) of
        true ->
            %% The previous instruction only has a valid value at the success branch.
            %% We must remove the substitution for Src from the failure branch.
            #b_blk{last=#b_br{succ=Succ,fail=Fail}} = Blk,
            M = cse_successors_1([Succ], EsSucc, M0),
            EsFail = maps:filter(fun(_, Val) -> Val =/= Src end, EsSucc),
            cse_successors_1([Fail], EsFail, M);
        false ->
            %% There can't be any replacement for Src in EsSucc. No need for
            %% any special handling.
            cse_successors_1(beam_ssa:successors(Blk), EsSucc, M0)
    end;
cse_successors(_Is, Blk, Es, M) ->
    cse_successors_1(beam_ssa:successors(Blk), Es, M).

cse_successors_1([L|Ls], Es0, M) ->
    case M of
        #{L:=Es1} when map_size(Es1) =:= 0 ->
            %% The map is already empty. No need to do anything
            %% since the intersection will be empty.
            cse_successors_1(Ls, Es0, M);
        #{L:=Es1} ->
            %% Calculate the intersection of the two maps.
            %% Both keys and values must match.
            Es = maps:filter(fun(Key, Value) ->
                                     case Es1 of
                                         #{Key:=Value} -> true;
                                         #{} -> false
                                     end
                             end, Es0),
            cse_successors_1(Ls, Es0, M#{L:=Es});
        #{} ->
            cse_successors_1(Ls, Es0, M#{L=>Es0})
    end;
cse_successors_1([], _, M) -> M.

cse_is([#b_set{op=succeeded,dst=Bool,args=[Src]}=I0|Is], Es, Sub0, Acc) ->
    I = sub(I0, Sub0),
    case I of
        #b_set{args=[Src]} ->
            cse_is(Is, Es, Sub0, [I|Acc]);
        #b_set{} ->
            %% The previous instruction has been eliminated. Eliminate the
            %% 'succeeded' instruction too.
            Sub = Sub0#{Bool=>#b_literal{val=true}},
            cse_is(Is, Es, Sub, Acc)
    end;
cse_is([#b_set{dst=Dst}=I0|Is], Es0, Sub0, Acc) ->
    I = sub(I0, Sub0),
    case beam_ssa:clobbers_xregs(I) of
        true ->
            %% Retaining the expressions map across calls and other
            %% clobbering instructions would work, but it would cause
            %% the common subexpressions to be saved to Y registers,
            %% which would probably increase the size of the stack
            %% frame.
            cse_is(Is, #{}, Sub0, [I|Acc]);
        false ->
            case cse_expr(I) of
                none ->
                    %% Not suitable for CSE.
                    cse_is(Is, Es0, Sub0, [I|Acc]);
                {ok,ExprKey} ->
                    case Es0 of
                        #{ExprKey:=Src} ->
                            Sub = Sub0#{Dst=>Src},
                            cse_is(Is, Es0, Sub, Acc);
                        #{} ->
                            Es = Es0#{ExprKey=>Dst},
                            cse_is(Is, Es, Sub0, [I|Acc])
                    end
            end
    end;
cse_is([], Es, Sub, Acc) ->
    {Acc,Es,Sub}.

cse_expr(#b_set{op=Op,args=Args}=I) ->
    case cse_suitable(I) of
        true -> {ok,{Op,Args}};
        false -> none
    end.

cse_suitable(#b_set{op=get_hd}) -> true;
cse_suitable(#b_set{op=get_tl}) -> true;
cse_suitable(#b_set{op=put_list}) -> true;
cse_suitable(#b_set{op=get_tuple_element}) -> true;
cse_suitable(#b_set{op=put_tuple}) -> true;
cse_suitable(#b_set{op={bif,tuple_size}}) ->
    %% Doing CSE for tuple_size/1 can prevent the
    %% creation of test_arity and select_tuple_arity
    %% instructions. That could decrease performance
    %% and beam_validator could fail to understand
    %% that tuple operations that follow are safe.
    false;
cse_suitable(#b_set{anno=Anno,op={bif,Name},args=Args}) ->
    %% Doing CSE for floating point operators is unsafe.
    %% Doing CSE for comparison operators would prevent
    %% creation of 'test' instructions.
    Arity = length(Args),
    not (is_map_key(float_op, Anno) orelse
         erl_internal:new_type_test(Name, Arity) orelse
         erl_internal:comp_op(Name, Arity) orelse
         erl_internal:bool_op(Name, Arity));
cse_suitable(#b_set{}) -> false.

%%%
%%% Using floating point instructions.
%%%
%%% Use the special floating points version of arithmetic
%%% instructions, if the operands are known to be floats or the result
%%% of the operation will be a float.
%%%
%%% The float instructions were never used in guards before, so we
%%% will take special care to keep not using them in guards.  Using
%%% them in guards would require a new version of the 'fconv'
%%% instruction that would take a failure label.  Since it is unlikely
%%% that using float instructions in guards would be benefical, why
%%% bother implementing a new instruction?  Also, implementing float
%%% instructions in guards in HiPE could turn out to be a lot of work.
%%%

-record(fs,
        {s=undefined :: 'undefined' | 'cleared',
         regs=#{} :: #{beam_ssa:b_var():=beam_ssa:b_var()},
         vars=cerl_sets:new() :: cerl_sets:set(),
         fail=none :: 'none' | beam_ssa:label(),
         non_guards :: gb_sets:set(beam_ssa:label()),
         bs :: beam_ssa:block_map()
        }).

ssa_opt_float({#opt_st{ssa=Linear0,cnt=Count0}=St, FuncDb}) ->
    NonGuards = non_guards(Linear0),
    Blocks = maps:from_list(Linear0),
    Fs = #fs{non_guards=NonGuards,bs=Blocks},
    {Linear,Count} = float_opt(Linear0, Count0, Fs),
    {St#opt_st{ssa=Linear,cnt=Count}, FuncDb}.

%% The fconv instruction doesn't support jumping to a fail label, so we have to
%% skip this optimization if the fail block is a guard.
%%
%% We also skip the optimization in blocks that always fail, as it's both
%% difficult and pointless to rewrite them to use float ops.
float_can_optimize_blk(#b_blk{last=#b_br{bool=#b_var{},fail=F}},
                       #fs{non_guards=NonGuards}) ->
    gb_sets:is_member(F, NonGuards);
float_can_optimize_blk(#b_blk{}, #fs{}) ->
    false.

float_opt([{L,#b_blk{is=[#b_set{op=exception_trampoline,args=[Var]}]}=Blk0} |
           Bs0], Count0, Fs) ->
    %% If we've replaced a BIF with float operations, we'll have a lot of extra
    %% blocks that jump to the same failure block, which may have a trampoline
    %% that refers to the original operation.
    %%
    %% Since the point of the trampoline is to keep the BIF from being removed
    %% by liveness optimization, we can discard it as the liveness pass leaves
    %% floats alone.
    Blk = case cerl_sets:is_element(Var, Fs#fs.vars) of
              true -> Blk0#b_blk{is=[]};
              false -> Blk0
          end,
    {Bs, Count} = float_opt(Bs0, Count0, Fs),
    {[{L,Blk}|Bs],Count};
float_opt([{L,Blk}|Bs0], Count0, Fs) ->
    case float_can_optimize_blk(Blk, Fs) of
        true ->
            float_opt_1(L, Blk, Bs0, Count0, Fs);
        false ->
            {Bs,Count} = float_opt(Bs0, Count0, Fs),
            {[{L,Blk}|Bs],Count}
    end;
float_opt([], Count, _Fs) ->
    {[],Count}.

float_opt_1(L, #b_blk{is=Is0}=Blk0, Bs0, Count0, Fs0) ->
    case float_opt_is(Is0, Fs0, Count0, []) of
        {Is1,Fs1,Count1} ->
            Fs2 = float_fail_label(Blk0, Fs1),
            Fail = Fs2#fs.fail,
            {Flush,Blk,Fs,Count2} = float_maybe_flush(Blk0, Fs2, Count1),
            Split = float_split_conv(Is1, Blk),
            {Blks0,Count3} = float_number(Split, L, Count2),
            {Blks,Count4} = float_conv(Blks0, Fail, Count3),
            {Bs,Count} = float_opt(Bs0, Count4, Fs),
            {Blks++Flush++Bs,Count};
        none ->
            {Bs,Count} = float_opt(Bs0, Count0, Fs0),
            {[{L,Blk0}|Bs],Count}
    end.

%% Split {float,convert} instructions into individual blocks.
float_split_conv(Is0, Blk) ->
    Br = #b_br{bool=#b_literal{val=true},succ=0,fail=0},
    case splitwith(fun(#b_set{op=Op}) ->
                           Op =/= {float,convert}
                   end, Is0) of
        {Is,[]} ->
            [Blk#b_blk{is=Is}];
        {[_|_]=Is1,[#b_set{op={float,convert}}=Conv|Is2]} ->
            [#b_blk{is=Is1,last=Br},
             #b_blk{is=[Conv],last=Br}|float_split_conv(Is2, Blk)];
        {[],[#b_set{op={float,convert}}=Conv|Is1]} ->
            [#b_blk{is=[Conv],last=Br}|float_split_conv(Is1, Blk)]
    end.

%% Number the blocks that were split.
float_number([B|Bs0], FirstL, Count0) ->
    {Bs,Count} = float_number(Bs0, Count0),
    {[{FirstL,B}|Bs],Count}.

float_number([B|Bs0], Count0) ->
    {Bs,Count} = float_number(Bs0, Count0+1),
    {[{Count0,B}|Bs],Count};
float_number([], Count) ->
    {[],Count}.

%% Insert 'succeeded' instructions after each {float,convert}
%% instruction.
float_conv([{L,#b_blk{is=Is0}=Blk0}|Bs0], Fail, Count0) ->
    case Is0 of
        [#b_set{op={float,convert}}=Conv] ->
            {Bool0,Count1} = new_reg('@ssa_bool', Count0),
            Bool = #b_var{name=Bool0},
            Succeeded = #b_set{op=succeeded,dst=Bool,
                               args=[Conv#b_set.dst]},
            Is = [Conv,Succeeded],
            [{NextL,_}|_] = Bs0,
            Br = #b_br{bool=Bool,succ=NextL,fail=Fail},
            Blk = Blk0#b_blk{is=Is,last=Br},
            {Bs,Count} = float_conv(Bs0, Fail, Count1),
            {[{L,Blk}|Bs],Count};
        [_|_] ->
            case Bs0 of
                [{NextL,_}|_] ->
                    Br = #b_br{bool=#b_literal{val=true},
                               succ=NextL,fail=NextL},
                    Blk = Blk0#b_blk{last=Br},
                    {Bs,Count} = float_conv(Bs0, Fail, Count0),
                    {[{L,Blk}|Bs],Count};
                [] ->
                    {[{L,Blk0}],Count0}
            end
    end.

float_maybe_flush(Blk0, #fs{s=cleared,fail=Fail,bs=Blocks}=Fs0, Count0) ->
    #b_blk{last=#b_br{bool=#b_var{},succ=Succ}=Br} = Blk0,

    %% If the success block starts with a floating point operation, we can
    %% defer flushing to that block as long as it's suitable for optimization.
    #b_blk{is=Is} = SuccBlk = map_get(Succ, Blocks),
    CanOptimizeSucc = float_can_optimize_blk(SuccBlk, Fs0),

    case Is of
        [#b_set{anno=#{float_op:=_}}|_] when CanOptimizeSucc ->
            %% No flush needed.
            {[],Blk0,Fs0,Count0};
        _ ->
            %% Flush needed.
            {Bool0,Count1} = new_reg('@ssa_bool', Count0),
            Bool = #b_var{name=Bool0},

            %% Allocate block numbers.
            CheckL = Count1,              %For checkerror.
            FlushL = Count1 + 1,          %For flushing of float regs.
            Count = Count1 + 2,
            Blk = Blk0#b_blk{last=Br#b_br{succ=CheckL}},

            %% Build the block with the checkerror instruction.
            CheckIs = [#b_set{op={float,checkerror},dst=Bool}],
            CheckBr = #b_br{bool=Bool,succ=FlushL,fail=Fail},
            CheckBlk = #b_blk{is=CheckIs,last=CheckBr},

            %% Build the block that flushes all registers.
            FlushIs = float_flush_regs(Fs0),
            FlushBr = #b_br{bool=#b_literal{val=true},succ=Succ,fail=Succ},
            FlushBlk = #b_blk{is=FlushIs,last=FlushBr},

            %% Update state and blocks.
            Fs = Fs0#fs{s=undefined,regs=#{},fail=none},
            FlushBs = [{CheckL,CheckBlk},{FlushL,FlushBlk}],
            {FlushBs,Blk,Fs,Count}
    end;
float_maybe_flush(Blk, Fs, Count) ->
    {[],Blk,Fs,Count}.

float_opt_is([#b_set{op=succeeded,args=[Src]}=I0],
             #fs{regs=Rs}=Fs, Count, Acc) ->
    case Rs of
        #{Src:=Fr} ->
            I = I0#b_set{args=[Fr]},
            {reverse(Acc, [I]),Fs,Count};
        #{} ->
            {reverse(Acc, [I0]),Fs,Count}
    end;
float_opt_is([#b_set{anno=Anno0}=I0|Is0], Fs0, Count0, Acc) ->
    case Anno0 of
        #{float_op:=FTypes} ->
            Anno = maps:remove(float_op, Anno0),
            I1 = I0#b_set{anno=Anno},
            {Is,Fs,Count} = float_make_op(I1, FTypes, Fs0, Count0),
            float_opt_is(Is0, Fs, Count, reverse(Is, Acc));
        #{} ->
            float_opt_is(Is0, Fs0#fs{regs=#{}}, Count0, [I0|Acc])
    end;
float_opt_is([], Fs, _Count, _Acc) ->
    #fs{s=undefined} = Fs,                      %Assertion.
    none.

float_make_op(#b_set{op={bif,Op},dst=Dst,args=As0}=I0,
              Ts, #fs{s=S,regs=Rs0,vars=Vs0}=Fs, Count0) ->
    {As1,Rs1,Count1} = float_load(As0, Ts, Rs0, Count0, []),
    {As,Is0} = unzip(As1),
    {Fr,Count2} = new_reg('@fr', Count1),
    FrDst = #b_var{name=Fr},
    I = I0#b_set{op={float,Op},dst=FrDst,args=As},
    Vs = cerl_sets:add_element(Dst, Vs0),
    Rs = Rs1#{Dst=>FrDst},
    Is = append(Is0) ++ [I],
    case S of
        undefined ->
            {Ignore,Count} = new_reg('@ssa_ignore', Count2),
            C = #b_set{op={float,clearerror},dst=#b_var{name=Ignore}},
            {[C|Is],Fs#fs{s=cleared,regs=Rs,vars=Vs},Count};
        cleared ->
            {Is,Fs#fs{regs=Rs,vars=Vs},Count2}
    end.

float_load([A|As], [T|Ts], Rs0, Count0, Acc) ->
    {Load,Rs,Count} = float_reg_arg(A, T, Rs0, Count0),
    float_load(As, Ts, Rs, Count, [Load|Acc]);
float_load([], [], Rs, Count, Acc) ->
    {reverse(Acc),Rs,Count}.

float_reg_arg(A, T, Rs, Count0) ->
    case Rs of
        #{A:=Fr} ->
            {{Fr,[]},Rs,Count0};
        #{} ->
            {Fr,Count} = new_float_copy_reg(Count0),
            Dst = #b_var{name=Fr},
            I = float_load_reg(T, A, Dst),
            {{Dst,[I]},Rs#{A=>Dst},Count}
    end.

float_load_reg(convert, #b_var{}=Src, Dst) ->
    #b_set{op={float,convert},dst=Dst,args=[Src]};
float_load_reg(convert, #b_literal{val=Val}=Src, Dst) ->
    try float(Val) of
        F ->
            #b_set{op={float,put},dst=Dst,args=[#b_literal{val=F}]}
    catch
        error:_ ->
            %% Let the exception happen at runtime.
            #b_set{op={float,convert},dst=Dst,args=[Src]}
    end;
float_load_reg(float, Src, Dst) ->
    #b_set{op={float,put},dst=Dst,args=[Src]}.

new_float_copy_reg(Count) ->
    new_reg('@fr_copy', Count).

new_reg(Base, Count) ->
    Fr = {Base,Count},
    {Fr,Count+1}.

float_fail_label(#b_blk{last=Last}, Fs) ->
    case Last of
        #b_br{bool=#b_var{},fail=Fail} ->
            Fs#fs{fail=Fail};
        _ ->
            Fs
    end.

float_flush_regs(#fs{regs=Rs}) ->
    maps:fold(fun(_, #b_var{name={'@fr_copy',_}}, Acc) ->
                      Acc;
                 (Dst, Fr, Acc) ->
                      [#b_set{op={float,get},dst=Dst,args=[Fr]}|Acc]
              end, [], Rs).

%%%
%%% Live optimization.
%%%
%%% Optimize instructions whose values are not used. They could be
%%% removed if they have no side effects, or in a few cases replaced
%%% with a cheaper instructions
%%%

ssa_opt_live({#opt_st{ssa=Linear0}=St, FuncDb}) ->
    RevLinear = reverse(Linear0),
    Blocks0 = maps:from_list(RevLinear),
    Blocks = live_opt(RevLinear, #{}, Blocks0),
    Linear = beam_ssa:linearize(Blocks),
    {St#opt_st{ssa=Linear}, FuncDb}.

live_opt([{L,Blk0}|Bs], LiveMap0, Blocks) ->
    Blk1 = beam_ssa_share:block(Blk0, Blocks),
    Successors = beam_ssa:successors(Blk1),
    Live0 = live_opt_succ(Successors, L, LiveMap0, gb_sets:empty()),
    {Blk,Live} = live_opt_blk(Blk1, Live0),
    LiveMap = live_opt_phis(Blk#b_blk.is, L, Live, LiveMap0),
    live_opt(Bs, LiveMap, Blocks#{L:=Blk});
live_opt([], _, Acc) -> Acc.

live_opt_succ([S|Ss], L, LiveMap, Live0) ->
    Key = {S,L},
    case LiveMap of
        #{Key:=Live} ->
            %% The successor has a phi node, and the value for
            %% this block in the phi node is a variable.
            live_opt_succ(Ss, L, LiveMap, gb_sets:union(Live, Live0));
        #{S:=Live} ->
            %% No phi node in the successor, or the value for
            %% this block in the phi node is a literal.
            live_opt_succ(Ss, L, LiveMap, gb_sets:union(Live, Live0));
        #{} ->
            %% A peek_message block which has not been processed yet.
            live_opt_succ(Ss, L, LiveMap, Live0)
    end;
live_opt_succ([], _, _, Acc) -> Acc.

live_opt_phis(Is, L, Live0, LiveMap0) ->
    LiveMap = LiveMap0#{L=>Live0},
    Phis = takewhile(fun(#b_set{op=Op}) -> Op =:= phi end, Is),
    case Phis of
        [] ->
            LiveMap;
        [_|_] ->
            PhiArgs = append([Args || #b_set{args=Args} <- Phis]),
            case [{P,V} || {#b_var{}=V,P} <- PhiArgs] of
                [_|_]=PhiVars ->
                    PhiLive0 = rel2fam(PhiVars),
                    PhiLive = [{{L,P},gb_sets:union(gb_sets:from_list(Vs), Live0)} ||
                                  {P,Vs} <- PhiLive0],
                    maps:merge(LiveMap, maps:from_list(PhiLive));
                [] ->
                    %% There were only literals in the phi node(s).
                    LiveMap
            end
    end.

live_opt_blk(#b_blk{is=Is0,last=Last}=Blk, Live0) ->
    Live1 = gb_sets:union(Live0, gb_sets:from_ordset(beam_ssa:used(Last))),
    {Is,Live} = live_opt_is(reverse(Is0), Live1, []),
    {Blk#b_blk{is=Is},Live}.

live_opt_is([#b_set{op=phi,dst=Dst}=I|Is], Live, Acc) ->
    case gb_sets:is_member(Dst, Live) of
        true ->
            live_opt_is(Is, Live, [I|Acc]);
        false ->
            live_opt_is(Is, Live, Acc)
    end;
live_opt_is([#b_set{op=succeeded,dst=SuccDst,args=[MapDst]}=SuccI,
             #b_set{op=get_map_element,dst=MapDst}=MapI | Is],
            Live0, Acc) ->
    case {gb_sets:is_member(SuccDst, Live0),
          gb_sets:is_member(MapDst, Live0)} of
        {true, true} ->
            Live = gb_sets:delete(SuccDst, Live0),
            live_opt_is([MapI | Is], Live, [SuccI | Acc]);
        {true, false} ->
            %% 'get_map_element' is unused; replace 'succeeded' with
            %% 'has_map_field'
            NewI = MapI#b_set{op=has_map_field,dst=SuccDst},
            live_opt_is([NewI | Is], Live0, Acc);
        {false, true} ->
            %% 'succeeded' is unused (we know it will succeed); discard it and
            %% keep 'get_map_element'
            live_opt_is([MapI | Is], Live0, Acc);
        {false, false} ->
            live_opt_is(Is, Live0, Acc)
    end;
live_opt_is([#b_set{dst=Dst}=I|Is], Live0, Acc) ->
    case gb_sets:is_member(Dst, Live0) of
        true ->
            LiveUsed = gb_sets:from_ordset(beam_ssa:used(I)),
            Live1 = gb_sets:union(Live0, LiveUsed),
            Live = gb_sets:delete(Dst, Live1),
            live_opt_is(Is, Live, [I|Acc]);
        false ->
            case beam_ssa:no_side_effect(I) of
                true ->
                    live_opt_is(Is, Live0, Acc);
                false ->
                    LiveUsed = gb_sets:from_ordset(beam_ssa:used(I)),
                    Live = gb_sets:union(Live0, LiveUsed),
                    live_opt_is(Is, Live, [I|Acc])
            end
    end;
live_opt_is([], Live, Acc) ->
    {Acc,Live}.

%%%
%%% Do a strength reduction of try/catch and catch.
%%%
%%% In try/catch constructs where the expression is restricted
%%% (essentially a guard expression) and the error reason is ignored
%%% in the catch part, such as:
%%%
%%%   try
%%%      <RestrictedExpression>
%%%   catch
%%%      _:_ ->
%%%        ...
%%%   end
%%%
%%% the try/catch can be eliminated by simply removing the `new_try_tag`,
%%% `landingpad`, and `kill_try_tag` instructions.

ssa_opt_try({#opt_st{ssa=Linear0}=St, FuncDb}) ->
    Linear = opt_try(Linear0),
    {St#opt_st{ssa=Linear}, FuncDb}.

opt_try([{L,#b_blk{is=[#b_set{op=new_try_tag}],
                      last=Last}=Blk0}|Bs0]) ->
    #b_br{succ=Succ,fail=Fail} = Last,
    Ws = cerl_sets:from_list([Succ,Fail]),
    try do_opt_try(Bs0, Ws) of
        Bs ->
            Blk = Blk0#b_blk{is=[],
                             last=#b_br{bool=#b_literal{val=true},
                                        succ=Succ,fail=Succ}},
            [{L,Blk}|opt_try(Bs)]
    catch
        throw:not_possible ->
            [{L,Blk0}|opt_try(Bs0)]
    end;
opt_try([{L,Blk}|Bs]) ->
    [{L,Blk}|opt_try(Bs)];
opt_try([]) -> [].

do_opt_try([{L,Blk}|Bs]=Bs0, Ws0) ->
    case cerl_sets:is_element(L, Ws0) of
        false ->
            %% This block is not reachable from the block with the
            %% `new_try_tag` instruction. Retain it. There is no
            %% need to check it for safety.
            case cerl_sets:size(Ws0) of
                0 -> Bs0;
                _ -> [{L,Blk}|do_opt_try(Bs, Ws0)]
            end;
        true ->
            Ws1 = cerl_sets:del_element(L, Ws0),
            #b_blk{is=Is0} = Blk,
            case is_safe_without_try(Is0, []) of
                safe ->
                    %% This block does not execute any instructions
                    %% that would require a try. Analyze successors.
                    Successors = beam_ssa:successors(Blk),
                    Ws = cerl_sets:union(cerl_sets:from_list(Successors),
                                         Ws1),
                    [{L,Blk}|do_opt_try(Bs, Ws)];
                unsafe ->
                    %% There is something unsafe in the block, for
                    %% example a `call` instruction or an `extract`
                    %% instruction.
                    throw(not_possible);
                {done,Is} ->
                    %% This block kills the try tag (either after successful
                    %% execution or at the landing pad). Don't analyze
                    %% successors.
                    [{L,Blk#b_blk{is=Is}}|do_opt_try(Bs, Ws1)]
            end
    end;
do_opt_try([], Ws) ->
    0 = cerl_sets:size(Ws),                     %Assertion.
    [].

is_safe_without_try([#b_set{op=kill_try_tag}|Is], Acc) ->
    %% Remove this kill_try_tag instruction. If there was a landingpad
    %% instruction in this block, it has already been removed. Preserve
    %% all other instructions in the block.
    {done,reverse(Is, Acc)};
is_safe_without_try([#b_set{op=extract}|_], _Acc) ->
    %% The error reason is accessed.
    unsafe;
is_safe_without_try([#b_set{op=exception_trampoline}|Is], Acc) ->
    is_safe_without_try(Is, Acc);
is_safe_without_try([#b_set{op=landingpad}|Is], Acc) ->
    is_safe_without_try(Is, Acc);
is_safe_without_try([#b_set{op=Op}=I|Is], Acc) ->
    IsSafe = case Op of
                 phi -> true;
                 _ -> beam_ssa:no_side_effect(I)
             end,
    case IsSafe of
        true -> is_safe_without_try(Is, [I|Acc]);
        false -> unsafe
    end;
is_safe_without_try([], _Acc) -> safe.

%%%
%%% Optimize binary matching.
%%%
%%% * If the value of segment is never extracted, rewrite
%%%   to a bs_skip instruction.
%%%
%%% * Coalesce adjacent bs_skip instructions and skip instructions
%%%   with bs_test_tail.
%%%

ssa_opt_bsm({#opt_st{ssa=Linear}=St, FuncDb}) ->
    Extracted0 = bsm_extracted(Linear),
    Extracted = cerl_sets:from_list(Extracted0),
    {St#opt_st{ssa=bsm_skip(Linear, Extracted)}, FuncDb}.

bsm_skip([{L,#b_blk{is=Is0}=Blk}|Bs0], Extracted) ->
    Bs = bsm_skip(Bs0, Extracted),
    Is = bsm_skip_is(Is0, Extracted),
    coalesce_skips({L,Blk#b_blk{is=Is}}, Bs);
bsm_skip([], _) -> [].

bsm_skip_is([I0|Is], Extracted) ->
    case I0 of
        #b_set{op=bs_match,
               dst=Ctx,
               args=[#b_literal{val=T}=Type,PrevCtx|Args0]}
          when T =/= string, T =/= skip ->
            I = case cerl_sets:is_element(Ctx, Extracted) of
                    true ->
                        I0;
                    false ->
                        %% The value is never extracted.
                        Args = [#b_literal{val=skip},PrevCtx,Type|Args0],
                        I0#b_set{args=Args}
                end,
            [I|Is];
        #b_set{} ->
            [I0|bsm_skip_is(Is, Extracted)]
    end;
bsm_skip_is([], _) -> [].

bsm_extracted([{_,#b_blk{is=Is}}|Bs]) ->
    case Is of
        [#b_set{op=bs_extract,args=[Ctx]}|_] ->
            [Ctx|bsm_extracted(Bs)];
        _ ->
            bsm_extracted(Bs)
    end;
bsm_extracted([]) -> [].

coalesce_skips({L,#b_blk{is=[#b_set{op=bs_extract}=Extract|Is0],
                         last=Last0}=Blk0}, Bs0) ->
    case coalesce_skips_is(Is0, Last0, Bs0) of
        not_possible ->
            [{L,Blk0}|Bs0];
        {Is,Last,Bs} ->
            Blk = Blk0#b_blk{is=[Extract|Is],last=Last},
            [{L,Blk}|Bs]
    end;
coalesce_skips({L,#b_blk{is=Is0,last=Last0}=Blk0}, Bs0) ->
    case coalesce_skips_is(Is0, Last0, Bs0) of
        not_possible ->
            [{L,Blk0}|Bs0];
        {Is,Last,Bs} ->
            Blk = Blk0#b_blk{is=Is,last=Last},
            [{L,Blk}|Bs]
    end.

coalesce_skips_is([#b_set{op=bs_match,
                          args=[#b_literal{val=skip},
                                Ctx0,Type,Flags,
                                #b_literal{val=Size0},
                                #b_literal{val=Unit0}]}=Skip0,
                   #b_set{op=succeeded}],
                  #b_br{succ=L2,fail=Fail}=Br0,
                  Bs0) when is_integer(Size0) ->
    case Bs0 of
        [{L2,#b_blk{is=[#b_set{op=bs_match,
                               dst=SkipDst,
                               args=[#b_literal{val=skip},_,_,_,
                                     #b_literal{val=Size1},
                                     #b_literal{val=Unit1}]},
                        #b_set{op=succeeded}=Succeeded],
                    last=#b_br{fail=Fail}=Br}}|Bs] when is_integer(Size1) ->
            SkipBits = Size0 * Unit0 + Size1 * Unit1,
            Skip = Skip0#b_set{dst=SkipDst,
                               args=[#b_literal{val=skip},Ctx0,
                                     Type,Flags,
                                     #b_literal{val=SkipBits},
                                     #b_literal{val=1}]},
            Is = [Skip,Succeeded],
            {Is,Br,Bs};
        [{L2,#b_blk{is=[#b_set{op=bs_test_tail,
                               args=[_Ctx,#b_literal{val=TailSkip}]}],
                    last=#b_br{succ=NextSucc,fail=Fail}}}|Bs] ->
            SkipBits = Size0 * Unit0,
            TestTail = Skip0#b_set{op=bs_test_tail,
                                   args=[Ctx0,#b_literal{val=SkipBits+TailSkip}]},
            Br = Br0#b_br{bool=TestTail#b_set.dst,succ=NextSucc},
            Is = [TestTail],
            {Is,Br,Bs};
        _ ->
            not_possible
    end;
coalesce_skips_is(_, _, _) ->
    not_possible.

%%%
%%% Short-cutting binary matching instructions.
%%%

ssa_opt_bsm_shortcut({#opt_st{ssa=Linear}=St, FuncDb}) ->
    Positions = bsm_positions(Linear, #{}),
    case map_size(Positions) of
        0 ->
            %% No binary matching instructions.
            {St, FuncDb};
        _ ->
            {St#opt_st{ssa=bsm_shortcut(Linear, Positions)}, FuncDb}
    end.

bsm_positions([{L,#b_blk{is=Is,last=Last}}|Bs], PosMap0) ->
    PosMap = bsm_positions_is(Is, PosMap0),
    case {Is,Last} of
        {[#b_set{op=bs_test_tail,dst=Bool,args=[Ctx,#b_literal{val=Bits0}]}],
         #b_br{bool=Bool,fail=Fail}} ->
            Bits = Bits0 + map_get(Ctx, PosMap0),
            bsm_positions(Bs, PosMap#{L=>{Bits,Fail}});
        {_,_} ->
            bsm_positions(Bs, PosMap)
    end;
bsm_positions([], PosMap) -> PosMap.

bsm_positions_is([#b_set{op=bs_start_match,dst=New}|Is], PosMap0) ->
    PosMap = PosMap0#{New=>0},
    bsm_positions_is(Is, PosMap);
bsm_positions_is([#b_set{op=bs_match,dst=New,args=Args}|Is], PosMap0) ->
    [_,Old|_] = Args,
    #{Old:=Bits0} = PosMap0,
    Bits = bsm_update_bits(Args, Bits0),
    PosMap = PosMap0#{New=>Bits},
    bsm_positions_is(Is, PosMap);
bsm_positions_is([_|Is], PosMap) ->
    bsm_positions_is(Is, PosMap);
bsm_positions_is([], PosMap) -> PosMap.

bsm_update_bits([#b_literal{val=string},_,#b_literal{val=String}], Bits) ->
    Bits + bit_size(String);
bsm_update_bits([#b_literal{val=utf8}|_], Bits) ->
    Bits + 8;
bsm_update_bits([#b_literal{val=utf16}|_], Bits) ->
    Bits + 16;
bsm_update_bits([#b_literal{val=utf32}|_], Bits) ->
    Bits + 32;
bsm_update_bits([_,_,_,#b_literal{val=Sz},#b_literal{val=U}], Bits)
  when is_integer(Sz) ->
    Bits + Sz*U;
bsm_update_bits(_, Bits) -> Bits.

bsm_shortcut([{L,#b_blk{is=Is,last=Last0}=Blk}|Bs], PosMap) ->
    case {Is,Last0} of
        {[#b_set{op=bs_match,dst=New,args=[_,Old|_]},
          #b_set{op=succeeded,dst=Bool,args=[New]}],
         #b_br{bool=Bool,fail=Fail}} ->
            case PosMap of
                #{Old:=Bits,Fail:={TailBits,NextFail}} when Bits > TailBits ->
                    Last = Last0#b_br{fail=NextFail},
                    [{L,Blk#b_blk{last=Last}}|bsm_shortcut(Bs, PosMap)];
                #{} ->
                    [{L,Blk}|bsm_shortcut(Bs, PosMap)]
            end;
        {_,_} ->
            [{L,Blk}|bsm_shortcut(Bs, PosMap)]
    end;
bsm_shortcut([], _PosMap) -> [].

%%%
%%% Optimize binary construction.
%%%
%%% If an integer segment or a float segment has a literal size and
%%% a literal value, convert to a binary segment. Coalesce adjacent
%%% literal binary segments. Literal binary segments will be converted
%%% to bs_put_string instructions in later pass.
%%%

ssa_opt_bs_puts({#opt_st{ssa=Linear0,cnt=Count0}=St, FuncDb}) ->
    {Linear,Count} = opt_bs_puts(Linear0, Count0, []),
    {St#opt_st{ssa=Linear,cnt=Count}, FuncDb}.

opt_bs_puts([{L,#b_blk{is=Is}=Blk0}|Bs], Count0, Acc0) ->
    case Is of
        [#b_set{op=bs_put}=I0] ->
            case opt_bs_put(L, I0, Blk0, Count0, Acc0) of
                not_possible ->
                    opt_bs_puts(Bs, Count0, [{L,Blk0}|Acc0]);
                {Count,Acc1} ->
                    Acc = opt_bs_puts_merge(Acc1),
                    opt_bs_puts(Bs, Count, Acc)
            end;
        _ ->
            opt_bs_puts(Bs, Count0, [{L,Blk0}|Acc0])
    end;
opt_bs_puts([], Count, Acc) ->
    {reverse(Acc),Count}.

opt_bs_puts_merge([{L1,#b_blk{is=Is}=Blk0},{L2,#b_blk{is=AccIs}}=BAcc|Acc]) ->
    case {AccIs,Is} of
        {[#b_set{op=bs_put,
                 args=[#b_literal{val=binary},
                       #b_literal{},
                       #b_literal{val=Bin0},
                       #b_literal{val=all},
                       #b_literal{val=1}]}],
         [#b_set{op=bs_put,
                 args=[#b_literal{val=binary},
                       #b_literal{},
                       #b_literal{val=Bin1},
                       #b_literal{val=all},
                       #b_literal{val=1}]}=I0]} ->
            %% Coalesce the two segments to one.
            Bin = <<Bin0/bitstring,Bin1/bitstring>>,
            I = I0#b_set{args=bs_put_args(binary, Bin, all)},
            Blk = Blk0#b_blk{is=[I]},
            [{L2,Blk}|Acc];
        {_,_} ->
            [{L1,Blk0},BAcc|Acc]
    end.

opt_bs_put(L, I0, #b_blk{last=Br0}=Blk0, Count0, Acc) ->
    case opt_bs_put(I0) of
        [Bin] when is_bitstring(Bin) ->
            Args = bs_put_args(binary, Bin, all),
            I = I0#b_set{args=Args},
            Blk = Blk0#b_blk{is=[I]},
            {Count0,[{L,Blk}|Acc]};
        [{int,Int,Size},Bin] when is_bitstring(Bin) ->
            %% Construct a bs_put_integer instruction following
            %% by a bs_put_binary instruction.
            IntArgs = bs_put_args(integer, Int, Size),
            BinArgs = bs_put_args(binary, Bin, all),
            {BinL,BinVarNum} = {Count0,Count0+1},
            Count = Count0 + 2,
            BinVar = #b_var{name={'@ssa_bool',BinVarNum}},
            BinI = I0#b_set{dst=BinVar,args=BinArgs},
            BinBlk = Blk0#b_blk{is=[BinI],last=Br0#b_br{bool=BinVar}},
            IntI = I0#b_set{args=IntArgs},
            IntBlk = Blk0#b_blk{is=[IntI],last=Br0#b_br{succ=BinL}},
            {Count,[{BinL,BinBlk},{L,IntBlk}|Acc]};
        not_possible ->
            not_possible
    end.

opt_bs_put(#b_set{args=[#b_literal{val=binary},_,#b_literal{val=Val},
                        #b_literal{val=all},#b_literal{val=Unit}]})
  when is_bitstring(Val) ->
    if
        bit_size(Val) rem Unit =:= 0 ->
            [Val];
        true ->
            not_possible
    end;
opt_bs_put(#b_set{args=[#b_literal{val=Type},#b_literal{val=Flags},
                        #b_literal{val=Val},#b_literal{val=Size},
                        #b_literal{val=Unit}]}=I0) when is_integer(Size) ->
    EffectiveSize = Size * Unit,
    if
        EffectiveSize > 0 ->
            case {Type,opt_bs_put_endian(Flags)} of
                {integer,big} when is_integer(Val) ->
                    if
                        EffectiveSize < 64 ->
                            [<<Val:EffectiveSize>>];
                        true ->
                            opt_bs_put_split_int(Val, EffectiveSize)
                    end;
                {integer,little} when is_integer(Val), EffectiveSize < 128 ->
                    %% To avoid an explosion in code size, we only try
                    %% to optimize relatively small fields.
                    <<Int:EffectiveSize>> = <<Val:EffectiveSize/little>>,
                    Args = bs_put_args(Type, Int, EffectiveSize),
                    I = I0#b_set{args=Args},
                    opt_bs_put(I);
                {binary,_} when is_bitstring(Val) ->
                    <<Bitstring:EffectiveSize/bits,_/bits>> = Val,
                    [Bitstring];
                {float,Endian} ->
                    try
                        [opt_bs_put_float(Val, EffectiveSize, Endian)]
                    catch error:_ ->
                            not_possible
                    end;
                {_,_} ->
                    not_possible
            end;
        true ->
            not_possible
    end;
opt_bs_put(#b_set{}) -> not_possible.

opt_bs_put_float(N, Sz, Endian) ->
    case Endian of
        big -> <<N:Sz/big-float-unit:1>>;
        little -> <<N:Sz/little-float-unit:1>>
    end.

bs_put_args(Type, Val, Size) ->
    [#b_literal{val=Type},
     #b_literal{val=[unsigned,big]},
     #b_literal{val=Val},
     #b_literal{val=Size},
     #b_literal{val=1}].

opt_bs_put_endian([big=E|_]) -> E;
opt_bs_put_endian([little=E|_]) -> E;
opt_bs_put_endian([native=E|_]) -> E;
opt_bs_put_endian([_|Fs]) -> opt_bs_put_endian(Fs).

opt_bs_put_split_int(Int, Size) ->
    Pos = opt_bs_put_split_int_1(Int, 0, Size - 1),
    UpperSize = Size - Pos,
    if
        Pos =:= 0 ->
            %% Value is 0 or -1 -- keep the original instruction.
            not_possible;
        UpperSize < 64 ->
            %% No or few leading zeroes or ones.
            [<<Int:Size>>];
        true ->
            %% There are 64 or more leading ones or zeroes in
            %% the resulting binary. Split into two separate
            %% segments to avoid an explosion in code size.
            [{int,Int bsr Pos,UpperSize},<<Int:Pos>>]
    end.

opt_bs_put_split_int_1(_Int, L, R) when L > R ->
    8 * ((L + 7) div 8);
opt_bs_put_split_int_1(Int, L, R) ->
    Mid = (L + R) div 2,
    case Int bsr Mid of
        Upper when Upper =:= 0; Upper =:= -1 ->
            opt_bs_put_split_int_1(Int, L, Mid - 1);
        _ ->
            opt_bs_put_split_int_1(Int, Mid + 1, R)
    end.

%%%
%%% Optimize expressions such as "tuple_size(Var) =:= 2".
%%%
%%% Consider this code:
%%%
%%% 0:
%%%   .
%%%   .
%%%   .
%%%   Size = bif:tuple_size Var
%%%   BoolVar1 = succeeded Size
%%%   br BoolVar1, label 4, label 3
%%%
%%% 4:
%%%   BoolVar2 = bif:'=:=' Size, literal 2
%%%   br BoolVar2, label 6, label 3
%%%
%%% 6: ...   %% OK
%%%
%%% 3: ...   %% Not a tuple of size 2
%%%
%%% The BEAM code will look this:
%%%
%%%   {bif,tuple_size,{f,3},[{x,0}],{x,0}}.
%%%   {test,is_eq_exact,{f,3},[{x,0},{integer,2}]}.
%%%
%%% Better BEAM code will be produced if we transform the
%%% code like this:
%%%
%%% 0:
%%%   .
%%%   .
%%%   .
%%%   br label 10
%%%
%%% 10:
%%%   NewBoolVar = bif:is_tuple Var
%%%   br NewBoolVar, label 11, label 3
%%%
%%% 11:
%%%   Size = bif:tuple_size Var
%%%   br label 4
%%%
%%% 4:
%%%   BoolVar2 = bif:'=:=' Size, literal 2
%%%   br BoolVar2, label 6, label 3
%%%
%%% (The key part of the transformation is the removal of
%%% the 'succeeded' instruction to signal to the code generator
%%% that the call to tuple_size/1 can't fail.)
%%%
%%% The BEAM code will look like:
%%%
%%%   {test,is_tuple,{f,3},[{x,0}]}.
%%%   {test_arity,{f,3},[{x,0},2]}.
%%%
%%% Those two instructions will be combined into a single
%%% is_tuple_of_arity instruction by the loader.
%%%

ssa_opt_tuple_size({#opt_st{ssa=Linear0,cnt=Count0}=St, FuncDb}) ->
    %% This optimization is only safe in guards, as prefixing tuple_size with
    %% an is_tuple check prevents it from throwing an exception.
    NonGuards = non_guards(Linear0),
    {Linear,Count} = opt_tup_size(Linear0, NonGuards, Count0, []),
    {St#opt_st{ssa=Linear,cnt=Count}, FuncDb}.

opt_tup_size([{L,#b_blk{is=Is,last=Last}=Blk}|Bs], NonGuards, Count0, Acc0) ->
    case {Is,Last} of
        {[#b_set{op={bif,'=:='},dst=Bool,args=[#b_var{}=Tup,#b_literal{val=Arity}]}],
         #b_br{bool=Bool}} when is_integer(Arity), Arity >= 0 ->
            {Acc,Count} = opt_tup_size_1(Tup, L, NonGuards, Count0, Acc0),
            opt_tup_size(Bs, NonGuards, Count, [{L,Blk}|Acc]);
        {_,_} ->
            opt_tup_size(Bs, NonGuards, Count0, [{L,Blk}|Acc0])
    end;
opt_tup_size([], _NonGuards, Count, Acc) ->
    {reverse(Acc),Count}.

opt_tup_size_1(Size, EqL, NonGuards, Count0, [{L,Blk0}|Acc]) ->
    #b_blk{is=Is0,last=Last} = Blk0,
    case Last of
        #b_br{bool=Bool,succ=EqL,fail=Fail} ->
            case gb_sets:is_member(Fail, NonGuards) of
                true ->
                    {[{L,Blk0}|Acc],Count0};
                false ->
                    case opt_tup_size_is(Is0, Bool, Size, []) of
                        none ->
                            {[{L,Blk0}|Acc],Count0};
                        {PreIs,TupleSizeIs,Tuple} ->
                            opt_tup_size_2(PreIs, TupleSizeIs, L, EqL,
                                           Tuple, Fail, Count0, Acc)
                    end
            end;
        _ ->
            {[{L,Blk0}|Acc],Count0}
    end;
opt_tup_size_1(_, _, _, Count, Acc) ->
    {Acc,Count}.

opt_tup_size_2(PreIs, TupleSizeIs, PreL, EqL, Tuple, Fail, Count0, Acc) ->
    IsTupleL = Count0,
    TupleSizeL = Count0 + 1,
    Bool = #b_var{name={'@ssa_bool',Count0+2}},
    Count = Count0 + 3,

    True = #b_literal{val=true},
    PreBr = #b_br{bool=True,succ=IsTupleL,fail=IsTupleL},
    PreBlk = #b_blk{is=PreIs,last=PreBr},

    IsTupleIs = [#b_set{op={bif,is_tuple},dst=Bool,args=[Tuple]}],
    IsTupleBr = #b_br{bool=Bool,succ=TupleSizeL,fail=Fail},
    IsTupleBlk = #b_blk{is=IsTupleIs,last=IsTupleBr},

    TupleSizeBr = #b_br{bool=True,succ=EqL,fail=EqL},
    TupleSizeBlk = #b_blk{is=TupleSizeIs,last=TupleSizeBr},
    {[{TupleSizeL,TupleSizeBlk},
      {IsTupleL,IsTupleBlk},
      {PreL,PreBlk}|Acc],Count}.

opt_tup_size_is([#b_set{op={bif,tuple_size},dst=Size,args=[Tuple]}=I,
                 #b_set{op=succeeded,dst=Bool,args=[Size]}],
                Bool, Size, Acc) ->
    {reverse(Acc),[I],Tuple};
opt_tup_size_is([I|Is], Bool, Size, Acc) ->
    opt_tup_size_is(Is, Bool, Size, [I|Acc]);
opt_tup_size_is([], _, _, _Acc) -> none.

%%%
%%% Optimize #b_switch{} instructions.
%%%
%%% A #b_switch{} with only one value can be rewritten to
%%% a #b_br{}. A switch that only verifies that the argument
%%% is 'true' or 'false' can be rewritten to an is_boolean test.
%%%b

ssa_opt_sw({#opt_st{ssa=Linear0,cnt=Count0}=St, FuncDb}) ->
    {Linear,Count} = opt_sw(Linear0, Count0, []),
    {St#opt_st{ssa=Linear,cnt=Count}, FuncDb}.

opt_sw([{L,#b_blk{is=Is,last=#b_switch{}=Sw0}=Blk0}|Bs], Count0, Acc) ->
    case Sw0 of
        #b_switch{arg=Arg,fail=Fail,list=[{Lit,Lbl}]} ->
            %% Rewrite a single value switch to a br.
            {Bool,Count} = new_var('@ssa_bool', Count0),
            IsEq = #b_set{op={bif,'=:='},dst=Bool,args=[Arg,Lit]},
            Br = #b_br{bool=Bool,succ=Lbl,fail=Fail},
            Blk = Blk0#b_blk{is=Is++[IsEq],last=Br},
            opt_sw(Bs, Count, [{L,Blk}|Acc]);
        #b_switch{arg=Arg,fail=Fail,
                  list=[{#b_literal{val=B1},Lbl},{#b_literal{val=B2},Lbl}]}
          when B1 =:= not B2 ->
            %% Replace with is_boolean test.
            {Bool,Count} = new_var('@ssa_bool', Count0),
            IsBool = #b_set{op={bif,is_boolean},dst=Bool,args=[Arg]},
            Br = #b_br{bool=Bool,succ=Lbl,fail=Fail},
            Blk = Blk0#b_blk{is=Is++[IsBool],last=Br},
            opt_sw(Bs, Count, [{L,Blk}|Acc]);
        _ ->
            opt_sw(Bs, Count0, [{L,Blk0}|Acc])
    end;
opt_sw([{L,#b_blk{}=Blk}|Bs], Count, Acc) ->
    opt_sw(Bs, Count, [{L,Blk}|Acc]);
opt_sw([], Count, Acc) ->
    {reverse(Acc),Count}.

%%%
%%% Merge blocks.
%%%

ssa_opt_merge_blocks({#opt_st{ssa=Blocks}=St, FuncDb}) ->
    Preds = beam_ssa:predecessors(Blocks),
    Merged = merge_blocks_1(beam_ssa:rpo(Blocks), Preds, Blocks),
    {St#opt_st{ssa=Merged}, FuncDb}.

merge_blocks_1([L|Ls], Preds0, Blocks0) ->
    case Preds0 of
        #{L:=[P]} ->
            #{P:=Blk0,L:=Blk1} = Blocks0,
            case is_merge_allowed(L, Blk0, Blk1) of
                true ->
                    #b_blk{is=Is0} = Blk0,
                    #b_blk{is=Is1} = Blk1,
                    verify_merge_is(Is1),
                    Is = Is0 ++ Is1,
                    Blk = Blk1#b_blk{is=Is},
                    Blocks1 = maps:remove(L, Blocks0),
                    Blocks2 = Blocks1#{P:=Blk},
                    Successors = beam_ssa:successors(Blk),
                    Blocks = beam_ssa:update_phi_labels(Successors, L, P, Blocks2),
                    Preds = merge_update_preds(Successors, L, P, Preds0),
                    merge_blocks_1(Ls, Preds, Blocks);
                false ->
                    merge_blocks_1(Ls, Preds0, Blocks0)
            end;
        #{} ->
            merge_blocks_1(Ls, Preds0, Blocks0)
    end;
merge_blocks_1([], _Preds, Blocks) -> Blocks.

merge_update_preds([L|Ls], From, To, Preds0) ->
    Ps = [rename_label(P, From, To) || P <- map_get(L, Preds0)],
    Preds = Preds0#{L:=Ps},
    merge_update_preds(Ls, From, To, Preds);
merge_update_preds([], _, _, Preds) -> Preds.

rename_label(From, From, To) -> To;
rename_label(Lbl, _, _) -> Lbl.

verify_merge_is([#b_set{op=Op}|_]) ->
    %% The merged block has only one predecessor, so it should not have any phi
    %% nodes.
    true = Op =/= phi;                          %Assertion.
verify_merge_is(_) ->
    ok.

is_merge_allowed(_, #b_blk{}, #b_blk{is=[#b_set{op=exception_trampoline}|_]}) ->
    false;
is_merge_allowed(_, #b_blk{is=[#b_set{op=exception_trampoline}|_]}, #b_blk{}) ->
    false;
is_merge_allowed(L, #b_blk{}=Blk1, #b_blk{is=[#b_set{}=I|_]}=Blk2) ->
    not beam_ssa:is_loop_header(I) andalso
        is_merge_allowed_1(L, Blk1, Blk2);
is_merge_allowed(L, Blk1, Blk2) ->
    is_merge_allowed_1(L, Blk1, Blk2).

is_merge_allowed_1(L, #b_blk{last=#b_br{}}=Blk, #b_blk{is=Is}) ->
    %% The predecessor block must have exactly one successor (L) for
    %% the merge to be safe.
    case beam_ssa:successors(Blk) of
        [L] ->
            case Is of
                [#b_set{op=phi,args=[_]}|_] ->
                    %% The type optimizer pass must have been
                    %% turned off, since it would have removed this
                    %% redundant phi node. Refuse to merge the blocks
                    %% to ensure that this phi node remains at the
                    %% beginning of a block.
                    false;
                _ ->
                    true
            end;
        [_|_] ->
            false
    end;
is_merge_allowed_1(_, #b_blk{last=#b_switch{}}, #b_blk{}) ->
    false.

%%%
%%% When a tuple is matched, the pattern matching compiler generates a
%%% get_tuple_element instruction for every tuple element that will
%%% ever be used in the rest of the function. That often forces the
%%% extracted tuple elements to be stored in Y registers until it's
%%% time to use them. It could also mean that there could be execution
%%% paths that will never use the extracted elements.
%%%
%%% This optimization will sink get_tuple_element instructions, that
%%% is, move them forward in the execution stream to the last possible
%%% block there they will still dominate all uses. That may reduce the
%%% size of stack frames, reduce register shuffling, and avoid
%%% extracting tuple elements on execution paths that never use the
%%% extracted values.
%%%

ssa_opt_sink({#opt_st{ssa=Linear}=St, FuncDb}) ->
    %% Create a map with all variables that define get_tuple_element
    %% instructions. The variable name map to the block it is defined in.
    case def_blocks(Linear) of
        [] ->
            %% No get_tuple_element instructions, so there is nothing to do.
            {St, FuncDb};
        [_|_]=Defs0 ->
            Defs = maps:from_list(Defs0),
            {do_ssa_opt_sink(Defs, St), FuncDb}
    end.

do_ssa_opt_sink(Defs, #opt_st{ssa=Linear}=St) ->
    Blocks0 = maps:from_list(Linear),

    %% Now find all the blocks that use variables defined by get_tuple_element
    %% instructions.
    Used = used_blocks(Linear, Defs, []),

    %% Calculate dominators.
    {Dom,Numbering} = beam_ssa:dominators(Blocks0),

    %% It is not safe to move get_tuple_element instructions to blocks
    %% that begin with certain instructions. It is also unsafe to move
    %% the instructions into any part of a receive. To avoid such
    %% unsafe moves, pretend that the unsuitable blocks are not
    %% dominators.
    Unsuitable = unsuitable(Linear, Blocks0),

    %% Calculate new positions for get_tuple_element instructions. The new
    %% position is a block that dominates all uses of the variable.
    DefLoc = new_def_locations(Used, Defs, Dom, Numbering, Unsuitable),

    %% Now move all suitable get_tuple_element instructions to their
    %% new blocks.
    Blocks = foldl(fun({V,To}, A) ->
                           From = map_get(V, Defs),
                           move_defs(V, From, To, A)
                   end, Blocks0, DefLoc),

    St#opt_st{ssa=beam_ssa:linearize(Blocks)}.

def_blocks([{L,#b_blk{is=Is}}|Bs]) ->
    def_blocks_is(Is, L, def_blocks(Bs));
def_blocks([]) -> [].

def_blocks_is([#b_set{op=get_tuple_element,dst=Dst}|Is], L, Acc) ->
    def_blocks_is(Is, L, [{Dst,L}|Acc]);
def_blocks_is([_|Is], L, Acc) ->
    def_blocks_is(Is, L, Acc);
def_blocks_is([], _, Acc) -> Acc.

used_blocks([{L,Blk}|Bs], Def, Acc0) ->
    Used = beam_ssa:used(Blk),
    Acc = [{V,L} || V <- Used, maps:is_key(V, Def)] ++ Acc0,
    used_blocks(Bs, Def, Acc);
used_blocks([], _Def, Acc) ->
    rel2fam(Acc).

%% unsuitable(Linear, Blocks) -> Unsuitable.
%%  Return an ordset of block labels for the blocks that are not
%%  suitable for sinking of get_tuple_element instructions.

unsuitable(Linear, Blocks) ->
    Predecessors = beam_ssa:predecessors(Blocks),
    Unsuitable0 = unsuitable_1(Linear),
    Unsuitable1 = unsuitable_recv(Linear, Blocks, Predecessors),
    gb_sets:from_list(Unsuitable0 ++ Unsuitable1).

unsuitable_1([{L,#b_blk{is=[#b_set{op=Op}=I|_]}}|Bs]) ->
    Unsuitable = case Op of
                     bs_extract -> true;
                     bs_put -> true;
                     exception_trampoline -> true;
                     {float,_} -> true;
                     landingpad -> true;
                     _ -> beam_ssa:is_loop_header(I)
                 end,
    case Unsuitable of
        true ->
            [L|unsuitable_1(Bs)];
        false ->
            unsuitable_1(Bs)
    end;
unsuitable_1([{_,#b_blk{}}|Bs]) ->
    unsuitable_1(Bs);
unsuitable_1([]) -> [].

unsuitable_recv([{L,#b_blk{is=[#b_set{op=Op}|_]}}|Bs], Blocks, Predecessors) ->
    Ls = case Op of
             remove_message ->
                 unsuitable_loop(L, Blocks, Predecessors);
             recv_next ->
                 unsuitable_loop(L, Blocks, Predecessors);
             _ ->
                 []
         end,
    Ls ++ unsuitable_recv(Bs, Blocks, Predecessors);
unsuitable_recv([_|Bs], Blocks, Predecessors) ->
    unsuitable_recv(Bs, Blocks, Predecessors);
unsuitable_recv([], _, _) -> [].

unsuitable_loop(L, Blocks, Predecessors) ->
    unsuitable_loop(L, Blocks, Predecessors, []).

unsuitable_loop(L, Blocks, Predecessors, Acc) ->
    Ps = map_get(L, Predecessors),
    unsuitable_loop_1(Ps, Blocks, Predecessors, Acc).

unsuitable_loop_1([P|Ps], Blocks, Predecessors, Acc0) ->
    case is_loop_header(P, Blocks) of
        true ->
            unsuitable_loop_1(Ps, Blocks, Predecessors, Acc0);
        false ->
            case ordsets:is_element(P, Acc0) of
                false ->
                    Acc1 = ordsets:add_element(P, Acc0),
                    Acc = unsuitable_loop(P, Blocks, Predecessors, Acc1),
                    unsuitable_loop_1(Ps, Blocks, Predecessors, Acc);
                true ->
                    unsuitable_loop_1(Ps, Blocks, Predecessors, Acc0)
            end
    end;
unsuitable_loop_1([], _, _, Acc) -> Acc.

is_loop_header(L, Blocks) ->
    case map_get(L, Blocks) of
        #b_blk{is=[I|_]} ->
            beam_ssa:is_loop_header(I);
        #b_blk{} ->
            false
    end.

%% new_def_locations([{Variable,[UsedInBlock]}|Vs], Defs,
%%                   Dominators, Numbering, Unsuitable) ->
%%  [{Variable,NewDefinitionBlock}]
%%
%%  Calculate new locations for get_tuple_element instructions. For
%%  each variable, the new location is a block that dominates all uses
%%  of the variable and as near to the uses of as possible.

new_def_locations([{V,UsedIn}|Vs], Defs, Dom, Numbering, Unsuitable) ->
    DefIn = map_get(V, Defs),
    Common = common_dominator(UsedIn, Dom, Numbering, Unsuitable),
    case member(Common, map_get(DefIn, Dom)) of
        true ->
            %% The common dominator is either DefIn or an
            %% ancestor of DefIn.
            new_def_locations(Vs, Defs, Dom, Numbering, Unsuitable);
        false ->
            %% We have found a suitable descendant of DefIn,
            %% to which the get_tuple_element instruction can
            %% be sunk.
            [{V,Common}|new_def_locations(Vs, Defs, Dom, Numbering, Unsuitable)]
    end;
new_def_locations([], _, _, _, _) -> [].

common_dominator(Ls0, Dom, Numbering, Unsuitable) ->
    [Common|_] = beam_ssa:common_dominators(Ls0, Dom, Numbering),
    case gb_sets:is_member(Common, Unsuitable) of
        true ->
            %% It is not allowed to place the instruction here. Try
            %% to find another suitable dominating block by going up
            %% one step in the dominator tree.
            [Common,OneUp|_] = map_get(Common, Dom),
            common_dominator([OneUp], Dom, Numbering, Unsuitable);
        false ->
            Common
    end.

%% Move get_tuple_element instructions to their new locations.

move_defs(V, From, To, Blocks) ->
    #{From:=FromBlk0,To:=ToBlk0} = Blocks,
    {Def,FromBlk} = remove_def(V, FromBlk0),
    try insert_def(V, Def, ToBlk0) of
        ToBlk ->
            %%io:format("~p: ~p => ~p\n", [V,From,To]),
            Blocks#{From:=FromBlk,To:=ToBlk}
    catch
        throw:not_possible ->
            Blocks
    end.

remove_def(V, #b_blk{is=Is0}=Blk) ->
    {Def,Is} = remove_def_is(Is0, V, []),
    {Def,Blk#b_blk{is=Is}}.

remove_def_is([#b_set{dst=Dst}=Def|Is], Dst, Acc) ->
    {Def,reverse(Acc, Is)};
remove_def_is([I|Is], Dst, Acc) ->
    remove_def_is(Is, Dst, [I|Acc]).

insert_def(V, Def, #b_blk{is=Is0}=Blk) ->
    Is = insert_def_is(Is0, V, Def),
    Blk#b_blk{is=Is}.

insert_def_is([#b_set{op=phi}=I|Is], V, Def) ->
    case member(V, beam_ssa:used(I)) of
        true ->
            throw(not_possible);
        false ->
            [I|insert_def_is(Is, V, Def)]
    end;
insert_def_is([#b_set{op=Op}=I|Is]=Is0, V, Def) ->
    Action0 = case Op of
                  call -> beyond;
                  'catch_end' -> beyond;
                  timeout -> beyond;
                  _ -> here
              end,
    Action = case Is of
                 [#b_set{op=succeeded}|_] -> here;
                 _ -> Action0
             end,
    case Action of
        beyond ->
            case member(V, beam_ssa:used(I)) of
                true ->
                    %% The variable is used by this instruction. We must
                    %% place the definition before this instruction.
                    [Def|Is0];
                false ->
                    %% Place it beyond the current instruction.
                    [I|insert_def_is(Is, V, Def)]
            end;
        here ->
            [Def|Is0]
    end;
insert_def_is([], _V, Def) ->
    [Def].

%%%
%%% Order consecutive get_tuple_element instructions in ascending
%%% position order. This will give the loader more opportunities
%%% for combining get_tuple_element instructions.
%%%

ssa_opt_get_tuple_element({#opt_st{ssa=Blocks0}=St, FuncDb}) ->
    Blocks = opt_get_tuple_element(maps:to_list(Blocks0), Blocks0),
    {St#opt_st{ssa=Blocks}, FuncDb}.

opt_get_tuple_element([{L,#b_blk{is=Is0}=Blk0}|Bs], Blocks) ->
    case opt_get_tuple_element_is(Is0, false, []) of
        {yes,Is} ->
            Blk = Blk0#b_blk{is=Is},
            opt_get_tuple_element(Bs, Blocks#{L:=Blk});
        no ->
            opt_get_tuple_element(Bs, Blocks)
    end;
opt_get_tuple_element([], Blocks) -> Blocks.

opt_get_tuple_element_is([#b_set{op=get_tuple_element,
                                 args=[#b_var{}=Src,_]}=I0|Is0],
                         _AnyChange, Acc) ->
    {GetIs0,Is} = collect_get_tuple_element(Is0, Src, [I0]),
    GetIs1 = sort([{Pos,I} || #b_set{args=[_,Pos]}=I <- GetIs0]),
    GetIs = [I || {_,I} <- GetIs1],
    opt_get_tuple_element_is(Is, true, reverse(GetIs, Acc));
opt_get_tuple_element_is([I|Is], AnyChange, Acc) ->
    opt_get_tuple_element_is(Is, AnyChange, [I|Acc]);
opt_get_tuple_element_is([], AnyChange, Acc) ->
    case AnyChange of
        true -> {yes,reverse(Acc)};
        false -> no
    end.

collect_get_tuple_element([#b_set{op=get_tuple_element,
                                  args=[Src,_]}=I|Is], Src, Acc) ->
    collect_get_tuple_element(Is, Src, [I|Acc]);
collect_get_tuple_element(Is, _Src, Acc) ->
    {Acc,Is}.

%%%
%%% Unfold literals to avoid unnecessary move instructions in call
%%% instructions.
%%%
%%% Consider the following example:
%%%
%%%     -module(whatever).
%%%     -export([foo/0]).
%%%     foo() ->
%%%         foobar(1, 2, 3).
%%%     foobar(A, B, C) ->
%%%         foobar(A, B, C, []).
%%%     foobar(A, B, C, D) -> ...
%%%
%%% The type optimization pass will find out that A, B, and C have constant
%%% values and do constant folding, rewriting foobar/3 to:
%%%
%%%     foobar(A, B, C) ->
%%%         foobar(1, 2, 3, []).
%%%
%%% That will result in three extra `move` instructions.
%%%
%%% This optimization sub pass will undo the constant folding
%%% optimization, rewriting code to use the original variable instead
%%% of the constant if the original variable is known to be in an x
%%% register.
%%%
%%% This optimization sub pass will also undo constant folding of the
%%% list of arguments in the call to error/2 in the last clause of a
%%% function. For example:
%%%
%%%     bar(X, Y) ->
%%%         error(function_clause, [X,42]).
%%%
%%% will be rewritten to:
%%%
%%%     bar(X, Y) ->
%%%         error(function_clause, [X,Y]).
%%%

ssa_opt_unfold_literals({St,FuncDb}) ->
    #opt_st{ssa=Blocks0,args=Args,anno=Anno,cnt=Count0} = St,
    ParamInfo = maps:get(parameter_info, Anno, #{}),
    LitMap = collect_arg_literals(Args, ParamInfo, 0, #{}),
    case map_size(LitMap) of
        0 ->
            %% None of the arguments for this function are known
            %% literals. Nothing to do.
            {St,FuncDb};
        _ ->
            SafeMap = #{0 => true},
            {Blocks,Count} = unfold_literals(beam_ssa:rpo(Blocks0),
                                             LitMap, SafeMap, Count0, Blocks0),
            {St#opt_st{ssa=Blocks,cnt=Count},FuncDb}
    end.

collect_arg_literals([V|Vs], Info, X, Acc0) ->
    case Info of
        #{V:=VarInfo} ->
            Type = proplists:get_value(type, VarInfo, any),
            case beam_types:get_singleton_value(Type) of
                {ok,Val} ->
                    F = fun(Vars) -> [{X,V}|Vars] end,
                    Acc = maps:update_with(Val, F, [{X,V}], Acc0),
                    collect_arg_literals(Vs, Info, X + 1, Acc);
                error ->
                    collect_arg_literals(Vs, Info, X + 1, Acc0)
            end;
        #{} ->
            collect_arg_literals(Vs, Info, X + 1, Acc0)
    end;
collect_arg_literals([], _Info, _X, Acc) -> Acc.

unfold_literals([L|Ls], LitMap, SafeMap0, Count0, Blocks0) ->
    {Blocks,Safe,Count} =
        case map_get(L, SafeMap0) of
            false ->
                %% Before reaching this block, an instruction that
                %% clobbers x registers has been executed.  *If* we
                %% would use an argument variable instead of literal,
                %% it would force the value to be saved to a y
                %% register. This is not what we want.
                {Blocks0,false,Count0};
            true ->
                %% All x registers live when entering the function
                %% are still live. Using the variable instead of
                %% the substituted value will eliminate a `move`
                %% instruction.
                #b_blk{is=Is0} = Blk = map_get(L, Blocks0),
                {Is,Safe0,Count1} = unfold_lit_is(Is0, LitMap, Count0, []),
                {Blocks0#{L:=Blk#b_blk{is=Is}},Safe0,Count1}
        end,
    %% Propagate safeness to successors.
    Successors = beam_ssa:successors(L, Blocks),
    SafeMap = unfold_update_succ(Successors, Safe, SafeMap0),
    unfold_literals(Ls, LitMap, SafeMap, Count,Blocks);
unfold_literals([], _, _, Count, Blocks) ->
    {Blocks,Count}.

unfold_update_succ([S|Ss], Safe, SafeMap0) ->
    F = fun(Prev) -> Prev and Safe end,
    SafeMap = maps:update_with(S, F, Safe, SafeMap0),
    unfold_update_succ(Ss, Safe, SafeMap);
unfold_update_succ([], _, SafeMap) -> SafeMap.

unfold_lit_is([#b_set{op=call,
                      args=[#b_remote{mod=#b_literal{val=erlang},
                                      name=#b_literal{val=error},
                                      arity=2},
                            #b_literal{val=function_clause},
                            ArgumentList]}=I0|Is], LitMap, Count0, Acc0) ->
    %% This is a call to error/2 that raises a function_clause
    %% exception in the final clause of a function. Try to undo
    %% constant folding in the list of arguments (the second argument
    %% for error/2).
    case unfold_arg_list(Acc0, ArgumentList, LitMap, Count0, 0, []) of
        {[FinalPutList|_]=Acc,Count} ->
            %% Acc now contains the possibly rewritten code that
            %% creates the argument list. All that remains is to
            %% rewrite the call to error/2 itself so that is will
            %% refer to rewritten argument list. This is essential
            %% when all arguments have known literal values as in this
            %% example:
            %%
            %%     foo(X, Y) -> error(function_clause, [0,1]).
            %%
            #b_set{op=put_list,dst=ListVar} = FinalPutList,
            #b_set{args=[ErlangError,Fc,_]} = I0,
            I = I0#b_set{args=[ErlangError,Fc,ListVar]},
            {reverse(Acc, [I|Is]),false,Count};
        {[],_} ->
            %% Handle code such as:
            %%
            %% bar(KnownValue, Stk) -> error(function_clause, Stk).
            {reverse(Acc0, [I0|Is]),false,Count0}
    end;
unfold_lit_is([#b_set{op=Op,args=Args0}=I0|Is], LitMap, Count, Acc) ->
    %% Using a register instead of a literal is a clear win only for
    %% `call` and `make_fun` instructions. Substituting into other
    %% instructions is unlikely to be an improvement.
    Unfold = case Op of
                 call -> true;
                 make_fun -> true;
                 _ -> false
             end,
    I = case Unfold of
            true ->
                Args = unfold_call_args(Args0, LitMap, -1),
                I0#b_set{args=Args};
            false ->
                I0
        end,
    case beam_ssa:clobbers_xregs(I) of
        true ->
            %% This instruction clobbers x register. Don't do
            %% any substitutions in rest of this block or in any
            %% of its successors.
            {reverse(Acc, [I|Is]),false,Count};
        false ->
            unfold_lit_is(Is, LitMap, Count, [I|Acc])
    end;
unfold_lit_is([], _LitMap, Count, Acc) ->
    {reverse(Acc),true,Count}.

%% unfold_arg_list(Is, ArgumentList, LitMap, Count0, X, Acc) ->
%%     {UpdatedAcc, Count}.
%%
%%  Unfold the arguments in the argument list (second argument for error/2).
%%
%%  Note that Is is the reversed list of instructions before the
%%  call to error/2. Because of the way the list is built in reverse,
%%  it means that the first put_list instruction found will add the first
%%  argument (x0) to the list, the second the second argument (x1), and
%%  so on.

unfold_arg_list(Is, #b_literal{val=[Hd|Tl]}, LitMap, Count0, X, Acc) ->
    %% Handle the case that the entire argument list (the second argument
    %% for error/2) is a literal.
    {PutListDst,Count} = new_var('@put_list', Count0),
    PutList = #b_set{op=put_list,dst=PutListDst,
                     args=[#b_literal{val=Hd},#b_literal{val=Tl}]},
    unfold_arg_list([PutList|Is], PutListDst, LitMap, Count, X, Acc);
unfold_arg_list([#b_set{op=put_list,dst=List,
                         args=[Hd0,#b_literal{val=[Hd|Tl]}]}=I0|Is0],
                 List, LitMap, Count0, X, Acc) ->
    %% The rest of the argument list is a literal list.
    {PutListDst,Count} = new_var('@put_list', Count0),
    PutList = #b_set{op=put_list,dst=PutListDst,
                     args=[#b_literal{val=Hd},#b_literal{val=Tl}]},
    I = I0#b_set{args=[Hd0,PutListDst]},
    unfold_arg_list([I,PutList|Is0], List, LitMap, Count, X, Acc);
unfold_arg_list([#b_set{op=put_list,dst=List,args=[Hd0,Tl]}=I0|Is],
                 List, LitMap, Count, X, Acc) ->
    %% Unfold the head of the list.
    Hd = unfold_arg(Hd0, LitMap, X),
    I = I0#b_set{args=[Hd,Tl]},
    unfold_arg_list(Is, Tl, LitMap, Count, X + 1, [I|Acc]);
unfold_arg_list([I|Is], List, LitMap, Count, X, Acc) ->
    %% Some other instruction, such as bs_get_tail.
    unfold_arg_list(Is, List, LitMap, Count, X, [I|Acc]);
unfold_arg_list([], _, _, Count, _, Acc) ->
    {reverse(Acc),Count}.

unfold_call_args([A0|As], LitMap, X) ->
    A = unfold_arg(A0, LitMap, X),
    [A|unfold_call_args(As, LitMap, X + 1)];
unfold_call_args([], _, _) -> [].

unfold_arg(#b_literal{val=Val}=Lit, LitMap, X) ->
    case LitMap of
        #{Val:=Vars} ->
            %% This literal is available in an x register.
            %% If it is in the correct x register, use
            %% the register. Don't bother if it is in the
            %% wrong register, because that would still result
            %% in a `move` instruction.
            case keyfind(X, 1, Vars) of
                false -> Lit;
                {X,Var} -> Var
            end;
        #{} -> Lit
    end;
unfold_arg(Expr, _LitMap, _X) -> Expr.

%%%
%%% Common utilities.
%%%

non_guards(Linear) ->
    gb_sets:from_list(non_guards_1(Linear)).

non_guards_1([{L,#b_blk{is=Is}}|Bs]) ->
    case Is of
        [#b_set{op=exception_trampoline}|_] ->
            [L | non_guards_1(Bs)];
        [#b_set{op=landingpad}|_] ->
            [L | non_guards_1(Bs)];
        _ ->
            non_guards_1(Bs)
    end;
non_guards_1([]) ->
    [?EXCEPTION_BLOCK].

rel2fam(S0) ->
    S1 = sofs:relation(S0),
    S = sofs:rel2fam(S1),
    sofs:to_external(S).

sub(I, Sub) ->
    beam_ssa:normalize(sub_1(I, Sub)).

sub_1(#b_set{op=phi,args=Args}=I, Sub) ->
    I#b_set{args=[{sub_arg(A, Sub),P} || {A,P} <- Args]};
sub_1(#b_set{args=Args}=I, Sub) ->
    I#b_set{args=[sub_arg(A, Sub) || A <- Args]};
sub_1(#b_br{bool=#b_var{}=Old}=Br, Sub) ->
    New = sub_arg(Old, Sub),
    Br#b_br{bool=New};
sub_1(#b_switch{arg=#b_var{}=Old}=Sw, Sub) ->
    New = sub_arg(Old, Sub),
    Sw#b_switch{arg=New};
sub_1(#b_ret{arg=#b_var{}=Old}=Ret, Sub) ->
    New = sub_arg(Old, Sub),
    Ret#b_ret{arg=New};
sub_1(Last, _) -> Last.

sub_arg(#b_remote{mod=Mod,name=Name}=Rem, Sub) ->
    Rem#b_remote{mod=sub_arg(Mod, Sub),name=sub_arg(Name, Sub)};
sub_arg(Old, Sub) ->
    case Sub of
        #{Old:=New} -> New;
        #{} -> Old
    end.

new_var(#b_var{name={Base,N}}, Count) ->
    true = is_integer(N),                       %Assertion.
    {#b_var{name={Base,Count}},Count+1};
new_var(#b_var{name=Base}, Count) ->
    {#b_var{name={Base,Count}},Count+1};
new_var(Base, Count) when is_atom(Base) ->
    {#b_var{name={Base,Count}},Count+1}.
