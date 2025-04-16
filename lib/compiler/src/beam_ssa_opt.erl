%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2018-2025. All Rights Reserved.
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
-moduledoc false.
-export([module/2]).

-include("beam_ssa_opt.hrl").

-import(lists, [all/2,append/1,droplast/1,duplicate/2,flatten/1,foldl/3,
                keyfind/3,last/1,mapfoldl/3,member/2,
                partition/2,reverse/1,reverse/2,
                splitwith/2,sort/1,takewhile/2,unzip/1]).

-define(MAX_REPETITIONS, 16).

-spec module(beam_ssa:b_module(), [compile:option()]) ->
                    {'ok',beam_ssa:b_module()}.

module(Module0, Opts) ->
    {Module,NifInfo} = isolate_nifs(Module0),
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
              {once, Order, early_epilogue_passes(Opts)},
              {module, epilogue_module_passes(Opts)},
              {once, Order, late_epilogue_passes(Opts)}],

    StMap = run_phases(Phases, StMap0, FuncDb),
    {ok, restore_nifs(finish(Module, StMap), NifInfo)}.

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
fixpoint(FuncIds0, Order0, Passes, StMap0, FuncDb0, N) when is_map(StMap0) ->
    {StMap, FuncDb} = phase(FuncIds0, Passes, StMap0, FuncDb0),
    Repeat = changed(FuncIds0, FuncDb0, FuncDb, StMap0, StMap),
    case sets:is_empty(Repeat) of
        true ->
            %% No change. Fixpoint reached.
            {StMap, FuncDb};
        false ->
            %% Repeat the optimizations for functions whose code has
            %% changed or for which there is potentially updated type
            %% information.
            {OrderA, OrderB} = Order0,
            Order = {OrderB, OrderA},
            FuncIds = [Id || Id <- OrderA, sets:is_element(Id, Repeat)],
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
    EmptySet = sets:new([{version,2}]),
    Changed0 = changed_types(PrevIds, FuncDb0, FuncDb, EmptySet, EmptySet),

    %% From all functions that were optimized in the previous run, find the
    %% functions that had any change in the SSA code. Those functions might
    %% gain from being optimized again. (For example, when beam_ssa_dead has
    %% shortcut branches, the types for some variables could become narrower,
    %% giving beam_ssa_type new opportunities for optimization.)
    %%
    %% Note that the functions examined could be functions with module-level
    %% optimization turned off (and thus not included in FuncDb).
    foldl(fun(Id, Changed) ->
                  case sets:is_element(Id, Changed) of
                      true ->
                          %% Already scheduled for another optimization.
                          %% No need to compare the SSA code.
                          Changed;
                      false ->
                          %% Compare the SSA code before and after optimization.
                          case {map_get(Id, StMap0), map_get(Id, StMap)} of
                              {Same, Same} -> Changed;
                              {_,_} -> sets:add_element(Id, Changed)
                          end
                  end
          end, Changed0, PrevIds).

%% Find all functions in FuncDb that can be reached by changes of argument
%% and/or return types. Those are the functions that may gain from running the
%% optimization passes again.
%%
%% Note that we examine all functions in FuncDb, not only functions optimized
%% in the previous run, because the argument types may have been updated for
%% functions not included in the previous run.
changed_types([Id | Ids], Fdb0, Fdb, In0, Out0) ->
    case {Fdb0, Fdb} of
        {#{ Id := #func_info{arg_types=ATs0,succ_types=ST0} },
         #{ Id := #func_info{arg_types=ATs,succ_types=ST} }} ->
            In = case ST0 =:= ST of
                     true -> In0;
                     false -> changed_types_1([Id], #func_info.in, Fdb, In0)
                 end,
            Out = case ATs0 =:= ATs of
                      true -> Out0;
                      false -> changed_types_1([Id], #func_info.out, Fdb, Out0)
                  end,
            changed_types(Ids, Fdb0, Fdb, In, Out);
        _ ->
            %% This function is exempt from module-level optimization and will
            %% not provide any more information.
            changed_types(Ids, Fdb0, Fdb, In0, Out0)
    end;
changed_types([], _Fdb0, _Fdb, In, Out) ->
    sets:union(In, Out).

changed_types_1([Id | Ids], Direction, Fdb, Seen0) ->
    case sets:is_element(Id, Seen0) of
        true ->
            changed_types_1(Ids, Direction, Fdb, Seen0);
        false ->
            case Fdb of
                #{ Id := FuncInfo } ->
                    Next = element(Direction, FuncInfo),

                    Seen1 = sets:add_element(Id, Seen0),
                    Seen2 = changed_types_1(Next, Direction, Fdb, Seen1),
                    changed_types_1(Ids, Direction, Fdb, Seen2);
                #{} ->
                    changed_types_1(Ids, Direction, Fdb, Seen0)
            end
    end;
changed_types_1([], _, _, Seen) ->
    Seen.


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
          ?PASS(ssa_opt_update_tuple),
          ?PASS(ssa_opt_cse),                   % Helps the first type pass.
          ?PASS(ssa_opt_live)],                 % ...
    passes_1(Ps, Opts).

module_passes(Opts) ->
    Ps0 = [{ssa_opt_bc_size,
            fun({StMap, FuncDb}) ->
                    {beam_ssa_bc_size:opt(StMap), FuncDb}
            end},
           {ssa_opt_type_start,
            fun({StMap, FuncDb}) ->
                    beam_ssa_type:opt_start(StMap, FuncDb)
            end}],
    passes_1(Ps0, Opts).

%% These passes all benefit from each other (in roughly this order), so they
%% are repeated as required.
repeated_passes(Opts) ->
    Ps = [?PASS(ssa_opt_live),
          ?PASS(ssa_opt_ne),
          ?PASS(ssa_opt_bs_create_bin),
          ?PASS(ssa_opt_dead),
          ?PASS(ssa_opt_cse),
          ?PASS(ssa_opt_tail_phis),
          ?PASS(ssa_opt_sink),
          ?PASS(ssa_opt_tuple_size),
          ?PASS(ssa_opt_merge_updates),
          ?PASS(ssa_opt_record),
          ?PASS(ssa_opt_try),
          ?PASS(ssa_opt_type_continue)],        %Must run after ssa_opt_dead to
                                                %clean up phi nodes.
    passes_1(Ps, Opts).

epilogue_module_passes(Opts) ->
    Ps0 = [{ssa_opt_alias,
            fun({StMap, FuncDb}) ->
                    beam_ssa_alias:opt(StMap, FuncDb)
            end},
           {ssa_opt_destructive_update,
            fun({StMap, FuncDb}) ->
                    beam_ssa_destructive_update:opt(StMap, FuncDb)
            end}],
    passes_1(Ps0, Opts).

early_epilogue_passes(Opts) ->
    Ps = [?PASS(ssa_opt_type_finish),
          ?PASS(ssa_opt_float),
          ?PASS(ssa_opt_sw),
          ?PASS(ssa_opt_no_reuse)],
    passes_1(Ps, Opts).

late_epilogue_passes(Opts) ->
    Ps = [%% Run live one more time to clean up after the previous
          %% epilogue passes.
          ?PASS(ssa_opt_live),
          ?PASS(ssa_opt_bsm),
          ?PASS(ssa_opt_bsm_shortcut),
          ?PASS(ssa_opt_sink),
          ?PASS(ssa_opt_blockify),
          ?PASS(ssa_opt_redundant_br),
          ?PASS(ssa_opt_merge_blocks),
          ?PASS(ssa_opt_bs_ensure),
          ?PASS(ssa_opt_try),
          ?PASS(ssa_opt_get_tuple_element),
          ?PASS(ssa_opt_tail_literals),
          ?PASS(ssa_opt_trim_unreachable),
          ?PASS(ssa_opt_unfold_literals),
          ?PASS(ssa_opt_ranges)],
    passes_1(Ps, Opts).

passes_1(Ps, Opts0) ->
    Negations = [{list_to_atom("no_"++atom_to_list(N)),N} ||
                    {N,_} <:- Ps],
    Expansions = [{no_bs_match,[no_ssa_opt_bs_ensure,no_bs_match]}],
    Opts = proplists:normalize(Opts0, [{expand,Expansions},
                                       {negations,Negations}]),
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
    fdb_fs(Fs, Exports, #{}).

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

    RPO = beam_ssa:rpo(Bs),
    FuncDb = beam_ssa:fold_blocks(fun(_L, #b_blk{is=Is}, FuncDb) ->
                                       fdb_is(Is, Id, FuncDb)
                               end, RPO, FuncDb1, Bs),

    fdb_fs(Fs, Exports, FuncDb);
fdb_fs([], _Exports, FuncDb) ->
    FuncDb.

fdb_is([#b_set{op=call,
               args=[#b_local{}=Callee | _]} | Is],
       Caller, FuncDb) ->
    fdb_is(Is, Caller, fdb_update(Caller, Callee, FuncDb));
fdb_is([#b_set{op=make_fun,args=[#b_local{}=Callee | _]} | Is],
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

get_call_order_po(StMap, FuncDb) when is_map(FuncDb) ->
    Order = gco_po(FuncDb),
    Order ++ sort([K || K <- maps:keys(StMap), not is_map_key(K, FuncDb)]).

gco_po(FuncDb) ->
    All = sort(maps:keys(FuncDb)),
    {RPO,_} = gco_rpo(All, FuncDb, sets:new(), []),
    reverse(RPO).

gco_rpo([Id|Ids], FuncDb, Seen0, Acc0) ->
    case sets:is_element(Id, Seen0) of
        true ->
            gco_rpo(Ids, FuncDb, Seen0, Acc0);
        false ->
            #func_info{out=Successors} = map_get(Id, FuncDb),
            Seen1 = sets:add_element(Id, Seen0),
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

ssa_opt_merge_blocks({#opt_st{ssa=Blocks0}=St, FuncDb}) ->
    RPO = beam_ssa:rpo(Blocks0),
    Blocks = beam_ssa:merge_blocks(RPO, Blocks0),
    {St#opt_st{ssa=Blocks}, FuncDb}.

ssa_opt_ranges({#opt_st{ssa=Blocks}=St, FuncDb}) ->
    {St#opt_st{ssa=beam_ssa_type:opt_ranges(Blocks)}, FuncDb}.

%%%
%%% Merges updates that cannot fail, for example two consecutive updates of the
%%% same record.
%%%

ssa_opt_merge_updates({#opt_st{ssa=Linear0}=St, FuncDb}) ->
    Linear = merge_updates_bs(Linear0),
    {St#opt_st{ssa=Linear}, FuncDb}.

%% As update_record is always converted from setelement/3 operations they can
%% only occur alone in their blocks at this point, so we don't need to look
%% deeper than this.
merge_updates_bs([{LblA,
                   #b_blk{is=[#b_set{op=update_record,
                                     dst=DstA,
                                     args=[SpecA, Size, Src | ListA]}],
                          last=#b_br{bool=#b_literal{val=true},
                                     succ=LblB}}=BlkA},
                  {LblB,
                   #b_blk{is=[#b_set{op=update_record,
                                     args=[SpecB, Size, DstA | ListB]}=Update0]
                          }=BlkB} | Bs]) ->
    Spec = case SpecA =:= SpecB of
               true -> SpecA;
               false -> #b_literal{val=copy}
           end,
    List = merge_update_record_lists(ListA ++ ListB, #{}),
    Update = Update0#b_set{args=[Spec, Size, Src | List]},

    %% Note that we retain the first update_record in case it's used elsewhere,
    %% it's too rare to warrant special handling here.
    [{LblA, BlkA}, {LblB, BlkB#b_blk{is=[Update]}}| merge_updates_bs(Bs)];
merge_updates_bs([{LblA,
                   #b_blk{is=[#b_set{op=update_record}]=IsA,
                          last=#b_br{bool=#b_literal{val=true},
                                     succ=LblB}}=BlkA},
                  {LblB,
                   #b_blk{is=[#b_set{op=executable_line}]=IsB}=BlkB} | Bs0]) ->
    Bs = [{LblB,BlkB#b_blk{is=IsA}} | Bs0],
    [{LblA,BlkA#b_blk{is=IsB}} | merge_updates_bs(Bs)];
merge_updates_bs([{LblA, BlkA}, {LblB, BlkB} | Bs]) ->
    %% Try to merge creation of tuple followed by update_record.
    maybe
        #b_blk{is=[#b_set{op=update_record,
                          dst=UpdateDst,
                          args=[_,_,Dst|Updates]}]} ?= BlkB,
        #b_blk{is=[_|_]=Is,
               last=#b_br{bool=#b_literal{val=true},
                          succ=LblB}} ?= BlkA,
        #b_set{op=put_tuple,dst=Dst,args=Args0}=PutTuple0 ?= last(Is),
        Args = merge_tuple_update(Updates, Args0),
        PutTuple = PutTuple0#b_set{dst=UpdateDst,args=Args},
        [{LblA, BlkA}, {LblB, BlkB#b_blk{is=[PutTuple]}} |
         merge_updates_bs(Bs)]
    else
        _ ->
            [{LblA, BlkA} | merge_updates_bs([{LblB, BlkB} | Bs])]
    end;
merge_updates_bs([{Lbl, Blk} | Bs]) ->
    [{Lbl, Blk} | merge_updates_bs(Bs)];
merge_updates_bs([]) ->
    [].

merge_update_record_lists([Index, Value | List], Updates) ->
    merge_update_record_lists(List, Updates#{ Index => Value });
merge_update_record_lists([], Updates) ->
    maps:fold(fun(K, V, Acc) ->
                      [K, V | Acc]
              end, [], maps:iterator(Updates, reversed)).

merge_tuple_update(Updates, Args) ->
    merge_tuple_update_1(Updates, list_to_tuple(Args)).

merge_tuple_update_1([#b_literal{val=Position}, Val | Updates], Tuple0) ->
    Tuple = setelement(Position, Tuple0, Val),
    merge_tuple_update_1(Updates, Tuple);
merge_tuple_update_1([], Tuple) ->
    tuple_to_list(Tuple).

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
           (#b_set{op=bs_init_writable}) -> true;
           (#b_set{op=make_fun}) -> true;
           (_) -> false
        end,
    RPO = beam_ssa:rpo(Blocks0),
    {Blocks,Count} = beam_ssa:split_blocks_before(RPO, P, Blocks0, Count0),
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

ssa_opt_coalesce_phis({#opt_st{ssa=Blocks0}=St, FuncDb}) when is_map(Blocks0) ->
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
    [{Value,P} || {_,P} <:- Values] ++ As;
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
    [{L,{PhiDst,Val}} || {Val,L} <:- PhiArgs] ++ reduce_phis(Is);
reduce_phis([]) -> [].

opt_tail_phi_arg({PredL,Sub0}, Is0, Ret0, {Blocks0,Count0,Cost0}) ->
    Blk0 = map_get(PredL, Blocks0),
    #b_blk{is=IsPrefix,last=#b_br{succ=Next,fail=Next}} = Blk0,
    Sub1 = maps:from_list(Sub0),
    {Is1,Count,Sub} = new_names(Is0, Sub1, Count0, []),
    Is2 = [sub(I, Sub) || I <- Is1],
    Cost = build_cost(Is2, Cost0),
    Is = IsPrefix ++ Is2,
    Ret = sub(Ret0, Sub),
    Blk = Blk0#b_blk{is=Is,last=Ret},
    Blocks = Blocks0#{PredL:=Blk},
    {Blocks,Count,Cost}.

new_names([#b_set{dst=Dst}=I|Is], Sub0, Count0, Acc) ->
    {NewDst,Count} = new_var(Count0),
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
          #b_set{op={succeeded,guard},dst=Bool,args=[Element]}],
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
%%% Replaces setelement/3 with the update_tuple psuedo-instruction, and merges
%%% multiple such calls into the same instruction.
%%%
ssa_opt_update_tuple({#opt_st{ssa=Linear0}=St, FuncDb}) ->
    {St#opt_st{ssa=update_tuple_opt(Linear0, #{})}, FuncDb}.

update_tuple_opt([{L, #b_blk{is=Is0}=B} | Bs], SetOps0) ->
    {Is, SetOps} = update_tuple_opt_is(Is0, SetOps0, []),
    [{L, B#b_blk{is=Is}} | update_tuple_opt(Bs, SetOps)];
update_tuple_opt([], _SetOps) ->
    [].

update_tuple_opt_is([#b_set{op=call,
                            dst=Dst,
                            args=[#b_remote{mod=#b_literal{val=erlang},
                                            name=#b_literal{val=setelement}},
                                  #b_literal{val=N}=Index,
                                  Src,
                                  Value]}=I0 | Is],
                  SetOps0, Acc) when is_integer(N), N >= 1 ->
    SetOps1 = SetOps0#{ Dst => {Src, Index, Value} },
    SetOps = maps:remove(Value, SetOps1),

    Args = update_tuple_merge(Src, SetOps, [Index, Value],
                              sets:new([{version,2}])),
    I = I0#b_set{op=update_tuple,dst=Dst,args=Args},

    update_tuple_opt_is(Is, SetOps, [I | Acc]);
update_tuple_opt_is([#b_set{op=Op}=I | Is], SetOps0, Acc) ->
    case {Op, beam_ssa:clobbers_xregs(I)} of
        {_, true} ->
            %% Merging setelement across stack frames is potentially very
            %% expensive as the update list needs to be saved on the stack, so
            %% we discard our state whenever we need one.
            update_tuple_opt_is(Is, #{}, [I | Acc]);
        {{succeeded, _}, false} ->
            %% This is a psuedo-op used to link ourselves with our catch block,
            %% so it doesn't really count as a use.
            update_tuple_opt_is(Is, SetOps0, [I | Acc]);
        {_, false} ->
            %% It's pointless to merge us with later ops if our result is used
            %% and needs to be created anyway.
            SetOps = maps:without(beam_ssa:used(I), SetOps0),
            update_tuple_opt_is(Is, SetOps, [I | Acc])
    end;
update_tuple_opt_is([], SetOps, Acc) ->
    {reverse(Acc), SetOps}.

update_tuple_merge(Src, SetOps, Updates0, Seen0) ->
    %% Note that we're merging in reverse order, so Updates0 contains the
    %% updates made *after* this one.
    case SetOps of
        #{ Src := {Ancestor, Index, Value} } ->
            %% Drop redundant updates, which can happen when when a record is
            %% updated in several branches and one of them overwrites a
            %% previous index.
            Updates = case sets:is_element(Index, Seen0) of
                          false -> [Index, Value | Updates0];
                          true -> Updates0
                      end,
            Seen = sets:add_element(Index, Seen0),
            update_tuple_merge(Ancestor, SetOps, Updates, Seen);
        #{} ->
            [Src | Updates0]
    end.

%%%
%%% Common subexpression elimination (CSE).
%%%
%%% Eliminate repeated evaluation of identical expressions. To avoid
%%% increasing the size of the stack frame, we don't eliminate
%%% subexpressions across instructions that clobber the X registers.
%%%

ssa_opt_cse({#opt_st{ssa=Linear0}=St, FuncDb}) ->
    M = #{0 => #{}, ?EXCEPTION_BLOCK => #{}},
    Linear1 = cse(Linear0, #{}, M),
    Linear = beam_ssa:trim_unreachable(Linear1), %Fix up phi nodes.
    {St#opt_st{ssa=Linear}, FuncDb}.

cse([{L,#b_blk{is=Is0,last=Last0}=Blk0}|Bs], Sub0, M0) ->
    case M0 of
        #{L := Es0} ->
            {Is1,Es,Sub} = cse_is(Is0, Es0, Sub0, []),
            Last = sub(Last0, Sub),
            Blk = Blk0#b_blk{last=Last},
            M = cse_successors(Is1, Blk, Es, M0),
            Is = reverse(Is1),
            [{L,Blk#b_blk{is=Is,last=Last}}|cse(Bs, Sub, M)];
        #{} ->
            %% This block is never reached.
            cse(Bs, Sub0, M0)
    end;
cse([], _, _) -> [].

cse_successors([#b_set{op={succeeded,_},args=[Src]},Bif|_], Blk, EsSucc, M0) ->
    case cse_suitable(Bif) of
        true ->
            %% The previous instruction only has a valid value at the success branch.
            %% We must remove the substitution for Src from the failure branch.
            #b_blk{last=#b_br{succ=Succ,fail=Fail}} = Blk,
            M1 = cse_successors_1([Succ], EsSucc, M0),
            cse_successor_fail(Fail, Src, EsSucc, M1);
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
            Es = cse_intersection(Es0, Es1),
            cse_successors_1(Ls, Es0, M#{L:=Es});
        #{} ->
            cse_successors_1(Ls, Es0, M#{L=>Es0})
    end;
cse_successors_1([], _, M) -> M.

cse_successor_fail(Fail, Src, LHS0, M) ->
    case M of
        #{Fail := RHS} when map_size(RHS) =:= 0 ->
            M;
        #{Fail := RHS} ->
            LHS = #{Var => Val || Var := Val <- LHS0,
                                  is_map_key(Var, RHS),
                                  Val =/= Src},
            M#{Fail := cse_intersection(LHS, RHS)};
        #{} ->
            LHS = #{Var => Val || Var := Val <- LHS0, Val =/= Src},
            M#{Fail => LHS}
    end.

%% Calculate the intersection of the two maps. Both keys and values
%% must match.
cse_intersection(M1, M2) ->
    MapSize1 = map_size(M1),
    MapSize2 = map_size(M2),
    if
        MapSize1 < MapSize2 ->
            cse_intersection_1(maps:to_list(M1), M2, M1);
        MapSize1 > MapSize2 ->
            cse_intersection_1(maps:to_list(M2), M1, M2);
        M1 =:= M2 ->
            M2;
        true ->
            cse_intersection_1(maps:to_list(M1), M2, M1)
    end.

cse_intersection_1([{Key,Value}|KVs], M, Result) ->
    case M of
        #{Key := Value} ->
            cse_intersection_1(KVs, M, Result);
        #{} ->
            cse_intersection_1(KVs, M, maps:remove(Key, Result))
    end;
cse_intersection_1([], _, Result) -> Result.

cse_is([#b_set{op={succeeded,_},dst=Bool,args=[Src]}=I0|Is], Es, Sub0, Acc) ->
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
cse_is([#b_set{op=put_map,dst=Dst,args=[_Kind,Map|_]}=I0|Is],
       Es0, Sub0, Acc) ->
    I1 = sub(I0, Sub0),
    {ok,ExprKey} = cse_expr(I1),
    case Es0 of
        #{ExprKey:=PrevPutMap} ->
            Sub = Sub0#{Dst=>PrevPutMap},
            cse_is(Is, Es0, Sub, Acc);
        #{Map:=PutMap} ->
            case combine_put_maps(PutMap, I1) of
                none ->
                    Es1 = Es0#{ExprKey=>Dst},
                    Es = cse_add_inferred_exprs(I1, Es1),
                    cse_is(Is, Es, Sub0, [I1|Acc]);
                I ->
                    Es1 = Es0#{ExprKey=>Dst},
                    Es = cse_add_inferred_exprs(I1, Es1),
                    cse_is(Is, Es, Sub0, [I|Acc])
            end;
        #{} ->
            Es1 = Es0#{ExprKey=>Dst},
            Es = cse_add_inferred_exprs(I1, Es1),
            cse_is(Is, Es, Sub0, [I1|Acc])
    end;
cse_is([#b_set{dst=Dst}=I0|Is], Es0, Sub0, Acc) ->
    I = sub(I0, Sub0),
    case beam_ssa:eval_instr(I) of
        #b_literal{}=Value ->
            Sub = Sub0#{Dst => Value},
            cse_is(Is, Es0, Sub, Acc);
        failed ->
            case Is of
                [#b_set{op={succeeded,guard},dst=SuccDst,args=[Dst]}] ->
                    %% In a guard. The failure reason doesn't matter,
                    %% so we can discard this instruction and the
                    %% `succeeded` instruction. Since the success
                    %% branch will never be taken, it usually means
                    %% that one or more blocks can be discarded as
                    %% well, saving some compilation time.
                    Sub = Sub0#{SuccDst => #b_literal{val=false}},
                    {Acc,Es0,Sub};
                _ ->
                    %% In a body. We must preserve the exact failure
                    %% reason, which is most easily done by keeping the
                    %% instruction.
                    cse_instr(I, Is, Es0, Sub0, Acc)
            end;
        any ->
            cse_instr(I, Is, Es0, Sub0, Acc)
    end;
cse_is([], Es, Sub, Acc) ->
    {Acc,Es,Sub}.

cse_instr(#b_set{dst=Dst}=I, Is, Es0, Sub0, Acc) ->
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
                        #{ExprKey := Src} ->
                            %% Reuse the result of the previous expression.
                            Sub = Sub0#{Dst => Src},
                            cse_is(Is, Es0, Sub, Acc);
                        #{} ->
                            Es1 = Es0#{ExprKey => Dst},
                            Es = cse_add_inferred_exprs(I, Es1),
                            cse_is(Is, Es, Sub0, [I|Acc])
                    end
            end
    end.

cse_add_inferred_exprs(#b_set{op=put_list,dst=List,args=[Hd,Tl]}, Es) ->
    Es#{{get_hd,[List]} => Hd,
        {get_tl,[List]} => Tl};
cse_add_inferred_exprs(#b_set{op=put_tuple,dst=Tuple,args=[E1,E2|_]}, Es) ->
    %% Adding tuple elements beyond the first two does not seem to be
    %% worthwhile (at least not in the sample used by scripts/diffable).
    Es#{{get_tuple_element,[Tuple,#b_literal{val=0}]} => E1,
        {get_tuple_element,[Tuple,#b_literal{val=1}]} => E2};
cse_add_inferred_exprs(#b_set{op=put_map,dst=Map,args=[_,_|Args]}=I, Es0) ->
    Es = cse_add_map_get(Args, Map, Es0),
    Es#{Map => I};
cse_add_inferred_exprs(_, Es) -> Es.

cse_add_map_get([Key,Value|T], Map, Es0) ->
    Es = Es0#{{get_map_element,[Map,Key]} => Value},
    cse_add_map_get(T, Map, Es);
cse_add_map_get([], _, Es) -> Es.

cse_expr(#b_set{op={bif,hd},args=[List]}) ->
    {ok,{get_hd,[List]}};
cse_expr(#b_set{op={bif,tl},args=[List]}) ->
    {ok,{get_tl,[List]}};
cse_expr(#b_set{op={bif,element},args=[#b_literal{val=Index},Tuple]})
  when is_integer(Index) ->
    {ok,{get_tuple_element,[Tuple,#b_literal{val=Index-1}]}};
cse_expr(#b_set{op={bif,map_get},args=[Key,Map]}) ->
    {ok,{get_map_element,[Map,Key]}};
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
cse_suitable(#b_set{op=get_map_element}) -> true;
cse_suitable(#b_set{op=put_map}) -> true;
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

combine_put_maps(#b_set{dst=Prev,args=[#b_literal{val=assoc},Map|Args1]},
                 #b_set{args=[#b_literal{val=assoc},Prev|Args2]}=I) ->
    case are_map_keys_literals(Args1) andalso are_map_keys_literals(Args2) of
        true ->
            Args = combine_put_map_args(Args1, Args2),
            I#b_set{args=[#b_literal{val=assoc},Map|Args]};
        false ->
            none
    end;
combine_put_maps(#b_set{}, #b_set{}) ->
    none.

combine_put_map_args(Args1, Args2) ->
    Keys = sets:from_list(get_map_keys(Args2), [{version,2}]),
    combine_put_map_args_1(Args1, Args2, Keys).

combine_put_map_args_1([Key,Value|T], Tail, Keys) ->
    case sets:is_element(Key, Keys) of
        true ->
            combine_put_map_args_1(T, Tail, Keys);
        false ->
            [Key,Value|combine_put_map_args_1(T, Tail, Keys)]
    end;
combine_put_map_args_1([], Tail, _Keys) -> Tail.

get_map_keys([Key,_|T]) ->
    [Key|get_map_keys(T)];
get_map_keys([]) -> [].

are_map_keys_literals([#b_literal{},_Value|Args]) ->
    are_map_keys_literals(Args);
are_map_keys_literals([#b_var{}|_]) ->
    false;
are_map_keys_literals([]) ->
    true.

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
%%% that using float instructions in guards would be beneficial, why
%%% bother implementing a new instruction?
%%%

-type fr_status() :: 'original' | 'copy'.
-record(fs,
        {regs=#{} :: #{beam_ssa:b_var() := {beam_ssa:b_var(),fr_status()}},
         non_guards :: gb_sets:set(beam_ssa:label()),
         bs :: beam_ssa:block_map(),
         preds :: #{beam_ssa:label() => [beam_ssa:label()]}
        }).

ssa_opt_float({#opt_st{ssa=Linear0,cnt=Count0}=St, FuncDb}) ->
    NonGuards = non_guards(Linear0),
    Blocks = maps:from_list(Linear0),
    Preds = beam_ssa:predecessors(Blocks),
    Fs = #fs{non_guards=NonGuards,bs=Blocks,preds=Preds},
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
            {Flush,Blk,Fs,Count2} = float_maybe_flush(Blk0, Fs1, Count1),
            {Blks,Count3} = float_fixup_conv(L, Is1, Blk, Count2),
            {Bs,Count} = float_opt(Bs0, Count3, Fs),
            {Blks++Flush++Bs,Count};
        none ->
            {Bs,Count} = float_opt(Bs0, Count0, Fs0),
            {[{L,Blk0}|Bs],Count}
    end.

%% Split out {float,convert} instructions into separate blocks, number
%% the blocks, and add {succeeded,body} in each {float,convert} block.
float_fixup_conv(L, Is, Blk, Count0) ->
    Split = float_split_conv(Is, Blk),
    {Blks,Count} = float_number(Split, L, Count0),
    #b_blk{last=#b_br{bool=#b_var{},fail=Fail}} = Blk,
    float_conv(Blks, Fail, Count).

%% Split {float,convert} instructions into individual blocks.
float_split_conv(Is0, Blk) ->
    Br = #b_br{bool=#b_literal{val=true},succ=0,fail=0},

    %% Note that there may be other instructions such as
    %% remove_message before the floating point instructions;
    %% therefore, it is essential that we don't reorder instructions.
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

%% Number and chain the blocks that were split.
float_number(Bs0, FirstL, Count0) ->
    {[{_,FirstBlk}|Bs],Count} = float_number(Bs0, Count0),
    {[{FirstL,FirstBlk}|Bs],Count}.

float_number([B], Count) ->
    {[{Count,B}],Count};
float_number([B|Bs0], Count0) ->
    Next = Count0 + 1,
    {Bs,Count} = float_number(Bs0, Next),
    Br = #b_br{bool=#b_literal{val=true},succ=Next,fail=Next},
    {[{Count0,B#b_blk{last=Br}}|Bs],Count}.

%% Insert 'succeeded' instructions after each {float,convert}
%% instruction.
float_conv([{L,#b_blk{is=Is0,last=Last}=Blk0}|Bs0], Fail, Count0) ->
    case Is0 of
        [#b_set{op={float,convert}}=Conv] ->
            {Bool,Count1} = new_var(Count0),
            Succeeded = #b_set{op={succeeded,body},dst=Bool,
                               args=[Conv#b_set.dst]},
            Is = [Conv,Succeeded],
            Br = Last#b_br{bool=Bool,fail=Fail},
            Blk = Blk0#b_blk{is=Is,last=Br},
            {Bs,Count} = float_conv(Bs0, Fail, Count1),
            {[{L,Blk}|Bs],Count};
        [_|_] ->
            {Bs,Count} = float_conv(Bs0, Fail, Count0),
            {[{L,Blk0}|Bs],Count}
    end;
float_conv([], _, Count) ->
    {[],Count}.

float_maybe_flush(Blk0, Fs0, Count0) ->
    #b_blk{last=#b_br{bool=#b_var{},succ=Succ}=Br} = Blk0,

    %% If the success block has an optimizable floating point instruction,
    %% it is safe to defer flushing.
    case float_safe_to_skip_flush(Succ, Fs0) of
        true ->
            %% No flush needed.
            {[],Blk0,Fs0,Count0};
        false ->
            %% Flush needed. Allocate block numbers.
            FlushL = Count0,              %For flushing of float regs.
            Count = Count0 + 1,
            Blk = Blk0#b_blk{last=Br#b_br{succ=FlushL}},

            %% Build the block that flushes all registers.
            FlushIs = float_flush_regs(Fs0),
            FlushBr = #b_br{bool=#b_literal{val=true},succ=Succ,fail=Succ},
            FlushBlk = #b_blk{is=FlushIs,last=FlushBr},

            %% Update state record and blocks.
            Fs = Fs0#fs{regs=#{}},
            FlushBs = [{FlushL,FlushBlk}],
            {FlushBs,Blk,Fs,Count}
    end.

float_safe_to_skip_flush(L, #fs{bs=Blocks,preds=Preds}=Fs) ->
    #b_blk{is=Is} = Blk = map_get(L, Blocks),
    case Preds of
        #{L := [_]} ->
            float_can_optimize_blk(Blk, Fs) andalso float_optimizable_is(Is);
        #{} ->
            %% This block can be reached from more than one block; must flush.
            false
    end.

float_optimizable_is([#b_set{anno=#{float_op:=_}}|_]) ->
    true;
float_optimizable_is([#b_set{op=get_tuple_element}|Is]) ->
    %% The tuple sinking optimization can sink get_tuple_element instruction
    %% into a sequence of floating point operations.
    float_optimizable_is(Is);
float_optimizable_is(_) ->
    false.

float_opt_is([#b_set{op={succeeded,_},args=[Src]}=I0],
             #fs{regs=Rs}=Fs, Count, Acc) ->
    case Rs of
        #{Src := {Fr,_}} ->
            I = I0#b_set{args=[Fr]},
            {reverse(Acc, [I]),Fs,Count};
        #{} ->
            none
    end;
float_opt_is([#b_set{anno=Anno0}=I0|Is0], Fs0, Count0, Acc) ->
    case Anno0 of
        #{float_op:=FTypes} ->
            ArgTypes0 = maps:get(arg_types, Anno0, #{}),
            ArgTypes = float_arg_types(FTypes, 0, ArgTypes0),
            Anno1 = maps:remove(float_op, Anno0),
            Anno = maps:remove(arg_types, Anno1),
            I1 = I0#b_set{anno=Anno},
            {Is,Fs,Count} = float_make_op(I1, FTypes, ArgTypes, Fs0, Count0),
            float_opt_is(Is0, Fs, Count, reverse(Is, Acc));
        #{} ->
            float_opt_is(Is0, Fs0, Count0, [I0|Acc])
    end;
float_opt_is([], _Fs, _Count, _Acc) ->
    none.

float_arg_types([_|As], Index, ArgTypes) ->
    case ArgTypes of
        #{Index := ArgType} ->
            [ArgType|float_arg_types(As, Index + 1, ArgTypes)];
        #{} ->
            [any|float_arg_types(As, Index + 1, ArgTypes)]
    end;
float_arg_types([], _, _) -> [].

float_make_op(#b_set{op={bif,Op},dst=Dst,args=As0,anno=Anno}=I0,
              Ts, ArgTypes, #fs{regs=Rs0}=Fs, Count0) ->
    {As1,Rs1,Count1} = float_load(As0, Ts, ArgTypes, Anno, Rs0, Count0, []),
    {As,Is0} = unzip(As1),
    {FrDst,Count2} = new_var(Count1),
    I = I0#b_set{op={float,Op},dst=FrDst,args=As},
    Rs = Rs1#{Dst => {FrDst,original}},
    Is = append(Is0) ++ [I],
    {Is,Fs#fs{regs=Rs},Count2}.

float_load([A|As], [T|Ts], [AT|ATs], Anno, Rs0, Count0, Acc) ->
    {Load,Rs,Count} = float_reg_arg(A, T, AT, Anno, Rs0, Count0),
    float_load(As, Ts, ATs, Anno, Rs, Count, [Load|Acc]);
float_load([], [], [], _Anno, Rs, Count, Acc) ->
    {reverse(Acc),Rs,Count}.

float_reg_arg(A, T, AT, Anno0, Rs, Count0) ->
    case Rs of
        #{A := {Fr,_}} ->
            {{Fr,[]},Rs,Count0};
        #{} ->
            {Dst,Count} = new_var(Count0),
            I0 = float_load_reg(T, A, Dst),
            Anno = case AT of
                       any -> Anno0;
                       _ -> Anno0#{arg_types => #{0 => AT}}
                   end,
            I = I0#b_set{anno=Anno},
            {{Dst,[I]},Rs#{A => {Dst,copy}},Count}
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

float_flush_regs(#fs{regs=Rs}) ->
    maps:fold(fun(_, {#b_var{},copy}, Acc) ->
                      Acc;
                 (Dst, {Fr,original}, Acc) ->
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
    Live0 = live_opt_succ(Successors, L, LiveMap0, sets:new()),
    {Blk,Live} = live_opt_blk(Blk1, Live0),
    LiveMap = live_opt_phis(Blk#b_blk.is, L, Live, LiveMap0),
    live_opt(Bs, LiveMap, Blocks#{L:=Blk});
live_opt([], _, Acc) -> Acc.

live_opt_succ([S|Ss], L, LiveMap, Live0) ->
    case LiveMap of
        #{{S,L}:=Live} ->
            %% The successor has a phi node, and the value for
            %% this block in the phi node is a variable.
            live_opt_succ(Ss, L, LiveMap, sets:union(Live0, Live));
        #{S:=Live} ->
            %% No phi node in the successor, or the value for
            %% this block in the phi node is a literal.
            live_opt_succ(Ss, L, LiveMap, sets:union(Live0, Live));
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
            PhiArgs = append([Args || #b_set{args=Args} <:- Phis]),
            case [{P,V} || {#b_var{}=V,P} <- PhiArgs] of
                [_|_]=PhiVars ->
                    PhiLive0 = rel2fam(PhiVars),
                    PhiLive = #{{L,P} => list_set_union(Vs, Live0) ||
                                  {P,Vs} <:- PhiLive0},
                    maps:merge(LiveMap, PhiLive);
                [] ->
                    %% There were only literals in the phi node(s).
                    LiveMap
            end
    end.

live_opt_blk(#b_blk{is=Is0,last=Last}=Blk, Live0) ->
    Live1 = list_set_union(beam_ssa:used(Last), Live0),
    {Is,Live} = live_opt_is(reverse(Is0), Live1, []),
    {Blk#b_blk{is=Is},Live}.

live_opt_is([#b_set{op=phi,dst=Dst}=I|Is], Live0, Acc) ->
    Live = sets:del_element(Dst, Live0),
    case sets:is_element(Dst, Live0) of
        true ->
            live_opt_is(Is, Live, [I|Acc]);
        false ->
            live_opt_is(Is, Live, Acc)
    end;
live_opt_is([#b_set{op={succeeded,guard},dst=SuccDst,args=[Dst]}=SuccI,
             #b_set{op=Op,dst=Dst}=I0|Is],
            Live0, Acc) ->
    case {sets:is_element(SuccDst, Live0),
          sets:is_element(Dst, Live0)} of
        {true, true} ->
            Live = sets:del_element(SuccDst, Live0),
            live_opt_is([I0|Is], Live, [SuccI|Acc]);
        {true, false} ->
            %% The result of the instruction before {succeeded,guard} is
            %% unused. Attempt to perform a strength reduction.
            case Op of
                {bif,'not'} ->
                    I = I0#b_set{op={bif,is_boolean},dst=SuccDst},
                    live_opt_is([I|Is], Live0, Acc);
                {bif,tuple_size} ->
                    I = I0#b_set{op={bif,is_tuple},dst=SuccDst},
                    live_opt_is([I|Is], Live0, Acc);
                bs_start_match ->
                    %% This is safe in Erlang/OTP 27 and later, because match
                    %% contexts are now mutable sub binaries.
                    [#b_literal{val=new},Bin] = I0#b_set.args,
                    I = I0#b_set{op={bif,is_bitstring},args=[Bin],dst=SuccDst},
                    live_opt_is([I|Is], Live0, Acc);
                get_map_element ->
                    I = I0#b_set{op=has_map_field,dst=SuccDst},
                    live_opt_is([I|Is], Live0, Acc);
                _ ->
                    Live1 = sets:del_element(SuccDst, Live0),
                    Live = sets:add_element(Dst, Live1),
                    live_opt_is([I0|Is], Live, [SuccI|Acc])
            end;
        {false, true} ->
            live_opt_is([I0|Is], Live0, Acc);
        {false, false} ->
            live_opt_is(Is, Live0, Acc)
    end;
live_opt_is([#b_set{dst=Dst}=I|Is], Live0, Acc) ->
    case sets:is_element(Dst, Live0) of
        true ->
            Live1 = list_set_union(beam_ssa:used(I), Live0),
            Live = sets:del_element(Dst, Live1),
            live_opt_is(Is, Live, [I|Acc]);
        false ->
            case beam_ssa:no_side_effect(I) of
                true ->
                    live_opt_is(Is, Live0, Acc);
                false ->
                    Live = list_set_union(beam_ssa:used(I), Live0),
                    live_opt_is(Is, Live, [I|Acc])
            end
    end;
live_opt_is([], Live, Acc) ->
    {Acc,Live}.

%%%
%%% try/catch optimization.
%%%
%%% Attempts to rewrite try/catches as guards when we know the exception won't
%%% be inspected in any way, and removes try/catches whose expressions will
%%% never throw.
%%%

ssa_opt_try({#opt_st{ssa=SSA0,cnt=Count0}=St, FuncDb}) ->
    {Count, SSA} = opt_try(SSA0, Count0),
    {St#opt_st{ssa=SSA,cnt=Count}, FuncDb}.

opt_try(Blocks, Count0) when is_map(Blocks) ->
    {Count, Linear} = opt_try(beam_ssa:linearize(Blocks), Count0),
    {Count, maps:from_list(Linear)};
opt_try(Linear, Count0) when is_list(Linear) ->
    {Count, Shrunk} = shrink_try(Linear, Count0, []),

    Reduced = reduce_try(Shrunk, []),

    EmptySet = sets:new(),
    Trimmed = trim_try(Reduced, EmptySet, EmptySet, []),

    {Count, Trimmed}.

%% Moves all leading/trailing instructions that cannot fail out of try/catch
%% expressions. For example, we can move the tuple constructions `{defg,Arg}`
%% and `{hijk,A}` out of the `try` in the code below:
%%
%%     try
%%         A = abcd({defg,Arg}),
%%         ... snip ...
%%         {hijk,A}
%%     catch
%%         ... snip ...
%%     end.
shrink_try([{TryLbl0, #b_blk{is=[#b_set{op=new_try_tag,dst=Dst}],
                             last=#b_br{bool=Dst,succ=SuccLbl}}=TryBlk},
            {SuccLbl, #b_blk{is=SuccIs0,last=SuccLast}=SuccBlk0} | Bs],
           Count0, Acc0) ->
    %% Hoist leading known-safe instructions before `new_try_tag` instructions.
    {HoistIs, SuccIs} = hoist_try_is(SuccIs0, SuccLast, Dst, []),

    HoistLbl = TryLbl0,
    TryLbl = Count0,
    Count = Count0 + 1,

    HoistBlk = #b_blk{is=HoistIs,
                      last=#b_br{bool=#b_literal{val=true},
                                 succ=TryLbl,
                                 fail=TryLbl}},
    SuccBlk = SuccBlk0#b_blk{is=SuccIs},

    Acc = [{TryLbl, TryBlk},
           {HoistLbl, HoistBlk} | Acc0],

    shrink_try([{SuccLbl, SuccBlk} | Bs], Count, Acc);
shrink_try([{L, #b_blk{is=Is}=Blk0} | Bs], Count, Acc) ->
    Blk = Blk0#b_blk{is=sink_try_is(Is)},
    shrink_try(Bs, Count, [{L, Blk} | Acc]);
shrink_try([], Count, Acc) ->
    {Count, reverse(Acc)}.

hoist_try_is([#b_set{dst=Dst},
              #b_set{op={succeeded,_},args=[Dst]}]=Is,
             #b_br{}, _TryTag, HoistIs) ->
    {reverse(HoistIs), Is};
hoist_try_is([#b_set{dst=Dst}]=Is, #b_br{bool=Dst}, _TryTag, HoistIs) ->
    {reverse(HoistIs), Is};
hoist_try_is([#b_set{op=kill_try_tag,args=[TryTag]}=Kill | Rest],
             Last, TryTag, HoistIs0) ->
    %% We're killing the current try tag before we have a chance to throw an
    %% exception. Hoist the rest of the block and keep this instruction in the
    %% current block.
    {HoistIs, Is} = hoist_try_is(Rest, Last, TryTag, []),
    {reverse(HoistIs0, HoistIs), [Kill | Is]};
hoist_try_is([#b_set{}=I | Is], Last, TryTag, HoistIs) ->
    %% Note that we hoist instructions regardless of whether they side-effect
    %% or not: as long as they don't throw an exception, we don't need to care
    %% about side-effects as long as their order is unchanged.
    hoist_try_is(Is, Last, TryTag, [I | HoistIs]);
hoist_try_is([], _Last, _TryTag, HoistIs) ->
    {reverse(HoistIs), []}.

%% Moves trailing known-safe instructions past `kill_try_tag` instructions.
sink_try_is([#b_set{op=landingpad} | _]=Is) ->
    Is;
sink_try_is([#b_set{op=phi}=Phi | Is]) ->
    [Phi | sink_try_is(Is)];
sink_try_is(Is) ->
    sink_try_is_1(Is, []).

sink_try_is_1([#b_set{op=kill_try_tag}=Kill | Is], Acc) ->
    [Kill | reverse(Acc, Is)];
sink_try_is_1([I | Is], Acc) ->
    case is_safe_sink_try(I) of
        true -> sink_try_is_1(Is, [I | Acc]);
        false -> reverse(Acc, [I | Is])
    end;
sink_try_is_1([], Acc) ->
    reverse(Acc).

is_safe_sink_try(#b_set{op=Op}=I) ->
    case Op of
        bs_extract -> false;
        _ -> beam_ssa:no_side_effect(I)
    end.

%% Does a strength reduction of try/catch and catch.
%%
%% In try/catch constructs where the expression is restricted
%% (essentially a guard expression) and the error reason is ignored
%% in the catch part, such as:
%%
%%   try
%%      <RestrictedExpression>
%%   catch
%%      _:_ ->
%%        ...
%%   end
%%
%% the try/catch can be eliminated by simply removing the `new_try_tag`,
%% `landingpad`, and `kill_try_tag` instructions.
reduce_try([{L,#b_blk{is=[#b_set{op=new_try_tag}],
                      last=Last}=Blk0} | Bs0], Acc) ->
    #b_br{succ=Succ,fail=Fail} = Last,
    Ws = sets:from_list([Succ,Fail]),
    try do_reduce_try(Bs0, Ws) of
        Bs ->
            Blk = Blk0#b_blk{is=[],
                             last=#b_br{bool=#b_literal{val=true},
                                        succ=Succ,fail=Succ}},
            reduce_try(Bs, [{L, Blk} | Acc])
    catch
        throw:not_possible ->
            reduce_try(Bs0, [{L, Blk0} | Acc])
    end;
reduce_try([{L, Blk} | Bs], Acc) ->
    reduce_try(Bs, [{L, Blk} | Acc]);
reduce_try([], Acc) ->
    Acc.

do_reduce_try([{L, Blk} | Bs]=Bs0, Ws0) ->
    case sets:is_element(L, Ws0) of
        false ->
            %% This block is not reachable from the block with the
            %% `new_try_tag` instruction. Retain it. There is no
            %% need to check it for safety.
            case sets:is_empty(Ws0) of
                true -> Bs0;
                false -> [{L, Blk} | do_reduce_try(Bs, Ws0)]
            end;
        true ->
            Ws1 = sets:del_element(L, Ws0),
            #b_blk{is=Is0} = Blk,
            case reduce_try_is(Is0, []) of
                {safe,Is} ->
                    %% This block does not execute any instructions
                    %% that would require a try. Analyze successors.
                    Successors = beam_ssa:successors(Blk),
                    Ws = list_set_union(Successors, Ws1),
                    [{L, Blk#b_blk{is=Is}} | do_reduce_try(Bs, Ws)];
                unsafe ->
                    %% There is something unsafe in the block, for
                    %% example a `call` instruction or an `extract`
                    %% instruction.
                    throw(not_possible);
                {done,Is} ->
                    %% This block kills the try tag (either after successful
                    %% execution or at the landing pad). Don't analyze
                    %% successors.
                    [{L, Blk#b_blk{is=Is}} | do_reduce_try(Bs, Ws1)]
            end
    end;
do_reduce_try([], Ws) ->
    true = sets:is_empty(Ws),                   %Assertion.
    [].

reduce_try_is([#b_set{op=kill_try_tag}|Is], Acc) ->
    %% Remove this kill_try_tag instruction. If there was a landingpad
    %% instruction in this block, it has already been removed. Preserve
    %% all other instructions in the block.
    {done,reverse(Acc, Is)};
reduce_try_is([#b_set{op=extract}|_], _Acc) ->
    %% The error reason is accessed.
    unsafe;
reduce_try_is([#b_set{op=landingpad}|Is], Acc) ->
    reduce_try_is(Is, Acc);
reduce_try_is([#b_set{op={succeeded,body}}=I0|Is], Acc) ->
    %% If we reached this point, it means that the previous instruction
    %% has no side effects. We must now convert the flavor of the
    %% succeeded to the `guard`, since the try/catch will be removed.
    I = I0#b_set{op={succeeded,guard}},
    reduce_try_is(Is, [I|Acc]);
reduce_try_is([#b_set{op=call,args=[#b_remote{mod=#b_literal{val=M},
                                              name=#b_literal{val=F},
                                              arity=A}=R0|Args0]}=I0|Is],
              Acc) ->
    %% Rewrite binary_to_(existing_)atom/1 call to binary_to_(existing_)atom/2.
    {I1, Args1} = if {M, F, A} =:= {erlang, binary_to_atom, 1} orelse
                     {M, F, A} =:= {erlang, binary_to_existing_atom, 1} ->
                          Args = Args0++[#b_literal{val=utf8}],
                          {I0#b_set{args=[R0#b_remote{arity=2}|Args]},Args};
                     true -> {I0, Args0}
                  end,
    %% Remove try-catch for bifs that can be written as guards.
    case beam_ssa:can_be_guard_bif(M, F, A) of
        true ->
            I = I1#b_set{op={bif,F},args=Args1},
            reduce_try_is(Is, [I|Acc]);
        false -> unsafe
    end;
reduce_try_is([#b_set{op=Op}=I|Is], Acc) ->
    IsSafe = case Op of
                 phi -> true;
                 _ -> beam_ssa:no_side_effect(I)
             end,
    case IsSafe of
        true -> reduce_try_is(Is, [I|Acc]);
        false -> unsafe
    end;
reduce_try_is([], Acc) ->
    {safe,reverse(Acc)}.

%% Removes try/catch expressions whose expressions will never throw.
%%
%% We walk backwards through all blocks, maintaining a set of potentially
%% unreachable landing pads, removing them from the set whenever we see a
%% branch to that block. When we encounter a `new_try_tag` instruction that
%% references a block in the unreachable set, we'll remove the try/catch.
trim_try([{L, #b_blk{is=[#b_set{op=landingpad} | _]}=Blk}| Bs],
         Unreachable0, Killed, Acc) ->
    Unreachable1 = sets:add_element(L, Unreachable0),
    Successors = sets:from_list(beam_ssa:successors(Blk)),
    Unreachable = sets:subtract(Unreachable1, Successors),
    trim_try(Bs, Unreachable, Killed, [{L, Blk} | Acc]);
trim_try([{L, #b_blk{last=#b_ret{}}=Blk} | Bs], Unreachable, Killed, Acc) ->
    %% Nothing to update and nothing to optimize.
    trim_try(Bs, Unreachable, Killed, [{L,Blk}|Acc]);
trim_try([{L, Blk0} | Bs], Unreachable0, Killed0, Acc) ->
    case sets:is_empty(Unreachable0) of
        true ->
            %% Nothing to update and nothing to optimize.
            trim_try(Bs, Unreachable0, Killed0, [{L,Blk0}|Acc]);
        false ->
            #b_blk{is=Is0,last=Last0} = Blk0,
            case reverse(Is0) of
                [#b_set{op=new_try_tag,dst=Tag}|Is] ->
                    #b_br{succ=SuccLbl,fail=PadLbl} = Last0,
                    Unreachable = sets:del_element(PadLbl, Unreachable0),
                    case sets:is_element(PadLbl, Unreachable0) of
                        true ->
                            %% The landing pad can't be reached in any way,
                            %% remove the entire try/catch.
                            Blk = Blk0#b_blk{is=reverse(Is),
                                             last=#b_br{bool=#b_literal{val=true},
                                                        succ=SuccLbl,fail=SuccLbl}},
                            Killed = sets:add_element(Tag, Killed0),
                            trim_try(Bs, Unreachable, Killed, [{L,Blk}|Acc]);
                        false ->
                            trim_try(Bs, Unreachable, Killed0, [{L,Blk0}|Acc])
                    end;
                _ ->
                    %% Update the set of unreachable landing_pad blocks.
                    Successors = sets:from_list(beam_ssa:successors(Blk0)),
                    Unreachable = sets:subtract(Unreachable0, Successors),
                    trim_try(Bs, Unreachable, Killed0, [{L,Blk0}|Acc])
            end
    end;
trim_try([], _Unreachable, Killed, Acc0) ->
    case sets:is_empty(Killed) of
        true ->
            Acc0;
        false ->
            %% Remove all `kill_try_tag` instructions referencing removed
            %% try/catches.
            [{L, Blk#b_blk{is=trim_try_is(Is0, Killed)}} ||
                {L, #b_blk{is=Is0}=Blk} <:- Acc0]
    end.

trim_try_is([#b_set{op=phi,dst=CatchEndVal}=Phi,
             #b_set{op=catch_end,dst=Dst,args=[Tag,CatchEndVal]}=Catch | Is],
            Killed) ->
    case sets:is_element(Tag, Killed) of
        true -> [Phi#b_set{dst=Dst} | trim_try_is(Is, Killed)];
        false -> [Phi, Catch | trim_try_is(Is, Killed)]
    end;
trim_try_is([#b_set{op=kill_try_tag,args=[Tag]}=I | Is], Killed) ->
    case sets:is_element(Tag, Killed) of
        true -> trim_try_is(Is, Killed);
        false -> [I | trim_try_is(Is, Killed)]
    end;
trim_try_is([I | Is], Killed) ->
    [I | trim_try_is(Is, Killed)];
trim_try_is([], _Killed) ->
    [].

%%%
%%% Optimize binary matching.
%%%
%%% * If the value of segment is never extracted, rewrite
%%%   to a bs_skip instruction.
%%%
%%% * Coalesce adjacent bs_skip instructions and skip instructions
%%%   with bs_test_tail.
%%%

ssa_opt_bsm({#opt_st{ssa=Linear0}=St, FuncDb}) ->
    Extracted0 = bsm_extracted(Linear0),
    Extracted = sets:from_list(Extracted0),
    Linear1 = bsm_skip(Linear0, Extracted),
    Linear = bsm_coalesce_skips(Linear1, #{}),
    {St#opt_st{ssa=Linear}, FuncDb}.

bsm_skip([{L,#b_blk{is=Is0}=Blk}|Bs0], Extracted) ->
    Bs = bsm_skip(Bs0, Extracted),
    Is = bsm_skip_is(Is0, Extracted),
    [{L,Blk#b_blk{is=Is}}|Bs];
bsm_skip([], _) -> [].

bsm_skip_is([I0|Is], Extracted) ->
    case I0 of
        #b_set{anno=Anno0,
               op=bs_match,
               dst=Ctx,
               args=[#b_literal{val=T}=Type,PrevCtx|Args0]}
          when T =/= float, T =/= string, T =/= skip ->
            %% Note that it is never safe to skip matching
            %% of floats, even if the size is known to be correct.
            I = case sets:is_element(Ctx, Extracted) of
                    true ->
                        I0;
                    false ->
                        %% The value is never extracted.
                        Args = [#b_literal{val=skip},PrevCtx,Type|Args0],
                        Anno = maps:remove(arg_types, Anno0),
                        I0#b_set{anno=Anno,args=Args}
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

bsm_coalesce_skips([{L,Blk0}|Bs0], Renames0) ->
    case coalesce_skips({L,Blk0}, Bs0, Renames0) of
        not_possible ->
            [{L,Blk0}|bsm_coalesce_skips(Bs0, Renames0)];
        {Bs,Renames} ->
            bsm_coalesce_skips(Bs, Renames)
    end;
bsm_coalesce_skips([], _Renames) -> [].

coalesce_skips({L,#b_blk{is=[#b_set{op=bs_extract}=Extract|Is0],
                         last=Last0}=Blk0}, Bs0, Renames0) ->
    case coalesce_skips_is(Is0, Last0, Bs0, Renames0) of
        not_possible ->
            not_possible;
        {Is,Last,Bs,Renames} ->
            Blk = Blk0#b_blk{is=[Extract|Is],last=Last},
            {[{L,Blk}|Bs],Renames}
    end;
coalesce_skips({L,#b_blk{is=Is0,last=Last0}=Blk0}, Bs0, Renames0) ->
    case coalesce_skips_is(Is0, Last0, Bs0, Renames0) of
        not_possible ->
            not_possible;
        {Is,Last,Bs,Renames} ->
            Blk = Blk0#b_blk{is=Is,last=Last},
            {[{L,Blk}|Bs],Renames}
    end.

coalesce_skips_is([#b_set{op=bs_match,
                          args=[#b_literal{val=skip},
                                Ctx0,Type,Flags,
                                #b_literal{val=Size0},
                                #b_literal{val=Unit0}],
                          dst=PrevCtx}=Skip0,
                   #b_set{op={succeeded,guard}}],
                  #b_br{succ=L2,fail=Fail}=Br0,
                  Bs0,
                  Renames0) when is_integer(Size0) ->
    case Bs0 of
        [{L2,#b_blk{is=[#b_set{op=bs_match,
                               dst=SkipDst,
                               args=[#b_literal{val=skip},PrevCtx,_,_,
                                     #b_literal{val=Size1},
                                     #b_literal{val=Unit1}]},
                        #b_set{op={succeeded,guard}}=Succeeded],
                    last=#b_br{fail=Fail}=Br}}|Bs] when is_integer(Size1) ->
            OldCtx = maps:get(Ctx0, Renames0, Ctx0),
            SkipBits = Size0 * Unit0 + Size1 * Unit1,
            Skip = Skip0#b_set{dst=SkipDst,
                               args=[#b_literal{val=skip},OldCtx,
                                     Type,Flags,
                                     #b_literal{val=SkipBits},
                                     #b_literal{val=1}]},
            Is = [Skip,Succeeded],
            Renames = Renames0#{PrevCtx => Ctx0},
            {Is,Br,Bs,Renames};
        [{L2,#b_blk{is=[#b_set{op=bs_test_tail,
                               args=[PrevCtx,#b_literal{val=TailSkip}]}],
                    last=#b_br{succ=NextSucc,fail=Fail}}}|Bs] ->
            OldCtx = maps:get(Ctx0, Renames0, Ctx0),
            SkipBits = Size0 * Unit0,
            TestTail = Skip0#b_set{op=bs_test_tail,
                                   args=[OldCtx,#b_literal{val=SkipBits+TailSkip}]},
            Br = Br0#b_br{bool=TestTail#b_set.dst,succ=NextSucc},
            Is = [TestTail],
            Renames = Renames0#{PrevCtx => Ctx0},
            {Is,Br,Bs,Renames};
        _ ->
            not_possible
    end;
coalesce_skips_is(_, _, _, _) ->
    not_possible.

%%%
%%% Short-cutting binary matching instructions.
%%%

ssa_opt_bsm_shortcut({#opt_st{ssa=Linear0}=St, FuncDb}) ->
    Positions = bsm_positions(Linear0, #{}),
    case map_size(Positions) of
        0 ->
            %% No binary matching instructions.
            {St, FuncDb};
        _ ->
            Linear1 = bsm_shortcut(Linear0, Positions),
            Linear = bsm_tail(Linear1, #{}),
            ssa_opt_live({St#opt_st{ssa=Linear}, FuncDb})
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

bsm_shortcut([{L,#b_blk{is=Is,last=Last0}=Blk}|Bs], PosMap0) ->
    case {Is,Last0} of
        {[#b_set{op=bs_match,dst=New,args=[_,Old|_]},
          #b_set{op={succeeded,guard},dst=Bool,args=[New]}],
         #b_br{bool=Bool,fail=Fail}} ->
            case PosMap0 of
                #{Old := Bits,Fail := {TailBits,NextFail}} when Bits > TailBits ->
                    Last = Last0#b_br{fail=NextFail},
                    [{L,Blk#b_blk{last=Last}}|bsm_shortcut(Bs, PosMap0)];
                #{} ->
                    [{L,Blk}|bsm_shortcut(Bs, PosMap0)]
            end;
        {[#b_set{op=bs_test_tail,dst=Bool,args=[Old,#b_literal{val=TailBits}]}],
         #b_br{bool=Bool,succ=Succ,fail=Fail}} ->
            case PosMap0 of
                #{{bs_test_tail,Old,L} := ActualTailBits} ->
                    Last1 = if
                                TailBits =:= ActualTailBits ->
                                    Last0#b_br{fail=Succ};
                                true ->
                                    Last0#b_br{succ=Fail}
                            end,
                    Last = beam_ssa:normalize(Last1),
                    [{L,Blk#b_blk{last=Last}}|bsm_shortcut(Bs, PosMap0)];
                #{} ->
                    PosMap = PosMap0#{{bs_test_tail,Old,Succ} => TailBits},
                    [{L,Blk}|bsm_shortcut(Bs, PosMap)]
            end;
        {_,_} ->
            [{L,Blk}|bsm_shortcut(Bs, PosMap0)]
    end;
bsm_shortcut([], _PosMap) -> [].

%% Remove `bs_test_tail` instructions that are known to always
%% succeed, such as in the following example:
%%
%%     m(Bin) when is_binary(Bin) ->
%%        m1(Bin).
%%     m1(<<_, Rest/binary>>) -> m1(Rest);
%%     m1(<<>>) -> ok.
%%
%% The second clause of `m1/1` does not need to check for an empty
%% binary.

bsm_tail([{L,#b_blk{is=Is0,last=Last0}=Blk0}|Bs], Map0) ->
    {Is,Last,Map} = bsm_tail_is(Is0, Last0, L, Map0, []),
    Blk = Blk0#b_blk{is=Is,last=Last},
    [{L,Blk}|bsm_tail(Bs, Map)];
bsm_tail([], _Map) ->
    [].

bsm_tail_is([#b_set{op=bs_start_match,anno=Anno,dst=Dst}=I|Is], Last, L, Map0, Acc) ->
    case Anno of
        #{arg_types := #{1 := Type}} ->
            case beam_types:get_bs_matchable_unit(Type) of
                error ->
                    bsm_tail_is(Is, Last, L, Map0, [I|Acc]);
                Unit when is_integer(Unit) ->
                    Map = Map0#{Dst => Unit},
                    bsm_tail_is(Is, Last, L, Map, [I|Acc])
            end;
        #{} ->
            bsm_tail_is(Is, Last, L, Map0, [I|Acc])
    end;
bsm_tail_is([#b_set{op=bs_match,dst=Dst,args=Args},
             #b_set{op={succeeded,guard},dst=SuccDst,args=[Dst]}|_]=Is,
            #b_br{bool=SuccDst,fail=Fail}=Last,
            _L, Map0, Acc) ->
    case bsm_tail_num_matched(Args, Map0) of
        unknown ->
            %% Unknown number of bits or the match operation will fail
            %% to match certain values.
            Map = Map0#{Fail => unknown},
            {reverse(Acc, Is),Last,Map};
        Bits when is_integer(Bits) ->
            case Map0 of
                #{Fail := Bits} ->
                    {reverse(Acc, Is),Last,Map0};
                #{Fail := _} ->
                    Map = Map0#{Fail => unknown},
                    {reverse(Acc, Is),Last,Map};
                #{} ->
                    Map = Map0#{Fail => Bits},
                    {reverse(Acc, Is),Last,Map}
            end
    end;
bsm_tail_is([#b_set{op=bs_test_tail,args=[_,#b_literal{val=0}],dst=Dst}]=Is,
            #b_br{bool=Dst,succ=Succ}=Last0, L, Map0, Acc) ->
    case Map0 of
        #{L := Bits} when is_integer(Bits) ->
            %% The `bs_match` instruction targeting this block on failure
            %% will only fail when the end of the binary has been reached.
            %% There is no need for the test.
            Last = beam_ssa:normalize(Last0#b_br{fail=Succ}),
            {reverse(Acc, Is),Last,Map0};
        #{} ->
            {reverse(Acc, Is),Last0,Map0}
    end;
bsm_tail_is([#b_set{}=I|Is], Last, L, Map, Acc) ->
    bsm_tail_is(Is, Last, L, Map, [I|Acc]);
bsm_tail_is([], Last, _L, Map0, Acc) ->
    Map = foldl(fun(F, A) ->
                        A#{F => unknown}
                end, Map0, beam_ssa:successors(#b_blk{is=[],last=Last})),
    {reverse(Acc),Last,Map}.

bsm_tail_num_matched([#b_literal{val=skip},Ctx,Type,Flags,Size,Unit], Map) ->
    bsm_tail_num_matched([Type,Ctx,Flags,Size,Unit], Map);
bsm_tail_num_matched([#b_literal{val=Type},Ctx,#b_literal{},
                      #b_literal{val=Size},#b_literal{val=Unit}], Map)
  when (Type =:= integer orelse Type =:= binary),
       is_integer(Size), is_integer(Unit) ->
    Bits = Size * Unit,
    case Map of
        #{Ctx := Bits} when is_integer(Bits) ->
            Bits;
        #{} ->
            unknown
    end;
bsm_tail_num_matched(_Args, _Map) ->
    unknown.

%%%
%%% Optimize binary construction.
%%%
%%% If an integer segment or a float segment has a literal size and
%%% a literal value, convert to a binary segment. Coalesce adjacent
%%% literal binary segments. Literal binary segments will be converted
%%% to bs_put_string instructions in a later pass.
%%%

ssa_opt_bs_create_bin({#opt_st{ssa=Linear0}=St, FuncDb}) ->
    Linear = opt_create_bin_fs(Linear0),
    {St#opt_st{ssa=Linear}, FuncDb}.

opt_create_bin_fs([{L,#b_blk{is=Is0}=Blk0}|Bs]) ->
    Is = opt_create_bin_is(Is0),
    Blk = Blk0#b_blk{is=Is},
    [{L,Blk}|opt_create_bin_fs(Bs)];
opt_create_bin_fs([]) -> [].

opt_create_bin_is([#b_set{op=bs_create_bin,args=Args0}=I0|Is]) ->
    Args = opt_create_bin_args(Args0),
    I = I0#b_set{args=Args},
    [I|opt_create_bin_is(Is)];
opt_create_bin_is([I|Is]) ->
    [I|opt_create_bin_is(Is)];
opt_create_bin_is([]) -> [].

opt_create_bin_args([#b_literal{val=binary},#b_literal{val=[1|_]},
                     #b_literal{val=Bin0},#b_literal{val=all},
                     #b_literal{val=binary},#b_literal{val=[1|_]},
                     #b_literal{val=Bin1},#b_literal{val=all}|Args0])
  when is_bitstring(Bin0), is_bitstring(Bin1) ->
    %% Coalesce two litary binary segments to one.
    Bin = <<Bin0/bitstring,Bin1/bitstring>>,
    Args = [#b_literal{val=binary},#b_literal{val=[1]},
            #b_literal{val=Bin},#b_literal{val=all}|Args0],
    opt_create_bin_args(Args);
opt_create_bin_args([#b_literal{val=Type}=Type0,#b_literal{val=UFs}=UFs0,Val,Size|Args0]) ->
    [Unit|Flags] = UFs,
    case opt_create_bin_arg(Type, Unit, UFs, Val, Size) of
        not_possible ->
            [Type0,UFs0,Val,Size|opt_create_bin_args(Args0)];
        [Bin] when is_bitstring(Bin) ->
            Args = [#b_literal{val=binary},#b_literal{val=[1]},
                    #b_literal{val=Bin},#b_literal{val=all}|Args0],
            opt_create_bin_args(Args);
        [{int,Int,IntSize},Bin] when is_bitstring(Bin) ->
            Args = [#b_literal{val=integer},#b_literal{val=[1|Flags]},
                    #b_literal{val=Int},#b_literal{val=IntSize},
                    #b_literal{val=binary},#b_literal{val=[1]},
                    #b_literal{val=Bin},#b_literal{val=all}|Args0],
            opt_create_bin_args(Args)
    end;
opt_create_bin_args([]) -> [].

opt_create_bin_arg(binary, Unit, _Flags, #b_literal{val=Val}, #b_literal{val=all})
  when Unit =/= 1, bit_size(Val) rem Unit =:= 0 ->
    [Val];
opt_create_bin_arg(Type, Unit, Flags, #b_literal{val=Val}, #b_literal{val=Size})
  when is_integer(Size), is_integer(Unit) ->
    EffectiveSize = Size * Unit,
    if
        EffectiveSize > (1 bsl 24) ->
            %% Don't bother converting really huge segments as they might fail
            %% with a `system_limit` exception in runtime. Keeping them as-is
            %% ensures that the extended error information will be accurate.
            %%
            %% We'll also reduce the risk of crashing with an unhelpful "out of
            %% memory" error message during compilation.
            not_possible;
        EffectiveSize > 0, EffectiveSize =< (1 bsl 24) ->
            case {Type,opt_create_bin_endian(Flags)} of
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
                    opt_create_bin_arg(Type, 1, [], #b_literal{val=Int},
                                       #b_literal{val=EffectiveSize});
                {binary,_} when is_bitstring(Val) ->
                    case Val of
                        <<Bitstring:EffectiveSize/bits,_/bits>> ->
                            [Bitstring];
                        _ ->
                            %% Specified size exceeds size of bitstring.
                            not_possible
                    end;
                {float,Endian} ->
                    try
                        case Endian of
                            big ->
                                [<<Val:EffectiveSize/big-float-unit:1>>];
                            little ->
                                [<<Val:EffectiveSize/little-float-unit:1>>]
                        end
                    catch
                        error:_ ->
                            not_possible
                    end;
                {_,_} ->
                    not_possible
            end;
        true ->
            not_possible
    end;
opt_create_bin_arg(_, _, _, _, _) -> not_possible.

opt_create_bin_endian([little=E|_]) -> E;
opt_create_bin_endian([native=E|_]) -> E;
opt_create_bin_endian([_|Fs]) -> opt_create_bin_endian(Fs);
opt_create_bin_endian([]) -> big.

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
%%%   BoolVar1 = succeeded:guard Size
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
    Bool = #b_var{name=Count0+2},
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
                 #b_set{op={succeeded,_},dst=Bool,args=[Size]}],
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
            {Bool,Count} = new_var(Count0),
            IsEq = #b_set{op={bif,'=:='},dst=Bool,args=[Arg,Lit]},
            Br = #b_br{bool=Bool,succ=Lbl,fail=Fail},
            Blk = Blk0#b_blk{is=Is++[IsEq],last=Br},
            opt_sw(Bs, Count, [{L,Blk}|Acc]);
        #b_switch{arg=Arg,fail=Fail,
                  list=[{#b_literal{val=B1},Lbl},{#b_literal{val=B2},Lbl}]}
          when B1 =:= not B2 ->
            %% Replace with is_boolean test.
            {Bool,Count} = new_var(Count0),
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

%%% Try to replace `=/=` with `=:=` and `/=` with `==`. For example,
%%% this code:
%%%
%%%    Bool = bif:'=/=' Anything, AnyValue
%%%    br Bool, ^Succ, ^Fail
%%%
%%% can be rewritten like this:
%%%
%%%    Bool = bif:'=:=' Anything, AnyValue
%%%    br Bool, ^Fail, ^Succ
%%%
%%% The transformation is only safe if the only used of Bool is in the
%%% terminator. We therefore need to verify that there is only one
%%% use.
%%%
%%% This transformation is not an optimization in itself, but it opens
%%% up for other optimizations in beam_ssa_type and beam_ssa_dead.
%%%

ssa_opt_ne({#opt_st{ssa=Linear0}=St, FuncDb}) ->
    Linear = opt_ne(Linear0, {uses,Linear0}),
    {St#opt_st{ssa=Linear}, FuncDb}.

opt_ne([{L,#b_blk{is=[_|_]=Is0,last=#b_br{bool=#b_var{}=Bool}}=Blk0}|Bs], Uses0) ->
    case last(Is0) of
        #b_set{op={bif,'=/='},dst=Bool}=I0 ->
            I = I0#b_set{op={bif,'=:='}},
            {Blk,Uses} = opt_ne_replace(I, Blk0, Uses0),
            [{L,Blk}|opt_ne(Bs, Uses)];
        #b_set{op={bif,'/='},dst=Bool}=I0 ->
            I = I0#b_set{op={bif,'=='}},
            {Blk,Uses} = opt_ne_replace(I, Blk0, Uses0),
            [{L,Blk}|opt_ne(Bs, Uses)];
        _ ->
            [{L,Blk0}|opt_ne(Bs, Uses0)]
    end;
opt_ne([{L,Blk}|Bs], Uses) ->
    [{L,Blk}|opt_ne(Bs, Uses)];
opt_ne([], _Uses) -> [].

opt_ne_replace(#b_set{dst=Bool}=I,
               #b_blk{is=Is0,last=#b_br{succ=Succ,fail=Fail}=Br0}=Blk,
               Uses0) ->
    case opt_ne_single_use(Bool, Uses0) of
        {true,Uses} ->
            Is = replace_last(Is0, I),
            Br = Br0#b_br{succ=Fail,fail=Succ},
            {Blk#b_blk{is=Is,last=Br},Uses};
        {false,Uses} ->
            %% The variable is used more than once. Not safe.
            {Blk,Uses}
    end.

replace_last([_], Repl) -> [Repl];
replace_last([I|Is], Repl) -> [I|replace_last(Is, Repl)].

opt_ne_single_use(Var, {uses,Linear}) ->
    Blocks = maps:from_list(Linear),
    RPO = beam_ssa:rpo(Blocks),
    Uses = beam_ssa:uses(RPO, Blocks),
    opt_ne_single_use(Var, Uses);
opt_ne_single_use(Var, Uses) when is_map(Uses) ->
    {case Uses of
         #{Var:=[_]} -> true;
         #{Var:=[_|_]} -> false
     end,Uses}.

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
%%% However, there is one caveat to be aware of. Sinking tuple elements
%%% will keep the entire tuple alive longer. In rare circumstance, that
%%% can be a problem. Here is an example:
%%%
%%%    mapfoldl(F, Acc0, [Hd|Tail]) ->
%%%        {R,Acc1} = F(Hd, Acc0),
%%%        {Rs,Acc2} = mapfoldl(F, Acc1, Tail),
%%%        {[R|Rs],Acc2};
%%%    mapfoldl(F, Acc, []) ->
%%%        {[],Acc}.
%%%
%%% Sinking get_tuple_element instructions will generate code similar
%%% to this example:
%%%
%%%    slow_mapfoldl(F, Acc0, [Hd|Tail]) ->
%%%        Res1 = F(Hd, Acc0),
%%%        Res2 = slow_mapfoldl(F, element(2, Res1), Tail),
%%%        {[element(1, Res1)|element(1, Res2)],element(2, Res2)};
%%%    slow_mapfoldl(F, Acc, []) ->
%%%        {[],Acc}.
%%%
%%% Here the entire tuple bound to `Res1` will be kept alive until
%%% `slow_mapfoldl/3` returns. That is, every intermediate accumulator
%%% will be kept alive.
%%%
%%% In this case, not sinking is clearly superior:
%%%
%%%    fast_mapfoldl(F, Acc0, [Hd|Tail]) ->
%%%        Res1 = F(Hd, Acc0),
%%%        R = element(1, Res1),
%%%        Res2 = fast_mapfoldl(F, element(2, Res1), Tail),
%%%        {[R|element(1, Res2)],element(2, Res2)};
%%%    fast_mapfoldl(F, Acc, []) ->
%%%        {[],Acc}.
%%%
%%% The `slow_mapfoldl/3` and `fast_mapfoldl/3` use the same number of
%%% stack slots.
%%%
%%% To avoid producing code similar to `slow_mapfoldl/3`, use an
%%% heuristic to only sink when sinking would reduce the number of
%%% stack slots, or if there can't possibly be a recursive call
%%% involved. This is a heuristic because it is difficult to exactly
%%% predict the number of stack slots that will be needed for a given
%%% piece of code.

ssa_opt_sink({#opt_st{ssa=Linear}=St, FuncDb}) ->
    %% Create a map with all variables that define get_tuple_element
    %% instructions. The variable name maps to the block it is defined
    %% in and the source tuple.
    case def_blocks(Linear) of
        [] ->
            %% No get_tuple_element instructions, so there is nothing to do.
            {St, FuncDb};
        [_|_]=Defs0 ->
            Defs = maps:from_list(Defs0),
            {do_ssa_opt_sink(Defs, St), FuncDb}
    end.

do_ssa_opt_sink(Defs, #opt_st{ssa=Linear}=St) when is_map(Defs) ->
    %% Find all the blocks that use variables defined by
    %% get_tuple_element instructions.
    Used = used_blocks(Linear, Defs, []),

    %% Calculate dominators.
    Blocks0 = maps:from_list(Linear),
    RPO = beam_ssa:rpo(Blocks0),
    Preds = beam_ssa:predecessors(Blocks0),
    {Dom, Numbering} = beam_ssa:dominators_from_predecessors(RPO, Preds),

    %% It is not safe to move get_tuple_element instructions to blocks
    %% that begin with certain instructions. It is also unsafe to move
    %% the instructions into any part of a receive.
    Unsuitable = unsuitable(Linear, Blocks0, Preds),

    %% Calculate new positions for get_tuple_element instructions. The new
    %% position is a block that dominates all uses of the variable.
    DefLocs0 = new_def_locations(Used, Defs, Dom, Numbering, Unsuitable),

    %% Avoid keeping tuples alive if only one element is accessed later and
    %% if there is the chance of a recursive call being made. This is an
    %% important precaution to avoid that lists:mapfoldl/3 keeps all previous
    %% versions of the accumulator alive until the end of the input list.
    Ps = partition_deflocs(DefLocs0, Defs, Blocks0),
    DefLocs1 = filter_deflocs(Ps, Preds, Blocks0),
    DefLocs = sort(DefLocs1),

    %% Now move all suitable get_tuple_element instructions to their
    %% new blocks.
    Blocks = foldl(fun({V,{From,To}}, A) ->
                           move_defs(V, From, To, A)
                   end, Blocks0, DefLocs),

    St#opt_st{ssa=beam_ssa:linearize(Blocks)}.

def_blocks([{L,#b_blk{is=Is}}|Bs]) ->
    def_blocks_is(Is, L, def_blocks(Bs));
def_blocks([]) -> [].

def_blocks_is([#b_set{op=get_tuple_element,args=[Tuple,_],dst=Dst}|Is], L, Acc) ->
    def_blocks_is(Is, L, [{Dst,{L,Tuple}}|Acc]);
def_blocks_is([_|Is], L, Acc) ->
    def_blocks_is(Is, L, Acc);
def_blocks_is([], _, Acc) -> Acc.

used_blocks([{L,Blk}|Bs], Def, Acc0) ->
    Used = beam_ssa:used(Blk),
    Acc = [{V,L} || V <- Used, maps:is_key(V, Def)] ++ Acc0,
    used_blocks(Bs, Def, Acc);
used_blocks([], _Def, Acc) ->
    rel2fam(Acc).

%% Partition sinks for get_tuple_element instructions in the same
%% clause extracting from the same tuple. Sort each partition in
%% execution order.
partition_deflocs(DefLoc, _Defs, Blocks) ->
    {BlkNums0,_} = mapfoldl(fun(L, N) -> {{L,N},N+1} end, 0, beam_ssa:rpo(Blocks)),
    BlkNums = maps:from_list(BlkNums0),
    S = [{Tuple,{map_get(To, BlkNums),{V,{From,To}}}} ||
            {V,Tuple,{From,To}} <:- DefLoc],
    F = rel2fam(S),
    partition_deflocs_1(F, Blocks).

partition_deflocs_1([{Tuple,DefLocs0}|T], Blocks) ->
    DefLocs1 = [DL || {_,DL} <:- DefLocs0],
    DefLocs = partition_dl(DefLocs1, Blocks),
    [{Tuple,DL} || DL <- DefLocs] ++ partition_deflocs_1(T, Blocks);
partition_deflocs_1([], _) -> [].

partition_dl([_]=DefLoc, _Blocks) ->
    [DefLoc];
partition_dl([{_,{_,First}}|_]=DefLoc0, Blocks) ->
    RPO = beam_ssa:rpo([First], Blocks),
    {P,DefLoc} = partition_dl_1(DefLoc0, RPO, []),
    [P|partition_dl(DefLoc, Blocks)];
partition_dl([], _Blocks) -> [].

partition_dl_1([{_,{_,L}}=DL|DLs], [L|_]=Ls, Acc) ->
    partition_dl_1(DLs, Ls, [DL|Acc]);
partition_dl_1([_|_]=DLs, [_|Ls], Acc) ->
    partition_dl_1(DLs, Ls, Acc);
partition_dl_1([], _, Acc) ->
    {reverse(Acc),[]};
partition_dl_1([_|_]=DLs, [], Acc) ->
    {reverse(Acc),DLs}.

filter_deflocs([{Tuple,DefLoc0}|DLs], Preds, Blocks) ->
    %% Input is a list of sinks of get_tuple_element instructions in
    %% execution order from the same tuple in the same clause.
    [{_,{_,First}}|_] = DefLoc0,
    Paths = find_paths_to_check(DefLoc0, First),
    WillGC0 = ordsets:from_list([FromTo || {{_,_}=FromTo,_} <:- Paths]),
    WillGC = #{{From,To} => will_gc(From, To, Preds, Blocks, true) ||
                 {From,To} <:- WillGC0},

    %% Separate sinks that will force the reference to the tuple to be
    %% saved on the stack from sinks that don't force.
    {DefLocGC0,DefLocNoGC} =
        partition(fun({{_,_}=FromTo,_}) ->
                          map_get(FromTo, WillGC)
                  end, Paths),

    %% Avoid potentially harmful sinks.
    DefLocGC = filter_gc_deflocs(DefLocGC0, Tuple, First, Preds, Blocks),

    %% Construct the complete list of sink operations.
    DefLoc1 = DefLocGC ++ DefLocNoGC,
    [DL || {_,{_,{From,To}}=DL} <:- DefLoc1, From =/= To] ++
        filter_deflocs(DLs, Preds, Blocks);
filter_deflocs([], _, _) -> [].

%% Use an heuristic to avoid harmful sinking in lists:mapfold/3 and
%% similar functions.
filter_gc_deflocs(DefLocGC, Tuple, First, Preds, Blocks) ->
    case DefLocGC of
        [] ->
            [];
        [{_,{_,{From,To}}}] ->
            %% There is only one get_tuple_element instruction that
            %% can be sunk. That means that we may not gain any slots
            %% by sinking it.
            case is_on_stack(First, Tuple, Blocks) of
                true ->
                    %% The tuple itself must be stored in a stack slot
                    %% (because it will be used later) in addition to
                    %% the tuple element being extracted. It is
                    %% probably a win to sink this instruction.
                    DefLocGC;
                false ->
                    case will_gc(From, To, Preds, Blocks, false) of
                        false ->
                            %% There is no risk for recursive calls,
                            %% so it should be safe to
                            %% sink. Theoretically, we shouldn't win
                            %% any stack slots, but in practice it
                            %% seems that sinking makes it more likely
                            %% that the stack slot for a dying value
                            %% can be immediately reused for another
                            %% value.
                            DefLocGC;
                        true ->
                            %% This function could be involved in a
                            %% recursive call. Since there is no
                            %% obvious reduction in the number of
                            %% stack slots, we will not sink.
                            []
                    end
            end;
        [_,_|_] ->
            %% More than one get_tuple_element instruction can be
            %% sunk. Sinking will almost certainly reduce the number
            %% of stack slots.
            DefLocGC
    end.

find_paths_to_check([{_,{_,To}}=Move|T], First) ->
    [{{First,To},Move}|find_paths_to_check(T, First)];
find_paths_to_check([], _First) -> [].

will_gc(From, To, Preds, Blocks, All) ->
    Between = beam_ssa:between(From, To, Preds, Blocks),
    will_gc_1(Between, To, Blocks, All, #{From => false}).

will_gc_1([To|_], To, _Blocks, _All, WillGC) ->
    map_get(To, WillGC);
will_gc_1([L|Ls], To, Blocks, All, WillGC0) ->
    #b_blk{is=Is} = Blk = map_get(L, Blocks),
    GC = map_get(L, WillGC0) orelse will_gc_is(Is, All),
    WillGC = gc_update_successors(Blk, GC, WillGC0),
    will_gc_1(Ls, To, Blocks, All, WillGC).

will_gc_is([#b_set{op=call,args=Args}|Is], false) ->
    case Args of
        [#b_remote{mod=#b_literal{val=erlang}}|_] ->
            %% Assume that remote calls to the erlang module can't cause a recursive
            %% call.
            will_gc_is(Is, false);
        [_|_] ->
            true
    end;
will_gc_is([_|Is], false) ->
    %% Instructions that clobber X registers may cause a GC, but will not cause
    %% a recursive call.
    will_gc_is(Is, false);
will_gc_is([I|Is], All) ->
    beam_ssa:clobbers_xregs(I) orelse will_gc_is(Is, All);
will_gc_is([], _All) -> false.

is_on_stack(From, Var, Blocks) ->
    is_on_stack(beam_ssa:rpo([From], Blocks), Var, Blocks, #{From => false}).

is_on_stack([L|Ls], Var, Blocks, WillGC0) ->
    #b_blk{is=Is} = Blk = map_get(L, Blocks),
    GC0 = map_get(L, WillGC0),
    case is_on_stack_is(Is, Var, GC0) of
        {done,GC} ->
            GC;
        GC ->
            WillGC = gc_update_successors(Blk, GC, WillGC0),
            is_on_stack(Ls, Var, Blocks, WillGC)
    end;
is_on_stack([], _Var, _, _) -> false.

is_on_stack_is([#b_set{op=get_tuple_element}|Is], Var, GC) ->
    is_on_stack_is(Is, Var, GC);
is_on_stack_is([I|Is], Var, GC0) ->
    case GC0 andalso member(Var, beam_ssa:used(I)) of
        true ->
            {done,GC0};
        false ->
            GC = GC0 orelse beam_ssa:clobbers_xregs(I),
            is_on_stack_is(Is, Var, GC)
    end;
is_on_stack_is([], _, GC) -> GC.

gc_update_successors(Blk, GC, WillGC) ->
    foldl(fun(L, Acc) ->
                  case Acc of
                      #{L := true} -> Acc;
                      #{L := false} when GC =:= false -> Acc;
                      #{} -> Acc#{L => GC}
                  end
          end, WillGC, beam_ssa:successors(Blk)).

%% unsuitable(Linear, Blocks, Predecessors) -> Unsuitable.
%%  Return an gbset of block labels for the blocks that are not
%%  suitable for sinking of get_tuple_element instructions.

unsuitable(Linear, Blocks, Predecessors) when is_map(Blocks), is_map(Predecessors) ->
    Unsuitable0 = unsuitable_1(Linear),
    Unsuitable1 = unsuitable_recv(Linear, Blocks, Predecessors),
    gb_sets:from_list(Unsuitable0 ++ Unsuitable1).

unsuitable_1([{L,#b_blk{is=[#b_set{op=Op}=I|_]}}|Bs]) ->
    Unsuitable = case Op of
                     bs_extract -> true;
                     bs_match -> true;
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
    {DefIn,Tuple} = map_get(V, Defs),
    Common = common_dominator(UsedIn, Dom, Numbering, Unsuitable),
    Sink = case member(Common, map_get(DefIn, Dom)) of
               true ->
                   %% The common dominator is either DefIn or an
                   %% ancestor of DefIn. We'll need to consider all
                   %% get_tuple_element instructions so we will add
                   %% a dummy sink.
                   {V,Tuple,{DefIn,DefIn}};
               false ->
                   %% We have found a suitable descendant of DefIn,
                   %% to which the get_tuple_element instruction can
                   %% be sunk.
                   {V,Tuple,{DefIn,Common}}
           end,
    [Sink|new_def_locations(Vs, Defs, Dom, Numbering, Unsuitable)];
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
                  wait_timeout -> beyond;
                  _ -> here
              end,
    Action = case Is of
                 [#b_set{op={succeeded,_}}|_] -> here;
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
    GetIs1 = sort([{Pos,I} || #b_set{args=[_,Pos]}=I <:- GetIs0]),
    GetIs = [I || {_,I} <:- GetIs1],
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

ssa_opt_unfold_literals({St,FuncDb}) ->
    #opt_st{ssa=Blocks0,args=Args,anno=Anno} = St,
    true = is_map(Blocks0),                     %Assertion.
    ParamInfo = maps:get(parameter_info, Anno, #{}),
    LitMap = collect_arg_literals(Args, ParamInfo, 0, #{}),
    case map_size(LitMap) of
        0 ->
            %% None of the arguments for this function are known
            %% literals. Nothing to do.
            {St, FuncDb};
        _ ->
            SafeMap = #{0 => true},
            Blocks = unfold_literals(beam_ssa:rpo(Blocks0),
                                     LitMap, SafeMap, Blocks0),
            {St#opt_st{ssa=Blocks}, FuncDb}
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
collect_arg_literals([], _Info, _X, Acc) ->
    Acc.

unfold_literals([?EXCEPTION_BLOCK|Ls], LitMap, SafeMap, Blocks) ->
    unfold_literals(Ls, LitMap, SafeMap,Blocks);
unfold_literals([L|Ls], LitMap, SafeMap0, Blocks0) ->
    {Blocks,Safe} =
        case map_get(L, SafeMap0) of
            false ->
                %% Before reaching this block, an instruction that
                %% clobbers x registers has been executed. *If* we
                %% would use an argument variable instead of literal,
                %% it would force the value to be saved to a y
                %% register. This is not what we want.
                {Blocks0,false};
            true ->
                %% All x registers live when entering the function
                %% are still live. Using the variable instead of
                %% the substituted value will eliminate a `move`
                %% instruction.
                #b_blk{is=Is0} = Blk = map_get(L, Blocks0),
                {Is, Safe0} = unfold_lit_is(Is0, LitMap, []),
                {Blocks0#{ L := Blk#b_blk{is=Is} }, Safe0}
        end,
    %% Propagate safeness to successors.
    Successors = beam_ssa:successors(L, Blocks),
    SafeMap = unfold_update_succ(Successors, Safe, SafeMap0),
    unfold_literals(Ls, LitMap, SafeMap,Blocks);
unfold_literals([], _, _, Blocks) ->
    Blocks.

unfold_update_succ([S|Ss], Safe, SafeMap0) ->
    F = fun(Prev) -> Prev and Safe end,
    SafeMap = maps:update_with(S, F, Safe, SafeMap0),
    unfold_update_succ(Ss, Safe, SafeMap);
unfold_update_succ([], _, SafeMap) ->
    SafeMap.

unfold_lit_is([#b_set{op=match_fail,
                      args=[#b_literal{val=function_clause} | Args0]}=I0 | Is],
              LitMap, Acc) ->
    %% Undoing constant folding for this kind of failure lets us jump
    %% directly to the `func_info` instruction.
    Args = unfold_call_args(Args0, LitMap, 0),
    I = I0#b_set{args=[#b_literal{val=function_clause} | Args]},
    {reverse(Acc, [I | Is]), false};
unfold_lit_is([#b_set{op=Op,args=Args0}=I0|Is], LitMap, Acc) ->
    %% Using a register instead of a literal is a clear win only for
    %% `call` instructions. Substituting into other instructions is
    %% unlikely to be an improvement.
    Unfold = case Op of
                 call -> true;
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
            {reverse(Acc, [I|Is]), false};
        false ->
            unfold_lit_is(Is, LitMap, [I|Acc])
    end;
unfold_lit_is([], _LitMap, Acc) ->
    {reverse(Acc), true}.

unfold_call_args([A0|As], LitMap, X) ->
    A = unfold_arg(A0, LitMap, X),
    [A | unfold_call_args(As, LitMap, X + 1)];
unfold_call_args([], _, _) ->
    [].

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
unfold_arg(Expr, _LitMap, _X) ->
    Expr.

%%%
%%% Restore tail calls that were damaged by optimizations.
%%%
%%% Consider the following example of a tail call in Erlang code:
%%%
%%%    bar() ->
%%%        foo().
%%%
%%% The SSA code for the call will look like this:
%%%
%%%      @ssa_result = call (`foo`/0)
%%%      @ssa_bool = succeeded:body @ssa_result
%%%      br @ssa_bool, ^999, ^1
%%%
%%%    999:
%%%      ret @ssa_result
%%%
%%% Now imagine that an optimization has figured out that `foo/0` always
%%% returns the atom `ok` and substituted the result everywhere, resulting in
%%% the following SSA:
%%%
%%%      %% Result type: `ok`
%%%      @ssa_ignored = call (`foo`/0)
%%%      @ssa_bool = succeeded:body @ssa_ignored
%%%      br @ssa_bool, ^999, ^1
%%%
%%%    999:
%%%      ret `ok`
%%%
%%% The `beam_ssa_pre_codegen` pass will not recognize this code as a tail call
%%% and will generate an unnecessary stack frame, and may also generate
%%% unnecessary `kill` instructions.
%%%
%%% The beam_jump pass does the same optimization, but it does it too
%%% late to avoid creating an uncessary stack frame or unnecessary
%%% `kill` instructions.
%%%

ssa_opt_tail_literals({St,FuncDb}) ->
    #opt_st{cnt=Count0,ssa=Blocks0} = St,
    true = is_map(Blocks0),                     %Assertion.
    {Count, Blocks} = opt_tail_literals(beam_ssa:rpo(Blocks0), Count0, Blocks0),
    {St#opt_st{cnt=Count,ssa=Blocks},FuncDb}.

opt_tail_literals([L | Ls], Count, Blocks0) ->
    #b_blk{is=Is0,last=Last} = Blk0 = map_get(L, Blocks0),

    case is_tail_literal(Is0, Last, Blocks0) of
        {yes, Var} ->
            %% Yes, this is a call followed by a block returning the same value
            %% as the call itself. Create a new block that returns the result
            %% directly, as the successor block may be reachable from
            %% elsewhere.
            RetBlk = #b_blk{is=[],last=#b_ret{arg=Var}},
            RetLbl = Count,

            Blk = Blk0#b_blk{last=Last#b_br{succ=RetLbl}},

            Blocks = Blocks0#{ L := Blk, RetLbl => RetBlk },
            opt_tail_literals(Ls, Count + 1, Blocks);
        no ->
            opt_tail_literals(Ls, Count, Blocks0)
    end;
opt_tail_literals([], Count, Blocks) ->
    {Count, Blocks}.

is_tail_literal([#b_set{op=call,dst=Dst}=Call,
                 #b_set{op={succeeded,body},dst=Bool}],
                #b_br{bool=#b_var{}=Bool,succ=Succ}, Blocks) ->
    case Blocks of
        #{ Succ := #b_blk{is=[],last=#b_ret{arg=#b_literal{val=Val}}} } ->
            %% Our success block does nothing but return a literal. Now we'll
            %% check whether it's the same literal as the one returned by the
            %% call itself.
            Type = beam_ssa:get_anno(result_type, Call, any),
            case beam_types:get_singleton_value(Type) of
                {ok, Val} -> {yes, Dst};
                _ -> no
            end;
        #{} ->
            no
    end;
is_tail_literal([_ | Is], #b_br{}=Last, Blocks) ->
    is_tail_literal(Is, Last, Blocks);
is_tail_literal(_Is, _Last, _Blocks) ->
    no.

%%%
%%% Eliminate redundant branches.
%%%
%%% Redundant `br` instructions following calls to guard BIFs such as:
%%%
%%%     @bif_result = bif:Bif ...
%%%     br @bif_result, ^100, ^200
%%%
%%%   100:
%%%      ret `true`
%%%
%%%   200:
%%%      ret `false`
%%%
%%% can can be rewritten to:
%%%
%%%     @bif_result = bif:Bif ...
%%%     ret @bif_result
%%%
%%% A similar rewriting is possible if the true and false branches end
%%% up at a phi node.
%%%
%%% A code sequence such as:
%%%
%%%   @ssa_bool = bif:'=:=' Var, Other
%%%   br @ssa_bool, ^100, ^200
%%%
%%% 100:
%%%   ret Other
%%%
%%% 200:
%%%   ret Var
%%%
%%% can be rewritten to:
%%%
%%%   ret Var
%%%

ssa_opt_redundant_br({#opt_st{ssa=Blocks0}=St, FuncDb}) when is_map(Blocks0) ->
    Blocks = redundant_br(beam_ssa:rpo(Blocks0), Blocks0),
    {St#opt_st{ssa=Blocks}, FuncDb}.

redundant_br([L|Ls], Blocks0) ->
    Blk0 = map_get(L, Blocks0),
    case Blk0 of
        #b_blk{is=Is,
               last=#b_br{bool=#b_var{}=Bool,
                          succ=Succ,
                          fail=Fail}} ->
            case Blocks0 of
                #{Succ := #b_blk{is=[],last=#b_ret{arg=#b_literal{val=true}}},
                  Fail := #b_blk{is=[],last=#b_ret{arg=#b_literal{val=false}}}} ->
                    case redundant_br_safe_bool(Is, Bool) of
                        true ->
                            Blk = Blk0#b_blk{last=#b_ret{arg=Bool}},
                            Blocks = Blocks0#{L => Blk},
                            redundant_br(Ls, Blocks);
                        false ->
                            redundant_br(Ls, Blocks0)
                    end;
                #{Succ := #b_blk{is=[],last=#b_br{succ=PhiL,fail=PhiL}},
                  Fail := #b_blk{is=[],last=#b_br{succ=PhiL,fail=PhiL}}} ->
                    case redundant_br_safe_bool(Is, Bool) of
                        true ->
                            Blocks = redundant_br_phi(L, Blk0, PhiL, Blocks0),
                            redundant_br(Ls, Blocks);
                        false ->
                            redundant_br(Ls, Blocks0)
                    end;
                #{Succ := #b_blk{is=[],last=#b_ret{arg=Other}},
                  Fail := #b_blk{is=[],last=#b_ret{arg=Var}}} when Is =/= [] ->
                    case last(Is) of
                        #b_set{op={bif,'=:='},args=[Var,Other]} ->
                            Blk = Blk0#b_blk{is=droplast(Is),
                                             last=#b_ret{arg=Var}},
                            Blocks = Blocks0#{L => Blk},
                            redundant_br(Ls, Blocks);
                        #b_set{} ->
                            redundant_br(Ls, Blocks0)
                    end;
                #{} ->
                    redundant_br(Ls, Blocks0)
            end;
        _ ->
            redundant_br(Ls, Blocks0)
    end;
redundant_br([], Blocks) -> Blocks.

redundant_br_phi(L, Blk0, PhiL, Blocks) ->
    #b_blk{is=Is0} = PhiBlk0 = map_get(PhiL, Blocks),
    case Is0 of
        [#b_set{op=phi},#b_set{op=phi}|_] ->
            Blocks;
        [#b_set{op=phi,args=PhiArgs0}=I0|Is] ->
            #b_blk{last=#b_br{succ=Succ,fail=Fail}} = Blk0,
            BoolPhiArgs = [{#b_literal{val=false},Fail},
                           {#b_literal{val=true},Succ}],
            PhiArgs1 = ordsets:from_list(PhiArgs0),
            case ordsets:is_subset(BoolPhiArgs, PhiArgs1) of
                true ->
                    #b_blk{last=#b_br{bool=Bool}} = Blk0,
                    PhiArgs = ordsets:add_element({Bool,L}, PhiArgs1),
                    I = I0#b_set{args=PhiArgs},
                    PhiBlk = PhiBlk0#b_blk{is=[I|Is]},
                    Br = #b_br{bool=#b_literal{val=true},succ=PhiL,fail=PhiL},
                    Blk = Blk0#b_blk{last=Br},
                    Blocks#{L := Blk, PhiL := PhiBlk};
                false ->
                    Blocks
            end
    end.

redundant_br_safe_bool([], _Bool) ->
    true;
redundant_br_safe_bool(Is, Bool) ->
    case last(Is) of
        #b_set{op={bif,_}} -> true;
        #b_set{op=has_map_field} -> true;
        #b_set{dst=Dst} -> Dst =/= Bool
    end.

%%%
%%% Add the `bs_ensure` instruction before a sequence of `bs_match`
%%% (SSA) instructions, each having a literal size and the
%%% same failure label.
%%%
%%% This is the first part of building the `bs_match` (BEAM)
%%% instruction that can match multiple segments having the same
%%% failure label.
%%%
%%% It is beneficial but not essential to run this pass after
%%% the `merge_blocks/1` pass. For the following example, two separate
%%% `bs_match/1` instructions will emitted if blocks have not been
%%% merged before this pass:
%%%
%%%    A = 0,
%%%    B = <<1, 2, 3>>,
%%%    <<A, B:(byte_size(B))/binary>> = <<0, 1, 2, 3>>
%%%

ssa_opt_bs_ensure({#opt_st{ssa=Blocks0,cnt=Count0,anno=Anno0}=St, FuncDb})
  when is_map(Blocks0) ->
    RPO = beam_ssa:rpo(Blocks0),
    Seen = sets:new([{version,2}]),
    {Blocks,Count} = ssa_opt_bs_ensure(RPO, Seen, Count0, Blocks0),
    Anno = Anno0#{bs_ensure_opt => true},
    {St#opt_st{ssa=Blocks,cnt=Count,anno=Anno}, FuncDb}.

ssa_opt_bs_ensure([L|Ls], Seen0, Count0, Blocks0) ->
    case sets:is_element(L, Seen0) of
        true ->
            %% This block is already covered by a `bs_ensure`
            %% instruction.
            ssa_opt_bs_ensure(Ls, Seen0, Count0, Blocks0);
        false ->
            case is_bs_match_blk(L, Blocks0) of
                no ->
                    ssa_opt_bs_ensure(Ls, Seen0, Count0, Blocks0);
                {yes,Size0,#b_br{succ=Succ,fail=Fail}} ->
                    {Size,Blocks1,Seen} =
                        ssa_opt_bs_ensure_collect(Succ, Fail,
                                                  Blocks0, Seen0, Size0),
                    Blocks2 = annotate_match(L, Blocks1),
                    {Blocks,Count} = build_bs_ensure_match(L, Size, Count0, Blocks2),
                    ssa_opt_bs_ensure(Ls, Seen, Count, Blocks)
            end
    end;
ssa_opt_bs_ensure([], _Seen, Count, Blocks) ->
    {Blocks,Count}.

ssa_opt_bs_ensure_collect(L, Fail, Blocks0, Seen0, Acc0) ->
    case is_bs_match_blk(L, Blocks0) of
        no ->
            {Acc0,Blocks0,Seen0};
        {yes,Size,#b_br{succ=Succ,fail=Fail}} ->
            case update_size(Size, Acc0) of
                no ->
                    {Acc0,Blocks0,Seen0};
                Acc ->
                    Seen = sets:add_element(L, Seen0),
                    Blocks = annotate_match(L, Blocks0),
                    ssa_opt_bs_ensure_collect(Succ, Fail, Blocks, Seen, Acc)
            end;
        {yes,_,_} ->
            {Acc0,Blocks0,Seen0}
    end.

annotate_match(L, Blocks) ->
    #b_blk{is=Is0} = Blk0 = map_get(L, Blocks),
    Is = [case I of
              #b_set{op=bs_match} ->
                  beam_ssa:add_anno(ensured, true, I);
              #b_set{} ->
                  I
          end || I <- Is0],
    Blk = Blk0#b_blk{is=Is},
    Blocks#{L := Blk}.

update_size({{PrevCtx,NewCtx},Size,Unit}, {{_,PrevCtx},Sum,Unit0}) ->
    {{PrevCtx,NewCtx},Sum + Size,max(Unit, Unit0)};
update_size(_, _) ->
    no.

is_bs_match_blk(L, Blocks) ->
    Blk = map_get(L, Blocks),
    case Blk of
        #b_blk{is=Is,last=#b_br{bool=#b_var{}}=Last} ->
            case is_bs_match_is(Is, true) of
                no ->
                    no;
                {yes,CtxSizeUnit} ->
                    {yes,CtxSizeUnit,Last}
            end;
        #b_blk{} ->
            no
    end.

is_bs_match_is([#b_set{op=bs_match,dst=Dst}=I,
                #b_set{op={succeeded,guard},args=[Dst]}], Safe) ->
    case Safe of
        false ->
            %% This `bs_match` (SSA) instruction was preceded by other
            %% instructions (such as guard BIF calls) that would
            %% prevent this match operation to be incorporated into
            %% the commands list of a `bs_match` (BEAM) instruction.
            no;
        true ->
            case is_viable_match(I) of
                no ->
                    no;
                {yes,{Ctx,Size,Unit}} when Size bsr 24 =:= 0 ->
                    %% Only include matches of reasonable size.
                    {yes,{{Ctx,Dst},Size,Unit}};
                {yes,_} ->
                    %% Too large size.
                    no
            end
    end;
is_bs_match_is([#b_set{op=bs_extract}|Is], Safe) ->
    is_bs_match_is(Is, Safe);
is_bs_match_is([#b_set{op=bs_start_match}|Is], _Safe) ->
    is_bs_match_is(Is, true);
is_bs_match_is([_|Is], _Safe) ->
    is_bs_match_is(Is, false);
is_bs_match_is([], _Safe) -> no.

is_viable_match(#b_set{op=bs_match,args=Args}) ->
    case Args of
        [#b_literal{val=binary},Ctx,_,#b_literal{val=all},#b_literal{val=U}]
          when is_integer(U), 1 =< U, U =< 256 ->
            {yes,{Ctx,0,U}};
        [#b_literal{val=binary},Ctx,_,#b_literal{val=Size},#b_literal{val=U}]
          when is_integer(Size) ->
            {yes,{Ctx,Size*U,1}};
        [#b_literal{val=integer},Ctx,_,#b_literal{val=Size},#b_literal{val=U}]
          when is_integer(Size) ->
            {yes,{Ctx,Size*U,1}};
        [#b_literal{val=skip},Ctx,_,_,#b_literal{val=all},#b_literal{val=U}] ->
            {yes,{Ctx,0,U}};
        [#b_literal{val=skip},Ctx,_,_,#b_literal{val=Size},#b_literal{val=U}]
          when is_integer(Size) ->
            {yes,{Ctx,Size*U,1}};
        [#b_literal{val=string},Ctx,#b_literal{val=Str}] when bit_size(Str) =< 64 ->
            {yes,{Ctx,bit_size(Str),1}};
        _ ->
            no
    end.

build_bs_ensure_match(L, {_,Size,Unit}, Count0, Blocks0) ->
    BsMatchL = Count0,
    Count1 = Count0 + 1,
    {NewCtx,Count2} = new_var(Count1),
    {SuccBool,Count} = new_var(Count2),

    BsMatchBlk0 = map_get(L, Blocks0),

    #b_blk{is=MatchIs,last=#b_br{fail=Fail}} = BsMatchBlk0,
    {Prefix,Suffix0} = splitwith(fun(#b_set{op=Op}) -> Op =/= bs_match end, MatchIs),
    [BsMatch0|Suffix1] = Suffix0,
    #b_set{args=[Type,_Ctx|Args]} = BsMatch0,
    BsMatch = BsMatch0#b_set{args=[Type,NewCtx|Args]},
    Suffix = [BsMatch|Suffix1],
    BsMatchBlk = BsMatchBlk0#b_blk{is=Suffix},

    #b_set{args=[_,Ctx|_]} = keyfind(bs_match, #b_set.op, MatchIs),
    Is = Prefix ++ [#b_set{op=bs_ensure,
                           dst=NewCtx,
                           args=[Ctx,#b_literal{val=Size},#b_literal{val=Unit}]},
                    #b_set{op={succeeded,guard},dst=SuccBool,args=[NewCtx]}],
    Blk = #b_blk{is=Is,last=#b_br{bool=SuccBool,succ=BsMatchL,fail=Fail}},

    Blocks = Blocks0#{L := Blk, BsMatchL => BsMatchBlk},

    {Blocks,Count}.

%%%
%%% Change the `reuse` hint to `copy` when it is highly probable that
%%% reuse will not happen.
%%%

ssa_opt_no_reuse({#opt_st{ssa=Linear0}=St, FuncDb}) when is_list(Linear0) ->
    New = sets:new([{version,2}]),
    Linear = ssa_opt_no_reuse_blks(Linear0, New),
    {St#opt_st{ssa=Linear}, FuncDb}.

ssa_opt_no_reuse_blks([{L,#b_blk{is=Is0}=Blk0}|Bs], New0) ->
    {Is,New} = ssa_opt_no_reuse_is(Is0, New0, []),
    Blk = Blk0#b_blk{is=Is},
    [{L,Blk}|ssa_opt_no_reuse_blks(Bs, New)];
ssa_opt_no_reuse_blks([], _) ->
    [].

ssa_opt_no_reuse_is([#b_set{op=update_record,args=Args}=I0|Is], New, Acc) ->
    [_,_,_|Updates] = Args,
    case cannot_reuse(Updates, New) of
        true ->
            I = I0#b_set{args=[#b_literal{val=copy}|tl(Args)]},
            ssa_opt_no_reuse_is(Is, New, [I|Acc]);
        false ->
            ssa_opt_no_reuse_is(Is, New, [I0|Acc])
    end;
ssa_opt_no_reuse_is([#b_set{dst=Dst}=I|Is], New0, Acc) ->
    case inhibits_reuse(I, New0) of
        true ->
            New = sets:add_element(Dst, New0),
            ssa_opt_no_reuse_is(Is, New, [I|Acc]);
        false ->
            ssa_opt_no_reuse_is(Is, New0, [I|Acc])
    end;
ssa_opt_no_reuse_is([], New, Acc) ->
    {reverse(Acc),New}.

inhibits_reuse(#b_set{op=phi,args=Args}, New) ->
    all(fun({Value,_}) ->
                   sets:is_element(Value, New)
           end, Args);
inhibits_reuse(#b_set{op=put_map,args=[_|Args]}, New) ->
    cannot_reuse(Args, New);
inhibits_reuse(#b_set{op=call,
                      args=[#b_remote{mod=#b_literal{val=erlang},
                                      name=#b_literal{val=Name}}|_]},
               _New) ->
    case Name of
        '++' -> true;
        '--' -> true;
        atom_to_list -> true;
        atom_to_binary -> true;
        list_to_tuple -> true;
        make_ref -> true;
        monitor -> true;
        setelement -> true;
        send_after -> true;
        spawn -> true;
        spawn_link -> true;
        spawn_monitor -> true;
        tuple_to_list -> true;
        _ -> false
    end;
inhibits_reuse(#b_set{op={bif,Arith},args=[#b_var{},#b_literal{}]}, _New)
  when Arith =:= '+'; Arith =:= '-' ->
    %% This is probably a counter in a record being updated. (Heuristic,
    %% but with a high probability of being correct).
    true;
inhibits_reuse(#b_set{op=Op}, _New) ->
    case Op of
        bs_create_bin -> true;
        bs_get_tail -> true;
        make_fun -> true;
        put_list -> true;
        put_tuple -> true;
        _ -> false
    end.

cannot_reuse([V|Values], New) ->
    sets:is_element(V, New) orelse cannot_reuse(Values, New);
cannot_reuse([], _New) ->
    false.

%%%
%%% Common utilities.
%%%

list_set_union([], Set) ->
    Set;
list_set_union([E], Set) ->
    sets:add_element(E, Set);
list_set_union(List, Set) ->
    sets:union(sets:from_list(List), Set).

non_guards(Linear) ->
    gb_sets:from_list(non_guards_1(Linear)).

non_guards_1([{L,#b_blk{is=Is}}|Bs]) ->
    case Is of
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
    I#b_set{args=[{sub_arg(A, Sub),P} || {A,P} <:- Args]};
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

new_var(Count) ->
    {#b_var{name=Count},Count+1}.

%%%
%%% NIF handling
%%%
%%% NIFs are problematic for the SSA optimization passes as when a
%%% loaded NIF replaces a function, essentially all bets are off as
%%% callers of the NIF cannot make any assumptions on the result of
%%% calling the NIF.
%%%
%%% A safe way to handle NIFs, but still allow optimization of
%%% functions not calling NIFs is to make calls to NIFs look like
%%% external calls. For the beam_ssa_opt compiler pass this is handled
%%% by the functions isolate_nifs/1 and restore_nifs/2.
%%%
%%% The function isolate_nifs/1 transforms the input #b_module{} by
%%% rewriting all calls to NIFs in the module to calls to external
%%% functions with the same names.  As this also removes all callers
%%% to the NIF, all non-exported NIFs are forcibly exported, to avoid
%%% them being removed as dead code.
%%%
%%% As all passes know how handle external calls, this allows for safe
%%% optimization of the module. That a NIF function can contain BEAM
%%% code which calls other functions in the module is not a problem,
%%% at worst it leads to missed optimizations.
%%%
%%% When all sub-passes of beam_ssa_opt have been executed
%%% restore_nifs/2 undoes the module transforms done by
%%% isolate_nifs/1. To avoid extra book-keeping to keep track of
%%% rewritten calls for use by restore_nifs/2, the module to which the
%%% calls are redirected is given a name, '\nnifs', which cannot be
%%% created by the user.
%%%
-define(ISOLATION_MODULE, #b_literal{val='\nnifs'}).

isolate_nifs(#b_module{body=Body0, exports=Exports0}=Module0) ->
    %% Scan to find NIFs
    NIFs = foldl(fun(#b_function{}=F, Acc) ->
                         case is_nif(F) of
                             true ->
                                 sets:add_element(get_func_id(F), Acc);
                             false ->
                                 Acc
                         end
                 end, sets:new([{version,2}]), Body0),

    %% Determine the set of previously not exported NIFs which should
    %% be exported.
    ExportsSet = foldl(fun({N,A}, Acc) ->
                               FA = #b_local{name=#b_literal{val=N},arity=A},
                               sets:add_element(FA, Acc)
                       end, sets:new([{version,2}]), Exports0),
    NIFsToExport = sets:subtract(NIFs, ExportsSet),
    Exports = Exports0 ++ [{N,A}
                           || #b_local{name=#b_literal{val=N},arity=A}
                                  <- sets:to_list(NIFsToExport)],

    %% Replace all calls to the NIFs with a call to an external
    %% function with the same name, but with a module name which
    %% cannot be created by the user ('\nnifs').
    CallReplacer =
        fun(#b_set{op=call,args=[#b_local{name=N,arity=A}=Callee|Rest]}=I)->
                case sets:is_element(Callee, NIFs) of
                    true ->
                        Args = [#b_remote{mod=?ISOLATION_MODULE,
                                          name=N,arity=A}|Rest],
                        I#b_set{args=Args};
                    false ->
                        I
                end;
           (I) ->
                I
        end,
    #b_module{body=Body} = map_module_instrs(CallReplacer, Module0),
    NIFsAsExternal = sets:fold(fun(#b_local{name=N,arity=A}, Acc) ->
                                       R = #b_remote{mod=?ISOLATION_MODULE,
                                                     name=N,arity=A},
                                       sets:add_element(R, Acc)
                               end, sets:new([{version,2}]), NIFs),
    {Module0#b_module{exports=Exports,body=Body},
     {NIFsToExport, NIFsAsExternal}}.

map_module_instrs(Fun, #b_module{body=Body}=Module) ->
    Module#b_module{body=[map_module_instrs_f(Fun, F) || F <- Body]}.

map_module_instrs_f(Fun, #b_function{bs=Bs}=F) ->
    F#b_function{bs=#{Lbl => map_module_instrs_b(Fun, Blk) || Lbl:=Blk <- Bs}}.

map_module_instrs_b(Fun, #b_blk{is=Is}=Blk) ->
    Blk#b_blk{is=[Fun(I) || I <- Is]}.

restore_nifs(#b_module{exports=Exports0}=Module0, {NIFsToExport, NIFs}) ->
    %% Remove the NIFs which where were forcibly exported by
    %% isolate_nifs/1 from the export list.
    Exports = [E
               || {N,A}=E <:- Exports0,
                  not sets:is_element(#b_local{name=#b_literal{val=N},
                                               arity=A}, NIFsToExport)],

    %% Restore all calls that were turned into calls to external
    %% functions in the '\nnifs' module by converting them to local
    %% calls.
    CallRestorer =
        fun(#b_set{op=call,args=[#b_remote{name=N,arity=A}=Callee|Rest]}=I)->
                case sets:is_element(Callee, NIFs) of
                    true ->
                        I#b_set{args=[#b_local{name=N,arity=A}|Rest]};
                    false ->
                        I
                end;
           (I) ->
                I
        end,
    #b_module{body=Body} = map_module_instrs(CallRestorer, Module0),
    Module0#b_module{exports=Exports,body=Body}.

%%%
%%% Predicate to check if a function is the stub for a nif.
%%%
is_nif(#b_function{bs=#{0:=#b_blk{is=[#b_set{op=nif_start}|_]}}}) ->
    true;
is_nif(_) ->
    false.
