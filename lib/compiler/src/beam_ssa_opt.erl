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

-module(beam_ssa_opt).
-export([module/2]).

-include("beam_ssa.hrl").
-import(lists, [all/2,append/1,foldl/3,keyfind/3,member/2,
                reverse/1,reverse/2,
                splitwith/2,takewhile/2,unzip/1]).

-spec module(beam_ssa:b_module(), [compile:option()]) ->
                    {'ok',beam_ssa:b_module()}.

module(#b_module{body=Fs0}=Module, Opts) ->
    Ps = passes(Opts),
    Fs = functions(Fs0, Ps),
    {ok,Module#b_module{body=Fs}}.

functions([F|Fs], Ps) ->
    [function(F, Ps)|functions(Fs, Ps)];
functions([], _Ps) -> [].

-type b_blk() :: beam_ssa:b_blk().
-type b_var() :: beam_ssa:b_var().
-type label() :: beam_ssa:label().

-record(st, {ssa :: beam_ssa:block_map() | [{label(),b_blk()}],
             args :: [b_var()],
             cnt :: label()}).
-define(PASS(N), {N,fun N/1}).

passes(Opts0) ->
    Ps = [?PASS(ssa_opt_split_blocks),
          ?PASS(ssa_opt_coalesce_phis),
          ?PASS(ssa_opt_element),
          ?PASS(ssa_opt_linearize),
          ?PASS(ssa_opt_record),
          ?PASS(ssa_opt_setelement),

          %% Run ssa_opt_cse twice, because it will help ssa_opt_dead,
          %% and ssa_opt_dead will help ssa_opt_cse. Run ssa_opt_live
          %% twice, because it will help ssa_opt_dead and ssa_opt_dead
          %% will help ssa_opt_live.
          ?PASS(ssa_opt_cse),
          ?PASS(ssa_opt_type),
          ?PASS(ssa_opt_live),
          ?PASS(ssa_opt_dead),
          ?PASS(ssa_opt_cse),                   %Second time.
          ?PASS(ssa_opt_float),
          ?PASS(ssa_opt_live),                  %Second time.

          ?PASS(ssa_opt_bsm),
          ?PASS(ssa_opt_bsm_units),
          ?PASS(ssa_opt_bsm_shortcut),
          ?PASS(ssa_opt_misc),
          ?PASS(ssa_opt_tuple_size),
          ?PASS(ssa_opt_sw),
          ?PASS(ssa_opt_blockify),
          ?PASS(ssa_opt_sink),
          ?PASS(ssa_opt_merge_blocks),
          ?PASS(ssa_opt_trim_unreachable)],
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

function(#b_function{anno=Anno,bs=Blocks0,args=Args,cnt=Count0}=F, Ps) ->
    try
        St = #st{ssa=Blocks0,args=Args,cnt=Count0},
        #st{ssa=Blocks,cnt=Count} = compile:run_sub_passes(Ps, St),
        F#b_function{bs=Blocks,cnt=Count}
    catch
        Class:Error:Stack ->
            #{func_info:={_,Name,Arity}} = Anno,
            io:fwrite("Function: ~w/~w\n", [Name,Arity]),
            erlang:raise(Class, Error, Stack)
    end.

%%%
%%% Trivial sub passes.
%%%

ssa_opt_dead(#st{ssa=Linear}=St) ->
    St#st{ssa=beam_ssa_dead:opt(Linear)}.

ssa_opt_linearize(#st{ssa=Blocks}=St) ->
    St#st{ssa=beam_ssa:linearize(Blocks)}.

ssa_opt_type(#st{ssa=Linear,args=Args}=St) ->
    St#st{ssa=beam_ssa_type:opt(Linear, Args)}.

ssa_opt_blockify(#st{ssa=Linear}=St) ->
    St#st{ssa=maps:from_list(Linear)}.

ssa_opt_trim_unreachable(#st{ssa=Blocks}=St) ->
    St#st{ssa=beam_ssa:trim_unreachable(Blocks)}.

%%%
%%% Split blocks before certain instructions to enable more optimizations.
%%%
%%% Splitting before element/2 enables the optimization that swaps
%%% element/2 instructions.
%%%
%%% Splitting before call and make_fun instructions gives more opportunities
%%% for sinking get_tuple_element instructions.
%%%

ssa_opt_split_blocks(#st{ssa=Blocks0,cnt=Count0}=St) ->
    P = fun(#b_set{op={bif,element}}) -> true;
           (#b_set{op=call}) -> true;
           (#b_set{op=make_fun}) -> true;
           (_) -> false
        end,
    {Blocks,Count} = beam_ssa:split_blocks(P, Blocks0, Count0),
    St#st{ssa=Blocks,cnt=Count}.

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

ssa_opt_coalesce_phis(#st{ssa=Blocks0}=St) ->
    Ls = beam_ssa:rpo(Blocks0),
    Blocks = c_phis_1(Ls, Blocks0),
    St#st{ssa=Blocks}.

c_phis_1([L|Ls], Blocks0) ->
    case maps:get(L, Blocks0) of
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
    case maps:get(Pred, Blocks) of
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
    #b_blk{last=Last0} = Blk0 = maps:get(Pred, Blocks0),
    #b_br{bool=#b_literal{val=true}} = Last0,   %Assertion.
    Last = Last0#b_br{bool=#b_literal{val=true},succ=L,fail=L},
    Blk = Blk0#b_blk{last=Last},
    Blocks = Blocks0#{Pred:=Blk},
    c_fix_branches(As, L, Blocks);
c_fix_branches([], _, Blocks) -> Blocks.

%%%
%%% Order element/2 calls.
%%%
%%% Order an unbroken chain of element/2 calls for the same tuple
%%% with the same failure label so that the highest element is
%%% retrieved first. That will allow the other element/2 calls to
%%% be replaced with get_tuple_element/3 instructions.
%%%

ssa_opt_element(#st{ssa=Blocks}=St) ->
    %% Collect the information about element instructions in this
    %% function.
    GetEls = collect_element_calls(beam_ssa:linearize(Blocks)),

    %% Collect the element instructions into chains. The
    %% element calls in each chain are ordered in reverse
    %% execution order.
    Chains = collect_chains(GetEls, []),

    %% For each chain, swap the first element call with the
    %% element call with the highest index.
    St#st{ssa=swap_element_calls(Chains, Blocks)}.

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

%%
%% Replaces setelement/3 with the update_tuple psuedo-instruction, and merges
%% multiple such calls into the same instruction.
%%

ssa_opt_setelement(#st{ssa=Linear0}=St) ->
    St#st{ssa=setelement_opt(Linear0, #{})}.

setelement_opt([{L, #b_blk{is=Is0}=B} | Bs], SetOps0) ->
    {Is, SetOps} = setelement_opt_is(Is0, SetOps0, []),
    [{L, B#b_blk{is=Is}} | setelement_opt(Bs, SetOps)];
setelement_opt([], _SetOps) ->
    [].

setelement_opt_is([#b_set{op=call,
                          dst=Dst,
                          args=[#b_remote{mod=#b_literal{val=erlang},
                                          name=#b_literal{val=setelement}},
                                Index, Src, Value]}=I0 | Is],
                  SetOps0, Acc) ->
    SetOps1 = maps:put(Dst, {Src, Index, Value}, SetOps0),
    SetOps = maps:remove(Value, SetOps1),

    Args = setelement_merge(Src, SetOps, [Index, Value], gb_sets:new()),
    I = I0#b_set{op=update_tuple,
                 dst=Dst,
                 args=[#b_literal{val=can_fail} | Args]},

    setelement_opt_is(Is, SetOps, [I | Acc]);
setelement_opt_is([#b_set{op=Op}=I | Is], SetOps0, Acc) ->
    case beam_ssa:clobbers_xregs(I) of
        true ->
            %% Merging setelement across stack frames is potentially very
            %% expensive as the update list needs to be saved on the stack, so
            %% we discard our state whenever we need one.
            setelement_opt_is(Is, #{}, [I | Acc]);
        false when Op =/= succeeded ->
            %% It's pointless to merge us with later ops if our result is used
            %% and needs to be created anyway.
            SetOps = maps:without(beam_ssa:used(I), SetOps0),
            setelement_opt_is(Is, SetOps, [I | Acc]);
        false when Op =:= succeeded ->
            %% This is a psuedo-op used to link ourselves with our catch block,
            %% so it doesn't really count as a use.
            setelement_opt_is(Is, SetOps0, [I | Acc])
    end;
setelement_opt_is([], SetOps, Acc) ->
    {reverse(Acc), SetOps}.

setelement_merge(Src, SetOps, Updates0, Seen0) ->
    %% Note that we're merging in reverse order, so Updates0 contains the
    %% updates made *after* this one.
    case SetOps of
        #{ Src := {Ancestor, #b_literal{}=Index, Value} } ->
            %% Drop redundant updates, which can happen when when a record is
            %% updated in several branches and one of them overwrites a
            %% previous index.
            Updates = case gb_sets:is_element(Index, Seen0) of
                          false -> [Index, Value | Updates0];
                          true -> Updates0
                      end,
            Seen = gb_sets:add(Index, Seen0),
            setelement_merge(Ancestor, SetOps, Updates, Seen);
        #{} ->
            [Src | Updates0]
    end.

%%%
%%% Record optimization.
%%%
%%% Replace tuple matching with an is_tagged_tuple instruction
%%% when applicable.
%%%

ssa_opt_record(#st{ssa=Linear}=St) ->
    Blocks = maps:from_list(Linear),
    St#st{ssa=record_opt(Linear, Blocks)}.

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
record_opt_is([I|Is], Last, Blocks) ->
    [I|record_opt_is(Is, Last, Blocks)];
record_opt_is([], _Last, _Blocks) -> [].

is_tagged_tuple(#b_var{}=Tuple, Bool,
                #b_br{bool=Bool,succ=Succ,fail=Fail},
                Blocks) ->
    SuccBlk = maps:get(Succ, Blocks),
    is_tagged_tuple_1(SuccBlk, Tuple, Fail, Blocks);
is_tagged_tuple(_, _, _, _) -> no.

is_tagged_tuple_1(#b_blk{is=Is,last=Last}, Tuple, Fail, Blocks) ->
    case Is of
        [#b_set{op={bif,tuple_size},dst=ArityVar,
                args=[#b_var{}=Tuple]},
         #b_set{op={bif,'=:='},
                dst=Bool,
                args=[ArityVar, #b_literal{val=ArityVal}=Arity]}]
        when is_integer(ArityVal) ->
            case Last of
                #b_br{bool=Bool,succ=Succ,fail=Fail} ->
                    SuccBlk = maps:get(Succ, Blocks),
                    case is_tagged_tuple_2(SuccBlk, Tuple, Fail) of
                        no ->
                            no;
                        {yes,Tag} ->
                            {yes,Arity,Tag}
                    end;
                _ ->
                    no
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

ssa_opt_cse(#st{ssa=Linear}=St) ->
    M = #{0=>#{}},
    St#st{ssa=cse(Linear, #{}, M)}.

cse([{L,#b_blk{is=Is0,last=Last0}=Blk}|Bs], Sub0, M0) ->
    Es0 = maps:get(L, M0),
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
         fail=none :: 'none' | beam_ssa:label(),
         non_guards :: gb_sets:set(beam_ssa:label()),
         bs :: beam_ssa:block_map()
        }).

ssa_opt_float(#st{ssa=Linear0,cnt=Count0}=St) ->
    NonGuards0 = float_non_guards(Linear0),
    NonGuards = gb_sets:from_list(NonGuards0),
    Blocks = maps:from_list(Linear0),
    Fs = #fs{non_guards=NonGuards,bs=Blocks},
    {Linear,Count} = float_opt(Linear0, Count0, Fs),
    St#st{ssa=Linear,cnt=Count}.

float_non_guards([{L,#b_blk{is=Is}}|Bs]) ->
    case Is of
        [#b_set{op=landingpad}|_] ->
            [L|float_non_guards(Bs)];
        _ ->
            float_non_guards(Bs)
    end;
float_non_guards([]) -> [?BADARG_BLOCK].

float_opt([{L,#b_blk{last=#b_br{fail=F}}=Blk}|Bs0],
            Count0, #fs{non_guards=NonGuards}=Fs) ->
    case gb_sets:is_member(F, NonGuards) of
        true ->
            %% This block is not inside a guard.
            %% We can do the optimization.
            float_opt_1(L, Blk, Bs0, Count0, Fs);
        false ->
            %% This block is inside a guard. Don't do
            %% any floating point optimizations.
            {Bs,Count} = float_opt(Bs0, Count0, Fs),
            {[{L,Blk}|Bs],Count}
    end;
float_opt([{L,Blk}|Bs], Count, Fs) ->
    float_opt_1(L, Blk, Bs, Count, Fs);
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
    #b_blk{is=Is} = maps:get(Succ, Blocks),
    case Is of
        [#b_set{anno=#{float_op:=_}}|_] ->
            %% The next operation is also a floating point operation.
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
              Ts, #fs{s=S,regs=Rs0}=Fs, Count0) ->
    {As1,Rs1,Count1} = float_load(As0, Ts, Rs0, Count0, []),
    {As,Is0} = unzip(As1),
    {Fr,Count2} = new_reg('@fr', Count1),
    FrDst = #b_var{name=Fr},
    I = I0#b_set{op={float,Op},dst=FrDst,args=As},
    Rs = Rs1#{Dst=>FrDst},
    Is = append(Is0) ++ [I],
    case S of
        undefined ->
            {Ignore,Count} = new_reg('@ssa_ignore', Count2),
            C = #b_set{op={float,clearerror},dst=#b_var{name=Ignore}},
            {[C|Is],Fs#fs{s=cleared,regs=Rs},Count};
        cleared ->
            {Is,Fs#fs{regs=Rs},Count2}
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

ssa_opt_live(#st{ssa=Linear0}=St) ->
    RevLinear = reverse(Linear0),
    Blocks0 = maps:from_list(RevLinear),
    Blocks = live_opt(RevLinear, #{}, Blocks0),
    Linear = beam_ssa:linearize(Blocks),
    St#st{ssa=Linear}.

live_opt([{L,Blk0}|Bs], LiveMap0, Blocks) ->
    Blk1 = beam_ssa_share:block(Blk0, Blocks),
    Successors = beam_ssa:successors(Blk1),
    Live0 = live_opt_succ(Successors, L, LiveMap0),
    {Blk,Live} = live_opt_blk(Blk1, Live0),
    LiveMap = live_opt_phis(Blk#b_blk.is, L, Live, LiveMap0),
    live_opt(Bs, LiveMap, Blocks#{L:=Blk});
live_opt([], _, Acc) -> Acc.

live_opt_succ([S|Ss], L, LiveMap) ->
    Live0 = live_opt_succ(Ss, L, LiveMap),
    Key = {S,L},
    case LiveMap of
        #{Key:=Live} ->
            gb_sets:union(Live, Live0);
        #{S:=Live} ->
            gb_sets:union(Live, Live0);
        #{} ->
            Live0
    end;
live_opt_succ([], _, _) ->
    gb_sets:empty().

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
live_opt_is([#b_set{op=succeeded,dst=SuccDst=SuccDstVar,
                    args=[Dst]}=SuccI,
             #b_set{dst=Dst}=I|Is], Live0, Acc) ->
    case gb_sets:is_member(Dst, Live0) of
        true ->
            case gb_sets:is_member(SuccDst, Live0) of
                true ->
                    Live1 = gb_sets:add(Dst, Live0),
                    Live = gb_sets:delete_any(SuccDst, Live1),
                    live_opt_is([I|Is], Live, [SuccI|Acc]);
                false ->
                    live_opt_is([I|Is], Live0, Acc)
            end;
        false ->
            case live_opt_unused(I) of
                {replace,NewI0} ->
                    NewI = NewI0#b_set{dst=SuccDstVar},
                    live_opt_is([NewI|Is], Live0, Acc);
                keep ->
                    case gb_sets:is_member(SuccDst, Live0) of
                        true ->
                            Live1 = gb_sets:add(Dst, Live0),
                            Live = gb_sets:delete_any(SuccDst, Live1),
                            live_opt_is([I|Is], Live, [SuccI|Acc]);
                        false ->
                            live_opt_is([I|Is], Live0, Acc)
                    end
            end
    end;
live_opt_is([#b_set{dst=Dst}=I|Is], Live0, Acc) ->
    case gb_sets:is_member(Dst, Live0) of
        true ->
            Live1 = gb_sets:union(Live0, gb_sets:from_ordset(beam_ssa:used(I))),
            Live = gb_sets:delete_any(Dst, Live1),
            live_opt_is(Is, Live, [I|Acc]);
        false ->
            case beam_ssa:no_side_effect(I) of
                true ->
                    live_opt_is(Is, Live0, Acc);
                false ->
                    Live = gb_sets:union(Live0, gb_sets:from_ordset(beam_ssa:used(I))),
                    live_opt_is(Is, Live, [I|Acc])
            end
    end;
live_opt_is([], Live, Acc) ->
    {Acc,Live}.

live_opt_unused(#b_set{op=get_map_element}=Set) ->
    {replace,Set#b_set{op=has_map_field}};
live_opt_unused(_) -> keep.

%%%
%%% Optimize binary matching instructions.
%%%

ssa_opt_bsm(#st{ssa=Linear}=St) ->
    Extracted0 = bsm_extracted(Linear),
    Extracted = cerl_sets:from_list(Extracted0),
    St#st{ssa=bsm_skip(Linear, Extracted)}.

bsm_skip([{L,#b_blk{is=Is0}=Blk}|Bs], Extracted) ->
    Is = bsm_skip_is(Is0, Extracted),
    [{L,Blk#b_blk{is=Is}}|bsm_skip(Bs, Extracted)];
bsm_skip([], _) -> [].

bsm_skip_is([I0|Is], Extracted) ->
    case I0 of
        #b_set{op=bs_match,args=[#b_literal{val=string}|_]} ->
            [I0|bsm_skip_is(Is, Extracted)];
        #b_set{op=bs_match,dst=Ctx,args=[Type,PrevCtx|Args0]} ->
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

%%%
%%% Short-cutting binary matching instructions.
%%%

ssa_opt_bsm_shortcut(#st{ssa=Linear}=St) ->
    Positions = bsm_positions(Linear, #{}),
    case map_size(Positions) of
        0 ->
            %% No binary matching instructions.
            St;
        _ ->
            St#st{ssa=bsm_shortcut(Linear, Positions)}
    end.

bsm_positions([{L,#b_blk{is=Is,last=Last}}|Bs], PosMap0) ->
    PosMap = bsm_positions_is(Is, PosMap0),
    case {Is,Last} of
        {[#b_set{op=bs_test_tail,dst=Bool,args=[Ctx,#b_literal{val=Bits0}]}],
         #b_br{bool=Bool,fail=Fail}} ->
            Bits = Bits0 + maps:get(Ctx, PosMap0),
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
%%% Eliminate redundant bs_test_unit2 instructions.
%%%

ssa_opt_bsm_units(#st{ssa=Linear}=St) ->
    St#st{ssa=bsm_units(Linear, #{})}.

bsm_units([{L,#b_blk{last=#b_br{succ=Succ,fail=Fail}}=Block0} | Bs], UnitMaps0) ->
    UnitsIn = maps:get(L, UnitMaps0, #{}),
    {Block, UnitsOut} = bsm_units_skip(Block0, UnitsIn),
    UnitMaps1 = bsm_units_join(Succ, UnitsOut, UnitMaps0),
    UnitMaps = bsm_units_join(Fail, UnitsIn, UnitMaps1),
    [{L, Block} | bsm_units(Bs, UnitMaps)];
bsm_units([{L,#b_blk{last=#b_switch{fail=Fail,list=Switch}}=Block} | Bs], UnitMaps0) ->
    UnitsIn = maps:get(L, UnitMaps0, #{}),
    Labels = [Fail | [Lbl || {_Arg, Lbl} <- Switch]],
    UnitMaps = foldl(fun(Lbl, UnitMaps) ->
                             bsm_units_join(Lbl, UnitsIn, UnitMaps)
                     end, UnitMaps0, Labels),
    [{L, Block} | bsm_units(Bs, UnitMaps)];
bsm_units([{L, Block} | Bs], UnitMaps) ->
    [{L, Block} | bsm_units(Bs, UnitMaps)];
bsm_units([], _UnitMaps) ->
    [].

bsm_units_skip(Block, Units) ->
    bsm_units_skip_1(Block#b_blk.is, Block, Units).

bsm_units_skip_1([#b_set{op=bs_start_match,dst=New}|_], Block, Units) ->
    %% We bail early since there can't be more than one match per block.
    {Block, Units#{ New => 1 }};
bsm_units_skip_1([#b_set{op=bs_match,
                         dst=New,
                         args=[#b_literal{val=skip},
                               Ctx,
                               #b_literal{val=binary},
                               _Flags,
                               #b_literal{val=all},
                               #b_literal{val=OpUnit}]}=Skip | Test],
                 Block0, Units) ->
    [#b_set{op=succeeded,dst=Bool,args=[New]}] = Test, %Assertion.
    #b_br{bool=Bool} = Last0 = Block0#b_blk.last, %Assertion.
    CtxUnit = maps:get(Ctx, Units),
    if
        CtxUnit rem OpUnit =:= 0 ->
            Is = takewhile(fun(I) -> I =/= Skip end, Block0#b_blk.is),
            Last = Last0#b_br{bool=#b_literal{val=true}},
            Block = Block0#b_blk{is=Is,last=Last},
            {Block, Units#{ New => CtxUnit }};
        CtxUnit rem OpUnit =/= 0 ->
            {Block0, Units#{ New => OpUnit, Ctx => OpUnit }}
    end;
bsm_units_skip_1([#b_set{op=bs_match,dst=New,args=Args}|_], Block, Units) ->
    [_,Ctx|_] = Args,
    CtxUnit = maps:get(Ctx, Units),
    OpUnit = bsm_op_unit(Args),
    {Block, Units#{ New => gcd(OpUnit, CtxUnit) }};
bsm_units_skip_1([_I | Is], Block, Units) ->
    bsm_units_skip_1(Is, Block, Units);
bsm_units_skip_1([], Block, Units) ->
    {Block, Units}.

bsm_op_unit([_,_,_,Size,#b_literal{val=U}]) ->
    case Size of
        #b_literal{val=Sz} when is_integer(Sz) -> Sz*U;
        _ -> U
    end;
bsm_op_unit([#b_literal{val=string},_,#b_literal{val=String}]) ->
    bit_size(String);
bsm_op_unit([#b_literal{val=utf8}|_]) ->
    8;
bsm_op_unit([#b_literal{val=utf16}|_]) ->
    16;
bsm_op_unit([#b_literal{val=utf32}|_]) ->
    32;
bsm_op_unit(_) ->
    1.

%% Several paths can lead to the same match instruction and the inferred units
%% may differ between them, so we can only keep the information that is common
%% to all paths.
bsm_units_join(Lbl, MapA, UnitMaps0) when is_map_key(Lbl, UnitMaps0) ->
    MapB = maps:get(Lbl, UnitMaps0),
    Merged = if
                 map_size(MapB) =< map_size(MapA) ->
                     bsm_units_join_1(maps:keys(MapB), MapA, MapB);
                 map_size(MapB) > map_size(MapA) ->
                     bsm_units_join_1(maps:keys(MapA), MapB, MapA)
             end,
    maps:put(Lbl, Merged, UnitMaps0);
bsm_units_join(Lbl, MapA, UnitMaps0) when MapA =/= #{} ->
    maps:put(Lbl, MapA, UnitMaps0);
bsm_units_join(_Lbl, _MapA, UnitMaps0) ->
    UnitMaps0.

bsm_units_join_1([Key | Keys], Left, Right) when is_map_key(Key, Left) ->
    UnitA = maps:get(Key, Left),
    UnitB = maps:get(Key, Right),
    bsm_units_join_1(Keys, Left, maps:put(Key, gcd(UnitA, UnitB), Right));
bsm_units_join_1([Key | Keys], Left, Right) ->
    bsm_units_join_1(Keys, Left, maps:remove(Key, Right));
bsm_units_join_1([], _MapA, Right) ->
    Right.

%%%
%%% Miscellanous optimizations in execution order.
%%%

ssa_opt_misc(#st{ssa=Linear}=St) ->
    St#st{ssa=misc_opt(Linear, #{})}.

misc_opt([{L,#b_blk{is=Is0,last=Last0}=Blk0}|Bs], Sub0) ->
    {Is,Sub} = misc_opt_is(Is0, Sub0, []),
    Last = sub(Last0, Sub),
    Blk = Blk0#b_blk{is=Is,last=Last},
    [{L,Blk}|misc_opt(Bs, Sub)];
misc_opt([], _) -> [].

misc_opt_is([#b_set{op=phi}=I0|Is], Sub0, Acc) ->
    #b_set{dst=Dst,args=Args} = I = sub(I0, Sub0),
    case all_same(Args) of
        true ->
            %% Eliminate the phi node if there is just one source
            %% value or if the values are identical.
            [{Val,_}|_] = Args,
            Sub = Sub0#{Dst=>Val},
            misc_opt_is(Is, Sub, Acc);
        false ->
            misc_opt_is(Is, Sub0, [I|Acc])
    end;
misc_opt_is([#b_set{op={bif,'and'}}=I0], Sub, Acc) ->
    #b_set{dst=Dst,args=Args} = I = sub(I0, Sub),
    case eval_and(Args) of
        error ->
            misc_opt_is([], Sub, [I|Acc]);
        Val ->
            misc_opt_is([], Sub#{Dst=>Val}, Acc)
    end;
misc_opt_is([#b_set{op={bif,'or'}}=I0], Sub, Acc) ->
    #b_set{dst=Dst,args=Args} = I = sub(I0, Sub),
    case eval_or(Args) of
        error ->
            misc_opt_is([], Sub, [I|Acc]);
        Val ->
            misc_opt_is([], Sub#{Dst=>Val}, Acc)
    end;
misc_opt_is([#b_set{}=I0|Is], Sub, Acc) ->
    #b_set{op=Op,dst=Dst,args=Args} = I = sub(I0, Sub),
    case make_literal(Op, Args) of
        #b_literal{}=Literal ->
            misc_opt_is(Is, Sub#{Dst=>Literal}, Acc);
        error ->
            misc_opt_is(Is, Sub, [I|Acc])
    end;
misc_opt_is([], Sub, Acc) ->
    {reverse(Acc),Sub}.

all_same([{H,_}|T]) ->
    all(fun({E,_}) -> E =:= H end, T).

make_literal(put_tuple, Args) ->
    case make_literal_list(Args, []) of
        error ->
            error;
        List ->
            #b_literal{val=list_to_tuple(List)}
    end;
make_literal(put_list, [#b_literal{val=H},#b_literal{val=T}]) ->
    #b_literal{val=[H|T]};
make_literal(_, _) -> error.

make_literal_list([#b_literal{val=H}|T], Acc) ->
    make_literal_list(T, [H|Acc]);
make_literal_list([_|_], _) ->
    error;
make_literal_list([], Acc) ->
    reverse(Acc).

eval_and(Args) ->
    case Args of
        [_,#b_literal{val=false}=Res] -> Res;
        [Res,#b_literal{val=true}] -> Res;
        [_,_] -> error
    end.

eval_or(Args) ->
    case Args of
        [Res,#b_literal{val=false}] -> Res;
        [_,#b_literal{val=true}=Res] -> Res;
        [_,_] -> error
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

ssa_opt_tuple_size(#st{ssa=Linear0,cnt=Count0}=St) ->
    {Linear,Count} = opt_tup_size(Linear0, Count0, []),
    St#st{ssa=Linear,cnt=Count}.

opt_tup_size([{L,#b_blk{is=Is,last=Last}=Blk}|Bs], Count0, Acc0) ->
    case {Is,Last} of
        {[#b_set{op={bif,'=:='},dst=Bool,args=[#b_var{}=Tup,#b_literal{val=Arity}]}],
         #b_br{bool=Bool}} when is_integer(Arity), Arity >= 0 ->
            {Acc,Count} = opt_tup_size_1(Tup, L, Count0, Acc0),
            opt_tup_size(Bs, Count, [{L,Blk}|Acc]);
        {_,_} ->
            opt_tup_size(Bs, Count0, [{L,Blk}|Acc0])
    end;
opt_tup_size([], Count, Acc) ->
    {reverse(Acc),Count}.

opt_tup_size_1(Size, EqL, Count0, [{L,Blk0}|Acc]) ->
    case Blk0 of
        #b_blk{is=Is0,last=#b_br{bool=Bool,succ=EqL,fail=Fail}} ->
            case opt_tup_size_is(Is0, Bool, Size, []) of
                none ->
                    {[{L,Blk0}|Acc],Count0};
                {PreIs,TupleSizeIs,Tuple} ->
                    opt_tup_size_2(PreIs, TupleSizeIs, L, EqL,
                                   Tuple, Fail, Count0, Acc)
            end;
        #b_blk{} ->
            {[{L,Blk0}|Acc],Count0}
    end;
opt_tup_size_1(_, _, Count, Acc) ->
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
%%% If the argument for a #b_switch{} comes from a phi node with all
%%% literals, any values in the switch list which are not in the phi
%%% node can be removed.
%%%
%%% If the values in the phi node and switch list are the same,
%%% the failure label can't be reached and be eliminated.
%%%
%%% A #b_switch{} with only one value can be rewritten to
%%% a #b_br{}. A switch that only verifies that the argument
%%% is 'true' or 'false' can be rewritten to a is_boolean test.
%%%

ssa_opt_sw(#st{ssa=Linear0,cnt=Count0}=St) ->
    {Linear,Count} = opt_sw(Linear0, #{}, Count0, []),
    St#st{ssa=Linear,cnt=Count}.

opt_sw([{L,#b_blk{is=Is,last=#b_switch{}=Last0}=Blk0}|Bs], Phis0, Count0, Acc) ->
    Phis = opt_sw_phis(Is, Phis0),
    case opt_sw_last(Last0, Phis) of
        #b_switch{arg=Arg,fail=Fail,list=[{Lit,Lbl}]} ->
            %% Rewrite a single value switch to a br.
            Bool = #b_var{name={'@ssa_bool',Count0}},
            Count = Count0 + 1,
            IsEq = #b_set{op={bif,'=:='},dst=Bool,args=[Arg,Lit]},
            Br = #b_br{bool=Bool,succ=Lbl,fail=Fail},
            Blk = Blk0#b_blk{is=Is++[IsEq],last=Br},
            opt_sw(Bs, Phis, Count, [{L,Blk}|Acc]);
        #b_switch{arg=Arg,fail=Fail,
                  list=[{#b_literal{val=B1},Lbl},{#b_literal{val=B2},Lbl}]}
          when B1 =:= not B2 ->
            %% Replace with is_boolean test.
            Bool = #b_var{name={'@ssa_bool',Count0}},
            Count = Count0 + 1,
            IsBool = #b_set{op={bif,is_boolean},dst=Bool,args=[Arg]},
            Br = #b_br{bool=Bool,succ=Lbl,fail=Fail},
            Blk = Blk0#b_blk{is=Is++[IsBool],last=Br},
            opt_sw(Bs, Phis, Count, [{L,Blk}|Acc]);
        Last0 ->
            opt_sw(Bs, Phis, Count0, [{L,Blk0}|Acc]);
        Last ->
            Blk = Blk0#b_blk{last=Last},
            opt_sw(Bs, Phis, Count0, [{L,Blk}|Acc])
    end;
opt_sw([{L,#b_blk{is=Is}=Blk}|Bs], Phis0, Count, Acc) ->
    Phis = opt_sw_phis(Is, Phis0),
    opt_sw(Bs, Phis, Count, [{L,Blk}|Acc]);
opt_sw([], _Phis, Count, Acc) ->
    {reverse(Acc),Count}.

opt_sw_phis([#b_set{op=phi,dst=Dst,args=Args}|Is], Phis) ->
    case opt_sw_literals(Args, []) of
        error ->
            opt_sw_phis(Is, Phis);
        Literals ->
            opt_sw_phis(Is, Phis#{Dst=>Literals})
    end;
opt_sw_phis(_, Phis) -> Phis.

opt_sw_last(#b_switch{arg=Arg,fail=Fail,list=List0}=Sw0, Phis) ->
    case Phis of
        #{Arg:=Values0} ->
            Values = gb_sets:from_list(Values0),

            %% Prune the switch list to only contain the possible values.
            List1 = [P || {Lit,_}=P <- List0, gb_sets:is_member(Lit, Values)],

            %% Now test whether the failure label can ever be reached.
            Sw = case gb_sets:size(Values) =:= length(List1) of
                     true ->
                         %% The switch list has the same number of values as the phi node.
                         %% The values must be the same, because the values that were not
                         %% possible were pruned from the switch list. Therefore, the
                         %% failure label can't possibly be reached, and we can choose a
                         %% a new failure label by picking a value from the list.
                         case List1 of
                             [{#b_literal{},Lbl}|List] ->
                                 Sw0#b_switch{fail=Lbl,list=List};
                             [] ->
                                 Sw0#b_switch{list=List1}
                         end;
                     false ->
                         %% There are some values in the phi node that are not in the
                         %% switch list; thus, the failure label can still be reached.
                         Sw0
                 end,
            beam_ssa:normalize(Sw);
        #{} ->
            %% Ensure that no label in the switch list is the same
            %% as the failure label.
            List = [{Val,Lbl} || {Val,Lbl} <- List0, Lbl =/= Fail],
            Sw = Sw0#b_switch{list=List},
            beam_ssa:normalize(Sw)
    end.

opt_sw_literals([{#b_literal{}=Lit,_}|T], Acc) ->
    opt_sw_literals(T, [Lit|Acc]);
opt_sw_literals([_|_], _Acc) ->
    error;
opt_sw_literals([], Acc) -> Acc.


%%%
%%% Merge blocks.
%%%

ssa_opt_merge_blocks(#st{ssa=Blocks}=St) ->
    Preds = beam_ssa:predecessors(Blocks),
    St#st{ssa=merge_blocks_1(beam_ssa:rpo(Blocks), Preds, Blocks)}.

merge_blocks_1([L|Ls], Preds0, Blocks0) ->
    case Preds0 of
        #{L:=[P]} ->
            #{P:=Blk0,L:=Blk1} = Blocks0,
            case is_merge_allowed(L, Blk0, Blk1) of
                true ->
                    #b_blk{is=Is0} = Blk0,
                    #b_blk{is=Is1} = Blk1,
                    Is = Is0 ++ Is1,
                    Blk = Blk1#b_blk{is=Is},
                    Blocks1 = maps:remove(L, Blocks0),
                    Blocks2 = maps:put(P, Blk, Blocks1),
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
    Ps = [rename_label(P, From, To) || P <- maps:get(L, Preds0)],
    Preds = maps:put(L, Ps, Preds0),
    merge_update_preds(Ls, From, To, Preds);
merge_update_preds([], _, _, Preds) -> Preds.

rename_label(From, From, To) -> To;
rename_label(Lbl, _, _) -> Lbl.

is_merge_allowed(_, _, #b_blk{is=[#b_set{op=peek_message}|_]}) ->
    false;
is_merge_allowed(L, Blk0, #b_blk{}) ->
    case beam_ssa:successors(Blk0) of
        [L] -> true;
        [_|_] -> false
    end.

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

ssa_opt_sink(#st{ssa=Blocks0}=St) ->
    Linear = beam_ssa:linearize(Blocks0),

    %% Create a map with all variables that define get_tuple_element
    %% instructions. The variable name map to the block it is defined in.
    Defs = maps:from_list(def_blocks(Linear)),

    %% Now find all the blocks that use variables defined by get_tuple_element
    %% instructions.
    Used = used_blocks(Linear, Defs, []),

    %% Calculate dominators.
    Dom0 = beam_ssa:dominators(Blocks0),

    %% It is not safe to move get_tuple_element instructions to blocks
    %% that begin with certain instructions. It is also unsafe to move
    %% the instructions into any part of a receive. To avoid such
    %% unsafe moves, pretend that the unsuitable blocks are not
    %% dominators.
    Unsuitable = unsuitable(Linear, Blocks0),
    Dom = case gb_sets:is_empty(Unsuitable) of
              true ->
                  Dom0;
              false ->
                  F = fun(_, DomBy) ->
                              [L || L <- DomBy,
                                    not gb_sets:is_element(L, Unsuitable)]
                      end,
                  maps:map(F, Dom0)
          end,

    %% Calculate new positions for get_tuple_element instructions. The new
    %% position is a block that dominates all uses of the variable.
    DefLoc = new_def_locations(Used, Defs, Dom),

    %% Now move all suitable get_tuple_element instructions to their
    %% new blocks.
    Blocks = foldl(fun({V,To}, A) ->
                           From = maps:get(V, Defs),
                           move_defs(V, From, To, A)
                   end, Blocks0, DefLoc),
    St#st{ssa=Blocks}.

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

unsuitable_1([{L,#b_blk{is=[#b_set{op=Op}|_]}}|Bs]) ->
    Unsuitable = case Op of
                     bs_extract -> true;
                     bs_put -> true;
                     {float,_} -> true;
                     landingpad -> true;
                     peek_message -> true;
                     wait_timeout -> true;
                     _ -> false
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
    Ps = maps:get(L, Predecessors),
    unsuitable_loop_1(Ps, Blocks, Predecessors, Acc).

unsuitable_loop_1([P|Ps], Blocks, Predecessors, Acc0) ->
    case maps:get(P, Blocks) of
        #b_blk{is=[#b_set{op=peek_message}|_]} ->
            unsuitable_loop_1(Ps, Blocks, Predecessors, Acc0);
        #b_blk{} ->
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

%% new_def_locations([{Variable,[UsedInBlock]}|Vs], Defs, Dominators) ->
%%          [{Variable,NewDefinitionBlock}]
%%  Calculate new locations for get_tuple_element instructions. For each
%%  variable, the new location is a block that dominates all uses of
%%  variable and as near to the uses of as possible. If no such block
%%  distinct from the block where the instruction currently is, the
%%  variable will not be included in the result list.

new_def_locations([{V,UsedIn}|Vs], Defs, Dom) ->
    DefIn = maps:get(V, Defs),
    case common_dom(UsedIn, DefIn, Dom) of
        [] ->
            new_def_locations(Vs, Defs, Dom);
        [_|_]=BetterDef ->
            L = most_dominated(BetterDef, Dom),
            [{V,L}|new_def_locations(Vs, Defs, Dom)]
    end;
new_def_locations([], _, _) -> [].

common_dom([L|Ls], DefIn, Dom) ->
    DomBy0 = maps:get(L, Dom),
    DomBy = ordsets:subtract(DomBy0, maps:get(DefIn, Dom)),
    common_dom_1(Ls, Dom, DomBy).

common_dom_1(_, _, []) ->
    [];
common_dom_1([L|Ls], Dom, [_|_]=DomBy0) ->
    DomBy1 = maps:get(L, Dom),
    DomBy = ordsets:intersection(DomBy0, DomBy1),
    common_dom_1(Ls, Dom, DomBy);
common_dom_1([], _, DomBy) -> DomBy.

most_dominated([L|Ls], Dom) ->
    most_dominated(Ls, L, maps:get(L, Dom), Dom).

most_dominated([L|Ls], L0, DomBy, Dom) ->
    case member(L, DomBy) of
        true ->
            most_dominated(Ls, L0, DomBy, Dom);
        false ->
            most_dominated(Ls, L, maps:get(L, Dom), Dom)
    end;
most_dominated([], L, _, _) -> L.


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
                  set_tuple_element -> beyond;
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
%%% Common utilities.
%%%

gcd(A, B) ->
    case A rem B of
        0 -> B;
        X -> gcd(B, X)
    end.

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
