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
%% Purpose: Prepare for code generation, including register allocation.
%%
%% The output of this compiler pass is still in the SSA format, but
%% it has been annotated and transformed to help the code generator.
%%
%% * Some instructions are translated to other instructions closer to
%% the BEAM instructions. For example, the binary matching
%% instructions are transformed from the optimization-friendly
%% internal format to instruction more similar to the actual BEAM
%% instructions.
%%
%% * Blocks that will need an instruction for allocating a stack frame
%% are annotated with a {frame_size,Size} annotation.
%%
%% * 'copy' instructions are added for all variables that need
%% to be saved to the stack frame. Additional 'copy' instructions
%% can be added as an optimization to reuse y registers (see
%% the copy_retval sub pass).
%%
%% * Each function is annotated with a {register,RegisterMap}
%% annotation that maps each variable to a BEAM register. The linear
%% scan algorithm is used to allocate registers.
%%
%% There are four kind of registers. x, y, fr (floating point register),
%% and z. A variable will be allocated to a z register if it is only
%% used by the instruction following the instruction that defines the
%% the variable. The code generator will typically combine those
%% instructions to a test instruction. z registers are also used for
%% some instructions that don't have a return value.
%%
%% References:
%%
%% [1] H. Mössenböck and M. Pfeiffer. Linear scan register allocation
%% in the context of SSA form and register constraints. In Proceedings
%% of the International Conference on Compiler Construction, pages
%% 229–246. LNCS 2304, Springer-Verlag, 2002.
%%
%% [2] C. Wimmer and H. Mössenböck. Optimized interval splitting in a
%% linear scan register allocator. In Proceedings of the ACM/USENIX
%% International Conference on Virtual Execution Environments, pages
%% 132–141. ACM Press, 2005.
%%
%% [3] C. Wimmer and M. Franz. Linear Scan Register Allocation on SSA
%% Form. In Proceedings of the International Symposium on Code
%% Generation and Optimization, pages 170-179. ACM Press, 2010.
%%

-module(beam_ssa_pre_codegen).
-moduledoc false.

-export([module/2]).

-include("beam_ssa.hrl").
-include("beam_asm.hrl").

-import(lists, [all/2,any/2,append/1,
                foldl/3,last/1,member/2,partition/2,
                reverse/1,reverse/2,seq/2,sort/1,sort/2,
                usort/1,zip/2]).

-spec module(beam_ssa:b_module(), [compile:option()]) ->
                    {'ok',beam_ssa:b_module()}.

module(#b_module{body=Fs0}=Module, Opts) ->
    Ps = passes(Opts),
    Fs1 = functions(Fs0, Ps),
    Fs = create_fc_stubs(Fs1, Module),
    {ok,Module#b_module{body=Fs}}.

functions([F|Fs], Ps) ->
    [function(F, Ps)|functions(Fs, Ps)];
functions([], _Ps) -> [].

-type b_var() :: beam_ssa:b_var().
-type var_name() :: beam_ssa:var_name().
-type instr_number() :: pos_integer().
-type range() :: {instr_number(),instr_number()}.
-type ypool() :: {'y',beam_ssa:label()}.
-type reservation() :: 'fr' | {'prefer',xreg()} | 'x' | {'x',xreg()} |
                       ypool() | {yreg(),ypool()} | 'z'.
-type ssa_register() :: beam_ssa_codegen:ssa_register().

-define(TC(Body), tc(fun() -> Body end, ?FILE, ?LINE)).
-record(st, {ssa :: beam_ssa:block_map(),
             args :: [b_var()],
             cnt :: beam_ssa:label(),
             frames=[] :: [beam_ssa:label()],
             intervals=[] :: [{b_var(),[range()]}],
             res=[] :: [{b_var(),reservation()}] | #{b_var():=reservation()},
             regs=#{} :: #{b_var():=ssa_register()},
             extra_annos=[] :: [{atom(),term()}],
             location :: term()
            }).
-define(PASS(N), {N,fun N/1}).

passes(Opts) ->
    AddPrecgAnnos = proplists:get_bool(dprecg, Opts),
    BeamDebugInfo = proplists:get_bool(beam_debug_info, Opts),

    Ps = [?PASS(assert_no_critical_edges),

          %% Preliminaries.
          ?PASS(fix_bs),
          ?PASS(sanitize),
          ?PASS(expand_match_fail),
          ?PASS(expand_update_tuple),

          case BeamDebugInfo of
              false -> ignore;
              true -> ?PASS(break_out_debug_line)
          end,

          ?PASS(place_frames),
          ?PASS(fix_receives),

          %% Find and reserve Y registers.
          ?PASS(find_yregs),
          ?PASS(reserve_yregs),

          %% Improve reuse of Y registers to potentially
          %% reduce the size of the stack frame.
          ?PASS(copy_retval),
          ?PASS(opt_get_list),

          %% Calculate live intervals.
          ?PASS(number_instructions),
          ?PASS(live_intervals),
          ?PASS(reserve_regs),

          %% If needed for a .precg file, save the live intervals
          %% so they can be included in an annotation.
          case AddPrecgAnnos of
              false -> ignore;
              true -> ?PASS(save_live_intervals)
          end,

          %% Allocate registers.
          ?PASS(linear_scan),
          ?PASS(frame_size),
          ?PASS(turn_yregs),

          ?PASS(assert_no_critical_edges)],
    [P || P <- Ps, P =/= ignore].

function(#b_function{anno=Anno,args=Args,bs=Blocks0,cnt=Count0}=F0, Ps) ->
    try
        Location = maps:get(location, Anno, none),
        St0 = #st{ssa=Blocks0,args=Args,
                  cnt=Count0,location=Location},
        St = compile:run_sub_passes(Ps, St0),
        #st{ssa=Blocks,cnt=Count,regs=Regs,extra_annos=ExtraAnnos} = St,
        F1 = add_extra_annos(F0, ExtraAnnos),
        F = beam_ssa:add_anno(registers, Regs, F1),
        F#b_function{bs=Blocks,cnt=Count}
    catch
        Class:Error:Stack ->
            #{func_info:={_,Name,Arity}} = Anno,
            io:fwrite("Function: ~w/~w\n", [Name,Arity]),
            erlang:raise(Class, Error, Stack)
    end.

save_live_intervals(#st{intervals=Intervals}=St) ->
    St#st{extra_annos=[{live_intervals,Intervals}]}.

%% Add extra annotations when a .precg listing file is being produced.
add_extra_annos(F, Annos) ->
    foldl(fun({Name,Value}, Acc) ->
                  beam_ssa:add_anno(Name, Value, Acc)
          end, F, Annos).

%% assert_no_critical_edges(St0) -> St.
%%  The code generator will not work if there are critial edges.
%%  Abort if any critical edges are found.

assert_no_critical_edges(#st{ssa=Blocks}=St) ->
    F = fun assert_no_ces/3,
    RPO = beam_ssa:rpo(Blocks),
    beam_ssa:fold_blocks(F, RPO, Blocks, Blocks),
    St.

assert_no_ces(_, #b_blk{is=[#b_set{op=phi,args=[_,_]=Phis}|_]}, Blocks) ->
    %% This block has multiple predecessors. Make sure that none
    %% of the precessors have more than one successor.
    true = all(fun({_,P}) ->
                       length(beam_ssa:successors(P, Blocks)) =:= 1
               end, Phis),                      %Assertion.
    Blocks;
assert_no_ces(_, _, Blocks) -> Blocks.

%% fix_bs(St0) -> St.
%%  Combine bs_match and bs_extract instructions to bs_get instructions.

fix_bs(#st{ssa=Blocks,cnt=Count0}=St) ->
    F = fun(#b_set{op=bs_start_match,dst=Dst}, A) ->
                %% Mark the root of the match context list.
                A#{Dst => {context,Dst}};
           (#b_set{op=bs_ensure,dst=Dst,args=[ParentCtx|_]}, A) ->
                %% Link this match context to the previous match context.
                A#{Dst => ParentCtx};
           (#b_set{op=bs_match,dst=Dst,args=[_,ParentCtx|_]}, A) ->
                %% Link this match context to the previous match context.
                A#{Dst => ParentCtx};
           (_, A) ->
                A
        end,
    RPO = beam_ssa:rpo(Blocks),
    CtxChain = beam_ssa:fold_instrs(F, RPO, #{}, Blocks),
    case map_size(CtxChain) of
        0 ->
            %% No binary matching in this function.
            St;
        _ ->
            Linear0 = beam_ssa:linearize(Blocks),

            %% Insert position instructions where needed.
            {Linear1,Count} = bs_pos_bsm3(Linear0, CtxChain, Count0),

            %% Rename instructions.
            Linear = bs_instrs(Linear1, CtxChain, []),

            St#st{ssa=maps:from_list(Linear),cnt=Count}
    end.

%% Insert bs_get_position and bs_set_position instructions as needed.
bs_pos_bsm3(Linear0, CtxChain, Count0) ->
    Rs0 = bs_restores(Linear0, CtxChain, #{}, #{}),
    Rs = maps:values(Rs0),
    S0 = sofs:relation(Rs, [{context,save_point}]),
    S1 = sofs:relation_to_family(S0),
    S = sofs:to_external(S1),

    {SavePoints,Count1} = make_bs_pos_dict(S, Count0, []),

    {Gets,Count2} = make_bs_getpos_map(Rs, SavePoints, Count1, []),
    {Sets,Count} = make_bs_setpos_map(maps:to_list(Rs0), SavePoints, Count2, []),

    %% Now insert all saves and restores.
    {bs_insert_bsm3(Linear0, Gets, Sets), Count}.

make_bs_getpos_map([{Ctx,Save}=Ps|T], SavePoints, Count, Acc) ->
    SavePoint = get_savepoint(Ps, SavePoints),
    I = #b_set{op=bs_get_position,dst=SavePoint,args=[Ctx]},
    make_bs_getpos_map(T, SavePoints, Count+1, [{Save,I}|Acc]);
make_bs_getpos_map([], _, Count, Acc) ->
    {maps:from_list(Acc),Count}.

make_bs_setpos_map([{Bef,{Ctx,_}=Ps}|T], SavePoints, Count, Acc) ->
    Ignored = #b_var{name=Count},
    Args = [Ctx, get_savepoint(Ps, SavePoints)],
    I = #b_set{op=bs_set_position,dst=Ignored,args=Args},
    make_bs_setpos_map(T, SavePoints, Count+1, [{Bef,I}|Acc]);
make_bs_setpos_map([], _, Count, Acc) ->
    {maps:from_list(Acc),Count}.

get_savepoint({_,_}=Ps, SavePoints) ->
    Name = map_get(Ps, SavePoints),
    #b_var{name=Name}.

make_bs_pos_dict([{Ctx,Pts}|T], Count0, Acc0) ->
    {Acc, Count} = make_bs_pos_dict_1(Pts, Ctx, Count0, Acc0),
    make_bs_pos_dict(T, Count, Acc);
make_bs_pos_dict([], Count, Acc) ->
    {maps:from_list(Acc), Count}.

make_bs_pos_dict_1([H|T], Ctx, I, Acc) ->
    make_bs_pos_dict_1(T, Ctx, I+1, [{{Ctx,H},I}|Acc]);
make_bs_pos_dict_1([], Ctx, I, Acc) ->
    {[{Ctx,I}|Acc], I}.

bs_restores([{L,#b_blk{is=Is,last=Last}}|Bs], CtxChain, D0, Rs0) ->
    InPos = maps:get(L, D0, #{}),
    {SuccPos, FailPos, Rs} = bs_restores_is(Is, CtxChain, InPos, InPos, Rs0),

    D = bs_update_successors(Last, SuccPos, FailPos, D0),
    bs_restores(Bs, CtxChain, D, Rs);
bs_restores([], _, _, Rs) -> Rs.

bs_update_successors(#b_br{succ=Succ,fail=Fail}, SPos, FPos, D) ->
    join_positions([{Succ,SPos},{Fail,FPos}], D);
bs_update_successors(#b_switch{fail=Fail,list=List}, SPos, FPos, D) ->
    SPos = FPos,                                %Assertion.
    Update = [{L,SPos} || {_,L} <:- List] ++ [{Fail,SPos}],
    join_positions(Update, D);
bs_update_successors(#b_ret{}, SPos, FPos, D) ->
    SPos = FPos,                                %Assertion.
    D.

join_positions([{L,MapPos0}|T], D) ->
    case D of
        #{L:=MapPos0} ->
            %% Same map.
            join_positions(T, D);
        #{L:=MapPos1} ->
            %% Different maps.
            MapPos = join_positions_1(MapPos0, MapPos1),
            join_positions(T, D#{L:=MapPos});
        #{} ->
            join_positions(T, D#{L=>MapPos0})
    end;
join_positions([], D) -> D.

join_positions_1(LHS, RHS) ->
    if
        map_size(LHS) < map_size(RHS) ->
            join_positions_2(maps:keys(LHS), RHS, LHS);
        true ->
            join_positions_2(maps:keys(RHS), LHS, RHS)
    end.

join_positions_2([V | Vs], Bigger, Smaller) ->
    case {Bigger, Smaller} of
        {#{ V := Same }, #{ V := Same }} ->
            join_positions_2(Vs, Bigger, Smaller);
        {#{ V := _ }, #{ V := _ }} ->
            join_positions_2(Vs, Bigger, Smaller#{ V := unknown });
        {#{}, #{ V := _ }} ->
            join_positions_2(Vs, Bigger, maps:remove(V, Smaller))
    end;
join_positions_2([], _Bigger, Smaller) ->
    Smaller.

%%
%% Updates the restore and position maps according to the given instructions.
%%
%% Note that positions may be updated even when a match fails; if a match
%% requires a restore, the position at the fail block will be the position
%% we've *restored to* and not the one we entered the current block with.
%%

bs_restores_is([#b_set{op=bs_start_match,dst=Start}|Is],
               CtxChain, SPos0, _FPos, Rs) ->
    %% Match instructions leave the position unchanged on failure, so
    %% FPos must be the SPos we entered the *instruction* with, and not the
    %% *block*.
    %%
    %% This is important when we have multiple matches in a single block where
    %% all but the last are guaranteed to succeed; the upcoming fail block must
    %% restore to the position of the next-to-last match, not the position we
    %% entered the current block with.
    FPos = SPos0,
    SPos = SPos0#{Start=>Start},
    bs_restores_is(Is, CtxChain, SPos, FPos, Rs);
bs_restores_is([#b_set{op=bs_ensure,dst=NewPos,args=Args}|Is],
               CtxChain, SPos0, _FPos, Rs0) ->
    Start = bs_subst_ctx(NewPos, CtxChain),
    [FromPos,#b_literal{val=Bits}|_] = Args,
    case SPos0 of
        #{Start := FromPos} ->
            %% Same position, no restore needed.
            SPos = case Bits of
                       0 -> SPos0;
                       _ -> SPos0#{Start := NewPos}
                   end,
            FPos = SPos0,
            bs_restores_is(Is, CtxChain, SPos, FPos, Rs0);
        #{} ->
            SPos = SPos0#{Start := NewPos},
            FPos = SPos0#{Start := FromPos},
            Rs = Rs0#{NewPos=>{Start,FromPos}},
            bs_restores_is(Is, CtxChain, SPos, FPos, Rs)
    end;
bs_restores_is([#b_set{anno=#{ensured := _},
                       op=bs_match,dst=NewPos,args=Args}|Is],
               CtxChain, SPos0, _FPos, Rs) ->
    %% This match instruction will be a part of a `bs_match` BEAM
    %% instruction, so there will never be a restore to this
    %% position.
    Start = bs_subst_ctx(NewPos, CtxChain),
    case Args of
        [#b_literal{val=skip},_FromPos,_Type,_Flags,#b_literal{val=all},_] ->
            %% This instruction will be optimized away. (The unit test
            %% part of it has been take care of by the preceding
            %% bs_ensure instruction.) All positions will be
            %% unchanged.
            SPos = FPos = SPos0,
            bs_restores_is(Is, CtxChain, SPos, FPos, Rs);
        [_,FromPos|_] ->
            SPos = SPos0#{Start := NewPos},
            FPos = SPos0#{Start := FromPos},
            bs_restores_is(Is, CtxChain, SPos, FPos, Rs)
    end;
bs_restores_is([#b_set{op=bs_match,dst=NewPos,args=Args}=I|Is],
               CtxChain, SPos0, _FPos, Rs0) ->
    Start = bs_subst_ctx(NewPos, CtxChain),
    [_,FromPos|_] = Args,
    case SPos0 of
        #{Start:=FromPos} ->
            %% Same position, no restore needed.
            SPos = case bs_match_type(I) of
                       plain ->
                            %% Update position to new position.
                            SPos0#{Start:=NewPos};
                        _ ->
                            %% Position will not change (test_unit
                            %% instruction or no instruction at
                            %% all).
                            SPos0
                   end,
            FPos = SPos0,
            bs_restores_is(Is, CtxChain, SPos, FPos, Rs0);
        #{Start:=_} ->
            %% Different positions, might need a restore instruction.
            case bs_match_type(I) of
                none ->
                    %% This is a tail test that will be optimized away.
                    %% There's no need to do a restore, and all
                    %% positions are unchanged.
                    FPos = SPos0,
                    bs_restores_is(Is, CtxChain, SPos0, FPos, Rs0);
                test_unit ->
                    %% This match instruction will be replaced by
                    %% a test_unit instruction. We will need a
                    %% restore. The new position will be the position
                    %% restored to (NOT NewPos).
                    SPos = SPos0#{Start:=FromPos},
                    FPos = SPos,
                    Rs = Rs0#{NewPos=>{Start,FromPos}},
                    bs_restores_is(Is, CtxChain, SPos, FPos, Rs);
                plain ->
                    %% Match or skip. Position will be changed.
                    SPos = SPos0#{Start:=NewPos},
                    FPos = SPos0#{Start:=FromPos},
                    Rs = Rs0#{NewPos=>{Start,FromPos}},
                    bs_restores_is(Is, CtxChain, SPos, FPos, Rs)
            end
    end;
bs_restores_is([#b_set{op=bs_extract,args=[FromPos|_]}|Is],
               CtxChain, SPos, _FPos, Rs) ->
    Start = bs_subst_ctx(FromPos, CtxChain),

    #{Start:=FromPos} = SPos,                   %Assertion.
    FPos = SPos,

    bs_restores_is(Is, CtxChain, SPos, FPos, Rs);
bs_restores_is([#b_set{op=call,dst=Dst,args=Args}|Is],
               CtxChain, SPos0, _FPos, Rs0) ->
    {SPos1, Rs} = bs_restore_args(Args, SPos0, CtxChain, Dst, Rs0),

    SPos = bs_invalidate_pos(Args, SPos1, CtxChain),
    FPos = SPos,

    bs_restores_is(Is, CtxChain, SPos, FPos, Rs);
bs_restores_is([#b_set{op=Op,dst=Dst,args=Args}|Is],
               CtxChain, SPos0, _FPos, Rs0)
  when Op =:= bs_test_tail;
       Op =:= bs_get_tail ->
    {SPos, Rs} = bs_restore_args(Args, SPos0, CtxChain, Dst, Rs0),
    FPos = SPos,

    bs_restores_is(Is, CtxChain, SPos, FPos, Rs);
bs_restores_is([#b_set{op={bif,Op},dst=Dst,args=Args}|Is],
               CtxChain, SPos0, _FPos, Rs0)
  when Op =:= byte_size;
       Op =:= bit_size ->
    {SPos, Rs} = bs_restore_args(Args, SPos0, CtxChain, Dst, Rs0),
    FPos = SPos,

    bs_restores_is(Is, CtxChain, SPos, FPos, Rs);
bs_restores_is([#b_set{op={succeeded,guard},args=[Arg]}],
               CtxChain, SPos, FPos0, Rs) ->
    %% If we're branching on a match operation, the positions will be different
    %% depending on whether it succeeds.
    Ctx = bs_subst_ctx(Arg, CtxChain),
    FPos = case SPos of
               #{ Ctx := _ } -> FPos0;
               #{} -> SPos
           end,
    {SPos, FPos, Rs};
bs_restores_is([_ | Is], CtxChain, SPos, _FPos, Rs) ->
    FPos = SPos,
    bs_restores_is(Is, CtxChain, SPos, FPos, Rs);
bs_restores_is([], _CtxChain, SPos, _FPos, Rs) ->
    FPos = SPos,
    {SPos, FPos, Rs}.

bs_match_type(#b_set{args=[#b_literal{val=skip},_Ctx,
                             #b_literal{val=binary},_Flags,
                             #b_literal{val=all},#b_literal{val=U}]}) ->
    case U of
        1 -> none;
        _ -> test_unit
    end;
bs_match_type(_) ->
    plain.

%% Call instructions leave the match position in an undefined state,
%% requiring us to invalidate each affected argument.
bs_invalidate_pos([#b_var{}=Arg|Args], Pos0, CtxChain) ->
    Start = bs_subst_ctx(Arg, CtxChain),
    case Pos0 of
        #{Start:=_} ->
            Pos = Pos0#{Start:=unknown},
            bs_invalidate_pos(Args, Pos, CtxChain);
        #{} ->
            %% Not a match context.
            bs_invalidate_pos(Args, Pos0, CtxChain)
    end;
bs_invalidate_pos([_|Args], Pos, CtxChain) ->
    bs_invalidate_pos(Args, Pos, CtxChain);
bs_invalidate_pos([], Pos, _CtxChain) ->
    Pos.

bs_restore_args([#b_var{}=Arg|Args], Pos0, CtxChain, Dst, Rs0) ->
    Start = bs_subst_ctx(Arg, CtxChain),
    case Pos0 of
        #{Start:=Arg} ->
            %% Same position, no restore needed.
            bs_restore_args(Args, Pos0, CtxChain, Dst, Rs0);
        #{Start:=_} ->
            %% Different positions, need a restore instruction.
            Pos = Pos0#{Start:=Arg},
            Rs = Rs0#{Dst=>{Start,Arg}},
            bs_restore_args(Args, Pos, CtxChain, Dst, Rs);
        #{} ->
            %% Not a match context.
            bs_restore_args(Args, Pos0, CtxChain, Dst, Rs0)
    end;
bs_restore_args([_|Args], Pos, CtxChain, Dst, Rs) ->
    bs_restore_args(Args, Pos, CtxChain, Dst, Rs);
bs_restore_args([], Pos, _CtxChain, _Dst, Rs) ->
    {Pos, Rs}.

%% Insert all bs_save and bs_restore instructions.

bs_insert_bsm3(Blocks, Saves, Restores) ->
    bs_insert_1(Blocks, [], Saves, Restores).

bs_insert_1([{L,#b_blk{is=Is0}=Blk} | Bs], Deferred0, Saves, Restores) ->
    Is1 = bs_insert_deferred(Is0, Deferred0),
    {Is, Deferred} = bs_insert_is(Is1, Saves, Restores, []),
    [{L,Blk#b_blk{is=Is}} | bs_insert_1(Bs, Deferred, Saves, Restores)];
bs_insert_1([], [], _, _) ->
    [].

bs_insert_deferred([#b_set{op=bs_extract}=I | Is], Deferred) ->
    [I | bs_insert_deferred(Is, Deferred)];
bs_insert_deferred(Is, Deferred) ->
    Deferred ++ Is.

bs_insert_is([#b_set{dst=Dst}=I|Is], Saves, Restores, Acc0) ->
    Pre = case Restores of
              #{Dst:=R} -> [R];
              #{} -> []
          end,
    Post = case Saves of
               #{Dst:=S} -> [S];
               #{} -> []
           end,
    Acc = [I | Pre] ++ Acc0,
    case Is of
        [#b_set{op={succeeded,_},args=[Dst]}] ->
            %% Defer the save sequence to the success block.
            {reverse(Acc, Is), Post};
        _ ->
            bs_insert_is(Is, Saves, Restores, Post ++ Acc)
    end;
bs_insert_is([], _, _, Acc) ->
    {reverse(Acc), []}.

%% Translate bs_match instructions to one of:
%%
%%    * bs_get / bs_ensured_get
%%    * bs_skip / bs_ensured_skip
%%    * bs_match_string / bs_ensured_match_string
%%
%% The bs_ensured_* instructions don't check that the bitstring being
%% matched is long enough, because that has already been done by a
%% bs_ensure instruction.
%%
%% Also rename match context variables to use the variable assigned to
%% by the start_match instruction.

bs_instrs([{L,#b_blk{is=Is0}=Blk}|Bs], CtxChain, Acc0) ->
    case bs_instrs_is(Is0, CtxChain, []) of
        [#b_set{op=bs_extract,dst=Dst,args=[Ctx]}|Is] ->
            %% Drop this instruction. Rewrite the corresponding
            %% bs_match instruction in the previous block to
            %% a bs_get instruction.
            Acc = bs_combine(Dst, Ctx, Acc0),
            bs_instrs(Bs, CtxChain, [{L,Blk#b_blk{is=Is}}|Acc]);
        Is ->
            bs_instrs(Bs, CtxChain, [{L,Blk#b_blk{is=Is}}|Acc0])
    end;
bs_instrs([], _, Acc) ->
    bs_rewrite_skip(Acc).

bs_rewrite_skip([{L,#b_blk{is=Is0,last=Last0}=Blk}|Bs]) ->
    case bs_rewrite_skip_is(Is0, []) of
        no ->
            [{L,Blk}|bs_rewrite_skip(Bs)];
        {yes,Is} ->
            %% bs_skip was rewritten to bs_ensured_skip, which
            %% can't fail.
            #b_br{succ=Succ} = Last0,
            Last = beam_ssa:normalize(Last0#b_br{fail=Succ}),
            [{L,Blk#b_blk{is=Is,last=Last}}|bs_rewrite_skip(Bs)]
    end;
bs_rewrite_skip([]) ->
    [].

bs_rewrite_skip_is([#b_set{anno=#{ensured := true},op=bs_skip}=I0,
                    #b_set{op={succeeded,guard}}], Acc) ->
    I = I0#b_set{op=bs_ensured_skip},
    {yes,reverse(Acc, [I])};
bs_rewrite_skip_is([I|Is], Acc) ->
    bs_rewrite_skip_is(Is, [I|Acc]);
bs_rewrite_skip_is([], _Acc) ->
    no.

bs_instrs_is([#b_set{op={succeeded,_}}=I|Is], CtxChain, Acc) ->
    %% This instruction refers to a specific operation, so we must not
    %% substitute the context argument.
    bs_instrs_is(Is, CtxChain, [I | Acc]);
bs_instrs_is([#b_set{anno=Anno0,op=Op,args=Args0}=I0|Is], CtxChain, Acc) ->
    Args = [bs_subst_ctx(A, CtxChain) || A <- Args0],
    I1 = I0#b_set{args=Args},
    I = case {Op,Args} of
            {bs_match,[#b_literal{val=skip},Ctx,Type|As]} ->
                Anno = case Anno0 of
                           #{arg_types := #{4 := SizeType}} ->
                               Anno0#{arg_types := #{3 => SizeType}};
                           #{} ->
                               Anno0
                       end,
                I1#b_set{anno=Anno,op=bs_skip,args=[Type,Ctx|As]};
            {bs_match,[#b_literal{val=string},Ctx,#b_literal{val=S}=S0]} ->
                case Anno0 of
                    #{ensured := true} when bit_size(S) =< 64 ->
                        I1#b_set{op=bs_ensured_match_string,args=[Ctx,S0]};
                    #{} ->
                        I1#b_set{op=bs_match_string,args=[Ctx,S0]}
                end;
            {_,_} ->
                I1
        end,
    bs_instrs_is(Is, CtxChain, [I|Acc]);
bs_instrs_is([], _, Acc) ->
    reverse(Acc).

%% Combine a bs_match instruction with the destination register
%% taken from a bs_extract instruction.

bs_combine(Dst, Ctx, [{L,#b_blk{is=Is0}=Blk}|Acc]) ->
    [#b_set{}=Succeeded,
     #b_set{anno=Anno,op=bs_match,args=[Type,_|As]}=BsMatch|Is1] = reverse(Is0),
    if
        is_map_key(ensured, Anno) ->
            %% This instruction can't fail.
            Is = reverse(Is1, [BsMatch#b_set{op=bs_ensured_get,dst=Dst,
                                             args=[Type,Ctx|As]}]),
            #b_blk{last=#b_br{succ=Succ}=Br0} = Blk,
            Br = beam_ssa:normalize(Br0#b_br{fail=Succ}),
            [{L,Blk#b_blk{is=Is,last=Br}}|Acc];
        true ->
            Is = reverse(Is1, [BsMatch#b_set{op=bs_get,dst=Dst,args=[Type,Ctx|As]},
                               Succeeded#b_set{args=[Dst]}]),
            [{L,Blk#b_blk{is=Is}}|Acc]
    end.

bs_subst_ctx(#b_var{}=Var, CtxChain) ->
    case CtxChain of
        #{Var:={context,Ctx}} ->
            Ctx;
        #{Var:=ParentCtx} ->
            bs_subst_ctx(ParentCtx, CtxChain);
        #{} ->
            %% Not a match context variable.
            Var
    end;
bs_subst_ctx(Other, _CtxChain) ->
    Other.

%% sanitize(St0) -> St.
%%  Remove constructs that can cause problems later:
%%
%%  * Unreachable blocks may cause problems for determination of
%%  dominators.
%%
%%  * Some instructions (such as get_hd) don't accept literal
%%  arguments. Evaluate the instructions and remove them.

sanitize(#st{ssa=Blocks0,cnt=Count0}=St) ->
    Ls = beam_ssa:rpo(Blocks0),
    {Blocks,Count} = sanitize(Ls, Blocks0, Count0, #{}, #{0 => reachable}),
    St#st{ssa=Blocks,cnt=Count}.

sanitize([L|Ls], InBlocks, Count0, Values0, Blocks0) ->
    case is_map_key(L, Blocks0) of
        false ->
            %% This block will never be reached. Discard it.
            sanitize(Ls, InBlocks, Count0, Values0, Blocks0);
        true ->
            #b_blk{is=Is0,last=Last0} = Blk0 = map_get(L, InBlocks),
            case sanitize_is(Is0, Last0, InBlocks, Blocks0, Count0, Values0, false, []) of
                no_change ->
                    Blk = sanitize_last(Blk0, Values0),
                    Blocks1 = Blocks0#{L := Blk},
                    Blocks = sanitize_reachable(Blk, Blocks1),
                    sanitize(Ls, InBlocks, Count0, Values0, Blocks);
                {Is,Last,Count,Values} ->
                    Blk1 = Blk0#b_blk{is=Is,last=Last},
                    Blk = sanitize_last(Blk1, Values),
                    Blocks1 = Blocks0#{L := Blk},
                    Blocks = sanitize_reachable(Blk, Blocks1),
                    sanitize(Ls, InBlocks, Count, Values, Blocks)
            end
    end;
sanitize([], _InBlocks, Count, _Values, Blocks) ->
    {Blocks,Count}.

sanitize_reachable(Blk, Blocks) ->
    foldl(fun(S, A) when is_map_key(S, A) -> A;
             (S, A) -> A#{S => reachable}
          end, Blocks, beam_ssa:successors(Blk)).

sanitize_is([#b_set{op=get_map_element,args=Args0}=I0|Is],
            Last, InBlocks, Blocks, Count0, Values, Changed, Acc) ->
    case sanitize_args(Args0, Values) of
        [#b_literal{}=Map,Key] ->
            %% Bind the literal map to a variable.
            {MapVar,Count} = new_var(Count0),
            I = I0#b_set{args=[MapVar,Key]},
            Copy = #b_set{op=copy,dst=MapVar,args=[Map]},
            sanitize_is(Is, Last, InBlocks, Blocks, Count,
                        Values, true, [I,Copy|Acc]);
        [_,_]=Args0 ->
            sanitize_is(Is, Last, InBlocks, Blocks, Count0,
                        Values, Changed, [I0|Acc]);
        [_,_]=Args ->
            I = I0#b_set{args=Args},
            sanitize_is(Is, Last, InBlocks, Blocks, Count0,
                        Values, true, [I|Acc])
    end;
sanitize_is([#b_set{op=call,dst=CallDst}=Call,
             #b_set{op={succeeded,body},dst=SuccDst,args=[CallDst]}=Succ],
            #b_br{bool=SuccDst,succ=SuccLbl,fail=?EXCEPTION_BLOCK}=Last0,
            InBlocks, Blocks, Count, Values, Changed, Acc) ->
    case InBlocks of
        #{SuccLbl := #b_blk{is=[],last=#b_ret{arg=CallDst}=Last}} ->
            %% Tail call that may fail, translate the terminator to an ordinary
            %% return to simplify code generation.
            do_sanitize_is(Call, [], Last, InBlocks, Blocks,
                           Count, Values, true, Acc);
        #{} ->
            do_sanitize_is(Call, [Succ], Last0, InBlocks, Blocks,
                           Count, Values, Changed, Acc)
    end;
sanitize_is([#b_set{op=Op,dst=Dst}=Fail,
             #b_set{op={succeeded,body},args=[Dst]}],
            #b_br{fail=?EXCEPTION_BLOCK},
            InBlocks, Blocks, Count, Values, _Changed, Acc)
  when Op =:= match_fail; Op =:= resume ->
    %% Match failure or rethrow without a local handler. Translate the
    %% terminator to an ordinary return to simplify code generation.
    Last = #b_ret{arg=Dst},
    do_sanitize_is(Fail, [], Last, InBlocks, Blocks, Count, Values, true, Acc);
sanitize_is([#b_set{op=match_fail,dst=RaiseDst},
             #b_set{op={succeeded,guard},dst=SuccDst,args=[RaiseDst]}],
            #b_br{bool=SuccDst}=Last0,
            InBlocks, Blocks, Count, Values, _Changed, Acc) ->
    %% Match failures may be present in guards when optimizations are turned
    %% off. They must be treated as if they always fail.
    Last = beam_ssa:normalize(Last0#b_br{bool=#b_literal{val=false}}),
    sanitize_is([], Last, InBlocks, Blocks, Count, Values, true, Acc);
sanitize_is([#b_set{op={succeeded,_Kind},dst=Dst,args=[Arg0]}=I0],
            #b_br{bool=Dst}=Last, _InBlocks, _Blocks, Count, Values, _Changed, Acc) ->
    %% We no longer need to distinguish between guard and body checks, so we'll
    %% rewrite this as a plain 'succeeded'.
    case sanitize_arg(Arg0, Values) of
        #b_var{}=Arg ->
            I = I0#b_set{op=succeeded,args=[Arg]},
            {reverse(Acc, [I]), Last, Count, Values};
        #b_literal{} ->
            Value = #b_literal{val=true},
            {reverse(Acc), Last, Count, Values#{ Dst => Value }}
    end;
sanitize_is([#b_set{op={succeeded,Kind},args=[Arg0]} | Is],
            Last, InBlocks, Blocks, Count, Values, _Changed, Acc) ->
    %% We're no longer branching on this instruction and can safely remove it.
    [] = Is, #b_br{succ=Same,fail=Same} = Last, %Assertion.
    if
        Same =:= ?EXCEPTION_BLOCK ->
            %% The checked instruction always fails at runtime and we're not
            %% in a try/catch; rewrite the terminator to a return.
            body = Kind,                        %Assertion.
            Arg = sanitize_arg(Arg0, Values),
            sanitize_is(Is, #b_ret{arg=Arg}, InBlocks, Blocks,
                        Count, Values, true, Acc);
        Same =/= ?EXCEPTION_BLOCK ->
            %% We either always succeed, or always fail to somewhere other than
            %% the exception block.
            true = Kind =:= guard orelse Kind =:= body, %Assertion.
            sanitize_is(Is, Last, InBlocks, Blocks, Count, Values, true, Acc)
    end;
sanitize_is([#b_set{op=bs_test_tail}=I], Last, InBlocks, Blocks,
            Count, Values, Changed, Acc) ->
    case Last of
        #b_br{succ=Same,fail=Same} ->
            sanitize_is([], Last, InBlocks, Blocks,
                        Count, Values, true, Acc);
        _ ->
            do_sanitize_is(I, [], Last, InBlocks, Blocks,
                           Count, Values, Changed, Acc)
    end;
sanitize_is([#b_set{op=bs_get,args=Args0}=I0|Is], Last, InBlocks, Blocks,
            Count, Values, Changed, Acc) ->
    case {Args0,sanitize_args(Args0, Values)} of
        {[_,_,_,#b_var{},_],[Type,Val,Flags,#b_literal{val=all},Unit]} ->
            %% The size `all` is used for the size of the final binary
            %% segment in a pattern. Using `all` explicitly is not allowed,
            %% so we convert it to an obvious invalid size.
            Args = [Type,Val,Flags,#b_literal{val=bad_size},Unit],
            I = I0#b_set{args=Args},
            sanitize_is(Is, Last, InBlocks, Blocks, Count, Values, true, [I|Acc]);
        {_,Args} ->
            I = I0#b_set{args=Args},
            sanitize_is(Is, Last, InBlocks, Blocks, Count, Values, Changed, [I|Acc])
    end;
sanitize_is([#b_set{}=I|Is], Last, InBlocks, Blocks, Count, Values, Changed, Acc) ->
    do_sanitize_is(I, Is, Last, InBlocks, Blocks, Count, Values, Changed, Acc);
sanitize_is([], Last, _InBlocks, _Blocks, Count, Values, Changed, Acc) ->
    case Changed of
        true ->
            {reverse(Acc), Last, Count, Values};
        false ->
            no_change
    end.

do_sanitize_is(#b_set{anno=Anno0,op=debug_line,args=Args0}=I0,
               Is, Last, InBlocks, Blocks, Count, Values, _Changed0, Acc) ->
    Args = sanitize_args(Args0, Values),
    #{alias := Alias0, literals := Literals0} = Anno0,
    Alias = sanitize_alias(Alias0, Values),
    Anno1 = Anno0#{alias := Alias},
    Anno = case [{Val,From} || #b_var{name=From} := #b_literal{val=Val} <- Values] of
               [] ->
                   Anno1;
               [_|_]=Literals ->
                   Anno1#{literals => Literals ++ Literals0}
           end,
    I = I0#b_set{anno=Anno,args=Args},
    Changed = true,
    sanitize_is(Is, Last, InBlocks, Blocks, Count, Values, Changed, [I|Acc]);
do_sanitize_is(#b_set{op=Op,dst=Dst,args=Args0}=I0,
               Is, Last, InBlocks, Blocks, Count, Values, Changed0, Acc) ->
    Args = sanitize_args(Args0, Values),
    case sanitize_instr(Op, Args, I0, Blocks) of
        {subst,Subst} ->
            sanitize_is(Is, Last, InBlocks, Blocks, Count, Values#{Dst => Subst},
                        true, Acc);
        {ok,I} ->
            sanitize_is(Is, Last, InBlocks, Blocks, Count, Values, true, [I|Acc]);
        ok ->
            I = I0#b_set{args=Args},
            Changed = Changed0 orelse Args =/= Args0,
            sanitize_is(Is, Last, InBlocks, Blocks, Count, Values, Changed, [I|Acc])
    end.

sanitize_last(#b_blk{last=Last0}=Blk, Values) ->
    Last = case Last0 of
               #b_br{bool=#b_literal{}} ->
                   Last0;
               #b_br{bool=Bool} ->
                   beam_ssa:normalize(Last0#b_br{bool=sanitize_arg(Bool, Values)});
               #b_ret{arg=Arg} ->
                   Last0#b_ret{arg=sanitize_arg(Arg, Values)};
               #b_switch{arg=Arg} ->
                   beam_ssa:normalize(Last0#b_switch{arg=sanitize_arg(Arg, Values)})
           end,
    if
        Last =/= Last0 ->
            Blk#b_blk{last=Last};
        true ->
            Blk
    end.

sanitize_alias(Alias, Values) ->
    sanitize_alias_1(maps:keys(Alias), Values, Alias).

sanitize_alias_1([Old|Vs], Values, Alias0) ->
    Alias = case Values of
                #{#b_var{name=Old} := #b_var{name=New}} ->
                    Alias0#{New => map_get(Old, Alias0)};
                #{} ->
                    Alias0
            end,
    sanitize_alias_1(Vs, Values, Alias);
sanitize_alias_1([], _Values, Alias) -> Alias.

sanitize_args(Args, Values) ->
    [sanitize_arg(Arg, Values) || Arg <- Args].

sanitize_arg(#b_remote{mod=Mod0,name=Name0}=Remote, Values) ->
    Mod = sanitize_arg(Mod0, Values),
    Name = sanitize_arg(Name0, Values),
    Remote#b_remote{mod=Mod,name=Name};
sanitize_arg({#b_var{}=Var,L}, Values) ->
    {sanitize_arg(Var, Values),L};
sanitize_arg(#b_var{}=Var, Values) ->
    case Values of
        #{Var := New} -> New;
        #{} -> Var
    end;
sanitize_arg(Arg, _Values) ->
    Arg.

sanitize_instr(phi, PhiArgs0, I, Blocks) ->
    PhiArgs = [{V,L} || {V,L} <:- PhiArgs0,
                        is_map_key(L, Blocks)],
    case phi_all_same(PhiArgs) of
        true ->
            %% (Can only happen when some optimizations have been
            %% turned off.)
            %%
            %% This phi node always produces the same literal value or
            %% variable.
            %%
            %% We must do constant propagation of literal values to
            %% ensure that we can sanitize any instructions that don't
            %% accept literals (such as `get_hd`). This is necessary
            %% for correctness, because
            %% beam_ssa_codegen:prefer_xregs/2 does constant
            %% propagation and could propagate a literal into an
            %% instruction that don't accept literals.
            %%
            %% The singleton phi nodes generated for the try/catch
            %% construct are problematic. For example:
            %%
            %%   try B = (A = bit_size(iolist_to_binary("a"))) rem 1 of
            %%      _ -> A;
            %%      _ -> B
            %%   after
            %%      ok
            %%   end.
            %%
            %% The try expression exports three values, resulting in three
            %% singleton phi nodes (with optimizations disabled):
            %%
            %%   _4 = phi { B, ^15 }
            %%    A = phi { _2, ^15 }
            %%   _14 = phi { B, ^15 }
            %%
            %% All three variable will be assigned to the same register,
            %% causing the correct variable (`A`) to be overwritten by `_14`.
            [{Subst,_}|_] = PhiArgs,
            {subst,Subst};
        false ->
            {ok,I#b_set{args=PhiArgs}}
    end;
sanitize_instr(Op, Args, I, _Blocks) ->
    sanitize_instr(Op, Args, I).

sanitize_instr({bif,Bif}, [#b_literal{val=Lit}], _I) ->
    case erl_bifs:is_pure(erlang, Bif, 1) of
        false ->
            ok;
        true ->
            try
                {subst,#b_literal{val=erlang:Bif(Lit)}}
            catch
                error:_ ->
                    ok
            end
    end;
sanitize_instr({bif,Bif}, [#b_literal{val=Lit1},#b_literal{val=Lit2}], _I) ->
    true = erl_bifs:is_pure(erlang, Bif, 2) orelse
        beam_ssa:can_be_guard_bif(erlang, Bif, 2),    %Assertion.
    try
        {subst,#b_literal{val=erlang:Bif(Lit1, Lit2)}}
    catch
        error:_ ->
            ok
    end;
sanitize_instr(bs_match, Args, I) ->
    %% Matching of floats are never changed to a bs_skip even when the
    %% value is never used, because the match can always fail (for
    %% example, if it is a NaN). Sometimes (for contrived code) the
    %% optimizing passes fail to do the conversion to bs_skip for
    %% other data types as well.
    {ok,I#b_set{op=bs_get,args=Args}};
sanitize_instr(get_hd, [#b_literal{val=[Hd|_]}], _I) ->
    {subst,#b_literal{val=Hd}};
sanitize_instr(get_tl, [#b_literal{val=[_|Tl]}], _I) ->
    {subst,#b_literal{val=Tl}};
sanitize_instr(get_tuple_element, [#b_literal{val=T},
                                   #b_literal{val=I}], _I)
  when I < tuple_size(T) ->
    {subst,#b_literal{val=element(I+1, T)}};
sanitize_instr(is_nonempty_list, [#b_literal{val=Term}], _I) ->
    Lit = case Term of
              [_|_] -> true;
              _ -> false
          end,
    {subst,#b_literal{val=Lit}};
sanitize_instr(is_tagged_tuple, [#b_literal{val=Tuple},
                                 #b_literal{val=Arity},
                                 #b_literal{val=Tag}], _I)
  when is_integer(Arity), is_atom(Tag) ->
    if
        tuple_size(Tuple) =:= Arity, element(1, Tuple) =:= Tag ->
            {subst,#b_literal{val=true}};
        true ->
            {subst,#b_literal{val=false}}
    end;
sanitize_instr(succeeded, [#b_literal{}], _I) ->
    {subst,#b_literal{val=true}};
sanitize_instr(_, _, _) ->
    ok.

phi_all_same([{Arg,_From}|Phis]) ->
    phi_all_same_1(Phis, Arg).

phi_all_same_1([{Arg,_From}|Phis], Arg) ->
    phi_all_same_1(Phis, Arg);
phi_all_same_1([], _Arg) ->
    true;
phi_all_same_1(_Phis, _Arg) ->
    false.

%%% Rewrite certain calls to erlang:error/{1,2} to specialized
%%% instructions:
%%%
%%% erlang:error({badmatch,Value})       => badmatch Value
%%% erlang:error({case_clause,Value})    => case_end Value
%%% erlang:error({try_clause,Value})     => try_case_end Value
%%% erlang:error(if_clause)              => if_end
%%% erlang:error(function_clause, Args)  => jump FuncInfoLabel
%%%
%%% In SSA code, we represent those instructions as a 'match_fail'
%%% instruction with the name of the BEAM instruction as the first
%%% argument.

expand_match_fail(#st{ssa=Blocks0,
                      cnt=Count0,
                      args=Args,
                      location=Location}=St) ->
    Bs = maps:to_list(Blocks0),
    {Blocks, Count} = expand_mf_bs(Bs, length(Args), Location, Blocks0, Count0),
    St#st{ssa=Blocks,cnt=Count}.

expand_mf_bs([{L,#b_blk{is=Is0}=Blk} | Bs], Arity, Location, Blocks0, Count0) ->
    case expand_mf_is(Is0, Arity, Location, Count0, []) of
        none ->
            expand_mf_bs(Bs, Arity, Location, Blocks0, Count0);
        {Is, Count} ->
            Blocks = Blocks0#{L:=Blk#b_blk{is=Is}},
            expand_mf_bs(Bs, Arity, Location, Blocks, Count)
    end;
expand_mf_bs([], _Arity, _Location, Blocks, Count) ->
    {Blocks, Count}.

expand_mf_is([#b_set{op=match_fail,
                     anno=Anno,
                     args=[#b_literal{val=function_clause} | Args]}=I0 | Is],
             Arity, Location, Count0, Acc) ->
    case Anno of
        #{ location := Location } when length(Args) =:= Arity ->
            %% We have the same location as the `func_info` instruction at the
            %% beginning of the function; keep the instruction.
            none;
        #{ inlined := {Name,InlinedArity} } when length(Args) =:= InlinedArity ->
            %% We're raising this for an inlined function, convert it to a call
            %% to a stub function that will raise a proper `function_clause`
            %% exception. The stub function will be created later by
            %% `create_fc_stubs/2`.
            Target = #b_local{name=#b_literal{val=Name},arity=InlinedArity},
            I = I0#b_set{op=call,args=[Target | Args]},
            {reverse(Acc, [I | Is]), Count0}
    end;
expand_mf_is([#b_set{op=match_fail}=I | Is], _Arity, _Location, Count, Acc) ->
    expand_mf_instr(I, Is, Count, Acc);
expand_mf_is([I | Is], Arity, Location, Count, Acc) ->
    expand_mf_is(Is, Arity, Location, Count, [I | Acc]);
expand_mf_is(_, _, _, _, _) ->
    none.

expand_mf_instr(#b_set{args=[#b_literal{val=case_clause} | Args]}=I0,
                Is, Count, Acc) ->
    I = I0#b_set{args=[#b_literal{val=case_end} | Args]},
    {reverse(Acc, [I | Is]), Count};
expand_mf_instr(#b_set{args=[#b_literal{val=if_clause} | Args]}=I0,
                Is, Count, Acc) ->
    I = I0#b_set{args=[#b_literal{val=if_end} | Args]},
    {reverse(Acc, [I | Is]), Count};
expand_mf_instr(#b_set{args=[#b_literal{val=try_clause} | Args]}=I0,
                Is, Count, Acc) ->
    I = I0#b_set{args=[#b_literal{val=try_case_end} | Args]},
    {reverse(Acc, [I | Is]), Count};
expand_mf_instr(#b_set{args=[#b_literal{val=badmatch} | _Args]}=I,
                Is, Count, Acc) ->
    {reverse(Acc, [I | Is]), Count};
expand_mf_instr(#b_set{args=[#b_literal{val=badrecord} | _Args]}=I,
                Is, Count, Acc) ->
    {reverse(Acc, [I | Is]), Count};
expand_mf_instr(#b_set{args=[#b_literal{}|_]=Args}=I0, Is, Count0, Acc) ->
    %% We don't have a specialized instruction for this: simulate it with
    %% `erlang:error/1` instead.
    {Tuple, Count} = new_var(Count0),
    Put = #b_set{op=put_tuple,dst=Tuple,args=Args},
    Call = I0#b_set{op=call,
                    args=[#b_remote{mod=#b_literal{val=erlang},
                                    name=#b_literal{val=error},
                                    arity=1},
                         Tuple]},
    {reverse(Acc, [Put, Call | Is]), Count}.

%% Create stubs for `function_clause` exceptions generated by
%% inlined code.
create_fc_stubs(Fs, #b_module{name=Mod}) ->
    Stubs0 = usort(find_fc_errors(Fs, [])),
    Stubs = [begin
                 Seq = seq(0, Arity-1),
                 Args = [#b_var{name=V} || V <- Seq],
                 XRegs = [{x,V} || V <- Seq],
                 Ret = #b_var{name='@ssa_ret'},
                 Regs = maps:from_list([{Ret,{x,0}}|zip(Args, XRegs)]),
                 Anno = #{func_info => {Mod,Name,Arity},
                          location => Location,
                          parameter_info => #{},
                          registers => Regs},
                 Fc = #b_set{op=match_fail,dst=Ret,
                             args=[#b_literal{val=function_clause}|Args]},
                 Blk = #b_blk{is=[Fc],last=#b_ret{arg=Ret}},
                 #b_function{anno=Anno,args=Args,
                             bs=#{0 => Blk},
                             cnt=1}
             end || {{Name,Arity},Location} <:- Stubs0],
    Fs ++ Stubs.

find_fc_errors([#b_function{bs=Blocks}|Fs], Acc0) ->
    F = fun(#b_set{anno=Anno,op=call,args=[#b_local{} | _]}, A) ->
                case Anno of
                    #{ inlined := FA } ->
                        [{FA, maps:get(location, Anno, [])} | A];
                    #{} ->
                        A
                end;
           (_, A) ->
                A
        end,
    Acc = beam_ssa:fold_instrs(F, maps:keys(Blocks), Acc0, Blocks),
    find_fc_errors(Fs, Acc);
find_fc_errors([], Acc) ->
    Acc.

%%% expand_update_tuple(St0) -> St
%%%
%%%   Expands the update_tuple psuedo-instruction into its actual instructions.
%%%
expand_update_tuple(#st{ssa=Blocks0,cnt=Count0}=St) ->
    Linear0 = beam_ssa:linearize(Blocks0),
    {Linear, Count} = expand_update_tuple_1(Linear0, Count0, []),
    Blocks = maps:from_list(Linear),
    St#st{ssa=Blocks,cnt=Count}.

expand_update_tuple_1([{L, #b_blk{is=Is0}=B0} | Bs], Count0, Acc0) ->
    case expand_update_tuple_is(Is0, Count0, []) of
        {Is, Count} ->
            expand_update_tuple_1(Bs, Count, [{L, B0#b_blk{is=Is}} | Acc0]);
        {Is, NextIs, Count1} ->
            %% There are `set_tuple_element` instructions that we must put into
            %% a new block to avoid separating the `setelement` instruction from
            %% its `succeeded` instruction.
            #b_blk{last=Br} = B0,
            #b_br{succ=Succ} = Br,
            NextL = Count1,
            Count = Count1 + 1,
            NextBr = #b_br{bool=#b_literal{val=true},succ=Succ,fail=Succ},
            NextB = #b_blk{is=NextIs,last=NextBr},
            B = B0#b_blk{is=Is,last=Br#b_br{succ=NextL}},
            Acc = [{NextL, NextB}, {L, B} | Acc0],
            expand_update_tuple_1(Bs, Count, Acc)
    end;
expand_update_tuple_1([], Count, Acc) ->
    {Acc, Count}.

expand_update_tuple_is([#b_set{op=update_tuple, args=[Src | Args]}=I0 | Is],
                        Count0, Acc) ->
    {SetElement, Sets, Count} = expand_update_tuple_list(Args, I0, Src, Count0),
    case {Sets, Is} of
        {[_ | _], [#b_set{op=succeeded}]} ->
            {reverse(Acc, [SetElement | Is]), reverse(Sets), Count};
        {_, _} ->
            expand_update_tuple_is(Is, Count, Sets ++ [SetElement | Acc])
    end;
expand_update_tuple_is([I | Is], Count, Acc) ->
    expand_update_tuple_is(Is, Count, [I | Acc]);
expand_update_tuple_is([], Count, Acc) ->
    {reverse(Acc), Count}.

%% Expands an update_tuple list into setelement/3 + set_tuple_element.
%%
%% Note that it returns the instructions in reverse order.
expand_update_tuple_list(Args, I0, Src, Count0) ->
    [Index, Value | Rest] = sort_update_tuple(Args, []),

    %% set_tuple_element is destructive, so we have to start off with a
    %% setelement/3 call to give them something to work on.
    I = I0#b_set{op=call,
                 args=[#b_remote{mod=#b_literal{val=erlang},
                       name=#b_literal{val=setelement},
                       arity=3},
                 Index, Src, Value]},
    {Sets, Count} = expand_update_tuple_list_1(Rest, I#b_set.dst, Count0, []),
    {I, Sets, Count}.

expand_update_tuple_list_1([], _Src, Count, Acc) ->
    {Acc, Count};
expand_update_tuple_list_1([Index0, Value | Updates], Src, Count0, Acc) ->
    %% Change to the 0-based indexing used by `set_tuple_element`.
    Index = #b_literal{val=(Index0#b_literal.val - 1)},
    {Dst, Count} = new_var(Count0),
    SetOp = #b_set{op=set_tuple_element,
                   dst=Dst,
                   args=[Value, Src, Index]},
    expand_update_tuple_list_1(Updates, Src, Count, [SetOp | Acc]).

%% Sorts updates so that the highest index comes first, letting us use
%% set_tuple_element for all subsequent operations as we know their indexes
%% will be valid.
sort_update_tuple([_Index, _Value]=Args, []) ->
    Args;
sort_update_tuple([#b_literal{}=Index, Value | Updates], Acc) ->
    sort_update_tuple(Updates, [{Index, Value} | Acc]);
sort_update_tuple([], Acc) ->
    append([[Index, Value] || {Index, Value} <:- sort(fun erlang:'>='/2, Acc)]).


%%%
%%% Avoid placing stack frame allocation instructions before an
%%% `debug_line` instruction to potentially provide information for
%%% more variables.
%%%
%%% This sub pass is only run when the `beam_debug_info` option has been given.
%%%
%%% As an example, consider this function:
%%%
%%%     foo(A, B, C) ->
%%%         {ok,bar(A),B}.
%%%
%%% When compiled with the `beam_debug_info` option the first part of the SSA code
%%% will look like this:
%%%
%%%     0:
%%%       _7 = debug_line `1`
%%%       _3 = call (`bar`/1), _0
%%%
%%% The beam_ssa_pre_codegen pass will place a stack frame before the block:
%%%
%%%     %% #{frame_size => 1,yregs => #{{b_var,1} => []}}
%%%     0:
%%%       [1] y0/_12 = copy x1/_1
%%%       [3] z0/_7 = debug_line `1`
%%%
%%% In the resulting BEAM code there will not be any information for
%%% variable `C`, because the allocate instruction will kill it before
%%% reaching the `debug_line` instruction:
%%%
%%%     {allocate,1,2}.
%%%     {move,{x,1},{y,0}}.
%%%     {debug_line,[{location,...}],1,
%%%                  {1,[{'A',[{x,0}]},{'B',[{x,1},{y,0}]}]},
%%%                  2}.
%%%
%%% If we split the block after the `debug_line` instruction, the
%%% allocation of the stack frame will be placed after the
%%% `debug_line` instruction:
%%%
%%%     0:
%%%       [1] z0/_7 = debug_line `1`
%%%       [3] br ^12
%%%
%%%     %% #{frame_size => 1,yregs => #{{b_var,1} => []}}
%%%     12:
%%%       [5] y0/_13 = copy x1/_1
%%%       [7] x0/_3 = call (`bar`/1), x0/_0
%%%
%%% In the resulting BEAM code, there will now be information for variable `C`:
%%%
%%%     {debug_line,[{location,"t.erl",5}],
%%%                 1,
%%%                 {none,[{'A',[{x,0}]},{'B',[{x,1}]},{'C',[{x,2}]}]},
%%%                 2}.
%%%     {allocate,1,2}.
%%%

break_out_debug_line(#st{ssa=Blocks0,cnt=Count0}=St) ->
    RPO = beam_ssa:rpo(Blocks0),

    %% Calculate the set of all indices for `debug_line` instructions
    %% that occur as the first instruction in a block. Splitting after
    %% every `debug_line` instruction is not always beneficial, and
    %% can even result in worse information about variables.
    F = fun(_, #b_blk{is=[#b_set{op=debug_line,
                                 args=[#b_literal{val=Index}]}|_]}, Acc) ->
                sets:add_element(Index, Acc);
           (_, _, Acc) ->
                Acc
        end,
    ToBeSplit = beam_ssa:fold_blocks(F, RPO, sets:new(), Blocks0),

    %% Now split blocks after the found `debug_line` instructions that
    %% are known to start blocks.
    P = fun(#b_set{op=debug_line,args=[#b_literal{val=Index}]}) ->
                sets:is_element(Index, ToBeSplit);
           (_) ->
                false
        end,
    {Blocks,Count} = beam_ssa:split_blocks_after(RPO, P, Blocks0, Count0),

    St#st{ssa=Blocks,cnt=Count}.

%%%
%%% Find out where frames should be placed.
%%%

%% place_frames(St0) -> St.
%%   Return a list of the labels for the blocks that need stack frame
%%   allocation instructions.
%%
%%   This function attempts to place stack frames as tight as possible
%%   around the code, to avoid building stack frames for code paths
%%   that don't need one.
%%
%%   Stack frames are placed in blocks that dominate all of their
%%   descendants. That guarantees that the deallocation instructions
%%   cannot be reached from other execution paths that didn't set up
%%   a stack frame or set up a stack frame with a different size.

place_frames(#st{ssa=Blocks}=St) ->
    Ls = beam_ssa:rpo(Blocks),
    {Doms,_} = beam_ssa:dominators(Ls, Blocks),
    Tried = gb_sets:empty(),
    Frames0 = [],
    {Frames,_} = place_frames_1(Ls, Blocks, Doms, Tried, Frames0),
    St#st{frames=Frames}.

place_frames_1([L|Ls], Blocks, Doms, Tried0, Frames0) ->
    Blk = map_get(L, Blocks),
    case need_frame(Blk) of
        true ->
            %% This block needs a frame. Try to place it here.
            {Frames,Tried} = do_place_frame(L, Blocks, Doms, Tried0, Frames0),

            %% Successfully placed. Try to place more frames in descendants
            %% that are not dominated by this block.
            place_frames_1(Ls, Blocks, Doms, Tried, Frames);
        false ->
            try
                place_frames_1(Ls, Blocks, Doms, Tried0, Frames0)
            catch
                throw:{need_frame,For,Tried1}=Reason ->
                    %% An descendant block needs a stack frame. Try to
                    %% place it here.
                    case is_dominated_by(For, L, Doms) of
                        true ->
                            %% Try to place a frame here.
                            {Frames,Tried} = do_place_frame(L, Blocks, Doms,
                                                            Tried1, Frames0),
                            place_frames_1(Ls, Blocks, Doms, Tried, Frames);
                        false ->
                            %% Wrong place. This block does not dominate
                            %% the block that needs the frame. Pass it on
                            %% to our ancestors.
                            throw(Reason)
                    end
            end
    end;
place_frames_1([], _, _, Tried, Frames) ->
    {Frames,Tried}.

%% do_place_frame(Label, Blocks, Dominators, Tried0, Frames0) -> {Frames,Tried}.
%%  Try to place a frame in this block. This function returns
%%  successfully if it either succeeds at placing a frame in this
%%  block, if an ancestor that dominates this block has already placed
%%  a frame, or if we have already tried to put a frame in this block.
%%
%%  An {need_frame,Label,Tried} exception will be thrown if this block
%%  block is not suitable for having a stack frame (i.e. it does not dominate
%%  all of its descendants). The exception means that an ancestor will have to
%%  place the frame needed by this block.

do_place_frame(L, Blocks, Doms, Tried0, Frames) ->
    case gb_sets:is_element(L, Tried0) of
        true ->
            %% We have already tried to put a frame in this block.
            {Frames,Tried0};
        false ->
            %% Try to place a frame in this block.
            Tried = gb_sets:insert(L, Tried0),
            case place_frame_here(L, Blocks, Doms, Frames) of
                yes ->
                    %% We need a frame and it is safe to place it here.
                    {[L|Frames],Tried};
                no ->
                    %% An ancestor has a frame. Not needed.
                    {Frames,Tried};
                ancestor ->
                    %% This block does not dominate all of its
                    %% descendants. We must place the frame in
                    %% an ancestor.
                    throw({need_frame,L,Tried})
            end
    end.

%% place_frame_here(Label, Blocks, Doms, Frames) -> no|yes|ancestor.
%%  Determine whether a frame should be placed in block Label.

place_frame_here(L, Blocks, Doms, Frames) ->
    B0 = any(fun(DomBy) ->
                     is_dominated_by(L, DomBy, Doms)
             end, Frames),
    case B0 of
        true ->
            %% This block is dominated by an ancestor block that
            %% defines a frame. Not needed/allowed to put a frame
            %% here.
            no;
        false ->
            %% No frame in any ancestor. We need a frame.
            %% Now check whether the frame can be placed here.
            %% If this block dominates all of its descendants
            %% and the predecessors of any phi nodes it can be
            %% placed here.
            Descendants = beam_ssa:rpo([L], Blocks),
            PhiPredecessors = phi_predecessors(L, Blocks),
            MustDominate = ordsets:from_list(PhiPredecessors ++ Descendants),
            Dominates = all(fun(?EXCEPTION_BLOCK) ->
                                    %% This block defines no variables and calls
                                    %% erlang:error(badarg). It does not matter
                                    %% whether L dominates ?EXCEPTION_BLOCK or not;
                                    %% it is still safe to put the frame in L.
                                    true;
                               (Bl) ->
                                    is_dominated_by(Bl, L, Doms)
                            end, MustDominate),

            %% Also, this block must not be a loop header.
            IsLoopHeader = is_loop_header(L, Blocks),
            case Dominates andalso not IsLoopHeader of
                true -> yes;
                false -> ancestor
            end
    end.

%% phi_predecessors(Label, Blocks) ->
%%  Return all predecessors referenced in phi nodes.

phi_predecessors(L, Blocks) ->
    #b_blk{is=Is} = map_get(L, Blocks),
    [P || #b_set{op=phi,args=Args} <- Is, {_,P} <:- Args].

%% is_dominated_by(Label, DominatedBy, Dominators) -> true|false.
%%  Test whether block Label is dominated by block DominatedBy.

is_dominated_by(L, DomBy, Doms) ->
    DominatedBy = map_get(L, Doms),
    member(DomBy, DominatedBy).

%% need_frame(#b_blk{}) -> true|false.
%%  Test whether any of the instructions in the block requires a stack frame.

need_frame(#b_blk{is=Is,last=#b_ret{arg=Ret}}) ->
    need_frame_1(Is, {return,Ret});
need_frame(#b_blk{is=Is}) ->
    need_frame_1(Is, body).

need_frame_1([#b_set{op=new_try_tag}|_], _) ->
    true;
need_frame_1([#b_set{op=call,dst=Val}]=Is, {return,Ret}) ->
    if
        Val =:= Ret -> need_frame_1(Is, tail);
        true -> need_frame_1(Is, body)
    end;
need_frame_1([#b_set{op=call,args=[Func|_]}|Is], Context) ->
    case Func of
        #b_remote{mod=#b_literal{val=Mod},
                  name=#b_literal{val=Name},
                  arity=Arity} when is_atom(Mod), is_atom(Name) ->
            Context =:= body orelse
                Is =/= [] orelse
                is_trap_bif(Mod, Name, Arity);
        #b_remote{} ->
            %% This is an apply(), which always needs a frame.
            true;
        #b_local{} ->
            Context =:= body orelse Is =/= [];
        _ ->
             %% A fun call always needs a frame.
            true
    end;
need_frame_1([I|Is], Context) ->
    beam_ssa:clobbers_xregs(I) orelse need_frame_1(Is, Context);
need_frame_1([], _) -> false.

%% is_trap_bif(Mod, Name, Arity) -> true|false.
%%   Test whether we need a stack frame for this BIF.

is_trap_bif(erlang, '!', 2) -> true;
is_trap_bif(erlang, link, 1) -> true;
is_trap_bif(erlang, unlink, 1) -> true;
is_trap_bif(erlang, monitor_node, 2) -> true;
is_trap_bif(erlang, group_leader, 2) -> true;
is_trap_bif(erlang, exit, 2) -> true;
is_trap_bif(_, _, _) -> false.

%%%
%%% Fix variables used in matching in receive.
%%%
%%% The loop_rec/2 instruction may return a reference to a
%%% message outside of any heap or heap fragment. If the message
%%% does not match, it is not allowed to store any reference to
%%% the message (or part of the message) on the stack. If we do,
%%% the message will be corrupted if there happens to be a GC.
%%%
%%% Here we make sure to introduce copies of variables that are
%%% matched out and subsequently used after the remove_message/0
%%% instructions. That will make sure that only X registers are
%%% used during matching.
%%%
%%% Depending on where variables are defined and used, they must
%%% be handled in two different ways.
%%%
%%% Variables that are always defined in the receive (before branching
%%% out into the different clauses of the receive) and used after the
%%% receive must be handled in the following way: Before each
%%% remove_message instruction, each such variable must be copied, and
%%% all variables must be consolidated using a phi node in the
%%% common exit block for the receive.
%%%
%%% Variables that are matched out and used in the same clause
%%% need copy instructions before the remove_message instruction
%%% in that clause.
%%%

fix_receives(#st{ssa=Blocks0,cnt=Count0}=St) ->
    {Blocks,Count} = fix_receives_1(maps:to_list(Blocks0),
                                    Blocks0, Count0),
    St#st{ssa=Blocks,cnt=Count}.

fix_receives_1([{L,Blk}|Ls], Blocks0, Count0) ->
    case Blk of
        #b_blk{is=[#b_set{op=peek_message}|_]} ->
            Rm0 = find_rm_blocks(L, Blocks0),
            {Rm,Blocks1,Count1} = split_rm_blocks(Rm0, Blocks0, Count0, []),
            LoopExit = find_loop_exit(Rm, Blocks1),
            RPO = beam_ssa:rpo([L], Blocks1),
            Defs0 = beam_ssa:def(RPO, Blocks1),
            CommonUsed = recv_common(Defs0, LoopExit, Blocks1),
            {Blocks2,Count2} = recv_crit_edges(Rm, LoopExit, Blocks1, Count1),
            {Blocks3,Count3} = recv_fix_common(CommonUsed, LoopExit, Rm,
                                               Blocks2, Count2),
            Defs = ordsets:subtract(Defs0, CommonUsed),
            {Blocks,Count} = fix_receive(Rm, Defs, Blocks3, Count3),
            fix_receives_1(Ls, Blocks, Count);
        #b_blk{} ->
            fix_receives_1(Ls, Blocks0, Count0)
    end;
fix_receives_1([], Blocks, Count) ->
    {Blocks,Count}.

split_rm_blocks([L|Ls], Blocks0, Count0, Acc) ->
    #b_blk{is=Is} = map_get(L, Blocks0),
    case need_split(Is) of
        false ->
            %% Don't split because there are no unsafe instructions.
            split_rm_blocks(Ls, Blocks0, Count0, [L|Acc]);
        true ->
            %% An unsafe instruction, such as `bs_get_tail`, was
            %% found. Split the block before `remove_message`.
            P = fun(#b_set{op=Op}) ->
                        Op =:= remove_message
                end,
            Next = Count0,
            {Blocks,Count} = beam_ssa:split_blocks_before([L], P,
                                                          Blocks0, Count0),
            true = Count0 =/= Count,            %Assertion.
            split_rm_blocks(Ls, Blocks, Count, [Next|Acc])
    end;
split_rm_blocks([], Blocks, Count, Acc) ->
    {reverse(Acc),Blocks,Count}.

need_split([#b_set{op=Op}|T]) ->
    case Op of
        %% Unnecessarily splitting the block can introduce extra
        %% `move` instructions, so we will avoid splitting as long
        %% there are only known safe instructions before the
        %% `remove_message` instruction.
        get_tuple_element -> need_split(T);
        recv_marker_clear -> need_split(T);
        remove_message -> false;
        _ -> true
    end.

recv_common(_Defs, none, _Blocks) ->
    %% There is no common exit block because receive is used
    %% in the tail position of a function.
    [];
recv_common(Defs, Exit, Blocks) ->
    RPO = beam_ssa:rpo([Exit], Blocks),
    {ExitDefs,ExitUnused} = beam_ssa:def_unused(RPO, Defs, Blocks),
    Def = ordsets:subtract(Defs, ExitDefs),
    ordsets:subtract(Def, ExitUnused).

%% recv_crit_edges([RemoveMessageLabel], LoopExit,
%%                 Blocks0, Count0) -> {Blocks,Count}.
%%
%%  Adds dummy blocks on all conditional jumps to the exit block so that
%%  recv_fix_common/5 can insert phi nodes without having to worry about
%%  critical edges.

recv_crit_edges(_Rms, none, Blocks0, Count0) ->
    {Blocks0, Count0};
recv_crit_edges(Rms, Exit, Blocks0, Count0) ->
    Ls = beam_ssa:rpo(Rms, Blocks0),
    rce_insert_edges(Ls, Exit, Count0, Blocks0).

rce_insert_edges([L | Ls], Exit, Count0, Blocks0) ->
    Successors = beam_ssa:successors(map_get(L, Blocks0)),
    case member(Exit, Successors) of
        true when Successors =/= [Exit] ->
            {Blocks, Count} = rce_insert_edge(L, Exit, Count0, Blocks0),
            rce_insert_edges(Ls, Exit, Count, Blocks);
        _ ->
            rce_insert_edges(Ls, Exit, Count0, Blocks0)
    end;
rce_insert_edges([], _Exit, Count, Blocks) ->
    {Blocks, Count}.

rce_insert_edge(L, Exit, Count, Blocks0) ->
    #b_blk{last=Last0} = FromBlk0 = map_get(L, Blocks0),

    ToExit = #b_br{bool=#b_literal{val=true},succ=Exit,fail=Exit},

    FromBlk = FromBlk0#b_blk{last=rce_reroute_terminator(Last0, Exit, Count)},
    EdgeBlk = #b_blk{anno=#{},is=[],last=ToExit},

    Blocks = Blocks0#{ Count => EdgeBlk, L => FromBlk },
    {Blocks, Count + 1}.

rce_reroute_terminator(#b_br{succ=Exit}=Last, Exit, New) ->
    rce_reroute_terminator(Last#b_br{succ=New}, Exit, New);
rce_reroute_terminator(#b_br{fail=Exit}=Last, Exit, New) ->
    rce_reroute_terminator(Last#b_br{fail=New}, Exit, New);
rce_reroute_terminator(#b_br{}=Last, _Exit, _New) ->
    Last;
rce_reroute_terminator(#b_switch{fail=Exit}=Last, Exit, New) ->
    rce_reroute_terminator(Last#b_switch{fail=New}, Exit, New);
rce_reroute_terminator(#b_switch{list=List0}=Last, Exit, New) ->
    List = [if
                Lbl =:= Exit -> {Arg, New};
                Lbl =/= Exit -> {Arg, Lbl}
            end || {Arg, Lbl} <:- List0],
    Last#b_switch{list=List}.

%% recv_fix_common([CommonVar], LoopExit, [RemoveMessageLabel],
%%                 Blocks0, Count0) -> {Blocks,Count}.
%%  Handle variables alwys defined in a receive and used
%%  in the exit block following the receive.

recv_fix_common([Msg0|T], Exit, Rm, Blocks0, Count0) ->
    {Msg,Count1} = new_var(Count0),
    RPO = beam_ssa:rpo([Exit], Blocks0),
    Blocks1 = beam_ssa:rename_vars(#{Msg0=>Msg}, RPO, Blocks0),
    {MsgVars,Count} = new_vars(length(Rm), Count1),
    PhiArgs = fix_exit_phi_args(MsgVars, Rm, Exit, Blocks1),
    Phi = #b_set{op=phi,dst=Msg,args=PhiArgs},
    ExitBlk0 = map_get(Exit, Blocks1),
    ExitBlk = ExitBlk0#b_blk{is=[Phi|ExitBlk0#b_blk.is]},
    Blocks2 = Blocks1#{Exit:=ExitBlk},
    Blocks = recv_fix_common_1(MsgVars, Rm, Msg0, Blocks2),
    recv_fix_common(T, Exit, Rm, Blocks, Count);
recv_fix_common([], _, _, Blocks, Count) ->
    {Blocks,Count}.

recv_fix_common_1([V|Vs], [Rm|Rms], Msg, Blocks0) ->
    Ren = #{Msg=>V},
    RPO = beam_ssa:rpo([Rm], Blocks0),
    Blocks1 = beam_ssa:rename_vars(Ren, RPO, Blocks0),
    #b_blk{is=Is0} = Blk0 = map_get(Rm, Blocks1),
    Copy = #b_set{op=copy,dst=V,args=[Msg]},
    Is = [Copy|Is0],
    Blk = Blk0#b_blk{is=Is},
    Blocks = Blocks1#{Rm:=Blk},
    recv_fix_common_1(Vs, Rms, Msg, Blocks);
recv_fix_common_1([], [], _Msg, Blocks) -> Blocks.

fix_exit_phi_args(Vs, Rms, Exit, Blocks) ->
    [{V,Pred} ||
        V <- Vs && Rm <- Rms,
        Pred <- exit_predecessors(beam_ssa:rpo([Rm], Blocks), Exit, Blocks)].

exit_predecessors([L|Ls], Exit, Blocks) ->
    Blk = map_get(L, Blocks),
    case member(Exit, beam_ssa:successors(Blk)) of
        true ->
            [L|exit_predecessors(Ls, Exit, Blocks)];
        false ->
            exit_predecessors(Ls, Exit, Blocks)
    end;
exit_predecessors([], _Exit, _Blocks) -> [].

%% fix_receive([Label], Defs, Blocks0, Count0) -> {Blocks,Count}.
%%  Add a copy instruction for all variables that are matched out and
%%  later used within a clause of the receive.

fix_receive([L|Ls], Defs, Blocks0, Count0) ->
    RPO = beam_ssa:rpo([L], Blocks0),
    {RmDefs,Unused} = beam_ssa:def_unused(RPO, Defs, Blocks0),
    Def = ordsets:subtract(Defs, RmDefs),
    Used = ordsets:subtract(Def, Unused),
    {NewVars,Count} = new_vars(length(Used), Count0),
    Ren = zip(Used, NewVars),
    Blocks1 = beam_ssa:rename_vars(Ren, RPO, Blocks0),
    #b_blk{is=Is0} = Blk1 = map_get(L, Blocks1),
    Is = [#b_set{op=copy,dst=New,args=[Old]} ||
             {Old,New} <:- Ren] ++ Is0,
    Blk = Blk1#b_blk{is=Is},
    Blocks = Blocks1#{L:=Blk},
    fix_receive(Ls, Defs, Blocks, Count);
fix_receive([], _Defs, Blocks, Count) ->
    {Blocks,Count}.

%% find_loop_exit([Label], Blocks) -> Label | none.
%%  Given the list of all blocks with the remove_message instructions
%%  for this receive, find the block to which control is transferred
%%  when the receive loop is exited (if any).

find_loop_exit([_,_|_]=RmBlocks, Blocks) ->
    %% We used to only analyze the path from two of the remove_message
    %% blocks. That would fail to find a common block if one or both
    %% of the blocks happened to raise an exception. To be sure that
    %% we always find a common block if there is one (shared by at
    %% least two clauses), we must analyze the path from all
    %% remove_message blocks.
    RPO = beam_ssa:rpo(Blocks),
    {Dominators,_} = beam_ssa:dominators(RPO, Blocks),
    RmSet = sets:from_list(RmBlocks),
    RmRPO = beam_ssa:rpo(RmBlocks, Blocks),
    find_loop_exit_1(RmRPO, RmSet, Dominators, Blocks);
find_loop_exit(_, _) ->
    %% There is (at most) a single clause. There is no common
    %% loop exit block.
    none.

find_loop_exit_1([?EXCEPTION_BLOCK|Ls], RmSet, Dominators, Blocks) ->
    %% ?EXCEPTION_BLOCK is a marker and not an actual block, so it is not
    %% the block we are looking for.
    find_loop_exit_1(Ls, RmSet, Dominators, Blocks);
find_loop_exit_1([L|Ls0], RmSet, Dominators, Blocks) ->
    DomBy = map_get(L, Dominators),
    case any(fun(E) -> sets:is_element(E, RmSet) end, DomBy) of
        true ->
            %% This block is dominated by one of the remove_message blocks,
            %% which means that the block is part of only one clause.
            %% It is not the block we are looking for.
            find_loop_exit_1(Ls0, RmSet, Dominators, Blocks);
        false ->
            %% This block is the first block that is not dominated by
            %% any of the blocks with remove_message instructions.
            case map_get(L, Blocks) of
                #b_blk{is=[#b_set{op=landingpad}|_]} ->
                    %% This is the landing pad reached when an
                    %% exception is caught. It is not the block
                    %% we are looking for. Furthermore, none of the
                    %% blocks reachable from this block can be
                    %% the exit block we are looking for.
                    Ls = Ls0 -- beam_ssa:rpo([L], Blocks),
                    find_loop_exit_1(Ls, RmSet, Dominators, Blocks);
                #b_blk{} ->
                    %% This block is not dominated by any of the receive
                    %% clauses and is not the landing pad for an exception.
                    %% It is the common exit block we are looking for.
                    L
            end
    end;
find_loop_exit_1([], _, _, _) ->
    %% None of clauses transfers control to a common block after the receive
    %% statement. That means that the receive statement is a the end of a
    %% function (or that all clauses raise exceptions).
    none.

%% find_rm_blocks(StartLabel, Blocks) -> [Label].
%%  Find all blocks that start with remove_message within the receive
%%  loop whose peek_message label is StartLabel.

find_rm_blocks(L, Blocks) ->
    Seen = gb_sets:singleton(L),
    Blk = map_get(L, Blocks),
    Succ = beam_ssa:successors(Blk),
    find_rm_blocks_1(Succ, Seen, Blocks).

find_rm_blocks_1([L|Ls], Seen0, Blocks) ->
    case gb_sets:is_member(L, Seen0) of
        true ->
            find_rm_blocks_1(Ls, Seen0, Blocks);
        false ->
            Seen = gb_sets:insert(L, Seen0),
            Blk = map_get(L, Blocks),
            case find_rm_act(Blk#b_blk.is) of
                prune ->
                    %% Looping back. Don't look at any successors.
                    find_rm_blocks_1(Ls, Seen, Blocks);
                continue ->
                    %% Neutral block. Do nothing here, but look at
                    %% all successors.
                    Succ = beam_ssa:successors(Blk),
                    find_rm_blocks_1(Succ++Ls, Seen, Blocks);
                found ->
                    %% Found remove_message instruction.
                    [L|find_rm_blocks_1(Ls, Seen, Blocks)]
            end
    end;
find_rm_blocks_1([], _, _) -> [].

find_rm_act([#b_set{op=Op}|Is]) ->
    case Op of
        remove_message -> found;
        peek_message -> prune;
        recv_next -> prune;
        wait_timeout -> prune;
        _ -> find_rm_act(Is)
    end;
find_rm_act([]) ->
    continue.

%%%
%%% Find out which variables need to be stored in Y registers.
%%%

-record(dk, {d :: ordsets:ordset(b_var()),
             k :: sets:set(b_var())
            }).

%% find_yregs(St0) -> St.
%%  Find all variables that must be stored in Y registers. Annotate
%%  the blocks that allocate frames with the set of Y registers
%%  used within that stack frame.
%%
%%  Basically, we following all execution paths starting from a block
%%  that allocates a frame, keeping track of of all defined registers
%%  and all registers killed by an instruction that clobbers X
%%  registers. For every use of a variable, we check if if it is in
%%  the set of killed variables; if it is, it must be stored in an Y
%%  register.

find_yregs(#st{frames=[]}=St) ->
    St;
find_yregs(#st{frames=[_|_]=Frames,args=Args,ssa=Blocks0}=St) ->
    FrameDefs = find_defs(Frames, Blocks0, [V || #b_var{}=V <- Args]),
    Blocks = find_yregs_1(FrameDefs, Blocks0),
    St#st{ssa=Blocks}.

find_yregs_1([{F,Defs}|Fs], Blocks0) ->
    DK = #dk{d=Defs,k=sets:new()},
    D0 = #{F => DK,?EXCEPTION_BLOCK => DK#dk{d=[]}},
    Ls = beam_ssa:rpo([F], Blocks0),
    Yregs0 = sets:new(),
    Yregs = find_yregs_2(Ls, Blocks0, D0, Yregs0),
    Blk0 = map_get(F, Blocks0),
    Blk = beam_ssa:add_anno(yregs, Yregs, Blk0),
    Blocks = Blocks0#{F:=Blk},
    find_yregs_1(Fs, Blocks);
find_yregs_1([], Blocks) -> Blocks.

find_yregs_2([L|Ls], Blocks0, D0, Yregs0) ->
    Blk0 = map_get(L, Blocks0),
    #b_blk{is=Is,last=Last} = Blk0,
    Ys0 = map_get(L, D0),
    {Yregs1,Ys} = find_yregs_is(Is, Ys0, Yregs0),
    Yregs = find_yregs_terminator(Last, Ys, Yregs1),
    Successors = beam_ssa:successors(Blk0),
    D = find_update_succ(Successors, Ys, D0),
    find_yregs_2(Ls, Blocks0, D, Yregs);
find_yregs_2([], _Blocks, _D, Yregs) -> Yregs.

find_defs(Frames, Blocks, Defs) ->
    Seen = gb_sets:empty(),
    FramesSet = gb_sets:from_list(Frames),
    {FrameDefs,_} = find_defs_1([0], Blocks, FramesSet, Seen, Defs, []),
    FrameDefs.

find_defs_1([L|Ls], Blocks, Frames, Seen0, Defs0, Acc0) ->
    case gb_sets:is_member(L, Frames) of
        true ->
            OrderedDefs = ordsets:from_list(Defs0),
            find_defs_1(Ls, Blocks, Frames, Seen0, Defs0,
                        [{L,OrderedDefs}|Acc0]);
        false ->
            case gb_sets:is_member(L, Seen0) of
                true ->
                    find_defs_1(Ls, Blocks, Frames, Seen0, Defs0, Acc0);
                false ->
                    Seen1 = gb_sets:insert(L, Seen0),
                    {Acc,Seen} = find_defs_1(Ls, Blocks, Frames, Seen1, Defs0, Acc0),
                    #b_blk{is=Is} = Blk = map_get(L, Blocks),
                    Defs = find_defs_is(Is, Defs0),
                    Successors = beam_ssa:successors(Blk),
                    find_defs_1(Successors, Blocks, Frames, Seen, Defs, Acc)
            end
    end;
find_defs_1([], _, _, Seen, _, Acc) ->
    {Acc,Seen}.

find_defs_is([#b_set{dst=Dst}|Is], Acc) ->
    find_defs_is(Is, [Dst|Acc]);
find_defs_is([], Acc) -> Acc.

find_update_succ([?EXCEPTION_BLOCK|Ss], DK, D) ->
    find_update_succ(Ss, DK, D);
find_update_succ([S|Ss], #dk{d=Defs0,k=Killed0}=DK0, D0) ->
    case D0 of
        #{S:=#dk{d=Defs1,k=Killed1}} ->
            Defs = ordsets:intersection(Defs0, Defs1),
            Killed = sets:union(Killed0, Killed1),
            DK = #dk{d=Defs,k=Killed},
            D = D0#{S:=DK},
            find_update_succ(Ss, DK0, D);
        #{} ->
            D = D0#{S=>DK0},
            find_update_succ(Ss, DK0, D)
    end;
find_update_succ([], _, D) -> D.

find_yregs_is([#b_set{dst=Dst}=I|Is], #dk{d=Defs0,k=Killed0}=Ys, Yregs0) ->
    Yregs1 = intersect_used(I, Killed0),
    Yregs = sets:union(Yregs0, Yregs1),
    case beam_ssa:clobbers_xregs(I) of
        false ->
            Defs = ordsets:add_element(Dst, Defs0),
            find_yregs_is(Is, Ys#dk{d=Defs}, Yregs);
        true ->
            Killed = sets:union(sets:from_list(Defs0), Killed0),
            Defs = [Dst],
            find_yregs_is(Is, Ys#dk{d=Defs,k=Killed}, Yregs)
    end;
find_yregs_is([], Ys, Yregs) -> {Yregs,Ys}.

find_yregs_terminator(Terminator, #dk{k=Killed}, Yregs0) ->
    Yregs = intersect_used(Terminator, Killed),
    sets:union(Yregs0, Yregs).

intersect_used(#b_br{bool=#b_var{}=V}, Set) ->
    intersect_used_keep_singleton(V, Set);
intersect_used(#b_ret{arg=#b_var{}=V}, Set) ->
    intersect_used_keep_singleton(V, Set);
intersect_used(#b_set{op=phi,args=Args}, Set) ->
    sets:from_list([V || {#b_var{}=V,_} <- Args, sets:is_element(V, Set)]);
intersect_used(#b_set{args=Args}, Set) ->
    sets:from_list(intersect_used_keep(used_args(Args), Set));
intersect_used(#b_switch{arg=#b_var{}=V}, Set) ->
    intersect_used_keep_singleton(V, Set);
intersect_used(_, _) -> sets:new().

intersect_used_keep_singleton(V, Set) ->
    case sets:is_element(V, Set) of
        true -> sets:from_list([V]);
        false -> sets:new()
    end.

intersect_used_keep(Vs, Set) ->
    [V || V <- Vs, sets:is_element(V, Set)].

used_args([#b_var{}=V|As]) ->
    [V|used_args(As)];
used_args([#b_remote{mod=Mod,name=Name}|As]) ->
    used_args([Mod,Name|As]);
used_args([_|As]) ->
    used_args(As);
used_args([]) -> [].

%%%
%%% Try to reduce the size of the stack frame, by adding an explicit
%%% 'copy' instructions for return values from 'call' that need to be
%%% saved in Y registers. Here is an example to show how that's
%%% useful. First, here is the Erlang code:
%%%
%%% f(Pid) ->
%%%    Res = foo(42),
%%%    _ = node(Pid),
%%%    bar(),
%%%    Res.
%%%
%%% Compiled to SSA format, the main part of the code looks like this:
%%%
%%% 0:
%%%   Res = call local literal foo/1, literal 42
%%%   @ssa_bool:1 = succeeded:body Res
%%%   br @ssa_bool:1, label 2, label 1
%%% 2:
%%%   _1 = bif:node Pid
%%%   @ssa_bool:2 = succeeded:body _1
%%%   br @ssa_bool:2, label 3, label 1
%%% 3:
%%%   _2 = call local literal bar/0
%%%   @ssa_bool:3 = succeeded:body _2
%%%   br @ssa_bool:3, label 4, label 1
%%% 4:
%%%   ret Res
%%%
%%% It can be seen that the variables Pid and Res must be saved in Y
%%% registers in order to survive the function calls. A previous sub
%%% pass has inserted a 'copy' instruction to save the value of the
%%% variable Pid:
%%%
%%% 0:
%%%   Pid:4 = copy Pid
%%%   Res = call local literal foo/1, literal 42
%%%   @ssa_bool:1 = succeeded:body Res
%%%   br @ssa_bool:1, label 2, label 1
%%% 2:
%%%   _1 = bif:node Pid:4
%%%   @ssa_bool:2 = succeeded:body _1
%%%   br @ssa_bool:2, label 3, label 1
%%% 3:
%%%   _2 = call local literal bar/0
%%%   @ssa_bool:3 = succeeded:body _2
%%%   br @ssa_bool:3, label 4, label 1
%%% 4:
%%%   ret Res
%%%
%%% The Res and Pid:4 variables must be assigned to different Y registers
%%% because they are live at the same time. copy_retval() inserts a
%%% 'copy' instruction to copy Res to a new variable:
%%%
%%% 0:
%%%   Pid:4 = copy Pid
%%%   Res:6 = call local literal foo/1, literal 42
%%%   @ssa_bool:1 = succeeded:body Res:6
%%%   br @ssa_bool:1, label 2, label 1
%%% 2:
%%%   _1 = bif:node Pid:4
%%%   @ssa_bool:2 = succeeded:body _1
%%%   br @ssa_bool:2, label 3, label 1
%%% 3:
%%%   Res = copy Res:6
%%%   _2 = call local literal bar/0
%%%   @ssa_bool:3 = succeeded:body _2
%%%   br @ssa_bool:3, label 4, label 1
%%% 4:
%%%   ret Res
%%%
%%% The new variable Res:6 is used to capture the return value from the call.
%%% The variables Pid:4 and Res are no longer live at the same time, so they
%%% can be assigned to the same Y register.
%%%

copy_retval(#st{frames=Frames,ssa=Blocks0,cnt=Count0}=St) ->
    {Blocks,Count} = copy_retval_1(Frames, Blocks0, Count0),
    St#st{ssa=Blocks,cnt=Count}.

copy_retval_1([F|Fs], Blocks0, Count0) ->
    #b_blk{anno=#{yregs:=Yregs0},is=Is} = map_get(F, Blocks0),
    Yregs = collect_yregs(Is, Yregs0),
    Ls = beam_ssa:rpo([F], Blocks0),
    {Blocks,Count} = copy_retval_2(Ls, Yregs, none, Blocks0, Count0),
    copy_retval_1(Fs, Blocks, Count);
copy_retval_1([], Blocks, Count) ->
    {Blocks,Count}.

collect_yregs([#b_set{op=copy,dst=Y,args=[#b_var{}=X]}|Is],
              Yregs0) ->
    true = sets:is_element(X, Yregs0),        %Assertion.
    Yregs = sets:add_element(Y, sets:del_element(X, Yregs0)),
    collect_yregs(Is, Yregs);
collect_yregs([#b_set{}|Is], Yregs) ->
    collect_yregs(Is, Yregs);
collect_yregs([], Yregs) -> Yregs.

copy_retval_2([L|Ls], Yregs, Copy0, Blocks0, Count0) ->
    #b_blk{is=Is0,last=Last} = Blk = map_get(L, Blocks0),
    RC = case {Last,Ls} of
             {#b_br{succ=Succ,fail=?EXCEPTION_BLOCK},[Succ|_]} ->
                 true;
             {_,_} ->
                 false
         end,
    case copy_retval_is(Is0, RC, Yregs, Copy0, Count0, []) of
        {Is,Count} ->
            case Copy0 =:= none andalso Count0 =:= Count of
                true ->
                    copy_retval_2(Ls, Yregs, none, Blocks0, Count0);
                false ->
                    Blocks = Blocks0#{L=>Blk#b_blk{is=Is}},
                    copy_retval_2(Ls, Yregs, none, Blocks, Count)
            end;
        {Is,Count,Copy} ->
            Blocks = Blocks0#{L=>Blk#b_blk{is=Is}},
            copy_retval_2(Ls, Yregs, Copy, Blocks, Count)
    end;
copy_retval_2([], _Yregs, none, Blocks, Count) ->
    {Blocks,Count}.

copy_retval_is([#b_set{op=call}=I0], false, Yregs, Copy, Count0, Acc0) ->
    {I,Count,Acc} = place_retval_copy(I0, Yregs, Copy, Count0, Acc0),
    {reverse(Acc, [I]),Count};
copy_retval_is([#b_set{}]=Is, false, _Yregs, Copy, Count, Acc) ->
    {reverse(Acc, acc_copy(Is, Copy)),Count};
copy_retval_is([#b_set{},#b_set{op=succeeded}]=Is, false, _Yregs, Copy, Count, Acc) ->
    {reverse(Acc, acc_copy(Is, Copy)),Count};
copy_retval_is([#b_set{op=call,dst=#b_var{}=Dst}=I0|Is], RC, Yregs,
           Copy0, Count0, Acc0) ->
    {I1,Count1,Acc} = place_retval_copy(I0, Yregs, Copy0, Count0, Acc0),
    case sets:is_element(Dst, Yregs) of
        true ->
            {NewVar,Count} = new_var(Count1),
            Copy = #b_set{anno=#{delayed_yreg_copy => true},
                          op=copy,dst=Dst,args=[NewVar]},
            I = I1#b_set{dst=NewVar},
            copy_retval_is(Is, RC, Yregs, Copy, Count, [I|Acc]);
        false ->
            copy_retval_is(Is, RC, Yregs, none, Count1, [I1|Acc])
    end;
copy_retval_is([#b_set{op=landingpad,args=[#b_literal{val='try'}|_]=Args0}=I0|Is],
               _RC, Yregs, Copy, Count, Acc0) ->
    I = I0#b_set{args=copy_sub_args(Args0, Copy)},
    Acc = [I|acc_copy(Acc0, Copy)],
    copy_landingpad(Is, Yregs, Count, Acc, []);
copy_retval_is([#b_set{args=Args0}=I0|Is], RC, Yregs, Copy, Count, Acc) ->
    I = I0#b_set{args=copy_sub_args(Args0, Copy)},
    case beam_ssa:clobbers_xregs(I) of
        true ->
            copy_retval_is(Is, RC, Yregs, none, Count, [I|acc_copy(Acc, Copy)]);
        false ->
            copy_retval_is(Is, RC, Yregs, Copy, Count, [I|Acc])
        end;
copy_retval_is([], RC, _, Copy, Count, Acc) ->
    case {Copy,RC} of
        {none,_} ->
            {reverse(Acc),Count};
        {#b_set{},true} ->
            {reverse(Acc),Count,Copy};
        {#b_set{},false} ->
            {reverse(Acc, [Copy]),Count}
    end.

%% Consider this function:
%%
%%     do_try(F) ->
%%        try F()
%%        catch
%%            C:R:Stk ->
%%                {'EXIT',C,R,Stk}
%%        end.
%%
%% That would result in the following SSA code for the `catch` clause:
%%
%%     z0/_16 = landingpad `'try'`, y2/_14
%%     y1/_4 = extract z0/_16, `0`
%%     y0/_3 = extract z0/_16, `1`
%%     x0/_2 = extract z0/_16, `2`
%%     z0/_17 = kill_try_tag y2/_14
%%     x0/Stk = build_stacktrace x0/_2
%%
%% Note that three Y registers are required. That can be reduced to
%% two Y registers if we rewrite the code like so:
%%
%%      x0/_37 = extract z0/_16, `0`
%%      x1/_38 = extract z0/_16, `1`
%%      x2/_2 = extract z0/_16, `2`
%%      z0/_17 = kill_try_tag y1/_14
%%      y1/_3 = copy x1/_38
%%      y0/_4 = copy x0/_37
%%

copy_landingpad([I0|Is], Yregs, Count0, Acc0, Copies0) ->
    case I0 of
        #b_set{dst=Dst,op=extract} ->
            case sets:is_element(Dst, Yregs) of
                true ->
                    {NewDst,Count} = new_var(Count0),
                    Copies = [#b_set{op=copy,dst=Dst,args=[NewDst]}|Copies0],
                    I = I0#b_set{dst=NewDst},
                    Acc = [I|Acc0],
                    copy_landingpad(Is, Yregs, Count, Acc, Copies);
                false ->
                    Acc = [I0|Acc0],
                    copy_landingpad(Is, Yregs, Count0, Acc, Copies0)
            end;
        #b_set{op=kill_try_tag} ->
            {reverse(Acc0, [I0|Copies0 ++ Is]),Count0}
    end.

%%
%% Consider this code:
%%
%%   P = ...
%%   Q = ...
%%   ...
%%   A = call foo/0
%%   A1 = copy A
%%   B = call bar/2, P, Q
%%
%% If the P or Q variables are no longer used after this code, one of
%% their Y registers can't be reused for A. To allow one of the Y registers to
%% be reused we will need to insert 'copy' instructions for arguments
%% that are in Y registers:
%%
%%   P = ...
%%   Q = ...
%%   ...
%%   A1 = call foo/0
%%   Q1 = copy Q
%%   P1 = copy P
%%   A = copy A1
%%   B = call bar/2, P1, Q1
%%
%% Note that copies of the arguments are done in reverse order to help the
%% reserve_xregs/3 function place the copies into the X registers they will
%% need to be in.
%%
%% For this example, P1 needs to be in x0 and Q1 needs to be in x1. If we
%% would copy the arguments in order the registers would be assigned like
%% this:
%%
%%   x0/A1 = call foo/0
%%   x1/P1 = copy P
%%   x2/Q1 = copy Q
%%   A = copy A1
%%   B = call bar/2, P1, Q1
%%
%% That is, both P1 and Q1 would be misplaced and would have to be
%% moved to their correct registers before the call. However, with the
%% copies in reverse order and with a little help from
%% reserve_xregs/3, at least the Q1 variable can be can be placed in
%% the correct register:
%%
%%   x0/A1 = call foo/0
%%   x1/Q1 = copy Q
%%   x2/P1 = copy P
%%   A = copy A1
%%   B = call bar/2, P1, Q1
%%
%% In general, all but the first argument can be placed in their correct registers.
%%

place_retval_copy(I, _Yregs, none, Count, Acc) ->
    %% There is no copy of a previous return value, so there is nothing
    %% to gain by copying the function arguments.
    {I,Count,Acc};
place_retval_copy(#b_set{args=[F|Args0]}=I0, Yregs0, RetCopy, Count0, Acc0) ->
    %% Copy function arguments, but make sure that we don't make an extra
    %% copy of the previous return value.
    #b_set{dst=Avoid} = RetCopy,
    Yregs = sets:del_element(Avoid, Yregs0),
    {Args,Acc1,Count} = copy_func_args(Args0, Yregs, Acc0, Count0),
    I = I0#b_set{args=[F|Args]},

    %% Place the copy instruction for the previous return value after the
    %% copy instruction for the arguments.
    Acc = [RetCopy|Acc1],
    {I,Count,Acc}.

copy_func_args(Args, Yregs, Acc, Count) ->
    copy_func_args_1(reverse(Args), Yregs, Acc, [], Count).

copy_func_args_1([#b_var{}=A|As], Yregs, InstrAcc, ArgAcc, Count0) ->
    case sets:is_element(A, Yregs) of
        true ->
            {NewVar,Count} = new_var(Count0),
            Copy = #b_set{op=copy,dst=NewVar,args=[A]},
            copy_func_args_1(As, Yregs, [Copy|InstrAcc], [NewVar|ArgAcc], Count);
        false ->
            copy_func_args_1(As, Yregs, InstrAcc, [A|ArgAcc], Count0)
    end;
copy_func_args_1([A|As], Yregs, InstrAcc, ArgAcc, Count) ->
    copy_func_args_1(As, Yregs, InstrAcc, [A|ArgAcc], Count);
copy_func_args_1([], _Yregs, InstrAcc, ArgAcc, Count) ->
    {ArgAcc,InstrAcc,Count}.

acc_copy(Acc, none) -> Acc;
acc_copy(Acc, #b_set{}=Copy) -> [Copy|Acc].

copy_sub_args(Args, none) ->
    Args;
copy_sub_args(Args, #b_set{dst=Dst,args=[Src]}) ->
    [sub_arg(A, Dst, Src) || A <- Args].

sub_arg(Old, Old, New) -> New;
sub_arg(Old, _, _) -> Old.

%%%
%%% Consider:
%%%
%%%   x1/Hd = get_hd x0/Cons
%%%   y0/Tl = get_tl x0/Cons
%%%
%%% Register x0 can't be reused for Hd. If Hd needs to be in x0,
%%% a 'move' instruction must be inserted.
%%%
%%% If we swap get_hd and get_tl when Tl is in a Y register,
%%% x0 can be used for Hd if Cons is not used again:
%%%
%%%   y0/Tl = get_tl x0/Cons
%%%   x0/Hd = get_hd x0/Cons
%%%

opt_get_list(#st{ssa=Blocks,res=Res}=St) ->
    ResMap = maps:from_list(Res),
    Ls = beam_ssa:rpo(Blocks),
    St#st{ssa=opt_get_list_1(Ls, ResMap, Blocks)}.

opt_get_list_1([L|Ls], Res, Blocks0) ->
    #b_blk{is=Is0} = Blk = map_get(L, Blocks0),
    case opt_get_list_is(Is0, Res, [], false) of
        no ->
            opt_get_list_1(Ls, Res, Blocks0);
        {yes,Is} ->
            Blocks = Blocks0#{L:=Blk#b_blk{is=Is}},
            opt_get_list_1(Ls, Res, Blocks)
    end;
opt_get_list_1([], _, Blocks) -> Blocks.

opt_get_list_is([#b_set{op=get_hd,dst=Hd,
                        args=[Cons]}=GetHd,
                 #b_set{op=get_tl,dst=Tl,
                        args=[Cons]}=GetTl|Is],
                Res, Acc, Changed) ->
    %% Note that when this pass is run, only Y registers have
    %% reservations. The absence of an entry for a variable therefore
    %% means that the variable will be in an X register.
    case Res of
        #{Hd:={y,_}} ->
            %% Hd will be in a Y register. Don't swap.
            opt_get_list_is([GetTl|Is], Res, [GetHd|Acc], Changed);
        #{Tl:={y,_}} ->
            %% Tl will be in a Y register. Swap.
            opt_get_list_is([GetHd|Is], Res, [GetTl|Acc], true);
        #{} ->
            %% Both are in X registers. Nothing to do.
            opt_get_list_is([GetTl|Is], Res, [GetHd|Acc], Changed)
    end;
opt_get_list_is([I|Is], Res, Acc, Changed) ->
    opt_get_list_is(Is, Res, [I|Acc], Changed);
opt_get_list_is([], _Res, Acc, Changed) ->
    case Changed of
        true ->
            {yes,reverse(Acc)};
        false ->
            no
    end.

%%%
%%% Number instructions in the order they are executed.
%%%

%% number_instructions(St0) -> St.
%%  Number instructions in the order they are executed. Use a step
%%  size of 2. Don't number phi instructions. All phi variables in
%%  a block will be live one unit before the first non-phi instruction
%%  in the block.

number_instructions(#st{ssa=Blocks0}=St) ->
    Ls = beam_ssa:rpo(Blocks0),
    St#st{ssa=number_is_1(Ls, 1, Blocks0)}.

number_is_1([L|Ls], N0, Blocks0) ->
    #b_blk{is=Is0,last=Last0} = Bl0 = map_get(L, Blocks0),
    {Is,N1} = number_is_2(Is0, N0, []),
    Last = beam_ssa:add_anno(n, N1, Last0),
    N = N1 + 2,
    Bl = Bl0#b_blk{is=Is,last=Last},
    Blocks = Blocks0#{L:=Bl},
    number_is_1(Ls, N, Blocks);
number_is_1([], _, Blocks) -> Blocks.

number_is_2([#b_set{op=phi}=I|Is], N, Acc) ->
    number_is_2(Is, N, [I|Acc]);
number_is_2([I0|Is], N, Acc) ->
    I = beam_ssa:add_anno(n, N, I0),
    number_is_2(Is, N+2, [I|Acc]);
number_is_2([], N, Acc) ->
    {reverse(Acc),N}.

%%%
%%% Calculate live intervals for all variables in this function.
%%%
%%% This code uses the algorithm and terminology from [3].
%%%
%%% For each variable, we calculate its live interval. The live
%%% interval is a list of ranges, where a range is a tuple
%%% {Def,LastUse}.  Def is the instruction number for the instruction
%%% that first defines the variable and LastUse is instruction number
%%% of the last instruction that uses it.
%%%
%%% We traverse instruction in post order so that we will see the last
%%% use before we see the definition.
%%%

live_intervals(#st{args=Args,ssa=Blocks}=St) ->
    PO = reverse(beam_ssa:rpo(Blocks)),
    Intervals0 = live_interval_blk(PO, Blocks, #{}, #{}),
    Intervals1 = add_ranges([{V,{0,1}} || #b_var{}=V <- Args], Intervals0),
    Intervals = maps:to_list(Intervals1),
    St#st{intervals=Intervals}.

live_interval_blk([L|Ls], Blocks, LiveMap0, Intervals0) ->
    Blk = map_get(L, Blocks),
    Successors = beam_ssa:successors(Blk),
    Live1 = live_in_successors(Successors, L, Blocks, LiveMap0),

    %% Add default ranges for all variables that are live in the
    %% successors.
    #b_blk{is=Is,last=Last} = Blk,
    FirstNumber = first_number(Is, Last),
    DefaultRange = {FirstNumber,1+beam_ssa:get_anno(n, Last)},
    Ranges0 = [{V,DefaultRange} || V <- Live1],

    case {Is,Last} of
        {[],#b_br{bool=#b_literal{val=true}}} ->
            %% Optimize the interval calculation for blocks without variables.
            Intervals = add_ranges(Ranges0, Intervals0),
            LiveMap = LiveMap0#{L => Live1},
            live_interval_blk(Ls, Blocks, LiveMap, Intervals);
        {_,_} ->
            %% Update the ranges. Variables whose last use is in this
            %% block will be added, and variables that are defined
            %% in this block will have their starting instruction
            %% number updated.
            %%
            %% We use a gb_tree instead of a map because conversion to and
            %% from an orddict is faster.
            Ranges1 = gb_trees:from_orddict(Ranges0),
            Ranges2 = live_interval_last(Last, FirstNumber, Ranges1),
            Ranges3 = live_interval_blk_is(Is, FirstNumber, Ranges2),
            Ranges = gb_trees:to_list(Ranges3),

            %% Update the interval for each variable.
            Intervals = add_ranges(Ranges, Intervals0),

            %% Update what is live at the beginning of this block and
            %% store it.
            Live = [V || {V,{From,_To}} <:- Ranges,
                         From =< FirstNumber],
            LiveMap = LiveMap0#{L => Live},
            live_interval_blk(Ls, Blocks, LiveMap, Intervals)
    end;
live_interval_blk([], _Blocks, _LiveMap, Intervals) ->
    Intervals.

live_interval_last(I, FirstNumber, Ranges) ->
    N = beam_ssa:get_anno(n, I),
    Used = beam_ssa:used(I),
    update_used(Used, FirstNumber, N, Ranges).

live_interval_blk_is([#b_set{op=phi,dst=Dst}|Is], FirstNumber, Acc0) ->
    Acc = live_interval_blk_is(Is, FirstNumber, Acc0),
    case gb_trees:is_defined(Dst, Acc) of
        true ->
            %% The value in the tree already has the correct starting value.
            update_def(Dst, FirstNumber, Acc);
        false ->
            %% Unused phi node -- can only happen if optimizations passes
            %% have been turned off.
            gb_trees:insert(Dst, {FirstNumber,FirstNumber}, Acc)
    end;
live_interval_blk_is([#b_set{args=Args,dst=Dst}=I|Is], FirstNumber, Acc0) ->
    Acc1 = live_interval_blk_is(Is, FirstNumber, Acc0),
    N = beam_ssa:get_anno(n, I),
    Used = used_args(Args),
    Acc = update_used(Used, FirstNumber, N, Acc1),
    update_def(Dst, N, Acc);
live_interval_blk_is([], _FirstNumber, Acc) ->
    Acc.

update_def(V, N, Ranges) ->
    case gb_trees:lookup(V, Ranges) of
        {value,{_From,To}} ->
            gb_trees:update(V, {N,To}, Ranges);
        none ->
            %% The variable is defined but never used.
            gb_trees:insert(V, {N,N}, Ranges)
    end.

update_used([V|Vs], First, N, Ranges) ->
    case gb_trees:is_defined(V, Ranges) of
        true ->
            %% Already up to date. (A later use has already been stored.)
            update_used(Vs, First, N, Ranges);
        false ->
            %% The last use of this variable. (But the first time we
            %% see it because we visit instructions in PO.)
            update_used(Vs, First, N, gb_trees:insert(V, {First,N}, Ranges))
    end;
update_used([], _First, _N, Ranges) -> Ranges.

add_ranges([{V,{A,N}=Range}|T], Map) ->
    case Map of
        #{V := [{N,Z}|Ranges]} ->
            %% Coalesce two adjacent ranges.
            add_ranges(T, Map#{V := [{A,Z}|Ranges]});
        #{V := [{A,N}|_]} ->
            %% Ignore repeated range (probably from arguments).
            add_ranges(T, Map);
        #{V := Ranges} ->
            %% This range is not adjacent to any other range.
            add_ranges(T, Map#{V := [Range|Ranges]});
        #{} ->
            %% The last use of this variable is in the current block.
            add_ranges(T, Map#{V => [Range]})
    end;
add_ranges([], Map) -> Map.

%% first_number([#b_set{}]) -> InstructionNumber.
%%  Return the number for the first instruction for the block.
%%  Note that this number is one less than the first
%%  non-phi instruction in the block.

first_number([#b_set{op=phi}|Is], Last) ->
    first_number(Is, Last);
first_number([I|_], _) ->
    beam_ssa:get_anno(n, I) - 1;
first_number([], Last) ->
    beam_ssa:get_anno(n, Last) - 1.

live_in_successors(Ls, Pred, Blocks, LiveMap) ->
    live_in_successors(Ls, Pred, Blocks, LiveMap, []).

live_in_successors([L|Ls], Pred, Blocks, LiveMap, Live0) ->
    Live1 = ordsets:union(Live0, get_live(L, LiveMap)),
    #b_blk{is=Is} = map_get(L, Blocks),
    Live = live_in_phis(Is, Pred, Live1),
    live_in_successors(Ls, Pred, Blocks, LiveMap, Live);
live_in_successors([], _, _, _, Live) -> Live.

get_live(L, LiveMap) ->
    case LiveMap of
        #{L:=Live} -> Live;
        #{} -> []
    end.

live_in_phis([#b_set{op=phi,dst=Killed,args=Args}|Is],
                 Pred, Live0) ->
    Used = [V || {#b_var{}=V,L} <- Args, L =:= Pred],
    Live1 = ordsets:union(Used, Live0),
    Live = ordsets:del_element(Killed, Live1),
    live_in_phis(Is, Pred, Live);
live_in_phis(_, _, Live) -> Live.

%%%
%%% Reserve Y registers.
%%%

%% reserve_yregs(St0) -> St.
%%  In each block that allocates a stack frame, insert instructions
%%  that copy variables that must be in Y registers (given by
%%  the `yregs` annotation) to new variables.
%%
%%  Also allocate specific Y registers for try and catch tags.
%%  The outermost try/catch tag is placed in y0, any directly
%%  nested tag in y1, and so on. Note that this is the reversed
%%  order as required by BEAM; it will be corrected later by
%%  turn_yregs().

reserve_yregs(#st{frames=Frames}=St0) ->
    foldl(fun reserve_yregs_1/2, St0, Frames).

reserve_yregs_1(L, #st{ssa=Blocks0,cnt=Count0,res=Res0}=St) ->
    Blk = map_get(L, Blocks0),
    Yregs = ordsets:from_list(sets:to_list(beam_ssa:get_anno(yregs, Blk))),
    RPO = beam_ssa:rpo([L], Blocks0),
    {Def,Unused} = beam_ssa:def_unused(RPO, Yregs, Blocks0),
    UsedYregs = ordsets:subtract(Yregs, Unused),
    DefBefore = ordsets:subtract(UsedYregs, Def),
    {BeforeVars,Blocks,Count} = rename_vars(DefBefore, L, RPO, Blocks0, Count0),
    InsideVars = ordsets:subtract(UsedYregs, DefBefore),
    ResTryTags0 = reserve_try_tags(L, Blocks),
    ResTryTags = [{V,{Reg,Count}} || {V,Reg} <:- ResTryTags0],
    Vars = BeforeVars ++ InsideVars,
    Res = [{V,{y,Count}} || V <- Vars] ++ ResTryTags ++ Res0,
    St#st{res=Res,ssa=Blocks,cnt=Count+1}.

reserve_try_tags(L, Blocks) ->
    Seen = gb_sets:empty(),
    {Res0,_} = reserve_try_tags_1([L], Blocks, Seen, #{}),
    Res1 = [maps:to_list(M) || M <- maps:values(Res0)],
    Res = [{V,{y,Y}} || {V,Y} <:- append(Res1)],
    ordsets:from_list(Res).

reserve_try_tags_1([L|Ls], Blocks, Seen0, ActMap0) ->
    case gb_sets:is_element(L, Seen0) of
        true ->
            reserve_try_tags_1(Ls, Blocks, Seen0, ActMap0);
        false ->
            Seen1 = gb_sets:insert(L, Seen0),
            #b_blk{is=Is} = Blk = map_get(L, Blocks),
            Active0 = get_active(L, ActMap0),
            Active = reserve_try_tags_is(Is, Active0),
            Successors = beam_ssa:successors(Blk),
            ActMap1 = update_act_map(Successors, Active, ActMap0),
            {ActMap,Seen} = reserve_try_tags_1(Ls, Blocks, Seen1, ActMap1),
            reserve_try_tags_1(Successors, Blocks, Seen,ActMap)
    end;
reserve_try_tags_1([], _Blocks, Seen, ActMap) ->
    {ActMap,Seen}.

get_active(L, ActMap) ->
    case ActMap of
        #{L:=Active} -> Active;
        #{} -> #{}
    end.

reserve_try_tags_is([#b_set{op=new_try_tag,dst=V}|Is], Active) ->
    N = map_size(Active),
    reserve_try_tags_is(Is, Active#{V=>N});
reserve_try_tags_is([#b_set{op=kill_try_tag,args=[Tag]}|Is], Active) ->
    reserve_try_tags_is(Is, maps:remove(Tag, Active));
reserve_try_tags_is([_|Is], Active) ->
    reserve_try_tags_is(Is, Active);
reserve_try_tags_is([], Active) -> Active.

update_act_map([L|Ls], Active0, ActMap0) ->
    case ActMap0 of
        #{L:=Active1} ->
            ActMap = ActMap0#{L=>maps:merge(Active0, Active1)},
            update_act_map(Ls, Active0, ActMap);
        #{} ->
            ActMap = ActMap0#{L=>Active0},
            update_act_map(Ls, Active0, ActMap)
    end;
update_act_map([], _, ActMap) -> ActMap.

rename_vars([], _, _, Blocks, Count) ->
    {[],Blocks,Count};
rename_vars(Vs, L, RPO, Blocks0, Count0) ->
    {NewVars,Count} = new_vars(length(Vs), Count0),
    Ren = zip(Vs, NewVars),
    Blocks1 = beam_ssa:rename_vars(Ren, RPO, Blocks0),
    #b_blk{is=Is0} = Blk0 = map_get(L, Blocks1),
    CopyIs = [#b_set{op=copy,dst=New,args=[Old]} || {Old,New} <:- Ren],
    Is = insert_after_phis(Is0, CopyIs),
    Blk = Blk0#b_blk{is=Is},
    Blocks = Blocks1#{L:=Blk},
    {NewVars,Blocks,Count}.

insert_after_phis([#b_set{op=phi}=I|Is], InsertIs) ->
    [I|insert_after_phis(Is, InsertIs)];
insert_after_phis(Is, InsertIs) ->
    InsertIs ++ Is.

%% frame_size(St0) -> St.
%%  Calculate the frame size for each block that allocates a frame.
%%  Annotate the block with the frame size. Also annotate all
%%  return instructions with {deallocate,FrameSize} to simplify
%%  code generation.

frame_size(#st{frames=Frames,regs=Regs,ssa=Blocks0}=St) ->
    Blocks = foldl(fun(L, Blks) ->
                           frame_size_1(L, Regs, Blks)
                   end, Blocks0, Frames),
    St#st{ssa=Blocks}.

frame_size_1(L, Regs, Blocks0) ->
    RPO = beam_ssa:rpo([L], Blocks0),
    Def = beam_ssa:def(RPO, Blocks0),
    Yregs0 = [map_get(V, Regs) || V <- Def, is_yreg(map_get(V, Regs))],
    Yregs = ordsets:from_list(Yregs0),
    FrameSize = length(ordsets:from_list(Yregs)),
    if
        FrameSize =/= 0 ->
            [{y,0}|_] = Yregs,                  %Assertion.
            {y,Last} = last(Yregs),
            Last = FrameSize - 1,               %Assertion.
            ok;
        true ->
            ok
    end,
    Blk0 = map_get(L, Blocks0),
    Blk = beam_ssa:add_anno(frame_size, FrameSize, Blk0),

    %% Insert an annotation for frame deallocation on
    %% each #b_ret{}.
    Blocks = Blocks0#{L:=Blk},
    Reachable = beam_ssa:rpo([L], Blocks),
    frame_deallocate(Reachable, FrameSize, Blocks).

frame_deallocate([L|Ls], Size, Blocks0) ->
    Blk0 = map_get(L, Blocks0),
    Blk = case Blk0 of
              #b_blk{last=#b_ret{}=Ret0} ->
                  Ret = beam_ssa:add_anno(deallocate, Size, Ret0),
                  Blk0#b_blk{last=Ret};
              #b_blk{} ->
                  Blk0
          end,
    Blocks = Blocks0#{L:=Blk},
    frame_deallocate(Ls, Size, Blocks);
frame_deallocate([], _, Blocks) -> Blocks.


%% turn_yregs(St0) -> St.
%%  Renumber y registers so that {y,0} becomes {y,FrameSize-1},
%%  {y,FrameSize-1} becomes {y,0} and so on. This is to make nested
%%  catches work. The register allocator (linear_scan()) has given
%%  a lower number to the outermost catch.

turn_yregs(#st{frames=Frames,regs=Regs0,ssa=Blocks}=St) ->
    Regs1 = foldl(fun(L, A) ->
                          Blk = map_get(L, Blocks),
                          FrameSize = beam_ssa:get_anno(frame_size, Blk),
                          RPO = beam_ssa:rpo([L], Blocks),
                          Def = beam_ssa:def(RPO, Blocks),
                          [turn_yregs_1(Def, FrameSize, Regs0)|A]
                  end, [], Frames),
    Regs = maps:merge(Regs0, maps:from_list(append(Regs1))),
    St#st{regs=Regs}.

turn_yregs_1(Def, FrameSize, Regs) ->
    Yregs0 = [{map_get(V, Regs),V} || V <- Def, is_yreg(map_get(V, Regs))],
    Yregs1 = rel2fam(Yregs0),
    FrameSize = length(Yregs1),
    Yregs2 = [{{y,FrameSize-Y-1},Vs} || {{y,Y},Vs} <- Yregs1],
    R0 = sofs:family(Yregs2),
    R1 = sofs:family_to_relation(R0),
    R = sofs:converse(R1),
    sofs:to_external(R).

%%%
%%% Reserving registers before register allocation.
%%%

%% reserve_regs(St0) -> St.
%%  Reserve registers prior to register allocation. Y registers
%%  have already been reserved. This function will reserve z,
%%  fr, and specific x registers.

reserve_regs(#st{args=Args,ssa=Blocks,intervals=Intervals,res=Res0}=St) ->
    %% Reserve x0, x1, and so on for the function arguments.
    Res1 = reserve_arg_regs(Args, 0, Res0),
    RPO = beam_ssa:rpo(Blocks),

    %% Reserve Z registers (dummy registers) for instructions with no
    %% return values (e.g. remove_message) or pseudo-return values
    %% (e.g. landingpad).
    Res2 = reserve_zregs(RPO, Blocks, Intervals, Res1),

    %% Reserve float registers.
    Res3 = reserve_fregs(RPO, Blocks, Res2),

    %% Reserve all remaining unreserved variables as X registers.
    Res = maps:from_list(Res3),
    St#st{res=reserve_xregs(RPO, Blocks, Res)}.

reserve_arg_regs([#b_var{}=Arg|Is], N, Acc) ->
    reserve_arg_regs(Is, N+1, [{Arg,{x,N}}|Acc]);
reserve_arg_regs([], _, Acc) -> Acc.

reserve_zregs(RPO, Blocks, Intervals, Res) ->
    ShortLived0 = [V || {V,[{Start,End}]} <- Intervals, Start+2 =:= End],
    ShortLived = sets:from_list(ShortLived0),
    F = fun(_, #b_blk{is=Is,last=Last}, A) ->
                reserve_zreg(Is, Last, ShortLived, A)
        end,
    beam_ssa:fold_blocks(F, RPO, Res, Blocks).

reserve_zreg([#b_set{op={bif,tuple_size},dst=Dst},
              #b_set{op={bif,'=:='},args=[Dst,Val],dst=Bool}],
             Last, ShortLived, A0) ->
    case {Val,Last} of
        {#b_literal{val=Arity},#b_br{bool=Bool}} when Arity bsr 32 =:= 0 ->
            %% These two instructions can be combined to a test_arity
            %% instruction provided that the arity variable is short-lived.
            A1 = reserve_test_zreg(Dst, ShortLived, A0),
            reserve_test_zreg(Bool, ShortLived, A1);
        {_,_} ->
            %% Either the arity is too big, or the boolean value is not
            %% used in a conditional branch.
            A0
    end;
reserve_zreg([#b_set{op={bif,tuple_size},dst=Dst}],
             #b_switch{arg=Dst}, ShortLived, A) ->
    reserve_test_zreg(Dst, ShortLived, A);
reserve_zreg([#b_set{op=Op,dst=Dst}], #b_br{bool=Dst}, ShortLived, A) ->
    case use_zreg(Op) of
        yes -> [{Dst,z} | A];
        no -> A;
        'maybe' -> reserve_test_zreg(Dst, ShortLived, A)
    end;
reserve_zreg([#b_set{op=Op,dst=Dst} | Is], Last, ShortLived, A) ->
    case use_zreg(Op) of
        yes -> reserve_zreg(Is, Last, ShortLived, [{Dst,z} | A]);
        _Other -> reserve_zreg(Is, Last, ShortLived, A)
    end;
reserve_zreg([], _, _, A) -> A.

use_zreg(bs_ensured_match_string) -> yes;
use_zreg(bs_ensured_skip) -> yes;
use_zreg(bs_ensure) -> yes;
use_zreg(bs_match_string) -> yes;
use_zreg(bs_set_position) -> yes;
use_zreg(debug_line) -> yes;
use_zreg(executable_line) -> yes;
use_zreg(kill_try_tag) -> yes;
use_zreg(landingpad) -> yes;
use_zreg(nif_start) -> yes;
use_zreg(recv_marker_bind) -> yes;
use_zreg(recv_marker_clear) -> yes;
use_zreg(remove_message) -> yes;
use_zreg(set_tuple_element) -> yes;
use_zreg(succeeded) -> yes;
use_zreg(wait_timeout) -> yes;
%% There's no way we can combine these into a test instruction, so we must
%% avoid using a z register if their result is used directly in a branch.
use_zreg(call) -> no;
use_zreg({bif,element}) -> no;
use_zreg({bif,is_map_key}) -> no;
use_zreg({bif,is_record}) -> no;
use_zreg({bif,map_get}) -> no;
use_zreg({bif,'xor'}) -> no;
use_zreg(get_hd) -> no;
use_zreg(get_tl) -> no;
use_zreg(get_tuple_element) -> no;
%% Assume the instruction can use a z register, provided it's the last in its
%% block and that the result is only used in the terminator.
use_zreg(_) -> 'maybe'.

%% If V is defined just before a branch, we may be able to combine it into a
%% test instruction.
reserve_test_zreg(#b_var{}=V, ShortLived, A) ->
    case sets:is_element(V, ShortLived) of
        true -> [{V,z}|A];
        false -> A
    end.

reserve_fregs(RPO, Blocks, Res) ->
    F = fun(_, #b_blk{is=Is}, A) ->
                reserve_freg(Is, A)
        end,
    beam_ssa:fold_blocks(F, RPO, Res, Blocks).

reserve_freg([#b_set{op={float,Op},dst=V}|Is], Res) ->
    case Op of
        get ->
            reserve_freg(Is, Res);
        _ ->
            reserve_freg(Is, [{V,fr}|Res])
    end;
reserve_freg([_|Is], Res) ->
    reserve_freg(Is, Res);
reserve_freg([], Res) -> Res.

%% reserve_xregs(St0) -> St.
%%  Reserve all remaining variables as X registers.
%%
%%  If a variable will need to be in a specific X register for a
%%  'call' instruction (and there is nothing that will kill it
%%  between the definition and use), reserve the register using a
%%  {prefer,{x,X} annotation. That annotation means that the linear
%%  scan algorithm will place the variable in the preferred register,
%%  unless that register is already occupied.
%%
%%  All remaining variables are reserved as X registers. Linear scan
%%  will allocate the lowest free X register for the variable.

reserve_xregs(RPO, Blocks, Res) ->
    Ls = reverse(RPO),
    reserve_xregs(Ls, Blocks, #{}, Res).

reserve_xregs([L|Ls], Blocks, XsMap0, Res0) ->
    #b_blk{anno=Anno,is=Is0,last=Last} = map_get(L, Blocks),

    %% Calculate mapping from variable name to the preferred
    %% register.
    Xs0 = reserve_terminator(L, Is0, Last, Blocks, XsMap0, Res0),

    %% We need to figure out where the code generator will
    %% place instructions that will do a garbage collection.
    %% Insert 'gc' markers as pseudo-instructions in the
    %% instruction sequence.
    Is1 = reverse(Is0),
    Is2 = res_place_gc_instrs(Is1, []),
    Is = res_place_allocate(Anno, Is2),

    %% Add register hints for variables that are defined
    %% in the (reversed) instruction sequence.
    {Res,Xs} = reserve_xregs_is(Is, Res0, Xs0, []),

    XsMap = XsMap0#{L=>Xs},
    reserve_xregs(Ls, Blocks, XsMap, Res);
reserve_xregs([], _, _, Res) -> Res.

%% Insert explicit 'gc' markers points where there will
%% be a garbage collection. (Note that the instruction
%% sequence passed to this function is reversed.)

res_place_gc_instrs([#b_set{op=phi}=I|Is], Acc) ->
    res_place_gc_instrs(Is, [I|Acc]);
res_place_gc_instrs([#b_set{op=call}=I|Is], Acc) ->
    case Acc of
        [] ->
            res_place_gc_instrs(Is, [I|Acc]);
        [GC|_] when GC =:= gc; GC =:= test_heap ->
            res_place_gc_instrs(Is, [I,gc|Acc]);
        [_|_] ->
            res_place_gc_instrs(Is, [I,gc|Acc])
    end;
res_place_gc_instrs([#b_set{op=Op,args=Args}=I|Is], Acc0) ->
    case beam_ssa_codegen:classify_heap_need(Op, Args) of
        neutral ->
            case Acc0 of
                [test_heap|Acc] ->
                    res_place_gc_instrs(Is, [test_heap,I|Acc]);
                Acc ->
                    res_place_gc_instrs(Is, [I|Acc])
            end;
        {put,_} ->
            res_place_gc_instrs(Is, res_place_test_heap(I, Acc0));
        {put_fun,_} ->
            res_place_gc_instrs(Is, res_place_test_heap(I, Acc0));
        put_float ->
            res_place_gc_instrs(Is, res_place_test_heap(I, Acc0));
        gc ->
            res_place_gc_instrs(Is, [gc,I|Acc0])
    end;
res_place_gc_instrs([], Acc) ->
    %% Reverse and replace 'test_heap' markers with 'gc'.
    %% (The distinction is no longer useful.)
    res_place_gc_instrs_rev(Acc, []).

res_place_test_heap(I, Acc) ->
    case Acc of
        [test_heap|Acc] ->
            [test_heap,I|Acc];
        _ ->
            [test_heap,I|Acc]
    end.

res_place_gc_instrs_rev([test_heap|Is], [gc|_]=Acc) ->
    res_place_gc_instrs_rev(Is, Acc);
res_place_gc_instrs_rev([test_heap|Is], Acc) ->
    res_place_gc_instrs_rev(Is, [gc|Acc]);
res_place_gc_instrs_rev([gc|Is], [gc|_]=Acc) ->
    res_place_gc_instrs_rev(Is, Acc);
res_place_gc_instrs_rev([I|Is], Acc) ->
    res_place_gc_instrs_rev(Is, [I|Acc]);
res_place_gc_instrs_rev([], Acc) -> Acc.

res_place_allocate(#{yregs:=_}, Is) ->
    %% There will be an 'allocate' instruction inserted here.
    Is ++ [gc];
res_place_allocate(#{}, Is) -> Is.

reserve_xregs_is([gc|Is], Res, Xs0, Used) ->
    %% At this point, the code generator will place an instruction
    %% that does a garbage collection. We must prune the remembered
    %% registers.
    Xs = res_xregs_prune(Xs0, Used, Res),
    reserve_xregs_is(Is, Res, Xs, Used);
reserve_xregs_is([#b_set{op=Op,dst=Dst,args=Args}=I|Is], Res0, Xs0, Used0) ->
    Res1 = reserve_xreg(Dst, Xs0, Res0),
    Used1 = ordsets:union(Used0, beam_ssa:used(I)),
    Used = ordsets:del_element(Dst, Used1),
    case Op of
        call ->
            Xs = reserve_call_args(tl(Args)),
            reserve_xregs_is(Is, Res1, Xs, Used);
        extract ->
            %% Avoid potential register shuffling by pinning the
            %% destination variable to the X register where the
            %% runtime system will place it.
            [_,#b_literal{val=Reg}] = Args,
            Res = Res1#{Dst => {x,Reg}},
            reserve_xregs_is(Is, Res, Xs0, Used);
        _ ->
            reserve_xregs_is(Is, Res1, Xs0, Used)
    end;
reserve_xregs_is([], Res, Xs, _Used) ->
    {Res,Xs}.

%% Pick up register hints from the successors of this blocks.
reserve_terminator(L, Is, #b_br{bool=#b_var{},succ=Succ,fail=Fail},
                   Blocks, XsMap, Res) when Succ =/= Fail,
                                            Fail =/= ?EXCEPTION_BLOCK ->
    #{Succ:=SuccBlk,Fail:=FailBlk} = Blocks,
    case {SuccBlk,FailBlk} of
        {#b_blk{is=[],last=#b_br{succ=PhiL,fail=PhiL}},
         #b_blk{is=[],last=#b_br{succ=PhiL,fail=PhiL}}} ->
            %% Both branches ultimately transfer to the same
            %% block (via two blocks with no instructions).
            %% Pick up register hints from the phi nodes
            %% in the common block.
            #{PhiL:=#b_blk{is=PhiIs}} = Blocks,
            Xs = res_xregs_from_phi(PhiIs, Succ, Res, #{}),
            res_xregs_from_phi(PhiIs, Fail, Res, Xs);
        {_,_} when Is =/= [] ->
            case last(Is) of
                #b_set{op=succeeded,args=[Arg]} ->
                    %% We know that Arg will not be used at the failure
                    %% label, so we can pick up register hints from the
                    %% success label.
                    Br = #b_br{bool=#b_literal{val=true},succ=Succ,fail=Succ},
                    case reserve_terminator(L, [], Br, Blocks, XsMap, Res) of
                        #{Arg:=Reg} -> #{Arg=>Reg};
                        #{} -> #{}
                    end;
                #b_set{op=new_try_tag} ->
                    %% We know that no X registers will be used at the
                    %% failure label (a block starting with the
                    %% landingpad instruction), so we can pick up
                    %% register hints from the success label.
                    reserve_terminator_1(L, Succ, Is, Blocks, XsMap, Res);
                _ ->
                    %% Register hints from the success block may not
                    %% be safe at the failure block, and vice versa.
                    #{}
            end;
        {_,_} ->
            %% Register hints from the success block may not
            %% be safe at the failure block, and vice versa.
            #{}
    end;
reserve_terminator(L, Is, #b_br{bool=Bool,succ=Succ,fail=Fail},
                   Blocks, XsMap, Res) ->
    case {Bool, Fail} of
        {_, ?EXCEPTION_BLOCK} ->
            %% We know that no variables are used from ?EXCEPTION_BLOCK, so any
            %% register hints from the successor block are safe to use.
            reserve_terminator_1(L, Succ, Is, Blocks, XsMap, Res);
        {#b_literal{val=true}, _} ->
            %% We only have one successor, so its hints are safe to use.
            reserve_terminator_1(L, Succ, Is, Blocks, XsMap, Res);
        {_, _} ->
            %% Register hints from the success block may not
            %% be safe at the failure block, and vice versa.
            #{}
    end;
reserve_terminator(_, _, _, _, _, _) ->
    #{}.

reserve_terminator_1(L, Succ, _Is, Blocks, XsMap, Res) ->
    case {Blocks, XsMap} of
        {#{ Succ := #b_blk{is=[#b_set{op=phi}|_]=PhiIs}}, #{}} ->
            res_xregs_from_phi(PhiIs, L, Res, #{});
        {#{}, #{ Succ := Xs }}->
            Xs;
        {#{}, #{}} ->
            #{}
    end.

%% Pick up a reservation from a phi node.
res_xregs_from_phi([#b_set{op=phi,dst=Dst,args=Args}|Is],
                   Pred, Res, Acc) ->
    case [V || {#b_var{}=V,L} <- Args, L =:= Pred] of
        [] ->
            %% The value of the phi node for this predecessor
            %% is a literal. Nothing to do here.
            res_xregs_from_phi(Is, Pred, Res, Acc);
        [V] ->
            case Res of
                #{Dst:={prefer,Reg}} ->
                    %% Try placing V in the same register as for
                    %% the phi node.
                    res_xregs_from_phi(Is, Pred, Res, Acc#{V=>Reg});
                #{Dst:=_} ->
                    res_xregs_from_phi(Is, Pred, Res, Acc)
            end
    end;
res_xregs_from_phi(_, _, _, Acc) -> Acc.

reserve_call_args(Args) ->
    reserve_call_args(Args, 0, #{}).

reserve_call_args([#b_var{}=Var|As], X, Xs) ->
    reserve_call_args(As, X+1, Xs#{Var => {x,X}});
reserve_call_args([#b_literal{}|As], X, Xs) ->
    reserve_call_args(As, X+1, Xs#{{x,X} => hole});
reserve_call_args([], _, Xs) -> Xs.

reserve_xreg(V, Xs, Res) ->
    case Res of
        #{V:=_} ->
            %% Already reserved (but not as an X register).
            Res;
        #{} ->
            case Xs of
                #{V:=X} ->
                    %% Add a hint that this specific X register is
                    %% preferred, unless it is already in use.
                    Res#{V=>{prefer,X}};
                #{} ->
                    %% Reserve as an X register in general.
                    Res#{V=>x}
            end
    end.

%% res_xregs_prune(PreferredRegs, Used, Res) -> PreferredRegs.
%%  Prune the list of preferred registers, to make sure that
%%  there are no "holes" (uninitialized X registers) when
%%  invoking the garbage collector.

res_xregs_prune(Xs, Used, Res) when map_size(Xs) =/= 0 ->
    %% Calculate a conservative estimate for the number of safe
    %% registers based on the used X register after this point. The
    %% actual number of safe registers may be higher than this number.
    NumSafe0 = foldl(fun(V, N) ->
                             %% Count the number of used variables
                             %% allocated to X registers.
                             case Res of
                                 #{V := {x,_}} -> N + 1;
                                 #{V := _} -> N;
                                 #{} -> N + 1
                             end
                     end, 0, Used),
    NumSafe = foldl(fun(X, N) ->
                            %% Decrement the count if there are holes.
                            case Xs of
                                #{{x,X} := hole} -> N - 1;
                                #{} -> N
                            end
                    end, NumSafe0, seq(0, NumSafe0-1)),

    %% Remove unsafe registers from the list of potential
    %% preferred registers.
    #{Var => Reg || Var := {x,X}=Reg <- Xs, X < NumSafe};
res_xregs_prune(Xs, _Used, _Res) -> Xs.

%%%
%%% Register allocation using linear scan.
%%%

-record(i,
        {sort=1 :: instr_number(),
         reg=none :: i_reg(),
         pool=x :: pool_id(),
         var=#b_var{} :: b_var(),
         rs=[] :: [range()]
        }).

-record(l,
        {cur=#i{} :: interval(),
         unhandled_res=[] :: [interval()],
         unhandled_any=[] :: [interval()],
         active=[] :: [interval()],
         inactive=[] :: [interval()],
         free=#{} :: #{var_name()=>pool(),
                       {'next',pool_id()}:=reg_num()},
         regs=[] :: [{b_var(),ssa_register()}]
        }).

-type interval() :: #i{}.
-type i_reg() :: ssa_register() | {'prefer',xreg()} | 'none'.
-type pool_id() :: 'fr' | 'x' | 'z' | instr_number().
-type pool() :: ordsets:ordset(ssa_register()).

linear_scan(#st{intervals=Intervals0,res=Res}=St0) ->
    St = St0#st{intervals=[],res=[]},
    Free = init_free(maps:to_list(Res)),
    Intervals1 = [init_interval(Int, Res) || Int <- Intervals0],
    Intervals = sort(Intervals1),
    IsReserved = fun(#i{reg=Reg}) ->
                         case Reg of
                             none -> false;
                             {prefer,{_,_}} -> false;
                             {_,_} -> true
                         end
                 end,
    {UnhandledRes,Unhandled} = partition(IsReserved, Intervals),
    L = #l{unhandled_res=UnhandledRes,
           unhandled_any=Unhandled,free=Free},
    #l{regs=Regs} = do_linear(L),
    St#st{regs=maps:from_list(Regs)}.

init_interval({V,[{Start,_}|_]=Rs}, Res) ->
    Info = map_get(V, Res),
    Pool = case Info of
               {prefer,{x,_}} -> x;
               x -> x;
               {x,_} -> x;
               {y,Uniq} -> Uniq;
               {{y,_},Uniq} -> Uniq;
               z -> z;
               fr -> fr
           end,
    Reg = case Info of
              {prefer,{x,_}} -> Info;
              {x,_} -> Info;
              {{y,_}=Y,_} -> Y;
              _ -> none
          end,
    #i{sort=Start,var=V,reg=Reg,pool=Pool,rs=Rs}.

init_free(Res) ->
    Free0 = rel2fam([{x,{x,0}}|init_free_1(Res)]),
    #{x:=Xs0} = Free1 = maps:from_list(Free0),
    Xs = init_xregs(Xs0),
    Free = Free1#{x:=Xs},
    Next = #{{next,K} => length(V) || K := V <- Free},
    maps:merge(Free, Next).

init_free_1([{_,{prefer,{x,_}=Reg}}|Res]) ->
    [{x,Reg}|init_free_1(Res)];
init_free_1([{_,{x,_}=Reg}|Res]) ->
    [{x,Reg}|init_free_1(Res)];
init_free_1([{_,{y,Uniq}}|Res]) ->
    [{Uniq,{y,0}}|init_free_1(Res)];
init_free_1([{_,{{y,_}=Reg,Uniq}}|Res]) ->
    [{Uniq,Reg}|init_free_1(Res)];
init_free_1([{_,z}|Res]) ->
    [{z,{z,0}}|init_free_1(Res)];
init_free_1([{_,fr}|Res]) ->
    [{fr,{fr,0}}|init_free_1(Res)];
init_free_1([{_,x}|Res]) ->
    init_free_1(Res);
init_free_1([]) -> [].

%% Make sure that the pool of xregs is contiguous.
init_xregs([{x,N},{x,M}|Is]) when N+1 =:= M ->
    [{x,N}|init_xregs([{x,M}|Is])];
init_xregs([{x,N}|[{x,_}|_]=Is]) ->
    [{x,N}|init_xregs([{x,N+1}|Is])];
init_xregs([{x,_}]=Is) -> Is.

do_linear(L0) ->
    case set_next_current(L0) of
        done ->
            L0;
        L1 ->
            L2 = expire_active(L1),
            L3 = check_inactive(L2),
            Available = collect_available(L3),
            L4 = select_register(Available, L3),
            L = make_cur_active(L4),
            do_linear(L)
    end.

set_next_current(#l{unhandled_res=[Cur1|T1],
                    unhandled_any=[Cur2|T2]}=L) ->
    case {Cur1,Cur2} of
        {#i{sort=N1},#i{sort=N2}} when N1 < N2 ->
            L#l{cur=Cur1,unhandled_res=T1};
        {_,_} ->
            L#l{cur=Cur2,unhandled_any=T2}
    end;
set_next_current(#l{unhandled_res=[],
                    unhandled_any=[Cur|T]}=L) ->
    L#l{cur=Cur,unhandled_any=T};
set_next_current(#l{unhandled_res=[Cur|T],
                    unhandled_any=[]}=L) ->
    L#l{cur=Cur,unhandled_res=T};
set_next_current(#l{unhandled_res=[],unhandled_any=[]}) ->
    done.

expire_active(#l{cur=#i{sort=CurBegin},active=Act0}=L0) ->
    {Act,L} = expire_active(Act0, CurBegin, L0, []),
    L#l{active=Act}.

expire_active([#i{reg=Reg,rs=Rs0}=I|Is], CurBegin, L0, Acc) ->
    {_,_} = Reg,                                %Assertion.
    case overlap_status(Rs0, CurBegin) of
        ends_before_cur ->
            L = free_reg(I, L0),
            expire_active(Is, CurBegin, L, Acc);
        overlapping ->
            expire_active(Is, CurBegin, L0, [I|Acc]);
        not_overlapping ->
            Rs = strip_before_current(Rs0, CurBegin),
            L1 = free_reg(I, L0),
            L = L1#l{inactive=[I#i{rs=Rs}|L1#l.inactive]},
            expire_active(Is, CurBegin, L, Acc)
    end;
expire_active([], _CurBegin, L, Acc) ->
    {Acc,L}.

check_inactive(#l{cur=#i{sort=CurBegin},inactive=InAct0}=L0) ->
    {InAct,L} = check_inactive(InAct0, CurBegin, L0, []),
    L#l{inactive=InAct}.

check_inactive([#i{rs=Rs0}=I|Is], CurBegin, L0, Acc) ->
    case overlap_status(Rs0, CurBegin) of
        ends_before_cur ->
            check_inactive(Is, CurBegin, L0, Acc);
        not_overlapping ->
            check_inactive(Is, CurBegin, L0, [I|Acc]);
        overlapping ->
            Rs = strip_before_current(Rs0, CurBegin),
            L1 = L0#l{active=[I#i{rs=Rs}|L0#l.active]},
            L = reserve_reg(I, L1),
            check_inactive(Is, CurBegin, L, Acc)
    end;
check_inactive([], _CurBegin, L, Acc) ->
    {Acc,L}.

strip_before_current([{_,E}|Rs], CurBegin) when E =< CurBegin ->
    strip_before_current(Rs, CurBegin);
strip_before_current(Rs, _CurBegin) -> Rs.

collect_available(#l{cur=#i{reg={prefer,{_,_}=Prefer}}=I}=L) ->
    %% Use the preferred register if it is available.
    Avail = collect_available(L#l{cur=I#i{reg=none}}),
    case member(Prefer, Avail) of
        true -> [Prefer];
        false -> Avail
    end;
collect_available(#l{cur=#i{reg={_,_}=ReservedReg}}) ->
    %% Return the already reserved register.
    [ReservedReg];
collect_available(#l{unhandled_res=Unhandled,cur=Cur}=L) ->
    Free = get_pool(Cur, L),

    %% Note that since the live intervals are constructed from
    %% SSA form, there cannot be any overlap of the current interval
    %% with any inactive interval. See [3], page 175. Therefore we
    %% only have check the unhandled intervals for overlap with
    %% the current interval. As a further optimization, we only need
    %% to check the intervals that have reserved registers.
    collect_available(Unhandled, Cur, Free).

collect_available([#i{pool=Pool1}|Is], #i{pool=Pool2}=Cur, Free)
  when Pool1 =/= Pool2 ->
    %% Wrong pool. Ignore this interval.
    collect_available(Is, Cur, Free);
collect_available([#i{reg={_,_}=Reg}=I|Is], Cur, Free0) ->
    case overlaps(I, Cur) of
        true ->
            Free = ordsets:del_element(Reg, Free0),
            collect_available(Is, Cur, Free);
        false ->
            collect_available(Is, Cur, Free0)
    end;
collect_available([], _, Free) -> Free.

select_register([{_,_}=Reg|_], #l{cur=Cur0,regs=Regs}=L) ->
    Cur = Cur0#i{reg=Reg},
    reserve_reg(Cur, L#l{cur=Cur,regs=[{Cur#i.var,Reg}|Regs]});
select_register([], #l{cur=Cur0,regs=Regs}=L0) ->
    %% Allocate a new register in the pool.
    {Reg,L1} = get_next_free(Cur0, L0),
    Cur = Cur0#i{reg=Reg},
    L = L1#l{cur=Cur,regs=[{Cur#i.var,Reg}|Regs]},
    reserve_reg(Cur, L).

make_cur_active(#l{cur=Cur,active=Act}=L) ->
    L#l{active=[Cur|Act]}.

overlaps(#i{rs=Rs1}, #i{rs=Rs2}) ->
    are_overlapping(Rs1, Rs2).

overlap_status([{S,E}], CurBegin) ->
    if
        E =< CurBegin -> ends_before_cur;
        CurBegin < S -> not_overlapping;
        true -> overlapping
    end;
overlap_status([{S,E}|Rs], CurBegin) ->
    if
        E =< CurBegin ->
            overlap_status(Rs, CurBegin);
        S =< CurBegin ->
            overlapping;
        true ->
            not_overlapping
    end.

reserve_reg(#i{reg={_,_}=Reg}=I, L) ->
    FreeRegs0 = get_pool(I, L),
    FreeRegs = ordsets:del_element(Reg, FreeRegs0),
    update_pool(I, FreeRegs, L).

free_reg(#i{reg={_,_}=Reg}=I, L) ->
    FreeRegs0 = get_pool(I, L),
    FreeRegs = ordsets:add_element(Reg, FreeRegs0),
    update_pool(I, FreeRegs, L).

get_pool(#i{pool=Pool}, #l{free=Free}) ->
    map_get(Pool, Free).

update_pool(#i{pool=Pool}, New, #l{free=Free0}=L) ->
    Free = Free0#{Pool:=New},
    L#l{free=Free}.

get_next_free(#i{pool=Pool}, #l{free=Free0}=L0) ->
    K = {next,Pool},
    N = map_get(K, Free0),
    Free = Free0#{K:=N+1},
    L = L0#l{free=Free},
    if
        is_integer(Pool) -> {{y,N},L};
        is_atom(Pool)    -> {{Pool,N},L}
    end.

%%%
%%% Interval utilities.
%%%

are_overlapping([R|Rs1], Rs2) ->
    case are_overlapping_1(R, Rs2) of
        true ->
            true;
        false ->
            are_overlapping(Rs1, Rs2)
    end;
are_overlapping([], _) -> false.

are_overlapping_1({_S1,E1}, [{S2,_E2}|_]) when E1 < S2 ->
    false;
are_overlapping_1({S1,E1}=R, [{S2,E2}|Rs]) ->
    (S2 < E1 andalso E2 > S1) orelse are_overlapping_1(R, Rs);
are_overlapping_1({_,_}, []) -> false.

%%%
%%% Utilities.
%%%

%% is_loop_header(L, Blocks) -> false|true.
%%  Check whether the block is a loop header.

is_loop_header(L, Blocks) ->
    case map_get(L, Blocks) of
        #b_blk{is=[I|_]} -> beam_ssa:is_loop_header(I);
        #b_blk{} -> false
    end.

rel2fam(S0) ->
    S1 = sofs:relation(S0),
    S = sofs:rel2fam(S1),
    sofs:to_external(S).

is_yreg({y,_}) -> true;
is_yreg({x,_}) -> false;
is_yreg({z,_}) -> false;
is_yreg({fr,_}) -> false.

new_vars(N, Count0) when is_integer(N), N >= 0 ->
    Count = Count0 + N,
    Vars = [#b_var{name=I} || I <- lists:seq(Count0, Count-1)],
    {Vars,Count}.

new_var(Count) ->
    {#b_var{name=Count},Count+1}.
