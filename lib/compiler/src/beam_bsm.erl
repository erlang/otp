%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2016. All Rights Reserved.
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

-module(beam_bsm).
-export([module/2,format_error/1]).

-import(lists, [member/2,foldl/3,reverse/1,sort/1,all/2]).

%%%
%%% We optimize bit syntax matching where the tail end of a binary is
%%% matched out and immediately passed on to a bs_start_match2 instruction,
%%% such as in this code sequence:
%%%
%%%     func_info ...
%%% L1	test bs_start_match2 {f,...} {x,0} Live SavePositions {x,0}
%%%              . . .
%%%     test bs_get_binary2 {f,...} {x,0} all 1 Flags {x,0}
%%%              . . .
%%%     call_only 2 L1
%%%
%%% The sequence can be optimized simply by removing the bs_get_binary2
%%% instruction. Another example:
%%%
%%%     func_info ...
%%% L1	test bs_start_match2 {f,...} {x,0} Live SavePositions {x,0}
%%%              . . .
%%%     test bs_get_binary2 {f,...} {x,0} all 8 Flags {x,1}
%%%              . . .
%%%     move {x,1} {x,0}
%%%     call_only 2 L1
%%%
%%% In this case, the bs_get_binary2 instruction must be replaced by
%%%
%%%	test bs_unit {x,1} 8
%%%
%%% to ensure that the match fail if the length of the binary in bits
%%% is not evenly divisible by 8.
%%%
%%% Note that the bs_start_match2 instruction doesn't need to be in the same
%%% function as the caller. It can be in the beginning of any function, or
%%% follow the bs_get_binary2 instruction in the same function. The important
%%% thing is that the match context register is not copied or built into
%%% data structures or passed to BIFs.
%%%

-type label() :: beam_asm:label().
-type func_info() :: {beam_asm:reg(),boolean()}.

-record(btb,
	{f :: gb_trees:tree(label(), func_info()),
	 index :: beam_utils:code_index(), %{Label,Code} index (for liveness).
	 ok_br=gb_sets:empty() :: gb_sets:set(label()), %Labels that are OK.
	 must_not_save=false :: boolean(), %Must not save position when
					   % optimizing (reaches
                                           % bs_context_to_binary).
	 must_save=false :: boolean() %Must save position when optimizing.
	}).


-spec module(beam_utils:module_code(), [compile:option()]) ->
                    {'ok',beam_utils:module_code()}.

module({Mod,Exp,Attr,Fs0,Lc}, Opts) ->
    FIndex = btb_index(Fs0),
    Fs = [function(F, FIndex) || F <- Fs0],
    Code = {Mod,Exp,Attr,Fs,Lc},
    case proplists:get_bool(bin_opt_info, Opts) of
	true ->
	    {ok,Code,collect_warnings(Fs)};
	false ->
	    {ok,Code}
    end.

-spec format_error('bin_opt' | {'no_bin_opt', term()}) -> nonempty_string().

format_error(bin_opt) ->
    "OPTIMIZED: creation of sub binary delayed";
format_error({no_bin_opt,Reason}) ->
    lists:flatten(["NOT OPTIMIZED: "|format_error_1(Reason)]).

%%%
%%% Local functions.
%%% 

function({function,Name,Arity,Entry,Is}, FIndex) ->
    try
	Index = beam_utils:index_labels(Is),
	D = #btb{f=FIndex,index=Index},
	{function,Name,Arity,Entry,btb_opt_1(Is, D, [])}
    catch
        Class:Error:Stack ->
	    io:fwrite("Function: ~w/~w\n", [Name,Arity]),
	    erlang:raise(Class, Error, Stack)
    end.

btb_opt_1([{test,bs_get_binary2,F,_,[Reg,{atom,all},U,Fs],Reg}=I0|Is], D, Acc0) ->
    case btb_reaches_match(Is, [Reg], D) of
	{error,Reason} ->
	    Comment = btb_comment_no_opt(Reason, Fs),
	    btb_opt_1(Is, D, [Comment,I0|Acc0]);
	{ok,MustSave} ->
	    Comment = btb_comment_opt(Fs),
	    Acc1 = btb_gen_save(MustSave, Reg, [Comment|Acc0]),
	    Acc = case U of
		      1 -> Acc1;
		      _ -> [{test,bs_test_unit,F,[Reg,U]}|Acc1]
		  end,
	    btb_opt_1(Is, D, Acc)
    end;
btb_opt_1([{test,bs_get_binary2,F,_,[Ctx,{atom,all},U,Fs],Dst}=I0|Is0], D, Acc0) ->
    case btb_reaches_match(Is0, [Ctx,Dst], D) of
	{error,Reason} ->
	    Comment = btb_comment_no_opt(Reason, Fs),
	    btb_opt_1(Is0, D, [Comment,I0|Acc0]);
	{ok,MustSave} when U =:= 1 ->
	    Comment = btb_comment_opt(Fs),
            Acc = btb_gen_save(MustSave, Ctx, [Comment|Acc0]),
            Is = prepend_move(Ctx, Dst, Is0),
	    btb_opt_1(Is, D, Acc);
	{ok,MustSave} ->
	    Comment = btb_comment_opt(Fs),
	    Acc1 = btb_gen_save(MustSave, Ctx, [Comment|Acc0]),
            Acc = [{test,bs_test_unit,F,[Ctx,U]}|Acc1],
            Is = prepend_move(Ctx, Dst, Is0),
	    btb_opt_1(Is, D, Acc)
    end;
btb_opt_1([I|Is], D, Acc) ->
    %%io:format("~p\n", [I]),
    btb_opt_1(Is, D, [I|Acc]);
btb_opt_1([], _, Acc) ->
    reverse(Acc).

btb_gen_save(true, Reg, Acc) ->
    [{bs_save2,Reg,{atom,start}}|Acc];
btb_gen_save(false, _, Acc) -> Acc.

prepend_move(Ctx, Dst, [{block,Bl0}|Is]) ->
    Bl = [{set,[Dst],[Ctx],move}|Bl0],
    [{block,Bl}|Is];
prepend_move(Ctx, Dst, Is) ->
    [{move,Ctx,Dst}|Is].

%% btb_reaches_match([Instruction], [Register], D) ->
%%   {ok,MustSave}|{error,Reason}
%%
%%  The list of Registers should be a list of registers referencing a
%%  match context. The Register may contain one element if the
%%  bs_get_binary2 instruction looks like
%%
%%    test bs_get_binary2 {f,...} Ctx all _ _ Ctx
%%
%%  or two elements if the instruction looks like
%%
%%    test bs_get_binary2 {f,...} Ctx all _ _ Dst
%%
%%  This function determines whether the bs_get_binary2 instruction
%%  can be omitted (retaining the match context instead of creating
%%  a sub binary).
%%
%%  The rule is that the match context ultimately must end up at a
%%  bs_start_match2 instruction and nowhere else. That it, it must not
%%  be passed to BIFs, or copied or put into data structures. There
%%  must only be one copy alive when the match context reaches the
%%  bs_start_match2 instruction.
%%
%%  At a branch, we must follow all branches and make sure that the above
%%  rule is followed (or that the branch kills the match context).
%%
%%  The MustSave return value will be true if control may end up at
%%  bs_context_to_binary instruction. Since that instruction uses the
%%  saved start position, we must use "bs_save2 Ctx start" to
%%  update the saved start position. An additional complication is that
%%  "bs_save2 Ctx start" must not be used if Dst and Ctx are
%%  different registers and both registers may be passed to
%%  a bs_context_to_binary instruction.
%% 

btb_reaches_match(Is, RegList, D) ->
    try
	Regs = btb_regs_from_list(RegList),
	#btb{must_not_save=MustNotSave,must_save=MustSave} =
            btb_reaches_match_1(Is, Regs, D),
	case MustNotSave andalso MustSave of
	    true -> btb_error(must_and_must_not_save);
	    false -> {ok,MustSave}
	end
    catch
	throw:{error,_}=Error -> Error
    end.

btb_reaches_match_1(Is, Regs, D) ->
    case btb_are_registers_empty(Regs) of
	false ->
	    btb_reaches_match_2(Is, Regs, D);
	true ->
	    %% The context was killed, which is OK.
	    D
    end.

btb_reaches_match_2([{block,Bl}|Is], Regs0, D) ->
    Regs = btb_reaches_match_block(Bl, Regs0),
    btb_reaches_match_1(Is, Regs, D);
btb_reaches_match_2([{call,Arity,{f,Lbl}}|Is], Regs0, D) ->
    case is_tail_call(Is) of
	true ->
	    Regs1 = btb_kill_not_live(Arity, Regs0),
	    Regs = btb_kill_yregs(Regs1),
	    btb_tail_call(Lbl, Regs, D);
	false ->
	    btb_call(Arity, Lbl, Regs0, Is, D)
    end;
btb_reaches_match_2([{apply,Arity}|Is], Regs, D) ->
    btb_call(Arity+2, apply, Regs, Is, D);
btb_reaches_match_2([{call_fun,Live}=I|Is], Regs, D) ->
    btb_ensure_not_used([{x,Live}], I, Regs),
    btb_call(Live, I, Regs, Is, D);
btb_reaches_match_2([{make_fun2,_,_,_,Live}|Is], Regs, D) ->
    btb_call(Live, make_fun2, Regs, Is, D);
btb_reaches_match_2([{call_ext,Arity,Func}=I|Is], Regs0, D) ->
    %% Allow us scanning beyond the call in case the match
    %% context is saved on the stack.
    case beam_jump:is_exit_instruction(I) of
	false ->
	    btb_call(Arity, Func, Regs0, Is, D);
	true ->
	    Regs = btb_kill_not_live(Arity, Regs0),
	    btb_tail_call(Func, Regs, D)
    end;
btb_reaches_match_2([{kill,Y}|Is], Regs, D) ->
    btb_reaches_match_1(Is, btb_kill([Y], Regs), D);
btb_reaches_match_2([{deallocate,_}|Is], Regs0, D) ->
    Regs = btb_kill_yregs(Regs0),
    btb_reaches_match_1(Is, Regs, D);
btb_reaches_match_2([return=I|_], Regs0, D) ->
    btb_ensure_not_used([{x,0}], I, Regs0),
    D;
btb_reaches_match_2([{gc_bif,_,{f,F},Live,Ss,Dst}=I|Is], Regs0, D0) ->
    btb_ensure_not_used(Ss, I, Regs0),
    Regs1 = btb_kill_not_live(Live, Regs0),
    Regs = btb_kill([Dst], Regs1),
    D = btb_follow_branch(F, Regs, D0),
    btb_reaches_match_1(Is, Regs, D);
btb_reaches_match_2([{bif,_,{f,F},Ss,Dst}=I|Is], Regs0, D0) ->
    btb_ensure_not_used(Ss, I, Regs0),
    Regs = btb_kill([Dst], Regs0),
    D = btb_follow_branch(F, Regs, D0),
    btb_reaches_match_1(Is, Regs, D);
btb_reaches_match_2([{get_map_elements,{f,F},Src,{list,Ls}}=I|Is], Regs0, D0) ->
    {Ss,Ds} = beam_utils:split_even(Ls),
    btb_ensure_not_used([Src|Ss], I, Regs0),
    Regs = btb_kill(Ds, Regs0),
    D = btb_follow_branch(F, Regs, D0),
    btb_reaches_match_1(Is, Regs, D);
btb_reaches_match_2([{test,bs_start_match2,{f,F},Live,[Ctx,_],Ctx}=I|Is],
		    Regs0, D0) ->
    CtxRegs = btb_context_regs(Regs0),
    case member(Ctx, CtxRegs) of
	false ->
	    %% This bs_start_match2 instruction does not use "our"
	    %% match state. Therefore we can continue the search
	    %% for another bs_start_match2 instruction.
	    D = btb_follow_branch(F, Regs0, D0),
	    Regs = btb_kill_not_live(Live, Regs0),
	    btb_reaches_match_2(Is, Regs, D);
	true ->
	    %% OK. This instruction will use "our" match state,
	    %% but we must make sure that all other copies of the
	    %% match state are killed in the code that follows
	    %% the instruction. (We know that the fail branch cannot
	    %% be taken in this case.)
	    OtherCtxRegs = CtxRegs -- [Ctx],
	    case btb_are_all_unused(OtherCtxRegs, Is, D0) of
		false -> btb_error({OtherCtxRegs,not_all_unused_after,I});
		true -> D0
	    end
    end;
btb_reaches_match_2([{test,bs_start_match2,{f,F},Live,[Bin,_],Ctx}|Is],
		    Regs0, D0) ->
    CtxRegs = btb_context_regs(Regs0),
    case member(Bin, CtxRegs) orelse member(Ctx, CtxRegs) of
	false ->
	    %% This bs_start_match2 does not reference any copy of the
	    %% match state. Therefore it can safely be passed on the
	    %% way to another (perhaps more suitable) bs_start_match2
	    %% instruction.
	    D = btb_follow_branch(F, Regs0, D0),
	    Regs = btb_kill_not_live(Live, Regs0),
	    btb_reaches_match_2(Is, Regs, D);
	true ->
	    %% This variant of the bs_start_match2 instruction does
	    %% not accept a match state as source.
	    btb_error(unsuitable_bs_start_match)
    end;
btb_reaches_match_2([{test,_,{f,F},Ss}=I|Is], Regs, D0) ->
    btb_ensure_not_used(Ss, I, Regs),
    D = btb_follow_branch(F, Regs, D0),
    btb_reaches_match_1(Is, Regs, D);
btb_reaches_match_2([{test,_,{f,F},_,Ss,_}=I|Is], Regs, D0) ->
    btb_ensure_not_used(Ss, I, Regs),
    D = btb_follow_branch(F, Regs, D0),
    btb_reaches_match_1(Is, Regs, D);
btb_reaches_match_2([{select,_,Src,{f,F},Conds}=I|Is], Regs, D0) ->
    btb_ensure_not_used([Src], I, Regs),
    D1 = btb_follow_branch(F, Regs, D0),
    D = btb_follow_branches(Conds, Regs, D1),
    btb_reaches_match_1(Is, Regs, D);
btb_reaches_match_2([{jump,{f,Lbl}}|_], Regs, #btb{index=Li}=D) ->
    Is = fetch_code_at(Lbl, Li),
    btb_reaches_match_2(Is, Regs, D);
btb_reaches_match_2([{label,_}|Is], Regs, D) ->
    btb_reaches_match_2(Is, Regs, D);
btb_reaches_match_2([{bs_init,{f,0},_,_,Ss,Dst}=I|Is], Regs, D) ->
    btb_ensure_not_used(Ss, I, Regs),
    btb_reaches_match_1(Is, btb_kill([Dst], Regs), D);
btb_reaches_match_2([{bs_put,{f,0},_,Ss}=I|Is], Regs, D) ->
    btb_ensure_not_used(Ss, I, Regs),
    btb_reaches_match_1(Is, Regs, D);
btb_reaches_match_2([{bs_restore2,Src,_}=I|Is], Regs0, D) ->
    case btb_contains_context(Src, Regs0) of
	false ->
	    btb_reaches_match_1(Is, Regs0, D);
	true ->
	    %% Check that all other copies of the context registers
	    %% are unused by the following instructions.
	    Regs = btb_kill([Src], Regs0),
	    CtxRegs = btb_context_regs(Regs),
	    case btb_are_all_unused(CtxRegs, Is, D) of
		false -> btb_error({CtxRegs,not_all_unused_after,I});
		true -> D#btb{must_not_save=true}
	    end
    end;
btb_reaches_match_2([{bs_context_to_binary,Src}=I|Is], Regs0, D) ->
    case btb_contains_context(Src, Regs0) of
	false ->
	    btb_reaches_match_1(Is, Regs0, D);
	true ->
	    %% Check that all other copies of the context registers
	    %% are unused by the following instructions.
	    Regs = btb_kill([Src], Regs0),
	    CtxRegs = btb_context_regs(Regs),
	    case btb_are_all_unused(CtxRegs, Is, D) of
		false -> btb_error({CtxRegs,not_all_unused_after,I});
		true -> D#btb{must_not_save=true}
	    end
    end;
btb_reaches_match_2([{badmatch,Src}=I|_], Regs, D) ->
    btb_ensure_not_used([Src], I, Regs),
    D;
btb_reaches_match_2([{case_end,Src}=I|_], Regs, D) ->
    btb_ensure_not_used([Src], I, Regs),
    D;
btb_reaches_match_2([if_end|_], _Regs, D) ->
    D;
btb_reaches_match_2([{func_info,_,_,Arity}=I|_], Regs0, D) ->
    Regs = btb_kill_yregs(btb_kill_not_live(Arity, Regs0)),
    case btb_context_regs(Regs) of
	[] -> D;
	_ -> {binary_used_in,I}
    end;
btb_reaches_match_2([{line,_}|Is], Regs, D) ->
    btb_reaches_match_1(Is, Regs, D);
btb_reaches_match_2([I|_], Regs, _) ->
    btb_error({btb_context_regs(Regs),I,not_handled}).

is_tail_call([{deallocate,_}|_]) -> true;
is_tail_call([return|_]) -> true;
is_tail_call(_) -> false.

btb_call(Arity, Lbl, Regs0, Is, D0) ->
    Regs = btb_kill_not_live(Arity, Regs0),
    case btb_are_x_registers_empty(Regs) of
	false ->
	    %% There is a match context in one of the x registers.
	    %% First handle the call as if it were a tail call.
	    D = btb_tail_call(Lbl, Regs, D0),

	    %% No problem so far (the called function can handle a
	    %% match context). Now we must make sure that we don't
	    %% have any copies of the match context tucked away in an
	    %% y register.
	    RegList = btb_context_regs(Regs),
	    case [R || {y,_}=R <- RegList] of
		[] ->
		    D;
		[_|_] ->
		    btb_error({multiple_uses,RegList})
	    end;
	true ->
	    %% No match context in any x register. It could have been
	    %% saved to an y register, so continue to scan the code following
	    %% the call.
	    btb_reaches_match_1(Is, Regs, D0)
    end.

btb_tail_call(Lbl, Regs, #btb{f=Ftree,must_save=MustSave0}=D) ->
    %% Ignore any y registers here.
    case [R || {x,_}=R <- btb_context_regs(Regs)] of
	[] ->
	    D;
	[{x,_}=Reg] ->
	    case gb_trees:lookup(Lbl, Ftree) of
		{value,{Reg,MustSave}} ->
		    D#btb{must_save=MustSave0 or MustSave};
		_ when is_integer(Lbl) ->
		    btb_error({{label,Lbl},no_suitable_bs_start_match});
		_ ->
		    btb_error({binary_used_in,Lbl})
	    end;
	[_|_] when not is_integer(Lbl) ->
	    btb_error({binary_used_in,Lbl});
	[_|_]=RegList ->
	    btb_error({multiple_uses,RegList})
    end.

%% btb_follow_branches([Cond], Regs, D) -> D'
%%  Recursively follow all the branches.

btb_follow_branches([{f,Lbl}|T], Regs, D0) ->
    D = btb_follow_branch(Lbl, Regs, D0),
    btb_follow_branches(T, Regs, D);
btb_follow_branches([_|T], Regs, D) ->
    btb_follow_branches(T, Regs, D);
btb_follow_branches([], _, D) -> D.

%% btb_follow_branch(Lbl, Regs, D) -> D'
%%  Recursively follow the branch.

btb_follow_branch(0, _Regs, D) -> D;
btb_follow_branch(Lbl, Regs, #btb{ok_br=Br0,index=Li}=D) ->
    Key = {Lbl,Regs},
    case gb_sets:is_member(Key, Br0) of
	true ->
	    %% We have already followed this branch and it was OK.
	    D;
	false ->
	    %% New branch. Try it.
	    Is = fetch_code_at(Lbl, Li),
	    #btb{ok_br=Br,must_not_save=MustNotSave,must_save=MustSave} =
		btb_reaches_match_1(Is, Regs, D),

	    %% Since we got back, this branch is OK.
	    D#btb{ok_br=gb_sets:insert(Key, Br),must_not_save=MustNotSave,
		  must_save=MustSave}
    end.

btb_reaches_match_block([{set,Ds,Ss,{alloc,Live,_}}=I|Is], Regs0) ->
    %% An allocation instruction or a GC bif. We'll kill all registers
    %% if any copy of the context is used as the source to the BIF.
    btb_ensure_not_used(Ss, I, Regs0),
    Regs1 = btb_kill_not_live(Live, Regs0),
    Regs = btb_kill(Ds, Regs1),
    btb_reaches_match_block(Is, Regs);
btb_reaches_match_block([{set,[Dst]=Ds,[Src],move}|Is], Regs0) ->
    Regs1 = btb_kill(Ds, Regs0),
    Regs = case btb_contains_context(Src, Regs1) of
	       false -> Regs1;
	       true -> btb_set_context(Dst, Regs1)
	   end,
    btb_reaches_match_block(Is, Regs);
btb_reaches_match_block([{set,Ds,Ss,_}=I|Is], Regs0) ->
    btb_ensure_not_used(Ss, I, Regs0),
    Regs = btb_kill(Ds, Regs0),
    btb_reaches_match_block(Is, Regs);
btb_reaches_match_block([], Regs) ->
    Regs.

%% btb_are_all_killed([Register], [Instruction], D) -> true|false
%%  Test whether all of the register are unused in the instruction stream.

btb_are_all_unused(RegList, Is, #btb{index=Li}) ->
    all(fun(R) ->
		beam_utils:is_not_used(R, Is, Li)
	end, RegList).

%% btp_regs_from_list([Register]) -> RegisterSet.
%%  Create a register set from a list of registers.

btb_regs_from_list(L) ->
    foldl(fun(R, Regs) ->
		  btb_set_context(R, Regs)
	  end, {0,0}, L).

%% btb_set_context(Register, RegisterSet) -> RegisterSet'
%%  Update RegisterSet to indicate that Register contains the matching context.

btb_set_context({x,N}, {Xregs,Yregs}) ->
    {Xregs bor (1 bsl N),Yregs};
btb_set_context({y,N}, {Xregs,Yregs}) ->
    {Xregs,Yregs bor (1 bsl N)}.

%% btb_ensure_not_used([Register], Instruction, RegisterSet) -> ok
%%  If any register in RegisterSet (the register(s) known to contain
%%  the match context) is used in the list of registers, generate an error.

btb_ensure_not_used(Rs, I, Regs) ->
    case lists:any(fun(R) -> btb_contains_context(R, Regs) end, Rs) of
	true -> btb_error({binary_used_in,I});
	false -> ok
    end.

%% btb_kill([Register], RegisterSet) -> RegisterSet'
%%  Kill all registers mentioned in the list of registers.

btb_kill([{x,N}|Rs], {Xregs,Yregs}) ->
    btb_kill(Rs, {Xregs band (bnot (1 bsl N)),Yregs});
btb_kill([{y,N}|Rs], {Xregs,Yregs}) ->
    btb_kill(Rs, {Xregs,Yregs band (bnot (1 bsl N))});
btb_kill([{fr,_}|Rs], Regs) ->
    btb_kill(Rs, Regs);
btb_kill([], Regs) -> Regs.

%% btb_kill_not_live(Live, RegisterSet) -> RegisterSet'
%%  Kill all registers indicated not live by Live.

btb_kill_not_live(Live, {Xregs,Yregs}) ->
    {Xregs band ((1 bsl Live)-1),Yregs}.

%% btb_kill(Regs0) -> Regs
%%  Kill all y registers.

btb_kill_yregs({Xregs,_}) -> {Xregs,0}.

%% btb_are_registers_empty(RegisterSet) -> true|false
%%  Test whether the register set is empty.

btb_are_registers_empty({0,0}) -> true;
btb_are_registers_empty({_,_}) -> false.

%% btb_are_x_registers_empty(Regs) -> true|false
%%  Test whether the x registers are empty.

btb_are_x_registers_empty({0,_}) -> true;
btb_are_x_registers_empty({_,_}) -> false.

%% btb_contains_context(Register, RegisterSet) -> true|false
%%  Test whether Register contains the context.

btb_contains_context({x,N}, {Regs,_}) -> Regs band (1 bsl N) =/= 0;
btb_contains_context({y,N}, {_,Regs}) -> Regs band (1 bsl N) =/= 0;
btb_contains_context(_, _) -> false.

%% btb_context_regs(RegisterSet) -> [Register]
%%  Convert the register set to an explicit list of registers.
btb_context_regs({Xregs,Yregs}) ->
    btb_context_regs_1(Xregs, 0, x, btb_context_regs_1(Yregs, 0, y, [])).

btb_context_regs_1(0, _, _, Acc) ->
    Acc;
btb_context_regs_1(Regs, N, Tag, Acc) when (Regs band 1) =:= 1 ->
    btb_context_regs_1(Regs bsr 1, N+1, Tag, [{Tag,N}|Acc]);
btb_context_regs_1(Regs, N, Tag, Acc) ->
    btb_context_regs_1(Regs bsr 1, N+1, Tag, Acc).

%% btb_index([Function]) -> GbTree({EntryLabel,{Register,MustSave}})
%%  Build an index of functions that accept a match context instead of
%%  a binary. MustSave is true if the function may pass the match
%%  context to the bs_context_to_binary instruction (in which case
%%  the current position in the binary must have saved into the
%%  start position using "bs_save_2 Ctx start").

btb_index(Fs) ->
    btb_index_1(Fs, []).

btb_index_1([{function,_,_,Entry,Is0}|Fs], Acc0) ->
    Is = drop_to_label(Is0, Entry),
    Acc = btb_index_2(Is, Entry, false, Acc0),
    btb_index_1(Fs, Acc);
btb_index_1([], Acc) -> gb_trees:from_orddict(sort(Acc)).

btb_index_2([{test,bs_start_match2,{f,_},_,[Reg,_],Reg}|_],
	    Entry, MustSave, Acc) ->
    [{Entry,{Reg,MustSave}}|Acc];
btb_index_2(Is0, Entry, _, Acc) ->
    try btb_index_find_start_match(Is0) of
	Is -> btb_index_2(Is, Entry, true, Acc)
    catch
	throw:none -> Acc
    end.

drop_to_label([{label,L}|Is], L) -> Is;
drop_to_label([_|Is], L) -> drop_to_label(Is, L).

btb_index_find_start_match([{test,_,{f,F},_},{bs_context_to_binary,_}|Is]) ->
    btb_index_find_label(Is, F);
btb_index_find_start_match(_) ->
    throw(none).

btb_index_find_label([{label,L}|Is], L) -> Is;
btb_index_find_label([_|Is], L) -> btb_index_find_label(Is, L).

btb_error(Error) ->
    throw({error,Error}).

fetch_code_at(Lbl, Li) ->
    case beam_utils:code_at(Lbl, Li) of
	Is when is_list(Is) -> Is
    end.

%%%
%%% Compilation information warnings.
%%%

btb_comment_opt({field_flags,[{anno,A}|_]}) ->
    {'%',{bin_opt,A}};
btb_comment_opt(_) ->
    {'%',{bin_opt,[]}}.

btb_comment_no_opt(Reason, {field_flags,[{anno,A}|_]}) ->
    {'%',{no_bin_opt,Reason,A}};
btb_comment_no_opt(Reason, _) ->
    {'%',{no_bin_opt,Reason,[]}}.

collect_warnings(Fs) ->
    D = warning_index_functions(Fs),
    foldl(fun(F, A) -> collect_warnings_fun(F, D, A) end, [], Fs).

collect_warnings_fun({function,_,_,_,Is}, D, A) ->
    collect_warnings_instr(Is, D, A).

collect_warnings_instr([{'%',{bin_opt,Where}}|Is], D, Acc0) ->
    Acc = add_warning(bin_opt, Where, Acc0),
    collect_warnings_instr(Is, D, Acc);
collect_warnings_instr([{'%',{no_bin_opt,Reason0,Where}}|Is], D, Acc0) ->
    Reason = warning_translate_label(Reason0, D),
    Acc = add_warning({no_bin_opt,Reason}, Where, Acc0),
    collect_warnings_instr(Is, D, Acc);
collect_warnings_instr([_|Is], D, Acc) ->
    collect_warnings_instr(Is, D, Acc);
collect_warnings_instr([], _, Acc) -> Acc.

add_warning(Term, Anno, Ws) ->
    Line = get_line(Anno),
    File = get_file(Anno),
    [{File,[{Line,?MODULE,Term}]}|Ws].

warning_translate_label(Term, D) when is_tuple(Term) ->
    case element(1, Term) of
	{label,F} ->
	    FA = gb_trees:get(F, D),
	    setelement(1, Term, FA);
	_ -> Term
    end;
warning_translate_label(Term, _) -> Term.

get_line([Line|_]) when is_integer(Line) -> Line;
get_line([_|T]) -> get_line(T);
get_line([]) -> none.
    
get_file([{file,File}|_]) -> File;
get_file([_|T]) -> get_file(T);
get_file([]) -> "no_file". % should not happen

warning_index_functions(Fs) ->
    D = [{Entry,{F,A}} || {function,F,A,Entry,_} <- Fs],
    gb_trees:from_orddict(sort(D)).

format_error_1({binary_used_in,{extfunc,M,F,A}}) ->
    [io_lib:format("sub binary used by ~p:~p/~p", [M,F,A])|
     case {M,F,A} of
	 {erlang,split_binary,2} ->
	     "; SUGGEST using binary matching instead of split_binary/2";
	 _ ->
	     ""
     end];
format_error_1({binary_used_in,_}) ->
    "sub binary is used or returned";
format_error_1({multiple_uses,_}) ->
    "sub binary is matched or used in more than one place";
format_error_1(unsuitable_bs_start_match) ->
    "the binary matching instruction that follows in the same function "
	"have problems that prevent delayed sub binary optimization "
	"(probably indicated by INFO warnings)";
format_error_1({{F,A},no_suitable_bs_start_match}) ->
    io_lib:format("called function ~p/~p does not begin with a suitable "
		  "binary matching instruction", [F,A]);
format_error_1(must_and_must_not_save) ->
    "different control paths use different positions in the binary";
format_error_1({_,I,not_handled}) ->
    case I of
	{'catch',_,_} ->
	    "the compiler currently does not attempt the delayed sub binary "
		"optimization when catch is used";
	{'try',_,_} ->
	    "the compiler currently does not attempt the delayed sub binary "
		"optimization when try/catch is used";
	_ ->
	    io_lib:format("compiler limitation: instruction ~p prevents "
			  "delayed sub binary optimization", [I])
    end;
format_error_1(Term) ->
    io_lib:format("~w", [Term]).
