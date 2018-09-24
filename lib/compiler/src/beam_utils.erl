%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2018. All Rights Reserved.
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
%% Purpose : Common utilities used by several optimization passes.
%% 

-module(beam_utils).
-export([is_killed/3,is_killed_at/3,is_not_used/3,
	 empty_label_index/0,index_label/3,index_labels/1,replace_labels/4,
	 code_at/2,is_pure_test/1,
	 split_even/1]).

-export_type([code_index/0,module_code/0,instruction/0]).

-import(lists, [map/2,member/2,sort/1,reverse/1]).

-define(is_const(Val), (Val =:= nil orelse
                        element(1, Val) =:= integer orelse
                        element(1, Val) =:= float orelse
                        element(1, Val) =:= atom orelse
                        element(1, Val) =:= literal)).

%% instruction() describes all instructions that are used during optimization
%% (from beam_a to beam_z).
-type instruction() :: atom() | tuple().

-type code_index() :: gb_trees:tree(beam_asm:label(), [instruction()]).

-type int_function() :: {'function',beam_asm:function_name(),arity(),
                         beam_asm:label(),[instruction()]}.

-type module_code() ::
        {module(),[_],[_],[int_function()],pos_integer()}.

%% Internal types.
-type fail() :: beam_asm:fail() | 'fail'.
-type test() :: {'test',atom(),fail(),[beam_asm:src()]} |
		{'test',atom(),fail(),integer(),list(),beam_asm:reg()}.
-type result_cache() :: gb_trees:tree(beam_asm:label(), 'killed' | 'used').

-record(live,
	{lbl :: code_index(),            %Label to code index.
	 res :: result_cache()}).        %Result cache for each label.

%% is_killed(Register, [Instruction], State) -> true|false
%%  Determine whether a register is killed by the instruction sequence.
%%  If true is returned, it means that the register will not be
%%  referenced in ANY way (not even indirectly by an allocate instruction);
%%  i.e. it is OK to enter the instruction sequence with Register
%%  containing garbage.
%%
%%  The state (constructed by index_instructions/1) is used to allow us
%%  to determine the kill state across branches.

-spec is_killed(beam_asm:reg(), [instruction()], code_index()) -> boolean().

is_killed(R, Is, D) ->
    St = #live{lbl=D,res=gb_trees:empty()},
    case check_liveness(R, Is, St) of
	{killed,_} -> true;
	{exit_not_used,_} -> false;
	{_,_} -> false
    end.

%% is_killed_at(Reg, Lbl, State) -> true|false
%%  Determine whether Reg is killed at label Lbl.

-spec is_killed_at(beam_asm:reg(), beam_asm:label(), code_index()) -> boolean().

is_killed_at(R, Lbl, D) when is_integer(Lbl) ->
    St0 = #live{lbl=D,res=gb_trees:empty()},
    case check_liveness_at(R, Lbl, St0) of
	{killed,_} -> true;
	{exit_not_used,_} -> false;
	{_,_} -> false
    end.

%% is_not_used(Register, [Instruction], State) -> true|false
%%  Determine whether a register is never used in the instruction sequence
%%  (it could still be referenced by an allocate instruction, meaning that
%%  it MUST be initialized, but that its value does not matter).
%%    The state is used to allow us to determine the usage state
%%  across branches.

-spec is_not_used(beam_asm:reg(), [instruction()], code_index()) -> boolean().

is_not_used(R, Is, D) ->
    St = #live{lbl=D,res=gb_trees:empty()},
    case check_liveness(R, Is, St) of
	{used,_} -> false;
	{exit_not_used,_} -> true;
	{_,_} -> true
    end.

%% index_labels(FunctionIs) -> State
%%  Index the instruction sequence so that we can quickly
%%  look up the instruction following a specific label.

-spec index_labels([instruction()]) -> code_index().

index_labels(Is) ->
    index_labels_1(Is, []).

%% empty_label_index() -> State
%%  Create an empty label index.

-spec empty_label_index() -> code_index().

empty_label_index() ->
    gb_trees:empty().

%% index_label(Label, [Instruction], State) -> State
%%  Add an index for a label.

-spec index_label(beam_asm:label(), [instruction()], code_index()) ->
   code_index().

index_label(Lbl, Is0, Acc) ->
    Is = drop_labels(Is0),
    gb_trees:enter(Lbl, Is, Acc).


%% code_at(Label, State) -> [I].
%%  Retrieve the code at the given label.

-spec code_at(beam_asm:label(), code_index()) -> [instruction()].

code_at(L, Ll) ->
    gb_trees:get(L, Ll).

%% replace_labels(FunctionIs, Tail, ReplaceDb, Fallback) -> FunctionIs.
%%  Replace all labels in instructions according to the ReplaceDb.
%%  If label is not found the Fallback is called with the label to
%%  produce a new one.

-spec replace_labels([instruction()],
                     [instruction()],
                     #{beam_asm:label() => beam_asm:label()},
                     fun((beam_asm:label()) -> term())) -> [instruction()].
replace_labels(Is, Acc, D, Fb) ->
    replace_labels_1(Is, Acc, D, Fb).

%% is_pure_test({test,Op,Fail,Ops}) -> true|false.
%%  Return 'true' if the test instruction does not modify any
%%  registers and/or bit syntax matching state.
%%

-spec is_pure_test(test()) -> boolean().

is_pure_test({test,is_eq,_,[_,_]}) -> true;
is_pure_test({test,is_ne,_,[_,_]}) -> true;
is_pure_test({test,is_eq_exact,_,[_,_]}) -> true;
is_pure_test({test,is_ne_exact,_,[_,_]}) -> true;
is_pure_test({test,is_ge,_,[_,_]}) -> true;
is_pure_test({test,is_lt,_,[_,_]}) -> true;
is_pure_test({test,is_nonempty_list,_,[_]}) -> true;
is_pure_test({test,is_tagged_tuple,_,[_,_,_]}) -> true;
is_pure_test({test,test_arity,_,[_,_]}) -> true;
is_pure_test({test,has_map_fields,_,[_|_]}) -> true;
is_pure_test({test,is_bitstr,_,[_]}) -> true;
is_pure_test({test,is_function2,_,[_,_]}) -> true;
is_pure_test({test,Op,_,Ops}) -> 
    erl_internal:new_type_test(Op, length(Ops)).

%% split_even/1
%% [1,2,3,4,5,6] -> {[1,3,5],[2,4,6]}

-spec split_even(list()) -> {list(),list()}.

split_even(Rs) -> split_even(Rs, [], []).

%%%
%%% Local functions.
%%%


%% check_liveness(Reg, [Instruction], #live{}) ->
%%                      {killed | not_used | used, #live{}}
%%  Find out whether Reg is used or killed in instruction sequence.
%%
%%    killed - Reg is assigned or killed by an allocation instruction.
%%    not_used - the value of Reg is not used, but Reg must not be garbage
%%    exit_not_used - the value of Reg is not used, but must not be garbage
%%                    because the stack will be scanned because an
%%                    exit BIF will raise an exception
%%    used - Reg is used

check_liveness({fr,_}, _, St) ->
    %% Conservatively always consider the floating point register used.
    {used,St};
check_liveness(R, [{block,Blk}|Is], St0) ->
    case check_liveness_block(R, Blk, St0) of
	{transparent,St1} ->
	    check_liveness(R, Is, St1);
	{alloc_used,St1} ->
            %% Used by an allocating instruction, but value not referenced.
            %% Must check the rest of the instructions.
	    not_used(check_liveness(R, Is, St1));
	{Other,_}=Res when is_atom(Other) ->
	    Res
    end;
check_liveness(R, [{label,_}|Is], St) ->
    check_liveness(R, Is, St);
check_liveness(R, [{test,_,{f,Fail},As}|Is], St0) ->
    case member(R, As) of
	true ->
	    {used,St0};
	false ->
	    case check_liveness_at(R, Fail, St0) of
		{killed,St1} ->
		    check_liveness(R, Is, St1);
		{exit_not_used,St1} ->
		    not_used(check_liveness(R, Is, St1));
		{not_used,St1} ->
		    not_used(check_liveness(R, Is, St1));
		{used,_}=Used ->
		    Used
	    end
    end;
check_liveness(R, [{test,Op,Fail,Live,Ss,Dst}|Is], St) ->
    %% Check this instruction as a block to get a less conservative
    %% result if the caller is is_not_used/3.
    Block = [{set,[Dst],Ss,{alloc,Live,{bif,Op,Fail}}}],
    check_liveness(R, [{block,Block}|Is], St);
check_liveness(R, [{select,_,R,_,_}|_], St) ->
    {used,St};
check_liveness(R, [{select,_,_,Fail,Branches}|_], St) ->
    check_liveness_everywhere(R, [Fail|Branches], St);
check_liveness(R, [{jump,{f,F}}|_], St) ->
    check_liveness_at(R, F, St);
check_liveness(R, [{case_end,Used}|_], St) ->
    check_liveness_exit(R, Used, St);
check_liveness(R, [{try_case_end,Used}|_], St) ->
    check_liveness_exit(R, Used, St);
check_liveness(R, [{badmatch,Used}|_], St) ->
    check_liveness_exit(R, Used, St);
check_liveness(R, [if_end|_], St) ->
    check_liveness_exit(R, ignore, St);
check_liveness(R, [{func_info,_,_,Ar}|_], St) ->
    case R of
	{x,X} when X < Ar -> {used,St};
	_ -> {killed,St}
    end;
check_liveness(R, [{kill,R}|_], St) ->
    {killed,St};
check_liveness(R, [{kill,_}|Is], St) ->
    check_liveness(R, Is, St);
check_liveness(R, [{bs_init,_,_,none,Ss,Dst}|Is], St) ->
    case member(R, Ss) of
	true ->
	    {used,St};
	false ->
	    if
		R =:= Dst -> {killed,St};
		true -> check_liveness(R, Is, St)
	    end
    end;
check_liveness(R, [{bs_init,_,_,Live,Ss,Dst}|Is], St) ->
    case R of
	{x,X} ->
            case member(R, Ss) of
                true ->
                    {used,St};
                false ->
                    if
                        X < Live ->
                            not_used(check_liveness(R, Is, St));
                        true ->
                            {killed,St}
                    end
	    end;
	{y,_} ->
	    case member(R, Ss) of
		true -> {used,St};
		false ->
                    %% If the exception is taken, the stack may
                    %% be scanned. Therefore the register is not
                    %% guaranteed to be killed.
		    if
                        R =:= Dst -> {not_used,St};
			true -> not_used(check_liveness(R, Is, St))
		    end
	    end
    end;
check_liveness(R, [{deallocate,_}|Is], St) ->
    case R of
	{y,_} -> {killed,St};
	_ -> check_liveness(R, Is, St)
    end;
check_liveness({x,_}=R, [return|_], St) ->
    case R of
	{x,0} -> {used,St};
	{x,_} -> {killed,St}
    end;
check_liveness(R, [{call,Live,_}|Is], St) ->
    case R of
	{x,X} when X < Live -> {used,St};
	{x,_} -> {killed,St};
	{y,_} -> not_used(check_liveness(R, Is, St))
    end;
check_liveness(R, [{call_ext,Live,_}=I|Is], St) ->
    case R of
	{x,X} when X < Live ->
	    {used,St};
	{x,_} ->
	    {killed,St};
	{y,_} ->
	    case beam_jump:is_exit_instruction(I) of
		false ->
		    not_used(check_liveness(R, Is, St));
		true ->
		    %% We must make sure we don't check beyond this
		    %% instruction or we will fall through into random
		    %% unrelated code and get stuck in a loop.
		    {exit_not_used,St}
	    end
    end;
check_liveness(R, [{call_fun,Live}|Is], St) ->
    case R of
	{x,X} when X =< Live -> {used,St};
	{x,_} -> {killed,St};
	{y,_} -> not_used(check_liveness(R, Is, St))
    end;
check_liveness(R, [{apply,Args}|Is], St) ->
    case R of
	{x,X} when X < Args+2 -> {used,St};
	{x,_} -> {killed,St};
	{y,_} -> not_used(check_liveness(R, Is, St))
    end;
check_liveness(R, [{bif,Op,Fail,Ss,D}|Is], St) ->
    Set = {set,[D],Ss,{bif,Op,Fail}},
    check_liveness(R, [{block,[Set]}|Is], St);
check_liveness(R, [{gc_bif,Op,{f,Fail},Live,Ss,D}|Is], St) ->
    Set = {set,[D],Ss,{alloc,Live,{gc_bif,Op,Fail}}},
    check_liveness(R, [{block,[Set]}|Is], St);
check_liveness(R, [{bs_put,{f,0},_,Ss}|Is], St) ->
    case member(R, Ss) of
	true -> {used,St};
	false -> check_liveness(R, Is, St)
    end;
check_liveness(R, [{bs_restore2,S,_}|Is], St) ->
    case R of
	S -> {used,St};
	_ -> check_liveness(R, Is, St)
    end;
check_liveness(R, [{bs_save2,S,_}|Is], St) ->
    case R of
	S -> {used,St};
	_ -> check_liveness(R, Is, St)
    end;
check_liveness(R, [{move,S,D}|Is], St) ->
    case R of
	S -> {used,St};
	D -> {killed,St};
	_ -> check_liveness(R, Is, St)
    end;
check_liveness(R, [{make_fun2,_,_,_,NumFree}|Is], St) ->
    case R of
	{x,X} when X < NumFree -> {used,St};
	{x,_} -> {killed,St};
	{y,_} -> not_used(check_liveness(R, Is, St))
    end;
check_liveness(R, [{'catch'=Op,Y,Fail}|Is], St) ->
    Set = {set,[Y],[],{try_catch,Op,Fail}},
    check_liveness(R, [{block,[Set]}|Is], St);
check_liveness(R, [{'try'=Op,Y,Fail}|Is], St) ->
    Set = {set,[Y],[],{try_catch,Op,Fail}},
    check_liveness(R, [{block,[Set]}|Is], St);
check_liveness(R, [{try_end,Y}|Is], St) ->
    case R of
	Y ->
	    {killed,St};
	{y,_} ->
	    %% y registers will be used if an exception occurs and
	    %% control transfers to the label given in the previous
	    %% try/2 instruction.
	    {used,St};
	_ ->
	    check_liveness(R, Is, St)
    end;
check_liveness(R, [{catch_end,Y}|Is], St) ->
    case R of
	Y -> {killed,St};
	_ -> check_liveness(R, Is, St)
    end;
check_liveness(R, [{get_tuple_element,S,_,D}|Is], St) ->
    case R of
	S -> {used,St};
	D -> {killed,St};
	_ -> check_liveness(R, Is, St)
    end;
check_liveness(R, [{bs_context_to_binary,S}|Is], St) ->
    case R of
	S -> {used,St};
	_ -> check_liveness(R, Is, St)
    end;
check_liveness(R, [{loop_rec,{f,_},{x,0}}|_], St) ->
    case R of
	{x,_} ->
	    {killed,St};
	_ ->
	    %% y register. Rarely happens. Be very conversative and
	    %% assume it's used.
	    {used,St}
    end;
check_liveness(R, [{loop_rec_end,{f,Fail}}|_], St) ->
    check_liveness_at(R, Fail, St);
check_liveness(R, [{line,_}|Is], St) ->
    check_liveness(R, Is, St);
check_liveness(R, [{get_map_elements,{f,Fail},S,{list,L}}|Is], St0) ->
    {Ss,Ds} = split_even(L),
    case member(R, [S|Ss]) of
	true ->
	    {used,St0};
	false ->
	    case check_liveness_at(R, Fail, St0) of
		{killed,St}=Killed ->
		    case member(R, Ds) of
			true -> Killed;
			false -> check_liveness(R, Is, St)
		    end;
		Other ->
		    Other
	    end
    end;
check_liveness(R, [{put_map,F,Op,S,D,Live,{list,Puts}}|Is], St) ->
    Set = {set,[D],[S|Puts],{alloc,Live,{put_map,Op,F}}},
    check_liveness(R, [{block,[Set]}||Is], St);
check_liveness(R, [{put_tuple,Ar,D}|Is], St) ->
    Set = {set,[D],[],{put_tuple,Ar}},
    check_liveness(R, [{block,[Set]}||Is], St);
check_liveness(R, [{put_list,S1,S2,D}|Is], St) ->
    Set = {set,[D],[S1,S2],put_list},
    check_liveness(R, [{block,[Set]}||Is], St);
check_liveness(R, [{test_heap,N,Live}|Is], St) ->
    I = {block,[{set,[],[],{alloc,Live,{nozero,nostack,N,[]}}}]},
    check_liveness(R, [I|Is], St);
check_liveness(R, [{allocate_zero,N,Live}|Is], St) ->
    I = {block,[{set,[],[],{alloc,Live,{zero,N,0,[]}}}]},
    check_liveness(R, [I|Is], St);
check_liveness(R, [{get_hd,S,D}|Is], St) ->
    I = {block,[{set,[D],[S],get_hd}]},
    check_liveness(R, [I|Is], St);
check_liveness(R, [{get_tl,S,D}|Is], St) ->
    I = {block,[{set,[D],[S],get_tl}]},
    check_liveness(R, [I|Is], St);
check_liveness(R, [remove_message|Is], St) ->
    check_liveness(R, Is, St);
check_liveness({x,X}, [build_stacktrace|_], St) when X > 0 ->
    {killed,St};
check_liveness(R, [{recv_mark,_}|Is], St) ->
    check_liveness(R, Is, St);
check_liveness(R, [{recv_set,_}|Is], St) ->
    check_liveness(R, Is, St);
check_liveness(R, [{'%',_}|Is], St) ->
    check_liveness(R, Is, St);
check_liveness(_R, Is, St) when is_list(Is) ->
    %% Not implemented. Conservatively assume that the register is used.
    {used,St}.

check_liveness_everywhere(R, Lbls, St0) ->
    check_liveness_everywhere_1(R, Lbls, killed, St0).

check_liveness_everywhere_1(R, [{f,Lbl}|T], Res0, St0) ->
    {Res1,St} = check_liveness_at(R, Lbl, St0),
    Res = case Res1 of
	      killed -> Res0;
	      _ -> Res1
	  end,
    case Res of
	used -> {used,St};
	_ -> check_liveness_everywhere_1(R, T, Res, St)
    end;
check_liveness_everywhere_1(R, [_|T], Res, St) ->
    check_liveness_everywhere_1(R, T, Res, St);
check_liveness_everywhere_1(_, [], Res, St) ->
    {Res,St}.

check_liveness_at(R, Lbl, #live{lbl=Ll,res=ResMemorized}=St0) ->
    case gb_trees:lookup(Lbl, ResMemorized) of
	{value,Res} ->
	    {Res,St0};
	none ->
	    {Res,St} = case gb_trees:lookup(Lbl, Ll) of
			   {value,Is} -> check_liveness(R, Is, St0);
			   none -> {used,St0}
		       end,
	    {Res,St#live{res=gb_trees:insert(Lbl, Res, St#live.res)}}
    end.

not_used({used,_}=Res) -> Res;
not_used({_,St}) -> {not_used,St}.

check_liveness_exit(R, R, St) -> {used,St};
check_liveness_exit({x,_}, _, St) -> {killed,St};
check_liveness_exit({y,_}, _, St) -> {exit_not_used,St}.

%% check_liveness_block(Reg, [Instruction], State) ->
%%     {killed | not_used | used | alloc_used | transparent,State'}
%%  Finds out how Reg is used in the instruction sequence inside a block.
%%  Returns one of:
%%    killed - Reg is assigned a new value or killed by an
%%       allocation instruction
%%    not_used - The value is not used, but the register is referenced
%%       e.g. by an allocation instruction
%%    transparent - Reg is neither used nor killed
%%    alloc_used - Used only in an allocate instruction
%%    used - Reg is explicitly used by an instruction
%%
%%  Annotations are not allowed.
%%
%%  (Unknown instructions will cause an exception.)

check_liveness_block({x,X}=R, [{set,Ds,Ss,{alloc,Live,Op}}|Is], St0) ->
    if 
	X >= Live ->
	    {killed,St0};
	true ->
	    case check_liveness_block_1(R, Ss, Ds, Op, Is, St0) of
                {transparent,St} -> {alloc_used,St};
		{_,_}=Res -> not_used(Res)
	    end
    end;
check_liveness_block({y,_}=R, [{set,Ds,Ss,{alloc,_Live,Op}}|Is], St0) ->
    case check_liveness_block_1(R, Ss, Ds, Op, Is, St0) of
        {transparent,St} -> {alloc_used,St};
        {_,_}=Res -> not_used(Res)
    end;
check_liveness_block({y,_}=R, [{set,Ds,Ss,{try_catch,_,Op}}|Is], St0) ->
    case Ds of
        [R] ->
            {killed,St0};
        _ ->
            case check_liveness_block_1(R, Ss, Ds, Op, Is, St0) of
                {exit_not_used,St} ->
                    {used,St};
                {transparent,St} ->
                    %% Conservatively assumed that it is used.
                    {used,St};
                {_,_}=Res ->
                    Res
            end
    end;
check_liveness_block(R, [{set,Ds,Ss,Op}|Is], St) ->
    check_liveness_block_1(R, Ss, Ds, Op, Is, St);
check_liveness_block(_, [], St) -> {transparent,St}.

check_liveness_block_1(R, Ss, Ds, Op, Is, St0) ->
    case member(R, Ss) of
	true ->
	    {used,St0};
	false ->
	    case check_liveness_block_2(R, Op, Ss, St0) of
		{killed,St} ->
		    case member(R, Ds) of
			true -> {killed,St};
			false -> check_liveness_block(R, Is, St)
		    end;
		{exit_not_used,St} ->
		    case member(R, Ds) of
			true -> {exit_not_used,St};
			false -> check_liveness_block(R, Is, St)
		    end;
		{not_used,St} ->
		    not_used(case member(R, Ds) of
				 true -> {killed,St};
				 false -> check_liveness_block(R, Is, St)
			     end);
		{used,St} ->
		    {used,St}
	    end
    end.

check_liveness_block_2(R, {gc_bif,Op,{f,Lbl}}, Ss, St) ->
    check_liveness_block_3(R, Lbl, {Op,length(Ss)}, St);
check_liveness_block_2(R, {bif,Op,{f,Lbl}}, Ss, St) ->
    Arity = length(Ss),
    case erl_internal:comp_op(Op, Arity) orelse
	erl_internal:new_type_test(Op, Arity) of
	true ->
	    {killed,St};
	false ->
	    check_liveness_block_3(R, Lbl, {Op,length(Ss)}, St)
    end;
check_liveness_block_2(R, {put_map,_Op,{f,Lbl}}, _Ss, St) ->
    check_liveness_block_3(R, Lbl, {unsafe,0}, St);
check_liveness_block_2(_, _, _, St) ->
    {killed,St}.

check_liveness_block_3({x,_}, 0, _FA, St) ->
    {killed,St};
check_liveness_block_3({y,_}, 0, {F,A}, St) ->
    %% If the exception is thrown, the stack may be scanned,
    %% thus implicitly using the y register.
    case erl_bifs:is_safe(erlang, F, A) of
        true -> {killed,St};
        false -> {used,St}
    end;
check_liveness_block_3(R, Lbl, _FA, St0) ->
    check_liveness_at(R, Lbl, St0).

index_labels_1([{label,Lbl}|Is0], Acc) ->
    Is = drop_labels(Is0),
    index_labels_1(Is0, [{Lbl,Is}|Acc]);
index_labels_1([_|Is], Acc) ->
    index_labels_1(Is, Acc);
index_labels_1([], Acc) -> gb_trees:from_orddict(sort(Acc)).

drop_labels([{label,_}|Is]) -> drop_labels(Is);
drop_labels(Is) -> Is.


replace_labels_1([{test,Test,{f,Lbl},Ops}|Is], Acc, D, Fb) ->
    replace_labels_1(Is, [{test,Test,{f,label(Lbl, D, Fb)},Ops}|Acc], D, Fb);
replace_labels_1([{test,Test,{f,Lbl},Live,Ops,Dst}|Is], Acc, D, Fb) ->
    replace_labels_1(Is, [{test,Test,{f,label(Lbl, D, Fb)},Live,Ops,Dst}|Acc], D, Fb);
replace_labels_1([{select,I,R,{f,Fail0},Vls0}|Is], Acc, D, Fb) ->
    Vls = map(fun ({f,L}) -> {f,label(L, D, Fb)};
		   (Other) -> Other
	      end, Vls0),
    Fail = label(Fail0, D, Fb),
    replace_labels_1(Is, [{select,I,R,{f,Fail},Vls}|Acc], D, Fb);
replace_labels_1([{'try',R,{f,Lbl}}|Is], Acc, D, Fb) ->
    replace_labels_1(Is, [{'try',R,{f,label(Lbl, D, Fb)}}|Acc], D, Fb);
replace_labels_1([{'catch',R,{f,Lbl}}|Is], Acc, D, Fb) ->
    replace_labels_1(Is, [{'catch',R,{f,label(Lbl, D, Fb)}}|Acc], D, Fb);
replace_labels_1([{jump,{f,Lbl}}|Is], Acc, D, Fb) ->
    replace_labels_1(Is, [{jump,{f,label(Lbl, D, Fb)}}|Acc], D, Fb);
replace_labels_1([{loop_rec,{f,Lbl},R}|Is], Acc, D, Fb) ->
    replace_labels_1(Is, [{loop_rec,{f,label(Lbl, D, Fb)},R}|Acc], D, Fb);
replace_labels_1([{loop_rec_end,{f,Lbl}}|Is], Acc, D, Fb) ->
    replace_labels_1(Is, [{loop_rec_end,{f,label(Lbl, D, Fb)}}|Acc], D, Fb);
replace_labels_1([{wait,{f,Lbl}}|Is], Acc, D, Fb) ->
    replace_labels_1(Is, [{wait,{f,label(Lbl, D, Fb)}}|Acc], D, Fb);
replace_labels_1([{wait_timeout,{f,Lbl},To}|Is], Acc, D, Fb) ->
    replace_labels_1(Is, [{wait_timeout,{f,label(Lbl, D, Fb)},To}|Acc], D, Fb);
replace_labels_1([{recv_mark=Op,{f,Lbl}}|Is], Acc, D, Fb) ->
    replace_labels_1(Is, [{Op,{f,label(Lbl, D, Fb)}}|Acc], D, Fb);
replace_labels_1([{recv_set=Op,{f,Lbl}}|Is], Acc, D, Fb) ->
    replace_labels_1(Is, [{Op,{f,label(Lbl, D, Fb)}}|Acc], D, Fb);
replace_labels_1([{bif,Name,{f,Lbl},As,R}|Is], Acc, D, Fb) when Lbl =/= 0 ->
    replace_labels_1(Is, [{bif,Name,{f,label(Lbl, D, Fb)},As,R}|Acc], D, Fb);
replace_labels_1([{gc_bif,Name,{f,Lbl},Live,As,R}|Is], Acc, D, Fb) when Lbl =/= 0 ->
    replace_labels_1(Is, [{gc_bif,Name,{f,label(Lbl, D, Fb)},Live,As,R}|Acc], D, Fb);
replace_labels_1([{call,Ar,{f,Lbl}}|Is], Acc, D, Fb) ->
    replace_labels_1(Is, [{call,Ar,{f,label(Lbl, D, Fb)}}|Acc], D, Fb);
replace_labels_1([{make_fun2,{f,Lbl},U1,U2,U3}|Is], Acc, D, Fb) ->
    replace_labels_1(Is, [{make_fun2,{f,label(Lbl, D, Fb)},U1,U2,U3}|Acc], D, Fb);
replace_labels_1([{bs_init,{f,Lbl},Info,Live,Ss,Dst}|Is], Acc, D, Fb) when Lbl =/= 0 ->
    replace_labels_1(Is, [{bs_init,{f,label(Lbl, D, Fb)},Info,Live,Ss,Dst}|Acc], D, Fb);
replace_labels_1([{bs_put,{f,Lbl},Info,Ss}|Is], Acc, D, Fb) when Lbl =/= 0 ->
    replace_labels_1(Is, [{bs_put,{f,label(Lbl, D, Fb)},Info,Ss}|Acc], D, Fb);
replace_labels_1([{put_map=I,{f,Lbl},Op,Src,Dst,Live,List}|Is], Acc, D, Fb)
  when Lbl =/= 0 ->
    replace_labels_1(Is, [{I,{f,label(Lbl, D, Fb)},Op,Src,Dst,Live,List}|Acc], D, Fb);
replace_labels_1([{get_map_elements=I,{f,Lbl},Src,List}|Is], Acc, D, Fb) when Lbl =/= 0 ->
    replace_labels_1(Is, [{I,{f,label(Lbl, D, Fb)},Src,List}|Acc], D, Fb);
replace_labels_1([I|Is], Acc, D, Fb) ->
    replace_labels_1(Is, [I|Acc], D, Fb);
replace_labels_1([], Acc, _, _) -> Acc.

label(Old, D, Fb) ->
    case D of
        #{Old := New} -> New;
        _ -> Fb(Old)
    end.

%% live_opt/4.

split_even([], Ss, Ds) ->
    {reverse(Ss),reverse(Ds)};
split_even([S,D|Rs], Ss, Ds) ->
    split_even(Rs, [S|Ss], [D|Ds]).
