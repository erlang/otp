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
%% Purpose : Common utilities used by several optimization passes.
%% 

-module(beam_utils).
-export([is_killed_block/2,is_killed/3,is_killed_at/3,
	 is_not_used/3,is_not_used_at/3,
	 empty_label_index/0,index_label/3,index_labels/1,
	 code_at/2,bif_to_test/3,is_pure_test/1,
	 live_opt/1,delete_live_annos/1,combine_heap_needs/2,
	 join_even/2,split_even/1]).

-import(lists, [member/2,sort/1,reverse/1,splitwith/2]).

-record(live,
	{bl,					%Block check fun.
	 lbl,					%Label to code index.
	 res}).					%Result cache for each label.


%% is_killed_block(Register, [Instruction]) -> true|false
%%  Determine whether a register is killed by the instruction sequence inside
%%  a block.
%%
%%  If true is returned, it means that the register will not be
%%  referenced in ANY way (not even indirectly by an allocate instruction);
%%  i.e. it is OK to enter the instruction sequence with Register
%%  containing garbage.

is_killed_block(R, Is) ->
    case check_killed_block(R, Is) of
	killed -> true;
	used -> false;
	transparent -> false
    end.

%% is_killed(Register, [Instruction], State) -> true|false
%%  Determine whether a register is killed by the instruction sequence.
%%  If true is returned, it means that the register will not be
%%  referenced in ANY way (not even indirectly by an allocate instruction);
%%  i.e. it is OK to enter the instruction sequence with Register
%%  containing garbage.
%%
%%  The state (constructed by index_instructions/1) is used to allow us
%%  to determine the kill state across branches.

is_killed(R, Is, D) ->
    St = #live{bl=check_killed_block_fun(),lbl=D,res=gb_trees:empty()},
    case check_liveness(R, Is, St) of
	{killed,_} -> true;
	{used,_} -> false
    end.

%% is_killed_at(Reg, Lbl, State) -> true|false
%%  Determine whether Reg is killed at label Lbl.

is_killed_at(R, Lbl, D) when is_integer(Lbl) ->
    St0 = #live{bl=check_killed_block_fun(),lbl=D,res=gb_trees:empty()},
    case check_liveness_at(R, Lbl, St0) of
	{killed,_} -> true;
	{used,_} -> false
    end.

%% is_not_used(Register, [Instruction], State) -> true|false
%%  Determine whether a register is never used in the instruction sequence
%%  (it could still be referenced by an allocate instruction, meaning that
%%  it MUST be initialized, but that its value does not matter).
%%    The state is used to allow us to determine the usage state
%%  across branches.

is_not_used(R, Is, D) ->
    St = #live{bl=fun check_used_block/3,lbl=D,res=gb_trees:empty()},
    case check_liveness(R, Is, St) of
	{killed,_} -> true;
	{used,_} -> false
    end.

%% is_not_used(Register, [Instruction], State) -> true|false
%%  Determine whether a register is never used in the instruction sequence
%%  (it could still be referenced by an allocate instruction, meaning that
%%  it MUST be initialized, but that its value does not matter).
%%    The state is used to allow us to determine the usage state
%%  across branches.

is_not_used_at(R, Lbl, D) ->
    St = #live{bl=fun check_used_block/3,lbl=D,res=gb_trees:empty()},
    case check_liveness_at(R, Lbl, St) of
	{killed,_} -> true;
	{used,_} -> false
    end.

%% index_labels(FunctionIs) -> State
%%  Index the instruction sequence so that we can quickly
%%  look up the instruction following a specific label.

index_labels(Is) ->
    index_labels_1(Is, []).

%% empty_label_index() -> State
%%  Create an empty label index.

empty_label_index() ->
    gb_trees:empty().

%% index_label(Label, [Instruction], State) -> State
%%  Add an index for a label.

index_label(Lbl, Is0, Acc) ->
    Is = drop_labels(Is0),
    gb_trees:enter(Lbl, Is, Acc).


%% code_at(Label, State) -> [I].
%%  Retrieve the code at the given label.

code_at(L, Ll) ->
    gb_trees:get(L, Ll).

%% bif_to_test(Bif, [Op], Fail) -> {test,Test,Fail,[Op]}
%%  Convert a BIF to a test. Fail if not possible.

bif_to_test(is_atom,     [_]=Ops, Fail) -> {test,is_atom,Fail,Ops};
bif_to_test(is_boolean,  [_]=Ops, Fail) -> {test,is_boolean,Fail,Ops};
bif_to_test(is_binary,   [_]=Ops, Fail) -> {test,is_binary,Fail,Ops};
bif_to_test(is_bitstring,[_]=Ops, Fail) -> {test,is_bitstr,Fail,Ops};
bif_to_test(is_float,    [_]=Ops, Fail) -> {test,is_float,Fail,Ops};
bif_to_test(is_function, [_]=Ops, Fail) -> {test,is_function,Fail,Ops};
bif_to_test(is_function, [_,_]=Ops, Fail) -> {test,is_function2,Fail,Ops};
bif_to_test(is_integer,  [_]=Ops, Fail) -> {test,is_integer,Fail,Ops};
bif_to_test(is_list,     [_]=Ops, Fail) -> {test,is_list,Fail,Ops};
bif_to_test(is_map,      [_]=Ops, Fail) -> {test,is_map,Fail,Ops};
bif_to_test(is_number,   [_]=Ops, Fail) -> {test,is_number,Fail,Ops};
bif_to_test(is_pid,      [_]=Ops, Fail) -> {test,is_pid,Fail,Ops};
bif_to_test(is_port,     [_]=Ops, Fail) -> {test,is_port,Fail,Ops};
bif_to_test(is_reference, [_]=Ops, Fail) -> {test,is_reference,Fail,Ops};
bif_to_test(is_tuple,    [_]=Ops, Fail)     -> {test,is_tuple,Fail,Ops};
bif_to_test('=<', [A,B], Fail) -> {test,is_ge,Fail,[B,A]};
bif_to_test('>', [A,B], Fail) -> {test,is_lt,Fail,[B,A]};
bif_to_test('<', [_,_]=Ops, Fail) -> {test,is_lt,Fail,Ops};
bif_to_test('>=', [_,_]=Ops, Fail) -> {test,is_ge,Fail,Ops};
bif_to_test('==', [A,nil], Fail) -> {test,is_nil,Fail,[A]};
bif_to_test('==', [_,_]=Ops, Fail) -> {test,is_eq,Fail,Ops};
bif_to_test('/=', [_,_]=Ops, Fail) -> {test,is_ne,Fail,Ops};
bif_to_test('=:=', [A,nil], Fail) -> {test,is_nil,Fail,[A]};
bif_to_test('=:=', [_,_]=Ops, Fail) -> {test,is_eq_exact,Fail,Ops};
bif_to_test('=/=', [_,_]=Ops, Fail) -> {test,is_ne_exact,Fail,Ops};
bif_to_test(is_record, [_,_,_]=Ops, Fail) -> {test,is_record,Fail,Ops}.


%% is_pure_test({test,Op,Fail,Ops}) -> true|false.
%%  Return 'true' if the test instruction does not modify any
%%  registers and/or bit syntax matching state.
%%
is_pure_test({test,is_eq,_,[_,_]}) -> true;
is_pure_test({test,is_ne,_,[_,_]}) -> true;
is_pure_test({test,is_eq_exact,_,[_,_]}) -> true;
is_pure_test({test,is_ne_exact,_,[_,_]}) -> true;
is_pure_test({test,is_ge,_,[_,_]}) -> true;
is_pure_test({test,is_lt,_,[_,_]}) -> true;
is_pure_test({test,is_nil,_,[_]}) -> true;
is_pure_test({test,is_nonempty_list,_,[_]}) -> true;
is_pure_test({test,test_arity,_,[_,_]}) -> true;
is_pure_test({test,has_map_fields,_,[_|_]}) -> true;
is_pure_test({test,is_bitstr,_,[_]}) -> true;
is_pure_test({test,is_function2,_,[_,_]}) -> true;
is_pure_test({test,Op,_,Ops}) -> 
    erl_internal:new_type_test(Op, length(Ops)).

    
%% live_opt([Instruction]) -> [Instruction].
%%  Go through the instruction sequence in reverse execution
%%  order, keep track of liveness and remove 'move' instructions
%%  whose destination is a register that will not be used.
%%  Also insert {'%live',Live,Regs} annotations at the beginning
%%  and end of each block.
%%
live_opt(Is0) ->
    {[{label,Fail}|_]=Bef,[Fi|Is]} =
	splitwith(fun({func_info,_,_,_}) -> false;
		     (_) -> true
		  end, Is0),
    {func_info,_,_,Live} = Fi,
    D = gb_trees:insert(Fail, live_call(Live), gb_trees:empty()),
    Bef ++ [Fi|live_opt(reverse(Is), 0, D, [])].


%% delete_live_annos([Instruction]) -> [Instruction].
%%  Delete all live annotations.
%%
delete_live_annos([{block,Bl0}|Is]) ->
    case delete_live_annos(Bl0) of
	[] -> delete_live_annos(Is);
	[_|_]=Bl -> [{block,Bl}|delete_live_annos(Is)]
    end;
delete_live_annos([{'%live',_,_}|Is]) ->
    delete_live_annos(Is);
delete_live_annos([I|Is]) ->
    [I|delete_live_annos(Is)];
delete_live_annos([]) -> [].
    
%% combine_heap_needs(HeapNeed1, HeapNeed2) -> HeapNeed
%%  Combine the heap need for two allocation instructions.

combine_heap_needs({alloc,Alloc1}, {alloc,Alloc2}) ->
    {alloc,combine_alloc_lists(Alloc1, Alloc2)};
combine_heap_needs({alloc,Alloc}, Words) when is_integer(Words) ->
    {alloc,combine_alloc_lists(Alloc, [{words,Words}])};
combine_heap_needs(Words, {alloc,Alloc}) when is_integer(Words) ->
    {alloc,combine_alloc_lists(Alloc, [{words,Words}])};
combine_heap_needs(H1, H2) when is_integer(H1), is_integer(H2) ->
    H1+H2.

%% split_even/1
%% [1,2,3,4,5,6] -> {[1,3,5],[2,4,6]}

split_even(Rs) -> split_even(Rs, [], []).

%% join_even/1
%% {[1,3,5],[2,4,6]} -> [1,2,3,4,5,6]

join_even([], []) -> [];
join_even([S|Ss], [D|Ds]) -> [S,D|join_even(Ss, Ds)].

%%%
%%% Local functions.
%%%


%% check_liveness(Reg, [Instruction], #live{}) ->
%%                      {killed | used, #live{}}
%%  Find out whether Reg is used or killed in instruction sequence.
%%  'killed' means that Reg is assigned a new value or killed by an
%%  allocation instruction. 'used' means that Reg is used in some way.

check_liveness(R, [{block,Blk}|Is], #live{bl=BlockCheck}=St0) ->
    case BlockCheck(R, Blk, St0) of
	{transparent,St} -> check_liveness(R, Is, St);
	{Other,_}=Res when is_atom(Other) -> Res
    end;
check_liveness(R, [{label,_}|Is], St) ->
    check_liveness(R, Is, St);
check_liveness(R, [{test,_,{f,Fail},As}|Is], St0) ->
    case member(R, As) of
	true ->
	    {used,St0};
	false ->
	    case check_liveness_at(R, Fail, St0) of
		{killed,St} -> check_liveness(R, Is, St);
		{_,_}=Other -> Other
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
    check_liveness_ret(R, Used, St);
check_liveness(R, [{badmatch,Used}|_], St) ->
    check_liveness_ret(R, Used, St);
check_liveness(_, [if_end|_], St) ->
    {killed,St};
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
	    case X < Live orelse member(R, Ss) of
		true -> {used,St};
		false -> {killed,St}
	    end;
	{y,_} ->
	    case member(R, Ss) of
		true -> {used,St};
		false ->
		    if
			R =:= Dst -> {killed,St};
			true -> check_liveness(R, Is, St)
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
	{y,_} -> check_liveness(R, Is, St)
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
		    check_liveness(R, Is, St);
		true ->
		    %% We must make sure we don't check beyond this
		    %% instruction or we will fall through into random
		    %% unrelated code and get stuck in a loop.
		    {killed,St}
	    end
    end;
check_liveness(R, [{call_fun,Live}|Is], St) ->
    case R of
	{x,X} when X =< Live -> {used,St};
	{x,_} -> {killed,St};
	{y,_} -> check_liveness(R, Is, St)
    end;
check_liveness(R, [{apply,Args}|Is], St) ->
    case R of
	{x,X} when X < Args+2 -> {used,St};
	{x,_} -> {killed,St};
	{y,_} -> check_liveness(R, Is, St)
    end;
check_liveness(R, [{bif,Op,{f,Fail},Ss,D}|Is], St0) ->
    case check_liveness_fail(R, Op, Ss, Fail, St0) of
	{killed,St} = Killed ->
	    case member(R, Ss) of
		true -> {used,St};
		false when R =:= D -> Killed;
		false -> check_liveness(R, Is, St)
	    end;
	Other ->
	    Other
    end;
check_liveness(R, [{gc_bif,Op,{f,Fail},Live,Ss,D}|Is], St0) ->
    case R of
	{x,X} when X >= Live ->
	    {killed,St0};
	{x,_} ->
	    {used,St0};
	_ ->
	    case check_liveness_fail(R, Op, Ss, Fail, St0) of
		{killed,St}=Killed ->
		    case member(R, Ss) of
			true -> {used,St};
			false when R =:= D -> Killed;
			false -> check_liveness(R, Is, St)
		    end;
		Other ->
		    Other
	    end
    end;
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
	_ -> check_liveness(R, Is, St)
    end;
check_liveness({x,_}=R, [{'catch',_,_}|Is], St) ->
    %% All x registers will be killed if an exception occurs.
    %% Therefore we only need to check the liveness for the
    %% instructions following the catch instruction.
    check_liveness(R, Is, St);
check_liveness({x,_}=R, [{'try',_,_}|Is], St) ->
    %% All x registers will be killed if an exception occurs.
    %% Therefore we only need to check the liveness for the
    %% instructions inside the 'try' block.
    check_liveness(R, Is, St);
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
check_liveness(R, [{put_map,{f,_},_,Src,_D,Live,{list,_}}|_], St0) ->
    case R of
	Src ->
	    {used,St0};
	{x,X} when X < Live ->
	    {used,St0};
	{x,_} ->
	    {killed,St0};
	{y,_} ->
	    %% Conservatively mark it as used.
	    {used,St0}
    end;
check_liveness(R, [{test_heap,N,Live}|Is], St) ->
    I = {block,[{set,[],[],{alloc,Live,{nozero,nostack,N,[]}}}]},
    check_liveness(R, [I|Is], St);
check_liveness(R, [{allocate_zero,N,Live}|Is], St) ->
    I = {block,[{set,[],[],{alloc,Live,{zero,N,0,[]}}}]},
    check_liveness(R, [I|Is], St);
check_liveness(R, [{get_list,S,D1,D2}|Is], St) ->
    I = {block,[{set,[D1,D2],[S],get_list}]},
    check_liveness(R, [I|Is], St);
check_liveness(_R, Is, St) when is_list(Is) ->
    %% Not implemented. Conservatively assume that the register is used.
    {used,St}.
    
check_liveness_everywhere(R, [{f,Lbl}|T], St0) ->
    case check_liveness_at(R, Lbl, St0) of
	{killed,St} -> check_liveness_everywhere(R, T, St);
	{_,_}=Other -> Other
    end;
check_liveness_everywhere(R, [_|T], St) ->
    check_liveness_everywhere(R, T, St);
check_liveness_everywhere(_, [], St) ->
    {killed,St}.

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

check_liveness_ret(R, R, St) -> {used,St};
check_liveness_ret(_, _, St) -> {killed,St}.

check_liveness_fail(_, _, _, 0, St) ->
    {killed,St};
check_liveness_fail(R, Op, Args, Fail, St) ->
    Arity = length(Args),
    case erl_internal:comp_op(Op, Arity) orelse
	erl_internal:new_type_test(Op, Arity) of
	true -> {killed,St};
	false -> check_liveness_at(R, Fail, St)
    end.

%% check_killed_block(Reg, [Instruction], State) -> killed | transparent | used
%%  Finds out how Reg is used in the instruction sequence inside a block.
%%  Returns one of:
%%    killed - Reg is assigned a new value or killed by an allocation instruction
%%    transparent - Reg is neither used nor killed
%%    used - Reg is used or referenced by an allocation instruction.
%%  
%%    (Unknown instructions will cause an exception.)

check_killed_block_fun() ->
    fun(R, Is, St) -> {check_killed_block(R, Is),St} end.

check_killed_block({x,X}, [{set,_,_,{alloc,Live,_}}|_]) ->
    if 
	X >= Live -> killed;
	true -> used
    end;
check_killed_block(R, [{set,Ds,Ss,_Op}|Is]) ->
    case member(R, Ss) of
	true -> used;
	false ->
	    case member(R, Ds) of
		true -> killed;
		false -> check_killed_block(R, Is)
	    end
    end;
check_killed_block(R, [{'%live',_,Regs}|Is]) ->
    case R of
	{x,X} when (Regs bsr X) band 1 =:= 0 -> killed;
	_ -> check_killed_block(R, Is)
    end;
check_killed_block(_, []) -> transparent.

%% check_used_block(Reg, [Instruction], State) -> killed | transparent | used
%%  Finds out how Reg is used in the instruction sequence inside a block.
%%  Returns one of:
%%    killed - Reg is assigned a new value or killed by an allocation instruction
%%    transparent - Reg is neither used nor killed
%%    used - Reg is explicitly used by an instruction
%%
%%  '%live' annotations are not allowed.
%%
%%  (Unknown instructions will cause an exception.)

check_used_block({x,X}=R, [{set,Ds,Ss,{alloc,Live,Op}}|Is], St) ->
    if 
	X >= Live -> {killed,St};
	true -> check_used_block_1(R, Ss, Ds, Op, Is, St)
    end;
check_used_block(R, [{set,Ds,Ss,Op}|Is], St) ->
    check_used_block_1(R, Ss, Ds, Op, Is, St);
check_used_block(_, [], St) -> {transparent,St}.

check_used_block_1(R, Ss, Ds, Op, Is, St0) ->
    case member(R, Ss) of
	true ->
	    {used,St0};
	false ->
	    case is_reg_used_at(R, Op, St0) of
		{true,St} ->
		    {used,St};
		{false,St} ->
		    case member(R, Ds) of
			true -> {killed,St};
			false -> check_used_block(R, Is, St)
		    end
	    end
    end.

is_reg_used_at(R, {gc_bif,_,{f,Lbl}}, St) ->
    is_reg_used_at_1(R, Lbl, St);
is_reg_used_at(R, {bif,_,{f,Lbl}}, St) ->
    is_reg_used_at_1(R, Lbl, St);
is_reg_used_at(_, _, St) ->
    {false,St}.

is_reg_used_at_1(_, 0, St) ->
    {false,St};
is_reg_used_at_1(R, Lbl, St0) ->
    case check_liveness_at(R, Lbl, St0) of
	{killed,St} -> {false,St};
	{used,St} -> {true,St}
    end.

index_labels_1([{label,Lbl}|Is0], Acc) ->
    Is = drop_labels(Is0),
    index_labels_1(Is0, [{Lbl,Is}|Acc]);
index_labels_1([_|Is], Acc) ->
    index_labels_1(Is, Acc);
index_labels_1([], Acc) -> gb_trees:from_orddict(sort(Acc)).

drop_labels([{label,_}|Is]) -> drop_labels(Is);
drop_labels(Is) -> Is.

%% Help functions for combine_heap_needs.

combine_alloc_lists(Al1, Al2) ->
    combine_alloc_lists_1(sort(Al1++Al2)).

combine_alloc_lists_1([{words,W1},{words,W2}|T])
  when is_integer(W1), is_integer(W2) ->
    [{words,W1+W2}|combine_alloc_lists_1(T)];
combine_alloc_lists_1([{floats,F1},{floats,F2}|T])
  when is_integer(F1), is_integer(F2) ->
    [{floats,F1+F2}|combine_alloc_lists_1(T)];
combine_alloc_lists_1([{words,_}=W|T]) ->
    [W|combine_alloc_lists_1(T)];
combine_alloc_lists_1([{floats,_}=F|T]) ->
    [F|combine_alloc_lists_1(T)];
combine_alloc_lists_1([]) -> [].

%% live_opt/4.

%% Bit syntax instructions.
live_opt([{bs_context_to_binary,Src}=I|Is], Regs0, D, Acc) ->
    Regs = x_live([Src], Regs0),
    live_opt(Is, Regs, D, [I|Acc]);
live_opt([{bs_init,Fail,_,none,Ss,Dst}=I|Is], Regs0, D, Acc) ->
    Regs1 = x_live(Ss, x_dead([Dst], Regs0)),
    Regs = live_join_label(Fail, D, Regs1),
    live_opt(Is, Regs, D, [I|Acc]);
live_opt([{bs_init,Fail,Info,Live0,Ss,Dst}|Is], Regs0, D, Acc) ->
    Regs1 = x_dead([Dst], Regs0),
    Live = live_regs(Regs1),
    true = Live =< Live0,	%Assertion.
    Regs2 = live_call(Live),
    Regs3 = x_live(Ss, Regs2),
    Regs = live_join_label(Fail, D, Regs3),
    I = {bs_init,Fail,Info,Live,Ss,Dst},
    live_opt(Is, Regs, D, [I|Acc]);
live_opt([{bs_put,Fail,_,Ss}=I|Is], Regs0, D, Acc) ->
    Regs1 = x_live(Ss, Regs0),
    Regs = live_join_label(Fail, D, Regs1),
    live_opt(Is, Regs, D, [I|Acc]);
live_opt([{bs_restore2,Src,_}=I|Is], Regs0, D, Acc) ->
    Regs = x_live([Src], Regs0),
    live_opt(Is, Regs, D, [I|Acc]);
live_opt([{bs_save2,Src,_}=I|Is], Regs0, D, Acc) ->
    Regs = x_live([Src], Regs0),
    live_opt(Is, Regs, D, [I|Acc]);
live_opt([{test,bs_start_match2,Fail,Live,[Src,_],_}=I|Is], _, D, Acc) ->
    Regs0 = live_call(Live),
    Regs1 = x_live([Src], Regs0),
    Regs = live_join_label(Fail, D, Regs1),
    live_opt(Is, Regs, D, [I|Acc]);

%% Other instructions.
live_opt([{block,Bl0}|Is], Regs0, D, Acc) ->
    Live0 = {'%live',live_regs(Regs0),Regs0},
    {Bl,Regs} = live_opt_block(reverse(Bl0), Regs0, D, [Live0]),
    Live = {'%live',live_regs(Regs),Regs},
    live_opt(Is, Regs, D, [{block,[Live|Bl]}|Acc]);
live_opt([{label,L}=I|Is], Regs, D0, Acc) ->
    D = gb_trees:insert(L, Regs, D0),
    live_opt(Is, Regs, D, [I|Acc]);
live_opt([{jump,{f,L}}=I|Is], _, D, Acc) ->
    Regs = gb_trees:get(L, D),
    live_opt(Is, Regs, D, [I|Acc]);
live_opt([return=I|Is], _, D, Acc) ->
    live_opt(Is, 1, D, [I|Acc]);
live_opt([{catch_end,_}=I|Is], _, D, Acc) ->
    live_opt(Is, live_call(1), D, [I|Acc]);
live_opt([{badmatch,Src}=I|Is], _, D, Acc) ->
    Regs = x_live([Src], 0),
    live_opt(Is, Regs, D, [I|Acc]);
live_opt([{case_end,Src}=I|Is], _, D, Acc) ->
    Regs = x_live([Src], 0),
    live_opt(Is, Regs, D, [I|Acc]);
live_opt([{try_case_end,Src}=I|Is], _, D, Acc) ->
    Regs = x_live([Src], 0),
    live_opt(Is, Regs, D, [I|Acc]);
live_opt([if_end=I|Is], _, D, Acc) ->
    Regs = 0,
    live_opt(Is, Regs, D, [I|Acc]);
live_opt([{call,Arity,_}=I|Is], _, D, Acc) ->
    live_opt(Is, live_call(Arity), D, [I|Acc]);
live_opt([{call_ext,Arity,_}=I|Is], _, D, Acc) ->
    live_opt(Is, live_call(Arity), D, [I|Acc]);
live_opt([{call_fun,Arity}=I|Is], _, D, Acc) ->
    live_opt(Is, live_call(Arity+1), D, [I|Acc]);
live_opt([{apply,Arity}=I|Is], _, D, Acc) ->
    live_opt(Is, live_call(Arity+2), D, [I|Acc]);
live_opt([{make_fun2,_,_,_,Arity}=I|Is], _, D, Acc) ->
    live_opt(Is, live_call(Arity), D, [I|Acc]);
live_opt([{test,_,Fail,Ss}=I|Is], Regs0, D, Acc) ->
    Regs1 = x_live(Ss, Regs0),
    Regs = live_join_label(Fail, D, Regs1),
    live_opt(Is, Regs, D, [I|Acc]);
live_opt([{test,_,Fail,Live,Ss,_}=I|Is], _, D, Acc) ->
    Regs0 = live_call(Live),
    Regs1 = x_live(Ss, Regs0),
    Regs = live_join_label(Fail, D, Regs1),
    live_opt(Is, Regs, D, [I|Acc]);
live_opt([{select,_,Src,Fail,List}=I|Is], Regs0, D, Acc) ->
    Regs1 = x_live([Src], Regs0),
    Regs = live_join_labels([Fail|List], D, Regs1),
    live_opt(Is, Regs, D, [I|Acc]);
live_opt([{try_case,_}=I|Is], _, D, Acc) ->
    live_opt(Is, live_call(1), D, [I|Acc]);
live_opt([{loop_rec,_Fail,_Dst}=I|Is], _, D, Acc) ->
    live_opt(Is, 0, D, [I|Acc]);
live_opt([timeout=I|Is], _, D, Acc) ->
    live_opt(Is, 0, D, [I|Acc]);
live_opt([{wait,_}=I|Is], _, D, Acc) ->
    live_opt(Is, 0, D, [I|Acc]);

%% Transparent instructions - they neither use nor modify x registers.
live_opt([{deallocate,_}=I|Is], Regs, D, Acc) ->
    live_opt(Is, Regs, D, [I|Acc]);
live_opt([{kill,_}=I|Is], Regs, D, Acc) ->
    live_opt(Is, Regs, D, [I|Acc]);
live_opt([{try_end,_}=I|Is], Regs, D, Acc) ->
    live_opt(Is, Regs, D, [I|Acc]);
live_opt([{loop_rec_end,_}=I|Is], Regs, D, Acc) ->
    live_opt(Is, Regs, D, [I|Acc]);
live_opt([{wait_timeout,_,nil}=I|Is], Regs, D, Acc) ->
    live_opt(Is, Regs, D, [I|Acc]);
live_opt([{wait_timeout,_,{Tag,_}}=I|Is], Regs, D, Acc) when Tag =/= x ->
    live_opt(Is, Regs, D, [I|Acc]);
live_opt([{line,_}=I|Is], Regs, D, Acc) ->
    live_opt(Is, Regs, D, [I|Acc]);

%% The following instructions can occur if the "compilation" has been
%% started from a .S file using the 'from_asm' option.
live_opt([{trim,_,_}=I|Is], Regs, D, Acc) ->
    live_opt(Is, Regs, D, [I|Acc]);
live_opt([{'%',_}=I|Is], Regs, D, Acc) ->
    live_opt(Is, Regs, D, [I|Acc]);
live_opt([{recv_set,_}=I|Is], Regs, D, Acc) ->
    live_opt(Is, Regs, D, [I|Acc]);
live_opt([{recv_mark,_}=I|Is], Regs, D, Acc) ->
    live_opt(Is, Regs, D, [I|Acc]);

live_opt([], _, _, Acc) -> Acc.

live_opt_block([{set,Ds,Ss,Op}=I0|Is], Regs0, D, Acc) ->
    Regs1 = x_live(Ss, x_dead(Ds, Regs0)),
    {I,Regs} = case Op of
		   {alloc,Live0,Alloc} ->
		       %% The life-time analysis used by the code generator
		       %% is sometimes too conservative, so it may be
		       %% possible to lower the number of live registers
		       %% based on the exact liveness information.
		       %% The main benefit is that more optimizations that
		       %% depend on liveness information (such as the
		       %% beam_bool and beam_dead passes) may be applied.
		       Live = live_regs(Regs1),
		       true = Live =< Live0,	%Assertion.
		       I1 = {set,Ds,Ss,{alloc,Live,Alloc}},
		       {I1,live_call(Live)};
		   _ ->
		       {I0,Regs1}
	       end,
    case Ds of
	[{x,X}] ->
	    case (not is_live(X, Regs0)) andalso Op =:= move of
		true ->
		    live_opt_block(Is, Regs0, D, Acc);
		false ->
		    live_opt_block(Is, Regs, D, [I|Acc])
	    end;
	_ ->
	    live_opt_block(Is, Regs, D, [I|Acc])
    end;
live_opt_block([], Regs, _, Acc) -> {Acc,Regs}.

live_join_labels([{f,L}|T], D, Regs0) when L =/= 0 ->
    Regs = gb_trees:get(L, D) bor Regs0,
    live_join_labels(T, D, Regs);
live_join_labels([_|T], D, Regs) ->
    live_join_labels(T, D, Regs);
live_join_labels([], _, Regs) -> Regs.

live_join_label({f,0}, _, Regs) ->
    Regs;
live_join_label({f,L}, D, Regs) ->
    gb_trees:get(L, D) bor Regs.

live_call(Live) -> (1 bsl Live) - 1.

live_regs(Regs) ->
    live_regs_1(0, Regs).

live_regs_1(N, 0) -> N;
live_regs_1(N, Regs) -> live_regs_1(N+1, Regs bsr 1).

x_dead([{x,N}|Rs], Regs) -> x_dead(Rs, Regs band (bnot (1 bsl N)));
x_dead([_|Rs], Regs) -> x_dead(Rs, Regs);
x_dead([], Regs) -> Regs.

x_live([{x,N}|Rs], Regs) -> x_live(Rs, Regs bor (1 bsl N));
x_live([_|Rs], Regs) -> x_live(Rs, Regs);
x_live([], Regs) -> Regs.

is_live(X, Regs) -> ((Regs bsr X) band 1) =:= 1.

split_even([], Ss, Ds) ->
    {reverse(Ss),reverse(Ds)};
split_even([S,D|Rs], Ss, Ds) ->
    split_even(Rs, [S|Ss], [D|Ds]).
