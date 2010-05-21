%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2010. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
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
	 live_opt/1,delete_live_annos/1,combine_heap_needs/2]).

-import(lists, [member/2,sort/1,reverse/1]).

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
    St = #live{bl=fun check_killed_block/2,lbl=D,res=gb_trees:empty()},
    case check_liveness(R, Is, St) of
	{killed,_} -> true;
	{used,_} -> false;
	{unknown,_} -> false
    end.

%% is_killed_at(Reg, Lbl, State) -> true|false
%%  Determine whether Reg is killed at label Lbl.

is_killed_at(R, Lbl, D) when is_integer(Lbl) ->
    St0 = #live{bl=fun check_killed_block/2,lbl=D,res=gb_trees:empty()},
    case check_liveness_at(R, Lbl, St0) of
	{killed,_} -> true;
	{used,_} -> false;
	{unknown,_} -> false
    end.

%% is_not_used(Register, [Instruction], State) -> true|false
%%  Determine whether a register is never used in the instruction sequence
%%  (it could still be referenced by an allocate instruction, meaning that
%%  it MUST be initialized, but that its value does not matter).
%%    The state is used to allow us to determine the usage state
%%  across branches.

is_not_used(R, Is, D) ->
    St = #live{bl=fun check_used_block/2,lbl=D,res=gb_trees:empty()},
    case check_liveness(R, Is, St) of
	{killed,_} -> true;
	{used,_} -> false;
	{unknown,_} -> false
    end.

%% is_not_used(Register, [Instruction], State) -> true|false
%%  Determine whether a register is never used in the instruction sequence
%%  (it could still be referenced by an allocate instruction, meaning that
%%  it MUST be initialized, but that its value does not matter).
%%    The state is used to allow us to determine the usage state
%%  across branches.

is_not_used_at(R, Lbl, D) ->
    St = #live{bl=fun check_used_block/2,lbl=D,res=gb_trees:empty()},
    case check_liveness_at(R, Lbl, St) of
	{killed,_} -> true;
	{used,_} -> false;
	{unknown,_} -> false
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
    Is = lists:dropwhile(fun({label,_}) -> true;
			    (_) -> false end, Is0),
    gb_trees:enter(Lbl, Is, Acc).


%% code_at(Label, State) -> [I].
%%  Retrieve the code at the given label.

code_at(L, Ll) ->
    case gb_trees:lookup(L, Ll) of
	{value,Code} -> Code;
	none -> none
    end.

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
bif_to_test(is_number,   [_]=Ops, Fail) -> {test,is_number,Fail,Ops};
bif_to_test(is_pid,      [_]=Ops, Fail) -> {test,is_pid,Fail,Ops};
bif_to_test(is_port,     [_]=Ops, Fail) -> {test,is_port,Fail,Ops};
bif_to_test(is_reference, [_]=Ops, Fail) -> {test,is_reference,Fail,Ops};
bif_to_test(is_tuple,    [_]=Ops, Fail)     -> {test,is_tuple,Fail,Ops};
bif_to_test('=<', [A,B], Fail) -> {test,is_ge,Fail,[B,A]};
bif_to_test('>', [A,B], Fail) -> {test,is_lt,Fail,[B,A]};
bif_to_test('<', [_,_]=Ops, Fail) -> {test,is_lt,Fail,Ops};
bif_to_test('>=', [_,_]=Ops, Fail) -> {test,is_ge,Fail,Ops};
bif_to_test('==', [A,[]], Fail) -> {test,is_nil,Fail,[A]};
bif_to_test('==', [_,_]=Ops, Fail) -> {test,is_eq,Fail,Ops};
bif_to_test('/=', [_,_]=Ops, Fail) -> {test,is_ne,Fail,Ops};
bif_to_test('=:=', [A,[]], Fail) -> {test,is_nil,Fail,[A]};
bif_to_test('=:=', [_,_]=Ops, Fail) -> {test,is_eq_exact,Fail,Ops};
bif_to_test('=/=', [_,_]=Ops, Fail) -> {test,is_ne_exact,Fail,Ops};
bif_to_test(is_record, [_,_,_]=Ops, Fail) -> {test,is_record,Fail,Ops}.


%% is_pure_test({test,Op,Fail,Ops}) -> true|false.
%%  Return 'true' if the test instruction does not modify any
%%  registers and/or bit syntax matching state, nor modifies
%%  any bit syntax matching state.
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
is_pure_test({test,Op,_,Ops}) -> 
    erl_internal:new_type_test(Op, length(Ops)).

    
%% live_opt([Instruction]) -> [Instruction].
%%  Go through the instruction sequence in reverse execution
%%  order, keep track of liveness and remove 'move' instructions
%%  whose destination is a register that will not be used.
%%  Also insert {'%live',Live} annotations at the beginning
%%  and end of each block.
%%
live_opt([{label,Fail}=I1,
	  {func_info,_,_,Live}=I2|Is]) ->
    D = gb_trees:insert(Fail, live_call(Live), gb_trees:empty()),
    [I1,I2|live_opt(reverse(Is), 0, D, [])].


%% delete_live_annos([Instruction]) -> [Instruction].
%%  Delete all live annotations.
%%
delete_live_annos([{block,Bl0}|Is]) ->
    case delete_live_annos(Bl0) of
	[] -> delete_live_annos(Is);
	[_|_]=Bl -> [{block,Bl}|delete_live_annos(Is)]
    end;
delete_live_annos([{'%live',_}|Is]) ->
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

%%%
%%% Local functions.
%%%


%% check_liveness(Reg, [Instruction], {State,BlockCheckFun}) ->
%%                      {killed | used | unknown,UpdateState}
%%  Finds out how Reg is used in the instruction sequence. Returns one of:
%%    killed - Reg is assigned a new value or killed by an allocation instruction
%%    used - Reg is used (or possibly referenced by an allocation instruction)
%%    unknown - not possible to determine (perhaps because of an instruction
%%              that we don't recognize)

check_liveness(R, [{set,_,_,_}=I|_], St) ->
    erlang:error(only_allowed_in_blocks, [R,I,St]);
check_liveness(R, [{block,Blk}|Is], #live{bl=BlockCheck}=St) ->
    case BlockCheck(R, Blk) of
	transparent -> check_liveness(R, Is, St);
	Other when is_atom(Other) -> {Other,St}
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
check_liveness(R, [{test,_,{f,Fail},Live,Ss,_}|Is], St0) ->
    case R of
	{x,X} ->
	    case X < Live orelse member(R, Ss) of
		true -> {used,St0};
		false -> check_liveness_at(R, Fail, St0)
	    end;
	{y,_} ->
	    case check_liveness_at(R, Fail, St0) of
		{killed,St} -> check_liveness(R, Is, St);
		{_,_}=Other -> Other
	    end
    end;
check_liveness(R, [{select_val,R,_,_}|_], St) ->
    {used,St};
check_liveness(R, [{select_val,_,Fail,{list,Branches}}|_], St) ->
    check_liveness_everywhere(R, [Fail|Branches], St);
check_liveness(R, [{select_tuple_arity,R,_,_}|_], St) ->
    {used,St};
check_liveness(R, [{select_tuple_arity,_,Fail,{list,Branches}}|_], St) ->
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
check_liveness(R, [bs_init_writable|Is], St) ->
    if
	R =:= {x,0} -> {used,St};
	true -> check_liveness(R, Is, St)
    end;
check_liveness(R, [{bs_private_append,_,Bits,_,Bin,_,Dst}|Is], St) ->
    case R of
	Bits -> {used,St};
	Bin -> {used,St};
	Dst -> {killed,St};
	_ -> check_liveness(R, Is, St)
    end;
check_liveness(R, [{bs_append,_,Bits,_,_,_,Bin,_,Dst}|Is], St) ->
    case R of
	Bits -> {used,St};
	Bin -> {used,St};
	Dst -> {killed,St};
	_ -> check_liveness(R, Is, St)
    end;
check_liveness(R, [{bs_init2,_,_,_,_,_,Dst}|Is], St) ->
    if
	R =:= Dst -> {killed,St};
	true -> check_liveness(R, Is, St)
    end;
check_liveness(R, [{bs_init_bits,_,_,_,_,_,Dst}|Is], St) ->
    if
	R =:= Dst -> {killed,St};
	true -> check_liveness(R, Is, St)
    end;
check_liveness(R, [{bs_put_string,_,_}|Is], St) ->
    check_liveness(R, Is, St);
check_liveness(R, [{deallocate,_}|Is], St) ->
    case R of
	{y,_} -> {killed,St};
	_ -> check_liveness(R, Is, St)
    end;
check_liveness(R, [return|_], St) ->
    check_liveness_live_ret(R, 1, St);
check_liveness(R, [{call_last,Live,_,_}|_], St) ->
    check_liveness_live_ret(R, Live, St);
check_liveness(R, [{call_only,Live,_}|_], St) ->
    check_liveness_live_ret(R, Live, St);
check_liveness(R, [{call_ext_last,Live,_,_}|_], St) ->
    check_liveness_live_ret(R, Live, St);
check_liveness(R, [{call_ext_only,Live,_}|_], St) ->
    check_liveness_live_ret(R, Live, St);
check_liveness(R, [{call,Live,_}|Is], St) ->
    case R of
	{x,X} when X < Live -> {used,St};
	{x,_} -> {killed,St};
	{y,_} -> check_liveness(R, Is, St)
    end;
check_liveness(R, [{call_ext,Live,Func}|Is], St) ->
    case R of
	{x,X} when X < Live ->
	    {used,St};
	{x,_} ->
	    {killed,St};
	{y,_} ->
	    {extfunc,Mod,Name,Arity} = Func,
	    case erl_bifs:is_exit_bif(Mod, Name, Arity) of
		false ->
		    check_liveness(R, Is, St);
		true ->
		    %% We must make sure we don't check beyond this instruction
		    %% or we will fall through into random unrelated code and
		    %% get stuck in a loop.
		    %%
		    %% We don't want to overwrite a 'catch', so consider this
		    %% register in use.
		    %% 
		    {used,St}
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
check_liveness(R, [{apply_last,Args,_}|_], St) ->
    check_liveness_live_ret(R, Args+2, St);
check_liveness(R, [send|Is], St) ->
    case R of
	{x,X} when X < 2 -> {used,St};
	{x,_} -> {killed,St};
	{y,_} -> check_liveness(R, Is, St)
    end;
check_liveness({x,R}, [{'%live',Live}|Is], St) ->
    if
	R < Live -> check_liveness(R, Is, St);
	true -> {killed,St}
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
check_liveness(R, [{gc_bif,Op,{f,Fail},_,Ss,D}|Is], St0) ->
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
check_liveness(R, [{bs_add,{f,0},Ss,D}|Is], St) ->
    case member(R, Ss) of
	true -> {used,St};
	false when R =:= D -> {killed,St};
	false -> check_liveness(R, Is, St)
    end;
check_liveness(R, [{bs_put_binary,{f,0},Sz,_,_,Src}|Is], St) ->
    case member(R, [Sz,Src]) of
	true -> {used,St};
	false -> check_liveness(R, Is, St)
    end;
check_liveness(R, [{bs_put_integer,{f,0},Sz,_,_,Src}|Is], St) ->
    case member(R, [Sz,Src]) of
	true -> {used,St};
	false -> check_liveness(R, Is, St)
    end;
check_liveness(R, [{bs_put_float,{f,0},Sz,_,_,Src}|Is], St) ->
    case member(R, [Sz,Src]) of
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
check_liveness(R, [{try_end,Y}|Is], St) ->
    case R of
	Y -> {killed,St};
	_ -> check_liveness(R, Is, St)
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
check_liveness(R, [{loop_rec,{f,_},{x,0}}|Is], St) ->
    case R of
	{x,_} -> {killed,St};
	_ -> check_liveness(R, Is, St)
    end;
check_liveness(R, [{loop_rec_end,{f,Fail}}|_], St) ->
    check_liveness_at(R, Fail, St);
check_liveness(_R, Is, St) when is_list(Is) ->
%%     case Is of
%% 	[I|_] ->
%% 	    io:format("~p ~p\n", [_R,I]);
%% 	_ -> ok
%%     end,
    {unknown,St}.
    
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
			   none -> {unknown,St0}
		       end,
	    {Res,St#live{res=gb_trees:insert(Lbl, Res, St#live.res)}}
    end.

check_liveness_ret(R, R, St) -> {used,St};
check_liveness_ret(_, _, St) -> {killed,St}.

check_liveness_live_ret({x,R}, Live, St) ->
    if
	R < Live -> {used,St};
	true -> {killed,St}
    end;
check_liveness_live_ret({y,_}, _, St) ->
    {killed,St}.

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
check_killed_block(R, [{'%live',Live}|Is]) ->
    case R of
	{x,X} when X >= Live -> killed;
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
%%    (Unknown instructions will cause an exception.)

check_used_block({x,X}=R, [{set,_,_,{alloc,Live,_}}|Is]) ->
    if 
	X >= Live -> killed;
	true -> check_used_block(R, Is)
    end;
check_used_block(R, [{set,Ds,Ss,_Op}|Is]) ->
    case member(R, Ss) of
	true -> used;
	false ->
	    case member(R, Ds) of
		true -> killed;
		false -> check_used_block(R, Is)
	    end
    end;
check_used_block(R, [{'%live',Live}|Is]) ->
    case R of
	{x,X} when X >= Live -> killed;
	_ -> check_used_block(R, Is)
    end;
check_used_block(_, []) -> transparent.

index_labels_1([{label,Lbl}|Is0], Acc) ->
    Is = lists:dropwhile(fun({label,_}) -> true;
			    (_) -> false end, Is0),
    index_labels_1(Is0, [{Lbl,Is}|Acc]);
index_labels_1([_|Is], Acc) ->
    index_labels_1(Is, Acc);
index_labels_1([], Acc) -> gb_trees:from_orddict(sort(Acc)).

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
live_opt([{bs_add,Fail,[Src1,Src2,_],Dst}=I|Is], Regs0, D, Acc) ->
    Regs1 = x_live([Src1,Src2], x_dead([Dst], Regs0)),
    Regs = live_join_label(Fail, D, Regs1),
    live_opt(Is, Regs, D, [I|Acc]);
live_opt([{bs_init2,Fail,_,_,Live,_,_}=I|Is], _, D, Acc) ->
    Regs1 = live_call(Live),
    Regs = live_join_label(Fail, D, Regs1),
    live_opt(Is, Regs, D, [I|Acc]);
live_opt([{bs_init_bits,Fail,Src1,_,Live,_,Src2}=I|Is], _, D, Acc) ->
    Regs1 = live_call(Live),
    Regs2 = x_live([Src1,Src2], Regs1),
    Regs = live_join_label(Fail, D, Regs2),
    live_opt(Is, Regs, D, [I|Acc]);
live_opt([{bs_append,Fail,Src1,_,Live,_,Src2,_,Dst}=I|Is], _Regs0, D, Acc) ->
    Regs1 = x_dead([Dst], x_live([Src1,Src2], live_call(Live))),
    Regs = live_join_label(Fail, D, Regs1),
    live_opt(Is, Regs, D, [I|Acc]);
live_opt([{bs_private_append,Fail,Src1,_,Src2,_,Dst}=I|Is], Regs0, D, Acc) ->
    Regs1 = x_live([Src1,Src2], x_dead([Dst], Regs0)),
    Regs = live_join_label(Fail, D, Regs1),
    live_opt(Is, Regs, D, [I|Acc]);
live_opt([{bs_put_binary,Fail,Src1,_,_,Src2}=I|Is], Regs0, D, Acc) ->
    Regs1 = x_live([Src1,Src2], Regs0),
    Regs = live_join_label(Fail, D, Regs1),
    live_opt(Is, Regs, D, [I|Acc]);
live_opt([{bs_put_float,Fail,Src1,_,_,Src2}=I|Is], Regs0, D, Acc) ->
    Regs1 = x_live([Src1,Src2], Regs0),
    Regs = live_join_label(Fail, D, Regs1),
    live_opt(Is, Regs, D, [I|Acc]);
live_opt([{bs_put_integer,Fail,Src1,_,_,Src2}=I|Is], Regs0, D, Acc) ->
    Regs1 = x_live([Src1,Src2], Regs0),
    Regs = live_join_label(Fail, D, Regs1),
    live_opt(Is, Regs, D, [I|Acc]);
live_opt([{bs_put_utf8,Fail,_,Src}=I|Is], Regs0, D, Acc) ->
    Regs1 = x_live([Src], Regs0),
    Regs = live_join_label(Fail, D, Regs1),
    live_opt(Is, Regs, D, [I|Acc]);
live_opt([{bs_put_utf16,Fail,_,Src}=I|Is], Regs0, D, Acc) ->
    Regs1 = x_live([Src], Regs0),
    Regs = live_join_label(Fail, D, Regs1),
    live_opt(Is, Regs, D, [I|Acc]);
live_opt([{bs_put_utf32,Fail,_,Src}=I|Is], Regs0, D, Acc) ->
    Regs1 = x_live([Src], Regs0),
    Regs = live_join_label(Fail, D, Regs1),
    live_opt(Is, Regs, D, [I|Acc]);
live_opt([{bs_restore2,Src,_}=I|Is], Regs0, D, Acc) ->
    Regs = x_live([Src], Regs0),
    live_opt(Is, Regs, D, [I|Acc]);
live_opt([{bs_save2,Src,_}=I|Is], Regs0, D, Acc) ->
    Regs = x_live([Src], Regs0),
    live_opt(Is, Regs, D, [I|Acc]);
live_opt([{bs_utf8_size,Fail,Src,Dst}=I|Is], Regs0, D, Acc) ->
    Regs1 = x_live([Src], x_dead([Dst], Regs0)),
    Regs = live_join_label(Fail, D, Regs1),
    live_opt(Is, Regs, D, [I|Acc]);
live_opt([{bs_utf16_size,Fail,Src,Dst}=I|Is], Regs0, D, Acc) ->
    Regs1 = x_live([Src], x_dead([Dst], Regs0)),
    Regs = live_join_label(Fail, D, Regs1),
    live_opt(Is, Regs, D, [I|Acc]);
live_opt([{test,bs_start_match2,Fail,Live,[Src,_],_}=I|Is], _, D, Acc) ->
    Regs0 = live_call(Live),
    Regs1 = x_live([Src], Regs0),
    Regs = live_join_label(Fail, D, Regs1),
    live_opt(Is, Regs, D, [I|Acc]);

%% Other instructions.
live_opt([{block,Bl0}|Is], Regs0, D, Acc) ->
    Live0 = {'%live',live_regs(Regs0)},
    {Bl,Regs} = live_opt_block(reverse(Bl0), Regs0, D, [Live0]),
    Live = {'%live',live_regs(Regs)},
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
live_opt([if_end=I|Is], _, D, Acc) ->
    Regs = 0,
    live_opt(Is, Regs, D, [I|Acc]);
live_opt([bs_init_writable=I|Is], _, D, Acc) ->
    live_opt(Is, live_call(1), D, [I|Acc]);
live_opt([{call,Arity,_}=I|Is], _, D, Acc) ->
    live_opt(Is, live_call(Arity), D, [I|Acc]);
live_opt([{call_ext,Arity,_}=I|Is], _, D, Acc) ->
    live_opt(Is, live_call(Arity), D, [I|Acc]);
live_opt([{call_fun,Arity}=I|Is], _, D, Acc) ->
    live_opt(Is, live_call(Arity+1), D, [I|Acc]);
live_opt([{call_last,Arity,_,_}=I|Is], _, D, Acc) ->
    live_opt(Is, live_call(Arity), D, [I|Acc]);
live_opt([{call_ext_last,Arity,_,_}=I|Is], _, D, Acc) ->
    live_opt(Is, live_call(Arity), D, [I|Acc]);
live_opt([{apply,Arity}=I|Is], _, D, Acc) ->
    live_opt(Is, live_call(Arity+2), D, [I|Acc]);
live_opt([{apply_last,Arity,_}=I|Is], _, D, Acc) ->
    live_opt(Is, live_call(Arity+2), D, [I|Acc]);
live_opt([{call_only,Arity,_}=I|Is], _, D, Acc) ->
    live_opt(Is, live_call(Arity), D, [I|Acc]);
live_opt([{call_ext_only,Arity,_}=I|Is], _, D, Acc) ->
    live_opt(Is, live_call(Arity), D, [I|Acc]);
live_opt([{make_fun2,_,_,_,Arity}=I|Is], _, D, Acc) ->
    live_opt(Is, live_call(Arity), D, [I|Acc]);
live_opt([send=I|Is], _, D, Acc) ->
    live_opt(Is, live_call(2), D, [I|Acc]);
live_opt([{test,_,Fail,Ss}=I|Is], Regs0, D, Acc) ->
    Regs1 = x_live(Ss, Regs0),
    Regs = live_join_label(Fail, D, Regs1),
    live_opt(Is, Regs, D, [I|Acc]);
live_opt([{test,_,Fail,Live,Ss,_}=I|Is], _, D, Acc) ->
    Regs0 = live_call(Live),
    Regs1 = x_live(Ss, Regs0),
    Regs = live_join_label(Fail, D, Regs1),
    live_opt(Is, Regs, D, [I|Acc]);
live_opt([{select_val,Src,Fail,{list,List}}=I|Is], Regs0, D, Acc) ->
    Regs1 = x_live([Src], Regs0),
    Regs = live_join_labels([Fail|List], D, Regs1),
    live_opt(Is, Regs, D, [I|Acc]);
live_opt([{select_tuple_arity,Src,Fail,{list,List}}=I|Is], Regs0, D, Acc) ->
    Regs1 = x_live([Src], Regs0),
    Regs = live_join_labels([Fail|List], D, Regs1),
    live_opt(Is, Regs, D, [I|Acc]);
live_opt([{'try',_,Fail}=I|Is], Regs0, D, Acc) ->
    Regs = live_join_label(Fail, D, Regs0),
    live_opt(Is, Regs, D, [I|Acc]);
live_opt([{try_case,_}=I|Is], _, D, Acc) ->
    live_opt(Is, live_call(1), D, [I|Acc]);
live_opt([{loop_rec,_Fail,_Dst}=I|Is], _, D, Acc) ->
    live_opt(Is, 0, D, [I|Acc]);
live_opt([timeout=I|Is], _, D, Acc) ->
    live_opt(Is, 0, D, [I|Acc]);

%% Transparent instructions - they neither use nor modify x registers.
live_opt([{bs_put_string,_,_}=I|Is], Regs, D, Acc) ->
    live_opt(Is, Regs, D, [I|Acc]);
live_opt([{deallocate,_}=I|Is], Regs, D, Acc) ->
    live_opt(Is, Regs, D, [I|Acc]);
live_opt([{kill,_}=I|Is], Regs, D, Acc) ->
    live_opt(Is, Regs, D, [I|Acc]);
live_opt([{try_case_end,_}=I|Is], Regs, D, Acc) ->
    live_opt(Is, Regs, D, [I|Acc]);
live_opt([{try_end,_}=I|Is], Regs, D, Acc) ->
    live_opt(Is, Regs, D, [I|Acc]);
live_opt([{loop_rec_end,_}=I|Is], Regs, D, Acc) ->
    live_opt(Is, Regs, D, [I|Acc]);
live_opt([{wait,_}=I|Is], Regs, D, Acc) ->
    live_opt(Is, Regs, D, [I|Acc]);
live_opt([{wait_timeout,_,{Tag,_}}=I|Is], Regs, D, Acc) when Tag =/= x ->
    live_opt(Is, Regs, D, [I|Acc]);

%% The following instructions can occur if the "compilation" has been
%% started from a .S file using the 'asm' option.
live_opt([{trim,_,_}=I|Is], Regs, D, Acc) ->
    live_opt(Is, Regs, D, [I|Acc]);
live_opt([{allocate,_,Live}=I|Is], _, D, Acc) ->
    live_opt(Is, live_call(Live), D, [I|Acc]);
live_opt([{allocate_heap,_,_,Live}=I|Is], _, D, Acc) ->
    live_opt(Is, live_call(Live), D, [I|Acc]);

live_opt([], _, _, Acc) -> Acc.

live_opt_block([{set,[],[],{alloc,Live,_}}=I|Is], _, D, Acc) ->
    live_opt_block(Is, live_call(Live), D, [I|Acc]);
live_opt_block([{set,Ds,Ss,Op}=I|Is], Regs0, D, Acc) ->
    Regs = case Op of
	       {alloc,Live,_} -> live_call(Live);
	       _ -> x_live(Ss, x_dead(Ds, Regs0))
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
