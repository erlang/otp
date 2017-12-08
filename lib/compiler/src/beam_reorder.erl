%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2016. All Rights Reserved.
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

-module(beam_reorder).

-export([module/2]).
-import(lists, [member/2,reverse/1]).

-spec module(beam_utils:module_code(), [compile:option()]) ->
                    {'ok',beam_utils:module_code()}.

module({Mod,Exp,Attr,Fs0,Lc}, _Opt) ->
    Fs = [function(F) || F <- Fs0],
    {ok,{Mod,Exp,Attr,Fs,Lc}}.

function({function,Name,Arity,CLabel,Is0}) ->
    try
	Is = reorder(Is0),
	{function,Name,Arity,CLabel,Is}
    catch
        Class:Error:Stack ->
	    io:fwrite("Function: ~w/~w\n", [Name,Arity]),
	    erlang:raise(Class, Error, Stack)
    end.

%% reorder(Instructions0) -> Instructions
%%  Reorder instructions before the beam_block pass, because reordering
%%  will be more cumbersome when the blocks are in place.
%%
%%  Execution of get_tuple_element instructions can be delayed until
%%  they are actually needed. Consider the sequence:
%%
%%      get_tuple_element Tuple Pos Dst
%%      test Test Fail Operands
%%
%%  If Dst is killed at label Fail (and not referenced in Operands),
%%  we can can swap the instructions:
%%
%%      test Test Fail Operands
%%      get_tuple_element Tuple Pos Dst
%%
%%  That can be beneficial in two ways: Firstly, if the branch is taken
%%  we have avoided execution of the get_tuple_element instruction.
%%  Secondly, even if the branch is not taken, subsequent optimization
%%  (opt_blocks/1) may be able to change Dst to the final destination
%%  register and eliminate a 'move' instruction.

reorder(Is) ->
    D = beam_utils:index_labels(Is),
    reorder_1(Is, D, []).

reorder_1([{Op,_,_}=TryCatch|[I|Is]=Is0], D, Acc)
  when Op =:= 'catch'; Op =:= 'try' ->
    %% Don't allow 'try' or 'catch' instructions to split blocks if
    %% it can be avoided.
    case is_safe(I) of
	false ->
	    reorder_1(Is0, D, [TryCatch|Acc]);
	true ->
	    reorder_1([TryCatch|Is], D, [I|Acc])
    end;
reorder_1([{label,L}=I|_], D, Acc) ->
    Is = beam_utils:code_at(L, D),
    reorder_1(Is, D, [I|Acc]);
reorder_1([{test,is_nonempty_list,_,_}=I|Is], D, Acc) ->
    %% The run-time system may combine the is_nonempty_list test with
    %% the following get_list instruction.
    reorder_1(Is, D, [I|Acc]);
reorder_1([{test,_,_,_}=I,
	   {select,_,_,_,_}=S|Is], D, Acc) ->
    %% There is nothing to gain by inserting a get_tuple_element
    %% instruction between the test instruction and the select
    %% instruction.
    reorder_1(Is, D, [S,I|Acc]);
reorder_1([{test,_,{f,_},[Src|_]}=I|Is], D,
	  [{get_tuple_element,Src,_,_}|_]=Acc) ->
    %% We want to avoid code that can confuse beam_validator such as:
    %%   is_tuple Fail Src
    %%   test_arity Fail Src Arity
    %%   is_map Fail Src
    %%   get_tuple_element Src Pos Dst
    %% Therefore, don't reorder the instructions in such cases.
    reorder_1(Is, D, [I|Acc]);
reorder_1([{test,_,{f,L},Ss}=I|Is0], D0,
	  [{get_tuple_element,_,_,El}=G|Acc0]=Acc) ->
    case member(El, Ss) of
	true ->
	    reorder_1(Is0, D0, [I|Acc]);
	false ->
	    case beam_utils:is_killed_at(El, L, D0) of
		true ->
		    Is = [I,G|Is0],
		    reorder_1(Is, D0, Acc0);
		false ->
		    case beam_utils:is_killed(El, Is0, D0) of
			true ->
			    Code0 = beam_utils:code_at(L, D0),
			    Code = [G|Code0],
			    D = beam_utils:index_label(L, Code, D0),
			    Is = [I|Is0],
			    reorder_1(Is, D, Acc0);
			false ->
			    reorder_1(Is0, D0, [I|Acc])
		    end
	    end
    end;
reorder_1([{allocate_zero,N,Live}=I0|Is], D,
	  [{get_tuple_element,{x,Tup},_,{x,Dst}}=G|Acc]=Acc0) ->
    case Tup < Dst andalso Dst+1 =:= Live of
	true ->
	    %% Move allocation instruction upwards past
	    %% get_tuple_element instructions to create more
	    %% opportunities for moving get_tuple_element
	    %% instructions.
	    I = {allocate_zero,N,Dst},
	    reorder_1([I,G|Is], D, Acc);
	false ->
	    reorder_1(Is, D, [I0|Acc0])
    end;
reorder_1([I|Is], D, Acc) ->
    reorder_1(Is, D, [I|Acc]);
reorder_1([], _, Acc) -> reverse(Acc).

%% is_safe(Instruction) -> true|false
%%  Test whether an instruction is safe (cannot cause an exception).

is_safe({kill,_}) -> true;
is_safe({move,_,_}) -> true;
is_safe({put,_}) -> true;
is_safe({put_list,_,_,_}) -> true;
is_safe({put_tuple,_,_}) -> true;
is_safe({test_heap,_,_}) -> true;
is_safe(_) -> false.
