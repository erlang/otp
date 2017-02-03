%% -*- erlang-indent-level: 2 -*-
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2000 by Erik Johansson.  All Rights Reserved 
%% ====================================================================
%%  Filename : 	hipe_icode_heap_test.erl
%%  Module   :	hipe_icode_heap_test
%%  Purpose  :  
%%  Notes    : 
%%  History  :	* 2000-11-07 Erik Johansson (happi@it.uu.se): 
%%               Created.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_icode_heap_test).

-export([cfg/1]).

-define(DO_ASSERT,true).

-include("../main/hipe.hrl").
-include("hipe_icode.hrl").
-include("hipe_icode_primops.hrl").
-include("../flow/cfg.hrl").
-include("../rtl/hipe_literals.hrl").

%-------------------------------------------------------------------------

-spec cfg(#cfg{}) -> #cfg{}.

cfg(CFG) ->
  Icode = hipe_icode_cfg:cfg_to_linear(CFG),
  Code = hipe_icode:icode_code(Icode),
  ActualVmax = hipe_icode:highest_var(Code),
  ActualLmax = hipe_icode:highest_label(Code),
  hipe_gensym:set_label(icode, ActualLmax+1),
  hipe_gensym:set_var(icode, ActualVmax+1),
  EBBs = hipe_icode_ebb:cfg(CFG),
  {EBBcode,_Visited} = ebbs(EBBs, [], CFG),
  NewCode = add_gc_tests(EBBcode),
  NewIcode = hipe_icode:icode_code_update(Icode, NewCode),
  NewCFG = hipe_icode_cfg:linear_to_cfg(NewIcode),
  %% hipe_icode_cfg:pp(NewCFG),
  NewCFG.

ebbs([EBB|EBBs], Visited, CFG) ->
  case hipe_icode_ebb:type(EBB) of
    node ->
      L = hipe_icode_ebb:node_label(EBB),
      case visited(L, Visited) of
	true ->
	  ebbs(EBBs, Visited, CFG);
	false ->
	  EBBCode = hipe_bb:code(hipe_icode_cfg:bb(CFG, L)),
	  case hipe_icode_ebb:node_successors(EBB) of
	    [Succ|Succs]  ->
	      {[SuccCode|More], Visited1} = 
		ebbs([Succ], [L|Visited], CFG),
	      {[OtherCode|MoreOther], Visited2} = 
		ebbs(Succs ++ EBBs, Visited1, CFG),
	      {[[hipe_icode:mk_label(L)|EBBCode] ++ SuccCode|
		More] ++ [OtherCode|MoreOther],
	       Visited2};
	    [] ->
	      {OtherCode, Visited1} = ebbs(EBBs, [L|Visited], CFG),
	      {[[hipe_icode:mk_label(L)|EBBCode] | OtherCode], Visited1}
	  end
      end;
    leaf ->
      ebbs(EBBs, Visited, CFG)
  end;
ebbs([], Visited,_) ->
  {[[]], Visited}.

visited(L, Visited) ->
  lists:member(L, Visited).

add_gc_tests([[]|EBBCodes]) -> add_gc_tests(EBBCodes);
add_gc_tests([EBBCode|EBBCodes]) ->
  case need(EBBCode, 0, []) of
    {Need, RestCode, [Lbl|Code]}  ->
      if Need > 0 ->
	  [Lbl] ++ gc_test(Need) ++ Code ++ add_gc_tests([RestCode|EBBCodes]);
	 true ->
	  [Lbl|Code] ++ add_gc_tests([RestCode|EBBCodes])
      end;
    {0, RestCode, []} ->
      add_gc_tests([RestCode|EBBCodes])
  end;
add_gc_tests([]) -> [].

need([I|Is] , Need, Code) ->
  case split(I) of 
    true -> 
      case I of
	#icode_call{} ->
	  case hipe_icode:call_continuation(I) of
	    [] -> %% Was fallthrough.
	      NewLab = hipe_icode:mk_new_label(),
	      LabName = hipe_icode:label_name(NewLab),
	      NewCall = hipe_icode:call_set_continuation(I,LabName),
	      {Need + need(I), [NewLab|Is], lists:reverse([NewCall|Code])};
	    _ ->
	      {Need + need(I), Is, lists:reverse([I|Code])}
	    end;
	_ ->
	  {Need + need(I), Is, lists:reverse([I|Code])}
      end;
    false ->
      need(Is, Need + need(I), [I|Code])
  end;
need([], Need, Code) ->
  {Need, [], lists:reverse(Code)}.

need(I) ->
  case I of 
    #icode_call{} ->
      primop_need(hipe_icode:call_fun(I), hipe_icode:call_args(I));
    #icode_enter{} ->
      primop_need(hipe_icode:enter_fun(I), hipe_icode:enter_args(I));
    _ -> 
      0
  end.
	      
primop_need(Op, As) ->
  case Op of
    cons ->
      2;
    mktuple ->
      length(As) + 1;
    #mkfun{} ->
      NumFree = length(As),
      ?ERL_FUN_SIZE + NumFree;
    unsafe_tag_float ->
      3;
    _ ->
      0
  end.

gc_test(Need) ->
  L = hipe_icode:mk_new_label(),
  [hipe_icode:mk_primop([], #gc_test{need=Need}, [],
		        hipe_icode:label_name(L),
			hipe_icode:label_name(L)),
   L].

split(I) ->
  case I of
    #icode_call{} -> not known_heap_need(hipe_icode:call_fun(I));
    #icode_enter{} -> not known_heap_need(hipe_icode:enter_fun(I));
    _ -> false
  end.

known_heap_need(Name) ->
  case Name of
    %% Primops
    cons -> true;
    fcheckerror -> true;
    fclearerror -> true;
    fnegate -> true;
    fp_add -> true;
    fp_div -> true;
    fp_mul -> true;
    fp_sub -> true;
    mktuple -> true;
    unsafe_hd -> true;
    unsafe_tag_float -> true;
    unsafe_tl -> true;
    unsafe_untag_float -> true;
    #element{} -> true;
    #unsafe_element{} -> true;
    #unsafe_update_element{}  -> true;

    %% MFAs
    {erlang, element, 2} -> true;
    {erlang, length, 1} -> true;
    {erlang, self, 0} -> true;
    {erlang, size, 1} -> true;

    _ -> false
  end.
