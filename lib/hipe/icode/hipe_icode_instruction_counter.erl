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
%%-------------------------------------------------------------------
%% File    : icode_instruction_counter.erl
%% Author  : Andreas Hasselberg <anha0825@student.uu.se>
%% Purpose : This module counts the number of different instructions
%%           in a function. It is useful when you want to know if
%%           your Icode analysis or specialization is good, bad or
%%           simply unlucky :)
%%
%% Created :  2 Oct 2006 by Andreas Hasselberg <anha0825@student.uu.se>
%%-------------------------------------------------------------------

-module(hipe_icode_instruction_counter).

-export([cfg/3, compare/3]).

-include("../main/hipe.hrl").
-include("hipe_icode.hrl").
-include("../flow/cfg.hrl").

%%-------------------------------------------------------------------
%% A general CFG instruction walktrough
%%-------------------------------------------------------------------

-spec cfg(#cfg{}, mfa(), comp_options()) -> [_].

cfg(Cfg, _IcodeFun, _Options) ->
  Labels = hipe_icode_cfg:labels(Cfg),
  %% Your Info init function goes here
  InitInfo = counter__init_info(),
  Info = lists:foldl(fun (Label, InfoAcc) ->
			 BB = hipe_icode_cfg:bb(Cfg, Label),
			 Code = hipe_bb:code(BB),
			 walktrough_bb(Code, InfoAcc)
		     end, InitInfo,  Labels),
  %% counter__output_info(IcodeFun, Info),
  Info.

walktrough_bb(BB, Info) ->
  lists:foldl(fun (Insn, InfoAcc) ->
		  %% Your analysis function here
		  counter__analys_insn(Insn, InfoAcc)
	      end, Info, BB).

%%-------------------------------------------------------------------
%% The counter specific functions
%%-------------------------------------------------------------------

-spec compare(gb_trees:tree(), gb_trees:tree(), gb_trees:tree()) ->
        gb_trees:tree().

compare(Name, Old, New) ->
  NewList = gb_trees:to_list(New),
  OldList = gb_trees:to_list(Old),
  TempTree = compare_one_way(NewList, Old, added, gb_trees:empty()),
  DiffTree = compare_one_way(OldList, New, removed, TempTree),
  DiffList = gb_trees:to_list(DiffTree),
  if DiffList =:= [] ->
      ok;
     true ->
      io:format("~p: ~p ~n", [Name, DiffList])
  end,
  DiffTree.

compare_one_way(List, Tree, Key, Fold_tree) ->
  lists:foldl(fun({Insn, ListCount}, DiffAcc) when is_integer(ListCount) ->
		  DiffCount = 
		    case gb_trees:lookup(Insn, Tree) of
		      {value, TreeCount} when is_integer(TreeCount) ->
			ListCount - TreeCount;
		      none ->
			ListCount
		    end,
		  if DiffCount > 0 ->
		      gb_trees:insert({Key, Insn}, DiffCount, DiffAcc);
		     true ->
		      DiffAcc
		  end
	      end,
	      Fold_tree,
	      List).
      
counter__init_info() ->
  gb_trees:empty().

counter__analys_insn(Insn, Info) ->
  Key = counter__insn_get_key(Insn),
  counter__increase_key(Key, Info).

counter__insn_get_key(If = #icode_if{}) -> {'if', hipe_icode:if_op(If)};
counter__insn_get_key(Call = #icode_call{}) -> {call, hipe_icode:call_fun(Call)};
counter__insn_get_key(#icode_enter{}) -> enter;
counter__insn_get_key(#icode_return{}) -> return;
counter__insn_get_key(#icode_type{}) -> type;
counter__insn_get_key(#icode_switch_val{}) -> switch_val;
counter__insn_get_key(#icode_switch_tuple_arity{}) -> switch_tuple_arity;
counter__insn_get_key(#icode_goto{}) -> goto;
counter__insn_get_key(#icode_move{}) -> move;
counter__insn_get_key(#icode_phi{}) -> phi;
counter__insn_get_key(#icode_begin_try{}) -> begin_try;
counter__insn_get_key(#icode_end_try{}) -> end_try;
counter__insn_get_key(#icode_begin_handler{}) -> begin_handler;
counter__insn_get_key(#icode_fail{}) -> fail;
counter__insn_get_key(#icode_comment{}) -> comment.

counter__increase_key(Key, Info) ->
  NewCounter = 
    case gb_trees:lookup(Key, Info) of
      {value, Counter} when is_integer(Counter) ->
	Counter + 1;
      none ->
	1
    end,
  gb_trees:enter(Key, NewCounter, Info).

%%counter__output_info(IcodeFun, Info) ->
%%  InfoList = gb_trees:to_list(Info),
%%  io:format("~p instructions : ~p ~n", [IcodeFun, InfoList]).
