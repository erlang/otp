%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2016. All Rights Reserved.
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
%%-------------------------------------------------------------------
%% File    : hipe_icode_split_arith.erl
%% Author  : Tobias Lindahl <tobiasl@it.uu.se>
%% Description : 
%%
%% Created : 12 Nov 2003 by Tobias Lindahl <tobiasl@it.uu.se>
%%-------------------------------------------------------------------
-module(hipe_icode_split_arith).

-export([cfg/3]).

-include("../main/hipe.hrl").
-include("hipe_icode.hrl").
-include("../flow/cfg.hrl").

-define(MIN_RATIO, 0.005).

%%-------------------------------------------------------------------

-spec cfg(#cfg{}, mfa(), comp_options()) -> #cfg{}.

cfg(Cfg, _MFA, Options) ->
  Icode = hipe_icode_cfg:cfg_to_linear(Cfg),
  case proplists:get_bool(split_arith_unsafe, Options) of
    true -> make_split_unsafe(Icode);
    _ ->
      case preprocess(Icode) of
	{do_not_split, _Ratio} -> 
	  Cfg;
	{split, _Ratio, Icode1} ->
	  NewCfg = split(Icode1),
	  %% hipe_icode_cfg:pp(NewCfg),
	  NewCfg
      end
  end.

check_nofix_const([Arg1|Arg2]) ->
  case hipe_icode:is_const(Arg1) of
    true ->
      Val1 = hipe_tagscheme:fixnum_val(hipe_icode:const_value(Arg1)),
      case hipe_tagscheme:is_fixnum(Val1) of
	true ->
	  check_nofix_const(Arg2);
	false -> {no}
      end;
    false ->
      check_nofix_const(Arg2)
  end;	  
check_nofix_const([]) -> true.

check_const([I|Left]) ->
  case I of
    #icode_call{} ->
      case is_arith(I) of
	true ->
	  Args = hipe_icode:call_args(I),
	  case check_nofix_const(Args) of
	    {no} -> {do_not_split};
	    _ -> check_const(Left)
	  end;
	_ -> check_const(Left)
      end;
    _ -> check_const(Left)
  end;
check_const([]) -> {yes}.

make_split_unsafe(Icode) ->
  LinearCode = hipe_icode:icode_code(Icode),
  NewLinearCode = change_unsafe(LinearCode),
  NewIcode = hipe_icode:icode_code_update(Icode, NewLinearCode),
  hipe_icode_cfg:linear_to_cfg(NewIcode).

change_unsafe([I|Is]) ->
  case I of
    #icode_call{} ->
      case is_arith_extra_unsafe(I) of
	true ->
	  NewOp = arithop_to_extra_unsafe(hipe_icode:call_fun(I)),
	  NewI1 = hipe_icode:call_fun_update(I, NewOp),
	  [NewI1|change_unsafe(Is)];
	false ->
	  [I|change_unsafe(Is)]
      end;
    _ ->
      [I|change_unsafe(Is)]
  end;
change_unsafe([]) -> [].

preprocess(Icode) ->
  LinearCode = hipe_icode:icode_code(Icode),
  case check_const(LinearCode) of
    {do_not_split} -> %%io:format("NO FIXNUM....."),
      {do_not_split, 1.9849}; % Ratio val is ignored
    _ ->
      {NofArith, NofIns, NewLinearCode} = preprocess_code(LinearCode),
      case NofArith / NofIns of
	X when X >= ?MIN_RATIO ->
	  NewIcode = hipe_icode:icode_code_update(Icode, NewLinearCode),
	  {split, X, NewIcode};
	Y ->
	  {do_not_split, Y}
      end
  end.

preprocess_code([H|Code]) ->
  preprocess_code(Code, 0, 0, [H]).

preprocess_code([I|Left], NofArith, NofIns, CodeAcc = [PrevI|_]) ->
  case I of
    #icode_call{} ->
      case is_arith(I) of
	true ->
	  %% Note that we need to put these instructions in a separate
	  %% basic block since we need the ability to fail to these
	  %% instructions, but also fail from them. The basic block
	  %% merger will take care of unnecessary splits.

	  %% If call is an arithmetic operation replace the operation
	  %% with the specified replacement operator.  
	  NewOp = arithop_to_split(hipe_icode:call_fun(I)),
	  NewI = hipe_icode:call_fun_update(I, NewOp),
	  case hipe_icode:is_label(PrevI) of
	    true ->
	      case (Left =:= []) orelse hipe_icode:is_label(hd(Left)) of
		true -> 
		  preprocess_code(Left, NofArith+1, NofIns+1, [NewI|CodeAcc]);
		false ->
		  NewLabel = hipe_icode:mk_new_label(),
		  NewLabelName = hipe_icode:label_name(NewLabel),
		  NewI1 = hipe_icode:call_set_continuation(NewI, NewLabelName),
		  preprocess_code(Left, NofArith+1, NofIns+1, 
				  [NewLabel, NewI1|CodeAcc])
	      end;
	    false ->
	      RevPreCode =
		case hipe_icode:is_branch(PrevI) of
		  true ->
		    [hipe_icode:mk_new_label()];
		  false ->
		    NewLabel1 = hipe_icode:mk_new_label(),
		    NewLabelName1 = hipe_icode:label_name(NewLabel1),
		    [NewLabel1, hipe_icode:mk_goto(NewLabelName1)]
		end,
	      case (Left =:= []) orelse hipe_icode:is_label(hd(Left)) of
		true -> 
		  preprocess_code(Left, NofArith+1, NofIns+1, 
				  [NewI|RevPreCode] ++ CodeAcc);
		false ->
		  NewLabel2 = hipe_icode:mk_new_label(),
		  NewLabelName2 = hipe_icode:label_name(NewLabel2),
		  NewI1 = hipe_icode:call_set_continuation(NewI, NewLabelName2),
		  preprocess_code(Left, NofArith+1, NofIns+1, 
				  [NewLabel2, NewI1|RevPreCode] ++ CodeAcc)
	      end
	  end;
	false ->
	  preprocess_code(Left, NofArith, NofIns + 1, [I|CodeAcc])
      end;
    #icode_label{} ->
      %% Don't count labels as instructions.
      preprocess_code(Left, NofArith, NofIns, [I|CodeAcc]);
    _ ->
      preprocess_code(Left, NofArith, NofIns+1, [I|CodeAcc])
  end;
preprocess_code([], NofArith, NofIns, CodeAcc) ->
  {NofArith, NofIns, lists:reverse(CodeAcc)}.

split(Icode) ->
  LinearCode = hipe_icode:icode_code(Icode),
  %% create a new icode label for each existing icode label
  %% create mappings, NewToOld and OldToNew.
  AllLabels = lists:foldl(fun(I, Acc) ->
			      case hipe_icode:is_label(I) of
				true -> [hipe_icode:label_name(I)|Acc];
				false -> Acc
			      end
			  end, [], LinearCode),
  {OldToNewMap, NewToOldMap}  = new_label_maps(AllLabels),

  %% the call below doubles the number of basic blocks with the new
  %% labels instead of the old.

  NewLinearCode = map_code(LinearCode, OldToNewMap),
  NewIcode = hipe_icode:icode_code_update(Icode, NewLinearCode),
  NewCfg = hipe_icode_cfg:linear_to_cfg(NewIcode),
  NewCfg2 = 
    insert_tests(NewCfg, [gb_trees:get(X, OldToNewMap) || X<-AllLabels], 
		 NewToOldMap, OldToNewMap),
  %% io:format("split(Cfg): Inserting testsL Done\n", []),
  NewCfg2.

map_code(OldCode, LabelMap) ->
  AddedCode = map_code(OldCode, none, LabelMap, []),
  OldCode ++ AddedCode.

map_code([I|Left], ArithFail, LabelMap, Acc) ->
  case I of
    #icode_call{} ->
      case is_arith(I) of
	true ->
	  case hipe_icode:defines(I) of
	    []->  
	      map_code(Left, ArithFail, LabelMap, [redirect(I, LabelMap)|Acc]);
	    _ ->
	      NewOp = split_to_unsafe(I),
	      NewI1 = hipe_icode:call_fun_update(I, NewOp),
	      NewI2 = redirect(NewI1, LabelMap),
	      NewI3 = hipe_icode:call_set_fail_label(NewI2, ArithFail),
	      map_code(Left, ArithFail, LabelMap, [NewI3|Acc])
	  end;
	false ->
	  map_code(Left, ArithFail, LabelMap, [redirect(I, LabelMap)|Acc])
      end;
    #icode_label{} ->
      LabelName = hipe_icode:label_name(I),
      NewLabel = hipe_icode:mk_label(gb_trees:get(LabelName, LabelMap)),
      map_code(Left, LabelName, LabelMap, [NewLabel|Acc]);
    _ -> 
      map_code(Left, ArithFail, LabelMap, [redirect(I, LabelMap)|Acc])
  end;
map_code([], _ArithFail, _LabelMap, Acc) ->
  lists:reverse(Acc).

insert_tests(Cfg, Labels,NewToOldMap, OldToNewMap) ->
  InfoMap = infomap_init(Labels),
  %%io:format("insert_tests/3: Finding testpoints ...\n", []),
  NewInfoMap = find_testpoints(Cfg, Labels, InfoMap),
  %%io:format("insert_tests/3: Finding testpoints: Done\n", []),
  %%io:format("insert_tests/3: Infomap: ~w\n", [gb_trees:to_list(NewInfoMap)]),
  make_tests(Cfg, NewInfoMap, NewToOldMap, OldToNewMap).

find_testpoints(Cfg, Labels, InfoMap) ->
  case find_testpoints(Labels, InfoMap, Cfg, false) of
    {dirty, NewInfoMap} -> 
      %%io:format("find_testpoints/3: Looping\n", []),
      find_testpoints(Cfg, Labels, NewInfoMap);
    fixpoint ->
      InfoMap 
  end.

find_testpoints([Lbl|Left], InfoMap, Cfg, Dirty) ->
  Code = hipe_bb:code(hipe_icode_cfg:bb(Cfg, Lbl)),
  InfoOut = join_info(hipe_icode_cfg:succ(Cfg, Lbl), InfoMap),  
  OldInfoIn = infomap_get_all(Lbl, InfoMap),
  NewInfoIn = traverse_code(lists:reverse(Code), InfoOut),
  case (gb_sets:is_subset(OldInfoIn, NewInfoIn) andalso 
	gb_sets:is_subset(NewInfoIn, OldInfoIn)) of
    true ->
      find_testpoints(Left, InfoMap, Cfg, Dirty);
    false ->
      %%io:format("find_testpoints/4: Label: ~w: OldMap ~w\nNewMap: ~w\n", 
      %%	 [Lbl, gb_sets:to_list(OldInfoIn), gb_sets:to_list(NewInfoIn)]),
      NewInfoMap = gb_trees:update(Lbl, NewInfoIn, InfoMap),
      find_testpoints(Left, NewInfoMap, Cfg, true)
  end;
find_testpoints([], InfoMap, _Cfg, Dirty) ->
  if Dirty -> {dirty, InfoMap};
     true -> fixpoint
  end.

traverse_code([I|Left], Info) ->
  NewInfo = kill_defines(I, Info),
  case I of
    #icode_call{} ->
      case is_unsafe_arith(I) of
	true ->
	  %% The dst is sure to be a fixnum. Remove the 'killed' mark.
	  Dst = hd(hipe_icode:call_dstlist(I)),
	  NewInfo1 = gb_sets:delete_any({killed, Dst}, NewInfo),
	  NewInfo2 = 
	    gb_sets:union(NewInfo1, gb_sets:from_list(hipe_icode:uses(I))),
	  traverse_code(Left, NewInfo2);
	false ->
	  traverse_code(Left, NewInfo)
      end;
    #icode_move{} ->
      Dst = hipe_icode:move_dst(I),
      case gb_sets:is_member(Dst, Info) of 
	true -> 
	  %% The dst is an argument to an arith op. Transfer the test
	  %% to the src and remove the 'killed' mark from the dst.
	  NewInfo1 = gb_sets:delete({killed, Dst}, NewInfo),
	  Src = hipe_icode:move_src(I),
	  case hipe_icode:is_const(Src) of
	    true ->
	      traverse_code(Left, NewInfo1);
	    false ->
	      NewInfo2 = gb_sets:add(Src, NewInfo1),
	      traverse_code(Left, NewInfo2)
	  end;
	false ->
	  traverse_code(Left, NewInfo)
      end;
    _ ->
      traverse_code(Left, NewInfo)
  end;
traverse_code([], Info) ->
  Info.

kill_defines(I, Info) ->
  Defines = hipe_icode:defines(I),
  case [X || X<-Defines, gb_sets:is_member(X, Info)] of
    [] ->
      Info;
    List ->
      TmpInfo = gb_sets:difference(Info, gb_sets:from_list(List)),
      gb_sets:union(gb_sets:from_list([{killed, X} || X <- List]), TmpInfo)
  end.

make_tests(Cfg, InfoMap, NewToOldMap, OldToNewMap) ->
  %%io:format("make_tests 0:\n",[]),
  WorkList = make_worklist(gb_trees:keys(NewToOldMap), InfoMap, 
			   NewToOldMap, Cfg, []),
  %%io:format("make_tests 1:Worklist: ~w\n",[WorkList]),
  NewCfg = make_tests(WorkList, Cfg),
  %%io:format("make_tests 2\n",[]),
  %% If the arguments to this function are used in unsafe arith
  %% they should be marked as killed by a new start block.
  Args = hipe_icode_cfg:params(NewCfg),
  Start = hipe_icode_cfg:start_label(NewCfg),
  AltStart = gb_trees:get(Start, OldToNewMap),
  UnsafeIn = gb_sets:to_list(infomap_get(AltStart, InfoMap)),
  case [X || X <- UnsafeIn, Y <- Args, X =:= Y] of
    [] -> 
      hipe_icode_cfg:start_label_update(NewCfg, AltStart);
    KilledArgs ->
      NewStart = hipe_icode:label_name(hipe_icode:mk_new_label()),
      NewCfg1 = insert_test_block(NewStart, AltStart, Start,
				  KilledArgs, NewCfg),
      hipe_icode_cfg:start_label_update(NewCfg1, NewStart)
  end.

make_worklist([Lbl|Left], InfoMap, LabelMap, Cfg, Acc) ->
  Vars = infomap_get_killed(Lbl, InfoMap),
  case gb_sets:is_empty(Vars) of
    true -> make_worklist(Left, InfoMap, LabelMap, Cfg, Acc);
    false ->
      %% io:format("make_worklist 1 ~w\n", [Vars]),
      NewAcc0 =
	[{Lbl, Succ, gb_trees:get(Succ, LabelMap),
	  gb_sets:intersection(infomap_get(Succ, InfoMap), Vars)}
	 || Succ <- hipe_icode_cfg:succ(Cfg, Lbl)],
      NewAcc = [{Label, Succ, FailLbl, gb_sets:to_list(PrunedVars)}
		|| {Label, Succ, FailLbl, PrunedVars} <- NewAcc0,
		   gb_sets:is_empty(PrunedVars) =:= false] ++ Acc,
      %% io:format("make_worklist 2\n", []),
      make_worklist(Left, InfoMap, LabelMap, Cfg, NewAcc)
  end;
make_worklist([], _InfoMap, _LabelMap, _Cfg, Acc) ->
  Acc.

make_tests([{FromLbl, ToLbl, FailLbl, Vars}|Left], Cfg) ->
  NewLbl = hipe_icode:label_name(hipe_icode:mk_new_label()),
  TmpCfg = insert_test_block(NewLbl, ToLbl, FailLbl, Vars, Cfg),  
  NewCfg = hipe_icode_cfg:redirect(TmpCfg, FromLbl, ToLbl, NewLbl),
  make_tests(Left, NewCfg);
make_tests([], Cfg) ->
  Cfg.

insert_test_block(NewLbl, Succ, FailLbl, Vars, Cfg) ->
  Code = [hipe_icode:mk_type(Vars, fixnum, Succ, FailLbl, 0.99)],
  BB = hipe_bb:mk_bb(Code),
  hipe_icode_cfg:bb_add(Cfg, NewLbl, BB).

infomap_init(Labels) ->
  infomap_init(Labels, gb_trees:empty()).

infomap_init([Lbl|Left], Map) ->
  infomap_init(Left, gb_trees:insert(Lbl, gb_sets:empty(), Map));
infomap_init([], Map) ->
  Map.

join_info(Labels, Map) ->
  join_info(Labels, Map, gb_sets:empty()).

join_info([Lbl|Left], Map, Set) ->  
  join_info(Left, Map, gb_sets:union(Set, infomap_get(Lbl, Map)));
join_info([], _Map, Set) ->
  Set.

infomap_get(Lbl, Map) ->
  case gb_trees:lookup(Lbl, Map) of
    none -> gb_sets:empty();
    {value, Val} ->
      gb_sets:filter(fun(X) -> case X of 
				 {killed, _} -> false;
				 _ -> true
			       end
		     end,
		     Val)
  end.

infomap_get_all(Lbl, Map) ->
  case gb_trees:lookup(Lbl, Map) of
    none -> gb_sets:empty();
    {value, Val} -> Val
  end.

infomap_get_killed(Lbl, Map) ->
  case gb_trees:lookup(Lbl, Map) of
    none -> gb_sets:empty();
    {value, Val} ->
      Fun = fun(X, Acc) ->
		case X of
		  {killed, Var} -> [Var|Acc];
		  _ -> Acc
		end
	    end,
      gb_sets:from_list(lists:foldl(Fun, [], gb_sets:to_list(Val)))
  end.

%%%-------------------------------------------------------------------
%%% General replace of '+'/'-' to super safe version

arithop_to_split(Op) ->
  case Op of
    '+' -> gen_add;
    '-' -> gen_sub;
    _ -> Op
  end.

%%%-------------------------------------------------------------------
%%% Check if it's an arith op that needs to be split

is_arith(I) ->
  case hipe_icode:call_fun(I) of
    '+' -> true;
    '-' -> true;
    gen_add -> true;
    gen_sub -> true;
    'bor' -> true;
    'bxor' -> true;
    'bsr' ->
      %% Need to check that the second argument is a non-negative
      %% fixnum. We only allow for constants to simplify things.
      [_, Arg2] = hipe_icode:args(I),
      hipe_icode:is_const(Arg2) andalso (hipe_icode:const_value(Arg2) >= 0);
    'bsl' ->
      %% There are major issues with bsl since it doesn't flag
      %% overflow. We cannot allow for this in this optimization pass.
      false;
    'bnot' -> true;
    'band' -> true;
    _ -> false
  end.

%%%-------------------------------------------------------------------

is_unsafe_arith(I) ->
  case hipe_icode:call_fun(I) of
    unsafe_add -> true;
    unsafe_sub -> true;
    unsafe_bor -> true;
    unsafe_bxor -> true;
    unsafe_bsr -> true;
    unsafe_bsl -> true;
    unsafe_bnot -> true;
    unsafe_band -> true;
    _ -> false
  end.

split_to_unsafe(I) ->
  case hipe_icode:call_fun(I) of
    gen_add -> unsafe_add;
    gen_sub -> unsafe_sub;
    'bor' -> unsafe_bor;
    'bxor' -> unsafe_bxor;
    'bsr' -> 
      case is_arith(I) of
	true -> unsafe_bsr;
	false -> 'bsr'
      end;
    'bsl' ->
      %% There are major issues with bsl since it doesn't flag
      %% overflow. We cannot allow for this in this optimization pass.
      'bsl';
    'bnot' -> unsafe_bnot;
    'band' -> unsafe_band;
    Op -> Op
  end.

%%%-------------------------------------------------------------------
%%% FLAG = split_arith_unsafe

is_arith_extra_unsafe(I) ->
  case hipe_icode:call_fun(I) of
    '+' -> true;
    '-' -> true;
    'bor' -> true;
    'bxor' -> true;
    'bsr' -> is_arith(I);
    'bsl' -> false; %% See comment in is_arith/1
    'bnot' -> true;
    'band' -> true;
    _ -> false
  end.

arithop_to_extra_unsafe(Op) ->
  case Op of
    '+' -> extra_unsafe_add;
    '-' -> extra_unsafe_sub;
    'bor' -> unsafe_bor;
    'bxor' -> unsafe_bxor;
    'bsr' -> unsafe_bsr;
    'bsl' -> 'bsl'; %% See comment in split_to_unsafe/1
    'bnot' -> unsafe_bnot;
    'band' -> unsafe_band
  end.

%%%-------------------------------------------------------------------

redirect(I, LabelMap) ->
  case hipe_icode:successors(I) of
    [] -> I;
    Successors ->
      RedirectMap = [{X, gb_trees:get(X, LabelMap)} || X <- Successors],
      redirect_1(RedirectMap, I)
  end.

redirect_1([{From, To}|Left], I) ->
  redirect_1(Left, hipe_icode:redirect_jmp(I, From, To));
redirect_1([], I) ->
  I.

new_label_maps(Labels) ->
  new_label_maps(Labels, gb_trees:empty(), gb_trees:empty()).

new_label_maps([Lbl|Left], Map1, Map2) ->
  NewLabel = hipe_icode:label_name(hipe_icode:mk_new_label()),
  NewMap1 = gb_trees:insert(Lbl, NewLabel, Map1),
  NewMap2 = gb_trees:insert(NewLabel, Lbl, Map2),
  new_label_maps(Left, NewMap1, NewMap2);
new_label_maps([], Map1, Map2) ->
  {Map1, Map2}.
