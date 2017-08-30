%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2016. All Rights Reserved.
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

-module(hipe_rtl_cfg).

-export([init/1,
         labels/1,
	 params/1, params_update/2,
         start_label/1,
         succ/2,
         pred/2,
         bb/2, bb_add/3, bb_insert_between/5,
	 redirect/4,
         remove_trivial_bbs/1, remove_unreachable_code/1,
	 linearize/1,
	 pp/1, pp/2]).
-export([preorder/1, postorder/1, reverse_postorder/1]).

-define(RTL_CFG, true).	% needed for cfg.inc below

-include("../main/hipe.hrl").
-include("hipe_rtl.hrl").
-include("../flow/cfg.hrl").
-include("../flow/cfg.inc").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% CFG interface to RTL.
%%

init(Rtl) ->
  %% hipe_rtl:pp(Rtl),
  Code = hipe_rtl:rtl_code(Rtl),
  StartLabel = hipe_rtl:label_name(hd(Code)),
  CFG0 = mk_empty_cfg(hipe_rtl:rtl_fun(Rtl), 
		      StartLabel, 
		      hipe_rtl:rtl_data(Rtl),
		      hipe_rtl:rtl_is_closure(Rtl),
		      hipe_rtl:rtl_is_leaf(Rtl),
		      hipe_rtl:rtl_params(Rtl)),
  CFG = info_update(CFG0, hipe_rtl:rtl_info(Rtl)),
  take_bbs(Code, CFG).

%% @spec is_comment(hipe_rtl:rtl_instruction()) -> boolean()
%% @doc  Succeeds if Instr has no effect.
is_comment(Instr) ->
  hipe_rtl:is_comment(Instr).

%% @spec is_goto(hipe_rtl:rtl_instruction()) -> boolean()
%% @doc  Succeeds if Instr is just a jump (no side-effects).
is_goto(Instr) ->
  hipe_rtl:is_goto(Instr).

is_label(Instr) ->
  hipe_rtl:is_label(Instr).

label_name(Instr) ->
  hipe_rtl:label_name(Instr).

mk_label(Name) ->
  hipe_rtl:mk_label(Name).

mk_goto(Name) ->
  hipe_rtl:mk_goto(Name).

branch_successors(Instr) ->
  case Instr of
    #branch{} -> [hipe_rtl:branch_true_label(Instr), 
		  hipe_rtl:branch_false_label(Instr)];
    #alub{} -> [hipe_rtl:alub_true_label(Instr), 
	        hipe_rtl:alub_false_label(Instr)];
    #switch{} -> hipe_rtl:switch_labels(Instr);
    #call{} -> 
      case hipe_rtl:call_fail(Instr) of
	[] -> [hipe_rtl:call_continuation(Instr)];
	Fail -> [hipe_rtl:call_continuation(Instr),Fail]
      end;
    #goto{} -> [hipe_rtl:goto_label(Instr)];
    #goto_index{} -> hipe_rtl:goto_index_labels(Instr);
    _ -> []
  end.

fails_to(Instr) ->
  case Instr of
    #call{} -> [hipe_rtl:call_fail(Instr)];
    _ -> []
  end.

is_branch(Instr) ->
   case Instr of
     #branch{} -> true;
     #alub{} -> true;
     #switch{} -> true;
     #goto{} -> true;
     #goto_index{} -> true;
     #enter{} -> true;
     #return{} -> true;
     #call{} -> 
       case hipe_rtl:call_fail(Instr) of
	[] ->  
	   case hipe_rtl:call_continuation(Instr) of
	     [] -> false;
	     _ -> true
	   end;
	 _ -> true
       end;
     _ -> false
   end.

is_pure_branch(Instr) ->
  case Instr of
    #branch{} -> true;
    #switch{} -> true;
    #goto{} -> true;
    _ -> false
  end.

redirect_jmp(Jmp, ToOld, ToNew) ->
   hipe_rtl:redirect_jmp(Jmp, ToOld, ToNew).

redirect_ops([Label|Labels], CFG, Map) ->
  BB = bb(CFG, Label),
  Code = hipe_bb:code(BB),
  NewCode = [rewrite(I,Map) || I <- Code],
  NewCFG = bb_add(CFG, Label, hipe_bb:code_update(BB, NewCode)),
  redirect_ops(Labels, NewCFG, Map);
redirect_ops([],CFG,_) -> CFG.

rewrite(I, Map) ->
  case I of
    #load_address{} ->
	case hipe_rtl:load_address_type(I) of
	  constant -> I;
	  _ -> 
	    NewL =
	      find_new_label(hipe_rtl:load_address_addr(I), Map),
	    hipe_rtl:load_address_addr_update(I, NewL)
	end;
    _ -> I
  end.


pp(CFG) ->
  hipe_rtl:pp(linearize(CFG)).

pp(Dev, CFG) ->
  hipe_rtl:pp(Dev, linearize(CFG)).

linearize(CFG) ->
  Code = linearize_cfg(CFG),
  Rtl = hipe_rtl:mk_rtl(function(CFG),
			params(CFG),
			is_closure(CFG),
			is_leaf(CFG),
			Code, 
			data(CFG),	 
			hipe_gensym:var_range(rtl), 
			hipe_gensym:label_range(rtl)),
  hipe_rtl:rtl_info_update(Rtl, info(CFG)).

%% %% Warning: this arity might not be the true arity.
%% %% The true arity of a closure usually differs.
%% arity(CFG) ->
%%    {_M,_F,A} = function(CFG),
%%    A.

%% init_gensym(CFG)->
%%   HighestVar = find_highest_var(CFG),
%%   HighestLabel = find_highest_label(CFG),
%%   hipe_gensym:init(),
%%   hipe_gensym:set_var(rtl, HighestVar),
%%   hipe_gensym:set_label(rtl, HighestLabel).
%% 
%% highest_var(Code)->
%%   hipe_rtl:highest_var(Code).

is_phi(I) ->
  hipe_rtl:is_phi(I).

phi_remove_pred(I, Pred) ->
  hipe_rtl:phi_remove_pred(I, Pred).

phi_redirect_pred(I, OldPred, NewPred) ->
  hipe_rtl:phi_redirect_pred(I, OldPred, NewPred).
