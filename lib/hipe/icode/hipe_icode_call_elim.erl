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
%%----------------------------------------------------------------------
%% File    : hipe_icode_call_elim.erl
%% Authors : Daniel S. McCain <dsmccain@acm.org>,
%%           Magnus Lång <margnus1@telia.com>
%% Created : 14 Apr 2014 by Magnus Lång <margnus1@telia.com>
%% Purpose : Eliminate calls to BIFs that are side-effect free only when
%%           executed on some argument types.
%%----------------------------------------------------------------------
-module(hipe_icode_call_elim).
-export([cfg/1]).

-include("hipe_icode.hrl").
-include("../flow/cfg.hrl").

-spec cfg(cfg()) -> cfg().

cfg(IcodeSSA) ->
  lists:foldl(fun (Lbl, CFG1) ->
		  BB1 = hipe_icode_cfg:bb(CFG1, Lbl),
		  Code1 = hipe_bb:code(BB1),
		  Code2 = lists:map(fun elim_insn/1, Code1),
		  BB2 = hipe_bb:code_update(BB1, Code2),
		  hipe_icode_cfg:bb_add(CFG1, Lbl, BB2)
	      end, IcodeSSA, hipe_icode_cfg:labels(IcodeSSA)).

-spec elim_insn(icode_instr()) -> icode_instr().
elim_insn(Insn=#icode_call{'fun'={_,_,_}=MFA, args=Args, type=remote,
			   dstlist=[Dst=#icode_variable{
					   annotation={type_anno, RetType, _}}],
			   continuation=[], fail_label=[]}) ->
  Opaques = 'universe',
  case erl_types:t_is_singleton(RetType, Opaques) of
    true ->
      ArgTypes = [case Arg of
		    #icode_variable{annotation={type_anno, Type, _}} -> Type;
		    #icode_const{} ->
		      erl_types:t_from_term(hipe_icode:const_value(Arg))
		  end || Arg <- Args],
      case can_be_eliminated(MFA, ArgTypes) of
	true ->
	  Const = hipe_icode:mk_const(
		    erl_types:t_singleton_to_term(RetType, Opaques)),
	  #icode_move{dst=Dst, src=Const};
	false -> Insn
      end;
    false -> Insn
  end;
elim_insn(Insn) -> Insn.


%% A function can be eliminated for some argument types if it has no side
%% effects when run on arguments of those types.

-spec can_be_eliminated(mfa(), [erl_types:erl_type()]) -> boolean().

can_be_eliminated({maps, is_key, 2}, [_K, M]) ->
  erl_types:t_is_map(M);
can_be_eliminated(_, _) ->
  false.
