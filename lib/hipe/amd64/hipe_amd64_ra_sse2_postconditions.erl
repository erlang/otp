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

-module(hipe_amd64_ra_sse2_postconditions).

-export([check_and_rewrite/2]).

-include("../x86/hipe_x86.hrl").
-define(HIPE_INSTRUMENT_COMPILER, true).
-include("../main/hipe.hrl").
-define(count_temp(T), ?cons_counter(counter_mfa_mem_temps, T)).


check_and_rewrite(AMD64Defun, Coloring) ->
  %%io:format("Converting\n"),
  TempMap = hipe_temp_map:cols2tuple(Coloring,hipe_amd64_specific_sse2),
  %%io:format("Rewriting\n"),
  #defun{code=Code0} = AMD64Defun,
  {Code1, DidSpill} = do_insns(Code0, TempMap, [], false),
  {AMD64Defun#defun{code=Code1, var_range={0, hipe_gensym:get_var(x86)}}, 
   DidSpill}.

do_insns([I|Insns], TempMap, Accum, DidSpill0) ->
  {NewIs, DidSpill1} = do_insn(I, TempMap),
  do_insns(Insns, TempMap, lists:reverse(NewIs, Accum), DidSpill0 or DidSpill1);
do_insns([], _TempMap, Accum, DidSpill) ->
  {lists:reverse(Accum), DidSpill}.

do_insn(I, TempMap) ->	% Insn -> {Insn list, DidSpill}
  case I of
    #fmove{} ->
      do_fmove(I, TempMap);
    #fp_unop{} ->
      do_fp_unop(I, TempMap);
    #fp_binop{} ->
      do_fp_binop(I, TempMap);
    _ ->
      %% All non sse2 ops
      {[I], false}
  end.

%%% Fix an fp_binop.
do_fp_binop(I, TempMap) ->
  #fp_binop{src=Src,dst=Dst} = I,
  case is_mem_opnd(Dst, TempMap) of
    true ->
      Tmp = clone(Dst),
      {[#fmove{src=Dst, dst=Tmp},
	I#fp_binop{src=Src,dst=Tmp},
	#fmove{src=Tmp,dst=Dst}],
       true};
    false ->
      {[I], false}
  end.

do_fp_unop(I, TempMap) ->
  #fp_unop{arg=Arg} = I,
  case is_mem_opnd(Arg, TempMap) of
    true ->
      Tmp = clone(Arg),
      {[#fmove{src=Arg, dst=Tmp},
	I#fp_unop{arg=Tmp},
	#fmove{src=Tmp,dst=Arg}],
       true};
    false ->
      {[I], false}
  end.

%%% Fix an fmove op.
do_fmove(I, TempMap) ->
  #fmove{src=Src,dst=Dst} = I,
  case is_mem_opnd(Dst, TempMap) and is_mem_opnd(Src, TempMap) of
    true ->
      Tmp = clone(Src),
      {[#fmove{src=Src, dst=Tmp},I#fmove{src=Tmp,dst=Dst}],
       true};
    false ->
      {[I], false}
  end.

%%% Check if an operand denotes a memory cell (mem or pseudo).

is_mem_opnd(Opnd, TempMap) ->
  R =
    case Opnd of
      #x86_mem{} -> true;
      #x86_temp{} -> 
	Reg = hipe_x86:temp_reg(Opnd),
	case hipe_x86:temp_is_allocatable(Opnd) of
	  true -> 
	    case tuple_size(TempMap) > Reg of 
	      true ->
		case 
		  hipe_temp_map:is_spilled(Reg, TempMap) of
		  true ->
		    ?count_temp(Reg),
		    true;
		  false -> false
		end;
	      _ -> false
	    end;
	  false -> true
	end;
      _ -> false
    end,
  %% io:format("Op ~w mem: ~w\n",[Opnd,R]),
  R.

%%% Check if an operand is a spilled Temp.

%%src_is_spilled(Src, TempMap) ->
%%  case hipe_x86:is_temp(Src) of
%%    true ->
%%      Reg = hipe_x86:temp_reg(Src),
%%      case hipe_x86:temp_is_allocatable(Src) of
%%	true -> 
%%	  case tuple_size(TempMap) > Reg of 
%%	    true ->
%%	      case hipe_temp_map:is_spilled(Reg, TempMap) of
%%		true ->
%%		  ?count_temp(Reg),
%%		  true;
%%		false ->
%%		  false
%%	      end;
%%	    false ->
%%	      false
%%	  end;
%%	false -> true
%%      end;
%%    false -> false
%%  end.

%% is_spilled(Temp, TempMap) ->
%%   case hipe_x86:temp_is_allocatable(Temp) of
%%     true ->
%%       Reg = hipe_x86:temp_reg(Temp),
%%       case tuple_size(TempMap) > Reg of 
%%  	true ->
%%  	  case hipe_temp_map:is_spilled(Reg, TempMap) of
%%  	    true ->
%%  	      ?count_temp(Reg),
%%  	      true;
%%  	    false ->
%%  	      false
%%  	  end;
%%  	false ->
%%  	  false
%%       end;
%%     false -> true
%%   end.

%%% Make Reg a clone of Dst (attach Dst's type to Reg).

clone(Dst) ->
  Type =
    case Dst of
      #x86_mem{} -> hipe_x86:mem_type(Dst);
      #x86_temp{} -> hipe_x86:temp_type(Dst)
    end,
  hipe_x86:mk_new_temp(Type).

%%% Make a certain reg into a clone of Dst

%% clone2(Dst, Reg) ->
%%   Type =
%%     case Dst of
%%       #x86_mem{} -> hipe_x86:mem_type(Dst);
%%       #x86_temp{} -> hipe_x86:temp_type(Dst)
%%     end,
%%   hipe_x86:mk_temp(Reg,Type).
