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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% LIVENESS ANALYSIS
%%
%% Exports:
%% ~~~~~~~
%% analyze(CFG) - returns a liveness analysis of CFG.
%% liveout(Liveness, Label) - returns a set of variables that are live on
%%      exit from basic block named Label.
%% livein(Liveness, Label) - returns a set of variables that are live on
%%      entry to the basic block named Label.
%% list(Instructions, LiveOut) - Given a list of instructions and a liveout
%%      set, returns a set of variables live at the first instruction.
%%

-module(hipe_rtl_liveness).

%% -define(DEBUG_LIVENESS,true).
-define(LIVEOUT_NEEDED,true).	% needed for liveness.inc below.
-define(PRETTY_PRINT,false).

-include("hipe_rtl.hrl").
-include("../flow/liveness.inc").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Interface to CFG and RTL.
%%

cfg_bb(CFG, L) ->
   hipe_rtl_cfg:bb(CFG, L).

cfg_postorder(CFG) ->
   hipe_rtl_cfg:postorder(CFG).

cfg_succ(CFG, L) ->
   hipe_rtl_cfg:succ(CFG, L).

uses(Instr) ->
  hipe_rtl:uses(Instr).

defines(Instr) ->
  hipe_rtl:defines(Instr).

%%
%% This is the list of registers that are live at exit from a function
%%

liveout_no_succ() ->
  hipe_rtl_arch:live_at_return().

%%
%% The following are used only if annotation of the code is requested.
%%

cfg_labels(CFG) ->
   hipe_rtl_cfg:reverse_postorder(CFG).

pp_block(Label, CFG) ->
  BB=hipe_rtl_cfg:bb(CFG, Label),
  Code=hipe_bb:code(BB),
  hipe_rtl:pp_block(Code).

pp_liveness_info(LiveList) ->
  NewList=remove_precoloured(LiveList),
  print_live_list(NewList).

print_live_list([]) ->
  io:format(" none~n", []);
print_live_list([Last]) ->
  io:format(" ", []),
  print_var(Last),
  io:format("~n", []);
print_live_list([Var|Rest]) ->
  io:format(" ", []),
  print_var(Var),
  io:format(",", []), 
  print_live_list(Rest).

print_var(A) ->
  case hipe_rtl:is_var(A) of
    true -> 
      pp_var(A);
    false ->
      case hipe_rtl:is_reg(A) of
	true ->
	  pp_reg(A);
	false ->
	  case hipe_rtl:is_fpreg(A) of
	    true ->
	      io:format("f~w", [hipe_rtl:fpreg_index(A)]);
	    false ->
	      io:format("unknown:~w", [A])
	  end
      end
  end.

pp_hard_reg(N) ->
  io:format("~s", [hipe_rtl_arch:reg_name(N)]).

pp_reg(Arg) ->
  case hipe_rtl_arch:is_precoloured(Arg) of
    true ->
      pp_hard_reg(hipe_rtl:reg_index(Arg));
    false ->
      io:format("r~w", [hipe_rtl:reg_index(Arg)])
  end.

pp_var(Arg) ->
  case hipe_rtl_arch:is_precoloured(Arg) of
    true ->
      pp_hard_reg(hipe_rtl:var_index(Arg));
    false ->
      io:format("v~w", [hipe_rtl:var_index(Arg)])
  end.	      

remove_precoloured(List) ->
  List.  
  %% [X || X <- List, not hipe_rtl_arch:is_precoloured(X)].

-ifdef(DEBUG_LIVENESS).
cfg_bb_add(CFG, L, NewBB) ->
  hipe_rtl_cfg:bb_add(CFG, L, NewBB).

mk_comment(Text) ->
  hipe_rtl:mk_comment(Text).
-endif.
