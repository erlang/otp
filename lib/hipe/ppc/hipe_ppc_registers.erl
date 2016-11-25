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

-module(hipe_ppc_registers).

-export([reg_name_gpr/1,
	 reg_name_fpr/1,
	 first_virtual/0,
	 is_precoloured_gpr/1,
	 is_precoloured_fpr/1,
	 all_precoloured/0,
	 return_value/0,
	 temp1/0,
	 temp2/0,
	 temp3/0,	% for base2 in storeix :-(
	 heap_pointer/0,
	 stack_pointer/0,
	 proc_pointer/0,
	 %%heap_limit/0,
	 %%fcalls/0,
	 allocatable_gpr/0,
	 allocatable_fpr/0,
	 is_fixed/1,
	 nr_args/0,
	 arg/1,
	 args/1,
	 is_arg/1,	% for linear scan
	 call_clobbered/0,
	 tailcall_clobbered/0,
	 live_at_return/0
	 ]).

-include("../rtl/hipe_literals.hrl").

-define(R0, 0).
-define(R1, 1).
-define(R2, 2).
-define(R3, 3).
-define(R4, 4).
-define(R5, 5).
-define(R6, 6).
-define(R7, 7).
-define(R8, 8).
-define(R9, 9).
-define(R10, 10).
-define(R11, 11).
-define(R12, 12).
-define(R13, 13).
-define(R14, 14).
-define(R15, 15).
-define(R16, 16).
-define(R17, 17).
-define(R18, 18).
-define(R19, 19).
-define(R20, 20).
-define(R21, 21).
-define(R22, 22).
-define(R23, 23).
-define(R24, 24).
-define(R25, 25).
-define(R26, 26).
-define(R27, 27).
-define(R28, 28).
-define(R29, 29).
-define(R30, 30).
-define(R31, 31).
-define(LAST_PRECOLOURED, 31). % must handle both GPR and FPR ranges

-define(ARG0, ?R4).
-define(ARG1, ?R5).
-define(ARG2, ?R6).
-define(ARG3, ?R7).
-define(ARG4, ?R8).
-define(ARG5, ?R9).
-define(ARG6, ?R10).

-define(TEMP1, ?R28).
-define(TEMP2, ?R27).
-define(TEMP3, ?R26).	% XXX: for base2 in storeix, switch to R0 instead?

-define(RETURN_VALUE, ?R3).
-define(HEAP_POINTER, ?R29).
-define(STACK_POINTER, ?R30).
-define(PROC_POINTER, ?R31).

reg_name_gpr(R) -> [$r | integer_to_list(R)].
reg_name_fpr(R) -> [$f | integer_to_list(R)].

%%% Must handle both GPR and FPR ranges.
first_virtual() -> ?LAST_PRECOLOURED + 1.

%%% These two tests have the same implementation, but that's
%%% not something we should cast in stone in the interface. 
is_precoloured_gpr(R) -> R =< ?LAST_PRECOLOURED.
is_precoloured_fpr(R) -> R =< ?LAST_PRECOLOURED.

all_precoloured() ->
  %% XXX: skip R1, R2, and R13. They should never occur anywhere.
  [ ?R0,  ?R1,  ?R2,  ?R3,  ?R4,  ?R5,  ?R6,  ?R7,
    ?R8,  ?R9, ?R10, ?R11, ?R12, ?R13, ?R14, ?R15,
   ?R16, ?R17, ?R18, ?R19, ?R20, ?R21, ?R22, ?R23,
   ?R24, ?R25, ?R26, ?R27, ?R28, ?R29, ?R30, ?R31].

return_value() -> ?RETURN_VALUE.

temp1() -> ?TEMP1.
temp2() -> ?TEMP2.
temp3() -> ?TEMP3.	% for base2 in storeix :-(

heap_pointer() -> ?HEAP_POINTER.

stack_pointer() -> ?STACK_POINTER.

proc_pointer() -> ?PROC_POINTER.

allocatable_gpr() ->
  %% r0 is too restricted to be useful for variables
  %% r1, r2, and r13 are reserved for C
  %% r29, r30, and r31 are fixed global registers
  [                   ?R3,  ?R4,  ?R5,  ?R6,  ?R7,
    ?R8,  ?R9, ?R10, ?R11, ?R12,       ?R14, ?R15,
   ?R16, ?R17, ?R18, ?R19, ?R20, ?R21, ?R22, ?R23,
   ?R24, ?R25, ?R26, ?R27, ?R28].

allocatable_fpr() ->
  [ 0,  1,  2,  3,  4,  5,  6,  7,
    8,  9, 10, 11, 12, 13, 14, 15,
   16, 17, 18, 19, 20, 21, 22, 23,
   24, 25, 26, 27, 28, 29, 30, 31].

%% Needed for hipe_graph_coloring_regalloc.
%% Presumably true for Reg in AllPrecoloured \ Allocatable.
is_fixed(Reg) ->
  case Reg of
    ?HEAP_POINTER -> true;
    ?STACK_POINTER -> true;
    ?PROC_POINTER -> true;
    %% The following cases are required for linear scan:
    %% it gets confused if it sees a register which is
    %% neither allocatable nor global (fixed or one of
    %% the scratch registers set aside for linear scan).
    ?R0 -> true;
    ?R1 -> true;
    ?R2 -> true;
    ?R13 -> true;
    _ -> false
  end.

nr_args() -> ?PPC_NR_ARG_REGS.

args(Arity) when is_integer(Arity) ->
  N = erlang:min(Arity, ?PPC_NR_ARG_REGS),
  args(N-1, []).

args(I, Rest) when is_integer(I), I < 0 -> Rest;
args(I, Rest) -> args(I-1, [arg(I) | Rest]).

arg(N) ->
  if N < ?PPC_NR_ARG_REGS ->
      case N of
	0 -> ?ARG0;
	1 -> ?ARG1;
	2 -> ?ARG2;
	3 -> ?ARG3;
	4 -> ?ARG4;
	5 -> ?ARG5;
	6 -> ?ARG6;
	_ -> exit({?MODULE, arg, N})
      end;
     true ->
      exit({?MODULE, arg, N})
  end.

is_arg(R) ->
  case R of
    ?ARG0 -> ?PPC_NR_ARG_REGS > 0;
    ?ARG1 -> ?PPC_NR_ARG_REGS > 1;
    ?ARG2 -> ?PPC_NR_ARG_REGS > 2;
    ?ARG3 -> ?PPC_NR_ARG_REGS > 3;
    ?ARG4 -> ?PPC_NR_ARG_REGS > 4;
    ?ARG5 -> ?PPC_NR_ARG_REGS > 5;
    ?ARG6 -> ?PPC_NR_ARG_REGS > 6;
    _ -> false
  end.

%% Note: the fact that allocatable_gpr() is a subset of call_clobbered() is
%% hard-coded in hipe_ppc_defuse:insn_defs_all_gpr/1
call_clobbered() ->		% does the RA strip the type or not?
  [{?R0,tagged},{?R0,untagged},
   %% R1 is reserved for C
   %% R2 is reserved for C
   {?R3,tagged},{?R3,untagged},
   {?R4,tagged},{?R4,untagged},
   {?R5,tagged},{?R5,untagged},
   {?R6,tagged},{?R6,untagged},
   {?R7,tagged},{?R7,untagged},
   {?R8,tagged},{?R8,untagged},
   {?R9,tagged},{?R9,untagged},
   {?R10,tagged},{?R10,untagged},
   {?R11,tagged},{?R11,untagged},
   {?R12,tagged},{?R12,untagged},
   %% R13 is reserved for C
   {?R14,tagged},{?R14,untagged},
   {?R15,tagged},{?R15,untagged},
   {?R16,tagged},{?R16,untagged},
   {?R17,tagged},{?R17,untagged},
   {?R18,tagged},{?R18,untagged},
   {?R19,tagged},{?R19,untagged},
   {?R20,tagged},{?R20,untagged},
   {?R21,tagged},{?R21,untagged},
   {?R22,tagged},{?R22,untagged},
   {?R23,tagged},{?R23,untagged},
   {?R24,tagged},{?R24,untagged},
   {?R25,tagged},{?R25,untagged},
   {?R26,tagged},{?R26,untagged},
   {?R27,tagged},{?R27,untagged},
   {?R28,tagged},{?R28,untagged}
   %% R29 is fixed (HP)
   %% R30 is fixed (NSP)
   %% R31 is fixed (P)
  ].

tailcall_clobbered() ->		% tailcall crapola needs one temp
  [{?TEMP1,tagged},{?TEMP1,untagged}].

live_at_return() ->
  [%%{?LR,untagged},
   {?HEAP_POINTER,untagged},
   {?STACK_POINTER,untagged},
   {?PROC_POINTER,untagged}
  ].
