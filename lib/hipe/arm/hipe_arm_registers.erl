%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2005-2016. All Rights Reserved.
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

-module(hipe_arm_registers).

-export([reg_name_gpr/1,
	 first_virtual/0,
	 is_precoloured_gpr/1,
	 all_precoloured/0, % for coalescing
	 return_value/0,
	 temp1/0,	% C callee-save, not parameter, may be allocatable
	 temp2/0,	% not parameter, must not be allocatable (frame)
	 temp3/0,	% not parameter, may be allocatable
	 heap_pointer/0,
	 stack_pointer/0,
	 proc_pointer/0,
	 lr/0,
	 pc/0,
	 %% heap_limit/0,
	 %% fcalls/0,
	 allocatable_gpr/0, % for coalescing
	 is_fixed/1,	% for graph coloring
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
-define(R13, 13). % XXX: see all_precoloured()
-define(R14, 14).
-define(R15, 15).
-define(LAST_PRECOLOURED, 15). % must handle both GPR and FPR ranges

-define(ARG0, ?R1).
-define(ARG1, ?R2).
-define(ARG2, ?R3).
-define(ARG3, ?R4).
-define(ARG4, ?R5).
-define(ARG5, ?R6).

-define(TEMP1, ?R8).	% stores LR around inc_stack calls, must be C calleE-save
-define(TEMP2, ?R12).
-define(TEMP3, ?R7).

-define(RETURN_VALUE, ?R0).
-define(HEAP_POINTER, ?R9).
-define(STACK_POINTER, ?R10).
-define(PROC_POINTER, ?R11).

reg_name_gpr(R) -> [$r | integer_to_list(R)].

%%% Must handle both GPR and FPR ranges.
first_virtual() -> ?LAST_PRECOLOURED + 1.

%%% These two tests have the same implementation, but that's
%%% not something we should cast in stone in the interface. 
is_precoloured_gpr(R) -> R =< ?LAST_PRECOLOURED.

all_precoloured() ->
  %% XXX: R13 should be skipped as it never is used anywhere.
  %% Unfortunately, gaps in the list of precoloured registers
  %% cause the graph_color register allocator to create bogus
  %% assignments for those "registers", which in turn causes
  %% the "precoloured reg must map to itself" sanity check in
  %% the frame module to signal errors.
  [ ?R0,  ?R1,  ?R2,  ?R3,  ?R4,  ?R5,  ?R6,  ?R7,
    ?R8,  ?R9, ?R10, ?R11, ?R12, ?R13, ?R14, ?R15].

return_value() -> ?RETURN_VALUE.

temp1() -> ?TEMP1.
temp2() -> ?TEMP2.
temp3() -> ?TEMP3.	% for base2 in storeix :-(

heap_pointer() -> ?HEAP_POINTER.

stack_pointer() -> ?STACK_POINTER.

proc_pointer() -> ?PROC_POINTER.

lr() -> ?R14.

pc() -> ?R15.

allocatable_gpr() ->
  %% r9, r10, and r11 are fixed global registers.
  %% r12 may be used by the frame module for large load/store offsets.
  %% r13 is reserved for C.
  %% r15 is the PC, and is not usable as a variable.
  [ ?R0,  ?R1,  ?R2,  ?R3,  ?R4,  ?R5,  ?R6,  ?R7,
    ?R8,                               ?R14].

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
    ?R15 -> true;
    ?R13 -> true; % XXX: see all_precoloured()
    ?R12 -> true;
    _ -> false
  end.

nr_args() -> ?ARM_NR_ARG_REGS.

args(Arity) when is_integer(Arity) ->
  N = erlang:min(Arity, ?ARM_NR_ARG_REGS),
  args(N-1, []).

args(-1, Rest) -> Rest;
args(I, Rest) -> args(I-1, [arg(I) | Rest]).

arg(N) ->
  if N < ?ARM_NR_ARG_REGS ->
      case N of
	0 -> ?ARG0;
	1 -> ?ARG1;
	2 -> ?ARG2;
	3 -> ?ARG3;
	4 -> ?ARG4;
	5 -> ?ARG5;
	_ -> exit({?MODULE, arg, N})
      end;
     true ->
      exit({?MODULE, arg, N})
  end.

is_arg(R) ->
  case R of
    ?ARG0 -> ?ARM_NR_ARG_REGS > 0;
    ?ARG1 -> ?ARM_NR_ARG_REGS > 1;
    ?ARG2 -> ?ARM_NR_ARG_REGS > 2;
    ?ARG3 -> ?ARM_NR_ARG_REGS > 3;
    ?ARG4 -> ?ARM_NR_ARG_REGS > 4;
    ?ARG5 -> ?ARM_NR_ARG_REGS > 5;
    _ -> false
  end.

call_clobbered() ->		% does the RA strip the type or not?
  [{?R0,tagged},{?R0,untagged},
   {?R1,tagged},{?R1,untagged},
   {?R2,tagged},{?R2,untagged},
   {?R3,tagged},{?R3,untagged},
   {?R4,tagged},{?R4,untagged},
   {?R5,tagged},{?R5,untagged},
   {?R6,tagged},{?R6,untagged},
   {?R7,tagged},{?R7,untagged},
   {?R8,tagged},{?R8,untagged},
   %% R9 is fixed (HP)
   %% R10 is fixed (NSP)
   %% R11 is fixed (P)
   {?R12,tagged},{?R12,untagged},
   %% R13 is reserved for C
   {?R14,tagged},{?R14,untagged}
   %% R15 is the non-allocatable PC
  ].

tailcall_clobbered() ->		% tailcall crapola needs one temp
  [{?TEMP1,tagged},{?TEMP1,untagged}].

live_at_return() ->
  [%%{?LR,untagged},
   {?HEAP_POINTER,untagged},
   {?STACK_POINTER,untagged},
   {?PROC_POINTER,untagged}
  ].
