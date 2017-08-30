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

-module(hipe_sparc_registers).

-export([reg_name_gpr/1,
	 reg_name_fpr/1,
	 first_virtual/0,
	 is_precoloured_gpr/1,
	 is_precoloured_fpr/1,
	 all_precoloured/0,	% for coalescing ra
	 return_value/0,
	 temp1/0,
	 temp2/0,
	 temp3/0,
	 heap_pointer/0,
	 stack_pointer/0,
	 proc_pointer/0,
	 return_address/0,
	 g0/0,
	 %% heap_limit/0,
	 %% fcalls/0,
	 allocatable_gpr/0,	% for coalescing ra
	 allocatable_fpr/0,
	 is_fixed/1,		% for graph_coloring ra
	 nr_args/0,
	 arg/1,
	 args/1,
	 is_arg/1,		% for linear_scan ra
	 call_clobbered/0,
	 tailcall_clobbered/0,
	 live_at_return/0
	 ]).

-include("../rtl/hipe_literals.hrl").

-define(G0, 0).
-define(G1, 1).
-define(G2, 2).
-define(G3, 3).
-define(G4, 4).
-define(G5, 5).
-define(G6, 6).
-define(G7, 7).
-define(O0, 8).
-define(O1, 9).
-define(O2, 10).
-define(O3, 11).
-define(O4, 12).
-define(O5, 13).
-define(O6, 14).
-define(O7, 15).
-define(L0, 16).
-define(L1, 17).
-define(L2, 18).
-define(L3, 19).
-define(L4, 20).
-define(L5, 21).
-define(L6, 22).
-define(L7, 23).
-define(I0, 24).
-define(I1, 25).
-define(I2, 26).
-define(I3, 27).
-define(I4, 28).
-define(I5, 29).
-define(I6, 30).
-define(I7, 31).
-define(LAST_PRECOLOURED,31).	% must handle both GRP and FPR ranges

-define(RA, ?O7).

-define(ARG0, ?O1).
-define(ARG1, ?O2).
-define(ARG2, ?O3).
-define(ARG3, ?O4).
-define(ARG4, ?O5).
-define(ARG5, ?O0).

-define(TEMP1, ?I3).	% stores RA around inc_stack calls, must be C calleE-save
-define(TEMP2, ?I4).
-define(TEMP3, ?I5).

-define(RETURN_VALUE, ?O0).
-define(HEAP_POINTER, ?I2).
-define(STACK_POINTER, ?I1).
-define(PROC_POINTER, ?I0).

reg_name_gpr(R) ->
  case R of
    ?G0 -> "%g0";
    ?G1 -> "%g1";
    ?G2 -> "%g2";
    ?G3 -> "%g3";
    ?G4 -> "%g4";
    ?G5 -> "%g5";
    ?G6 -> "%g6";
    ?G7 -> "%g7";
    ?O0 -> "%o0";
    ?O1 -> "%o1";
    ?O2 -> "%o2";
    ?O3 -> "%o3";
    ?O4 -> "%o4";
    ?O5 -> "%o5";
    ?O6 -> "%sp";
    ?O7 -> "%o7";
    ?L0 -> "%l0";
    ?L1 -> "%l1";
    ?L2 -> "%l2";
    ?L3 -> "%l3";
    ?L4 -> "%l4";
    ?L5 -> "%l5";
    ?L6 -> "%l6";
    ?L7 -> "%l7";
    ?I0 -> "%i0";
    ?I1 -> "%i1";
    ?I2 -> "%i2";
    ?I3 -> "%i3";
    ?I4 -> "%i4";
    ?I5 -> "%i5";
    ?I6 -> "%fp";
    ?I7 -> "%i7";
    %% to handle code before regalloc:
    _   -> "%r" ++ integer_to_list(R)
  end.

reg_name_fpr(R) -> [$f | integer_to_list(2*R)].

%%% Must handle both GPR and FPR ranges.
first_virtual() -> ?LAST_PRECOLOURED + 1.

%%% These two tests have the same implementation, but that's
%%% not something we should cast in stone in the interface. 
is_precoloured_gpr(R) -> R =< ?LAST_PRECOLOURED.
is_precoloured_fpr(R) -> R =< ?LAST_PRECOLOURED.

all_precoloured() ->
  %% <%g6, %g7, %o6, %i6> should be skipped as they are unused.
  %% Unfortunately, gaps in the list of precoloured registers
  %% cause the graph_color register allocator to create bogus
  %% assignments for those "registers", which in turn causes
  %% the "precoloured reg must map to itself" sanity check in
  %% the frame module to signal errors.
  [?G0, ?G1, ?G2, ?G3, ?G4, ?G5, ?G6, ?G7,
   ?O0, ?O1, ?O2, ?O3, ?O4, ?O5, ?O6, ?O7,
   ?L0, ?L1, ?L2, ?L3, ?L4, ?L5, ?L6, ?L7,
   ?I0, ?I1, ?I2, ?I3, ?I4, ?I5, ?I6, ?I7].

return_value() -> ?RETURN_VALUE.

temp1() -> ?TEMP1.
temp2() -> ?TEMP2.
temp3() -> ?TEMP3.

heap_pointer() -> ?HEAP_POINTER.

stack_pointer() -> ?STACK_POINTER.

proc_pointer() -> ?PROC_POINTER.

return_address() -> ?RA.

g0() -> ?G0.

allocatable_gpr() ->
  %% %g0 is not writable
  %% %g6, %g7, %o6, and %i6 are reserved for C
  %% %i0, %i1, and %i2 are fixed global registers
  %% %i4 may be used by the frame module for large load/store offsets
  [     ?G1, ?G2, ?G3, ?G4, ?G5,
   ?O0, ?O1, ?O2, ?O3, ?O4, ?O5,      ?O7,
   ?L0, ?L1, ?L2, ?L3, ?L4, ?L5, ?L6, ?L7,
                  ?I3,      ?I5,      ?I7].

allocatable_fpr() ->
  %% We expose 16 virtual fp regs, 0-15, corresponding to the
  %% f0/f2/f4/.../f28/f30 double-precision hardware fp regs.
  %% The mapping is done by reg_name_fpr/1 and the assembler.
  %% We ignore f32/.../f60 since they cannot be used in loads
  %% or stores for non 8-byte aligned addresses.
  [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15].

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
    ?G0 -> true;
    ?G6 -> true;
    ?G7 -> true;
    ?O6 -> true;
    ?I6 -> true;
    _ -> false
  end.

nr_args() -> ?SPARC_NR_ARG_REGS.

args(Arity) when is_integer(Arity) ->
  N = erlang:min(Arity, ?SPARC_NR_ARG_REGS),
  args(N-1, []).

args(I, Rest) when is_integer(I), I < 0 -> Rest;
args(I, Rest) -> args(I-1, [arg(I) | Rest]).

arg(N) ->
  if N < ?SPARC_NR_ARG_REGS ->
      case N of
	0 -> ?ARG0;
	1 -> ?ARG1;
	2 -> ?ARG2;
	3 -> ?ARG3;
	4 -> ?ARG4;
	5 -> ?ARG5
      end
  end.

is_arg(R) ->
  case R of
    ?ARG0 -> ?SPARC_NR_ARG_REGS > 0;
    ?ARG1 -> ?SPARC_NR_ARG_REGS > 1;
    ?ARG2 -> ?SPARC_NR_ARG_REGS > 2;
    ?ARG3 -> ?SPARC_NR_ARG_REGS > 3;
    ?ARG4 -> ?SPARC_NR_ARG_REGS > 4;
    ?ARG5 -> ?SPARC_NR_ARG_REGS > 5;
    _ -> false
  end.

%% Note: the fact that allocatable_gpr() is a subset of call_clobbered() is
%% hard-coded in hipe_sparc_defuse:insn_defs_all_gpr/1
call_clobbered() ->		% does the RA strip the type or not?
  [%% ?G0 is the non-allocatable constant zero
   {?G1,tagged},{?G1,untagged},
   {?G2,tagged},{?G2,untagged},
   {?G3,tagged},{?G3,untagged},
   {?G4,tagged},{?G4,untagged},
   {?G5,tagged},{?G5,untagged},
   %% ?G6 is reserved for C
   %% ?G7 is reserved for C
   {?O0,tagged},{?O0,untagged},
   {?O1,tagged},{?O1,untagged},
   {?O2,tagged},{?O2,untagged},
   {?O3,tagged},{?O3,untagged},
   {?O4,tagged},{?O4,untagged},
   {?O5,tagged},{?O5,untagged},
   %% ?O6 is reserved for C
   {?O7,tagged},{?O7,untagged},
   {?L0,tagged},{?L0,untagged},
   {?L1,tagged},{?L1,untagged},
   {?L2,tagged},{?L2,untagged},
   {?L3,tagged},{?L3,untagged},
   {?L4,tagged},{?L4,untagged},
   {?L5,tagged},{?L5,untagged},
   {?L6,tagged},{?L6,untagged},
   {?L7,tagged},{?L7,untagged},
   %% ?I0 is fixed (P)
   %% ?I1 is fixed (NSP)
   %% ?I2 is fixed (HP)
   {?I3,tagged},{?I3,untagged},
   {?I4,tagged},{?I4,untagged},
   {?I5,tagged},{?I5,untagged},
   %% ?I6 is reserved for C
   {?I7,tagged},{?I7,untagged}
  ].

tailcall_clobbered() ->		% tailcall crapola needs one temp
  [{?TEMP1,tagged},{?TEMP1,untagged}
  ,{?RA,tagged},{?RA,untagged}
  ].

live_at_return() ->
  [{?HEAP_POINTER,untagged},
   {?STACK_POINTER,untagged},
   {?PROC_POINTER,untagged}
  ].
