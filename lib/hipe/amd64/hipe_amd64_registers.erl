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

-module(hipe_amd64_registers).

-export([
 	 all_precoloured/0,
 	 allocatable/0,
	 allocatable_sse2/0,
	 allocatable_x87/0,
 	 arg/1,
         args/1,
         call_clobbered/0,
	 fcalls/0,
	 float_size/0,
	 first_virtual/0,
 	 heap_limit/0,
 	 is_arg/1,
 	 is_fixed/1,
 	 is_precoloured/1,
 	 is_precoloured_sse2/1,
	 is_precoloured_x87/1,
 	 live_at_return/0,
	 nr_args/0,
 	 proc_offset/1,
 	 proc_pointer/0,
 	 rax/0,
         rcx/0,
	 ret/1,
         sp/0,
         sp_limit_offset/0,
	 reg_name/1,
         alignment/0,
 	 tailcall_clobbered/0,
 	 temp0/0,
	 temp1/0,
	 sse2_temp0/0,
	 %% fixed/0,
	 wordsize/0
	]).

-include("../rtl/hipe_literals.hrl").

-ifdef(AMD64_HP_IN_REGISTER).
-export([heap_pointer/0]).
-endif.

-ifdef(AMD64_FCALLS_IN_REGISTER).
fcalls_offset() -> false.
-else.
fcalls_offset() -> ?P_FCALLS.
-define(AMD64_FCALLS_REGISTER,16).
-endif.

-ifdef(AMD64_HEAP_LIMIT_IN_REGISTER).
heap_limit_offset() -> false.
-else.
-define(AMD64_HEAP_LIMIT_REGISTER, 17).
heap_limit_offset() -> ?P_HP_LIMIT.
-endif.


-define(RAX,  0).
-define(RCX,  1).
-define(RDX,  2).
-define(RBX,  3).
-define(RSP,  4).
-define(RBP,  5).
-define(RSI,  6).
-define(RDI,  7).
-define(R8 ,  8).
-define(R9 ,  9).
-define(R10, 10).
-define(R11, 11).
-define(R12, 12).
-define(R13, 13).
-define(R14, 14).
-define(R15, 15).
-define(FCALLS,           ?AMD64_FCALLS_REGISTER).
-define(HEAP_LIMIT,       ?AMD64_HEAP_LIMIT_REGISTER).
-define(LAST_PRECOLOURED, 17).

-define(ARG0, ?RSI).
-define(ARG1, ?RDX).
-define(ARG2, ?RCX).
-define(ARG3, ?R8).
-define(ARG4, ?R9).
-define(ARG5, ?RDI).

-define(TEMP0, ?R14).
-define(TEMP1, ?R13).

-define(SSE2_TEMP0, 00).

-define(PROC_POINTER, ?RBP).

reg_name(R) ->
  case R of
    ?RAX -> "%rax";
    ?RCX -> "%rcx";
    ?RDX -> "%rdx";
    ?RBX -> "%rbx";
    ?RSP -> "%rsp";
    ?RBP -> "%rbp";
    ?RSI -> "%rsi";
    ?RDI -> "%rdi";
    ?FCALLS -> "%fcalls";
    ?HEAP_LIMIT -> "%hplim";
    Other -> "%r" ++ integer_to_list(Other)
  end.

alignment() -> 8.  

float_size() -> 8.  

first_virtual() -> ?LAST_PRECOLOURED + 1.

is_precoloured(X) -> X =< ?LAST_PRECOLOURED.

is_precoloured_sse2(X) -> X =< 15.

is_precoloured_x87(X) -> X =< 6.

all_precoloured() ->
  [?RAX,
   ?RCX,
   ?RDX,
   ?RBX,
   ?RSP,
   ?RBP,
   ?RSI,
   ?RDI,
   ?R8 ,
   ?R9 ,
   ?R10,
   ?R11,
   ?R12,
   ?R13,
   ?R14,
   ?R15,
   ?FCALLS,
   ?HEAP_LIMIT].

rax() -> ?RAX.
rcx() -> ?RCX.
temp0() -> ?TEMP0.
temp1() -> ?TEMP1.
sp() -> ?RSP.
proc_pointer() -> ?PROC_POINTER.
fcalls() -> ?FCALLS.
heap_limit() -> ?HEAP_LIMIT.


-ifdef(AMD64_HP_IN_REGISTER).
-define(HEAP_POINTER, ?AMD64_HEAP_POINTER).
heap_pointer() -> ?HEAP_POINTER.
-define(LIST_HP_LIVE_AT_RETURN,[{?HEAP_POINTER,untagged}]).
is_heap_pointer(?HEAP_POINTER) -> true;
is_heap_pointer(_) -> false.
%% -define(LIST_HP_FIXED,[?HEAP_POINTER]).

-else.
-define(HEAP_POINTER, -1).
is_heap_pointer(_) -> false.
%% -define(LIST_HP_FIXED,[]).
-define(LIST_HP_LIVE_AT_RETURN,[]).
-endif.

proc_offset(?FCALLS) -> fcalls_offset();
proc_offset(?HEAP_LIMIT) -> heap_limit_offset();
proc_offset(_) -> false.

sp_limit_offset() -> ?P_NSP_LIMIT.

is_fixed(?RSP) -> true;
is_fixed(?PROC_POINTER) -> true;
is_fixed(?FCALLS) -> true;
is_fixed(?HEAP_LIMIT) -> true;
is_fixed(R) -> is_heap_pointer(R).

%% fixed() ->
%%     [?ESP, ?PROC_POINTER, ?FCALLS, ?HEAP_LIMIT | ?LIST_HP_FIXED].

allocatable() ->
  [?RDX, ?RCX, ?RBX, ?RAX, ?RSI, ?RDI,
   ?R8 , ?R9 , ?R10, ?R11, ?R12, ?R13, ?R14, ?R15]
    -- [?FCALLS, ?HEAP_POINTER, ?HEAP_LIMIT].

allocatable_sse2() ->
  [00,01,02,03,04,05,06,07,08,09,10,11,12,13,14,15]. %% xmm0 - xmm15

sse2_temp0() -> ?SSE2_TEMP0.

allocatable_x87() ->
  [0,1,2,3,4,5,6].

nr_args() -> ?AMD64_NR_ARG_REGS.

arg(N) when N < ?AMD64_NR_ARG_REGS ->
  case N of
    0 -> ?ARG0;
    1 -> ?ARG1;
    2 -> ?ARG2;
    3 -> ?ARG3;
    4 -> ?ARG4;
    5 -> ?ARG5
  end.

is_arg(R) ->
  case R of
    ?ARG0 -> ?AMD64_NR_ARG_REGS > 0;
    ?ARG1 -> ?AMD64_NR_ARG_REGS > 1;
    ?ARG2 -> ?AMD64_NR_ARG_REGS > 2;
    ?ARG3 -> ?AMD64_NR_ARG_REGS > 3;
    ?ARG4 -> ?AMD64_NR_ARG_REGS > 4;
    ?ARG5 -> ?AMD64_NR_ARG_REGS > 5;
    _ -> false
  end.

args(Arity) when is_integer(Arity), Arity >= 0 ->
  N = erlang:min(Arity, ?AMD64_NR_ARG_REGS),
  args(N-1, []).

args(I, Rest) when I < 0 -> Rest;
args(I, Rest) -> args(I-1, [arg(I) | Rest]).

ret(0) -> ?RAX.

%% Note: the fact that (allocatable() UNION allocatable_x87() UNION
%% allocatable_sse2()) is a subset of call_clobbered() is hard-coded in
%% hipe_x86_defuse:insn_defs_all/1
call_clobbered() ->
  [{?RAX,tagged},{?RAX,untagged},	% does the RA strip the type or not?
   {?RDX,tagged},{?RDX,untagged},
   {?RCX,tagged},{?RCX,untagged},
   {?RBX,tagged},{?RBX,untagged},
   {?RDI,tagged},{?RDI,untagged},
   {?RSI,tagged},{?RSI,untagged},
   {?R8 ,tagged},{?R8 ,untagged},
   {?R9 ,tagged},{?R9 ,untagged},
   {?R10,tagged},{?R10,untagged},
   {?R11,tagged},{?R11,untagged},
   {?R12,tagged},{?R12,untagged},
   {?R13,tagged},{?R13,untagged},
   {?R14,tagged},{?R14,untagged},
   {?R15,tagged},{?R15,untagged}
   | fp_call_clobbered()]
    --
    [{?FCALLS,tagged},{?FCALLS,untagged},
     {?HEAP_POINTER,tagged},{?HEAP_POINTER,untagged},
     {?HEAP_LIMIT,tagged},{?HEAP_LIMIT,untagged}
    ].

fp_call_clobbered() -> %% sse2 since it has more registers than x87
  [{Reg,double} || Reg <- allocatable_sse2()].

tailcall_clobbered() ->		% tailcall crapola needs two temps
  [{?TEMP0,tagged},{?TEMP0,untagged},
   {?TEMP1,tagged},{?TEMP1,untagged}
  | fp_call_clobbered()].

live_at_return() ->
    [{?RSP,untagged}
     ,{?PROC_POINTER,untagged}
     ,{?FCALLS,untagged}
     ,{?HEAP_LIMIT,untagged}
     | ?LIST_HP_LIVE_AT_RETURN
    ].

wordsize() -> 8.
