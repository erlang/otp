%%%
%%% %CopyrightBegin%
%%% 
%%% Copyright Ericsson AB 2001-2016. All Rights Reserved.
%%% 
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%% 
%%% %CopyrightEnd%
%%%
%%%
%%% TODO:
%%% - Do we need a pseudo reg for the condition codes?

-module(hipe_x86_registers).

-export([reg_name/1,
	 first_virtual/0,
	 is_precoloured/1,
	 is_precoloured_x87/1,
	 all_precoloured/0,
	 eax/0,
	 ecx/0,
	 temp0/0,
	 temp1/0,
	 sp/0,
	 proc_pointer/0,
	 heap_limit/0,
	 fcalls/0,
	 proc_offset/1,
	 sp_limit_offset/0,
	 is_fixed/1,
	 %% fixed/0,
	 allocatable/0,
	 allocatable_x87/0,
	 nr_args/0,
	 arg/1,
	 is_arg/1,
	 args/1,
	 nr_rets/0,
	 ret/1,
	 call_clobbered/0,
	 tailcall_clobbered/0,
	 live_at_return/0,
	 float_size/0,
	 wordsize/0,
	 alignment/0]).

-include("../rtl/hipe_literals.hrl").

-ifdef(X86_HP_IN_ESI).
-export([heap_pointer/0]).
-endif.

-define(EAX, 0).
-define(ECX, 1).
-define(EDX, 2).
-define(EBX, 3).
-define(ESP, 4).
-define(EBP, 5).
-define(ESI, 6).
-define(EDI, 7).
-define(FCALLS, 8).		% proc field alias
-define(HEAP_LIMIT, 9).		% proc field alias
-define(LAST_PRECOLOURED, 9).

-define(ARG0, ?EAX).
-define(ARG1, ?EDX).
-define(ARG2, ?ECX).
-define(ARG3, ?EBX).
-define(ARG4, ?EDI).

-define(RET0, ?EAX).
-define(RET1, ?EDX).
-define(RET2, ?ECX).
-define(RET3, ?EBX).
-define(RET4, ?EDI).

-define(TEMP0, ?EBX).	% XXX: was EAX
-define(TEMP1, ?EDI).	% XXX: was EDX then EDI

-define(PROC_POINTER, ?EBP).

reg_name(R) ->
    case R of
	?EAX -> "%eax";
	?ECX -> "%ecx";
	?EDX -> "%edx";
	?EBX -> "%ebx";
	?ESP -> "%esp";
	?EBP -> "%ebp";
	?ESI -> "%esi";
	?EDI -> "%edi";
	?FCALLS -> "%fcalls";
	?HEAP_LIMIT -> "%hplim";
	Other -> "%r" ++ integer_to_list(Other)
    end.

first_virtual() -> ?LAST_PRECOLOURED + 1.

is_precoloured(X) -> X =< ?LAST_PRECOLOURED.

is_precoloured_x87(X) -> X =< 6.

all_precoloured() ->
    [?EAX,
     ?ECX,
     ?EDX,
     ?EBX,
     ?ESP,
     ?EBP,
     ?ESI,
     ?EDI,
     ?FCALLS,
     ?HEAP_LIMIT].

eax() -> ?EAX.
ecx() -> ?ECX.
temp0() -> ?TEMP0.
temp1() -> ?TEMP1.
sp() -> ?ESP.
proc_pointer() -> ?PROC_POINTER.
fcalls() -> ?FCALLS.
heap_limit() -> ?HEAP_LIMIT.

-ifdef(X86_HP_IN_ESI).
-define(ESI_IS_FIXED,1).
-define(HEAP_POINTER, ?ESI).
heap_pointer() -> ?HEAP_POINTER.
is_heap_pointer(?HEAP_POINTER) -> true;
is_heap_pointer(_) -> false.
-define(LIST_HP_FIXED,[?HEAP_POINTER]).
-define(LIST_HP_LIVE_AT_RETURN,[{?HEAP_POINTER,untagged}]).
-else.
is_heap_pointer(_) -> false.
-define(LIST_HP_FIXED,[]).
-define(LIST_HP_LIVE_AT_RETURN,[]).
-endif.

-ifdef(ESI_IS_FIXED).
-define(LIST_ESI_ALLOCATABLE,[]).
-define(LIST_ESI_CALL_CLOBBERED,[]).
-else.
-define(LIST_ESI_ALLOCATABLE,[?ESI]).
-define(LIST_ESI_CALL_CLOBBERED,[{?ESI,tagged},{?ESI,untagged}]).
-endif.

proc_offset(?FCALLS) -> ?P_FCALLS;
proc_offset(?HEAP_LIMIT) -> ?P_HP_LIMIT;
proc_offset(_) -> false.

sp_limit_offset() -> ?P_NSP_LIMIT.

is_fixed(?ESP) -> true;
is_fixed(?PROC_POINTER) -> true;
is_fixed(?FCALLS) -> true;
is_fixed(?HEAP_LIMIT) -> true;
is_fixed(R) -> is_heap_pointer(R).

%% fixed() ->
%%     [?ESP, ?PROC_POINTER, ?FCALLS, ?HEAP_LIMIT | ?LIST_HP_FIXED].

allocatable() ->
    [?EDX, ?ECX, ?EBX, ?EAX, ?EDI| ?LIST_ESI_ALLOCATABLE].

allocatable_x87() ->
    [0,1,2,3,4,5,6].

nr_args() -> ?X86_NR_ARG_REGS.

arg(N) ->
    if N < ?X86_NR_ARG_REGS ->
	    case N of
		0 -> ?ARG0;
		1 -> ?ARG1;
		2 -> ?ARG2;
		3 -> ?ARG3;
		4 -> ?ARG4;
		_ -> exit({?MODULE, arg, N})
	    end;
       true ->
	    exit({?MODULE, arg, N})
    end.

is_arg(R) ->
    case R of
	?ARG0 -> ?X86_NR_ARG_REGS > 0;
	?ARG1 -> ?X86_NR_ARG_REGS > 1;
	?ARG2 -> ?X86_NR_ARG_REGS > 2;
	?ARG3 -> ?X86_NR_ARG_REGS > 3;
	?ARG4 -> ?X86_NR_ARG_REGS > 4;
	_ -> false
    end.

args(Arity) when is_integer(Arity), Arity >= 0 ->
    N = erlang:min(Arity, ?X86_NR_ARG_REGS),
    args(N-1, []).

args(I, Rest) when I < 0 -> Rest;
args(I, Rest) -> args(I-1, [arg(I) | Rest]).

nr_rets() -> ?X86_NR_RET_REGS.

ret(N) ->
    if N < ?X86_NR_RET_REGS ->
	    case N of
		0 -> ?RET0;
		1 -> ?RET1;
		2 -> ?RET2;
		3 -> ?RET3;
		4 -> ?RET4;
		_ -> exit({?MODULE, ret, N})
	    end;
       true ->
	    exit({?MODULE, ret, N})
    end.

call_clobbered() ->
    [{?EAX,tagged},{?EAX,untagged},	% does the RA strip the type or not?
     {?EDX,tagged},{?EDX,untagged},
     {?ECX,tagged},{?ECX,untagged},
     {?EBX,tagged},{?EBX,untagged},
     {?EDI,tagged},{?EDI,untagged}
     | ?LIST_ESI_CALL_CLOBBERED] ++ all_x87_pseudos().

tailcall_clobbered() ->		% tailcall crapola needs two temps
    [{?TEMP0,tagged},{?TEMP0,untagged},
     {?TEMP1,tagged},{?TEMP1,untagged}] ++ all_x87_pseudos().

all_x87_pseudos() ->
  [{0,double}, {1,double}, {2,double}, {3,double},
   {4,double}, {5,double}, {6,double}].

live_at_return() ->
    [{?ESP,untagged}
     ,{?PROC_POINTER,untagged}
     ,{?FCALLS,untagged}
     ,{?HEAP_LIMIT,untagged}
     | ?LIST_HP_LIVE_AT_RETURN
    ].

alignment() -> 4.

float_size() -> 8.

wordsize() -> 4.
