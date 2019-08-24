%% -*- mode: erlang; erlang-indent-level: 2 -*-
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
-module(hipe_rtl_verify_gcsafe).

-export([check/1]).

-include("../flow/cfg.hrl").              %% needed for the specs
-include("hipe_rtl.hrl").

check(CFG) ->
  Liveness = hipe_rtl_liveness:analyze(CFG),
  put({?MODULE, 'fun'}, CFG#cfg.info#cfg_info.'fun'),
  lists:foreach(
    fun(Lb) ->
        put({?MODULE, label}, Lb),
        Liveout = hipe_rtl_liveness:liveout(Liveness, Lb),
        BB = hipe_rtl_cfg:bb(CFG, Lb),
        check_instrs(lists:reverse(hipe_bb:code(BB)), Liveout)
    end, hipe_rtl_cfg:labels(CFG)),
  erase({?MODULE, 'fun'}),
  erase({?MODULE, label}),
  erase({?MODULE, instr}),
  ok.

check_instrs([], _Livein) -> ok;
check_instrs([I|Is], LiveOut) ->
  Def = ordsets:from_list(hipe_rtl:defines(I)),
  Use = ordsets:from_list(hipe_rtl:uses(I)),
  LiveOver = ordsets:subtract(LiveOut, Def),
  LiveIn = ordsets:union(LiveOver, Use),
  case (hipe_rtl:is_call(I)
        andalso not safe_primop(hipe_rtl:call_fun(I)))
    orelse is_record(I, gctest)
  of
    false -> ok;
    true ->
      put({?MODULE, instr}, I),
      lists:foreach(fun verify_live/1, LiveOver)
  end,
  check_instrs(Is, LiveIn).

verify_live(T) ->
  case hipe_rtl:is_reg(T) of
    false -> ok;
    true ->
      case hipe_rtl:reg_is_gcsafe(T) of
        true -> ok;
        false ->
          error({gcunsafe_live_over_call,
                 get({?MODULE, 'fun'}),
                 {label, get({?MODULE, label})},
                 get({?MODULE, instr}),
                 T})
      end
  end.

%% Primops that can't gc
%% Note: This information is essentially duplicated from hipe_bif_list.m4
safe_primop(is_divisible) -> true;
safe_primop(is_unicode) -> true;
safe_primop(cmp_2) -> true;
safe_primop(eq_2) -> true;
safe_primop(bs_allocate) -> true;
safe_primop(bs_reallocate) -> true;
safe_primop(bs_utf8_size) -> true;
safe_primop(bs_get_utf8) -> true;
safe_primop(bs_put_utf8) -> true;
safe_primop(bs_utf16_size) -> true;
safe_primop(bs_get_utf16) -> true;
safe_primop(bs_validate_unicode_retract) -> true;
safe_primop(bs_put_small_float) -> true;
safe_primop(bs_put_bits) -> true;
safe_primop(emasculate_binary) -> true;
safe_primop(atomic_inc) -> true;
%% Not noproc but manually verified
safe_primop(bs_put_big_integer) -> true;
safe_primop(_) -> false.
