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
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% ICODE LIVENESS ANALYSIS
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_icode_liveness).

-define(PRETTY_PRINT, true).

-include("hipe_icode.hrl").
-include("../flow/liveness.inc").

%%--------------------------------------------------------------------
%% Interface to CFG and icode.
%%--------------------------------------------------------------------

cfg_bb(CFG, L) ->
  hipe_icode_cfg:bb(CFG, L).

cfg_postorder(CFG) ->
  hipe_icode_cfg:postorder(CFG).

cfg_succ(CFG, L) ->
  hipe_icode_cfg:succ(CFG, L).

uses(Instr) ->
  hipe_icode:uses(Instr).

defines(Instr) ->
  hipe_icode:defines(Instr).

%%
%% This is the list of registers that are live at exit from a function
%%
cfg_labels(CFG) ->
  hipe_icode_cfg:labels(CFG).

liveout_no_succ() ->
  ordsets:new().

pp_liveness_info(LiveList) ->
 print_live_list(LiveList).

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

pp_block(Label, CFG) ->
  BB = hipe_icode_cfg:bb(CFG, Label),
  Code = hipe_bb:code(BB),
  hipe_icode_pp:pp_block(Code).

print_var(#icode_variable{name=V, kind=Kind, annotation=T}) ->
  case Kind of
    var -> io:format("v~p", [V]);
    reg -> io:format("r~p", [V]);
    reg_gcsafe -> io:format("rs~p", [V]);
    fvar -> io:format("fv~p", [V])
  end,
  case T of
    [] -> ok;
    {_,X,F} -> io:format(" (~s)", F(X))
  end.
 
%%
%% The following are used only if annotation of the code is requested.
%%
-ifdef(DEBUG_LIVENESS).
cfg_bb_add(CFG, L, NewBB) ->
  hipe_icode_cfg:bb_add(CFG, L, NewBB).

mk_comment(Text) ->
  hipe_icode:mk_comment(Text).
-endif.
