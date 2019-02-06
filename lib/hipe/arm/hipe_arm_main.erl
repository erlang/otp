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

-module(hipe_arm_main).
-export([rtl_to_arm/3]).

rtl_to_arm(MFA, RTL, Options) ->
  Defun1 = hipe_rtl_to_arm:translate(RTL),
  CFG1 = hipe_arm_cfg:init(Defun1),
  %% io:format("~w: after translate\n", [?MODULE]),
  %% hipe_arm_pp:pp(Defun1),
  CFG2 = hipe_arm_ra:ra(CFG1, Options),
  %% io:format("~w: after regalloc\n", [?MODULE]),
  %% hipe_arm_pp:pp(hipe_arm_cfg:linearise(CFG2)),
  CFG3 = hipe_arm_frame:frame(CFG2),
  Defun3 = hipe_arm_cfg:linearise(CFG3),
  %% io:format("~w: after frame\n", [?MODULE]),
  %% hipe_arm_pp:pp(Defun3),
  Defun4 = hipe_arm_finalise:finalise(Defun3, Options),
  %% io:format("~w: after finalise\n", [?MODULE]),
  pp(Defun4, MFA, Options),
  {native, arm, {unprofiled, Defun4}}.

pp(Defun, MFA, Options) ->
  case proplists:get_value(pp_native, Options) of
    true ->
      hipe_arm_pp:pp(Defun);
    {only,Lst} when is_list(Lst) ->
      case lists:member(MFA,Lst) of
	true ->
	  hipe_arm_pp:pp(Defun);
	false ->
	  ok
      end;
    {only,MFA} ->
       hipe_arm_pp:pp(Defun);
    {file,FileName} ->
      {ok, File} = file:open(FileName, [write,append]),
      hipe_arm_pp:pp(File, Defun),
      ok = file:close(File);
    _ ->
      ok
  end.
