%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2005-2009. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%

-module(hipe_arm_main).
-export([rtl_to_arm/3]).

rtl_to_arm(MFA, RTL, Options) ->
  Defun1 = hipe_rtl_to_arm:translate(RTL),
  %% io:format("~w: after translate\n", [?MODULE]),
  %% hipe_arm_pp:pp(Defun1),
  Defun2 = hipe_arm_ra:ra(Defun1, Options),
  %% io:format("~w: after regalloc\n", [?MODULE]),
  %% hipe_arm_pp:pp(Defun2),
  Defun3 = hipe_arm_frame:frame(Defun2),
  %% io:format("~w: after frame\n", [?MODULE]),
  %% hipe_arm_pp:pp(Defun3),
  Defun4 = hipe_arm_finalise:finalise(Defun3),
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
