%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2016. All Rights Reserved.
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

-module(hipe_ppc_main).
-export([rtl_to_ppc/3]).

rtl_to_ppc(MFA, RTL, Options) ->
  PPC1 = hipe_rtl_to_ppc:translate(RTL),
  PPC2 = hipe_ppc_ra:ra(PPC1, Options),
  PPC3 = hipe_ppc_frame:frame(PPC2),
  PPC4 = hipe_ppc_finalise:finalise(PPC3),
  ppc_pp(PPC4, MFA, Options),
  {native, powerpc, {unprofiled, PPC4}}.

ppc_pp(PPC, MFA, Options) ->
  case proplists:get_value(pp_native, Options) of
    true ->
      hipe_ppc_pp:pp(PPC);
    {only,Lst} when is_list(Lst) ->
      case lists:member(MFA,Lst) of
	true ->
	  hipe_ppc_pp:pp(PPC);
	false ->
	  ok
      end;
    {only,MFA} ->
       hipe_ppc_pp:pp(PPC);
    {file,FileName} ->
      {ok, File} = file:open(FileName, [write,append]),
      hipe_ppc_pp:pp(File, PPC),
      ok = file:close(File);
    _ ->
      ok
  end.
