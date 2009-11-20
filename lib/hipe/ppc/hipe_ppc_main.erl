%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2009. All Rights Reserved.
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
