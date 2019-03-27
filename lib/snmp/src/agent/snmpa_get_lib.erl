%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2019-2019. All Rights Reserved.
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

%%
%% Note that most of these functions *assume* that they are executed
%% by the agent. If they are not they may note work as they require
%% some properties to be set in the process dictionary!
%%

-module(snmpa_get_lib).

-export([
         split_vbs/1, split_vbs/3,
         split_vbs_view/2,
         split_vbs_gb/2,

         agent_sort_vbs/1,
         oid_sort_vbs/1, org_index_sort_vbs/1,

         sa_split/1,

         delete_prefixes/2,

         dbg_apply/3,

         user_err/2
        ]).

-define(VMODULE,"GET-LIB").
-include("snmpa_internal.hrl").
-include("snmp_types.hrl").
-include("snmp_debug.hrl").
-include("snmp_verbosity.hrl").




%%-----------------------------------------------------------------
%% split_vbs/1,3
%%
%% Splits the list of varbinds (basically) into two lists. One
%% of 'end of'-varbinds (mib view and tables) and then the rest
%% of the varbinds.
%%-----------------------------------------------------------------

-spec split_vbs(VBs :: [snmp:varbind()]) ->
                       {ResVBs   :: [snmp:varbind()], 
                        EndOfVBs :: [snmp:varbind()]}.

split_vbs(VBs) ->
    split_vbs(VBs, [], []).

-spec split_vbs(VBs    :: [snmp:varbind()],
                Res    :: [snmp:varbind()],
                EndOfs :: [snmp:varbind()]) ->
                       {ResVBs   :: [snmp:varbind()],
                        EndOfVBs :: [snmp:varbind()]}.

split_vbs([], ResVBs, EndOfVBs) ->
    {ResVBs, EndOfVBs};
split_vbs([VB | VBs], Res, EndOfs) ->
    case VB#varbind.value of
        {endOfMibView, _} -> split_vbs(VBs, Res, [VB | EndOfs]);
        {endOfTable, _}   -> split_vbs(VBs, Res, [VB | EndOfs]);
        _                 -> split_vbs(VBs, [VB | Res], EndOfs)
    end.



%%-----------------------------------------------------------------
%% split_vbs_view/2
%%
%% Splits a list of varbinds into two lists based on the provided
%% MibView. One list of varbinds inside the MibView and one of
%% varbinds outside the MibView.
%%-----------------------------------------------------------------

-spec split_vbs_view(VBs     :: [snmp:varbind()],
                     MibView :: snmp_view_based_acm_mib:mibview()) ->
                            {OutSideView :: [snmp:varbind()],
                             InSideView  :: [snmp:varbind()]}.

split_vbs_view(VBs, MibView) ->
    ?vtrace("split the varbinds view", []),
    split_vbs_view(VBs, MibView, [], []).

split_vbs_view([], _MibView, Out, In) ->
    {Out, In};
split_vbs_view([VB | VBs], MibView, Out, In) ->
    case snmpa_acm:validate_mib_view(VB#varbind.oid, MibView) of
	true ->
            split_vbs_view(VBs, MibView, Out, [VB | In]);
	false ->
            VB2 = VB#varbind{value = noSuchObject},
            split_vbs_view(VBs, MibView, [VB2 | Out], In)
    end.



%%-----------------------------------------------------------------
%% split_vbs_gb/2
%%
%% Performs a get-bulk split of the varbinds
%%-----------------------------------------------------------------

-spec split_vbs_gb(NonRepeaters :: integer(),
                   VBs          :: [snmp:varbind()]) ->
                          {NonRepVBs :: [snmp:varbind()], 
                           RestVBs   :: [snmp:varbind()]}.

split_vbs_gb(N, VBs) ->
    split_vbs_gb(N, VBs, []).

split_vbs_gb(N, Varbinds, Res) when N =< 0 ->
    {Res, Varbinds};
split_vbs_gb(N, [H | T], Res) ->
    split_vbs_gb(N-1, T, [H | Res]);
split_vbs_gb(_N, [], Res) ->
    {Res, []}.



%%-----------------------------------------------------------------
%% agent_sort_vbs/1
%%
%% Sorts the varbinds into two categories. The first is varbinds
%% belonging to "our" agent and the other is varbinds for 
%% subagents.
%%-----------------------------------------------------------------

-spec agent_sort_vbs(VBs :: [snmp:varbind()]) ->
                      {AgentVBs    :: [snmp:varbind()],
                       SubAgentVBs :: [snmp:varbind()]}.

agent_sort_vbs(VBs) ->
    snmpa_svbl:sort_varbindlist(get(mibserver), VBs).


%%-----------------------------------------------------------------
%% oid_sort_vbs/1
%%
%% Sorts the varbinds based on their oid.
%%-----------------------------------------------------------------

-spec oid_sort_vbs(VBs :: [snmp:varbind()]) -> SortedVBs :: [snmp:varbind()].

oid_sort_vbs(VBs) ->
    lists:keysort(#varbind.oid, VBs).


%%-----------------------------------------------------------------
%% org_index_sort_vbs/1
%%
%% Sorts the varbinds based on their org_index.
%%-----------------------------------------------------------------

-spec org_index_sort_vbs(VBs :: [snmp:varbind()]) -> SortedVBs :: [snmp:varbind()].

org_index_sort_vbs(Vbs) ->
    lists:keysort(#varbind.org_index, Vbs).



%%-----------------------------------------------------------------
%% sa_split/1
%%
%% Splits a list of {oid(), varbind()} into two lists of oid() 
%% and varbind. The resulting lists are reversed!
%%-----------------------------------------------------------------

-spec sa_split(SAVBs :: [{SAOid :: snmp:oid(), snmp:varbind()}]) ->
                      {Oids :: [snmp:oid()], VBs :: [snmp:varbind()]}.

sa_split(SAVBs) ->
    snmpa_svbl:sa_split(SAVBs).



%%-----------------------------------------------------------------
%% delete_prefixes/2
%%
%% Takes an Oid prefix and a list of ivarbinds and produces a list
%% of {ShortOid, ASN1Type}. The ShortOid is basically the oid with 
%% the OidPrefix removed.
%%-----------------------------------------------------------------

-spec delete_prefixes(OidPrefix :: snmp:oid(),
                      VBs       :: [snmp:ivarbind()]) ->
                             [{ShortOid :: snmp:oid(),
                               ASN1Type :: snmp:asn1_type()}].

delete_prefixes(OidPrefix, IVBs) ->
    [{snmp_misc:diff(Oid, OidPrefix), ME#me.asn1_type} ||
        #ivarbind{varbind = #varbind{oid = Oid}, mibentry = ME} <- IVBs].



%%-----------------------------------------------------------------
%% dbg_apply/3
%%
%% Call instrumentation functions, but allow for debug printing
%% of useful debug info.
%%-----------------------------------------------------------------

-spec dbg_apply(M :: atom(), F :: atom(), A :: list()) ->
                       any().

dbg_apply(M, F, A) ->
    case get(verbosity) of
	silence -> 
	    apply(M,F,A);
	_ ->
	    ?vlog("~n   apply: ~w, ~w, ~p~n", [M,F,A]),
	    Res = (catch apply(M,F,A)),
	    case Res of
		{'EXIT', Reason} ->
		    ?vinfo("Call to: "
			   "~n   Module:   ~p"
			   "~n   Function: ~p"
			   "~n   Args:     ~p"
			   "~n"
			   "~nresulted in an exit"
			   "~n"
			   "~n   ~p~n", [M, F, A, Reason]);
		_ ->
		    ?vlog("~n   returned: ~p~n", [Res])
	    end,
	    Res
    end.


%% ---------------------------------------------------------------------

user_err(F, A) ->
    snmpa_error:user_err(F, A).


