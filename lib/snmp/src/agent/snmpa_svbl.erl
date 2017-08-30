%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2016. All Rights Reserved.
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
-module(snmpa_svbl).

-include("snmp_types.hrl").

-define(VMODULE,"SVBL").
-include("snmp_verbosity.hrl").

-export([sort_varbindlist/2, sort_varbinds_rows/1, sa_split/1,
	 delete_org_index/1, col_to_orgindex/2]).

%%-----------------------------------------------------------------
%% Func: sort_varbindlist/2
%% Args: Varbinds is a list of #varbind
%% Purpose: Group all variablebindings that corresponds to logically
%%          the same entity, i.e. group all plain variables, all
%%          table operations for each table, all varbinds to each
%%          subagent.
%% Returns: {VarbindsForThisAgent
%%           VarbindsForSubAgents}   where
%%             VarbindsForThisAgent  = List of {TableOid, List of #ivarbinds} |
%%                                              #ivarbinds
%%             VarbindsForSubAgents = List of {SubAgentPid,
%%                                             List of {SAOid, #varbinds}}
%%-----------------------------------------------------------------
sort_varbindlist(Mib, Varbinds) ->
    {Vars, Tabs, Subagents} = partition(Mib, Varbinds),
    {lists:append(Tabs, Vars), Subagents}.

partition(Mib, Vbs) ->
    partition(Mib, Vbs, [], [], []).
partition(Mib, [Varbind | Vbs], Vars, Tabs, Subs) ->
    #varbind{oid = Oid} = Varbind,
    ?vtrace("partition -> Oid: ~p", [Oid]),
    case snmpa_mib:lookup(Mib, Oid) of
	{table_column, MibEntry, TableOid} ->
	    IVarbind = #ivarbind{varbind = fix_bits(Varbind, MibEntry),
				 mibentry = MibEntry},
	    NewTabs = insert_key(TableOid, IVarbind, Tabs),
	    partition(Mib, Vbs, Vars, NewTabs, Subs);
	{subagent, SubagentPid, SAOid} ->
	    NewSubs = insert_key(SubagentPid, {SAOid, Varbind}, Subs),
	    partition(Mib, Vbs, Vars, Tabs, NewSubs);
	{variable, MibEntry} ->
	    IVarbind = #ivarbind{varbind = fix_bits(Varbind, MibEntry),
				 mibentry = MibEntry},
	    partition(Mib, Vbs, [IVarbind | Vars], Tabs, Subs);
	{false, ErrorCode} -> % ErrorCode = noSuchObject | noSuchInstance
	    IVarbind = #ivarbind{status = ErrorCode, varbind = Varbind},
	    partition(Mib, Vbs, [IVarbind | Vars], Tabs, Subs)
    end;
partition(_Mib, [], Vars, Subs, Tabs) ->
    {Vars, Subs, Tabs}.

% fix_bits(#varbind{bertype      = 'BITS', 
% 		  variabletype = 'OCTET STRING', 
% 		  value        = V} = VarBind, #me{asn1_type = A}) ->
%     VarBind#varbind{variabletype = 'BITS',
% 		    value = snmp_pdus:octet_str_to_bits(V)};
fix_bits(VarBind, #me{asn1_type=A})
  when ((A#asn1_type.bertype =:= 'BITS') andalso 
	(VarBind#varbind.variabletype =:= 'OCTET STRING') andalso 
	is_list(VarBind#varbind.value)) ->
    VarBind#varbind{variabletype = 'BITS',
 		    value = snmp_pdus:octet_str_to_bits(VarBind#varbind.value)};
fix_bits(Vb,_me) -> Vb.

insert_key(Key, Value, [{Key, Values} | Rest]) ->
    [{Key, [Value | Values]} | Rest];
insert_key(Key, Value, [{KeyX, Values} | Rest]) ->
    [{KeyX, Values} | insert_key(Key, Value, Rest)];
insert_key(Key, Value, []) ->
    [{Key, [Value]}].

%%-----------------------------------------------------------------
%% Tranforms a list of {Oid, Vb} to a 2-tuple with all
%% Oids and all Vbs. These lists will be reversed.
%%-----------------------------------------------------------------
sa_split(Vbs) -> sa_split(Vbs, [], []).
sa_split([{SAOid, Vb} | T], Oids, Vbs) ->
    sa_split(T, [SAOid | Oids], [Vb | Vbs]);
sa_split([], Oids, Vbs) ->
    {Oids, Vbs}.

%%-----------------------------------------------------------------
%% Func: sort_varbinds_rows/1
%% Args: Varbinds is a list of {Oid, Value}.
%% Pre: Varbinds is for one table.
%% Purpose: Sorts all varbinds in Oid order, and in row order.
%% Returns: list of Row where
%%          Row = {Indexes, List of Col} and
%%          Col = {ColNo, Value, OrgIndex} and
%%          OrgIndex is index in original varbind list.
%%-----------------------------------------------------------------
sort_varbinds_rows(Varbinds) ->
    P = pack(Varbinds),
    S = lists:keysort(1, P),
    unpack(S).

%% In: list of {Oid, Value}
%% Out: list of {{Indexes_for_row, Col}, Val, Index}
pack(V) -> pack(1, V).
pack(Index, [{[Col | Rest], Val} | T]) -> 
    [{{Rest, Col}, Val, Index} | pack(Index+1, T)];
pack(_, []) -> [].

unpack([{{Rest, Col}, Val, Index} | T]) ->
    unpack(Rest, [[{Col, Val, Index}]], T);
unpack([]) -> [].

unpack(Rest, [Row | Rows], [{{Rest, Col}, Val, Index} | T]) ->
    unpack(Rest, [[{Col, Val, Index} | Row] | Rows], T);
unpack(Rest, [Row | Rows], [{{Rest2, Col}, Val, Index} | T]) ->
    unpack(Rest2, [[{Col, Val, Index}], 
		   {Rest, lists:reverse(Row)} | Rows], T);
unpack(Rest, [Row | Rows], []) ->
    NewRow = {Rest, lists:reverse(Row)},
    lists:reverse([NewRow | Rows]).

%% OrgIndex should not be present when we call the is_set_ok/set/undo
%% table functions. They just see the list of cols, and if an error
%% occurs, they return the column nunber.
%% Also, delete duplicate columns.  If a SET is performed with duplicate
%% columns, it is undefined which column to use.  We just pick one.
delete_org_index([{RowIndex, Cols} | Rows]) ->
    [{RowIndex, doi(Cols)} | delete_org_index(Rows)];
delete_org_index([]) -> [].

doi([{Col, Val, OrgIndex}, {Col, _Val, _OrgIndex} | T]) ->
    doi([{Col, Val, OrgIndex} | T]);
doi([{Col, Val, _OrgIndex} | T]) ->
    [{Col, Val} | doi(T)];
doi([]) -> [].

%% Maps the column number to OrgIndex.
col_to_orgindex(0, _) -> 0;
col_to_orgindex(Col, [{Col, _Val, OrgIndex}|_]) ->
    OrgIndex;
col_to_orgindex(Col, [_|Cols]) ->
    col_to_orgindex(Col, Cols);
col_to_orgindex(BadCol, _) ->
    {false, BadCol}.
