%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2015. All Rights Reserved.
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
-module(snmpa_vacm).

-export([get_mib_view/5]).
-export([init/1, init/2, backup/1]).
-export([delete/1, get_row/1, get_next_row/1, insert/1, insert/2,
	 cleanup/0, dump_table/0]).

-include("SNMPv2-TC.hrl").
-include("SNMP-VIEW-BASED-ACM-MIB.hrl").
-include("SNMP-FRAMEWORK-MIB.hrl").
-include("snmp_types.hrl").
-include("snmpa_vacm.hrl").

-define(VMODULE,"VACM").
-include("snmp_verbosity.hrl").


%%%-----------------------------------------------------------------
%%% Access Control Module for VACM  (see also snmpa_acm)
%%% This module implements:
%%%   1. access control functions for VACM
%%%   2. vacmAccessTable as an ordered ets table
%%%
%%% This version of VACM handles v1, v2c and v3.
%%%-----------------------------------------------------------------

%%%-----------------------------------------------------------------
%%%   1.  access control functions for VACM
%%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: get_mib_view/5 -> {ok, ViewName} | 
%%                         {discarded, Reason}
%% Types: ViewType    = read | write | notify
%%        SecModel    = ?SEC_*  (see snmp_types.hrl)
%%        SecName     = string()
%%        SecLevel    = ?'SnmpSecurityLevel_*' (see SNMP-FRAMEWORK-MIB.hrl)
%%        ContextName = string()
%% Purpose: This function is used to map VACM parameters to a mib
%%          view.
%%-----------------------------------------------------------------
get_mib_view(ViewType, SecModel, SecName, SecLevel, ContextName) ->
    check_auth(catch auth(ViewType, SecModel, SecName, SecLevel, ContextName)).


%% Follows the procedure in rfc2275
auth(ViewType, SecModel, SecName, SecLevel, ContextName) ->
    ?vtrace("auth -> entry with"
	    "~n   ViewType:    ~p"
	    "~n   SecModel:    ~p"
	    "~n   SecName:     ~p"
	    "~n   SecLevel:    ~p"
	    "~n   ContextName: ~p", 
	    [ViewType, SecModel, SecName, SecLevel, ContextName]),
    % 3.2.1 - Check that the context is known to us
    ?vdebug("check that the context (~p) is known to us",[ContextName]),
    case snmp_view_based_acm_mib:vacmContextTable(get, ContextName,
						  [?vacmContextName]) of
	[_Found] ->
	    ok;
	_ ->
	    snmpa_mpd:inc(snmpUnknownContexts),
	    throw({discarded, noSuchContext})
    end,
    % 3.2.2 - Check that the SecModel and SecName is valid
    ?vdebug("check that SecModel (~p) and SecName (~p) is valid",
	    [SecModel, SecName]),
    GroupName = 
	case snmp_view_based_acm_mib:get(vacmSecurityToGroupTable, 
					 [SecModel, length(SecName) | SecName],
		 [?vacmGroupName, ?vacmSecurityToGroupStatus]) of
	    [{value, GN}, {value, ?'RowStatus_active'}] ->
		GN;
	    [{value, _GN}, {value, RowStatus}] ->
		?vlog("valid SecModel and SecName but wrong row status:"
		      "~n   RowStatus: ~p", [RowStatus]),
		throw({discarded, noGroupName});
	    _ ->
		throw({discarded, noGroupName})
	end,
    % 3.2.3-4 - Find an access entry and its view name
    ?vdebug("find an access entry and its view name",[]),
    ViewName =
	case get_view_name(ViewType, GroupName, ContextName,
			   SecModel, SecLevel) of
	    {ok, VN} -> VN;
	    Error -> throw(Error)
	end,
    % 3.2.5a - Find the corresponding mib view
    ?vdebug("find the corresponding mib view (for ~p)",[ViewName]),
    get_mib_view(ViewName).

check_auth({'EXIT',    Error})  -> exit(Error);
check_auth({discarded, Reason}) -> {discarded, Reason};
check_auth(Res)                 -> {ok, Res}.

%%-----------------------------------------------------------------
%% Returns a list of {ViewSubtree, ViewMask, ViewType}
%% The view table is index by ViewIndex, ViewSubtree,
%% so a next on ViewIndex returns the first
%% key in the table >= ViewIndex.
%%-----------------------------------------------------------------
get_mib_view(ViewName) ->
    ?vtrace("get_mib_view -> entry with"
	    "~n   ViewName: ~p", [ViewName]),
    ViewKey = [length(ViewName) | ViewName],
    case snmp_view_based_acm_mib:table_next(vacmViewTreeFamilyTable,
					    ViewKey) of
	endOfTable ->
	    {discarded, noSuchView};
	Indexes ->
	    case split_prefix(ViewKey, Indexes) of
		{ok, Subtree} ->
		    loop_mib_view(ViewKey, Subtree, Indexes, []);
		false ->
		    {discarded, noSuchView}
	    end
    end.

split_prefix([H|T], [H|T2]) -> split_prefix(T,T2);
split_prefix([], Rest) -> {ok, Rest};
split_prefix(_, _) -> false.
    

%% ViewName is including length from now on
loop_mib_view(ViewName, Subtree, Indexes, MibView) ->
    [{value, Mask}, {value, Type}, {value, Status}] = 
	snmp_view_based_acm_mib:vacmViewTreeFamilyTable(
	  get, Indexes,
	  [?vacmViewTreeFamilyMask, 
	   ?vacmViewTreeFamilyType,
	   ?vacmViewTreeFamilyStatus]),
    NextMibView = 
	case Status of
	    ?'RowStatus_active' ->
		[_Length | Tree] = Subtree,
		[{Tree, Mask, Type} | MibView];
	    _ ->
		MibView
	end,
    case snmp_view_based_acm_mib:table_next(vacmViewTreeFamilyTable, 
					    Indexes) of
	endOfTable -> NextMibView;
	NextIndexes ->
	    case split_prefix(ViewName, NextIndexes) of
		{ok, NextSubTree} ->
		    loop_mib_view(ViewName, NextSubTree, NextIndexes,
				  NextMibView);
		false ->
		    NextMibView
	    end
    end.

%%%-----------------------------------------------------------------
%%%  1b.  The ordered ets table that implements vacmAccessTable
%%%-----------------------------------------------------------------

init(Dir) ->
    init(Dir, terminate).

init(Dir, InitError) ->
    FName = filename:join(Dir, "snmpa_vacm.db"),
    case file:read_file_info(FName) of
	{ok, _} -> 
	    %% File exists - we must check this, since ets doesn't tell
	    %% us the reason in case of error...
	    case ets:file2tab(FName) of
		{ok, _Tab} -> 
		    gc_tab([]);
		{error, Reason} ->
		    user_err("Corrupt VACM database ~p", [FName]),
		    case InitError of
			terminate ->
			    throw({error, {file2tab, FName, Reason}});
			_ ->
			    %% Rename old file (for later analyzes)
			    Saved = FName ++ ".saved",
			    file:rename(FName, Saved),
			    ets:new(snmpa_vacm, 
				    [public, ordered_set, named_table])
		    end
	    end;
	{error, _} ->
	    ets:new(snmpa_vacm, [public, ordered_set, named_table])
    end,
    ets:insert(snmp_agent_table, {snmpa_vacm_file, FName}),
    {ok, FName}.


backup(BackupDir) ->
    BackupFile = filename:join(BackupDir, "snmpa_vacm.db"),
    ets:tab2file(snmpa_vacm, BackupFile).


%% Ret: {ok, ViewName} | {error, Reason}
get_view_name(ViewType, GroupName, ContextName, SecModel, SecLevel) ->
    ?vtrace("get_view_name -> entry with"
	    "~n   ViewType:    ~p"
	    "~n   GroupName:   ~p"
	    "~n   ContextName: ~p"
	    "~n   SecModel:    ~p"
	    "~n   SecLevel:    ~p", 
	    [ViewType, GroupName, ContextName, SecModel, SecLevel]),
    GroupKey = [length(GroupName) | GroupName],
    case get_access_row(GroupKey, ContextName, SecModel, SecLevel) of
	undefined ->
	    {discarded, noAccessEntry};
	Row ->
	    ?vtrace("get_view_name -> Row: ~n   ~p", [Row]),
	    ViewName =
		case ViewType of
		    read -> element(?vacmAReadViewName, Row);
		    write -> element(?vacmAWriteViewName, Row);
		    notify -> element(?vacmANotifyViewName, Row)
		end,
	    case ViewName of
		"" -> 
		    ?vtrace("get_view_name -> not found when"
			    "~n   ViewType:    ~p"
			    "~n   GroupName:   ~p"
			    "~n   ContextName: ~p"
			    "~n   SecModel:    ~p"
			    "~n   SecLevel:    ~p", [ViewType, GroupName, 
						     ContextName, SecModel, 
						     SecLevel]),
		    {discarded, noSuchView};
		_ -> {ok, ViewName}
	    end
    end.


get_row(Key) -> 
    case ets:lookup(snmpa_vacm, Key) of
	[{_Key, Row}] -> {ok, Row};
	_ -> false
    end.

get_next_row(Key) ->
    case ets:next(snmpa_vacm, Key) of
	'$end_of_table' -> false;
	NextKey  ->
	    case ets:lookup(snmpa_vacm, NextKey) of
		[Entry] -> Entry;
		_ -> false
	    end
    end.

insert(Entries) -> insert(Entries, true).

insert(Entries, Dump) ->
    lists:foreach(fun(Entry) -> ets:insert(snmpa_vacm, Entry) end, Entries),
    dump_table(Dump).

delete(Key) ->
    ets:delete(snmpa_vacm, Key),
    dump_table().


cleanup() ->
    ets:delete_all_objects(snmpa_vacm),
    dump_table().

dump_table(true) ->
    dump_table();
dump_table(_) ->
    ok.


dump_table() ->
    [{_, FName}] = ets:lookup(snmp_agent_table, snmpa_vacm_file),
    TmpName = unique_table_name(FName), 
    case ets:tab2file(snmpa_vacm, TmpName) of
	ok ->
	    case file:rename(TmpName, FName) of
		ok ->
		    ok;
		Else -> % What is this? Undocumented return code...
		    user_err("Warning: could not move VACM db ~p"
			     " (~p)", [FName, Else])
	    end;
	{error, Reason} ->
	    user_err("Warning: could not save vacm db ~p (~p)",
		     [FName, Reason])
    end.

%% This little thing is an attempt to create a "unique" filename
%% in order to minimize the risk of two processes at the same 
%% time dumping the table.
unique_table_name(Pre) ->
    %% We want something that is guaranteed to be unique, 
    %% therefor we use erlang:timestamp() instead of os:timestamp()
    unique_table_name(Pre, erlang:timestamp()).

unique_table_name(Pre, {_A, _B, C} = Now) ->
    {Date, Time}     = calendar:now_to_datetime(Now),
    {YYYY, MM, DD}   = Date,
    {Hour, Min, Sec} = Time,
    FormatDate =
        io_lib:format("~.4w~.2.0w~.2.0w_~.2.0w~.2.0w~.2.0w_~w",
                      [YYYY, MM, DD, Hour, Min, Sec, round(C/1000)]), 
    unique_table_name2(Pre, FormatDate).

unique_table_name2(Pre, FormatedDate) ->
    PidPart = unique_table_name_pid(), 
    lists:flatten(io_lib:format("~s.~s~s.tmp", [Pre, PidPart, FormatedDate])).

unique_table_name_pid() ->
    case string:tokens(pid_to_list(self()), [$<,$.,$>]) of
	[A, B, C] ->
	    A ++ B ++ C ++ ".";
	_ ->
	    ""
    end.


%%-----------------------------------------------------------------
%% Alg.
%% Procedure is defined in the descr. of vacmAccessTable.
%%
%% for (each entry with matching group name, context, secmodel and seclevel)
%% {
%%   rate the entry; if it's score is > prev max score, keep it
%% }
%%
%% Rating:  The procedure says to keep entries in order
%%    1.  matching secmodel  ('any'(0) or same(1) is ok)
%%    2.  matching contextprefix (exact(1) or prefix(0) is ok)
%%    3.  longest prefix (0..32)
%%    4.  highest secLevel (noAuthNoPriv(0) < authNoPriv(1) < authPriv(2))
%%  We give each entry a single rating number according to this order.
%%  The number is chosen so that a higher number gives a better
%%  entry, according to the order above.
%%  The number is:
%%    secLevel + (3 * prefix_len) + (99 * match_prefix) + (198 * match_secmodel)
%%
%% Optimisation:  Maybe the most common case is that there
%% is just one matching entry, and it matches exact.  We could do
%% an exact lookup for this entry; if we find one, use it, otherwise
%% perform this alg.
%%-----------------------------------------------------------------
get_access_row(GroupKey, ContextName, SecModel, SecLevel) ->
    %% First, try the optimisation...
    ExactKey =
	GroupKey ++ [length(ContextName) | ContextName] ++ [SecModel,SecLevel],
    case ets:lookup(snmpa_vacm, ExactKey) of
	[{_Key, Row}] ->
	    Row;
	_ -> % Otherwise, perform the alg
	    get_access_row(GroupKey, GroupKey, ContextName,
			   SecModel, SecLevel, 0, undefined)
    end.

get_access_row(Key, GroupKey, ContextName, SecModel, SecLevel, Score, Found) ->
    case get_next_row(Key) of
	{NextKey, Row}
	when element(?vacmAStatus, Row) == ?'RowStatus_active'->
	    case catch score(NextKey, GroupKey, ContextName,
			     element(?vacmAContextMatch, Row), 
			     SecModel, SecLevel) of
		{ok, NScore} when NScore > Score ->
		    get_access_row(NextKey, GroupKey, ContextName,
				   SecModel, SecLevel, NScore, Row);
		{ok, _} -> % e.g. a throwed {ok, 0}
		    get_access_row(NextKey, GroupKey, ContextName,
				   SecModel, SecLevel, Score, Found);
		false ->
		    Found
	    end;
	{NextKey, _InvalidRow} ->
	    get_access_row(NextKey, GroupKey, ContextName, SecModel,
			   SecLevel, Score, Found);
	false ->
	    Found
    end.
		
		

score(Key, GroupKey, ContextName, Match, SecModel, SecLevel) ->
    [CtxLen | Rest1] = chop_off_group(GroupKey, Key),
    {NPrefix, [VSecModel, VSecLevel]} =
	chop_off_context(ContextName, Rest1, 0, CtxLen, Match),
    %% Make sure the vacmSecModel is valid (any or matching)
    NSecModel = case VSecModel of
		    SecModel -> 198;
		    ?SEC_ANY -> 0;
		    _        -> throw({ok, 0})
		end,
    %% Make sure the vacmSecLevel is less than the requested
    NSecLevel =	if 
		    VSecLevel =< SecLevel -> VSecLevel - 1;
		    true                  -> throw({ok, 0})
		end,
    {ok, NSecLevel + 3*CtxLen + NPrefix + NSecModel}.
    


chop_off_group([H|T], [H|T2]) -> chop_off_group(T, T2);
chop_off_group([], Rest) -> Rest;
chop_off_group(_, _) -> throw(false).

chop_off_context([H|T], [H|T2], Cnt, Len, Match) when Cnt < Len ->
    chop_off_context(T, T2, Cnt+1, Len, Match);
chop_off_context([], Rest, _Len, _Len, _Match) ->
    %% We have exact match; don't care about Match
    {99, Rest};
chop_off_context(_, Rest, Len, Len, ?vacmAccessContextMatch_prefix) ->
    %% We have a prefix match
    {0, Rest};
chop_off_context(_Ctx, _Rest, _Cnt, _Len, _Match) ->    
    %% Otherwise, it didn't match!
    throw({ok, 0}).


gc_tab(Oid) ->
    case get_next_row(Oid) of
	{NextOid, Row} ->
	    case element(?vacmAStorageType, Row) of
		?'StorageType_volatile' ->
		    ets:delete(snmpa_vacm, NextOid),
		    gc_tab(NextOid);
		_ ->
		    gc_tab(NextOid)
	    end;
	false ->
	    ok
    end.


user_err(F, A) ->
    snmpa_error:user_err(F, A).
