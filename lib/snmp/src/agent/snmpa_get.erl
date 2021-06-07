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

-module(snmpa_get).

-behaviour(snmpa_get_mechanism).


%%%-----------------------------------------------------------------
%%% snmpa_get_mechanism exports
%%%-----------------------------------------------------------------

-export([
         do_get/3, do_get/4,
         do_get_next/3,
         do_get_bulk/7
        ]).

-define(VMODULE,"GET").
-include("snmpa_internal.hrl").
-include("snmp_types.hrl").
-include("snmp_debug.hrl").
-include("snmp_verbosity.hrl").

-ifndef(default_verbosity).
-define(default_verbosity,silence).
-endif.

-define(empty_pdu_size, 21).

-ifdef(snmp_extended_verbosity).
-define(vt(F,A), ?vtrace(F, A)).
-else.
-define(vt(_F, _A), ok).
-endif.


-define(AGENT, snmpa_agent).
-define(LIB,   snmpa_get_lib).



%%%-----------------------------------------------------------------
%%% 3. GET REQUEST
%%% --------------
%%%   According to RFC1157, section 4.1.2 and RFC1905, section 4.2.1.
%%%   In rfc1157:4.1.2 it isn't specified if noSuchName should be
%%%   returned even if some other varbind generates a genErr.
%%%   In rfc1905:4.2.1 this is not a problem since exceptions are
%%%   used, and thus a genErr will be returned anyway.
%%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% Func: do_get/2
%% Purpose: Handles all VBs in a request that is inside the 
%%          mibview (local).
%% Returns: {noError, 0, ListOfNewVarbinds} |
%%          {ErrorStatus, ErrorIndex, []}
%%-----------------------------------------------------------------

%% There is no way to properly spec the behaviour of the
%% ?AGENT:subagent_get/3 function (it *can* exit).
-dialyzer({nowarn_function, do_get/3}).
do_get(UnsortedVarbinds, IsNotification, _Extra) ->
    {MyVarbinds, SubagentVarbinds} = ?LIB:agent_sort_vbs(UnsortedVarbinds),
    case do_get_local(MyVarbinds, IsNotification) of
	{noError, 0, NewMyVarbinds} ->
	    case do_get_subagents(SubagentVarbinds, IsNotification) of
		{noError, 0, NewSubagentVarbinds} ->
		    {noError, 0, NewMyVarbinds ++ NewSubagentVarbinds};
		{ErrorStatus, ErrorIndex, _} ->
		    {ErrorStatus, ErrorIndex, []}
	    end;
	{ErrorStatus, ErrorIndex, _} -> 
	    {ErrorStatus, ErrorIndex, []}
    end.


%%-----------------------------------------------------------------
%% Func: do_get/3
%% Purpose: do_get handles "getRequests".
%% Pre: incoming varbinds have type == 'NULL', value == unSpecified
%% Returns: {noError, 0, ListOfNewVarbinds} |
%%          {ErrorStatus, ErrorIndex, []}
%%-----------------------------------------------------------------

do_get(MibView, UnsortedVarbinds, IsNotification, Extra) ->
    ?vtrace("do_get -> entry with"
	    "~n   MibView:          ~p"
	    "~n   UnsortedVarbinds: ~p"
	    "~n   IsNotification:   ~p", 
	    [MibView, UnsortedVarbinds, IsNotification]),
    %% This is me, the master, so go ahead
    {OutSideView, InSideView} = ?LIB:split_vbs_view(UnsortedVarbinds, MibView),
    {Error, Index, NewVbs}    = do_get(InSideView, IsNotification, Extra),
    {Error, Index, NewVbs ++ OutSideView}.



%%-----------------------------------------------------------------
%% Func: do_get_local/2,3
%% Purpose: Loop the variablebindings list. We know that each varbind
%%          in that list belongs to us.
%% Returns: {noError, 0, ListOfNewVarbinds} |
%%          {ErrorStatus, ErrorIndex, []}
%%-----------------------------------------------------------------

do_get_local(VBs, IsNotification) ->
    do_get_local(VBs, [], IsNotification).

-dialyzer({nowarn_function, do_get_local/3}).
do_get_local([Vb | Vbs], Res, IsNotification) ->
    case try_get(Vb, IsNotification) of
	NewVb when is_record(NewVb, varbind) ->
	    do_get_local(Vbs, [NewVb | Res], IsNotification);
	ListOfNewVb when is_list(ListOfNewVb) ->
	    do_get_local(Vbs, lists:append(ListOfNewVb, Res), IsNotification);
	{error, Error, OrgIndex} ->
	    {Error, OrgIndex, []}
    end;
do_get_local([], Res, _IsNotification) -> 
    {noError, 0, Res}.



%%-----------------------------------------------------------------
%% Func: do_get_subagents/2
%% Purpose: Loop the list of varbinds for different subagents.
%%          For each of them, call sub_agent_get to retreive
%%          the values for them.
%% Returns: {noError, 0, ListOfNewVarbinds} |
%%          {ErrorStatus, ErrorIndex, []}
%%-----------------------------------------------------------------

%% There is now to properly spec the behaviour of the ?AGENT:subagent_get/3
%% function (it *can* exit).
-dialyzer({nowarn_function, do_get_subagents/3}).
do_get_subagents(SubagentVarbinds, IsNotification) ->
    do_get_subagents(SubagentVarbinds, [], IsNotification).

do_get_subagents([{SubAgentPid, SAVbs} | Tail], Res, IsNotification) ->
    {_SAOids, Vbs} = ?LIB:sa_split(SAVbs),
    case (catch ?AGENT:subagent_get(SubAgentPid, Vbs, IsNotification)) of
	{noError, 0, NewVbs} ->
	    do_get_subagents(Tail, lists:append(NewVbs, Res), IsNotification);
	{ErrorStatus, ErrorIndex, _} ->
	    {ErrorStatus, ErrorIndex, []};
	{'EXIT', Reason} ->
	    ?LIB:user_err("Lost contact with subagent (get) ~w. Using genErr", 
                          [Reason]),
	    {genErr, 0, []} 
    end;
do_get_subagents([], Res, _IsNotification) ->
    {noError, 0, Res}.


%%-----------------------------------------------------------------
%% Func: try_get/2
%% Returns: {error, ErrorStatus, OrgIndex} |
%%          #varbind |
%%          List of #varbind
%%-----------------------------------------------------------------

-dialyzer({nowarn_function, try_get/2}).
try_get(IVb, IsNotification) when is_record(IVb, ivarbind) ->
    ?vtrace("try_get(ivarbind) -> entry with"
	    "~n   IVb: ~p", [IVb]),
    get_var_value_from_ivb(IVb, IsNotification);
try_get({TableOid, TableVbs}, IsNotification) ->
    ?vtrace("try_get(table) -> entry with"
	    "~n   TableOid: ~p"
	    "~n   TableVbs: ~p", [TableOid, TableVbs]),
    [#ivarbind{mibentry = MibEntry}|_] = TableVbs,
    {NoAccessVbs, AccessVbs} =
	check_all_table_vbs(TableVbs, IsNotification, [], []),
    case get_tab_value_from_mib(MibEntry, TableOid, AccessVbs) of
	{error, ErrorStatus, OrgIndex} ->
	    {error, ErrorStatus, OrgIndex};
	NVbs ->
	    NVbs ++ NoAccessVbs
    end.


%%-----------------------------------------------------------------
%% Make sure all requested columns are accessible.
%%-----------------------------------------------------------------

-dialyzer({nowarn_function, check_all_table_vbs/4}).
check_all_table_vbs([IVb| IVbs], IsNotification, NoA, A) ->
    #ivarbind{mibentry = Me, varbind = Vb} = IVb,
    case Me#me.access of
	'not-accessible' -> 
	    NNoA = [Vb#varbind{value = noSuchInstance} | NoA],
	    check_all_table_vbs(IVbs, IsNotification, NNoA, A);
	'accessible-for-notify' when IsNotification =:= false -> 
	    NNoA = [Vb#varbind{value = noSuchInstance} | NoA],
	    check_all_table_vbs(IVbs, IsNotification, NNoA, A);
	'write-only' -> 
	    NNoA = [Vb#varbind{value = noSuchInstance} | NoA],
	    check_all_table_vbs(IVbs, IsNotification, NNoA, A);
	_ ->
	    check_all_table_vbs(IVbs, IsNotification, NoA, [IVb | A])
    end;
check_all_table_vbs([], _IsNotification, NoA, A) -> {NoA, A}.

%%-----------------------------------------------------------------
%% Returns: {error, ErrorStatus, OrgIndex} |
%%          #varbind
%%-----------------------------------------------------------------
-dialyzer({nowarn_function, get_var_value_from_ivb/2}).
get_var_value_from_ivb(IVb, IsNotification)
  when IVb#ivarbind.status =:= noError ->
    ?vtrace("get_var_value_from_ivb(noError) -> entry", []),
    #ivarbind{mibentry = Me, varbind = Vb} = IVb,
    #varbind{org_index = OrgIndex, oid = Oid} = Vb,
    case Me#me.access of
	'not-accessible' -> 
	    Vb#varbind{value = noSuchInstance};
	'accessible-for-notify' when IsNotification =:= false -> 
	    Vb#varbind{value = noSuchInstance};
	'write-only' -> 
	    Vb#varbind{value = noSuchInstance};
	_ -> 
	    case get_var_value_from_mib(Me, Oid) of
		{value, Type, Value} ->
		    Vb#varbind{variabletype = Type, value = Value};
		{error, ErrorStatus} ->
		    {error, ErrorStatus, OrgIndex}
	    end
    end;
get_var_value_from_ivb(#ivarbind{status = Status, varbind = Vb}, _) ->
    ?vtrace("get_var_value_from_ivb(~p) -> entry", [Status]),
    Vb#varbind{value = Status}.

%%-----------------------------------------------------------------
%% Func: get_var_value_from_mib/1
%% Purpose: 
%% Returns: {error, ErrorStatus} |
%%          {value, Type, Value}
%%-----------------------------------------------------------------
%% Pre: Oid is a correct instance Oid (lookup checked that).
%% Returns: A correct return value (see ?AGENT:make_value_a_correct_value)
-dialyzer({nowarn_function, get_var_value_from_mib/2}).
get_var_value_from_mib(#me{entrytype = variable,
			   asn1_type = ASN1Type,
			   mfa       = {Mod, Func, Args}},
		       _Oid) ->
    ?vtrace("get_var_value_from_mib(variable) -> entry when"
	    "~n   Mod:  ~p"
	    "~n   Func: ~p"
	    "~n   Args: ~p", [Mod, Func, Args]),
    Result = (catch ?LIB:dbg_apply(Mod, Func, [get | Args])),
    %% mib shall return {value, <a-nice-value-within-range>} |
    %% {noValue, noSuchName} (v1) | 
    %% {noValue, noSuchObject | noSuchInstance} (v2, v1)
    %% everything else (including 'genErr') will generate 'genErr'.
    ?AGENT:make_value_a_correct_value(Result, ASN1Type, {Mod, Func, Args});

get_var_value_from_mib(#me{entrytype = table_column,
			   oid       = MeOid,
			   asn1_type = ASN1Type,
			   mfa       = {Mod, Func, Args}},
		       Oid) ->
    ?vtrace("get_var_value_from_mib(table_column) -> entry when"
	    "~n   MeOid: ~p"
	    "~n   Mod:   ~p"
	    "~n   Func:  ~p"
	    "~n   Args:  ~p"
	    "~n   Oid:   ~p", [MeOid, Mod, Func, Args, Oid]),
    Col = lists:last(MeOid),
    Indexes = snmp_misc:diff(Oid, MeOid),
    [Result] = (catch ?LIB:dbg_apply(Mod, Func, [get, Indexes, [Col] | Args])),
    ?AGENT:make_value_a_correct_value(Result, ASN1Type, 
                                      {Mod, Func, Args, Indexes, Col}).


%% For table operations we need to pass RestOid down to the table-function.
%% Its up to the table-function to check for noSuchInstance (ex: a 
%% non-existing row).
%% Returns: {error, ErrorStatus, OrgIndex} |
%%          {value, Type, Value}
-dialyzer({nowarn_function, get_tab_value_from_mib/3}).
get_tab_value_from_mib(#me{mfa = {Mod, Func, Args}}, TableOid, TableVbs) ->
    ?vtrace("get_tab_value_from_mib -> entry when"
	    "~n   Mod:  ~p"
	    "~n   Func: ~p"
	    "~n   Args: ~p", [Mod, Func, Args]),
    TableOpsWithShortOids = ?LIB:delete_prefixes(TableOid, TableVbs),
    SortedVBsRows = snmpa_svbl:sort_varbinds_rows(TableOpsWithShortOids), 
    case get_value_all_rows(SortedVBsRows, Mod, Func, Args, []) of
	{Error, Index} ->
	    #ivarbind{varbind = Vb} = lists:nth(Index, TableVbs),
	    {error, Error, Vb#varbind.org_index};
	ListOfValues -> 
	    merge_varbinds_and_value(TableVbs, ListOfValues)
    end.

%%-----------------------------------------------------------------
%% Values is a scrambled list of {CorrectValue, Index}, where Index
%% is index into the #ivarbind list. So for each Value, we must
%% find the corresponding #ivarbind, and merge them into a new
%% #varbind.
%% The Values list comes from validate_tab_res.
%%-----------------------------------------------------------------
-dialyzer({nowarn_function, merge_varbinds_and_value/2}).
merge_varbinds_and_value(IVbs, [{{value, Type, Value}, Index} | Values]) ->
    #ivarbind{varbind = Vb} = lists:nth(Index, IVbs),
    [Vb#varbind{variabletype = Type, value = Value} |
     merge_varbinds_and_value(IVbs, Values)];
merge_varbinds_and_value(_, []) -> [].
    
-dialyzer({nowarn_function, get_value_all_rows/5}).
get_value_all_rows([{[], OrgCols} | Rows], Mod, Func, Args, Res) ->
    ?vtrace("get_value_all_rows -> entry when"
	    "~n   OrgCols: ~p", [OrgCols]),
    Cols   = [{{value, noValue, noSuchInstance}, Index} || 
		 {_Col, _ASN1Type, Index} <- OrgCols], 
    NewRes = lists:append(Cols, Res),
    get_value_all_rows(Rows, Mod, Func, Args, NewRes);
get_value_all_rows([{RowIndex, OrgCols} | Rows], Mod, Func, Args, Res) ->
    ?vtrace("get_value_all_rows -> entry when"
	    "~n   RowIndex: ~p"
	    "~n   OrgCols:  ~p", [RowIndex, OrgCols]),
    {DOrgCols, Dup} = remove_duplicates(OrgCols),
    Cols   = delete_index(DOrgCols),
    Result = (catch ?LIB:dbg_apply(Mod, Func, [get, RowIndex, Cols | Args])),
    case validate_tab_res(Result, DOrgCols, {Mod, Func, Args}) of
	Values when is_list(Values) ->
	    NVals  = restore_duplicates(Dup, Values),
	    NewRes = lists:append(NVals, Res),
	    get_value_all_rows(Rows, Mod, Func, Args, NewRes);
	{error, ErrorStatus, Index} ->
	    ?AGENT:validate_err(row_set, {ErrorStatus, Index}, {Mod, Func, Args})
    end;
get_value_all_rows([], _Mod, _Func, _Args, Res) -> 
    ?vtrace("get_value_all_rows -> entry when done"
	    "~n   Res: ~p", [Res]),
    Res.

%%-----------------------------------------------------------------
%% Args: {RowIndex, list of {ShortOid, ASN1Type}}
%% Returns: list of Col
%%-----------------------------------------------------------------
delete_index([{Col, _Val, _OrgIndex} | T]) ->
    [Col | delete_index(T)];
delete_index([]) -> [].

%%-----------------------------------------------------------------
%% This function is called before 'get' on a table, and removes
%% any duplicate columns.  It returns {Cols, DupInfo}.  The Cols
%% are the unique columns.  The instrumentation function is
%% called to get the values.  These values, together with the
%% DupInfo, is later passed to restore_duplicates, which uses
%% the retrieved values to reconstruct the original column list,
%% but with the retrieved value for each column.
%%-----------------------------------------------------------------

-dialyzer({nowarn_function, remove_duplicates/1}).
remove_duplicates(Cols) ->
    remove_duplicates(Cols, [], []).


-dialyzer({nowarn_function, remove_duplicates/3}).
remove_duplicates([{Col, V1, OrgIdx1}, {Col, V2, OrgIdx2} | T], NCols, Dup) ->
    remove_duplicates([{Col, V1, OrgIdx1} | T], NCols, 
		      [{Col, V2, OrgIdx2} | Dup]);
remove_duplicates([Col | T], NCols, Dup) ->
    remove_duplicates(T, [Col | NCols], Dup);
remove_duplicates([], NCols, Dup) ->
    {lists:reverse(NCols), lists:reverse(Dup)}.

-dialyzer({nowarn_function, restore_duplicates/2}).
restore_duplicates([], Cols) ->
    [{Val, OrgIndex} || {_Col, Val, OrgIndex} <- Cols];
restore_duplicates([{Col, _Val2, OrgIndex2} | Dup],
		   [{Col, NVal, OrgIndex1} | Cols]) ->
    [{NVal, OrgIndex2} |
     restore_duplicates(Dup, [{Col, NVal, OrgIndex1} | Cols])];
restore_duplicates(Dup, [{_Col, Val, OrgIndex} | T]) ->
    [{Val, OrgIndex} | restore_duplicates(Dup, T)].



%%-----------------------------------------------------------------
%% Three cases:
%%   1) All values ok
%%   2) table_func returned {Error, ...}
%%   3) Some value in Values list is erroneous.
%% Args: Value is a list of values from table_func(get..)
%%       OrgCols is a list with {Col, ASN1Type, OrgIndex} 
%%         each element in Values and OrgCols correspond to each
%%         other.
%%-----------------------------------------------------------------
-dialyzer({nowarn_function, validate_tab_res/3}).
validate_tab_res(Values, OrgCols, Mfa) when is_list(Values) ->
    {_Col, _ASN1Type, OneIdx} = hd(OrgCols),
    validate_tab_res(Values, OrgCols, Mfa, [], OneIdx);
validate_tab_res({noValue, Error}, OrgCols, Mfa) ->
    Values = lists:duplicate(length(OrgCols), {noValue, Error}),
    validate_tab_res(Values, OrgCols, Mfa);
validate_tab_res({genErr, Col}, OrgCols, Mfa) ->
    case lists:keysearch(Col, 1, OrgCols) of
	{value, {_Col, _ASN1Type, Index}} ->
	    {error, genErr, Index};
	_ ->
	    ?LIB:user_err("Invalid column in {genErr, ~w} from ~w (get)",
                          [Col, Mfa]),
	    [{_Col, _ASN1Type, Index} | _] = OrgCols,
	    {error, genErr, Index}
    end;
validate_tab_res(genErr, [{_Col, __ASN1Type, Index} | _OrgCols], _Mfa) ->
    {error, genErr, Index};
validate_tab_res(Error, [{_Col, _ASN1Type, Index} | _OrgCols], Mfa) ->
    ?LIB:user_err("Invalid return value ~w from ~w (get)",[Error, Mfa]),
    {error, genErr, Index}.

-dialyzer({nowarn_function, validate_tab_res/5}).
validate_tab_res([Value | Values], 
		 [{Col, ASN1Type, Index} | OrgCols],
		 Mfa, Res, I) ->
    %% This one makes it possible to return a list of genErr, which
    %% is not allowed according to the manual.  But that's ok, as
    %% everything else will generate a genErr! (the only problem is
    %% that it won't generate a user_error).
    case ?AGENT:make_value_a_correct_value(Value, ASN1Type, Mfa) of
	{error, ErrorStatus} ->
	    {error, ErrorStatus, Index};
	CorrectValue ->
	    NewRes = [{Col, CorrectValue, Index} | Res],
	    validate_tab_res(Values, OrgCols, Mfa, NewRes, I)
    end;
validate_tab_res([], [], _Mfa, Res, _I) -> 
    lists:reverse(Res);
validate_tab_res([], [{_Col, _ASN1Type, Index}|_], Mfa, _Res, _I) ->
    ?LIB:user_err("Too few values returned from ~w (get)", [Mfa]),
    {error, genErr, Index};
validate_tab_res(_TooMany, [], Mfa, _Res, I) ->
    ?LIB:user_err("Too many values returned from ~w (get)", [Mfa]),
    {error, genErr, I}.



%%%-----------------------------------------------------------------
%%% 4. GET-NEXT REQUEST
%%% --------------
%%%   According to RFC1157, section 4.1.3 and RFC1905, section 4.2.2.
%%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: do_get_next/3
%% Purpose: do_get_next handles "getNextRequests".
%% Note: Even if it is SNMPv1, a varbind's value can be
%%       endOfMibView. This is converted to noSuchName in process_pdu.
%% Returns: {noError, 0, ListOfNewVarbinds} |
%%          {ErrorStatus, ErrorIndex, []}
%% Note2: ListOfNewVarbinds is not sorted in any order!!!
%% Alg: First, the variables are sorted in OID order.
%%
%%      Second, next in the MIB is performed for each OID, and
%%      the result is collected as: if next oid is a variable,
%%      perform a get to retrieve its value; if next oid is in a
%%      table, save this value and continue until we get an oid
%%      outside this table. Then perform get_next on the table,
%%      and continue with all endOfTables and the oid outside the
%%      table; if next oid is an subagent, save this value and
%%      continue as in the table case.
%%
%%      Third, each response is checked for endOfMibView, or (for
%%      subagents) that the Oid returned has the correct prefix.
%%      (This is necessary since an SA can be registered under many
%%      separated subtrees, and if the last variable in the first
%%      subtree is requested in a next, the SA will return the first
%%      variable in the second subtree. This might be working, since
%%      there may be a variable in between these subtrees.) For each
%%      of these, a new get-next is performed, one at a time.
%%      This alg. might be optimised in several ways. The most 
%%      striking one is that the same SA might be called several
%%      times, when one time should be enough. But it isn't clear
%%      that this really matters, since many nexts across the same
%%      subagent must be considered to be very rare.
%%-----------------------------------------------------------------

do_get_next(MibView, UnsortedVBs, _Extra) ->
    do_get_next2(MibView, UnsortedVBs, infinity).

%% The third argument is only used if we are called as result
%% of a get-bulk request.
do_get_next2(_MibView, UnsortedVarbinds, GbMaxVBs) 
  when (is_integer(GbMaxVBs) andalso (length(UnsortedVarbinds) > GbMaxVBs)) ->
    {tooBig, 0, []}; % What is the correct index in this case?
do_get_next2(MibView, UnsortedVBs, GbMaxVBs) ->
    ?vt("do_get_next2 -> entry when"
 	"~n   MibView:          ~p"
 	"~n   UnsortedVBs: ~p", [MibView, UnsortedVBs]),
    SortedVBs = ?LIB:oid_sort_vbs(UnsortedVBs),
    ?vt("do_get_next -> "
 	"~n   SortedVBs: ~p", [SortedVBs]),
    next_loop_varbinds([], SortedVBs, MibView, [], [], GbMaxVBs).

next_loop_varbinds(_, Vbs, _MibView, Res, _LAVb, GbMaxVBs) 
  when (is_integer(GbMaxVBs) andalso 
	((length(Vbs) + length(Res)) > GbMaxVBs)) ->
    {tooBig, 0, []}; % What is the correct index in this case?

%% LAVb is Last Accessible Vb
next_loop_varbinds([], [Vb | Vbs], MibView, Res, LAVb, GbMaxVBs) ->
    ?vt("next_loop_varbinds -> entry when"
 	"~n   Vb:      ~p"
 	"~n   MibView: ~p", [Vb, MibView]),
    case varbind_next(Vb, MibView) of
	endOfMibView ->
	    ?vt("next_loop_varbind -> endOfMibView", []),
	    RVb = if LAVb =:= [] -> Vb;
		     true -> LAVb
		  end,
	    NewVb = RVb#varbind{variabletype = 'NULL', value = endOfMibView},
	    next_loop_varbinds([], Vbs, MibView, [NewVb | Res], [], GbMaxVBs);

	{variable, ME, VarOid} when ((ME#me.access =/= 'not-accessible') andalso 
				     (ME#me.access =/= 'write-only') andalso 
				     (ME#me.access =/= 'accessible-for-notify')) -> 
	    ?vt("next_loop_varbind -> variable: "
		"~n   ME:     ~p"
		"~n   VarOid: ~p", [ME, VarOid]),
	    case try_get_instance(Vb, ME) of
		{value, noValue, _NoSuchSomething} ->
		    ?vt("next_loop_varbind -> noValue", []),
		    %% Try next one
		    NewVb = Vb#varbind{oid   = VarOid, 
				       value = 'NULL'},
		    next_loop_varbinds([], [NewVb | Vbs], MibView, Res, [], 
				       GbMaxVBs);
		{value, Type, Value} ->
		    ?vt("next_loop_varbind -> value"
			"~n   Type:  ~p"
			"~n   Value: ~p", [Type, Value]),
		    NewVb = Vb#varbind{oid          = VarOid, 
				       variabletype = Type,
				       value        = Value},
		    next_loop_varbinds([], Vbs, MibView, [NewVb | Res], [],
				       GbMaxVBs);
		{error, ErrorStatus} ->
		    ?vdebug("next loop varbinds:"
			    "~n   ErrorStatus: ~p",[ErrorStatus]),
		    {ErrorStatus, Vb#varbind.org_index, []}
	    end;
	{variable, _ME, VarOid} -> 
	    ?vt("next_loop_varbind -> variable: "
		"~n   VarOid: ~p", [VarOid]),
	    RVb = if LAVb =:= [] -> Vb;
		     true -> LAVb
		  end,
	    NewVb = Vb#varbind{oid = VarOid, value = 'NULL'},
	    next_loop_varbinds([], [NewVb | Vbs], MibView, Res, RVb, GbMaxVBs);
	{table, TableOid, TableRestOid, ME} ->
	    ?vt("next_loop_varbind -> table: "
		"~n   TableOid:     ~p"
		"~n   TableRestOid: ~p"
		"~n   ME:           ~p", [TableOid, TableRestOid, ME]),
	    next_loop_varbinds({table, TableOid, ME,
				[{tab_oid(TableRestOid), Vb}]},
			       Vbs, MibView, Res, [], GbMaxVBs);
	{subagent, SubAgentPid, SAOid} ->
	    ?vt("next_loop_varbind -> subagent: "
		"~n   SubAgentPid: ~p"
		"~n   SAOid:       ~p", [SubAgentPid, SAOid]),
	    NewVb = Vb#varbind{variabletype = 'NULL', value = 'NULL'},
	    next_loop_varbinds({subagent, SubAgentPid, SAOid, [NewVb]},
			       Vbs, MibView, Res, [], GbMaxVBs)
    end;
next_loop_varbinds({table, TableOid, ME, TabOids},
		   [Vb | Vbs], MibView, Res, _LAVb, GbMaxVBs) ->
    ?vt("next_loop_varbinds(table) -> entry with"
 	"~n   TableOid: ~p"
 	"~n   Vb:       ~p", [TableOid, Vb]),
    case varbind_next(Vb, MibView) of
	{table, TableOid, TableRestOid, _ME} ->
	    next_loop_varbinds({table, TableOid, ME,
				[{tab_oid(TableRestOid), Vb} | TabOids]},
			       Vbs, MibView, Res, [], GbMaxVBs);
	_ ->
	    case get_next_table(ME, TableOid, TabOids, MibView) of
		{ok, TabRes, TabEndOfTabVbs} ->
		    NewVbs = lists:append(TabEndOfTabVbs, [Vb | Vbs]),
		    NewRes = lists:append(TabRes, Res),
		    next_loop_varbinds([], NewVbs, MibView, NewRes, [], 
				       GbMaxVBs);
		{ErrorStatus, OrgIndex} ->
		    ?vdebug("next loop varbinds: next varbind"
			    "~n   ErrorStatus: ~p"
			    "~n   OrgIndex:    ~p",
			    [ErrorStatus,OrgIndex]),
		    {ErrorStatus, OrgIndex, []}
	    end
    end;
next_loop_varbinds({table, TableOid, ME, TabOids},
		   [], MibView, Res, _LAVb, GbMaxVBs) ->
    ?vt("next_loop_varbinds(table) -> entry with"
	"~n   TableOid: ~p", [TableOid]),
    case get_next_table(ME, TableOid, TabOids, MibView) of
	{ok, TabRes, TabEndOfTabVbs} ->
 	    ?vt("next_loop_varbinds(table) -> get_next_table result:"
		"~n   TabRes:         ~p"
		"~n   TabEndOfTabVbs: ~p", [TabRes, TabEndOfTabVbs]),
	    NewRes = lists:append(TabRes, Res),
	    next_loop_varbinds([], TabEndOfTabVbs, MibView, NewRes, [], 
			       GbMaxVBs);
	{ErrorStatus, OrgIndex} ->
	    ?vdebug("next loop varbinds: next table"
		    "~n   ErrorStatus: ~p"
		    "~n   OrgIndex:    ~p",
		    [ErrorStatus,OrgIndex]),
	    {ErrorStatus, OrgIndex, []}
    end;
next_loop_varbinds({subagent, SAPid, SAOid, SAVbs},
		   [Vb | Vbs], MibView, Res, _LAVb, GbMaxVBs) ->
    ?vt("next_loop_varbinds(subagent) -> entry with"
	"~n   SAPid: ~p"
	"~n   SAOid: ~p"
 	"~n   Vb:    ~p", [SAPid, SAOid, Vb]),
    case varbind_next(Vb, MibView) of
	{subagent, _SubAgentPid, SAOid} ->
	    next_loop_varbinds({subagent, SAPid, SAOid,
				[Vb | SAVbs]},
			       Vbs, MibView, Res, [], GbMaxVBs);
	_ ->
	    case get_next_sa(SAPid, SAOid, SAVbs, MibView) of
		{ok, SARes, SAEndOfMibViewVbs} ->
		    NewVbs = lists:append(SAEndOfMibViewVbs, [Vb | Vbs]),
		    NewRes = lists:append(SARes, Res),
		    next_loop_varbinds([], NewVbs, MibView, NewRes, [], 
				       GbMaxVBs);
		{noSuchName, OrgIndex} ->
		    %% v1 reply, treat this Vb as endOfMibView, and try again
		    %% for the others.
		    case lists:keysearch(OrgIndex, #varbind.org_index, SAVbs) of
			{value, EVb} ->
			    NextOid = next_oid(SAOid),
			    EndOfVb = 
				EVb#varbind{oid = NextOid,
					    value = {endOfMibView, NextOid}},
			    case lists:delete(EVb, SAVbs) of
				[] ->
				    next_loop_varbinds([], [EndOfVb, Vb | Vbs],
						       MibView, Res, [],
						       GbMaxVBs);
				TryAgainVbs ->
				    next_loop_varbinds({subagent, SAPid, SAOid,
							TryAgainVbs},
						       [EndOfVb, Vb | Vbs],
						       MibView, Res, [],
						       GbMaxVBs)
			    end;
			false ->
			    %% bad index from subagent
			    {genErr, (hd(SAVbs))#varbind.org_index, []}
		    end;
		{ErrorStatus, OrgIndex} ->
 		    ?vdebug("next loop varbinds: next subagent"
 			    "~n   Vb:          ~p"
 			    "~n   ErrorStatus: ~p"
 			    "~n   OrgIndex:    ~p",
 			    [Vb,ErrorStatus,OrgIndex]),
		    {ErrorStatus, OrgIndex, []}
	    end
    end;
next_loop_varbinds({subagent, SAPid, SAOid, SAVbs},
		   [], MibView, Res, _LAVb, GbMaxVBs) ->
     ?vt("next_loop_varbinds(subagent) -> entry with"
	 "~n   SAPid: ~p"
	 "~n   SAOid: ~p", [SAPid, SAOid]),
    case get_next_sa(SAPid, SAOid, SAVbs, MibView) of
	{ok, SARes, SAEndOfMibViewVbs} ->
	    NewRes = lists:append(SARes, Res),
	    next_loop_varbinds([], SAEndOfMibViewVbs, MibView, NewRes, [],
			       GbMaxVBs);
	{noSuchName, OrgIndex} ->
	    %% v1 reply, treat this Vb as endOfMibView, and try again for
	    %% the others.
	    case lists:keysearch(OrgIndex, #varbind.org_index, SAVbs) of
		{value, EVb} ->
		    NextOid = next_oid(SAOid),
		    EndOfVb = EVb#varbind{oid = NextOid,
					  value = {endOfMibView, NextOid}},
		    case lists:delete(EVb, SAVbs) of
			[] ->
			    next_loop_varbinds([], [EndOfVb], MibView, Res, [],
					       GbMaxVBs);
			TryAgainVbs ->
			    next_loop_varbinds({subagent, SAPid, SAOid,
						TryAgainVbs},
					       [EndOfVb], MibView, Res, [],
					       GbMaxVBs)
		    end;
		false ->
		    %% bad index from subagent
		    {genErr, (hd(SAVbs))#varbind.org_index, []}
	    end;
	{ErrorStatus, OrgIndex} ->
 	    ?vdebug("next loop varbinds: next subagent"
 		    "~n   ErrorStatus: ~p"
 		    "~n   OrgIndex:    ~p",
 		    [ErrorStatus,OrgIndex]),
 	    {ErrorStatus, OrgIndex, []}
    end;
next_loop_varbinds([], [], _MibView, Res, _LAVb, _GbMaxVBs) ->
    ?vt("next_loop_varbinds -> entry when done", []),
    {noError, 0, Res}.

try_get_instance(_Vb, #me{mfa = {M, F, A}, asn1_type = ASN1Type}) ->
    ?vtrace("try_get_instance -> entry with"
	    "~n   M: ~p"
	    "~n   F: ~p"
	    "~n   A: ~p", [M,F,A]),
    Result = (catch ?LIB:dbg_apply(M, F, [get | A])),
    % mib shall return {value, <a-nice-value-within-range>} |
    % {noValue, noSuchName} (v1) | 
    % {noValue, noSuchObject | noSuchInstance} (v2, v1)
    % everything else (including 'genErr') will generate 'genErr'.
    ?AGENT:make_value_a_correct_value(Result, ASN1Type, {M, F, A}).

tab_oid([]) -> [0];
tab_oid(X) -> X.


%%-----------------------------------------------------------------
%% Perform a next, using the varbinds Oid if value is simple
%% value. If value is {endOf<something>, NextOid}, use NextOid.
%% This case happens when a table has returned endOfTable, or
%% a subagent has returned endOfMibView.
%%-----------------------------------------------------------------
varbind_next(#varbind{value = Value, oid = Oid}, MibView) ->
    ?vt("varbind_next -> entry with"
 	"~n   Value:   ~p"
 	"~n   Oid:     ~p"
 	"~n   MibView: ~p", [Value, Oid, MibView]),
    case Value of
	{endOfTable, NextOid} ->
	    snmpa_mib:next(get(mibserver), NextOid, MibView);
	{endOfMibView, NextOid} ->
	    snmpa_mib:next(get(mibserver), NextOid, MibView);
	_ ->
	    snmpa_mib:next(get(mibserver), Oid, MibView)
    end.

get_next_table(#me{mfa = {M, F, A}}, TableOid, TableOids, MibView) ->
    % We know that all TableOids have at least a column number as oid
    ?vt("get_next_table -> entry with"
	"~n   M:         ~p"
	"~n   F:         ~p"
	"~n   A:         ~p"
	"~n   TableOid:  ~p"
	"~n   TableOids: ~p"
	"~n   MibView:   ~p", [M, F, A, TableOid, TableOids, MibView]),
    Sorted = snmpa_svbl:sort_varbinds_rows(TableOids),
    case get_next_values_all_rows(Sorted, M,F,A, [], TableOid) of
	NewVbs when is_list(NewVbs) ->
 	    ?vt("get_next_table -> "
		"~n   NewVbs: ~p", [NewVbs]),
	    % We must now check each Vb for endOfTable and that it is
	    % in the MibView. If not, it becomes a endOfTable. We 
	    % collect all of these together.
	    transform_tab_next_result(NewVbs, {[], []}, MibView);
	{ErrorStatus, OrgIndex} ->
	    {ErrorStatus, OrgIndex}
    end.

get_next_values_all_rows([Row | Rows], M, F, A, Res, TabOid) ->
    {RowIndex, TableOids} = Row,
    Cols = delete_index(TableOids),
    ?vt("get_next_values_all_rows -> "
	"~n   Cols: ~p", [Cols]),
    Result = (catch ?LIB:dbg_apply(M, F, [get_next, RowIndex, Cols | A])),
    ?vt("get_next_values_all_rows -> "
 	"~n   Result: ~p", [Result]),
    case validate_tab_next_res(Result, TableOids, {M, F, A}, TabOid) of
	Values when is_list(Values) -> 
 	    ?vt("get_next_values_all_rows -> "
 		"~n   Values: ~p", [Values]),
	    NewRes = lists:append(Values, Res),
	    get_next_values_all_rows(Rows, M, F, A, NewRes, TabOid);
	{ErrorStatus, OrgIndex} ->
	    {ErrorStatus, OrgIndex}
    end;
get_next_values_all_rows([], _M, _F, _A, Res, _TabOid) ->
    Res.

transform_tab_next_result([Vb | Vbs], {Res, EndOfs}, MibView) ->
    case Vb#varbind.value of
	{endOfTable, _} ->
	    {ResVBs, EndOfVBs} = ?LIB:split_vbs(Vbs, Res, [Vb | EndOfs]),
            {ok, ResVBs, EndOfVBs};
	_ ->
	    case snmpa_acm:validate_mib_view(Vb#varbind.oid, MibView) of
		true ->
		    transform_tab_next_result(Vbs, {[Vb|Res], EndOfs},MibView);
		_ ->
		    Oid = Vb#varbind.oid,
		    NewEndOf = Vb#varbind{value = {endOfTable, Oid}},
		    transform_tab_next_result(Vbs, {Res, [NewEndOf | EndOfs]},
					      MibView)
	    end
    end;
transform_tab_next_result([], {Res, EndOfs}, _MibView) ->
    ?vt("transform_tab_next_result -> entry with: "
 	"~n   Res:    ~p"
 	"~n   EndIfs: ~p",[Res, EndOfs]),
    {ok, Res, EndOfs}.



%%-----------------------------------------------------------------
%% Three cases:
%%   1) All values ok
%%   2) table_func returned {Error, ...}
%%   3) Some value in Values list is erroneous.
%% Args: Value is a list of values from table_func(get_next, ...)
%%       TableOids is a list of {TabRestOid, OrgVb} 
%%         each element in Values and TableOids correspond to each
%%         other.
%% Returns: List of NewVarbinds |
%%          {ErrorStatus, OrgIndex}
%%          (In the NewVarbinds list, the value may be endOfTable)
%%-----------------------------------------------------------------
validate_tab_next_res(Values, TableOids, Mfa, TabOid) ->
     ?vt("validate_tab_next_res -> entry with: "
	 "~n   Values:     ~p"
	 "~n   TableOids:  ~p"
	 "~n   Mfa:        ~p"
	 "~n   TabOid:     ~p", [Values, TableOids, Mfa, TabOid]),
    {_Col, _ASN1Type, OneIdx} = hd(TableOids),
    validate_tab_next_res(Values, TableOids, Mfa, [], TabOid,
			  next_oid(TabOid), OneIdx).
validate_tab_next_res([{NextOid, Value} | Values],
		      [{_ColNo, OrgVb, _Index} | TableOids],
		      Mfa, Res, TabOid, TabNextOid, I) ->
    ?vt("validate_tab_next_res -> entry with: "
 	"~n   NextOid:    ~p"
 	"~n   Value:      ~p"
 	"~n   Values:     ~p"
 	"~n   TableOids:  ~p"
 	"~n   Mfa:        ~p"
 	"~n   TabOid:     ~p", 
 	[NextOid, Value, Values, TableOids, Mfa, TabOid]),
    #varbind{org_index = OrgIndex} = OrgVb,
    ?vt("validate_tab_next_res -> OrgIndex: ~p", [OrgIndex]),
    NextCompleteOid = lists:append(TabOid, NextOid),
    case snmpa_mib:lookup(get(mibserver), NextCompleteOid) of
	{table_column, #me{asn1_type = ASN1Type}, _TableEntryOid} ->
  	    ?vt("validate_tab_next_res -> ASN1Type: ~p", [ASN1Type]),
	    case ?AGENT:make_value_a_correct_value({value, Value}, ASN1Type, Mfa) of
		{error, ErrorStatus} ->
 		    ?vt("validate_tab_next_res -> "
 			"~n   ErrorStatus: ~p", [ErrorStatus]),
		    {ErrorStatus, OrgIndex};
		{value, Type, NValue} ->
 		    ?vt("validate_tab_next_res -> "
     			"~n   Type:   ~p"
			"~n   NValue: ~p", [Type, NValue]),
		    NewVb = OrgVb#varbind{oid = NextCompleteOid,
					  variabletype = Type, value = NValue},
		    validate_tab_next_res(Values, TableOids, Mfa,
					  [NewVb | Res], TabOid, TabNextOid, I)
	    end;
	Error ->
	    ?LIB:user_err("Invalid oid ~w from ~w (get_next). Using genErr => ~p",
                          [NextOid, Mfa, Error]),
	    {genErr, OrgIndex}
    end;
validate_tab_next_res([endOfTable | Values],
		      [{_ColNo, OrgVb, _Index} | TableOids],
		      Mfa, Res, TabOid, TabNextOid, I) ->
     ?vt("validate_tab_next_res(endOfTable) -> entry with: "
	 "~n   Values:     ~p"
	 "~n   OrgVb:      ~p"
	 "~n   TableOids:  ~p"
	 "~n   Mfa:        ~p"
	 "~n   Res:        ~p"
	 "~n   TabOid:     ~p"
	 "~n   TabNextOid: ~p"
	 "~n   I:          ~p",
	 [Values, OrgVb, TableOids, Mfa, Res, TabOid, TabNextOid, I]),
    NewVb = OrgVb#varbind{value = {endOfTable, TabNextOid}},
    validate_tab_next_res(Values, TableOids, Mfa, [NewVb | Res],
			  TabOid, TabNextOid, I);
validate_tab_next_res([], [], _Mfa, Res, _TabOid, _TabNextOid, _I) ->
    Res;
validate_tab_next_res([], [{_Col, _OrgVb, Index}|_], Mfa, _Res, _, _, _I) ->
    ?LIB:user_err("Too few values returned from ~w (get_next)", [Mfa]),
    {genErr, Index};
validate_tab_next_res({genErr, ColNumber}, OrgCols,
		      Mfa, _Res, _TabOid, _TabNextOid, _I) ->
    OrgIndex = snmpa_svbl:col_to_orgindex(ColNumber, OrgCols),
    ?AGENT:validate_err(table_next, {genErr, OrgIndex}, Mfa);
validate_tab_next_res({error, Reason}, [{_ColNo, OrgVb, _Index} | _TableOids],
		      Mfa, _Res, _TabOid, _TabNextOid, _I) ->
    #varbind{org_index = OrgIndex} = OrgVb,
    ?LIB:user_err("Erroneous return value ~w from ~w (get_next)",
                  [Reason, Mfa]),
    {genErr, OrgIndex};
validate_tab_next_res(Error, [{_ColNo, OrgVb, _Index} | _TableOids],
		      Mfa, _Res, _TabOid, _TabNextOid, _I) ->
    #varbind{org_index = OrgIndex} = OrgVb,
    ?LIB:user_err("Invalid return value ~w from ~w (get_next)",
                  [Error, Mfa]),
    {genErr, OrgIndex};
validate_tab_next_res(TooMany, [], Mfa, _Res, _, _, I) ->
    ?LIB:user_err("Too many values ~w returned from ~w (get_next)",
                  [TooMany, Mfa]),
    {genErr, I}.

%%-----------------------------------------------------------------
%% Func: get_next_sa/4
%% Purpose: Loop the list of varbinds for the subagent.
%%          Call subagent_get_next to retreive
%%          the next varbinds.
%% Returns: {ok, ListOfNewVbs, ListOfEndOfMibViewsVbs} |
%%          {ErrorStatus, ErrorIndex}
%%-----------------------------------------------------------------
get_next_sa(SAPid, SAOid, SAVbs, MibView) ->
    case catch ?AGENT:subagent_get_next(SAPid, MibView, SAVbs) of
	{noError, 0, NewVbs} ->
	    NewerVbs = transform_sa_next_result(NewVbs,SAOid,next_oid(SAOid)),
	    {ResVBs, EndOfVBs} = ?LIB:split_vbs(NewerVbs),
            {ok, ResVBs, EndOfVBs};
	{ErrorStatus, ErrorIndex, _} ->
	    {ErrorStatus, ErrorIndex};
	{'EXIT', Reason} ->
	    ?LIB:user_err("Lost contact with subagent (next) ~w. Using genErr",
                          [Reason]),
	    {genErr, 0}
    end.

%%-----------------------------------------------------------------
%% Check for wrong prefix returned or endOfMibView, and convert
%% into {endOfMibView, SANextOid}.
%%-----------------------------------------------------------------
transform_sa_next_result([Vb | Vbs], SAOid, SANextOid)
  when Vb#varbind.value =:= endOfMibView ->
    [Vb#varbind{value = {endOfMibView, SANextOid}} |
     transform_sa_next_result(Vbs, SAOid, SANextOid)];
transform_sa_next_result([Vb | Vbs], SAOid, SANextOid) ->
    case lists:prefix(SAOid, Vb#varbind.oid) of
	true ->
	    [Vb | transform_sa_next_result(Vbs, SAOid, SANextOid)];
	_ ->
	    [Vb#varbind{oid = SANextOid, value = {endOfMibView, SANextOid}} |
	     transform_sa_next_result(Vbs, SAOid, SANextOid)]
    end;
transform_sa_next_result([], _SAOid, _SANextOid) ->
    [].


next_oid(Oid) ->
    case lists:reverse(Oid) of
	[H | T] -> lists:reverse([H+1 | T]);
	[] -> []
    end.



%%%-----------------------------------------------------------------
%%% 5. GET-BULK REQUEST
%%% 
%%% In order to prevent excesses in reply sizes there are two 
%%% preventive methods in place. One is to check that the encode
%%% size does not exceed Max PDU size (this is mentioned in the
%%% standard). The other is a simple VBs limit. That is, the 
%%% resulting response cannot contain more then this number of VBs.
%%%-----------------------------------------------------------------

do_get_bulk(MibView, NonRepeaters, MaxRepetitions,
            PduMS, Varbinds, GbMaxVBs, _Extra) ->
    ?vtrace("do_get_bulk -> entry with"
	    "~n   MibView:        ~p"
	    "~n   NonRepeaters:   ~p"
	    "~n   MaxRepetitions: ~p"
	    "~n   PduMS:          ~p"
	    "~n   Varbinds:       ~p"
	    "~n   GbMaxVBs:       ~p",
	    [MibView, NonRepeaters, MaxRepetitions, PduMS, Varbinds, GbMaxVBs]),
    {NonRepVbs, RestVbs} = ?LIB:split_vbs_gb(NonRepeaters, Varbinds),
    ?vt("do_get_bulk -> split: "
	"~n   NonRepVbs: ~p"
	"~n   RestVbs:   ~p", [NonRepVbs, RestVbs]),
    case do_get_next2(MibView, NonRepVbs, GbMaxVBs) of
	{noError, 0, UResNonRepVbs} ->
	    ?vt("do_get_bulk -> next noError: "
		"~n   UResNonRepVbs: ~p", [UResNonRepVbs]),
	    ResNonRepVbs = lists:keysort(#varbind.org_index, UResNonRepVbs),
	    %% Decode the first varbinds, produce a reversed list of
	    %% listOfBytes.
	    case (catch enc_vbs(PduMS - ?empty_pdu_size, ResNonRepVbs)) of
 		{error, Idx, Reason} ->
		    ?LIB:user_err("failed encoding varbind ~w:~n~p", [Idx, Reason]),
                    {genErr, Idx, []};
                {SizeLeft, Res} when is_integer(SizeLeft) and is_list(Res) ->
 		    ?vtrace("do_get_bulk -> encoded: "
			    "~n   SizeLeft: ~p"
			    "~n   Res:      ~w", [SizeLeft, Res]),
		    case (catch do_get_rep(SizeLeft, MibView, MaxRepetitions,
					   RestVbs, Res, 
					   length(UResNonRepVbs), GbMaxVBs)) of
			{error, Idx, Reason} ->
			    ?LIB:user_err("failed encoding varbind ~w:~n~p", 
                                          [Idx, Reason]),
			    {genErr, Idx, []};
			Res when is_list(Res) ->
			    ?vtrace("do get bulk -> Res: "
				    "~n   ~w", [Res]),
			    {noError, 0, conv_res(Res)};
			{noError, 0, Data} = OK ->
			    ?vtrace("do get bulk -> OK: "
				    "~n   length(Data): ~w", [length(Data)]),
			    OK;
			Else ->
			    ?vtrace("do get bulk -> Else: "
				    "~n   ~w", [Else]),
			    Else
		    end;
		Res when is_list(Res) ->
		    {noError, 0, conv_res(Res)}
	    end;

	{ErrorStatus, Index, _} ->
	    ?vdebug("do get bulk: "
		    "~n   ErrorStatus: ~p"
		    "~n   Index:       ~p",[ErrorStatus, Index]),
	    {ErrorStatus, Index, []}
    end.

enc_vbs(SizeLeft, Vbs) ->
    ?vt("enc_vbs -> entry with"
	"~n   SizeLeft: ~w", [SizeLeft]),
    Fun = fun(Vb, {Sz, Res}) when Sz > 0 ->
		  ?vt("enc_vbs -> (fun) entry with"
		      "~n   Vb:  ~p"
		      "~n   Sz:  ~p"
		      "~n   Res: ~w", [Vb, Sz, Res]),
		  case (catch snmp_pdus:enc_varbind(Vb)) of
		      {'EXIT', Reason} ->
			  ?vtrace("enc_vbs -> encode failed: "
				  "~n   Reason: ~p", [Reason]),
			  throw({error, Vb#varbind.org_index, Reason});
		      X ->
			  ?vt("enc_vbs -> X: ~w", [X]),
			  Lx = length(X),
			  ?vt("enc_vbs -> Lx: ~w", [Lx]),
			  if
			      Lx < Sz ->
				  {Sz - length(X), [X | Res]};
			      true ->
				  throw(Res)
			  end
		  end;
	     (_Vb, {_Sz, [_H | T]}) ->
		  ?vt("enc_vbs -> (fun) entry with"
		      "~n   T: ~p", [T]),
		  throw(T);
	     (_Vb, {_Sz, []}) ->
		  ?vt("enc_vbs -> (fun) entry", []),
		  throw([])
	  end,
    lists:foldl(Fun, {SizeLeft, []}, Vbs).

do_get_rep(Sz, MibView, MaxRepetitions, Varbinds, Res, GbNumVBs, GbMaxVBs) 
  when MaxRepetitions >= 0 ->
    do_get_rep(Sz, MibView, 0, MaxRepetitions, Varbinds, Res, 
	       GbNumVBs, GbMaxVBs);
do_get_rep(Sz, MibView, _MaxRepetitions, Varbinds, Res, GbNumVBs, GbMaxVBs) ->
    do_get_rep(Sz, MibView, 0, 0, Varbinds, Res, GbNumVBs, GbMaxVBs).

conv_res(ResVarbinds) ->
    conv_res(ResVarbinds, []).
conv_res([VbListOfBytes | T], Bytes) ->
    conv_res(T, VbListOfBytes ++ Bytes);
conv_res([], Bytes) ->
    Bytes.

%% The only other value, then a positive integer, is infinity.
do_get_rep(_Sz, _MibView, Count, Max, _, _Res, GbNumVBs, GbMaxVBs) 
  when (is_integer(GbMaxVBs) andalso (GbNumVBs > GbMaxVBs)) ->
    ?vinfo("Max Get-BULK VBs limit (~w) exceeded (~w) when:"
	   "~n   Count: ~p"
	   "~n   Max:   ~p", [GbMaxVBs, GbNumVBs, Count, Max]),
    {tooBig, 0, []};
do_get_rep(_Sz, _MibView, Max, Max, _, Res, _GbNumVBs, _GbMaxVBs) ->
    ?vt("do_get_rep -> done when: "
	"~n   Res: ~p", [Res]),
    {noError, 0, conv_res(Res)};
do_get_rep(Sz, MibView, Count, Max, Varbinds, Res, GbNumVBs, GbMaxVBs) -> 
    ?vt("do_get_rep -> entry when: "
	"~n   Sz:    ~p"
	"~n   Count: ~p"
	"~n   Res:   ~w", [Sz, Count, Res]),
    case try_get_bulk(Sz, MibView, Varbinds, GbMaxVBs) of
	{noError, NextVarbinds, SizeLeft, Res2} -> 
	    ?vt("do_get_rep -> noError: "
		"~n   SizeLeft: ~p"
		"~n   Res2:     ~p", [SizeLeft, Res2]),
	    do_get_rep(SizeLeft, MibView, Count+1, Max, NextVarbinds,
		       Res2 ++ Res, 
		       GbNumVBs + length(Varbinds), GbMaxVBs);
	{endOfMibView, _NextVarbinds, _SizeLeft, Res2} -> 
	    ?vt("do_get_rep -> endOfMibView: "
		"~n   Res2: ~p", [Res2]),
	    {noError, 0, conv_res(Res2 ++ Res)};
	{ErrorStatus, Index} ->
	    ?vtrace("do_get_rep -> done when error: "
		    "~n   ErrorStatus: ~p"
		    "~n   Index:       ~p", [ErrorStatus, Index]),
	    {ErrorStatus, Index, []}
    end.

try_get_bulk(Sz, MibView, Varbinds, GbMaxVBs) -> 
    ?vt("try_get_bulk -> entry with"
	"~n   Sz:       ~w"
	"~n   MibView:  ~w"
	"~n   Varbinds: ~w", [Sz, MibView, Varbinds]),
    case do_get_next2(MibView, Varbinds, GbMaxVBs) of
	{noError, 0, UNextVarbinds} -> 
	    ?vt("try_get_bulk -> noError: "
		"~n   UNextVarbinds: ~p", [UNextVarbinds]),
	    NextVarbinds = ?LIB:org_index_sort_vbs(UNextVarbinds),
	    case (catch enc_vbs(Sz, NextVarbinds)) of
		{error, Idx, Reason} ->
		    ?LIB:user_err("failed encoding varbind ~w:~n~p", [Idx, Reason]),
		    ?vtrace("try_get_bulk -> encode error: "
			    "~n   Idx:    ~p"
			    "~n   Reason: ~p", [Idx, Reason]),
		    {genErr, Idx};
		{SizeLeft, Res} when is_integer(SizeLeft) andalso 
				     is_list(Res) ->
		    ?vt("try get bulk -> encode ok: "
			"~n   SizeLeft: ~w"
			"~n   Res:      ~w", [SizeLeft, Res]),
		    {check_end_of_mibview(NextVarbinds),
		     NextVarbinds, SizeLeft, Res};
		Res when is_list(Res) ->
		    ?vt("try get bulk -> Res: "
			"~n   ~w", [Res]),
		    {endOfMibView, [], 0, Res}
	    end;
	{ErrorStatus, Index, _} ->
	    ?vt("try_get_bulk -> error: "
		"~n   ErrorStatus: ~p"
		"~n   Index:       ~p", [ErrorStatus, Index]),
	    {ErrorStatus, Index}
    end.

%% If all variables in this pass are endOfMibView,
%% there is no reason to continue.
check_end_of_mibview([#varbind{value = endOfMibView} | T]) ->
    check_end_of_mibview(T);
check_end_of_mibview([]) -> endOfMibView;
check_end_of_mibview(_) -> noError.



