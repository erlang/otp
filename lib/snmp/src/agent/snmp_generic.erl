%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2017. All Rights Reserved.
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
-module(snmp_generic).

%% Avoid warning for local function error/1 clashing with autoimported BIF.
-compile({no_auto_import,[error/1]}).
-export([variable_func/2, variable_func/3, variable_get/1, variable_set/2]).
-export([table_func/2, table_func/4, 
	 table_set_row/5, table_set_cols/3, table_set_cols/4,
	 table_row_exists/2, table_foreach/2, table_foreach/3,
	 table_try_row/4, table_get_row/2, table_get_row/3, 
         table_get_elements/3, table_get_elements/4, table_get_element/3,
	 table_set_element/4, table_set_elements/3,
         table_next/2, handle_table_next/6, 
	 table_try_make_consistent/3, table_max_col/2,
         find_col/2, table_check_status/5, 
	 table_find/3,split_index_to_keys/2, init_defaults/2, init_defaults/3,
	 table_info/1,
	 try_apply/2, get_own_indexes/2, table_create_rest/6,
	 handle_table_get/4, variable_inc/2,
	 get_status_col/2, get_table_info/2, get_index_types/1]).

-include("STANDARD-MIB.hrl").
-include("snmp_types.hrl").

-define(VMODULE,"GENERIC").
-include("snmp_verbosity.hrl").

-ifndef(default_verbosity).
-define(default_verbosity,silence).
-endif.


%%%-----------------------------------------------------------------
%%% Generic functions for implementing software tables
%%% and variables. 
%%%-----------------------------------------------------------------
%% NameDb is {TableName, Db} where Db is volatile | persistent | mnesia

%%------------------------------------------------------------------
%% Access functions to the database.
%%------------------------------------------------------------------
variable_get({Name, mnesia}) ->
    snmp_generic_mnesia:variable_get(Name);
variable_get(NameDb) ->                   % ret {value, Val} | undefined
    snmpa_local_db:variable_get(NameDb).
variable_set({Name, mnesia}, Val) ->
    snmp_generic_mnesia:variable_set(Name, Val);
variable_set(NameDb, Val) ->              % ret true
    snmpa_local_db:variable_set(NameDb, Val).

variable_inc({Name, mnesia}, N) ->
    snmp_generic_mnesia:variable_inc(Name, N);
variable_inc(NameDb, N) ->              % ret true
    snmpa_local_db:variable_inc(NameDb, N).

%%-----------------------------------------------------------------
%% Returns: {value, Val} | undefined
%%
%% snmpa_local_db overloads (for performance reasons? (mbj?))
%%-----------------------------------------------------------------
table_get_element({Name, volatile}, RowIndex, Col) ->
    snmpa_local_db:table_get_element({Name, volatile}, RowIndex, Col);
table_get_element({Name, persistent}, RowIndex, Col) ->
    snmpa_local_db:table_get_element({Name, persistent}, RowIndex, Col);
table_get_element(NameDb, RowIndex, Col) ->
    TableInfo = table_info(NameDb),
    case handle_table_get(NameDb,RowIndex,[Col],
			  TableInfo#table_info.first_own_index) of
	[{value, Val}] -> {value, Val};
	_ -> undefined
    end.

table_get_elements(NameDb, RowIndex, Cols) ->
    TableInfo = snmp_generic:table_info(NameDb),
    table_get_elements(NameDb, RowIndex, Cols,
		       TableInfo#table_info.first_own_index).

%%----------------------------------------------------------------------
%% Returns: list of vals | undefined
%%----------------------------------------------------------------------
table_get_elements({Name, mnesia}, RowIndex, Cols, FirstOwnIndex) ->
    ?vtrace("table_get_elements(mnesia) -> entry with"
	    "~n   Name:          ~p"
	    "~n   RowIndex:      ~p"
	    "~n   Cols:          ~p"
	    "~n   FirstOwnIndex: ~p", [Name, RowIndex, Cols, FirstOwnIndex]),
    snmp_generic_mnesia:table_get_elements(Name, RowIndex, Cols, FirstOwnIndex);
table_get_elements(NameDb, RowIndex, Cols, FirstOwnIndex) -> 
    ?vtrace("table_get_elements -> entry with"
	    "~n   NameDb:        ~p"
	    "~n   RowIndex:      ~p"
	    "~n   Cols:          ~p"
	    "~n   FirstOwnIndex: ~p", [NameDb, RowIndex, Cols, FirstOwnIndex]),
    snmpa_local_db:table_get_elements(NameDb, RowIndex, Cols, FirstOwnIndex).


%% ret true
table_set_element({Name,mnesia}, RowIndex, Col, NewVal) -> 
    snmp_generic_mnesia:table_set_elements(Name, RowIndex,
					   [{Col, NewVal}]);
table_set_element(NameDb, RowIndex, Col, NewVal) ->
    snmpa_local_db:table_set_elements(NameDb, RowIndex, [{Col, NewVal}]).

table_set_elements({Name, mnesia}, RowIndex, Cols) ->
    snmp_generic_mnesia:table_set_elements(Name, RowIndex, Cols);
table_set_elements(NameDb, RowIndex, Cols) -> % ret true
    snmpa_local_db:table_set_elements(NameDb, RowIndex, Cols).

table_next({Name, mnesia}, RestOid) ->
    snmp_generic_mnesia:table_next(Name, RestOid);
table_next(NameDb, RestOid) ->              % ret RRestOid | endOfTable
    snmpa_local_db:table_next(NameDb, RestOid).
table_max_col(NameDb, Col) ->               % ret largest element in Col
                                            % in the table NameDb.
    snmpa_local_db:table_max_col(NameDb, Col).


%%------------------------------------------------------------------
%% Theses functions could be in the MIB for simple 
%% variables or tables, i.e. vars without complex 
%% set-operations. If there are complex set op, an
%% extra layer-function should be added, and that
%% function should be in the MIB, and it can call these
%% functions.
%% The MIB functions just provide the table name, column
%% and a list of the keys for the table.
%%------------------------------------------------------------------

%%------------------------------------------------------------------
%% Variables
%%------------------------------------------------------------------
%%------------------------------------------------------------------
%% This is the default function for variables.
%%------------------------------------------------------------------
 
variable_func(new, NameDb) ->
    case variable_get(NameDb) of
	{value, _} -> ok;
	undefined ->
	    #variable_info{defval = Defval} = variable_info(NameDb),
	    variable_set(NameDb, Defval)
    end;

variable_func(delete, _NameDb) ->
    ok;

variable_func(get, NameDb) ->
    case variable_get(NameDb) of
	{value, Val} -> {value, Val};
	_ -> genErr
    end.

variable_func(is_set_ok, _Val, _NameDb) ->
    noError;
variable_func(set, Val, NameDb) ->
    case variable_set(NameDb, Val) of
	true -> noError;
	false -> commitFailed
    end;
variable_func(undo, _Val, _NameDb) ->
    noError.

%%------------------------------------------------------------------
%% Tables
%% Assumes the RowStatus is the last column in the
%% table.
%%------------------------------------------------------------------
%%------------------------------------------------------------------
%% This is the default function for tables.
%%
%% NameDb       is the name of the table (atom)
%% RowIndex   is a flat list of the indexes for the row.
%% Col        is the column number.
%%------------------------------------------------------------------
%% Each database implements its own table_func
%%------------------------------------------------------------------
table_func(Op, {Name, mnesia}) ->
    snmp_generic_mnesia:table_func(Op, Name);

table_func(Op, NameDb) ->
    snmpa_local_db:table_func(Op, NameDb).

table_func(Op, RowIndex, Cols, {Name, mnesia}) ->
    snmp_generic_mnesia:table_func(Op, RowIndex, Cols, Name);

table_func(Op, RowIndex, Cols, NameDb) ->
    snmpa_local_db:table_func(Op, RowIndex, Cols, NameDb).

%%----------------------------------------------------------------------
%% DB independent.
%%----------------------------------------------------------------------
handle_table_get(NameDb, RowIndex, Cols, FirstOwnIndex) ->
    case table_get_elements(NameDb, RowIndex, Cols, FirstOwnIndex) of
	undefined -> 
	    ?vdebug("handle_table_get -> undefined", []),
	    make_list(length(Cols), {noValue, noSuchInstance});
	Res -> 
	    ?vtrace("handle_table_get -> Res: ~n   ~p", [Res]),
	    validate_get(Cols, Res)
    end.

validate_get([_Col | Cols], [Res | Ress]) ->
    NewVal = 
	case Res of
	    noinit -> {noValue, unSpecified};
	    noacc -> {noAccess, unSpecified};
	    Val -> {value, Val}
	end,
    [NewVal | validate_get(Cols, Ress)];
validate_get([], []) -> [].

make_list(N, X) when N > 0 -> [X | make_list(N-1, X)];
make_list(_, _) -> [].

table_foreach(Tab, Fun) ->
    ?vdebug("apply fun to all in table ~w",[Tab]),
    table_foreach(Tab, Fun, undefined, []).
table_foreach(Tab, Fun, FOI) ->
    ?vdebug("apply fun to all in table ~w",[Tab]),
    table_foreach(Tab, Fun, FOI, []).
table_foreach(Tab, Fun, FOI, Oid) ->
    case table_next(Tab, Oid) of
	endOfTable ->
	    ?vdebug("end of table",[]),
	    ok;
	Oid ->
	    %% OOUPS, circular ref, major db fuckup
	    ?vinfo("cyclic reference: ~w -> ~w",[Oid,Oid]),
	    exit({cyclic_db_reference,Oid});
	NextOid ->
	    ?vtrace("get row for oid ~w",[NextOid]),
	    case table_get_row(Tab, NextOid, FOI) of
		undefined -> ok;
		Row -> 
		    ?vtrace("row: ~w",[Row]),
		    Fun(NextOid, Row)
	    end,
	    table_foreach(Tab, Fun, FOI, NextOid)
    end.

%%------------------------------------------------------------------
%% Used to implement next, and to find next entry's
%% keys in a table when not all of the keys are known.
%%
%% FirstCol is the first column in the search.
%% LastCol is the last column.
%% Col is the current column.
%% If Col is less than FirstCol, (or not present), the
%% search shall begin in the first row (no indexes) of
%% column FirstCol.
%% Returns: List of endOfTable | {NextOid, Value}
%%------------------------------------------------------------------
handle_table_next(_NameDb, _RowIndex, [], _FirstCol, _FOI, _LastCol) ->
    [];
handle_table_next(NameDb, RowIndex, OrgCols, FirstCol, FOI, LastCol) ->
    FirstVals = 
	case split_cols(OrgCols, FirstCol, LastCol) of
	    {[], Cols, LastCols} ->
		[];
	    {FirstCols, Cols, LastCols} ->
		handle_table_next(NameDb, [], FirstCols, FirstCol, FOI, LastCol)
	end,
    NextVals = 
	case table_next(NameDb, RowIndex) of
	    endOfTable -> 
		{NewCols, EndOfTabs} = make_new_cols(Cols, LastCol),
		NewVals = 
		    handle_table_next(NameDb, [], NewCols,FirstCol,FOI,LastCol),
		lists:append(NewVals, EndOfTabs);
	    NextIndex ->  
		% We found next Row; check if all Cols are initialized.
		Row = table_get_elements(NameDb, NextIndex, Cols, FOI),
		check_all_initalized(Row,Cols,NameDb,NextIndex,
				     FirstCol, FOI, LastCol)
	end,
    lists:append([FirstVals, NextVals, LastCols]).

%% Split into three parts A,B,C; A < FirstCol =<  B =<  LastCol < C
split_cols([Col | Cols], FirstCol, LastCol) when Col < FirstCol ->
    {A, B, C} = split_cols(Cols, FirstCol, LastCol),
    {[FirstCol | A], B, C};
split_cols([Col | Cols], FirstCol, LastCol) when Col > LastCol ->
    {A, B, C} = split_cols(Cols, FirstCol, LastCol),
    {A, B, [endOfTable | C]};
split_cols([Col | Cols], FirstCol, LastCol)  ->
    {A, B, C} = split_cols(Cols, FirstCol, LastCol),
    {A, [Col | B], C};
split_cols([], _FirstCol, _LastCol) ->
    {[], [], []}.

%% Add 1 to each col < lastcol. Otherwise make it into
%% endOfTable.
make_new_cols([Col | Cols], LastCol) when Col < LastCol ->
    {NewCols, Ends} = make_new_cols(Cols, LastCol),
    {[Col+1 | NewCols], Ends};
make_new_cols([_Col | Cols], LastCol) ->
    {NewCols, Ends} = make_new_cols(Cols, LastCol),
    {NewCols, [endOfTable | Ends]};
make_new_cols([], _LastCol) ->
    {[], []}.

check_all_initalized([noinit|Vals],[Col|Cols],Name,RowIndex,
		     FirstCol, FOI, LastCol) ->
    [NextValForThisCol] = 
	handle_table_next(Name, RowIndex, [Col], FirstCol, FOI, LastCol),
    [NextValForThisCol | 
     check_all_initalized(Vals, Cols, Name, RowIndex, FirstCol, FOI, LastCol)];
check_all_initalized([noacc|Vals],[Col|Cols],Name,RowIndex,
		     FirstCol, FOI, LastCol) ->
    [NextValForThisCol] = 
	handle_table_next(Name, RowIndex, [Col], FirstCol, FOI, LastCol),
    [NextValForThisCol | 
     check_all_initalized(Vals, Cols, Name, RowIndex, FirstCol, FOI, LastCol)];
check_all_initalized([Val | Vals], [Col | Cols], Name, RowIndex, 
		     FirstCol, FOI, LastCol) ->
    [{[Col | RowIndex], Val} |
     check_all_initalized(Vals, Cols, Name, RowIndex, FirstCol, FOI, LastCol)];
check_all_initalized([], [], _Name, _RowIndex, _FirstCol, _FOI, _LastCol) ->
    [].
    

%%------------------------------------------------------------------
%%  Implements is_set_ok. 
%%------------------------------------------------------------------
%% TryChangeStatusFunc is a function that will be
%% called if the rowstatus column is changed.
%% Arguments: (StatusVal, RowIndex, Cols)
%% Two cases:
%% 1) Last col is RowStatus - check status
%% 2) No modification to RowStatus - check that row exists.
%%------------------------------------------------------------------
table_try_row(_NameDb, _TryChangeStatusFunc, _RowIndex, []) -> {noError, 0};
table_try_row(NameDb, TryChangeStatusFunc, RowIndex, Cols) ->
    #table_info{status_col = StatusCol} = table_info(NameDb),
    case lists:keysearch(StatusCol, 1, Cols) of
	{value, {StatusCol, Val}} ->
	    case table_check_status(NameDb, StatusCol, 
				    Val, RowIndex, Cols) of
		{noError, 0} ->
		    try_apply(TryChangeStatusFunc, [NameDb, Val,
						    RowIndex, Cols]);
		Error -> Error
	    end;
	_ -> 
	    case table_row_exists(NameDb, RowIndex) of
		true -> {noError, 0};
		false ->
		    [{ColNo, _Val}|_] = Cols,
		    {inconsistentName, ColNo}
	    end
    end.

%%------------------------------------------------------------------
%% table_check_status can be used by the is_set_ok
%% procedure of all tables, to check the
%% status variable, if present in Cols.
%% table_check_status(NameDb, Col, Val, RowIndex, Cols) ->
%% NameDb    : the name of the table
%% Col       : the columnnumber of RowStatus
%% Val       : the value of the RowStatus Col
%%------------------------------------------------------------------

%% Try to make the row active. Ok if status != notReady
%% If it is notReady, make sure no row has value noinit.
table_check_status(NameDb, Col, ?'RowStatus_active', RowIndex, Cols) ->
    case table_get_row(NameDb, RowIndex) of
	Row when is_tuple(Row) andalso 
		 (element(Col, Row) =:= ?'RowStatus_notReady') ->
	    case is_any_noinit(Row, Cols) of
		false -> {noError, 0};
		true -> {inconsistentValue, Col}
	    end;
	undefined -> {inconsistentValue, Col};
	_Else -> {noError, 0}
    end;

%% Try to make the row inactive. Ok if status != notReady
table_check_status(NameDb, Col, ?'RowStatus_notInService', RowIndex, Cols) ->
    case table_get_row(NameDb, RowIndex) of
	Row when is_tuple(Row) andalso 
		 (element(Col, Row) =:= ?'RowStatus_notReady') ->
	    case is_any_noinit(Row, Cols) of
		false -> {noError, 0};
		true -> {inconsistentValue, Col}
	    end;
	undefined -> {inconsistentValue, Col};
	_Else -> {noError, 0}
    end;

%% Try to createAndGo
%% Ok if values are provided, or default values can be used for
%% all columns.
table_check_status(NameDb, Col, ?'RowStatus_createAndGo', RowIndex, Cols) ->
    case table_row_exists(NameDb, RowIndex) of
	false -> 
	    % it's ok to use snmpa_local_db:table_construct_row since it's
	    % side effect free and we only use the result temporary.
	    try snmpa_local_db:table_construct_row(
			 NameDb, RowIndex, ?'RowStatus_createAndGo', Cols) of
		Row ->
		    case lists:member(noinit, tuple_to_list(Row)) of
			false -> {noError, 0};
			_Found -> {inconsistentValue, Col}
		    end
            catch
                _:_Reason ->
		    ?vtrace(
		       "failed construct row (createAndGo): "
		       " n   Reason: ~p"
		       " n   Stack:  ~p",
		       [_Reason, erlang:get_stacktrace()]),
		    {noCreation, Col}           % Bad RowIndex
	    end;
	true -> {inconsistentValue, Col}
    end;

%% Try to createAndWait - ok if row doesn't exist.
table_check_status(NameDb, Col, ?'RowStatus_createAndWait', RowIndex, Cols) ->
    case table_row_exists(NameDb, RowIndex) of
	false ->
	    try snmpa_local_db:table_construct_row(
			 NameDb, RowIndex, ?'RowStatus_createAndGo', Cols) of
		_Row ->
		    {noError, 0}
            catch
                _:_Reason ->
		    ?vtrace(
		       "failed construct row (createAndWait): "
		       " n   Reason: ~p"
		       " n   Stack:  ~p",
		       [_Reason, erlang:get_stacktrace()]),
		    {noCreation, Col}           % Bad RowIndex
	    end;
	true -> {inconsistentValue, Col}
    end;

%% Try to destroy
table_check_status(_NameDb, _Col, ?'RowStatus_destroy', _RowIndex, _Cols) ->
    {noError, 0};
    
%% Otherwise, notReady. It isn't possible to set a row to notReady.
table_check_status(_NameDb, Col, _, _RowIndex, _Cols) ->
    {inconsistentValue, Col}.

is_any_noinit(Row, Cols) ->
    is_any_noinit(tuple_to_list(Row), Cols, 1).
is_any_noinit([noinit | Vals], [{N, _Value} | Cols], N) ->
    is_any_noinit(Vals, Cols, N+1);
is_any_noinit([noinit | _Vals], _Cols, _N) ->
    true;
is_any_noinit([_ | Vals], [{N, _Value} | Cols], N) ->
    is_any_noinit(Vals, Cols, N+1);
is_any_noinit([_ | Vals], Cols, N) ->
    is_any_noinit(Vals, Cols, N+1);
is_any_noinit([], _, _) ->
    false.

%%------------------------------------------------------------------
%%  Implements set.
%% ChangedStatusFunc is a function that will be
%%   called if the rowstatus column is changed.
%%   The function is called *after* the row is created or
%%   otherwise modified, but *before* it is deleted.
%%   Arguments: (StatusVal, RowIndex, Cols)
%% ConsFunc is a consistency-check function which will
%%   be called with the RowIndex of this row, if
%%   no operation on the row is made, when
%%   all columns are set, OR when row is createAndWait:ed.
%%   This is useful when the RowStatus
%%   could change, e.g. if the manager has provided all
%%   mandatory columns in this set operation.
%%   If it is nofunc, no function will be called after all
%%   sets.
%%------------------------------------------------------------------
table_set_row(_NameDb, _, _, _RowIndex, []) -> {noError, 0};
table_set_row(NameDb, ChangedStatusFunc, ConsFunc, RowIndex, Cols) ->
    #table_info{status_col = StatusCol} = table_info(NameDb),
    case lists:keysearch(StatusCol, 1, Cols) of
	{value, {StatusCol, Val}} ->
	    table_set_status(NameDb, RowIndex, Val, StatusCol, 
			     Cols, ChangedStatusFunc, ConsFunc);
	_ -> table_set_cols(NameDb, RowIndex, Cols, ConsFunc)
    end.
    
%%----------------------------------------------------------------------
%% Mnesia overloads for performance reasons.
%%----------------------------------------------------------------------
table_set_status({Name, mnesia}, RowIndex, Status, StatusCol, Cols, 
		 ChangedStatusFunc, ConsFunc) ->
    snmp_generic_mnesia:table_set_status(Name, RowIndex,
					 Status, StatusCol, Cols, 
					 ChangedStatusFunc, ConsFunc);

table_set_status(NameDb,RowIndex, Status, StatusCol, Cols,
		 ChangedStatusFunc,ConsFunc) ->
    snmpa_local_db:table_set_status(NameDb, RowIndex,
				   Status, StatusCol, Cols, 
				   ChangedStatusFunc, ConsFunc).

init_defaults(Defs, InitRow) ->
    table_defaults(InitRow, Defs).
init_defaults(Defs, InitRow, StartCol) ->
    table_defaults(InitRow, StartCol, Defs).
%%-----------------------------------------------------------------
%% Get, from a list of Keys, the Keys defined in this table.
%% (e.g. if INDEX { ifIndex, myOwnIndex }, the Keys is a list
%% of two elements, and returned from this func is a list of
%% the last of the two.)
%%-----------------------------------------------------------------
get_own_indexes(0, _Keys) -> [];
get_own_indexes(1, Keys) -> Keys;
get_own_indexes(Index, [_Key | Keys]) ->
    get_own_indexes(Index - 1, Keys).

%%-----------------------------------------------------------------
%% Creates everything but the INDEX columns.
%% Pre: The StatusColumn is present
%% Four cases:
%% 0) If a column is 'not-accessible' => use noacc
%% 1) If no value is provided for the column and column is
%%    not StatusCol => use noinit
%% 2) If column is not StatusCol, use the provided value
%% 3) If column is StatusCol, use Status
%%-----------------------------------------------------------------
table_create_rest(Col, Max, _ , _ , [], _NoAcc) when Col > Max -> [];
table_create_rest(Col,Max,StatusCol,Status,[{Col,_Val}|Defs],[Col|NoAccs]) ->
    % case 0
    [noacc | table_create_rest(Col+1, Max, StatusCol, Status, Defs, NoAccs)];
table_create_rest(Col,Max,StatusCol,Status,Defs,[Col|NoAccs]) ->
    % case 0
    [noacc | table_create_rest(Col+1, Max, StatusCol, Status, Defs, NoAccs)];
table_create_rest(StatCol, Max, StatCol, Status, [{_Col, _Val} |Defs], NoAccs) ->
    % case 3
    [Status | table_create_rest(StatCol+1, Max, StatCol, Status,Defs,NoAccs)];
table_create_rest(Col, Max, StatusCol, Status, [{Col, Val} |Defs],NoAccs) ->
    % case 2
    [Val | table_create_rest(Col+1, Max, StatusCol, Status,Defs,NoAccs)];
table_create_rest(StatCol, Max, StatCol, Status, Cols, NoAccs) ->
    % case 3
    [Status | table_create_rest(StatCol+1, Max, StatCol, Status, Cols, NoAccs)];
table_create_rest(Col, Max, StatusCol, Status, Cols, NoAccs) when Col =< Max->
    % case 1
    [noinit | table_create_rest(Col+1, Max, StatusCol, Status, Cols, NoAccs)].

%%------------------------------------------------------------------
%%  Sets default values to a row.
%%  InitRow is a list of values.
%%  Defs is a list of {Col, DefVal}, in Column order.
%%  Returns a new row (a list of values) with the same values as
%%  InitRow, except if InitRow has value noinit in a column, and
%%  the corresponing Col has a DefVal in Defs, then the DefVal
%%  will be the new value.
%%------------------------------------------------------------------
table_defaults(InitRow, Defs) -> table_defaults(InitRow, 1, Defs).

table_defaults([], _, _Defs) -> [];
table_defaults([noinit | T], CurIndex, [{CurIndex, DefVal} | Defs]) ->
    [DefVal | table_defaults(T, CurIndex+1, Defs)];
%% 'not-accessible' columns don't get a value
table_defaults([noacc | T], CurIndex, [{CurIndex, _DefVal} | Defs]) ->
    [noacc | table_defaults(T, CurIndex+1, Defs)];
table_defaults([Val | T], CurIndex, [{CurIndex, _DefVal} | Defs]) ->
    [Val | table_defaults(T, CurIndex+1, Defs)];
table_defaults([Val | T], CurIndex, Defs) ->
    [Val | table_defaults(T, CurIndex+1, Defs)].


%%------------------------------------------------------------------
%% table_set_cols/3,4
%% can be used by the set procedure of all tables
%% to set all columns in Cols, one at a time.
%% ConsFunc is a check-consistency function, which will
%% be called with the RowIndex of this row, when
%% all columns are set. This is useful when the RowStatus
%% could change, e.g. if the manager has provided all
%% mandatory columns in this set operation.
%% If ConsFunc is nofunc, no function will be called after all
%% sets.
%% Returns: {noError, 0} | {Error, Col}
%%------------------------------------------------------------------
%% mnesia uses its own for performance reasons.
%% -----------------------------------------------------------------
table_set_cols({Name,mnesia}, RowIndex, Cols, ConsFunc) ->
    snmp_generic_mnesia:table_set_cols(Name, RowIndex,Cols,ConsFunc);
table_set_cols(NameDb, RowIndex, Cols, ConsFunc) ->
    case table_set_cols(NameDb, RowIndex, Cols) of
	{noError, 0} -> try_apply(ConsFunc, [NameDb, RowIndex, Cols]);
	Error -> Error
    end.

table_set_cols(_NameDb, _RowIndex, []) -> 
    {noError, 0};
table_set_cols(NameDb, RowIndex, [{Col, Val} | Cols]) ->
    case catch table_set_element(NameDb, RowIndex, Col, Val) of
	true -> 
	    table_set_cols(NameDb, RowIndex, Cols);
	X ->
	    user_err("snmp_generic:table_set_cols set ~w to"
		     " ~w returned ~w",
		     [{NameDb, RowIndex}, {Col, Val}, X]),
	    {undoFailed, Col}
    end.
    
%%------------------------------------------------------------------
%% This function splits RowIndex which is part
%% of a OID, into a list of the indexes for the
%% table. So a table with indexes {integer, octet string},
%% and a RowIndex [4,3,5,6,7], will be split into
%% [4, [5,6,7]].
%%------------------------------------------------------------------
split_index_to_keys(Indexes, RowIndex) ->
    collect_keys(Indexes, RowIndex).

collect_keys([#asn1_type{bertype = 'INTEGER'} | Indexes], [IntKey | Keys]) ->
    [IntKey | collect_keys(Indexes, Keys)];
collect_keys([#asn1_type{bertype = 'Unsigned32'} | Indexes], [IntKey | Keys]) ->
    [IntKey | collect_keys(Indexes, Keys)];
collect_keys([#asn1_type{bertype = 'Counter32'} | Indexes], [IntKey | Keys]) ->
    %% Should we allow this - counter in INDEX is strange!
    [IntKey | collect_keys(Indexes, Keys)];
collect_keys([#asn1_type{bertype = 'TimeTicks'} | Indexes], [IntKey | Keys]) ->
    %% Should we allow this - timeticks in INDEX is strange!
    [IntKey | collect_keys(Indexes, Keys)];
collect_keys([#asn1_type{bertype = 'IpAddress'} | Indexes], 
	     [A, B, C, D | Keys]) ->
    [[A, B, C, D] | collect_keys(Indexes, Keys)];
%% Otherwise, check if it has constant size
collect_keys([#asn1_type{lo = X, hi = X} | Indexes], Keys)
   when is_integer(X) andalso (length(Keys) >= X) ->
    {StrKey, Rest} = collect_length(X, Keys, []),
    [StrKey | collect_keys(Indexes, Rest)];
collect_keys([#asn1_type{lo = X, hi = X} | _Indexes], Keys)
   when is_integer(X) ->
    exit({error, {size_mismatch, X, Keys}});
%% Otherwise, its a dynamic-length type => its a list
%% OBJECT IDENTIFIER, OCTET STRING or BITS (or derivatives)
%% Check if it is IMPLIED (only last element can be IMPLIED)
collect_keys([#asn1_type{implied = true}], Keys) ->
    [Keys];
collect_keys([_Type | Indexes], [Length | Keys]) when length(Keys) >= Length ->
    {StrKey, Rest} = collect_length(Length, Keys, []),
    [StrKey | collect_keys(Indexes, Rest)];
collect_keys([_Type | _Indexes], [Length | Keys]) ->
    exit({error, {size_mismatch, Length, Keys}});
collect_keys([], []) -> [];
collect_keys([], Keys) ->
    exit({error, {bad_keys, Keys}});
collect_keys(_Any, Key) -> [Key].

collect_length(0, Rest, Rts) ->
    {lists:reverse(Rts), Rest};
collect_length(N, [El | Rest], Rts) ->
    collect_length(N-1, Rest, [El | Rts]).

%%------------------------------------------------------------------
%% Checks if a certain row exists.
%% Returns true or false.
%%------------------------------------------------------------------
table_row_exists(NameDb, RowIndex) ->
    case table_get_element(NameDb, RowIndex, 1) of
	undefined -> false;
	_ -> true
    end.

%%------------------------------------------------------------------
%% table_find(NameDb, Col, Value)
%% Finds a row (if one exists) in table NameDb
%% with column Col equals to Value.
%% Returns the RowIndex of the row, or false
%% if no row exists.
%%------------------------------------------------------------------
table_find(NameDb, Col, Value) -> table_find(NameDb, Col, Value, []).
table_find(NameDb, Col, Value, Indexes) ->
    case table_next(NameDb, Indexes) of
	endOfTable ->
	    false;
	NewIndexes ->
	    case table_get_element(NameDb, NewIndexes, Col) of
		{value, Value} -> NewIndexes;
		_Else -> table_find(NameDb, Col, Value, NewIndexes)
	    end
    end.


%%------------------------------------------------------------------
%%  find_col(Col, Cols)
%%    undefined if a Col for column Col doesn't exist.
%%    {value, Val} if a Col for Col with value Val exists.
%%------------------------------------------------------------------
find_col(_Col, []) -> undefined;
find_col(Col, [{Col, Val} | _T]) -> {value, Val};
find_col(Col, [_H | T]) -> find_col(Col, T).

%%------------------------------------------------------------------
%%  check_mandatory_cols(ListOfCols, Cols)
%%     {noError 0}if all columns in ListOfCols are present in Cols.
%%     {inconsistentValue 0} otherwise. (Index = 0. It's hard to tell
%%        which Col is wrong, when the problem is that one is missing!)
%%------------------------------------------------------------------
% check_mandatory_cols([], _) -> {noError, 0};
% check_mandatory_cols(_, []) -> {inconsistentValue, 0};
% check_mandatory_cols([Col | Cols], [{Col, Val} | T]) ->
%     check_mandatory_cols(Cols, T);
% check_mandatory_cols([Col | Cols], [{Col2, Val} | T]) ->
%     check_mandatory_cols([Col | Cols], T).


try_apply(nofunc, _) -> {noError, 0};
try_apply(F, Args) -> maybe_verbose_apply(F, Args).

maybe_verbose_apply(M, Args) ->
    case get(verbosity) of
        false ->
            apply(M, Args);
        _ ->
            ?vlog("~n   apply: ~w,~p~n", [M,Args]),
            Res = apply(M,Args),
            ?vlog("~n   returned: ~p", [Res]),
            Res
    end.


table_info({Name, _Db}) ->
    case snmpa_symbolic_store:table_info(Name) of
	{value, TI} ->
	    TI;
	false ->
	    error({table_not_found, Name})
    end;
table_info(Name) ->
    case snmpa_symbolic_store:table_info(Name) of
	{value, TI} ->
	    TI;
	false ->
	    error({table_not_found, Name})
    end.

variable_info({Name, _Db}) ->
    case snmpa_symbolic_store:variable_info(Name) of
	{value, VI} ->
	    VI;
	false ->
	    error({variable_not_found, Name})
    end;
variable_info(Name) ->
    case snmpa_symbolic_store:variable_info(Name) of
	{value, VI} ->
	    VI;
	false ->
	    error({variable_not_found, Name})
    end.


%%------------------------------------------------------------------
%% This function is a simple consistency check
%% function which could be used by the user-defined
%% table functions.
%% Check if the row has all information needed to
%% make row notInService (from notReady). This is
%% a simple check, which just checks if some col
%% in the row has the value 'noinit'.
%% If it has the information, the status is changed
%% to notInService.
%%------------------------------------------------------------------
table_try_make_consistent(Name, RowIndex, _Cols) ->
    TableInfo = table_info(Name),
    case TableInfo#table_info.status_col of
	StatusCol when is_integer(StatusCol) ->
	    {value, StatusVal} = table_get_element(Name, RowIndex, StatusCol),
	    table_try_make_consistent(Name, RowIndex, StatusVal, TableInfo);
	_ ->
	    {noError, 0}
    end.
    
table_try_make_consistent(Name, RowIndex, ?'RowStatus_notReady', TableInfo) ->
    %% this *should* be a generic function, 
    %% but since mnesia got its own try_mk_cons
    %% and I don't have time to impl table_get_row 
    %% for mnesia I call snmpa_local_db:
    Row = snmpa_local_db:table_get_row(Name, RowIndex),
    case lists:member(noinit, tuple_to_list(Row)) of
	true -> {noError, 0};
	false -> 
	    case catch table_set_element(Name, RowIndex,
					 TableInfo#table_info.status_col,
					 ?'RowStatus_notInService') of
		true -> {noError, 0};
		X -> 
		    user_err("snmp_generic:table_try_make_consistent "
			     "set ~w to notInService returned ~w",
			     [{Name, RowIndex}, X]),
		    {commitFailed, TableInfo#table_info.status_col}
	    end
    end;

table_try_make_consistent(_Name, _RowIndex, _StatusVal, _TableInfo) ->
    {noError, 0}.

table_get_row({Name, mnesia}, RowIndex) ->
    snmp_generic_mnesia:table_get_row(Name, RowIndex);
table_get_row(NameDb, RowIndex) ->
    snmpa_local_db:table_get_row(NameDb, RowIndex).

table_get_row(NameDb, RowIndex, undefined) ->
    table_get_row(NameDb, RowIndex);
table_get_row({Name, mnesia}, RowIndex, FOI) ->
    snmp_generic_mnesia:table_get_row(Name, RowIndex, FOI);
table_get_row(NameDb, RowIndex, _FOI) ->
    snmpa_local_db:table_get_row(NameDb, RowIndex).


%%-----------------------------------------------------------------
%% Purpose: These functions can be used by the user's instrum func 
%%          to retrieve various table info.
%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% Description:
%% Used by user's instrum func to check if mstatus column is 
%% modified.
%%-----------------------------------------------------------------
get_status_col(Name, Cols) ->
    #table_info{status_col = StatusCol} = table_info(Name),
    case lists:keysearch(StatusCol, 1, Cols) of
	{value, {StatusCol, Val}} -> {ok, Val};
	_ -> false
    end.


%%-----------------------------------------------------------------
%% Description:
%% Used by user's instrum func to get the table info. Specific parts
%% or all of it. If all is selected then the result will be a tagged
%% list of values.
%%-----------------------------------------------------------------
get_table_info(Name, nbr_of_cols) ->
    get_nbr_of_cols(Name);
get_table_info(Name, defvals) ->
    get_defvals(Name);
get_table_info(Name, status_col) ->
    get_status_col(Name);
get_table_info(Name, not_accessible) ->
    get_not_accessible(Name);
get_table_info(Name, index_types) ->
    get_index_types(Name);
get_table_info(Name, first_accessible) ->
    get_first_accessible(Name);
get_table_info(Name, first_own_index) ->
    get_first_own_index(Name);
get_table_info(Name, all) ->
    TableInfo = table_info(Name),
    [{nbr_of_cols,      TableInfo#table_info.nbr_of_cols},
     {defvals,          TableInfo#table_info.defvals},
     {status_col,       TableInfo#table_info.status_col},
     {not_accessible,   TableInfo#table_info.not_accessible},
     {index_types,      TableInfo#table_info.index_types},
     {first_accessible, TableInfo#table_info.first_accessible},
     {first_own_index,  TableInfo#table_info.first_own_index}].


%%-----------------------------------------------------------------
%% Description:
%% Used by user's instrum func to get the index types.
%%-----------------------------------------------------------------
get_index_types(Name) ->
    #table_info{index_types = IndexTypes} = table_info(Name),
    IndexTypes.

get_nbr_of_cols(Name) ->
    #table_info{nbr_of_cols = NumberOfCols} = table_info(Name),
    NumberOfCols.

get_defvals(Name) ->
    #table_info{defvals = DefVals} = table_info(Name),
    DefVals.

get_status_col(Name) ->
    #table_info{status_col = StatusCol} = table_info(Name),
    StatusCol.

get_not_accessible(Name) ->
    #table_info{not_accessible = NotAcc} = table_info(Name),
    NotAcc.

get_first_accessible(Name) ->
    #table_info{first_accessible = FirstAcc} = table_info(Name),
    FirstAcc.

get_first_own_index(Name) ->
    #table_info{first_own_index = FirstOwnIdx} = table_info(Name),
    FirstOwnIdx.


%%-----------------------------------------------------------------

error(Reason) ->
    throw({error, Reason}).

user_err(F, A) ->
    snmpa_error:user_err(F, A).
