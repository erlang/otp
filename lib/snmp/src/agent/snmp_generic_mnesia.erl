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
-module(snmp_generic_mnesia).

-export([variable_get/1, variable_set/2, variable_inc/2]).
-export([table_func/2, table_func/4, 
	 table_set_cols/4, table_set_element/4, table_set_elements/3,
	 table_get_elements/4, table_get_row/2, table_get_row/3,
         table_next/2,table_set_status/7,
	 table_try_make_consistent/2,
	 table_delete_row/2]).

-include("STANDARD-MIB.hrl").
-include("snmp_types.hrl").
%% -include("snmp_generic.hrl").

%%%-----------------------------------------------------------------
%%% Generic functions for implementing software tables
%%% and variables.  Mnesia is used.
%%%-----------------------------------------------------------------

%%------------------------------------------------------------------
%% Theses functions could be in the MIB for simple 
%% variables or tables, i.e. vars without complex 
%% set-operations. If there are complex set op, an
%% extra layer-function should be added, and that
%% function should be in the MIB, and it can call these
%% functions.
%%------------------------------------------------------------------

%%------------------------------------------------------------------
%% Variables
%%------------------------------------------------------------------
%%------------------------------------------------------------------
%% This is the default function for variables.
%%------------------------------------------------------------------
variable_get(Name) ->
    case mnesia:dirty_read({snmp_variables, Name}) of
	[{_Db, _Name, Val}] -> {value, Val};
	_ -> undefined
    end.

variable_set(Name, Val) ->
    mnesia:dirty_write({snmp_variables, Name, Val}),
    true.

variable_inc(Name, N) ->
    case mnesia:dirty_update_counter({snmp_variables, Name}, N) of
	NewVal when NewVal < 4294967296 ->
	    ok;
	NewVal ->
	    mnesia:dirty_write({snmp_variables, Name, NewVal rem 4294967296})
    end.

%%------------------------------------------------------------------
%% Tables
%% Assumes the RowStatus is the last column in the
%% table.
%%------------------------------------------------------------------
%%------------------------------------------------------------------
%% This is the default function for tables.
%%
%% Name       is the name of the table (atom)
%% RowIndex   is a flat list of the indexes for the row.
%% Cols       is a list of column numbers.
%%------------------------------------------------------------------
table_func(new, _Name) ->
    ok;

table_func(delete, _Name) ->
    ok.

table_func(get, RowIndex, Cols, Name) ->
    TableInfo = snmp_generic:table_info(Name),
    snmp_generic:handle_table_get({Name, mnesia}, RowIndex, Cols,
				  TableInfo#table_info.first_own_index);

%%------------------------------------------------------------------
%% Returns: List of endOfTable | {NextOid, Value}.
%% Implements the next operation, with the function
%% handle_table_next. Next should return the next accessible
%% instance, which cannot be a key (well, it could, but it
%% shouldn't).
%%------------------------------------------------------------------
table_func(get_next, RowIndex, Cols, Name) ->
    #table_info{first_accessible = FirstCol, first_own_index = FOI,
		nbr_of_cols = LastCol} = snmp_generic:table_info(Name),
    snmp_generic:handle_table_next({Name,mnesia},RowIndex,Cols,
				   FirstCol, FOI, LastCol);

table_func(is_set_ok, RowIndex, Cols, Name) ->
    snmp_generic:table_try_row({Name, mnesia}, nofunc, RowIndex, Cols);

%%------------------------------------------------------------------
%% Cols is here a list of {ColumnNumber, NewValue}
%% This function must only be used by tables with a RowStatus col!
%% Other tables should use table_set_cols/4.
%% All set functionality is handled within a transaction.
%%
%% GenericMnesia uses its own table_set_status and own table_try_make_consistent
%% for performance reasons.
%%------------------------------------------------------------------
table_func(set, RowIndex, Cols, Name) ->
    case mnesia:transaction(
	   fun() ->
		   snmp_generic:table_set_row(
		     {Name, mnesia}, nofunc,
		     fun table_try_make_consistent/2,
		     RowIndex, Cols)
	   end) of
	{atomic, Value} ->
	    Value;
	{aborted, Reason} ->
	    user_err("set transaction aborted. Tab ~w, RowIndex"
		     " ~w, Cols ~w. Reason ~w",
		     [Name, RowIndex, Cols, Reason]),
	    {Col, _Val} = hd(Cols),
	    {commitFailed, Col}
    end;

table_func(undo, _RowIndex, _Cols, _Name) ->
    {noError, 0}.


table_get_row(Name, RowIndex) ->
    case mnesia:snmp_get_row(Name, RowIndex) of
	{ok, DbRow} ->
	    TableInfo = snmp_generic:table_info(Name),
	    make_row(DbRow, TableInfo#table_info.first_own_index);
	undefined ->
	    undefined
    end.
table_get_row(Name, RowIndex, FOI) ->
    case mnesia:snmp_get_row(Name, RowIndex) of
        {ok, DbRow} ->
            make_row(DbRow, FOI);
        undefined ->
            undefined
    end.

%%-----------------------------------------------------------------
%% Returns: [Val | noacc | noinit] | undefined
%%-----------------------------------------------------------------
table_get_elements(Name, RowIndex, Cols, FirstOwnIndex) ->
    case mnesia:snmp_get_row(Name, RowIndex) of
	{ok, DbRow} ->
	    Row = make_row(DbRow, FirstOwnIndex),
	    get_elements(Cols, Row);
	undefined ->
	    undefined
    end.

get_elements([Col | Cols], Row) ->
    [element(Col, Row) | get_elements(Cols, Row)];
get_elements([], _Row) -> [].

%%-----------------------------------------------------------------
%% Args: DbRow is a mnesia row ({name, Keys, Cols, ...}).
%% Returns: A tuple with a SNMP-table row. Each SNMP-col is one
%%          element, list or int.
%%-----------------------------------------------------------------
make_row(DbRow, 0) ->
    [_Name, _Keys | Cols] = tuple_to_list(DbRow),
    list_to_tuple(Cols);
make_row(DbRow, FirstOwnIndex) ->
    list_to_tuple(make_row2(make_row_list(DbRow), FirstOwnIndex)).
make_row2(RowList, 1) -> RowList;
make_row2([_OtherIndex | RowList], N) ->
    make_row2(RowList, N-1).

make_row_list(Row) ->
    make_row_list(size(Row), Row, []).
make_row_list(N, Row, Acc) when N > 2 ->
    make_row_list(N-1, Row, [element(N, Row) | Acc]);
make_row_list(2, Row, Acc) ->
    case element(2, Row) of
	Keys when is_tuple(Keys) ->
	    lists:append(tuple_to_list(Keys), Acc);
	Key ->
	    [Key | Acc]
    end.

%% createAndGo
table_set_status(Name, RowIndex, ?'RowStatus_createAndGo', _StatusCol, Cols, 
                 ChangedStatusFunc, _ConsFunc) ->
    Row = table_construct_row(Name, RowIndex, ?'RowStatus_active', Cols),
    mnesia:write(Row),
    snmp_generic:try_apply(ChangedStatusFunc, [Name, ?'RowStatus_createAndGo',
					       RowIndex, Cols]);
 
%%------------------------------------------------------------------
%% createAndWait - set status to notReady, and try to 
%% make row consistent.
%%------------------------------------------------------------------
table_set_status(Name, RowIndex, ?'RowStatus_createAndWait', _StatusCol, 
                 Cols, ChangedStatusFunc, ConsFunc) ->
    Row = table_construct_row(Name, RowIndex, ?'RowStatus_notReady', Cols),
    mnesia:write(Row),
    case snmp_generic:try_apply(ConsFunc, [RowIndex, Row]) of
        {noError, 0} -> snmp_generic:try_apply(ChangedStatusFunc, 
					       [Name, ?'RowStatus_createAndWait',
						RowIndex, Cols]);
        Error -> Error
    end;
    
%% destroy
table_set_status(Name, RowIndex, ?'RowStatus_destroy', _StatusCol, Cols,
                 ChangedStatusFunc, _ConsFunc) ->
    case snmp_generic:try_apply(ChangedStatusFunc,
				[Name, ?'RowStatus_destroy', RowIndex, Cols]) of
        {noError, 0} ->
            #table_info{index_types = Indexes} = snmp_generic:table_info(Name),
            Key = 
                case snmp_generic:split_index_to_keys(Indexes, RowIndex) of
                    [Key1] -> Key1;
                    KeyList -> list_to_tuple(KeyList)
                end,
            mnesia:delete({Name, Key}),
            {noError, 0};
        Error -> Error
    end;
 
%% Otherwise, active or notInService
table_set_status(Name, RowIndex, Val, _StatusCol, Cols,
                 ChangedStatusFunc, ConsFunc) ->
    table_set_cols(Name, RowIndex, Cols, ConsFunc),
    snmp_generic:try_apply(ChangedStatusFunc, [Name, Val, RowIndex, Cols]).

table_delete_row(Name, RowIndex) ->
    case mnesia:snmp_get_mnesia_key(Name, RowIndex) of
        {ok, Key} ->
            mnesia:delete({Name, Key});
        undefined ->
            ok
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
table_try_make_consistent(RowIndex, NewDbRow) ->
    Name = element(1, NewDbRow),
    #table_info{first_own_index = FirstOwnIndex,
		status_col = StatusCol, index_types = IT} = 
	snmp_generic:table_info(Name),
    if
	is_integer(StatusCol) ->
	    NewRow = make_row(NewDbRow, FirstOwnIndex),
	    StatusVal = element(StatusCol, NewRow),
	    AddCol = if
			 FirstOwnIndex == 0 -> 2;
			 true -> 1 + FirstOwnIndex - length(IT)
		     end,
	    table_try_make_consistent(Name, RowIndex, NewRow, NewDbRow, 
				      AddCol, StatusCol, StatusVal);
	true ->
	    {noError, 0}
    end.

    
table_try_make_consistent(Name, RowIndex, NewRow, NewDbRow, 
			  AddCol, StatusCol, ?'RowStatus_notReady') ->
    case lists:member(noinit, tuple_to_list(NewRow)) of
	true -> {noError, 0};
	false -> 
	    table_set_element(Name, RowIndex, StatusCol,
			      ?'RowStatus_notInService'),
	    NewDbRow2 = set_new_row([{StatusCol, ?'RowStatus_notInService'}],
				    AddCol, NewDbRow),
	    mnesia:write(NewDbRow2),
	    {noError, 0}
    end;

table_try_make_consistent(_Name, _RowIndex, _NewRow, _NewDBRow,
			  _AddCol, _StatusCol, _StatusVal) ->
    {noError, 0}.

%%------------------------------------------------------------------
%%  Constructs a row that is to be stored in Mnesia, i.e.
%%  {Name, Key, Col1, ...} |
%%  {Name, {Key1, Key2, ..}, ColN, ColN+1...}
%%  dynamic key values are stored without length first.
%%  RowIndex is a list of the first elements. RowStatus is needed,
%%  because the provided value may not be stored, e.g. createAndGo
%%  should be active. If a value isn't specified in the Col list,
%%  then the corresponding value will be noinit.
%%------------------------------------------------------------------
table_construct_row(Name, RowIndex, Status, Cols) ->
    #table_info{nbr_of_cols = LastCol, index_types = Indexes,
		defvals = Defs, status_col = StatusCol,
		first_own_index = FirstOwnIndex, not_accessible = NoAccs} =
	snmp_generic:table_info(Name),
    KeyList = snmp_generic:split_index_to_keys(Indexes, RowIndex),
    OwnKeyList = snmp_generic:get_own_indexes(FirstOwnIndex, KeyList),
    StartCol = length(OwnKeyList) + 1,
    RowList = snmp_generic:table_create_rest(StartCol, LastCol, 
					     StatusCol, Status, Cols, NoAccs),
    L = snmp_generic:init_defaults(Defs, RowList, StartCol),
    Keys = case KeyList of
	       [H] -> H;
	       _ -> list_to_tuple(KeyList)
	   end,
    list_to_tuple([Name, Keys | L]).

%%------------------------------------------------------------------
%% table_set_cols/4
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
table_set_cols(Name, RowIndex, Cols, ConsFunc) ->
    table_set_elements(Name, RowIndex, Cols, ConsFunc).
    
%%-----------------------------------------------------------------
%% Col is _not_ a key column. A row in the db is stored as
%% {Name, {Key1, Key2,...}, Col1, Col2, ...}
%%-----------------------------------------------------------------
table_set_element(Name, RowIndex, Col, NewVal) ->
    #table_info{index_types = Indexes, first_own_index = FirstOwnIndex} =
	snmp_generic:table_info(Name),
    DbCol = if
		FirstOwnIndex == 0 -> Col + 2;
		true -> 1 + FirstOwnIndex - length(Indexes) + Col
	    end,
    case mnesia:snmp_get_row(Name, RowIndex) of
	{ok, DbRow} ->
	    NewDbRow = setelement(DbCol, DbRow, NewVal),
	    mnesia:write(NewDbRow),
	    true;
	undefined ->
	    false
    end.

table_set_elements(Name, RowIndex, Cols) ->
    case table_set_elements(Name, RowIndex, Cols, nofunc) of
	{noError, 0} -> true;
	_ -> false
    end.
table_set_elements(Name, RowIndex, Cols, ConsFunc) ->
    #table_info{index_types     = Indexes, 
		first_own_index = FirstOwnIndex} = 
	snmp_generic:table_info(Name),
    AddCol = if
		 FirstOwnIndex == 0 -> 2;
		 true -> 1 + FirstOwnIndex - length(Indexes)
	     end,
    case mnesia:snmp_get_row(Name, RowIndex) of
	{ok, DbRow} ->
	    NewDbRow = set_new_row(Cols, AddCol, DbRow),
	    mnesia:write(NewDbRow),
	    snmp_generic:try_apply(ConsFunc, [RowIndex, NewDbRow]);
	undefined ->
	    {Col, _Val} = hd(Cols),
	    {commitFailed, Col}
    end.

set_new_row([{Col, Val} | Cols], AddCol, Row) ->
    set_new_row(Cols, AddCol, setelement(Col+AddCol, Row, Val));
set_new_row([], _AddCol, Row) ->
    Row.

table_next(Name, RestOid) ->
    case mnesia:snmp_get_next_index(Name, RestOid) of
	{ok, NextIndex} -> NextIndex;
	endOfTable -> endOfTable
    end.


user_err(F, A) ->
    snmpa_error:user_err(F, A).
