%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%% 
%% Copyright Ericsson AB 1996-2025. All Rights Reserved.
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
-moduledoc """
Generic Functions for Implementing SNMP Objects in a Database

The module `snmp_generic` contains generic functions for implementing tables
(and variables) using the SNMP built-in database or Mnesia. These default
functions are used if no instrumentation function is provided for a managed
object in a MIB. Sometimes, it might be necessary to customize the behaviour of
the default functions. For example, in some situations a trap should be sent if
a row is deleted or modified, or some hardware is to be informed, when
information is changed.

The overall structure is shown in the following figure:

```c
         +---------------+
         |   SNMP Agent  |
         +- - - - - - - -+
         |      MIB      |
         +---------------+
                 |
         Association file       (associates a MIB object with
                 |               snmp_generic:table_funct
                 |               snmp_generic:variable_func)
+--------------------------------------+
|           snmp_generic               |  Support for get-next,
|                                      |  RowStatus operations
+----------------------+---------------+
|    snmpa_local_db    |    Mnesia     |  Database
+--------------+-------+---------------+
|     dets     |  ets  |
| (persistent) |       |
+--------------+-------+
```

Each function takes the argument `NameDb`, which is a tuple `{Name, Db}`, to
identify which database the functions should use. `Name` is the symbolic name of
the managed object as defined in the MIB, and `Db` is either `volatile`,
`persistent`, or `mnesia`. If it is `mnesia`, all variables are stored in the
Mnesia table `snmp_variables` which must be a table with two attributes (not a
Mnesia SNMP table). The SNMP tables are stored in Mnesia tables with the same
names as the SNMP tables. All functions assume that a Mnesia table exists with
the correct name and attributes. It is the programmer's responsibility to ensure
this. Specifically, if variables are stored in Mnesia, the table
`snmp_variables` must be created by the programmer. The record definition for
this table is defined in the file `snmp/include/snmp_types.hrl`.

If an instrumentation function in the association file for a variable `myVar`
does not have a name when compiling an MIB, the compiler generates an entry.

```erlang
{myVar, {snmp_generic, variable_func, [{myVar, Db]}}.
```

And for a table:

```erlang
{myTable, {snmp_generic, table_func, [{myTable, Db]}}.
```

## Example

The following example shows an implementation of a table which is stored in
Mnesia, but with some checks performed at set-request operations.

```erlang
myTable_func(new, NameDb) ->   % pass unchanged
  snmp_generic:table_func(new, NameDb).

myTable_func(delete, NameDb) ->   % pass unchanged
  snmp_generic:table_func(delete, NameDb).

%% change row
myTable_func(is_set_ok, RowIndex, Cols, NameDb) ->
  case snmp_generic:table_func(is_set_ok, RowIndex,
                               Cols, NameDb) of
    {noError, 0} ->
      myApplication:is_set_ok(RowIndex, Cols);
    Err ->
      Err
  end;

myTable_func(set, RowIndex, Cols, NameDb) ->
  case snmp_generic:table_func(set, RowIndex, Cols,
                               NameDb),
    {noError, 0} ->
      % Now the row is updated, tell the application
      myApplication:update(RowIndex, Cols);
    Err ->
      Err
  end;

myTable_func(Op, RowIndex, Cols, NameDb) ->   % pass unchanged
  snmp_generic:table_func(Op, RowIndex, Cols, NameDb).
```

The `.funcs` file would look like:

```erlang
{myTable, {myModule, myTable_func, [{myTable, mnesia}]}}.
```
""".

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

-export_type([
              column/0,
              columns/0
             ]).

-include("STANDARD-MIB.hrl").
-include("snmp_types.hrl").

-define(VMODULE,"GENERIC").
-include("snmp_verbosity.hrl").

-ifndef(default_verbosity).
-define(default_verbosity,silence).
-endif.


-type column()          :: pos_integer().
-doc """
Is a list of column numbers in the case of a get operation, and a list of column
numbers and values in the case of a set operation.
""".
-type columns()         :: [column()] | [{column(), Value :: term()}].
-doc """
For an ordinary table, the types will be the following:

- **`nbr_of_cols`** - Number of columns.

  Value type: [pos_integer()](`t:erlang:pos_integer/0`)

- **`defvals`** - A list of default values, ordered by column.

  Value type: [\{Col :: [pos_integer()](`t:erlang:pos_integer/0`), DefVal ::
  [term()](`t:erlang:term/0`)\}]

- **`status_col`** - Column number of the status column.

  Value type: [pos_integer()](`t:erlang:pos_integer/0`)

- **`not_accessible`** - A sorted list of columns (> first_accessible) that are
  'not-accessible'.

  Value type: [[pos_integer()](`t:erlang:pos_integer/0`)]

- **`index_types`** - A list of [asn1_type()](`t:snmp:asn1_type/0`) for the
  index columns, ordered by column number or an "augment"-tuple (see below).

  Value type: [[asn1_type()](`t:snmp:asn1_type/0`)]

- **`first_accessible`** - The first accessible column.

  Value type: [pos_integer()](`t:erlang:pos_integer/0`)

- **`first_own_index`** - Column number of the first own index. Will be `0` if
  there is no such index for this table.

  Value type: [non_neg_integer()](`t:erlang:non_neg_integer/0`)

For a augmented table, it will instead look like this:

- **`index_types`** - Value type: \{augments, \{[atom()](`t:erlang:atom/0`),
  [asn1_type()](`t:snmp:asn1_type/0`)\}\}

- **`nbr_of_cols`** - Value type: [pos_integer()](`t:erlang:pos_integer/0`)

- **`not_accessible`** - Value type: [[pos_integer()](`t:erlang:pos_integer/0`)]

- **`first_accessible`** - Value type: [pos_integer()](`t:erlang:pos_integer/0`)
""".
-type table_info_item() :: nbr_of_cols |
                           defvals |
                           status_col |
                           not_accessible |
                           index_types |
                           first_accessible |
                           first_own_index.


%%%-----------------------------------------------------------------
%%% Generic functions for implementing software tables
%%% and variables. 
%%%-----------------------------------------------------------------
%% NameDb is {TableName, Db} where Db is volatile | persistent | mnesia

%%------------------------------------------------------------------
%% Access functions to the database.
%%------------------------------------------------------------------

-doc """
Gets the value of a variable.

""".
-spec variable_get(Name) -> {value, Value} | undefined when
      Name  :: snmpa:name() | snmpa:name_db(),
      Value :: term().

variable_get({Name, mnesia}) ->
    snmp_generic_mnesia:variable_get(Name);
variable_get(Name) ->                   % ret {value, Val} | undefined
    snmpa_local_db:variable_get(Name).


-doc """
Sets a new value to a variable. The variable is created if it does not exist. No
checks are made on the type of the new value.

Returns `false` if the `NameDb` argument is incorrectly specified, otherwise
`true`.
""".
-spec variable_set(Name, Value) -> boolean() when
      Name  :: snmpa:name() | snmpa:name_db(),
      Value :: term().

variable_set({Name, mnesia}, Val) ->
    snmp_generic_mnesia:variable_set(Name, Val);
variable_set(Name, Val) ->              % ret true
    snmpa_local_db:variable_set(Name, Val).


-doc false.
variable_inc({Name, mnesia}, N) ->
    snmp_generic_mnesia:variable_inc(Name, N);
variable_inc(NameDb, N) ->              % ret true
    snmpa_local_db:variable_inc(NameDb, N).


%%-----------------------------------------------------------------
%% Returns: {value, Val} | undefined
%%
%% snmpa_local_db overloads (for performance reasons? (mbj?))
%%-----------------------------------------------------------------

-doc false.
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


-doc """
Returns a list with values for all columns in `Cols`. If a column is undefined,
its value is `noinit`.

""".
-spec table_get_elements(NameDb, RowIndex, Cols) -> Values when
      NameDb   :: snmpa:name_db(),
      RowIndex :: snmp:row_index(),
      Cols     :: columns(),
      Values   :: [noinit | Value],
      Value    :: term().

table_get_elements(NameDb, RowIndex, Cols) ->
    TableInfo = snmp_generic:table_info(NameDb),
    table_get_elements(NameDb, RowIndex, Cols,
		       TableInfo#table_info.first_own_index).


%%----------------------------------------------------------------------
%% Returns: list of vals | undefined
%%----------------------------------------------------------------------
-doc false.
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
-doc false.
table_set_element({Name,mnesia}, RowIndex, Col, NewVal) -> 
    snmp_generic_mnesia:table_set_elements(Name, RowIndex,
					   [{Col, NewVal}]);
table_set_element(NameDb, RowIndex, Col, NewVal) ->
    snmpa_local_db:table_set_elements(NameDb, RowIndex, [{Col, NewVal}]).

-doc """
table_set_elements(NameDb, RowIndex, Cols)

Sets the elements in `Cols` to the row specified by `RowIndex`. No checks are
performed on the new values.

If the Mnesia database is used, this function calls `mnesia:write` to store the
values. This means that this function must be called from within a transaction
(`mnesia:transaction/1`).

""".
table_set_elements({Name, mnesia}, RowIndex, Cols) ->
    snmp_generic_mnesia:table_set_elements(Name, RowIndex, Cols);
table_set_elements(NameDb, RowIndex, Cols) -> % ret true
    snmpa_local_db:table_set_elements(NameDb, RowIndex, Cols).

-doc """
table_next(NameDb, RestOid)

Finds the indices of the next row in the table. `RestOid` does not have to
specify an existing row.

""".
table_next({Name, mnesia}, RestOid) ->
    snmp_generic_mnesia:table_next(Name, RestOid);
table_next(NameDb, RestOid) ->              % ret RRestOid | endOfTable
    snmpa_local_db:table_next(NameDb, RestOid).
-doc false.
table_max_col(NameDb, Col) ->               % ret largest element in Col
                                            % in the table NameDb.
    snmpa_local_db:table_max_col(NameDb, Col).


%%------------------------------------------------------------------
%% These functions could be in the MIB for simple 
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

-doc """
This is the default instrumentation function for variables.

- The `new` opeation creates a new variable in the database with a
  default value as defined in the MIB, or a zero value (depending on
  the type).
- The `delete` function does not delete the variable from the database.

The function returns according to the specification of an instrumentation
function.

""".
-spec variable_func(Op :: new, Name) -> Result when
      Name   :: snmpa:name() | snmpa:name_db(),
      Result :: ok | boolean();
                   (Op :: delete, Name) -> Result when
      Name   :: snmpa:name() | snmpa:name_db(),
      Result :: ok;
                   (Op :: get, Name) -> Result when
      Name   :: snmpa:name() | snmpa:name_db(),
      Result :: {value, Value} | genErr,
      Value  :: term().
      
variable_func(new, Name) ->
    case variable_get(Name) of
	{value, _} -> ok;
	undefined ->
	    #variable_info{defval = Defval} = variable_info(Name),
	    variable_set(Name, Defval)
    end;

variable_func(delete, _Name) ->
    ok;

variable_func(get, Name) ->
    case variable_get(Name) of
	{value, Val} -> {value, Val};
	_ -> genErr
    end.


-doc """
This is the default instrumentation function for variables with operations;
`is_set_ok | set | undo`.

- The `is_set_ok` operation does nothing.
- The `set` operation return `noError` if successful or `commitFailed` otherwise.
- The `undo` operation does nothing.

The function returns according to the specification of an instrumentation
function.

""".
-spec variable_func(Op :: is_set_ok, Value, Name) -> Result when
      Value  :: term(),
      Name   :: snmpa:name() | snmpa:name_db(),
      Result :: noError;
                   (Op :: set, Value, Name) -> Result when
      Value  :: term(),
      Name   :: snmpa:name() | snmpa:name_db(),
      Result :: noError | commitFailed;
                   (Op :: undo, Value, Name) -> Result when
      Value  :: term(),
      Name   :: snmpa:name() | snmpa:name_db(),
      Result :: noError.

variable_func(is_set_ok, _Val, _Name) ->
    noError;
variable_func(set, Val, Name) ->
    case variable_set(Name, Val) of
	true -> noError;
	false -> commitFailed
    end;
variable_func(undo, _Val, _Name) ->
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

-doc """
This is the default instrumentation function for tables.

- The `new` operation creates the table if it does not exist, but only if the
  database is the SNMP internal db.
- The `delete` operation does not delete the table from the database since
  unloading an MIB does not necessarily mean that the table should be destroyed.

If it is possible for a manager to create or delete rows in the table, there
must be a `RowStatus` column for `is_set_ok`, `set` and `undo` to work properly.

The function returns according to the specification of an instrumentation
function.

""".
-spec table_func(Op, NameDb) -> Return when
      Op     :: new | delete,
      NameDb :: snmpa:name_db(),
      Return :: term().

table_func(Op, {Name, mnesia}) ->
    snmp_generic_mnesia:table_func(Op, Name);

table_func(Op, NameDb) ->
    snmpa_local_db:table_func(Op, NameDb).


-doc """
This is the default instrumentation function for tables.

- The `is_set_ok` operation checks that a row which is to be modified or deleted
  exists, and that a row which is to be created does not exist.
- The `undo` operation does nothing.
- The `set` operation checks if it has enough information to make the row change
  its status from `notReady` to `notInService` (when a row has been been set to
  `createAndWait`). If a row is set to `createAndWait`, columns without a value
  are set to `noinit`. If Mnesia is used, the set functionality is handled
  within a transaction.

If it is possible for a manager to create or delete rows in the table, there
must be a `RowStatus` column for `is_set_ok`, `set` and `undo` to work properly.

The function returns according to the specification of an instrumentation
function.

""".
-spec table_func(Op, RowIndex, Cols, NameDb) -> Return when
      Op       :: get | get_next | is_set_ok | set | undo,
      RowIndex :: snmp:row_index(),
      Cols     :: columns(),
      NameDb   :: snmpa:name_db(),
      Return   :: term().

table_func(Op, RowIndex, Cols, {Name, mnesia}) ->
    snmp_generic_mnesia:table_func(Op, RowIndex, Cols, Name);

table_func(Op, RowIndex, Cols, NameDb) ->
    snmpa_local_db:table_func(Op, RowIndex, Cols, NameDb).


%%----------------------------------------------------------------------
%% DB independent.
%%----------------------------------------------------------------------
-doc false.
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
	    noacc  -> {noAccess, unSpecified};
	    Val    -> {value, Val}
	end,
    [NewVal | validate_get(Cols, Ress)];
validate_get([], []) -> [].

make_list(N, X) when N > 0 -> [X | make_list(N-1, X)];
make_list(_, _) -> [].

-doc false.
table_foreach(Tab, Fun) ->
    ?vdebug("apply fun to all in table ~w",[Tab]),
    table_foreach(Tab, Fun, undefined, []).
-doc false.
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
-doc false.
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
-doc false.
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
-doc false.
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
                _:_E:_S ->
		    ?vtrace(
		       "failed construct row (createAndGo): "
		       "~n   Error: ~p"
		       "~n   Stack: ~p",
		       [_E, _S]),
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
                _:_E:_S ->
		    ?vtrace(
		       "failed construct row (createAndWait): "
		       "~n   Error: ~p"
		       "~n   Stack: ~p",
		       [_E, _S]),
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
-doc false.
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

-doc false.
init_defaults(Defs, InitRow) ->
    table_defaults(InitRow, Defs).
-doc false.
init_defaults(Defs, InitRow, StartCol) ->
    table_defaults(InitRow, StartCol, Defs).
%%-----------------------------------------------------------------
%% Get, from a list of Keys, the Keys defined in this table.
%% (e.g. if INDEX { ifIndex, myOwnIndex }, the Keys is a list
%% of two elements, and returned from this func is a list of
%% the last of the two.)
%%-----------------------------------------------------------------
-doc false.
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
-doc false.
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
%%  the corresponding Col has a DefVal in Defs, then the DefVal
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
-doc false.
table_set_cols({Name,mnesia}, RowIndex, Cols, ConsFunc) ->
    snmp_generic_mnesia:table_set_cols(Name, RowIndex,Cols,ConsFunc);
table_set_cols(NameDb, RowIndex, Cols, ConsFunc) ->
    case table_set_cols(NameDb, RowIndex, Cols) of
	{noError, 0} -> try_apply(ConsFunc, [NameDb, RowIndex, Cols]);
	Error -> Error
    end.

-doc false.
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
-doc false.
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
-doc """
Checks if a row in a table exists.
""".
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
-doc false.
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
-doc false.
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


-doc false.
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


-doc false.
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
-doc false.
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

-doc false.
table_get_row({Name, mnesia}, RowIndex) ->
    snmp_generic_mnesia:table_get_row(Name, RowIndex);
table_get_row(NameDb, RowIndex) ->
    snmpa_local_db:table_get_row(NameDb, RowIndex).

-doc false.
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

-doc """
Gets the value of the status column from `Cols`.

This function can be used in instrumentation functions for `is_set_ok`, `undo`
or `set` to check if the status column of a table is modified.

""".
-spec get_status_col(Name, Cols) -> false | {value, StatusCol} when
      Name      :: snmpa:name() | snmpa:name_db(),
      Cols      :: columns(),
      StatusCol :: term().

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

-doc """
Get a specific table info item or, if `Item` has the
value 'all', a two tuple list (property list) is instead 
returned with all the items and their respctive values of the 
given table.

This function can be used in instrumentation functions to
retrieve a given part of the table info.

""".
-doc(#{since => <<"OTP R15B01">>}).
-spec get_table_info(Name, Item :: nbr_of_cols) -> Result when
      Name   :: snmpa:name() | snmpa:name_db(),
      %% Item   :: nbr_of_cols,
      Result :: pos_integer();
                    (Name, Item :: defvals) -> Result when
      Name   :: snmpa:name() | snmpa:name_db(),
      %% Item   :: defvals,
      Result :: [{Col, DefVal}],
      Col    :: pos_integer(),
      DefVal :: term();
                    (Name, Item :: status_col) -> Result when
      Name   :: snmpa:name() | snmpa:name_db(),
      %% Item   :: status_col,
      Result :: pos_integer();
                    (Name, Item :: not_accessible) -> Result when
      Name   :: snmpa:name() | snmpa:name_db(),
      %% Item   :: not_accessible,
      Result :: [pos_integer()];
                    (Name, Item :: index_types) -> Result when
      Name   :: snmpa:name() | snmpa:name_db(),
      %% Item   :: index_types,
      Result :: [snmp:asn1_type()];
                    (Name, Item :: first_accessible) -> Result when
      Name   :: snmpa:name() | snmpa:name_db(),
      %% Item   :: first_accessible,
      Result :: pos_integer();
                    (Name, Item :: first_own_index) -> Result when
      Name   :: snmpa:name() | snmpa:name_db(),
      %% Item   :: first_own_index,
      Result :: non_neg_integer();
                    (Name, Item :: all) -> Result when
      Name   :: snmpa:name() | snmpa:name_db(),
      %% Item   :: all,
      Result :: [{table_info_item(), term()}].
      
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

-doc """
Gets the index types of `Name`

This function can be used in instrumentation functions to retrieve the index
types part of the table info.

""".
-spec get_index_types(Name) -> IndexTypes when
      Name       :: snmpa:name() | snmpa:name_db(),
      IndexTypes :: [snmp:asn1_type()].

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
