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
-module(snmpa_mib_lib).

-export([table_cre_row/3, table_del_row/2]).
-export([get_table/2]).
-export([print_variables/1, print_table/3, print_table/4, print_tables/1]).
-export([gc_tab/3, gc_tab/5]).

-include("SNMPv2-TC.hrl").
-include("snmp_types.hrl").

-define(VMODULE,"MIB-LIB").
-include("snmp_verbosity.hrl").

-ifndef(default_verbosity).
-define(default_verbosity,silence).
-endif.


%%%-----------------------------------------------------------------
%%%-----------------------------------------------------------------

%% returns: bool()
table_cre_row({Tab, mnesia}, Key, _Row) ->
    ?vtrace("create mnesia table ~w row with Key: ~w",[Tab, Key]),
    {error, mnesia_not_supported};
table_cre_row({Tab, Db} = TabDb, Key, Row) ->
    ?vtrace("create ~w table ~w row with Key: ~w",[Db, Tab, Key]),
    snmpa_local_db:table_create_row(TabDb, Key, Row).

%% returns: bool()
table_del_row({Tab, mnesia}, Key) ->
    ?vtrace("delete mnesia table ~w row with Key: ~w",[Tab, Key]),
    {error, mnesia_not_supported};
table_del_row({Tab, Db} = TabDb, Key) ->
    ?vtrace("delete ~w table ~w row with Key: ~w", [Db, Tab, Key]),
    snmpa_local_db:table_delete_row(TabDb, Key).


%%%-----------------------------------------------------------------
%%% Retreives the entire table. Used for debugging
%%%-----------------------------------------------------------------

get_table(NameDb, FOI) ->
    (catch get_table(NameDb, FOI, [], [])).

get_table(NameDb, FOI, Key, Acc) ->
    case table_next(NameDb, Key) of
        endOfTable ->
            ?vdebug("end of table",[]),
            {ok, lists:reverse(Acc)};
        Key ->
            %% Crap, circular ref
            ?vinfo("cyclic reference: ~w -> ~w", [Key, Key]),
            throw({error, {cyclic_db_reference, Key, Acc}});
        NextKey ->
            ?vtrace("get row for key ~w", [NextKey]),
            case table_get_row(NameDb, NextKey, FOI) of
                undefined -> 
		    throw({error, {invalid_rowindex, NextKey, Acc}});
                Row ->
                    ?vtrace("row: ~w", [Row]),
		    get_table(NameDb, FOI, NextKey, [{NextKey, Row}|Acc])
            end
    end.
    

print_variables(Variables) when is_list(Variables) ->
    Variables2 = print_variables_prefixify(Variables), 
    lists:foreach(fun({Variable, ValueResult, Prefix}) ->
			  print_variable(Variable, ValueResult, Prefix)
		  end, Variables2),
    ok.

print_variable(Variable, {value, Val}, Prefix) when is_atom(Variable) ->
    io:format("~w~s=> ~p~n", [Variable, Prefix, Val]);
print_variable(Variable, Error, Prefix) when is_atom(Variable) ->
    io:format("~w~s=> [e] ~p~n", [Variable, Prefix, Error]).

print_variables_prefixify(Variables) ->
    MaxVarLength = print_variables_maxlength(Variables),
    print_variables_prefixify(Variables, MaxVarLength, []).

print_variables_prefixify([], _MaxVarLength, Acc) ->
    lists:reverse(Acc);
print_variables_prefixify([{Var, Res}|Variables], MaxVarLength, Acc) ->
    Prefix = make_variable_print_prefix(Var, MaxVarLength),
    print_variables_prefixify(Variables, MaxVarLength, 
			      [{Var, Res, Prefix}|Acc]).
    
make_variable_print_prefix(Var, MaxVarLength) ->
    lists:duplicate(MaxVarLength - length(atom_to_list(Var)) + 1, $ ).

print_variables_maxlength(Variables) ->
    print_variables_maxlength(Variables, 0).

print_variables_maxlength([], MaxLength) ->
    MaxLength;
print_variables_maxlength([{Var, _}|Variables], MaxLength) when is_atom(Var) ->
    VarLen = length(atom_to_list(Var)),
    if 
	VarLen > MaxLength ->
	    print_variables_maxlength(Variables, VarLen);
	true ->
	    print_variables_maxlength(Variables, MaxLength)
    end.


print_tables(Tables) when is_list(Tables) ->
    lists:foreach(fun({Table, DB, FOI, PrintRow}) ->
			  print_table(Table, DB, FOI, PrintRow)
		  end, Tables),
    ok.

print_table(Table, DB, FOI, PrintRow) ->
    TableInfo = get_table(DB, FOI),
    print_table(Table, TableInfo, PrintRow).

print_table(Table, TableInfo, PrintRow) when is_function(PrintRow, 2) ->
    io:format("~w =>", [Table]),
    do_print_table(TableInfo, PrintRow).

do_print_table({ok, [] = _TableInfo}, _PrintRow) ->
    io:format(" -~n", []);
do_print_table({ok, TableInfo}, PrintRow) when is_function(PrintRow, 2) ->
    io:format("~n", []),
    lists:foreach(fun({RowIdx, Row}) ->
			  io:format("   ~w => ~n~s~n", 
				    [RowIdx, PrintRow("      ", Row)])
		  end, TableInfo);
do_print_table({error, {invalid_rowindex, BadRowIndex, []}}, _PrintRow) ->
    io:format("Error: Bad rowindex ~w~n", [BadRowIndex]);
do_print_table({error, {invalid_rowindex, BadRowIndex, TableInfo}}, PrintRow) ->
    io:format("Error: Bad rowindex ~w", [BadRowIndex]),
    do_print_table(TableInfo, PrintRow);
do_print_table(Error, _PrintRow) ->
    io:format("Error: ~p~n", [Error]).
    

%%%-----------------------------------------------------------------
%%% 
%%%-----------------------------------------------------------------

table_next({Name, mnesia}, RestOid) ->
    snmp_generic_mnesia:table_next(Name, RestOid);
table_next(NameDb, RestOid) -> 
    snmpa_local_db:table_next(NameDb, RestOid).


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


%%%-----------------------------------------------------------------
%%% Utility module for the mib-implementation modules (such as
%%% snmp_target_mib).
%%%-----------------------------------------------------------------

gc_tab(TabDb, STC, FOI) ->
    InvalidateRow = fun(_) -> ok end,
    UpdateRow     = fun(_) -> ok end,
    gc_tab(TabDb, STC, FOI, InvalidateRow, UpdateRow).

gc_tab({Tab,mnesia} = TabDb, STC, FOI, InvalidateRow, UpdateRow) ->
    F = fun(RowIndex, Row) ->
		case element(STC, Row) of
                    ?'StorageType_volatile' ->
			snmp_generic_mnesia:table_delete_row(Tab, RowIndex),
                        InvalidateRow(RowIndex);
		    _ ->
			UpdateRow(RowIndex)
		end
	end,
    gc_tab1(F, TabDb, FOI);

gc_tab(TabDb, STC, FOI, InvalidateRow, UpdateRow) ->
    F = fun(RowIndex, Row) ->
		case element(STC, Row) of
 		    ?'StorageType_volatile' ->
			snmpa_local_db:table_delete_row(TabDb, RowIndex),
			InvalidateRow(RowIndex);
		    _ ->
			UpdateRow(RowIndex),
			ok
		end
	end,
    gc_tab1(F, TabDb, FOI).


gc_tab1(F, {Tab,_} = TabDb, FOI) ->
    case (catch snmp_generic:table_foreach(TabDb, F, FOI)) of
	{'EXIT',{cyclic_db_reference,Oid}} ->
	    %% Remove the row regardless of storage type since this
	    %% is a major error. This row must be removed.
	    case table_delete_row(TabDb, Oid) of
		true -> 
		    ?vlog("deleted cyclic ref row for: ~w;~w",
			  [Tab, Oid]),
		    config_err("cyclic reference in table ~w: "
			       "~w -> ~w. Row deleted", 
			       [Tab, Oid, Oid]),
		    gc_tab1(F, TabDb, FOI);
		false ->
		    ?vlog("unable to remove faulty row from table ~w",
			  [Tab]),
		    config_err("failed removing faulty row. "
			       "Giving up on table ~w cleanup", [Tab])
	    end;
	_ ->
	    ok
    end.

table_delete_row({Tab, mnesia}, Oid) ->
    snmp_generic_mnesia:table_delete_row(Tab, Oid),
    true;
table_delete_row(TabDb, Oid) ->
    snmpa_local_db:table_delete_row(TabDb, Oid).

config_err(F, A) ->
    snmpa_error:config_err(F, A).
