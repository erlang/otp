%%<copyright>
%% <year>1996-2007</year>
%% <holder>Ericsson AB, All Rights Reserved</holder>
%%</copyright>
%%<legalnotice>
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
%% The Initial Developer of the Original Code is Ericsson AB.
%%</legalnotice>
%%
-module(ex1).

%% External exports
-export([start/0, my_name/1, my_name/2, friends_table/3]).

%% Internal exports
-export([init/0]).

-define(status_col, 4).

-define(active, 1).
-define(notInService, 2).
-define(notReady, 3).
-define(createAndGo, 4).   % Action; written, not read
-define(createAndWait, 5). % Action; written, not read
-define(destroy, 6).       % Action; written, not read

start() ->
    spawn(ex1, init, []).


%%----------------------------------------------------------------
%% Instrumentation function for variable myName.
%% Returns: (get) {value, Name}
%%          (set) noError
%%----------------------------------------------------------------
my_name(get) ->
    ex1_server ! {self(), get_my_name},
    Name = wait_answer(),
    {value, Name}.

my_name(set, NewName) ->
    ex1_server ! {self(), {set_my_name, NewName}},
    noError.

%%----------------------------------------------------------------
%% Instrumentation function for table friendsTable.
%%----------------------------------------------------------------
friends_table(get, RowIndex, Cols) ->
    case get_row(RowIndex) of
	{ok, Row} ->
	    get_cols(Cols, Row);
	_  ->
	    {noValue, noSuchInstance}
    end;

friends_table(get_next, RowIndex, Cols) ->
    case get_next_row(RowIndex) of
	{ok, Row} ->
	    get_next_cols(Cols, Row);
	_  ->
	    case get_next_row([]) of
		{ok, Row} ->
		    % Get next cols from first row.
		    NewCols = add_one_to_cols(Cols),
		    get_next_cols(NewCols, Row);
		_  ->
		    end_of_table(Cols)
	    end
    end;

%%----------------------------------------------------------------
%% If RowStatus is set, then:
%%    *) If set to destroy, check that row does exist
%%    *) If set to createAndGo, check that row doesn't exist AND
%%         that all columns are given values.
%%    *) Otherwise, error (for simplicity).
%% Otherwise, row is modified; check that row exists.
%%----------------------------------------------------------------
friends_table(is_set_ok, RowIndex, Cols) ->
    RowExists = 
	case get_row(RowIndex) of
	    {ok, _Row} -> true;
	    _          -> false
	end,
    case is_row_status_col_changed(Cols) of
	{true, ?destroy} when RowExists == true ->
	    {noError, 0};
	{true, ?createAndGo} when RowExists == false,
                                 length(Cols) == 3 ->
	    {noError, 0};
	{true, _} ->
	    {inconsistentValue, ?status_col};
	false when RowExists == true ->
	    {noError, 0};
	_ ->
	    [{Col, _NewVal} | _Cols] = Cols,
	    {inconsistentName, Col}
    end;

friends_table(set, RowIndex, Cols) ->
    case is_row_status_col_changed(Cols) of
	{true, ?destroy} ->
	    ex1_server ! {self(), {delete_row, RowIndex}};
	{true, ?createAndGo} ->
	    NewRow = make_row(RowIndex, Cols),
	    ex1_server ! {self(), {add_row, NewRow}};
	false ->
	    {ok, Row} = get_row(RowIndex),
	    NewRow = merge_rows(Row, Cols),
	    ex1_server ! {self(), {delete_row, RowIndex}},
	    ex1_server ! {self(), {add_row, NewRow}}
    end,
    {noError, 0}.
    
%%----------------------------------------------------------------
%% Make a list of {value, Val} of the Row and Cols list.
%%----------------------------------------------------------------
get_cols([Col | Cols], Row) ->
    [{value, element(Col, Row)} | get_cols(Cols, Row)];
get_cols([], _Row) ->
    [].

%%----------------------------------------------------------------
%% As get_cols, but the Cols list may contain invalid column
%% numbers. If it does, we must find the next valid column,
%% or return endOfTable.
%%----------------------------------------------------------------
get_next_cols([Col | Cols], Row) when Col < 2 ->
    [{[2, element(1, Row)], element(2, Row)} | 
     get_next_cols(Cols, Row)];
get_next_cols([Col | Cols], Row) when Col > 4 ->
    [endOfTable | 
     get_next_cols(Cols, Row)];
get_next_cols([Col | Cols], Row) ->
    [{[Col, element(1, Row)], element(Col, Row)} | 
     get_next_cols(Cols, Row)];
get_next_cols([], _Row) ->
    [].

%%----------------------------------------------------------------
%% Make a list of endOfTable with as many elems as Cols list.
%%----------------------------------------------------------------
end_of_table([_Col | Cols]) ->
    [endOfTable | end_of_table(Cols)];
end_of_table([]) ->
    [].

add_one_to_cols([Col | Cols]) ->
    [Col + 1 | add_one_to_cols(Cols)];
add_one_to_cols([]) ->
    [].

is_row_status_col_changed(Cols) ->
    case lists:keysearch(?status_col, 1, Cols) of
	{value, {?status_col, StatusVal}} ->
	    {true, StatusVal};
	_ -> false
    end.

get_row(RowIndex) ->
    ex1_server ! {self(), {get_row, RowIndex}},
    wait_answer().

get_next_row(RowIndex) ->
    ex1_server ! {self(), {get_next_row, RowIndex}},
    wait_answer().

wait_answer() ->
    receive
	{ex1_server, Answer} ->
	    Answer
    end.

%%%----------------------------------------------------------------
%%% Server code follows
%%%----------------------------------------------------------------
init() ->
    register(ex1_server, self()),
    loop("", []).
    
loop(MyName, Table) ->
    receive
	{From, get_my_name} ->
	    From ! {ex1_server, MyName},
	    loop(MyName, Table);
	{_From, {set_my_name, NewName}} ->
	    loop(NewName, Table);
	{From, {get_row, RowIndex}} ->
	    Res = table_get_row(Table, RowIndex),
	    From ! {ex1_server, Res},
	    loop(MyName, Table);
	{From, {get_next_row, RowIndex}} ->
	    Res = table_get_next_row(Table, RowIndex),
	    From ! {ex1_server, Res},
	    loop(MyName, Table);
	{_From, {delete_row, RowIndex}} ->
	    NewTable = table_delete_row(Table, RowIndex),
	    loop(MyName, NewTable);
	{_From, {add_row, NewRow}} ->
	    NewTable = table_add_row(Table, NewRow),
	    loop(MyName, NewTable)
    end.


%%-----------------------------------------------------------------
%% Implementation of the table.
%%-----------------------------------------------------------------
table_get_row([{Index, Name, Address, Status} | _], [Index]) ->
    {ok, {Index, Name, Address, Status}};
table_get_row([_H | T], RowIndex) ->
    table_get_row(T, RowIndex);
table_get_row([], _RowIndex) ->
    no_such_row.

table_get_next_row([Row | _T], []) ->
    {ok, Row};
table_get_next_row([Row | _T], [Index | _]) when element(1, Row) > Index ->
    {ok, Row};
table_get_next_row([_Row | T], RowIndex) ->
    table_get_next_row(T, RowIndex);
table_get_next_row([], _RowIndex) ->
    endOfTable.

table_delete_row([{Index, _, _, _} | T], [Index]) ->
    T;
table_delete_row([H | T], RowIndex) ->
    [H | table_delete_row(T, RowIndex)];
table_delete_row([], _RowIndex) ->
    [].

table_add_row([Row | T], NewRow) when element(1, Row) > element(1, NewRow) ->
    [NewRow, Row | T];
table_add_row([H | T], NewRow) ->
    [H | table_add_row(T, NewRow)];
table_add_row([], NewRow) ->
    [NewRow].

make_row([Index], [{2, Name}, {3, Address} | _]) ->
    {Index, Name, Address, ?active}.

merge_rows(Row, [{Col, NewVal} | T]) ->
    merge_rows(setelement(Col, Row, NewVal), T);
merge_rows(Row, []) ->
    Row.


