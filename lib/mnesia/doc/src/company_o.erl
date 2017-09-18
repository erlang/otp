%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2016. All Rights Reserved.
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

-module(company_o).

-export([sinit/0, init/0,insert_emp/3,females/0,
         female_bosses/0, raise_females/1, over_write/2, raise/2,
         bad_raise/2, get_emps/2, get_emps2/2]).


-import(mnesia, [transaction/1]).

%0
-include_lib("stdlib/include/qlc.hrl").
-include("company_o.hrl").


sinit() ->
    mnesia:create_schema([node()]).

init() ->
    mnesia:create_table(employee,
                        [{attributes, record_info(fields, employee)}]),
    mnesia:create_table(dept,
                        [{attributes, record_info(fields, dept)}]),
    mnesia:create_table(project, 
                        [{attributes, record_info(fields, project)}]).

%0
    


%1

insert_emp(Emp, DeptId, ProjNames) ->
    Fun = fun() ->
                  mnesia:write(Emp#employee{dept = DeptId, 
					    projects = ProjNames})
          end,
    mnesia:transaction(Fun).


%1
 
%2
females() ->
    F = fun() ->
		Q = qlc:q([E#employee.name || E <- mnesia:table(employee),
					      E#employee.sex == female]),
		qlc:e(Q)
	end,
    mnesia:transaction(F).
%2

%3
female_bosses() ->
    F = fun() -> qlc:e(qlc:q( 
			 [{E#employee.name, Boss#employee.name} ||
			     E <- mnesia:table(employee),
			     Boss <- mnesia:table(employee),
			     Boss#employee.emp_no == E#employee.manager,
			     E#employee.sex == female]
			))
        end,
    mnesia:transaction(F).

                    
%4
raise_females(Amount) ->
    F = fun() ->
		Q = qlc:q([E || E <- mnesia:table(employee),
                                E#employee.sex == female]),
		Fs = qlc:e(Q),
                over_write(Fs, Amount)
        end,
    mnesia:transaction(F).

over_write([E|Tail], Amount) ->
    Salary = E#employee.salary + Amount,
    New = E#employee{salary = Salary},
    mnesia:write(New),
    1 + over_write(Tail, Amount);
over_write([], _) ->
    0.
%4

%5
raise(Eno, Raise) ->
    F = fun() ->
                [E] = mnesia:read({employee, Eno}),
                Salary = E#employee.salary + Raise,
                New = E#employee{salary = Salary},
                mnesia:write(New)
        end,
    mnesia:transaction(F).
%5


%6
bad_raise(Eno, Raise) ->
    F = fun() ->
                [E] = mnesia:read({employee, Eno}),
                Salary = E#employee.salary + Raise,
                New = E#employee{salary = Salary},
                io:format("Trying to write ... ~n", []),
                mnesia:write(New)
        end,
    mnesia:transaction(F).
%6
                       
%9
get_emps(Salary, Dep) ->
    Q = qlc:q( 
          [E || E <- mnesia:table(employee),
                E#employee.salary > Salary,
                E#employee.dept == Dep]
	 ),
    F = fun() -> qlc:e(Q) end,
    transaction(F).
%9

%10
get_emps2(Salary, Dep) ->
    Epat0 = mnesia:table_info(employee, wild_pattern),
    Epat = Epat0#employee{dept = Dep},
    F = fun() ->
                All = mnesia:match_object(Epat),
		[E || E <-All, E#employee.salary > Salary ]
	end,
    mnesia:transaction(F).
                

%10

