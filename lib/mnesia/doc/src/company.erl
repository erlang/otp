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
-module(company).

-export([init/0,insert_emp/3,mk_projs/2,females/0,all_females/0,
         g/0,female_bosses/0, raise_females/1, over_write/2, raise/2,
         bad_raise/2, get_emps/2, get_emps2/2, filter/2, filter_deps/3,
         search_deps/3, bench1/0, dotimes/2, dist_init/0, remove_proj/1,
         del_in_projs/1, sync/0, tabs/0, find_male_on_second_floor/0,
         panic/1, fill_tables/0]).

%0

-include_lib("stdlib/include/qlc.hrl").
-include("company.hrl").

init() ->
    mnesia:create_table(employee,
                        [{attributes, record_info(fields, employee)}]),
    mnesia:create_table(dept,
                        [{attributes, record_info(fields, dept)}]),
    mnesia:create_table(project,
                        [{attributes, record_info(fields, project)}]),
    mnesia:create_table(manager, [{type, bag}, 
                                  {attributes, record_info(fields, manager)}]),
    mnesia:create_table(at_dep,
                         [{attributes, record_info(fields, at_dep)}]),
    mnesia:create_table(in_proj, [{type, bag}, 
                                  {attributes, record_info(fields, in_proj)}]).

%0
    
%1

insert_emp(Emp, DeptId, ProjNames) ->
    Ename = Emp#employee.name,
    Fun = fun() ->
                  mnesia:write(Emp),
                  AtDep = #at_dep{emp = Ename, dept_id = DeptId},
                  mnesia:write(AtDep),
                  mk_projs(Ename, ProjNames)
          end,
    mnesia:transaction(Fun).


mk_projs(Ename, [ProjName|Tail]) ->
    mnesia:write(#in_proj{emp = Ename, proj_name = ProjName}),
    mk_projs(Ename, Tail);
mk_projs(_, []) -> ok.
    

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
%20
all_females() ->
    F = fun() ->
		Female = #employee{sex = female, name = '$1', _ = '_'},
		mnesia:select(employee, [{Female, [], ['$1']}])
        end,
    mnesia:transaction(F).
%20

g() -> l.

%3
female_bosses() ->
    H1 = qlc:q( [{Atdep#at_dep.dept_id,E} ||
		    E <- mnesia:table(employee),
		    E#employee.sex == female,
		    Atdep <- mnesia:table(at_dep),
		    Atdep#at_dep.emp == E#employee.emp_no]
              ),

    H2 = qlc:q( [{Mgr#manager.emp,E} ||
		    {AtdepDeptId, E} <- H1,
		    Mgr <- mnesia:table(manager),
		    AtdepDeptId == Mgr#manager.dept]
	      ),

    Q = qlc:q( [{E#employee.name, Boss#employee.name} ||
                   {MgrEmp,E} <- H2,
                   Boss <- mnesia:table(employee),
                   MgrEmp == Boss#employee.emp_no]
             ),
    mnesia:transaction(fun() -> qlc:e(Q) end).
%3

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
                [E] = mnesia:read(employee, Eno, write),
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
                At <- mnesia:table(at_dep),
                E#employee.salary > Salary,
                E#employee.emp_no == At#at_dep.emp,
                At#at_dep.dept_id == Dep]
	 ),
    F = fun() -> qlc:e(Q) end,
    mnesia:transaction(F).
%9
%10
get_emps2(Salary, Dep) ->
    Epat = mnesia:table_info(employee, wild_pattern),
    Apat = mnesia:table_info(at_dep, wild_pattern),
    F = fun() ->
                All = mnesia:match_object(Epat),
                High = filter(All, Salary),
                Alldeps = mnesia:match_object(Apat),
                filter_deps(High, Alldeps, Dep)
        end,
    mnesia:transaction(F).
                

filter([E|Tail], Salary) ->
    if 
        E#employee.salary > Salary ->
            [E | filter(Tail, Salary)];
        true ->
            filter(Tail, Salary)
    end;
filter([], _) ->
    [].

filter_deps([E|Tail], Deps, Dep) ->
    case search_deps(E#employee.name, Deps, Dep) of
        true ->
            [E | filter_deps(Tail, Deps, Dep)];
        false ->
            filter_deps(Tail, Deps, Dep)
    end;
filter_deps([], _,_) -> 
    [].


search_deps(Name, [D|Tail], Dep) ->
    if
        D#at_dep.emp == Name,
        D#at_dep.dept_id == Dep -> true;
        true -> search_deps(Name, Tail, Dep)
    end;
search_deps(_Name, _Tail, _Dep) ->
    false.

%10


                
%11
bench1() ->
    Me = #employee{emp_no= 104732,
               name = klacke,
               salary = 7,
               sex = male,
               phone = 99586,
               room_no = {221, 015}},
    
    F = fun() -> insert_emp(Me, 'B/DUR', [erlang, mnesia, otp]) end,
    T1 = timer:tc(company, dotimes, [1000, F]),
    mnesia:add_table_copy(employee, b@skeppet, ram_copies),
    mnesia:add_table_copy(at_dep, b@skeppet, ram_copies),
    mnesia:add_table_copy(in_proj, b@skeppet, ram_copies),
    T2 = timer:tc(company, dotimes, [1000, F]),
    {T1, T2}.

dotimes(0, _) ->
    ok;
dotimes(I, F) ->
    F(), dotimes(I-1, F).

%11
    
    

    
            
%12

dist_init() ->
    mnesia:create_table(employee,
                         [{ram_copies, [a@gin, b@skeppet]},
                          {attributes, record_info(fields,
						   employee)}]),
    mnesia:create_table(dept,
                         [{ram_copies, [a@gin, b@skeppet]},
                          {attributes, record_info(fields, dept)}]),
    mnesia:create_table(project,
                         [{ram_copies, [a@gin, b@skeppet]},
                          {attributes, record_info(fields, project)}]),
    mnesia:create_table(manager, [{type, bag}, 
                                  {ram_copies, [a@gin, b@skeppet]},
                                  {attributes, record_info(fields,
							   manager)}]),
    mnesia:create_table(at_dep,
                         [{ram_copies, [a@gin, b@skeppet]},
                          {attributes, record_info(fields, at_dep)}]),
    mnesia:create_table(in_proj,
                        [{type, bag}, 
                         {ram_copies, [a@gin, b@skeppet]},
                         {attributes, record_info(fields, in_proj)}]).
%12

%13
remove_proj(ProjName) ->
    F = fun() ->
                Ip = qlc:e(qlc:q([X || X <- mnesia:table(in_proj),
				       X#in_proj.proj_name == ProjName]
				)),
                mnesia:delete({project, ProjName}),
                del_in_projs(Ip)
        end,
    mnesia:transaction(F).

del_in_projs([Ip|Tail]) ->
    mnesia:delete_object(Ip),
    del_in_projs(Tail);
del_in_projs([]) ->
    done.
%13
        
%14
sync() ->
    case mnesia:wait_for_tables(tabs(), 10000) of
        {timeout, RemainingTabs} ->
            panic(RemainingTabs);
        ok ->
            synced
    end.

tabs() -> [employee, dept, project, at_dep, in_proj, manager].

%14


find_male_on_second_floor() ->
    Select = fun() ->
%21
      MatchHead = #employee{name='$1', sex=male, room_no={'$2', '_'}, _='_'},
      Guard = [{'>=', '$2', 220},{'<', '$2', 230}],
      Result = '$1',
      mnesia:select(employee,[{MatchHead, Guard, [Result]}])
%21
    end,
    mnesia:transaction(Select).

panic(X) -> exit({panic, X}).


fill_tables() ->
    Emps = 
        [
	 {employee, 104465, "Johnson Torbjorn",   1, male, 99184, {242,038}},
	 {employee, 107912, "Carlsson Tuula",     2, female,94556, {242,056}},
	 {employee, 114872, "Dacker Bjarne",      3, male, 99415, {221,035}},
	 {employee, 104531, "Nilsson Hans",       3, male, 99495, {222,026}},
	 {employee, 104659, "Tornkvist Torbjorn", 2, male, 99514, {222,022}},
	 {employee, 104732, "Wikstrom Claes",     2, male, 99586, {221,015}},
	 {employee, 117716, "Fedoriw Anna",       1, female,99143, {221,031}},
	 {employee, 115018, "Mattsson Hakan",     3, male, 99251, {203,348}}
        ],

    Dept = [
	    {dept, 'B/SF',  "Open Telecom Platform"},
	    {dept, 'B/SFP', "OTP - Product Development"},
	    {dept, 'B/SFR', "Computer Science Laboratory"}
	   ],

    Projects = [
		{project, erlang, 1},
		{project, otp, 2},
		{project, beam, 3},
		{project, mnesia, 5},
		{project, wolf, 6},
		{project, documentation, 7},
		{project, www, 8}
	       ],

    Manager = [
	       {manager, 104465, 'B/SF'},
	       {manager, 104465, 'B/SFP'},
	       {manager, 114872, 'B/SFR'}
	      ],

    At_dep = [
	      {at_dep, 104465, 'B/SF'},
	      {at_dep, 107912, 'B/SF'},
	      {at_dep, 114872, 'B/SFR'},
	      {at_dep, 104531, 'B/SFR'},
	      {at_dep, 104659, 'B/SFR'},
	      {at_dep, 104732, 'B/SFR'},
	      {at_dep, 117716, 'B/SFP'},
	      {at_dep, 115018, 'B/SFP'}
	     ],

    In_proj = [
	       {in_proj, 104465, otp},
	       {in_proj, 107912, otp},
	       {in_proj, 114872, otp},
	       {in_proj, 104531, otp},
	       {in_proj, 104531, mnesia},
	       {in_proj, 104545, wolf},
	       {in_proj, 104659, otp},
	       {in_proj, 104659, wolf},
	       {in_proj, 104732, otp},
	       {in_proj, 104732, mnesia},
	       {in_proj, 104732, erlang},
	       {in_proj, 117716, otp},
	       {in_proj, 117716, documentation},
	       {in_proj, 115018, otp},
	       {in_proj, 115018, mnesia}
	      ],
    
    [mnesia:dirty_write(W) || W <- Emps],
    [mnesia:dirty_write(W) || W <- Dept],
    [mnesia:dirty_write(W) || W <- Projects],
    %% Relations
    [mnesia:dirty_write(W) || W <- Manager],
    [mnesia:dirty_write(W) || W <- At_dep],
    [mnesia:dirty_write(W) || W <- In_proj],
    
    ok.
