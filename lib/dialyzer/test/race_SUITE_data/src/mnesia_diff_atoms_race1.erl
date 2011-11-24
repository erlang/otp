%% This tests that the race condition detection between mnesia:dirty_read/
%% mnesia:dirty_write is robust even when the functions are called with
%% different atoms as arguments.

-module(mnesia_diff_atoms_race1).
-export([test/2]).

-record(employee, {emp_no,
                   name,
                   salary,
                   sex,
                   phone,
                   room_no}).

test(Eno, Raise) ->
  {race(employee, Eno, Raise), no_race(employee, Eno, Raise)}.

race(Tab, Eno, Raise) ->
    [E] = mnesia:dirty_read(Tab, Eno),
    Salary = E#employee.salary + Raise,
    New = E#employee{salary = Salary},
    aux(Tab, New).

no_race(Tab, Eno, Raise) ->
    [E] = mnesia:dirty_read(Tab, Eno),
    Salary = E#employee.salary + Raise,
    New = E#employee{salary = Salary},
    AnotherTab = employer,
    aux(AnotherTab, New).


aux(Table, Record) ->
  mnesia:dirty_write(Table, Record).
