%% This tests that the race condition detection between mnesia:dirty_read/
%% mnesia:dirty_write is robust even when the functions are called with
%% different atoms as arguments.

-module(mnesia_diff_atoms_race2).
-export([test/2]).

-record(employee, {emp_no,
                   name,
                   salary,
                   sex,
                   phone,
                   room_no}).

-record(employer, {emp_no,
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
    aux(New).

no_race(Tab, Eno, Raise) ->
    [E] = mnesia:dirty_read(Tab, Eno),
    AnotherRecord = #employer{},
    aux(AnotherRecord).

aux(Record) ->
  mnesia:dirty_write(Record).
