%% This tests the presence of possible races due to an mnesia:dirty_read/
%% mnesia:dirty_write combination. It takes into account multiple
%% mnesia:dirty_writes that might exist.

-module(mnesia_dirty_read_write_double2).
-export([raise/3]).

-record(employee, {emp_no,
                   name,
                   salary,
                   sex,
                   phone,
                   room_no}).


raise(Eno, Raise, Room) ->
    [E] = mnesia:dirty_read({employee, Eno}),
    Salary = E#employee.salary + Raise,
    New = E#employee{salary = Salary},
    mnesia:dirty_write(New),
    move(E, Room).

move(E, Room) ->
    New = E#employee{room_no = Room},
    mnesia:dirty_write(New).
