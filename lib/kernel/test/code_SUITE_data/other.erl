-module(other).

-ifdef(VERSION_1).
-define(VERSION,1).
-export([exp1/0]).
-export([exp1loc2/0]).
-export([exp1exp2/0]).
exp1() -> check([loc1(),exp1loc2(),exp1exp2(),loc1exp2(),loc1loc2()]).
loc1() -> check([exp1loc2(),exp1exp2(),loc1exp2(),loc1loc2()]).
exp1loc2() -> check([exp1exp2(),loc1exp2(),loc1loc2()]).
exp1exp2() -> check([loc1exp2(),loc1loc2()]).
loc1exp2() -> check([loc1loc2()]).
-endif. % VERSION_1

-ifdef(VERSION_2).
-define(VERSION,2).
-export([exp2/0]).
-export([loc1exp2/0]).
-export([exp1exp2/0]).
loc1exp2() -> check([exp1exp2(),exp1loc2(),loc2(),exp2(),loc1loc2()]).
exp1exp2() -> check([exp1loc2(),loc2(),exp2(),loc1loc2()]).
exp1loc2() -> check([loc2(),exp2(),loc1loc2()]).
loc2() -> check([exp2(),loc1loc2()]).
exp2() -> check([loc1loc2()]).

-endif. % VERSION_2

loc1loc2() -> ?VERSION.


check(VerList) ->
    case lists:all(fun(?VERSION) -> true; (_) -> false end,
		   VerList) of
	true ->
	    ?VERSION;
	false ->
	    VerList
    end.
