-module(upgradee).

-export([dispatch_loop/0]).

-ifdef(VERSION_1).
-define(VERSION,1).

-export([exp1/0]).     % only exported in v1
-export([exp1loc2/0]). % exported in v1, local in v2
-export([exp1exp2/0]). % exported in v1 and v2
-export([get_local_fun/1]).
-export([get_exp1exp2_fun/0]).
-export([exp1exp2_fun/0]).

exp1() -> ?VERSION.
loc1() -> ?VERSION.

-endif. % VERSION_1

-ifdef(VERSION_2).
-define(VERSION,2).

-export([exp2/0]).
-export([loc1exp2/0]).
-export([exp1exp2/0]).
-export([get_local_fun/1]).
-export([get_exp1exp2_fun/0]).
-export([exp1exp2_fun/0]).

exp2() -> ?VERSION.
loc2() -> ?VERSION.

-endif. % VERSION_2

exp1exp2() -> ?VERSION.
exp1loc2() -> ?VERSION.
loc1exp2() -> ?VERSION.
loc1loc2() -> ?VERSION.

get_local_fun(Env) -> fun() -> {?VERSION,Env} end.
get_exp1exp2_fun() -> fun ?MODULE:exp1exp2_fun/0.

exp1exp2_fun() ->
    ?VERSION.

dispatch_loop() ->
    receive 
	upgrade_order ->
	    %%erlang:display({"upgradee version", ?VERSION, "got upgrade_order"}),
	    ?MODULE:dispatch_loop();

	Msg -> 
	    %%erlang:display({"upgradee version", ?VERSION, "got msg", Msg}),
	    {Func,Ret} = case Msg of
			     %% Local calls
			     {Pid, local, F=exp1} ->
				 {F, local_exp1()};
			     {Pid, local, F=loc1} ->
				 {F, local_loc1()};
			     {Pid, local, F=exp1exp2} ->
				 {F, catch exp1exp2()};
			     {Pid, local, F=exp1loc2} ->
				 {F, catch exp1loc2()};
			     {Pid, local, F=loc1exp2} ->
				 {F, catch loc1exp2()};
			     {Pid, local, F=loc1loc2} ->
				 {F, catch loc1loc2()};
			     {Pid, local, F=exp2} ->
				 {F, local_exp2()};
			     {Pid, local, F=loc2} ->
				 {F, local_loc2()};
			     
			     %% Extern calls to own module
			     {Pid, external, F=exp1} ->
				 {F, catch ?MODULE:exp1()};
			     {Pid, external, F=loc1} ->
				 {F, catch ?MODULE:loc1()};
			     {Pid, external, F=exp1exp2} ->
				 {F, catch ?MODULE:exp1exp2()};
			     {Pid, external, F=exp1loc2} ->
				 {F, catch ?MODULE:exp1loc2()};
			     {Pid, external, F=loc1exp2} ->
				 {F, catch ?MODULE:loc1exp2()};
			     {Pid, external, F=loc1loc2} ->
				 {F, catch ?MODULE:loc1loc2()};
			     {Pid, external, F=exp2} ->
				 {F, catch ?MODULE:exp2()};
			     {Pid, external, F=loc2} ->
				 {F, catch ?MODULE:loc2()};
			     
			     %% External calls to other module
			     {Pid, other, F=exp1} ->
				 {F, catch other:exp1()};
			     {Pid, other, F=loc1} ->
				 {F, catch other:loc1()};
			     {Pid, other, F=exp1exp2} ->
				 {F, catch other:exp1exp2()};
			     {Pid, other, F=exp1loc2} ->
				 {F, catch other:exp1loc2()};
			     {Pid, other, F=loc1exp2} ->
				 {F, catch other:loc1exp2()};
			     {Pid, other, F=loc1loc2} ->
				 {F, catch other:loc1loc2()};
			     {Pid, other, F=exp2} ->
				 {F, catch other:exp2()};
			     {Pid, other, F=loc2} ->
				 {F, catch other:loc2()}
			 end,
	    Pid ! {self(), call_result, Func, Ret},
	    
	    dispatch_loop() % A local call, we don't want to upgrade the dispatcher
    end.



-ifdef(VERSION_1).
local_exp1() -> catch exp1().
local_loc1() -> catch loc1().
-else.
local_exp1() ->
    %%erlang:display({"upgradee:local_exp1 in version", ?VERSION}),
    {cannot_compile,?VERSION}.
local_loc1() -> {cannot_compile,?VERSION}.
-endif.

-ifdef(VERSION_2).
local_exp2() -> catch exp2().
local_loc2() -> catch loc2().
-else.
local_exp2() -> 
    %%erlang:display({"upgradee:local_exp2 in version", ?VERSION}),
    {cannot_compile,?VERSION}.
local_loc2() -> 
    {cannot_compile,?VERSION}.
-endif.
