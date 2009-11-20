%%%-------------------------------------------------------------------
%%% File    : test_driver.erl
%%% Author  : Bertil Karlsson <bertil@finrod>
%%% Description : 
%%%
%%% Created : 27 Mar 2002 by Bertil Karlsson <bertil@finrod>
%%%-------------------------------------------------------------------
-module(test_driver).

-export([start/0, start/1, init/1, complete/1,complete/0]).

start() ->
    start("asn1_erl_drv").

start(SharedLib) ->
    case erl_ddll:load_driver("../priv/bin/", SharedLib) of
	ok -> ok;
	{error, already_loaded} -> ok;
	Error -> exit({error, could_not_load_driver,Error})
    end,
    spawn(?MODULE, init, [SharedLib]).

init(SharedLib) ->
    Port = open_port({spawn, SharedLib}, []),
    register(drv_complete,Port),
    register(compl_pid,self()),
    receive
	stop ->
	    exit(goodbye)
    end.

complete(Data) ->
    Ret=port_control(drv_complete,1,Data),
    io:format("complete result:~n~p~n",[Ret]).

complete() ->
    %% The result should be <<64,192,17,17,24,96>>
    Data1 = [<<0:8>>,<<1:8>>,<<2:8>>,<<10:8,2:8,3:8>>,<<20:8,2:8,17:8,17:8>>,<<30:8,2:8,1:8,24:8>>,<<31:8,2:8,0:8,1:8,24:8>>,<<2:8>>],
    complete(Data1),
    io:format("should have been:~n<<64,192,17,17,24,96>>~n"),
    Data2 = [40,8,8,1,0,1,0,1,0,1,0,0,2,1,45,8,1,170,45,7,1,170],
    complete(Data2),
    io:format("should have been:~n<<170,0,213,85>>~n"),
    Data3 = [40,8,8,1,0,1,0,1,0,1,0,0,2,1,45,8,1,170,45,8,1,170],
    complete(Data3),
    io:format("should have been:~n<<170,0,213,85,0>>~n").
