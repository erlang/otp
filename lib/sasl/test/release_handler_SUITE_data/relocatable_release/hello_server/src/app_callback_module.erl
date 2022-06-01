-module(app_callback_module).

-behaviour(application).

-export([start/2, stop/1, get_response/0, update/0]).

start(_Type, _Args) ->
    Pid = hello_server:start_server(),
    global:register_name(hello_server, Pid),
    {ok, Pid}.

update() ->
    global:whereis_name(hello_server) ! update,
    ok.

get_response() ->
    global:whereis_name(hello_server) ! self(),
    receive
        A ->
            A
    end.

stop(_State) ->
    Pid = global:whereis_name(hello_server),
    hello_server:stop(Pid),
    ok.
