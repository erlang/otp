-module(sp).
-vsn(2).

-export([start/0, get_data/0, set_data/1]).
-export([init/1, system_continue/3, system_terminate/4, 
        system_code_change/4]).

-record(state, {data, last_pid}).

start() ->
    Pid = proc_lib:spawn_link(?MODULE, init, [self()]),
    {ok, Pid}.

get_data() ->
    sp_server ! {self(), get_data},
    receive
	{sp_server, Data} -> Data
    end.

set_data(Data) ->
    sp_server ! {self(), set_data, Data}.

init(Parent) ->
    register(sp_server, self()),
    process_flag(trap_exit, true),
    loop(#state{last_pid = no_one}, Parent).

loop(State, Parent) ->
    receive
	{system, From, Request} ->
	    sys:handle_system_msg(Request, From, Parent, 
				  ?MODULE, [], State);
	{'EXIT', Parent, Reason} ->
	    cleanup(State),
	    exit(Reason);
	{From, get_data} ->
	    From ! {sp_server, State#state.data},
	    loop(State, Parent);
	{From, set_data, Data} ->
	    loop(State#state{data = Data, last_pid = From}, Parent);
	_Any ->
	    loop(State, Parent)
    end.

cleanup(State) -> ok.

%% Here are the sys call back functions
system_continue(Parent, _, State) ->
    loop(State, Parent).

system_terminate(Reason, Parent, _, State) ->
    cleanup(State),
    exit(Reason).

system_code_change({state, Data}, _Mod, 1, _Extra) ->
    {ok, #state{data = Data, last_pid = no_one}};
system_code_change(#state{data = Data}, _Mod, {down, 1}, _Extra) ->
    {ok, {state, Data}}.
