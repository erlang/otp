-module(sp).
-vsn(1).

-export([start/0, get_data/0]).
-export([init/1, system_continue/3, system_terminate/4]).

-record(state, {data}).

start() ->
    Pid = proc_lib:spawn_link(?MODULE, init, [self()]),
    {ok, Pid}.

get_data() ->
    sp_server ! {self(), get_data},
    receive
	{sp_server, Data} -> Data
    end.

init(Parent) ->
    register(sp_server, self()),
    process_flag(trap_exit, true),
    loop(#state{}, Parent).

loop(State, Parent) ->
    receive
	{system, From, Request} ->
	    sys:handle_system_msg(Request, From, Parent, ?MODULE, [], State);
	{'EXIT', Parent, Reason} ->
	    cleanup(State),
	    exit(Reason);
	{From, get_data} ->
	    From ! {sp_server, State#state.data},
	    loop(State, Parent);
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
