-module(uds).

-export([listen/1, connect/1, accept/1, send/2, recv/1, close/1,
	 get_port/1, get_status_counters/1, set_mode/2, controlling_process/2,
	 tick/1, get_creation/1]).

-define(decode(A,B,C,D), (((A) bsl 24) bor 
			  ((B) bsl 16) bor ((C) bsl 8) bor (D))).
-define(encode(N), [(((N) bsr 24) band 16#FF), (((N) bsr 16) band 16#FF),  
		    (((N) bsr 8) band 16#FF), ((N) band 16#FF)]).  
-define(check_server(), case whereis(uds_server) of 
			    undefined ->
				exit(uds_server_not_started);
			    _ ->
				ok
			end).

listen(Name) ->
    ?check_server(),
    command(port(),$L,Name).
    

connect(Name) ->
    ?check_server(),
    command(port(),$C,Name).

accept(Port) ->
    ?check_server(),
    case control(Port,$N) of
	{ok, N} ->
	    command(port(),$A,N);
	Else ->
	    Else
    end.

send(Port,Data) ->
    ?check_server(),
    command(Port, $S, Data).

recv(Port) ->
    ?check_server(),
    command(Port, $R, []).

close(Port) ->
    ?check_server(),
    (catch unlink(Port)), %% Avoids problem with trap exits.
    case (catch erlang:port_close(Port)) of
	{'EXIT', Reason} ->
	    {error, closed};
	_ ->
	    ok
    end.

get_port(Port) ->
    ?check_server(),
    {ok,Port}.

get_status_counters(Port) ->
    ?check_server(),
    case control(Port, $S) of
	{ok, {C0, C1, C2}} ->
	    {ok, C0, C1, C2};
	Other ->
	    Other
    end.

get_creation(Port) ->
    ?check_server(),
    case control(Port, $R) of
	{ok, [A]} ->
	    A;
	Else ->
	    Else
    end.
    

set_mode(Port, command) -> 
    ?check_server(),
    control(Port,$C);
set_mode(Port,intermediate) ->
    ?check_server(),
    control(Port,$I);
set_mode(Port,data) ->
    ?check_server(),
    control(Port,$D).

tick(Port) ->
    ?check_server(),
    control(Port,$T).

controlling_process(Port, Pid) ->
    ?check_server(),
    case (catch erlang:port_connect(Port, Pid)) of
	true ->
	    (catch unlink(Port)),
	    ok;
	{'EXIT', {badarg, _}} ->
	    {error, closed};
	Else ->
	    exit({unexpected_driver_response, Else})
    end.
    

control(Port, Command) ->
    case (catch erlang:port_control(Port, Command, [])) of
	[0] ->
	    ok;
	[0,A] ->
	    {ok, [A]};
	[0,A,B,C,D] ->
	    {ok, [A,B,C,D]};
	[0,A1,B1,C1,D1,A2,B2,C2,D2,A3,B3,C3,D3] ->
	    {ok, {?decode(A1,B1,C1,D1),?decode(A2,B2,C2,D2),
		  ?decode(A3,B3,C3,D3)}};
	[1|Error] ->
	    exit({error, list_to_atom(Error)});
	{'EXIT', {badarg, _}} ->
	    {error, closed};
	Else ->
	    exit({unexpected_driver_response, Else})
    end.
	    

command(Port, Command, Parameters) ->
    SavedTrapExit = process_flag(trap_exit,true),
    case (catch erlang:port_command(Port,[Command | Parameters])) of
	true ->
	    receive
		{Port, {data, [Command, $o, $k]}} ->
		    process_flag(trap_exit,SavedTrapExit),
		    {ok, Port};
		{Port, {data, [Command |T]}} ->
		    process_flag(trap_exit,SavedTrapExit),
		    {ok, T};
		{Port, Else} ->
		    process_flag(trap_exit,SavedTrapExit),
		    exit({unexpected_driver_response, Else});
		{'EXIT', Port, normal} ->
		    process_flag(trap_exit,SavedTrapExit),
		    {error, closed};
		{'EXIT', Port, Error} -> 
		    process_flag(trap_exit,SavedTrapExit),
		    exit(Error)
	    end;
	{'EXIT', {badarg, _}} ->
	    process_flag(trap_exit,SavedTrapExit),
	    {error, closed};
	Unexpected ->
	    process_flag(trap_exit,SavedTrapExit),
	    exit({unexpected_driver_response, Unexpected})
    end.

port() ->
    SavedTrapExit = process_flag(trap_exit,true),
    case open_port({spawn, "uds_drv"},[]) of
	P when port(P) ->
	    process_flag(trap_exit,SavedTrapExit),
	    P;
	{'EXIT',Error} ->
	    process_flag(trap_exit,SavedTrapExit),
	    exit(Error);
	Else ->
	    process_flag(trap_exit,SavedTrapExit),
	    exit({unexpected_driver_response, Else})
    end.

