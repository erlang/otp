%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2013. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%
-module(heart). 

-compile(no_native).
% 'no_native' as part of a crude fix to make init:restart/0 work by clearing
% all hipe inter-module information (hipe_mfa_info's in hipe_bif0.c).

%%%--------------------------------------------------------------------
%%% This is a rewrite of pre_heart from BS.3.
%%%
%%% The purpose of this process-module is to act as an supervisor
%%% of the entire erlang-system. This 'heart' beats with a frequence
%%% satisfying an external port program *not* reboot the entire
%%% system. If however the erlang-emulator would hang, a reboot is
%%% then needed.
%%%
%%% It recognizes the flag '-heart'
%%%--------------------------------------------------------------------
-export([start/0, init/2, set_cmd/1, clear_cmd/0, get_cmd/0, cycle/0]).

-define(START_ACK, 1).
-define(HEART_BEAT, 2).
-define(SHUT_DOWN, 3).
-define(SET_CMD, 4).
-define(CLEAR_CMD, 5).
-define(GET_CMD, 6).
-define(HEART_CMD, 7).
-define(PREPARING_CRASH, 8). % Used in beam vm

-define(TIMEOUT, 5000).
-define(CYCLE_TIMEOUT, 10000).
-define(HEART_PORT_NAME, heart_port).

%%---------------------------------------------------------------------

-spec start() -> 'ignore' | {'error', term()} | {'ok', pid()}.

start() ->
    case whereis(heart) of
	undefined ->
	    %% As heart survives a init:restart/0 the Parent
	    %% of heart must be init.
	    %% The init process is responsible to create a link
	    %% to heart.
	    Pid = spawn(?MODULE, init, [self(), whereis(init)]),
	    wait_for_init_ack(Pid);
	Pid ->
	    {ok, Pid}
    end.

wait_for_init_ack(From) ->
    receive
	{ok, From} = Ok ->
	    Ok;
	{no_heart, From} ->
	    ignore;
	{Error, From} ->
	    {error, Error}
    end.

-spec init(pid(), pid()) -> {'no_heart', pid()} | {'start_error', pid()}.

init(Starter, Parent) ->
    process_flag(trap_exit, true),
    process_flag(priority, max),
    register(heart, self()),
    case catch start_portprogram() of
	{ok, Port} ->
	    Starter ! {ok, self()},
	    loop(Parent, Port, "");
	no_heart ->
	    Starter ! {no_heart, self()};
	error ->
	    Starter ! {start_error, self()}
    end.

-spec set_cmd(Cmd) -> 'ok' | {'error', {'bad_cmd', Cmd}} when
      Cmd :: string().

set_cmd(Cmd) ->
    heart ! {self(), set_cmd, Cmd},
    wait().

-spec get_cmd() -> {ok, Cmd} when
      Cmd :: string().

get_cmd() ->
    heart ! {self(), get_cmd},
    wait().

-spec clear_cmd() -> ok.

clear_cmd() ->
    heart ! {self(), clear_cmd},
    wait().


%%% Should be used solely by the release handler!!!!!!!
-spec cycle() -> 'ok' | {'error', term()}.

cycle() ->
    heart ! {self(), cycle},
    wait().

wait() ->
    receive
	{heart, Res} ->
	    Res
    end.

start_portprogram() ->
    check_start_heart(),
    HeartCmd = "heart -pid " ++ os:getpid() ++ " " ++ get_heart_timeouts(),
    try open_port({spawn, HeartCmd}, [{packet, 2}]) of
	Port when is_port(Port) ->
	    case wait_ack(Port) of
		ok ->
		    %% register port so the vm can find it if need be
		    register(?HEART_PORT_NAME, Port),
		    {ok, Port};
		{error, Reason} ->
		    report_problem({{port_problem, Reason},
				    {heart, start_portprogram, []}}),
		    error
	    end
    catch
	_:Reason ->
	    report_problem({{open_port, Reason}, 
			    {heart, start_portprogram, []}}),
	    error
    end.

get_heart_timeouts() ->
    HeartOpts = case os:getenv("HEART_BEAT_TIMEOUT") of
		    false -> "";
		    H when is_list(H) -> 
			"-ht " ++ H
		end,
    HeartOpts ++ case os:getenv("HEART_BEAT_BOOT_DELAY") of
		     false -> "";
		     W when is_list(W) ->
			 " -wt " ++ W
		 end.

check_start_heart() ->
    case init:get_argument(heart) of
	{ok, [[]]} ->
	    ok;
	error ->
	    throw(no_heart);
	{ok, [[X|_]|_]} ->
	    report_problem({{bad_heart_flag, list_to_atom(X)},
			    {heart, check_start_heart, []}}),
	    throw(error)
    end.

wait_ack(Port) ->
    receive
	{Port, {data, [?START_ACK]}} ->
	    ok;
	{'EXIT', Port, badsig} -> % Since this is not synchronous, skip it!
	    wait_ack(Port);
	{'EXIT', Port, Reason} -> % The port really terminated.
	    {error, Reason}
    end.

loop(Parent, Port, Cmd) ->
    _ = send_heart_beat(Port),
    receive
	{From, set_cmd, NewCmd0} ->
	    Enc = file:native_name_encoding(),
	    case catch unicode:characters_to_binary(NewCmd0,Enc,Enc) of
		NewCmd when is_binary(NewCmd), byte_size(NewCmd) < 2047 ->
		    _ = send_heart_cmd(Port, NewCmd),
		    _ = wait_ack(Port),
		    From ! {heart, ok},
		    loop(Parent, Port, NewCmd);
		_ ->
		    From ! {heart, {error, {bad_cmd, NewCmd0}}},
		    loop(Parent, Port, Cmd)
	    end;
	{From, clear_cmd} ->
	    From ! {heart, ok},
	    _ = send_heart_cmd(Port, ""),
	    _ = wait_ack(Port),
	    loop(Parent, Port, "");
	{From, get_cmd} ->
	    From ! {heart, get_heart_cmd(Port)},
	    loop(Parent, Port, Cmd);
	{From, cycle} ->
	    %% Calls back to loop
	    do_cycle_port_program(From, Parent, Port, Cmd);  
	{'EXIT', Parent, shutdown} ->
	    no_reboot_shutdown(Port);
	{'EXIT', Parent, Reason} ->
	    exit(Port, Reason),
	    exit(Reason);
	{'EXIT', Port, badsig} ->  % we can ignore badsig-messages!
	    loop(Parent, Port, Cmd);
	{'EXIT', Port, _Reason} ->
	    exit({port_terminated, {heart, loop, [Parent, Port, Cmd]}});
	_ -> 
	    loop(Parent, Port, Cmd)
    after
	?TIMEOUT ->
	    loop(Parent, Port, Cmd)
    end.

-spec no_reboot_shutdown(port()) -> no_return().

no_reboot_shutdown(Port) ->
    _ = send_shutdown(Port),
    receive
	{'EXIT', Port, Reason} when Reason =/= badsig ->
	    exit(normal)
    end.

do_cycle_port_program(Caller, Parent, Port, Cmd) ->
    unregister(?HEART_PORT_NAME),
    case catch start_portprogram() of
	{ok, NewPort} ->
	    _ = send_shutdown(Port),
	    receive
		{'EXIT', Port, _Reason} ->
		    _ = send_heart_cmd(NewPort, Cmd),
		    Caller ! {heart, ok},
		    loop(Parent, NewPort, Cmd)
	    after
		?CYCLE_TIMEOUT ->
		    %% Huh! Two heart port programs running...
		    %% well, the old one has to be sick not to respond
		    %% so we'll settle for the new one...
		    _ = send_heart_cmd(NewPort, Cmd),
		    Caller ! {heart, {error, stop_error}},
		    loop(Parent, NewPort, Cmd)
	    end;
	no_heart ->
	    Caller ! {heart, {error, no_heart}},
	    loop(Parent, Port, Cmd);
	error ->
	    Caller ! {heart, {error, start_error}},
	    loop(Parent, Port, Cmd)
    end.
    

%% "Beates" the heart once.
send_heart_beat(Port) -> Port ! {self(), {command, [?HEART_BEAT]}}.

%% Set a new HEART_COMMAND.
send_heart_cmd(Port, []) ->
    Port ! {self(), {command, [?CLEAR_CMD]}};
send_heart_cmd(Port, Cmd) ->
    Port ! {self(), {command, [?SET_CMD|Cmd]}}.

get_heart_cmd(Port) ->
    Port ! {self(), {command, [?GET_CMD]}},
    receive
	{Port, {data, [?HEART_CMD | Cmd]}} ->
	    {ok, Cmd}
    end.

%% Sends shutdown command to the port.
send_shutdown(Port) -> Port ! {self(), {command, [?SHUT_DOWN]}}.

%% We must report using erlang:display/1 since we don't know whether
%% there is an error_logger available or not.
report_problem(Error) ->
    erlang:display(Error).
