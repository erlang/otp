%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2016. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
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
%%% The purpose of this process-module is to act as a supervisor
%%% of the entire erlang-system. This 'heart' beats with a frequence
%%% satisfying an external port program *not* reboot the entire
%%% system. If however the erlang-emulator would hang, a reboot is
%%% then needed.
%%%
%%% It recognizes the flag '-heart'
%%%--------------------------------------------------------------------
-export([start/0, init/2,
         set_cmd/1, clear_cmd/0, get_cmd/0,
         set_callback/2, clear_callback/0, get_callback/0,
         set_options/1, get_options/0,
         cycle/0]).

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

%% valid heart options
-define(SCHEDULER_CHECK_OPT, check_schedulers).

-type heart_option() :: ?SCHEDULER_CHECK_OPT.

-record(state,{port :: port(),
               cmd  :: [] | binary(),
               options :: [heart_option()],
               callback :: 'undefined' | {atom(), atom()}}).

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
    register(?MODULE, self()),
    case catch start_portprogram() of
	{ok, Port} ->
	    Starter ! {ok, self()},
	    loop(Parent, #state{port=Port, cmd=[], options=[]});
	no_heart ->
	    Starter ! {no_heart, self()};
	error ->
	    Starter ! {start_error, self()}
    end.

-spec set_cmd(Cmd) -> 'ok' | {'error', {'bad_cmd', Cmd}} when
      Cmd :: string().

set_cmd(Cmd) ->
    ?MODULE ! {self(), set_cmd, Cmd},
    wait().

-spec get_cmd() -> {ok, Cmd} when
      Cmd :: string().

get_cmd() ->
    ?MODULE ! {self(), get_cmd},
    wait().

-spec clear_cmd() -> ok.

clear_cmd() ->
    ?MODULE ! {self(), clear_cmd},
    wait().

-spec set_callback(Module,Function) -> 'ok' | {'error', {'bad_callback', {Module, Function}}} when
      Module :: atom(),
      Function :: atom().

set_callback(Module, Function) ->
    ?MODULE ! {self(), set_callback, {Module,Function}},
    wait().

-spec get_callback() -> {'ok', {Module, Function}} | 'none' when
      Module :: atom(),
      Function :: atom().

get_callback() ->
    ?MODULE ! {self(), get_callback},
    wait().

-spec clear_callback() -> ok.

clear_callback() ->
    ?MODULE ! {self(), clear_callback},
    wait().

-spec set_options(Options) -> 'ok' | {'error', {'bad_options', Options}} when
      Options :: [heart_option()].

set_options(Options) ->
    ?MODULE ! {self(), set_options, Options},
    wait().

-spec get_options() -> {'ok', Options} | 'none' when
      Options :: [atom()].

get_options() ->
    ?MODULE ! {self(), get_options},
    wait().

%%% Should be used solely by the release handler!!!!!!!
-spec cycle() -> 'ok' | {'error', term()}.

cycle() ->
    ?MODULE ! {self(), cycle},
    wait().

wait() ->
    receive
	{?MODULE, Res} ->
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
    case os:getenv("HEART_BEAT_TIMEOUT") of
	false -> "";
	H when is_list(H) ->
	    "-ht " ++ H
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

loop(Parent, #state{port=Port}=S) ->
    _ = send_heart_beat(S),
    receive
	{From, set_cmd, NewCmd0} ->
	    Enc = file:native_name_encoding(),
	    case catch unicode:characters_to_binary(NewCmd0,Enc,Enc) of
		NewCmd when is_binary(NewCmd), byte_size(NewCmd) < 2047 ->
		    _ = send_heart_cmd(Port, NewCmd),
		    _ = wait_ack(Port),
		    From ! {?MODULE, ok},
		    loop(Parent, S#state{cmd=NewCmd});
		_ ->
		    From ! {?MODULE, {error, {bad_cmd, NewCmd0}}},
		    loop(Parent, S)
	    end;
	{From, clear_cmd} ->
	    From ! {?MODULE, ok},
	    _ = send_heart_cmd(Port, []),
	    _ = wait_ack(Port),
	    loop(Parent, S#state{cmd = []});
	{From, get_cmd} ->
	    From ! {?MODULE, get_heart_cmd(Port)},
            loop(Parent, S);
	{From, set_callback, Callback} ->
            case Callback of
                {M,F} when is_atom(M), is_atom(F) ->
                    From ! {?MODULE, ok},
                    loop(Parent, S#state{callback=Callback});
                _ ->
		    From ! {?MODULE, {error, {bad_callback, Callback}}},
                    loop(Parent, S)
            end;
        {From, get_callback} ->
            Res = case S#state.callback of
                      undefined -> none;
                      Cb -> {ok, Cb}
                  end,
            From ! {?MODULE, Res},
            loop(Parent, S);
        {From, clear_callback} ->
            From ! {?MODULE, ok},
            loop(Parent, S#state{callback=undefined});
	{From, set_options, Options} ->
            case validate_options(Options) of
                Validated when is_list(Validated) ->
                    From ! {?MODULE, ok},
                    loop(Parent, S#state{options=Validated});
                _ ->
		    From ! {?MODULE, {error, {bad_options, Options}}},
                    loop(Parent, S)
            end;
        {From, get_options} ->
            Res = case S#state.options of
                      [] -> none;
                      Cb -> {ok, Cb}
                  end,
            From ! {?MODULE, Res},
            loop(Parent, S);
	{From, cycle} ->
	    %% Calls back to loop
	    do_cycle_port_program(From, Parent, S);
	{'EXIT', Parent, shutdown} ->
	    no_reboot_shutdown(Port);
	{'EXIT', Parent, Reason} ->
	    exit(Port, Reason),
	    exit(Reason);
	{'EXIT', Port, badsig} ->  % we can ignore badsig-messages!
	    loop(Parent, S);
	{'EXIT', Port, _Reason} ->
	    exit({port_terminated, {?MODULE, loop, [Parent, S]}});
	_ -> 
	    loop(Parent, S)
    after
	?TIMEOUT ->
	    loop(Parent, S)
    end.

-spec no_reboot_shutdown(port()) -> no_return().

no_reboot_shutdown(Port) ->
    _ = send_shutdown(Port),
    receive
	{'EXIT', Port, Reason} when Reason =/= badsig ->
	    exit(normal)
    end.

validate_options(Opts) -> validate_options(Opts,[]).
validate_options([],Res) -> Res;
validate_options([?SCHEDULER_CHECK_OPT=Opt|Opts],Res) -> validate_options(Opts,[Opt|Res]);
validate_options(_,_) -> error.

do_cycle_port_program(Caller, Parent, #state{port=Port} = S) ->
    unregister(?HEART_PORT_NAME),
    case catch start_portprogram() of
	{ok, NewPort} ->
	    _ = send_shutdown(Port),
	    receive
		{'EXIT', Port, _Reason} ->
		    _ = send_heart_cmd(NewPort, S#state.cmd),
		    Caller ! {?MODULE, ok},
		    loop(Parent, S#state{port=NewPort})
	    after
		?CYCLE_TIMEOUT ->
		    %% Huh! Two heart port programs running...
		    %% well, the old one has to be sick not to respond
		    %% so we'll settle for the new one...
		    _ = send_heart_cmd(NewPort, S#state.cmd),
		    Caller ! {?MODULE, {error, stop_error}},
		    loop(Parent, S#state{port=NewPort})
	    end;
	no_heart ->
	    Caller ! {?MODULE, {error, no_heart}},
	    loop(Parent, S);
	error ->
	    Caller ! {?MODULE, {error, start_error}},
	    loop(Parent, S)
    end.
    

%% "Beates" the heart once.
send_heart_beat(#state{port=Port, callback=Cb, options=Opts}) ->
    ok = check_system(Opts),
    ok = check_callback(Cb),
    Port ! {self(), {command, [?HEART_BEAT]}}.

%% Set a new HEART_COMMAND.
-dialyzer({no_improper_lists, send_heart_cmd/2}).
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

check_system([]) -> ok;
check_system([?SCHEDULER_CHECK_OPT|Opts]) ->
    ok = erts_internal:system_check(schedulers),
    check_system(Opts).

%% validate system by performing a check before the heartbeat
%% return 'ok' if everything is alright.
%% Terminate if with reason if something is a miss.
%% It is fine to timeout in the callback, in fact that is the intention
%% if something goes wront -> no heartbeat.

check_callback(Callback) ->
    case Callback of
        undefined -> ok;
        {M,F} ->
            erlang:apply(M,F,[])
    end.

%% Sends shutdown command to the port.
send_shutdown(Port) -> Port ! {self(), {command, [?SHUT_DOWN]}}.

%% We must report using erlang:display/1 since we don't know whether
%% there is an error_logger available or not.
report_problem(Error) ->
    erlang:display(Error).
