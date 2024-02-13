%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2024. All Rights Reserved.
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
-moduledoc """
Heartbeat monitoring of an Erlang runtime system.

This modules contains the interface to the `heart` process. `heart` sends
periodic heartbeats to an external port program, which is also named `heart`.
The purpose of the `heart` port program is to check that the Erlang runtime
system it is supervising is still running. If the port program has not received
any heartbeats within `HEART_BEAT_TIMEOUT` seconds (defaults to 60 seconds), the
system can be rebooted.

An Erlang runtime system to be monitored by a heart program is to be started
with command-line flag `-heart` (see also [`erl(1)`](`e:erts:erl_cmd.md`)). The
`heart` process is then started automatically:

```text
% erl -heart ...
```

If the system is to be rebooted because of missing heartbeats, or a terminated
Erlang runtime system, environment variable `HEART_COMMAND` must be set before
the system is started. If this variable is not set, a warning text is printed
but the system does not reboot.

To reboot on Windows, `HEART_COMMAND` can be set to `heart -shutdown` (included
in the Erlang delivery) or to any other suitable program that can activate a
reboot.

The environment variable `HEART_BEAT_TIMEOUT` can be used to configure the heart
time-outs; it can be set in the operating system shell before Erlang is started
or be specified at the command line:

```text
% erl -heart -env HEART_BEAT_TIMEOUT 30 ...
```

The value (in seconds) must be in the range 10 < X <= 65535.

When running on OSs lacking support for monotonic time, `heart` is susceptible
to system clock adjustments of more than `HEART_BEAT_TIMEOUT` seconds. When this
happens, `heart` times out and tries to reboot the system. This can occur, for
example, if the system clock is adjusted automatically by use of the Network
Time Protocol (NTP).

If a crash occurs, an `erl_crash.dump` is _not_ written unless environment
variable `ERL_CRASH_DUMP_SECONDS` is set:

```text
% erl -heart -env ERL_CRASH_DUMP_SECONDS 10 ...
```

If a regular core dump is wanted, let `heart` know by setting the kill signal to
abort using environment variable `HEART_KILL_SIGNAL=SIGABRT`. If unset, or not
set to `SIGABRT`, the default behavior is a kill signal using `SIGKILL`:

```text
% erl -heart -env HEART_KILL_SIGNAL SIGABRT ...
```

If heart should _not_ kill the Erlang runtime system, this can be indicated
using the environment variable `HEART_NO_KILL=TRUE`. This can be useful if the
command executed by heart takes care of this, for example as part of a specific
cleanup sequence. If unset, or not set to `TRUE`, the default behaviour will be
to kill as described above.

```text
% erl -heart -env HEART_NO_KILL 1 ...
```

Furthermore, `ERL_CRASH_DUMP_SECONDS` has the following behavior on `heart`:

- **`ERL_CRASH_DUMP_SECONDS=0`** - Suppresses the writing of a crash dump file
  entirely, thus rebooting the runtime system immediately. This is the same as
  not setting the environment variable.

- **`ERL_CRASH_DUMP_SECONDS=-1`** - Setting the environment variable to a
  negative value does not reboot the runtime system until the crash dump file is
  completely written.

- **`ERL_CRASH_DUMP_SECONDS=S`** - `heart` waits for `S` seconds to let the
  crash dump file be written. After `S` seconds, `heart` reboots the runtime
  system, whether the crash dump file is written or not.

In the following descriptions, all functions fail with reason `badarg` if
`heart` is not started.
""".

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

-doc false.
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

-doc false.
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

-doc """
Sets a temporary reboot command. This command is used if a `HEART_COMMAND` other
than the one specified with the environment variable is to be used to reboot the
system. The new Erlang runtime system uses (if it misbehaves) environment
variable `HEART_COMMAND` to reboot.

Limitations: Command string `Cmd` is sent to the `heart` program as an ISO
Latin-1 or UTF-8 encoded binary, depending on the filename encoding mode of the
emulator (see `file:native_name_encoding/0`). The size of the encoded binary
must be less than 2047 bytes.
""".
-spec set_cmd(Cmd) -> 'ok' | {'error', {'bad_cmd', Cmd}} when
      Cmd :: string().

set_cmd(Cmd) ->
    ?MODULE ! {self(), set_cmd, Cmd},
    wait().

-doc """
Gets the temporary reboot command. If the command is cleared, the empty string
is returned.
""".
-spec get_cmd() -> {ok, Cmd} when
      Cmd :: string().

get_cmd() ->
    ?MODULE ! {self(), get_cmd},
    wait().

-doc """
Clears the temporary boot command. If the system terminates, the normal
`HEART_COMMAND` is used to reboot.
""".
-spec clear_cmd() -> ok.

clear_cmd() ->
    ?MODULE ! {self(), clear_cmd},
    wait().

-doc """
This validation callback will be executed before any heartbeat is sent to the
port program. For the validation to succeed it needs to return with the value
`ok`.

An exception within the callback will be treated as a validation failure.

The callback will be removed if the system reboots.
""".
-doc(#{since => <<"OTP 18.3">>}).
-spec set_callback(Module,Function) -> 'ok' | {'error', {'bad_callback', {Module, Function}}} when
      Module :: atom(),
      Function :: atom().

set_callback(Module, Function) ->
    ?MODULE ! {self(), set_callback, {Module,Function}},
    wait().

-doc """
Get the validation callback. If the callback is cleared, `none` will be
returned.
""".
-doc(#{since => <<"OTP 18.3">>}).
-spec get_callback() -> {'ok', {Module, Function}} | 'none' when
      Module :: atom(),
      Function :: atom().

get_callback() ->
    ?MODULE ! {self(), get_callback},
    wait().

-doc "Removes the validation callback call before heartbeats.".
-doc(#{since => <<"OTP 18.3">>}).
-spec clear_callback() -> ok.

clear_callback() ->
    ?MODULE ! {self(), clear_callback},
    wait().

-doc """
Valid options `set_options` are:

- **`check_schedulers`** - If enabled, a signal will be sent to each scheduler
  to check its responsiveness. The system check occurs before any heartbeat sent
  to the port program. If any scheduler is not responsive enough the heart
  program will not receive its heartbeat and thus eventually terminate the node.

Returns with the value `ok` if the options are valid.
""".
-doc(#{since => <<"OTP 18.3">>}).
-spec set_options(Options) -> 'ok' | {'error', {'bad_options', Options}} when
      Options :: [heart_option()].

set_options(Options) ->
    ?MODULE ! {self(), set_options, Options},
    wait().

-doc """
Returns `{ok, Options}` where `Options` is a list of current options enabled for
heart. If the callback is cleared, `none` will be returned.
""".
-doc(#{since => <<"OTP 18.3">>}).
-spec get_options() -> {'ok', Options} | 'none' when
      Options :: [atom()].

get_options() ->
    ?MODULE ! {self(), get_options},
    wait().

%%% Should be used solely by the release handler!!!!!!!
-doc false.
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
%% if something goes wrong -> no heartbeat.

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
