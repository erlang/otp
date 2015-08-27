%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2015. All Rights Reserved.
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

-module(erlpmd_ctl).

-export([start/0]).

-include("erlpmd_internal.hrl").
-include("../../kernel/src/erl_epmd.hrl").

start() ->
	%%
	%% Check environment variables first
	%%

	EnvAddr = os:getenv("ERL_EPMD_ADDRESS", "0.0.0.0"),
	EnvPort = os:getenv("ERL_EPMD_PORT", "4369"),

	%%
	%% Now check for command-line switches
	%%

	Addrs  = addrstr_to_ip(get_arg(address, EnvAddr)),
	Port = list_to_integer(get_arg(port, EnvPort)),

	Names = check_arg(names),
	Kill = check_arg(kill),
	Stop = check_arg(stop),
	Help = check_arg(h),

	if
		Names -> names(Addrs, Port);
		Kill -> kill(Addrs, Port);
		Stop -> stop(Addrs, Port);
		Help -> help();
		true -> daemon(Addrs, Port)
	end.

%%
%% Private functions
%%

names([Addr | _], Port) ->
	{ok, <<Port:32, Data/binary>>} = sendrecv(Addr, Port, <<?EPMD_NAMES>>),
	io:format("epmd: up and running on port ~p with data:~n", [Port]),
	io:format("~s", [Data]),
	init:stop().

kill([Addr | _], Port) ->
	{ok, <<"OK">>} = sendrecv(Addr, Port, <<?EPMD_KILL>>),
	io:format("Killed~n"),
	init:stop().

stop([Addr | _] , Port) ->
	{ok, [[N]]} = init:get_argument(stop),
	Name = list_to_binary(N),

	{ok, Ret} = sendrecv(Addr, Port, <<?EPMD_STOP, Name/binary>>),

	io:format("~s~n", [Ret]),
	init:stop().

sendrecv(Addr, Port, Data) ->
	{ok, Fd} = gen_tcp:connect(Addr, Port, [binary, {packet, 2}, {active, false}]),
	ok = gen_tcp:send(Fd, Data),
	% We have to switch to raw here
	inet:setopts(Fd, [{packet, raw}]),
	% FIXME should we use packet_timeout here (or default value)?
	{ok, Ret} = gen_tcp:recv(Fd, 0, 1000),
	ok = gen_tcp:close(Fd),
	{ok, Ret}.

help() ->
    io:format(
"usage: epmd [-d|-debug] [DbgExtra...] [-address List]~n"
"            [-port No] [-daemon] [-relaxed_command_check]~n"
"       epmd [-d|-debug] [-port No] [-names|-kill|-stop name]~n"
"~n"
"See the Erlang epmd manual page for info about the usage.~n"
"~n"
"Regular options~n"
"    -address List~n"
"        Let epmd listen only on the comma-separated list of IP~n"
"        addresses (and on the loopback interface).~n"
"    -port No~n"
"        Let epmd listen to another port than default 4369~n"
"    -d~n"
"    -debug~n"
"        Enable debugging. This will give a log to~n"
"        the standard error stream. It will shorten~n"
"        the number of saved used node names to 5.~n"
"~n"
"        If you give more than one debug flag you may~n"
"        get more debugging information.~n"
"    -daemon~n"
"        Start epmd detached (as a daemon)~n"
"    -relaxed_command_check~n"
"        Allow this instance of epmd to be killed with~n"
"        epmd -kill even if there are registered nodes.~n"
"        Also allows forced unregister (epmd -stop).~n"
"~n"
"DbgExtra options~n"
"    -packet_timeout Seconds~n"
"       Set the number of seconds a connection can be~n"
"       inactive before epmd times out and closes the~n"
"       connection (default 60).~n"
"~n"
"    -delay_accept Seconds~n"
"       To simulate a busy server you can insert a~n"
"       delay between epmd gets notified about that~n"
"       a new connection is requested and when the~n"
"       connections gets accepted.~n"
"~n"
"    -delay_write Seconds~n"
"       Also a simulation of a busy server. Inserts~n"
"       a delay before a reply is sent.~n"
"~n"
"Interactive options~n"
"    -names~n"
"       List names registered with the currently running epmd~n"
"    -kill~n"
"       Kill the currently running epmd~n"
"       (only allowed if -names show empty database or~n"
"       -relaxed_command_check was given when epmd was started).~n"
"    -stop Name~n"
"       Forcibly unregisters a name with epmd~n"
"       (only allowed if -relaxed_command_check was given when~n"
"       epmd was started).~n"
	),
	init:stop().

daemon(Addr, Port) ->
	%%
	%% Check environment variables first
	%%
	EnvRelaxedCommandCheck = os:getenv("ERL_EPMD_RELAXED_COMMAND_CHECK", "false"),

	%% Run daemonised - ignored for now FIXME
	_Daemonize = check_arg(daemon),

	%% ignored - autodetection of systemd
	check_arg(systemd) andalso error_logger:warning_msg("-systemd option is ignored (autodetected)~n"),

	%% Allow this instance of epmd to be killed with
	%% epmd -kill even if there are registered nodes.
	%% Also allows forced unregister (epmd -stop)
	RelaxedCommandCheck = check_arg(relaxed_command_check) orelse EnvRelaxedCommandCheck == "true",

	%% Set debug level
	Debug = check_arg(d) orelse check_arg(debug),

	%% DbgExtra options
	PacketTimeout = list_to_integer(get_arg(packet_timeout, "0")),
	DelayAccept = list_to_integer(get_arg(delay_accept, "0")),
	DelayWrite = list_to_integer(get_arg(delay_write, "0")),

	application:set_env(erlpmd, argv, #argv{address = Addr,
						port = Port,
						relaxed_command_check = RelaxedCommandCheck,
						debug = Debug,
						packet_timeout = PacketTimeout,
						delay_accept = DelayAccept,
						delay_write = DelayWrite
					       }),

	application:start(erlpmd).

%%
%% Various helpers
%%

get_arg(Arg, Default) ->
	case init:get_argument(Arg)of
		error -> Default;
		{ok,[[V]]} -> V
	end.

check_arg(Arg) ->
	case init:get_argument(Arg)of
		error -> false;
		{ok, _} -> true
	end.

addrstr_to_ip(AddrStr) ->
	[begin {ok, Y} = inet_parse:address(X), Y end || X <- string:tokens(AddrStr,",")].
