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

-module(erlpmd).
-behaviour(gen_server).

-include("erlpmd_internal.hrl").
-include("../../kernel/src/erl_epmd.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Args) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Argv) ->
	erlpmd = ets:new(erlpmd, [public, named_table]),
	error_logger:info_msg("ErlPMD: started.~n"),
	self() ! notify_init,
	{ok, Argv}.

handle_call(Request, From, State) ->
	error_logger:warning_msg("ErlPMD: strange call: ~p from: ~p.~n", [Request, From]),
	{reply, ok, State}.

handle_cast({{msg,From},<<?EPMD_ALIVE2_REQ, PortNo:16, NodeType:8, Proto:8, HiVer:16, LoVer:16, NLen:16, Rest/binary>>, Fd, Ip, Port}, State = #argv{delay_write = DelayWrite}) ->
	<<NodeName:NLen/binary, _ELen:16, Extra/binary>> = Rest,
	Creation = random:uniform(3),
	error_logger:info_msg(
		"ErlPMD: alive request from ~s:~b PortNo: ~b, NodeType: ~b, Proto: ~b, HiVer: ~b, LoVer: ~b, NodeName: '~s', Extra: ~p, Creation: ~b.~n",
		[inet_parse:ntoa(Ip), Port, PortNo, NodeType, Proto, HiVer, LoVer, NodeName, Extra, Creation]),
	case ets:lookup(erlpmd, NodeName) of
		[] ->
			ets:insert_new(erlpmd, {NodeName, {PortNo, NodeType, Proto, HiVer, LoVer, Extra, Creation, Fd}}),
			reply(From, {msg, <<?EPMD_ALIVE2_RESP, 0:8, Creation:16>>, Ip, Port}, DelayWrite);
		_ ->
			% Already registered - reply with error
			error_logger:error_msg("ErlPMD: ~s 'name' is already registered.~n", [NodeName]),
			reply(From, {msg, <<?EPMD_ALIVE2_RESP, 1:8, 99:16>>, Ip, Port}, DelayWrite)
	end,
	{noreply, State};


handle_cast({{msg, From},<<?EPMD_PORT_PLEASE2_REQ, NodeName/binary>>, _Fd, Ip, Port}, State = #argv{delay_write = DelayWrite}) ->
	error_logger:info_msg("ErlPMD: port ~s request from ~s:~p.~n", [NodeName, inet_parse:ntoa(Ip), Port]),
	case ets:lookup(erlpmd, NodeName) of
		[] ->
			reply(From, {msg, <<$w, 1:8>>, Ip, Port}, DelayWrite);
		[{NodeName, {PortNo, NodeType, Proto, HiVer, LoVer, Extra, _, _}}] ->
			NLen = size(NodeName),
			ELen = size(Extra),
			reply(From,
			      {msg, <<?EPMD_PORT2_RESP, 0:8, PortNo:16, NodeType:8, Proto:8, HiVer:16, LoVer:16, NLen:16, NodeName:NLen/binary, ELen:16, Extra:ELen/binary>>, Ip, Port},
			      DelayWrite)
	end,
	gen_server:cast(From, {close, Ip, Port}),
	{noreply, State};

handle_cast({{msg, From},<<?EPMD_NAMES>>, _Fd, Ip, Port}, State = #argv{port = ServerPort, delay_write = DelayWrite}) ->
	error_logger:info_msg("ErlPMD: name(s) request from ~s:~p.~n", [inet_parse:ntoa(Ip), Port]),
	Nodes = list_to_binary(lists:flatten([ io_lib:format("name ~s at port ~p~n", [X, Y]) || [X, Y] <- ets:match(erlpmd, {'$1', {'$2', 77, '_', '_', '_', '_', '_', '_'}})])),
	reply(From, {msg, <<ServerPort:32, Nodes/binary>>, Ip, Port}, DelayWrite),
	gen_server:cast(From, {close, Ip, Port}),
	error_logger:info_msg("~s", [Nodes]),
	{noreply, State};

handle_cast({{msg, From},<<?EPMD_DUMP>>, _Fd, Ip, Port}, State = #argv{port = ServerPort, delay_write = DelayWrite}) ->
	error_logger:info_msg("ErlPMD: dump request from ~s:~p.~n", [inet_parse:ntoa(Ip), Port]),
	Nodes = list_to_binary(lists:flatten([ io_lib:format("active name     ~s at port ~p, fd = ~p ~n", [X, Y, F]) || [X, Y, F] <- ets:match(erlpmd, {'$1', {'$2', 77, '_', '_', '_', '_', '_', '$3'}})])),
	reply(From, {msg, <<ServerPort:32, Nodes/binary>>, Ip, Port}, DelayWrite),
	gen_server:cast(From, {close, Ip, Port}),
	{noreply, State};

handle_cast({{msg, From},<<?EPMD_KILL>>, _Fd, Ip, Port}, #argv{relaxed_command_check = true, delay_write = DelayWrite}) ->
	% Allow stop command in case we're running with -relaxed_command_check
	% w/o checking for actually available nodes
	error_logger:info_msg("ErlPMD: kill request from ~s:~p.~n", [inet_parse:ntoa(Ip), Port]),
	reply(From, {msg, <<"OK">>, Ip, Port}, DelayWrite),
	gen_server:cast(From, stop),
	{stop, normal, true};
handle_cast({{msg, From},<<?EPMD_KILL>>, _Fd, Ip, Port}, State = #argv{relaxed_command_check = false, delay_write = DelayWrite}) ->
	error_logger:info_msg("ErlPMD: kill request from ~s:~p.~n", [inet_parse:ntoa(Ip), Port]),
	reply(From, {msg, <<"OK">>, Ip, Port}, DelayWrite),
	case ets:match(erlpmd, {'_', {'_', '_', '_', '_', '_', '_', '_', '_'}}) of
		[] ->
			% No live nodes - we may exit now
			gen_server:cast(From, stop),
			{stop, normal, false};
		_ ->
			% Disallow killing while some nodes are alive
			{noreply, State}
	end;

handle_cast({{msg, From},<<?EPMD_STOP, NodeName/binary>>, _Fd, Ip, Port}, State = #argv{relaxed_command_check = false, delay_write = DelayWrite}) ->
	% Ignore stop command in case we're running w/o -relaxed_command_check
	error_logger:info_msg("ErlPMD: '~s' stop request from ~s:~p. (IGNORED)~n", [NodeName, inet_parse:ntoa(Ip), Port]),
	reply(From, {msg, <<"STOPPED">>, Ip, Port}, DelayWrite),
	{noreply, State};
handle_cast({{msg, From},<<?EPMD_STOP, NodeName/binary>>, _Fd, Ip, Port}, State = #argv{relaxed_command_check = true, delay_write = DelayWrite}) ->
	error_logger:info_msg("ErlPMD: '~s' stop request from ~s:~p.~n", [NodeName, inet_parse:ntoa(Ip), Port]),
	case ets:match(erlpmd, {NodeName, {'_', '_', '_', '_', '_', '_', '_', '_'}}) of
		[] ->
			reply(From, {msg, <<"NOEXIST">>, Ip, Port}, DelayWrite);
		_ ->
			ets:delete(erlpmd, NodeName),
			reply(From, {msg, <<"STOPPED">>, Ip, Port}, DelayWrite)
	end,
	gen_server:cast(From, {close, Ip, Port}),
	{noreply, State};

handle_cast({{close, _From}, Fd}, State) ->
	error_logger:info_msg("ErlPMD: closed connection: ~p.~n", [Fd]),
	case ets:match(erlpmd, {'$1', {'_', '_', '_', '_', '_', '_', '_', Fd}}) of
		[[NodeName]] -> ets:delete(erlpmd, NodeName);
		_ -> ok
	end,
	{noreply, State};

handle_cast(Msg, State) ->
	error_logger:warning_msg("ErlPMD: strange cast: ~p.~n", [Msg]),
	{noreply, State}.

handle_info(notify_init, State) ->
	error_logger:warning_msg("ErlPMD: info: ~p while ~p.~n", [notify_init, State]),
	{module, sd_notify} == code:load_file(sd_notify) andalso sd_notify:sd_notifyf(0, "READY=1~nSTATUS=~s~nMAINPID=~s", ["Processing port mapping requests...", os:getpid()]),
	{noreply, State};

handle_info(Info, State) ->
	error_logger:warning_msg("ErlPMD: strange info: ~p.~n", [Info]),
	{noreply, State}.

terminate(_Reason, _State) ->
	error_logger:info_msg("ErlPMD: stopped.~n"),
	init:stop().

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

reply(From, Msg, 0) ->
	gen_server:cast(From, Msg);
reply(From, Msg, Timeout) ->
	timer:sleep(Timeout * 1000),
	reply(From, Msg, 0).
