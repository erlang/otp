%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
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

%% 
%% @doc Percept Collector 
%%
%%	This module provides the user interface for the percept data
%	collection (profiling).
%% 

-module(percept_profile).
-export([
	start/1, 
	start/2, 
	start/3,
	stop/0
	]).


%%==========================================================================
%%
%% 		Type definitions 
%%
%%==========================================================================

%% @type percept_option() = procs | ports | exclusive

-type percept_option() :: 'procs' | 'ports' | 'exclusive' | 'scheduler'.

%%==========================================================================
%%
%% 		Interface functions
%%
%%==========================================================================

%% @spec start(Filename::string()) -> {ok, Port} | {already_started, Port}
%% @equiv start(Filename, [procs])

-spec start(Filename :: file:filename()) ->
	{'ok', port()} | {'already_started', port()}.

start(Filename) ->
    profile_to_file(Filename, [procs]).

%% @spec start(Filename::string(), [percept_option()]) -> {ok, Port} | {already_started, Port}
%%	Port = port()
%% @doc Starts profiling with supplied options. 
%%	All events are stored in the file given by Filename. 
%%	An explicit call to stop/0 is needed to stop profiling. 

-spec start(Filename :: file:filename(),
	    Options :: [percept_option()]) ->
	{'ok', port()} | {'already_started', port()}.

start(Filename, Options) ->
    profile_to_file(Filename, Options). 

%% @spec start(string(), MFA::mfa(), [percept_option()]) -> ok | {already_started, Port} | {error, not_started}
%%	Port = port()
%% @doc Starts profiling at the entrypoint specified by the MFA. All events are collected, 
%%	this means that processes outside the scope of the entry-point are also profiled. 
%%	No explicit call to stop/0 is needed, the profiling stops when
%%	the entry function returns.

-spec start(Filename :: file:filename(),
	    Entry :: {atom(), atom(), list()},
	    Options :: [percept_option()]) ->
	'ok' | {'already_started', port()} | {'error', 'not_started'}.

start(Filename, {Module, Function, Args}, Options) ->
    case whereis(percept_port) of
	undefined ->
            {ok, _} = profile_to_file(Filename, Options),
	    erlang:apply(Module, Function, Args),
	    stop();
	Port ->
	    {already_started, Port}
    end.

deliver_all_trace() ->
    Tracee = self(),
    Tracer = spawn(fun() -> 
	receive {Tracee, start} -> ok end,
    	Ref = erlang:trace_delivered(Tracee),
	receive {trace_delivered, Tracee, Ref} -> Tracee ! {self(), ok} end
    end),
    erlang:trace(Tracee, true, [procs, {tracer, Tracer}]),
    Tracer ! {Tracee, start},
    receive {Tracer, ok} -> ok end,
    erlang:trace(Tracee, false, [procs]),
    ok.

%% @spec stop() -> ok | {'error', 'not_started'}
%% @doc Stops profiling.
    
-spec stop() -> 'ok' | {'error', 'not_started'}.

stop() ->
    _ = erlang:system_profile(undefined, [runnable_ports, runnable_procs]),
    erlang:trace(all, false, [procs, ports, timestamp]),
    deliver_all_trace(), 
    case whereis(percept_port) of
    	undefined -> 
	    {error, not_started};
	Port ->
	    erlang:port_command(Port, erlang:term_to_binary({profile_stop, erlang:timestamp()})),
	    %% trace delivered?
	    erlang:port_close(Port),
	    ok
    end. 

%%==========================================================================
%%
%% 		Auxiliary functions 
%%
%%==========================================================================

profile_to_file(Filename, Opts) ->
    case whereis(percept_port) of 
	undefined ->
	    io:format("Starting profiling.~n", []),

	    erlang:system_flag(multi_scheduling, block),
	    Port = (dbg:trace_port(file, Filename))(),
	    % Send start time
	    erlang:port_command(Port, erlang:term_to_binary({profile_start, erlang:timestamp()})),
	    erlang:system_flag(multi_scheduling, unblock),
		
	    %% Register Port
    	    erlang:register(percept_port, Port),
	    set_tracer(Port, Opts), 
	    {ok, Port};
	Port ->
	    io:format("Profiling already started at port ~p.~n", [Port]),
	    {already_started, Port}
    end.

%% set_tracer

set_tracer(Port, Opts) ->
    {TOpts, POpts} = parse_profile_options(Opts),
    % Setup profiling and tracing
    erlang:trace(all, true, [{tracer, Port}, timestamp | TOpts]),
    _ = erlang:system_profile(Port, POpts),
    ok.

%% parse_profile_options

parse_profile_options(Opts) ->
    parse_profile_options(Opts, {[],[]}).

parse_profile_options([], Out) ->
    Out;
parse_profile_options([Opt|Opts], {TOpts, POpts}) ->
    case Opt of
	procs ->
	    parse_profile_options(Opts, {
		[procs | TOpts], 
		[runnable_procs | POpts]
	    });
	ports ->
	    parse_profile_options(Opts, {
		[ports | TOpts], 
		[runnable_ports | POpts]
	    });
	scheduler ->
	    parse_profile_options(Opts, {
		TOpts, 
		[scheduler | POpts]
	    });
	exclusive ->
	    parse_profile_options(Opts, {
		TOpts, 
		[exclusive | POpts]
	    });
	_ -> 
	    parse_profile_options(Opts, {TOpts, POpts})
    end.
