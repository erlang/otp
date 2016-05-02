%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2016. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% Purpose: Utility module used by the codec suites when testing flex 
%%          configured text codec(s).
%%----------------------------------------------------------------------

-module(megaco_codec_flex_lib).

%% ----

-include("megaco_test_lib.hrl").

%% ----

-export([
	 init/1, 
	 finish/1,
	 scanner_conf/1,
	 start/0, 
	 stop/1
	 ]).

-export([
	 handler/1
	]).  


%% ----

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(Config) when is_list(Config) ->
    %% io:format("~w:init -> entry with"
    %% 	      "~n   Config: ~p"
    %% 	      "~n", [?MODULE, Config]),
    Flag = process_flag(trap_exit, true),    
    Res = (catch start()),
    %% io:format("~w:init -> start result"
    %% 	      "~n   Res: ~p"
    %% 	      "~n", [?MODULE, Res]),
    process_flag(trap_exit, Flag),
    case Res of
	{error, Reason} ->
	    skip(Reason);
	{ok, FlexConfig} ->
	    [{flex_scanner, FlexConfig} | Config]
    end.
    

finish(Config) when is_list(Config) ->
    case lists:keysearch(flex_scanner, 1, Config) of
	{value, {flex_scanner, {Pid, _Conf}}} ->
	    stop(Pid),
	    lists:keydelete(flex_scanner, 1, Config);
	false ->
	    Config
    end.
    

start() ->
    Pid = proc_lib:spawn(?MODULE, handler, [self()]),
    receive
	{flex_scanner_started, Pid, Conf} ->
	    {ok, {Pid, Conf}};
	{flex_scanner_error, {failed_loading_flex_scanner_driver, Reason}} ->
 	    ?LOG("start_flex_scanner -> failed loading flex scanner driver: "
 		 "~n   Reason: ~p~n", [Reason]),
	    {error, {failed_loading_flex_scanner_driver, Reason}};
	{flex_scanner_error, Reason} ->
 	    ?LOG("start_flex_scanner -> error: "
 		 "~n   Reason: ~p~n", [Reason]),
	    {error, {failed_loading_flex_scanner_driver, Reason}}
    after 10000 ->
	    exit(Pid, kill),
	    {error, {failed_starting_flex_scanner, timeout}}
    end.


scanner_conf(Config) when is_list(Config) ->
    case lists:keysearch(flex_scanner, 1, Config) of
	{value, {flex_scanner, {Pid, Conf}}} ->
	    case ping_flex_scanner(Pid) of
		ok ->
		    Conf;
		Else ->
		    skip({no_response_from_flex_scanner_handler, Else})
	    end;
	false ->
	    skip("Flex scanner driver not loaded")
    end.


ping_flex_scanner(Pid) ->
    Pid ! {ping, self()},
    receive
	{pong, Pid} ->
	    ok
    after 5000 ->
	    timeout
    end.


stop(Pid) ->
    Pid ! stop.


handler(Pid) ->
    SMP = erlang:system_info(smp_support), 
    case (catch megaco_flex_scanner:start(SMP)) of
	{ok, PortOrPorts} ->
	    Pid ! {flex_scanner_started, self(), {flex, PortOrPorts}},
	    handler(Pid, PortOrPorts);
	{error, {load_driver, {open_error, Reason}}} ->
	    Error = {failed_loading_flex_scanner_driver, Reason},
	    Pid ! {flex_scanner_error, Error},
	    exit(Error);
	{error, {load_driver, Reason}} ->
	    Error = {failed_loading_flex_scanner_driver, Reason},
	    Pid ! {flex_scanner_error, Error},
	    exit(Error);
	Else ->
	    Error = {unknown_result_from_start_flex_scanner, Else},
	    Pid ! {flex_scanner_error, Error},
	    exit(Error)
    end.

handler(Pid, PortOrPorts) ->
    receive
	{ping, Pinger} ->
	    Pinger ! {pong, self()},
	    handler(Pid, PortOrPorts);
	{'EXIT', Port, Reason} when (PortOrPorts =:= Port) ->
	    Pid ! {flex_scanner_exit, Reason},
	    exit({flex_scanner_exit, Reason});
	{'EXIT', Port, Reason} when is_port(Port) ->
	    case megaco_flex_scanner:is_scanner_port(Port, PortOrPorts) of
		true ->
		    Pid ! {flex_scanner_exit, Reason},
		    exit({flex_scanner_exit, Reason});
		false ->
		    io:format("flex scanner handler got port exit "
			      "from unknown:"
			      "~n   ~p: ~p", [Port, Reason]),
		    ok
	    end,
	    handler(Pid, PortOrPorts);
	stop ->
	    megaco_flex_scanner:stop(PortOrPorts),
	    exit(normal);
	Other ->
	    io:format("flex scanner handler got something:~n"
		      "~p", [Other]),
	    handler(Pid, PortOrPorts)
    end.
	    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

skip(Reason) ->
    megaco_codec_test_lib:skip(Reason).

