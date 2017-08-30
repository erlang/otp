%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2016. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% Purpose: This is a simple logger utility for the HDLT toolkit.
%%          It assumesd that the debug level and the "name" of the 
%%          logging entity has been put in process environment
%%          (using the set_level and set_name functions respectively).
%%----------------------------------------------------------------------

%%

-module(hdlt_logger).

-export([
	 start/0,
	 set_level/1, get_level/0, set_name/1,
	 info/2, log/2, debug/2
	]).

-export([logger/1]).

-define(LOGGER,    ?MODULE).
-define(MSG,       hdlt_logger_msg).
-define(LEVEL,     hdlt_logger_level).
-define(NAME,      hdlt_logger_name).
-define(INFO_STR,  "INFO").
-define(LOG_STR,   "LOG ").
-define(DEBUG_STR, "DBG ").


start() ->
    Self = self(), 
    proc_lib:start(?MODULE, logger, [Self]).

set_name(Name) when is_list(Name) ->
    put(?NAME, Name),
    ok.

get_level() ->
    get(?LEVEL).

set_level(Level) ->
    case lists:member(Level, [silence, info, log, debug]) of
	true ->
	    put(?LEVEL, Level),
	    ok;
	false ->
	    erlang:error({bad_debug_level, Level})
    end.


info(F, A) ->
%%     io:format("info -> " ++ F ++ "~n", A),
    do_log(info, get(?LEVEL), F, A).

log(F, A) ->
%%     io:format("log -> " ++ F ++ "~n", A),
    do_log(log, get(?LEVEL), F, A).

debug(F, A) ->
%%     io:format("debug -> " ++ F ++ "~n", A),
    do_log(debug, get(?LEVEL), F, A).


logger(Parent) ->
    global:register_name(?LOGGER, self()),
    Ref = erlang:monitor(process, Parent),
    proc_lib:init_ack(self()),
    logger_loop(Ref).

logger_loop(Ref) ->
    receive
	{?MSG, F, A} ->
	    io:format(F, A),
	    logger_loop(Ref);
	{'DOWN', Ref, process, _Object, _Info} ->
	    %% start the stop timer
	    erlang:send_after(timer:seconds(5), self(), stop),
	    logger_loop(undefined);
	stop ->
	    global:unregister_name(?LOGGER),
	    ok
    end.


formated_timestamp() ->
    {Date, Time}   = erlang:localtime(),
    {YYYY,MM,DD}   = Date,
    {Hour,Min,Sec} = Time,
    FormatDate =
        io_lib:format("~.4w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w",
                      [YYYY,MM,DD,Hour,Min,Sec]),
    lists:flatten(FormatDate).

do_log(_, silence, _, _) ->
    ok;
do_log(info, info, F, A) ->
    do_log(?INFO_STR, F, A);
do_log(info, log, F, A) ->
    do_log(?INFO_STR, F, A);
do_log(log, log, F, A) ->
    do_log(?LOG_STR, F, A);
do_log(info, debug, F, A) ->
    do_log(?INFO_STR, F, A);
do_log(log, debug, F, A) ->
    do_log(?LOG_STR, F, A);
do_log(debug, debug, F, A) ->
    do_log(?DEBUG_STR, F, A);
do_log(_, _, _F, _A) ->
    ok.

do_log(SEV, F, A) ->
    Name = 
	case get(?NAME) of
	    L when is_list(L) ->
		L;
	    _ ->
		"UNDEFINED"
	end,
    Msg = {?MSG, "~s ~s [~s] " ++ F ++ "~n", 
	   [SEV, Name, formated_timestamp() | A]}, 
    (catch global:send(?LOGGER, Msg)).
