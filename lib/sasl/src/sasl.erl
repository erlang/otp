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
-module(sasl).

%% External exports
-export([start/2, stop/1]).

%% Internal exports
-export([init/1, pred/1]).

%%%-----------------------------------------------------------------
%%% This module implements the application SASL,
%%% and a supervisor for SASL.
%%%-----------------------------------------------------------------
-behaviour(application).

-record(state, {sasl_error_logger, error_logger_mf}).

start(_, []) ->
    Handler = get_sasl_error_logger(),
    Type = get_sasl_error_logger_type(),
    Mf = get_error_logger_mf(),
    add_sasl_error_logger(Handler, Type),
    add_error_logger_mf(Mf),
    State = #state{sasl_error_logger = Handler, error_logger_mf = Mf}, 
    case supervisor:start_link({local, sasl_sup}, sasl, []) of
	{ok, Pid} -> {ok, Pid, State};
	Error -> Error
    end.

stop(State) ->
    delete_sasl_error_logger(State#state.sasl_error_logger),
    delete_error_logger_mf(State#state.error_logger_mf).

%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------
get_sasl_error_logger() ->
    case application:get_env(sasl, sasl_error_logger) of
	{ok, false} -> undefined;
	{ok, tty} -> tty;
	{ok, {file, File}} when is_list(File) -> {file, File, [write]};
	{ok, {file, File, Modes}} when is_list(File), is_list(Modes) ->
        {file, File, Modes};
	{ok, Bad} -> exit({bad_config, {sasl, {sasl_error_logger, Bad}}});
	_ -> undefined
    end.

get_sasl_error_logger_type() ->
    case application:get_env(sasl, errlog_type) of
	{ok, error} -> error;
	{ok, progress} -> progress;
	{ok, all} -> all;
	{ok, Bad} -> exit({bad_config, {sasl, {errlog_type, Bad}}});
	_ -> all
    end.

get_error_logger_mf() ->
    case catch get_mf() of
	{'EXIT', Reason} ->
	    exit(Reason);
	Mf ->
	    Mf
    end.

get_mf() -> 
    Dir = get_mf_dir(),
    MaxB = get_mf_maxb(),
    MaxF = get_mf_maxf(),
    case {Dir, MaxB, MaxF} of
	{undefined,undefined,undefined} ->
	    undefined;
	{undefined,_,_} ->
	    exit({missing_config, {sasl, error_logger_mf_dir}});
	{_,undefined,_} ->
	    exit({missing_config, {sasl, error_logger_mf_maxbytes}});
	{_,_,undefined} ->
	    exit({missing_config, {sasl, error_logger_mf_maxfiles}});
	R ->
	    R
    end.

get_mf_dir() ->
    case application:get_env(sasl, error_logger_mf_dir) of
	{ok, false} -> undefined;
	{ok, Dir} when is_list(Dir) -> Dir;
	undefined -> undefined;
	{ok, Bad} -> exit({bad_config, {sasl, {error_logger_mf_dir, Bad}}})
    end.

get_mf_maxb() ->
    case application:get_env(sasl, error_logger_mf_maxbytes) of
	{ok, MaxB} when is_integer(MaxB) -> MaxB;
	undefined -> undefined;
	{ok, Bad} -> exit({bad_config, {sasl, {error_logger_mf_maxbytes, Bad}}})
    end.

get_mf_maxf() ->
    case application:get_env(sasl, error_logger_mf_maxfiles) of
	{ok, MaxF} when is_integer(MaxF), MaxF > 0, MaxF < 256 -> MaxF;
	undefined -> undefined;
	{ok, Bad} -> exit({bad_config, {sasl, {error_logger_mf_maxfiles, Bad}}})
    end.

add_sasl_error_logger(undefined, _Type) -> ok;
add_sasl_error_logger(Handler, Type) ->
    error_logger:add_report_handler(mod(Handler), args(Handler, Type)).

delete_sasl_error_logger(undefined) -> ok;
delete_sasl_error_logger(Type) ->
    error_logger:delete_report_handler(mod(Type)).

mod(tty) -> sasl_report_tty_h;
mod({file, _File, _Modes}) -> sasl_report_file_h.

args({file, File, Modes}, Type) -> {File, Modes, type(Type)};
args(_, Type) -> type(Type).

type(error) -> error;
type(progress) -> progress;
type(_) -> all.

add_error_logger_mf(undefined) -> ok;
add_error_logger_mf({Dir, MaxB, MaxF}) ->
    error_logger:add_report_handler(
      log_mf_h, log_mf_h:init(Dir, MaxB, MaxF, fun pred/1)).

delete_error_logger_mf(undefined) -> ok;
delete_error_logger_mf(_) ->
    error_logger:delete_report_handler(log_mf_h).

pred({_Type, GL, _Msg}) when node(GL) =/= node() -> false;
pred(_) -> true.

%%%-----------------------------------------------------------------
%%% supervisor functionality
%%%-----------------------------------------------------------------
init([]) ->
    SupFlags = {one_for_one, 0, 1},
    %% Reboot node if release_handler crashes!
    SafeSupervisor = {sasl_safe_sup,
		      {supervisor, start_link,
		       [{local, sasl_safe_sup}, ?MODULE, safe]},
		      permanent, infinity, supervisor, [?MODULE]},
    ReleaseH = {release_handler,
		{release_handler, start_link, []},
		permanent, 2000, worker, []},	% Note! [] for modules!  We
						% can't change code on r_h
						% this way!!
    {ok, {SupFlags, [SafeSupervisor, ReleaseH]}};
init(safe) ->
    SupFlags = {one_for_one, 4, 3600},
    AlarmH = {alarm_handler,
	      {alarm_handler, start_link, []},
	      permanent, 2000, worker, dynamic},	      
    {ok, {SupFlags, [AlarmH]}}.
