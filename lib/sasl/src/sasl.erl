%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2018. All Rights Reserved.
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

-record(state, {sasl_logger, error_logger_mf}).

start(_, []) ->
    {Dest,Level} = get_logger_info(),
    Mf = get_error_logger_mf(),
    add_sasl_logger(Dest, Level),
    add_error_logger_mf(Mf),
    State = #state{sasl_logger = Dest, error_logger_mf = Mf},
    case supervisor:start_link({local, sasl_sup}, sasl, []) of
	{ok, Pid} -> {ok, Pid, State};
	Error -> Error
    end.

stop(State) ->
    delete_sasl_logger(State#state.sasl_logger),
    delete_error_logger_mf(State#state.error_logger_mf).

%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------
get_logger_info() ->
    case application:get_env(kernel, logger_sasl_compatible) of
        {ok,true} ->
            {get_logger_dest(),get_logger_level()};
        _ ->
            {std,undefined}
    end.

get_logger_dest() ->
    case application:get_env(sasl, sasl_error_logger) of
        {ok, false} -> undefined;
        {ok, tty} -> standard_io;
        {ok, {file, File}} when is_list(File) -> {file, File};
        {ok, {file, File, Modes}} when is_list(File), is_list(Modes) ->
            {file, File, Modes};
        {ok, Bad} -> exit({bad_config, {sasl, {sasl_logger_dest, Bad}}});
        undefined -> standard_io
    end.

get_logger_level() ->
    case application:get_env(sasl, errlog_type) of
        {ok, error} -> error;
        {ok, progress} -> info;
        {ok, all} -> info;
        {ok, Bad} -> exit({bad_config, {sasl, {errlog_type, Bad}}});
        _ -> info
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

add_sasl_logger(undefined, _Level) -> ok;
add_sasl_logger(std, undefined) -> ok;
add_sasl_logger(Dest, Level) ->
    FC = #{legacy_header=>true,
           single_line=>false},
    case Level of
        info -> allow_progress();
        _ -> ok
    end,
    ok = logger:add_handler(sasl,logger_std_h,
                            #{level=>Level,
                              filter_default=>stop,
                              filters=>
                                  [{remote_gl,
                                    {fun logger_filters:remote_gl/2,stop}},
                                   {sasl_domain,
                                    {fun logger_filters:domain/2,
                                     {log,equal,[otp,sasl]}}}],
                              config=>#{type=>Dest},
                              formatter=>{logger_formatter,FC}}).

delete_sasl_logger(undefined) -> ok;
delete_sasl_logger(std) -> ok;
delete_sasl_logger(_Type) ->
    _ = logger:remove_handler(sasl),
    ok.

add_error_logger_mf(undefined) -> ok;
add_error_logger_mf({Dir, MaxB, MaxF}) ->
    allow_progress(),
    error_logger:add_report_handler(
      log_mf_h, log_mf_h:init(Dir, MaxB, MaxF, fun pred/1)).

delete_error_logger_mf(undefined) -> ok;
delete_error_logger_mf(_) ->
    error_logger:delete_report_handler(log_mf_h).

pred({_Type, GL, _Msg}) when node(GL) =/= node() -> false;
pred(_) -> true.

allow_progress() ->
    #{level:=PL} = logger:get_primary_config(),
    case logger:compare_levels(info,PL) of
        lt -> ok = logger:set_primary_config(level,info);
        _ -> ok
    end.

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
