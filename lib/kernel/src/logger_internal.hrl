%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2017-2018. All Rights Reserved.
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
-include_lib("kernel/include/logger.hrl").
-define(LOGGER_TABLE,logger).
-define(PROXY_KEY,'$proxy_config$').
-define(PRIMARY_KEY,'$primary_config$').
-define(HANDLER_KEY,'$handler_config$').
-define(LOGGER_META_KEY,'$logger_metadata$').
-define(STANDARD_HANDLER, default).
-define(DEFAULT_HANDLER_FILTERS,?DEFAULT_HANDLER_FILTERS([otp])).
-define(DEFAULT_HANDLER_FILTERS(Domain),
        [{remote_gl,{fun logger_filters:remote_gl/2,stop}},
         {domain,{fun logger_filters:domain/2,{log,super,Domain}}},
         {no_domain,{fun logger_filters:domain/2,{log,undefined,[]}}}]).
-define(DEFAULT_FORMATTER,logger_formatter).
-define(DEFAULT_FORMAT_CONFIG,#{legacy_header=>true,
                                single_line=>false}).
-define(DEFAULT_FORMAT_TEMPLATE_HEADER,
        [[logger_formatter,header],"\n",msg,"\n"]).
-define(DEFAULT_FORMAT_TEMPLATE_SINGLE,
        [time," ",level,": ",msg,"\n"]).
-define(DEFAULT_FORMAT_TEMPLATE,
        [time," ",level,":\n",msg,"\n"]).

-define(DEFAULT_LOGGER_CALL_TIMEOUT, infinity).

-define(LOG_INTERNAL(Level,Report),?DO_LOG_INTERNAL(Level,[Report])).
-define(LOG_INTERNAL(Level,Format,Args),?DO_LOG_INTERNAL(Level,[Format,Args])).
-define(DO_LOG_INTERNAL(Level,Data),
        case logger:allow(Level,?MODULE) of
            true ->
                %% Spawn this to avoid deadlocks
                _ = spawn(logger,macro_log,[?LOCATION,Level|Data]++
                              [logger:add_default_metadata(#{})]),
                ok;
            false ->
                ok
        end).

%%%-----------------------------------------------------------------
%%% Levels
%%% Using same as syslog
-define(LEVELS,[none,
                emergency,
                alert,
                critical,
                error,
                warning,
                notice,
                info,
                debug,
                all]).
-define(LOG_NONE,-1).
-define(EMERGENCY,0).
-define(ALERT,1).
-define(CRITICAL,2).
-define(ERROR,3).
-define(WARNING,4).
-define(NOTICE,5).
-define(INFO,6).
-define(DEBUG,7).
-define(LOG_ALL,10).

-define(IS_LEVEL(L),
        (L=:=emergency orelse
            L=:=alert orelse
            L=:=critical orelse
            L=:=error orelse
            L=:=warning orelse
            L=:=notice orelse
            L=:=info orelse
            L=:=debug )).

-define(IS_LEVEL_ALL(L),
        ?IS_LEVEL(L) orelse
            L=:=all orelse
			L=:=none ).

-define(IS_MSG(Msg),
        ((is_tuple(Msg) andalso tuple_size(Msg)==2)
         andalso
           (is_list(element(1,Msg)) andalso is_list(element(2,Msg)))
         orelse
           (element(1,Msg)==report andalso ?IS_REPORT(element(2,Msg)))
         orelse
           (element(1,Msg)==string andalso ?IS_STRING(element(2,Msg))))).

-define(IS_REPORT(Report),
        (is_map(Report) orelse (is_list(Report) andalso is_tuple(hd(Report))))).

-define(IS_STRING(String),
        (is_list(String) orelse is_binary(String))).
