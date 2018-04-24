%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2017. All Rights Reserved.
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
-define(LOGGER_KEY,'$logger_config$').
-define(HANDLER_KEY,'$handler_config$').
-define(LOGGER_META_KEY,'$logger_metadata$').
-define(STANDARD_HANDLER, logger_std_h).
-define(DEFAULT_LOGGER_CALL_TIMEOUT, 10000).
-define(DEFAULT_HANDLER_FILTERS,
        ?DEFAULT_HANDLER_FILTERS([beam,erlang,otp])).
-define(DEFAULT_HANDLER_FILTERS(Domain),
        [{remote_gl,{fun logger_filters:remote_gl/2,stop}},
         {domain,{fun logger_filters:domain/2,{log,prefix_of,Domain}}},
         {no_domain,{fun logger_filters:domain/2,{log,no_domain,[]}}}]).
-define(DEFAULT_FORMATTER,logger_formatter).
-define(DEFAULT_FORMAT_CONFIG,#{legacy_header=>true,
                                template=>?DEFAULT_FORMAT_TEMPLATE_HEADER}).
-define(DEFAULT_FORMAT_TEMPLATE_HEADER,
        [{logger_formatter,header},"\n",msg,"\n"]).
-define(DEFAULT_FORMAT_TEMPLATE_SINGLE,
        [time," ",level,": ",msg,"\n"]).
-define(DEFAULT_FORMAT_TEMPLATE,
        [time," ",level,":\n",msg,"\n"]).

-define(LOG_INTERNAL(Level,Report),
        case logger:allow(Level,?MODULE) of
            true ->
                _ = spawn(logger,macro_log,[?LOCATION,Level,Report,
                                            logger:add_default_metadata(#{})]),
                ok;
            false ->
                ok
        end).

%%%-----------------------------------------------------------------
%%% Levels
%%% Using same as syslog
-define(LEVELS,[emergency,
                alert,
                critical,
                error,
                warning,
                notice,
                info,
                debug]).
-define(EMERGENCY,0).
-define(ALERT,1).
-define(CRITICAL,2).
-define(ERROR,3).
-define(WARNING,4).
-define(NOTICE,5).
-define(INFO,6).
-define(DEBUG,7).

-define(IS_LEVEL(L),
        (L=:=emergency orelse
            L=:=alert orelse
            L=:=critical orelse
            L=:=error orelse
            L=:=warning orelse
            L=:=notice orelse
            L=:=info orelse
            L=:=debug)).

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
