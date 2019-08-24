%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1998-2016. All Rights Reserved.
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

%%% Purpose : Application master for SSL.

-module(ssl_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
    start_logger(),
    ssl_sup:start_link().

stop(_State) ->
    stop_logger(),
    ok.

%%
%% Description: Start SSL logger
start_logger() ->
    Config = #{level => debug,
               filter_default => stop,
               formatter => {ssl_logger, #{}}},
    Filter = {fun logger_filters:domain/2,{log,sub,[otp,ssl]}},
    logger:add_handler(ssl_handler, logger_std_h, Config),
    logger:add_handler_filter(ssl_handler, filter_non_ssl, Filter),
    logger:set_application_level(ssl, debug).

%%
%% Description: Stop SSL logger
stop_logger() ->
    logger:unset_application_level(ssl),
    logger:remove_handler(ssl_handler).
