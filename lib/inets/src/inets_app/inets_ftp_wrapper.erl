%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2006-2018. All Rights Reserved.
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
-module(inets_ftp_wrapper).


-export([start_standalone/1,
         start_service/1,
	 stop_service/1,
	 services/0,
         service_info/1]).


start_standalone(Options) ->
    ftp:start_standalone(Options).


start_service(Options) ->
    application:ensure_started(ftp),
    ftp:start_service(Options).


stop_service(Pid) ->
    ftp:stop_service(Pid).


services() ->
    [].


service_info(_) ->
    [].
