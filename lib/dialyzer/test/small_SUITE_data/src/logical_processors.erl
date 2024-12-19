%%
%% %CopyrightBegin%
%%
%% Copyright 2024 Ildar Khizbulin <khizbulin@erlyvideo.org>
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

-module(logical_processors).

-export([t0/0,t1/0,t2/0]).

t0() ->
      unknown = erlang:system_info(logical_processors),
      unknown = erlang:system_info(logical_processors_available),
      unknown = erlang:system_info(logical_processors_online),
      ok.

t1() ->
      ok = erlang:system_info(logical_processors_available).

t2() ->
      ok = erlang:system_info(logical_processors_online).

