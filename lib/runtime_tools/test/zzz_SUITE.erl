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
-module(zzz_SUITE).

%% The sole purpose of this test suite is for things we want to run last
%% before the VM terminates.

-export([all/0]).

-export([lc_graph/1]).


all() ->
    [lc_graph].

lc_graph(_Config) ->
    %% Create "lc_graph" file in current working dir
    %% if lock checker is enabled.
    erts_debug:lc_graph(),
    ok.
