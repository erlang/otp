%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2025. All Rights Reserved.
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

-module(group_SUITE).

-export([all/0,
        suite/0,
        init_per_suite/1,
        end_per_suite/1]).

-export([check_unknown_message/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{seconds,30}}].

all() -> 
    [check_unknown_message].

init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

check_unknown_message(_Config) ->
    Drv = spawn_link(fun() -> timer:sleep(infinity) end),
    Group = group:start(Drv),
    try
        Group ! unknown,
        timer:sleep(100),
        true = is_process_alive(Group)
    after
        ok = gen_statem:stop(Group)
    end.

