%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2016. All Rights Reserved.
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
-module(math_SUITE).

-include_lib("common_test/include/ct.hrl").

%% Test server specific exports
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1,
	 init_per_group/2,end_per_group/2]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% Test cases must be exported.

-export([floor_ceil/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() ->
    [floor_ceil].

groups() -> [].

init_per_suite(Config) -> Config.
end_per_suite(_Config) -> ok.

init_per_group(_GroupName, Config) -> Config.
end_per_group(_GroupName, Config) -> Config.

init_per_testcase(_Case, Config) -> Config.
end_per_testcase(_Case, _Config) -> ok.

floor_ceil(Config) when is_list(Config) ->
    MinusZero = 0.0/(-1.0),
    Mod = math,
    1  = Mod:floor(1),
    -1 = Mod:floor(-1),
    0  = Mod:floor(0),
    0  = Mod:floor(0.0),
    0  = Mod:floor(MinusZero),
    -0 = Mod:floor(MinusZero),

    10  = Mod:floor(10.0),
    -10 = Mod:floor(-10.0),
    -11 = Mod:floor(-10.1),
    10.0  = Mod:floorf(10.1),
    -11.0 = Mod:floorf(-10.1),

    1  = Mod:ceil(1),
    -1 = Mod:ceil(-1),
    0  = Mod:ceil(0),
    0  = Mod:ceil(0.0),
    0  = Mod:ceil(MinusZero),
    -0 = Mod:ceil(MinusZero),

    10  = Mod:ceil(10.0),
    -10 = Mod:ceil(-10.0),
    11  = Mod:ceil(10.1),
    -10 = Mod:ceil(-10.1),
    11.0  = Mod:ceilf(10.1),
    -10.0 = Mod:ceilf(-10.1),
    ok.
