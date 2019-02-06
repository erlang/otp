%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2016. All Rights Reserved.
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
	 init_per_group/2,end_per_group/2,
	 init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([floor_ceil/1]).


suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() ->
    [floor_ceil].

groups() ->
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

floor_ceil(_Config) ->
    MinusZero = 0.0/(-1.0),
    -43.0 = do_floor_ceil(-42.1),
    -43.0 = do_floor_ceil(-42.7),
    0.0 = do_floor_ceil(MinusZero),
    10.0 = do_floor_ceil(10.1),
    10.0 = do_floor_ceil(10.9),

    -533.0 = do_floor_ceil(-533.0),
    453555.0 = do_floor_ceil(453555.0),

    -58.0 = do_floor_ceil(-58),
    777.0 = do_floor_ceil(777),

    ok.

do_floor_ceil(Val) ->
    Floor = math:floor(Val),
    Ceil = math:ceil(Val),

    true = is_float(Floor),
    true = is_float(Ceil),

    if
	Floor =:= Ceil ->
	    Floor;
	true ->
	    1.0 = Ceil - Floor,
	    Floor
    end.
