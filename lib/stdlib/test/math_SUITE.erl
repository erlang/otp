%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2025. All Rights Reserved.
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
-export([floor_ceil/1, error_info/1, constants/1, doctests/1]).


suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() ->
    [floor_ceil, error_info, constants, doctests].

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

constants(_Config) ->
    3.1415926535897932 = math:pi(),
    6.2831853071795864 = math:tau(),
    ok.

floor_ceil(_Config) ->
    MinusZero = 0.0/(-1.0),
    -43.0 = do_floor_ceil(-42.1),
    -43.0 = do_floor_ceil(-42.7),
    true = (0.0 == do_floor_ceil(MinusZero)),
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

error_info(_Config) ->
    L0 = [{acosh, [a]},
          {acosh, [0.5]},

          {asinh, [a]},

          {atanh, [a]},
          {atanh, [10]},
          {atanh, [1]},

          {cosh, [a]},
          {sinh, [a]},
          {tanh, [a]},

          %% Trigonmetric functions.
          {acos, [a]},
          {acos, [999]},

          {asin, [a]},
          {asin, [999]},

          {atan, [a]},
          {atan2, [a, b],[{1,".*"},{2,".*"}]},

          {cos, [a]},
          {sin, [a]},
          {tan, [a]},

          %% Logarithms.

          {log, [a]},
          {log, [-1]},

          {log10, [a]},
          {log10, [-1]},

          {log2, [a]},
          {log2, [-1]},

          %% The others.
          {ceil, [a]},
          {erf, [a]},
          {erfc, [a]},
          {exp, [a]},
          {floor, [a]},

          {fmod, [a, b],[{1,".*"},{2,".*"}]},
          {fmod, [a, 1]},
          {fmod, [1, 0]},

          {pow, [a, b],[{1,".*"},{2,".*"}]},

          {sqrt, [a]},
          {sqrt, [-1]},

          %% Intentionally unexplained errors. It is difficult to explain
          %% a range error in a sensible way because neither argument by itself
          %% is guilty.
          {pow, [2.0, 10000000], [unexplained]}
         ],
    L = ignore_undefined(L0),
    error_info_lib:test_error_info(math, L).

ignore_undefined([H|T]) ->
    Name = element(1, H),
    Args = element(2, H),
    try apply(math, Name, Args) of
        _ ->
            [H|ignore_undefined(T)]
    catch
        _:undef ->
            %% This math BIF is not implemented on this platform.
            %% Ignore it.
            [{Name, length(Args)}|ignore_undefined(T)];
        _:_ ->
            [H|ignore_undefined(T)]
    end;
ignore_undefined([]) ->
    [].

doctests(_Config) ->
    shell_docs:test(math, []).
