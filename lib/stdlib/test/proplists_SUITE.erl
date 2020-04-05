%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2019. All Rights Reserved.
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

-module(proplists_SUITE).

-export([all/0, suite/0,groups/0, init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
	 init_per_testcase/2, end_per_testcase/2,
         examples/1]).

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,5}}].

all() ->
    [examples].

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

%% Test all examples in the documentation.

examples(_Config) ->
    [1,2,3,4] = proplists:append_values(a, [{a, [1,2]}, {b, 0}, {a, 3}, {c, -1}, {a, [4]}]),

    ExpandRes = [fie, bar, baz, fum],
    ExpandRes = proplists:expand([{foo, [bar, baz]}], [fie, foo, fum]),
    ExpandRes = proplists:expand([{{foo, true}, [bar, baz]}], [fie, foo, fum]),
    ExpandRes = proplists:expand([{{foo, false}, [bar, baz]}], [fie, {foo, false}, fum]),

    [{foo, false}, fie, foo, fum] = proplists:expand([{{foo, true}, [bar, baz]}],
                                                     [{foo, false}, fie, foo, fum]),

    {[[a], [{b, 5}, b],[{c, 2}, {c, 3, 4}]], [{e, 1}, d]} =
        proplists:split([{c, 2}, {e, 1}, a, {c, 3, 4}, d, {b, 5}, b], [a, b, c]),

    ColorList = [{color, red}, {colour, green}, color, colour],
    ColorListRes = [{colour, red}, {colour, green}, colour, colour],
    ColorListRes = proplists:substitute_aliases([{color, colour}], ColorList),

    NegList = [no_foo, {no_foo, true}, {no_foo, false}, {no_foo, any}, foo],
    NegListRes = [{foo, false}, {foo, false}, foo, foo, foo],
    NegListRes = proplists:substitute_negations([{no_foo, foo}], NegList),

    ok.
