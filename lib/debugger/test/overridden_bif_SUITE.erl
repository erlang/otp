%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2001-2016. All Rights Reserved.
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
-module(overridden_bif_SUITE).
-compile({no_auto_import,[is_reference/1,size/1]}).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1,
	 init_per_group/2,end_per_group/2,
	 init_per_testcase/2,end_per_testcase/2,
	 overridden_bif/1]).

-include_lib("common_test/include/ct.hrl").

%% Used by overridden_bif/1.
-import(gb_sets, [size/1]).
-import(test_lib, [binary/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() ->
    [overridden_bif].

groups() ->
    [].

init_per_suite(Config) ->
    test_lib:interpret(?MODULE),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


init_per_testcase(Case, Config) when is_atom(Case), is_list(Config) ->
    Config.

end_per_testcase(Case, Config) when is_atom(Case), is_list(Config) ->
    ok.

overridden_bif(_Config) ->
    L = [-3,-2,-1,0,1,2,3,4],
    [-3,0,3] = do_overridden_bif_1(L),
    [-2,0,2,4] = do_overridden_bif_2(L),
    [3] = do_overridden_bif_3(L),
    [2,4] = do_overridden_bif_4(L),

    Set = gb_sets:from_list(L),
    [Set] = do_overridden_bif_5([gb_sets:singleton(42),Set]),

    [100,0] = do_overridden_bif_6([100|L]),
    ok.

do_overridden_bif_1(L) ->
    [E || E <- L, is_reference(E)].

do_overridden_bif_2(L) ->
    [E || E <- L, port(E)].

do_overridden_bif_3(L) ->
    [E || E <- L, (is_reference(E) andalso E > 0)].

do_overridden_bif_4(L) ->
    [E || E <- L, (port(E) andalso E > 0)].

do_overridden_bif_5(L) ->
    [E || E <- L, size(E) > 1].

do_overridden_bif_6(L) ->
    [E || E <- L, binary(E)].

is_reference(N) ->
    N rem 3 =:= 0.

port(N) ->
    N rem 2 =:= 0.
