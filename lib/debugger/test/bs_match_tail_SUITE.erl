%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2000-2016. All Rights Reserved.
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

-module(bs_match_tail_SUITE).

-author('bjorn@erix.ericsson.se').
-export([all/0, suite/0,groups/0,init_per_group/2,end_per_group/2,
	 init_per_testcase/2,end_per_testcase/2,
	 init_per_suite/1,end_per_suite/1,
	 aligned/1,unaligned/1,zero_tail/1]).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() -> 
    cases().

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


cases() -> 
    [aligned, unaligned, zero_tail].

init_per_testcase(_Case, Config) ->
    test_lib:interpret(?MODULE),
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

init_per_suite(Config) when is_list(Config) ->
    test_lib:interpret(?MODULE),
    true = lists:member(?MODULE, int:interpreted()),
    Config.

end_per_suite(Config) when is_list(Config) ->
    ok.

%% Test aligned tails.
aligned(Config) when is_list(Config) ->
    Tail1 = mkbin([]),
    {258,Tail1} = al_get_tail_used(mkbin([1,2])),
    Tail2 = mkbin(lists:seq(1, 127)),
    {35091,Tail2} = al_get_tail_used(mkbin([137,19|Tail2])),

    64896 = al_get_tail_unused(mkbin([253,128])),
    64895 = al_get_tail_unused(mkbin([253,127|lists:seq(42, 255)])),

    Tail3 = mkbin(lists:seq(0, 19)),
    {0,Tail1} = get_dyn_tail_used(Tail1, 0),
    {0,Tail3} = get_dyn_tail_used(mkbin([Tail3]), 0),
    {73,Tail3} = get_dyn_tail_used(mkbin([73|Tail3]), 8),

    0 = get_dyn_tail_unused(mkbin([]), 0),
    233 = get_dyn_tail_unused(mkbin([233]), 8),
    23 = get_dyn_tail_unused(mkbin([23,22,2]), 8),
    ok.

al_get_tail_used(<<A:16,T/binary>>) -> {A,T}.
al_get_tail_unused(<<A:16,_/binary>>) -> A.

%% Test that an non-aligned tail cannot be matched out.
unaligned(Config) when is_list(Config) ->
    {'EXIT',{function_clause,_}} = (catch get_tail_used(mkbin([42]))),
    {'EXIT',{{badmatch,_},_}} = (catch get_dyn_tail_used(mkbin([137]), 3)),
    {'EXIT',{function_clause,_}} = (catch get_tail_unused(mkbin([42,33]))),
    {'EXIT',{{badmatch,_},_}} = (catch get_dyn_tail_unused(mkbin([44]), 7)),
    ok.

get_tail_used(<<A:1,T/binary>>) -> {A,T}.

get_tail_unused(<<A:15,_/binary>>) -> A.

get_dyn_tail_used(Bin, Sz) ->
    <<A:Sz,T/binary>> = Bin,
    {A,T}.

get_dyn_tail_unused(Bin, Sz) ->
    <<A:Sz,_/binary>> = Bin,
    A.

%% Test that zero tails are tested correctly.
zero_tail(Config) when is_list(Config) ->
    7 = (catch test_zero_tail(mkbin([7]))),
    {'EXIT',{function_clause,_}} = (catch test_zero_tail(mkbin([1,2]))),
    {'EXIT',{function_clause,_}} = (catch test_zero_tail2(mkbin([1,2,3]))),
    ok.

test_zero_tail(<<A:8>>) -> A.

test_zero_tail2(<<_A:4,_B:4>>) -> ok.

mkbin(L) when is_list(L) -> list_to_binary(L).
