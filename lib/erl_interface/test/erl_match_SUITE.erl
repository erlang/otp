%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2018. All Rights Reserved.
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

%%
-module(erl_match_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("erl_match_SUITE_data/match_test_cases.hrl").

-export([all/0, suite/0,
         init_per_testcase/2,
         atoms/1, lists/1, tuples/1, references/1, pids/1, ports/1,
         bind/1, integers/1, floats/1, binaries/1, strings/1]).

%% For interactive running of matcher.
-export([start_matcher/1, erl_match/3]).

%% This test suite tests the erl_match() function.

suite() ->
    [{ct_hooks,[ts_install_cth]}].

all() -> 
    [atoms, lists, tuples, references, pids, ports, bind,
     integers, floats, binaries, strings].

init_per_testcase(Case, Config) ->
    runner:init_per_testcase(?MODULE, Case, Config).

atoms(Config) when is_list(Config) ->
    P = start_matcher(Config),

    eq(P, '', ''),
    eq(P, a, a),
    ne(P, a, b),
    ne(P, a, aa),
    eq(P, kalle, kalle),
    ne(P, kalle, arne),

    ne(P, kalle, 42),
    ne(P, 42, kalle),

    runner:finish(P),
    ok.

lists(Config) when is_list(Config) ->
    P = start_matcher(Config),
    eq(P, [], []),

    ne(P, [], [a]),
    ne(P, [a], []),

    eq(P, [a], [a]),
    ne(P, [a], [b]),

    eq(P, [a|b], [a|b]),
    ne(P, [a|b], [a|x]),

    eq(P, [a, b], [a, b]),
    ne(P, [a, b], [a, x]),

    eq(P, [a, b, c], [a, b, c]),
    ne(P, [a, b|c], [a, b|x]),
    ne(P, [a, b, c], [a, b, x]),
    ne(P, [a, b|c], [a, b|x]),
    ne(P, [a, x|c], [a, b|c]),
    ne(P, [a, b, c], [a, x, c]),

    runner:finish(P),
    ok.

tuples(Config) when is_list(Config) ->
    P = start_matcher(Config),

    ne(P, {}, {a, b}),
    ne(P, {a, b}, {}),
    ne(P, {a}, {a, b}),
    ne(P, {a, b}, {a}),

    eq(P, {}, {}),

    eq(P, {a}, {a}),
    ne(P, {a}, {b}),

    eq(P, {1}, {1}),
    ne(P, {1}, {2}),

    eq(P, {a, b}, {a, b}),
    ne(P, {x, b}, {a, b}),

    ne(P, {error, x}, {error, y}),
    ne(P, {error, {undefined, {subscriber, last}}},
       {error, {undefined, {subscriber, name}}}),

    runner:finish(P),
    ok.


references(Config) when is_list(Config) ->
    P = start_matcher(Config),
    Ref1 = make_ref(),
    Ref2 = make_ref(),

    eq(P, Ref1, Ref1),
    eq(P, Ref2, Ref2),
    ne(P, Ref1, Ref2),
    ne(P, Ref2, Ref1),

    runner:finish(P),
    ok.


pids(Config) when is_list(Config) ->
    P = start_matcher(Config),
    Pid1 = c:pid(0,1,2),
    Pid2 = c:pid(0,1,3),

    eq(P, self(), self()),
    eq(P, Pid1, Pid1),
    ne(P, Pid1, self()),
    ne(P, Pid2, Pid1),

    runner:finish(P),
    ok.


ports(Config) when is_list(Config) ->
    case os:type() of
        vxworks ->
            {skipped,"not on vxworks, pucko"};
        _ ->
            P = start_matcher(Config),
            P2 = start_matcher(Config),

            eq(P, P, P),
            ne(P, P, P2),

            runner:finish(P),
            runner:finish(P2),
            ok
    end.

integers(Config) when is_list(Config) ->
    P = start_matcher(Config),
    I1 = 123,
    I2 = 12345,
    I3 = -123,
    I4 = 2234,

    eq(P, I1, I1),
    eq(P, I2, I2),
    ne(P, I1, I2),
    ne(P, I1, I3),
    eq(P, I4, I4),

    runner:finish(P),
    ok.



floats(Config) when is_list(Config) ->
    P = start_matcher(Config),
    F1 = 3.1414,
    F2 = 3.1415,
    F3 = 3.1416,

    S1 = "string",
    S2 = "string2",

    eq(P, F1, F1),
    eq(P, F2, F2),
    ne(P, F1, F2),
    ne(P, F3, F2),

    eq(P, S2, S2),
    ne(P, S1, S2),

    runner:finish(P),
    ok.



binaries(Config) when is_list(Config) ->
    P = start_matcher(Config),
    Bin1 = term_to_binary({kalle, 146015, {kungsgatan, 23}}),
    Bin2 = term_to_binary(sune),
    Bin3 = list_to_binary("sune"),

    eq(P, Bin1, Bin1),
    eq(P, Bin2, Bin2),
    eq(P, Bin3, Bin3),
    ne(P, Bin1, Bin2),
    ne(P, Bin1, Bin3),
    ne(P, Bin2, Bin3),

    runner:finish(P),
    ok.


strings(Config) when is_list(Config) ->
    P = start_matcher(Config),

    S1 = "string",
    S2 = "streng",
    S3 = "String",

    eq(P, S1, S1),
    ne(P, S1, S2),
    ne(P, S1, S3),

    runner:finish(P),
    ok.


bind(Config) when is_list(Config) ->
    P = start_bind(Config),
    S = "[X,Y,Z]",
    L1 = [301,302,302],
    L2 = [65,66,67],

    bind_ok(P, S, L1),
    bind_ok(P, S, L2),

    runner:finish(P),
    ok.

start_bind(Config) ->
    runner:start(Config, ?erl_match_bind).

bind_ok(Port, Bind, Term) ->
    true = erl_bind(Port, Bind, Term).

%bind_nok(Port, Bind, Term) ->
%    false = erl_bind(Port, Bind, Term).

erl_bind(Port, Pattern, Term) ->
    Port ! {self(), {command, [$b, Pattern, 0]}},
    runner:send_term(Port, Term),
    case runner:get_term(Port) of
        {term, 0} -> false;
        {term, 1} -> true
    end.



start_matcher(Config) ->
    runner:start(Config, ?erl_match_server).

eq(Port, Pattern, Term) ->
    true = erl_match(Port, Pattern, Term).

ne(Port, Pattern, Term) ->
    false = erl_match(Port, Pattern, Term).



erl_match(Port, Pattern, Term) ->
    runner:send_term(Port, Pattern),
    runner:send_term(Port, Term),
    case runner:get_term(Port) of
        {term, 0} -> false;
        {term, 1} -> true
    end.
