%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2021. All Rights Reserved.
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
-module(ei_print_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("ei_print_SUITE_data/ei_print_test_cases.hrl").

-export([all/0, suite/0,
         init_per_testcase/2,
         atoms/1, tuples/1, lists/1, strings/1,
         maps/1, funs/1, binaries/1, bitstrings/1,
         integers/1]).

-import(runner, [get_term/1]).

%% This test suite test the ei_print() function.
%% It uses the port program "ei_format_test".

suite() ->
    [{ct_hooks,[ts_install_cth]}].

all() ->
    [atoms, tuples, lists, strings, maps, funs, binaries, bitstrings, integers].

init_per_testcase(Case, Config) ->
    runner:init_per_testcase(?MODULE, Case, Config).

%% Tests formatting various atoms.

atoms(Config) when is_list(Config) ->
    P = runner:start(Config, ?atoms),

    {term, "''"} = get_term(P),
    {term, "a"} = get_term(P),
    {term, "'A'"} = get_term(P),
    {term, "abc"} = get_term(P),
    {term, "'Abc'"} = get_term(P),
    {term, "ab@c"} = get_term(P),
    {term, "'The rain in Spain stays mainly in the plains'"} = get_term(P),

    {term, "a"} = get_term(P),
    {term, "ab"} = get_term(P),
    {term, "abc"} = get_term(P),
    {term, "ab@c"} = get_term(P),
    {term, "abcdefghijklmnopq"} = get_term(P),

    {term, "''"} = get_term(P),
    {term, "a"} = get_term(P),
    {term, "'A'"} = get_term(P),
    {term, "abc"} = get_term(P),
    {term, "'Abc'"} = get_term(P),
    {term, "ab@c"} = get_term(P),
    {term, "'The rain in Spain stays mainly in the plains'"} = get_term(P),

    {term, "a"} = get_term(P),
    {term, "ab"} = get_term(P),
    {term, "abc"} = get_term(P),
    {term, "ab@c"} = get_term(P),
    {term, "'   abcdefghijklmnopq   '"} = get_term(P),

    runner:recv_eot(P),
    ok.



%% Tests formatting various tuples

tuples(Config) when is_list(Config) ->
    P = runner:start(Config, ?tuples),

    {term, "{}"} = get_term(P),
    {term, "{a}"} = get_term(P),
    {term, "{a, b}"} = get_term(P),
    {term, "{a, b, c}"} = get_term(P),
    {term, "{1}"} = get_term(P),
    {term, "{[]}"} = get_term(P),
    {term, "{[], []}"} = get_term(P),
    {term, "{[], a, b, c}"} = get_term(P),
    {term, "{[], a, [], b, c}"} = get_term(P),
    {term, "{[], a, '', b, c}"} = get_term(P),

    runner:recv_eot(P),
    ok.



%% Tests formatting various lists

lists(Config) when is_list(Config) ->
    P = runner:start(Config, ?lists),

    {term, "[]"} = get_term(P),
    {term, "[a]"} = get_term(P),
    {term, "[a, b]"} = get_term(P),
    {term, "[a, b, c]"} = get_term(P),
    {term, "[1]"} = get_term(P),
    {term, "[[]]"} = get_term(P),
    {term, "[[], []]"} = get_term(P),
    {term, "[[], a, b, c]"} = get_term(P),
    {term, "[[], a, [], b, c]"} = get_term(P),
    {term, "[[], a, '', b, c]"} = get_term(P),
    {term, "[[x, 2], [y, 3], [z, 4]]"}= get_term(P),

    %% {term, "[{name, 'Madonna'}, {age, 21}, {data, [{addr, "E-street", 42}]}]"} = get_term(P),
    %% maybe regexp instead?
    {term, "[{pi, 3.141500}, {'cos(70)', 0.342020}]"} = get_term(P),
    {term, "[[pi, 3.141500], ['cos(70)', 0.342020]]"} = get_term(P),

    {term, "[-1]"} = get_term(P),

    runner:recv_eot(P),
    ok.

strings(Config) when is_list(Config) ->
    P = runner:start(Config, ?strings),

    {term, "\"\\n\""} = get_term(P),
    {term, "\"\\r\\n\""} = get_term(P),
    {term, "\"a\""} = get_term(P),
    {term, "\"A\""} = get_term(P),
    {term, "\"0\""} = get_term(P),
    {term, "\"9\""} = get_term(P),
    {term, "\"The rain in Spain stays mainly in the plains\""} = get_term(P),
    {term, "\"   abcdefghijklmnopq   \""} = get_term(P),

    runner:recv_eot(P),
    ok.

maps(Config) ->
    P = runner:start(Config, ?maps),

    {term, "#{}"} = get_term(P),
    {term, "#{key => value}"} = get_term(P),
    {term, "#{key => value, another_key => {ok, 42}}"} = get_term(P),

    runner:recv_eot(P),
    ok.

funs(Config) ->
    P = runner:start(Config, ?funs),

    {term, "#Fun{some_module.42.3735928559}"} = get_term(P),
    {term, "#Fun{some_module.37.195935983}"} = get_term(P),
    {term, "fun erlang:abs/1"} = get_term(P),

    runner:recv_eot(P),
    ok.

binaries(Config) ->
    P = runner:start(Config, ?binaries),

    "#Bin<>" = send_term_get_printed(P, <<>>),
    "#Bin<1,2,3>" = send_term_get_printed(P, <<1,2,3>>),
    "#Bin<0,127,128,255>" = send_term_get_printed(P, <<0,127,128,255>>),
    Bin30 = <<1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30>>,
    "#Bin<1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30>"
        = send_term_get_printed(P, Bin30),
    "#Bin<1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,...>"
        = send_term_get_printed(P, <<Bin30/binary,31>>),

    runner:recv_eot(P),
    ok.

bitstrings(Config) ->
    P = runner:start(Config, ?bitstrings),

    "#Bits<1:1>" = send_term_get_printed(P, <<1:1>>),
    "#Bits<123:7>" = send_term_get_printed(P, <<123:7>>),
    "#Bits<1,2,3:4>" = send_term_get_printed(P, <<1,2,3:4>>),
    "#Bits<0,127,128,255,126:7>" = send_term_get_printed(P, <<0,127,128,255,-2:7>>),
    Bits30 = <<1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,
               20,21,22,23,24,25,26,27,28,29,30:5>>,
    "#Bits<1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30:5>"
        = send_term_get_printed(P, Bits30),
    "#Bin<1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,241>"
        = send_term_get_printed(P, <<Bits30/bits,1:3>>),
    "#Bits<1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,240,...>"
        = send_term_get_printed(P, <<Bits30/bits,1:4>>),

    runner:recv_eot(P),
    ok.

integers(Config) ->
    Port = runner:start(Config, ?integers),

    test_integers(Port, -1000, 1000),
    test_integers(Port, (1 bsl 27) - 1000, (1 bsl 27) + 1000),
    test_integers(Port, -(1 bsl 27) - 1000, -(1 bsl 27) + 1000),
    test_integers(Port, (1 bsl 28) - 1000, (1 bsl 28) + 1000),
    test_integers(Port, -(1 bsl 28) - 1000, -(1 bsl 28) + 1000),
    test_integers(Port, (1 bsl 31) - 1000, (1 bsl 31) + 1000),
    test_integers(Port, -(1 bsl 31) - 1000, -(1 bsl 31) + 1000),
    test_integers(Port, (1 bsl 32) - 1000, (1 bsl 32) + 1000),
    test_integers(Port, -(1 bsl 32) - 1000, -(1 bsl 32) + 1000),
    test_integers(Port, (1 bsl 60) - 1000, (1 bsl 60) + 1000),
    test_integers(Port, -(1 bsl 60) - 1000, -(1 bsl 60) + 1000),
    test_integers(Port, 16#feeddeaddeadbeef - 1000, 16#feeddeaddeadbeef + 1000),
    test_integers(Port, -16#feeddeaddeadbeef - 1000, -16#feeddeaddeadbeef + 1000),
    test_integers(Port, (1 bsl 64) - 1000, (1 bsl 64) + 1000),
    test_integers(Port, 16#addfeeddeaddeadbeef - 1000, 16#addfeeddeaddeadbeef + 1000),
    test_integers(Port, -16#addfeeddeaddeadbeef - 1000, -16#addfeeddeaddeadbeef + 1000),
    test_integers(Port, -(1 bsl 64) - 1000, -(1 bsl 64) + 1000),
    test_integers(Port, (1 bsl 8192) - 1000, (1 bsl 8192) + 1000),
    test_integers(Port, -(1 bsl 8192) - 1000, -(1 bsl 8192) + 1000),

    "done" = send_term_get_printed(Port, done),

    runner:recv_eot(Port),

    ok.

test_integer(Port, Int, Print) when is_integer(Int) ->
    Res = send_term_get_printed(Port, Int),
    case Print of
        true ->
            io:format("Res: ~s~n", [Res]);
        false ->
            ok
    end,
    %% Large bignums are printed in base 16...
    Exp = case Res of
              "16#" ++ _ ->
                  "16#" ++ integer_to_list(Int, 16);
              "-16#" ++ _ ->
                  "-16#" ++ integer_to_list(-1*Int, 16);
              _ ->
                  integer_to_list(Int)
           end,
    case Exp =:= Res of
        true ->
            ok;
        false ->
            io:format("Exp: ~s~nRes: ~s~n", [Exp, Res]),
            ct:fail({Exp, Res})
    end.

test_integers(Port, FromInt, ToInt) ->
    test_integers(Port, FromInt, ToInt, true).

test_integers(_Port, FromInt, ToInt, _Print) when FromInt > ToInt ->
    ok;
test_integers(Port, FromInt, ToInt, Print) ->
    ok = test_integer(Port, FromInt, Print),
    NewFromInt = FromInt + 1,
    test_integers(Port, NewFromInt, ToInt, NewFromInt == ToInt).


send_term_get_printed(Port, Term) ->
    Port ! {self(), {command, term_to_binary(Term)}},
    {term, String} = get_term(Port),
    String.
