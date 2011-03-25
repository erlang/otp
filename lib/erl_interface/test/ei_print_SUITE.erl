%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2011. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%

%%
-module(ei_print_SUITE).

-include_lib("test_server/include/test_server.hrl").
-include("ei_print_SUITE_data/ei_print_test_cases.hrl").

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, 
	 atoms/1, tuples/1, lists/1, strings/1]).

-import(runner, [get_term/1]).

%% This test suite test the ei_print() function.
%% It uses the port program "ei_format_test".

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [atoms, tuples, lists, strings].

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


%% Tests formatting various atoms.

atoms(suite) -> [];
atoms(Config) when is_list(Config) ->
    ?line P = runner:start(?atoms),

    ?line {term, "''"} = get_term(P),
    ?line {term, "a"} = get_term(P),
    ?line {term, "'A'"} = get_term(P),
    ?line {term, "abc"} = get_term(P),
    ?line {term, "'Abc'"} = get_term(P),
    ?line {term, "ab@c"} = get_term(P),
    ?line {term, "'The rain in Spain stays mainly in the plains'"} =
	get_term(P),

    ?line {term, "a"} = get_term(P),
    ?line {term, "ab"} = get_term(P),
    ?line {term, "abc"} = get_term(P),
    ?line {term, "ab@c"} = get_term(P),
    ?line {term, "abcdefghijklmnopq"} = get_term(P),

    ?line {term, "''"} = get_term(P),
    ?line {term, "a"} = get_term(P),
    ?line {term, "'A'"} = get_term(P),
    ?line {term, "abc"} = get_term(P),
    ?line {term, "'Abc'"} = get_term(P),
    ?line {term, "ab@c"} = get_term(P),
    ?line {term, "'The rain in Spain stays mainly in the plains'"} =
	get_term(P),

    ?line {term, "a"} = get_term(P),
    ?line {term, "ab"} = get_term(P),
    ?line {term, "abc"} = get_term(P),
    ?line {term, "ab@c"} = get_term(P),
    ?line {term, "'   abcdefghijklmnopq   '"} = get_term(P),

    ?line runner:recv_eot(P),
    ok.



%% Tests formatting various tuples

tuples(suite) -> [];
tuples(Config) when is_list(Config) ->
    ?line P = runner:start(?tuples),

    ?line {term, "{}"} = get_term(P),
    ?line {term, "{a}"} = get_term(P),
    ?line {term, "{a, b}"} = get_term(P),
    ?line {term, "{a, b, c}"} = get_term(P),
    ?line {term, "{1}"} = get_term(P),
    ?line {term, "{[]}"} = get_term(P),
    ?line {term, "{[], []}"} = get_term(P),
    ?line {term, "{[], a, b, c}"} = get_term(P),
    ?line {term, "{[], a, [], b, c}"} = get_term(P),
    ?line {term, "{[], a, '', b, c}"} = get_term(P),

    ?line runner:recv_eot(P),
    ok.



%% Tests formatting various lists

lists(suite) -> [];
lists(Config) when is_list(Config) ->
    ?line P = runner:start(?lists),

    ?line {term, "[]"} = get_term(P),
    ?line {term, "[a]"} = get_term(P),
    ?line {term, "[a, b]"} = get_term(P),
    ?line {term, "[a, b, c]"} = get_term(P),
    ?line {term, "[1]"} = get_term(P),
    ?line {term, "[[]]"} = get_term(P),
    ?line {term, "[[], []]"} = get_term(P),
    ?line {term, "[[], a, b, c]"} = get_term(P),
    ?line {term, "[[], a, [], b, c]"} = get_term(P),
    ?line {term, "[[], a, '', b, c]"} = get_term(P),
    ?line {term, "[[x, 2], [y, 3], [z, 4]]"}= get_term(P),

%%    ?line {term, "[{name, 'Madonna'}, {age, 21}, {data, [{addr, "E-street", 42}]}]"} = 
%%	get_term(P),
    %% kanske regexp i stället?
    ?line {term, "[{pi, 3.141500}, {'cos(70)', 0.342020}]"} = get_term(P),
    ?line {term, "[[pi, 3.141500], ['cos(70)', 0.342020]]"} = get_term(P),

    ?line {term, "[-1]"} = get_term(P),

    ?line runner:recv_eot(P),
    ok.

strings(suite) -> [];
strings(Config) when is_list(Config) ->
    ?line P = runner:start(?strings),

    ?line {term, "\"\\n\""} = get_term(P),
    ?line {term, "\"\\r\\n\""} = get_term(P),
    ?line {term, "\"a\""} = get_term(P),
    ?line {term, "\"A\""} = get_term(P),
    ?line {term, "\"0\""} = get_term(P),
    ?line {term, "\"9\""} = get_term(P),
    ?line {term, "\"The rain in Spain stays mainly in the plains\""} = get_term(P),
    ?line {term, "\"   abcdefghijklmnopq   \""} = get_term(P),

    ?line runner:recv_eot(P),
    ok.
    
