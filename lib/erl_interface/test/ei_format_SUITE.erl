%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2011. All Rights Reserved.
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
-module(ei_format_SUITE).

-include_lib("test_server/include/test_server.hrl").
-include("ei_format_SUITE_data/ei_format_test_cases.hrl").

-export([
	 format_wo_ver/1,
	 all/0, suite/0,groups/0,
	 init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, 
	 atoms/1, 
	 tuples/1, 
	 lists/1
	]).

-import(runner, [get_term/1]).

%% This test suite test the erl_format() function.
%% It uses the port program "ei_format_test".

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [format_wo_ver, atoms, tuples, lists].

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

    ?line {term, ''} = get_term(P),
    ?line {term, 'a'} = get_term(P),
    ?line {term, 'A'} = get_term(P),
    ?line {term, 'abc'} = get_term(P),
    ?line {term, 'Abc'} = get_term(P),
    ?line {term, 'ab@c'} = get_term(P),
    ?line {term, 'The rain in Spain stays mainly in the plains'} =
	get_term(P),

    ?line {term, a} = get_term(P),
    ?line {term, ab} = get_term(P),
    ?line {term, abc} = get_term(P),
    ?line {term, ab@c} = get_term(P),
    ?line {term, abcdefghijklmnopq} = get_term(P),

    ?line {term, ''} = get_term(P),
    ?line {term, 'a'} = get_term(P),
    ?line {term, 'A'} = get_term(P),
    ?line {term, 'abc'} = get_term(P),
    ?line {term, 'Abc'} = get_term(P),
    ?line {term, 'ab@c'} = get_term(P),
    ?line {term, 'The rain in Spain stays mainly in the plains'} =
	get_term(P),

    ?line {term, a} = get_term(P),
    ?line {term, ab} = get_term(P),
    ?line {term, abc} = get_term(P),
    ?line {term, ab@c} = get_term(P),
    ?line {term, '   abcdefghijklmnopq   '} = get_term(P),

    ?line runner:recv_eot(P),
    ok.



%% Tests formatting various tuples

tuples(suite) -> [];
tuples(Config) when is_list(Config) ->
    ?line P = runner:start(?tuples),

    ?line {term, {}} = get_term(P),
    ?line {term, {a}} = get_term(P),
    ?line {term, {a, b}} = get_term(P),
    ?line {term, {a, b, c}} = get_term(P),
    ?line {term, {1}} = get_term(P),
    ?line {term, {[]}} = get_term(P),
    ?line {term, {[], []}} = get_term(P),
    ?line {term, {[], a, b, c}} = get_term(P),
    ?line {term, {[], a, [], b, c}} = get_term(P),
    ?line {term, {[], a, '', b, c}} = get_term(P),

    ?line runner:recv_eot(P),
    ok.



%% Tests formatting various lists

lists(suite) -> [];
lists(Config) when is_list(Config) ->
    ?line P = runner:start(?lists),

    ?line {term, []} = get_term(P),
    ?line {term, [a]} = get_term(P),
    ?line {term, [a, b]} = get_term(P),
    ?line {term, [a, b, c]} = get_term(P),
    ?line {term, [1]} = get_term(P),
    ?line {term, [[]]} = get_term(P),
    ?line {term, [[], []]} = get_term(P),
    ?line {term, [[], a, b, c]} = get_term(P),
    ?line {term, [[], a, [], b, c]} = get_term(P),
    ?line {term, [[], a, '', b, c]} = get_term(P),
    ?line {term, [[x, 2], [y, 3], [z, 4]]}= get_term(P),
    ?line {term, [{a,b},{c,d}]}= get_term(P),
%%    ?line {term, [{name, 'Madonna'}, {age, 21}, {data, [{addr, "E-street", 42}]}]} = 
%%	get_term(P),
    
    ?line {term, [{pi, F1}, {'cos(70)', F2}]} = get_term(P),
    %% don't match floats directly
    true= abs(3.1415-F1) < 0.01,
    true= abs(0.34202-F2) < 0.01,

    ?line {term, [[pi, F3], ['cos(70)', F4]]} = get_term(P),
    true= abs(3.1415-F3) < 0.01,
    true= abs(0.34202-F4) < 0.01,


%%    ?line {term, [[pi, 3.1415], [], ["cos(70)", 0.34202]]} = get_term(P),
    ?line {term, [-1]} = get_term(P),
    ?line {term, "hejsan"} = get_term(P),


    ?line Str1 = lists:duplicate(65535,$A),
    ?line Str2 = lists:duplicate(65536,$A),
    ?line {term,Str1} = get_term(P),
    ?line {term,Str2} = get_term(P),

    ?line runner:recv_eot(P),
    ok.


format_wo_ver(suite) -> [];
format_wo_ver(Config) when is_list(Config) ->
    ?line P = runner:start(?format_wo_ver),

    ?line {term, [-1, 2, $c, {a, "b"}, {c, 10}]} = get_term(P),

    ?line runner:recv_eot(P),
    ok.
