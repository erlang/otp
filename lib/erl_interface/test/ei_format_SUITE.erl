%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2009. All Rights Reserved.
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
-module(ei_format_SUITE).

-include("test_server.hrl").
-include("ei_format_SUITE_data/ei_format_test_cases.hrl").

-export([
	format_wo_ver/1,
	all/1, 
	atoms/1, 
	tuples/1, 
	lists/1
	]).

-import(runner, [get_term/1]).

%% This test suite test the erl_format() function.
%% It uses the port program "ei_format_test".

all(suite) -> [
	format_wo_ver,
	atoms, 
	tuples, 
	lists
	].

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

    ?line {term, [-1, 2, {a, "b"}, {c, 10}]} = get_term(P),

    ?line runner:recv_eot(P),
    ok.
