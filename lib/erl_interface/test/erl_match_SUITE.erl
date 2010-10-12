%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2010. All Rights Reserved.
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
-module(erl_match_SUITE).

-include_lib("test_server/include/test_server.hrl").
-include("erl_match_SUITE_data/match_test_cases.hrl").

-export([all/0,groups/0,init_per_group/2,end_per_group/2, atoms/1, lists/1, tuples/1, references/1, pids/1, ports/1,
	 bind/1, integers/1, floats/1, binaries/1, strings/1]).

%% For interactive running of matcher.
-export([start_matcher/0, erl_match/3]).

%% This test suite tests the erl_match() function.

all() -> 
[atoms, lists, tuples, references, pids, ports, bind,
 integers, floats, binaries, strings].

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
	Config.

end_per_group(_GroupName, Config) ->
	Config.


atoms(suite) -> [];
atoms(Config) when is_list(Config) ->
    ?line P = start_matcher(),

    ?line eq(P, '', ''),
    ?line eq(P, a, a),
    ?line ne(P, a, b),
    ?line ne(P, a, aa),
    ?line eq(P, kalle, kalle),
    ?line ne(P, kalle, arne),

    ?line ne(P, kalle, 42),
    ?line ne(P, 42, kalle),

    ?line runner:finish(P),
    ok.

lists(suite) -> [];
lists(Config) when is_list(Config) ->
    ?line P = start_matcher(),
    ?line eq(P, [], []),

    ?line ne(P, [], [a]),
    ?line ne(P, [a], []),

    ?line eq(P, [a], [a]),
    ?line ne(P, [a], [b]),

    ?line eq(P, [a|b], [a|b]),
    ?line ne(P, [a|b], [a|x]),

    ?line eq(P, [a, b], [a, b]),
    ?line ne(P, [a, b], [a, x]),

    ?line eq(P, [a, b, c], [a, b, c]),
    ?line ne(P, [a, b|c], [a, b|x]),
    ?line ne(P, [a, b, c], [a, b, x]),
    ?line ne(P, [a, b|c], [a, b|x]),
    ?line ne(P, [a, x|c], [a, b|c]),
    ?line ne(P, [a, b, c], [a, x, c]),

    ?line runner:finish(P),
    ok.

tuples(suite) -> [];
tuples(Config) when is_list(Config) ->
    ?line P = start_matcher(),

    ?line ne(P, {}, {a, b}),
    ?line ne(P, {a, b}, {}),
    ?line ne(P, {a}, {a, b}),
    ?line ne(P, {a, b}, {a}),

    ?line eq(P, {}, {}),

    ?line eq(P, {a}, {a}),
    ?line ne(P, {a}, {b}),

    ?line eq(P, {1}, {1}),
    ?line ne(P, {1}, {2}),

    ?line eq(P, {a, b}, {a, b}),
    ?line ne(P, {x, b}, {a, b}),

    ?line ne(P, {error, x}, {error, y}),
    ?line ne(P, {error, {undefined, {subscriber, last}}},
	     {error, {undefined, {subscriber, name}}}),

    ?line runner:finish(P),
    ok.


references(suite) -> [];
references(Config) when is_list(Config) ->
    ?line P = start_matcher(),
    ?line Ref1 = make_ref(),
    ?line Ref2 = make_ref(),
    
    ?line eq(P, Ref1, Ref1),
    ?line eq(P, Ref2, Ref2),
    ?line ne(P, Ref1, Ref2),
    ?line ne(P, Ref2, Ref1),

    ?line runner:finish(P),
    ok.


pids(suite) -> [];
pids(Config) when is_list(Config) ->
    ?line P = start_matcher(),
    ?line Pid1 = c:pid(0,1,2),
    ?line Pid2 = c:pid(0,1,3),
    
    ?line eq(P, self(), self()),
    ?line eq(P, Pid1, Pid1),
    ?line ne(P, Pid1, self()),
    ?line ne(P, Pid2, Pid1),

    ?line runner:finish(P),
    ok.


ports(suite) -> [];
ports(Config) when is_list(Config) ->
    case os:type() of
	vxworks ->
	    {skipped,"not on vxworks, pucko"};
	_ ->
	    ?line P = start_matcher(),
	    ?line P2 = start_matcher(),
    
	    ?line eq(P, P, P),
	    ?line ne(P, P, P2),

	    ?line runner:finish(P),
	    ?line runner:finish(P2),
	    ok
    end.

integers(suite) -> [];
integers(Config) when is_list(Config) ->
    ?line P = start_matcher(),
    ?line I1 = 123,
    ?line I2 = 12345,
    ?line I3 = -123,
    ?line I4 = 2234,
    
    ?line eq(P, I1, I1),
    ?line eq(P, I2, I2),
    ?line ne(P, I1, I2),
    ?line ne(P, I1, I3),
    ?line eq(P, I4, I4),

    ?line runner:finish(P),
    ok.



floats(suite) -> [];
floats(Config) when is_list(Config) ->
    ?line P = start_matcher(),
    ?line F1 = 3.1414,
    ?line F2 = 3.1415,
    ?line F3 = 3.1416,

    ?line S1 = "string",
    ?line S2 = "string2",
    
    ?line eq(P, F1, F1),
    ?line eq(P, F2, F2),
    ?line ne(P, F1, F2),
    ?line ne(P, F3, F2),

    ?line eq(P, S2, S2),
    ?line ne(P, S1, S2),

    ?line runner:finish(P),
    ok.



binaries(suite) -> [];
binaries(Config) when is_list(Config) ->
    ?line P = start_matcher(),
    ?line Bin1 = term_to_binary({kalle, 146015, {kungsgatan, 23}}),
    ?line Bin2 = term_to_binary(sune),
    ?line Bin3 = list_to_binary("sune"),
    
    ?line eq(P, Bin1, Bin1),
    ?line eq(P, Bin2, Bin2),
    ?line eq(P, Bin3, Bin3),
    ?line ne(P, Bin1, Bin2),
    ?line ne(P, Bin1, Bin3),
    ?line ne(P, Bin2, Bin3),

    ?line runner:finish(P),
    ok.



strings(suite) -> [];
strings(Config) when is_list(Config) ->
    ?line P = start_matcher(),

    ?line S1 = "string",
    ?line S2 = "streng",
    ?line S3 = "String",
    
    ?line eq(P, S1, S1),
    ?line ne(P, S1, S2),
    ?line ne(P, S1, S3),

    ?line runner:finish(P),
    ok.



bind(suite) -> [];
bind(Config) when is_list(Config) ->
    ?line P = start_bind(),
    ?line S = "[X,Y,Z]",
    ?line L1 = [301,302,302],
    ?line L2 = [65,66,67],
        
    ?line bind_ok(P, S, L1),
    ?line bind_ok(P, S, L2),

    ?line runner:finish(P),
    ok.

start_bind() ->
    runner:start(?erl_match_bind).

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






start_matcher() ->
    runner:start(?erl_match_server).

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


