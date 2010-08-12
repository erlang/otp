%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2001-2010. All Rights Reserved.
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
-module(lc_SUITE).

-author('bjorn@erix.ericsson.se').
-export([all/1,init_per_testcase/2,fin_per_testcase/2,init_all/1,finish_all/1,
	 basic/1]).

-include("test_server.hrl").

all(suite) ->
    [{conf,init_all,cases(),finish_all}].

cases() ->
    [basic].

init_per_testcase(_Case, Config) ->
    test_lib:interpret(?MODULE),
    Dog = test_server:timetrap(?t:minutes(1)),
    [{watchdog,Dog}|Config].

fin_per_testcase(_Case, Config) ->
    Dog = ?config(watchdog, Config),
    ?t:timetrap_cancel(Dog),
    ok.

init_all(Config) when is_list(Config) ->
    ?line test_lib:interpret(?MODULE),
    ?line true = lists:member(?MODULE, int:interpreted()),
    ok.

finish_all(Config) when is_list(Config) ->
    ok.

basic(Config) when list(Config) ->
    ?line L0 = lists:seq(1, 10),
    ?line L1 = my_map(fun(X) -> {x,X} end, L0),
    ?line L1 = [{x,X} || X <- L0],
    ?line L0 = my_map(fun({x,X}) -> X end, L1),
    ?line [1,2,3,4,5] = [X || X <- L0, X < 6],
    ?line [4,5,6] = [X || X <- L0, X > 3, X < 7],
    ?line [] = [X || X <- L0, X > 32, X < 7],
    ?line [1,3,5,7,9] = [X || X <- L0, odd(X)],

    %% Error cases.
    ?line [] = [X || X <- L1, X+1 < 2],
    ?line [] = [{xx,X} || X <- L0, element(2, X) == no_no_no],
    ?line {'EXIT',_}  = (catch [X || X <- L1, odd(X)]),

    ok.

my_map(F, L) ->
    [F(X) || X <- L].

odd(X) ->
    X rem 2 == 1.
