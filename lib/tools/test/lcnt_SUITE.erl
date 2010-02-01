%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2009. All Rights Reserved.
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

-module(lcnt_SUITE).
-include("test_server.hrl").

%% Test server specific exports
-export([all/1]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([
	load_v1/1,
	conflicts/1,
	locations/1,
	swap_keys/1
	]).

%% Default timetrap timeout (set in init_per_testcase)
-define(default_timeout, ?t:minutes(2)).

init_per_suite(Config) when is_list(Config) ->
    Config.

end_per_suite(Config) when is_list(Config) ->
    Config.

init_per_testcase(_Case, Config) ->
    Dog = ?t:timetrap(?default_timeout),
    [{watchdog,Dog} | Config].

end_per_testcase(_Case, Config) ->
    Dog = ?config(watchdog, Config),
    ?t:timetrap_cancel(Dog),
    ok.

all(suite) ->
    % Test cases
    [load_v1, conflicts, locations, swap_keys].

%%----------------------------------------------------------------------
%% Tests
%%----------------------------------------------------------------------

load_v1(suite) ->
    [];
load_v1(doc) ->
    ["Load data from file."];
load_v1(Config) when is_list(Config) ->
    ?line {ok, _} = lcnt:start(),
    ?line Path = ?config(data_dir, Config),
    ?line File = filename:join([Path,"big_bang_40.lcnt"]),
    ?line ok = lcnt:load(File),
    ?line ok = lcnt:stop(),
    ok.

conflicts(suite) ->
    [];
conflicts(doc) ->
    ["API: conflicts"];
conflicts(Config) when is_list(Config) ->
    ?line {ok, _} = lcnt:start(),
    ?line Path = ?config(data_dir, Config),
    ?line File = filename:join([Path,"big_bang_40.lcnt"]),
    ?line ok = lcnt:load(File),
    ?line ok = lcnt:conflicts(),
    THs      = [-1, 0, 100, 1000],
    Print    = [name , id , type , entry , tries , colls , ratio , time , duration],
    Opts     = [
	[{sort, Sort}, {reverse, Rev}, {max_locks, ML}, {combine, Combine}, {thresholds, [TH]}, {print, [Print]}] ||
	    Sort    <- [name , id , type , tries , colls , ratio , time , entry],
	    ML      <- [none, 1 , 32,  4096],
	    Combine <- [true, false],
	    TH      <- [{tries, Tries} || Tries <- THs] ++ [{colls, Colls} || Colls <- THs] ++ [{time, Time} || Time <- THs],
	    Rev     <- [true, false]
	],
    ?line ok = test_conflicts_opts(Opts),
    ?line ok = lcnt:stop(),
    ok.

test_conflicts_opts([]) -> ok;
test_conflicts_opts([Opt|Opts]) ->
    ?line ok = lcnt:conflicts(Opt),
    test_conflicts_opts(Opts).

locations(suite) ->
    [];
locations(doc) ->
    ["API: locations"];
locations(Config) when is_list(Config) ->
    ?line {ok, _} = lcnt:start(),
    ?line Path = ?config(data_dir, Config),
    ?line File = filename:join([Path,"big_bang_40.lcnt"]),
    ?line ok = lcnt:load(File),
    ?line ok = lcnt:locations(),
    THs      = [-1, 0, 100, 1000],
    Print    = [name , id , type , entry , tries , colls , ratio , time , duration],
    Opts     = [
	[{full_id, Id}, {sort, Sort}, {max_locks, ML}, {combine, Combine}, {thresholds, [TH]}, {print, Print}] ||
	    Sort    <- [name , id , type , tries , colls , ratio , time , entry],
	    ML      <- [none, 1 , 64],
	    Combine <- [true, false],
	    TH      <- [{tries, Tries} || Tries <- THs] ++ [{colls, Colls} || Colls <- THs] ++ [{time, Time} || Time <- THs],
	    Id      <- [true, false]
	],
    ?line ok = test_locations_opts(Opts),
    ?line ok = lcnt:stop(),
    ok.

test_locations_opts([]) -> ok;
test_locations_opts([Opt|Opts]) ->
    ?line ok = lcnt:locations(Opt),
    test_locations_opts(Opts).

swap_keys(suite) ->
    [];
swap_keys(doc) ->
    ["Test interchanging port/process id with class"];
swap_keys(Config) when is_list(Config) ->
    ?line {ok, _} = lcnt:start(),
    ?line Path = ?config(data_dir, Config),
    ?line File = filename:join([Path,"big_bang_40.lcnt"]),
    ?line ok = lcnt:load(File),
    ?line ok = lcnt:conflicts(),
    ?line ok = lcnt:swap_pid_keys(),
    ?line ok = lcnt:conflicts(),
    ?line ok = lcnt:stop(),
    ok.


%%----------------------------------------------------------------------
%% Auxiliary tests
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Auxiliary
%%----------------------------------------------------------------------
