%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2011. All Rights Reserved.
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
-include_lib("test_server/include/test_server.hrl").

%% Test server specific exports
-export([all/0, suite/0,groups/0,init_per_group/2,end_per_group/2]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([
	t_load/1,
	t_conflicts/1,
	t_locations/1,
	t_swap_keys/1
    ]).

%% Default timetrap timeout (set in init_per_testcase)
-define(default_timeout, ?t:minutes(4)).

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
    catch lcnt:stop(),
    ok.

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> [t_load, t_conflicts, t_locations, t_swap_keys].

groups() -> [].

init_per_group(_GroupName, Config) -> Config.

end_per_group(_GroupName, Config) -> Config.


%%----------------------------------------------------------------------
%% Tests
%%----------------------------------------------------------------------

t_load(suite) -> [];
t_load(doc) -> ["Load data from file."];
t_load(Config) when is_list(Config) ->
    Path = ?config(data_dir, Config),
    Files = [filename:join([Path,"big_bang_40.lcnt"]),
	     filename:join([Path,"ehb_3_3_hist.lcnt"])],
    ok = t_load_file(Files),
    ok.

t_load_file([]) -> ok;
t_load_file([File|Files]) ->
    {ok, _} = lcnt:start(),
    ok = lcnt:load(File),
    ok = lcnt:stop(),
    t_load_file(Files).

t_conflicts(suite) -> [];
t_conflicts(doc) -> ["API: conflicts"];
t_conflicts(Config) when is_list(Config) ->
    Path = ?config(data_dir, Config),
    Files = [filename:join([Path,"big_bang_40.lcnt"]),
	     filename:join([Path,"ehb_3_3_hist.lcnt"])],
    ok = t_conflicts_file(Files),
    ok.

t_conflicts_file([]) -> ok;
t_conflicts_file([File|Files]) ->
    {ok, _} = lcnt:start(),
    ok = lcnt:load(File),
    ok = lcnt:conflicts(),
    THs   = [-1, 0, 100, 1000],
    Print = [name , id , type , entry , tries , colls , ratio , time , duration],
    Opts  = [
	[{sort, Sort}, {reverse, Rev}, {max_locks, ML}, {combine, Combine}, {thresholds, [TH]}, {print, [Print]}] ||
	    Sort    <- [name , id , type , tries , colls , ratio , time , entry],
	    ML      <- [none, 1 , 32,  4096],
	    Combine <- [true, false],
	    TH      <- [{tries, Tries} || Tries <- THs] ++ [{colls, Colls} || Colls <- THs] ++ [{time, Time} || Time <- THs],
	    Rev     <- [true, false]
	],
    ok = test_conflicts_opts(Opts),
    ok = lcnt:stop(),
    t_conflicts_file(Files).


test_conflicts_opts([]) -> ok;
test_conflicts_opts([Opt|Opts]) ->
    ok = lcnt:conflicts(Opt),
    test_conflicts_opts(Opts).

t_locations(suite) -> [];
t_locations(doc) -> ["API: locations"];
t_locations(Config) when is_list(Config) ->
    Path = ?config(data_dir, Config),
    Files = [filename:join([Path,"big_bang_40.lcnt"]),
	     filename:join([Path,"ehb_3_3_hist.lcnt"])],
    ok = t_locations_file(Files),
    ok.

t_locations_file([]) -> ok;
t_locations_file([File|Files]) ->
    {ok, _} = lcnt:start(),
    ok = lcnt:load(File),
    ok = lcnt:locations(),
    THs   = [-1, 0, 100, 1000],
    Print = [name , id , type , entry , tries , colls , ratio , time , duration],
    Opts  = [
	[{full_id, Id}, {sort, Sort}, {max_locks, ML}, {combine, Combine}, {thresholds, [TH]}, {print, Print}] ||
	    Sort    <- [name , id , type , tries , colls , ratio , time , entry],
	    ML      <- [none, 1 , 64],
	    Combine <- [true, false],
	    TH      <- [{tries, Tries} || Tries <- THs] ++ [{colls, Colls} || Colls <- THs] ++ [{time, Time} || Time <- THs],
	    Id      <- [true, false]
	],
    ok = test_locations_opts(Opts),
    ok = lcnt:stop(),
    t_locations_file(Files).

test_locations_opts([]) -> ok;
test_locations_opts([Opt|Opts]) ->
    ok = lcnt:locations(Opt),
    test_locations_opts(Opts).

t_swap_keys(suite) -> [];
t_swap_keys(doc) -> ["Test interchanging port/process id with class"];
t_swap_keys(Config) when is_list(Config) ->
    Path = ?config(data_dir, Config),
    Files = [filename:join([Path,"big_bang_40.lcnt"]),
	     filename:join([Path,"ehb_3_3_hist.lcnt"])],
    ok = t_swap_keys_file(Files),
    ok.

t_swap_keys_file([]) -> ok;
t_swap_keys_file([File|Files]) ->
    {ok, _} = lcnt:start(),
    ok = lcnt:load(File),
    ok = lcnt:conflicts(),
    ok = lcnt:swap_pid_keys(),
    ok = lcnt:conflicts(),
    ok = lcnt:stop(),
    t_swap_keys_file(Files).


%%----------------------------------------------------------------------
%% Auxiliary tests
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Auxiliary
%%----------------------------------------------------------------------
