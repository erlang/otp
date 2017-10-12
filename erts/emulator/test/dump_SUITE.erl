%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2017. All Rights Reserved.
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

-module(dump_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, suite/0, init_per_testcase/2, end_per_testcase/2]).

-export([signal_abort/1]).

-export([load/0]).

-include_lib("kernel/include/file.hrl").

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 2}}].

all() ->
    [signal_abort].

init_per_testcase(signal_abort, Config) ->
    SO = erlang:system_info(schedulers_online),
    erts_debug:set_internal_state(available_internal_state, true),
    Dump = erts_debug:get_internal_state(scheduler_dump),
    erts_debug:set_internal_state(available_internal_state, false),
    if SO < 3 ->
            {skip, "not enough schedulers"};
       not Dump ->
            {skip, "the platform does not support scheduler dump"};
       Dump ->
            Config
    end.

end_per_testcase(_, Config) ->
    Config.

%%%
%%% The test cases -------------------------------------------------------------
%%%

%% Test that a snapshot is taken of other schedulers using a signal
%% when a crash dump is generated.
signal_abort(Config) ->

    Dump = filename:join(proplists:get_value(priv_dir, Config),"signal_abort.dump"),

    {ok, Node} = start_node(Config),

    _P1 = spawn(Node, ?MODULE, load, []),
    _P2 = spawn(Node, ?MODULE, load, []),
    _P3 = spawn(Node, ?MODULE, load, []),
    _P4 = spawn(Node, ?MODULE, load, []),
    _P5 = spawn(Node, ?MODULE, load, []),
    _P6 = spawn(Node, ?MODULE, load, []),

    timer:sleep(500),

    true = rpc:call(Node, os, putenv, ["ERL_CRASH_DUMP",Dump]),
    rpc:call(Node, erlang, halt, ["dump"]),

    {ok, Bin} = get_dump_when_done(Dump),

    ct:log("~s",[Bin]),

    {match, Matches} = re:run(Bin,"Current Process: <",[global]),

    ct:log("Found ~p",[Matches]),

    true = length(Matches) > 1,

    file:delete(Dump),

    ok.

get_dump_when_done(Dump) ->
    case file:read_file_info(Dump) of
        {ok, #file_info{ size = Sz }} ->
            get_dump_when_done(Dump, Sz);
        {error, enoent} ->
            timer:sleep(100),
            get_dump_when_done(Dump)
    end.

get_dump_when_done(Dump, Sz) ->
    timer:sleep(100),
    case file:read_file_info(Dump) of
        {ok, #file_info{ size = Sz }} ->
            file:read_file(Dump);
        {ok, #file_info{ size = NewSz }} ->
            get_dump_when_done(Dump, NewSz)
    end.

load() ->
    lists:seq(1,10000),
    load().

start_node(Config) when is_list(Config) ->
    Pa = filename:dirname(code:which(?MODULE)),
    Name = list_to_atom(atom_to_list(?MODULE)
                        ++ "-"
                        ++ atom_to_list(proplists:get_value(testcase, Config))
                        ++ "-"
                        ++ integer_to_list(erlang:system_time(second))
                        ++ "-"
                        ++ integer_to_list(erlang:unique_integer([positive]))),
    test_server:start_node(Name, slave, [{args, "-pa "++Pa}]).
