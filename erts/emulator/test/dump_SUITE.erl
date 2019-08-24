%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2018. All Rights Reserved.
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

-export([signal_abort/1, exiting_dump/1, free_dump/1]).

-export([load/0]).

-include_lib("kernel/include/file.hrl").

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 2}}].

all() ->
    [signal_abort, exiting_dump, free_dump].

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
    end;
init_per_testcase(_, Config) ->
    Config.


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

    SO = rpc:call(Node, erlang, system_info, [schedulers_online]),

    _P1 = spawn_opt(Node, ?MODULE, load, [], [{scheduler, (0 rem SO) + 1}]),
    _P2 = spawn_opt(Node, ?MODULE, load, [], [{scheduler, (1 rem SO) + 1}]),
    _P3 = spawn_opt(Node, ?MODULE, load, [], [{scheduler, (2 rem SO) + 1}]),
    _P4 = spawn_opt(Node, ?MODULE, load, [], [{scheduler, (3 rem SO) + 1}]),
    _P5 = spawn_opt(Node, ?MODULE, load, [], [{scheduler, (4 rem SO) + 1}]),
    _P6 = spawn_opt(Node, ?MODULE, load, [], [{scheduler, (5 rem SO) + 1}]),

    timer:sleep(500),

    true = rpc:call(Node, os, putenv, ["ERL_CRASH_DUMP",Dump]),
    rpc:call(Node, erlang, halt, ["dump"]),

    {ok, Bin} = get_dump_when_done(Dump),

    {match, Matches} = re:run(Bin,"Current Process: <",[global]),

    ct:log("Found ~p",[Matches]),

    true = length(Matches) > 1,

    file:delete(Dump),

    ok.

load() ->
    lists:seq(1,10000),
    load().


%% Test that crash dumping when a process is in the state EXITING works
exiting_dump(Config) when is_list(Config) ->
    Dump = filename:join(proplists:get_value(priv_dir, Config),"signal_abort.dump"),

    {ok, Node} = start_node(Config),

    Self = self(),

    Pid = spawn_link(Node,
                     fun() ->
                             [begin
                                  T = ets:new(hej,[]),
                                  [ets:insert(T,{I,I}) || I <- lists:seq(1,1000)]
                              end || _ <- lists:seq(1,1000)],
                             Self ! ready,
                             receive ok -> ok end
                     end),

    true = rpc:call(Node, os, putenv, ["ERL_CRASH_DUMP",Dump]),

    receive ready -> unlink(Pid), Pid ! ok end,

    rpc:call(Node, erlang, halt, ["dump"]),

    {ok, Bin} = get_dump_when_done(Dump),

    {match, Matches} = re:run(Bin,"^State: Exiting", [global, multiline]),

    ct:log("Found ~p",[Matches]),

    true = length(Matches) == 1,

    file:delete(Dump),

    ok.

%% Test that crash dumping when a process is in the state FREE works
free_dump(Config) when is_list(Config) ->
    Dump = filename:join(proplists:get_value(priv_dir, Config),"signal_abort.dump"),

    {ok, NodeA} = start_node(Config),
    {ok, NodeB} = start_node(Config),

    Self = self(),

    PidA = spawn_link(
             NodeA,
             fun() ->
                     Self ! ready,
                     Reason = lists:duplicate(1000000,100),
                     receive
                         ok ->
                             spawn(fun() ->
                                           erlang:system_monitor(self(), [busy_dist_port]),
                                           timer:sleep(5),
                                           receive
                                               M ->
                                                   io:format("~p",[M])
%% We may want to add this timeout here in-case no busy condition is triggered
%%                                           after 60 * 1000 ->
%%                                                   io:format("Timeout")
                                           end,
                                           erlang:halt("dump")
                                   end),
                             exit(Reason)
                     end
             end),

    PidB = spawn_link(NodeB,
                      fun() ->
                              [erlang:monitor(process, PidA) || _ <- lists:seq(1,10000)],
                              Self ! done,
                              receive _ -> ok end
                      end),

    receive done -> ok end,
    true = rpc:call(NodeA, os, putenv, ["ERL_CRASH_DUMP",Dump]),
    %% Make the node busy towards NodeB for 10 seconds.
    BusyPid = rpc:call(NodeA, distribution_SUITE, make_busy, [NodeB,10000]),
    ct:pal("~p",[BusyPid]),

    receive ready -> unlink(PidA), PidA ! ok end,

    {ok, Bin} = get_dump_when_done(Dump),

    {match, Matches} = re:run(Bin,"^State: Non Existing", [global, multiline]),

    ct:log("Found ~p",[Matches]),

    true = length(Matches) == 1,

    file:delete(Dump),

    unlink(PidB),

    rpc:call(NodeB, erlang, halt, [0]),

    ok.


get_dump_when_done(Dump) ->
    case file:read_file_info(Dump) of
        {ok, #file_info{ size = Sz }} ->
            get_dump_when_done(Dump, Sz);
        {error, enoent} ->
            timer:sleep(1000),
            get_dump_when_done(Dump)
    end.

get_dump_when_done(Dump, Sz) ->
    timer:sleep(1000),
    case file:read_file_info(Dump) of
        {ok, #file_info{ size = Sz }} ->
            {ok, Bin} = file:read_file(Dump),
            ct:log("~s",[Bin]),
            {ok, Bin};
        {ok, #file_info{ size = NewSz }} ->
            get_dump_when_done(Dump, NewSz)
    end.

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
