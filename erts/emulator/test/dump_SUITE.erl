%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2022. All Rights Reserved.
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

-export([signal_abort/1, exiting_dump/1, free_dump/1,
         heart_dump/1, heart_no_dump/1]).

-export([load/1]).

-include_lib("kernel/include/file.hrl").

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 2}}].

all() ->
    [signal_abort, exiting_dump, free_dump, heart_dump, heart_no_dump].

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
    erts_test_utils:ept_check_leaked_nodes(Config).

%%%
%%% The test cases -------------------------------------------------------------
%%%

%% Test that a snapshot is taken of other schedulers using a signal
%% when a crash dump is generated.
signal_abort(Config) ->

    Dump = filename:join(proplists:get_value(priv_dir, Config),"signal_abort.dump"),

    {ok, _Peer, Node} = ?CT_PEER(),

    false = rpc:call(Node, erts_debug, set_internal_state,
                     [available_internal_state, true]),

    Iter = lists:seq(2, 3),

    spawn_opt(Node,
              fun() ->
                      os:putenv("ERL_CRASH_DUMP", Dump),
                      code:ensure_loaded(timer),

                      %% We spread the load on all schedulers except scheduler 1
                      [spawn_opt(?MODULE, load, [self()], [{scheduler, I}])
                       || I <- Iter],

                      %% Make sure that each process is started
                      [receive ok -> ok end || _ <- Iter],
                      timer:sleep(500),
                      erlang:halt("dump")
              end,
              [{scheduler,1},{priority,high}, monitor]),
    receive
        M -> ct:pal("~p",[M])
    end,

    {ok, Bin} = get_dump_when_done(Dump),

    {match, Matches} = re:run(Bin,"Current Process: <",[global]),

    ct:log("Found ~p",[Matches]),

    true = length(Matches) > 1,

    file:delete(Dump),

    ok.

load(Parent) ->
    Parent ! ok,
    load().
load() ->
    %% We generate load by sleeping
    erts_debug:set_internal_state(sleep, 10),
    load().


%% Test that crash dumping when a process is in the state EXITING works
exiting_dump(Config) when is_list(Config) ->
    Dump = filename:join(proplists:get_value(priv_dir, Config),"signal_abort.dump"),

    {ok, _Peer, Node} = ?CT_PEER(),

    Self = self(),

    Pid = spawn_link(Node,
                     fun() ->
                             [begin
                                  T = ets:new(hej,[]),
                                  [ets:insert(T,{I,I}) || I <- lists:seq(1,1000)]
                              end || _ <- lists:seq(1,1000)],
                             Self ! ready,
                             receive {terminate, Pid} -> Pid ! ok end
                     end),

    true = rpc:call(Node, os, putenv, ["ERL_CRASH_DUMP",Dump]),

    receive ready -> unlink(Pid), Pid ! {terminate, self()} end,
    receive ok -> ok end,

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

    {ok, _PeerA, NodeA} = ?CT_PEER(),
    {ok, PeerB, NodeB} = ?CT_PEER(),

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

    peer:stop(PeerB),

    ok.

%% Test that crash dumping works when heart is used
heart_dump(Config) ->
    Dump = filename:join(proplists:get_value(priv_dir, Config),"heart.dump"),
    {ok, _Peer, Node} = ?CT_PEER(#{ args => ["-heart"] }),
    true = rpc:call(Node, os, putenv, ["ERL_CRASH_DUMP",Dump]),
    true = rpc:call(Node, os, putenv, ["ERL_CRASH_DUMP_SECONDS","10"]),
    rpc:call(Node, erlang, halt, ["dump"]),
    {ok, _Bin} = get_dump_when_done(Dump),
    ok.

%% Test that there is no crash dump if heart is used and DUMP_SECONDS is not set
heart_no_dump(Config) ->
    Dump = filename:join(proplists:get_value(priv_dir, Config),"heart_no.dump"),
    {ok, _Peer, Node} = ?CT_PEER(#{ args => ["-heart"] }),
    true = rpc:call(Node, os, putenv, ["ERL_CRASH_DUMP",Dump]),
    true = rpc:call(Node, os, unsetenv, ["ERL_CRASH_DUMP_SECONDS"]),
    rpc:call(Node, erlang, halt, ["dump"]),
    timer:sleep(1000),
    {error, enoent} = file:read_file_info(Dump),
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
        {ok, #file_info{ size = Sz }} when Sz > 1000 ->
            {ok, Bin} = file:read_file(Dump),
            ct:log("~s",[Bin]),
            {ok, Bin};
        {ok, #file_info{ size = NewSz }} ->
            get_dump_when_done(Dump, NewSz)
    end.
