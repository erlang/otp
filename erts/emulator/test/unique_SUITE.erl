%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2014-2017. All Rights Reserved.
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

-module(unique_SUITE).

-export([all/0, suite/0, init_per_suite/1, end_per_suite/1]).
-export([unique_monotonic_integer_white_box/1,
	 unique_integer_white_box/1]).

-include_lib("common_test/include/ct.hrl").

%-define(P(V), V).
-define(P(V), print_ret_val(?FILE, ?LINE, V)).

-define(PRINT(V), print_ret_val(?FILE, ?LINE, V)).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 4}}].

all() -> 
    [unique_monotonic_integer_white_box,
     unique_integer_white_box].

init_per_suite(Config) ->
    erts_debug:set_internal_state(available_internal_state, true),
    Config.

end_per_suite(_Config) ->
    erts_debug:set_internal_state(available_internal_state, false),
    ok.

%%
%%
%% Unique counter white box test case
%%
%%

unique_monotonic_integer_white_box(Config) when is_list(Config) ->
    {ok, Node} = start_node(Config),
    TestServer = self(),
    Success = make_ref(),
    %% Run this in a separate node, so we don't mess up
    %% the system when moving the strict monotonic counter
    %% around in a non-strict monotonic way...
    Test = spawn(Node,
                 fun () ->
                         unique_monotonic_integer_white_box_test(TestServer, Success)
                 end),
    Mon = erlang:monitor(process, Test),
    receive
        {'DOWN', Mon, process, Test, Error} ->
            ct:fail(Error);
        Success ->
            ok
    end,
    erlang:demonitor(Mon, [flush]),
    stop_node(Node),
    ok.

set_unique_monotonic_integer_state(MinCounter, NextValue) ->
    true = erts_debug:set_internal_state(unique_monotonic_integer_state,
                                         NextValue-MinCounter-1).



unique_monotonic_integer_white_box_test(TestServer, Success) ->
    erts_debug:set_internal_state(available_internal_state, true),

    WordSize = erlang:system_info({wordsize, internal}),
    SmallBits = WordSize*8 - 4,

    MinSmall = -1*(1 bsl (SmallBits-1)),
    MaxSmall = (1 bsl (SmallBits-1))-1,
    %% Make sure we got small sizes correct...
    0 = erts_debug:size(MinSmall),
    false = 0 =:= erts_debug:size(MinSmall-1),
    0 = erts_debug:size(MaxSmall),
    false = 0 =:= erts_debug:size(MaxSmall+1),

    ?PRINT({min_small, MinSmall}),
    ?PRINT({max_small, MaxSmall}),

    MinSint64 = -1*(1 bsl 63),
    MaxSint64 = (1 bsl 63)-1,

    ?PRINT({min_Sint64, MinSint64}),
    ?PRINT({max_Sint64, MaxSint64}),

    MinCounter = erts_debug:get_internal_state(min_unique_monotonic_integer),
    MaxCounter = MinCounter + (1 bsl 64) - 1,

    ?PRINT({min_counter, MinCounter}),
    ?PRINT({max_counter, MaxCounter}),

    case WordSize of
        4 ->
            MinCounter = MinSint64;
        8 ->
            MinCounter = MinSmall
    end,

    StartState = erts_debug:get_internal_state(unique_monotonic_integer_state),

    %% Verify that we get expected results over all internal limits...

    case MinCounter < MinSmall of
        false ->
            8 = WordSize,
            ok;
        true ->
            4 = WordSize,
            ?PRINT(over_min_small),
            set_unique_monotonic_integer_state(MinCounter, MinSmall-2),
            true = (?P(erlang:unique_integer([monotonic])) == MinSmall - 2),
            true = (?P(erlang:unique_integer([monotonic])) == MinSmall - 1),
            true = (?P(erlang:unique_integer([monotonic])) == MinSmall),
            true = (?P(erlang:unique_integer([monotonic]))  == MinSmall + 1),
            true = (?P(erlang:unique_integer([monotonic]))  == MinSmall + 2),
            garbage_collect(),
            ok
    end,

    ?PRINT(over_zero), %% Not really an interesting limit, but...
    set_unique_monotonic_integer_state(MinCounter, -2),
    true = (?P(erlang:unique_integer([monotonic])) == -2),
    true = (?P(erlang:unique_integer([monotonic])) == -1),
    true = (?P(erlang:unique_integer([monotonic])) == 0),
    true = (?P(erlang:unique_integer([monotonic]))  == 1),
    true = (?P(erlang:unique_integer([monotonic]))  == 2),
    garbage_collect(),

    ?PRINT(over_max_small),
    set_unique_monotonic_integer_state(MinCounter, MaxSmall-2),
    true = (?P(erlang:unique_integer([monotonic])) == MaxSmall - 2),
    true = (?P(erlang:unique_integer([monotonic])) == MaxSmall - 1),
    true = (?P(erlang:unique_integer([monotonic])) == MaxSmall),
    true = (?P(erlang:unique_integer([monotonic]))  == MaxSmall + 1),
    true = (?P(erlang:unique_integer([monotonic]))  == MaxSmall + 2),
    garbage_collect(),

    case MaxCounter > MaxSint64 of
        false ->
            4 = WordSize,
            ok;
        true ->
            8 = WordSize,
            ?PRINT(over_max_sint64),
            set_unique_monotonic_integer_state(MinCounter, MaxSint64-2),
            true = (?P(erlang:unique_integer([monotonic])) == MaxSint64 - 2),
            true = (?P(erlang:unique_integer([monotonic])) == MaxSint64 - 1),
            true = (?P(erlang:unique_integer([monotonic])) == MaxSint64),
            true = (?P(erlang:unique_integer([monotonic])) == MaxSint64 + 1),
            true = (?P(erlang:unique_integer([monotonic])) == MaxSint64 + 2),
            garbage_collect()
    end,

    ?PRINT(over_max_min_counter),
    set_unique_monotonic_integer_state(MinCounter, if MaxCounter == MaxSint64 ->
                                                          MaxCounter-2;
                                                      true ->
                                                          MinCounter-3
                                                   end),
    true = (?P(erlang:unique_integer([monotonic])) == MaxCounter - 2),
    true = (?P(erlang:unique_integer([monotonic])) == MaxCounter - 1),
    true = (?P(erlang:unique_integer([monotonic])) == MaxCounter),
    true = (?P(erlang:unique_integer([monotonic])) == MinCounter),
    true = (?P(erlang:unique_integer([monotonic])) == MinCounter + 1),
    true = (?P(erlang:unique_integer([monotonic])) == MinCounter + 2),
    garbage_collect(),

    %% Restore initial state and hope we didn't mess it up for the
    %% system...
    true = erts_debug:set_internal_state(unique_monotonic_integer_state,
                                         StartState),

    TestServer ! Success.

%%
%%
%% Unique integer white box test case
%%
%%

-record(uniqint_info, {min_int,
                       max_int,
                       max_small,
                       schedulers,
                       sched_bits}).

unique_integer_white_box(Config) when is_list(Config) ->
    UinqintInfo = init_uniqint_info(),
    #uniqint_info{min_int = MinInt,
                  max_int = MaxInt,
                  max_small = MaxSmall} = UinqintInfo,
    io:format("****************************************************~n", []),
    io:format("*** Around MIN_UNIQ_INT ~p ***~n", [MinInt]),
    io:format("****************************************************~n", []),
    check_unique_integer_around(MinInt, UinqintInfo),
    io:format("****************************************************~n", []),
    io:format("*** Around 0 ***~n", []),
    io:format("****************************************************~n", []),
    check_unique_integer_around(0, UinqintInfo),
    io:format("****************************************************~n", []),
    io:format("*** Around MAX_SMALL ~p ***~n", [MaxSmall]),
    io:format("****************************************************~n", []),
    check_unique_integer_around(MaxSmall, UinqintInfo),
    io:format("****************************************************~n", []),
    io:format("*** Around 2^64+MIN_UNIQ_INT ~p ***~n", [(1 bsl 64)+MinInt]),
    io:format("****************************************************~n", []),
    check_unique_integer_around((1 bsl 64)+MinInt, UinqintInfo),
    io:format("****************************************************~n", []),
    io:format("*** Around 2^64 ~p~n", [(1 bsl 64)]),
    io:format("****************************************************~n", []),
    check_unique_integer_around((1 bsl 64), UinqintInfo),
    io:format("****************************************************~n", []),
    io:format("*** Around 2^64-MIN_UNIQ_INT ~p ***~n", [(1 bsl 64)-MinInt]),
    io:format("****************************************************~n", []),
    check_unique_integer_around((1 bsl 64)-MinInt, UinqintInfo),
    io:format("****************************************************~n", []),
    io:format("*** Around MAX_UNIQ_INT ~p ***~n", [MaxInt]),
    io:format("****************************************************~n", []),
    check_unique_integer_around(MaxInt, UinqintInfo),
    ok.


%%% Internal unique_integer_white_box/1 test case

calc_sched_bits(NoScheds, Shift) when NoScheds < 1 bsl Shift ->
    Shift;
calc_sched_bits(NoScheds, Shift) ->
    calc_sched_bits(NoScheds, Shift+1).

schedulers() ->
    S = erlang:system_info(schedulers),
    try
        DCPUS = erlang:system_info(dirty_cpu_schedulers),
        DIOS = erlang:system_info(dirty_io_schedulers),
        S+DCPUS+DIOS
    catch
        _ : _ ->
            S
    end.

init_uniqint_info() ->
    SmallBits = erlang:system_info({wordsize, internal})*8-4,
    io:format("SmallBits=~p~n", [SmallBits]),
    Schedulers = schedulers(),
    io:format("Schedulers=~p~n", [Schedulers]),
    MinSmall = -1*(1 bsl (SmallBits-1)),
    io:format("MinSmall=~p~n", [MinSmall]),
    MaxSmall = (1 bsl (SmallBits-1))-1,
    io:format("MaxSmall=~p~n", [MaxSmall]),
    SchedBits = calc_sched_bits(Schedulers, 0),
    io:format("SchedBits=~p~n", [SchedBits]),
    MaxInt = ((((1 bsl 64) - 1) bsl SchedBits) bor Schedulers) + MinSmall,
    io:format("MaxInt=~p~n", [MaxInt]),
    #uniqint_info{min_int = MinSmall,
                  max_int = MaxInt,
                  max_small = MaxSmall,
                  schedulers = Schedulers,
                  sched_bits = SchedBits}.

valid_uniqint(Int, #uniqint_info{min_int = MinInt} = UinqintInfo) when Int < MinInt ->
    valid_uniqint(MinInt, UinqintInfo);
valid_uniqint(Int, #uniqint_info{min_int = MinInt,
                                 sched_bits = SchedBits,
                                 schedulers = Scheds}) ->
    Int1 = Int - MinInt,
    {Inc, ThreadNo} = case Int1 band ((1 bsl SchedBits) - 1) of
                          TN when TN > Scheds ->
                              {1, Scheds};
                          TN ->
                              {0, TN}
                      end,
    Counter = ((Int1 bsr SchedBits) + Inc) rem (1 bsl 64),
    ((Counter bsl SchedBits) bor ThreadNo) + MinInt.

smaller_valid_uniqint(Int, UinqintInfo) ->
    Cand = Int-1,
    case valid_uniqint(Cand, UinqintInfo) of
        RI when RI < Int ->
            RI;
        _ ->
            smaller_valid_uniqint(Cand, UinqintInfo)
    end.

mk_uniqint(Int, #uniqint_info {min_int = MinInt,
                               sched_bits = SchedBits} = _UinqintInfo) ->
    Int1 = Int - MinInt,
    ThrId = Int1 band ((1 bsl SchedBits) - 1),
    Value = (Int1 bsr SchedBits) band ((1 bsl 64) - 1),
    0 = Int1 bsr (SchedBits + 64),
    Make = {make_unique_integer, ThrId, Value},
    %% erlang:display(Make),
    Res = erts_debug:get_internal_state(Make),
    %% erlang:display({uniq_int, Res}),
    Res.

check_uniqint(Int, UinqintInfo) ->
    UniqInt = mk_uniqint(Int, UinqintInfo),
    io:format("UniqInt=~p ", [UniqInt]),
    case UniqInt =:= Int of
        true ->
            io:format("OK~n~n", []);
        false ->
            io:format("result Int=~p FAILED~n", [Int]),
            exit(badres)
    end.

check_unique_integer_around(Int, #uniqint_info{min_int = MinInt,
                                               max_int = MaxInt} = UinqintInfo) ->
    {Start, End} = case {Int =< MinInt+100, Int >= MaxInt-100} of
                       {true, false} ->
                           {MinInt, MinInt+100};
                       {false, false} ->
                           {smaller_valid_uniqint(Int-100, UinqintInfo),
                            valid_uniqint(Int+100, UinqintInfo)};
                       {false, true} ->
                           {MaxInt-100, MaxInt}
                   end,
    lists:foldl(fun (I, OldRefInt) ->
                        RefInt = valid_uniqint(I, UinqintInfo),
                        case OldRefInt =:= RefInt of
                            true ->
                                ok;
                            false ->
                                check_uniqint(RefInt, UinqintInfo)
                        end,
                        RefInt
                end,
                none,
                lists:seq(Start, End)).


%% helpers

print_ret_val(File, Line, Value) ->    
    io:format("~s:~p: ~p~n", [File, Line, Value]),
    Value.

start_node(Config) ->
    start_node(Config, []).
start_node(Config, Opts) when is_list(Config), is_list(Opts) ->
    Pa = filename:dirname(code:which(?MODULE)),
    A = erlang:monotonic_time(1) + erlang:time_offset(1),
    B = erlang:unique_integer([positive]),
    Name = list_to_atom(atom_to_list(?MODULE)
                        ++ "-"
                        ++ atom_to_list(proplists:get_value(testcase, Config))
                        ++ "-"
                        ++ integer_to_list(A)
                        ++ "-"
                        ++ integer_to_list(B)),
    test_server:start_node(Name, slave, [{args, Opts++" -pa "++Pa}]).

stop_node(Node) ->
    test_server:stop_node(Node).
