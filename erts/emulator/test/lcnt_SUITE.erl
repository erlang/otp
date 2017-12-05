%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2017. All Rights Reserved.
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

-module(lcnt_SUITE).

-include_lib("common_test/include/ct.hrl").

-export(
    [all/0, suite/0,
     init_per_suite/1, end_per_suite/1,
     init_per_testcase/2, end_per_testcase/2]).

-export(
    [toggle_lock_counting/1, error_on_invalid_category/1, preserve_locks/1,
     registered_processes/1, registered_db_tables/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {seconds, 10}}].

all() ->
    [toggle_lock_counting, error_on_invalid_category, preserve_locks,
     registered_processes, registered_db_tables].

init_per_suite(Config) ->
    case erlang:system_info(lock_counting) of
        true ->
            %% The tests will run straight over these properties, so we have to
            %% preserve them to avoid tainting the other tests.
            OldCopySave = erts_debug:lcnt_control(copy_save),
            OldMask = erts_debug:lcnt_control(mask),
            [{lcnt_SUITE, {OldCopySave, OldMask}} | Config];
        _ ->
            {skip, "Lock counting is not enabled"}
    end.

end_per_suite(Config) ->
    {OldCopySave, OldMask} = proplists:get_value(lcnt_SUITE, Config),

    erts_debug:lcnt_control(copy_save, OldCopySave),
    OldCopySave = erts_debug:lcnt_control(copy_save),

    erts_debug:lcnt_control(mask, OldMask),
    OldMask = erts_debug:lcnt_control(mask),

    erts_debug:lcnt_clear(),
    ok.

init_per_testcase(_Case, Config) ->
    disable_lock_counting(),
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

disable_lock_counting() ->
    ok = erts_debug:lcnt_control(copy_save, false),
    ok = erts_debug:lcnt_control(mask, []),
    ok = erts_debug:lcnt_clear(),

    %% Sanity check.
    false = erts_debug:lcnt_control(copy_save),
    [] = erts_debug:lcnt_control(mask),

    %% The above commands rely on some lazy operations, so we'll have to wait
    %% for the list to clear.
    ok = wait_for_empty_lock_list().

wait_for_empty_lock_list() ->
    wait_for_empty_lock_list(10).
wait_for_empty_lock_list(Tries) when Tries > 0 ->
    try_flush_cleanup_ops(),
    case erts_debug:lcnt_collect() of
        [{duration, _}, {locks, []}] ->
            ok;
        _ ->
            timer:sleep(50),
            wait_for_empty_lock_list(Tries - 1)
    end;
wait_for_empty_lock_list(0) ->
    ct:fail("Lock list failed to clear after disabling lock counting.").

%% Queue up a lot of thread progress cleanup ops in a vain attempt to
%% flush the lock list.
try_flush_cleanup_ops() ->
    false = lists:member(process, erts_debug:lcnt_control(mask)),
    [spawn(fun() -> ok end) || _ <- lists:seq(1, 1000)].

%%
%% Test cases
%%

toggle_lock_counting(Config) when is_list(Config) ->
    Categories =
        [allocator, db, debug, distribution, generic, io, process, scheduler],
    lists:foreach(
        fun(Category) ->
            Locks = get_lock_info_for(Category),
            if
                Locks =/= [] ->
                    disable_lock_counting();
                Locks =:= [] ->
                    ct:fail("Failed to toggle ~p locks.", [Category])
            end
        end, Categories).

get_lock_info_for(Categories) when is_list(Categories) ->
    ok = erts_debug:lcnt_control(mask, Categories),
    [{duration, _}, {locks, Locks}] = erts_debug:lcnt_collect(),
    Locks;

get_lock_info_for(Category) when is_atom(Category) ->
    get_lock_info_for([Category]).

preserve_locks(Config) when is_list(Config) ->
    erts_debug:lcnt_control(mask, [process]),

    erts_debug:lcnt_control(copy_save, true),
    [spawn(fun() -> ok end) || _ <- lists:seq(1, 1000)],

    %% Wait for the processes to be fully destroyed before disabling copy_save,
    %% then remove all active locks from the list. (There's no foolproof method
    %% to do this; sleeping before/after is the best way we have)
    timer:sleep(500),

    erts_debug:lcnt_control(copy_save, false),
    erts_debug:lcnt_control(mask, []),

    try_flush_cleanup_ops(),
    timer:sleep(500),

    case erts_debug:lcnt_collect() of
        [{duration, _}, {locks, Locks}] when length(Locks) > 0 ->
            ct:pal("Preserved ~p locks.", [length(Locks)]);
        [{duration, _}, {locks, []}] ->
            ct:fail("copy_save didn't preserve any locks.")
    end.

error_on_invalid_category(Config) when is_list(Config) ->
    {error, badarg, q_invalid} = erts_debug:lcnt_control(mask, [q_invalid]),
    ok.

registered_processes(Config) when is_list(Config) ->
    %% There ought to be at least one registered process (init/code_server)
    erts_debug:lcnt_control(mask, [process]),
    [_, {locks, ProcLocks}] = erts_debug:lcnt_collect(),
    true = lists:any(
        fun
            ({proc_main, RegName, _, _}) when is_atom(RegName) -> true;
            (_Lock) -> false
        end, ProcLocks),
    ok.

registered_db_tables(Config) when is_list(Config) ->
    %% There ought to be at least one registered table (code)
    erts_debug:lcnt_control(mask, [db]),
    [_, {locks, DbLocks}] = erts_debug:lcnt_collect(),
    true = lists:any(
        fun
            ({db_tab, RegName, _, _}) when is_atom(RegName) -> true;
            (_Lock) -> false
        end, DbLocks),
    ok.
