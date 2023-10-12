%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2022. All Rights Reserved.
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
-module(gen_event_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1,
	 init_per_group/2,end_per_group/2, init_per_testcase/2,
         end_per_testcase/2]).
-export([start/1, add_handler/1, add_sup_handler/1,
	 delete_handler/1, swap_handler/1, swap_sup_handler/1,
	 notify/1, sync_notify/1, call/1, info/1, hibernate/1, auto_hibernate/1,
	 call_format_status/1, call_format_status_anon/1,
         error_format_status/1, get_state/1, replace_state/1,
         start_opt/1,
         undef_init/1, undef_handle_call/1, undef_handle_event/1,
         undef_handle_info/1, undef_code_change/1, undef_terminate/1,
         undef_in_terminate/1, format_log_1/1, format_log_2/1,
         send_request_receive_reqid_collection/1, send_request_wait_reqid_collection/1,
         send_request_check_reqid_collection/1]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [start, {group, test_all}, hibernate, auto_hibernate,
     call_format_status, call_format_status_anon, error_format_status,
     get_state, replace_state,
     start_opt, {group, undef_callbacks}, undef_in_terminate,
     format_log_1, format_log_2,
     send_request_receive_reqid_collection, send_request_wait_reqid_collection,
     send_request_check_reqid_collection].

groups() ->
    [{test_all, [],
      [add_handler, add_sup_handler, delete_handler,
       swap_handler, swap_sup_handler, notify, sync_notify,
       call, info]},
     {undef_callbacks, [],
      [undef_init, undef_handle_call, undef_handle_event, undef_handle_info,
       undef_code_change, undef_terminate]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(undef_callbacks, Config) ->
    DataDir = ?config(data_dir, Config),
    Event1 = filename:join(DataDir, "oc_event.erl"),
    {ok, oc_event} = compile:file(Event1),
    Config;
init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(Case, Config) when Case == undef_handle_call;
                                     Case == undef_handle_info;
                                     Case == undef_handle_event;
                                     Case == undef_code_change;
                                     Case == undef_terminate ->
    {ok, Pid} = oc_event:start(),
    [{event_pid, Pid}|Config];
init_per_testcase(undef_init, Config) ->
    {ok, Pid} = gen_event:start({local, oc_init_event}),
    [{event_pid, Pid}|Config];
init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(Case, Config) when Case == undef_init;
                                    Case == undef_handle_call;
                                    Case == undef_handle_info;
                                    Case == undef_handle_event;
                                    Case == undef_code_change;
                                    Case == undef_terminate ->
    Pid = ?config(event_pid, Config),
    gen_event:stop(Pid);
end_per_testcase(_Case, _Config) ->
    ok.

%% --------------------------------------
%% Start an event manager.
%% --------------------------------------

-define(LMGR, {local, my_dummy_name}).
-define(GMGR, {global, my_dummy_name}).

start(Config) when is_list(Config) ->
    OldFl = process_flag(trap_exit, true),

    dummy_via:reset(),

    {ok, Pid0} = gen_event:start(), %anonymous
    [] = gen_event:which_handlers(Pid0),
    ok = gen_event:stop(Pid0),

    {ok, Pid1} = gen_event:start_link(), %anonymous
    [] = gen_event:which_handlers(Pid1),
    ok = gen_event:stop(Pid1),

    {ok, {Pid1b,Mon1b}} = gen_event:start_monitor(), %anonymous
    [] = gen_event:which_handlers(Pid1b),
    ok = gen_event:stop(Pid1b),
    receive {'DOWN',Mon1b,process,Pid1b,_} -> ok end,

    {ok, Pid2} = gen_event:start(?LMGR),
    [] = gen_event:which_handlers(my_dummy_name),
    [] = gen_event:which_handlers(Pid2),
    ok = gen_event:stop(my_dummy_name),

    {ok, Pid3} = gen_event:start_link(?LMGR),
    [] = gen_event:which_handlers(my_dummy_name),
    [] = gen_event:which_handlers(Pid3),
    ok = gen_event:stop(my_dummy_name),

    {ok, {Pid3b,Mon3b}} = gen_event:start_monitor(?LMGR),
    [] = gen_event:which_handlers(my_dummy_name),
    [] = gen_event:which_handlers(Pid3b),
    ok = gen_event:stop(my_dummy_name),
    receive {'DOWN',Mon3b,process,Pid3b,_} -> ok end,

    {ok, Pid4} = gen_event:start_link(?GMGR),
    [] = gen_event:which_handlers(?GMGR),
    [] = gen_event:which_handlers(Pid4),
    ok = gen_event:stop(?GMGR),

    {ok, {Pid4b,Mon4b}} = gen_event:start_monitor(?GMGR),
    [] = gen_event:which_handlers(?GMGR),
    [] = gen_event:which_handlers(Pid4b),
    ok = gen_event:stop(?GMGR),
    receive {'DOWN',Mon4b,process,Pid4b,_} -> ok end,

    {ok, Pid5} = gen_event:start_link({via, dummy_via, my_dummy_name}),
    [] = gen_event:which_handlers({via, dummy_via, my_dummy_name}),
    [] = gen_event:which_handlers(Pid5),
    ok = gen_event:stop({via, dummy_via, my_dummy_name}),

    {ok, {Pid5b,Mon5b}} = gen_event:start_monitor({via, dummy_via, my_dummy_name}),
    [] = gen_event:which_handlers({via, dummy_via, my_dummy_name}),
    [] = gen_event:which_handlers(Pid5b),
    ok = gen_event:stop({via, dummy_via, my_dummy_name}),
    receive {'DOWN',Mon5b,process,Pid5b,_} -> ok end,

    {ok, _} = gen_event:start_link(?LMGR),
    {error, {already_started, _}} = gen_event:start_link(?LMGR),
    {error, {already_started, _}} = gen_event:start(?LMGR),
    ok = gen_event:stop(my_dummy_name),

    {ok, {Pid5c,Mon5c}} = gen_event:start_monitor(?LMGR),
    {error, {already_started, Pid5c}} = gen_event:start_monitor(?LMGR),
    {error, {already_started, Pid5c}} = gen_event:start(?LMGR),
    ok = gen_event:stop(my_dummy_name),
    receive {'DOWN',Mon5c,process,Pid5c,_} -> ok end,

    {ok, Pid6} = gen_event:start_link(?GMGR),
    {error, {already_started, _}} = gen_event:start_link(?GMGR),
    {error, {already_started, _}} = gen_event:start(?GMGR),

    ok = gen_event:stop(?GMGR, shutdown, 10000),
    receive
	{'EXIT', Pid6, shutdown} -> ok
    after 10000 ->
	    ct:fail(exit_gen_event)
    end,

    {ok, {Pid6b,Mon6b}} = gen_event:start_monitor(?GMGR),
    {error, {already_started, _}} = gen_event:start_monitor(?GMGR),
    {error, {already_started, _}} = gen_event:start(?GMGR),

    ok = gen_event:stop(?GMGR, shutdown, 10000),
    receive
	{'DOWN', Mon6b, process, Pid6b, shutdown} -> ok
    after 10000 ->
	    ct:fail(exit_gen_event)
    end,

    {ok, Pid7} = gen_event:start_link({via, dummy_via, my_dummy_name}),
    {error, {already_started, _}} = gen_event:start_link({via, dummy_via, my_dummy_name}),
    {error, {already_started, _}} = gen_event:start({via, dummy_via, my_dummy_name}),

    exit(Pid7, shutdown),
    receive
	{'EXIT', Pid7, shutdown} -> ok
    after 10000 ->
	    ct:fail(exit_gen_event)
    end,

    {ok, {Pid7b,Mon7b}} = gen_event:start_monitor({via, dummy_via, my_dummy_name}),
    {error, {already_started, _}} = gen_event:start_monitor({via, dummy_via, my_dummy_name}),
    {error, {already_started, _}} = gen_event:start({via, dummy_via, my_dummy_name}),

    exit(Pid7b, shutdown),
    receive
	{'DOWN', Mon7b, process, Pid7b, shutdown} -> ok
    after 10000 ->
	    ct:fail(exit_gen_event)
    end,

    process_flag(trap_exit, OldFl),
    ok.

start_opt(Config) when is_list(Config) ->
    OldFl = process_flag(trap_exit, true),

    dummy_via:reset(),

    {ok, Pid0} = gen_event:start([]), %anonymous
    [] = gen_event:which_handlers(Pid0),
    ok = gen_event:stop(Pid0),

    {ok, Pid1} = gen_event:start_link([]), %anonymous
    [] = gen_event:which_handlers(Pid1),
    ok = gen_event:stop(Pid1),

    {ok, Pid2} = gen_event:start(?LMGR, []),
    [] = gen_event:which_handlers(my_dummy_name),
    [] = gen_event:which_handlers(Pid2),
    ok = gen_event:stop(my_dummy_name),

    {ok, Pid3} = gen_event:start_link(?LMGR, []),
    [] = gen_event:which_handlers(my_dummy_name),
    [] = gen_event:which_handlers(Pid3),
    ok = gen_event:stop(my_dummy_name),

    {ok, Pid4} = gen_event:start_link(?GMGR, []),
    [] = gen_event:which_handlers(?GMGR),
    [] = gen_event:which_handlers(Pid4),
    ok = gen_event:stop(?GMGR),

    {ok, Pid5} = gen_event:start_link({via, dummy_via, my_dummy_name}, []),
    [] = gen_event:which_handlers({via, dummy_via, my_dummy_name}),
    [] = gen_event:which_handlers(Pid5),
    ok = gen_event:stop({via, dummy_via, my_dummy_name}),

    {ok, _} = gen_event:start_link(?LMGR, []),
    {error, {already_started, _}} = gen_event:start_link(?LMGR, []),
    {error, {already_started, _}} = gen_event:start(?LMGR, []),
    ok = gen_event:stop(my_dummy_name),

    {ok, Pid7} = gen_event:start_link(?GMGR),
    {error, {already_started, _}} = gen_event:start_link(?GMGR, []),
    {error, {already_started, _}} = gen_event:start(?GMGR, []),

    ok = gen_event:stop(?GMGR, shutdown, 10000),
    receive
	{'EXIT', Pid7, shutdown} -> ok
    after 10000 ->
	    ct:fail(exit_gen_event)
    end,

    {ok, Pid8} = gen_event:start_link({via, dummy_via, my_dummy_name}),
    {error, {already_started, _}} = gen_event:start_link({via, dummy_via, my_dummy_name}, []),
    {error, {already_started, _}} = gen_event:start({via, dummy_via, my_dummy_name}, []),

    exit(Pid8, shutdown),
    receive
	{'EXIT', Pid8, shutdown} -> ok
    after 10000 ->
	    ct:fail(exit_gen_event)
    end,

    %% test spawn_opt
    MinHeapSz = 10000,
    {ok, Pid9} = gen_event:start_link(?LMGR, [{spawn_opt, [{min_heap_size, MinHeapSz}]}]),
    {error, {already_started, _}} = gen_event:start_link(?LMGR, []),
    {error, {already_started, _}} = gen_event:start(?LMGR, []),
    {heap_size, HeapSz} = erlang:process_info(Pid9, heap_size),
    true = HeapSz > MinHeapSz,
    ok = gen_event:stop(my_dummy_name),

    %% test debug opt
    {ok, _} = gen_event:start_link(?LMGR, [{debug,[debug]}]),
    {error, {already_started, _}} = gen_event:start_link(?LMGR, []),
    {error, {already_started, _}} = gen_event:start(?LMGR, []),
    ok = gen_event:stop(my_dummy_name),

    process_flag(trap_exit, OldFl),
    ok.

hibernate(Config) when is_list(Config) ->
    {ok,Pid} = gen_event:start({local, my_dummy_handler}),
    ok = gen_event:add_handler(my_dummy_handler, dummy_h, [self()]),
    [dummy_h] = gen_event:which_handlers(my_dummy_handler),
    true = gen_event:call(my_dummy_handler, dummy_h, hibernate),
    is_in_erlang_hibernate(Pid),

    Pid ! wake,
    is_not_in_erlang_hibernate(Pid),
    later = gen_event:call(my_dummy_handler, dummy_h, hibernate_later),
    true = ({current_function,{erlang,hibernate,3}} =/=
		erlang:process_info(Pid, current_function)),
    is_in_erlang_hibernate(Pid),

    Pid ! wake,
    is_not_in_erlang_hibernate(Pid),
    gen_event:notify(my_dummy_handler, hibernate),
    is_in_erlang_hibernate(Pid),
    gen_event:notify(my_dummy_handler, wakeup),
    is_not_in_erlang_hibernate(Pid),
    gen_event:notify(my_dummy_handler, hibernate),
    is_in_erlang_hibernate(Pid),
    gen_event:sync_notify(my_dummy_handler, wakeup),
    true = ({current_function,{erlang,hibernate,3}} =/=
		erlang:process_info(Pid, current_function)),
    ok = gen_event:sync_notify(my_dummy_handler, hibernate),
    is_in_erlang_hibernate(Pid),

    Pid ! wake,
    is_not_in_erlang_hibernate(Pid),
    ok = gen_event:add_handler(my_dummy_handler, dummy1_h, [self()]),
    [_,_] = gen_event:which_handlers(my_dummy_handler),
    gen_event:notify(my_dummy_handler, hibernate),
    is_in_erlang_hibernate(Pid),
    gen_event:notify(my_dummy_handler, wakeup),
    is_in_erlang_hibernate(Pid),

    Pid ! wake,
    is_not_in_erlang_hibernate(Pid),

    Pid ! gnurf,
    is_in_erlang_hibernate(Pid),

    Pid ! sleep,
    is_in_erlang_hibernate(Pid),

    Pid ! wake,
    is_not_in_erlang_hibernate(Pid),
    ok = gen_event:stop(my_dummy_handler),

    {ok,Pid2} = gen_event:start({local, my_dummy_handler}),
    ok = gen_event:add_handler(my_dummy_handler, dummy_h,
			       [self(),hibernate]),
    is_in_erlang_hibernate(Pid2),
    sys:suspend(my_dummy_handler),
    is_in_erlang_hibernate(Pid2),
    sys:resume(my_dummy_handler),
    is_in_erlang_hibernate(Pid2),

    Pid2 ! wake,
    is_not_in_erlang_hibernate(Pid2),

    ok = gen_event:stop(my_dummy_handler),

    ok.

auto_hibernate(Config) when is_list(Config) ->
    HibernateAfterTimeout = 100,
    State = {auto_hibernate_state},
    {ok,Pid} = gen_event:start({local, auto_hibernate_handler}, [{hibernate_after, HibernateAfterTimeout}]),
    %% After init test
    is_not_in_erlang_hibernate(Pid),
    timer:sleep(HibernateAfterTimeout),
    is_in_erlang_hibernate(Pid),
    ok = gen_event:add_handler(auto_hibernate_handler, dummy_h, [State]),
    %% Get state test
    [{dummy_h,false,State}] = sys:get_state(Pid),
    is_in_erlang_hibernate(Pid),
    %% Call test
    {ok, hejhopp} = gen_event:call(auto_hibernate_handler, dummy_h, hejsan),
    is_not_in_erlang_hibernate(Pid),
    timer:sleep(HibernateAfterTimeout),
    is_in_erlang_hibernate(Pid),
    %% Event test
    ok = gen_event:notify(auto_hibernate_handler, {self(), handle_event}),
    receive
        handled_event ->
            ok
    after 1000 ->
        ct:fail(event)
    end,
    is_not_in_erlang_hibernate(Pid),
    timer:sleep(HibernateAfterTimeout),
    is_in_erlang_hibernate(Pid),
    %% Info test
    Pid ! {self(), handle_info},
    receive
        handled_info ->
            ok
    after 1000 ->
        ct:fail(info)
    end,
    is_not_in_erlang_hibernate(Pid),
    timer:sleep(HibernateAfterTimeout),
    is_in_erlang_hibernate(Pid),
    ok = gen_event:stop(auto_hibernate_handler),
    ok.

is_in_erlang_hibernate(Pid) ->
    receive after 1 -> ok end,
    is_in_erlang_hibernate_1(200, Pid).

is_in_erlang_hibernate_1(0, Pid) ->
    io:format("~p\n", [erlang:process_info(Pid, current_function)]),
    ct:fail(not_in_erlang_hibernate_3);
is_in_erlang_hibernate_1(N, Pid) ->
    {current_function,MFA} = erlang:process_info(Pid, current_function),
    case MFA of
	{erlang,hibernate,3} ->
	    ok;
	_ ->
	    receive after 10 -> ok end,
	    is_in_erlang_hibernate_1(N-1, Pid)
    end.

is_not_in_erlang_hibernate(Pid) ->
    receive after 1 -> ok end,
    is_not_in_erlang_hibernate_1(200, Pid).

is_not_in_erlang_hibernate_1(0, Pid) ->
    io:format("~p\n", [erlang:process_info(Pid, current_function)]),
    ct:fail(not_in_erlang_hibernate_3);
is_not_in_erlang_hibernate_1(N, Pid) ->
    {current_function,MFA} = erlang:process_info(Pid, current_function),
    case MFA of
	{erlang,hibernate,3} ->
	    receive after 10 -> ok end,
	    is_not_in_erlang_hibernate_1(N-1, Pid);
	_ ->
	    ok
    end.


add_handler(Config) when is_list(Config) ->
    {ok,_} = gen_event:start({local, my_dummy_handler}),
    {error, my_error} =
	gen_event:add_handler(my_dummy_handler, dummy_h, make_error),
    [] = gen_event:which_handlers(my_dummy_handler),
    ok = gen_event:add_handler(my_dummy_handler, dummy_h, [self()]),
    [dummy_h] = gen_event:which_handlers(my_dummy_handler),

    {error, my_error} =
	gen_event:add_handler(my_dummy_handler, {dummy_h, self()}, make_error),
    [dummy_h] = gen_event:which_handlers(my_dummy_handler),
    ok = gen_event:add_handler(my_dummy_handler, {dummy_h,self()},
			       [self()]),
    Self = self(),
    [{dummy_h, Self}, dummy_h] =
	gen_event:which_handlers(my_dummy_handler),
    ok = gen_event:stop(my_dummy_handler),
    ok.

add_sup_handler(Config) when is_list(Config) ->
    {ok,Pid} = gen_event:start({local, my_dummy_handler}),
    {error, my_error} =
	gen_event:add_sup_handler(my_dummy_handler, dummy_h, make_error),
    [] = gen_event:which_handlers(my_dummy_handler),
    ok = gen_event:add_sup_handler(my_dummy_handler, dummy_h, [self()]),
    [dummy_h] = gen_event:which_handlers(my_dummy_handler),
    exit(Pid, sup_died),
    ct:sleep(1000),
    [] = gen_event:which_handlers(my_dummy_handler),

    ok = gen_event:add_sup_handler(my_dummy_handler, dummy_h, [self()]),
    [dummy_h] = gen_event:which_handlers(my_dummy_handler),

    {error, my_error} =
	gen_event:add_handler(my_dummy_handler, {dummy_h, self()}, make_error),
    [dummy_h] = gen_event:which_handlers(my_dummy_handler),
    ok = gen_event:add_sup_handler(my_dummy_handler, {dummy_h,self()},
				   [self()]),
    Self = self(),
    [{dummy_h, Self}, dummy_h] =
	gen_event:which_handlers(my_dummy_handler),
    ok = gen_event:stop(my_dummy_handler),

    receive
	{gen_event_EXIT, dummy_h, shutdown} ->
	    ok
    after 1000 ->
	    ct:fail({no,{gen_event_EXIT, dummy_h, shutdown}})
    end,

    receive
	{gen_event_EXIT, {dummy_h,Self}, shutdown} ->
	    ok
    after 1000 ->
	    ct:fail({no,{gen_event_EXIT, {dummy_h,Self},
			 shutdown}})
    end,
    ok.

delete_handler(Config) when is_list(Config) ->
    {ok,_} = gen_event:start({local, my_dummy_handler}),
    ok = gen_event:add_handler(my_dummy_handler, dummy_h, [self()]),
    {error, module_not_found} =
	gen_event:delete_handler(my_dummy_handler, duuuuuuuuumy, []),
    return_hej =
	gen_event:delete_handler(my_dummy_handler, dummy_h, return_hej),
    [] = gen_event:which_handlers(my_dummy_handler),
    ok = gen_event:add_handler(my_dummy_handler, dummy_h, [self()]),
    ok =
	gen_event:delete_handler(my_dummy_handler, dummy_h, []),
    [] = gen_event:which_handlers(my_dummy_handler),

    ok = gen_event:add_handler(my_dummy_handler, {dummy_h,1}, [self()]),
    ok = gen_event:add_handler(my_dummy_handler, {dummy_h,2}, [self()]),
    {error, module_not_found} =
	gen_event:delete_handler(my_dummy_handler, {duuuuuuuuumy,1}, []),
    return_hej =
	gen_event:delete_handler(my_dummy_handler, {dummy_h,1}, return_hej),
    return_hej =
	gen_event:delete_handler(my_dummy_handler, {dummy_h,2}, return_hej),
    [] = gen_event:which_handlers(my_dummy_handler),
    ok = gen_event:add_handler(my_dummy_handler, {dummy_h,2}, [self()]),
    ok =
	gen_event:delete_handler(my_dummy_handler, {dummy_h,2}, []),
    [] = gen_event:which_handlers(my_dummy_handler),

    ok = gen_event:stop(my_dummy_handler),
    ok.

swap_handler(Config) when is_list(Config) ->
    {ok,_} = gen_event:start({local, my_dummy_handler}),
    ok = gen_event:add_handler(my_dummy_handler, dummy_h, [self()]),
    {error, non_existing} =
	gen_event:swap_handler(my_dummy_handler, {faulty_h, swap},
			       {dummy1_h, []}),
    ok =
	gen_event:swap_handler(my_dummy_handler, {dummy_h, swap},
			       {dummy1_h, swap}),
    [dummy1_h] = gen_event:which_handlers(my_dummy_handler),

    ok = gen_event:delete_handler(my_dummy_handler, dummy1_h, []),

    ok = gen_event:add_handler(my_dummy_handler, {dummy_h,3}, [self()]),
    {error, non_existing} =
	gen_event:swap_handler(my_dummy_handler, {faulty_h, swap},
			       {dummy1_h, []}),
    ok =
	gen_event:swap_handler(my_dummy_handler, {{dummy_h,3}, swap},
			       {{dummy1_h,4}, swap}),
    [{dummy1_h,4}] = gen_event:which_handlers(my_dummy_handler),

    ok = gen_event:delete_handler(my_dummy_handler, {dummy1_h,4}, []),

    ok = gen_event:stop(my_dummy_handler),
    ok.

swap_sup_handler(Config) when is_list(Config) ->
    {ok,_} = gen_event:start({local, my_dummy_handler}),
    ok = gen_event:add_sup_handler(my_dummy_handler, dummy_h, [self()]),
    {error, non_existing} =
	gen_event:swap_handler(my_dummy_handler, {faulty_h, swap},
			       {dummy1_h, []}),
    ok =
	gen_event:swap_handler(my_dummy_handler, {dummy_h, swap},
			       {dummy1_h, swap}),
    [dummy1_h] = gen_event:which_handlers(my_dummy_handler),

    ok = gen_event:delete_handler(my_dummy_handler, dummy1_h, []),
    receive
	{gen_event_EXIT, dummy1_h, normal} ->
	    ok
    after 1000 ->
	    ct:fail({no,{gen_event_EXIT, dummy1_h, normal}})
    end,

    ok = gen_event:add_sup_handler(my_dummy_handler, {dummy_h,3},
				   [self()]),
    {error, non_existing} =
	gen_event:swap_sup_handler(my_dummy_handler, {faulty_h, swap},
				   {dummy1_h, []}),
    ok =
	gen_event:swap_sup_handler(my_dummy_handler, {{dummy_h,3}, swap},
				   {{dummy1_h,4}, swap}),
    [{dummy1_h,4}] = gen_event:which_handlers(my_dummy_handler),

    ok = gen_event:delete_handler(my_dummy_handler, {dummy1_h,4}, []),
    receive
	{gen_event_EXIT, {dummy1_h,4}, normal} ->
	    ok
    after 1000 ->
	    ct:fail({no,{gen_event_EXIT, {dummy1_h,4}, normal}})
    end,

    ok = gen_event:stop(my_dummy_handler),
    ok.

notify(Config) when is_list(Config) ->
    {ok,_} = gen_event:start({local, my_dummy_handler}),
    ok = gen_event:add_handler(my_dummy_handler, dummy_h, [self()]),
    Event = {event, self()},
    [dummy_h] = gen_event:which_handlers(my_dummy_handler),
    ok = gen_event:notify(my_dummy_handler, Event),
    receive
	{dummy_h, Event} ->
	    ok
    end,
    ok = gen_event:notify(my_dummy_handler, {swap_event,dummy1_h,swap}),
    [dummy1_h] = gen_event:which_handlers(my_dummy_handler),
    ok = gen_event:notify(my_dummy_handler, Event),
    receive
	{dummy1_h, Event} ->
	    ok
    end,
    ok = gen_event:notify(my_dummy_handler, delete_event),
    receive
	{dummy1_h, removed} ->
	    ok
    end,
    [] = gen_event:which_handlers(my_dummy_handler),
    ok = gen_event:add_handler(my_dummy_handler, dummy_h, [self()]),

    ok = gen_event:notify(my_dummy_handler, error_event),
    receive
	{dummy_h, returned_error} ->
	    ok
    end,
    [] = gen_event:which_handlers(my_dummy_handler),

    %% Handler with id, {Mod,Id}

    ok = gen_event:add_handler(my_dummy_handler, {dummy_h,5}, [self()]),
    [{dummy_h,5}] = gen_event:which_handlers(my_dummy_handler),
    ok = gen_event:notify(my_dummy_handler, Event),
    receive
	{dummy_h, Event} ->
	    ok
    end,
    ok = gen_event:notify(my_dummy_handler,
			  {swap_event, {dummy1_h, 9}, swap}),
    [{dummy1_h,9}] = gen_event:which_handlers(my_dummy_handler),
    ok = gen_event:notify(my_dummy_handler, Event),
    receive
	{dummy1_h, Event} ->
	    ok
    end,
    ok = gen_event:notify(my_dummy_handler, delete_event),
    receive
	{dummy1_h, removed} ->
	    ok
    end,
    [] = gen_event:which_handlers(my_dummy_handler),
    ok = gen_event:add_handler(my_dummy_handler, {dummy_h,a}, [self()]),

    ok = gen_event:notify(my_dummy_handler, error_event),
    receive
	{dummy_h, returned_error} ->
	    ok
    end,
    [] = gen_event:which_handlers(my_dummy_handler),

    %% Supervised handler.

    ok = gen_event:add_sup_handler(my_dummy_handler, dummy_h, [self()]),
    [dummy_h] = gen_event:which_handlers(my_dummy_handler),
    ok = gen_event:notify(my_dummy_handler, Event),
    receive
	{dummy_h, Event} ->
	    ok
    end,

    ok = gen_event:notify(my_dummy_handler, do_crash),
    receive
	{gen_event_EXIT, dummy_h, {'EXIT',_}} ->
	    ok
    end,

    ok = gen_event:add_sup_handler(my_dummy_handler, dummy_h, [self()]),
    ok = gen_event:notify(my_dummy_handler, {swap_event,dummy1_h,swap}),
    [dummy1_h] = gen_event:which_handlers(my_dummy_handler),

    ok = gen_event:notify(my_dummy_handler, do_crash),
    receive
	{gen_event_EXIT, dummy1_h, {'EXIT',_}} ->
	    ok
    end,

    ok = gen_event:add_sup_handler(my_dummy_handler, dummy_h, [self()]),
    ok = gen_event:notify(my_dummy_handler, {swap_event,dummy1_h,swap}),
    [dummy1_h] = gen_event:which_handlers(my_dummy_handler),

    ok = gen_event:notify(my_dummy_handler, delete_event),
    receive
	{dummy1_h, removed} ->
	    ok
    end,

    receive
	{gen_event_EXIT, dummy1_h, normal} ->
	    ok
    end,

    [] = gen_event:which_handlers(my_dummy_handler),

    ok = gen_event:stop(my_dummy_handler),
    ok.

sync_notify(Config) when is_list(Config) ->
    {ok,_} = gen_event:start({local, my_dummy_handler}),
    ok = gen_event:add_handler(my_dummy_handler, dummy_h, [self()]),
    Event = {event, self()},
    [dummy_h] = gen_event:which_handlers(my_dummy_handler),
    ok = gen_event:sync_notify(my_dummy_handler, Event),
    receive
	{dummy_h, Event} ->
	    ok
    end,
    ok = gen_event:sync_notify(my_dummy_handler,
			       {swap_event, dummy1_h, swap}),
    [dummy1_h] = gen_event:which_handlers(my_dummy_handler),
    ok = gen_event:sync_notify(my_dummy_handler, Event),
    receive
	{dummy1_h, Event} ->
	    ok
    end,
    ok = gen_event:sync_notify(my_dummy_handler, delete_event),
    receive
	{dummy1_h, removed} ->
	    ok
    end,
    [] = gen_event:which_handlers(my_dummy_handler),
    ok = gen_event:add_handler(my_dummy_handler, dummy_h, [self()]),

    ok = gen_event:sync_notify(my_dummy_handler, error_event),
    receive
	{dummy_h, returned_error} ->
	    ok
    end,
    [] = gen_event:which_handlers(my_dummy_handler),

    %% Handler with id, {Mod,Id}

    ok = gen_event:add_handler(my_dummy_handler, {dummy_h,5}, [self()]),
    [{dummy_h,5}] = gen_event:which_handlers(my_dummy_handler),
    ok = gen_event:sync_notify(my_dummy_handler, Event),
    receive
	{dummy_h, Event} ->
	    ok
    end,
    ok = gen_event:sync_notify(my_dummy_handler,
			       {swap_event, {dummy1_h, 9}, swap}),
    [{dummy1_h,9}] = gen_event:which_handlers(my_dummy_handler),
    ok = gen_event:sync_notify(my_dummy_handler, Event),
    receive
	{dummy1_h, Event} ->
	    ok
    end,
    ok = gen_event:sync_notify(my_dummy_handler, delete_event),
    receive
	{dummy1_h, removed} ->
	    ok
    end,
    [] = gen_event:which_handlers(my_dummy_handler),
    ok = gen_event:add_handler(my_dummy_handler, {dummy_h,a}, [self()]),

    ok = gen_event:sync_notify(my_dummy_handler, error_event),
    receive
	{dummy_h, returned_error} ->
	    ok
    end,
    [] = gen_event:which_handlers(my_dummy_handler),

    %% Supervised handler.

    ok = gen_event:add_sup_handler(my_dummy_handler, dummy_h, [self()]),
    [dummy_h] = gen_event:which_handlers(my_dummy_handler),
    ok = gen_event:sync_notify(my_dummy_handler, Event),
    receive
	{dummy_h, Event} ->
	    ok
    end,

    ok = gen_event:sync_notify(my_dummy_handler, do_crash),
    receive
	{gen_event_EXIT, dummy_h, {'EXIT',_}} ->
	    ok
    end,

    ok = gen_event:add_sup_handler(my_dummy_handler, dummy_h, [self()]),
    ok = gen_event:sync_notify(my_dummy_handler,
			       {swap_event,dummy1_h,swap}),
    [dummy1_h] = gen_event:which_handlers(my_dummy_handler),

    ok = gen_event:sync_notify(my_dummy_handler, do_crash),
    receive
	{gen_event_EXIT, dummy1_h, {'EXIT',_}} ->
	    ok
    end,

    ok = gen_event:add_sup_handler(my_dummy_handler, dummy_h, [self()]),
    ok = gen_event:sync_notify(my_dummy_handler,
			       {swap_event,dummy1_h,swap}),
    [dummy1_h] = gen_event:which_handlers(my_dummy_handler),

    ok = gen_event:sync_notify(my_dummy_handler, delete_event),
    receive
	{dummy1_h, removed} ->
	    ok
    end,

    receive
	{gen_event_EXIT, dummy1_h, normal} ->
	    ok
    end,

    [] = gen_event:which_handlers(my_dummy_handler),

    ok = gen_event:stop(my_dummy_handler),
    ok.

call(Config) when is_list(Config) ->
    Async = fun(Mgr,H,Req) ->
                    try
                        Promise = gen_event:send_request(Mgr,H,Req),
                        gen_event:wait_response(Promise, infinity)
                    catch _:Reason ->
                            {'did_exit', Reason}
                    end
            end,
    {ok,_} = gen_event:start({local, my_dummy_handler}),
    ok = gen_event:add_handler(my_dummy_handler, dummy_h, [self()]),
    ok = gen_event:add_handler(my_dummy_handler, {dummy_h, 1}, [self()]),
    [{dummy_h, 1}, dummy_h] = gen_event:which_handlers(my_dummy_handler),
    {'EXIT',_} = (catch gen_event:call(non_exist, dummy_h, hejsan)),
    {error, _} = Async(non_exist, dummy_h, hejsan),
    {error, bad_module} = gen_event:call(my_dummy_handler, bad_h, hejsan),
    {error, bad_module} = Async(my_dummy_handler, bad_h, hejsan),

    {ok, hejhopp} = gen_event:call(my_dummy_handler, dummy_h, hejsan),
    {reply, {ok, hejhopp}} = Async(my_dummy_handler, dummy_h, hejsan),
    {ok, hejhopp} = gen_event:call(my_dummy_handler, {dummy_h, 1}, hejsan),
    {reply, {ok, hejhopp}} = Async(my_dummy_handler, {dummy_h, 1}, hejsan),
    {ok, hejhopp} = gen_event:call(my_dummy_handler, {dummy_h, 1}, hejsan),
    {reply, {ok, hejhopp}} = Async(my_dummy_handler, {dummy_h, 1}, hejsan),
    {ok, hejhopp} = gen_event:call(my_dummy_handler, dummy_h, hejsan, 10000),
    {'EXIT', {timeout, _}} =
	(catch gen_event:call(my_dummy_handler, dummy_h, hejsan, 0)),
    flush(),
    P1 = gen_event:send_request(my_dummy_handler, dummy_h, hejsan),
    timeout = gen_event:wait_response(P1, 0),
    {reply, {ok, hejhopp}} = gen_event:wait_response(P1, infinity),

    flush(),
    P2 = gen_event:send_request(my_dummy_handler, dummy_h, hejsan),
    no_reply = gen_event:check_response({other,msg}, P2),
    {reply, {ok, hejhopp}} = receive Msg -> gen_event:check_response(Msg, P2)
                             after 1000 -> exit(tmo) end,

    ok = gen_event:delete_handler(my_dummy_handler, {dummy_h, 1}, []),
    {ok, swapped} = gen_event:call(my_dummy_handler, dummy_h,
				   {swap_call,dummy1_h,swap}),
    [dummy1_h] = gen_event:which_handlers(my_dummy_handler),
    {error, bad_module} = gen_event:call(my_dummy_handler, dummy_h, hejsan),
    {error, bad_module} = Async(my_dummy_handler, dummy_h, hejsan),
    ok = gen_event:call(my_dummy_handler, dummy1_h, delete_call),
    receive
	{dummy1_h, removed} ->
	    ok
    end,
    [] = gen_event:which_handlers(my_dummy_handler),
    ok = gen_event:add_handler(my_dummy_handler, dummy_h, [self()]),

    {error, {return, faulty}} =
	gen_event:call(my_dummy_handler, dummy_h, error_call),
    receive
	{dummy_h, returned_error} ->
	    ok
    end,
    [] = gen_event:which_handlers(my_dummy_handler),
    ok = gen_event:add_handler(my_dummy_handler, dummy_h, [self()]),

    {error, {'EXIT', _}} =
	gen_event:call(my_dummy_handler, dummy_h, exit_call),

    [] = gen_event:which_handlers(my_dummy_handler),

    %% Handler with id, {Mod,Id}

    ok = gen_event:add_handler(my_dummy_handler, {dummy_h,1}, [self()]),
    [{dummy_h,1}] = gen_event:which_handlers(my_dummy_handler),
    {error, bad_module} =
	gen_event:call(my_dummy_handler, bad_h, hejsan),
    {ok, hejhopp} = gen_event:call(my_dummy_handler, {dummy_h,1},
				   hejsan),
    {ok, swapped} = gen_event:call(my_dummy_handler, {dummy_h,1},
				   {swap_call,{dummy1_h,2},swap}),
    [{dummy1_h,2}] = gen_event:which_handlers(my_dummy_handler),
    {error, bad_module} =
	gen_event:call(my_dummy_handler, dummy_h, hejsan),
    ok = gen_event:call(my_dummy_handler, {dummy1_h,2}, delete_call),
    receive
	{dummy1_h, removed} ->
	    ok
    end,
    [] = gen_event:which_handlers(my_dummy_handler),
    ok = gen_event:add_handler(my_dummy_handler, {dummy_h,3}, [self()]),

    {error, {return, faulty}} =
	gen_event:call(my_dummy_handler, {dummy_h,3}, error_call),
    receive
	{dummy_h, returned_error} ->
	    ok
    end,
    [] = gen_event:which_handlers(my_dummy_handler),
    ok = gen_event:add_handler(my_dummy_handler, {dummy_h,4}, [self()]),

    {error, {'EXIT', _}} =
	gen_event:call(my_dummy_handler, {dummy_h,4}, exit_call),

    [] = gen_event:which_handlers(my_dummy_handler),

    %% Supervised handler.

    ok = gen_event:add_sup_handler(my_dummy_handler, dummy_h, [self()]),
    [dummy_h] = gen_event:which_handlers(my_dummy_handler),
    {error, bad_module} =
	gen_event:call(my_dummy_handler, bad_h, hejsan),
    {ok, hejhopp} = gen_event:call(my_dummy_handler, dummy_h, hejsan),
    {ok, swapped} = gen_event:call(my_dummy_handler, dummy_h,
				   {swap_call,dummy1_h,swap}),
    [dummy1_h] = gen_event:which_handlers(my_dummy_handler),
    {error, bad_module} =
	gen_event:call(my_dummy_handler, dummy_h, hejsan),
    ok = gen_event:call(my_dummy_handler, dummy1_h, delete_call),
    receive
	{dummy1_h, removed} ->
	    ok
    end,

    receive
	{gen_event_EXIT, dummy1_h, normal} ->
	    ok
    end,

    [] = gen_event:which_handlers(my_dummy_handler),
    ok = gen_event:add_sup_handler(my_dummy_handler, dummy_h, [self()]),

    {error, {return, faulty}} =
	gen_event:call(my_dummy_handler, dummy_h, error_call),
    receive
	{dummy_h, returned_error} ->
	    ok
    end,

    receive
	{gen_event_EXIT, dummy_h, {return,faulty}} ->
	    ok
    after 1000 ->
	    ct:fail({no, {gen_event_EXIT, dummy_h, {return,faulty}}})
    end,

    [] = gen_event:which_handlers(my_dummy_handler),
    ok = gen_event:add_sup_handler(my_dummy_handler, dummy_h, [self()]),

    {error, {'EXIT', _}} =
	gen_event:call(my_dummy_handler, dummy_h, exit_call),

    receive
	{gen_event_EXIT, dummy_h, {'EXIT',_}} ->
	    ok
    after 1000 ->
	    ct:fail({no, {gen_event_EXIT, dummy_h, {'EXIT','_'}}})
    end,

    [] = gen_event:which_handlers(my_dummy_handler),

    ok = gen_event:stop(my_dummy_handler),
    ok.

flush() ->
    receive M -> [M|flush()] after 0 -> [] end.

info(Config) when is_list(Config) ->
    {ok,_} = gen_event:start({local, my_dummy_handler}),
    ok = gen_event:add_handler(my_dummy_handler, dummy_h, [self()]),
    Info = {info, self()},
    [dummy_h] = gen_event:which_handlers(my_dummy_handler),
    my_dummy_handler ! Info,
    receive
	{dummy_h, Info} ->
	    ok
    end,
    my_dummy_handler ! {swap_info,dummy1_h,swap},
    [dummy1_h] = gen_event:which_handlers(my_dummy_handler),
    my_dummy_handler ! Info,
    receive
	{dummy1_h, Info} ->
	    ok
    end,
    my_dummy_handler ! delete_info,
    receive
	{dummy1_h, removed} ->
	    ok
    end,
    [] = gen_event:which_handlers(my_dummy_handler),
    ok = gen_event:add_handler(my_dummy_handler, dummy_h, [self()]),

    my_dummy_handler ! error_info,
    receive
	{dummy_h, returned_error} ->
	    ok
    end,
    [] = gen_event:which_handlers(my_dummy_handler),

    %% Handler with id, {Mod,Id}

    ok = gen_event:add_handler(my_dummy_handler, {dummy_h,1}, [self()]),
    [{dummy_h,1}] = gen_event:which_handlers(my_dummy_handler),
    my_dummy_handler ! Info,
    receive
	{dummy_h, Info} ->
	    ok
    end,
    my_dummy_handler ! {swap_info,{dummy1_h,2},swap},
    [{dummy1_h,2}] = gen_event:which_handlers(my_dummy_handler),
    my_dummy_handler ! Info,
    receive
	{dummy1_h, Info} ->
	    ok
    end,
    my_dummy_handler ! delete_info,
    receive
	{dummy1_h, removed} ->
	    ok
    end,
    [] = gen_event:which_handlers(my_dummy_handler),
    ok = gen_event:add_handler(my_dummy_handler, {dummy_h,3}, [self()]),

    my_dummy_handler ! error_info,
    receive
	{dummy_h, returned_error} ->
	    ok
    end,
    [] = gen_event:which_handlers(my_dummy_handler),

    %% Supervised handler

    ok = gen_event:add_sup_handler(my_dummy_handler, dummy_h, [self()]),
    [dummy_h] = gen_event:which_handlers(my_dummy_handler),
    my_dummy_handler ! Info,
    receive
	{dummy_h, Info} ->
	    ok
    end,
    my_dummy_handler ! {swap_info,dummy1_h,swap},
    [dummy1_h] = gen_event:which_handlers(my_dummy_handler),
    my_dummy_handler ! Info,
    receive
	{dummy1_h, Info} ->
	    ok
    end,
    my_dummy_handler ! delete_info,
    receive
	{dummy1_h, removed} ->
	    ok
    end,

    receive
	{gen_event_EXIT, dummy1_h, normal} ->
	    ok
    after 1000 ->
	    ct:fail({no, {gen_event_EXIT, dummy1_h, normal}})
    end,

    [] = gen_event:which_handlers(my_dummy_handler),

    ok = gen_event:add_sup_handler(my_dummy_handler, dummy_h, [self()]),

    my_dummy_handler ! error_info,
    receive
	{dummy_h, returned_error} ->
	    ok
    end,

    receive
	{gen_event_EXIT, dummy_h, {return,faulty}} ->
	    ok
    after 1000 ->
	    ct:fail({no, {gen_event_EXIT, dummy_h, {return,faulty}}})
    end,

    ok = gen_event:add_sup_handler(my_dummy_handler, dummy_h, [self()]),
    my_dummy_handler ! do_crash,

    receive
	{gen_event_EXIT, dummy_h, {'EXIT',_}} ->
	    ok
    after 1000 ->
	    ct:fail({no, {gen_event_EXIT, dummy_h, {'EXIT','_'}}})
    end,

    [] = gen_event:which_handlers(my_dummy_handler),

    ok = gen_event:stop(my_dummy_handler),
    ok.

%% Test that sys:get_status/1,2 calls format_status/1,2.
call_format_status(Config) when is_list(Config) ->
    call_format_status(dummy1_h),
    call_format_status(dummy_h);
call_format_status(Module) when is_atom(Module) ->
    {ok, Pid} = gen_event:start({local, my_dummy_handler}),
    %% State here intentionally differs from what we expect from format_status
    State = self(),
    FmtState = "dummy1_h handler state",
    ok = gen_event:add_handler(my_dummy_handler, Module, [State]),
    Status1 = sys:get_status(Pid),
    Status2 = sys:get_status(Pid, 5000),
    ok = gen_event:stop(Pid),
    {status, Pid, _, [_, _, Pid, [], Data1]} = Status1,
    HandlerInfo1 = proplists:get_value(items, Data1),
    {"Installed handlers", [{_,Module,_,FmtState,_}]} = HandlerInfo1,
    {status, Pid, _, [_, _, Pid, [], Data2]} = Status2,
    HandlerInfo2 = proplists:get_value(items, Data2),
    {"Installed handlers", [{_,Module,_,FmtState,_}]} = HandlerInfo2,
    ok.

%% Test that sys:get_status/1,2 calls format_status/2 for anonymous
%% gen_event processes.
call_format_status_anon(Config) when is_list(Config) ->
    {ok, Pid} = gen_event:start(),
    %% The 'Name' of the gen_event process will be a pid() here, so
    %% the next line will crash if format_status can't string-ify pids.
    Status1 = sys:get_status(Pid),
    ok = gen_event:stop(Pid),
    Header = "Status for event handler " ++  pid_to_list(Pid),
    {status, Pid, _, [_, _, Pid, [], Data1]} = Status1,
    Header = proplists:get_value(header, Data1),
    ok.


%% Test that a handler error calls format_status/2.
error_format_status(Config) when is_list(Config) ->
    error_logger_forwarder:register(),
    OldFl = process_flag(trap_exit, true),
    try
        error_format_status(dummy1_h),
        error_format_status(dummy_h)
    after
            process_flag(trap_exit, OldFl),
            error_logger_forwarder:unregister()
    end;
error_format_status(Module) when is_atom(Module) ->
    State = self(),
    {ok, Pid} = gen_event:start({local, my_dummy_handler}),
    ok = gen_event:add_sup_handler(my_dummy_handler, Module, [State]),
    ok = gen_event:notify(my_dummy_handler, do_crash),
    receive
	{gen_event_EXIT,Module,{'EXIT',_}} -> ok
    after 5000 ->
	    ct:fail({exit_gen_event,flush()})
    end,
    FmtState = "dummy1_h handler state",
    receive
	{error,_GroupLeader, {Pid,
			      "** gen_event handler"++_,
			      [Module,my_dummy_handler,do_crash,
			       FmtState, _]}} ->
	    ok;
	Other ->
	    ct:pal("Unexpected: ~p", [Other]),
	    ct:fail(failed)
    after 5000 ->
	    ct:fail({exit_gen_event,flush()})
    end,
    unlink(Pid),
    ok = gen_event:stop(Pid),
    ok.

%% Test that sys:get_state/1,2 return the gen_event state.
get_state(Config) when is_list(Config) ->
    {ok, Pid} = gen_event:start({local, my_dummy_handler}),
    State1 = self(),
    ok = gen_event:add_handler(my_dummy_handler, dummy1_h, [State1]),
    [{dummy1_h,false,State1}] = sys:get_state(Pid),
    [{dummy1_h,false,State1}] = sys:get_state(Pid, 5000),
    State2 = {?MODULE, self()},
    ok = gen_event:add_handler(my_dummy_handler, {dummy1_h,id}, [State2]),
    Result1 = sys:get_state(Pid),
    [{dummy1_h,false,State1},{dummy1_h,id,State2}] = lists:sort(Result1),
    Result2 = sys:get_state(Pid, 5000),
    [{dummy1_h,false,State1},{dummy1_h,id,State2}] = lists:sort(Result2),
    ok = sys:suspend(Pid),
    Result3 = sys:get_state(Pid),
    [{dummy1_h,false,State1},{dummy1_h,id,State2}] = lists:sort(Result3),
    ok = sys:resume(Pid),
    ok = gen_event:stop(Pid),
    ok.

%% Test that replace_state/2,3 replace the gen_event state.
replace_state(Config) when is_list(Config) ->
    {ok, Pid} = gen_event:start({local, my_dummy_handler}),
    State1 = self(),
    ok = gen_event:add_handler(my_dummy_handler, dummy1_h, [State1]),
    [{dummy1_h,false,State1}] = sys:get_state(Pid),
    NState1 = "replaced",
    Replace1 = fun({dummy1_h,false,_}=S) -> setelement(3,S,NState1) end,
    [{dummy1_h,false,NState1}] = sys:replace_state(Pid, Replace1),
    [{dummy1_h,false,NState1}] = sys:get_state(Pid),
    NState2 = "replaced again",
    Replace2 = fun({dummy1_h,false,_}=S) -> setelement(3,S,NState2) end,
    [{dummy1_h,false,NState2}] = sys:replace_state(Pid, Replace2, 5000),
    [{dummy1_h,false,NState2}] = sys:get_state(Pid),
    %% verify no change in state if replace function crashes
    Replace3 = fun(_) -> exit(fail) end,
    [{dummy1_h,false,NState2}] = sys:replace_state(Pid, Replace3),
    [{dummy1_h,false,NState2}] = sys:get_state(Pid),
    %% verify state replaced if process sys suspended
    NState3 = "replaced again and again",
    Replace4 = fun({dummy1_h,false,_}=S) -> setelement(3,S,NState3) end,
    ok = sys:suspend(Pid),
    [{dummy1_h,false,NState3}] = sys:replace_state(Pid, Replace4),
    ok = sys:resume(Pid),
    [{dummy1_h,false,NState3}] = sys:get_state(Pid),
    ok.

%% No default provided for init, so it should fail
undef_init(Config) ->
    Pid = ?config(event_pid, Config),
    {'EXIT', {undef, [{oc_init_event, init, [_], _}|_]}}
        = gen_event:add_handler(Pid, oc_init_event, []),
    ok.

%% No default provided for init, so it should fail
undef_handle_call(Config) when is_list(Config) ->
    Pid = ?config(event_pid, Config),
    {error, {'EXIT', {undef, [{oc_event, handle_call, _, _}|_]}}}
        = gen_event:call(Pid, oc_event, call_msg),
    [] = gen_event:which_handlers(Pid),
    ok.

%% No default provided for init, so it should fail
undef_handle_event(Config) ->
    Pid = ?config(event_pid, Config),
    ok = gen_event:sync_notify(Pid, event_msg),
    [] = gen_event:which_handlers(Pid),

    gen_event:add_handler(oc_event, oc_event, []),
    [oc_event] = gen_event:which_handlers(Pid),

    ok = gen_event:notify(Pid, event_msg),
    [] = gen_event:which_handlers(Pid),
    ok.

%% Defaulting to doing nothing with a log warning.
undef_handle_info(Config) when is_list(Config) ->
    error_logger_forwarder:register(),
    Pid = ?config(event_pid, Config),
    Pid ! hej,
    wait_until_processed(Pid, hej, 10),
    [oc_event] = gen_event:which_handlers(Pid),
    receive
        {warning_msg, _GroupLeader,
         {Pid, "** Undefined handle_info in " ++ _, [oc_event, hej]}} ->
            ok;
        Other ->
            io:format("Unexpected: ~p", [Other]),
            ct:fail(failed)
    end.

wait_until_processed(_Pid, _Message, 0) ->
    ct:fail(not_processed);
wait_until_processed(Pid, Message, N) ->
    {messages, Messages} = erlang:process_info(Pid, messages),
    case lists:member(Message, Messages) of
        true ->
            timer:sleep(100),
            wait_until_processed(Pid, Message, N-1);
        false ->
            ok
    end.

%% No default provided for init, so it should fail
undef_code_change(Config) when is_list(Config) ->
    Pid = ?config(event_pid, Config),
    {error, {'EXIT', {undef, [{oc_event, code_change, [_, _, _], _}|_]}}} =
        fake_upgrade(Pid, oc_event),
    [oc_event] = gen_event:which_handlers(Pid),
    ok.

%% Defaulting to doing nothing. Test that it works when not defined.
undef_terminate(Config) when is_list(Config) ->
    Pid = ?config(event_pid, Config),
    ok = gen_event:delete_handler(Pid, oc_event, []),
    [] = gen_event:which_handlers(Pid),
    ok.

%% Test that the default implementation doesn't catch the wrong undef error
undef_in_terminate(_Config) ->
    {ok, Pid} = gen_event:start({local, dummy}),
    State = {undef_in_terminate, {dummy_h, terminate}},
    ok = gen_event:add_handler(Pid, dummy_h, {state, State}),
    [dummy_h] = gen_event:which_handlers(Pid),
    {'EXIT', {undef, [{dummy_h, terminate, [], []}|_]}}
        = gen_event:delete_handler(Pid, dummy_h, []),
    [] = gen_event:which_handlers(Pid),
    ok.

fake_upgrade(Pid, Mod) ->
    sys:suspend(Pid),
    sys:replace_state(Pid, fun(S) -> {new, S} end),
    Ret = sys:change_code(Pid, Mod, old_vsn, []),
    ok = sys:resume(Pid),
    Ret.

%% Test report callback for Logger handler error_logger
format_log_1(_Config) ->
    FD = application:get_env(kernel, error_logger_format_depth),
    application:unset_env(kernel, error_logger_format_depth),
    Term = lists:seq(1, 15),
    Handler = my_handler,
    Name = self(),
    Report = #{label=>{gen_event,terminate},
               handler=>Handler,
               name=>Name,
               last_message=>Term,
               state=>Term,
               reason=>Term},
    {F1, A1} = gen_event:format_log(Report),
    FExpected1 = "** gen_event handler ~tp crashed.\n"
        "** Was installed in ~tp\n"
        "** Last event was: ~tp\n"
        "** When handler state == ~tp\n"
        "** Reason == ~tp\n",
    ct:log("F1: ~ts~nA1: ~tp", [F1,A1]),
    FExpected1 = F1,
    [Handler,Name,Term,Term,Term] = A1,

    Warning = #{label=>{gen_event,no_handle_info},
                module=>?MODULE,
                message=> Term},
    {WF1,WA1} = gen_event:format_log(Warning),
    WFExpected1 = "** Undefined handle_info in ~p\n"
                  "** Unhandled message: ~tp\n",
    ct:log("WF1: ~ts~nWA1: ~tp", [WF1,WA1]),
    WFExpected1 = WF1,
    [?MODULE,Term] = WA1,
    WarningLimited = [1,2,3,4,5,6,7,8,9,'...'],

    Depth = 10,
    ok = application:set_env(kernel, error_logger_format_depth, Depth),
    Limited = [1,2,3,4,5,6,7,8,'...'],
    LastMsg = ["Last msg" | Term],
    State   = ["State" | Term],
    Reason  = ["Reason" | Term],
    LastMsgLimited = ["Last msg" | Limited],
    StateLimited   = ["State" | Limited],
    ReasonLimited  = ["Reason" | Limited],
    {F2,A2} = gen_event:format_log(#{label=>{gen_event,terminate},
                                     handler=>Handler,
                                     name=>Name,
                                     last_message=>LastMsg,
                                     state=>State,
                                     reason=>Reason}),
    FExpected2 = "** gen_event handler ~tP crashed.\n"
        "** Was installed in ~tP\n"
        "** Last event was: ~tP\n"
        "** When handler state == ~tP\n"
        "** Reason == ~tP\n",
    ct:log("F2: ~ts~nA2: ~tp", [F2,A2]),
    FExpected2 = F2,
    [Handler,Depth,Name,Depth,LastMsgLimited,Depth,StateLimited,Depth,ReasonLimited,Depth] = A2,

    {WF2,WA2} = gen_event:format_log(Warning),
    WFExpected2 = "** Undefined handle_info in ~p\n"
                  "** Unhandled message: ~tP\n",
    ct:log("WF2: ~ts~nWA2: ~tp", [WF2,WA2]),
    WFExpected2 = WF2,
    ct:log("WF2: ~tp~nWA2: ~tp", [[?MODULE,WarningLimited,Depth], WA2]),
    [?MODULE,WarningLimited,Depth] = WA2,

    case FD of
        undefined ->
            application:unset_env(kernel, error_logger_format_depth);
        _ ->
            application:set_env(kernel, error_logger_format_depth, FD)
    end,
    ok.

%% Test report callback for any Logger handler
format_log_2(_Config) ->
    Term = lists:seq(1, 15),
    Handler = my_handler,
    Name = self(),
    NameStr = pid_to_list(Name),
    Report = #{label=>{gen_event,terminate},
               handler=>Handler,
               name=>Name,
               last_message=>Term,
               state=>Term,
               reason=>Term},
    FormatOpts1 = #{},
    Str1 = flatten_format_log(Report, FormatOpts1),
    L1 = length(Str1),
    Expected1 = "** gen_event handler my_handler crashed.\n"
        "** Was installed in "++NameStr++"\n"
        "** Last event was: [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]\n"
        "** When handler state == [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]\n"
        "** Reason == [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]\n",
    ct:log("Str1: ~ts", [Str1]),
    ct:log("length(Str1): ~p", [L1]),
    true = Expected1 =:= Str1,

    Warning = #{label=>{gen_event,no_handle_info},
                module=>?MODULE,
                message=>Term},
    WStr1 = flatten_format_log(Warning, FormatOpts1),
    WL1 = length(WStr1),
    WExpected1 = "** Undefined handle_info in gen_event_SUITE\n"
        "** Unhandled message: [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]\n",
    ct:log("WStr1: ~ts", [WStr1]),
    ct:log("length(WStr1): ~p", [WL1]),
    true = WExpected1 =:= WStr1,

    Depth = 10,
    FormatOpts2 = #{depth=>Depth},
    Str2 = flatten_format_log(Report, FormatOpts2),
    L2 = length(Str2),
    Expected2 = "** gen_event handler my_handler crashed.\n"
        "** Was installed in " ++ NameStr ++ "\n"
        "** Last event was: [1,2,3,4,5,6,7,8,9|...]\n"
        "** When handler state == [1,2,3,4,5,6,7,8,9|...]\n"
        "** Reason == [1,2,3,4,5,6,7,8,9|...]\n",
    ct:log("Str2: ~ts", [Str2]),
    ct:log("length(Str2): ~p", [L2]),
    true = Expected2 =:= Str2,

    WStr2 = flatten_format_log(Warning, FormatOpts2),
    WL2 = length(WStr2),
    WExpected2 = "** Undefined handle_info in gen_event_SUITE\n"
        "** Unhandled message: [1,2,3,4,5,6,7,8,9|...]\n",
    ct:log("WStr2: ~ts", [WStr2]),
    ct:log("length(WStr2): ~p", [WL2]),
    true = WExpected2 =:= WStr2,

    FormatOpts3 = #{chars_limit=>200},
    Str3 = flatten_format_log(Report, FormatOpts3),
    L3 = length(Str3),
    Expected3 = "** gen_event handler my_handler crashed.\n"
                "** Was installed",
    ct:log("Str3: ~ts", [Str3]),
    ct:log("length(Str3): ~p", [L3]),
    true = lists:prefix(Expected3, Str3),
    true = L3 < L1,

    WFormatOpts3 = #{chars_limit=>80},
    WStr3 = flatten_format_log(Warning, WFormatOpts3),
    WL3 = length(WStr3),
    WExpected3 = "** Undefined handle_info in gen_event_SUITE\n"
        "** Unhandled message: ",
    ct:log("WStr3: ~ts", [WStr3]),
    ct:log("length(WStr3): ~p", [WL3]),
    true = lists:prefix(WExpected3, WStr3),
    true = WL3 < WL1,

    FormatOpts4 = #{single_line=>true},
    Str4 = flatten_format_log(Report, FormatOpts4),
    L4 = length(Str4),

    Expected4 = "Generic event handler my_handler crashed. "
        "Installed: "++NameStr++". "
        "Last event: [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]. "
        "State: [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]. "
        "Reason: [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15].",
    ct:log("Str4: ~ts", [Str4]),
    ct:log("length(Str4): ~p", [L4]),
    true = Expected4 =:= Str4,

    WStr4 = flatten_format_log(Warning, FormatOpts4),
    WL4 = length(WStr4),
    WExpected4 = "Undefined handle_info in gen_event_SUITE. "
        "Unhandled message: [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15].",
    ct:log("WStr4: ~ts", [WStr4]),
    ct:log("length(WStr4): ~p", [WL4]),
    true = WExpected4 =:= WStr4,

    FormatOpts5 = #{single_line=>true, depth=>Depth},
    Str5 = flatten_format_log(Report, FormatOpts5),
    L5 = length(Str5),
    Expected5 = "Generic event handler my_handler crashed. "
        "Installed: "++NameStr++". "
        "Last event: [1,2,3,4,5,6,7,8,9|...]. "
        "State: [1,2,3,4,5,6,7,8,9|...]. "
        "Reason: [1,2,3,4,5,6,7,8,9|...].",
    ct:log("Str5: ~ts", [Str5]),
    ct:log("length(Str5): ~p", [L5]),
    true = Expected5 =:= Str5,

    WStr5 = flatten_format_log(Warning, FormatOpts5),
    WL5 = length(WStr5),
    WExpected5 = "Undefined handle_info in gen_event_SUITE. "
        "Unhandled message: [1,2,3,4,5,6,7,8,9|...].",
    ct:log("WStr5: ~ts", [WStr5]),
    ct:log("length(WStr5): ~p", [WL5]),
    true = WExpected5 =:= WStr5,

    FormatOpts6 = #{single_line=>true, chars_limit=>200},
    Str6 = flatten_format_log(Report, FormatOpts6),
    L6 = length(Str6),
    Expected6 = "Generic event handler my_handler crashed. Installed: ",
    ct:log("Str6: ~ts", [Str6]),
    ct:log("length(Str6): ~p", [L6]),
    true = lists:prefix(Expected6, Str6),
    true = L6 < L4,

    WFormatOpts6 = #{single_line=>true, chars_limit=>80},
    WStr6 = flatten_format_log(Warning, WFormatOpts6),
    WL6 = length(WStr6),
    WExpected6 = "Undefined handle_info in gen_event_SUITE. ",
    ct:log("WStr6: ~ts", [WStr6]),
    ct:log("length(WStr6): ~p", [WL6]),
    true = lists:prefix(WExpected6, WStr6),
    true = WL6 < WL4,

    ok.

flatten_format_log(Report, Format) ->
    lists:flatten(gen_event:format_log(Report, Format)).


send_request_receive_reqid_collection(Config) when is_list(Config) ->
    {ok, Pid1} = gen_event:start(),
    ok = gen_event:add_handler(Pid1, dummy_h, [self()]),
    [dummy_h] = gen_event:which_handlers(Pid1),
    {ok, Pid2} = gen_event:start(),
    ok = gen_event:add_handler(Pid2, dummy_h, [self()]),
    [dummy_h] = gen_event:which_handlers(Pid2),
    {ok, Pid3} = gen_event:start(),
    ok = gen_event:add_handler(Pid3, dummy_h, [self()]),
    [dummy_h] = gen_event:which_handlers(Pid3),
    send_request_receive_reqid_collection(Pid1, Pid2, Pid3),
    send_request_receive_reqid_collection_timeout(Pid1, Pid2, Pid3),
    send_request_receive_reqid_collection_error(Pid1, Pid2, Pid3),
    ok = gen_event:stop(Pid1),
    try gen_event:stop(Pid2) catch exit:noproc -> ok end,
    ok = gen_event:stop(Pid3).

send_request_receive_reqid_collection(Pid1, Pid2, Pid3) ->

    ReqId0 = gen_event:send_request(Pid1, dummy_h, hejsan),

    ReqIdC0 = gen_event:reqids_new(),

    ReqId1 = gen_event:send_request(Pid1, dummy_h, {delayed_answer,400}),
    ReqIdC1 = gen_event:reqids_add(ReqId1, req1, ReqIdC0),
    1 = gen_event:reqids_size(ReqIdC1),

    ReqIdC2 = gen_event:send_request(Pid2, dummy_h, {delayed_answer,1}, req2, ReqIdC1),
    2 = gen_event:reqids_size(ReqIdC2),

    ReqIdC3 = gen_event:send_request(Pid3, dummy_h, {delayed_answer,200}, req3, ReqIdC2),
    3 = gen_event:reqids_size(ReqIdC3),
        
    {{reply, delayed}, req2, ReqIdC4} = gen_event:receive_response(ReqIdC3, infinity, true),
    2 = gen_event:reqids_size(ReqIdC4),

    {{reply, delayed}, req3, ReqIdC5} = gen_event:receive_response(ReqIdC4, 5678, true),
    1 = gen_event:reqids_size(ReqIdC5),

    {{reply, delayed}, req1, ReqIdC6} = gen_event:receive_response(ReqIdC5, 5000, true),
    0 = gen_event:reqids_size(ReqIdC6),

    no_request = gen_event:receive_response(ReqIdC6, 5000, true),

    {reply, {ok, hejhopp}} = gen_event:receive_response(ReqId0, infinity),

    ok.

send_request_receive_reqid_collection_timeout(Pid1, Pid2, Pid3) ->

    ReqId0 = gen_event:send_request(Pid1, dummy_h, hejsan),

    ReqIdC0 = gen_event:reqids_new(),

    ReqId1 = gen_event:send_request(Pid1, dummy_h, {delayed_answer,1000}),
    ReqIdC1 = gen_event:reqids_add(ReqId1, req1, ReqIdC0),

    ReqIdC2 = gen_event:send_request(Pid2, dummy_h, {delayed_answer,1}, req2, ReqIdC1),

    ReqId3 = gen_event:send_request(Pid3, dummy_h, {delayed_answer,500}),
    ReqIdC3 = gen_event:reqids_add(ReqId3, req3, ReqIdC2),

    Deadline = erlang:monotonic_time(millisecond) + 100,
    
    {{reply, delayed}, req2, ReqIdC4} = gen_event:receive_response(ReqIdC3, {abs, Deadline}, true),
    2 = gen_event:reqids_size(ReqIdC4),

    timeout = gen_event:receive_response(ReqIdC4, {abs, Deadline}, true),

    Abandoned = lists:sort([{ReqId1, req1}, {ReqId3, req3}]),
    Abandoned = lists:sort(gen_event:reqids_to_list(ReqIdC4)),

    %% Make sure requests were abandoned...
    timeout = gen_event:receive_response(ReqIdC4, {abs, Deadline+1000}, true),

    {reply, {ok, hejhopp}} = gen_event:receive_response(ReqId0, infinity),

    ok.
    
send_request_receive_reqid_collection_error(Pid1, Pid2, Pid3) ->

    ReqId0 = gen_event:send_request(Pid1, dummy_h, hejsan),

    ReqIdC0 = gen_event:reqids_new(),

    ReqId1 = gen_event:send_request(Pid1, dummy_h, {delayed_answer,400}),
    ReqIdC1 = gen_event:reqids_add(ReqId1, req1, ReqIdC0),
    try
        nope = gen_event:reqids_add(ReqId1, req2, ReqIdC1)
    catch
        error:badarg -> ok
    end,

    unlink(Pid2),
    exit(Pid2, kill),
    ReqIdC2 = gen_event:send_request(Pid2, dummy_h, {delayed_answer,1}, req2, ReqIdC1),
    ReqIdC3 = gen_event:send_request(Pid3, dummy_h, {delayed_answer,200}, req3, ReqIdC2),
    ReqIdC4 = gen_event:send_request(Pid1, bad_h, hejsan, req4, ReqIdC3),
    4 = gen_event:reqids_size(ReqIdC4),
    
    {{error, {noproc, _}}, req2, ReqIdC5} = gen_event:receive_response(ReqIdC4, 2000, true),
    3 = gen_event:reqids_size(ReqIdC5),

    {{reply, delayed}, req3, ReqIdC5} = gen_event:receive_response(ReqIdC5, infinity, false),
    {{reply, delayed}, req1, ReqIdC5} = gen_event:receive_response(ReqIdC5, infinity, false),
    {{error, bad_module}, req4, ReqIdC5} = gen_event:wait_response(ReqIdC5, infinity, false),

    {reply, {ok, hejhopp}} = gen_event:receive_response(ReqId0, infinity),

    ok.

send_request_wait_reqid_collection(Config) when is_list(Config) ->
    {ok, Pid1} = gen_event:start(),
    ok = gen_event:add_handler(Pid1, dummy_h, [self()]),
    [dummy_h] = gen_event:which_handlers(Pid1),
    {ok, Pid2} = gen_event:start(),
    ok = gen_event:add_handler(Pid2, dummy_h, [self()]),
    [dummy_h] = gen_event:which_handlers(Pid2),
    {ok, Pid3} = gen_event:start(),
    ok = gen_event:add_handler(Pid3, dummy_h, [self()]),
    [dummy_h] = gen_event:which_handlers(Pid3),
    send_request_wait_reqid_collection(Pid1, Pid2, Pid3),
    send_request_wait_reqid_collection_timeout(Pid1, Pid2, Pid3),
    send_request_wait_reqid_collection_error(Pid1, Pid2, Pid3),
    ok = gen_event:stop(Pid1),
    try gen_event:stop(Pid2) catch exit:noproc -> ok end,
    ok = gen_event:stop(Pid3).

send_request_wait_reqid_collection(Pid1, Pid2, Pid3) ->

    ReqId0 = gen_event:send_request(Pid1, dummy_h, hejsan),

    ReqIdC0 = gen_event:reqids_new(),

    ReqId1 = gen_event:send_request(Pid1, dummy_h, {delayed_answer,400}),
    ReqIdC1 = gen_event:reqids_add(ReqId1, req1, ReqIdC0),
    1 = gen_event:reqids_size(ReqIdC1),

    ReqIdC2 = gen_event:send_request(Pid2, dummy_h, {delayed_answer,1}, req2, ReqIdC1),
    2 = gen_event:reqids_size(ReqIdC2),

    ReqIdC3 = gen_event:send_request(Pid3, dummy_h, {delayed_answer,200}, req3, ReqIdC2),
    3 = gen_event:reqids_size(ReqIdC3),
    
    {{reply, delayed}, req2, ReqIdC4} = gen_event:wait_response(ReqIdC3, infinity, true),
    2 = gen_event:reqids_size(ReqIdC4),

    {{reply, delayed}, req3, ReqIdC5} = gen_event:wait_response(ReqIdC4, 5678, true),
    1 = gen_event:reqids_size(ReqIdC5),

    {{reply, delayed}, req1, ReqIdC6} = gen_event:wait_response(ReqIdC5, 5000, true),
    0 = gen_event:reqids_size(ReqIdC6),

    no_request = gen_event:wait_response(ReqIdC6, 5000, true),

    {reply, {ok, hejhopp}} = gen_event:receive_response(ReqId0, infinity),

    ok.

send_request_wait_reqid_collection_timeout(Pid1, Pid2, Pid3) ->

    ReqId0 = gen_event:send_request(Pid1, dummy_h, hejsan),

    ReqIdC0 = gen_event:reqids_new(),

    ReqId1 = gen_event:send_request(Pid1, dummy_h, {delayed_answer,1000}),
    ReqIdC1 = gen_event:reqids_add(ReqId1, req1, ReqIdC0),

    ReqIdC2 = gen_event:send_request(Pid2, dummy_h, {delayed_answer,1}, req2, ReqIdC1),

    ReqId3 = gen_event:send_request(Pid3, dummy_h, {delayed_answer,500}),
    ReqIdC3 = gen_event:reqids_add(ReqId3, req3, ReqIdC2),

    Deadline = erlang:monotonic_time(millisecond) + 100,
    
    {{reply, delayed}, req2, ReqIdC4} = gen_event:wait_response(ReqIdC3, {abs, Deadline}, true),
    2 = gen_event:reqids_size(ReqIdC4),

    timeout = gen_event:wait_response(ReqIdC4, {abs, Deadline}, true),

    Unhandled = lists:sort([{ReqId1, req1}, {ReqId3, req3}]),
    Unhandled = lists:sort(gen_event:reqids_to_list(ReqIdC4)),

    %% Make sure requests were not abandoned...
    {{reply, delayed}, req3, ReqIdC4} = gen_event:wait_response(ReqIdC4, {abs, Deadline+1500}, false),
    {{reply, delayed}, req1, ReqIdC4} = gen_event:wait_response(ReqIdC4, {abs, Deadline+1500}, false),

    {reply, {ok, hejhopp}} = gen_event:receive_response(ReqId0, infinity),

    ok.
    
send_request_wait_reqid_collection_error(Pid1, Pid2, Pid3) ->

    ReqId0 = gen_event:send_request(Pid1, dummy_h, hejsan),

    ReqIdC0 = gen_event:reqids_new(),

    ReqId1 = gen_event:send_request(Pid1, dummy_h, {delayed_answer,400}),
    ReqIdC1 = gen_event:reqids_add(ReqId1, req1, ReqIdC0),
    try
        nope = gen_event:reqids_add(ReqId1, req2, ReqIdC1)
    catch
        error:badarg -> ok
    end,

    unlink(Pid2),
    exit(Pid2, kill),
    ReqIdC2 = gen_event:send_request(Pid2, dummy_h, {delayed_answer,1}, req2, ReqIdC1),
    ReqIdC3 = gen_event:send_request(Pid3, dummy_h, {delayed_answer,200}, req3, ReqIdC2),
    ReqIdC4 = gen_event:send_request(Pid1, bad_h, hejsan, req4, ReqIdC3),
    4 = gen_event:reqids_size(ReqIdC4),
    
    {{error, {noproc, _}}, req2, ReqIdC5} = gen_event:wait_response(ReqIdC4, 2000, true),
    3 = gen_event:reqids_size(ReqIdC5),

    {{reply, delayed}, req3, ReqIdC5} = gen_event:wait_response(ReqIdC5, infinity, false),
    {{reply, delayed}, req1, ReqIdC5} = gen_event:wait_response(ReqIdC5, infinity, false),
    {{error, bad_module}, req4, ReqIdC5} = gen_event:wait_response(ReqIdC5, infinity, false),

    {reply, {ok, hejhopp}} = gen_event:receive_response(ReqId0, infinity),

    ok.

send_request_check_reqid_collection(Config) when is_list(Config) ->
    {ok, Pid1} = gen_event:start(),
    ok = gen_event:add_handler(Pid1, dummy_h, [self()]),
    [dummy_h] = gen_event:which_handlers(Pid1),
    {ok, Pid2} = gen_event:start(),
    ok = gen_event:add_handler(Pid2, dummy_h, [self()]),
    [dummy_h] = gen_event:which_handlers(Pid2),
    {ok, Pid3} = gen_event:start(),
    ok = gen_event:add_handler(Pid3, dummy_h, [self()]),
    [dummy_h] = gen_event:which_handlers(Pid3),
    send_request_check_reqid_collection(Pid1, Pid2, Pid3),
    send_request_check_reqid_collection_error(Pid1, Pid2, Pid3),
    ok = gen_event:stop(Pid1),
    try gen_event:stop(Pid2) catch exit:noproc -> ok end,
    ok = gen_event:stop(Pid3).

send_request_check_reqid_collection(Pid1, Pid2, Pid3) ->

    ReqId0 = gen_event:send_request(Pid1, dummy_h, hejsan),

    receive after 100 -> ok end,

    ReqIdC0 = gen_event:reqids_new(),

    ReqIdC1 = gen_event:send_request(Pid1, dummy_h, {delayed_answer,400}, req1, ReqIdC0),
    1 = gen_event:reqids_size(ReqIdC1),

    ReqId2 = gen_event:send_request(Pid2, dummy_h, {delayed_answer,1}),
    ReqIdC2 = gen_event:reqids_add(ReqId2, req2, ReqIdC1),
    2 = gen_event:reqids_size(ReqIdC2),

    ReqIdC3 = gen_event:send_request(Pid3, dummy_h, {delayed_answer,200}, req3, ReqIdC2),
    3 = gen_event:reqids_size(ReqIdC3),

    Msg0 = next_msg(),
    no_reply = gen_event:check_response(Msg0, ReqIdC3, true),
    
    {{reply, delayed}, req2, ReqIdC4} = gen_event:check_response(next_msg(), ReqIdC3, true),
    2 = gen_event:reqids_size(ReqIdC4),

    {{reply, delayed}, req3, ReqIdC5} = gen_event:check_response(next_msg(), ReqIdC4, true),
    1 = gen_event:reqids_size(ReqIdC5),

    {{reply, delayed}, req1, ReqIdC6} = gen_event:check_response(next_msg(), ReqIdC5, true),
    0 = gen_event:reqids_size(ReqIdC6),

    no_request = gen_event:check_response(Msg0, ReqIdC6, true),

    {reply, {ok, hejhopp}} = gen_event:check_response(Msg0, ReqId0),

    ok.
    
send_request_check_reqid_collection_error(Pid1, Pid2, Pid3) ->

    ReqId0 = gen_event:send_request(Pid1, dummy_h, hejsan),

    receive after 100 -> ok end,

    ReqIdC0 = gen_event:reqids_new(),

    ReqId1 = gen_event:send_request(Pid1, dummy_h, {delayed_answer,400}),
    ReqIdC1 = gen_event:reqids_add(ReqId1, req1, ReqIdC0),
    try
        nope = gen_event:reqids_add(ReqId1, req2, ReqIdC1)
    catch
        error:badarg -> ok
    end,

    unlink(Pid2),
    exit(Pid2, kill),
    ReqIdC2 = gen_event:send_request(Pid2, dummy_h, {delayed_answer,1}, req2, ReqIdC1),

    ReqIdC3 = gen_event:send_request(Pid3, dummy_h, {delayed_answer,200}, req3, ReqIdC2),

    ReqIdC4 = gen_event:send_request(Pid1, bad_h, hejsan, req4, ReqIdC3),
    4 = gen_event:reqids_size(ReqIdC4),

    Msg0 = next_msg(),

    no_reply = gen_event:check_response(Msg0, ReqIdC3, true),
    
    {{error, {noproc, _}}, req2, ReqIdC5} = gen_event:check_response(next_msg(), ReqIdC4, true),
    3 = gen_event:reqids_size(ReqIdC5),

    {{reply, delayed}, req3, ReqIdC5} = gen_event:check_response(next_msg(), ReqIdC5, false),
    {{reply, delayed}, req1, ReqIdC5} = gen_event:check_response(next_msg(), ReqIdC5, false),
    {{error, bad_module}, req4, ReqIdC5} = gen_event:check_response(next_msg(), ReqIdC5, false),

    no_reply = gen_event:check_response(Msg0, ReqIdC3, false),

    {reply, {ok, hejhopp}} = gen_event:check_response(Msg0, ReqId0),

    ok.

next_msg() ->
    receive M -> M end.
