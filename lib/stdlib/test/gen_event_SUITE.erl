%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2016. All Rights Reserved.
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
	 init_per_group/2,end_per_group/2]).
-export([start/1, add_handler/1, add_sup_handler/1,
	 delete_handler/1, swap_handler/1, swap_sup_handler/1,
	 notify/1, sync_notify/1, call/1, info/1, hibernate/1,
	 call_format_status/1, call_format_status_anon/1,
         error_format_status/1, get_state/1, replace_state/1]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [start, {group, test_all}, hibernate,
     call_format_status, call_format_status_anon, error_format_status,
     get_state, replace_state].

groups() -> 
    [{test_all, [],
      [add_handler, add_sup_handler, delete_handler,
       swap_handler, swap_sup_handler, notify, sync_notify,
       call, info]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


%% --------------------------------------
%% Start an event manager.
%% --------------------------------------

start(Config) when is_list(Config) ->
    OldFl = process_flag(trap_exit, true),

    dummy_via:reset(),

    {ok, Pid0} = gen_event:start(), %anonymous
    [] = gen_event:which_handlers(Pid0),
    ok = gen_event:stop(Pid0),

    {ok, Pid1} = gen_event:start_link(), %anonymous
    [] = gen_event:which_handlers(Pid1),
    ok = gen_event:stop(Pid1),

    {ok, Pid2} = gen_event:start({local, my_dummy_name}),
    [] = gen_event:which_handlers(my_dummy_name),
    [] = gen_event:which_handlers(Pid2),
    ok = gen_event:stop(my_dummy_name),

    {ok, Pid3} = gen_event:start_link({local, my_dummy_name}),
    [] = gen_event:which_handlers(my_dummy_name),
    [] = gen_event:which_handlers(Pid3),
    ok = gen_event:stop(my_dummy_name),

    {ok, Pid4} = gen_event:start_link({global, my_dummy_name}),
    [] = gen_event:which_handlers({global, my_dummy_name}),
    [] = gen_event:which_handlers(Pid4),
    ok = gen_event:stop({global, my_dummy_name}),

    {ok, Pid5} = gen_event:start_link({via, dummy_via, my_dummy_name}),
    [] = gen_event:which_handlers({via, dummy_via, my_dummy_name}),
    [] = gen_event:which_handlers(Pid5),
    ok = gen_event:stop({via, dummy_via, my_dummy_name}),

    {ok, _} = gen_event:start_link({local, my_dummy_name}),
    {error, {already_started, _}} =
	gen_event:start_link({local, my_dummy_name}),
    {error, {already_started, _}} =
	gen_event:start({local, my_dummy_name}),
    ok = gen_event:stop(my_dummy_name),

    {ok, Pid6} = gen_event:start_link({global, my_dummy_name}),
    {error, {already_started, _}} =
	gen_event:start_link({global, my_dummy_name}),
    {error, {already_started, _}} =
	gen_event:start({global, my_dummy_name}),

    ok = gen_event:stop({global, my_dummy_name}, shutdown, 10000),
    receive
	{'EXIT', Pid6, shutdown} -> ok
    after 10000 ->
	    ct:fail(exit_gen_event)
    end,

    {ok, Pid7} = gen_event:start_link({via, dummy_via, my_dummy_name}),
    {error, {already_started, _}} =
	gen_event:start_link({via, dummy_via, my_dummy_name}),
    {error, {already_started, _}} =
	gen_event:start({via, dummy_via, my_dummy_name}),

    exit(Pid7, shutdown),
    receive
	{'EXIT', Pid7, shutdown} -> ok
    after 10000 ->
	    ct:fail(exit_gen_event)
    end,

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
    {ok,_} = gen_event:start({local, my_dummy_handler}),
    ok = gen_event:add_handler(my_dummy_handler, dummy_h, [self()]),
    ok = gen_event:add_handler(my_dummy_handler, {dummy_h, 1}, [self()]),
    [{dummy_h, 1}, dummy_h] = gen_event:which_handlers(my_dummy_handler),
    {'EXIT',_} = (catch gen_event:call(non_exist, dummy_h, hejsan)),
    {error, bad_module} =
	gen_event:call(my_dummy_handler, bad_h, hejsan),
    {ok, hejhopp} = gen_event:call(my_dummy_handler, dummy_h, hejsan),
    {ok, hejhopp} = gen_event:call(my_dummy_handler, {dummy_h, 1},
				   hejsan),
    {ok, hejhopp} = gen_event:call(my_dummy_handler, dummy_h, hejsan,
				   10000),
    {'EXIT', {timeout, _}} =
	(catch gen_event:call(my_dummy_handler, dummy_h, hejsan, 0)),
    flush(),
    ok = gen_event:delete_handler(my_dummy_handler, {dummy_h, 1}, []),
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
    receive _ -> flush() after 0 -> ok end.

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

%% Test that sys:get_status/1,2 calls format_status/2.
call_format_status(Config) when is_list(Config) ->
    {ok, Pid} = gen_event:start({local, my_dummy_handler}),
    %% State here intentionally differs from what we expect from format_status
    State = self(),
    FmtState = "dummy1_h handler state",
    ok = gen_event:add_handler(my_dummy_handler, dummy1_h, [State]),
    Status1 = sys:get_status(Pid),
    Status2 = sys:get_status(Pid, 5000),
    ok = gen_event:stop(Pid),
    {status, Pid, _, [_, _, Pid, [], Data1]} = Status1,
    HandlerInfo1 = proplists:get_value(items, Data1),
    {"Installed handlers", [{_,dummy1_h,_,FmtState,_}]} = HandlerInfo1,
    {status, Pid, _, [_, _, Pid, [], Data2]} = Status2,
    HandlerInfo2 = proplists:get_value(items, Data2),
    {"Installed handlers", [{_,dummy1_h,_,FmtState,_}]} = HandlerInfo2,
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
    State = self(),
    {ok, Pid} = gen_event:start({local, my_dummy_handler}),
    ok = gen_event:add_sup_handler(my_dummy_handler, dummy1_h, [State]),
    ok = gen_event:notify(my_dummy_handler, do_crash),
    receive
	{gen_event_EXIT,dummy1_h,{'EXIT',_}} -> ok
    after 5000 ->
	    ct:fail(exit_gen_event)
    end,
    FmtState = "dummy1_h handler state",
    receive
	{error,_GroupLeader, {Pid,
			      "** gen_event handler"++_,
			      [dummy1_h,my_dummy_handler,do_crash,
			       FmtState, _]}} ->
	    ok;
	Other ->
	    io:format("Unexpected: ~p", [Other]),
	    ct:fail(failed)
    end,
    ok = gen_event:stop(Pid),
    process_flag(trap_exit, OldFl),
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
