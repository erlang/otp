%% 
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2020. All Rights Reserved.
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

%%----------------------------------------------------------------------
%% Purpose:
%%----------------------------------------------------------------------
-module(snmp_note_store_SUITE).

%%----------------------------------------------------------------------
%% Include files
%%----------------------------------------------------------------------
-include_lib("common_test/include/ct.hrl").
-include("snmp_test_lib.hrl").


%%----------------------------------------------------------------------
%% External exports
%%----------------------------------------------------------------------
-export([
         suite/0, all/0, groups/0,
         init_per_suite/1,    end_per_suite/1,
         init_per_group/2,    end_per_group/2, 
         init_per_testcase/2, end_per_testcase/2,

	 start_and_stop/1,
	 notes/1,
	 info/1,
	 garbage_in/1
	]).

%%----------------------------------------------------------------------
%% Internal exports
%%----------------------------------------------------------------------
-export([
	 system_start_time/0
        ]).


%%======================================================================
%% External functions
%%======================================================================

%%======================================================================
%% Test case definitions
%%======================================================================

suite() -> 
    [{ct_hooks, [ts_install_cth]}].

all() -> 
    [
     start_and_stop,
     notes,
     info,
     garbage_in
    ].

groups() -> 
    [].



%%
%% -----
%%

init_per_suite(Config0) when is_list(Config0) ->
    ?IPRINT("init_per_suite -> entry with"
            "~n   Config: ~p", [Config0]),

    case ?LIB:init_per_suite(Config0) of
        {skip, _} = SKIP ->
            SKIP;

        Config1 when is_list(Config1) ->
            %% We need a monitor on this node also
            snmp_test_sys_monitor:start(),

            ?IPRINT("init_per_suite -> end when"
                    "~n      Config1: ~p", [Config1]),
            
            Config1
    end.

end_per_suite(Config0) when is_list(Config0) ->
    ?IPRINT("end_per_suite -> entry with"
            "~n   Config: ~p", [Config0]),

    snmp_test_sys_monitor:stop(),
    Config1 = ?LIB:end_per_suite(Config0),

    ?IPRINT("end_per_suite -> end"),

    Config1.



%%
%% -----
%%

init_per_group(_GroupName, Config) ->
	Config.

end_per_group(_GroupName, Config) ->
	Config.



%%
%% -----
%%

init_per_testcase(_Case, Config) when is_list(Config) ->
    ?IPRINT("init_per_testcase -> entry with"
            "~n   Config: ~p", [Config]),

    snmp_test_global_sys_monitor:reset_events(),

    ?IPRINT("init_per_testcase -> end"),

    Config.

end_per_testcase(_Case, Config) when is_list(Config) ->

    ?IPRINT("end_per_testcase -> entry with"
            "~n   Config: ~p",
            [Config]),

    ?IPRINT("system events during test: ~p",
            [snmp_test_global_sys_monitor:events()]),

    Config.



%%======================================================================
%% Test functions
%%======================================================================


%%======================================================================

start_and_stop(suite) ->
    [];
start_and_stop(doc) ->
    ["Simple start and stop."];
start_and_stop(Config) when is_list(Config) ->

    Prio = normal,
    Mod  = ?MODULE, 
    Opts = [{verbosity, trace}], 
    {ok, Pid} = snmp_note_store:start_link(Prio, Mod, Opts),
    snmp_note_store:stop(Pid),

    ok.


%%======================================================================

notes(suite) ->
    [];
notes(doc) ->
    ["Testing that it does what it is actually supposed to do, "
     "namilly to handle notes. "];
notes(Config) when is_list(Config) ->
    Pre = fun() ->
                  ?IPRINT("try start note-store"),
                  case note_store_handler_start() of
                      {ok, Handler, Pid} ->
                          ?IPRINT("started - sleep some before we begin the tests"),
                          ?SLEEP(?SECS(1)), 
                          {Handler, Pid};
                      {error, Reason} ->
                          {skip, ?F("Failed starting note-store: ~p", [Reason])}
                  end
          end,
    Case = fun(State) -> do_notes(State, Config) end,
    Post = fun({Handler, _Pid}) ->
                   note_store_handler_stop(Handler)
           end,
    ?TC_TRY(notes, Pre, Case, Post).

do_notes({_, Pid}, _Config) ->

    %% Default lifetime is infinity. A note with lifetime
    %% infinity is permanent
    ?IPRINT("create permanent note: sune"),
    true = snmp_note_store:set_note(Pid, sune, 10), 
    10 = snmp_note_store:get_note(Pid, sune),
    10 = snmp_note_store:get_note(Pid, sune),
    
    %% Lifetime is in 1/100 sec ticks, so 500 equals 5 seconds
    ?IPRINT("create 5 sec note kalle"),
    true = snmp_note_store:set_note(Pid, 500, kalle, hobbe), 
    ?IPRINT("wait 1 sec"),
    ?SLEEP(timer:seconds(1)),
    ?IPRINT("get note kalle"),
    hobbe = snmp_note_store:get_note(Pid, kalle),
    ?IPRINT("wait 5 sec"),
    ?SLEEP(timer:seconds(5)),
    ?IPRINT("get note kalle again (now it should not exist)"),
    undefined = snmp_note_store:get_note(Pid, kalle),

    ?IPRINT("create 5 sec note kalle"),
    true = snmp_note_store:set_note(Pid, 500, kalle, hobbe), 
    ?IPRINT("wait 6 sec (to allow timer to clean up note)"),
    ?SLEEP(timer:seconds(6)),
    ?IPRINT("get note kalle - should not exist"),
    undefined = snmp_note_store:get_note(Pid, kalle),

    ?IPRINT("read the permanent note sune again"),
    10 = snmp_note_store:get_note(Pid, sune),

    ?IPRINT("done"),

    ok.


%%======================================================================

info(suite) ->
    [];
info(doc) ->
    ["Testing that we can retreive process info."];
info(Config) when is_list(Config) ->
    Pre = fun() ->
                  ?IPRINT("try start note-store"),
                  Prio = normal,
                  Mod  = ?MODULE, 
                  Opts = [{verbosity, trace}], 
                  case snmp_note_store:start_link(Prio, Mod, Opts) of
                      {ok, Pid} ->
                          ?IPRINT("note-store started: ~p", [Pid]),
                          Pid;
                      {error, Reason} ->
                          {skip, ?F("Failed starting note-store: ~p", [Reason])}
                  end
          end,
    Case = fun(State) -> do_info(State, Config) end,
    Post = fun(Pid) ->
                   ?IPRINT("attempt stop note-store"),
                   snmp_note_store:stop(Pid)
           end,
    ?TC_TRY(info, Pre, Case, Post).

do_info(Pid, _Config) ->

    %% Get the info:
    ?IPRINT("get initial info"),
    Info = snmp_note_store:info(Pid),
    ?IPRINT("Info: "
            "~n   ~p", [Info]),

    %% Verify content
    ?IPRINT("verify content: get notes process memory"),
    {value, {process_memory, ProcMem}} = 
	lists:keysearch(process_memory, 1, Info),
    ?IPRINT("get notes process memory"),
    {value, {notes, NotesProcMem}} = 
	lists:keysearch(notes, 1, ProcMem),
    ?IPRINT("verify notes process memory"),
    if
	is_integer(NotesProcMem) andalso (NotesProcMem > 0) ->
	    ok;
	true ->
	    throw({error, {bad_notes_proc_memery, NotesProcMem}})
    end,
    ?IPRINT("verify content: get timer process memory"),
    {value, {timer, TmrProcMem}} = 
	lists:keysearch(timer, 1, ProcMem),
    ?IPRINT("verify content: timer process memory"),
    if
	is_integer(TmrProcMem) andalso (TmrProcMem > 0) ->
	    ok;
	true ->
	    throw({error, {bad_timer_proc_memery, TmrProcMem}})
    end,    
    ?IPRINT("verify content: get db memory"),
    {value, {db_memory, DbMem}} = 
	lists:keysearch(db_memory, 1, Info),
    ?IPRINT("verify content: get notes db memory"),
    {value, {notes, NotesDbMem}} = 
	lists:keysearch(notes, 1, DbMem),
    ?IPRINT("verify content: notes db memory"),
    if
	is_integer(NotesDbMem) andalso (NotesDbMem > 0) ->
	    ok;
	true ->
	    throw({error, {bad_notes_db_memery, NotesDbMem}})
    end,

    ?IPRINT("done"),
    ok.


%%======================================================================

garbage_in(suite) ->
    [];
garbage_in(doc) ->
    ["Test that the process handles garbage sent to it."];
garbage_in(Config) when is_list(Config) ->
    Pre = fun() ->
                  ?IPRINT("try start note-store"),
                  Prio = normal,
                  Mod  = ?MODULE, 
                  Opts = [{verbosity, trace}], 
                  case snmp_note_store:start_link(Prio, Mod, Opts) of
                      {ok, Pid} ->
                          ?IPRINT("note-store started: ~p", [Pid]),
                          Pid;
                      {error, Reason} ->
                          {skip, ?F("Failed starting note-store: ~p", [Reason])}
                  end
          end,
    Case = fun(State) -> do_garbage_in(State, Config) end,
    Post = fun(Pid) ->
                   ?IPRINT("attempt stop note-store"),
                   snmp_note_store:stop(Pid)
           end,
    ?TC_TRY(garbage_in, Pre, Case, Post).

do_garbage_in(Pid, _Config) ->

    ?IPRINT("issue bad request"),
    {error, _} = gen_server:call(Pid, bad_request),

    ?IPRINT("cast bad message"),
    gen_server:cast(Pid, bad_message),
    
    ?IPRINT("bang bad info"),
    Pid ! bad_info,

    ?IPRINT("verify note-store server still alive and kicking"),
    Info = snmp_note_store:info(Pid),
    if
	is_list(Info) andalso (length(Info) > 0) ->
	    ok;
	true ->
	    throw({error, {bad_info, Info}})
    end,

    ?IPRINT("done"),
    ok.


%%======================================================================
%% Internal functions
%%======================================================================

system_start_time() ->
    note_store_handler ! {system_start_time, self()},
    receive
	{system_start_time, SysStartTime} ->
	    SysStartTime
    end.

note_store_handler_start() ->
    Self = self(),
    Fun = fun() -> note_store_handler(Self) end,
    HandlerPid = spawn_link(Fun),
    receive
	{started, HandlerPid, NSPid} ->
	    {ok, HandlerPid, NSPid}
    after 5000 ->
	    exit(HandlerPid, kill),
	    {error, timeout}
    end.

note_store_handler_stop(Pid) ->
    Pid ! {stop, self()},
    receive
	{stopped, Pid} ->
	    ok
    after 5000 ->
	    exit(Pid, kill),
	    {error, timeout}
    end.

note_store_handler(Parent) ->
    erlang:register(note_store_handler, self()),
    put(system_start_time, snmp_misc:now(cs)),
    Prio = normal,
    Mod  = ?MODULE, 
    Opts = [{verbosity, trace}], 
    {ok, Pid} = snmp_note_store:start_link(Prio, Mod, Opts),
    Parent ! {started, self(), Pid},
    note_store_handler_loop(Parent, Pid).

note_store_handler_loop(Parent, NSPid) ->
    receive
	{stop, Parent} ->
	    snmp_note_store:stop(NSPid),
	    Parent ! {stopped, self()},
	    exit(normal);
	{system_start_time, Pid} ->
	    StartTime = get(system_start_time),
	    Pid ! {system_start_time, StartTime},
	    note_store_handler_loop(Parent, NSPid)
    end.
    
