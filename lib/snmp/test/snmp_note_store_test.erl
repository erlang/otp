%% 
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2016. All Rights Reserved.
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
-module(snmp_note_store_test).

%%----------------------------------------------------------------------
%% Include files
%%----------------------------------------------------------------------
-include_lib("common_test/include/ct.hrl").
-include("snmp_test_lib.hrl").


%%----------------------------------------------------------------------
%% External exports
%%----------------------------------------------------------------------
-export([
         init_per_testcase/2, end_per_testcase/2,
	all/0,groups/0,init_per_group/2,end_per_group/2, 
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

%%----------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Records
%%----------------------------------------------------------------------

%%======================================================================
%% External functions
%%======================================================================

init_per_testcase(_Case, Config) when is_list(Config) ->
    Config.

end_per_testcase(_Case, Config) when is_list(Config) ->
    Config.

%%======================================================================
%% Test case definitions
%%======================================================================
all() -> 
[start_and_stop, notes, info, garbage_in].

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
	Config.

end_per_group(_GroupName, Config) ->
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

    {ok, Handler, Pid} = note_store_handler_start(),

    io:format("sleep some before we begin the tests~n", []),
    ?SLEEP(timer:seconds(1)), 

    %% Default lifetime is infinity. A note with lifetime
    %% infinity is permanent
    io:format("create permanent note sune~n", []),
    true = snmp_note_store:set_note(Pid, sune, 10), 
    10 = snmp_note_store:get_note(Pid, sune),
    10 = snmp_note_store:get_note(Pid, sune),
    
    %% Lifetime is in 1/100 sec ticks, so 500 equals 5 seconds
    io:format("create 5 sec note kalle~n", []),
    true = snmp_note_store:set_note(Pid, 500, kalle, hobbe), 
    io:format("wait 1 sec~n", []),
    ?SLEEP(timer:seconds(1)),
    io:format("get note kalle~n", []),
    hobbe = snmp_note_store:get_note(Pid, kalle),
    io:format("wait 5 sec~n", []),
    ?SLEEP(timer:seconds(5)),
    io:format("get note kalle again (now it should not exist)~n", []),
    undefined = snmp_note_store:get_note(Pid, kalle),

    io:format("create 5 sec note kalle~n", []),
    true = snmp_note_store:set_note(Pid, 500, kalle, hobbe), 
    io:format("wait 6 sec (to allow timer to clean up note)~n", []),
    ?SLEEP(timer:seconds(6)),
    io:format("get note kalle - should not exist~n", []),
    undefined = snmp_note_store:get_note(Pid, kalle),

    io:format("read the permanent note sune again~n", []),
    10 = snmp_note_store:get_note(Pid, sune),

    note_store_handler_stop(Handler),

    ok.


%%======================================================================

info(suite) ->
    [];
info(doc) ->
    ["Testing that we can retreive process info."];
info(Config) when is_list(Config) ->

    Prio = normal,
    Mod  = ?MODULE, 
    Opts = [{verbosity, trace}], 
    {ok, Pid} = snmp_note_store:start_link(Prio, Mod, Opts),

    %% Get the info:
    Info = snmp_note_store:info(Pid),
    io:format("Info: ~p~n", [Info]),

    %% Verify content
    io:format("get process memory~n", []),
    {value, {process_memory, ProcMem}} = 
	lists:keysearch(process_memory, 1, Info),
    io:format("get notes process memory~n", []),
    {value, {notes, NotesProcMem}} = 
	lists:keysearch(notes, 1, ProcMem),
    io:format("verify notes process memory~n", []),
    if
	is_integer(NotesProcMem) andalso (NotesProcMem > 0) ->
	    ok;
	true ->
	    throw({error, {bad_notes_proc_memery, NotesProcMem}})
    end,
    io:format("get timer process memory~n", []),
    {value, {timer, TmrProcMem}} = 
	lists:keysearch(timer, 1, ProcMem),
    io:format("verify timer process memory~n", []),
    if
	is_integer(TmrProcMem) andalso (TmrProcMem > 0) ->
	    ok;
	true ->
	    throw({error, {bad_timer_proc_memery, TmrProcMem}})
    end,    
    io:format("get db memory~n", []),
    {value, {db_memory, DbMem}} = 
	lists:keysearch(db_memory, 1, Info),
    io:format("get notes db memory~n", []),
    {value, {notes, NotesDbMem}} = 
	lists:keysearch(notes, 1, DbMem),
    io:format("verify notes db memory~n", []),
    if
	is_integer(NotesDbMem) andalso (NotesDbMem > 0) ->
	    ok;
	true ->
	    throw({error, {bad_notes_db_memery, NotesDbMem}})
    end,


    snmp_note_store:stop(Pid),

    ok.


%%======================================================================

garbage_in(suite) ->
    [];
garbage_in(doc) ->
    ["Test that the process handles garbage sent to it."];
garbage_in(Config) when is_list(Config) ->

    io:format("start note_store server~n", []),
    Prio = normal,
    Mod  = ?MODULE, 
    Opts = [{verbosity, trace}], 
    {ok, Pid} = snmp_note_store:start_link(Prio, Mod, Opts),

    io:format("issue bad request~n", []),
    {error, _} = gen_server:call(Pid, bad_request),

    io:format("cast bad message~n", []),
    gen_server:cast(Pid, bad_message),
    
    io:format("bang bad info~n", []),
    Pid ! bad_info,

    io:format("verify note_Store server still alive and kicking~n", []),
    Info = snmp_note_store:info(Pid),
    if
	is_list(Info) andalso (length(Info) > 0) ->
	    ok;
	true ->
	    throw({error, {bad_info, Info}})
    end,

    io:format("stop note_store server~n", []),
    snmp_note_store:stop(Pid),

    io:format("done~n", []),
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
    
