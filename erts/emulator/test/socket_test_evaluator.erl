%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2018-2019. All Rights Reserved.
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

-module(socket_test_evaluator).

%% Evaluator control functions
-export([
         start/3,
         await_finish/1
        ]).

%% Functions used by evaluators to interact with eachother
-export([
         %% Announce functions
         %% (Send an announcement from one evaluator to another)
         announce_start/1,     announce_start/2,
         announce_continue/2,  announce_continue/3,
         announce_ready/2,     announce_ready/3,
         announce_terminate/1,

         %% Await functions
         %% (Wait for an announcement from another evaluator)
         await_start/0,        await_start/1,
         await_continue/3,     await_continue/4,
         await_ready/3,        await_ready/4,
         await_terminate/2,    await_terminate/3,
         await_termination/1,  await_termination/2
        ]).

%% Utility functions
-export([
         iprint/2, % Info  printouts
         eprint/2  % Error printouts
        ]).

-export_type([
              ev/0,
              initial_evaluator_state/0,
              evaluator_state/0,
              command_fun/0,
              command/0
             ]).


-include("socket_test_evaluator.hrl").

-type ev() :: #ev{}.
-type initial_evaluator_state() :: map().
-type evaluator_state() :: term().
-type command_fun() :: 
        fun((State :: evaluator_state()) -> ok) |
        fun((State :: evaluator_state()) -> {ok, evaluator_state()}) |
        fun((State :: evaluator_state()) -> {error, term()}).

-type command() :: #{desc  := string(),
                     cmd   := command_fun()}.


%% ============================================================================

-define(LIB,                    socket_test_lib).
-define(LOGGER,                 socket_test_logger).

-define(EXTRA_NOTHING,          '$nothing').
-define(ANNOUNCEMENT_START,     '$start').
-define(ANNOUNCEMENT_READY,     '$ready').
-define(ANNOUNCEMENT_CONTINUE,  '$continue').
-define(ANNOUNCEMENT_TERMINATE, '$terminate').

-define(START_NAME_NONE,        '$no-name').
-define(START_SLOGAN,           ?ANNOUNCEMENT_START).
-define(TERMINATE_SLOGAN,       ?ANNOUNCEMENT_TERMINATE).


%% ============================================================================

-spec start(Name, Seq, Init) -> ev() when
      Name :: string(),
      Seq  :: [command()],
      Init :: initial_evaluator_state().
                             
start(Name, Seq, InitState) 
  when is_list(Name) andalso is_list(Seq) andalso (Seq =/= []) ->
    %% Make sure 'parent' is not already used
    case maps:find(parent, InitState) of
        {ok, _} ->
            erlang:error({already_used, parent});
        error ->
            InitState2 = InitState#{parent => self()},
            Pid = erlang:spawn_link(
                    fun() -> init(Name, Seq, InitState2) end),
            %% MRef = erlang:monitor(process, Pid),
            #ev{name = Name, pid = Pid}%, mref = MRef}
    end.

init(Name, Seq, Init) ->
    put(sname, Name),
    process_flag(trap_exit, true),
    loop(1, Seq, Init).

loop(_ID, [], FinalState) ->
    exit(FinalState);
loop(ID, [#{desc := Desc,
            cmd  := Cmd}|Cmds], State) when is_function(Cmd, 1) ->
    iprint("evaluate command ~2w: ~s", [ID, Desc]),
    try Cmd(State) of
        ok ->
            loop(ID + 1, Cmds, State);
        {ok, NewState} ->
            loop(ID + 1, Cmds, NewState);
        {skip, Reason} ->
            ?SEV_IPRINT("command ~w skip: "
                        "~n   ~p", [ID, Reason]),
            exit({skip, Reason});
        {error, Reason} ->
            ?SEV_EPRINT("command ~w failed: "
                        "~n   ~p", [ID, Reason]),
            exit({command_failed, ID, Reason, State})
    catch
        C:{skip, command} = E:_ when ((C =:= throw) orelse (C =:= exit)) ->
            %% Secondary skip
            exit(E);
        C:{skip, R} = E:_ when ((C =:= throw) orelse (C =:= exit)) ->
            ?SEV_IPRINT("command ~w skip catched(~w): "
                        "~n   Reason: ~p", [ID, C, R]),
            exit(E);
        C:E:S ->
            ?SEV_EPRINT("command ~w crashed: "
                        "~n   Class:      ~p"
                        "~n   Error:      ~p"
                        "~n   Call Stack: ~p", [ID, C, E, S]),
            exit({command_crashed, ID, {C,E,S}, State})
    end.


%% ============================================================================

-spec await_finish(Evs) -> term() when
      Evs :: [ev()].

await_finish(Evs) ->
    await_finish(Evs, [], []).

await_finish([], _, []) ->
    ok;
await_finish([], _OK, Fails) ->
    ?SEV_EPRINT("Fails: "
		"~n   ~p", [Fails]),
    Fails;
await_finish(Evs, OK, Fails) ->
    receive
        %% Successfull termination of evaluator
        {'DOWN', _MRef, process, Pid, normal} ->
            {Evs2, OK2, Fails2} = await_finish_normal(Pid, Evs, OK, Fails),
            await_finish(Evs2, OK2, Fails2);
        {'EXIT', Pid, normal} ->
            {Evs2, OK2, Fails2} = await_finish_normal(Pid, Evs, OK, Fails),
            await_finish(Evs2, OK2, Fails2);

        %% The evaluator can skip the test case:
        {'DOWN', _MRef, process, Pid, {skip, Reason}} ->
            %% ?SEV_IPRINT("await_finish -> skip (down) received: "
            %%             "~n   Pid:    ~p"
            %%             "~n   Reason: ~p", [Pid, Reason]),
            await_finish_skip(Pid, Reason, Evs, OK);
        {'EXIT', Pid, {skip, Reason}} ->
            %% ?SEV_IPRINT("await_finish -> skip (exit) received: "
            %%             "~n   Pid:    ~p"
            %%             "~n   Reason: ~p", [Pid, Reason]),
            await_finish_skip(Pid, Reason, Evs, OK);

        %% Evaluator failed
        {'DOWN', _MRef, process, Pid, Reason} ->
            %% ?SEV_IPRINT("await_finish -> fail (down) received: "
            %%             "~n   Pid:    ~p"
            %%             "~n   Reason: ~p", [Pid, Reason]),
            {Evs2, OK2, Fails2} =
                await_finish_fail(Pid, Reason, Evs, OK, Fails),
            await_finish(Evs2, OK2, Fails2);
        {'EXIT', Pid, Reason} ->
            %% ?SEV_IPRINT("await_finish -> fail (exit) received: "
            %%             "~n   Pid:    ~p"
            %%             "~n   Reason: ~p", [Pid, Reason]),
            {Evs2, OK2, Fails2} =
                await_finish_fail(Pid, Reason, Evs, OK, Fails),
            await_finish(Evs2, OK2, Fails2)
    end.


await_finish_normal(Pid, Evs, OK, Fails) ->
    case lists:keysearch(Pid, #ev.pid, Evs) of
        {value, #ev{name = Name}} ->
            iprint("evaluator '~s' (~p) success", [Name, Pid]),
            NewEvs = lists:keydelete(Pid, #ev.pid, Evs),
            {NewEvs, [Pid|OK], Fails};
        false ->
            case lists:member(Pid, OK) of
                true ->
                    ok;
                false ->
                    iprint("unknown process ~p died (normal)", [Pid]),
                    ok
            end,
            {Evs, OK, Fails}
    end.

await_finish_skip(Pid, Reason, Evs, OK) ->
    Evs2 =
        case lists:keysearch(Pid, #ev.pid, Evs) of
            {value, #ev{name = Name}} ->
                ?SEV_IPRINT("evaluator '~s' (~p) issued SKIP: "
                            "~n   ~p", [Name, Pid, Reason]),
                lists:keydelete(Pid, #ev.pid, Evs);
            false ->
                case lists:member(Pid, OK) of
                    true ->
                        ?SEV_IPRINT("already terminated (ok) process ~p skip"
                                    "~n   ~p", [Pid]),
                        ok;
                    false ->
                        ?SEV_IPRINT("unknown process ~p issued SKIP: "
                                    "~n   ~p", [Pid, Reason]),
                        iprint("unknown process ~p issued SKIP: "
                               "~n   ~p", [Pid, Reason])
                end,
                Evs
        end,
    await_evs_terminated(Evs2),
    ?LIB:skip(Reason).

await_evs_terminated(Evs) ->
    Instructions =
        [
         %% Just wait for the evaluators to die on their own
         {fun() -> ?SEV_IPRINT("await (no action) evs termination") end,
          fun(_) -> ok end},

         %% Send them a skip message, causing the evaluators to
         %% die with a skip reason.
         {fun() -> ?SEV_IPRINT("await (send skip message) evs termination") end,
          fun(#ev{pid = Pid}) -> Pid ! skip end},

         %% And if nothing else works, try to kill the remaining evaluators
         {fun() -> ?SEV_IPRINT("await (issue exit kill) evs termination") end,
          fun(#ev{pid = Pid}) -> exit(Pid, kill) end}],

    await_evs_terminated(Evs, Instructions).

await_evs_terminated([], _) ->
    ok;
await_evs_terminated(Evs, []) ->
    {error, {failed_terminated, [P||#ev{pid=P} <- Evs]}};
await_evs_terminated(Evs, [{Inform, Command}|Instructions]) ->
    Inform(),
    lists:foreach(Command, Evs),
    RemEvs = await_evs_termination(Evs),
    await_evs_terminated(RemEvs, Instructions).

await_evs_termination(Evs) ->
    await_evs_termination(Evs, 2000).

await_evs_termination([], _Timeout) ->
    [];
await_evs_termination(Evs, Timeout) ->
    T = t(),
    receive
        {'DOWN', _MRef, process, Pid, _Reason} ->
            %% ?SEV_IPRINT("await_evs_termination -> DOWN: "
            %%             "~n   Pid:    ~p"
            %%             "~n   Reason: ~p", [Pid, Reason]),
            Evs2 = lists:keydelete(Pid, #ev.pid, Evs),
            await_evs_termination(Evs2, tdiff(T, t()));
        {'EXIT', Pid, _Reason} ->
            %% ?SEV_IPRINT("await_evs_termination -> EXIT: "
            %%             "~n   Pid:    ~p"
            %%             "~n   Reason: ~p", [Pid, Reason]),
            Evs2 = lists:keydelete(Pid, #ev.pid, Evs),
            await_evs_termination(Evs2, tdiff(T, t()))

    after Timeout ->
            Evs
    end.

        
await_finish_fail(Pid, Reason, Evs, OK, Fails) ->
    case lists:keysearch(Pid, #ev.pid, Evs) of
        {value, #ev{name = Name}} ->
            iprint("evaluator '~s' (~p) failed", [Name, Pid]),
            NewEvs = lists:keydelete(Pid, #ev.pid, Evs),
            {NewEvs, OK, [{Pid, Reason}|Fails]};
        false ->
            case lists:member(Pid, OK) of
                true ->
                    ok;
                false ->
                    iprint("unknown process ~p died: "
                           "~n   ~p", [Pid, Reason])
            end,
            {Evs, OK, Fails}
    end.



%% ============================================================================

-spec announce_start(To) -> ok when
      To :: pid().

announce_start(To) ->
    announce(To, ?ANNOUNCEMENT_START, ?START_SLOGAN).

-spec announce_start(To, Extra) -> ok when
      To    :: pid(),
      Extra :: term().

announce_start(To, Extra) ->
    announce(To, ?ANNOUNCEMENT_START, ?START_SLOGAN, Extra).


%% ============================================================================

-spec announce_continue(To, Slogan) -> ok when
      To     :: pid(),
      Slogan :: atom().

announce_continue(To, Slogan) ->
    announce_continue(To, Slogan, ?EXTRA_NOTHING).

-spec announce_continue(To, Slogan, Extra) -> ok when
      To     :: pid(),
      Slogan :: atom(),
      Extra  :: term().

announce_continue(To, Slogan, Extra) ->
    announce(To, ?ANNOUNCEMENT_CONTINUE, Slogan, Extra).


%% ============================================================================

-spec announce_ready(To, Slogan) -> ok when
      To     :: pid(),
      Slogan :: atom().

announce_ready(To, Slogan) ->
    announce_ready(To, Slogan, ?EXTRA_NOTHING).

-spec announce_ready(To, Slogan, Extra) -> ok when
      To     :: pid(),
      Slogan :: atom(),
      Extra  :: term().

announce_ready(To, Slogan, Extra) ->
    announce(To, ?ANNOUNCEMENT_READY, Slogan, Extra).


%% ============================================================================

-spec announce_terminate(To) -> ok when
      To     :: pid().

announce_terminate(To) ->
    announce(To, ?ANNOUNCEMENT_TERMINATE, ?TERMINATE_SLOGAN).


%% ============================================================================

-spec announce(To, Announcement, Slogan) -> ok when
      To           :: pid(),
      Announcement :: atom(),
      Slogan       :: atom().

announce(To, Announcement, Slogan) ->
    announce(To, Announcement, Slogan, ?EXTRA_NOTHING).

-spec announce(To, Announcement, Slogan, Extra) -> ok when
      To           :: pid(),
      Announcement :: atom(),
      Slogan       :: atom(),
      Extra        :: term().

announce(To, Announcement, Slogan, Extra) 
  when is_pid(To) andalso 
       is_atom(Announcement) andalso  
       is_atom(Slogan) ->
    %% iprint("announce -> entry with: "
    %%        "~n   To:           ~p"
    %%        "~n   Announcement: ~p"
    %%        "~n   Slogan:       ~p"
    %%        "~n   Extra:        ~p", 
    %%        [To, Announcement, Slogan, Extra]),
    To ! {Announcement, self(), Slogan, Extra},
    ok.



%% ============================================================================

-spec await_start() -> Pid | {Pid, Extra} when
      Pid   :: pid(),
      Extra :: term().

await_start() ->
    await_start(any).

-spec await_start(Pid) -> Pid | {Pid, Extra} when
      Pid   :: pid(),
      Extra :: term().

await_start(P) when is_pid(P) orelse (P =:= any) ->
    case await(P, ?START_NAME_NONE, ?ANNOUNCEMENT_START, ?START_SLOGAN, []) of
        {ok, Any} when is_pid(P) ->
            Any;
        {ok, Pid} when is_pid(Pid) andalso (P =:= any) ->
            Pid;
        {ok, {Pid, _} = OK} when is_pid(Pid) andalso (P =:= any) ->
            OK
    end.


%% ============================================================================

-spec await_continue(From, Name, Slogan) -> ok | {ok, Extra} | {error, Reason} when 
      From   :: pid(),
      Name   :: atom(),
      Slogan :: atom(),
      Extra  :: term(),
      Reason :: term().

await_continue(From, Name, Slogan) ->
    await_continue(From, Name, Slogan, []).

-spec await_continue(From, Name, Slogan, OtherPids) -> 
                         ok | {ok, Extra} | {error, Reason} when 
      From      :: pid(),
      Name      :: atom(),
      Slogan    :: atom(),
      OtherPids :: [{pid(), atom()}],
      Extra     :: term(),
      Reason    :: term().

await_continue(From, Name, Slogan, OtherPids) 
  when is_pid(From) andalso
       is_atom(Name) andalso
       is_atom(Slogan) andalso
       is_list(OtherPids) ->
    await(From, Name, ?ANNOUNCEMENT_CONTINUE, Slogan, OtherPids).



%% ============================================================================

-spec await_ready(From, Name, Slogan) -> ok | {ok, Extra} | {error, Reason} when 
      From   :: pid(),
      Name   :: atom(),
      Slogan :: atom(),
      Extra  :: term(),
      Reason :: term().

await_ready(From, Name, Slogan) ->
    await_ready(From, Name, Slogan, []).

-spec await_ready(From, Name, Slogan, OtherPids) -> 
                         ok | {ok, Extra} | {error, Reason} when 
      From      :: pid(),
      Name      :: atom(),
      Slogan    :: atom(),
      OtherPids :: [{pid(), atom()}],
      Extra     :: term(),
      Reason    :: term().

await_ready(From, Name, Slogan, OtherPids) 
  when is_pid(From) andalso
       is_atom(Name) andalso
       is_atom(Slogan) andalso
       is_list(OtherPids) ->
    await(From, Name, ?ANNOUNCEMENT_READY, Slogan, OtherPids).



%% ============================================================================

-spec await_terminate(Pid, Name) -> ok | {error, Reason} when
      Pid    :: pid(),
      Name   :: atom(),
      Reason :: term().

await_terminate(Pid, Name) when is_pid(Pid) andalso is_atom(Name) ->
    await_terminate(Pid, Name, []).

-spec await_terminate(Pid, Name, OtherPids) -> ok | {error, Reason} when
      Pid       :: pid(),
      Name      :: atom(),
      OtherPids :: [{pid(), atom()}],
      Reason    :: term().

await_terminate(Pid, Name, OtherPids) ->
    await(Pid, Name, ?ANNOUNCEMENT_TERMINATE, ?TERMINATE_SLOGAN, OtherPids).


%% ============================================================================

-spec await_termination(Pid) -> ok | {error, Reason} when
      Pid    :: pid(),
      Reason :: term().

await_termination(Pid) when is_pid(Pid) ->
    await_termination(Pid, any).

-spec await_termination(Pid, ExpReason) -> ok | {error, Reason} when
      Pid       :: pid(),
      ExpReason :: term(),
      Reason    :: term().

await_termination(Pid, ExpReason) ->
    receive
        {'DOWN', _, process, Pid, _} when (ExpReason =:= any) ->
            ok;
        {'DOWN', _, process, Pid, Reason} when (ExpReason =:= Reason) ->
            ok;
        {'DOWN', _, process, Pid, Reason} ->
            {error, {unexpected_reason, ExpReason, Reason}}
    end.


%% ============================================================================

%% We expect a message (announcement) from Pid, but we also watch for DOWN from 
%% both Pid and OtherPids, in which case the test has failed!

-spec await(ExpPid, Name, Announcement, Slogan, OtherPids) -> 
                   ok | {ok, Extra} | {error, Reason} when
      ExpPid       :: any | pid(),
      Name         :: atom(),
      Announcement :: atom(),
      Slogan       :: atom(),
      OtherPids    :: [{pid(), atom()}],
      Extra        :: term(),
      Reason       :: term().

await(ExpPid, Name, Announcement, Slogan, OtherPids) 
  when (is_pid(ExpPid) orelse (ExpPid =:= any)) andalso 
       is_atom(Name) andalso 
       is_atom(Announcement) andalso 
       is_atom(Slogan) andalso 
       is_list(OtherPids) ->
    receive
        skip ->
            %% This means that another evaluator has issued a skip,
            %% and we have been instructed to terminate as a result.
            ?LIB:skip(command);
        {Announcement, Pid, Slogan, ?EXTRA_NOTHING} when (ExpPid =:= any) ->
            {ok, Pid};
        {Announcement, Pid, Slogan, Extra} when (ExpPid =:= any) ->
            {ok, {Pid, Extra}};
        {Announcement, Pid, Slogan, ?EXTRA_NOTHING} when (Pid =:= ExpPid) ->
            ok;
        {Announcement, Pid, Slogan, Extra} when (Pid =:= ExpPid) ->
            {ok, Extra};
        {'DOWN', _, process, Pid, {skip, SkipReason}} when (Pid =:= ExpPid) ->
            iprint("Unexpected SKIP from ~w (~p): "
                   "~n   ~p", [Name, Pid, SkipReason]),
            ?LIB:skip({Name, SkipReason});
        {'DOWN', _, process, Pid, Reason} when (Pid =:= ExpPid) ->
            eprint("Unexpected DOWN from ~w (~p): "
                   "~n   ~p", [Name, Pid, Reason]),
            {error, {unexpected_exit, Name, Reason}};
        {'DOWN', _, process, OtherPid, Reason} ->
            case check_down(OtherPid, Reason, OtherPids) of
                ok ->
                    iprint("DOWN from unknown process ~p: "
                           "~n      ~p"
                           "~n   when"
                           "~n      OtherPids: "
                           "~n         ~p", [OtherPid, Reason, OtherPids]),
                    await(ExpPid, Name, Announcement, Slogan, OtherPids);
                {error, _} = ERROR ->
                    ERROR
            end
    after infinity -> % For easy debugging, just change to some valid time (5000)
            iprint("await -> timeout for msg from ~p (~w): "
                   "~n   Announcement: ~p"
                   "~n   Slogan:       ~p"
                   "~nwhen"
                   "~n   Messages:     ~p", 
                   [ExpPid, Name, Announcement, Slogan, pi(messages)]),
            await(ExpPid, Name, Announcement, Slogan, OtherPids)
    end.

pi(Item) ->
    pi(self(), Item).

pi(Pid, Item) ->
    {Item, Info} = process_info(Pid, Item),
    Info.

check_down(Pid, DownReason, Pids) ->
    case lists:keymember(Pid, 1, Pids) of
        {value, {_, Name}} ->
            eprint("Unexpected DOWN from ~w (~p): "
                   "~n   ~p", [Name, Pid, DownReason]),
            {error, {unexpected_exit, Name, DownReason}};
        false ->
            ok
    end.


%% ============================================================================

f(F, A) ->
    lists:flatten(io_lib:format(F, A)).


iprint(F, A) ->
    print("", F, A).

eprint(F, A) ->
    print("<ERROR> ", F, A).

print(Prefix, F, A) ->
    %% The two prints is to get the output both in the shell (for when
    %% "personal" testing is going on) and in the logs.
    IDStr = 
        case get(sname) of
            undefined ->
                %% This means its not an evaluator, 
                %% or a named process. Instead its 
                %% most likely the test case itself, 
                %% so skip the name and the pid.
                "";
            SName ->
                f("[~s][~p]", [SName, self()])
        end,
    ?LOGGER:format("[~s]~s ~s" ++ F,
                   [?LIB:formated_timestamp(), IDStr, Prefix | A]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t() ->
    os:timestamp().


tdiff({A1, B1, C1} = _T1x, {A2, B2, C2} = _T2x) ->
    T1 = A1*1000000000+B1*1000+(C1 div 1000), 
    T2 = A2*1000000000+B2*1000+(C2 div 1000), 
    T2 - T1.

