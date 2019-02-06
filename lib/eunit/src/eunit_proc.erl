%% Licensed under the Apache License, Version 2.0 (the "License"); you may
%% not use this file except in compliance with the License. You may obtain
%% a copy of the License at <http://www.apache.org/licenses/LICENSE-2.0>
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% Alternatively, you may use this file under the terms of the GNU Lesser
%% General Public License (the "LGPL") as published by the Free Software
%% Foundation; either version 2.1, or (at your option) any later version.
%% If you wish to allow use of your version of this file only under the
%% terms of the LGPL, you should delete the provisions above and replace
%% them with the notice and other provisions required by the LGPL; see
%% <http://www.gnu.org/licenses/>. If you do not delete the provisions
%% above, a recipient may use your version of this file under the terms of
%% either the Apache License or the LGPL.
%%
%% @author Richard Carlsson <carlsson.richard@gmail.com>
%% @copyright 2006 Richard Carlsson
%% @private
%% @see eunit
%% @doc Test runner process tree functions

-module(eunit_proc).

-include("eunit.hrl").
-include("eunit_internal.hrl").

-export([start/4]).

%% This must be exported; see new_group_leader/1 for details.
-export([group_leader_process/1]).

-record(procstate, {ref, id, super, insulator, parent, order}).


%% Spawns test process and returns the process Pid; sends {done,
%% Reference, Pid} to caller when finished. See the function
%% wait_for_task/2 for details about the need for the reference.
%%
%% The `Super' process receives a stream of status messages; see
%% message_super/3 for details.

start(Tests, Order, Super, Reference)
  when is_pid(Super), is_reference(Reference) ->
    St = #procstate{ref = Reference,
		    id = [],
		    super = Super,
		    order = Order},
    spawn_group(local, #group{tests = Tests}, St).


%% Status messages sent to the supervisor process. (A supervisor does
%% not have to act on these messages - it can e.g. just log them, or
%% even discard them.) Each status message has the following form:
%%
%%   {status, Id, Info}
%%
%% where Id identifies the item that the message pertains to, and the
%% Info part can be one of:
%%
%%   {progress, 'begin', {test | group, Data}}
%%       indicates that the item has been entered, and what type it is;
%%       Data is [{desc,binary()}, {source,Source}, {line,integer()}] for
%%       a test, and [{desc,binary()}, {spawn,SpawnType},
%%       {order,OrderType}] for a group.
%%
%%   {progress, 'end', {Status, Data}}
%%       Status = 'ok' | {error, Exception} | {skipped, Cause} | integer()
%%       Data = [{time,integer()}, {output,binary()}]
%%
%%       where Time is measured in milliseconds and Output is the data
%%       written to the standard output stream during the test; if
%%       Status is {skipped, Cause}, then Cause is a term thrown from
%%       eunit_test:run_testfun/1. For a group item, the Status field is
%%       the number of immediate subitems of the group; this helps the
%%       collation of results. Failure for groups is always signalled
%%       through a cancel message, not through the Status field.
%%
%%   {cancel, Descriptor}
%%       where Descriptor can be:
%%           timeout            a timeout occurred
%%           {blame, Id}        forced to terminate because of item `Id'
%%           {abort, Cause}     the test or group failed to execute
%%           {exit, Reason}     the test process terminated unexpectedly
%%           {startup, Reason}  failed to start a remote test process
%%
%%       where Cause is a term thrown from eunit_data:enter_context/4 or
%%       from eunit_data:iter_next/2, and Reason is an exit term from a
%%       crashed process
%%
%% Note that due to concurrent (and possibly distributed) execution,
%% there are *no* strict ordering guarantees on the status messages,
%% with one exception: a 'begin' message will always arrive before its
%% corresponding 'end' message.

message_super(Id, Info, St) ->
    St#procstate.super ! {status, Id, Info}.


%% @TODO implement synchronized mode for insulator/child execution

%% Ideas for synchronized mode:
%%
%% * At each "program point", i.e., before entering a test, entering a
%% group, or leaving a group, the child will synchronize with the
%% insulator to make sure it is ok to proceed.
%%
%% * The insulator can receive controlling messages from higher up in
%% the hierarchy, telling it to pause, resume, single-step, repeat, etc.
%%
%% * Synchronization on entering/leaving groups is necessary in order to
%% get control over things such as subprocess creation/termination and
%% setup/cleanup, making it possible to, e.g., repeat all the tests
%% within a particular subprocess without terminating and restarting it,
%% or repeating tests without repeating the setup/cleanup.
%%
%% * Some tests that depend on state will not be possible to repeat, but
%% require a fresh context setup. There is nothing that can be done
%% about this, and the many tests that are repeatable should not be
%% punished because of it. The user must decide which level to restart.
%%
%% * Question: How propagate control messages down the hierarchy
%% (preferably only to the correct insulator process)? An insulator does
%% not currenctly know whether its child process has spawned subtasks.
%% (The "supervisor" process does not know the Pids of the controlling
%% insulator processes in the tree, and it probably should not be
%% responsible for this anyway.)


%% ---------------------------------------------------------------------
%% Process tree primitives

%% A "task" consists of an insulator process and a child process which
%% handles the actual work. When the child terminates, the insulator
%% process sends {done, Reference, self()} to the process which started
%% the task (the "parent"). The child process is given a State record
%% which contains the process id:s of the parent, the insulator, and the
%% supervisor.

%% @spec (Type, (#procstate{}) -> () -> term(), #procstate{}) -> pid()
%%   Type = local | {remote, Node::atom()}

start_task(Type, Fun, St0) ->
    St = St0#procstate{parent = self()},
    %% (note: the link here is mainly to propagate signals *downwards*,
    %% so that the insulator can detect if the process that started the
    %% task dies before the task is done)
    F = fun () -> insulator_process(Type, Fun, St) end,
    case Type of
	local ->
	    %% we assume (at least for now) that local spawns can never
	    %% fail in such a way that the process does not start, so a
	    %% new local insulator does not need to synchronize here
	    spawn_link(F);
	{remote, Node} ->
	    Pid = spawn_link(Node, F),
	    %% See below for the need for the {ok, Reference, Pid}
	    %% message.
	    Reference = St#procstate.ref,
	    Monitor = erlang:monitor(process, Pid),
	    %% (the DOWN message is guaranteed to arrive after any
	    %% messages sent by the process itself)
	    receive
		{ok, Reference, Pid} ->
		    Pid;
		{'DOWN', Monitor, process, Pid, Reason} ->
		    %% send messages as if the insulator process was
		    %% started, but terminated on its own accord
		    Msg = {startup, Reason},
		    message_super(St#procstate.id, {cancel, Msg}, St),
		    self() ! {done, Reference, Pid}
	    end,
	    erlang:demonitor(Monitor, [flush]),
	    Pid
    end.

%% Relatively simple, and hopefully failure-proof insulator process
%% (This is cleaner than temporarily setting up the caller to trap
%% signals, and does not affect the caller's mailbox or other state.)
%%
%% We assume that nobody does a 'kill' on an insulator process - if that
%% should happen, the test framework will hang since the insulator will
%% never send a reply; see below for more.
%%
%% Note that even if the insulator process itself never fails, it is
%% still possible that it does not start properly, if it is spawned
%% remotely (e.g., if the remote node is down). Therefore, remote
%% insulators must always immediately send an {ok, Reference, self()}
%% message to the parent as soon as it is spawned.

%% @spec (Type, Fun::() -> term(), St::#procstate{}) -> ok
%%  Type = local | {remote, Node::atom()}

insulator_process(Type, Fun, St0) ->
    process_flag(trap_exit, true),
    Parent = St0#procstate.parent,
    if Type =:= local -> ok;
       true -> Parent ! {ok, St0#procstate.ref, self()}
    end,
    St = St0#procstate{insulator = self()},
    Child = spawn_link(fun () -> child_process(Fun(St), St) end),
    insulator_wait(Child, Parent, [], St).

%% Normally, child processes exit with the reason 'normal' even if the
%% executed tests failed (by throwing exceptions), since the tests are
%% executed within a try-block. Child processes can terminate abnormally
%% by the following reasons:
%%   1) an error in the processing of the test descriptors (a malformed
%%      descriptor, failure in a setup, cleanup or initialization, a
%%      missing module or function, or a failing generator function);
%%   2) an internal error in the test running framework itself;
%%   3) receiving a non-trapped error signal as a consequence of running
%%      test code.
%% Those under point 1 are "expected errors", handled specially in the
%% protocol, while the other two are unexpected errors. (Since alt. 3
%% implies that the test neither reported success nor failure, it can
%% never be considered "proper" behaviour of a test.) Abnormal
%% termination is reported to the supervisor process but otherwise does
%% not affect the insulator compared to normal termination. Child
%% processes can also be killed abruptly by their insulators, in case of
%% a timeout or if a parent process dies.
%%
%% The insulator is the group leader for the child process, and gets all
%% of its standard I/O. The output is buffered and associated with the
%% currently active test or group, and is sent along with the 'end'
%% progress message when the test or group has finished.

insulator_wait(Child, Parent, Buf, St) ->
    receive
	{child, Child, Id, {'begin', Type, Data}} ->
	    message_super(Id, {progress, 'begin', {Type, Data}}, St),
	    insulator_wait(Child, Parent, [[] | Buf], St);
	{child, Child, Id, {'end', Status, Time}} ->
	    Data = [{time, Time}, {output, lists:reverse(hd(Buf))}],
	    message_super(Id, {progress, 'end', {Status, Data}}, St),
	    insulator_wait(Child, Parent, tl(Buf), St);
	{child, Child, Id, {skipped, Reason}} ->
	    %% this happens when a subgroup fails to enter the context
	    message_super(Id, {cancel, {abort, Reason}}, St),
	    insulator_wait(Child, Parent, Buf, St);
	{child, Child, Id, {abort, Cause}} ->
	    %% this happens when the child code threw an internal
	    %% eunit_abort; the child process has already exited
	    exit_messages(Id, {abort, Cause}, St),
	    %% no need to wait for the {'EXIT',Child,_} message
	    terminate_insulator(St);
	{io_request, Child, ReplyAs, Req} ->
	    %% we only collect output from the child process itself, not
	    %% from secondary processes, otherwise we get race problems;
	    %% however, each test runs its personal group leader that
	    %% funnels all output - see the run_test() function
	    Buf1 = io_request(Child, ReplyAs, Req, hd(Buf)),
	    insulator_wait(Child, Parent, [Buf1 | tl(Buf)], St);
	{io_request, From, ReplyAs, Req} when is_pid(From) ->
	    %% (this shouldn't happen anymore, but we keep it safe)
	    %% just ensure the sender gets a reply; ignore the data
	    io_request(From, ReplyAs, Req, []),
	    insulator_wait(Child, Parent, Buf, St);
	{timeout, Child, Id} ->
	    exit_messages(Id, timeout, St),
	    kill_task(Child, St);
	{'EXIT', Child, normal} ->
	    terminate_insulator(St);
	{'EXIT', Child, Reason} ->
	    exit_messages(St#procstate.id, {exit, Reason}, St),
	    terminate_insulator(St);
	{'EXIT', Parent, _} ->
	    %% make sure child processes are cleaned up recursively
	    kill_task(Child, St)
    end.

-spec kill_task(_, _) -> no_return().

kill_task(Child, St) ->
    exit(Child, kill),
    terminate_insulator(St).

%% Unlinking before exit avoids polluting the parent process with exit
%% signals from the insulator. The child process is already dead here.

terminate_insulator(St) ->
    %% messaging/unlinking is ok even if the parent is already dead
    Parent = St#procstate.parent,
    Parent ! {done, St#procstate.ref, self()},
    unlink(Parent),
    exit(normal).

%% send cancel messages for the Id of the "causing" item, and also for
%% the Id of the insulator itself, if they are different
exit_messages(Id, Cause, St) ->
    %% the message for the most specific Id is always sent first
    message_super(Id, {cancel, Cause}, St),
    case St#procstate.id of
	Id -> ok;
	Id1 -> message_super(Id1, {cancel, {blame, Id}}, St)
    end.

%% Child processes send all messages via the insulator to ensure proper
%% sequencing with timeouts and exit signals.

message_insulator(Data, St) ->
    St#procstate.insulator ! {child, self(), St#procstate.id, Data}.

%% Timeout handling

set_timeout(Time, St) ->
    erlang:send_after(Time, St#procstate.insulator,
		      {timeout, self(), St#procstate.id}).

clear_timeout(Ref) ->
    erlang:cancel_timer(Ref).

with_timeout(undefined, Default, F, St) ->
    with_timeout(Default, F, St);
with_timeout(Time, _Default, F, St) ->
    with_timeout(Time, F, St).

with_timeout(infinity, F, _St) ->
    %% don't start timers unnecessarily
    {T0, _} = statistics(wall_clock),
    Value = F(),
    {T1, _} = statistics(wall_clock),
    {Value, T1 - T0};
with_timeout(Time, F, St) when is_integer(Time), Time > 16#FFFFffff ->
    with_timeout(16#FFFFffff, F, St);
with_timeout(Time, F, St) when is_integer(Time), Time < 0 ->
    with_timeout(0, F, St);
with_timeout(Time, F, St) when is_integer(Time) ->
    Ref = set_timeout(Time, St),
    {T0, _} = statistics(wall_clock),
    try F() of
	Value ->
	    %% we could also read the timer, but this is simpler
	    {T1, _} = statistics(wall_clock),
	    {Value, T1 - T0}
    after
	clear_timeout(Ref)
    end.

%% The normal behaviour of a child process is not to trap exit
%% signals. The testing framework is not dependent on this, however, so
%% the test code is allowed to enable signal trapping as it pleases.
%% Note that I/O is redirected to the insulator process.

%% @spec (() -> term(), #procstate{}) -> ok

child_process(Fun, St) ->
    group_leader(St#procstate.insulator, self()),
    try Fun() of
	_ -> ok
    catch
	%% the only "normal" way for a child process to bail out (e.g,
	%% when not being able to parse the test descriptor) is to throw
	%% an {eunit_abort, Reason} exception; any other exception will
	%% be reported as an unexpected termination of the test
	{eunit_abort, Cause} ->
	    message_insulator({abort, Cause}, St),
	    exit(aborted)
    end.

-ifdef(TEST).
child_test_() ->
    [{"test processes do not trap exit signals",
      ?_assertMatch(false, process_flag(trap_exit, false))}].
-endif.

%% @throws abortException()
%% @type abortException() = {eunit_abort, Cause::term()}

abort_task(Cause) ->
    throw({eunit_abort, Cause}).

%% Typically, the process that executes this code is not trapping
%% signals, but it might be - it is outside of our control, since test
%% code can enable or disable trapping at will. That we cannot rely on
%% process links here, is why the insulator process of a task must be
%% guaranteed to always send a reply before it terminates.
%%
%% The unique reference guarantees that we don't extract any message
%% from the mailbox unless it belongs to the test framework (and not to
%% the running tests) - it is not possible to use selective receive to
%% match only messages that are tagged with some pid out of a
%% dynamically varying set of pids. When the wait-loop terminates, no
%% such message should remain in the mailbox.

wait_for_task(Pid, St) ->
    wait_for_tasks(sets:from_list([Pid]), St).

wait_for_tasks(PidSet, St) ->
    case sets:size(PidSet) of
	0 ->
	    ok;
	_ ->
	    %% (note that when we receive this message for some task, we
	    %% are guaranteed that the insulator process of the task has
	    %% already informed the supervisor about any anomalies)
	    Reference = St#procstate.ref,
	    receive
		{done, Reference, Pid} ->
		    %% (if Pid is not in the set, del_element has no
		    %% effect, so this is always safe)
		    Rest = sets:del_element(Pid, PidSet),
		    wait_for_tasks(Rest, St)
	    end
    end.

%% ---------------------------------------------------------------------
%% Separate testing process

%% TODO: Ability to stop after N failures.
%% TODO: Flow control, starting new job as soon as slot is available

tests(T, St) ->
    I = eunit_data:iter_init(T, St#procstate.id),
    case St#procstate.order of
	inorder -> tests_inorder(I, St);
	inparallel -> tests_inparallel(I, 0, St);
	{inparallel, N} when is_integer(N), N >= 0 ->
	    tests_inparallel(I, N, St)
    end.

set_id(I, St) ->
    St#procstate{id = eunit_data:iter_id(I)}.

tests_inorder(I, St) ->
    tests_inorder(I, 0, St).

tests_inorder(I, N, St) ->
    case get_next_item(I) of
	{T, I1} ->
	    handle_item(T, set_id(I1, St)),
	    tests_inorder(I1, N+1, St);
	none ->
	    N    % the return status of a group is the subtest count
    end.

tests_inparallel(I, K0, St) ->
    tests_inparallel(I, 0, St, K0, K0, sets:new()).

tests_inparallel(I, N, St, K, K0, Children) when K =< 0, K0 > 0 ->
    wait_for_tasks(Children, St),
    tests_inparallel(I, N, St, K0, K0, sets:new());
tests_inparallel(I, N, St, K, K0, Children) ->
    case get_next_item(I) of
	{T, I1} ->
	    Child = spawn_item(T, set_id(I1, St)),
	    tests_inparallel(I1, N+1, St, K - 1, K0,
			     sets:add_element(Child, Children));
	none ->
	    wait_for_tasks(Children, St),
	    N    % the return status of a group is the subtest count
    end.

%% this starts a new separate task for an inparallel-item (which might
%% be a group and in that case might cause yet another spawn in the
%% handle_group() function, but it might also be just a single test)
spawn_item(T, St0) ->
    Fun = fun (St) ->
		  fun () -> handle_item(T, St) end
	  end,
    %% inparallel-items are always spawned locally
    start_task(local, Fun, St0).

get_next_item(I) ->
    try eunit_data:iter_next(I)
    catch
	Term -> abort_task(Term)
    end.

handle_item(T, St) ->
    case T of
	#test{} -> handle_test(T, St);
	#group{} -> handle_group(T, St)
    end.

handle_test(T, St) ->
    Data = [{desc, T#test.desc}, {source, T#test.location},
	    {line, T#test.line}],
    message_insulator({'begin', test, Data}, St),

    %% each test case runs under a fresh group leader process
    G0 = group_leader(),
    Runner = self(),
    G1 = new_group_leader(Runner),
    group_leader(G1, self()),

    %% run the actual test, handling timeouts and getting the total run
    %% time of the test code (and nothing else)
    {Status, Time} = with_timeout(T#test.timeout, ?DEFAULT_TEST_TIMEOUT,
				  fun () -> run_test(T) end, St),

    %% restore group leader, get the collected output, and re-emit it so
    %% that it all seems to come from this process, and always comes
    %% before the 'end' message for this test
    group_leader(G0, self()),
    Output = group_leader_sync(G1),
    io:put_chars(Output),

    message_insulator({'end', Status, Time}, St),
    ok.

%% @spec (#test{}) -> ok | {error, eunit_lib:exception()}
%%                  | {skipped, eunit_test:wrapperError()}

run_test(#test{f = F}) ->
    try eunit_test:run_testfun(F) of
	{ok, _Value} ->
	    %% just discard the return value
	    ok;
	{error, Exception} ->
	    {error, Exception}
    catch
	throw:WrapperError -> {skipped, WrapperError}
    end.

set_group_order(#group{order = undefined}, St) ->
    St;
set_group_order(#group{order = Order}, St) ->
    St#procstate{order = Order}.

handle_group(T, St0) ->
    St = set_group_order(T, St0),
    case T#group.spawn of
	undefined ->
	    run_group(T, St);
	Type ->
	    Child = spawn_group(Type, T, St),
	    wait_for_task(Child, St)
    end.

spawn_group(Type, T, St0) ->
    Fun = fun (St) ->
		  fun () -> run_group(T, St) end
	  end,
    start_task(Type, Fun, St0).

run_group(T, St) ->
    %% note that the setup/cleanup is outside the group timeout; if the
    %% setup fails, we do not start any timers
    Timeout = T#group.timeout,
    Data = [{desc, T#group.desc}, {spawn, T#group.spawn},
	    {order, T#group.order}],
    message_insulator({'begin', group, Data}, St),
    F = fun (G) -> enter_group(G, Timeout, St) end,
    try with_context(T, F) of
	{Status, Time} ->
	    message_insulator({'end', Status, Time}, St)
    catch
	%% a throw here can come from eunit_data:enter_context/4 or from
	%% get_next_item/1; for context errors, report group as aborted,
	%% but continue processing tests
	{context_error, Why, Trace} ->
	    message_insulator({skipped, {Why, Trace}}, St)
    end,
    ok.

enter_group(T, Timeout, St) ->
    with_timeout(Timeout, ?DEFAULT_GROUP_TIMEOUT,
		 fun () -> tests(T, St) end, St).

with_context(#group{context = undefined, tests = T}, F) ->
    F(T);
with_context(#group{context = #context{} = C, tests = I}, F) ->
    eunit_data:enter_context(C, I, F).

%% Group leader process for test cases - collects I/O output requests.

new_group_leader(Runner) ->
    %% We must use spawn/3 here (with explicit module and function
    %% name), because the 'current function' status of the group leader
    %% is used by the UNDER_EUNIT macro (in eunit.hrl). If we spawn
    %% using a fun, the current function will be 'erlang:apply/2' during
    %% early process startup, which will fool the macro.
    spawn_link(?MODULE, group_leader_process, [Runner]).

group_leader_process(Runner) ->
    group_leader_loop(Runner, infinity, []).

group_leader_loop(Runner, Wait, Buf) ->
    receive
	{io_request, From, ReplyAs, Req} ->
	    P = process_flag(priority, normal),
	    %% run this part under normal priority always
	    Buf1 = io_request(From, ReplyAs, Req, Buf),
	    process_flag(priority, P),
	    group_leader_loop(Runner, Wait, Buf1);
	stop ->
	    %% quitting time: make a minimal pause, go low on priority,
	    %% set receive-timeout to zero and schedule out again
	    receive after 2 -> ok end,
	    process_flag(priority, low),
	    group_leader_loop(Runner, 0, Buf);
	_ ->
	    %% discard any other messages
	    group_leader_loop(Runner, Wait, Buf)
    after Wait ->
	    %% no more messages and nothing to wait for; we ought to
	    %% have collected all immediately pending output now
	    process_flag(priority, normal),
	    Runner ! {self(), lists:reverse(Buf)}
    end.

group_leader_sync(G) ->
    G ! stop,
    receive
	{G, Buf} -> Buf
    end.

%% Implementation of buffering I/O for group leader processes. (Note that
%% each batch of characters is just pushed on the buffer, so it needs to
%% be reversed when it is flushed.)

io_request(From, ReplyAs, Req, Buf) ->
    {Reply, Buf1} = io_request(Req, Buf),
    io_reply(From, ReplyAs, Reply),
    Buf1.

io_reply(From, ReplyAs, Reply) ->
    From ! {io_reply, ReplyAs, Reply}.

io_request({put_chars, Chars}, Buf) ->
    {ok, [Chars | Buf]};
io_request({put_chars, M, F, As}, Buf) ->
    try apply(M, F, As) of
	Chars -> {ok, [Chars | Buf]}
    catch
	C:T:S -> {{error, {C,T,S}}, Buf}
    end;
io_request({put_chars, _Enc, Chars}, Buf) ->
    io_request({put_chars, Chars}, Buf);
io_request({put_chars, _Enc, Mod, Func, Args}, Buf) ->
    io_request({put_chars, Mod, Func, Args}, Buf);
io_request({get_chars, _Enc, _Prompt, _N}, Buf) ->
    {eof, Buf};
io_request({get_chars, _Prompt, _N}, Buf) ->
    {eof, Buf};
io_request({get_line, _Prompt}, Buf) ->
    {eof, Buf};
io_request({get_line, _Enc, _Prompt}, Buf) ->
    {eof, Buf};
io_request({get_until, _Prompt, _M, _F, _As}, Buf) ->
    {eof, Buf};
io_request({setopts, _Opts}, Buf) ->
    {ok, Buf};
io_request(getopts, Buf) ->
    {{error, enotsup}, Buf};
io_request({get_geometry,columns}, Buf) ->
    {{error, enotsup}, Buf};
io_request({get_geometry,rows}, Buf) ->
    {{error, enotsup}, Buf};
io_request({requests, Reqs}, Buf) ->
    io_requests(Reqs, {ok, Buf});
io_request(_, Buf) ->
    {{error, request}, Buf}.

io_requests([R | Rs], {ok, Buf}) ->
    io_requests(Rs, io_request(R, Buf));
io_requests(_, Result) ->
    Result.

-ifdef(TEST).
io_error_test_() ->
    [?_assertMatch({error, enotsup}, io:getopts()),
     ?_assertMatch({error, enotsup}, io:columns()),
     ?_assertMatch({error, enotsup}, io:rows())].
-endif.
