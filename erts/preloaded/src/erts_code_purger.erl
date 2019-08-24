%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2016-2018. All Rights Reserved.
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
-module(erts_code_purger).

%% Purpose : Implement system process erts_code_purger
%%           to handle code module purging.

-export([start/0, purge/1, soft_purge/1, pending_purge_lambda/3,
	 finish_after_on_load/2]).

-spec start() -> no_return().
start() ->
    register(erts_code_purger, self()),
    process_flag(trap_exit, true),
    wait_for_request().

wait_for_request() ->
    handle_request(receive Msg -> Msg end, []).

handle_request({purge, Mod, From, Ref}, Reqs) when is_atom(Mod), is_pid(From) ->
    {Res, NewReqs} = do_purge(Mod, Reqs),
    From ! {reply, purge, Res, Ref},
    check_requests(NewReqs);
handle_request({soft_purge, Mod, From, Ref}, Reqs) when is_atom(Mod), is_pid(From) ->
    {Res, NewReqs} = do_soft_purge(Mod, Reqs),
    From ! {reply, soft_purge, Res, Ref},
    check_requests(NewReqs);
handle_request({finish_after_on_load, {Mod,Keep}, From, Ref}, Reqs)
  when is_atom(Mod), is_boolean(Keep), is_pid(From) ->
    NewReqs = do_finish_after_on_load(Mod, Keep, Reqs),
    From ! {reply, finish_after_on_load, ok, Ref},
    check_requests(NewReqs);
handle_request({test_purge, Mod, From, Type, Ref}, Reqs) when is_atom(Mod), is_pid(From) ->
    NewReqs = do_test_purge(Mod, From, Type, Ref, Reqs),
    check_requests(NewReqs);
handle_request(_Garbage, Reqs) ->
    check_requests(Reqs).

check_requests([]) ->
    wait_for_request();
check_requests([R|Rs]) ->
    handle_request(R, Rs).

%%
%% Processes that tries to call a fun that belongs to
%% a module that currently is being purged will end
%% up here (pending_purge_lambda) in a suspended state.
%% When the purge operation completes or aborts (soft
%% purge that failed) these processes will be resumed.
%%
pending_purge_lambda(_Module, Fun, Args) ->
    %%
    %% When the process is resumed, the following
    %% scenarios exist:
    %% * The code that the fun refers to is still
    %%   there due to a failed soft purge. The
    %%   call to the fun will succeed via apply/2.
    %% * The code was purged, and a current version
    %%   of the module is loaded which does not
    %%   contain this fun. The call will result
    %%   in an exception being raised.
    %% * The code was purged, and no current
    %%   version of the module is loaded. An attempt
    %%   to load the module (via the error_handler)
    %%   will be made. This may or may not succeed.
    %%   If the module is loaded, it may or may
    %%   not contain the fun. The call will
    %%   succeed if the error_handler was able
    %%   to load the module and loaded module
    %%   contains this fun; otherwise, an exception
    %%   will be raised.
    %%
    apply(Fun, Args).

%% purge(Module)
%%  Kill all processes running code from *old* Module, and then purge the
%%  module. Return {WasOld, DidKill}:
%%  {false, false} there was no old module to purge
%%  {true, false} module purged, no process killed
%%  {true, true} module purged, at least one process killed

purge(Mod) when is_atom(Mod) ->
    Ref = make_ref(),
    erts_code_purger ! {purge, Mod, self(), Ref},
    receive
	{reply, purge, Result, Ref} ->
	    Result
    end.

do_purge(Mod, Reqs) ->
    case erts_internal:purge_module(Mod, prepare) of
	false ->
	    {{false, false}, Reqs};
	true ->
	    {DidKill, NewReqs} = check_proc_code(erlang:processes(),
						 Mod, true, Reqs),
	    true = erts_internal:purge_module(Mod, complete),
	    {{true, DidKill}, NewReqs}
    end.

%% soft_purge(Module)
%% Purge old code only if no procs remain that run old code.
%% Return true in that case, false if procs remain (in this
%% case old code is not purged)

soft_purge(Mod) ->
    Ref = make_ref(),
    erts_code_purger ! {soft_purge, Mod, self(), Ref},
    receive
	{reply, soft_purge, Result, Ref} ->
	    Result
    end.

do_soft_purge(Mod, Reqs) ->
    case erts_internal:purge_module(Mod, prepare) of
	false ->
	    {true, Reqs};
	true ->
	    {PurgeOp, NewReqs} = check_proc_code(erlang:processes(),
						 Mod, false, Reqs),
	    {erts_internal:purge_module(Mod, PurgeOp), NewReqs}
    end.

%% finish_after_on_load(Module, Keep)
%% Finish after running on_load function. If Keep is false,
%% purge the code for the on_load function.

finish_after_on_load(Mod, Keep) ->
    Ref = make_ref(),
    erts_code_purger ! {finish_after_on_load, {Mod,Keep}, self(), Ref},
    receive
	{reply, finish_after_on_load, Result, Ref} ->
	    Result
    end.

do_finish_after_on_load(Mod, Keep, Reqs) ->
    erlang:finish_after_on_load(Mod, Keep),
    case Keep of
	true ->
	    Reqs;
	false ->
	    case erts_internal:purge_module(Mod, prepare_on_load) of
		false ->
		    Reqs;
		true ->
		    {_DidKill, NewReqs} =
			check_proc_code(erlang:processes(),
					Mod, true, Reqs),
		    true = erts_internal:purge_module(Mod, complete),
		    NewReqs
	    end
    end.


%%
%% check_proc_code(Pids, Mod, Hard, Preqs) - Send asynchronous
%%   requests to all processes to perform a check_process_code
%%   operation. Each process will check their own state and
%%   reply with the result. If 'Hard' equals
%%   - true, processes that refer 'Mod' will be killed. If
%%     any processes were killed true is returned; otherwise,
%%     false.
%%   - false, and any processes refer 'Mod', 'abort' will
%%     be returned; otherwise, 'complete'.
%%
%%   We only allow ?MAX_CPC_NO_OUTSTANDING_KILLS
%%   outstanding kills. This both in order to avoid flooding
%%   our message queue with 'DOWN' messages and limiting the
%%   amount of memory used to keep references to all
%%   outstanding kills.
%%

-define(MAX_CPC_NO_OUTSTANDING_KILLS, 10).

-record(cpc_static, {hard, module, tag, purge_requests}).

-record(cpc_kill, {outstanding = [],
		   no_outstanding = 0,
		   waiting = [],
		   killed = false}).

check_proc_code(Pids, Mod, Hard, PReqs) ->
    Tag = erlang:make_ref(),
    CpcS = #cpc_static{hard = Hard,
		       module = Mod,
		       tag = Tag,
		       purge_requests = PReqs},
    cpc_receive(CpcS, cpc_init(CpcS, Pids, 0), #cpc_kill{}, []).

cpc_receive(#cpc_static{hard = true} = CpcS,
	    0,
	    #cpc_kill{outstanding = [], waiting = [], killed = Killed},
	    PReqs) ->
    %% No outstanding cpc requests. We did a hard check, so result is
    %% whether or not we killed any processes...
    cpc_result(CpcS, PReqs, Killed);
cpc_receive(#cpc_static{hard = false} = CpcS, 0, _KillState, PReqs) ->
    %% No outstanding cpc requests and we did a soft check that succeeded...
    cpc_result(CpcS, PReqs, complete);
cpc_receive(#cpc_static{tag = Tag} = CpcS, NoReq, KillState0, PReqs) ->
    receive
	{check_process_code, {Tag, _Pid}, false} ->
	    %% Process not referring the module; done with this process...
	    cpc_receive(CpcS, NoReq-1, KillState0, PReqs);
	{check_process_code, {Tag, Pid}, true} ->
	    %% Process referring the module...
	    case CpcS#cpc_static.hard of
		false ->
		    %% ... and soft check. The whole operation failed so
		    %% no point continuing; fail straight away. Garbage
		    %% messages from this session will be ignored
		    %% by following sessions...
		    cpc_result(CpcS, PReqs, abort);
		true ->
		    %% ... and hard check; schedule kill of it...
		    KillState1 = cpc_sched_kill(Pid, KillState0),
		    cpc_receive(CpcS, NoReq-1, KillState1, PReqs)
	    end;
	{'DOWN', MonRef, process, _, _} ->
	    KillState1 = cpc_handle_down(MonRef, KillState0),
	    cpc_receive(CpcS, NoReq, KillState1, PReqs);
	PReq when element(1, PReq) == purge;
		  element(1, PReq) == soft_purge;
		  element(1, PReq) == test_purge ->
	    %% A new purge request; save it until later...
	    cpc_receive(CpcS, NoReq, KillState0, [PReq | PReqs]);
	_Garbage ->
	    %% Garbage message; ignore it...
	    cpc_receive(CpcS, NoReq, KillState0, PReqs)
    end.

cpc_result(#cpc_static{purge_requests = PReqs}, NewPReqs, Res) ->
    {Res, PReqs ++ cpc_reverse(NewPReqs)}.

cpc_reverse([_] = L) -> L;
cpc_reverse(Xs) -> cpc_reverse(Xs, []).

cpc_reverse([], Ys) -> Ys;
cpc_reverse([X|Xs], Ys) -> cpc_reverse(Xs, [X|Ys]).

cpc_handle_down(R, #cpc_kill{outstanding = Rs,
			     no_outstanding = N} = KillState0) ->
    try
	NewOutst = cpc_list_rm(R, Rs),
	KillState1 = KillState0#cpc_kill{outstanding = NewOutst,
					 no_outstanding = N-1},
	cpc_sched_kill_waiting(KillState1)
    catch
	throw : undefined -> %% Triggered by garbage message...
	    KillState0
    end.

cpc_list_rm(_R, []) ->
    throw(undefined);
cpc_list_rm(R, [R|Rs]) ->
    Rs;
cpc_list_rm(R0, [R1|Rs]) ->
    [R1|cpc_list_rm(R0, Rs)].

cpc_sched_kill_waiting(#cpc_kill{waiting = []} = KillState) ->
    KillState;
cpc_sched_kill_waiting(#cpc_kill{outstanding = Rs,
				 no_outstanding = N,
				 waiting = [P|Ps]} = KillState) ->
    R = erlang:monitor(process, P),
    exit(P, kill),
    KillState#cpc_kill{outstanding = [R|Rs],
		       no_outstanding = N+1,
		       waiting = Ps,
		       killed = true}.

cpc_sched_kill(Pid, #cpc_kill{no_outstanding = N, waiting = Pids} = KillState)
  when N >= ?MAX_CPC_NO_OUTSTANDING_KILLS ->
    KillState#cpc_kill{waiting = [Pid|Pids]};
cpc_sched_kill(Pid,
	       #cpc_kill{outstanding = Rs, no_outstanding = N} = KillState) ->
    R = erlang:monitor(process, Pid),
    exit(Pid, kill),
    KillState#cpc_kill{outstanding = [R|Rs],
		       no_outstanding = N+1,
		       killed = true}.

cpc_request(#cpc_static{tag = Tag, module = Mod}, Pid) ->
    erts_internal:check_process_code(Pid, Mod, [{async, {Tag, Pid}}]).

cpc_init(_CpcS, [], NoReqs) ->
    NoReqs;
cpc_init(CpcS, [Pid|Pids], NoReqs) ->
    cpc_request(CpcS, Pid),
    cpc_init(CpcS, Pids, NoReqs+1).

% end of check_proc_code() implementation.

%%
%% FOR TESTING ONLY
%%
%% do_test_purge() is for testing only. The purge is done
%% as usual, but the tester can control when to enter the
%% specific phases.
%%
do_test_purge(Mod, From, true, Ref, Reqs) ->
    {Res, NewReqs} = do_test_hard_purge(Mod, From, Ref, Reqs),
    From ! {test_purge, Res, Ref},
    NewReqs;
do_test_purge(Mod, From, false, Ref, Reqs) ->
    {Res, NewReqs} = do_test_soft_purge(Mod, From, Ref, Reqs),
    From ! {test_purge, Res, Ref},
    NewReqs;
do_test_purge(_, _, _, _, Reqs) ->
    Reqs.

do_test_soft_purge(Mod, From, Ref, Reqs) ->
    PrepRes = erts_internal:purge_module(Mod, prepare),
    TestRes = test_progress(started, From, Ref, ok),
    case PrepRes of
	false ->
	    _ = test_progress(continued, From, Ref, TestRes),
	    {true, Reqs};
	true ->
	    {PurgeOp, NewReqs} = check_proc_code(erlang:processes(),
						 Mod, false, Reqs),
	    _ = test_progress(continued, From, Ref, TestRes),
	    {erts_internal:purge_module(Mod, PurgeOp), NewReqs}
    end.

do_test_hard_purge(Mod, From, Ref, Reqs) ->
    PrepRes = erts_internal:purge_module(Mod, prepare),
    TestRes = test_progress(started, From, Ref, ok),
    case PrepRes of
	false ->
	    _ = test_progress(continued, From, Ref, TestRes),
	    {{false, false}, Reqs};
	true ->
	    {DidKill, NewReqs} = check_proc_code(erlang:processes(),
						 Mod, true, Reqs),
	    _ = test_progress(continued, From, Ref, TestRes),
	    true = erts_internal:purge_module(Mod, complete),
	    {{true, DidKill}, NewReqs}
    end.

test_progress(_State, _From, _Ref, died) ->
    %% Test process died; continue so we wont
    %% leave the system in an inconsistent
    %% state...
    died;
test_progress(started, From, Ref, ok) ->
    From ! {started, Ref},
    Mon = erlang:monitor(process, From),
    receive
	{'DOWN', Mon, process, From, _} -> died;
	{continue, Ref} -> erlang:demonitor(Mon, [flush]), ok
    end;
test_progress(continued, From, Ref, ok) ->
    From ! {continued, Ref},
    Mon = erlang:monitor(process, From),
    receive
	{'DOWN', Mon, process, From, _} -> died;
	{complete, Ref} -> erlang:demonitor(Mon, [flush]), ok
    end.

