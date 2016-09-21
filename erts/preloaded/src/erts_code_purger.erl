%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2016. All Rights Reserved.
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

-spec start() -> term().
start() ->
    register(erts_code_purger, self()),
    process_flag(trap_exit, true),
    loop().

loop() ->
    _ = receive
	{purge,Mod,From,Ref} when is_atom(Mod), is_pid(From) ->
	    Res = do_purge(Mod),
	    From ! {reply, purge, Res, Ref};

	{soft_purge,Mod,From,Ref} when is_atom(Mod), is_pid(From) ->
	    Res = do_soft_purge(Mod),
	    From ! {reply, soft_purge, Res, Ref};

	{finish_after_on_load,{Mod,Keep},From,Ref}
	      when is_atom(Mod), is_pid(From) ->
	    Res = do_finish_after_on_load(Mod, Keep),
	    From ! {reply, finish_after_on_load, Res, Ref};

	{test_purge, Mod, From, Type, Ref} when is_atom(Mod), is_pid(From) ->
	     do_test_purge(Mod, From, Type, Ref);

	_Other -> ignore
    end,
    loop().

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

do_purge(Mod) ->
    case erts_internal:purge_module(Mod, prepare) of
	false ->
	    {false, false};
	true ->
	    DidKill = check_proc_code(erlang:processes(), Mod, true),
	    true = erts_internal:purge_module(Mod, complete),
	    {true, DidKill}
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

do_soft_purge(Mod) ->
    case erts_internal:purge_module(Mod, prepare) of
	false ->
	    true;
	true ->
	    Res = check_proc_code(erlang:processes(), Mod, false),
	    erts_internal:purge_module(Mod,
				       case Res of
					   false -> abort;
					   true -> complete
				       end)
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

do_finish_after_on_load(Mod, Keep) ->
    erlang:finish_after_on_load(Mod, Keep),
    case Keep of
	true ->
	    ok;
	false ->
	    case erts_internal:purge_module(Mod, prepare_on_load) of
		false ->
		    true;
		true ->
		    _ = check_proc_code(erlang:processes(), Mod, true),
		    true = erts_internal:purge_module(Mod, complete)
	    end
    end.



%%
%% check_proc_code(Pids, Mod, Hard) - Send asynchronous
%%   requests to all processes to perform a check_process_code
%%   operation. Each process will check their own state and
%%   reply with the result. If 'Hard' equals
%%   - true, processes that refer 'Mod' will be killed. If
%%     any processes were killed true is returned; otherwise,
%%     false.
%%   - false, and any processes refer 'Mod', false will
%%     returned; otherwise, true.
%%
%%   Requests will be sent to all processes identified by
%%   Pids at once, but without allowing GC to be performed.
%%   Check process code operations that are aborted due to
%%   GC need, will be restarted allowing GC. However, only
%%   ?MAX_CPC_GC_PROCS outstanding operation allowing GC at
%%   a time will be allowed. This in order not to blow up
%%   memory wise.
%%
%%   We also only allow ?MAX_CPC_NO_OUTSTANDING_KILLS
%%   outstanding kills. This both in order to avoid flooding
%%   our message queue with 'DOWN' messages and limiting the
%%   amount of memory used to keep references to all
%%   outstanding kills.
%%

%% We maybe should allow more than two outstanding
%% GC requests, but for now we play it safe...
-define(MAX_CPC_GC_PROCS, 2).
-define(MAX_CPC_NO_OUTSTANDING_KILLS, 10).

-record(cpc_static, {hard, module, tag}).

-record(cpc_kill, {outstanding = [],
		   no_outstanding = 0,
		   waiting = [],
		   killed = false}).

check_proc_code(Pids, Mod, Hard) ->
    Tag = erlang:make_ref(),
    CpcS = #cpc_static{hard = Hard,
		       module = Mod,
		       tag = Tag},
    check_proc_code(CpcS, cpc_init(CpcS, Pids, 0), 0, [], #cpc_kill{}, true).

check_proc_code(#cpc_static{hard = true}, 0, 0, [],
		#cpc_kill{outstanding = [], waiting = [], killed = Killed},
		true) ->
    %% No outstanding requests. We did a hard check, so result is whether or
    %% not we killed any processes...
    Killed;
check_proc_code(#cpc_static{hard = false}, 0, 0, [], _KillState, Success) ->
    %% No outstanding requests and we did a soft check...
    Success;
check_proc_code(#cpc_static{hard = false, tag = Tag} = CpcS, NoReq0, NoGcReq0,
		[], _KillState, false) ->
    %% Failed soft check; just cleanup the remaining replies corresponding
    %% to the requests we've sent...
    {NoReq1, NoGcReq1} = receive
			     {check_process_code, {Tag, _P, GC}, _Res} ->
				 case GC of
				     false -> {NoReq0-1, NoGcReq0};
				     true -> {NoReq0, NoGcReq0-1}
				 end
			 end,
    check_proc_code(CpcS, NoReq1, NoGcReq1, [], _KillState, false);
check_proc_code(#cpc_static{tag = Tag} = CpcS, NoReq0, NoGcReq0, NeedGC0,
		KillState0, Success) ->

    %% Check if we should request a GC operation
    {NoGcReq1, NeedGC1} = case NoGcReq0 < ?MAX_CPC_GC_PROCS of
			      GcOpAllowed when GcOpAllowed == false;
					       NeedGC0 == [] ->
				  {NoGcReq0, NeedGC0};
			      _ ->
				  {NoGcReq0+1, cpc_request_gc(CpcS,NeedGC0)}
			  end,

    %% Wait for a cpc reply or 'DOWN' message
    {NoReq1, NoGcReq2, Pid, Result, KillState1} = cpc_recv(Tag,
							   NoReq0,
							   NoGcReq1,
							   KillState0),

    %% Check the result of the reply
    case Result of
	aborted ->
	    %% Operation aborted due to the need to GC in order to
	    %% determine if the process is referring the module.
	    %% Schedule the operation for restart allowing GC...
	    check_proc_code(CpcS, NoReq1, NoGcReq2, [Pid|NeedGC1], KillState1,
			    Success);
	false ->
	    %% Process not referring the module; done with this process...
	    check_proc_code(CpcS, NoReq1, NoGcReq2, NeedGC1, KillState1,
			    Success);
	true ->
	    %% Process referring the module...
	    case CpcS#cpc_static.hard of
		false ->
		    %% ... and soft check. The whole operation failed so
		    %% no point continuing; clean up and fail...
		    check_proc_code(CpcS, NoReq1, NoGcReq2, [], KillState1,
				    false);
		true ->
		    %% ... and hard check; schedule kill of it...
		    check_proc_code(CpcS, NoReq1, NoGcReq2, NeedGC1,
				    cpc_sched_kill(Pid, KillState1), Success)
	    end;
	'DOWN' ->
	    %% Handled 'DOWN' message
	    check_proc_code(CpcS, NoReq1, NoGcReq2, NeedGC1,
			    KillState1, Success)
    end.

cpc_recv(Tag, NoReq, NoGcReq, #cpc_kill{outstanding = []} = KillState) ->
    receive
	{check_process_code, {Tag, Pid, GC}, Res} ->
	    cpc_handle_cpc(NoReq, NoGcReq, GC, Pid, Res, KillState)
    end;
cpc_recv(Tag, NoReq, NoGcReq,
	 #cpc_kill{outstanding = [R0, R1, R2, R3, R4 | _]} = KillState) ->
    receive
	{'DOWN', R, process, _, _} when R == R0;
					R == R1;
					R == R2;
					R == R3;
					R == R4 ->
	    cpc_handle_down(NoReq, NoGcReq, R, KillState);
	{check_process_code, {Tag, Pid, GC}, Res} ->
	    cpc_handle_cpc(NoReq, NoGcReq, GC, Pid, Res, KillState)
    end;
cpc_recv(Tag, NoReq, NoGcReq, #cpc_kill{outstanding = [R|_]} = KillState) ->
    receive
	{'DOWN', R, process, _, _} ->
	    cpc_handle_down(NoReq, NoGcReq, R, KillState);
	{check_process_code, {Tag, Pid, GC}, Res} ->
	    cpc_handle_cpc(NoReq, NoGcReq, GC, Pid, Res, KillState)
    end.

cpc_handle_down(NoReq, NoGcReq, R, #cpc_kill{outstanding = Rs,
					     no_outstanding = N} = KillState) ->
    {NoReq, NoGcReq, undefined, 'DOWN',
     cpc_sched_kill_waiting(KillState#cpc_kill{outstanding = cpc_list_rm(R, Rs),
					       no_outstanding = N-1})}.

cpc_list_rm(R, [R|Rs]) ->
    Rs;
cpc_list_rm(R0, [R1|Rs]) ->
    [R1|cpc_list_rm(R0, Rs)].

cpc_handle_cpc(NoReq, NoGcReq, false, Pid, Res, KillState) ->
    {NoReq-1, NoGcReq, Pid, Res, KillState};
cpc_handle_cpc(NoReq, NoGcReq, true, Pid, Res, KillState) ->
    {NoReq, NoGcReq-1, Pid, Res, KillState}.

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

cpc_request(#cpc_static{tag = Tag, module = Mod}, Pid, AllowGc) ->
    erts_internal:check_process_code(Pid, Mod, [{async, {Tag, Pid, AllowGc}},
						{allow_gc, AllowGc}]).

cpc_request_gc(CpcS, [Pid|Pids]) ->
    cpc_request(CpcS, Pid, true),
    Pids.

cpc_init(_CpcS, [], NoReqs) ->
    NoReqs;
cpc_init(CpcS, [Pid|Pids], NoReqs) ->
    cpc_request(CpcS, Pid, false),
    cpc_init(CpcS, Pids, NoReqs+1).

% end of check_proc_code() implementation.

%%
%% FOR TESTING ONLY
%%
%% do_test_purge() is for testing only. The purge is done
%% as usual, but the tester can control when to enter the
%% specific phases.
%%
do_test_purge(Mod, From, Type, Ref) when Type == true; Type == false ->
    Mon = erlang:monitor(process, From),
    Res = case Type of
	      true -> do_test_hard_purge(Mod, From, Ref, Mon);
	      false -> do_test_soft_purge(Mod, From, Ref, Mon)
	  end,
    From ! {test_purge, Res, Ref},
    erlang:demonitor(Mon, [flush]),
    ok;
do_test_purge(_, _, _, _) ->
    ok.

do_test_soft_purge(Mod, From, Ref, Mon) ->
    PrepRes = erts_internal:purge_module(Mod, prepare),
    TestRes = test_progress(started, From, Mon, Ref, ok),
    case PrepRes of
	false ->
	    _ = test_progress(continued, From, Mon, Ref, TestRes),
	    true;
	true ->
	    Res = check_proc_code(erlang:processes(), Mod, false),
	    _ = test_progress(continued, From, Mon, Ref, TestRes),
	    erts_internal:purge_module(Mod,
				       case Res of
					   false -> abort;
					   true -> complete
				       end)
    end.

do_test_hard_purge(Mod, From, Ref, Mon) ->
    PrepRes = erts_internal:purge_module(Mod, prepare),
    TestRes = test_progress(started, From, Mon, Ref, ok),
    case PrepRes of
	false ->
	    _ = test_progress(continued, From, Mon, Ref, TestRes),
	    {false, false};
	true ->
	    DidKill = check_proc_code(erlang:processes(), Mod, true),
	    _ = test_progress(continued, From, Mon, Ref, TestRes),
	    true = erts_internal:purge_module(Mod, complete),
	    {true, DidKill}
    end.

test_progress(_State, _From, _Mon, _Ref, died) ->
    %% Test process died; continue so we wont
    %% leave the system in an inconsistent
    %% state...
    died;
test_progress(started, From, Mon, Ref, ok) ->
    From ! {started, Ref},
    receive
	{'DOWN', Mon, process, From, _} -> died;
	{continue, Ref} -> ok
    end;
test_progress(continued, From, Mon, Ref, ok) ->
    From ! {continued, Ref},
    receive
	{'DOWN', Mon, process, From, _} -> died;
	{complete, Ref} -> ok
    end.

