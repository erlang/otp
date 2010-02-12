%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2010. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%
%% Description: Tests supervisor.erl

-module(supervisor_SUITE).

-include("test_server.hrl").

%% Testserver specific export
-export([all/1]).

%% Indirect spawn export
-export([init/1]).

%% API tests
-export([sup_start/1, sup_start_normal/1, sup_start_ignore_init/1, 
	 sup_start_ignore_child/1, sup_start_error_return/1, 
	 sup_start_fail/1, sup_stop/1, sup_stop_infinity/1, 
	 sup_stop_timeout/1, sup_stop_brutal_kill/1, child_adm/1,
	 child_adm_simple/1, child_specs/1, extra_return/1]).

%% Tests concept permanent, transient and temporary 
-export([normal_termination/1, permanent_normal/1, transient_normal/1,
	 temporary_normal/1, abnormal_termination/1,
	 permanent_abnormal/1, transient_abnormal/1,
	 temporary_abnormal/1]).

%% Restart strategy tests 
-export([restart_one_for_one/1, one_for_one/1,
	 one_for_one_escalation/1, restart_one_for_all/1, one_for_all/1,
	 one_for_all_escalation/1, restart_simple_one_for_one/1,
	 simple_one_for_one/1, simple_one_for_one_escalation/1,
	 restart_rest_for_one/1, rest_for_one/1, rest_for_one_escalation/1,
	 simple_one_for_one_extra/1]).

%% Misc tests
-export([child_unlink/1, tree/1, count_children_memory/1]).

%-------------------------------------------------------------------------

all(suite) -> 
    {req,[stdlib], 
     [sup_start, sup_stop, child_adm,
      child_adm_simple, extra_return, child_specs,
      restart_one_for_one, restart_one_for_all,
      restart_simple_one_for_one, restart_rest_for_one,
      normal_termination, abnormal_termination, child_unlink, tree,
      count_children_memory]}.


start(InitResult) ->
    supervisor:start_link({local, sup_test}, ?MODULE, InitResult).

%% Simulate different supervisors callback.  
init(fail) ->
    erlang:error({badmatch,2});
init(InitResult) ->
    InitResult.

%% Respect proplist return of supervisor:count_children
get_child_counts(Supervisor) ->
    Counts = supervisor:count_children(Supervisor),
    [proplists:get_value(specs, Counts),
     proplists:get_value(active, Counts),
     proplists:get_value(supervisors, Counts),
     proplists:get_value(workers, Counts)].


%-------------------------------------------------------------------------
%
% Test cases starts here.
%
%-------------------------------------------------------------------------

sup_start(doc) ->
    ["Test start of a supervisor."];
sup_start(suite) ->
    [sup_start_normal, sup_start_ignore_init, sup_start_ignore_child,
     sup_start_error_return, sup_start_fail].

%-------------------------------------------------------------------------
sup_start_normal(doc) ->
    ["Tests that the supervisor process starts correctly and that it "
    "can be terminated gracefully."];
sup_start_normal(suite) -> [];
sup_start_normal(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ?line {ok, Pid} = start({ok, {{one_for_one, 2, 3600}, []}}),
    ?line exit(Pid, shutdown),
    receive
	{'EXIT', Pid, shutdown} ->
	    ok;
	{'EXIT', Pid, Else} ->
	    ?line test_server:fail({bad_exit_reason, Else})
    after
	2000 ->
	    ?line test_server:fail(no_exit_reason)
    end,
    ok.
%-------------------------------------------------------------------------
sup_start_ignore_init(doc) ->
    ["Tests what happens if init-callback returns ignore"];
sup_start_ignore_init(suite) -> [];
sup_start_ignore_init(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ?line ignore = start(ignore),

    receive
	{'EXIT', _Pid, normal} ->
	    ok;
	{'EXIT', _Pid, Else} ->
	    ?line test_server:fail({bad_exit_reason, Else})
    after
	2000 ->
	    ?line test_server:fail(no_exit_reason)
    end,
    ok.


%-------------------------------------------------------------------------
sup_start_ignore_child(doc) ->
    ["Tests what happens if init-callback returns ignore"];
sup_start_ignore_child(suite) -> [];
sup_start_ignore_child(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ?line {ok, _Pid}  = start({ok, {{one_for_one, 2, 3600}, []}}),
    Child1 = {child1, {supervisor_1, start_child, [ignore]}, 
	      permanent, 1000, worker, []},
    Child2 = {child2, {supervisor_1, start_child, []}, permanent,
	      1000, worker, []},
    
    ?line {ok, undefined} = supervisor:start_child(sup_test, Child1),
    ?line {ok, CPid2} = supervisor:start_child(sup_test, Child2),

    ?line [{child2, CPid2, worker, []},{child1, undefined, worker, []}] 
	= supervisor:which_children(sup_test),
    ?line [2,1,0,2] = get_child_counts(sup_test),

    ok.

%-------------------------------------------------------------------------
sup_start_error_return(doc) ->
    ["Tests what happens if init-callback returns a invalid value"];
sup_start_error_return(suite) -> [];
sup_start_error_return(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ?line {error, Term} = start(invalid),

    receive
	{'EXIT', _Pid, Term} ->
	    ok;
	{'EXIT', _Pid, Else} ->
	    ?line test_server:fail({bad_exit_reason, Else})
    after
	2000 ->
	    ?line test_server:fail(no_exit_reason)
    end,
    ok.

%-------------------------------------------------------------------------
sup_start_fail(doc) ->
    ["Tests what happens if init-callback fails"];
sup_start_fail(suite) -> [];
sup_start_fail(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ?line {error, Term} = start(fail),

    receive
	{'EXIT', _Pid, Term} ->
	    ok;
	{'EXIT', _Pid, Else} ->
	    ?line test_server:fail({bad_exit_reason, Else})
    after
	2000 ->
	    ?line test_server:fail(no_exit_reason)
    end,
    ok.
%-------------------------------------------------------------------------
sup_stop(doc) ->
    ["Tests that the supervisor shoutdowns its children if it is " 
     "shutdown itself."];
sup_stop(suite) -> [sup_stop_infinity, sup_stop_timeout, sup_stop_brutal_kill].

%-------------------------------------------------------------------------

sup_stop_infinity(doc) ->
    ["See sup_stop/1 when Shutdown = infinity, this walue is only allowed "
    "for children of type supervisor"];
sup_stop_infinity(suite) -> [];

sup_stop_infinity(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ?line {ok, Pid} = start({ok, {{one_for_one, 2, 3600}, []}}),
    Child1 = {child1, {supervisor_1, start_child, []}, 
	      permanent, infinity, supervisor, []},
    Child2 = {child2, {supervisor_1, start_child, []}, permanent,
	       infinity, worker, []},
    ?line {ok, CPid1} = supervisor:start_child(sup_test, Child1),
    link(CPid1),
    ?line {error, {invalid_shutdown,infinity}} = 
	supervisor:start_child(sup_test, Child2),    

    ?line exit(Pid, shutdown),

    receive
	{'EXIT', Pid, shutdown} ->
	    ok;
	{'EXIT', Pid, Else} ->
	    ?line test_server:fail({bad_exit_reason, Else})
    after
	5000 ->
	    ?line test_server:fail(no_exit_reason)
    end,
    receive
	{'EXIT', CPid1, shutdown} -> ok;
	{'EXIT', CPid1, Reason} ->
	    ?line test_server:fail({bad_exit_reason, Reason})
    after
	2000 -> ?line test_server:fail(no_exit_reason)
    end,
    ok.

%-------------------------------------------------------------------------

sup_stop_timeout(doc) ->
    ["See sup_stop/1 when Shutdown = 1000"];
sup_stop_timeout(suite) -> [];

sup_stop_timeout(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ?line {ok, Pid} = start({ok, {{one_for_one, 2, 3600}, []}}),
    Child1 = {child1, {supervisor_1, start_child, []}, 
	      permanent, 1000, worker, []},
    Child2 = {child2, {supervisor_1, start_child, []}, permanent,
	      1000, worker, []},
    ?line {ok, CPid1} = supervisor:start_child(sup_test, Child1),
    link(CPid1),
    ?line {ok, CPid2} = supervisor:start_child(sup_test, Child2),    
    link(CPid2),
   
    CPid2 ! {sleep, 200000},

    ?line exit(Pid, shutdown),

    receive
	{'EXIT', Pid, shutdown} ->
	    ok;
	{'EXIT', Pid, Else} ->
	    ?line test_server:fail({bad_exit_reason, Else})
    after
	5000 ->
	    ?line test_server:fail(no_exit_reason)
    end,

    receive
	{'EXIT', CPid1, shutdown} -> ok;
	{'EXIT', CPid1, Reason} ->
	    ?line test_server:fail({bad_exit_reason,Reason})
    after
	2000 -> ?line test_server:fail(no_exit_reason)
    end,
   
    receive
	{'EXIT', CPid2, killed} -> ok;
	{'EXIT', CPid2, Reason2} ->
	    ?line test_server:fail({bad_exit_reason, Reason2})
    after
	2000 -> ?line test_server:fail(no_exit_reason)
    end,
    ok.

%-------------------------------------------------------------------------
sup_stop_brutal_kill(doc) ->
    ["See sup_stop/1 when Shutdown = brutal_kill"];
sup_stop_brutal_kill(suite) -> [];

sup_stop_brutal_kill(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ?line {ok, Pid} = start({ok, {{one_for_one, 2, 3600}, []}}),
    Child1 = {child1, {supervisor_1, start_child, []}, 
	      permanent, 1000, worker, []},
    Child2 = {child2, {supervisor_1, start_child, []}, permanent,
	      brutal_kill, worker, []},
    ?line {ok, CPid1} = supervisor:start_child(sup_test, Child1),
    link(CPid1),
    ?line {ok, CPid2} = supervisor:start_child(sup_test, Child2),    
    link(CPid2),

    ?line exit(Pid, shutdown),

    receive
	{'EXIT', Pid, shutdown} ->
	    ok;
	{'EXIT', Pid, Else} ->
	    ?line test_server:fail({bad_exit_reason, Else})
    after
	5000 ->
	    ?line test_server:fail(no_exit_reason)
    end,

    receive
	{'EXIT', CPid1, shutdown} -> ok;
	{'EXIT', CPid1, Reason} ->
	    ?line test_server:fail({bad_exit_reason, Reason})
    after
	2000 -> ?line test_server:fail(no_exit_reason)
    end,
    receive
	{'EXIT', CPid2, killed} -> ok;
	{'EXIT', CPid2, Reason2} ->
	    ?line test_server:fail({bad_exit_reason, Reason2})
    after
	2000 -> ?line test_server:fail(no_exit_reason)
    end,
    ok.

%-------------------------------------------------------------------------
extra_return(doc) -> 
    ["The start function provided to start a child may " 
     "return {ok, Pid} or {ok, Pid, Info}, if it returns "
     "the later check that the supervisor ignores the Info, "
     "and includes it unchanged in return from start_child/2 "
     "and restart_child/2"];
extra_return(suite) -> [];

extra_return(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    Child = {child1, {supervisor_1, start_child, [extra_return]}, 
	     permanent, 1000,
	     worker, []},
    ?line {ok, _Pid} = start({ok, {{one_for_one, 2, 3600}, [Child]}}),
    ?line [{child1, CPid, worker, []}] = supervisor:which_children(sup_test),
    link(CPid),
    ?line {error, not_found} = supervisor:terminate_child(sup_test, hej),
    ?line {error, not_found} = supervisor:delete_child(sup_test, hej),
    ?line {error, not_found} = supervisor:restart_child(sup_test, hej),
    ?line {error, running} = supervisor:delete_child(sup_test, child1),
    ?line {error, running} = supervisor:restart_child(sup_test, child1),
    ?line [{child1, CPid, worker, []}] = supervisor:which_children(sup_test),
    ?line [1,1,0,1] = get_child_counts(sup_test),

    ?line ok = supervisor:terminate_child(sup_test, child1),
    receive
	{'EXIT', CPid, shutdown} -> ok;
	{'EXIT', CPid, Reason} ->
	    ?line test_server:fail({bad_reason, Reason})
    after 1000 ->
	    ?line test_server:fail(no_child_termination)
    end,
    ?line [{child1,undefined,worker,[]}] = supervisor:which_children(sup_test),
    ?line [1,0,0,1] = get_child_counts(sup_test),

    ?line {ok, CPid2,extra_return} = 
	supervisor:restart_child(sup_test, child1),
    ?line [{child1, CPid2, worker, []}] = supervisor:which_children(sup_test),
    ?line [1,1,0,1] = get_child_counts(sup_test),

    ?line ok = supervisor:terminate_child(sup_test, child1),
    ?line ok = supervisor:terminate_child(sup_test, child1),
    ?line ok = supervisor:delete_child(sup_test, child1),
    ?line {error, not_found} = supervisor:restart_child(sup_test, child1),
    ?line [] = supervisor:which_children(sup_test),
    ?line [0,0,0,0] = get_child_counts(sup_test),

    ?line {ok, CPid3, extra_return} = supervisor:start_child(sup_test, Child),
    ?line [{child1, CPid3, worker, []}] = supervisor:which_children(sup_test),
    ?line [1,1,0,1] = get_child_counts(sup_test),

    ok.
%-------------------------------------------------------------------------
child_adm(doc)->
    ["Test API functions start_child/2, terminate_child/2, delete_child/2 "
     "restart_child/2, which_children/1, count_children/1. Only correct "
     "childspecs are used, handling of incorrect childspecs is tested in "
     "child_specs/1"];
child_adm(suite) -> [];
child_adm(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    Child = {child1, {supervisor_1, start_child, []}, permanent, 1000,
	     worker, []},
    ?line {ok, _Pid} = start({ok, {{one_for_one, 2, 3600}, [Child]}}),
    ?line [{child1, CPid, worker, []}] = supervisor:which_children(sup_test),
    ?line [1,1,0,1] = get_child_counts(sup_test),
    link(CPid),

    %% Start of an already runnig process 
    ?line {error,{already_started, CPid}} =
	supervisor:start_child(sup_test, Child),
    
    %% Termination
    ?line {error, not_found} = supervisor:terminate_child(sup_test, hej),
    ?line  {'EXIT',{noproc,{gen_server,call, _}}} = 
	(catch supervisor:terminate_child(foo, child1)),
    ?line ok = supervisor:terminate_child(sup_test, child1),
    receive
	{'EXIT', CPid, shutdown} -> ok;
	{'EXIT', CPid, Reason} ->
	    ?line test_server:fail({bad_reason, Reason})
    after 1000 ->
	    ?line test_server:fail(no_child_termination)
    end,
    ?line [{child1,undefined,worker,[]}] = supervisor:which_children(sup_test),
    ?line [1,0,0,1] = get_child_counts(sup_test),
    %% Like deleting something that does not exist, it will succeed!
    ?line ok = supervisor:terminate_child(sup_test, child1),

    %% Start of already existing but not running process 
    ?line {error,already_present} =
	supervisor:start_child(sup_test, Child),

    %% Restart
    ?line {ok, CPid2} = supervisor:restart_child(sup_test, child1),
    ?line [{child1, CPid2, worker, []}] = supervisor:which_children(sup_test),
    ?line [1,1,0,1] = get_child_counts(sup_test),
    ?line {error, running} = supervisor:restart_child(sup_test, child1),
    ?line {error, not_found} = supervisor:restart_child(sup_test, child2),
    
    %% Deletion
    ?line {error, running} = supervisor:delete_child(sup_test, child1),
    ?line {error, not_found} = supervisor:delete_child(sup_test, hej),
    ?line  {'EXIT',{noproc,{gen_server,call, _}}} = 
	(catch supervisor:delete_child(foo, child1)),
    ?line ok = supervisor:terminate_child(sup_test, child1),
    ?line ok = supervisor:delete_child(sup_test, child1),
    ?line {error, not_found} = supervisor:restart_child(sup_test, child1),
    ?line [] = supervisor:which_children(sup_test),
    ?line [0,0,0,0] = get_child_counts(sup_test),
    
    %% Start
    ?line {'EXIT',{noproc,{gen_server,call, _}}} =
	(catch supervisor:start_child(foo, Child)),
    ?line {ok, CPid3} = supervisor:start_child(sup_test, Child),
    ?line [{child1, CPid3, worker, []}] = supervisor:which_children(sup_test),
    ?line [1,1,0,1] = get_child_counts(sup_test),

    ?line {'EXIT',{noproc,{gen_server,call,[foo,which_children,infinity]}}}
	= (catch supervisor:which_children(foo)),
    ?line {'EXIT',{noproc,{gen_server,call,[foo,count_children,infinity]}}}
	= (catch supervisor:count_children(foo)),
    ok.
%-------------------------------------------------------------------------
child_adm_simple(doc) ->
    ["The API functions terminate_child/2, delete_child/2 "
     "restart_child/2 are not valid for a simple_one_for_one supervisor "
    "check that the correct error message is returned."];
child_adm_simple(suite) -> [];
child_adm_simple(Config) when is_list(Config) ->
    Child = {child, {supervisor_1, start_child, []}, permanent, 1000,
	     worker, []},
    ?line {ok, _Pid} = start({ok, {{simple_one_for_one, 2, 3600}, [Child]}}),
    %% In simple_one_for_one all children are added dynamically 
    ?line [] = supervisor:which_children(sup_test), 
    ?line [1,0,0,0] = get_child_counts(sup_test),
    
    %% Start
    ?line {'EXIT',{noproc,{gen_server,call, _}}} =
	(catch supervisor:start_child(foo, [])),
    ?line {ok, CPid1} = supervisor:start_child(sup_test, []),
    ?line [{undefined, CPid1, worker, []}] = 
	supervisor:which_children(sup_test),
    ?line [1,1,0,1] = get_child_counts(sup_test),
    
    ?line {ok, CPid2} = supervisor:start_child(sup_test, []),
    ?line Children = supervisor:which_children(sup_test),
    ?line 2 = length(Children),
    ?line true = lists:member({undefined, CPid2, worker, []}, Children),
    ?line true = lists:member({undefined, CPid1, worker, []}, Children),
    ?line [1,2,0,2] = get_child_counts(sup_test),

    %% Termination
    ?line {error, simple_one_for_one} =
	supervisor:terminate_child(sup_test, child1),

    %% Restart
    ?line {error, simple_one_for_one} = 
	supervisor:restart_child(sup_test, child1),
    
    %% Deletion
    ?line {error, simple_one_for_one} = 
	supervisor:delete_child(sup_test, child1),
    ok.
    
%-------------------------------------------------------------------------
child_specs(doc) ->
    ["Tests child specs, invalid formats should be rejected."];
child_specs(suite) -> [];
child_specs(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ?line {ok, _Pid} = start({ok, {{one_for_one, 2, 3600}, []}}),
    ?line {error, _} = supervisor:start_child(sup_test, hej),

    %% Bad child specs 
    B1 = {child, mfa, permanent, 1000, worker, []},
    B2 = {child, {m,f,[a]}, prmanent, 1000, worker, []}, 
    B3 = {child, {m,f,[a]}, permanent, -10, worker, []},
    B4 = {child, {m,f,[a]}, permanent, 10, wrker, []},
    B5 = {child, {m,f,[a]}, permanent, infinity, worker, []},
    B6 = {child, {m,f,[a]}, permanent, 1000, worker, dy},
    B7 = {child, {m,f,[a]}, permanent, 1000, worker, [1,2,3]},
				       
    %% Correct child specs!
    %% <Modules> (last parameter in a child spec) can be [] as we do 
    %% not test code upgrade here.  
    C1 = {child, {m,f,[a]}, permanent, infinity, supervisor, []},
    C2 = {child, {m,f,[a]}, permanent, 1000, supervisor, []},
    C3 = {child, {m,f,[a]}, temporary, 1000, worker, dynamic},
    C4 = {child, {m,f,[a]}, transient, 1000, worker, [m]},

    ?line {error, {invalid_mfa,mfa}} = supervisor:start_child(sup_test, B1),
    ?line {error, {invalid_restart_type, prmanent}} =  
	supervisor:start_child(sup_test, B2),
    ?line {error,  {invalid_shutdown,-10}} 
	= supervisor:start_child(sup_test, B3), 
    ?line {error, {invalid_child_type,wrker}} 
	= supervisor:start_child(sup_test, B4),
    ?line {error, _} = supervisor:start_child(sup_test, B5),
    ?line {error, {invalid_modules,dy}} 
	= supervisor:start_child(sup_test, B6),
    
    ?line {error, {invalid_mfa,mfa}} = supervisor:check_childspecs([B1]),
    ?line {error, {invalid_restart_type,prmanent}} = 
	supervisor:check_childspecs([B2]),
    ?line {error, {invalid_shutdown,-10}} = supervisor:check_childspecs([B3]),
    ?line {error, {invalid_child_type,wrker}} 
	= supervisor:check_childspecs([B4]),
    ?line {error, _} = supervisor:check_childspecs([B5]),
    ?line {error, {invalid_modules,dy}} = supervisor:check_childspecs([B6]),
    ?line {error, {invalid_module, 1}} = 
	supervisor:check_childspecs([B7]),

    ?line ok = supervisor:check_childspecs([C1]),
    ?line ok = supervisor:check_childspecs([C2]),
    ?line ok = supervisor:check_childspecs([C3]),
    ?line ok = supervisor:check_childspecs([C4]),
    ok.
%-------------------------------------------------------------------------
normal_termination(doc) ->
    ["Testes the supervisors behaviour if a child dies with reason normal"];
normal_termination(suite) -> 
    [permanent_normal, transient_normal, temporary_normal].

%-------------------------------------------------------------------------
permanent_normal(doc) ->
    ["A permanent child should always be restarted"];
permanent_normal(suite) -> [];
permanent_normal(Config) when is_list(Config) ->
    ?line  {ok, _SupPid} = start({ok, {{one_for_one, 2, 3600}, []}}),
    Child1 = {child1, {supervisor_1, start_child, []}, permanent, 1000,
	      worker, []},
    
    ?line {ok, CPid1} = supervisor:start_child(sup_test, Child1),
    
    CPid1 ! stop,
    test_server:sleep(100),
    ?line [{child1, Pid ,worker,[]}] = supervisor:which_children(sup_test),
    case is_pid(Pid) of
	true ->
	    ok;
	false ->
	    ?line test_server:fail({permanent_child_not_restarted, Child1})
    end,
    ?line [1,1,0,1] = get_child_counts(sup_test),

    ok.
%------------------------------------------------------------------------- 
transient_normal(doc) ->
    ["A transient child should not be restarted if it exits with " 
     "reason normal"];
transient_normal(suite) -> [];
transient_normal(Config) when is_list(Config) ->
    ?line  {ok, _SupPid} = start({ok, {{one_for_one, 2, 3600}, []}}),
    Child1 = {child1, {supervisor_1, start_child, []}, transient, 1000,
	      worker, []},
    
    ?line {ok, CPid1} = supervisor:start_child(sup_test, Child1),
   
    CPid1 ! stop,
    test_server:sleep(100),
    
    ?line [{child1,undefined,worker,[]}] = supervisor:which_children(sup_test),
    ?line [1,0,0,1] = get_child_counts(sup_test),

    ok.
%-------------------------------------------------------------------------    
temporary_normal(doc) ->
    ["A temporary process should never be restarted"];
temporary_normal(suite) -> [];
temporary_normal(Config) when is_list(Config) ->
     ?line  {ok, _SupPid} = start({ok, {{one_for_one, 2, 3600}, []}}),
    Child1 = {child1, {supervisor_1, start_child, []}, temporary, 1000,
	      worker, []},
    
    ?line {ok, CPid1} = supervisor:start_child(sup_test, Child1),
    
    CPid1 ! stop,
    test_server:sleep(100),
    
    ?line [{child1,undefined,worker,[]}] = supervisor:which_children(sup_test),
    ?line [1,0,0,1] = get_child_counts(sup_test),

    ok.
%-------------------------------------------------------------------------
abnormal_termination(doc) ->
    ["Testes the supervisors behaviour if a child dies with reason abnormal"];
abnormal_termination(suite) -> 
    [permanent_abnormal, transient_abnormal, temporary_abnormal].

%-------------------------------------------------------------------------
permanent_abnormal(doc) ->
    ["A permanent child should always be restarted"];
permanent_abnormal(suite) -> [];
permanent_abnormal(Config) when is_list(Config) ->
    ?line  {ok, _SupPid} = start({ok, {{one_for_one, 2, 3600}, []}}),
    Child1 = {child1, {supervisor_1, start_child, []}, permanent, 1000,
	      worker, []},
    
    ?line {ok, CPid1} = supervisor:start_child(sup_test, Child1),
    
    CPid1 ! die,
    test_server:sleep(100),
    ?line [{child1, Pid ,worker,[]}] = supervisor:which_children(sup_test),
    case is_pid(Pid) of
	true ->
	    ok;
	false ->
	    ?line test_server:fail({permanent_child_not_restarted, Child1})
    end,
    ?line [1,1,0,1] = get_child_counts(sup_test),

    ok.
%------------------------------------------------------------------------- 
transient_abnormal(doc) ->
    ["A transient child should be restarted if it exits with " 
     "reason abnormal"];
transient_abnormal(suite) -> [];
transient_abnormal(Config) when is_list(Config) ->
    ?line  {ok, _SupPid} = start({ok, {{one_for_one, 2, 3600}, []}}),
    Child1 = {child1, {supervisor_1, start_child, []}, transient, 1000,
	      worker, []},
    
    ?line {ok, CPid1} = supervisor:start_child(sup_test, Child1),
   
    CPid1 ! die,
    test_server:sleep(100),
    
    ?line [{child1, Pid ,worker,[]}] = supervisor:which_children(sup_test),
    case is_pid(Pid) of
	true ->
	    ok;
	false ->
	    ?line test_server:fail({transient_child_not_restarted, Child1})
    end,
    ?line [1,1,0,1] = get_child_counts(sup_test),

    ok.
%-------------------------------------------------------------------------    
temporary_abnormal(doc) ->
    ["A temporary process should never be restarted"];
temporary_abnormal(suite) -> [];
temporary_abnormal(Config) when is_list(Config) ->
     ?line  {ok, _SupPid} = start({ok, {{one_for_one, 2, 3600}, []}}),
    Child1 = {child1, {supervisor_1, start_child, []}, temporary, 1000,
	      worker, []},
    
    ?line {ok, CPid1} = supervisor:start_child(sup_test, Child1),
    
    CPid1 ! die,
    test_server:sleep(100),
    
    ?line [{child1,undefined,worker,[]}] = supervisor:which_children(sup_test),
    ?line [1,0,0,1] = get_child_counts(sup_test),

    ok.
%-------------------------------------------------------------------------
restart_one_for_one(doc) ->
    ["Test that the one_for_one strategy works."];

restart_one_for_one(suite) -> [one_for_one, one_for_one_escalation].

%-------------------------------------------------------------------------
one_for_one(doc) ->
    ["Test the one_for_one base case."];
one_for_one(suite) -> [];
one_for_one(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    Child1 = {child1, {supervisor_1, start_child, []}, permanent, 1000,
	     worker, []},
    Child2 = {child2, {supervisor_1, start_child, []}, permanent, 1000,
	     worker, []},
    ?line {ok, Pid} = start({ok, {{one_for_one, 2, 3600}, []}}),
    ?line {ok, CPid1} = supervisor:start_child(sup_test, Child1),
    link(CPid1),
    ?line {ok, CPid2} = supervisor:start_child(sup_test, Child2),    
    link(CPid2),
    CPid1 ! die,
    receive
	{'EXIT', CPid1, died} -> ok;
	{'EXIT', CPid1, Reason} ->
	    ?line test_server:fail({bad_exit_reason, Reason})
    end,
    test_server:sleep(100),
    Children = supervisor:which_children(sup_test),
    if length(Children) == 2 ->
	    case lists:keysearch(CPid2, 2, Children) of
		{value, _} -> ok;
		_ -> ?line test_server:fail(bad_child)
	    end;
       true -> ?line test_server:fail({bad_child_list, Children})
    end,
    ?line [2,2,0,2] = get_child_counts(sup_test),
    
    %% Test restart frequency property
    CPid2 ! die,
    receive
	{'EXIT', CPid2, _} -> ok
    end,
    test_server:sleep(100),
    [{_, Pid4, _, _}|_] = supervisor:which_children(sup_test),
    Pid4 ! die,
    receive
	{'EXIT', Pid, _} -> ok
    after 3000 -> ?line test_server:fail(restart_failed)
    end,
    ok.
%-------------------------------------------------------------------------
one_for_one_escalation(doc) ->
    ["Test restart escalation on a one_for_one supervisor."];
one_for_one_escalation(suite) -> [];
one_for_one_escalation(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    Child1 = {child1, {supervisor_1, start_child, [error]},
	      permanent, 1000,
	     worker, []},
    Child2 = {child2, {supervisor_1, start_child, []}, permanent, 1000,
	     worker, []},
    ?line {ok, Pid} = start({ok, {{one_for_one, 4, 3600}, []}}),
    ?line {ok, CPid1} = supervisor:start_child(sup_test, Child1),
    link(CPid1),
    ?line {ok, CPid2} = supervisor:start_child(sup_test, Child2),    
    link(CPid2),
    CPid1 ! die,
    receive
	{'EXIT', CPid1, died} -> ok;
	{'EXIT', CPid1, Reason} ->
	    ?line test_server:fail({bad_exit_reason, Reason})
    end,
    receive
	{'EXIT', Pid, _} -> ok
    after
	2000 -> ?line test_server:fail(supervisor_alive)
    end,
    receive
	{'EXIT', CPid2, _} -> ok
    after
	4000 -> ?line test_server:fail(all_not_terminated)
    end,
    ok.
%-------------------------------------------------------------------------
restart_one_for_all(doc) ->
    ["Test that the one_for_all strategy works."];

restart_one_for_all(suite) -> 
    [one_for_all, one_for_all_escalation].

%-------------------------------------------------------------------------
one_for_all(doc) ->
    ["Test the one_for_all base case."];
one_for_all(suite) -> [];
one_for_all(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    Child1 = {child1, {supervisor_1, start_child, []}, permanent, 1000,
	     worker, []},
    Child2 = {child2, {supervisor_1, start_child, []}, permanent, 1000,
	     worker, []},
    ?line {ok, Pid} = start({ok, {{one_for_all, 2, 3600}, []}}),
    ?line {ok, CPid1} = supervisor:start_child(sup_test, Child1),
    link(CPid1),
    ?line {ok, CPid2} = supervisor:start_child(sup_test, Child2),    
    link(CPid2),
    CPid1 ! die,
    receive
	{'EXIT', CPid1, died} -> ok;
	{'EXIT', CPid1, Reason} ->
	    ?line test_server:fail({bad_exit_reason, Reason})
    end,
    receive
	{'EXIT', CPid2, _} -> ok
    end,
    test_server:sleep(100),
    Children = supervisor:which_children(sup_test),
    if length(Children) == 2 -> ok;
       true -> ?line test_server:fail({bad_child_list, Children})
    end,
    %% Test that no old children is still alive
    SCh = lists:map(fun({_,P,_,_}) -> P end, Children),
    case lists:member(CPid1, SCh) of
	true -> ?line test_server:fail(bad_child);
	false -> ok
    end,
    case lists:member(CPid2, SCh) of
	true -> ?line test_server:fail(bad_child);
	false -> ok
    end,
    ?line [2,2,0,2] = get_child_counts(sup_test),

    %%% Test restart frequency property
    [{_, Pid3, _, _}|_] = supervisor:which_children(sup_test),
    Pid3 ! die,
    test_server:sleep(100),
    [{_, Pid4, _, _}|_] = supervisor:which_children(sup_test),
    Pid4 ! die,
    receive
	{'EXIT', Pid, _} -> ok
    after 3000 -> ?line test_server:fail(restart_failed)
    end,
    exit(Pid, shutdown).

%-------------------------------------------------------------------------
one_for_all_escalation(doc) -> 
    ["Test restart escalation on a one_for_all supervisor."];
one_for_all_escalation(suite) -> [];
one_for_all_escalation(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    Child1 = {child1, {supervisor_1, start_child, []}, permanent, 1000,
	     worker, []},
    Child2 = {child2, {supervisor_1, start_child, [error]},
	      permanent, 1000,
	     worker, []},
    ?line {ok, Pid} = start({ok, {{one_for_all, 4, 3600}, []}}),
    ?line {ok, CPid1} = supervisor:start_child(sup_test, Child1),
    link(CPid1),
    ?line {ok, CPid2} = supervisor:start_child(sup_test, Child2),    
    link(CPid2),
    CPid1 ! die,
    receive
	{'EXIT', CPid1, died} -> ok;
	{'EXIT', CPid1, Reason} ->
	    ?line test_server:fail({bad_exit_reason, Reason})
    end,
    receive
	{'EXIT', CPid2, _} -> ok
    after
	2000 -> ?line test_server:fail(all_not_terminated)
    end,
    receive
	{'EXIT', Pid, _} -> ok
    after
	4000 -> ?line test_server:fail(supervisor_alive)
    end,
    ok.

%-------------------------------------------------------------------------
restart_simple_one_for_one(doc) ->
    ["Test that the simple_one_for_one strategy works."];

restart_simple_one_for_one(suite) -> 
    [simple_one_for_one, simple_one_for_one_extra,
     simple_one_for_one_escalation].

%-------------------------------------------------------------------------
simple_one_for_one(doc) ->
    ["Test the simple_one_for_one base case."];
simple_one_for_one(suite) -> [];
simple_one_for_one(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    Child = {child, {supervisor_1, start_child, []}, permanent, 1000,
	     worker, []},
    ?line {ok, Pid} = start({ok, {{simple_one_for_one, 2, 3600}, [Child]}}),
    ?line {ok, CPid1} = supervisor:start_child(sup_test, []),
    link(CPid1),
    ?line {ok, CPid2} = supervisor:start_child(sup_test, []),    
    link(CPid2),
    CPid1 ! die,
    receive
	{'EXIT', CPid1, died} -> ok;
	{'EXIT', CPid1, Reason} ->
	    ?line test_server:fail({bad_exit_reason, Reason})
    end,
    test_server:sleep(100),
    Children = supervisor:which_children(sup_test),
    if length(Children) == 2 ->
	    case lists:keysearch(CPid2, 2, Children) of
		{value, _} -> ok;
		_ -> ?line test_server:fail(bad_child)
	    end;
       true -> ?line test_server:fail({bad_child_list, Children})
    end,
    ?line [1,2,0,2] = get_child_counts(sup_test),

    %% Test restart frequency property
    CPid2 ! die,
    receive
	{'EXIT', CPid2, _} -> ok
    end,
    test_server:sleep(100),
    [{_, Pid4, _, _}|_] = supervisor:which_children(sup_test),
    Pid4 ! die,
    receive
	{'EXIT', Pid, _} -> ok
    after 3000 -> ?line test_server:fail(restart_failed)
    end,
    ok.
%-------------------------------------------------------------------------
simple_one_for_one_extra(doc) -> 
    ["Tests automatic restart of children " 
     "who's start function return extra info."];
simple_one_for_one_extra(suite) -> [];
simple_one_for_one_extra(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    Child = {child, {supervisor_1, start_child, [extra_info]}, 
	     permanent, 1000, worker, []},
    ?line {ok, Pid} = start({ok, {{simple_one_for_one, 2, 3600}, [Child]}}),
    ?line {ok, CPid1, extra_info} = supervisor:start_child(sup_test, []),
    link(CPid1),
    ?line {ok, CPid2, extra_info} = supervisor:start_child(sup_test, []),    
    link(CPid2),
    CPid1 ! die,
    receive
	{'EXIT', CPid1, died} -> ok;
	{'EXIT', CPid1, Reason} ->
	    ?line test_server:fail({bad_exit_reason, Reason})
    end,
    test_server:sleep(100),
    Children = supervisor:which_children(sup_test),
    if length(Children) == 2 ->
	    case lists:keysearch(CPid2, 2, Children) of
		{value, _} -> ok;
		_ -> ?line test_server:fail(bad_child)
	    end;
       true -> ?line test_server:fail({bad_child_list, Children})
    end,
    ?line [1,2,0,2] = get_child_counts(sup_test),

    CPid2 ! die,
    receive
	{'EXIT', CPid2, _} -> ok
    end,
    test_server:sleep(100),
    [{_, Pid4, _, _}|_] = supervisor:which_children(sup_test),
    Pid4 ! die,
    receive
	{'EXIT', Pid, _} -> ok
    after 3000 -> ?line test_server:fail(restart_failed)
    end,
    ok.
%-------------------------------------------------------------------------
simple_one_for_one_escalation(doc) ->
    ["Test restart escalation on a simple_one_for_one supervisor."];
simple_one_for_one_escalation(suite) -> [];
simple_one_for_one_escalation(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    Child = {child, {supervisor_1, start_child, []}, permanent, 1000,
	     worker, []},
    ?line {ok, Pid} = start({ok, {{simple_one_for_one, 4, 3600}, [Child]}}),
    ?line {ok, CPid1} = supervisor:start_child(sup_test, [error]),
    link(CPid1),
    ?line {ok, CPid2} = supervisor:start_child(sup_test, []),    
    link(CPid2),
    CPid1 ! die,
    receive
	{'EXIT', CPid1, died} -> ok;
	{'EXIT', CPid1, Reason} ->
	    ?line test_server:fail({bad_exit_reason, Reason})
    end,
    receive
	{'EXIT', Pid, _} -> ok
    after
	2000 -> ?line test_server:fail(supervisor_alive)
    end,
    receive
	{'EXIT', CPid2, _} -> ok
    after
	2000 -> ?line test_server:fail(all_not_terminated)
    end,
    ok.
%-------------------------------------------------------------------------
restart_rest_for_one(doc) ->
    ["Test that the rest_for_one strategy works."];
restart_rest_for_one(suite) -> [rest_for_one, rest_for_one_escalation].

%-------------------------------------------------------------------------
rest_for_one(doc) ->
    ["Test the rest_for_one base case."];
rest_for_one(suite) -> [];
rest_for_one(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    Child1 = {child1, {supervisor_1, start_child, []}, permanent, 1000,
	     worker, []},
    Child2 = {child2, {supervisor_1, start_child, []}, permanent, 1000,
	     worker, []},
    Child3 = {child3, {supervisor_1, start_child, []}, permanent, 1000,
	     worker, []},
    ?line {ok, Pid} = start({ok, {{rest_for_one, 2, 3600}, []}}),
    ?line {ok, CPid1} = supervisor:start_child(sup_test, Child1),
    link(CPid1),
    ?line {ok, CPid2} = supervisor:start_child(sup_test, Child2),    
    link(CPid2),
    ?line {ok, CPid3} = supervisor:start_child(sup_test, Child3),    
    link(CPid3),
    ?line [3,3,0,3] = get_child_counts(sup_test),

    CPid2 ! die,
    receive
	{'EXIT', CPid2, died} -> ok;
	{'EXIT', CPid2, Reason} ->
	    ?line test_server:fail({bad_exit_reason, Reason})
    after 2000 ->
	    ?line test_server:fail(no_exit)
    end,
    %% Check that Cpid3 did die
    receive
	{'EXIT', CPid3, _} -> ok
    after 2000 ->
	    ?line test_server:fail(no_exit)
    end,
    %% Check that Cpid1 didn't die
    receive
	{'EXIT', CPid1, _} ->
	    ?line test_server:fail(bad_exit)
    after
	100 -> ok
    end,
    Children = supervisor:which_children(sup_test),
    if length(Children) == 3 -> ok;
       true -> ?line test_server:fail({bad_child_list, Children})
    end,
    ?line [3,3,0,3] = get_child_counts(sup_test),

    %% Test that no old children is still alive
    SCh = lists:map(fun({_,P,_,_}) -> P end, Children),
    case lists:member(CPid1, SCh) of
	true -> ok;
	false -> ?line test_server:fail(bad_child)
    end,
    case lists:member(CPid2, SCh) of
	true -> ?line test_server:fail(bad_child);
	false -> ok
    end,
    case lists:member(CPid3, SCh) of
	true -> ?line test_server:fail(bad_child);
	false -> ok
    end,
   
    %% Test restart frequency property
    [{child3, Pid3, _, _}|_] = supervisor:which_children(sup_test),
    Pid3 ! die,
    test_server:sleep(100),
    [_,{child2, Pid4, _, _}|_] = supervisor:which_children(sup_test),
    Pid4 ! die,
    receive
	{'EXIT', Pid, _} -> ok
    after 3000 -> ?line test_server:fail(restart_failed)
    end,
    exit(Pid, shutdown).

%-------------------------------------------------------------------------
rest_for_one_escalation(doc) ->
    ["Test restart escalation on a rest_for_one supervisor."];
rest_for_one_escalation(suite) -> [];
rest_for_one_escalation(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    Child1 = {child1, {supervisor_1, start_child, []}, permanent, 1000,
	     worker, []},
    Child2 = {child2, {supervisor_1, start_child, [error]},
	      permanent, 1000,
	     worker, []},
    ?line {ok, Pid} = start({ok, {{rest_for_one, 4, 3600}, []}}),
    ?line {ok, CPid1} = supervisor:start_child(sup_test, Child1),
    link(CPid1),
    ?line {ok, CPid2} = supervisor:start_child(sup_test, Child2),    
    link(CPid2),
    CPid1 ! die,
    receive
	{'EXIT', CPid1, died} -> ok;
	{'EXIT', CPid1, Reason} ->
	    ?line test_server:fail({bad_exit_reason, Reason})
    end,
    receive
	{'EXIT', CPid2, _} -> ok
    after
	2000 -> ?line test_server:fail(not_terminated)
    end,
    receive
	{'EXIT', Pid, _} -> ok
    after
	4000 -> ?line test_server:fail(supervisor_alive)
    end,
    ok.

%-------------------------------------------------------------------------
child_unlink(doc)-> ["Test that the supervisor does not hang forever if "
    "the child unliks and then is terminated by the supervisor."];
child_unlink(suite) -> [];
child_unlink(Config) when is_list(Config) ->
    
    ?line {ok, SupPid} = start({ok, {{one_for_one, 2, 3600}, []}}),
    
    Child = {naughty_child, {naughty_child, 
			     start_link, [SupPid]}, permanent, 
	     1000, worker, [supervisor_SUITE]},
    
    ?line {ok, _ChildPid} = supervisor:start_child(sup_test, Child),

    Pid = spawn(supervisor, terminate_child, [SupPid, naughty_child]),

    SupPid ! foo,
    timer:sleep(5000),
    %% If the supervisor did not hang it will have got rid of the 
    %% foo message that we sent.
    case erlang:process_info(SupPid, message_queue_len) of
	{message_queue_len, 0}->
	    ok;
	_ ->
	    exit(Pid, kill),
	    ?line test_server:fail(supervisor_hangs)
    end.
%-------------------------------------------------------------------------

tree(doc) ->
    ["Test a basic supervison tree."];
tree(suite) ->
    [];
tree(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    
    Child1 = {child1, {supervisor_1, start_child, []},
	      permanent, 1000,
	      worker, []},
    Child2 = {child2, {supervisor_1, start_child, []},
	      permanent, 1000,
	      worker, []},
    Child3 = {child3, {supervisor_1, start_child, [error]},
	      permanent, 1000,
	      worker, []},
    Child4 = {child4, {supervisor_1, start_child, []},
	      permanent, 1000,
	      worker, []},

    ChildSup1 = {supchild1, 
		 {supervisor, start_link,
		  [?MODULE, {ok, {{one_for_one, 4, 3600}, [Child1, Child2]}}]},
		 permanent, infinity,
		 supervisor, []},
    ChildSup2 = {supchild2, 
		 {supervisor, start_link, 
		  [?MODULE, {ok, {{one_for_one, 4, 3600}, []}}]},
		 permanent, infinity,
		 supervisor, []},

    %% Top supervisor
    ?line {ok, Pid} = start({ok, {{one_for_all, 4, 3600}, []}}),
    
    %% Child supervisors  
    ?line {ok, Sup1} = supervisor:start_child(Pid, ChildSup1),
    ?line {ok, Sup2} = supervisor:start_child(Pid, ChildSup2),
    ?line [2,2,2,0] = get_child_counts(Pid),
    
    %% Workers
     ?line [{_, CPid2, _, _},{_, CPid1, _, _}] = 
	supervisor:which_children(Sup1),
    ?line [2,2,0,2] = get_child_counts(Sup1),
    ?line [0,0,0,0] = get_child_counts(Sup2),
   
    %% Dynamic children
    ?line {ok, CPid3} = supervisor:start_child(Sup2, Child3),
    ?line {ok, CPid4} = supervisor:start_child(Sup2, Child4),
    ?line [2,2,0,2] = get_child_counts(Sup1),
    ?line [2,2,0,2] = get_child_counts(Sup2),
    
    link(Sup1),
    link(Sup2),
    link(CPid1),
    link(CPid2),
    link(CPid3),
    link(CPid4),
 
    %% Test that the only the process that dies is restarted
    CPid4 ! die,
 
    receive
	{'EXIT', CPid4, _} -> ?line ok
    after 10000 ->
	    ?line test_server:fail(child_was_not_killed)
    end,
    
    test_server:sleep(100),
    
    ?line [{_, CPid2, _, _},{_, CPid1, _, _}] = 
	supervisor:which_children(Sup1),
    ?line [2,2,0,2] = get_child_counts(Sup1),
    
    ?line [{_, NewCPid4, _, _},{_, CPid3, _, _}] = 
	supervisor:which_children(Sup2),
    ?line [2,2,0,2] = get_child_counts(Sup2),
    
    link(NewCPid4),

    %% Test that supervisor tree is restarted, but not dynamic children.
    CPid3 ! die,

    receive
	{'EXIT', CPid3, died} -> ?line ok;
	{'EXIT', CPid3, Reason} ->
	    ?line test_server:fail({bad_exit_reason, Reason})
    after 1000 ->
	    ?line test_server:fail(child_was_not_killed)
    end,

    test_server:sleep(1000),

    receive
 	{'EXIT', NewCPid4, _} ->  ?line ok
    after 1000 ->
 	    ?line test_server:fail(child_was_not_killed)
    end,
    
    receive
	{'EXIT', Sup2, _} -> ?line  ok
    after 1000 ->
	    ?line test_server:fail(child_was_not_killed)
    end,
    
    receive
	{'EXIT', CPid1, _} ->  ?line ok
    after 1000 ->
	    ?line test_server:fail(child_was_not_killed)
    end,
    
    receive
	{'EXIT', CPid2, _} -> ?line  ok
    after 1000 ->
	  ?line test_server:fail(child_was_not_killed)
    end,
    
    receive
	{'EXIT', Sup1, _} -> ?line  ok
    after 1000 ->
	    ?line test_server:fail(child_was_not_killed)
    end,
        
    ?line [{supchild2, NewSup2, _, _},{supchild1, NewSup1, _, _}] =
	supervisor:which_children(Pid),
    ?line [2,2,2,0] = get_child_counts(Pid),
    
    ?line [{child2, _, _, _},{child1, _, _, _}]  =
	supervisor:which_children(NewSup1),
    ?line [2,2,0,2] = get_child_counts(NewSup1),

    ?line [] = supervisor:which_children(NewSup2),
    ?line [0,0,0,0] = get_child_counts(NewSup2),
    
    ok.
%-------------------------------------------------------------------------
count_children_allocator_test(MemoryState) ->
    Allocators = [temp_alloc, eheap_alloc, binary_alloc, ets_alloc,
		  driver_alloc, sl_alloc, ll_alloc, fix_alloc, std_alloc,
		  sys_alloc],
    MemoryStateList = element(4, MemoryState),
    AllocTypes = [lists:keyfind(Alloc, 1, MemoryStateList)
		  || Alloc <- Allocators],
    AllocStates = [lists:keyfind(e, 1, AllocValue)
		   || {_Type, AllocValue} <- AllocTypes],
    lists:all(fun(State) -> State == {e, true} end, AllocStates).

count_children_memory(doc) ->
    ["Test that which_children eats memory, but count_children does not."];
count_children_memory(suite) ->
    MemoryState = erlang:system_info(allocator),
    case count_children_allocator_test(MemoryState) of
	true -> [];
	false ->
	    {skip, "+Meamin used during test; erlang:memory/1 not available"}
    end;
count_children_memory(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    Child = {child, {supervisor_1, start_child, []}, temporary, 1000,
	     worker, []},
    ?line {ok, _Pid} = start({ok, {{simple_one_for_one, 2, 3600}, [Child]}}),
    [supervisor:start_child(sup_test, []) || _Ignore <- lists:seq(1,1000)],

    garbage_collect(),
    _Size1 = erlang:memory(processes_used),
    Children = supervisor:which_children(sup_test),
    _Size2 = erlang:memory(processes_used),
    ChildCount = get_child_counts(sup_test),
    Size3 = erlang:memory(processes_used),

    [supervisor:start_child(sup_test, []) || _Ignore2 <- lists:seq(1,1000)],

    garbage_collect(),
    Children2 = supervisor:which_children(sup_test),
    Size4 = erlang:memory(processes_used),
    ChildCount2 = get_child_counts(sup_test),
    Size5 = erlang:memory(processes_used),

    garbage_collect(),
    Children3 = supervisor:which_children(sup_test),
    Size6 = erlang:memory(processes_used),
    ChildCount3 = get_child_counts(sup_test),
    Size7 = erlang:memory(processes_used),

    ?line 1000 = length(Children),
    ?line [1,1000,0,1000] = ChildCount,
    ?line 2000 = length(Children2),
    ?line [1,2000,0,2000] = ChildCount2,
    ?line Children3 = Children2,
    ?line ChildCount3 = ChildCount2,

    %% count_children consumes memory using an accumulator function,
    %% but the space can be reclaimed incrementally, whereas
    %% which_children generates a return list.
    case (Size5 =< Size4) of
	true -> ok;
	false ->
	    ?line test_server:fail({count_children, used_more_memory})
    end,
    case Size7 =< Size6 of
	true -> ok;
	false ->
	    ?line test_server:fail({count_children, used_more_memory})
    end,

    case Size4 > Size3 of
	true -> ok;
	false ->
	    ?line test_server:fail({which_children, used_no_memory})
    end,
    case Size6 > Size5 of
	true -> ok;
	false ->
	    ?line test_server:fail({which_children, used_no_memory})
    end,

    [exit(Pid, kill) || {undefined, Pid, worker, _Modules} <- Children3],
    test_server:sleep(100),
    ?line [1,0,0,0] = get_child_counts(sup_test),

    ok.
