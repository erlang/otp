%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2025. All Rights Reserved.
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
%% Description: Tests supervisor.erl

-module(supervisor_SUITE).

-include_lib("common_test/include/ct.hrl").

%% Testserver specific export
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, init_per_testcase/2,
	 end_per_testcase/2]).

%% Internal export
-export([init/1, terminate_all_children/1,
         middle9212/0, gen_server9212/0, handle_info/2, start_registered_name/1, log/2]).

%% API tests
-export([ sup_start_normal/1, sup_start_ignore_init/1, 
	  sup_start_ignore_child/1, sup_start_ignore_temporary_child/1,
	  sup_start_ignore_temporary_child_start_child/1,
	  sup_start_ignore_temporary_child_start_child_simple/1,
          sup_start_ignore_permanent_child_start_child_simple/1,
	  sup_start_error_return/1, sup_start_fail/1,
          sup_start_child_returns_error/1,
          sup_start_restart_child_returns_error/1,
          sup_start_child_returns_error_simple/1,
	  sup_start_map/1, sup_start_map_simple/1,
	  sup_start_map_faulty_specs/1,
	  sup_stop_infinity/1, sup_stop_timeout/1, sup_stop_timeout_dynamic/1,
	  sup_stop_brutal_kill/1, sup_stop_brutal_kill_dynamic/1,
          sup_stop_race/1, sup_stop_non_shutdown_exit_dynamic/1,
	  child_adm/1, child_adm_simple/1, child_specs/1, child_specs_map/1,
	  extra_return/1, sup_flags/1]).

%% Tests concept permanent, transient and temporary 
-export([external_start_no_progress_log/1, 
         permanent_normal/1, transient_normal/1, temporary_normal/1,
         permanent_shutdown/1, transient_shutdown/1, temporary_shutdown/1,
         faulty_application_shutdown/1,
         permanent_abnormal/1, transient_abnormal/1,
         temporary_abnormal/1, temporary_bystander/1]).

%% Restart strategy tests 
-export([ multiple_restarts/1,
	  one_for_one/1,
	  one_for_one_escalation/1, one_for_all/1,
	  one_for_all_escalation/1, one_for_all_other_child_fails_restart/1,
	  simple_one_for_one/1, simple_one_for_one_escalation/1,
          simple_one_for_one_corruption/1,
	  rest_for_one/1, rest_for_one_escalation/1,
	  rest_for_one_other_child_fails_restart/1,
	  simple_one_for_one_extra/1, simple_one_for_one_shutdown/1,
          simple_one_for_one_restart_ignore/1]).

%% Significant child tests
-export([ nonsignificant_temporary/1, nonsignificant_transient/1,
	  significant_temporary/1, significant_transient/1,
	  significant_simple/1, significant_bystander/1,
	  significant_escalation/1, significant_upgrade_never_any/1,
	  significant_upgrade_any_never/1, significant_upgrade_never_all/1,
	  significant_upgrade_all_never/1, significant_upgrade_any_all/1,
	  significant_upgrade_all_any/1, significant_upgrade_child/1]).

%% Misc tests
-export([child_unlink/1, tree/1, count_children/1, count_children_supervisor/1,
	 count_restarting_children/1, get_callback_module/1,
	 do_not_save_start_parameters_for_temporary_children/1,
	 do_not_save_child_specs_for_temporary_children/1,
	 simple_one_for_one_scale_many_temporary_children/1,
         simple_global_supervisor/1, hanging_restart_loop/1,
	 hanging_restart_loop_rest_for_one/1,
	 hanging_restart_loop_simple/1, code_change/1, code_change_map/1,
	 code_change_simple/1, code_change_simple_map/1,
         order_of_children/1, scale_start_stop_many_children/1,
         format_log_1/1, format_log_2/1, already_started_outside_supervisor/1,
	 which_children/1, which_children_simple_one_for_one/1]).

%%-------------------------------------------------------------------------

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() -> 
    [{group, sup_start}, {group, sup_start_map}, {group, sup_stop}, child_adm,
     child_adm_simple, extra_return, child_specs, child_specs_map, sup_flags,
     multiple_restarts,
     {group, restart_one_for_one},
     {group, restart_one_for_all},
     {group, restart_simple_one_for_one},
     {group, restart_rest_for_one},
     {group, normal_termination},
     {group, shutdown_termination},
     {group, abnormal_termination}, child_unlink, tree,
     {group, significant},
     count_children, count_children_supervisor, count_restarting_children,
     get_callback_module,
     do_not_save_start_parameters_for_temporary_children,
     do_not_save_child_specs_for_temporary_children,
     simple_one_for_one_scale_many_temporary_children, temporary_bystander,
     simple_global_supervisor, hanging_restart_loop,
     hanging_restart_loop_rest_for_one, hanging_restart_loop_simple,
     code_change, code_change_map, code_change_simple, code_change_simple_map,
     order_of_children, scale_start_stop_many_children,
     format_log_1, format_log_2, already_started_outside_supervisor,
     which_children, which_children_simple_one_for_one].

groups() -> 
    [{sup_start, [],
      [sup_start_normal, sup_start_ignore_init,
       sup_start_ignore_child, sup_start_ignore_temporary_child,
       sup_start_ignore_temporary_child_start_child,
       sup_start_ignore_temporary_child_start_child_simple,
       sup_start_ignore_permanent_child_start_child_simple,
       sup_start_error_return, sup_start_fail,
       sup_start_child_returns_error, sup_start_restart_child_returns_error,
       sup_start_child_returns_error_simple
      ]},
     {sup_start_map, [],
      [sup_start_map, sup_start_map_simple, sup_start_map_faulty_specs]},
     {sup_stop, [],
      [sup_stop_infinity, sup_stop_timeout, sup_stop_timeout_dynamic,
       sup_stop_brutal_kill, sup_stop_brutal_kill_dynamic,
       sup_stop_race, sup_stop_non_shutdown_exit_dynamic]},
     {normal_termination, [],
      [external_start_no_progress_log, permanent_normal, transient_normal, temporary_normal]},
     {shutdown_termination, [],
      [permanent_shutdown, transient_shutdown, temporary_shutdown,
       faulty_application_shutdown]},
     {abnormal_termination, [],
      [permanent_abnormal, transient_abnormal,
       temporary_abnormal]},
     {restart_one_for_one, [],
      [one_for_one, one_for_one_escalation]},
     {restart_one_for_all, [],
      [one_for_all, one_for_all_escalation,
       one_for_all_other_child_fails_restart]},
     {restart_simple_one_for_one, [],
      [simple_one_for_one, simple_one_for_one_shutdown,
       simple_one_for_one_extra, simple_one_for_one_escalation,
       simple_one_for_one_corruption, simple_one_for_one_restart_ignore]},
     {restart_rest_for_one, [],
      [rest_for_one, rest_for_one_escalation,
       rest_for_one_other_child_fails_restart]},
     {significant, [],
      [nonsignificant_temporary, nonsignificant_transient,
       significant_temporary, significant_transient,
       significant_simple, significant_bystander,
       significant_escalation,
       significant_upgrade_never_any, significant_upgrade_any_never,
       significant_upgrade_never_all, significant_upgrade_all_never,
       significant_upgrade_any_all, significant_upgrade_all_any,
       significant_upgrade_child]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    %% Clean up to avoid unnecessary error reports in the shell
    case whereis(sup_test) of
        SupPid when is_pid(SupPid) ->
            unlink(SupPid),
            exit(SupPid,shutdown),
            ok;
        _ ->
            error
    end,
    ok.

start_link(InitResult) ->
    supervisor:start_link({local, sup_test}, ?MODULE, InitResult).

%% Simulate different supervisors callback.  
init(fail) ->
    erlang:error(fail);
init(InitResult) ->
    InitResult.

%% Respect proplist return of supervisor:count_children
get_child_counts(Supervisor) ->
    Counts = supervisor:count_children(Supervisor),
    [proplists:get_value(specs, Counts),
     proplists:get_value(active, Counts),
     proplists:get_value(supervisors, Counts),
     proplists:get_value(workers, Counts)].

%%-------------------------------------------------------------------------
%% Test cases starts here.
%% -------------------------------------------------------------------------
%% Tests that the supervisor process starts correctly and that it can
%% be terminated gracefully.
sup_start_normal(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    {ok, Pid} = start_link({ok, {{one_for_one, 2, 3600}, []}}),
    terminate(Pid, shutdown).

%%-------------------------------------------------------------------------
%% Tests what happens if init-callback returns ignore.
sup_start_ignore_init(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ignore = start_link(ignore),
    check_no_exit(100).

%%-------------------------------------------------------------------------
%% Tests what happens if init-callback returns ignore.
sup_start_ignore_child(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    {ok, _Pid}  = start_link({ok, {{one_for_one, 2, 3600}, []}}),
    Child1 = {child1, {supervisor_1, start_child, [ignore]}, 
	      permanent, 1000, worker, []},
    Child2 = {child2, {supervisor_1, start_child, []}, permanent,
	      1000, worker, []},

    {ok, undefined} = supervisor:start_child(sup_test, Child1),
    {ok, CPid2} = supervisor:start_child(sup_test, Child2),

    [{child2, CPid2, worker, []},{child1, undefined, worker, []}]
	= supervisor:which_children(sup_test),
    [2,1,0,2] = get_child_counts(sup_test).

%%-------------------------------------------------------------------------
%% Tests what happens if child's init-callback returns ignore for a
%% temporary child when ChildSpec is returned directly from supervisor
%% init callback.
%% Child spec shall NOT be saved!!!
sup_start_ignore_temporary_child(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    Child1 = {child1, {supervisor_1, start_child, [ignore]},
	      temporary, 1000, worker, []},
    Child2 = {child2, {supervisor_1, start_child, []}, temporary,
	      1000, worker, []},
    {ok, _Pid}  = start_link({ok, {{one_for_one, 2, 3600}, [Child1,Child2]}}),

    [{child2, CPid2, worker, []}] = supervisor:which_children(sup_test),
    true = is_pid(CPid2),
    [1,1,0,1] = get_child_counts(sup_test).

%%-------------------------------------------------------------------------
%% Tests what happens if child's init-callback returns ignore for a
%% temporary child when child is started with start_child/2.
%% Child spec shall NOT be saved!!!
sup_start_ignore_temporary_child_start_child(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    {ok, _Pid}  = start_link({ok, {{one_for_one, 2, 3600}, []}}),
    Child1 = {child1, {supervisor_1, start_child, [ignore]},
	      temporary, 1000, worker, []},
    Child2 = {child2, {supervisor_1, start_child, []}, temporary,
	      1000, worker, []},

    {ok, undefined} = supervisor:start_child(sup_test, Child1),
    {ok, CPid2} = supervisor:start_child(sup_test, Child2),

    [{child2, CPid2, worker, []}] = supervisor:which_children(sup_test),
    [1,1,0,1] = get_child_counts(sup_test).

%%-------------------------------------------------------------------------
%% Tests what happens if child's init-callback returns ignore for a
%% temporary child when child is started with start_child/2, and the
%% supervisor is simple_one_for_one.
%% Child spec shall NOT be saved!!!
sup_start_ignore_temporary_child_start_child_simple(Config)
  when is_list(Config) ->
    process_flag(trap_exit, true),
    Child1 = {child1, {supervisor_1, start_child, [ignore]},
	      temporary, 1000, worker, []},
    {ok, _Pid}  = start_link({ok, {{simple_one_for_one, 2, 3600}, [Child1]}}),

    {ok, undefined} = supervisor:start_child(sup_test, []),
    {ok, CPid2} = supervisor:start_child(sup_test, []),

    [{undefined, CPid2, worker, []}] = supervisor:which_children(sup_test),
    [1,1,0,1] = get_child_counts(sup_test).

%%-------------------------------------------------------------------------
%% Tests what happens if child's init-callback returns ignore for a
%% permanent child when child is started with start_child/2, and the
%% supervisor is simple_one_for_one.
%% Child spec shall NOT be saved!!!
sup_start_ignore_permanent_child_start_child_simple(Config)
  when is_list(Config) ->
    process_flag(trap_exit, true),
    Child1 = {child1, {supervisor_1, start_child, [ignore]},
	      permanent, 1000, worker, []},
    {ok, Pid}  = start_link({ok, {{simple_one_for_one, 2, 3600}, [Child1]}}),

    {ok, undefined} = supervisor:start_child(sup_test, []),
    {ok, CPid2} = supervisor:start_child(sup_test, []),

    [{undefined, CPid2, worker, []}] = supervisor:which_children(sup_test),
    [1,1,0,1] = get_child_counts(sup_test),

    %% Regression test: check that the supervisor terminates without error.
    exit(Pid, shutdown),
    check_exit_reason(Pid, shutdown).

%%-------------------------------------------------------------------------
%% Tests what happens if init-callback returns a invalid value.
sup_start_error_return(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    %% The bad return is processed in supervisor:init/1
    InitResult = invalid,
    {error, {bad_return, {?MODULE, init, InitResult}}} =
        start_link(InitResult),
    check_no_exit(100).

%%-------------------------------------------------------------------------
%% Tests what happens if init-callback fails.
sup_start_fail(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    %% The exception is processed in gen_server:init_it/2
    ErrorReason = fail,
    {error, {ErrorReason, _Stacktrace}} = start_link(ErrorReason),
    check_no_exit(100).

%%-------------------------------------------------------------------------
%% Test what happens when the start function for a child returns
%% {error,Reason} or some other term().
sup_start_restart_child_returns_error(_Config) ->
    process_flag(trap_exit, true),
    Child = {child1, {supervisor_1, start_child, [error]},
              permanent, 1000, worker, []},
    {ok, _Pid}  = start_link({ok, {{one_for_one, 2, 3600}, [Child]}}),

    ok = supervisor:terminate_child(sup_test, child1),
    {error,{function_clause,_}} = supervisor:restart_child(sup_test,child1),

    [{child1,undefined,worker,[]}] = supervisor:which_children(sup_test),
    ok.

%%-------------------------------------------------------------------------
%% Test what happens when the start function for a child returns
%% {error,Reason} or some other term().
sup_start_child_returns_error(_Config) ->
    process_flag(trap_exit, true),
    Child1 = {child1, {supervisor_1, start_child, [{return,{error,reason}}]},
              permanent, 1000, worker, []},
    Child2 = {child2, {supervisor_1, start_child, [{return,error_reason}]},
              permanent, 1000, worker, []},
    {ok, _Pid}  = start_link({ok, {{one_for_one, 2, 3600}, []}}),

    {error,{reason,_}} = supervisor:start_child(sup_test,Child1),
    {error,{error_reason,_}} = supervisor:start_child(sup_test,Child2),

    [] = supervisor:which_children(sup_test),
    ok.

%%-------------------------------------------------------------------------
%% Test what happens when the start function for a child returns
%% {error,Reason} - simple_one_for_one
sup_start_child_returns_error_simple(_Config) ->
    process_flag(trap_exit, true),
    Child = {child1, {supervisor_1, start_child, []},
             permanent, 1000, worker, []},
    {ok, _Pid}  = start_link({ok, {{simple_one_for_one, 2, 3600}, [Child]}}),

    {error,reason} = supervisor:start_child(sup_test,[{return,{error,reason}}]),
    {error,error_reason} = supervisor:start_child(sup_test,[{return,error_reason}]),

    [] = supervisor:which_children(sup_test),
    ok.

%%-------------------------------------------------------------------------
%% Tests that the supervisor process starts correctly with map
%% startspec, and that the full childspec can be read.
sup_start_map(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    Child1 = #{id=>child1, start=>{supervisor_1, start_child, []}},
    Child2 = #{id=>child2,
	       start=>{supervisor_1, start_child, []},
	       shutdown=>brutal_kill},
    Child3 = #{id=>child3,
	       start=>{supervisor_1, start_child, []},
	       type=>supervisor},
    {ok, Pid} = start_link({ok, {#{}, [Child1,Child2,Child3]}}),

    %% Check default values
    {ok,#{id:=child1,
	  start:={supervisor_1,start_child,[]},
	  restart:=permanent,
	  shutdown:=5000,
	  type:=worker,
	  modules:=[supervisor_1]}} = supervisor:get_childspec(Pid, child1),
    {ok,#{id:=child2,
	  start:={supervisor_1,start_child,[]},
	  restart:=permanent,
	  shutdown:=brutal_kill,
	  type:=worker,
	  modules:=[supervisor_1]}} = supervisor:get_childspec(Pid, child2),
    {ok,#{id:=child3,
	  start:={supervisor_1,start_child,[]},
	  restart:=permanent,
	  shutdown:=infinity,
	  type:=supervisor,
	  modules:=[supervisor_1]}} = supervisor:get_childspec(Pid, child3),
    {error,not_found} = supervisor:get_childspec(Pid, child4),
    terminate(Pid, shutdown).

%%-------------------------------------------------------------------------
%% Tests that the supervisor process starts correctly with map
%% startspec, and that the full childspec can be read when using
%% simple_one_for_one strategy.
sup_start_map_simple(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    SupFlags = #{strategy=>simple_one_for_one},
    ChildSpec = #{id=>undefined,
		  start=>{supervisor_1, start_child, []},
		  restart=>temporary},
    {ok, Pid} = start_link({ok, {SupFlags, [ChildSpec]}}),

    {ok, Child1} = supervisor:start_child(Pid, []),
    {ok, Child2} = supervisor:start_child(Pid, []),
    {ok, Child3} = supervisor:start_child(Pid, []),

    Spec = ChildSpec#{type=>worker, shutdown=>5000, modules=>[supervisor_1],
		      significant=>false},

    {ok, Spec} = supervisor:get_childspec(Pid, Child1),
    {ok, Spec} = supervisor:get_childspec(Pid, Child2),
    {ok, Spec} = supervisor:get_childspec(Pid, Child3),
    {error,not_found} = supervisor:get_childspec(Pid, self()),
    terminate(Pid, shutdown).

%%-------------------------------------------------------------------------
%% Tests that the supervisor produces good error messages when start-
%% and child specs are faulty.
sup_start_map_faulty_specs(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    Child1 = #{start=>{supervisor_1, start_child, []}},
    Child2 = #{id=>child2},
    Child3 = #{id=>child3,
	       start=>{supervisor_1, start_child, []},
	       silly_flag=>true},
    Child4 = child4,
    {error,{start_spec,missing_id}} = start_link({ok, {#{}, [Child1]}}),
    {error,{start_spec,missing_start}} = start_link({ok, {#{}, [Child2]}}),
    {ok,Pid} = start_link({ok, {#{}, [Child3]}}),
    terminate(Pid,shutdown),
    {error,{start_spec,{invalid_child_spec,child4}}} =
	start_link({ok, {#{}, [Child4]}}).

%%-------------------------------------------------------------------------
%% See sup_stop/1 when Shutdown = infinity, this walue is allowed for
%% children of type supervisor _AND_ worker.
sup_stop_infinity(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    {ok, Pid} = start_link({ok, {{one_for_one, 2, 3600}, []}}),
    Child1 = {child1, {supervisor_1, start_child, []}, 
	      permanent, infinity, supervisor, []},
    Child2 = {child2, {supervisor_1, start_child, []}, permanent,
	      infinity, worker, []},
    {ok, CPid1} = supervisor:start_child(sup_test, Child1),
    {ok, CPid2} = supervisor:start_child(sup_test, Child2),
    link(CPid1),
    link(CPid2),

    terminate(Pid, shutdown),
    check_exit_reason(CPid1, shutdown),
    check_exit_reason(CPid2, shutdown).

%%-------------------------------------------------------------------------
%% See sup_stop/1 when Shutdown = 1000
sup_stop_timeout(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    {ok, Pid} = start_link({ok, {{one_for_one, 2, 3600}, []}}),
    Child1 = {child1, {supervisor_1, start_child, []}, 
	      permanent, 1000, worker, []},
    Child2 = {child2, {supervisor_1, start_child, []}, permanent,
	      1000, worker, []},
    {ok, CPid1} = supervisor:start_child(sup_test, Child1),
    link(CPid1),
    {ok, CPid2} = supervisor:start_child(sup_test, Child2),
    link(CPid2),

    CPid2 ! {sleep, 200000},

    terminate(Pid, shutdown),

    check_exit_reason(CPid1, shutdown),
    check_exit_reason(CPid2, killed).

sup_stop_timeout_dynamic(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    Child = {child, {supervisor_1, start_child, []}, temporary, 1000, worker, []},
    {ok, Pid} = start_link({ok, {{simple_one_for_one, 2, 3600}, [Child]}}),
    {ok, CPid1} = supervisor:start_child(Pid, []),
    link(CPid1),
    {ok, CPid2} = supervisor:start_child(Pid, []),
    link(CPid2),
    CPid2 ! {sleep, 200000},
    terminate(Pid, shutdown),
    check_exit_reason(Pid, shutdown),
    check_exit_reason(CPid1, shutdown),
    check_exit_reason(CPid2, killed).


%%-------------------------------------------------------------------------
%% See sup_stop/1 when Shutdown = brutal_kill
sup_stop_brutal_kill(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    {ok, Pid} = start_link({ok, {{one_for_one, 2, 3600}, []}}),
    Child1 = {child1, {supervisor_1, start_child, []}, 
	      permanent, 1000, worker, []},
    Child2 = {child2, {supervisor_1, start_child, []}, permanent,
	      brutal_kill, worker, []},
    {ok, CPid1} = supervisor:start_child(sup_test, Child1),
    link(CPid1),
    {ok, CPid2} = supervisor:start_child(sup_test, Child2),
    link(CPid2),

    terminate(Pid, shutdown),

    check_exit_reason(CPid1, shutdown),
    check_exit_reason(CPid2, killed).

sup_stop_brutal_kill_dynamic(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    Child = {child, {supervisor_4, start_child, []}, temporary, brutal_kill, worker, []},
    {ok, Pid} = start_link({ok, {{simple_one_for_one, 2, 3600}, [Child]}}),
    CPids = lists:map(
        fun (_) ->
            {ok, CPid} = supervisor:start_child(Pid, []),
            link(CPid),
            CPid
        end,
        lists:seq(1, 10)),
    terminate(Pid, shutdown),
    check_exit_reason(Pid, shutdown),
    lists:foreach(
        fun (CPid) ->
            check_exit_reason(CPid, killed)
        end,
        CPids
    ).

%%-------------------------------------------------------------------------
%% Tests that a supervisor completes its shutdown properly while children
%% exit by themselves for reasons other than shutdown
sup_stop_race(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    {ok, Pid} = start_link({ok, {{one_for_one, 2, 3600}, []}}),
    {ok, CPidA} = supervisor:start_child(Pid,
        #{id => child_a,
          start => {supervisor_1, start_child, []},
          restart => temporary,
          shutdown => 1000}),
    link(CPidA),
    CPids = lists:foldl(
        fun ({Restart, Reason}, Acc) ->
            {ok, CPid} = supervisor:start_child(Pid,
                #{id => {child, make_ref()},
                  start => {supervisor_4, start_child, [Reason]},
                  restart => Restart,
                  shutdown => 1000
                }),
            link(CPid),
            Acc#{CPid => Reason}
        end,
        #{},
        [
            {Restart, Reason}
            || Restart <- [temporary, transient, permanent],
               Reason <- [normal, shutdown, {shutdown, test}, other]
        ]
    ),
    {ok, CPidZ} = supervisor:start_child(Pid,
        #{
            id => child_z,
            start => {supervisor_2, start_child, [1000]},
            restart => temporary,
            shutdown => 2000
        }),
    link(CPidZ),
    Self = self(),
    spawn_link(
        fun () ->
            timer:sleep(200),
            maps:foreach(
                fun (CPid, _) ->
                    CPid ! stop
                end,
                CPids
            ),
            unlink(Self)
        end
    ),
    ok = terminate(Pid, shutdown),
    check_exit_reason(Pid, shutdown),
    check_exit_reason(CPidZ, shutdown),
    maps:foreach(
        fun (CPid, Reason) ->
            check_exit_reason(CPid, Reason)
        end,
        CPids),
    check_exit_reason(CPidA, shutdown).

%%-------------------------------------------------------------------------
%% Tests that a simple_one_for_one supervisor shuts down correctly when
%% some children exit with a reason other than shutdown
sup_stop_non_shutdown_exit_dynamic(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    lists:foreach(
        fun (Restart) ->
            Child = {child, {supervisor_4, start_child, []}, Restart, 5000, worker, []},
            {ok, Pid} = start_link({ok, {{simple_one_for_one, 2, 3600}, [Child]}}),
            CPids = lists:foldl(
                fun (Reason, Acc) ->
                    {ok, CPid} = supervisor:start_child(Pid, [Reason]),
                    link(CPid),
                    Acc#{CPid => Reason}
                end,
                #{},
                [normal, shutdown, {shutdown, test}, other]
            ),
            ok = terminate(Pid, shutdown),
            check_exit_reason(Pid, shutdown),
            maps:foreach(
                fun (CPid, Reason) ->
                    check_exit_reason(CPid, Reason)
                end,
                CPids)
        end,
        [temporary, transient, permanent]
    ).

%%-------------------------------------------------------------------------
%% The start function provided to start a child may return {ok, Pid}
%% or {ok, Pid, Info}, if it returns the latter check that the
%% supervisor ignores the Info, and includes it unchanged in return
%% from start_child/2 and restart_child/2.
extra_return(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    Child = {child1, {supervisor_1, start_child, [extra_return]}, 
	     permanent, 1000,
	     worker, []},
    {ok, _Pid} = start_link({ok, {{one_for_one, 2, 3600}, [Child]}}),
    [{child1, CPid, worker, []}] = supervisor:which_children(sup_test),
    link(CPid),
    {error, not_found} = supervisor:terminate_child(sup_test, hej),
    {error, not_found} = supervisor:delete_child(sup_test, hej),
    {error, not_found} = supervisor:restart_child(sup_test, hej),
    {error, running} = supervisor:delete_child(sup_test, child1),
    {error, running} = supervisor:restart_child(sup_test, child1),
    [{child1, CPid, worker, []}] = supervisor:which_children(sup_test),
    [1,1,0,1] = get_child_counts(sup_test),

    ok = supervisor:terminate_child(sup_test, child1),
    check_exit_reason(CPid, shutdown),
    [{child1,undefined,worker,[]}] = supervisor:which_children(sup_test),
    [1,0,0,1] = get_child_counts(sup_test),

    {ok, CPid2,extra_return} =
	supervisor:restart_child(sup_test, child1),
    [{child1, CPid2, worker, []}] = supervisor:which_children(sup_test),
    [1,1,0,1] = get_child_counts(sup_test),

    ok = supervisor:terminate_child(sup_test, child1),
    ok = supervisor:terminate_child(sup_test, child1),
    ok = supervisor:delete_child(sup_test, child1),
    {error, not_found} = supervisor:restart_child(sup_test, child1),
    [] = supervisor:which_children(sup_test),
    [0,0,0,0] = get_child_counts(sup_test),

    {ok, CPid3, extra_return} = supervisor:start_child(sup_test, Child),
    [{child1, CPid3, worker, []}] = supervisor:which_children(sup_test),
    [1,1,0,1] = get_child_counts(sup_test),

    %% Check that it can be automatically restarted
    terminate(CPid3, abnormal),
    [{child1, CPid4, worker, []}] = supervisor:which_children(sup_test),
    [1,1,0,1] = get_child_counts(sup_test),
    if (not is_pid(CPid4)) orelse CPid4=:=CPid3 ->
            ct:fail({not_restarted,CPid3,CPid4});
       true ->
            ok
    end.

%%-------------------------------------------------------------------------
%% Test API functions start_child/2, terminate_child/2, delete_child/2
%% restart_child/2, which_children/1, count_children/1. Only correct
%% childspecs are used, handling of incorrect childspecs is tested in
%% child_specs/1.
child_adm(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    Child = {child1, {supervisor_1, start_child, []}, permanent, 1000,
	     worker, []},
    {ok, Pid} = start_link({ok, {{one_for_one, 2, 3600}, [Child]}}),

    %% Test that supervisors of static nature are hibernated after start
    {current_function, {gen_server, loop_hibernate, 4}} =
	process_info(Pid, current_function),

    [{child1, CPid, worker, []}] = supervisor:which_children(sup_test),
    [1,1,0,1] = get_child_counts(sup_test),
    link(CPid),
    %% Start of an already runnig process 
    {error,{already_started, CPid}} =
	supervisor:start_child(sup_test, Child),

    %% Termination
    {error, not_found} = supervisor:terminate_child(sup_test, hej),
    {'EXIT',{noproc,{gen_server,call, _}}} =
	(catch supervisor:terminate_child(foo, child1)),
    ok = supervisor:terminate_child(sup_test, child1),
    check_exit_reason(CPid, shutdown),
    [{child1,undefined,worker,[]}] = supervisor:which_children(sup_test),
    [1,0,0,1] = get_child_counts(sup_test),
    %% Like deleting something that does not exist, it will succeed!
    ok = supervisor:terminate_child(sup_test, child1),

    %% Start of already existing but not running process 
    {error,already_present} = supervisor:start_child(sup_test, Child),

    %% Restart
    {ok, CPid2} = supervisor:restart_child(sup_test, child1),
    [{child1, CPid2, worker, []}] = supervisor:which_children(sup_test),
    [1,1,0,1] = get_child_counts(sup_test),
    {error, running} = supervisor:restart_child(sup_test, child1),
    {error, not_found} = supervisor:restart_child(sup_test, child2),

    %% Deletion
    {error, running} = supervisor:delete_child(sup_test, child1),
    {error, not_found} = supervisor:delete_child(sup_test, hej),
    {'EXIT',{noproc,{gen_server,call, _}}} =
	(catch supervisor:delete_child(foo, child1)),
    ok = supervisor:terminate_child(sup_test, child1),
    ok = supervisor:delete_child(sup_test, child1),
    {error, not_found} = supervisor:restart_child(sup_test, child1),
    [] = supervisor:which_children(sup_test),
    [0,0,0,0] = get_child_counts(sup_test),

    %% Start
    {'EXIT',{noproc,{gen_server,call, _}}} =
	(catch supervisor:start_child(foo, Child)),
    {ok, CPid3} = supervisor:start_child(sup_test, Child),
    [{child1, CPid3, worker, []}] = supervisor:which_children(sup_test),
    [1,1,0,1] = get_child_counts(sup_test),

    %% Terminate with Pid not allowed when not simple_one_for_one
    {error,not_found} = supervisor:terminate_child(sup_test, CPid3),
    [{child1, CPid3, worker, []}] = supervisor:which_children(sup_test),
    [1,1,0,1] = get_child_counts(sup_test),

    {'EXIT',{noproc,{gen_server,call,[foo,which_children,infinity]}}}
	= (catch supervisor:which_children(foo)),
    {'EXIT',{noproc,{gen_server,call,[foo,count_children,infinity]}}}
	= (catch supervisor:count_children(foo)),
    ok.
%%-------------------------------------------------------------------------
%% The API functions terminate_child/2, delete_child/2 restart_child/2
%% are not valid for a simple_one_for_one supervisor check that the
%% correct error message is returned.
child_adm_simple(Config) when is_list(Config) ->
    Child = {child, {supervisor_1, start_child, []}, permanent, 1000,
	     worker, []},
    {ok, Pid} = start_link({ok, {{simple_one_for_one, 2, 3600}, [Child]}}),

       %% Test that supervisors of dynamic nature are not hibernated after start
    {current_function, {_, Function, _}} =
	process_info(Pid, current_function),
    true = Function =/= hibernate,

    %% In simple_one_for_one all children are added dynamically 
    [] = supervisor:which_children(sup_test),
    [1,0,0,0] = get_child_counts(sup_test),

    %% Start
    {'EXIT',{noproc,{gen_server,call, _}}} =
	(catch supervisor:start_child(foo, [])),
    {ok, CPid1} = supervisor:start_child(sup_test, []),
    [{undefined, CPid1, worker, []}] =
	supervisor:which_children(sup_test),
    [1,1,0,1] = get_child_counts(sup_test),

    {ok, CPid2} = supervisor:start_child(sup_test, []),
    Children = supervisor:which_children(sup_test),
    2 = length(Children),
    true = lists:member({undefined, CPid2, worker, []}, Children),
    true = lists:member({undefined, CPid1, worker, []}, Children),
    [1,2,0,2] = get_child_counts(sup_test),

    %% Termination
    {error, simple_one_for_one} = supervisor:terminate_child(sup_test, child1),
    [1,2,0,2] = get_child_counts(sup_test),
    ok = supervisor:terminate_child(sup_test,CPid1),
    [_] = supervisor:which_children(sup_test),
    [1,1,0,1] = get_child_counts(sup_test),
    false = erlang:is_process_alive(CPid1),
    %% Terminate non-existing proccess is ok
    ok = supervisor:terminate_child(sup_test,CPid1),
    [_] = supervisor:which_children(sup_test),
    [1,1,0,1] = get_child_counts(sup_test),
    %% Terminate pid which is not a child of this supervisor is not ok
    NoChildPid = spawn_link(fun() -> receive after infinity -> ok end end),
    {error, not_found} = supervisor:terminate_child(sup_test, NoChildPid),
    true = erlang:is_process_alive(NoChildPid),

    %% Restart
    {error, simple_one_for_one} = supervisor:restart_child(sup_test, child1),

    %% Deletion
    {error, simple_one_for_one} = supervisor:delete_child(sup_test, child1),
    ok.

%%-------------------------------------------------------------------------
%% Tests child specs (tuple form), invalid formats should be rejected.
child_specs(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    {ok, Pid} = start_link({ok, {{one_for_one, 2, 3600}, []}}),
    {error, _} = supervisor:start_child(sup_test, hej),

    %% Bad child specs 
    B1 = {child, mfa, permanent, 1000, worker, []},
    B2 = {child, {m,f,[a]}, prmanent, 1000, worker, []}, 
    B3 = {child, {m,f,[a]}, permanent, -10, worker, []},
    B4 = {child, {m,f,[a]}, permanent, 10, wrker, []},
    B5 = {child, {m,f,[a]}, permanent, 1000, worker, dy},
    B6 = {child, {m,f,[a]}, permanent, 1000, worker, [1,2,3]},

    {error, {invalid_mfa,mfa}} = supervisor:start_child(sup_test, B1),
    {error, {invalid_restart_type, prmanent}} =
	supervisor:start_child(sup_test, B2),
    {error,  {invalid_shutdown,-10}}
	= supervisor:start_child(sup_test, B3),
    {error, {invalid_child_type,wrker}}
	= supervisor:start_child(sup_test, B4),
    {error, {invalid_modules,dy}}
	= supervisor:start_child(sup_test, B5),

    {error, {badarg, _}} = supervisor:check_childspecs(B1), % should be list
    {error, {invalid_mfa,mfa}} = supervisor:check_childspecs([B1]),
    {error, {invalid_restart_type,prmanent}} =
	supervisor:check_childspecs([B2]),
    {error, {invalid_shutdown,-10}} = supervisor:check_childspecs([B3]),
    {error, {invalid_child_type,wrker}}
	= supervisor:check_childspecs([B4]),
    {error, {invalid_modules,dy}} = supervisor:check_childspecs([B5]),
    {error, {invalid_module, 1}} =
	supervisor:check_childspecs([B6]),

    lists:foreach(
	fun (ChildSpec) ->
	    ok = supervisor:check_childspecs([ChildSpec])
	end,
	[
	    {child, {m, f, [a]}, Restart, Shutdown, Type, Modules}
	    ||
		Restart <- [permanent, transient, temporary],
		Shutdown <- [0, 1000, infinity, brutal_kill],
		Type <- [supervisor, worker],
		Modules <- [dynamic, [], [m], [m1, m2]]
	]
    ),

    C1 = {child, {m,f,[a]}, permanent, infinity, supervisor, []},
    C2 = {child, {m,f,[a]}, permanent, 1000, supervisor, []},

    {error,{duplicate_child_name,child}} = supervisor:check_childspecs([C1,C2]),

    terminate(Pid, shutdown),

    %% Faulty child specs in supervisor start
    {error, {start_spec, {invalid_mfa, mfa}}} =
	 start_link({ok, {{one_for_one, 2, 3600}, [B1]}}),
    {error, {start_spec, {invalid_restart_type, prmanent}}} =
	start_link({ok, {{simple_one_for_one, 2, 3600}, [B2]}}),

    %% simple_one_for_one needs exactly one child
    {error,{bad_start_spec,[]}} =
	start_link({ok, {{simple_one_for_one, 2, 3600}, []}}),
    {error,{bad_start_spec,[C1,C2]}} =
	start_link({ok, {{simple_one_for_one, 2, 3600}, [C1,C2]}}),

    ok.

%%-------------------------------------------------------------------------
%% Tests child specs (map form), invalid formats should be rejected.
child_specs_map(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    {ok, Pid} = start_link({ok, {#{}, []}}),
    {error, _} = supervisor:start_child(sup_test, hej),

    CS0 = #{id => child, start => {m, f, [a]}},

    %% Bad child specs 
    B1 = #{},
    B2 = #{id => child},
    B3 = #{start => {m, f, [a]}},
    B4 = CS0#{start => mfa},
    B5 = CS0#{restart => prmanent}, 
    B6 = CS0#{shutdown => -10},
    B7 = CS0#{type => wrker},
    B8 = CS0#{modules => dy},
    B9 = CS0#{modules => [1,2,3]},
    B10 = CS0#{significant => 'maybe'},

    {error, missing_id} = supervisor:start_child(sup_test, B1),
    {error, missing_start} = supervisor:start_child(sup_test, B2),
    {error, missing_id} = supervisor:start_child(sup_test, B3),
    {error, {invalid_mfa,mfa}} = supervisor:start_child(sup_test, B4),
    {error, {invalid_restart_type, prmanent}} =
	supervisor:start_child(sup_test, B5),
    {error,  {invalid_shutdown,-10}}
	= supervisor:start_child(sup_test, B6),
    {error, {invalid_child_type,wrker}}
	= supervisor:start_child(sup_test, B7),
    {error, {invalid_modules,dy}}
	= supervisor:start_child(sup_test, B8),

    {error, {badarg, _}} = supervisor:check_childspecs(B1), % should be list
    {error, missing_id} = supervisor:check_childspecs([B1]),
    {error, missing_start} = supervisor:check_childspecs([B2]),
    {error, missing_id} = supervisor:check_childspecs([B3]),
    {error, {invalid_mfa,mfa}} = supervisor:check_childspecs([B4]),
    {error, {invalid_restart_type,prmanent}} =
	supervisor:check_childspecs([B5]),
    {error, {invalid_shutdown,-10}} = supervisor:check_childspecs([B6]),
    {error, {invalid_child_type,wrker}}
	= supervisor:check_childspecs([B7]),
    {error, {invalid_modules,dy}} = supervisor:check_childspecs([B8]),
    {error, {invalid_module, 1}} =
	supervisor:check_childspecs([B9]),
    {error, {invalid_significant, 'maybe'}} =
	supervisor:check_childspecs([B10]),

    CSFilter = fun (CS) -> maps:filter(fun (_, V) -> V =/= undefined end, CS) end,
    lists:foreach(
	fun
	    ({never, ChildSpec = #{significant := true}}) ->
		ChildSpec1 = CSFilter(ChildSpec),
		{error, {bad_combination, BadOpts}} =
		    supervisor:check_childspecs([ChildSpec1], never),
		[{auto_shutdown, never}, {significant, true}] = lists:sort(BadOpts);
	    ({AutoShutdown, ChildSpec = #{restart := undefined, significant := true}}) ->
		ChildSpec1 = CSFilter(ChildSpec),
		{error, {bad_combination, BadOpts}} =
		    supervisor:check_childspecs([ChildSpec1], AutoShutdown),
		[{restart, permanent}, {significant, true}] = lists:sort(BadOpts);
	    ({AutoShutdown, ChildSpec = #{restart := permanent, significant := true}}) ->
		ChildSpec1 = CSFilter(ChildSpec),
		{error, {bad_combination, BadOpts}} =
		    supervisor:check_childspecs([ChildSpec1], AutoShutdown),
		[{restart, permanent}, {significant, true}] = lists:sort(BadOpts);
	    ({AutoShutdown, ChildSpec}) ->
		ChildSpec1 = CSFilter(ChildSpec),
		ok = supervisor:check_childspecs([ChildSpec1], AutoShutdown)
	end,
	[
	    {AutoShutdown,
	     CS0#{restart => Restart,
		  shutdown => Shutdown,
		  type => Type,
		  modules => Modules,
		  significant => Significant,
		  dummy => Dummy}}
	    ||
		AutoShutdown <- [undefined, never, any_significant, all_significant],
		Restart <- [undefined, permanent, transient, temporary],
		Shutdown <- [undefined, 0, 1000, infinity, brutal_kill],
		Type <- [undefined, supervisor, worker],
		Modules <- [undefined, dynamic, [], [m], [m1, m2]],
		Significant <- [undefined, true, false],
		Dummy <- [undefined, dummy]
	]
    ),

    {error,{duplicate_child_name,child}} = supervisor:check_childspecs([CS0,CS0]),

    terminate(Pid, shutdown),

    %% Faulty child specs in supervisor start
    {error, {start_spec, {invalid_mfa, mfa}}} =
	 start_link({ok, {{one_for_one, 2, 3600}, [B4]}}),
    {error, {start_spec, {invalid_restart_type, prmanent}}} =
	start_link({ok, {{simple_one_for_one, 2, 3600}, [B5]}}),

    %% simple_one_for_one needs exactly one child
    {error,{bad_start_spec,[]}} =
	start_link({ok, {{simple_one_for_one, 2, 3600}, []}}),
    {error,{bad_start_spec,[CS0,CS0]}} =
	start_link({ok, {{simple_one_for_one, 2, 3600}, [CS0,CS0]}}),

    %% auto_shutdown => never should not accept significant children
    {error, {start_spec, {bad_combination, _}}} =
	start_link({ok, {#{auto_shutdown => never}, [CS0#{significant => true}]}}),

    ok.

%%-------------------------------------------------------------------------
%% Test error handling of supervisor flags
sup_flags(_Config) ->
    process_flag(trap_exit,true),
    {error,{supervisor_data,{invalid_strategy,_}}} =
	start_link({ok, {{none_for_one, 2, 3600}, []}}),
    {error,{supervisor_data,{invalid_strategy,_}}} =
	start_link({ok, {#{strategy=>none_for_one}, []}}),
    {error,{supervisor_data,{invalid_intensity,_}}} =
	start_link({ok, {{one_for_one, infinity, 3600}, []}}),
    {error,{supervisor_data,{invalid_intensity,_}}} =
	start_link({ok, {#{intensity=>infinity}, []}}),
    {error,{supervisor_data,{invalid_period,_}}} =
	start_link({ok, {{one_for_one, 2, 0}, []}}),
    {error,{supervisor_data,{invalid_period,_}}} =
	start_link({ok, {#{period=>0}, []}}),
    {error,{supervisor_data,{invalid_period,_}}} =
	start_link({ok, {{one_for_one, 2, infinity}, []}}),
    {error,{supervisor_data,{invalid_period,_}}} =
	start_link({ok, {#{period=>infinity}, []}}),
    {error,{supervisor_data,{invalid_auto_shutdown,_}}} =
	start_link({ok, {#{auto_shutdown=>sometimes}, []}}),

    %% SupFlags other than a map or a 3-tuple
    {error,{supervisor_data,{invalid_type,_}}} =
	start_link({ok, {{one_for_one, 2}, []}}),

    %% Unexpected flags are ignored
    {ok,Pid} = start_link({ok,{#{silly_flag=>true},[]}}),
    terminate(Pid,shutdown),

    ok.

%%-------------------------------------------------------------------------
external_start_no_progress_log(Config) when is_list(Config) ->
    ok = logger:add_handler(?MODULE, ?MODULE, #{test_case_pid => self()}),
    Filter = {fun logger_filters:domain/2,{log,sub,[otp,sasl]}},
    logger:add_handler_filter(?MODULE, filter_non_sasl, Filter),
    logger:set_module_level([supervisor], info),
    permanent_normal(Config),
    receive
        ok ->
            ok = logger:remove_handler(?MODULE);
        {fail, Msg} ->
            ok = logger:remove_handler(?MODULE),
            ct:fail({"unexpected progress report", Msg})
    end.

%%-------------------------------------------------------------------------
%% A permanent child should always be restarted.
permanent_normal(Config) when is_list(Config) ->
    {ok, SupPid} = start_link({ok, {{one_for_one, 2, 3600}, []}}),
    Child1 = {child1, {supervisor_1, start_child, []}, permanent, 1000,
	      worker, []},
    {ok, CPid1} = supervisor:start_child(sup_test, Child1),
    terminate(SupPid, CPid1, child1, normal),
    [{child1, Pid ,worker,[]}] = supervisor:which_children(sup_test),
    case is_pid(Pid) of
	true ->
	    ok;
	false ->
	    ct:fail({permanent_child_not_restarted, Child1})
    end,
    [1,1,0,1] = get_child_counts(sup_test).

%%-------------------------------------------------------------------------
%% A transient child should not be restarted if it exits with reason
%% normal.
transient_normal(Config) when is_list(Config) ->
    {ok, SupPid} = start_link({ok, {{one_for_one, 2, 3600}, []}}),
    Child1 = {child1, {supervisor_1, start_child, []}, transient, 1000,
	      worker, []},

    {ok, CPid1} = supervisor:start_child(sup_test, Child1),

    terminate(SupPid, CPid1, child1, normal),

    [{child1,undefined,worker,[]}] = supervisor:which_children(sup_test),
    [1,0,0,1] = get_child_counts(sup_test).

%%-------------------------------------------------------------------------
%% A temporary process should never be restarted.
temporary_normal(Config) when is_list(Config) ->
    {ok, SupPid} = start_link({ok, {{one_for_one, 2, 3600}, []}}),
    Child1 = {child1, {supervisor_1, start_child, []}, temporary, 1000,
	      worker, []},

    {ok, CPid1} = supervisor:start_child(sup_test, Child1),

    terminate(SupPid, CPid1, child1, normal),

    [] = supervisor:which_children(sup_test),
    [0,0,0,0] = get_child_counts(sup_test).

%%-------------------------------------------------------------------------
%% A permanent child should always be restarted.
permanent_shutdown(Config) when is_list(Config) ->
    {ok, SupPid} = start_link({ok, {{one_for_one, 2, 3600}, []}}),
    Child1 = {child1, {supervisor_1, start_child, []}, permanent, 1000,
	      worker, []},

    {ok, CPid1} = supervisor:start_child(sup_test, Child1),

    terminate(SupPid, CPid1, child1, shutdown),

    [{child1, CPid2 ,worker,[]}] = supervisor:which_children(sup_test),
    case is_pid(CPid2) of
	true ->
	    ok;
	false ->
	    ct:fail({permanent_child_not_restarted, Child1})
    end,
    [1,1,0,1] = get_child_counts(sup_test),

    terminate(SupPid, CPid2, child1, {shutdown, some_info}),

    [{child1, CPid3 ,worker,[]}] = supervisor:which_children(sup_test),
    case is_pid(CPid3) of
	true ->
	    ok;
	false ->
	    ct:fail({permanent_child_not_restarted, Child1})
    end,

    [1,1,0,1] = get_child_counts(sup_test).

%%-------------------------------------------------------------------------
%% A transient child should not be restarted if it exits with reason
%% shutdown or {shutdown,Term}.
transient_shutdown(Config) when is_list(Config) ->
    {ok, SupPid} = start_link({ok, {{one_for_one, 2, 3600}, []}}),
    Child1 = {child1, {supervisor_1, start_child, []}, transient, 1000,
	      worker, []},

    {ok, CPid1} = supervisor:start_child(sup_test, Child1),

    terminate(SupPid, CPid1, child1, shutdown),

    [{child1,undefined,worker,[]}] = supervisor:which_children(sup_test),
    [1,0,0,1] = get_child_counts(sup_test),

    {ok, CPid2} = supervisor:restart_child(sup_test, child1),

    terminate(SupPid, CPid2, child1, {shutdown, some_info}),

    [{child1,undefined,worker,[]}] = supervisor:which_children(sup_test),
    [1,0,0,1] = get_child_counts(sup_test).

%%-------------------------------------------------------------------------
%% A temporary process should never be restarted.
temporary_shutdown(Config) when is_list(Config) ->
    {ok, SupPid} = start_link({ok, {{one_for_one, 2, 3600}, []}}),
    Child1 = {child1, {supervisor_1, start_child, []}, temporary, 1000,
	      worker, []},

    {ok, CPid1} = supervisor:start_child(sup_test, Child1),

    terminate(SupPid, CPid1, child1, shutdown),

    [] = supervisor:which_children(sup_test),
    [0,0,0,0] = get_child_counts(sup_test),

    {ok, CPid2} = supervisor:start_child(sup_test, Child1),

    terminate(SupPid, CPid2, child1, {shutdown, some_info}),

    [] = supervisor:which_children(sup_test),
    [0,0,0,0] = get_child_counts(sup_test).

%%-------------------------------------------------------------------------
%% Faulty application should shutdown and pass on errors
faulty_application_shutdown(Config) when is_list(Config) ->

    %% Set some paths
    AppDir  = filename:join(proplists:get_value(data_dir, Config), "app_faulty"),
    EbinDir = filename:join(AppDir, "ebin"),

    %% Start faulty app
    code:add_patha(EbinDir),

    %% {error,
    %%  {{shutdown,
    %%    {failed_to_start_child,
    %% 	app_faulty,
    %% 	{undef,
    %% 	 [{an_undefined_module_with,an_undefined_function,[argument1,argument2],
    %% 	   []},
    %% 	  {app_faulty_server,init,1,
    %% 	   [{file,"app_faulty/src/app_faulty_server.erl"},{line,16}]},
    %% 	  {gen_server,init_it,6,
    %% 	   [{file,"gen_server.erl"},{line,304}]},
    %% 	  {proc_lib,init_p_do_apply,3,
    %% 	   [{file,"proc_lib.erl"},{line,227}]}]}}},
    %%   {app_faulty,start,[normal,[]]}}}

    {error, Error} = application:start(app_faulty),
    {{shutdown, {failed_to_start_child,app_faulty,{undef, CallStack}}},
     {app_faulty,start,_}} = Error,
    [{an_undefined_module_with,an_undefined_function,_,_}|_] = CallStack,
    ok = application:unload(app_faulty),
    ok.

%%-------------------------------------------------------------------------
%% A permanent child should always be restarted.
permanent_abnormal(Config) when is_list(Config) ->
    {ok, SupPid} = start_link({ok, {{one_for_one, 2, 3600}, []}}),
    Child1 = {child1, {supervisor_1, start_child, []}, permanent, 1000,
	      worker, []},

    {ok, CPid1} = supervisor:start_child(sup_test, Child1),
    terminate(SupPid, CPid1, child1, abnormal),

    [{child1, Pid ,worker,[]}] = supervisor:which_children(sup_test),
    case is_pid(Pid) of
	true ->
	    ok;
	false ->
	    ct:fail({permanent_child_not_restarted, Child1})
    end,
    [1,1,0,1] = get_child_counts(sup_test).

%%-------------------------------------------------------------------------
%% A transient child should be restarted if it exits with reason abnormal.
transient_abnormal(Config) when is_list(Config) ->
    {ok, SupPid} = start_link({ok, {{one_for_one, 2, 3600}, []}}),
    Child1 = {child1, {supervisor_1, start_child, []}, transient, 1000,
	      worker, []},

    {ok, CPid1} = supervisor:start_child(sup_test, Child1),
    terminate(SupPid, CPid1, child1, abnormal),

    [{child1, Pid ,worker,[]}] = supervisor:which_children(sup_test),
    case is_pid(Pid) of
	true ->
	    ok;
	false ->
	    ct:fail({transient_child_not_restarted, Child1})
    end,
    [1,1,0,1] = get_child_counts(sup_test).

%%-------------------------------------------------------------------------
%% A temporary process should never be restarted.
temporary_abnormal(Config) when is_list(Config) ->
    {ok, SupPid} = start_link({ok, {{one_for_one, 2, 3600}, []}}),
    Child1 = {child1, {supervisor_1, start_child, []}, temporary, 1000,
	      worker, []},

    {ok, CPid1} = supervisor:start_child(sup_test, Child1),
    terminate(SupPid, CPid1, child1, abnormal),

    [] = supervisor:which_children(sup_test),
    [0,0,0,0] = get_child_counts(sup_test).

%%-------------------------------------------------------------------------
%% A temporary process killed as part of a rest_for_one or one_for_all
%% restart strategy should not be restarted given its args are not
%% saved. Otherwise the supervisor hits its limit and crashes.
temporary_bystander(_Config) ->
    Child1 = {child1, {supervisor_1, start_child, []}, permanent, 100,
	      worker, []},
    Child2 = {child2, {supervisor_1, start_child, []}, temporary, 100,
	      worker, []},
    {ok, SupPid1} = supervisor:start_link(?MODULE, {ok, {{one_for_all, 2, 300}, []}}),
    {ok, SupPid2} = supervisor:start_link(?MODULE, {ok, {{rest_for_one, 2, 300}, []}}),
    unlink(SupPid1), % otherwise we crash with it
    unlink(SupPid2), % otherwise we crash with it
    {ok, CPid1} = supervisor:start_child(SupPid1, Child1),
    {ok, _CPid2} = supervisor:start_child(SupPid1, Child2),
    {ok, CPid3} = supervisor:start_child(SupPid2, Child1),
    {ok, _CPid4} = supervisor:start_child(SupPid2, Child2),
    terminate(SupPid1, CPid1, child1, normal),
    terminate(SupPid2, CPid3, child1, normal),
    timer:sleep(350),
    catch link(SupPid1),
    catch link(SupPid2),
    %% The supervisor would die attempting to restart child2
    true = erlang:is_process_alive(SupPid1),
    true = erlang:is_process_alive(SupPid2),
    %% Child2 has not been restarted
    [{child1, _, _, _}] = supervisor:which_children(SupPid1),
    [{child1, _, _, _}] = supervisor:which_children(SupPid2).

%%-------------------------------------------------------------------------
%% Test restarting a process multiple times, being careful not
%% to exceed the maximum restart frquency.
multiple_restarts(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    Child1 = #{id => child1,
	       start => {supervisor_1, start_child, []},
	       restart => permanent,
	       shutdown => brutal_kill,
	       type => worker,
	       modules => []},
    SupFlags = #{strategy => one_for_one,
		 intensity => 1,
		 period => 1},
    {ok, SupPid} = start_link({ok, {SupFlags, []}}),
    {ok, CPid1} = supervisor:start_child(sup_test, Child1),

    %% Terminate the process several times, but being careful
    %% not to exceed the maximum restart intensity.
    terminate(SupPid, CPid1, child1, abnormal),
    _ = [begin
	     receive after 2100 -> ok end,
	     [{_, Pid, _, _}|_] = supervisor:which_children(sup_test),
	     terminate(SupPid, Pid, child1, abnormal)
	 end || _ <- [1,2,3]],

    %% Verify that the supervisor is still alive and clean up.
    ok = supervisor:terminate_child(SupPid, child1),
    ok = supervisor:delete_child(SupPid, child1),
    exit(SupPid, kill),
    ok.


%%-------------------------------------------------------------------------
%% Test the one_for_one base case.
one_for_one(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    Child1 = {child1, {supervisor_1, start_child, []}, permanent, 1000,
	      worker, []},
    Child2 = {child2, {supervisor_1, start_child, []}, permanent, 1000,
	      worker, []},
    {ok, SupPid} = start_link({ok, {{one_for_one, 2, 3600}, []}}),
    {ok, CPid1} = supervisor:start_child(sup_test, Child1),
    {ok, CPid2} = supervisor:start_child(sup_test, Child2),

    terminate(SupPid, CPid1, child1, abnormal),
    Children = supervisor:which_children(sup_test),
    if length(Children) == 2 ->
	    case lists:keysearch(CPid2, 2, Children) of
		{value, _} -> ok;
		_ ->  ct:fail(bad_child)
	    end;
       true ->  ct:fail({bad_child_list, Children})
    end,
    [2,2,0,2] = get_child_counts(sup_test),

    %% Test restart frequency property
    terminate(SupPid, CPid2, child2, abnormal),

    [{Id4, Pid4, _, _}|_] = supervisor:which_children(sup_test),
    terminate(SupPid, Pid4, Id4, abnormal),
    check_exit([SupPid]).

%%-------------------------------------------------------------------------
%% Test restart escalation on a one_for_one supervisor.
one_for_one_escalation(Config) when is_list(Config) ->
    process_flag(trap_exit, true),

    Child1 = {child1, {supervisor_1, start_child, [error]},
	      permanent, 1000,
	      worker, []},
    Child2 = {child2, {supervisor_1, start_child, []}, permanent, 1000,
	      worker, []},

    {ok, SupPid} = start_link({ok, {{one_for_one, 4, 3600}, []}}),
    {ok, CPid1} = supervisor:start_child(sup_test, Child1),
    {ok, CPid2} = supervisor:start_child(sup_test, Child2),
    link(CPid2),

    terminate(SupPid, CPid1, child1, abnormal),
    check_exit([SupPid, CPid2]).


%%-------------------------------------------------------------------------
%% Test the one_for_all base case.
one_for_all(Config) when is_list(Config) ->
    process_flag(trap_exit, true),

    Child1 = {child1, {supervisor_1, start_child, []}, permanent, 1000,
	     worker, []},
    Child2 = {child2, {supervisor_1, start_child, []}, permanent, 1000,
	     worker, []},
    {ok, SupPid} = start_link({ok, {{one_for_all, 2, 3600}, []}}),
    {ok, CPid1} = supervisor:start_child(sup_test, Child1),
    {ok, CPid2} = supervisor:start_child(sup_test, Child2),
    link(CPid2),

    terminate(SupPid, CPid1, child1, abnormal),
    check_exit([CPid2]),

    Children = supervisor:which_children(sup_test),
    if length(Children) == 2 -> ok;
       true ->
	    ct:fail({bad_child_list, Children})
    end,

    %% Test that no old children is still alive
    not_in_child_list([CPid1, CPid2], lists:map(fun({_,P,_,_}) -> P end, Children)),

    [2,2,0,2] = get_child_counts(sup_test),

    %%% Test restart frequency property
    [{Id3, Pid3, _, _}|_] = supervisor:which_children(sup_test),
    terminate(SupPid, Pid3, Id3, abnormal),
    [{Id4, Pid4, _, _}|_] = supervisor:which_children(sup_test),
    terminate(SupPid, Pid4, Id4, abnormal),
    check_exit([SupPid]).


%%-------------------------------------------------------------------------
%% Test restart escalation on a one_for_all supervisor.
one_for_all_escalation(Config) when is_list(Config) ->
    process_flag(trap_exit, true),

    Child1 = {child1, {supervisor_1, start_child, []}, permanent, 1000,
	     worker, []},
    Child2 = {child2, {supervisor_1, start_child, [error]},
	      permanent, 1000,
	     worker, []},
    {ok, SupPid} = start_link({ok, {{one_for_all, 4, 3600}, []}}),
    {ok, CPid1} = supervisor:start_child(sup_test, Child1),
    {ok, CPid2} = supervisor:start_child(sup_test, Child2),
    link(CPid2),

    terminate(SupPid, CPid1, child1, abnormal),
    check_exit([CPid2, SupPid]).


%%-------------------------------------------------------------------------
%% Test that the supervisor terminates a restarted child when a different
%% child fails to start.
one_for_all_other_child_fails_restart(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    Self = self(),
    Child1 = {child1, {supervisor_3, start_child, [child1, Self]},
	      permanent, 1000, worker, []},
    Child2 = {child2, {supervisor_3, start_child, [child2, Self]},
	      permanent, 1000, worker, []},
    Children = [Child1, Child2],
    StarterFun = fun() ->
	    {ok, SupPid} = start_link({ok, {{one_for_all, 3, 3600}, Children}}),
	    Self ! {sup_pid, SupPid},
	    receive {stop, Self} -> ok end
    end,
    StarterPid = spawn_link(StarterFun),
    Ok = {{ok, undefined}, Self},
    %% Let the children start.
    Child1Pid = receive {child1, Pid1} -> Pid1 end,
    Child1Pid ! Ok,
    Child2Pid = receive {child2, Pid2} -> Pid2 end,
    Child2Pid ! Ok,
    %% Supervisor started.
    SupPid = receive {sup_pid, Pid} -> Pid end,
    link(SupPid),
    exit(Child1Pid, die),
    %% Let child1 restart but don't let child2.
    Child1Pid2  = receive {child1, Pid3} -> Pid3 end,
    Child1Pid2Ref = erlang:monitor(process, Child1Pid2),
    Child1Pid2 ! Ok,
    Child2Pid2 = receive {child2, Pid4} -> Pid4 end,
    Child2Pid2 ! {{stop, normal}, Self},
    %% Check child1 is terminated.
    receive
	{'DOWN', Child1Pid2Ref, _, _, shutdown} ->
	    ok;
	{_childName, _Pid} ->
	    exit(SupPid, kill),
	    check_exit([StarterPid, SupPid]),
	    ct:fail({restarting_child_not_terminated, Child1Pid2})
    end,
    %% Let the restart complete.
    Child1Pid3 = receive {child1, Pid5} -> Pid5 end,
    Child1Pid3 ! Ok,
    Child2Pid3 = receive {child2, Pid6} -> Pid6 end,
    Child2Pid3 ! Ok,
    StarterPid ! {stop, Self},
    check_exit([StarterPid, SupPid]).


%%-------------------------------------------------------------------------
%% Test the simple_one_for_one base case.
simple_one_for_one(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    Child = {child, {supervisor_1, start_child, []}, permanent, 1000,
	     worker, []},
    {ok, SupPid} = start_link({ok, {{simple_one_for_one, 2, 3600}, [Child]}}),
    {ok, CPid1} = supervisor:start_child(sup_test, []),
    {ok, CPid2} = supervisor:start_child(sup_test, []),

    terminate(SupPid, CPid1, child1, abnormal),

    Children = supervisor:which_children(sup_test),
    if length(Children) == 2 ->
	    case lists:keysearch(CPid2, 2, Children) of
		{value, _} -> ok;
		_ ->  ct:fail(bad_child)
	    end;
       true ->  ct:fail({bad_child_list, Children})
    end,
    [1,2,0,2] = get_child_counts(sup_test),

    %% Test restart frequency property
    terminate(SupPid, CPid2, child2, abnormal),

    [{Id4, Pid4, _, _}|_] = supervisor:which_children(sup_test),

    terminate(SupPid, Pid4, Id4, abnormal),
    check_exit_reason(SupPid,shutdown).


%%-------------------------------------------------------------------------
%% Test simple_one_for_one children restarts and returning ignore.
simple_one_for_one_restart_ignore(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    Self = self(),
    lists:foreach(
        fun(Restart) ->
            Child = {child, {supervisor_3, start_child, []}, Restart, 1000, worker, []},
            {ok, SupPid} = start_link({ok, {{simple_one_for_one, 10, 3600}, [Child]}}),
            StarterPid1 = spawn_link(fun() -> supervisor:start_child(SupPid, [child1, Self]) end),
            ChildPid1 = receive {child1, CPid1} -> CPid1 end,
            ChildPid1 ! {{ok, 0}, Self},
            check_exit([StarterPid1]),
            [{undefined, ChildPid1, _, _}] = supervisor:which_children(SupPid),
            terminate(ChildPid1, kill),
            ChildPid2 = receive {child1, CPid2} -> CPid2 end,
            ChildPid2 ! {{ok, 0}, Self},
            [{undefined, ChildPid2, _, _}] = supervisor:which_children(SupPid),
            terminate(ChildPid2, kill),
            ChildPid3 = receive {child1, CPid3} -> CPid3 end,
            ChildPid3 ! {ignore, Self},
            [] = supervisor:which_children(SupPid),
            terminate(SupPid, shutdown),
            ok = check_exit_reason(SupPid, shutdown)
        end,
        [transient, permanent]
    ).


%%-------------------------------------------------------------------------
%% Test simple_one_for_one children shutdown accordingly to the
%% supervisor's shutdown strategy.
simple_one_for_one_shutdown(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ShutdownTime = 1000,
    Child = {child, {supervisor_2, start_child, []},
             permanent, 2*ShutdownTime, worker, []},
    {ok, SupPid} = start_link({ok, {{simple_one_for_one, 2, 3600}, [Child]}}),

    %% Will be gracefully shutdown
    {ok, _CPid1} = supervisor:start_child(sup_test, [ShutdownTime]),
    {ok, _CPid2} = supervisor:start_child(sup_test, [ShutdownTime]),

    %% Will be killed after 2*ShutdownTime milliseconds
    {ok, _CPid3} = supervisor:start_child(sup_test, [5*ShutdownTime]),

    {T, ok} = timer:tc(fun terminate/2, [SupPid, shutdown]),
    if T < 1000*ShutdownTime ->
            %% Because supervisor's children wait before exiting, it can't
            %% terminate quickly
            ct:fail({shutdown_too_short, T});
       T >= 1000*5*ShutdownTime ->
            ct:fail({shutdown_too_long, T});
       true ->
            check_exit([SupPid])
    end.


%%-------------------------------------------------------------------------
%% Tests automatic restart of children who's start function return
%% extra info.
simple_one_for_one_extra(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    Child = {child, {supervisor_1, start_child, [extra_info]}, 
	     permanent, 1000, worker, []},
    {ok, SupPid} = start_link({ok, {{simple_one_for_one, 2, 3600}, [Child]}}),
    {ok, CPid1, extra_info} = supervisor:start_child(sup_test, []),
    {ok, CPid2, extra_info} = supervisor:start_child(sup_test, []),
    link(CPid2),
    terminate(SupPid, CPid1, child1, abnormal),
    Children = supervisor:which_children(sup_test),
    if length(Children) == 2 ->
	    case lists:keysearch(CPid2, 2, Children) of
		{value, _} -> ok;
		_ ->  ct:fail(bad_child)
	    end;
       true ->  ct:fail({bad_child_list, Children})
    end,
    [1,2,0,2] = get_child_counts(sup_test),
    terminate(SupPid, CPid2, child2, abnormal),
    [{Id4, Pid4, _, _}|_] = supervisor:which_children(sup_test),
    terminate(SupPid, Pid4, Id4, abnormal),
    check_exit([SupPid]).

%%-------------------------------------------------------------------------
%% Test for subtle corruption of internal state for a
%% simple_one_for_one supervisor. Thanks to Zeyu Zhang (@zzydxm) and
%% Maxim Fedorov for noticing this bug. (OTP-16804)
simple_one_for_one_corruption(Config) when is_list(Config) ->
    process_flag(trap_exit, true),

    logger:add_handler(?MODULE, ?MODULE, #{test_case_pid => self()}),

    try
        Child = #{id => child, start => {supervisor_1, start_child, []},
                  restart => temporary, shutdown => 1000,
                  type => worker, modules => []},
        {ok, SupPid} = start_link({ok, {{simple_one_for_one, 2, 3600}, [Child]}}),
        {ok, CPid1} = supervisor:start_child(sup_test, []),

        terminate(SupPid, CPid1, child1, abnormal),

        %% The first time a child of simple_one_for_one supervisor
        %% with restart strategy `temporary` dies, the internal state
        %% for the supervisor will become inconsistent (the `dynamics`
        %% field would change from `{mapsets,Map}` to
        %% `{maps,Map}`). That inconsistency will make the supervisor
        %% retain the start arguments even for temporary processes.
        %%
        %% To test that the bug is fixed, start a new child process
        %% with a large term in its argument list.

        N = 50000,
        BigData = binary_to_list(<<0:N/unit:8>>),
        {ok, CPid2, BigData} = supervisor:start_child(sup_test, [BigData]),

        %% Since the child is temporary, the supervisor should not keep
        %% the argument list for the child and the supervisor's heap
        %% size should shrink after a GC.

        true = erlang:garbage_collect(SupPid),
        {total_heap_size, HeapSize} = process_info(SupPid, total_heap_size),
        if
            HeapSize > 2*N ->
                %% The start arguments for the child have been kept.
                ct:fail({excessive_heap_size,HeapSize});
            true ->
                ok
        end,

        terminate(SupPid, CPid2, child2, abnormal),

        exit(SupPid, kill)
    after
        logger:remove_handler(?MODULE)
    end,

    ok.


%%-------------------------------------------------------------------------
%% Test restart escalation on a simple_one_for_one supervisor.
simple_one_for_one_escalation(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    Child = {child, {supervisor_1, start_child, []}, permanent, 1000,
	     worker, []},
    {ok, SupPid} = start_link({ok, {{simple_one_for_one, 4, 3600}, [Child]}}),
    {ok, CPid1} = supervisor:start_child(sup_test, [error]),
    link(CPid1),
    {ok, CPid2} = supervisor:start_child(sup_test, []),
    link(CPid2),

    terminate(SupPid, CPid1, child, abnormal),
    check_exit([SupPid, CPid2]).

%%-------------------------------------------------------------------------
%% Test the rest_for_one base case.
rest_for_one(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    Child1 = {child1, {supervisor_1, start_child, []}, permanent, 1000,
	     worker, []},
    Child2 = {child2, {supervisor_1, start_child, []}, permanent, 1000,
	     worker, []},
    Child3 = {child3, {supervisor_1, start_child, []}, permanent, 1000,
	     worker, []},
    {ok, SupPid} = start_link({ok, {{rest_for_one, 2, 3600}, []}}),
    {ok, CPid1} = supervisor:start_child(sup_test, Child1),
    link(CPid1),
    {ok, CPid2} = supervisor:start_child(sup_test, Child2),
    {ok, CPid3} = supervisor:start_child(sup_test, Child3),
    link(CPid3),
    [3,3,0,3] = get_child_counts(sup_test),

    terminate(SupPid, CPid2, child2, abnormal),

    %% Check that Cpid3 did die
    check_exit([CPid3]),

    Children = supervisor:which_children(sup_test),
    is_in_child_list([CPid1], Children),

    if length(Children) == 3 ->
	    ok;
       true ->
	    ct:fail({bad_child_list, Children})
    end,
    [3,3,0,3] = get_child_counts(sup_test),

    %% Test that no old children is still alive
    Pids = lists:map(fun({_,P,_,_}) -> P end, Children),
    not_in_child_list([CPid2, CPid3], Pids),
    in_child_list([CPid1], Pids),

    %% Test restart frequency property
    [{child3, Pid3, _, _}|_] = supervisor:which_children(sup_test),

    terminate(SupPid, Pid3, child3, abnormal),

    [_,{child2, Pid4, _, _}|_] = supervisor:which_children(sup_test),

    terminate(SupPid, Pid4, child2, abnormal),
    check_exit([SupPid]).

%%-------------------------------------------------------------------------
%% Test restart escalation on a rest_for_one supervisor.
rest_for_one_escalation(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    Child1 = {child1, {supervisor_1, start_child, []}, permanent, 1000,
	     worker, []},
    Child2 = {child2, {supervisor_1, start_child, [error]},
	      permanent, 1000,
	     worker, []},
    {ok, SupPid} = start_link({ok, {{rest_for_one, 4, 3600}, []}}),
    {ok, CPid1} = supervisor:start_child(sup_test, Child1),
    {ok, CPid2} = supervisor:start_child(sup_test, Child2),
    link(CPid2),

    terminate(SupPid, CPid1, child1, abnormal),
    check_exit([CPid2, SupPid]).


%%-------------------------------------------------------------------------
%% Test that the supervisor terminates a restarted child when a different
%% child fails to start.
rest_for_one_other_child_fails_restart(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    Self = self(),
    Child1 = {child1, {supervisor_3, start_child, [child1, Self]},
	      permanent, 1000, worker, []},
    Child2 = {child2, {supervisor_3, start_child, [child2, Self]},
	      permanent, 1000, worker, []},
    Children = [Child1, Child2],
    StarterFun = fun() ->
	    {ok, SupPid} = start_link({ok, {{rest_for_one, 3, 3600}, Children}}),
	    Self ! {sup_pid, SupPid},
	    receive {stop, Self} -> ok end
    end,
    StarterPid = spawn_link(StarterFun),
    Ok = {{ok, undefined}, Self},
    %% Let the children start.
    Child1Pid = receive {child1, Pid1} -> Pid1 end,
    Child1Pid ! Ok,
    Child2Pid = receive {child2, Pid2} -> Pid2 end,
    Child2Pid ! Ok,
    %% Supervisor started.
    SupPid = receive {sup_pid, Pid} -> Pid end,
    link(SupPid),
    exit(Child1Pid, die),
    %% Let child1 restart but don't let child2.
    Child1Pid2  = receive {child1, Pid3} -> Pid3 end,
    Child1Pid2 ! Ok,
    Child2Pid2 = receive {child2, Pid4} -> Pid4 end,
    Child2Pid2 ! {{stop, normal}, Self},
	%% Let child2 restart.
    receive
	{child2, Child2Pid3} ->
	    Child2Pid3 ! Ok;
	{child1, _Child1Pid3} ->
	    exit(SupPid, kill),
	    check_exit([StarterPid, SupPid]),
	    ct:fail({restarting_started_child, Child1Pid2})
    end,
    StarterPid ! {stop, Self},
    check_exit([StarterPid, SupPid]).


%%-------------------------------------------------------------------------
%% Test that the supervisor does not hang forever if the child unliks
%% and then is terminated by the supervisor.
child_unlink(Config) when is_list(Config) ->

    {ok, SupPid} = start_link({ok, {{one_for_one, 2, 3600}, []}}),

    Child = {naughty_child, {naughty_child, 
			     start_link, [SupPid]}, permanent, 
	     1000, worker, [supervisor_SUITE]},

    {ok, _ChildPid} = supervisor:start_child(sup_test, Child),

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
	    ct:fail(supervisor_hangs)
    end.
%%-------------------------------------------------------------------------
%% Test a basic supervison tree.
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
    ChildSup3 = {supchild3,
		 {supervisor, start_link,
		  [?MODULE, {ok, {{one_for_one, 4, 3600}, []}}]},
		 transient, infinity,
		 supervisor, []},

    %% Top supervisor
    {ok, SupPid} = start_link({ok, {{one_for_all, 4, 3600}, []}}),

    %% Child supervisors  
    {ok, Sup1} = supervisor:start_child(SupPid, ChildSup1),
    {ok, Sup2} = supervisor:start_child(SupPid, ChildSup2),
    {ok, _Sup3} = supervisor:start_child(SupPid, ChildSup3),
    ok = supervisor:terminate_child(SupPid, supchild3),
    [3,2,3,0] = get_child_counts(SupPid),

    %% Workers
    [{_, CPid2, _, _},{_, CPid1, _, _}] =
	supervisor:which_children(Sup1),
    [2,2,0,2] = get_child_counts(Sup1),
    [0,0,0,0] = get_child_counts(Sup2),

    %% Dynamic children
    {ok, CPid3} = supervisor:start_child(Sup2, Child3),
    {ok, CPid4} = supervisor:start_child(Sup2, Child4),
    [2,2,0,2] = get_child_counts(Sup1),
    [2,2,0,2] = get_child_counts(Sup2),

    %% Test that the only the process that dies is restarted
    terminate(Sup2, CPid4, child4, abnormal),

    [{_, CPid2, _, _},{_, CPid1, _, _}] =
	supervisor:which_children(Sup1),
    [2,2,0,2] = get_child_counts(Sup1),

    [{_, NewCPid4, _, _},{_, CPid3, _, _}] =
	supervisor:which_children(Sup2),
    [2,2,0,2] = get_child_counts(Sup2),

    false = NewCPid4 == CPid4,

    %% Test that supervisor tree is restarted, but not dynamic children.
    terminate(Sup2, CPid3, child3, abnormal),

    timer:sleep(1000),

    [{supchild3, NewSup3, _, _},
     {supchild2, NewSup2, _, _},
     {supchild1, NewSup1, _, _}] =
	supervisor:which_children(SupPid),
    [3,3,3,0] = get_child_counts(SupPid),

    [{child2, _, _, _},{child1, _, _, _}]  =
	supervisor:which_children(NewSup1),
    [2,2,0,2] = get_child_counts(NewSup1),

    [] = supervisor:which_children(NewSup2),
    [0,0,0,0] = get_child_counts(NewSup2),

    [] = supervisor:which_children(NewSup3),
    [0,0,0,0] = get_child_counts(NewSup3).

%%-------------------------------------------------------------------------
%% Test count_children
count_children(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    Child = {child, {supervisor_1, start_child, []}, temporary, 1000,
	     worker, []},
    {ok, SupPid} = start_link({ok, {{simple_one_for_one, 2, 3600}, [Child]}}),
    [supervisor:start_child(sup_test, []) || _Ignore <- lists:seq(1,1000)],

    Children = supervisor:which_children(sup_test),
    ChildCount = get_child_counts(sup_test),

    [supervisor:start_child(sup_test, []) || _Ignore2 <- lists:seq(1,1000)],

    ChildCount2 = get_child_counts(sup_test),
    Children2 = supervisor:which_children(sup_test),

    ChildCount3 = get_child_counts(sup_test),
    Children3 = supervisor:which_children(sup_test),

    1000 = length(Children),
    [1,1000,0,1000] = ChildCount,
    2000 = length(Children2),
    [1,2000,0,2000] = ChildCount2,
    Children3 = Children2,
    ChildCount3 = ChildCount2,

    [terminate(SupPid, Pid, child, kill) || {undefined, Pid, worker, _Modules} <- Children3],
    [1,0,0,0] = get_child_counts(sup_test).

%%-------------------------------------------------------------------------
%% Test count_children for simple_one_for_one, when children are supervisors
count_children_supervisor(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    Child = {child, {supervisor_1, start_child, []}, temporary, infinity,
	     supervisor, []},
    {ok, SupPid} = start_link({ok, {{simple_one_for_one, 2, 3600}, [Child]}}),
    [supervisor:start_child(sup_test, []) || _Ignore <- lists:seq(1,1000)],

    Children = supervisor:which_children(sup_test),
    ChildCount = get_child_counts(sup_test),

    [supervisor:start_child(sup_test, []) || _Ignore2 <- lists:seq(1,1000)],

    ChildCount2 = get_child_counts(sup_test),
    Children2 = supervisor:which_children(sup_test),

    ChildCount3 = get_child_counts(sup_test),
    Children3 = supervisor:which_children(sup_test),

    1000 = length(Children),
    [1,1000,1000,0] = ChildCount,
    2000 = length(Children2),
    [1,2000,2000,0] = ChildCount2,
    Children3 = Children2,
    ChildCount3 = ChildCount2,

    [terminate(SupPid, Pid, child, kill) || {undefined, Pid, supervisor, _Modules} <- Children3],
    [1,0,0,0] = get_child_counts(sup_test).

%%-------------------------------------------------------------------------
%% Test count_children when some children are restarting
count_restarting_children(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    Child = {child, {supervisor_deadlock, start_child_noreg, []},
	     permanent, brutal_kill, worker, []},
    %% 2 sek delay on failing restart (see supervisor_deadlock.erl) ->
    %% MaxR=20, MaxT=10 should ensure a restart loop when starting and
    %% restarting 3 instances of the child (as below)
    {ok, SupPid} = start_link({ok, {{simple_one_for_one, 20, 10}, [Child]}}),

    %% Ets table with state read by supervisor_deadlock.erl
    ets:new(supervisor_deadlock,[set,named_table,public]),
    ets:insert(supervisor_deadlock,{fail_start,false}),

    [1,0,0,0] = get_child_counts(SupPid),
    {ok, Ch1_1} = supervisor:start_child(SupPid, []),
    [1,1,0,1] = get_child_counts(SupPid),
    {ok, Ch1_2} = supervisor:start_child(SupPid, []),
    [1,2,0,2] = get_child_counts(SupPid),
    {ok, Ch1_3} = supervisor:start_child(SupPid, []),
    [1,3,0,3] = get_child_counts(SupPid),

    supervisor_deadlock:restart_child(Ch1_1),
    supervisor_deadlock:restart_child(Ch1_2),
    supervisor_deadlock:restart_child(Ch1_3),
    ct:sleep(400),
    [1,3,0,3] = get_child_counts(SupPid),
    [Ch2_1, Ch2_2, Ch2_3] = [C || {_,C,_,_} <- supervisor:which_children(SupPid)],

    ets:insert(supervisor_deadlock,{fail_start,true}),
    supervisor_deadlock:restart_child(Ch2_1),
    supervisor_deadlock:restart_child(Ch2_2),
    ct:sleep(4000),	   % allow restart to happen before proceeding
    [1,1,0,3] = get_child_counts(SupPid),

    ets:insert(supervisor_deadlock,{fail_start,false}),
    ct:sleep(4000),	   % allow restart to happen before proceeding
    [1,3,0,3] = get_child_counts(SupPid),

    ok = supervisor:terminate_child(SupPid, Ch2_3),
    [1,2,0,2] = get_child_counts(SupPid),
    [Ch3_1, Ch3_2] = [C || {_,C,_,_} <- supervisor:which_children(SupPid)],
    ok = supervisor:terminate_child(SupPid, Ch3_1),
    [1,1,0,1] = get_child_counts(SupPid),
    ok = supervisor:terminate_child(SupPid, Ch3_2),
    [1,0,0,0] = get_child_counts(SupPid).

%%-------------------------------------------------------------------------
%% Test get_callback_module
get_callback_module(Config) when is_list(Config) ->
    Child = {child, {supervisor_1, start_child, []}, temporary, 1000,
	     worker, []},
    {ok, SupPid} = start_link({ok, {{simple_one_for_one, 2, 3600}, [Child]}}),
    supervisor_SUITE = supervisor:get_callback_module(SupPid).

%%-------------------------------------------------------------------------
%% Temporary children shall not be restarted so they should not save
%% start parameters, as it potentially can take up a huge amount of
%% memory for no purpose.
do_not_save_start_parameters_for_temporary_children(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    dont_save_start_parameters_for_temporary_children(one_for_all),
    dont_save_start_parameters_for_temporary_children(one_for_one),
    dont_save_start_parameters_for_temporary_children(rest_for_one),
    dont_save_start_parameters_for_temporary_children(simple_one_for_one).

start_children(_,_, 0) ->
    ok;
start_children(Sup, Args, N) ->
    Spec = child_spec(Args, N),
    {ok, _, _} = supervisor:start_child(Sup, Spec),
    start_children(Sup, Args, N-1).

child_spec([_|_] = SimpleOneForOneArgs, _) ->
    SimpleOneForOneArgs;
child_spec({Name, MFA, RestartType, Shutdown, Type, Modules}, N) ->
    NewName = list_to_atom((atom_to_list(Name) ++ integer_to_list(N))),
    {NewName, MFA, RestartType, Shutdown, Type, Modules}.

%%-------------------------------------------------------------------------
%% Temporary children shall not be restarted so supervisors should not
%% save their spec when they terminate.
do_not_save_child_specs_for_temporary_children(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    dont_save_child_specs_for_temporary_children(one_for_all, kill),
    dont_save_child_specs_for_temporary_children(one_for_one, kill),
    dont_save_child_specs_for_temporary_children(rest_for_one, kill),

    dont_save_child_specs_for_temporary_children(one_for_all, normal),
    dont_save_child_specs_for_temporary_children(one_for_one, normal),
    dont_save_child_specs_for_temporary_children(rest_for_one, normal),

    dont_save_child_specs_for_temporary_children(one_for_all, abnormal),
    dont_save_child_specs_for_temporary_children(one_for_one, abnormal),
    dont_save_child_specs_for_temporary_children(rest_for_one, abnormal),

    dont_save_child_specs_for_temporary_children(one_for_all, supervisor),
    dont_save_child_specs_for_temporary_children(one_for_one, supervisor),
    dont_save_child_specs_for_temporary_children(rest_for_one, supervisor).

%%-------------------------------------------------------------------------
dont_save_start_parameters_for_temporary_children(simple_one_for_one = Type) ->
    Permanent = {child, {supervisor_1, start_child, []},
		 permanent, 1000, worker, []},
    Transient = {child, {supervisor_1, start_child, []},
		 transient, 1000, worker, []},
    Temporary = {child, {supervisor_1, start_child, []},
		 temporary, 1000, worker, []},
    {ok, Sup1} = supervisor:start_link(?MODULE, {ok, {{Type, 2, 3600}, [Permanent]}}),
    {ok, Sup2} = supervisor:start_link(?MODULE, {ok, {{Type, 2, 3600}, [Transient]}}),
    {ok, Sup3} = supervisor:start_link(?MODULE, {ok, {{Type, 2, 3600}, [Temporary]}}),

    LargeList = lists:duplicate(10, "Potentially large"),

    start_children(Sup1, [LargeList], 100),
    start_children(Sup2, [LargeList], 100),
    start_children(Sup3, [LargeList], 100),

    Size1 = erts_debug:flat_size(sys:get_status(Sup1)),
    Size2 = erts_debug:flat_size(sys:get_status(Sup2)),
    Size3 = erts_debug:flat_size(sys:get_status(Sup3)),

    true = (Size3 < Size1)  and  (Size3 < Size2),

    terminate(Sup1, shutdown),
    terminate(Sup2, shutdown),
    terminate(Sup3, shutdown);

dont_save_start_parameters_for_temporary_children(Type) ->
    {ok, Sup1} = supervisor:start_link(?MODULE, {ok, {{Type, 2, 3600}, []}}),
    {ok, Sup2} = supervisor:start_link(?MODULE, {ok, {{Type, 2, 3600}, []}}),
    {ok, Sup3} = supervisor:start_link(?MODULE, {ok, {{Type, 2, 3600}, []}}),

    LargeList = lists:duplicate(10, "Potentially large"),

    Permanent = {child1, {supervisor_1, start_child, [LargeList]},
		 permanent, 1000, worker, []},
    Transient = {child2, {supervisor_1, start_child, [LargeList]},
		 transient, 1000, worker, []},
    Temporary = {child3, {supervisor_1, start_child, [LargeList]},
		 temporary, 1000, worker, []},

    start_children(Sup1, Permanent, 100),
    start_children(Sup2, Transient, 100),
    start_children(Sup3, Temporary, 100),

    Size1 = erts_debug:flat_size(sys:get_status(Sup1)),
    Size2 = erts_debug:flat_size(sys:get_status(Sup2)),
    Size3 = erts_debug:flat_size(sys:get_status(Sup3)),

    true = (Size3 < Size1)  and  (Size3 < Size2),

    terminate(Sup1, shutdown),
    terminate(Sup2, shutdown),
    terminate(Sup3, shutdown).

dont_save_child_specs_for_temporary_children(Type, TerminateHow)->
    {ok, Sup} =
	supervisor:start_link(?MODULE, {ok, {{Type, 2, 3600}, []}}),

    Permanent = {child1, {supervisor_1, start_child, []},
		 permanent, 1000, worker, []},
    Transient = {child2, {supervisor_1, start_child, []},
		 transient, 1000, worker, []},
    Temporary = {child3, {supervisor_1, start_child, []},
		 temporary, 1000, worker, []},

    permanent_child_spec_saved(Permanent, Sup, TerminateHow),

    transient_child_spec_saved(Transient, Sup, TerminateHow),

    temporary_child_spec_not_saved(Temporary, Sup, TerminateHow),

    terminate(Sup, shutdown).

permanent_child_spec_saved(ChildSpec, Sup, supervisor = TerminateHow) ->
    already_present(Sup, ChildSpec, TerminateHow);

permanent_child_spec_saved(ChildSpec, Sup, TerminateHow) ->
    restarted(Sup, ChildSpec, TerminateHow).

transient_child_spec_saved(ChildSpec, Sup, supervisor = TerminateHow) ->
    already_present(Sup, ChildSpec, TerminateHow);

transient_child_spec_saved(ChildSpec, Sup, normal = TerminateHow) ->
    already_present(Sup, ChildSpec, TerminateHow);

transient_child_spec_saved(ChildSpec, Sup, TerminateHow) ->
    restarted(Sup, ChildSpec, TerminateHow).

temporary_child_spec_not_saved({Id, _,_,_,_,_} = ChildSpec, Sup, TerminateHow) ->
    {ok, Pid} = supervisor:start_child(Sup, ChildSpec),
    terminate(Sup, Pid, Id, TerminateHow),
    {ok, _} = supervisor:start_child(Sup, ChildSpec).

already_present(Sup, {Id,_,_,_,_,_} = ChildSpec, TerminateHow) ->
    {ok, Pid} = supervisor:start_child(Sup, ChildSpec),
    terminate(Sup, Pid, Id, TerminateHow),
    {error, already_present} = supervisor:start_child(Sup, ChildSpec),
    {ok, _} = supervisor:restart_child(Sup, Id).

restarted(Sup, {Id,_,_,_,_,_} = ChildSpec, TerminateHow) ->
    {ok, Pid} = supervisor:start_child(Sup, ChildSpec),
    terminate(Sup, Pid, Id, TerminateHow),
    %% Permanent processes will be restarted by the supervisor
    %% when not terminated by api
    {error, {already_started, _}} = supervisor:start_child(Sup, ChildSpec).


%%-------------------------------------------------------------------------
%% OTP-9242: Pids for dynamic temporary children were saved as a list,
%% which caused bad scaling when adding/deleting many processes.
simple_one_for_one_scale_many_temporary_children(_Config) ->
    process_flag(trap_exit, true),
    Child = {child, {supervisor_1, start_child, []}, temporary, 1000,
	     worker, []},
    {ok, _SupPid} = start_link({ok, {{simple_one_for_one, 2, 3600}, [Child]}}),

    C1 = [begin 
		 {ok,P} = supervisor:start_child(sup_test,[]), 
		 P
	     end || _<- lists:seq(1,1000)],
    {T1,done} = timer:tc(?MODULE,terminate_all_children,[C1]),
    
    C2 = [begin 
		 {ok,P} = supervisor:start_child(sup_test,[]), 
		 P
	     end || _<- lists:seq(1,10000)],
    {T2,done} = timer:tc(?MODULE,terminate_all_children,[C2]),
    
    if T1 > 0 ->
	    Scaling = T2 div T1,
	    if Scaling > 50 ->
		    %% The scaling shoul be linear (i.e.10, really), but we
		    %% give some extra here to avoid failing the test
		    %% unecessarily.
		    ct:fail({bad_scaling,Scaling});
	       true ->
		    ok
	    end;
       true ->
	    %% Means T2 div T1 -> infinity
	    ok
    end.
    

terminate_all_children([C|Cs]) ->
    ok = supervisor:terminate_child(sup_test,C),
    terminate_all_children(Cs);
terminate_all_children([]) ->
    done.


%%-------------------------------------------------------------------------
%% OTP-9212. Restart of global supervisor.
simple_global_supervisor(_Config) ->
    kill_supervisor(),
    kill_worker(),
    exit_worker(),
    restart_worker(),
    ok.

kill_supervisor() ->
    {Top, Sup2_1, Server_1} = start9212(),

    %% Killing a supervisor isn't really supported, but try it anyway...
    exit(Sup2_1, kill),
    timer:sleep(200),
    Sup2_2 = global:whereis_name(sup2),
    Server_2 = global:whereis_name(server),
    true = is_pid(Sup2_2),
    true = is_pid(Server_2),
    true = Sup2_1 =/= Sup2_2,
    true = Server_1 =/= Server_2,

    stop9212(Top).

handle_info({fail, With, After}, _State) ->
    timer:sleep(After),
    erlang:error(With).

kill_worker() ->
    {Top, _Sup2, Server_1} = start9212(),
    exit(Server_1, kill),
    timer:sleep(200),
    Server_2 = global:whereis_name(server),
    true = is_pid(Server_2),
    true = Server_1 =/= Server_2,
    stop9212(Top).

exit_worker() ->
    %% Very much the same as kill_worker().
    {Top, _Sup2, Server_1} = start9212(),
    Server_1 ! {fail, normal, 0},
    timer:sleep(200),
    Server_2 = global:whereis_name(server),
    true = is_pid(Server_2),
    true = Server_1 =/= Server_2,
    stop9212(Top).

restart_worker() ->
    {Top, _Sup2, Server_1} = start9212(),
    ok = supervisor:terminate_child({global, sup2}, child),
    {ok, _Child} = supervisor:restart_child({global, sup2}, child),
    Server_2 = global:whereis_name(server),
    true = is_pid(Server_2),
    true = Server_1 =/= Server_2,
    stop9212(Top).

start9212() ->
    Middle = {middle,{?MODULE,middle9212,[]}, permanent,2000,supervisor,[]},
    InitResult = {ok, {{one_for_all,3,60}, [Middle]}},
    {ok, TopPid} = start_link(InitResult),

    Sup2 = global:whereis_name(sup2),
    Server = global:whereis_name(server),
    true = is_pid(Sup2),
    true = is_pid(Server),
    {TopPid, Sup2, Server}.

stop9212(Top) ->
    Old = process_flag(trap_exit, true),
    exit(Top, kill),
    timer:sleep(200),
    undefined = global:whereis_name(sup2),
    undefined = global:whereis_name(server),
    check_exit([Top]),
    _ = process_flag(trap_exit, Old),
    ok.

middle9212() ->
    Child = {child, {?MODULE,gen_server9212,[]},permanent, 2000, worker, []},
    InitResult = {ok, {{one_for_all,3,60}, [Child]}},
    supervisor:start_link({global,sup2}, ?MODULE, InitResult).

gen_server9212() ->
    InitResult = {ok, []},
    gen_server:start_link({global,server}, ?MODULE, InitResult, []).


%%-------------------------------------------------------------------------
%% Test that child and supervisor can be shutdown while hanging in restart loop.
%% See OTP-9549.
hanging_restart_loop(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    {ok, Pid} = start_link({ok, {{one_for_one, 8, 10}, []}}),
    Child1 = {child1, {supervisor_deadlock, start_child, []},
	      permanent, brutal_kill, worker, []},

    %% Ets table with state read by supervisor_deadlock.erl
    ets:new(supervisor_deadlock,[set,named_table,public]),
    ets:insert(supervisor_deadlock,{fail_start,false}),

    {ok, CPid1} = supervisor:start_child(sup_test, Child1),
    link(CPid1),

    ets:insert(supervisor_deadlock,{fail_start,true}),
    supervisor_deadlock:restart_child(),
    timer:sleep(2000), % allow restart to happen before proceeding

    {error, already_present} = supervisor:start_child(sup_test, Child1),
    {error, restarting} = supervisor:restart_child(sup_test, child1),
    {error, restarting} = supervisor:delete_child(sup_test, child1),
    [{child1,restarting,worker,[]}] = supervisor:which_children(sup_test),
    [1,0,0,1] = get_child_counts(sup_test),

    ok = supervisor:terminate_child(sup_test, child1),
    check_exit_reason(CPid1, error),
    [{child1,undefined,worker,[]}] = supervisor:which_children(sup_test),

    ets:insert(supervisor_deadlock,{fail_start,false}),
    {ok, CPid2} = supervisor:restart_child(sup_test, child1),
    link(CPid2),

    ets:insert(supervisor_deadlock,{fail_start,true}),
    supervisor_deadlock:restart_child(),
    timer:sleep(2000), % allow restart to happen before proceeding

    %% Terminating supervisor.
    %% OTP-9549 fixes so this does not give a timetrap timeout -
    %% i.e. that supervisor does not hang in restart loop.
    terminate(Pid,shutdown),

    %% Check that child died with reason from 'restart' request above
    check_exit_reason(CPid2, error),
    undefined = whereis(sup_test),
    ok.

hanging_restart_loop_rest_for_one(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    {ok, Pid} = start_link({ok, {{rest_for_one, 8, 10}, []}}),
    Child1 = {child1, {supervisor_1, start_child, []},
	      permanent, brutal_kill, worker, []},
    Child2 = {child2, {supervisor_deadlock, start_child, []},
	      permanent, brutal_kill, worker, []},
    Child3 = {child3, {supervisor_1, start_child, []},
	      permanent, brutal_kill, worker, []},

    %% Ets table with state read by supervisor_deadlock.erl
    ets:new(supervisor_deadlock,[set,named_table,public]),
    ets:insert(supervisor_deadlock,{fail_start,false}),

    {ok, CPid1} = supervisor:start_child(sup_test, Child1),
    {ok, CPid2} = supervisor:start_child(sup_test, Child2),
    link(CPid2),
    {ok, _CPid3} = supervisor:start_child(sup_test, Child3),

    ets:insert(supervisor_deadlock,{fail_start,true}),
    supervisor_deadlock:restart_child(),
    timer:sleep(2000), % allow restart to happen before proceeding

    {error, already_present} = supervisor:start_child(sup_test, Child2),
    {error, restarting} = supervisor:restart_child(sup_test, child2),
    {error, restarting} = supervisor:delete_child(sup_test, child2),
    [{child3,undefined,worker,[]},
     {child2,restarting,worker,[]},
     {child1,CPid1,worker,[]}] = supervisor:which_children(sup_test),
    [3,1,0,3] = get_child_counts(sup_test),

    ok = supervisor:terminate_child(sup_test, child2),
    check_exit_reason(CPid2, error),
    [{child3,undefined,worker,[]},
     {child2,undefined,worker,[]},
     {child1,CPid1,worker,[]}] = supervisor:which_children(sup_test),

    ets:insert(supervisor_deadlock,{fail_start,false}),
    {ok, CPid22} = supervisor:restart_child(sup_test, child2),
    link(CPid22),

    ets:insert(supervisor_deadlock,{fail_start,true}),
    supervisor_deadlock:restart_child(),
    timer:sleep(2000), % allow restart to happen before proceeding

    %% Terminating supervisor.
    %% OTP-9549 fixes so this does not give a timetrap timeout -
    %% i.e. that supervisor does not hang in restart loop.
    terminate(Pid,shutdown),

    %% Check that child died with reason from 'restart' request above
    check_exit_reason(CPid22, error),
    undefined = whereis(sup_test),
    ok.

%%-------------------------------------------------------------------------
%% Test that child and supervisor can be shutdown while hanging in
%% restart loop, simple_one_for_one.
%% See OTP-9549.
hanging_restart_loop_simple(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    Child1 = {child1, {supervisor_deadlock, start_child, []},
	      permanent, brutal_kill, worker, []},
    {ok, Pid} = start_link({ok, {{simple_one_for_one, 8, 10}, [Child1]}}),

    %% Ets table with state read by supervisor_deadlock.erl
    ets:new(supervisor_deadlock,[set,named_table,public]),
    ets:insert(supervisor_deadlock,{fail_start,false}),

    {ok, CPid1} = supervisor:start_child(sup_test, []),
    link(CPid1),

    ets:insert(supervisor_deadlock,{fail_start,true}),
    supervisor_deadlock:restart_child(),
    timer:sleep(2000), % allow restart to happen before proceeding

    {error, simple_one_for_one} = supervisor:restart_child(sup_test, child1),
    {error, simple_one_for_one} = supervisor:delete_child(sup_test, child1),
    [{undefined,restarting,worker,[]}] = supervisor:which_children(sup_test),
    [1,0,0,1] = get_child_counts(sup_test),

    ok = supervisor:terminate_child(sup_test, CPid1),
    check_exit_reason(CPid1, error),
    [] = supervisor:which_children(sup_test),

    ets:insert(supervisor_deadlock,{fail_start,false}),
    {ok, CPid2} = supervisor:start_child(sup_test, []),
    link(CPid2),

    ets:insert(supervisor_deadlock,{fail_start,true}),
    supervisor_deadlock:restart_child(),
    timer:sleep(2000), % allow restart to happen before proceeding

    %% Terminating supervisor.
    %% OTP-9549 fixes so this does not give a timetrap timeout -
    %% i.e. that supervisor does not hang in restart loop.
    terminate(Pid,shutdown),

    %% Check that child died with reason from 'restart' request above
    check_exit_reason(CPid2, error),
    undefined = whereis(sup_test),
    ok.

%%-------------------------------------------------------------------------
%% Test the code_change function
code_change(_Config) ->
    process_flag(trap_exit, true),

    SupFlags = {one_for_one, 0, 1},
    {ok, Pid} = start_link({ok, {SupFlags, []}}),
    [] = supervisor:which_children(Pid),

    %% Change supervisor flags
    S1 = sys:get_state(Pid),
    ok = fake_upgrade(Pid,{ok, {{one_for_one, 1, 3}, []}}),
    S2 = sys:get_state(Pid),
    true = (S1 /= S2),

    %% Faulty childspec
    FaultyChild = {child1, permanent, brutal_kill, worker, []}, % missing start
    {error,{error,{invalid_child_spec,FaultyChild}}} =
	fake_upgrade(Pid,{ok,{SupFlags,[FaultyChild]}}),

    %% Add child1 and child2
    Child1 = {child1, {supervisor_1, start_child, []},
	      permanent, 2000, worker, []},
    Child2 = {child2, {supervisor_1, start_child, []},
	      permanent, brutal_kill, worker, []},
    ok = fake_upgrade(Pid,{ok,{SupFlags,[Child1,Child2]}}),
    %% Children are not automatically started
    {ok,_} = supervisor:restart_child(Pid,child1),
    {ok,_} = supervisor:restart_child(Pid,child2),
    [{child2,_,_,_},{child1,_,_,_}] = supervisor:which_children(Pid),

    %% Change child1, remove child2 and add child3
    Child11 = {child1, {supervisor_1, start_child, []},
	      permanent, 1000, worker, []},
    Child3 = {child3, {supervisor_1, start_child, []},
	      permanent, brutal_kill, worker, []},
    ok = fake_upgrade(Pid,{ok, {SupFlags, [Child11,Child3]}}),
    %% Children are not deleted on upgrade, so it is ok that child2 is
    %% still here
    [{child2,_,_,_},{child3,_,_,_},{child1,_,_,_}] =
	supervisor:which_children(Pid),

    %% Ignore during upgrade
    ok = fake_upgrade(Pid,ignore),

    %% Error during upgrade
    {error, faulty_return} = fake_upgrade(Pid,faulty_return),

    %% Faulty flags
    {error,{error, {invalid_intensity,faulty_intensity}}} =
	fake_upgrade(Pid,{ok, {{one_for_one,faulty_intensity,1}, []}}),
    {error,{error,{bad_flags, faulty_flags}}} =
	fake_upgrade(Pid,{ok, {faulty_flags, []}}),

    terminate(Pid,shutdown).

code_change_map(_Config) ->
    process_flag(trap_exit, true),

    {ok, Pid} = start_link({ok, {#{}, []}}),
    [] = supervisor:which_children(Pid),

    %% Change supervisor flags
    S1 = sys:get_state(Pid),
    ok = fake_upgrade(Pid,{ok, {#{intensity=>1, period=>3}, []}}),
    S2 = sys:get_state(Pid),
    true = (S1 /= S2),

    %% Faulty childspec
    FaultyChild = #{id=>faulty_child},
    {error,{error,missing_start}} =
	fake_upgrade(Pid,{ok,{#{},[FaultyChild]}}),

    %% Add child1 and child2
    Child1 = #{id=>child1,
	       start=>{supervisor_1, start_child, []},
	       shutdown=>2000},
    Child2 = #{id=>child2,
	       start=>{supervisor_1, start_child, []}},
    ok = fake_upgrade(Pid,{ok,{#{},[Child1,Child2]}}),
    %% Children are not automatically started
    {ok,_} = supervisor:restart_child(Pid,child1),
    {ok,_} = supervisor:restart_child(Pid,child2),
    [{child2,_,_,_},{child1,_,_,_}] = supervisor:which_children(Pid),
    {ok,#{shutdown:=2000}} = supervisor:get_childspec(Pid,child1),

    %% Change child1, remove child2 and add child3
    Child11 = #{id=>child1,
		start=>{supervisor_1, start_child, []},
		shutdown=>1000},
    Child3 = #{id=>child3,
	       start=>{supervisor_1, start_child, []}},
    ok = fake_upgrade(Pid,{ok, {#{}, [Child11,Child3]}}),
    %% Children are not deleted on upgrade, so it is ok that child2 is
    %% still here
    [{child2,_,_,_},{child3,_,_,_},{child1,_,_,_}] =
	supervisor:which_children(Pid),
    {ok,#{shutdown:=1000}} = supervisor:get_childspec(Pid,child1),

    %% Ignore during upgrade
    ok = fake_upgrade(Pid,ignore),

    %% Error during upgrade
    {error, faulty_return} = fake_upgrade(Pid,faulty_return),

    %% Faulty flags
    {error,{error, {invalid_intensity,faulty_intensity}}} =
	fake_upgrade(Pid,{ok, {#{intensity=>faulty_intensity}, []}}),

    terminate(Pid,shutdown).

code_change_simple(_Config) ->
    process_flag(trap_exit, true),

    SimpleChild1 = {child1,{supervisor_1, start_child, []}, permanent,
		    brutal_kill, worker, []},
    SimpleFlags = {simple_one_for_one, 0, 1},
    {ok, SimplePid}  = start_link({ok, {SimpleFlags,[SimpleChild1]}}),
    %% Change childspec
    SimpleChild11 = {child1,{supervisor_1, start_child, []}, permanent,
		    1000, worker, []},
    ok = fake_upgrade(SimplePid,{ok,{SimpleFlags,[SimpleChild11]}}),

    %% Attempt to add child
    SimpleChild2 = {child2,{supervisor_1, start_child, []}, permanent,
		    brutal_kill, worker, []},

    {error, {error, {ok,{[_,_],_}}}} =
	fake_upgrade(SimplePid,{ok,{SimpleFlags,[SimpleChild1,SimpleChild2]}}),

    %% Attempt to remove child
    {error, {error, {ok,{[],_}}}} = fake_upgrade(SimplePid,{ok,{SimpleFlags,[]}}),

    terminate(SimplePid,shutdown),
    ok.

code_change_simple_map(_Config) ->
    process_flag(trap_exit, true),

    SimpleChild1 = #{id=>child1,
		     start=>{supervisor_1, start_child, []}},
    SimpleFlags = #{strategy=>simple_one_for_one},
    {ok, SimplePid}  = start_link({ok, {SimpleFlags,[SimpleChild1]}}),
    %% Change childspec
    SimpleChild11 = #{id=>child1,
		      start=>{supervisor_1, start_child, []},
		      shutdown=>1000},
    ok = fake_upgrade(SimplePid,{ok,{SimpleFlags,[SimpleChild11]}}),

    %% Attempt to add child
    SimpleChild2 = #{id=>child2,
		     start=>{supervisor_1, start_child, []}},
    {error, {error, {ok, {[_,_],_}}}} =
	fake_upgrade(SimplePid,{ok,{SimpleFlags,[SimpleChild1,SimpleChild2]}}),

    %% Attempt to remove child
    {error, {error, {ok, {[],_}}}} =
	fake_upgrade(SimplePid,{ok,{SimpleFlags,[]}}),

    terminate(SimplePid,shutdown),
    ok.

fake_upgrade(Pid,NewInitReturn) ->
    ok = sys:suspend(Pid),

    %% Update state to fake code change
    %% The #state record in supervisor.erl holds the arguments given
    %% to the callback init function. By replacing these arguments the
    %% init function will return something new and by that fake a code
    %% change (see init function above in this module).
    Fun = fun(State) ->
		  Size = size(State), % 'args' is the last field in #state.
		  setelement(Size,State,NewInitReturn)
	  end,
    sys:replace_state(Pid,Fun),

    R = sys:change_code(Pid,gen_server,dummy_vsn,[]),
    ok = sys:resume(Pid),
    R.

%% Test that children are started in the order they are given, and
%% terminated in the opposite order
order_of_children(_Config) ->
    process_flag(trap_exit, true),
    %% Use child ids that are not alphabetically storted
    Id1 = ch7,
    Id2 = ch3,
    Id3 = ch10,
    Id4 = ch2,
    Id5 = ch5,
    Children =
        [{Id, {supervisor_1, start_child, []}, permanent, 1000, worker, []} ||
            Id <- [Id1,Id2,Id3,Id4,Id5]],

    {ok, SupPid}  = start_link({ok, {{rest_for_one, 2, 3600}, Children}}),


    %% Check start order (pids are growing)
    Which1 = supervisor:which_children(sup_test),
    IsPid = fun({_,P,_,_}) when is_pid(P) -> true; (_) -> false end,
    true = lists:all(IsPid,Which1),
    SortedOnPid1 = lists:keysort(2,Which1),
    [{Id1,Pid1,_,_},
     {Id2,Pid2,_,_},
     {Id3,Pid3,_,_},
     {Id4,Pid4,_,_},
     {Id5,Pid5,_,_}] = SortedOnPid1,

    TPid = self(),
    TraceHandler = fun({trace,P,exit,_},{Last,Ps}) when P=:=Last ->
                           TPid ! {exited,lists:reverse([P|Ps])},
                           {Last,Ps};
                      ({trace,P,exit,_},{Last,Ps}) ->
                           {Last,[P|Ps]};
                      (_T,Acc) ->
                           Acc
                   end,

    %% Terminate Pid3 and check that Pid4 and Pid5 are terminated in
    %% expected order.
    Expected1 = [Pid5,Pid4],
    {ok,_} = dbg:tracer(process,{TraceHandler,{Pid4,[]}}),
    [{ok,[_]} = dbg:p(P,procs) || P <- Expected1],
    terminate(Pid3, abnormal),
    receive {exited,ExitedPids1} ->
            dbg:stop(),
            case ExitedPids1 of
                Expected1 -> ok;
                _ -> ct:fail({faulty_termination_order,
                              {expected,Expected1},
                              {got,ExitedPids1}})
            end
    after 3000 ->
            dbg:stop(),
            ct:fail({shutdown_fail,timeout})
    end,

    %% Then check that Id3-5 are started again in correct order
    Which2 = supervisor:which_children(sup_test),
    true = lists:all(IsPid,Which2),
    SortedOnPid2 = lists:keysort(2,Which2),
    [{Id1,Pid1,_,_},
     {Id2,Pid2,_,_},
     {Id3,Pid32,_,_},
     {Id4,Pid42,_,_},
     {Id5,Pid52,_,_}] = SortedOnPid2,

    %% Terminate supervisor and check that all children are terminated
    %% in opposite start order
    Expected2 = [Pid52,Pid42,Pid32,Pid2,Pid1],
    {ok,_} = dbg:tracer(process,{TraceHandler,{Pid1,[]}}),
    [{ok,[_]} = dbg:p(P,procs) || P <- Expected2],
    exit(SupPid,shutdown),
    receive {exited,ExitedPids2} ->
            dbg:stop(),
            case ExitedPids2 of
                Expected2 -> ok;
                _ -> ct:fail({faulty_termination_order,
                              {expected,Expected2},
                              {got,ExitedPids2}})
            end
    after 3000 ->
            dbg:stop(),
            ct:fail({shutdown_fail,timeout})
    end,
    ok.

%% Test that a non-simple supervisor scales well for starting and
%% stopping many children.
scale_start_stop_many_children(_Config) ->
    case erlang:system_info(build_type) of
        opt -> scale_start_stop_many_children();
        Other -> {skip,"Run on build type 'opt' only (current: '" ++
                      atom_to_list(Other)++"')"}
    end.

scale_start_stop_many_children() ->
    process_flag(trap_exit, true),
    {ok, _Pid}  = start_link({ok, {{one_for_one, 2, 3600}, []}}),
    N1 = 1000,
    N2 = 100000,
    Ids1 = lists:seq(1,N1),
    Ids2 = lists:seq(1,N2),
    Children1 = [{Id,{supervisor_1,start_child,[]},permanent,1000,worker,[]} ||
                    Id <- Ids1],
    Children2 = [{Id,{supervisor_1,start_child,[]},permanent,1000,worker,[]} ||
                    Id <- Ids2],

    {StartT1,_} =
        timer:tc(fun() ->
                         [supervisor:start_child(sup_test,C) || C <- Children1]
                 end),
    {StopT1,_} =
        timer:tc(fun() ->
                         [supervisor:terminate_child(sup_test,I) || I <- Ids1]
                 end),
    ct:log("~w children, start time: ~w ms, stop time: ~w ms",
           [N1, StartT1 div 1000, StopT1 div 1000]),

    {StartT2,_} =
        timer:tc(fun() ->
                         [supervisor:start_child(sup_test,C) || C <- Children2]
                 end),
    {StopT2,_} =
        timer:tc(fun() ->
                         [supervisor:terminate_child(sup_test,I) || I <- Ids2]
                 end),
    ct:log("~w children, start time: ~w ms, stop time: ~w ms",
           [N2, StartT2 div 1000, StopT2 div 1000]),

    TimerAdjustment =
        case os:type() of
            {win32,_} ->
                %% Windows timer unit is really bad...
                16000;
            _ ->
                %% To avoid div by zero
                1
        end,

    %% Scaling should be more or less linear, but allowing a bit more
    %% to avoid false alarms (add 1 to avoid div zero)
    ScaleLimit = (N2 div N1) * 10,
    StartScale = StartT2 div (StartT1+TimerAdjustment),
    StopScale = StopT2 div (StopT1+TimerAdjustment),

    ct:log("Scale limit: ~w~nStart scale: ~w~nStop scale: ~w",
           [ScaleLimit, StartScale, StopScale]),

    if StartScale > ScaleLimit ->
            ct:fail({bad_start_scale,StartScale});
       StopScale > ScaleLimit ->
            ct:fail({bad_stop_scale,StopScale});
       true ->
            ok
    end,

    ok.

%% Test report callback for Logger handler error_logger
format_log_1(_Config) ->
    FD = application:get_env(kernel, error_logger_format_depth),
    application:unset_env(kernel, error_logger_format_depth),
    Term = lists:seq(1, 15),
    Supervisor = my_supervisor,
    Name = self(),
    Error = shutdown_error,
    Child = [{pid,Name},{id,any_id},
             {mfargs,{mod,func,[any,Term]}},
             {restart_type,temporary},
             {shutdown,brutal_kill},
             {child_type,worker}],
    Report = #{label=>{supervisor,Error},
               report=>[{supervisor,Supervisor},
                        {errorContext,Error},
                        {reason,Term},
                        {offender,Child}]},
    {F1, A1} = supervisor:format_log(Report),
    FExpected1 = "    supervisor: ~tp~n"
        "    errorContext: ~tp~n"
        "    reason: ~tp~n"
        "    offender: ~tp~n",
    ct:log("F1: ~ts~nA1: ~tp", [F1,A1]),
    FExpected1 = F1,
    [Supervisor,Error,Term,Child] = A1,

    Progress = #{label=>{supervisor,progress},
                 report=>[{supervisor,Supervisor},{started,Child}]},
    {PF1,PA1} = supervisor:format_log(Progress),
    PFExpected1 = "    supervisor: ~tp~n    started: ~tp~n",
    ct:log("PF1: ~ts~nPA1: ~tp", [PF1,PA1]),
    PFExpected1 = PF1,
    [Supervisor,Child] = PA1,

    Depth = 10,
    ok = application:set_env(kernel, error_logger_format_depth, Depth),
    Limited = [1,2,3,4,5,6,7,8,9,'...'],
    {F2,A2} = supervisor:format_log(Report),
    FExpected2 = "    supervisor: ~tP~n"
        "    errorContext: ~tP~n"
        "    reason: ~tP~n"
        "    offender: ~tP~n",
    ct:log("F2: ~ts~nA2: ~tp", [F2,A2]),
    FExpected2 = F2,
    Limited = [1,2,3,4,5,6,7,8,9,'...'],
    LimitedChild = [{pid,Name},{id,any_id},
                    {mfargs,{mod,func,[any,'...']}},
                    {restart_type,temporary},
                    {shutdown,brutal_kill},
                    {child_type,worker}],

    [Supervisor,Depth,Error,Depth,Limited,Depth,LimitedChild,Depth] = A2,

    {PF2,PA2} = supervisor:format_log(Progress),
    PFExpected2 = "    supervisor: ~tP~n    started: ~tP~n",
    ct:log("PF2: ~ts~nPA2: ~tp", [PF2,PA2]),
    PFExpected2 = PF2,
    [Supervisor,Depth,LimitedChild,Depth] = PA2,

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
    Supervisor = my_supervisor,
    Name = self(),
    NameStr = pid_to_list(Name),
    Error = shutdown_error,
    Child = [{pid,Name},{id,any_id},
             {mfargs,{mod,func,[Term]}},
             {restart_type,temporary},
             {shutdown,brutal_kill},
             {child_type,worker}],
    Report = #{label=>{supervisor,Error},
               report=>[{supervisor,Supervisor},
                        {errorContext,Error},
                        {reason,Term},
                        {offender,Child}]},
    FormatOpts1 = #{},
    Str1 = flatten_format_log(Report, FormatOpts1),
    L1 = length(Str1),
    Expected1 = "    supervisor: my_supervisor\n"
        "    errorContext: shutdown_error\n"
        "    reason: [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]\n"
        "    offender: [{pid,"++NameStr++"},\n"
        "               {id,any_id},\n"
        "               {mfargs,{mod,func,"
                          "[[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]]}},\n"
        "               {restart_type,temporary},\n"
        "               {shutdown,brutal_kill},\n"
        "               {child_type,worker}]\n",
    ct:log("Str1: ~ts", [Str1]),
    ct:log("length(Str1): ~p", [L1]),
    true = Expected1 =:= Str1,

    Progress = #{label=>{supervisor,progress},
                 report=>[{supervisor,Supervisor},{started,Child}]},
    PStr1 = flatten_format_log(Progress, FormatOpts1),
    PL1 = length(PStr1),
    PExpected1 = "    supervisor: my_supervisor\n"
        "    started: [{pid,"++NameStr++"},\n"
        "              {id,any_id},\n"
        "              {mfargs,{mod,func,"
                          "[[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]]}},\n"
        "              {restart_type,temporary},\n"
        "              {shutdown,brutal_kill},\n"
        "              {child_type,worker}]\n",
    ct:log("PStr1: ~ts", [PStr1]),
    ct:log("length(PStr1): ~p", [PL1]),
    true = PExpected1 =:= PStr1,

    Depth = 10,
    FormatOpts2 = #{depth=>Depth},
    Str2 = flatten_format_log(Report, FormatOpts2),
    L2 = length(Str2),
    Expected2 = "    supervisor: my_supervisor\n"
        "    errorContext: shutdown_error\n"
        "    reason: [1,2,3,4,5,6,7,8,9|...]\n"
        "    offender: [{pid,"++NameStr++"},\n"
        "               {id,any_id},\n"
        "               {mfargs,{mod,func,[[...]]}},\n"
        "               {restart_type,temporary},\n"
        "               {shutdown,brutal_kill},\n"
        "               {child_type,worker}]\n",
    ct:log("Str2: ~ts", [Str2]),
    ct:log("length(Str2): ~p", [L2]),
    true = Expected2 =:= Str2,

    PStr2 = flatten_format_log(Progress, FormatOpts2),
    PL2 = length(PStr2),
    PExpected2 = "    supervisor: my_supervisor\n"
        "    started: [{pid,"++NameStr++"},\n"
        "              {id,any_id},\n"
        "              {mfargs,{mod,func,[[...]]}},\n"
        "              {restart_type,temporary},\n"
        "              {shutdown,brutal_kill},\n"
        "              {child_type,worker}]\n",
    ct:log("PStr2: ~ts", [PStr2]),
    ct:log("length(PStr2): ~p", [PL2]),
    true = PExpected2 =:= PStr2,

    FormatOpts3 = #{chars_limit=>200},
    Str3 = flatten_format_log(Report, FormatOpts3),
    L3 = length(Str3),
    Expected3 = "    supervisor: my_supervisor\n"
        "    errorContext: ",
    ct:log("Str3: ~ts", [Str3]),
    ct:log("length(Str3): ~p", [L3]),
    true = lists:prefix(Expected3, Str3),
    true = L3 < L1,

    PFormatOpts3 = #{chars_limit=>80},
    PStr3 = flatten_format_log(Progress, PFormatOpts3),
    PL3 = length(PStr3),
    PExpected3 = "    supervisor: my_supervisor\n    started:",
    ct:log("PStr3: ~ts", [PStr3]),
    ct:log("length(PStr3): ~p", [PL3]),
    true = lists:prefix(PExpected3, PStr3),
    true = PL3 < PL1,

    FormatOpts4 = #{single_line=>true},
    Str4 = flatten_format_log(Report, FormatOpts4),
    L4 = length(Str4),

    Expected4 = "Supervisor: my_supervisor. Context: shutdown_error. "
        "Reason: [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]. "
        "Offender: id=any_id,pid="++NameStr++".",
    ct:log("Str4: ~ts", [Str4]),
    ct:log("length(Str4): ~p", [L4]),
    true = Expected4 =:= Str4,

    PStr4 = flatten_format_log(Progress, FormatOpts4),
    PL4 = length(PStr4),
    PExpected4 = "Supervisor: my_supervisor. "
        "Started: id=any_id,pid="++NameStr++".",
    ct:log("PStr4: ~ts", [PStr4]),
    ct:log("length(PStr4): ~p", [PL4]),
    true = PExpected4 =:= PStr4,

    FormatOpts5 = #{single_line=>true, depth=>Depth},
    Str5 = flatten_format_log(Report, FormatOpts5),
    L5 = length(Str5),
    Expected5 = "Supervisor: my_supervisor. Context: shutdown_error. "
        "Reason: [1,2,3,4,5,6,7,8,9|...]. "
        "Offender: id=any_id,pid="++NameStr++".",
    ct:log("Str5: ~ts", [Str5]),
    ct:log("length(Str5): ~p", [L5]),
    true = Expected5 =:= Str5,

    PStr5 = flatten_format_log(Progress, FormatOpts5),
    PL5 = length(PStr5),
    PExpected5 = "Supervisor: my_supervisor. "
        "Started: id=any_id,pid="++NameStr++".",
    ct:log("PStr5: ~ts", [PStr5]),
    ct:log("length(PStr5): ~p", [PL5]),
    true = PExpected5 =:= PStr5,

    FormatOpts6 = #{single_line=>true, chars_limit=>200},
    Str6 = flatten_format_log(Report, FormatOpts6),
    L6 = length(Str6),
    Expected6 = "Supervisor: my_supervisor. Context:",
    ct:log("Str6: ~ts", [Str6]),
    ct:log("length(Str6): ~p", [L6]),
    true = lists:prefix(Expected6, Str6),
    true = L6 < L4,

    PFormatOpts6 = #{single_line=>true, chars_limit=>60},
    PStr6 = flatten_format_log(Progress, PFormatOpts6),
    PL6 = length(PStr6),
    PExpected6 = "Supervisor: my_supervisor.",
    ct:log("PStr6: ~ts", [PStr6]),
    ct:log("length(PStr6): ~p", [PL6]),
    true = lists:prefix(PExpected6, PStr6),
    true = PL6 < PL4,

    Child2 = [{nb_children,7},{id,any_id},
              {mfargs,{mod,func,[Term]}},
              {restart_type,temporary},
              {shutdown,brutal_kill},
              {child_type,worker}],
    Report2 = #{label=>{supervisor,Error},
                report=>[{supervisor,Supervisor},
                         {errorContext,Error},
                         {reason,Term},
                         {offender,Child2}]},
    Str7 = flatten_format_log(Report2, FormatOpts6),
    L7 = length(Str7),
    ct:log("Str7: ~ts", [Str7]),
    ct:log("length(Str7): ~p", [L7]),
    true = string:find(Str7, "Offender: id=any_id,nb_children=7.") =/= nomatch,

    ok.

flatten_format_log(Report, Format) ->
    lists:flatten(supervisor:format_log(Report, Format)).

% Test that terminations of temporary non-significant children do not
% trigger auto-shutdown regardless of the supervisor's auto_shutdown
% mode.
nonsignificant_temporary(_Config) ->
    process_flag(trap_exit, true),
    Child1 = #{id => child1,
	       start => {supervisor_1, start_child, []},
	       restart => temporary,
	       significant => false},

    lists:foreach(
	fun (AutoShutdown) ->
	    {ok, Sup1} = start_link({ok, {#{auto_shutdown => AutoShutdown}, []}}),

	    % Terminating a temporary non-significant child via the supervisor
	    % should not trigger auto-shutdown.
	    {ok, ChildPid1} = supervisor:start_child(Sup1, Child1),
	    link(ChildPid1),
	    terminate(Sup1, ChildPid1, child1, supervisor),
	    ok = check_exit([ChildPid1]),
	    error = check_exit([Sup1], 1000),

	    % Killing a temporary non-significant child should not trigger
	    % auto-shutdown.
	    {ok, ChildPid2} = supervisor:start_child(Sup1, Child1),
	    link(ChildPid2),
	    terminate(ChildPid2, kill),
	    ok = check_exit([ChildPid2]),
	    error = check_exit([Sup1], 1000),

	    % Abnormal termination of a temporary non-significant child
	    % should not trigger auto-shutdown.
	    {ok, ChildPid3} = supervisor:start_child(Sup1, Child1),
	    link(ChildPid3),
	    terminate(ChildPid3, abnormal),
	    ok = check_exit([ChildPid3]),
	    error = check_exit([Sup1], 1000),

	    % Normal termination of a temporary non-significant child
	    % should not trigger auto-shutdown.
	    {ok, ChildPid4} = supervisor:start_child(Sup1, Child1),
	    link(ChildPid4),
	    terminate(ChildPid4, normal),
	    ok = check_exit([ChildPid4]),
	    error = check_exit([Sup1], 1000),

	    terminate(Sup1, shutdown),
	    ok = check_exit([Sup1])
	end,
	[never, any_significant, all_significant]),

    ok.

% Test that terminations of transient non-significant children do not
% trigger auto-shutdown regardless of the supervisor's auto_shutdown
% mode.
nonsignificant_transient(_Config) ->
    process_flag(trap_exit, true),
    Child1 = #{id => child1,
	       start => {supervisor_1, start_child, []},
	       restart => transient,
	       significant => false},

    lists:foreach(
	fun (AutoShutdown) ->
	    {ok, Sup1} = start_link({ok, {#{intensity => 5,
					    auto_shutdown => AutoShutdown}, []}}),

	    % Terminating a transient non-significant child via the supervisor
	    % should not trigger auto-shutdown.
	    {ok, ChildPid1} = supervisor:start_child(Sup1, Child1),
	    link(ChildPid1),
	    terminate(Sup1, ChildPid1, child1, supervisor),
	    ok = check_exit([ChildPid1]),
	    error = check_exit([Sup1], 1000),

	    % Killing a transient non-significant child should not trigger
	    % auto-shutdown. The killed child should be restarted.
	    {ok, ChildPid2} = supervisor:restart_child(Sup1, child1),
	    link(ChildPid2),
	    terminate(ChildPid2, kill),
	    ok = check_exit([ChildPid2]),
	    error = check_exit([Sup1], 1000),

	    % Abnormal termination of a transient non-significant child should
	    % not trigger auto-shutdown. The terminated child should be restarted.
	    [{child1, ChildPid3, _, _}] = supervisor:which_children(Sup1),
	    link(ChildPid3),
	    terminate(ChildPid3, abnormal),
	    ok = check_exit([ChildPid3]),
	    error = check_exit([Sup1], 1000),

	    % Normal termination of a transient non-significant child should not
	    % trigger  auto-shutdown.
	    [{child1, ChildPid4, _, _}] = supervisor:which_children(Sup1),
	    link(ChildPid4),
	    terminate(ChildPid4, normal),
	    ok = check_exit([ChildPid4]),
	    error = check_exit([Sup1], 1000),

	    terminate(Sup1, shutdown),
	    ok = check_exit([Sup1])
	end,
	[never, any_significant, all_significant]),

    ok.

% Test the auto-shutdown feature with temporary significant children.
significant_temporary(_Config) ->
    process_flag(trap_exit, true),
    Child1 = #{id => child1,
	       start => {supervisor_1, start_child, []},
	       restart => temporary,
	       significant => true},
    Child2 = #{id => child2,
	       start => {supervisor_1, start_child, []},
	       restart => temporary,
	       significant => true},
    Child3 = #{id => child3,
	       start => {supervisor_1, start_child, []},
	       restart => temporary,
	       significant => false},

    % Test auto-shutdown on the exit of any significant child.

    % Terminating a temporary significant child via the supervisor should not
    % trigger auto-shutdown.
    {ok, Sup1} = start_link({ok, {#{intensity => 5,
				    auto_shutdown => any_significant}, []}}),
    supervisor:start_child(Sup1, Child1),
    supervisor:start_child(Sup1, Child2),
    supervisor:start_child(Sup1, Child3),
    [{child3, ChildPid1_3, _, _},
     {child2, ChildPid1_2, _, _},
     {child1, ChildPid1_1, _, _}] = supervisor:which_children(Sup1),
    link(ChildPid1_1),
    link(ChildPid1_2),
    link(ChildPid1_3),
    terminate(Sup1, ChildPid1_1, child1, supervisor),
    ok = check_exit([ChildPid1_1]),
    error = check_exit([ChildPid1_2], 1000),
    error = check_exit([ChildPid1_3], 1000),
    error = check_exit([Sup1], 1000),

    % Killing a temporary significant child should trigger auto-shutdown.
    supervisor:start_child(Sup1, Child1),
    [{child1, ChildPid2_1, _, _},
     {child3, ChildPid2_3, _, _},
     {child2, ChildPid2_2, _, _}] = supervisor:which_children(Sup1),
    link(ChildPid2_1),
    link(ChildPid2_2),
    link(ChildPid2_3),
    terminate(ChildPid2_1, kill),
    ok = check_exit([ChildPid2_1, ChildPid2_2, ChildPid2_3, Sup1]),

    % Abnormal termination of a temporary significant child should trigger
    % auto-shutdown.
    {ok, Sup2} = start_link({ok, {#{intensity => 5,
				    auto_shutdown => any_significant}, []}}),
    supervisor:start_child(Sup2, Child1),
    supervisor:start_child(Sup2, Child2),
    supervisor:start_child(Sup2, Child3),
    [{child3, ChildPid3_3, _, _},
     {child2, ChildPid3_2, _, _},
     {child1, ChildPid3_1, _, _}] = supervisor:which_children(Sup2),
    link(ChildPid3_1),
    link(ChildPid3_2),
    link(ChildPid3_3),
    terminate(ChildPid3_1, abnormal),
    ok = check_exit([ChildPid3_1, ChildPid3_2, ChildPid3_3, Sup2]),

    % Normal termination of a temporary significant child should trigger
    % auto-shutdown.
    {ok, Sup3} = start_link({ok, {#{intensity => 5,
				    auto_shutdown => any_significant}, []}}),
    supervisor:start_child(Sup3, Child1),
    supervisor:start_child(Sup3, Child2),
    supervisor:start_child(Sup3, Child3),
    [{child3, ChildPid4_3, _, _},
     {child2, ChildPid4_2, _, _},
     {child1, ChildPid4_1, _, _}] = supervisor:which_children(Sup3),
    link(ChildPid4_1),
    link(ChildPid4_2),
    link(ChildPid4_3),
    terminate(ChildPid4_1, normal),
    ok = check_exit([ChildPid4_1, ChildPid4_2, ChildPid4_3, Sup3]),

    % Test auto-shutdown on the exit of all significant children.

    {ok, Sup4} = start_link({ok, {#{intensity => 5,
				    auto_shutdown => all_significant}, []}}),

    % The termination of only one of the two significant children should
    % not trigger auto-shutdown. The termination of the second (last of the two)
    % significant child should trigger auto-shutdown.
    supervisor:start_child(Sup4, Child1),
    supervisor:start_child(Sup4, Child2),
    supervisor:start_child(Sup4, Child3),
    [{child3, ChildPid5_3, _, _},
     {child2, ChildPid5_2, _, _},
     {child1, ChildPid5_1, _, _}] = supervisor:which_children(Sup4),
    link(ChildPid5_1),
    link(ChildPid5_2),
    link(ChildPid5_3),
    terminate(ChildPid5_1, normal),
    ok = check_exit([ChildPid5_1]),
    error = check_exit([ChildPid5_2], 1000),
    error = check_exit([ChildPid5_3], 1000),
    error = check_exit([Sup4], 1000),
    terminate(ChildPid5_2, normal),
    ok = check_exit([ChildPid5_2, ChildPid5_3, Sup4]),

    ok.

% Test the auto-shutdown feature with transient significant children.
significant_transient(_Config) ->
    process_flag(trap_exit, true),
    Child1 = #{id => child1,
               start => {supervisor_1, start_child, []},
               restart => transient,
               significant => true},
    Child2 = #{id => child2,
               start => {supervisor_1, start_child, []},
               restart => transient,
               significant => true},
    Child3 = #{id => child3,
               start => {supervisor_1, start_child, []},
               restart => transient,
               significant => false},

    % Test auto-shutdown on the exit of any significant child.

    {ok, Sup1} = start_link({ok, {#{intensity => 5,
				    auto_shutdown => any_significant}, []}}),

    % Terminating a transient significant child via the supervisor should not
    % trigger auto-shutdown.
    supervisor:start_child(Sup1, Child1),
    supervisor:start_child(Sup1, Child2),
    supervisor:start_child(Sup1, Child3),
    [{child3, ChildPid1_3, _, _},
     {child2, ChildPid1_2, _, _},
     {child1, ChildPid1_1, _, _}] = supervisor:which_children(Sup1),
    link(ChildPid1_1),
    link(ChildPid1_2),
    link(ChildPid1_3),
    terminate(Sup1, ChildPid1_1, child1, supervisor),
    ok = check_exit([ChildPid1_1]),
    error = check_exit([ChildPid1_2], 1000),
    error = check_exit([ChildPid1_3], 1000),
    error = check_exit([Sup1], 1000),
    supervisor:restart_child(Sup1, child1),

    % Killing a transient significant child should not trigger
    % auto-shutdown. The killed child should be restarted.
    [{child3, ChildPid2_3, _, _},
     {child2, ChildPid2_2, _, _},
     {child1, ChildPid2_1, _, _}] = supervisor:which_children(Sup1),
    link(ChildPid2_1),
    link(ChildPid2_2),
    link(ChildPid2_3),
    terminate(ChildPid2_1, kill),
    ok = check_exit([ChildPid2_1]),
    error = check_exit([ChildPid2_2], 1000),
    error = check_exit([ChildPid2_3], 1000),
    error = check_exit([Sup1], 1000),

    % Abnormal termination of a transient significant child should not trigger
    % auto-shutdown. The terminated child should be restarted.
    [{child3, ChildPid3_3, _, _},
     {child2, ChildPid3_2, _, _},
     {child1, ChildPid3_1, _, _}] = supervisor:which_children(Sup1),
    link(ChildPid3_1),
    link(ChildPid3_2),
    link(ChildPid3_3),
    terminate(ChildPid3_1, abnormal),
    ok = check_exit([ChildPid3_1]),
    error = check_exit([ChildPid3_2], 1000),
    error = check_exit([ChildPid3_3], 1000),
    error = check_exit([Sup1], 1000),

    % Normal termination of a transient significant child should trigger
    % auto-shutdown.
    [{child3, ChildPid4_3, _, _},
     {child2, ChildPid4_2, _, _},
     {child1, ChildPid4_1, _, _}] = supervisor:which_children(Sup1),
    link(ChildPid4_1),
    link(ChildPid4_2),
    link(ChildPid4_3),
    terminate(ChildPid4_1, normal),
    ok = check_exit([ChildPid4_1, ChildPid4_2, ChildPid4_3, Sup1]),

    % Test auto-shutdown on the exit of all significant children.

    {ok, Sup2} = start_link({ok, {#{intensity => 5,
				    auto_shutdown => all_significant}, []}}),

    % The termination of only one of the two significant children should
    % not trigger auto-shutdown. The termination of the second (last of the two)
    % significant child should trigger auto-shutdown.
    supervisor:start_child(Sup2, Child1),
    supervisor:start_child(Sup2, Child2),
    supervisor:start_child(Sup2, Child3),
    [{child3, ChildPid5_3, _, _},
     {child2, ChildPid5_2, _, _},
     {child1, ChildPid5_1, _, _}] = supervisor:which_children(Sup2),
    link(ChildPid5_1),
    link(ChildPid5_2),
    link(ChildPid5_3),
    terminate(ChildPid5_1, normal),
    ok = check_exit([ChildPid5_1]),
    error = check_exit([ChildPid5_2], 1000),
    error = check_exit([ChildPid5_3], 1000),
    error = check_exit([Sup2], 1000),
    terminate(ChildPid5_2, normal),
    ok = check_exit([ChildPid5_2, ChildPid5_3, Sup2]),

    ok.

% The auto-shutdown feature is not allowed with simple_one_for_one supervisors.
significant_simple(_Config) ->
    process_flag(trap_exit, true),
    Child1 = #{id => child1,
	       start => {supervisor_1, start_child, []}},

    {error, {supervisor_data, {bad_combination, _}}} = start_link({ok, {#{strategy => simple_one_for_one,
                                                                          auto_shutdown => any_significant},
                                                                        [Child1]}}),

    {error, {supervisor_data, {bad_combination, _}}} = start_link({ok, {#{strategy => simple_one_for_one,
                                                                          auto_shutdown => all_significant},
                                                                        [Child1]}}),

    {ok, SupPid} = start_link({ok, {#{strategy => simple_one_for_one,
                                      auto_shutdown => never},
                                    [Child1]}}),
    terminate(SupPid, shutdown),
    check_exit([SupPid]).

% Test that terminations of significant children caused by
% the death of a sibling does not trigger auto-shutdown.
significant_bystander(_Config) ->
    process_flag(trap_exit, true),
    Child1 = #{id => child1,
               start => {supervisor_1, start_child, []},
               restart => transient,
               significant => true},
    Child2 = #{id => child2,
               start => {supervisor_1, start_child, []},
               restart => transient,
               significant => false},
    Child3 = #{id => child3,
               start => {supervisor_1, start_child, []},
               restart => transient,
               significant => true},

    lists:foreach(
        fun ({Strategy, AutoShutdown}) ->
            {ok, Sup} = start_link({ok, {#{strategy => Strategy,
                                           intensity => 5,
                                           auto_shutdown => AutoShutdown}, []}}),
            supervisor:start_child(Sup, Child1),
            supervisor:start_child(Sup, Child2),
            supervisor:start_child(Sup, Child3),
            [{child3, ChildPid3, _, _},
             {child2, ChildPid2, _, _},
             {child1, ChildPid1, _, _}] = supervisor:which_children(Sup),
            link(ChildPid1),
            link(ChildPid2),
            link(ChildPid3),
            terminate(ChildPid2, kill),
            _ = case Strategy of
		one_for_one ->
		    ok = check_exit([ChildPid2]),
		    error = check_exit([ChildPid1], 1000),
		    error = check_exit([ChildPid2], 1000);
                one_for_all ->
                    ok = check_exit([ChildPid1, ChildPid2, ChildPid3]);
                rest_for_one ->
                    ok = check_exit([ChildPid2, ChildPid3]),
                    error = check_exit([ChildPid1], 1000)
            end,
            error = check_exit([Sup], 1000),
            terminate(Sup, shutdown),
            ok = check_exit([Sup])
        end,
        [
            {Strategy, AutoShutdown}
            ||
                Strategy <- [one_for_one, one_for_all, rest_for_one],
                AutoShutdown <- [any_significant, all_significant]
        ]
    ),

    ok.

% Test auto-shutdown escalation. 
significant_escalation(_Config) ->
    process_flag(trap_exit, true),
    ChildSup1 = #{id => sup1,
		  start => {supervisor,
			    start_link,
			    [?MODULE, {ok, {#{auto_shutdown => any_significant}, []}}]},
		  restart => transient,
                  type => supervisor},
    Child1 = #{id => child1,
               start => {supervisor_1, start_child, []},
               restart => transient,
               significant => true},

    % Supervisor A has another supervisor B as a significant child, and supervisor B
    % has a worker C as a significant child. A and B have auto-shutdown enabled.
    % When C terminates, it should cause the auto-shutdown of B, and the termination
    % of B should in turn cause the auto-shutdown of A.
    {ok, Sup1} = start_link({ok, {#{auto_shutdown => any_significant}, []}}),
    {ok, ChildSupPid1} = supervisor:start_child(Sup1, ChildSup1#{significant => true}),
    {ok, ChildPid1} = supervisor:start_child(ChildSupPid1, Child1),
    link(ChildSupPid1),
    link(ChildPid1),
    terminate(ChildPid1, normal),
    ok = check_exit([ChildPid1, ChildSupPid1, Sup1]),

    % Supervisor A has another supervisor B as a non-significant child, and supervisor B
    % has a worker C as a significant child. A and B have auto-shutdown enabled.
    % When C terminates, it should cause the auto-shutdown of B, but the termination
    % of B should not in turn cause the auto-shutdown of A.
    {ok, Sup2} = start_link({ok, {#{auto_shutdown => any_significant}, []}}),
    {ok, ChildSupPid2} = supervisor:start_child(Sup2, ChildSup1#{significant => false}),
    {ok, ChildPid2} = supervisor:start_child(ChildSupPid2, Child1),
    link(ChildSupPid2),
    link(ChildPid2),
    terminate(ChildPid2, normal),
    ok = check_exit([ChildPid2, ChildSupPid2]),
    error = check_exit([Sup2], 1000),
    terminate(Sup2, shutdown),
    ok = check_exit([Sup2]),

    ok.

% Test upgrading auto-shutdown from never to any_significant.
significant_upgrade_never_any(_Config) ->
    process_flag(trap_exit, true),
    Child1 = #{id => child1,
	       start => {supervisor_1, start_child, []},
	       restart => transient,
	       significant => true},
    Child2 = #{id => child2,
	       start => {supervisor_1, start_child, []},
	       restart => transient,
	       significant => true},
    {ok, Sup1} = start_link({ok, {#{auto_shutdown => never}, []}}),
    {error, {bad_combination, _}} = supervisor:start_child(Sup1, Child1),
    S1 = sys:get_state(Sup1),
    ok = fake_upgrade(Sup1, {ok, {#{auto_shutdown => any_significant}, []}}),
    S2 = sys:get_state(Sup1),
    true = (S1 /= S2),
    {ok, ChildPid1} = supervisor:start_child(Sup1, Child1),
    {ok, ChildPid2} = supervisor:start_child(Sup1, Child2),
    link(ChildPid1),
    link(ChildPid2),
    terminate(ChildPid1, normal),
    ok = check_exit([ChildPid1, ChildPid2, Sup1]),
    ok.

% Test upgrading auto-shutdown from any_significant to never.
significant_upgrade_any_never(_Config) ->
    process_flag(trap_exit, true),
    Child1 = #{id => child1,
	       start => {supervisor_1, start_child, []},
	       restart => transient,
	       significant => true},
    Child2 = #{id => child2,
	       start => {supervisor_1, start_child, []},
	       restart => transient,
	       significant => true},
    {ok, Sup1} = start_link({ok, {#{auto_shutdown => any_significant}, []}}),
    {ok, ChildPid1} = supervisor:start_child(Sup1, Child1),
    link(ChildPid1),
    S1 = sys:get_state(Sup1),
    ok = fake_upgrade(Sup1, {ok, {#{auto_shutdown => never}, []}}),
    S2 = sys:get_state(Sup1),
    true = (S1 /= S2),
    error = check_exit([ChildPid1], 1000),
    error = check_exit([Sup1], 1000),
    {error, {bad_combination, _}} = supervisor:start_child(Sup1, Child2),
    terminate(ChildPid1, normal),
    ok = check_exit([ChildPid1]),
    error = check_exit([Sup1], 1000),
    {ok, ChildPid2} = supervisor:restart_child(Sup1, child1),
    link(ChildPid2),
    terminate(Sup1, shutdown),
    ok = check_exit([ChildPid2, Sup1]),
    ok.

% Test upgrading auto-shutdown from never to all_significant.
significant_upgrade_never_all(_Config) ->
    process_flag(trap_exit, true),
    Child1 = #{id => child1,
	       start => {supervisor_1, start_child, []},
	       restart => transient,
	       significant => true},
    Child2 = #{id => child2,
	       start => {supervisor_1, start_child, []},
	       restart => transient,
	       significant => true},
    {ok, Sup1} = start_link({ok, {#{auto_shutdown => never}, []}}),
    {error, {bad_combination, _}} = supervisor:start_child(Sup1, Child1),
    S1 = sys:get_state(Sup1),
    ok = fake_upgrade(Sup1, {ok, {#{auto_shutdown => all_significant}, []}}),
    S2 = sys:get_state(Sup1),
    true = (S1 /= S2),
    {ok, ChildPid1} = supervisor:start_child(Sup1, Child1),
    {ok, ChildPid2} = supervisor:start_child(Sup1, Child2),
    link(ChildPid1),
    link(ChildPid2),
    terminate(ChildPid1, normal),
    ok = check_exit([ChildPid1]),
    error = check_exit([ChildPid2], 1000),
    error = check_exit([Sup1], 1000),
    terminate(ChildPid2, normal),
    ok = check_exit([ChildPid2, Sup1]),
    ok.

% Test upgrading auto-shutdown from all_significant to never.
significant_upgrade_all_never(_Config) ->
    process_flag(trap_exit, true),
    Child1 = #{id => child1,
	       start => {supervisor_1, start_child, []},
	       restart => transient,
	       significant => true},
    Child2 = #{id => child2,
	       start => {supervisor_1, start_child, []},
	       restart => transient,
	       significant => true},
    {ok, Sup1} = start_link({ok, {#{auto_shutdown => all_significant}, []}}),
    {ok, ChildPid1} = supervisor:start_child(Sup1, Child1),
    link(ChildPid1),
    S1 = sys:get_state(Sup1),
    ok = fake_upgrade(Sup1, {ok, {#{auto_shutdown => never}, []}}),
    S2 = sys:get_state(Sup1),
    true = (S1 /= S2),
    error = check_exit([ChildPid1], 1000),
    error = check_exit([Sup1], 1000),
    {error, {bad_combination, _}} = supervisor:start_child(Sup1, Child2),
    terminate(ChildPid1, normal),
    ok = check_exit([ChildPid1]),
    error = check_exit([Sup1], 1000),
    {ok, ChildPid2} = supervisor:restart_child(Sup1, child1),
    link(ChildPid2),
    terminate(Sup1, shutdown),
    ok = check_exit([ChildPid2, Sup1]),
    ok.

% Test upgrading auto-shutdown from any_significant to all_significant.
significant_upgrade_any_all(_Config) ->
    process_flag(trap_exit, true),
    Child1 = #{id => child1,
	       start => {supervisor_1, start_child, []},
	       restart => transient,
	       significant => true},
    Child2 = #{id => child2,
	       start => {supervisor_1, start_child, []},
	       restart => transient,
	       significant => true},
    {ok, Sup1} = start_link({ok, {#{auto_shutdown => any_significant}, []}}),
    {ok, ChildPid1} = supervisor:start_child(Sup1, Child1),
    {ok, ChildPid2} = supervisor:start_child(Sup1, Child2),
    link(ChildPid1),
    link(ChildPid2),
    S1 = sys:get_state(Sup1),
    ok = fake_upgrade(Sup1, {ok, {#{auto_shutdown => all_significant}, []}}),
    S2 = sys:get_state(Sup1),
    true = (S1 /= S2),
    error = check_exit([ChildPid1], 1000),
    error = check_exit([ChildPid2], 1000),
    error = check_exit([Sup1], 1000),
    terminate(ChildPid1, normal),
    ok = check_exit([ChildPid1]),
    error = check_exit([ChildPid2], 1000),
    error = check_exit([Sup1], 1000),
    terminate(ChildPid2, normal),
    ok = check_exit([ChildPid2, Sup1]),
    ok.

% Test upgrading auto-shutdown from all_significant to any_significant.
significant_upgrade_all_any(_Config) ->
    process_flag(trap_exit, true),
    Child1 = #{id => child1,
	       start => {supervisor_1, start_child, []},
	       restart => transient,
	       significant => true},
    Child2 = #{id => child2,
	       start => {supervisor_1, start_child, []},
	       restart => transient,
	       significant => true},
    {ok, Sup1} = start_link({ok, {#{auto_shutdown => all_significant}, []}}),
    {ok, ChildPid1} = supervisor:start_child(Sup1, Child1),
    {ok, ChildPid2} = supervisor:start_child(Sup1, Child2),
    link(ChildPid1),
    link(ChildPid2),
    S1 = sys:get_state(Sup1),
    ok = fake_upgrade(Sup1, {ok, {#{auto_shutdown => any_significant}, []}}),
    S2 = sys:get_state(Sup1),
    true = (S1 /= S2),
    error = check_exit([ChildPid1], 1000),
    error = check_exit([ChildPid2], 1000),
    error = check_exit([Sup1], 1000),
    terminate(ChildPid1, normal),
    ok = check_exit([ChildPid1, ChildPid2, Sup1]),
    ok.

% Test upgrading a child from non-significant to significant
% and vice versa.
significant_upgrade_child(_Config) ->
    process_flag(trap_exit, true),
    Child1 = #{id => child,
	      start => {supervisor_1, start_child, []},
	      restart => transient,
	      significant => false},
    Child2 = #{id => child,
	      start => {supervisor_1, start_child, []},
	      restart => transient,
	      significant => true},

    % A supervisor with auto-shutdown set to never should not allow upgrading
    % a non-significant child to significant.
    {ok, Sup1} = start_link({ok, {#{auto_shutdown => never}, [Child1]}}),
    [{child, ChildPid1, _, _}] = supervisor:which_children(Sup1),
    link(ChildPid1),
    S1_1 = sys:get_state(Sup1),
    {error, _} = fake_upgrade(Sup1, {ok, {#{auto_shutdown => never}, [Child2]}}),
    S1_2 = sys:get_state(Sup1),
    true = (S1_1 /= S1_2),
    error = check_exit([ChildPid1], 1000),
    error = check_exit([Sup1], 1000),
    terminate(Sup1, shutdown),
    ok = check_exit([ChildPid1, Sup1]),

    % Start with a non-significant child, upgrade it to significant
    % and ensure that it triggers auto-shutdown.
    {ok, Sup2} = start_link({ok, {#{auto_shutdown => any_significant}, [Child1]}}),
    [{child, ChildPid2, _, _}] = supervisor:which_children(Sup2),
    link(ChildPid2),
    S2_1 = sys:get_state(Sup2),
    ok = fake_upgrade(Sup2, {ok, {#{auto_shutdown => any_significant}, [Child2]}}),
    S2_2 = sys:get_state(Sup2),
    true = (S2_1 /= S2_2),
    error = check_exit([ChildPid2], 1000),
    error = check_exit([Sup2], 1000),
    terminate(ChildPid2, normal),
    ok = check_exit([ChildPid2, Sup2]),

    % Start with a significant child, upgrade it to non-significant
    % and ensure that it does not trigger auto-shutdown.
    {ok, Sup3} = start_link({ok, {#{auto_shutdown => any_significant}, [Child2]}}),
    [{child, ChildPid3, _, _}] = supervisor:which_children(Sup3),
    link(ChildPid3),
    S3_1 = sys:get_state(Sup3),
    ok = fake_upgrade(Sup3, {ok, {#{auto_shutdown => any_significant}, [Child1]}}),
    S3_2 = sys:get_state(Sup3),
    true = (S3_1 /= S3_2),
    error = check_exit([ChildPid3], 1000),
    error = check_exit([Sup3], 1000),
    terminate(ChildPid3, normal),
    ok = check_exit([ChildPid3]),
    error = check_exit([Sup3], 1000),
    terminate(Sup3, shutdown),
    ok = check_exit([Sup3]),

    ok.

%% Test trying to start a child that uses an already registered name.
already_started_outside_supervisor(_Config) ->
    %% Avoid inter-testcase flakiness
    ensure_supervisor_is_stopped(),
    process_flag(trap_exit, true),
    {ok, SupPid} = start_link({ok, {#{}, []}}),
    RegName = registered_name,
    Child = #{id => child,
	      start => {?MODULE, start_registered_name, [RegName]},
	      restart => transient,
	      significant => false},
    %% We start another process and register the name.
    Pid = spawn_link(fun() ->
                             true = register(RegName, self()),
                             receive
                                 die -> ok
                             end
                     end),
    {error, {already_started, P}} = supervisor:start_child(SupPid, Child),
    Pid = P,
    terminate(SupPid, shutdown),
    Pid ! die,
    ok = check_exit([SupPid]),
    ok.

%% Test which_children/1 and which_child/2.
which_children(Config) when is_list(Config) ->
    {ok, SupPid} = start_link({ok, {#{}, []}}),

    [] = supervisor:which_children(SupPid),
    {error, not_found} = supervisor:which_child(SupPid, childx),

    {ok, Child1} = supervisor:start_child(SupPid, #{id => child1,
						    start => {supervisor_1, start_child, []}}),
    [{child1, Child1, worker, [supervisor_1]}] = supervisor:which_children(SupPid),
    {ok, {child1, Child1, worker, [supervisor_1]}} = supervisor:which_child(SupPid, child1),
    {error, not_found} = supervisor:which_child(SupPid, childx),

    {ok, Child2} = supervisor:start_child(SupPid, #{id => child2,
						    start => {supervisor_1, start_child, []}}),
    [{child2, Child2, worker, [supervisor_1]},
     {child1, Child1, worker, [supervisor_1]}] = supervisor:which_children(SupPid),
    {ok, {child1, Child1, worker, [supervisor_1]}} = supervisor:which_child(SupPid, child1),
    {ok, {child2, Child2, worker, [supervisor_1]}} = supervisor:which_child(SupPid, child2),
    {error, not_found} = supervisor:which_child(SupPid, childx),

    ok = supervisor:terminate_child(SupPid, child1),
    [{child2, Child2, worker, [supervisor_1]},
     {child1, undefined, worker, [supervisor_1]}] = supervisor:which_children(SupPid),
    {ok, {child1, undefined, worker, [supervisor_1]}} = supervisor:which_child(SupPid, child1),
    {ok, {child2, Child2, worker, [supervisor_1]}} = supervisor:which_child(SupPid, child2),
    {error, not_found} = supervisor:which_child(SupPid, childx),

    ok = supervisor:delete_child(SupPid, child1),
    [{child2, Child2, worker, [supervisor_1]}] = supervisor:which_children(SupPid),
    {error, not_found} = supervisor:which_child(SupPid, child1),
    {ok, {child2, Child2, worker, [supervisor_1]}} = supervisor:which_child(SupPid, child2),
    {error, not_found} = supervisor:which_child(SupPid, childx),

    ok.

which_children_simple_one_for_one(Config) when is_list(Config) ->
    {ok, SupPid} = start_link({ok, {#{strategy => simple_one_for_one}, [#{id => child,
									  start => {supervisor_1, start_child, []},
									  restart => temporary}]}}),

    [] = supervisor:which_children(SupPid),
    {error, not_found} = supervisor:which_child(SupPid, self()),

    {ok, Child1} = supervisor:start_child(SupPid, []),
    [{undefined, Child1, worker, [supervisor_1]}] = supervisor:which_children(SupPid),
    {ok, {undefined, Child1, worker, [supervisor_1]}} = supervisor:which_child(SupPid, Child1),
    {error, not_found} = supervisor:which_child(SupPid, self()),

    {ok, Child2} = supervisor:start_child(SupPid, []),
    [{undefined, Child1, worker, [supervisor_1]},
     {undefined, Child2, worker, [supervisor_1]}] = supervisor:which_children(SupPid),
    {ok, {undefined, Child1, worker, [supervisor_1]}} = supervisor:which_child(SupPid, Child1),
    {ok, {undefined, Child2, worker, [supervisor_1]}} = supervisor:which_child(SupPid, Child2),
    {error, not_found} = supervisor:which_child(SupPid, self()),

    ok = supervisor:terminate_child(SupPid, Child1),
    [{undefined, Child2, worker, [supervisor_1]}] = supervisor:which_children(SupPid),
    {error, not_found} = supervisor:which_child(SupPid, Child1),
    {ok, {undefined, Child2, worker, [supervisor_1]}} = supervisor:which_child(SupPid, Child2),
    {error, not_found} = supervisor:which_child(SupPid, self()),

    {error, simple_one_for_one} = supervisor:which_child(SupPid, not_a_pid),

    ok.

%%-------------------------------------------------------------------------
terminate(Pid, Reason) when Reason =/= supervisor ->
    terminate(dummy, Pid, dummy, Reason).

terminate(Sup, _, ChildId, supervisor) ->
    ok = supervisor:terminate_child(Sup, ChildId);
terminate(_, ChildPid, _, kill) ->
    Ref = erlang:monitor(process, ChildPid),
    exit(ChildPid, kill),
    receive
	{'DOWN', Ref, process, ChildPid, killed} ->
	    ok
    end;
terminate(_, ChildPid, _, shutdown) ->
    Ref = erlang:monitor(process, ChildPid),
    exit(ChildPid, shutdown),
    receive
	{'DOWN', Ref, process, ChildPid, shutdown} ->
	    ok
    end;
terminate(_, ChildPid, _, {shutdown, Term}) ->
    Ref = erlang:monitor(process, ChildPid),
    exit(ChildPid, {shutdown, Term}),
    receive
	{'DOWN', Ref, process, ChildPid, {shutdown, Term}} ->
	    ok
    end;
terminate(_, ChildPid, _, normal) ->
    Ref = erlang:monitor(process, ChildPid),
    ChildPid ! stop,
    receive
	{'DOWN', Ref, process, ChildPid, normal} ->
	    ok
    end;
terminate(_, ChildPid, _,abnormal) ->
    Ref = erlang:monitor(process, ChildPid),
    ChildPid ! die,
    receive
	{'DOWN', Ref, process, ChildPid, died} ->
	    ok
    end.

in_child_list([], _) ->
    true;
in_child_list([Pid | Rest], Pids) ->
    case is_in_child_list(Pid, Pids) of
	true ->
	    in_child_list(Rest, Pids);
	false ->
	    ct:fail(child_should_be_alive)
    end.
not_in_child_list([], _) ->
    true;
not_in_child_list([Pid | Rest], Pids) ->
    case is_in_child_list(Pid, Pids) of
	true ->
	    ct:fail(child_should_not_be_alive);
	false ->
	    not_in_child_list(Rest, Pids)
    end.

is_in_child_list(Pid, ChildPids) ->
    lists:member(Pid, ChildPids).

check_exit(Pids) ->
    check_exit(Pids, infinity).

check_exit([], _Timeout) ->
    ok;
check_exit([Pid | Pids], Timeout) ->
    receive
	{'EXIT', Pid, _} ->
	    check_exit(Pids, Timeout)
    after Timeout ->
	error
    end.

check_exit_reason(Pid, Reason) when is_pid(Pid) ->
    receive
	{'EXIT', Pid, Reason} ->
	    ok;
	{'EXIT', Pid, Else} ->
	    ct:fail({bad_exit_reason, Else})
    end.

check_no_exit(Timeout) ->
    receive
        {'EXIT', Pid, _} = Exit when is_pid(Pid) ->
            ct:fail({unexpected_message, Exit})
    after Timeout ->
            ok
    end.

start_registered_name(Name) ->
    supervisor:start_link({local, Name}, ?MODULE, []).

ensure_supervisor_is_stopped() ->
    case whereis(sup_test) of
        undefined ->
            ok;
        Pid ->
            terminate(Pid, shutdown)
    end.

%%-----------------------------------------------------------------
%% The Logger handler used.
%%-----------------------------------------------------------------
log(#{meta := #{mfa := {supervisor,do_restart,3}}}, #{test_case_pid := Pid}) ->
    Pid ! ok;
log(#{level := info, msg := Msg}, #{test_case_pid := Pid}) ->
    Pid ! {fail, Msg}.
