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
-module(release_handler_1).

%% External exports
-export([eval_script/1, eval_script/5,
	 check_script/2, check_old_processes/2]).
-export([get_current_vsn/1, get_supervised_procs/0]). %% exported because used in a test case

-record(eval_state, {bins = [], stopped = [], suspended = [], apps = [],
		     libdirs, unpurged = [], vsns = [], newlibs = [],
		     opts = []}).
%%-----------------------------------------------------------------
%% bins      = [{Mod, Binary, FileName}]
%% stopped   = [{Mod, [pid()]}] - list of stopped pids for each module
%% suspended = [{Mod, [pid()]}] - list of suspended pids for each module
%% apps      = [app_spec()] - list of all apps in the new release
%% libdirs   = [{Lib, LibVsn, LibDir}] - Maps Lib to Vsn and Directory
%% unpurged  = [{Mod, soft_purge | brutal_purge}]
%% vsns      = [{Mod, OldVsn, NewVsn}] - remember the old vsn of a mod
%%                  before a new vsn is loaded; the new vsn
%%                  is kept in case of a downgrade, where the code_change
%%                  function receives the vsn of the module to downgrade
%%                  *to*.
%% newlibs   = [{Lib, LibVsn, LibDir}] - list of all new libs; used to change
%%                            the code path
%% opts      = [{Tag, Value}] - list of options
%%-----------------------------------------------------------------


%%%-----------------------------------------------------------------
%%% This is a low-level release handler.
%%%-----------------------------------------------------------------
check_script([restart_new_emulator|Script], LibDirs) ->
    %% There is no need to check for old processes, since the node
    %% will be restarted before anything else happens.
    do_check_script(Script, LibDirs, []);
check_script(Script, LibDirs) ->
    case catch check_old_processes(Script,soft_purge) of
	{ok, PurgeMods} ->
	    do_check_script(Script, LibDirs, PurgeMods);
	{error, Mod} ->
	    {error, {old_processes, Mod}}
    end.

do_check_script(Script, LibDirs, PurgeMods) ->
    {Before, After} = split_instructions(Script),
    case catch lists:foldl(fun(Instruction, EvalState1) ->
				   eval(Instruction, EvalState1)
			   end,
			   #eval_state{libdirs = LibDirs},
			   Before) of
	       EvalState2 when is_record(EvalState2, eval_state) ->
		 case catch syntax_check_script(After) of
		     ok ->
			 {ok,PurgeMods};
		     Other ->
			 {error,Other}
		 end;
	       {error, Error} ->
		 {error, Error};
	       Other ->
		 {error, Other}
	 end.

%% eval_script/1 - For testing only - no apps added, just testing instructions
eval_script(Script) ->
    eval_script(Script, [], [], [], []).

eval_script(Script, Apps, LibDirs, NewLibs, Opts) ->
    case catch check_old_processes(Script,soft_purge) of
	{ok,_} ->
	    {Before, After} = split_instructions(Script),
	    case catch lists:foldl(fun(Instruction, EvalState1) ->
					   eval(Instruction, EvalState1)
				   end,
				   #eval_state{apps = Apps, 
					       libdirs = LibDirs,
					       newlibs = NewLibs,
					       opts = Opts},
				   Before) of
		       EvalState2 when is_record(EvalState2, eval_state) ->
			 case catch syntax_check_script(After) of
			     ok ->
				 case catch lists:foldl(
					      fun(Instruction, EvalState3) ->
						      eval(Instruction,
							   EvalState3)
					      end,
					      EvalState2,
					      After) of
				     EvalState4
				       when is_record(EvalState4, eval_state) ->
					 {ok, EvalState4#eval_state.unpurged};
				     restart_emulator ->
					 restart_emulator;
				     Error ->
					 {'EXIT', Error}
				 end;
			     Other ->
				 {error,Other}
			 end;
		       {error, Error} -> {error, Error};
		       Other -> {error, Other}
		 end;
	{error, Mod} ->
	    {error, {old_processes, Mod}}
    end.

%%%-----------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------
split_instructions(Script) ->
    split_instructions(Script, []).
split_instructions([point_of_no_return | T], Before) ->
    {lists:reverse(Before), [point_of_no_return | T]};
split_instructions([H | T], Before) ->
    split_instructions(T, [H | Before]);
split_instructions([], Before) ->
    {[], lists:reverse(Before)}.

%%-----------------------------------------------------------------
%% Func: check_old_processes/2
%% Args: Script = [instruction()]
%%       PrePurgeMethod = soft_purge | brutal_purge
%% Purpose: Check if there is any process that runs an old version
%%          of a module that should be purged according to PrePurgeMethod.
%%          Returns a list of modules that can be soft_purged.
%%
%%          If PrePurgeMethod == soft_purge, the function will succeed
%%          only if there is no process running old code of any of the
%%          modules. Else it will throw {error,Mod}, where Mod is the
%%          first module found that can not be soft_purged.
%%
%%          If PrePurgeMethod == brutal_purge, the function will
%%          always succeed and return a list of all modules that are
%%          specified in the script with PrePurgeMethod brutal_purge,
%%          but that can be soft_purged.
%%
%% Returns: {ok,PurgeMods} | {error, Mod}
%%          PurgeMods = [Mod]
%%          Mod = atom()  
%%-----------------------------------------------------------------
check_old_processes(Script,PrePurgeMethod) ->
    Procs = erlang:processes(),
    {ok,lists:flatmap(
	  fun({load, {Mod, PPM, _PostPurgeMethod}}) when PPM==PrePurgeMethod ->
		  check_old_code(Mod,Procs,PrePurgeMethod);
	     ({remove, {Mod, PPM, _PostPurgeMethod}}) when PPM==PrePurgeMethod ->
		  check_old_code(Mod,Procs,PrePurgeMethod);
	     (_) -> []
	  end,
	  Script)}.

check_old_code(Mod,Procs,PrePurgeMethod) ->
    case erlang:check_old_code(Mod) of
	true when PrePurgeMethod==soft_purge ->
	    do_check_old_code(Mod,Procs);
	true when PrePurgeMethod==brutal_purge ->
	    case catch do_check_old_code(Mod,Procs) of
		{error,Mod} -> [];
		R -> R
	    end;
	false ->
	    []
    end.


do_check_old_code(Mod,Procs) ->
    lists:foreach(
      fun(Pid) ->
	      case erlang:check_process_code(Pid, Mod) of
		  false -> ok;
		  true -> throw({error, Mod})
	      end
      end,
      Procs),
    [Mod].


%% Check the last part of the script, i.e. the part after point_of_no_return.
%% More or less a syntax check in case the script was handwritten.
syntax_check_script([point_of_no_return | Script]) ->
    syntax_check_script(Script);
syntax_check_script([{load, {_,_,_}} | Script]) ->
    syntax_check_script(Script);
syntax_check_script([{remove, {_,_,_}} | Script]) ->
    syntax_check_script(Script);
syntax_check_script([{purge, _} | Script]) ->
    syntax_check_script(Script);
syntax_check_script([{suspend, _} | Script]) ->
    syntax_check_script(Script);
syntax_check_script([{resume, _} | Script]) ->
    syntax_check_script(Script);
syntax_check_script([{code_change, _} | Script]) ->
    syntax_check_script(Script);
syntax_check_script([{code_change, _, _} | Script]) ->
    syntax_check_script(Script);
syntax_check_script([{stop, _} | Script]) ->
    syntax_check_script(Script);
syntax_check_script([{start, _} | Script]) ->
    syntax_check_script(Script);
syntax_check_script([{sync_nodes, _, {_,_,_}} | Script]) ->
    syntax_check_script(Script);
syntax_check_script([{sync_nodes, _, _} | Script]) ->
    syntax_check_script(Script);
syntax_check_script([{apply, {_,_,_}} | Script]) ->
    syntax_check_script(Script);
syntax_check_script([restart_emulator | Script]) ->
    syntax_check_script(Script);
syntax_check_script([Illegal | _Script]) ->
    throw({illegal_instruction_after_point_of_no_return,Illegal});
syntax_check_script([]) ->
    ok.


%%-----------------------------------------------------------------
%% An unpurged module is a module for which there exist an old
%% version of the code.  This should only be the case if there are
%% processes running the old version of the code.
%%
%% This functions evaluates each instruction.  Note that the
%% instructions here are low-level instructions.  e.g. lelle's
%% old synchronized_change would be translated to
%%   {load_object_code, Modules},
%%   {suspend, Modules}, [{load, Module}],
%%   {resume, Modules}, {purge, Modules}
%% Or, for example, if we want to do advanced external code change 
%% on two modules that depend on each other, by killing them and
%% then restaring them, we could do:
%%   {load_object_code, [Mod1, Mod2]},
%%   % delete old version
%%   {remove, {Mod1, brutal_purge}}, {remove, {Mod2, brutal_purge}},
%%   % now, some procs migth be running prev current (now old) version
%%   % kill them, and load new version
%%   {load, {Mod1, brutal_purge}}, {load, {Mod2, brutal_purge}}
%%   % now, there is one version of the code (new, current)
%%
%% NOTE: All load_object_code must be first in the script,
%%       a point_of_no_return must be present (if load_object_code
%%       is present).
%%
%% {load_object_code, {Lib, LibVsn, [Mod]} 
%%    read the files as binarys. do not make code out of them
%% {load, {Module, PrePurgeMethod, PostPurgeMethod}}
%%    Module must have been load_object_code:ed.  make code out of it
%%    old procs && soft_purge => no new release
%%    old procs && brutal_purge => old procs killed
%%    The new old code will be gc:ed later on, if PostPurgeMethod =
%%    soft_purge.  If it is brutal_purge, the code is purged when
%%    the release is made permanent.
%% {remove, {Module, PrePurgeMethod, PostPurgeMethod}}
%%    make current version old.  no current left.
%%    old procs && soft_purge => no new release
%%    old procs && brutal_purge => old procs killed
%%    The new old code will be gc:ed later on, if PostPurgeMethod =
%%    soft_purge.  If it is brutal_purge, the code is purged when
%%    the release is made permanent.
%% {purge, Modules}
%%    kill all procs running old code, delete old code
%% {suspend, [Module | {Module, Timeout}]}
%%    If a process doesn't repsond - never mind.  It will be killed
%%    later on (if a purge is performed).
%%    Hmm, we must do something smart here... we should probably kill it,
%%    but we cant, because its supervisor will restart it directly!  Maybe
%%    we should keep a list of those, call supervisor:terminate_child()
%%    when all others are suspended, and call sup:restart_child() when the
%%    others are resumed.
%% {code_change, [{Module, Extra}]}
%% {code_change, Mode, [{Module, Extra}]}  Mode = up | down
%%    Send code_change only to suspended procs running this code
%% {resume, [Module]}
%%    resume all previously suspended processes
%% {stop, [Module]}
%%    stop all procs running this code
%% {start, [Module]}
%%    starts the procs that were previously stopped for this code.
%%    Note that this will start processes in exactly the same place
%%    in the suptree where there were procs previously.
%% {sync_nodes, Id, [Node]}
%% {sync_nodes, Id, {M, F, A}}
%%    Synchronizes with the Nodes (or apply(M,F,A) == Nodes).  All Nodes
%%    must also exectue the same line.  Waits for all these nodes to get
%%    to this line.
%% point_of_no_return
%% restart_emulator
%% {stop_application, Appl}     - Impl with apply
%% {unload_application, Appl}   - Impl with {remove..}
%% {load_application, Appl}     - Impl with {load..}
%% {start_application, Appl}    - Impl with apply
%%-----------------------------------------------------------------
eval({load_object_code, {Lib, LibVsn, Modules}}, EvalState) ->
    case lists:keysearch(Lib, 1, EvalState#eval_state.libdirs) of
	{value, {Lib, LibVsn, LibDir} = LibInfo} ->
	    Ext = code:objfile_extension(),
	    {NewBins, NewVsns} = 
		lists:foldl(fun(Mod, {Bins, Vsns}) ->
				    File = lists:concat([Mod, Ext]),
				    FName = filename:join([LibDir, "ebin", File]),
				    case erl_prim_loader:get_file(FName) of
					{ok, Bin, FName2} ->
					    NVsns = add_vsns(Mod, Bin, Vsns),
					    {[{Mod, Bin, FName2} | Bins],NVsns};
					error ->
					    throw({error, {no_such_file,FName}})
				    end
			    end,
			    {EvalState#eval_state.bins,
			     EvalState#eval_state.vsns},
			    Modules),
	    NewLibs = lists:keystore(Lib,1,EvalState#eval_state.newlibs,LibInfo),
	    EvalState#eval_state{bins = NewBins,
				 newlibs = NewLibs,
				 vsns = NewVsns};
	{value, {Lib, LibVsn2, _LibDir}} ->
	    throw({error, {bad_lib_vsn, Lib, LibVsn2}})
    end;
eval(point_of_no_return, EvalState) ->
    Libs = case get_opt(update_paths, EvalState, false) of
	       false ->
		   EvalState#eval_state.newlibs;
	       true ->
		   EvalState#eval_state.libdirs
	   end,
    lists:foreach(fun({Lib, _LibVsn, LibDir}) ->
			  Ebin = filename:join(LibDir,"ebin"),
			  code:replace_path(Lib, Ebin)
		  end,
		  Libs),
    EvalState;
eval({load, {Mod, _PrePurgeMethod, PostPurgeMethod}}, EvalState) ->
    Bins = EvalState#eval_state.bins,
    {value, {_Mod, Bin, File}} = lists:keysearch(Mod, 1, Bins),
    % load_binary kills all procs running old code
    % if soft_purge, we know that there are no such procs now
    {module,_} = code:load_binary(Mod, File, Bin),
    % Now, the prev current is old.  There might be procs
    % running it.  Find them.
    Unpurged = do_soft_purge(Mod,PostPurgeMethod,EvalState#eval_state.unpurged),
    EvalState#eval_state{bins = lists:keydelete(Mod, 1, Bins),
			 unpurged = Unpurged};
eval({remove, {Mod, _PrePurgeMethod, PostPurgeMethod}}, EvalState) ->
    %% purge kills all procs running old code
    %% if soft_purge, we know that there are no such procs now
    code:purge(Mod),
    code:delete(Mod),
    %% Now, the prev current is old.  There might be procs
    %% running it.  Find them.
    Unpurged = do_soft_purge(Mod,PostPurgeMethod,EvalState#eval_state.unpurged),
    EvalState#eval_state{unpurged = Unpurged};
eval({purge, Modules}, EvalState) ->
    % Now, if there are any processes still executing old code, OR
    % if some new processes started after suspend but before load,
    % these are killed.
    lists:foreach(fun(Mod) -> code:purge(Mod) end, Modules),
    EvalState;
eval({suspend, Modules}, EvalState) ->
    Procs = get_supervised_procs(),
    NewSuspended =
	lists:foldl(fun(ModSpec, Suspended) ->
			    {Module, Def} = case ModSpec of 
						{Mod, ModTimeout} ->
						    {Mod, ModTimeout};
						Mod ->
						    {Mod, default}
					    end,
			    Timeout = get_opt(suspend_timeout, EvalState, Def),
			    Pids = suspend(Module, Procs, Timeout),
			    [{Module, Pids} | Suspended]
		    end,
		    EvalState#eval_state.suspended,
		    Modules),
    EvalState#eval_state{suspended = NewSuspended};
eval({resume, Modules}, EvalState) ->
    NewSuspended =
	lists:foldl(fun(Mod, Suspended) ->
			    lists:filter(fun({Mod2, Pids}) when Mod2 == Mod ->
						 resume(Pids),
						 false;
					    (_) ->
						 true
					 end,
					 Suspended)
		    end,
		    EvalState#eval_state.suspended,
		    Modules),
    EvalState#eval_state{suspended = NewSuspended};
eval({code_change, Modules}, EvalState) ->
    eval({code_change, up, Modules}, EvalState);
eval({code_change, Mode, Modules}, EvalState) ->
    Suspended = EvalState#eval_state.suspended,
    Vsns = EvalState#eval_state.vsns,
    Timeout = get_opt(code_change_timeout, EvalState, default),
    lists:foreach(fun({Mod, Extra}) ->
			  Vsn =
			      case lists:keysearch(Mod, 1, Vsns) of
				  {value, {Mod, OldVsn, _NewVsn}}
				    when Mode == up -> OldVsn;
				  {value, {Mod, _OldVsn, NewVsn}}
				    when Mode == down -> {down, NewVsn};
				  _ when Mode == up -> undefined;
				  _ -> {down, undefined}
			      end,
			  case lists:keysearch(Mod, 1, Suspended) of
			      {value, {_Mod, Pids}} ->
				  change_code(Pids, Mod, Vsn, Extra, Timeout);
			      _ -> ok
			  end
		  end,
		  Modules),
    EvalState;
eval({stop, Modules}, EvalState) ->
    Procs = get_supervised_procs(),
    NewStopped =
	lists:foldl(fun(Mod, Stopped) ->
			    Procs2 = stop(Mod, Procs),
			    [{Mod, Procs2} | Stopped]
		    end,
		    EvalState#eval_state.stopped,
		    Modules),
    EvalState#eval_state{stopped = NewStopped};
eval({start, Modules}, EvalState) ->
    NewStopped =
	lists:foldl(fun(Mod, Stopped) ->
			    lists:filter(fun({Mod2, Procs}) when Mod2 == Mod ->
						 start(Procs),
						 false;
					    (_) ->
						 true
					 end,
					 Stopped)
		    end,
		    EvalState#eval_state.stopped,
		    Modules),
    EvalState#eval_state{stopped = NewStopped};
eval({sync_nodes, Id, {M, F, A}}, EvalState) ->
    sync_nodes(Id, apply(M, F, A)),
    EvalState;
eval({sync_nodes, Id, Nodes}, EvalState) ->
    sync_nodes(Id, Nodes),
    EvalState;
eval({apply, {M, F, A}}, EvalState) ->
    apply(M, F, A),
    EvalState;
eval(restart_emulator, _EvalState) ->
    throw(restart_emulator);
eval(restart_new_emulator, _EvalState) ->
    throw(restart_new_emulator).

get_opt(Tag, EvalState, Default) ->
    case lists:keysearch(Tag, 1, EvalState#eval_state.opts) of
	{value,  {_Tag, Value}} -> Value;
	false                   -> Default
    end.

%%-----------------------------------------------------------------
%% This is a first approximation.  Unfortunately, we might end up
%% with the situation that after this suspendation, some new
%% processes start *before* we have loaded the new code, and these
%% will execute the old code.  These processes could be terminated
%% later on (if the prev current version is purged).  The same
%% goes for processes that didn't respond to the suspend message.
%%-----------------------------------------------------------------
suspend(Mod, Procs, Timeout) ->
    lists:zf(fun({_Sup, _Name, Pid, Mods}) -> 
		     case lists:member(Mod, Mods) of
			 true ->
			     case catch sys_suspend(Pid, Timeout) of
				 ok -> {true, Pid};
				 _ -> 
				     % If the proc hangs, make sure to
				     % resume it when it gets suspended!
				     catch sys:resume(Pid),
				     false
			     end;
			 false ->
			     false
		     end
	     end,
	     Procs).

sys_suspend(Pid, default) ->
    sys:suspend(Pid);
sys_suspend(Pid, Timeout) ->
    sys:suspend(Pid, Timeout).

resume(Pids) ->
    lists:foreach(fun(Pid) -> catch sys:resume(Pid) end, Pids).

change_code(Pids, Mod, Vsn, Extra, Timeout) ->
    Fun = fun(Pid) -> 
		  case sys_change_code(Pid, Mod, Vsn, Extra, Timeout) of
		      ok ->
			  ok;
		      {error,Reason} ->
			  throw({code_change_failed,Pid,Mod,Vsn,Reason})
		  end
	  end,
    lists:foreach(Fun, Pids).

sys_change_code(Pid, Mod, Vsn, Extra, default) ->
    sys:change_code(Pid, Mod, Vsn, Extra);
sys_change_code(Pid, Mod, Vsn, Extra, Timeout) ->
    sys:change_code(Pid, Mod, Vsn, Extra, Timeout).

stop(Mod, Procs) ->
    lists:zf(fun({undefined, _Name, _Pid, _Mods}) ->
		     false;
		({Sup, Name, _Pid, Mods}) -> 
		     case lists:member(Mod, Mods) of
			 true ->
			     case catch supervisor:terminate_child(
					  Sup, Name) of
				 ok -> {true, {Sup, Name}};
				 _ -> false
			     end;
			 false -> false
		     end
	     end,
	     Procs).

start(Procs) ->
    lists:foreach(fun({Sup, Name}) -> 
			  catch supervisor:restart_child(Sup, Name)
		  end,
		  Procs).

%%-----------------------------------------------------------------
%% Func: get_supervised_procs/0
%% Purpose: This is the magic function.  It finds all process in
%%          the system and which modules they execute as a call_back or
%%          process module.
%%          This is achieved by asking the main supervisor for the
%%          applications for all children and their modules
%%          (recursively).
%% NOTE: If a supervisor is suspended, it isn't possible to call
%%       which_children.  Code change on a supervisor should be
%%       done in another way; the only code in a supervisor is
%%       code for starting children.  Therefore, to change a
%%       supervisor module, we should load the new version, and then
%%       delete the old.  Then we should perform the start changes
%%       manually, by adding/deleting children.
%%
%%       Recent changes to this code cause the upgrade error out and
%%       log the case where a suspended supervisor has which_children
%%       called against it. This retains the behavior of causing a VM
%%       restart to the *old* version of a release but has the
%%       advantage of logging the pid and supervisor that had the
%%       issue.
%%
%%       A second case where this can occur is if a child spec is
%%       incorrect and get_modules is called against a process that
%%       can't respond to the gen:call. Again an error is logged,
%%       an error returned and a VM restart is issued.
%%
%% Returns: [{SuperPid, ChildName, ChildPid, Mods}]
%%-----------------------------------------------------------------
%% OTP-3452. For each application the first item contains the pid
%% of the top supervisor, and the name of the supervisor call-back module.  
%%-----------------------------------------------------------------

get_supervised_procs() ->
    lists:foldl(
      fun(Application, Procs) ->
              get_master_procs(Application,
                               Procs,
                               application_controller:get_master(Application))
      end,
      [],
      get_application_names()).

get_supervised_procs(_, Root, Procs, {ok, SupMod}) ->
    get_procs(maybe_supervisor_which_children(Root, SupMod, Root), Root) ++
        [{undefined, undefined, Root, [SupMod]} |  Procs];
get_supervised_procs(Application, Root, Procs, {error, _}) ->
    error_logger:error_msg("release_handler: cannot find top supervisor for "
                           "application ~w~n", [Application]),
    get_procs(maybe_supervisor_which_children(Root, Application, Root), Root) ++ Procs.

get_application_names() ->
    lists:map(fun({Application, _Name, _Vsn}) ->
                      Application
              end,
              application:which_applications()).

get_master_procs(Application, Procs, Pid) when is_pid(Pid) ->
    {Root, _AppMod} = application_master:get_child(Pid),
    get_supervised_procs(Application, Root, Procs, get_supervisor_module(Root));
get_master_procs(_, Procs, _) ->
    Procs.

get_procs([{Name, Pid, worker, dynamic} | T], Sup) when is_pid(Pid) ->
    Mods = maybe_get_dynamic_mods(Name, Pid),
    [{Sup, Name, Pid, Mods} | get_procs(T, Sup)];
get_procs([{Name, Pid, worker, Mods} | T], Sup) when is_pid(Pid), is_list(Mods) ->
    [{Sup, Name, Pid, Mods} | get_procs(T, Sup)];
get_procs([{Name, Pid, supervisor, Mods} | T], Sup) when is_pid(Pid) ->
    [{Sup, Name, Pid, Mods} | get_procs(T, Sup)] ++
        get_procs(maybe_supervisor_which_children(Pid, Name, Pid), Pid);
get_procs([_H | T], Sup) ->
    get_procs(T, Sup);
get_procs(_, _Sup) ->
    [].

maybe_supervisor_which_children(Proc, Name, Pid) ->
    case get_proc_state(Proc) of
        noproc ->
            %% process exited before we could interrogate it.
            %% not necessarily a bug, but reporting a warning as a curiosity.
            error_logger:warning_msg("release_handler: a process (~p) exited"
                                     " during supervision tree interrogation."
                                     " Continuing ...~n", [Proc]),
            [];

        suspended ->
            error_logger:error_msg("release_handler: a which_children call"
                                   " to ~p (~w) was avoided. This supervisor"
                                   " is suspended and should likely be upgraded"
                                   " differently. Exiting ...~n", [Name, Pid]),
            error(suspended_supervisor);

        running ->
            case catch supervisor:which_children(Pid) of
                Res when is_list(Res) ->
                    Res;
                Other ->
                    error_logger:error_msg("release_handler: ~p~nerror during"
                                           " a which_children call to ~p (~w)."
                                           " [State: running] Exiting ... ~n",
                                           [Other, Name, Pid]),
                    error(which_children_failed)
            end
    end.

get_proc_state(Proc) ->
    %% sys:send_system_msg can exit with {noproc, {m,f,a}}.
    %% This happens if a supervisor exits after which_children has provided
    %% its pid for interrogation.
    %% ie. Proc may no longer be running at this point.
    try sys:get_status(Proc) of
        %% as per sys:get_status/1, SysState can only be running | suspended.
        {status, _, {module, _}, [_, State, _, _, _]} when State == running ;
                                                           State == suspended ->
            State
    catch exit:{noproc, {sys, get_status, [Proc]}} ->
        noproc
    end.

maybe_get_dynamic_mods(Name, Pid) ->
    case catch gen:call(Pid, self(), get_modules) of
        {ok, Res} ->
            Res;
        Other ->
            error_logger:error_msg("release_handler: ~p~nerror during a"
                                   " get_modules call to ~p (~w),"
                                   " there may be an error in it's"
                                   " childspec. Exiting ...~n",
                                   [Other, Name, Pid]),
            error(get_modules_failed)
    end.

%% Return the name of the call-back module that implements the
%% (top) supervisor SupPid.
%% Returns: {ok, Module} | {error,undefined}
%%
get_supervisor_module(SupPid) ->
    case catch supervisor:get_callback_module(SupPid) of
	Module when is_atom(Module) ->
	    {ok, Module};
	_Other ->
	    io:format("~w: reason: ~w~n", [SupPid, _Other]),
	    {error, undefined}
    end.

%%-----------------------------------------------------------------
%% Func: do_soft_purge/3
%% Args: Mod = atom()
%%       PostPurgeMethod = soft_purge | brutal_purge
%%       Unpurged = [{Mod, PostPurgeMethod}]
%% Purpose: Check if there are any processes left running this code.
%%          If so, make sure Mod is a member in the returned list.
%%          Otherwise, make sure Mod isn't a member in the returned
%%          list.
%% Returns: An updated list of unpurged modules.
%%-----------------------------------------------------------------
do_soft_purge(Mod, PostPurgeMethod, Unpurged) ->
    IsNoOldProcsLeft = code:soft_purge(Mod),
    case lists:keymember(Mod, 1, Unpurged) of
	true when IsNoOldProcsLeft == true -> lists:keydelete(Mod, 1, Unpurged);
	true -> Unpurged;
	false when IsNoOldProcsLeft == true -> Unpurged;
	false -> [{Mod, PostPurgeMethod} | Unpurged]
    end.

%%-----------------------------------------------------------------
%% Func: sync_nodes/2
%% Args: Id = term()
%%       Nodes = [atom()]
%% Purpose: Synchronizes with all nodes.
%%-----------------------------------------------------------------
sync_nodes(Id, Nodes) ->
    NNodes = lists:delete(node(), Nodes),
    lists:foreach(fun(Node) ->
			  {release_handler, Node} ! {sync_nodes, Id, node()}
		  end,
		  NNodes),
    lists:foreach(fun(Node) ->
			  receive
			      {sync_nodes, Id, Node} ->
				  ok;
			      {nodedown, Node} ->
				  throw({sync_error, {nodedown, Node}})
			  end
		  end,
		  NNodes).

add_vsns(Mod, NewBin, Vsns) ->
    OldVsn = get_current_vsn(Mod),
    NewVsn = get_vsn(NewBin),
    case lists:keysearch(Mod, 1, Vsns) of
	{value, {Mod, OldVsn0, NewVsn0}} ->
	    lists:keyreplace(Mod, 1, Vsns, {Mod,
					    replace_undefined(OldVsn0,OldVsn),
					    replace_undefined(NewVsn0,NewVsn)});
	false ->
	    [{Mod, OldVsn, NewVsn} | Vsns]
    end.

replace_undefined(undefined,Vsn) -> Vsn;
replace_undefined(Vsn,_) -> Vsn.

%%-----------------------------------------------------------------
%% Func: get_current_vsn/1
%% Args: Mod = atom()
%% Purpose: This function returns the equivalent of 
%%   beam_lib:version(code:which(Mod)), but it will also handle the
%%   case when using erl_prim_loader loader different from 'efile'.
%%   The reason for not using the Binary from the 'bins' or the
%%   version directly from the 'vsns' state field is that these are
%%   updated already by load_object_code, and this function is called
%%   from load and remove.
%% Returns: Vsn = term()
%%-----------------------------------------------------------------
get_current_vsn(Mod) ->
    File = code:which(Mod),
    case erl_prim_loader:get_file(File) of
	{ok, Bin, _File2} ->
	    get_vsn(Bin);
	error ->
	    %% This is the case when a new module is added, there will
	    %% be no current version of it at the time of this call.
	    undefined
    end.

%%-----------------------------------------------------------------
%% Func: get_vsn/1
%% Args: Bin = binary()
%% Purpose: Finds the version attribute of a module.
%% Returns: Vsn = term()
%%-----------------------------------------------------------------
get_vsn(Bin) ->
    {ok, {_Mod, Vsn}} = beam_lib:version(Bin),
    case misc_supp:is_string(Vsn) of
	true ->
	    Vsn;
	false ->
	    %% If -vsn(Vsn) defines a term which is not a
	    %% string, the value is returned here as [Vsn].
	    case Vsn of
		[VsnTerm] ->
		    VsnTerm;
		_ ->
		    Vsn
	    end
    end.
