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
-module(application_master).

%% External exports
-export([start_link/2, start_type/0, stop/1]).
-export([get_child/1]).

%% Internal exports
-export([init/4, start_it/4]).

-include("application_master.hrl").

-record(state, {child, appl_data, children = [], procs = 0, gleader, req=[]}).

%%-----------------------------------------------------------------
%% Func: start_link/1
%% Args: ApplData = record(appl_data)
%% Purpose: Starts an application master for the application.
%%          Called from application_controller.  (The application is
%%          also started).
%% Returns: {ok, Pid} | {error, Reason} (Pid is unregistered)
%%-----------------------------------------------------------------
start_link(ApplData, Type) ->
    Parent = whereis(application_controller),
    proc_lib:start_link(application_master, init,
			[Parent, self(), ApplData, Type]).

start_type() ->
    group_leader() ! {start_type, self()},
    receive
	{start_type, Type} ->
	    Type
    after 5000 ->
	    {error, timeout}
    end.

%%-----------------------------------------------------------------
%% Func: stop/1
%% Purpose: Stops the application.  This function makes sure
%%          that all processes belonging to the applicication is
%%          stopped (shutdown or killed).  The application master
%%          is also stopped.
%% Returns: ok
%%-----------------------------------------------------------------
stop(AppMaster) -> call(AppMaster, stop).

%%-----------------------------------------------------------------
%% Func: get_child/1
%% Purpose: Get the topmost supervisor of an application.
%% Returns: {pid(), App}
%%-----------------------------------------------------------------
get_child(AppMaster) -> call(AppMaster, get_child).
    
call(AppMaster, Req) ->
    Tag = make_ref(),
    Ref = erlang:monitor(process, AppMaster),
    AppMaster ! {Req, Tag, self()},
    receive 
	{'DOWN', Ref, process, _, _Info} ->
	    ok;
	{Tag, Res} ->
	    erlang:demonitor(Ref, [flush]),
	    Res
    end.

%%%-----------------------------------------------------------------
%%% The logical and physical process structrure is as follows:
%%%
%%%         logical                physical
%%%
%%%         --------               --------
%%%         |AM(GL)|               |AM(GL)|               
%%%         --------               -------- 
%%%            |                       |
%%%         --------               --------
%%%         |Appl P|               |   X  |
%%%         --------               --------
%%%                                    |
%%%                                --------
%%%                                |Appl P|
%%%                                --------
%%%
%%% Where AM(GL) == Application Master (Group Leader)
%%%       Appl P == The application specific root process (child to AM)
%%%       X      == A special 'invisible' process
%%% The reason for not using the logical structrure is that
%%% the application start function is synchronous, and 
%%% that the AM is GL.  This means that if AM executed the start
%%% function, and this function uses io, deadlock would occur.  
%%% Therefore, this function is executed by the process X. 
%%% Also, AM needs three loops;
%%% init_loop (waiting for the start function to return)
%%% main_loop
%%% terminate_loop (waiting for the process to die)
%%% In each of these loops, io and other requests are handled.
%%%-----------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------
init(Parent, Starter, ApplData, Type) ->
    link(Parent),
    process_flag(trap_exit, true),
    OldGleader = group_leader(),
    group_leader(self(), self()),
    %% Insert ourselves as master for the process.  This ensures that
    %% the processes in the application can use get_env/1 at startup.
    Name = ApplData#appl_data.name,
    ets:insert(ac_tab, {{application_master, Name}, self()}),
    State = #state{appl_data = ApplData, gleader = OldGleader},
    case start_it(State, Type) of
	{ok, Pid} ->          % apply(M,F,A) returned ok
	    ok = set_timer(ApplData#appl_data.maxT),
	    unlink(Starter),
	    proc_lib:init_ack(Starter, {ok,self()}),
	    main_loop(Parent, State#state{child = Pid});
	{error, Reason} ->    % apply(M,F,A) returned error
	    exit(Reason);
	Else ->               % apply(M,F,A) returned erroneous
	    exit(Else)
    end.

%%-----------------------------------------------------------------
%% We want to start the new application synchronously, but we still
%% want to handle io requests.  So we spawn off a new process that
%% performs the apply, and we wait for a start ack.
%%-----------------------------------------------------------------
start_it(State, Type) ->
    Tag = make_ref(),
    Pid = spawn_link(application_master, start_it, [Tag, State, self(), Type]),
    init_loop(Pid, Tag, State, Type).


%%-----------------------------------------------------------------
%% These are the three different loops executed by the application_
%% master
%%-----------------------------------------------------------------
init_loop(Pid, Tag, State, Type) ->
    receive
 	IoReq when element(1, IoReq) =:= io_request ->
	    State#state.gleader ! IoReq,
	    init_loop(Pid, Tag, State, Type);
	{Tag, Res} ->
	    Res;
	{'EXIT', Pid, Reason} ->
	    {error, Reason};
	{start_type, From} ->
	    From ! {start_type, Type},
	    init_loop(Pid, Tag, State, Type);
	Other ->
	    NewState = handle_msg(Other, State),
	    init_loop(Pid, Tag, NewState, Type)
    end.

main_loop(Parent, State) ->
    receive
	IoReq when element(1, IoReq) =:= io_request ->
	    State#state.gleader ! IoReq,
	    main_loop(Parent, State);
	{'EXIT', Parent, Reason} ->
	    terminate(Reason, State);
	{'EXIT', Child, Reason} when State#state.child =:= Child ->
	    terminate(Reason, State#state{child=undefined});
	{'EXIT', _, timeout} ->
	    terminate(normal, State);
	{'EXIT', Pid, _Reason} ->
	    Children = lists:delete(Pid, State#state.children),
	    Procs = State#state.procs - 1,
	    main_loop(Parent, State#state{children=Children, procs=Procs});
	{start_type, From} ->
	    From ! {start_type, local},
	    main_loop(Parent, State);
	Other ->
	    NewState = handle_msg(Other, State),
	    main_loop(Parent, NewState)
    end.

terminate_loop(Child, State) ->
    receive
 	IoReq when element(1, IoReq) =:= io_request ->
	    State#state.gleader ! IoReq,
	    terminate_loop(Child, State);
	{'EXIT', Child, _} ->
	    ok;
	Other ->
	    NewState = handle_msg(Other, State),
	    terminate_loop(Child, NewState)
    end.


%%-----------------------------------------------------------------
%% The Application Master is linked to *all* processes in the group
%% (application).
%%-----------------------------------------------------------------
handle_msg({get_child, Tag, From}, State) ->
    get_child_i(State, Tag, From);
handle_msg({stop, Tag, From}, State) ->
    catch terminate(normal, State),
    From ! {Tag, ok},
    exit(normal);
handle_msg({child, Ref, GrandChild, Mod}, #state{req=Reqs0}=State) ->
    {value, {_, Tag, From}, Reqs} = lists:keytake(Ref, 1, Reqs0),
    From ! {Tag, {GrandChild, Mod}},
    State#state{req=Reqs};
handle_msg(_, State) ->
    State.

terminate(Reason, State = #state{child=Child, children=Children, req=Reqs}) ->
    _ = [From ! {Tag, error} || {_, Tag, From} <- Reqs],
    terminate_child(Child, State),
    kill_children(Children),
    exit(Reason).




%%======================================================================
%%======================================================================
%%======================================================================
%% This is the process X above...
%%======================================================================
%%======================================================================
%%======================================================================

%%======================================================================
%% Start an application.
%% If the start_phases is defined in the .app file, the application is
%% to be started in one or several start phases.
%% If the Module in the mod-key is set to application_starter then
%% the generic help module application_starter is used to control
%% the start.
%%======================================================================

start_it(Tag, State, From, Type) ->
    process_flag(trap_exit, true),
    ApplData = State#state.appl_data,
    case {ApplData#appl_data.phases, ApplData#appl_data.mod} of
	{undefined, _} ->
	    start_it_old(Tag, From, Type, ApplData);
	{Phases, {application_starter, [M, A]}} ->
	    start_it_new(Tag, From, Type, M, A, Phases, 
			 [ApplData#appl_data.name]);
	{Phases, {M, A}} ->
	    start_it_new(Tag, From, Type, M, A, Phases, 
			 [ApplData#appl_data.name]);
	{OtherP, OtherM} ->
	    From ! {Tag, {error, {bad_keys, {{mod, OtherM}, 
					     {start_phases, OtherP}}}}}
    end.


%%%-----------------------------------------------------
%%% No start phases are defined
%%%-----------------------------------------------------
start_it_old(Tag, From, Type, ApplData) ->
    {M,A} = ApplData#appl_data.mod,
    case catch M:start(Type, A) of
	{ok, Pid} ->
	    link(Pid),
	    From ! {Tag, {ok, self()}},
	    loop_it(From, Pid, M, []);
	{ok, Pid, AppState} ->
	    link(Pid),
	    From ! {Tag, {ok, self()}},
	    loop_it(From, Pid, M, AppState);
	{'EXIT', normal} ->
	    From ! {Tag, {error, {{'EXIT',normal},{M,start,[Type,A]}}}};
	{error, Reason} ->
	    From ! {Tag, {error, {Reason, {M,start,[Type,A]}}}};
	Other ->
	    From ! {Tag, {error, {bad_return,{{M,start,[Type,A]},Other}}}}
    end.


%%%-----------------------------------------------------
%%% Start phases are defined
%%%-----------------------------------------------------
start_it_new(Tag, From, Type, M, A, Phases, Apps) ->
    case catch start_the_app(Type, M, A, Phases, Apps) of
	{ok, Pid, AppState} ->
	    From ! {Tag, {ok, self()}},
	    loop_it(From, Pid, M, AppState);    
	Error ->
	    From ! {Tag, Error}
    end.


%%%=====================================================
%%% Start the application in the defined phases, 
%%% but first the supervisors are starter.
%%%=====================================================
start_the_app(Type, M, A, Phases, Apps) ->
    case start_supervisor(Type, M, A) of
 	{ok, Pid, AppState} ->
	    link(Pid),
	    case application_starter:start(Phases, Type, Apps) of
		ok ->
		    {ok, Pid, AppState};
		Error2 ->
		    unlink(Pid),
		    Error2
	    end;
	Error ->
	    Error
    end.

%%%-------------------------------------------------------------
%%% Start the supervisors
%%%-------------------------------------------------------------
start_supervisor(Type, M, A) ->
    case catch M:start(Type, A) of
	{ok, Pid} ->
	    {ok, Pid, []};
	{ok, Pid, AppState} ->
	    {ok, Pid, AppState};
	{error, Reason} ->
	    {error, {Reason, {M, start, [Type, A]}}};
	{'EXIT', normal} ->
	    {error, {{'EXIT', normal}, {M, start, [Type, A]}}};
	Other ->
	    {error, {bad_return, {{M, start, [Type, A]}, Other}}}
    end.




%%======================================================================
%%
%%======================================================================

loop_it(Parent, Child, Mod, AppState) ->
    receive
	{Parent, get_child, Ref} ->
	    Parent ! {child, Ref, Child, Mod},
	    loop_it(Parent, Child, Mod, AppState);
	{Parent, terminate} ->
	    NewAppState = prep_stop(Mod, AppState),
	    exit(Child, shutdown),
	    receive
		{'EXIT', Child, _} -> ok
	    end,
	    catch Mod:stop(NewAppState),
	    exit(normal);
	{'EXIT', Parent, Reason} ->
	    NewAppState = prep_stop(Mod, AppState),
	    exit(Child, Reason),
	    receive
		{'EXIT', Child, Reason2} ->
		    exit(Reason2)
	    end,
	    catch Mod:stop(NewAppState);
	{'EXIT', Child, Reason} -> % forward *all* exit reasons (inc. normal)
	    NewAppState = prep_stop(Mod, AppState),
	    catch Mod:stop(NewAppState),
	    exit(Reason);
	_ ->
	    loop_it(Parent, Child, Mod, AppState)
    end.

prep_stop(Mod, AppState) ->
    case catch Mod:prep_stop(AppState) of
	{'EXIT', {undef, _}} ->
	    AppState;
	{'EXIT', Reason} ->
	    error_logger:error_report([{?MODULE, shutdown_error},
				       {Mod, {prep_stop, [AppState]}},
				       {error_info, Reason}]),
	    AppState;
	NewAppState ->
	    NewAppState
    end.

get_child_i(#state{child=Child, req=Reqs}=State, Tag, From) ->
    Ref = erlang:make_ref(),
    case erlang:is_process_alive(Child) of
	true ->
	    Child ! {self(), get_child, Ref},
	    State#state{req=[{Ref, Tag, From}|Reqs]};
	false ->
	    From ! {Tag, error},
	    State
    end.

terminate_child_i(Child, State) ->
    Child ! {self(), terminate},
    terminate_loop(Child, State).

%% Try to shutdown the child gently
terminate_child(undefined, _) -> ok;
terminate_child(Child, State) ->
    terminate_child_i(Child, State).

kill_children(Children) ->
    lists:foreach(fun(Pid) -> exit(Pid, kill) end, Children),
    kill_all_procs().

kill_all_procs() ->
    kill_all_procs_1(processes(), self(), 0).

kill_all_procs_1([Self|Ps], Self, N) ->
    kill_all_procs_1(Ps, Self, N);
kill_all_procs_1([P|Ps], Self, N) ->
    case process_info(P, group_leader) of
	{group_leader,Self} ->
	    exit(P, kill),
	    kill_all_procs_1(Ps, Self, N+1);
	_ ->
	    kill_all_procs_1(Ps, Self, N)
    end;
kill_all_procs_1([], _, 0) -> ok;
kill_all_procs_1([], _, _) -> kill_all_procs().

set_timer(infinity) -> ok;
set_timer(Time) ->
    {ok, _} = timer:exit_after(Time, timeout),
    ok.
