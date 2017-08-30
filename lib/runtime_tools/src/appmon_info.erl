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
%%----------------------------------------------------------------------
%%
%% Information centre for appmon. Must be present on each node
%% monitored.
%%
%%
%%	A worklist is maintained that contain all current work that
%%	should be performed at each timeout. Each entry in the
%%	worklist describes where the result shall be sent and a list
%%	of options relevant for that particular task
%%
%%
%% Maintenance Note:
%%
%%	This module is supposed to be updated by any who would like to
%%	subscribe for information. The idea is that several tools
%%	could use this module for their core information gathering
%%	services.
%%
%%	The module is based on the notion of tasks. Each task should
%%	have a nice public interface function which should handle task
%%	administration. Tasks are identified by a "key" consisting of
%%	three items, the requesting pid, the name of the task and the
%%	task auxillary parameter. The requesting pid is the pid of the
%%	callee (in the appmon case it can be the node window for
%%	instance), the task name is whatever name the task is given
%%	(in the appmon case it can be app, app_ctrl or load). The task
%%	name can be seen as the type of the task. The task auxillary
%%	parameter is an all purpose parameter that have a different
%%	meaning for each type of task so in appmon the Aux for app
%%	contains the root pid of the monitored application and in
%%	app_ctrl it contains the node name (just to distinguish from
%%	the other app_ctrl tasks, if any) while the Aux parameter is
%%	not used for the load task at all.
%%
%%	Each task also carries a list of options for
%%	customisation. The options valid for a task is completely
%%	internal to that task type except for the timeout option which
%%	is used by do_work to determine the interval at which to
%%	perform the task. The timeout option may also have the value
%%	at_most_once that indicates that the task should not be done
%%	more than once, in appmon the remote port (or process) info
%%	(pinfo) task is such a task that is only done once for each
%%	call. Note that the only way to change or update options is to
%%	call the public interface function for the task, this will
%%	merge the old options with the new ones and also force the
%%	task to be executed.
%%
%%	All tasks are managed by the do_work function. The basic
%%	functionality being that the result of the task is compared to
%%	the previous result and a delivery is sent to the callee if
%%	they differ. Most tasks are then done on a regular basis using
%%	the timer module for a delay.
%%	
%%	There are a limited number of places where the module need to
%%	be updated when new services are added, they are all marked
%%	with "Maintenance Note", and here is a quick guide:
%%
%%	First implement the task. Put the functions in this module
%%	among the other task implementations. Currently all task
%%	implementations should be put in this file to make it simple
%%	to monitor a node, this module should be the only one
%%	needed. Then add your implementation to the do_work2 function
%%	and finally add a public interface function among the other
%%	public interface functions. Voila.
%%
%%
%%
%%	Future ideas:
%%
%%	Appmon should maybe be enhanced to show all processes on a
%%	node. First put all processes in an ets P, then pick those
%%	that belong to applications (the normal way), then try to find
%%	those processes that are roots in process link trees and pick
%%	them. The final step would be to do something with those
%%	processes that are left.
%%
%%----------------------------------------------------------------------
-module(appmon_info).
-behaviour(gen_server).

%% Exported functions
-export([start_link/3, app/4, pinfo/4, load/4, app_ctrl/4]).

%% For internal use (RPC call)
-export([start_link2/3]).

%% For debugging
-export([status/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).


%%----------------------------------------------------------------------
%% The records
%%
%%	state is used for keeping track of all tasks.
%%
%%	db is the database used in the app task.
%%

-record(state, {starter, opts=[], work=[], clients=[]}).
-record(db, {q, p, links, links2}).


%%----------------------------------------------------------------------
%% Macros
%%

-define(MK_KEY(CMD, AUX, FROM, OPTS), {CMD, AUX, FROM}).
-define(MK_DOIT(KEY), {do_it, KEY}).
-define(ifthen(P,S), if P -> S; true -> ok end).


%%----------------------------------------------------------------------
%% Public interface
%%
%%	The Aux parameter is an auxillary parameter that can be used
%%	freely by the requesting process, it is included in the work
%%	task key. appmon uses it for storing the node name when
%%	requesting load and app_ctrl tasks, and appmon_a uses it for
%%	storing application name when requesting app task.
%%
%%	Maintenance Note: Put new tasks at the end, please.
%%


%% Do not use gen_server:start_link because we do not want the
%% appmon_info to die when initiating process dies unless special
%% conditions apply.
%% Uhu, we don't??? Made a fix so that this proces DOES indeed die
%% if it's starter dies. /Gunilla
start_link(Node, Client, Opts) ->
    rpc:call(Node, ?MODULE, start_link2, [self(), Client, Opts]).
start_link2(Starter, Client, Opts) ->
    Name = {local, ?MODULE},
    Args = {Starter, Opts, Client},
    case gen_server:start(Name, ?MODULE, Args, []) of
	{ok, Pid} ->
	    {ok, Pid};
	{error, {already_started, Pid}} -> 
	    register_client(Pid, Client),
	    {ok, Pid}
    end.
	

%% app_ctrl
%%
%%	Monitors which applications exist on a node
%%
app_ctrl(Serv, Aux, OnOff, Opts) ->
    gen_server:cast(Serv, {self(), app_ctrl, Aux, OnOff, Opts}).


%% load
%%
%%	Monitors load on a node
%%
load(Serv, Aux, OnOff, Opts) ->
    gen_server:cast(Serv, {self(), load, Aux, OnOff, Opts}).


%% app
%%
%%	Monitors one application given by name (this ends up in a
%%	process tree
%%
app(Serv, AppName, OnOff, Opts) ->
    gen_server:cast(Serv, {self(), app, AppName, OnOff, Opts}).

%% pinfo
%%
%%	Process or Port info
%%
pinfo(Serv, Pid, OnOff, Opt) ->
    gen_server:cast(Serv, {self(), pinfo, Pid, OnOff, Opt}).

%% register_client
%%
%%	Registers a client (someone subscribing for information)
%%

register_client(Serv, P) ->
    link(Serv),
    gen_server:call(Serv, {register_client, P}).

%% status
%%
%%	Status of appmon_info
%%

status() ->
    gen_server:cast(?MODULE, status).

%%----------------------------------------------------------------------
%%
%% Gen server administration
%%
%%----------------------------------------------------------------------

init({Starter, Opts, Pid}) ->
    link(Pid),
    process_flag(trap_exit, true),
    WorkStore = ets:new(workstore, [set, public]),
    {ok, #state{starter=Starter, opts=Opts, work=WorkStore,
		clients=[Pid]}}.

terminate(_Reason, State) ->
    ets:delete(State#state.work),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%----------------------------------------------------------------------
%%
%% Gen server calls
%%
%%----------------------------------------------------------------------

handle_call({register_client, Pid}, _From, State) ->
    NewState = case lists:member(Pid, State#state.clients) of
		   true -> State;
		   _ -> State#state{clients=[Pid | State#state.clients]}
	       end,
    {reply, ok, NewState};
handle_call(_Other, _From, State) ->
    {reply, ok, State}.

%%----------------------------------------------------------------------
%%
%% Gen server casts
%%
%%----------------------------------------------------------------------

%% Cmd = app_ctrl | load | app | pinfo
handle_cast({From, Cmd, Aux, OnOff, Opts}, State) ->
    NewState = update_worklist(Cmd, Aux, From, OnOff, Opts, State),
    {noreply, NewState};
handle_cast(status, State) ->
    print_state(State),
    {noreply, State};
handle_cast(_Other, State) ->
    {noreply, State}.


%%----------------------------------------------------------------------
%%
%% Gen server info's
%%
%%----------------------------------------------------------------------

handle_info({do_it, Key}, State) ->
    ok = do_work(Key, State),
    {noreply, State};

handle_info({'EXIT', Pid, Reason}, State) ->
    case State#state.starter of
	Pid ->
	    {stop, Reason, State};
	_Other ->
	    Work = State#state.work,
	    del_work(ets:match(Work, {{'$1','$2',Pid}, '_', '_', '_'}),
		     Pid, Work),
	    case lists:delete(Pid, State#state.clients) of
		[] -> case get_opt(stay_resident, State#state.opts) of
			  true -> {noreply, State#state{clients=[]}};
			  _ -> {stop, normal, State}
		      end;
		NewClients -> {noreply, State#state{clients=NewClients}}
	    end
    end;
handle_info(_Other, State) ->
    {noreply, State}.


%%----------------------------------------------------------------------
%%
%% Doing actual work
%%
%%----------------------------------------------------------------------

do_work(Key, State) ->
    WorkStore = State#state.work,
    {Cmd, Aux, From, _OldRef, Old, Opts} = retrieve(WorkStore, Key),
    {ok, Result} = do_work2(Cmd, Aux, From, Old, Opts),
    if
        Result==Old -> ok;
        true ->
            From ! {delivery, self(), Cmd, Aux, Result},
            ok
    end,
    case get_opt(timeout, Opts) of
	at_most_once ->
	    del_task(Key, WorkStore);
	T when is_integer(T) ->
	    {ok, Ref} = timer:send_after(T, ?MK_DOIT(Key)),
	    store(WorkStore, Key, Ref, Result, Opts)
    end,
    ok.


%%----------------------------------------------------------------------
%%
%% Name: do_work2
%%
%% Maintenance Note: Add a clause here for each new task.
%%
do_work2(load, _Aux, _From, Old, Opts) ->
    calc_load(Old, Opts);
do_work2(app_ctrl, _Aux, _From, _Old, _Opts) ->
    calc_app_on_node();
do_work2(app, Aux, _From, _Old, Opts) ->
    calc_app_tree(Aux, Opts);
do_work2(pinfo, Aux, _From, _Old, _Opts) ->
    calc_pinfo(pinfo, Aux);
do_work2(Cmd, Aux, _From, _Old, _Opts) ->
    {Cmd, Aux}.


retrieve(Tab, Key) ->
    case ets:lookup(Tab, Key) of
	[{{Cmd, Aux, From}, Ref, Old, Opts}] ->
	    {Cmd, Aux, From, Ref, Old, Opts};
	_Other ->
	    false
    end.

store(Tab, Key, Ref, Old, Opts) ->
    ets:insert(Tab, {Key, Ref, Old, Opts}),
    Key.


%%----------------------------------------------------------------------
%%
%% WorkStore handling
%%
%%----------------------------------------------------------------------

update_worklist(Cmd, Aux, From, true, Opts, State) ->
    add_task(Cmd, Aux, From, Opts, State),
    State;
update_worklist(Cmd, Aux, From, _Other, _Opts, State) ->
    del_task(Cmd, Aux, From, State#state.work),
    State.

%% First check if a task like this already exists and if so cancel its
%% timer and make really sure that no stray do it command will come
%% later. Then start a new timer for the task and store it i
%% WorkStorage
add_task(Cmd, Aux, From, Opts, State) ->
    WorkStore = State#state.work,
    Key = ?MK_KEY(Cmd, Aux, From, Opts),
    OldOpts = del_task(Key, WorkStore),
    store(WorkStore, Key, nil, nil, ins_opts(Opts, OldOpts)),
    catch do_work(Key, State),
    ok.

%% Delete a list of tasks belonging to a pid
del_work([[Cmd, Aux] | Ws], Pid, Work) ->
    del_task(Cmd, Aux, Pid, Work),
    del_work(Ws, Pid, Work);
del_work([], _Pid, _Work) -> ok.

%% Must return old options or empty list
del_task(Cmd, Aux, From, WorkStore) ->
    del_task(?MK_KEY(Cmd, Aux, From, []), WorkStore).
del_task(Key, WorkStore) ->
    OldStuff = retrieve(WorkStore, Key),
    ets:delete(WorkStore, Key),
    case OldStuff of
	{_Cmd, _Aux, _From, Ref, _Old, Opts} ->
	    if
		Ref /= nil ->
                    {ok,_} = timer:cancel(Ref),
		    receive
			{do_it, Key} ->
			    Opts
		    after 10 ->
			    Opts
		    end;
		true -> Opts
	    end;
	_ ->
	    []
    end.


%%
%% Maintenance Note:
%%
%% Add new task implementations somewhere here below.
%%


%%----------------------------------------------------------------------
%%**********************************************************************
%%
%%
%% BEGIN OF calc_app_tree
%%
%%	App tree is the process tree shown in the application window
%%
%%	The top (root) pid is found by calling
%%	application_controller:get_master(AppName) and this is done in
%%	calc_app_on_node (before the call to calc_app_tree).
%%
%%	We are going to add processes to the P ets and we are doing it
%%	in a two step process. First all prospect processes are put on
%%	the queue Q. Then we examine the front of Q and add this
%%	process to P if it's not already in P. Then all children of
%%	the process is put on the queue Q and the process is repeated.
%%
%%	We also maintain two link ets'es, one for primary links and
%%	one for secondary links. These databases are updated at the
%%	same time as the queue is updated with children.
%%
%%**********************************************************************
%%----------------------------------------------------------------------


calc_app_tree(Name, Opts) ->
    Mode = get_opt(info_type, Opts),
    case application_controller:get_master(Name) of
	Pid when is_pid(Pid) ->
	    DB = new_db(Mode, Pid),
	    GL = groupl(Pid),
	    R = case catch do_find_proc(Mode, DB, GL, find_avoid()) of
		    {ok, DB2} ->
			{ok, {format(Pid),
			      format(ets:tab2list(DB2#db.p)),
			      format(ets:tab2list(DB2#db.links)), 
			      format(ets:tab2list(DB2#db.links2))}};
		    {error, Reason} -> 
			{error, Reason};
		    Other ->
			{error, Other}
		end,
	    ets:delete(DB#db.p),
	    ets:delete(DB#db.links),
	    ets:delete(DB#db.links2),
	    R;
	_ ->
	    {ok, {[], [], [], []}}
    end.

get_pid(P) when is_pid(P) -> P;
get_pid(P) when is_port(P) -> P;
get_pid(X) when is_tuple(X) -> element(2, X).


%----------------------------------------------------------------------
%%---------------------------------------------------------------------
%% Handling process trees of processses that are linked to each other

do_find_proc(Mode, DB, GL, Avoid) ->
    case get_next(DB) of
	{{value, V}, DB2} ->
	    do_find_proc2(V, Mode, DB2, GL, Avoid);
	{empty, DB2} ->
	    {ok, DB2}
    end.

do_find_proc2(X, Mode, DB, GL, Avoid) when is_port(X) ->
    %% There used to be a broken attempt here to handle ports,
    %% but the rest of appmon can't handle ports, so now we
    %% explicitly ignore ports.
    do_find_proc(Mode, DB, GL, Avoid);
do_find_proc2(X, Mode, DB, GL, Avoid) ->
    Xpid = get_pid(X),
    DB2 = case is_proc(DB, Xpid) of
	      false ->
		  add_proc(DB, Xpid),
		  C1 = find_children(X, Mode),
		  add_children(C1, Xpid, DB, GL, Avoid, Mode);
	      _ -> 
		  DB
	  end,
    do_find_proc(Mode, DB2, GL, Avoid).


%% Find children finds the children of a process. The method varies
%% with the selected mode (sup or link) and there are also some
%% processes that must be treated differently, notably the application
%% master.
%% 
find_children(X, sup) when is_pid(X) ->
    %% This is the first (root) process of a supervision tree and it
    %% better be a supervisor, we are smoked otherwise
    supervisor:which_children(X);
find_children(X, link) when is_pid(X), node(X) /= node() ->
    [];
find_children(X, link) when is_pid(X) ->
    case process_info(X, links) of
	{links, Links} ->
	    lists:reverse(Links); % OTP-4082
	_ -> []
    end;
find_children({master, X}, sup) -> 
    case application_master:get_child(X) of
	{Pid, _Name} when is_pid(Pid) -> [Pid];
	Pid when is_pid(Pid) -> [Pid]
    end;
find_children({_, _X, worker, _}, sup) -> [];
find_children({_, X, supervisor, _}, sup) ->
    lists:filter(fun(Thing) -> 
			 Pid = get_pid(Thing),
			 if
			     is_pid(Pid) -> true;
			     true -> false
			 end
		 end,
		 supervisor:which_children(X)).


%% Add links to primary (L1) or secondary (L2) sets and return an
%% updated queue. A link is considered secondary if its endpoint is in
%% the queue of un-visited but known processes.
add_children(CList, Paren, DB, _GL, _Avoid, sup) ->
    lists:foldr(fun(C, DB2) -> 
			case get_pid(C) of
			    P when is_pid(P) -> 
				add_prim(C, Paren, DB2);
			    _ -> DB2 end end,
		DB, CList);

add_children(CList, Paren, DB, GL, Avoid, _Mode) ->
    lists:foldr(fun(C, DB2) ->
			maybe_add_child(C, Paren, DB2, GL, Avoid)
		end, DB, CList).

%% Check if the child is already in P
maybe_add_child(C, Paren, DB, GL, Avoid) ->
    case is_proc(DB, C) of
	false -> 
	    maybe_add_child_node(C, Paren, DB, GL, Avoid);
	_ -> DB					% In P: no action
    end.

%% Check if process on this node
maybe_add_child_node(C, Paren, DB, GL, Avoid) ->
    if
	node(C) /= node() -> 
	    add_foreign(C, Paren, DB);
	true -> 
	    maybe_add_child_avoid(C, Paren, DB, GL, Avoid)
    end.

%% Check if child is on the avoid list
maybe_add_child_avoid(C, Paren, DB, GL, Avoid) ->
    case lists:member(C, Avoid) of
	true -> DB;
	false ->
	    maybe_add_child_port(C, Paren, DB, GL)
    end.

%% Check if it is a port, then it is added
maybe_add_child_port(C, Paren, DB, GL) ->
    if
	is_port(C) ->
	    add_prim(C, Paren, DB);
	true ->
	    maybe_add_child_sasl(C, Paren, DB, GL)
    end.

%% Use SASL stuff if present
maybe_add_child_sasl(C, Paren, DB, GL) ->
    case check_sasl_ancestor(Paren, C) of
	yes ->					% Primary
	    add_prim(C, Paren, DB);
	no ->					% Secondary
	    add_sec(C, Paren, DB);
	dont_know ->
	    maybe_add_child_gl(C, Paren, DB, GL)
    end.
		    
%% Check group leader
maybe_add_child_gl(C, Paren, DB, GL) ->
    case cmp_groupl(GL, groupl(C)) of
	true -> maybe_add_child_sec(C, Paren, DB);
	_ -> DB
    end.

%% Check if the link should be a secondary one. Note that this part is
%% pretty much a guess.
maybe_add_child_sec(C, Paren, DB) ->
    case is_in_queue(DB, C) of
	true ->					% Yes, secondary
	    add_sec(C, Paren, DB);
	_ ->					% Primary link
	    add_prim(C, Paren, DB)
    end.

check_sasl_ancestor(Paren, C) ->
    case lists:keysearch('$ancestors', 1, 
			 element(2,process_info(C, dictionary))) of
	{value, {_, L}} when is_list(L) ->
	    H = if
		    is_atom(hd(L)) -> whereis(hd(L));
		    true -> hd(L)
		end,
	    if
		H == Paren -> yes;
		true -> no
	    end;
	_ -> dont_know
    end.


%----------------------------------------------------------------------
%%---------------------------------------------------------------------
%% Primitives for the database DB of all links, processes and the
%% queue of not visited yet processes.

-define(add_link(C, Paren, L), ets:insert(L, {Paren, C})).

new_db(Mode, Pid) ->
    P  = ets:new(processes, [set, public]),
    L1 = ets:new(links, [bag, public]),
    L2 = ets:new(extralinks, [bag, public]),
    Q = if
	    Mode =:= sup -> queue:in({master, Pid}, queue:new());
	    true -> queue:in(Pid, queue:new())
	end,
    #db{q=Q, p=P, links=L1, links2=L2}.

get_next(DB) ->
    {X, Q} = queue:out(DB#db.q),
    {X, DB#db{q=Q}}.
add_proc(DB, P) ->
    ets:insert(DB#db.p, {P}).
add_prim(C, Paren, DB) ->
    ?add_link(get_pid(C), Paren, DB#db.links),
    DB#db{q=queue:in(C, DB#db.q)}.
add_foreign(C, Paren, DB) ->
    ?add_link(C, Paren, DB#db.links2),
    DB#db{q=queue:in(C, DB#db.q)}.
add_sec(C, Paren, DB) ->
    ?add_link(C, Paren, DB#db.links2),
    DB.

is_proc(#db{p=Tab}, P) ->
    ets:member(Tab, P).

is_in_queue(#db{q=Q}, P) ->
    queue:member(P, Q).

%% Group leader handling. No processes or Links to processes must be
%% added when group leaders differ. Note that catch all is needed
%% because net_sup is undefined when not networked but still present
%% in the kernel_sup child list. Blahh, didn't like that.
groupl(P) ->
    case process_info(P, group_leader) of
	{group_leader, GL} -> GL;
	_Other -> nil
    end.

cmp_groupl(_GL1, nil) -> true;
cmp_groupl(GL1, GL1) -> true;
cmp_groupl(_, _) -> false.


%% Do some intelligent guessing as to cut in the tree
find_avoid() ->
    lists:foldr(fun(X, Accu) -> 
		       case whereis(X) of
			   P when is_pid(P) ->
			       [P|Accu];
			   _ -> Accu end end,
		[undefined],
		[application_controller, init, error_logger, gs, 
		 node_serv, appmon, appmon_a, appmon_info]).



%%----------------------------------------------------------------------
%%
%% Formats the output strings
%%
%%----------------------------------------------------------------------
format([{P} | Fs]) ->				% Process or port
    [{P, format(P)} | format(Fs)];
format([{P1, P2} | Fs]) ->			% Link
    [{format(P1), format(P2)} | format(Fs)];
format([]) -> [];
format(P) when is_pid(P), node(P) /= node() ->
    pid_to_list(P) ++ " " ++ atom_to_list(node(P));
format(P) when is_pid(P) ->
    case process_info(P, registered_name) of
	{registered_name, Name} -> atom_to_list(Name);
	_ -> pid_to_list(P)
    end;
format(P) when is_port(P) ->
    "port " ++ integer_to_list(element(2, erlang:port_info(P, id)));
format(X) ->
    io:format("What: ~p~n", [X]),
    "???".


%%----------------------------------------------------------------------
%%**********************************************************************
%%
%%
%% END OF calc_app_tree
%%
%%
%%**********************************************************************
%%----------------------------------------------------------------------




%%----------------------------------------------------------------------
%%**********************************************************************
%%
%%
%% BEGIN OF calc_app_on_node
%%
%%
%%**********************************************************************
%%----------------------------------------------------------------------

%% Finds all applications on a node
calc_app_on_node() ->
    NewApps = reality_check(application:which_applications()),
    {ok, NewApps}.


reality_check([E|Es]) ->
    N = element(1, E),
    case catch application_controller:get_master(N) of
        P when is_pid(P) -> [{P, N, E} | reality_check(Es)];
        _ -> reality_check(Es)
    end;
reality_check([]) -> [].




%%----------------------------------------------------------------------
%%**********************************************************************
%%
%%
%% END OF calc_app_on_node
%%
%%
%%**********************************************************************
%%----------------------------------------------------------------------



%%----------------------------------------------------------------------
%%**********************************************************************
%%
%%
%% BEGIN OF calc_load
%%
%%
%%**********************************************************************
%%----------------------------------------------------------------------

calc_load(Old, Opts) ->
    L = load(Opts),
    case get_opt(load_average, Opts) of
	true ->
	    case Old of
		{_, L} -> {ok, {L, L}};
		{_, O2} when abs(L-O2) < 3 -> {ok, {O2, L}};
		{_, O2}	-> {ok, {O2, trunc((2*L+O2)/3)}};
		_ -> {ok, {0, L}}
	    end;
	_ -> 
	    case Old of
		{_, O2} -> {ok, {O2, L}};
		_ -> {ok, {0, L}}
	    end
    end.


load(Opts) ->
    Q   = get_sample(queue),

    case get_opt(load_method, Opts) of
	time ->
	    Td  = get_sample(runtime),
	    Tot = get_sample(tot_time),
	    
	    case get_opt(load_scale, Opts) of
		linear ->
		    erlang:min(trunc(load_range()*(Td/Tot+Q/6)),
			load_range());
		prog ->
		    erlang:min(trunc(load_range()*prog(Td/Tot+Q/6)),
			load_range())
	    end;
	queue ->
	    case get_opt(load_scale, Opts) of
		linear ->
		    erlang:min(trunc(load_range()*Q/6), load_range());
		prog ->
		    erlang:min(trunc(load_range()*prog(Q/6)), load_range())
		end
    end.


%%
%% T shall be within 0 and 0.9 for this to work correctly
prog(T) ->
    math:sqrt(abs(T)/0.9).


get_sample(queue)  -> statistics(run_queue);
get_sample(runtime)  -> {Rt,Rd} = statistics(runtime), 
			delta(runtime, Rt, Rd);
get_sample(tot_time)  -> {Rt,Rd} = statistics(wall_clock), 
			 delta(tot_time, Rt, Rd).


%% Keeps track of differences between calls
%% Needed because somebody else might have called
%% statistics/1.
%%
%% Note that due to wrap-arounds, we use a cheating 
%% delta which is correct unless somebody else
%% uses statistics/1
delta(KeyWord, Val, CheatDelta) ->
    RetVal = case get(KeyWord) of 
		 undefined ->
		     Val;
		 Other ->
		     if
			 Other > Val ->
			     CheatDelta;
			 true ->
			     Val-Other
		     end
	     end,
    put(KeyWord, Val),
    RetVal.


load_range() -> 16.



%%----------------------------------------------------------------------
%%**********************************************************************
%%
%%
%% END OF calc_load
%%
%%
%%**********************************************************************
%%----------------------------------------------------------------------


%%----------------------------------------------------------------------
%%**********************************************************************
%%
%%
%% BEGIN OF calc_pinfo
%%
%%
%%**********************************************************************
%%----------------------------------------------------------------------

calc_pinfo(pinfo, Pid) when is_pid(Pid) ->
    Info = process_info(Pid),
    {ok, io_lib:format("Node: ~p, Process: ~p~n~p~n~n",
		       [node(), Pid, Info])};
calc_pinfo(pinfo, Pid) when is_port(Pid) ->
    Info = lists:map(fun(Key) ->erlang:port_info(Pid, Key) end,
		     [id, name, connected, links, input, output]),
    
    {ok, io_lib:format("Node: ~p, Port: ~p~n~p~n~n", 
		       [node(),  element(2, erlang:port_info(Pid, id)),
			Info])};
calc_pinfo(pinfo, _Pid) ->
    {ok, ""}.


%%----------------------------------------------------------------------
%%**********************************************************************
%%
%%
%% END OF calc_pinfo
%%
%%
%%**********************************************************************
%%----------------------------------------------------------------------



%%----------------------------------------------------------------------
%%
%% Print the State
%%
%%	-record(state, {opts=[], work=[], clients=[]}).
%%
%%----------------------------------------------------------------------
print_state(State) ->
    io:format("Status:~n    Opts: ~p~n"
	      "Clients: ~p~n    WorkStore:~n",
	      [State#state.opts, State#state.clients]),
    print_work(ets:tab2list(State#state.work)).

print_work([W|Ws]) ->
    io:format("        ~p~n", [W]), print_work(Ws);
print_work([]) -> ok.
    

%%----------------------------------------------------------------------
%%
%% Option handling
%%
%%----------------------------------------------------------------------

%% The only options ever set by a user is info_type, timeout,
%% load_scale and load_method.
get_opt(Name, Opts) ->
    case lists:keysearch(Name, 1, Opts) of
	{value, Val} -> element(2, Val);
	false -> default(Name)
    end.

%% not all options have default values
default(info_type)	-> link;
default(load_average)	-> true;
default(load_method)	-> time;
default(load_scale)	-> prog;
default(stay_resident)	-> false;
default(timeout)	-> 2000.

ins_opts([Opt | Opts], Opts2) ->
    ins_opts(Opts, ins_opt(Opt, Opts2));
ins_opts([], Opts2) -> Opts2.

ins_opt({Opt, Val}, [{Opt, _} | Os]) -> [{Opt, Val} | Os];
ins_opt(Opt, [Opt2 | Os]) -> [Opt2 | ins_opt(Opt, Os)];
ins_opt(Opt, []) -> [Opt].
