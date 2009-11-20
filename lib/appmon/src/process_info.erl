%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2009. All Rights Reserved.
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
-module(process_info).
-behavior(gen_server).

-export([start/0, start_link/0, stop/0]).
-export([is_node/1, get_nodes/0,
	 get_applications/1, get_application_keys/2,
	 get_processes/3, get_process_data/2,
	 send_trace/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(data, {que=undef,
	       procs=undef,
	       links=undef,
	       links2=undef}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                    %%
%% Functions to retrieve information about which application          %%
%% at the node                                                        %%
%%                                                                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start() ->
    gen_server:start({local, proc_info}, process_info, [], []).

start_link() ->
    gen_server:start_link({local, proc_info}, process_info, [], []).

stop() ->
    gen_server:call(proc_info, stop, 1000).

%% is_node(NodeS) -> {bool(), Node, NodeS2}
%%   NodeS = NodeS2 = string()
%%   Node = node()
is_node(NodeS) ->
    Node = list_to_atom(NodeS),
    case lists:member(Node, [node()|nodes()]) of
	true->
	    {true, Node, NodeS};
	false ->
	    {false, node(), atom_to_list(node())}
    end.

%% get_nodes() -> [node()]
get_nodes() ->
    [node()|nodes()].

%% get_applications(Node) -> [App]
%%   Node = node()
%%   App = atom()
%% Returns the list of all applications with a supervision tree (that
%% is, not library applications such as stdlib) at Node.
get_applications(Node) ->
    Info = rpc:call(Node, application, info, []),
    {value, {running, Apps}} = lists:keysearch(running, 1, Info),
    [App || {App, Pid} <- Apps, is_pid(Pid)].

%% get_application_keys(App, Node) -> {ok, Keys} | {error, Reason}
%%   Node = node()
%%   App = atom()
%%   Keys = [{Key, Val}]
%%     Key = atom()
%%     Val = term()
%%   Reason = badapp | badrpc
get_application_keys(App, Node) ->
    case rpc:call(Node, application, get_all_key, [App]) of
	{ok, Keys} ->
	    {ok, Keys};
	undefined ->
	    {error, badapp};
	{badrpc, _} ->
	    {error, badrpc}
    end.

%% get_processes(App, Mode, Node) -> {Tree, Dict} | unknown
%%   App = atom()
%%   Mode = sup | sup_child | all
%%   Node = node()
get_processes(App, Mode, Node) ->
    gen_server:call(proc_info, {get_processes, App, Mode, Node}).

%% get_process_data(Pid, Node) -> ProcData
%%   Pid = pid()
%%   Node = node()
%%   ProcData -- see erlang:process_info/1
get_process_data(Pid, Node) ->
    case rpc:call(Node, erlang, process_info, [Pid]) of
	{badrpc, _} ->
	    [{error,"Please try again"}];
	Res ->
	    Res
    end.

%% send_trace(PidL) -> void()
send_trace(PidL) ->
    gen_server:call(proc_info, {send_trace, PidL}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                    %%
%%  gen_server callbacks                                              %%
%%                                                                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([]) ->
    {ok, ets:new(procs, [])}.

handle_call({get_processes, App, Mode, Node}, _From, State) ->
    case do_get_processes(App, Mode, Node) of
	unknown ->
	    {reply, unknown, State};
	Tree ->
	    {reply, {Tree, State}, State}
    end;
handle_call({send_trace, PidL}, _From, State) ->
    do_send_trace(PidL, State),
    {reply, ok, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                    %%
%% Internal functions                                                 %%
%%                                                                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% do_get_processes(App, Mode, Node) -> Tree | unknown
%%   App = atom()
%%   Mode = all | sup | sup_childs
%%   Node = node()
%%   Tree = term()
do_get_processes(App, Mode, Node) ->
    case rpc:call(Node, application_controller, get_master, [App]) of
	Pid when is_pid(Pid) ->
	    start_collecting_data(Pid, Mode, Node);
	undefined ->
	    unknown
    end.

%% Initiate the database, get the processes and the links.
%% Then build lists and return them.
start_collecting_data(Pid, Mode, Node) ->
    Db = get_database(),
    {Db2, Tree} = build_graph({master,Pid}, Db, Pid, Mode, Node),
    delete_database(Db2),
    Tree.

get_database() ->
    P = ets:new(procs,[]),
    L = ets:new(link,[bag]),
    L2 = ets:new(link2,[bag]),
    Q = queue:new(),
    ets:insert(P, {whereis(application_controller), crap}),
    ets:insert(P, {whereis(gs), crap}),
    #data{que=Q, procs=P, links=L, links2=L2}.

delete_database(Db) ->
    ets:delete(Db#data.procs),
    ets:delete(Db#data.links),
    ets:delete(Db#data.links2).

%% The thought is
%% 1. Get the processes that links to Pid.
%%    Pid is the application master the first time.
%% 2. Add the processes to the database and clear the list of children
%%    from processes which for some resason not should be there.
%% 3. Queue the children, so we later can se if they have any links.
%% 4. Add links to the childrens.
%% 5. When the whole tree is retreived remove the unnecessary processes
%%    depending on the mode.
%% 6. Take all links that point to the same pid and sort out
%%    the primary and secondary relations.
%%    If more than one process links to the same process, the relation
%%    between a supervisor and a process is primary. The rest is
%%    secondary, there is no different in real world just in logic
%%    between a secondary and a primary relation.
%% When all processes in the application is collected,
%% fix secondary links and return the tree.
build_graph(finish, Db, Grp, Mode, Node) ->
    Db = fix_links(Db, Grp, Node), 
    delete_unwanted(Db, Mode, Grp),
    Tree = start_tree(Db, Node),
    {Db, Tree};
build_graph(Pid, Db, Grp, Mode, Node) ->
    Children = get_children(Pid, Mode, Node),
    Children2 = add_and_remove(Children, Pid, Db, Grp, Node),
    Q2 = queue_children(Db#data.que, Children2),
    add_children(Pid, Db, Children2, 1),
    case queue:out(Q2) of
	{empty, _}->
	    build_graph(finish, Db, Grp, Mode, Node);
	{{value,NPid}, Q3}->
	    Db2 = Db#data{que=Q3},
	    build_graph(NPid,Db2,Grp,Mode,Node)
    end.

%% Collect the processes which the current process has a link to
%% Pid is now the application_master and the application master's
%% child is the application supervisor but in reality there are two
%% application master processes.
%% Fix this by reordering the processes a little.
get_children({master,Pid}, _Mode, Node) when is_pid(Pid) ->
    %% Get the master pid
    MPid = case application_master:get_child(Pid) of
	       {Pid1, _App} -> Pid1;
	       Pid1 -> Pid1
	   end,
    %% Get the second appplication master process and order them
    %% correctly
    case rpc:call(Node, erlang, process_info, [MPid,links]) of
	{links, [H|T]} -> [H,MPid|T];
	{links, []} -> MPid
    end;
get_children({Pid, _Name}, _Mode, Node) when is_pid(Pid),
					   Node==node(Pid) ->
    {links,Links} = rpc:call(Node, erlang, process_info, [Pid,links]),
    Links;
get_children(Pid, _Mode, Node) when is_pid(Pid), Node==node(Pid) ->
    {links,Links} = rpc:call(Node, erlang, process_info, [Pid,links]),
    Links;
get_children(Pid, _Mode, Node) when is_pid(Pid), Node/=node(Pid) ->
    [];
get_children(Port, _Mode, _Node) when is_port(Port) ->
    [].

%% Add the links to the database.
%% The first case -- when it is the application master process -- there
%% is only one real child even though there are more links.
add_children({master,Pid}, Db, [Child|_Rest], N) ->
    add_child(Pid, Db, Child, N);
add_children(_Pid, _Db, [], _N) ->
    ok;
add_children(Pid, Db, [Child|Rest], N) ->
    add_child(Pid, Db, Child, N),
    add_children(Pid, Db, Rest, N+1).

add_child(Pid, Db, Child, N) ->
    case ets:match_object(Db#data.links, {Pid,Child,'_'}) of
	[] ->
	    ets:insert(Db#data.links, {Pid,Child,N});
	_ ->
	    ok
    end.

%% Add the list of processes to the queue.
queue_children(Queue, []) ->
    Queue;
queue_children(Queue, [H|T]) ->
    Q = queue:in(H, Queue),
    queue_children(Q, T).
    
%% The processess that we already has added to the database are
%% not children to the current process, so we don't need to add them a
%% second time. 
remove_used_children([], _Db, New_list) ->
    lists:reverse(New_list);
remove_used_children([Child|Rest], Db, New) ->
    case ets:lookup(Db#data.procs, Child) of
	[] ->
	    remove_used_children(Rest, Db, [Child|New]);
	_ ->
	    remove_used_children(Rest, Db, New)
    end.

%% Take the list of links and separate it into a list with ports and a
%% list with pids.
separate_ports([], Pids, Ports) ->
    {Pids, Ports};
separate_ports([Child|Rest], Pids, Ports) ->
    if
	is_port(Child) ->
	    separate_ports(Rest, Pids, [Child|Ports]);
	is_pid(Child) ->
	    separate_ports(Rest, [Child|Pids], Ports)
    end.

%% Add the current pid to the ets table with processes and clear
%% the list of children from processes that should not be there.
%% In the first case, no children are used so it's not necessary.
add_and_remove(Children, {master,Pid}, Db, _Grp, Node)
  when is_pid(Pid), Node==node(Pid) ->
    ets:insert(Db#data.procs, {Pid, {master,master}, controller}),
    {_Pids,Ports} = separate_ports(Children, [], []),
    Ports++Children;
%% This clause is removable when using only link as retrieving mode  .
add_and_remove(Children, {Pid,_Name}, Db, Grp, Node)
  when is_pid(Pid), Node==node(Pid) ->
    ets:insert(Db#data.procs, {Pid,
			       rpc:call(Node,erlang,process_info,
					[Pid,registered_name])}), 
    {Pids, Ports} = separate_ports(Children, [], []),
    Children1 = remove_used_children(Pids, Db, []),
    Children2 = remove_others_children(Children1, Grp, Node),
    Ports++Children2;
add_and_remove(Children, Pid, Db, Grp, Node) when is_pid(Pid),
						  Node==node(Pid) ->
    ets:insert(Db#data.procs, {Pid,
			       rpc:call(Node,erlang,process_info,
					[Pid,registered_name])}), 
    {Pids, Ports} = separate_ports(Children, [], []),
    Children1 = remove_used_children(Pids, Db, []),
    Children2 =remove_others_children(Children1, Grp, Node),
    Ports++Children2;
add_and_remove(_Children, Pid, _Db, _Grp, Node) when is_pid(Pid),
						     Node/=node(Pid) ->
    [];
%% Take care of the ports, don't add them to the table with processes.
add_and_remove(_Children, Pid, _Db, _Grp, _Node) when is_port(Pid) ->
    [].

%% Control that the application's group leader is the group leader of
%% Pid
group_leader_check({Pid,_Name}, Grp, Node) ->
    group_leader_check(Pid, Grp, Node);
group_leader_check(Pid, Grp, Node) ->
    case rpc:call(Node, erlang, process_info, [Pid,group_leader]) of
	{_Item, Grp} -> yes;
	_ -> no
    end.

%% Take the list of children and remove the ones with anoother group
%% leader.
remove_others_children(Children, Grp, Node) ->
    lists:filter(fun(Child) ->
		   case group_leader_check(Child, Grp, Node) of
		       yes -> true;
		       no -> false
		   end
		 end,
		 Children).

%% Mark the processes in the procs table as either supervisor or worker.
fix_links(Db, Leader, Node) ->
    {Sup,_Work} = mark_supervisors_workers(Db, Leader, Node),
    ets:match_delete(Db#data.procs, {'_',crap}),
    [_Pid|Procs] = ets:tab2list(Db#data.procs),
    N_links = get_n_links(Procs, Db#data.links, []),
    N_links2 = take_sup_links(Sup, Db#data.links, N_links),
    add_shared_links(N_links2, Db#data.links2),
    Db.

%% Add the links that point to the same child to the shared links table 
add_shared_links(N_links, Links2) ->
    Insert_fun = fun(Link) -> ets:insert(Links2, Link) end,
    lists:map(fun(List) -> lists:map(Insert_fun, List) end, N_links).
		    
%% Take the list of links that point to the same children and remove
%% the ones that are children to supervisors.
%% The first argument is a list of the supervisors.
%% N_links contains a list of list of links that points to the same
%% child.
take_sup_links([], _Db, N_links) ->
    N_links;
take_sup_links([H|Supervised], Links_table, N_links) ->
   N_list_fun = fun(Link) ->
			insert_sup_links(Link, H, Links_table)
		end,
    N_links2 = lists:map(fun(Link_list) ->
				 lists:filter(N_list_fun,Link_list)
			 end,
			 N_links),
    take_sup_links(Supervised, Links_table, N_links2).

%% Insert the supervised links in the primary links list.
%% This function should be used as a fun to the filter function in
%% take_sup_links/3.
insert_sup_links({From,To,N}, Sup, Links_table) ->
    case From of
	Sup -> 
	    ets:insert(Links_table, {From,To,N}),
	    false;
	_ ->
	    true
    end.

%% Get the links which points to the same children.
get_n_links([], _Links, N_link) ->
    N_link;
get_n_links([{Pid,_,_}|Procs], Links, N_link) ->
    case ets:match_object(Links, {'_',Pid,'_'}) of
	L when length(L)>1 ->
	    ets:match_delete(Links, {'_',Pid,'_'}),
	    get_n_links(Procs, Links, [L|N_link]);
	_L ->
	    get_n_links(Procs, Links, N_link)
    end;
get_n_links([{Pid,_}|Procs], Links, N_link) ->
    case ets:match_object(Links, {'_',Pid,'_'}) of
	L when length(L)>1 ->
	    ets:match_delete(Links, {'_',Pid,'_'}),
	    get_n_links(Procs, Links, [L|N_link]);
	_L ->
	    get_n_links(Procs, Links, N_link)
    end.

%% Mark the processes that are in the supervisor tree as either worker
%% or supervisor.
mark_supervisors_workers(Db, Leader, Node) ->
    %% Get the supervisors and workers.
    {Sup_list, Worker_list} = get_by_supervisors1(Leader),
    %% Update the supervisor pids.
    lists:map(fun(Pid) ->
		      ets:insert(Db#data.procs,
				 {Pid,
				  rpc:call(Node, erlang,process_info,
					   [Pid,registered_name]),
				  supervisor})
	      end,
	      Sup_list),
    %% Update the worker pids.
    lists:map(fun(Pid) ->
		      ets:insert(Db#data.procs,
				 {Pid,
				  rpc:call(Node, erlang,process_info,
					   [Pid,registered_name]),
				  worker})
	      end,
	      Worker_list),
    {lists:reverse(Sup_list), Worker_list}.

%% The second way to retrieve the applications processes is to go by
%% the supervision tree.
get_by_supervisors1(Leader) ->
    case application_master:get_child(Leader) of
	{Pid, _Name}->
	    get_by_supervisors([{namn,Pid,supervisor,list_of_mods}],
			       [], []);
	Pid ->
	    get_by_supervisors([{namn,Pid,supervisor,list_of_mods}],
			       [], [])
    end.

get_by_supervisors([], Sup, Work) ->
    {Sup, Work};
get_by_supervisors([{_,Pid,supervisor,_}|Rest], Sup, Work)
  when is_pid(Pid) ->
    Children = supervisor:which_children(Pid),
    Children2 = lists:append(Children, Rest),
    get_by_supervisors(Children2, [Pid|Sup], Work);
get_by_supervisors([{_,Pid,_,_}|Rest], Sup, Work) when is_pid(Pid) ->
    get_by_supervisors(Rest, Sup, [Pid|Work]);
get_by_supervisors([_Whatever|Rest], Sup, Work) ->
    get_by_supervisors(Rest, Sup, Work).

%% Use pattern matching to select mode and delete the unneccesary pids
delete_unwanted(Db, sup_child, App_pid) ->
    delete_not_in_supervisor_tree(Db),
    add_main_link(Db, App_pid),
    Db;
delete_unwanted(Db, all, _App_pid) ->
    Db;
delete_unwanted(Db, sup, App_pid) ->
    delete_workers(Db),
    delete_not_in_supervisor_tree(Db),
    add_main_link(Db, App_pid),
    Db.

add_main_link(Db, App_pid) ->
    case application_master:get_child(App_pid) of
	{Pid, _Name} when is_pid(Pid) -> 
	    ets:insert(Db#data.links, {App_pid,Pid,1});
	Pid when is_pid(Pid) ->
	    ets:insert(Db#data.links, {App_pid,Pid,1});
	_ ->
	    false
    end.

%% Delete the processes that are in the supervision tree but are
%% workers, and their links.
delete_workers(Db) ->
    Pids = ets:match_object(Db#data.procs, {'_','_',worker}),
    Pids2 =
	lists:map(
	  fun({Pid,_,_}) ->
		  %% Remove the unwanted pids from the process table.
		  ets:match_delete(Db#data.procs, {Pid,'_','_'}),
		  %% Remove the links to and from the pid.
		  ets:match_delete(Db#data.links, {Pid,'_','_'}),
		  ets:match_delete(Db#data.links, {'_',Pid,'_'}),
		  ets:match_delete(Db#data.links2, {Pid,'_','_'}),
		  ets:match_delete(Db#data.links2, {'_',Pid,'_'})	
	  end,
	  Pids),
    Pids2.

%% Delete the processes that are not in the supervision tree.
delete_not_in_supervisor_tree(Db) ->
    Pids = ets:match_object(Db#data.procs,{'_','_'}),
    Pids2 =
	lists:map(
	  fun({Pid,_}) ->
		  %% Remove the unwanted from the process table.
		  ets:match_delete(Db#data.procs, {Pid,'_'}),
		  %% Remove the links to and from the pid.
		  ets:match_delete(Db#data.links, {Pid,'_','_'}),
		  ets:match_delete(Db#data.links, {'_',Pid,'_'}),
		  ets:match_delete(Db#data.links2, {Pid,'_','_'}),
		  ets:match_delete(Db#data.links2, {'_',Pid,'_'})
	  end,
	  Pids),
    Pids2.

%% Start generating the tree.
start_tree(Db, Node) ->
    case get_master(Db) of
	no -> false;
	Pid ->
	    build_node(Pid, Db, Node)
    end.

%% Build a node and then it runs itself on every child to the current
%% pid.
build_node(Pid, Db, Node) when is_pid(Pid), Node==node(Pid) ->
    Sort_fun = fun sort_order/2,
    Fix_sec_name_fun = fun(Pid2) -> get_link_name(Pid2, Db) end,
    Build_tree_fun = fun({_,Pid1,_}) -> build_node(Pid1,Db,Node) end,
    Children = ets:match_object(Db#data.links, {Pid,'_','_'}),
    Children1 = lists:sort(Sort_fun, Children),
    Sec_children = ets:match_object(Db#data.links2, {Pid,'_','_'}),
    {get_name(Pid,Db),
     lists:map(Build_tree_fun,Children1),
     lists:map(Fix_sec_name_fun,Sec_children)};
build_node(Pid, _Db, Node) when is_pid(Pid), Node/=node(Pid) ->
    {"Runs on another node:"++erlang:pid_to_list(Pid), [], []};
build_node(Pid, _Db, _Node) when is_port(Pid) ->
    {"Port :"++erlang:port_to_list(Pid), [], []}.

%% Select the name of the pid from the database where we previosly
%% added it.
get_name(Pid, Db) ->    
    case ets:lookup(Db#data.procs, Pid) of
	[{_,{_,master},_}] -> pid_to_list(Pid);
	[{_,{_,Name}}] -> atom_to_list(Name)++" : "++pid_to_list(Pid);
	[{_,{_,Name},_}] -> atom_to_list(Name)++" : "++pid_to_list(Pid);
	_ -> pid_to_list(Pid)
    end.

%% Select the name of the process which we have a link to.
get_link_name({_,Pid,_}, Db) when is_pid(Pid) ->    
    case ets:lookup(Db#data.procs, Pid) of
	[{_,{_,Name}}] -> atom_to_list(Name)++" : "++pid_to_list(Pid);
	[{_,{_,Name},_}] -> atom_to_list(Name)++" : "++pid_to_list(Pid);
	_ -> pid_to_list(Pid)
    end;
get_link_name({_,Port,_}, _Db) when is_port(Port) ->    
    "Port :"++" : ";
get_link_name(_, _) ->
    "".

%% Sort the links in the order they where added, in ascending order.
sort_order({_,_,N1}, {_,_,N2}) when N1>N2 -> true;
sort_order(_N1, _N2) -> false.

%% Select the pid of the application master.
get_master(Db) ->
    case ets:match_object(Db#data.procs,
			  {'_',{master,master},controller}) of
	[{Pid,_,_}|_Rest] -> Pid;
	_ -> no
    end.

%% The main function to handle tracing.
%% Checks if the process is in the table with traced processes. If so,
%% it stops the trace, otherwise it starts the trace.
do_send_trace(PidL, Traced_tab) ->
    Pid = list_to_pid(PidL),
    Key = get_key(Pid),
    case catch ets:lookup(Traced_tab, Key) of
	[] ->
	    trace_process(Pid, Key, true, Traced_tab);
	[_Object]->
	    trace_process(Pid, Key, false, Traced_tab)
    end,
    filter_procs(Traced_tab, ets:tab2list(Traced_tab)).

get_key(Pid) ->
    Node = node(Pid),
    case rpc:call(Node, erlang, process_info, [Pid,registered_name]) of
	[] -> pid_to_list(Pid);
	{registered_name, Name} ->
	    atom_to_list(Name)++" : "++pid_to_list(Pid)
    end.

%% Tries to toggle the trace flag for the process.
trace_process(Pid, Key, On_or_off, Procs_tab) ->
    case rpc:call(node(Pid), sys, trace, [Pid,On_or_off,1000]) of
	timeout ->
	    Node = node(Pid),
	    io:fwrite("timeout node= ~w, Pid= ~w mode= ~w ~n",
		      [Node, Pid, On_or_off]);
	{badrpc, _} ->
	    Node = node(Pid),
	    io:fwrite("badrpc node= ~w, Pid= ~w mode= ~w ~n",
		      [Node, Pid, On_or_off]);
	Res ->
	    Node = node(Pid),
	    io:fwrite("anymode ~w node= ~w, Pid= ~w mode= ~w ~n",
		      [Res, Node, Pid,On_or_off]),
	    case On_or_off of
		true -> ets:insert(Procs_tab, {Key,On_or_off});
		false -> ets:delete(Procs_tab, Key)
	    end
    end.

%% Check if the processes in the ets table with traced processes
%% are alive. If not, remove them.
filter_procs(Tab, Tab_list) ->
    lists:foreach(fun({Key,_Val}) -> is_alive(Key, Tab) end, Tab_list).

is_alive(Key, Tab) ->
    case get_pid(Key) of
	nopid -> false;
	Pid -> is_alive2(Pid, Key, Tab)
    end.

%% Key is either a pid in list form or Pidname:Pid in list form.
get_pid(Key) ->
    case catch list_to_pid(string:substr(Key,string:rchr(Key,$<))) of
	Pid when is_pid(Pid) ->
	    Pid;
	_ ->
	    nopid
    end.

is_alive2(Pid, Key, Tab) ->
    case catch rpc:call(node(Pid), erlang, is_process_alive, [Pid]) of
	true -> true;
	false ->
	    catch ets:delete(Tab, Key),
	    ok
    end.
