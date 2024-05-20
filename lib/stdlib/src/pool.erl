%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2024. All Rights Reserved.
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
-module(pool).
-moduledoc """
Load distribution facility.

This module can be used to run a set of Erlang nodes as a pool of computational
processors. It is organized as a master and a set of slave nodes and includes
the following features:

- The slave nodes send regular reports to the master about their current load.
- Queries can be sent to the master to determine which node will have the least
  load.

The BIF [`statistics(run_queue)`](`statistics/1`) is used for estimating future
loads. It returns the length of the queue of ready to run processes in the
Erlang runtime system.

The slave nodes are started with the `m:slave` module. This effects terminal
I/O, file I/O, and code loading.

If the master node fails, the entire pool exits.

[](){: #files }

## Files

`.hosts.erlang` is used to pick hosts where nodes can be started. For
information about format and location of this file, see `net_adm:host_file/0`.

`$HOME/.erlang.slave.out.HOST` is used for all extra I/O that can come from the
slave nodes on standard I/O. If the startup procedure does not work, this file
can indicate the reason.
""".

%% Supplies a computational pool of processors.
%% The chief user interface function here is get_node()
%% Which returns the name of the nodes in the pool
%% with the least load !!!!
%% This function is callable from any node including the master
%% That is part of the pool
%% nodes are scheduled on a per usage basis and per load basis,
%% Whenever we use a node, we put at the end of the queue, and whenever
%% a node report a change in load, we insert it accordingly

% User interface Exports ...
-export([start/1, 
	 start/2, 
	 stop/0,
	 get_nodes/0, 
	 get_nodes_and_load/0, 
	 get_node/0,
	 pspawn/3, 
	 attach/1,
	 pspawn_link/3]).

%% Internal Exports 
-export([statistic_collector/0,
	 do_spawn/4,
	 init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2]).
	 
%% User interface 

%% Start up using the .hosts.erlang file

-doc(#{equiv => start(Name, [])}).
-spec start(Name) -> Nodes when
      Name :: atom(),
      Nodes :: [node()].
start(Name) ->
    start(Name,[]).

-doc """
Starts a new pool.

The file `.hosts.erlang` is read to find host names where the
pool nodes can be started; see section [Files](`m:pool#module-files`). The startup
procedure fails if the file is not found.

The slave nodes are started with [`slave:start/2,3`](`slave:start/2`), passing
along `Name` and, if provided, `Args`. `Name` is used as the first part of the
node names, `Args` is used to specify command-line arguments.

Access rights must be set so that all nodes in the pool have the authority to
access each other.

The function is synchronous and all the nodes, and all the system servers, are
running when it returns a value.
""".
-spec start(Name, Args) -> Nodes when
      Name :: atom(),
      Args :: string(),
      Nodes :: [node()].
start(Name, Args) when is_atom(Name) ->
    _ = gen_server:start({global, pool_master}, pool, [], []),
    Hosts = net_adm:host_file(),
    Nodes = start_nodes(Hosts, Name, Args),
    lists:foreach(fun attach/1, Nodes),
    Nodes.

%%
%% Interface functions ...
%%
-doc "Returns a list of the current member nodes of the pool.".
-spec get_nodes() -> [node()].
get_nodes() ->
    get_elements(2, get_nodes_and_load()).

-doc """
Ensures that a pool master is running and includes `Node` in the pool master's
pool of nodes.
""".
-spec attach(Node) -> 'already_attached' | 'attached' when
      Node :: node().
attach(Node) ->
    gen_server:call({global, pool_master}, {attach, Node}).

-doc false.
get_nodes_and_load() ->
    gen_server:call({global, pool_master}, get_nodes).

-doc "Returns the node with the expected lowest future load.".
-spec get_node() -> node().
get_node() ->
    gen_server:call({global, pool_master}, get_node).

-doc """
Spawns a process on the pool node that is expected to have the lowest future
load.
""".
-spec pspawn(Mod, Fun, Args) -> pid() when
      Mod :: module(),
      Fun :: atom(),
      Args :: [term()].
pspawn(M, F, A) ->
    gen_server:call({global, pool_master}, {spawn, group_leader(), M, F, A}).

-doc """
Spawns and links to a process on the pool node that is expected to have the
lowest future load.
""".
-spec pspawn_link(Mod, Fun, Args) -> pid() when
      Mod :: module(),
      Fun :: atom(),
      Args :: [term()].
pspawn_link(M, F, A) ->
    spawn_link(get_node(), M, F, A).

-compile([{nowarn_deprecated_function,[{slave,start,3}]}]).

start_nodes([], _, _) -> [];
start_nodes([Host|Tail], Name, Args) -> 
    case slave:start(Host, Name, Args) of 
	{error, {already_running, Node}} ->
	    io:format("Can't start node on host ~w due to ~w~n",[Host, {already_running, Node}]),
	    [Node | start_nodes(Tail, Name, Args)];
	{error, R} ->
	    io:format("Can't start node on host ~w due to ~w~n",[Host, R]),
	    start_nodes(Tail, Name, Args);
	{ok, Node} -> 
	    [Node | start_nodes(Tail, Name, Args)]
    end.

-doc "Stops the pool and kills all the slave nodes.".
-spec stop() -> 'stopped'.
stop() ->
    gen_server:call({global, pool_master}, stop).

get_elements(_Pos,[]) -> [];
get_elements(Pos,[E|T]) -> [element(Pos,E) | get_elements(Pos,T)].

stop_em([]) -> stopped;
stop_em([N|Tail]) ->
    rpc:cast(N, erlang, halt, []),
    stop_em(Tail).

-doc false.
init([]) ->
    process_flag(trap_exit, true),
    spawn_link(pool, statistic_collector, []),
    {ok,[{0,node()}]}.

-doc false.
handle_call(get_nodes, _From, Nodes)->
    {reply, Nodes, Nodes};
handle_call(get_node, _From, [{Load,N}|Tail]) ->
    {reply, N, Tail++[{Load+1, N}]};
handle_call({attach, Node}, _From, Nodes) ->
    case lists:keymember(Node, 2, Nodes) of
	true ->
	    {reply, already_attached, Nodes};
	false ->
	    erlang:monitor_node(Node, true),
	    spawn_link(Node, pool, statistic_collector, []),
	    {reply, attached, Nodes++[{999999,Node}]}
    end;
handle_call({spawn, Gl, M, F, A}, _From, Nodes) ->
    {reply, N, NewNodes} = handle_call(get_node, _From, Nodes),
    Pid = spawn(N, pool, do_spawn, [Gl, M, F, A]),
    {reply, Pid, NewNodes};
handle_call(stop, _From, Nodes) ->
    %% clean up in terminate/2
    {stop, normal, stopped, Nodes}.

-doc false.
handle_cast(_, Nodes) ->
    {noreply, Nodes}.

-doc false.
handle_info({Node,load,Load}, Nodes) ->
    Nodes2 = insert_node({Load,Node}, Nodes),
    {noreply, Nodes2};
handle_info({nodedown, Node}, Nodes) ->
    {noreply, lists:keydelete(Node, 2, Nodes)};
handle_info(_, Nodes) ->  %% The EXIT signals etc.etc
    {noreply, Nodes}.

-doc false.
terminate(_Reason, Nodes) ->
    N = lists:delete(node(), get_elements(2, Nodes)),
    stop_em(N),
    ok.

-doc false.
-spec do_spawn(pid(), module(), atom(), [term()]) -> term().
do_spawn(Gl, M, F, A) ->
    group_leader(Gl, self()),
    apply(M, F, A).

insert_node({Load,Node},[{L,Node}|Tail]) when Load > L ->
    %% We have a raised load here
    pure_insert({Load,Node},Tail);
insert_node({Load,Node},[{L,N}|Tail]) when Load =< L ->
    %% Move forward in the list
    T = lists:keydelete(Node,2,[{L,N}|Tail]),
    [{Load,Node} | T];
insert_node(Ln,[H|T]) ->
    [H | insert_node(Ln,T)];
insert_node(X,[]) ->          % Can't happen
    error_logger:error_msg("Pool_master: Bad node list X=~w\n", [X]),
    exit(crash).

pure_insert({Load,Node},[]) ->
    [{Load,Node}];
pure_insert({Load,Node},[{L,N}|Tail]) when Load < L ->
    [{Load,Node}, {L,N} | Tail];
pure_insert(L,[H|T]) -> [H|pure_insert(L,T)].

%% Really should not measure the contributions from
%% the background processes here .... which we do :-(
%% We don't have to monitor the master, since we're slaves anyway

-doc false.
statistic_collector() ->
    statistic_collector(5).

statistic_collector(0) -> exit(normal);
statistic_collector(I) ->
    timer:sleep(300),
    case global:whereis_name(pool_master) of
	undefined ->
	    statistic_collector(I-1);
	M ->
	    stat_loop(M, 999999)
    end.

%% Do not tell the master about our load if it has not changed

stat_loop(M, Old) ->
    timer:sleep(2000),
    case statistics(run_queue) of
	Old ->
	    stat_loop(M, Old);
	NewLoad ->
	    M ! {node(), load, NewLoad}, %% async 
	    stat_loop(M, NewLoad)
    end.
