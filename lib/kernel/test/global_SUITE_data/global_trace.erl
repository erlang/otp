%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2016. All Rights Reserved.
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
-module(global_trace).

%%%
%%% Inspection of High Level Trace created by global.erl.
%%%

%%% A few handy functions when running the test_server
%%% 

d() ->
    lists:foreach(fun(F) -> dd(F, []) end, last()).

d(Testcase) ->
    d(Testcase, []).

%% Skip "global_" from T.
d(Testcase, Options) ->
    [Filename] = tmp_files(Testcase),
    dd(Filename, Options).

dd(Filename, Options) ->
    io:format("\n======= ~s \n", [Filename]), 
    t(Filename, Options).

last() ->
    tmp_files("*").

%% global_groups_change: one node is restarted
%% global_mass_death: nodes are stopped
%% global_lock_die: two spurious (trying to remove locks taken by other pid)
%% global_otp_5640: 4 spurious (names registered again &c)
tmp_files(A) when is_atom(A) ->
    tmp_files(atom_to_list(A));
tmp_files(T) when is_list(T) ->
    Logs = logdir(),
    Dir = lists:last(filelib:wildcard(filename:join(Logs, "*"))),
    filelib:wildcard(filename:join([Dir, log_private, "global_" ++ T])).

%logdir() ->
%    "/net/yoshi/ldisk/daily_build/otp_norel_linux_r11b.2007-02-18_19/"
%        "test/test_server/global_SUITE.logs";
%logdir() ->
%    "/ldisk/daily_build/otp_norel_linux_suse_r11b.2007-02-07_19/test/"
%        "test_server/global_SUITE.logs";
logdir() ->
    "/tmp/tests/test_server/global_SUITE.logs".



%%% The contents of this file is by no means fixed; the printouts are
%%% updated depending on the problems at hand. Not everything has been
%%% designed very carefully :)
%%% 
%%% For one thing, the trace from all nodes are written onto the file
%%% as one single term. One term per node would be better. &c.

-compile(export_all).

-record(state, {connect_all, known = [], synced = [],
		resolvers = [], syncers = [], node_name = node(),
		the_locker, the_deleter, the_registrar, trace = [],
                global_lock_down
               }).

%% Compatible with different versions.
state(#state{}=S) ->
    S;
state({state, ConnectAll, Known, Synced, LockersResolvers, Syncers,
       NodeName, TheLocker, TheDeleter}) ->
    %% r10b: Lockers, r10b_patched, r11b: Resolvers
    #state{connect_all = ConnectAll, known = Known, synced = Synced,
           resolvers = LockersResolvers, syncers = Syncers, 
           node_name = NodeName, the_locker = TheLocker, 
           the_deleter = TheDeleter, the_registrar = undefined, trace = []};
state({state, ConnectAll, Known, Synced, Resolvers, Syncers,
       NodeName, TheLocker, TheDeleter, Trace}) ->
    %% r11b, some time before r11b-3
    #state{connect_all = ConnectAll, known = Known, synced = Synced,
           resolvers = Resolvers, syncers = Syncers, 
           node_name = NodeName, the_locker = TheLocker, 
           the_deleter = TheDeleter, the_registrar = undefined, 
           trace = Trace};
state({state, ConnectAll, Known, Synced, Resolvers, Syncers,
       NodeName, TheLocker, TheDeleter, TheRegistrar, Trace}) ->
    %% r11b, some time after r11b-3
    #state{connect_all = ConnectAll, known = Known, synced = Synced,
           resolvers = Resolvers, syncers = Syncers, 
           node_name = NodeName, the_locker = TheLocker, 
           the_deleter = TheDeleter, the_registrar = TheRegistrar, 
           trace = Trace, global_lock_down = false};
state(Else) ->
    Else.

%%% Trace tuples look like {Node, Now, Message, Nodes, Extra}.
%%% Nodes is the list as returned by nodes().
%%% Extra is [] most of the time.
%%%
%%% init
%%% {nodedown,DownNode}
%%% {extra_nodedown,DownNode}
%%% {nodeup, UpNode}
%%% {added,AddedNodes}, Extra = [{new_nodes, NewNodes}, 
%%%                              {abcast, Known},
%%%                              {ops,Ops}]
%%%    NewKnown = Known ++ AddedNodes
%%%    AddedNodes = NewNodes -- Known
%%%    NewNodes är här den man förhandlat med plus de noder den känner till.
%%% {added, AddedNodes}, Extra = [{ops,Ops}]
%%%    NewKnown = Known ++ AddedNodes
%%%    Den (passiva) noden får Nodes som är NewNodes
%%%    hos den förhandlande. Sedan: AddedNodes = (Nodes -- Known) -- [node()].
%%%    Det är som hos förhandlaren.
%%% {nodes_changed, {New,Old}}
%%%    Every now and then the list [node() | nodes()] is checked for updates.
%%%    New are the nodes that global does not know of (yet).
%%% {new_node_name, NewNode}
%%%    Ignored. Someone changes the nodename dynamically.
%%% {ins_name, Node}, Extra = [Name, Pid]
%%%    Node = node(Pid)
%%% {ins_name_ext, Node}, Extra = [Name, Pid]
%%%    Node = node(Pid)
%%% {del_name, Node}, Extra = [Name, Pid]
%%%    Node = node(Pid)
%%% {ins_lock, Node}, Extra = [Id, Pid]
%%%    Node = node(Pid)
%%% {rem_lock, Node}, Extra = [Id, Pid]
%%%    Node = node(Pid)
%%% {locker_succeeded, node()}, Extra = {First, Known}
%%% {locker_failed, node()}, Extra = {Tried, SoFar}
%%%    The nodes in SoFar have been locked, could not lock Tried.
%%%
%%% Also trace of the creation and deletion of resolvers
%%% (this kind of resolvers are created when nodeup arrives from
%%%  unknown nodes (there are also name resolvers...)).
%%% {new_resolver, Node}, Extra = [Tag, ResolverPid]
%%% {kill_resolver, Node}, Extra = [Tag, ResolverPid]
%%% {exit_resolver, Node}, Extra = [Tag]

-record(node, {
          node, 
          known = [],         % #state.known (derived)
          nodes = [],         % nodes()
          locks = [],         % [{Id, [Pid, node(Pid)]}] (derived)
          names = [],         % [{Name, Pid, node(Pid)}] (derived)
          resolvers = [],     % [{Node, Tag, ResolverPid}]
          n_locks = {0,       % OK
                     0,       % Tried to lock the boss
                     0,       % Tried to lock other boss
                     0},      % Tried to lock known
          rejected = 0        % Lock OK, but later rejected
         }).

-record(w, {nodes = [], % [#node{}]
            n = []}).

t(File) ->
    t(File, []).

%%% What to search for in the output of t/2?
%%% - 'NEGOTIATIONS': this is a list of the name negotiations 
%%%   (the big picture);
%%% - '###' signals a possibly strange event;
%%% - 'spurious' is used for "tagging" such events;
%%% - 'resol ' could mean that some resolver process has not been removed;
%%% ...

%% Options:
%% {show_state, From, To} 
%%    From = To = integer() | {integer(), integer()}
%%      Examples: {7, 8} (show states between seconds 7.0 and 8.0);
%%                {{1,431234},{2,432}} (between 1.431234 and 2.000432)
%%      The state of a node includes locks, names, nodes, known, ...
%%      Default is {{0,0}, {0,0}}, that is, do not show state.
%% show_state
%%      same as {show_state, 0, 1 bsl 28}, that is, show every state
%% {show_trace, bool()
%%      Show the complete trace as one list and per node pair.
%%      Default is true.
t(File, Options) ->
    {StateFun, ShowTrace} = 
        case options(Options, [show_state, show_trace]) of
            [{From,To}, ST] ->
                {fun(T, S) ->
                         Time =  element(2, T),
                         if 
                             Time >= From, Time =< To ->
                                 io:format("===> ~p~n", [T]),
                                 display_nodes("After", Time, S#w.nodes, T);
                             true ->
                                 ok
                         end
                 end, ST};
            _ ->
                erlang:error(badarg, [File, Options])
        end,
    D1 = try
             %% All nodes' trace is put on the file as one binary.
             %% This could (and should?) be improved upon.
             {ok, Bin} = file:read_file(File),
             io:format("Size of trace file is ~p bytes~n", [size(Bin)]),
             binary_to_term(Bin)
         catch _:_ ->
             {ok, [D0]} = file:consult(File),
             D0
         end,
    {D2, End} = case D1 of
                    {high_level_trace, ET, D3} ->
                        {D3, ET};
                    _ ->
                        {D1, now()}
                end,
    D = adjust_nodes(D2),
    {NodeNodeTrace, _NodeTrace, Trace, Base} = get_trace(D, End),
    messages(D, Base, End),

    %io:format("NET~n ~p~n", [net_kernel_nodes(NodeTrace)]),

    io:format("NEGOTIATIONS:~n ~p~n", [negotiations(Trace)]),

    io:format("*** Complete trace ***~n"),
    if 
        ShowTrace ->
            show_trace(Trace),
            io:format("--- End of complete trace ---~n"),
            lists:map(fun({{Node,ActionNode},Ts}) ->
                              io:format("*** Trace for ~p on node ~p ***~n", 
                                        [ActionNode, Node]),
                              show_trace(lists:keysort(2, Ts)),
                              io:format("--- End of trace for ~p on node ~p ---~n", 
                                        [ActionNode, Node])
                      end, NodeNodeTrace);
        true -> ok
    end,
    io:format("*** Evaluation ***~n"),
    {Fini, Spurious} = eval(Trace, StateFun),
    io:format("*** End of evaluation ***~n"),
    show_spurious(NodeNodeTrace, Spurious),
    display_nodes("FINI", '', Fini),
    ok.

% show_trace(Trace) ->
%     lists:foreach(fun({Node, {S,Mu}, Message, Nodes, Extra}) ->
%                           io:format("~2w.~6..0w ~w~n", [S, Mu, Node]),
%                           io:format("     ~p~n", [Message]),
%                           io:format("          Nodes: ~p~n", [Nodes]),
%                           case Extra of 
%                               [] -> ok;
%                               _ -> io:format("          Extra: ~p~n", [Extra])
%                           end
%                   end, Trace);
show_trace(Trace) ->
    lists:map(fun(T) -> io:format("~p~n", [T]) end, Trace).

get_trace(D, EndTime0) ->
    NodeTrace0 = [{Node,lists:keysort(2, (state(State))#state.trace)} || 
                     {Node,{info,State}} <- D,
                     case state(State) of
                         #state{trace = no_trace} ->
                             io:format("No trace for ~p~n", [Node]),
                             false;
                         #state{} ->
                             true;
                         Else ->
                             io:format("Bad state for ~p: ~p~n", 
                                       [Node, Else]),
                             false
                     end],
    Trace0 = lists:keysort(2, lists:append([T || {_Node, T} <- NodeTrace0])),
    Trace1 = sort_nodes(Trace0),
    {Base, Trace2} = adjust_times(Trace1),
    EndTime = adjust_time(EndTime0, Base),
    io:format("The trace was generated at ~p~n", [EndTime]),
    Trace = [T || T <- Trace2, element(2, T) < EndTime],
    NodeTrace = [{Node, adjust_times(Ts, Base)} ||
                    {Node, Ts} <- NodeTrace0],
    NodeNodeTrace = 
        [{{Node,ActionNode}, T} || {Node, Ts} <- NodeTrace,
                                   T <- Ts,
                                   ActionNode <- action_nodes(T)],
    {family(NodeNodeTrace), NodeTrace, Trace, Base}.

adjust_nodes([E | Es]) ->
    [adjust_nodes(E) | adjust_nodes(Es)];
adjust_nodes(T) when is_tuple(T) ->
    list_to_tuple(adjust_nodes(tuple_to_list(T)));
adjust_nodes(A) when is_atom(A) ->
    adjust_node(A);
adjust_nodes(E) ->
    E.

sort_nodes(Ts) ->
    [setelement(4, T, lists:sort(element(4, T))) || T <- Ts].

adjust_times([]) ->
    {0, []};
adjust_times([T1 | _]=Ts) ->
    Base = element(2, T1),
    {Base, adjust_times(Ts, Base)}.

adjust_times(Ts, Base) ->
    [setelement(2, adj_tag(T, Base), adjust_time(element(2, T), Base)) || 
        T <- Ts].

adj_tag({Node, Time, {M, Node2}, Nodes, Extra}=T, Base) ->
    if
        M =:= new_resolver;
        M =:= kill_resolver;
        M =:= exit_resolver ->
            {Node, Time, {M, Node2}, Nodes, 
             [adjust_time(hd(Extra), Base) | tl(Extra)]};
        true ->
            T
    end.

adjust_time(Time, Base) ->
    musec2sec(timer:now_diff(Time, Base)).

action_nodes({_Node, _Time, {_, Nodes}, _, _}) when is_list(Nodes) ->
    Nodes;
action_nodes({_Node, _Time, {_, Node}, _, _}) ->
    [Node].

%% Some of the names in global_SUITE.erl are recognized.
adjust_node(Node) ->
    case atom_to_list(Node) of
        "cp" ++ L ->
            list_to_atom([$c, $p | lists:takewhile(fun is_digit/1, L)]);
        "test_server" ++ _ ->
            test_server;
        "a_2" ++ _ ->
            a_2;
        "n_1" ++ _ ->
            n_1;
        "n_2" ++ _ ->
            n_2;
        "z_2" ++ _ ->
            z_2;
        "z_" ++ _ ->
            z;
        "b_" ++ _ ->
            b;
        "c_external_nodes" ++ _ ->
            c_external_nodes;
        _ ->
            Node
    end.

is_digit(C) ->
    (C >= $0) and (C =< $9).

eval(Trace, Fun) ->
    eval(Trace, {0, 0}, #w{}, Fun).

eval([T | Ts], Time0, S0, Fun) ->
    Time1 = element(2, T),
    case is_fresh(S0#w.nodes) of
        true ->
            io:format("~p ***************** FRESH *****************~n",
                      [Time1]);
        false -> 
            ok
    end,
    case time_diff(Time1, Time0) > 0 of 
        true ->
            display_nodes("PAUS", Time1, S0#w.nodes, T);
        false ->
            ok
    end,
    S = eval_trace(T, S0),
    Fun(T, S),
    eval(Ts, Time1, S, Fun);
eval([], _, S, _Fun) ->
    {S#w.nodes, lists:usort(S#w.n)}.

%% Old.
eval_trace({Node, Time, {added,Added}, _Nodes, [_NewNodes,_Abc]}, S0) ->
    added(Node, Added, Time, S0);
eval_trace({Node, Time, {added,Added}, _Nodes, []}, S0) ->
    added(Node, Added, Time, S0);


eval_trace({Node, Time, {init, Node}, Nodes, []}, S0) ->
    init(Node, Nodes, Time, S0);
eval_trace({Node, Time, {nodedown, DownNode}, Nodes, []}, S0) ->
    node_down(Node, DownNode, Nodes, Time, S0);
eval_trace({Node, Time, {extra_nodedown, DownNode}, Nodes, []}, S0) ->
    node_down(Node, DownNode, Nodes, Time, S0);
eval_trace({Node, Time, {nodeup, UpNode}, Nodes, []}, S0) ->
    node_up(Node, UpNode, Nodes, Time, S0);
eval_trace({Node, Time, {added,Added}, _Nodes, [_NewNodes,_Abc,_Ops]}, S0) ->
    added(Node, Added, Time, S0);
eval_trace({Node, Time, {added,Added}, _Nodes, [_Ops]}, S0) ->
    added(Node, Added, Time, S0);
eval_trace({Node, Time, {nodes_changed, {New,Old}}, _Nodes, []}, S0) ->
    nodes_changed(Node, New, Old, Time, S0);
eval_trace({Node, Time, {ins_name, PNode}, _Nodes, [Name, Pid]}, S0) ->
    insert_name(Node, PNode, Time, Name, Pid, S0);
eval_trace({Node, Time, {del_name, PNode}, _Nodes, [Name, Pid]}, S0) ->
    delete_name(Node, PNode, Time, Name, Pid, S0);
eval_trace({Node, Time, {ins_name_ext, PNode}, _Nodes, [Name, Pid]}, S0) ->
    insert_external_name(Node, PNode, Time, Name, Pid, S0);
eval_trace({Node, Time, {ins_lock, PNode}, _Nodes, [Id, Pid]}, S0) ->
    insert_lock(Node, PNode, Time, Id, Pid, S0);
eval_trace({Node, Time, {rem_lock, PNode}, _Nodes, [Id, Pid]}, S0) ->
    remove_lock(Node, PNode, Time, Id, Pid, S0);
eval_trace({Node, Time, {locker_succeeded, _}, _Nodes,{_First,_Known}}, S0) ->
    locker_succeeded(Node, Time, S0);
eval_trace({Node, Time, {lock_rejected, _}, _Nodes, Known}, S0) ->
    lock_rejected(Node, Time, Known, S0);
eval_trace({Node, Time, {locker_failed, _}, _Nodes, {Tried,SoFar}}, S0) ->
    locker_failed(Node, Time, Tried, SoFar, S0);
eval_trace({Node, Time, {new_resolver, RNode}, _Nodes, [Tag, ResPid]}, S0) ->
    new_resolver(Node, Time, RNode, Tag, ResPid, S0);
eval_trace({Node, Time, {kill_resolver, RNode}, _Nodes, [Tag,_ResPid]}, S0) ->
    stop_resolver(Node, Time, RNode, Tag, kill, S0);
eval_trace({Node, Time, {exit_resolver, RNode}, _Nodes, [Tag]}, S0) ->
    stop_resolver(Node, Time, RNode, Tag, exit, S0);
eval_trace(_Ignored, S) ->
io:format("ignored ~p~n", [_Ignored]),
    S.

init(_Node, [], _Time, S) ->
    S;
init(Node, NodesList, Time, S) ->
    io:format("### ~p ~p: already in nodes(): ~p~n", [Node, Time, NodesList]),
    S.

node_down(Node, DownNode, NodesList, Time, S0) ->
    case get_node(Node, S0) of
        {ok, #node{known = Known, nodes = Nodes}=N} ->
            case lists:member(DownNode, Nodes) of
                true ->
                    S1 = case lists:member(DownNode, Known) of
                             true -> 
                                 S0;
                             false ->
                                 io:format("### ~p ~p:~n   "
                                           "nodedown but unknown ~p~n",
                                           [Node, Time, DownNode]),
                                 case lists:member(DownNode, Nodes) of
                                     true ->
                                         io:format("(but note that ~p"
                                                   " is member of nodes())~n",
                                                   [DownNode]);
                                     false ->
                                         ok
                                 end,
                                 add_spurious(Node, DownNode, S0, Time)
                         end,
                    NewKnown = lists:delete(DownNode, Known),
                    NewNodes = lists:delete(DownNode, Nodes),
                    put_node(N#node{known = NewKnown, nodes = NewNodes}, S1);
                false ->
                    io:format("### ~p ~p:~n   spurious nodedown from ~p~n  "
                              "~p~n", [Node, Time, DownNode, NodesList]),
                    NewKnown = lists:delete(DownNode, Known),
                    S1 = put_node(N#node{known = NewKnown,nodes = Nodes}, S0),
                    add_spurious(Node, DownNode, S1, Time)
            end;
        not_ok ->
            io:format("### ~p ~p:~n   unknown node got nodedown from ~p~n", 
                      [Node, Time, DownNode]),
            add_spurious(Node, DownNode, S0, Time)
    end.

node_up(Node, UpNode, NodesList, Time, S) ->
    case get_node(Node, S) of
        {ok, #node{nodes = Nodes}=N} ->
            case lists:member(UpNode, Nodes) of
                true ->
                    io:format("### ~p ~p:~n   spurious nodeup from ~p~n  "
                              "~p~n", [Node, Time, UpNode, NodesList]),
                    add_spurious(Node, UpNode, S, Time);
                false ->
                    put_node(N#node{nodes = lists:sort([UpNode | Nodes])}, S)
            end;
        not_ok ->
            S#w{nodes = [#node{node = Node, nodes = [UpNode]} | S#w.nodes]}
    end.

added(Node, Added, Time,  S0) ->
    case get_node(Node, S0) of
        {ok, #node{known = Known, nodes = Nodes}=N} ->
            case Known -- (Known -- Added) of
                [] ->
                    S1 = put_node(N#node{known = lists:sort(Added ++ Known), 
                                         nodes = Nodes}, S0),
                    case lists:member(Node, Added) of
                        true ->
                            io:format("### ~p ~p:~n   adding node()"
                                      " to known (~p)~n", [Node, Time,Added]),
                            add_spurious(Node, Added, S1, Time);
                        false ->
                            S1
                    end;
                AK ->
                    io:format("### ~p ~p:~n   added already known ~p~n", 
                              [Node, Time, AK]),
                    S1 = put_node(N#node{known = lists:usort(Added ++ Known), 
                                         nodes = Nodes}, S0),
                    add_spurious(Node, AK, S1, Time)
            end;
        not_ok ->
            io:format("### ~p ~p:~n   unknown node got added ~p~n", 
                      [Node, Time, Added]),
            S1 = S0#w{nodes = [#node{node = Node, known = Added} | 
                               S0#w.nodes]},
            add_spurious(Node, Added, S1, Time)
    end.

nodes_changed(Node, New, Old, Time, S) ->
    io:format("### ~p ~p:~n   nodes changed, new are ~p, old are ~p~n",
              [Node, Time, New, Old]),
    S.

insert_external_name(Node, PNode, Time, Name, Pid, S) ->
    insert_name(Node, PNode, Time, Name, Pid, S).

insert_name(Node, PNode, Time, Name, Pid, S0) ->
    RegName = {Name, Pid, PNode},
    case get_node(Node, S0) of
        {ok, #node{names = Names}=N} ->
            case lists:keysearch(Name, 1, Names) of
                {value, {Name, OldPid, OldPNode}} ->
                    io:format("### ~p ~p:~n   name ~p already registered "
                              "for ~p on ~p~n", 
                              [Node, Time, Name, OldPid, OldPNode]),
                    add_spurious(Node, [PNode], S0, Time);
                false ->
                    case lists:keysearch(Pid, 2, Names) of
                        {value, {OldName, Pid, OldPNode}} ->
                            io:format("### ~p ~p:~n   pid ~p already "
                                      "registered as ~p on ~p~n", 
                                      [Node, Time, Pid, OldName, OldPNode]),
                            add_spurious(Node, [PNode], S0, Time);
                        false -> 
                            put_node(N#node{names = [RegName | Names]}, S0)
                    end
            end;
        not_ok ->
            io:format("### ~p ~p:~n   unknown node registered ~p for ~p "
                      "on ~p~n", [Node, Time, Name, Pid, PNode]),
            Known = add_to_known(Node, PNode, []),
            N = #node{node = Node, known = Known, names = [RegName]},
            S1 = S0#w{nodes = [N | S0#w.nodes]},
            add_spurious(Node, [PNode], S1, Time)
    end.

delete_name(Node, PNode, Time, Name, Pid, S0) ->
    case get_node(Node, S0) of
        {ok, #node{names = Names}=N} ->
            case lists:keysearch(Name, 1, Names) of
                {value, {Name, Pid, PNode}} ->
                    NewNames = lists:keydelete(Name, 1, Names),
                    put_node(N#node{names = NewNames}, S0);
                {value, {Name, Pid2, PNode2}} -> % bad log
                    io:format("### ~p ~p:~n   name ~p not registered "
                              "for ~p on ~p but for ~p on ~p~n", 
                              [Node, Time, Name, Pid, PNode, Pid2, PNode2]),
                    add_spurious(Node, [PNode], S0, Time);
                false -> 
                    io:format("### ~p ~p:~n   name ~p not registered "
                              "for ~p on ~p~n", 
                              [Node, Time, Name, Pid, PNode]),
                    add_spurious(Node, [PNode], S0, Time)
            end;
        not_ok ->
            io:format("### ~p ~p:~n   unknown node deleted ~p for ~p on ~p~n",
                      [Node, Time, Name, Pid, PNode]),
            Known = add_to_known(Node, PNode, []),
            N = #node{node = Node, known = Known},
            S1 = S0#w{nodes = [N | S0#w.nodes]},
            add_spurious(Node, [PNode], S1, Time)
    end.

insert_lock(Node, PNode, Time, Id, Pid, S0) ->
    Lock = {Pid, PNode},
    case get_node(Node, S0) of
        {ok, #node{locks = NLocks}=N} ->
            case lists:keysearch(Id, 1, NLocks) of
                {value, {Id, OldLocks}} ->
                    case lists:member(Lock, OldLocks) of
                        true ->
                            io:format("### ~p ~p:~n   lock ~p already set "
                                      "for ~p on ~p~n", 
                                      [Node, Time, Id, Pid, PNode]),
                            %% This is not so strange, actually. 
                            add_spurious(Node, [PNode], S0, Time);
                        false ->
                            NewLocks = {Id, [Lock | OldLocks]},
                            Ls = lists:keyreplace(Id, 1, NLocks, NewLocks),
                            put_node(N#node{locks = Ls}, S0)
                    end;
                false ->
                    put_node(N#node{locks = [{Id,[Lock]}|N#node.locks]}, S0)
            end;
        not_ok ->
            Known = add_to_known(Node, PNode, []),
            N = #node{node = Node, known = Known, locks = [{Id, [Lock]}]},
            S1 = S0#w{nodes = [N | S0#w.nodes]},
            if 
                Node =/= PNode ->
                    io:format("### ~p ~p:~n   unknown pid ~p locked ~p on "
                              "~p~n", [Node, Time, Pid, Id, PNode]),
                    add_spurious(Node, [PNode], S1, Time);
                true ->
                    S1
            end
    end.

remove_lock(Node, PNode, Time, Id, Pid, S0) ->
    Lock = {Pid, PNode},
    case get_node(Node, S0) of
        {ok, #node{locks = NLocks}=N} ->
            case lists:keysearch(Id, 1, NLocks) of
                {value, {Id, OldLocks}} ->
                    case lists:member(Lock, OldLocks) of
                        true ->
                            NewLocks = lists:delete(Lock, OldLocks),
                            Ls = case NewLocks of
                                     [] ->
                                         lists:keydelete(Id, 1, NLocks);
                                     _ ->
                                         lists:keyreplace(Id, 1, NLocks, 
                                                          {Id, NewLocks})
                                 end,
                            put_node(N#node{locks = Ls}, S0);
                        false ->
                            io:format("### ~p ~p:~n   lock ~p not set "
                                      "by ~p on ~p~n", 
                                      [Node, Time, Id, Pid, PNode]),
                            add_spurious(Node, [PNode], S0, Time)
                    end;
                false ->
                    io:format("### ~p ~p:~n   lock ~p not set "
                              "by ~p on ~p~n", 
                              [Node, Time, Id, Pid, PNode]),
                    add_spurious(Node, [PNode], S0, Time)
            end;
        not_ok ->
            io:format("### ~p ~p:~n   ~p unlocked ~p on unknown node ~p~n",
                      [Node, Time, Pid, Id, PNode]),
            Known = add_to_known(Node, PNode, []),
            N = #node{node = Node, known = Known},
            S1 = S0#w{nodes = [N | S0#w.nodes]},
            add_spurious(Node, [PNode], S1, Time)
    end.
    
%% This is just statistics...
locker_succeeded(Node, Time, S0) ->
    case get_node(Node, S0) of
        {ok, #node{n_locks = {Ok,Boss,NodeX,Bad}}=N} ->
            put_node(N#node{n_locks = {Ok+1,Boss,NodeX,Bad}}, S0);
        not_ok ->
            io:format("### ~p ~p:~n   unknown node's locker succeeded~n", 
                      [Node, Time]),
            add_spurious(Node, [Node], S0, Time)
    end.

lock_rejected(Node, Time, _Known, S0) ->
    case get_node(Node, S0) of
        {ok, #node{rejected = Rej}=N} ->
            put_node(N#node{rejected = Rej+1}, S0);
        not_ok ->
            io:format("### ~p ~p:~n   unknown node's lock rejected~n", 
                      [Node, Time]),
            add_spurious(Node, [Node], S0, Time)
    end.

locker_failed(Node, Time, Tried, SoFar, S0) ->
    case get_node(Node, S0) of
        {ok, #node{known = Known, n_locks = {Ok,Boss,NodeX,Bad}}=N} ->
            TheBoss = lists:max([Node | Known]),
            Cheap = (Tried =:= [TheBoss]),
            RatherCheap = ((SoFar -- [Node, TheBoss]) =:= []) and
                          ((Tried -- [Node, TheBoss]) =/= []),
            if
                Cheap ->
                    put_node(N#node{n_locks = {Ok,Boss+1,NodeX,Bad}}, S0);
                RatherCheap ->
                    put_node(N#node{n_locks = {Ok,Boss,NodeX+1,Bad}}, S0);
                true ->
                    put_node(N#node{n_locks = {Ok,Boss,NodeX,Bad+1}}, S0)
            end;
        not_ok ->
            io:format("### ~p ~p:~n   unknown node's locker failed~n", 
                      [Node, Time]),
            add_spurious(Node, [Node], S0, Time)
    end.

new_resolver(Node, Time, ResNode, Tag, ResPid, S0) ->
    case get_node(Node, S0) of
        {ok, #node{resolvers = Rs}=N} ->
            put_node(N#node{resolvers = [{ResNode, Tag, ResPid} | Rs]}, S0);
        not_ok ->
            io:format("### ~p ~p:~n   resolver created for unknown node~n", 
                      [Node, Time]),
            add_spurious(Node, [Node], S0, Time)
    end.

stop_resolver(Node, Time, ResNode, Tag, How, S0) ->
    case get_node(Node, S0) of
        {ok, #node{resolvers = Rs}=N} ->
            case lists:keysearch(Tag, 2, Rs) of
                {value, {ResNode, Tag, _ResPid}} ->
                    NewRs = lists:keydelete(Tag, 2, Rs),
                    put_node(N#node{resolvers = NewRs}, S0);
                false ->
                    case lists:keysearch(ResNode, 1, Rs) of
                        {value, {ResNode, _Tag2, _ResPid2}} ->
                            NewRs = lists:keydelete(ResNode, 1, Rs),
                            put_node(N#node{resolvers = NewRs}, S0);
                        false when How =:= exit -> 
                            io:format("### ~p ~p:~n   there is no resolver "
                                      "with tag ~p on node ~p~n", 
                                      [Node, Time, Tag, ResNode]),
                            add_spurious(Node, [ResNode], S0, Time);
                        false when How =:= kill ->
                            S0
                    end
            end;
        not_ok ->
            io:format("### ~p ~p:~n   resolver stopped for unknown node~n", 
                      [Node, Time]),
            add_spurious(Node, [Node], S0, Time)
    end.

add_to_known(Node, NodeToAdd, Known) ->
    if
        Node =:= NodeToAdd ->
            Known;
        true ->
            lists:sort([NodeToAdd | Known])
    end.

get_node(Node, S) ->
    case lists:keysearch(Node, #node.node, S#w.nodes) of
        {value, N} ->
            {ok, N};
        false ->
            not_ok
    end.

put_node(#node{node = Node, known = [], nodes = [], locks = [], names = [], 
               n_locks = {0,0,0,0}}, 
         S) ->
    S#w{nodes = lists:keydelete(Node, #node.node, S#w.nodes)};
put_node(N, S) ->
    S#w{nodes = lists:keyreplace(N#node.node, #node.node, S#w.nodes, N)}.

is_fresh(#node{known = [], nodes = [], locks = [], names = []}) ->
    true;
is_fresh(#node{}) ->
    false;
is_fresh([]) ->
    true;
is_fresh([N | Ns]) ->
    is_fresh(N) andalso is_fresh(Ns).

add_spurious(Node, ActionNodes, S, Time) when is_list(ActionNodes) ->
    S#w{n = [{{Node,N},Time}|| N <- ActionNodes] ++ S#w.n};
add_spurious(Node, ActionNode, S, Time) ->
    add_spurious(Node, [ActionNode], S, Time).

messages(D, Base, End) ->
    messages1(no_info(D), no_info), 
    messages1(resolvers(D, Base, End), resolvers),
    messages1(syncers(D), syncers).

messages1(M, ST) ->
    [foo || {Node, T} <- M,
            ok =:= io:format(ms(ST), [Node, T])].

ms(no_info) ->
    "~p: ~p~n";    
ms(resolvers) ->
    "~p: resolvers ~p~n";
ms(syncers) ->
    "~p: syncers ~p~n".

no_info(D) ->
    [{Node,no_info} || {Node, no_info} <- D].

resolvers(D, Base, End) ->
    [{Node,
      [{N,adjust_time(T, Base),P} || {N, T, P} <- Rs, T < End]} || 
        {Node, {info,State}} <- D,
        is_record(State, state),
        [] =/= (Rs = (state(State))#state.resolvers)].

syncers(D) ->
    [{Node,Ss} || {Node, {info,State}} <- D,
                  is_record(State, state),
                  [] =/= (Ss = (state(State))#state.syncers)].

net_kernel_nodes(NodeTrace) ->
    [{Node, nkn(Trace, [])} || {Node, Trace} <- NodeTrace].

nkn([], _Nodes) ->
    [];
nkn([{Node, Time, _Message, Ns, _X} | Ts], Nodes) ->
    {NewS, _, OldS} = sofs:symmetric_partition(sofs:set(Ns), sofs:set(Nodes)),
    New = sofs:to_external(NewS),
    Old = sofs:to_external(OldS),
    [{Node, Time, {newnode, N}, []} || N <- New] ++
    [{Node, Time, {oldnode, N}, []} || N <- Old] ++
    nkn(Ts, (Nodes -- Old) ++ New).

negotiations(Trace) ->
    Ns = [{Node,T,Added,X} || 
             {Node,T,{added,Added},_Nodes,X} <- Trace],
    Pass = [{passive,Node,T,Added} || 
               {Node,T,Added,[_Ops]} <- Ns],
    Act = [{active,Node,T,Other,Added,NewNodes} ||
              {Node,T,Added,[{new_nodes,[Other|_]=NewNodes},_Abcast,_Ops]} <- Ns],
    Act ++ Pass.

show_spurious(NodeTrace, Spurious) ->
    Pairs = [{Node,ActionNode} || {{Node,ActionNode}, _Time} <- Spurious],
    S = sofs:restriction(sofs:relation(NodeTrace), sofs:set(Pairs)),
    [foo || 
        {{{Node,ANode},Times},
         {{Node,ANode},Ts}} <- lists:zip(family(Spurious), 
                                         sofs:to_external(S)),
        show_spurious(Node, ANode, Times, lists:keysort(2, Ts))].

show_spurious(Node, ActionNode, Times, Ts) ->
    io:format("** Actions for ~p on node ~p **~n", [ActionNode, Node]),
    lists:map(fun(T) -> spurious(Node, T, Times) end, Ts),
    io:format("-- End of actions for ~p on node ~p --~n", [ActionNode, Node]),
    true.

spurious(Node, Trace, Times) ->
    As = case Trace of
             {Node, _T0, {init, Node}, _Nodes, _} ->
                 init; % should not happen, I guess
             {Node, _T0, {nodedown, _ActionNode}, _Nodes, _} ->   
                 nodedown;
             {Node, _T0, {extra_nodedown, _ActionNode}, _Nodes, _} ->
                 extra_nodedown;
             {Node, _T0, {nodeup, _ActionNode}, _Nodes, _} ->
                 nodeup;
             {Node, _T0, {added, Added}, _Nodes, [_Ops]} ->
                 {passive, Added};
             {Node, _T0, {added, Added}, _Nodes, [_NewNodes,_AbCast,_Ops]} ->
                 {negotiator, Added};
             {Node, _T0, {ins_lock, PNode}, _Nodes, [Id, Pid]} ->
                 {insert_lock, [Id, Pid, PNode]};
             {Node, _T0, {rem_lock, PNode}, _Nodes, [Id, Pid]} ->
                 {remove_lock, [Id, Pid, PNode]};
             {Node, _T0, {ins_name, PNode}, _Nodes, [Name, Pid]} ->
                 {insert_name, [Name, Pid, PNode]};
             {Node, _T0, {del_name, PNode}, _Nodes, [Name, Pid]} ->
                 {insert_name, [Name, Pid, PNode]};
             {Node, _T0, {nodes_changed, CNode}, _Nodes, []} ->
                 {nodes_changed, [CNode]};
             {Node, _T0, {Any, Some}, _Nodes, X} ->
                 {Any, [Some | X]}
        end,
    T = element(2, Trace),
    _Nodes2 = element(4, Trace),
    TS = ["(spurious)" || lists:member(T, Times)],
    io:format("~p: ~p ~s~n", [T, As, TS]),
%    io:format("  ~w~n", [_Nodes2]),
    ok.

display_nodes(Why, Time, Nodes) ->
    display_nodes(Why, Time, Nodes, none).

display_nodes(Why, Time, Nodes, LastTrace) ->
    io:format("~p **** ~s ****~n", [Time, Why]),
    {OkL, BossL, NodeXL, BadL} = unzip4([L || #node{n_locks = L} <- Nodes]),
    [NOk, NBoss, NNodeX, NBad] = 
        [lists:sum(L) || L <- [OkL, BossL, NodeXL, BadL]],
    Rejected = lists:sum([Rej || #node{rejected = Rej} <- Nodes]),
    io:format("Locks: (~w+~w+~w=~w)/~w, ~w of ~w rejected~n", 
              [NOk, NBoss, NNodeX, NOk+NBoss+NNodeX, NOk+NBoss+NNodeX+NBad,
               Rejected, NOk]),
    lists:foreach(fun(#node{node = Node, known = Known, nodes = Ns,
                            locks = Locks, names = Names, 
                            n_locks = {Ok, Boss, NodeX, Bad},
                            resolvers = Resolvers0,
                            rejected = Rej}) ->
                          NodeL = io_lib:format("~p: ",[Node]),
                          io:format("~sknown ~p~n", [NodeL, Known]),
                          Sp = spaces(NodeL),
                          case Ns =:= Known of
                              true -> ok;
                              false -> display_list(Sp, nodes, Ns)
                          end,
                          display_list(Sp, locks, Locks),
                          display_list(Sp, names, lists:sort(Names)),
                          Resolvers = lists:sort(Resolvers0),
                          _ResNs = [R || {R,_,_} <- Resolvers],
                          %% Should check trace on this node (Node) only:
                          New = [N || {_,_,{nodeup,N},_,_} <- [LastTrace]],
                          _ResAllowed = (Ns -- New) -- Known,
%% Displays too much junk.
%                           case ResAllowed =:= ResNs of
%                               true -> ok;
%                               false -> display_list(Sp, resol, Resolvers)
%                           end,
                          %% This is less bulky:
                          case Known =:= Ns of
                              true -> display_list(Sp, resol, Resolvers);
                              false -> ok
                          end,
                          case {Ok, Boss, NodeX, Bad} of
                              {0, 0, 0, 0} -> ok;
                              _ -> io:format("~slocks (~w+~w+~w=~w)/~w, "
                                             "~w of ~w rejected~n", 
                                           [Sp, Ok, Boss, NodeX,
                                            Ok+Boss+NodeX,Ok+Boss+NodeX+Bad,
                                            Rej, Ok])
                          end
                  end, lists:keysort(#node.node, Nodes)),
    io:format("\n").

display_list(_S, _What, []) ->
    ok;
display_list(S, What, L) ->
    io:format("~s~p ~p~n", [S, What, L]).

spaces(Iolist) ->
    lists:duplicate(iolist_size(Iolist), $\s).

family(R) ->
    sofs:to_external(sofs:relation_to_family(sofs:relation(R))).

time_diff({S1,MyS1}, {S0,MyS0}) ->
    ((S1*1000000+MyS1) - (S0*1000000+MyS0)) div 1000000.

musec2sec(T) ->
    S = T div 1000000,
    M = (T - S * 1000000),
    {S, M}.

%%% Options

options(Options, Keys) when is_list(Options) ->
    options(Options, Keys, []);
options(Option, Keys) ->
    options([Option], Keys, []).

options(Options0, [Key | Keys], L) when is_list(Options0) ->
    Options = case lists:member(Key, Options0) of
                  true -> 
                      [atom_option(Key) | lists:delete(Key, Options0)];
                  false ->
                      Options0
              end,
    V = case lists:keysearch(Key, 1, Options) of
            {value, {show_state, From, To}} when is_integer(From), From >= 0,
                                                 is_integer(To), To >= From ->
                {ok, {{From,0}, {To,0}}};
            {value, {show_state, {From, FromMusec}, 
                                  {To, ToMusec}}} when is_integer(From), 
                                                       From >= 0,
                                                       is_integer(To), 
                                                       To >= From,
                                                       FromMusec >= 0,
                                                       FromMusec =< 999999,
                                                       ToMusec >= 0,
                                                       ToMusec =< 999999 ->
                {ok, {{From,FromMusec}, {To,ToMusec}}};
            {value, {show_state, false}} ->
                {value, default_option(show_state)};
            {value, {show_trace, Bool}} when Bool; not Bool ->
                {ok, Bool};
            {value, {Key, _}} ->
                badarg;
            false ->
                Default = default_option(Key),
                {ok, Default}
        end,
    case V of
        badarg ->
            badarg;
        {ok, Value} ->
            NewOptions = lists:keydelete(Key, 1, Options),
            options(NewOptions, Keys, [Value | L])
    end;
options([], [], L) ->
    lists:reverse(L);
options(_Options, _, _L) ->
    badarg.

default_option(show_state) -> {{0,0}, {0,0}};
default_option(show_trace) -> true.

atom_option(show_state) ->
    {show_state, 0, 1 bsl 28};
atom_option(show_trace) ->
    {show_trace, true};
atom_option(_) ->
    erlang:error(program_error, []).

unzip4(Ts) -> unzip4(Ts, [], [], [], []).

unzip4([{X, Y, Z, W} | Ts], Xs, Ys, Zs, Ws) ->
    unzip4(Ts, [X | Xs], [Y | Ys], [Z | Zs], [W | Ws]);
unzip4([], Xs, Ys, Zs,  Ws) ->
    {lists:reverse(Xs), lists:reverse(Ys), 
     lists:reverse(Zs), lists:reverse(Ws)}.

