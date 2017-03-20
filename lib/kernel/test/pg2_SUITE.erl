%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
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
%%----------------------------------------------------------------
%% Purpose:Test Suite for the 'pg2' module.
%%-----------------------------------------------------------------
-module(pg2_SUITE).

-include_lib("common_test/include/ct.hrl").
-define(datadir, proplists:get_value(data_dir, Config)).
-define(privdir, proplists:get_value(priv_dir, Config)).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, 
	 init_per_testcase/2, end_per_testcase/2]).

-export([
	 otp_7277/1, otp_8259/1, otp_8653/1,
         basic/1]).

-define(TESTCASE, testcase_name).
-define(testcase, proplists:get_value(?TESTCASE, Config)).

%% Internal export.
-export([mk_part_node_and_group/3, part2/4,
         mk_part_node/3, part1/5, p_init/3, start_proc/1, sane/0]).

init_per_testcase(Case, Config) ->
    [{?TESTCASE, Case}| Config].

end_per_testcase(_Case, _Config) ->
    test_server_ctrl:kill_slavenodes(),
    ok.

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() -> 
    [{group, tickets}].

groups() -> 
    [{tickets, [],
      [otp_7277, otp_8259, otp_8653, basic]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.



%% OTP-7277. Bugfix leave().
otp_7277(Config) when is_list(Config) ->
    ok = pg2:create(a),
    ok = pg2:create(b),
    P = spawn(forever()),
    ok = pg2:join(a, P),
    ok = pg2:leave(b, P),
    true = exit(P, kill),
    case {pg2:get_members(a), pg2:get_local_members(a)} of
        {[], []} ->
            ok;
        _ ->
            timer:sleep(100),
            [] = pg2:get_members(a),
            [] = pg2:get_local_members(a)
    end,
    _ = pg2:delete(a),
    _ = pg2:delete(b),
    ok.

-define(UNTIL(Seq), loop_until_true(fun() -> Seq end, Config)).
-define(UNTIL_LOOP, 300).

%% OTP-8259. Member was not removed after being killed.
otp_8653(Config) when is_list(Config) ->
    [A, B, C] = start_nodes([a, b, c], peer, Config),

    wait_for_ready_net(Config),

    %% make b and c connected, partitioned from node() and a
    rpc_cast(B, ?MODULE, part2, [Config, node(), A, C]),
    ?UNTIL(is_ready_partition(Config)),

    %% Connect to the other partition.
    pong = net_adm:ping(B),
    timer:sleep(100),
    pong = net_adm:ping(C),
    _ = global:sync(),
    [A, B, C] = lists:sort(nodes()),

    G = pg2_otp_8653,
    ?UNTIL(begin
	       GA = lists:sort(rpc:call(A, pg2, get_members, [G])),
	       GB = lists:sort(rpc:call(B, pg2, get_members, [G])),
	       GC = lists:sort(rpc:call(C, pg2, get_members, [G])),
	       GT = lists:sort(pg2:get_members(G)),
	       GA =:= GB andalso
		   GB =:= GC andalso
		   GC =:= GT andalso
		   8 =:= length(GA)
	   end),
    ok = pg2:delete(G),
    stop_nodes([A,B,C]),
    ok.

part2(Config, Main, A, C) ->
    Function = mk_part_node_and_group,
    case catch begin
		   make_partition(Config, [Main, A], [node(), C], Function)
	       end
    of
	ok -> ok
    end.

mk_part_node_and_group(File, MyPart0, Config) ->
    touch(File, "start"), % debug
    MyPart = lists:sort(MyPart0),
    ?UNTIL(is_node_in_part(File, MyPart)),
    G = pg2_otp_8653,
    Pid = spawn(forever()),
    ok = pg2:create(G),
    _ = [ok = pg2:join(G, Pid) || _ <- [1,1]],
    touch(File, "done").

%% OTP-8259. Member was not removed after being killed.
otp_8259(Config) when is_list(Config) ->
    [A, B, C] = start_nodes([a, b, c], peer, Config),

    wait_for_ready_net(Config),

    G = pg2_otp_8259,
    Name = otp_8259_a_global_name,

    %% start different processes in both partitions
    {Pid, yes} = rpc:call(A, ?MODULE, start_proc, [Name]),

    ok = pg2:create(G),
    ok = pg2:join(G, Pid),

    %% make b and c connected, partitioned from node() and a
    rpc_cast(B, ?MODULE, part1, [Config, node(), A, C, Name]),
    ?UNTIL(is_ready_partition(Config)),

    %% Connect to the other partition.
    %% The resolver on node b will be called.
    pong = net_adm:ping(B),
    timer:sleep(100),
    pong = net_adm:ping(C),
    _ = global:sync(),
    [A, B, C] = lists:sort(nodes()),

    %% Pid has been killed by the resolver.
    %% Pid has been removed from pg2 on all nodes, in particular node B.
    ?UNTIL([] =:= rpc:call(B, pg2, get_members, [G])),
    ?UNTIL([] =:= pg2:get_members(G)),
    ?UNTIL([] =:= rpc:call(A, pg2, get_members, [G])),
    ?UNTIL([] =:= rpc:call(C, pg2, get_members, [G])),

    ok = pg2:delete(G),
    stop_nodes([A,B,C]),
    ok.

part1(Config, Main, A, C, Name) ->
    case catch begin
		   make_partition(Config, [Main, A], [node(), C]),
		   {_Pid, yes} = start_proc(Name)
	       end of
	{_, yes} -> ok
    end.

start_proc(Name) ->
    Pid = spawn(?MODULE, p_init, [self(), Name, node()]),
    receive
	{Pid, Res} -> {Pid, Res}
    end.

p_init(Parent, Name, TestServer) ->
    Resolve = fun(_Name, Pid1, Pid2) ->
                      %% The pid on node a will be chosen.
                      [{_,Min}, {_,Max}] =
                          lists:sort([{node(Pid1),Pid1}, {node(Pid2),Pid2}]),
                      %% b is connected to test_server.
                      %% exit(Min, kill), % would ping a
                      rpc:cast(TestServer, erlang, exit, [Min, kill]),
		      Max
              end,
    X = global:register_name(Name, self(), Resolve),
    Parent ! {self(),X},
    loop().

loop() ->
    receive
	die ->
	    exit(normal)
    end.

%% OTP-8259. Some basic tests.
basic(Config) when is_list(Config) ->
    _ = [pg2:delete(G) || G <- pg2:which_groups()],
    _ = [do(Cs, T, Config) || {T,Cs} <- ts()],
    ok.

ts() ->
    [
     {t1,
      [{create,[a],ignore},
       {which_groups,[],[a]},
       {get_closest_pid,[a],{error, {no_process, a}}},
       {delete,[a],ignore}]},
     {t2,
      [{create,[a],ignore},
       {join,[a,self()],ok},
       {get_closest_pid,[a],self()},
       {delete,[a],ignore}]},
     {t3,
      [{create,[a],ignore},
       {new,p1},
       {leave,[a,p1],ok},
       {join,[b,p1],{error,{no_such_group,b}}},
       {leave,[b,p1],{error,{no_such_group,b}}},
       {get_members,[c],{error,{no_such_group,c}}},
       {get_local_members,[c],{error,{no_such_group,c}}},
       {join,[a,p1],ok},
       {leave,[a,p1],ok},
       {join,[a,p1],ok},
       {join,[a,p1],ok},
       {create,[a],ignore},
       {get_closest_pid,[a],p1},
       {leave,[a,p1],ok},
       {get_closest_pid,[a],p1},
       {leave,[a,p1],ok},
       {get_closest_pid,[a],{error,{no_process, a}}},
       {kill,p1},
       {delete,[a],ignore}]},
     {t4,
      [{create,[a],ignore},
       {new,p1},
       {join,[a,p1],ok},
       {get_members,[a],[p1]},
       {get_local_members,[a],[p1]},
       {kill,p1},
       {get_members,[a],[]},
       {get_local_members,[a],[]},
       {delete,[a],ignore}]},
     {t5,
      [{create,[a],ignore},
       {nodeup,n1},
       {create,[a],ignore},
       {join,[a,self()],ok},
       {new,n1,p1},
       {n1,{create,[b],ignore}},
       {join,[a,p1],ok},
       {join,[b,p1],ok},
       {n1,{which_groups,[],[a,b]}},
       {n1,{join,[a,p1],ok}},
       {n1,{join,[b,p1],ok}},
       {leave,[a,self()],ok},
       {n1,{leave,[a,self()],ok}}, % noop
       {n1,{leave,[b,p1],ok}},
       {leave,[b,p1],ok},
       {kill,n1,p1},
       {nodedown,n1},
       {delete,[b],ignore},
       {delete,[a],ignore}]},
     {t6,
      [{create,[a],ignore}, % otp_7277
       {create,[b],ignore},
       {new,p},
       {join,[a,p],ok},
       {leave,[b,p],ok},
       {kill,p},
       {get_members,[a],[]},
       {get_local_members,[a],[]},
       {delete,[a],ignore},
       {delete,[b],ignore}]},
     {t7, % p1 joins twice, the new node gets informed about that
      [{create,[a],ignore},
       {new,p1},
       {join,[a,p1],ok},
       {join,[a,p1],ok},
       {get_members,[a],[p1,p1]},
       {get_local_members,[a],[p1,p1]},
       {nodeup,n1},
       {leave,[a,p1],ok},
       {get_members,[a],[p1]},
       {get_local_members,[a],[p1]},
       {n1,{get_members,[a],[p1]}},
       {leave,[a,p1],ok},
       {get_members,[a],[]},
       {n1,{get_members,[a],[]}},
       {nodedown,n1},
       {delete,[a],ignore},
       {kill,p1}]},
     {t8,
      [{create,[a],ignore},
       {new,p1},
       {join,[a,p1],ok},
       {join,[a,p1],ok},
       {delete,[a],ignore},
       {get_members,[a],{error,{no_such_group,a}}},
       {kill,p1}]}
    ].

do(Cs, T, Config) ->
    io:format("*** Test ~p ***~n", [T]),
    {ok,T} = (catch {do(Cs, [], [], Config),T}).

do([{nodeup,N} | Cs], Ps, Ns, Config) ->
    [TestNode] = start_nodes([N], peer, Config),
    pr(node(), {nodeup,N,TestNode}),
    global:sync(),
    timer:sleep(100),
    {ok,_} = rpc:call(TestNode, pg2, start, []),
    NNs = [{N,TestNode} | Ns],
    sane(NNs),
    do(Cs, Ps, NNs, Config);
do([{nodedown,N}=C | Cs], Ps, Ns, Config) ->
    {N, TestNode} = lists:keyfind(N, 1, Ns),
    stop_node(TestNode),
    timer:sleep(100),
    pr(node(), C),
    do(Cs, Ps, lists:keydelete(N, 1, Ns), Config);
do([{new,P} | Cs], Ps, Ns, Config) ->
    NPs = new_proc(node(), P, Ps, Ns),
    do(Cs, NPs, Ns, Config);
do([{new,N,P} | Cs], Ps, Ns, Config) ->
    NPs = new_proc(N, P, Ps, Ns),
    do(Cs, NPs, Ns, Config);
do([{kill,P} | Cs], Ps, Ns, Config) ->
    NPs = killit(node(), P, Ps, Ns),
    do(Cs, NPs, Ns, Config);
do([{kill,N,P} | Cs], Ps, Ns, Config) ->
    NPs = killit(N, P, Ps, Ns),
    do(Cs, NPs, Ns, Config);
do([{Node,{_,_,_}=C} | Cs], Ps, Ns, Config) ->
    doit(Node, C, Ps, Ns),
    do(Cs, Ps, Ns, Config);
do([C | Cs], Ps, Ns, Config) ->
    doit(node(), C, Ps, Ns),
    do(Cs, Ps, Ns, Config);
do([], Ps, Ns, _Config) ->
    [] = Ns,
    [] = Ps,
    [] = pg2:which_groups(),
    [] = ets:tab2list(pg2_table),
    [] = nodes(),
    ok.

doit(N, C, Ps, Ns) ->
    Node = get_node(N, Ns),
    pr(Node, C),
    {F,As,R} = replace_pids(C, Ps),
    case rpc:call(Node, erlang, apply, [pg2, F, As]) of
        Result when Result =:= R orelse R =:= ignore ->
            sane(Ns);
        Else ->
            io:format("~p and ~p: expected ~p, but got ~p~n",
                      [F, As, R, Else]),
            throw({error,{F, As, R, Else}})
    end.

new_proc(N, P, Ps, Ns) ->
    Node = get_node(N, Ns),
    Pid = rpc:call(Node, erlang, spawn, [forever()]),
    pr(Node, {new,P,Pid}),
    [{P,Pid}|Ps].

killit(N, P, Ps, Ns) ->
    {P, Pid} = lists:keyfind(P, 1, Ps),
    Node = get_node(N, Ns),
    pr(Node, {kill,P,Pid}),
    rpc:call(Node, erlang, exit, [Pid, kill]),
    timer:sleep(100),
    sane(Ns),
    lists:keydelete(P, 1, Ps).

pr(Node, C) ->
    _ = [io:format("~p: ", [Node]) || Node =/= node()],
    io:format("do ~p~n", [C]).

get_node(N, Ns) ->
    if
        N =:= node() ->
            node();
        true ->
            {N, TestNode} = lists:keyfind(N, 1, Ns),
            TestNode
    end.

forever() ->
    fun() -> receive after infinity -> ok end end.

replace_pids(T, Ps) when is_tuple(T) ->
    list_to_tuple(replace_pids(tuple_to_list(T), Ps));
replace_pids([E | Es], Ps) ->
    [replace_pids(E, Ps) | replace_pids(Es, Ps)];
replace_pids(A, Ps) ->
    case lists:keyfind(A, 1, Ps) of
        {A, Pid} ->
            Pid;
        _ ->
            A
    end.

sane(Ns) ->
    Nodes = [node()] ++ [NN || {_,NN} <- Ns],
    _ = [io:format("~p, pg2_table:~n   ~p~n",  % debug
                   [N, rpc:call(N, ets, tab2list, [pg2_table])]) ||
            N <- Nodes],
    R = [case rpc:call(Node, ?MODULE, sane, []) of
             {'EXIT',Error} ->
                 {error, Node, Error};
             _ ->
                 ok
         end || Node <- Nodes],
    case lists:usort(R) of
        [ok] -> wsane(Nodes);
        _ -> throw(R)
    end.

wsane(Ns) ->
    %% Same members on all nodes:
    {[_],gs} =
        {lists:usort([rpc:call(N, pg2, which_groups, []) || N <- Ns]),gs},
    _ = [{[_],ms,G} = {lists:usort([rpc:call(N, pg2, get_members, [G]) ||
                                       N <- Ns]),ms,G} ||
            G <- pg2:which_groups()],
    %% The local members are a partitioning of the members:
    [begin
         LocalMembers =
             lists:sort(lists:append(
                          [rpc:call(N, pg2, get_local_members, [G]) ||
                              N <- Ns])),
         {part, LocalMembers} = {part, lists:sort(pg2:get_members(G))}
     end || G <- pg2:which_groups()],
    %% The closest pid should run on the local node, if possible.
    [[case rpc:call(N, pg2, get_closest_pid, [G]) of
          Pid when is_pid(Pid), node(Pid) =:= N ->
              true =
                  lists:member(Pid, rpc:call(N, pg2, get_local_members, [G]));
	  %% FIXME. Om annan nod: member, local = [].
          _ -> [] = rpc:call(N, pg2, get_local_members, [G])
      end || N <- Ns]
     || G <- pg2:which_groups()].

%% Look inside the pg2_table.
sane() ->
    L = ets:tab2list(pg2_table),
    Gs = lists:sort([G || {{group,G}} <- L]),
    MGs = lists:usort([G || {{member,G,_},_} <- L]),
    MPs = lists:usort([P || {{member,_,P},_} <- L]),
    {[],mg,MGs,Gs} = {MGs -- Gs,mg,MGs,Gs},
    RPs = [P || {{ref,P},_RPid,_Ref,_C} <- L],
    {MPs,rp} = {RPs,rp},
    RPs2 = [P || {{ref,_Ref},P} <- L],
    {MPs,rp2} = {RPs2,rp2},
    _ = [true = C >= 1 || {{ref,_P},_RPid,_Ref,C} <- L],
    LGs = lists:usort([G || {{local_member,G,_}} <- L]),
    LPs = lists:usort([P || {{local_member,_,P}} <- L]),
    {[],lg} = {LGs -- Gs,lg},
    {[],lp} = {LPs -- MPs,lp},
    PGs = lists:usort([G || {{pid,_,G}} <- L]),
    PPs = lists:usort([P || {{pid,P,_}} <- L]),
    {[],pg} = {PGs -- Gs,pg},
    {MPs,pp} = {PPs,pp},
    _ = [true = C >= 1 || {{member,_,_},C} <- L],
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Mostly copied from global_SUITE.erl
%% (Setting up a partition is quite tricky.)

loop_until_true(Fun, Config) ->
    case Fun() of
	true ->
	    true;
	_ ->
            timer:sleep(?UNTIL_LOOP),
            loop_until_true(Fun, Config)
    end.

start_node_rel(Name, Rel, How) ->
    {Release, Compat} = case Rel of
                            this ->
                                {[this], "+R8"};
                            Rel when is_atom(Rel) ->
                                {[{release, atom_to_list(Rel)}], ""};
                            RelList ->
				{RelList, ""}
			end,
    Pa = filename:dirname(code:which(?MODULE)),
    Res = test_server:start_node(Name, How,
				 [{args,
				   Compat ++
				       " -kernel net_setuptime 100 "
				   " -pa " ++ Pa},
				  {erl, Release}]),
    Res.

start_nodes(L, How, Config) ->
    start_nodes2(L, How, 0, Config),
    Nodes = collect_nodes(0, length(L)),
    ?UNTIL([] =:= Nodes -- nodes()),
    %% Pinging doesn't help, we have to wait too, for nodes() to become
    %% correct on the other node.
    lists:foreach(fun(E) ->
			  net_adm:ping(E)
		  end,
		  Nodes),
    verify_nodes(Nodes, Config),
    Nodes.

verify_nodes(Nodes, Config) ->
    verify_nodes(Nodes, lists:sort([node() | Nodes]), Config).

verify_nodes([], _N, _Config) ->
    [];
verify_nodes([Node | Rest], N, Config) ->
    ?UNTIL(
       case rpc:call(Node, erlang, nodes, []) of
	   Nodes when is_list(Nodes) ->
	       case N =:= lists:sort([Node | Nodes]) of
		   true ->
		       true;
		   false ->
		       lists:foreach(fun(Nd) ->
					     rpc:call(Nd, net_adm, ping,
                                                      [Node])
				     end,
				     nodes()),
		       false
	       end;
	   _ ->
	       false
       end
      ),
    verify_nodes(Rest, N, Config).


start_nodes2([], _How, _, _Config) ->
    [];
start_nodes2([Name | Rest], How, N, Config) ->
    Self = self(),
    spawn(fun() ->
		  erlang:display({starting, Name}),
		  {ok, R} = start_node(Name, How, Config),
		  erlang:display({started, Name, R}),
		  Self ! {N, R},
		  %% sleeping is necessary, or with peer nodes, they will
		  %% go down again, despite {linked, false}.
		  ct:sleep(100000)
	  end),
    start_nodes2(Rest, How, N+1, Config).

collect_nodes(N, N) ->
    [];
collect_nodes(N, Max) ->
    receive
	{N, Node} ->
            [Node | collect_nodes(N+1, Max)]
    end.

start_node(Name, How, Config) ->
    start_node(Name, How, "", Config).

start_node(Name0, How, Args, Config) ->
    Name = node_name(Name0, Config),
    Pa = filename:dirname(code:which(?MODULE)),
    test_server:start_node(Name, How, [{args,
                                        Args ++ " " ++
					    "-kernel net_setuptime 100 "
                                        "-noshell "
                                        "-pa " ++ Pa},
                                       {linked, false}]).
stop_nodes(Nodes) ->
    lists:foreach(fun(Node) -> stop_node(Node) end, Nodes).

stop_node(Node) ->
    test_server:stop_node(Node).

get_known(Node) ->
    case catch gen_server:call({global_name_server,Node},get_known,infinity) of
        {'EXIT', _} ->
            [list, without, nodenames];
        Known when is_list(Known) ->
            lists:sort([Node | Known])
    end.

node_name(Name, Config) ->
    U = "_",
    {{Y,M,D}, {H,Min,S}} = calendar:now_to_local_time(now()),
    Date = io_lib:format("~4w_~2..0w_~2..0w__~2..0w_~2..0w_~2..0w",
                         [Y,M,D, H,Min,S]),
    L = lists:flatten(Date),
    lists:concat([Name,U,?testcase,U,U,L]).

%% This one runs on one node in Part2.
%% The partition is ready when is_ready_partition(Config) returns (true).
make_partition(Config, Part1, Part2) ->
    make_partition(Config, Part1, Part2, mk_part_node).

make_partition(Config, Part1, Part2, Function) ->
    Dir = proplists:get_value(priv_dir, Config),
    Ns = [begin
              Name = lists:concat([atom_to_list(N),"_",msec(),".part"]),
              File = filename:join([Dir, Name]),
              file:delete(File),
              rpc_cast(N, ?MODULE, Function, [File, Part, Config], File),
              {N, File}
          end || Part <- [Part1, Part2], N <- Part],
    all_nodes_files(Ns, "done", Config),
    lists:foreach(fun({_N,File}) -> file:delete(File) end, Ns),
    PartFile = make_partition_file(Config),
    touch(PartFile, "done").

%% The node signals its success by touching a file.
mk_part_node(File, MyPart0, Config) ->
    touch(File, "start"), % debug
    MyPart = lists:sort(MyPart0),
    ?UNTIL(is_node_in_part(File, MyPart)),
    touch(File, "done").

%% The calls to append_to_file are for debugging.
is_node_in_part(File, MyPart) ->
    lists:foreach(fun(N) ->
                          _ = erlang:disconnect_node(N)
                  end, nodes() -- MyPart),
    case {(Known = get_known(node())) =:= MyPart,
          (Nodes = lists:sort([node() | nodes()])) =:= MyPart} of
        {true, true} ->
            %% Make sure the resolvers have been terminated,
            %% otherwise they may pop up and send some message.
            %% (This check is probably unnecessary.)
            case element(5, global:info()) of
                [] ->
                    true;
                Rs ->
                    append_to_file(File, {now(), Known, Nodes, Rs}),
                    false
            end;
        _ ->
            append_to_file(File, {now(), Known, Nodes}),
            false
    end.

is_ready_partition(Config) ->
    File = make_partition_file(Config),
    file_contents(File, "done", Config),
    file:delete(File),
    true.

wait_for_ready_net(Config) ->
    wait_for_ready_net([node()|nodes()], Config).

wait_for_ready_net(Nodes0, Config) ->
    Nodes = lists:sort(Nodes0),
    io:format("wait_for_ready_net ~p~n", [Nodes]),
    ?UNTIL(begin
               lists:all(fun(N) -> Nodes =:= get_known(N) end, Nodes) and
		   lists:all(fun(N) ->
				     LNs = rpc:call(N, erlang, nodes, []),
				     Nodes =:= lists:sort([N | LNs])
			     end, Nodes)
           end).

%% To make it less probable that some low-level problem causes
%% problems, the receiving node is ping:ed.
rpc_cast(Node, Module, Function, Args) ->
    {_,pong,Node}= {node(),net_adm:ping(Node),Node},
    rpc:cast(Node, Module, Function, Args).

rpc_cast(Node, Module, Function, Args, File) ->
    case net_adm:ping(Node) of
        pong ->
            rpc:cast(Node, Module, Function, Args);
        Else ->
            append_to_file(File, {now(), {rpc_cast, Node, Module, Function,
                                          Args, Else}})
            %% Maybe we should crash, but it probably doesn't matter.
    end.

touch(File, List) ->
    ok = file:write_file(File, list_to_binary(List)).

append_to_file(File, Term) ->
    {ok, Fd} = file:open(File, [raw,binary,append]),
    ok = file:write(Fd, io_lib:format("~p.~n", [Term])),
    ok = file:close(Fd).

all_nodes_files(Files, ContentsList, Config) ->
    lists:all(fun({_N,File}) ->
                      file_contents(File, ContentsList, Config)
              end, Files).

file_contents(File, ContentsList, Config) ->
    file_contents(File, ContentsList, Config, no_log_file).

file_contents(File, ContentsList, Config, LogFile) ->
    Contents = list_to_binary(ContentsList),
    Sz = size(Contents),
    ?UNTIL(begin
               case file:read_file(File) of
                   {ok, FileContents}=Reply ->
                       case catch split_binary(FileContents, Sz) of
                           {Contents,_} ->
                               true;
                           _ ->
                               catch append_to_file(LogFile,
                                                    {File,Contents,Reply}),
                               false
                       end;
                   Reply ->
                       catch append_to_file(LogFile, {File, Contents, Reply}),
                       false
               end
           end).

make_partition_file(Config) ->
    Dir = proplists:get_value(priv_dir, Config),
    filename:join([Dir, atom_to_list(make_partition_done)]).

msec() ->
    msec(now()).

msec(T) ->
    element(1,T)*1000000000 + element(2,T)*1000 + element(3,T) div 1000.
