%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2012. All Rights Reserved.
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
-module(erlang).

-export([apply/2,apply/3,spawn/4,spawn_link/4,
	 spawn_monitor/1,spawn_monitor/3,
	 spawn_opt/2,spawn_opt/3,spawn_opt/4,spawn_opt/5,
	 disconnect_node/1]).
-export([spawn/1, spawn_link/1, spawn/2, spawn_link/2]).
-export([yield/0]).
-export([crasher/6]).
-export([fun_info/1]).
-export([send_nosuspend/2, send_nosuspend/3]).
-export([localtime_to_universaltime/1]).
-export([suspend_process/1]).
-export([min/2, max/2]).
-export([dlink/1, dunlink/1, dsend/2, dsend/3, dgroup_leader/2,
	 dexit/2, dmonitor_node/3, dmonitor_p/2]).
-export([delay_trap/2]).
-export([set_cookie/2, get_cookie/0]).
-export([nodes/0]).

-export([list_to_integer/2,integer_to_list/2]).
-export([flush_monitor_message/2]).
-export([set_cpu_topology/1, format_cpu_topology/1]).
-export([await_proc_exit/3]).
-export([memory/0, memory/1]).
-export([alloc_info/1, alloc_sizes/1]).

-export([gather_sched_wall_time_result/1,
	 await_sched_wall_time_modifications/2]).

-deprecated([hash/2]).

% Get rid of autoimports of spawn to avoid clashes with ourselves.
-compile({no_auto_import,[spawn/1]}).
-compile({no_auto_import,[spawn/4]}).
-compile({no_auto_import,[spawn_link/1]}).
-compile({no_auto_import,[spawn_link/4]}).
-compile({no_auto_import,[spawn_opt/2]}).
-compile({no_auto_import,[spawn_opt/4]}).
-compile({no_auto_import,[spawn_opt/5]}).

-export_type([timestamp/0]).

-type timestamp() :: {MegaSecs :: non_neg_integer(),
                      Secs :: non_neg_integer(),
                      MicroSecs :: non_neg_integer()}.

%%--------------------------------------------------------------------------

-spec apply(Fun, Args) -> term() when
      Fun :: function(),
      Args :: [term()].
apply(Fun, Args) ->
    erlang:apply(Fun, Args).

-spec apply(Module, Function, Args) -> term() when
      Module :: module(),
      Function :: atom(),
      Args :: [term()].
apply(Mod, Name, Args) ->
    erlang:apply(Mod, Name, Args).

%% Spawns with a fun

-spec spawn(Fun) -> pid() when
      Fun :: function().
spawn(F) when is_function(F) ->
    spawn(erlang, apply, [F, []]);
spawn({M,F}=MF) when is_atom(M), is_atom(F) ->
    spawn(erlang, apply, [MF, []]);
spawn(F) ->
    erlang:error(badarg, [F]).

-spec spawn(Node, Fun) -> pid() when
      Node :: node(),
      Fun :: function().
spawn(N, F) when N =:= node() ->
    spawn(F);
spawn(N, F) when is_function(F) ->
    spawn(N, erlang, apply, [F, []]);
spawn(N, {M,F}=MF) when is_atom(M), is_atom(F) ->
    spawn(N, erlang, apply, [MF, []]);
spawn(N, F) ->
    erlang:error(badarg, [N, F]).

-spec spawn_link(Fun) -> pid() when
      Fun :: function().
spawn_link(F) when is_function(F) ->
    spawn_link(erlang, apply, [F, []]);
spawn_link({M,F}=MF) when is_atom(M), is_atom(F) ->
    spawn_link(erlang, apply, [MF, []]);
spawn_link(F) ->
    erlang:error(badarg, [F]).

-spec spawn_link(Node, Fun) -> pid() when
      Node :: node(),
      Fun :: function().
spawn_link(N, F) when N =:= node() ->
    spawn_link(F);
spawn_link(N, F) when is_function(F) ->
    spawn_link(N, erlang, apply, [F, []]);
spawn_link(N, {M,F}=MF) when is_atom(M), is_atom(F) ->
    spawn_link(N, erlang, apply, [MF, []]);
spawn_link(N, F) ->
    erlang:error(badarg, [N, F]).

%% Spawn and atomically set up a monitor.

-spec spawn_monitor(Fun) -> {pid(), reference()} when
      Fun :: function().
spawn_monitor(F) when is_function(F, 0) ->
    erlang:spawn_opt({erlang,apply,[F,[]],[monitor]});
spawn_monitor(F) ->
    erlang:error(badarg, [F]).

-spec spawn_monitor(Module, Function, Args) -> {pid(), reference()} when
      Module :: module(),
      Function :: atom(),
      Args :: [term()].
spawn_monitor(M, F, A) when is_atom(M), is_atom(F), is_list(A) ->
    erlang:spawn_opt({M,F,A,[monitor]});
spawn_monitor(M, F, A) ->
    erlang:error(badarg, [M,F,A]).

-spec spawn_opt(Fun, Options) -> pid() | {pid(), reference()} when
      Fun :: function(),
      Options :: [Option],
      Option :: link | monitor | {priority, Level}
              | {fullsweep_after, Number :: non_neg_integer()}
              | {min_heap_size, Size :: non_neg_integer()}
              | {min_bin_vheap_size, VSize :: non_neg_integer()},
      Level :: low | normal | high.
spawn_opt(F, O) when is_function(F) ->
    spawn_opt(erlang, apply, [F, []], O);
spawn_opt({M,F}=MF, O) when is_atom(M), is_atom(F) ->
    spawn_opt(erlang, apply, [MF, []], O);
spawn_opt({M,F,A}, O) -> % For (undocumented) backward compatibility
    spawn_opt(M, F, A, O);
spawn_opt(F, O) ->
    erlang:error(badarg, [F, O]).

-spec spawn_opt(Node, Fun, Options) -> pid() | {pid(), reference()} when
      Node :: node(),
      Fun :: function(),
      Options :: [Option],
      Option :: link | monitor | {priority, Level}
              | {fullsweep_after, Number :: non_neg_integer()}
              | {min_heap_size, Size :: non_neg_integer()}
              | {min_bin_vheap_size, VSize :: non_neg_integer()},
      Level :: low | normal | high.
spawn_opt(N, F, O) when N =:= node() ->
    spawn_opt(F, O);
spawn_opt(N, F, O) when is_function(F) ->
    spawn_opt(N, erlang, apply, [F, []], O);
spawn_opt(N, {M,F}=MF, O) when is_atom(M), is_atom(F) ->
    spawn_opt(N, erlang, apply, [MF, []], O);
spawn_opt(N, F, O) ->
    erlang:error(badarg, [N, F, O]).

%% Spawns with MFA

-spec spawn(Node, Module, Function, Args) -> pid() when
      Node :: node(),
      Module :: module(),
      Function :: atom(),
      Args :: [term()].
spawn(N,M,F,A) when N =:= node(), is_atom(M), is_atom(F), is_list(A) ->
    spawn(M,F,A);
spawn(N,M,F,A) when is_atom(N), is_atom(M), is_atom(F) ->
    case is_well_formed_list(A) of
	true ->
	    ok;
	false ->
	    erlang:error(badarg, [N, M, F, A])
    end,
    case catch gen_server:call({net_kernel,N},
			       {spawn,M,F,A,group_leader()},
			       infinity) of
	Pid when is_pid(Pid) ->
	    Pid;
	Error ->
	    case remote_spawn_error(Error, {no_link, N, M, F, A, []}) of
		{fault, Fault} ->
		    erlang:error(Fault, [N, M, F, A]);
		Pid ->
		    Pid
	    end
    end;
spawn(N,M,F,A) ->
    erlang:error(badarg, [N, M, F, A]).

-spec spawn_link(Node, Module, Function, Args) -> pid() when
      Node :: node(),
      Module :: module(),
      Function :: atom(),
      Args :: [term()].
spawn_link(N,M,F,A) when N =:= node(), is_atom(M), is_atom(F), is_list(A) ->
    spawn_link(M,F,A);
spawn_link(N,M,F,A) when is_atom(N), is_atom(M), is_atom(F) ->
    case is_well_formed_list(A) of
	true ->
	    ok;
	_ ->
	    erlang:error(badarg, [N, M, F, A])
    end,
    case catch gen_server:call({net_kernel,N},
			       {spawn_link,M,F,A,group_leader()},
			       infinity) of
	Pid when is_pid(Pid) ->
	    Pid;
	Error ->
	    case remote_spawn_error(Error, {link, N, M, F, A, []}) of
		{fault, Fault} ->
		    erlang:error(Fault, [N, M, F, A]);
		Pid ->
		    Pid
	    end
    end;
spawn_link(N,M,F,A) ->
    erlang:error(badarg, [N, M, F, A]).

-spec spawn_opt(Module, Function, Args, Options) ->
                       pid() | {pid(), reference()} when
      Module :: module(),
      Function :: atom(),
      Args :: [term()],
      Options :: [Option],
      Option :: link | monitor | {priority, Level}
              | {fullsweep_after, Number :: non_neg_integer()}
              | {min_heap_size, Size :: non_neg_integer()}
              | {min_bin_vheap_size, VSize :: non_neg_integer()},
      Level :: low | normal | high.
spawn_opt(M, F, A, Opts) ->
    case catch erlang:spawn_opt({M,F,A,Opts}) of
	{'EXIT',{Reason,_}} ->
	    erlang:error(Reason, [M,F,A,Opts]);
	Res ->
	    Res
    end.

-spec spawn_opt(Node, Module, Function, Args, Options) ->
                       pid() | {pid(), reference()} when
      Node :: node(),
      Module :: module(),
      Function :: atom(),
      Args :: [term()],
      Options :: [Option],
      Option :: link | monitor | {priority, Level}
              | {fullsweep_after, Number :: non_neg_integer()}
              | {min_heap_size, Size :: non_neg_integer()}
              | {min_bin_vheap_size, VSize :: non_neg_integer()},
      Level :: low | normal | high.
spawn_opt(N, M, F, A, O) when N =:= node(),
			      is_atom(M), is_atom(F), is_list(A),
			      is_list(O) ->
    spawn_opt(M, F, A, O);
spawn_opt(N, M, F, A, O) when is_atom(N), is_atom(M), is_atom(F) ->
    case {is_well_formed_list(A), is_well_formed_list(O)} of
	{true, true} ->
	    ok;
	_ ->
	    erlang:error(badarg, [N, M, F, A, O])
    end,
    case lists:member(monitor, O) of
	false -> ok;
	true -> erlang:error(badarg, [N, M, F, A, O])
    end,
    {L,NO} = lists:foldl(fun (link, {_, NewOpts}) ->
				 {link, NewOpts};
			     (Opt, {LO, NewOpts}) ->
				 {LO, [Opt|NewOpts]}
			 end,
			 {no_link,[]},
			 O),
    case catch gen_server:call({net_kernel,N},
			       {spawn_opt,M,F,A,NO,L,group_leader()},
			       infinity) of
	Pid when is_pid(Pid) ->
	    Pid;
	Error ->
	    case remote_spawn_error(Error, {L, N, M, F, A, NO}) of
		{fault, Fault} ->
		    erlang:error(Fault, [N, M, F, A, O]);
		Pid ->
		    Pid
	    end
    end;
spawn_opt(N,M,F,A,O) ->
    erlang:error(badarg, [N,M,F,A,O]).

remote_spawn_error({'EXIT', {{nodedown,N}, _}}, {L, N, M, F, A, O}) ->
    {Opts, LL} = case L =:= link of
		     true ->
			 {[link|O], [link]};
		     false ->
			 {O, []}
		 end,
    spawn_opt(erlang,crasher,[N,M,F,A,Opts,noconnection], LL);
remote_spawn_error({'EXIT', {Reason, _}}, _) ->
    {fault, Reason};
remote_spawn_error({'EXIT', Reason}, _) ->
    {fault, Reason};
remote_spawn_error(Other, _) ->
    {fault, Other}.
    
is_well_formed_list([]) ->
    true;
is_well_formed_list([_|Rest]) ->
    is_well_formed_list(Rest);
is_well_formed_list(_) ->
    false.

crasher(Node,Mod,Fun,Args,[],Reason) ->
    error_logger:warning_msg("** Can not start ~w:~w,~w on ~w **~n",
			     [Mod,Fun,Args,Node]),
    exit(Reason);
crasher(Node,Mod,Fun,Args,Opts,Reason) ->
    error_logger:warning_msg("** Can not start ~w:~w,~w (~w) on ~w **~n",
			     [Mod,Fun,Args,Opts,Node]),
    exit(Reason).

-spec erlang:yield() -> 'true'.
yield() ->
    erlang:yield().

-spec nodes() -> Nodes when
      Nodes :: [node()].
nodes() ->
    erlang:nodes(visible).

-spec disconnect_node(Node) -> boolean() | ignored when
      Node :: node().
disconnect_node(Node) ->
    net_kernel:disconnect(Node).

-spec erlang:fun_info(Fun) -> [{Item, Info}] when
      Fun :: function(),
      Item :: arity | env | index | name
            | module | new_index | new_uniq | pid | type | uniq,
      Info :: term().
fun_info(Fun) when is_function(Fun) ->
    Keys = [type,env,arity,name,uniq,index,new_uniq,new_index,module,pid],
    fun_info_1(Keys, Fun, []).

fun_info_1([K|Ks], Fun, A) ->
    case erlang:fun_info(Fun, K) of
	{K,undefined} -> fun_info_1(Ks, Fun, A);
	{K,_}=P -> fun_info_1(Ks, Fun, [P|A])
    end;
fun_info_1([], _, A) -> A.

-type dst() :: pid()
             | port()
             | (RegName :: atom())
             | {RegName :: atom(), Node :: node()}.

-spec erlang:send_nosuspend(Dest, Msg) -> boolean() when
      Dest :: dst(),
      Msg :: term().
send_nosuspend(Pid, Msg) ->
    send_nosuspend(Pid, Msg, []).

-spec erlang:send_nosuspend(Dest, Msg, Options) -> boolean() when
      Dest :: dst(),
      Msg :: term(),
      Options :: [noconnect].
send_nosuspend(Pid, Msg, Opts) ->
    case erlang:send(Pid, Msg, [nosuspend|Opts]) of
	ok -> true;
	_  -> false
    end.

-spec erlang:localtime_to_universaltime({Date1, Time1}) -> {Date2, Time2} when
      Date1 :: calendar:date(),
      Date2 :: calendar:date(),
      Time1 :: calendar:time(),
      Time2 :: calendar:time().
localtime_to_universaltime(Localtime) ->
    erlang:localtime_to_universaltime(Localtime, undefined).

-spec erlang:suspend_process(Suspendee) -> 'true' when
      Suspendee :: pid().
suspend_process(P) ->
    case catch erlang:suspend_process(P, []) of
	{'EXIT', {Reason, _}} -> erlang:error(Reason, [P]);
	{'EXIT', Reason} -> erlang:error(Reason, [P]);
	Res -> Res
    end.

%%
%% If the emulator wants to perform a distributed command and
%% a connection is not established to the actual node the following 
%% functions are called in order to set up the connection and then
%% reactivate the command.
%%

-spec dlink(pid() | port()) -> 'true'.
dlink(Pid) ->
    case net_kernel:connect(node(Pid)) of
	true -> link(Pid);
	false -> erlang:dist_exit(self(), noconnection, Pid), true
    end.

%% Can this ever happen?
-spec dunlink(identifier()) -> 'true'.
dunlink(Pid) ->
    case net_kernel:connect(node(Pid)) of
	true -> unlink(Pid);
	false -> true
    end.

dmonitor_node(Node, Flag, []) ->
    case net_kernel:connect(Node) of
	true -> erlang:monitor_node(Node, Flag, []);
	false -> self() ! {nodedown, Node}, true
    end;

dmonitor_node(Node, Flag, Opts) ->
    case lists:member(allow_passive_connect, Opts) of
	true ->
	    case net_kernel:passive_cnct(Node) of
		true -> erlang:monitor_node(Node, Flag, Opts);
		false -> self() ! {nodedown, Node}, true
	    end;
	_ ->
	    dmonitor_node(Node,Flag,[])
    end.

dgroup_leader(Leader, Pid) ->
    case net_kernel:connect(node(Pid)) of
	true -> group_leader(Leader, Pid);
	false -> true  %% bad arg ?
    end.

dexit(Pid, Reason) -> 
    case net_kernel:connect(node(Pid)) of
	true -> exit(Pid, Reason);
	false -> true
    end.

dsend(Pid, Msg) when is_pid(Pid) ->
    case net_kernel:connect(node(Pid)) of
	true -> erlang:send(Pid, Msg);
	false -> Msg
    end;
dsend(Port, Msg) when is_port(Port) ->
    case net_kernel:connect(node(Port)) of
	true -> erlang:send(Port, Msg);
	false -> Msg
    end;
dsend({Name, Node}, Msg) ->
    case net_kernel:connect(Node) of
	true -> erlang:send({Name,Node}, Msg);
	false -> Msg;
	ignored -> Msg				% Not distributed.
    end.

dsend(Pid, Msg, Opts) when is_pid(Pid) ->
    case net_kernel:connect(node(Pid)) of
	true -> erlang:send(Pid, Msg, Opts);
	false -> ok
    end;
dsend(Port, Msg, Opts) when is_port(Port) ->
    case net_kernel:connect(node(Port)) of
	true -> erlang:send(Port, Msg, Opts);
	false -> ok
    end;
dsend({Name, Node}, Msg, Opts) ->
    case net_kernel:connect(Node) of
	true -> erlang:send({Name,Node}, Msg, Opts);
	false -> ok;
	ignored -> ok				% Not distributed.
    end.

-spec dmonitor_p('process', pid() | {atom(),atom()}) -> reference().
dmonitor_p(process, ProcSpec) ->
    %% ProcSpec = pid() | {atom(),atom()}
    %% ProcSpec CANNOT be an atom because a locally registered process
    %% is never handled here.
    Node = case ProcSpec of
	       {S,N} when is_atom(S), is_atom(N), N =/= node() -> N;
	       _ when is_pid(ProcSpec) -> node(ProcSpec)
	   end,
    case net_kernel:connect(Node) of
	true ->
	    erlang:monitor(process, ProcSpec);
	false ->
	    Ref = make_ref(),
	    self() ! {'DOWN', Ref, process, ProcSpec, noconnection},
	    Ref
    end.

%%
%% Trap function used when modified timing has been enabled.
%%

-spec delay_trap(Result, timeout()) -> Result.
delay_trap(Result, 0) -> erlang:yield(), Result;
delay_trap(Result, Timeout) -> receive after Timeout -> Result end.

%%
%% The business with different in and out cookies represented
%% everywhere is discarded.
%% A node has a cookie, connections/messages to that node use that cookie.
%% Messages to us use our cookie. IF we change our cookie, other nodes 
%% have to reflect that, which we cannot forsee.
%%
-spec erlang:set_cookie(Node, Cookie) -> true when
      Node :: node(),
      Cookie :: atom().
set_cookie(Node, C) when Node =/= nonode@nohost, is_atom(Node) ->
    case is_atom(C) of
	true ->
	    auth:set_cookie(Node, C);
	false ->
	    error(badarg)
    end.

-spec erlang:get_cookie() -> Cookie | nocookie when
      Cookie :: atom().
get_cookie() ->
    auth:get_cookie().

-spec integer_to_list(Integer, Base) -> string() when
      Integer :: integer(),
      Base :: 2..36.
integer_to_list(I, 10) ->
    erlang:integer_to_list(I);
integer_to_list(I, Base) 
  when is_integer(I), is_integer(Base), Base >= 2, Base =< 1+$Z-$A+10 ->
    if I < 0 ->
	    [$-|integer_to_list(-I, Base, [])];
       true ->
	    integer_to_list(I, Base, [])
    end;
integer_to_list(I, Base) ->
    erlang:error(badarg, [I, Base]).

integer_to_list(I0, Base, R0) ->
    D = I0 rem Base,
    I1 = I0 div Base,
    R1 = if D >= 10 ->
		 [D-10+$A|R0];
	    true ->
		 [D+$0|R0]
	 end,
    if I1 =:= 0 ->
	    R1;
       true ->
	    integer_to_list(I1, Base, R1)
    end.


-spec list_to_integer(String, Base) -> integer() when
      String :: string(),
      Base :: 2..36.
list_to_integer(L, 10) ->
    erlang:list_to_integer(L);
list_to_integer(L, Base)
  when is_list(L), is_integer(Base), Base >= 2, Base =< 1+$Z-$A+10 ->
    case list_to_integer_sign(L, Base) of 
	I when is_integer(I) ->
	    I;
	Fault ->
	    erlang:error(Fault, [L,Base])
    end;
list_to_integer(L, Base) ->
    erlang:error(badarg, [L,Base]).

list_to_integer_sign([$-|[_|_]=L], Base) ->
    case list_to_integer(L, Base, 0) of
	I when is_integer(I) ->
	    -I;
	I ->
	    I
    end;
list_to_integer_sign([$+|[_|_]=L], Base) ->
    list_to_integer(L, Base, 0);
list_to_integer_sign([_|_]=L, Base) ->
    list_to_integer(L, Base, 0);
list_to_integer_sign(_, _) ->
    badarg.

list_to_integer([D|L], Base, I) 
  when is_integer(D), D >= $0, D =< $9, D < Base+$0 ->
    list_to_integer(L, Base, I*Base + D-$0);
list_to_integer([D|L], Base, I) 
  when is_integer(D), D >= $A, D < Base+$A-10 ->
    list_to_integer(L, Base, I*Base + D-$A+10);
list_to_integer([D|L], Base, I) 
  when is_integer(D), D >= $a, D < Base+$a-10 ->
    list_to_integer(L, Base, I*Base + D-$a+10);
list_to_integer([], _, I) ->
    I;
list_to_integer(_, _, _) ->
    badarg.

%% erlang:flush_monitor_message/2 is for internal use only!
%%
%% erlang:demonitor(Ref, [flush]) traps to
%% erlang:flush_monitor_message(Ref, Res) when
%% it needs to flush a monitor message.
flush_monitor_message(Ref, Res) when is_reference(Ref), is_atom(Res) ->
    receive {_, Ref, _, _, _} -> ok after 0 -> ok end,
    Res.

-record(cpu, {node = -1,
	      processor = -1,
	      processor_node = -1,
	      core = -1,
	      thread = -1,
	      logical = -1}).

%% erlang:set_cpu_topology/1 is for internal use only!
%%
%% erlang:system_flag(cpu_topology, CpuTopology) traps to 
%% erlang:set_cpu_topology(CpuTopology).
set_cpu_topology(CpuTopology) ->
    try format_cpu_topology(erlang:system_flag(internal_cpu_topology,
					       cput_e2i(CpuTopology)))
    catch
	Class:Exception when Class =/= error; Exception =/= internal_error -> 
	    erlang:error(badarg, [CpuTopology])
    end.

cput_e2i_clvl({logical, _}, _PLvl) ->
    #cpu.logical;
cput_e2i_clvl([E | _], PLvl) ->
    case element(1, E) of
	node -> case PLvl of
		    0 -> #cpu.node;
		    #cpu.processor -> #cpu.processor_node
		end;
	processor -> case PLvl of
			 0 -> #cpu.node;
			 #cpu.node -> #cpu.processor
		     end;
	core -> #cpu.core;
	thread -> #cpu.thread
    end.

cput_e2i(undefined) ->
    undefined;
cput_e2i(E) ->
    rvrs(cput_e2i(E, -1, -1, #cpu{}, 0, cput_e2i_clvl(E, 0), [])).

cput_e2i([], _NId, _PId, _IS, _PLvl, _Lvl, Res) -> 
    Res;
cput_e2i([E|Es], NId0, PId, IS, PLvl, Lvl, Res0) -> 
    case cput_e2i(E, NId0, PId, IS, PLvl, Lvl, Res0) of
	[] ->
	    cput_e2i(Es, NId0, PId, IS, PLvl, Lvl, Res0);
	[#cpu{node = N,
	      processor = P,
	      processor_node = PN} = CPU|_] = Res1 ->
	    NId1 = case N > PN of
			 true -> N;
			 false -> PN
		     end,
	    cput_e2i(Es, NId1, P, CPU, PLvl, Lvl, Res1)
    end;
cput_e2i({Tag, [], TagList}, Nid, PId, CPU, PLvl, Lvl, Res) ->
    %% Currently [] is the only valid InfoList
    cput_e2i({Tag, TagList}, Nid, PId, CPU, PLvl, Lvl, Res);
cput_e2i({node, NL}, Nid0, PId, _CPU, 0, #cpu.node, Res) ->
    Nid1 = Nid0+1,
    Lvl = cput_e2i_clvl(NL, #cpu.node),
    cput_e2i(NL, Nid1, PId, #cpu{node = Nid1}, #cpu.node, Lvl, Res);
cput_e2i({processor, PL}, Nid, PId0, _CPU, 0, #cpu.node, Res) ->
    PId1 = PId0+1,
    Lvl = cput_e2i_clvl(PL, #cpu.processor),
    cput_e2i(PL, Nid, PId1, #cpu{processor = PId1}, #cpu.processor, Lvl, Res);
cput_e2i({processor, PL}, Nid, PId0, CPU, PLvl, CLvl, Res)
  when PLvl < #cpu.processor, CLvl =< #cpu.processor ->
    PId1 = PId0+1,
    Lvl = cput_e2i_clvl(PL, #cpu.processor),
    cput_e2i(PL, Nid, PId1, CPU#cpu{processor = PId1,
				    processor_node = -1,
				    core = -1,
				    thread = -1}, #cpu.processor, Lvl, Res);
cput_e2i({node, NL}, Nid0, PId, CPU, #cpu.processor, #cpu.processor_node,
	 Res) ->
    Nid1 = Nid0+1,
    Lvl = cput_e2i_clvl(NL, #cpu.processor_node),
    cput_e2i(NL, Nid1, PId, CPU#cpu{processor_node = Nid1},
	     #cpu.processor_node, Lvl, Res);
cput_e2i({core, CL}, Nid, PId, #cpu{core = C0} = CPU, PLvl, #cpu.core, Res)
  when PLvl < #cpu.core ->
    Lvl = cput_e2i_clvl(CL, #cpu.core),
    cput_e2i(CL, Nid, PId, CPU#cpu{core = C0+1, thread = -1}, #cpu.core, Lvl,
	     Res);
cput_e2i({thread, TL}, Nid, PId, #cpu{thread = T0} = CPU, PLvl, #cpu.thread,
	 Res) when PLvl < #cpu.thread ->
    Lvl = cput_e2i_clvl(TL, #cpu.thread),
    cput_e2i(TL, Nid, PId, CPU#cpu{thread = T0+1}, #cpu.thread, Lvl, Res);
cput_e2i({logical, ID}, _Nid, PId, #cpu{processor=P, core=C, thread=T} = CPU,
	 PLvl, #cpu.logical, Res)
  when PLvl < #cpu.logical, is_integer(ID), 0 =< ID, ID < 65536 ->
    [CPU#cpu{processor = case P of -1 -> PId+1; _ -> P end,
	     core = case C of -1 -> 0; _ -> C end,
	     thread = case T of -1 -> 0; _ -> T end,
	     logical = ID} | Res].

%% erlang:format_cpu_topology/1 is for internal use only!
%%
%% erlang:system_info(cpu_topology),
%% and erlang:system_info({cpu_topology, Which}) traps to
%% erlang:format_cpu_topology(InternalCpuTopology).
format_cpu_topology(InternalCpuTopology) ->
    try cput_i2e(InternalCpuTopology)
    catch _ : _ -> erlang:error(internal_error, [InternalCpuTopology])
    end.


cput_i2e(undefined) -> undefined;
cput_i2e(Is) -> cput_i2e(Is, true, #cpu.node, cput_i2e_tag_map()).

cput_i2e([], _Frst, _Lvl, _TM) ->
    [];
cput_i2e([#cpu{logical = LID}| _], _Frst, Lvl, _TM) when Lvl == #cpu.logical ->
    {logical, LID};
cput_i2e([#cpu{} = I | Is], Frst, Lvl, TM) ->
    cput_i2e(element(Lvl, I), Frst, Is, [I], Lvl, TM).

cput_i2e(V, Frst, [I | Is], SameV, Lvl, TM) when V =:= element(Lvl, I) ->
    cput_i2e(V, Frst, Is, [I | SameV], Lvl, TM);
cput_i2e(-1, true, [], SameV, Lvl, TM) ->
    cput_i2e(rvrs(SameV), true, Lvl+1, TM);
cput_i2e(_V, true, [], SameV, Lvl, TM) when Lvl =/= #cpu.processor,
                                            Lvl =/= #cpu.processor_node ->
    cput_i2e(rvrs(SameV), true, Lvl+1, TM);
cput_i2e(-1, _Frst, Is, SameV, #cpu.node, TM) ->
    cput_i2e(rvrs(SameV), true, #cpu.processor, TM)
	++ cput_i2e(Is, false, #cpu.node, TM);
cput_i2e(_V, _Frst, Is, SameV, Lvl, TM) ->
    [{cput_i2e_tag(Lvl, TM), cput_i2e(rvrs(SameV), true, Lvl+1, TM)}
     | cput_i2e(Is, false, Lvl, TM)].

cput_i2e_tag_map() -> list_to_tuple([cpu | record_info(fields, cpu)]).

cput_i2e_tag(Lvl, TM) ->
    case element(Lvl, TM) of processor_node -> node; Other -> Other end.

rvrs([_] = L) -> L;
rvrs(Xs) -> rvrs(Xs, []).

rvrs([],Ys) -> Ys;
rvrs([X|Xs],Ys) -> rvrs(Xs, [X|Ys]).

%% erlang:await_proc_exit/3 is for internal use only!
%%
%% BIFs that need to await a specific process exit before
%% returning traps to erlang:await_proc_exit/3.
%%
%% NOTE: This function is tightly coupled to
%%       the implementation of the
%%       erts_bif_prep_await_proc_exit_*()
%%       functions in bif.c. Do not make
%%       any changes to it without reading
%%       the comment about them in bif.c!
-spec await_proc_exit(dst(), 'apply' | 'data' | 'reason', term()) -> term().
await_proc_exit(Proc, Op, Data) ->
    Mon = erlang:monitor(process, Proc),
    receive
	{'DOWN', Mon, process, _Proc, Reason} ->
	    case Op of
		apply ->
		    {M, F, A} = Data,
		    erlang:apply(M, F, A);
		data ->
		    Data;
		reason ->
		    Reason
	    end
    end.

-spec min(Term1, Term2) -> Minimum when
      Term1 :: term(),
      Term2 :: term(),
      Minimum :: term().
min(A, B) when A > B -> B;
min(A, _) -> A.

-spec max(Term1, Term2) -> Maximum when
      Term1 :: term(),
      Term2 :: term(),
      Maximum :: term().
max(A, B) when A < B -> B;
max(A, _) -> A.


%%
%% erlang:memory/[0,1]
%%
%% NOTE! When updating these functions, make sure to also update
%%       erts_memory() in $ERL_TOP/erts/emulator/beam/erl_alloc.c
%%

-type memory_type() :: 'total' | 'processes' | 'processes_used' | 'system' | 'atom' | 'atom_used' | 'binary' | 'code' | 'ets' | 'low' | 'maximum'.

-define(CARRIER_ALLOCS, [mseg_alloc, sbmbc_alloc, sbmbc_low_alloc]).
-define(LOW_ALLOCS, [sbmbc_low_alloc, ll_low_alloc, std_low_alloc]).
-define(ALL_NEEDED_ALLOCS, (erlang:system_info(alloc_util_allocators)
			    -- ?CARRIER_ALLOCS)).

-record(memory, {total = 0,
		 processes = 0,
		 processes_used = 0,
		 system = 0,
		 atom = 0,
		 atom_used = 0,
		 binary = 0,
		 code = 0,
		 ets = 0,
		 low = 0,
		 maximum = 0}).

-spec memory() -> [{memory_type(), non_neg_integer()}].
memory() ->
    case aa_mem_data(au_mem_data(?ALL_NEEDED_ALLOCS)) of
	notsup ->
	    erlang:error(notsup);
	Mem ->
	    InstrTail = case Mem#memory.maximum of
			    0 -> [];
			    _ -> [{maximum, Mem#memory.maximum}]
			end,
	    Tail = case Mem#memory.low of
		       0 -> InstrTail;
		       _ -> [{low, Mem#memory.low} | InstrTail]
		   end,
	    [{total, Mem#memory.total},
	     {processes, Mem#memory.processes},
	     {processes_used, Mem#memory.processes_used},
	     {system, Mem#memory.system},
	     {atom, Mem#memory.atom},
	     {atom_used, Mem#memory.atom_used},
	     {binary, Mem#memory.binary},
	     {code, Mem#memory.code},
	     {ets, Mem#memory.ets} | Tail]
    end.

-spec memory(memory_type()|[memory_type()]) -> non_neg_integer() | [{memory_type(), non_neg_integer()}].
memory(Type) when is_atom(Type) ->
    {AA, ALCU, ChkSup, BadArgZero} = need_mem_info(Type),
    case get_mem_data(ChkSup, ALCU, AA) of
	notsup ->
	    erlang:error(notsup, [Type]);
	Mem ->
	    Value = get_memval(Type, Mem),
	    case {BadArgZero, Value} of
		{true, 0} -> erlang:error(badarg, [Type]);
		_ -> Value
	    end
    end;
memory(Types) when is_list(Types) ->
    {AA, ALCU, ChkSup, BadArgZeroList} = need_mem_info_list(Types),
    case get_mem_data(ChkSup, ALCU, AA) of
	notsup ->
	    erlang:error(notsup, [Types]);
	Mem ->
	    case memory_result_list(Types, BadArgZeroList, Mem) of
		badarg -> erlang:error(badarg, [Types]);
		Result -> Result
	    end
    end.

memory_result_list([], [], _Mem) ->
    [];
memory_result_list([T|Ts], [BAZ|BAZs], Mem) ->
    case memory_result_list(Ts, BAZs, Mem) of
	badarg -> badarg;
	TVs ->
	    V = get_memval(T, Mem),
	    case {BAZ, V} of
		{true, 0} -> badarg;
		_ -> [{T, V}| TVs]
	    end
    end.

get_mem_data(true, AlcUAllocs, NeedAllocatedAreas) ->
    case memory_is_supported() of
	false -> notsup;
	true -> get_mem_data(false, AlcUAllocs, NeedAllocatedAreas)
    end;
get_mem_data(false, AlcUAllocs, NeedAllocatedAreas) ->
    AlcUMem = case AlcUAllocs of
		  [] -> #memory{};
		  _ ->
		      au_mem_data(AlcUAllocs)
	      end,
    case NeedAllocatedAreas of
	true -> aa_mem_data(AlcUMem);
	false -> AlcUMem
    end.

need_mem_info_list([]) ->
    {false, [], false, []};
need_mem_info_list([T|Ts]) ->
    {MAA, MALCU, MChkSup, MBadArgZero} = need_mem_info_list(Ts),
    {AA, ALCU, ChkSup, BadArgZero} = need_mem_info(T),
    {case AA of
	 true -> true;
	 _ -> MAA
     end,
     ALCU ++ (MALCU -- ALCU),
     case ChkSup of
	 true -> true;
	 _ -> MChkSup
     end,
     [BadArgZero|MBadArgZero]}.

need_mem_info(Type) when Type == total;
			 Type == system ->
    {true, ?ALL_NEEDED_ALLOCS, false, false};
need_mem_info(Type) when Type == processes;
			 Type == processes_used ->
    {true, [eheap_alloc, fix_alloc], true, false};
need_mem_info(Type) when Type == atom;
			 Type == atom_used;
			 Type == code ->
    {true, [], true, false};
need_mem_info(binary) ->
    {false, [binary_alloc], true, false};
need_mem_info(ets) ->
    {true, [ets_alloc], true, false};
need_mem_info(low) ->
    LowAllocs = ?LOW_ALLOCS -- ?CARRIER_ALLOCS,
    {_, _, FeatureList, _} = erlang:system_info(allocator),
    AlcUAllocs = case LowAllocs -- FeatureList of
		     [] -> LowAllocs;
		     _ -> []
		 end,
    {false, AlcUAllocs, true, true};
need_mem_info(maximum) ->
    {true, [], true, true};
need_mem_info(_) ->
    {false, [], false, true}.

get_memval(total, #memory{total = V}) -> V;
get_memval(processes, #memory{processes = V}) -> V;
get_memval(processes_used, #memory{processes_used = V}) -> V;
get_memval(system, #memory{system = V}) -> V;
get_memval(atom, #memory{atom = V}) -> V;
get_memval(atom_used, #memory{atom_used = V}) -> V;
get_memval(binary, #memory{binary = V}) -> V;
get_memval(code, #memory{code = V}) -> V;
get_memval(ets, #memory{ets = V}) -> V;
get_memval(low, #memory{low = V}) -> V;
get_memval(maximum, #memory{maximum = V}) -> V;
get_memval(_, #memory{}) -> 0.

memory_is_supported() ->
    {_, _, FeatureList, _} = erlang:system_info(allocator),
    case ((erlang:system_info(alloc_util_allocators) 
	   -- ?CARRIER_ALLOCS)
	  -- FeatureList) of
	[] -> true;
	_ -> false
    end.

get_blocks_size([{blocks_size, Sz, _, _} | Rest], Acc) ->
    get_blocks_size(Rest, Acc+Sz);
get_blocks_size([{_, _, _, _} | Rest], Acc) ->
    get_blocks_size(Rest, Acc);
get_blocks_size([], Acc) ->
    Acc.

blocks_size([{Carriers, SizeList} | Rest], Acc) when Carriers == mbcs;
						     Carriers == sbcs;
						     Carriers == sbmbcs ->
    blocks_size(Rest, get_blocks_size(SizeList, Acc));
blocks_size([_ | Rest], Acc) ->
    blocks_size(Rest, Acc);
blocks_size([], Acc) ->
    Acc.

get_fix_proc([{ProcType, A1, U1}| Rest], {A0, U0}) when ProcType == proc;
							ProcType == monitor_sh;
							ProcType == nlink_sh;
							ProcType == msg_ref ->
    get_fix_proc(Rest, {A0+A1, U0+U1});
get_fix_proc([_|Rest], Acc) ->
    get_fix_proc(Rest, Acc);
get_fix_proc([], Acc) ->
    Acc.

fix_proc([{fix_types, SizeList} | _Rest], Acc) ->
    get_fix_proc(SizeList, Acc);
fix_proc([_ | Rest], Acc) ->
    fix_proc(Rest, Acc);
fix_proc([], Acc) ->
    Acc.

is_low_alloc(_A, []) ->
    false;
is_low_alloc(A, [A|_As]) ->
    true;
is_low_alloc(A, [_A|As]) ->
    is_low_alloc(A, As).

is_low_alloc(A) ->
    is_low_alloc(A, ?LOW_ALLOCS).

au_mem_data(notsup, _) ->
    notsup;
au_mem_data(_, [{_, false} | _]) ->
    notsup;
au_mem_data(#memory{total = Tot,
		    processes = Proc,
		    processes_used = ProcU} = Mem,
	    [{eheap_alloc, _, Data} | Rest]) ->
    Sz = blocks_size(Data, 0),
    au_mem_data(Mem#memory{total = Tot+Sz,
			   processes = Proc+Sz,
			   processes_used = ProcU+Sz},
		Rest);
au_mem_data(#memory{total = Tot,
		    system = Sys,
		    ets = Ets} = Mem,
	    [{ets_alloc, _, Data} | Rest]) ->
    Sz = blocks_size(Data, 0),
    au_mem_data(Mem#memory{total = Tot+Sz,
			   system = Sys+Sz,
			   ets = Ets+Sz},
		Rest);
au_mem_data(#memory{total = Tot,
		    system = Sys,
		    binary = Bin} = Mem,
	    [{binary_alloc, _, Data} | Rest]) ->
    Sz = blocks_size(Data, 0),
    au_mem_data(Mem#memory{total = Tot+Sz,
			   system = Sys+Sz,
			   binary = Bin+Sz},
		Rest);
au_mem_data(#memory{total = Tot,
		    processes = Proc,
		    processes_used = ProcU,
		    system = Sys} = Mem,
	    [{fix_alloc, _, Data} | Rest]) ->
    {A, U} = fix_proc(Data, {0, 0}),
    Sz = blocks_size(Data, 0),
    au_mem_data(Mem#memory{total = Tot+Sz,
			   processes = Proc+A,
			   processes_used = ProcU+U,
			   system = Sys+Sz-A},
		Rest);
au_mem_data(#memory{total = Tot,
		    system = Sys,
		    low = Low} = Mem,
	    [{A, _, Data} | Rest]) ->
    Sz = blocks_size(Data, 0),
    au_mem_data(Mem#memory{total = Tot+Sz,
			   system = Sys+Sz,
			   low = case is_low_alloc(A) of
				     true -> Low+Sz;
				     false -> Low
				 end},
		Rest);
au_mem_data(EMD, []) ->
    EMD.

au_mem_data(Allocs) ->
    Ref = make_ref(),
    erlang:system_info({allocator_sizes, Ref, Allocs}),
    receive_emd(Ref).

receive_emd(_Ref, EMD, 0) ->
    EMD;
receive_emd(Ref, EMD, N) ->
    receive
	{Ref, _, Data} ->
	    receive_emd(Ref, au_mem_data(EMD, Data), N-1)
    end.

receive_emd(Ref) ->
    receive_emd(Ref, #memory{}, erlang:system_info(schedulers)).

aa_mem_data(#memory{} = Mem,
	    [{maximum, Max} | Rest]) ->
    aa_mem_data(Mem#memory{maximum = Max},
		Rest);
aa_mem_data(#memory{} = Mem,
	    [{total, Tot} | Rest]) ->
    aa_mem_data(Mem#memory{total = Tot,
			   system = 0}, % system will be adjusted later
		Rest);
aa_mem_data(#memory{atom = Atom,
		    atom_used = AtomU} = Mem,
	    [{atom_space, Alloced, Used} | Rest]) ->
    aa_mem_data(Mem#memory{atom = Atom+Alloced,
			   atom_used = AtomU+Used},
		Rest);
aa_mem_data(#memory{atom = Atom,
		    atom_used = AtomU} = Mem,
	    [{atom_table, Sz} | Rest]) ->
    aa_mem_data(Mem#memory{atom = Atom+Sz,
			   atom_used = AtomU+Sz},
		Rest);
aa_mem_data(#memory{ets = Ets} = Mem,
	    [{ets_misc, Sz} | Rest]) ->
    aa_mem_data(Mem#memory{ets = Ets+Sz},
		Rest);
aa_mem_data(#memory{processes = Proc,
		    processes_used = ProcU,
		    system = Sys} = Mem,
	    [{ProcData, Sz} | Rest]) when ProcData == bif_timer;
					  ProcData == link_lh;
					  ProcData == process_table ->
    aa_mem_data(Mem#memory{processes = Proc+Sz,
			   processes_used = ProcU+Sz,
			   system = Sys-Sz},
		Rest);
aa_mem_data(#memory{code = Code} = Mem,
	    [{CodeData, Sz} | Rest]) when CodeData == module_table;
					  CodeData == export_table;
					  CodeData == export_list;
					  CodeData == fun_table;
					  CodeData == module_refs;
					  CodeData == loaded_code ->
    aa_mem_data(Mem#memory{code = Code+Sz},
		Rest);
aa_mem_data(EMD, [{_, _} | Rest]) ->
    aa_mem_data(EMD, Rest);
aa_mem_data(#memory{total = Tot,
		    processes = Proc,
		    system = Sys} = Mem,
	    []) when Sys =< 0 ->
    %% Instrumented runtime system -> Sys = Tot - Proc
    Mem#memory{system = Tot - Proc};
aa_mem_data(EMD, []) ->
    EMD.

aa_mem_data(notsup) ->
    notsup;
aa_mem_data(EMD) ->
    aa_mem_data(EMD, erlang:system_info(allocated_areas)).

%%
%% alloc_info/1 and alloc_sizes/1 are for internal use only (used by
%% erlang:system_info({allocator|allocator_sizes, _})).
%%

alloc_info(Allocs) ->
    get_alloc_info(allocator, Allocs).

alloc_sizes(Allocs) ->
    get_alloc_info(allocator_sizes, Allocs).

get_alloc_info(Type, AAtom) when is_atom(AAtom) ->
    [{AAtom, Result}] = get_alloc_info(Type, [AAtom]),
    Result;
get_alloc_info(Type, AList) when is_list(AList) ->
    Ref = make_ref(),
    erlang:system_info({Type, Ref, AList}),
    receive_allocator(Ref,
		      erlang:system_info(schedulers),
		      mk_res_list(AList)).

mk_res_list([]) ->
    [];
mk_res_list([Alloc | Rest]) ->
    [{Alloc, []} | mk_res_list(Rest)].

insert_instance(I, N, []) ->
    [{instance, N, I}];
insert_instance(I, N, [{instance, M, _}|_] = Rest) when N < M ->
    [{instance, N, I} | Rest];
insert_instance(I, N, [Prev|Rest]) ->
    [Prev | insert_instance(I, N, Rest)].

insert_info([], Ys) ->
    Ys;
insert_info([{A, false}|Xs], [{A, _IList}|Ys]) ->
    insert_info(Xs, [{A, false}|Ys]);
insert_info([{A, N, I}|Xs], [{A, IList}|Ys]) ->
    insert_info(Xs, [{A, insert_instance(I, N, IList)}|Ys]);
insert_info([{A1, _}|_] = Xs, [{A2, _} = Y | Ys]) when A1 /= A2 ->
    [Y | insert_info(Xs, Ys)];
insert_info([{A1, _, _}|_] = Xs, [{A2, _} = Y | Ys]) when A1 /= A2 ->
    [Y | insert_info(Xs, Ys)].

receive_allocator(_Ref, 0, Acc) ->
    Acc;
receive_allocator(Ref, N, Acc) ->
    receive
	{Ref, _, InfoList} ->
	    receive_allocator(Ref, N-1, insert_info(InfoList, Acc))
    end.

-spec await_sched_wall_time_modifications(Ref, Result) -> boolean() when
      Ref :: reference(),
      Result :: boolean().

await_sched_wall_time_modifications(Ref, Result) ->
    sched_wall_time(Ref, erlang:system_info(schedulers)),
    Result.

-spec gather_sched_wall_time_result(Ref) -> [{pos_integer(),
					      non_neg_integer(),
					      non_neg_integer()}] when
      Ref :: reference().

gather_sched_wall_time_result(Ref) when is_reference(Ref) ->
    sched_wall_time(Ref, erlang:system_info(schedulers), []).

sched_wall_time(_Ref, 0) ->
    ok;
sched_wall_time(Ref, N) ->
    receive Ref -> sched_wall_time(Ref, N-1) end.

sched_wall_time(_Ref, 0, Acc) ->
    Acc;
sched_wall_time(Ref, N, undefined) ->
    receive {Ref, _} -> sched_wall_time(Ref, N-1, undefined) end;
sched_wall_time(Ref, N, Acc) ->
    receive
	{Ref, undefined} -> sched_wall_time(Ref, N-1, undefined);
	{Ref, SWT} -> sched_wall_time(Ref, N-1, [SWT|Acc])
    end.
