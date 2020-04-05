%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2017. All Rights Reserved.
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
-module(dbg).
-export([p/1,p/2,c/3,c/4,i/0,start/0,stop/0,stop_clear/0,tracer/0,
	 tracer/2, tracer/3, get_tracer/0, get_tracer/1, tp/2, tp/3, tp/4, 
	 tpe/2, ctpe/1,
	 ctp/0, ctp/1, ctp/2, ctp/3, tpl/2, tpl/3, tpl/4, ctpl/0, ctpl/1, 
	 ctpl/2, ctpl/3, ctpg/0, ctpg/1, ctpg/2, ctpg/3, ltp/0, wtp/1, rtp/1, 
	 dtp/0, dtp/1, n/1, cn/1, ln/0, h/0, h/1]).

-export([trace_port/2, flush_trace_port/0, flush_trace_port/1,
	 trace_port_control/1, trace_port_control/2, trace_client/2, 
	 trace_client/3, stop_trace_client/1]).

-export([transform_flags/1,dhandler/2]).

-export([fun2ms/1]).

%% Local exports
-export([erlang_trace/3,get_info/0,deliver_and_flush/1]).

%% Debug exports
-export([wrap_presort/2, wrap_sort/2, wrap_postsort/1, wrap_sortfix/2,
	 match_front/2, match_rear/2,
	 match_0_9/1]).


%%% Shell callable utility
fun2ms(ShellFun) when is_function(ShellFun) ->
    % Check that this is really a shell fun...
    case erl_eval:fun_data(ShellFun) of
        {fun_data,ImportList,Clauses} ->
            case ms_transform:transform_from_shell(
                   ?MODULE,Clauses,ImportList) of
                {error,[{_,[{_,_,Code}|_]}|_],_} ->
                    Modifier = modifier(),
                    io:format("Error: ~"++Modifier++"s~n",
                              [ms_transform:format_error(Code)]),
                    {error,transform_error};
                Else ->
                    Else
            end;
        false ->
            exit({badarg,{?MODULE,fun2ms,
                          [function,called,with,real,'fun',
                           should,be,transformed,with,
                           parse_transform,'or',called,with,
                           a,'fun',generated,in,the,
                           shell]}})
    end.


%%% Client functions.

%%
%% n(Node) -> {ok, Node} | {error, Reason}
%% Adds Node to the list of traced nodes.
%%
n(Node) when Node =:= node() ->
    {error, cant_add_local_node};
n(Node) ->
    case (catch net_adm:ping(Node)) of
	{'EXIT',_} ->
	    {error, {bad_node, Node}};
	pang ->
	    {error, {nodedown, Node}};
	pong ->
	    req({add_node, Node});
	Other ->
	    {error, Other}
    end.

%%
%% cn(Node) -> ok
%% Remove Node from the list of traced nodes.
%%    
cn(Node) ->
    req({remove_node, Node}).

%%
%% ln() -> ok
%% List traced nodes
%%
ln() ->
    lists:foreach(fun(X) ->
                          io:format("~p~n",[X])
                  end,
                  req(get_nodes)),
    ok.

%%
%% tp/tpl(Module, Pattern) | tp/tpl(Module,Function,Pattern) |
%% tp/tpl(Module,Function,Arity,Pattern) | tp/tpl({M,F,A},Pattern) 
%% -> {ok, [{matched,Node,N}]} | 
%%    {ok, [{matched,Node,N}, {saved,M}]} | 
%%    {ok, [{saved,M}]} | 
%%    {error, Reason}
%% Set trace pattern for function or group of functions.
%%
tp(Module, Function, Pattern) ->
    do_tp({Module, Function, '_'}, Pattern, []).
tp(Module, Function, Arity, Pattern) ->
    do_tp({Module, Function, Arity}, Pattern, []).
tp(Module, Pattern) when is_atom(Module) ->
    do_tp({Module, '_', '_'}, Pattern, []);
tp({_Module, _Function, _Arity} = X, Pattern) ->
    do_tp(X,Pattern,[]).
tpl(Module, Function, Pattern) ->
    do_tp({Module, Function, '_'}, Pattern, [local]).
tpl(Module, Function, Arity, Pattern) ->
    do_tp({Module, Function, Arity}, Pattern, [local]).
tpl(Module, Pattern) when is_atom(Module) ->
    do_tp({Module, '_', '_'}, Pattern, [local]);
tpl({_Module, _Function, _Arity} = X, Pattern) ->
    do_tp(X,Pattern,[local]).

tpe(Event, Pattern) when Event =:= send;
			 Event =:= 'receive' ->
    do_tp(Event, Pattern, []).

do_tp(X, Pattern, Flags)
  when is_integer(Pattern);
       is_atom(Pattern) ->
    case ets:lookup(get_pattern_table(), Pattern) of
	[{_,NPattern}] ->
	    do_tp(X, binary_to_term(NPattern), Flags);
	_ ->
	    {error, unknown_pattern}
    end;
do_tp(X, Pattern, Flags) when is_list(Pattern) ->
    Nodes = req(get_nodes),
    case X of
	{M,_,_} when is_atom(M) ->
	    %% Try to load M on all nodes
	    lists:foreach(fun(Node) ->
				  rpc:call(Node, M, module_info, [])
			  end,
			  Nodes);
	_ -> ok
    end,
    case lint_tp(Pattern) of
	{ok,_} ->
	    SaveInfo = case save_pattern(Pattern) of
			   N when is_integer(N), N > 0;  is_atom(N)  ->
			       [{saved, N}];
			   _ ->
			       []
		       end,
	    {ok, do_tp_on_nodes(Nodes, X, Pattern, Flags) ++ SaveInfo};
	Other ->
	    Other
    end.

%% All nodes are handled the same way - also the local node if it is traced
do_tp_on_nodes(Nodes, X, P, Flags) ->
    lists:map(fun(Node) ->
		      case rpc:call(Node,erlang,trace_pattern,[X,P, Flags]) of
			  N when is_integer(N) ->
			      {matched, Node, N};
			  Else ->
			      {matched, Node, 0, Else}
		      end
	      end,
	      Nodes).

%%
%% ctp/ctpl(Module) | ctp/ctpl(Module,Function) | 
%% ctp/ctpl(Module,Function,Arity) | ctp/ctpl({M,F,A}) ->
%% {ok, [{matched, N}]} | {error, Reason}
%% Clears trace pattern for function or group of functions.
%%
ctp() ->
    do_ctp({'_','_','_'},[]).
ctp(Module, Function) ->
    do_ctp({Module, Function, '_'}, []).
ctp(Module, Function, Arity) ->
    do_ctp({Module, Function, Arity}, []).
ctp(Module) when is_atom(Module) ->
    do_ctp({Module, '_', '_'}, []);
ctp({_Module, _Function, _Arity} = X) ->
    do_ctp(X,[]).
ctpl() ->
    do_ctp({'_', '_', '_'}, [local]).    
ctpl(Module, Function) ->
    do_ctp({Module, Function, '_'}, [local]).
ctpl(Module, Function, Arity) ->
    do_ctp({Module, Function, Arity}, [local]).
ctpl(Module) when is_atom(Module) ->
    do_ctp({Module, '_', '_'}, [local]);
ctpl({_Module, _Function, _Arity} = X) ->
    do_ctp(X,[local]).
ctpg() ->
    do_ctp({'_', '_', '_'}, [global]).
ctpg(Module, Function) ->
    do_ctp({Module, Function, '_'}, [global]).
ctpg(Module, Function, Arity) ->
    do_ctp({Module, Function, Arity}, [global]).
ctpg(Module) when is_atom(Module) ->
    do_ctp({Module, '_', '_'}, [global]);
ctpg({_Module, _Function, _Arity} = X) ->
    do_ctp(X,[global]).

do_ctp({Module, Function, Arity},[]) ->
    {ok,_} = do_ctp({Module, Function, Arity},[global]),
    do_ctp({Module, Function, Arity},[local]);
do_ctp({_Module, _Function, _Arity}=MFA,Flags) ->
    Nodes = req(get_nodes),
    {ok,do_tp_on_nodes(Nodes,MFA,false,Flags)}.

ctpe(Event) when Event =:= send;
		 Event =:= 'receive' ->
    Nodes = req(get_nodes),
    {ok,do_tp_on_nodes(Nodes,Event,true,[])}.

%%
%% ltp() -> ok
%% List saved and built-in trace patterns.
%%
ltp() ->
    Modifier = modifier(),
    Format = "~p: ~"++Modifier++"p~n",
    pt_doforall(fun({X, El},_Ignore) -> 
			io:format(Format, [X,El])
		end,[]).

%%
%% dtp() | dtp(N) -> ok
%% Delete saved pattern with number N or all saved patterns
%%
%% Do not delete built-in trace patterns.
dtp() ->
    pt_doforall(fun ({Key, _}, _) when is_integer(Key) ->
			dtp(Key);
		    ({_, _}, _) ->
			ok
		end,
		[]).
dtp(N) when is_integer(N) ->
    ets:delete(get_pattern_table(), N),
    ok;
dtp(_) ->
    ok.

%%
%% wtp(FileName) -> ok | {error, Reason}
%% Writes all current saved trace patterns to a file.
%%
%% Actually write the built-in trace patterns too.
wtp(FileName) ->
    case file:open(FileName,[write,{encoding,utf8}]) of
	{error, Reason} ->
	    {error, Reason};
	{ok, File} ->
            io:format(File, "%% ~s\n", [epp:encoding_to_string(utf8)]),
	    pt_doforall(fun ({_, Val}, _) when is_list(Val) ->
				io:format(File, "~tp.~n", [Val]);
			    ({_, _}, _) ->
				ok
			end,
			[]),
	    ok = file:close(File)
    end.

%%
%% rtp(FileName) -> ok | {error, Reason}
%% Reads in previously saved pattern file and merges the contents
%% with what's there now.
%%
%% So the saved built-in trace patterns will merge with
%% the already existing, which should be the same.
rtp(FileName) ->
    T = get_pattern_table(),
    case file:consult(FileName) of
	{error, Reason1} ->
	    {error, {read_error, Reason1}};
	{ok, Data} ->
	    case check_list(Data) of
		ok ->
		    lists:foreach(fun(X) ->
					  save_pattern(X,T)
				  end, Data),
		    ok;
		{error, Reason2} ->
		    {error, {file_format_error, Reason2}}
	    end
    end.

tracer() ->
    tracer(process, {fun dhandler/2,user}).

tracer(port, Fun) when is_function(Fun) ->
    start(Fun);

tracer(port, Port) when is_port(Port) ->
    start(fun() -> Port end);

tracer(process, {Handler,HandlerData}) ->
    start(fun() -> start_tracer_process(Handler, HandlerData) end);

tracer(module, Fun) when is_function(Fun) ->
    start(Fun);
tracer(module, {Module, State}) ->
    start(fun() -> {Module, State} end).


remote_tracer(port, Fun) when is_function(Fun) ->
    remote_start(Fun);

remote_tracer(port, Port) when is_port(Port) ->
    remote_start(fun() -> Port end);

remote_tracer(process, {Handler,HandlerData}) ->
    remote_start(fun() -> start_tracer_process(Handler, HandlerData) end);

remote_tracer(module, Fun) when is_function(Fun) ->
    remote_start(Fun);
remote_tracer(module, {Module, State}) ->
    remote_start(fun() -> {Module, State} end).


remote_start(StartTracer) ->
    case (catch StartTracer()) of
	{'EXIT', Reason} ->
	    {error, Reason};
	Tracer ->
	    {ok,Tracer}
    end.

%%
%% tracer(Node,Type,Data) -> {ok, Node} | {error, Reason}
%% Add Node to the list of traced nodes and a trace port defined by
%% Type and Data is started on Node.
%%
tracer(Node,Type,Data) when Node =:= node() ->
    case tracer(Type,Data) of
	{ok,_Dbg} -> {ok,Node};
	Error -> Error
    end;
tracer(Node,Type,Data) ->
    case (catch net_adm:ping(Node)) of
	{'EXIT',_} ->
	    {error, {bad_node, Node}};
	pang ->
	    {error, {nodedown, Node}};
	pong ->
	    req({add_node, Node, Type, Data});
	Other ->
	    {error, Other}
    end.

flush_trace_port() ->
    trace_port_control(flush).
flush_trace_port(Node) ->
    trace_port_control(Node, flush).

trace_port_control(Operation) ->
    trace_port_control(node(), Operation).

trace_port_control(Node, flush) ->
    case get_tracer(Node) of
	{ok, Port} when is_port(Port) ->
	    case catch rpc:call(Node,?MODULE,deliver_and_flush,[Port]) of
		[0] ->
		    ok;
		_ ->
		    {error, not_supported_by_trace_driver}
	    end;
	_ ->
	    {error, no_trace_driver}
    end;
trace_port_control(Node,get_listen_port) ->
    case trace_port_control(Node,$p, "") of
	{ok, <<0, IpPort:16>>} ->
	    {ok, IpPort};
	{ok, _Other} ->
	    {error, not_supported_by_trace_driver};
	Other ->
	    Other
    end.

trace_port_control(Node, Command, Arg) ->
    case get_tracer(Node) of
	{ok, Port} when is_port(Port) ->
	    {ok, catch rpc:call(Node,erlang,port_control,[Port, Command, Arg])};
	_ ->
	    {error, no_trace_driver}
    end.
    
%% A bit more than just flush - it also makes sure all trace messages
%% are delivered first, before flushing the driver.
deliver_and_flush(Port) ->
    Ref = erlang:trace_delivered(all),
    receive
	{trace_delivered,all,Ref} -> ok
    end,
    erlang:port_control(Port, $f, "").
					   

trace_port(file, {Filename, wrap, Tail}) ->
    trace_port(file, {Filename, wrap, Tail, 128*1024});
trace_port(file, {Filename, wrap, Tail, WrapSize}) ->
    trace_port(file, {Filename, wrap, Tail, WrapSize, 8});
trace_port(file, {Filename, wrap, Tail, WrapSize, WrapCnt})
  when is_list(Tail), 
       is_integer(WrapSize), WrapSize >= 0, WrapSize < (1 bsl 32),
       is_integer(WrapCnt), WrapCnt >= 1, WrapCnt < (1 bsl 32) ->
    trace_port1(file, Filename, {wrap, Tail, WrapSize, WrapCnt, 0});
trace_port(file, {Filename, wrap, Tail, {time, WrapTime}, WrapCnt})
  when is_list(Tail), 
       is_integer(WrapTime), WrapTime >= 1, WrapTime < (1 bsl 32),
       is_integer(WrapCnt), WrapCnt >= 1, WrapCnt < (1 bsl 32) ->
    trace_port1(file, Filename, {wrap, Tail, 0, WrapCnt, WrapTime});
trace_port(file, Filename) ->
    trace_port1(file, Filename, nowrap);

trace_port(ip, Portno) when is_integer(Portno) -> 
    trace_port(ip,{Portno,200});

trace_port(ip, {Portno, Qsiz}) when is_integer(Portno), is_integer(Qsiz) -> 
    fun() ->
	    Driver = "trace_ip_drv",
	    Dir1 = filename:join(code:priv_dir(runtime_tools), "lib"),
	    case catch erl_ddll:load_driver(Dir1, Driver) of
		ok ->
		    ok;
		_ ->
		    Dir2 = filename:join(
			     Dir1, 
			     erlang:system_info(system_architecture)),
		    catch erl_ddll:load_driver(Dir2, Driver)
	    end,
	    L = lists:flatten(
		  io_lib:format("~s ~p ~p 2",
				[Driver, Portno, Qsiz])),
	    open_port({spawn, L}, [eof])
    end.

trace_port1(file, Filename, Options) ->
    Driver = "trace_file_drv",
    fun() ->
	    Name = filename:absname(Filename), 
	    %% Absname is needed since the driver uses 
	    %% the supplied name without further investigations.

	    %% Also, the absname must be found inside the fun,
	    %% in case the actual node where the port shall be
	    %% started is on another node (or even another host)
	    {Wrap, Tail} =
		case Options of
		    {wrap, T, WrapSize, WrapCnt, WrapTime} ->
			{lists:flatten(
			   io_lib:format("w ~p ~p ~p ~p ", 
					 [WrapSize, WrapCnt, WrapTime, 
					  length(Name)])),
			 T};
		    nowrap ->
			{"", ""}
		end,
	    Command = Driver ++ " " ++ Wrap ++ "n " ++ Name ++ Tail,
	    Dir1 = filename:join(code:priv_dir(runtime_tools), "lib"),
	    case catch erl_ddll:load_driver(Dir1, Driver) of
		ok ->
		    ok;
		_ ->
		    Dir2 = filename:join(
			     Dir1, 
			     erlang:system_info(system_architecture)),
		    catch erl_ddll:load_driver(Dir2, Driver)
	    end,
	    if 	element(1, Options) == wrap ->
		    %% Delete all files from any previous wrap log
		    Files = wrap_postsort(wrap_presort(Name, Tail)),
		    lists:foreach(
		      fun(N) -> file:delete(N) end,
		      Files);
		true -> ok
	    end,
	    open_port({spawn, Command}, [eof])
    end.


trace_client(file, Filename) ->
    trace_client(file, Filename, {fun dhandler/2,user});
trace_client(follow_file, Filename) ->
    trace_client(follow_file, Filename, {fun dhandler/2,user});
trace_client(ip, Portno) when is_integer(Portno) ->
    trace_client1(ip, {"localhost", Portno}, {fun dhandler/2,user});
trace_client(ip, {Host, Portno}) when is_integer(Portno) ->
    trace_client1(ip, {Host, Portno}, {fun dhandler/2,user}).

trace_client(file, {Filename, wrap, Tail}, FD) ->
    trace_client(file, {Filename, wrap, Tail, 128*1024}, FD);
trace_client(file, {Filename, wrap, Tail, WrapSize}, FD) ->
    trace_client(file, {Filename, wrap, Tail, WrapSize, 8}, FD);
trace_client(file, 
	     {_Filename, wrap, Tail, _WrapSize, WrapCnt} = WrapSpec, 
	     {Fun, _Data} = FD)
  when is_list(Tail), is_function(Fun), is_integer(WrapCnt), WrapCnt >= 1 ->
    trace_client1(file, WrapSpec, FD);
trace_client(file, Filename, {Fun, Data} ) when is_function(Fun) ->
    trace_client1(file, Filename, {Fun, Data});
trace_client(follow_file, Filename, {Fun, Data} ) when is_function(Fun) ->
    trace_client1(follow_file, Filename, {Fun, Data});
trace_client(ip, Portno, {Fun, Data}) when is_integer(Portno), is_function(Fun) ->
    trace_client1(ip, {"localhost", Portno}, {Fun, Data});
trace_client(ip, {Host, Portno}, {Fun, Data}) when is_integer(Portno), 
						   is_function(Fun) ->
    trace_client1(ip, {Host, Portno}, {Fun, Data}).

trace_client1(Type, OpenData, {Handler,HData}) ->
    case req({link_to, 
	      spawn(
		fun() ->
			tc_loop(gen_reader(Type, OpenData), Handler, HData)
		end)}) of
	{ok, Pid} ->
	    Pid;
	Other ->
	    Other
    end.

stop_trace_client(Pid) when is_pid(Pid) ->
    process_flag(trap_exit,true),
    link(Pid),
    exit(to_pidspec(Pid),abnormal),
    Res = receive 
	      {'EXIT', Pid, _} ->
		  ok
	  after 5000 ->
		  {error, timeout}
	  end,
    process_flag(trap_exit,false),
    Res.

p(Pid) ->
    p(Pid, [m]).

p(Pid, Flags) when is_atom(Flags) ->
    p(Pid, [Flags]);
p(Pid, Flags) ->
    req({p,Pid,Flags}).

i() -> req(i).
	
c(M, F, A) ->
    c(M, F, A, all).
c(M, F, A, Flags) when is_atom(Flags) ->
    c(M, F, A, [Flags]);
c(M, F, A, Flags) ->
    case transform_flags(Flags) of
	{error,Reason} -> {error,Reason};
	Flags1 ->
	    tracer(),
	    S = self(),
	    Pid = spawn(fun() -> c(S, M, F, A, [get_tracer_flag() | Flags1]) end),
	    Mref = erlang:monitor(process, Pid),
	    receive
		{'DOWN', Mref, _, _, Reason} ->
		    stop_clear(),
		    {error, Reason};
		{Pid, Res} ->
		    erlang:demonitor(Mref, [flush]),
		    %% 'sleep' prevents the tracer (recv_all_traces) from
		    %% receiving garbage {'EXIT',...} when dbg i stopped.
		    timer:sleep(1),
		    stop_clear(),
		    Res
	    end
    end.

c(Parent, M, F, A, Flags) ->
    %% The trace BIF is used directly here instead of the existing function
    %% p/2. The reason is that p/2 (when stopping trace) sends messages which 
    %% we don't want to show up in this simple tracing from the shell.
    erlang:trace(self(), true, Flags),
    Res = apply(M, F, A),
    erlang:trace(self(), false, [all]),
    Parent ! {self(), Res}.

stop() ->
    Mref = erlang:monitor(process, dbg),
    catch dbg ! {self(),stop},
    receive
	{'DOWN',Mref,_,_,_} ->
	    ok
    end.

stop_clear() ->
    {ok, _} = ctp(),
    {ok, _} = ctpe('receive'),
    {ok, _} = ctpe('send'),
    stop().

%%% Calling the server.

req(R) ->
    P = ensure(), % The pid or perhaps the name of the server
    Mref = erlang:monitor(process, P),
    catch P ! {self(), R}, % May crash if P = atom() and server died
    receive
	{'DOWN', Mref, _, _, _} -> % If server died
	    exit(dbg_server_crash);
	{dbg, Reply} ->
	    erlang:demonitor(Mref, [flush]),
	    Reply
    end.

%% Returns the pid of the dbg server, or in worst case the name.
%% Starts a new server if necessary.
ensure() ->
    case whereis(dbg) of
	undefined -> 
	    case start() of
		{ok, P} ->
		    P;
		{error, already_started} ->
		    dbg
	    end;
	Pid -> 
	    Pid
    end.


%%% Server implementation.
start() ->
    start(no_tracer).

start(TracerFun) ->
    S = self(),
    case whereis(dbg) of
	undefined ->
	    Dbg = spawn(fun() -> init(S) end),
	    receive {Dbg,started} -> ok end,
	    case TracerFun of
		no_tracer ->
		    {ok, Dbg};
		Fun when is_function(Fun) ->
		    req({tracer,TracerFun})
	    end;
	Pid when is_pid(Pid), is_function(TracerFun) ->
	    req({tracer,TracerFun})
    end.

init(Parent) ->
    process_flag(trap_exit, true),
    register(dbg, self()),
    Parent ! {self(),started},
    loop({[],[]},[]).

%
% SurviveLinks = Processes we should take with us while falling, 
%                but not get killed by if they die (i. e. trace clients 
%                and relay processes on other nodes)
%                SurviveLinks = {TraceClients,Relays}
%
loop({C,T}=SurviveLinks, Table) ->
    receive
	{From,i} ->
            Modifier = modifier(),
            Reply = display_info(lists:map(fun({N,_}) -> N end,get()), Modifier),
	    reply(From, Reply),
	    loop(SurviveLinks, Table);
	{From,{p,Pid,Flags}} ->
	    reply(From, trace_process(Pid, Flags)),
	    loop(SurviveLinks, Table);
	{From,{tracer,TracerFun}} when is_function(TracerFun) ->
	    case get(node()) of
		undefined ->
		    case (catch TracerFun()) of
			{'EXIT', Reason} ->
			    reply(From, {error, Reason});
			Tracer when is_pid(Tracer); is_port(Tracer) ->
			    put(node(),{self(),Tracer}),
			    reply(From, {ok,self()});
                        {Module, _State} = Tracer when is_atom(Module) ->
                            put(node(),{self(),Tracer}),
			    reply(From, {ok,self()})
		    end;
		{_Relay,_Tracer} ->
		    reply(From, {error, already_started})
	    end,
	    loop(SurviveLinks,Table);
	{From,{get_tracer,Node}} ->
	    case get(Node) of
		undefined -> reply(From,{error, {no_tracer_on_node,Node}});
		{_Relay,Tracer} -> reply(From, {ok,Tracer})
	    end,
	    loop(SurviveLinks, Table);
	{From, get_table} ->
	    Tab = case Table of
		      [] ->
			  new_pattern_table();
		      _exists ->
			  Table
		  end,
	    reply(From, {ok, Tab}),
	    loop(SurviveLinks, Tab);
	{_From,stop} ->
	    %% We want to make sure that all trace messages have been delivered
	    %% on all nodes that might be traced. Since dbg:cn/1 does not turn off
	    %% tracing on the node it removes from the list of active trace nodes,
	    %% we will call erlang:trace_delivered/1 on ALL nodes that we have
	    %% connections to.
	    %% If it is a file trace driver, we will also flush the port.
	    lists:foreach(fun({Node,{_Relay,Port}}) ->
				  rpc:call(Node,?MODULE,deliver_and_flush,[Port])
			  end,
			  get()),
	    exit(done);
	{From, {link_to, Pid}} -> 	    
	    case (catch link(Pid)) of
		{'EXIT', Reason} ->
		    reply(From, {error, Reason}),
		    loop(SurviveLinks, Table);
		_ ->
		    reply(From, {ok, Pid}),
		    loop({[Pid|C],T}, Table)
	    end;
	{From, {add_node, Node}} ->
	    case get(node()) of
		undefined -> 
		    reply(From, {error, no_local_tracer}),
		    loop(SurviveLinks, Table);
		{_LocalRelay,Tracer} when is_port(Tracer) -> 
		    reply(From, {error, cant_trace_remote_pid_to_local_port}),
		    loop(SurviveLinks, Table);
		{_LocalRelay,Tracer} when is_tuple(Tracer) -> 
		    reply(From, {error, cant_trace_remote_pid_to_local_module}),
		    loop(SurviveLinks, Table);
	        {_LocalRelay,Tracer} when is_pid(Tracer) ->
		    case (catch relay(Node, Tracer)) of
			{ok,Relay} ->
			    reply(From, {ok, Node}),
			    loop({C,[Relay|T]}, Table);
			{'EXIT', Something} ->
			    reply(From, {error, Something}),
			    loop(SurviveLinks, Table);
			Error ->
			    reply(From, Error),
			    loop(SurviveLinks, Table)
		    end
	    end;
	{From, {add_node, Node, Type, Data}} ->
	    case (catch relay(Node, {Type,Data})) of
		{ok,Relay} ->
		    reply(From, {ok, Node}),
		    loop({C,[Relay|T]}, Table);
		{'EXIT', Something} ->
		    reply(From, {error, Something}),
		    loop(SurviveLinks, Table);
		Error ->
		    reply(From, Error),
		    loop(SurviveLinks, Table)
	    end;
	{From, {remove_node, Node}} ->
	    erase(Node),
	    reply(From, ok),
	    loop(SurviveLinks, Table);
	{From, get_nodes} ->
	    reply(From, lists:map(fun({N,_}) -> N end, get())),
	    loop(SurviveLinks, Table);
	{'EXIT', Pid, Reason} ->
	    case lists:delete(Pid, C) of
		C ->
		    case lists:delete(Pid,T) of
			T ->
                            Modifier = modifier(user),
			    io:format(user,
                                      "** dbg got EXIT - terminating: ~"++
                                          Modifier++"p~n",
				      [Reason]),
			    exit(done);
			NewT -> 
			    erase(node(Pid)),
			    loop({C,NewT}, Table)
		    end;
		NewC ->
		    loop({NewC,T}, Table)
	    end;
	Other ->
            Modifier = modifier(user),
	    io:format(user,"** dbg got garbage: ~"++Modifier++"p~n",
		      [{Other,SurviveLinks,Table}]),
	    loop(SurviveLinks, Table)
    end.

reply(Pid, Reply) ->
    Pid ! {dbg,Reply},
    ok.


%%% A process-based tracer.

start_tracer_process(Handler, HandlerData) ->
    spawn_opt(fun() -> tracer_init(Handler, HandlerData) end,
	      [link,{priority,max}]).
    

tracer_init(Handler, HandlerData) ->
    process_flag(trap_exit, true),
    tracer_loop(Handler, HandlerData).

tracer_loop(Handler, Hdata) ->
    {State, Suspended, Traces} =  recv_all_traces(),
    NewHdata = handle_traces(Suspended, Traces, Handler, Hdata),
    case State of
	done ->
	    exit(normal);
	loop ->
	    tracer_loop(Handler, NewHdata)
    end.

recv_all_traces() ->
    recv_all_traces([], [], infinity).

recv_all_traces(Suspended0, Traces, Timeout) ->
    receive
	Trace when is_tuple(Trace), element(1, Trace) == trace ->
	    Suspended = suspend(Trace, Suspended0),
	    recv_all_traces(Suspended, [Trace|Traces], 0);
	Trace when is_tuple(Trace), element(1, Trace) == trace_ts ->
	    Suspended = suspend(Trace, Suspended0),
	    recv_all_traces(Suspended, [Trace|Traces], 0);
	Trace when is_tuple(Trace), element(1, Trace) == seq_trace ->
	    Suspended = suspend(Trace, Suspended0),
	    recv_all_traces(Suspended, [Trace|Traces], 0);
	Trace when is_tuple(Trace), element(1, Trace) == drop ->
	    Suspended = suspend(Trace, Suspended0),
	    recv_all_traces(Suspended, [Trace|Traces], 0);
	{'EXIT', _Pid, _Reason} ->
	    {done, Suspended0, Traces};
	Other ->
	    %%% Is this really a good idea?
            Modifier = modifier(user),
	    io:format(user,"** tracer received garbage: ~"++Modifier++"p~n",
                      [Other]),
	    recv_all_traces(Suspended0, Traces, Timeout)
    after Timeout ->
	    {loop, Suspended0, Traces}
    end.

handle_traces(Suspended, Traces, Handler, Hdata) ->
    case catch invoke_handler(Traces, Handler, Hdata) of
	{'EXIT',Reason} -> 
	    resume(Suspended),
	    exit({trace_handler_crashed,Reason});
	NewHdata ->
	    resume(Suspended),
	    NewHdata
    end.

invoke_handler([Tr|Traces], Handler, Hdata0) ->
    Hdata = invoke_handler(Traces, Handler, Hdata0),
    Handler(Tr, Hdata);
invoke_handler([], _Handler, Hdata) ->
    Hdata.

suspend({trace,From,call,_Func}, Suspended) when node(From) == node() ->
    case (catch erlang:suspend_process(From, [unless_suspending,
					      asynchronous])) of
	true ->
	    [From | Suspended];
	_ ->
	    Suspended
    end;
suspend(_Other, Suspended) -> Suspended.

resume([Pid|Pids]) when node(Pid) == node() ->
    (catch erlang:resume_process(Pid)),
    resume(Pids);
resume([]) -> ok.



%%% Utilities.

trac(Proc, How, Flags) when is_atom(Proc) ->
    %% Proc = all | new | existing | RegisteredName
    %% Must go to all nodes
    case get() of
	[] ->
	    {error,no_tracers};
	Nodes ->
	    Matched = [trac(Node, NodeInfo, Proc, How, Flags)
		       || {Node, NodeInfo} <- Nodes],
	    {ok,Matched}
    end;
trac(Proc, How, Flags) ->
    %% Proc = Pid | Integer | {X,Y,Z} | "<X.Y.Z>"
    %% One node only
    Pid = to_pid(Proc),
    case Pid of
	{badpid,_} ->
	    {error,Pid};
	_ ->
	    Node = if is_pid(Pid) -> node(Pid); true -> node() end,
	    case get(Node) of
		undefined ->
		    {error,{no_tracer_on_node,Node}};
		NodeInfo ->
		    Match = trac(Node, NodeInfo, Pid, How, Flags),
		    {ok,[Match]}
	    end
    end.

trac(Node, {_Replay, Tracer}, AtomPid, How, Flags) ->
    case rpc:call(Node, ?MODULE, erlang_trace,
		  [AtomPid, How, [get_tracer_flag(Tracer) | Flags]]) of
	N when is_integer(N) ->
	    {matched, Node, N};
	{badrpc,Reason} ->
	    {matched, Node, 0, Reason};
	Else ->
	    {matched, Node, 0, Else}
    end.

erlang_trace(AtomPid, How, Flags) ->
    case to_pidspec(AtomPid) of
	{badpid,_} ->
	    {no_proc,AtomPid};
	P ->
	    erlang:trace(P, How, Flags)
    end.

%% Since we are not allowed to do erlang:trace/3 on a remote
%% process, we create a relay process at the remote node.

relay(Node,To) when Node /= node() ->
    case get(Node) of
	undefined -> 
	    S = self(),
	    Pid = spawn_link(Node, fun() -> do_relay(S,To) end),
	    receive {started,Remote} -> put(Node, {Pid,Remote}) end,
	    {ok,Pid};
	{_Relay,PortOrPid} ->
	    {error, {already_started, PortOrPid}}
    end.

do_relay(Parent,RelP) ->
    process_flag(trap_exit, true),
    case RelP of
	{Type,Data} -> 
	    {ok,Tracer} = remote_tracer(Type,Data),
	    Parent ! {started,Tracer},
            ok;
	Pid when is_pid(Pid) ->
	    Parent ! {started,self()},
            ok
    end,
    do_relay_1(RelP).

do_relay_1(RelP) ->
    %% In the case of a port tracer, this process exists only so that
    %% dbg know that the node is alive... should maybe use monitor instead?
    receive
	{'EXIT', _P, _} ->
	    exit(normal);
	TraceInfo when is_pid(RelP) ->  % Here is the normal case for trace i/o
	    RelP ! TraceInfo, 
	    do_relay_1(RelP);
	Other ->
            Modifier = modifier(user),
	    io:format(user,"** relay got garbage: ~"++Modifier++"p~n", [Other]),
	    do_relay_1(RelP)
    end.

dhandler(end_of_trace, Out) ->
    Out;
dhandler(Trace, Out) when element(1, Trace) == trace, tuple_size(Trace) >= 3 ->
    dhandler1(Trace, tuple_size(Trace), out(Out));
dhandler(Trace, Out) when element(1, Trace) == trace_ts, tuple_size(Trace) >= 4 ->
    dhandler1(Trace, tuple_size(Trace)-1, element(tuple_size(Trace),Trace)
             , out(Out));
dhandler(Trace, Out) when element(1, Trace) == drop, tuple_size(Trace) =:= 2 ->
    {Device,Modifier} = out(Out),
    io:format(Device, "*** Dropped ~p messages.~n", [element(2,Trace)]),
    {Device,Modifier};
dhandler(Trace, Out) when element(1, Trace) == seq_trace,
                          tuple_size(Trace) >= 3 ->
    {Device,Modifier} = out(Out),
    SeqTraceInfo = case Trace of
		       {seq_trace, Lbl, STI, TS} ->
			   io:format(Device, "SeqTrace ~p [~p]: ",
				     [TS, Lbl]),
			   STI;
		       {seq_trace, Lbl, STI} ->
			  io:format(Device, "SeqTrace [~p]: ",
				     [Lbl]),
			   STI 
		   end,
    case SeqTraceInfo of
	{send, Ser, Fr, To, Mes} ->
	    io:format(Device, "(~p) ~p ! ~"++Modifier++"p [Serial: ~p]~n",
		      [Fr, To, Mes, Ser]);
	{'receive', Ser, Fr, To, Mes} ->
	    io:format(Device, "(~p) << ~"++Modifier++"p [Serial: ~p, From: ~p]~n",
		      [To, Mes, Ser, Fr]);
	{print, Ser, Fr, _, Info} ->
	    io:format(Device, "-> ~"++Modifier++"p [Serial: ~p, From: ~p]~n",
		      [Info, Ser, Fr]);
	Else ->
	    io:format(Device, "~"++Modifier++"p~n", [Else])
    end,
    {Device,Modifier};
dhandler(_Trace, Out) ->
    Out.

dhandler1(Trace, Size, {Device,Modifier}) ->
    From = element(2, Trace),
    case element(3, Trace) of
	'receive' ->
	    case element(4, Trace) of
		{dbg,ok} -> ok;
		Message ->
		    io:format(Device, "(~p) << ~"++Modifier++"p~n",
                              [From,Message])
	    end;
	'send' ->
	    Message = element(4, Trace),
	    To = element(5, Trace),
	    io:format(Device, "(~p) ~p ! ~"++Modifier++"p~n", [From,To,Message]);
	call ->
	    case element(4, Trace) of
		MFA when Size == 5 ->
		    Message = element(5, Trace),
		    io:format(Device,
                              "(~p) call ~"++Modifier++"s (~"++Modifier++"p)~n",
                              [From,ffunc(MFA,Modifier),Message]);
		MFA ->
		    io:format(Device, "(~p) call ~"++Modifier++"s~n",
                              [From,ffunc(MFA,Modifier)])
	    end;
	return -> %% To be deleted...
	    case element(4, Trace) of
		MFA when Size == 5 ->
		    Ret = element(5, Trace),
		    io:format(Device,
                              "(~p) old_ret ~"++Modifier++"s -> ~"++Modifier++
                                  "p~n",
                              [From,ffunc(MFA,Modifier),Ret]);
		MFA ->
		    io:format(Device, "(~p) old_ret ~"++Modifier++"s~n",
                              [From,ffunc(MFA,Modifier)])
	    end;
	return_from ->
	    MFA = element(4, Trace),
	    Ret = element(5, Trace),
	    io:format(Device,
                      "(~p) returned from ~"++Modifier++"s -> ~"++Modifier++"p~n",
                      [From,ffunc(MFA,Modifier),Ret]);
	return_to ->
	    MFA = element(4, Trace),
	    io:format(Device, "(~p) returning to ~"++Modifier++"s~n",
                      [From,ffunc(MFA,Modifier)]);
	spawn when Size == 5 ->
	    Pid = element(4, Trace),
	    MFA = element(5, Trace),
	    io:format(Device, "(~p) spawn ~p as ~"++Modifier++"s~n",
                      [From,Pid,ffunc(MFA,Modifier)]);
	Op ->
	    io:format(Device, "(~p) ~p ~"++Modifier++"s~n",
                      [From,Op,ftup(Trace,4,Size,Modifier)])
    end,
    {Device,Modifier}.

dhandler1(Trace, Size, TS, {Device,Modifier}) ->
    From = element(2, Trace),
    case element(3, Trace) of
	'receive' ->
	    case element(4, Trace) of
		{dbg,ok} -> ok;
		Message ->
		    io:format(Device,
                              "(~p) << ~"++Modifier++"p (Timestamp: ~p)~n",
                              [From,Message,TS])
	    end;
	'send' ->
	    Message = element(4, Trace),
	    To = element(5, Trace),
	    io:format(Device, "(~p) ~p ! ~"++Modifier++"p (Timestamp: ~p)~n",
                      [From,To,Message,TS]);
	call ->
	    case element(4, Trace) of
		MFA when Size == 5 ->
		    Message = element(5, Trace),
		    io:format(Device,
                              "(~p) call ~"++Modifier++"s (~"++Modifier++
                                  "p) (Timestamp: ~p)~n",
                              [From,ffunc(MFA,Modifier),Message,TS]);
		MFA ->
		    io:format(Device,
                              "(~p) call ~"++Modifier++"s (Timestamp: ~p)~n",
                              [From,ffunc(MFA,Modifier),TS])
	    end;
	return -> %% To be deleted...
	    case element(4, Trace) of
		MFA when Size == 5 ->
		    Ret = element(5, Trace),
		    io:format(Device,
                              "(~p) old_ret ~"++Modifier++"s -> ~"++Modifier++
                                  "p (Timestamp: ~p)~n",
                              [From,ffunc(MFA,Modifier),Ret,TS]);
		MFA ->
		    io:format(Device,
                              "(~p) old_ret ~"++Modifier++"s (Timestamp: ~p)~n",
                              [From,ffunc(MFA,Modifier),TS])
	    end;
	return_from ->
	    MFA = element(4, Trace),
	    Ret = element(5, Trace),
	    io:format(Device,
                      "(~p) returned from ~"++Modifier++"s -> ~"++Modifier++
                          "p (Timestamp: ~p)~n",
                      [From,ffunc(MFA,Modifier),Ret,TS]);
	return_to ->
	    MFA = element(4, Trace),
	    io:format(Device,
                      "(~p) returning to ~"++Modifier++"s (Timestamp: ~p)~n",
                      [From,ffunc(MFA,Modifier),TS]);
	spawn when Size == 5 ->
	    Pid = element(4, Trace),
	    MFA = element(5, Trace),
	    io:format(Device,
                      "(~p) spawn ~p as ~"++Modifier++"s (Timestamp: ~p)~n",
                      [From,Pid,ffunc(MFA,Modifier),TS]);
	Op ->
	    io:format(Device, "(~p) ~p ~"++Modifier++"s (Timestamp: ~p)~n",
                      [From,Op,ftup(Trace,4,Size,Modifier),TS])
    end,
    {Device,Modifier}.

%%% These f* functions returns non-flat strings

%% {M,F,[A1, A2, ..., AN]} -> "M:F(A1, A2, ..., AN)"
%% {M,F,A}                 -> "M:F/A"
ffunc({M,F,Argl},Modifier) when is_list(Argl) ->
    io_lib:format("~p:~"++Modifier++"p(~"++Modifier++"s)",
                  [M, F, fargs(Argl,Modifier)]);
ffunc({M,F,Arity},Modifier) ->
    io_lib:format("~p:~"++Modifier++"p/~p", [M,F,Arity]);
ffunc(X,Modifier) -> io_lib:format("~"++Modifier++"p", [X]).

%% Integer           -> "Integer"
%% [A1, A2, ..., AN] -> "A1, A2, ..., AN"
fargs(Arity,_) when is_integer(Arity) -> integer_to_list(Arity);
fargs([],_) -> [];
fargs([A],Modifier) ->
    io_lib:format("~"++Modifier++"p", [A]);  %% last arg
fargs([A|Args],Modifier) ->
    [io_lib:format("~"++Modifier++"p,", [A]) | fargs(Args,Modifier)];
fargs(A,Modifier) ->
    io_lib:format("~"++Modifier++"p", [A]). % last or only arg

%% {A_1, A_2, ..., A_N} -> "A_Index A_Index+1 ... A_Size"
ftup(Trace, Index, Index, Modifier) ->
    io_lib:format("~"++Modifier++"p", [element(Index, Trace)]);
ftup(Trace, Index, Size, Modifier) ->
    [io_lib:format("~"++Modifier++"p ", [element(Index, Trace)])
     | ftup(Trace, Index+1, Size, Modifier)].

out({_,_}=Out) ->
    Out;
out(Device) ->
    {Device,modifier(Device)}.

modifier() ->
    modifier(group_leader()).
modifier(Device) ->
    Encoding =
        case io:getopts(Device) of
            List when is_list(List) ->
                proplists:get_value(encoding,List,latin1);
            _ ->
                latin1
        end,
    encoding_to_modifier(Encoding).

encoding_to_modifier(latin1) -> "";
encoding_to_modifier(_) -> "t".

trace_process(Pid, [clear]) ->
    trac(Pid, false, all());
trace_process(Pid, Flags0) ->
    case transform_flags(Flags0) of
	{error,Reason} -> {error,Reason};
	Flags -> trac(Pid, true, Flags)
    end.

transform_flags(Flags0) ->
    transform_flags(Flags0,[]).
transform_flags([],Acc) -> Acc;
transform_flags([m|Tail],Acc) -> transform_flags(Tail,[send,'receive'|Acc]);
transform_flags([s|Tail],Acc) -> transform_flags(Tail,[send|Acc]);
transform_flags([r|Tail],Acc) -> transform_flags(Tail,['receive'|Acc]);
transform_flags([c|Tail],Acc) -> transform_flags(Tail,[call|Acc]);
transform_flags([call|Tail],Acc) -> transform_flags(Tail,[call|Acc]);
transform_flags([p|Tail],Acc) -> transform_flags(Tail,[procs|Acc]);
transform_flags([sos|Tail],Acc) -> transform_flags(Tail,[set_on_spawn|Acc]);
transform_flags([sol|Tail],Acc) -> transform_flags(Tail,[set_on_link|Acc]);
transform_flags([sofs|Tail],Acc) -> transform_flags(Tail,[set_on_first_spawn|Acc]);
transform_flags([sofl|Tail],Acc) -> transform_flags(Tail,[set_on_first_link|Acc]);
transform_flags([all|_],_Acc) -> all()--[silent,running];
transform_flags([F|Tail]=List,Acc) when is_atom(F) ->
    case lists:member(F, all()) of
	true -> transform_flags(Tail,[F|Acc]);
	false -> {error,{bad_flags,List}}
    end;
transform_flags(Bad,_Acc) -> {error,{bad_flags,Bad}}.

all() ->
    [send,'receive',call,procs,ports,garbage_collection,running,
     set_on_spawn,set_on_first_spawn,set_on_link,set_on_first_link,
     timestamp,monotonic_timestamp,strict_monotonic_timestamp,
     arity,return_to,silent,running_procs,running_ports,exiting].

display_info([Node|Nodes],Modifier) ->
    io:format("~nNode ~w:~n",[Node]),
    io:format("~-12s ~-21s Trace ~n", ["Pid", "Initial call"]),
    List = rpc:call(Node,?MODULE,get_info,[]),
    display_info1(List,Modifier),
    display_info(Nodes,Modifier);
display_info([],_) ->
    ok.

display_info1([{Pid,Call,Flags}|T],Modifier) ->
    io:format("~-12s ~-21"++Modifier++"s ~s~n",
	      [io_lib:format("~w",[Pid]),
	       io_lib:format("~"++Modifier++"p", [Call]),
	       format_trace(Flags)]),
    display_info1(T,Modifier);
display_info1([],_) ->
    ok.

get_info() ->
    get_info(processes(),get_info(erlang:ports(),[])).

get_info([Port|T], Acc) when is_port(Port) ->
    case pinfo(Port, name) of
        undefined ->
            get_info(T,Acc);
        {name, Name} ->
            get_info(T,get_tinfo(Port, Name, Acc))
    end;
get_info([Pid|T],Acc) ->
    case pinfo(Pid, initial_call) of
        undefined ->
            get_info(T,Acc);
        {initial_call, Call} ->
            get_info(T,get_tinfo(Pid, Call, Acc))
    end;
get_info([],Acc) -> Acc.

get_tinfo(P, Id, Acc) ->
    case tinfo(P, flags) of
        undefined ->
            Acc;
		{flags,[]} ->
            Acc;
        {flags,Flags} ->
            [{P,Id,Flags}|Acc]
    end.

format_trace([]) -> [];
format_trace([Item]) -> [ts(Item)];
format_trace([Item|T]) -> [ts(Item) ," | ", format_trace(T)].

ts(send) -> "s";
ts('receive') -> "r";
ts(call) -> "c";
ts(procs) -> "p";
ts(set_on_spawn) -> "sos";
ts(set_on_first_spawn) -> "sofs";
ts(set_on_link) -> "sol";
ts(set_on_first_link) -> "sofl";
ts(Other) -> atom_to_list(Other).

%%
%% Turn (pid or) atom into a PidSpec for erlang:trace,
%% return {badpid,X} on failure 
%%

to_pidspec(X) when is_pid(X) -> 
    case erlang:is_process_alive(X) of
	true -> X;
	false -> {badpid,X}
    end;
to_pidspec(X) when is_port(X) ->
    case erlang:port_info(X) of
        undefined -> {badport, X};
        _ -> X
    end;
to_pidspec(Tag)
  when Tag =:= all;
       Tag =:= ports;
       Tag =:= processes;
       Tag =:= new;
       Tag =:= new_ports;
       Tag =:= new_processes;
       Tag =:= existing;
       Tag =:= existing_ports;
       Tag =:= existing_processes ->
    Tag;
to_pidspec(X) when is_atom(X) ->
    case whereis(X) of
	undefined -> {badpid,X};
	Pid -> Pid
    end;
to_pidspec(X) -> {badpid,X}.

%%
%% Turn (pid or) integer or tuple or list into pid
%%

to_pid(X) when is_pid(X) -> X;
to_pid(X) when is_port(X) -> X;
to_pid(X) when is_integer(X) -> to_pid({0,X,0});
to_pid({X,Y,Z}) ->
    to_pid(lists:concat(["<",integer_to_list(X),".",
			 integer_to_list(Y),".",
			 integer_to_list(Z),">"]));
to_pid(X) when is_list(X) ->
    try list_to_pid(X) of
	Pid -> Pid
    catch
	error:badarg -> {badpid,X}
    end;
to_pid(X) -> {badpid,X}.


pinfo(P, X) when node(P) == node(), is_port(P) -> erlang:port_info(P, X);
pinfo(P, X) when node(P) == node() -> erlang:process_info(P, X);
pinfo(P, X) when is_port(P) -> check(rpc:call(node(P), erlang, port_info, [P, X]));
pinfo(P, X) -> check(rpc:call(node(P), erlang, process_info, [P, X])).


tinfo(P, X) when node(P) == node() -> erlang:trace_info(P, X);
tinfo(P, X) -> check(rpc:call(node(P), erlang, trace_info, [P, X])).

check({badrpc, _}) -> undefined;
check(X) -> X.

%% Process loop that processes a trace. Reads the trace with 
%% the reader Reader, and feeds the trace terms 
%% to handler Handler, keeping a state variable for the 
%% handler.
%%
%% Exits 'normal' at end of trace, other exits due to errors.
%%
%% Reader is a lazy list, i.e either a list or a fun/0. 
%% If it is a fun, it is evaluated for rest of the lazy list.
%% A list head is considered to be a trace term. End of list 
%% is interpreted as end of trace.

tc_loop([Term|Tail], Handler, HData0) ->
    HData = Handler(Term, HData0),
    tc_loop(Tail, Handler, HData);
tc_loop([], Handler, HData) ->
    Handler(end_of_trace, HData),
    exit(normal);
tc_loop(Reader, Handler, HData) when is_function(Reader) ->
    tc_loop(Reader(), Handler, HData);
tc_loop(Other, _Handler, _HData) ->
    Modifier = modifier(),
    io:format("~p:tc_loop ~"++Modifier++"p~n", [?MODULE, Other]),
    exit({unknown_term_from_reader, Other}).



%% Returns a reader (lazy list of trace terms) for tc_loop/2.
gen_reader(ip, {Host, Portno}) ->
    case gen_tcp:connect(Host, Portno, [{active, false}, binary]) of
        {ok, Sock} ->    
	    %% Just in case this is on the traced node,
	    %% make sure the port is not traced.
	    p(Sock,clear),
	    mk_reader(fun ip_read/2, Sock);
	Error ->
	    exit(Error)
    end;
gen_reader(file, {Filename, wrap, Tail, _, WrapCnt}) ->
    mk_reader_wrap(wrap_sort(wrap_presort(Filename, Tail), WrapCnt));
gen_reader(file, Filename) ->
    gen_reader_file(fun file_read/2, Filename);
gen_reader(follow_file, Filename) ->
    gen_reader_file(fun follow_read/2, Filename).

%% Opens a file and returns a reader (lazy list).
gen_reader_file(ReadFun, Filename) ->
    case file:open(Filename, [read, raw, binary, read_ahead]) of
	{ok, File} ->
	    mk_reader(ReadFun, File);
	Error ->
	    exit({client_cannot_open, Error})
    end.

-dialyzer({no_improper_lists, mk_reader/2}).

%% Creates and returns a reader (lazy list).
mk_reader(ReadFun, Source) ->
    fun() ->
	    case read_term(ReadFun, Source) of
		{ok, Term} ->
		    [Term | mk_reader(ReadFun, Source)];
		eof ->
		    [] % end_of_trace
	    end
    end.

%% Creates and returns a reader (lazy list) for a wrap log.
%% The argument is a sorted list of sort converted 
%% wrap log file names, see wrap_presort/2.

mk_reader_wrap([]) ->
    [];
mk_reader_wrap([Hd | _] = WrapFiles) ->
    case file:open(wrap_name(Hd), [read, raw, binary, read_ahead]) of
	{ok, File} ->
	    mk_reader_wrap(WrapFiles, File);
	Error ->
	    exit({client_cannot_open, Error})
    end.

-dialyzer({no_improper_lists, mk_reader_wrap/2}).

mk_reader_wrap([_Hd | Tail] = WrapFiles, File) ->
    fun() ->
	    case read_term(fun file_read/2, File) of
		{ok, Term} ->
		    [Term | mk_reader_wrap(WrapFiles, File)];
		eof ->
		    ok = file:close(File),
		    case Tail of
			[_|_] ->
			    mk_reader_wrap(Tail);
			[] ->
			    [] % end_of_trace
		    end
	    end
    end.



%% Generic read term function. 
%% Returns {ok, Term} | 'eof'. Exits on errors.

read_term(ReadFun, Source) ->
    case ReadFun(Source, 5) of
	Bin when is_binary(Bin) ->
	    read_term(ReadFun, Source, Bin);
	List when is_list(List) ->
	    read_term(ReadFun, Source, list_to_binary(List));
	eof ->
	    eof
    end.

read_term(ReadFun, Source, <<Op, Size:32>> = Tag) ->
    case Op of
	0 ->
	    case ReadFun(Source, Size) of
		eof ->
		    exit({'trace term missing', 
			  binary_to_list(Tag)});
		Bin when is_binary(Bin) ->
		    {ok, binary_to_term(Bin)};
		List when is_list(List) ->
		    {ok, binary_to_term(list_to_binary(List))}
	    end;
	1 ->
	    {ok, {drop, Size}};
	Junk ->
	    exit({'bad trace tag', Junk})
    end.
    


%% Read functions for different source types, for read_term/2.
%%
%% Returns a binary of length N, an I/O-list of 
%% effective length N or 'eof'. Exits on errors.

file_read(File, N) ->
    case file:read(File, N) of
	{ok, Bin} when byte_size(Bin) =:= N -> 
	    Bin;
	{ok, Bin} when is_binary(Bin) ->
	    exit({'truncated file', binary_to_list(Bin)});
	eof ->
	    eof;
	{error, Reason} ->
	    exit({'file read error', Reason})
    end.

follow_read(File, N) ->
    follow_read(File, N, cur).

follow_read(File, N, Pos) ->
    case file:position(File, Pos) of
	{ok, Offset} ->
	    case file:read(File, N) of
		{ok, Bin} when byte_size(Bin) =:= N -> 
		    Bin;
		{ok, Bin} when is_binary(Bin) ->
		    follow_read(File, N, Offset);
		eof ->
		    follow_read(File, N, Offset);
		{error, Reason} ->
		    exit({'file read error', Reason})
	    end;
	{error, Reason} ->
	    exit({'file position error', Reason})
    end.

ip_read(Socket, N) ->
    case gen_tcp:recv(Socket, N) of
	{ok, Bin} when byte_size(Bin) < N ->
	    [Bin | ip_read(Socket, N-byte_size(Bin))];
	{ok, Bin} when byte_size(Bin) == N ->
	    [Bin];
	{ok, Bin} when is_binary(Bin) ->
	    exit({'socket read too much data', Bin});
	{error, closed} ->
	    eof;
	{error, _Reason} = Error ->
	    exit({'socket read error', Error})
    end.

get_tracer() ->
    req({get_tracer,node()}).
get_tracer(Node) ->
    req({get_tracer,Node}).
get_tracer_flag() ->
    {ok, Tracer} = get_tracer(),
    get_tracer_flag(Tracer).
get_tracer_flag({Module,State}) ->
    {tracer, Module, State};
get_tracer_flag(Port = Pid) when is_port(Port); is_pid(Pid)->
    {tracer, Pid = Port}.

save_pattern([]) ->
    0;
save_pattern(P) ->
    (catch save_pattern(P, get_pattern_table())).

save_pattern(Pattern, PT) ->
    Last = last_pattern(ets:last(PT), PT),
    BPattern = term_to_binary(Pattern),
    case ets:match_object(PT, {'_', BPattern}) of
	[] ->
	    ets:insert(PT, {Last + 1, BPattern}),
	    Last + 1;
	[{N, BPattern}] ->
	    N
    end.

last_pattern('$end_of_table', _PT) ->
    0;
last_pattern(I, PT) when is_atom(I) ->
    last_pattern(ets:prev(PT, I), PT);
last_pattern(I, _PT) when is_integer(I) ->
    I;
last_pattern(_, _) ->
    throw({error, badtable}).


get_pattern_table() ->
    {ok, Ret} = req(get_table),
    Ret.

new_pattern_table() ->
    PT = ets:new(dbg_tab, [ordered_set, public]),
    ets:insert(PT, 
	       {x, 
		term_to_binary([{'_',[],[{exception_trace}]}])}),
    ets:insert(PT, 
	       {exception_trace, 
		term_to_binary(x)}),
    ets:insert(PT,
	       {c,
		term_to_binary([{'_',[],[{message,{caller}}]}])}),
    ets:insert(PT,
	       {caller_trace,
		term_to_binary(c)}),
    ets:insert(PT,
	       {cx,
		term_to_binary([{'_',[],[{exception_trace},
					 {message,{caller}}]}])}),
    ets:insert(PT,
	       {caller_exception_trace,
		term_to_binary(cx)}),
    PT.


pt_doforall(Fun, Ld) ->
    T = get_pattern_table(),
    pt_doforall(T, Fun, ets:first(T), Ld).

pt_doforall(_, _, '$end_of_table', _Ld) -> 
    ok;
pt_doforall(T, Fun, Key, Ld) ->
    [{A,B}] = ets:lookup(T,Key),
    NLd = Fun({A,binary_to_term(B)},Ld),
    pt_doforall(T,Fun,ets:next(T,Key),NLd).

lint_tp([]) ->
    {ok,[]};
lint_tp(Pattern) ->
    case erlang:match_spec_test([],Pattern,trace) of
	{ok,_Res,Warnings,_Flags} ->
	    {ok, Warnings};
	{error, Reasons} ->
	    {error, Reasons}
    end.

check_list(T) ->
    case (catch lists:foldl(
		  fun(Val,_) ->
			  {ok,_,_,_} = 
			      erlang:match_spec_test([],Val,trace),
			  ok
		  end,
		  ok, T)) of
	{'EXIT',_} ->
	    {error, bad_match_spec};
	ok ->
	    ok;
	_Else ->
	    {error, badfile}
    end.



%% Find all possible wrap log files.
%% Returns: a list of sort converted filenames.
%%
%% The sort conversion is done by extracting the wrap sequence counter
%% from the filename, and calling wrap_encode/2.
wrap_presort(Filename, Tail) ->
    Name = filename:basename(Filename),
    Dirname = filename:dirname(Filename),
    case file:list_dir(Dirname) of
	{ok, Files} ->
	    lists:zf(
	      fun(N) ->
		      case match_front(N, Name) of
			  false ->
			      false;
			  X ->
			      case match_rear(X, Tail) of
				  false ->
				      false;
				  C -> % Counter
				      case match_0_9(C) of
					  true ->
					      {true, 
					       wrap_encode(
						 filename:join(Dirname, N),
						 C)};
					  false ->
					      false
				      end
			      end
		      end
	      end,
	      Files);
	_ ->
	    []
    end.



%% Sorts a list of sort converted files
wrap_sort(Files, N) ->
    wrap_sortfix(lists:sort(Files), N).

%% Finish the sorting, since the lists:sort order is not the correct order.
%% Cut the list of files at the gap (at least one file is supposed
%% to be 'missing') and create a new list by cons'ing the two parts
%% in the right order.
wrap_sortfix([], N) when N >= 1 ->
    [];
wrap_sortfix([], _N) ->
    exit(inconsistent_wrap_file_trace_set);
%% file 0, gap 1..N
wrap_sortfix([{0, _}] = Files, N) when N >= 1 ->
    Files;
wrap_sortfix([{0, _}], _N) ->
    exit(inconsistent_wrap_file_trace_set);
%% files 0, ...
wrap_sortfix([{0, _} | _] = Files, N) when N >= 1->
    wrap_sortfix_1(Files, N, [], Files);
%% gap 0, files 1, ...
wrap_sortfix([{1, _} | _] = Files, N) when N >= 1 ->
    wrap_sortfix_2(Files, N, [], Files);
wrap_sortfix([{_C, _} | _], _N) ->
    exit(inconsistent_wrap_file_trace_set).

%% files 0..C, gap C+1..N
wrap_sortfix_1([{C, _}], N, _R, Files) 
  when C < N ->
    Files;
%% files 0..C1, C1+1==C2, ...
wrap_sortfix_1([{C1, _} = F1 | [{C2, _} | _] = Tail], N, R, Files) 
  when C1+1 == C2, C2 < N ->
    wrap_sortfix_1(Tail, N, [F1 | R], Files);
%% files 0..C1, gap C1+1, files C1+2==C2, ...
wrap_sortfix_1([{C1, _} = F1 | [{C2, _} | _] = Tail], N, R, _Files) 
  when C1+2 == C2, C2 =< N ->
    wrap_sortfix_2(Tail, N, lists:reverse([F1 | R]), Tail);
wrap_sortfix_1([_F1 | [_F2 | _]], _N, _R, _Files) ->
    exit(inconsistent_wrap_file_trace_set).

%% M == length(R); files 0..M-1, gap M, files M+1..N
wrap_sortfix_2([{N, _}], N, R, Files) ->
    Files ++ R;
wrap_sortfix_2([{_C, _}], _N, _R, _Files) ->
    exit(inconsistent_wrap_file_trace_set);
%% M == length(R); files 0..M-1, gap M, files M+1..C1, C1+1==C2, ...
wrap_sortfix_2([{C1, _} | [{C2, _} | _] = Tail], N, R, Files)
  when C1+1 == C2, C2 =< N ->
    wrap_sortfix_2(Tail, N, R, Files);
wrap_sortfix_2([{_C1, _} | [{_C2, _} | _]], _N, _R, _Files) ->
    exit(inconsistent_wrap_file_trace_set).



%% Extract the filenames from a list of sort converted ones.
wrap_postsort(Files) ->    
    lists:map(fun wrap_name/1, Files).

wrap_encode(N, C) ->
    {list_to_integer(C), N}.

wrap_name({_C, N}) ->
    N.

%% Returns what is left of ListA when removing all matching
%% elements from ListB, or false if some element did not match,
%% or if ListA runs out of elements before ListB.
match_front(ListA, []) when is_list(ListA) ->
    ListA;
match_front([], ListB) when is_list(ListB) ->
    false;
match_front([Hd|TlA], [Hd|TlB]) ->
    match_front(TlA,TlB);
match_front([_HdA|_], [_HdB|_]) ->
    false.

%% Reversed version of match_front/2
match_rear(ListA, ListB) when is_list(ListA), is_list(ListB) ->
    case match_front(lists:reverse(ListA), lists:reverse(ListB)) of
	false ->
	    false;
	List ->
	    lists:reverse(List)
    end.

%% Returns true if the non-empty list arguments contains all
%% characters $0 .. $9.
match_0_9([]) ->
    false;
match_0_9([H]) when is_integer(H), $0 =< H, H =< $9 ->
    true;
match_0_9([H|T]) when is_integer(H), $0 =< H, H =< $9 ->
    match_0_9(T);
match_0_9(L) when is_list(L) ->
    false.

%%%%%%%%%%%%%%%%%%
%% Help...
%%%%%%%%%%%%%%%%%%

help_display([]) ->
    io:format("~n",[]),
    ok;
help_display([H|T]) ->
    io:format("~s~n",[H]),
    help_display(T).

h() ->
    help_display(
      [
       "The following help items are available:",
       "   p, c",
       "       - Set trace flags for processes",
       "   tp, tpl, ctp, ctpl, ctpg, ltp, dtp, wtp, rtp",
       "       - Manipulate trace patterns for functions",
       "   n, cn, ln",
       "       - Add/remove traced nodes.",
       "   tracer, trace_port, trace_client, get_tracer, stop, stop_clear", 
       "       - Manipulate tracer process/port",
       "   i",
       "       - Info", 
       "",
       "call dbg:h(Item) for brief help a brief description",
       "of one of the items above."]).
h(p) ->
    help_display(["p(Item) -> {ok, MatchDesc} | {error, term()}",
		  " - Traces messages to and from Item.",
		  "p(Item, Flags) -> {ok, MatchDesc} | {error, term()}",
		  " - Traces Item according to Flags.",
		  "   Flags can be one of s,r,m,c,p,sos,sol,sofs,",
		  "   sofl,all,clear or any flag accepted by erlang:trace/3"]);
h(c) ->
    help_display(["c(Mod, Fun, Args)",
		  " - Evaluates apply(M,F,Args) with all trace flags set.",
		  "c(Mod, Fun, Args, Flags)",
		  " - Evaluates apply(M,F,Args) with Flags trace flags set."]);
h(i) ->
    help_display(["i() -> ok",
		  " - Displays information about all traced processes."]);
h(tp) ->
    help_display(
      ["tp(Module,MatchSpec)",
       " - Same as tp({Module, '_', '_'}, MatchSpec)",
       "tp(Module,Function,MatchSpec)",
       " - Same as tp({Module, Function, '_'}, MatchSpec)",
       "tp(Module, Function, Arity, MatchSpec)",
       " - Same as tp({Module, Function, Arity}, MatchSpec)",
       "tp({Module, Function, Arity}, MatchSpec) -> {ok, MatchDesc} "
       "| {error, term()}",
       " - Set pattern for traced global function calls."]);
h(tpl) ->
    help_display(
      ["tpl(Module,MatchSpec)",
       " - Same as tpl({Module, '_', '_'}, MatchSpec)",
       "tpl(Module,Function,MatchSpec)",
       " - Same as tpl({Module, Function, '_'}, MatchSpec)",
       "tpl(Module, Function, Arity, MatchSpec)",
       " - Same as tpl({Module, Function, Arity}, MatchSpec)",
       "tpl({Module, Function, Arity}, MatchSpec) -> {ok, MatchDesc} "
       "| {error, term()}",
       " - Set pattern for traced local (as well as global) function calls."]);
h(ctp) ->
    help_display(
      ["ctp()",
       " - Same as ctp({'_', '_', '_'})",
       "ctp(Module)",
       " - Same as ctp({Module, '_', '_'})",
       "ctp(Module, Function)",
       " - Same as ctp({Module, Function, '_'})",
       "ctp(Module, Function, Arity)",
       " - Same as ctp({Module, Function, Arity})",
       "ctp({Module, Function, Arity}) -> {ok, MatchDesc} | {error, term()}",
       " - Clear call trace pattern for the specified functions"]);
h(ctpl) ->
    help_display(
      ["ctpl()",
       " - Same as ctpl({'_', '_', '_'})",
       "ctpl(Module)",
       " - Same as ctpl({Module, '_', '_'})",
       "ctpl(Module, Function)",
       " - Same as ctpl({Module, Function, '_'})",
       "ctpl(Module, Function, Arity)",
       " - Same as ctpl({Module, Function, Arity})",
       "ctpl({Module, Function, Arity}) -> {ok, MatchDesc} | {error, term()}",
       " - Clear local call trace pattern for the specified functions"]);
h(ctpg) ->
    help_display(
      ["ctpg()",
       " - Same as ctpg({'_', '_', '_'})",
       "ctpg(Module)",
       " - Same as ctpg({Module, '_', '_'})",
       "ctpg(Module, Function)",
       " - Same as ctpg({Module, Function, '_'})",
       "ctpg(Module, Function, Arity)",
       " - Same as ctpg({Module, Function, Arity})",
       "ctpg({Module, Function, Arity}) -> {ok, MatchDesc} | {error, term()}",
       " - Clear global call trace pattern for the specified functions"]);
h(ltp) ->
    help_display(["ltp() -> ok",
		  " - Lists saved and built-in match_spec's on the console."]);
h(dtp) ->
    help_display(["dtp() -> ok",
		  " - Deletes all saved match_spec's.",
		  "dtp(N) -> ok",
		  " - Deletes a specific saved match_spec."]);
h(wtp) ->
    help_display(["wtp(Name) -> ok | {error, IOError}",
		  " - Writes all saved match_spec's to a file"]);
h(rtp) ->
    help_display(["rtp(Name) -> ok | {error, Error}",
		  " - Read saved match specifications from file."]);
h(n) ->
    help_display(
      ["n(Nodename) -> {ok, Nodename} | {error, Reason}",
       " - Starts a tracer server on the given node.",
       "n(Nodename,Type,Data) -> {ok, Nodename} | {error, Reason}",
       " - Starts a tracer server with additional args on the given node."]);
h(cn) ->
    help_display(["cn(Nodename) -> ok",
		  " - Clears a node from the list of traced nodes."]);
h(ln) ->
    help_display(["ln() -> ok",
		  " - Shows the list of traced nodes on the console."]);
h(tracer) ->
    help_display(["tracer() -> {ok, pid()} | {error, already_started}",
		  " - Starts a tracer server that handles trace messages.",
		  "tracer(Type, Data) -> {ok, pid()} | {error, Error}",
		  " - Starts a tracer server with additional parameters"]);
h(trace_port) ->
    help_display(["trace_port(Type, Parameters) -> fun()",
		  " - Creates and returns a trace port generating fun"]);
h(trace_client) ->
    help_display(["trace_client(Type, Parameters) -> pid()",
		  " - Starts a trace client that reads messages created by "
		  "a trace port driver",
		  "trace_client(Type, Parameters, HandlerSpec) -> pid()",
		  " - Starts a trace client that reads messages created by a",
		  "   trace port driver, with a user defined handler"]);
h(get_tracer) ->
    help_display(
      ["get_tracer() -> {ok, Tracer}",
       " - Returns the process or port to which all trace messages are sent.",
      "get_tracer(Node) -> {ok, Tracer}",
       " - Returns the process or port to which all trace messages are sent."]);
h(stop) ->
    help_display(
      ["stop() -> ok",
       " - Stops the dbg server and the tracing of all processes.",
       "   Does not clear any trace patterns."]);
h(stop_clear) ->
    help_display(
      ["stop_clear() -> ok",
       " - Stops the dbg server and the tracing of all processes,",
       "   and clears all trace patterns."]).

