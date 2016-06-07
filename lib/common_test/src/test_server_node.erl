%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2002-2016. All Rights Reserved.
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
-module(test_server_node).
-compile(r12).

%%%
%%% The same compiled code for this module must be possible to load
%%% in R12B and later.
%%%

%% Test Controller interface
-export([is_release_available/1]).
-export([start_tracer_node/2,trace_nodes/2,stop_tracer_node/1]).
-export([start_node/5, stop_node/1]).
-export([kill_nodes/0, nodedown/1]).
%% Internal export
-export([node_started/1,trc/1,handle_debug/4]).

-include("test_server_internal.hrl").
-record(slave_info, {name,socket,client}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                  %%%
%%% All code in this module executes on the test_server_ctrl process %%%
%%% except for node_started/1 and trc/1 which execute on a new node. %%%
%%%                                                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_release_available(Rel) when is_atom(Rel) ->
    is_release_available(atom_to_list(Rel));
is_release_available(Rel) ->
    case os:type() of
	{unix,_} ->
	    Erl = find_release(Rel),
	    case Erl of
		none -> false;
		_ -> filelib:is_regular(Erl)
	    end;
	_ ->
	    false
    end.

nodedown(Sock) ->
    Match = #slave_info{name='$1',socket=Sock,client='$2',_='_'},
    case ets:match(slave_tab,Match) of
	[[Node,_Client]] -> % Slave node died
	    gen_tcp:close(Sock),
	    ets:delete(slave_tab,Node),
	    slave_died;
	[] ->
	    ok
    end.





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Start trace node
%%%
start_tracer_node(TraceFile,TI) ->
    Match = #slave_info{name='$1',_='_'},
    SlaveNodes = lists:map(fun([N]) -> [" ",N] end,
			   ets:match(slave_tab,Match)),
    TargetNode = node(),
    Cookie = TI#target_info.cookie,
    {ok,LSock} = gen_tcp:listen(0,[binary,{reuseaddr,true},{packet,2}]),
    {ok,TracePort} = inet:port(LSock),
    Prog = quote_progname(pick_erl_program(default)),
    Cmd = lists:concat([Prog, " -sname tracer -hidden -setcookie ", Cookie, 
			" -s ", ?MODULE, " trc ", TraceFile, " ", 
			TracePort, " ", TI#target_info.os_family]),
    spawn(fun() -> print_data(open_port({spawn,Cmd},[stream])) end),
%!    open_port({spawn,Cmd},[stream]),
    case gen_tcp:accept(LSock,?ACCEPT_TIMEOUT) of
	{ok,Sock} -> 
	    gen_tcp:close(LSock),
	    receive 
		{tcp,Sock,Result} when is_binary(Result) ->
		    case unpack(Result) of
			error ->
			    gen_tcp:close(Sock),
			    {error,timeout};
			{ok,started} ->
			    trace_nodes(Sock,[TargetNode | SlaveNodes]),
			    {ok,Sock};
			{ok,Error} -> Error
		    end;
		{tcp_closed,Sock} ->
		    gen_tcp:close(Sock),
		    {error,could_not_start_tracernode}
	    after ?ACCEPT_TIMEOUT ->
		    gen_tcp:close(Sock),
		    {error,timeout}
	    end;
	Error -> 
	    gen_tcp:close(LSock),
	    {error,{could_not_start_tracernode,Error}}
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Start a tracer on each of these nodes and set flags and patterns
%%%
trace_nodes(Sock,Nodes) ->
    Bin = term_to_binary({add_nodes,Nodes}),
    ok = gen_tcp:send(Sock, tag_trace_message(Bin)),
    receive_ack(Sock).


receive_ack(Sock) ->
    receive
	{tcp,Sock,Bin} when is_binary(Bin) ->
	    case unpack(Bin) of
		error -> receive_ack(Sock);
		{ok,_} -> ok
	    end;
	_ ->
	    receive_ack(Sock)
    end.
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Stop trace node
%%%
stop_tracer_node(Sock) ->
    Bin = term_to_binary(id(stop)),
    ok = gen_tcp:send(Sock, tag_trace_message(Bin)),
    receive {tcp_closed,Sock} -> gen_tcp:close(Sock) end,
    ok.
    



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% trc([TraceFile,Nodes]) -> ok
%%
%% Start tracing on the given nodes
%%
%% This function executes on the new node
%%
trc([TraceFile, PortAtom, Type]) ->
    {Result,Patterns} = 
	case file:consult(TraceFile) of
	    {ok,TI} ->
		Pat = parse_trace_info(lists:flatten(TI)),
		{started,Pat};
	    Error ->
		{Error,[]}
	end,
    Port = list_to_integer(atom_to_list(PortAtom)),
    case catch gen_tcp:connect("localhost", Port, [binary, 
						   {reuseaddr,true}, 
						   {packet,2}]) of
	{ok,Sock} -> 
	    BinResult = term_to_binary(Result),
	    ok = gen_tcp:send(Sock,tag_trace_message(BinResult)),
	    trc_loop(Sock,Patterns,Type);
	_else ->
	    ok
    end,
    erlang:halt().
trc_loop(Sock,Patterns,Type) ->
    receive
	{tcp,Sock,Bin} ->
	    case unpack(Bin) of
		error ->
		    ttb:stop(),
		    gen_tcp:close(Sock);
		{ok,{add_nodes,Nodes}} -> 
		    add_nodes(Nodes,Patterns,Type),
		    Bin = term_to_binary(id(ok)),
		    ok = gen_tcp:send(Sock, tag_trace_message(Bin)),
		    trc_loop(Sock,Patterns,Type);
		{ok,stop} -> 
		    ttb:stop(),
		    gen_tcp:close(Sock)
	    end;
	{tcp_closed,Sock} ->
	    ttb:stop(),
	    gen_tcp:close(Sock)
    end.
add_nodes(Nodes,Patterns,_Type) ->
    {ok, _} = ttb:tracer(Nodes,[{file,{local, test_server}},
			        {handler, {{?MODULE,handle_debug},initial}}]),
    {ok, _} = ttb:p(all,[call,timestamp]),
    lists:foreach(fun({TP,M,F,A,Pat}) -> ttb:TP(M,F,A,Pat);
		     ({CTP,M,F,A}) -> ttb:CTP(M,F,A) 
		  end,
		  Patterns).

parse_trace_info([{TP,M,Pat}|Pats]) when TP=:=tp; TP=:=tpl ->
    [{TP,M,'_','_',Pat}|parse_trace_info(Pats)];
parse_trace_info([{TP,M,F,Pat}|Pats]) when TP=:=tp; TP=:=tpl ->
    [{TP,M,F,'_',Pat}|parse_trace_info(Pats)];
parse_trace_info([{TP,M,F,A,Pat}|Pats]) when TP=:=tp; TP=:=tpl ->
    [{TP,M,F,A,Pat}|parse_trace_info(Pats)];
parse_trace_info([CTP|Pats]) when CTP=:=ctp; CTP=:=ctpl; CTP=:=ctpg ->
    [{CTP,'_','_','_'}|parse_trace_info(Pats)];
parse_trace_info([{CTP,M}|Pats]) when CTP=:=ctp; CTP=:=ctpl; CTP=:=ctpg ->
    [{CTP,M,'_','_'}|parse_trace_info(Pats)];
parse_trace_info([{CTP,M,F}|Pats]) when CTP=:=ctp; CTP=:=ctpl; CTP=:=ctpg ->
    [{CTP,M,F,'_'}|parse_trace_info(Pats)];
parse_trace_info([{CTP,M,F,A}|Pats]) when CTP=:=ctp; CTP=:=ctpl; CTP=:=ctpg ->
    [{CTP,M,F,A}|parse_trace_info(Pats)];
parse_trace_info([]) ->
    [];
parse_trace_info([_other|Pats]) -> % ignore
    parse_trace_info(Pats).

handle_debug(Out,Trace,TI,initial) ->
    handle_debug(Out,Trace,TI,0);
handle_debug(_Out,end_of_trace,_TI,N) ->
    N;
handle_debug(Out,Trace,_TI,N) ->
    print_trc(Out,Trace,N),
    N+1.

print_trc(Out,{trace_ts,P,call,{M,F,A},C,Ts},N) ->
    io:format(Out,
	      "~w: ~s~n"
	      "Process   : ~w~n"
	      "Call      : ~w:~w/~w~n"
	      "Arguments : ~p~n"
	      "Caller    : ~w~n~n",
	      [N,ts(Ts),P,M,F,length(A),A,C]);
print_trc(Out,{trace_ts,P,call,{M,F,A},Ts},N) ->
    io:format(Out,
	      "~w: ~s~n"
	      "Process   : ~w~n"
	      "Call      : ~w:~w/~w~n"
	      "Arguments : ~p~n~n",
	      [N,ts(Ts),P,M,F,length(A),A]);
print_trc(Out,{trace_ts,P,return_from,{M,F,A},R,Ts},N) ->
    io:format(Out,
	      "~w: ~s~n"
	      "Process      : ~w~n"
	      "Return from  : ~w:~w/~w~n"
	      "Return value : ~p~n~n",
	      [N,ts(Ts),P,M,F,A,R]);
print_trc(Out,{drop,X},N) ->
    io:format(Out,
	      "~w: Tracer dropped ~w messages - too busy~n~n",
	      [N,X]);
print_trc(Out,Trace,N) ->
    Ts = element(size(Trace),Trace),
    io:format(Out,
	      "~w: ~s~n"
	      "Trace        : ~p~n~n",
	      [N,ts(Ts),Trace]).
ts({_, _, Micro} = Now) ->
    {{Y,M,D},{H,Min,S}} = calendar:now_to_local_time(Now),
    io_lib:format("~4.4.0w-~2.2.0w-~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w,~6.6.0w",
		  [Y,M,D,H,Min,S,Micro]).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Start slave/peer nodes (initiated by test_server:start_node/5)
%%%
start_node(SlaveName, slave, Options, From, TI) when is_list(SlaveName) ->
    start_node_slave(list_to_atom(SlaveName), Options, From, TI);
start_node(SlaveName, slave, Options, From, TI) ->
    start_node_slave(SlaveName, Options, From, TI);
start_node(SlaveName, peer, Options, From, TI) when is_atom(SlaveName) ->
    start_node_peer(atom_to_list(SlaveName), Options, From, TI);
start_node(SlaveName, peer, Options, From, TI) ->
    start_node_peer(SlaveName, Options, From, TI);
start_node(_SlaveName, _Type, _Options, _From, _TI) ->
    not_implemented_yet.

%%
%% Peer nodes are always started on the same host as test_server_ctrl
%%
%% (Socket communication is used since in early days the test target
%% and the test server controller node could be on different hosts and
%% the target could not know the controller node via erlang
%% distribution)
%%
start_node_peer(SlaveName, OptList, From, TI) ->
    SuppliedArgs = start_node_get_option_value(args, OptList, []),
    Cleanup = start_node_get_option_value(cleanup, OptList, true),
    HostStr = test_server_sup:hoststr(),
    {ok,LSock} = gen_tcp:listen(0,[binary,
				   {reuseaddr,true},
				   {packet,2}]),
    {ok,WaitPort} = inet:port(LSock),
    NodeStarted = lists:concat([" -s ", ?MODULE, " node_started ",
				      HostStr, " ", WaitPort]),

    % Support for erl_crash_dump files..
    CrashDir = test_server_sup:crash_dump_dir(),
    CrashFile = filename:join([CrashDir,
			       "erl_crash_dump."++cast_to_list(SlaveName)]),
    CrashArgs = lists:concat([" -env ERL_CRASH_DUMP \"",CrashFile,"\" "]),
    FailOnError = start_node_get_option_value(fail_on_error, OptList, true),
    Prog0 = start_node_get_option_value(erl, OptList, default),
    Prog = quote_progname(pick_erl_program(Prog0)),
    Args = 
	case string:str(SuppliedArgs,"-setcookie") of
	    0 -> "-setcookie " ++ TI#target_info.cookie ++ " " ++ SuppliedArgs;
	    _ -> SuppliedArgs
	end,
    Cmd = lists:concat([Prog,
			" -detached ",
			TI#target_info.naming, " ", SlaveName,
			NodeStarted,
			CrashArgs,
			" ", Args]),
    Opts = case start_node_get_option_value(env, OptList, []) of
	       [] -> [];
	       Env -> [{env, Env}]
	   end,
    %% peer is always started on localhost
    %%
    %% Bad environment can cause open port to fail. If this happens,
    %% we ignore it and let the testcase handle the situation...
    catch open_port({spawn, Cmd}, [stream|Opts]),

    Tmo = 60000 * test_server:timetrap_scale_factor(),
    
    case start_node_get_option_value(wait, OptList, true) of
	true ->
	    Ret = wait_for_node_started(LSock,Tmo,undefined,Cleanup,TI,self()),
	    case {Ret,FailOnError} of
		{{{ok, Node}, Warning},_} ->
		    gen_server:reply(From,{{ok,Node},HostStr,Cmd,[],Warning});
		{_,false} ->
		    gen_server:reply(From,{Ret, HostStr, Cmd});
		{_,true} ->
		    gen_server:reply(From,{fail,{Ret, HostStr, Cmd}})
	    end;
	false ->
	    Nodename = list_to_atom(SlaveName ++ "@" ++ HostStr),
	    I = "=== Not waiting for node",
	    gen_server:reply(From,{{ok, Nodename}, HostStr, Cmd, I, []}),
	    Self = self(),
	    spawn_link(wait_for_node_started_fun(LSock,Tmo,Cleanup,TI,Self)),
	    ok
    end.

-spec wait_for_node_started_fun(_, _, _, _, _) -> fun(() -> no_return()).
wait_for_node_started_fun(LSock, Tmo, Cleanup, TI, Self) ->
    fun() ->
            {{ok, _}, _} = wait_for_node_started(LSock,Tmo,undefined,
                                                 Cleanup,TI,Self),
            receive after infinity -> ok end
    end.

%%
%% Slave nodes are started on a remote host if
%% - the option remote is given when calling test_server:start_node/3
%%
start_node_slave(SlaveName, OptList, From, _TI) ->
    SuppliedArgs = start_node_get_option_value(args, OptList, []),
    Cleanup = start_node_get_option_value(cleanup, OptList, true),

    CrashDir = test_server_sup:crash_dump_dir(),
    CrashFile = filename:join([CrashDir,
			       "erl_crash_dump."++cast_to_list(SlaveName)]),
    CrashArgs = lists:concat([" -env ERL_CRASH_DUMP \"",CrashFile,"\" "]),
    Args = lists:concat([" ", SuppliedArgs, CrashArgs]),

    Prog0 = start_node_get_option_value(erl, OptList, default),
    Prog = pick_erl_program(Prog0),
    Ret = 
	case start_which_node(OptList) of
	    {error,Reason} -> {{error,Reason},undefined,undefined};
	    Host0 -> do_start_node_slave(Host0,SlaveName,Args,Prog,Cleanup)
	end,
    gen_server:reply(From,Ret).


do_start_node_slave(Host0, SlaveName, Args, Prog, Cleanup) ->
    Host =
	case Host0 of
	    local -> test_server_sup:hoststr();
	    _ -> cast_to_list(Host0)
	end,
    Cmd = Prog ++ " " ++ Args,
    case slave:start(Host, SlaveName, Args, no_link, Prog) of
	{ok,Nodename} ->
	    case Cleanup of
		true -> ets:insert(slave_tab,#slave_info{name=Nodename});
		false -> ok
	    end,
	    {{ok,Nodename}, Host, Cmd, [], []};
	Ret ->
	    {Ret, Host, Cmd}
    end.


wait_for_node_started(LSock,Timeout,Client,Cleanup,TI,CtrlPid) ->
    case gen_tcp:accept(LSock,Timeout) of
	{ok,Sock} -> 
	    gen_tcp:close(LSock),
	    receive 
		{tcp,Sock,Started0} when is_binary(Started0) ->
		    case unpack(Started0) of
			error ->
			    gen_tcp:close(Sock),
			    {error, connection_closed};
			{ok,Started} ->
			    Version = TI#target_info.otp_release,
			    VsnStr = TI#target_info.system_version,
			    {ok,Nodename, W} = 
				handle_start_node_return(Version,
							 VsnStr,
							 Started),
			    case Cleanup of
				true ->
				    ets:insert(slave_tab,#slave_info{name=Nodename,
								     socket=Sock,
								     client=Client});
				false -> ok
			    end,
			    ok = gen_tcp:controlling_process(Sock,CtrlPid),
			    test_server_ctrl:node_started(Nodename),
			    {{ok,Nodename},W}
		    end;
		{tcp_closed,Sock} ->
		    gen_tcp:close(Sock),
		    {error, connection_closed}
	    after Timeout ->
		    gen_tcp:close(Sock),
		    {error, timeout}
	    end;
	{error,Reason} -> 
	    gen_tcp:close(LSock),
	    {error, {no_connection,Reason}}
    end.



handle_start_node_return(Version,VsnStr,{started, Node, Version, VsnStr}) ->
    {ok, Node, []};
handle_start_node_return(Version,VsnStr,{started, Node, OVersion, OVsnStr}) ->
    Str = io_lib:format("WARNING: Started node "
			"reports different system "
			"version than current node! "
			"Current node version: ~p, ~p "
			"Started node version: ~p, ~p",
			[Version, VsnStr, 
			 OVersion, OVsnStr]),
    Str1 = lists:flatten(Str),
    {ok, Node, Str1}.


%%
%% This function executes on the new node
%%
node_started([Host,PortAtom]) ->
    %% Must spawn a new process because the boot process should not 
    %% hang forever!!
    spawn(node_started_fun(Host,PortAtom)).

-spec node_started_fun(_, _) -> fun(() -> no_return()).
node_started_fun(Host,PortAtom) ->
    fun() -> node_started(Host,PortAtom) end.

%% This process hangs forever, just waiting for the socket to be
%% closed and terminating the node
node_started(Host,PortAtom) ->
    {_, Version} = init:script_id(),
    VsnStr = erlang:system_info(system_version),
    Port = list_to_integer(atom_to_list(PortAtom)),
    case catch gen_tcp:connect(Host,Port, [binary, 
				     {reuseaddr,true}, 
				     {packet,2}]) of
	
	{ok,Sock} -> 
	    Started = term_to_binary({started, node(), Version, VsnStr}),
	    ok = gen_tcp:send(Sock, tag_trace_message(Started)),
	    receive _Anyting ->
		    gen_tcp:close(Sock),
		    erlang:halt()
	    end;
	_else ->
	    erlang:halt()
    end.


-compile({inline, [tag_trace_message/1]}).
-dialyzer({no_improper_lists, tag_trace_message/1}).
tag_trace_message(M) ->
    [1|M].

% start_which_node(Optlist) -> hostname
start_which_node(Optlist) ->
    case start_node_get_option_value(remote, Optlist) of
	undefined ->
	    local;
	true ->
	    case find_remote_host() of
		{error, Other} ->
		    {error, Other};
		RHost ->
		    RHost
	    end
    end.
 
find_remote_host() ->
    HostList=test_server_ctrl:get_hosts(),
    case lists:delete(test_server_sup:hoststr(), HostList) of
	[] ->
	    {error, no_remote_hosts};
	[RHost|_Rest] ->
	    RHost
    end.

start_node_get_option_value(Key, List) ->
    start_node_get_option_value(Key, List, undefined).

start_node_get_option_value(Key, List, Default) ->
    case lists:keysearch(Key, 1, List) of
	{value, {Key, Value}} ->
	    Value;
	false ->
	    Default
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% stop_node(Name) -> ok | {error,Reason}
%%
%% Clean up - test_server will stop this node
stop_node(Name) ->
    case ets:lookup(slave_tab,Name) of
	[#slave_info{}] ->
	    ets:delete(slave_tab,Name),
	    ok;
	[] -> 
	    {error, not_a_slavenode}
    end.
	    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% kill_nodes() -> ok
%%
%% Brutally kill all slavenodes that were not stopped by test_server
kill_nodes() ->
    case ets:match_object(slave_tab,'_') of
	[] -> [];
	List ->
	    lists:map(fun(SI) -> kill_node(SI) end, List)
    end.

kill_node(SI) ->
    Name = SI#slave_info.name,
    ets:delete(slave_tab,Name),
    case SI#slave_info.socket of
	undefined ->
	    catch rpc:call(Name,erlang,halt,[]);
	Sock ->
	    gen_tcp:close(Sock)
    end,
    Name.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% cast_to_list(X) -> string()
%%% X = list() | atom() | void()
%%% Returns a string representation of whatever was input

cast_to_list(X) when is_list(X) -> X;
cast_to_list(X) when is_atom(X) -> atom_to_list(X);
cast_to_list(X) -> lists:flatten(io_lib:format("~w", [X])).


%%% L contains elements of the forms
%%%  {prog, String}
%%%  {release, Rel} where Rel = String | latest | previous
%%%  this
%%%
pick_erl_program(default) ->
    cast_to_list(lib:progname());
pick_erl_program(L) ->
    P = random_element(L),
    case P of
	{prog, S} ->
	    S;
	{release, S} ->
	    find_release(S);
	this ->
	    cast_to_list(lib:progname())
    end.

%% This is an attempt to distinguish between spaces in the program
%% path and spaces that separate arguments. The program is quoted to
%% allow spaces in the path.
%%
%% Arguments could exist either if the executable is excplicitly given
%% ({prog,String}) or if the -program switch to beam is used and
%% includes arguments (typically done by cerl in OTP test environment
%% in order to ensure that slave/peer nodes are started with the same
%% emulator and flags as the test node. The return from lib:progname()
%% could then typically be '/<full_path_to>/cerl -gcov').
quote_progname(Progname) ->
    do_quote_progname(string:tokens(Progname," ")).

do_quote_progname([Prog]) ->
    "\""++Prog++"\"";
do_quote_progname([Prog,Arg|Args]) ->
    case os:find_executable(Prog) of
	false ->
	    do_quote_progname([Prog++" "++Arg | Args]);
	_ ->
	    %% this one has an executable - we assume the rest are arguments
	    "\""++Prog++"\""++
		lists:flatten(lists:map(fun(X) -> [" ",X] end, [Arg|Args]))
    end.

random_element(L) ->
    lists:nth(rand:uniform(length(L)), L).

find_release(latest) ->
    "/usr/local/otp/releases/latest/bin/erl";
find_release(previous) ->
    "kaka";
find_release(Rel) ->
    find_release(os:type(), Rel).

find_release({unix,sunos}, Rel) ->
    case os:cmd("uname -p") of
	"sparc" ++ _ ->
	    "/usr/local/otp/releases/otp_beam_solaris8_" ++ Rel ++ "/bin/erl";
	_ ->
	    none
    end;
find_release({unix,linux}, Rel) ->
    Candidates = find_rel_linux(Rel),
    case lists:dropwhile(fun(N) ->
				 not filelib:is_regular(N)
			 end, Candidates) of
	[] -> none;
	[Erl|_] -> Erl
    end;
find_release(_, _) -> none.

find_rel_linux(Rel) ->
    case suse_release() of
	none -> [];
	SuseRel -> find_rel_suse(Rel, SuseRel)
    end.

find_rel_suse(Rel, SuseRel) ->
    Root = "/usr/local/otp/releases/sles",
    case SuseRel of
	"11" ->
	    %% Try both SuSE 11, SuSE 10 and SuSe 9 in that order.
	    find_rel_suse_1(Rel, Root++"11") ++
		find_rel_suse_1(Rel, Root++"10") ++
		find_rel_suse_1(Rel, Root++"9");
	"10" ->
	    %% Try both SuSE 10 and SuSe 9 in that order.
	    find_rel_suse_1(Rel, Root++"10") ++
		find_rel_suse_1(Rel, Root++"9");
	"9" ->
	    find_rel_suse_1(Rel, Root++"9");
	_ ->
	    []
    end.

find_rel_suse_1(Rel, RootWc) ->
    case erlang:system_info(wordsize) of
	4 ->
	    find_rel_suse_2(Rel, RootWc++"_32");
	8 ->
	    find_rel_suse_2(Rel, RootWc++"_64") ++
		find_rel_suse_2(Rel, RootWc++"_32")
    end.

find_rel_suse_2(Rel, RootWc) ->
    RelDir = filename:dirname(RootWc),
    Pat = filename:basename(RootWc ++ "_" ++ Rel) ++ ".*",
    case file:list_dir(RelDir) of
	{ok,Dirs} ->
	    case lists:filter(fun(Dir) ->
				      case re:run(Dir, Pat) of
					  nomatch -> false;
					  _       -> true
				      end
			      end, Dirs) of
		[] ->
		    [];
		[R|_] ->
		    [filename:join([RelDir,R,"bin","erl"])]
	    end;
	_ ->
	    []
    end.

%% suse_release() -> VersionString | none.
%%  Return the major SuSE version number for this platform or
%%  'none' if this is not a SuSE platform.
suse_release() ->
    case file:open("/etc/SuSE-release", [read]) of
	{ok,Fd} ->
	    try
		suse_release(Fd)
	    after
		file:close(Fd)
	    end;
	{error,_} -> none
    end.

suse_release(Fd) ->
    case io:get_line(Fd, '') of
	eof -> none;
	Line when is_list(Line) ->
	    case re:run(Line, "^VERSION\\s*=\\s*(\\d+)\s*",
			[{capture,all_but_first,list}]) of
		nomatch ->
		    suse_release(Fd);
		{match,[Version]} ->
		    Version
	    end
    end.

unpack(Bin) ->
    {One,Term} = split_binary(Bin, 1),
    case binary_to_list(One) of
	[1] ->
	    case catch {ok,binary_to_term(Term)} of
		{'EXIT',_} -> error;
		{ok,_}=Res -> Res
	    end;
	_ -> error
    end.

id(I) -> I.
   
print_data(Port) ->
    receive
	{Port, {data, Bytes}} ->
	    io:put_chars(Bytes),
	    print_data(Port);
	{Port, eof} ->
	    Port ! {self(), close}, 
	    receive
		{Port, closed} ->
		    true
	    end, 
	    receive
		{'EXIT',  Port,  _} -> 
		    ok
	    after 1 ->				% force context switch
		    ok
	    end
    end.
