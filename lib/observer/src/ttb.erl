%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2002-2009. All Rights Reserved.
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
-module(ttb).
-author('siri@erix.ericsson.se').

%% API
-export([tracer/0,tracer/1,tracer/2,p/2,stop/0,stop/1]).
-export([tp/2, tp/3, tp/4, ctp/0, ctp/1, ctp/2, ctp/3, tpl/2, tpl/3, tpl/4, 
	 ctpl/0, ctpl/1, ctpl/2, ctpl/3, ctpg/0, ctpg/1, ctpg/2, ctpg/3]).
-export([seq_trigger_ms/0,seq_trigger_ms/1]).
-export([write_trace_info/2]).
-export([write_config/2,write_config/3,run_config/1,run_config/2,list_config/1]).
-export([list_history/0,run_history/1]).
-export([format/1,format/2]).

%% For debugging
-export([dump_ti/1]).

-include_lib("kernel/include/file.hrl").
-define(meta_time,5000).
-define(history_table,ttb_history_table).
-define(seq_trace_flags,[send,'receive',print,timestamp]).
-define(upload_dir,"ttb_upload").
-ifdef(debug).
-define(get_status,;get_status -> erlang:display(dict:to_list(NodeInfo)),loop(NodeInfo)).
-else.
-define(get_status,).
-endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Open a trace port on all given nodes and create the meta data file
tracer() -> tracer(node()).
tracer(Nodes) -> tracer(Nodes,[]).
tracer(Nodes,Opt) ->
    start(),
    store(tracer,[Nodes,Opt]),
    {PI,Client,Traci} = opt(Opt),
    do_tracer(Nodes,PI,Client,Traci).

do_tracer(Nodes0,PI,Client,Traci) ->
    Nodes = nods(Nodes0),
    Clients = clients(Nodes,Client),
    do_tracer(Clients,PI,Traci).

do_tracer(Clients,PI,Traci) ->
    {ClientSucc,Succ} = 
	lists:foldl(
	  fun({N,{local,File},TF},{CS,S}) -> 
		  [_Sname,Host] = string:tokens(atom_to_list(N),"@"),
		  case catch dbg:tracer(N,port,dbg:trace_port(ip,0)) of
		      {ok,N} ->
			  {ok,Port} = dbg:trace_port_control(N,get_listen_port),
			  {ok,T} = dbg:get_tracer(N),
			  rpc:call(N,seq_trace,set_system_tracer,[T]),
			  dbg:trace_client(ip,{Host,Port},
					   {fun ip_to_file/2,{file,File}}),
			  {[{N,{local,File,Port},TF}|CS], [N|S]};
		      Other ->
			  display_warning(N,{cannot_open_ip_trace_port,
					     Host,
					     Other}),
			  {CS, S}
		     end;
	     ({N,C,_}=Client,{CS,S}) -> 
		  case catch dbg:tracer(N,port,dbg:trace_port(file,C)) of
		      {ok,N} -> 
			  {ok,T} = dbg:get_tracer(N),
			  rpc:call(N,seq_trace,set_system_tracer,[T]),
			  {[Client|CS], [N|S]};
		      Other -> 
			  display_warning(N,Other),
			  {CS,S}
		  end
	  end, 
	  {[],[]}, 
	  Clients),
    case Succ of
	[] -> 
	    {ok,Succ};
	_list ->
	    write_info(ClientSucc,PI,Traci),
	    {ok,Succ}
    end.

opt(Opt) ->
    opt(Opt,{true,?MODULE,[]}).

opt([{process_info,PI}|O],{_,Client,Traci}) ->
    opt(O,{PI,Client,Traci});
opt([{file,Client}|O],{PI,_,Traci}) ->
    opt(O,{PI,Client,Traci});
opt([{handler,Handler}|O],{PI,Client,Traci}) ->
    opt(O,{PI,Client,[{handler,Handler}|Traci]});
opt([],Opt) ->
    Opt.

nods(all) ->
    Nodes1 = remove_active([node()|nodes()]),
    remove_faulty_runtime_tools_vsn(Nodes1);
nods(Node) when is_atom(Node) ->
    nods([Node]);
nods(Nodes) when is_list(Nodes) ->
    Nodes1 = remove_active(Nodes),
    Nodes2 = remove_noexist(Nodes1),
    remove_faulty_runtime_tools_vsn(Nodes2).

remove_active(Nodes) ->
    Active = get_nodes(),
    lists:filter(
      fun(N) -> case lists:member(N,Active) of
		    false -> true;
		    true -> display_warning(N,already_started), false
		end
      end, Nodes).

remove_noexist(Nodes) ->
    lists:filter(
      fun(N) when N=:=node() ->
	      true;
	 (N) ->
	      case net_adm:ping(N) of
		  pong -> true;
		  pang -> display_warning(N,no_connection), false
	      end
      end, Nodes).

remove_faulty_runtime_tools_vsn(Nodes) ->
    lists:filter(
      fun(N) ->
	      case rpc:call(N,observer_backend,vsn,[]) of
		  {ok,Vsn} -> check_vsn(N,Vsn);
		  _Error -> display_warning(N,faulty_vsn_of_runtime_tools), false
	      end
      end,Nodes).

check_vsn(_Node,_Vsn) -> true.
%check_vsn(Node,_Vsn) -> 
%    display_warning(Node,faulty_vsn_of_runtime_tools),
%    false.

clients(Nodes, {wrap,Name}) ->
    F = fun(Node) -> 
		TraceFile = name(Node,Name),
		{Node,{TraceFile++".",wrap,".wrp"},TraceFile} 
	end,
    lists:map(F,Nodes);
clients(Nodes, {wrap,Name,Size,Count}) ->
    F = fun(Node) -> 
		TraceFile = name(Node,Name),
		{Node,{TraceFile++".",wrap,".wrp",Size,Count},TraceFile} 
	end,
    lists:map(F,Nodes);
clients(Nodes, {local,RealClient}) ->
    WrapClients = clients(Nodes,RealClient),
    F = fun({Node,Client,TraceFile}) -> 
		{Node,{local,Client},TraceFile}
	end,
    lists:map(F,WrapClients);
clients(Nodes, Name) ->
    F = fun(Node) -> 
		TraceFile = name(Node,Name),
		{Node,TraceFile,TraceFile}
	end,
    lists:map(F,Nodes).
    
name(Node,Filename) ->
    Dir = filename:dirname(Filename), 
    File = filename:basename(Filename),
    filename:join(Dir,atom_to_list(Node) ++ "-" ++ File).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Handling of config file
store(Func,Args) ->
    Last = case ets:last(?history_table) of
	       '$end_of_table' -> 0;
	       Int when is_integer(Int) -> Int
	   end,
    ets:insert(?history_table,{Last+1,{?MODULE,Func,Args}}).

list_history() -> 
    %% the check is only to see if the tool is started.
    case ets:info(?history_table) of
	undefined -> {error, not_running};
	_info -> ets:tab2list(?history_table)
    end.

run_history([H|T]) ->
    case run_history(H) of
	ok -> run_history(T);
	{error,not_found} -> {error,{not_found,H}}
    end;
run_history([]) ->
    ok;
run_history(N) ->
    case catch ets:lookup(?history_table,N) of
	[{N,{M,F,A}}] -> 
	    print_func(M,F,A),
	    R = apply(M,F,A),
	    print_result(R);
	_ -> 
	    {error, not_found}
    end.
	
write_config(ConfigFile,all) ->
    write_config(ConfigFile,['_']);
write_config(ConfigFile,Config) ->
    write_config(ConfigFile,Config,[]).
write_config(ConfigFile,all,Opt) ->
    write_config(ConfigFile,['_'],Opt);
write_config(ConfigFile,Nums,Opt) when is_list(Nums), is_integer(hd(Nums)); 
				       Nums=:=['_'] ->
    F = fun(N) -> ets:select(?history_table,
			     [{{N,'$1'},[],['$1']}])
	end,
    Config = lists:append(lists:map(F,Nums)),
    do_write_config(ConfigFile,Config,Opt);
write_config(ConfigFile,Config,Opt) when is_list(Config) ->
    case check_config(Config,[]) of
	{ok,Config1} -> do_write_config(ConfigFile,Config1,Opt);
	Error -> Error
    end.

do_write_config(ConfigFile,Config,Opt) ->
    case Opt of
	[append] -> ok;
        [] -> file:delete(ConfigFile)
    end,
    write_binary(ConfigFile,Config).

check_config([{?MODULE=Mod,Func,Args}|Rest],Acc) ->
    %% Check only in this module, since other modules might not
    %% be loaded at the time of creating the config file.
    case erlang:function_exported(Mod,Func,length(Args)) of
	true -> check_config(Rest,[{Mod,Func,Args}|Acc]);
	false -> {error, {not_exported,{Mod,Func,Args}}}
    end;
check_config([{Mod,Func,Args}|Rest],Acc) ->
    check_config(Rest,[{Mod,Func,Args}|Acc]);
check_config([],Acc) ->
    {ok,lists:reverse(Acc)};
check_config([Other|_Rest],_Acc) ->
    {error,{illegal_config,Other}}.


list_config(ConfigFile) ->
    case file:read_file(ConfigFile) of
	{ok,B} -> read_config(B,[],1);
	Error -> Error
    end.
    
read_config(<<>>,Acc,_N) ->
    lists:reverse(Acc);
read_config(B,Acc,N) ->
    {{M,F,A},Rest} = get_term(B),
    read_config(Rest,[{N,{M,F,A}}|Acc],N+1).


run_config(ConfigFile) ->
    case list_config(ConfigFile) of
	Config when is_list(Config) ->
	    lists:foreach(fun({_,{M,F,A}}) -> print_func(M,F,A),
					      R = apply(M,F,A),
					      print_result(R) 
			  end,
			  Config);
	Error -> Error
    end.

run_config(ConfigFile,N) ->
    case list_config(ConfigFile) of
	Config when is_list(Config) ->
	    case lists:keysearch(N,1,Config) of
		{value,{N,{M,F,A}}} -> 
		    print_func(M,F,A),
		    apply(M,F,A);
		false -> 
		    {error, not_found}
	    end;
	Error -> Error
    end.
    
    
print_func(M,F,A) ->
    Args = arg_list(A,[]),
    io:format("~w:~w(~s) ->~n",[M,F,Args]).
print_result(R) ->
    io:format("~p~n~n",[R]).

arg_list([],[]) ->
    "";
arg_list([A1],Acc) ->
    Acc++io_lib:format("~w",[A1]);
arg_list([A1|A],Acc) ->
    arg_list(A,Acc++io_lib:format("~w,",[A1])).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Set trace flags on processes
p(Procs0,Flags0) ->
    store(p,[Procs0,Flags0]),
    no_store_p(Procs0,Flags0).
no_store_p(Procs0,Flags0) ->
    case transform_flags(to_list(Flags0)) of
	{error,Reason} -> 
	    {error,Reason};
	Flags ->
	    Procs = procs(Procs0),
	    case lists:foldl(fun(P,{PMatched,Ps}) -> case dbg:p(P,Flags) of
					     {ok,Matched} -> 
						 {[{P,Matched}|PMatched],[P|Ps]};
					     {error,Reason} -> 
						 display_warning(P,Reason),
						 {PMatched,Ps}
					 end
			     end,{[],[]},Procs) of
		{[],[]} -> {error, no_match};
		{SuccMatched,Succ} ->
		    no_store_write_trace_info(flags,{Succ,Flags}),
		    {ok,SuccMatched}
	    end
    end.

transform_flags([clear]) ->
    [clear];
transform_flags(Flags) ->
    dbg:transform_flags(Flags).


procs(Procs) when is_list(Procs) ->
    lists:foldl(fun(P,Acc) -> proc(P)++Acc end,[],Procs);
procs(Proc) ->
    proc(Proc).

proc(Procs) when Procs=:=all; Procs=:=existing; Procs=:=new ->
    [Procs];
proc(Name) when is_atom(Name) ->
    [Name]; % can be registered on this node or other node
proc(Pid) when is_pid(Pid) ->
    [Pid];
proc({global,Name}) ->
    case global:whereis_name(Name) of
	Pid when is_pid(Pid) ->
	    [Pid];
	undefined ->
	    []
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Trace pattern
tp(A,B) ->
    store(tp,[A,B]),
    dbg:tp(A,B).
tp(A,B,C) ->
    store(tp,[A,B,C]),
    dbg:tp(A,B,C).
tp(A,B,C,D) ->
    store(tp,[A,B,C,D]),
    dbg:tp(A,B,C,D).

tpl(A,B) ->
    store(tpl,[A,B]),
    dbg:tpl(A,B).
tpl(A,B,C) ->
    store(tpl,[A,B,C]),
    dbg:tpl(A,B,C).
tpl(A,B,C,D) ->
    store(tpl,[A,B,C,D]),
    dbg:tpl(A,B,C,D).

ctp() ->
    store(ctp,[]),
    dbg:ctp().
ctp(A) ->
    store(ctp,[A]),
    dbg:ctp(A).
ctp(A,B) ->
    store(ctp,[A,B]),
    dbg:ctp(A,B).
ctp(A,B,C) ->
    store(ctp,[A,B,C]),
    dbg:ctp(A,B,C).

ctpl() ->
    store(ctpl,[]),
    dbg:ctpl().
ctpl(A) ->
    store(ctpl,[A]),
    dbg:ctpl(A).
ctpl(A,B) ->
    store(ctpl,[A,B]),
    dbg:ctpl(A,B).
ctpl(A,B,C) ->
    store(ctpl,[A,B,C]),
    dbg:ctpl(A,B,C).

ctpg() ->
    store(ctpg,[]),
    dbg:ctpg().
ctpg(A) ->
    store(ctpg,[A]),
    dbg:ctpg(A).
ctpg(A,B) ->
    store(ctpg,[A,B]),
    dbg:ctpg(A,B).
ctpg(A,B,C) ->
    store(ctpg,[A,B,C]),
    dbg:ctpg(A,B,C).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Support for sequential trace
seq_trigger_ms() -> seq_trigger_ms(all).
seq_trigger_ms(all) -> seq_trigger_ms(?seq_trace_flags);
seq_trigger_ms(Flag) when is_atom(Flag) -> seq_trigger_ms([Flag],[]);
seq_trigger_ms(Flags) -> seq_trigger_ms(Flags,[]).
seq_trigger_ms([Flag|Flags],Body) ->
    case lists:member(Flag,?seq_trace_flags) of
	true -> seq_trigger_ms(Flags,[{set_seq_token,Flag,true}|Body]);
	false -> {error,{illegal_flag,Flag}}
    end;
seq_trigger_ms([],Body) ->
    [{'_',[],Body}].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Write information to the .ti file
write_trace_info(Key,What) ->
    store(write_trace_info,[Key,What]),
    no_store_write_trace_info(Key,What).

no_store_write_trace_info(Key,What) ->
    case whereis(?MODULE) of
	undefined -> ok;
	Pid when is_pid(Pid) -> ?MODULE ! {write_trace_info,Key,What}
    end,
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Stop tracing on all nodes
stop() ->
    stop([]).
stop(Opts) ->
    Fetch = stop_opts(Opts),
    case whereis(?MODULE) of
	undefined -> ok;
	Pid when is_pid(Pid) -> 
	    ?MODULE ! {stop,Fetch,self()},
	    receive {?MODULE,stopped} -> ok end
    end,
    stopped.

stop_opts(Opts) ->
    case lists:member(format,Opts) of
	true -> 
	    format; % format implies fetch
	false -> 
	    case lists:member(fetch,Opts) of
		true -> fetch;
		false -> nofetch
	    end
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Process implementation
start() ->
    case whereis(?MODULE) of
	undefined ->
	    Parent = self(),
	    Pid = spawn(fun() -> init(Parent) end),
	    receive {started,Pid} -> ok end;
	Pid when is_pid(Pid) ->
	    ok
    end.


init(Parent) ->
    register(?MODULE,self()),
    ets:new(?history_table,[ordered_set,named_table,public]),
    Parent ! {started,self()},
    loop(dict:new()).

loop(NodeInfo) ->
    receive 
	{init_node,Node,MetaFile,PI,Traci} ->
	    erlang:monitor_node(Node,true),
	    MetaPid = 
		case rpc:call(Node,
			      observer_backend,
			      ttb_init_node,
			      [MetaFile,PI,Traci]) of
		    {ok,MP} ->
			MP;
		    {badrpc,nodedown} ->
			%% We will get a nodedown message
			undefined
		end,
	    loop(dict:store(Node,{MetaFile,MetaPid},NodeInfo));
	{get_nodes,Sender} ->
	    Sender ! {?MODULE,dict:fetch_keys(NodeInfo)},
	    loop(NodeInfo);
	{write_trace_info,Key,What} ->
	    dict:fold(fun(Node,{_MetaFile,MetaPid},_) -> 
			      rpc:call(Node,observer_backend,
				       ttb_write_trace_info,[MetaPid,Key,What])
		      end,
		      ok,
		      NodeInfo),
	    loop(NodeInfo);
	{nodedown,Node} ->
	    loop(dict:erase(Node,NodeInfo));
	{stop,nofetch,Sender} ->
	    dict:fold(
	      fun(Node,{_,MetaPid},_) -> 
		      rpc:call(Node,observer_backend,ttb_stop,[MetaPid])
	      end,
	      ok,
	      NodeInfo),
	    dbg:stop_clear(),
	    ets:delete(?history_table),
	    Sender ! {?MODULE,stopped};
	{stop,FetchOrFormat,Sender} ->
	    Localhost = host(node()),
	    Dir = ?upload_dir++ts(),
 	    file:make_dir(Dir),
	    %% The nodes are traversed twice here because
	    %% the meta tracing in observer_backend must be
	    %% stopped before dbg is stopped, and dbg must
	    %% be stopped before the trace logs are moved orelse
	    %% windows complains.
	    AllNodesAndMeta = 
		dict:fold(
		  fun(Node,{MetaFile,MetaPid},Nodes) -> 
			  rpc:call(Node,observer_backend,ttb_stop,[MetaPid]),
			  [{Node,MetaFile}|Nodes]
		  end,
		  [],
		  NodeInfo),
	    dbg:stop_clear(),
	    AllNodes = 
		lists:map(
		  fun({Node,MetaFile}) ->
			  spawn(fun() -> fetch(Localhost,Dir,Node,MetaFile) end),
			  Node
		  end,
		  AllNodesAndMeta),
	    ets:delete(?history_table),
	    wait_for_fetch(AllNodes),
	    io:format("Stored logs in ~s~n",[filename:absname(Dir)]),
	    case FetchOrFormat of
		format -> format(Dir);
		fetch -> ok
	    end,
	    Sender ! {?MODULE,stopped}
         ?get_status
    end.

get_nodes() ->
    ?MODULE ! {get_nodes,self()},
    receive {?MODULE,Nodes} -> Nodes end.

ts() ->
    {{Y,M,D},{H,Min,S}} = calendar:now_to_local_time(now()),
    io_lib:format("-~4.4.0w~2.2.0w~2.2.0w-~2.2.0w~2.2.0w~2.2.0w",
		  [Y,M,D,H,Min,S]).



fetch(Localhost,Dir,Node,MetaFile) ->
    case host(Node) of
	Localhost -> % same host, just move the files
	    Files = rpc:call(Node,observer_backend,ttb_get_filenames,[MetaFile]),
	    lists:foreach(
	      fun(File0) ->
		      File = filename:join(Dir,filename:basename(File0)),
		      file:rename(File0,File)
	      end,
	      Files);
	_Otherhost ->
	    {ok, LSock} = gen_tcp:listen(0, [binary,{packet,2},{active,false}]),
	    {ok,Port} = inet:port(LSock),
	    rpc:cast(Node,observer_backend,ttb_fetch,
		     [MetaFile,{Port,Localhost}]),
	    {ok, Sock} = gen_tcp:accept(LSock),
	    receive_files(Dir,Sock,undefined),
	    ok = gen_tcp:close(LSock),
	    ok = gen_tcp:close(Sock)
    end,
    ?MODULE ! {fetch_complete,Node}.

receive_files(Dir,Sock,Fd) ->
    case gen_tcp:recv(Sock, 0) of
	{ok, <<0,Bin/binary>>} ->
	    file:write(Fd,Bin),
	    receive_files(Dir,Sock,Fd);
	{ok, <<1,Bin/binary>>} ->
	    File0 = binary_to_list(Bin),
	    File = filename:join(Dir,File0),
	    {ok,Fd1} = file:open(File,[raw,write]),
	    receive_files(Dir,Sock,Fd1);
	{error, closed} ->
	    ok = file:close(Fd)
    end.    

host(Node) ->
    [_name,Host] = string:tokens(atom_to_list(Node),"@"),
    Host.


wait_for_fetch([]) ->
    ok;
wait_for_fetch(Nodes) ->
    receive 
	{fetch_complete,Node} ->
	    wait_for_fetch(lists:delete(Node,Nodes))
    end.

%%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%%% TRACE INFORMATION FILE
%%% ======================
%%% The trace information file has the same name as the trace log,
%%% but with the extension ".ti". It contains process information,
%%% trace information and any data the user writes with the
%%% function write_trace_info/2.
%%%
%%% The file is read during formatting of trace logs, and all data
%%% except process information is included in the handler function.
%%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

write_info(Nodes,PI,Traci) ->
    lists:foreach(fun({N,{local,C,_},F}) -> 
			  MetaFile = F ++ ".ti",
			  file:delete(MetaFile),
			  Traci1 = [{node,N},{file,C}|Traci],
			  {ok,Port} = dbg:get_tracer(N),
			  ?MODULE ! 
			      {init_node, N, {local,MetaFile,Port}, PI, Traci1};
		     ({N,C,F}) -> 
			  MetaFile = F ++ ".ti",
			  Traci1 = [{node,N},{file,C}|Traci],
			  ?MODULE ! {init_node, N, MetaFile, PI, Traci1}
		  end,
		  Nodes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Format binary trace logs
format(Files) ->
    format(Files,[]).
format(Files,Opt) ->
    {Out,Handler} = format_opt(Opt),
    ets:new(?MODULE,[named_table]),
    format(Files,Out,Handler).
format(File,Out,Handler) when is_list(File), is_integer(hd(File)) ->
    Files = 
	case filelib:is_dir(File) of
	    true ->  % will merge all files in the directory
		MetaFiles = filelib:wildcard(filename:join(File,"*.ti")),
		lists:map(fun(M) ->
				  Sub = string:left(M,length(M)-3),
				  case filelib:is_file(Sub) of
				      true -> Sub;
				      false -> Sub++".*.wrp"
				  end
			  end,
			  MetaFiles);
	    false -> % format one file
		[File]
	end,
    format(Files,Out,Handler);
format(Files,Out,Handler) when is_list(Files), is_list(hd(Files)) ->
    StopDbg = case whereis(dbg) of
		  undefined -> true;
		  _ -> false
	      end,
    Details = lists:foldl(fun(File,Acc) -> [prepare(File,Handler)|Acc] end,
			  [],Files),
    Fd = get_fd(Out),
    R = do_format(Fd,Details),
    file:close(Fd),
    ets:delete(?MODULE),
    case StopDbg of
	true -> dbg:stop_clear();
	false -> ok
    end,
    R.

prepare(File,Handler) ->
    {Traci,Proci} = read_traci(File),
    Node = get_node(Traci),
    lists:foreach(fun({Pid,PI}) ->
			  %% The last definition for a Pid will overwrite
			  %% any previous definitions. That should be what
			  %% we want (we will get the registered name for
			  %% the process, rather than the initial call if
			  %% both are present in the list).
			  ets:insert(?MODULE,{Pid,PI,Node})
		  end,Proci),
    FileOrWrap = get_file(File,Traci),
    Handler1 = get_handler(Handler,Traci),
    {FileOrWrap,Traci,Handler1}.

format_opt(Opt) ->
    Out = case lists:keysearch(out,1,Opt) of
	      {value,{out,O}} -> O;
	      _ -> standard_io
	  end,
    Handler = case lists:keysearch(handler,1,Opt) of
	      {value,{handler,H}} -> H;
	      _ -> undefined
	  end,
    {Out,Handler}.


read_traci(File) ->
    MetaFile = get_metafile(File),
    case file:read_file(MetaFile) of
	{ok,B} -> 
	    interpret_binary(B,dict:new(),[]);
	_ -> 
	    io:format("Warning: no meta data file: ~s~n",[MetaFile]),
	    {dict:new(),[]}
    end.

get_metafile(File) ->
    case filename:rootname(File,".wrp") of
	File -> File++".ti";
	Wrap -> filename:rootname(Wrap)++".ti"
    end.


interpret_binary(<<>>,Dict,P) ->
    {Dict,lists:reverse(P)};
interpret_binary(B,Dict,P) ->
    {Term,Rest} = get_term(B),
    {Dict1,P1} = 
	case Term of
	    {pid,PI} ->
		{Dict,[PI|P]};
	    {Key,Val} ->
		{dict:update(Key,fun(Val0) -> [Val|Val0] end, [Val], Dict),P}
	end,
    interpret_binary(Rest,Dict1,P1).

get_fd(Out) ->
    case Out of
	standard_io ->
	    Out;
	_file ->
	    file:delete(Out),
	    case file:open(Out,[append]) of
		{ok,Fd} -> Fd;
		Error -> exit(Error)
	    end
    end.

get_node(Traci) ->
    case dict:find(node,Traci) of
	{ok,[Node]} -> Node;
	error -> unknown
    end.

get_file(File,Traci) ->
    case dict:find(file,Traci) of
	{ok,[Client]} ->
	    check_client(Client,File);
	error ->
	    check_exists(File)
    end.

check_client(Client,File) when is_list(Client) ->
    check_exists(File);
check_client(Client,File) when is_tuple(Client),element(2,Client)==wrap ->
    Root = filename:rootname(File,".wrp"),
    case filename:extension(Root) of
	".*" -> 
	    Part1 = filename:rootname(Root,"*"),
	    setelement(1,Client,Part1);
	_ -> 
	    check_exists(File)
    end.

check_exists(File) ->
    case file:read_file_info(File) of
	{ok,#file_info{type=regular}} -> File;
	_ -> 
	    exit({error,no_file})
    end.
	

get_handler(Handler,Traci) ->
    case Handler of
	undefined -> 
	    case dict:find(handler,Traci) of
		{ok,[H]} -> H;
		error -> undefined
	    end;
	_ ->
	    Handler
    end.

do_format(Fd,Details) ->
    Clients = lists:foldl(fun({FileOrWrap,Traci,Handler},Acc) ->
				  [start_client(FileOrWrap,Traci,Handler)
				   |Acc]
			  end,[],Details),
    init_collector(Fd,Clients).


start_client(FileOrWrap,Traci,et) ->
    dbg:trace_client(file, FileOrWrap, 
		     {fun handler/2, 
		      {dict:to_list(Traci),{{ttb_et,handler},initial}}});
start_client(FileOrWrap,Traci,undefined) ->
    dbg:trace_client(file, FileOrWrap, 
		     {fun handler/2, 
		      {dict:to_list(Traci),{fun defaulthandler/4,initial}}});
start_client(FileOrWrap,Traci,Handler) ->
    dbg:trace_client(file, FileOrWrap, 
		     {fun handler/2, {dict:to_list(Traci),Handler}}).

handler(Trace,State) ->
    %% State here is only used for the initial state. The accumulated
    %% State is maintained by collector!!!
    receive 
	{get,Collector} -> Collector ! {self(),{Trace,State}};
	done -> ok
    end,
    State.

handler1(Trace,{Fd,{Traci,{Fun,State}}}) when is_function(Fun) ->
    {Traci,{Fun,Fun(Fd,Trace,Traci,State)}};
handler1(Trace,{Fd,{Traci,{{M,F},State}}}) when is_atom(M), is_atom(F) ->
    {Traci,{{M,F},M:F(Fd,Trace,Traci,State)}}.

defaulthandler(Fd,Trace,_Traci,initial) ->
    dbg:dhandler(Trace,Fd);
defaulthandler(_Fd,Trace,_Traci,State) ->
    dbg:dhandler(Trace,State).

init_collector(Fd,Clients) ->
    Collected = get_first(Clients),
    collector(Fd,sort(Collected)).

collector(Fd,[{_,{Client,{Trace,State}}}|Rest]) ->
    Trace1 = update_procinfo(Trace),
    State1 = handler1(Trace1,{Fd,State}),
    case get_next(Client,State1) of
	end_of_trace -> 
	    handler1(end_of_trace,{Fd,State1}),
	    collector(Fd,Rest);
	Next -> collector(Fd,sort([Next|Rest]))
    end;
collector(_Fd,[]) ->
    ok.

update_procinfo({drop,_N}=Trace) ->
    Trace;
update_procinfo(Trace) when element(1,Trace)==seq_trace ->
    Info = element(3,Trace),
    Info1 = 
	case Info of
	    {send, Serial, From, To, Msg} ->
		{send, Serial, get_procinfo(From), get_procinfo(To), Msg};
	    {'receive', Serial, From, To, Msg} ->
		{'receive', Serial, get_procinfo(From), get_procinfo(To), Msg};
	    {print, Serial, From, Void, UserInfo} ->
		{print, Serial, get_procinfo(From), Void, UserInfo};
	    Other ->
		Other
	end,
    setelement(3,Trace,Info1);
update_procinfo(Trace) when element(3,Trace)==send ->
    PI = get_procinfo(element(5,Trace)),
    setelement(5,Trace,PI);
update_procinfo(Trace) ->
    Pid = element(2,Trace),
    ProcInfo = get_procinfo(Pid),
    setelement(2,Trace,ProcInfo).

get_procinfo(Pid) when is_pid(Pid) ->
    case ets:lookup(?MODULE,Pid) of
	[PI] -> PI;
	[] -> Pid
    end;
get_procinfo(Name) when is_atom(Name) ->
    case ets:match_object(?MODULE,{'_',Name,node()}) of
	[PI] -> PI;
	[] -> Name
    end;
get_procinfo({Name,Node}) when is_atom(Name) ->
    case ets:match_object(?MODULE,{'_',Name,Node}) of
	[PI] -> PI;
	[] -> {Name,Node}
    end.	 

get_first([Client|Clients]) ->
    Client ! {get,self()},
    receive 
	{Client,{end_of_trace,_}} -> 
	    get_first(Clients);
	{Client,{Trace,_State}}=Next -> 
	    [{timestamp(Trace),Next}|get_first(Clients)]
    end;
get_first([]) -> [].

get_next(Client,State) when is_pid(Client) ->
    Client ! {get,self()},
    receive 
	{Client,{end_of_trace,_}} -> 
	    end_of_trace;
	{Client,{Trace,_OldState}} -> 
	    {timestamp(Trace),{Client,{Trace,State}}} % inserting new state!!
    end.

sort(List) ->
    lists:keysort(1,List).


timestamp(Trace) when element(1,Trace) =:= trace_ts;
		      element(1,Trace) =:= seq_trace, tuple_size(Trace) =:= 4 ->
    element(tuple_size(Trace),Trace);
timestamp(_Trace) ->
    0.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% common internal functions
to_list(Atom) when is_atom(Atom) -> [Atom];
to_list(List) when is_list(List) -> List.

write_binary(File,TermList) ->
    {ok,Fd} = file:open(File,[raw,append]),    
    %% Using the function implemented in observer_backend, only because
    %% is exists - so I don't have to write the same code twice.
    observer_backend:ttb_write_binary(Fd,TermList),
    file:close(Fd).

get_term(B) ->
    <<S:8, B2/binary>> = B,
    <<T:S/binary, Rest/binary>> = B2,
    case binary_to_term(T) of
	{'$size',Sz} -> 
	    %% size of the actual term was bigger than 8 bits
	    <<T1:Sz/binary, Rest1/binary>> = Rest,
	    {binary_to_term(T1),Rest1};
	Term ->
	    {Term,Rest}
    end.

display_warning(Item,Warning) ->
    io:format("Warning: {~w,~w}~n",[Warning,Item]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Trace client which reads an IP port and puts data directly to a file.
%%% This is used when tracing remote nodes with no file system.
ip_to_file(Trace,{file,File}) ->
    Fun = dbg:trace_port(file,File), %File can be a filename or a wrap spec
    Port = Fun(),
    ip_to_file(Trace,Port);
ip_to_file({metadata,MetaFile,MetaData},Port) ->
    {ok,MetaFd} = file:open(MetaFile,[write,raw,append]),
    file:write(MetaFd,MetaData),
    file:close(MetaFd),
    Port;
ip_to_file(Trace,Port) ->
    B = term_to_binary(Trace),
    erlang:port_command(Port,B),
    Port.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% For debugging
dump_ti(File) ->
    {ok,B} = file:read_file(File),
    dump_ti(B,[]).

dump_ti(<<>>,Acc) ->
    lists:reverse(Acc);
dump_ti(B,Acc) ->
    {Term,Rest} = get_term(B),
    dump_ti(Rest,[Term|Acc]).


