%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2002-2017. All Rights Reserved.
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
-module(ttb).
-author('siri@erix.ericsson.se').
-author('bartlomiej.puzon@erlang-solutions.com').

%% API
-export([tracer/0,tracer/1,tracer/2,p/2,stop/0,stop/1,start_trace/4]).
-export([get_et_handler/0]).
-export([tp/2, tp/3, tp/4, ctp/0, ctp/1, ctp/2, ctp/3, tpl/2, tpl/3, tpl/4, 
	 ctpl/0, ctpl/1, ctpl/2, ctpl/3, ctpg/0, ctpg/1, ctpg/2, ctpg/3,
	 tpe/2, ctpe/1]).
-export([seq_trigger_ms/0,seq_trigger_ms/1]).
-export([write_trace_info/2]).
-export([write_config/2,write_config/3,run_config/1,run_config/2,list_config/1]).
-export([list_history/0,run_history/1]).
-export([format/1,format/2]).

%% For debugging
-export([dump_ti/1]).

-include_lib("kernel/include/file.hrl").
-define(meta_time,5000).
-define(fetch_time, 10000).
-define(history_table,ttb_history_table).
-define(seq_trace_flags,[send,'receive',print,timestamp]).
-define(upload_dir(Logname),"ttb_upload_"++Logname).
-define(last_config, "ttb_last_config").
-define(partial_dir, "ttb_partial_result").
-ifdef(debug).
-define(get_status,;get_status -> erlang:display(dict:to_list(NodeInfo),loop(NodeInfo, TraceInfo)).
-else.
-define(get_status,).
-endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Shortcut
start_trace(Nodes, Patterns, {Procs, Flags}, Options) ->
    {ok, _} = tracer(Nodes, Options),
    [{ok, _} = apply(?MODULE, tpl, tuple_to_list(Args)) || Args <- Patterns],
    {ok, _} = p(Procs, Flags).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Open a trace port on all given nodes and create the meta data file
tracer() -> tracer(node()).
tracer(shell) -> tracer(node(), shell);
tracer(dbg) -> tracer(node(), {shell, only});
tracer(Nodes) -> tracer(Nodes,[]).
tracer(Nodes,Opt) ->
    {PI,Client,Traci} = opt(Opt),
    %%We use initial Traci as SessionInfo for loop/2
    Pid = start(Traci),
    store(tracer,[Nodes,Opt]),
    do_tracer(Nodes,PI,Client,[{ttb_control, Pid}|Traci]).

do_tracer(Nodes0,PI,Client,Traci) ->
    Nodes = nods(Nodes0),
    Clients = clients(Nodes,Client),
    do_tracer(Clients,PI,Traci).

do_tracer(Clients,PI,Traci) ->
    Shell = proplists:get_value(shell, Traci, false),
    IpPortSpec =
	case proplists:get_value(queue_size, Traci) of
	    undefined -> 0;
	    QS -> {0,QS}
	end,
    DefShell = fun(Trace) -> dbg:dhandler(Trace, standard_io) end,
    {ClientSucc,Succ} =
	lists:foldl(
	  fun({N,{local,File},TF},{CS,S}) ->
                  {TF2, FileInfo, ShellOutput} =
		      case Shell of
			  only  -> {none, shell_only, DefShell};
			  true  -> {TF, {file,File}, DefShell};
			  {only,Fun} -> {none, shell_only, Fun};
			  Fun when is_function(Fun) -> {TF, {file,File}, Fun};
			  _    -> {TF, {file,File}, false}
		      end,
		  Host = case N of
			     nonode@nohost ->
				 {ok, H} = inet:gethostname(),
				 H;
			     _ ->
				 [_,H] = string:lexemes(atom_to_list(N),"@"),
				 H
			 end,
		  case catch dbg:tracer(N,port,dbg:trace_port(ip,IpPortSpec)) of
		      {ok,N} ->
			  {ok,Port} = dbg:trace_port_control(N,get_listen_port),
			  {ok,T} = dbg:get_tracer(N),
			  rpc:call(N,seq_trace,set_system_tracer,[T]),
			  dbg:trace_client(ip,{Host,Port},
					   {fun ip_to_file/2,{FileInfo, ShellOutput}}),
			  {[{N,{local,File,Port},TF2}|CS], [N|S]};
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

opt(Opt) when is_list(Opt) ->
    opt(Opt,{true,?MODULE,[]});
opt(Opt) ->
    opt([Opt]).

opt([{process_info,PI}|O],{_,Client,Traci}) ->
    opt(O,{PI,Client,Traci});
opt([{file,Client}|O],{PI,_,Traci}) ->
    opt(O,{PI,Client,[{logfile,get_logname(Client)}|Traci]});
opt([{handler,Handler}|O],{PI,Client,Traci}) ->
    opt(O,{PI,Client,[{handler,Handler}|Traci]});
opt([{timer, {MSec, StopOpts}}|O],{PI,Client,Traci}) ->
    opt(O,{PI,Client,[{timer,{MSec, StopOpts}}|Traci]});
opt([{timer, MSec}|O],{PI,Client,Traci}) ->
    opt(O,{PI,Client,[{timer,{MSec, []}}|Traci]});
opt([{overload_check, {MSec,M,F}}|O],{PI,Client,Traci}) ->
    opt(O,{PI,Client,[{overload_check,{MSec,M,F}}|Traci]});
opt([shell|O],{PI,Client,Traci}) ->
    opt(O,{PI,Client,[{shell, true}|Traci]});
opt([{shell,Type}|O],{PI,Client,Traci}) ->
    opt(O,{PI,Client,[{shell, Type}|Traci]});
opt([resume|O],{PI,Client,Traci}) ->
    opt(O,{PI,Client,[{resume, {true, ?fetch_time}}|Traci]});
opt([{resume,MSec}|O],{PI,Client,Traci}) ->
    opt(O,{PI,Client,[{resume, {true, MSec}}|Traci]});
opt([{flush,MSec}|O],{PI,Client,Traci}) ->
    opt(O,{PI,Client,[{flush, MSec}|Traci]});
opt([{queue_size,QueueSize}|O],{PI,Client,Traci}) ->
    opt(O,{PI,Client,[{queue_size,QueueSize}|Traci]});
opt([],Opt) ->
    ensure_opt(Opt).

ensure_opt({PI,Client,Traci}) ->
    case {proplists:get_value(flush, Traci), Client} of
        {undefined, _}  -> ok;
        {_, {local, _}} -> exit(flush_unsupported_with_ip_trace_port);
        {_,_}           -> ok
    end,
    NeedIpTracer = proplists:get_value(shell, Traci, false) /= false,
    case {NeedIpTracer, Client} of
        {false, _}        -> {PI, Client, Traci};
        {true, ?MODULE}       -> {PI, {local, ?MODULE}, Traci};
        {true, {local, File}} -> {PI, {local, File}, Traci};
        {true, _}             -> exit(local_client_required_on_shell_tracing)
    end.

get_logname({local, F}) -> get_logname(F);
get_logname({wrap, F}) -> filename:basename(F);
get_logname({wrap, F, _, _}) -> filename:basename(F);
get_logname(F) -> filename:basename(F).

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

run_history(all) ->
    CurrentHist = ets:tab2list(?history_table),
    ets:delete_all_objects(?history_table),
    [run_printed(MFA,true) || {_, MFA} <- CurrentHist];
run_history(all_silent) ->
    CurrentHist = ets:tab2list(?history_table),
    ets:delete_all_objects(?history_table),
    [run_printed(MFA,false) || {_, MFA} <- CurrentHist];
run_history([]) ->
    ok;
run_history(N) ->
    case catch ets:lookup(?history_table,N) of
	[{N,{M,F,A}}] -> 
            run_printed({M,F,A},true);
	_ -> 
	    {error, not_found}
    end.

run_printed({M,F,A},Verbose) ->
    Verbose andalso print_func(M,F,A),
    R = apply(M,F,A),
    Verbose andalso print_result(R).
	
write_config(ConfigFile,all) ->
    write_config(ConfigFile,['_']);
write_config(ConfigFile,Config) ->
    write_config(ConfigFile,Config,[]).
write_config(ConfigFile,all,Opt) ->
    write_config(ConfigFile,['_'],Opt);
write_config(ConfigFile,Config,Opt) when not(is_list(Opt)) ->
    write_config(ConfigFile,Config,[Opt]);
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
    io:format("~w:~tw(~ts) ->~n",[M,F,Args]).
print_result(R) ->
    io:format("~tp~n~n",[R]).

arg_list([],[]) ->
    "";
arg_list([A1],Acc) ->
    Acc++io_lib:format("~tw",[A1]);
arg_list([A1|A],Acc) ->
    arg_list(A,Acc++io_lib:format("~tw,",[A1])).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Set trace flags on processes
p(ProcsPorts0,Flags0) ->
    ensure_no_overloaded_nodes(),
    store(p,[ProcsPorts0,Flags0]),
    no_store_p(ProcsPorts0,Flags0).
no_store_p(ProcsPorts0,Flags0) ->
    case transform_flags(to_list(Flags0)) of
	{error,Reason} -> 
	    {error,Reason};
	Flags ->
	    ProcsPorts = procs_ports(ProcsPorts0),
	    case lists:foldl(fun(P,{PMatched,Ps}) -> case dbg:p(P,Flags) of
					     {ok,Matched} -> 
						 {[{P,Matched}|PMatched],[P|Ps]};
					     {error,Reason} -> 
						 display_warning(P,Reason),
						 {PMatched,Ps}
						     end
			     end,{[],[]},ProcsPorts) of
		{[],[]} -> {error, no_match};
		{SuccMatched,Succ} ->
		    no_store_write_trace_info(flags,{Succ,Flags}),
		    ?MODULE ! trace_started,
		    {ok,SuccMatched}
	    end
    end.

transform_flags([clear]) ->
    [clear];
transform_flags(Flags) ->
    dbg:transform_flags([timestamp | Flags]).


procs_ports(Procs) when is_list(Procs) ->
    lists:foldl(fun(P,Acc) -> proc_port(P)++Acc end,[],Procs);
procs_ports(Proc) ->
    proc_port(Proc).

proc_port(P) when P=:=all; P=:=ports; P=:=processes;
                 P=:=existing; P=:=existing_ports; P=:=existing_processes;
                 P=:=new; P=:=new_ports; P=:=new_processes ->
    [P];
proc_port(Name) when is_atom(Name) ->
    [Name]; % can be registered on this node or other node
proc_port(Pid) when is_pid(Pid) ->
    [Pid];
proc_port(Port) when is_port(Port) ->
    [Port];
proc_port({global,Name}) ->
    case global:whereis_name(Name) of
	Pid when is_pid(Pid) ->
	    [Pid];
	undefined ->
	    []
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Trace pattern
tp(A,B) ->
    ensure_no_overloaded_nodes(),
    store(tp,[A,ms(B)]),
    dbg:tp(A,ms(B)).
tp(A,B,C) ->
    ensure_no_overloaded_nodes(),
    store(tp,[A,B,ms(C)]),
    dbg:tp(A,B,ms(C)).
tp(A,B,C,D) ->
    ensure_no_overloaded_nodes(),
    store(tp,[A,B,C,ms(D)]),
    dbg:tp(A,B,C,ms(D)).

tpl(A,B) ->
    ensure_no_overloaded_nodes(),
    store(tpl,[A,ms(B)]),
    dbg:tpl(A,ms(B)).
tpl(A,B,C) ->
    ensure_no_overloaded_nodes(),
    store(tpl,[A,B,ms(C)]),
    dbg:tpl(A,B,ms(C)).
tpl(A,B,C,D) ->
    ensure_no_overloaded_nodes(),
    store(tpl,[A,B,C,ms(D)]),
    dbg:tpl(A,B,C,ms(D)).

tpe(A,B) ->
    ensure_no_overloaded_nodes(),
    store(tpe,[A,ms(B)]),
    dbg:tpe(A,ms(B)).

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

ctpe(A) ->
    store(ctpe,[A]),
    dbg:ctpe(A).

ms(return) ->
    [{'_',[],[{return_trace}]}];
ms(caller) ->
    [{'_',[],[{message,{caller}}]}];
ms({codestr, FunStr}) ->
    {ok, MS} = string2ms(FunStr),
    MS;
ms(Other) ->
    Other.

ensure_no_overloaded_nodes() ->
    Overloaded = case whereis(?MODULE) of
                     undefined ->
                         [];
                     _ ->
                         ?MODULE ! {get_overloaded, self()},
                         receive {overloaded,O} -> O end
                 end,
    case Overloaded of
        [] -> ok;
        Overloaded -> exit({error, overload_protection_active, Overloaded})
    end.

-spec string2ms(string()) -> {ok, list()} | {error, fun_format}.
string2ms(FunStr) ->
    case erl_scan:string(fix_dot(FunStr)) of
	{ok, Tokens, _} ->
	    case erl_parse:parse_exprs(Tokens) of
		{ok, [Expression]} ->
		    case Expression of
			{_, _, {clauses, Clauses}} ->
			    {ok, ms_transform:transform_from_shell(dbg, Clauses, [])};
			_ ->
			    {error, fun_format}
		    end;
		_ ->
		    {error, fun_format}
	    end;
	_ ->{error, fun_format}
    end.

-spec fix_dot(string()) -> string().
fix_dot(FunStr) ->
    [H | Rest]  = lists:reverse(FunStr),
    case H of
	$. ->
	    FunStr;
	H ->
	    lists:reverse([$., H | Rest])
    end.

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
stop(Opts) when is_list(Opts) ->
    Fetch = stop_opts(Opts),
    Result =
        case whereis(?MODULE) of
            undefined -> ok;
            Pid when is_pid(Pid) ->
                ?MODULE ! {stop,Fetch,self()},
                receive {?MODULE,R} -> R end
        end,
    case {Fetch, Result} of
        {nofetch, _} ->
            ok;
        {_, {stopped, _}} ->
            %% Printout moved out of the ttb loop to avoid occasional deadlock
            io:format("Stored logs in ~ts~n", [element(2, Result)]);
        {_, _} ->
            ok
    end,
    stop_return(Result,Opts);
stop(Opts) ->
    stop([Opts]).

stop_opts(Opts) ->
    FetchDir = proplists:get_value(fetch_dir, Opts),
    ensure_fetch_dir(FetchDir),
    FormatData = case proplists:get_value(format, Opts) of
                     undefined -> false;
                     true ->      {format, []};
                     FOpts ->     {format, FOpts}
                 end,
    case {FormatData, lists:member(return_fetch_dir, Opts)} of
	{false, true} ->
	    {fetch, FetchDir}; % if we specify return_fetch_dir, the data should be fetched
	{false, false} ->
	    case lists:member(nofetch,Opts) of
		    false -> {fetch, FetchDir};
		    true -> nofetch
	    end;
    {FormatData, _} ->
            {FormatData, FetchDir}
    end.

ensure_fetch_dir(undefined) -> ok;
ensure_fetch_dir(Dir) ->
    case filelib:is_file(Dir) of
	true ->
	    throw({error, exists, Dir});
	false ->
	   ok
    end.

stop_return(R,Opts) ->
    case {lists:member(return_fetch_dir,Opts),R} of
        {true,_} ->
            R;
        {false,{stopped,_}} ->
            stopped;
        {false,_} ->
            %% Anything other than 'stopped' would not be bw compatible...
            stopped
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Process implementation
start(SessionInfo) ->
    case whereis(?MODULE) of
	undefined ->
	    Parent = self(),
	    Pid = spawn(fun() -> init(Parent, SessionInfo) end),
	    receive {started,Pid} -> ok end,
            Pid;
	Pid when is_pid(Pid) ->
	    Pid
    end.

init(Parent, SessionInfo) ->
    register(?MODULE,self()),
    ets:new(?history_table,[ordered_set,named_table,public]),
    Parent ! {started,self()},
    NewSessionInfo = [{partials, 0}, {dead_nodes, []} | SessionInfo],
    try_send_flush_tick(NewSessionInfo),
    loop(dict:new(), NewSessionInfo).

loop(NodeInfo, SessionInfo) ->
    receive 
	{init_node,Node,MetaFile,PI,Traci} ->
	    erlang:monitor_node(Node,true),
	    {AbsoluteMetaFile, MetaPid} =
		case rpc:call(Node,
			      observer_backend,
			      ttb_init_node,
			      [MetaFile,PI,Traci]) of
		    {ok,MF,MP} ->
			{MF,MP};
		    {badrpc,nodedown} ->
			%% We will get a nodedown message
			{MetaFile,undefined}
		end,
	    loop(dict:store(Node,{AbsoluteMetaFile,MetaPid},NodeInfo), SessionInfo);
	{ip_to_file_trace_port,Port,Sender} ->
	    Ports = proplists:get_value(ip_to_file_trace_ports, SessionInfo, []),
	    NewSessionInfo = [{ip_to_file_trace_ports,[Port|Ports]}|SessionInfo],
	    Sender ! {?MODULE,ok},
	    loop(NodeInfo, NewSessionInfo);
	{get_nodes,Sender} ->
	    Sender ! {?MODULE,dict:fetch_keys(NodeInfo)},
	    loop(NodeInfo, SessionInfo);
	{write_trace_info,Key,What} ->
	    dict:fold(fun(Node,{_MetaFile,MetaPid},_) -> 
			      rpc:call(Node,observer_backend,
				       ttb_write_trace_info,[MetaPid,Key,What])
		      end,
		      ok,
		      NodeInfo),
	    loop(NodeInfo, SessionInfo);
	{nodedown,Node} ->
            NewState = make_node_dead(Node, NodeInfo, SessionInfo),
	    loop(dict:erase(Node,NodeInfo), NewState);
        {noderesumed,Node,Reporter} ->
            {MetaFile, CurrentSuffix, NewState} = make_node_alive(Node, SessionInfo),
            fetch_partial_result(Node, MetaFile, CurrentSuffix),
            spawn(fun() -> resume_trace(Reporter) end),
            loop(NodeInfo, NewState);
	{timeout, StopOpts} ->
	    spawn(?MODULE, stop, [StopOpts]),
	    loop(NodeInfo, SessionInfo);
        {node_overloaded, Node} ->
            io:format("Overload check activated on node: ~p.~n", [Node]),
            {Overloaded, SI} = {proplists:get_value(overloaded, SessionInfo, []),
                                lists:keydelete(overloaded, 1, SessionInfo)},
	    loop(NodeInfo, [{overloaded, [Node|Overloaded]} | SI]);
        {get_overloaded, Pid} ->
            Pid ! {overloaded,proplists:get_value(overloaded, SessionInfo, [])},
            loop(NodeInfo, SessionInfo);
	trace_started ->
	    case proplists:get_value(timer, SessionInfo) of
		undefined -> ok;
		{MSec, StopOpts}  -> erlang:send_after(MSec, self(), {timeout, StopOpts})
	    end,
	    loop(NodeInfo, SessionInfo);
        flush_timeout ->
            [ dbg:flush_trace_port(Node) || Node <- dict:fetch_keys(NodeInfo) ],
            try_send_flush_tick(SessionInfo),
            loop(NodeInfo, SessionInfo);
	{stop,nofetch,Sender} ->
            do_stop(nofetch, Sender, NodeInfo, SessionInfo);
	{stop,FetchSpec,Sender} ->
            case proplists:get_value(shell, SessionInfo, false) of
                only -> do_stop(nofetch, Sender, NodeInfo, SessionInfo);
                _    -> do_stop(FetchSpec, Sender, NodeInfo, SessionInfo)
            end
    end.

do_stop(nofetch, Sender, NodeInfo, SessionInfo) ->
    write_config(?last_config, all),
    dict:fold(
      fun(Node,{_,MetaPid},_) ->
              rpc:call(Node,observer_backend,ttb_stop,[MetaPid])
      end,
      ok,
      NodeInfo),
    stop_ip_to_file_trace_ports(SessionInfo),
    dbg:stop_clear(),
    ets:delete(?history_table),
    Sender ! {?MODULE, stopped};

do_stop({FetchOrFormat, UserDir}, Sender, NodeInfo, SessionInfo) ->
    write_config(?last_config, all),
    Localhost = host(node()),
    Dir = get_fetch_dir(UserDir, proplists:get_value(logfile, SessionInfo)),
    ok = filelib:ensure_dir(filename:join(Dir,"*")),
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
    stop_ip_to_file_trace_ports(SessionInfo),
    dbg:stop_clear(),
    AllNodes =
        lists:map(
          fun({Node,MetaFile}) ->
                  spawn(fun() -> fetch_report(Localhost,Dir,Node,MetaFile) end),
                  Node
          end,
          AllNodesAndMeta),
    ets:delete(?history_table),
    wait_for_fetch(AllNodes),
    copy_partials(Dir, proplists:get_value(partials, SessionInfo)),
    Absname = filename:absname(Dir),
    case FetchOrFormat of
        fetch -> ok;
        {format, Opts} -> format(Dir, Opts)
    end,
    Sender ! {?MODULE,{stopped,Absname}}.

stop_ip_to_file_trace_ports(SessionInfo) ->
    lists:foreach(fun(Port) ->
			  case lists:member(Port,erlang:ports()) of
			      true ->
				  dbg:deliver_and_flush(Port),
				  erlang:port_close(Port);
			      false ->
				  ok
			  end
		  end,
		  proplists:get_value(ip_to_file_trace_ports,SessionInfo,[])).


make_node_dead(Node, NodeInfo, SessionInfo) ->
    {MetaFile,_} = dict:fetch(Node, NodeInfo),
    NewDeadNodes = [{Node, MetaFile} | proplists:get_value(dead_nodes, SessionInfo)],
    [{dead_nodes, NewDeadNodes} | lists:keydelete(dead_nodes, 1, SessionInfo)].

make_node_alive(Node, SessionInfo) ->
            DeadNodes = proplists:get_value(dead_nodes, SessionInfo),
            Partials = proplists:get_value(partials, SessionInfo),
            {value, {_, MetaFile}, Dn2} = lists:keytake(Node, 1, DeadNodes),
            SessionInfo2 = lists:keyreplace(dead_nodes, 1, SessionInfo, {dead_nodes, Dn2}),
            {MetaFile, Partials + 1, lists:keyreplace(partials, 1, SessionInfo2, {partials, Partials + 1})}.

try_send_flush_tick(State) ->
    case proplists:get_value(flush, State) of
        undefined ->
            ok;
        MSec ->
            erlang:send_after(MSec, self(), flush_timeout)
    end.

get_fetch_dir(undefined,undefined) -> ?upload_dir(?MODULE_STRING) ++ ts();
get_fetch_dir(undefined,Logname) -> ?upload_dir(Logname) ++ ts();
get_fetch_dir(Dir,_) -> Dir.

resume_trace(Reporter) ->
    ?MODULE:run_history(all_silent),
    Reporter ! trace_resumed.

get_nodes() ->
    ?MODULE ! {get_nodes,self()},
    receive {?MODULE,Nodes} -> Nodes end.

ts() ->
    {{Y,M,D},{H,Min,S}} = calendar:now_to_local_time(erlang:timestamp()),
    io_lib:format("-~4.4.0w~2.2.0w~2.2.0w-~2.2.0w~2.2.0w~2.2.0w",
		  [Y,M,D,H,Min,S]).

copy_partials(_, 0) ->
    ok;
copy_partials(Dir, Num) ->
    PartialDir = ?partial_dir ++ integer_to_list(Num),
    file:rename(PartialDir, filename:join(Dir,PartialDir)),
    copy_partials(Dir, Num - 1).

fetch_partial_result(Node,MetaFile,Current) ->
    DirName = ?partial_dir ++ integer_to_list(Current),
    case file:list_dir(DirName) of
        {error, enoent} ->
            ok;
        {ok, Files} ->
            [ file:delete(filename:join(DirName, File)) || File <- Files ],
            file:del_dir(DirName)
    end,
    file:make_dir(DirName),
    fetch(host(node()), DirName, Node, MetaFile).

fetch_report(Localhost, Dir, Node, MetaFile) ->
    fetch(Localhost,Dir,Node,MetaFile),
    ?MODULE ! {fetch_complete,Node}.

fetch(Localhost,Dir,Node,MetaFile) ->
    case (host(Node) == Localhost) orelse is_local(MetaFile) of
        true -> % same host, just move the files
	    Files = get_filenames(Node,MetaFile),
	    lists:foreach(
              fun(File0) ->
                      Dest = filename:join(Dir,filename:basename(File0)),
                      file:rename(File0, Dest)
              end,
              Files);
	false ->
	    {ok, LSock} = gen_tcp:listen(0, [binary,{packet,2},{active,false}]),
	    {ok,Port} = inet:port(LSock),
            Enc = file:native_name_encoding(),
            Args =
                case rpc:call(Node,erlang,function_exported,
                              [observer_backend,ttb_fetch,3]) of
                    true ->
                        [MetaFile,{Port,Localhost},Enc];
                    false ->
                        [MetaFile,{Port,Localhost}]
                end,
            rpc:cast(Node,observer_backend,ttb_fetch,Args),
	    {ok, Sock} = gen_tcp:accept(LSock),
	    receive_files(Dir,Sock,undefined,Enc),
	    ok = gen_tcp:close(LSock),
	    ok = gen_tcp:close(Sock)
    end.

is_local({local, _, _}) ->
    true;
is_local(_) ->
    false.

get_filenames(_N, {local,F,_}) ->
    observer_backend:ttb_get_filenames(F);
get_filenames(N, F) ->
    rpc:call(N, observer_backend,ttb_get_filenames,[F]).

receive_files(Dir,Sock,Fd,Enc) ->
    case gen_tcp:recv(Sock, 0) of
	{ok, <<0,Bin/binary>>} ->
	    file:write(Fd,Bin),
	    receive_files(Dir,Sock,Fd,Enc);
	{ok, <<Code,Bin/binary>>} when Code==1; Code==2; Code==3 ->
            File0 = decode_filename(Code,Bin,Enc),
	    File = filename:join(Dir,File0),
	    {ok,Fd1} = file:open(File,[raw,write]),
	    receive_files(Dir,Sock,Fd1,Enc);
	{error, closed} ->
	    ok = file:close(Fd)
    end.    

decode_filename(1,Bin,_Enc) ->
    %% Old version of observer_backend - filename encoded with
    %% list_to_binary
    binary_to_list(Bin);
decode_filename(2,Bin,Enc) ->
    %% Successfully encoded filename with correct encoding
    unicode:characters_to_list(Bin,Enc);
decode_filename(3,Bin,latin1) ->
    %% Filename encoded with faulty encoding. This has to be utf8
    %% remote and latin1 here, and the filename actually containing
    %% characters outside the latin1 range. So making an escaped
    %% variant of the filename and warning about it.
    File0 = unicode:characters_to_list(Bin,utf8),
    File = [ case X of
                     High when High > 255 ->
                         ["\\\\x{",erlang:integer_to_list(X, 16),$}];
                     Low ->
                         Low
                 end || X <- File0 ],
    io:format("Warning: fetching file with faulty filename encoding ~ts~n"
              "Will be written as ~ts~n",
              [File0,File]),
    File.

host(Node) ->
    [_name,Host] = string:lexemes(atom_to_list(Node),"@"),
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
    {ok, Cwd} = file:get_cwd(),
    lists:foreach(fun({N,{local,C,_},F}) ->
			  MetaFile = case F of
                                         none ->
                                             none;
                                         F ->
                                             AbsFile = filename:join(Cwd, F) ++ ".ti",
                                             file:delete(AbsFile),
                                             AbsFile
                                     end,
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
get_et_handler() ->
    {fun ttb_et:handler/4, initial}.

format(Files) ->
    format(Files,[]).
format(Files,Opt) ->
    {Out,Handler,DisableSort} = format_opt(Opt),
    ets:new(?MODULE,[named_table]),
    format(Files,Out,Handler, DisableSort).
format(File,Out,Handler,DisableSort) when is_list(File), is_integer(hd(File)) ->
    Files = 
	case filelib:is_dir(File) of
	    true ->  % will merge all files in the directory
                List = filelib:wildcard(filename:join(File, ?partial_dir++"*")),
                lists:append(collect_files([File | List]));
	    false -> % format one file
		[File]
	end,
    format(Files,Out,Handler,DisableSort);
format(Files,Out,Handler,DisableSort) when is_list(Files), is_list(hd(Files)) ->
    StopDbg = case whereis(dbg) of
		  undefined -> true;
		  _ -> false
	      end,
    Details = lists:foldl(fun(File,Acc) -> [prepare(File)|Acc] end,
			  [],Files),
    Fd = get_fd(Out),
    RealHandler = get_handler(Handler, Files),
    R = do_format(Fd,Details,DisableSort,RealHandler),
    file:close(Fd),
    ets:delete(?MODULE),
    case StopDbg of
	true -> dbg:stop_clear();
	false -> ok
    end,
    R.

collect_files(Dirs) ->
    lists:map(fun(Dir) ->
                      MetaFiles = filelib:wildcard(filename:join(Dir,"*.ti")),
                      lists:map(fun(M) ->
                                        Sub = filename:rootname(M,".ti"),
                                        case filelib:is_file(Sub) of
                                            true -> Sub;
                                            false -> Sub++".*.wrp"
                                        end
                                end,
                                MetaFiles)
              end, Dirs).

get_handler(undefined, Files) ->
    %%We retrieve traci from the first available file
    {Traci, _} = read_traci(hd(Files)),
    case dict:find(handler, Traci) of
        error             -> {fun defaulthandler/4, initial};
        {ok, [Handler]}   -> Handler
    end;
get_handler(Handler, _) ->
    Handler.

prepare(File) ->
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
    {FileOrWrap,Traci}.

format_opt(Opt) when is_list(Opt) ->
    Out = case lists:keysearch(out,1,Opt) of
	      {value,{out,O}} -> O;
	      _ -> standard_io
	  end,
    Handler = case lists:keysearch(handler,1,Opt) of
                  {value,{handler,H}} -> H;
                  _ -> undefined
	  end,
    DisableSort = proplists:get_value(disable_sort, Opt, false),
    {Out,Handler,DisableSort};
format_opt(Opt) ->
    format_opt([Opt]).


read_traci(File) ->
    MetaFile = get_metafile(File),
    case file:read_file(MetaFile) of
	{ok,B} -> 
	    interpret_binary(B,dict:new(),[]);
	_ -> 
	    io:format("Warning: no meta data file: ~ts~n",[MetaFile]),
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
	    case file:open(Out,[append,{encoding,utf8}]) of
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


do_format(Fd,Details,DisableSort,Handler) ->
    Clients = lists:foldl(fun({FileOrWrap,Traci},Acc) ->
                                  [start_client(FileOrWrap,Traci)|Acc]
			  end,[],Details),
    init_collector(Fd,Clients,DisableSort,Handler).

start_client(FileOrWrap,Traci) ->
    dbg:trace_client(file, FileOrWrap,
		     {fun handler/2, dict:to_list(Traci)}).

handler(Trace,Traci) ->
    %%We return our own Traci so that it not necesarry to look it up
    %%This may take time if something huge has been written to it
    receive
	{get,Collector} -> Collector ! {self(),{Trace,Traci}};
	done -> ok
    end,
    Traci.

%%Used to handle common state (the same for all clients)
handler2(Trace,{Fd,Traci,{Fun,State}}) when is_function(Fun) ->
    {Fun, Fun(Fd, Trace, Traci, State)};
handler2(Trace,{Fd,Traci,{{M,F},State}}) when is_atom(M), is_atom(F) ->
    {{M,F}, M:F(Fd, Trace, Traci, State)}.

defaulthandler(Fd,Trace,_Traci,initial) ->
    dbg:dhandler(Trace,Fd);
defaulthandler(_Fd,Trace,_Traci,State) ->
    dbg:dhandler(Trace,State).

init_collector(Fd,Clients,DisableSort,Handler) ->
    Collected = get_first(Clients),
    case DisableSort of
        true -> collector(Fd,Collected, DisableSort, Handler);
        false -> collector(Fd,sort(Collected), DisableSort, Handler)
    end.

collector(Fd,[{_,{Client,{Trace,Traci}}} |Rest], DisableSort, CommonState) ->
    Trace1 = update_procinfo(Trace),
    CommonState2 = handler2(Trace1, {Fd, Traci, CommonState}),
    case get_next(Client) of
	end_of_trace ->
	    collector(Fd,Rest,DisableSort, CommonState2);
	Next -> case DisableSort of
                    false -> collector(Fd,sort([Next|Rest]), DisableSort, CommonState2);
                    true -> collector(Fd,[Next|Rest], DisableSort, CommonState2)
                end
    end;
collector(Fd,[], _, CommonState) ->
    handler2(end_of_trace, {Fd, end_of_trace, CommonState}),
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

get_procinfo(Pid) when is_pid(Pid); is_port(Pid) ->
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
	{Client,{Trace,_}}=Next ->
	    [{timestamp(Trace),Next}|get_first(Clients)]
    end;
get_first([]) -> [].

get_next(Client) when is_pid(Client) ->
    Client ! {get,self()},
    receive
	{Client,{end_of_trace,_}} ->
	    end_of_trace;
	{Client,{Trace, Traci}} ->
	    {timestamp(Trace),{Client,{Trace,Traci}}}
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
    io:format("Warning: {~tw,~tw}~n",[Warning,Item]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Trace client which reads an IP port and puts data directly to a file.
%%% This is used when tracing remote nodes with no file system.
ip_to_file({metadata,_,_},{shell_only, _} = State) ->
    State;
ip_to_file(Trace, {shell_only, Fun} = State) ->
    Fun(Trace),
    State;
ip_to_file(Trace,{{file,File}, ShellOutput}) ->
    Fun = dbg:trace_port(file,File), %File can be a filename or a wrap spec
    Port = Fun(),
    %% Just in case this is on the traced node,
    %% make sure the port is not traced.
    p(Port,clear),
    %% Store the port so it can be properly closed
    ?MODULE ! {ip_to_file_trace_port, Port, self()},
    receive {?MODULE,ok} -> ok end,
    case Trace of
        {metadata, _, _} -> ok;
        Trace            -> show_trace(Trace, ShellOutput)
    end,
    ip_to_file(Trace,{Port,ShellOutput});
ip_to_file({metadata,MetaFile,MetaData},State) ->
    {ok,MetaFd} = file:open(MetaFile,[write,raw,append]),
    file:write(MetaFd,MetaData),
    file:close(MetaFd),
    State;
ip_to_file(Trace,{Port, ShellOutput}) ->
    show_trace(Trace, ShellOutput),
    B = term_to_binary(Trace),
    erlang:port_command(Port,B),
    {Port, ShellOutput}.

show_trace(Trace, Fun) when is_function(Fun) ->
    Fun(Trace);
show_trace(_, _) ->
    ok.

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
