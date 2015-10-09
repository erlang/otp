%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2014. All Rights Reserved.
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
-module(proc_lib).

%% This module is used to set some initial information
%% in each created process. 
%% Then a process terminates the Reason is checked and
%% a crash report is generated if the Reason was not expected.

-export([spawn/1, spawn_link/1, spawn/2, spawn_link/2,
         spawn/3, spawn_link/3, spawn/4, spawn_link/4,
         spawn_opt/2, spawn_opt/3, spawn_opt/4, spawn_opt/5,
	 start/3, start/4, start/5, start_link/3, start_link/4, start_link/5,
	 hibernate/3,
	 init_ack/1, init_ack/2,
	 init_p/3,init_p/5,format/1,format/2,format/3,
	 initial_call/1,
         translate_initial_call/1,
	 stop/1, stop/3]).

%% Internal exports.
-export([wake_up/3]).

-export_type([spawn_option/0]).

%%-----------------------------------------------------------------------------

-type priority_level() :: 'high' | 'low' | 'max' | 'normal'.
-type spawn_option()   :: 'link'
                        | 'monitor'
                        | {'priority', priority_level()}
                        | {'min_heap_size', non_neg_integer()}
                        | {'min_bin_vheap_size', non_neg_integer()}
                        | {'fullsweep_after', non_neg_integer()}.

-type dict_or_pid()    :: pid()
                        | (ProcInfo :: [_])
                        | {X :: integer(), Y :: integer(), Z :: integer()}.

%%-----------------------------------------------------------------------------

-spec spawn(Fun) -> pid() when
      Fun :: function().

spawn(F) when is_function(F) ->
    Parent = get_my_name(),
    Ancestors = get_ancestors(),
    erlang:spawn(?MODULE, init_p, [Parent,Ancestors,F]).

-spec spawn(Module, Function, Args) -> pid() when
      Module :: module(),
      Function :: atom(),
      Args :: [term()].

spawn(M,F,A) when is_atom(M), is_atom(F), is_list(A) ->
    Parent = get_my_name(),
    Ancestors = get_ancestors(),
    erlang:spawn(?MODULE, init_p, [Parent,Ancestors,M,F,A]).

-spec spawn_link(Fun) -> pid() when
      Fun :: function().

spawn_link(F) when is_function(F) ->
    Parent = get_my_name(),
    Ancestors = get_ancestors(),
    erlang:spawn_link(?MODULE, init_p, [Parent,Ancestors,F]).

-spec spawn_link(Module, Function, Args) -> pid() when
      Module :: module(),
      Function :: atom(),
      Args :: [term()].

spawn_link(M,F,A) when is_atom(M), is_atom(F), is_list(A) ->
    Parent = get_my_name(),
    Ancestors = get_ancestors(),
    erlang:spawn_link(?MODULE, init_p, [Parent,Ancestors,M,F,A]).

-spec spawn(Node, Fun) -> pid() when
      Node :: node(),
      Fun :: function().

spawn(Node, F) when is_function(F) ->
    Parent = get_my_name(),
    Ancestors = get_ancestors(),
    erlang:spawn(Node, ?MODULE, init_p, [Parent,Ancestors,F]).

-spec spawn(Node, Module, Function, Args) -> pid() when
      Node :: node(),
      Module :: module(),
      Function :: atom(),
      Args :: [term()].

spawn(Node, M, F, A) when is_atom(M), is_atom(F), is_list(A) ->
    Parent = get_my_name(),
    Ancestors = get_ancestors(),
    erlang:spawn(Node, ?MODULE, init_p, [Parent,Ancestors,M,F,A]).

-spec spawn_link(Node, Fun) -> pid() when
      Node :: node(),
      Fun :: function().

spawn_link(Node, F) when is_function(F) ->
    Parent = get_my_name(),
    Ancestors = get_ancestors(),
    erlang:spawn_link(Node, ?MODULE, init_p, [Parent,Ancestors,F]).

-spec spawn_link(Node, Module, Function, Args) -> pid() when
      Node :: node(),
      Module :: module(),
      Function :: atom(),
      Args :: [term()].

spawn_link(Node, M, F, A) when is_atom(M), is_atom(F), is_list(A) ->
    Parent = get_my_name(),
    Ancestors = get_ancestors(),
    erlang:spawn_link(Node, ?MODULE, init_p, [Parent,Ancestors,M,F,A]).

-spec spawn_opt(Fun, SpawnOpts) -> pid() when
      Fun :: function(),
      SpawnOpts :: [spawn_option()].

spawn_opt(F, Opts) when is_function(F) ->
    Parent = get_my_name(),
    Ancestors = get_ancestors(),
    check_for_monitor(Opts),
    erlang:spawn_opt(?MODULE, init_p, [Parent,Ancestors,F],Opts).

-spec spawn_opt(Node, Function, SpawnOpts) -> pid() when
      Node :: node(),
      Function :: function(),
      SpawnOpts :: [spawn_option()].

spawn_opt(Node, F, Opts) when is_function(F) ->
    Parent = get_my_name(),
    Ancestors = get_ancestors(),
    check_for_monitor(Opts),
    erlang:spawn_opt(Node, ?MODULE, init_p, [Parent,Ancestors,F], Opts).

-spec spawn_opt(Module, Function, Args, SpawnOpts) -> pid() when
      Module :: module(),
      Function :: atom(),
      Args :: [term()],
      SpawnOpts :: [spawn_option()].

spawn_opt(M, F, A, Opts) when is_atom(M), is_atom(F), is_list(A) ->
    Parent = get_my_name(),
    Ancestors = get_ancestors(),
    check_for_monitor(Opts),
    erlang:spawn_opt(?MODULE, init_p, [Parent,Ancestors,M,F,A], Opts).

-spec spawn_opt(Node, Module, Function, Args, SpawnOpts) -> pid() when
      Node :: node(),
      Module :: module(),
      Function :: atom(),
      Args :: [term()],
      SpawnOpts :: [spawn_option()].

spawn_opt(Node, M, F, A, Opts) when is_atom(M), is_atom(F), is_list(A) ->
    Parent = get_my_name(),
    Ancestors = get_ancestors(),
    check_for_monitor(Opts),
    erlang:spawn_opt(Node, ?MODULE, init_p, [Parent,Ancestors,M,F,A], Opts).

%% OTP-6345
%% monitor spawn_opt option is currently not possible to use
check_for_monitor(SpawnOpts) ->
    case lists:member(monitor, SpawnOpts) of
	true ->
	    erlang:error(badarg);
	false ->
	    false
    end.

spawn_mon(M,F,A) ->
    Parent = get_my_name(),
    Ancestors = get_ancestors(),
    erlang:spawn_monitor(?MODULE, init_p, [Parent,Ancestors,M,F,A]).

spawn_opt_mon(M, F, A, Opts) when is_atom(M), is_atom(F), is_list(A) ->
    Parent = get_my_name(),
    Ancestors = get_ancestors(),
    check_for_monitor(Opts),
    erlang:spawn_opt(?MODULE, init_p, [Parent,Ancestors,M,F,A], [monitor|Opts]).

-spec hibernate(Module, Function, Args) -> no_return() when
      Module :: module(),
      Function :: atom(),
      Args :: [term()].

hibernate(M, F, A) when is_atom(M), is_atom(F), is_list(A) ->
    erlang:hibernate(?MODULE, wake_up, [M, F, A]).

ensure_link(SpawnOpts) ->
    case lists:member(link, SpawnOpts) of
	true -> 
	    SpawnOpts;
	false ->
	    [link|SpawnOpts]
    end.

-spec init_p(pid(), [pid()], function()) -> term().

init_p(Parent, Ancestors, Fun) when is_function(Fun) ->
    put('$ancestors', [Parent|Ancestors]),
    Mfa = erlang:fun_info_mfa(Fun),
    put('$initial_call', Mfa),
    try
	Fun()
    catch
	Class:Reason ->
	    exit_p(Class, Reason)
    end.

-spec init_p(pid(), [pid()], atom(), atom(), [term()]) -> term().

init_p(Parent, Ancestors, M, F, A) when is_atom(M), is_atom(F), is_list(A) ->
    put('$ancestors', [Parent|Ancestors]),
    put('$initial_call', trans_init(M, F, A)),
    init_p_do_apply(M, F, A).

init_p_do_apply(M, F, A) ->
    try
	apply(M, F, A) 
    catch
	Class:Reason ->
	    exit_p(Class, Reason)
    end.

-spec wake_up(atom(), atom(), [term()]) -> term().

wake_up(M, F, A) when is_atom(M), is_atom(F), is_list(A) ->
    try
	apply(M, F, A) 
    catch
	Class:Reason ->
	    exit_p(Class, Reason)
    end.

exit_p(Class, Reason) ->
    case get('$initial_call') of
	{M,F,A} when is_atom(M), is_atom(F), is_integer(A) ->
	    MFA = {M,F,make_dummy_args(A, [])},
	    crash_report(Class, Reason, MFA),
	    exit(Reason);
	_ ->
	    %% The process dictionary has been cleared or
	    %% possibly modified.
	    crash_report(Class, Reason, []),
	    exit(Reason)
    end.

-spec start(Module, Function, Args) -> Ret when
      Module :: module(),
      Function :: atom(),
      Args :: [term()],
      Ret :: term() | {error, Reason :: term()}.

start(M, F, A) when is_atom(M), is_atom(F), is_list(A) ->
    start(M, F, A, infinity).

-spec start(Module, Function, Args, Time) -> Ret when
      Module :: module(),
      Function :: atom(),
      Args :: [term()],
      Time :: timeout(),
      Ret :: term() | {error, Reason :: term()}.

start(M, F, A, Timeout) when is_atom(M), is_atom(F), is_list(A) ->
    PidRef = spawn_mon(M, F, A),
    sync_wait_mon(PidRef, Timeout).

-spec start(Module, Function, Args, Time, SpawnOpts) -> Ret when
      Module :: module(),
      Function :: atom(),
      Args :: [term()],
      Time :: timeout(),
      SpawnOpts :: [spawn_option()],
      Ret :: term() | {error, Reason :: term()}.

start(M, F, A, Timeout, SpawnOpts) when is_atom(M), is_atom(F), is_list(A) ->
    PidRef = spawn_opt_mon(M, F, A, SpawnOpts),
    sync_wait_mon(PidRef, Timeout).

-spec start_link(Module, Function, Args) -> Ret when
      Module :: module(),
      Function :: atom(),
      Args :: [term()],
      Ret :: term() | {error, Reason :: term()}.

start_link(M, F, A) when is_atom(M), is_atom(F), is_list(A) ->
    start_link(M, F, A, infinity).

-spec start_link(Module, Function, Args, Time) -> Ret when
      Module :: module(),
      Function :: atom(),
      Args :: [term()],
      Time :: timeout(),
      Ret :: term() | {error, Reason :: term()}.

start_link(M, F, A, Timeout) when is_atom(M), is_atom(F), is_list(A) ->
    Pid = ?MODULE:spawn_link(M, F, A),
    sync_wait(Pid, Timeout).

-spec start_link(Module, Function, Args, Time, SpawnOpts) -> Ret when
      Module :: module(),
      Function :: atom(),
      Args :: [term()],
      Time :: timeout(),
      SpawnOpts :: [spawn_option()],
      Ret :: term() | {error, Reason :: term()}.

start_link(M,F,A,Timeout,SpawnOpts) when is_atom(M), is_atom(F), is_list(A) ->
    Pid = ?MODULE:spawn_opt(M, F, A, ensure_link(SpawnOpts)),
    sync_wait(Pid, Timeout).

sync_wait(Pid, Timeout) ->
    receive
	{ack, Pid, Return} ->
	    Return;
	{'EXIT', Pid, Reason} ->
	    {error, Reason}
    after Timeout ->
	    unlink(Pid),
	    exit(Pid, kill),
	    flush(Pid),
	    {error, timeout}
    end.

sync_wait_mon({Pid, Ref}, Timeout) ->
    receive
	{ack, Pid, Return} ->
	    erlang:demonitor(Ref, [flush]),
	    Return;
	{'DOWN', Ref, _Type, Pid, Reason} ->
	    {error, Reason};
	{'EXIT', Pid, Reason} -> %% link as spawn_opt?
	    erlang:demonitor(Ref, [flush]),
	    {error, Reason}
    after Timeout ->
	    erlang:demonitor(Ref, [flush]),
	    exit(Pid, kill),
	    flush(Pid),
	    {error, timeout}
    end.

-spec flush(pid()) -> 'true'.

flush(Pid) ->
    receive
	{'EXIT', Pid, _} ->
	    true
    after 0 ->
	    true
    end.

-spec init_ack(Parent, Ret) -> 'ok' when
      Parent :: pid(),
      Ret :: term().

init_ack(Parent, Return) ->
    Parent ! {ack, self(), Return},
    ok.

-spec init_ack(Ret) -> 'ok' when
      Ret :: term().

init_ack(Return) ->
    [Parent|_] = get('$ancestors'),
    init_ack(Parent, Return).

%% -----------------------------------------------------
%% Fetch the initial call of a proc_lib spawned process.
%% -----------------------------------------------------

-spec initial_call(Process) -> {Module, Function, Args} | 'false' when
      Process :: dict_or_pid(),
      Module :: module(),
      Function :: atom(),
      Args :: [atom()].

initial_call(DictOrPid) ->
    case raw_initial_call(DictOrPid) of
	{M,F,A} ->
	    {M,F,make_dummy_args(A, [])};
	false ->
	    false
    end.

make_dummy_args(0, Acc) ->
    Acc;
make_dummy_args(N, Acc) ->
    Arg = list_to_atom("Argument__" ++ integer_to_list(N)),
    make_dummy_args(N-1, [Arg|Acc]).

%% -----------------------------------------------------
%% Translate the '$initial_call' to some useful information.
%% However, the arguments are not returned here; only the
%% arity of the initial function.
%% This function is typically called from c:i() and c:regs().
%% -----------------------------------------------------

-spec translate_initial_call(Process) -> {Module, Function, Arity} when
      Process :: dict_or_pid(),
      Module :: module(),
      Function :: atom(),
      Arity :: byte().

translate_initial_call(DictOrPid) ->
    case raw_initial_call(DictOrPid) of
	{_,_,_}=MFA ->
	    MFA;
	false ->
	    {?MODULE,init_p,5}
    end.

%% -----------------------------------------------------
%% Fetch the initial call information exactly as stored
%% in the process dictionary.
%% -----------------------------------------------------

raw_initial_call({X,Y,Z}) when is_integer(X), is_integer(Y), is_integer(Z) ->
    raw_initial_call(c:pid(X,Y,Z));
raw_initial_call(Pid) when is_pid(Pid) ->
    case get_process_info(Pid, dictionary) of
	{dictionary,Dict} ->
	    raw_init_call(Dict);
	_ ->
	    false
    end;
raw_initial_call(ProcInfo) when is_list(ProcInfo) ->
    case lists:keyfind(dictionary, 1, ProcInfo) of
	{dictionary,Dict} ->
	    raw_init_call(Dict);
	_ ->
	    false
    end.

raw_init_call(Dict) ->
    case lists:keyfind('$initial_call', 1, Dict) of
	{_,{_,_,_}=MFA} ->
	    MFA;
	_ ->
	    false
    end.

%% -----------------------------------------------------
%% Translate the initial call to some useful information.
%% -----------------------------------------------------

trans_init(gen,init_it,[gen_server,_,_,supervisor,{_,Module,_},_]) ->
    {supervisor,Module,1};
trans_init(gen,init_it,[gen_server,_,_,_,supervisor,{_,Module,_},_]) ->
    {supervisor,Module,1};
trans_init(gen,init_it,[gen_server,_,_,supervisor_bridge,[Module|_],_]) ->
    {supervisor_bridge,Module,1};
trans_init(gen,init_it,[gen_server,_,_,_,supervisor_bridge,[Module|_],_]) ->
    {supervisor_bridge,Module,1};
trans_init(gen,init_it,[gen_server,_,_,Module,_,_]) ->
    {Module,init,1};
trans_init(gen,init_it,[gen_server,_,_,_,Module|_]) ->
    {Module,init,1};
trans_init(gen,init_it,[gen_fsm,_,_,Module,_,_]) ->
    {Module,init,1};
trans_init(gen,init_it,[gen_fsm,_,_,_,Module|_]) ->
    {Module,init,1};
trans_init(gen,init_it,[gen_event|_]) ->
    {gen_event,init_it,6};
trans_init(M, F, A) when is_atom(M), is_atom(F) ->
    {M,F,length(A)}.

%% -----------------------------------------------------
%% Generate a crash report.
%% -----------------------------------------------------

crash_report(exit, normal, _)       -> ok;
crash_report(exit, shutdown, _)     -> ok;
crash_report(exit, {shutdown,_}, _) -> ok;
crash_report(Class, Reason, StartF) ->
    OwnReport = my_info(Class, Reason, StartF),
    LinkReport = linked_info(self()),
    Rep = [OwnReport,LinkReport],
    error_logger:error_report(crash_report, Rep).

my_info(Class, Reason, []) ->
    my_info_1(Class, Reason);
my_info(Class, Reason, StartF) ->
    [{initial_call, StartF}|my_info_1(Class, Reason)].

my_info_1(Class, Reason) ->
    [{pid, self()},
     get_process_info(self(), registered_name),         
     {error_info, {Class,Reason,erlang:get_stacktrace()}}, 
     get_ancestors(self()),        
     get_process_info(self(), messages),
     get_process_info(self(), links),
     get_cleaned_dictionary(self()),
     get_process_info(self(), trap_exit),
     get_process_info(self(), status),
     get_process_info(self(), heap_size),
     get_process_info(self(), stack_size),
     get_process_info(self(), reductions)
    ].

-spec get_ancestors(pid()) -> {'ancestors', [pid()]}.

get_ancestors(Pid) ->
    case get_dictionary(Pid,'$ancestors') of
	{'$ancestors',Ancestors} ->
	    {ancestors,Ancestors};
	_ ->
	    {ancestors,[]}
    end.

get_cleaned_dictionary(Pid) ->
    case get_process_info(Pid,dictionary) of
	{dictionary,Dict} -> {dictionary,clean_dict(Dict)};
	_                 -> {dictionary,[]}
    end.

clean_dict([{'$ancestors',_}|Dict]) ->
    clean_dict(Dict);
clean_dict([{'$initial_call',_}|Dict]) ->
    clean_dict(Dict);
clean_dict([E|Dict]) ->
    [E|clean_dict(Dict)];
clean_dict([]) ->
    [].

get_dictionary(Pid,Tag) ->
    case get_process_info(Pid,dictionary) of
	{dictionary,Dict} ->
	    case lists:keysearch(Tag,1,Dict) of
		{value,Value} -> Value;
		_             -> undefined
	    end;
	_ ->
	    undefined
    end.

linked_info(Pid) ->
  make_neighbour_reports1(neighbours(Pid)).
  
make_neighbour_reports1([P|Ps]) ->
  ReportBody = make_neighbour_report(P),
  %%
  %%  Process P might have been deleted.
  %%
  case lists:member(undefined, ReportBody) of
    true ->
      make_neighbour_reports1(Ps);
    false ->
      [{neighbour, ReportBody}|make_neighbour_reports1(Ps)]
  end;
make_neighbour_reports1([]) ->
  [].
  
make_neighbour_report(Pid) ->
  [{pid, Pid},
   get_process_info(Pid, registered_name),          
   get_initial_call(Pid),
   get_process_info(Pid, current_function),
   get_ancestors(Pid),
   get_process_info(Pid, messages),
   get_process_info(Pid, links),
   get_cleaned_dictionary(Pid),
   get_process_info(Pid, trap_exit),
   get_process_info(Pid, status),
   get_process_info(Pid, heap_size),
   get_process_info(Pid, stack_size),
   get_process_info(Pid, reductions)
  ].
 
get_initial_call(Pid) ->
    case get_dictionary(Pid, '$initial_call') of
	{'$initial_call', {M, F, A}} ->
	    {initial_call, {M, F, make_dummy_args(A, [])}};
	_ ->
	    get_process_info(Pid, initial_call)
    end.

%%  neighbours(Pid) = list of Pids
%%
%%  Get the neighbours of Pid. A neighbour is a process which is 
%%  linked to Pid and does not trap exit; or a neigbour of a 
%%  neighbour etc.
%% 
%%  A breadth-first search is performed.

-spec neighbours(pid()) -> [pid()].

neighbours(Pid) ->
    {_, Visited} = visit(adjacents(Pid), {max_neighbours(), [Pid]}),
    lists:delete(Pid, Visited).

max_neighbours() -> 15.

%%
%% visit(Ps, {N, Vs}) = {N0, V0s}
%%
%% A breadth-first search of neighbours.
%%    Ps   processes,
%%    Vs   visited processes,
%%    N    max number to visit.
%%   
visit([P|Ps], {N, Vs} = NVs) when N > 0 ->
  case lists:member(P, Vs) of
    false -> visit(adjacents(P), visit(Ps, {N-1, [P|Vs]}));
    true  -> visit(Ps, NVs)
  end;
visit(_, {_N, _Vs} = NVs) ->
  NVs.

%%
%% adjacents(Pid) = AdjacencyList
%% 
-spec adjacents(pid()) -> [pid()].

adjacents(Pid) ->
  case catch proc_info(Pid, links) of
    {links, Links} -> no_trap(Links);
    _              -> []
  end.
  
no_trap([P|Ps]) ->
  case catch proc_info(P, trap_exit) of
    {trap_exit, false} -> [P|no_trap(Ps)];
    _                  -> no_trap(Ps)
  end;
no_trap([]) ->
  [].
 
get_process_info(Pid, Tag) ->
 translate_process_info(Tag, catch proc_info(Pid, Tag)).

translate_process_info(registered_name, []) ->
  {registered_name, []};
translate_process_info(_ , {'EXIT', _}) ->
  undefined;
translate_process_info(_, Result) ->
  Result.

%%% -----------------------------------------------------------
%%% Misc. functions
%%% -----------------------------------------------------------

get_my_name() ->
    case proc_info(self(),registered_name) of
	{registered_name,Name} -> Name;
	_                      -> self()
    end.

-spec get_ancestors() -> [pid()].

get_ancestors() ->
    case get('$ancestors') of
	A when is_list(A) -> A;
	_                 -> []
    end.

proc_info(Pid,Item) when node(Pid) =:= node() ->
    process_info(Pid,Item);
proc_info(Pid,Item) ->
    case lists:member(node(Pid),nodes()) of
	true ->
	    check(rpc:call(node(Pid), erlang, process_info, [Pid, Item]));
	_ ->
	    hidden
    end.

check({badrpc,nodedown}) -> undefined;
check({badrpc,Error})    -> Error;
check(Res)               -> Res.

%%% -----------------------------------------------------------
%%% Format (and write) a generated crash info structure.
%%% -----------------------------------------------------------

-spec format(CrashReport) -> string() when
      CrashReport :: [term()].
format(CrashReport) ->
    format(CrashReport, latin1).

-spec format(CrashReport, Encoding) -> string() when
      CrashReport :: [term()],
      Encoding :: latin1 | unicode | utf8.

format(CrashReport, Encoding) ->
    format(CrashReport, Encoding, unlimited).

-spec format(CrashReport, Encoding, Depth) -> string() when
      CrashReport :: [term()],
      Encoding :: latin1 | unicode | utf8,
      Depth :: unlimited | pos_integer().

format([OwnReport,LinkReport], Encoding, Depth) ->
    Extra = {Encoding,Depth},
    OwnFormat = format_report(OwnReport, Extra),
    LinkFormat = format_report(LinkReport, Extra),
    Str = io_lib:format("  crasher:~n~ts  neighbours:~n~ts",
                        [OwnFormat, LinkFormat]),
    lists:flatten(Str).

format_report(Rep, Extra) when is_list(Rep) ->
    format_rep(Rep, Extra);
format_report(Rep, {Enc,_}) ->
    io_lib:format("~"++modifier(Enc)++"p~n", [Rep]).

format_rep([{initial_call,InitialCall}|Rep], {_Enc,Depth}=Extra) ->
    [format_mfa(InitialCall, Depth)|format_rep(Rep, Extra)];
format_rep([{error_info,{Class,Reason,StackTrace}}|Rep], Extra) ->
    [format_exception(Class, Reason, StackTrace, Extra)|format_rep(Rep, Extra)];
format_rep([{Tag,Data}|Rep], Extra) ->
    [format_tag(Tag, Data, Extra)|format_rep(Rep, Extra)];
format_rep(_, _Extra) ->
    [].

format_exception(Class, Reason, StackTrace, {Enc,_}=Extra) ->
    PF = pp_fun(Extra),
    StackFun = fun(M, _F, _A) -> (M =:= erl_eval) or (M =:= ?MODULE) end,
    %% EI = "    exception: ",
    EI = "    ",
    [EI, lib:format_exception(1+length(EI), Class, Reason, 
                              StackTrace, StackFun, PF, Enc), "\n"].

format_mfa({M,F,Args}=StartF, Depth) ->
    try
	A = length(Args),
	["    initial call: ",atom_to_list(M),$:,atom_to_list(F),$/,
	 integer_to_list(A),"\n"]
    catch
	error:_ ->
	    format_tag(initial_call, StartF, Depth)
    end.

pp_fun({Enc,Depth}) ->
    {Letter,Tl} = case Depth of
		      unlimited -> {"p",[]};
		      _ -> {"P",[Depth]}
		  end,
    P = modifier(Enc) ++ Letter,
    fun(Term, I) -> 
            io_lib:format("~." ++ integer_to_list(I) ++ P, [Term|Tl])
    end.

format_tag(Tag, Data, {_Enc,Depth}) ->
    case Depth of
	unlimited ->
	    io_lib:format("    ~p: ~80.18p~n", [Tag, Data]);
	_ ->
	    io_lib:format("    ~p: ~80.18P~n", [Tag, Data, Depth])
    end.

modifier(latin1) -> "";
modifier(_) -> "t".


%%% -----------------------------------------------------------
%%% Stop a process and wait for it to terminate
%%% -----------------------------------------------------------
-spec stop(Process) -> 'ok' when
      Process :: pid() | RegName | {RegName,node()},
      RegName :: atom().
stop(Process) ->
    stop(Process, normal, infinity).

-spec stop(Process, Reason, Timeout) -> 'ok' when
      Process :: pid() | RegName | {RegName,node()},
      RegName :: atom(),
      Reason :: term(),
      Timeout :: timeout().
stop(Process, Reason, Timeout) ->
    {Pid, Mref} = erlang:spawn_monitor(do_stop(Process, Reason)),
    receive
	{'DOWN', Mref, _, _, Reason} ->
	    ok;
	{'DOWN', Mref, _, _, {noproc,{sys,terminate,_}}} ->
	    exit(noproc);
	{'DOWN', Mref, _, _, CrashReason} ->
	    exit(CrashReason)
    after Timeout ->
	    exit(Pid, kill),
	    receive
		{'DOWN', Mref, _, _, _} ->
		    exit(timeout)
	    end
    end.

-spec do_stop(Process, Reason) -> Fun when
      Process :: pid() | RegName | {RegName,node()},
      RegName :: atom(),
      Reason :: term(),
      Fun :: fun(() -> no_return()).
do_stop(Process, Reason) ->
    fun() ->
	    Mref = erlang:monitor(process, Process),
	    ok = sys:terminate(Process, Reason, infinity),
	    receive
		{'DOWN', Mref, _, _, ExitReason} ->
		    exit(ExitReason)
	    end
    end.
