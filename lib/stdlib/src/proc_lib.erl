%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2009. All Rights Reserved.
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
	 init_p/3,init_p/5,format/1,initial_call/1,translate_initial_call/1]).

%% Internal exports.
-export([wake_up/3]).

%%-----------------------------------------------------------------------------

-type priority_level() :: 'high' | 'low' | 'max' | 'normal'.
-type spawn_option()   :: 'link'
                        | {'priority', priority_level()}
                        | {'min_heap_size', non_neg_integer()}
                        | {'fullsweep_after', non_neg_integer()}.

-type dict_or_pid()    :: pid() | [_] | {integer(), integer(), integer()}.

%%-----------------------------------------------------------------------------

-spec spawn(function()) -> pid().

spawn(F) when is_function(F) ->
    Parent = get_my_name(),
    Ancestors = get_ancestors(),
    erlang:spawn(?MODULE, init_p, [Parent,Ancestors,F]).

-spec spawn(atom(), atom(), [term()]) -> pid().

spawn(M,F,A) when is_atom(M), is_atom(F), is_list(A) ->
    Parent = get_my_name(),
    Ancestors = get_ancestors(),
    erlang:spawn(?MODULE, init_p, [Parent,Ancestors,M,F,A]).

-spec spawn_link(function()) -> pid().

spawn_link(F) when is_function(F) ->
    Parent = get_my_name(),
    Ancestors = get_ancestors(),
    erlang:spawn_link(?MODULE, init_p, [Parent,Ancestors,F]).

-spec spawn_link(atom(), atom(), [term()]) -> pid().

spawn_link(M,F,A) when is_atom(M), is_atom(F), is_list(A) ->
    Parent = get_my_name(),
    Ancestors = get_ancestors(),
    erlang:spawn_link(?MODULE, init_p, [Parent,Ancestors,M,F,A]).

-spec spawn(node(), function()) -> pid().

spawn(Node, F) when is_function(F) ->
    Parent = get_my_name(),
    Ancestors = get_ancestors(),
    erlang:spawn(Node, ?MODULE, init_p, [Parent,Ancestors,F]).

-spec spawn(node(), atom(), atom(), [term()]) -> pid().
spawn(Node, M, F, A) when is_atom(M), is_atom(F), is_list(A) ->
    Parent = get_my_name(),
    Ancestors = get_ancestors(),
    erlang:spawn(Node, ?MODULE, init_p, [Parent,Ancestors,M,F,A]).

-spec spawn_link(node(), function()) -> pid().

spawn_link(Node, F) when is_function(F) ->
    Parent = get_my_name(),
    Ancestors = get_ancestors(),
    erlang:spawn_link(Node, ?MODULE, init_p, [Parent,Ancestors,F]).

-spec spawn_link(node(), atom(), atom(), [term()]) -> pid().

spawn_link(Node, M, F, A) when is_atom(M), is_atom(F), is_list(A) ->
    Parent = get_my_name(),
    Ancestors = get_ancestors(),
    erlang:spawn_link(Node, ?MODULE, init_p, [Parent,Ancestors,M,F,A]).

-spec spawn_opt(function(), [spawn_option()]) -> pid().
spawn_opt(F, Opts) when is_function(F) ->
    Parent = get_my_name(),
    Ancestors = get_ancestors(),
    check_for_monitor(Opts),
    erlang:spawn_opt(?MODULE, init_p, [Parent,Ancestors,F],Opts).

-spec spawn_opt(node(), function(), [spawn_option()]) -> pid().

spawn_opt(Node, F, Opts) when is_function(F) ->
    Parent = get_my_name(),
    Ancestors = get_ancestors(),
    check_for_monitor(Opts),
    erlang:spawn_opt(Node, ?MODULE, init_p, [Parent,Ancestors,F], Opts).

-spec spawn_opt(atom(), atom(), [term()], [spawn_option()]) -> pid().

spawn_opt(M, F, A, Opts) when is_atom(M), is_atom(F), is_list(A) ->
    Parent = get_my_name(),
    Ancestors = get_ancestors(),
    check_for_monitor(Opts),
    erlang:spawn_opt(?MODULE, init_p, [Parent,Ancestors,M,F,A], Opts).

-spec spawn_opt(node(), atom(), atom(), [term()], [spawn_option()]) -> pid().

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

-spec hibernate(module(), atom(), [term()]) -> no_return().

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
    {module,Mod} = erlang:fun_info(Fun, module),
    {name,Name} = erlang:fun_info(Fun, name),
    {arity,Arity} = erlang:fun_info(Fun, arity),
    put('$initial_call', {Mod,Name,Arity}),
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

-spec start(atom(), atom(), [term()]) -> term().

start(M, F, A) when is_atom(M), is_atom(F), is_list(A) ->
    start(M, F, A, infinity).

-spec start(atom(), atom(), [term()], timeout()) -> term().

start(M, F, A, Timeout) when is_atom(M), is_atom(F), is_list(A) ->
    Pid = ?MODULE:spawn(M, F, A),
    sync_wait(Pid, Timeout).

-spec start(atom(), atom(), [term()], timeout(), [spawn_option()]) -> term().

start(M, F, A, Timeout, SpawnOpts) when is_atom(M), is_atom(F), is_list(A) ->
    Pid = ?MODULE:spawn_opt(M, F, A, SpawnOpts),
    sync_wait(Pid, Timeout).

-spec start_link(atom(), atom(), [term()]) -> term().

start_link(M, F, A) when is_atom(M), is_atom(F), is_list(A) ->
    start_link(M, F, A, infinity).

-spec start_link(atom(), atom(), [term()], timeout()) -> term().

start_link(M, F, A, Timeout) when is_atom(M), is_atom(F), is_list(A) ->
    Pid = ?MODULE:spawn_link(M, F, A),
    sync_wait(Pid, Timeout).

-spec start_link(atom(),atom(),[term()],timeout(),[spawn_option()]) -> term().

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

-spec flush(pid()) -> 'true'.

flush(Pid) ->
    receive
	{'EXIT', Pid, _} ->
	    true
    after 0 ->
	    true
    end.

-spec init_ack(pid(), term()) -> 'ok'.

init_ack(Parent, Return) ->
    Parent ! {ack, self(), Return},
    ok.

-spec init_ack(term()) -> 'ok'.
init_ack(Return) ->
    [Parent|_] = get('$ancestors'),
    init_ack(Parent, Return).

%% -----------------------------------------------------
%% Fetch the initial call of a proc_lib spawned process.
%% -----------------------------------------------------

-spec initial_call(dict_or_pid()) -> {atom(), atom(), [atom()]} | 'false'.

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

-spec translate_initial_call(dict_or_pid()) -> mfa().

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

-spec format([term()]) -> string().

format([OwnReport,LinkReport]) ->
    OwnFormat = format_report(OwnReport),
    LinkFormat = format_report(LinkReport),
    S = io_lib:format("  crasher:~n~s  neighbours:~n~s",[OwnFormat,LinkFormat]),
    lists:flatten(S).

format_report(Rep) when is_list(Rep) ->
    format_rep(Rep);
format_report(Rep) ->
    io_lib:format("~p~n", [Rep]).

format_rep([{initial_call,InitialCall}|Rep]) ->
    [format_mfa(InitialCall)|format_rep(Rep)];
format_rep([{error_info,{Class,Reason,StackTrace}}|Rep]) ->
    [format_exception(Class, Reason, StackTrace)|format_rep(Rep)];
format_rep([{Tag,Data}|Rep]) ->
    [format_tag(Tag, Data)|format_rep(Rep)];
format_rep(_) ->
    [].

format_exception(Class, Reason, StackTrace) ->
    PF = pp_fun(),
    StackFun = fun(M, _F, _A) -> (M =:= erl_eval) or (M =:= ?MODULE) end,
    %% EI = "    exception: ",
    EI = "    ",
    [EI, lib:format_exception(1+length(EI), Class, Reason, 
                              StackTrace, StackFun, PF), "\n"].

format_mfa({M,F,Args}=StartF) ->
    try
	A = length(Args),
	["    initial call: ",atom_to_list(M),$:,atom_to_list(F),$/,
	 integer_to_list(A),"\n"]
    catch
	error:_ ->
	    format_tag(initial_call, StartF)
    end.

pp_fun() ->
    fun(Term, I) -> 
            io_lib:format("~." ++ integer_to_list(I) ++ "p", [Term]) 
    end.

format_tag(Tag, Data) ->
    io_lib:format("    ~p: ~80.18p~n", [Tag, Data]).
