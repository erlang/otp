%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2019. All Rights Reserved.
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
         start_monitor/3, start_monitor/4, start_monitor/5,
	 hibernate/3,
	 init_ack/1, init_ack/2,
	 init_p/3,init_p/5,format/1,format/2,format/3,report_cb/2,
	 initial_call/1,
         translate_initial_call/1,
	 stop/1, stop/3]).

%% Internal exports.
-export([wake_up/3]).

-export_type([spawn_option/0]).
-export_type([start_spawn_option/0]).

-include("logger.hrl").

%%-----------------------------------------------------------------------------

-type start_spawn_option() :: 'link'
                            | {'priority', erlang:priority_level()}
                            | {'max_heap_size', erlang:max_heap_size()}
                            | {'min_heap_size', non_neg_integer()}
                            | {'min_bin_vheap_size', non_neg_integer()}
                            | {'fullsweep_after', non_neg_integer()}
                            | {'message_queue_data', erlang:message_queue_data() }.

-type spawn_option()   :: erlang:spawn_opt_option().

-type dict_or_pid()    :: pid()
                        | (ProcInfo :: [_])
                        | {X :: integer(), Y :: integer(), Z :: integer()}.

%%-----------------------------------------------------------------------------

-define(VERIFY_NO_MONITOR_OPT(M, F, A, T, Opts),
        case lists:member(monitor, Opts) of
            true -> erlang:error(badarg, [M,F,A,T,Opts]);
            false -> ok
        end).

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

-spec spawn_opt(Fun, SpawnOpts) -> pid() | {pid(), reference()} when
      Fun :: function(),
      SpawnOpts :: [spawn_option()].

spawn_opt(F, Opts) when is_function(F) ->
    Parent = get_my_name(),
    Ancestors = get_ancestors(),
    erlang:spawn_opt(?MODULE, init_p, [Parent,Ancestors,F],Opts).

-spec spawn_opt(Node, Function, SpawnOpts) -> pid() | {pid(), reference()} when
      Node :: node(),
      Function :: function(),
      SpawnOpts :: [spawn_option()].

spawn_opt(Node, F, Opts) when is_function(F) ->
    Parent = get_my_name(),
    Ancestors = get_ancestors(),
    erlang:spawn_opt(Node, ?MODULE, init_p, [Parent,Ancestors,F], Opts).

-spec spawn_opt(Module, Function, Args, SpawnOpts) -> pid() | {pid(), reference()} when
      Module :: module(),
      Function :: atom(),
      Args :: [term()],
      SpawnOpts :: [spawn_option()].

spawn_opt(M, F, A, Opts) when is_atom(M), is_atom(F), is_list(A) ->
    Parent = get_my_name(),
    Ancestors = get_ancestors(),
    erlang:spawn_opt(?MODULE, init_p, [Parent,Ancestors,M,F,A], Opts).

-spec spawn_opt(Node, Module, Function, Args, SpawnOpts) -> pid() | {pid(), reference()} when
      Node :: node(),
      Module :: module(),
      Function :: atom(),
      Args :: [term()],
      SpawnOpts :: [spawn_option()].

spawn_opt(Node, M, F, A, Opts) when is_atom(M), is_atom(F), is_list(A) ->
    Parent = get_my_name(),
    Ancestors = get_ancestors(),
    erlang:spawn_opt(Node, ?MODULE, init_p, [Parent,Ancestors,M,F,A], Opts).

spawn_mon(M,F,A) ->
    Parent = get_my_name(),
    Ancestors = get_ancestors(),
    erlang:spawn_monitor(?MODULE, init_p, [Parent,Ancestors,M,F,A]).

-spec hibernate(Module, Function, Args) -> no_return() when
      Module :: module(),
      Function :: atom(),
      Args :: [term()].

hibernate(M, F, A) when is_atom(M), is_atom(F), is_list(A) ->
    erlang:hibernate(?MODULE, wake_up, [M, F, A]).

-spec init_p(pid(), [pid()], function()) -> term().

init_p(Parent, Ancestors, Fun) when is_function(Fun) ->
    put('$ancestors', [Parent|Ancestors]),
    Mfa = erlang:fun_info_mfa(Fun),
    put('$initial_call', Mfa),
    try
	Fun()
    catch
	Class:Reason:Stacktrace ->
	    exit_p(Class, Reason, Stacktrace)
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
	Class:Reason:Stacktrace ->
	    exit_p(Class, Reason, Stacktrace)
    end.

-spec wake_up(atom(), atom(), [term()]) -> term().

wake_up(M, F, A) when is_atom(M), is_atom(F), is_list(A) ->
    try
	apply(M, F, A) 
    catch
	Class:Reason:Stacktrace ->
	    exit_p(Class, Reason, Stacktrace)
    end.

exit_p(Class, Reason, Stacktrace) ->
    case get('$initial_call') of
	{M,F,A} when is_atom(M), is_atom(F), is_integer(A) ->
	    MFA = {M,F,make_dummy_args(A, [])},
	    crash_report(Class, Reason, MFA, Stacktrace),
	    erlang:raise(exit, exit_reason(Class, Reason, Stacktrace), Stacktrace);
	_ ->
	    %% The process dictionary has been cleared or
	    %% possibly modified.
	    crash_report(Class, Reason, [], Stacktrace),
	    erlang:raise(exit, exit_reason(Class, Reason, Stacktrace), Stacktrace)
    end.

exit_reason(error, Reason, Stacktrace) ->
    {Reason, Stacktrace};
exit_reason(exit, Reason, _Stacktrace) ->
    Reason;
exit_reason(throw, Reason, Stacktrace) ->
    {{nocatch, Reason}, Stacktrace}.

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
    sync_start(spawn_mon(M, F, A), Timeout).

-spec start(Module, Function, Args, Time, SpawnOpts) -> Ret when
      Module :: module(),
      Function :: atom(),
      Args :: [term()],
      Time :: timeout(),
      SpawnOpts :: [start_spawn_option()],
      Ret :: term() | {error, Reason :: term()}.

start(M, F, A, Timeout, SpawnOpts) when is_atom(M), is_atom(F), is_list(A) ->
    ?VERIFY_NO_MONITOR_OPT(M, F, A, Timeout, SpawnOpts),
    sync_start(?MODULE:spawn_opt(M, F, A, [monitor|SpawnOpts]), Timeout).

sync_start({Pid, Ref}, Timeout) ->
    receive
	{ack, Pid, Return} ->
	    erlang:demonitor(Ref, [flush]),
            Return;
	{'DOWN', Ref, process, Pid, Reason} ->
            {error, Reason}
    after Timeout ->
	    erlang:demonitor(Ref, [flush]),
            kill_flush(Pid),
            {error, timeout}
    end.

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
    sync_start_link(?MODULE:spawn_link(M, F, A), Timeout).

-spec start_link(Module, Function, Args, Time, SpawnOpts) -> Ret when
      Module :: module(),
      Function :: atom(),
      Args :: [term()],
      Time :: timeout(),
      SpawnOpts :: [start_spawn_option()],
      Ret :: term() | {error, Reason :: term()}.

start_link(M,F,A,Timeout,SpawnOpts) when is_atom(M), is_atom(F), is_list(A) ->
    ?VERIFY_NO_MONITOR_OPT(M, F, A, Timeout, SpawnOpts),
    sync_start_link(?MODULE:spawn_opt(M, F, A, [link|SpawnOpts]), Timeout).

sync_start_link(Pid, Timeout) ->
    receive
	{ack, Pid, Return} ->
            Return;
	{'EXIT', Pid, Reason} ->
            {error, Reason}
    after Timeout ->
            kill_flush(Pid),
            {error, timeout}
    end.

-spec start_monitor(Module, Function, Args) -> {Ret, Mon} when
      Module :: module(),
      Function :: atom(),
      Args :: [term()],
      Mon :: reference(),
      Ret :: term() | {error, Reason :: term()}.

start_monitor(M, F, A) when is_atom(M), is_atom(F), is_list(A) ->
    start_monitor(M, F, A, infinity).

-spec start_monitor(Module, Function, Args, Time) -> {Ret, Mon} when
      Module :: module(),
      Function :: atom(),
      Args :: [term()],
      Time :: timeout(),
      Mon :: reference(),
      Ret :: term() | {error, Reason :: term()}.

start_monitor(M, F, A, Timeout) when is_atom(M), is_atom(F), is_list(A) ->
    sync_start_monitor(spawn_mon(M, F, A), Timeout).

-spec start_monitor(Module, Function, Args, Time, SpawnOpts) -> {Ret, Mon} when
      Module :: module(),
      Function :: atom(),
      Args :: [term()],
      Time :: timeout(),
      SpawnOpts :: [start_spawn_option()],
      Mon :: reference(),
      Ret :: term() | {error, Reason :: term()}.

start_monitor(M,F,A,Timeout,SpawnOpts) when is_atom(M),
                                            is_atom(F),
                                            is_list(A) ->
    ?VERIFY_NO_MONITOR_OPT(M, F, A, Timeout, SpawnOpts),
    sync_start_monitor(?MODULE:spawn_opt(M, F, A, [monitor|SpawnOpts]),
                       Timeout).

sync_start_monitor({Pid, Ref}, Timeout) ->
    receive
	{ack, Pid, Return} ->
            {Return, Ref};
	{'DOWN', Ref, process, Pid, Reason} = Down ->
            self() ! Down,
            {{error, Reason}, Ref}
    after Timeout ->
            kill_flush(Pid),
            {{error, timeout}, Ref}
    end.

-spec kill_flush(Pid) -> 'ok' when
      Pid :: pid().

kill_flush(Pid) ->
    unlink(Pid),
    exit(Pid, kill),
    receive {'EXIT', Pid, _} -> ok after 0 -> ok end,
    ok.

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
trans_init(gen,init_it,[gen_event|_]) ->
    {gen_event,init_it,6};
trans_init(gen,init_it,[_GenMod,_,_,Module,_,_]) when is_atom(Module) ->
    {Module,init,1};
trans_init(gen,init_it,[_GenMod,_,_,_,Module|_]) when is_atom(Module) ->
    {Module,init,1};
trans_init(M, F, A) when is_atom(M), is_atom(F) ->
    {M,F,length(A)}.

%% -----------------------------------------------------
%% Generate a crash report.
%% -----------------------------------------------------

crash_report(exit, normal, _, _)       -> ok;
crash_report(exit, shutdown, _, _)     -> ok;
crash_report(exit, {shutdown,_}, _, _) -> ok;
crash_report(Class, Reason, StartF, Stacktrace) ->
    ?LOG_ERROR(#{label=>{proc_lib,crash},
                 report=>[my_info(Class, Reason, StartF, Stacktrace),
                          linked_info(self())]},
               #{domain=>[otp,sasl],
                 report_cb=>fun proc_lib:report_cb/2,
                 logger_formatter=>#{title=>"CRASH REPORT"},
                 error_logger=>#{tag=>error_report,type=>crash_report}}).

my_info(Class, Reason, [], Stacktrace) ->
    my_info_1(Class, Reason, Stacktrace);
my_info(Class, Reason, StartF, Stacktrace) ->
    [{initial_call, StartF}|
     my_info_1(Class, Reason, Stacktrace)].

my_info_1(Class, Reason, Stacktrace) ->
    [{pid, self()},
     get_process_info(self(), registered_name),         
     {error_info, {Class,Reason,Stacktrace}},
     get_ancestors(self()),        
     get_process_info(self(), message_queue_len),
     get_messages(self()),
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

%% The messages and the dictionary are possibly limited too much if
%% some error handles output the messages or the dictionary using ~P
%% or ~W with depth greater than the depth used here (the depth of
%% control characters P and W takes precedence over the depth set by
%% application variable error_logger_format_depth). However, it is
%% assumed that all report handlers call proc_lib:format().
get_messages(Pid) ->
    Messages = get_process_messages(Pid),
    {messages, error_logger:limit_term(Messages)}.

get_process_messages(Pid) ->
    Depth = error_logger:get_format_depth(),
    case Pid =/= self() orelse Depth =:= unlimited of
        true ->
            {messages, Messages} = get_process_info(Pid, messages),
            Messages;
        false ->
            %% If there are more messages than Depth, garbage
            %% collection can sometimes be avoided by collecting just
            %% enough messages for the crash report. It is assumed the
            %% process is about to die anyway.
            receive_messages(Depth)
    end.

receive_messages(0) -> [];
receive_messages(N) ->
    receive
        M ->
            [M|receive_messages(N - 1)]
    after 0 ->
            []
    end.

get_cleaned_dictionary(Pid) ->
    case get_process_info(Pid,dictionary) of
	{dictionary,Dict} -> {dictionary,cleaned_dict(Dict)};
	_                 -> {dictionary,[]}
    end.

cleaned_dict(Dict) ->
    CleanDict = clean_dict(Dict),
    error_logger:limit_term(CleanDict).

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
  
%% Do not include messages or process dictionary, even if
%% error_logger_format_depth is unlimited.
make_neighbour_report(Pid) ->
  [{pid, Pid},
   get_process_info(Pid, registered_name),          
   get_initial_call(Pid),
   get_process_info(Pid, current_function),
   get_ancestors(Pid),
   get_process_info(Pid, message_queue_len),
   %% get_messages(Pid),
   get_process_info(Pid, links),
   %% get_cleaned_dictionary(Pid),
   get_process_info(Pid, trap_exit),
   get_process_info(Pid, status),
   get_process_info(Pid, heap_size),
   get_process_info(Pid, stack_size),
   get_process_info(Pid, reductions),
   get_process_info(Pid, current_stacktrace)
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
%%% Format a generated crash info structure.
%%% -----------------------------------------------------------

-spec report_cb(CrashReport,FormatOpts) -> unicode:chardata() when
      CrashReport :: #{label => {proc_lib,crash},
                       report => [term()]},
      FormatOpts :: logger:report_cb_config().
report_cb(#{label:={proc_lib,crash}, report:=CrashReport}, Extra) ->
    Default = #{chars_limit => unlimited,
                depth => unlimited,
                single_line => false,
                encoding => utf8},
    do_format(CrashReport, maps:merge(Default,Extra)).

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

format(CrashReport, Encoding, Depth) ->
    do_format(CrashReport, #{chars_limit => unlimited,
                             depth => Depth,
                             encoding => Encoding,
                             single_line => false}).

do_format([OwnReport,LinkReport], Extra) ->
    #{encoding:=Enc, single_line:=Single, chars_limit:=Limit0} = Extra,
    Indent = if Single -> "";
                true -> "  "
             end,
    Nl = nl(Single," "),
    Sep = nl(Single, report_separator()),
    {PartLimit, Limit} =
        case Limit0 of
            unlimited ->
                {Limit0, Limit0};
            _ when is_integer(Limit0) ->
                %% HardcodedSize is the length of the hardcoded heading +
                %% separators in the final format string below,
                %% including neighbours. Just make sure the limit
                %% does not become negative.
                Num = length(OwnReport),
                HardcodedSize = (length(Indent) + length("crasher")
                                 + length(Nl) + length(Sep)
                                 + (length(Sep) * Num)),
                Limit1 = max(Limit0-HardcodedSize, 1),

                %% Divide the available characters over all report
                %% parts. Spend one third of the characters on the
                %% crash reason, and let the rest of the elements
                %% (including the neighbours) share the other two
                %% thirds. This is to make sure we see a good part of
                %% the crash reason. Most of the other elements in the
                %% crasher's report are quite small, so we don't loose
                %% a lot of info from these anyway.
                EL = Limit1 div 3,
                PL = (Limit1-EL) div (Num),
                {PL, Limit1}
        end,
    LinkFormat = format_link_reports(LinkReport, Indent, Extra, PartLimit),
    LinkFormatSize = size(Enc, LinkFormat),

    OwnFormat = format_own_report(OwnReport, Indent, Extra,
                                  LinkFormatSize, PartLimit, Limit),
    io_lib:format("~scrasher:"++Nl++"~ts"++Sep++"~ts",
                  [Indent,OwnFormat,LinkFormat]).

format_own_report(OwnReport, Indent, Extra, LinkFormatSize, PartLimit, Limit0) ->
    MyIndent = Indent ++ Indent,
    case separate_error_info(OwnReport) of
        {First,{Class,Reason,StackTrace},Rest} ->
            F = format_report(First, MyIndent, Extra, PartLimit),
            R = format_report(Rest, MyIndent, Extra, PartLimit),
            #{encoding:=Enc, single_line:=Single} = Extra,
            Sep = nl(Single, part_separator()),
            Limit = case Limit0 of
                        unlimited ->
                            Limit0;
                        _ when is_integer(Limit0) ->
                            %% Some of the report parts are quite small,
                            %% and we can use the leftover chars to show
                            %% more of the error_info part.
                            SizeOfOther = (size(Enc, F)
                                           +size(Enc, R)
                                           -length(Sep)*(length(F)+length(R))
                                           +LinkFormatSize),
                            max(Limit0-SizeOfOther, 1)
                end,
            EI = format_exception(Class, Reason, StackTrace, Extra, Limit),
            lists:join(Sep, [F, EI, R]);
    no ->
        Limit = case Limit0 of
                    unlimited ->
                        Limit0;
                    _ when is_integer(Limit0) ->
                        max(Limit0-LinkFormatSize, 1)
                end,
        format_report(OwnReport, MyIndent, Extra, Limit)
    end.

separate_error_info(Report) ->
    try
        lists:splitwith(fun(A) -> element(1, A) =/= error_info end, Report)
    of
        {First, [{error_info,ErrorInfo}|Rest]} ->
            {First,ErrorInfo,Rest};
        _ -> no
    catch _:_ -> no
    end.

%% If the size of the total report is limited by chars_limit, then
%% print only the pids.
format_link_reports(LinkReports, Indent, Extra, PartLimit)
         when is_integer(PartLimit) ->
    #{encoding:=Enc, depth:=Depth, single_line:=Single} = Extra,
    Pids = [P || {neighbour,[{pid,P}|_]} <- LinkReports],
    {P,Tl} = p(Enc,Depth),
    Width = if Single -> "0";
               true -> ""
            end,
    io_lib:format(Indent++"neighbours: ~"++Width++P,
                  [Pids|Tl],
                  [{chars_limit,PartLimit}]);
format_link_reports(LinkReports, Indent, Extra, PartLimit) ->
    #{single_line:=Single} = Extra,
    MyIndent = Indent ++ Indent,
    LinkFormat =
      lists:join(nl(Single, report_separator()),
                 format_link_report(LinkReports, MyIndent, Extra, PartLimit)),
    [Indent,"neighbours:",nl(Single," "),LinkFormat].

format_link_report([Link|Reps], Indent0, Extra, PartLimit) ->
    #{single_line:=Single} = Extra,
    Rep = case Link of
              {neighbour,Rep0} -> Rep0;
              _ -> Link
          end,
    Indent = if Single -> "";
                true -> Indent0
             end,
    LinkIndent = ["  ",Indent],
    [[Indent,"neighbour:",nl(Single," "),
      format_report(Rep, LinkIndent, Extra, PartLimit)]|
     format_link_report(Reps, Indent, Extra, PartLimit)];
format_link_report(Rep, Indent, Extra, PartLimit) ->
    format_report(Rep, Indent, Extra, PartLimit).

format_report(Rep, Indent, Extra, Limit) when is_list(Rep) ->
    #{single_line:=Single} = Extra,
    lists:join(nl(Single, part_separator()),
               format_rep(Rep, Indent, Extra, Limit));
format_report(Rep, Indent0, Extra, Limit) ->
    #{encoding:=Enc, depth:=Depth, single_line:=Single} = Extra,
    {P,Tl} = p(Enc,Depth),
    {Indent,Width} = if Single -> {"","0"};
                        true -> {Indent0,""}
                     end,
    Opts = chars_limit_opt(Limit),
    io_lib:format("~s~"++Width++P, [Indent, Rep | Tl], Opts).

format_rep([{initial_call,InitialCall}|Rep], Indent, Extra, Limit) ->
    [format_mfa(Indent, InitialCall, Extra, Limit)|
     format_rep(Rep, Indent, Extra, Limit)];
format_rep([{Tag,Data}|Rep], Indent, Extra, Limit) ->
    [format_tag(Indent, Tag, Data, Extra, Limit)|
     format_rep(Rep, Indent, Extra, Limit)];
format_rep(_, _, _Extra, _Limit) ->
    [].

format_exception(Class, Reason, StackTrace, Extra, Limit) ->
    #{encoding:=Enc,depth:=Depth, single_line:=Single} = Extra,
    StackFun = fun(M, _F, _A) -> (M =:= erl_eval) or (M =:= ?MODULE) end,
    if Single ->
            {P,Tl} = p(Enc,Depth),
            Opts = chars_limit_opt(Limit),
            [atom_to_list(Class), ": ",
             io_lib:format("~0"++P,[{Reason,StackTrace}|Tl],Opts)];
       true ->
            %% Notice that each call to PF uses chars_limit, which
            %% means that the total size of the formatted exception
            %% can exceed the limit a lot.
            PF = pp_fun(Extra, Enc),
            EI = "    ",
            Lim = case Limit of
                      unlimited -> -1;
                      _ -> Limit
                  end,
            FE = erl_error:format_exception(1+length(EI), Class, Reason,
                                            StackTrace, StackFun, PF, Enc,
                                            Lim),
            [EI, FE]
    end.

format_mfa(Indent0, {M,F,Args}=StartF, Extra, Limit) ->
    #{encoding:=Enc,single_line:=Single} = Extra,
    Indent = if Single -> "";
                true -> Indent0
             end,
    try
	A = length(Args),
	[Indent,"initial call: ",to_string(M, Enc),$:,to_string(F, Enc),$/,
	 integer_to_list(A)]
    catch
	error:_ ->
	    format_tag(Indent, initial_call, StartF, Extra, Limit)
    end.

to_string(A, latin1) ->
    io_lib:write_atom_as_latin1(A);
to_string(A, _) ->
    io_lib:write_atom(A).

pp_fun(Extra, Enc) ->
    #{encoding:=Enc,depth:=Depth, single_line:=Single} = Extra,
    {P,Tl} = p(Enc, Depth),
    Width = if Single -> "0";
               true -> ""
            end,
    fun(Term, I, Limit) ->
            S = io_lib:format("~" ++ Width ++ "." ++ integer_to_list(I) ++ P,
                              [Term|Tl], [{chars_limit, Limit}]),
            {S, sub(Limit, S, Enc)}
    end.

format_tag(Indent0, Tag, Data, Extra, Limit) ->
    #{encoding:=Enc,depth:=Depth,single_line:=Single} = Extra,
    {P,Tl} = p(Enc, Depth),
    {Indent,Width} = if Single -> {"","0"};
                        true -> {Indent0,""}
                     end,
    Opts = chars_limit_opt(Limit),
    io_lib:format("~s~" ++ Width ++ "p: ~" ++ Width ++ ".18" ++ P,
                  [Indent, Tag, Data|Tl], Opts).

p(Encoding, Depth) ->
    {Letter, Tl}  = case Depth of
                        unlimited -> {"p", []};
                        _         -> {"P", [Depth]}
                    end,
    P = modifier(Encoding) ++ Letter,
    {P, Tl}.

report_separator() -> "; ".

part_separator() -> ", ".

chars_limit_opt(CharsLimit) ->
    [{chars_limit, CharsLimit} || is_integer(CharsLimit)].

modifier(latin1) -> "";
modifier(_) -> "t".

nl(true,Else) -> Else;
nl(false,_) -> "\n".

%% Make sure T does change sign.
sub(T, _, _Enc) when T < 0 -> T;
sub(T, E, Enc) ->
    Sz = size(Enc, E),
    if
        T >= Sz ->
            T - Sz;
        true ->
            0
    end.

size(latin1, S) ->
    iolist_size(S);
size(_, S) ->
    string:length(S).

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
