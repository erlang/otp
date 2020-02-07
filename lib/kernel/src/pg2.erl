%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2017. All Rights Reserved.
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
-module(pg2).

-export([create/1, delete/1, join/2, leave/2]).
-export([get_members/1, get_local_members/1]).
-export([get_closest_pid/1, which_groups/0]).
-export([start/0,start_link/0,init/1,handle_call/3,handle_cast/2,handle_info/2,
         terminate/2]).

-deprecated([create/1, delete/1, join/2, leave/2]).
-deprecated([get_members/1, get_local_members/1]).
-deprecated([get_closest_pid/1, which_groups/0]).
-deprecated([start/0,start_link/0,init/1,handle_call/3,handle_cast/2,handle_info/2,
             terminate/2]).

%%% As of R13B03 monitors are used instead of links.

%%%
%%% Exported functions
%%%

-spec start_link() -> {'ok', pid()} | {'error', any()}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec start() -> {'ok', pid()} | {'error', any()}.

start() ->
    ensure_started().

-type name() :: any().

-spec create(Name :: name()) -> 'ok'.

create(Name) ->
    _ = ensure_started(),
    case ets:member(pg2_table, {group, Name}) of
        false ->
            global:trans({{?MODULE, Name}, self()},
                         fun() ->
                                 gen_server:multi_call(?MODULE, {create, Name})
                         end),
            ok;
        true ->
            ok
    end.

-spec delete(Name :: name()) -> 'ok'.

delete(Name) ->
    _ = ensure_started(),
    global:trans({{?MODULE, Name}, self()},
                 fun() ->
                         gen_server:multi_call(?MODULE, {delete, Name})
                 end),
    ok.

-spec join(Name, Pid :: pid()) -> 'ok' | {'error', {'no_such_group', Name}}
      when Name :: name().

join(Name, Pid) when is_pid(Pid) ->
    _ = ensure_started(),
    case ets:member(pg2_table, {group, Name}) of
        false ->
            {error, {no_such_group, Name}};
        true ->
            global:trans({{?MODULE, Name}, self()},
                         fun() ->
                                 gen_server:multi_call(?MODULE,
                                                       {join, Name, Pid})
                         end),
            ok
    end.

-spec leave(Name, Pid :: pid()) -> 'ok' | {'error', {'no_such_group', Name}}
      when Name :: name().

leave(Name, Pid) when is_pid(Pid) ->
    _ = ensure_started(),
    case ets:member(pg2_table, {group, Name}) of
        false ->
            {error, {no_such_group, Name}};
        true ->
            global:trans({{?MODULE, Name}, self()},
                         fun() ->
                                 gen_server:multi_call(?MODULE,
                                                       {leave, Name, Pid})
                         end),
            ok
    end.

-spec get_members(Name) -> [pid()] | {'error', {'no_such_group', Name}}
      when Name :: name().

get_members(Name) ->
    _ = ensure_started(),
    case ets:member(pg2_table, {group, Name}) of
        true ->
            group_members(Name);
        false ->
            {error, {no_such_group, Name}}
    end.

-spec get_local_members(Name) -> [pid()] | {'error', {'no_such_group', Name}}
      when Name :: name().

get_local_members(Name) ->
    _ = ensure_started(),
    case ets:member(pg2_table, {group, Name}) of
        true ->
            local_group_members(Name);
        false ->
            {error, {no_such_group, Name}}
    end.

-spec which_groups() -> [Name :: name()].

which_groups() ->
    _ = ensure_started(),
    all_groups().

-spec get_closest_pid(Name) ->  pid() | {'error', Reason} when
      Name :: name(),
      Reason ::  {'no_process', Name} | {'no_such_group', Name}.

get_closest_pid(Name) ->
    case get_local_members(Name) of
        [Pid] ->
            Pid;
        [] ->
            case get_members(Name) of
                [] -> {error, {no_process, Name}};
                Members ->
                    random_element(Members)
            end;
        Members when is_list(Members) ->
            random_element(Members);
        Else ->
            Else
    end.

random_element(List) ->
    X = abs(erlang:monotonic_time()
		bxor erlang:unique_integer()),
    lists:nth((X rem length(List)) + 1, List).

%%%
%%% Callback functions from gen_server
%%%

-record(state, {}).

-type state() :: #state{}.

-spec init(Arg :: []) -> {'ok', state()}.

init([]) ->
    Ns = nodes(),
    ok = net_kernel:monitor_nodes(true),
    lists:foreach(fun(N) ->
                          {?MODULE, N} ! {new_pg2, node()},
                          self() ! {nodeup, N}
                  end, Ns),
    pg2_table = ets:new(pg2_table, [ordered_set, protected, named_table]),
    {ok, #state{}}.

-spec handle_call(Call :: {'create', Name}
                        | {'delete', Name}
                        | {'join', Name, Pid :: pid()}
                        | {'leave', Name, Pid :: pid()},
                  From :: {pid(),Tag :: any()},
                  State :: state()) -> {'reply', 'ok', state()}
      when Name :: name().

handle_call({create, Name}, _From, S) ->
    assure_group(Name),
    {reply, ok, S};
handle_call({join, Name, Pid}, _From, S) ->
    ets:member(pg2_table, {group, Name}) andalso join_group(Name, Pid),
    {reply, ok, S};
handle_call({leave, Name, Pid}, _From, S) ->
    ets:member(pg2_table, {group, Name}) andalso leave_group(Name, Pid),
    {reply, ok, S};
handle_call({delete, Name}, _From, S) ->
    delete_group(Name),
    {reply, ok, S};
handle_call(Request, From, S) ->
    error_logger:warning_msg("The pg2 server received an unexpected message:\n"
                             "handle_call(~tp, ~tp, _)\n",
                             [Request, From]),
    {noreply, S}.

-spec handle_cast(Cast :: {'exchange', node(), Names :: [[Name,...]]}
                        | {'del_member', Name, Pid :: pid()},
                  State :: state()) -> {'noreply', state()}
      when Name :: name().

handle_cast({exchange, _Node, List}, S) ->
    store(List),
    {noreply, S};
handle_cast(_, S) ->
    %% Ignore {del_member, Name, Pid}.
    {noreply, S}.

-spec handle_info(Tuple :: tuple(), State :: state()) ->
    {'noreply', state()}.

handle_info({'DOWN', MonitorRef, process, _Pid, _Info}, S) ->
    member_died(MonitorRef),
    {noreply, S};
handle_info({nodeup, Node}, S) ->
    gen_server:cast({?MODULE, Node}, {exchange, node(), all_members()}),
    {noreply, S};
handle_info({new_pg2, Node}, S) ->
    gen_server:cast({?MODULE, Node}, {exchange, node(), all_members()}),
    {noreply, S};
handle_info(_, S) ->
    {noreply, S}.

-spec terminate(Reason :: any(), State :: state()) -> 'ok'.

terminate(_Reason, _S) ->
    true = ets:delete(pg2_table),
    ok.

%%%
%%% Local functions
%%%

%%% One ETS table, pg2_table, is used for bookkeeping. The type of the
%%% table is ordered_set, and the fast matching of partially
%%% instantiated keys is used extensively.
%%%
%%% {{group, Name}}
%%%    Process group Name.
%%% {{ref, Pid}, RPid, MonitorRef, Counter}
%%% {{ref, MonitorRef}, Pid}
%%%    Each process has one monitor. Sometimes a process is spawned to
%%%    monitor the pid (RPid). Counter is incremented when the Pid joins
%%%    some group.
%%% {{member, Name, Pid}, GroupCounter}
%%% {{local_member, Name, Pid}}
%%%    Pid is a member of group Name, GroupCounter is incremented when the
%%%    Pid joins the group Name.
%%% {{pid, Pid, Name}}
%%%    Pid is a member of group Name.

store(List) ->
    _ = [(assure_group(Name)
          andalso
          [join_group(Name, P) || P <- Members -- group_members(Name)]) ||
            [Name, Members] <- List],
    ok.

assure_group(Name) ->
    Key = {group, Name},
    ets:member(pg2_table, Key) orelse true =:= ets:insert(pg2_table, {Key}).

delete_group(Name) ->
    _ = [leave_group(Name, Pid) || Pid <- group_members(Name)],
    true = ets:delete(pg2_table, {group, Name}),
    ok.

member_died(Ref) ->
    [{{ref, Ref}, Pid}] = ets:lookup(pg2_table, {ref, Ref}),
    Names = member_groups(Pid),
    _ = [leave_group(Name, P) || 
            Name <- Names,
            P <- member_in_group(Pid, Name)],
    %% Kept for backward compatibility with links. Can be removed, eventually.
    _ = [gen_server:abcast(nodes(), ?MODULE, {del_member, Name, Pid}) ||
            Name <- Names],
    ok.

join_group(Name, Pid) ->
    Ref_Pid = {ref, Pid}, 
    try _ = ets:update_counter(pg2_table, Ref_Pid, {4, +1}), true
    catch _:_ ->
            {RPid, Ref} = do_monitor(Pid),
            true = ets:insert(pg2_table, {Ref_Pid, RPid, Ref, 1}),
            true = ets:insert(pg2_table, {{ref, Ref}, Pid})
    end,
    Member_Name_Pid = {member, Name, Pid},
    try _ = ets:update_counter(pg2_table, Member_Name_Pid, {2, +1})
    catch _:_ ->
            true = ets:insert(pg2_table, {Member_Name_Pid, 1}),
            _ = [ets:insert(pg2_table, {{local_member, Name, Pid}}) ||
                    node(Pid) =:= node()],
            true = ets:insert(pg2_table, {{pid, Pid, Name}})
    end.

leave_group(Name, Pid) ->
    Member_Name_Pid = {member, Name, Pid},
    try ets:update_counter(pg2_table, Member_Name_Pid, {2, -1}) of
        N ->
            if 
                N =:= 0 ->
                    true = ets:delete(pg2_table, {pid, Pid, Name}),
                    _ = [ets:delete(pg2_table, {local_member, Name, Pid}) ||
                            node(Pid) =:= node()],
                    true = ets:delete(pg2_table, Member_Name_Pid);
                true ->
                    ok
            end,
            Ref_Pid = {ref, Pid}, 
            case ets:update_counter(pg2_table, Ref_Pid, {4, -1}) of
                0 ->
                    [{Ref_Pid,RPid,Ref,0}] = ets:lookup(pg2_table, Ref_Pid),
                    true = ets:delete(pg2_table, {ref, Ref}),
                    true = ets:delete(pg2_table, Ref_Pid),
                    true = erlang:demonitor(Ref, [flush]),
                    kill_monitor_proc(RPid, Pid);
                _ ->
                    ok
            end
    catch _:_ ->
            ok
    end.

all_members() ->
    [[G, group_members(G)] || G <- all_groups()].

group_members(Name) ->
    [P || 
        [P, N] <- ets:match(pg2_table, {{member, Name, '$1'},'$2'}),
        _ <- lists:seq(1, N)].

local_group_members(Name) ->
    [P || 
        [Pid] <- ets:match(pg2_table, {{local_member, Name, '$1'}}),
        P <- member_in_group(Pid, Name)].

member_in_group(Pid, Name) ->
    case ets:lookup(pg2_table, {member, Name, Pid}) of
        [] -> [];
        [{{member, Name, Pid}, N}] ->
            lists:duplicate(N, Pid)
    end.

member_groups(Pid) ->
    [Name || [Name] <- ets:match(pg2_table, {{pid, Pid, '$1'}})].

all_groups() ->
    [N || [N] <- ets:match(pg2_table, {{group,'$1'}})].

ensure_started() ->
    case whereis(?MODULE) of
        undefined ->
            C = {pg2, {?MODULE, start_link, []}, permanent,
                 1000, worker, [?MODULE]},
            supervisor:start_child(kernel_safe_sup, C);
        Pg2Pid ->
            {ok, Pg2Pid}
    end.


kill_monitor_proc(RPid, Pid) ->
    RPid =:= Pid orelse exit(RPid, kill).

%% When/if erlang:monitor() returns before trying to connect to the
%% other node this function can be removed.
do_monitor(Pid) ->
    case (node(Pid) =:= node()) orelse lists:member(node(Pid), nodes()) of
        true ->
            %% Assume the node is still up
            {Pid, erlang:monitor(process, Pid)};
        false ->
            F = fun() -> 
                        Ref = erlang:monitor(process, Pid),
                        receive 
                            {'DOWN', Ref, process, Pid, _Info} ->
                                exit(normal)
                        end
                end,
            erlang:spawn_monitor(F)
    end.
