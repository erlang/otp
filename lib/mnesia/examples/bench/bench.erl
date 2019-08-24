%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2016. All Rights Reserved.
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% File    : bench.hrl
%%% Author  : Hakan Mattsson <hakan@cslab.ericsson.se>
%%% Purpose : Implement the Canadian database benchmark (LMC/UU-01:025)
%%% Created : 21 Jun 2001 by Hakan Mattsson <hakan@cslab.ericsson.se>
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(bench).
-author('hakan@cslab.ericsson.se').

-include("bench.hrl").

-export([
         run/0, run/1,
	 
         start_all/0, start_all/1,
	 populate/0, populate/1,
	 generate/0, generate/1,
	 
	 args_to_config/1, verify_config/2,
	 start/0, start/1,
	 stop_slave_nodes/1,
	 bind_schedulers/0
        ]).

bind_schedulers() ->
    try
        %% Avoid first core and bind schedules to the remaining ones
	Topo = erlang:system_info(cpu_topology),
	erlang:system_flag(cpu_topology,lists:reverse(Topo)),
	%% N = erlang:system_info(schedulers),
	%% erlang:system_flag(schedulers_online, lists:max([N - 1, 1])),
	erlang:system_flag(scheduler_bind_type, default_bind),
	timer:sleep(timer:seconds(1)), % Wait for Rickard
	erlang:system_info(scheduler_bindings)
    catch _:_ ->
        %% Ancient systems
        ignore
    end.

%% Run the benchmark:
%% 
%%   - Start all necessary Erlang nodes
%%   - Populate the database
%%   - Start the traffic generators
%%   - Calculate benchmark statistics
%%   - Stop the temporary Erlang nodes
run() ->
    FileName = "bench.config",
    run([FileName]).

run(Args) ->
    C = args_to_config(Args),
    SlaveNodes = start_all(C),
    bench_populate:start(C),
    Result = bench_generate:start(C),
    stop_slave_nodes(SlaveNodes),
    Result.

%% Start Mnesia on the local node
start() ->
    FileName = 'bench.config',
    start([FileName]).

start(Args) ->
    C = args_to_config(Args),
    erlang:set_cookie(node(), C#config.cookie),
    Nodes = [node() | (((C#config.table_nodes -- C#config.generator_nodes) ++
			C#config.generator_nodes) -- [node()])],
    Extra = [{extra_db_nodes, Nodes}],
    ?d("Starting Mnesia on node ~p...", [node()]),
    case mnesia:start(Extra) of
	ok ->
	    Tables = mnesia:system_info(tables),
	    io:format(" ok.~n" , []),
	    ?d("Waiting for ~p tables...", [length(Tables)]),
	    wait(Tables);
	{error, Reason} ->
	    io:format(" FAILED: ~p~n", [Reason]),
	    {error, Reason}
    end.

wait(Tables) ->
    case mnesia:wait_for_tables(Tables, timer:seconds(10)) of
	ok ->
	    io:format(" loaded.~n", []),
	    ok;
	{timeout, More} ->
	    io:format(" ~p...", [length(More)]),
	    wait(More)
    end.
    
%% Populate the database
populate() ->
    FileName = 'bench.config',
    populate([FileName]).

populate(Args) ->
    C = args_to_config(Args),
    bench_populate:start(C).

%% Start the traffic generators
generate() ->
    FileName = 'bench.config',
    generate([FileName]).

generate(Args) ->
    C = args_to_config(Args),
    bench_generate:start(C).

start_all() ->
    FileName = 'bench.config',
    start_all([FileName]).

start_all(Args) ->
    C = args_to_config(Args),
    Nodes = [node() | (((C#config.table_nodes -- C#config.generator_nodes) ++
			C#config.generator_nodes) -- [node()])],
    erlang:set_cookie(node(), C#config.cookie),
    ?d("Starting Erlang nodes...~n", []),
    ?d("~n", []),
   SlaveNodes = do_start_all(Nodes, [], C#config.cookie),
    Extra = [{extra_db_nodes, Nodes}],
    ?d("~n", []),
    ?d("Starting Mnesia...", []),
    case rpc:multicall(Nodes, mnesia, start, [Extra]) of
	{Replies, []} ->
	    case [R || R <- Replies, R /= ok] of
		[] ->
		    io:format(" ok~n", []),
		    SlaveNodes;
		Bad ->
		    io:format(" FAILED: ~p~n", [Bad]),
		    exit({mnesia_start, Bad})
	    end;
	Bad ->
	    io:format(" FAILED: ~p~n", [Bad]),
	    exit({mnesia_start, Bad})
    end.

do_start_all([Node | Nodes], Acc, Cookie) when is_atom(Node) ->    
    case string:tokens(atom_to_list(Node), [$@]) of
	[Name, Host] ->
	    Arg = lists:concat(["-setcookie ", Cookie]),
	    ?d("    ~s", [left(Node)]),
	    case slave:start_link(Host, Name, Arg) of
		{ok, Node} ->
		    load_modules(Node),
		    rpc:call(Node, ?MODULE, bind_schedulers, []),
		    io:format(" started~n", []),
		    do_start_all(Nodes, [Node | Acc], Cookie);
		{error, {already_running, Node}} ->
		    rpc:call(Node, ?MODULE, bind_schedulers, []),
		    io:format(" already started~n", []),
		    do_start_all(Nodes, Acc, Cookie);
		{error, Reason} ->
		    io:format(" FAILED:~p~n", [Reason]),
		    stop_slave_nodes(Acc),
		    exit({slave_start_failed, Reason})
	    end;
	_ ->
	    ?d("    ~s FAILED: "
	       "Not valid as node name. Must be 'name@host'.~n",
	       [left(Node)]),
	    stop_slave_nodes(Acc),
	    exit({bad_node_name, Node})
    end;
do_start_all([], StartedNodes, _Cookie) ->
    StartedNodes.

load_modules(Node) ->
    Fun = 
	fun(Mod) ->
		case code:get_object_code(Mod) of
		    {_Module, Bin, Fname} ->
			rpc:call(Node, code,load_binary,[Mod,Fname,Bin]);
		    Other ->
			Other
		end
	end,
    lists:foreach(Fun, [bench, bench_generate, bench_populate, bench_trans]).

stop_slave_nodes([]) ->
    ok;
stop_slave_nodes(Nodes) ->
    ?d("~n", []),
    ?d("Stopping Erlang nodes...~n", []),
    ?d("~n", []),
    do_stop_slave_nodes(Nodes).

do_stop_slave_nodes([Node | Nodes]) ->
    ?d("    ~s", [left(Node)]),
    Res = slave:stop(Node),
    io:format(" ~p~n", [Res]),
    do_stop_slave_nodes(Nodes);
do_stop_slave_nodes([]) ->
    ok.
	    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% The configuration
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

args_to_config(C) when is_record(C, config) ->
    C;
args_to_config(Args) when is_list(Args) ->
    do_args_to_config(Args, []).

do_args_to_config([{Key, Val} | Rest], Acc) when is_list(Acc) ->
    do_args_to_config(Rest,  Acc ++ [{Key, Val}]);
do_args_to_config([FileName | Rest], Acc) when is_list(Acc) ->
    io:nl(),
    ?d("Reading configuration file ~p...", [FileName]),
    case file:consult(FileName) of
        {ok, Config} ->
            io:format(" ok~n", []),
            do_args_to_config(Rest, Acc ++ Config);
        {error, Reason} ->
            io:format(" FAILED: ~s~n",
               [[lists:flatten(file:format_error( Reason))]]),
            {error, {args_to_config, FileName, Reason}}
    end;
do_args_to_config([], Acc) when is_list(Acc) ->
    verify_config(Acc, #config{}).

verify_config([{Tag, Val} | T], C) ->
    case Tag of
        cookie when is_atom(Val) ->
            verify_config(T, C#config{cookie = Val});
        generator_profile when Val == random ->
            verify_config(T, C#config{generator_profile = Val});
        generator_profile when Val == t1 ->
            verify_config(T, C#config{generator_profile = Val});
        generator_profile when Val == t2 ->
            verify_config(T, C#config{generator_profile = Val});
        generator_profile when Val == t3 ->
            verify_config(T, C#config{generator_profile = Val});
        generator_profile when Val == t4 ->
            verify_config(T, C#config{generator_profile = Val});
        generator_profile when Val == t5 ->
            verify_config(T, C#config{generator_profile = Val});
        generator_profile when Val == ping ->
            verify_config(T, C#config{generator_profile = Val});
        generator_nodes when is_list(Val) ->
            verify_config(T, C#config{generator_nodes = Val});
        n_generators_per_node when is_integer(Val), Val >= 0 ->
            verify_config(T, C#config{n_generators_per_node = Val});
        generator_warmup when is_integer(Val), Val >= 0 ->
            verify_config(T, C#config{generator_warmup = Val});
        generator_duration when is_integer(Val), Val >= 0 ->
            verify_config(T, C#config{generator_duration = Val});
        generator_cooldown when is_integer(Val), Val >= 0 ->
            verify_config(T, C#config{generator_cooldown = Val});
        statistics_detail when Val == debug ->
            verify_config(T, C#config{statistics_detail = Val});
        statistics_detail when Val == debug2 ->
            verify_config(T, C#config{statistics_detail = Val});
        statistics_detail when Val == normal ->
            verify_config(T, C#config{statistics_detail = Val});
        table_nodes when is_list(Val) ->
            verify_config(T, C#config{table_nodes = Val});
        use_binary_subscriber_key when Val == true ->
            verify_config(T, C#config{use_binary_subscriber_key = Val});
        use_binary_subscriber_key when Val == false ->
            verify_config(T, C#config{use_binary_subscriber_key = Val});
        storage_type when is_atom(Val) ->
            verify_config(T, C#config{storage_type = Val});
        write_lock_type when Val == sticky_write ->
            verify_config(T, C#config{write_lock_type = Val});
        write_lock_type when Val == write ->
            verify_config(T, C#config{write_lock_type = Val});
        n_replicas when is_integer(Val), Val >= 0 ->
            verify_config(T, C#config{n_replicas = Val});
        n_fragments when is_integer(Val), Val >= 0 ->
            verify_config(T, C#config{n_fragments = Val});
        n_subscribers when is_integer(Val), Val >= 0 ->
            verify_config(T, C#config{n_subscribers = Val});
        n_groups when is_integer(Val), Val >= 0 ->
            verify_config(T, C#config{n_groups = Val});
        n_servers when is_integer(Val), Val >= 0 ->
            verify_config(T, C#config{n_servers = Val});
        always_try_nearest_node when Val == true; Val == false ->
            verify_config(T, C#config{always_try_nearest_node = Val});
        _ ->
	    ?e("Bad config value:  ~p~n", [Tag, Val]),
	    exit({bad_config_value, {Tag, Val}})
    end;
verify_config([], C) ->
    display_config(C),
    C;
verify_config(Config, _) ->
    ?e("Bad config:  ~p~n", [Config]),
    exit({bad_config, Config}).

display_config(C) when is_record(C, config) ->
    ?d("~n", []),
    ?d("Actual configuration...~n", []),
    ?d("~n", []),
    Fields = record_info(fields, config),
    [config | Values] = tuple_to_list(C),
    display_config(Fields, Values).

display_config([F | Fields], [V | Values]) ->
    ?d("    ~s ~p~n", [left(F), V]),
    display_config(Fields, Values);
display_config([], []) ->
    ?d("~n", []),
    ok.

left(Term) ->
    string:left(lists:flatten(io_lib:format("~p", [Term])), 27, $.).
