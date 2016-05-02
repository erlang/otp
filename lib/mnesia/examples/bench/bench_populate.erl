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
%%% File    : bench_populate.hrl
%%% Author  : Hakan Mattsson <hakan@cslab.ericsson.se>
%%% Purpose : Populate the database
%%% Created : 21 Jun 2001 by Hakan Mattsson <hakan@cslab.ericsson.se>
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(bench_populate).
-author('hakan@cslab.ericsson.se').

-include("bench.hrl").

%% Public
-export([start/1]).

%% Populate the database
start(C) when is_record(C, config) ->
    ?d("~n",[]),
    ?d("Populate database...~n",[]),
    ?d("~n",[]),
    create_tables(C),
    Populate =
	fun() ->
		populate_subscriber(write, C),
		populate_group(write, C),
		populate_server(write, C)
	end,
    mnesia:activity(sync_dirty, Populate, [], mnesia_frag).

%% -------------------------------------------------------------------
%% Create the tables
%% -------------------------------------------------------------------

create_tables(C) ->
    ?d("    Delete old tables...~n",[]),
    mnesia:delete_table(group),
    mnesia:delete_table(subscriber),
    mnesia:delete_table(session),
    mnesia:delete_table(server),
    mnesia:delete_table(suffix),

    ?d("    Creating ~p tables, with ~p replicas distributed over ~p nodes...~n",
       [C#config.storage_type,
	C#config.n_replicas,
	length(C#config.table_nodes)]),

    %% Create group table
    GroupDef = [{C#config.storage_type, C#config.table_nodes},
                {attributes, record_info(fields, group)}],
    ?APPLY(mnesia, create_table, [group, GroupDef]),

    %% Create suffix table
    FragStorage =
        case C#config.storage_type of
            ram_copies       -> n_ram_copies;
            disc_copies      -> n_disc_copies;
            disc_only_copies -> n_disc_only_copies
        end,
    FragProps =
        [{FragStorage, C#config.n_replicas},
         {node_pool, C#config.table_nodes},
         {n_fragments, C#config.n_fragments}],
    SuffixDef = [{frag_properties, FragProps}],
    ?APPLY(mnesia, create_table, [suffix, SuffixDef]),

    %% Create subscriber table
    SubscriberDef =
        [{frag_properties, [{foreign_key, {suffix, #subscriber.suffix}} | FragProps]},
              {attributes, record_info(fields, subscriber)}],
    ?APPLY(mnesia, create_table, [subscriber, SubscriberDef]),

    %% Create session table
    SessionDef =
        [{frag_properties, [{foreign_key, {suffix, #session.suffix}} | FragProps]},
         {attributes, record_info(fields, session)}],
    ?APPLY(mnesia, create_table, [session, SessionDef]),

    %% Create server table
    ServerDef =
        [{frag_properties, [{foreign_key, {suffix, #server.suffix}} | FragProps]},
         {attributes, record_info(fields, server)}],
    ?APPLY(mnesia, create_table, [server, ServerDef]).

%% -------------------------------------------------------------------
%% Populate the subscriber table
%% -------------------------------------------------------------------

populate_subscriber(Wlock, C) ->
    random:seed(),
    N = C#config.n_subscribers,
    ?d("    Populate ~p subscribers...", [N]),
    do_populate_subscriber(Wlock, N - 1, C).

do_populate_subscriber(Wlock, Id, C) when Id >= 0 ->
    Suffix = bench_trans:number_to_suffix(Id),
    SubscrId = bench_trans:number_to_key(Id, C),
    Name = list_to_binary([random:uniform(26) + $A - 1]),
    GroupId = random:uniform(C#config.n_groups) - 1,
    Subscr = #subscriber{subscriber_number = SubscrId,
                         subscriber_name   = Name,
                         group_id          = GroupId,
                         location          = 0,
                         active_sessions   = 0,
                         changed_by        = <<"">>,
                         changed_time      = <<"">>,
                         suffix            = Suffix},
    ?APPLY(mnesia, write, [subscriber, Subscr, Wlock]),
    do_populate_subscriber(Wlock, Id - 1, C);
do_populate_subscriber(_Wlock, _, _) ->
    io:format(" totally ~p bytes~n", 
	      [mnesia:table_info(subscriber, memory) * 4]),
    ok.

%% -------------------------------------------------------------------
%% Populate the group table
%% -------------------------------------------------------------------

populate_group(Wlock, C) ->
    random:seed(),
    N = C#config.n_groups,
    ?d("    Populate ~p groups...", [N]),
    do_populate_group(Wlock, N - 1, C).

do_populate_group(Wlock, Id, C) when Id >= 0 ->
    Name = list_to_binary(["-group ", integer_to_list(Id), "-"]),
    Allow = init_allow(C),
    Group = #group{group_id     = Id,
                   group_name   = Name,
                   allow_read   = Allow,
                   allow_insert = Allow,
                   allow_delete = Allow},
    ?APPLY(mnesia, write, [group, Group, Wlock]),
    do_populate_group(Wlock, Id - 1, C);
do_populate_group(_Wlock, _, _) ->
    io:format(" totally ~p bytes~n",
	      [mnesia:table_info(group, memory) * 4]),
    ok.

init_allow(C) ->
    do_init_allow(0, C#config.n_servers - 1).

do_init_allow(Allow, NS) when NS >= 0 ->
    case random:uniform(100) < (90 + 1) of
        true ->
	    ServerBit = 1 bsl NS,
            do_init_allow(Allow bor ServerBit, NS - 1);
        false ->
	    do_init_allow(Allow, NS - 1)
    end;
do_init_allow(Allow, _) ->
    Allow.

%% -------------------------------------------------------------------
%% Populate the server table
%% -------------------------------------------------------------------

populate_server(Wlock, C) ->
    random:seed(),
    N = C#config.n_servers,
    ?d("    Populate ~p servers with 100 records each...", [N]),
    do_populate_server(Wlock, N - 1).

do_populate_server(Wlock, Id) when Id >= 0 ->
    populate_server_suffixes(Wlock, Id, 99),
    do_populate_server(Wlock, Id - 1);
do_populate_server(_Wlock, _) ->
    io:format(" totally ~p bytes~n",
	      [mnesia:table_info(server, memory) * 4]),
    ok.

populate_server_suffixes(Wlock, Id, Suffix) when Suffix >= 0 ->
    Name = list_to_binary(["-server ", integer_to_list(Id), "-"]),
    Server = #server{server_key   = {Id, Suffix},
                     server_name  = Name,
                     no_of_read   = 0,
                     no_of_insert = 0,
                     no_of_delete = 0,
                     suffix       = Suffix},
    ?APPLY(mnesia, write, [server, Server, Wlock]),
    populate_server_suffixes(Wlock, Id, Suffix - 1);
populate_server_suffixes(_Wlock, _, _) ->
    ok.

