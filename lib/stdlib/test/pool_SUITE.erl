%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2018. All Rights Reserved.
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
-module(pool_SUITE).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1,
	 init_per_group/2,end_per_group/2]).
-export([basic/1, link_race/1, echo/1]).

suite() -> [{ct_hooks,[{ts_install_cth,[{nodenames, 1}]}]}].

all() ->
    [basic, link_race].

groups() ->
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

basic(Config) ->

    {ok, Node, PoolNode} = init_pool(pool_SUITE_basic, basic, Config),

    Node = rpc:call(Node, pool, get_node, []),
    PoolNode = rpc:call(Node, pool, get_node, []),

    do_echo(Node),
    do_echo(Node),

    test_server:stop_node(Node),
    ok.

link_race(Config) ->

    {ok, Node, PoolNode} = init_pool(pool_SUITE_basic, basic, Config),

    Node = rpc:call(Node, pool, get_node, []),
    PoolNode = rpc:call(Node, pool, get_node, []),

    rpc:call(Node, pool, pspawn_link, [erlang, is_atom, [?MODULE]]),

    test_server:stop_node(Node),
    ok.

do_echo(Node) ->
    EchoPid = rpc:call(Node, pool, pspawn, [?MODULE, echo, [self()]]),
    Ref = EchoPid ! make_ref(),
    receive Ref -> ok after 1000 -> ct:fail(receive M -> M after 0 -> timeout end) end.

echo(Parent) ->
    receive Msg ->
            Parent ! Msg
    end.


init_pool(Proxy, Name, Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    [Slave] = proplists:get_value(nodenames, Config),
    {ok, Node} = test_server:start_node(Proxy, slave, []),
    {ok, Hostname} = inet:gethostname(),
    file:write_file(filename:join(PrivDir,".hosts.erlang"),"'"++Hostname++"'.\n"),
    ok = rpc:call(Node, file, set_cwd, [PrivDir]),

    [PoolNode] = rpc:call(Node, pool, start, [Name]),

    Nodes = rpc:call(Node, pool, get_nodes, []),
    [rpc:call(N, code, add_patha, [filename:dirname(code:which(?MODULE))]) || N <- Nodes],

    {ok, Node, PoolNode}.
