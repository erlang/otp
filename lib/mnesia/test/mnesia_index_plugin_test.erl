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

%%
-module(mnesia_index_plugin_test).
-author('ulf@wiger.net').

-export([init_per_testcase/2, end_per_testcase/2,
         init_per_group/2, end_per_group/2,
         init_per_suite/1, end_per_suite/1,
         all/0, groups/0]).

-export([
         add_rm_plugin/1,
         tab_with_plugin_index/1,
         tab_with_multiple_plugin_indexes/1,
         ix_match_w_plugin/1,
         ix_match_w_plugin_ordered/1,
         ix_match_w_plugin_bag/1,
         ix_update_w_plugin/1
        ]).

-export([ix_prefixes/3,    % test plugin
         ix_prefixes2/3]). % test plugin 2

-include("mnesia_test_lib.hrl").

init_per_suite(Conf) ->
    Conf.

end_per_suite(Conf) ->
    Conf.

init_per_testcase(Func, Conf) ->
    mnesia_test_lib:init_per_testcase(Func, Conf).

end_per_testcase(Func, Conf) ->
    mnesia_test_lib:end_per_testcase(Func, Conf).

all() ->
    [add_rm_plugin,
     tab_with_plugin_index,
     tab_with_multiple_plugin_indexes,
     ix_match_w_plugin,
     ix_match_w_plugin_ordered,
     ix_match_w_plugin_bag,
     ix_update_w_plugin].

groups() ->
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


add_rm_plugin(suite) -> [];
add_rm_plugin(Config) when is_list(Config) ->
    [N1, N2] = Nodes = ?acquire_nodes(2, Config),
    ok = add_plugin(),
    ok = rpc_check_plugin(N1),
    ok = rpc_check_plugin(N2),
    ok = add_plugin2(),
    ok = del_plugin(),
    ok = del_plugin2(),
    ok = add_plugin(),
    ok = add_plugin2(),
    ok = del_plugin(),
    ok = del_plugin2(),
    ?verify_mnesia(Nodes, []).

-define(PLUGIN1, {{pfx},?MODULE,ix_prefixes}).
-define(PLUGIN2, {{pfx2},?MODULE,ix_prefixes2}).

add_plugin() ->
    {atomic, ok} = mnesia_schema:add_index_plugin({pfx}, ?MODULE, ix_prefixes),
    [?PLUGIN1] = mnesia_schema:index_plugins(),
    ok.

add_plugin2() ->
    {atomic, ok} = mnesia_schema:add_index_plugin({pfx2}, ?MODULE, ix_prefixes2),
    [?PLUGIN1, ?PLUGIN2] = lists:sort(mnesia_schema:index_plugins()),
    ok.

del_plugin() ->
    {atomic, ok} = mnesia_schema:delete_index_plugin({pfx}),
    [?PLUGIN2] = mnesia_schema:index_plugins(),
    ok.

del_plugin2() ->
    {atomic, ok} = mnesia_schema:delete_index_plugin({pfx2}),
    [] = mnesia_schema:index_plugins(),
    ok.

rpc_check_plugin(N) ->
    [?PLUGIN1] =
        rpc:call(N, mnesia_schema, index_plugins, []),
    ok.

tab_with_plugin_index(suite) -> [];
tab_with_plugin_index(Config) when is_list(Config) ->
    [_N1] = Nodes = ?acquire_nodes(1, Config),
    ok = add_plugin(),
    {atomic, ok} = mnesia:create_table(t, [{attributes, [k,v1,v2]},
                                           {index, [{{pfx}, ordered},
                                                    {v1, ordered},
                                                    v2]}]),
    [ok,ok,ok,ok] =
        [mnesia:dirty_write({t, K, V1, V2})
         || {K,V1,V2} <- [{1,a,"123"},
                          {2,b,"12345"},
                          {3,c,"6789"},
                          {4,d,nil}]],
    [{t,1,a,"123"},{t,2,b,"12345"}] =
        mnesia:dirty_index_read(t,<<"123">>,{pfx}),
    [{t,3,c,"6789"}] =
        mnesia:dirty_index_read(t,"6789",v2),
    [{t,1,a,"123"}] =
        mnesia:dirty_match_object({t,'_',a,"123"}),
    [{t,1,a,"123"}] =
        mnesia:dirty_select(t, [{ {t,'_',a,"123"}, [], ['$_']}]),
    mnesia:dirty_delete(t,2),
    [{t,1,a,"123"}] =
        mnesia:dirty_index_read(t,<<"123">>,{pfx}),
    ?verify_mnesia(Nodes, []).

tab_with_multiple_plugin_indexes(suite) -> [];
tab_with_multiple_plugin_indexes(Config) when is_list(Config) ->
    [_N1] = Nodes = ?acquire_nodes(1, Config),
    ok = add_plugin(),
    ok = add_plugin2(),
    {atomic, ok} =
        mnesia:create_table(u, [{attributes, [k,v1,v2]},
                                {index, [{{pfx}, ordered},
                                         {{pfx2}, ordered}]}]),
        [ok,ok,ok,ok] =
        [mnesia:dirty_write({u, K, V1, V2})
         || {K,V1,V2} <- [{1,a,"123"},
                          {2,b,"12345"},
                          {3,c,"6789"},
                          {4,d,nil}]],
    [{u,1,a,"123"},{u,2,b,"12345"}] =
        mnesia:dirty_index_read(u,<<"123">>,{pfx}),
    [{u,1,a,"123"},{u,2,b,"12345"}] =
        mnesia:dirty_index_read(u,<<"321">>,{pfx2}),
    ?verify_mnesia(Nodes, []).

ix_match_w_plugin(suite) -> [];
ix_match_w_plugin(Config) when is_list(Config) ->
    [_N1] = Nodes = ?acquire_nodes(1, Config),
    ok = add_plugin(),
    {atomic, ok} = mnesia:create_table(im1, [{attributes, [k, v1, v2]},
                                             {index, [{{pfx}, ordered},
                                                      {v1, ordered}]}]),
    fill_and_test_index_match(im1, set),
    ?verify_mnesia(Nodes, []).


ix_match_w_plugin_ordered(suite) -> [];
ix_match_w_plugin_ordered(Config) when is_list(Config) ->
    [_N1] = Nodes = ?acquire_nodes(1, Config),
    ok = add_plugin(),
    {atomic, ok} = mnesia:create_table(im2, [{attributes, [k, v1, v2]},
                                             {type, ordered_set},
                                             {index, [{{pfx}, ordered},
                                                      {v1, ordered}]}]),
    fill_and_test_index_match(im2, ordered_set),
    ?verify_mnesia(Nodes, []).

ix_match_w_plugin_bag(suite) -> [];
ix_match_w_plugin_bag(Config) when is_list(Config) ->
    [_N1] = Nodes = ?acquire_nodes(1, Config),
    ok = add_plugin(),
    {atomic, ok} = mnesia:create_table(im3, [{attributes, [k, v1, v2]},
                                             {type, bag},
                                             {index, [{{pfx}, ordered},
                                                      {v1, ordered}]}]),
    fill_and_test_index_match(im3, bag),
    ?verify_mnesia(Nodes, []).

ix_update_w_plugin(suite) -> [];
ix_update_w_plugin(Config) when is_list(Config) ->
    [_N1] = Nodes = ?acquire_nodes(1, Config),
    ok = add_plugin(),
    {atomic, ok} = mnesia:create_table(im4, [{attributes, [k, v1, v2]},
                                             {type, ordered_set},
                                             {index, [{{pfx}, ordered},
                                                      {v1, ordered}]}]),

    mnesia:dirty_write({im4, 1, "1234", "abcd"}),
    ?match([{im4, 1, "1234", "abcd"}], mnesia:dirty_index_read(im4, <<"123">>, {pfx})),
    ?match([{im4, 1, "1234", "abcd"}], mnesia:dirty_index_read(im4, <<"abc">>, {pfx})),
    mnesia:dirty_write({im4, 1, "1234", "efgh"}),
    ?match([{im4, 1, "1234", "efgh"}], mnesia:dirty_index_read(im4, <<"123">>, {pfx})),
    ?match([{im4, 1, "1234", "efgh"}], mnesia:dirty_index_read(im4, <<"efg">>, {pfx})),
    ?verify_mnesia(Nodes, []).

fill_and_test_index_match(Tab, Type) ->
    [ok,ok,ok,ok,ok,ok,ok,ok,ok] =
        [mnesia:dirty_write({Tab, K, V1, V2})
         || {K,V1,V2} <- [{1,a,"123"},
                          {2,b,"12345"},
                          {3,c,"123"},
                          {4,d,nil},
                          {5,e,nil},
                          {6,f,nil},
                          {7,g,nil},  %% overwritten if not bag
                          {7,g,"234"},
                          {8,h,"123"}]],
    mnesia:activity(
      transaction,
      fun() ->
              ok = mnesia:write({Tab, 1, aa, "1234"}), %% replaces if not bag
              ok = mnesia:delete({Tab, 2}),
              ok = mnesia:delete({Tab, 4}),
              ok = mnesia:write({Tab, 6, ff, nil}),
              ok = mnesia:write({Tab, 7, gg, "123"}),
              ok = mnesia:write({Tab, 100, x, nil}),
              ok = mnesia:delete_object({Tab,3,c,"123"}),
              ok = mnesia:delete_object({Tab,5,e,nil}),
              Res = mnesia:index_read(Tab, <<"123">>, {pfx}),
              SetRes = [{Tab,1,aa,"1234"}, {Tab,7,gg,"123"}, {Tab,8,h,"123"}],
              case Type of
                  set ->
                      SetRes = lists:sort(Res);
                  ordered_set ->
                      SetRes = Res;
                  bag ->
                      [{Tab,1,a,"123"}, {Tab,1,aa,"1234"},
                       {Tab,7,gg,"123"}, {Tab,8,h,"123"}] = lists:sort(Res)
              end
      end).

%% ============================================================
%%
ix_prefixes(_Tab, _Pos, Obj) ->
    lists:foldl(
      fun(V, Acc) when is_list(V) ->
              try Pfxs = prefixes(list_to_binary(V)),
                   Pfxs ++ Acc
              catch
                  error:_ ->
                      Acc
              end;
         (V, Acc) when is_binary(V) ->
              Pfxs = prefixes(V),
              Pfxs ++ Acc;
         (_, Acc) ->
              Acc
      end, [], tl(tuple_to_list(Obj))).

ix_prefixes2(Tab, Pos, Obj) ->
    [rev(P) || P <- ix_prefixes(Tab, Pos, Obj)].

rev(B) when is_binary(B) ->
    list_to_binary(lists:reverse(binary_to_list(B))).

prefixes(<<P:3/binary, _/binary>>) ->
    [P];
prefixes(_) ->
    [].
