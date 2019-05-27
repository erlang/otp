%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2019. All Rights Reserved.
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                             %%%
%%%                       WARNING                               %%%
%%%                                                             %%%
%%% This is experimental code which may be changed or removed   %%%
%%%               anytime without any warning.                  %%%
%%%                                                             %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hash_property_test_SUITE).

-export([suite/0,all/0,groups/0,init_per_suite/1,
         end_per_suite/1,init_per_group/2,end_per_group/2]).

-export([test_phash2_no_diff/1,
         test_phash2_no_diff_long/1,
         test_phash2_no_diff_between_versions/1]).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{ct_hooks,[ts_install_cth]}].

all() -> [{group, proper}].

groups() ->
    [{proper, [], [test_phash2_no_diff,
                   test_phash2_no_diff_long,
                   test_phash2_no_diff_between_versions]}].


%%% First prepare Config and compile the property tests for the found tool:
init_per_suite(Config) ->
    ct_property_test:init_per_suite(Config).

end_per_suite(Config) ->
    Config.

%%% Only proper is supported
init_per_group(proper, Config) ->
    case proplists:get_value(property_test_tool,Config) of
	proper -> Config;
	X -> {skip, lists:concat([X," is not supported"])}
    end;
init_per_group(_, Config) ->
    Config.

end_per_group(_, Config) ->
    Config.

test_phash2_no_diff(Config) when is_list(Config) ->
    true = ct_property_test:quickcheck(
             phash2_properties:prop_phash2_same_with_same_input(),
             Config).

test_phash2_no_diff_long(Config) when is_list(Config) ->
    true = ct_property_test:quickcheck(
             phash2_properties:prop_phash2_same_with_same_long_input(),
             Config).

test_phash2_no_diff_between_versions(Config) when is_list(Config) ->
    R = "21",
    case test_server:is_release_available(R) of
        true ->
            Rel = {release,R},
            case test_server:start_node(rel21,peer,[{erl,[Rel]}]) of
                {error, Reason} -> {skip, io_lib:format("Could not start node: ~p~n", [Reason])};
                {ok, Node} ->
                    try
                        true = ct_property_test:quickcheck(
                                 phash2_properties:prop_phash2_same_in_different_versions(Node),
                                 Config),
                        true = ct_property_test:quickcheck(
                                 phash2_properties:prop_phash2_same_in_different_versions_with_long_input(Node),
                                 Config)
                    after
                        test_server:stop_node(Node)
                    end
            end;
        false ->
            {skip, io_lib:format("Release ~s not available~n", [R])}
    end.
