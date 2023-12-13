%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2019-2023. All Rights Reserved.
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

-module(phash2_properties).

-ifdef(PROPER).

-include_lib("proper/include/proper.hrl").
-export([prop_phash2_same_with_same_input/0,
         prop_phash2_same_with_same_long_input/0,
         prop_phash2_same_in_different_versions/1,
         prop_phash2_same_in_different_versions_with_long_input/1]).
-proptest([proper]).

%%--------------------------------------------------------------------
%% Properties --------------------------------------------------------
%%--------------------------------------------------------------------

prop_phash2_same_with_same_input() ->
    ?FORALL(T, any(), erlang:phash2(T) =:= erlang:phash2(T)).

prop_phash2_same_with_same_long_input() ->
    ?FORALL(T, any(),
            begin
                BigTerm = lists:duplicate(10000, T),
                erlang:phash2(BigTerm) =:= erlang:phash2(BigTerm)
            end).

prop_phash2_same_in_different_versions(DifferntVersionNode) ->
    ?FORALL(T, any(),
            erlang:phash2(T) =:= rpc:call(DifferntVersionNode,erlang,phash2,[T])).

prop_phash2_same_in_different_versions_with_long_input(DifferntVersionNode) ->
    ?FORALL(T, any(),
            begin
                BigTerm = lists:duplicate(10000, T),
                RpcRes = rpc:call(DifferntVersionNode,erlang,phash2,[BigTerm]),
                LocalRes = erlang:phash2(BigTerm),
                RpcRes =:= LocalRes
            end).

%%--------------------------------------------------------------------
%% Generators  -------------------------------------------------------
%%--------------------------------------------------------------------

-endif.
