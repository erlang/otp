%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2016. All Rights Reserved.
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

-module(old_mod).
-compile(r10).

-export([sort_on_old_node/1, sorter/3]).

-include_lib("common_test/include/ct.hrl").

sorter(Receiver, Ref, List) ->
    Receiver ! {Ref, lists:sort(List)}.

sort_on_old_node(List) when is_list(List) ->
    OldVersion = "r10",
    Pa = filename:dirname(code:which(?MODULE)),
    {X, Y, Z} = now(),
    NodeName = list_to_atom(OldVersion 
                            ++ "_"
                            ++ integer_to_list(X)
                            ++ integer_to_list(Y)
                            ++ integer_to_list(Z)),
    {ok, Node} = test_server:start_node(NodeName,
                                        peer,
                                        [{args, " -pa " ++ Pa},
                                         {erl, [{release, OldVersion++"b_patched"}]}]),
    Ref = make_ref(),
    spawn_link(Node, ?MODULE, sorter, [self(), Ref, List]),
    SortedPids = receive {Ref, SP} -> SP end,
    true = test_server:stop_node(Node),
    SortedPids.
