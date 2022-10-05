%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2022. All Rights Reserved.
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

-module(ref_SUITE).

-export([all/0, suite/0]).
-export([wrap_1/1]).
-export([compare_list/1, compare_ets/1]).
-export([internal_size/1, external_size/1]).

-export([init_per_testcase/2, end_per_testcase/2]).

-export([loop_ref/1]).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 2}}].

init_per_testcase(Func, Config) when is_atom(Func), is_list(Config) ->
    [{testcase, Func}|Config].

end_per_testcase(Func, Config) when is_atom(Func), is_list(Config) ->
    erts_test_utils:ept_check_leaked_nodes(Config).

all() -> 
    [wrap_1, compare_list, compare_ets, internal_size, external_size].

%% Check that refs don't wrap around easily.
wrap_1(Config) when is_list(Config) ->
    spawn_link(?MODULE, loop_ref, [self()]),
    receive
        done ->
            ct:fail(wrapfast)
    after 30000 ->
              ok
    end,
    ok.

loop_ref(Parent) ->
    Ref0 = make_ref(),
    loop_ref(Ref0, first, 0),
    Parent ! done.

loop_ref(R, R, _) -> ok;
loop_ref(R0, _, N) ->
    loop_ref(R0, make_ref(), N+1).

%% Check that ref ordering works
compare_list(Config) when is_list(Config) ->
    %% Although this test uses external refs, it would apply the same to plain refs
    ExtRef1 = <<131,114,0,3,100,0,3,110,64,98,3, 0,0,173,156, 0,216,0,4, 0,0,0,0>>,
    ExtRef2 = <<131,114,0,3,100,0,3,110,64,98,3, 0,1,31,27,   129,4,0,1, 0,0,0,0>>,

    Ref1 = binary_to_term(ExtRef1), %% #Ref<n@b.0.14155780.44444>
    Ref2 = binary_to_term(ExtRef2), %% #Ref<n@b.0.2164523009.73499>
    OrderedList = [Ref1, Ref2],
    OrderedList = lists:sort(OrderedList),
    ok.

%% This is the scarier case since it makes terms "invisible" in ets or Mnesia
%% (the underlying fault cause is the same as compare_list/1)
compare_ets(Config) when is_list(Config) ->
    W2s = [610350147,899574699,2994196869,686384822,2397690439, 923302211],
    ExtRefBase = <<131,114,0,3,100,0,3,110,64,98,3>>,
    ExtRefs = [<<ExtRefBase/binary, 1:32, W2:32, 0:32>> || W2 <- W2s],
    Refs = [binary_to_term(Bin) || Bin <- ExtRefs],

    Ets = ets:new(refbug, [ordered_set]),
    ets:insert(Ets, [{Ref,Ref} || Ref <- Refs]),
    0 = length([R || R <- ets:tab2list(Ets), ets:lookup(Ets, element(1,R)) == []]),
    ok.

internal_size(Config) when is_list(Config) ->
    %% Verifies that the range of heap size used for internal references
    %% matches what the documentation say in the advanced chapter of the
    %% efficiency guide. Note that the values in the efficiency guide
    %% also add the word referencing the heap structure.

    %% Ordinary internal reference
    ORef = check_internal_size(make_ref()),
    io:format("ORef = ~p~n", [ORef]),

    %% Internal pid reference (reference containing a pid)
    PRef = check_internal_size(alias()),
    io:format("PRef = ~p~n", [PRef]),

    %% Internal magic reference
    MRef = check_internal_size(ets:new(blipp, [])),
    io:format("MRef = ~p~n", [MRef]),

    ok.

check_internal_size(Ref) when is_reference(Ref), node(Ref) == node() ->
    case erlang:system_info(wordsize) of
        4 ->
            case erts_debug:size(Ref) of
                Sz when 3 =< Sz, Sz =< 6 ->
                    Sz;
                Sz ->
                    error({internal_ref_size_out_of_range, Sz})
            end;
        8 ->
            case erts_debug:size(Ref) of
                Sz when 3 =< Sz, Sz =< 5 ->
                    Sz;
                Sz ->
                    error({internal_ref_size_out_of_range, Sz})
            end
    end.

external_size(Config) when is_list(Config) ->
    %% Verifies that the range of heap size used for external references
    %% matches what the documentation say in the advanced chapter of the
    %% efficiency guide. Note that the values in the efficiency guide
    %% also add the word referencing the heap structure.
    {ok, Peer, Node} = ?CT_PEER(),

    %% Ordinary external reference
    ORef = check_external_size(erpc:call(Node, fun () -> make_ref() end)),
    io:format("ORef = ~p~n", [ORef]),

    %% External pid reference (reference containing a pid) (nothing produce
    %% this yet, but we need to handle it)
    PRef = check_external_size(erts_test_utils:mk_ext_ref({Node, 4711},
                                                          [1, 2, 3, 4, 5])),
    io:format("PRef = ~p~n", [PRef]),


    peer:stop(Peer),
    ok.

check_external_size(Ref) when is_reference(Ref) ->
    case erlang:system_info(wordsize) of
        4 ->
            case erts_debug:size(Ref) of
                Sz when 6 =< Sz, Sz =< 8 ->
                    Sz;
                Sz ->
                    error({internal_ref_size_out_of_range, Sz})
            end;
        8 ->
            case erts_debug:size(Ref) of
                Sz when 5 =< Sz, Sz =< 6 ->
                    Sz;
                Sz ->
                    error({internal_ref_size_out_of_range, Sz})
            end
    end.
