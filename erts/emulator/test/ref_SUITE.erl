%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2018. All Rights Reserved.
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

-export([loop_ref/1]).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 2}}].

all() -> 
    [wrap_1, compare_list, compare_ets].

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
