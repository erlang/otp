%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2025. All Rights Reserved.
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
-module(native_record_SUITE).
-include_lib("stdlib/include/assert.hrl").

-export([all/0,suite/0,groups/0,init_per_suite/1,end_per_suite/1,
	 init_per_group/2,end_per_group/2,
         term_order/1,gc/1,external_term_format/1]).

-record #a{x=1, y=2}.
-record #b{a=1, b=2}.

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() ->
    [term_order,
     gc,
     external_term_format].

groups() ->
    [].

init_per_suite(Config) ->
    id(Config),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

term_order(_Config) ->
    RecA = id(#a{}),

    true = RecA > 42,
    true = RecA > 42.0,
    true = RecA > atom,
    true = RecA > make_ref(),
    true = RecA > self(),
    true = RecA > {a,b,c},
    true = RecA < #{},
    true = RecA < [],
    true = RecA < [a],
    true = RecA < ~"abc",

    true = id(#a{}) =/= id(#b{}),

    true = id(#a{}) < id(#b{}),
    true = id(#a{}) =:= id(#a{}),
    true = id(#a{x=1}) < id(#a{x=2}),
    true = id(#a{x=10}) > id(#a{x=0}),

    ok.

gc(_Config) ->
    N = 10000,
    A0 = #a{x=sets:new(), y=#b{b=[]}},
    #a{x=Set0, y=#b{b=List0}} = gc_add(N, A0),
    Set = lists:sort(sets:to_list(Set0)),
    List = lists:sort(List0),
    true = Set =:= List,
    Seq = lists:seq(1, N),
    Seq = List,
    ok.

gc_add(0, A) ->
    A;
gc_add(I, #a{x=S0, y=#b{b=L0}=B0}=A0) ->
    S = sets:add_element(I, S0),
    L = [I|L0],
    A = A0#a{x=S, y=B0#b{b=L}},
    gc_add(I - 1, A).

external_term_format(_Config) ->
    RecA = #a{},
    RecB = #b{a=RecA,b=lists:seq(1, 10)},

    Local = ext_records:local([1,2,3], {RecA, RecB}),
    Vector = #ext_records:vector{},

    Records = [RecA, RecB, Vector, Local],
    Bin = term_to_binary(Records),

    erts_debug:set_internal_state(available_internal_state, true),

    _ = [begin
             io:format("~p\n", [R1]),
             Def1 = record_def(R1),
             Def2 = record_def(R2),

             true = R1 =:= R2,
             true = Def1 =:= Def2,
             true = erts_debug:same(Def1, Def2)
         end || R1 <- Records && R2 <- binary_to_term(Bin)],

    true = code:delete(ext_records),
    _ = code:purge(ext_records),

    %% FIXME: This should fail.
    records:create(ext_records, vector, #{}),

    _ = [begin
             io:format("~p\n", [R1]),
             Def1 = record_def(R1),
             Def2 = record_def(R2),

             true = R1 =:= R2,
             true = Def1 =:= Def2,
             case records:get_module(R1) of
                 ?MODULE ->
                     true = erts_debug:same(Def1, Def2);
                 ext_records ->
                     %% The original definition has been unloaded.
                     false = erts_debug:same(Def1, Def2)
             end
         end || R1 <- Records && R2 <- binary_to_term(Bin)],

    erts_debug:set_internal_state(available_internal_state, false),

    ok.

record_def(R) ->
    erts_debug:get_internal_state({native_record_def, R}).


%%% Common utilities.

id(I) ->
    I.
