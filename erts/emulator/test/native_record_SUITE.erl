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
         term_order/1,gc/1,external_term_format/1,
         messages/1,records_module/1]).

-record #a{x=1, y=2}.
-record #b{x=none, y=none, z=none}.
-record #bb{a=1, b=2}.
-record #c{x::integer, y=0::integer, z=[]}.
-record #empty{}.

-record #big{f1, f2, f3, f4, f5, f6, f7, f8,
              f9, f10, f11, f12, f13, f14, f15, f16,
              f17, f18, f19, f20, f21, f22, f23, f24,
              f25, f26, f27, f28, f29, f30, f31, f32,
              f33, f34, f35, f36, f37, f38, f39, f40,
              f41, f42, f43, f44, f45, f46, f47, f48,
              f49, f50, f51, f52, f53, f54, f55, f56,
              f57, f58, f59, f60, f61, f62, f63, f64}.

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() ->
    [term_order,
     gc,
     external_term_format,
     messages,
     records_module].

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

    true = id(#a{}) =/= id(#bb{}),

    true = id(#a{}) < id(#bb{}),
    true = id(#bb{}) < id(#c{x=0}),

    true = id(#a{}) =:= id(#a{}),
    true = id(#a{x=1}) < id(#a{x=2}),
    true = id(#a{x=10}) > id(#a{x=0}),

    term_order_module(),
    term_order_name(),
    term_order_visibility(),
    term_order_num_fields(),
    term_order_keys(),
    term_order_values(),

    ok.

term_order_module() ->
    RecA = id(#a{}),

    true = is_gt(RecA, fake_record(aa_module, a, false, [{a,0}])),
    true = is_gt(RecA, fake_record(aa_module, a, false, [{x,0}])),
    true = is_gt(RecA, fake_record(aa_module, a, false, [{a,0},{b,0},{c,0}])),
    true = is_gt(RecA, fake_record(aa_module, a, false, [{x,0},{y,0},{z,0}])),

    true = is_lt(RecA, fake_record(zz_module, a, false, [{a,0}])),
    true = is_lt(RecA, fake_record(zz_module, a, false, [{x,0}])),
    true = is_lt(RecA, fake_record(zz_module, a, false, [{a,0},{b,0},{c,0}])),
    true = is_lt(RecA, fake_record(zz_module, a, false, [{x,0},{y,0},{z,0}])),

    ok.

term_order_name() ->
    RecB = id(#b{}),

    true = is_gt(RecB, fake_record(a, false, [{a,0}])),
    true = is_gt(RecB, fake_record(a, false, [{x,0}])),
    true = is_gt(RecB, fake_record(a, false, [{a,0},{b,0},{c,0}])),
    true = is_gt(RecB, fake_record(a, false, [{x,0},{y,0},{z,0}])),

    true = is_lt(RecB, fake_record(z, false, [{a,0}])),
    true = is_lt(RecB, fake_record(z, false, [{x,0}])),
    true = is_lt(RecB, fake_record(z, false, [{a,0},{b,0},{c,0}])),
    true = is_lt(RecB, fake_record(z, false, [{x,0},{y,0},{z,0}])),

    false = is_lt(#empty{}, #empty{}),
    ok.

term_order_visibility() ->
    RecA = id(#a{}),

    true = is_lt(RecA, fake_record(a, true, [{x,1}, {y,2}])),

    ok.

term_order_num_fields() ->
    RecA = id(#a{}),
    true = is_gt(RecA, fake_record(a, false, [{a,0}])),
    true = is_gt(RecA, fake_record(a, false, [{z,0}])),
    true = is_lt(RecA, fake_record(a, false, [{a,0}, {b,0}, {c,0}])),
    true = is_lt(RecA, fake_record(a, false, [{z,0}, {w,0}, {q,0}])),

    ok.

term_order_keys() ->
    RecA = id(#a{}),

    true = is_gt(RecA, fake_record(a, false, [{a,0}, {b,0}])),
    true = is_gt(RecA, fake_record(a, false, [{a,10}, {b,100}])),
    true = is_gt(RecA, fake_record(a, false, [{b,100}, {a,10}])),
    true = is_gt(RecA, fake_record(a, false, [{b,0}, {a,0}])),
    true = is_gt(RecA, fake_record(a, false, [{b,2}, {a,1}])),

    true = is_lt(RecA, fake_record(a, false, [{y,0},{z,0}])),
    true = is_lt(RecA, fake_record(a, false, [{y,10},{z,100}])),
    true = is_lt(RecA, fake_record(a, false, [{y,0}, {x,0}])),

    true = is_lt(fake_record(order, false, [{wwww,3}, {zzzz,0}, {true,1}, {aaaa,2}]),
                 fake_record(order, false, [{zzzz,0}, {true,1}, {aaaa,2}, {wwww,3}])),

    true = is_lt(fake_record(order, false, [{zzzz,0}, {true,1}, {aaaa,2}, {wwww,3}]),
                 fake_record(order, false, [{zzzz,0}, {true,1}, {wwww,3}, {aaaa,2}])),

    true = is_lt(fake_record(a, false, [{a,0}, {b,0}]),
                 fake_record(a, false, [{b,0}, {a,0}])),

    ok.

term_order_values() ->
    true = is_lt(fake_record(r, false, [{a,0}, {b,10}]),
                 fake_record(r, false, [{a,10}, {b,5}])),
    true = is_lt(fake_record(r, false, [{b,0}, {a,10}]),
                 fake_record(r, false, [{b,10}, {a,5}])),
    true = is_lt(fake_record(r, false, [{a,[0]}, {b,[10]}]),
                 fake_record(r, false, [{a,[10]}, {b,[5]}])),
    true = is_lt(fake_record(r, false, [{b,{0}}, {a,{10}}]),
                 fake_record(r, false, [{b,{10}}, {a,{5}}])),
    ok.

is_gt(A, B) ->
    Res = id(A > B),
    Res = id(B < A),
    Res.

is_lt(A, B) ->
    Res = id(A < B),
    Res = id(B > A),
    Res.

gc(_Config) ->
    N = 10000,
    A0 = #a{x=sets:new(), y=#bb{b=[]}},
    #a{x=Set0, y=#bb{b=List0}} = gc_add(N, A0),
    Set = lists:sort(sets:to_list(Set0)),
    List = lists:sort(List0),
    true = Set =:= List,
    Seq = lists:seq(1, N),
    Seq = List,
    ok.

gc_add(0, A) ->
    A;
gc_add(I, #a{x=S0, y=#bb{b=L0}=B0}=A0) ->
    S = sets:add_element(I, S0),
    L = [I|L0],
    A = A0#a{x=S, y=B0#bb{b=L}},
    gc_add(I - 1, A).

external_term_format(_Config) ->
    RecA = #a{},
    RecB = #bb{a=RecA,b=lists:seq(1, 10)},

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

    ?assertError({badrecord,ExtLocal}, records:create(ext_records, vector, #{})),
    ?assertError({badrecord,ExtLocal}, #ext_records:vector{}),

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

    _ = code:purge(ext_records),

    ok.

record_def(R) ->
    erts_debug:get_internal_state({native_record_def, R}).

messages(Config) ->
    Local = ext_records:local([1,2,id(3)], [length(Config)]),

    Echo = spawn_link(fun echo_loop/0),

    _ = [begin
             R = #a{x={counter,I}, y=Local},
             Echo ! {self(),R},
             receive
                 {ok,Echo,R} ->
                     ok;
                 Other ->
                     error({unexpected,Other})
             after 10_000 ->
                     error(timeout)
             end
         end || I <- lists:seq(1, 1000)],

    ok.

echo_loop() ->
    receive
        {From,Msg} ->
            From ! {ok,self(),Msg},
            echo_loop()
    end.

records_module(_Config) ->
    ARec = id(#a{x=1, y=2}),
    BRec = id(#b{}),
    CRec = id(#c{x=42, y=100}),

    false = records:is_exported(ARec),
    false = records:is_exported(BRec),
    false = records:is_exported(CRec),

    ARec = records:create(?MODULE, a, #{x=>1, y=>2}),
    BRec = records:create(?MODULE, b, #{}),
    CRec = records:create(?MODULE, c, #{x=>42, y=>100}),

    ?assertError({badmap,{a,b,c}}, records:create(?MODULE, b, {a,b,c})),
    ?assertError({badfield,qqq}, records:create(?MODULE, b, #{qqq => aaa})),
    ?assertError({badfield,{really,bad}},
                 records:create(?MODULE, b, #{{really,bad} => value})),
    ?assertError({novalue,x}, records:create(?MODULE, c, #{})),

    Fields = [list_to_atom("f"++integer_to_list(I)) ||
                 I <- lists:seq(1, 64)],
    BigRecord = id(#big{f1=1, f2=2, f3=3, f4=4, f5=5, f6=6, f7=7, f8=8,
                        f9=9, f10=10, f11=11, f12=12, f13=13, f14=14, f15=15, f16=16,
                        f17=17, f18=18, f19=19, f20=20, f21=21, f22=22, f23=23, f24=24,
                        f25=25, f26=26, f27=27, f28=28, f29=29, f30=30, f31=31, f32=32,
                        f33=33, f34=34, f35=35, f36=36, f37=37, f38=38, f39=39, f40=40,
                        f41=41, f42=42, f43=43, f44=44, f45=45, f46=46, f47=47, f48=48,
                        f49=49, f50=50, f51=51, f52=52, f53=53, f54=54, f55=55, f56=56,
                        f57=57, f58=58, f59=59, f60=60, f61=61, f62=62, f63=63, f64=64}),
    LargeMap0 = #{Field => I || Field <- Fields && I <- lists:seq(1, 64)},
    BigRecord = records:create(?MODULE, big, LargeMap0),

    LargeMap1 = LargeMap0#{whatever => 42},
    ?assertError({badfield,whatever}, records:create(?MODULE, big, LargeMap1)),

    LargeMap2 = maps:remove(f42, LargeMap0),
    ?assertError({novalue,f42}, records:create(?MODULE, big, LargeMap2)),

    [x,y] = records:get_field_names(ARec),
    [x,y,z] = records:get_field_names(BRec),
    [x,y,z] = records:get_field_names(CRec),

    R0 = #b{},
    R0 = R0#b{},
    R1 = records:update(R0, ?MODULE, b, #{x=>foo}),
    #b{x=foo, y=none, z=none} = id(R1),
    #?MODULE:b{x=foo, y=none, z=none} = id(R1),

    ?assertError({badmap,not_a_map}, records:update(CRec, ?MODULE, c, not_a_map)),
    ?assertError({badfield,a}, records:update(CRec, ?MODULE, c, #{a => b})),
    ?assertError({badfield,{really,bad}},
                 records:update(CRec, ?MODULE, c, #{{really,bad} => b})),

    LargeUpdate = #{Field => I * 2 || Field <- Fields && I <- lists:seq(1, 64)},
    UpdatedBigRecord =
        BigRecord#big{f1=2, f2=4, f3=6, f4=8, f5=10, f6=12, f7=14, f8=16,
                      f9=18, f10=20, f11=22, f12=24, f13=26, f14=28, f15=30, f16=32,
                      f17=34, f18=36, f19=38, f20=40, f21=42, f22=44, f23=46, f24=48,
                      f25=50, f26=52, f27=54, f28=56, f29=58, f30=60, f31=62, f32=64,
                      f33=66, f34=68, f35=70, f36=72, f37=74, f38=76, f39=78, f40=80,
                      f41=82, f42=84, f43=86, f44=88, f45=90, f46=92, f47=94, f48=96,
                      f49=98, f50=100, f51=102, f52=104, f53=106, f54=108, 
                      f55=110, f56=112, f57=114, f58=116, f59=118, f60=120,
                      f61=122, f62=124, f63=126, f64=128},
    UpdatedBigRecord = records:update(BigRecord, ?MODULE, big, LargeUpdate),

    ?assertError({badfield,whatever}, records:update(BigRecord, ?MODULE, big, LargeMap1)),

    N = 10000,
    #a{x=N,y=0} =
        lists:foldl(fun(_, #a{x=X,y=Y}=R) ->
                            records:update(R, ?MODULE, a, #{x=>X+1,y=>Y-1})
                    end, #a{x=0,y=N}, lists:seq(1, N)),
    ok.


%%% Common utilities.

fake_record(Name, Exp, Fs) ->
    fake_record(?MODULE, Name, Exp, Fs).

fake_record(Mod0, Name0, Exp0, Fs) ->
    Ext = 131,
    RecordExt = $C,

    Mod = fake_term(Mod0),
    Name = fake_term(Name0),
    Exp = if Exp0 -> 1; true -> 0 end,

    N = length(Fs),

    Defs = << <<(fake_term(K))/binary>> || {K,_} <- Fs >>,
    Vs = << <<(fake_term(V))/binary>> || {_,V} <- Fs >>,

    Bin = <<Ext,RecordExt,N:32,Exp,Mod/binary,Name/binary,Defs/binary,Vs/binary>>,
    binary_to_term(Bin).

fake_term(Term) ->
    <<131,Bin/binary>> = term_to_binary(Term),
    Bin.

id(I) ->
    I.
