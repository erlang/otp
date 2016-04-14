%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2014. All Rights Reserved.
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

-module(nif_SUITE).

%%-define(line_trace,true).
-define(CHECK(Exp,Got), check(Exp,Got,?LINE)).
%%-define(CHECK(Exp,Got), ?line Exp = Got).

-include_lib("test_server/include/test_server.hrl").

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, 
	 init_per_testcase/2, 
	 end_per_testcase/2, basic/1, reload/1, upgrade/1, heap_frag/1,
	 types/1, many_args/1, binaries/1, get_string/1, get_atom/1,
	 maps/1,
	 api_macros/1,
	 from_array/1, iolist_as_binary/1, resource/1, resource_binary/1, 
	 resource_takeover/1,
	 threading/1, send/1, send2/1, send3/1, send_threaded/1, neg/1, 
	 is_checks/1,
	 get_length/1, make_atom/1, make_string/1, reverse_list_test/1,
	 otp_9828/1,
	 otp_9668/1, consume_timeslice/1, dirty_nif/1, dirty_nif_send/1,
	 dirty_nif_exception/1, call_dirty_nif_exception/1, nif_schedule/1,
	 nif_exception/1, call_nif_exception/1,
	 nif_nan_and_inf/1, nif_atom_too_long/1,
	 nif_monotonic_time/1, nif_time_offset/1, nif_convert_time_unit/1
	]).

-export([many_args_100/100]).


%% -export([lib_version/0,call_history/0,hold_nif_mod_priv_data/1,nif_mod_call_history/0,
%% 	 list_seq/1,type_test/0,tuple_2_list/1,is_identical/2,compare/2,
%% 	 clone_bin/1,make_sub_bin/3,string_to_bin/2,atom_to_bin/2,macros/1,
%% 	 tuple_2_list_and_tuple/1,iolist_2_bin/1,get_resource_type/1,alloc_resource/2,
%% 	 make_resource/1,get_resource/2,release_resource/1,last_resource_dtor_call/0, suite/0,
%% 	 make_new_resource/2,make_new_resource_binary/1,send_list_seq/2,send_new_blob/2,
%% 	 alloc_msgenv/0,clear_msgenv/1,grow_blob/2,send_blob/2,send_blob_thread/3,
%% 	 join_send_thread/1]).


-define(nif_stub,nif_stub_error(?LINE)).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [basic, reload, upgrade, heap_frag, types, many_args,
     binaries, get_string, get_atom, maps, api_macros, from_array,
     iolist_as_binary, resource, resource_binary,
     resource_takeover, threading, send, send2, send3,
     send_threaded, neg, is_checks, get_length, make_atom,
     make_string,reverse_list_test,
     otp_9828,
     otp_9668, consume_timeslice,
     nif_schedule, dirty_nif, dirty_nif_send, dirty_nif_exception,
     nif_exception, nif_nan_and_inf, nif_atom_too_long,
     nif_monotonic_time, nif_time_offset, nif_convert_time_unit
    ].

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


init_per_testcase(_Case, Config) ->
%    ?line Dog = ?t:timetrap(?t:seconds(60*60*24)),
    Config.

end_per_testcase(_Func, _Config) ->
    %%Dog = ?config(watchdog, Config),
    %%?t:timetrap_cancel(Dog),
    P1 = code:purge(nif_mod),
    Del = code:delete(nif_mod),
    P2 = code:purge(nif_mod),
    io:format("fin purged=~p, deleted=~p and then purged=~p\n",[P1,Del,P2]).

basic(doc) -> ["Basic smoke test of load_nif and a simple NIF call"];
basic(suite) -> [];
basic(Config) when is_list(Config) ->
    ensure_lib_loaded(Config),
    ?line true = (lib_version() =/= undefined),
    ?line [{load,1,1,101},{lib_version,1,2,102}] = call_history(),
    ?line [] = call_history(),
    ?line true = lists:member(?MODULE, erlang:system_info(taints)),
    ok.

reload(doc) -> ["Test reload callback in nif lib"];
reload(suite) -> [];  
reload(Config) when is_list(Config) ->    
    TmpMem = tmpmem(),
    ensure_lib_loaded(Config),

    ?line Data = ?config(data_dir, Config),
    ?line File = filename:join(Data, "nif_mod"),
    ?line {ok,nif_mod,Bin} = compile:file(File, [binary,return_errors]),
    ?line {module,nif_mod} = erlang:load_module(nif_mod,Bin),

    ?line ok = nif_mod:load_nif_lib(Config, 1),

    ?line hold_nif_mod_priv_data(nif_mod:get_priv_data_ptr()),
    ?line [{load,1,1,101},{get_priv_data_ptr,1,2,102}] = nif_mod_call_history(),    
        
    ?line ok = nif_mod:load_nif_lib(Config, 2),
    ?line 2 = nif_mod:lib_version(),
    ?line [{reload,2,1,201},{lib_version,2,2,202}] = nif_mod_call_history(),    

    ?line ok = nif_mod:load_nif_lib(Config, 1),
    ?line 1 = nif_mod:lib_version(),
    ?line [{reload,1,1,101},{lib_version,1,2,102}] = nif_mod_call_history(),    

    ?line true = erlang:delete_module(nif_mod),
    ?line [] = nif_mod_call_history(),    

    %%?line false= check_process_code(Pid, nif_mod),
    ?line true = erlang:purge_module(nif_mod),
    ?line [{unload,1,3,103}] = nif_mod_call_history(),    

    ?line true = lists:member(?MODULE, erlang:system_info(taints)),
    ?line true = lists:member(nif_mod, erlang:system_info(taints)),
    ?line verify_tmpmem(TmpMem),
    ok.

upgrade(doc) -> ["Test upgrade callback in nif lib"];
upgrade(suite) -> [];  
upgrade(Config) when is_list(Config) ->    
    TmpMem = tmpmem(),
    ensure_lib_loaded(Config),

    ?line Data = ?config(data_dir, Config),
    ?line File = filename:join(Data, "nif_mod"),
    ?line {ok,nif_mod,Bin} = compile:file(File, [binary,return_errors]),
    ?line {module,nif_mod} = erlang:load_module(nif_mod,Bin),

    ?line ok = nif_mod:load_nif_lib(Config, 1),
    ?line {Pid,MRef} = nif_mod:start(),
    ?line 1 = call(Pid,lib_version),

    ?line hold_nif_mod_priv_data(nif_mod:get_priv_data_ptr()),
    ?line [{load,1,1,101},{lib_version,1,2,102},{get_priv_data_ptr,1,3,103}] = nif_mod_call_history(),    
        
    %% Module upgrade with same lib-version
    ?line {module,nif_mod} = erlang:load_module(nif_mod,Bin),
    ?line undefined = nif_mod:lib_version(),
    ?line 1 = call(Pid,lib_version),
    ?line [{lib_version,1,4,104}] = nif_mod_call_history(),

    ?line ok = nif_mod:load_nif_lib(Config, 1),
    ?line 1 = nif_mod:lib_version(),
    ?line [{upgrade,1,5,105},{lib_version,1,6,106}] = nif_mod_call_history(),

    ?line upgraded = call(Pid,upgrade),
    ?line false = check_process_code(Pid, nif_mod),
    ?line true = erlang:purge_module(nif_mod),
    ?line [{unload,1,7,107}] = nif_mod_call_history(),

    ?line 1 = nif_mod:lib_version(),
    ?line [{lib_version,1,8,108}] = nif_mod_call_history(),

    ?line true = erlang:delete_module(nif_mod),
    ?line [] = nif_mod_call_history(),    

    ?line Pid ! die,
    ?line {'DOWN', MRef, process, Pid, normal} = receive_any(),
    ?line false = check_process_code(Pid, nif_mod),
    ?line true = erlang:purge_module(nif_mod),
    ?line [{unload,1,9,109}] = nif_mod_call_history(),    

    %% Module upgrade with different lib version
    ?line {module,nif_mod} = erlang:load_module(nif_mod,Bin),
    ?line undefined = nif_mod:lib_version(),
    ?line {Pid2,MRef2} = nif_mod:start(),
    ?line undefined = call(Pid2,lib_version),

    ?line ok = nif_mod:load_nif_lib(Config, 1),
    ?line hold_nif_mod_priv_data(nif_mod:get_priv_data_ptr()),
    ?line 1 = call(Pid2,lib_version),
    ?line [{load,1,1,101},{get_priv_data_ptr,1,2,102},{lib_version,1,3,103}] = nif_mod_call_history(),

    ?line {module,nif_mod} = erlang:load_module(nif_mod,Bin),
    ?line undefined = nif_mod:lib_version(),
    ?line [] = nif_mod_call_history(),
    ?line 1 = call(Pid2,lib_version),
    ?line [{lib_version,1,4,104}] = nif_mod_call_history(),

    ?line ok = nif_mod:load_nif_lib(Config, 2),
    ?line 2 = nif_mod:lib_version(),
    ?line [{upgrade,2,1,201},{lib_version,2,2,202}] = nif_mod_call_history(),

    ?line 1 = call(Pid2,lib_version),
    ?line [{lib_version,1,5,105}] = nif_mod_call_history(),

    ?line upgraded = call(Pid2,upgrade),
    ?line false = check_process_code(Pid2, nif_mod),
    ?line true = erlang:purge_module(nif_mod),
    ?line [{unload,1,6,106}] = nif_mod_call_history(),

    ?line 2 = nif_mod:lib_version(),
    ?line [{lib_version,2,3,203}] = nif_mod_call_history(),

    ?line true = erlang:delete_module(nif_mod),
    ?line [] = nif_mod_call_history(),    

    ?line Pid2 ! die,
    ?line {'DOWN', MRef2, process, Pid2, normal} = receive_any(),
    ?line false= check_process_code(Pid2, nif_mod),
    ?line true = erlang:purge_module(nif_mod),
    ?line [{unload,2,4,204}] = nif_mod_call_history(),    

    ?line true = lists:member(?MODULE, erlang:system_info(taints)),
    ?line true = lists:member(nif_mod, erlang:system_info(taints)),
    ?line verify_tmpmem(TmpMem),
    ok.

heap_frag(doc) -> ["Test NIF building heap fragments"];
heap_frag(suite) -> [];  
heap_frag(Config) when is_list(Config) ->    
    TmpMem = tmpmem(),
    ensure_lib_loaded(Config),
    
    heap_frag_do(1,1000000),
    ?line verify_tmpmem(TmpMem),
    ok.

heap_frag_do(N, Max) when N > Max ->
    ok;
heap_frag_do(N, Max) ->
    io:format("Create list of length ~p\n",[N]),
    L = lists:seq(1,N),
    L = list_seq(N),
    heap_frag_do(((N*5) div 4) + 1, Max).

types(doc) -> ["Type tests"];
types(suite) -> [];
types(Config) when is_list(Config) ->
    TmpMem = tmpmem(),
    ensure_lib_loaded(Config),
    ?line ok = type_test(),
    lists:foreach(fun(Tpl) ->
                    Lst = erlang:tuple_to_list(Tpl),                 
                    Lst = tuple_2_list(Tpl)
                  end,
                  [{},{ok},{{}},{[],{}},{1,2,3,4,5}]),
    Stuff = [[],{},0,0.0,(1 bsl 100),(fun()-> ok end),make_ref(),self()],
    [eq_cmp(A,clone(B)) || A<-Stuff, B<-Stuff],

    {IntSz, LongSz} = type_sizes(),
    UintMax = (1 bsl (IntSz*8)) - 1,
    IntMax = UintMax bsr 1,
    IntMin = -(IntMax+1),
    UlongMax = (1 bsl (LongSz*8)) - 1,
    LongMax = UlongMax bsr 1,
    LongMin = -(LongMax+1),
    Uint64Max = (1 bsl 64) - 1,
    Int64Max = Uint64Max bsr 1,
    Int64Min = -(Int64Max+1),
    Limits = [{IntMin,IntMax},{0,UintMax},{LongMin,LongMax},{0,UlongMax},{Int64Min,Int64Max},{0,Uint64Max}],
    io:format("Limits = ~p\n", [Limits]),
    lists:foreach(fun(I) ->
			  R1 = echo_int(I),
			  %%io:format("echo_int(~p) -> ~p\n", [I, R1]),
			  R2 = my_echo_int(I, Limits),
			  ?line R1 = R2,
			  ?line true = (R1 =:= R2),
			  ?line true = (R1 == R2)
		  end, int_list()),

    ?line verify_tmpmem(TmpMem),
    ?line true = (compare(-1294536544000, -1178704800000) < 0),
    ?line true = (compare(-1178704800000, -1294536544000) > 0),
    ?line true = (compare(-295147905179352825856, -36893488147419103232) < 0),
    ?line true = (compare(-36893488147419103232, -295147905179352825856) > 0),
    ?line true = (compare(-29514790517935282585612345678, -36893488147419103232) < 0),
    ?line true = (compare(-36893488147419103232, -29514790517935282585612345678) > 0),
    ok.

int_list() ->
    Start = 1 bsl 200,
    int_list([Start], -Start).
int_list([N | _]=List, End) when N<End ->
    List;
int_list([N | _]=List, End) ->
    int_list([N - (1 + (abs(N) div 3)) | List], End).
    
my_echo_int(I, Limits) ->
    lists:map(fun({Min,Max}) ->
		      if I < Min -> false;
			 I > Max -> false;
			 true -> I
		      end
	      end, Limits).

clone(X) ->
    binary_to_term(term_to_binary(X)).

eq_cmp(A,B) ->
    eq_cmp_do(A,B),
    eq_cmp_do([A,B],[A,B]),
    eq_cmp_do({A,B},{A,B}).

eq_cmp_do(A,B) ->
    %%?t:format("compare ~p and ~p\n",[A,B]),
    Eq = (A =:= B),
    ?line Eq = is_identical(A,B),
    ?line Cmp = if
            A < B -> -1;
            A == B -> 0;
            A > B -> 1
        end,
    ?line Cmp = case compare(A,B) of
                    C when is_integer(C), C < 0 -> -1;
                    0 -> 0;
                    C when is_integer(C) -> 1
                end,       
    ok. 


many_args(doc) -> ["Test NIF with many arguments"];
many_args(suite) -> [];
many_args(Config) when is_list(Config) ->
    TmpMem = tmpmem(),
    ?line ensure_lib_loaded(Config ,1),
    ?line ok = apply(?MODULE,many_args_100,lists:seq(1,100)),
    ?line ok = many_args_100(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100),
    ?line verify_tmpmem(TmpMem),
    ok.

binaries(doc) -> ["Test NIF binary handling."];
binaries(suite) -> [];
binaries(Config) when is_list(Config) ->
    TmpMem = tmpmem(),
    ?line ensure_lib_loaded(Config, 1),
    ?line RefcBin = list_to_binary(lists:seq(1,255)),
    ?line RefcBin = clone_bin(RefcBin),
    ?line HeapBin = list_to_binary(lists:seq(1,20)),
    ?line HeapBin = clone_bin(HeapBin),
    ?line <<_:8,Sub1:6/binary,_/binary>> = RefcBin, 
    ?line <<_:8,Sub2:6/binary,_/binary>> = HeapBin,
    ?line Sub1 = Sub2,
    ?line Sub1 = clone_bin(Sub1),
    ?line Sub2 = clone_bin(Sub2),
    ?line <<_:9,Sub3:6/binary,_/bitstring>> = RefcBin, 
    ?line <<_:9,Sub4:6/binary,_/bitstring>> = HeapBin,
    ?line Sub3 = Sub4,
    ?line Sub3 = clone_bin(Sub3),
    ?line Sub4 = clone_bin(Sub4),
    %% When NIFs get bitstring support
    %%?line <<_:8,Sub5:27/bitstring,_/bitstring>> = RefcBin, 
    %%?line <<_:8,Sub6:27/bitstring,_/bitstring>> = HeapBin,
    %%?line Sub5 = Sub6,
    %%?line Sub5 = clone_bin(Sub5),
    %%?line Sub6 = clone_bin(Sub6),
    %%?line <<_:9,Sub7:27/bitstring,_/bitstring>> = RefcBin, 
    %%?line <<_:9,Sub8:27/bitstring,_/bitstring>> = HeapBin,
    %%?line Sub7 = Sub8,
    %%?line Sub7 = clone_bin(Sub7),
    %%?line Sub8 = clone_bin(Sub8),
    %%?line <<>> = clone_bin(<<>>),

    <<_:8,SubBinA:200/binary,_/binary>> = RefcBin,
    <<_:9,SubBinB:200/binary,_/bitstring>> = RefcBin,
    <<_:8,SubBinC:17/binary,_/binary>> = HeapBin,
    <<_:9,SubBinD:17/binary,_/bitstring>> = HeapBin,
    test_make_sub_bin(RefcBin),
    test_make_sub_bin(HeapBin),
    test_make_sub_bin(SubBinA),
    test_make_sub_bin(SubBinB),
    test_make_sub_bin(SubBinC),
    test_make_sub_bin(SubBinD),
    
    ?line verify_tmpmem(TmpMem),
    ok.

test_make_sub_bin(Bin) ->
    Size = byte_size(Bin),
    Rest10 = Size - 10,
    Rest1 = Size - 1,
    ?line Bin = make_sub_bin(Bin, 0, Size),
    <<_:10/binary,Sub0:Rest10/binary>> = Bin,
    ?line Sub0 = make_sub_bin(Bin, 10, Rest10),
    <<Sub1:10/binary,_/binary>> = Bin,
    ?line Sub1 = make_sub_bin(Bin, 0, 10),
    <<_:7/binary,Sub2:10/binary,_/binary>> = Bin,
    ?line Sub2 = make_sub_bin(Bin, 7, 10),
    ?line <<>> = make_sub_bin(Bin, 0, 0),
    ?line <<>> = make_sub_bin(Bin, 10, 0),
    ?line <<>> = make_sub_bin(Bin, Rest1, 0),
    ?line <<>> = make_sub_bin(Bin, Size, 0),
    ok.
    
get_string(doc) -> ["Test enif_get_string"];
get_string(suite) -> [];
get_string(Config) when is_list(Config) ->
    ?line ensure_lib_loaded(Config, 1),
    ?line {7, <<"hejsan",0,_:3/binary>>} = string_to_bin("hejsan",10),
    ?line {7, <<"hejsan",0,_>>} = string_to_bin("hejsan",8),
    ?line {7, <<"hejsan",0>>} = string_to_bin("hejsan",7),
    ?line {-6, <<"hejsa",0>>} = string_to_bin("hejsan",6),
    ?line {-5, <<"hejs",0>>} = string_to_bin("hejsan",5),
    ?line {-1, <<0>>} = string_to_bin("hejsan",1),
    ?line {0, <<>>} = string_to_bin("hejsan",0),
    ?line {1, <<0>>} = string_to_bin("",1),
    ?line {0, <<>>} = string_to_bin("",0),
    ok.

get_atom(doc) -> ["Test enif_get_atom"];
get_atom(suite) -> [];
get_atom(Config) when is_list(Config) ->
    ?line ensure_lib_loaded(Config, 1),
    ?line {7, <<"hejsan",0,_:3/binary>>} = atom_to_bin(hejsan,10),
    ?line {7, <<"hejsan",0,_>>} = atom_to_bin(hejsan,8),
    ?line {7, <<"hejsan",0>>} = atom_to_bin(hejsan,7),
    ?line {0, <<_:6/binary>>} = atom_to_bin(hejsan,6),
    ?line {0, <<>>} = atom_to_bin(hejsan,0),
    ?line {1, <<0>>} = atom_to_bin('',1),
    ?line {0, <<>>} = atom_to_bin('',0),
    ok.

maps(doc) -> ["Test NIF maps handling."];
maps(suite) -> [];
maps(Config) when is_list(Config) ->
    TmpMem = tmpmem(),
    Pairs = [{adam, "bert"}] ++
            [{I,I}||I <- lists:seq(1,10)] ++
	    [{a,value},{"a","value"},{<<"a">>,<<"value">>}],
    ok = ensure_lib_loaded(Config, 1),
    M  = maps_from_list_nif(Pairs),
    R = {RIs,Is} = sorted_list_from_maps_nif(M),
    io:format("Pairs: ~p~nMap: ~p~nReturned: ~p~n", [lists:sort(Pairs),M,R]),
    true = (lists:sort(Is) =:= lists:sort(Pairs)),
    Is = lists:reverse(RIs),

    #{} = maps_from_list_nif([]),
    {[],[]} = sorted_list_from_maps_nif(#{}),

    1 = is_map_nif(M),
    0 = is_map_nif("no map"),

    Msz = map_size(M),
    {1,Msz} = get_map_size_nif(M),
    {1,0} = get_map_size_nif(#{}),
    {0,-123} = get_map_size_nif({#{}}),

    #{} = M0 = make_new_map_nif(),

    {1, #{key := value}=M1} = make_map_put_nif(M0, key, value),
    {1, #{key := value, "key2" := "value2"}=M2} = make_map_put_nif(M1, "key2", "value2"),
    {1, #{key := "value", "key2" := "value2"}=M3} = make_map_put_nif(M2, key, "value"),
    {0, undefined} = make_map_put_nif(666, key, value),

    {1, "value2"} = get_map_value_nif(M3,"key2"),
    {0, undefined} = get_map_value_nif(M3,"key3"),
    {0, undefined} = get_map_value_nif(false,key),

    {0, undefined} = make_map_update_nif(M0, key, value),
    {0, undefined} = make_map_update_nif(M1, "key2", "value2"),
    {1, #{key := "value", "key2" := "value2"}} = make_map_update_nif(M2, key, "value"),
    {0, undefined} = make_map_update_nif(666, key, value),

    {1, #{}} = make_map_remove_nif(M1, key),
    {1, M1} = make_map_remove_nif(M2, "key2"),
    {1, M2} = make_map_remove_nif(M2, "key3"),
    {0, undefined} = make_map_remove_nif(self(), key),

    ok.
 
api_macros(doc) -> ["Test macros enif_make_list<N> and enif_make_tuple<N>"];
api_macros(suite) -> [];
api_macros(Config) when is_list(Config) ->
    ?line ensure_lib_loaded(Config, 1),
    Expected = {[lists:seq(1,N) || N <- lists:seq(1,9)],
		[list_to_tuple(lists:seq(1,N)) || N <- lists:seq(1,9)]
	       },
    ?line Expected = macros(list_to_tuple(lists:seq(1,9))),
    ok.

from_array(doc) -> ["enif_make_[tuple|list]_from_array"];
from_array(suite) -> [];
from_array(Config) when is_list(Config) ->
    ?line ensure_lib_loaded(Config, 1),
    lists:foreach(fun(Tpl) ->
			  Lst = tuple_to_list(Tpl),
			  ?line {Lst,Tpl} = tuple_2_list_and_tuple(Tpl)
		  end,
		  [{}, {1,2,3}, {[4,5],[],{},{6,7}}, {{}}, {[]}]),
    ok.

iolist_as_binary(doc) -> ["enif_inspect_iolist_as_binary"];
iolist_as_binary(suite) -> [];
iolist_as_binary(Config) when is_list(Config) ->
    ?line ensure_lib_loaded(Config, 1),
    TmpMem = tmpmem(),
    List = [<<"hejsan">>, <<>>, [], [17], [<<>>],
	    [127,128,255,0],
	    [1, 2, 3, <<"abc">>, [<<"def">>,4], 5, <<"ghi">>],
	    [1, 2, 3, <<"abc">>, [<<"def">>,4], 5 | <<"ghi">>]],
	    
    lists:foreach(fun(IoL) ->
			  B1 = erlang:iolist_to_binary(IoL),
			  ?line B2 = iolist_2_bin(IoL),
			  ?line B1 = B2
		  end,
		  List),
    ?line verify_tmpmem(TmpMem),
    ok.

resource(doc) -> ["Test memory managed objects, aka 'resources'"];
resource(suite) -> [];
resource(Config) when is_list(Config) ->
    ?line ensure_lib_loaded(Config, 1),
    ?line Type = get_resource_type(0),
    resource_hugo(Type),
    resource_otto(Type),
    resource_new(Type),
    resource_neg(Type),
    ok.

resource_hugo(Type) ->
    DtorCall = resource_hugo_do(Type),
    erlang:garbage_collect(),
    ?line DtorCall = last_resource_dtor_call(),
    ok.

resource_hugo_do(Type) ->
    HugoBin = <<"Hugo Hacker">>,
    ?line HugoPtr = alloc_resource(Type, HugoBin),
    ?line Hugo = make_resource(HugoPtr),
    ?line <<>> = Hugo,
    release_resource(HugoPtr),
    erlang:garbage_collect(),
    ?line {HugoPtr,HugoBin} = get_resource(Type,Hugo),
    Pid = spawn_link(fun() -> 			     
			     receive {Pid, Type, Resource, Ptr, Bin} ->
				     Pid ! {self(), got_it},
				     receive {Pid, check_it} ->
					     ?line {Ptr,Bin} = get_resource(Type,Resource),
					     Pid ! {self(), ok}
				     end
			     end
		     end),
    Pid ! {self(), Type, Hugo, HugoPtr, HugoBin},
    ?line {Pid, got_it} = receive_any(),
    erlang:garbage_collect(),   % just to make our ProcBin move in memory
    Pid ! {self(), check_it},
    ?line {Pid, ok} = receive_any(),
    ?line [] = last_resource_dtor_call(),
    ?line {HugoPtr,HugoBin} = get_resource(Type,Hugo),
    {HugoPtr, HugoBin, 1}.

resource_otto(Type) ->
    {OttoPtr, OttoBin} = resource_otto_do(Type),
    erlang:garbage_collect(),
    ?line [] = last_resource_dtor_call(),
    release_resource(OttoPtr),
    ?line {OttoPtr,OttoBin,1} = last_resource_dtor_call(),
    ok.
    
resource_otto_do(Type) ->
    OttoBin = <<"Otto Ordonnans">>,
    ?line OttoPtr = alloc_resource(Type, OttoBin),
    ?line Otto = make_resource(OttoPtr),
    ?line <<>> = Otto,
    %% forget resource term but keep referenced by NIF
    {OttoPtr, OttoBin}.    

resource_new(Type) ->
    ?line {PtrB,BinB} = resource_new_do1(Type),
    erlang:garbage_collect(),
    ?line {PtrB,BinB,1} = last_resource_dtor_call(),
    ok.
    
resource_new_do1(Type) ->
    ?line {{PtrA,BinA}, {ResB,PtrB,BinB}} = resource_new_do2(Type),
    erlang:garbage_collect(),
    ?line {PtrA,BinA,1} = last_resource_dtor_call(),
    ?line {PtrB,BinB} = get_resource(Type, ResB),
    %% forget ResB and make it garbage
    {PtrB,BinB}.
    
resource_new_do2(Type) ->
    BinA = <<"NewA">>,
    BinB = <<"NewB">>,
    ?line ResA = make_new_resource(Type, BinA),
    ?line ResB = make_new_resource(Type, BinB),
    ?line <<>> = ResA,
    ?line <<>> = ResB,
    ?line {PtrA,BinA} = get_resource(Type, ResA),
    ?line {PtrB,BinB} = get_resource(Type, ResB),
    ?line true = (PtrA =/= PtrB),
    %% forget ResA and make it garbage
    {{PtrA,BinA}, {ResB,PtrB,BinB}}.

resource_neg(TypeA) ->
    resource_neg_do(TypeA),

    catch exit(42), % dummy exception to purge saved stacktraces from earlier exception
    erlang:garbage_collect(),
    ?line {_,_,2} = last_resource_dtor_call(),
    ok.

resource_neg_do(TypeA) ->
    TypeB = get_resource_type(1),
    ResA = make_new_resource(TypeA, <<"Arnold">>),
    ResB= make_new_resource(TypeB, <<"Bobo">>),
    ?line {'EXIT',{badarg,_}} = (catch get_resource(TypeA, ResB)),
    ?line {'EXIT',{badarg,_}} = (catch get_resource(TypeB, ResA)),
    ok.

resource_binary(doc) -> ["Test enif_make_resource_binary"];
resource_binary(suite) -> [];
resource_binary(Config) when is_list(Config) ->
    ?line ensure_lib_loaded(Config, 1),
    ?line {Ptr,Bin} = resource_binary_do(),
    erlang:garbage_collect(),
    Last = last_resource_dtor_call(),
    ?CHECK({Ptr,Bin,1}, Last),
    ok.

resource_binary_do() ->
    Bin = <<"Hej Hopp i lingonskogen">>,
    ?line {Ptr,ResBin1} = make_new_resource_binary(Bin),
    ?line ResBin1 = Bin,          
    ?line ResInfo = {Ptr,_} = get_resource(binary_resource_type,ResBin1),

    Papa = self(),
    Forwarder = spawn_link(fun() -> forwarder(Papa) end),
    io:format("sending to forwarder pid=~p\n",[Forwarder]),  
    Forwarder ! ResBin1,
    ResBin2 = receive_any(),
    ?line ResBin2 = ResBin1,
    ?line ResInfo = get_resource(binary_resource_type,ResBin2),
    Forwarder ! terminate,
    ?line {Forwarder, 1} = receive_any(),
    erlang:garbage_collect(),
    ?line ResInfo = get_resource(binary_resource_type,ResBin1),
    ?line ResInfo = get_resource(binary_resource_type,ResBin2),
    ResInfo.

    
-define(RT_CREATE,1).
-define(RT_TAKEOVER,2).

resource_takeover(doc) -> ["Test resource takeover by module reload and upgrade"];
resource_takeover(suite) -> [];  
resource_takeover(Config) when is_list(Config) ->    
    TmpMem = tmpmem(),
    ensure_lib_loaded(Config),

    ?line Data = ?config(data_dir, Config),
    ?line File = filename:join(Data, "nif_mod"),
    ?line {ok,nif_mod,ModBin} = compile:file(File, [binary,return_errors]),
    ?line {module,nif_mod} = erlang:load_module(nif_mod,ModBin),

    ?line ok = nif_mod:load_nif_lib(Config, 1, 
				    [{resource_type, 0, ?RT_CREATE, "resource_type_A",resource_dtor_A,
				      ?RT_CREATE},
				     {resource_type, 1, ?RT_CREATE, "resource_type_null_A",null,
				      ?RT_CREATE},
				     {resource_type, 2, ?RT_CREATE bor ?RT_TAKEOVER, "resource_type_A_null",resource_dtor_A,
				      ?RT_CREATE},
				     {resource_type, 3, ?RT_CREATE, "resource_type_B_goneX",resource_dtor_B,
				      ?RT_CREATE},
				     {resource_type, 4, ?RT_CREATE, "resource_type_null_goneX",null,
				      ?RT_CREATE},
				     {resource_type, null, ?RT_TAKEOVER, "Pink unicorn", resource_dtor_A,
				      ?RT_TAKEOVER}
				    ]),

    ?line hold_nif_mod_priv_data(nif_mod:get_priv_data_ptr()),
    ?line [{load,1,1,101},{get_priv_data_ptr,1,2,102}] = nif_mod_call_history(),

    ?line {Holder, _MRef} = spawn_opt(fun resource_holder/0, [link, monitor]),

    {A1,BinA1} = make_resource(0,Holder,"A1"),
    {A2,BinA2} = make_resource(0,Holder,"A2"),
    {A3,BinA3} = make_resource(0,Holder,"A3"),

    {NA1,_BinNA1} = make_resource(1,Holder,"NA1"),
    {NA2,BinNA2} = make_resource(1,Holder,"NA2"),
    {NA3,_BinNA3} = make_resource(1,Holder,"NA3"),

    {AN1,BinAN1} = make_resource(2,Holder,"AN1"),
    {AN2,_BinAN2} = make_resource(2,Holder,"AN2"),
    {AN3,BinAN3} = make_resource(2,Holder,"AN3"),

    {BGX1,BinBGX1} = make_resource(3,Holder,"BGX1"),
    {BGX2,BinBGX2} = make_resource(3,Holder,"BGX2"),

    {NGX1,_BinNGX1} = make_resource(4,Holder,"NGX1"),
    {NGX2,_BinNGX2} = make_resource(4,Holder,"NGX2"),

    ?line [] = nif_mod_call_history(),

    ?line ok = forget_resource(A1),
    ?line [{{resource_dtor_A_v1,BinA1},1,3,103}] = nif_mod_call_history(),    

    ?line ok = forget_resource(NA1),
    ?line [] = nif_mod_call_history(), % no dtor

    ?line ok = forget_resource(AN1),
    ?CHECK([{{resource_dtor_A_v1,BinAN1},1,4,104}] , nif_mod_call_history()),

    ?line ok = forget_resource(BGX1),
    ?CHECK([{{resource_dtor_B_v1,BinBGX1},1,5,105}], nif_mod_call_history()),

    ?line ok = forget_resource(NGX1),
    ?CHECK([], nif_mod_call_history()), % no dtor

    ?line ok = nif_mod:load_nif_lib(Config, 2,
				    [{resource_type, 0, ?RT_TAKEOVER, "resource_type_A",resource_dtor_A,
				      ?RT_TAKEOVER},
				     {resource_type, 1, ?RT_TAKEOVER bor ?RT_CREATE, "resource_type_null_A",resource_dtor_A,
				      ?RT_TAKEOVER},
				     {resource_type, 2, ?RT_TAKEOVER, "resource_type_A_null",null,
				      ?RT_TAKEOVER},
				     {resource_type, null, ?RT_TAKEOVER, "Pink unicorn", resource_dtor_A,
				      ?RT_TAKEOVER},
				     {resource_type, null, ?RT_CREATE, "resource_type_B_goneX",resource_dtor_B,
				      ?RT_CREATE},
				     {resource_type, null, ?RT_CREATE, "resource_type_null_goneX",null,
				      ?RT_CREATE},
				     {resource_type, 3, ?RT_CREATE, "resource_type_B_goneY",resource_dtor_B,
				      ?RT_CREATE},
				     {resource_type, 4, ?RT_CREATE, "resource_type_null_goneY",null,
				      ?RT_CREATE}
				    ]),
    ?CHECK([{reload,2,1,201}], nif_mod_call_history()),

    ?line BinA2 = read_resource(0,A2),
    ?line ok = forget_resource(A2),
    ?CHECK([{{resource_dtor_A_v2,BinA2},2,2,202}], nif_mod_call_history()),    

    ?line ok = forget_resource(NA2),
    ?CHECK([{{resource_dtor_A_v2,BinNA2},2,3,203}], nif_mod_call_history()),    

    ?line ok = forget_resource(AN2),
    ?CHECK([], nif_mod_call_history()),    % no dtor

    ?line ok = forget_resource(BGX2),  % calling dtor in orphan library v1 still loaded
    ?CHECK([{{resource_dtor_B_v1,BinBGX2},1,6,106}], nif_mod_call_history()),
    % How to test that lib v1 is closed here?

    ?line ok = forget_resource(NGX2),
    ?CHECK([], nif_mod_call_history()),  % no dtor

    {BGY1,BinBGY1} = make_resource(3,Holder,"BGY1"),
    {NGY1,_BinNGY1} = make_resource(4,Holder,"NGY1"),

    %% Module upgrade with same lib-version
    ?line {module,nif_mod} = erlang:load_module(nif_mod,ModBin),
    ?line undefined = nif_mod:lib_version(),
    ?line ok = nif_mod:load_nif_lib(Config, 2,
				    [{resource_type, 2, ?RT_TAKEOVER, "resource_type_A",resource_dtor_B,
				      ?RT_TAKEOVER},
				     {resource_type, 0, ?RT_TAKEOVER bor ?RT_CREATE, "resource_type_null_A",null,
				      ?RT_TAKEOVER},
				     {resource_type, 1, ?RT_TAKEOVER, "resource_type_A_null",resource_dtor_A,
				      ?RT_TAKEOVER},
				     {resource_type, null, ?RT_TAKEOVER, "Pink elephant", resource_dtor_A,
				      ?RT_TAKEOVER},
				     {resource_type, 3, ?RT_CREATE, "resource_type_B_goneZ",resource_dtor_B,
				      ?RT_CREATE},
				     {resource_type, 4, ?RT_CREATE, "resource_type_null_goneZ",null,
				      ?RT_CREATE}
				    ]),

    ?line 2 = nif_mod:lib_version(),
    ?CHECK([{upgrade,2,4,204},{lib_version,2,5,205}], nif_mod_call_history()),

    ?line ok = forget_resource(A3),
    ?CHECK([{{resource_dtor_B_v2,BinA3},2,6,206}], nif_mod_call_history()),    

    ?line ok = forget_resource(NA3),
    ?CHECK([], nif_mod_call_history()),    

    ?line ok = forget_resource(AN3),
    ?CHECK([{{resource_dtor_A_v2,BinAN3},2,7,207}], nif_mod_call_history()),

    {A4,BinA4} = make_resource(2,Holder, "A4"),
    {NA4,BinNA4} = make_resource(0,Holder, "NA4"),
    {AN4,_BinAN4} = make_resource(1,Holder, "AN4"),

    {BGZ1,BinBGZ1} = make_resource(3,Holder,"BGZ1"),
    {NGZ1,_BinNGZ1} = make_resource(4,Holder,"NGZ1"),

    ?line false = code:purge(nif_mod),
    ?line [] = nif_mod_call_history(),

    ?line ok = forget_resource(NGY1),
    ?line [] = nif_mod_call_history(),

    ?line ok = forget_resource(BGY1),  % calling dtor in orphan library v2 still loaded
    ?line [{{resource_dtor_B_v2,BinBGY1},2,8,208},{unload,2,9,209}] = nif_mod_call_history(),

    %% Module upgrade with other lib-version
    ?line {module,nif_mod} = erlang:load_module(nif_mod,ModBin),
    ?line undefined = nif_mod:lib_version(),
    ?line ok = nif_mod:load_nif_lib(Config, 1,
				    [{resource_type, 2, ?RT_TAKEOVER, "resource_type_A",resource_dtor_A,
				      ?RT_TAKEOVER},
				     {resource_type, 0, ?RT_TAKEOVER bor ?RT_CREATE, "resource_type_null_A",resource_dtor_A,
				      ?RT_TAKEOVER},
				     {resource_type, 1, ?RT_TAKEOVER, "resource_type_A_null",null,
				      ?RT_TAKEOVER},
				     {resource_type, null, ?RT_TAKEOVER, "Mr Pink", resource_dtor_A,
				      ?RT_TAKEOVER}
				    ]),

    ?line 1 = nif_mod:lib_version(),
    ?line [{upgrade,1,1,101},{lib_version,1,2,102}] = nif_mod_call_history(),

    %%?line false= check_process_code(Pid, nif_mod),
    ?line false = code:purge(nif_mod),
    %% no unload here as we still have instances with destructors
    ?line [] = nif_mod_call_history(),

    ?line ok = forget_resource(BGZ1),  % calling dtor in orphan library v2 still loaded
    ?line [{{resource_dtor_B_v2,BinBGZ1},2,10,210},{unload,2,11,211}] = nif_mod_call_history(),

    ?line ok = forget_resource(NGZ1),
    ?line [] = nif_mod_call_history(),

    ?line ok = forget_resource(A4),
    ?line [{{resource_dtor_A_v1,BinA4},1,3,103}] = nif_mod_call_history(),

    ?line ok = forget_resource(NA4),
    ?line [{{resource_dtor_A_v1,BinNA4},1,4,104}] = nif_mod_call_history(),

    ?line ok = forget_resource(AN4),
    ?line [] = nif_mod_call_history(),


    %%
    %% Test rollback after failed upgrade of same lib-version
    %%

    {A5,BinA5} = make_resource(2, Holder, "A5"),
    {NA5,BinNA5} = make_resource(0, Holder, "NA5"),
    {AN5,_BinAN5} = make_resource(1, Holder, "AN5"),

    {A6,BinA6} = make_resource(2, Holder, "A6"),
    {NA6,BinNA6} = make_resource(0, Holder, "NA6"),
    {AN6,_BinAN6} = make_resource(1, Holder, "AN6"),

    {module,nif_mod} = erlang:load_module(nif_mod,ModBin),
    undefined = nif_mod:lib_version(),
    {error,{upgrade,_}} =
	nif_mod:load_nif_lib(Config, 1,
			     [{resource_type, 4, ?RT_TAKEOVER, "resource_type_A",resource_dtor_B,
			       ?RT_TAKEOVER},
			      {resource_type, 4, ?RT_TAKEOVER bor ?RT_CREATE, "resource_type_null_A",null,
			       ?RT_TAKEOVER},
			      {resource_type, 4, ?RT_TAKEOVER, "resource_type_A_null",resource_dtor_A,
			       ?RT_TAKEOVER},
			      {resource_type, 4, ?RT_CREATE, "Mr Pink", resource_dtor_A,
			       ?RT_CREATE},

			      {return, 1}  % FAIL
			     ]),

    undefined = nif_mod:lib_version(),
    [{upgrade,1,5,105}] = nif_mod_call_history(),

    %% Make sure dtor was not changed (from A to B)
    ok = forget_resource(A5),
    [{{resource_dtor_A_v1,BinA5},1,6,106}] = nif_mod_call_history(),

    %% Make sure dtor was not nullified (from A to null)
    ok = forget_resource(NA5),
    [{{resource_dtor_A_v1,BinNA5},1,7,107}] = nif_mod_call_history(),

    %% Make sure dtor was not added (from null to A)
    ok = forget_resource(AN5),
    [] = nif_mod_call_history(),

    %%
    %% Test rollback after failed upgrade of other lib-version
    %%

    {error,{upgrade,_}} =
	nif_mod:load_nif_lib(Config, 2,
			     [{resource_type, 4, ?RT_TAKEOVER, "resource_type_A",resource_dtor_B,
			       ?RT_TAKEOVER},
			      {resource_type, 4, ?RT_TAKEOVER bor ?RT_CREATE, "resource_type_null_A",null,
			       ?RT_TAKEOVER},
			      {resource_type, 4, ?RT_TAKEOVER, "resource_type_A_null",resource_dtor_A,
			       ?RT_TAKEOVER},
			      {resource_type, null, ?RT_TAKEOVER, "Mr Pink", resource_dtor_A,
			       ?RT_TAKEOVER},
			      {resource_type, 4, ?RT_CREATE, "Mr Pink", resource_dtor_A,
			       ?RT_CREATE},

			      {return, 1}  % FAIL
			     ]),

    undefined = nif_mod:lib_version(),
    [{upgrade,2,_,_}] = nif_mod_call_history(),

    %% Make sure dtor was not changed (from A to B)
    ok = forget_resource(A6),
    [{{resource_dtor_A_v1,BinA6},1,_,_}] = nif_mod_call_history(),

    %% Make sure dtor was not nullified (from A to null)
    ok = forget_resource(NA6),
    [{{resource_dtor_A_v1,BinNA6},1,_,_}] = nif_mod_call_history(),

    %% Make sure dtor was not added (from null to A)
    ok = forget_resource(AN6),
    [] = nif_mod_call_history(),

    %%
    %% Test rolback after failed initial load
    %%
    false = code:purge(nif_mod),
    [{unload,1,_,_}] = nif_mod_call_history(),
    true = code:delete(nif_mod),
    false = code:purge(nif_mod),
    [] = nif_mod_call_history(),


    {module,nif_mod} = erlang:load_module(nif_mod,ModBin),
    undefined = nif_mod:lib_version(),
    {error,{load,_}} =
	nif_mod:load_nif_lib(Config, 1,
			     [{resource_type, null, ?RT_TAKEOVER, "resource_type_A",resource_dtor_A,
			       ?RT_TAKEOVER},
			      {resource_type, 4, ?RT_TAKEOVER bor ?RT_CREATE, "resource_type_null_A",null,
			       ?RT_CREATE},
			      {resource_type, 4, ?RT_CREATE, "resource_type_A_null",resource_dtor_A,
			       ?RT_CREATE},
			      {resource_type, 4, ?RT_CREATE, "Mr Pink", resource_dtor_A,
			       ?RT_CREATE},

			      {return, 1}  % FAIL
			     ]),

    undefined = nif_mod:lib_version(),
    ok = nif_mod:load_nif_lib(Config, 1,
			      [{resource_type, null, ?RT_TAKEOVER, "resource_type_A",resource_dtor_A,
				?RT_TAKEOVER},
			       {resource_type, 0, ?RT_TAKEOVER bor ?RT_CREATE, "resource_type_null_A",
				resource_dtor_A, ?RT_CREATE},

			       {resource_type, 1, ?RT_CREATE, "resource_type_A_null", null,
				?RT_CREATE},
			       {resource_type, null, ?RT_TAKEOVER, "Mr Pink", resource_dtor_A,
				?RT_TAKEOVER},

			       {return, 0}  % SUCCESS
			      ]),

    ?line hold_nif_mod_priv_data(nif_mod:get_priv_data_ptr()),
    ?line [{load,1,1,101},{get_priv_data_ptr,1,2,102}] = nif_mod_call_history(),

    {NA7,BinNA7} = make_resource(0, Holder, "NA7"),
    {AN7,BinAN7} = make_resource(1, Holder, "AN7"),

    ok = forget_resource(NA7),
    [{{resource_dtor_A_v1,BinNA7},1,_,_}] = nif_mod_call_history(),

    ok = forget_resource(AN7),
    [] = nif_mod_call_history(),

    ?line true = lists:member(?MODULE, erlang:system_info(taints)),
    ?line true = lists:member(nif_mod, erlang:system_info(taints)),
    ?line verify_tmpmem(TmpMem),    
    ok.

make_resource(Type,Holder,Str) when is_list(Str) ->
    Bin = list_to_binary(Str),
    A1 = make_resource_do(Type,Holder,Bin),
    ?line Bin = read_resource(Type,A1),
    {A1,Bin}.

make_resource_do(Type, Holder, Bin) ->
    Holder ! {self(), make, Type, Bin},
    {Holder, make_ok, Id} = receive_any(),
    {Holder,Id}.

read_resource(Type, {Holder,Id}) ->
    Holder ! {self(), get, Type, Id},
    {Holder, get_ok, Bin} = receive_any(),
    Bin.

forget_resource({Holder,Id}) ->
    Holder ! {self(), forget, Id},
    {Holder, forget_ok, Id} = receive_any(),
    ok.


resource_holder() ->
    resource_holder([]).
resource_holder(List) ->
    %%io:format("resource_holder waiting for msg\n", []),
    Msg = receive_any(),
    %%io:format("resource_holder got ~p with list = ~p\n", [Msg,List]),
    case Msg of
	{Pid, make, Type, Bin} ->	    
	    ?line Resource = nif_mod:make_new_resource(Type, Bin),
	    Id = {make_ref(),Bin},
	    Pid ! {self(), make_ok, Id},
	    resource_holder([{Id,Resource} | List]);
	{Pid, get, Type, Id} ->
	    {Id,Resource} = lists:keyfind(Id, 1, List),
	    Pid ! {self(), get_ok, nif_mod:get_resource(Type, Resource)},
	    resource_holder(List);
	
	{Pid, forget, Id} ->
	    NewList = lists:keydelete(Id, 1, List),
	    %%io:format("resource_holder forget: NewList = ~p\n", [NewList]),
	    resource_holder(Pid, {self(),forget_ok,Id}, NewList)
    end.

resource_holder(Pid,Reply,List) ->
    erlang:garbage_collect(),
    %%io:format("resource_holder GC'ed, now send ~p to ~p\n", [Reply,Pid]),
    Pid ! Reply,
    resource_holder(List).


threading(doc) -> ["Test the threading API functions (reuse tests from driver API)"];
threading(Config) when is_list(Config) ->
    case erlang:system_info(threads) of
    	true -> threading_do(Config);
	false -> {skipped,"No thread support"}
    end.

threading_do(Config) ->
    ?line Data = ?config(data_dir, Config),
    ?line File = filename:join(Data, "tester"),
    ?line {ok,tester,ModBin} = compile:file(File, [binary,return_errors]),
    ?line {module,tester} = erlang:load_module(tester,ModBin),

    ?line ok = tester:load_nif_lib(Config, "basic"),   
    ?line ok = tester:run(),

    ?line ok = tester:load_nif_lib(Config, "rwlock"),
    ?line ok = tester:run(),

    ?line ok = tester:load_nif_lib(Config, "tsd"),
    ?line ok = tester:run().

send(doc) -> ["Test NIF message sending"];
send(Config) when is_list(Config) ->    
    ensure_lib_loaded(Config),

    N = 1500,
    List = lists:seq(1,N),
    ?line {ok,1} = send_list_seq(N, self),
    ?line {ok,1} = send_list_seq(N, self()),
    ?line List = receive_any(),
    ?line List = receive_any(),
    Papa = self(),
    spawn_link(fun() -> ?line {ok,1} = send_list_seq(N, Papa) end),
    ?line List = receive_any(),

    ?line {ok, 1, BlobS} = send_new_blob(self(), other_term()),
    ?line BlobR = receive_any(),
    io:format("Sent ~p\nGot ~p\n", [BlobS, BlobR]),
    ?line BlobR = BlobS,

    %% send to dead pid
    {DeadPid, DeadMon} = spawn_monitor(fun() -> void end),
    ?line {'DOWN', DeadMon, process, DeadPid, normal} = receive_any(),
    {ok,0} = send_list_seq(7, DeadPid),
    ok.

send2(doc) -> ["More NIF message sending"];
send2(Config) when is_list(Config) ->
    ensure_lib_loaded(Config),

    send2_do1(fun send_blob_dbg/2),
    ok.

send_threaded(doc) -> ["Send msg from user thread"];
send_threaded(Config) when is_list(Config) ->
    case erlang:system_info(smp_support) of
	true ->
	    send2_do1(fun(ME,To) -> send_blob_thread_dbg(ME,To,join) end),
	    send2_do1(fun(ME,To) -> send_blob_thread_and_join(ME,To) end),
	    ok;
	false ->
	    {skipped,"No threaded send on non-SMP"}
    end.


send2_do1(SendBlobF) ->
    io:format("sending to self=~p\n",[self()]),
    send2_do2(SendBlobF, self()),

    Papa = self(),
    Forwarder = spawn_link(fun() -> forwarder(Papa) end),
    io:format("sending to forwarder pid=~p\n",[Forwarder]),
    send2_do2(SendBlobF, Forwarder),
    Forwarder ! terminate,
    ?line {Forwarder, 4} = receive_any(),
    ok.

send2_do2(SendBlobF, To) ->   
    MsgEnv = alloc_msgenv(),
    repeat(50, fun(_) -> grow_blob(MsgEnv,other_term()) end, []),
    ?line {ok,1,Blob0} = SendBlobF(MsgEnv, To),
    ?line Blob1 = receive_any(),
    ?line Blob1 = Blob0,
    
    clear_msgenv(MsgEnv),
    repeat(50, fun(_) -> grow_blob(MsgEnv,other_term()) end, []),
    ?line {ok,1,Blob2} = SendBlobF(MsgEnv, To),
    ?line Blob3 = receive_any(),
    ?line Blob3 = Blob2,

    clear_msgenv(MsgEnv),
    repeat(50, fun(_) -> grow_blob(MsgEnv,other_term()) end, []),

    clear_msgenv(MsgEnv),
    repeat(50, fun(_) -> grow_blob(MsgEnv,other_term()) end, []),
    ?line {ok,1,Blob4} = SendBlobF(MsgEnv, To),
    ?line Blob5 = receive_any(),
    ?line Blob5 = Blob4,

    clear_msgenv(MsgEnv),
    clear_msgenv(MsgEnv),
    repeat(50, fun(_) -> grow_blob(MsgEnv,other_term()) end, []),
    ?line {ok,1,Blob6} = SendBlobF(MsgEnv, To),
    ?line Blob7 = receive_any(),
    ?line Blob7 = Blob6,

    ok.


send_blob_thread_and_join(MsgEnv, To) ->
    ?line {ok,Blob} = send_blob_thread_dbg(MsgEnv, To, no_join),
    ?line {ok,SendRes} = join_send_thread(MsgEnv),
    {ok,SendRes,Blob}.

send_blob_dbg(MsgEnv, To) ->
    Ret = send_blob(MsgEnv, To),
    %%io:format("send_blob to ~p returned ~p\n",[To,Ret]),
    Ret.

send_blob_thread_dbg(MsgEnv, To, Join) ->
    Ret = send_blob_thread(MsgEnv, To, Join),
    %%io:format("send_blob_thread to ~p Join=~p returned ~p\n",[To,Join,Ret]),
    Ret.


forwarder(To) ->
    forwarder(To, 0).
forwarder(To, N) ->
    case receive_any() of
	terminate ->
	    To ! {self(), N};
	Msg ->	    
	    To ! Msg,	   
	    forwarder(To, N+1)
    end.

other_term() ->
    {fun(X,Y) -> X*Y end, make_ref()}.

send3(doc) -> ["Message sending stress test"];
send3(Config) when is_list(Config) ->
    %% Let a number of processes send random message blobs between each other
    %% using enif_send. Kill and spawn new ones randomly to keep a ~constant
    %% number of workers running.
    Seed = {erlang:monotonic_time(),
	    erlang:time_offset(),
	    erlang:unique_integer()},
    io:format("seed: ~p\n",[Seed]), 
    random:seed(Seed),    
    ets:new(nif_SUITE,[named_table,public]),
    ?line true = ets:insert(nif_SUITE,{send3,0,0,0,0}),
    timer:send_after(10000, timeout), % Run for 10 seconds
    SpawnCnt = send3_controller(0, [], [], 20),
    ?line [{_,Rcv,SndOk,SndFail,Balance}] = ets:lookup(nif_SUITE,send3),
    io:format("spawns=~p received=~p, sent=~p send-failure=~p balance=~p\n",
              [SpawnCnt,Rcv,SndOk,SndFail,Balance]),
    ets:delete(nif_SUITE).

send3_controller(SpawnCnt, [], _, infinity) ->
    SpawnCnt;
send3_controller(SpawnCnt0, Mons0, Pids0, Tick) ->
    receive 
        timeout ->
            io:format("Timeout. Sending 'halt' to ~p\n",[Pids0]),            
            lists:foreach(fun(P) -> P ! {halt,self()} end, Pids0),
            lists:foreach(fun(P) -> receive {halted,P} -> ok end end, Pids0),
            QTot = lists:foldl(fun(P,QSum) ->
                                 {message_queue_len,QLen} = 
                                    erlang:process_info(P,message_queue_len),
                                 QSum + QLen
                               end, 0, Pids0),
            io:format("Total queue length ~p\n",[QTot]),            
            lists:foreach(fun(P) -> P ! die end, Pids0),
            send3_controller(SpawnCnt0, Mons0, [], infinity);
        {'DOWN', MonRef, process, _Pid, _} ->
            Mons1 = lists:delete(MonRef, Mons0),
            %%io:format("Got DOWN from ~p. Monitors left: ~p\n",[Pid,Mons1]),            
            send3_controller(SpawnCnt0, Mons1, Pids0, Tick)
    after Tick -> 
        Max = 20,
        N = length(Pids0),
        PidN = random:uniform(Max),
        %%io:format("N=~p PidN=~p Pids0=~p\n", [N,PidN,Pids0]), 
        case PidN > N of
            true ->
                {NewPid,Mon} = spawn_opt(fun send3_proc/0, [link,monitor]),                
                lists:foreach(fun(P) -> P ! {is_born,NewPid} end, Pids0),
                ?line Balance = ets:lookup_element(nif_SUITE,send3,5),
                Inject = (Balance =< 0),
                case Inject of
                    true ->  ok;
                    false -> ets:update_element(nif_SUITE,send3,{5,-1})
                end,
                NewPid ! {pids,Pids0,Inject},
                send3_controller(SpawnCnt0+1, [Mon|Mons0], [NewPid|Pids0], Tick);
            false ->
                KillPid = lists:nth(PidN,Pids0),
                KillPid ! die,
                Pids1 = lists:delete(KillPid, Pids0),
                lists:foreach(fun(P) -> P ! {is_dead,KillPid} end, Pids1),
                send3_controller(SpawnCnt0, Mons0, Pids1, Tick)
        end        
   end.

send3_proc() ->
    %%io:format("Process ~p spawned\n",[self()]),
    send3_proc([self()], {0,0,0}, {1,2,3,4,5}).
send3_proc(Pids0, Counters={Rcv,SndOk,SndFail}, State0) ->
    %%io:format("~p: Pids0=~p", [self(), Pids0]),
    %%timer:sleep(10),
    receive 
        {pids, Pids1, Inject} ->
            %%io:format("~p: got ~p Inject=~p\n", [self(), Pids1, Inject]), 
            ?line Pids0 = [self()],
            Pids2 = [self() | Pids1],
            case Inject of
                true -> send3_proc_send(Pids2, Counters, State0);
                false -> send3_proc(Pids2, Counters, State0)
            end;
        {is_born, Pid} ->
            %%io:format("~p: is_born ~p, got ~p\n", [self(), Pid, Pids0]), 
            send3_proc([Pid | Pids0], Counters, State0);
        {is_dead, Pid} ->            
            Pids1 = lists:delete(Pid,Pids0),
            %%io:format("~p: is_dead ~p, got ~p\n", [self(), Pid, Pids1]),
            send3_proc(Pids1, Counters, State0);
        {blob, Blob0} ->
            %%io:format("~p: blob ~p\n", [self(), Blob0]),
            State1 = send3_new_state(State0, Blob0),
            send3_proc_send(Pids0, {Rcv+1,SndOk,SndFail}, State1);
        die ->
            %%io:format("Process ~p terminating, stats = ~p\n",[self(),Counters]),
            {message_queue_len,Dropped} = erlang:process_info(self(),message_queue_len),
            _R = ets:update_counter(nif_SUITE,send3,
                               [{2,Rcv},{3,SndOk},{4,SndFail},{5,1-Dropped}]),
            %%io:format("~p: dies R=~p\n", [self(), R]),
            ok;
        {halt,Papa} ->
            Papa ! {halted,self()},
            io:format("~p halted\n",[self()]),
            receive die -> ok end,
            io:format("~p dying\n",[self()])
    end.

send3_proc_send(Pids, {Rcv,SndOk,SndFail}, State0) ->
    To = lists:nth(random:uniform(length(Pids)),Pids),
    Blob = send3_make_blob(),
    State1 = send3_new_state(State0,Blob), 
    case send3_send(To, Blob) of
        true ->
            send3_proc(Pids, {Rcv,SndOk+1,SndFail}, State1);
        false ->
            send3_proc(Pids, {Rcv,SndOk,SndFail+1}, State1)
    end.


send3_make_blob() ->    
    case random:uniform(20)-1 of
        0 -> {term,[]};
        N ->
            MsgEnv = alloc_msgenv(), 
            repeat(N bsr 1,
                   fun(_) -> grow_blob(MsgEnv,other_term(),random:uniform(1 bsl 20))
                   end, void),
            case (N band 1) of
                0 -> {term,copy_blob(MsgEnv)};
                1 -> {msgenv,MsgEnv}
            end
    end.

send3_send(Pid, Msg) ->
    %% 90% enif_send and 10% normal bang
    case random:uniform(10) of
        1 -> send3_send_bang(Pid,Msg);
        _ -> send3_send_nif(Pid,Msg)
    end.
send3_send_nif(Pid, {term,Blob}) ->
    %%io:format("~p send term nif\n",[self()]),
    send_term(Pid, {blob, Blob}) =:= 1;
send3_send_nif(Pid, {msgenv,MsgEnv}) ->
    %%io:format("~p send blob nif\n",[self()]),   
    send3_blob(MsgEnv, Pid, blob) =:= 1.

send3_send_bang(Pid, {term,Blob}) ->
    %%io:format("~p send term bang\n",[self()]),   
    Pid ! {blob, Blob},
    true;
send3_send_bang(Pid, {msgenv,MsgEnv}) ->
    %%io:format("~p send blob bang\n",[self()]),   
    Pid ! {blob, copy_blob(MsgEnv)},
    true.

send3_new_state(State, Blob) ->
    case random:uniform(5+2) of
        N when N =< 5-> setelement(N, State, Blob);
        _ -> State  % Don't store blob
    end.

neg(doc) -> ["Negative testing of load_nif"];
neg(Config) when is_list(Config) ->
    TmpMem = tmpmem(),
    ?line {'EXIT',{badarg,_}} = (catch erlang:load_nif(badarg, 0)),
    ?line {error,{load_failed,_}} = erlang:load_nif("pink_unicorn", 0),
    
    ?line Data = ?config(data_dir, Config),
    ?line File = filename:join(Data, "nif_mod"),
    ?line {ok,nif_mod,Bin} = compile:file(File, [binary,return_errors]),
    ?line {module,nif_mod} = erlang:load_module(nif_mod,Bin),

    ?line {error,{bad_lib,_}} = nif_mod:load_nif_lib(Config, no_init),    
    ?line verify_tmpmem(TmpMem),
    ?line ok.

is_checks(doc) -> ["Test all enif_is functions"];
is_checks(Config) when is_list(Config) ->
    ?line ensure_lib_loaded(Config, 1),
    ?line ok = check_is(hejsan, <<19,98>>, make_ref(), ok, fun() -> ok end,
			self(), hd(erlang:ports()), [], [1,9,9,8],
			{hejsan, "hejsan", [$h,"ejs",<<"an">>]}, 12),
    ?line ok = check_is(hejsan, <<19,98>>, make_ref(), ok, fun() -> ok end,
			self(), hd(erlang:ports()), [], [1,9,9,8],
			{hejsan, "hejsan", [$h,"ejs",<<"an">>]}, -12),
    ?line ok = check_is(hejsan, <<19,98>>, make_ref(), ok, fun() -> ok end,
			self(), hd(erlang:ports()), [], [1,9,9,8],
			{hejsan, "hejsan", [$h,"ejs",<<"an">>]}, 18446744073709551617),
    ?line ok = check_is(hejsan, <<19,98>>, make_ref(), ok, fun() -> ok end,
			self(), hd(erlang:ports()), [], [1,9,9,8],
			{hejsan, "hejsan", [$h,"ejs",<<"an">>]}, -18446744073709551617),
    ?line ok = check_is(hejsan, <<19,98>>, make_ref(), ok, fun() -> ok end,
			self(), hd(erlang:ports()), [], [1,9,9,8],
			{hejsan, "hejsan", [$h,"ejs",<<"an">>]}, 99.146),
    ?line ok = check_is(hejsan, <<19,98>>, make_ref(), ok, fun() -> ok end,
			self(), hd(erlang:ports()), [], [1,9,9,8],
			{hejsan, "hejsan", [$h,"ejs",<<"an">>]}, -99.146),
    ?line ok = check_is(hejsan, <<19,98>>, make_ref(), ok, fun() -> ok end,
			self(), hd(erlang:ports()), [], [1,9,9,8],
			{hejsan, "hejsan", [$h,"ejs",<<"an">>]}, 18446744073709551616.2e2),
    ?line ok = check_is(hejsan, <<19,98>>, make_ref(), ok, fun() -> ok end,
			self(), hd(erlang:ports()), [], [1,9,9,8],
			{hejsan, "hejsan", [$h,"ejs",<<"an">>]}, -18446744073709551616.2e2),
    try
        ?line check_is_exception(),
        ?line throw(expected_badarg)
    catch
        error:badarg ->
            ?line ok
    end.

get_length(doc) -> ["Test all enif_get_length functions"];
get_length(Config) when is_list(Config) ->
    ?line ensure_lib_loaded(Config, 1),
    ?line ok = length_test(hejsan, "hejsan", [], [], not_a_list).

ensure_lib_loaded(Config) ->
    ensure_lib_loaded(Config, 1).
ensure_lib_loaded(Config, Ver) ->
    ?line case lib_version() of
	      undefined ->
		  ?line Path = ?config(data_dir, Config),    
		  ?line Lib = "nif_SUITE." ++ integer_to_list(Ver),
		  ?line ok = erlang:load_nif(filename:join(Path,Lib), []);
	      Ver when is_integer(Ver) ->
		  ok
	  end.

make_atom(Config) when is_list(Config) ->
    ?line ensure_lib_loaded(Config, 1),
    An0Atom = an0atom,
    An0Atom0 = 'an\000atom\000',
    ?line Atoms = make_atoms(),
    ?line 7 = size(Atoms),
    ?line Atoms = {An0Atom,An0Atom,An0Atom,An0Atom0,An0Atom,An0Atom,An0Atom0}.

make_string(Config) when is_list(Config) ->
    ?line ensure_lib_loaded(Config, 1),
    ?line Strings = make_strings(),
    ?line 5 = size(Strings),
    A0String = "a0string",
    A0String0 = [$a,0,$s,$t,$r,$i,$n,$g,0],
    AStringWithAccents = [$E,$r,$l,$a,$n,$g,$ ,16#e4,$r,$ ,$e,$t,$t,$ ,$g,$e,$n,$e,$r,$e,$l,$l,$t,$ ,$p,$r,$o,$g,$r,$a,$m,$s,$p,$r,16#e5,$k],
    ?line Strings = {A0String,A0String,A0String,A0String0, AStringWithAccents}.

reverse_list_test(Config) ->
    ?line ensure_lib_loaded(Config, 1),
    List = lists:seq(1,100),
    RevList = lists:reverse(List),
    ?line RevList = reverse_list(List),
    ?line badarg = reverse_list(foo).

otp_9668(doc) -> ["Memory leak of tmp-buffer when inspecting iolist or unaligned binary in unbound environment"];
otp_9668(Config) ->
    ensure_lib_loaded(Config, 1),
    TmpMem = tmpmem(),
    IOList = ["This",' ',<<"is">>,' ',[<<"an iolist">>,'.']],    
    otp_9668_nif(IOList),

    <<_:5/bitstring,UnalignedBin:10/binary,_/bitstring>> = <<"Abuse me as unaligned">>,
    otp_9668_nif(UnalignedBin),

    ?line verify_tmpmem(TmpMem),
    ok.

otp_9828(doc) -> ["Copy of writable binary"];
otp_9828(Config) ->
    ensure_lib_loaded(Config, 1),

    otp_9828_loop(<<"I'm alive!">>, 1000).

otp_9828_loop(Bin, 0) ->
    ok;
otp_9828_loop(Bin, Val) ->
    WrtBin = <<Bin/binary, Val:32>>,
    ok = otp_9828_nif(WrtBin),
    otp_9828_loop(WrtBin, Val-1).


consume_timeslice(Config) when is_list(Config) ->
    CONTEXT_REDS = 2000,
    Me = self(),
    Go = make_ref(),
    RedDiff = make_ref(),
    Done = make_ref(),
    DummyMFA = {?MODULE,dummy_call,1},
    P = spawn(fun () ->
		      receive Go -> ok end,
		      {reductions, R1} = process_info(self(), reductions),
		      1 = consume_timeslice_nif(100, false),
		      dummy_call(111),
		      0 = consume_timeslice_nif(90, false),
		      dummy_call(222),
		      1 = consume_timeslice_nif(10, false),
		      dummy_call(333),
		      0 = consume_timeslice_nif(25, false),
		      0 = consume_timeslice_nif(25, false),
		      0 = consume_timeslice_nif(25, false),
		      1 = consume_timeslice_nif(25, false),
		      0 = consume_timeslice_nif(25, false),

		      ok = case consume_timeslice_nif(1, true) of
			       Cnt when Cnt > 70, Cnt < 80 -> ok;
			       Other -> Other
			   end,
		      dummy_call(444),

		      {reductions, R2} = process_info(self(), reductions),
		      Me ! {RedDiff, R2 - R1},
		      exit(Done)
	end),
    erlang:yield(),

    erlang:trace_pattern(DummyMFA, [], [local]),
    ?line 1 = erlang:trace(P, true, [call, running, procs, {tracer, self()}]),

    P ! Go,

    %% receive Go -> ok end,
    ?line {trace, P, in, _} = next_tmsg(P),
    
    %% consume_timeslice_nif(100),
    %% dummy_call(111)
    ?line {trace, P, out, _} = next_tmsg(P),
    ?line {trace, P, in, _} = next_tmsg(P),
    ?line {trace, P, call, {?MODULE,dummy_call,[111]}} = next_tmsg(P),

    %% consume_timeslice_nif(90),
    %% dummy_call(222)
    ?line {trace, P, call, {?MODULE,dummy_call,[222]}} = next_tmsg(P),

    %% consume_timeslice_nif(10),
    %% dummy_call(333)
    ?line {trace, P, out, _} = next_tmsg(P),
    ?line {trace, P, in, _} = next_tmsg(P),
    ?line {trace, P, call, {?MODULE,dummy_call,[333]}} = next_tmsg(P),

    %% 25,25,25,25, 25
    ?line {trace, P, out, {?MODULE,consume_timeslice_nif,2}} = next_tmsg(P),
    ?line {trace, P, in, {?MODULE,consume_timeslice_nif,2}} = next_tmsg(P),

    %% consume_timeslice(1,true)
    %% dummy_call(444)
    ?line {trace, P, out, DummyMFA} = next_tmsg(P),
    ?line {trace, P, in, DummyMFA} = next_tmsg(P),
    ?line {trace, P, call, {?MODULE,dummy_call,[444]}} = next_tmsg(P),

    %% exit(Done)
    ?line {trace, P, exit, Done} = next_tmsg(P),

    ExpReds = (100 + 90 + 10 + 25*5 + 75) * CONTEXT_REDS div 100,
    receive
	{RedDiff, Reductions} when Reductions < (ExpReds + 10), Reductions > (ExpReds - 10) ->
	    io:format("Reductions = ~p~n", [Reductions]),
	    ok;
	{RedDiff, Reductions} ->
	    ?t:fail({unexpected_reduction_count, Reductions})
    end,
    
    none = next_msg(P),

    ok.

nif_schedule(Config) when is_list(Config) ->
    ensure_lib_loaded(Config),
    A = "this is a string",
    B = {this,is,a,tuple},
    {B,A} = call_nif_schedule(A, B),
    ok = try call_nif_schedule(1, 2)
	 catch
	     error:badarg ->
		 [{?MODULE,call_nif_schedule,[1,2],_}|_] =
		     erlang:get_stacktrace(),
		 ok
	 end,
    ok.

dirty_nif(Config) when is_list(Config) ->
    try erlang:system_info(dirty_cpu_schedulers) of
	N when is_integer(N) ->
	    ensure_lib_loaded(Config),
	    Val1 = 42,
	    Val2 = "Erlang",
	    Val3 = list_to_binary([Val2, 0]),
	    {Val1, Val2, Val3} = call_dirty_nif(Val1, Val2, Val3),
	    LargeArray = lists:duplicate(1000, ok),
	    LargeArray = call_dirty_nif_zero_args(),
	    ok
    catch
	error:badarg ->
	    {skipped,"No dirty scheduler support"}
    end.

dirty_nif_send(Config) when is_list(Config) ->
    try erlang:system_info(dirty_cpu_schedulers) of
	N when is_integer(N) ->
	    ensure_lib_loaded(Config),
	    Parent = self(),
	    Pid = spawn_link(fun() ->
				     Self = self(),
				     {ok, Self} = receive_any(),
				     Parent ! {ok, Self}
			     end),
	    {ok, Pid} = send_from_dirty_nif(Pid),
	    {ok, Pid} = receive_any(),
	    ok
    catch
	error:badarg ->
	    {skipped,"No dirty scheduler support"}
    end.

dirty_nif_exception(Config) when is_list(Config) ->
    try erlang:system_info(dirty_cpu_schedulers) of
	N when is_integer(N) ->
	    ensure_lib_loaded(Config),
	    try
		%% this checks that the expected exception occurs when the
		%% dirty NIF returns the result of enif_make_badarg
		%% directly
	        call_dirty_nif_exception(1),
	        ?t:fail(expected_badarg)
	    catch
	        error:badarg ->
		    [{?MODULE,call_dirty_nif_exception,[1],_}|_] =
			erlang:get_stacktrace(),
		    ok
	    end,
	    try
		%% this checks that the expected exception occurs when the
		%% dirty NIF calls enif_make_badarg at some point but then
		%% returns a value that isn't an exception
	        call_dirty_nif_exception(0),
	        ?t:fail(expected_badarg)
	    catch
	        error:badarg ->
		    [{?MODULE,call_dirty_nif_exception,[0],_}|_] =
			erlang:get_stacktrace(),
		    ok
	    end,
	    %% this checks that a dirty NIF can raise various terms as
	    %% exceptions
	    ok = nif_raise_exceptions(call_dirty_nif_exception)
    catch
	error:badarg ->
	    {skipped,"No dirty scheduler support"}
    end.

nif_exception(Config) when is_list(Config) ->
    ensure_lib_loaded(Config),
    try
	%% this checks that the expected exception occurs when the NIF
	%% calls enif_make_badarg at some point but then tries to return a
	%% value that isn't an exception
	call_nif_exception(0),
	?t:fail(expected_badarg)
    catch
	error:badarg ->
	    ok
    end,
    %% this checks that a NIF can raise various terms as exceptions
    ok = nif_raise_exceptions(call_nif_exception),
    ok.

nif_nan_and_inf(Config) when is_list(Config) ->
    ensure_lib_loaded(Config),
    try
	call_nif_nan_or_inf(nan),
	?t:fail(expected_badarg)
    catch
	error:badarg ->
	    ok
    end,
    try
	call_nif_nan_or_inf(inf),
	?t:fail(expected_badarg)
    catch
	error:badarg ->
	    ok
    end,
    try
	call_nif_nan_or_inf(tuple),
	?t:fail(expected_badarg)
    catch
	error:badarg ->
	    ok
    end.

nif_atom_too_long(Config) when is_list(Config) ->
    ensure_lib_loaded(Config),
    try
	call_nif_atom_too_long(all),
	?t:fail(expected_badarg)
    catch
	error:badarg ->
	    ok
    end,
    try
	call_nif_atom_too_long(len),
	?t:fail(expected_badarg)
    catch
	error:badarg ->
	    ok
    end.

next_msg(_Pid) ->
    receive
	M -> M
    after 100 ->
	  none
    end.

next_tmsg(Pid) ->    
    receive TMsg when is_tuple(TMsg),
		      element(1, TMsg) == trace,
		      element(2, TMsg) == Pid ->
     	    TMsg
     after 100 ->
     	    none
     end.

dummy_call(_) ->    
    ok.

tmpmem() ->
    case erlang:system_info({allocator,temp_alloc}) of
	false -> undefined;
	MemInfo ->
	    MSBCS = lists:foldl(
		      fun ({instance, 0, _}, Acc) ->
			      Acc; % Ignore instance 0
			  ({instance, _, L}, Acc) ->
			      {value,{_,MBCS}} = lists:keysearch(mbcs, 1, L),
			      {value,{_,SBCS}} = lists:keysearch(sbcs, 1, L),
			      [MBCS,SBCS | Acc]
		      end,
		      [],
		      MemInfo),
	    lists:foldl(
	      fun(L, {Bl0,BlSz0}) ->
		      {value,{_,Bl,_,_}} = lists:keysearch(blocks, 1, L),
		      {value,{_,BlSz,_,_}} = lists:keysearch(blocks_size, 1, L),
		      {Bl0+Bl,BlSz0+BlSz}
	      end, {0,0}, MSBCS)
    end.

verify_tmpmem(MemInfo) ->
    %%wait_for_test_procs(),
    case tmpmem() of
	MemInfo ->
	    io:format("Tmp mem info: ~p", [MemInfo]),
	    case MemInfo of
		{notsup,undefined} ->
		    %% Use 'erl +Mea max' to do more complete memory leak testing.
		    {comment,"Incomplete or no mem leak testing"};
		_ ->
		    ok
	    end;
	Other ->
	    io:format("Expected: ~p", [MemInfo]),
	    io:format("Actual:   ~p", [Other]),
	    ?t:fail()
    end.

call(Pid,Cmd) ->
    %%io:format("~p calling ~p with ~p\n",[self(), Pid, Cmd]),
    Pid ! {self(), Cmd},
    receive
	{Pid,Reply} -> Reply
    end.

receive_any() ->
    receive M -> M end.	     

repeat(0, _, Arg) ->
    Arg;
repeat(N, Fun, Arg0) ->
    repeat(N-1, Fun, Fun(Arg0)).

check(Exp,Got,Line) ->
    case Got of
 	Exp -> Exp;	    
  	_ ->
  	    io:format("CHECK at ~p: Expected ~p but got ~p\n",[Line,Exp,Got]),
 	    Got
    end.

nif_raise_exceptions(NifFunc) ->
    ExcTerms = [{error, test}, "a string", <<"a binary">>,
		42, [1,2,3,4,5], [{p,1},{p,2},{p,3}]],
    lists:foldl(fun(Term, ok) ->
			try
			    erlang:apply(?MODULE,NifFunc,[Term]),
			    ?t:fail({expected,Term})
			catch
			    error:Term ->
				[{?MODULE,NifFunc,[Term],_}|_] = erlang:get_stacktrace(),
				ok
			end
		end, ok, ExcTerms).

-define(ERL_NIF_TIME_ERROR, -9223372036854775808).
-define(TIME_UNITS, [seconds, milli_seconds, micro_seconds, nano_seconds]).

nif_monotonic_time(Config) ->
    ?ERL_NIF_TIME_ERROR = monotonic_time(invalid_time_unit),
    mtime_loop(1000000).

mtime_loop(0) ->
    ok;
mtime_loop(N) ->
    chk_mtime(?TIME_UNITS),
    mtime_loop(N-1).

chk_mtime([]) ->
    ok;
chk_mtime([TU|TUs]) ->
    A = erlang:monotonic_time(TU),
    B = monotonic_time(TU),
    C = erlang:monotonic_time(TU),
    try
	true = A =< B,
	true = B =< C
    catch
	_ : _ ->
	    ?t:fail({monotonic_time_missmatch, TU, A, B, C})
    end,
    chk_mtime(TUs).

nif_time_offset(Config) ->
    ?ERL_NIF_TIME_ERROR = time_offset(invalid_time_unit),
    toffs_loop(1000000).

toffs_loop(0) ->
    ok;
toffs_loop(N) ->
    chk_toffs(?TIME_UNITS),
    toffs_loop(N-1).

chk_toffs([]) ->
    ok;
chk_toffs([TU|TUs]) ->
    TO = erlang:time_offset(TU),
    NifTO = time_offset(TU),
    case TO =:= NifTO of
	true ->
	    ok;
	false ->
	    case erlang:system_info(time_warp_mode) of
		no_time_warp ->
		    ?t:fail({time_offset_mismatch, TU, TO, NifTO});
		_ ->
		    %% Most frequent time offset change
		    %% is currently only every 15:th
		    %% second so this should currently
		    %% work...
		    NTO = erlang:time_offset(TU),
		    case NifTO =:= NTO of
			true ->
			    ok;
			false ->
			    ?t:fail({time_offset_mismatch, TU, TO, NifTO, NTO})
		    end
	    end
    end,
    chk_toffs(TUs).

nif_convert_time_unit(Config) ->
    ?ERL_NIF_TIME_ERROR = convert_time_unit(0, seconds, invalid_time_unit),
    ?ERL_NIF_TIME_ERROR = convert_time_unit(0, invalid_time_unit, seconds),
    ?ERL_NIF_TIME_ERROR = convert_time_unit(0, invalid_time_unit, invalid_time_unit),
    lists:foreach(fun (Offset) ->
			  lists:foreach(fun (Diff) ->
						chk_ctu(Diff+(Offset*1000*1000*1000))
					end,
					[999999999999,
					 99999999999,
					 9999999999,
					 999999999,
					 99999999,
					 9999999,
					 999999,
					 99999,
					 999,
					 99,
					 9,
					 1,
					 11,
					 101,
					 1001,
					 10001,
					 100001,
					 1000001,
					 10000001,
					 100000001,
					 1000000001,
					 100000000001,
					 1000000000001,
					 5,
					 50,
					 500,
					 5000,
					 50000,
					 500000,
					 5000000,
					 50000000,
					 500000000,
					 5000000000,
					 50000000000,
					 500000000000])
		  end,
		  [-4711, -1000, -475, -5, -4, -3, -2, -1, 0,
		   1, 2, 3, 4, 5, 475, 1000, 4711]),
    ctu_loop(1000000).

ctu_loop(0) ->
    ok;
ctu_loop(N) ->
    chk_ctu(erlang:monotonic_time(nano_seconds)),
    ctu_loop(N-1).

chk_ctu(Time) ->
    chk_ctu(Time, ?TIME_UNITS).

chk_ctu(_Time, []) ->
    ok;
chk_ctu(Time, [FromTU|FromTUs]) ->
    chk_ctu(Time, FromTU, ?TIME_UNITS),
    chk_ctu(Time, FromTUs).

chk_ctu(_Time, _FromTU, []) ->
    ok;
chk_ctu(Time, FromTU, [ToTU|ToTUs]) ->
    T = erlang:convert_time_unit(Time, nano_seconds, FromTU),
    TE = erlang:convert_time_unit(T, FromTU, ToTU),
    TN = convert_time_unit(T, FromTU, ToTU),
    case TE =:= TN of
	false ->
	    ?t:fail({conversion_mismatch, FromTU, T, ToTU, TE, TN});
	true ->
	    chk_ctu(Time, FromTU, ToTUs)
    end.

%% The NIFs:
lib_version() -> undefined.
call_history() -> ?nif_stub.
hold_nif_mod_priv_data(_Ptr) -> ?nif_stub.
nif_mod_call_history() -> ?nif_stub.
list_seq(_To) -> ?nif_stub.
type_test() -> ?nif_stub.
tuple_2_list(_) -> ?nif_stub.    
is_identical(_,_) -> ?nif_stub.
compare(_,_) -> ?nif_stub.
many_args_100(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> ?nif_stub.
clone_bin(_) -> ?nif_stub.
make_sub_bin(_,_,_) -> ?nif_stub.
string_to_bin(_,_) -> ?nif_stub.
atom_to_bin(_,_) -> ?nif_stub.    
macros(_) -> ?nif_stub.
tuple_2_list_and_tuple(_) -> ?nif_stub.
iolist_2_bin(_) -> ?nif_stub.
get_resource_type(_) -> ?nif_stub.
alloc_resource(_,_) -> ?nif_stub.
make_resource(_) -> ?nif_stub.
get_resource(_,_) -> ?nif_stub.
release_resource(_) -> ?nif_stub.
last_resource_dtor_call() -> ?nif_stub.
make_new_resource(_,_) -> ?nif_stub.
check_is(_,_,_,_,_,_,_,_,_,_,_) -> ?nif_stub.
check_is_exception() -> ?nif_stub.
length_test(_,_,_,_,_) -> ?nif_stub.
make_atoms() -> ?nif_stub.
make_strings() -> ?nif_stub.
make_new_resource_binary(_) -> ?nif_stub.
send_list_seq(_,_) -> ?nif_stub.     
send_new_blob(_,_) -> ?nif_stub.     
alloc_msgenv() -> ?nif_stub.
clear_msgenv(_) -> ?nif_stub.
grow_blob(_,_) -> ?nif_stub.
grow_blob(_,_,_) -> ?nif_stub.
send_blob(_,_) -> ?nif_stub.
send3_blob(_,_,_) -> ?nif_stub.
send_blob_thread(_,_,_) -> ?nif_stub.
join_send_thread(_) -> ?nif_stub.
copy_blob(_) -> ?nif_stub.
send_term(_,_) -> ?nif_stub.
reverse_list(_) -> ?nif_stub.
echo_int(_) -> ?nif_stub.
type_sizes() -> ?nif_stub.
otp_9668_nif(_) -> ?nif_stub.
otp_9828_nif(_) -> ?nif_stub.
consume_timeslice_nif(_,_) -> ?nif_stub.
call_nif_schedule(_,_) -> ?nif_stub.
call_dirty_nif(_,_,_) -> ?nif_stub.
send_from_dirty_nif(_) -> ?nif_stub.
call_dirty_nif_exception(_) -> ?nif_stub.
call_dirty_nif_zero_args() -> ?nif_stub.
call_nif_exception(_) -> ?nif_stub.
call_nif_nan_or_inf(_) -> ?nif_stub.
call_nif_atom_too_long(_) -> ?nif_stub.

%% maps
is_map_nif(_) -> ?nif_stub.
get_map_size_nif(_) -> ?nif_stub.
make_new_map_nif() -> ?nif_stub.
make_map_put_nif(_,_,_) -> ?nif_stub.
get_map_value_nif(_,_) -> ?nif_stub.
make_map_update_nif(_,_,_) -> ?nif_stub.
make_map_remove_nif(_,_) -> ?nif_stub.
maps_from_list_nif(_) -> ?nif_stub.
sorted_list_from_maps_nif(_) -> ?nif_stub.

%% Time
monotonic_time(_) -> ?nif_stub.
time_offset(_) -> ?nif_stub.
convert_time_unit(_,_,_) -> ?nif_stub.
    

nif_stub_error(Line) ->
    exit({nif_not_loaded,module,?MODULE,line,Line}).
