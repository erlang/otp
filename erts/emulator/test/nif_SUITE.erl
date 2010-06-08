%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2010. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%

-module(nif_SUITE).

%%-define(line_trace,true).
-define(CHECK(Exp,Got), check(Exp,Got,?LINE)).
%%-define(CHECK(Exp,Got), ?line Exp = Got).

-include("test_server.hrl").

-export([all/1, 
	 %%init_per_testcase/2, 
	 fin_per_testcase/2, basic/1, reload/1, upgrade/1, heap_frag/1,
	 types/1, many_args/1, binaries/1, get_string/1, get_atom/1, api_macros/1,
	 from_array/1, iolist_as_binary/1, resource/1, resource_binary/1, resource_takeover/1,
	 threading/1, send/1, send2/1, send_threaded/1, neg/1, is_checks/1,
	 get_length/1, make_atom/1, make_string/1]).

-export([many_args_100/100]).


%% -export([lib_version/0,call_history/0,hold_nif_mod_priv_data/1,nif_mod_call_history/0,
%% 	 list_seq/1,type_test/0,tuple_2_list/1,is_identical/2,compare/2,
%% 	 clone_bin/1,make_sub_bin/3,string_to_bin/2,atom_to_bin/2,macros/1,
%% 	 tuple_2_list_and_tuple/1,iolist_2_bin/1,get_resource_type/1,alloc_resource/2,
%% 	 make_resource/1,get_resource/2,release_resource/1,last_resource_dtor_call/0,
%% 	 make_new_resource/2,make_new_resource_binary/1,send_list_seq/2,send_new_blob/2,
%% 	 alloc_msgenv/0,clear_msgenv/1,grow_blob/2,send_blob/2,send_blob_thread/3,
%% 	 join_send_thread/1]).


-define(nif_stub,nif_stub_error(?LINE)).

all(suite) ->
    [basic, reload, upgrade, heap_frag, types, many_args, binaries, get_string,
     get_atom, api_macros, from_array, iolist_as_binary, resource, resource_binary,
     resource_takeover, threading, send, send2, send_threaded, neg, is_checks,
     get_length, make_atom, make_string].

%%init_per_testcase(_Case, Config) ->
%%    ?line Dog = ?t:timetrap(?t:seconds(60*60*24)),
%%    [{watchdog, Dog}|Config].

fin_per_testcase(_Func, _Config) ->
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
    ?line [?MODULE] = erlang:system_info(taints),
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

    ?line [?MODULE, nif_mod] = erlang:system_info(taints),
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

    ?line [?MODULE, nif_mod] = erlang:system_info(taints),    
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
    ?line verify_tmpmem(TmpMem),
    ok.

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
    ?line [] = last_resource_dtor_call(),
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

    ?line [?MODULE, nif_mod] = erlang:system_info(taints),
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
			{hejsan, "hejsan", [$h,"ejs",<<"an">>]}).

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

tmpmem() ->
    case erlang:system_info({allocator,temp_alloc}) of
	false -> undefined;
	MemInfo ->
	    MSBCS = lists:foldl(
		      fun ({instance, _, L}, Acc) ->
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
check_is(_,_,_,_,_,_,_,_,_,_) -> ?nif_stub.
length_test(_,_,_,_,_) -> ?nif_stub.
make_atoms() -> ?nif_stub.
make_strings() -> ?nif_stub.
make_new_resource_binary(_) -> ?nif_stub.
send_list_seq(_,_) -> ?nif_stub.     
send_new_blob(_,_) -> ?nif_stub.     
alloc_msgenv() -> ?nif_stub.
clear_msgenv(_) -> ?nif_stub.
grow_blob(_,_) -> ?nif_stub.
send_blob(_,_) -> ?nif_stub.
send_blob_thread(_,_,_) -> ?nif_stub.
join_send_thread(_) -> ?nif_stub.


nif_stub_error(Line) ->
    exit({nif_not_loaded,module,?MODULE,line,Line}).
