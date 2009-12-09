%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2009. All Rights Reserved.
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

-include("test_server.hrl").

-export([all/1, fin_per_testcase/2, basic/1, reload/1, upgrade/1, heap_frag/1,
	 types/1, many_args/1, neg/1]).

-export([many_args_100/100]).
-define(nif_stub,nif_stub_error(?LINE)).

all(suite) ->
    [basic, reload, upgrade, heap_frag, types, many_args, neg].

fin_per_testcase(_Func, _Config) ->
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
    ensure_lib_loaded(Config),

    ?line Data = ?config(data_dir, Config),
    ?line File = filename:join(Data, "nif_mod"),
    ?line {ok,nif_mod,Bin} = compile:file(File, [binary,return_errors]),
    ?line {module,nif_mod} = erlang:load_module(nif_mod,Bin),

    ?line nif_mod:load_nif_lib(Config, 1),

    ?line hold_nif_mod_priv_data(nif_mod:get_priv_data_ptr()),
    ?line [{load,1,1,101},{get_priv_data_ptr,1,2,102}] = nif_mod_call_history(),    
        
    ?line nif_mod:load_nif_lib(Config, 2),
    ?line 2 = nif_mod:lib_version(),
    ?line [{reload,2,1,201},{lib_version,2,2,202}] = nif_mod_call_history(),    

    ?line nif_mod:load_nif_lib(Config, 1),
    ?line 1 = nif_mod:lib_version(),
    ?line [{reload,1,1,101},{lib_version,1,2,102}] = nif_mod_call_history(),    

    ?line true = erlang:delete_module(nif_mod),
    ?line [] = nif_mod_call_history(),    

    %%?line false= check_process_code(Pid, nif_mod),
    ?line true = erlang:purge_module(nif_mod),
    ?line [{unload,1,3,103}] = nif_mod_call_history(),    

    ?line [?MODULE, nif_mod] = erlang:system_info(taints),
    ok.

upgrade(doc) -> ["Test upgrade callback in nif lib"];
upgrade(suite) -> [];  
upgrade(Config) when is_list(Config) ->    
    ensure_lib_loaded(Config),

    ?line Data = ?config(data_dir, Config),
    ?line File = filename:join(Data, "nif_mod"),
    ?line {ok,nif_mod,Bin} = compile:file(File, [binary,return_errors]),
    ?line {module,nif_mod} = erlang:load_module(nif_mod,Bin),

    ?line nif_mod:load_nif_lib(Config, 1),
    ?line {Pid,MRef} = nif_mod:start(),
    ?line 1 = call(Pid,lib_version),

    ?line hold_nif_mod_priv_data(nif_mod:get_priv_data_ptr()),
    ?line [{load,1,1,101},{lib_version,1,2,102},{get_priv_data_ptr,1,3,103}] = nif_mod_call_history(),    
        
    %% Module upgrade with same lib-version
    ?line {module,nif_mod} = erlang:load_module(nif_mod,Bin),
    ?line undefined = nif_mod:lib_version(),
    ?line 1 = call(Pid,lib_version),
    ?line [{lib_version,1,4,104}] = nif_mod_call_history(),

    ?line nif_mod:load_nif_lib(Config, 1),
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

    ?line nif_mod:load_nif_lib(Config, 1),
    ?line hold_nif_mod_priv_data(nif_mod:get_priv_data_ptr()),
    ?line 1 = call(Pid2,lib_version),
    ?line [{load,1,1,101},{get_priv_data_ptr,1,2,102},{lib_version,1,3,103}] = nif_mod_call_history(),

    ?line {module,nif_mod} = erlang:load_module(nif_mod,Bin),
    ?line undefined = nif_mod:lib_version(),
    ?line [] = nif_mod_call_history(),
    ?line 1 = call(Pid2,lib_version),
    ?line [{lib_version,1,4,104}] = nif_mod_call_history(),

    ?line nif_mod:load_nif_lib(Config, 2),
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
    ok.

heap_frag(doc) -> ["Test NIF building heap fragments"];
heap_frag(suite) -> [];  
heap_frag(Config) when is_list(Config) ->    
    ensure_lib_loaded(Config),
    
    heap_frag_do(1,1000000),
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
    ensure_lib_loaded(Config),
    ?line ok = type_test(),
    lists:foreach(fun(Tpl) ->
                    Lst = erlang:tuple_to_list(Tpl),                 
                    Lst = tuple_2_list(Tpl)
                  end,
                  [{},{ok},{{}},{[],{}},{1,2,3,4,5}]),
    Stuff = [[],{},0,0.0,(1 bsl 100),(fun()-> ok end),make_ref(),self()],
    [eq_cmp(A,clone(B)) || A<-Stuff, B<-Stuff],                   
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
    ?line ensure_lib_loaded(Config ,1),
    ?line ok = apply(?MODULE,many_args_100,lists:seq(1,100)),
    ?line ok = many_args_100(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100),
    ok.

        
    

neg(doc) -> ["Negative testing of load_nif"];
neg(suite) -> [];
neg(Config) when is_list(Config) ->
    ?line {'EXIT',{badarg,_}} = (catch erlang:load_nif(badarg, 0)),
    ?line {error,{load_failed,_}} = erlang:load_nif("pink_unicorn", 0),
    
    ?line Data = ?config(data_dir, Config),
    ?line File = filename:join(Data, "nif_mod"),
    ?line {ok,nif_mod,Bin} = compile:file(File, [binary,return_errors]),
    ?line {module,nif_mod} = erlang:load_module(nif_mod,Bin),

    ?line {error,{bad_lib,_}} = nif_mod:load_nif_lib(Config, no_init),    
    ?line ok.



ensure_lib_loaded(Config) ->
    ensure_lib_loaded(Config, 1).

ensure_lib_loaded(Config, Ver) ->
    ?line case lib_version() of
	      undefined ->
		  ?line Path = ?config(data_dir, Config),    
		  ?line Lib = "nif_SUITE." ++ integer_to_list(Ver),
		  ?line ok = erlang:load_nif(filename:join(Path,Lib), 0);
	      Ver when is_integer(Ver) ->
		  ok
	  end.

call(Pid,Cmd) ->
    %%io:format("~p calling ~p with ~p\n",[self(), Pid, Cmd]),
    Pid ! {self(), Cmd},
    receive
	{Pid,Reply} -> Reply
    end.

receive_any() ->
    receive M -> M end.	     

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
    
nif_stub_error(Line) ->
    exit({nif_not_loaded,module,?MODULE,line,Line}).
