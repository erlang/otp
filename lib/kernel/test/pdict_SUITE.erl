%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2016. All Rights Reserved.
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
-module(pdict_SUITE).


-include_lib("common_test/include/ct.hrl").

-define(M(A,B),m(A,B,?MODULE,?LINE)).
-ifdef(DEBUG).
-define(DEBUGF(A,B), io:format(A,B)).
-else.
-define(DEBUGF(A,B), noop).
-endif.

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 mixed/1,
	 simple/1, complicated/1, heavy/1, simple_all_keys/1, info/1]).
-export([init_per_testcase/2, end_per_testcase/2]).
-export([other_process/2]).

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() -> 
    [simple, complicated, heavy, simple_all_keys, info,
     mixed].

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


%% Tests simple functionality in process dictionary.
simple(Config) when is_list(Config) ->
    XX = get(),
    ok = match_keys(XX),
    erase(),
    L = [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,
	    q,r,s,t,u,v,x,y,z,'A','B','C','D'],
    ins_list_0(L),
    ins_list_1(L),
    L2 = lists:keysort(1, lists:map(fun(X) ->
					    {X, atom_to_list(X)}
				    end,
				    L)),
    ?DEBUGF("~p~n",[L2]),
    ?M(L2,lists:keysort(1, get())),
    ins_list_2(L),
    L3 = lists:keysort(1, lists:map(fun(X) ->
					    {hd(atom_to_list(X)) - $a, 
					     atom_to_list(X)}
				    end,
				    L) ++ L2),
    ?DEBUGF("~p~n",[L3]),
    ?M(L3, lists:keysort(1, get())),
    L4 = lists:map(fun(X) ->
			   lists:sort(get_keys(atom_to_list(X)))
		   end,
		   L),
    ?DEBUGF("~p~n",[L4]),
    ?M(L4,lists:map(fun(X) ->
			   lists:sort([X, hd(atom_to_list(X)) - $a])
		   end,
		   L)),
    erase(),
    ?M([],get()),
    [put(Key, Value) || {Key,Value} <- XX],
    ok.

complicated(Config) when is_list(Config) ->
    Previous = get(),
    ok = match_keys(Previous),
    Previous = erase(),
    N = case test_server:is_debug() of
	    false -> 500000;
	    true -> 5000
	end,
    comp_1(N),
    comp_2(N),
    N = comp_3(lists:sort(get()), 1),
    ok = match_keys(get()),
    comp_4(get()),
    [] = get(),
    [] = get_keys(),
    [put(Key, Value) || {Key,Value} <- Previous],
    ok.

comp_1(0) -> ok;
comp_1(N) ->
    undefined = put({key,N}, {value,N}),
    comp_1(N-1).

comp_2(0) -> ok;
comp_2(N) ->
    {value,N} = put({key,N}, {value,N*N}),
    comp_2(N-1).

comp_3([{{key,K},{value,V}}], K) when V =:= K*K ->
    K;
comp_3([{{key,K},{value,V}}|T], K) when V =:= K*K ->
    comp_3(T, K+1).

comp_4([{{key,_}=K,{value,_}=Val}|T]) ->
    Val = erase(K),
    comp_4(T);
comp_4([]) -> ok.

%% Tests heavy usage of the process dictionary.
heavy(Config) when is_list(Config) ->
    XX = get(),
    erase(),
    time(50),
    ?M([],get()),
    time(500),
    ?M([],get()),
    time(5000),
    ?M([],get()),
    case {os:type(),test_server:is_debug()} of
	{_,true} -> ok;	    
	_ ->
	    time(50000),
	    ?M([], get())
    end,
    [put(Key, Value) || {Key,Value} <- XX],
    ok.

simple_all_keys(Config) when is_list(Config) ->
    erase(),
    ok = simple_all_keys_add_loop(1000),
    [] = get_keys(),
    [] = get(),
    ok.

simple_all_keys_add_loop(0) ->
    simple_all_keys_del_loop(erlang:get_keys());
simple_all_keys_add_loop(N) ->
   put(gen_key(N),value),
   ok = match_keys(get()),
   simple_all_keys_add_loop(N-1).

simple_all_keys_del_loop([]) -> ok;
simple_all_keys_del_loop([K|Ks]) ->
    value = erase(K),
    ok = match_keys(get()),
    simple_all_keys_del_loop(Ks).

%% Tests process_info(Pid, dictionary).
info(Config) when is_list(Config) ->
    L = [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,
	    q,r,s,t,u,v,x,y,z,'A','B','C','D'],
    process_flag(trap_exit,true),
    Pid = spawn_link(?MODULE, other_process, [L,self()]),
    Dict = receive
	       {Pid, D} ->
		   D
	   end,
    ?M({dictionary, Dict}, process_info(Pid, dictionary)), 
    Pid ! bye,
    receive
	{'EXIT', Pid, _} ->
	    ok
    end,
    ok.
    
other_process(List,From) ->
    erase(),
    ins_list_1(List),
    From ! {self(), get()},
    receive
	bye ->
	    ok
    end.
    
ins_list_2([]) ->
    done;
ins_list_2([H|T]) ->
    X = {hd(atom_to_list(H)) - $a, atom_to_list(H)},
    _Y = put(element(1,X), element(2,X)),
    ?DEBUGF("Inserting ~w: ~w~n",[X,_Y]),
    ins_list_2(T).

ins_list_1([]) ->
    done;
ins_list_1([H|T]) ->
    X = {H, atom_to_list(H)},
    _Y = put(element(1,X), element(2,X)),
    ?DEBUGF("Inserting ~w: ~w~n",[X,_Y]),
    ins_list_1(T).

ins_list_0([]) ->
    done;
ins_list_0([H|T]) ->
    X = {H, H},
    _Y = put(element(1,X), element(2,X)),
    ?DEBUGF("Inserting ~w: ~w~n",[X,_Y]),
    ins_list_0(T).

time(N) ->
    ?DEBUGF("~p~n",[erlang:process_info(self())]),
    TT1 = erlang:now(),
    T1 = insert_testloop(N,N,0),
    TT2 = erlang:now(),
    T2 = lookup_testloop(N,N,0),
    TT3 = erlang:now(),
    T5 = delete_testloop(N,N,0),
    TT6 = erlang:now(),
    io:format("~p inserts took ~.2f(~.2f) seconds~n",
	      [N, nowdiff3(TT1,TT2), T1 / 100]),
    io:format("~p lookups took ~.2f(~.2f) seconds~n",
	      [N, nowdiff3(TT2,TT3), T2 / 100]),
    io:format("~p deletes took ~.2f(~.2f) seconds~n",
	      [N, nowdiff3(TT3,TT6), T5 / 100]),
    io:format("Total time for ~p elements is ~.2f(~.2f) seconds~n",
	      [N, nowdiff3(TT1,TT6), (T1+T2+T5) / 100]),
    ok.

key_to_object(Key) ->
    {Key, Key,[Key, Key, {Key, banan}]}.

time_call(Fun,Acc) ->
    T1 = erlang:now(),
    Ret = Fun(),
    T2 = erlang:now(),
    {nowdiff2(T1,T2)+Acc,Ret}.

delete_testloop(0, _X, Acc) ->
    ?DEBUGF("all ~p deleted~n",[_X]),
    Acc;

delete_testloop(N, X, Acc) ->
    Key = gen_key(N),
    Obj = key_to_object(Key),
    case get(Key) of
	Obj ->
	    ok;
	Y ->
	    io:format("Error - Object ~p does not exist when we are "
		      "gonna delete!(N=~p, result=~p)~n",[Obj,N,Y]),
	    exit({inconsistent_1, delete_testloop, Obj, N, Y})
    end,
    
    {T, Obj2} = time_call(fun() -> erase(Key) end, Acc),
    ?M(Obj,Obj2),
    case {(X-N) rem 10000,(X-N)} of
	{_,0} ->
	    ok;
	{0,_} ->
	    ?DEBUGF("~p~n",[X-N]);
	_ ->
	    ok
    end,
    case get(Key) of
	undefined ->
	    ok;
	Else ->
	    io:format("Error - Object ~p does still exist after "
		      "delete!(N=~p, result=~p)~n",[Obj,N,Else]),
	    exit({inconsistent_2, delete_testloop, Obj, N, Else})
    end,
    delete_testloop(N-1,X,T).

lookup_testloop(0, X, Acc) ->
    io:format("all ~p looked up~n",[X]),
    Acc;
lookup_testloop(N, X, Acc) ->
    Key = gen_key(N),
    D = key_to_object(Key),
    {T, D2} = time_call(fun() -> get(Key) end, Acc),
    ?M(D,D2),
    case {(X-N) rem 10000,(X-N)} of
	{_,0} ->
	    ok;
	{0,_} ->
	    ?DEBUGF("~p~n",[X-N]);
	_ ->
	    ok
    end,
    lookup_testloop(N-1,X,T).
    
insert_testloop(0,X,Acc) ->
    io:format("all ~p inserted~n",[X]),
    Acc;
insert_testloop(N,X,Acc) ->
    Key = gen_key(N),
    D = key_to_object(Key),
    {T,_} = time_call(fun() -> put(Key,D) end, Acc),
    case {(X-N) rem 10000,(X-N)} of
	{_,0} ->
	    ok;
	{0,_} ->
	    ?DEBUGF("~p~n",[X-N]);
	_ ->
	    ok
    end,
    insert_testloop(N-1,X,T).
    

gen_key(0,A)->
    A;
gen_key(N,A) ->
    X = ((N-1) rem 26) + $a,
    gen_key((N-1) div 26, [X|A]).
gen_key(N) ->
    gen_key(N+1,[]).

nowtonumber({Mega, Secs, Milli}) ->
    Milli div 10000 + Secs * 100 + Mega * 100000000.

nowdiff2(T1,T2) ->
    nowtonumber(T2) - nowtonumber(T1).
nowdiff3(T1,T2) ->
    (nowtonumber(T2) - nowtonumber(T1)) / 100.

m(A,B,Module,Line) ->
    case A == B of
	true ->
	    ok;
	_ ->
	    io:format("~p does not match ~p in module ~p, line ~p, exit.~n",
		      [A,B,Module,Line]),
	    exit({no_match,{A,B},Module,Line})
    end.

match_keys(All) ->
    Ks = lists:sort([K||{K,_}<-All]),
    Ks = lists:sort(erlang:get_keys()),
    ok.


%% Do random mixed put/erase to test grow/shrink
%% Written for a temporary bug in gc during shrink
mixed(_Config) ->
    Rand0 = rand:seed_s(exsplus),
    io:format("Random seed = ~p\n\n", [rand:export_seed_s(Rand0)]),

    erts_debug:set_internal_state(available_internal_state, true),
    try
	C = do_mixed([10,0,100,50,1000,500,600,100,150,1,11,2,30,0],
		     0,
		     array:new(),
		     1,
		     Rand0),
	io:format("\nDid total of ~p operations\n", [C])
    after
	erts_debug:set_internal_state(available_internal_state, false)
    end.

do_mixed([], _, _, C, _) ->
    C;
do_mixed([GoalN | Tail], GoalN, Array, C, Rand0) ->
    io:format("Reached goal of ~p keys in dict after ~p mixed ops\n",[GoalN, C]),
    GoalN = array:size(Array),
    do_mixed(Tail, GoalN, Array, C, Rand0);
do_mixed([GoalN | _]=Goals, CurrN, Array0, C, Rand0) ->
    CurrN = array:size(Array0),
    GrowPercent = case GoalN > CurrN of
		      true when CurrN == 0 -> 100;
		      true -> 75;
		      false -> 25
		  end,
    {R, Rand1} = rand:uniform_s(100, Rand0),
    case R of
	_ when R =< GrowPercent ->   %%%%%%%%%%%%% GROW
	    {Key, Rand2} = rand:uniform_s(10000, Rand1),
	    case put(Key, {Key,C}) of
		undefined ->
		    Array1 = array:set(CurrN, Key, Array0),
		    do_mixed(Goals, CurrN+1, Array1, C+1, Rand2);
		_ ->
		    do_mixed(Goals, CurrN, Array0, C+1, Rand2)
	    end;

	_ ->                          %%%%%%%%%% SHRINK
	    {Kix, Rand2} = rand:uniform_s(CurrN, Rand1),
	    Key = array:get(Kix-1, Array0),

	    %% provoke GC during shrink
	    erts_debug:set_internal_state(fill_heap, true),

	    {Key, _} = erase(Key),
	    Array1 = array:set(Kix-1, array:get(CurrN-1, Array0), Array0),
	    Array2 = array:resize(CurrN-1, Array1),
	    do_mixed(Goals, CurrN-1, Array2, C+1, Rand2)
    end.
