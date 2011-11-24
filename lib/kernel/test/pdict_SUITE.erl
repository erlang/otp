%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2011. All Rights Reserved.
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
-module(pdict_SUITE).
%% NB: The ?line macro cannot be used when testing the dictionary.


-include_lib("test_server/include/test_server.hrl").

-define(M(A,B),m(A,B,?MODULE,?LINE)).
-ifdef(DEBUG).
-define(DEBUGF(A,B), io:format(A,B)).
-else.
-define(DEBUGF(A,B), noop).
-endif.

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 simple/1, complicated/1, heavy/1, info/1]).
-export([init_per_testcase/2, end_per_testcase/2]).
-export([other_process/2]).

init_per_testcase(_Case, Config) ->
    ?line Dog = ?t:timetrap(test_server:minutes(10)),
    [{watchdog, Dog} | Config].
end_per_testcase(_Case, Config) ->
    Dog = ?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [simple, complicated, heavy, info].

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


simple(doc) ->
    ["Tests simple functionality in process dictionary."];
simple(suite) ->
    [];
simple(Config) when is_list(Config) ->
    XX = get(),
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
    Previous = erase(),
    N = case ?t:is_debug() of
	    false -> 500000;
	    true -> 5000
	end,
    comp_1(N),
    comp_2(N),
    N = comp_3(lists:sort(get()), 1),
    comp_4(get()),
    [] = get(),
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

heavy(doc) ->
    ["Tests heavy usage of the process dictionary"];
heavy(suite) ->
    [];
heavy(Config) when is_list(Config) ->
    XX = get(),
    erase(),
    time(50),
    ?M([],get()),
    time(500),
    ?M([],get()),
    time(5000),
    ?M([],get()),
    case {os:type(),?t:is_debug()} of
	{vxworks,_} -> ok;
	{_,true} -> ok;	    
	_ ->
	    time(50000),
	    ?M([], get())
    end,
    [put(Key, Value) || {Key,Value} <- XX],
    ok.

info(doc) ->
    ["Tests process_info(Pid, dictionary)"];
info(suite) ->
    [];
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
