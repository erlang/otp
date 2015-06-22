%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2000-2011. All Rights Reserved.
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

-module(select_SUITE).
-author('pan@erix.ericsson.se').

-export([test/0]).

%%
%% Define to run outside of test server
%%
%%-define(STANDALONE,1).
 
%%
%% Define for debug output
%%
%%-define(debug,1).
 
-ifdef(STANDALONE).
-define(config(A,B),config(A,B)).
-export([config/2]).
-define(fmt(A,B),io:format(A,B)).
-else.
-include_lib("test_server/include/test_server.hrl").
-define(fmt(A,B),test_server:format(A,B)).
-endif.
 
-ifdef(debug).
-ifdef(STANDALONE).
-define(line, erlang:display({?MODULE,?LINE}), ).
-endif.
-define(dbgformat(A,B),io:format(A,B)).
-else.
-ifdef(STANDALONE).
-define(line, noop, ).
-endif.
-define(dbgformat(A,B),noop).
-endif.
 
-ifdef(STANDALONE).
config(priv_dir,_) ->
    ".".
-else.
%% When run in test server.
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,select_test/1,
	 init_per_testcase/2, end_per_testcase/2, 
	 return_values/1]).

init_per_testcase(_Case, Config) when is_list(Config) ->
    ?line Dog=test_server:timetrap(test_server:seconds(1200)),
    [{watchdog, Dog}|Config].

end_per_testcase(_Case, Config) ->
    Dog=?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [return_values, select_test].

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


select_test(suite) ->
    [];
select_test(doc) ->
    ["Tests select in numerous ways"];
select_test(Config) when is_list(Config) ->
    do_test(Config).

return_values(suite) ->
    [];
return_values(doc) ->
    ["Tests return values in specific situations for select/3 and select/1"];
return_values(Config) when is_list(Config) ->
    do_return_values().

-endif.


table_factor({dets,_}) ->
    1;
table_factor({ets,_}) ->
    100.

gen_dets_filename(Config,N) ->
    filename:join(?config(priv_dir,Config),
		  "testdets_" ++ integer_to_list(N) ++ ".dets").

create_tables(Config) ->
    Hash = ets:new(xxx, []), 
    Tree = ets:new(yyy, [ordered_set]),
    Bag = ets:new(yyy, [bag]),
    DBag = ets:new(yyy, [duplicate_bag]),
    F1 = gen_dets_filename(Config,1),
    (catch file:delete(F1)),
    {ok,DetsPlain} = dets:open_file(testdets_1,
			       [{file, F1}]),
    F3 = gen_dets_filename(Config,3),
    (catch file:delete(F3)),
    {ok,DetsBag} = dets:open_file(testdets_3,
				     [{file, F3},{type, bag}]),
    F4 = gen_dets_filename(Config,4),
    (catch file:delete(F4)),
    {ok,DetsDBag} = dets:open_file(testdets_4,
				     [{file, F4},{type, duplicate_bag}]),
    [{ets,Hash}, {ets,Tree}, {ets,Bag}, {ets,DBag}, 
     {dets, DetsPlain}, {dets, DetsBag}, {dets, DetsDBag}].


gen_key(N,list) ->
    [N,N+1,N+2];
gen_key(N,tuple) ->
    {N,N+1,N+2};
gen_key(N,complex) ->
    {[N,N+1],N+2}.

gen_fun(N) ->
    fun() ->
	    N
    end.

gen_bin(N) ->
    list_to_binary(integer_to_list(N)).

gen_object(N,Type) ->
    L = integer_to_list(N),
    A = list_to_atom("a" ++ L),
    {gen_key(N,Type), L, A, gen_fun(N), gen_bin(N)}.
gen_object1(N,Type) ->
    L = integer_to_list(N),
    A = list_to_atom("a" ++ L),
    {gen_key(N,Type), A, L, gen_fun(N), gen_bin(N)}.

fill_table(_,0,_) ->
    ok;
fill_table({Mod,Tab},N,Type) ->
    Obj1 = gen_object1(N,Type),
    Obj = gen_object(N,Type),
    Mod:insert(Tab, Obj1),
    case Mod:info(Tab,type) of
	bag ->
	    Mod:insert(Tab, Obj);
	duplicate_bag ->
	    Mod:insert(Tab, Obj),
	    Mod:insert(Tab, Obj1);
	_ ->
	    ok
    end,
    fill_table({Mod,Tab},N-1,Type).

table_size(Tab) ->
    15 *table_factor(Tab).

build_tables(Config,Type) ->
    L = create_tables(Config),
    ?dbgformat("Tables: ~p~n",[L]),
    lists:foreach(fun(TD) ->
		      fill_table(TD,table_size(TD),Type)
		  end,
		  L),
    L.

destroy_tables([]) ->
    ok;
destroy_tables([{ets,Tab}|T]) ->
    ets:delete(Tab),
    destroy_tables(T);
destroy_tables([{dets,Tab}|T]) ->
    dets:close(Tab),
    destroy_tables(T).
    

init_random(Config) ->
    WriteDir = ReadDir = ?config(priv_dir,Config),
    (catch file:make_dir(WriteDir)),
    Seed = case file:consult(filename:join([ReadDir, 
					    "preset_random_seed2.txt"])) of
	       {ok,[X]} ->
		   X;
	       _ ->
		   {A,B,C} = erlang:timestamp(),
		   random:seed(A,B,C),
		   get(random_seed)
	   end,
    put(random_seed,Seed),
    {ok, F} = file:open(filename:join([WriteDir, "last_random_seed2.txt"]), 
			[write]),
    io:format(F,"~p. ~n",[Seed]),
    file:close(F),
    ok.

create_random_key(N,Type) ->
    gen_key(random:uniform(N),Type).

create_pb_key(N,list) ->
    X = random:uniform(N),
    case random:uniform(4) of
	3 -> {[X, X+1, '_'], fun([Z,Z1,P1]) ->  
				      [Z,Z1,P1] =:= [X,X+1,P1] end};
	2 -> {[X, '_', '_'], fun([Z,P1,P2]) ->  [Z,P1,P2] =:= [X,P1,P2] end};
	1 -> {[X, X+1, '$1'], fun([Z,Z1,P1]) ->  
				      [Z,Z1,P1] =:= [X,X+1,P1] end};
	_ -> {[X, '$1', '$2'], fun([Z,P1,P2]) ->  [Z,P1,P2] =:= [X,P1,P2] end}
    end;
create_pb_key(N, tuple) ->
    X = random:uniform(N),
    case random:uniform(2) of
	1 -> {{X, X+1, '$1'},fun({Z,Z1,P1}) ->  {Z,Z1,P1} =:= {X,X+1,P1} end};
	_ -> {{X, '$1', '$2'},fun({Z,P1,P2}) ->  {Z,P1,P2} =:= {X,P1,P2} end}
    end;
create_pb_key(N, complex) ->
    X = random:uniform(N),
    case random:uniform(2) of
	1 -> {{[X, X+1], '$1'}, fun({[Z,Z1],P1}) ->  
					{[Z,Z1],P1} =:= {[X,X+1],P1} end};
	_ -> {{[X, '$1'], '$2'},fun({[Z,P1],P2}) -> 
					{[Z,P1],P2} =:= {[X,P1],P2} end}
    end.
table_foldl(_Fun, Acc,{_Mod,_Tab},'$end_of_table') ->
    Acc;
table_foldl(Fun, Acc,{Mod,Tab},Key) ->
    Objs = Mod:lookup(Tab,Key),
    Acc2 = lists:foldl(Fun,Acc,Objs),
    ?dbgformat("Objs: ~p, Acc2: ~p~n",[Objs,Acc2]),
    table_foldl(Fun, Acc2, {Mod,Tab}, Mod:next(Tab,Key)).
table_foldl(Fun, Acc,{Mod,Tab}) ->
    table_foldl(Fun, Acc,{Mod,Tab},Mod:first(Tab)).

chunked_select(Mod,Tab,MS,0) ->
    Mod:select(Tab,MS);
chunked_select(Mod,Tab,MS,Chunk) when Chunk > 0->
    do_chunk_select(Mod, Mod:select(Tab,MS,Chunk),[]);
chunked_select(Mod,Tab,MS,Chunk) when Chunk < 0->
    case Mod of
	ets ->
	    do_chunk_select_reverse(Mod, 
				    Mod:select_reverse(Tab,MS,-Chunk),[]);
	_ ->
	    chunked_select(Mod,Tab,MS,-Chunk)
    end.


do_chunk_select_reverse(_Mod, '$end_of_table',Acc) ->
    %% OK, all this reversing is only needed for ordered_set, but
    %% this is only testcases, right?
    erlang:display(did_chunked_select_reverse),
    Acc; 
do_chunk_select_reverse(Mod, {L,C},Acc) ->
    NewAcc = lists:reverse(L)++Acc,
    do_chunk_select(Mod, Mod:select(C), NewAcc).

do_chunk_select(_Mod, '$end_of_table',Acc) ->
    %% OK, all this reversing is only needed for ordered_set, but
    %% this is only testcases, right?
    lists:reverse(Acc); 
do_chunk_select(Mod, {L,C},Acc) ->
    NewAcc = lists:reverse(L)++Acc,
    do_chunk_select(Mod, Mod:select(C), NewAcc).

cmp_ms_to_fun({Mod,Tab}, MS, Fun1, Fun2) ->
    cmp_ms_to_fun({Mod,Tab}, MS, Fun1, Fun2, 0).

cmp_ms_to_fun({Mod,Tab}, MS, Fun1, Fun2, ChunkSize) ->
    MSRes = lists:sort(chunked_select(Mod,Tab,MS,ChunkSize)),
    FunRes0 = table_foldl(Fun1,[],{Mod,Tab}),
    FunRes = case Fun2 of
		 F when is_function(F) ->
		     FunRes1 = table_foldl(F,[],{Mod,Tab}),
		     lists:merge(FunRes0,FunRes1);
		 [] ->
		     lists:sort(FunRes0)
	     end,
    case MSRes =:= FunRes of
	true ->
	    true;
	false ->
	    ?fmt("Match_spec result differs from fun result:~n",[]),
	    ?fmt("Parameters: ~p,~p,~p,~p~n", 
		      [{Mod,Tab}, MS, Fun1, Fun2]),
	    ?fmt("Match_spec Result: ~p~n", [MSRes]),
	    ?fmt("Fun Result: ~p~n", [FunRes]),
	    Info = (catch Mod:info(Tab)),
	    ?fmt("Table info:~p~n", [Info]),
	    {'EXIT', {hej, ST}} = (catch erlang:error(hej)),
	    ?fmt("Stack backtrace: ~p~n", [ST]),
	    erlang:error(badmatch)
    end.

do_n(0,_) -> ok;
do_n(N,Fun) ->
    Fun(),
    do_n(N-1,Fun).

%%
%% We want some misses too, so pretend the tables are slightly
%% larger than they really are.
%%
num_els(Tab) ->
    16 * table_factor(Tab).


test() ->
    do_return_values(),
    do_test([]).

do_test(Config) ->
    init_random(Config),
    whitebox(),
    lists:foreach(fun(Type) ->
			  Tabs = build_tables(Config,Type),
			  basic_key(Tabs,Type),
			  ?fmt("basic_key done for type ~w~n",[Type]),
			  basic_pb_key(Tabs,Type),
			  ?fmt("basic_pb_key done for type ~w~n",[Type]),
			  double_pb_key(Tabs,Type),
			  ?fmt("double_pb_key done for type ~w~n",[Type]),
			  multi_key(Tabs,Type),
			  ?fmt("multi_key done for type ~w~n",[Type]),
			  multi_mixed_key(Tabs,Type),
			  ?fmt("multi_mixed_key done for type ~w~n",
				    [Type]),
			  destroy_tables(Tabs)
		  end,
		  [tuple, list, complex]),
    ok.
    
basic_key(Tabs,Type) ->
    Fun = fun() ->
		  lists:map(fun(Tab) ->
				    ?line Key = 
					create_random_key(num_els(Tab),Type),
				    ?line  MS = 
					[{{Key,'_','_','_','_'},[],['$_']}],
				    MF = fun({Key0,A,B,F,Bi},Acc) ->
						 case Key =:= Key0 of
						     true ->
							 [{Key0,A,B,F,Bi} | 
							  Acc];
						     _ ->
							 Acc
						 end
					 end,
				    ?line cmp_ms_to_fun(Tab,MS,MF,[])
			    end,
			    Tabs)
	  end,
    ?line do_n(50,Fun),
    ok.
		  
basic_pb_key(Tabs,Type) ->
    InnerFun = fun(Tab) ->
		       ?line {Key,KeyFun}  = 
			   create_pb_key(num_els(Tab),Type),
		       ?line MS = [{{Key,'_','_','_','_'},[],['$_']}],
		       MF = fun({Key0,A,B,F,Bi},Acc) ->
				    case KeyFun(Key0) of
					true ->
					    [{Key0,A,B,F,Bi} | 
					     Acc];
					_ ->
					    Acc
				    end
			    end,
		       ?line cmp_ms_to_fun(Tab,MS,MF,[])
	       end,
    ?line {Etses, Detses} = split_by_type(Tabs),
    
    ?line FunEts = fun() ->
			   ?line lists:foreach(InnerFun,
					       Etses)
		   end,
    ?line FunDets = fun() ->
			    ?line lists:foreach(InnerFun,
						Detses)
		    end,
    ?line do_n(table_factor(hd(Etses)) div 2,FunEts),
    ?line do_n(10,FunDets),
    ok.
		  
double_pb_key(Tabs,Type) ->
    InnerFun = fun(Tab) ->
		       ?line {KeyA,KeyFunA}  = 
			   create_pb_key(num_els(Tab),Type),
		       ?line {KeyB,KeyFunB}  = 
			   create_pb_key(num_els(Tab),Type),
		       MS = [{{KeyA,'_','_','_','_'},[],['$_']},
			     {{KeyB,'_','_','_','_'},[],['$_']}],
		       ?dbgformat("Tab: ~p, MS: ~p~n",
				  [Tab,MS]),
		       MF = fun({Key0,A,B,F,Bi},Acc) ->
				    case KeyFunA(Key0) of
					true ->
					    ?dbgformat
					       ("FunMatched:"
						" ~p~n",
						[{Key0,A,
						  B,F,Bi}]),
					    [{Key0,A,B,F,Bi} | 
					     Acc];
					_ ->
					    case KeyFunB(Key0) of
						true ->
						    ?dbgformat
						       ("Fun"
							"Matched:"
							" ~p~n",
							[{Key0,A,
							  B,F,
							  Bi}]),
						    [{Key0,A,B,
						      F,Bi} |
						     Acc];
						_ ->
						    Acc
					    end
				    end
			    end,
		       ?line cmp_ms_to_fun(Tab,MS,MF,[])
	       end,
    ?line {Etses, Detses} = split_by_type(Tabs),
    
    ?line FunEts = fun() ->
			   ?line lists:foreach(InnerFun,
					       Etses)
		   end,
    ?line FunDets = fun() ->
			    ?line lists:foreach(InnerFun,
						Detses)
		    end,
    ?line do_n(table_factor(hd(Etses)) div 2,FunEts),
    ?line do_n(10,FunDets),
    ok.
		  
	    
multi_key(Tabs,Type) ->
    Fun = fun() ->
		  lists:map(fun(Tab) ->
				    ?line KeyA  = 
					create_random_key(num_els(Tab),Type),
				    ?line KeyB  = 
					create_random_key(num_els(Tab),Type),
				    ?line KeyC  = 
					create_random_key(num_els(Tab),Type),
				    ?line KeyD  = 
					create_random_key(num_els(Tab),Type),
				    ?line KeyE  = 
					create_random_key(num_els(Tab),Type),
				    ?line KeyF  = 
					create_random_key(num_els(Tab),Type),
				    ?line KeyG  = 
					create_random_key(num_els(Tab),Type),
				    ?line KeyH  = 
					create_random_key(num_els(Tab),Type),
				    ?line KeyI  = 
					create_random_key(num_els(Tab),Type),
				    ?line KeyJ  = 
					create_random_key(num_els(Tab),Type),
				    ?line KeyK  = 
					create_random_key(num_els(Tab),Type),
				    ?line KeyL  = 
					create_random_key(num_els(Tab),Type),
				    
				    MS = [{{KeyA,'$1','_','$2','_'},[],
					   [{{'$1','$2'}}]},
					  {{KeyB,'$1','_','$2','_'},[],
					   [{{'$1','$2'}}]},
					  {{KeyC,'$1','_','$2','_'},[],
					   [{{'$1','$2'}}]},
					  {{KeyD,'$1','_','$2','_'},[],
					   [{{'$1','$2'}}]},
					  {{KeyE,'$1','_','$2','_'},[],
					   [{{'$1','$2'}}]},
					  {{KeyF,'$1','_','$2','_'},[],
					   [{{'$1','$2'}}]},
					  {{KeyG,'$1','_','$2','_'},[],
					   [{{'$1','$2'}}]},
					  {{KeyH,'$1','_','$2','_'},[],
					   [{{'$1','$2'}}]},
					  {{KeyI,'$1','_','$2','_'},[],
					   [{{'$1','$2'}}]},
					  {{KeyJ,'$1','_','$2','_'},[],
					   [{{'$1','$2'}}]},
					  {{KeyK,'$1','_','$2','_'},[],
					   [{{'$1','$2'}}]},
					  {{KeyL,'$1','_','$2','_'},[],
					   [{{'$1','$2'}}]}
					 ],
				    ?dbgformat("Tab: ~p, MS: ~p~n",
					      [Tab,MS]),
				    MF = fun({Key0,A,_B,F,_Bi},Acc) ->
						 case Key0 of
						     KeyA ->
							 [ {A,F} | 
							   Acc];
						     KeyB ->
							 [ {A,F} | 
							   Acc];
						     KeyC ->
							 [ {A,F} | 
							   Acc];
						     KeyD ->
							 [ {A,F} | 
							   Acc];
						     KeyE ->
							 [ {A,F} | 
							   Acc];
						     KeyF ->
							 [ {A,F} | 
							   Acc];
						     KeyG ->
							 [ {A,F} | 
							   Acc];
						     KeyH ->
							 [ {A,F} | 
							   Acc];
						     KeyI ->
							 [ {A,F} | 
							   Acc];
						     KeyJ ->
							 [ {A,F} | 
							   Acc];
						     KeyK ->
							 [ {A,F} | 
							   Acc];
						     KeyL ->
							 [ {A,F} | 
							   Acc];
						     _ ->
							 Acc
						 end
					 end,
				    ?line cmp_ms_to_fun(Tab,MS,MF,[])
			    end,
			    Tabs)
	  end,
    ?line do_n(33,Fun),
    ok.
		  
multi_mixed_key(Tabs,Type) ->
    InnerFun = fun(Tab) ->
		       ?line KeyA  = 
			   create_random_key(num_els(Tab),Type),
		       ?line KeyB  = 
			   create_random_key(num_els(Tab),Type),
		       ?line KeyC  = 
			   create_random_key(num_els(Tab),Type),
		       ?line KeyD  = 
			   create_random_key(num_els(Tab),Type),
		       ?line {KeyE, FunE}  = 
			   create_pb_key(num_els(Tab),Type),
		       ?line KeyF  = 
			   create_random_key(num_els(Tab),Type),
		       ?line {KeyG, FunG}  = 
			   create_pb_key(num_els(Tab),Type),
		       ?line KeyH  = 
			   create_random_key(num_els(Tab),Type),
		       ?line KeyI  = 
			   create_random_key(num_els(Tab),Type),
		       ?line {KeyJ, FunJ}  = 
			   create_pb_key(num_els(Tab),Type),
		       ?line KeyK  = 
			   create_random_key(num_els(Tab),Type),
		       ?line KeyL  = 
			   create_random_key(num_els(Tab),Type),
		       
		       MS = [{{KeyA,'$1','_','$2','_'},[],
			      [{{'$1','$2'}}]},
			     {{KeyB,'$1','_','$2','_'},[],
			      [{{'$1','$2'}}]},
			     {{KeyC,'$1','_','$2','_'},[],
			      [{{'$1','$2'}}]},
			     {{KeyD,'$1','_','$2','_'},[],
			      [{{'$1','$2'}}]},
			     {{KeyE,'$100','_','$200','_'},[],
			      [{{'$100','$200'}}]},
			     {{KeyF,'$1','_','$2','_'},[],
			      [{{'$1','$2'}}]},
			     {{KeyG,'$100','_','$200','_'},[],
			      [{{'$100','$200'}}]},
			     {{KeyH,'$1','_','$2','_'},[],
			      [{{'$1','$2'}}]},
			     {{KeyI,'$1','_','$2','_'},[],
			      [{{'$1','$2'}}]},
			     {{KeyJ,'$100','_','$200','_'},[],
			      [{{'$100','$200'}}]},
			     {{KeyK,'$1','_','$2','_'},[],
			      [{{'$1','$2'}}]},
			     {{KeyL,'$1','_','$2','_'},[],
			      [{{'$1','$2'}}]}
			    ],
		       ?dbgformat("Tab: ~p, MS: ~p~n",
				  [Tab,MS]),
		       MF = fun({Key0,A,_B,F,_Bi},Acc) ->
				    case Key0 of
					KeyA ->
					    [ {A,F} | 
					      Acc];
					KeyB ->
					    [ {A,F} | 
					      Acc];
					KeyC ->
					    [ {A,F} | 
					      Acc];
					KeyD ->
					    [ {A,F} | 
					      Acc];
					KeyF ->
					    [ {A,F} | 
					      Acc];
					KeyH ->
					    [ {A,F} | 
					      Acc];
					KeyI ->
					    [ {A,F} | 
					      Acc];
					KeyK ->
					    [ {A,F} | 
					      Acc];
					KeyL ->
					    [ {A,F} | 
					      Acc];
					Else ->
					    case FunE(Else) or 
						FunG(Else) or
						FunJ(Else) of
						true ->
						    [ {A,F} | 
						      Acc]; 
						_ ->
						    Acc
					    end
				    end
			    end,
		       ?line cmp_ms_to_fun(Tab,MS,MF,[]),
		       ?line case Tab of
				 {ets,_} ->
				     ?line cmp_ms_to_fun(Tab,MS,MF,[],1),  
				     ?line cmp_ms_to_fun(Tab,MS,MF,[],10),  
				     ?line cmp_ms_to_fun(Tab,MS,MF,[],1000000),  
				     ?line cmp_ms_to_fun(Tab,MS,MF,[],-1),  
				     ?line cmp_ms_to_fun(Tab,MS,MF,[],-10),  
				     ?line cmp_ms_to_fun(Tab,MS,MF,[],-1000000);  
				 _ ->
				     ok
			     end
	       end,
    ?line {Etses, Detses} = split_by_type(Tabs),
    
    ?line FunEts = fun() ->
			   ?line lists:foreach(InnerFun,
					       Etses)
		   end,
    ?line FunDets = fun() ->
			    ?line lists:foreach(InnerFun,
						Detses)
		    end,
    ?line do_n(table_factor(hd(Etses)) div 2,FunEts),
    ?line do_n(table_factor(hd(Detses)) div 2,FunDets),
    ok.
		  
	    
split_by_type(List) ->    
    split_by_type(List,[],[]).
split_by_type([],AccEts,AccDets) ->
    {AccEts,AccDets};
split_by_type([{dets,Tab}|T],AccEts,AccDets) ->
    split_by_type(T,AccEts,[{dets,Tab}|AccDets]);
split_by_type([{ets,Tab}|T],AccEts,AccDets) ->
    split_by_type(T,[{ets,Tab}|AccEts],AccDets).

whitebox() ->
    ?line ets:new(xxx,[named_table, ordered_set]),
    ?line ets:new(yyy,[named_table]),
    ?line E = fun(0,_)->ok;
		 (N,F) -> 
		      ?line ets:insert(xxx,{N,N rem 10}), 
		      ?line ets:insert(yyy,{N,N rem 10}), 
		      F(N-1,F) 
	      end,
    ?line E(10000,E),                                                          

    ?line G = fun(F,C,A) -> 
		      ?line case ets:select(C) of 
				{L,C2} -> 
				    ?line F(F,C2,A+length(L)); 
				'$end_of_table' -> 
				    ?line A 
			    end 
	      end,
    ?line H=fun({L,C}) -> 
		    ?line G(G,C,length(L)) 
	    end,
    
    ?line 1 = H(ets:select(xxx,[{{'$1','$2'},[{'<','$1',2}],['$_']}],7)),
    ?line 10000 = H(ets:select(xxx,[{{'$1','$2'},[],['$_']}],1)),
    ?line 1 = H(ets:select(yyy,[{{'$1','$2'},[{'<','$1',2}],['$_']}],7)),
    ?line 10000 = H(ets:select(yyy,[{{'$1','$2'},[],['$_']}],1)),

    ?line {[{5,5}],_} = ets:select(xxx,[{{5,'$2'},[],['$_']}],1),
    ?line {[{5,5}],_} = ets:select(yyy,[{{5,'$2'},[],['$_']}],1),

    ?line I = fun(_,0) -> 
		      ok;
		 (I,N) -> 
		      ?line 10000 =  
			  H(ets:select(xxx,[{{'$1','$2'},[],['$_']}],N)), 
		      I(I,N-1) 
	      end,
    ?line I(I,2000),
    ?line J = fun(F,C,A) -> 
		      ?line case ets:select(C) of 
				{L,C2} -> 
				    ?line F(F,C2,lists:reverse(L)++A); 
				'$end_of_table' -> 
				    ?line lists:reverse(A) 
			    end 
	      end,
    ?line K = fun({L,C}) -> 
		      ?line J(J,C,lists:reverse(L)) 
	      end,
    ?line M = fun(_, _, 0) -> 
		      ok;
		 (F, What, N) -> 
		      ?line What =  
			  K(ets:select(xxx,[{{'$1','$2'},[],['$_']}],N)), 
		      F(F, What, N-1) 
	      end,
    ?line N = fun(HM) -> 
		      ?line What = ets:select(xxx,[{{'$1','$2'},[],['$_']}]), 
		      ?line What = lists:sort(What), 
		      M(M, What, HM) 
	      end,
    ?line N(2000),
    ?line ets:delete(xxx),
    ?line ets:delete(yyy).


do_return_values() ->
    ?line T = ets:new(xxx,[ordered_set]),
    ?line U = ets:new(xxx,[]),
    ?line '$end_of_table' = ets:select(T,[{'_',[],['$_']}],1),
    ?line '$end_of_table' = ets:select(U,[{'_',[],['$_']}],1),
    ?line ets:insert(T,{ett,1}),
    ?line ets:insert(U,{ett,1}),
    ?line {[{ett,1}],C1} = ets:select(T,[{'_',[],['$_']}],1),
    ?line '$end_of_table' = ets:select(C1),
    ?line {[{ett,1}],C2} = ets:select(U,[{'_',[],['$_']}],1),
    ?line '$end_of_table' = ets:select(C2),
    ?line {[{ett,1}],C3} = ets:select(T,[{'_',[],['$_']}],2),
    ?line '$end_of_table' = ets:select(C3),
    ?line {[{ett,1}],C4} = ets:select(U,[{'_',[],['$_']}],2),
    ?line '$end_of_table' = ets:select(C4),
    ?line E = fun(0,_)->ok;
		 (N,F) -> 
		      ?line ets:insert(T,{N,N rem 10}), 
		      ?line ets:insert(U,{N,N rem 10}), 
		      F(N-1,F) 
	      end,
    ?line E(10000,E),                                                          
    ?line '$end_of_table' = ets:select(T,[{{hej, hopp},[],['$_']}],1),
    ?line '$end_of_table' = ets:select(U,[{{hej,hopp},[],['$_']}],1),
    ?line {[{ett,1}],CC1} = ets:select(T,[{{'$1','_'},[{is_atom, '$1'}],
					  ['$_']}],1),
    ?line '$end_of_table' = ets:select(CC1),
    ?line {[{ett,1}],CC2} = ets:select(U,[{{'$1','_'},[{is_atom, '$1'}],
					  ['$_']}],1),
    ?line '$end_of_table' = ets:select(CC2),
    ?line {[{ett,1}],CC3} = ets:select(T,[{{'$1','_'},[{is_atom, '$1'}],
					  ['$_']}],2),
    ?line '$end_of_table' = ets:select(CC3),
    ?line {[{ett,1}],CC4} = ets:select(U,[{{'$1','_'},[{is_atom, '$1'}],
					  ['$_']}],2),
    ?line '$end_of_table' = ets:select(CC4),
    ?line ets:delete(T),
    ?line ets:delete(U),
    ?line V = ets:new(xxx,[{keypos, 4}]),
    ?line X = ets:new(xxx,[ordered_set, {keypos, 4}]),
    ?line ets:insert(V,{1,1,1,ett}),
    ?line ets:insert(X,{1,1,1,ett}),
    ?line '$end_of_table' = ets:select(V,[{{1,1,1},[],['$_']}],1),
    ?line '$end_of_table' = ets:select(X,[{{1,1,1},[],['$_']}],1),
    ?line ets:delete(V),
    ?line ets:delete(X),
    ok.
    
    



