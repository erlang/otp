%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2013. All Rights Reserved.
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
-module(map_SUITE).
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	init_per_group/2,end_per_group/2
    ]).

-export([
	t_build_and_match_literals/1,
	t_update_literals/1,t_match_and_update_literals/1,
	t_update_map_expressions/1,
	t_update_assoc/1,t_update_exact/1,
	t_guard_bifs/1, t_guard_sequence/1, t_guard_update/1,
	t_guard_receive/1, t_guard_fun/1,
	t_list_comprehension/1,
	t_map_sort_literals/1,
	t_map_equal/1,
	t_map_compare/1,
	t_map_size/1,

	%% Specific Map BIFs
	t_bif_map_get/1,
	t_bif_map_find/1,
	t_bif_map_is_key/1,
	t_bif_map_keys/1,
	t_bif_map_merge/1,
	t_bif_map_new/1,
	t_bif_map_put/1,
	t_bif_map_remove/1,
	t_bif_map_update/1,
	t_bif_map_values/1,
	t_bif_map_to_list/1,
	t_bif_map_from_list/1,

	%% erlang
	t_erlang_hash/1,
	t_map_encode_decode/1,

	%% non specific BIF related
	t_bif_build_and_check/1,
	t_bif_merge_and_check/1,

	%% maps module not bifs
	t_maps_fold/1,
	t_maps_map/1,
	t_maps_size/1,
	t_maps_without/1,

	%% misc
	t_pdict/1,
	t_ets/1,
	t_dets/1,
	t_tracing/1
    ]).

-include_lib("stdlib/include/ms_transform.hrl").

-define(CHECK(Cond,Term),
	case (catch (Cond)) of
	    true -> true;
	    _ -> io:format("###### CHECK FAILED ######~nINPUT:  ~p~n", [Term]),
		 exit(Term)
	end).

suite() -> [].

all() -> [
	t_build_and_match_literals,
	t_update_literals, t_match_and_update_literals,
	t_update_map_expressions,
	t_update_assoc,t_update_exact,
	t_guard_bifs, t_guard_sequence, t_guard_update,
	t_guard_receive,t_guard_fun, t_list_comprehension,
	t_map_equal, t_map_compare,
	t_map_sort_literals,

	%% Specific Map BIFs
	t_bif_map_get,t_bif_map_find,t_bif_map_is_key,
	t_bif_map_keys, t_bif_map_merge, t_bif_map_new,
	t_bif_map_put,
	t_bif_map_remove, t_bif_map_update,
	t_bif_map_values,
	t_bif_map_to_list, t_bif_map_from_list,

	%% erlang
	t_erlang_hash, t_map_encode_decode,
	t_map_size,

	%% non specific BIF related
	t_bif_build_and_check,
	t_bif_merge_and_check,

	%% maps module
	t_maps_fold, t_maps_map,
	t_maps_size, t_maps_without,


        %% Other functions
	t_pdict,
	t_ets,
	t_tracing
    ].

groups() -> [].

init_per_suite(Config) -> Config.
end_per_suite(_Config) -> ok.

init_per_group(_GroupName, Config) -> Config.
end_per_group(_GroupName, Config) -> Config.

%% tests

t_build_and_match_literals(Config) when is_list(Config) ->
    #{} = id(#{}),
    #{1:=a} = id(#{1=>a}),
    #{1:=a,2:=b} = id(#{1=>a,2=>b}),
    #{1:=a,2:=b,3:="c"} = id(#{1=>a,2=>b,3=>"c"}),
    #{1:=a,2:=b,3:="c","4":="d"} = id(#{1=>a,2=>b,3=>"c","4"=>"d"}),
    #{1:=a,2:=b,3:="c","4":="d",<<"5">>:=<<"e">>} =
	id(#{1=>a,2=>b,3=>"c","4"=>"d",<<"5">>=><<"e">>}),
    #{1:=a,2:=b,3:="c","4":="d",<<"5">>:=<<"e">>,{"6",7}:="f"} =
	id(#{1=>a,2=>b,3=>"c","4"=>"d",<<"5">>=><<"e">>,{"6",7}=>"f"}),
    #{1:=a,2:=b,3:="c","4":="d",<<"5">>:=<<"e">>,{"6",7}:="f",8:=g} =
	id(#{1=>a,2=>b,3=>"c","4"=>"d",<<"5">>=><<"e">>,{"6",7}=>"f",8=>g}),

    #{<<"hi all">> := 1} = id(#{<<"hi",32,"all">> => 1}),

    #{a:=X,a:=X=3,b:=4} = id(#{a=>3,b=>4}), % weird but ok =)

    #{ a:=#{ b:=#{c := third, b:=second}}, b:=first} =
	id(#{ b=>first, a=>#{ b=>#{c => third, b=> second}}}),

    M = #{ map_1=>#{ map_2=>#{value_3 => third}, value_2=> second}, value_1=>first},
    M = #{ map_1:=#{ map_2:=#{value_3 := third}, value_2:= second}, value_1:=first} =
	 id(#{ map_1=>#{ map_2=>#{value_3 => third}, value_2=> second}, value_1=>first}),

    %% error case
    %V = 32,
    %{'EXIT',{{badmatch,_},_}} = (catch (#{<<"hi all">> => 1} = id(#{<<"hi",V,"all">> => 1}))),
    {'EXIT',{{badmatch,_},_}} = (catch (#{x:=3,x:=2} = id(#{x=>3}))),
    {'EXIT',{{badmatch,_},_}} = (catch (#{x:=2} = id(#{x=>3}))),
    {'EXIT',{{badmatch,_},_}} = (catch (#{x:=3} = id({a,b,c}))),
    {'EXIT',{{badmatch,_},_}} = (catch (#{x:=3} = id(#{y=>3}))),
    {'EXIT',{{badmatch,_},_}} = (catch (#{x:=3} = id(#{x=>"three"}))),
    ok.


t_map_size(Config) when is_list(Config) ->
    0 = map_size(id(#{})),
    1 = map_size(id(#{a=>1})),
    1 = map_size(id(#{a=>"wat"})),
    2 = map_size(id(#{a=>1, b=>2})),
    3 = map_size(id(#{a=>1, b=>2, b=>"3","33"=><<"n">>})),

    true = map_is_size(#{a=>1}, 1),
    true = map_is_size(#{a=>1, a=>2}, 1),
    M = #{ "a" => 1, "b" => 2},
    true  = map_is_size(M, 2),
    false = map_is_size(M, 3),
    true  = map_is_size(M#{ "a" => 2}, 2),
    false = map_is_size(M#{ "c" => 2}, 2),

    Ks = [build_key(fun(K) -> <<1,K:32,1>> end,I)||I<-lists:seq(1,100)],
    ok = build_and_check_size(Ks,0,#{}),

    %% Error cases.
    {'EXIT',{badarg,_}} = (catch map_size([])),
    {'EXIT',{badarg,_}} = (catch map_size(<<1,2,3>>)),
    {'EXIT',{badarg,_}} = (catch map_size(1)),
    ok.

build_and_check_size([K|Ks],N,M0) ->
    N = map_size(M0),
    M1 = M0#{ K => K },
    build_and_check_size(Ks,N + 1,M1);
build_and_check_size([],N,M) ->
    N = map_size(M),
    ok.

map_is_size(M,N) when map_size(M) =:= N -> true;
map_is_size(_,_) -> false.

% test map updates without matching
t_update_literals(Config) when is_list(Config) ->
    Map = #{x=>1,y=>2,z=>3,q=>4},
    #{x:="d",q:="4"} = loop_update_literals_x_q(Map, [
		{"a","1"},{"b","2"},{"c","3"},{"d","4"}
	]),
    ok.

loop_update_literals_x_q(Map, []) -> Map;
loop_update_literals_x_q(Map, [{X,Q}|Vs]) ->
    loop_update_literals_x_q(Map#{q=>Q,x=>X},Vs).

% test map updates with matching
t_match_and_update_literals(Config) when is_list(Config) ->
    Map = #{x=>0,y=>"untouched",z=>"also untouched",q=>1},
    #{x:=16,q:=21,y:="untouched",z:="also untouched"} = loop_match_and_update_literals_x_q(Map, [
	    {1,2},{3,4},{5,6},{7,8}
	]),
    M0 = id(#{ "hi" => "hello", int => 3, <<"key">> => <<"value">>,
	    4 => number, 18446744073709551629 => wat}),
    M1 = id(#{}),
    M2 = M1#{ "hi" => "hello", int => 3, <<"key">> => <<"value">>,
	4 => number, 18446744073709551629 => wat},
    M0 = M2,

    #{ 4 := another_number, int := 3 } = M2#{ 4 => another_number },
    ok.

loop_match_and_update_literals_x_q(Map, []) -> Map;
loop_match_and_update_literals_x_q(#{q:=Q0,x:=X0} = Map, [{X,Q}|Vs]) ->
    loop_match_and_update_literals_x_q(Map#{q=>Q0+Q,x=>X0+X},Vs).


t_update_map_expressions(Config) when is_list(Config) ->
    M = maps:new(),
    #{ a := 1 } = M#{a => 1},

    #{ b := 2 } = (maps:new())#{ b => 2 },

    #{ a :=42, b:=42, c:=42 } = (maps:from_list([{a,1},{b,2},{c,3}]))#{ a := 42, b := 42, c := 42 },
    #{ "a" :=1, "b":=42, "c":=42 } = (maps:from_list([{"a",1},{"b",2}]))#{ "b" := 42, "c" => 42 },

    %% Error cases, FIXME: should be 'badmap'?
    {'EXIT',{badarg,_}} = (catch (id(<<>>))#{ a := 42, b => 2 }),
    {'EXIT',{badarg,_}} = (catch (id([]))#{ a := 42, b => 2 }),
    ok.


t_update_assoc(Config) when is_list(Config) ->
    M0 = id(#{1=>a,2=>b,3.0=>c,4=>d,5=>e}),

    M1 = M0#{1=>42,2=>100,4=>[a,b,c]},
    #{1:=42,2:=100,3.0:=c,4:=[a,b,c],5:=e} = M1,
    #{1:=42,2:=b,4:=d,5:=e,2.0:=100,3.0:=c,4.0:=[a,b,c]} = M0#{1.0=>float,1:=42,2.0=>wrong,2.0=>100,4.0=>[a,b,c]},

    M2 = M0#{3.0=>new},
    #{1:=a,2:=b,3.0:=new,4:=d,5:=e} = M2,
    M2 = M0#{3.0:=wrong,3.0=>new},

    %% Errors cases.
    BadMap = id(badmap),
    {'EXIT',{badarg,_}} = (catch BadMap#{nonexisting=>val}),

    ok.

t_update_exact(Config) when is_list(Config) ->
    M0 = id(#{1=>a,2=>b,3.0=>c,4=>d,5=>e}),

    M1 = M0#{1:=42,2:=100,4:=[a,b,c]},
    #{1:=42,2:=100,3.0:=c,4:=[a,b,c],5:=e} = M1,
    M1 = M0#{1:=wrong,1=>42,2=>wrong,2:=100,4:=[a,b,c]},

    M2 = M0#{3.0:=new},
    #{1:=a,2:=b,3.0:=new,4:=d,5:=e} = M2,
    M2 = M0#{3.0=>wrong,3.0:=new},
    true = M2 =/= M0#{3=>right,3.0:=new},
    #{ 3 := right, 3.0 := new } = M0#{3=>right,3.0:=new},

    M3 = id(#{ 1 => val}),
    #{1 := update2,1.0 := new_val4} = M3#{
	1.0 => new_val1, 1 := update, 1=> update3,
	1 := update2, 1.0 := new_val2, 1.0 => new_val3,
	1.0 => new_val4 },


    %% Errors cases.
    {'EXIT',{badarg,_}} = (catch M0#{nonexisting:=val}),
    {'EXIT',{badarg,_}} = (catch M0#{1.0:=v,1.0=>v2}),
    {'EXIT',{badarg,_}} = (catch M0#{42.0:=v,42:=v2}),
    {'EXIT',{badarg,_}} = (catch M0#{42=>v1,42.0:=v2,42:=v3}),

    ok.

t_guard_bifs(Config) when is_list(Config) ->
    true   = map_guard_head(#{a=>1}),
    false  = map_guard_head([]),
    true   = map_guard_body(#{a=>1}),
    false  = map_guard_body({}),
    true   = map_guard_pattern(#{a=>1, <<"hi">> => "hi" }),
    false  = map_guard_pattern("list"),
    ok.

map_guard_head(M) when is_map(M) -> true;
map_guard_head(_) -> false.

map_guard_body(M) -> is_map(M).

map_guard_pattern(#{}) -> true;
map_guard_pattern(_)   -> false.

t_guard_sequence(Config) when is_list(Config) ->
	{1, "a"} = map_guard_sequence_1(#{seq=>1,val=>id("a")}),
	{2, "b"} = map_guard_sequence_1(#{seq=>2,val=>id("b")}),
	{3, "c"} = map_guard_sequence_1(#{seq=>3,val=>id("c")}),
	{4, "d"} = map_guard_sequence_1(#{seq=>4,val=>id("d")}),
	{5, "e"} = map_guard_sequence_1(#{seq=>5,val=>id("e")}),

	{1,M1}       = map_guard_sequence_2(M1 = id(#{a=>3})),
	{2,M2}       = map_guard_sequence_2(M2 = id(#{a=>4, b=>4})),
	{3,gg,M3}    = map_guard_sequence_2(M3 = id(#{a=>gg, b=>4})),
	{4,sc,sc,M4} = map_guard_sequence_2(M4 = id(#{a=>sc, b=>3, c=>sc2})),
	{5,kk,kk,M5} = map_guard_sequence_2(M5 = id(#{a=>kk, b=>other, c=>sc2})),
	
	%% error case
	{'EXIT',{function_clause,_}} = (catch map_guard_sequence_1(#{seq=>6,val=>id("e")})),
	{'EXIT',{function_clause,_}} = (catch map_guard_sequence_2(#{b=>5})),
	ok.

map_guard_sequence_1(#{seq:=1=Seq, val:=Val}) -> {Seq,Val};
map_guard_sequence_1(#{seq:=2=Seq, val:=Val}) -> {Seq,Val};
map_guard_sequence_1(#{seq:=3=Seq, val:=Val}) -> {Seq,Val};
map_guard_sequence_1(#{seq:=4=Seq, val:=Val}) -> {Seq,Val};
map_guard_sequence_1(#{seq:=5=Seq, val:=Val}) -> {Seq,Val}.

map_guard_sequence_2(#{ a:=3 }=M) -> {1, M};
map_guard_sequence_2(#{ a:=4 }=M) -> {2, M};
map_guard_sequence_2(#{ a:=X, a:=X, b:=4 }=M) -> {3,X,M};
map_guard_sequence_2(#{ a:=X, a:=Y, b:=3 }=M) when X =:= Y -> {4,X,Y,M};
map_guard_sequence_2(#{ a:=X, a:=Y }=M) when X =:= Y -> {5,X,Y,M}.


t_guard_update(Config) when is_list(Config) ->
    error  = map_guard_update(#{},#{}),
    first  = map_guard_update(#{}, #{x=>first}),
    second = map_guard_update(#{y=>old}, #{x=>second,y=>old}),
    ok.

map_guard_update(M1, M2) when M1#{x=>first}  =:= M2 -> first;
map_guard_update(M1, M2) when M1#{x=>second} =:= M2 -> second;
map_guard_update(_, _) -> error.

t_guard_receive(Config) when is_list(Config) ->
    M0  = #{ id => 0 },
    Pid = spawn_link(fun() -> guard_receive_loop() end),
    Big = 36893488147419103229,
    B1  = <<"some text">>,
    B2  = <<"was appended">>,
    B3  = <<B1/binary, B2/binary>>,

    #{id:=1, res:=Big} = M1 = call(Pid, M0#{op=>sub,in=>{1 bsl 65, 3}}),
    #{id:=2, res:=26}  = M2 = call(Pid, M1#{op=>idiv,in=>{53,2}}),
    #{id:=3, res:=832} = M3 = call(Pid, M2#{op=>imul,in=>{26,32}}),
    #{id:=4, res:=4}   = M4 = call(Pid, M3#{op=>add,in=>{1,3}}),
    #{id:=5, res:=Big} = M5 = call(Pid, M4#{op=>sub,in=>{1 bsl 65, 3}}),
    #{id:=6, res:=B3}  = M6 = call(Pid, M5#{op=>"append",in=>{B1,B2}}),
    #{id:=7, res:=4}   = _  = call(Pid, M6#{op=>add,in=>{1,3}}),


    %% update old maps and check id update
    #{id:=2, res:=B3} = call(Pid, M1#{op=>"append",in=>{B1,B2}}),
    #{id:=5, res:=99} = call(Pid, M4#{op=>add,in=>{33, 66}}),

    %% cleanup
    done = call(Pid, done),
    ok.

call(Pid, M) ->
    Pid ! {self(), M}, receive {Pid, Res} -> Res end.

guard_receive_loop() ->
    receive
	{Pid, #{ id:=Id, op:="append", in:={X,Y}}=M} when is_binary(X), is_binary(Y) ->
	    Pid ! {self(), M#{ id=>Id+1, res=><<X/binary,Y/binary>>}},
	    guard_receive_loop();
	{Pid, #{ id:=Id, op:=add, in:={X,Y}}} ->
	    Pid ! {self(), #{ id=>Id+1, res=>X+Y}},
	    guard_receive_loop();
	{Pid, #{ id:=Id, op:=sub,  in:={X,Y}}=M} ->
	    Pid ! {self(), M#{ id=>Id+1, res=>X-Y}},
	    guard_receive_loop();
	{Pid, #{ id:=Id, op:=idiv, in:={X,Y}}=M} ->
	    Pid ! {self(), M#{ id=>Id+1, res=>X div Y}},
	    guard_receive_loop();
	{Pid, #{ id:=Id, op:=imul, in:={X,Y}}=M} ->
	    Pid ! {self(), M#{ id=>Id+1, res=>X * Y}},
	    guard_receive_loop();
	{Pid, done} ->
	    Pid ! {self(), done};
	{Pid, Other} ->
	    Pid ! {error, Other},
	    guard_receive_loop()
    end.


t_list_comprehension(Config) when is_list(Config) ->
    [#{k:=1},#{k:=2},#{k:=3}] = [#{k=>I} || I <- [1,2,3]],
    ok.

t_guard_fun(Config) when is_list(Config) ->
    F1 = fun
	    (#{s:=v,v:=V})     -> {v,V};
	    (#{s:=t,v:={V,V}}) -> {t,V};
	    (#{s:=l,v:=[V,V]}) -> {l,V}
    end,
    
    F2 = fun
	    (#{s:=T,v:={V,V}}) -> {T,V};
	    (#{s:=T,v:=[V,V]}) -> {T,V};
	    (#{s:=T,v:=V})     -> {T,V}
    end,
    V = <<"hi">>,

    {v,V} = F1(#{s=>v,v=>V}),
    {t,V} = F1(#{s=>t,v=>{V,V}}),
    {l,V} = F1(#{s=>l,v=>[V,V]}),

    {v,V} = F2(#{s=>v,v=>V}),
    {t,V} = F2(#{s=>t,v=>{V,V}}),
    {l,V} = F2(#{s=>l,v=>[V,V]}),

    %% error case
    {'EXIT', {function_clause,[{?MODULE,_,[#{s:=none,v:=none}],_}|_]}} = (catch F1(#{s=>none,v=>none})),
    ok.


t_map_sort_literals(Config) when is_list(Config) ->
    % test relation

    %% size order
    true  = #{ a => 1, b => 2} < id(#{ a => 1, b => 1, c => 1}),
    true  = #{ b => 1, a => 1} < id(#{ c => 1, a => 1, b => 1}),
    false = #{ c => 1, b => 1, a => 1} < id(#{ c => 1, a => 1}),

    %% key order
    true  = #{ a => 1 } < id(#{ b => 1}),
    false = #{ b => 1 } < id(#{ a => 1}),
    true  = #{ a => 1, b => 1, c => 1 } < id(#{ b => 1, c => 1, d => 1}),
    true  = #{ b => 1, c => 1, d => 1 } > id(#{ a => 1, b => 1, c => 1}),
    true  = #{ c => 1, b => 1, a => 1 } < id(#{ b => 1, c => 1, d => 1}),
    true  = #{ "a" => 1 } < id(#{ <<"a">> => 1}),
    false = #{ <<"a">> => 1 } < id(#{ "a" => 1}),
    true = #{ 1 => 1 } < id(#{ 1.0 => 1}),
    false = #{ 1.0 => 1 } < id(#{ 1 => 1}),

    %% value order
    true  = #{ a => 1 } < id(#{ a => 2}),
    false = #{ a => 2 } < id(#{ a => 1}),
    false = #{ a => 2, b => 1 } < id(#{ a => 1, b => 3}),
    true  = #{ a => 1, b => 1 } < id(#{ a => 1, b => 3}),
    false = #{ a => 1 } < id(#{ a => 1.0}),
    false = #{ a => 1.0 } < id(#{ a => 1}),

    true  = #{ "a" => "hi", b => 134 } == id(#{ b => 134,"a" => "hi"}),

    %% lists:sort

    SortVs = [#{"a"=>1},#{a=>2},#{1=>3},#{<<"a">>=>4}],
    [#{1:=ok},#{a:=ok},#{"a":=ok},#{<<"a">>:=ok}] = lists:sort([#{"a"=>ok},#{a=>ok},#{1=>ok},#{<<"a">>=>ok}]),
    [#{1:=3},#{a:=2},#{"a":=1},#{<<"a">>:=4}] = lists:sort(SortVs),
    [#{1:=3},#{a:=2},#{"a":=1},#{<<"a">>:=4}] = lists:sort(lists:reverse(SortVs)),

    ok.

t_map_equal(Config) when is_list(Config) ->
    true  = id(#{}) =:= id(#{}),
    false = id(#{}) =:= id(#{a=>1}),
    false = id(#{a=>1}) =:= id(#{}),
    true  = id(#{ "a" => "hi", b => 134 }) =:= id(#{ b => 134,"a" => "hi"}),

    false = id(#{ a => 1 }) =:= id(#{ a => 2}),
    false = id(#{ a => 2 }) =:= id(#{ a => 1}),
    false = id(#{ a => 2, b => 1 }) =:= id(#{ a => 1, b => 3}),
    false = id(#{ a => 1, b => 1 }) =:= id(#{ a => 1, b => 3}),

    true = id(#{ a => 1 }) =:= id(#{ a => 1}),
    true = id(#{ "a" => 2 }) =:= id(#{ "a" => 2}),
    true = id(#{ "a" => 2, b => 3 }) =:= id(#{ "a" => 2, b => 3}),
    true = id(#{ a => 1, b => 3, c => <<"wat">> }) =:= id(#{ a => 1, b => 3, c=><<"wat">>}),
    ok.


t_map_compare(Config) when is_list(Config) ->
    Seed = erlang:now(),
    io:format("seed = ~p\n", [Seed]),
    random:seed(Seed),
    repeat(100, fun(_) -> float_int_compare() end, []),
    repeat(100, fun(_) -> recursive_compare() end, []),
    ok.

float_int_compare() ->
    Terms = numeric_keys(3),
    %%io:format("Keys to use: ~p\n", [Terms]),
    Pairs = lists:map(fun(K) -> list_to_tuple([{K,V} || V <- Terms]) end, Terms),
    lists:foreach(fun(Size) ->
			  MapGen = fun() -> map_gen(list_to_tuple(Pairs), Size) end,
			  repeat(100, fun do_compare/1, [MapGen, MapGen])
		  end,
		  lists:seq(1,length(Terms))),
    ok.

numeric_keys(N) ->
    lists:foldl(fun(_,Acc) ->
			Int = random:uniform(N*4) - N*2,
			Float = float(Int),
			[Int, Float, Float * 0.99, Float * 1.01 | Acc]
		end,
		[],
		lists:seq(1,N)).


repeat(0, _, _) ->
    ok;
repeat(N, Fun, Arg) ->
    Fun(Arg),
    repeat(N-1, Fun, Arg).

copy_term(T) ->
    Papa = self(),
    P = spawn_link(fun() -> receive Msg -> Papa ! Msg end end),
    P ! T,
    receive R -> R end.

do_compare([Gen1, Gen2]) ->
    M1 = Gen1(),
    M2 = Gen2(),
    %%io:format("Maps to compare: ~p AND ~p\n", [M1, M2]),
    C = (M1 < M2),
    Erlang = maps_lessthan(M1, M2),
    C = Erlang,
    ?CHECK(M1==M1, M1),

    %% Change one key from int to float (or vice versa) and check compare
    ML1 = maps:to_list(M1),
    {K1,V1} = lists:nth(random:uniform(length(ML1)), ML1),
    case K1 of
	I when is_integer(I) ->
	    case maps:find(float(I),M1) of
		error ->
		    M1f = maps:remove(I, maps:put(float(I), V1, M1)),
		    ?CHECK(M1f > M1, [M1f, M1]);
		_ -> ok
	    end;

	F when is_float(F), round(F) == F ->
	    case maps:find(round(F),M1) of
		error ->
		    M1i = maps:remove(F, maps:put(round(F), V1, M1)),
		    ?CHECK(M1i < M1, [M1i, M1]);
		_ -> ok
	    end;

	_ -> ok   % skip floats with decimals
    end,

    ?CHECK(M2 == M2, [M2]).


maps_lessthan(M1, M2) ->
  case {maps:size(M1),maps:size(M2)} of
      {_S,_S} ->
	  {K1,V1} = lists:unzip(term_sort(maps:to_list(M1))),
	  {K2,V2} = lists:unzip(term_sort(maps:to_list(M2))),

	  case erts_internal:cmp_term(K1,K2) of
	      -1 -> true;
	      0 -> (V1 < V2);
	      1 -> false
	  end;

      {S1, S2} ->
	  S1 < S2
  end.

term_sort(L) ->
    lists:sort(fun(A,B) -> erts_internal:cmp_term(A,B) =< 0 end,
	       L).


cmp(T1, T2, Exact) when is_tuple(T1) and is_tuple(T2) ->
    case {size(T1),size(T2)} of
	{_S,_S} -> cmp(tuple_to_list(T1), tuple_to_list(T2), Exact);
	{S1,S2} when S1 < S2 -> -1;
	{S1,S2} when S1 > S2 -> 1
    end;

cmp([H1|T1], [H2|T2], Exact) ->
    case cmp(H1,H2, Exact) of
	0 -> cmp(T1,T2, Exact);
	C -> C
    end;

cmp(M1, M2, Exact) when is_map(M1) andalso is_map(M2) ->
    cmp_maps(M1,M2,Exact);
cmp(M1, M2, Exact) ->
    cmp_others(M1, M2, Exact).

cmp_maps(M1, M2, Exact) ->
    case {maps:size(M1),maps:size(M2)} of
	{_S,_S} ->
	    {K1,V1} = lists:unzip(term_sort(maps:to_list(M1))),
	    {K2,V2} = lists:unzip(term_sort(maps:to_list(M2))),

	    case cmp(K1, K2, true) of
		0 -> cmp(V1, V2, Exact);
		C -> C
	    end;

	{S1,S2} when S1 < S2 -> -1;
	{S1,S2} when S1 > S2 -> 1
    end.

cmp_others(I, F, true) when is_integer(I), is_float(F) ->
    -1;
cmp_others(F, I, true) when is_float(F), is_integer(I) ->
    1;
cmp_others(T1, T2, _) ->
    case {T1<T2, T1==T2} of
	{true,false} -> -1;
	{false,true} -> 0;
	{false,false} -> 1
    end.

map_gen(Pairs, Size) ->
    {_,L} = lists:foldl(fun(_, {Keys, Acc}) ->
				KI = random:uniform(size(Keys)),
				K = element(KI,Keys),
				KV = element(random:uniform(size(K)), K),
				{erlang:delete_element(KI,Keys), [KV | Acc]}
			end,
			{Pairs, []},
			lists:seq(1,Size)),

    maps:from_list(L).


recursive_compare() ->
    Leafs = {atom, 17, 16.9, 17.1, [], self(), spawn(fun() -> ok end), make_ref(), make_ref()},
    {A, B} = term_gen_recursive(Leafs, 0, 0),
    %%io:format("Recursive term A = ~p\n", [A]),
    %%io:format("Recursive term B = ~p\n", [B]),

    ?CHECK({true,false} =:=  case do_cmp(A, B, false) of
				 -1 -> {A<B, A>=B};
				 0 -> {A==B, A/=B};
				 1 -> {A>B, A=<B}
			     end,
	   {A,B}),
    A2 = copy_term(A),
    ?CHECK(A == A2, {A,A2}),
    ?CHECK(0 =:= cmp(A, A2, false), {A,A2}),

    B2 = copy_term(B),
    ?CHECK(B == B2, {B,B2}),
    ?CHECK(0 =:= cmp(B, B2, false), {B,B2}),
    ok.

do_cmp(A, B, Exact) ->
    C = cmp(A, B, Exact),
    C.

%% Generate two terms {A,B} that may only differ
%% at float vs integer types.
term_gen_recursive(Leafs, Flags, Depth) ->
    MaxDepth = 10,
    Rnd = case {Flags, Depth} of
	      {_, MaxDepth} -> % Only leafs
		  random:uniform(size(Leafs)) + 3;
	      {0, 0} ->        % Only containers
		  random:uniform(3);
	      {0,_} ->         % Anything
		  random:uniform(size(Leafs)+3)
	  end,
    case Rnd of
	1 -> % Make map
	    Size = random:uniform(size(Leafs)),
	    lists:foldl(fun(_, {Acc1,Acc2}) ->
				{K1,K2} = term_gen_recursive(Leafs, Flags,
							     Depth+1),
				{V1,V2} = term_gen_recursive(Leafs, Flags, Depth+1),
				%%ok = check_keys(K1,K2, 0),
				{maps:put(K1,V1, Acc1), maps:put(K2,V2, Acc2)}
			end,
			{maps:new(), maps:new()},
			lists:seq(1,Size));
	2 -> % Make cons
	    {Car1,Car2} = term_gen_recursive(Leafs, Flags, Depth+1),
	    {Cdr1,Cdr2} = term_gen_recursive(Leafs, Flags, Depth+1),
	    {[Car1 | Cdr1], [Car2 | Cdr2]};
	3 -> % Make tuple
	    Size = random:uniform(size(Leafs)),
	    L = lists:map(fun(_) -> term_gen_recursive(Leafs, Flags, Depth+1) end,
			  lists:seq(1,Size)),
	    {L1, L2} = lists:unzip(L),
	    {list_to_tuple(L1), list_to_tuple(L2)};

	N -> % Make leaf
	    case element(N-3, Leafs) of
		I when is_integer(I) ->
		    case random:uniform(4) of
			1 -> {I, float(I)};
			2 -> {float(I), I};
			_ -> {I,I}
		    end;
		T -> {T,T}
	    end
    end.

check_keys(K1, K2, _) when K1 =:= K2 ->
    case erlang:phash3(K1) =:= erlang:phash3(K2) of
	true -> ok;
	false ->
	    io:format("Same keys with different hash values !!!\nK1 = ~p\nK2 = ~p\n", [K1,K2]),
	    error
    end;
check_keys(K1, K2, 0) ->
    case {erlang:phash3(K1), erlang:phash3(K2)} of
	{H,H} -> check_keys(K1, K2, 1);
	{_,_} -> ok
    end;
check_keys(K1, K2, L) when L < 10 ->
    case {erlang:phash3([L|K1]), erlang:phash3([L|K2])} of
	{H,H} -> check_keys(K1, K2, L+1);
	{_,_} -> ok
    end;
check_keys(K1, K2, L) ->
    io:format("Same hash value at level ~p !!!\nK1 = ~p\nK2 = ~p\n", [L,K1,K2]),
    error.

%% BIFs
t_bif_map_get(Config) when is_list(Config) ->
    %% small map
    1    = maps:get(a, #{ a=> 1}),
    2    = maps:get(b, #{ a=> 1, b => 2}),
    "hi" = maps:get("hello", #{ a=>1, "hello" => "hi"}),
    "tuple hi" = maps:get({1,1.0}, #{ a=>a, {1,1.0} => "tuple hi"}),

    M0    = id(#{ k1=>"v1", <<"k2">> => <<"v3">> }),
    "v4" = maps:get(<<"k2">>, M0#{<<"k2">> => "v4"}),

    %% large map
    M1   = maps:from_list([{I,I}||I<-lists:seq(1,100)] ++
			  [{a,1},{b,2},{"hello","hi"},{{1,1.0},"tuple hi"},
			   {k1,"v1"},{<<"k2">>,"v3"}]),
    1    = maps:get(a, M1),
    2    = maps:get(b, M1),
    "hi" = maps:get("hello", M1),
    "tuple hi" = maps:get({1,1.0}, M1),
    "v3" = maps:get(<<"k2">>, M1),

    %% error case
    {'EXIT',{badarg, [{maps,get,_,_}|_]}} = (catch maps:get(a,[])),
    {'EXIT',{badarg, [{maps,get,_,_}|_]}} = (catch maps:get(a,<<>>)),
    {'EXIT',{bad_key,[{maps,get,_,_}|_]}} = (catch maps:get({1,1}, #{{1,1.0} => "tuple"})),
    {'EXIT',{bad_key,[{maps,get,_,_}|_]}} = (catch maps:get(a,#{})),
    {'EXIT',{bad_key,[{maps,get,_,_}|_]}} = (catch maps:get(a,#{b=>1, c=>2})),
    ok.

t_bif_map_find(Config) when is_list(Config) ->
    %% small map
    {ok, 1}     = maps:find(a, #{ a=> 1}),
    {ok, 2}     = maps:find(b, #{ a=> 1, b => 2}),
    {ok, "int"} = maps:find(1, #{ 1   => "int"}),
    {ok, "float"} = maps:find(1.0, #{ 1.0=> "float"}),

    {ok, "hi"} = maps:find("hello", #{ a=>1, "hello" => "hi"}),
    {ok, "tuple hi"} = maps:find({1,1.0}, #{ a=>a, {1,1.0} => "tuple hi"}),

    M0 = id(#{ k1=>"v1", <<"k2">> => <<"v3">> }),
    {ok, "v4"} = maps:find(<<"k2">>, M0#{ <<"k2">> => "v4" }),

    %% large map
    M1   = maps:from_list([{I,I}||I<-lists:seq(1,100)] ++
			  [{a,1},{b,2},{"hello","hi"},{{1,1.0},"tuple hi"},
			   {k1,"v1"},{<<"k2">>,"v3"}]),
    {ok, 1}    = maps:find(a, M1),
    {ok, 2}    = maps:find(b, M1),
    {ok, "hi"} = maps:find("hello", M1),
    {ok, "tuple hi"} = maps:find({1,1.0}, M1),
    {ok, "v3"} = maps:find(<<"k2">>, M1),

    %% error case
    error = maps:find(a,#{}),
    error = maps:find(a,#{b=>1, c=>2}),
    error = maps:find(1.0, #{ 1 => "int"}),
    error = maps:find(1, #{ 1.0  => "float"}),
    error = maps:find({1.0,1}, #{ a=>a, {1,1.0} => "tuple hi"}), % reverse types in tuple key

    {'EXIT',{badarg,[{maps,find,_,_}|_]}} = (catch maps:find(a,id([]))),
    {'EXIT',{badarg,[{maps,find,_,_}|_]}} = (catch maps:find(a,id(<<>>))),
    ok.


t_bif_map_is_key(Config) when is_list(Config) ->
    M1 = #{ "hi" => "hello", int => 3, <<"key">> => <<"value">>, 4 => number},

    true  = maps:is_key("hi", M1),
    true  = maps:is_key(int, M1),
    true  = maps:is_key(<<"key">>, M1),
    true  = maps:is_key(4, M1),

    false = maps:is_key(5, M1),
    false = maps:is_key(<<"key2">>, M1),
    false = maps:is_key("h", M1),
    false = maps:is_key("hello", M1),
    false = maps:is_key(atom, M1),
    false = maps:is_key(any, id(#{})),

    false = maps:is_key("hi", maps:remove("hi", M1)),
    true  = maps:is_key("hi", M1),
    true  = maps:is_key(1, maps:put(1, "number", M1)),
    false = maps:is_key(1.0, maps:put(1, "number", M1)),

    %% error case
    {'EXIT',{badarg,[{maps,is_key,_,_}|_]}} = (catch maps:is_key(a,id([]))),
    {'EXIT',{badarg,[{maps,is_key,_,_}|_]}} = (catch maps:is_key(a,id(<<>>))),
    ok.

t_bif_map_keys(Config) when is_list(Config) ->
    [] = maps:keys(#{}),

    [1,2,3,4,5] = lists:sort(maps:keys(#{ 1 => a, 2 => b, 3 => c, 4 => d, 5 => e})),
    [1,2,3,4,5] = lists:sort(maps:keys(#{ 4 => d, 5 => e, 1 => a, 2 => b, 3 => c})),

    % values in key order: [4,int,"hi",<<"key">>]
    M1 = #{ "hi" => "hello", int => 3, <<"key">> => <<"value">>, 4 => number},
    [4,int,"hi",<<"key">>] = lists:sort(maps:keys(M1)),

    %% error case
    {'EXIT',{badarg,[{maps,keys,_,_}|_]}} = (catch maps:keys(1 bsl 65 + 3)),
    {'EXIT',{badarg,[{maps,keys,_,_}|_]}} = (catch maps:keys(154)),
    {'EXIT',{badarg,[{maps,keys,_,_}|_]}} = (catch maps:keys(atom)),
    {'EXIT',{badarg,[{maps,keys,_,_}|_]}} = (catch maps:keys([])),
    {'EXIT',{badarg,[{maps,keys,_,_}|_]}} = (catch maps:keys(<<>>)),
    ok.

t_bif_map_new(Config) when is_list(Config) ->
    #{} = maps:new(),
    0   = erlang:map_size(maps:new()),
    ok.

t_bif_map_merge(Config) when is_list(Config) ->
    0   = erlang:map_size(maps:merge(#{},#{})),

    M0 = #{ "hi" => "hello", int => 3, <<"key">> => <<"value">>,
	4 => number, 18446744073709551629 => wat},

    #{ "hi" := "hello", int := 3, <<"key">> := <<"value">>,
	4 := number, 18446744073709551629 := wat} = maps:merge(#{}, M0),

    #{ "hi" := "hello", int := 3, <<"key">> := <<"value">>,
	4 := number, 18446744073709551629 := wat} = maps:merge(M0, #{}),

    M1 = #{ "hi" => "hello again", float => 3.3, {1,2} => "tuple", 4 => integer },

    #{4 := number, 18446744073709551629 := wat, float := 3.3, int := 3,
	{1,2} := "tuple", "hi" := "hello", <<"key">> := <<"value">>} = maps:merge(M1,M0),

    #{4 := integer, 18446744073709551629 := wat, float := 3.3, int := 3,
	{1,2} := "tuple", "hi" := "hello again", <<"key">> := <<"value">>} = maps:merge(M0,M1),

    %% error case
    {'EXIT',{badarg,[{maps,merge,_,_}|_]}} = (catch maps:merge((1 bsl 65 + 3), <<>>)),
    {'EXIT',{badarg,[{maps,merge,_,_}|_]}} = (catch maps:merge(<<>>, id(#{ a => 1}))),
    {'EXIT',{badarg,[{maps,merge,_,_}|_]}} = (catch maps:merge(id(#{ a => 2}), <<>> )),

    ok.


t_bif_map_put(Config) when is_list(Config) ->
    M0 = #{ "hi" => "hello", int => 3, <<"key">> => <<"value">>,
	4 => number, 18446744073709551629 => wat},

    M1 = #{ "hi" := "hello"} = maps:put("hi", "hello", #{}),

    true = is_members(["hi"],maps:keys(M1)),
    true = is_members(["hello"],maps:values(M1)),

    M2 = #{ int := 3 } = maps:put(int, 3, M1),

    true = is_members([int,"hi"],maps:keys(M2)),
    true = is_members([3,"hello"],maps:values(M2)),

    M3 = #{ <<"key">> := <<"value">> } = maps:put(<<"key">>, <<"value">>, M2),

    true = is_members([int,"hi",<<"key">>],maps:keys(M3)),
    true = is_members([3,"hello",<<"value">>],maps:values(M3)),

    M4 = #{ 18446744073709551629 := wat } = maps:put(18446744073709551629, wat, M3),

    true = is_members([18446744073709551629,int,"hi",<<"key">>],maps:keys(M4)),
    true = is_members([wat,3,"hello",<<"value">>],maps:values(M4)),

    M0 = #{ 4 := number } = M5 = maps:put(4, number, M4),

    true = is_members([4,18446744073709551629,int,"hi",<<"key">>],maps:keys(M5)),
    true = is_members([number,wat,3,"hello",<<"value">>],maps:values(M5)),

    M6 = #{ <<"key">> := <<"other value">> } = maps:put(<<"key">>, <<"other value">>, M5),

    true = is_members([4,18446744073709551629,int,"hi",<<"key">>],maps:keys(M6)),
    true = is_members([number,wat,3,"hello",<<"other value">>],maps:values(M6)),

    %% error case
    {'EXIT',{badarg,[{maps,put,_,_}|_]}} = (catch maps:put(1,a,1 bsl 65 + 3)),
    {'EXIT',{badarg,[{maps,put,_,_}|_]}} = (catch maps:put(1,a,154)),
    {'EXIT',{badarg,[{maps,put,_,_}|_]}} = (catch maps:put(1,a,atom)),
    {'EXIT',{badarg,[{maps,put,_,_}|_]}} = (catch maps:put(1,a,[])),
    {'EXIT',{badarg,[{maps,put,_,_}|_]}} = (catch maps:put(1,a,<<>>)),
    ok.

is_members([],_) -> true;
is_members([K|Ks],Ls) ->
    lists:member(K,Ls) andalso is_members(Ks,Ls).

t_bif_map_remove(Config) when is_list(Config) ->
    0  = erlang:map_size(maps:remove(some_key, #{})),

    M0 = #{ "hi" => "hello", int => 3, <<"key">> => <<"value">>,
	4 => number, 18446744073709551629 => wat},

    M1 = maps:remove("hi", M0),
    true = is_members([4,18446744073709551629,int,<<"key">>],maps:keys(M1)),
    true = is_members([number,wat,3,<<"value">>],maps:values(M1)),

    M2 = maps:remove(int, M1),
    true = is_members([4,18446744073709551629,<<"key">>],maps:keys(M2)),
    true = is_members([number,wat,<<"value">>],maps:values(M2)),

    M3 = maps:remove(<<"key">>, M2),
    true = is_members([4,18446744073709551629],maps:keys(M3)),
    true = is_members([number,wat],maps:values(M3)),

    M4 = maps:remove(18446744073709551629, M3),
    true = is_members([4],maps:keys(M4)),
    true = is_members([number],maps:values(M4)),

    M5 = maps:remove(4, M4),
    [] = maps:keys(M5),
    [] = maps:values(M5),

    M0 = maps:remove(5,M0),
    M0 = maps:remove("hi there",M0),

    #{ "hi" := "hello", int := 3, 4 := number} = maps:remove(18446744073709551629,maps:remove(<<"key">>,M0)),

    %% error case
    {'EXIT',{badarg,[{maps,remove,_,_}|_]}} = (catch maps:remove(a,1 bsl 65 + 3)),
    {'EXIT',{badarg,[{maps,remove,_,_}|_]}} = (catch maps:remove(1,154)),
    {'EXIT',{badarg,[{maps,remove,_,_}|_]}} = (catch maps:remove(a,atom)),
    {'EXIT',{badarg,[{maps,remove,_,_}|_]}} = (catch maps:remove(1,[])),
    {'EXIT',{badarg,[{maps,remove,_,_}|_]}} = (catch maps:remove(a,<<>>)),
     ok.

t_bif_map_update(Config) when is_list(Config) ->
    M0 = #{ "hi" => "hello", int => 3, <<"key">> => <<"value">>,
	4 => number, 18446744073709551629 => wat},

    #{ "hi" := "hello again", int := 3, <<"key">> := <<"value">>,
	4 := number, 18446744073709551629 := wat} = maps:update("hi", "hello again", M0),

    #{ "hi" := "hello", int := 1337, <<"key">> := <<"value">>,
	4 := number, 18446744073709551629 := wat} = maps:update(int, 1337, M0),

    #{ "hi" := "hello", int := 3, <<"key">> := <<"new value">>,
	4 := number, 18446744073709551629 := wat} = maps:update(<<"key">>, <<"new value">>, M0),

    #{ "hi" := "hello", int := 3, <<"key">> := <<"value">>,
	4 := integer, 18446744073709551629 := wat} = maps:update(4, integer, M0),

    #{ "hi" := "hello", int := 3, <<"key">> := <<"value">>,
	4 := number, 18446744073709551629 := wazzup} = maps:update(18446744073709551629, wazzup, M0),

    %% error case
    {'EXIT',{badarg,[{maps,update,_,_}|_]}} = (catch maps:update(1,none,{})),
    {'EXIT',{badarg,[{maps,update,_,_}|_]}} = (catch maps:update(1,none,<<"value">>)),
    {'EXIT',{badarg,[{maps,update,_,_}|_]}} = (catch maps:update(5,none,M0)),

    ok.



t_bif_map_values(Config) when is_list(Config) ->

    [] = maps:values(#{}),
    [1] = maps:values(#{a=>1}),

    true = is_members([a,b,c,d,e],maps:values(#{ 1 => a, 2 => b, 3 => c, 4 => d, 5 => e})),
    true = is_members([a,b,c,d,e],maps:values(#{ 4 => d, 5 => e, 1 => a, 2 => b, 3 => c})),

    M1 = #{ "hi" => "hello", int => 3, <<"key">> => <<"value">>, 4 => number},
    M2 = M1#{ "hi" => "hello2", <<"key">> => <<"value2">> },
    true = is_members([number,3,"hello2",<<"value2">>],maps:values(M2)),
    true = is_members([number,3,"hello",<<"value">>],maps:values(M1)),

    %% error case
    {'EXIT',{badarg,[{maps,values,_,_}|_]}} = (catch maps:values(1 bsl 65 + 3)),
    {'EXIT',{badarg,[{maps,values,_,_}|_]}} = (catch maps:values(atom)),
    {'EXIT',{badarg,[{maps,values,_,_}|_]}} = (catch maps:values([])),
    {'EXIT',{badarg,[{maps,values,_,_}|_]}} = (catch maps:values(<<>>)),
    ok.

t_erlang_hash(Config) when is_list(Config) ->

    ok = t_bif_erlang_phash2(),
    ok = t_bif_erlang_phash(),
    ok = t_bif_erlang_hash(),

    ok.

t_bif_erlang_phash2() ->

    39679005 = erlang:phash2(#{}),
    33667975 = erlang:phash2(#{ a => 1, "a" => 2, <<"a">> => 3, {a,b} => 4 }), % 78942764
    95332690 = erlang:phash2(#{ 1 => a, 2 => "a", 3 => <<"a">>, 4 => {a,b} }), % 37338230
    108954384 = erlang:phash2(#{ 1 => a }), % 14363616
    59617982 = erlang:phash2(#{ a => 1 }), % 51612236

    42770201 = erlang:phash2(#{{} => <<>>}), % 37468437
    71687700 = erlang:phash2(#{<<>> => {}}), % 44049159

    M0 = #{ a => 1, "key" => <<"value">> },
    M1 = maps:remove("key",M0),
    M2 = M1#{ "key" => <<"value">> },

    70249457 = erlang:phash2(M0), % 118679416
    59617982 = erlang:phash2(M1), % 51612236
    70249457 = erlang:phash2(M2), % 118679416
    ok.

t_bif_erlang_phash() ->
    Sz = 1 bsl 32,
    1113425985 = erlang:phash(#{},Sz), % 268440612
    1510068139 = erlang:phash(#{ a => 1, "a" => 2, <<"a">> => 3, {a,b} => 4 },Sz), % 1196461908
    3182345590 = erlang:phash(#{ 1 => a, 2 => "a", 3 => <<"a">>, 4 => {a,b} },Sz), % 3944426064
    2927531828 = erlang:phash(#{ 1 => a },Sz), % 1394238263
    1670235874 = erlang:phash(#{ a => 1 },Sz), % 4066388227

    3935089469 = erlang:phash(#{{} => <<>>},Sz), % 1578050717
    71692856   = erlang:phash(#{<<>> => {}},Sz), % 1578050717

    M0 = #{ a => 1, "key" => <<"value">> },
    M1 = maps:remove("key",M0),
    M2 = M1#{ "key" => <<"value">> },

    2620391445 = erlang:phash(M0,Sz), % 3590546636
    1670235874 = erlang:phash(M1,Sz), % 4066388227
    2620391445 = erlang:phash(M2,Sz), % 3590546636
    ok.

t_bif_erlang_hash() ->
    Sz = 1 bsl 27 - 1,
    39684169 = erlang:hash(#{},Sz),  % 5158
    33673142 = erlang:hash(#{ a => 1, "a" => 2, <<"a">> => 3, {a,b} => 4 },Sz), % 71555838
    95337869 = erlang:hash(#{ 1 => a, 2 => "a", 3 => <<"a">>, 4 => {a,b} },Sz), % 5497225
    108959561 = erlang:hash(#{ 1 => a },Sz), % 126071654
    59623150 = erlang:hash(#{ a => 1 },Sz), % 126426236

    42775386 = erlang:hash(#{{} => <<>>},Sz), % 101655720
    71692856 = erlang:hash(#{<<>> => {}},Sz), % 101655720

    M0 = #{ a => 1, "key" => <<"value">> },
    M1 = maps:remove("key",M0),
    M2 = M1#{ "key" => <<"value">> },

    70254632 = erlang:hash(M0,Sz), % 38260486
    59623150 = erlang:hash(M1,Sz), % 126426236
    70254632 = erlang:hash(M2,Sz), % 38260486
    ok.


t_map_encode_decode(Config) when is_list(Config) ->
    <<131,116,0,0,0,0>> = erlang:term_to_binary(#{}),
    Pairs = [
	{a,b},{"key","values"},{<<"key">>,<<"value">>},
	{1,b},{[atom,1],{<<"wat">>,1,2,3}},
	{aa,"values"},
	{1 bsl 64 + (1 bsl 50 - 1), sc1},
	{99, sc2},
	{1 bsl 65 + (1 bsl 51 - 1), sc3},
	{88, sc4},
	{1 bsl 66 + (1 bsl 52 - 1), sc5},
	{77, sc6},
	{1 bsl 67 + (1 bsl 53 - 1), sc3},
	{75, sc6}, {-10,sc8},
	{<<>>, sc9}, {3.14158, sc10},
	{[3.14158], sc11}, {more_atoms, sc12},
	{{more_tuples}, sc13}, {self(), sc14},
	{{},{}},{[],[]}
    ],
    ok = map_encode_decode_and_match(Pairs,[],#{}),

    %% check sorting

    %% literally #{ b=>2, a=>1 } in the internal order
    #{ a:=1, b:=2 } =
	erlang:binary_to_term(<<131,116,0,0,0,2,100,0,1,98,97,2,100,0,1,97,97,1>>),


    %% literally #{ "hi" => "value", a=>33, b=>55 } in the internal order
    #{ a:=33, b:=55, "hi" := "value"} = erlang:binary_to_term(<<131,116,0,0,0,3,
	107,0,2,104,105,            % "hi" :: list()
	107,0,5,118,97,108,117,101, % "value" :: list()
	100,0,1,97,                 % a :: atom()
	97,33,                      % 33 :: integer()
	100,0,1,98,                 % b :: atom()
	97,55                       % 55 :: integer()
	>>),


    %% error cases
    %% template: <<131,116,0,0,0,2,100,0,1,97,100,0,1,98,97,1,97,1>>
    %% which is: #{ a=>1, b=>1 }

    %% uniqueness violation
    %% literally #{ a=>1, "hi"=>"value", a=>2 }
    {'EXIT',{badarg,[{_,_,_,_}|_]}} = (catch
	erlang:binary_to_term(<<131,116,0,0,0,3,
			       100,0,1,97,
			       97,1,
			       107,0,2,104,105,
			       107,0,5,118,97,108,117,101,
			       100,0,1,97,
			       97,2>>)),

    %% bad size (too large)
    {'EXIT',{badarg,[{_,_,_,_}|_]}} = (catch
	erlang:binary_to_term(<<131,116,0,0,0,12,100,0,1,97,97,1,100,0,1,98,97,1>>)),

    %% bad size (too small) .. should fail just truncate it .. weird.
    %% possibly change external format so truncated will be #{a:=1}
    #{ a:=b } =
	erlang:binary_to_term(<<131,116,0,0,0,1,100,0,1,97,100,0,1,98,97,1,97,1>>),

    ok.

map_encode_decode_and_match([{K,V}|Pairs], EncodedPairs, M0) ->
    M1 = maps:put(K,V,M0),
    B0 = erlang:term_to_binary(M1),
    Ls = [{erlang:term_to_binary(K), erlang:term_to_binary(V)}|EncodedPairs],
    ok = match_encoded_map(B0, length(Ls), Ls),
    %% decode and match it
    M1 = erlang:binary_to_term(B0),
    map_encode_decode_and_match(Pairs,Ls,M1);
map_encode_decode_and_match([],_,_) -> ok.

match_encoded_map(<<131,116,Size:32,Encoded/binary>>,Size,Items) ->
    match_encoded_map_stripped_size(Encoded,Items,Items);
match_encoded_map(_,_,_) -> no_match_size.

match_encoded_map_stripped_size(<<>>,_,_) -> ok;
match_encoded_map_stripped_size(B0,[{<<131,K/binary>>,<<131,V/binary>>}|Items],Ls) ->
    Ksz = byte_size(K),
    Vsz = byte_size(V),
    case B0 of
	<<K:Ksz/binary,V:Vsz/binary,B1/binary>> ->
	    match_encoded_map_stripped_size(B1,Ls,Ls);
	_ ->
	    match_encoded_map_stripped_size(B0,Items,Ls)
    end;
match_encoded_map_stripped_size(_,[],_) -> fail.


t_bif_map_to_list(Config) when is_list(Config) ->
    [] = maps:to_list(#{}),
    [{a,1},{b,2}] = lists:sort(maps:to_list(#{a=>1,b=>2})),
    [{a,1},{b,2},{c,3}] = lists:sort(maps:to_list(#{c=>3,a=>1,b=>2})),
    [{a,1},{b,2},{g,3}] = lists:sort(maps:to_list(#{g=>3,a=>1,b=>2})),
    [{a,1},{b,2},{g,3},{"c",4}] = lists:sort(maps:to_list(#{g=>3,a=>1,b=>2,"c"=>4})),
    [{3,v2},{hi,v4},{{hi,3},v5},{"hi",v3},{<<"hi">>,v1}] =
	lists:sort(maps:to_list(#{<<"hi">>=>v1,3=>v2,"hi"=>v3,hi=>v4,{hi,3}=>v5})),

    [{3,v7},{hi,v9},{{hi,3},v10},{"hi",v8},{<<"hi">>,v6}] =
	lists:sort(maps:to_list(#{<<"hi">>=>v1,3=>v2,"hi"=>v3,hi=>v4,{hi,3}=>v5,
				  <<"hi">>=>v6,3=>v7,"hi"=>v8,hi=>v9,{hi,3}=>v10})),

    %% error cases
    {'EXIT', {badarg,_}} = (catch maps:to_list(id(a))),
    {'EXIT', {badarg,_}} = (catch maps:to_list(id(42))),
    ok.


t_bif_map_from_list(Config) when is_list(Config) ->
    #{} = maps:from_list([]),
    A   = maps:from_list([]),
    0   = erlang:map_size(A),

    #{a:=1,b:=2}      = maps:from_list([{a,1},{b,2}]),
    #{c:=3,a:=1,b:=2} = maps:from_list([{a,1},{b,2},{c,3}]),
    #{g:=3,a:=1,b:=2} = maps:from_list([{a,1},{b,2},{g,3}]),

    #{a:=2} = maps:from_list([{a,1},{a,3},{a,2}]),

    #{ <<"hi">>:=v1,3:=v3,"hi":=v6,hi:=v4,{hi,3}:=v5} =
	maps:from_list([{3,v3},{"hi",v6},{hi,v4},{{hi,3},v5},{<<"hi">>,v1}]),

    #{<<"hi">>:=v6,3:=v8,"hi":=v11,hi:=v9,{hi,3}:=v10} =
	maps:from_list([ {{hi,3},v3}, {"hi",v0},{3,v1}, {<<"hi">>,v4}, {hi,v2},
	    {<<"hi">>,v6}, {{hi,3},v10},{"hi",v11}, {hi,v9}, {3,v8}]),

    %% error cases
    {'EXIT', {badarg,_}} = (catch maps:from_list(id([{a,b},b]))),
    {'EXIT', {badarg,_}} = (catch maps:from_list(id([{a,b},{b,b,3}]))),
    {'EXIT', {badarg,_}} = (catch maps:from_list(id([{a,b},<<>>]))),
    {'EXIT', {badarg,_}} = (catch maps:from_list(id([{a,b}|{b,a}]))),
    {'EXIT', {badarg,_}} = (catch maps:from_list(id(a))),
    {'EXIT', {badarg,_}} = (catch maps:from_list(id(42))),
    ok.

t_bif_build_and_check(Config) when is_list(Config) ->
    ok = check_build_and_remove(750,[
				      fun(K) -> [K,K] end,
				      fun(K) -> [float(K),K] end,
				      fun(K) -> K end,
				      fun(K) -> {1,K} end,
				      fun(K) -> {K} end,
				      fun(K) -> [K|K] end,
				      fun(K) -> [K,1,2,3,4] end,
				      fun(K) -> {K,atom} end,
				      fun(K) -> float(K) end,
				      fun(K) -> integer_to_list(K) end,
				      fun(K) -> list_to_atom(integer_to_list(K)) end,
				      fun(K) -> [K,{K,[K,{K,[K]}]}] end,
				      fun(K) -> <<K:32>> end
			      ]),

    ok.

check_build_and_remove(_,[]) -> ok;
check_build_and_remove(N,[F|Fs]) ->
    {M,Ks} = build_and_check(N, maps:new(), F, []),
    ok     = remove_and_check(Ks,M),
    check_build_and_remove(N,Fs).

build_and_check(0, M0, _, Ks) -> {M0, Ks};
build_and_check(N, M0, F, Ks) ->
    K  = build_key(F,N),
    M1 = maps:put(K,K,M0),
    ok = check_keys_exist([K|Ks], M1),
    M2 = maps:update(K,v,M1),
    v  = maps:get(K,M2),
    build_and_check(N-1,M1,F,[K|Ks]).

remove_and_check([],_) -> ok;
remove_and_check([K|Ks], M0) ->
    K     = maps:get(K,M0),
    true  = maps:is_key(K,M0),
    M1    = maps:remove(K,M0),
    false = maps:is_key(K,M1),
    true  = maps:is_key(K,M0),
    ok    = check_keys_exist(Ks,M1),
    error = maps:find(K,M1),
    remove_and_check(Ks, M1).

build_key(F,N) when N rem 3 =:= 0 -> F(N);
build_key(F,N) when N rem 3 =:= 1 -> K = F(N), {K,K};
build_key(F,N) when N rem 3 =:= 2 -> K = F(N), [K,K].

check_keys_exist([], _) -> ok;
check_keys_exist([K|Ks],M) ->
    true = maps:is_key(K,M),
    check_keys_exist(Ks,M).

t_bif_merge_and_check(Config) when is_list(Config) ->
    %% simple disjunct ones
    %% make sure all keys are unique
    Kss = [[a,b,c,d],
	   [1,2,3,4],
	   [],
	   ["hi"],
	   [e],
	   [build_key(fun(K) -> {small,K} end, I) || I <- lists:seq(1,32)],
	   lists:seq(5, 28),
	   lists:seq(29, 59),
	   [build_key(fun(K) -> integer_to_list(K) end, I) || I <- lists:seq(2000,10000)],
	   [build_key(fun(K) -> <<K:32>> end, I) || I <- lists:seq(1,80)],
	   [build_key(fun(K) -> {<<K:32>>} end, I) || I <- lists:seq(100,1000)]],


    KsMs = build_keys_map_pairs(Kss),
    Cs   = [{CKs1,CM1,CKs2,CM2} || {CKs1,CM1} <- KsMs, {CKs2,CM2} <- KsMs],
    ok   = merge_and_check_combo(Cs),

    %% overlapping ones

    KVs1 = [{a,1},{b,2},{c,3}],
    KVs2 = [{b,3},{c,4},{d,5}],
    KVs  = [{I,I} || I <- lists:seq(1,32)],
    KVs3 = KVs1 ++ KVs,
    KVs4 = KVs2 ++ KVs,

    M1  = maps:from_list(KVs1),
    M2  = maps:from_list(KVs2),
    M3  = maps:from_list(KVs3),
    M4  = maps:from_list(KVs4),

    M12 = maps:merge(M1,M2),
    ok  = check_key_values(KVs2 ++ [{a,1}], M12),
    M21 = maps:merge(M2,M1),
    ok  = check_key_values(KVs1 ++ [{d,5}], M21),

    M34 = maps:merge(M3,M4),
    ok  = check_key_values(KVs4 ++ [{a,1}], M34),
    M43 = maps:merge(M4,M3),
    ok  = check_key_values(KVs3 ++ [{d,5}], M43),

    M14 = maps:merge(M1,M4),
    ok  = check_key_values(KVs4 ++ [{a,1}], M14),
    M41 = maps:merge(M4,M1),
    ok  = check_key_values(KVs1 ++ [{d,5}] ++ KVs, M41),

    ok.

check_key_values([],_) -> ok;
check_key_values([{K,V}|KVs],M) ->
    V = maps:get(K,M),
    check_key_values(KVs,M).

merge_and_check_combo([]) -> ok;
merge_and_check_combo([{Ks1,M1,Ks2,M2}|Cs]) ->
    M12 = maps:merge(M1,M2),
    ok  = check_keys_exist(Ks1 ++ Ks2, M12),
    M21 = maps:merge(M2,M1),
    ok  = check_keys_exist(Ks1 ++ Ks2, M21),

    true = M12 =:= M21,
    M12  = M21,

    merge_and_check_combo(Cs).

build_keys_map_pairs([]) -> [];
build_keys_map_pairs([Ks|Kss]) ->
    M  = maps:from_list(keys_to_pairs(Ks)),
    ok = check_keys_exist(Ks, M),
    [{Ks,M}|build_keys_map_pairs(Kss)].

keys_to_pairs(Ks) -> [{K,K} || K <- Ks].


%% Maps module, not BIFs
t_maps_fold(_Config) ->
    Vs = lists:seq(1,100),
    M  = maps:from_list([{{k,I},{v,I}}||I<-Vs]),

    %% fold
    5050 = maps:fold(fun({k,_},{v,V},A) -> V + A end, 0, M),

    ok.

t_maps_map(_Config) ->
    Vs = lists:seq(1,100),
    M1 = maps:from_list([{I,I}||I<-Vs]),
    M2 = maps:from_list([{I,{token,I}}||I<-Vs]),

    M2 = maps:map(fun(_K,V) -> {token,V} end, M1),
    ok.

t_maps_size(_Config) ->
    Vs = lists:seq(1,100),
    lists:foldl(fun(I,M) ->
		M1 = maps:put(I,I,M),
		I  = maps:size(M1),
		M1
	end, #{}, Vs),
    ok.


t_maps_without(_Config) ->
    Ki = [11,22,33,44,55,66,77,88,99],
    M0 = maps:from_list([{{k,I},{v,I}}||I<-lists:seq(1,100)]),
    M1 = maps:from_list([{{k,I},{v,I}}||I<-lists:seq(1,100) -- Ki]),
    M1 = maps:without([{k,I}||I <- Ki],M0),
    ok.


%% MISC
t_pdict(_Config) ->

    put(#{ a => b, b => a},#{ c => d}),
    put(get(#{ a => b, b => a}),1),
    1 = get(#{ c => d}),
    #{ c := d } = get(#{ a => b, b => a}).

t_ets(_Config) ->

    Tid = ets:new(map_table,[]),

    [ets:insert(Tid,{maps:from_list([{I,-I}]),I}) || I <- lists:seq(1,100)],


    [{#{ 2 := -2},2}] = ets:lookup(Tid,#{ 2 => -2 }),

    %% Test equal
    [3,4] = lists:sort(
	      ets:select(Tid,[{{'$1','$2'},
			       [{'or',{'==','$1',#{ 3 => -3 }},
				 {'==','$1',#{ 4 => -4 }}}],
			       ['$2']}])),
    %% Test match
    [30,50] = lists:sort(
		ets:select(Tid,
			   [{{#{ 30 => -30}, '$1'},[],['$1']},
			    {{#{ 50 => -50}, '$1'},[],['$1']}]
			  )),

    ets:insert(Tid,{#{ a => b, b => c, c => a},transitivity}),

    %% Test equal with map of different size
    [] = ets:select(Tid,[{{'$1','_'},[{'==','$1',#{ b => c }}],['$_']}]),

    %% Test match with map of different size
    %[{#{ a := b },_}] = ets:select(Tid,[{{#{ b => c },'_'},[],['$_']}]),

    %%% Test match with don't care value
    %[{#{ a := b },_}] = ets:select(Tid,[{{#{ b => '_' },'_'},[],['$_']}]),

    %% Test is_map bif
    101 = length(ets:select(Tid,[{'$1',[{is_map,{element,1,'$1'}}],['$1']}])),
    ets:insert(Tid,{not_a_map,2}),
    101 = length(ets:select(Tid,[{'$1',[{is_map,{element,1,'$1'}}],['$1']}])),
    ets:insert(Tid,{{nope,a,tuple},2}),
    101 = length(ets:select(Tid,[{'$1',[{is_map,{element,1,'$1'}}],['$1']}])),

    %% Test map_size bif
    [3] = ets:select(Tid,[{{'$1','_'},[{'==',{map_size,'$1'},3}],
			   [{map_size,'$1'}]}]),

    true = ets:delete(Tid,#{50 => -50}),
    [] = ets:lookup(Tid,#{50 => -50}),

    ets:delete(Tid),
    ok.

t_dets(_Config) ->
    ok.

t_tracing(_Config) ->

    dbg:stop_clear(),
    {ok,Tracer} = dbg:tracer(process,{fun trace_collector/2, self()}),
    dbg:p(self(),c),

    %% Test basic map call
    {ok,_} = dbg:tpl(?MODULE,id,x),
    id(#{ a => b }),
    {trace,_,call,{?MODULE,id,[#{ a := b }]}} = getmsg(Tracer),
    {trace,_,return_from,{?MODULE,id,1},#{ a := b }} = getmsg(Tracer),
    dbg:ctpl(),

    %% Test equals in argument list
    {ok,_} = dbg:tpl(?MODULE,id,[{['$1'],[{'==','$1',#{ b => c}}],
				  [{return_trace}]}]),
    id(#{ a => b }),
    id(#{ b => c }),
    {trace,_,call,{?MODULE,id,[#{ b := c }]}} = getmsg(Tracer),
    {trace,_,return_from,{?MODULE,id,1},#{ b := c }} = getmsg(Tracer),
    dbg:ctpl(),

    %% Test match in head
    {ok,_} = dbg:tpl(?MODULE,id,[{[#{b => c}],[],[]}]),
    id(#{ a => b }),
    id(#{ b => c }),
    {trace,_,call,{?MODULE,id,[#{ b := c }]}} = getmsg(Tracer),
    dbg:ctpl(),

    % Test map guard bifs
    {ok,_} = dbg:tpl(?MODULE,id,[{['$1'],[{is_map,{element,1,'$1'}}],[]}]),
    id(#{ a => b }),
    id({1,2}),
    id({#{ a => b},2}),
    {trace,_,call,{?MODULE,id,[{#{ a := b },2}]}} = getmsg(Tracer),
    dbg:ctpl(),

    {ok,_} = dbg:tpl(?MODULE,id,[{['$1'],[{'==',{map_size,{element,1,'$1'}},2}],[]}]),
    id(#{ a => b }),
    id({1,2}),
    id({#{ a => b},2}),
    id({#{ a => b, b => c},atom}),
    {trace,_,call,{?MODULE,id,[{#{ a := b, b := c },atom}]}} = getmsg(Tracer),
    dbg:ctpl(),

    %MS = dbg:fun2ms(fun([A]) when A == #{ a => b} -> ok end),
    %dbg:tpl(?MODULE,id,MS),
    %id(#{ a => b }),
    %id(#{ b => c }),
    %{trace,_,call,{?MODULE,id,[#{ a := b }]}} = getmsg(Tracer),
    %dbg:ctpl(),

    %% Check to extra messages
    timeout = getmsg(Tracer),

    dbg:stop_clear(),
    ok.

getmsg(_Tracer) ->
    receive V -> V after 100 -> timeout end.

trace_collector(Msg,Parent) ->
    io:format("~p~n",[Msg]),
    Parent ! Msg,
    Parent.

%% Use this function to avoid compile-time evaluation of an expression.
id(I) -> I.
