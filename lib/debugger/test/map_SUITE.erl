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

%% Copied from map_SUITE in erts.

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
	%t_size/1,
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

	%% maps module not bifs
	t_maps_fold/1,
	t_maps_map/1,
	t_maps_size/1,
	t_maps_without/1,

	%% misc
	t_pdict/1,
	t_ets/1,
	t_dets/1
    ]).

-include_lib("stdlib/include/ms_transform.hrl").

suite() -> [].

all() -> [
	t_build_and_match_literals,
	t_update_literals, t_match_and_update_literals,
	t_update_map_expressions,
	t_update_assoc,t_update_exact,
	t_guard_bifs, t_guard_sequence, t_guard_update,
	t_guard_receive,t_guard_fun, t_list_comprehension,
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

	%% maps module
	t_maps_fold, t_maps_map,
	t_maps_size, t_maps_without,


        %% Other functions
	t_pdict,
	t_ets
    ].

groups() -> [].

init_per_suite(Config) ->
    test_lib:interpret(?MODULE),
    Config.

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


%% Tests size(Map).
%% not implemented, perhaps it shouldn't be either

%t_size(Config) when is_list(Config) ->
%    0 = size(#{}),
%    1 = size(#{a=>1}),
%    1 = size(#{a=>#{a=>1}}),
%    2 = size(#{a=>1, b=>2}),
%    3 = size(#{a=>1, b=>2, b=>"3"}),
%    ok.

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

    %% Error cases.
    {'EXIT',{badarg,_}} = (catch map_size([])),
    {'EXIT',{badarg,_}} = (catch map_size(<<1,2,3>>)),
    {'EXIT',{badarg,_}} = (catch map_size(1)),
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
    {'EXIT',{{badarg,<<>>},_}} = (catch (id(<<>>))#{ a := 42, b => 2 }),
    {'EXIT',{{badarg,[]},_}} = (catch (id([]))#{ a := 42, b => 2 }),
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
    {'EXIT',{{badarg,BadMap},_}} = (catch BadMap#{nonexisting=>val}),

    ok.

t_update_exact(Config) when is_list(Config) ->
    M0 = id(#{1=>a,2=>b,3.0=>c,4=>d,5=>e}),

    M1 = M0#{1:=42,2:=100,4:=[a,b,c]},
    #{1:=42,2:=100,3.0:=c,4:=[a,b,c],5:=e} = M1,
    M1 = M0#{1:=wrong,1=>42,2=>wrong,2:=100,4:=[a,b,c]},

    M2 = M0#{3.0:=new},
    #{1:=a,2:=b,3.0:=new,4:=d,5:=e} = M2,
    M2 = M0#{3.0=>wrong,3.0:=new},
    %% M2 = M0#{3=>wrong,3.0:=new}, %% FIXME

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
    false = #{ 1 => 1 } < id(#{ 1.0 => 1}),
    false = #{ 1.0 => 1 } < id(#{ 1 => 1}),

    %% value order
    true  = #{ a => 1 } < id(#{ a => 2}),
    false = #{ a => 2 } < id(#{ a => 1}),
    false = #{ a => 2, b => 1 } < id(#{ a => 1, b => 3}),
    true  = #{ a => 1, b => 1 } < id(#{ a => 1, b => 3}),

    true  = #{ "a" => "hi", b => 134 } == id(#{ b => 134,"a" => "hi"}),

    %% lists:sort

    SortVs = [#{"a"=>1},#{a=>2},#{1=>3},#{<<"a">>=>4}],
    [#{1:=ok},#{a:=ok},#{"a":=ok},#{<<"a">>:=ok}] = lists:sort([#{"a"=>ok},#{a=>ok},#{1=>ok},#{<<"a">>=>ok}]),
    [#{1:=3},#{a:=2},#{"a":=1},#{<<"a">>:=4}] = lists:sort(SortVs),
    [#{1:=3},#{a:=2},#{"a":=1},#{<<"a">>:=4}] = lists:sort(lists:reverse(SortVs)),

    ok.

%% BIFs
t_bif_map_get(Config) when is_list(Config) ->

    1    = maps:get(a, #{ a=> 1}),
    2    = maps:get(b, #{ a=> 1, b => 2}),
    "hi" = maps:get("hello", #{ a=>1, "hello" => "hi"}),
    "tuple hi" = maps:get({1,1.0}, #{ a=>a, {1,1.0} => "tuple hi"}),

    M    = id(#{ k1=>"v1", <<"k2">> => <<"v3">> }),
    "v4" = maps:get(<<"k2">>, M#{ <<"k2">> => "v4" }),

    %% error case
    {'EXIT',{badarg, [{maps,get,_,_}|_]}} = (catch maps:get(a,[])),
    {'EXIT',{badarg, [{maps,get,_,_}|_]}} = (catch maps:get(a,<<>>)),
    {'EXIT',{bad_key,[{maps,get,_,_}|_]}} = (catch maps:get({1,1}, #{{1,1.0} => "tuple"})),
    {'EXIT',{bad_key,[{maps,get,_,_}|_]}} = (catch maps:get(a,#{})),
    {'EXIT',{bad_key,[{maps,get,_,_}|_]}} = (catch maps:get(a,#{ b=>1, c=>2})),
    ok.

t_bif_map_find(Config) when is_list(Config) ->

    {ok, 1}     = maps:find(a, #{ a=> 1}),
    {ok, 2}     = maps:find(b, #{ a=> 1, b => 2}),
    {ok, "int"} = maps:find(1, #{ 1   => "int"}),
    {ok, "float"} = maps:find(1.0, #{ 1.0=> "float"}),

    {ok, "hi"} = maps:find("hello", #{ a=>1, "hello" => "hi"}),
    {ok, "tuple hi"} = maps:find({1,1.0}, #{ a=>a, {1,1.0} => "tuple hi"}),

    M = id(#{ k1=>"v1", <<"k2">> => <<"v3">> }),
    {ok, "v4"} = maps:find(<<"k2">>, M#{ <<"k2">> => "v4" }),

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

    [1,2,3,4,5] = maps:keys(#{ 1 => a, 2 => b, 3 => c, 4 => d, 5 => e}),
    [1,2,3,4,5] = maps:keys(#{ 4 => d, 5 => e, 1 => a, 2 => b, 3 => c}),

    % values in key order: [4,int,"hi",<<"key">>]
    M1 = #{ "hi" => "hello", int => 3, <<"key">> => <<"value">>, 4 => number},
    [4,int,"hi",<<"key">>] = maps:keys(M1),

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

    ["hi"]    = maps:keys(M1),
    ["hello"] = maps:values(M1),

    M2 = #{ int := 3 } = maps:put(int, 3, M1),

    [int,"hi"]  = maps:keys(M2),
    [3,"hello"] = maps:values(M2),

    M3 = #{ <<"key">> := <<"value">> } = maps:put(<<"key">>, <<"value">>, M2),

    [int,"hi",<<"key">>]    = maps:keys(M3),
    [3,"hello",<<"value">>] = maps:values(M3),

    M4 = #{ 18446744073709551629 := wat } = maps:put(18446744073709551629, wat, M3),

    [18446744073709551629,int,"hi",<<"key">>] = maps:keys(M4),
    [wat,3,"hello",<<"value">>]               = maps:values(M4),

    M0 = #{ 4 := number } = M5 = maps:put(4, number, M4),

    [4,18446744073709551629,int,"hi",<<"key">>] = maps:keys(M5),
    [number,wat,3,"hello",<<"value">>]          = maps:values(M5),

    M6 = #{ <<"key">> := <<"other value">> } = maps:put(<<"key">>, <<"other value">>, M5),

    [4,18446744073709551629,int,"hi",<<"key">>] = maps:keys(M6),
    [number,wat,3,"hello",<<"other value">>]    = maps:values(M6),

    %% error case
    {'EXIT',{badarg,[{maps,put,_,_}|_]}} = (catch maps:put(1,a,1 bsl 65 + 3)),
    {'EXIT',{badarg,[{maps,put,_,_}|_]}} = (catch maps:put(1,a,154)),
    {'EXIT',{badarg,[{maps,put,_,_}|_]}} = (catch maps:put(1,a,atom)),
    {'EXIT',{badarg,[{maps,put,_,_}|_]}} = (catch maps:put(1,a,[])),
    {'EXIT',{badarg,[{maps,put,_,_}|_]}} = (catch maps:put(1,a,<<>>)),
     ok.

t_bif_map_remove(Config) when is_list(Config) ->
    0  = erlang:map_size(maps:remove(some_key, #{})),

    M0 = #{ "hi" => "hello", int => 3, <<"key">> => <<"value">>,
	4 => number, 18446744073709551629 => wat},

    M1 = maps:remove("hi", M0),
    [4,18446744073709551629,int,<<"key">>] = maps:keys(M1),
    [number,wat,3,<<"value">>]             = maps:values(M1),

    M2 = maps:remove(int, M1),
    [4,18446744073709551629,<<"key">>] = maps:keys(M2),
    [number,wat,<<"value">>]           = maps:values(M2),

    M3 = maps:remove(<<"key">>, M2),
    [4,18446744073709551629] = maps:keys(M3),
    [number,wat]             = maps:values(M3),

    M4 = maps:remove(18446744073709551629, M3),
    [4]      = maps:keys(M4),
    [number] = maps:values(M4),

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

    [a,b,c,d,e] = maps:values(#{ 1 => a, 2 => b, 3 => c, 4 => d, 5 => e}),
    [a,b,c,d,e] = maps:values(#{ 4 => d, 5 => e, 1 => a, 2 => b, 3 => c}),

    % values in key order: [4,int,"hi",<<"key">>]
    M1 = #{ "hi" => "hello", int => 3, <<"key">> => <<"value">>, 4 => number},
    M2 = M1#{ "hi" => "hello2", <<"key">> => <<"value2">> },
    [number,3,"hello2",<<"value2">>] = maps:values(M2),
    [number,3,"hello",<<"value">>]   = maps:values(M1),

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
    78942764 = erlang:phash2(#{ a => 1, "a" => 2, <<"a">> => 3, {a,b} => 4 }),
    37338230 = erlang:phash2(#{ 1 => a, 2 => "a", 3 => <<"a">>, 4 => {a,b} }),
    14363616 = erlang:phash2(#{ 1 => a }),
    51612236 = erlang:phash2(#{ a => 1 }),

    37468437 = erlang:phash2(#{{} => <<>>}),
    44049159 = erlang:phash2(#{<<>> => {}}),

    M0 = #{ a => 1, "key" => <<"value">> },
    M1 = maps:remove("key",M0),
    M2 = M1#{ "key" => <<"value">> },

    118679416 = erlang:phash2(M0),
    51612236  = erlang:phash2(M1),
    118679416 = erlang:phash2(M2),
    ok.

t_bif_erlang_phash() ->
    Sz = 1 bsl 32,
    268440612  = erlang:phash(#{},Sz),
    1196461908 = erlang:phash(#{ a => 1, "a" => 2, <<"a">> => 3, {a,b} => 4 },Sz),
    3944426064 = erlang:phash(#{ 1 => a, 2 => "a", 3 => <<"a">>, 4 => {a,b} },Sz),
    1394238263 = erlang:phash(#{ 1 => a },Sz),
    4066388227 = erlang:phash(#{ a => 1 },Sz),

    1578050717 = erlang:phash(#{{} => <<>>},Sz),
    1578050717 = erlang:phash(#{<<>> => {}},Sz), % yep, broken

    M0 = #{ a => 1, "key" => <<"value">> },
    M1 = maps:remove("key",M0),
    M2 = M1#{ "key" => <<"value">> },

    3590546636 = erlang:phash(M0,Sz),
    4066388227 = erlang:phash(M1,Sz),
    3590546636 = erlang:phash(M2,Sz),
    ok.

t_bif_erlang_hash() ->
    Sz = 1 bsl 27 - 1,
    5158      = erlang:hash(#{},Sz),
    71555838  = erlang:hash(#{ a => 1, "a" => 2, <<"a">> => 3, {a,b} => 4 },Sz),
    5497225   = erlang:hash(#{ 1 => a, 2 => "a", 3 => <<"a">>, 4 => {a,b} },Sz),
    126071654 = erlang:hash(#{ 1 => a },Sz),
    126426236 = erlang:hash(#{ a => 1 },Sz),

    101655720 = erlang:hash(#{{} => <<>>},Sz),
    101655720 = erlang:hash(#{<<>> => {}},Sz), % yep, broken

    M0 = #{ a => 1, "key" => <<"value">> },
    M1 = maps:remove("key",M0),
    M2 = M1#{ "key" => <<"value">> },

    38260486  = erlang:hash(M0,Sz),
    126426236 = erlang:hash(M1,Sz),
    38260486  = erlang:hash(M2,Sz),
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
	107,0,2,104,105, % "hi" :: list()
	107,0,5,118,97,108,117,101, % "value" :: list()
	100,0,1,97, % a :: atom()
	97,33, % 33 :: integer()
	100,0,1,98, % b :: atom()
	97,55  % 55 :: integer()
	>>),


    %% error cases
    %% template: <<131,116,0,0,0,2,100,0,1,97,100,0,1,98,97,1,97,1>>
    %% which is: #{ a=>1, b=>1 }

    %% uniqueness violation
    %% literally #{ a=>1, "hi"=>"value", a=>2 }
    {'EXIT',{badarg,[{_,_,_,_}|_]}} = (catch
	erlang:binary_to_term(<<131,116,0,0,0,3,100,0,1,97,107,0,2,104,105,100,0,1,97,97,1,107,0,5,118,97,108,117,101,97,2>>)),

    %% bad size (too large)
    {'EXIT',{badarg,[{_,_,_,_}|_]}} = (catch
	erlang:binary_to_term(<<131,116,0,0,0,12,100,0,1,97,100,0,1,98,97,1,97,1>>)),

    %% bad size (too small) .. should fail just truncate it .. weird.
    %% possibly change external format so truncated will be #{a:=1}
    #{ a:=b } =
	erlang:binary_to_term(<<131,116,0,0,0,1,100,0,1,97,100,0,1,98,97,1,97,1>>),

    ok.

map_encode_decode_and_match([{K,V}|Pairs], EncodedPairs, M0) ->
    M1 = maps:put(K,V,M0),
    B0 = erlang:term_to_binary(M1),
    Ls = lists:sort(fun(A,B) -> erts_internal:cmp_term(A,B) < 0 end, [{K, erlang:term_to_binary(K), erlang:term_to_binary(V)}|EncodedPairs]),
    %% sort Ks and Vs according to term spec, then match it
    KVbins = lists:foldr(fun({_,Kbin,Vbin}, Acc) -> [Kbin,Vbin | Acc] end, [], Ls),
    ok = match_encoded_map(B0, length(Ls), KVbins),
    %% decode and match it
    M1 = erlang:binary_to_term(B0),
    map_encode_decode_and_match(Pairs,Ls,M1);
map_encode_decode_and_match([],_,_) -> ok.

match_encoded_map(<<131,116,Size:32,Encoded/binary>>,Size,Items) ->
    match_encoded_map(Encoded,Items);
match_encoded_map(_,_,_) -> no_match_size.

match_encoded_map(<<>>,[]) -> ok;
match_encoded_map(Bin,[<<131,Item/binary>>|Items]) ->
    Size = erlang:byte_size(Item),
    <<EncodedTerm:Size/binary, Bin1/binary>> = Bin,
    EncodedTerm = Item, %% Asssert
    match_encoded_map(Bin1,Items).


t_bif_map_to_list(Config) when is_list(Config) ->
    [] = maps:to_list(#{}),
    [{a,1},{b,2}] = maps:to_list(#{a=>1,b=>2}),
    [{a,1},{b,2},{c,3}] = maps:to_list(#{c=>3,a=>1,b=>2}),
    [{a,1},{b,2},{g,3}] = maps:to_list(#{g=>3,a=>1,b=>2}),
    [{a,1},{b,2},{g,3},{"c",4}] = maps:to_list(#{g=>3,a=>1,b=>2,"c"=>4}),
    [{3,v2},{hi,v4},{{hi,3},v5},{"hi",v3},{<<"hi">>,v1}] = maps:to_list(#{
	    <<"hi">>=>v1,3=>v2,"hi"=>v3,hi=>v4,{hi,3}=>v5}),

    [{3,v7},{hi,v9},{{hi,3},v10},{"hi",v8},{<<"hi">>,v6}] = maps:to_list(#{
	    <<"hi">>=>v1,3=>v2,"hi"=>v3,hi=>v4,{hi,3}=>v5,
	    <<"hi">>=>v6,3=>v7,"hi"=>v8,hi=>v9,{hi,3}=>v10}),

    %% error cases
    {'EXIT', {badarg,_}} = (catch maps:to_list(id(a))),
    {'EXIT', {badarg,_}} = (catch maps:to_list(id(42))),
    ok.


t_bif_map_from_list(Config) when is_list(Config) ->
    #{} = maps:from_list([]),
    A   = maps:from_list([]),
    0   = erlang:map_size(A),

    #{a:=1,b:=2}  = maps:from_list([{a,1},{b,2}]),
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

getmsg(_Tracer) ->
    receive V -> V after 100 -> timeout end.

trace_collector(Msg,Parent) ->
    io:format("~p~n",[Msg]),
    Parent ! Msg,
    Parent.

%% Use this function to avoid compile-time evaluation of an expression.
id(I) -> I.
