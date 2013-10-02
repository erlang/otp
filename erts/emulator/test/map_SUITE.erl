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
	update_assoc/1,update_exact/1,
	t_guard_bifs/1, t_guard_sequence/1, t_guard_update/1,
	t_guard_receive/1, t_guard_fun/1,
	t_list_comprehension/1,
	t_map_sort_literals/1,
	t_size/1, t_map_size/1,

	%% Specific Map BIFs
	t_bif_map_get/1,
	t_bif_map_find/1,
	t_bif_map_is_key/1,
	t_bif_map_keys/1,
	t_bif_map_new/1,
	t_bif_map_put/1,
	t_bif_map_remove/1,
	t_bif_map_values/1,
	t_bif_map_to_list/1,
	t_bif_map_from_list/1,

	%% erlang
	t_erlang_hash/1,
	t_map_encode_decode/1
    ]).

-include_lib("test_server/include/test_server.hrl").


suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> [
	t_build_and_match_literals,
	t_update_literals, t_match_and_update_literals,
	update_assoc,update_exact,
	t_guard_bifs, t_guard_sequence, t_guard_update,
	t_guard_receive,t_guard_fun, t_list_comprehension,
	t_map_sort_literals,

	%% Specific Map BIFs
	t_bif_map_get,t_bif_map_find,t_bif_map_is_key,
	t_bif_map_keys,t_bif_map_new,t_bif_map_put,
	t_bif_map_remove,t_bif_map_values,
	t_bif_map_to_list, t_bif_map_from_list,

	%% erlang
	t_erlang_hash, t_map_encode_decode
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

%% Tests size(Map).

t_size(Config) when is_list(Config) ->
%    0 = size(#{}),
%    1 = size(#{a=>1}),
%    1 = size(#{a=>#{a=>1}}),
%    2 = size(#{a=>1, b=>2}),
%    3 = size(#{a=>1, b=>2, b=>"3"}),
    ok.

t_map_size(Config) when is_list(Config) ->
    0 = map_size(id(#{})),
    1 = map_size(id(#{a=>1})),
    1 = map_size(id(#{a=>"wat"})),
    2 = map_size(id(#{a=>1, b=>2})),
    3 = map_size(id(#{a=>1, b=>2, b=>"3","33"=><<"n">>})),

    true = map_is_size(#{a=>1}, 1),
    true = map_is_size(#{a=>1, a=>2}, 2),
    M = #{ "a" => 1, "b" => 2},
    true  = map_is_size(M#{ "a" => 2}, 2),
    false = map_is_size(M#{ "a" => 2}, 3),

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

update_assoc(Config) when is_list(Config) ->
    M0 = id(#{1=>a,2=>b,3.0=>c,4=>d,5=>e}),

    M1 = M0#{1=>42,2=>100,4=>[a,b,c]},
    #{1:=42,2:=100,3.0:=c,4:=[a,b,c],5:=e} = M1,
    M1 = M0#{1.0=>wrong,1:=42,2.0=>wrong,2.0=>100,4.0=>[a,b,c]},

    M2 = M0#{3.0=>new},
    #{1:=a,2:=b,3.0:=new,4:=d,5:=e} = M2,
    M2 = M0#{3.0:=wrong,3.0=>new},

    %% Errors cases.
    BadMap = id(badmap),
    {'EXIT',{badarg,_}} = (catch BadMap#{nonexisting=>val}),

    ok.

update_exact(Config) when is_list(Config) ->
    M0 = id(#{1=>a,2=>b,3.0=>c,4=>d,5=>e}),

    M1 = M0#{1:=42,2:=100,4:=[a,b,c]},
    #{1:=42,2:=100,3.0:=c,4:=[a,b,c],5:=e} = M1,
    M1 = M0#{1:=wrong,1=>42,2=>wrong,2:=100,4:=[a,b,c]},

    M2 = M0#{3.0:=new},
    #{1:=a,2:=b,3.0:=new,4:=d,5:=e} = M2,
    M2 = M0#{3.0=>wrong,3.0:=new},
    M2 = M0#{3=>wrong,3.0:=new},

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
    [#{k:=1},#{k:=2},#{k:=3}] = [#{k:=I} || I <- [1,2,3]],
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

    1    = map:get(a, #{ a=> 1}),
    2    = map:get(b, #{ a=> 1, b => 2}),
    "hi" = map:get("hello", #{ a=>1, "hello" => "hi"}),
    "tuple hi" = map:get({1,1.0}, #{ a=>a, {1,1.0} => "tuple hi"}),

    M    = id(#{ k1=>"v1", <<"k2">> => <<"v3">> }),
    "v4" = map:get(<<"k2">>, M#{ <<"k2">> => "v4" }),

    %% error case
    {'EXIT',{badarg,[{map,get,_,_}|_]}}  = (catch map:get(a,[])),
    {'EXIT',{badarg,[{map,get,_,_}|_]}}  = (catch map:get(a,<<>>)),
    {'EXIT',{bad_key,[{map,get,_,_}|_]}} = (catch map:get({1,1}, #{{1,1.0} => "tuple"})),
    {'EXIT',{bad_key,[{map,get,_,_}|_]}} = (catch map:get(a,#{})),
    {'EXIT',{bad_key,[{map,get,_,_}|_]}} = (catch map:get(a,#{ b=>1, c=>2})),
    ok.

t_bif_map_find(Config) when is_list(Config) ->

    {ok, 1}     = map:find(a, #{ a=> 1}),
    {ok, 2}     = map:find(b, #{ a=> 1, b => 2}),
    {ok, "int"} = map:find(1, #{ 1   => "int"}),
    {ok, "int"} = map:find(1.0, #{ 1 => "int"}),
    {ok, "float"} = map:find(1, #{ 1.0  => "float"}),
    {ok, "float"} = map:find(1.0, #{ 1.0=> "float"}),

    {ok, "hi"} = map:find("hello", #{ a=>1, "hello" => "hi"}),
    {ok, "tuple hi"} = map:find({1.0,1}, #{ a=>a, {1,1.0} => "tuple hi"}), % reverse types in tuple key

    M = id(#{ k1=>"v1", <<"k2">> => <<"v3">> }),
    {ok, "v4"} = map:find(<<"k2">>, M#{ <<"k2">> => "v4" }),

    %% error case
    error = map:find(a,#{}),
    error = map:find(a,#{b=>1, c=>2}),

    {'EXIT',{badarg,[{map,find,_,_}|_]}} = (catch map:find(a,[])),
    {'EXIT',{badarg,[{map,find,_,_}|_]}} = (catch map:find(a,<<>>)),
    ok.


t_bif_map_is_key(Config) when is_list(Config) ->
    M1 = #{ "hi" => "hello", int => 3, <<"key">> => <<"value">>, 4 => number},

    true  = map:is_key("hi", M1),
    true  = map:is_key(int, M1),
    true  = map:is_key(<<"key">>, M1),
    true  = map:is_key(4, M1),

    false = map:is_key(5, M1),
    false = map:is_key(<<"key2">>, M1),
    false = map:is_key("h", M1),
    false = map:is_key("hello", M1),
    false = map:is_key(atom, M1),

    false = map:is_key("hi", map:remove("hi", M1)),
    true  = map:is_key("hi", M1),
    true  = map:is_key(1, map:put(1, "number", M1)),
    false = map:is_key(1.0, map:put(1, "number", M1)),
    ok.

t_bif_map_keys(Config) when is_list(Config) ->
    [] = map:keys(#{}),

    [1,2,3,4,5] = map:keys(#{ 1 => a, 2 => b, 3 => c, 4 => d, 5 => e}),
    [1,2,3,4,5] = map:keys(#{ 4 => d, 5 => e, 1 => a, 2 => b, 3 => c}),

    % values in key order: [4,int,"hi",<<"key">>]
    M1 = #{ "hi" => "hello", int => 3, <<"key">> => <<"value">>, 4 => number},
    [4,int,"hi",<<"key">>] = map:keys(M1),

    %% error case
    {'EXIT',{badarg,[{map,keys,_,_}|_]}} = (catch map:keys(1 bsl 65 + 3)),
    {'EXIT',{badarg,[{map,keys,_,_}|_]}} = (catch map:keys(154)),
    {'EXIT',{badarg,[{map,keys,_,_}|_]}} = (catch map:keys(atom)),
    {'EXIT',{badarg,[{map,keys,_,_}|_]}} = (catch map:keys([])),
    {'EXIT',{badarg,[{map,keys,_,_}|_]}} = (catch map:keys(<<>>)),
    ok.

t_bif_map_new(Config) when is_list(Config) ->
    #{} = map:new(),
    0   = erlang:map_size(map:new()),
    ok.

t_bif_map_put(Config) when is_list(Config) ->
    M0 = #{ "hi" => "hello", int => 3, <<"key">> => <<"value">>,
	4 => number, 18446744073709551629 => wat},

    M1 = #{ "hi" := "hello"} = map:put("hi", "hello", #{}),

    ["hi"]    = map:keys(M1),
    ["hello"] = map:values(M1),

    M2 = #{ int := 3 } = map:put(int, 3, M1),

    [int,"hi"]  = map:keys(M2),
    [3,"hello"] = map:values(M2),

    M3 = #{ <<"key">> := <<"value">> } = map:put(<<"key">>, <<"value">>, M2),

    [int,"hi",<<"key">>]    = map:keys(M3),
    [3,"hello",<<"value">>] = map:values(M3),

    M4 = #{ 18446744073709551629 := wat } = map:put(18446744073709551629, wat, M3),

    [18446744073709551629,int,"hi",<<"key">>] = map:keys(M4),
    [wat,3,"hello",<<"value">>]               = map:values(M4),

    M0 = #{ 4 := number } = M5 = map:put(4, number, M4),

    [4,18446744073709551629,int,"hi",<<"key">>] = map:keys(M5),
    [number,wat,3,"hello",<<"value">>]          = map:values(M5),

    %% error case
    {'EXIT',{badarg,[{map,put,_,_}|_]}} = (catch map:put(1,a,1 bsl 65 + 3)),
    {'EXIT',{badarg,[{map,put,_,_}|_]}} = (catch map:put(1,a,154)),
    {'EXIT',{badarg,[{map,put,_,_}|_]}} = (catch map:put(1,a,atom)),
    {'EXIT',{badarg,[{map,put,_,_}|_]}} = (catch map:put(1,a,[])),
    {'EXIT',{badarg,[{map,put,_,_}|_]}} = (catch map:put(1,a,<<>>)),
     ok.

t_bif_map_remove(Config) when is_list(Config) ->
    M0 = #{ "hi" => "hello", int => 3, <<"key">> => <<"value">>,
	4 => number, 18446744073709551629 => wat},

    M1 = map:remove("hi", M0),
    [4,18446744073709551629,int,<<"key">>] = map:keys(M1),
    [number,wat,3,<<"value">>]             = map:values(M1),

    M2 = map:remove(int, M1),
    [4,18446744073709551629,<<"key">>] = map:keys(M2),
    [number,wat,<<"value">>]           = map:values(M2),

    M3 = map:remove(<<"key">>, M2),
    [4,18446744073709551629] = map:keys(M3),
    [number,wat]             = map:values(M3),

    M4 = map:remove(18446744073709551629, M3),
    [4]      = map:keys(M4),
    [number] = map:values(M4),

    M5 = map:remove(4, M4),
    [] = map:keys(M5),
    [] = map:values(M5),

    M0 = map:remove(5,M0),
    M0 = map:remove("hi there",M0),

    #{ "hi" := "hello", int := 3, 4 := number} = map:remove(18446744073709551629,map:remove(<<"key">>,M0)),

    %% error case
    {'EXIT',{badarg,[{map,remove,_,_}|_]}} = (catch map:remove(a,1 bsl 65 + 3)),
    {'EXIT',{badarg,[{map,remove,_,_}|_]}} = (catch map:remove(1,154)),
    {'EXIT',{badarg,[{map,remove,_,_}|_]}} = (catch map:remove(a,atom)),
    {'EXIT',{badarg,[{map,remove,_,_}|_]}} = (catch map:remove(1,[])),
    {'EXIT',{badarg,[{map,remove,_,_}|_]}} = (catch map:remove(a,<<>>)),
     ok.


t_bif_map_values(Config) when is_list(Config) ->

    [] = map:values(#{}),

    [a,b,c,d,e] = map:values(#{ 1 => a, 2 => b, 3 => c, 4 => d, 5 => e}),
    [a,b,c,d,e] = map:values(#{ 4 => d, 5 => e, 1 => a, 2 => b, 3 => c}),

    % values in key order: [4,int,"hi",<<"key">>]
    M1 = #{ "hi" => "hello", int => 3, <<"key">> => <<"value">>, 4 => number},
    M2 = M1#{ "hi" => "hello2", <<"key">> => <<"value2">> },
    [number,3,"hello2",<<"value2">>] = map:values(M2),
    [number,3,"hello",<<"value">>]   = map:values(M1),

    %% error case
    {'EXIT',{badarg,[{map,values,_,_}|_]}} = (catch map:values(1 bsl 65 + 3)),
    {'EXIT',{badarg,[{map,values,_,_}|_]}} = (catch map:values(atom)),
    {'EXIT',{badarg,[{map,values,_,_}|_]}} = (catch map:values([])),
    {'EXIT',{badarg,[{map,values,_,_}|_]}} = (catch map:values(<<>>)),
    ok.

t_erlang_hash(Config) when is_list(Config) ->

    39679005  = erlang:phash2(#{}),
    106033788 = erlang:phash2(#{ a => 1, "a" => 2, <<"a">> => 3, {a,b} => 4 }),
    119861073 = erlang:phash2(#{ 1 => a, 2 => "a", 3 => <<"a">>, 4 => {a,b} }),
    87559846  = erlang:phash2(#{ 1 => a }),
    76038729  = erlang:phash2(#{ a => 1 }),

    M0 = #{ a => 1, "key" => <<"value">> },
    M1 = map:remove("key",M0),
    M2 = M1#{ "key" => <<"value">> },

    67968757  = erlang:phash2(M0),
    76038729  = erlang:phash2(M1),
    67968757  = erlang:phash2(M2),
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

    %% error cases
    %% template: <<131,116,0,0,0,2,100,0,1,97,100,0,1,98,97,1,97,1>>
    %% which is: #{ a=>1, b=>1 }

    %% order violation
    %% literally #{ b=>1, a=>1 } in the internal order (bad)
    {'EXIT',{badarg,[{_,_,_,_}|_]}} = (catch
	erlang:binary_to_term(<<131,116,0,0,0,2,100,0,1,98,100,0,1,97,97,1,97,1>>)),

    %% uniqueness violation
    %% literally #{ a=>1, a=>1 }
    {'EXIT',{badarg,[{_,_,_,_}|_]}} = (catch
	erlang:binary_to_term(<<131,116,0,0,0,2,100,0,1,97,100,0,1,97,97,1,97,1>>)),

    %% bad size (too large)
    {'EXIT',{badarg,[{_,_,_,_}|_]}} = (catch
	erlang:binary_to_term(<<131,116,0,0,0,12,100,0,1,97,100,0,1,98,97,1,97,1>>)),

    %% bad size (too small) .. should fail just truncate it .. weird.
    %% possibly change external format so truncated will be #{a:=1}
    #{ a:=b } =
	erlang:binary_to_term(<<131,116,0,0,0,1,100,0,1,97,100,0,1,98,97,1,97,1>>),

    ok.

map_encode_decode_and_match([{K,V}|Pairs], EncodedPairs, M0) ->
    M1 = map:put(K,V,M0),
    B0 = erlang:term_to_binary(M1),
    Ls = lists:sort([{K, erlang:term_to_binary(K), erlang:term_to_binary(V)}|EncodedPairs]),
    %% sort Ks and Vs according to term spec, then match it
    ok = match_encoded_map(B0, length(Ls), [Kbin||{_,Kbin,_}<-Ls] ++ [Vbin||{_,_,Vbin}<-Ls]),
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
    [] = map:to_list(#{}),
    [{a,1},{b,2}] = map:to_list(#{a=>1,b=>2}),
    [{a,1},{b,2},{c,3}] = map:to_list(#{c=>3,a=>1,b=>2}),
    [{a,1},{b,2},{g,3}] = map:to_list(#{g=>3,a=>1,b=>2}),
    [{a,1},{b,2},{g,3},{"c",4}] = map:to_list(#{g=>3,a=>1,b=>2,"c"=>4}),
    [{3,v2},{hi,v4},{{hi,3},v5},{"hi",v3},{<<"hi">>,v1}] = map:to_list(#{
	    <<"hi">>=>v1,3=>v2,"hi"=>v3,hi=>v4,{hi,3}=>v5}),

    [{3,v7},{hi,v9},{{hi,3},v10},{"hi",v8},{<<"hi">>,v6}] = map:to_list(#{
	    <<"hi">>=>v1,3=>v2,"hi"=>v3,hi=>v4,{hi,3}=>v5,
	    <<"hi">>=>v6,3=>v7,"hi"=>v8,hi=>v9,{hi,3}=>v10}),

    %% error cases
    {'EXIT', {badarg,_}} = (catch map:to_list(id(a))),
    {'EXIT', {badarg,_}} = (catch map:to_list(id(42))),
    ok.


t_bif_map_from_list(Config) when is_list(Config) ->
    #{} = map:from_list([]),
    A   = map:from_list([]),
    0   = erlang:map_size(A),

    #{a:=1,b:=2}  = map:from_list([{a,1},{b,2}]),
    #{c:=3,a:=1,b:=2} = map:from_list([{a,1},{b,2},{c,3}]),
    #{g:=3,a:=1,b:=2} = map:from_list([{a,1},{b,2},{g,3}]),

    #{a:=2} = map:from_list([{a,1},{a,3},{a,2}]),

    #{ <<"hi">>:=v1,3:=v3,"hi":=v6,hi:=v4,{hi,3}:=v5} =
	map:from_list([{3,v3},{"hi",v6},{hi,v4},{{hi,3},v5},{<<"hi">>,v1}]),

    #{<<"hi">>:=v6,3:=v8,"hi":=v11,hi:=v9,{hi,3}:=v10} =
	map:from_list([ {{hi,3},v3}, {"hi",v0},{3,v1}, {<<"hi">>,v4}, {hi,v2},
	    {<<"hi">>,v6}, {{hi,3},v10},{"hi",v11}, {hi,v9}, {3,v8}]),

    %% error cases
    {'EXIT', {badarg,_}} = (catch map:from_list(id(a))),
    {'EXIT', {badarg,_}} = (catch map:from_list(id(42))),
    ok.


%% Use this function to avoid compile-time evaluation of an expression.
id(I) -> I.
