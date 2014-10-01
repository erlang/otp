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
	%% literals
	t_build_and_match_literals/1,
	t_update_literals/1,t_match_and_update_literals/1,
	t_update_map_expressions/1,
	t_update_assoc/1,t_update_exact/1,
	t_guard_bifs/1, t_guard_sequence/1, t_guard_update/1,
	t_guard_receive/1, t_guard_fun/1,
	t_list_comprehension/1,
	t_map_sort_literals/1,
	t_map_size/1,
	t_build_and_match_aliasing/1,

	%% variables
	t_build_and_match_variables/1,
	t_update_assoc_variables/1,t_update_exact_variables/1,
	t_nested_pattern_expressions/1,
	t_guard_update_variables/1,
	t_guard_sequence_variables/1,
	t_guard_sequence_mixed/1,
	t_frequency_table/1,

	%% warnings
	t_warn_useless_build/1,
	t_warn_pair_key_overloaded/1,

	%% not covered in 17.0-rc1
	t_build_and_match_over_alloc/1,
	t_build_and_match_empty_val/1,
	t_build_and_match_val/1,
	t_build_and_match_nil/1,
	t_build_and_match_structure/1,

	%% errors in 17.0-rc1
	t_update_values/1,
        t_expand_map_update/1,
        t_export/1
    ]).

suite() -> [].

all() -> [
	%% literals
	t_build_and_match_literals,
	t_update_literals, t_match_and_update_literals,
	t_update_map_expressions,
	t_update_assoc,t_update_exact,
	t_guard_bifs, t_guard_sequence, t_guard_update,
	t_guard_receive,t_guard_fun, t_list_comprehension,
	t_map_sort_literals,
	t_map_size,
	t_build_and_match_aliasing,

	%% variables
	t_build_and_match_variables,
	t_update_assoc_variables,t_update_exact_variables,
	t_nested_pattern_expressions,
	t_guard_update_variables,
	t_guard_sequence_variables,
	t_guard_sequence_mixed,
	t_frequency_table,

	%% warnings
	t_warn_useless_build,
	t_warn_pair_key_overloaded,

	%% not covered in 17.0-rc1
	t_build_and_match_over_alloc,
	t_build_and_match_empty_val,
	t_build_and_match_val,
	t_build_and_match_nil,
	t_build_and_match_structure,


	%% errors in 17.0-rc1
	t_update_values,
        t_expand_map_update,
        t_export
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

    %% map key
    #{ #{} := 42 } = id(#{ #{} => 42 }),
    #{ #{ "a" => 3 } := 42 } = id(#{ #{ "a" => 3} => 42 }),

    %% nil key
    #{[]:=ok,1:=2} = id(#{[]=>ok,1=>2}),
    #{1:=2,[]:=ok,1:=2} = id(#{[]=>ok,1=>2}),

    %% pseudo literals
    #{ -3 := yep } = id(#{ -3 => yep }),
    #{ <<0:358>> := "three" } = id(#{<<0:358>> =>"three"}),

    %% error case
    {'EXIT',{{badmatch,_},_}} = (catch (#{x:=3,x:=2} = id(#{x=>3}))),
    {'EXIT',{{badmatch,_},_}} = (catch (#{x:=2} = id(#{x=>3}))),
    {'EXIT',{{badmatch,_},_}} = (catch (#{x:=3} = id({a,b,c}))),
    {'EXIT',{{badmatch,_},_}} = (catch (#{x:=3} = id(#{y=>3}))),
    {'EXIT',{{badmatch,_},_}} = (catch (#{x:=3} = id(#{x=>"three"}))),
    {'EXIT',{{badmatch,_},_}} = (catch (#{#{"a"=>42} := 3}=id(#{#{"a"=>3}=>42}))),
    ok.


t_build_and_match_aliasing(Config) when is_list(Config) ->
    M1 = id(#{a=>1,b=>2,c=>3,d=>4}),
    #{c:=C1=_=_=C2} = M1,
    true = C1 =:= C2,
    #{a:=A,a:=A,a:=A,b:=B,b:=B} = M1,
    #{a:=A,a:=A,a:=A,b:=B,b:=B,b:=2} = M1,
    #{a:=A=1,a:=A,a:=A,b:=B=2,b:=B,b:=2} = M1,
    #{c:=C1, c:=_, c:=3, c:=_, c:=C2} = M1,
    #{c:=C=_=3=_=C} = M1,

    M2 = id(#{"a"=>1,"b"=>2,"c"=>3,"d"=>4}),
    #{"a":=A2,"a":=A2,"a":=A2,"b":=B2,"b":=B2,"b":=2} = M2,
    #{"a":=_,"a":=_,"a":=_,"b":=_,"b":=_,"b":=2} = M2,

    #{a:=A1,a:=A1,a:=A1,b:=B1,b:=B1} = #{a:=A1,a:=A1,a:=A1,b:=B1,b:=B1,b:=2} = M1,
    #{"a":=A3,"b":=B3} = #{"a":=A3,"a":=A3} = #{"b":=B3,"b":=2} = M2,

    #{"a":=1,"b":=2,"c":=3,"d":=4} = #{"a":=A4,"b":=B4} = #{"a":=A4,"a":=A4} = #{"b":=B4,"d":=4} = M2,
    #{"a":=A5,"b":=B5} = #{"a":=A5,"a":=A5} = #{"b":=B5,"d":=4} = #{"a":=1,"b":=2,"c":=3,"d":=4} = M2,
    #{"a":=_,"b":=_} = #{"a":=_,"a":=_} = #{"b":=_,"d":=4} = #{"a":=1,"b":=2,"c":=3,"d":=4} = M2,

    M3 = id(#{<<12:300>>=>1,<<13:300>>=>2}),
    #{<<12:300>> := V1, <<13:300>> := V2} = #{<<13:300>> := V2, <<12:300>> := V1} = M3,
    #{<<12:300>> := 1, <<13:300>> := 2} = #{<<13:300>> := _, <<12:300>> := _} = M3,
    #{<<13:300>> := _, <<12:300>> := _} = #{<<12:300>> := 1, <<13:300>> := 2} = M3,

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
    X = id(fondue),
    M1 = #{ a := 1 } = M#{a => 1},
    #{ b := {X} } = M1#{ a := 1, b => {X} },

    #{ b := 2 } = (maps:new())#{ b => 2 },

    #{ a :=42, b:=42, c:=42 } = (maps:from_list([{a,1},{b,2},{c,3}]))#{ a := 42, b := 42, c := 42 },
    #{ "a" :=1, "b":=42, "c":=42 } = (maps:from_list([{"a",1},{"b",2}]))#{ "b" := 42, "c" => 42 },

    %% Test need to be in a fun.
    %% This tests that let expr optimisation in sys_core_fold
    %% covers maps correctly.
    F = fun() ->
	    M0 = id(#{ "a" => [1,2,3] }),
	    #{ "a" := _ } = M0,
	    M0#{ "a" := b }
    end,

    #{ "a" := b } = F(),

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

    % Can't handle directly yet
    Bin = <<0:257>>,
    #{ Bin := val } = id(M0#{<<0:257>> => val}), %% binary limitation

    %% Errors cases.
    BadMap = id(badmap),
    {'EXIT',{badarg,_}} = (catch BadMap#{nonexisting=>val}),
    {'EXIT',{badarg,_}} = (catch <<>>#{nonexisting=>val}),
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
    {'EXIT',{badarg,_}} = (catch ((id(nil))#{ a := b })),
    {'EXIT',{badarg,_}} = (catch M0#{nonexisting:=val}),
    {'EXIT',{badarg,_}} = (catch M0#{1.0:=v,1.0=>v2}),
    {'EXIT',{badarg,_}} = (catch M0#{42.0:=v,42:=v2}),
    {'EXIT',{badarg,_}} = (catch M0#{42=>v1,42.0:=v2,42:=v3}),
    {'EXIT',{badarg,_}} = (catch <<>>#{nonexisting:=val}),
    {'EXIT',{badarg,_}} = (catch M0#{<<0:257>> := val}), %% limitation
    ok.

t_update_values(Config) when is_list(Config) ->
    V0 = id(1337),
    M0 = #{ a => 1, val => V0},
    V1 = get_val(M0),
    M1 = id(M0#{ val := [V0,V1], "wazzup" => 42 }),
    [1337, {some_val, 1337}] = get_val(M1),
    M2 = id(M1#{ <<42:333>> => 1337 }),
    {bin_key,1337} = get_val(M2),

    N = 110,
    List = [{[I,1,2,3,I],{1,2,3,"wat",I}}|| I <- lists:seq(1,N)],

    {_,_,#{val2 := {1,2,3,"wat",N}, val1 := [N,1,2,3,N]}} = lists:foldl(fun
	    ({V2,V3},{Old2,Old3,Mi}) ->
		ok = check_val(Mi,Old2,Old3),
		#{ val1 := Old2, val2 := Old3 } = Mi,
		{V2,V3, Mi#{ val1 := id(V2), val2 := V1, val2 => id(V3)}}
	end, {none, none, #{val1=>none,val2=>none}},List),
    ok.

t_expand_map_update(Config) when is_list(Config) ->
    M = #{<<"hello">> => <<"world">>}#{<<"hello">> := <<"les gens">>},
    #{<<"hello">> := <<"les gens">>} = M,
    ok.

t_export(Config) when is_list(Config) ->
    Raclette = id(#{}),
    case brie of brie -> Fromage = Raclette end,
    Raclette = Fromage#{},
    ok.

check_val(#{val1:=V1, val2:=V2},V1,V2) -> ok.

get_val(#{ <<42:333>> := V }) -> {bin_key, V};
get_val(#{ "wazzup" := _, val := V}) -> V;
get_val(#{ val := V }) -> {some_val, V}.

t_guard_bifs(Config) when is_list(Config) ->
    true   = map_guard_empty(),
    true   = map_guard_empty_2(),
    true   = map_guard_head(#{a=>1}),
    false  = map_guard_head([]),
    true   = map_guard_body(#{a=>1}),
    false  = map_guard_body({}),
    true   = map_guard_pattern(#{a=>1, <<"hi">> => "hi" }),
    false  = map_guard_pattern("list"),
    true   = map_guard_tautology(),
    true   = map_guard_ill_map_size(),
    ok.

map_guard_empty() when is_map(#{}); false -> true.

map_guard_empty_2() when true; #{} andalso false -> true.

map_guard_head(M) when is_map(M) -> true;
map_guard_head(_) -> false.

map_guard_body(M) -> is_map(M).

map_guard_pattern(#{}) -> true;
map_guard_pattern(_)   -> false.

map_guard_tautology() when #{} =:= #{}; true -> true.

map_guard_ill_map_size() when true; map_size(0) -> true.

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
    third  = map_guard_update(#{x=>old,y=>old}, #{x=>third,y=>old}),
    ok.

map_guard_update(M1, M2) when M1#{x=>first}  =:= M2 -> first;
map_guard_update(M1, M2) when M1#{x=>second} =:= M2 -> second;
map_guard_update(M1, M2) when M1#{x:=third}  =:= M2 -> third;
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
    [#{k:=1},#{k:=2},#{k:=3}] = id([#{k=>I} || I <- [1,2,3]]),
    Ls = id([#{<<2:301>> => I, "wat" => I + 1} || I <- [1,2,3]]),
    [#{<<2:301>>:=1,"wat":=2},#{<<2:301>>:=2,"wat":=3},#{<<2:301>>:=3,"wat":=4}] = Ls,
    [{1,2},{2,3},{3,4}] = id([{I2,I1} || #{"wat" := I1, <<2:301>> := I2} <- Ls]),
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
    case (catch F1(#{s=>none,v=>none})) of
	{'EXIT', {function_clause,[{?MODULE,_,[#{s:=none,v:=none}],_}|_]}} -> ok;
	{'EXIT', {{case_clause,_},_}} -> {comment,inlined};
	Other ->
	    test_server:fail({no_match, Other})
    end.


t_map_sort_literals(Config) when is_list(Config) ->
    % test relation

    %% size order
    true  = #{ a => 1, b => 2} < id(#{ a => 1, b => 1, c => 1}),
    true  = #{ b => 1, a => 1} < id(#{ c => 1, a => 1, b => 1}),
    false = #{ c => 1, b => 1, a => 1} < id(#{ c => 1, a => 1}),

    %% key order
    true  = id(#{ a => 1 }) < id(#{ b => 1}),
    false = id(#{ b => 1 }) < id(#{ a => 1}),
    true  = id(#{ a => 1, b => 1, c => 1 }) < id(#{ b => 1, c => 1, d => 1}),
    true  = id(#{ b => 1, c => 1, d => 1 }) > id(#{ a => 1, b => 1, c => 1}),
    true  = id(#{ c => 1, b => 1, a => 1 }) < id(#{ b => 1, c => 1, d => 1}),
    true  = id(#{ "a" => 1 }) < id(#{ <<"a">> => 1}),
    false = id(#{ <<"a">> => 1 }) < id(#{ "a" => 1}),
    false = id(#{ 1 => 1 }) < id(#{ 1.0 => 1}),
    false = id(#{ 1.0 => 1 }) < id(#{ 1 => 1}),

    %% value order
    true  = id(#{ a => 1 }) < id(#{ a => 2}),
    false = id(#{ a => 2 }) < id(#{ a => 1}),
    false = id(#{ a => 2, b => 1 }) < id(#{ a => 1, b => 3}),
    true  = id(#{ a => 1, b => 1 }) < id(#{ a => 1, b => 3}),

    true  = id(#{ "a" => "hi", b => 134 }) == id(#{ b => 134,"a" => "hi"}),

    %% lists:sort

    SortVs = [#{"a"=>1},#{a=>2},#{1=>3},#{<<"a">>=>4}],
    [#{1:=ok},#{a:=ok},#{"a":=ok},#{<<"a">>:=ok}] = lists:sort([#{"a"=>ok},#{a=>ok},#{1=>ok},#{<<"a">>=>ok}]),
    [#{1:=3},#{a:=2},#{"a":=1},#{<<"a">>:=4}] = lists:sort(SortVs),
    [#{1:=3},#{a:=2},#{"a":=1},#{<<"a">>:=4}] = lists:sort(lists:reverse(SortVs)),

    ok.

t_warn_pair_key_overloaded(Config) when is_list(Config) ->
    #{ "hi1" := 42 } = id(#{ "hi1" => 1, "hi1" => 42 }),

    #{ "hi1" := 1337, "hi2" := [2], "hi3" := 3 } = id(#{
	    "hi1" => erlang:atom_to_binary(?MODULE,utf8),
	    "hi1" => erlang:binary_to_atom(<<"wazzup">>,utf8),
	    "hi1" => erlang:binary_to_float(<<"3.1416">>),
	    "hi1" => erlang:float_to_binary(3.1416),
	    "hi2" => erlang:pid_to_list(self()),
	    "hi3" => erlang:float_to_binary(3.1416),
	    "hi2" => lists:subtract([1,2],[1]),
	    "hi3" => +3,
	    "hi1" => erlang:min(1,2),
	    "hi1" => erlang:hash({1,2},35),
	    "hi1" => erlang:phash({1,2},33),
	    "hi1" => erlang:phash2({1,2},34),
	    "hi1" => erlang:integer_to_binary(1337),
	    "hi1" => erlang:binary_to_integer(<<"1337">>),
	    "hi4" => erlang:float_to_binary(3.1416)
	}),
    ok.

t_warn_useless_build(Config) when is_list(Config) ->
    [#{ a => id(I)} || I <- [1,2,3]],
    ok.

t_build_and_match_over_alloc(Config) when is_list(Config) ->
    Ls = id([1,2,3]),
    V0 = [a|Ls],
    M0 = id(#{ "a" => V0 }),
    #{ "a" := V1 } = M0,
    V2 = id([c|Ls]),
    M2 = id(#{ "a" => V2 }),
    #{ "a" := V3 } = M2,
    {[a,1,2,3],[c,1,2,3]} = id({V1,V3}),
    ok.

t_build_and_match_empty_val(Config) when is_list(Config) ->
    F = fun(#{ "hi":=_,{1,2}:=_,1337:=_}) -> ok end,
    ok = F(id(#{"hi"=>ok,{1,2}=>ok,1337=>ok})),

    %% error case
    case (catch (F(id(#{"hi"=>ok})))) of
	{'EXIT',{function_clause,_}} -> ok;
	{'EXIT', {{case_clause,_},_}} -> {comment,inlined};
	Other ->
	    test_server:fail({no_match, Other})
    end.

t_build_and_match_val(Config) when is_list(Config) ->
    F = fun
	(#{ "hi" := first,  v := V}) -> {1,V};
	(#{ "hi" := second, v := V}) -> {2,V}
    end,


    {1,"hello"}  = F(id(#{"hi"=>first,v=>"hello"})),
    {2,"second"} = F(id(#{"hi"=>second,v=>"second"})),

    %% error case
    case (catch (F(id(#{"hi"=>ok})))) of
	{'EXIT',{function_clause,_}} -> ok;
	{'EXIT', {{case_clause,_},_}} -> {comment,inlined};
	Other ->
	    test_server:fail({no_match, Other})
    end.

t_build_and_match_nil(Config) when is_list(Config) ->
    %% literals removed the coverage
    V1 = id(cookie),
    V2 = id(cake),
    V3 = id(crisps),

    #{ [] := V1, "treat" := V2, {your,treat} := V3 } = id(#{
	    {your,treat} => V3,
	    "treat" => V2, 
	    [] => V1 }),
    #{ [] := V3, [] := V3 } = id(#{ [] => V1, [] => V3 }),
    ok.

t_build_and_match_structure(Config) when is_list(Config) ->
    V2 = id("it"),
    S = id([42,{"hi", "=)", #{ "a" => 42, any => any, val => "get_" ++ V2}}]),

    %% match deep map values
    V2 = case S of
	[42,{"hi",_, #{ "a" := 42, val := "get_" ++ V1, any := _ }}] -> V1
    end,
    %% match deep map
    ok = case S of
	[42,{"hi",_, #{ }}] -> ok
    end,
    ok.

%% simple build and match variables
t_build_and_match_variables(Config) when is_list(Config) ->
    K0 = id(#{}),
    K1 = id(1),       V1 = id(a),
    K2 = id(2),       V2 = id(b),
    K3 = id(3),       V3 = id("c"),
    K4 = id("4"),     V4 = id("d"),
    K5 = id(<<"5">>), V5 = id(<<"e">>),
    K6 = id({"6",7}), V6 = id("f"),
    K7 = id(#{ "a" => 3 }),
    #{K1:=V1} = id(#{K1=>V1}),
    #{K1:=V1,K2:=V2} = id(#{K1=>V1,K2=>V2}),
    #{K1:=V1,K2:=V2,K3:=V3} = id(#{K1=>V1,K2=>V2,K3=>V3}),
    #{K1:=V1,K2:=V2,K3:=V3,K4:=V4} = id(#{K1=>V1,K2=>V2,K3=>V3,K4=>V4}),
    #{K1:=V1,K2:=V2,K3:=V3,K4:=V4,K5:=V5} = id(#{K1=>V1,K2=>V2,K3=>V3,K4=>V4,K5=>V5}),
    #{K1:=V1,K2:=V2,K3:=V3,K4:=V4,K5:=V5,K6:=V6} = id(#{K1=>V1,K2=>V2,K3=>V3,K4=>V4,K5=>V5,K6=>V6}),

    #{K5:=X,K5:=X=3,K4:=4} = id(#{K5=>3,K4=>4}),
    #{K5:=X,<<"5">>:=X=3,K4:=4} = id(#{K5=>3,K4=>4}),
    #{K5:=X,<<"5">>:=X=3,K4:=4} = id(#{<<"5">>=>3,K4=>4}),

    #{ K4:=#{ K3:=#{K1:=V1, K2:=V2}}, K5:=V5} =
	id(#{ K5=>V5, K4=>#{ K3=>#{K2 => V2, K1 => V1}}}),
    #{ K4 := #{ K5 := Res }, K6 := Res} = id(#{K4=>#{K5 => 99}, K6 => 99}),

    %% has keys
    #{a :=_,b :=_,K1:=_,K2:=_,K3:=V3,K4:=ResKey,K4:=ResKey,"4":=ResKey,"4":="ok"} =
	id(#{ a=>1, b=>1, K1=>V1, K2=>V2, K3=>V3, K4=>"nope", "4"=>"ok" }),

    %% function
    ok = match_function_map_neg_keys(#{ -1 => a, -2 => b, -3 => c }),

    %% map key
    #{ K0 := 42 } = id(#{ K0 => 42 }),
    #{ K7 := 42 } = id(#{ K7 => 42 }),

    %% nil key
    KNIL = id([]),
    #{KNIL:=ok,1:=2} = id(#{KNIL=>ok,1=>2}),

    Bin = <<0:258>>,
    #{ Bin := "three" } = id(#{<<0:258>> =>"three"}),

    %% error case
    {'EXIT',{{badmatch,_},_}} = (catch (#{K5:=3,x:=2} = id(#{K5=>3}))),
    {'EXIT',{{badmatch,_},_}} = (catch (#{K5:=2} = id(#{K5=>3}))),
    {'EXIT',{{badmatch,_},_}} = (catch (#{K5:=3} = id({a,b,c}))),
    {'EXIT',{{badmatch,_},_}} = (catch (#{K5:=3} = id(#{K6=>3}))),
    {'EXIT',{{badmatch,_},_}} = (catch (#{K5:=3} = id(K7))),
    {'EXIT',{{badmatch,_},_}} = (catch (#{K7:=3} = id(#{K7=>42}))),
    ok.


match_function_map_neg_keys(#{ -1 := a, -2 := b, -3 := c }) -> ok.

t_update_assoc_variables(Config) when is_list(Config) ->
    K1 = id(1),
    K2 = id(2),
    K3 = id(3.0),
    K4 = id(4),
    K5 = id(5),
    K6 = id(2.0),

    M0 = #{K1=>a,K2=>b,K3=>c,K4=>d,K5=>e},

    M1 = M0#{K1=>42,K2=>100,K4=>[a,b,c]},
    #{1:=42,2:=100,3.0:=c,4:=[a,b,c],5:=e} = M1,
    #{1:=42,2:=b,4:=d,5:=e,2.0:=100,K3:=c,4.0:=[a,b,c]} = M0#{1.0=>float,1:=42,2.0=>wrong,K6=>100,4.0=>[a,b,c]},

    M2 = M0#{K3=>new},
    #{1:=a,2:=b,K3:=new,4:=d,5:=e} = M2,
    M2 = M0#{3.0:=wrong,K3=>new},

    #{ <<0:258>> := val } = id(M0#{<<0:258>> => val}), %% binary limitation

    %% Errors cases.
    BadMap = id(badmap),
    {'EXIT',{badarg,_}} = (catch BadMap#{nonexisting=>val}),
    {'EXIT',{badarg,_}} = (catch <<>>#{nonexisting=>val}),
    ok.

t_update_exact_variables(Config) when is_list(Config) ->
    K1 = id(1),
    K2 = id(2),
    K3 = id(3.0),
    K4 = id(4),

    M0 = id(#{1=>a,2=>b,3.0=>c,4=>d,5=>e}),

    M1 = M0#{K1:=42,K2:=100,K4:=[a,b,c]},
    #{1:=42,2:=100,3.0:=c,K4:=[a,b,c],5:=e} = M1,
    M1 = M0#{K1:=wrong,1:=also_wrong,K1=>42,2=>wrong,K2:=100,4:=[a,b,c]},

    M2 = M0#{K3:=new},
    #{1:=a,K2:=b,3.0:=new,K4:=d,5:=e} = M2,
    M2 = M0#{3.0=>wrong,K3:=new},
    true = M2 =/= M0#{3=>right,3.0:=new},
    #{ 3 := right, 3.0 := new } = M0#{3=>right,K3:=new},

    M3 = id(#{ 1 => val}),
    #{1 := update2,1.0 := new_val4} = M3#{
	1.0 => new_val1, K1 := update, K1=> update3,
	K1 := update2, 1.0 := new_val2, 1.0 => new_val3,
	1.0 => new_val4 },

    #{ "wat" := 3, 2 := a } = id(#{ "wat" => 1, K2 => 2 }#{ K2 := a, "wat" := 3 }),

    %% Errors cases.
    {'EXIT',{badarg,_}} = (catch ((id(nil))#{ a := b })),
    {'EXIT',{badarg,_}} = (catch M0#{nonexisting:=val}),
    {'EXIT',{badarg,_}} = (catch M0#{1.0:=v,1.0=>v2}),
    {'EXIT',{badarg,_}} = (catch M0#{42.0:=v,42:=v2}),
    {'EXIT',{badarg,_}} = (catch M0#{42=>v1,42.0:=v2,42:=v3}),
    {'EXIT',{badarg,_}} = (catch <<>>#{nonexisting:=val}),
    {'EXIT',{badarg,_}} = (catch M0#{<<0:257>> := val}), %% limitation
    ok.

t_nested_pattern_expressions(Config) when is_list(Config) ->
    K1 = id("hello"),
    %K2 = id({ok}),
    [_,_,#{ <<"hi">> := wat, K1 := 42 }|_] = id([k,k,#{<<"hi">> => wat, K1 => 42}]),
    [_,_,#{ -1 := wat, K1 := 42 }|_] = id([k,k,#{-1 => wat, K1 => 42}]),
    [_,_,{#{ -1 := #{ {-3,<<0:300>>} := V1 }, K1 := 42 },3}|_] = id([k,k,{#{-1 => #{{-3,<<0:300>>}=>"hi"}, K1 => 42},3}]),
    "hi" = V1,
    %[k,#{ {-1,K1,[]} := {wat,K1}, K2 := 42 }|_] = id([k,#{{-1,K1,[]} => {wat,K1}, K2 => 42}]),
    %[k,#{ [-1,K2,[]] := {wat,K1}, K1 := 42 }|_] = id([k,#{[-1,K2,[]] => {wat,K1}, K1 => 42}]),

    M0 = id(#{ <<33:333>> => 1, <<332:333>> => ok, a => ok, wat => yep, watzor => ok }),
    F0 = map_nested_pattern_funs(M0),
    F1 = F0(wat),
    F2 = F1(watzor),
    {yep,ok} = F2(M0),
    ok.

map_nested_pattern_funs(M) ->
    K0 = id(a),
    fun(K1) ->
	    case M of
		#{ K0 := ok, K1 := yep, <<33:333>> := 1 } ->
		    fun(K2) ->
			    case M of
				#{ K2 := ok, K1 := yep, <<33:333>> := 1 } ->
				    fun
					(#{ <<332:333>> := ok, K1 := V1, K2 := V2 }) ->
					    {V1,V2}
				    end
			    end
		    end
	    end
    end.

t_guard_update_variables(Config) when is_list(Config) ->
    error  = map_guard_update_variables(n,#{},#{}),
    first  = map_guard_update_variables(x,#{}, #{x=>first}),
    second = map_guard_update_variables(x,#{y=>old}, #{x=>second,y=>old}),
    third  = map_guard_update_variables(x,#{x=>old,y=>old}, #{x=>third,y=>old}),
    fourth = map_guard_update_variables(x,#{x=>old,y=>old}, #{x=>4,y=>new}),
    ok.

map_guard_update_variables(K,M1,M2) when M1#{K=>first}    =:= M2 -> first;
map_guard_update_variables(K,M1,M2) when M1#{K=>second}   =:= M2 -> second;
map_guard_update_variables(K,M1,M2) when M1#{K:=third}    =:= M2 -> third;
map_guard_update_variables(K,M1,M2) when M1#{K:=4,y=>new} =:= M2 -> fourth;
map_guard_update_variables(_,_,_) -> error.

t_guard_sequence_variables(Config) when is_list(Config) ->
    {1,"a"} = map_guard_sequence_var_1(a,#{seq=>1,a=>id("a"),b=>no}),
    {2,"b"} = map_guard_sequence_var_1(b,#{seq=>2,b=>id("b"),a=>no}),
    {3,"c"} = map_guard_sequence_var_1(a,#{seq=>3,a=>id("c"),b=>no}),
    {4,"d"} = map_guard_sequence_var_1(b,#{seq=>4,b=>id("d"),a=>no}),
    {4,4}   = map_guard_sequence_var_1(seq,#{seq=>4}),
    {4,4,y} = map_guard_sequence_var_1(seq,#{seq=>4,b=>id("d"),a=>y}),
    {5,"d"} = map_guard_sequence_var_1(b,#{seq=>5,b=>id("d"),a=>y}),

    %% error case
    {'EXIT',{{case_clause,_},_}} = (catch map_guard_sequence_var_1("a",#{seq=>4,val=>id("e")})),
    ok.


map_guard_sequence_var_1(K,M) ->
    case M of
	#{seq:=1=Seq, K:=Val} -> {Seq,Val};
	#{seq:=2=Seq, K:=Val} -> {Seq,Val};
	#{seq:=3=Seq, K:=Val} -> {Seq,Val};
	#{K:=4=Seq,   K:=Val1,a:=Val2} -> {Seq,Val1,Val2};
	#{seq:=4=Seq, K:=Val} -> {Seq,Val};
	#{K:=4=Seq,   K:=Val} -> {Seq,Val};
	#{seq:=5=Seq, K:=Val} -> {Seq,Val}
    end.


t_guard_sequence_mixed(Config) when is_list(Config) ->
    M0 = id(#{ a=>1, b=>1, c=>1, d=>1, e=>1, f=>1, g=>1, h=>1 }),
    M1 = id(M0#{ d := 3 }),
    1 = map_guard_sequence_mixed(a,d,M1),
    M2 = id(M1#{ b := 2, d := 4, h := 2 }),
    2 = map_guard_sequence_mixed(a,d,M2),
    M3 = id(M2#{ b := 3, e := 5, g := 3 }),
    3 = map_guard_sequence_mixed(a,e,M3),
    M4 = id(M3#{ c := 4, e := 6, h := 1 }),
    4 = map_guard_sequence_mixed(a,e,M4),
    M5 = id(M4#{ c := 5, f := 7, g := 2 }),
    5 = map_guard_sequence_mixed(a,f,M5),
    M6 = id(M5#{ c := 6, f := 8, h := 3 }),
    6 = map_guard_sequence_mixed(a,f,M6),

    %% error case
    {'EXIT',{{case_clause,_},_}} = (catch map_guard_sequence_mixed(a,b,M0)),
    ok.

map_guard_sequence_mixed(K1,K2,M) ->
    case M of
	#{ K1 := 1, b := 1, K2 := 3, g := 1} -> 1;
	#{ K1 := 1, b := 2, K2 := 4, h := 2} -> 2;
	#{ K1 := 1, b := 3, K2 := 5, g := 3} -> 3;
	#{ K1 := 1, c := 4, K2 := 6, h := 1} -> 4;
	#{ K1 := 1, c := 5, K2 := 7, g := 2} -> 5;
	#{ K1 := 1, c := 6, K2 := 8, h := 3} -> 6
    end.



t_frequency_table(Config) when is_list(Config) ->
    random:seed({13,1337,54}),  % pseudo random
    N = 100000,
    Ts = rand_terms(N),
    #{ n:=N, tf := Tf } = frequency_table(Ts,#{ n=>0, tf => #{}}),
    ok = check_frequency(Ts,Tf),
    ok.


frequency_table([T|Ts], M) ->
    case M of
	#{ n := N, tf := #{ T := C } = F } ->
	    frequency_table(Ts,M#{ n := N + 1, tf := F#{ T := C + 1 }});
	#{ n := N, tf := F } ->
	    frequency_table(Ts,M#{ n := N + 1, tf := F#{ T => 1 }})
    end;
frequency_table([], M) -> M.


check_frequency(Ts,Tf) ->
    check_frequency(Ts,Tf,dict:new()).

check_frequency([T|Ts],Tf,D) ->
    case dict:find(T,D) of
	error  -> check_frequency(Ts,Tf,dict:store(T,1,D));
	{ok,C} -> check_frequency(Ts,Tf,dict:store(T,C+1,D))
    end;
check_frequency([],Tf,D) ->
    validate_frequency(dict:to_list(D),Tf).

validate_frequency([{T,C}|Fs],Tf) ->
    case Tf of
	#{ T := C } -> validate_frequency(Fs,Tf);
	_ -> error
    end;
validate_frequency([], _) -> ok.


%% aux

rand_terms(0) -> [];
rand_terms(N) -> [rand_term()|rand_terms(N-1)].

rand_term() ->
    case random:uniform(6) of
	1 -> rand_binary();
	2 -> rand_number();
	3 -> rand_atom();
	4 -> rand_tuple();
	5 -> rand_list();
	6 -> rand_map()
    end.

rand_binary() ->
    case random:uniform(3) of
	1 -> <<>>;
	2 -> <<"hi">>;
	3 -> <<"message text larger than 64 bytes. yep, message text larger than 64 bytes.">>
    end.

rand_number() ->
    case random:uniform(3) of
	1 -> random:uniform(5);
	2 -> float(random:uniform(5));
	3 -> 1 bsl (63 + random:uniform(3))
    end.

rand_atom() ->
    case random:uniform(3) of
	1 -> hi;
	2 -> some_atom;
	3 -> some_other_atom
    end.


rand_tuple() ->
    case random:uniform(3) of
	1 -> {ok, rand_term()}; % careful
	2 -> {1, 2, 3};
	3 -> {<<"yep">>, 1337}
    end.

rand_list() ->
    case random:uniform(3) of
	1 -> "hi";
	2 -> [1,rand_term()]; % careful
	3 -> [improper|list]
    end.

rand_map() ->
    case random:uniform(3) of
	1 -> #{ hi => 3 };
	2 -> #{ wat => rand_term(), other => 3 };  % careful
	3 -> #{ hi => 42, other => 42, yet_anoter => 1337 }
    end.



%% Use this function to avoid compile-time evaluation of an expression.
id(I) -> I.
