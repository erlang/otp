-module(maps_map_sort_literals).
-export([test/0]).

test() ->
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
    true  = #{ 1 => 1 } < id(#{ 1.0 => 1}),
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

%% Use this function to avoid compile-time evaluation of an expression.
id(I) -> I.
