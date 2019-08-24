-module(maps_update_map_expressions).
-export([test/0]).

test() ->
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

    %% Error cases.
    {'EXIT',{{badmap,<<>>},_}} = (catch (id(<<>>))#{ a := 42, b => 2 }),
    {'EXIT',{{badmap,[]},_}} = (catch (id([]))#{ a := 42, b => 2 }),
    ok.

%% Use this function to avoid compile-time evaluation of an expression.
id(I) -> I.
