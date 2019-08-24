-module(maps_warn_pair_key_overloaded).
-export([test/0]).

test() ->
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
	    "hi1" => erlang:phash({1,2},33),
	    "hi1" => erlang:phash2({1,2},34),
	    "hi1" => erlang:integer_to_binary(1337),
	    "hi1" => erlang:binary_to_integer(<<"1337">>),
	    "hi4" => erlang:float_to_binary(3.1416)
	}),
    ok.

%% Use this function to avoid compile-time evaluation of an expression.
id(I) -> I.
