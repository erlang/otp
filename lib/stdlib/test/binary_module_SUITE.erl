-module(binary_module_SUITE).

-export([all/1, interesting/1]).

-define(STANDALONE,1).

-ifdef(STANDALONE).

-define(line,erlang:display({?MODULE,?LINE}),).

-else.

-include("test_server.hrl").

-endif.



-ifdef(STANDALONE).
-export([run/0]).

run() ->
    [ apply(?MODULE,X,[[]]) || X <- all(suite) ].

-endif.

all(suite) -> [interesting].


interesting(doc) ->
    ["Try some interesting patterns"];
interesting(Config) when is_list(Config) ->
    X = do_interesting(binary),
    X = do_interesting(binref).

do_interesting(Module) ->
    ?line {0,4} = Module:match(<<"123456">>,
			 Module:compile_pattern([<<"12">>,<<"1234">>,
						 <<"23">>,<<"3">>,
						 <<"34">>,<<"456">>,
						 <<"45">>,<<"6">>])),
    ?line [{0,4},{5,1}] = Module:matches(<<"123456">>,
				   Module:compile_pattern([<<"12">>,<<"1234">>,
							   <<"23">>,<<"3">>,
							   <<"34">>,<<"456">>,
							   <<"45">>,<<"6">>])),
    ?line [{0,4}] = Module:matches(<<"123456">>,
			     Module:compile_pattern([<<"12">>,<<"1234">>,
						     <<"23">>,<<"3">>,
						     <<"34">>,<<"456">>,
						     <<"45">>])),
    ?line [{0,2},{2,2}] = Module:matches(<<"123456">>,
			     Module:compile_pattern([<<"12">>,
						     <<"23">>,<<"3">>,
						     <<"34">>,<<"456">>,
						     <<"45">>])),
    ?line {1,4} = Module:match(<<"123456">>,
			 Module:compile_pattern([<<"34">>,<<"34">>,
						 <<"12347">>,<<"2345">>])),
    ?line [{1,4}] = Module:matches(<<"123456">>,
			     Module:compile_pattern([<<"34">>,<<"34">>,
						     <<"12347">>,<<"2345">>])),
    ?line [{2,2}] = Module:matches(<<"123456">>,
			     Module:compile_pattern([<<"34">>,<<"34">>,
						     <<"12347">>,<<"2346">>])),

    ?line {0,4} = Module:match(<<"123456">>,
			 [<<"12">>,<<"1234">>,
			  <<"23">>,<<"3">>,
			  <<"34">>,<<"456">>,
			  <<"45">>,<<"6">>]),
    ?line [{0,4},{5,1}] = Module:matches(<<"123456">>,
				   [<<"12">>,<<"1234">>,
				    <<"23">>,<<"3">>,
				    <<"34">>,<<"456">>,
				    <<"45">>,<<"6">>]),
    ?line [{0,4}] = Module:matches(<<"123456">>,
			     [<<"12">>,<<"1234">>,
			      <<"23">>,<<"3">>,
			      <<"34">>,<<"456">>,
			      <<"45">>]),
    ?line [{0,2},{2,2}] = Module:matches(<<"123456">>,
					 [<<"12">>,
					  <<"23">>,<<"3">>,
					  <<"34">>,<<"456">>,
					  <<"45">>]),
    ?line {1,4} = Module:match(<<"123456">>,
			       [<<"34">>,<<"34">>,
				<<"12347">>,<<"2345">>]),
    ?line [{1,4}] = Module:matches(<<"123456">>,
				   [<<"34">>,<<"34">>,
				    <<"12347">>,<<"2345">>]),
    ?line [{2,2}] = Module:matches(<<"123456">>,
				   [<<"34">>,<<"34">>,
				    <<"12347">>,<<"2346">>]),
    ok.
