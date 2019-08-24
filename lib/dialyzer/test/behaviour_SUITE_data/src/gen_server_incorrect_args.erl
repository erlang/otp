-module(gen_server_incorrect_args).

-behaviour(gen_server).

-export([handle_call/3]).

handle_call(_Request, From, _State) ->
    case From of
	'boo' -> {'ok'};
	'foo' -> {'no'}
    end.
