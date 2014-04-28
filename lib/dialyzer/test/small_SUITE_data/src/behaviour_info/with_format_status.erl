-module(with_format_status).

-behaviour(gen_server).
-export([handle_call/3,handle_cast/2,handle_info/2,
         code_change/3, init/1, terminate/2, format_status/2]).
-export([handle_call/3,handle_cast/2,handle_info/2]).
handle_call(_, _, S) -> {noreply, S}.
handle_cast(_, S) -> {noreply, S}.
handle_info(_, S) -> {noreply, S}.
code_change(_, _, _) -> {error, not_implemented}.
init(_) -> {ok, state}.
terminate(_, _) -> ok.
format_status(normal, _) -> ok. % optional callback
