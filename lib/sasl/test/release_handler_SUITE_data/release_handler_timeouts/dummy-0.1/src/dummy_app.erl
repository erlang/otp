-module(dummy_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_,_) ->
    dummy_sup:start_link().

stop(_) -> ok.
