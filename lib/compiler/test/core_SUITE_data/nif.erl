-module(nif).

-export([init/1, start/1, bug0/1]).

-ifdef(WITH_ATTRIBUTE).
-nifs([start/1]).
-endif.

-ifdef(WITH_LOAD_NIF).
init(File) ->
    ok = erlang:load_nif(File, 0).
-else.
init(_File) ->
    ok.
-endif.

start(_) -> erlang:nif_error(not_loaded).

%% This used to crash the compiler in the v3_core pass as
%% insert_nif_start/1 did not support letrecs.
bug0(<<HL:32/signed-integer-big-unit:1, _:HL/binary, _/binary>>) ->
    <<>>.
