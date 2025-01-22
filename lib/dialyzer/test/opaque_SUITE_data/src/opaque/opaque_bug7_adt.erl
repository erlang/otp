-module(opaque_bug7_adt).
-export([do/1]).

-export_type([adt/0]).
-opaque adt() :: {a, term()}.

-spec do(adt()) -> ok | {error, atom()}.
do({Mod, Arg}) -> Mod:foo(Arg).
