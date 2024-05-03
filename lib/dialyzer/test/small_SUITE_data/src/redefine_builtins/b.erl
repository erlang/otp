-module(b).
-export_type([integer/0, collection/0]).

-type integer() :: [integer()].

-type collection() :: {erlang:integer(), integer()}.
