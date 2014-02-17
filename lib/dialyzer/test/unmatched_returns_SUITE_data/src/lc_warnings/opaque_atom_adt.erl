-module(opaque_atom_adt).
-export([atom/1]).

-opaque opaque_atom() :: atom().

-spec atom(atom()) -> opaque_atom().

atom(Atom) ->
    Atom.
