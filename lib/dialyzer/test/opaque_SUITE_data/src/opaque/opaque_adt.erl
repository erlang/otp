-module(opaque_adt).
-export([atom_or_list/1]).

-opaque abc() :: 'a' | 'b' | 'c'.

-spec atom_or_list(_) -> abc() | list().

atom_or_list(1) -> a;
atom_or_list(2) -> b;
atom_or_list(3) -> c;
atom_or_list(N) -> lists:duplicate(N, a).
