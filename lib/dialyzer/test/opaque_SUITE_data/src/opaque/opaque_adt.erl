-module(opaque_adt).
-export([atom_or_list/1, line/1, location/1]).

-export_type([anno/0]).

-type annotation() :: {'location', location()} | {'text', string()}.
-nominal column() :: pos_integer().
-nominal line() :: non_neg_integer().
-nominal location() :: line() | {line(), column()}.
-opaque anno() :: location() | [annotation(), ...].

-opaque abc() :: 'a' | 'b' | 'c'.

-spec atom_or_list(_) -> abc() | list().

atom_or_list(1) -> a;
atom_or_list(2) -> b;
atom_or_list(3) -> c;
atom_or_list(N) -> lists:duplicate(N, a).

-spec line(Anno) -> line() when
      Anno :: anno().
line(Anno) ->
    case location(Anno) of
        {Line, _Column} ->
            Line;
        Line ->
            Line
    end.

-spec location(Anno) -> location() when
      Anno :: anno().

location(Line) when is_integer(Line) ->
    Line;
location({Line, Column}=Location) when is_integer(Line), is_integer(Column) ->
    Location;
location(Anno) ->
    ext:ernal(Anno, location).
