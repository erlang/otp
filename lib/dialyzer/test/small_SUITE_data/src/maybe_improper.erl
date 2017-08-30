%%%========================================================================
%%% Tests handling of maybe improper lists
%%%========================================================================
-module(maybe_improper).

-export([s/1, t/0]).

-spec s(maybe_improper_list(X,Y)) -> {[X], maybe_improper_list(X,Y)}.
s(L) ->
    lists:split(2, L).

%% Having a remote type in the 'tail' of the list crashed dialyzer.
%% The problem was fixed for R16B03.
-type t_mil() :: maybe_improper_list(integer(), orddict:orddict()).

-spec t() -> t_mil().
t() ->
    [42 | []].
