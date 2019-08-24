-module(test).

-export([bin_compr/0]).

bin_compr() ->
%    [ 0 || {N, V} <- [{a, b}] ]. % Works ok
    << <<>> || {A, B} <- [{a, b}] >>. % Complains
%    << <<>> || X <- [{a, b}] >>. % Works ok
