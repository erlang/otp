-module(trycatch_3).
-export([three/1]).

three({error,Term}) ->
    error(Term);
three({throw,Term}) ->
    throw(Term);
three({exit,Term}) ->
    exit(Term).
