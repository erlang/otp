-module(gh_6518).
-export([f/1]).

f(X) ->
    case
        fun() ->
                [ok || _ <- []]
        end
    of
        _ ->
            (Y = X)
    end andalso Y.
