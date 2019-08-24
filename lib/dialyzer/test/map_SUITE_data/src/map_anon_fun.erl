-module(map_anon_fun).

%% Not exported.
g(A) ->
    maps:map(fun F(K, {V, _C}) ->
                    F(K, V);
                 F(_K, _V) ->
                    #{ system => {A} }
            end, #{}).
