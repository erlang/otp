%% Test that option 'nowarn_unused_funcion' works similarly in
%% Dialyzer as in the compiler.

-module(nowarn_unused_function_3).

-compile({warn_unused_function,[{f1,1},{f2,1}]}).
-compile({nowarn_unused_function,[{f3,1}]}).

f1(_) ->
    a.

f2(_) ->
    a.

f3(_) ->
    a.
