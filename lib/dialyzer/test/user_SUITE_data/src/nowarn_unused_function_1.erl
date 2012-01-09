%% Test that option 'nowarn_unused_funcion' works similarly in
%% Dialyzer as in the compiler.

-module(nowarn_unused_function_1).

-compile(warn_unused_function).

-compile({nowarn_unused_function,f1/1}).
f1(_) ->
    a.

-compile({nowarn_unused_function,[{f2,1}]}).
f2(_) ->
    a.

-compile({warn_unused_function,[{f3,1}]}).
f3(_) ->
    a.
