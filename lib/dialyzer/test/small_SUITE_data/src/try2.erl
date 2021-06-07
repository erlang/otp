-module(try2).
-export([main/0, run/2, run2/2, run3/2]).

main() ->
    try A = foo() of
        _ -> A
    after ok
    end.

foo() -> 1.

run(Module, Args) ->
    try
        Module:main(Args),
        halt(0)
    catch
        Class:Reason:StackTrace ->
            format_exception(Class, Reason, StackTrace)
    end.

run2(Module, Args) ->
    try
        Result = Module:main(Args),
        ok
    of
        ok ->
            Result
    catch
        Class:Reason:StackTrace ->
            format_exception(Class, Reason, StackTrace)
    end.

run3(Module, Args) ->                           %Function run3/2 has no local return
    try
        Result = error(badarg),
        ok
    of
        ok ->
            Result
    catch
        Class:Reason:StackTrace ->
            format_exception(Class, Reason, StackTrace)
    end.

format_exception(Class, Reason, StackTrace) ->
    erlang:raise(Class, Reason, StackTrace).
