%% -*- Mode: erlang; indent-tabs-mode: nil -*-
%% Copyright Ericsson AB 2017. All Rights Reserved.

%%% Try and catch indentation is hard

%%% Not everything in these test are set in stone
%%% better indentation rules can be added but by having
%%% these tests we can see what changes in new implementations
%%% and notice when doing unintentional changes

try_catch() ->
    try
        io:format(stdout, "Parsing file ~s, ",
                  [St0#leex.xfile]),
        {ok,Line3,REAs,Actions,St3} =
            parse_rules(Xfile, Line2, Macs, St2)
    catch
        exit:{badarg,R} ->
            foo(R),
            io:format(stdout,
                      "ERROR reason ~p~n",
                      R);
        error:R
          when R =:= 42 ->			% when should be indented
            foo(R);
        error:R
          when					% when should be indented
              R =:= 42 ->			% but unsure about this (maybe 2 more)
            foo(R);
        error:R when
              R =:= foo ->			% line should be 2 indented (works)
            foo(R);
        error:R ->
            foo(R),
            io:format(stdout,
                      "ERROR reason ~p~n",
                      R)
    after
        foo('after'),
        file:close(Xfile)
    end;
try_catch() ->
    try
        foo(bar)
    of
        X when true andalso
               kalle ->
            io:format(stdout, "Parsing file ~s, ",
                      [St0#leex.xfile]),
            {ok,Line3,REAs,Actions,St3} =
                parse_rules(Xfile, Line2, Macs, St2);
        X
          when false andalso			% when should be 2 indented
               bengt ->
            gurka();
        X when
              false andalso			% line should be 2 indented
              not bengt ->
            gurka();
        X ->
            io:format(stdout, "Parsing file ~s, ",
                      [St0#leex.xfile]),
            {ok,Line3,REAs,Actions,St3} =
                parse_rules(Xfile, Line2, Macs, St2)
    catch
        exit:{badarg,R} ->
            foo(R),
            io:format(stdout,
                      "ERROR reason ~p~n",
                      R);
        error:R ->
            foo(R),
            io:format(stdout,
                      "ERROR reason ~p~n",
                      R)
    after
        foo('after'),
        file:close(Xfile),
        bar(with_long_arg,
            with_second_arg)
    end;
try_catch() ->
    try foo()
    after
        foo(),
        bar(with_long_arg,
            with_second_arg)
    end.

indent_catch() ->
    D = B +
        float(43.1),

    B = catch oskar(X),

    A = catch (baz +
                   bax),
    catch foo(),

    C = catch B +
        float(43.1),

    case catch foo(X) of
        A ->
            B
    end,

    case
        catch foo(X)
    of
        A ->
            B
    end,

    case
        foo(X)
    of
        A ->
            catch B,
            X
    end,

    try sune of
        _ -> foo
    catch _:_ -> baf
    end,

    Variable = try
                   sune
               of
                   _ ->
                       X = 5,
                       (catch foo(X)),
                       X + 10
               catch _:_ -> baf
               after cleanup()
               end,

    try
        (catch sune)
    of
        _ ->
            foo1(),
    catch foo()  %% BUGBUG can't handle catch inside try without parentheses
    catch _:_ ->
            baf
    end,

    try
        (catch exit())
    catch
        _ ->
            catch baf()
    end,
    ok.

%% this used to result in 2x the correct indentation within the function
%% body, due to the function name being mistaken for a keyword
catcher(N) ->
    try generate_exception(N) of
        Val -> {N, normal, Val}
    catch
        throw:X -> {N, caught, thrown, X};
        exit:X -> {N, caught, exited, X};
        error:X -> {N, caught, error, X}
    end.
