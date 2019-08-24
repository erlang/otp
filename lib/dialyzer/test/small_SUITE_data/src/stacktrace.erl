-module(stacktrace).

%% Check the stacktrace variable introduced in Erlang/OTP 21.0

-export([t1/0, t2/0, t3/0, t4/0, s1/0, s2/0, s3/0, s4/0]).

t1() ->
    try foo:bar()
    catch
        E:P:S ->
            {a,b} = S, % can never match
            {E, P}
    end.

t2() ->
    try foo:bar()
    catch
        E:P:S ->
            [a,b] = S, % can never match
            {E, P}
    end.

t3() ->
    try foo:bar()
    catch
        E:P:S ->
            [{m,f,[],[]}] = S,
            {E, P}
    end.

t4() ->
    try foo:bar()
    catch
        E:P:S ->
            [{m,f,1,[{file,"tjo"},{line,95}]}] = S,
            {E, P}
    end.

s1() ->
    try foo:bar()
    catch
        E:P ->
            S = erlang:get_stacktrace(),
            {a,b} = S, % can never match
            {E, P}
    end.

s2() ->
    try foo:bar()
    catch
        E:P ->
            S = erlang:get_stacktrace(),
            [a,b] = S, % can never match
            {E, P}
    end.

s3() ->
    try foo:bar()
    catch
        E:P ->
            S = erlang:get_stacktrace(),
            [{m,f,[],[]}] = S,
            {E, P}
    end.

s4() ->
    try foo:bar()
    catch
        E:P ->
            S = erlang:get_stacktrace(),
            [{m,f,1,[{file,"tjo"},{line,95}]}] = S,
            {E, P}
    end.
