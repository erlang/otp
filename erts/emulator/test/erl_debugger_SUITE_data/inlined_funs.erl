-module(inlined_funs).
-export([go/2]).

-compile({inline, [f/2]}).

-define(breadcrumb(Pid), breadcrumb(Pid, {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY}, ?LINE)).

go(Pid, X0) ->
    X1 = f(Pid, X0),
    X2 = f(Pid, X1),
    Pid ! {done, self(), X2}.

f(Pid, X) ->
    ?breadcrumb(Pid),
    X + 1.

breadcrumb(Pid, MFA, Line) ->
    Pid ! {executed, self(), MFA, {line, Line}}.
