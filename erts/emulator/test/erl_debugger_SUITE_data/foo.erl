-module(foo).
-export([go/1]).

-define(breadcrumb(Pid), breadcrumb(Pid, {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY}, ?LINE)).

go(Pid) ->
    ?breadcrumb(Pid),
    ?breadcrumb(Pid),

    do_stuff(Pid),

    ?breadcrumb(Pid),

    Pid ! {done, self()},
    ok.

do_stuff(Pid) ->
    ?breadcrumb(Pid),
    ?breadcrumb(Pid),
    ok.

breadcrumb(Pid, MFA, Line) ->
    Pid ! {executed, self(), MFA, {line, Line}}.
