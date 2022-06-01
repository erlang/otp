-module(f_include_3).

-feature(unless_expr, enable).

-ifdef(end_prefix).
-record(constant, {value = 42}).
-endif.

-ifdef(end_include).
-feature(maybe_expr, enable).
-endif.

-include("enable.hrl").

-ifdef(end_include).
-feature(ifnot_expr, enable).
-endif.

%% At this point the prefix will definitely end, if it has not already
-export([foo/1]).

foo(1) -> one.
