-module(f_include_3).

-feature(experimental_ftr_1, enable).

-ifdef(end_prefix).
-record(constant, {value = 42}).
-endif.

%% Enable feature inside include file
-include("enable.hrl").

%% At this point the prefix will definitely end, if it has not already
-export([foo/1]).

-if(?FEATURE_ENABLED(experimental_ftr_2)).
foo(1) ->
    exp2_enabled.
-else.
foo(1) ->
    exp2_disabled.
-endif.
