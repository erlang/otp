-module(f_include_1).

-feature(experimental_ftr_1, enable).

-ifdef(end_include).
-feature(experimental_ftr_2, enable).
-endif.

%% This defines a record, so ends the prefix.
-include("is_enabled.hrl").

-ifdef(end_include).
-feature(approved_ftr_1, disable).
-endif.

-export([foo/1]).

foo(1) -> one.
