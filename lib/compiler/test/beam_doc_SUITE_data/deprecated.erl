-module(deprecated).

-export([test/0, test/1, test/2]).
-export_type([test/0, test/1]).

-ifndef(REASON).
-define(REASON,"Deprecation reason").
-endif.

-ifndef(TREASON).
-define(TREASON,"Deprecation reason").
-endif.

-doc #{ deprecated => "Meta reason" }.
-callback test() -> ok.

-ifdef(TEST_WILDCARD).
-deprecated({test, '_', ?REASON}).
-else.
-ifdef(ALL_WILDCARD).
-deprecated({'_', '_', ?REASON}).
-else.
-deprecated({test, 1, ?REASON}).
-endif.
-endif.
-deprecated({test, 0}).

-ifdef(TEST_WILDCARD).
-deprecated_type({test, '_'}).
-else.
-ifdef(ALL_WILDCARD).
-deprecated_type({'_', '_', ?TREASON}).
-else.
-deprecated_type({test, 1, ?TREASON}).
-endif.
-endif.
-deprecated_type({test, 0}).
-type test() :: ok.
-type test(N) :: N.

test() -> ok.
test(N) -> N.

-doc #{ deprecated => "Meta reason" }.
test(N,M) -> N + M.
     
