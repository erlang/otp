-module(warn_missing_doc).

-export([test/0, test/1, test/2]).
-export_type([test/0, test/1]).

-type test() :: ok.
-type test(N) :: N.

-callback test() -> ok.

-include("warn_missing_doc.hrl").

test() -> ok.
test(N) -> N.
