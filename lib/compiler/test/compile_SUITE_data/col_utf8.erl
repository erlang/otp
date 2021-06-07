-module(col_utf8).
-export([bar/0]).
bar() ->
    %% the below line contains both TAB and SPC to check error column
    B = <<"xyzåäö">>,	<<"12345">>,
    B.
