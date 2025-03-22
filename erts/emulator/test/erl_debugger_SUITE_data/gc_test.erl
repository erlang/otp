-module(gc_test).
-export([go/1]).

%% NB. If N is high enough, we will eventually need to do a GC to
%% grow the stack. Since we will set a breakpoint before the
%% recursive call, we will be forced to the GC while processing
%% the breakpoint
go({max_recursion_depth, 0}) ->
    1;
go({max_recursion_depth, N}) ->
    N_1 = N-1,
    Acc = ?MODULE:go({max_recursion_depth, N_1}),
    Acc + 1.
