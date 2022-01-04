%% This will fail when unless_expr is not enabled
%% or end the prefix when it is enabled
-record(random, {unless = 42, maybe = 17}).
