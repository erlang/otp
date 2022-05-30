%% This will fail when experimental_ftr_1 is not enabled
%% or end the prefix when it is enabled
-record(random, {ifn = 42, unless = 17}).
