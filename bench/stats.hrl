%%================================================================
%% Statistics data structures
%%================================================================

-record(range, {min, max}).

-record(stat, {range,
               median,
               average,
               stddev}).
