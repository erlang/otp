%% should not be included from include/foo.hrl even though the
%% include path points here - include/bar.hrl overrides it

-define(BAR_HRL, false).
