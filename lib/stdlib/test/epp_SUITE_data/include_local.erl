
-module(include_local).

-include("include/foo.hrl").

-a({?FOO_HRL, ?BAR_HRL}).
