-module(epgsql_cmd_connect).
-export([open_socket/1]).

%% ... no code at line 3 ...

-spec open_socket(epgsql:connect_opts()) -> ok.
open_socket(_ConnectOpts) ->
  ok.
