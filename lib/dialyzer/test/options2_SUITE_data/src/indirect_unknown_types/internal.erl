-module(internal).
-export([open_socket/1]).
-include("type_defs.hrl").

-spec open_socket(connect_opts()) -> ok.
open_socket(_ConnectOpts) ->
  ok.
