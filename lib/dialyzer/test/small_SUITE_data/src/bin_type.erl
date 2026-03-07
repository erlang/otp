-module(bin_type).

-export([handle/1, status/0, map_key/1]).

-type cmd() :: <<"start">> | <<"stop">>.

-spec handle(cmd()) -> ok.
handle(<<"start">>) -> ok;
handle(<<"stop">>) -> ok.

-spec status() -> <<"ok">> | <<"error">>.
status() -> <<"ok">>.

-spec map_key(#{<<"name">> := binary()}) -> binary().
map_key(#{<<"name">> := V}) -> V.
