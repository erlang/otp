-module(bin_type).

-export([handle/1, status/0]).

-type cmd() :: <<"start">> | <<"stop">>.

-spec handle(cmd()) -> ok.
handle(<<"start">>) -> ok;
handle(<<"stop">>) -> ok.

-spec status() -> <<"ok">> | <<"error">>.
status() -> <<"ok">>.
