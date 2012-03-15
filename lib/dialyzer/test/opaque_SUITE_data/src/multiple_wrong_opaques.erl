-module(multiple_wrong_opaques).

-export([weird/1]).

-spec weird(dict() | gb_tree()) -> 42.

weird(gazonk) -> 42.

