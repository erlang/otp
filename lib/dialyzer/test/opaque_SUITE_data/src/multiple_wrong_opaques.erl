-module(multiple_wrong_opaques).

-export([weird/1]).

-spec weird(dict:dict() | gb_trees:tree()) -> 42.

weird(gazonk) -> 42.

