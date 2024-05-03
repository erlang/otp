-module(gh_7153).
-export([t/1]).

t(X = '原子') ->
    <<X/utf8>>.
