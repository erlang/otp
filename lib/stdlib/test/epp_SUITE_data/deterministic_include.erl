-module(deterministic_include).

-export([]).

-include("include/baz.hrl").
-include_lib("kernel/include/file.hrl").
