-module(f_include_2).

-feature(experimental_ftr_1, enable).

%% Include file defines a macro, conditional on whether experimental_ftr_1
%% is enabled.
-include("macro_enabled.hrl").

%% At this point the prefix will definitely end, if it has not already
-export([foo/1]).

foo(2) -> ?UNLESS.
