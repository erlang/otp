%%---------------------------------------------------------------------
%% A test for which the analysis went into an infinite loop due to
%% specialization using structured type instead of the opaque one.
%%---------------------------------------------------------------------

-module(opaque_bug1).

-export([test/1]).

-record(c, {a::atom()}).

-opaque erl_type() :: 'any' | #c{}.

test(#c{a=foo} = T) -> local(T).

local(#c{a=foo}) -> any.
