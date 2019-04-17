%%%-------------------------------------------------------------------
%%% File    : record_pat.erl
%%% Author  : Tobias Lindahl <>
%%% Description : Emit warning if a pattern violates the record type
%%%
%%% Created : 21 Oct 2008 by Tobias Lindahl <>
%%%-------------------------------------------------------------------
-module(record_pat).

-export([t/1]).

-record(foo, {bar :: integer()}).

t(#foo{bar=baz}) -> no_way;
t(#foo{bar=1}) -> ok.
