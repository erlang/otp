%%%-------------------------------------------------------------------
%%% File    : fun_ref_record.erl
%%% Author  : Tobias Lindahl <tobiasl@csd.uu.se>
%%% Description : Exposes a bug when referring to a fun in a record.
%%%
%%% Created : 25 Sep 2007 by Tobias Lindahl <tobiasl@csd.uu.se>
%%%-------------------------------------------------------------------
-module(fun_ref_record).

-export([t1/0, t2/0]).

-record(foo, {bar}).

t1() ->
    #foo{bar=fun t2/0}.

t2() -> ok.
