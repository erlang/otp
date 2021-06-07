-module(record_creation_diffs).

-export([foo/1]).

-record(bar, {
          some_atom :: atom(),
          some_list :: list()
         }).

foo(Input) ->
    #bar{some_atom = Input, some_list = {this,is,a,tuple}}.
