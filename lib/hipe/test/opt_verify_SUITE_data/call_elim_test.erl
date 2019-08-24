-module(call_elim_test).

-export([test/0]).

test() ->
    true = has_1_field(#{1=>true}),
    true = has_1_field(#{1=>"hej", b=>2}),
    true = has_1_field(#{b=>3, 1=>4}),
    ok.

has_1_field(#{1:=_}) -> true;
has_1_field(#{}) -> false.
