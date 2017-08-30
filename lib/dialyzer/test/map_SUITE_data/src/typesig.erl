-module(typesig).

-export([t1/0, t2/0, t3/0, test/1]).

t1() -> test(#{a=>1}).
t2() -> test(#{a=>{b}}).
t3() -> test(#{a=>{3}}).

test(#{a:={X}}) -> X+1.
