%% ---------------------------------------------------------------------
%% Licensed under the Apache License, Version 2.0 (the "License"); you may
%% not use this file except in compliance with the License. You may obtain
%% a copy of the License at <http://www.apache.org/licenses/LICENSE-2.0>
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% @author Richard Carlsson <carlsson.richard@gmail.com>
%% @copyright 2012 Richard Carlsson
%% @doc Tests. For including in another module.

%-module(lisp_test).
%-import(lisp, eval/1)

-export([fib/1, lisp_fib/1]).

-include_lib("eunit/include/eunit.hrl").

basics_test_() ->
    [?_assertEqual(42, eval(42)),
     ?_assertEqual("hello", eval([quote, "hello"])),
     ?_assertEqual(print, eval([quote, print])),
     ?_assertMatch([17,[1,2],42], eval([list,17,[list,1,2],42])),
     ?_assertEqual([], eval([print, [quote, "hello ~w"], [list, 42]])),
     ?_assertEqual(5, eval([plus, 2, 3])),
     ?_assertEqual(5, eval([plus, 8, -3])),
     ?_assertEqual([], eval([equal, 0, 1])),
     ?_assertEqual(1, eval([equal, 1, 1])),
     ?_assertEqual([], eval([gt, 0, 1])),
     ?_assertEqual([], eval([gt, 1, 1])),
     ?_assertEqual(1, eval([gt, 2, 1])),
     ?_assertEqual([], eval([knot, 42])),
     ?_assertEqual(1, eval([knot, []])),
     ?_assertEqual(42, eval([do, 17, 42])),
     ?_assertEqual([], eval([apply, print, [quote, ["~p", [42]]]])),
     ?_assertEqual(42, eval([iff, [], 17, 42])),
     ?_assertEqual(17, eval([iff, 1, 17, 42])),
     ?_assertEqual(42, eval([iff, [], [apply], 42])),
     ?_assertEqual(17, eval([iff, 1, 17, [apply]])),
     ?_assertEqual(17, eval([def, foo, 17, foo])),
     ?_assertEqual(17, eval([def, bar, 42, [def, foo, 17, foo]])),
     ?_assertEqual(42, eval([def, bar, 42, [def, foo, 17, bar]])),
     ?_assertEqual(17, eval([def, foo, 42, [def, foo, 17, foo]]))
    ].

-ifdef(INTERPRETED).
interpreter_basics_test_() ->
    [?_assertThrow({undefined, foo}, eval(foo)),
     ?_assertMatch({builtin,_}, eval(print)),
     ?_assertThrow(bad_do, eval([do])),
     ?_assertThrow(bad_apply, eval([apply])),
     ?_assertThrow({undefined, foo}, eval([def, bar, 17, foo]))
    ].

interpreter_lambda_test_() ->
    [?_assertMatch({lambda,_,_,_}, eval([lambda, [], 42])),
     ?_assertMatch({lambda,_,_,_}, eval([lambda, [x], x])),
     ?_assertMatch({lambda,_,_,_}, eval([lambda, [x,y], 42]))
    ].
-endif.

lambda_test_() ->
    [?_assertThrow(bad_lambda, eval([lambda])),
     ?_assertThrow(bad_lambda, eval([lambda, []])),
     ?_assertThrow(bad_lambda, eval([lambda, [], 17, 42])),
     ?_assertThrow(bad_lambda, eval([lambda, 17, 42])),
     ?_assertThrow(bad_lambda, eval([lambda, [17], 42])),
     ?_assertThrow(bad_lambda, eval([lambda, [foo, foo], 42])),
     ?_assertEqual(42, eval([[lambda, [x], x], 42])),
     ?_assertEqual([42, 17], eval([[lambda, [x], [list, x, 17]], 42])),
     ?_assertEqual([42, 17], eval([def, f, [def, y, 42,
                                            [lambda, [x], [list, y, x]]],
                                   [f, 17]]))
    ].

fib_test_() ->
    [?_assertEqual(fib(N), lisp_fib(N)) || N <- lists:seq(1,15)
    ].


fib(N) when N > 1 ->
    fib(N-1) + fib(N-2);
fib(_) ->
    1.

lisp_fib(N) ->
    eval([def, fib,
          [y, [lambda, [f], [lambda, [x],
                             [iff, [gt, x, 1],
                              [plus, [f, [plus,x,-1]], [f, [plus,x,-2]]],
                              1]
                            ]]],
          [fib, N]
         ]).
