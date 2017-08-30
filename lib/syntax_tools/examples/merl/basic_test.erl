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

%-module(basic_test).
%-import(basic, run/1)

-export([basic_fib/1]).

-include_lib("eunit/include/eunit.hrl").

basics_test_() ->
    [?_assertEqual(42, run(1,[{1,{stop, 42}}])),
     ?_assertEqual("hello", run(1,[{1,{stop,"hello"}}])),
     ?_assertEqual(0, run(1,[{1,{print, "hello ~w", [42]}}])),
     ?_assertEqual(5, run(1,[{1,{stop, {plus, 2, 3}}}])),
     ?_assertEqual(5, run(1,[{1,{stop,{plus, 8, -3}}}])),
     ?_assertEqual(0, run(1,[{1,{stop,{equal, 0, 1}}}])),
     ?_assertEqual(1, run(1,[{1,{stop,{equal, 1, 1}}}])),
     ?_assertEqual(0, run(1,[{1,{stop,{gt, 0, 1}}}])),
     ?_assertEqual(0, run(1,[{1,{stop,{gt, 1, 1}}}])),
     ?_assertEqual(1, run(1,[{1,{stop,{gt, 2, 1}}}])),
     ?_assertEqual(0, run(1,[{1,{stop,{knot, 42}}}])),
     ?_assertEqual(1, run(1,[{1,{stop,{knot, 0}}}])),
     ?_assertEqual(42, run(1,[{1,{set, x, 42}}, {2,{stop,x}}])),
     ?_assertEqual(17, run(1,[{1,{iff, 1, 2, 3}},
                              {2,{stop, 17}},
                              {3,{stop, 42}}])),
     ?_assertEqual(42, run(1,[{1,{iff, 0, 2, 3}},
                              {2,{stop, 17}},
                              {3,{stop, 42}}])),
     ?_assertEqual(17, run(1,[{1,{iff, 1, 2, 3}},
                              {2,{stop, 17}},
                              {3,{stop, -1}}])),
     ?_assertEqual(42, run(1,[{1,{iff, 0, 2, 3}},
                              {2,{stop, -1}},
                              {3,{stop, 42}}]))


    ].


fib_test_() ->
    [?_assertEqual(fib(N), basic_fib(N)) || N <- lists:seq(1,15)
    ].


fib(N) when N > 1 ->
    fib(N-1) + fib(N-2);
fib(_) ->
    1.

basic_fib(N) ->
    run(1,
        [{1,{set,x,0}},
         {2,{set,a,1}},
         {3,{set,b,0}},
         {10,{iff, {equal, x, N}, 20, 30}},
         {20,{stop,a}},
         {30,{print,"~w, ~w, ~w\n",[x,a,b]}},
         {31,{set,t,a}},
         {32,{set,a,{plus,a,b}}},
         {33,{set,b,t}},
         {34,{set,x,{plus,x,1}}},
         {40,{goto,10}}
         ]).
