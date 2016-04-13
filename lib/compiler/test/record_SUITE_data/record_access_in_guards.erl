%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2006-2016. All Rights Reserved.
%% 
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%% 
%% %CopyrightEnd%
%%
-module(record_access_in_guards).

-export([t/0]).

-record(r, {a = 4,b}).
-record(r1, {a,b}).
-record(r2, {a = #r1{},b,c=length([1,2,3])}).
-record(r3, {a = fun(_) -> #r1{} end(1), b}).

t() ->
    foo = fun(A) when A#r1.a > A#r1.b -> foo end(#r1{b = 2}),
    0 = fun(A) when A#r2.a -> 0 end(#r2{a = true}),
    1 = fun(A) when (#r1{a = A})#r1.a > 2 -> 1 end(3),
    2 = fun(N) when ((#r2{a = #r{a = 4}, b = length([a,b,c])})#r2.a)#r.a > N ->
                2 end(2),
    3 = fun(A) when (A#r2.a)#r1.a =:= 3 -> 3 end(#r2{a = #r1{a = 3}}),
    ok = fun() ->
                 F = fun(A) when record(A#r.a, r1) -> 4;
                        (A) when record(A#r1.a, r1) -> 5
                     end,
                 5 = F(#r1{a = #r1{}}),
                 4 = F(#r{a = #r1{}}),
                 ok
         end(),
    3 = fun(A) when record(A#r1.a, r),
                          (A#r1.a)#r.a > 3 -> 3
        end(#r1{a = #r{a = 4}}),
    7 = fun(A) when record(A#r3.a, r1) -> 7 end(#r3{}),
    [#r1{a = 2,b = 1}] = 
        fun() ->
                [A || A <- [#r1{a = 1, b = 3}, 
                            #r2{a = 2,b = 1}, 
                            #r1{a = 2, b = 1}],
                      A#r1.a > 
                          A#r1.b]
        end(),
    {[_],b} = 
        fun(L) ->
                %% A is checked only once:
                R1 = [{A,B} || A <- L, A#r1.a, B <- L, A#r1.b],
                A = #r2{a = true},
                %% A is checked again:
                B = if A#r1.a -> a; true -> b end,
                {R1,B}
        end([#r1{a = true, b = true}]),

    p = fun(A) when (A#r1.a =:= 2) or (A#r2.a =:= 1) -> o;
           (_) -> p
        end(#r1{a = 2}),

    o = fun(A) when (A#r1.a =:= 2) orelse (A#r2.a =:= 1) -> o;
           (_) -> p
        end(#r1{a = 2}),

    3 = fun(A) when A#r1.a > 3, 
                    record(A, r1) -> 3
        end(#r1{a = 5}),

    ok = fun() ->
                 F = fun(A) when (A#r2.a =:= 1) orelse (A#r2.a) -> 2;
                        (A) when (A#r1.a =:= 1) orelse (A#r1.a) -> 1;
                        (A) when (A#r2.a =:= 2) andalso (A#r2.b) -> 3
                     end,
                 1 = F(#r1{a = 1}),
                 2 = F(#r2{a = true}),
                 3 = F(#r2{a = 2, b = true}),
                 ok
         end(),

    b = fun(A) when false or not (A#r.a =:= 1) -> a;
           (_) -> b
        end(#r1{a = 1}),
    b = fun(A) when not (A#r.a =:= 1) or false -> a;
           (_) -> b
        end(#r1{a = 1}),

    ok = fun() ->
                 F = fun(A) when not (A#r.a =:= 1) -> yes;
                        (_) -> no
                     end,
                 no = F(#r1{a = 2}),
                 yes = F(#r{a = 2}),
                 no = F(#r{a = 1}),
                 ok
         end(),

    a = fun(A) when record(A, r),
                    A#r.a =:= 1,
                    A#r.b =:= 2 ->a
        end(#r{a = 1, b = 2}),
    a = fun(A) when erlang:is_record(A, r),
                    A#r.a =:= 1,
                    A#r.b =:= 2 -> a
        end(#r{a = 1, b = 2}),
    a = fun(A) when is_record(A, r),
                    A#r.a =:= 1,
                    A#r.b =:= 2 -> a
        end(#r{a = 1, b = 2}),

    nop = fun(A) when (is_record(A, r1) and (A#r1.a > 3)) or (A#r2.a < 1) ->
                  japp;
             (_) ->
                  nop
          end(#r2{a = 0}),
    nop = fun(A) when (A#r1.a > 3) or (A#r2.a < 1) -> japp;
             (_) ->
                  nop
          end(#r2{a = 0}),

    ok = fun() ->
                 F = fun(A) when (A#r1.a =:= 2) or (A#r2.a =:= 1) -> o;
                        (_) -> p
                     end,
                 p = F(#r2{a = 1}),
                 p = F(#r1{a = 2}),
                 ok
         end(),

    ok = fun() ->
                 F = fun(A) when fail, A#r1.a; A#r1.a -> ab;
                        (_) -> bu
                     end,
                 ab = F(#r1{a = true}),
                 bu = F(#r2{a = true}),
                 ok
         end(),

    both = fun(A) when A#r.a, A#r.b -> both 
           end(#r{a = true, b = true}),

    ok = fun() ->
                 F = fun(A, B) when ((A#r1.a) orelse (B#r2.a)) 
                                    or (B#r2.b) or (A#r1.b) -> true;
                        (_, _) -> false
                     end,
                 true = F(#r1{a = false, b = false}, #r2{a = false, b = true}),
                 false = F(#r1{a = true, b = true}, #r1{a = false, b = true}),
                 ok
         end(),

    ok = fun(R) ->
		 F = fun(Head, Version, Tab) ->
			     VersionOK = (Head#r.a =:= Version),
			     if
				 Tab =:= Head#r.b, VersionOK ->
				     ok;
				 true ->
				     error
			     end
		     end,
		 ok = F(R, 42, tab),
		 error = F(R, 42, a),
 		 error = F(R, 0, tab),
		 {'EXIT',{{badrecord,r},_}} = (catch F({x,y,z}, 4, 5)),
		 ok
	 end(#r{a=42,b=tab}),

    ok.

