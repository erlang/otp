%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2005-2016. All Rights Reserved.
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

-module(erl_expand_records_SUITE).

%%-define(debug, true).

-ifdef(debug).
-define(line, put(line, ?LINE), ).
-define(config(X,Y), foo).
-define(privdir, "erl_expand_records_SUITE_priv").
-define(t, test_server).
-else.
-include_lib("common_test/include/ct.hrl").
-define(privdir, proplists:get_value(priv_dir, Config)).
-endif.

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, 
	 init_per_testcase/2, end_per_testcase/2]).

-export([attributes/1, expr/1, guard/1,
         init/1, pattern/1, strict/1, update/1,
	 otp_5915/1, otp_7931/1, otp_5990/1,
	 otp_7078/1, otp_7101/1, maps/1]).

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() -> 
    [attributes, expr, guard, init,
     pattern, strict, update, maps, {group, tickets}].

groups() -> 
    [{tickets, [],
      [otp_5915, otp_7931, otp_5990, otp_7078, otp_7101]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


%% Import module and functions.
attributes(Config) when is_list(Config) ->
    Ts = [
	  <<"-import(lists, [append/2, reverse/1]).

         -record(r, {a,b}).

t() ->
    [2,1] = reverse(append([1],[2])),
    3 = length([1,2,3]),
    3 = record_info(size, r),
    [a, b] = record_info(fields, r),
    [_|_] = erl_expand_records_SUITE:all(),
    ok.
">>
      ],
    run(Config, Ts),
    ok.
    
%% Some expressions.
expr(Config) when is_list(Config) ->
    Ts = [
      <<"
         -record(r, {a,b,c}).

         t() ->
             [1,2] = [R#r.a || R <- [#r{a = 1}, #r{a = 2}, #r{a = 3}],
                               R#r.a < 3],
             [1,2] = [R#r.a || R <- [#r{a = 1}, #r{a = 2}, #r{a = 3}],
                               begin R#r.a < 3 end],
             [1,2,3] = [R#r.a || R <- [#r{a = 1}, #r{a = 2}, #r{a = 3}],
                                 begin is_record(R, r) end],
             [1,2,3] = [R#r.a || R <- [#r{a = 1}, #r{a = 2}, #r{a = 3}],
                                 begin erlang:is_record(R, r) end],
             ok.
      ">>,
      <<"
         -record(r, {a,b,c}).

         f(X) -> X.

         t() ->
             A = {$c, 1, 3.14, a, \"hi\", [], [a,b]},
             R = #r{a = element(6, A), b = #r.b},
             3 = R#r.b,
             <<1:8>> = <<(begin erlang:element(2, A) end):8>>,
             self() ! {a, message, []},
             One = 1 = fun f/1(1),
             2 = fun(X) -> X end(One + One),
             3 = fun exprec_test:f/1(3),
             4 = exprec_test:f(4),
             5 = f(5),
             L = receive 
                     {a,message,L0} ->
                         L0
                 end,
             case catch a:foo(bar) of
                 {'EXIT', _} -> ok
             end,
             _ = receive 			%Suppress warning.
                    noop ->
                       1/(length(L) - 0)
                 after 0 ->
                    ok
                 end,
             if 
                 R#r.c =:= undefined ->
                     ok;
                 true ->
                     not_ok
             end.

         is_record(_, _, _) ->
             error(wrong_is_record).
      ">>
      ],

    %% The code above should run equally well with and without
    %% strict record tests.
    run(Config, Ts, [no_strict_record_tests]),
    run(Config, Ts, [strict_record_tests]),
    
    ok.
    
%% is_record in guards.
guard(Config) when is_list(Config) ->
    File = filename("guard.erl", Config),
    Beam = filename("guard.beam", Config),
    Test = <<"-module(guard).

              -export([t/1]).

              -record(r, {a,b}).

              t(_) when is_record(3, r) ->
                  1;
              t(_) when is_record(a, r) ->
                  2;
              t(_) when is_record(3.14, r) ->
                  3;
              t(_) when is_record([], r) ->
                  4;
              t(_) when is_record([a], r) ->
                  5;
              t(_) when is_record($a, r) ->
                  6;
              t(_) when is_record(\"foo\", r) ->
                  7;
              t(_) when is_record(#r.a, r) ->
                  8;
              t(_) when is_record(<<\"foo\">>, r) -> % line 23
                  9;
              t(_) when is_record(1 + 2, r) ->
                  10;
              t(_) when is_record(+ 3, r) ->
                  11;
              t(_) ->
                  12.
             ">>,

    ok = file:write_file(File, Test),
    {ok, guard, Ws} = compile:file(File, [return,{outdir,?privdir}]),
    Warnings = [L || {_File,WL} <- Ws, {L,_M,nomatch_guard} <- WL],
    [7,9,11,13,15,17,19,21,23,25,27] = Warnings,

    ok = file:delete(File),
    ok = file:delete(Beam),
    ok.

%% Wildcard initialisation.
init(Config) when is_list(Config) ->
    Ts = [
      <<"
         -record(r, {a,b,c,d = foo}).

         t() ->
             R = #r{_ = init, b = b},
             #r{c = init, b = b, a = init} = R,
             case R of
                 #r{b = b, _ = init} -> ok;
                 _ -> not_ok
             end.
      ">>
      ],
    run(Config, Ts),
    ok.
    
%% Some patterns.
pattern(Config) when is_list(Config) ->
    Ts = [
      <<"-import(lists, [append/2, reverse/1]).

         -record(r, {a,b}).

         t() ->
             1 = t(#r{}),
             2 = t($a),
             3 = t(1000),
             4 = t({1000}),
             5 = t(3),
             6 = t(-3.14),
             7 = t({4.0}),
             8 = t(3.14),
             9 = t(\"str\"),
             10 = t([]),
             11 = t([a|b]),
             12 = t(\"string\"),
             13 = t({[]}),
             14 = t({a,b}),
             15 = t({{}}),
             16 = t({tuple,tupel}),
             17 = t(4),
             18 = t(10),
             19 = t({a}),
             20 = t(<<100:8,220:8>>),
             21 = t(#r{a = #r{}}),
             22 = t(2),
             23 = t(#r{a = #r{}, b = b}),
             24 = t(abc),
             ok.

         t(abc) ->
             24;
         t($a) ->
             2;
         t(3) ->
             5;
         t(3.14) ->
             8;
         t(\"str\") ->
             9;
         t([]) ->
             10;
         t([a|b]) ->
             11;
         t(L) when is_list(L) ->
             12;
         t({L}) when list(L) ->
             13;
         t({a,b}) ->
             14;
         t({T}) when is_tuple(T) ->
             15;
         t(+ 4) ->
             17;
         t(3+7) ->
             18;
         t(<<A:8, (100+120):8>>) when A =:= 100 ->
             20;
         t(#r{a = #r{}, b = undefined}) ->
             21;
         t(#r.a) ->
             22;
         t(A) when is_record(A, r), record(element(2, A), r) ->
             23;
         t(A) when is_record(A, r) ->
             1;
         t(I) when is_integer(I) ->
             3;
         t({I}) when integer(I) ->
             4;
         t({F}) when float(F) ->
             7;
         t({A} = B) when A < B ->
             19;
         t(F) when is_float(F) ->
             6;
         t(T) when tuple(T) ->
             16.
      ">>
      ],
    run(Config, Ts),
    ok.
    
strict(Config) when is_list(Config) ->
    Ts1 = [
      <<"-record(r1, {a,b}).
         -record(r2, {a,b}).

         t() ->
             A = #r1{a = 1, b = 2},
             ok = try 
                      {1, 2} = {A#r2.a, A#r2.b},
                      not_ok
                  catch error:{badrecord,r2} -> ok
                  end,
             try
                 case foo of
                     _ when A#r2.a =:= 1 -> not_ok
                 end
             catch error:_ -> ok
             end.
         element(_, _) ->
             error(wrong_element).
      ">>
      ],
    run(Config, Ts1, [strict_record_tests]),

    Ts2 = [
      <<"-record(r1, {a,b}).
         -record(r2, {a,b}).

         t() ->
             A = #r1{a = 1, b = 2},
             {1, 2} = {A#r2.a, A#r2.b},
             case foo of
                 _ when A#r2.a =:= 1 -> ok
             end.
         element(_, _) ->
             error(wrong_element).
      ">>
      ],
    run(Config, Ts2, [no_strict_record_tests]),
    ok.
    
%% Record updates.
update(Config) when is_list(Config) ->
    Ts = [
      <<"-record(r, {a,b,c,d,e,f}).

         t() ->
             R0 = #r{},
             R1 = R0#r{a = #r.a, e = {x,y}},
             2 = R1#r.a,
             R2 = R1#r{},
             true = R1 =:= R2,
             R3 = R2#r{c = fun(X) -> X end, 
                       d = <<\"foo\">>, 
                       e = [x,y,z], 
                       f = {R0,R1}},
             R4 = R3#r{a = R3#r{b = #r{}}},
             true = erlang:is_record((R4#r.a)#r.b, r),
             #r{a = R0, b = 3, c = 3.14, d = [], e = [[]], f = [{}]} = 
                 R4#r{a = R0, b = 3, c = 3.14, d = [], e = [[]], f = [{}]},
             ok.

         %% Just playing around a bit...
         t1() ->
             ((#r{a = (#r{b = #r{}})#r{a = #r{}}})#r{b = #r{}})#r{c = #r{}}.

         t2() ->
             R0 = #r{},
             #r{_ = R0#r{a = ok}}.

         %% Implicit calls to setelement/3 must go to the BIF,
         %% not to this function.
         setelement(_, _, _) ->
             erlang:error(wrong_setelement_called).
      ">>
      ],
    run(Config, Ts),
    ok.

maps(Config) when is_list(Config) ->
    Ts = [<<"-record(rr, {a,b,c}).
             t() ->
                 R0 = id(#rr{a=1,b=2,c=3}),
                 R1 = id(#rr{a=4,b=5,c=6}),
                 [{R0,R1}] =
                     maps:to_list(#{#rr{a=1,b=2,c=3} => #rr{a=4,b=5,c=6}}),
                 #{#rr{a=1,b=2,c=3} := #rr{a=1,b=2,c=3}} =
                     #{#rr{a=1,b=2,c=3} => R1}#{#rr{a=1,b=2,c=3} := R0},
                 ok.

             id(X) -> X.
            ">>],
    run(Config, Ts, [strict_record_tests]),
    ok.

%% Strict record tests in guards.
otp_5915(Config) when is_list(Config) ->
    %% These tests are also run by the compiler's record_SUITE.
    Ts = [
      <<"-record(r, {a = 4,b}).
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

             ok.
      ">>
      ],
    run(Config, Ts, [strict_record_tests]),
    ok.

%% Test optimization of record accesses and is_record/3 tests in guards.
otp_7931(Config) when is_list(Config) ->
    Ts = [
      <<"-record(r, {a = 4,b}).
         -record(r1, {a,b}).
         -record(r2, {a = #r1{},b,c=length([1,2,3])}).
         -record(r3, {a = fun(_) -> #r1{} end(1), b}).

         t() ->
             ok = fun() ->
                    F = fun(F, [H,H|T]) when is_record(H, r) ->
                                [H|F(F, T)];
                             (F, [H|T]) when is_record(H, r) ->
                                [H|F(F, T)];
                             (_, []) -> []
                          end,
                    [#r{a=4,b=7},#r{a=1,b=42}] =
                       F(F, [#r{a=4,b=7},#r{a=4,b=7},#r{a=1,b=42}]),
                    {'EXIT',_} = (catch F(F, [#r1{}])),
                    ok
                  end(),

             true = fun() ->
                      R = #r{},
                      if is_record(R, r) -> true; true -> false end
                     end(),

             ok = fun() ->
                      F = fun(true, B) when B#r1.a -> ok;
                             (false, _) -> error
                      end,
                      ok = F(true, #r1{a=true}),
                      error = F(false, anything_goes),
                      {'EXIT',_} = (catch F(true, #r1{})),
                      {'EXIT',_} = (catch F(true, #r{})),
                      ok
                  end(),

	     ok = fun() ->
		      F = fun([{a,R}=T]) when R#r.a =:= 42 ->
                                   {ok,tuple_size(T)};
			     ([{a,R}=T]) when R#r1.a =:= 7 ->
                                   {ok,tuple_size(T)};
			     (_) -> error
		          end,
		      {ok,2} = F([{a,#r{a=42}}]),
		      {ok,2} = F([{a,#r1{a=7}}]),
		      error = F([{a,#r1{}}]),
		      error = F({a,b,c}),
		      error = F([]),
		      ok
	     end(),

	     ok = fun() ->
		      F = fun(X, Y, Z) when is_record(X, r1) andalso
                                            (is_record(Y, r2) orelse
                                             is_record(Z, r3)) -> true;
			     (_, _, _) -> false
		          end,
		      true = F(#r1{}, #r2{}, #r3{}),
 		      true = F(#r1{}, #r2{}, blurf),
 		      true = F(#r1{}, blurf, #r3{}),
 		      false = F(#r1{}, blurf, blurf),
		      false = F(blurf, #r2{}, #r3{}),
 		      false = F(blurf, #r2{}, blurf),
 		      false = F(blurf, blurf, #r3{}),
 		      false = F(blurf, blurf, blurf),
		      ok
	     end(),

	     ok = fun() ->
		      F = fun(R=#r{a=42}) when R#r.b =:= 7 ->
                                   {ok,R};
			     (_) -> error
		          end,
		      {ok,#r{a=42,b=7}} = F(#r{a=42,b=7}),
		      error = F(#r{}),
		      error = F([a,b,c]),
		      ok
	     end(),

             ok.
      ">>
      ],
    run(Config, Ts, [strict_record_tests]),
    ok.

%% OTP-5990. {erlang,is_record}.
otp_5990(Config) when is_list(Config) ->
    Ts = [
      <<"
         -record(r, {a,b,c}).

         t() ->
             [1,2,3] = [R#r.a || R <- [#r{a = 1}, #r{a = 2}, #r{a = 3}],
                                 begin {erlang,is_record}(R, r) end],
             [1,2,3] = [R#r.a || R <- [#r{a = 1}, #r{a = 2}, #r{a = 3}],
                                 begin {erlang,is_record}(R, r) end],
             ok.
      ">>,

      <<"
         -record('OrdSet', {orddata = {},
                            ordtype = {}}).

         to_sets(S) when tuple(S#'OrdSet'.ordtype) ->
             ok.

         lc(S) ->
             [X || X <- [S], tuple(X#'OrdSet'.ordtype)].

         t() ->
             S = #'OrdSet'{},
             ok = to_sets(S),
             [S] = lc(S),
             ok.
      ">>
      ],
    run(Config, Ts, [strict_record_tests]),
    ok.
        

%% OTP-7078. Record update: missing test.
otp_7078(Config) when is_list(Config) ->
    Ts = [
      <<"
         -record(r, {f}).
         -record(r2, {}).

         t() ->
             {'EXIT',_} = (catch (#r2{})#r{}),
             {'EXIT',_} = (catch (#r2{})#r{f = 2}),
             ok.
      ">>,

      <<"
         -record(r, {f}).

         maker(F) -> 
             put(a, get(a)+1),
             #r{f = F}.

         t() ->
             put(a, 0),
             (maker(2))#r{},
             1 = get(a),
             ok.
      ">>

      ],
    run(Config, Ts, [strict_record_tests]),
    ok.

-record(otp_7101, {a,b,c=[],d=[],e=[]}).

%% OTP-7101. Record update: more than one call to setelement/3.
otp_7101(Config) when is_list(Config) ->
    Rec = #otp_7101{},

    %% Spawn a tracer process to count the number of setelement/3 calls.
    %% The tracer will forward all trace messages to us.
    Self = self(),
    Tracer = spawn_link(fun() -> otp_7101_tracer(Self, 0) end),
    1 = erlang:trace_pattern({erlang,setelement,3}, true),
    erlang:trace(self(), true, [{tracer,Tracer},call]),
    
    %% Update the record.
    #otp_7101{a=2,b=1,c=[],d=[],e=[]} = otp_7101_update1(Rec),
    #otp_7101{a=1,b=2,c=[],d=[],e=[]} = otp_7101_update2(Rec),
    #otp_7101{a=2,b=1,c=[],d=[],e=[]} = otp_7101_update3(Rec),
    #otp_7101{a=1,b=2,c=[],d=[],e=[]} = otp_7101_update4(Rec),

    %% Verify that setelement/3 was called the same number of times as
    %% the number of record updates.
    Ref = erlang:trace_delivered(Self),
    receive
	{trace_delivered, Self, Ref} ->
	    Tracer ! done
    end,
    1 = erlang:trace_pattern({erlang,setelement,3}, false),
    receive
	4 ->
	    ok;
	Other ->
	    ct:fail({unexpected,Other})
    end.

otp_7101_tracer(Parent, N) ->
    receive
	{trace,Parent,call,{erlang,setelement,[_,_,_]}} ->
	    otp_7101_tracer(Parent, N+1);
	done ->
	    Parent ! N
    end.

otp_7101_update1(R) ->
    R#otp_7101{b=1,
	       a=2}.

otp_7101_update2(R) ->
    R#otp_7101{a=1,
	       b=2}.

otp_7101_update3(R) ->
    R#otp_7101{b=1,a=2}.

otp_7101_update4(R) ->
    R#otp_7101{a=1,b=2}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

run(Config, Tests) ->
    run(Config, Tests, []).

run(Config, Tests, Opts) ->
    F = fun(P) ->
                {SourceFile, Mod} = compile_file_mod(Config),
                _ = compile_file(Config, P, Opts),
                AbsFile = filename:rootname(SourceFile, ".erl"),
                code:purge(Mod),
                code:load_abs(AbsFile, Mod),
                case catch Mod:t() of
                    {'EXIT', _Reason} = Error ->
                        io:format("failed, got ~p~n", [Error]),
                        fail();
                    ok ->
                        ok
                end
        end,
    lists:foreach(F, Tests).

%% Compiles a test module and returns the list of errors and warnings.

compile_file(Config, Test0, Opts0) ->
    {File, _Mod} = compile_file_mod(Config),
    Filename = 'exprec_test.erl',
    Test = list_to_binary(["-module(exprec_test). "
                           "-compile(export_all). ",
                           Test0]),
    File = filename(Filename, Config),
    Opts = [export_all,return,{outdir,?privdir}|Opts0],
    ok = file:write_file(File, Test),
    {ok, _M, Ws} = compile:file(File, Opts),
    warnings(File, Ws).

compile_file_mod(Config) ->
    {filename('exprec_test.erl', Config), exprec_test}.

filename(Name, Config) when is_atom(Name) ->
    filename(atom_to_list(Name), Config);
filename(Name, Config) ->
    filename:join(?privdir, Name).

warnings(File, Ws) ->
    case lists:append([W || {F, W} <- Ws, F =:= File]) of
        [] -> [];
        L -> {warnings, L}
    end.

fail() ->
    ct:fail(failed).
