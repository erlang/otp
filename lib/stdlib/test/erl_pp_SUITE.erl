%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2006-2012. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%
%%%----------------------------------------------------------------
%%% Purpose:Test Suite for the 'erl_pp' module.
%%%-----------------------------------------------------------------
-module(erl_pp_SUITE).

%-define(debug, true).

-ifdef(debug).
-define(line, put(line, ?LINE), ).
-define(config(X,Y), foo).
-define(datadir, "erl_pp_SUITE_data").
-define(privdir, "erl_pp_SUITE_priv").
-define(t, test_server).
-else.
-include_lib("test_server/include/test_server.hrl").
-define(datadir, ?config(data_dir, Config)).
-define(privdir, ?config(priv_dir, Config)).
-endif.

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, 
	 init_per_testcase/2, end_per_testcase/2]).

-export([ func/1, call/1, recs/1, try_catch/1, if_then/1,
	  receive_after/1, bits/1, head_tail/1,
	  cond1/1, block/1, case1/1, ops/1, messages/1,
	  old_mnemosyne_syntax/1,
	  import_export/1, misc_attrs/1,
	  hook/1,
	  neg_indent/1,

	  otp_6321/1, otp_6911/1, otp_6914/1, otp_8150/1, otp_8238/1,
	  otp_8473/1, otp_8522/1, otp_8567/1, otp_8664/1, otp_9147/1,
          otp_10302/1]).

%% Internal export.
-export([ehook/6]).

% Default timetrap timeout (set in init_per_testcase).
-define(default_timeout, ?t:minutes(2)).

init_per_testcase(_Case, Config) ->
    ?line Dog = ?t:timetrap(?default_timeout),
    [{watchdog, Dog} | Config].

end_per_testcase(_Case, _Config) ->
    Dog = ?config(watchdog, _Config),
    test_server:timetrap_cancel(Dog),
    ok.

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [{group, expr}, {group, attributes}, hook, neg_indent,
     {group, tickets}].

groups() -> 
    [{expr, [],
      [func, call, recs, try_catch, if_then, receive_after,
       bits, head_tail, cond1, block, case1, ops,
       messages, old_mnemosyne_syntax]},
     {attributes, [], [misc_attrs, import_export]},
     {tickets, [],
      [otp_6321, otp_6911, otp_6914, otp_8150, otp_8238,
       otp_8473, otp_8522, otp_8567, otp_8664, otp_9147, otp_10302]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.



func(suite) ->
    [];
func(Config) when is_list(Config) ->
    Ts = [{func_1,
           <<"-record(r1, {a,b}).
              -record(r3, {a = fun(_) -> #r1{} end(1), b}).

              t() ->
                  fun(A) when record(A#r3.a, r1) -> 7 end(#r3{}).
             ">>},
          {func_2,
           <<"-record(r1, {a,b}).
              -record(r3, {a = fun(_) -> #r1{} end(1), b}).

              t() ->
                  fsdfsdfjsdfjkljf:sdlfjdsfjlf(
                      fun(sdfsd) -> {sdkjsdf,sdfjsdkljfsdl,sdfkjdklf} end).
             ">>},
          {func_3,
           <<"t() -> fun t/0.">>},
          {func_4,
           <<"t() -> fun modul:foo/3.">>},
          {func_5, % 'when' is moved down one line
           <<"tkjlksjflksdjflsdjlk()
                 when kljlsajflksjdfklsjdlkjfklsdklfjsdlf <
                      kljasjfdsjflsdjfklsdjfklsdjfklsd ->
                 foo.">>},
          {func_6,
           <<"t() ->
                  (fun() ->
                           true
                   end)().">>},
	  {func_7,
           <<"t(M, F, A) -> fun M:F/A.">>}
          ],
    ?line compile(Config, Ts),
    ok.

call(suite) ->
    [];
call(Config) when is_list(Config) ->
    Ts = [{call_1,
           <<"t() ->
                  fookjljsflj:barlkjlkjsdfj(kjdslfjsdl,hej,san,sa,
                      foo,sdfds,sdfsdf,sdfsd,sdfdsf,sdfdsf,sfdsf,
                      sfds,sdfsdf,sfds).
             ">>}
          ],
    ?line compile(Config, Ts),
    ok.

recs(suite) ->
    [];
recs(Config) when is_list(Config) ->
    %% Evolved while testing strict record tests in guards...
    Ts = [{recs_1,
           <<"-compile(strict_record_tests).
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

                  %% The test done twice (an effect of doing the test as soon as possible).
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

                  %% No extra check added:
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
                                                  or (B#r2.b) or (A#r1.b) ->
                                         true;
                                      (_, _) -> false
                                   end,
                               true = F(#r1{a = false, b = false},
                                        #r2{a = false, b = true}),
                               false = F(#r1{a = true, b = true},
                                         #r1{a = false, b = true}),
                               ok
                       end(),

                  ok.
             ">>},
          {recs_2,
           <<"-record(r1, {a, b = foo:bar(kljlfjsdlf, kjlksdjf)}).
              -record(r2, {c = #r1{}, d = #r1{a = bar:foo(kljklsjdf)}}).

              t() ->
                  R = #r2{},
                  R#r2{c = R, d = #r1{}}.">>}
          ],
    ?line compile(Config, Ts),

    ?line ok = pp_expr(<<"case #r{a={1,2},b=#r{}} of
                              X=Y=#r{a=foo,b=bar} ->
                                  {(foooo:baaaar(X))#r{a = rep},Y,#r.b}
                          end">>),
    ?line ok = pp_expr(<<"R#r{a = {kljasdklf,sdkfjsdl,sdafjkllsdf,sdfkjsd,
                          sdafjsd,sdf,sdafsd,sdfdsf,sdfdsf,dsfds}}">>),
    ok.

try_catch(suite) ->
    [];
try_catch(Config) when is_list(Config) ->
    Ts = [{try_1, % copied from erl_eval_SUITE
           <<"t() -> try 1 of 1 -> 2 catch _:_ -> 3 end.">>},
          {try_2,
           <<"t() -> try 1 of 1 -> 2; 3 -> 4 catch _:_ -> 5 end.">>},
          {try_3,
           <<"t() -> try 3 of 1 -> 2; 3 -> 4 catch _:_ -> 5 end.">>},
          {try_4,
           <<"t() -> try 1 after put(try_catch, 2) end.">>},
          {try_5,
           <<"t() -> try 1 of 1 -> 2; 3 -> 4
   	             after put(try_catch, 5) end.">>},
          {try_6,
           <<"t() -> try 1=2 catch throw:{badmatch,2} -> 3 end.">>},
          {try_7,
           <<"t() -> try 1=2 of 3 -> 4
		     catch error:{badmatch,2} -> 5 end.">>},
          {try_8,
           <<"t() -> try 1=2
		     catch error:{badmatch,2} -> 3
		     after put(try_catch, 4) end.">>},
          {try_9,
           <<"t() -> try 1=2
		     catch error:{badmatch,2} -> 3
		     after put(try_catch, 4) end.">>},
          {try_10,
           <<"t() -> try a,b,c
                         catch exit:_ -> d;
                               throw:_ -> t;
                               error:{foo,bar} -> foo,
                                                  bar
                         end.">>},

          {catch_1,
           <<"t() -> catch foo.">>},
          {catch_2,
           <<"t() -> case catch foo of bar -> foo end.">>},
          {catch_3,
           <<"t() -> catch begin begin foo, bar, foo:bar(kljsldkfjdls,kljsdl),
                           (catch bar:foo(foo)) end end.">>}
          ],
    ?line compile(Config, Ts),
    ?line ok = pp_expr(<<"try
                              erl_internal:bif(M,F,length(Args))
                          of
                              true ->
                                  call(N,Args,Prec,Hook);
                              false ->
                                  call(Name,Args,Prec,Hook)
                          after foo end">>),
    ok.

if_then(suite) ->
    [];
if_then(Config) when is_list(Config) ->
    Ts = [{if_1,
           <<"t() -> if 1 > 2 -> 1; true -> b end.">>},
          {if_2,
           <<"t() -> if true -> true end.">>},
          {if_3,
           <<"t() -> if 1 == 2 -> a; 1 > 2 -> b; 1 < 2 -> c end.">>}
          ],
    ?line compile(Config, Ts),
    ok.

receive_after(suite) ->
    [];
receive_after(Config) when is_list(Config) ->
    Ts = [{rec_1,
           <<"t() -> receive foo -> bar; bar -> foo end.">>},
          {rec_2,
           <<"t() -> receive foo -> bar after foo:bar() -> 0 end.">>},
          {rec_3,
           <<"t() -> receive after 1 -> ok end.">>},
          {rec_4,
           <<"t() -> receive {X,Y} -> {a,X,Y} end.">>},
          {rec_5,
           <<"t() -> receive
                         {X,Y} ->
                             {X,Y};
                         Z ->
                             Z
                     after
                         foo:bar() ->
                             {3,4}
                     end.">>}
          ],
    ?line compile(Config, Ts),
    ok.

bits(suite) ->
    [];
bits(Config) when is_list(Config) ->
    Ts = [{bit_1, % copied from shell_SUITE
           <<"t() -> <<(<<\"abc\">>):3/binary>>.">>},
          {bit_2,
           <<"t() -> <<(<<\"abc\">>)/binary>>.">>},
          {bit_3,
           <<"t() -> <<3.14:64/float>>.">>},
          {bit_4,
           <<"t() -> <<-20/signed>> = <<-20>>.">>},
          {bit_5,
           <<"t() -> <<-300:16/signed>> = <<-300:16>>.">>},
          {bit_6,
           <<"t() -> <<-(1 bsl 29):32/signed>> = <<-(1 bsl 29):32>>.">>},
          {bit_7,
           <<"t() -> <<A:4,B:4,C:4,D:4,E:4,F:4>> = <<\"hej\">>.">>},
          {bit_8,
           <<"t() -> <<>>.">>},
          {bit_9,
           <<"">>}
          ],
    ?line compile(Config, Ts),
    ?line ok = pp_expr(<<"<<(list_to_binary([1,2]))/binary>>">>),
    ?line ok = pp_expr(
          <<"<<(list_to_binary([1,2])):all/binary-unit:8-unsigned-big>>">>),
    ?line ok = pp_expr(<<"<<<<\"hej\">>/binary>>">>),
    ?line ok = pp_expr(<<"<<(foo:bar())/binary>>">>),
    ?line ok = pp_expr(<<"<<(a)/binary>>">>),
    ?line ok = pp_expr(<<"<<a/binary>>">>),
    ?line ok = pp_expr(<<"<<{a,b}/binary>>">>),
    ?line ok = pp_expr(<<"<<{foo:bar(),b}/binary>>">>),
    ?line ok = pp_expr(<<"<<(foo:bar()):(foo:bar())/binary>>">>),
    ok.

head_tail(suite) ->
    [];
head_tail(Config) when is_list(Config) ->
    Ts = [{list_1,
           <<"t() -> [a | b].">>},
          {list_2,
           <<"t() -> [a,b,$\n].">>},
          {list_3,
           <<"t() -> [].">>},
          {list_4,
           <<"t() -> [a].">>},
          {list_5,
           <<"t() ->
               [foo:bar(lkjljlskdfj, klsdajflds, sdafkljsdlfkjdas, kjlsdadjl),
               bar:foo(kljlkjsdf, lkjsdlfj, [kljsfj, sdfdsfsad])].">>}
          ],
    ?line compile(Config, Ts),
    ok.

cond1(suite) ->
    [];
cond1(Config) when is_list(Config) ->
    C = {'cond',1,[{clause,2,[],[[{tuple,2,[{atom,2,foo},{atom,2,bar}]}]],
                    [{cons,3,{atom,3,a},{cons,3,{atom,3,b},{nil,3}}}]},
                   {clause,4,[],[[{atom,4,true}]],
                    [{tuple,5,[{atom,5,x},{atom,5,y}]}]}]},
    ?line CChars = lists:flatten(erl_pp:expr(C)),
%    ?line "cond {foo,bar} -> [a,b]; true -> {x,y} end" = CChars,
    ?line "cond\n"
          "    {foo,bar} ->\n"
          "        [a,b];\n"
          "    true ->\n"
          "        {x,y}\n"
          "end" = CChars,
%     ?line ok = pp_expr(<<"cond
%                               {foo,bar} ->
%                                   [a,b];
%                               true ->
%                                   {x,y}
%                           end">>),
    ok.

block(suite) ->
    [];
block(Config) when is_list(Config) ->
    Ts = [{block_1,
           <<"t() -> begin a,{c,d} end.">>}
          ],
    ?line compile(Config, Ts),
    ok.

case1(suite) ->
    [];
case1(Config) when is_list(Config) ->
    Ts = [{case_1,
           <<"t() -> case {foo,bar} of
                         {A,B} when true ->
                             [A,B];
                         _ ->
                             foo
                     end.">>}
          ],
    ?line compile(Config, Ts),
    ?line ok = pp_expr(<<"case
                              erl_internal:bif(M,F,length(Args))
                          of
                              true ->
                                  call(N,Args,Prec,Hook);
                              false ->
                                  call(Name,Args,Prec,Hook)
                          end">>),
    ok.

ops(suite) ->
    [];
ops(Config) when is_list(Config) ->
    Ts = [{ops_1,
           <<"t() -> {a,b} + (3 - 2) + 4.">>},
          {ops_2,
           <<"t() -> a - (3 + 4).">>},
          {ops_3,
           <<"t() -> - (- (- (- (- 3)))).">>}
          ],
    ?line compile(Config, Ts),
    ok.

messages(suite) ->
    [];
messages(Config) when is_list(Config) ->
    ?line true = "{error,{some,\"error\"}}\n" =:=
        lists:flatten(erl_pp:form({error,{some,"error"}})),
    ?line true = "{warning,{some,\"warning\"}}\n" =:=
        lists:flatten(erl_pp:form({warning,{some,"warning"}})),
    ?line true = "\n" =:= lists:flatten(erl_pp:form({eof,0})),
    ok.

old_mnemosyne_syntax(Config) when is_list(Config) ->
    %% Since we have kept the ':-' token,
    %% better test that we can pretty print it.
    R = {rule,12,sales,2,
         [{clause,12,
           [{var,12,'E'},{atom,12,employee}],
           [],
           [{generate,13,
             {var,13,'E'},
             {call,13,{atom,13,table},[{atom,13,employee}]}},
            {match,14,
             {record_field,14,{var,14,'E'},{atom,14,salary}},
             {atom,14,sales}}]}]},
    ?line "sales(E, employee) :-\n"
          "    E <- table(employee),\n"
          "    E.salary = sales.\n" =
        lists:flatten(erl_pp:form(R)),
    ok.



import_export(suite) ->
    [];
import_export(Config) when is_list(Config) ->
    Ts = [{import_1,
           <<"-import(lists, [max/1, reverse/1]).
              -import(sofs, []).
              t(L) ->
                  max(reverse(L)).">>},
          {export_1,
           <<"-export([t/0]).
              -export([]).
              t() -> [].">>},
          {qlc_1,
           <<"-include_lib(\"stdlib/include/qlc.hrl\").
              t() -> qlc:q([X || X <- []]).">>}
          ],
    ?line compile(Config, Ts),
    ok.

misc_attrs(suite) ->
    [];
misc_attrs(Config) when is_list(Config) ->
    ?line ok = pp_forms(<<"-module(m). ">>),
    ?line ok = pp_forms(<<"-module(m, [Aafjlksfjdlsjflsdfjlsdjflkdsfjlk,"
                          "Blsjfdlslfjsdf]). ">>),
    ?line ok = pp_forms(<<"-export([]). ">>),
    ?line ok = pp_forms(<<"-export([foo/2, bar/0]). ">>),
    ?line ok = pp_forms(<<"-export([bar/0]). ">>),
    ?line ok = pp_forms(<<"-import(lists, []). ">>),
    ?line ok = pp_forms(<<"-import(lists, [map/2]). ">>),
    ?line ok = pp_forms(<<"-import(lists, [map/2, foreach/2]). ">>),
    ?line ok = pp_forms(<<"-'wild '({attr2,3}). ">>),
    ?line ok = pp_forms(<<"-record(a, {b,c}). ">>),
    ?line ok = pp_forms(<<"-record(' a ', {}). ">>),
    ?line ok = pp_forms(<<"-record(' a ', {foo = foo:bar()}). ">>),

    ok.

hook(suite) ->
    [];
hook(Config) when is_list(Config) ->
    F = fun(H) -> H end,
    do_hook(F).

do_hook(HookFun) ->
    Lc = parse_expr(binary_to_list(<<"[X || X <- [1,2,3]].">>)),
    H = HookFun(fun hook/4),
    Expr = {call,0,{atom,0,fff},[{foo,Lc},{foo,Lc},{foo,Lc}]},
    EChars = lists:flatten(erl_pp:expr(Expr, 0, H)),
    Call = {call,0,{atom,0,foo},[Lc]},
    Expr2 = {call,0,{atom,0,fff},[Call,Call,Call]},
    EChars2 = erl_pp:exprs([Expr2]),
    ?line true = EChars =:= lists:flatten(EChars2),

    EsChars = erl_pp:exprs([Expr], H),
    ?line true = EChars =:= lists:flatten(EsChars),

    F = {function,1,ffff,0,[{clause,1,[],[],[Expr]}]},
    FuncChars = lists:flatten(erl_pp:function(F, H)),
    F2 = {function,1,ffff,0,[{clause,1,[],[],[Expr2]}]},
    FuncChars2 = erl_pp:function(F2),
    ?line true = FuncChars =:= lists:flatten(FuncChars2),
    FFormChars = erl_pp:form(F, H),
    ?line true = FuncChars =:= lists:flatten(FFormChars),

    A = {attribute,1,record,{r,[{record_field,1,{atom,1,a},Expr}]}},
    AChars = lists:flatten(erl_pp:attribute(A, H)),
    A2 = {attribute,1,record,{r,[{record_field,1,{atom,1,a},Expr2}]}},
    AChars2 = erl_pp:attribute(A2),
    ?line true = AChars =:= lists:flatten(AChars2),
    AFormChars = erl_pp:form(A, H),
    ?line true = AChars =:= lists:flatten(AFormChars),

    R = {rule,0,sales,0,
         [{clause,0,[{var,0,'E'},{atom,0,employee}],[],
           [{generate,2,{var,2,'E'},
             {call,2,{atom,2,table},[{atom,2,employee}]}},
            {match,3,
             {record_field,3,{var,3,'E'},{atom,3,salary}},
             {foo,Expr}}]}]},
    RChars = lists:flatten(erl_pp:rule(R, H)),
    R2 = {rule,0,sales,0,
          [{clause,0,[{var,0,'E'},{atom,0,employee}],[],
            [{generate,2,{var,2,'E'},
              {call,2,{atom,2,table},[{atom,2,employee}]}},
             {match,3,
              {record_field,3,{var,3,'E'},{atom,3,salary}},
              {call,0,{atom,0,foo},[Expr2]}}]}]},
    RChars2 = erl_pp:rule(R2),
    ?line true = RChars =:= lists:flatten(RChars2),
    ARChars = erl_pp:form(R, H),
    ?line true = RChars =:= lists:flatten(ARChars),

    ?line "INVALID-FORM:{foo,bar}:" = lists:flatten(erl_pp:expr({foo,bar})),

    %% A list (as before R6), not a list of lists.
    G = [{op,1,'>',{atom,1,a},{foo,{atom,1,b}}}], % not a proper guard
    GChars = lists:flatten(erl_pp:guard(G, H)),
    G2 = [{op,1,'>',{atom,1,a},
           {call,0,{atom,0,foo},[{atom,1,b}]}}], % not a proper guard
    GChars2 = erl_pp:guard(G2),
    ?line true = GChars =:= lists:flatten(GChars2),

    EH = HookFun({?MODULE, ehook, [foo,bar]}),
    XEChars = erl_pp:expr(Expr, -1, EH),
    ?line true = remove_indentation(EChars) =:= lists:flatten(XEChars),
    XEChars2 = erl_pp:expr(Expr, EH),
    ?line true = EChars =:= lists:flatten(XEChars2),

    %% Note: no leading spaces before "begin".
    Block = {block,0,[{match,0,{var,0,'A'},{integer,0,3}},
                      {atom,0,true}]},
    ?line "begin\n                     A =" ++ _ =
               lists:flatten(erl_pp:expr(Block, 17, none)),

    %% Special...
    ?line true =
        "{some,value}" =:= lists:flatten(erl_pp:expr({value,0,{some,value}})),

    %% Silly...
    ?line true =
        "if true -> 0 end" =:=
              flat_expr({'if',0,[{clause,0,[],[],[{atom,0,0}]}]}),

    %% More compatibility: before R6
    OldIf = {'if',0,[{clause,0,[],[{atom,0,true}],[{atom,0,b}]}]},
    NewIf = {'if',0,[{clause,0,[],[[{atom,0,true}]],[{atom,0,b}]}]},
    OldIfChars = lists:flatten(erl_pp:expr(OldIf)),
    NewIfChars = lists:flatten(erl_pp:expr(NewIf)),
    ?line true = OldIfChars =:= NewIfChars,

    ok.

remove_indentation(S) ->
    %% T is for the very special leaf(" ") used for lc and bc.
    T = re:replace(S, " \n *", "", [{return,list},global]),
    re:replace(T, "\n *", " ", [{return,list},global]).

ehook(HE, I, P, H, foo, bar) ->
    hook(HE, I, P, H).

hook({foo,E}, I, P, H) ->
    erl_pp:expr({call,0,{atom,0,foo},[E]}, I, P, H).

neg_indent(suite) ->
    [];
neg_indent(Config) when is_list(Config) ->
    ?line ok = pp_expr(<<"begin a end">>),
    ?line ok = pp_expr(<<"begin a,b end">>),
    ?line ok = pp_expr(<<"try a,b,c
                              catch exit:_ -> d;
                                    throw:_ -> t;
                                    error:{foo,bar} -> foo,
                                                       bar
                              end">>),
    ?line ok = pp_expr(
            <<"fun() ->
                  F = fun(A, B) when ((A#r1.a) orelse (B#r2.a))
                                     or (B#r2.b) or (A#r1.b) ->
                            true;
                         (_, _) -> false
                      end,
                  true = F(#r1{a = false, b = false},
                           #r2{a = false, b = true}),
                  false = F(#r1{a = true, b = true},
                            #r1{a = false, b = true}),
                  ok
                       end()">>),

    ?line ok = pp_expr(<<"[X || X <- a, true]">>),
    ?line ok = pp_expr(<<"{[a,b,c],[d,e|f]}">>),
    ?line ok = pp_expr(<<"f(a,b,c)">>),
    ?line ok = pp_expr(<<"fun() when a,b;c,d -> a end">>),
    ?line ok = pp_expr(<<"<<34:32,17:32>>">>),
    ?line ok = pp_expr(<<"if a,b,c -> d; e,f,g -> h,i end">>),
    ?line ok = pp_expr(<<"if a -> d; c -> d end">>),
    ?line ok = pp_expr(<<"receive after 1 -> 2 end">>),
    ?line ok = pp_expr(<<"begin a,b,c end">>),

    ?line "\"\"" = flat_expr({string,0,""}),
    ?line ok = pp_expr(<<"\"abc\"">>),
    ?line ok = pp_expr(<<"\"abc\n\n\n\n\nkjsd\n\n\n\n\nkljsddf\n\n\n\n\n"
                         "klafd\n\n\n\n\nkljsdf\n\n\n\n\nsdf\n\n\n\n\n\"">>),
    ?line ok = pp_expr(<<"fkjlskljklkkljlkjlkjkljlkjsljklf"
                          "lsdjlfdsjlfjsdlfjdslfjdlsjfsdjfklsdkfjsdf("
                          "\"abc\n\n\n\n\nkjsd\n\n\n\n\nkljsddf\n\n\n\n\n"
                          "kljsafd\n\n\n\n\nkljsdf\n\n\n\n\nkjsdf"
                          "\n\n\n\n\n\")">>),

    %% fun-info is skipped when everything is to fit on one single line
    Fun1 = {'fun',1,{function,t,0},{0,45353021,'-t/0-fun-0-'}},
    ?line "fun t/0" = flat_expr(Fun1),
    Fun2 = {'fun',2,{clauses,[{clause,2,[],[],[{atom,3,true}]}]},
            {0,108059557,'-t/0-fun-0-'}},
    ?line "fun() -> true end" = flat_expr(Fun2),

    ok.


otp_6321(doc) ->
    "OTP_6321. Bug fix of exprs().";
otp_6321(suite) -> [];
otp_6321(Config) when is_list(Config) ->
    Str = "S = hopp, {hej, S}. ",
    {done, {ok, Tokens, _EndLine}, ""} = erl_scan:tokens("", Str, _L=1),
    {ok, Exprs} = erl_parse:parse_exprs(Tokens),
    "S = hopp, {hej,S}" = lists:flatten(erl_pp:exprs(Exprs)),
    ok.

otp_6911(doc) ->
    "OTP_6911. More newlines.";
otp_6911(suite) -> [];
otp_6911(Config) when is_list(Config) ->
    F = {function,5,thomas,1,
         [{clause,5,
           [{var,5,'X'}],
           [],
           [{'case',6,
             {var,6,'X'},
             [{clause,7,[{atom,7,true}],[],[{integer,7,12}]},
              {clause,8,[{atom,8,false}],[],[{integer,8,14}]}]}]}]},
    ?line Chars = lists:flatten(erl_pp:form(F)),
    ?line "thomas(X) ->\n"
          "    case X of\n"
          "        true ->\n"
          "            12;\n"
          "        false ->\n"
          "            14\n"
          "    end.\n" = Chars,
    ?line ok = pp_expr(<<"case X of true -> 12; false -> 14 end">>),
    ?line ok = pp_expr(<<"receive after 1 -> ok end">>),
    ok.

otp_6914(doc) ->
    "OTP_6914. Binary comprehensions.";
otp_6914(suite) -> [];
otp_6914(Config) when is_list(Config) ->
    ?line ok = pp_expr(<<"<< <<B:1>> || B <- [0,1,1] >>">>),
    ?line ok = pp_expr(<<"[ B || <<B:1>> <= <<\"hi\">>]">>),
    ?line ok = pp_expr(<<"<< <<1:1>> || true >>">>),
    ok.

otp_8150(doc) ->
    "OTP_8150. Types.";
otp_8150(suite) -> [];
otp_8150(Config) when is_list(Config) ->
    ?line _ = [{N,ok} = {N,pp_forms(B)} ||
                  {N,B} <- type_examples()
                     ],
    ok.

otp_8238(doc) ->
    "OTP_8238. Bugfix 'E'.";
otp_8238(suite) -> [];
otp_8238(Config) when is_list(Config) ->
    Ex = [<<"-record(rec1, {}).\n"
            "-record(rec2, {a, b}).\n"
            "-record(rec3, {f123, g, h}).\n">>,
          <<"-type line() :: integer().\n">>,
          <<"-type info_line() :: integer() | term().\n">>,
          <<"-type column() :: pos_integer().\n">>,
          [["\n", B] || {_,B} <- type_examples()],
          <<"t1(T) ->\n"
            "    foo:bar(#rec1{}, #rec2{}),\n"
            "    T.\n"
            "t2() ->\n"
            "    #r{}.\n">>
         ],
    ?line compile(Config, [{otp_8238,iolist_to_binary(Ex)}]),
    ok.

type_examples() ->
    [{ex1,<<"-type ann() :: Var :: integer(). ">>},
     {ex2,<<"-type ann2() :: Var :: "
            "'return' | 'return_white_spaces' | 'return_comments'"
            " | 'text' | ann(). ">>},
     {ex3,<<"-type paren() :: (ann2()). ">>},
     {ex4,<<"-type t1() :: atom(). ">>},
     {ex5,<<"-type t2() :: [t1()]. ">>},
     {ex6,<<"-type t3(Atom) :: integer(Atom). ">>},
     {ex7,<<"-type '\\'t::4'() :: t3('\\'foobar'). ">>},
     {ex8,<<"-type t5() :: {t1(), t3(foo)}. ">>},
     {ex9,<<"-type t6() :: 1 | 2 | 3 | 'foo' | 'bar'. ">>},
     {ex10,<<"-type t7() :: []. ">>},
     {ex11,<<"-type t71() :: [_]. ">>},
     {ex12,<<"-type t8() :: {any(),none(),pid(),port(),"
       "reference(),float()}. ">>},
     {ex13,<<"-type t9() :: [1|2|3|foo|bar] | "
       "list(a | b | c) | t71(). ">>},
     {ex14,<<"-type t10() :: {1|2|3|foo|t9()} | {}. ">>},
     {ex15,<<"-type t11() :: 1..2. ">>},
     {ex16,<<"-type t13() :: maybe_improper_list(integer(), t11()). ">>},
     {ex17,<<"-type t14() :: [erl_scan:foo() | "
       "erl_scan:bar(34, 92) | t13() | m:f(integer() | <<_:_*16>>)]. ">>},
     {ex18,<<"-type t15() :: {binary(),<<>>,<<_:34>>,<<_:_*42>>,"
       "<<_:3,_:_*14>>,<<>>} | [<<>>|<<_:34>>|<<_:16>>|"
       "<<_:3,_:_*1472>>|<<_:19,_:_*14>>| <<_:34>>|"
       "<<_:34>>|<<_:34>>|<<_:34>>]. ">>},
     {ex19,<<"-type t16() :: fun(). ">>},
     {ex20,<<"-type t17() :: fun((...) -> paren()). ">>},
     {ex21,<<"-type t18() :: fun(() -> t17() | t16()). ">>},
     {ex22,<<"-type t19() :: fun((t18()) -> t16()) |"
       "fun((nonempty_maybe_improper_list('integer', any())|"
       "1|2|3|a|b|<<_:3,_:_*14>>|integer()) ->"
       "nonempty_maybe_improper_list('integer', any())|"
       "1|2|3|a|b|<<_:3,_:_*14>>|integer()). ">>},
     {ex23,<<"-type t20() :: [t19(), ...]. ">>},
     {ex24,<<"-type t21() :: tuple(). ">>},
     {ex25,<<"-type t21(A) :: A. ">>},
     {ex26,<<"-type t22() :: t21(integer()). ">>},
     {ex27,<<"-type t23() :: #rec1{}. ">>},
     {ex28,<<"-type t24() :: #rec2{a :: t23(), b :: [atom()]}. ">>},
     {ex29,<<"-type t25() :: #rec3{f123 :: [t24() | "
       "1|2|3|4|a|b|c|d| "
       "nonempty_maybe_improper_list(integer, any())]}. ">>},
     {ex30,<<"-type t99() ::"
       "{t2(),'\\'t::4'(),t5(),t6(),t7(),t8(),t10(),t14(),"
       "t15(),t20(),t21(), t22(),t25()}. ">>},
     {ex31,<<"-spec t1(FooBar :: t99()) -> t99();"
                          "(t2()) -> t2();"
                          "('\\'t::4'()) -> '\\'t::4'() when is_subtype('\\'t::4'(), t24);"
                          "(t23()) -> t23() when is_subtype(t23(), atom()),"
                          "                      is_subtype(t23(), t14());"
                          "(t24()) -> t24() when is_subtype(t24(), atom()),"
                          "                      is_subtype(t24(), t14()),"
                          "                      is_subtype(t24(), '\\'t::4'()).">>},
     {ex32,<<"-spec mod:t2() -> any(). ">>},
     {ex33,<<"-opaque attributes_data() :: "
       "[{'column', column()} | {'line', info_line()} |"
       " {'text', string()}] |  {line(),column()}. ">>},
     {ex34,<<"-record(r,{"
       "f1 :: attributes_data(),"
           "f222 = foo:bar(34, #rec3{}, 234234234423, "
           "               aassdsfsdfsdf, 2234242323) :: "
           "               [t24() | 1|2|3|4|a|b|c|d| "
           "     nonempty_maybe_improper_list(integer, any())],"
           "f333 :: [t24() | 1|2|3|4|a|b|c|d| "
           "    nonempty_maybe_improper_list(integer, any())],"
           "f3 = x:y(),"
           "f4 = x:z() :: t99(),"
           "f17 :: 'undefined',"
           "f18 :: 1 | 2 | 'undefined',"
           "f19 = 3 :: integer()|undefined,"
           "f5 = 3 :: undefined|integer()}). ">>}].

otp_8473(doc) ->
    "OTP_8473. Bugfix abstract type 'fun'.";
otp_8473(suite) -> [];
otp_8473(Config) when is_list(Config) ->
    Ex = [{ex1,<<"-type 'fun'(A) :: A.\n"
                 "-type funkar() :: 'fun'(fun((integer()) -> atom())).\n">>}],
    ?line _ = [{N,ok} = {N,pp_forms(B)} ||
                  {N,B} <- Ex],
    ok.

otp_8522(doc) ->
    "OTP_8522. Avoid duplicated 'undefined' in record field types.";
otp_8522(suite) -> [];
otp_8522(Config) when is_list(Config) ->
    FileName = filename('otp_8522.erl', Config),
    C = <<"-module(otp_8522).\n"
          "-record(r, {f1 :: undefined,\n"
          "            f2 :: A :: undefined,\n"
          "            f3 :: (undefined),\n"
          "            f4 :: x | y | undefined | z,\n"
          "            f5 :: a}).\n">>,
    ?line ok = file:write_file(FileName, C),
    ?line {ok, _} = compile:file(FileName, [{outdir,?privdir},debug_info]),
    BF = filename("otp_8522", Config),
    ?line {ok, A} = beam_lib:chunks(BF, [abstract_code]),
    ?line 5 = count_atom(A, undefined),
    ok.

count_atom(A, A) ->
    1;
count_atom(T, A) when is_tuple(T) ->
    count_atom(tuple_to_list(T), A);
count_atom(L, A) when is_list(L) ->
    lists:sum([count_atom(T, A) || T <- L]);
count_atom(_, _) ->
    0.

otp_8567(doc) ->
    "OTP_8567. Avoid duplicated 'undefined' in record field types.";
otp_8567(suite) -> [];
otp_8567(Config) when is_list(Config) ->
    FileName = filename('otp_8567.erl', Config),
    C = <<"-module otp_8567.\n"
          "-compile export_all.\n"
          "-spec(a).\n"
          "-record r, {a}.\n"
          "-record s, {a :: integer()}.\n"
          "-type t() :: {#r{},#s{}}.\n">>,
    ?line ok = file:write_file(FileName, C),
    ?line {error,[{_,[{3,erl_parse,["syntax error before: ","')'"]}]}],_} =
        compile:file(FileName, [return]),

    F = <<"-module(otp_8567).\n"
          "-compile(export_all).\n"
          "-record(t, {a}).\n"
          "-record(u, {a :: integer()}).\n"
          "-opaque ot() :: {#t{}, #u{}}.\n"
          "-opaque(ot1() :: atom()).\n"
          "-type a() :: integer().\n"
          "-spec t() -> a().\n"
          "t() ->\n"
          "    3.\n"
          "\n"
          "-spec(t1/1 :: (ot()) -> ot1()).\n"
          "t1(A) ->\n"
          "    A.\n"
          "\n"
          "-spec(t2 (ot()) -> ot1()).\n"
          "t2(A) ->\n"
          "    A.\n"
          "\n"
          "-spec(otp_8567:t3/1 :: (ot()) -> ot1()).\n"
          "t3(A) ->\n"
          "    A.\n"
          "\n"
          "-spec(otp_8567:t4 (ot()) -> ot1()).\n"
          "t4(A) ->\n"
          "    A.\n">>,
    ?line ok = pp_forms(F),

    ok.

otp_8664(doc) ->
    "OTP_8664. Types with integer expressions.";
otp_8664(suite) -> [];
otp_8664(Config) when is_list(Config) ->
    FileName = filename('otp_8664.erl', Config),
    C1 = <<"-module(otp_8664).\n"
           "-export([t/0]).\n"
           "-define(A, -3).\n"
           "-define(B, (?A*(-1 band (((2)))))).\n"
           "-type t1() :: ?B | ?A.\n"
           "-type t2() :: ?B-1 .. -?B.\n"
           "-type t3() :: 9 band (8 - 3) | 1+2 | 5 band 3.\n"
           "-type b1() :: <<_:_*(3-(-1))>>\n"
           "            | <<_:(-(?B))>>\n"
           "            | <<_:4>>.\n"
           "-type u() :: 1 .. 2 | 3.. 4 | (8-3) ..6 | 5+0..6.\n"
           "-type t() :: t1() | t2() | t3() | b1() | u().\n"
           "-spec t() -> t().\n"
           "t() -> 3.\n">>,
    ?line ok = file:write_file(FileName, C1),
    ?line {ok, _, []} = compile:file(FileName, [return]),

    C2 = <<"-module(otp_8664).\n"
           "-export([t/0]).\n"
           "-spec t() -> 9 and 4.\n"
           "t() -> 0.\n">>,
    ?line ok = file:write_file(FileName, C2),
    ?line {error,[{_,[{3,erl_lint,{type_syntax,integer}}]}],_} =
        compile:file(FileName, [return]),

    ok.

otp_9147(doc) ->
    "OTP_9147. Create well-formed types when adding 'undefined'.";
otp_9147(suite) -> [];
otp_9147(Config) when is_list(Config) ->
    FileName = filename('otp_9147.erl', Config),
    C1 = <<"-module(otp_9147).\n"
           "-export_type([undef/0]).\n"
           "-record(undef, {f1 :: F1 :: a | b}).\n"
           "-type undef() :: #undef{}.\n">>,
    ?line ok = file:write_file(FileName, C1),
    ?line {ok, _, []} = 
       compile:file(FileName, [return,'P',{outdir,?privdir}]),
    PFileName = filename('otp_9147.P', Config),
    ?line {ok, Bin} = file:read_file(PFileName),
    %% The parentheses around "F1 :: a | b" are new (bugfix).
    ?line true = 
        lists:member("-record(undef,{f1 :: undefined | (F1 :: a | b)}).",
                     string:tokens(binary_to_list(Bin), "\n")),
    ok.

otp_10302(doc) ->
    "OTP-10302. Unicode characters scanner/parser.";
otp_10302(suite) -> [];
otp_10302(Config) when is_list(Config) ->
    Ts = [{uni_1,
           <<"t() -> <<(<<\"abc\\x{aaa}\">>):3/binary>>.">>}
          ],
    compile(Config, Ts),
    ok = pp_expr(<<"$\\x{aaa}">>),
    ok = pp_expr(<<"\"1\\x{aaa}\"">>),
    ok = pp_expr(<<"<<<<\"hej\">>/binary>>">>),
    ok = pp_expr(<<"<< <<\"1\\x{aaa}\">>/binary>>">>),

    U = [{encoding,unicode}],

    do_hook(fun(H) -> [{hook,H}] end),
    do_hook(fun(H) -> [{hook,H}]++U end),

    ok = pp_expr(<<"$\\x{aaa}">>, [{hook,fun hook/4}]),

    Opts = [{hook, fun unicode_hook/4},{encoding,unicode}],
    Lc = parse_expr("[X || X <- [\"\x{400}\",\"\xFF\"]]."),
    Expr = {call,0,{atom,0,fff},[{foo,{foo,Lc}},{foo,{foo,Lc}}]},
    EChars = lists:flatten(erl_pp:expr(Expr, 0, Opts)),
    Call = {call,0,{atom,0,foo},[{call,0,{atom,0,foo},[Lc]}]},
    Expr2 = {call,0,{atom,0,fff},[Call,Call]},
    EChars2 = erl_pp:exprs([Expr2], U),
    EChars = lists:flatten(EChars2),
    [$\x{400},$\x{400}] = [C || C <- EChars, C > 255],

    ok = pp_forms(<<"function() -> {\"\x{400}\",$\x{400}}. "/utf8>>, U),
    ok = pp_forms("function() -> {\"\x{400}\",$\x{400}}. ", []),
    ok.

unicode_hook({foo,E}, I, P, H) ->
    erl_pp:expr({call,0,{atom,0,foo},[E]}, I, P, H).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compile(Config, Tests) ->
    F = fun({N,P}, BadL) ->
                case catch compile_file(Config, P) of
                    ok ->
                        case pp_forms(P) of
                            ok ->
                                BadL;
                            not_ok ->
                                ?t:format("~nTest ~p failed.~n", [N]),
                                fail()
                        end;
                    Bad ->
                        ?t:format("~nTest ~p failed. got~n  ~p~n",
                                  [N, Bad]),
                        fail()
                end
        end,
    lists:foldl(F, [], Tests).

compile_file(Config, Test0) ->
    case compile_file(Config, Test0, ['E']) of
        {ok, RootFile} ->
            File = RootFile ++ ".E",
            {ok, Bin0} = file:read_file(File),
            Bin = strip_module_info(Bin0),
            %% A very simple check: just try to compile the output.
            case compile_file(Config, Bin, []) of
                {ok, RootFile2} ->
                    File2 = RootFile2 ++ ".E",
                    {ok, Bin1} = file:read_file(File2),
                    case Bin0 =:= Bin1 of
                        true ->
                            test_max_line(binary_to_list(Bin));
                        false ->
                            {error, file_contents_modified, {Bin0, Bin1}}
                    end;
                Error ->
                    {error, could_not_compile_E_file, Error}
            end;
        Error ->
            Error
    end.

compile_file(Config, Test0, Opts0) ->
    FileName = filename('erl_pp_test.erl', Config),
    Test = list_to_binary(["-module(erl_pp_test). "
                           "-compile(export_all). ",
                           Test0]),
    Opts = [export_all,return,nowarn_unused_record,{outdir,?privdir} | Opts0],
    ok = file:write_file(FileName, Test),
    case compile:file(FileName, Opts) of
        {ok, _M, _Ws} ->
            {ok, filename:rootname(FileName)};
        Error -> Error
    end.

strip_module_info(Bin) ->
    {match, [{Start,_Len}|_]} = re:run(Bin, "module_info"),
    <<R:Start/binary,_/binary>> = Bin,
    R.

flat_expr(Expr) ->
    lists:flatten(erl_pp:expr(Expr, -1, none)).

pp_forms(Bin) ->
    pp_forms(Bin, none).

pp_forms(Bin, Options) when is_binary(Bin) ->
    pp_forms(to_list(Bin, Options), Options);
pp_forms(List, Options) when is_list(List) ->
    PP1 = (catch parse_and_pp_forms(List, Options)),
    PP2 = (catch parse_and_pp_forms(PP1, Options)),
    case PP1 =:= PP2 of % same line numbers
        true ->
            test_max_line(PP1);
        false ->
            not_ok
    end.

parse_and_pp_forms(String, Options) ->
    lists:append(lists:map(fun(AF) -> erl_pp:form(AF, Options)
                           end, parse_forms(String))).

parse_forms(Chars) ->
    String = lists:flatten(Chars),
    parse_forms2(String, [], 1, []).

parse_forms2([], _Cont, _Line, Forms) ->
    lists:reverse(Forms);
parse_forms2(String, Cont0, Line, Forms) ->
    case erl_scan:tokens(Cont0, String, Line, [unicode]) of
        {done, {ok, Tokens, EndLine}, Chars} ->
            {ok, Form} = erl_parse:parse_form(Tokens),
            parse_forms2(Chars, [], EndLine, [Form | Forms]);
        {more, Cont} when element(4, Cont) =:= [] ->
            %% extra spaces after forms...
            parse_forms2([], Cont, Line, Forms);
        {more, Cont} ->
            %% final dot needs a space...
            parse_forms2(" ", Cont, Line, Forms)
    end.

pp_expr(Bin) ->
    pp_expr(Bin, none).

%% Final dot is added.
pp_expr(Bin, Options) when is_binary(Bin) ->
    pp_expr(to_list(Bin, Options), Options);
pp_expr(List, Options) when is_list(List) ->
    PP1 = (catch parse_and_pp_expr(List, 0, Options)),
    PPneg = (catch parse_and_pp_expr(List, -1, Options)),
    PP2 = (catch parse_and_pp_expr(PPneg, 0, Options)),
    if
        PP1 =:= PP2 -> % same line numbers
            case
                (test_max_line(PP1) =:= ok) and (test_new_line(PPneg) =:= ok)
            of
                true ->
                    ok;
                false ->
                    not_ok
            end;
        true ->
            not_ok
    end.

parse_and_pp_expr(String, Indent, Options) ->
    StringDot = lists:flatten(String) ++ ".",
    erl_pp:expr(parse_expr(StringDot), Indent, Options).

parse_expr(Chars) ->
    {ok, Tokens, _} = erl_scan:string(Chars, 1, [unicode]),
    {ok, [Expr]} = erl_parse:parse_exprs(Tokens),
    Expr.

to_list(Bin, Options) when is_list(Options) ->
    case proplists:get_value(encoding, Options) of
        unicode -> unicode:characters_to_list(Bin);
        encoding -> binary_to_list(Bin);
        undefined -> binary_to_list(Bin)
    end;
to_list(Bin, _Hook) ->
    binary_to_list(Bin).

test_new_line(String) ->
    case string:chr(String, $\n) of
        0 -> ok;
        _ -> not_ok
    end.

test_max_line(String) ->
    case max_line(String) of
        ML when ML > 100 ->
            {error, max_line_too_big, {ML,String}};
        _ML ->
            ok
    end.

max_line(String) ->
    lists:max([0 | [length(Sub) ||
                       Sub <- string:tokens(String, "\n"),
                       string:substr(Sub, 1, 5) =/= "-file"]]).

filename(Name, Config) when is_atom(Name) ->
    filename(atom_to_list(Name), Config);
filename(Name, Config) ->
    filename:join(?privdir, Name).

fail() ->
    io:format("failed~n"),
    ?t:fail().
