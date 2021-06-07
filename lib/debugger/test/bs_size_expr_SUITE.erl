%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2019. All Rights Reserved.
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

-module(bs_size_expr_SUITE).
-compile(nowarn_shadow_vars).

-export([all/0,suite/0,groups/0,init_per_suite/1, end_per_suite/1,
         init_per_group/2,end_per_group/2,
         init_per_testcase/2,end_per_testcase/2,
         basic/1,size_shadow/1,complex/1,
         recv/1,no_match/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() ->
    [{group,p}].

groups() ->
    [{p,[],
      [basic,
       size_shadow,
       complex,
       recv,
       no_match]}].

init_per_suite(Config) ->
    test_lib:interpret(?MODULE),
    true = lists:member(?MODULE, int:interpreted()),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(Case, Config) when is_atom(Case), is_list(Config) ->
    test_lib:interpret(?MODULE),
    Config.

end_per_testcase(Case, Config) when is_atom(Case), is_list(Config) ->
    ok.

basic(_Config) ->
    <<>> = do_basic(<<1:32>>),
    <<"abcd">> = do_basic(<<2:32,"abcd">>),
    no_match = do_basic(<<0:32>>),
    no_match = do_basic(<<777:32>>),
    ok.

do_basic(Bin) ->
    Res = do_basic_1(Bin),

    Res = do_basic_2({tag,Bin}),
    Res = do_basic_2([list,Bin]),
    6 = do_basic_2({2,4}),

    Res = do_basic_3(Bin),
    Res = do_basic_4(Bin),

    {result,Res} = do_basic_5(Bin),
    case Res of
        no_match ->
            ok;
        _ ->
            {result,{Res,7777777}} = do_basic_5(<<Bin/binary,7777777:32>>)
    end,

    Res.

do_basic_1(<<Sz:32,Tail:(4*Sz-4)/binary>>) ->
    Tail;
do_basic_1(<<_/binary>>) ->
    no_match.

do_basic_2({tag,<<Sz:32,Tail:(4*Sz-4)/binary>>}) ->
    Tail;
do_basic_2([list,<<Sz:32,Tail:((Sz-1)*4)/binary>>]) ->
    Tail;
do_basic_2({A,B}) when is_integer(A), is_integer(B) ->
    A + B;
do_basic_2(_) ->
    no_match.

do_basic_3(Bin) ->
    WordSize = id(4),
    case Bin of
        <<Sz:32,Tail:(WordSize*Sz-WordSize)/binary>> ->
            Tail;
        _ ->
            no_match
    end.

do_basic_4(Bin) ->
    WordSize = id(4),
    F = fun() ->
                case Bin of
                    <<Sz:32,Tail:(WordSize*Sz-WordSize)/binary>> ->
                        Tail;
                    _ ->
                        no_match
                end
        end,
    F().

do_basic_5(Bin) ->
    WordSize = id(4),
    F = fun() ->
                Res = case Bin of
                          <<Sz:32,Tail:(WordSize*Sz-WordSize)/binary,More:(8*WordSize)>> ->
                              {Tail,More};
                          <<Sz:32,Tail:(WordSize*Sz-WordSize)/binary>> ->
                              Tail;
                          _ ->
                              no_match
                      end,
                {result,Res}
        end,
    F().

size_shadow(_Config) ->
    12345678 = size_shadow_1(),
    ok.

size_shadow_1() ->
    L = 8,
    Offset = 16,
    Fs = [fun(<<L:L,B:(L+16)>>) -> B end,
          fun(<<L:L,B:(L+Offset)>>) -> B end,
          fun(A) ->
                  Res = (fun([<<L:L,B:(L+16)>>]) -> B end)([A]),
                  Res = (fun([<<L:L,B:(L+Offset)>>]) -> B end)([A])
          end,
          fun(A) ->
                  Res = (fun({<<L:L,B:(L+16)>>,<<L:L,B:(L+16)>>}) -> B end)({A,A}),
                  Res = (fun({<<L:L,B:(L+Offset)>>,<<L:L,B:(L+16)>>}) -> B end)({A,A}),
                  Res = (fun({<<L:L,B:(L+16)>>,<<L:L,B:(L+Offset)>>}) -> B end)({A,A}),
                  Res = (fun({<<L:L,B:(L+Offset)>>,<<L:L,B:(L+Offset)>>}) -> B end)({A,A})
          end,
          fun(A) ->
                  <<Size:L,_/bits>> = A,
                  Inner = fun([L], {#{key1 := <<L:L,B:(L+Offset)>>,
                                      key2 := <<L:L,B:(L+Offset)>>}, L}) -> B end,
                  Inner([Size], {#{key1 => A,key2 => A},Size})
          end],
    size_shadow_apply(Fs, <<16:8, 12345678:32>>).

size_shadow_apply([F|Fs], Arg) when is_function(F, 1) ->
    size_shadow_apply(Fs, Arg, F(Arg)).

size_shadow_apply([F|Fs], Arg, Res) when is_function(F, 1) ->
    Res = F(Arg),
    size_shadow_apply(Fs, Arg, Res);
size_shadow_apply([], _, Res) ->
    Res.

-record(r, {a,b,c}).
complex(Config) ->
    (fun() ->
             Len = length(id(Config)),
             Bin = << <<I:13>> || I <- lists:seq(1, Len) >>,
             <<Bin:(length(Config))/binary-unit:13>> = Bin
     end)(),

    (fun() ->
             V = id([a,b,c]),
             F = fun(<<V:(bit_size(<<0:(length(V))>>)*8)/signed-integer>>) ->
                         V;
                    ({A,B}) ->
                         A + B
                 end,
             -1 = F(<<-1:(length(V)*8)>>),
             7 = F({3,4})
     end)(),

    (fun() ->
             A = a,
             B = b,
             F = fun(<<A:16,B:16,C:(A+B),D/bits>>) ->
                         {A,B,C,D};
                    (<<A:16,B:16>>) ->
                         {A,B};
                    (<<A:8,B:8>>) ->
                         {A,B}
                 end,
             {13,21,16#cafebeef,<<"more">>} = F(<<13:16,21:16,16#cafebeef:34,"more">>),
             {100,500} = F(<<100:16,500:16>>),
             {157,77} = F(<<157:8,77:8>>),
             {A,B}
     end)(),

    (fun() ->
             Two = id(2),
             F = fun(a, <<_:(#r.a - Two)/binary,Int:8,_/binary>>) -> Int;
                    (b, <<_:(#r.b - Two)/binary,Int:8,_/binary>>) -> Int;
                    (c, <<_:(#r.c - Two)/binary,Int:8,_/binary>>) -> Int
                 end,
             1 = F(a, <<1,2,3>>),
             2 = F(b, <<1,2,3>>),
             3 = F(c, <<1,2,3>>)
     end)(),

    (fun() ->
             Bin = <<1,2,3,4>>,
             F = fun(R) ->
                         <<First:(R#r.a)/binary,Tail/binary>> = Bin,
                         {First,Tail}
                 end,
             {<<>>,<<1,2,3,4>>} = F(#r{a=0}),
             {<<1>>,<<2,3,4>>} = F(#r{a=1}),
             {<<1,2>>,<<3,4>>} = F(#r{a=2}),
             {<<1,2,3>>,<<4>>} = F(#r{a=3}),
             {<<1,2,3,4>>,<<>>} = F(#r{a=4})
     end)(),

    ok.

recv(_Config) ->
    R = fun(Msg) ->
                self() ! Msg,
                Res = receive
                          <<L,I:(L-1)/unit:8,X:32>> -> {I,X};
                          <<L,I:(L-1)/unit:8,X:64>> -> {I,X}
                      end,
                self() ! {tag,[Msg]},
                Res = receive
                          {tag,[<<L,I:(8*(L-1)),X:32>>]} -> {I,X};
                          {tag,[<<L,I:(8*(L-1)),X:64>>]} -> {I,X}
                      end
        end,
    {1234,16#deadbeef} = R(<<3,1234:16,16#deadbeef:32>>),
    {99,16#cafebeeff00d} = R(<<2,99:8,16#cafebeeff00d:64>>),
    ok.

no_match(_Config) ->
    B = id(<<1,2,3,4>>),
    no_match = case B of
                   <<Int:(bit_size(B)-1)>> -> Int;
                   <<Int:(bit_size(B)*2)>> -> Int;
                   <<Int:(length(B))>> -> Int;
                   _ -> no_match
               end,
    no_match = case B of
                   <<L:8,Int2:(is_integer(L))>> -> Int2;
                   <<L:8,Int2:(L+3.0)>> -> Int2;
                   _ -> no_match
               end,
    ok.

id(I) ->
    I.
