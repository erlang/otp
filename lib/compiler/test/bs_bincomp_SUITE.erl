%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2006-2024. All Rights Reserved.
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
%% Originally based on Per Gustafsson's test suite.
%%

-module(bs_bincomp_SUITE).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
         verify_highest_opcode/1,
	 byte_aligned/1,bit_aligned/1,extended_byte_aligned/1,
	 extended_bit_aligned/1,mixed/1,filters/1,trim_coverage/1,
	 nomatch/1,sizes/1,general_expressions/1,
         no_generator/1,zero_pattern/1,multiple_segments/1,
         grab_bag/1]).

-include_lib("common_test/include/ct.hrl").

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [verify_highest_opcode,
     byte_aligned, bit_aligned, extended_byte_aligned,
     extended_bit_aligned, mixed, filters, trim_coverage,
     nomatch, sizes, general_expressions,
     no_generator, zero_pattern, multiple_segments,
     grab_bag].

groups() ->
    [].

init_per_suite(Config) ->
    test_lib:recompile(?MODULE),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
	Config.

end_per_group(_GroupName, Config) ->
	Config.

verify_highest_opcode(_Config) ->
    case ?MODULE of
        bs_construct_r24_SUITE ->
            {ok,Beam} = file:read_file(code:which(?MODULE)),
            case test_lib:highest_opcode(Beam) of
                Highest when Highest =< 176 ->
                    ok;
                TooHigh ->
                    ct:fail({too_high_opcode,TooHigh})
            end;
        bs_construct_r25_SUITE ->
            {ok,Beam} = file:read_file(code:which(?MODULE)),
            case test_lib:highest_opcode(Beam) of
                Highest when Highest =< 180 ->
                    ok;
                TooHigh ->
                    ct:fail({too_high_opcode,TooHigh})
            end;
        _ ->
            ok
    end.

byte_aligned(Config) when is_list(Config) ->
    cs_init(),
    <<"abcdefg">> = cs(<< <<(X+32)>> || <<X>> <= <<"ABCDEFG">> >>),
    <<"AxyzBxyzCxyz">> = cs(<< <<X, "xyz">> || <<X>> <= <<"ABC">> >>),
    <<1:32/little,2:32/little,3:32/little,4:32/little>> =
	cs(<< <<X:32/little>> || <<X:32>> <= <<1:32,2:32,3:32,4:32>> >>),
    cs(<<1:32/little,2:32/little,3:32/little,4:32/little>> =
	   << <<X:32/little>> || <<X:16>> <= <<1:16,2:16,3:16,4:16>> >>),
    cs_end().

bit_aligned(Config) when is_list(Config) ->
    cs_init(),
    <<$a:7,$b:7,$c:7,$d:7,$e:7,$f:7,$g:7>> =
	cs(<< <<(X+32):7>> || <<X>> <= <<"ABCDEFG">> >>),
    <<"ABCDEFG">> = cs(<< <<(X-32)>> || <<X:7>> <= id(<<$a:7,$b:7,$c:7,$d:7,$e:7,$f:7,$g:7>>) >>),
    <<1:31/little,2:31/little,3:31/little,4:31/little>> =
	cs(<< <<X:31/little>> || <<X:31>> <= <<1:31,2:31,3:31,4:31>> >>),
    <<1:31/little,2:31/little,3:31/little,4:31/little>> =
	cs(<< <<X:31/little>> || <<X:15>> <= <<1:15,2:15,3:15,4:15>> >>),
    cs_end().

extended_byte_aligned(Config) when is_list(Config) ->
    cs_init(),
    <<"abcdefg">> = cs(<< <<(X+32)>> || X <- "ABCDEFG" >>),
    "abcdefg" = [(X+32) || <<X>> <= <<"ABCDEFG">>],
    <<1:32/little,2:32/little,3:32/little,4:32/little>> =
	cs(<< <<X:32/little>> || X <- [1,2,3,4] >>),
    [256,512,768,1024] =
	[X || <<X:16/little>> <= <<1:16,2:16,3:16,4:16>>],
    cs_end().

extended_bit_aligned(Config) when is_list(Config) ->
    cs_init(),
    <<$a:7,$b:7,$c:7,$d:7,$e:7,$f:7,$g:7>> =
	cs(<< <<(X+32):7>> || X <- "ABCDEFG" >>),
    "ABCDEFG" = [(X-32) || <<X:7>> <= <<$a:7,$b:7,$c:7,$d:7,$e:7,$f:7,$g:7>>],
    <<1:31/little,2:31/little,3:31/little,4:31/little>> =
	cs(<< <<X:31/little>> || X <- [1,2,3,4] >>),
    [256,512,768,1024] =
	[X || <<X:15/little>> <= <<1:15,2:15,3:15,4:15>>],
    cs_end().

mixed(Config) when is_list(Config) ->
    cs_init(),
    <<2,3,3,4,4,5,5,6>> =
	cs(<< <<(X+Y)>> || <<X>> <= <<1,2,3,4>>, <<Y>> <= <<1,2>> >>),
    <<2,3,3,4,4,5,5,6>> =
	cs(<< <<(X+Y)>> || <<X>> <= <<1,2,3,4>>, Y <- [1,2] >>),
    <<2,3,3,4,4,5,5,6>> =
	cs(<< <<(X+Y)>> || X <- [1,2,3,4], Y <- [1,2] >>),
    One = id([1,2,3,4]),
    Two = id([1,2]),
    <<2,3,3,4,4,5,5,6>> =
	cs(<< <<(X+Y)>> || X <- One, Y <- Two >>),
    [2,3,3,4,4,5,5,6] =
	[(X+Y) || <<X>> <= <<1,2,3,4>>, <<Y>> <= <<1,2>>],
    [2,3,3,4,4,5,5,6] =
	[(X+Y) || <<X>> <= <<1,2,3,4>>, Y <- [1,2]],
    <<2:3,3:3,3:3,4:3,4:3,5:3,5:3,6:3>> =
	cs(<< <<(X+Y):3>> || <<X:3>> <= <<1:3,2:3,3:3,4:3>>,
                              <<Y:3>> <= <<1:3,2:3>> >>),
    <<2:3,3:3,3:3,4:3,4:3,5:3,5:3,6:3>> =
	cs(<< <<(X+Y):3>> || <<X:3>> <= <<1:3,2:3,3:3,4:3>>, Y <- [1,2] >>),
    <<2:3,3:3,3:3,4:3,4:3,5:3,5:3,6:3>> =
	cs(<< <<(X+Y):3>> || X <- [1,2,3,4], Y <- [1,2] >>),
    <<2:3,3:3,3:3,4:3,4:3,5:3,5:3,6:3>> =
	cs_default(<< <<(X+Y):3>> || {X,Y} <- [{1,1},{1,2},{2,1},{2,2},
                                               {3,1},{3,2},{4,1},{4,2}] >>),
    [2,3,3,4,4,5,5,6] =
	[(X+Y) || <<X:3>> <= <<1:3,2:3,3:3,4:3>>, <<Y:3>> <= <<1:3,2:3>>],
    [2,3,3,4,4,5,5,6] =
	[(X+Y) || <<X:3>> <= <<1:3,2:3,3:3,4:3>>, {_,Y} <- [{a,1},{b,2}]],

    %% OTP-16899: Nested binary comprehensions would fail to load.
    <<0,1,0,2,0,3,99>> = mixed_nested([1,2,3]),

    <<1>> = cs_default(<< <<X>> || L <- [[1]], X <- L >>),

    %% The compiler would crash in v3_kernel.
    <<42:32,75:32,253:32,(42 bsl 8 bor 75):32>> =
        cs_default(mixed_size(id([8,16]), <<42,75,253>>)),

    silly_lc_bc(5),

    gen_data(0),
    gen_data(256),
    gen_data(512),

    <<1,2,3>> = cs_default(match_context_1(<<1,2,3>>)),
    <<4,5,6>> = cs_default(match_context_2(<<4,5,6>>)),

    <<255>> = over_complex_generator(),
    {'EXIT',_} = catch float_segment_size(),

    <<>> = inconsistent_types_1([]),
    {'EXIT',{{bad_generator,42},_}} = catch inconsistent_types_1(42),
    Self = self(),
    {'EXIT',{{bad_generator,Self},_}} = catch inconsistent_types_1(Self),

    {'EXIT',{{bad_filter,<<>>},_}} = catch inconsistent_types_2(),

    %% Cover some code in beam_ssa_pre_codegen.
    [] = fun(A) ->
                 [] = [ok || <<A:A, _:(A bsr 1)>> <= A]
         end(id(<<>>)),

    cs_end().


mixed_nested(L) ->
    << << << << E:16 >> || E <- L >> || true >>/binary, 99:(id(8))>>.

mixed_size(List, Bin) ->
    << <<X:32>> || Size <- List, <<X:Size>> <= Bin >>.

silly_lc_bc(N) when N > 0 ->
    Bin = iolist_to_binary(silly_lc(N)),
    Bin = cs(silly_bc(N)),
    Size = byte_size(Bin),
    Size = 5 bsl N,
    silly_lc_bc(N - 1);
silly_lc_bc(_) -> ok.

silly_bc(0) ->
    <<0, 1, 2, 3, 4>>;
silly_bc(N) ->
    << <<X, X>> || <<X>> <= silly_bc(N - 1) >>.

silly_lc(0) ->
    [0, 1, 2, 3, 4];
silly_lc(N) ->
    [[X, X] || X <- silly_lc(N - 1)].

gen_data(Size) ->
    Data = cs(<< <<C>> || C <- lists:seq(0, Size-1) >>),
    Data = << <<C>> || _ <- lists:seq(1, Size div 256),
                       C <- lists:seq(0, 255) >>.

match_context_1(<<B/binary>>) ->
    << <<V>> || <<V>> <= B >>.

match_context_2(<<B/binary>>) ->
    do_match_context_2(B).

do_match_context_2(B) ->
    << <<V>> || <<V>> <= B >>.

%% Would crash beam_ssa_bc_size when the no_copt option was given.
over_complex_generator() ->
    <<
      <<255>> ||
        <<0:2>> <= <<0:2>>,
        <<_:8>> <=
            case true of
                true ->
                    <<8>>;
                [6.6 | bad_tail] ->
                    ok;
                [3 | 4] ->
                    error
            end
    >>.

float_segment_size() ->
    try
        V = 0.79
    of
        _ ->
            %% Would crash beam_ssa_bc_size when trying to
            %% interpret V * U = 0.79 * 8 as a size.
            <<
              0 || <<5.9:V/unit:8-float>> <= 42
            >>
    catch
        _:_ ->
            error
    end.

%% GH-6468. Would crash in beam_ssa_bc_size:update_successors/3.
inconsistent_types_1(X) ->
    <<
      X || _ <- X,
           case is_pid(X) of
               Y ->
                   (#{
                        ((not Y) andalso
                         <<Y:(Y andalso X)>>) := Y
                     } = X);
               _ ->
                   false
           end
    >>.

%% GH-6468. Same type of crash.
inconsistent_types_2() ->
    <<
      0 || case id([]) of
               Y ->
                   <<
                     Y ||
                       _ <- Y,
                       (not ((false = Y) = (Y /= []))), (_ = Y)
                   >>
           end
    >>.

filters(Config) when is_list(Config) ->
    cs_init(),
    <<"BDF">> =
	cs_default(<< <<(X-32)>> ||
		       <<X:7>> <= <<$a:7,$b:7,$c:7,$d:7,$e:7,$f:7,$g:7>>,
		       X rem 2 == 0>>),
    <<"abc">> = cs_default(<< <<(X+32)>> ||
			       X <- "ABCDEFG",
			       is_less_than(X, $D)>>),
    <<"efg">> = cs_default(<< <<(X+32)>> ||
			       X <- "ABCDEFG",
			       not is_less_than(X, $E)>>),
    <<"b">> = cs_default(<< <<(X+32)>> ||
			     X <- "ABCDEFG",
			     is_less_than(X, $D),
			     X rem 2 == 0>>),
    <<"eg">> = cs_default(<< <<(X+32)>> ||
			      X <- "ABCDEFG",
			      not is_less_than(X, $E),
			      X rem 2 == 1>>),
    <<1,3>> = cs_default(<< <<(length(L))>> ||
                             L <- [[],[a],[],[x,y,z]],
                             case L of
                                 [] -> false;
                                 [_|_] -> true
                             end >>),
    <<77,42>> = cs_default(<< <<B>> || <<B>> <- [<<77>>,<<1,2>>,<<42>>] >>),

    %% Filtering by a non-matching pattern.
    <<"abd">> = cs_default(<< <<X:8>> ||
			       <<0:1,X:7>> <= <<$a:8,$b:8,1:1,$c:7,$d:8,
						1:1,$e:7,0:4>> >>),

    <<42,42>> = cs_default(<< <<42:8>> || 42 <- [1,2,3,42,43,42] >>),
    cs_end().

is_less_than(X, C) when X < C -> true;
is_less_than(_, _) -> false.

trim_coverage(Config) when is_list(Config) ->
    <<0,0,0,2,0,0,5,48,0,11,219,174,0,0,0,0>> = coverage_materialiv(a, b, {1328,777134}),
    <<67,40,0,0,66,152,0,0,69,66,64,0>> = coverage_trimmer([42,19,777]),
    <<0,0,2,43,0,0,3,9,0,0,0,3,64,8,0,0,0,0,0,0,
	   64,68,0,0,0,0,0,0,192,171,198,0,0,0,0,0>> = 
	coverage_lightfv(555, 777, {3.0,40.0,-3555.0}),
    <<"abcabc">> = coverage_strange(0, <<"abc">>),
    ok.

coverage_materialiv(A, B, Params) ->
    A = id(A),
    B = id(B),
    <<(tuple_size(Params)):32,
     (<< <<C:32>> || C <- tuple_to_list(Params)>>)/binary,
     0:(((1+tuple_size(Params)) rem 2)*32)>>.

coverage_lightfv(Light, Pname, Params) ->
    id(<<Light:32,Pname:32,(size(Params)):32,
	(<< <<C:64/float>> || C <- tuple_to_list(Params)>>)/binary,
	0:(((1+size(Params)) rem 2)*32)>>).

coverage_trimmer(Params) ->
    X = id(0),
    Y = id(1),
    id({X,Y}),
    << <<(begin {A,B,D} = id({C,C,C}), id(0),
		coverage_summer(A, B, C, D) end):32/float>> ||
	C <- Params >>.

coverage_summer(A, B, C, D) -> A+B+C+D.

coverage_strange(V, Bin) ->
    <<
      << <<X>> || <<X/utf8>> <= Bin >>/bits,
      << <<Y>> || <<Y>> <= Bin>>:
      case V of
          V ->
              3;
          0 ->
              receive
                  whatever ->
                      1
              end
      end/bytes>>.

nomatch(Config) when is_list(Config) ->
    Bin = id(<<1,2,3,4,5>>),
    <<>> = << <<X:8>> || X = {_,_} = [_|_] <- [1,2,3] >>,
    [] = [X || <<X:all/binary>> <= Bin],
    [] = [X || <<X:bad/binary>> <= Bin],
    <<>> = << <<X:32>> || <<X:all/binary>> <= Bin >>,
    <<>> = << <<X:32>> || <<X:bad/binary>> <= Bin >>,

    <<>> = << <<"a">> || <<_:1/float>> <= Bin>>,

    NaN = <<(-1):32>>,
    <<>> = << <<"a">> || <<_:32/float>> <= NaN >>,

    <<1:32,2:32,3:32>> = nomatch_1(<<1,2,3>>, 8),
    <<>> = nomatch_1(<<1,2,3>>, bad),

    <<>> = << <<>> || <<_:8>> <= <<>> >>,

    %% GH-7494. Qualifiers should be evaluated from left to right. The
    %% second (failing) generator should never be evaluated because the
    %% first generator is an empty list.
    <<>> = id(<< <<C:8>> || C <- [], _ <- ok >>),
    <<>> = id(<<0 || _ <- [], _ <- ok, false>>),

    {'EXIT',{{bad_generator,false},_}} = catch << [] || <<0:0>> <= false >>,

    ok.

nomatch_1(Bin, Size) ->
    << <<X:32>> || <<X:Size>> <= Bin >>.

sizes(Config) when is_list(Config) ->
    cs_init(),
    Fun0 = fun(List) ->
		   cs(<< <<E:8>> || E <- List >>)
	   end,
    <<>> = Fun0([]),
    <<1>> = Fun0([1]),
    <<1,2>> = Fun0([1,2]),
    <<1,2,3>> = Fun0([1,2,3]),

    Fun1 = fun(List) ->
		   cs(<< <<E:16>> || E <- List >>)
	   end,
    <<>> = Fun1([]),
    <<1:16>> = Fun1([1]),
    <<1:16,2:16>> = Fun1([1,2]),
    <<1:16,2:16,3:16>> = Fun1([1,2,3]),

    Fun2 = fun(List) ->
		   cs(<< <<E:4>> || E <- List >>)
	   end,
    <<>> = Fun2([]),
    <<1:4>> = Fun2([1]),
    <<1:4,13:4>> = Fun2([1,13]),
    <<1:4,13:4,7:4>> = Fun2([1,13,7]),
    <<0:1000/unit:8>> = Fun2(lists:duplicate(2000, 0)),

    Fun3 = fun(List) ->
		   cs(<< <<E:3>> || E <- List >>)
	   end,
    <<>> = Fun3([]),
    <<40,177,29:5>> = Fun3([1,2,1,3,0,7,5]),
    <<0:512/unit:3>> = Fun3(lists:duplicate(512, 0)),

    Fun4 = fun(List, Size) ->
		   cs(<< <<E:Size>> || E <- List >>)
	   end,
    <<>> = Fun4([], 8),
    <<42:6>> = Fun4([42], 6),
    <<42:16>> = Fun4([42], 16),

    Fun5 = fun(List, Sz1, Sz2, Sz3) ->
		   cs(<< <<E:Sz1,(E+1):Sz2/unit:8,(E+2):Sz3/unit:8>> || E <- List >>)
	   end,
    <<>> = Fun5([], 1, 1, 1),
    <<7:3,8:40,9:56>> = Fun5([7], 3, 5, 7),

    Fun5a = fun(List, Sz1, Sz2, Sz3) ->
                    cs(<< <<"abc",E:Sz1,(E+1):Sz2/unit:8,"qqq",(E+2):Sz3/unit:8,"xyz">> ||
                          E <- List >>)
	   end,
    <<>> = Fun5a([], 1, 1, 1),
    <<"abc",7:3,8:40,"qqq",9:56,"xyz">> = Fun5a([7], 3, 5, 7),

    Fun6 = fun(List, Size) ->
		   cs(<< <<E:8,(E+1):Size>> || E <- List >>)
	   end,
    <<>> = Fun6([], 42),
    <<42,43:20>> = Fun6([42], 20),

    Fun7 = fun(B) ->
                   cs_default(<< <<C/utf8>> || C <- B >>)
            end,
    <<"Foundation"/utf8>> = Fun7("Foundation"),
    <<"Основание"/utf8>> = Fun7("Основание"),

    %% Binary generators.

    Fun10 = fun(Bin) ->
		    cs(<< <<E:16>> || <<E:8>> <= id(Bin) >>)
            end,
    <<>> = Fun10(<<>>),
    <<1:16>> = Fun10(<<1>>),
    <<1:16,2:16>> = Fun10(<<1,2>>),

    Fun11 = fun(Bin) ->
		    cs(<< <<E:8>> || <<E:16>> <= id(Bin) >>)
            end,
    <<>> = Fun11(<<>>),
    <<1>> = Fun11(<<1:16>>),
    <<1,2>> = Fun11(<<1:16,2:16>>),
    <<1,2>> = Fun11(<<1:16,2:16,0:1>>),
    <<1,2>> = Fun11(<<1:16,2:16,0:7>>),
    <<1,2>> = Fun11(<<1:16,2:16,42:8>>),
    <<1,2>> = Fun11(<<1:16,2:16,42:9>>),
    <<1,2>> = Fun11(<<1:16,2:16,255:15>>),

    Fun12 = fun(Bin, Sz1, Sz2) ->
		    cs(<< <<E:Sz1>> || <<E:Sz2>> <= id(Bin) >>)
	    end,
    <<>> = Fun12(<<>>, 1, 1),
    Binary = list_to_binary(lists:seq(0, 255)),
    Binary = Fun12(Binary, 1, 1),
    Binary = Fun12(Binary, 4, 4),
    Binary = Fun12(Binary, 8, 8),
    <<17:9,19:9>> = Fun12(<<17:6,19:6>>, 9, 6),

    Fun13 = fun(Sz) ->
		    cs(<< <<C:8>> || <<C:4>> <= <<1:4,2:4,3:4,0:Sz>> >>)
   	    end,
    <<1,2,3>> = Fun13(0),
    <<1,2,3,0>> = Fun13(4),
    <<1,2,3,0>> = Fun13(5),
    <<1,2,3,0>> = Fun13(6),
    <<1,2,3,0>> = Fun13(7),
    <<1,2,3,0,0>> = Fun13(8),

    <<0:3>> = cs_default(<< <<0:S>> || S <- [0,1,2] >>),
    <<0:3>> = cs_default(<< <<0:S>> || <<S>> <= <<0,1,2>> >>),

    Fun14 = fun(L, B) ->
                    cs_default(<< <<X:32>> || Size <- L, <<X:Size>> <= B >>)
            end,
    <<$a:32,$b:32,$c:32,($a bsl 8 bor $b):32>> = Fun14([8,16], <<"abc">>),
    <<$a:32,$b:32,$c:32>> = Fun14([8,bad], <<"abc">>),

    Fun15 = fun(B) ->
                    cs_default(<< <<C/utf8>> || << C:32 >> <= id(B) >>)
            end,
    <<"Foundation"/utf8>> = Fun15(<<"Foundation"/utf32>>),
    <<"Основание"/utf8>> = Fun15(<<"Основание"/utf32>>),

    {'EXIT',_} = (catch << <<C:4>> || <<C:8>> <= {1,2,3} >>),

    cs_end(),
    ok.

-define(BAD(E),   {'EXIT',{badarg,_}} = (catch << (E) || _ <- [1,2,3] >>)).
-define(BAD_V(E), {'EXIT',{badarg,_}} = (catch << (E) || I <- [1,2,3] >>)).

general_expressions(_) ->
    cs_init(),

    <<1,2,3>> = cs(<< begin <<1,2,3>> end || _ <- [1] >>),
    <<"abc">> = cs(<< begin <<"abc">> end || _ <- [1] >>),
    <<1,2,3>> = cs_default(<< begin
                                  I = <<(I0+1)>>,
                                  id(I)
                              end || <<I0>> <= <<0,1,2>> >>),
    <<1,2,3>> = cs_default(<< I || I <- [<<1,2>>,<<3>>] >>),
    <<1,2,3>> = cs_default(<< (id(<<I>>)) || I <- [1,2,3] >>),
    <<2,4>> = cs_default(<< case I rem 2 of
                                0 -> <<I>>;
                                1 -> <<>>
                            end || I <- [1,2,3,4,5] >>),
    <<2,3,4,5,6,7>> = cs_default(<< << (id(<<J>>)) || J <- [2*I,2*I+1] >> ||
                                     I <- [1,2,3] >>),
    <<1,2,2,3,4,4>> = cs_default(<< if
                                        I rem 2 =:= 0 -> <<I,I>>;
                                        true -> <<I>>
                                    end || I <- [1,2,3,4] >>),
    self() ! <<42>>,
    <<42>> = cs_default(<< receive B -> B end || _ <- [1] >>),
    <<10,5,3>> = cs_default(<< try
                                   <<(10 div I)>>
                               catch _:_ ->
                                       <<>>
                               end || I <- [0,1,2,3] >>),

    <<3:4,16#A:4,7:4>> = cs(hstring_to_bitstring("3A7")),
    <<0:3,1:3,2:3,3:3,4:3,5:3,6:3,7:3>> = cs(encode_chars_compact_map("ABCDEFGH", id(3), id({$A,8}))),

    cs_end(),

    %% Failing expressions.
    ?BAD(bad_atom),
    ?BAD(42),
    ?BAD(42.0),
    ?BAD_V({ok,I}),
    ?BAD_V([I]),
    ?BAD_V(fun() -> I end),

    ok.

hstring_to_bitstring(L) ->
    << <<(hex_to_int(D)):4>> || D <- L >>.

hex_to_int(D) when $0 =< D, D =< $9 -> D - $0;
hex_to_int(D) when $A =< D, D =< $F -> D - ($A - 10).

encode_chars_compact_map(Val, NumBits, {Lb,Limit}) ->
    << <<(enc_char_cm(C, Lb, Limit)):NumBits>> || C <- Val >>.

enc_char_cm(C0, Lb, Limit) ->
    C = C0 - Lb,
    if
	0 =< C, C < Limit ->
	    C;
	true ->
            error(illegal)
    end.

-undef(BAD).

no_generator(_Config) ->
    [<<"abc">>] = [<<(id(<<"abc">>)) || true >>],
    {<<>>} = {<<(id(<<"abc">>)) || false >>},

    %% Would crash the compiler when compiled with +no_type_opt.
    {'EXIT',{badarg,_}} = (catch << (catch "\001") || true >>),

    ok.

zero_pattern(Config) ->
    case is_atom(Config) of
        true ->
            %% Patterns that match zero bits loops forever, so we must
            %% be careful not to execute them.
            _ = << <<>> || <<>> <= <<>> >>,
            _ = << <<42>> || <<>> <= <<42>> >>,
            _ = <<
                  <<>> ||
                    <<>> <= << >>,
                    <<3:back>> <= << >>
                >>,
            _ = <<
                  <<>> ||
                    <<>> <= <<>>,
                    <<>> <- << <<>> || area >>
                >>;
        false ->
            ok
    end.

multiple_segments(_Config) ->
    cs_init(),

    [1,2] = matched_out_size(<<4, 1:4, 4, 2:4>>),
    [42] = matched_out_size(<<16, 42:16, 72>>),

    [] = do_multiple_segments_1(<<>>),
    [] = do_multiple_segments_1(<<1>>),
    [] = do_multiple_segments_1(<<1,2>>),
    [] = do_multiple_segments_1(<<1,2,3>>),
    [1,4] = do_multiple_segments_1(<<99,0,1,1,2,3,4,4>>),

    [] = do_multiple_segments_2(<<1,2>>),
    [6] = do_multiple_segments_2(<<1,2,3>>),
    [6,15] = do_multiple_segments_2(<<1,2,3,4,5,6,7,8>>),

    cs_end(),
    ok.

matched_out_size(Gen) ->
    Bin = cs_default(<< <<X>> || <<S,X:S>> <= Gen >>),
    List = [X || <<S,X:S>> <= Gen],
    Bin = list_to_binary(List),
    List.

do_multiple_segments_1(Gen) ->
    Bin = cs_default(<< <<V>> || <<V,V>> <= Gen >>),
    List = [V || <<V,V>> <= Gen],
    Bin = list_to_binary(List),
    List.

do_multiple_segments_2(Gen) ->
    Bin = cs(<< <<(A+B+C)>> || <<A,B,C>> <= Gen >>),
    List = [A+B+C || <<A,B,C>> <= Gen],
    Bin = list_to_binary(List),
    List.

grab_bag(_Config) ->
    {'EXIT',{function_clause,_}} = catch grab_bag_gh_6553(<<>>),
    {'EXIT',{function_clause,_}} = catch grab_bag_gh_6553(a),
    {'EXIT',{{badmatch,<<>>},_}} = catch grab_bag_gh_6553(<<42>>),

    %% Cover a line v3_kernel:get_line/1.
    _ = catch << ok || <<>> <= ok, ok >>,

    ok.

grab_bag_gh_6553(<<X>>) ->
    %% Would crash in beam_ssa_pre_codegen.
    <<X, ((<<_:0>> = <<_>>) = <<>>)>>.

cs_init() ->
    erts_debug:set_internal_state(available_internal_state, true),
    ok.

cs_end() ->
    erts_debug:set_internal_state(available_internal_state, false),
    ok.

%% Verify that the allocated size is exact (rounded up to the nearest byte).
cs(Bin) ->
    case ?MODULE of
        bs_bincomp_cover_SUITE ->
            ok;
        bs_bincomp_no_opt_SUITE ->
            ok;
        bs_bincomp_no_ssa_opt_SUITE ->
            ok;
        bs_bincomp_no_copt_SUITE ->
            ok;
        bs_bincomp_no_copt_ssa_SUITE ->
            ok;
        bs_bincomp_post_opt_SUITE ->
            ok;
        bs_bincomp_r24_SUITE ->
            ok;
        bs_bincomp_r25_SUITE ->
            ok;
        bs_bincomp_r26_SUITE ->
            ok;
        _ ->
            ByteSize = byte_size(Bin),
            {refc_binary,ByteSize,{binary,ByteSize},_} =
                erts_debug:get_internal_state({binary_info,Bin})
    end,
    Bin.

%% Verify that the allocated size of the binary is the default size.
cs_default(Bin) ->
    ByteSize = byte_size(Bin),
    {refc_binary,ByteSize,{binary,256},_} = 
	erts_debug:get_internal_state({binary_info,Bin}),
    Bin.

id(I) -> I.
