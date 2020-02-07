%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2006-2018. All Rights Reserved.
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
	 byte_aligned/1,bit_aligned/1,extended_byte_aligned/1,
	 extended_bit_aligned/1,mixed/1,filters/1,trim_coverage/1,
	 nomatch/1,sizes/1,general_expressions/1,matched_out_size/1]).

-include_lib("common_test/include/ct.hrl").

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [byte_aligned, bit_aligned, extended_byte_aligned,
     extended_bit_aligned, mixed, filters, trim_coverage,
     nomatch, sizes, general_expressions, matched_out_size].

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
    <<"ABCDEFG">> =
	cs(<< <<(X-32)>> || <<X:7>> <= <<$a:7,$b:7,$c:7,$d:7,$e:7,$f:7,$g:7>> >>),
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
	<< <<(X+Y)>> || <<X>> <= <<1,2,3,4>>, Y <- [1,2] >>,
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
    cs_end().

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

nomatch(Config) when is_list(Config) ->
    Bin = id(<<1,2,3,4,5>>),
    <<>> = << <<X:8>> || X = {_,_} = [_|_] <- [1,2,3] >>,
    [] = [X || <<X:all/binary>> <= Bin],
    [] = [X || <<X:bad/binary>> <= Bin],
    <<>> = << <<X:32>> || <<X:all/binary>> <= Bin >>,
    <<>> = << <<X:32>> || <<X:bad/binary>> <= Bin >>,

    ok.

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

    Fun6 = fun(List, Size) ->
		   cs(<< <<E:8,(E+1):Size>> || E <- List >>)
	   end,
    <<>> = Fun6([], 42),
    <<42,43:20>> = Fun6([42], 20),

    %% Binary generators.

    Fun10 = fun(Bin) ->
		    cs(<< <<E:16>> || <<E:8>> <= Bin >>)
            end,
    <<>> = Fun10(<<>>),
    <<1:16>> = Fun10(<<1>>),
    <<1:16,2:16>> = Fun10(<<1,2>>),

    Fun11 = fun(Bin) ->
		    cs(<< <<E:8>> || <<E:16>> <= Bin >>)
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
		    cs(<< <<E:Sz1>> || <<E:Sz2>> <= Bin >>)
	    end,
    <<>> = Fun12(<<>>, 1, 1),
    Binary = list_to_binary(lists:seq(0, 255)),
    Binary = Fun12(Binary, 1, 1),
    Binary = Fun12(Binary, 4, 4),
    Binary = Fun12(Binary, 8, 8),
    <<17:9,19:9>> = Fun12(<<17:6,19:6>>, 9, 6),

    Fun13 = fun(Sz) ->
		    cs_default(<< <<C:8>> || <<C:4>> <= <<1:4,2:4,3:4,0:Sz>> >>)
   	    end,
    <<1,2,3>> = Fun13(0),
    <<1,2,3,0>> = Fun13(4),
    <<1,2,3,0>> = Fun13(5),
    <<1,2,3,0>> = Fun13(6),
    <<1,2,3,0>> = Fun13(7),
    <<1,2,3,0,0>> = Fun13(8),

    <<0:3>> = cs_default(<< <<0:S>> || S <- [0,1,2] >>),
    <<0:3>> = cs_default(<< <<0:S>> || <<S>> <= <<0,1,2>> >>),

    {'EXIT',_} = (catch << <<C:4>> || <<C:8>> <= {1,2,3} >>),

    cs_end(),
    ok.

-define(BAD(E),   {'EXIT',{badarg,_}} = (catch << (E) || _ <- [1,2,3] >>)).
-define(BAD_V(E), {'EXIT',{badarg,_}} = (catch << (E) || I <- [1,2,3] >>)).

general_expressions(_) ->
    <<1,2,3>> = << begin <<1,2,3>> end || _ <- [1] >>,
    <<"abc">> = << begin <<"abc">> end || _ <- [1] >>,
    <<1,2,3>> = << begin
		       I = <<(I0+1)>>,
		       id(I)
		   end || <<I0>> <= <<0,1,2>> >>,
    <<1,2,3>> = << I || I <- [<<1,2>>,<<3>>] >>,
    <<1,2,3>> = << (id(<<I>>)) || I <- [1,2,3] >>,
    <<2,4>> = << case I rem 2 of
		     0 -> <<I>>;
		     1 -> <<>>
		 end || I <- [1,2,3,4,5] >>,
    <<2,3,4,5,6,7>> = << << (id(<<J>>)) || J <- [2*I,2*I+1] >> ||
			  I <- [1,2,3] >>,
    <<1,2,2,3,4,4>> = << if
			     I rem 2 =:= 0 -> <<I,I>>;
			     true -> <<I>>
			 end || I <- [1,2,3,4] >>,
    self() ! <<42>>,
    <<42>> = << receive B -> B end || _ <- [1] >>,
    <<10,5,3>> = << try
			<<(10 div I)>>
		    catch _:_ ->
			    <<>>
		    end || I <- [0,1,2,3] >>,

    %% Failing expressions.
    ?BAD(bad_atom),
    ?BAD(42),
    ?BAD(42.0),
    ?BAD_V({ok,I}),
    ?BAD_V([I]),
    ?BAD_V(fun() -> I end),

    ok.

-undef(BAD).

matched_out_size(Config) when is_list(Config) ->
    <<1, 2>> = matched_out_size_1(<<4, 1:4, 4, 2:4>>),
    ok.

matched_out_size_1(Binary) ->
    << <<X>> || <<S, X:S>> <= Binary>>.

cs_init() ->
    erts_debug:set_internal_state(available_internal_state, true),
    ok.

cs_end() ->
    erts_debug:set_internal_state(available_internal_state, false),
    ok.

%% Verify that the allocated size is exact (rounded up to the nearest byte).
cs(Bin) ->
    ByteSize = byte_size(Bin),
    {refc_binary,ByteSize,{binary,ByteSize},_} = 
	erts_debug:get_internal_state({binary_info,Bin}),
    Bin.

%% Verify that the allocated size of the binary is the default size.
cs_default(Bin) ->
    ByteSize = byte_size(Bin),
    {refc_binary,ByteSize,{binary,256},_} = 
	erts_debug:get_internal_state({binary_info,Bin}),
    Bin.

id(I) -> I.
