%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2006-2013. All Rights Reserved.
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
%% Originally based on Per Gustafsson's test suite.
%%

-module(bs_bincomp_SUITE).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 byte_aligned/1,bit_aligned/1,extended_byte_aligned/1,
	 extended_bit_aligned/1,mixed/1,filters/1,trim_coverage/1,
	 nomatch/1,sizes/1]).

-include_lib("test_server/include/test_server.hrl").

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    test_lib:recompile(?MODULE),
    [byte_aligned, bit_aligned, extended_byte_aligned,
     extended_bit_aligned, mixed, filters, trim_coverage,
     nomatch, sizes].

groups() -> 
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
	Config.

end_per_group(_GroupName, Config) ->
	Config.

byte_aligned(Config) when is_list(Config) ->
    cs_init(),
    ?line <<"abcdefg">> = cs(<< <<(X+32)>> || <<X>> <= <<"ABCDEFG">> >>),
    <<1:32/little,2:32/little,3:32/little,4:32/little>> =
	cs(<< <<X:32/little>> || <<X:32>> <= <<1:32,2:32,3:32,4:32>> >>),
    ?line cs(<<1:32/little,2:32/little,3:32/little,4:32/little>> =
	     << <<X:32/little>> || <<X:16>> <= <<1:16,2:16,3:16,4:16>> >>),
    cs_end().

bit_aligned(Config) when is_list(Config) ->
    cs_init(),
    ?line <<$a:7,$b:7,$c:7,$d:7,$e:7,$f:7,$g:7>> = 
	cs(<< <<(X+32):7>> || <<X>> <= <<"ABCDEFG">> >>),
    ?line <<"ABCDEFG">> = 
	cs(<< <<(X-32)>> || <<X:7>> <= <<$a:7,$b:7,$c:7,$d:7,$e:7,$f:7,$g:7>> >>),
    ?line <<1:31/little,2:31/little,3:31/little,4:31/little>> =
	cs(<< <<X:31/little>> || <<X:31>> <= <<1:31,2:31,3:31,4:31>> >>),
    ?line <<1:31/little,2:31/little,3:31/little,4:31/little>> =
	cs(<< <<X:31/little>> || <<X:15>> <= <<1:15,2:15,3:15,4:15>> >>),
    cs_end().

extended_byte_aligned(Config) when is_list(Config) ->
    cs_init(),
    ?line <<"abcdefg">> = cs(<< <<(X+32)>> || X <- "ABCDEFG" >>),
    ?line "abcdefg" = [(X+32) || <<X>> <= <<"ABCDEFG">>],
    ?line <<1:32/little,2:32/little,3:32/little,4:32/little>> =
	cs(<< <<X:32/little>> || X <- [1,2,3,4] >>),
    ?line [256,512,768,1024] =
	[X || <<X:16/little>> <= <<1:16,2:16,3:16,4:16>>],
    cs_end().

extended_bit_aligned(Config) when is_list(Config) ->
    cs_init(),
    ?line <<$a:7,$b:7,$c:7,$d:7,$e:7,$f:7,$g:7>> = 
	cs(<< <<(X+32):7>> || X <- "ABCDEFG" >>),
    ?line "ABCDEFG" = [(X-32) || <<X:7>> <= <<$a:7,$b:7,$c:7,$d:7,$e:7,$f:7,$g:7>>],
    ?line <<1:31/little,2:31/little,3:31/little,4:31/little>> =
	cs(<< <<X:31/little>> || X <- [1,2,3,4] >>),
    ?line [256,512,768,1024] =
	[X || <<X:15/little>> <= <<1:15,2:15,3:15,4:15>>],
    cs_end().

mixed(Config) when is_list(Config) ->
    cs_init(),
    ?line <<2,3,3,4,4,5,5,6>> =  
	cs(<< <<(X+Y)>> || <<X>> <= <<1,2,3,4>>, <<Y>> <= <<1,2>> >>),
    ?line <<2,3,3,4,4,5,5,6>> =  
	<< <<(X+Y)>> || <<X>> <= <<1,2,3,4>>, Y <- [1,2] >>,
    ?line <<2,3,3,4,4,5,5,6>> =  
	cs(<< <<(X+Y)>> || X <- [1,2,3,4], Y <- [1,2] >>),
    One = id([1,2,3,4]),
    Two = id([1,2]),
    ?line <<2,3,3,4,4,5,5,6>> =  
	cs(<< <<(X+Y)>> || X <- One, Y <- Two >>),
    ?line [2,3,3,4,4,5,5,6] =  
	[(X+Y) || <<X>> <= <<1,2,3,4>>, <<Y>> <= <<1,2>>],
    ?line [2,3,3,4,4,5,5,6] =  
	[(X+Y) || <<X>> <= <<1,2,3,4>>, Y <- [1,2]],
    ?line <<2:3,3:3,3:3,4:3,4:3,5:3,5:3,6:3>> =  
	cs(<< <<(X+Y):3>> || <<X:3>> <= <<1:3,2:3,3:3,4:3>>,
			     <<Y:3>> <= <<1:3,2:3>> >>),
    ?line <<2:3,3:3,3:3,4:3,4:3,5:3,5:3,6:3>> =  
	cs(<< <<(X+Y):3>> || <<X:3>> <= <<1:3,2:3,3:3,4:3>>, Y <- [1,2] >>),
    ?line <<2:3,3:3,3:3,4:3,4:3,5:3,5:3,6:3>> =  
	cs(<< <<(X+Y):3>> || X <- [1,2,3,4], Y <- [1,2] >>),
    ?line <<2:3,3:3,3:3,4:3,4:3,5:3,5:3,6:3>> =  
	cs_default(<< <<(X+Y):3>> || {X,Y} <- [{1,1},{1,2},{2,1},{2,2},
					       {3,1},{3,2},{4,1},{4,2}] >>),
    ?line [2,3,3,4,4,5,5,6] =  
	[(X+Y) || <<X:3>> <= <<1:3,2:3,3:3,4:3>>, <<Y:3>> <= <<1:3,2:3>>],
    ?line [2,3,3,4,4,5,5,6] =  
	[(X+Y) || <<X:3>> <= <<1:3,2:3,3:3,4:3>>, {_,Y} <- [{a,1},{b,2}]],
    cs_end().

filters(Config) when is_list(Config) ->
    cs_init(),
    ?line <<"BDF">> = 
	cs_default(<< <<(X-32)>> || <<X:7>> <= <<$a:7,$b:7,$c:7,$d:7,$e:7,$f:7,$g:7>>,
				    X rem 2 == 0>>),
    ?line <<"abc">> = cs_default(<< <<(X+32)>> || X <- "ABCDEFG",
						  is_less_than(X, $D)>>),
    ?line <<"efg">> = cs_default(<< <<(X+32)>> || X <- "ABCDEFG",
						  not is_less_than(X, $E)>>),
    ?line <<"b">> = cs_default(<< <<(X+32)>> || X <- "ABCDEFG",
						is_less_than(X, $D),
						X rem 2 == 0>>),
    ?line <<"eg">> = cs_default(<< <<(X+32)>> || X <- "ABCDEFG",
						 not is_less_than(X, $E),
						 X rem 2 == 1>>),

    %% Filtering by a non-matching pattern.
    ?line <<"abd">> = cs_default(<< <<X:8>> ||
				     <<0:1,X:7>> <= <<$a:8,$b:8,1:1,$c:7,$d:8,
						     1:1,$e:7,0:4>> >>),

    ?line <<42,42>> = cs_default(<< <<42:8>> ||
				     42 <- [1,2,3,42,43,42] >>),
    cs_end().

is_less_than(X, C) when X < C -> true;
is_less_than(_, _) -> false.

trim_coverage(Config) when is_list(Config) ->
    ?line <<0,0,0,2,0,0,5,48,0,11,219,174,0,0,0,0>> = coverage_materialiv(a, b, {1328,777134}),
    ?line <<67,40,0,0,66,152,0,0,69,66,64,0>> = coverage_trimmer([42,19,777]),
    ?line <<0,0,2,43,0,0,3,9,0,0,0,3,64,8,0,0,0,0,0,0,
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
    ?line <<>> = << <<X:8>> || X = {_,_} = [_|_] <- [1,2,3] >>,
    ok.

sizes(Config) when is_list(Config) ->
    ?line cs_init(),
    Fun0 = fun(List) ->
		   cs(<< <<E:8>> || E <- List >>)
	   end,
    ?line <<>> = Fun0([]),
    ?line <<1>> = Fun0([1]),
    ?line <<1,2>> = Fun0([1,2]),
    ?line <<1,2,3>> = Fun0([1,2,3]),

    Fun1 = fun(List) ->
		   cs(<< <<E:16>> || E <- List >>)
	   end,
    ?line <<>> = Fun1([]),
    ?line <<1:16>> = Fun1([1]),
    ?line <<1:16,2:16>> = Fun1([1,2]),
    ?line <<1:16,2:16,3:16>> = Fun1([1,2,3]),

    Fun2 = fun(List) ->
		   cs(<< <<E:4>> || E <- List >>)
	   end,
    ?line <<>> = Fun2([]),
    ?line <<1:4>> = Fun2([1]),
    ?line <<1:4,13:4>> = Fun2([1,13]),
    ?line <<1:4,13:4,7:4>> = Fun2([1,13,7]),
    ?line <<0:1000/unit:8>> = Fun2(lists:duplicate(2000, 0)),

    Fun3 = fun(List) ->
		   cs(<< <<E:3>> || E <- List >>)
	   end,
    ?line <<>> = Fun3([]),
    ?line <<40,177,29:5>> = Fun3([1,2,1,3,0,7,5]),
    ?line <<0:512/unit:3>> = Fun3(lists:duplicate(512, 0)),

    Fun4 = fun(List, Size) ->
		   cs(<< <<E:Size>> || E <- List >>)
	   end,
    ?line <<>> = Fun4([], 8),
    ?line <<42:6>> = Fun4([42], 6),
    ?line <<42:16>> = Fun4([42], 16),

    Fun5 = fun(List, Sz1, Sz2, Sz3) ->
		   cs(<< <<E:Sz1,(E+1):Sz2/unit:8,(E+2):Sz3/unit:8>> || E <- List >>)
	   end,
    ?line <<>> = Fun5([], 1, 1, 1),
    ?line <<7:3,8:40,9:56>> = Fun5([7], 3, 5, 7),

    Fun6 = fun(List, Size) ->
		   cs(<< <<E:8,(E+1):Size>> || E <- List >>)
	   end,
    ?line <<>> = Fun6([], 42),
    ?line <<42,43:20>> = Fun6([42], 20),

    %% Binary generators.

    Fun10 = fun(Bin) ->
		    cs(<< <<E:16>> || <<E:8>> <= Bin >>)
            end,
    ?line <<>> = Fun10(<<>>),
    ?line <<1:16>> = Fun10(<<1>>),
    ?line <<1:16,2:16>> = Fun10(<<1,2>>),

    Fun11 = fun(Bin) ->
		    cs(<< <<E:8>> || <<E:16>> <= Bin >>)
            end,
    ?line <<>> = Fun11(<<>>),
    ?line <<1>> = Fun11(<<1:16>>),
    ?line <<1,2>> = Fun11(<<1:16,2:16>>),
    ?line <<1,2>> = Fun11(<<1:16,2:16,0:1>>),
    ?line <<1,2>> = Fun11(<<1:16,2:16,0:7>>),
    ?line <<1,2>> = Fun11(<<1:16,2:16,42:8>>),
    ?line <<1,2>> = Fun11(<<1:16,2:16,42:9>>),
    ?line <<1,2>> = Fun11(<<1:16,2:16,255:15>>),

    Fun12 = fun(Bin, Sz1, Sz2) ->
		    cs(<< <<E:Sz1>> || <<E:Sz2>> <= Bin >>)
	    end,
    ?line <<>> = Fun12(<<>>, 1, 1),
    ?line Binary = list_to_binary(lists:seq(0, 255)),
    ?line Binary = Fun12(Binary, 1, 1),
    ?line Binary = Fun12(Binary, 4, 4),
    ?line Binary = Fun12(Binary, 8, 8),
    ?line <<17:9,19:9>> = Fun12(<<17:6,19:6>>, 9, 6),

    Fun13 = fun(Sz) ->
		    cs_default(<< <<C:8>> || <<C:4>> <= <<1:4,2:4,3:4,0:Sz>> >>)
   	    end,
    ?line <<1,2,3>> = Fun13(0),
    ?line <<1,2,3,0>> = Fun13(4),
    ?line <<1,2,3,0>> = Fun13(5),
    ?line <<1,2,3,0>> = Fun13(6),
    ?line <<1,2,3,0>> = Fun13(7),
    ?line <<1,2,3,0,0>> = Fun13(8),

    <<0:3>> = cs_default(<< <<0:S>> || S <- [0,1,2] >>),
    <<0:3>> = cs_default(<< <<0:S>> || <<S>> <= <<0,1,2>> >>),

    ?line {'EXIT',_} = (catch << <<C:4>> || <<C:8>> <= {1,2,3} >>),

    ?line cs_end(),
    ok.

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
