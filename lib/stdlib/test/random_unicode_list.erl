%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2008-2017. All Rights Reserved.
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

%% 
%% Generate random iolists to be used by crypto_SUITE.erl
%%

-module(random_unicode_list).

-export([run/3, run/4, standard_seed/0, compare/4,
	 random_unicode_list/2]).

run(I,F1,F2) ->
    run(I,F1,F2,utf8).
run(Iter,Fun1,Fun2,Enc) ->
    standard_seed(),
    compare(Iter,Fun1,Fun2,Enc).

int_to_utf8(I) when I =< 16#7F ->
    <<I>>;
int_to_utf8(I) when I =< 16#7FF ->
    B2 = I,
    B1 = (I bsr 6),
    <<1:1,1:1,0:1,B1:5,1:1,0:1,B2:6>>;
int_to_utf8(I) when I =< 16#FFFF ->
    B3 = I,
    B2 = (I bsr 6),
    B1 = (I bsr 12),
    <<1:1,1:1,1:1,0:1,B1:4,1:1,0:1,B2:6,1:1,0:1,B3:6>>;
int_to_utf8(I) when I =< 16#3FFFFF ->
    B4 = I,
    B3 = (I bsr 6),
    B2 = (I bsr 12),
    B1 = (I bsr 18),
    <<1:1,1:1,1:1,1:1,0:1,B1:3,1:1,0:1,B2:6,1:1,0:1,B3:6,1:1,0:1,B4:6>>;
int_to_utf8(I) when I =< 16#3FFFFFF ->
    B5 = I,
    B4 = (I bsr 6),
    B3 = (I bsr 12),
    B2 = (I bsr 18),
    B1 = (I bsr 24),
    <<1:1,1:1,1:1,1:1,1:1,0:1,B1:2,1:1,0:1,B2:6,1:1,0:1,B3:6,1:1,0:1,B4:6,
     1:1,0:1,B5:6>>.

int_to_utf16_big(I) when I < 16#10000 ->
    <<I:16/big>>;
int_to_utf16_big(I) ->
    I2 = I - 16#10000,
    B1 = 16#D800 bor (I2 bsr 10),
    B2 = 16#DC00 bor (I2 band 16#3FF),
    <<B1:16/big,B2:16/big>>.
int_to_utf16_little(I) when I < 16#10000 ->
    <<I:16/little>>;
int_to_utf16_little(I) ->
    I2 = I - 16#10000,
    B1 = 16#D800 bor (I2 bsr 10),
    B2 = 16#DC00 bor (I2 band 16#3FF),
    <<B1:16/little,B2:16/little>>.
int_to_utf32_big(I) ->
    <<I:32/big>>.
int_to_utf32_little(I) ->
    <<I:32/little>>.

id(I) -> I.

random_char() ->
     case rand:uniform(16#10FFFF+1) - 1 of
	 X when X >= 16#D800,
	  X =< 16#DFFF ->
	     random_char();
	 Y ->
	     Y
     end.

random_list(0,Acc) ->
    Acc;
random_list(N,Acc) ->
    random_list(N-1,[random_char() | Acc]).

int_to(utf8,X) ->
    int_to_utf8(X);
int_to({utf16,big},X) ->
    int_to_utf16_big(X);
int_to({utf16,little},X) ->
    int_to_utf16_little(X);
int_to({utf32,big},X) ->
    int_to_utf32_big(X);
int_to({utf32,little},X) ->
    int_to_utf32_little(X).


random_binary(N,Enc) ->
    L = random_list(N,[]),
    B = iolist_to_binary(lists:map(fun(X) ->
					   int_to(Enc,X)
				   end,
				   L)),
    case {rand:uniform(3),size(B)} of
	{2,M} when M > 1 ->
	    B2 = id(<<1:3,B/binary,1:5>>),
	    <<_:3,C:M/binary,_:5>> = B2,
	    C;
	{3,M} when M > 1 ->
	    X = rand:uniform(M+1)-1,
	    <<B1:X/binary,B2/binary>> = B,
	    [B1,B2];
	_ ->
	    B
    end.
random_list(N) ->
    random_list(N,[]).

front() ->
    case rand:uniform(10) of
	10 ->
	    false;
	_ ->
	    true
    end.

any_type() ->
    case rand:uniform(10) of
	1 ->
	    list;
	2 ->
	    binary;
	3 ->
	    iolist;
	_ ->
	    char
    end.

tail_type() ->
    case rand:uniform(5) of
	1 ->
	    list;
	2 ->
	    iolist;
	_ ->
	    binary
    end.

random_length(N) ->
    UpperLimit = 255,
    case N of
	M when M > UpperLimit ->
	    rand:uniform(UpperLimit+1) - 1;
	_ ->
	    rand:uniform(N+1) - 1
    end.

random_unicode_list(0,Acc,_Enc) ->
    Acc;
random_unicode_list(N,Acc,Enc) ->
    case front() of
	true ->
	    case any_type() of
		list ->
		    X = random_length(N),
		    L = random_list(X),
		    random_unicode_list(N-X,[L|Acc],Enc);
		binary ->
		    X = random_length(N),
		    B = random_binary(X,Enc),
		    random_unicode_list(N-X,[B|Acc],Enc);
		iolist ->
		    X = random_length(N),
		    B = random_unicode_list(X,Enc),
		    random_unicode_list(N-X,[B|Acc],Enc);
		char ->
		    C = random_char(),
		    random_unicode_list(N-1,[C|Acc],Enc)
	    end;
	false ->
	    case tail_type() of
		list ->
		    X = random_length(N),
		    L = random_list(X),
		    random_unicode_list(N-X,[Acc|L],Enc);
		binary ->
		    X = random_length(N),
		    B = random_binary(X,Enc),
		    random_unicode_list(N-X,[Acc|B],Enc);
		iolist ->
		    X = random_length(N),
		    B = random_unicode_list(X,Enc),
		    random_unicode_list(N-X,[Acc|B],Enc)
	    end
    end.

random_unicode_list(N,Enc) ->
    random_unicode_list(N,[],Enc).
    

standard_seed() ->
    rand:seed(exsplus, {1201,855653,380975}).

do_comp(List,F1,F2) ->
    X = F1(List),
    Y = F2(List),
    case X =:= Y of
	false ->
	    exit({not_matching,List,X,Y});
	_ ->
	    true
    end.

compare(0,Fun1,Fun2,_Enc) ->
    do_comp(<<>>,Fun1,Fun2),
    do_comp([],Fun1,Fun2),
    do_comp([[]|<<>>],Fun1,Fun2),
    do_comp([<<>>,[]|<<>>],Fun1,Fun2),
    true;

compare(N,Fun1,Fun2,Enc) ->
    L = random_unicode_list(N,Enc),
    do_comp(L,Fun1,Fun2),
    compare(N-1,Fun1,Fun2,Enc).
