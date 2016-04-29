%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
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

-module(random_iolist).

-export([run/3, run2/3, standard_seed/0, compare/3, compare2/3, 
	 random_iolist/1]).

run(Iter,Fun1,Fun2) ->
    standard_seed(),
    compare(Iter,Fun1,Fun2).

run2(Iter,Fun1,Fun2) ->
    standard_seed(),
    compare2(Iter,Fun1,Fun2).

random_byte() ->
     rand:uniform(256) - 1.

random_list(0,Acc) ->
    Acc;
random_list(N,Acc) ->
    random_list(N-1,[random_byte() | Acc]).

random_binary(N) ->
    B = list_to_binary(random_list(N,[])),
    case {rand:uniform(2),size(B)} of
	{2,M} when M > 1 ->
	    S = M-1,
	    <<_:3,C:S/binary,_:5>> = B,
	    C;
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
	    byte
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

random_iolist(0,Acc) ->
    Acc;
random_iolist(N,Acc) ->
    case front() of
	true ->
	    case any_type() of
		list ->
		    X = random_length(N),
		    L = random_list(X),
		    random_iolist(N-X,[L|Acc]);
		binary ->
		    X = random_length(N),
		    B = random_binary(X),
		    random_iolist(N-X,[B|Acc]);
		iolist ->
		    X = random_length(N),
		    B = random_iolist(X),
		    random_iolist(N-X,[B|Acc]);
		byte ->
		    C = random_byte(),
		    random_iolist(N-1,[C|Acc])
	    end;
	false ->
	    case tail_type() of
		list ->
		    X = random_length(N),
		    L = random_list(X),
		    random_iolist(N-X,[Acc|L]);
		binary ->
		    X = random_length(N),
		    B = random_binary(X),
		    random_iolist(N-X,[Acc|B]);
		iolist ->
		    X = random_length(N),
		    B = random_iolist(X),
		    random_iolist(N-X,[Acc|B])
	    end
    end.

random_iolist(N) ->
    random_iolist(N,[]).
    

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
	
do_comp(List,List2,F1,F2) ->
    X = F1(List,List2),
    Y = F2(List,List2),
    case X =:= Y of
	false ->
	    exit({not_matching,List,List2,X,Y});
	_ ->
	    true
    end.

compare(0,Fun1,Fun2) ->
    do_comp(<<>>,Fun1,Fun2),
    do_comp([],Fun1,Fun2),
    do_comp([[]|<<>>],Fun1,Fun2),
    do_comp([<<>>,[]|<<>>],Fun1,Fun2),
    true;

compare(N,Fun1,Fun2) ->
    L = random_iolist(N),
    do_comp(L,Fun1,Fun2),
    compare(N-1,Fun1,Fun2).

compare2(0,Fun1,Fun2) ->
    L = random_iolist(100),
    do_comp(<<>>,L,Fun1,Fun2),
    do_comp(L,<<>>,Fun1,Fun2),
    do_comp(<<>>,<<>>,Fun1,Fun2),
    do_comp([],L,Fun1,Fun2),
    do_comp(L,[],Fun1,Fun2),
    do_comp([],[],Fun1,Fun2),
    do_comp([[]|<<>>],L,Fun1,Fun2),
    do_comp(L,[[]|<<>>],Fun1,Fun2),
    do_comp([[]|<<>>],[[]|<<>>],Fun1,Fun2),
    do_comp([<<>>,[]|<<>>],L,Fun1,Fun2),
    do_comp(L,[<<>>,[]|<<>>],Fun1,Fun2),
    do_comp([<<>>,[]|<<>>],[<<>>,[]|<<>>],Fun1,Fun2),
    true;

compare2(N,Fun1,Fun2) ->
    L = random_iolist(N),
    L2 = random_iolist(N),
    do_comp(L,L2,Fun1,Fun2),
    compare2(N-1,Fun1,Fun2).
