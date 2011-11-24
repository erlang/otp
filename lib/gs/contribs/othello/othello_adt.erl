%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2010. All Rights Reserved.
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

%%
-module(othello_adt).
-compile(export_all).
%%-------------------------------------------------------
%% Use three main states for the strategy:
%%
%%  BeginPlay:  Stay in the inner square as long as possible.
%%              Use the possible_draws/3.
%%
%%  MiddlePlay: Try to choose stable markers (?)
%%              Use stable/3
%%
%%  EndPlay:    Try to flip as many markers as possible
%%
%%  The transition from Begin to Middle is obvious. From Middle
%%  to End however, is can be discussed.
%%-------------------------------------------------------

test(N,B) ->
    X=new(B),
    statistics(wall_clock),
    test0(N,X),
    {_,T} = statistics(wall_clock),
    {time_was,T/N}.


test0(0,_) -> true;
test0(N,X) -> 
    possible_draws(black,X,begin_play),
    test0(N-1,X).

%%-------------------------------------------------------
%% new/1 - returns a new board
%% 
%% Uses a tuple for storing the board
%%-------------------------------------------------------

new(B) ->
    Board = mk_board(B),
    {ordsets:from_list([18,19,20,21,26,29,34,37,42,43,44,45]),Board}.

mk_board(t) ->
    Tup = list_to_tuple(gen_list(64,grey)),
    Tup1 = setelement(28+1, Tup,  white),
    Tup2 = setelement(35+1, Tup1, white),
    Tup3 = setelement(27+1, Tup2, black),
    gen_score_board(),
    setelement(36+1, Tup3, black).

gen_list(0,_) -> [];
gen_list(I,Def) -> [Def|gen_list(I-1,Def)].

gen_score_board() -> put(score,list_to_tuple(gen_list(64,0))).

%%-------------------------------------------------------
%% pos(Col,Row) - returns a position describing column
%%                and row.
%%                Col and Row have the range 1 - 8.
%%-------------------------------------------------------

pos(Col,Row) -> ((Row - 1) bsl 3) + (Col - 1).

%%-------------------------------------------------------
%% col(Pos) - returns the column of the Pos position
%%-------------------------------------------------------

col(Pos) -> (Pos band 7) + 1.

%%-------------------------------------------------------
%% row(Pos) - returns the row of the Pos position
%%-------------------------------------------------------

row(Pos) -> (Pos bsr 3) + 1.

%%-------------------------------------------------------
%% is_draw(Pos,Colour,Board) - returns true if Pos is a
%%                             correct draw.
%%-------------------------------------------------------

is_draw(Pos,Colour,{Bset,Board}) ->
    case ordsets:is_element(Pos,Bset) of
	true ->
	    case catch is_good(Colour,Pos,Board) of
		true ->
		    true;
		_ ->
		    false
	    end;
	_ ->
	    false
    end.

%%-------------------------------------------------------
%% set(Pos,Colour,Board) - returns an updated board
%%-------------------------------------------------------

set(Pos,Colour,{Bset,Board}) ->
    case ordsets:is_element(Pos,Bset) of
	true ->
	    NewBoard = setelement(Pos+1,Board,Colour),
	    Empty = empty_neighbour(Pos,NewBoard),
	    NewBset = ordsets:union(Empty,ordsets:del_element(Pos,Bset)),
	    turn(Colour,Pos,{NewBset,NewBoard});
	_ ->
	    {error,invalid_position}
    end.
    
empty_neighbour(Pos,Board) ->
    ordsets:from_list(empty_neighbour(Pos,Board,deltas())).

empty_neighbour(_,_,[]) -> [];
empty_neighbour(Pos,Board,[H|T]) ->
    case is_empty(Pos+H,dir(Pos,H),Board) of
	true -> [Pos+H|empty_neighbour(Pos,Board,T)];
	_    -> empty_neighbour(Pos,Board,T)
    end.

is_empty(_,false,_) -> false;
is_empty(X,_,_Board) when X<0  -> false;
is_empty(X,_,_Board) when X>63 -> false;
is_empty(X,_,Board) ->
    case element(X+1,Board) of
	grey -> true;   % Empty 
	_ -> false
    end.

%%-------------------------------------------------------
%% get(Pos,Board) - returns the contents in Pos
%%-------------------------------------------------------

get(Pos,{_Bset,Board}) -> element(Pos+1,Board).

%%-------------------------------------------------------
%% pieces(Colour,Board) - returns the number of Colour
%%                        pieces.
%%-------------------------------------------------------

pieces(Colour,{_Bset,Board}) ->
    pieces(Colour,Board,0,0).

pieces(Colour,Board,Pos,Count) when Pos < 64 ->
    case element(Pos+1,Board) of
	Colour ->
	    pieces(Colour,Board,Pos+1,Count+1);
	_ ->
	    pieces(Colour,Board,Pos+1,Count)
    end;
pieces(_,_,_,Count) ->
    Count.

%%-------------------------------------------------------
%% possible_draws(Colour, Board, State) 
%% 
%% Returns a list of possible draws regarding the current
%% strategy state.
%%-------------------------------------------------------

possible_draws(Colour,{Bset,Board},begin_play) -> 
    Dset = ordsets:intersection(Bset,inner_square()),
    possible_draws_0(Colour,Dset,Board);
possible_draws(Colour,{Bset,Board},_) -> 
    possible_draws_0(Colour,Bset,Board).

possible_draws(Colour,{Bset,Board}) -> 
    possible_draws_0(Colour,Bset,Board).

possible_draws_0(_,[],_) -> [];
possible_draws_0(Colour,[H|T],Board) ->
    case catch is_good(Colour,H,Board) of
	true  -> [H|possible_draws_0(Colour,T,Board)];
	false -> possible_draws_0(Colour,T,Board)
    end.


%%-------------------------------------------------------
%% evaluate_board(Colour,Board) - returns the value of
%%                                the board from Colours
%%                                point of view.
%%-------------------------------------------------------

evaluate_board(Colour,{_Bset,Board}) ->
    Score = get(score),   % Initialized (zeroed) score board !!
    Colour1 = swap(Colour),
    Score1 = eval_board(Colour,Colour1,Score,Board,0),
    Score2 = cnt_corner(0,Score1,Board,Colour,Colour1),
    Score3 = cnt_corner(7,Score2,Board,Colour,Colour1),
    Score4 = cnt_corner(56,Score3,Board,Colour,Colour1),
    Score5 = cnt_corner(63,Score4,Board,Colour,Colour1),
    count(Score5,0).
%    A = count(Score5,0),
%    io:format('Score = ~w~n',[A]),
%    A.

eval_board(MyCol,OtCol,Score,Board,Pos) when Pos < 64 ->
    case element(Pos+1,Board) of
	MyCol ->
	    Score1 = setelement(Pos+1,Score,score(Pos)),
	    eval_board(MyCol,OtCol,Score1,Board,Pos+1);
	OtCol ->
	    Score1 = setelement(Pos+1,Score,-score(Pos)),
	    eval_board(MyCol,OtCol,Score1,Board,Pos+1);
	_ ->
	    eval_board(MyCol,OtCol,Score,Board,Pos+1)
    end;
eval_board(_,_,Score,_,_) ->
    Score.

cnt_corner(Corner,Score,Board,MyCol,OtCol) ->
    case element(Corner+1,Board) of
	MyCol ->
	    cnt_corn(Corner,setelement(Corner+1,Score,50),
		     Board,50,MyCol);
	OtCol ->
	    cnt_corn(Corner,setelement(Corner+1,Score,-50),
		     Board,-50,OtCol);
	_ ->
	    Score
    end.

cnt_corn(0,Score,Board,Value,Colour) ->
    Score1 = cnt_corn(0,1,8,Score,Board,Value,Colour),
    cnt_corn(0,8,1,Score1,Board,Value,Colour);
cnt_corn(7,Score,Board,Value,Colour) ->
    Score1 = cnt_corn(7,-1,8,Score,Board,Value,Colour),
    cnt_corn(7,8,-1,Score1,Board,Value,Colour);
cnt_corn(56,Score,Board,Value,Colour) ->
    Score1 = cnt_corn(56,1,-8,Score,Board,Value,Colour),
    cnt_corn(56,-8,1,Score1,Board,Value,Colour);
cnt_corn(63,Score,Board,Value,Colour) ->
    Score1 = cnt_corn(63,-1,-8,Score,Board,Value,Colour),
    cnt_corn(63,-8,-1,Score1,Board,Value,Colour).

cnt_corn(Pos,Dir,LineDir,Score,Board,Value,Colour) ->
    case dir(Pos,Dir) of
	Dir ->
	    NextEdge = Pos+Dir,
	    case element(NextEdge+1,Board) of
		Colour ->
		    Score1 = setelement(NextEdge+1,Score,Value),
		    Score2 = cnt_line(NextEdge,LineDir,Score1,Board,
				      Colour,Value),
		    cnt_corn(NextEdge,Dir,LineDir,Score2,Board,Value,Colour);
		_ ->
		    Score
	    end;
	_ ->
	    Score
    end.

cnt_line(Pos,Dir,Score,Board,Colour,Value) ->
    case dir(Pos,Dir) of
	Dir ->
	    OnLinePos = Pos+Dir,
	    case element(OnLinePos+1,Board) of
		Colour ->
		    Score1 = setelement(OnLinePos+1,Score,Value),
		    cnt_line(OnLinePos,Dir,Score1,Board,Colour,Value);
		_ ->
		    Score
	    end;
	_ ->
	    Score
    end.

count(Score,Pos) when Pos < 64 ->
    element(Pos+1,Score) + count(Score,Pos+1);
count(_,_) ->
    0.

swap(white) -> black;
swap(black) -> white.

%%-------------------------------------------------------
%% stable(Colour,Pos,Board) - returns a value 0-8 
%% 
%% A high value is regarded as more stable than a lower one.
%% The stability means how many "friendly" neighbours there
%% are, i.e markers of the same colour. Neighbours positions
%% outside the board are regarded as friendly.
%%-------------------------------------------------------

stable(Colour,Pos,{_,Board}) ->
    stable(deltas(),Colour,Pos,Board).

stable([],_,_,_) -> 0;
stable([H|T],Colour,Pos,Board) ->
    stable_val(Colour,Pos,H,Board) + stable(T,Colour,Pos,Board).

stable_val(_,H,D,_) when H+D<0 -> 1;
stable_val(_,H,D,_) when H+D>63 -> 1;
stable_val(black,H,D,Board) ->
    case element((H+D)+1,Board) of
	black -> 1;
	_     -> 0
    end;
stable_val(white,H,D,Board) ->
    case element((H+D)+1,Board) of
	white -> 1;
	_     -> 0
    end.

%%-------------------------------------------------------
%% diff(Board,OldBoard) - return a list of the positions
%%                        with changed pieces.
%%                        [{Pos1,Colour1},...]
%%-------------------------------------------------------

diff(Board,OldBoard) -> diff(0,Board,OldBoard).

diff(Pos,Board,OldBoard) when Pos < 64 ->
    OldP = get(Pos,OldBoard),
    case get(Pos,Board) of
	OldP ->
	    diff(Pos+1,Board,OldBoard);
	NewP ->
	    [{Pos,NewP}|diff(Pos+1,Board,OldBoard)]
    end;
diff(_,_,_) ->
    [].

%%-------------------------------------------------------
%% all_pos(Board) - return a list of the positions colour.
%%                        [{Pos1,Colour1},...]
%%-------------------------------------------------------

all_pos(Board) -> all_pos(0,Board).

all_pos(Pos,Board) when Pos < 64 ->
    [{Pos,get(Pos,Board)}|all_pos(Pos+1,Board)];
all_pos(_,_) ->
    [].

%%-------------------------------------------------------
%% Internal stuff
%%-------------------------------------------------------

deltas() -> [9,8,7,1,-1,-7,-8,-9].

inner_square() ->
    [18,19,20,21,26,27,28,29,34,35,36,37,42,43,44,45].  % Is already an ordset
                                                        % Save list traversing.
%    ordsets:list_to_set([18,19,20,21,26,27,28,29,34,35,36,37,42,43,44,45]).

inv(black) -> white;
inv(white) -> black.

is_good(Colour,H,Board) ->
    is_good_0(Colour,H,dir(H,-9),Board),
    is_good_0(Colour,H,dir(H,-8),Board),
    is_good_0(Colour,H,dir(H,-7),Board),
    is_good_0(Colour,H,dir(H,-1),Board),
    is_good_0(Colour,H,dir(H,1),Board),
    is_good_0(Colour,H,dir(H,7),Board),
    is_good_0(Colour,H,dir(H,8),Board),
    is_good_0(Colour,H,dir(H,9),Board),
    false.

is_good_0(_,_,false,_) -> false;
is_good_0(_,H,D,_) when is_integer(H), is_integer(D), H+D<0 -> false;
is_good_0(_,H,D,_) when is_integer(H), is_integer(D), H+D>63 -> false;
is_good_0(black,H,D,Board) when is_integer(H), is_integer(D) ->
    case element((H+D)+1,Board) of
	white -> is_good_1(black,H+D,dir(H+D,D),Board);
	_     -> false
    end;
is_good_0(white,H,D,Board) when is_integer(H), is_integer(D) ->
    case element((H+D)+1,Board) of
	black -> is_good_1(white,H+D,dir(H+D,D),Board);
	_     -> false
    end.

is_good_1(_,_,false,_) -> false;
is_good_1(_,H,D,_) when is_integer(H), is_integer(D), H+D<0 -> false;
is_good_1(_,H,D,_) when is_integer(H), is_integer(D), H+D>63 -> false;
is_good_1(black,H,D,Board) when is_integer(H), is_integer(D) ->
    case element((H+D)+1,Board) of
	white -> is_good_1(black,H+D,dir(H+D,D),Board);
	black -> throw(true);
	_     -> false
    end;
is_good_1(white,H,D,Board) when is_integer(H), is_integer(D) ->
    case element((H+D)+1,Board) of
	black -> is_good_1(white,H+D,dir(H+D,D),Board);
	white -> throw(true);
	_     -> false
    end.

%%-------------------------------------------------------
%% turn(Colour,Draw,Board) - returns an updated board
%%                           turn all possible pieces
%%                           on the board
%%                           Neighbours are not changed !!
%%-------------------------------------------------------

turn(Colour,Draw,{Bset,Board}) ->
    {Bset,turn(Colour,Draw,-9,
	       turn(Colour,Draw,-8,
		    turn(Colour,Draw,-7,
			 turn(Colour,Draw,-1,
			      turn(Colour,Draw,1,
				   turn(Colour,Draw,7,
					turn(Colour,Draw,8,
					     turn(Colour,Draw,9,Board))))))))}.

turn(Colour,H,D,Board) ->
    case catch is_good_0(Colour,H,dir(H,D),Board) of
	true ->
	    turn_0(Colour,H,D,Board);
	false ->
	    Board
    end.

turn_0(_,H,D,B) when is_integer(H), is_integer(D), H+D<0 -> B;
turn_0(_,H,D,B) when is_integer(H), is_integer(D), H+D>63 -> B;
turn_0(black,H,D,Board) when is_integer(H), is_integer(D) ->
    E = H+D,
    case element(E+1,Board) of
	white -> turn_0(black,H+D,D,swap(black,E,Board));
	_     -> Board
    end;
turn_0(white,H,D,Board) when is_integer(H), is_integer(D) ->
    E = H+D,
    case element(E+1,Board) of
	black -> turn_0(white,H+D,D,swap(white,E,Board));
	_     -> Board
    end.

%%-------------------------------------------------------
%% swap(Colour,Pos,Board) - returns an updated board
%%                          turn a piece on the board
%%                          Neighbours are not changed !!
%%-------------------------------------------------------

swap(Colour,Pos,Board) when is_integer(Pos) ->
    setelement(Pos+1,Board,Colour).

score(Pos) -> score1({col(Pos),row(Pos)}).
    
score1({Column,1}) when Column >= 3, Column =< 6 -> 20;              
score1({Column,8}) when Column >= 3, Column =< 6 -> 20;              
score1({1,Line}) when Line >= 3, Line =< 6 -> 20;              
score1({8,Line}) when Line >= 3, Line =< 6 -> 20;              
score1({Column,2}) when Column >= 3, Column =< 6 -> -7;
score1({Column,7}) when Column >= 3, Column =< 6 -> -7;
score1({2,Line}) when Line >= 3, Line =< 6 -> -7;
score1({7,Line}) when Line >= 3, Line =< 6 -> -7;
score1({Column,Line}) when Column >= 3, Column =< 6,
                           Line >= 3, Line =< 6 -> 1;
score1({1,1}) -> 100;
score1({1,8}) -> 100;
score1({8,1}) -> 100;
score1({8,8}) -> 100;
score1({2,1}) -> -30;
score1({7,1}) -> -30;
score1({1,2}) -> -30;
score1({8,2}) -> -30;
score1({1,7}) -> -30;
score1({8,7}) -> -30;
score1({2,8}) -> -30;
score1({7,8}) -> -30;
score1({2,2}) -> -50;
score1({7,2}) -> -50;
score1({2,7}) -> -50;
score1({7,7}) -> -50.

%%-------------------------------------------------------
%% dir(Pos,Dir) - return Dir if allowed direction at Pos.
%%                else return false.
%%-------------------------------------------------------

dir(0,1) -> 1;         % {1,1}
dir(0,8) -> 8;
dir(0,9) -> 9;
dir(0,_) -> false;

dir(7,-1) -> -1;       % {8,1}
dir(7,7) -> 7;
dir(7,8) -> 8;
dir(7,_) -> false;

dir(56,-8) -> -8;      % {1,8}
dir(56,-7) -> -7;
dir(56,1) -> 1;
dir(56,_) -> false;

dir(63,-9) -> -9;      % {8,8}
dir(63,-8) -> -8;
dir(63,-1) -> -1;
dir(63,_) -> false;

dir(Pos,-1) when (Pos bsr 3) == 0 -> -1;   % {_,1}
dir(Pos,1) when (Pos bsr 3) == 0 -> 1;  
dir(Pos,7) when (Pos bsr 3) == 0 -> 7;  
dir(Pos,8) when (Pos bsr 3) == 0 -> 8;  
dir(Pos,9) when (Pos bsr 3) == 0 -> 9;  
dir(Pos,_) when (Pos bsr 3) == 0 -> false;

dir(Pos,-9) when (Pos bsr 3) == 7 -> -9;   % {_,8}
dir(Pos,-8) when (Pos bsr 3) == 7 -> -8;  
dir(Pos,-7) when (Pos bsr 3) == 7 -> -7;  
dir(Pos,-1) when (Pos bsr 3) == 7 -> -1;  
dir(Pos,1) when (Pos bsr 3) == 7 -> 1;  
dir(Pos,_) when (Pos bsr 3) == 7 -> false;

dir(Pos,-8) when (Pos band 7) == 0 -> -8;    % {1,_}
dir(Pos,-7) when (Pos band 7) == 0 -> -7;       
dir(Pos,1) when (Pos band 7) == 0 -> 1;       
dir(Pos,8) when (Pos band 7) == 0 -> 8;       
dir(Pos,9) when (Pos band 7) == 0 -> 9;       
dir(Pos,_) when (Pos band 7) == 0 -> false;       

dir(Pos,-9) when (Pos band 7) == 7 -> -9;    % {8,_}
dir(Pos,-8) when (Pos band 7) == 7 -> -8;       
dir(Pos,-1) when (Pos band 7) == 7 -> -1;       
dir(Pos,7) when (Pos band 7) == 7 -> 7;       
dir(Pos,8) when (Pos band 7) == 7 -> 8;       
dir(Pos,_) when (Pos band 7) == 7 -> false;       

dir(_Pos,Dir) -> Dir.

