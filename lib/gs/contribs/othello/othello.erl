%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2009. All Rights Reserved.
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
-module(othello).
-export([start/0,new_game/4,start1/5]).



%%----------------------------------------------------------------------
%% The Othello program now uses the gs graphical package instead of the 
%% pxw package. See module othello_board for details
%%
%%----------------------------------------------------------------------


start() -> othello_board:start().

new_game(Computer,Player,Depth,Init) ->
    spawn_link(othello,start1,[self(),Computer,Player,Depth,Init]).

start1(Win,Computer,Player,Depth,Init) ->
    Board = othello_adt:new(t),
    random:seed(),
    init_display(Board,Win,Init),
    play(Computer,Player,Board,Depth,Win,1).

play(Computer,Player,Board,Depth,Win,NoDs) ->
    tell_win(Win,Computer,Player),
    case catch continue(Player,Board) of
	{game_over,Result} ->
	    game_over(Board,Player,Result,Win);
	{omit_draw,Player} ->
	    omit(Player,Win),
	    play(Computer,swap(Player),Board,Depth,Win,NoDs);
	ok ->
	    Draw = choose_draw(Computer,Player,Board,Depth,Win,NoDs),
	    Win ! {self(),draw,Draw},
	    Board1 = othello_adt:set(Draw,Player,Board),
	    display(Board1,Board,Win),
	    play(Computer,swap(Player),Board1,Depth,Win,NoDs+1)
    end.

continue(Player,Board) ->
    Draws = game_over(Player,Board),
    not_allowed(Draws,Player),
    ok.

choose_draw(Computer,Computer,Board,Depth,_Win,NoDs) ->    % Depth > 0 !!
    {Draw,_Value} = alpha_beta(Depth,Board,-11000,11000,Computer,NoDs),
%    io:format('Choosen draw is {~w,~w} : (~w)~n',
%	      [othello_adt:col(Draw),othello_adt:row(Draw),Value]),
%    io:format('=====================~n',[]),
    Draw;
choose_draw(Computer,Player,Board,Depth,Win,NoDs) ->
    receive
	{Win,position,Draw} ->
	    flush(Win),
	    case othello_adt:is_draw(Draw,Player,Board) of
		false ->
		    Win ! {self(),illegal_draw,Draw},
		    choose_draw(Computer,Player,Board,Depth,Win,NoDs);
		true ->
		    Draw
	    end
    end.

flush(Win) ->
    receive
	{Win,position,_} ->
	    flush(Win)
    after 1 ->
	    true
    end.

tell_win(Win,Computer,Player) ->
    Win ! {self(),player,Computer,Player},
    receive
	{Win,go_on_play} -> true
    end.

alpha_beta(0,Board,_,_,Player,_) ->
    {-1,othello_adt:evaluate_board(Player,Board)};
alpha_beta(Depth,Board,Alpha,Beta,Player,NoDs) ->
    case compute(Player,Board,NoDs) of
	[] ->
	    Player1 = swap(Player),
	    case compute(Player1,Board,NoDs) of
		[] ->
		    dead_lock(Board,Player);
		PosDraws1 ->
		    choose(PosDraws1,Board,Depth-1,-Beta,-Alpha,-1,
			   Player1,NoDs)
	    end;
	PosDraws ->
	    choose(PosDraws,Board,Depth-1,-Beta,-Alpha,-1,Player,NoDs)
%	    A = choose(PosDraws,Board,Depth-1,-Beta,-Alpha,-1,Player,NoDs),
%	    io:format('Alpha-Beta (~w) ==> ~w~n',[Depth,A]),
%	    A
    end.

choose([Draw|Draws],Board,Depth,Alpha,Beta,Record,Player,NoDs) ->
    Player1 = swap(Player),
    Board1 = othello_adt:set(Draw,Player,Board),
%    io:format('Alpha <~w> Beta <~w> ~n',[Alpha,Beta]),
    {_,Value} = alpha_beta(Depth,Board1,Alpha,Beta,Player1,NoDs+1),
    Value1 = -Value,
    cutoff(Draw,Value1,Depth,Alpha,Beta,Draws,Board,Record,Player,NoDs);
choose([],_,_,Alpha,_,Draw,_,_) ->
    {Draw,Alpha}.
    
cutoff(Draw,Value,_,_,Beta,_,_,_,_,_) when Value >= Beta ->
    {Draw,Value};
cutoff(Draw,Value,Depth,Alpha,Beta,Draws,Board,_,Player,NoDs)
  when Alpha < Value, Value < Beta ->
    choose(Draws,Board,Depth,Value,Beta,Draw,Player,NoDs);
cutoff(Draw,Value,Depth,Alpha,Beta,Draws,Board,Record,Player,NoDs)
  when Value == Alpha, NoDs < 13 ->
    choose(Draws,Board,Depth,Alpha,Beta,random_choice(Draw,Record),
	   Player,NoDs);
cutoff(_Draw,Value,Depth,Alpha,Beta,Draws,Board,Record,Player,NoDs)
  when Value =< Alpha ->
    choose(Draws,Board,Depth,Alpha,Beta,Record,Player,NoDs).

compute(Player,Board,NoOfDraws) when NoOfDraws < 13 ->
    case othello_adt:possible_draws(Player,Board,begin_play) of
	[] ->
	    othello_adt:possible_draws(Player,Board,playing);
	Draws ->
	    Draws
    end;
compute(Player,Board,_) ->
    othello_adt:possible_draws(Player,Board,playing).

%%----------------------------------------------------------
%% Randomly choose between two draws with the same value.
%%----------------------------------------------------------
    
random_choice(Draw,Draw1) ->
    case random:uniform(2) of
	1 ->
	    Draw;
	2 ->
	    Draw1
    end.

dead_lock(Board,Player) ->
    case win_or_loose(Board,Player) of
	0                    -> {-1,0};
	Value when Value > 0 -> {-1,10000};
	_                    -> {-1,-10000}
    end.

win_or_loose(Board,Player) ->
    Player1 = swap(Player),
    othello_adt:pieces(Player,Board) - othello_adt:pieces(Player1,Board).

game_over(Player,Board) ->
    case othello_adt:possible_draws(Player,Board,playing) of
	[] ->
	    Player1 = swap(Player),
	    case othello_adt:possible_draws(Player1,Board,playing) of
		[] ->
		    throw({game_over,{{Player,othello_adt:pieces(Player,Board)},
				      {Player1,othello_adt:pieces(Player1,Board)}}});
		_ ->
		    []  % Player`s Draws !!
	    end;
	Draws ->
	    Draws
    end.

game_over(_Board,_Player,Result,Win) ->
    Win ! {self(),game_over,white_res(Result),black_res(Result)}.

white_res({{white,Res},_}) -> Res;
white_res({_,{white,Res}}) -> Res.
	
black_res({{black,Res},_}) -> Res;
black_res({_,{black,Res}}) -> Res.

not_allowed([],Player) ->
    throw({omit_draw, Player});
not_allowed(_,_Player) ->
    ok.

omit(Player,Win) ->
    Win ! {self(),omit_draw,Player},
    receive
	{Win,continue} ->
	    ok
    end.

init_display(_Board,_Win,first_time) ->
    true;
init_display(Board,Win,_) ->
    display(Board,Win).

display(Board,Win) ->
    All = othello_adt:all_pos(Board),
    display1(All,Win),
    Win ! {self(),score,othello_adt:pieces(white,Board),
	   othello_adt:pieces(black,Board)}.

display(Board,OldB,Win) ->
    Diff = othello_adt:diff(Board,OldB),
    display1(Diff,Win),
    Win ! {self(),score,othello_adt:pieces(white,Board),
	   othello_adt:pieces(black,Board)}.
    
display1([{Pos,Colour}|Diff],Win) ->
    Win ! {self(),new_mark,Pos,Colour},
    display1(Diff,Win);
display1(_,_) ->
    true.

swap(white) -> black;
swap(black) -> white.


