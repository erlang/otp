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
-module(highscore).

-export([run/2]).

run(NScore, File) ->
    Scores = read_scores(File),
    case find_pos(NScore, 1, Scores) of
	false ->
	    display(Scores);
	Pos ->
	    NewScores = new_highscore(Scores, Pos, NScore),
	    write_scores(NewScores,File),
	    display(NewScores)
    end.


new_highscore(Scores, Pos, NScore) ->
    Txt = io_lib:format("You entered position ~w", [Pos]),
    W = gs:create(window, gs:start(), [{width, 200},{height, 110},{map,true},
				      {title, "New Highscore!!!"}]),
    gs:create(label, W, [{label, {text, Txt}}, {x, 0}, {y,0}, {align, center},
			 {width, 190},{height, 30}]),
    Entry = gs:create(entry, W, [{x, 0}, {y, 40}, {height, 30}, {width, 200}]),
    Ok = gs:create(button, W, [{label, {text, "Ok"}}, {x, 40}, {y, 75}]),
    receive
	{gs, Ok, click, _,_} -> 
	    T = gs:read(Entry, text),
	    gs:destroy(W),
	    lists:sublist(lists:reverse(
                 lists:keysort(1, [{NScore, T} | Scores])), 1, 10)
    end.



read_scores(File) ->
    case file:read_file(File) of
	{ok, Bin} -> binary_to_term(Bin);
	{error, _Reason} ->
	    mk_empty_high(10)
    end.

mk_empty_high(0) -> [];
mk_empty_high(N) -> [{N,"Erlang"}|mk_empty_high(N-1)].

find_pos(_NScore, _N, []) -> false;
find_pos(NScore, N, [{Score, _Name} | Scores]) when Score > NScore ->
    find_pos(NScore, N+1, Scores);
find_pos(_NScore, N, _) -> N.

write_scores(Scores,File) ->
    file:write_file(File, term_to_binary(Scores)).

display(Scores) ->
    Win = gs:window(gs:start(), [{width, 300},{height, 250},{map,true},
				 {title, "Highscores"}]),
    {W,H} = gs:read(Win,{font_wh,{{screen,12},"aaaaaaa"}}),
    G = gs:grid(Win,[{rows,{1,11}},{columnwidths,[W,4*W]},{hscroll,false},
		     {width, 300},{height, 220},{vscroll,false},
		     {cellheight,H+2},{font,{screen,12}}]),
    insert_scores(G,2,Scores),
    Ok = gs:button(Win, [{label, {text, "OK"}}, {x, 100}, {y, 220}]),
    receive
	{gs, Ok, click, _,_} -> gs:destroy(Win),
				ok
    end.

insert_scores(Grid,_N,[]) ->
    gs:create(gridline,Grid,[{row,1},{font,{screen,bold,12}},
			     {text,{1,"SCORE"}},{text,{2,"NAME"}}]);

insert_scores(Grid,Row,[{Score,Name}|Ss]) ->
    gs:create(gridline,Grid,[{row,Row},{text,{1,io_lib:format("~w",[Score])}},
			    {text,{2,Name}}]),
    insert_scores(Grid,Row+1,Ss).

