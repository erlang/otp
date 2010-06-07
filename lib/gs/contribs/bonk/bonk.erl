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
-module(bonk).
-export([run/0, run/1,bonk_dir/0,start/0]).

-record(colors, {miss, x, bomb, face}).
-record(scores, {points, level, bombs, hits, showed, bonus}).

start() ->
    spawn(bonk, run, []).

run() ->
    run(color).

run([ColorMode]) ->   % This is for the start script...
    run(ColorMode);

run(ColorMode) when is_atom(ColorMode) ->
    GS = gs:start(),
    SoundPid = spawn_link(bonk_sound,start,[]),
    {H,M,S} = time(),
    random:seed(H*13,M*7,S*3),
    {SqrPids, Bmps, Colors} = create_board(GS, ColorMode),
    {ScoreL,_File} = get_highscore(),
    display_highscore(ScoreL),
    put(colormode, ColorMode),
    SoundPid ! music,
    sleep(6500),
    gs:config(aboutButton, [{enable,true}]),
    gs:config(newButton, [{enable,true}]),
    gs:config(quitButton, [{enable,true}]),
    idle(SoundPid, SqrPids, Bmps, Colors).

%% This is not an application so we don't have their way of knowing
%% a private data directory where the GIF files are located (this directory).
%% We can find GS and makes it relative from there /kgb
%%
%% Note the silly slash that is added. The rest of the code uses
%% append to contruct file names and assumes that the directory ends
%% in slash. If filename:join was used and the problem is gone.

-define(EbinFromGsPriv,"../contribs/bonk").

bonk_dir() ->
    GsPrivDir = code:priv_dir(gs),
    filename:join(GsPrivDir,?EbinFromGsPriv) ++ "/".


idle(SoundPid, SqrPids, Bmps, Colors) ->
    receive
	{gs, newButton, click, _Data, _Args} ->
	    init(SoundPid, SqrPids, Bmps, Colors);
	{gs, aboutButton, click, _Data, _Args} ->
	    display_about(),
	    idle(SoundPid, SqrPids, Bmps, Colors);
	{gs, quitButton, click, _Data, _Args} ->
	    SoundPid ! quit,
	    send_to_all(SqrPids, quit);
	_Other ->
	    %%io:format("Got ~w in idle~n", [_Other]),
	    idle(SoundPid, SqrPids, Bmps, Colors)
    end.



init(SoundPid, SqrPids, Bmps, Colors) ->
    clear_board(Bmps),
    SoundPid ! start,
    gs:config(newButton, [{enable,false}]),
    gs:config(endButton, [{enable,true}]),
    gs:config(aboutButton, [{enable,false}]),
    Scores = #scores{points=0, level=1, bombs=0, hits=0, showed=0, bonus=10},
    clear_scores(Scores),
    flush(),
    send_to_all(SqrPids, start),
    game(SoundPid, SqrPids, Bmps, Colors, Scores).


game(SoundPid, SqrPids, Bmps, Colors, Scores) ->
    receive
	{gs, _Square, buttonpress, SqrPid, [1 | _Rest]} when is_pid(SqrPid) ->
	    SqrPid ! bonk,
	    game(SoundPid, SqrPids, Bmps, Colors, Scores);
	{gs, _Id, buttonpress, _Data, [Butt | _Rest]} when Butt =/= 1 ->
	    NewScores = bomb(SoundPid, SqrPids, Scores),
	    game(SoundPid, SqrPids, Bmps, Colors, NewScores);
	{show, Square, Rect} ->
	    NewScores = show_face(Square, Rect, Colors, Scores),
	    game(SoundPid, SqrPids, Bmps, Colors, NewScores);
	{hide, Square, Rect} ->
	    NewScores = hide_face(Square, Rect, Colors, Scores),
	    game(SoundPid, SqrPids, Bmps, Colors, NewScores);
	{missed, Square, Rect} ->
	    case miss_face(SoundPid, Square, Rect, Colors, Scores) of
		{continue, NewScores} ->
		    game(SoundPid, SqrPids, Bmps, Colors, NewScores);
		{game_over, NewScores} ->
		    game_over(SoundPid, SqrPids, Bmps, Colors, NewScores)
	    end;
	{bonked, Square, Rect} ->
	    NewScores = bonked(SoundPid, SqrPids, Square, Rect, Scores, Colors),
	    game(SoundPid, SqrPids, Bmps, Colors, NewScores);
	{bombed, Square, Rect} ->
	    NewScores = bombed(SoundPid, SqrPids, Square, Rect, Scores, Colors),
	    game(SoundPid, SqrPids, Bmps, Colors, NewScores);
	{gs, endButton, click, _Data, _Args} ->
	    game_over(SoundPid, SqrPids, Bmps, Colors, Scores);
	{gs, quitButton, click, _Data, _Args} ->
	    quit(SoundPid, SqrPids, Bmps, Colors, Scores);
	_Other ->
	    game(SoundPid, SqrPids, Bmps, Colors, Scores)
    end.
	    


game_over(SoundPid, SqrPids, Bmps, Colors, Scores) ->
    SoundPid ! game_over,
    send_to_all(SqrPids, stop),
    flush(),
    sleep(2000),
    update_scorelist(SoundPid, Scores),
    gs:config(newButton, [{enable,true}]),
    gs:config(endButton, [{enable,false}]),
    gs:config(aboutButton, [{enable,true}]),
    idle(SoundPid, SqrPids, Bmps, Colors).


quit(SoundPid, SqrPids, _Bmps, _Colors, _Scores) ->
    SoundPid ! quit,
    send_to_all(SqrPids, quit),
    true.



bomb(SoundPid, SqrPids, Scores) ->
    case Scores#scores.bombs of
	Bombs when Bombs > 0 ->
	    send_to_all(SqrPids, bomb),
	    SoundPid ! bomb,
	    gs:config(bombOut,[{text,integer_to_list(Bombs-1)}]),
	    Scores#scores{bombs=Bombs-1};
	_Other ->
	    Scores
    end.

show_face(Square, Rect, Colors, Scores) ->
    Showed = Scores#scores.showed,
    if
	Showed == Scores#scores.level+1 ->
	    Square ! sleep,
	    Scores;
	true ->
	    FaceColors = Colors#colors.face,
	    FaceColor = lists:nth(random:uniform(length(FaceColors)), FaceColors),
	    gs:config(Rect, [{bitmap,lists:append(bonk_dir(),"bitmaps/bonkface")},{fg, FaceColor}]),
	    Scores#scores{showed=Showed+1}
    end.
    
hide_face(_Square, Rect, _Colors, Scores) ->
    Showed = Scores#scores.showed,
    gs:config(Rect, [{bitmap,lists:append(bonk_dir(),"bitmaps/bonktom")}]),
    Scores#scores{showed=Showed-1}.


miss_face(SoundPid, _Square, Rect, Colors, Scores) ->
    SoundPid ! missed,
    gs:config(Rect, [{bitmap,lists:append(bonk_dir(),"bitmaps/bonkmiss")}, {fg, Colors#colors.miss}]),
    Bonus = Scores#scores.bonus,
    if
	Bonus > 1 ->
	    gs:config(bonusOut, [{text,integer_to_list(Bonus-1)}]),
	    {continue, Scores#scores{bonus=Bonus-1}};
	true ->
	    gs:config(bonusOut, [{text,"0"}]),
	    {game_over, Scores}
    end.

bonked(SoundPid, SqrPids, _Square, Rect, Scores, Colors) ->
    gs:config(Rect, [{bitmap,lists:append(bonk_dir(),"bitmaps/bonkx")}, {fg, Colors#colors.x}]),
    SoundPid ! bonk,
    update_score(SoundPid, SqrPids, Scores).

bombed(SoundPid, SqrPids, _Square, Rect, Scores, Colors) ->
    gs:config(Rect, [{bitmap,lists:append(bonk_dir(),"bitmaps/bonkbomb")}, {fg, Colors#colors.bomb}]),
    update_score(SoundPid, SqrPids, Scores).


update_score(SoundPid, SqrPids, Scores) ->
    Points = Scores#scores.points,
    Level = Scores#scores.level,
    NewPoints = Points+Level,
    gs:config(scoreOut,[{text,integer_to_list(NewPoints)}]),
    case Scores#scores.hits of
	24 ->
	    SoundPid ! new_level,
	    NewLevel = Level+1,
	    NewBombs = Scores#scores.bombs+1,
	    send_to_all(SqrPids, {new_level, NewLevel}),
	    gs:config(levelOut,[{text,integer_to_list(NewLevel)}]),
	    gs:config(bombOut,[{text,integer_to_list(NewBombs)}]),
	    Scores#scores{points=NewPoints, level=NewLevel, hits=0, bombs=NewBombs};
	Hits ->
	    Scores#scores{points=NewPoints, hits=Hits+1}
    end.
	    

send_to_all([], _Msg) ->
    true;
send_to_all([Pid|Rest],Msg) when is_pid(Pid) ->
    Pid ! Msg,
    send_to_all(Rest,Msg);
send_to_all([_Else|Rest],Msg) ->
    send_to_all(Rest,Msg).


create_board(GS, ColorMode) ->
    Colors =
	case ColorMode of
	    bw     -> #colors{miss=white, x=white, bomb=white, face=[white]};
	    _Color -> #colors{miss=red, x=green, bomb=white,
			      face=[lightblue, orange, magenta, peachpuff, pink]}
	end,
    BGCol     = if ColorMode==bw -> black; true -> black  end,	% background color
    TextCol   = if ColorMode==bw -> white; true -> pink   end,	% status texts
    NrCol     = if ColorMode==bw -> white; true -> purple end,	% status figures
    LogoCol   = if ColorMode==bw -> white; true -> green  end,	% bonk logo
    BLineCol  = if ColorMode==bw -> white; true -> grey   end,	% button line
    SLineCol  = if ColorMode==bw -> white; true -> red    end,	% status line
    BTextCol  = if ColorMode==bw -> white; true -> orange end,	% button text
    HiHeadCol = if ColorMode==bw -> white; true -> red    end,	% high score label
    HiCol     = if ColorMode==bw -> white; true -> cyan   end,	% high scores
    SquareCol = if ColorMode==bw -> white; true -> yellow end,	% game squares
    ErlFgCol  = if ColorMode==bw -> white; true -> red    end,	%
    ErlBGCol  = if ColorMode==bw -> black; true -> white  end,	%
    ErlTxtCol = if ColorMode==bw -> white; true -> black  end,	%

    Width = 550,      	       % width of bonk window
    Height = 550,     	       % Height of bonk window
    
    BX = 0,           	       % x-pos for first button
    DBX = 100,        	       % space between buttons
    BY = 0,           	       % y-pos for buttons
    BLineY = 30,      	       % y-pos of button line
    LogoX = (Width-320) div 2, % x-pos of bonk logo (logo is 320 pix wide)
    LogoY = BLineY+2, 	       % y-pos of bonk logo
    ErlLogoX = LogoX + 200,    % x-pos of Erlang e
    ErlLogoY = LogoY + 10,     % y-pos of Erlang e
    SLineY = Height-22,        % status line position
    TextWidth = 50,   	       % text width of status items
    SX = 2,          	       % x-pos for first status item
    DSX = TextWidth+94,	       % pixels between status items
    SY = SLineY+2,   	       % y-pos status items
    HiWidth = 100,             % width of high score field
    _HiHeight = 180,            % height of the same
    HiX = Width-HiWidth,       % high score text position
    HiY = BLineY+10,
    DHY = 20,                  % space between title & scores
    SquareSize = 76,           % size of each game square
    SquareSpace = 1,           % space between game squares
    SquareX = 40,
    SquareY = 65,
    
    gs:create(window, bonkWin, GS, [{width, Width}, {height, Height}, 
				    {bg, BGCol},
				    {title, "Bonk the game"},
				    {iconname, "Bonk!"},
				    {map, false}]),
    gs:create(canvas, bonkCanvas, bonkWin, [{width, Width},
					    {height, Height},
					    {bg, BGCol}]),
    gs:create(image, bonkCanvas, [{bitmap,lists:append(bonk_dir(), "bitmaps/bonklogo")},
				  {coords, [{LogoX, LogoY}]},
				  {fg, LogoCol},
				  {bg, BGCol}]),
    gs:create(image, bonkCanvas, [{bitmap,lists:append(bonk_dir(), "bitmaps/erl-e")},
				  {coords, [{ErlLogoX, ErlLogoY}]},
				  {fg, ErlFgCol},
				  {bg, ErlBGCol}]),
    gs:create(image, bonkCanvas, [{bitmap,lists:append(bonk_dir(), "bitmaps/erl-text")},
				  {coords, [{ErlLogoX, ErlLogoY}]},
				  {fg, ErlTxtCol}]),
    gs:create(line, bLine, bonkCanvas, [{coords, [{0,BLineY}, {Width,BLineY}]},
					{fg, BLineCol},
					{width, 2}]),
    gs:create(line, bLine, bonkCanvas, [{coords, [{0,SLineY}, {Width, SLineY}]},
					{fg, SLineCol},
					{width, 2}]),
    gs:create(text, scoreText, bonkCanvas, [{coords, [{SX, SY}]},
					    {fg, TextCol},
					    {text, "Score:"}]),
    gs:create(text, scoreOut, bonkCanvas, [{coords, [{SX+TextWidth, SY}]},
					   {fg, NrCol},
					   {width, DSX-TextWidth},
					   {text, ""}]),
    gs:create(text, bombText, bonkCanvas, [{coords, [{SX+DSX, SY}]},
					   {fg, TextCol},
					   {text, "Bombs:"}]),
    gs:create(text, bombOut, bonkCanvas, [{coords, [{SX+DSX+TextWidth, SY}]},
					  {fg, NrCol},
					  {width, DSX-TextWidth},
					  {text, ""}]),
    gs:create(text, bonusText, bonkCanvas, [{coords, [{SX+2*DSX, SY}]},
					    {fg, TextCol},
					    {text, "Bonus:"}]),
    gs:create(text, bonusOut, bonkCanvas, [{coords, [{SX+2*DSX+TextWidth, SY}]},
					   {fg, NrCol},
					   {width, DSX-TextWidth},
					   {text, ""}]),
    gs:create(text, levelText,bonkCanvas, [{coords, [{SX+3*DSX, SY}]},
					   {fg, TextCol},
					   {text, "Level:"}]),
    gs:create(text, levelOut, bonkCanvas, [{coords, [{SX+3*DSX+TextWidth, SY}]},
					   {fg, NrCol},
					   {width, DSX-TextWidth},
					   {text, ""}]),
    gs:create(text, hiScoreText, bonkCanvas, [{coords, [{HiX, HiY}]},
					      {fg, HiHeadCol},
					      {text, "High Scores"}]),
    gs:create(text, hiScoreOut, bonkCanvas, [{coords, [{HiX, HiY+DHY}]},
					     {fg, HiCol},
					     {justify, left},
					     {width, HiWidth}]),
    gs:create(button, newButton,bonkWin, [{x, BX},{y, BY},
					  {enable,false},
					  {label, {text, "New Game"}},
					  {click, true},
					  {fg, BTextCol},
					  {bg, BGCol},
					  {relief, flat},
					  {activefg, BTextCol},
					  {activebg, BGCol},
					  {align, center}]),
    gs:create(button, endButton,bonkWin, [{x, BX+DBX},{y, BY},
					  {enable,false},
					  {label, {text, "End Game"}},
					  {click, true},
					  {fg, BTextCol},
					  {bg, BGCol},
					  {relief, flat},
					  {activefg, BTextCol},
					  {activebg, BGCol},
					  {align, center}]),
    gs:create(button, aboutButton,bonkWin, [{x, BX+2*DBX},{y, BY},
					    {enable,false},
					    {label, {text, "About"}},
					    {click, true},
					    {fg, BTextCol},
					    {bg, BGCol},
					    {relief, flat},
					    {activefg, BTextCol},
					    {activebg, BGCol},
					    {align, center}]),
    gs:create(button, quitButton, bonkWin, [{x, BX+3*DBX},{y, BY},
					    {enable,false},
					    {label, {text, "Quit"}},
					    {click, true},
					    {fg, BTextCol},
					    {bg, BGCol},
					    {relief, flat},
					    {activefg, BTextCol},
					    {activebg, BGCol},
					    {align, center}]),

    {SqrPids, Bmps} =
	create_squares(SquareX, SquareY, SquareSize, SquareCol, SquareSpace),
    gs:config(bonkWin, [{map, true}]),
    {SqrPids, Bmps, Colors}.



create_squares(X, Y, Size, Color, Spc) ->
    create_squares(X, Y, Size, Color, Spc, 1, 1, [], []).


create_squares(_X, _Y, _Size, _Color, _Spc, 4, 5, Pids, Bmps) ->
    {Pids, Bmps};

create_squares(X, Y, Size, Color, Spc, Row, 5, Pids, Bmps) ->
    create_squares(X, Y, Size, Color, Spc, Row+1, 1, Pids, Bmps);

create_squares(X, Y, Size, Color, Spc, Row, Col, Pids, Bmps) ->
    Xpos = X+Col*Size+(Col-1)*Spc,
    Ypos = Y+Row*Size+(Row-1)*Spc,
    gs:create(rectangle, bonkCanvas,
	      [{coords, [{Xpos,Ypos},{Xpos+Size, Ypos+Size}]},
	       {bw, 2},{fg, Color},{buttonpress,true}]),
    Bmp = gs:create(image, bonkCanvas,
		    [{coords, [{Xpos+1, Ypos+1}]},
		     {bitmap,lists:append(bonk_dir(), "bitmaps/bonktom")},
		     {buttonpress, true},{fg, Color}]),
    Pid = bonk_square:start(Bmp),
    gs:config(Bmp, [{data, Pid}]),
    create_squares(X, Y, Size, Color, Spc, Row, Col+1, [Pid|Pids], [Bmp|Bmps]).



clear_board([]) ->
    true;
clear_board([Square | Rest]) ->
    gs:config(Square, [{bitmap,lists:append(bonk_dir(), "bitmaps/bonktom")}]),
    clear_board(Rest).


%% Prints the list on the screen.
%% The list is on the form [[Score,Name],[Score,Name]..].

display_highscore(ScoreList) ->
    display_highscore("",ScoreList,0).

display_highscore(Scores,[],_N) ->
    gs:config(hiScoreOut,[{text,Scores}]);

display_highscore(Scores,_ScoreList,10) ->   % This is max number of items.
    display_highscore(Scores,[], 10);

display_highscore(Scores,[[Score,Name]|Rest],N) ->
    NewScores = lists:append(Scores,lists:append(lists:append(Score, [32 | Name]), [10])),
    display_highscore(NewScores,Rest,N+1).


%% Reads the highscorelist from the file "bonk.score".
%% The list should be an sorted erlang-list.

get_highscore() ->
    case file:consult("bonk.score") of
	{ok,[Score_list]} ->
	    {Score_list,"./bonk.score"};
	{error,_} ->
	    {[],"./bonk.score"}
    end.


%% Prints out the highscorelist and places the new score in the
%% list if it is high enough.

update_scorelist(SoundPid, Scores) ->
    case Scores#scores.points of
	0 -> true;
	Score ->
	    {ScoreL,FileName} = get_highscore(),
	    New_scorelist=update_scorelist_2(ScoreL, Score, 0, SoundPid),
	    display_highscore(New_scorelist),
	    case file:open(FileName, [write]) of
		{error,_} ->
		    true;
		{ok,FD} ->
		    io:format(FD,"~w~s~n",[New_scorelist,"."]),
		    file:close(FD)
	    end
    end.


update_scorelist_2([], Score, N, _SoundPid) when N < 10 ->
    [[integer_to_list(Score),getuser()]];

update_scorelist_2(_, _, N, _SoundPid) when N >= 10 ->
    [];

update_scorelist_2([[Sc, Name] | Rest], Score, N, SoundPid) ->
    case list_to_integer(Sc) of
	Sc_int when Sc_int < Score ->
	    if
		N == 0 -> SoundPid ! best_score;
		true   -> SoundPid ! high_score
	    end,
	    lists:append([[integer_to_list(Score),getuser()]],
			 update_scorelist_3([[Sc,Name]|Rest],N+1));
	_Other ->
	    lists:append([[Sc,Name]],update_scorelist_2(Rest, Score, N+1, SoundPid))
    end.


update_scorelist_3([],_) ->
    [];

update_scorelist_3(_,N) when N >= 10 ->
    [];

update_scorelist_3([Item|Rest],N) ->
    lists:append([Item],update_scorelist_3(Rest,N+1)).

getuser() ->
    case os:type() of
	{unix,_} ->
	    lists:delete(10,os:cmd("whoami"));
	_ ->
	    "Unknown"
    end.

%% Prints out the initial values of scores, bonus, level and bombs.

clear_scores(Scores) ->
    Score = integer_to_list(Scores#scores.points),
    Bombs = integer_to_list(Scores#scores.bombs),
    Bonus = integer_to_list(Scores#scores.bonus),
    Level = integer_to_list(Scores#scores.level),
    gs:config(scoreOut,{text,Score}),
    gs:config(bombOut,{text,Bombs}),
    gs:config(bonusOut,{text,Bonus}),
    gs:config(levelOut,{text,Level}).


%% Removes everything that is present in the message-que.

flush() ->
    receive
	_X ->
	    flush()
	after
	    0 ->
		true
	end.

sleep(X) ->
    receive after X -> true end.

%% Opens a window and shows the contents of the file: "bonk.txt".
%% The window will be removed before the function ends.

display_about() ->
    {BGColor,TextColor,Bfg} =
	case get(colormode) of
	    bw     -> {black, white, white};
	    _Color -> {black, peachpuff1, orange}
	end,
    Wid = 500, Hei = 635, 
    GS = gs:start(),
    gs:create(window, aboutWin, GS, [{width, Wid}, {height,Hei},
				     {map, false},
				     {bg, BGColor},
				     {title, "About Bonk!"}]),
    gs:create(canvas, aboutCan, aboutWin, [{width, Wid},{height, Hei},
					   {bg, black}]),
    gs:create(button, okButton, aboutWin, [{x, Wid div 2 - 50},{y, Hei - 40},
					   {label,{text, "Ok"}}, {click, true},
					   {fg, Bfg}, {bg, BGColor},
					   {relief, flat},
					   {activefg, Bfg},
					   {activebg, BGColor}]),
    gs:create(text, aboutText, aboutCan, [{width, Wid-30}, {coords, [{15, 0}]},
					  {fg, TextColor}, {justify, center}]),
    case file:open(lists:append(bonk_dir(),"bonk.txt"), [read]) of
	{ok, Fd} ->
	    write_text(Fd, "", io:get_line(Fd, "")),
	    file:close(Fd);
	{error, _Reason} ->
	    gs:config(aboutText, {text, "Error: could not read the about file"})
    end,

    gs:config(aboutWin, [{map,true}]),
    receive
	{gs, okButton, click, _, _} ->
	    gs:destroy(aboutWin)
    end.

write_text(_Fd, Text, eof) ->
    gs:config(aboutText, {text, Text});
write_text(Fd, Text, More) ->
    write_text(Fd, lists:append(Text, More), io:get_line(Fd, "")).
