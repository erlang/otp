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
-module(othello_board).
-export([start/0,stop/0,init/0]).


%%----------------------------------------------------------------------
%% The Othello program now uses the gs graphical package instead of the 
%% pxw package.
%%
%% Differences are, explanation why and source of change in parenthesis:
%%
%%	-  Buttons looks different (gs feature)
%%	-  The black box around "Black to draw" have been removed. (me)
%%	-  The Colour and Level menues have been moved directly down to the 
%%	     'Status' box. (usability update, my addition)
%%	-  The mouse pointer does not change into a watch when the computer 
%%	     is thinking (not supported in gs)
%%	-  Buttons does not flash when being beeped. (not supported in gs)
%%
%%
%% /Peter
%%
%%----------------------------------------------------------------------


-define(BGCOL,forestgreen).

start() ->
    spawn(othello_board,init,[]).

stop() ->
    ok.

%% This is not an application so we don't have their way of knowing
%% a private data directory where the GIF files are located (this directory).
%% We can find GS and makes it relative from there /kgb

-define(BitmapPath,"../contribs/othello/priv").

setup_path() ->
    GsPrivDir = code:priv_dir(gs),
    Path = filename:join(GsPrivDir,?BitmapPath),
    put(path,Path).
    
path() -> get(path).



%%
%% The button area are the Quit, Rules buttons at the top of the window.
%% The Status area is the black and white scores level and colour etc 
%% inbetween the buttons and the othello board.
%% The board is the 8x8 board where othello battles are fought.
%%
init() ->
    process_flag(trap_exit,true),
    setup_path(),
    S = gs:start(),
    put(windowroot,S),				% Ugly global store

    %% Shell coordinates
    W = 496,
    H = 636,
    
    %% Fix top window
    Shell = gs:create(window, S, [{title,"Othello"}, 
				  {width, W},{height, H}]),


    %% Setup window contents

    setup_buttons(Shell,0,0,W,40),		% Fix Menubar
    setup_status_box(Shell,0,40,W,100),		% Fix Status area
    setup_board(Shell,0,140,496,496),		% Combat board

    GamePid = othello:new_game(white,black,1,first_time),

    %% Default settings
    Options = {white,black,1},
    %%Wids = {Status,B,W,Dr,Le,Co},
    Wids = {change,this,at,later,stage,ponto},
    write_options(Options,Wids),

    gs:config(Shell, {map, true}),		%Make win visible

    loop(computer,GamePid,Shell,Wids,Options).






loop(User,GamePid,Shell,Wids,Options) ->
    receive
	{gs,ButtId, click,_ButtId1,[Button]} ->
	    GamePid1 = but_pressed(Button,ButtId,User,GamePid,Shell,
				   Wids,Options),
	    loop(User,GamePid1,Shell,Wids,Options);

	{gs,_, click,_,[MenuItem,_MenuIndex]} ->
	    Ops = menu_selected(MenuItem,User,GamePid,Wids,Options),
	    loop(User,GamePid,Shell,Wids,Ops);

	{'EXIT',GamePid,_} ->
	    loop(User,null,Shell,Wids,Options);

	{'EXIT',_,_} ->
	    loop(User,GamePid,Shell,Wids,Options);

	GameMsg ->
	    game_msg(GameMsg,User,GamePid,Shell,Wids,Options)
    end.

but_pressed("Quit",_ButtId,_User,_GamePid,_Shell,_Wids,_Op) ->
    stop(),
    exit(quit);
but_pressed("Rules",_ButtId,_User,GamePid,_Shell,_Wids,_Op) ->
    io:format("No rules, do as you wish~n",[]),
    GamePid;
but_pressed("Help",_ButtId,_User,GamePid,_Shell,_Wids,_Op) ->
    io:format("Othello game~n",[]),
    io:format("------------~n",[]),
    io:format("  Put markers by clicking in squares~n",[]),
    io:format("  Change level by clicking on it~n",[]),
    io:format("  Change colour by clicking on it~n",[]),
    io:format("~n",[]),
    GamePid;
but_pressed("Newgame",_ButtId,_User,GamePid,_Shell,Wids,Options) ->
    new_game(GamePid,Wids,Options);
but_pressed([],ButtId,User,GamePid,_Shell,_Wids,_Op) 
					when pid(GamePid),User == player ->
    [C,R] = atom_to_list(ButtId),
    GamePid ! {self(),position,othello_adt:pos(C-96,translate(R-48))},
    GamePid;
but_pressed([],ButtId,_User,GamePid,_Shell,_Wids,_Op) ->
    [C,R] = atom_to_list(ButtId),
    beep(othello_adt:pos(C-96,translate(R-48))),
    GamePid;
but_pressed(Button,ButtId,_User,GamePid,_Shell,_Wids,_Op) ->
    io:format('Not implemented button pressed ~p, ~p!!!~n',[ButtId,Button]),
    GamePid.

menu_selected("Black",_User,_GamePid,Wids,Options) ->
    Op0 = setelement(1,Options,white),
    Op1 = setelement(2,Op0,white),
    write_options(Op1,Wids),
    Op1;
menu_selected("White",_User,_GamePid,Wids,Options) ->
    Op0 = setelement(1,Options,black),
    Op1 = setelement(2,Op0,black),
    write_options(Op1,Wids),
    Op1;
menu_selected("Black (begin)",_User,_GamePid,Wids,Options) ->
    Op0 = setelement(1,Options,white),
    Op1 = setelement(2,Op0,black),
    write_options(Op1,Wids),
    Op1;
menu_selected("White (begin)",_User,_GamePid,Wids,Options) ->
    Op0 = setelement(1,Options,black),
    Op1 = setelement(2,Op0,white),
    write_options(Op1,Wids),
    Op1;
menu_selected("Beginner",_User,_GamePid,Wids,Options) ->
    Op1 = setelement(3,Options,1),
    write_options(Op1,Wids),
    Op1;
menu_selected("Intermediate",_User,_GamePid,Wids,Options) ->
    Op1 = setelement(3,Options,2),
    write_options(Op1,Wids),
    Op1;
menu_selected("Advanced",_User,_GamePid,Wids,Options) ->
    Op1 = setelement(3,Options,3),
    write_options(Op1,Wids),
    Op1;
menu_selected("Expert",_User,_GamePid,Wids,Options) ->
    Op1 = setelement(3,Options,4),
    write_options(Op1,Wids),
    Op1;
menu_selected(What,_User,_GamePid,_Wids,Options) ->
    io:format('Menu item not implemented <~s>~n',[What]),
    Options.

game_msg(Msg,User,GamePid,Shell,Wids,Options) ->
    case Msg of
	{GamePid,new_mark,Pos,Colour} ->
	    new_mark(Pos,Colour),
	    loop(User,GamePid,Shell,Wids,Options);

	{GamePid,illegal_draw,Draw} ->
	    beep(Draw),
	    loop(User,GamePid,Shell,Wids,Options);

	{GamePid,player,Computer,Computer} ->
	    show_player(element(1,Wids),Computer),
	    cursor("watch"),
	    GamePid ! {self(),go_on_play},
	    loop(computer,GamePid,Shell,Wids,Options);

	{GamePid,player,_Computer,Player} ->
	    show_player(element(1,Wids),Player),
	    cursor("top_left_arrow"),
	    GamePid ! {self(),go_on_play},
	    loop(player,GamePid,Shell,Wids,Options);

	{GamePid,omit_draw,Player} ->
	    omit_draw(GamePid,Player),
	    loop(User,GamePid,Shell,Wids,Options);

	{GamePid,score,WhiteRes,BlackRes} ->
	    write_score(Wids,WhiteRes,BlackRes),
	    loop(User,GamePid,Shell,Wids,Options);

	{GamePid,draw,Draw} ->
	    write_draw(Wids,Draw),
	    loop(User,GamePid,Shell,Wids,Options);

	{GamePid,game_over,WhiteRes,BlackRes} ->
	    game_over(WhiteRes,BlackRes),
	    loop(User,GamePid,Shell,Wids,Options);

	What ->
            io:format('game_msg received: ~w~n',[What]),    
	    loop(User,GamePid,Shell,Wids,Options)
    end.
    

new_game(GamePid,Wids,Options) when pid(GamePid) ->
    exit(GamePid,kill),
    new_game(Wids,Options);
new_game(_,Wids,Options) ->
    new_game(Wids,Options).

new_game(_Wids,Options) ->
    label("",lastdraw),
    Computer = element(1,Options),
    Start = element(2,Options),
    Depth = element(3,Options),
    othello:new_game(Computer,Start,Depth,restart).

new_mark(Pos,Colour) ->
    Col = othello_adt:col(Pos),
    Row = othello_adt:row(Pos),
    Name = [Col+96,translate(Row)+48],
    Button = get(Name),
    butbit(Button,Colour).

beep(Draw) ->
    Col = othello_adt:col(Draw),
    Row = othello_adt:row(Draw),
    Name = [Col+96,translate(Row)+48],
    Button = get(Name),
    bell(Button).

show_player(_Status,white) ->
    label("White to draw",todraw);
show_player(_Status,black) ->
    label("Black to draw",todraw).

write_score(_Wids,WhiteRes,BlackRes) ->
    label(integer_to_list(BlackRes),bscore),
    label(integer_to_list(WhiteRes),wscore).

write_draw(_Wids,Draw) ->
    Col = othello_adt:col(Draw),
    Row = othello_adt:row(Draw),
    label(lists:flatten(io_lib:format('{~w,~w}',[Col,Row])), lastdraw).

write_options(Options,Wids) ->
    write_colour(Options,Wids),
    write_level(Options,Wids).

write_colour(Options,Wids) ->
    write_colour(element(1,Options),element(2,Options),Wids).

write_colour(black,white,_Wids) -> label("White (begin)",colour);
write_colour(black,black,_Wids) -> label("White",colour);
write_colour(white,black,_Wids) -> label("Black (begin)",colour);
write_colour(white,white,_Wids) -> label("Black",colour).
    
write_level(Options,_Wids) ->
    case element(3,Options) of
	1 -> label("Beginner",level);
	2 -> label("Intermediate",level);
	3 -> label("Advanced",level);
	4 -> label("Expert",level)
    end.

cursor(_What) ->
    done.
%cursor(What) -> cursor(get(),What).

%cursor([{[C,R],Button}|Buts],What) ->
%    set_widget(Button,"cursor",What),
%    cursor(Buts,What);
%cursor([_|Buts],What) ->
%    cursor(Buts,What);
%cursor([],_) ->
%    true.

translate(1) -> 8;
translate(2) -> 7;
translate(3) -> 6;
translate(4) -> 5;
translate(5) -> 4;
translate(6) -> 3;
translate(7) -> 2;
translate(8) -> 1.

bitmap(grey)  -> bitmap_path("square.bm");
bitmap(black) -> bitmap_path("marker.bm");
bitmap(white) -> bitmap_path("marker.bm").

bitmap_path(Bitmap) ->
    filename:join(path(),Bitmap).

xy_position([[Letter,Digit],_,_]) ->
    LettPos = Letter - 97,
    X = LettPos*60 ,
    Y = (8 - list_to_integer([Digit])) * 60,
    {X+6,Y+6};
xy_position(X) ->
    io:format("xy_position: ~w~n",[{error,X}]).


board() ->
    [["a1",grey,nil],
     ["b1",grey,nil],
     ["c1",grey,nil],
     ["d1",grey,nil],
     ["e1",grey,nil],
     ["f1",grey,nil],
     ["g1",grey,nil],
     ["h1",grey,nil],

     ["a2",grey,nil],
     ["b2",grey,nil],
     ["c2",grey,nil],
     ["d2",grey,nil],
     ["e2",grey,nil],
     ["f2",grey,nil],
     ["g2",grey,nil],
     ["h2",grey,nil],

     ["a3",grey,nil],
     ["b3",grey,nil],
     ["c3",grey,nil],
     ["d3",grey,nil],
     ["e3",grey,nil],
     ["f3",grey,nil],
     ["g3",grey,nil],
     ["h3",grey,nil],

     ["a4",grey,nil],
     ["b4",grey,nil],
     ["c4",grey,nil],
     ["d4",white,nil],
     ["e4",black,nil],
     ["f4",grey,nil],
     ["g4",grey,nil],
     ["h4",grey,nil],

     ["a5",grey,nil],
     ["b5",grey,nil],
     ["c5",grey,nil],
     ["d5",black,nil],
     ["e5",white,nil],
     ["f5",grey,nil],
     ["g5",grey,nil],
     ["h5",grey,nil],

     ["a6",grey,nil],
     ["b6",grey,nil],
     ["c6",grey,nil],
     ["d6",grey,nil],
     ["e6",grey,nil],
     ["f6",grey,nil],
     ["g6",grey,nil],
     ["h6",grey,nil],

     ["a7",grey,nil],
     ["b7",grey,nil],
     ["c7",grey,nil],
     ["d7",grey,nil],
     ["e7",grey,nil],
     ["f7",grey,nil],
     ["g7",grey,nil],
     ["h7",grey,nil],

     ["a8",grey,nil],
     ["b8",grey,nil],
     ["c8",grey,nil],
     ["d8",grey,nil],
     ["e8",grey,nil],
     ["f8",grey,nil],
     ["g8",grey,nil],
     ["h8",grey,nil]].


omit_draw(GamePid,Player) ->
%    %% Find mouse coords first
%    %% This was not possible in gs

    W = 200, H = 100, Root = get(windowroot),
    Box = gs:create(window, Root, [{title,"OMIT"}, {width, W},{height, H}]),
    
    mk_label_c(lists:flatten(io_lib:format('~w has to omit draw !',[Player])),
	     Box, W, 10),

    mk_button_c("Ok", Box, W, H-40, 80, 30),

    gs:config(Box, {map, true}),		%Make win visible

    receive
	{gs,_, click,_,["Ok"]} ->
	    gs:destroy(Box),
	    GamePid ! {self(),continue}
    end.

game_over(WhiteRes,BlackRes) ->
%    %% Find mouse coords first
%    %% This was not possible in gs

    W = 200, H = 160, 
    Root = get(windowroot),
    Box = gs:create(window, Root, [{title,"GAME OVER"}, 
				   {width, W},{height, H}]),
    
    mk_label_c("GAME OVER", Box, W, 10),

    mk_label_c(lists:flatten(io_lib:format('White score: ~w',[WhiteRes])),
	       Box,W,40),
    mk_label_c(lists:flatten(io_lib:format('Black score: ~w',[BlackRes])),
	       Box,W,70),

    mk_button_c("Ok", Box, W, H-40, 80, 30),

    gs:config(Box, {map, true}),		%Make win visible

    receive
	{gs,_, click,_,["Ok"]} ->
	    gs:destroy(Box)
    end.



%% ----------------------------------------------------------------
%% Library functions.
%% ----------------------------------------------------------------

bell(Widget) ->
    %% gs does not support bells,
    Widget.

label(Text,Label) ->
    gs:config(Label,[{label,{text,Text}}]).

%% mk_label in centered version
mk_label_c(Label,Parent,Width,Y) ->
    W = 8*length(Label),
    X = trunc((Width-W)/2),
    gs:create(label,Parent,[{width,W}, {height, 20}, {x,X}, {y,Y}, 
			     {label, {text, Label}}]).




setup_buttons(Shell,X,Y,W,H) ->
    ButBox = gs:create(frame, Shell,[{x,X}, {y,Y},{bg,white},
				     {width,W}, {height,H}]),
    C = gs:create(canvas,ButBox,[{x,X}, {y,Y}, {width, W}, {height, H},
				 {bg,white}]),
    gs:create(line, C, [{coords, [{0,H-1},{W,H-1}]}]),


    mk_button("Quit",ButBox, 10, 10, 70, 20),
    mk_button("Rules",ButBox, 80, 10, 70, 20),
    mk_button("Newgame",ButBox, 150, 10, 70, 20),
    mk_button("Help",ButBox, 220, 10, 70, 20),
%%    mk_button("Level",ButBox, 290, 10, 70, 20),
    
    done.

    



%%----------------------------------------
%% Sets up the middle window w. all the status info in.
%% The labels are given names:
%% bscore, wscore, lastdraw, todraw, level and colour to simplify
%% their frequent setting
%%
setup_status_box(Shell,X,Y,W,H) ->
    F = gs:create(frame, Shell,[{x,X}, {y,Y},
				  {width,W}, {height,H},{bg,white}]),
    C = gs:create(canvas,F,[{x,0}, {y,0}, {width, W}, {height, H},{bg,white}]),
    gs:create(line, C, [{coords, [{0,H-1},{W,H-1}]}]),

    %% Left side
    gs:create(label,F,[{align,w},{x,10}, {y,5}, {width, 100},
		       {label,{text, "Black score:"}},{bg,white}]),
    gs:create(label,bscore,F,[{align,w},{x,110}, {y,5}, {width, 40},
			      {label,{text, "2"}},{bg,white}]),
    gs:create(label,F,[{align,w},{x,10}, {y,35}, {width, 100},
		       {label,{text, "White score:"}},{bg,white}]),
    gs:create(label,wscore,F,[{align,w},{x,110}, {y,35}, {width, 40},
			      {label,{text, "2"}},{bg,white}]),
    gs:create(label,F,[{align,w},{x,10}, {y,65}, {width, 100},
			      {label,{text, "Last draw:"}},{bg,white}]),
    gs:create(label,lastdraw,F,[{align,w},{x,110}, {y,65}, {width, 40},
				{label,{text, ""}},{bg,white}]),
    

    %% Right side
    X2 = trunc(W/2)+10,
    gs:create(label,todraw,F,[{align,w},{x,X2}, {y,5}, {width, 100},
			      {label,{text, "Black to draw:"}},{bg,white}]),

    gs:create(label,F,[{align,w},{x,X2}, {y,35}, {width, 80},
			      {label,{text, "Level:"}},{bg,white}]),
    setup_level_menu(F,X2+80,35),

%%    gs:create(label,level,F,[{align,w},{x,X2+80}, {y,35}, {width, 130},
%%			      {label,{text, "Intermediate"}},{bg,white}]),
    gs:create(label,F,[{align,w},{x,X2}, {y,65}, {width, 80},
			      {label,{text, "Colour:"}},{bg,white}]),
    setup_col_menu(F,X2+80,65),

%%    gs:create(label,colour,F,[{align,w},{x,X2+80}, {y,65}, {width, 120},
%%			      {label,{text, "black (begin)"}},{bg,white}]),
    
    done.


setup_col_menu(P,X,Y) ->
    MB = gs:create(menubutton,colour,P,
		   [{x,X}, {y,Y}, {bw,3},
		    %%{width,W}, {height,H}, 
		    {align,w}, {bg,white},
		    {relief, raised},
		    {activefg,white}, {activebg,black},
		    {label, {text,"Colours"}}]),

    M = gs:create(menu,MB,[]),
    gs:create(menuitem,M,[{label,{text,"Black (begin)"}}]),
    gs:create(menuitem,M,[{label,{text,"Black"}}]),
    gs:create(menuitem,M,[{label,{text,"White (begin)"}}]),
    gs:create(menuitem,M,[{label,{text,"White"}}]),
    done.

setup_level_menu(P,X,Y) ->
    MB = gs:create(menubutton,level,P,
		   [{x,X}, {y,Y},
		    %%{width,W}, {height,H}, 
		    {relief, raised},
		    {activefg,white}, {activebg,black},
		    {align,w}, {bg,white},
		    {label, {text,"Colours"}}]),

    M = gs:create(menu,MB,[]),
    gs:create(menuitem,M,[{label,{text,"Beginner"}}]),
    gs:create(menuitem,M,[{label,{text,"Intermediate"}}]),
    gs:create(menuitem,M,[{label,{text,"Advanced"}}]),
    gs:create(menuitem,M,[{label,{text,"Expert"}}]),
    done.


setup_board(Shell,X,Y,W,H) ->
    F = gs:create(frame, Shell,[{x,X}, {y,Y},
				  {width,W}, {height,H},{bg,white}]),
    display_board(F).


mk_button(Label, Parent, X, Y, W, H) ->
    gs:create(button,Parent,[{width,W}, {height, H}, {x,X}, {y,Y}, 
			     {label, {text, Label}}, {bg,white},
			     {activefg,white}, {activebg,black}]).

%% Centers a button around Width
mk_button_c(Label, Parent, Width, Y, W, H) ->
    X = trunc((Width-W)/2),
    gs:create(button,Parent,[{width,W}, {height, H}, {x,X}, {y,Y}, 
			     {label, {text, Label}}, {bg,white}]).


butbit(Button,Col) ->
    gs:config(Button,[
		      {label,{image,bitmap(Col)}},
		      {fg, Col},
		      {activefg,Col},
		      {label, {image, bitmap(Col)}}]),
    Button.

mk_board_butt(Top,Name,X,Y,Col) ->
    B = gs:create(button,list_to_atom(Name), Top,
		  [{x,X}, {y,Y}, {width,60}, {height,60},
		   {padx,5},{pady,5},
		   {relief,flat},
		   {bg,?BGCOL}, {activebg,?BGCOL}]),
    butbit(B,Col),
    B.
	     


display_board(Top) ->
    Board = board(),
    display_board(Top,Board,1).

display_board(_,_,65) -> true;
display_board(Top,[H|T],Place) ->
    [Name,Colour,_] = H,
    {X,Y} = xy_position(H),
    Button = mk_board_butt(Top,Name,X,Y,Colour),
    %%Button = mk_button("",Name,Top,X,Y,60,60),
    put(Name,Button),
    %%Bitmap = bitmap(Colour),
    %%butbit(Button,Bitmap),
    %%set_widget(Button,"internalWidth","1"),
    %%set_widget(Button,"internalHeight","1"),
    %%borderWidth(2,Button),
    display_board(Top,T,Place+1).
    

