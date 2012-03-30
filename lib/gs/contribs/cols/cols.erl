%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2012. All Rights Reserved.
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
-module(cols).
-compile([{nowarn_deprecated_function,{gs,config,2}},
          {nowarn_deprecated_function,{gs,create,3}},
          {nowarn_deprecated_function,{gs,create,4}},
          {nowarn_deprecated_function,{gs,destroy,1}},
          {nowarn_deprecated_function,{gs,read,2}},
          {nowarn_deprecated_function,{gs,start,0}}]).

-export([start/0, init/0]).

%% internal export.
-export([make_board_elem/3]).

%%======================================================================
%% Contents
%%=====================
%% 1. The actual program
%% 2. Graphics
%% 3. Data structures and stuff
%% 4. Lambdas
%%======================================================================


-define(COLORS, {red,green,blue,grey,yellow,{66,153,130}}).
-define(HIGHFILE, "./cols.high").
-define(HEIGHT, 17).
-define(LEFT, 50).
-define(SIZE, 15).
-define(VERSION, "v0.9").
-define(WIDTH, 8).

-record(state, {bit,board,nextbit,ticks, score=0}).
%%----------------------------------------------------------------------
%% Consists of three boxes.
%%----------------------------------------------------------------------
-record(bit, {x,y,topColor, middleColor, bottomColor,
	      top_gsobj,mid_gsobj,bot_gsobj}).

%%======================================================================
%% 1. The actual program
%%======================================================================

start() ->
    spawn_link(cols,init,[]).

init() ->
    make_graphics(),
    {A,B,C} = erlang:now(),
    random:seed(A,B,C),
    NextBit = make_bit(),
    Board = make_screen_board(),
    S = #state{bit=make_bit(), board=Board, ticks=update_timer(1),
	       score=make_score(), nextbit=new_bit_xy(NextBit, -2,5)},
    gs:config(win, [{map, true}]),
    loop(S).

make_graphics() ->
    G = gs:start(),
    H = ?HEIGHT*?SIZE,
    W = ?WIDTH*?SIZE,
    BotMargin = 100,
    gs:create(window, win, G, [{destroy,true},{map, true},{title, "cols"},
			       {height, H+BotMargin}, {width, W+?LEFT+10},
			       {bg, grey},{keypress,true}]),
    gs:create(canvas, can, win, [{bg, black},
				 {height, H+BotMargin},
				 {width, W+?LEFT+20}]),
    gs:create(text, can, [{text, "Next"}, {coords, [{5, 45}]}, {fg, red}]),
    gs:create(image, help, can, [{coords,[{5,7}]},
				 {load_gif, dir() ++ "/help.gif"},
				 {buttonpress,true}]),
    draw_borders().

loop(State) ->
    receive
	Event -> loop(update(Event, State))
    end.

%%----------------------------------------------------------------------
%% How fast speed should be doubled
%%----------------------------------------------------------------------
-define(DBL_TICKS, 300).

update_timer(Ticks) ->
    K = 0.001/?DBL_TICKS,
    M = 1.001-K,
    Q = K*Ticks+M,
    Timeout = round(1/math:log(Q)),
    timer:send_after(Timeout, self(), fall_timeout),
    Ticks+1.

add_score({ScoreObj, NScore}, DScore) ->
    NScore2 = NScore + DScore,
    gs:config(ScoreObj, [{text, io_lib:format("Score: ~w", [NScore2])}]),
    {ScoreObj, NScore2}.
    

update({gs,_Obj,keypress,_Data, ['Left'|_]}, State) ->
    #state{bit=Bit, board = Board} = State,
    #bit{x=X,y=Y} = Bit,
    if X > 0 -> 
	    case is_board_empty(Board, X-1,Y) of
		true ->
		    State#state{bit=new_bit_xy(Bit, X-1, Y)};
		false ->
		    State
	    end;
	true -> State
    end;

update({gs,_Obj,keypress,_Data, ['Right'|_]}, State) ->
    #state{bit=Bit, board = Board} = State,
    #bit{x=X,y=Y} = Bit,
    if X < ?WIDTH - 1 ->
	    case is_board_empty(Board, X+1, Y) of
		true ->
		    State#state{bit=new_bit_xy(Bit, X+1, Y)};
		false ->
		    State
	    end;
	true -> State
    end;

update({gs,_Obj,keypress,_Data, ['Up'|_]}, State) ->
    State#state{bit=shift_bits(State#state.bit)};    

update({gs,_Obj,keypress,_Data, [Key|_]}, State) ->
    case drop_key(Key) of
	true ->
	    #state{bit=Bit, board=Board, score=Score} = State,
	    #bit{x=X,y=Y} = Bit,
	    {NewX, NewY, NewScore} = drop(X,Y,Score,Board),
	    fasten_bit(State#state{bit=new_bit_xy(Bit,NewX, NewY),
				   score=NewScore});
	false -> State
    end;
	
update(fall_timeout, State) ->
    #state{bit=Bit, board=Board, ticks = Ticks, score=Score} = State,
    NewY = Bit#bit.y+1,
    X = Bit#bit.x,
    case is_fall_ok(Board, X, NewY) of
	true ->
	    State#state{bit=new_bit_xy(Bit, X, NewY),
			ticks=update_timer(Ticks), score=add_score(Score, 1)};
	false ->
	    S1 = fasten_bit(State),
	    S1#state{ticks=update_timer(Ticks)}
    end;

update({gs,_,destroy,_,_}, _State) ->
    exit(normal);

update({gs,help,buttonpress,_,_}, State) ->
    show_help(),
    State;

update(OtherEvent, State) ->
    ok=io:format("got other! ~w~n", [OtherEvent]), State.
 
drop_key('Down') -> true;
drop_key(space) -> true;
drop_key(_) -> false.

is_board_empty(Board, X, Y) ->
    case {color_at(Board, X, Y),
	  color_at(Board, X, Y + 1),
	  color_at(Board, X, Y + 2)} of
	{black, black, black} -> true;
	_ -> false
    end.

%%----------------------------------------------------------------------
%% Returns: NewState
%%----------------------------------------------------------------------
fasten_bit(State) ->
    #state{board=Board, bit=Bit, nextbit=NextBit, score=Score} = State,
    #bit{x=X,y=Y,topColor=C1,middleColor=C2,bottomColor=C3} = Bit,
    B1 = update_screen_element(Board, X, Y, C1),
    B2 = update_screen_element(B1, X, Y+1, C2),
    B3 = update_screen_element(B2, X, Y+2, C3),
    destroy_bit(Bit),
    #bit{topColor=NC1,middleColor=NC2,bottomColor=NC3} = NextBit,
    {B4, ExtraScore} = erase_bits(B3, [{X,Y},{X,Y+1},{X,Y+2}], 0),
    NewBit = make_bit(NC1,NC2,NC3),
    case is_board_empty(B4, NewBit#bit.x, NewBit#bit.y) of
	true ->
	    State#state{score=add_score(Score, ExtraScore),
			bit=NewBit, nextbit=new_colors(NextBit),board=B4};
	false ->
	    {_GsObj,Score2}=State#state.score,
	    highscore:run(Score2,?HIGHFILE),
	    exit(normal)
    end.

%%----------------------------------------------------------------------
%% Args: Check: list of {X,Y} to check.
%% Returns: {NewBoard, ExtraScore}
%%----------------------------------------------------------------------
erase_bits(Board, Checks, ExtraScore) ->
    ElemsToDelete = elems2del(Checks,Board,[]),
    NDel = length(ElemsToDelete),
    if
	NDel > 0 ->
	    Board2 = delete_elems(Board, ElemsToDelete),
	    {NewBoard, NewCheck} = fall_down(Board2, ElemsToDelete),
	    if NDel > 3 -> 
		    {B,ES}=erase_bits(NewBoard,NewCheck,ExtraScore+2*NDel),
		    {NewBoard2, NewCheck2} = bonus(B, NewCheck),
		    erase_bits(NewBoard2, NewCheck2, ES);
		true -> 
		    erase_bits(NewBoard, NewCheck, 2*NDel)
	    end;
	true -> {Board, ExtraScore}
    end.

bonus(Board, Check) ->
    Cols = collect_bottom_bits(0,Board),
    NewBoard = randomize_columns(5, Board, Cols),
    NewCheck = update_check(Check, Cols),
    {NewBoard, NewCheck}.

randomize_columns(0, Board, _) -> Board;
randomize_columns(N, Board, Cols) ->
    NewBoard = randomize_columns(Cols,Board),
    randomize_columns(N-1, NewBoard, Cols).
			
randomize_columns([],Board) -> Board; 
randomize_columns([X|Xs],Board) ->
    flush(),
    timer:sleep(50),
    randomize_columns(Xs,update_screen_element(Board,X,?HEIGHT-1,rndColor())).

%%----------------------------------------------------------------------
%% Returns: NewBoard
%%----------------------------------------------------------------------
delete_elems(Board, Elems2Del) ->
    OrgObjs = org_objs(Elems2Del,Board),
    visual_effect(?SIZE, OrgObjs),
    NewBoard = update_board(Elems2Del, Board),
    put_back(OrgObjs),
    NewBoard.

visual_effect(0,_OrgObjs) -> done;
visual_effect(Size,OrgObjs) ->
    set_size(OrgObjs,Size),
    flush(),
    timer:sleep(20),
    visual_effect(Size-1,OrgObjs).

set_size([],_Size) -> done;
set_size([{GsObj,[{X1,Y1},{_X2,_Y2}]}|T],Size) ->
    gs:config(GsObj, [{coords, [{X1,Y1},{X1+Size,Y1+Size}]}]),
    set_size(T,Size).

%%----------------------------------------------------------------------
%% Note: Loop over columns where something is removed only. (efficiency)
%% Returns: {ReversedNewColumns (perhaps shorter), Checks}
%% cols:fall_column([a,b,black,black,c,f,black,d,black], 3, 15, [], []).
%% should return: {[a,b,c,f,d],[{3,11},{3,12},{3,13}]}
%%----------------------------------------------------------------------
fall_column([], _X, _Y, ColumnAcc, ChecksAcc) ->
    {ColumnAcc, ChecksAcc};
fall_column([black|Colors], X, Y, ColumnAcc, ChecksAcc) ->
    case find_box(Colors) of
	false -> {ColumnAcc, ChecksAcc};
	NewColors when is_list(NewColors) ->
		fall_one_step(NewColors, X, Y, ColumnAcc, ChecksAcc)
    end;
fall_column([Color|Colors], X, Y, ColumnAcc, ChecksAcc) ->
    fall_column(Colors, X, Y-1, [Color | ColumnAcc], ChecksAcc).

find_box([]) -> false;
find_box([black|Colors]) ->
    find_box(Colors);
find_box([Color|Colors]) -> [Color|Colors].
    
%%----------------------------------------------------------------------
%% Enters: ([a,b, , ,c,d], 3, 8, Q) 
%% Leaves: ([b,a|Q], [ , , ,c,d], 10, [{3,8},{4,9}])
%%----------------------------------------------------------------------
fall_one_step([], X, Y, ColumnAcc, Checks) ->
    fall_column([], X, Y, ColumnAcc, Checks);
fall_one_step([black|Colors], X, Y, ColumnAcc, Checks) ->
    fall_column([black|Colors], X, Y, ColumnAcc, Checks);
fall_one_step([Color|Colors], X, Y, ColumnAcc, Checks) ->
    fall_one_step(Colors, X, Y-1, [Color|ColumnAcc],[{X,Y}|Checks]).

%%----------------------------------------------------------------------
%% Returns: {NewBoard, NewChecks}
%%----------------------------------------------------------------------
fall_down(Board1, Elems2Del) ->
    UpDatedCols = updated_cols(Elems2Del, []),
    fall_column(UpDatedCols, Board1, []).

fall_column([], NewBoard, NewChecks) -> {NewBoard, NewChecks};
fall_column([X|Xs], BoardAcc, ChecksAcc) ->
    OrgColumn = boardcolumn_to_tuple(BoardAcc, X),
    Column = columntuple_to_list(OrgColumn),
    {NewColumn, NewChecksAcc} = fall_column(Column, X,?HEIGHT-1,[],ChecksAcc),
    NewBoardAcc =
	set_board_column(BoardAcc,X,new_column_list(NewColumn,OrgColumn)),
    fall_column(Xs,NewBoardAcc,NewChecksAcc).

new_column_list(NewColumn, ColumnTuple) ->
    Nempty = ?HEIGHT - length(NewColumn),
    L = make_list(black, Nempty) ++ NewColumn,
    new_column_list(L, 1, ColumnTuple).

new_column_list([H|T], N, Tuple) ->
    {GsObj, Color} = element(N, Tuple),
    [update_screen_element({GsObj, Color},H) | new_column_list(T, N+1, Tuple)];
new_column_list([], _, _) -> [].
    

%%----------------------------------------------------------------------
%% Returns: a reversed list of colors.
%%----------------------------------------------------------------------
columntuple_to_list(ColumnTuple) when is_tuple(ColumnTuple) ->
    columntuple_to_list(tuple_to_list(ColumnTuple),[]).

columntuple_to_list([],Acc) -> Acc;
columntuple_to_list([{_GsObj, Color}|T],Acc) ->
    columntuple_to_list(T,[Color|Acc]).

%%======================================================================
%% 2. Graphics
%%======================================================================

make_bit() ->
    make_bit(rndColor(),rndColor(),rndColor()).

make_bit(Tc,Mc,Bc) ->
    X = ?WIDTH div 2,
    Y = 0,
    #bit{x=X,y=Y,topColor= Tc, middleColor=Mc, bottomColor=Bc,
	top_gsobj = make_box(X,Y,Tc), mid_gsobj=make_box(X,Y+1,Mc),
	bot_gsobj=make_box(X,Y+2,Bc)}.
 
new_colors(Bit) ->
    #bit{top_gsobj=T,mid_gsobj=M,bot_gsobj=B} = Bit,
    Tc = rndColor(),
    Mc = rndColor(),
    Bc = rndColor(),
    gs:config(T, [{fill, Tc}]),
    gs:config(M, [{fill, Mc}]),
    gs:config(B, [{fill, Bc}]),
    Bit#bit{topColor= Tc, middleColor=Mc, bottomColor=Bc}.

new_bit_xy(Bit, NewX, NewY) ->
    #bit{x=X,y=Y,top_gsobj=T,mid_gsobj=M,bot_gsobj=B} = Bit,
    Dx = (NewX - X) * ?SIZE,
    Dy = (NewY - Y) * ?SIZE,
    gs:config(T, [{move, {Dx, Dy}}]),
    gs:config(M, [{move, {Dx, Dy}}]),
    gs:config(B, [{move, {Dx, Dy}}]),
    Bit#bit{x=NewX, y=NewY}.

destroy_bit(#bit{top_gsobj=T,mid_gsobj=M,bot_gsobj=B}) ->
    gs:destroy(T),
    gs:destroy(M),
    gs:destroy(B).

shift_bits(Bit) ->
    #bit{topColor=C1,middleColor=C2,bottomColor=C3,
	 top_gsobj=T,mid_gsobj=M,bot_gsobj=B} = Bit,
    gs:config(T, {fill,C2}),
    gs:config(M, {fill,C3}),
    gs:config(B, {fill,C1}),
    Bit#bit{topColor=C2, middleColor=C3, bottomColor=C1}.

rndColor() ->
    Siz = size(?COLORS),
    element(random:uniform(Siz), ?COLORS).

make_score() ->
    {gs:create(text, can, [{text, "Score: 0"}, {fg, red},
			   {coords, [{5,?HEIGHT*?SIZE+10}]}]), 0}.

make_screen_board() ->
    xy_loop({cols,make_board_elem}, make_board(), ?WIDTH, ?HEIGHT).

make_board_elem(X,Y,Board) ->
    set_board_element(Board,X,Y,{make_box(X,Y,black),black}).

flush() -> gs:read(can, bg).

draw_borders() ->
    BotY = ?HEIGHT*?SIZE,
    RightX = ?LEFT + ?SIZE*?WIDTH,
    LeftX = ?LEFT - 1,
    gs:create(line,can,[{coords,[{LeftX,0},{LeftX,BotY}]},{fg,white}]),
    gs:create(line,can,[{coords,[{LeftX,BotY},{RightX,BotY}]},{fg,white}]),
    gs:create(line,can,[{coords,[{RightX,0},{RightX, BotY}]}, {fg,white}]).

update_screen_element(ScrBoard, X, Y, Color) ->
    case board_element(ScrBoard,X,Y) of
	{_GsObj, Color} ->
	    ScrBoard; % don't have to update screen
	{GsObj, _ScreenColor} ->
	    gs:config(GsObj, color_args(Color)),
	    set_board_element(ScrBoard, X, Y, {GsObj, Color})
    end.

update_screen_element(ScrElem, Color) ->
    case ScrElem of
	{_GsObj, Color} ->
	    ScrElem; % don't have to update screen
	{GsObj, _ScreenColor} ->
	    gs:config(GsObj, color_args(Color)),
	    {GsObj, Color}
    end.
    

color_args(black) -> [{fg,black},{fill,black}];
color_args(Color) -> [{fg,white},{fill,Color}].

%%======================================================================
%% 3. Data structures and stuff
%%======================================================================

xy_loop(Fun, Acc,  XMax, YMax) ->
    xy_loop(Fun, Acc, 0, 0, XMax, YMax).

xy_loop(_Fun, Acc, _X, YMax, _XMax, YMax) -> Acc;
xy_loop(Fun, Acc, XMax, Y, XMax, YMax) ->
    xy_loop(Fun, Acc, 0, Y+1, XMax, YMax);
xy_loop(Fun, Acc, X, Y, XMax, YMax) ->
    xy_loop(Fun, apply(Fun, [X, Y,Acc]), X+1,Y,XMax, YMax).

%%----------------------------------------------------------------------
%% Returns: a sorted list of {X,Y} to delete.
%% Pre: PrevDelElems is sorted.
%%----------------------------------------------------------------------
erase_bits_at(Board, PrevDelElems, X,Y) ->
    C = color_at(Board, X, Y),
    erase_bits_at([vert, horiz, slash, backslash],X,Y,C,Board,PrevDelElems).
    
erase_bits_at([], _X,_Y,_C,_Board, Elems2Del) -> Elems2Del;
erase_bits_at([Dir|Ds],X,Y,C,Board, Elems2DelAcc) ->
    Dx = dx(Dir),
    Dy = dy(Dir),
    DelElems = lists:append(check_dir(Board, X-Dx,Y-Dy,-Dx,-Dy,C),
			    check_dir(Board, X,Y,Dx,Dy,C)),
    N_in_a_row = length(DelElems),
    if N_in_a_row >= 3 ->
	    erase_bits_at(Ds,X,Y,C,Board,
			  ordsets:union(lists:sort(DelElems),Elems2DelAcc));
       true -> erase_bits_at(Ds,X,Y,C,Board,Elems2DelAcc)
    end.

dx(vert) -> 0;
dx(horiz) -> 1;
dx(slash) -> 1;
dx(backslash) -> -1.

dy(vert) -> -1;
dy(horiz) -> 0;
dy(slash) -> -1;
dy(backslash) -> -1.


%%----------------------------------------------------------------------
%% Returns: list of {X,Y} to delete.
%%----------------------------------------------------------------------
check_dir(Board, X, Y, Dx, Dy, Color) 
  when X >= 0, X < ?WIDTH, Y >= 0, Y < ?HEIGHT ->
    case color_at(Board, X, Y) of
	Color ->
	    [{X,Y} | check_dir(Board, X+Dx, Y+Dy, Dx, Dy, Color)];
	_OtherColor ->
            []
    end;
check_dir(_Board, _X, _Y, _Dx, _Dy, _Color) -> [].
  
make_box(X, Y, Color) ->
    make_box(X, Y, 1, 1, Color).

%%----------------------------------------------------------------------
%% Returns: GsObj
%%----------------------------------------------------------------------
make_box(X, Y, Height, Width, Color) ->
    Opts = if Color == black -> [{fg, black}, {fill, black}];
	       true -> [{fill, Color}, {fg, white}] end,
    gs:create(rectangle, can, [{coords, [{?LEFT + X * ?SIZE, Y * ?SIZE},
					 {?LEFT + X * ?SIZE + (?SIZE*Width)-1,
					 Y * ?SIZE + (?SIZE*Height)-1}]}|Opts]).

is_fall_ok(_Board, _NewX, NewY) when NewY+2 >= ?HEIGHT -> false;
is_fall_ok(Board, NewX, NewY) ->
    case color_at(Board, NewX, NewY+2) of
	black ->
	    true;
	_ -> false
    end.

color_at(Board, X, Y) ->
    {_GsObj, Color} = board_element(Board, X, Y),
    Color.


%%----------------------------------------------------------------------
%% X:0..?WIDTH-1, Y:0..?HEIGHT
%%----------------------------------------------------------------------
make_board() ->
    list_to_tuple(make_list(make_column(), ?WIDTH)).

board_element(Board, X, Y) ->
    element(Y+1, element(X+1, Board)).

set_board_element(Board, X, Y, NewValue) ->
    Col = element(X+1, Board),
    NewCol=setelement(Y+1,Col, NewValue),
    setelement(X+1, Board, NewCol).

make_column() ->
    list_to_tuple(make_list(black, ?HEIGHT)).

make_list(_Elem, 0) -> [];
make_list(Elem, N) -> [Elem|make_list(Elem,N-1)].

boardcolumn_to_tuple(Board, X) ->
    element(X+1, Board).

set_board_column(Board, X, NewCol) when length(NewCol) == ?HEIGHT ->
    setelement(X+1, Board, list_to_tuple(NewCol)).

show_help() ->
    W = gs:create(window, win, [{title, "cols Help"}, {width, 300},
				{height,300}, {map, true}]),
    gs:create(label, W, [{x,0},{y,0},{height, 200},{width,300},{justify,center},
			 {label, {text, 
    "cols $Revision: 1.23 $" 
    "\nby\n"
    "Klas Eriksson, eklas@erlang.ericsson.se\n\n"
    "Help: Use arrows and space keys.\n"
    "      Try to get 3 in-a-row.\n"
    "      More than 3 gives bonus."}}]),
    B=gs:create(button, W, [{x,100},{y,250}, {label, {text, "Dismiss"}}]),
    receive
	{gs, B, click, _, _} -> ok
    end,
    gs:destroy(W).

%%======================================================================
%% 4. Lambdas
%%======================================================================

drop(X,Y,Score,Board) ->
    case is_fall_ok(Board, X, Y+1) of
	true -> drop(X,Y+1,add_score(Score, 1),Board);
	false -> {X,Y, Score}
    end.

elems2del([], _Board,Elems2DelAcc) -> Elems2DelAcc;
elems2del([{X,Y}|Checks],Board,Elems2DelAcc) ->
    NewElems2DelAcc = ordsets:union(erase_bits_at(Board,Elems2DelAcc,X,Y),
				    Elems2DelAcc),
    elems2del(Checks,Board,NewElems2DelAcc).

collect_bottom_bits(?WIDTH,_Board) -> [];
collect_bottom_bits(X,Board) ->
    case color_at(Board, X, ?HEIGHT-1) of
	black -> collect_bottom_bits(X+1,Board);
	_AcolorHere -> [X|collect_bottom_bits(X+1,Board)]
    end.

update_check(_Check,[]) -> [];
update_check(Check,[X|Xs]) ->
    case lists:member({X, ?HEIGHT-1}, Check) of
	true -> update_check(Check,Xs);
	false -> [{X, ?HEIGHT-1}|update_check(Check,Xs)]
    end.

org_objs([],_Board) -> [];
org_objs([{X,Y}|XYs],Board) ->
    {GsObj, _Color} = board_element(Board, X, Y),
    [{GsObj, lists:sort(gs:read(GsObj, coords))}|org_objs(XYs,Board)].

update_board([],Board) -> Board;
update_board([{X,Y}|XYs], Board) ->
    update_board(XYs,update_screen_element(Board, X, Y, black)).

put_back([]) -> done;
put_back([{GsObj, Coords}|Objs]) ->
    gs:config(GsObj, [{coords, Coords}]),
    put_back(Objs).

updated_cols([], UpdColsAcc) -> UpdColsAcc;
updated_cols([{X,_Y}|XYs], UpdColsAcc) ->
    case lists:member(X,UpdColsAcc) of
	true -> updated_cols(XYs,UpdColsAcc);
	false -> updated_cols(XYs,[X|UpdColsAcc])
    end.

%% This is not an application so we don't have their way of knowing
%% a private data directory where the GIF files are located (this directory).
%% We can find GS and makes it relative from there /kgb

-define(EbinFromGsPriv,"../contribs/ebin").

dir()->
    GsPrivDir = code:priv_dir(gs),
    filename:join(GsPrivDir,?EbinFromGsPriv).
