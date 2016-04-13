%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2009-2016. All Rights Reserved.
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
%%%-------------------------------------------------------------------
%%% File    : sud_board.erl
%%% Author  :  <dgud@erix.ericsson.se>
%%% Description : Manages the gui board
%%%
%%% Created :  9 Jan 2008 by  <dgud@erix.ericsson.se>
%%%-------------------------------------------------------------------
-module(sudoku_board).

-export([new/1, setup_board/2, clear_board/1, left/1,
	 get_board_data/1,set_board_data/2, 
	 set_butt/3, butt_correct/3,
	 get_state/1, redraw/3,
	 %% Callbacks
	 init/1, handle_sync_event/3, 
	 handle_event/2, handle_info/2, handle_call/3, handle_cast/2,
	 code_change/3, terminate/2]).

-include("sudoku.hrl").

-record(state, {win, parent, board=[], pen, fonts=[]}).
-record(sq, {key,val,correct=true,given=false}).
-define(BRD,10).
-define(ARC_R, 10).
    
-behaviour(wx_object).

%% API 
new(ParentObj) ->
    wx_object:start_link(?MODULE, [ParentObj, self()], []).

setup_board(Board, Init) ->
    wx_object:call(Board, {setup_board, Init}).

clear_board(Board) ->
    wx_object:call(Board, clear_board).

butt_correct(Board, Key, Correct) ->
    wx_object:call(Board, {butt_correct, Key, Correct}).

set_butt(Board, Indx, Val) when is_integer(Indx) ->
    {R,C,_} = sudoku_game:rcm(Indx),
    set_butt(Board, {R,C}, Val);
set_butt(Board, Id, Val) ->
    wx_object:call(Board, {set_butt, Id, Val}).

left(Board) ->
    wx_object:call(Board, left).

get_board_data(Board) ->
    wx_object:call(Board, get_board_data).
set_board_data(Board, List) ->
    wx_object:call(Board, {set_board_data, List}).

get_state(Board) ->
    wx_object:call(Board, get_state).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([ParentObj, ParentPid]) ->
    Win = wxPanel:new(ParentObj, [{style, ?wxFULL_REPAINT_ON_RESIZE}]),
    wxWindow:setFocus(Win), %% Get keyboard focus
    wxWindow:setSizeHints(Win, {250,250}),
    wxWindow:connect(Win, paint,  [callback]),
    wxWindow:connect(Win, size,  []),
    wxWindow:connect(Win, erase_background, []),
    wxWindow:connect(Win, key_up, [{skip, true}]),
    wxWindow:connect(Win, left_down, [{skip, true}]),
    wxWindow:connect(Win, enter_window, [{skip, true}]),

    %% Init pens and fonts
    Pen = wxPen:new({0,0,0}, [{width, 3}]),
    Fs0  = [{Sz,wxFont:new(Sz, ?wxSWISS, ?wxNORMAL, ?wxNORMAL,[])} ||
	       Sz <- [8,9,10,11,12,13,14,16,18,20,22,24,26,28,30,34,38,42,44,46]],
    TestDC  = wxMemoryDC:new(),
    Bitmap = wxBitmap:new(256,256),
    wxMemoryDC:selectObject(TestDC, Bitmap),
    true = wxDC:isOk(TestDC),
    CW = fun({Sz,Font},Acc) ->
		 case wxFont:ok(Font) of
		     true ->
			 wxDC:setFont(TestDC, Font),
			 CH = wxDC:getCharHeight(TestDC),
			 [{CH,Sz,Font} | Acc];
		     false ->
			 Acc
		 end
	 end,
    Fs = lists:foldl(CW, [], Fs0),
    wxMemoryDC:destroy(TestDC),
    {Win, #state{win=Win, board=[], pen=Pen, fonts=Fs,parent=ParentPid}}.

handle_sync_event(#wx{event=#wxPaint{}}, _Obj, State = #state{win=Win}) ->
    %% io:format("EPaint~n",[]),
    Size = wxWindow:getSize(Win),
    DC = wxPaintDC:new(Win),
    wxDC:destroyClippingRegion(DC),
    redraw(DC,Size,State),
    wxPaintDC:destroy(DC),
    %%io:format("...EPaint~n",[]),
    ok.

handle_event(#wx{event=#wxMouse{type=enter_window}}, State = #state{win=Win}) ->
    wxWindow:setFocus(Win), %% Get keyboard focus
    {noreply,State};
handle_event(#wx{event=#wxKey{keyCode=KeyC}},
	     S = #state{parent=Pid, win=Win}) ->
    Val = if KeyC > 47, KeyC < 58 -> KeyC - $0;
	     KeyC > 325, KeyC < 336 -> KeyC - 326; %% NUM LOCK
	     true -> 0
	  end,    
    Global = wx_misc:getMousePosition(),
    {CX,CY} = wxWindow:screenToClient(Win, Global),
    case get_butt(CX,CY,S) of
	error -> ignore;
	Id -> Pid ! {set_val,Id,Val}
    end,
    {noreply, S};
handle_event(#wx{event=#wxMouse{type=left_down,x=X,y=Y}},
	     S = #state{parent=Gui, win=F}) ->
    Id = get_butt(X,Y,S),
    case Id of
	error -> ignore;
	_ -> create_popup_menu(Gui,Id,X,Y,F)
    end,
    {noreply, S};
handle_event(#wx{event=#wxSize{}}, State) ->
    redraw(State),	    
    {noreply,State};
handle_event(_Ev, State) ->
    {noreply,State}.

%%%%%%%%%%%%%%%%%%%

handle_call({set_butt, Key, 0},_From,S0=#state{board=B0}) ->  %% Reset
    B = lists:keydelete(Key,2,B0),
    S = S0#state{board=B},
    redraw(S),
    {reply, ok, S};

handle_call({set_butt, Key, Val},_From,S0=#state{board=B0}) ->  
    case lists:keysearch(Key,2,B0) of
	{value, _} -> 
	    B = lists:keyreplace(Key, 2, B0, #sq{key=Key,val=Val});
	false ->
	    B = [#sq{key=Key, val=Val}|B0]
    end,
    S = S0#state{board=B},
    redraw(S),
    {reply, ok, S};

handle_call({butt_correct, Key, Correct},_From, S0=#state{board=B0}) ->
    case lists:keysearch(Key,2,B0) of
	{value, Butt} -> 
	    B = lists:keyreplace(Key, 2, B0, Butt#sq{key=Key,correct=Correct});
	false ->
	    B = B0	    
    end,
    S = S0#state{board=B},
    redraw(S),
    {reply, ok, S};

handle_call({setup_board, Init},_From, State) ->
    B = [#sq{given=true, correct=true, key=Key, val=Val} || {Key,Val} <- Init],
    S = State#state{board=B},
    redraw(S),
    {reply, ok, S};

handle_call(clear_board,_From, State = #state{board=B0}) ->    
    B = [Butt || Butt = #sq{given=true} <- B0],
    S = State#state{board=B},
    redraw(S),
    Given = [{Key, Val} || #sq{key=Key,val=Val,given=true} <- B],
    {reply, Given, S};
handle_call(get_board_data,_From, S=#state{board=B0}) ->    
    {reply, B0, S};
handle_call({set_board_data, B},_From, S0) ->    
    S = S0#state{board=B},
    redraw(S),
    G1 = [{Key, Val} || #sq{key=Key,val=Val,given=true} <- B],
    G2 = [{Key, Val} || #sq{key=Key,val=Val,given=false,correct=true} <- B],
    G3 = [{Key, Val} || #sq{key=Key,val=Val,given=false,correct=false} <- B],
    {reply, G1 ++ G2 ++ G3, S};
handle_call(left,_From, S = #state{board=B}) ->
    Res = 81 - length([ok || #sq{correct=C} <- B, C /= false]),
    {reply, Res, S};
handle_call(get_state, _From, S) ->
    {reply, {ok,S}, S}.

handle_cast(Msg, State) ->
    io:format("Got cast ~p~n",[Msg]),
    {noreply,State}.

code_change(_, _, State) ->
    {stop, not_yet_implemented, State}.

handle_info(Msg, State) ->
    {stop, {info, Msg}, State}.

terminate(_Reason, _State) ->
    normal.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_butt(X, Y, #state{win=Win}) ->
    {W0,H0} = wxWindow:getSize(Win),
    BoxSz = getGeomSz(W0,H0),
    %%    io:format("~p ~p ~p ~p~n", [{X,Y}, {W0,H0}, BoxSz, calc_pos(X-?BRD,Y-?BRD, BoxSz)]),
    case calc_pos(X-?BRD,Y-?BRD, BoxSz) of
	Pos = {R,C} when 0 < R, R < 10, 0 < C, C < 10 -> Pos;
	_ -> error
    end.

calc_pos(X,Y, BoxSz) ->
    {1+(Y*3 div BoxSz), 1+(X*3 div BoxSz)}.

redraw(S = #state{win=Win}) ->
    DC0  = wxClientDC:new(Win),
    DC   = wxBufferedDC:new(DC0),
    Size = wxWindow:getSize(Win),
    redraw(DC, Size, S),
    wxBufferedDC:destroy(DC),
    wxClientDC:destroy(DC0),
    ok.

redraw(DC, Size, S) ->    
    wx:batch(fun() -> 
		     wxDC:setBackground(DC, ?wxWHITE_BRUSH),
		     wxDC:clear(DC),
		     BoxSz = draw_board(DC,Size,S),
		     F = sel_font(BoxSz div 3,S#state.fonts),
		     [draw_number(DC,F,BoxSz,Sq) || Sq <- S#state.board]
	     end).

sel_font(_BS,[{_H,_Sz,F}]) ->
    %%   io:format("Font sz ~p height ~p in BS ~p~n",[_Sz,_H, _BS]),
    F;
sel_font(BS,[{H,_Sz,F}|_]) when BS > (H + 6) -> 
    %%   io:format("Font sz ~p height ~p in BS ~p~n",[_Sz,H, BS]),
    F;
sel_font(BS,[_|Fs]) ->
    sel_font(BS,Fs).

draw_number(DC,F,Sz,#sq{key={R,C},val=Num,given=Bold,correct=Correct}) ->
    {X,Y} = get_coords(Sz,R-1,C-1),
    TBox = Sz div 3,
    if Bold -> 
	    wxFont:setWeight(F,?wxBOLD),
	    wxDC:setTextForeground(DC,{0,0,0});
       Correct =:= false ->
	    wxFont:setWeight(F,?wxNORMAL),
	    wxDC:setTextForeground(DC,{255,40,40,255});
       true ->
	    wxFont:setWeight(F,?wxNORMAL),
	    wxDC:setTextForeground(DC,{50,50,100,255})
    end,
    wxDC:setFont(DC,F),
    CH = (TBox - wxDC:getCharHeight(DC)) div 2,
    CW = (TBox - wxDC:getCharWidth(DC)) div 2,
    wxDC:drawText(DC, integer_to_list(Num), {X+CW,Y+CH+1}),
    ok.

get_coords(Sz,R,C) ->
    TBox = Sz div 3,
    R1 = R div 3,
    R2 = R rem 3,
    C1 = C div 3,
    C2 = C rem 3,
    {?BRD + C1*Sz + C2*TBox,
     ?BRD + R1*Sz + R2*TBox}.

draw_board(DC,{W0,H0},#state{pen=Pen}) ->
    BoxSz = getGeomSz(W0,H0),
    BS = ?BRD+3*BoxSz,

    wxPen:setWidth(Pen, 3),
    wxPen:setColour(Pen, {0,0,0}),
    wxDC:setPen(DC,Pen),
    
    wxDC:drawRoundedRectangle(DC, {?BRD,?BRD,3*BoxSz+1,3*BoxSz+1}, 
			      float(?ARC_R)),
    %% Testing DrawLines
    wxDC:drawLines(DC, [{?BRD+BoxSz, ?BRD}, {?BRD+BoxSz, BS}]),
    wxDC:drawLine(DC, {?BRD+BoxSz*2, ?BRD}, {?BRD+BoxSz*2, BS}),
    wxDC:drawLine(DC, {?BRD, ?BRD+BoxSz}, {BS, ?BRD+BoxSz}),
    wxDC:drawLine(DC, {?BRD, ?BRD+BoxSz*2}, {BS, ?BRD+BoxSz*2}),

    %% Draw inside lines
    wxPen:setWidth(Pen, 1),
    wxDC:setPen(DC,Pen),
    TBox = BoxSz div 3,   
    wxDC:drawLine(DC, {?BRD+TBox, ?BRD}, {?BRD+TBox, BS}),
    wxDC:drawLine(DC, {?BRD+TBox*2, ?BRD}, {?BRD+TBox*2, BS}),
    wxDC:drawLine(DC, {?BRD+TBox+BoxSz, ?BRD}, {?BRD+TBox+BoxSz, BS}),
    wxDC:drawLine(DC, {?BRD+TBox*2+BoxSz, ?BRD}, {?BRD+TBox*2+BoxSz, BS}),
    wxDC:drawLine(DC, {?BRD+TBox+BoxSz*2, ?BRD}, {?BRD+TBox+BoxSz*2, BS}),
    wxDC:drawLine(DC, {?BRD+TBox*2+BoxSz*2, ?BRD}, {?BRD+TBox*2+BoxSz*2, BS}),
    %% Vert
    wxDC:drawLine(DC, {?BRD, ?BRD+TBox}, {BS, ?BRD+TBox}),
    wxDC:drawLine(DC, {?BRD, ?BRD+TBox*2}, {BS, ?BRD+TBox*2}),
    wxDC:drawLine(DC, {?BRD, ?BRD+TBox+BoxSz}, {BS, ?BRD+TBox+BoxSz}),
    wxDC:drawLine(DC, {?BRD, ?BRD+TBox*2+BoxSz}, {BS, ?BRD+TBox*2+BoxSz}),
    wxDC:drawLine(DC, {?BRD, ?BRD+TBox+BoxSz*2}, {BS, ?BRD+TBox+BoxSz*2}),
    wxDC:drawLine(DC, {?BRD, ?BRD+TBox*2+BoxSz*2}, {BS, ?BRD+TBox*2+BoxSz*2}),
    BoxSz.

getGeomSz(W,H) ->
    Small = if W < H -> W; true -> H end,
    (Small - 2*?BRD) div 3.


%% popupmenu

create_popup_menu(GFX,Butt,X,Y,Frame) ->
    Port = wx:get_env(),
    spawn_link(fun() -> create_popup_menu1(GFX,Butt,Port,X,Y,Frame) end).

create_popup_menu1(GFX,Butt,Port,X,Y,Frame) ->
    wx:set_env(Port),
    PopupMenu = wxMenu:new(),
    create_popup_menu2(1, PopupMenu),

    wxEvtHandler:connect(PopupMenu, command_menu_selected),
    wxWindow:popupMenu(Frame,PopupMenu,X,Y),
    receive 
	#wx{event=#wxCommand{type=command_menu_selected},id=10} ->
	    GFX ! {set_val,Butt,0};
	#wx{event=#wxCommand{type=command_menu_selected},id=What} ->
	    GFX ! {set_val,Butt,What}
    end.

create_popup_menu2(N,PP) when N > 9 ->
    wxMenu:append(PP, 10, "Clear");
create_popup_menu2(N,PP) ->
    wxMenu:append(PP, N,integer_to_list(N)),
    create_popup_menu2(N+1,PP).

