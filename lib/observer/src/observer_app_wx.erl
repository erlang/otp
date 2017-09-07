%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011-2017. All Rights Reserved.
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
-module(observer_app_wx).

-export([start_link/3]).

%% wx_object callbacks
-export([init/1, handle_info/2, terminate/2, code_change/3, handle_call/3,
	 handle_event/2, handle_sync_event/3, handle_cast/2]).

-behaviour(wx_object).
-include_lib("wx/include/wx.hrl").
-include("observer_defs.hrl").

%% Import drawing wrappers
-import(observer_perf_wx, [haveGC/0, make_gc/2, destroy_gc/1,
			   setPen/2, setFont/3, setBrush/2,
			   strokeLine/5, strokeLines/2, drawRoundedRectangle/6,
			   drawText/4, getTextExtent/2]).

-record(state,
	{
	  parent,
	  panel,
	  apps_w,
	  app_w,
	  paint,
	  current,
	  app,
	  sel,
	  appmon,
	  usegc = false
	}).

-record(paint, {font, pen, brush, sel, links}).

-record(app, {ptree, n2p, links, dim}).
-record(box, {x,y, w,h, s1}).
-record(str, {x,y,text,pid}).

-define(BX_E, 10). %% Empty width between text and box
-define(BX_HE, (?BX_E div 2)).
-define(BY_E, 10). %% Empty height between text and box
-define(BY_HE, (?BY_E div 2)).

-define(BB_X, 16). %% Empty width between boxes
-define(BB_Y, 12). %% Empty height between boxes

-define(DRAWAREA, 5).
-define(ID_PROC_INFO, 101).
-define(ID_PROC_MSG,  102).
-define(ID_PROC_KILL, 103).
-define(ID_TRACE_PID, 104).
-define(ID_TRACE_NAME, 105).
-define(ID_TRACE_TREE_PIDS, 106).
-define(ID_TRACE_TREE_NAMES, 107).

-define(wxGC, wxGraphicsContext).

start_link(Notebook, Parent, Config) ->
    wx_object:start_link(?MODULE, [Notebook, Parent, Config], []).

init([Notebook, Parent, _Config]) ->
    Panel = wxPanel:new(Notebook, [{size, wxWindow:getClientSize(Notebook)},
				   {winid, 1}
				  ]),
    Main = wxBoxSizer:new(?wxHORIZONTAL),
    Splitter = wxSplitterWindow:new(Panel, [{size, wxWindow:getClientSize(Panel)},
					    {style, ?SASH_STYLE},
					    {id, 2}
					   ]),
    Apps = wxListBox:new(Splitter, 3, []),
    %% Need extra panel and sizer to get correct size updates
    %% in draw area for some reason
    P2 = wxPanel:new(Splitter, [{winid, 4}]),
    Extra = wxBoxSizer:new(?wxVERTICAL),
    DrawingArea = wxScrolledWindow:new(P2, [{winid, ?DRAWAREA},
					    {style,?wxFULL_REPAINT_ON_RESIZE}]),
    wxWindow:setBackgroundColour(DrawingArea, ?wxWHITE),
    wxWindow:setVirtualSize(DrawingArea, 800, 800),
    wxSplitterWindow:setMinimumPaneSize(Splitter,50),
    wxSizer:add(Extra, DrawingArea, [{flag, ?wxEXPAND},{proportion, 1}]),
    wxWindow:setSizer(P2, Extra),
    wxSplitterWindow:splitVertically(Splitter, Apps, P2, [{sashPosition, 150}]),
    wxWindow:setSizer(Panel, Main),

    wxSizer:add(Main, Splitter, [{flag, ?wxEXPAND bor ?wxALL},
				 {proportion, 1}, {border, 5}]),
    wxWindow:setSizer(Panel, Main),
    wxListBox:connect(Apps, command_listbox_selected),
    wxPanel:connect(DrawingArea, paint, [callback]),
    wxPanel:connect(DrawingArea, size, [{skip, true}]),
    wxPanel:connect(DrawingArea, left_up),
    wxPanel:connect(DrawingArea, left_dclick),
    wxPanel:connect(DrawingArea, right_down),
    case os:type() of
	{win32, _} -> %% Ignore erase on windows
	    wxPanel:connect(DrawingArea, erase_background, [{callback, fun(_,_) -> ok end}]);
	_ -> ok
    end,

    UseGC = haveGC(),
    Version28 = ?wxMAJOR_VERSION =:= 2 andalso ?wxMINOR_VERSION =:= 8,
    Font = case os:type() of
	       {unix,_} when UseGC, Version28 ->
		   wxFont:new(12,?wxFONTFAMILY_DECORATIVE,?wxFONTSTYLE_NORMAL,?wxFONTWEIGHT_NORMAL);
	       _ ->
		   wxSystemSettings:getFont(?wxSYS_DEFAULT_GUI_FONT)
	   end,
    SelCol   = wxSystemSettings:getColour(?wxSYS_COLOUR_HIGHLIGHT),
    GreyBrush = wxBrush:new({230,230,240}),
    SelBrush = wxBrush:new(SelCol),
    LinkPen  = wxPen:new(SelCol, [{width, 2}]),
    process_flag(trap_exit, true),
    {Panel, #state{parent=Parent,
		   panel =Panel,
		   apps_w=Apps,
		   app_w =DrawingArea,
		   usegc = UseGC,
		   paint=#paint{font = Font,
				pen  = wxPen:new({80,80,80}, [{width, 2}]),
				brush= GreyBrush,
				sel  = SelBrush,
				links= LinkPen
			       }
		  }}.

setup_scrollbar(AppWin, App) ->
    setup_scrollbar(wxWindow:getClientSize(AppWin), AppWin, App).

setup_scrollbar({CW, CH}, AppWin, #app{dim={W0,H0}}) ->
    W = max(W0,CW),
    H = max(H0,CH),
    PPC = 20,
    if W0 =< CW, H0 =< CH ->
	    wxScrolledWindow:setScrollbars(AppWin, W, H, 0, 0);
       H0 =< CH ->
	    wxScrolledWindow:setScrollbars(AppWin, PPC, H, W div PPC+1, 0);
       W0 =< CW ->
	    wxScrolledWindow:setScrollbars(AppWin, W, PPC, 0, H div PPC+1);
       true ->
	    wxScrolledWindow:setScrollbars(AppWin, PPC, PPC, W div PPC+1, H div PPC+1)
    end;
setup_scrollbar(_, _, undefined) -> ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_event(#wx{event=#wxCommand{type=command_listbox_selected, cmdString=AppStr}},
	     State = #state{appmon=AppMon, current=Prev}) ->
    case AppStr of
	[] ->
	    {noreply, State};
	_ ->
	    App = list_to_atom(AppStr),
	    (Prev =/= undefined) andalso appmon_info:app(AppMon, Prev, false, []),
	    appmon_info:app(AppMon, App, true, []),
	    {noreply, State#state{current=App}}
    end;

handle_event(#wx{id=Id, event=_Sz=#wxSize{size=Size}},
	     State=#state{app=App, app_w=AppWin}) ->
    Id =:= ?DRAWAREA andalso setup_scrollbar(Size,AppWin,App),
    {noreply, State};

handle_event(#wx{event=#wxMouse{type=Type, x=X0, y=Y0}},
	     S0=#state{app=App, app_w=AppWin}) ->
    case App of
	#app{ptree=Tree} ->
	    {X,Y} = wxScrolledWindow:calcUnscrolledPosition(AppWin, X0, Y0),
	    Hit   = locate_node(X,Y, [Tree]),
	    State = handle_mouse_click(Hit, Type, S0),
	    {noreply, State};
	_ ->
	    {noreply, S0}
    end;

handle_event(#wx{event=#wxCommand{type=command_menu_selected}},
	     State = #state{panel=Panel,sel=undefined}) ->
    observer_lib:display_info_dialog(Panel,"Select process first"),
    {noreply, State};

handle_event(#wx{id=?ID_PROC_INFO, event=#wxCommand{type=command_menu_selected}},
	     State = #state{sel={#box{s1=#str{pid=Pid}},_}}) ->
    observer ! {open_link, Pid},
    {noreply, State};

handle_event(#wx{id=?ID_PROC_MSG, event=#wxCommand{type=command_menu_selected}},
	     State = #state{panel=Panel, sel={#box{s1=#str{pid=Pid}},_}}) ->
    case observer_lib:user_term(Panel, "Enter message", "") of
	cancel ->         ok;
	{ok, Term} ->     Pid ! Term;
	{error, Error} -> observer_lib:display_info_dialog(Panel,Error)
    end,
    {noreply, State};

handle_event(#wx{id=?ID_PROC_KILL, event=#wxCommand{type=command_menu_selected}},
	     State = #state{panel=Panel, sel={#box{s1=#str{pid=Pid}},_}}) ->
    case observer_lib:user_term(Panel, "Enter Exit Reason", "kill") of
	cancel ->         ok;
	{ok, Term} ->     exit(Pid, Term);
	{error, Error} -> observer_lib:display_info_dialog(Panel,Error)
    end,
    {noreply, State};

%%% Trace api
handle_event(#wx{id=?ID_TRACE_PID, event=#wxCommand{type=command_menu_selected}},
	     State = #state{sel={Box,_}}) ->
    observer_trace_wx:add_processes([box_to_pid(Box)]),
    {noreply, State};
handle_event(#wx{id=?ID_TRACE_NAME, event=#wxCommand{type=command_menu_selected}},
	     State = #state{sel={Box,_}}) ->
    observer_trace_wx:add_processes([box_to_reg(Box)]),
    {noreply, State};
handle_event(#wx{id=?ID_TRACE_TREE_PIDS, event=#wxCommand{type=command_menu_selected}},
	     State = #state{sel=Sel}) ->
    Get = fun(Box) -> box_to_pid(Box) end,
    observer_trace_wx:add_processes(tree_map(Sel, Get)),
    {noreply, State};
handle_event(#wx{id=?ID_TRACE_TREE_NAMES, event=#wxCommand{type=command_menu_selected}},
	     State = #state{sel=Sel}) ->
    Get = fun(Box) -> box_to_reg(Box) end,
    observer_trace_wx:add_processes(tree_map(Sel, Get)),
    {noreply, State};

handle_event(Event, _State) ->
    error({unhandled_event, Event}).

%%%%%%%%%%
handle_sync_event(#wx{event = #wxPaint{}},_,
		  #state{app_w=DA, app=App, sel=Sel, paint=Paint, usegc=UseGC}) ->
    GC = {GC0, DC} = make_gc(DA, UseGC),
    case UseGC of
	false ->
	    wxScrolledWindow:doPrepareDC(DA,DC);
	true ->
	    %% Argh must handle scrolling when using ?wxGC
	    {Sx,Sy} = wxScrolledWindow:calcScrolledPosition(DA, {0,0}),
	    ?wxGC:translate(GC0, Sx,Sy)
    end,
    %% Nothing is drawn until wxPaintDC is destroyed.
    draw(GC, App, Sel, Paint),
    destroy_gc(GC),
    ok.
%%%%%%%%%%
handle_call(get_config, _, State) ->
    {reply, #{}, State};
handle_call(Event, From, _State) ->
    error({unhandled_call, Event, From}).

handle_cast(Event, _State) ->
    error({unhandled_cast, Event}).
%%%%%%%%%%
handle_info({active, Node}, State = #state{parent=Parent, current=Curr, appmon=Appmon}) ->
    create_menus(Parent, []),
    Pid = try
	      Node = node(Appmon),
	      Appmon
	  catch _:_ ->
		  {ok, P} = appmon_info:start_link(Node, self(), []),
		  P
	  end,
    appmon_info:app_ctrl(Pid, Node, true, []),
    (Curr =/= undefined) andalso appmon_info:app(Pid, Curr, true, []),
    {noreply, State#state{appmon=Pid}};
handle_info(not_active, State = #state{appmon=AppMon}) ->
    appmon_info:app_ctrl(AppMon, node(AppMon), false, []),
    lists:member(node(AppMon), nodes()) andalso exit(AppMon, normal),
    observer_wx:set_status(""),
    {noreply, State#state{appmon=undefined}};
handle_info({delivery, Pid, app_ctrl, _, Apps0},
	    State = #state{appmon=Pid, apps_w=LBox, current=Curr0}) ->
    Apps = [atom_to_list(App) || {_, App, {_, _, _}} <- Apps0],
    wxListBox:clear(LBox),
    wxListBox:appendStrings(LBox, [App || App <- lists:sort(Apps)]),
    case Apps of
	[App|_] when Curr0 =:= undefined ->
	    Curr = list_to_atom(App),
	    appmon_info:app(Pid, Curr, true, []),
	    {noreply, State#state{current=Curr}};
	_ ->
	    {noreply, State}
    end;
handle_info({delivery, _Pid, app, _Curr, {[], [], [], []}},
	    State = #state{panel=Panel}) ->
    wxWindow:refresh(Panel),
    {noreply, State#state{app=undefined, sel=undefined}};

handle_info({delivery, Pid, app, Curr, AppData},
	    State = #state{panel=Panel, appmon=Pid, current=Curr, usegc=UseGC,
			   app_w=AppWin, paint=#paint{font=Font}}) ->
    GC = if UseGC -> {?wxGC:create(AppWin), false};
	    true ->  {false, wxWindowDC:new(AppWin)}
	 end,
    setFont(GC, Font, {0,0,0}),
    App = build_tree(AppData, GC),
    destroy_gc(GC),
    setup_scrollbar(AppWin, App),
    wxWindow:refresh(Panel),
    wxWindow:layout(Panel),
    {noreply, State#state{app=App, sel=undefined}};

handle_info({'EXIT', _, noconnection}, State) ->
    {noreply, State};
handle_info({'EXIT', _, normal}, State) ->
    {noreply, State};
handle_info(_Event, State) ->
    %% io:format("~p:~p: ~tp~n",[?MODULE,?LINE,_Event]),
    {noreply, State}.

%%%%%%%%%%
terminate(_Event, _State) ->
    ok.
code_change(_, _, State) ->
    State.

handle_mouse_click(Node = {#box{s1=#str{pid=Pid}},_}, Type,
		   State=#state{app_w=AppWin,panel=Panel}) ->
    case Type of
	left_dclick -> observer ! {open_link, Pid};
	right_down  -> popup_menu(Panel);
	_ ->           ok
    end,
    observer_wx:set_status(io_lib:format("Pid: ~p", [Pid])),
    wxWindow:refresh(AppWin),
    State#state{sel=Node};
handle_mouse_click(_, _, State = #state{sel=undefined}) ->
    State;
handle_mouse_click(_, right_down, State=#state{panel=Panel}) ->
    popup_menu(Panel),
    State;
handle_mouse_click(_, _, State=#state{app_w=AppWin}) ->
    observer_wx:set_status(""),
    wxWindow:refresh(AppWin),
    State#state{sel=undefined}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_menus(Parent, _) ->
    MenuEntries =
	[{"File",
	  [#create_menu{id=?ID_PROC_INFO, text="Process info"},
	   #create_menu{id=?ID_PROC_MSG,  text="Send Msg"},
	   #create_menu{id=?ID_PROC_KILL, text="Kill process"}
	  ]},
	 {"Trace",
	  [#create_menu{id=?ID_TRACE_PID,  text="Trace process"},
	   #create_menu{id=?ID_TRACE_NAME, text="Trace named process"},
	   #create_menu{id=?ID_TRACE_TREE_PIDS,  text="Trace process tree"},
	   #create_menu{id=?ID_TRACE_TREE_NAMES,  text="Trace named process tree"}
	  ]}],
    observer_wx:create_menus(Parent, MenuEntries).

popup_menu(Panel) ->
    Menu = wxMenu:new(),
    wxMenu:append(Menu, ?ID_PROC_INFO,  "Process info"),
    wxMenu:append(Menu, ?ID_TRACE_PID,  "Trace process"),
    wxMenu:append(Menu, ?ID_TRACE_NAME, "Trace named process"),
    wxMenu:append(Menu, ?ID_TRACE_TREE_PIDS, "Trace process tree"),
    wxMenu:append(Menu, ?ID_TRACE_TREE_NAMES, "Trace named process tree"),
    wxMenu:append(Menu, ?ID_PROC_MSG,   "Send Msg"),
    wxMenu:append(Menu, ?ID_PROC_KILL,  "Kill process"),
    wxWindow:popupMenu(Panel, Menu),
    wxMenu:destroy(Menu).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
locate_node(X, _Y, [{Box=#box{x=BX}, _Chs}|_Rest])
  when X < BX ->
    {left, Box};
locate_node(X,Y, [Node={Box=#box{x=BX,y=BY,w=BW,h=BH}, _Chs}|Rest])
  when X =< (BX+BW)->
    if
	Y < BY -> {above, Box}; %% Above
	Y =< (BY+BH) -> Node;
	true -> locate_node(X,Y,Rest)
    end;
locate_node(X,Y, [{_, Chs}|Rest]) ->
    case locate_node(X,Y,Chs) of
	Node = {#box{},_} -> Node;
	_Miss ->
	    locate_node(X,Y,Rest)
    end;
locate_node(_, _, []) -> false.

locate_box(From, [{Box=#box{s1=#str{pid=From}},_}|_]) -> Box;
locate_box(From, [{_,Chs}|Rest]) ->
    case locate_box(From, Chs) of
	Box = #box{} -> Box;
	_ -> locate_box(From, Rest)
    end;
locate_box(From, []) -> {false, From}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

build_tree({Root, P2Name, Links, XLinks0}, FontW) ->
    Fam = sofs:relation_to_family(sofs:relation(Links)),
    Name2P = gb_trees:from_orddict(lists:sort([{Name,Pid} || {Pid,Name} <- P2Name])),
    Lookup = gb_trees:from_orddict(sofs:to_external(Fam)),
    {_, Tree0} = build_tree2(Root, Lookup, Name2P, FontW),
    {Tree, Dim} = calc_tree_size(Tree0),
    Fetch = fun({From, To}, Acc) ->
		    try {value, ToPid} = gb_trees:lookup(To, Name2P),
			 FromPid = gb_trees:get(From, Name2P),
			 [{locate_box(FromPid, [Tree]),locate_box(ToPid, [Tree])}|Acc]
		    catch _:_ ->
			    Acc
		    end
	    end,
    XLinks = lists:foldl(Fetch, [], XLinks0),
    #app{ptree=Tree, dim=Dim, links=XLinks}.

build_tree2(Root, Tree0, N2P, FontW) ->
    case gb_trees:lookup(Root, Tree0) of
	none -> {Tree0, {box(Root, N2P, FontW), []}};
	{value, Children} ->
	    Tree1 = gb_trees:delete(Root, Tree0),
	    {Tree, CHs} = lists:foldr(fun("port " ++_, Acc) ->
					      Acc; %% Skip ports
					 (Child,{T0, Acc}) ->
					      {T, C} = build_tree2(Child, T0, N2P, FontW),
					      {T, [C|Acc]}
				      end, {Tree1, []}, Children),
	    {Tree, {box(Root, N2P, FontW), CHs}}
    end.

calc_tree_size(Tree) ->
    Cols = calc_col_start(Tree, [0]),
    {Boxes,{W,Hs}} = calc_tree_size(Tree, Cols, ?BB_X, [?BB_Y]),
    {Boxes, {W,lists:max(Hs)}}.

calc_col_start({#box{w=W}, Chs}, [Max|Acc0]) ->
    Acc = if Acc0 == [] -> [0]; true -> Acc0 end,
    Depth = lists:foldl(fun(Child, MDepth) -> calc_col_start(Child, MDepth) end,
			Acc, Chs),
    [max(W,Max)|Depth].

calc_tree_size({Box=#box{w=W,h=H}, []}, _, X, [Y|Ys]) ->
    {{Box#box{x=X,y=Y}, []}, {X+W+?BB_X,[Y+H+?BB_Y|Ys]}};
calc_tree_size({Box, Children}, [Col|Cols], X, [H0|Hs0]) ->
    Hs1 = calc_row_start(Children, H0, Hs0),
    StartX = X+Col+?BB_X,
    {Boxes, {W,Hs}} = calc_tree_sizes(Children, Cols, StartX, StartX, Hs1, []),
    Y = middle(Boxes, H0),
    H = Y+Box#box.h+?BB_Y,
    {{Box#box{x=X,y=Y}, Boxes}, {W,[H|Hs]}}.

calc_tree_sizes([Child|Chs], Cols, X0, W0, Hs0, Acc) ->
    {Tree, {W,Hs}} = calc_tree_size(Child, Cols, X0, Hs0),
    calc_tree_sizes(Chs, Cols, X0, max(W,W0), Hs, [Tree|Acc]);
calc_tree_sizes([], _, _, W,Hs, Acc) ->
    {lists:reverse(Acc), {W,Hs}}.

calc_row_start(Chs = [{#box{h=H},_}|_], Start, Hs0) ->
    NChs = length(Chs),
    Wanted = (H*NChs + ?BB_Y*(NChs-1)) div 2 - H div 2,
    case Hs0 of
	[] -> [max(?BB_Y, Start - Wanted)];
	[Next|Hs] ->
	    [max(Next, Start - Wanted)|Hs]
    end.

middle([], Y) -> Y;
middle([{#box{y=Y}, _}], _) -> Y;
middle([{#box{y=Y0},_}|List], _) ->
    {#box{y=Y1},_} = lists:last(List),
    (Y0+Y1) div 2.

box(Str0, N2P, FontW) ->
    Pid = gb_trees:get(Str0, N2P),
    Str = if hd(Str0) =:= $< -> lists:append(io_lib:format("~w", [Pid]));
	     true -> Str0
	  end,
    {TW,TH} = getTextExtent(FontW, Str),
    Data = #str{text=Str, x=?BX_HE, y=?BY_HE, pid=Pid},
    %% Add pid
    #box{w=round(TW)+?BX_E, h=round(TH)+?BY_E, s1=Data}.

box_to_pid(#box{s1=#str{pid=Pid}}) -> Pid.
box_to_reg(#box{s1=#str{text=[$<|_], pid=Pid}}) -> Pid;
box_to_reg(#box{s1=#str{text=Name}}) -> list_to_atom(Name).

tree_map({Box, Chs}, Fun) ->
    tree_map(Chs, Fun, [Fun(Box)]).
tree_map([{Box, Chs}|Rest], Fun, Acc0) ->
    Acc = tree_map(Chs, Fun, [Fun(Box)|Acc0]),
    tree_map(Rest, Fun, Acc);
tree_map([], _ , Acc) -> Acc.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
draw(_DC, undefined, _, _) ->
    ok;
draw(DC, #app{dim={_W,_H}, ptree=Tree, links=Links}, Sel,
     #paint{font=Font, pen=Pen, brush=Brush, links=LPen, sel=SelBrush}) ->
    setPen(DC, LPen),
    [draw_xlink(Link, DC) || Link <- Links],
    setPen(DC, Pen),
    %% ?wxGC:drawRectangle(DC, 2,2, _W-2,_H-2), %% DEBUG
    setBrush(DC, Brush),
    setFont(DC, Font, {0,0,0}),
    draw_tree(Tree, root, DC),
    case Sel of
	undefined -> ok;
	{#box{x=X,y=Y,w=W,h=H,s1=Str1}, _} ->
	    setBrush(DC, SelBrush),
	    drawRoundedRectangle(DC, X-1,Y-1, W+2,H+2, 8.0),
	    draw_str(DC, Str1, X, Y)
    end.

draw_tree({Box=#box{x=X,y=Y,w=W,h=H,s1=Str1}, Chs}, Parent, DC) ->
    drawRoundedRectangle(DC, X,Y, W,H, 8.0),
    draw_str(DC, Str1, X, Y),
    Dot = case Chs of
	      [] -> ok;
	      [{#box{x=CX0},_}|_] ->
		  CY = Y+(H div 2),
		  CX = CX0-(?BB_X div 2),
		  strokeLine(DC, X+W, CY, CX, CY),
		  {CX, CY}
	  end,
    draw_link(Parent, Box, DC),
    [draw_tree(Child, Dot, DC) || Child <- Chs].

draw_link({CX,CY}, #box{x=X,y=Y0,h=H}, DC) ->
    Y = Y0+(H div 2),
    case Y =:= CY of
	true ->
	    strokeLine(DC, CX, CY, X, CY);
	false ->
	    strokeLines(DC, [{CX, CY}, {CX, Y}, {X,Y}])
    end;
draw_link(_, _, _) -> ok.

draw_xlink({#box{x=X0, y=Y0, h=BH}, #box{x=X1, y=Y1}}, DC)
  when X0 =:= X1 ->
    draw_xlink(X0,Y0,X1,Y1,BH,DC);
draw_xlink({#box{x=X0, y=Y0, h=BH, w=BW}, #box{x=X1, y=Y1}}, DC)
  when X0 < X1 ->
    draw_xlink(X0+BW,Y0,X1,Y1,BH,DC);
draw_xlink({#box{x=X0, y=Y0, h=BH}, #box{x=X1, w=BW, y=Y1}}, DC)
  when X0 > X1 ->
    draw_xlink(X1+BW,Y1,X0,Y0,BH,DC);
draw_xlink({_From, _To}, _DC) ->
    ignore.
draw_xlink(X0, Y00, X1, Y11, BH, DC) ->
    {Y0,Y1} = if Y00 < Y11 -> {Y00+BH-6, Y11+6};
		 true -> {Y00+6, Y11+BH-6}
	      end,
    strokeLines(DC, [{X0,Y0}, {X0+5,Y0}, {X1-5,Y1}, {X1,Y1}]).

draw_str(DC, #str{x=Sx,y=Sy, text=Text}, X, Y) ->
    drawText(DC, Text, X+Sx,Y+Sy);
draw_str(_, _, _, _) -> ok.
