%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2009. All Rights Reserved.
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
-module(dbg_ui_mon_win).

%% External exports
-export([init/0]).
-export([create_win/3, get_window/1,
	 show_option/3,
	 enable/2, is_enabled/1, select/2,
	 add_module/3, delete_module/2,
	 add_process/6, update_process/4, clear_processes/1,
	 add_break/3, update_break/2, delete_break/2,
	 clear_breaks/1, clear_breaks/2,
	 handle_event/2
	]).

-define(default_rows,50).

-record(moduleInfo, {module, menubtn}).
-record(procInfo, {pid, row}).
-record(breakInfo, {point, status, break}).
-record(winInfo, {window,       % gsobj()
		  grid,         % gsobj()
		  row,          % int() Last row in grid

		  focus,        % int() Selected row in grid

		  modules=[],   % [#moduleInfo{}] Known modules
		  processes=[], % [#procInfo{}] Known processes
		  breaks=[],    % [#breakInfo{}] Known breakpoints

		  listbox,      % gsobj() Listinng known modules

		  %% Auto attach buttons
		  fbutton,      % gsobj()
		  bbutton,      % gsobj()
		  ebutton,      % gsobj()
		  selected=[],  % ['First Call'|'On Break'|'On Exit']

		  slabel,       % showing Stack Trace option
		  blabel        % showing Back Trace Size
		 }).

%%====================================================================
%% External exports
%%====================================================================

init() ->
    dbg_ui_win:init().

%%--------------------------------------------------------------------
%% create_win(GS, Title, Menus) -> #winInfo{}
%%   GS = gsobj()
%%   Title = string()
%%   Menus = [menu()]  See dbg_ui_win.erl
%%--------------------------------------------------------------------

-define(PAD, 5).
-define(Wf, 150).
-define(Wg, 770).
-define(W, 800).
-define(H, 390).

create_win(GS, Title, Menus) ->
    Win = gs:window(GS, [{title, Title},
			 {width, ?W}, {height, ?H},
			 {configure,true}, {destroy,true},
			 {keypress,true}, {motion,true}]),

    MenuBar = gs:menubar(Win, []),
    dbg_ui_win:create_menus(MenuBar, Menus),
    dbg_ui_winman:windows_menu(MenuBar),

    Font = dbg_ui_win:font(normal),

    Frame = gs:frame(Win, [{x, ?PAD}, {y, 30},
			   {width, ?Wf}, {height, ?H}]),
    Hlb = 200,
    Listbox = gs:listbox(Frame, [{x, 0}, {y, 0},
				 {width, ?Wf}, {height, Hlb},
				 {data, listbox},
				 {doubleclick, true},
				 {items, []}]),
    gs:label(Frame, [{x, 0}, {y, Hlb}, {width, ?Wf}, {height, 20},
		     {align, w},
		     {label, {text, "Auto Attach:"}}, {font, Font}]),
    Fbtn = gs:checkbutton(Frame, [{x, 0}, {y, Hlb+20},
				  {width, ?Wf}, {height, 20},
				  {label, {text, 'First Call'}},
				  {align, w}, {font, Font},
				  {data, autoattach}]),
    Bbtn = gs:checkbutton(Frame, [{x, 0}, {y, Hlb+40},
				  {width, ?Wf}, {height, 20},
				  {label, {text, 'On Break'}},
				  {align, w}, {font, Font},
				  {data, autoattach}]),
    Ebtn = gs:checkbutton(Frame, [{x, 0}, {y, Hlb+60},
				  {width, ?Wf}, {height, 20},
				  {label, {text, 'On Exit'}},
				  {align, w}, {font, Font},
				  {data, autoattach}]),
    SLabel = gs:label(Frame, [{x, 0}, {y, Hlb+80},
			      {width, ?Wf}, {height, 40},
			      {font, Font}, {align, w}]),
    BLabel = gs:label(Frame, [{x, 0}, {y, Hlb+120},
			      {width, ?Wf}, {height, 40},
			      {font, Font}, {align, w}]),

    Grid = gs:grid(Win, [{x, 2*?PAD+?Wf}, {y, 30},
			 {width, ?W-(2*?PAD+?Wf)}, {height, ?H-30},
			 {bg, grey}, {fg, black},
			 {vscroll, right}, {hscroll, bottom},
			 calc_columnwidths(?Wg),
			 {rows, {1,?default_rows}}]),
    gs:gridline(Grid, [{row, 1}, {bw, 5}, {fg, blue},
		       {font, Font},
		       {text, {1,"Pid"}}, {text, {2,"Initial Call"}},
		       {text, {3,"Name"}}, {text, {4,"Status"}},
		       {text, {5,"Information"}}]),

    gs:config(Win, {map, true}),
    #winInfo{window=Win, grid=Grid, row=1, focus=0,
	     listbox=Listbox,
	     fbutton=Fbtn, bbutton=Bbtn, ebutton=Ebtn,
	     slabel=SLabel, blabel=BLabel}.

%%--------------------------------------------------------------------
%% get_window(WinInfo) -> Window
%%   WinInfo = #winInfo{}
%%   Window = gsobj()
%%--------------------------------------------------------------------
get_window(WinInfo) ->
    WinInfo#winInfo.window.

%%--------------------------------------------------------------------
%% show_option(WinInfo, Option, Value) -> void()
%%   WinInfo = #winInfo{}
%%   Option = auto_attach | stack_trace | back_trace
%%   Value = [Flag]                          % Option==auto_attach
%%             Flag = init | break | exit
%%         | true | all | no_tail | false    % Option==stack_trace
%%         | int()                           % Option==back_trace
%%--------------------------------------------------------------------
show_option(WinInfo, Option, Value) ->
    case Option of

	auto_attach ->
	    lists:foreach(fun (Button) ->
				  gs:config(Button, {select, false})
			  end,
			  option_buttons(WinInfo, [init, break, exit])),
	    lists:foreach(fun (Button) ->
				  gs:config(Button, {select, true})
			  end,
			  option_buttons(WinInfo, Value));

	stack_trace ->
	    Text = case Value of
		       all ->     "Stack Trace:\n On (with tail)";
		       true ->    "Stack Trace:\n On (with tail)";
		       no_tail -> "Stack Trace:\n On (no tail)";
		       false ->   "Stack Trace:\n Off"
		   end,
	    gs:config(WinInfo#winInfo.slabel, {label, {text, Text}});

	back_trace ->
	    Text = "Back Trace Size:\n " ++ integer_to_list(Value),
	    gs:config(WinInfo#winInfo.blabel, {label, {text, Text}})
    end.

option_buttons(WinInfo, [init|Flags]) ->
    [WinInfo#winInfo.fbutton|option_buttons(WinInfo, Flags)];
option_buttons(WinInfo, [break|Flags]) ->
    [WinInfo#winInfo.bbutton|option_buttons(WinInfo, Flags)];
option_buttons(WinInfo, [exit|Flags]) ->
    [WinInfo#winInfo.ebutton|option_buttons(WinInfo, Flags)];
option_buttons(_WinInfo, []) ->
    [].

%%--------------------------------------------------------------------
%% enable([MenuItem], Bool)
%% is_enabled(MenuItem) -> Bool
%%   MenuItem = atom()
%%   Bool = boolean()
%%--------------------------------------------------------------------
enable(MenuItems, Bool) ->
    lists:foreach(fun(MenuItem) ->
			  gs:config(MenuItem, {enable, Bool})
		  end,
		  MenuItems).

is_enabled(MenuItem) ->
    gs:read(MenuItem, enable).

%%--------------------------------------------------------------------
%% select(MenuItem, Bool)
%%   MenuItem = atom()
%%   Bool = boolean()
%%--------------------------------------------------------------------
select(MenuItem, Bool) ->
    dbg_ui_win:select(MenuItem, Bool).

%%--------------------------------------------------------------------
%% add_module(WinInfo, Name, Mod) -> WinInfo
%%   WinInfo = #winInfo{}
%%   Name = atom()
%%   Mod = atom()
%%--------------------------------------------------------------------
add_module(WinInfo, Menu, Mod) ->
    Modules = WinInfo#winInfo.modules,
    case lists:keysearch(Mod, #moduleInfo.module, Modules) of
	{value, _ModInfo} -> WinInfo;
	false ->
	    %% Create a menu for the module
	    Font = dbg_ui_win:font(normal),
	    MenuBtn = gs:menuitem(Menu, [{label, {text,Mod}},
					 {font, Font},
					 {itemtype, cascade}]),
	    SubMenu = gs:menu(MenuBtn, []),
	    gs:menuitem(SubMenu, [{label, {text,"View"}},
				  {font, Font},
				  {data, {module,Mod,view}}]),
	    gs:menuitem(SubMenu, [{label, {text,"Delete"}},
				  {font, Font},
				  {data, {module,Mod,delete}}]),

	    %% Add the module to the listbox
	    gs:config(WinInfo#winInfo.listbox, {add, Mod}),

	    ModInfo = #moduleInfo{module=Mod, menubtn=MenuBtn},
	    WinInfo#winInfo{modules=[ModInfo | Modules]}
    end.
    
%%--------------------------------------------------------------------
%% delete_module(WinInfo, Mod) -> WinInfo
%%   WinInfo = #winInfo{}
%%   Mod = atom()
%%--------------------------------------------------------------------
delete_module(WinInfo, Mod) ->
    {value, ModInfo} = lists:keysearch(Mod, #moduleInfo.module,
				       WinInfo#winInfo.modules),
    gs:destroy(ModInfo#moduleInfo.menubtn),
    delete_module(WinInfo#winInfo.listbox, atom_to_list(Mod), 0),
    WinInfo#winInfo{modules=lists:keydelete(Mod, #moduleInfo.module,
					    WinInfo#winInfo.modules)}.

delete_module(Listbox, ModS, Index) ->
    case gs:read(Listbox, {get, Index}) of
	ModS ->
	    gs:config(Listbox, {del, Index});
	_OtherModS ->
	    delete_module(Listbox, ModS, Index+1)
    end.

%%--------------------------------------------------------------------
%% add_process(WinInfo, Pid, Name, Function, Status, Info) -> WinInfo
%%   WinInfo = #winInfo{}
%%   Pid = pid()
%%   Name = undefined | atom()
%%   Function = {Mod, Func, Args}
%%   Status = idle | running | break | exit
%%   Info = {} | term()
%%--------------------------------------------------------------------
add_process(WinInfo, Pid, Name, {Mod,Func,Args}, Status, Info) ->
    Grid = WinInfo#winInfo.grid,
    Row = (WinInfo#winInfo.row)+1,
    GridLine = case gs:read(Grid, {obj_at_row, Row}) of
		   undefined ->
		       if Row>?default_rows ->
			       gs:config(Grid,[{rows,{1,Row}}]);
			  true -> ok
		       end,
		       gs:gridline(Grid,[{row,Row}, {bw,5}, {fg,black},
					 {font,dbg_ui_win:font(normal)},
					 {click, true},
					 {doubleclick, true}]);
		   GSObj ->
		       GSObj
	       end,
    Name2 = case Name of undefined -> ""; _ -> Name end,
    FuncS = io_lib:format("~w:~w/~w", [Mod, Func, length(Args)]),
    Info2 = case Info of {} -> ""; _ -> Info end,
    Options = [{text, {1,Pid}}, {text, {2,FuncS}}, {text, {3,Name2}},
	       {text, {4,Status}}, {text, {5,Info2}},
	       {data, {gridline, Pid}}],
    gs:config(GridLine, Options),

    ProcInfo = #procInfo{pid=Pid, row=Row},
    WinInfo#winInfo{processes=[ProcInfo|WinInfo#winInfo.processes],
		    row=Row}.

%%--------------------------------------------------------------------
%% update_process(WinInfo, Pid, Status, Info)
%%   WinInfo = #winInfo{}
%%   Pid = pid()
%%   Status = idle | running | break | exit
%%   Info = {} | term()
%%--------------------------------------------------------------------
update_process(WinInfo, Pid, Status, Info) ->
    {value, ProcInfo} = lists:keysearch(Pid, #procInfo.pid,
					WinInfo#winInfo.processes),

    Grid = WinInfo#winInfo.grid,
    GridLine = gs:read(Grid, {obj_at_row, ProcInfo#procInfo.row}),
    
    Info2 = case Info of {} -> ""; _ -> Info end,
    gs:config(GridLine, [{text, {4,Status}}, {text, {5,Info2}}]).

%%--------------------------------------------------------------------
%% clear_processes(WinInfo) -> WinInfo
%%   WinInfo = #winInfo{}
%%--------------------------------------------------------------------
clear_processes(WinInfo) ->
    Grid = WinInfo#winInfo.grid,
    Max = WinInfo#winInfo.row,
    clear_processes(Grid, 2, Max),
    gs:config(Grid,[{rows,{1,?default_rows}}]),
    WinInfo#winInfo{row=1, focus=0, processes=[]}.

clear_processes(Grid, Row, Max) when Row=<Max ->
    GridLine = gs:read(Grid, {obj_at_row, Row}),
    case gs:read(GridLine,{text,4}) of
	"exit" -> 
	    Pid = list_to_pid(gs:read(GridLine,{text,1})),
	    dbg_ui_winman:clear_process(dbg_ui_trace:title(Pid));
	_ -> 
	    ok
    end,
	    
    Options = [{fg, black},
	       {{text,1}, ""}, {{text,2},""}, {{text,3},""},
	       {{text,4}, ""}, {{text,5},""},
	       {data, []}],
    gs:config(GridLine, Options),
    clear_processes(Grid, Row+1, Max);
clear_processes(_Grid, Row, Max) when Row>Max ->
    done.

%%--------------------------------------------------------------------
%% add_break(WinInfo, Name, {Point, Options}) -> WinInfo
%%   WinInfo = #winInfo{}
%%   Name = atom()
%%   Point = {Mod, Line}
%%   Options = [Status, Action, Mods, Cond]
%%     Status = active | inactive
%%     Action = enable | disable | delete
%%     Mods = null (not used)
%%     Cond = null | {Mod, Func}
%%--------------------------------------------------------------------
add_break(WinInfo, Menu, {Point, Options}) ->
    Break = dbg_ui_win:add_break(Menu, Point),
    dbg_ui_win:update_break(Break, Options),
    BreakInfo = #breakInfo{point=Point, break=Break},
    WinInfo#winInfo{breaks=[BreakInfo|WinInfo#winInfo.breaks]}.

%%--------------------------------------------------------------------
%% update_break(WinInfo, {Point, Options})
%%   WinInfo = #winInfo{}
%%   Point = {Mod, Line}
%%   Options = [Status, Action, Mods, Cond]
%%     Status = active | inactive
%%     Action = enable | disable | delete
%%     Mods = null (not used)
%%     Cond = null | {Mod, Func}
%%--------------------------------------------------------------------
update_break(WinInfo, {Point, Options}) ->
    {value, BreakInfo} = lists:keysearch(Point, #breakInfo.point,
					 WinInfo#winInfo.breaks),
    dbg_ui_win:update_break(BreakInfo#breakInfo.break, Options).

%%--------------------------------------------------------------------
%% delete_break(WinInfo, Point) -> WinInfo
%%   WinInfo = #winInfo{}
%%   Point = {Mod, Line}
%%--------------------------------------------------------------------
delete_break(WinInfo, Point) ->
    {value, BreakInfo} = lists:keysearch(Point, #breakInfo.point,
					 WinInfo#winInfo.breaks),
    dbg_ui_win:delete_break(BreakInfo#breakInfo.break),
    WinInfo#winInfo{breaks=lists:keydelete(Point, #breakInfo.point,
					   WinInfo#winInfo.breaks)}.

%%--------------------------------------------------------------------
%% clear_breaks(WinInfo) -> WinInfo
%% clear_breaks(WinInfo, Mod) -> WinInfo
%%   WinInfo = #winInfo{}
%%--------------------------------------------------------------------
clear_breaks(WinInfo) ->
    lists:foreach(fun(BreakInfo) ->
			  dbg_ui_win:delete_break(BreakInfo#breakInfo.break)
		  end,
		  WinInfo#winInfo.breaks),
    WinInfo#winInfo{breaks=[]}.
clear_breaks(WinInfo, Mod) ->
    Fun =
	fun(BreakInfo) ->
		case BreakInfo#breakInfo.point of
		    {Mod, _Line} ->
			dbg_ui_win:delete_break(BreakInfo#breakInfo.break),
			false;
		    _ -> true
		end
	end,
    Breaks = lists:filter(Fun, WinInfo#winInfo.breaks),
    WinInfo#winInfo{breaks=Breaks}.
    
%%--------------------------------------------------------------------
%% handle_event(GSEvent, WinInfo) -> Command
%%   GSEvent = {gs, Id, Event, Data, Arg}
%%   WinInfo = #winInfo{}
%%   Command = ignore
%%           | stopped
%%           | {coords, {X,Y}}
%%
%%           | {shortcut, Key}
%%           | MenuItem | {Menu, [MenuItem]}
%%               MenuItem = Menu = atom()
%%           | {break, Point, What}
%%               What = delete | {status, Status} | {trigger, Trigger}
%%           | {module, Mod, What}
%%               What = view | delete
%%
%%           | {focus, Pid, WinInfo}
%%           | default
%%--------------------------------------------------------------------
%% Window events
handle_event({gs, _Id, configure, _Data, [W, H |_]}, WinInfo) ->
    configure(WinInfo, {W, H}),
    ignore;
handle_event({gs, _Id, destroy, _Data, _Arg}, _WinInfo) ->
    stopped;
handle_event({gs, _Id, motion, _Data, [X,Y]}, WinInfo) ->
    {LastX, LastY} = dbg_ui_win:motion(X, Y),
    Win = WinInfo#winInfo.window,
    {coords, {gs:read(Win, x)+LastX-5, gs:read(Win, y)+LastY-5}};

%% Menus and keyboard shortcuts
handle_event({gs, _Id, keypress, _Data, [Key,_,_,1]}, _WinInfo) when
  Key/='Up', Key/='Down', Key/=p, Key/=n ->
    {shortcut, Key};
handle_event({gs, _Id, click, {dbg_ui_winman, Win}, _Arg}, _WinInfo) ->
    dbg_ui_winman:raise(Win),
    ignore;
handle_event({gs, _Id, click, {menuitem, Name}, _Arg}, _WinInfo) ->
    Name;
handle_event({gs, _Id, click, {menu, Menu}, _Arg}, _WinInfo) ->
    Names = dbg_ui_win:selected(Menu),
    {Menu, Names};
handle_event({gs, _Id, click, {break, Point, What}, _Arg}, _WinInfo) ->
    {break, Point, What};
handle_event({gs, _Id, click, {module, Mod, What}, _Arg}, _WinInfo) ->
    {module, Mod, What};

%% Listbox
handle_event({gs, _Id, doubleclick, listbox, [_Index, ModS|_]}, _WI) ->
    {module, list_to_atom(ModS), view};

%% Auto attach buttons
handle_event({gs, _Id, click, autoattach, _Arg}, WinInfo) ->
    Names = lists:foldl(fun (Button, NamesAcc) ->
				case gs:read(Button, select) of
				    true ->
					{text, Name} =
					    gs:read(Button, label),
					[list_to_atom(Name)|NamesAcc];
				    false ->
					NamesAcc
				end
			end,
			[],
			[WinInfo#winInfo.ebutton,
			 WinInfo#winInfo.bbutton,
			 WinInfo#winInfo.fbutton]),
    {'Auto Attach', Names};

%% Process grid
handle_event({gs, _Id, keypress, _Data, [Key|_]}, WinInfo) when
  Key=='Up'; Key=='Down' ->
    Dir = if Key=='Up' -> up; Key=='Down' -> down end,
    Row = move(WinInfo, Dir),

    if Row>1 ->
	    WinInfo2 = highlight(WinInfo, Row),
	    {value, #procInfo{pid=Pid}} =
		lists:keysearch(Row, #procInfo.row, WinInfo#winInfo.processes),
	    {focus, Pid, WinInfo2};
       true ->
	    ignore
    end;
handle_event({gs, _Id, click, {gridline, Pid}, [_Col,Row|_]}, WinInfo) ->
    WinInfo2 = highlight(WinInfo, Row),
    {focus, Pid, WinInfo2};
handle_event({gs, _Id, doubleclick, _Data, _Arg}, _WinInfo) ->
    default;

handle_event(_GSEvent, _WinInfo) ->
    ignore.

move(WinInfo, Dir) ->
    Row = WinInfo#winInfo.focus,
    Last = WinInfo#winInfo.row,

    if
	Dir==up, Row>1 -> Row-1;
	Dir==down, Row<Last -> Row+1;
	true -> Row
    end.

highlight(WinInfo, Row) ->
    Grid = WinInfo#winInfo.grid,
    case WinInfo#winInfo.focus of
	0 -> ignore;
	Focus ->
	    GridLine1 = gs:read(Grid, {obj_at_row, Focus}),
	    gs:config(GridLine1, {fg, black})
    end,
    GridLine2 = gs:read(Grid, {obj_at_row, Row}),
    gs:config(GridLine2, {fg, white}),
    WinInfo#winInfo{focus=Row}.
    

%%====================================================================
%% Internal functions
%%====================================================================

configure(WinInfo, {W, H}) ->
    Grid = WinInfo#winInfo.grid,
    NewW = W - (2*?PAD+?Wf),
    Dx = NewW - gs:read(Grid, width),
    Dy = H-42 - gs:read(Grid, height),
    if
	(Dx+Dy)=/=0 ->
	    gs:config(Grid, [{width, NewW}, {height, H-30}]),
	    Cols = calc_columnwidths(NewW),
	    gs:config(Grid, Cols);
	true ->
	    ok
    end.

calc_columnwidths(Width) ->
    W = if
	    Width=<?Wg -> ?Wg;
	    true -> Width
	end,
    First = lists:map(fun (X) -> round(X) end,
		      [0.13*W, 0.27*W, 0.18*W, 0.18*W]),
    Last = W - lists:sum(First) - 30,
    {columnwidths, First++[Last]}.
