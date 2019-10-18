%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2019. All Rights Reserved.
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
-module(dbg_wx_mon_win).

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

-import(dbg_wx_win, [to_string/1, to_string/2]).

-include_lib("wx/include/wx.hrl").

-define(STRTEXT, "Use range of +pc flag").

-define(default_rows,10).

-record(moduleInfo, {module, menubtn}).
-record(procInfo, {pid, row}).
-record(breakInfo, {point, status, break}).
-record(break, {mb, smi, emi, dimi, demi}).
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

                  %% Strings button(s)
                  stringsbutton,% gsobj()

		  slabel,       % showing Stack Trace option
		  blabel        % showing Back Trace Size
		 }).

%%====================================================================
%% External exports
%%====================================================================

init() ->
    dbg_wx_win:init().

-define(GRID,1000).

-define(PAD, 5).
-define(Wf, 150).
-define(Wg, 770).
-define(W, 800).
-define(H, 390).

-define(autoId, 314).
-define(stringsId, 271).

create_win(_Wx, Title, Menus) ->
    wx:batch(fun() -> create_win_batch(Title, Menus) end).
		      
create_win_batch(Title, Menus) ->
    Win = wxFrame:new(wx:null(), ?wxID_ANY, Title, 
		      [{size, {?W,?H}}]),
    wxFrame:connect(Win, close_window, [{skip, true}]),
    MenuBar = wxMenuBar:new(),
    dbg_wx_win:create_menus(MenuBar, Menus, Win, 1),
    wxFrame:setMenuBar(Win, MenuBar),
    
    MainSz = wxBoxSizer:new(?wxHORIZONTAL),
    LeftSz = wxBoxSizer:new(?wxVERTICAL),

    Panel = wxPanel:new(Win),
    Hlb = 200,
    Listbox = wxListBox:new(Panel, ?wxID_ANY, [{size,{?Wf,Hlb}},
					       {style,?wxLB_SINGLE}]),
    _ = wxSizer:add(LeftSz,Listbox,[{proportion,1},{border,3},{flag,?wxEXPAND}]),
    wxListBox:connect(Listbox, command_listbox_doubleclicked),
    wxListBox:connect(Listbox, right_down),

    SBox = wxStaticBox:new(Panel, ?wxID_ANY, "Auto Attach:"),
    SBS  = wxStaticBoxSizer:new(SBox, ?wxVERTICAL),
    Fbtn = wxCheckBox:new(Panel, ?autoId, "First Call"),
    _ = wxSizer:add(SBS,Fbtn),
    Bbtn = wxCheckBox:new(Panel, ?autoId, "On Break"),
    _ = wxSizer:add(SBS,Bbtn),
    Ebtn = wxCheckBox:new(Panel, ?autoId, "On Exit"),
    _ = wxSizer:add(SBS,Ebtn),
    wxFrame:connect(Panel, command_checkbox_clicked),
    _ = wxSizer:add(LeftSz,SBS, [{flag,?wxEXPAND}]),

    SLabel = wxStaticText:new(Panel, ?wxID_ANY, "Stack Trace:\n On (with tail)"), 
    _ = wxSizer:add(LeftSz,SLabel),
    BLabel = wxStaticText:new(Panel, ?wxID_ANY, "Back Trace Size:\n 50000"), 
    _ = wxSizer:add(LeftSz,BLabel),
    
    StringsBox = wxStaticBox:new(Panel, ?wxID_ANY, "Strings:"),
    StringsBS  = wxStaticBoxSizer:new(StringsBox, ?wxVERTICAL),
    Stringsbtn = wxCheckBox:new(Panel, ?stringsId, ?STRTEXT),
    _ = wxSizer:add(StringsBS,Stringsbtn),
    _ = wxSizer:add(LeftSz,StringsBS, [{flag,?wxEXPAND}]),

    %% Create list_crtl / grid
    Grid = wxListCtrl:new(Panel, [{winid, ?GRID},
				  {style, ?wxLC_REPORT bor ?wxLC_SINGLE_SEL 
				   bor ?wxLC_HRULES },
				  {size, {600, -1}}]),
    LI = wxListItem:new(),
    wxListItem:setText(LI, "Pid"), 
    wxListItem:setAlign(LI, ?wxLIST_FORMAT_CENTRE),
    wxListCtrl:insertColumn(Grid, 0, LI),
    wxListItem:setText(LI, "Initial Call"),
    wxListItem:setAlign(LI, ?wxLIST_FORMAT_LEFT),
    wxListCtrl:insertColumn(Grid, 1, LI),
    wxListItem:setText(LI, "Name"), 
    wxListCtrl:insertColumn(Grid, 2, LI),
    wxListItem:setAlign(LI, ?wxLIST_FORMAT_CENTRE),
    wxListItem:setText(LI, "Status"), 
    wxListCtrl:insertColumn(Grid, 3, LI),
    wxListItem:setText(LI, "Information"), 
    wxListItem:setAlign(LI, ?wxLIST_FORMAT_LEFT),
    wxListCtrl:insertColumn(Grid, 4, LI),
    wxListItem:destroy(LI),
        
    wxListCtrl:setColumnWidth(Grid, 0, 80),
    wxListCtrl:setColumnWidth(Grid, 1, 150),
    wxListCtrl:setColumnWidth(Grid, 2, 100),
    wxListCtrl:setColumnWidth(Grid, 3, 70),
    wxListCtrl:setColumnWidth(Grid, 4, 200),
    wxListCtrl:connect(Grid, command_list_item_activated), 
    wxListCtrl:connect(Grid, command_list_item_selected), 
    wxListCtrl:connect(Grid, size, [{skip, true}]),
    wxListCtrl:connect(Grid, key_up, [{id, ?GRID}, {skip,true}]),

    wxWindow:connect(Win, enter_window, [{skip,true}]),
    wxWindow:setFocus(Grid),

    %% Put it in the window
    _ = wxSizer:add(MainSz, LeftSz, [{border, 3}, {flag,?wxALL bor ?wxEXPAND}]),
    _ = wxSizer:add(MainSz, Grid,   [{border, 3}, {flag,?wxALL bor ?wxEXPAND}, 
				 {proportion, 1}]),

    wxWindow:setSizer(Panel,MainSz),
    _ = wxSizer:fit(MainSz, Win),
    wxSizer:setSizeHints(MainSz,Win),
    
    IconFile = dbg_wx_win:find_icon("erlang_bug.png"),
    Icon = wxIcon:new(IconFile, [{type,?wxBITMAP_TYPE_PNG}]),
    wxFrame:setIcon(Win, Icon),
    wxIcon:destroy(Icon),
    wxFrame:show(Win),
    dbg_wx_winman:raise(Win),
    #winInfo{window=Win, grid=Grid, row=0, focus=0,
	     listbox=Listbox,
	     fbutton=Fbtn, bbutton=Bbtn, ebutton=Ebtn,
             stringsbutton=Stringsbtn,
	     slabel=SLabel, blabel=BLabel}.

%%--------------------------------------------------------------------
%% get_window(WinInfo) -> Window
%%   WinInfo = #winInfo{}
%%   Window = wxobj()
%%--------------------------------------------------------------------
get_window(WinInfo) ->
    WinInfo#winInfo.window.

%%--------------------------------------------------------------------
%% show_option(WinInfo, Option, Value) -> void()
%%   WinInfo = #winInfo{}
%%   Option = auto_attach | stack_trace | back_trace | strings
%%   Value = [Flag]                          % Option==auto_attach
%%             Flag = init | break | exit
%%         | true | all | no_tail | false    % Option==stack_trace
%%         | integer()                       % Option==back_trace
%%         | [SFlag]                         % Option==strings
%%             SFlag = str_on
%%--------------------------------------------------------------------
show_option(WinInfo, Option, Value) ->
    case Option of	
	auto_attach ->
	    wx:foreach(fun(Button) ->
				  wxCheckBox:setValue(Button, false)
			  end,
			  option_buttons(WinInfo, [init, break, exit])),
	    wx:foreach(fun(Button) ->
				  wxCheckBox:setValue(Button, true)
			  end,
			  option_buttons(WinInfo, Value));

	stack_trace ->
	    Text = case Value of
		       all ->     "Stack Trace:\n On (with tail)";
		       true ->    "Stack Trace:\n On (with tail)";
		       no_tail -> "Stack Trace:\n On (no tail)";
		       false ->   "Stack Trace:\n Off"
		   end,
	    wxStaticText:setLabel(WinInfo#winInfo.slabel, Text);

	back_trace ->
	    Text = "Back Trace Size:\n " ++ integer_to_list(Value),
	    wxStaticText:setLabel(WinInfo#winInfo.blabel, Text);
        strings ->
	    wx:foreach(fun(Button) ->
				  wxCheckBox:setValue(Button, false)
			  end,
			  option_buttons(WinInfo, [str_on])),
	    wx:foreach(fun(Button) ->
				  wxCheckBox:setValue(Button, true)
			  end,
			  option_buttons(WinInfo, Value))
    end.

%% Auto Attach
option_buttons(WinInfo, [init|Flags]) ->
    [WinInfo#winInfo.fbutton|option_buttons(WinInfo, Flags)];
option_buttons(WinInfo, [break|Flags]) ->
    [WinInfo#winInfo.bbutton|option_buttons(WinInfo, Flags)];
option_buttons(WinInfo, [exit|Flags]) ->
    [WinInfo#winInfo.ebutton|option_buttons(WinInfo, Flags)];
%% Strings
option_buttons(WinInfo, [str_on|Flags]) ->
    [WinInfo#winInfo.stringsbutton|option_buttons(WinInfo, Flags)];
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
			  MI = get(MenuItem),
			  wxMenuItem:enable(MI, [{enable, Bool}])
		  end,
		  MenuItems).

is_enabled(MenuItem) ->
    MI = get(MenuItem),
    wxMenuItem:isEnabled(MI).

%%--------------------------------------------------------------------
%% select(MenuItem, Bool)
%%   MenuItem = atom()
%%   Bool = boolean()
%%--------------------------------------------------------------------
select(MenuItem, Bool) ->
    MI = get(MenuItem),
    wxMenuItem:check(MI, [{check, Bool}]).

%%--------------------------------------------------------------------
%% add_module(WinInfo, Name, Mod) -> WinInfo
%%   WinInfo = #winInfo{}
%%   Name = atom()
%%   Mod = atom()
%%--------------------------------------------------------------------
add_module(WinInfo, MenuName, Mod) ->
    Win = WinInfo#winInfo.window,
    Modules = WinInfo#winInfo.modules,
    case lists:keymember(Mod, #moduleInfo.module, Modules) of
	false ->
	    %% Create a menu for the module
	    Menu = get(MenuName),
	    Sub = wxMenu:new([]),
	    ViewItem = wxMenu:append(Sub, ?wxID_ANY, "View"), 
	    ViewId = wxMenuItem:getId(ViewItem),
	    wxMenu:connect(Win, command_menu_selected, 
			   [{id,ViewId}, {userData, {module,Mod,view}}]),
	    DelItem = wxMenu:append(Sub, ?wxID_ANY, "Delete"), 
	    DelId = wxMenuItem:getId(DelItem),
	    wxMenu:connect(Win, command_menu_selected, 
			   [{id,DelId}, {userData, {module,Mod,delete}}]),
	    MenuBtn = wxMenu:append(Menu, ?wxID_ANY, atom_to_list(Mod), Sub),
	    wxListBox:append(WinInfo#winInfo.listbox, atom_to_list(Mod)),
	    
	    ModInfo = #moduleInfo{module=Mod, menubtn={Menu,MenuBtn}},
	    WinInfo#winInfo{modules=[ModInfo | Modules]};
	true -> WinInfo
   end.
    
%%--------------------------------------------------------------------
%% delete_module(WinInfo, Mod) -> WinInfo
%%   WinInfo = #winInfo{}
%%   Mod = atom()
%%--------------------------------------------------------------------
delete_module(WinInfo, Mod) ->
    {value, ModInfo} = lists:keysearch(Mod, #moduleInfo.module,
				       WinInfo#winInfo.modules),
    {Menu, MenuBtn} = ModInfo#moduleInfo.menubtn,
    wxMenu:'Destroy'(Menu, MenuBtn),
    ListBox = WinInfo#winInfo.listbox,
    Id = wxListBox:findString(ListBox, atom_to_list(Mod)),
    wxListBox:delete(ListBox,Id),
    WinInfo#winInfo{modules=lists:keydelete(Mod, #moduleInfo.module,
					    WinInfo#winInfo.modules)}.

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
    Row = (WinInfo#winInfo.row),
    
    Name2 = case Name of undefined -> ""; _ -> to_string(Name) end,
    FuncS = to_string("~w:~tw/~w", [Mod, Func, length(Args)]),
    Info2 = case Info of {} -> ""; _ -> to_string(Info) end,
    Pid2  = to_string("~p",[Pid]),
    
    Add = fun() ->
		  _Dbg = wxListCtrl:insertItem(Grid, Row,""),
		  %%wxListCtrl:setItemData(Grid,Temp,Row),
		  if (Row rem 2) =:= 0 -> 
			  wxListCtrl:setItemBackgroundColour(Grid, Row, {240,240,255});
		     true -> ignore
		  end,

		  wxListCtrl:setItem(Grid, Row, 0, Pid2),
		  wxListCtrl:setItem(Grid, Row, 1, FuncS),
		  wxListCtrl:setItem(Grid, Row, 2, Name2),
		  wxListCtrl:setItem(Grid, Row, 3, to_string(Status)),
		  wxListCtrl:setItem(Grid, Row, 4, Info2),
		  ok
	  end,
    wx:batch(Add),

    ProcInfo = #procInfo{pid=Pid, row=Row},
    WinInfo#winInfo{processes=[ProcInfo|WinInfo#winInfo.processes],
		    row=Row+1}.

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
    Row  = ProcInfo#procInfo.row,    
    Info2 = case Info of {} -> ""; _ -> Info end,
    wxListCtrl:setItem(Grid, Row, 3, to_string(Status)),
    wxListCtrl:setItem(Grid, Row, 4, to_string(Info2)).
  
%%--------------------------------------------------------------------
%% clear_processes(WinInfo) -> WinInfo
%%   WinInfo = #winInfo{}
%%--------------------------------------------------------------------
clear_processes(WinInfo) ->
    Grid = WinInfo#winInfo.grid,
    Max = WinInfo#winInfo.row,
    wx:batch(fun() -> clear_processes(Grid, Max-1) end),
    WinInfo#winInfo{row=0, focus=0, processes=[]}.

clear_processes(Grid, Row) when Row >= 0 ->
    Item = wxListItem:new(),
    wxListItem:setId(Item,Row),
    wxListItem:setColumn(Item, 3),
    case wxListCtrl:getItem(Grid, Item) of
	true -> 
	    case wxListItem:getText(Item) of
		"exit" ->
		    wxListItem:setColumn(Item, 0),
		    wxListCtrl:getItem(Grid, Item),
		    Pid = list_to_pid(wxListItem:getText(Item)),
		    dbg_wx_winman:clear_process(dbg_wx_trace:title(Pid));
		_ ->
		    ok
	    end;
	false ->
	    ignore
    end,
    wxListItem:destroy(Item),
    wxListCtrl:deleteItem(Grid, Row),
    clear_processes(Grid, Row-1);
clear_processes(_Grid, _Row) ->
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
    Break = dbg_wx_win:add_break(WinInfo#winInfo.window, Menu, Point),
    dbg_wx_win:update_break(Break, Options),
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
    dbg_wx_win:update_break(BreakInfo#breakInfo.break, Options).

%%--------------------------------------------------------------------
%% delete_break(WinInfo, Point) -> WinInfo
%%   WinInfo = #winInfo{}
%%   Point = {Mod, Line}
%%--------------------------------------------------------------------
delete_break(WinInfo, Point) ->
    {value, BreakInfo} = lists:keysearch(Point, #breakInfo.point,
					 WinInfo#winInfo.breaks),
    dbg_wx_win:delete_break(BreakInfo#breakInfo.break),
    WinInfo#winInfo{breaks=lists:keydelete(Point, #breakInfo.point,
					   WinInfo#winInfo.breaks)}.

%%--------------------------------------------------------------------
%% clear_breaks(WinInfo) -> WinInfo
%% clear_breaks(WinInfo, Mod) -> WinInfo
%%   WinInfo = #winInfo{}
%%--------------------------------------------------------------------
clear_breaks(WinInfo) ->
    lists:foreach(fun(BreakInfo) ->
			  dbg_wx_win:delete_break(BreakInfo#breakInfo.break)
		  end,
		  WinInfo#winInfo.breaks),
    WinInfo#winInfo{breaks=[]}.
clear_breaks(WinInfo, Mod) ->
    Fun =
	fun(BreakInfo) ->
		case BreakInfo#breakInfo.point of
		    {Mod, _Line} ->
			dbg_wx_win:delete_break(BreakInfo#breakInfo.break),
			false;
		    _ -> true
		end
	end,
    Breaks = lists:filter(Fun, WinInfo#winInfo.breaks),
    WinInfo#winInfo{breaks=Breaks}.
    
%%--------------------------------------------------------------------
%% handle_event(WxEvent, WinInfo) -> Command
%%   WxEvent = #wx{}
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
handle_event(#wx{event=#wxSize{size={W,_}}}, #winInfo{grid=Grid}) ->
    wx:batch(fun() ->
		     Tot = wx:foldl(fun(C,Sum) -> 
					    Sum + wxListCtrl:getColumnWidth(Grid, C)
				    end, 0, [0,1,2,3]),
		     wxListCtrl:setColumnWidth(Grid, 4, W-Tot-4)
	     end),
    ignore;
handle_event(_Ev=#wx{event=#wxClose{}}, _WinInfo) ->
%%    io:format("~p Received ~p close ~p~n", [?MODULE, self(), _Ev]),
    stopped;

%% Menus and keyboard shortcuts
handle_event(#wx{userData={dbg_ui_winman, Win},
		 event=#wxCommand{type=command_menu_selected}}, _Wi) ->
    dbg_wx_winman:raise(Win),
    ignore;
handle_event(_Ev = #wx{event=#wxKey{keyCode=Key, controlDown=true}}, _WinInfo) ->
    if
	Key/=?WXK_UP, Key/=?WXK_DOWN, Key /=? WXK_RETURN -> 
	    try  
		{shortcut, list_to_atom([Key+($a-$A)])}
	    catch _:_ -> ignore
	    end;
	true -> 
	    ignore
    end;

handle_event(#wx{userData={break, Point, status}, 
		 event=#wxCommand{type=command_menu_selected}},
	     WinInfo) ->
    {value, BreakInfo} = lists:keysearch(Point, #breakInfo.point,
					 WinInfo#winInfo.breaks),
    %% This is a temporary hack !!
    #breakInfo{break=#break{smi=Smi}} = BreakInfo,

    case wxMenuItem:getText(Smi) of
	"Enable" -> {break, Point, {status, active}};
	"Disable" -> {break, Point, {status, inactive}}
    end;

%% Listbox
handle_event(#wx{event=#wxCommand{type=command_listbox_doubleclicked, cmdString=ModS}}, 
	     _WinInfo) ->
    {module, list_to_atom(ModS), view};
handle_event(#wx{obj=ListBox, event=#wxMouse{type=right_down, x=X,y=Y}}, 
	     #winInfo{listbox=ListBox}) ->
    case wxListBox:hitTest(ListBox, {X,Y}) of
	?wxNOT_FOUND -> ignore;
	Row ->	    
	    ModS = wxListBox:getString(ListBox,Row),
	    io:format("Re-loading/interpreting: ~s~n", [ModS]),
	    int:i(list_to_atom(ModS)),
	    ignore
    end;

%% Auto attach buttons
handle_event(#wx{id=?autoId,
                 event=#wxCommand{type=command_checkbox_clicked}},
	     WinInfo) ->
    Check = fun(Button, NamesAcc) ->
		    case wxCheckBox:isChecked(Button) of
			true ->
			    Name = wxCheckBox:getLabel(Button),
			    [list_to_atom(Name)|NamesAcc];
			false ->
			    NamesAcc
		    end
	    end,
    Names = wx:foldl(Check, [],
		     [WinInfo#winInfo.ebutton,
		      WinInfo#winInfo.bbutton,
		      WinInfo#winInfo.fbutton]),
    {'Auto Attach', Names};

%% Strings button(s)
handle_event(#wx{id=?stringsId,
                 event=#wxCommand{type=command_checkbox_clicked}},
	     WinInfo) ->
    Check = fun(Button, NamesAcc) ->
		    case wxCheckBox:isChecked(Button) of
			true ->
			    Name = wxCheckBox:getLabel(Button),
			    [list_to_atom(Name)|NamesAcc];
			false ->
			    NamesAcc
		    end
	    end,
    Names = wx:foldl(Check, [],
		     [WinInfo#winInfo.stringsbutton]),
    {'Strings', Names};

%% Process grid
handle_event(#wx{event=#wxList{type=command_list_item_selected,
			       itemIndex=Row}}, WinInfo) ->
    #winInfo{processes=Pids} = WinInfo,
    #procInfo{pid=Pid} = lists:keyfind(Row, #procInfo.row, Pids),
    {focus, Pid, WinInfo#winInfo{focus=Row}};
handle_event(#wx{event=#wxList{type=command_list_item_activated}}, 
	     _WinInfo) ->
    default;
handle_event(#wx{event=#wxMouse{type=enter_window}}, #winInfo{grid=Grid}) ->
    %% Keyboard focus
    wxWindow:setFocus(Grid),
    ignore;

%% Menu Events
handle_event(#wx{userData=Data, 
		 event=_Cmd=#wxCommand{type=command_menu_selected}},
	     _WinInfo) ->
    Data;
handle_event(_Event, _WinInfo) ->
    ignore.

%%====================================================================
%% Internal functions
%%====================================================================
   

