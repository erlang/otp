%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2010. All Rights Reserved.
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
-module(dbg_ui_trace_win).

%% External exports
-export([init/0]).
-export([create_win/4, get_window/1,
	 configure/2,
	 enable/2, is_enabled/1, select/2,
	 add_break/3, update_break/2, delete_break/2,
	 clear_breaks/1, clear_breaks/2,
	 display/1,                                   % Help messages
	 is_shown/2,                                  % Code area
	 show_code/3, show_no_code/1, remove_code/2,      
	 mark_line/3, unmark_line/1,
	 select_line/2, selected_line/1,
	 eval_output/2,                               % Evaluator area
	 update_bindings/1,                           % Bindings area
	 trace_output/1,                              % Trace area
	 handle_event/2
	]).
-export([helpwin/4,                                   % Help windows
	 helpwin/5]).

-record(breakInfo, {point, status, break}).
-record(winInfo, {window,          % gsobj()
		  size,            % {W, H}
		  flags,           % {F,F,F,F} F = open|close

		  marked_line=0,   % integer() Current line
		  selected_line=0, % integer() Selected line

		  breaks=[],       % [#breakInfo{}] Known breakpoints

		  editor,          % {Mod, Editor}  Visible code editor
		  editors=[]       % [{Mod,Editor}] Code editors
		 }).


%%====================================================================
%% External exports
%%====================================================================

%%--------------------------------------------------------------------
%% init() -> GS
%%   GS = term()
%%--------------------------------------------------------------------
init() ->
    dbg_ui_win:init().

%%--------------------------------------------------------------------
%% create_win(GS, Title, TraceWin, Menus) -> #winInfo{}
%%  GS = gsobj()
%%  Title = string()
%%  TraceWin = [WinArea]
%%    WinArea = 'Button|Evaluator|Bindings|Trace Area'
%%  Menus = [menu()]  See dbg_ui_win.erl
%%--------------------------------------------------------------------
create_win(GS, Title, TraceWin, Menus) ->
    Bu = flip(lists:member('Button Area', TraceWin)),
    Ev = flip(lists:member('Evaluator Area', TraceWin)),
    Bi = flip(lists:member('Bindings Area', TraceWin)),
    Tr = flip(lists:member('Trace Area', TraceWin)),
    
    Win = gs:window(trace_window, GS, [{title, Title},
				       {width, 550},
				       {configure,true}, {destroy,true},
				       {keypress,true}, {motion,true}]),

    MenuBar = gs:menubar(Win, []),
    dbg_ui_win:create_menus(MenuBar, Menus),
    dbg_ui_winman:windows_menu(MenuBar),

    FrameOpts = [{anchor,nw}, {relief,raised}, {bw,2}, {keypress,true}],
    Editor = code_area(2, 25, FrameOpts, Win),
    button_area(Bu, 2, 235, FrameOpts, Win),
    eval_area({Ev,Bi}, 2, 265, FrameOpts, Win),
    bind_area({Ev,Bi}, 300, 265, FrameOpts, Win),
    trace_area(Tr, 2, 475, FrameOpts, Win),

    Flags = {Bu, Ev, Bi, Tr},
    resizebar(rb1(Flags), 'RB1',   2, 225, 710,  10, Win),
    resizebar(rb2(Flags), 'RB2',   2, 465, 710,  10, Win),
    resizebar(rb3(Flags), 'RB3', 290, 265,  10, 200, Win),
    config_v(),
    config_h(),
    
    gs:config(Win,{height,
		   25 +
		   gs:read('CodeArea', height) +
		   gs:read('RB1', height) +
		   gs:read('ButtonArea', height) +
		   erlang:max(gs:read('EvalArea', height),
		       gs:read('BindArea', height)) +
		   gs:read('RB2', height) +
		   gs:read('TraceArea', height)}),
    
    gs:config(Win, {map, true}),
    #winInfo{window=Win, size={gs:read(Win,width), gs:read(Win,height)},
	     flags=Flags,
	     editor={'$top', Editor}, editors=[{'$top', Editor}]}.

flip(true) -> open;
flip(false) -> close.
    
%%--------------------------------------------------------------------
%% get_window(WinInfo) -> Window
%%   WinInfo = #winInfo{}
%%   Window = gsobj()
%%--------------------------------------------------------------------
get_window(WinInfo) ->
    WinInfo#winInfo.window.

%%--------------------------------------------------------------------
%% configure(WinInfo, TraceWin) -> WinInfo
%%   WinInfo = #winInfo{}
%%  TraceWin = [WinArea]
%%    WinArea = 'Button|Evaluator|Bindings|Trace Area'
%% Window areas should be opened or closed.
%%--------------------------------------------------------------------
configure(WinInfo, TraceWin) ->
    {Bu1, Ev1, Bi1, Tr1} = OldFlags = WinInfo#winInfo.flags,
    Bu2 = flip(lists:member('Button Area', TraceWin)),
    Ev2 = flip(lists:member('Evaluator Area', TraceWin)),
    Bi2 = flip(lists:member('Bindings Area', TraceWin)),
    Tr2 = flip(lists:member('Trace Area', TraceWin)),
    NewFlags = {Bu2, Ev2, Bi2, Tr2},

    Win = WinInfo#winInfo.window,
    W = gs:read(Win, width),
    H = gs:read(Win, height),

    H2 = if
	     Bu1==close, Bu2==open ->
		 resize_button_area(open, width, W-4),
		 gs:config('ButtonArea', {height, 30}),
		 H+30;
	     Bu1==open, Bu2==close ->
		 gs:config('ButtonArea', [{width, 0}, {height, 0}]),
		 H-30;
	     true -> H
	 end,
    H3 = if
	     Ev1==close, Ev2==open, Bi1==open ->
		 Wnew1 = round((W-10-4)/2), % W = window/2 - rb - pads
		 Hbi1 = gs:read('BindArea', height), % H = bind area h
		 resize_eval_area(open, width, Wnew1),
		 resize_eval_area(open, height, Hbi1),
		 gs:config('RB3', {width, 10}),
		 gs:config('RB3', {height, Hbi1}),
		 resize_bind_area(open, width,
				  Wnew1-gs:read('BindArea', width)),
		 H2;
	     Ev1==close, Ev2==open, Bi1==close ->
		 resize_eval_area(open, width, W-4),
		 resize_eval_area(open, height, 200),
		 H2+200;
	     Ev1==open, Ev2==close, Bi1==open ->
		 gs:config('EvalArea', [{width,0}, {height,0}]),
		 gs:config('RB3', [{width, 0}, {height, 0}]),
		 Wnew2 = W-4,
		 resize_bind_area(open, width,
				  Wnew2-gs:read('BindArea', width)),
		 H2;
	     Ev1==open, Ev2==close, Bi1==close ->
		 Hs1 = gs:read('EvalArea', height),
		 gs:config('EvalArea', [{width, 0}, {height, 0}]),
		 H2-Hs1;
	     true -> H2
	 end,
    H4 = if
	     Bi1==close, Bi2==open, Ev2==open ->
		 Wnew3 = round((W-10-4)/2), % W = window/2 - rb - pads
		 Hs2 = gs:read('EvalArea', height), % H = eval area h
		 resize_bind_area(open, width, Wnew3),
		 resize_bind_area(open, height, Hs2),
		 gs:config('RB3', [{width,10},{height,Hs2}]),
		 resize_eval_area(open, width,
				  Wnew3-gs:read('EvalArea', width)),
		 H3;
	     Bi1==close, Bi2==open, Ev2==close ->
		 resize_bind_area(open, width, W-4),
		 resize_bind_area(open, height, 200),
		 H3+200;
	     Bi1==open, Bi2==close, Ev2==open ->
		 gs:config('BindArea', [{width, 0}, {height, 0}]),
		 gs:config('RB3', [{width, 0}, {height, 0}]),
		 Wnew4 = W-4,
		 resize_eval_area(open, width,
				  Wnew4-gs:read('EvalArea', width)),
		 H3;
	     Bi1==open, Bi2==close, Ev2==close ->
		 Hbi2 = gs:read('BindArea', height),
		 gs:config('BindArea', [{width, 0}, {height, 0}]),
		 H3-Hbi2;
	     true -> H3
	 end,
    H5 = if
	     Tr1==close, Tr2==open ->
		 resize_trace_area(open, width, W-4),
		 resize_trace_area(open, height, 200),
		 H4+200;
	     Tr1==open, Tr2==close ->
		 Hf = gs:read('TraceArea', height),
		 gs:config('TraceArea', [{width, 0}, {height, 0}]),
		 H4-Hf;
	     true -> H4
	 end,
    gs:config(Win, {height, H5}),

    RB1old = rb1(OldFlags), RB1new = rb1(NewFlags),
    if
	RB1old==close, RB1new==open ->
	    gs:config('RB1', [{width, W-4}, {height, 10}]),
	    gs:config(Win, {height, gs:read(Win, height)+10});
	RB1old==open, RB1new==close ->
	    gs:config('RB1', [{width, 0}, {height, 0}, lower]),
	    gs:config(Win, {height, gs:read(Win, height)-10});
	true -> ignore
    end,

    RB2old = rb2(OldFlags), RB2new = rb2(NewFlags),
    if
	RB2old==close, RB2new==open ->
	    gs:config('RB2', [{width, W-4}, {height, 10}]),
	    gs:config(Win, {height,gs:read(Win, height)+10});
	RB2old==open, RB2new==close ->		
	    gs:config('RB2', [{width, 0}, {height, 0}, lower]),
	    gs:config(Win, {height, gs:read(Win, height)-10});
	true -> ignore
    end,
    config_v(),
    config_h(),

    flush_configure(),
    
    WinInfo#winInfo{size={gs:read(Win, width), gs:read(Win, height)},
		    flags=NewFlags}.

flush_configure() ->
    receive
	{gs, _Id, configure, _Data, _Arg} ->
	    flush_configure()
    after 100 ->
	    true
    end.

%%--------------------------------------------------------------------
%% enable([MenuItem], Bool)
%% is_enabled(MenuItem) -> Bool
%%   MenuItem = atom()
%%   Bool = boolean()
%%--------------------------------------------------------------------
enable(MenuItems, Bool) ->
    lists:foreach(fun(MenuItem) ->
			  gs:config(MenuItem, {enable, Bool}),
			  case is_button(MenuItem) of
			      {true, Button} ->
				  gs:config(Button, {enable, Bool});
			      false -> ignore
			  end
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
%% add_break(WinInfo, Name, {Point, Options}) -> WinInfo
%%   WinInfo = #winInfo{}
%%   Name = atom() Menu name
%%   Point = {Mod, Line}
%%   Options = [Status, Action, Mods, Cond]
%%     Status = active | inactive
%%     Action = enable | disable | delete
%%     Mods = null (not used)
%%     Cond = null | {Mod, Func}
%%--------------------------------------------------------------------
add_break(WinInfo, Menu, {{Mod,Line},[Status|_Options]}=Break) ->
    case lists:keysearch(Mod, 1, WinInfo#winInfo.editors) of
	{value, {Mod, Editor}} ->
	    add_break_to_code(Editor, Line, Status);
	false -> ignore
    end,
    add_break_to_menu(WinInfo, Menu, Break).

add_break_to_code(Editor, Line, Status) ->
    Color = if Status==active -> red; Status==inactive -> blue end,
    config_editor(Editor, [{overwrite,{{Line,0},"-@-  "}},
			   {fg,{{{Line,0},{Line,lineend}}, Color}}]).

add_break_to_menu(WinInfo, Menu, {Point, [Status|_Options]=Options}) ->
    Break = dbg_ui_win:add_break(Menu, Point),
    dbg_ui_win:update_break(Break, Options),
    BreakInfo = #breakInfo{point=Point, status=Status, break=Break},
    WinInfo#winInfo{breaks=[BreakInfo|WinInfo#winInfo.breaks]}.

%%--------------------------------------------------------------------
%% update_break(WinInfo, {Point, Options}) -> WinInfo
%%   WinInfo = #winInfo{}
%%   Point = {Mod, Line}
%%   Options = [Status, Action, Mods, Cond]
%%     Status = active | inactive
%%     Action = enable | disable | delete
%%     Mods = null (not used)
%%     Cond = null | {Mod, Func}
%%--------------------------------------------------------------------
update_break(WinInfo, {{Mod,Line},[Status|_Options]}=Break) ->
    case lists:keysearch(Mod, 1, WinInfo#winInfo.editors) of
	{value, {Mod, Editor}} ->
	    add_break_to_code(Editor, Line, Status);
	false -> ignore
    end,
    update_break_in_menu(WinInfo, Break).

update_break_in_menu(WinInfo, {Point, [Status|_Options]=Options}) ->
    {value, BreakInfo} = lists:keysearch(Point, #breakInfo.point,
					 WinInfo#winInfo.breaks),
    dbg_ui_win:update_break(BreakInfo#breakInfo.break, Options),
    BreakInfo2 = BreakInfo#breakInfo{status=Status},
    WinInfo#winInfo{breaks=lists:keyreplace(Point, #breakInfo.point,
					    WinInfo#winInfo.breaks,
					    BreakInfo2)}.

%%--------------------------------------------------------------------
%% delete_break(WinInfo, Point) -> WinInfo
%%   WinInfo = #winInfo{}
%%   Point = {Mod, Line}
%%--------------------------------------------------------------------
delete_break(WinInfo, {Mod,Line}=Point) ->
    case lists:keysearch(Mod, 1, WinInfo#winInfo.editors) of
	{value, {Mod, Editor}} -> delete_break_from_code(Editor, Line);
	false -> ignore
    end,
    delete_break_from_menu(WinInfo, Point).

delete_break_from_code(Editor, Line) ->
    Prefix = string:substr(integer_to_list(Line)++":   ", 1, 5),
    config_editor(Editor, [{overwrite,{{Line,0},Prefix}},
			   {fg,{{{Line,0},{Line,lineend}}, black}}]).

delete_break_from_menu(WinInfo, Point) ->
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
    clear_breaks(WinInfo, all).
clear_breaks(WinInfo, Mod) ->
    Remove = if
		 Mod==all -> WinInfo#winInfo.breaks;
		 true ->
		     lists:filter(fun(#breakInfo{point={Mod2,_L}}) ->
					  if
					      Mod2==Mod -> true;
					      true -> false
					  end
				  end,
				  WinInfo#winInfo.breaks)
	     end,
    lists:foreach(fun(#breakInfo{point=Point}) ->
			  delete_break(WinInfo, Point)
		  end,
		  Remove),
    Remain = WinInfo#winInfo.breaks -- Remove,
    WinInfo#winInfo{breaks=Remain}.

%%--------------------------------------------------------------------
%% display(Arg)
%%   Arg = idle | {Status,Mod,Line} | {running,Mod}
%%       ¦ {exit,Where,Reason} | {text,Text}
%%     Status = break | wait ¦ Level
%%       Level = int()
%%     Mod = atom()
%%     Line = integer()
%%     Where = {Mod,Line} | null
%%     Reason = term()
%%     Text = string()
%%--------------------------------------------------------------------
display(Arg) ->
    Str = case Arg of
	      idle -> "State: uninterpreted";
	      {exit, {Mod,Line}, Reason} ->
		  gs:config(trace_window, raise),
		  io_lib:format("State: EXITED [~w.erl/~w], Reason:~w",
				[Mod, Line, Reason]);
	      {exit, null, Reason} ->
		  gs:config(trace_window, raise),
		  io_lib:format("State: EXITED [uninterpreted], "
				"Reason:~w", [Reason]);
	      {Level, null, _Line} when is_integer(Level) ->
		  io_lib:format("*** Call level #~w "
				"(in non-interpreted code)",
				[Level]);
	      {Level, Mod, Line} when is_integer(Level) ->
		  io_lib:format("*** Call level #~w [~w.erl/~w]",
				[Level, Mod, Line]);
	      {Status, Mod, Line} ->
		  What = case Status of
			     wait -> 'receive';
			     _ -> Status
			 end,
		  io_lib:format("State: ~w [~w.erl/~w]",
				[What, Mod, Line]);
	      {running, Mod} ->
		  io_lib:format("State: running [~w.erl]", [Mod]);
	      {text, Text}  -> Text
	  end,
    gs:config(info_window, {label,{text,lists:flatten(Str)}}).

%%--------------------------------------------------------------------
%% is_shown(WinInfo, Mod) -> {true, WinInfo} | false
%% show_code(WinInfo, Mod, Contents) -> WinInfo
%% show_no_code(WinInfo) -> WinInfo
%% remove_code(WinInfo, Mod) -> WinInfo
%%   WinInfo = #winInfo{}
%%   Mod = atom()
%%   Contents = string()
%% Note: remove_code/2 should not be used for currently shown module.
%%--------------------------------------------------------------------
is_shown(WinInfo, Mod) ->
    case lists:keysearch(Mod, 1, WinInfo#winInfo.editors) of
	{value, {Mod, Editor}} ->
	    gs:config(Editor, raise),
	    {true, WinInfo#winInfo{editor={Mod, Editor}}};
	false -> false
    end.

show_code(WinInfo, Mod, Contents) ->
    Editors = WinInfo#winInfo.editors,
    {Flag, Editor} = case lists:keysearch(Mod, 1, Editors) of
			 {value, {Mod, Ed}} -> {existing, Ed};
			 false -> {new, code_editor()}
		     end,

    %% Insert code and update breakpoints, if any
    config_editor(Editor, [raise, clear]),
    show_code(Editor, Contents),
    lists:foreach(fun(BreakInfo) ->
			  case BreakInfo#breakInfo.point of
			      {Mod2, Line} when Mod2==Mod ->
				  Status = BreakInfo#breakInfo.status,
				  add_break_to_code(Editor, Line,Status);
			      _Point -> ignore
			  end
		  end,
		  WinInfo#winInfo.breaks),

    case Flag of
	existing ->
	    WinInfo#winInfo{editor={Mod, Editor}};
	new ->
	    WinInfo#winInfo{editor={Mod, Editor},
			    editors=[{Mod, Editor} | Editors]}
    end.
	
show_code(Editor, Text) when length(Text)>1500 ->
    %% Add some text at a time so that other processes may get scheduled
    Str = string:sub_string(Text, 1, 1500),
    config_editor(Editor, {insert,{'end', Str}}),
    show_code(Editor, string:sub_string(Text, 1501));
show_code(Editor, Text) ->
    config_editor(Editor, {insert,{'end',Text}}).

show_no_code(WinInfo) ->
    {value, {'$top', Editor}} =
	lists:keysearch('$top', 1, WinInfo#winInfo.editors),
    gs:config(Editor, raise),
    WinInfo#winInfo{editor={'$top', Editor}}.

remove_code(WinInfo, Mod) ->
    Editors = WinInfo#winInfo.editors,
    case lists:keysearch(Mod, 1, Editors) of
	{value, {Mod, Editor}} ->
	    gs:destroy(Editor),
	    WinInfo#winInfo{editors=lists:keydelete(Mod, 1, Editors)};
	false ->
	    WinInfo
    end.
    

%%--------------------------------------------------------------------
%% mark_line(WinInfo, Line, How) -> WinInfo
%%   WinInfo = #winInfo{}
%%   Line = integer()
%%   How = break | where
%% Mark the code line where the process is executing.
%%--------------------------------------------------------------------
mark_line(WinInfo, Line, How) ->
    {_Mod, Editor} = WinInfo#winInfo.editor,
    mark_line2(Editor, WinInfo#winInfo.marked_line, false),
    mark_line2(Editor, Line, How),
    if
	Line/=0 -> config_editor(Editor, {vscrollpos, Line-5});
	true -> ignore
    end,
    WinInfo#winInfo{marked_line=Line}.

unmark_line(WinInfo) ->
    mark_line(WinInfo, 0, false).

mark_line2(Editor, Line, How) ->
    Prefix = case How of
		 break -> "-->";
		 where -> ">>>";
		 false -> "   "
	     end,
    Font = if
	       How==false -> dbg_ui_win:font(normal);
	       true -> dbg_ui_win:font(bold)
	   end,
    config_editor(Editor, [{overwrite, {{Line,5}, Prefix}},
			   {font_style,
			    {{{Line,0},{Line,lineend}}, Font}}]).

%%--------------------------------------------------------------------
%% select_line(WinInfo, Line) -> WinInfo
%% selected_line(WinInfo) -> undefined | Line
%%   WinInfo = #winInfo{}
%%   Line = integer()
%% Select/unselect a line (unselect if Line=0).
%%--------------------------------------------------------------------
select_line(WinInfo, Line) ->
    {_Mod, Editor} = WinInfo#winInfo.editor,

    %% Since 'Line' may be specified by the user in the 'Go To Line'
    %% help window, it must be checked that it is correct
    Size = gs:read(Editor, size),
    if
	Line==0 ->
	    select_line(Editor, WinInfo#winInfo.selected_line, false),
	    WinInfo#winInfo{selected_line=0};
	Line<Size ->
	    select_line(Editor, Line, true),
	    config_editor(Editor, {vscrollpos, Line-5}),
	    WinInfo#winInfo{selected_line=Line};
	true ->
	    WinInfo
    end.

select_line(Editor, Line, true) ->
    config_editor(Editor, {selection, {{Line,0}, {Line,lineend}}});
select_line(Editor, _Line, false) ->
    config_editor(Editor, {selection, {{1,0}, {1,0}}}).

selected_line(WinInfo) ->
    case WinInfo#winInfo.selected_line of
	0 -> undefined;
	Line -> Line
    end.

%%--------------------------------------------------------------------
%% eval_output(Str, Face)
%%   Str = string()
%%   Face = normal | bold
%%--------------------------------------------------------------------
eval_output(Text, Face) ->
    Y1 = gs:read('EvalEditor', size),
    config_editor('EvalEditor', {insert,{'end',Text}}),
    Y2 = gs:read('EvalEditor', size),

    Font = dbg_ui_win:font(Face),
    config_editor('EvalEditor',
		  [{font_style, {{{Y1,0},{Y2,lineend}}, Font}},
		   {vscrollpos,Y2}]).

%%--------------------------------------------------------------------
%% update_bindings(Bs)
%%   Bs = [{Var,Val}]
%%--------------------------------------------------------------------
update_bindings(Bs) ->
    gs:config('BindGrid', {rows, {1,length(Bs)+1}}),
    Font = dbg_ui_win:font(normal),
    Last =
	lists:foldl(fun({Var, Val}, Row) ->
			    Opts = [{text, {1,atom_to_list(Var)}},
				    {text, {2,io_lib:format("~P",
							    [Val, 4])}},
				    {doubleclick, true},
				    {data, {binding,{Var,Val}}}],
			    case gs:read('BindGrid',{obj_at_row,Row}) of
				undefined ->
				    gs:gridline('BindGrid',
						[{row, Row},
						 {height, 14},
						 {font, Font} | Opts]);
				GridLine ->
				    gs:config(GridLine, Opts)
			    end,
			    Row+1
		    end,
		    2,
		    Bs),
    delete_gridlines(Last).

delete_gridlines(Row) -> 
    case gs:read('BindGrid', {obj_at_row, Row}) of
	undefined -> true;
	GridLine ->
	    gs:destroy(GridLine),
	    delete_gridlines(Row+1)
    end.

%%--------------------------------------------------------------------
%% trace_output(Str)
%%   Str = string()
%%--------------------------------------------------------------------
trace_output(Str) ->
    Font = dbg_ui_win:font(normal),
    config_editor('TraceEditor',
		  [{insert, {'end',Str}},
		   {fg, {{{1,0},'end'},black}},
		   {font_style, {{{1,0},'end'},Font}}]),
    Max = gs:read('TraceEditor', size),
    config_editor('TraceEditor', {vscrollpos, Max}).
    
%%--------------------------------------------------------------------
%% handle_event(GSEvent, WinInfo) -> Command
%%   GSEvent = {gs, Id, Event, Data, Arg}
%%   WinInfo = #winInfo{}
%%   Command = ignore
%%           | {win, WinInfo}
%%           | stopped
%%           | {coords, {X,Y}}
%%
%%           | {shortcut, Key}
%%           | MenuItem | {Menu, [MenuItem]}
%%               MenuItem = Menu = atom()
%%           | {break, Point, What}
%%               What = add | delete | {status,Status} |{trigger,Trigger}
%%           | {module, Mod, view}
%%
%%           | {user_command, Cmd}
%%
%%           | {edit, {Var, Val}}
%%--------------------------------------------------------------------
%% Window events
handle_event({gs, _Id, configure, _Data, [W, H|_]}, WinInfo) ->
    case WinInfo#winInfo.size of
	{W, H} -> ignore;
	_Size ->
	    configure(WinInfo, W, H),
	    {win, WinInfo#winInfo{size={W, H}}}
    end;
handle_event({gs, _Id, destroy, _Data, _Arg}, _WinInfo) ->
    stopped;
handle_event({gs, _Id, motion, _Data, [X,Y]}, WinInfo) ->
    {LastX, LastY} = dbg_ui_win:motion(X, Y),
    Win = WinInfo#winInfo.window,
    {coords, {gs:read(Win, x)+LastX-5, gs:read(Win, y)+LastY-5}};
handle_event({gs, RB, buttonpress, resizebar, _Arg}, WinInfo) ->
    resize(WinInfo, RB), % Resize window contents
    ignore;

%% Menus, buttons and keyboard shortcuts
handle_event({gs, _Id, keypress, _Data, [Key,_,_,1]}, _WinInfo) ->
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
handle_event({gs, _Id, click, {module, Mod, view}, _Arg}, _WinInfo) ->
    {module, Mod, view};

%% Code area
handle_event({gs, Editor, buttonpress, code_editor, _Arg}, WinInfo) ->
    {Row, _Col} = gs:read(Editor, insertpos),
    Again = receive
		{gs, Editor, buttonpress, code_editor, _} ->
		    gs:read(Editor, insertpos)
	    after 500 ->
		    false
	    end,
    case Again of
	{Row, _} ->
	    {Mod, _Editor} = WinInfo#winInfo.editor,
	    Point = {Mod, Row},
	    case lists:keysearch(Point, #breakInfo.point,
				 WinInfo#winInfo.breaks) of
		{value, _BreakInfo} -> {break, Point, delete};
		false -> {break, Point, add}
	    end;
	{Row2, _} ->
	    select_line(Editor, Row2, true),
	    {win, WinInfo#winInfo{selected_line=Row2}};
	false ->
	    select_line(Editor, Row, true),
	    {win, WinInfo#winInfo{selected_line=Row}}
    end;

%% Button area
handle_event({gs, _Id, click, {button, Name}, _Arg}, _WinInfo) ->
    Name;

%% Evaluator area
handle_event({gs, 'EvalEntry', keypress, _Data, ['Return'|_]}, _WI) ->
    Command = case gs:read('EvalEntry', text) of
		  [10] ->
		      eval_output("\n", normal),
		      ignore;
		  Cmd ->
		      eval_output([$>, Cmd, 10], normal),
		      {user_command, Cmd}
	      end,
    gs:config('EvalEntry', [{text,""}, {focus,false}]),
    Command;

%% Bindings area
handle_event({gs, _Id, click, {binding, {Var, Val}}, _Arg}, _WinInfo) ->
    Str = io_lib:format("< ~p = ~p~n", [Var, Val]),
    eval_output(Str, bold),
    ignore;
handle_event({gs, _Id, doubleclick, {binding, B}, _Arg}, _WinInfo) ->
    {edit, B};

handle_event(_GSEvent, _WinInfo) ->
    ignore.


%%====================================================================
%% Internal functions
%%====================================================================

%%--Code Area---------------------------------------------------------

code_area(X, Y, FrameOpts, Win) ->
    gs:frame('CodeArea', Win,
	     [{x,X}, {y,Y}, {width,546}, {height,400} | FrameOpts]),
    gs:label(info_window, 'CodeArea',
	     [{label,{text,""}}, {font,dbg_ui_win:font(normal)},
	      {x,5}, {y,10}, {width,406}, {height,15},
	      {anchor,nw}, {align,w}]),
    code_editor('CodeEditor', 536, 365).

code_editor() ->
    W = gs:read('CodeEditor', width),
    H = gs:read('CodeEditor', height),
    code_editor(null, W, H).

code_editor(Name, W, H) ->
    Editor = if
		 Name==null -> gs:editor('CodeArea', []);
		 true -> gs:editor(Name, 'CodeArea', [])
	     end,
    gs:config(Editor, [{x,5}, {y,30}, {width,W}, {height,H},
		       {keypress,false}, {buttonpress,true}, 
		       {data,code_editor}]),
    config_editor(Editor, [{vscroll,right}, {hscroll,bottom}]),
    Font = dbg_ui_win:font(normal),
    config_editor(Editor, [{wrap,none}, {fg,{{{1,0},'end'},black}},
			   {font, Font},
			   {font_style, {{{1,0},'end'},Font}}]),
    Editor.

resize_code_area(WinInfo, Key, Diff) ->
    gs:config('CodeArea', {Key,gs:read('CodeArea', Key)+Diff}),
    case Key of
	width ->
	    gs:config(info_window, {Key,gs:read(info_window,Key)+Diff});
	height -> ignore
    end,

    %% Resize all code editors
    Value = gs:read('CodeEditor', Key)+Diff,
    gs:config('CodeEditor', {Key,Value}),
    Editors = WinInfo#winInfo.editors,
    lists:foreach(fun({_Mod, Editor}) ->
			  gs:config(Editor, {Key,Value})
		  end,
		  Editors).

%%--Button Area-------------------------------------------------------

buttons() ->
    [{'Step','StepButton'}, {'Next','NextButton'},
     {'Continue','ContinueButton'}, {'Finish','FinishButton'},
     {'Where','WhereButton'}, {'Up','UpButton'}, {'Down','DownButton'}].

is_button(Name) ->
    case lists:keysearch(Name, 1, buttons()) of
	{value, {Name, Button}} -> {true, Button};
	false -> false
    end.

button_area(Bu, X, Y, FrameOpts, Win) ->
    {W,H} = case Bu of
		open -> {546,30};
		close -> {0,0}
	    end,
    gs:frame('ButtonArea', Win,
	     [{x,X}, {y,Y}, {width,W}, {height,H} | FrameOpts]),
    Font = dbg_ui_win:font(normal),
    lists:foldl(fun({Name, Button}, Xb) ->
			gs:button(Button, 'ButtonArea',
				  [{label, {text,Name}}, {font,Font},
				   {x, Xb}, {y, 1},
				   {width, 77}, {height, 24},
				   {data, {button,Name}}]),
			Xb+78
		end,
		1,
		buttons()).

resize_button_area(close, width, _Diff) ->
    ignore;
resize_button_area(open, width, Diff) ->
    gs:config('ButtonArea', {width, gs:read('ButtonArea', width)+Diff}).

%%--Evaluator Area----------------------------------------------------

eval_area({Ev,Bi}, X, Y, FrameOpts, Win) ->
    {W,H} = if
		Ev==open -> {289,200};
		true -> {0,0}
	    end,
    Font = dbg_ui_win:font(normal),
    gs:frame('EvalArea', Win,
	     [{x,X}, {y,Y}, {width,W}, {height,H} | FrameOpts]),
    gs:label('EvalArea',
	     [{label,{text,"Evaluator:"}}, {font, Font},
	      {x,5}, {y,35}, {width,80}, {height,25},
	      {anchor,sw}, {align,center}]),
    gs:entry('EvalEntry', 'EvalArea',
	     [{font, Font},
	      {x,80}, {y,35}, {width,185}, {height,25},
	      {anchor,sw}, {keypress,true}]),
    gs:editor('EvalEditor', 'EvalArea',
	      [{x,5}, {y,35}, {width, 280}, {height, 160},
	       {keypress,false},
	       {vscroll,right}, {hscroll,bottom},
	       {wrap,none}, {fg,{{{1,0},'end'},black}},
	       {font, Font},
	       {font_style,{{{1,0},'end'},Font}}]),
    gs:config('EvalEditor', {enable, false}),
    if
	Ev==open, Bi==close -> resize_eval_area(Ev, width, 257);
	true -> ignore
    end.

resize_eval_area(close, _Key, _Diff) ->
    ignore;
resize_eval_area(open, Key, Diff) ->
    New = gs:read('EvalArea', Key)+Diff,
    gs:config('EvalArea', {Key,New}),
    case Key of
	width ->
	    gs:config('EvalEntry', {width,New-104}),
	    gs:config('EvalEditor', {width,New-9});
	height ->
	    gs:config('EvalEditor', {height,New-40})
    end.

%%--Bindings Area-----------------------------------------------------

bind_area({Ev,Bi}, X, Y, FrameOpts, Win) ->
    {W,H} = if
		Bi==open -> {249,200};
		true -> {0,0}
	    end,
    gs:frame('BindArea', Win,
	     [{x,X}, {y,Y}, {width,W}, {height,H} | FrameOpts]),

    Font = dbg_ui_win:font(bold),
    gs:grid('BindGrid', 'BindArea',
	    [{x,2}, {y,2}, {height,193}, {width,241},
	     {fg,black}, {vscroll,right}, {hscroll,bottom},
	     {font,Font},
	     calc_columnwidths(241), {rows, {1,50}}]), 
    gs:gridline('BindGrid',
		[{row,1}, {height,14}, {fg,blue},
		 {text,{1,"Name"}}, {text,{2,"Value"}}, {font,Font}]),
    gs:config('BindGrid', {rows,{1,1}}),
    if
	Bi==open, Ev==close -> resize_bind_area(Bi, width, 297);
	true -> ignore
    end.

resize_bind_area(close, _Key, _Diff) ->
    ignore;
resize_bind_area(open, Key, Diff) ->
    New = gs:read('BindArea', Key)+Diff,
    gs:config('BindArea', {Key,New}),
    case Key of
	width ->
	    gs:config('BindGrid', {width,New-8}),
	    Cols = calc_columnwidths(New-8),
	    gs:config('BindGrid', Cols);
	height ->
	    gs:config('BindGrid', {height,New-7})
    end.

calc_columnwidths(Width) ->    
    if Width =< 291 -> 
	    {columnwidths,[90,198]};
       true -> 
	    S = (Width)/(90+198),
	    {columnwidths,[round(90*S),round(198*S)]}
    end.

%%--Trace Area--------------------------------------------------------

trace_area(Tr, X, Y, FrameOpts, Win) ->
    {W,H} = case Tr of
		open -> {546,200};
		close -> {0,0}
	    end,
    gs:frame('TraceArea', Win,
	     [{x,X}, {y,Y}, {width,W}, {height,H} | FrameOpts]),
    Editor = gs:editor('TraceEditor', 'TraceArea',
		       [{x,5}, {y,5}, {width,536}, {height,190},
			{keypress,false}]),
    Font = dbg_ui_win:font(normal),
    config_editor(Editor,
		  [{vscroll,right}, {hscroll,bottom},
		   {wrap,none},{fg,{{{1,0},'end'},black}},
		   {font, Font},
		   {font_style,{{{1,0},'end'},Font}}]).

resize_trace_area(close, _Key, _Diff) ->
    ignore;
resize_trace_area(open, Key, Diff) ->
    New = gs:read('TraceArea', Key)+Diff,
    gs:config('TraceArea', {Key,New}),
    gs:config('TraceEditor', {Key,New-10}).

%%--Editors-----------------------------------------------------------

config_editor(Editor, Opts) ->
    gs:config(Editor, {enable,true}),
    gs:config(Editor, Opts),
    gs:config(Editor, {enable,false}).

%%--Resize Bars-------------------------------------------------------
%% The resize bars are used to resize the areas within the window.

%%--------------------------------------------------------------------
%% resizebar(Flag, Name, X, Y, W, H, Obj) -> resizebar()
%%   Flag = open | close
%%   Name = atom()
%%   X = Y = integer()  Coordinates relative to Obj
%%   W = H = integer()  Width and height
%%   Obj = gsobj()
%% Creates a 'resize bar', a frame object over which the cursor will
%% be of the 'resize' type.
%%--------------------------------------------------------------------
resizebar(Flag, Name, X, Y, W, H, Obj) ->
    {W2,H2} = case Flag of
		  open  -> {W,H};
		  close -> {0,0}
	      end,
    gs:create(frame, Name, Obj, [{x,X}, {y,Y}, {width,W2}, {height,H2},
				 {bw,2},
				 {cursor,resize},
				 {buttonpress,true}, {buttonrelease,true},
				 {data,resizebar}]).

rb1({_Bu,Ev,Bi,Tr}) ->
    if
	Ev==close, Bi==close, Tr==close -> close;
	true -> open
    end.
    
rb2({_Bu,Ev,Bi,Tr}) ->
    if
	Tr==open ->
	    if
		Ev==close, Bi==close -> close;
		true -> open
	    end;
	true -> close
    end.
    
rb3({_Bu,Ev,Bi,_Tr}) ->
    if
	Ev==open, Bi==open -> open;
	true -> close
    end.

%%--Configuration-----------------------------------------------------
%% Resize the window as well as its contents

%%--------------------------------------------------------------------
%% config_v()
%% Reconfigure the window vertically.
%%--------------------------------------------------------------------
config_v() ->
    Y1 = 25+gs:read('CodeArea', height),
    gs:config('RB1', {y,Y1}),
    
    Y2 = Y1+gs:read('RB1', height),
    gs:config('ButtonArea', {y,Y2}),
    
    Y3 = Y2+gs:read('ButtonArea', height),
    gs:config('EvalArea', {y,Y3}),
    gs:config('RB3', {y,Y3}),
    gs:config('BindArea', {y,Y3}),
    
    Y4 = Y3 + erlang:max(gs:read('EvalArea', height),
		  gs:read('BindArea', height)),
    gs:config('RB2', {y,Y4}),
    
    Y5 = Y4 + gs:read('RB2', height),
    gs:config('TraceArea', {y,Y5}).

%%--------------------------------------------------------------------
%% config_h()
%% Reconfigure the window horizontally.
%%--------------------------------------------------------------------
config_h() ->
    X1 = 2+gs:read('EvalArea', width),
    gs:config('RB3', {x,X1}),
    
    X2 = X1+gs:read('RB3', width),
    gs:config('BindArea', {x,X2}).

%%--------------------------------------------------------------------
%% configure(WinInfo, W, H)
%% The window has been resized, now its contents must be resized too.
%%--------------------------------------------------------------------
configure(WinInfo, NewW, NewH) ->
    {Bu,Ev,Bi,Tr} = Flags = WinInfo#winInfo.flags,
    
    OldW = gs:read('CodeArea', width)+4,
    OldH = 25+gs:read('CodeArea', height)+
	gs:read('RB1', height)+
	gs:read('ButtonArea', height)+
	erlang:max(gs:read('EvalArea', height), gs:read('BindArea', height))+
	gs:read('RB2', height)+
	gs:read('TraceArea', height),

    %% Adjust width unless it is unchanged or less than minimum width
    if
	OldW/=NewW ->
	    {Dcode,Deval,Dbind} = configure_widths(OldW,NewW,Flags),
	    resize_code_area(WinInfo, width, Dcode),
	    case rb1(Flags) of
		open ->
		    gs:config('RB1', {width,gs:read('RB1',width)+Dcode});
		close -> ignore
	    end,
	    resize_button_area(Bu, width, Dcode),
	    resize_eval_area(Ev, width, Deval),
	    resize_bind_area(Bi, width, Dbind),
	    case rb2(Flags) of
		open ->
		    gs:config('RB2', {width,gs:read('RB2',width)+Dcode});
		close -> ignore
	    end,
	    resize_trace_area(Tr, width, Dcode),
	    config_h();
	true -> ignore
    end,
    
    %% Adjust height unless it is unchanged or less than minimum height
    if
	OldH/=NewH ->
	    {Dcode2,Deval2,Dtrace} = configure_heights(OldH,NewH,Flags),
	    resize_code_area(WinInfo, height, Dcode2),
	    resize_eval_area(Ev, height, Deval2),
	    case rb3(Flags) of
		open ->
		    gs:config('RB3',
			      {height,gs:read('RB3',height)+Deval2});
		close -> ignore
	    end,
	    resize_bind_area(Bi, height, Deval2),
	    resize_trace_area(Tr, height, Dtrace),
	    config_v();
	true -> ignore
    end.

%% Compute how much the width of each frame must be increased or
%% decreased in order to adjust to the new window width.
configure_widths(OldW, NewW, Flags) ->
    {_Bu,Ev,Bi,_Tr} = Flags,

    %% Difference between old and new width, considering min window width
    Diff = abs(erlang:max(OldW,330)-erlang:max(NewW,330)),
    
    %% Check how much the frames can be resized in reality
    Limits = if
		 %% Window larger
		 NewW>OldW ->
		     if
			 Ev==open,Bi==open -> {0,Diff,Diff};
			 Ev==open -> {0,Diff,0};
			 Bi==open -> {0,0,Diff};
			 true -> {Diff,0,0}
		     end;
		 
		 %% Window smaller; get difference between min size
		 %% and current size
		 OldW>NewW ->
		     if
			 Ev==open,Bi==open ->
			     {0,
			      gs:read('EvalArea',width)-204,
			      gs:read('BindArea',width)-112};
			 Ev==open -> {0,Diff,0};
			 Bi==open -> {0,0,Diff};
			 true -> {Diff,0,0}
		     end
	     end,
    
    case Limits of

	%% No Shell or Bind frame, larger window
	{T,0,0} when NewW>OldW -> {T,0,0};
	
	%% No Shell or Bind frame, smaller window
	{T,0,0} when OldW>NewW -> {-T,0,0};

	%% Window larger; divide Diff among the frames and return result
	{_,Sf,B} when NewW>OldW ->
	    {_,Sf2,B2} = divide([{0,0},{0,Sf},{0,B}],Diff),
	    {Sf2+B2,Sf2,B2};

	%% Window smaller; divide Diff among the frames and return
	%% the inverted result (the frames should shrink)
	{_,Sf,B} when OldW>NewW ->
	    {_,Sf2,B2} = divide([{0,0},{0,Sf},{0,B}],Diff),
	    {-(Sf2+B2),-Sf2,-B2}
    end.

%% Compute how much the height of each frame must be increased or
%% decreased in order to adjust to the new window height.
configure_heights(OldH, NewH, Flags) ->
    {_Bu,Ev,Bi,Tr} = Flags,

    %% Difference between old and new height, considering min win height
    MinH = min_height(Flags),
    Diff = abs(erlang:max(OldH,MinH)-erlang:max(NewH,MinH)),
    
    %% Check how much the frames can be resized in reality
    {T,Sf,Ff} = if
		  %% Window larger
		  NewH>OldH ->
		      {Diff,
		       if
			   Ev==close, Bi==close -> 0;
			   true -> Diff
		       end,
		       if
			   Tr==open -> Diff;
			   true -> 0
		       end};

		  %% Window smaller; get difference between min size
		  %% and current size
		  OldH>NewH ->
		      {gs:read('CodeArea',height)-100,
		       if
			   Ev==close, Bi==close -> 0;
			   true ->
			       if
				   Ev==open ->
				       gs:read('EvalArea',height)-100;
				   Bi==open ->
				       gs:read('BindArea',height)-100
			       end
		       end,
		       if
			   Tr==open -> gs:read('TraceArea',height)-100;
			   true -> 0
		       end}
	      end,
    
    if
	%% Window larger; divide Diff among the frames and return result
	NewH>OldH -> divide([{0,T},{0,Sf},{0,Ff}],Diff);

	%% Window smaller; divide Diff among the frames and return
	%% the inverted result (the frames should shrink)
	OldH>NewH ->
	    {T2,Sf2,Ff2} = divide([{0,T},{0,Sf},{0,Ff}],Diff),
	    {-T2,-Sf2,-Ff2}
    end.

%% Compute minimum window height
min_height(Flags) ->
    {Bu,S,Bi,F} = Flags,
    H1 = 25 + 100 + 2, % Upper pad + Trace frame + lower pad
    H2 = H1 + bu(Bu) + s_bi(S,Bi) + f(F),
    H3 = case rb1(Flags) of
	     open -> H2+10;
	     close -> H2
	 end,
    H4 = case rb2(Flags) of
	     open -> H3+10;
	     close -> H3
	 end,
    H4.

bu(close) -> 0;
bu(open)  -> 30.

s_bi(close,close) -> 0;
s_bi(_,_) -> 100.

f(close) -> 0;
f(open)  -> 100.

%% Try to distribute Diff as evenly as possible between E1, E2 and E3.
divide([{T,T},{S,S},{F,F}], _Diff) ->
    {T,S,F};
divide(L, Diff) ->
    [{T,Tmax},{S,Smax},{F,Fmax}] = L,

    %% Count how many elements in L can still be filled
    Rem = remaining(L),
    
    %% Divide Diff by Rem
    D = Diff div Rem,

    if
	%% All of Diff has been distributed
	D==0 -> {T,S,F};
	
	true ->
    
	    %% For each element, try to add as much as possible of D
	    {NewT,Dt} = divide2(D,T,Tmax),
	    {NewS,Ds} = divide2(D,S,Smax),
	    {NewF,Df} = divide2(D,F,Fmax),
    
	    %% Recur with a list of elements with new current values
	    %% and decreased Diff
	    divide([{NewT,Tmax},{NewS,Smax},{NewF,Fmax}],
		   (Diff rem Rem)+Dt+Ds+Df)
    end.

%% Count the number of 'non-filled' elements in L, ie where Curr<Max.
remaining([]) -> 0;
remaining([{Max,Max}|T]) -> remaining(T);
remaining([_H|T]) -> 1 + remaining(T).

divide2(_Diff, Max, Max) -> {Max,0};
divide2(Diff, Curr, Max) ->
    New = Curr+Diff,
    if
	New>Max -> {Max,New-Max};
	true -> {New,0}
    end.

%%--Resizing using resize bars----------------------------------------
%% Motions event will move the ResizeBar accordingly in Win, when
%% the mouse button is released, the window is reconfigured.

resize(WinInfo, ResizeBar) ->		
    
    %% Get window dimensions
    W = gs:read(WinInfo#winInfo.window, width),
    H = gs:read(WinInfo#winInfo.window, height),
    
    %% Call resize loop with min and max for the resize bars derived
    %% from the window dimensions
    resizeloop(WinInfo, ResizeBar, null,
	       rblimits('RB1',W,H),
	       rblimits('RB2',W,H),
	       rblimits('RB3',W,H)).

resizeloop(WI, RB, Prev, {Min1,Max1},{Min2,Max2},{Min3,Max3}) ->
    receive
	{gs,_,motion,_,[_,Y]} when RB=='RB1', Y>Min1,Y<Max1 ->
	    gs:config('RB1', {y,Y}),
	    resizeloop(WI, RB, Y, {Min1,Max1},{Min2,Max2},{Min3,Max3});
	{gs,_,motion,_,_} when RB=='RB1' ->
	    resizeloop(WI, RB, Prev,{Min1,Max1},{Min2,Max2},{Min3,Max3});
	
	{gs,_,motion,_,[_,Y]} when RB=='RB2', Y>Min2,Y<Max2 ->
	    gs:config('RB2', {y,Y}),
	    resizeloop(WI, RB, Y, {Min1,Max1},{Min2,Max2},{Min3,Max3});
	{gs,_,motion,_,_} when RB=='RB2' ->
	    resizeloop(WI, RB, Prev,{Min1,Max1},{Min2,Max2},{Min3,Max3});
	 
	{gs,_,motion,_,[X,_]} when RB=='RB3', X>Min3,X<Max3 ->
	    gs:config('RB3', {x,X}),
	    resizeloop(WI, RB, X, {Min1,Max1},{Min2,Max2},{Min3,Max3});
	{gs,_,motion,_,_} when RB=='RB3' ->
	    resizeloop(WI, RB, Prev,{Min1,Max1},{Min2,Max2},{Min3,Max3});
	
	{gs,_,buttonrelease,_,_} ->
	    resize_win(WI, RB, Prev)
    end.

resize_win(_WinInfo, _RB, null) -> ignore;
resize_win(WinInfo, 'RB1', Y) ->
    {_Bu,S,Bi,F} = Flags = WinInfo#winInfo.flags,
    H = gs:read('CodeArea', height),
    Diff = H-(Y-25),

    %% Resize Code, Evaluator and Binding areas
    resize_code_area(WinInfo, height, -Diff),
    if
	S==close, Bi==close, F==open ->
	    resize_trace_area(open, height, Diff);
	true ->
	    resize_eval_area(S, height, Diff),
	    resize_bind_area(Bi, height, Diff)
    end,

    %% Resize vertical resize bar
    case rb3(Flags) of
	open -> gs:config('RB3', {height,gs:read('RB3',height)+Diff});
	close -> ignore
    end,
    
    %% Adjust the frames y coordinates
    config_v();
resize_win(WinInfo, 'RB2', Y) ->
    {_Bu,S,Bi,F} = Flags = WinInfo#winInfo.flags,
    Prev = gs:read('TraceArea',y),
    Diff = Prev-(Y+10),
    
    %% Resize Trace, Evaluator and Binding areas
    resize_trace_area(F, height, Diff),
    resize_eval_area(S, height, -Diff),
    resize_bind_area(Bi, height, -Diff),

    %% Resize vertical resize bar
    case rb3(Flags) of
	open -> gs:config('RB3', {height,gs:read('RB3',height)-Diff});
	close -> ignore
    end,

    %% Adjust the frames y coordinates
    config_v();

resize_win(WinInfo, 'RB3', X) ->
    {_Bu,S,Bi,_F} = WinInfo#winInfo.flags,
    Prev = gs:read('BindArea', x),
    Diff = Prev-(X+10),
    
    %% Resize Binding and Trace areas
    resize_bind_area(Bi, width, Diff),
    resize_eval_area(S, width, -Diff),

    %% Adjust the frames x coordinates
    config_h().

%% Given the window dimensions, return the limits for a  resize bar.
rblimits('RB1',_W,H) ->
    
    %% Code frame should not have height <100
    Min = 126,
    
    %% Max is decided by a minimum distance to 'RB2'
    %% unless 'RB2' is invisible and 'CodeArea' is visible
    %% (=> EvalFrame and BindFrame invisible) in which case
    %% TraceFrame should not have height <100
    RB2 = gs:read('RB2',height),
    FF = gs:read('TraceArea',height),
    Max = case RB2 of
	      0 when FF/=0 ->
		  H-112;
	      _ ->
		  Y = gs:read('RB2',y),
		  erlang:max(Min,Y-140)
	  end,
    
    {Min,Max};
rblimits('RB2',_W,H) ->

    %% TraceFrame should not have height <100
    Max = H-112,
    
    %% Min is decided by a minimum distance to 'RB1'
    Y = gs:read('RB1',y),
    Min = erlang:min(Max,Y+140),
    
    {Min,Max};

rblimits('RB3',W,_H) ->
    
    %% Neither CodeArea nor BindArea should occupy 
    %% less than 1/3 of the total window width and EvalFrame should
    %% be at least 289 pixels wide
    {erlang:max(round(W/3),289),round(2*W/3)}.


%%====================================================================
%% 'Go To Line' and 'Search' help windows
%%====================================================================

helpwin(gotoline, WinInfo, GS, Coords) ->
    spawn_link(?MODULE, helpwin, [gotoline, WinInfo, GS, Coords,self()]);
helpwin(search, WinInfo, GS, Coords) ->
    spawn_link(?MODULE, helpwin, [search, WinInfo, GS, Coords, self()]).

helpwin(Type, WinInfo, GS, Coords, AttPid) ->
    {_Mod, Editor} =  WinInfo#winInfo.editor,
    Data = case Type of
	       gotoline -> null;
	       search ->
		   {{1, 0}, false}
	   end,
    Win = helpwin(Type, GS, Coords),
    helpwin_loop(Type, AttPid, Editor, Data, Win).

helpwin_loop(Type, AttPid, Editor, Data, Win) ->
    receive
	{gs, _Id, destroy, _Data, _Arg} ->
	    helpwin_stop(Type, AttPid, Editor, Data),
	    true;

	{gs, _Id, keypress, _Data, ['Return'|_]} ->
	    gs:config(btn(Win), flash),
	    Data2 = helpwin_action(Type, default,
				   AttPid, Editor, Data, Win),
	    helpwin_loop(Type, AttPid, Editor, Data2, Win);
	{gs, _Id, keypress, _Data, _Arg} ->
	    helpwin_loop(Type, AttPid, Editor, Data, Win);
	
	{gs, _Id, click, _Data, ["Clear"]} ->
	    gs:config(ent(Win), {delete, {0,last}}),
	    Data2 = helpwin_clear(Type, AttPid, Editor, Data, Win),
	    helpwin_loop(Type, AttPid, Editor, Data2, Win);
	{gs, _Id, click, _Data, ["Close"]} ->
	    helpwin_stop(Type, AttPid, Editor, Data),
	    true;
	{gs, _Id, click, Action, _Arg} ->
	    Data2 =
		helpwin_action(Type, Action, AttPid, Editor, Data, Win),
	    helpwin_loop(Type, AttPid, Editor, Data2, Win)
    end.

helpwin_stop(gotoline, _AttPid, _Editor, _Data) ->
    ignore;
helpwin_stop(search, _AttPid, Editor, {Pos, _CS}) ->
    unmark_string(Editor, Pos).

helpwin_clear(gotoline, _AttPid, _Editor, Data, _Win) ->
    Data;
helpwin_clear(search, _AttPid, Editor, {Pos, CS}, Win) ->
    unmark_string(Editor, Pos),
    gs:config(lbl(Win), {label, {text,""}}),
    {{1, 0}, CS}.

helpwin_action(gotoline, default, AttPid, _Editor, Data, Win) ->
    case string:strip(gs:read(ent(Win), text)) of
	"" -> ignore;
	Str ->
	    case catch list_to_integer(Str) of
		{'EXIT', _Reason} -> ignore;
		Line -> AttPid ! {gui, {gotoline, Line}}
	    end
    end,
    Data;
helpwin_action(search, case_sensitive, _AttPid, _Ed, {Pos, CS}, _Win) ->
    Bool = if CS==true -> false; CS==false -> true end,
    {Pos, Bool};
helpwin_action(search, default, _AttPid, Editor, {Pos, CS}, Win) ->
    gs:config(lbl(Win), {label, {text, ""}}),
    unmark_string(Editor, Pos),
    case gs:read(ent(Win), text) of
	"" -> {Pos, CS};
	Str ->
	    gs:config(lbl(Win), {label, {text,"Searching..."}}),
	    Str2 = lowercase(CS, Str),
	    case search(Str2, Editor, gs:read(Editor, size), Pos, CS) of
		{Row, Col} ->
		    gs:config(lbl(Win), {label, {text,""}}),
		    mark_string(Editor, {Row, Col}, Str),
		    {{Row, Col}, CS};
		not_found ->
		    gs:config(lbl(Win), {label, {text,"Not found"}}),
		    {Pos, CS}
	    end
    end.

search(_Str, _Editor, Max, {Row, _Col}, _CS) when Row>Max ->
    not_found;
search(Str, Editor, Max, {Row, Col}, CS) ->
    SearchIn = lowercase(CS, gs:read(Editor,
				     {get,{{Row,Col+1},{Row,lineend}}})),
    case string:str(SearchIn, Str) of
	0 -> search(Str, Editor, Max, {Row+1, 0}, CS);
	N -> {Row, Col+N}
    end.

lowercase(true, Str) -> Str;
lowercase(false, Str) ->
    lists:map(fun(Char) ->
		      if
			  Char>=$A, Char=<$Z -> Char+32;
			  true -> Char
		      end
	      end,
	      Str).

mark_string(Editor, {Row, Col}, Str) ->
    Between = {{Row,Col}, {Row,Col+length(Str)}},
    Font = dbg_ui_win:font(bold),
    gs:config(Editor, [{vscrollpos, Row-5},
		       {font_style, {Between, Font}},
		       {fg, {Between, red}}]).

unmark_string(Editor, {Row, Col}) ->
    Between = {{Row,Col}, {Row,lineend}},
    Font = dbg_ui_win:font(normal),
    gs:config(Editor, [{vscrollpos, Row-5},
		       {font_style, {Between, Font}},
		       {fg, {Between, black}}]).

helpwin(Type, GS, {X, Y}) ->
    W = 200, Pad=10, Wbtn = 50,

    Title =
	case Type of search -> "Search"; gotoline -> "Go To Line" end,
    Win = gs:window(GS, [{title, Title}, {x, X}, {y, Y}, {width, W},
			 {destroy, true}]),
    
    Ent = gs:entry(Win, [{x, Pad}, {y, Pad}, {width, W-2*Pad},
			 {keypress, true}]),
    Hent = gs:read(Ent, height),

    Font = dbg_ui_win:font(normal),

    {Ybtn, Lbl} =
	case Type of
	    search ->
		Ycb = Pad+Hent,
		gs:checkbutton(Win, [{label, {text, "Case Sensitive"}},
				     {font, Font},
				     {align, w},
				     {x, Pad}, {y, Ycb},
				     {width, W-2*Pad}, {height, 15},
				     {data, case_sensitive}]),
		Ylbl = Ycb+15,
		{Ylbl+Hent+Pad,
		 gs:label(Win, [{x, Pad}, {y, Ylbl},
				{width, W-2*Pad}, {height, Hent}])};
	    gotoline -> {Pad+Hent+Pad, null}
	end,

    BtnLbl = case Type of search -> "Search"; gotoline -> "Go" end,
    Btn = gs:button(Win, [{label, {text, BtnLbl}}, {font, Font},
			  {x, W/2-3/2*Wbtn-Pad}, {y, Ybtn},
			  {width, Wbtn}, {height, Hent},
			  {data, default}]),
    gs:button(Win, [{label, {text, "Clear"}}, {font, Font},
		    {x, W/2-1/2*Wbtn}, {y, Ybtn},
		    {width, Wbtn}, {height, Hent}]),
    gs:button(Win, [{label, {text, "Close"}}, {font, Font},
		    {x, W/2+1/2*Wbtn+Pad}, {y, Ybtn},
		    {width, Wbtn}, {height, Hent}]),

    H = Ybtn+Hent+Pad,
    gs:config(Win, [{height, H}, {map, true}]),
    {Ent, Lbl, Btn}.

ent(Win) -> element(1, Win).
lbl(Win) -> element(2, Win).
btn(Win) -> element(3, Win).
