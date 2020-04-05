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
-module(dbg_wx_trace_win).

%% External exports
-export([init/0, stop/1]).
-export([create_win/4, 
	 get_window/1,
	 configure/2,
	 enable/2, is_enabled/1, select/2,
	 add_break/3, update_break/2, delete_break/2,
	 clear_breaks/1, clear_breaks/2,
	 display/2,                                   % Help messages
	 is_shown/2,                                  % Code area
	 show_code/3, show_no_code/1, remove_code/2,      
	 mark_line/3, unmark_line/1,
	 select_line/2, selected_line/1,
	 eval_output/3,                               % Evaluator area
	 update_bindings/2,                           % Bindings area
         update_strings/1,
	 trace_output/2,                              % Trace area
	 handle_event/2
	]).
-export([helpwin/2]).

-include_lib("wx/include/wx.hrl").

-record(breakInfo, {point, status, break}).
-record(break, {mb, smi, emi, dimi, demi}).  %% BUGBUG defined in dbg_wx_win
-record(winInfo, {window,          % wxobj()
		  size,            % {W, H}
		  find,            % #find{}
		  m_szr,           % {Panel, Sizer},
		  e_szr,           % {bool Shown, Sizer},

		  code,            % code editor #sub{}
		  sb,              % status bar
		  sg,              % Search/Goto #sub{}
		  bs,              % Buttons #sub{} subwindow info
		  eval,            % Eval #sub{} subwindow info
		  bind,            % Bindings #sub{} subwindow info
		  trace,           % Trace #sub{} subwindow info
		  
		  marked_line=0,   % integer() Current line
		  selected_line=0, % integer() Selected line

		  breaks=[],       % [#breakInfo{}] Known breakpoints

		  editor,          % {Mod, Editor}  Visible code editor
		  editors=[]       % [{Mod,Editor}] Code editors
		 }).

-record(sub, {enable=true,         %  Subwindow is enabled
	      win,                 %  Sash Sub window obj
	      in,                  %  undefined or input window obj
	      out,                 %  undefined or output window obj
	      name                 %  name
	     }).               

-record(sa, {search,               %  Search input ctrl
	     goto,                 %  Goto  input ctrl
	     radio}).              %  Radio buttons

-record(find, {start,              % start pos
	       strlen,             % search string len
	       found               % status
	      }).


-define(StepButton,     401).
-define(NextButton,     402).
-define(ContinueButton, 403).
-define(FinishButton,   404).
-define(WhereButton,    405).
-define(UpButton,       406).
-define(DownButton,     407).

-define(EVAL_ENTRY,     410).
-define(EVAL_LOG,       411).
-define(BIND_PANEL,     412).
-define(SEARCH_ENTRY,   413).
-define(GOTO_ENTRY,     414).


-define(SASH_CODE,      425).
-define(SASH_EVAL,      426).
-define(SASH_TRACE,     427).

-define(WIN_W,          700).
-define(WIN_H,          650).

-define(CODE_H,         400).
-define(BUTT_H,          50).  % Maximum
-define(EVAL_H,         200).
-define(BIND_H,         200).
-define(TRACE_H,        100).

%%====================================================================
%% External exports
%%====================================================================

%%--------------------------------------------------------------------
%% init() -> GS
%%   GS = term()
%%--------------------------------------------------------------------
init() ->
    _ = dbg_wx_win:init(),
    ok.

stop(#winInfo{window=Win}) ->
    (catch wxFrame:destroy(Win)),
    ok.

%%--------------------------------------------------------------------
%% create_win(GS, Title, TraceWin, Menus) -> #winInfo{}
%%  GS = gsobj()
%%  Title = string()
%%  TraceWin = [WinArea]
%%    WinArea = 'Button|Evaluator|Bindings|Trace Area'
%%  Menus = [menu()]  See dbg_wx_win.erl
%%--------------------------------------------------------------------
create_win(Parent, Title, Windows, Menus) ->
    Do = 
	fun() ->
		Win = wxFrame:new(Parent, ?wxID_ANY, dbg_wx_win:to_string(Title), 
				  [{size, {?WIN_W,?WIN_H}}]),
		Panel = wxPanel:new(Win, [{size, {?WIN_W,?WIN_H}}]),
		MenuBar = wxMenuBar:new(),
		dbg_wx_win:create_menus(MenuBar, Menus, Win, 1),
		wxFrame:setMenuBar(Win, MenuBar),
		
		Sizer = wxBoxSizer:new(?wxVERTICAL),
		Code = code_area(Panel),
		_ = wxSizer:add(Sizer, Code#sub.win, 
			    [{proportion,1}, {border, 2}, 
			     {flag, ?wxEXPAND bor ?wxDOWN}]),
		wxSizer:setVirtualSizeHints(Sizer, Code#sub.win),

		ExpandWithBorder = [{border, 3},{flag,?wxEXPAND bor ?wxALL}],
		Search = search_area(Panel),
		_ = wxSizer:add(Sizer, Search#sub.win, ExpandWithBorder),
		Bs     = button_area(Panel),
		_ = wxSizer:add(Sizer, Bs#sub.win, ExpandWithBorder),

		InfoArea = wxBoxSizer:new(?wxHORIZONTAL),
		wxSizer:setMinSize(InfoArea, {100, ?EVAL_H}),
		Eval  = eval_area(Panel),
		_ = wxSizer:add(InfoArea, Eval#sub.win, [{proportion,1},{flag,?wxEXPAND}]),
		Bind  = bind_area(Panel),		
		_ = wxSizer:add(InfoArea, Bind#sub.win, 
			    [{proportion,1},{border, 2}, 
			     {flag,?wxEXPAND bor ?wxLEFT}]),
		_ = wxSizer:add(Sizer, InfoArea, ExpandWithBorder),
		
		Trace = trace_area(Panel),
		_ = wxSizer:add(Sizer, Trace#sub.win, ExpandWithBorder),
		SB    = wxFrame:createStatusBar(Win,[]),

		%% Note id and lastId to get the event when it dragged is complete
		wxFrame:connect(Win, sash_dragged, [{id,?SASH_CODE}, 
						    {lastId,?SASH_TRACE}]),
		wxFrame:connect(Win, close_window, [{skip, true}]),
		wxFrame:connect(Win, size, [{skip, true}]),
		wxWindow:connect(Win, key_up, [{skip,true}]),
		wxWindow:setFocus(Code#sub.out),
		
		Wi0 = #winInfo{window=Win, 
			       m_szr={Panel, Sizer}, 
			       e_szr={true, InfoArea},
			       code=Code, sb=SB, sg=Search, bs=Bs, 
			       eval=Eval, trace=Trace, bind=Bind,
			       editor={'$top', Code#sub.out}, 
			       editors=[{'$top', Code#sub.out}]},
		
		Wi = show_windows(enable_windows(Wi0,Windows)),
		wxWindow:setSizer(Panel, Sizer),
		_ = wxSizer:fit(Sizer, Win),
		wxSizer:setSizeHints(Sizer,Win),

		IconFile = dbg_wx_win:find_icon("erlang_bug.png"),
		Icon = wxIcon:new(IconFile, [{type,?wxBITMAP_TYPE_PNG}]),
		wxFrame:setIcon(Win, Icon),
		wxIcon:destroy(Icon),
		
		wxFrame:show(Win),
		put(window, Win),
                put(strings, [str_on]),
		Wi
	end,

    try wx:batch(Do) 
    catch E:R ->
	    io:format("Crashed ~p ~p",[E,R]),
	    erlang:error(E)
    end.


%%--------------------------------------------------------------------
%% get_window(WinInfo) -> Window
%%   WinInfo = #winInfo{}
%%   Window = gsobj()
%%--------------------------------------------------------------------
get_window(WinInfo) ->
    WinInfo#winInfo.window.

%%--------------------------------------------------------------------
%% configure(WinInfo, Windows) -> WinInfo
%%   WinInfo = #winInfo{}
%%  Windows = [WinArea]
%%    WinArea = 'Button|Evaluator|Bindings|Trace Area'
%% Window areas should be opened or closed.
%%--------------------------------------------------------------------
configure(Wi=#winInfo{window=Win,m_szr={Panel,Sizer}}) ->
    wx:batch(fun() ->
		     _ = show_windows(Wi),
		     wxSizer:layout(Sizer),
		     %%wxWindow:setSizerAndFit(Panel,Sizer),
		     wxWindow:setSizer(Panel, Sizer),
		     _ = wxSizer:fit(Sizer, Win),
		     wxSizer:setSizeHints(Sizer,Win),		
		     Wi
	     end).
    
configure(Wi0=#winInfo{window=Win,m_szr={Panel,Sizer}}, Windows) ->
    wx:batch(fun() -> 
		     Wi = enable_windows(Wi0, Windows),
		     _ = show_windows(Wi),
		     wxSizer:layout(Sizer),
		     wxWindow:setSizer(Panel, Sizer),
		     _ = wxSizer:fit(Sizer, Win),
		     wxSizer:setSizeHints(Sizer,Win),		
		     Wi
	     end).

enable_windows(Wi=#winInfo{e_szr={_,InfoArea},bs=Bs0,sg=SG0,
			   eval=Eval0,trace=Trace0,bind=Bind0},Windows) ->
    Subs  = [Window#sub{enable=lists:member(Window#sub.name,Windows)} 
	     || Window <- [SG0,Bs0,Eval0,Trace0,Bind0]],
    [SG, Bs,Eval,Trace,Bind] = Subs,
    ESzr = Eval#sub.enable orelse Bind#sub.enable,
    Wi#winInfo{e_szr={ESzr, InfoArea},sg=SG,bs=Bs,
	       eval=Eval,trace=Trace,bind=Bind}.
    

show_windows(Wi=#winInfo{m_szr={_,Sizer}, e_szr={_,InfoArea},bs=Bs,sg=SG,
		      eval=Eval,trace=Trace,bind=Bind}) -> 
    case SG#sub.enable of
	false -> wxSizer:hide(Sizer, SG#sub.win);
	_ ->     wxSizer:show(Sizer, SG#sub.win)
    end,
    case Bs#sub.enable of
	false -> wxSizer:hide(Sizer, Bs#sub.win);
	_ ->     wxSizer:show(Sizer, Bs#sub.win)
    end,
    if (not Eval#sub.enable) andalso (not Bind#sub.enable) ->
	    wxSizer:hide(Sizer, InfoArea);
       not Eval#sub.enable  ->
	    wxSizer:show(Sizer, InfoArea),
	    wxSizer:hide(InfoArea, Eval#sub.win),
	    wxSizer:show(InfoArea, Bind#sub.win);
       not Bind#sub.enable ->
	    [EvalSI|_] = wxSizer:getChildren(InfoArea),
	    wxSizerItem:setProportion(EvalSI, 1),
	    wxSizer:show(Sizer, InfoArea),
	    wxSizer:hide(InfoArea, Bind#sub.win),
	    wxSizer:show(InfoArea, Eval#sub.win),
	    true;
       true ->
	    wxSizer:show(Sizer, InfoArea),
	    wxSizer:show(InfoArea, Eval#sub.win),
	    wxSizer:show(InfoArea, Bind#sub.win)
    end,
    case Trace#sub.enable of
	false -> wxSizer:hide(Sizer, Trace#sub.win);
	_ ->     wxSizer:show(Sizer, Trace#sub.win)
    end,
    Wi.

%%--------------------------------------------------------------------
%% enable([MenuItem], Bool)
%% is_enabled(MenuItem) -> Bool
%%   MenuItem = atom()
%%   Bool = boolean()
%%--------------------------------------------------------------------
enable(MenuItems, Bool) ->
    wx:foreach(fun(MenuItem) ->
		       MI = get(MenuItem),
		       wxMenuItem:enable(MI, [{enable, Bool}]),
		       case is_button(MenuItem) of
			   {true, ButtonId} ->
			       Parent = get(window),
			       Butt = wxWindow:findWindowById(ButtonId, 
							      [{parent, Parent}]),
			       case wx:is_null(Butt) of
				   true -> ignore;
				   false ->
				       wxButton:enable(Butt, [{enable, Bool}])
			       end;
			   _ -> 
			       ignore
		       end
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
    case WinInfo#winInfo.editor of
	{Mod, Editor} ->
	    dbg_wx_code:add_break_to_code(Editor, Line, Status);
	_ -> ok
    end,
    add_break_to_menu(WinInfo, Menu, Break).

add_break_to_menu(WinInfo, Menu, {Point, [Status|_Options]=Options}) ->
    Break = dbg_wx_win:add_break(WinInfo#winInfo.window, Menu, Point),
    dbg_wx_win:update_break(Break, Options),
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
    case WinInfo#winInfo.editor of
	{Mod, Editor} ->
	    dbg_wx_code:add_break_to_code(Editor, Line, Status);
	_ -> ok
    end,
    update_break_in_menu(WinInfo, Break).

update_break_in_menu(WinInfo, {Point, [Status|_Options]=Options}) ->
    {value, BreakInfo} = lists:keysearch(Point, #breakInfo.point,
					 WinInfo#winInfo.breaks),
    dbg_wx_win:update_break(BreakInfo#breakInfo.break, Options),
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
    case WinInfo#winInfo.editor of
	{Mod, Editor} -> dbg_wx_code:del_break_from_code(Editor, Line);
	_ -> ignore
    end,
    delete_break_from_menu(WinInfo, Point).

delete_break_from_menu(WinInfo, Point) ->
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
    clear_breaks(WinInfo, all).
clear_breaks(WinInfo, Mod) ->
    Remove = if
		 Mod =:= all -> WinInfo#winInfo.breaks;
		 true ->
		     lists:filter(fun(#breakInfo{point={Mod2,_L}}) ->
					  if
					      Mod2 =:= Mod -> true;
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
%%       | {exit,Where,Reason} | {text,Text}
%%     Status = break | wait | Level
%%       Level = int()
%%     Mod = atom()
%%     Line = integer()
%%     Where = {Mod,Line} | null
%%     Reason = term()
%%     Text = string()
%%--------------------------------------------------------------------
display(#winInfo{window=Win, sb=Sb},Arg) ->
    Str = case Arg of
	      idle -> "State: uninterpreted";
	      {exit, {Mod,Line}, Reason} ->
		  wxWindow:raise(Win),
		  dbg_wx_win:to_string("State: EXITED [~w.erl/~w], Reason:~w",
				       [Mod, Line, Reason]);
	      {exit, null, Reason} ->
		  wxWindow:raise(Win),
		  dbg_wx_win:to_string("State: EXITED [uninterpreted], "
				       "Reason:~w", [Reason]);
	      {Level, null, _Line} when is_integer(Level) ->
		  dbg_wx_win:to_string("*** Call level #~w "
				       "(in non-interpreted code)",
				       [Level]);
	      {Level, Mod, Line} when is_integer(Level) ->
		  dbg_wx_win:to_string("*** Call level #~w [~w.erl/~w]",
				       [Level, Mod, Line]);
	      {Status, Mod, Line} ->
		  What = case Status of
			     wait -> 'receive';
			     _ -> Status
			 end,
		  dbg_wx_win:to_string("State: ~w [~w.erl/~w]",
				       [What, Mod, Line]);
	      {running, Mod} ->
		  dbg_wx_win:to_string("State: running [~w.erl]", [Mod]);
	      {text, Text}  -> dbg_wx_win:to_string(Text)
	  end,
    wxStatusBar:setStatusText(Sb, Str).

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
is_shown(_WinInfo, _Mod) ->
    %% Previously cached modules here, nyi so return false
    false.

show_code(WinInfo = #winInfo{editor={_, Ed}}, Mod, Contents) ->
    %% Insert code and update breakpoints, if any
    dbg_wx_code:load_code(Ed, Contents), 
    
    lists:foreach(fun(BreakInfo) ->
			  case BreakInfo#breakInfo.point of
			      {Mod2, Line} when Mod2 =:= Mod ->
				  Status = BreakInfo#breakInfo.status,
				  dbg_wx_code:add_break_to_code(Ed, Line,Status);
			      _Point -> ignore
			  end
		  end,
		  WinInfo#winInfo.breaks),
    
    WinInfo#winInfo{editor={Mod,Ed},find=undefined}.

show_no_code(WinInfo = #winInfo{editor={_, Ed}}) ->
    dbg_wx_code:unload_code(Ed),
    WinInfo#winInfo{editor={'$top', Ed}}.

remove_code(WinInfo, _Mod) ->
    WinInfo.

%%--------------------------------------------------------------------
%% mark_line(WinInfo, Line, How) -> WinInfo
%%   WinInfo = #winInfo{}
%%   Line = integer()
%%   How = break | where
%% Mark the code line where the process is executing.
%%--------------------------------------------------------------------
mark_line(WinInfo = #winInfo{editor={_,Ed}}, Line, _How) ->
    dbg_wx_code:mark_line(Ed, WinInfo#winInfo.marked_line, Line), 
    WinInfo#winInfo{marked_line=Line}.

unmark_line(WinInfo) ->    
    mark_line(WinInfo, 0, false).


%%--------------------------------------------------------------------
%% select_line(WinInfo, Line) -> WinInfo
%% selected_line(WinInfo) -> undefined | Line
%%   WinInfo = #winInfo{}
%%   Line = integer()
%% Select/unselect a line (unselect if Line=0).
%%--------------------------------------------------------------------
select_line(WinInfo, Line) ->
    {_Mod, Ed} = WinInfo#winInfo.editor,

    %% Since 'Line' may be specified by the user in the 'Go To Line'
    %% help window, it must be checked that it is correct
    Size = dbg_wx_code:get_no_lines(Ed),
    if
	Line =:= 0 ->
	    dbg_wx_code:goto_line(Ed,1),
	    WinInfo#winInfo{selected_line=0};
	Line < Size ->
	    dbg_wx_code:goto_line(Ed,Line),
	    WinInfo#winInfo{selected_line=Line};
	true ->
	    WinInfo
    end.

selected_line(#winInfo{editor={_,Ed}}) ->
    wxStyledTextCtrl:getCurrentLine(Ed)+1.

%%--------------------------------------------------------------------
%% eval_output(winInfo{}, Str, Face)
%%   Str = string()
%%   Face = normal | bold
%%--------------------------------------------------------------------
eval_output(#winInfo{eval=#sub{out=Log}}, Text, _Face) -> 
    wxTextCtrl:appendText(Log, dbg_wx_win:to_string(Text)),
    ok.
    
%%--------------------------------------------------------------------
%% update_bindings(Bs)
%%   Bs = [{Var,Val}]
%%--------------------------------------------------------------------
update_bindings(#winInfo{bind=#sub{out=BA}}, Bs) ->
    wxListCtrl:deleteAllItems(BA),
    wx:foldl(fun({Var,Val},Row) ->
		     wxListCtrl:insertItem(BA, Row, ""), 
		     wxListCtrl:setItem(BA, Row, 0, dbg_wx_win:to_string(Var)),
                     Format = case get(strings) of
                                  []        -> "~0ltP";
                                  [str_on]  -> "~0tP"
                              end,
		     wxListCtrl:setItem(BA, Row, 1, dbg_wx_win:to_string(Format,[Val, 20])),
		     Row+1
	     end, 0, Bs),
    put(bindings,Bs),
    ok.

update_strings(Strings) ->
    _ = put(strings, Strings),
    ok.

%%--------------------------------------------------------------------
%% trace_output(Str)
%%   Str = string()
%%--------------------------------------------------------------------
trace_output(#winInfo{trace=#sub{out=Log}}, Text) -> 
    wxTextCtrl:appendText(Log, dbg_wx_win:to_string(Text)),
    ok.

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
handle_event(_Ev=#wx{event=#wxClose{}}, _WinInfo) ->
    stopped;

handle_event(#wx{event=#wxSize{size=Size}}, Wi0) ->
    Wi = Wi0#winInfo{size=Size},
    resize(Wi),
    {win, Wi};

handle_event(#wx{event=#wxSash{dragStatus=?wxSASH_STATUS_OUT_OF_RANGE}},_Wi) ->
    ignore;
handle_event(#wx{id=?SASH_CODE, event=#wxSash{dragRect={_X,_Y,_W,H}}}, Wi) ->
    #winInfo{code=Code,m_szr={_,Sizer},e_szr={Enable,InfoSzr},trace=Trace} = Wi,

    case Enable orelse Trace#sub.enable of
	false -> 
	    ignore;
	true ->
	    {_, CMH} = wxWindow:getMinSize(Code#sub.win),
	    case CMH > H of
		true ->  wxSashWindow:setMinSize(Code#sub.win, {500, H});
		_ -> ignore 
	    end,
	    {_, CH} = wxWindow:getSize(Code#sub.win),
	    Change = CH - H, 
	    ChangeH = fun(Item) ->
			      {ItemW, ItemH} = wxSizerItem:getMinSize(Item),
			      wxSizerItem:setInitSize(Item, ItemW, erlang:max(ItemH+Change,-1))
		      end,
	    if Enable ->
		    {IW, IH} = wxSizer:getMinSize(InfoSzr),
		    [ChangeH(Child) || Child <- wxSizer:getChildren(InfoSzr)],
		    wxSizer:setMinSize(InfoSzr, {IW, IH+Change}),
		    ok;
	       Trace#sub.enable -> 
		    {TW, TH} = wxWindow:getMinSize(Trace#sub.win),
		    wxWindow:setMinSize(Trace#sub.win, {TW, TH+Change}),
		    ok
	    end,
	    wxSizer:layout(Sizer),
	    ignore
    end;

handle_event(#wx{id=?SASH_EVAL, event=#wxSash{dragRect={_X,_Y,W,_H}}}, Wi) ->
    #winInfo{m_szr={_,Sizer},e_szr={Enable,InfoSzr},
	     eval=#sub{enable=Enable, win=EvalSzr}} = Wi,
    case Enable of
	false ->
	    ignore;
	true ->
	    [Eval,Bind] = wxSizer:getChildren(InfoSzr),
	    {Tot,_} = wxSizer:getSize(InfoSzr),
	    EvalWidth = Tot-W,
	    
	    Change = fun(Szr, Width) ->
			     {_EW,EH} = wxSizerItem:getMinSize(Szr),
			     wxSizerItem:setInitSize(Szr, Width, EH)
		     end,

	    Change(Eval, EvalWidth),
	    [Change(Kid, EvalWidth) || Kid <- wxSizer:getChildren(EvalSzr)],
	    Change(Bind, W),
	    
	    wxSizerItem:setProportion(Eval, 0),
	    wxSizer:layout(InfoSzr),
	    wxSizer:layout(Sizer),

	    resize(Wi),
	    ignore
    end;

handle_event(#wx{id=?SASH_TRACE, event=#wxSash{dragRect={_X,_Y,_W,H}}}, Wi) ->
    #winInfo{code=Code,m_szr={_,Sizer},e_szr={Enable,InfoSzr},trace=Trace} = Wi,
    {TW, TH} = wxWindow:getSize(Trace#sub.win),
    Change = TH - H, 
    case Enable of
	false ->  %% Eval Area or Bindings
	    {_, CH}  = wxWindow:getSize(Code#sub.win),
	    {_, CMH} = wxWindow:getMinSize(Code#sub.win),
	    case CMH > CH+Change of
		true ->  wxSashWindow:setMinSize(Code#sub.win, {500, CH+Change});
		_ -> ignore 
	    end,
	    wxWindow:setMinSize(Trace#sub.win, {TW, H}),
	    wxSizer:layout(Sizer),
	    ignore;
	true ->  %% Change the Eval and Bindings area
	    ChangeH = fun(Item) ->
			      {ItemW, ItemH} = wxSizerItem:getMinSize(Item),
			      wxSizerItem:setInitSize(Item, ItemW, erlang:max(ItemH+Change,-1))
		      end,
	    {IW, IH} = wxSizer:getMinSize(InfoSzr),
	    [ChangeH(Child) || Child <- wxSizer:getChildren(InfoSzr)],
	    Wanted = IH+Change,
	    wxSizer:setMinSize(InfoSzr, {IW, Wanted}),
	    {_,RH} = wxSizer:getMinSize(InfoSzr),
	    case RH > Wanted of
		true ->  %% Couldn't get the size we wanted try adjusting the code area
		    {_, CH}  = wxWindow:getSize(Code#sub.win),
		    {_, CMH} = wxWindow:getMinSize(Code#sub.win),
		    CC = CH - (RH-Wanted),
		    case CMH > CC of
			true when CC > 50 ->  
			    wxWindow:setMinSize(Trace#sub.win, {TW, H}),
			    wxSashWindow:setMinSize(Code#sub.win, {500, CC});
			_ when CC < 50 -> 			    
			    ignore;
			_ ->
			    wxWindow:setMinSize(Trace#sub.win, {TW, H})
		    end,
		    ok;
		false ->
		    wxWindow:setMinSize(Trace#sub.win, {TW, H})
	    end,
	    wxSizer:layout(Sizer),
	    ignore
    end;

%% Menus, buttons and keyboard shortcuts
handle_event(_Ev = #wx{event=#wxKey{keyCode=Key, controlDown=true}}, _WinInfo) ->
    %% io:format("Key ~p ~n",[_Ev]),
    if
	Key/=?WXK_UP, Key/=?WXK_DOWN, Key /=? WXK_RETURN -> 
	    try  
		{shortcut, list_to_atom([Key+($a-$A)])}
	    catch _:_ -> ignore
	    end;
	true -> 
	    ignore
    end;
handle_event(#wx{userData={dbg_ui_winman, Win}, 
		 event=#wxCommand{type=command_menu_selected}}, _WinInfo) ->
    dbg_wx_winman:raise(Win),
    ignore;

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

handle_event(#wx{userData=Data, 
		 event=_Cmd=#wxCommand{type=command_menu_selected}},
	     _WinInfo) ->
    %%io:format("Command  ~p ~p~n",[Data,_Cmd]),
    Data;

%% Code area
handle_event(#wx{event=#wxStyledText{type=stc_doubleclick}}, 
	     WinInfo = #winInfo{editor={Mod,Ed}}) ->
    Line = wxStyledTextCtrl:getCurrentLine(Ed),
    Point = {Mod, Line+1},
    case lists:keymember(Point, #breakInfo.point, WinInfo#winInfo.breaks) of
	true -> {break, Point, delete};
	false -> {break, Point, add}
    end;

%% Search Area
handle_event(#wx{id=?GOTO_ENTRY, event=#wxCommand{cmdString=Str}}, WinInfo) ->
    try 
	Line = list_to_integer(Str),
	{gotoline, Line}
    catch 
	_:_ ->
	    display(WinInfo, {text,"Not a line number"}),
	    ignore
    end;
handle_event(#wx{id=?SEARCH_ENTRY, event=#wxFocus{}}, Wi) ->
    {win, Wi#winInfo{find=undefined}};
handle_event(#wx{id=?SEARCH_ENTRY, event=#wxCommand{type=command_text_enter,cmdString=Str}}, 
	     Wi = #winInfo{code=Code,find=Find, sg=#sub{in=#sa{radio={NextO,_,CaseO}}}}) 
  when Find =/= undefined ->
    Dir  = wxRadioButton:getValue(NextO) xor wx_misc:getKeyState(?WXK_SHIFT),
    Case = wxCheckBox:getValue(CaseO),
    Pos = if Find#find.found, Dir ->  %% Forward Continuation
		  wxStyledTextCtrl:getAnchor(Code#sub.out);
	     Find#find.found ->  %% Backward Continuation 
		  wxStyledTextCtrl:getCurrentPos(Code#sub.out);
	     Dir ->   %% Forward wrap
		  0;
	     true ->  %% Backward wrap
		  wxStyledTextCtrl:getLength(Code#sub.out)
	  end,
    dbg_wx_code:goto_pos(Code#sub.out,Pos),
    case dbg_wx_code:find(Code#sub.out, Str, Case, Dir) of
	true -> 	    
	    display(Wi, {text,""}),
	    {win, Wi#winInfo{find=Find#find{found=true}}};
	false ->
	    display(Wi, {text,"Not found (Hit Enter to wrap search)"}),
	    {win, Wi#winInfo{find=Find#find{found=false}}}
    end;
handle_event(#wx{id=?SEARCH_ENTRY, event=#wxCommand{cmdString=""}}, 
	     Wi=#winInfo{code=Code}) ->
    %% Reset search (and selection pos)
    Pos = dbg_wx_code:current_pos(Code#sub.out),
    dbg_wx_code:goto_pos(Code#sub.out,Pos),
    {win, Wi#winInfo{find=undefined}};
handle_event(#wx{id=?SEARCH_ENTRY, event=#wxCommand{cmdString=Str}}, 
	     Wi = #winInfo{code=Code,find=Find, 
			   sg=#sub{in=#sa{radio={NextO,_,CaseO}}}}) ->
    Dir  = wxRadioButton:getValue(NextO),
    Case = wxCheckBox:getValue(CaseO),   
    
    Cont = case Find of
	       undefined ->
		   Pos = dbg_wx_code:current_pos(Code#sub.out),
		   #find{start=Pos, strlen=length(Str)};
	       #find{strlen=Old} when Old < length(Str) ->
		   Find#find{strlen=length(Str)};
	       _ ->
		   dbg_wx_code:goto_pos(Code#sub.out,Find#find.start),
		   Find#find{strlen=length(Str)}
	   end,
    case dbg_wx_code:find(Code#sub.out, Str, Case, Dir) of
	true -> 
	    display(Wi, {text,""}),
	    {win, Wi#winInfo{find=Cont#find{found=true}}};
	false ->
	    display(Wi, {text,"Not found (Hit Enter to wrap search)"}),
	    {win, Wi#winInfo{find=Cont#find{found=false}}}
    end;

%% Button area
handle_event(#wx{id=ID, event=#wxCommand{type=command_button_clicked}},_Wi) ->
    {Button, _} = lists:keyfind(ID, 2, buttons()),
    Button;

%% Evaluator area
handle_event(#wx{id=?EVAL_ENTRY, event=#wxCommand{type=command_text_enter}},
	     Wi = #winInfo{eval=#sub{in=TC}}) ->
    case wxTextCtrl:getValue(TC) of
	[10] ->
	    eval_output(Wi, "\n", normal),
	    ignore;
	Cmd ->
	    eval_output(Wi, [$>, Cmd, 10], normal),
	    wxTextCtrl:setValue(TC,""),
	    {user_command, Cmd}
    end;

%% Bindings area
handle_event(#wx{event=#wxList{type=command_list_item_selected, itemIndex=Row}},Wi) ->
    Bs = get(bindings),
    {Var,Val} = lists:nth(Row+1, Bs),
    Str = case get(strings) of
              []       -> io_lib:format("< ~s = ~ltp~n", [Var, Val]);
              [str_on] -> io_lib:format("< ~s = ~tp~n", [Var, Val])
          end,
    eval_output(Wi, Str, bold),
    ignore;
handle_event(#wx{event=#wxList{type=command_list_item_activated, itemIndex=Row}},_Wi) ->    
    Bs = get(bindings),
    Binding = lists:nth(Row+1, Bs),
    {edit, Binding};
    
handle_event(_GSEvent, _WinInfo) ->
    %%io:format("~p: unhandled ~p~n",[?MODULE, _GSEvent]),
    ignore.


%%====================================================================
%% resize(WinInfo) -> WinInfo

resize(#winInfo{bind=Bind}) ->
    %% Tweak the Binding settings text size
    if 
	Bind#sub.enable =:= false ->  
	    ok;
	Bind#sub.enable -> 
	    {EW, _} = wxWindow:getClientSize(Bind#sub.out),
	    B0W = wxListCtrl:getColumnWidth(Bind#sub.out, 0),
	    wxListCtrl:setColumnWidth(Bind#sub.out, 1, EW - B0W),
	    ok
    end.

%%====================================================================
%% Internal functions
%%====================================================================

%%--Code Area-------------------------------------------------------
code_area(Win) ->
    CodeWin = wxSashWindow:new(Win, [{id,?SASH_CODE}, 
				     {size, {?WIN_W,?CODE_H}}, 
				     {style, ?wxSW_3D}]),
    Code  = dbg_wx_code:code_area(CodeWin),
    wxSashWindow:setSashVisible(CodeWin, ?wxSASH_BOTTOM, true),
    wxWindow:setMinSize(CodeWin, {600, ?CODE_H}),
    #sub{name='Code Area',enable=true, win=CodeWin, out=Code}.


%%--Button Area-------------------------------------------------------

buttons() ->
    [{'Step',?StepButton}, {'Next',?NextButton},
     {'Continue',?ContinueButton}, {'Finish',?FinishButton},
     {'Where',?WhereButton}, {'Up',?UpButton}, {'Down',?DownButton}].

is_button(Name) ->
    case lists:keyfind(Name, 1, buttons()) of
	{Name, Button} -> {true, Button};
	false -> false
    end.

button_area(Parent) -> 
    Sz = wxBoxSizer:new(?wxHORIZONTAL),
    wx:foreach(fun({Name, Button}) ->
		       B=wxButton:new(Parent, Button, 
				      [{label,dbg_wx_win:to_string(Name)}]),
		       Id = wxWindow:getId(B),
		       _ = wxSizer:add(Sz,B, []),
		       wxButton:connect(B, command_button_clicked, [{id,Id}])
	       end, buttons()),
    #sub{name='Button Area', win=Sz}.
    
%%--Search/Goto Area-------------------------------------------------

search_area(Parent) ->
    HSz = wxBoxSizer:new(?wxHORIZONTAL),
    _ = wxSizer:add(HSz, wxStaticText:new(Parent, ?wxID_ANY, "Find:"), 
		[{flag,?wxALIGN_CENTER_VERTICAL}]),
    TC1 = wxTextCtrl:new(Parent, ?SEARCH_ENTRY, [{style, ?wxTE_PROCESS_ENTER}]), 
    _ = wxSizer:add(HSz, TC1,  [{proportion,3}, {flag, ?wxEXPAND}]),
    Nbtn = wxRadioButton:new(Parent, ?wxID_ANY, "Next"),
    wxRadioButton:setValue(Nbtn, true),
    _ = wxSizer:add(HSz,Nbtn,[{flag,?wxALIGN_CENTER_VERTICAL}]),
    Pbtn = wxRadioButton:new(Parent, ?wxID_ANY, "Previous"),
    _ = wxSizer:add(HSz,Pbtn,[{flag,?wxALIGN_CENTER_VERTICAL}]),
    Cbtn = wxCheckBox:new(Parent, ?wxID_ANY, "Match Case"),
    _ = wxSizer:add(HSz,Cbtn,[{flag,?wxALIGN_CENTER_VERTICAL}]),
    _ = wxSizer:add(HSz, 15,15, [{proportion,1}, {flag, ?wxEXPAND}]),
    _ = wxSizer:add(HSz, wxStaticText:new(Parent, ?wxID_ANY, "Goto Line:"), 
		[{flag,?wxALIGN_CENTER_VERTICAL}]),
    TC2 = wxTextCtrl:new(Parent, ?GOTO_ENTRY, [{style, ?wxTE_PROCESS_ENTER}]), 
    _ = wxSizer:add(HSz, TC2,  [{proportion,0}, {flag, ?wxEXPAND}]),
    wxTextCtrl:connect(TC1, command_text_updated),
    wxTextCtrl:connect(TC1, command_text_enter),
    wxTextCtrl:connect(TC1, kill_focus),
    wxTextCtrl:connect(TC2, command_text_enter),
    wxWindow:connect(Parent, command_button_clicked),

    #sub{name='Search Area', win=HSz, 
	 in=#sa{search=TC1,goto=TC2,radio={Nbtn,Pbtn,Cbtn}}}.

%%--Evaluator Area----------------------------------------------------

eval_area(Parent) -> 
    VSz = wxBoxSizer:new(?wxVERTICAL),
    HSz = wxBoxSizer:new(?wxHORIZONTAL),
    
    _ = wxSizer:add(HSz, wxStaticText:new(Parent, ?wxID_ANY, "Evaluator:"), 
		[{flag,?wxALIGN_CENTER_VERTICAL}]),
    TC = wxTextCtrl:new(Parent, ?EVAL_ENTRY, [{style, ?wxTE_PROCESS_ENTER}]), 
    _ = wxSizer:add(HSz, TC,  [{proportion,1}, {flag, ?wxEXPAND}]),
    _ = wxSizer:add(VSz, HSz, [{flag, ?wxEXPAND}]),
    TL = wxTextCtrl:new(Parent, ?EVAL_LOG, [{style, ?wxTE_DONTWRAP bor 
					  ?wxTE_MULTILINE bor ?wxTE_READONLY}]), 
    _ = wxSizer:add(VSz, TL, [{proportion,5}, {flag, ?wxEXPAND}]),
    
    wxTextCtrl:connect(TC, command_text_enter),
    #sub{name='Evaluator Area', win=VSz, in=TC, out=TL}.

%%--Bindings Area-----------------------------------------------------

bind_area(Parent) ->
    Style = {style, ?wxSW_3D bor  ?wxCLIP_CHILDREN},
    Win = wxSashWindow:new(Parent, [{id, ?SASH_EVAL},Style]),
    wxSashWindow:setSashVisible(Win, ?wxSASH_LEFT, true),

    BA = wxListCtrl:new(Win, [{style, ?wxLC_REPORT bor ?wxLC_SINGLE_SEL}]),
    LI = wxListItem:new(),
    
    wxListItem:setText(LI, "Name"), 
    wxListItem:setAlign(LI, ?wxLIST_FORMAT_LEFT),
    wxListCtrl:insertColumn(BA, 0, LI),
    
    wxListItem:setText(LI, "Value"), 
    wxListCtrl:insertColumn(BA, 1, LI),
    wxListItem:destroy(LI),

    wxListCtrl:setColumnWidth(BA, 0, 100),
    wxListCtrl:setColumnWidth(BA, 1, 150),
    wxListCtrl:connect(BA, command_list_item_selected), 
    wxListCtrl:connect(BA, command_list_item_activated), 
    
    #sub{name='Bindings Area', win=Win, out=BA}.

%%--Trace Area--------------------------------------------------------

trace_area(Parent) -> 
    Style = {style, ?wxSW_3D bor  ?wxCLIP_CHILDREN},
    Win = wxSashWindow:new(Parent, [{id, ?SASH_TRACE},
				    {size, {?WIN_W,?TRACE_H}}, Style]),
    wxSashWindow:setSashVisible(Win, ?wxSASH_TOP, true),
    wxWindow:setMinSize(Win, {500, ?TRACE_H}),
    TC = wxTextCtrl:new(Win, ?wxID_ANY, [{style, ?wxTE_MULTILINE bor ?wxTE_READONLY}]),
    #sub{name='Trace Area', win=Win, out=TC}.
    
%%====================================================================
%% 'Go To Line' and 'Search' help windows
%%====================================================================

helpwin(Type, WinInfo = #winInfo{sg=Sg =#sub{in=Sa}}) ->
    Wi = case Sg#sub.enable of
	     false -> configure(WinInfo#winInfo{sg=Sg#sub{enable=true}});
	     true ->  WinInfo
	 end,
    case Type of
	gotoline ->  wxWindow:setFocus(Sa#sa.goto);
	search   ->  wxWindow:setFocus(Sa#sa.search)
    end,
    Wi.
