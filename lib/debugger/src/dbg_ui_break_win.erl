%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2002-2009. All Rights Reserved.
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
-module(dbg_ui_break_win).

%% External exports
-export([create_win/5,
	 update_functions/2,
	 handle_event/2]).

-record(winInfo, {type,            % line | conditional | function
		  win,             % gsobj()
		  packer,          % gsobj() | undefined
		  entries,         % [{atom|integer, GSobj()}]
		  trigger,         % enable | disable | delete
		  ok,              % gsobj()
		  cancel,          % gsobj()
		  listbox,         % gsobj()
		  funcs=[]         % [[Name, Arity]]
		 }).

%%====================================================================
%% External exports
%%====================================================================

%%--------------------------------------------------------------------
%% create_win(GS, Pos, Type, Mod, Line) -> #winInfo{}
%%   GS = graphics system identifier
%%   Pos = {X, Y}
%%     X = Y = integer()
%%   Type =  line | conditional | function
%%   Mod = atom() | ""
%%   Line = integer() | ""
%%--------------------------------------------------------------------
create_win(GS, {X, Y}, function, Mod, _Line) ->
    Pad = 8,
    W = 230,

    Font = dbg_ui_win:font(normal),

    %% Window
    Win = gs:window(GS, [{title, "Function Break"}, {x, X}, {y, Y},
			 {destroy, true}, {configure, true},
			 {keypress, true}, {data, window}]),

    %% Frame
    Frm = gs:frame(Win, [{x, 0}, {y, 0}, {width, W}, {height, 190},
			 {packer_x, [{fixed, 70}, {stretch, 1, W-80},
				     {fixed, 10}]},
			 {packer_y, [{fixed, 10}, {fixed, 30},
				     {stretch, 1, 100}, {fixed, 40}]}]),

    %% Create input field (label+entry)
    gs:label(Frm, [{label, {text,"Module:"}}, {font, Font}, {align, e},
		   {pack_x, 1}, {pack_y, 2}]),
    Ent = gs:entry(Frm, [{text, Mod},
			 {pack_x, 2}, {pack_y, 2},
			 {keypress, true}, {setfocus, true},
			 {buttonpress, true}]),
    Entries = [{Ent, atom}],

    %% Create a listbox containing the functions of the module
    gs:label(Frm, [{label, {text,"Function:"}}, {font, Font}, {align, ne},
		   {pack_x, 1}, {pack_y, 3}]),
    Lb = gs:listbox(Frm, [{bw, 2}, {relief, ridge}, {vscroll, right},
			  {pack_x, 2}, {pack_y, 3},
			  {selectmode, multiple}]),

    %% Add Ok and Cancel buttons
    {Wbtn, Hbtn} = dbg_ui_win:min_size(["Ok","Cancel"], 70, 30),
    Bot = gs:frame(Frm, [{pack_x, {1, 3}}, {pack_y, 4}]),
    Ok = gs:button(Bot, [{x, Pad}, {y, Pad},
			 {width, Wbtn}, {height, Hbtn},
			 {label, {text,"Ok"}}, {font, Font}]),
    Cancel = gs:button(Bot, [{x, W-Pad-Wbtn}, {y, Pad},
			     {width, Wbtn}, {height, Hbtn},
			     {label, {text,"Cancel"}}, {font, Font}]),

    Wfrm = gs:read(Frm, width), Hfrm = gs:read(Frm, height),
    gs:config(Win, [{width, Wfrm}, {height, Hfrm}, {map, true}]),
    #winInfo{type=function, win=Win,
	     packer=Frm, entries=Entries, trigger=enable,
	     ok=Ok, cancel=Cancel, listbox=Lb, funcs=[]};
create_win(GS, {X, Y}, Type, Mod, Line) ->
    Pad = 8,
    W = 230,

    Font = dbg_ui_win:font(normal),

    %% Window
    Title = case Type of
		line -> "Line Break";
		conditional -> "Conditional Break"
	    end,
    Win = gs:window(GS, [{title, Title}, {x, X}, {y, Y},
			 {destroy, true}]),

    %% Create input fields (label+entry)
    {Wlbl, Hlbl} = dbg_ui_win:min_size(["C-Function:"], 10, 30),
    Went = W-Wlbl-2*Pad,
    Labels = case Type of
		 line ->
		     [{atom,"Module:",Mod}, {integer,"Line:",Line}];
		 conditional ->
		     [{atom,"Module:",Mod}, {integer,"Line:",Line},
		      {atom,"C-Module:",""}, {atom,"C-Function:",""}]
	     end,
    Fun = fun({DataType, Label, Default}, Yin) ->
		  gs:create(label, Win, [{x, Pad}, {y, Yin},
					 {width,Wlbl}, {height,Hlbl},
					 {label, {text,Label}},
					 {font, Font}, {align, e}]),
		  Ent = gs:create(entry, Win, [{x, Pad+Wlbl}, {y, Yin},
					       {width, Went},
					       {height, Hlbl},
					       {text, Default},
					       {keypress, true}]),
		  {{Ent, DataType}, Yin+Hlbl}
	  end,
    {Entries, Yacc} = lists:mapfoldl(Fun, Pad, Labels),
    {First, _DataType} = hd(Entries),
    gs:config(First, [{buttonpress, true}, {setfocus, true}]),

    %% Add 'trigger action' buttons
    {Wlbl2, Hlbl2} = dbg_ui_win:min_size(["Trigger Action"], 100, 20),
    Wfrm = Wlbl2+8, Hfrm = Hlbl2*4+4,
    Grp = erlang:now(),
    Frm = gs:frame(Win, [{x, W/2-Wfrm/2-2}, {y, Yacc+Pad-2},
			 {width, Wfrm}, {height, Hfrm}, {bw, 2}]),
    gs:label(Frm, [{label, {text, "Trigger Action"}}, {font, Font},
		   {x, 2}, {y, 0}, {width, Wlbl2}, {height, Hlbl2}]),
    gs:radiobutton(Frm, [{label, {text, "Enable"}}, {font, Font},
			 {x, 10}, {y, Hlbl2},
			 {width, Wlbl2-10}, {height, Hlbl2},
			 {align, w}, {group, Grp},
			 {data, {trigger, enable}},
			 {select, true}]),
    gs:radiobutton(Frm, [{label, {text, "Disable"}}, {font, Font},
			 {x, 10}, {y, Hlbl2*2},
			 {width, Wlbl2-10}, {height, Hlbl2},
			 {align, w}, {group, Grp},
			 {data, {trigger, disable}}]),
    gs:radiobutton(Frm, [{label, {text, "Delete"}}, {font, Font},
			 {x, 10}, {y, Hlbl2*3},
			 {width, Wlbl2-10}, {height, Hlbl2},
			 {align, w}, {group, Grp},
			 {data, {trigger, delete}}]),

    %% Add Ok and Cancel buttons
    {Wbtn, Hbtn} = dbg_ui_win:min_size(["Ok","Cancel"], 70, 30),
    Ybtn = Yacc + Pad + Hfrm + Pad,
    Ok = gs:button(Win, [{x, Pad}, {y, Ybtn},
			 {width, Wbtn}, {height, Hbtn},
			 {label, {text,"Ok"}}, {font, Font}]),
    gs:button(Win, [{x, W-Pad-Wbtn}, {y, Ybtn},
		    {width, Wbtn}, {height, Hbtn},
		    {label, {text,"Cancel"}}, {font, Font}]),

    Hwin = Ybtn + Hbtn + Pad,
    gs:config(Win, [{width, W}, {height, Hwin}, {map, true}]),

    #winInfo{type=Type, win=Win,
	     entries=Entries, trigger=enable, ok=Ok}.

%%--------------------------------------------------------------------
%% update_functions(WinInfo, Funcs) -> WinInfo
%%   WinInfo = #winInfo{}
%%   Funcs = [{Name, Arity}]
%%     Name = atom()
%%     Arity = integer()
%%--------------------------------------------------------------------
update_functions(WinInfo, Funcs) ->
    Items = lists:map(fun([N, A]) -> io_lib:format("~p/~p", [N, A]) end,
		      Funcs),
    gs:config(WinInfo#winInfo.listbox, [{items, Items},
					{setfocus, true}]),
    WinInfo#winInfo{funcs=Funcs}.

%%--------------------------------------------------------------------
%% handle_event(GSEvent, WinInfo) -> Command
%% GSEvent = {gs, Id, Event, Data, Arg}
%% WinInfo = #winInfo{}
%% Command = ignore
%%         | stopped
%%         | {win, WinInfo}
%%         | {module, Mod}
%%         | {break, [[Mod, Line]], Action}
%%         | {break, [[Mod, Line, CMod, CFunc]], Action}
%%         | {break, [[Mod, Func, Arity]], Action}
%%--------------------------------------------------------------------
handle_event({gs, _Id, destroy, _Data, _Arg}, _WinInfo) ->
    stopped;
handle_event({gs, _Id, configure, _Data, [W, H|_]}, WinInfo) ->
    gs:config(WinInfo#winInfo.packer, [{width, W-10}, {height, H-10}]),
    gs:config(WinInfo#winInfo.cancel, [{x, W-80}]),
    ignore;
handle_event({gs, Ent, buttonpress, _,[N,X0,Y0|_]}, WinInfo) when N>1 ->
    %% Right (middle) mouse button click in module entry, display a
    %% menu containing all interpreted modules
    Mods = int:interpreted(),
    X = gs:read(Ent, x) + X0,
    Y = gs:read(Ent, y) + Y0,
    Menu = gs:menu(WinInfo#winInfo.win, [{post_at,{X,Y}}]),
    lists:foreach(fun(Mod) ->
			  gs:menuitem(Menu, [{label,{text,Mod}},
					     {data,{module,Mod}}])
		  end,
		  Mods),
    ignore;
handle_event({gs, LB, keypress, window, [Key|_]}, WinInfo) ->
    %% Used for functional break window, since listboxes for some
    %% reason doesn't generate keypress events
    if
	Key/='Tab', Key/='Return' ->
	    ignore;
	true ->
	    handle_event({gs, LB, click, listbox, ["Ok"]}, WinInfo)
    end;
handle_event({gs, Ent, keypress, Data, [Key|_]}, WinInfo) ->
    case WinInfo#winInfo.type of
	function when Key/='Tab', Key/='Return' ->
	    case gs:read(WinInfo#winInfo.listbox, items) of
		[] -> ignore;
		_Items ->
		    gs:config(WinInfo#winInfo.listbox, clear),
		    {win, WinInfo#winInfo{funcs=[]}}
	    end;
	function -> % 'Return' | 'Tab' pressed in Module entry
	    case check_input(WinInfo#winInfo.entries) of
		error -> ignore;
		[Mod] -> {module, Mod}
	    end;
	_Type when Key=='Tab'; Key=='Return' ->
	    case next_entry(Ent, WinInfo#winInfo.entries) of
		last ->
		    gs:config(WinInfo#winInfo.ok, flash),
		    handle_event({gs, Ent, click, Data, ["Ok"]}, WinInfo);
		Next ->
		    gs:config(Next, {setfocus, true}),
		    ignore
	    end;
	_Type -> ignore
    end;
handle_event({gs, _Id, click, _Data, ["Ok"|_]}, WinInfo) ->
    case check_input(WinInfo#winInfo.entries) of
	error -> ignore;
	Data when WinInfo#winInfo.type/=function ->
	    {break, [Data], WinInfo#winInfo.trigger};
	[Mod] -> % Function break window
	    case gs:read(WinInfo#winInfo.listbox, selection) of
		[] ->
		    {module, Mod};
		IndexL ->
		    Funcs = WinInfo#winInfo.funcs,
		    Breaks =
			[[Mod|lists:nth(Index+1, Funcs)] || Index <- IndexL],
		    {break, Breaks, enable}
	    end
    end;
handle_event({gs, _Id, click, _Data, ["Cancel"|_]}, _WinInfo) ->
    stopped;
handle_event({gs, _Id, click, {trigger,Trigger}, _Arg}, WinInfo) ->
    {win, WinInfo#winInfo{trigger=Trigger}};
handle_event({gs, _Id, click, {module, Mod}, _Arg}, WinInfo) ->
    {Ent, _DataType} = hd(WinInfo#winInfo.entries),
    gs:config(Ent, {insert,{0,Mod}}),
    ignore;
handle_event(_GSEvent, _WinInfo) ->
    ignore.

check_input(Entries) ->
    check_input(Entries, []).
check_input([{Entry, Type} | Entries], Data) ->
    Str = gs:read(Entry, text),
    case erl_scan:string(Str) of
	{ok, [{Type, _Line, Val}], _EndLine} ->
	    check_input(Entries, [Val|Data]);
	_Error -> error
    end;
check_input([], Data) -> lists:reverse(Data).

next_entry(Entry, [{Entry, _Type}]) ->
    last;
next_entry(Entry, [{Entry, _Type1}, {Next, _Type2}|_]) ->
    Next;
next_entry(Entry, [_|Entries]) ->
    next_entry(Entry, Entries).
