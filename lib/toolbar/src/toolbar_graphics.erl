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
-module(toolbar_graphics).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Erlang Toolbar
%
%%% Description %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Toolbar graphics.
% The Toolbar window looks something like this:
%
%  |-----------------------------|
%  | File Tools             Help |
%  |-----------------------------|
%  | |-----|  |-----|   |-----|  |
%  | |     |  |     |   |     |  |
%  | |Icon1|  |Icon2|...|IconN|  |
%  | |-----|  |-----|   |-----|  |
%  |-----------------------------|
%  | Help text area              |
%  |-----------------------------|
%
%%% Includes %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
-include("toolbar.hrl").
%
%%% Internal data structures %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Toolbar window record
-record(tbwindow,
	{window,menubar,canvas,labelframe,
	 label,helpmenu,
	 no_of_buttons,
	 min_height,min_width,cur_height,icons}).
%
%%% Constants %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Window width
-define(width,215).
%
% Icon width and height
-define(icon,34).
%
% Margin around icons
-define(pad,0).
%
% Default label width and height
-define(wlabel,50).
-define(hlabel,15).
%
% Default button width and height
-define(wbutton,50).
-define(hbutton,30).
%
%%% Exports %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
-export([event/4]).
-export([draw_window/1,redraw_window/2,already_added/2,add_icon/2]).
-export([get_window/1]).
-export([cursor/2]).
-export([listen_configure/1]).
-export([display_show/2,display_clear/1]).
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Exported functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%----------------------------------------
% event(Data,GsEvent,Data,Args) => Event
%   Data - term()
%   GsEvent - GS event
%   Data, Args - Data and Arg fields associated with the GS event
%   Event - {display,String} | display_clear | noevent | {start,{M,F,A}} |
%           update_toolbar | create_tool_file | add_gs_contribs |
%           {help,Html} | about_help | {redraw,{Width,Height}} | quit |
%           {newData,NewData}
%   String - string()
%   M, F - atom() Module and function name
%   A - [term()] Function argument
%   Html - string() HTML file | nofile
%   Width, Height - integer()
%   NewData - term()
% Tries to convert a GS event to an internal toolbar event. The separation
% is intented to keep the implementation details of the graphics hidden
% for toolbar.erl. Pure graphical events triggered by the GS event will
% be executed by this function.
% The Data field is used for saving information between different events
% (without having to use put/get or ets). Right now it is only used to save
% the coordinates of the last canvasbutton pressed, making it possible to
% check if the canvasbutton is released with the mouse moved outside the
% button (= no action) or with the mouse still inside the button (= action).
%----------------------------------------
%%% Mouse enters a icon, display short help message
event(_LoopData,enter,{canvasbutton,_Cbtn,{_Start,{message,String}}},_Args) ->
    {display,String};

%% Mouse leaves a icon, clear display area
event(_LoopData,leave,{canvasbutton,_Cbtn,_Data},_Args) ->
    display_clear;

%% An icon is pressed, create graphical illusion of this
event(_LoopData,buttonpress,{canvasbutton,Canvasbutton,_},_Args) ->
    canvasbutton:press(Canvasbutton),
    {newData,canvasbutton:read(Canvasbutton,coords)};

%% An icon is released, create graphical illusion of this
event(LoopData,buttonrelease,{canvasbutton,Cbtn,{{start,Start},_Msg}},
      [_,X,Y|_]) ->
    canvasbutton:release(Cbtn),
    case within(X,Y,LoopData) of
	true ->
	    {start,Start};
	false ->
	    noevent
    end;

%%% Update Toolbar button pressed
event(_LoopData,click,_Data,["Update Toolbar"|_]) ->
    update_toolbar;

%%% Tool configuration button pressed
event(_LoopData,click,_Data,["Create Tool File..."|_]) ->
    create_tool_file;

%%% Add GS contribution button pressed
event(_LoopData,click,_Data,["Add GS Contributions"|_]) ->
    add_gs_contribs;

%%% Help menu button selected
event(_LoopData,click,{help,Html},_Args) ->
    {help,Html};

%%% About Help menu button selected
event(_LoopData,click,about_help,_Args) ->
    about_help;

%% Window resized, redraw it
event(_LoopData,configure,_Data,[Width,Height|_]) ->
    {redraw,{Width,Height}};
	    
%%% Quit button pressed
event(_LoopData,click,_Data,["Quit"|_]) ->
    quit;

%%% Window closed
event(_LoopData,destroy,_Data,_Args) ->
    quit;

event(_LoopData,_GsEvent,_Data,_Args) ->
    noevent.

%=============================================================================
% Main window functions
%=============================================================================

%----------------------------------------
% draw_window(S) => Window
%   S - pid() GS
%   Window - tbwindow record
% This functions create the main window, initially without any tool icons
%----------------------------------------
draw_window(S) ->

    Norm = ?icon + 2*?pad,

    %% Main window
    Win = gs:create(window,S,[{title,"Erlang Tools"},{width,?width}]),

    %% Menu bar with menu buttons
    Menubar = gs:create(menubar,Win,[]),

    %% File menu
    File = gs:create(menubutton,Menubar,[{label,{text,"File"}},{side,left}]),
    FileM = gs:create(menu,File,[]),
    gs:create(menuitem,FileM,[{label,{text,"Update Toolbar"}}]),
    gs:create(menuitem,FileM,[{label,{text,"Quit"}}]),

    %% Tools menu
    Tool = gs:create(menubutton,Menubar,[{label,{text,"Tools"}},{side,left}]),
    ToolM = gs:create(menu,Tool,[]),
    gs:create(menuitem,ToolM,[{label,{text,"Create Tool File..."}}]),
    gs:create(menuitem,ToolM,[{label,{text,"Add GS Contributions"}}]),

    %% Help menu
    Help = gs:create(menubutton,Menubar,[{label,{text,"Help"}},{side,right}]),
    HelpM = gs:create(menu,Help,[]),
    gs:create(menuitem,HelpM,[{label,{text,"About..."}},
			      {data,about_help}]),
    gs:create(menuitem,HelpM,[{label,{text,"Toolbar"}},
			      {data,{help,toolbar_lib:help_file()}}]),
    gs:create(menuitem,HelpM,[{label,{text,"OTP"}},
			      {data,{help,toolbar_lib:otp_file()}}]),
    gs:create(menuitem,HelpM,[{itemtype,separator}]),
    
    %% Check height of menu bar
    H = gs:read(Menubar,height),

    %% Now the height of the window can be computed
    Height = H+Norm+?hlabel+2*?pad,
    gs:config(Win,{height,Height}),

    %% Canvas, here will the Tool canvasbuttons be inserted
    Canvas = gs:create(canvas,Win,[{width,?width},{height,Norm},{x,0},{y,H}]),

    %% Label for displaying help messages and the frame containing it
    LabelF = gs:create(frame,Win,[{bg,green},{bw,1},
				  {width,?width},{height,?hlabel+2*?pad},
				  {x,0},{y,H+Norm}]),
    Label = gs:create(label,LabelF,[{align,w},{height,?hlabel},
				    {width,?width},{x,?pad},{y,?pad},
				    {label,{text,string:copies(" ",30)}}]),

    gs:config(Win,{map,true}),

    #tbwindow{window=Win,
	      menubar=Menubar,canvas=Canvas,labelframe=LabelF,
	      label=Label,helpmenu=HelpM,
	      no_of_buttons=0,
	      min_height=Height,min_width=?width,cur_height=Height,
	      icons=[]}.

%----------------------------------------
% redraw_window(Window,{NewWidth,NewHeight}) => NewWindow
%   Window, NewWindow - tbwindow record
%   NewWidth, NewHeight - integer()
% Redraw main window contents according to a new size
%----------------------------------------
redraw_window(Window,{NewWidth,NewHeight}) ->
    
    MinWidth = Window#tbwindow.min_width,
    if
	NewWidth=<MinWidth ->
	    true;
	true ->
	    gs:config(Window#tbwindow.canvas,{width,NewWidth}),
	    gs:config(Window#tbwindow.labelframe,{width,NewWidth}),
	    gs:config(Window#tbwindow.label,{width,NewWidth-2*?pad})
    end,

    MinHeight = Window#tbwindow.min_height,
    if
	NewHeight=<MinHeight ->
	    Window;
	true ->

	    %% Compute size difference
	    Diff = NewHeight - Window#tbwindow.cur_height,

	    %% Resize button frame
	    Canvas = Window#tbwindow.canvas,
	    gs:config(Canvas,{height,gs:read(Canvas,height)+Diff}),

	    %% Move label frame accordingly
	    LabelF = Window#tbwindow.labelframe,
	    gs:config(LabelF,{y,gs:read(LabelF,y)+Diff}),

	    %% Return updated tbwindow record
	    Window#tbwindow{cur_height=NewHeight}
    end.

%----------------------------------------
% already_added(Window,ToolInfo) => true | false
%   Window - tbwindow record
%   ToolInfo - toolinfo record
% Returns true if ToolInfo contains information about a tool that
% is already included in Window
%----------------------------------------
already_added(Window,ToolInfo) ->
    already_added2(Window#tbwindow.icons,ToolInfo#toolinfo.tool).

%----------------------------------------
% already_added2(ToolInfos,Tool) => true | false
%   ToolInfos - [toolinfo record]
%   Tool - atom() Tool name
%----------------------------------------
already_added2([#toolinfo{tool=Tool}|_Rest],Tool) ->
    true;
already_added2([_|Rest],Tool) ->
    already_added2(Rest,Tool);
already_added2([],_ToolInfo) ->
    false.

%----------------------------------------
% add_icon(Window,ToolInfo) => NewWindow
%   Window, NewWindow - tbwindow record
%   ToolInfo - toolinfo record
% Add an icon to the main window
%----------------------------------------
add_icon(Window,ToolInfo) ->
    Norm = ?icon + 2*?pad,
    
    %% Extend window if necessary
    N = Window#tbwindow.no_of_buttons,
    ReqWidth = N*Norm+Norm,
    CurWidth = gs:read(Window#tbwindow.window,width),
    if
	ReqWidth>CurWidth ->
	    gs:config(Window#tbwindow.window,{width,ReqWidth}),
	    gs:config(Window#tbwindow.canvas,{width,ReqWidth}),
	    gs:config(Window#tbwindow.labelframe,{width,ReqWidth}),
	    gs:config(Window#tbwindow.label,{width,ReqWidth-2*?pad});
	true ->
	    true
    end,
    
    %% Insert icon into button frame
    canvasbutton:create(Window#tbwindow.canvas,
			[{image,ToolInfo#toolinfo.icon},
			 {x,N*Norm+?pad},{y,?pad},
			 {width,?icon},{height,?icon},
			 {data,{{start,ToolInfo#toolinfo.start},
				{message,ToolInfo#toolinfo.message}}}]),
    
    %% Insert tool name into help menu (if there is any help available)
    case ToolInfo#toolinfo.html of 
	nofile ->
	    true;
	Html ->
	    gs:create(menuitem,Window#tbwindow.helpmenu,
		      [{label,{text,ToolInfo#toolinfo.tool}},
		       {data,{help,Html}}])
    end,
    
    MinWidth = gs:read(Window#tbwindow.window,width),
    Window#tbwindow{no_of_buttons=N+1,min_width=MinWidth,
		    icons=[ToolInfo|Window#tbwindow.icons]}.

%----------------------------------------
% get_window(Window) -> gs_obj()
%   Window - tbwindow record
%----------------------------------------
get_window(Window) ->    
    Window#tbwindow.window.

%----------------------------------------
% cursor(Window,Cursor)
%   Window - tbwindow record
%   Cursor - arrow | busy
%----------------------------------------
cursor(Window,Cursor) ->
    gs:config(Window#tbwindow.window,{cursor,Cursor}).

%----------------------------------------
% listen_configure(Window)
%   Window - tbwindow record
% Configure Window to listen for configure events
%----------------------------------------
listen_configure(Window) ->
    gs:config(Window#tbwindow.window,{configure,true}).

%----------------------------------------
% display_show(Window,Text)
%   Window - tbwindow record
%   Text - string()
% Display text in the help text area
%----------------------------------------
display_show(Window,Text) ->
    gs:config(Window#tbwindow.label,{label,{text,Text}}).

%----------------------------------------
% display_clear(Window)
%   Window - tbwindow record
% Clear the help text area
%----------------------------------------
display_clear(Window) ->
    display_show(Window,"").

%%% Internal functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%----------------------------------------
% within(X,Y,[{Left,Top},{Right,Bot}]) => true | false
% Return true if {X,Y} is within the given rectangle.
%----------------------------------------
within(X,Y,[{L,T},{R,B}]) ->
    if
	X>=L,
	X=<R,
	Y>=T,
	Y=<B ->
	    true;
	true ->
	    false
    end.
