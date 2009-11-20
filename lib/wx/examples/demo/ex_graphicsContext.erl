%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2009. All Rights Reserved.
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

-module(ex_graphicsContext).

-behaviour(wx_object).

%% Client API
-export([start/1]).

%% wx_object callbacks
-export([init/1, terminate/2,  code_change/3,
	 handle_info/2, handle_call/3,
	 handle_event/2, handle_sync_event/3]).

-include_lib("wx/include/wx.hrl").

-record(state, 
	{
	  parent,
	  config,
	  win,
	  pen,
	  brush,
	  font
	}).

start(Config) ->
    wx_object:start_link(?MODULE, Config, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(Config) ->
    wx:batch(fun() -> do_init(Config) end).

do_init(Config) ->
    Parent = proplists:get_value(parent, Config),  
    Panel = wxPanel:new(Parent, []),

    %% Setup sizers
    MainSizer = wxBoxSizer:new(?wxVERTICAL),
    Sizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, 
				 [{label, "wxGrapicsContext"}]),

    Win = wxPanel:new(Panel, []),
    Pen = wxPen:new(),
    Brush = wxBrush:new(?wxBLACK),
    Font = wxFont:new(),
    wxFont:setWeight(Font, ?wxBOLD),

    wxPanel:connect(Win, paint, [callback]),

    %% Add to sizers
    wxSizer:add(Sizer, Win, [{flag, ?wxEXPAND},
				{proportion, 1}]),
    wxSizer:add(MainSizer, Sizer, [{flag, ?wxEXPAND},
				   {proportion, 1}]),

    wxPanel:setSizer(Panel, MainSizer),
    {Panel, #state{parent=Panel, config=Config, win = Win,
		   pen = Pen, brush = Brush, font = Font}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Sync events i.e. from callbacks must return ok, it can not return a new state.
%% Do the redrawing here.
handle_sync_event(#wx{event = #wxPaint{}},_,
		  #state{win=Win, pen = Pen, brush = Brush, font = Font}) ->
    %% PaintDC must be created in a callback to work on windows.
    DC = wxPaintDC:new(Win),
    %% Nothing is drawn until wxPaintDC is destroyed.
    draw(DC, Pen, Brush, Font),
    wxPaintDC:destroy(DC),
    ok.
%% Async Events are handled in handle_event as in handle_info
handle_event(#wx{}, State) ->
    {noreply, State}.

%% Callbacks handled as normal gen_server callbacks
handle_info(Msg, State) ->
    demo:format(State#state.config, "Got Info ~p\n", [Msg]),
    {noreply, State}.

handle_call(Msg, _From, State) ->
    demo:format(State#state.config, "Got Call ~p\n", [Msg]),
    {reply,{error, nyi}, State}.

code_change(_, _, State) ->
    {stop, ignore, State}.

terminate(_Reason, _State) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Local functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

draw(Win, Pen0, _Brush0, Font0) ->
    try
	Canvas = wxGraphicsContext:create(Win),
	Pen    = wxGraphicsContext:createPen(Canvas, Pen0),
	wxGraphicsContext:setPen(Canvas, Pen),
	Brush = wxGraphicsContext:createLinearGradientBrush(Canvas, 0.0,0.0, 30.0,30.0,
							    {200,50,50,50},
							    {200,50,50,200}),
	wxGraphicsContext:setBrush(Canvas, Brush),
	Font  = wxGraphicsContext:createFont(Canvas, Font0),
	wxGraphicsContext:setFont(Canvas, Font),
	
	wxGraphicsContext:drawRoundedRectangle(Canvas, 35.0,35.0, 100.0, 50.0, 10.0),
	wxGraphicsContext:drawText(Canvas, "Welcome", 60.0, 55.0),
	Path = wxGraphicsContext:createPath(Canvas),
	wxGraphicsPath:addCircle(Path, 0.0, 0.0, 40.0),
	wxGraphicsPath:closeSubpath(Path),
	wxGraphicsContext:translate(Canvas, 100.0, 100.0),

	Brush2 = wxGraphicsContext:createLinearGradientBrush(Canvas, 0.0,0.0, 30.0,30.0,
							     {50,200,50,50},
							     {50,50,200,50}),
	wxGraphicsContext:setBrush(Canvas, Brush2),
	
	F = fun(_) ->
		    wxGraphicsContext:scale(Canvas, 1.1, 1.1),
		    wxGraphicsContext:translate(Canvas, 3.0,3.0),
		    wxGraphicsContext:drawPath(Canvas, Path)
	    end,
	wx:foreach(F, lists:seq(1,5)),
	ok
    catch _:{not_supported, _} ->
	    Err = "wxGraphicsContext not available in this build of wxwidgets",
	    io:format(Err,[])
    end.

