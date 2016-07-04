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

-module(ex_graphicsContext).

-behaviour(wx_object).

%% Client API
-export([start/1]).

%% wx_object callbacks
-export([init/1, terminate/2,  code_change/3,
	 handle_info/2, handle_call/3,handle_cast/2, 	 
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
				 [{label, "wxGraphicsContext"}]),

    Win = wxPanel:new(Panel, []),
    Pen = ?wxBLACK_PEN,
    Brush = wxBrush:new({30, 175, 23, 127}),
    Font = ?wxITALIC_FONT,
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

handle_call(shutdown, _From, State=#state{parent=Panel}) ->
    wxPanel:destroy(Panel),
    {stop, normal, ok, State};

handle_call(Msg, _From, State) ->
    demo:format(State#state.config, "Got Call ~p\n", [Msg]),
    {reply,{error, nyi}, State}.

handle_cast(Msg, State) ->
    io:format("Got cast ~p~n",[Msg]),
    {noreply,State}.

code_change(_, _, State) ->
    {stop, ignore, State}.

terminate(_Reason, _State) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Local functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

draw(Win, Pen, Brush, Font) ->
    try
	Canvas = wxGraphicsContext:create(Win),
	wxGraphicsContext:setPen(Canvas, Pen),
	wxGraphicsContext:setBrush(Canvas, Brush),
	wxGraphicsContext:setFont(Canvas, Font, {0, 0, 50}),
	
	wxGraphicsContext:drawRoundedRectangle(Canvas, 35.0,35.0, 100.0, 50.0, 10.0),
	wxGraphicsContext:drawText(Canvas, "This text should be antialised", 60.0, 55.0),
	Path = wxGraphicsContext:createPath(Canvas),
	wxGraphicsPath:addCircle(Path, 0.0, 0.0, 40.0),
	wxGraphicsPath:closeSubpath(Path),
	wxGraphicsContext:translate(Canvas, 100.0, 250.0),
	
	F = fun(N) ->
		    wxGraphicsContext:scale(Canvas, 1.1, 1.1),
		    wxGraphicsContext:translate(Canvas, 15.0,-1.0*N),
		    wxGraphicsContext:drawPath(Canvas, Path)
	    end,
	wx:foreach(F, lists:seq(1,10)),
	wxGraphicsObject:destroy(Path),
	wxGraphicsObject:destroy(Canvas),
	ok
    catch _:{not_supported, _} ->
	    Err = "wxGraphicsContext not available in this build of wxwidgets",
	    io:format(Err,[])
    end.

