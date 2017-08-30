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

-module(ex_canvas_paint).

-behaviour(wx_object).

%% Client API
-export([start/1]).

%% wx_object callbacks
-export([init/1, terminate/2,  code_change/3,
	 handle_info/2, handle_call/3, handle_cast/2, handle_event/2, handle_sync_event/3]).

-include_lib("wx/include/wx.hrl").

-record(state, 
	{
	  parent,
	  config,
	  canvas,
	  pen,
	  brush,
	  old_pos,
	  bitmap
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
				 [{label, "wxDC"}]),
    
    %% Create the window to paint on and make it repaint the whole window on resize
    Canvas = wxPanel:new(Panel, [{style, ?wxFULL_REPAINT_ON_RESIZE}]),
    wxPanel:setToolTip(Canvas,
		       "Left-click and hold to draw something - release to stop drawing.\n"
		       "Middle-click to fill with pink\n"
		       "Middle-dclick to fill with white.\n"
		       "Right-click to clear."),

    %% Create a wxPen and a WxBrush and set its colors to draw with
    Brush = wxBrush:new(?wxWHITE),
    Pen = wxPen:new(?wxBLACK, [{width, 2}]),
    
    PrintButton = wxButton:new(Panel, ?wxID_ANY, [{label, "Print"}]),

    Bitmap = wxBitmap:new(30,30),

    %% Add to sizers
    wxSizer:add(Sizer, Canvas, [{flag, ?wxEXPAND},
				{proportion, 1}]),

    wxSizer:add(MainSizer, PrintButton, []),
    wxSizer:add(MainSizer, Sizer, [{flag, ?wxEXPAND},
				   {proportion, 1}]),
    
    wxPanel:connect(PrintButton, command_button_clicked),
    wxPanel:connect(Canvas, paint, [callback]),
    wxPanel:connect(Canvas, size),
    wxPanel:connect(Canvas, left_down),
    wxPanel:connect(Canvas, left_dclick),
    wxPanel:connect(Canvas, left_up),
    wxPanel:connect(Canvas, right_down),
    wxPanel:connect(Canvas, middle_down),
    wxPanel:connect(Canvas, middle_dclick),
    wxPanel:setSizer(Panel, MainSizer),
    wxSizer:layout(MainSizer),
    {Panel, #state{parent=Panel, config=Config,
		   canvas = Canvas, pen = Pen,
		   brush = Brush, bitmap = Bitmap}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Sync event from callback events, paint event must be handled in callbacks
%% otherwise nothing will be drawn on windows.
handle_sync_event(#wx{event = #wxPaint{}}, _wxObj, #state{canvas=Canvas, bitmap=Bitmap}) ->
    DC = wxPaintDC:new(Canvas),
    redraw(DC, Bitmap),
    wxPaintDC:destroy(DC),
    ok.

%% Print what's drawn
handle_event(#wx{event = #wxCommand{type = command_button_clicked}},
	     State = #state{bitmap=Bitmap}) ->
    PD = wxPrintData:new(),
    PDD = wxPrintDialogData:new(PD),
    PSDD = wxPageSetupDialogData:new(PD),
    Fun =
	fun(This,_Page) ->
		MX = MY = 500,  
		wxPrintout:fitThisSizeToPageMargins(This, {MX,MY}, PSDD),
		{_X,_Y,W,H} = wxPrintout:getLogicalPageMarginsRect(This, PSDD),
		wxPrintout:offsetLogicalOrigin(This,(W-MX) div 2, (H-MY) div 2),
		DC = wxPrintout:getDC(This),
		redraw(DC, Bitmap),
		true
	end,
    Printout1 = wxPrintout:new("Print", Fun,
			       [{getPageInfo, fun getPageInfo/1}]),
    Printout2 = wxPrintout:new("Print", Fun,
			       [{getPageInfo, fun getPageInfo/1}]),
    Preview = wxPrintPreview:new(Printout1, [{printoutForPrinting,Printout2},{data,PDD}]), 
    case wxPrintPreview:isOk(Preview) of
	true ->
	    Env = wx:get_env(), 
	    spawn_link(fun() ->
			       wx:set_env(Env),
			       PF = wxPreviewFrame:new(Preview, State#state.parent, []),
			       wxPreviewFrame:centre(PF, [{dir, ?wxBOTH}]),
			       wxPreviewFrame:initialize(PF),
			       wxPreviewFrame:centre(PF),
			       wxPreviewFrame:show(PF)
		       end);
	false ->
	    io:format("Could not create preview window.\n"
		      "Perhaps your current printer is not set correctly?~n", []),
	    wxPrintPreview:destroy(Preview)
    end,
    {noreply, State#state{}};
%% Draw a line
handle_event(#wx{event = #wxMouse{type = motion, x = X, y = Y}},
	     State = #state{canvas = Canvas, pen = Pen, brush = Brush}) ->
    Fun =
	fun(DC) ->
		wxDC:setPen(DC, Pen),
		wxBrush:setColour(Brush, ?wxBLACK),
		wxDC:setBrush(DC, Brush),
		wxDC:drawLine(DC, {X,Y}, State#state.old_pos)
	end,
    draw(Canvas,State#state.bitmap, Fun),
    {noreply, State#state{old_pos = {X,Y}}};
%% Resize event
handle_event(#wx{event = #wxSize{size = {W,H}}}, State = #state{bitmap=Prev}) ->
    case W > 0 andalso H > 0 of
	true ->
	    wxBitmap:destroy(Prev),
	    Bitmap = wxBitmap:new(W,H),
	    draw(State#state.canvas, Bitmap, fun(DC) -> wxDC:clear(DC) end),
	    {noreply, State#state{bitmap=Bitmap}};
	false ->
	    {noreply, State}
    end;
handle_event(#wx{event = #wxMouse{type = left_dclick,x = X,y = Y}}, State = #state{}) ->
    wxPanel:connect(State#state.canvas, motion),
    {noreply, State#state{old_pos = {X,Y}}};
handle_event(#wx{event = #wxMouse{type = left_down,x = X,y = Y}}, State = #state{}) ->
    wxPanel:connect(State#state.canvas, motion),
    {noreply, State#state{old_pos = {X,Y}}};
%% Fill with pink color
handle_event(#wx{event = #wxMouse{type = middle_down,x = X, y =Y}}, State = #state{}) ->
    case os:type() of
	{_, darwin} ->
	    io:format("Fill doesn't work on Darwin ~n",[]);
	_ ->
	    ok
    end,
    Fun =
	fun(DC) ->
		wxBrush:setColour(State#state.brush, {255,125,255,255}),
		wxDC:setBrush(DC, State#state.brush),
		wxDC:floodFill(DC, {X,Y}, ?wxBLACK, [{style, ?wxFLOOD_BORDER}])
	end,
    
    draw(State#state.canvas, State#state.bitmap, Fun),
    {noreply, State};
%% Fill with white color
handle_event(#wx{event = #wxMouse{type = middle_dclick,x = X, y =Y}}, State = #state{}) ->
    Fun =
	fun(DC) ->
		wxBrush:setColour(State#state.brush, ?wxWHITE),
		wxDC:setBrush(DC, State#state.brush),
		wxDC:floodFill(DC, {X,Y}, ?wxBLACK, [{style, ?wxFLOOD_BORDER}])
	end,
    
    draw(State#state.canvas,  State#state.bitmap,Fun),
    {noreply, State};
handle_event(#wx{event = #wxMouse{type = left_up}}, State = #state{}) ->
    wxPanel:disconnect(State#state.canvas, motion),
    {noreply, State};
%% Clear the DC
handle_event(#wx{event = #wxMouse{type = right_down}}, State = #state{}) ->
    draw(State#state.canvas, State#state.bitmap, fun(DC) -> wxDC:clear(DC) end),
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

terminate(_Reason, _) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Local functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

draw(Canvas, Bitmap, Fun) ->
    MemoryDC = wxMemoryDC:new(Bitmap),
    CDC = wxClientDC:new(Canvas),

    Fun(MemoryDC),
    wxDC:blit(CDC, {0,0},
	      {wxBitmap:getWidth(Bitmap), wxBitmap:getHeight(Bitmap)},
	      MemoryDC, {0,0}),

    wxClientDC:destroy(CDC),
    wxMemoryDC:destroy(MemoryDC).

redraw(DC, Bitmap) ->
    try
	MemoryDC = wxMemoryDC:new(Bitmap),

	wxDC:blit(DC, {0,0},
		  {wxBitmap:getWidth(Bitmap), wxBitmap:getHeight(Bitmap)},
		  MemoryDC, {0,0}),

	wxMemoryDC:destroy(MemoryDC)
    catch error:{{badarg,_},_} -> %% Bitmap have been deleted
	    ok
    end.

getPageInfo(_This) ->
    {1,1,1,1}.
