%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2009-2013. All Rights Reserved.
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

-module(ex_canvas).

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
				 [{label, "Various shapes"}]),

    Button = wxButton:new(Panel, ?wxID_ANY, [{label, "Redraw"}]),

    Canvas = wxPanel:new(Panel, [{style, ?wxFULL_REPAINT_ON_RESIZE}]),

    wxPanel:connect(Canvas, paint, [callback]),
    wxPanel:connect(Canvas, size),
    wxPanel:connect(Button, command_button_clicked),

    %% Add to sizers
    wxSizer:add(Sizer, Button, [{border, 5}, {flag, ?wxALL}]),
    wxSizer:addSpacer(Sizer, 5),
    wxSizer:add(Sizer, Canvas, [{flag, ?wxEXPAND},
				{proportion, 1}]),

    wxSizer:add(MainSizer, Sizer, [{flag, ?wxEXPAND},
				   {proportion, 1}]),

    wxPanel:setSizer(Panel, MainSizer),
    wxSizer:layout(MainSizer),

    {W,H} = wxPanel:getSize(Canvas),
    Bitmap = wxBitmap:new(erlang:max(W,30),erlang:max(30,H)),
    
    {Panel, #state{parent=Panel, config=Config,
		   canvas = Canvas, bitmap = Bitmap}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Sync event from callback events, paint event must be handled in callbacks
%% otherwise nothing will be drawn on windows.
handle_sync_event(#wx{event = #wxPaint{}}, _wxObj,
		  #state{canvas=Canvas, bitmap=Bitmap}) ->
    DC = wxPaintDC:new(Canvas),
    redraw(DC, Bitmap),
    wxPaintDC:destroy(DC),
    ok.

%% Async Events are handled in handle_event as in handle_info
handle_event(#wx{event = #wxCommand{type = command_button_clicked}},
	     State = #state{}) ->
    Image = wxImage:new("image.jpg"),
    Image2 = wxImage:scale(Image, wxImage:getWidth(Image) div 3,
			   wxImage:getHeight(Image) div 3),
    Bmp = wxBitmap:new(Image2),
    wxImage:destroy(Image),
    wxImage:destroy(Image2),
    {W,H} = wxPanel:getSize(State#state.canvas),
    Positions = lists:map(fun(_) ->
				  get_pos(W,H)
			  end, lists:seq(1,(W+H) div 20)),
    Fun = fun(DC) ->
		  wxDC:clear(DC),
		  lists:foreach(fun({X,Y}=Pos) ->
					wxDC:setBrush(DC, ?wxTRANSPARENT_BRUSH),
					wxDC:setPen(DC, wxPen:new(?wxBLACK, [{width, 2}])),
					case X rem 6 of
					    0 -> wxDC:drawBitmap(DC, Bmp, Pos);
					    1 -> wxDC:setBrush(DC, ?wxRED_BRUSH),
						 wxDC:drawRectangle(DC, Pos, {20,20});
					    2 -> wxDC:setBrush(DC, ?wxBLUE_BRUSH),
						 wxDC:drawCircle(DC, {X+10, Y+10}, 15);
					    3 -> wxDC:setPen(DC, wxPen:new({200,200,0,255}, [{width, 4}])),
						 wxDC:drawLine(DC, Pos, get_pos(W,H));
					    4 -> wxDC:setBrush(DC, ?wxGREEN_BRUSH),
						 wxDC:drawEllipse(DC, Pos, {60,20});
					    _ -> wxDC:drawLabel(DC, "Erlang /", {X,Y,60,20}),
						 wxDC:drawRotatedText(DC, "OTP", {X+60,Y}, 340.0)
					end
				end, Positions)
	  end,
    draw(State#state.canvas, State#state.bitmap, Fun),    
    wxBitmap:destroy(Bmp),
    {noreply, State};
handle_event(#wx{event = #wxSize{size={W,H}}},
	     State = #state{bitmap=Prev}) ->
    Bitmap = wxBitmap:new(W,H),
    draw(State#state.canvas, Bitmap, fun(DC) -> wxDC:clear(DC) end),
    wxBitmap:destroy(Prev),
    {noreply, State#state{bitmap = Bitmap}};
handle_event(Ev = #wx{}, State = #state{}) ->
    demo:format(State#state.config, "Got Event ~p\n", [Ev]),
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

%% Buffered makes it all appear on the screen at the same time
draw(Canvas, Bitmap, Fun) ->
    MemoryDC = wxMemoryDC:new(Bitmap),
    Fun(MemoryDC),

    CDC = wxWindowDC:new(Canvas),
    wxDC:blit(CDC, {0,0},
	      {wxBitmap:getWidth(Bitmap), wxBitmap:getHeight(Bitmap)},
	      MemoryDC, {0,0}),    
    wxWindowDC:destroy(CDC),
    wxMemoryDC:destroy(MemoryDC).

redraw(DC, Bitmap) ->
    MemoryDC = wxMemoryDC:new(Bitmap),
    wxDC:blit(DC, {0,0},
	      {wxBitmap:getWidth(Bitmap), wxBitmap:getHeight(Bitmap)},
	      MemoryDC, {0,0}),
    wxMemoryDC:destroy(MemoryDC).

get_pos(W,H) ->
    {random:uniform(W), random:uniform(H)}.
