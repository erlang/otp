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

-module(ex_static).

-behaviour(wx_object).

%% Client API
-export([start/1]).

%% wx_object callbacks
-export([init/1, terminate/2,  code_change/3,
	 handle_info/2, handle_call/3, handle_cast/2, handle_event/2]).

-include_lib("wx/include/wx.hrl").

-record(state, 
	{
	  parent,
	  config
	}).

start(Config) ->
    wx_object:start_link(?MODULE, Config, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(Config) ->
    wx:batch(fun() -> do_init(Config) end).

do_init(Config) ->
    Parent = proplists:get_value(parent, Config),  
    Panel = wxScrolledWindow:new(Parent, []),
    wxScrolledWindow:setScrollRate(Panel, 5, 5),

    %% Setup sizers
    MainSizer = wxBoxSizer:new(?wxVERTICAL),
    TextSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, 
				 [{label, "wxStaticText"}]),
    BitmapSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, 
				       [{label, "wxStaticBitmap"}]),
    LineSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, 
				     [{label, "wxStaticLine"}]),

    %% Create static texts
    Texts = [wxStaticText:new(Panel, 1, "This is a regular text (left aligned)", []),
	     wxStaticText:new(Panel, 2, "This is a centered text",
			      [{style, ?wxALIGN_CENTER bor ?wxST_NO_AUTORESIZE}]),
	     wxStaticText:new(Panel, 3, "This is a right aligned text",
			      [{style, ?wxALIGN_RIGHT bor ?wxST_NO_AUTORESIZE}])],


    Image = wxImage:new("image.jpg", []),
    Bitmap = wxBitmap:new(wxImage:scale(Image,
					round(wxImage:getWidth(Image)*1.5),
					round(wxImage:getHeight(Image)*1.5),
					[{quality, ?wxIMAGE_QUALITY_HIGH}])),
    StaticBitmap = wxStaticBitmap:new(Panel, 1, Bitmap),

    Line = wxStaticLine:new(Panel, [{style, ?wxLI_HORIZONTAL}]),
    Line2 = wxStaticLine:new(Panel, [{style, ?wxLI_VERTICAL},
				     {size, {-1, 100}}]),

    %% Add to sizers
    [wxSizer:add(TextSizer, Text, [{flag, ?wxEXPAND bor ?wxALL},
				   {border, 10}]) || Text <- Texts],
    wxSizer:add(BitmapSizer, StaticBitmap, []),
    wxSizer:add(LineSizer, Line, [{flag, ?wxTOP bor ?wxBOTTOM bor ?wxEXPAND},
				  {border, 10}]),
    wxSizer:add(LineSizer, Line2, [{flag, ?wxLEFT},
				   {border, 50}]),

    wxSizer:add(MainSizer, TextSizer, [{flag, ?wxEXPAND}]),
    wxSizer:add(MainSizer, BitmapSizer, []),
    wxSizer:add(MainSizer, LineSizer, [{flag, ?wxEXPAND}]),

    wxPanel:setSizer(Panel, MainSizer),
    {Panel, #state{parent=Panel, config=Config}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Async Events are handled in handle_event as in handle_info
handle_event(#wx{}, State = #state{}) ->
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

