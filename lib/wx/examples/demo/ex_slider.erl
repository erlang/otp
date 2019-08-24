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

-module(ex_slider).

-behaviour(wx_object).

-export([start/1, init/1, terminate/2,  code_change/3,
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
    Panel = wxPanel:new(Parent, []),

    %% Setup sizers
    MainSizer = wxBoxSizer:new(?wxVERTICAL),
    Sizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, 
				 [{label, "Horizontal wxSlider"}]),
    Sizer2 = wxStaticBoxSizer:new(?wxHORIZONTAL, Panel, 
				 [{label, "Vertical wxSlider"}]),

    %% Setup slider with range from 0 to 100
    %% and a start value of 25
    Min = 0,
    Max = 100,
    StartValue = 25,
    %% Horizontal slider (default) with label
    Slider = wxSlider:new(Panel, 1, StartValue, Min, Max,
				[{style, ?wxSL_HORIZONTAL bor
				  ?wxSL_LABELS}]),
    %% Horizontal inverse slider with label
    InverseSlider = wxSlider:new(Panel, 2, StartValue, Min, Max,
				 [{style, ?wxSL_HORIZONTAL bor
				   ?wxSL_LABELS bor
				   ?wxSL_INVERSE}]),
    VerticalSlider = wxSlider:new(Panel, 3, StartValue, Min, Max,
				 [{style, ?wxSL_VERTICAL bor
				   ?wxSL_LABELS}]),
    InverseVerticalSlider = wxSlider:new(Panel, 4, StartValue, Min, Max,
					 [{style, ?wxSL_VERTICAL bor
					   ?wxSL_LABELS bor
					   ?wxSL_INVERSE}]),

    %% Add to sizers
    wxSizer:add(Sizer, Slider, [{flag, ?wxEXPAND}]),
    wxSizer:add(Sizer, InverseSlider, [{flag, ?wxEXPAND}]),
    wxSizer:add(Sizer2, VerticalSlider, [{flag, ?wxEXPAND},
					 {proportion, 1}]),
    wxSizer:add(Sizer2, InverseVerticalSlider, [{flag, ?wxEXPAND},
						{proportion, 1}]),

    wxSizer:add(MainSizer, Sizer, [{flag, ?wxEXPAND}]),
    wxSizer:add(MainSizer, Sizer2, [{flag, ?wxEXPAND},
				    {proportion, 1}]),

    wxPanel:setSizer(Panel, MainSizer),
    {Panel, #state{parent=Panel, config=Config}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Async Events are handled in handle_event as in handle_info
handle_event(Ev = #wx{}, State = #state{}) ->
    demo:format(State#state.config,"Got Event ~p\n",[Ev]),
    {noreply, State}.

%% Callbacks handled as normal gen_server callbacks
handle_info(Msg, State) ->
    demo:format(State#state.config, "Got Info ~p\n",[Msg]),
    {noreply, State}.

handle_call(shutdown, _From, State=#state{parent=Panel}) ->
    wxPanel:destroy(Panel),
    {stop, normal, ok, State};

handle_call(Msg, _From, State) ->
    demo:format(State#state.config,"Got Call ~p\n",[Msg]),
    {reply, {error, nyi},State}.

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

