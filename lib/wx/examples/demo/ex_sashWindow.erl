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

-module(ex_sashWindow).

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
	  config,
	  top_sash,
	  bottom_sash
	}).

-define(TOP_SASH, 1).
-define(BOTTOM_SASH, 2).

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
    Sizer = wxBoxSizer:new(?wxVERTICAL),

    TopSash = wxSashWindow:new(Panel, [{id, ?TOP_SASH},
				       {style, ?wxSW_3D}]),
    Win1 = wxPanel:new(TopSash, []),
    wxStaticText:new(Win1, ?wxID_ANY, "This is the top sash", []),
    BottomSash = wxSashWindow:new(Panel, [{id, ?BOTTOM_SASH},
					  {style, ?wxSW_3D}]),
    Win2 = wxPanel:new(BottomSash, []),
    wxStaticText:new(Win2, ?wxID_ANY, "This is the bottom sash", []),

    %% Make the bottom edge of the top sash dragable
    wxSashWindow:setSashVisible(TopSash, ?wxSASH_BOTTOM, true),
    wxPanel:connect(Panel, sash_dragged),
    wxPanel:connect(Panel, size),

    %% Add to sizers
    Options = [{flag, ?wxEXPAND}, {proportion, 1}],
    wxSizer:add(Sizer, TopSash, Options),
    wxSizer:add(Sizer, BottomSash, Options),
    wxSizer:add(MainSizer, Sizer, Options),
    wxPanel:setSizer(Panel, MainSizer),
    wxSizer:fit(MainSizer, Panel),
    wxSizer:setSizeHints(MainSizer, Panel),
    {Panel, #state{parent=Panel, config=Config,
		   top_sash = TopSash, bottom_sash = BottomSash}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Async Events are handled in handle_event as in handle_info
handle_event(#wx{event = #wxSash{dragRect = {_X,Y, _W, H}}},
	     State = #state{top_sash = TopSash,
			    bottom_sash = BottomSash}) ->
    {OldW, OldH} = wxPanel:getSize(State#state.parent),
    Diff = OldH - H,
    {OldX, _} = wxSashWindow:getPosition(BottomSash),
    wxSashWindow:setMinSize(BottomSash, {OldW,Diff}),
    wxSashWindow:setMinSize(TopSash, {OldW,H}),
    wxSashWindow:setSize(BottomSash, {OldX, Y,OldW,Diff}),
    wxSashWindow:setSize(TopSash, {OldW,H}),
    wxPanel:refresh(State#state.parent),
    {noreply, State};
handle_event(#wx{event = #wxSize{size = {W, H}}},
	     State = #state{top_sash = TopSash,
			    bottom_sash = BottomSash}) ->
    wxSashWindow:setMinSize(TopSash, {W, H div 2}),
    wxSashWindow:setMinSize(BottomSash, {W, H div 2}),
    wxSashWindow:setSize(TopSash, {W, H div 2}),
    wxSashWindow:setSize(BottomSash, {0,H div 2,W,H div 2}),
    wxPanel:refresh(State#state.parent),
    {noreply, State};
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

terminate(_Reason, _State) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Local functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

