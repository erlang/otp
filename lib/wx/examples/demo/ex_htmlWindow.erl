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

-module(ex_htmlWindow).

-behaviour(wx_object).

%% Client API
-export([start/1]).

%% wx_object callbacks
-export([init/1, terminate/2,  code_change/3,
	 handle_info/2, handle_call/3, handle_cast/2, handle_event/2]).

-include("../../include/wx.hrl").

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
				 [{label, "wxHtmlWindow"}]),

    %% Create the wxHtmlWindow
    HtmlWin = wxHtmlWindow:new(Panel, []),
    %% Load a file and display it
    wxHtmlWindow:loadFile(HtmlWin, "ex_htmlWindow.html"),

    %% Add to sizers
    wxSizer:add(Sizer, HtmlWin, [{flag, ?wxEXPAND}, {proportion, 1}]),
    wxSizer:add(MainSizer, Sizer, [{flag, ?wxEXPAND}, {proportion, 1}]),

    wxHtmlWindow:connect(HtmlWin, command_html_link_clicked, [{skip,true}]),

    wxPanel:setSizer(Panel, MainSizer),
    {Panel, #state{parent=Panel, config=Config}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Async Events are handled in handle_event as in handle_info
handle_event(#wx{event = #wxHtmlLink{linkInfo = #wxHtmlLinkInfo{href=Link}}},
	     State = #state{}) ->
    demo:format(State#state.config, "You clicked link ~p.\n", [Link]),
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

