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

-module(ex_textCtrl).

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
				 [{label, "wxTextCtrl single line"}]),
    Sizer2 = wxStaticBoxSizer:new(?wxVERTICAL, Panel, 
				  [{label, "wxTextCtrl single line password"}]),
    Sizer3 = wxStaticBoxSizer:new(?wxVERTICAL, Panel, 
				  [{label, "wxTextCtrl multiline"}]),

    TextCtrl  = wxTextCtrl:new(Panel, 1, [{value, "This is a single line wxTextCtrl"},
					 {style, ?wxDEFAULT}]),
    TextCtrl2 = wxTextCtrl:new(Panel, 2, [{value, "password"},
					  {style, ?wxDEFAULT bor ?wxTE_PASSWORD}]),
    TextCtrl3 = wxTextCtrl:new(Panel, 3, [{value, "This is a\nmultiline\nwxTextCtrl"},
					  {style, ?wxDEFAULT bor ?wxTE_MULTILINE}]),

    %% Add to sizers
    wxSizer:add(Sizer, TextCtrl,  [{flag, ?wxEXPAND}]),
    wxSizer:add(Sizer2, TextCtrl2, []),
    wxSizer:add(Sizer3, TextCtrl3, [{flag, ?wxEXPAND}, {proportion, 1}]),

    wxSizer:add(MainSizer, Sizer,  [{flag, ?wxEXPAND}]),
    wxSizer:addSpacer(MainSizer, 10),
    wxSizer:add(MainSizer, Sizer2, [{flag, ?wxEXPAND}]),
    wxSizer:addSpacer(MainSizer, 10),
    wxSizer:add(MainSizer, Sizer3, [{flag, ?wxEXPAND}, {proportion, 1}]),

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
    {reply, {error,nyi}, State}.

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

