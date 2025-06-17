%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2009-2025. All Rights Reserved.
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

%% This is example of the widgets and usage of Wx.
%% Hopefully it will contain all implemented widgets, it's event handling
%% and some tutorials of how to use sizers and other stuff.

-module(ex_notificationMessage).

-include_lib("wx/include/wx.hrl").

-behaviour(wx_object).
-export([start/1, init/1,
	 terminate/2,  code_change/3,
	 handle_info/2, handle_call/3, handle_cast/2, handle_event/2]).

-record(state, {buttons, close, notifications, config, parent}).

start(Config) ->
    wx_object:start_link(?MODULE, Config, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(Config) ->
        wx:batch(fun() -> do_init(Config) end).

do_init(Config) ->
    Parent = proplists:get_value(parent, Config),  
    Panel = wxScrolledWindow:new(Parent),
    Sz = wxBoxSizer:new(?wxVERTICAL),
    wxWindow:setSizer(Panel, Sz),
    Version = {?wxMAJOR_VERSION, ?wxMINOR_VERSION, ?wxRELEASE_NUMBER},
    EventsEnabled = case Version of
        {Major, Minor, _} when Major >= 3, Minor >= 1 -> true;
        _ -> false
    end,
    EventsEnabled orelse demo:format(Config,"wxNotificationMessage events are not supported in ~p~n",[Version]),

    ButtBox = wxStaticBox:new(Panel, -1, "wxNotificationMessage"),
    wxStaticBox:setSizer(ButtBox, wxFlexGridSizer:new(3)),
    wxSizer:add(Sz, ButtBox, [{flag, ?wxALL bor ?wxEXPAND}, {proportion, 1}]),

    Configs = [
        {"Info", ?wxICON_INFORMATION},
        {"Warning", ?wxICON_WARNING},
        {"Error", ?wxICON_ERROR}
    ],
    {Buttons, Notifications} = create_buttons(ButtBox, Configs, EventsEnabled),

    CloseEmAll = wxButton:new(ButtBox, -1, [{label, "Close Em All"}]),
    wxSizer:add(wxStaticBox:getSizer(ButtBox), CloseEmAll),
    CloseId = wxButton:getId(CloseEmAll),

    wxWindow:connect(Panel, command_button_clicked),
    wxSizer:layout(Sz),
    wxWindow:refresh(Panel),
    wxScrolledWindow:setScrollRate(Panel, 5, 5),
    {Panel, #state{
        buttons=Buttons,
        notifications=Notifications,
        close=CloseId,
        config=Config,
        parent=Panel}}.

create_buttons(Parent, Configs, false) ->
    lists:unzip(lists:map(fun ({Label, Icon}) -> 
        Button = wxButton:new(Parent, -1, [{label, Label}]),
        wxSizer:add(wxWindow:getSizer(Parent), Button),
        ButtonId = wxButton:getId(Button),
        Notify = wxNotificationMessage:new(Label, [{message, "Example " ++ Label ++ " message"}, {flags, Icon}]),
        {ButtonId, Notify}
    end, Configs));

create_buttons(Parent, Configs, true) ->
    {Buttons0, Notifications0} = create_buttons(Parent, Configs, false),
    Events = [notification_message_click, notification_message_dismissed, notification_message_action],
    [wxWindow:connect(Notify, E) || Notify <- Notifications0, E <- Events],
    {Buttons1, Notifications1} = lists:unzip(lists:map(fun ({Label, Icon}) -> 
        Button = wxButton:new(Parent, -1, [{label, Label ++ " action"}]),
        wxSizer:add(wxWindow:getSizer(Parent), Button),
        ButtonId = wxButton:getId(Button),
        Notify = wxNotificationMessage:new(Label, [{message, "Example " ++ Label ++ " message"}, {flags, Icon}]),
        [wxWindow:connect(Notify, E) || E <- Events],
        wxNotificationMessage:addAction(Notify, -1, [{label, "An added action"}]),
        wxNotificationMessage:addAction(Notify, -1, [{label, "Another added action"}]),
        {ButtonId, Notify}
    end, Configs)),
    {Buttons0 ++ Buttons1, Notifications0 ++ Notifications1}.

%%%%%%%%%%%%
%% Async Events are handled in handle_event as in handle_info
handle_event(#wx{id=CloseId, event=#wxCommand{type=command_button_clicked}}, 
	     State = #state{close=CloseId, notifications=Ns}) ->
    [wxNotificationMessage:close(N) || N <- Ns],
    {noreply, State};

handle_event(#wx{id=Id, event=#wxCommand{type=command_button_clicked}}, 
	     State = #state{buttons=Bs, notifications=Ns}) ->
    {_, N} = lists:keyfind(Id, 1, lists:zip(Bs, Ns)),
    wxNotificationMessage:show(N),
    {noreply, State};

handle_event(Ev = #wx{}, State = #state{}) ->
    demo:format(State#state.config,"Got Event ~p~n",[Ev]),
    {noreply,State}.

%% Callbacks handled as normal gen_server callbacks
handle_info(Msg, State) ->
    demo:format(State#state.config, "Got Info ~p~n",[Msg]),
    {noreply,State}.

handle_call(shutdown, _From, State=#state{parent=Panel}) ->
    wxPanel:destroy(Panel),
    {stop, normal, ok, State};

handle_call(Msg, _From, State) ->
    demo:format(State#state.config,"Got Call ~p~n",[Msg]),
    {reply,ok,State}.

handle_cast(Msg, State) ->
    demo:format(State#state.config,"Got cast ~p~n",[Msg]),
    {noreply,State}.

code_change(_, _, State) ->
    {stop, ignore, State}.

terminate(_Reason, _) ->
    ok.

