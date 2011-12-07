%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011. All Rights Reserved.
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
-module(wx_obj_test).
-include_lib("wx/include/wx.hrl").

-export([start/1]).

%% wx_object callbacks
-export([init/1, handle_info/2, terminate/2, code_change/3, handle_call/3,
	 handle_sync_event/3, handle_event/2, handle_cast/2]).

-record(state, {frame, panel, opts}).

start(Opts) ->
    wx_object:start_link(?MODULE, [{parent, self()}, Opts], []).

init(Opts) ->
    put(parent_pid, proplists:get_value(parent, Opts)),
    Frame = wxFrame:new(wx:null(), ?wxID_ANY, "Test wx_object", [{size, {500, 400}}]),
    Sz = wxBoxSizer:new(?wxHORIZONTAL),
    Panel = wxPanel:new(Frame),
    wxSizer:add(Sz, Panel, [{flag, ?wxEXPAND}, {proportion, 1}]),
    wxPanel:connect(Panel, size, [{skip, true}]),
    wxPanel:connect(Panel, paint, [callback, {userData, proplists:get_value(parent, Opts)}]),
    wxWindow:show(Frame),
    {Frame, #state{frame=Frame, panel=Panel, opts=Opts}}.

handle_sync_event(Event = #wx{obj=Panel}, WxEvent, #state{opts=Opts}) ->
    DC=wxPaintDC:new(Panel),  %% We must create & destroy paintDC, or call wxEvent:skip(WxEvent))
    wxPaintDC:destroy(DC),    %% in sync_event. Otherwise wx on windows keeps sending the events.
    Pid = proplists:get_value(parent, Opts),
    true = is_pid(Pid),
    Pid ! {sync_event, Event, WxEvent},
    ok.

handle_event(Event, State = #state{opts=Opts}) ->
    Pid = proplists:get_value(parent, Opts),
    Pid ! {event, Event},
    {noreply, State}.

handle_call(What, From, State) when is_function(What) ->
    Result = What(State),
    {reply, {call, Result, From}, State};
handle_call(What, From, State) ->
    {reply, {call, What, From}, State}.

handle_cast(What, State = #state{opts=Opts})  when is_function(What) ->
    Result = What(State),
    Pid = proplists:get_value(parent, Opts),
    Pid ! {cast, Result},
    {noreply, State};

handle_cast(What, State = #state{opts=Opts}) ->
    Pid = proplists:get_value(parent, Opts),
    Pid ! {cast, What},
    {noreply, State}.

handle_info(What, State = #state{opts=Opts}) ->
    Pid = proplists:get_value(parent, Opts),
    Pid ! {info, What},
    {noreply, State}.

terminate(What, #state{opts=Opts}) ->
    Pid = proplists:get_value(parent, Opts),
    Pid ! {terminate, What},
    ok.

code_change(Ver1, Ver2, State = #state{opts=Opts}) ->
    Pid = proplists:get_value(parent, Opts),
    Pid ! {code_change, Ver1, Ver2},
    State.
