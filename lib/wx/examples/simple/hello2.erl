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
%%
%%%-------------------------------------------------------------------
%%% File    : hello.erl
%%% Author  : Matthew Harrison <harryhuk at users.sourceforge.net>
%%% Description : _really_ minimal example of a wxerlang app
%%%               implemented with wx_object behaviour
%%%
%%% Created :  18 Sep 2008 by  Matthew Harrison <harryhuk at users.sourceforge.net>
%%%            Dan rewrote it to show wx_object behaviour
%%%-------------------------------------------------------------------
-module(hello2).
-include_lib("wx/include/wx.hrl").

-export([start/0]).
-compile(export_all).

-behavoiur(wx_object).

-record(state, {win}).

start() ->
    wx_object:start_link(?MODULE, [], []).

%% Init is called in the new process.
init([]) ->
    wx:new(),
    Frame = wxFrame:new(wx:null(), 
			-1, % window id
			"Hello World", % window title
			[{size, {600,400}}]),
    
    wxFrame:createStatusBar(Frame,[]),

    %% if we don't handle this ourselves, wxwidgets will close the window
    %% when the user clicks the frame's close button, but the event loop still runs
    wxFrame:connect(Frame, close_window),
    
    ok = wxFrame:setStatusText(Frame, "Hello World!",[]),
    wxWindow:show(Frame),
    {Frame, #state{win=Frame}}.


%% Handled as in normal gen_server callbacks
handle_info(Msg, State) ->
    io:format("Got Info ~p~n",[Msg]),
    {noreply,State}.

handle_call(Msg, _From, State) ->
    io:format("Got Call ~p~n",[Msg]),
    {reply,ok,State}.

%% Async Events are handled in handle_event as in handle_info
handle_event(#wx{event=#wxClose{}}, State = #state{win=Frame}) ->
    io:format("~p Closing window ~n",[self()]),
    ok = wxFrame:setStatusText(Frame, "Closing...",[]),
    wxWindow:destroy(Frame),
    {stop, normal, State}.

code_change(_, _, State) ->
    {stop, not_yet_implemented, State}.

terminate(_Reason, _State) ->
    ok.

