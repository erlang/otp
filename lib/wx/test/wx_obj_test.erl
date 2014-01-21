%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011-2013. All Rights Reserved.
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

-export([start/1, stop/2]).

%% wx_object callbacks
-export([init/1, handle_info/2, terminate/2, code_change/3, handle_call/3,
	 handle_sync_event/3, handle_event/2, handle_cast/2]).

-record(state, {parent, opts, user_state}).

start(Opts) ->
    wx_object:start_link(?MODULE, [{parent, self()}| Opts], []).

stop(Object, Fun) ->
    wx_object:call(Object, {stop, Fun}).

init(Opts) ->
    Parent = proplists:get_value(parent, Opts),
    put(parent_pid, Parent),
    Init = proplists:get_value(init, Opts),
    {Obj, UserState} = Init(),
    {Obj, #state{parent=Parent, opts=Opts, user_state=UserState}}.

handle_sync_event(Event = #wx{obj=Panel, event=#wxPaint{}},
		  WxEvent, #state{parent=Parent, user_state=US, opts=Opts}) ->
    case proplists:get_value(redraw, Opts) of
	undefined ->
	    DC=wxPaintDC:new(Panel),  %% We must create & destroy paintDC, or call wxEvent:skip(WxEvent))
	    wxPaintDC:destroy(DC),    %% in sync_event. Otherwise wx on windows keeps sending the events.
	    Parent ! {sync_event, Event, WxEvent};
	Redraw ->
	    Redraw(Event, WxEvent, US)
    end,
    ok;
handle_sync_event(Event, WxEvent, #state{parent=Parent}) ->
    Parent ! {sync_event, Event, WxEvent},
    ok.

handle_event(Event, State = #state{parent=Parent}) ->
    Parent ! {event, Event},
    {noreply, State}.

handle_call(What, From, State = #state{user_state=US}) when is_function(What) ->
    Result = What(US),
    {reply, {call, Result, From}, State};
handle_call({stop, Fun}, From, State = #state{user_state=US}) ->
    {stop, Fun(US), {stop, From}, State};
handle_call(What, From, State) ->
    {reply, {call, What, From}, State}.

handle_cast(What, State = #state{parent=Pid, user_state=US})  when is_function(What) ->
    Result = What(US),
    Pid ! {cast, Result},
    {noreply, State};

handle_cast(What, State = #state{parent=Pid}) ->
    Pid ! {cast, What},
    {noreply, State}.

handle_info(What, State = #state{parent=Pid}) ->
    Pid ! {info, What},
    {noreply, State}.

terminate(What, #state{parent=Pid}) ->
    Pid ! {terminate, What},
    ok.

code_change(Ver1, Ver2, State = #state{parent=Pid}) ->
    Pid ! {code_change, Ver1, Ver2},
    State.
