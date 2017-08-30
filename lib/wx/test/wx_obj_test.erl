%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011-2017. All Rights Reserved.
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
-module(wx_obj_test).
-include_lib("wx/include/wx.hrl").

-export([start/1, stop/1, who_are_you/1]).

%% wx_object callbacks
-export([init/1, handle_info/2, terminate/2, code_change/3, handle_call/3,
	 handle_sync_event/3, handle_event/2, handle_cast/2]).

-record(state, {parent, me, opts, user_state}).

start(Opts) ->
    wx_object:start_link(?MODULE, [{parent, self()}| Opts], []).

stop(Object) ->
    wx_object:stop(Object).

who_are_you(Object) ->
    wx_object:call(Object, who_are_you).

init(Opts) ->
    Parent = proplists:get_value(parent, Opts),
    put(parent_pid, Parent),
    Init = proplists:get_value(init, Opts),
    {Obj, UserState} = Init(),
    {Obj, #state{me=Obj, parent=Parent, opts=Opts, user_state=UserState}}.

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

handle_call(who_are_you, _From, State = #state{me=Me}) ->
    {reply, Me, State};
handle_call(What, From, State = #state{user_state=US}) when is_function(What) ->
    Result = What(US),
    {reply, {call, Result, From}, State};
handle_call(What, From, State) ->
    {reply, {call, What, From}, State}.

handle_cast(What, State = #state{parent=Pid, user_state=US})  when is_function(What) ->
    Result = What(US),
    Pid ! {cast, Result},
    {noreply, State};

handle_cast(What, State = #state{parent=Pid}) ->
    Pid ! {cast, What},
    {noreply, State}.

handle_info({call_undef_fun, {Mod, Fun}}, State) ->
    Mod:Fun(),
    {noreply, State};
handle_info(What, State = #state{parent=Pid}) ->
    Pid ! {info, What},
    {noreply, State}.

terminate(What, #state{parent=Pid, opts=Opts, user_state=US}) ->
    case proplists:get_value(terminate, Opts) of
	undefined ->
	    ok;
	{Mod, Fun} ->
	    Mod:Fun();
	Terminate ->
	    Terminate(US)
    end,
    Pid ! {terminate, What},
    ok.

code_change(Ver1, Ver2, State = #state{parent=Pid}) ->
    Pid ! {code_change, Ver1, Ver2},
    State.
