%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2008-2025. All Rights Reserved.
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
%% This file is generated DO NOT EDIT

%% This module is actually handwritten see ../api_gen/wx_extra/wxEvtHandler.erl
%%
-module(wxEvtHandler).
-moduledoc """
The Event handler

A class that can handle events from the windowing system. `m:wxWindow` is (and
therefore all window classes are) derived from this class.

To get events from wxwidgets objects you subscribe to them by calling `connect/3`.

If the `callback` option is not supplied events are sent as messages.

These messages will be `#wx{}` where `EventRecord` is a record that depends on
the `wxEventType`. The records are defined in: `wx/include/wx.hrl`.

If a callback was supplied to connect, the callback will be invoked (in another
process) to handle the event. The callback should be of arity 2.

`fun Callback (EventRecord::wx(), EventObject::wxObject()).`

Note: The callback will be in executed in new process each time.

See:
[Overview events](https://docs.wxwidgets.org/3.2/overview_events.html#overview_events_processing)

wxWidgets docs:
[wxEvtHandler](https://docs.wxwidgets.org/3.2/classwx_evt_handler.html)
""".
-include("wxe.hrl").
-include("../include/wx.hrl").

%% API
-export([connect/2, connect/3, disconnect/1, disconnect/2, disconnect/3]).

%% internal exports
-export([connect_impl/2, disconnect_impl/2]).

-export_type([wxEvtHandler/0, wx/0, event/0]).
-type wxEvtHandler() :: wx:wx_object().

-doc(#{equiv => connect(This, EventType, [])}).
-spec connect(This::wxEvtHandler(), EventType::wxEventType()) -> 'ok'.
connect(This, EventType) ->
    connect(This, EventType, []).

-doc """
This function subscribes to events.

Subscribes to events of type `EventType`, in the range `id`, `lastId`.

The events will be received as messages if no callback is supplied.

Options

id:`{id, integer()} `The identifier (or first of the identifier range) to be
associated with this event handler. Default is ?wxID_ANY

lastid:`{lastId,integer()} `The second part of the identifier range. If used
'id' must be set as the starting identifier range. Default is ?wxID_ANY

skip:`{skip,boolean()} `If skip is true further event_handlers will be called.
This is not used if the 'callback' option is used. Default is `false`.

callback:`{callback,function()} `Use a
callback`fun(EventRecord::wx(),EventObject::wxObject()) `to process the event.
Default not specified i.e. a message will be delivered to the process calling
this function.

userData:`{userData,term()} `An erlang term that will be sent with the event.
Default: [].
""".
-spec connect(This::wxEvtHandler(), EventType::wxEventType(), [Option]) -> 'ok' when
      Option :: {'id', integer()} | {'lastId', integer()} | {'skip', boolean()} |
		'callback' | {'callback', function()} | {'userData', term()}.
connect(This=#wx_ref{type=ThisT}, EventType, Options) ->
    EvH = parse_opts(Options, #evh{et=EventType}),
    ?CLASS(ThisT,wxEvtHandler),
    case wxe_util:connect_cb(This, EvH) of
	ok -> ok;
	{badarg, event_type} ->
	    erlang:error({badarg,EventType})
    end.

parse_opts([{callback,Fun}|R], Opts) when is_function(Fun) ->
    %% Check Fun Arity?
    parse_opts(R, Opts#evh{cb=Fun});
parse_opts([{callback,CB={nospawn, Fun}}|R], Opts) when is_function(Fun) ->
    parse_opts(R, Opts#evh{cb=CB});
parse_opts([callback|R], Opts) ->
    parse_opts(R, Opts#evh{cb=self()});
parse_opts([{userData, UserData}|R],Opts) ->
    parse_opts(R, Opts#evh{userdata=UserData});
parse_opts([{skip, Skip}|R],Opts) when is_boolean(Skip) ->
    parse_opts(R, Opts#evh{skip=Skip});
parse_opts([{id, Id}|R],Opts) when is_integer(Id) ->
    parse_opts(R, Opts#evh{id=Id});
parse_opts([{lastId, Id}|R],Opts) when is_integer(Id) ->
    parse_opts(R, Opts#evh{lastId=Id});
parse_opts([_BadArg|R], Opts) ->
    parse_opts(R, Opts);
parse_opts([], Opts = #evh{id=Id,lastId=Lid,skip=Skip, cb=CB}) -> 
    if 
	Skip =/= undefined andalso CB =/= 0 -> 
	    erlang:error({badarg, {combined, skip, callback}});
	Lid =/= ?wxID_ANY andalso Id =:= ?wxID_ANY  ->
	    erlang:error({badarg, no_start_identifier_range});
	Skip =:= undefined ->  %% Default
	    Opts#evh{skip=false};
	true ->
	    Opts
    end.

-doc(#{equiv => disconnect(This, null, [])}).
-spec disconnect(This::wxEvtHandler()) -> boolean().
disconnect(This=#wx_ref{type=ThisT,ref=_ThisRef}) ->
    ?CLASS(ThisT,wxEvtHandler),
    disconnect(This, null, []).

-doc(#{equiv => disconnect(This, EventType, [])}).
-spec disconnect(This::wxEvtHandler(), EventType::wxEventType()) -> boolean().
disconnect(This=#wx_ref{type=ThisT,ref=_ThisRef}, EventType) when is_atom(EventType) ->
    ?CLASS(ThisT,wxEvtHandler),
    disconnect(This, EventType, []).

-doc """
This function unsubscribes the process or callback fun from the event handler.

EventType may be the atom 'null' to match any eventtype. Notice that the options
skip and userdata is not used to match the eventhandler.
""".
-spec disconnect(This::wxEvtHandler(), EventType::wxEventType(), [Option]) -> boolean() when
      Option :: {'id', integer()} | {'lastId', integer()} | {'callback', function()}.
disconnect(This=#wx_ref{type=ThisT,ref=_ThisRef}, EventType, Opts) ->
    ?CLASS(ThisT,wxEvtHandler),
    EvH = parse_opts(Opts, #evh{et=EventType}),
    case wxe_util:disconnect_cb(This, EvH) of
	{badarg, event_type} ->
	    erlang:error({badarg,EventType});
	Bool ->
	    Bool
    end.

-doc false.
connect_impl(#wx_ref{type=ThisT}=This,
	     #evh{id=Winid, lastId=LastId, et=EventType,
		  skip=Skip, userdata=UserData, cb=FunID})
  when is_integer(FunID)->
    wxe_util:queue_cmd(This, Winid, LastId, Skip, UserData,
                       FunID, EventType, ThisT, ?get_env(), 100),
    wxe_util:rec(100).

-doc false.
disconnect_impl(#wx_ref{type=_ThisT}=This,
		#evh{id=Winid, lastId=LastId, et=EventType,
		     handler=#wx_ref{type=wxeEvtListener}=EvtList}) ->
    wxe_util:queue_cmd(EvtList, This, Winid,LastId, EventType, ?get_env(), 101),
    wxe_util:rec(101).

