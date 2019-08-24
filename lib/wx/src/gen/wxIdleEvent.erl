%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxidleevent.html">wxIdleEvent</a>.
%% <dl><dt>Use {@link wxEvtHandler:connect/3.} with EventType:</dt>
%% <dd><em>idle</em></dd></dl>
%% See also the message variant {@link wxEvtHandler:wxIdle(). #wxIdle{}} event record type.
%%
%% <p>This class is derived (and can use functions) from:
%% <br />{@link wxEvent}
%% </p>
%% @type wxIdleEvent().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxIdleEvent).
-include("wxe.hrl").
-export([canSend/1,getMode/0,moreRequested/1,requestMore/1,requestMore/2,setMode/1]).

%% inherited exports
-export([getId/1,getSkipped/1,getTimestamp/1,isCommandEvent/1,parent_class/1,
  resumePropagation/2,shouldPropagate/1,skip/1,skip/2,stopPropagation/1]).

-export_type([wxIdleEvent/0]).
-deprecated([canSend/1]).

%% @hidden
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxIdleEvent() :: wx:wx_object().
%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxidleevent.html#wxidleeventcansend">external documentation</a>.
-spec canSend(Win) -> boolean() when
	Win::wxWindow:wxWindow().
canSend(#wx_ref{type=WinT,ref=WinRef}) ->
  ?CLASS(WinT,wxWindow),
  wxe_util:call(?wxIdleEvent_CanSend,
  <<WinRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxidleevent.html#wxidleeventgetmode">external documentation</a>.
%%<br /> Res = ?wxIDLE_PROCESS_ALL | ?wxIDLE_PROCESS_SPECIFIED
-spec getMode() -> wx:wx_enum().
getMode() ->
  wxe_util:call(?wxIdleEvent_GetMode,
  <<>>).

%% @equiv requestMore(This, [])
-spec requestMore(This) -> 'ok' when
	This::wxIdleEvent().

requestMore(This)
 when is_record(This, wx_ref) ->
  requestMore(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxidleevent.html#wxidleeventrequestmore">external documentation</a>.
-spec requestMore(This, [Option]) -> 'ok' when
	This::wxIdleEvent(),
	Option :: {'needMore', boolean()}.
requestMore(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxIdleEvent),
  MOpts = fun({needMore, NeedMore}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(NeedMore)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxIdleEvent_RequestMore,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxidleevent.html#wxidleeventmorerequested">external documentation</a>.
-spec moreRequested(This) -> boolean() when
	This::wxIdleEvent().
moreRequested(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxIdleEvent),
  wxe_util:call(?wxIdleEvent_MoreRequested,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxidleevent.html#wxidleeventsetmode">external documentation</a>.
%%<br /> Mode = ?wxIDLE_PROCESS_ALL | ?wxIDLE_PROCESS_SPECIFIED
-spec setMode(Mode) -> 'ok' when
	Mode::wx:wx_enum().
setMode(Mode)
 when is_integer(Mode) ->
  wxe_util:cast(?wxIdleEvent_SetMode,
  <<Mode:32/?UI>>).

 %% From wxEvent
%% @hidden
stopPropagation(This) -> wxEvent:stopPropagation(This).
%% @hidden
skip(This, Options) -> wxEvent:skip(This, Options).
%% @hidden
skip(This) -> wxEvent:skip(This).
%% @hidden
shouldPropagate(This) -> wxEvent:shouldPropagate(This).
%% @hidden
resumePropagation(This,PropagationLevel) -> wxEvent:resumePropagation(This,PropagationLevel).
%% @hidden
isCommandEvent(This) -> wxEvent:isCommandEvent(This).
%% @hidden
getTimestamp(This) -> wxEvent:getTimestamp(This).
%% @hidden
getSkipped(This) -> wxEvent:getSkipped(This).
%% @hidden
getId(This) -> wxEvent:getId(This).
