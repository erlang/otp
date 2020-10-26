%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2020. All Rights Reserved.
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

-module(wxIdleEvent).
-include("wxe.hrl").
-export([getMode/0,moreRequested/1,requestMore/1,requestMore/2,setMode/1]).

%% inherited exports
-export([getId/1,getSkipped/1,getTimestamp/1,isCommandEvent/1,parent_class/1,
  resumePropagation/2,shouldPropagate/1,skip/1,skip/2,stopPropagation/1]).

-type wxIdleEvent() :: wx:wx_object().
-include("wx.hrl").
-type wxIdleEventType() :: 'idle'.
-export_type([wxIdleEvent/0, wxIdle/0, wxIdleEventType/0]).
%% @hidden
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxidleevent.html#wxidleeventgetmode">external documentation</a>.
%%<br /> Res = ?wxIDLE_PROCESS_ALL | ?wxIDLE_PROCESS_SPECIFIED
-spec getMode() -> wx:wx_enum().
getMode() ->
  wxe_util:queue_cmd(?get_env(), ?wxIdleEvent_GetMode),
  wxe_util:rec(?wxIdleEvent_GetMode).

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
requestMore(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxIdleEvent),
  MOpts = fun({needMore, _needMore} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxIdleEvent_RequestMore).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxidleevent.html#wxidleeventmorerequested">external documentation</a>.
-spec moreRequested(This) -> boolean() when
	This::wxIdleEvent().
moreRequested(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxIdleEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxIdleEvent_MoreRequested),
  wxe_util:rec(?wxIdleEvent_MoreRequested).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxidleevent.html#wxidleeventsetmode">external documentation</a>.
%%<br /> Mode = ?wxIDLE_PROCESS_ALL | ?wxIDLE_PROCESS_SPECIFIED
-spec setMode(Mode) -> 'ok' when
	Mode::wx:wx_enum().
setMode(Mode)
 when is_integer(Mode) ->
  wxe_util:queue_cmd(Mode,?get_env(),?wxIdleEvent_SetMode).

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
