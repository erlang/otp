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

-module(wxShowEvent).
-include("wxe.hrl").
-export([isShown/1,setShow/2]).

%% inherited exports
-export([getId/1,getSkipped/1,getTimestamp/1,isCommandEvent/1,parent_class/1,
  resumePropagation/2,shouldPropagate/1,skip/1,skip/2,stopPropagation/1]).

-type wxShowEvent() :: wx:wx_object().
-include("wx.hrl").
-type wxShowEventType() :: 'show'.
-export_type([wxShowEvent/0, wxShow/0, wxShowEventType/0]).
%% @hidden
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxshowevent.html#wxshoweventsetshow">external documentation</a>.
-spec setShow(This, Show) -> 'ok' when
	This::wxShowEvent(), Show::boolean().
setShow(#wx_ref{type=ThisT}=This,Show)
 when is_boolean(Show) ->
  ?CLASS(ThisT,wxShowEvent),
  wxe_util:queue_cmd(This,Show,?get_env(),?wxShowEvent_SetShow).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxshowevent.html#wxshoweventisshown">external documentation</a>.
-spec isShown(This) -> boolean() when
	This::wxShowEvent().
isShown(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxShowEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxShowEvent_IsShown),
  wxe_util:rec(?wxShowEvent_IsShown).

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
