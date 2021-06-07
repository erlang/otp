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

-module(wxSashEvent).
-include("wxe.hrl").
-export([getDragRect/1,getDragStatus/1,getEdge/1]).

%% inherited exports
-export([getClientData/1,getExtraLong/1,getId/1,getInt/1,getSelection/1,getSkipped/1,
  getString/1,getTimestamp/1,isChecked/1,isCommandEvent/1,isSelection/1,
  parent_class/1,resumePropagation/2,setInt/2,setString/2,shouldPropagate/1,
  skip/1,skip/2,stopPropagation/1]).

-type wxSashEvent() :: wx:wx_object().
-include("wx.hrl").
-type wxSashEventType() :: 'sash_dragged'.
-export_type([wxSashEvent/0, wxSash/0, wxSashEventType/0]).
%% @hidden
parent_class(wxCommandEvent) -> true;
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsashevent.html#wxsasheventgetedge">external documentation</a>.
%%<br /> Res = ?wxSASH_TOP | ?wxSASH_RIGHT | ?wxSASH_BOTTOM | ?wxSASH_LEFT | ?wxSASH_NONE
-spec getEdge(This) -> wx:wx_enum() when
	This::wxSashEvent().
getEdge(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxSashEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxSashEvent_GetEdge),
  wxe_util:rec(?wxSashEvent_GetEdge).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsashevent.html#wxsasheventgetdragrect">external documentation</a>.
-spec getDragRect(This) -> {X::integer(), Y::integer(), W::integer(), H::integer()} when
	This::wxSashEvent().
getDragRect(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxSashEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxSashEvent_GetDragRect),
  wxe_util:rec(?wxSashEvent_GetDragRect).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsashevent.html#wxsasheventgetdragstatus">external documentation</a>.
%%<br /> Res = ?wxSASH_STATUS_OK | ?wxSASH_STATUS_OUT_OF_RANGE
-spec getDragStatus(This) -> wx:wx_enum() when
	This::wxSashEvent().
getDragStatus(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxSashEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxSashEvent_GetDragStatus),
  wxe_util:rec(?wxSashEvent_GetDragStatus).

 %% From wxCommandEvent
%% @hidden
setString(This,String) -> wxCommandEvent:setString(This,String).
%% @hidden
setInt(This,IntCommand) -> wxCommandEvent:setInt(This,IntCommand).
%% @hidden
isSelection(This) -> wxCommandEvent:isSelection(This).
%% @hidden
isChecked(This) -> wxCommandEvent:isChecked(This).
%% @hidden
getString(This) -> wxCommandEvent:getString(This).
%% @hidden
getSelection(This) -> wxCommandEvent:getSelection(This).
%% @hidden
getInt(This) -> wxCommandEvent:getInt(This).
%% @hidden
getExtraLong(This) -> wxCommandEvent:getExtraLong(This).
%% @hidden
getClientData(This) -> wxCommandEvent:getClientData(This).
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
