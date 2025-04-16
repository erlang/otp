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
%% This file is generated DO NOT EDIT

-module(wxAuiManagerEvent).
-moduledoc """
Event used to indicate various actions taken with `m:wxAuiManager`.

See `m:wxAuiManager` for available event types.

See:
* `m:wxAuiManager`

* `m:wxAuiPaneInfo`

This class is derived, and can use functions, from:

* `m:wxEvent`

wxWidgets docs: [wxAuiManagerEvent](https://docs.wxwidgets.org/3.2/classwx_aui_manager_event.html)

## Events

Use `wxEvtHandler:connect/3` with `wxAuiManagerEventType` to subscribe to events of this type.
""".
-include("wxe.hrl").
-export([canVeto/1,getButton/1,getDC/1,getManager/1,getPane/1,getVeto/1,setButton/2,
  setCanVeto/2,setDC/2,setManager/2,setPane/2,veto/1,veto/2]).

%% inherited exports
-export([getId/1,getSkipped/1,getTimestamp/1,isCommandEvent/1,parent_class/1,
  resumePropagation/2,shouldPropagate/1,skip/1,skip/2,stopPropagation/1]).

-type wxAuiManagerEvent() :: wx:wx_object().
-include("wx.hrl").
-type wxAuiManagerEventType() :: 'aui_pane_button' | 'aui_pane_close' | 'aui_pane_maximize' | 'aui_pane_restore' | 'aui_pane_activated' | 'aui_render' | 'aui_find_manager'.
-export_type([wxAuiManagerEvent/0, wxAuiManager/0, wxAuiManagerEventType/0]).
-doc false.
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc "Sets the `m:wxAuiManager` this event is associated with.".
-spec setManager(This, Manager) -> 'ok' when
	This::wxAuiManagerEvent(), Manager::wxAuiManager:wxAuiManager().
setManager(#wx_ref{type=ThisT}=This,#wx_ref{type=ManagerT}=Manager) ->
  ?CLASS(ThisT,wxAuiManagerEvent),
  ?CLASS(ManagerT,wxAuiManager),
  wxe_util:queue_cmd(This,Manager,?get_env(),?wxAuiManagerEvent_SetManager).

-doc "Return: The `m:wxAuiManager` this event is associated with.".
-spec getManager(This) -> wxAuiManager:wxAuiManager() when
	This::wxAuiManagerEvent().
getManager(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiManagerEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiManagerEvent_GetManager),
  wxe_util:rec(?wxAuiManagerEvent_GetManager).

-doc "Sets the pane this event is associated with.".
-spec setPane(This, Pane) -> 'ok' when
	This::wxAuiManagerEvent(), Pane::wxAuiPaneInfo:wxAuiPaneInfo().
setPane(#wx_ref{type=ThisT}=This,#wx_ref{type=PaneT}=Pane) ->
  ?CLASS(ThisT,wxAuiManagerEvent),
  ?CLASS(PaneT,wxAuiPaneInfo),
  wxe_util:queue_cmd(This,Pane,?get_env(),?wxAuiManagerEvent_SetPane).

-doc "Return: The pane this event is associated with.".
-spec getPane(This) -> wxAuiPaneInfo:wxAuiPaneInfo() when
	This::wxAuiManagerEvent().
getPane(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiManagerEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiManagerEvent_GetPane),
  wxe_util:rec(?wxAuiManagerEvent_GetPane).

-doc "Sets the ID of the button clicked that triggered this event.".
-spec setButton(This, Button) -> 'ok' when
	This::wxAuiManagerEvent(), Button::integer().
setButton(#wx_ref{type=ThisT}=This,Button)
 when is_integer(Button) ->
  ?CLASS(ThisT,wxAuiManagerEvent),
  wxe_util:queue_cmd(This,Button,?get_env(),?wxAuiManagerEvent_SetButton).

-doc "Return: The ID of the button that was clicked.".
-spec getButton(This) -> integer() when
	This::wxAuiManagerEvent().
getButton(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiManagerEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiManagerEvent_GetButton),
  wxe_util:rec(?wxAuiManagerEvent_GetButton).

-doc "".
-spec setDC(This, Pdc) -> 'ok' when
	This::wxAuiManagerEvent(), Pdc::wxDC:wxDC().
setDC(#wx_ref{type=ThisT}=This,#wx_ref{type=PdcT}=Pdc) ->
  ?CLASS(ThisT,wxAuiManagerEvent),
  ?CLASS(PdcT,wxDC),
  wxe_util:queue_cmd(This,Pdc,?get_env(),?wxAuiManagerEvent_SetDC).

-doc "".
-spec getDC(This) -> wxDC:wxDC() when
	This::wxAuiManagerEvent().
getDC(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiManagerEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiManagerEvent_GetDC),
  wxe_util:rec(?wxAuiManagerEvent_GetDC).

-doc(#{equiv => veto(This, [])}).
-spec veto(This) -> 'ok' when
	This::wxAuiManagerEvent().

veto(This)
 when is_record(This, wx_ref) ->
  veto(This, []).

-doc "Cancels the action indicated by this event if `canVeto/1` is true.".
-spec veto(This, [Option]) -> 'ok' when
	This::wxAuiManagerEvent(),
	Option :: {'veto', boolean()}.
veto(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxAuiManagerEvent),
  MOpts = fun({veto, _veto} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxAuiManagerEvent_Veto).

-doc """
Return: true if this event was vetoed.

See: `veto/2`
""".
-spec getVeto(This) -> boolean() when
	This::wxAuiManagerEvent().
getVeto(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiManagerEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiManagerEvent_GetVeto),
  wxe_util:rec(?wxAuiManagerEvent_GetVeto).

-doc "Sets whether or not this event can be vetoed.".
-spec setCanVeto(This, Can_veto) -> 'ok' when
	This::wxAuiManagerEvent(), Can_veto::boolean().
setCanVeto(#wx_ref{type=ThisT}=This,Can_veto)
 when is_boolean(Can_veto) ->
  ?CLASS(ThisT,wxAuiManagerEvent),
  wxe_util:queue_cmd(This,Can_veto,?get_env(),?wxAuiManagerEvent_SetCanVeto).

-doc """
Return: true if this event can be vetoed.

See: `veto/2`
""".
-spec canVeto(This) -> boolean() when
	This::wxAuiManagerEvent().
canVeto(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiManagerEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiManagerEvent_CanVeto),
  wxe_util:rec(?wxAuiManagerEvent_CanVeto).

 %% From wxEvent
-doc false.
stopPropagation(This) -> wxEvent:stopPropagation(This).
-doc false.
skip(This, Options) -> wxEvent:skip(This, Options).
-doc false.
skip(This) -> wxEvent:skip(This).
-doc false.
shouldPropagate(This) -> wxEvent:shouldPropagate(This).
-doc false.
resumePropagation(This,PropagationLevel) -> wxEvent:resumePropagation(This,PropagationLevel).
-doc false.
isCommandEvent(This) -> wxEvent:isCommandEvent(This).
-doc false.
getTimestamp(This) -> wxEvent:getTimestamp(This).
-doc false.
getSkipped(This) -> wxEvent:getSkipped(This).
-doc false.
getId(This) -> wxEvent:getId(This).
