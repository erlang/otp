%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2020. All Rights Reserved.
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

-module(wxAuiNotebookEvent).
-include("wxe.hrl").
-export([getDragSource/1,getOldSelection/1,getSelection/1,setDragSource/2,
  setOldSelection/2,setSelection/2]).

%% inherited exports
-export([allow/1,getClientData/1,getExtraLong/1,getId/1,getInt/1,getSkipped/1,
  getString/1,getTimestamp/1,isAllowed/1,isChecked/1,isCommandEvent/1,
  isSelection/1,parent_class/1,resumePropagation/2,setInt/2,setString/2,
  shouldPropagate/1,skip/1,skip/2,stopPropagation/1,veto/1]).

-type wxAuiNotebookEvent() :: wx:wx_object().
-include("wx.hrl").
-type wxAuiNotebookEventType() :: 'command_auinotebook_page_close' | 'command_auinotebook_page_changed' | 'command_auinotebook_page_changing' | 'command_auinotebook_button' | 'command_auinotebook_begin_drag' | 'command_auinotebook_end_drag' | 'command_auinotebook_drag_motion' | 'command_auinotebook_allow_dnd' | 'command_auinotebook_tab_middle_down' | 'command_auinotebook_tab_middle_up' | 'command_auinotebook_tab_right_down' | 'command_auinotebook_tab_right_up' | 'command_auinotebook_page_closed' | 'command_auinotebook_drag_done' | 'command_auinotebook_bg_dclick'.
-export_type([wxAuiNotebookEvent/0, wxAuiNotebook/0, wxAuiNotebookEventType/0]).
%% @hidden
parent_class(wxBookCtrlEvent) -> true;
parent_class(wxNotifyEvent) -> true;
parent_class(wxCommandEvent) -> true;
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauinotebookevent.html#wxauinotebookeventsetselection">external documentation</a>.
-spec setSelection(This, Page) -> 'ok' when
	This::wxAuiNotebookEvent(), Page::integer().
setSelection(#wx_ref{type=ThisT}=This,Page)
 when is_integer(Page) ->
  ?CLASS(ThisT,wxAuiNotebookEvent),
  wxe_util:queue_cmd(This,Page,?get_env(),?wxAuiNotebookEvent_SetSelection).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauinotebookevent.html#wxauinotebookeventgetselection">external documentation</a>.
-spec getSelection(This) -> integer() when
	This::wxAuiNotebookEvent().
getSelection(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiNotebookEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiNotebookEvent_GetSelection),
  wxe_util:rec(?wxAuiNotebookEvent_GetSelection).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauinotebookevent.html#wxauinotebookeventsetoldselection">external documentation</a>.
-spec setOldSelection(This, Page) -> 'ok' when
	This::wxAuiNotebookEvent(), Page::integer().
setOldSelection(#wx_ref{type=ThisT}=This,Page)
 when is_integer(Page) ->
  ?CLASS(ThisT,wxAuiNotebookEvent),
  wxe_util:queue_cmd(This,Page,?get_env(),?wxAuiNotebookEvent_SetOldSelection).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauinotebookevent.html#wxauinotebookeventgetoldselection">external documentation</a>.
-spec getOldSelection(This) -> integer() when
	This::wxAuiNotebookEvent().
getOldSelection(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiNotebookEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiNotebookEvent_GetOldSelection),
  wxe_util:rec(?wxAuiNotebookEvent_GetOldSelection).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauinotebookevent.html#wxauinotebookeventsetdragsource">external documentation</a>.
-spec setDragSource(This, S) -> 'ok' when
	This::wxAuiNotebookEvent(), S::wxAuiNotebook:wxAuiNotebook().
setDragSource(#wx_ref{type=ThisT}=This,#wx_ref{type=ST}=S) ->
  ?CLASS(ThisT,wxAuiNotebookEvent),
  ?CLASS(ST,wxAuiNotebook),
  wxe_util:queue_cmd(This,S,?get_env(),?wxAuiNotebookEvent_SetDragSource).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauinotebookevent.html#wxauinotebookeventgetdragsource">external documentation</a>.
-spec getDragSource(This) -> wxAuiNotebook:wxAuiNotebook() when
	This::wxAuiNotebookEvent().
getDragSource(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiNotebookEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiNotebookEvent_GetDragSource),
  wxe_util:rec(?wxAuiNotebookEvent_GetDragSource).

 %% From wxBookCtrlEvent
 %% From wxNotifyEvent
%% @hidden
veto(This) -> wxNotifyEvent:veto(This).
%% @hidden
isAllowed(This) -> wxNotifyEvent:isAllowed(This).
%% @hidden
allow(This) -> wxNotifyEvent:allow(This).
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
