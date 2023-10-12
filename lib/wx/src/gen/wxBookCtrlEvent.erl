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

-module(wxBookCtrlEvent).
-include("wxe.hrl").
-export([getOldSelection/1,getSelection/1,setOldSelection/2,setSelection/2]).

%% inherited exports
-export([allow/1,getClientData/1,getExtraLong/1,getId/1,getInt/1,getSkipped/1,
  getString/1,getTimestamp/1,isAllowed/1,isChecked/1,isCommandEvent/1,
  isSelection/1,parent_class/1,resumePropagation/2,setInt/2,setString/2,
  shouldPropagate/1,skip/1,skip/2,stopPropagation/1,veto/1]).

-type wxBookCtrlEvent() :: wx:wx_object().
-include("wx.hrl").
-type wxBookCtrlEventType() :: 'command_notebook_page_changed' | 'command_notebook_page_changing' | 'choicebook_page_changed' | 'choicebook_page_changing' | 'treebook_page_changed' | 'treebook_page_changing' | 'toolbook_page_changed' | 'toolbook_page_changing' | 'listbook_page_changed' | 'listbook_page_changing'.
-export_type([wxBookCtrlEvent/0, wxBookCtrl/0, wxBookCtrlEventType/0]).
%% @hidden
parent_class(wxNotifyEvent) -> true;
parent_class(wxCommandEvent) -> true;
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbookctrlevent.html#wxbookctrleventgetoldselection">external documentation</a>.
-spec getOldSelection(This) -> integer() when
	This::wxBookCtrlEvent().
getOldSelection(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxBookCtrlEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxBookCtrlEvent_GetOldSelection),
  wxe_util:rec(?wxBookCtrlEvent_GetOldSelection).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbookctrlevent.html#wxbookctrleventgetselection">external documentation</a>.
-spec getSelection(This) -> integer() when
	This::wxBookCtrlEvent().
getSelection(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxBookCtrlEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxBookCtrlEvent_GetSelection),
  wxe_util:rec(?wxBookCtrlEvent_GetSelection).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbookctrlevent.html#wxbookctrleventsetoldselection">external documentation</a>.
-spec setOldSelection(This, Page) -> 'ok' when
	This::wxBookCtrlEvent(), Page::integer().
setOldSelection(#wx_ref{type=ThisT}=This,Page)
 when is_integer(Page) ->
  ?CLASS(ThisT,wxBookCtrlEvent),
  wxe_util:queue_cmd(This,Page,?get_env(),?wxBookCtrlEvent_SetOldSelection).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbookctrlevent.html#wxbookctrleventsetselection">external documentation</a>.
-spec setSelection(This, Page) -> 'ok' when
	This::wxBookCtrlEvent(), Page::integer().
setSelection(#wx_ref{type=ThisT}=This,Page)
 when is_integer(Page) ->
  ?CLASS(ThisT,wxBookCtrlEvent),
  wxe_util:queue_cmd(This,Page,?get_env(),?wxBookCtrlEvent_SetSelection).

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
