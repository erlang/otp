%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2016. All Rights Reserved.
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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauinotebookevent.html">wxAuiNotebookEvent</a>.
%% <dl><dt>Use {@link wxEvtHandler:connect/3.} with EventType:</dt>
%% <dd><em>command_auinotebook_page_close</em>, <em>command_auinotebook_page_changed</em>, <em>command_auinotebook_page_changing</em>, <em>command_auinotebook_button</em>, <em>command_auinotebook_begin_drag</em>, <em>command_auinotebook_end_drag</em>, <em>command_auinotebook_drag_motion</em>, <em>command_auinotebook_allow_dnd</em>, <em>command_auinotebook_tab_middle_down</em>, <em>command_auinotebook_tab_middle_up</em>, <em>command_auinotebook_tab_right_down</em>, <em>command_auinotebook_tab_right_up</em>, <em>command_auinotebook_page_closed</em>, <em>command_auinotebook_drag_done</em>, <em>command_auinotebook_bg_dclick</em></dd></dl>
%% See also the message variant {@link wxEvtHandler:wxAuiNotebook(). #wxAuiNotebook{}} event record type.
%%
%% <p>This class is derived (and can use functions) from:
%% <br />{@link wxNotifyEvent}
%% <br />{@link wxCommandEvent}
%% <br />{@link wxEvent}
%% </p>
%% @type wxAuiNotebookEvent().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxAuiNotebookEvent).
-include("wxe.hrl").
-export([getDragSource/1,getOldSelection/1,getSelection/1,setDragSource/2,
  setOldSelection/2,setSelection/2]).

%% inherited exports
-export([allow/1,getClientData/1,getExtraLong/1,getId/1,getInt/1,getSkipped/1,
  getString/1,getTimestamp/1,isAllowed/1,isChecked/1,isCommandEvent/1,
  isSelection/1,parent_class/1,resumePropagation/2,setInt/2,setString/2,
  shouldPropagate/1,skip/1,skip/2,stopPropagation/1,veto/1]).

-export_type([wxAuiNotebookEvent/0]).
%% @hidden
parent_class(wxNotifyEvent) -> true;
parent_class(wxCommandEvent) -> true;
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxAuiNotebookEvent() :: wx:wx_object().
%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauinotebookevent.html#wxauinotebookeventsetselection">external documentation</a>.
-spec setSelection(This, S) -> 'ok' when
	This::wxAuiNotebookEvent(), S::integer().
setSelection(#wx_ref{type=ThisT,ref=ThisRef},S)
 when is_integer(S) ->
  ?CLASS(ThisT,wxAuiNotebookEvent),
  wxe_util:cast(?wxAuiNotebookEvent_SetSelection,
  <<ThisRef:32/?UI,S:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauinotebookevent.html#wxauinotebookeventgetselection">external documentation</a>.
-spec getSelection(This) -> integer() when
	This::wxAuiNotebookEvent().
getSelection(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiNotebookEvent),
  wxe_util:call(?wxAuiNotebookEvent_GetSelection,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauinotebookevent.html#wxauinotebookeventsetoldselection">external documentation</a>.
-spec setOldSelection(This, S) -> 'ok' when
	This::wxAuiNotebookEvent(), S::integer().
setOldSelection(#wx_ref{type=ThisT,ref=ThisRef},S)
 when is_integer(S) ->
  ?CLASS(ThisT,wxAuiNotebookEvent),
  wxe_util:cast(?wxAuiNotebookEvent_SetOldSelection,
  <<ThisRef:32/?UI,S:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauinotebookevent.html#wxauinotebookeventgetoldselection">external documentation</a>.
-spec getOldSelection(This) -> integer() when
	This::wxAuiNotebookEvent().
getOldSelection(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiNotebookEvent),
  wxe_util:call(?wxAuiNotebookEvent_GetOldSelection,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauinotebookevent.html#wxauinotebookeventsetdragsource">external documentation</a>.
-spec setDragSource(This, S) -> 'ok' when
	This::wxAuiNotebookEvent(), S::wxAuiNotebook:wxAuiNotebook().
setDragSource(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=ST,ref=SRef}) ->
  ?CLASS(ThisT,wxAuiNotebookEvent),
  ?CLASS(ST,wxAuiNotebook),
  wxe_util:cast(?wxAuiNotebookEvent_SetDragSource,
  <<ThisRef:32/?UI,SRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauinotebookevent.html#wxauinotebookeventgetdragsource">external documentation</a>.
-spec getDragSource(This) -> wxAuiNotebook:wxAuiNotebook() when
	This::wxAuiNotebookEvent().
getDragSource(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiNotebookEvent),
  wxe_util:call(?wxAuiNotebookEvent_GetDragSource,
  <<ThisRef:32/?UI>>).

 %% From wxNotifyEvent
%% @hidden
veto(This) -> wxNotifyEvent:veto(This).
%% @hidden
isAllowed(This) -> wxNotifyEvent:isAllowed(This).
%% @hidden
allow(This) -> wxNotifyEvent:allow(This).
 %% From wxCommandEvent
%% @hidden
setString(This,S) -> wxCommandEvent:setString(This,S).
%% @hidden
setInt(This,I) -> wxCommandEvent:setInt(This,I).
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
