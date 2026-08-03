%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0 AND LicenseRef-scancode-wxwindows-free-doc-3
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
%% For documentation, wxWindow Free Documentation License, Version 3 applies.
%% wxWindows Free Documentation Licence, Version 3, as follows.
%% ===============================================
%%
%% Everyone is permitted to copy and distribute verbatim copies
%% of this licence document, but changing it is not allowed.
%%
%%                  WXWINDOWS FREE DOCUMENTATION LICENCE
%%    TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION
%%
%% 1. Permission is granted to make and distribute verbatim copies of this
%% manual or piece of documentation provided any copyright notice and this
%% permission notice are preserved on all copies.
%%
%% 2. Permission is granted to process this file or document through a
%% document processing system and, at your option and the option of any third
%% party, print the results, provided a printed document carries a copying
%% permission notice identical to this one.
%%
%% 3. Permission is granted to copy and distribute modified versions of this
%% manual or piece of documentation under the conditions for verbatim copying,
%% provided also that any sections describing licensing conditions for this
%% manual, such as, in particular, the GNU General Public Licence, the GNU
%% Library General Public Licence, and any wxWindows Licence are included
%% exactly as in the original, and provided that the entire resulting derived
%% work is distributed under the terms of a permission notice identical to
%% this one.
%%
%% 4. Permission is granted to copy and distribute translations of this manual
%% or piece of documentation into another language, under the above conditions
%% for modified versions, except that sections related to licensing, including
%% this paragraph, may also be included in translations approved by the
%% copyright holders of the respective licence documents in addition to the
%% original English.
%%
%% %CopyrightEnd%
%% This file is generated DO NOT EDIT

-module(wxDropFilesEvent).
-moduledoc """
This class is used for drop files events, that is, when files have been dropped onto the
window.

The window must have previously been enabled for dropping by calling `wxWindow:dragAcceptFiles/2`.

Important note: this is a separate implementation to the more general drag and drop
implementation documented in the overview_dnd. It uses the older, Windows message-based
approach of dropping files.

See:
* [Overview events](https://docs.wxwidgets.org/3.2/overview_events.html#overview_events)

* `wxWindow:dragAcceptFiles/2`

This class is derived, and can use functions, from:

* `m:wxEvent`

wxWidgets docs: [wxDropFilesEvent](https://docs.wxwidgets.org/3.2/classwx_drop_files_event.html)

## Events

Use `wxEvtHandler:connect/3` with `wxDropFilesEventType` to subscribe to events of this type.
""".
-include("wxe.hrl").
-export([getFiles/1,getNumberOfFiles/1,getPosition/1]).

%% inherited exports
-export([getId/1,getSkipped/1,getTimestamp/1,isCommandEvent/1,parent_class/1,
  resumePropagation/2,shouldPropagate/1,skip/1,skip/2,stopPropagation/1]).

-type wxDropFilesEvent() :: wx:wx_object().
-include("wx.hrl").
-type wxDropFilesEventType() :: 'drop_files'.
-export_type([wxDropFilesEvent/0, wxDropFiles/0, wxDropFilesEventType/0]).
-doc false.
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc """
Returns the position at which the files were dropped.

Returns an array of filenames.
""".
-spec getPosition(This) -> {X::integer(), Y::integer()} when
	This::wxDropFilesEvent().
getPosition(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxDropFilesEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxDropFilesEvent_GetPosition),
  wxe_util:rec(?wxDropFilesEvent_GetPosition).

-doc "Returns the number of files dropped.".
-spec getNumberOfFiles(This) -> integer() when
	This::wxDropFilesEvent().
getNumberOfFiles(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxDropFilesEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxDropFilesEvent_GetNumberOfFiles),
  wxe_util:rec(?wxDropFilesEvent_GetNumberOfFiles).

-doc "Returns an array of filenames.".
-spec getFiles(This) -> [unicode:charlist()] when
	This::wxDropFilesEvent().
getFiles(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxDropFilesEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxDropFilesEvent_GetFiles),
  wxe_util:rec(?wxDropFilesEvent_GetFiles).

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
