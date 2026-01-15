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

-module(wxGridCellEditor).
-moduledoc """
This class is responsible for providing and manipulating the in-place edit controls for
the grid.

Instances of `m:wxGridCellEditor` (actually, instances of derived classes since it is an
abstract class) can be associated with the cell attributes for individual cells, rows,
columns, or even for the entire grid.

Normally `m:wxGridCellEditor` shows some UI control allowing the user to edit the cell,
but starting with wxWidgets 3.1.4 it's also possible to define "activatable" cell editors,
that change the value of the cell directly when it's activated (typically by pressing
Space key or clicking on it), see `TryActivate()` (not implemented in wx) method. Note
that when implementing an editor which is always activatable, i.e. never shows any
in-place editor, it is more convenient to derive its class from `wxGridCellActivatableEditor`
(not implemented in wx) than from `m:wxGridCellEditor` itself.

See:
* `m:wxGridCellBoolEditor`

* `m:wxGridCellChoiceEditor`

* `m:wxGridCellFloatEditor`

* `m:wxGridCellNumberEditor`

* `m:wxGridCellTextEditor`

wxWidgets docs: [wxGridCellEditor](https://docs.wxwidgets.org/3.2/classwx_grid_cell_editor.html)
""".
-include("wxe.hrl").
-export([create/4,handleReturn/2,isCreated/1,reset/1,setSize/2,show/2,show/3,
  startingClick/1,startingKey/2]).

%% inherited exports
-export([parent_class/1]).

-type wxGridCellEditor() :: wx:wx_object().
-export_type([wxGridCellEditor/0]).
-doc false.
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc "Creates the actual edit control.".
-spec create(This, Parent, Id, EvtHandler) -> 'ok' when
	This::wxGridCellEditor(), Parent::wxWindow:wxWindow(), Id::integer(), EvtHandler::wxEvtHandler:wxEvtHandler().
create(#wx_ref{type=ThisT}=This,#wx_ref{type=ParentT}=Parent,Id,#wx_ref{type=EvtHandlerT}=EvtHandler)
 when is_integer(Id) ->
  ?CLASS(ThisT,wxGridCellEditor),
  ?CLASS(ParentT,wxWindow),
  ?CLASS(EvtHandlerT,wxEvtHandler),
  wxe_util:queue_cmd(This,Parent,Id,EvtHandler,?get_env(),?wxGridCellEditor_Create).

-doc "Returns true if the edit control has been created.".
-spec isCreated(This) -> boolean() when
	This::wxGridCellEditor().
isCreated(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGridCellEditor),
  wxe_util:queue_cmd(This,?get_env(),?wxGridCellEditor_IsCreated),
  wxe_util:rec(?wxGridCellEditor_IsCreated).

-doc "Size and position the edit control.".
-spec setSize(This, Rect) -> 'ok' when
	This::wxGridCellEditor(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()}.
setSize(#wx_ref{type=ThisT}=This,{RectX,RectY,RectW,RectH} = Rect)
 when is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH) ->
  ?CLASS(ThisT,wxGridCellEditor),
  wxe_util:queue_cmd(This,Rect,?get_env(),?wxGridCellEditor_SetSize).

-doc(#{equiv => show(This,Show, [])}).
-spec show(This, Show) -> 'ok' when
	This::wxGridCellEditor(), Show::boolean().

show(This,Show)
 when is_record(This, wx_ref),is_boolean(Show) ->
  show(This,Show, []).

-doc "Show or hide the edit control, use the specified attributes to set colours/fonts for it.".
-spec show(This, Show, [Option]) -> 'ok' when
	This::wxGridCellEditor(), Show::boolean(),
	Option :: {'attr', wxGridCellAttr:wxGridCellAttr()}.
show(#wx_ref{type=ThisT}=This,Show, Options)
 when is_boolean(Show),is_list(Options) ->
  ?CLASS(ThisT,wxGridCellEditor),
  MOpts = fun({attr, #wx_ref{type=AttrT}} = Arg) ->   ?CLASS(AttrT,wxGridCellAttr),Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Show, Opts,?get_env(),?wxGridCellEditor_Show).

-doc "Reset the value in the control back to its starting value.".
-spec reset(This) -> 'ok' when
	This::wxGridCellEditor().
reset(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGridCellEditor),
  wxe_util:queue_cmd(This,?get_env(),?wxGridCellEditor_Reset).

-doc """
If the editor is enabled by pressing keys on the grid, this will be called to let the
editor do something about that first key if desired.
""".
-spec startingKey(This, Event) -> 'ok' when
	This::wxGridCellEditor(), Event::wxKeyEvent:wxKeyEvent().
startingKey(#wx_ref{type=ThisT}=This,#wx_ref{type=EventT}=Event) ->
  ?CLASS(ThisT,wxGridCellEditor),
  ?CLASS(EventT,wxKeyEvent),
  wxe_util:queue_cmd(This,Event,?get_env(),?wxGridCellEditor_StartingKey).

-doc "If the editor is enabled by clicking on the cell, this method will be called.".
-spec startingClick(This) -> 'ok' when
	This::wxGridCellEditor().
startingClick(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGridCellEditor),
  wxe_util:queue_cmd(This,?get_env(),?wxGridCellEditor_StartingClick).

-doc "Some types of controls on some platforms may need some help with the Return key.".
-spec handleReturn(This, Event) -> 'ok' when
	This::wxGridCellEditor(), Event::wxKeyEvent:wxKeyEvent().
handleReturn(#wx_ref{type=ThisT}=This,#wx_ref{type=EventT}=Event) ->
  ?CLASS(ThisT,wxGridCellEditor),
  ?CLASS(EventT,wxKeyEvent),
  wxe_util:queue_cmd(This,Event,?get_env(),?wxGridCellEditor_HandleReturn).

