%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2024. All Rights Reserved.
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

-module(wxGridCellEditor).
-moduledoc """
Functions for wxGridCellEditor class

This class is responsible for providing and manipulating the in-place edit
controls for the grid. Instances of `m:wxGridCellEditor` (actually, instances of
derived classes since it is an abstract class) can be associated with the cell
attributes for individual cells, rows, columns, or even for the entire grid.

Normally `m:wxGridCellEditor` shows some UI control allowing the user to edit
the cell, but starting with wxWidgets 3.1.4 it's also possible to define
"activatable" cell editors, that change the value of the cell directly when it's
activated (typically by pressing Space key or clicking on it), see
`TryActivate()` (not implemented in wx) method. Note that when implementing an
editor which is always activatable, i.e. never shows any in-place editor, it is
more convenient to derive its class from `wxGridCellActivatableEditor` (not
implemented in wx) than from `m:wxGridCellEditor` itself.

See: `wxGridCellAutoWrapStringEditor` (not implemented in wx),
`m:wxGridCellBoolEditor`, `m:wxGridCellChoiceEditor`, `wxGridCellEnumEditor`
(not implemented in wx), `m:wxGridCellFloatEditor`, `m:wxGridCellNumberEditor`,
`m:wxGridCellTextEditor`, `wxGridCellDateEditor` (not implemented in wx)

wxWidgets docs:
[wxGridCellEditor](https://docs.wxwidgets.org/3.1/classwx_grid_cell_editor.html)
""".
-include("wxe.hrl").
-export([create/4,handleReturn/2,isCreated/1,reset/1,setSize/2,show/2,show/3,
  startingClick/1,startingKey/2]).

%% inherited exports
-export([parent_class/1]).

-type wxGridCellEditor() :: wx:wx_object().
-export_type([wxGridCellEditor/0]).
%% @hidden
-doc false.
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcelleditor.html#wxgridcelleditorcreate">external documentation</a>.
-doc "Creates the actual edit control.".
-spec create(This, Parent, Id, EvtHandler) -> 'ok' when
	This::wxGridCellEditor(), Parent::wxWindow:wxWindow(), Id::integer(), EvtHandler::wxEvtHandler:wxEvtHandler().
create(#wx_ref{type=ThisT}=This,#wx_ref{type=ParentT}=Parent,Id,#wx_ref{type=EvtHandlerT}=EvtHandler)
 when is_integer(Id) ->
  ?CLASS(ThisT,wxGridCellEditor),
  ?CLASS(ParentT,wxWindow),
  ?CLASS(EvtHandlerT,wxEvtHandler),
  wxe_util:queue_cmd(This,Parent,Id,EvtHandler,?get_env(),?wxGridCellEditor_Create).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcelleditor.html#wxgridcelleditoriscreated">external documentation</a>.
-doc "Returns true if the edit control has been created.".
-spec isCreated(This) -> boolean() when
	This::wxGridCellEditor().
isCreated(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGridCellEditor),
  wxe_util:queue_cmd(This,?get_env(),?wxGridCellEditor_IsCreated),
  wxe_util:rec(?wxGridCellEditor_IsCreated).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcelleditor.html#wxgridcelleditorsetsize">external documentation</a>.
-doc "Size and position the edit control.".
-spec setSize(This, Rect) -> 'ok' when
	This::wxGridCellEditor(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()}.
setSize(#wx_ref{type=ThisT}=This,{RectX,RectY,RectW,RectH} = Rect)
 when is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH) ->
  ?CLASS(ThisT,wxGridCellEditor),
  wxe_util:queue_cmd(This,Rect,?get_env(),?wxGridCellEditor_SetSize).

%% @equiv show(This,Show, [])
-spec show(This, Show) -> 'ok' when
	This::wxGridCellEditor(), Show::boolean().

show(This,Show)
 when is_record(This, wx_ref),is_boolean(Show) ->
  show(This,Show, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcelleditor.html#wxgridcelleditorshow">external documentation</a>.
-doc """
Show or hide the edit control, use the specified attributes to set colours/fonts
for it.
""".
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcelleditor.html#wxgridcelleditorreset">external documentation</a>.
-doc "Reset the value in the control back to its starting value.".
-spec reset(This) -> 'ok' when
	This::wxGridCellEditor().
reset(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGridCellEditor),
  wxe_util:queue_cmd(This,?get_env(),?wxGridCellEditor_Reset).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcelleditor.html#wxgridcelleditorstartingkey">external documentation</a>.
-doc """
If the editor is enabled by pressing keys on the grid, this will be called to
let the editor do something about that first key if desired.
""".
-spec startingKey(This, Event) -> 'ok' when
	This::wxGridCellEditor(), Event::wxKeyEvent:wxKeyEvent().
startingKey(#wx_ref{type=ThisT}=This,#wx_ref{type=EventT}=Event) ->
  ?CLASS(ThisT,wxGridCellEditor),
  ?CLASS(EventT,wxKeyEvent),
  wxe_util:queue_cmd(This,Event,?get_env(),?wxGridCellEditor_StartingKey).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcelleditor.html#wxgridcelleditorstartingclick">external documentation</a>.
-doc "If the editor is enabled by clicking on the cell, this method will be called.".
-spec startingClick(This) -> 'ok' when
	This::wxGridCellEditor().
startingClick(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGridCellEditor),
  wxe_util:queue_cmd(This,?get_env(),?wxGridCellEditor_StartingClick).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcelleditor.html#wxgridcelleditorhandlereturn">external documentation</a>.
-doc "Some types of controls on some platforms may need some help with the Return key.".
-spec handleReturn(This, Event) -> 'ok' when
	This::wxGridCellEditor(), Event::wxKeyEvent:wxKeyEvent().
handleReturn(#wx_ref{type=ThisT}=This,#wx_ref{type=EventT}=Event) ->
  ?CLASS(ThisT,wxGridCellEditor),
  ?CLASS(EventT,wxKeyEvent),
  wxe_util:queue_cmd(This,Event,?get_env(),?wxGridCellEditor_HandleReturn).

