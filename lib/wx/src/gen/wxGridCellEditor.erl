%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcelleditor.html">wxGridCellEditor</a>.
%% @type wxGridCellEditor().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxGridCellEditor).
-include("wxe.hrl").
-export([beginEdit/4,create/4,endEdit/4,handleReturn/2,isCreated/1,paintBackground/3,
  reset/1,setSize/2,show/2,show/3,startingClick/1,startingKey/2]).

%% inherited exports
-export([parent_class/1]).

-export_type([wxGridCellEditor/0]).
-deprecated([endEdit/4,paintBackground/3]).

%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxGridCellEditor() :: wx:wx_object().
%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcelleditor.html#wxgridcelleditorcreate">external documentation</a>.
-spec create(This, Parent, Id, EvtHandler) -> 'ok' when
	This::wxGridCellEditor(), Parent::wxWindow:wxWindow(), Id::integer(), EvtHandler::wxEvtHandler:wxEvtHandler().
create(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=ParentT,ref=ParentRef},Id,#wx_ref{type=EvtHandlerT,ref=EvtHandlerRef})
 when is_integer(Id) ->
  ?CLASS(ThisT,wxGridCellEditor),
  ?CLASS(ParentT,wxWindow),
  ?CLASS(EvtHandlerT,wxEvtHandler),
  wxe_util:cast(?wxGridCellEditor_Create,
  <<ThisRef:32/?UI,ParentRef:32/?UI,Id:32/?UI,EvtHandlerRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcelleditor.html#wxgridcelleditoriscreated">external documentation</a>.
-spec isCreated(This) -> boolean() when
	This::wxGridCellEditor().
isCreated(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGridCellEditor),
  wxe_util:call(?wxGridCellEditor_IsCreated,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcelleditor.html#wxgridcelleditorsetsize">external documentation</a>.
-spec setSize(This, Rect) -> 'ok' when
	This::wxGridCellEditor(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()}.
setSize(#wx_ref{type=ThisT,ref=ThisRef},{RectX,RectY,RectW,RectH})
 when is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH) ->
  ?CLASS(ThisT,wxGridCellEditor),
  wxe_util:cast(?wxGridCellEditor_SetSize,
  <<ThisRef:32/?UI,RectX:32/?UI,RectY:32/?UI,RectW:32/?UI,RectH:32/?UI>>).

%% @equiv show(This,Show, [])
-spec show(This, Show) -> 'ok' when
	This::wxGridCellEditor(), Show::boolean().

show(This,Show)
 when is_record(This, wx_ref),is_boolean(Show) ->
  show(This,Show, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcelleditor.html#wxgridcelleditorshow">external documentation</a>.
-spec show(This, Show, [Option]) -> 'ok' when
	This::wxGridCellEditor(), Show::boolean(),
	Option :: {'attr', wxGridCellAttr:wxGridCellAttr()}.
show(#wx_ref{type=ThisT,ref=ThisRef},Show, Options)
 when is_boolean(Show),is_list(Options) ->
  ?CLASS(ThisT,wxGridCellEditor),
  MOpts = fun({attr, #wx_ref{type=AttrT,ref=AttrRef}}, Acc) ->   ?CLASS(AttrT,wxGridCellAttr),[<<1:32/?UI,AttrRef:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxGridCellEditor_Show,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Show)):32/?UI, BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcelleditor.html#wxgridcelleditorpaintbackground">external documentation</a>.
-spec paintBackground(This, RectCell, Attr) -> 'ok' when
	This::wxGridCellEditor(), RectCell::{X::integer(), Y::integer(), W::integer(), H::integer()}, Attr::wxGridCellAttr:wxGridCellAttr().
paintBackground(#wx_ref{type=ThisT,ref=ThisRef},{RectCellX,RectCellY,RectCellW,RectCellH},#wx_ref{type=AttrT,ref=AttrRef})
 when is_integer(RectCellX),is_integer(RectCellY),is_integer(RectCellW),is_integer(RectCellH) ->
  ?CLASS(ThisT,wxGridCellEditor),
  ?CLASS(AttrT,wxGridCellAttr),
  wxe_util:cast(?wxGridCellEditor_PaintBackground,
  <<ThisRef:32/?UI,RectCellX:32/?UI,RectCellY:32/?UI,RectCellW:32/?UI,RectCellH:32/?UI,AttrRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcelleditor.html#wxgridcelleditorbeginedit">external documentation</a>.
-spec beginEdit(This, Row, Col, Grid) -> 'ok' when
	This::wxGridCellEditor(), Row::integer(), Col::integer(), Grid::wxGrid:wxGrid().
beginEdit(#wx_ref{type=ThisT,ref=ThisRef},Row,Col,#wx_ref{type=GridT,ref=GridRef})
 when is_integer(Row),is_integer(Col) ->
  ?CLASS(ThisT,wxGridCellEditor),
  ?CLASS(GridT,wxGrid),
  wxe_util:cast(?wxGridCellEditor_BeginEdit,
  <<ThisRef:32/?UI,Row:32/?UI,Col:32/?UI,GridRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcelleditor.html#wxgridcelleditorendedit">external documentation</a>.
-spec endEdit(This, Row, Col, Grid) -> boolean() when
	This::wxGridCellEditor(), Row::integer(), Col::integer(), Grid::wxGrid:wxGrid().
endEdit(#wx_ref{type=ThisT,ref=ThisRef},Row,Col,#wx_ref{type=GridT,ref=GridRef})
 when is_integer(Row),is_integer(Col) ->
  ?CLASS(ThisT,wxGridCellEditor),
  ?CLASS(GridT,wxGrid),
  wxe_util:call(?wxGridCellEditor_EndEdit,
  <<ThisRef:32/?UI,Row:32/?UI,Col:32/?UI,GridRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcelleditor.html#wxgridcelleditorreset">external documentation</a>.
-spec reset(This) -> 'ok' when
	This::wxGridCellEditor().
reset(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGridCellEditor),
  wxe_util:cast(?wxGridCellEditor_Reset,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcelleditor.html#wxgridcelleditorstartingkey">external documentation</a>.
-spec startingKey(This, Event) -> 'ok' when
	This::wxGridCellEditor(), Event::wxKeyEvent:wxKeyEvent().
startingKey(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=EventT,ref=EventRef}) ->
  ?CLASS(ThisT,wxGridCellEditor),
  ?CLASS(EventT,wxKeyEvent),
  wxe_util:cast(?wxGridCellEditor_StartingKey,
  <<ThisRef:32/?UI,EventRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcelleditor.html#wxgridcelleditorstartingclick">external documentation</a>.
-spec startingClick(This) -> 'ok' when
	This::wxGridCellEditor().
startingClick(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGridCellEditor),
  wxe_util:cast(?wxGridCellEditor_StartingClick,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcelleditor.html#wxgridcelleditorhandlereturn">external documentation</a>.
-spec handleReturn(This, Event) -> 'ok' when
	This::wxGridCellEditor(), Event::wxKeyEvent:wxKeyEvent().
handleReturn(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=EventT,ref=EventRef}) ->
  ?CLASS(ThisT,wxGridCellEditor),
  ?CLASS(EventT,wxKeyEvent),
  wxe_util:cast(?wxGridCellEditor_HandleReturn,
  <<ThisRef:32/?UI,EventRef:32/?UI>>).

