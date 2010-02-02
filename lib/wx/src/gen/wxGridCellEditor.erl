%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2010. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%% This file is generated DO NOT EDIT

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgridcelleditor.html">wxGridCellEditor</a>.
%% @type wxGridCellEditor().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxGridCellEditor).
-include("wxe.hrl").
-export([beginEdit/4,create/4,endEdit/4,handleReturn/2,isCreated/1,paintBackground/3,
  reset/1,setSize/2,show/2,show/3,startingClick/1,startingKey/2]).

%% inherited exports
-export([parent_class/1]).

%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @spec (This::wxGridCellEditor(), Parent::wxWindow:wxWindow(), Id::integer(), EvtHandler::wxEvtHandler:wxEvtHandler()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgridcelleditor.html#wxgridcelleditorcreate">external documentation</a>.
create(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=ParentT,ref=ParentRef},Id,#wx_ref{type=EvtHandlerT,ref=EvtHandlerRef})
 when is_integer(Id) ->
  ?CLASS(ThisT,wxGridCellEditor),
  ?CLASS(ParentT,wxWindow),
  ?CLASS(EvtHandlerT,wxEvtHandler),
  wxe_util:cast(?wxGridCellEditor_Create,
  <<ThisRef:32/?UI,ParentRef:32/?UI,Id:32/?UI,EvtHandlerRef:32/?UI>>).

%% @spec (This::wxGridCellEditor()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgridcelleditor.html#wxgridcelleditoriscreated">external documentation</a>.
isCreated(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGridCellEditor),
  wxe_util:call(?wxGridCellEditor_IsCreated,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxGridCellEditor(), Rect::{X::integer(),Y::integer(),W::integer(),H::integer()}) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgridcelleditor.html#wxgridcelleditorsetsize">external documentation</a>.
setSize(#wx_ref{type=ThisT,ref=ThisRef},{RectX,RectY,RectW,RectH})
 when is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH) ->
  ?CLASS(ThisT,wxGridCellEditor),
  wxe_util:cast(?wxGridCellEditor_SetSize,
  <<ThisRef:32/?UI,RectX:32/?UI,RectY:32/?UI,RectW:32/?UI,RectH:32/?UI>>).

%% @spec (This::wxGridCellEditor(), Show::bool()) -> ok
%% @equiv show(This,Show, [])
show(This,Show)
 when is_record(This, wx_ref),is_boolean(Show) ->
  show(This,Show, []).

%% @spec (This::wxGridCellEditor(), Show::bool(), [Option]) -> ok
%% Option = {attr, wxGridCellAttr:wxGridCellAttr()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgridcelleditor.html#wxgridcelleditorshow">external documentation</a>.
show(#wx_ref{type=ThisT,ref=ThisRef},Show, Options)
 when is_boolean(Show),is_list(Options) ->
  ?CLASS(ThisT,wxGridCellEditor),
  MOpts = fun({attr, #wx_ref{type=AttrT,ref=AttrRef}}, Acc) ->   ?CLASS(AttrT,wxGridCellAttr),[<<1:32/?UI,AttrRef:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxGridCellEditor_Show,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Show)):32/?UI, BinOpt/binary>>).

%% @spec (This::wxGridCellEditor(), RectCell::{X::integer(),Y::integer(),W::integer(),H::integer()}, Attr::wxGridCellAttr:wxGridCellAttr()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgridcelleditor.html#wxgridcelleditorpaintbackground">external documentation</a>.
paintBackground(#wx_ref{type=ThisT,ref=ThisRef},{RectCellX,RectCellY,RectCellW,RectCellH},#wx_ref{type=AttrT,ref=AttrRef})
 when is_integer(RectCellX),is_integer(RectCellY),is_integer(RectCellW),is_integer(RectCellH) ->
  ?CLASS(ThisT,wxGridCellEditor),
  ?CLASS(AttrT,wxGridCellAttr),
  wxe_util:cast(?wxGridCellEditor_PaintBackground,
  <<ThisRef:32/?UI,RectCellX:32/?UI,RectCellY:32/?UI,RectCellW:32/?UI,RectCellH:32/?UI,AttrRef:32/?UI>>).

%% @spec (This::wxGridCellEditor(), Row::integer(), Col::integer(), Grid::wxGrid:wxGrid()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgridcelleditor.html#wxgridcelleditorbeginedit">external documentation</a>.
beginEdit(#wx_ref{type=ThisT,ref=ThisRef},Row,Col,#wx_ref{type=GridT,ref=GridRef})
 when is_integer(Row),is_integer(Col) ->
  ?CLASS(ThisT,wxGridCellEditor),
  ?CLASS(GridT,wxGrid),
  wxe_util:cast(?wxGridCellEditor_BeginEdit,
  <<ThisRef:32/?UI,Row:32/?UI,Col:32/?UI,GridRef:32/?UI>>).

%% @spec (This::wxGridCellEditor(), Row::integer(), Col::integer(), Grid::wxGrid:wxGrid()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgridcelleditor.html#wxgridcelleditorendedit">external documentation</a>.
endEdit(#wx_ref{type=ThisT,ref=ThisRef},Row,Col,#wx_ref{type=GridT,ref=GridRef})
 when is_integer(Row),is_integer(Col) ->
  ?CLASS(ThisT,wxGridCellEditor),
  ?CLASS(GridT,wxGrid),
  wxe_util:call(?wxGridCellEditor_EndEdit,
  <<ThisRef:32/?UI,Row:32/?UI,Col:32/?UI,GridRef:32/?UI>>).

%% @spec (This::wxGridCellEditor()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgridcelleditor.html#wxgridcelleditorreset">external documentation</a>.
reset(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGridCellEditor),
  wxe_util:cast(?wxGridCellEditor_Reset,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxGridCellEditor(), Event::wxKeyEvent:wxKeyEvent()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgridcelleditor.html#wxgridcelleditorstartingkey">external documentation</a>.
startingKey(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=EventT,ref=EventRef}) ->
  ?CLASS(ThisT,wxGridCellEditor),
  ?CLASS(EventT,wxKeyEvent),
  wxe_util:cast(?wxGridCellEditor_StartingKey,
  <<ThisRef:32/?UI,EventRef:32/?UI>>).

%% @spec (This::wxGridCellEditor()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgridcelleditor.html#wxgridcelleditorstartingclick">external documentation</a>.
startingClick(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGridCellEditor),
  wxe_util:cast(?wxGridCellEditor_StartingClick,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxGridCellEditor(), Event::wxKeyEvent:wxKeyEvent()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgridcelleditor.html#wxgridcelleditorhandlereturn">external documentation</a>.
handleReturn(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=EventT,ref=EventRef}) ->
  ?CLASS(ThisT,wxGridCellEditor),
  ?CLASS(EventT,wxKeyEvent),
  wxe_util:cast(?wxGridCellEditor_HandleReturn,
  <<ThisRef:32/?UI,EventRef:32/?UI>>).

