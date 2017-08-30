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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellrenderer.html">wxGridCellRenderer</a>.
%% @type wxGridCellRenderer().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxGridCellRenderer).
-include("wxe.hrl").
-export([draw/8,getBestSize/6]).

%% inherited exports
-export([parent_class/1]).

-export_type([wxGridCellRenderer/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxGridCellRenderer() :: wx:wx_object().
%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellrenderer.html#wxgridcellrendererdraw">external documentation</a>.
-spec draw(This, Grid, Attr, Dc, Rect, Row, Col, IsSelected) -> 'ok' when
	This::wxGridCellRenderer(), Grid::wxGrid:wxGrid(), Attr::wxGridCellAttr:wxGridCellAttr(), Dc::wxDC:wxDC(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()}, Row::integer(), Col::integer(), IsSelected::boolean().
draw(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=GridT,ref=GridRef},#wx_ref{type=AttrT,ref=AttrRef},#wx_ref{type=DcT,ref=DcRef},{RectX,RectY,RectW,RectH},Row,Col,IsSelected)
 when is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH),is_integer(Row),is_integer(Col),is_boolean(IsSelected) ->
  ?CLASS(ThisT,wxGridCellRenderer),
  ?CLASS(GridT,wxGrid),
  ?CLASS(AttrT,wxGridCellAttr),
  ?CLASS(DcT,wxDC),
  wxe_util:cast(?wxGridCellRenderer_Draw,
  <<ThisRef:32/?UI,GridRef:32/?UI,AttrRef:32/?UI,DcRef:32/?UI,RectX:32/?UI,RectY:32/?UI,RectW:32/?UI,RectH:32/?UI,Row:32/?UI,Col:32/?UI,(wxe_util:from_bool(IsSelected)):32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellrenderer.html#wxgridcellrenderergetbestsize">external documentation</a>.
-spec getBestSize(This, Grid, Attr, Dc, Row, Col) -> {W::integer(), H::integer()} when
	This::wxGridCellRenderer(), Grid::wxGrid:wxGrid(), Attr::wxGridCellAttr:wxGridCellAttr(), Dc::wxDC:wxDC(), Row::integer(), Col::integer().
getBestSize(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=GridT,ref=GridRef},#wx_ref{type=AttrT,ref=AttrRef},#wx_ref{type=DcT,ref=DcRef},Row,Col)
 when is_integer(Row),is_integer(Col) ->
  ?CLASS(ThisT,wxGridCellRenderer),
  ?CLASS(GridT,wxGrid),
  ?CLASS(AttrT,wxGridCellAttr),
  ?CLASS(DcT,wxDC),
  wxe_util:call(?wxGridCellRenderer_GetBestSize,
  <<ThisRef:32/?UI,GridRef:32/?UI,AttrRef:32/?UI,DcRef:32/?UI,Row:32/?UI,Col:32/?UI>>).

