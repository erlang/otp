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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgridcellrenderer.html">wxGridCellRenderer</a>.
%% @type wxGridCellRenderer().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxGridCellRenderer).
-include("wxe.hrl").
-export([draw/8,getBestSize/6]).

%% inherited exports
-export([parent_class/1]).

%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @spec (This::wxGridCellRenderer(), Grid::wxGrid:wxGrid(), Attr::wxGridCellAttr:wxGridCellAttr(), Dc::wxDC:wxDC(), Rect::{X::integer(),Y::integer(),W::integer(),H::integer()}, Row::integer(), Col::integer(), IsSelected::bool()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgridcellrenderer.html#wxgridcellrendererdraw">external documentation</a>.
draw(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=GridT,ref=GridRef},#wx_ref{type=AttrT,ref=AttrRef},#wx_ref{type=DcT,ref=DcRef},{RectX,RectY,RectW,RectH},Row,Col,IsSelected)
 when is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH),is_integer(Row),is_integer(Col),is_boolean(IsSelected) ->
  ?CLASS(ThisT,wxGridCellRenderer),
  ?CLASS(GridT,wxGrid),
  ?CLASS(AttrT,wxGridCellAttr),
  ?CLASS(DcT,wxDC),
  wxe_util:cast(?wxGridCellRenderer_Draw,
  <<ThisRef:32/?UI,GridRef:32/?UI,AttrRef:32/?UI,DcRef:32/?UI,RectX:32/?UI,RectY:32/?UI,RectW:32/?UI,RectH:32/?UI,Row:32/?UI,Col:32/?UI,(wxe_util:from_bool(IsSelected)):32/?UI>>).

%% @spec (This::wxGridCellRenderer(), Grid::wxGrid:wxGrid(), Attr::wxGridCellAttr:wxGridCellAttr(), Dc::wxDC:wxDC(), Row::integer(), Col::integer()) -> {W::integer(),H::integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgridcellrenderer.html#wxgridcellrenderergetbestsize">external documentation</a>.
getBestSize(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=GridT,ref=GridRef},#wx_ref{type=AttrT,ref=AttrRef},#wx_ref{type=DcT,ref=DcRef},Row,Col)
 when is_integer(Row),is_integer(Col) ->
  ?CLASS(ThisT,wxGridCellRenderer),
  ?CLASS(GridT,wxGrid),
  ?CLASS(AttrT,wxGridCellAttr),
  ?CLASS(DcT,wxDC),
  wxe_util:call(?wxGridCellRenderer_GetBestSize,
  <<ThisRef:32/?UI,GridRef:32/?UI,AttrRef:32/?UI,DcRef:32/?UI,Row:32/?UI,Col:32/?UI>>).

