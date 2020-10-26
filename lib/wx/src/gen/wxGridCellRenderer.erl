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

-module(wxGridCellRenderer).
-include("wxe.hrl").
-export([draw/8,getBestSize/6]).

%% inherited exports
-export([parent_class/1]).

-type wxGridCellRenderer() :: wx:wx_object().
-export_type([wxGridCellRenderer/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellrenderer.html#wxgridcellrendererdraw">external documentation</a>.
-spec draw(This, Grid, Attr, Dc, Rect, Row, Col, IsSelected) -> 'ok' when
	This::wxGridCellRenderer(), Grid::wxGrid:wxGrid(), Attr::wxGridCellAttr:wxGridCellAttr(), Dc::wxDC:wxDC(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()}, Row::integer(), Col::integer(), IsSelected::boolean().
draw(#wx_ref{type=ThisT}=This,#wx_ref{type=GridT}=Grid,#wx_ref{type=AttrT}=Attr,#wx_ref{type=DcT}=Dc,{RectX,RectY,RectW,RectH} = Rect,Row,Col,IsSelected)
 when is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH),is_integer(Row),is_integer(Col),is_boolean(IsSelected) ->
  ?CLASS(ThisT,wxGridCellRenderer),
  ?CLASS(GridT,wxGrid),
  ?CLASS(AttrT,wxGridCellAttr),
  ?CLASS(DcT,wxDC),
  wxe_util:queue_cmd(This,Grid,Attr,Dc,Rect,Row,Col,IsSelected,?get_env(),?wxGridCellRenderer_Draw).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellrenderer.html#wxgridcellrenderergetbestsize">external documentation</a>.
-spec getBestSize(This, Grid, Attr, Dc, Row, Col) -> {W::integer(), H::integer()} when
	This::wxGridCellRenderer(), Grid::wxGrid:wxGrid(), Attr::wxGridCellAttr:wxGridCellAttr(), Dc::wxDC:wxDC(), Row::integer(), Col::integer().
getBestSize(#wx_ref{type=ThisT}=This,#wx_ref{type=GridT}=Grid,#wx_ref{type=AttrT}=Attr,#wx_ref{type=DcT}=Dc,Row,Col)
 when is_integer(Row),is_integer(Col) ->
  ?CLASS(ThisT,wxGridCellRenderer),
  ?CLASS(GridT,wxGrid),
  ?CLASS(AttrT,wxGridCellAttr),
  ?CLASS(DcT,wxDC),
  wxe_util:queue_cmd(This,Grid,Attr,Dc,Row,Col,?get_env(),?wxGridCellRenderer_GetBestSize),
  wxe_util:rec(?wxGridCellRenderer_GetBestSize).

