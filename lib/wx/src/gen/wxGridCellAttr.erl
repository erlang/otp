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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellattr.html">wxGridCellAttr</a>.
%% @type wxGridCellAttr().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxGridCellAttr).
-include("wxe.hrl").
-export([getAlignment/1,getBackgroundColour/1,getEditor/4,getFont/1,getRenderer/4,
  getTextColour/1,hasAlignment/1,hasBackgroundColour/1,hasEditor/1,
  hasFont/1,hasRenderer/1,hasTextColour/1,isReadOnly/1,setAlignment/3,
  setBackgroundColour/2,setDefAttr/2,setEditor/2,setFont/2,setReadOnly/1,
  setReadOnly/2,setRenderer/2,setTextColour/2]).

%% inherited exports
-export([parent_class/1]).

-export_type([wxGridCellAttr/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxGridCellAttr() :: wx:wx_object().
%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellattr.html#wxgridcellattrsettextcolour">external documentation</a>.
-spec setTextColour(This, ColText) -> 'ok' when
	This::wxGridCellAttr(), ColText::wx:wx_colour().
setTextColour(#wx_ref{type=ThisT,ref=ThisRef},ColText)
 when tuple_size(ColText) =:= 3; tuple_size(ColText) =:= 4 ->
  ?CLASS(ThisT,wxGridCellAttr),
  wxe_util:cast(?wxGridCellAttr_SetTextColour,
  <<ThisRef:32/?UI,(wxe_util:colour_bin(ColText)):16/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellattr.html#wxgridcellattrsetbackgroundcolour">external documentation</a>.
-spec setBackgroundColour(This, ColBack) -> 'ok' when
	This::wxGridCellAttr(), ColBack::wx:wx_colour().
setBackgroundColour(#wx_ref{type=ThisT,ref=ThisRef},ColBack)
 when tuple_size(ColBack) =:= 3; tuple_size(ColBack) =:= 4 ->
  ?CLASS(ThisT,wxGridCellAttr),
  wxe_util:cast(?wxGridCellAttr_SetBackgroundColour,
  <<ThisRef:32/?UI,(wxe_util:colour_bin(ColBack)):16/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellattr.html#wxgridcellattrsetfont">external documentation</a>.
-spec setFont(This, Font) -> 'ok' when
	This::wxGridCellAttr(), Font::wxFont:wxFont().
setFont(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=FontT,ref=FontRef}) ->
  ?CLASS(ThisT,wxGridCellAttr),
  ?CLASS(FontT,wxFont),
  wxe_util:cast(?wxGridCellAttr_SetFont,
  <<ThisRef:32/?UI,FontRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellattr.html#wxgridcellattrsetalignment">external documentation</a>.
-spec setAlignment(This, HAlign, VAlign) -> 'ok' when
	This::wxGridCellAttr(), HAlign::integer(), VAlign::integer().
setAlignment(#wx_ref{type=ThisT,ref=ThisRef},HAlign,VAlign)
 when is_integer(HAlign),is_integer(VAlign) ->
  ?CLASS(ThisT,wxGridCellAttr),
  wxe_util:cast(?wxGridCellAttr_SetAlignment,
  <<ThisRef:32/?UI,HAlign:32/?UI,VAlign:32/?UI>>).

%% @equiv setReadOnly(This, [])
-spec setReadOnly(This) -> 'ok' when
	This::wxGridCellAttr().

setReadOnly(This)
 when is_record(This, wx_ref) ->
  setReadOnly(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellattr.html#wxgridcellattrsetreadonly">external documentation</a>.
-spec setReadOnly(This, [Option]) -> 'ok' when
	This::wxGridCellAttr(),
	Option :: {'isReadOnly', boolean()}.
setReadOnly(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxGridCellAttr),
  MOpts = fun({isReadOnly, IsReadOnly}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(IsReadOnly)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxGridCellAttr_SetReadOnly,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellattr.html#wxgridcellattrsetrenderer">external documentation</a>.
-spec setRenderer(This, Renderer) -> 'ok' when
	This::wxGridCellAttr(), Renderer::wxGridCellRenderer:wxGridCellRenderer().
setRenderer(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=RendererT,ref=RendererRef}) ->
  ?CLASS(ThisT,wxGridCellAttr),
  ?CLASS(RendererT,wxGridCellRenderer),
  wxe_util:cast(?wxGridCellAttr_SetRenderer,
  <<ThisRef:32/?UI,RendererRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellattr.html#wxgridcellattrseteditor">external documentation</a>.
-spec setEditor(This, Editor) -> 'ok' when
	This::wxGridCellAttr(), Editor::wxGridCellEditor:wxGridCellEditor().
setEditor(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=EditorT,ref=EditorRef}) ->
  ?CLASS(ThisT,wxGridCellAttr),
  ?CLASS(EditorT,wxGridCellEditor),
  wxe_util:cast(?wxGridCellAttr_SetEditor,
  <<ThisRef:32/?UI,EditorRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellattr.html#wxgridcellattrhastextcolour">external documentation</a>.
-spec hasTextColour(This) -> boolean() when
	This::wxGridCellAttr().
hasTextColour(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGridCellAttr),
  wxe_util:call(?wxGridCellAttr_HasTextColour,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellattr.html#wxgridcellattrhasbackgroundcolour">external documentation</a>.
-spec hasBackgroundColour(This) -> boolean() when
	This::wxGridCellAttr().
hasBackgroundColour(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGridCellAttr),
  wxe_util:call(?wxGridCellAttr_HasBackgroundColour,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellattr.html#wxgridcellattrhasfont">external documentation</a>.
-spec hasFont(This) -> boolean() when
	This::wxGridCellAttr().
hasFont(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGridCellAttr),
  wxe_util:call(?wxGridCellAttr_HasFont,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellattr.html#wxgridcellattrhasalignment">external documentation</a>.
-spec hasAlignment(This) -> boolean() when
	This::wxGridCellAttr().
hasAlignment(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGridCellAttr),
  wxe_util:call(?wxGridCellAttr_HasAlignment,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellattr.html#wxgridcellattrhasrenderer">external documentation</a>.
-spec hasRenderer(This) -> boolean() when
	This::wxGridCellAttr().
hasRenderer(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGridCellAttr),
  wxe_util:call(?wxGridCellAttr_HasRenderer,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellattr.html#wxgridcellattrhaseditor">external documentation</a>.
-spec hasEditor(This) -> boolean() when
	This::wxGridCellAttr().
hasEditor(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGridCellAttr),
  wxe_util:call(?wxGridCellAttr_HasEditor,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellattr.html#wxgridcellattrgettextcolour">external documentation</a>.
-spec getTextColour(This) -> wx:wx_colour4() when
	This::wxGridCellAttr().
getTextColour(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGridCellAttr),
  wxe_util:call(?wxGridCellAttr_GetTextColour,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellattr.html#wxgridcellattrgetbackgroundcolour">external documentation</a>.
-spec getBackgroundColour(This) -> wx:wx_colour4() when
	This::wxGridCellAttr().
getBackgroundColour(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGridCellAttr),
  wxe_util:call(?wxGridCellAttr_GetBackgroundColour,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellattr.html#wxgridcellattrgetfont">external documentation</a>.
-spec getFont(This) -> wxFont:wxFont() when
	This::wxGridCellAttr().
getFont(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGridCellAttr),
  wxe_util:call(?wxGridCellAttr_GetFont,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellattr.html#wxgridcellattrgetalignment">external documentation</a>.
-spec getAlignment(This) -> {HAlign::integer(), VAlign::integer()} when
	This::wxGridCellAttr().
getAlignment(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGridCellAttr),
  wxe_util:call(?wxGridCellAttr_GetAlignment,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellattr.html#wxgridcellattrgetrenderer">external documentation</a>.
-spec getRenderer(This, Grid, Row, Col) -> wxGridCellRenderer:wxGridCellRenderer() when
	This::wxGridCellAttr(), Grid::wxGrid:wxGrid(), Row::integer(), Col::integer().
getRenderer(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=GridT,ref=GridRef},Row,Col)
 when is_integer(Row),is_integer(Col) ->
  ?CLASS(ThisT,wxGridCellAttr),
  ?CLASS(GridT,wxGrid),
  wxe_util:call(?wxGridCellAttr_GetRenderer,
  <<ThisRef:32/?UI,GridRef:32/?UI,Row:32/?UI,Col:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellattr.html#wxgridcellattrgeteditor">external documentation</a>.
-spec getEditor(This, Grid, Row, Col) -> wxGridCellEditor:wxGridCellEditor() when
	This::wxGridCellAttr(), Grid::wxGrid:wxGrid(), Row::integer(), Col::integer().
getEditor(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=GridT,ref=GridRef},Row,Col)
 when is_integer(Row),is_integer(Col) ->
  ?CLASS(ThisT,wxGridCellAttr),
  ?CLASS(GridT,wxGrid),
  wxe_util:call(?wxGridCellAttr_GetEditor,
  <<ThisRef:32/?UI,GridRef:32/?UI,Row:32/?UI,Col:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellattr.html#wxgridcellattrisreadonly">external documentation</a>.
-spec isReadOnly(This) -> boolean() when
	This::wxGridCellAttr().
isReadOnly(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGridCellAttr),
  wxe_util:call(?wxGridCellAttr_IsReadOnly,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellattr.html#wxgridcellattrsetdefattr">external documentation</a>.
-spec setDefAttr(This, DefAttr) -> 'ok' when
	This::wxGridCellAttr(), DefAttr::wxGridCellAttr().
setDefAttr(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=DefAttrT,ref=DefAttrRef}) ->
  ?CLASS(ThisT,wxGridCellAttr),
  ?CLASS(DefAttrT,wxGridCellAttr),
  wxe_util:cast(?wxGridCellAttr_SetDefAttr,
  <<ThisRef:32/?UI,DefAttrRef:32/?UI>>).

