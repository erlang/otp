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

-module(wxGridCellAttr).
-include("wxe.hrl").
-export([getAlignment/1,getBackgroundColour/1,getEditor/4,getFont/1,getRenderer/4,
  getTextColour/1,hasAlignment/1,hasBackgroundColour/1,hasEditor/1,
  hasFont/1,hasRenderer/1,hasTextColour/1,isReadOnly/1,setAlignment/3,
  setBackgroundColour/2,setDefAttr/2,setEditor/2,setFont/2,setReadOnly/1,
  setReadOnly/2,setRenderer/2,setTextColour/2]).

%% inherited exports
-export([parent_class/1]).

-type wxGridCellAttr() :: wx:wx_object().
-export_type([wxGridCellAttr/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellattr.html#wxgridcellattrsettextcolour">external documentation</a>.
-spec setTextColour(This, ColText) -> 'ok' when
	This::wxGridCellAttr(), ColText::wx:wx_colour().
setTextColour(#wx_ref{type=ThisT}=This,ColText)
 when tuple_size(ColText) =:= 3; tuple_size(ColText) =:= 4 ->
  ?CLASS(ThisT,wxGridCellAttr),
  wxe_util:queue_cmd(This,wxe_util:color(ColText),?get_env(),?wxGridCellAttr_SetTextColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellattr.html#wxgridcellattrsetbackgroundcolour">external documentation</a>.
-spec setBackgroundColour(This, ColBack) -> 'ok' when
	This::wxGridCellAttr(), ColBack::wx:wx_colour().
setBackgroundColour(#wx_ref{type=ThisT}=This,ColBack)
 when tuple_size(ColBack) =:= 3; tuple_size(ColBack) =:= 4 ->
  ?CLASS(ThisT,wxGridCellAttr),
  wxe_util:queue_cmd(This,wxe_util:color(ColBack),?get_env(),?wxGridCellAttr_SetBackgroundColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellattr.html#wxgridcellattrsetfont">external documentation</a>.
-spec setFont(This, Font) -> 'ok' when
	This::wxGridCellAttr(), Font::wxFont:wxFont().
setFont(#wx_ref{type=ThisT}=This,#wx_ref{type=FontT}=Font) ->
  ?CLASS(ThisT,wxGridCellAttr),
  ?CLASS(FontT,wxFont),
  wxe_util:queue_cmd(This,Font,?get_env(),?wxGridCellAttr_SetFont).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellattr.html#wxgridcellattrsetalignment">external documentation</a>.
-spec setAlignment(This, HAlign, VAlign) -> 'ok' when
	This::wxGridCellAttr(), HAlign::integer(), VAlign::integer().
setAlignment(#wx_ref{type=ThisT}=This,HAlign,VAlign)
 when is_integer(HAlign),is_integer(VAlign) ->
  ?CLASS(ThisT,wxGridCellAttr),
  wxe_util:queue_cmd(This,HAlign,VAlign,?get_env(),?wxGridCellAttr_SetAlignment).

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
setReadOnly(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxGridCellAttr),
  MOpts = fun({isReadOnly, _isReadOnly} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxGridCellAttr_SetReadOnly).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellattr.html#wxgridcellattrsetrenderer">external documentation</a>.
-spec setRenderer(This, Renderer) -> 'ok' when
	This::wxGridCellAttr(), Renderer::wxGridCellRenderer:wxGridCellRenderer().
setRenderer(#wx_ref{type=ThisT}=This,#wx_ref{type=RendererT}=Renderer) ->
  ?CLASS(ThisT,wxGridCellAttr),
  ?CLASS(RendererT,wxGridCellRenderer),
  wxe_util:queue_cmd(This,Renderer,?get_env(),?wxGridCellAttr_SetRenderer).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellattr.html#wxgridcellattrseteditor">external documentation</a>.
-spec setEditor(This, Editor) -> 'ok' when
	This::wxGridCellAttr(), Editor::wxGridCellEditor:wxGridCellEditor().
setEditor(#wx_ref{type=ThisT}=This,#wx_ref{type=EditorT}=Editor) ->
  ?CLASS(ThisT,wxGridCellAttr),
  ?CLASS(EditorT,wxGridCellEditor),
  wxe_util:queue_cmd(This,Editor,?get_env(),?wxGridCellAttr_SetEditor).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellattr.html#wxgridcellattrhastextcolour">external documentation</a>.
-spec hasTextColour(This) -> boolean() when
	This::wxGridCellAttr().
hasTextColour(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGridCellAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxGridCellAttr_HasTextColour),
  wxe_util:rec(?wxGridCellAttr_HasTextColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellattr.html#wxgridcellattrhasbackgroundcolour">external documentation</a>.
-spec hasBackgroundColour(This) -> boolean() when
	This::wxGridCellAttr().
hasBackgroundColour(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGridCellAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxGridCellAttr_HasBackgroundColour),
  wxe_util:rec(?wxGridCellAttr_HasBackgroundColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellattr.html#wxgridcellattrhasfont">external documentation</a>.
-spec hasFont(This) -> boolean() when
	This::wxGridCellAttr().
hasFont(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGridCellAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxGridCellAttr_HasFont),
  wxe_util:rec(?wxGridCellAttr_HasFont).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellattr.html#wxgridcellattrhasalignment">external documentation</a>.
-spec hasAlignment(This) -> boolean() when
	This::wxGridCellAttr().
hasAlignment(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGridCellAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxGridCellAttr_HasAlignment),
  wxe_util:rec(?wxGridCellAttr_HasAlignment).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellattr.html#wxgridcellattrhasrenderer">external documentation</a>.
-spec hasRenderer(This) -> boolean() when
	This::wxGridCellAttr().
hasRenderer(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGridCellAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxGridCellAttr_HasRenderer),
  wxe_util:rec(?wxGridCellAttr_HasRenderer).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellattr.html#wxgridcellattrhaseditor">external documentation</a>.
-spec hasEditor(This) -> boolean() when
	This::wxGridCellAttr().
hasEditor(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGridCellAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxGridCellAttr_HasEditor),
  wxe_util:rec(?wxGridCellAttr_HasEditor).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellattr.html#wxgridcellattrgettextcolour">external documentation</a>.
-spec getTextColour(This) -> wx:wx_colour4() when
	This::wxGridCellAttr().
getTextColour(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGridCellAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxGridCellAttr_GetTextColour),
  wxe_util:rec(?wxGridCellAttr_GetTextColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellattr.html#wxgridcellattrgetbackgroundcolour">external documentation</a>.
-spec getBackgroundColour(This) -> wx:wx_colour4() when
	This::wxGridCellAttr().
getBackgroundColour(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGridCellAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxGridCellAttr_GetBackgroundColour),
  wxe_util:rec(?wxGridCellAttr_GetBackgroundColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellattr.html#wxgridcellattrgetfont">external documentation</a>.
-spec getFont(This) -> wxFont:wxFont() when
	This::wxGridCellAttr().
getFont(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGridCellAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxGridCellAttr_GetFont),
  wxe_util:rec(?wxGridCellAttr_GetFont).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellattr.html#wxgridcellattrgetalignment">external documentation</a>.
-spec getAlignment(This) -> {HAlign::integer(), VAlign::integer()} when
	This::wxGridCellAttr().
getAlignment(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGridCellAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxGridCellAttr_GetAlignment),
  wxe_util:rec(?wxGridCellAttr_GetAlignment).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellattr.html#wxgridcellattrgetrenderer">external documentation</a>.
-spec getRenderer(This, Grid, Row, Col) -> wxGridCellRenderer:wxGridCellRenderer() when
	This::wxGridCellAttr(), Grid::wxGrid:wxGrid(), Row::integer(), Col::integer().
getRenderer(#wx_ref{type=ThisT}=This,#wx_ref{type=GridT}=Grid,Row,Col)
 when is_integer(Row),is_integer(Col) ->
  ?CLASS(ThisT,wxGridCellAttr),
  ?CLASS(GridT,wxGrid),
  wxe_util:queue_cmd(This,Grid,Row,Col,?get_env(),?wxGridCellAttr_GetRenderer),
  wxe_util:rec(?wxGridCellAttr_GetRenderer).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellattr.html#wxgridcellattrgeteditor">external documentation</a>.
-spec getEditor(This, Grid, Row, Col) -> wxGridCellEditor:wxGridCellEditor() when
	This::wxGridCellAttr(), Grid::wxGrid:wxGrid(), Row::integer(), Col::integer().
getEditor(#wx_ref{type=ThisT}=This,#wx_ref{type=GridT}=Grid,Row,Col)
 when is_integer(Row),is_integer(Col) ->
  ?CLASS(ThisT,wxGridCellAttr),
  ?CLASS(GridT,wxGrid),
  wxe_util:queue_cmd(This,Grid,Row,Col,?get_env(),?wxGridCellAttr_GetEditor),
  wxe_util:rec(?wxGridCellAttr_GetEditor).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellattr.html#wxgridcellattrisreadonly">external documentation</a>.
-spec isReadOnly(This) -> boolean() when
	This::wxGridCellAttr().
isReadOnly(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGridCellAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxGridCellAttr_IsReadOnly),
  wxe_util:rec(?wxGridCellAttr_IsReadOnly).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellattr.html#wxgridcellattrsetdefattr">external documentation</a>.
-spec setDefAttr(This, DefAttr) -> 'ok' when
	This::wxGridCellAttr(), DefAttr::wxGridCellAttr().
setDefAttr(#wx_ref{type=ThisT}=This,#wx_ref{type=DefAttrT}=DefAttr) ->
  ?CLASS(ThisT,wxGridCellAttr),
  ?CLASS(DefAttrT,wxGridCellAttr),
  wxe_util:queue_cmd(This,DefAttr,?get_env(),?wxGridCellAttr_SetDefAttr).

