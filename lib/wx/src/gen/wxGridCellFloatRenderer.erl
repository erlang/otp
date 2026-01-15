%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0 AND LicenseRef-scancode-wxwindows-free-doc-3
%%
%% Copyright Ericsson AB 2009-2025. All Rights Reserved.
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

-module(wxGridCellFloatRenderer).
-moduledoc """
This class may be used to format floating point data in a cell.

See:
* `m:wxGridCellRenderer`

* `m:wxGridCellBoolRenderer`

* `m:wxGridCellNumberRenderer`

* `m:wxGridCellStringRenderer`

This class is derived, and can use functions, from:

* `m:wxGridCellStringRenderer`

* `m:wxGridCellRenderer`

wxWidgets docs: [wxGridCellFloatRenderer](https://docs.wxwidgets.org/3.2/classwx_grid_cell_float_renderer.html)
""".
-include("wxe.hrl").
-export([destroy/1,getPrecision/1,getWidth/1,new/0,new/1,setParameters/2,setPrecision/2,
  setWidth/2]).

%% inherited exports
-export([draw/8,getBestSize/6,parent_class/1]).

-type wxGridCellFloatRenderer() :: wx:wx_object().
-export_type([wxGridCellFloatRenderer/0]).
-doc false.
parent_class(wxGridCellStringRenderer) -> true;
parent_class(wxGridCellRenderer) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc(#{equiv => new([])}).
-spec new() -> wxGridCellFloatRenderer().

new() ->
  new([]).

-doc "Float cell renderer ctor.".
-spec new([Option]) -> wxGridCellFloatRenderer() when
	Option :: {'width', integer()}
		 | {'precision', integer()}
		 | {'format', integer()}.
new(Options)
 when is_list(Options) ->
  MOpts = fun({width, _width} = Arg) -> Arg;
          ({precision, _precision} = Arg) -> Arg;
          ({format, _format} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Opts,?get_env(),?wxGridCellFloatRenderer_new),
  wxe_util:rec(?wxGridCellFloatRenderer_new).

-doc "Returns the precision.".
-spec getPrecision(This) -> integer() when
	This::wxGridCellFloatRenderer().
getPrecision(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGridCellFloatRenderer),
  wxe_util:queue_cmd(This,?get_env(),?wxGridCellFloatRenderer_GetPrecision),
  wxe_util:rec(?wxGridCellFloatRenderer_GetPrecision).

-doc "Returns the width.".
-spec getWidth(This) -> integer() when
	This::wxGridCellFloatRenderer().
getWidth(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGridCellFloatRenderer),
  wxe_util:queue_cmd(This,?get_env(),?wxGridCellFloatRenderer_GetWidth),
  wxe_util:rec(?wxGridCellFloatRenderer_GetWidth).

-doc """
The parameters string format is "width[,precision[,format]]" where `format` should be
chosen between f|e|g|E|G (f is used by default)
""".
-spec setParameters(This, Params) -> 'ok' when
	This::wxGridCellFloatRenderer(), Params::unicode:chardata().
setParameters(#wx_ref{type=ThisT}=This,Params)
 when ?is_chardata(Params) ->
  ?CLASS(ThisT,wxGridCellFloatRenderer),
  Params_UC = unicode:characters_to_binary(Params),
  wxe_util:queue_cmd(This,Params_UC,?get_env(),?wxGridCellFloatRenderer_SetParameters).

-doc "Sets the precision.".
-spec setPrecision(This, Precision) -> 'ok' when
	This::wxGridCellFloatRenderer(), Precision::integer().
setPrecision(#wx_ref{type=ThisT}=This,Precision)
 when is_integer(Precision) ->
  ?CLASS(ThisT,wxGridCellFloatRenderer),
  wxe_util:queue_cmd(This,Precision,?get_env(),?wxGridCellFloatRenderer_SetPrecision).

-doc "Sets the width.".
-spec setWidth(This, Width) -> 'ok' when
	This::wxGridCellFloatRenderer(), Width::integer().
setWidth(#wx_ref{type=ThisT}=This,Width)
 when is_integer(Width) ->
  ?CLASS(ThisT,wxGridCellFloatRenderer),
  wxe_util:queue_cmd(This,Width,?get_env(),?wxGridCellFloatRenderer_SetWidth).

-doc "Destroys the object".
-spec destroy(This::wxGridCellFloatRenderer()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxGridCellFloatRenderer),
  wxe_util:queue_cmd(Obj, ?get_env(), ?wxGridCellFloatRenderer_destroy),
  ok.
 %% From wxGridCellStringRenderer
 %% From wxGridCellRenderer
-doc false.
getBestSize(This,Grid,Attr,Dc,Row,Col) -> wxGridCellRenderer:getBestSize(This,Grid,Attr,Dc,Row,Col).
-doc false.
draw(This,Grid,Attr,Dc,Rect,Row,Col,IsSelected) -> wxGridCellRenderer:draw(This,Grid,Attr,Dc,Rect,Row,Col,IsSelected).
