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

-module(wxAuiDockArt).
-moduledoc """
`m:wxAuiDockArt` is part of the wxAUI class framework.

See also overview_aui.

`m:wxAuiDockArt` is the art provider: provides all drawing functionality to the wxAui
dock manager. This allows the dock manager to have a pluggable look-and-feel.

By default, a `m:wxAuiManager` uses an instance of this class called `wxAuiDefaultDockArt`
(not implemented in wx) which provides bitmap art and a colour scheme that is adapted to
the major platforms' look. You can either derive from that class to alter its behaviour or
write a completely new dock art class. Call `wxAuiManager:setArtProvider/2` to force wxAUI to use your new dock art provider.

See:
* `m:wxAuiManager`

* `m:wxAuiPaneInfo`

wxWidgets docs: [wxAuiDockArt](https://docs.wxwidgets.org/3.2/classwx_aui_dock_art.html)
""".
-include("wxe.hrl").
-export([getColour/2,getFont/2,getMetric/2,setColour/3,setFont/3,setMetric/3]).

%% inherited exports
-export([parent_class/1]).

-type wxAuiDockArt() :: wx:wx_object().
-export_type([wxAuiDockArt/0]).
-doc false.
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc """
Get the colour of a certain setting.

`id` can be one of the colour values of `wxAuiPaneDockArtSetting`.
""".
-spec getColour(This, Id) -> wx:wx_colour4() when
	This::wxAuiDockArt(), Id::integer().
getColour(#wx_ref{type=ThisT}=This,Id)
 when is_integer(Id) ->
  ?CLASS(ThisT,wxAuiDockArt),
  wxe_util:queue_cmd(This,Id,?get_env(),?wxAuiDockArt_GetColour),
  wxe_util:rec(?wxAuiDockArt_GetColour).

-doc "Get a font setting.".
-spec getFont(This, Id) -> wxFont:wxFont() when
	This::wxAuiDockArt(), Id::integer().
getFont(#wx_ref{type=ThisT}=This,Id)
 when is_integer(Id) ->
  ?CLASS(ThisT,wxAuiDockArt),
  wxe_util:queue_cmd(This,Id,?get_env(),?wxAuiDockArt_GetFont),
  wxe_util:rec(?wxAuiDockArt_GetFont).

-doc """
Get the value of a certain setting.

`id` can be one of the size values of `wxAuiPaneDockArtSetting`.
""".
-spec getMetric(This, Id) -> integer() when
	This::wxAuiDockArt(), Id::integer().
getMetric(#wx_ref{type=ThisT}=This,Id)
 when is_integer(Id) ->
  ?CLASS(ThisT,wxAuiDockArt),
  wxe_util:queue_cmd(This,Id,?get_env(),?wxAuiDockArt_GetMetric),
  wxe_util:rec(?wxAuiDockArt_GetMetric).

-doc """
Set a certain setting with the value `colour`.

`id` can be one of the colour values of `wxAuiPaneDockArtSetting`.
""".
-spec setColour(This, Id, Colour) -> 'ok' when
	This::wxAuiDockArt(), Id::integer(), Colour::wx:wx_colour().
setColour(#wx_ref{type=ThisT}=This,Id,Colour)
 when is_integer(Id),?is_colordata(Colour) ->
  ?CLASS(ThisT,wxAuiDockArt),
  wxe_util:queue_cmd(This,Id,wxe_util:color(Colour),?get_env(),?wxAuiDockArt_SetColour).

-doc "Set a font setting.".
-spec setFont(This, Id, Font) -> 'ok' when
	This::wxAuiDockArt(), Id::integer(), Font::wxFont:wxFont().
setFont(#wx_ref{type=ThisT}=This,Id,#wx_ref{type=FontT}=Font)
 when is_integer(Id) ->
  ?CLASS(ThisT,wxAuiDockArt),
  ?CLASS(FontT,wxFont),
  wxe_util:queue_cmd(This,Id,Font,?get_env(),?wxAuiDockArt_SetFont).

-doc """
Set a certain setting with the value `new\_val`.

`id` can be one of the size values of `wxAuiPaneDockArtSetting`.
""".
-spec setMetric(This, Id, New_val) -> 'ok' when
	This::wxAuiDockArt(), Id::integer(), New_val::integer().
setMetric(#wx_ref{type=ThisT}=This,Id,New_val)
 when is_integer(Id),is_integer(New_val) ->
  ?CLASS(ThisT,wxAuiDockArt),
  wxe_util:queue_cmd(This,Id,New_val,?get_env(),?wxAuiDockArt_SetMetric).

