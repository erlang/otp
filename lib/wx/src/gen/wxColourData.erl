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

-module(wxColourData).
-moduledoc """
This class holds a variety of information related to colour dialogs.

See:
* `wx_color()`

* `m:wxColourDialog`

* [Overview cmndlg](https://docs.wxwidgets.org/3.2/overview_cmndlg.html#overview_cmndlg_colour)

wxWidgets docs: [wxColourData](https://docs.wxwidgets.org/3.2/classwx_colour_data.html)
""".
-include("wxe.hrl").
-export([destroy/1,getChooseFull/1,getColour/1,getCustomColour/2,new/0,setChooseFull/2,
  setColour/2,setCustomColour/3]).

%% inherited exports
-export([parent_class/1]).

-type wxColourData() :: wx:wx_object().
-export_type([wxColourData/0]).
-doc false.
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc """
Constructor.

Initializes the custom colours to `wxNullColour`, the `data` colour setting to black, and
the `choose` full setting to true.
""".
-spec new() -> wxColourData().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxColourData_new),
  wxe_util:rec(?wxColourData_new).

-doc """
Under Windows, determines whether the Windows colour dialog will display the full dialog
with custom colour selection controls.

Has no meaning under other platforms.

The default value is true.
""".
-spec getChooseFull(This) -> boolean() when
	This::wxColourData().
getChooseFull(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxColourData),
  wxe_util:queue_cmd(This,?get_env(),?wxColourData_GetChooseFull),
  wxe_util:rec(?wxColourData_GetChooseFull).

-doc """
Gets the current colour associated with the colour dialog.

The default colour is black.
""".
-spec getColour(This) -> wx:wx_colour4() when
	This::wxColourData().
getColour(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxColourData),
  wxe_util:queue_cmd(This,?get_env(),?wxColourData_GetColour),
  wxe_util:rec(?wxColourData_GetColour).

-doc "Returns custom colours associated with the colour dialog.".
-spec getCustomColour(This, I) -> wx:wx_colour4() when
	This::wxColourData(), I::integer().
getCustomColour(#wx_ref{type=ThisT}=This,I)
 when is_integer(I) ->
  ?CLASS(ThisT,wxColourData),
  wxe_util:queue_cmd(This,I,?get_env(),?wxColourData_GetCustomColour),
  wxe_util:rec(?wxColourData_GetCustomColour).

-doc """
Under Windows, tells the Windows colour dialog to display the full dialog with custom
colour selection controls.

Under other platforms, has no effect.

The default value is true.
""".
-spec setChooseFull(This, Flag) -> 'ok' when
	This::wxColourData(), Flag::boolean().
setChooseFull(#wx_ref{type=ThisT}=This,Flag)
 when is_boolean(Flag) ->
  ?CLASS(ThisT,wxColourData),
  wxe_util:queue_cmd(This,Flag,?get_env(),?wxColourData_SetChooseFull).

-doc """
Sets the default colour for the colour dialog.

The default colour is black.
""".
-spec setColour(This, Colour) -> 'ok' when
	This::wxColourData(), Colour::wx:wx_colour().
setColour(#wx_ref{type=ThisT}=This,Colour)
 when ?is_colordata(Colour) ->
  ?CLASS(ThisT,wxColourData),
  wxe_util:queue_cmd(This,wxe_util:color(Colour),?get_env(),?wxColourData_SetColour).

-doc "Sets custom colours for the colour dialog.".
-spec setCustomColour(This, I, Colour) -> 'ok' when
	This::wxColourData(), I::integer(), Colour::wx:wx_colour().
setCustomColour(#wx_ref{type=ThisT}=This,I,Colour)
 when is_integer(I),?is_colordata(Colour) ->
  ?CLASS(ThisT,wxColourData),
  wxe_util:queue_cmd(This,I,wxe_util:color(Colour),?get_env(),?wxColourData_SetCustomColour).

-doc "Destroys the object".
-spec destroy(This::wxColourData()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxColourData),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
