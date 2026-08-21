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

-module(wxFontData).
-moduledoc """
This class holds a variety of information related to font dialogs.

See:
* [Overview cmndlg](https://docs.wxwidgets.org/3.2/overview_cmndlg.html#overview_cmndlg_font)

* `m:wxFont`

* `m:wxFontDialog`

wxWidgets docs: [wxFontData](https://docs.wxwidgets.org/3.2/classwx_font_data.html)
""".
-include("wxe.hrl").
-export([destroy/1,enableEffects/2,getAllowSymbols/1,getChosenFont/1,getColour/1,
  getEnableEffects/1,getInitialFont/1,getShowHelp/1,new/0,new/1,setAllowSymbols/2,
  setChosenFont/2,setColour/2,setInitialFont/2,setRange/3,setShowHelp/2]).

%% inherited exports
-export([parent_class/1]).

-type wxFontData() :: wx:wx_object().
-export_type([wxFontData/0]).
-doc false.
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc """
Constructor.

Initializes `fontColour` to black, `showHelp` to false, `allowSymbols` to true, `enableEffects`
to true, `minSize` to 0 and `maxSize` to 0.
""".
-spec new() -> wxFontData().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxFontData_new_0),
  wxe_util:rec(?wxFontData_new_0).

-doc "Copy Constructor.".
-spec new(Data) -> wxFontData() when
	Data::wxFontData().
new(#wx_ref{type=DataT}=Data) ->
  ?CLASS(DataT,wxFontData),
  wxe_util:queue_cmd(Data,?get_env(),?wxFontData_new_1),
  wxe_util:rec(?wxFontData_new_1).

-doc """
Enables or disables "effects" under Windows or generic only.

This refers to the controls for manipulating colour, strikeout and underline properties.

The default value is true.
""".
-spec enableEffects(This, Enable) -> 'ok' when
	This::wxFontData(), Enable::boolean().
enableEffects(#wx_ref{type=ThisT}=This,Enable)
 when is_boolean(Enable) ->
  ?CLASS(ThisT,wxFontData),
  wxe_util:queue_cmd(This,Enable,?get_env(),?wxFontData_EnableEffects).

-doc """
Under Windows, returns a flag determining whether symbol fonts can be selected.

Has no effect on other platforms.

The default value is true.
""".
-spec getAllowSymbols(This) -> boolean() when
	This::wxFontData().
getAllowSymbols(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxFontData),
  wxe_util:queue_cmd(This,?get_env(),?wxFontData_GetAllowSymbols),
  wxe_util:rec(?wxFontData_GetAllowSymbols).

-doc """
Gets the colour associated with the font dialog.

The default value is black.
""".
-spec getColour(This) -> wx:wx_colour4() when
	This::wxFontData().
getColour(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxFontData),
  wxe_util:queue_cmd(This,?get_env(),?wxFontData_GetColour),
  wxe_util:rec(?wxFontData_GetColour).

-doc """
Gets the font chosen by the user if the user pressed OK (`wxFontDialog::ShowModal()` (not
implemented in wx) returned wxID\_OK).
""".
-spec getChosenFont(This) -> wxFont:wxFont() when
	This::wxFontData().
getChosenFont(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxFontData),
  wxe_util:queue_cmd(This,?get_env(),?wxFontData_GetChosenFont),
  wxe_util:rec(?wxFontData_GetChosenFont).

-doc """
Determines whether "effects" are enabled under Windows.

This refers to the controls for manipulating colour, strikeout and underline properties.

The default value is true.
""".
-spec getEnableEffects(This) -> boolean() when
	This::wxFontData().
getEnableEffects(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxFontData),
  wxe_util:queue_cmd(This,?get_env(),?wxFontData_GetEnableEffects),
  wxe_util:rec(?wxFontData_GetEnableEffects).

-doc """
Gets the font that will be initially used by the font dialog.

This should have previously been set by the application.
""".
-spec getInitialFont(This) -> wxFont:wxFont() when
	This::wxFontData().
getInitialFont(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxFontData),
  wxe_util:queue_cmd(This,?get_env(),?wxFontData_GetInitialFont),
  wxe_util:rec(?wxFontData_GetInitialFont).

-doc """
Returns true if the Help button will be shown (Windows only).

The default value is false.
""".
-spec getShowHelp(This) -> boolean() when
	This::wxFontData().
getShowHelp(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxFontData),
  wxe_util:queue_cmd(This,?get_env(),?wxFontData_GetShowHelp),
  wxe_util:rec(?wxFontData_GetShowHelp).

-doc """
Under Windows, determines whether symbol fonts can be selected.

Has no effect on other platforms.

The default value is true.
""".
-spec setAllowSymbols(This, AllowSymbols) -> 'ok' when
	This::wxFontData(), AllowSymbols::boolean().
setAllowSymbols(#wx_ref{type=ThisT}=This,AllowSymbols)
 when is_boolean(AllowSymbols) ->
  ?CLASS(ThisT,wxFontData),
  wxe_util:queue_cmd(This,AllowSymbols,?get_env(),?wxFontData_SetAllowSymbols).

-doc "Sets the font that will be returned to the user (for internal use only).".
-spec setChosenFont(This, Font) -> 'ok' when
	This::wxFontData(), Font::wxFont:wxFont().
setChosenFont(#wx_ref{type=ThisT}=This,#wx_ref{type=FontT}=Font) ->
  ?CLASS(ThisT,wxFontData),
  ?CLASS(FontT,wxFont),
  wxe_util:queue_cmd(This,Font,?get_env(),?wxFontData_SetChosenFont).

-doc """
Sets the colour that will be used for the font foreground colour.

The default colour is black.
""".
-spec setColour(This, Colour) -> 'ok' when
	This::wxFontData(), Colour::wx:wx_colour().
setColour(#wx_ref{type=ThisT}=This,Colour)
 when ?is_colordata(Colour) ->
  ?CLASS(ThisT,wxFontData),
  wxe_util:queue_cmd(This,wxe_util:color(Colour),?get_env(),?wxFontData_SetColour).

-doc "Sets the font that will be initially used by the font dialog.".
-spec setInitialFont(This, Font) -> 'ok' when
	This::wxFontData(), Font::wxFont:wxFont().
setInitialFont(#wx_ref{type=ThisT}=This,#wx_ref{type=FontT}=Font) ->
  ?CLASS(ThisT,wxFontData),
  ?CLASS(FontT,wxFont),
  wxe_util:queue_cmd(This,Font,?get_env(),?wxFontData_SetInitialFont).

-doc """
Sets the valid range for the font point size (Windows only).

The default is 0, 0 (unrestricted range).
""".
-spec setRange(This, Min, Max) -> 'ok' when
	This::wxFontData(), Min::integer(), Max::integer().
setRange(#wx_ref{type=ThisT}=This,Min,Max)
 when is_integer(Min),is_integer(Max) ->
  ?CLASS(ThisT,wxFontData),
  wxe_util:queue_cmd(This,Min,Max,?get_env(),?wxFontData_SetRange).

-doc """
Determines whether the Help button will be displayed in the font dialog (Windows only).

The default value is false.
""".
-spec setShowHelp(This, ShowHelp) -> 'ok' when
	This::wxFontData(), ShowHelp::boolean().
setShowHelp(#wx_ref{type=ThisT}=This,ShowHelp)
 when is_boolean(ShowHelp) ->
  ?CLASS(ThisT,wxFontData),
  wxe_util:queue_cmd(This,ShowHelp,?get_env(),?wxFontData_SetShowHelp).

-doc "Destroys the object".
-spec destroy(This::wxFontData()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxFontData),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
