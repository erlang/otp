%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
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
%% %CopyrightEnd%
%% This file is generated DO NOT EDIT

-module(wxPageSetupDialogData).
-moduledoc """
This class holds a variety of information related to `m:wxPageSetupDialog`.

It contains a `m:wxPrintData` member which is used to hold basic printer configuration
data (as opposed to the user-interface configuration settings stored by `m:wxPageSetupDialogData`).

See:
* [Overview printing](https://docs.wxwidgets.org/3.2/overview_printing.html#overview_printing)

* `m:wxPageSetupDialog`

wxWidgets docs: [wxPageSetupDialogData](https://docs.wxwidgets.org/3.2/classwx_page_setup_dialog_data.html)
""".
-include("wxe.hrl").
-export([destroy/1,enableHelp/2,enableMargins/2,enableOrientation/2,enablePaper/2,
  enablePrinter/2,getDefaultInfo/1,getDefaultMinMargins/1,getEnableHelp/1,
  getEnableMargins/1,getEnableOrientation/1,getEnablePaper/1,getEnablePrinter/1,
  getMarginBottomRight/1,getMarginTopLeft/1,getMinMarginBottomRight/1,
  getMinMarginTopLeft/1,getPaperId/1,getPaperSize/1,getPrintData/1,
  isOk/1,new/0,new/1,setDefaultInfo/2,setDefaultMinMargins/2,setMarginBottomRight/2,
  setMarginTopLeft/2,setMinMarginBottomRight/2,setMinMarginTopLeft/2,
  setPaperId/2,setPaperSize/2,setPrintData/2]).

%% inherited exports
-export([parent_class/1]).

-type wxPageSetupDialogData() :: wx:wx_object().
-export_type([wxPageSetupDialogData/0]).
-doc false.
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc "Default constructor.".
-spec new() -> wxPageSetupDialogData().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxPageSetupDialogData_new_0),
  wxe_util:rec(?wxPageSetupDialogData_new_0).

-doc "Construct an object from a print data object.".
-spec new(PrintData) -> wxPageSetupDialogData() when
	PrintData::wxPrintData:wxPrintData() | wxPageSetupDialogData:wxPageSetupDialogData().
new(#wx_ref{type=PrintDataT}=PrintData) ->
  IswxPrintData = ?CLASS_T(PrintDataT,wxPrintData),
  IswxPageSetupDialogData = ?CLASS_T(PrintDataT,wxPageSetupDialogData),
  PrintDataType = if
    IswxPrintData ->   wxPrintData;
    IswxPageSetupDialogData ->   wxPageSetupDialogData;
    true -> error({badarg, PrintDataT})
  end,
  wxe_util:queue_cmd(wx:typeCast(PrintData, PrintDataType),?get_env(),?wxPageSetupDialogData_new_1),
  wxe_util:rec(?wxPageSetupDialogData_new_1).

-doc """
Enables or disables the "Help" button (Windows only).
""".
-spec enableHelp(This, Flag) -> 'ok' when
	This::wxPageSetupDialogData(), Flag::boolean().
enableHelp(#wx_ref{type=ThisT}=This,Flag)
 when is_boolean(Flag) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:queue_cmd(This,Flag,?get_env(),?wxPageSetupDialogData_EnableHelp).

-doc "Enables or disables the margin controls (Windows only).".
-spec enableMargins(This, Flag) -> 'ok' when
	This::wxPageSetupDialogData(), Flag::boolean().
enableMargins(#wx_ref{type=ThisT}=This,Flag)
 when is_boolean(Flag) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:queue_cmd(This,Flag,?get_env(),?wxPageSetupDialogData_EnableMargins).

-doc "Enables or disables the orientation control (Windows only).".
-spec enableOrientation(This, Flag) -> 'ok' when
	This::wxPageSetupDialogData(), Flag::boolean().
enableOrientation(#wx_ref{type=ThisT}=This,Flag)
 when is_boolean(Flag) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:queue_cmd(This,Flag,?get_env(),?wxPageSetupDialogData_EnableOrientation).

-doc "Enables or disables the paper size control (Windows only).".
-spec enablePaper(This, Flag) -> 'ok' when
	This::wxPageSetupDialogData(), Flag::boolean().
enablePaper(#wx_ref{type=ThisT}=This,Flag)
 when is_boolean(Flag) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:queue_cmd(This,Flag,?get_env(),?wxPageSetupDialogData_EnablePaper).

-doc """
Enables or disables the "Printer" button, which invokes a printer setup dialog.
""".
-spec enablePrinter(This, Flag) -> 'ok' when
	This::wxPageSetupDialogData(), Flag::boolean().
enablePrinter(#wx_ref{type=ThisT}=This,Flag)
 when is_boolean(Flag) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:queue_cmd(This,Flag,?get_env(),?wxPageSetupDialogData_EnablePrinter).

-doc """
Returns true if the page setup dialog will take its minimum margin values from the
currently selected printer properties (Windows only).
""".
-spec getDefaultMinMargins(This) -> boolean() when
	This::wxPageSetupDialogData().
getDefaultMinMargins(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:queue_cmd(This,?get_env(),?wxPageSetupDialogData_GetDefaultMinMargins),
  wxe_util:rec(?wxPageSetupDialogData_GetDefaultMinMargins).

-doc "Returns true if the margin controls are enabled (Windows only).".
-spec getEnableMargins(This) -> boolean() when
	This::wxPageSetupDialogData().
getEnableMargins(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:queue_cmd(This,?get_env(),?wxPageSetupDialogData_GetEnableMargins),
  wxe_util:rec(?wxPageSetupDialogData_GetEnableMargins).

-doc "Returns true if the orientation control is enabled (Windows only).".
-spec getEnableOrientation(This) -> boolean() when
	This::wxPageSetupDialogData().
getEnableOrientation(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:queue_cmd(This,?get_env(),?wxPageSetupDialogData_GetEnableOrientation),
  wxe_util:rec(?wxPageSetupDialogData_GetEnableOrientation).

-doc "Returns true if the paper size control is enabled (Windows only).".
-spec getEnablePaper(This) -> boolean() when
	This::wxPageSetupDialogData().
getEnablePaper(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:queue_cmd(This,?get_env(),?wxPageSetupDialogData_GetEnablePaper),
  wxe_util:rec(?wxPageSetupDialogData_GetEnablePaper).

-doc "Returns true if the printer setup button is enabled.".
-spec getEnablePrinter(This) -> boolean() when
	This::wxPageSetupDialogData().
getEnablePrinter(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:queue_cmd(This,?get_env(),?wxPageSetupDialogData_GetEnablePrinter),
  wxe_util:rec(?wxPageSetupDialogData_GetEnablePrinter).

-doc "Returns true if the printer setup button is enabled.".
-spec getEnableHelp(This) -> boolean() when
	This::wxPageSetupDialogData().
getEnableHelp(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:queue_cmd(This,?get_env(),?wxPageSetupDialogData_GetEnableHelp),
  wxe_util:rec(?wxPageSetupDialogData_GetEnableHelp).

-doc """
Returns true if the dialog will simply return default printer information (such as
orientation) instead of showing a dialog (Windows only).
""".
-spec getDefaultInfo(This) -> boolean() when
	This::wxPageSetupDialogData().
getDefaultInfo(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:queue_cmd(This,?get_env(),?wxPageSetupDialogData_GetDefaultInfo),
  wxe_util:rec(?wxPageSetupDialogData_GetDefaultInfo).

-doc "Returns the left (x) and top (y) margins in millimetres.".
-spec getMarginTopLeft(This) -> {X::integer(), Y::integer()} when
	This::wxPageSetupDialogData().
getMarginTopLeft(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:queue_cmd(This,?get_env(),?wxPageSetupDialogData_GetMarginTopLeft),
  wxe_util:rec(?wxPageSetupDialogData_GetMarginTopLeft).

-doc "Returns the right (x) and bottom (y) margins in millimetres.".
-spec getMarginBottomRight(This) -> {X::integer(), Y::integer()} when
	This::wxPageSetupDialogData().
getMarginBottomRight(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:queue_cmd(This,?get_env(),?wxPageSetupDialogData_GetMarginBottomRight),
  wxe_util:rec(?wxPageSetupDialogData_GetMarginBottomRight).

-doc """
Returns the left (x) and top (y) minimum margins the user can enter (Windows only).

Units are in millimetres.
""".
-spec getMinMarginTopLeft(This) -> {X::integer(), Y::integer()} when
	This::wxPageSetupDialogData().
getMinMarginTopLeft(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:queue_cmd(This,?get_env(),?wxPageSetupDialogData_GetMinMarginTopLeft),
  wxe_util:rec(?wxPageSetupDialogData_GetMinMarginTopLeft).

-doc """
Returns the right (x) and bottom (y) minimum margins the user can enter (Windows only).

Units are in millimetres.
""".
-spec getMinMarginBottomRight(This) -> {X::integer(), Y::integer()} when
	This::wxPageSetupDialogData().
getMinMarginBottomRight(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:queue_cmd(This,?get_env(),?wxPageSetupDialogData_GetMinMarginBottomRight),
  wxe_util:rec(?wxPageSetupDialogData_GetMinMarginBottomRight).

-doc """
Returns the paper id (stored in the internal `m:wxPrintData` object).

See: `wxPrintData:setPaperId/2`
""".
%%  Res = ?wxPAPER_NONE | ?wxPAPER_LETTER | ?wxPAPER_LEGAL | ?wxPAPER_A4 | ?wxPAPER_CSHEET | ?wxPAPER_DSHEET | ?wxPAPER_ESHEET | ?wxPAPER_LETTERSMALL | ?wxPAPER_TABLOID | ?wxPAPER_LEDGER | ?wxPAPER_STATEMENT | ?wxPAPER_EXECUTIVE | ?wxPAPER_A3 | ?wxPAPER_A4SMALL | ?wxPAPER_A5 | ?wxPAPER_B4 | ?wxPAPER_B5 | ?wxPAPER_FOLIO | ?wxPAPER_QUARTO | ?wxPAPER_10X14 | ?wxPAPER_11X17 | ?wxPAPER_NOTE | ?wxPAPER_ENV_9 | ?wxPAPER_ENV_10 | ?wxPAPER_ENV_11 | ?wxPAPER_ENV_12 | ?wxPAPER_ENV_14 | ?wxPAPER_ENV_DL | ?wxPAPER_ENV_C5 | ?wxPAPER_ENV_C3 | ?wxPAPER_ENV_C4 | ?wxPAPER_ENV_C6 | ?wxPAPER_ENV_C65 | ?wxPAPER_ENV_B4 | ?wxPAPER_ENV_B5 | ?wxPAPER_ENV_B6 | ?wxPAPER_ENV_ITALY | ?wxPAPER_ENV_MONARCH | ?wxPAPER_ENV_PERSONAL | ?wxPAPER_FANFOLD_US | ?wxPAPER_FANFOLD_STD_GERMAN | ?wxPAPER_FANFOLD_LGL_GERMAN | ?wxPAPER_ISO_B4 | ?wxPAPER_JAPANESE_POSTCARD | ?wxPAPER_9X11 | ?wxPAPER_10X11 | ?wxPAPER_15X11 | ?wxPAPER_ENV_INVITE | ?wxPAPER_LETTER_EXTRA | ?wxPAPER_LEGAL_EXTRA | ?wxPAPER_TABLOID_EXTRA | ?wxPAPER_A4_EXTRA | ?wxPAPER_LETTER_TRANSVERSE | ?wxPAPER_A4_TRANSVERSE | ?wxPAPER_LETTER_EXTRA_TRANSVERSE | ?wxPAPER_A_PLUS | ?wxPAPER_B_PLUS | ?wxPAPER_LETTER_PLUS | ?wxPAPER_A4_PLUS | ?wxPAPER_A5_TRANSVERSE | ?wxPAPER_B5_TRANSVERSE | ?wxPAPER_A3_EXTRA | ?wxPAPER_A5_EXTRA | ?wxPAPER_B5_EXTRA | ?wxPAPER_A2 | ?wxPAPER_A3_TRANSVERSE | ?wxPAPER_A3_EXTRA_TRANSVERSE | ?wxPAPER_DBL_JAPANESE_POSTCARD | ?wxPAPER_A6 | ?wxPAPER_JENV_KAKU2 | ?wxPAPER_JENV_KAKU3 | ?wxPAPER_JENV_CHOU3 | ?wxPAPER_JENV_CHOU4 | ?wxPAPER_LETTER_ROTATED | ?wxPAPER_A3_ROTATED | ?wxPAPER_A4_ROTATED | ?wxPAPER_A5_ROTATED | ?wxPAPER_B4_JIS_ROTATED | ?wxPAPER_B5_JIS_ROTATED | ?wxPAPER_JAPANESE_POSTCARD_ROTATED | ?wxPAPER_DBL_JAPANESE_POSTCARD_ROTATED | ?wxPAPER_A6_ROTATED | ?wxPAPER_JENV_KAKU2_ROTATED | ?wxPAPER_JENV_KAKU3_ROTATED | ?wxPAPER_JENV_CHOU3_ROTATED | ?wxPAPER_JENV_CHOU4_ROTATED | ?wxPAPER_B6_JIS | ?wxPAPER_B6_JIS_ROTATED | ?wxPAPER_12X11 | ?wxPAPER_JENV_YOU4 | ?wxPAPER_JENV_YOU4_ROTATED | ?wxPAPER_P16K | ?wxPAPER_P32K | ?wxPAPER_P32KBIG | ?wxPAPER_PENV_1 | ?wxPAPER_PENV_2 | ?wxPAPER_PENV_3 | ?wxPAPER_PENV_4 | ?wxPAPER_PENV_5 | ?wxPAPER_PENV_6 | ?wxPAPER_PENV_7 | ?wxPAPER_PENV_8 | ?wxPAPER_PENV_9 | ?wxPAPER_PENV_10 | ?wxPAPER_P16K_ROTATED | ?wxPAPER_P32K_ROTATED | ?wxPAPER_P32KBIG_ROTATED | ?wxPAPER_PENV_1_ROTATED | ?wxPAPER_PENV_2_ROTATED | ?wxPAPER_PENV_3_ROTATED | ?wxPAPER_PENV_4_ROTATED | ?wxPAPER_PENV_5_ROTATED | ?wxPAPER_PENV_6_ROTATED | ?wxPAPER_PENV_7_ROTATED | ?wxPAPER_PENV_8_ROTATED | ?wxPAPER_PENV_9_ROTATED | ?wxPAPER_PENV_10_ROTATED | ?wxPAPER_A0 | ?wxPAPER_A1
-spec getPaperId(This) -> wx:wx_enum() when
	This::wxPageSetupDialogData().
getPaperId(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:queue_cmd(This,?get_env(),?wxPageSetupDialogData_GetPaperId),
  wxe_util:rec(?wxPageSetupDialogData_GetPaperId).

-doc "Returns the paper size in millimetres.".
-spec getPaperSize(This) -> {W::integer(), H::integer()} when
	This::wxPageSetupDialogData().
getPaperSize(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:queue_cmd(This,?get_env(),?wxPageSetupDialogData_GetPaperSize),
  wxe_util:rec(?wxPageSetupDialogData_GetPaperSize).

-doc "".
-spec getPrintData(This) -> wxPrintData:wxPrintData() when
	This::wxPageSetupDialogData().
getPrintData(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:queue_cmd(This,?get_env(),?wxPageSetupDialogData_GetPrintData),
  wxe_util:rec(?wxPageSetupDialogData_GetPrintData).

-doc """
Returns true if the print data associated with the dialog data is valid.

This can return false on Windows if the current printer is not set, for example. On all
other platforms, it returns true.
""".
-spec isOk(This) -> boolean() when
	This::wxPageSetupDialogData().
isOk(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:queue_cmd(This,?get_env(),?wxPageSetupDialogData_IsOk),
  wxe_util:rec(?wxPageSetupDialogData_IsOk).

-doc """
Pass true if the dialog will simply return default printer information (such as
orientation) instead of showing a dialog (Windows only).
""".
-spec setDefaultInfo(This, Flag) -> 'ok' when
	This::wxPageSetupDialogData(), Flag::boolean().
setDefaultInfo(#wx_ref{type=ThisT}=This,Flag)
 when is_boolean(Flag) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:queue_cmd(This,Flag,?get_env(),?wxPageSetupDialogData_SetDefaultInfo).

-doc """
Pass true if the page setup dialog will take its minimum margin values from the currently
selected printer properties (Windows only).

Units are in millimetres.
""".
-spec setDefaultMinMargins(This, Flag) -> 'ok' when
	This::wxPageSetupDialogData(), Flag::boolean().
setDefaultMinMargins(#wx_ref{type=ThisT}=This,Flag)
 when is_boolean(Flag) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:queue_cmd(This,Flag,?get_env(),?wxPageSetupDialogData_SetDefaultMinMargins).

-doc "Sets the left (x) and top (y) margins in millimetres.".
-spec setMarginTopLeft(This, Pt) -> 'ok' when
	This::wxPageSetupDialogData(), Pt::{X::integer(), Y::integer()}.
setMarginTopLeft(#wx_ref{type=ThisT}=This,{PtX,PtY} = Pt)
 when is_integer(PtX),is_integer(PtY) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:queue_cmd(This,Pt,?get_env(),?wxPageSetupDialogData_SetMarginTopLeft).

-doc "Sets the right (x) and bottom (y) margins in millimetres.".
-spec setMarginBottomRight(This, Pt) -> 'ok' when
	This::wxPageSetupDialogData(), Pt::{X::integer(), Y::integer()}.
setMarginBottomRight(#wx_ref{type=ThisT}=This,{PtX,PtY} = Pt)
 when is_integer(PtX),is_integer(PtY) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:queue_cmd(This,Pt,?get_env(),?wxPageSetupDialogData_SetMarginBottomRight).

-doc """
Sets the left (x) and top (y) minimum margins the user can enter (Windows only).

Units are in millimetres.
""".
-spec setMinMarginTopLeft(This, Pt) -> 'ok' when
	This::wxPageSetupDialogData(), Pt::{X::integer(), Y::integer()}.
setMinMarginTopLeft(#wx_ref{type=ThisT}=This,{PtX,PtY} = Pt)
 when is_integer(PtX),is_integer(PtY) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:queue_cmd(This,Pt,?get_env(),?wxPageSetupDialogData_SetMinMarginTopLeft).

-doc """
Sets the right (x) and bottom (y) minimum margins the user can enter (Windows only).

Units are in millimetres.
""".
-spec setMinMarginBottomRight(This, Pt) -> 'ok' when
	This::wxPageSetupDialogData(), Pt::{X::integer(), Y::integer()}.
setMinMarginBottomRight(#wx_ref{type=ThisT}=This,{PtX,PtY} = Pt)
 when is_integer(PtX),is_integer(PtY) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:queue_cmd(This,Pt,?get_env(),?wxPageSetupDialogData_SetMinMarginBottomRight).

-doc """
Sets the paper size id.

Calling this function overrides the explicit paper dimensions passed in `setPaperSize/2`.

See: `wxPrintData:setPaperId/2`
""".
%%  Id = ?wxPAPER_NONE | ?wxPAPER_LETTER | ?wxPAPER_LEGAL | ?wxPAPER_A4 | ?wxPAPER_CSHEET | ?wxPAPER_DSHEET | ?wxPAPER_ESHEET | ?wxPAPER_LETTERSMALL | ?wxPAPER_TABLOID | ?wxPAPER_LEDGER | ?wxPAPER_STATEMENT | ?wxPAPER_EXECUTIVE | ?wxPAPER_A3 | ?wxPAPER_A4SMALL | ?wxPAPER_A5 | ?wxPAPER_B4 | ?wxPAPER_B5 | ?wxPAPER_FOLIO | ?wxPAPER_QUARTO | ?wxPAPER_10X14 | ?wxPAPER_11X17 | ?wxPAPER_NOTE | ?wxPAPER_ENV_9 | ?wxPAPER_ENV_10 | ?wxPAPER_ENV_11 | ?wxPAPER_ENV_12 | ?wxPAPER_ENV_14 | ?wxPAPER_ENV_DL | ?wxPAPER_ENV_C5 | ?wxPAPER_ENV_C3 | ?wxPAPER_ENV_C4 | ?wxPAPER_ENV_C6 | ?wxPAPER_ENV_C65 | ?wxPAPER_ENV_B4 | ?wxPAPER_ENV_B5 | ?wxPAPER_ENV_B6 | ?wxPAPER_ENV_ITALY | ?wxPAPER_ENV_MONARCH | ?wxPAPER_ENV_PERSONAL | ?wxPAPER_FANFOLD_US | ?wxPAPER_FANFOLD_STD_GERMAN | ?wxPAPER_FANFOLD_LGL_GERMAN | ?wxPAPER_ISO_B4 | ?wxPAPER_JAPANESE_POSTCARD | ?wxPAPER_9X11 | ?wxPAPER_10X11 | ?wxPAPER_15X11 | ?wxPAPER_ENV_INVITE | ?wxPAPER_LETTER_EXTRA | ?wxPAPER_LEGAL_EXTRA | ?wxPAPER_TABLOID_EXTRA | ?wxPAPER_A4_EXTRA | ?wxPAPER_LETTER_TRANSVERSE | ?wxPAPER_A4_TRANSVERSE | ?wxPAPER_LETTER_EXTRA_TRANSVERSE | ?wxPAPER_A_PLUS | ?wxPAPER_B_PLUS | ?wxPAPER_LETTER_PLUS | ?wxPAPER_A4_PLUS | ?wxPAPER_A5_TRANSVERSE | ?wxPAPER_B5_TRANSVERSE | ?wxPAPER_A3_EXTRA | ?wxPAPER_A5_EXTRA | ?wxPAPER_B5_EXTRA | ?wxPAPER_A2 | ?wxPAPER_A3_TRANSVERSE | ?wxPAPER_A3_EXTRA_TRANSVERSE | ?wxPAPER_DBL_JAPANESE_POSTCARD | ?wxPAPER_A6 | ?wxPAPER_JENV_KAKU2 | ?wxPAPER_JENV_KAKU3 | ?wxPAPER_JENV_CHOU3 | ?wxPAPER_JENV_CHOU4 | ?wxPAPER_LETTER_ROTATED | ?wxPAPER_A3_ROTATED | ?wxPAPER_A4_ROTATED | ?wxPAPER_A5_ROTATED | ?wxPAPER_B4_JIS_ROTATED | ?wxPAPER_B5_JIS_ROTATED | ?wxPAPER_JAPANESE_POSTCARD_ROTATED | ?wxPAPER_DBL_JAPANESE_POSTCARD_ROTATED | ?wxPAPER_A6_ROTATED | ?wxPAPER_JENV_KAKU2_ROTATED | ?wxPAPER_JENV_KAKU3_ROTATED | ?wxPAPER_JENV_CHOU3_ROTATED | ?wxPAPER_JENV_CHOU4_ROTATED | ?wxPAPER_B6_JIS | ?wxPAPER_B6_JIS_ROTATED | ?wxPAPER_12X11 | ?wxPAPER_JENV_YOU4 | ?wxPAPER_JENV_YOU4_ROTATED | ?wxPAPER_P16K | ?wxPAPER_P32K | ?wxPAPER_P32KBIG | ?wxPAPER_PENV_1 | ?wxPAPER_PENV_2 | ?wxPAPER_PENV_3 | ?wxPAPER_PENV_4 | ?wxPAPER_PENV_5 | ?wxPAPER_PENV_6 | ?wxPAPER_PENV_7 | ?wxPAPER_PENV_8 | ?wxPAPER_PENV_9 | ?wxPAPER_PENV_10 | ?wxPAPER_P16K_ROTATED | ?wxPAPER_P32K_ROTATED | ?wxPAPER_P32KBIG_ROTATED | ?wxPAPER_PENV_1_ROTATED | ?wxPAPER_PENV_2_ROTATED | ?wxPAPER_PENV_3_ROTATED | ?wxPAPER_PENV_4_ROTATED | ?wxPAPER_PENV_5_ROTATED | ?wxPAPER_PENV_6_ROTATED | ?wxPAPER_PENV_7_ROTATED | ?wxPAPER_PENV_8_ROTATED | ?wxPAPER_PENV_9_ROTATED | ?wxPAPER_PENV_10_ROTATED | ?wxPAPER_A0 | ?wxPAPER_A1
-spec setPaperId(This, Id) -> 'ok' when
	This::wxPageSetupDialogData(), Id::wx:wx_enum().
setPaperId(#wx_ref{type=ThisT}=This,Id)
 when is_integer(Id) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:queue_cmd(This,Id,?get_env(),?wxPageSetupDialogData_SetPaperId).

-doc """
Sets the paper size in millimetres.

If a corresponding paper id is found, it will be set in the internal `m:wxPrintData`
object, otherwise the paper size overrides the paper id.
""".
-spec setPaperSize(This, Size) -> 'ok' when
	This::wxPageSetupDialogData(), Size::{W::integer(), H::integer()}.
setPaperSize(#wx_ref{type=ThisT}=This,{SizeW,SizeH} = Size)
 when is_integer(SizeW),is_integer(SizeH) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:queue_cmd(This,Size,?get_env(),?wxPageSetupDialogData_SetPaperSize).

-doc "Sets the print data associated with this object.".
-spec setPrintData(This, PrintData) -> 'ok' when
	This::wxPageSetupDialogData(), PrintData::wxPrintData:wxPrintData().
setPrintData(#wx_ref{type=ThisT}=This,#wx_ref{type=PrintDataT}=PrintData) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  ?CLASS(PrintDataT,wxPrintData),
  wxe_util:queue_cmd(This,PrintData,?get_env(),?wxPageSetupDialogData_SetPrintData).

-doc "Destroys the object".
-spec destroy(This::wxPageSetupDialogData()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxPageSetupDialogData),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
