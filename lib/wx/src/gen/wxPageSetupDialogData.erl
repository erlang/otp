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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpagesetupdialogdata.html">wxPageSetupDialogData</a>.
%% @type wxPageSetupDialogData().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxPageSetupDialogData).
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

-export_type([wxPageSetupDialogData/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxPageSetupDialogData() :: wx:wx_object().
%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpagesetupdialogdata.html#wxpagesetupdialogdatawxpagesetupdialogdata">external documentation</a>.
-spec new() -> wxPageSetupDialogData().
new() ->
  wxe_util:construct(?wxPageSetupDialogData_new_0,
  <<>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpagesetupdialogdata.html#wxpagesetupdialogdatawxpagesetupdialogdata">external documentation</a>.
-spec new(PrintData) -> wxPageSetupDialogData() when
	PrintData::wxPrintData:wxPrintData() | wxPageSetupDialogData().
new(#wx_ref{type=PrintDataT,ref=PrintDataRef}) ->
  PrintDataOP = case ?CLASS_T(PrintDataT,wxPrintData) of
     true ->
       ?wxPageSetupDialogData_new_1_1;
     _ -> ?CLASS(PrintDataT,wxPageSetupDialogData),
       ?wxPageSetupDialogData_new_1_0
     end,
  wxe_util:construct(PrintDataOP,
  <<PrintDataRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpagesetupdialogdata.html#wxpagesetupdialogdataenablehelp">external documentation</a>.
-spec enableHelp(This, Flag) -> 'ok' when
	This::wxPageSetupDialogData(), Flag::boolean().
enableHelp(#wx_ref{type=ThisT,ref=ThisRef},Flag)
 when is_boolean(Flag) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:cast(?wxPageSetupDialogData_EnableHelp,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Flag)):32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpagesetupdialogdata.html#wxpagesetupdialogdataenablemargins">external documentation</a>.
-spec enableMargins(This, Flag) -> 'ok' when
	This::wxPageSetupDialogData(), Flag::boolean().
enableMargins(#wx_ref{type=ThisT,ref=ThisRef},Flag)
 when is_boolean(Flag) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:cast(?wxPageSetupDialogData_EnableMargins,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Flag)):32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpagesetupdialogdata.html#wxpagesetupdialogdataenableorientation">external documentation</a>.
-spec enableOrientation(This, Flag) -> 'ok' when
	This::wxPageSetupDialogData(), Flag::boolean().
enableOrientation(#wx_ref{type=ThisT,ref=ThisRef},Flag)
 when is_boolean(Flag) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:cast(?wxPageSetupDialogData_EnableOrientation,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Flag)):32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpagesetupdialogdata.html#wxpagesetupdialogdataenablepaper">external documentation</a>.
-spec enablePaper(This, Flag) -> 'ok' when
	This::wxPageSetupDialogData(), Flag::boolean().
enablePaper(#wx_ref{type=ThisT,ref=ThisRef},Flag)
 when is_boolean(Flag) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:cast(?wxPageSetupDialogData_EnablePaper,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Flag)):32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpagesetupdialogdata.html#wxpagesetupdialogdataenableprinter">external documentation</a>.
-spec enablePrinter(This, Flag) -> 'ok' when
	This::wxPageSetupDialogData(), Flag::boolean().
enablePrinter(#wx_ref{type=ThisT,ref=ThisRef},Flag)
 when is_boolean(Flag) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:cast(?wxPageSetupDialogData_EnablePrinter,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Flag)):32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpagesetupdialogdata.html#wxpagesetupdialogdatagetdefaultminmargins">external documentation</a>.
-spec getDefaultMinMargins(This) -> boolean() when
	This::wxPageSetupDialogData().
getDefaultMinMargins(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:call(?wxPageSetupDialogData_GetDefaultMinMargins,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpagesetupdialogdata.html#wxpagesetupdialogdatagetenablemargins">external documentation</a>.
-spec getEnableMargins(This) -> boolean() when
	This::wxPageSetupDialogData().
getEnableMargins(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:call(?wxPageSetupDialogData_GetEnableMargins,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpagesetupdialogdata.html#wxpagesetupdialogdatagetenableorientation">external documentation</a>.
-spec getEnableOrientation(This) -> boolean() when
	This::wxPageSetupDialogData().
getEnableOrientation(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:call(?wxPageSetupDialogData_GetEnableOrientation,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpagesetupdialogdata.html#wxpagesetupdialogdatagetenablepaper">external documentation</a>.
-spec getEnablePaper(This) -> boolean() when
	This::wxPageSetupDialogData().
getEnablePaper(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:call(?wxPageSetupDialogData_GetEnablePaper,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpagesetupdialogdata.html#wxpagesetupdialogdatagetenableprinter">external documentation</a>.
-spec getEnablePrinter(This) -> boolean() when
	This::wxPageSetupDialogData().
getEnablePrinter(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:call(?wxPageSetupDialogData_GetEnablePrinter,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpagesetupdialogdata.html#wxpagesetupdialogdatagetenablehelp">external documentation</a>.
-spec getEnableHelp(This) -> boolean() when
	This::wxPageSetupDialogData().
getEnableHelp(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:call(?wxPageSetupDialogData_GetEnableHelp,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpagesetupdialogdata.html#wxpagesetupdialogdatagetdefaultinfo">external documentation</a>.
-spec getDefaultInfo(This) -> boolean() when
	This::wxPageSetupDialogData().
getDefaultInfo(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:call(?wxPageSetupDialogData_GetDefaultInfo,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpagesetupdialogdata.html#wxpagesetupdialogdatagetmargintopleft">external documentation</a>.
-spec getMarginTopLeft(This) -> {X::integer(), Y::integer()} when
	This::wxPageSetupDialogData().
getMarginTopLeft(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:call(?wxPageSetupDialogData_GetMarginTopLeft,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpagesetupdialogdata.html#wxpagesetupdialogdatagetmarginbottomright">external documentation</a>.
-spec getMarginBottomRight(This) -> {X::integer(), Y::integer()} when
	This::wxPageSetupDialogData().
getMarginBottomRight(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:call(?wxPageSetupDialogData_GetMarginBottomRight,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpagesetupdialogdata.html#wxpagesetupdialogdatagetminmargintopleft">external documentation</a>.
-spec getMinMarginTopLeft(This) -> {X::integer(), Y::integer()} when
	This::wxPageSetupDialogData().
getMinMarginTopLeft(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:call(?wxPageSetupDialogData_GetMinMarginTopLeft,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpagesetupdialogdata.html#wxpagesetupdialogdatagetminmarginbottomright">external documentation</a>.
-spec getMinMarginBottomRight(This) -> {X::integer(), Y::integer()} when
	This::wxPageSetupDialogData().
getMinMarginBottomRight(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:call(?wxPageSetupDialogData_GetMinMarginBottomRight,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpagesetupdialogdata.html#wxpagesetupdialogdatagetpaperid">external documentation</a>.
%%<br /> Res = ?wxPAPER_NONE | ?wxPAPER_LETTER | ?wxPAPER_LEGAL | ?wxPAPER_A4 | ?wxPAPER_CSHEET | ?wxPAPER_DSHEET | ?wxPAPER_ESHEET | ?wxPAPER_LETTERSMALL | ?wxPAPER_TABLOID | ?wxPAPER_LEDGER | ?wxPAPER_STATEMENT | ?wxPAPER_EXECUTIVE | ?wxPAPER_A3 | ?wxPAPER_A4SMALL | ?wxPAPER_A5 | ?wxPAPER_B4 | ?wxPAPER_B5 | ?wxPAPER_FOLIO | ?wxPAPER_QUARTO | ?wxPAPER_10X14 | ?wxPAPER_11X17 | ?wxPAPER_NOTE | ?wxPAPER_ENV_9 | ?wxPAPER_ENV_10 | ?wxPAPER_ENV_11 | ?wxPAPER_ENV_12 | ?wxPAPER_ENV_14 | ?wxPAPER_ENV_DL | ?wxPAPER_ENV_C5 | ?wxPAPER_ENV_C3 | ?wxPAPER_ENV_C4 | ?wxPAPER_ENV_C6 | ?wxPAPER_ENV_C65 | ?wxPAPER_ENV_B4 | ?wxPAPER_ENV_B5 | ?wxPAPER_ENV_B6 | ?wxPAPER_ENV_ITALY | ?wxPAPER_ENV_MONARCH | ?wxPAPER_ENV_PERSONAL | ?wxPAPER_FANFOLD_US | ?wxPAPER_FANFOLD_STD_GERMAN | ?wxPAPER_FANFOLD_LGL_GERMAN | ?wxPAPER_ISO_B4 | ?wxPAPER_JAPANESE_POSTCARD | ?wxPAPER_9X11 | ?wxPAPER_10X11 | ?wxPAPER_15X11 | ?wxPAPER_ENV_INVITE | ?wxPAPER_LETTER_EXTRA | ?wxPAPER_LEGAL_EXTRA | ?wxPAPER_TABLOID_EXTRA | ?wxPAPER_A4_EXTRA | ?wxPAPER_LETTER_TRANSVERSE | ?wxPAPER_A4_TRANSVERSE | ?wxPAPER_LETTER_EXTRA_TRANSVERSE | ?wxPAPER_A_PLUS | ?wxPAPER_B_PLUS | ?wxPAPER_LETTER_PLUS | ?wxPAPER_A4_PLUS | ?wxPAPER_A5_TRANSVERSE | ?wxPAPER_B5_TRANSVERSE | ?wxPAPER_A3_EXTRA | ?wxPAPER_A5_EXTRA | ?wxPAPER_B5_EXTRA | ?wxPAPER_A2 | ?wxPAPER_A3_TRANSVERSE | ?wxPAPER_A3_EXTRA_TRANSVERSE | ?wxPAPER_DBL_JAPANESE_POSTCARD | ?wxPAPER_A6 | ?wxPAPER_JENV_KAKU2 | ?wxPAPER_JENV_KAKU3 | ?wxPAPER_JENV_CHOU3 | ?wxPAPER_JENV_CHOU4 | ?wxPAPER_LETTER_ROTATED | ?wxPAPER_A3_ROTATED | ?wxPAPER_A4_ROTATED | ?wxPAPER_A5_ROTATED | ?wxPAPER_B4_JIS_ROTATED | ?wxPAPER_B5_JIS_ROTATED | ?wxPAPER_JAPANESE_POSTCARD_ROTATED | ?wxPAPER_DBL_JAPANESE_POSTCARD_ROTATED | ?wxPAPER_A6_ROTATED | ?wxPAPER_JENV_KAKU2_ROTATED | ?wxPAPER_JENV_KAKU3_ROTATED | ?wxPAPER_JENV_CHOU3_ROTATED | ?wxPAPER_JENV_CHOU4_ROTATED | ?wxPAPER_B6_JIS | ?wxPAPER_B6_JIS_ROTATED | ?wxPAPER_12X11 | ?wxPAPER_JENV_YOU4 | ?wxPAPER_JENV_YOU4_ROTATED | ?wxPAPER_P16K | ?wxPAPER_P32K | ?wxPAPER_P32KBIG | ?wxPAPER_PENV_1 | ?wxPAPER_PENV_2 | ?wxPAPER_PENV_3 | ?wxPAPER_PENV_4 | ?wxPAPER_PENV_5 | ?wxPAPER_PENV_6 | ?wxPAPER_PENV_7 | ?wxPAPER_PENV_8 | ?wxPAPER_PENV_9 | ?wxPAPER_PENV_10 | ?wxPAPER_P16K_ROTATED | ?wxPAPER_P32K_ROTATED | ?wxPAPER_P32KBIG_ROTATED | ?wxPAPER_PENV_1_ROTATED | ?wxPAPER_PENV_2_ROTATED | ?wxPAPER_PENV_3_ROTATED | ?wxPAPER_PENV_4_ROTATED | ?wxPAPER_PENV_5_ROTATED | ?wxPAPER_PENV_6_ROTATED | ?wxPAPER_PENV_7_ROTATED | ?wxPAPER_PENV_8_ROTATED | ?wxPAPER_PENV_9_ROTATED | ?wxPAPER_PENV_10_ROTATED
-spec getPaperId(This) -> wx:wx_enum() when
	This::wxPageSetupDialogData().
getPaperId(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:call(?wxPageSetupDialogData_GetPaperId,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpagesetupdialogdata.html#wxpagesetupdialogdatagetpapersize">external documentation</a>.
-spec getPaperSize(This) -> {W::integer(), H::integer()} when
	This::wxPageSetupDialogData().
getPaperSize(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:call(?wxPageSetupDialogData_GetPaperSize,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpagesetupdialogdata.html#wxpagesetupdialogdatagetprintdata">external documentation</a>.
-spec getPrintData(This) -> wxPrintData:wxPrintData() when
	This::wxPageSetupDialogData().
getPrintData(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:call(?wxPageSetupDialogData_GetPrintData,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpagesetupdialogdata.html#wxpagesetupdialogdataisok">external documentation</a>.
-spec isOk(This) -> boolean() when
	This::wxPageSetupDialogData().
isOk(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:call(?wxPageSetupDialogData_IsOk,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpagesetupdialogdata.html#wxpagesetupdialogdatasetdefaultinfo">external documentation</a>.
-spec setDefaultInfo(This, Flag) -> 'ok' when
	This::wxPageSetupDialogData(), Flag::boolean().
setDefaultInfo(#wx_ref{type=ThisT,ref=ThisRef},Flag)
 when is_boolean(Flag) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:cast(?wxPageSetupDialogData_SetDefaultInfo,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Flag)):32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpagesetupdialogdata.html#wxpagesetupdialogdatasetdefaultminmargins">external documentation</a>.
-spec setDefaultMinMargins(This, Flag) -> 'ok' when
	This::wxPageSetupDialogData(), Flag::boolean().
setDefaultMinMargins(#wx_ref{type=ThisT,ref=ThisRef},Flag)
 when is_boolean(Flag) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:cast(?wxPageSetupDialogData_SetDefaultMinMargins,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Flag)):32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpagesetupdialogdata.html#wxpagesetupdialogdatasetmargintopleft">external documentation</a>.
-spec setMarginTopLeft(This, Pt) -> 'ok' when
	This::wxPageSetupDialogData(), Pt::{X::integer(), Y::integer()}.
setMarginTopLeft(#wx_ref{type=ThisT,ref=ThisRef},{PtX,PtY})
 when is_integer(PtX),is_integer(PtY) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:cast(?wxPageSetupDialogData_SetMarginTopLeft,
  <<ThisRef:32/?UI,PtX:32/?UI,PtY:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpagesetupdialogdata.html#wxpagesetupdialogdatasetmarginbottomright">external documentation</a>.
-spec setMarginBottomRight(This, Pt) -> 'ok' when
	This::wxPageSetupDialogData(), Pt::{X::integer(), Y::integer()}.
setMarginBottomRight(#wx_ref{type=ThisT,ref=ThisRef},{PtX,PtY})
 when is_integer(PtX),is_integer(PtY) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:cast(?wxPageSetupDialogData_SetMarginBottomRight,
  <<ThisRef:32/?UI,PtX:32/?UI,PtY:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpagesetupdialogdata.html#wxpagesetupdialogdatasetminmargintopleft">external documentation</a>.
-spec setMinMarginTopLeft(This, Pt) -> 'ok' when
	This::wxPageSetupDialogData(), Pt::{X::integer(), Y::integer()}.
setMinMarginTopLeft(#wx_ref{type=ThisT,ref=ThisRef},{PtX,PtY})
 when is_integer(PtX),is_integer(PtY) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:cast(?wxPageSetupDialogData_SetMinMarginTopLeft,
  <<ThisRef:32/?UI,PtX:32/?UI,PtY:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpagesetupdialogdata.html#wxpagesetupdialogdatasetminmarginbottomright">external documentation</a>.
-spec setMinMarginBottomRight(This, Pt) -> 'ok' when
	This::wxPageSetupDialogData(), Pt::{X::integer(), Y::integer()}.
setMinMarginBottomRight(#wx_ref{type=ThisT,ref=ThisRef},{PtX,PtY})
 when is_integer(PtX),is_integer(PtY) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:cast(?wxPageSetupDialogData_SetMinMarginBottomRight,
  <<ThisRef:32/?UI,PtX:32/?UI,PtY:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpagesetupdialogdata.html#wxpagesetupdialogdatasetpaperid">external documentation</a>.
%%<br /> Id = ?wxPAPER_NONE | ?wxPAPER_LETTER | ?wxPAPER_LEGAL | ?wxPAPER_A4 | ?wxPAPER_CSHEET | ?wxPAPER_DSHEET | ?wxPAPER_ESHEET | ?wxPAPER_LETTERSMALL | ?wxPAPER_TABLOID | ?wxPAPER_LEDGER | ?wxPAPER_STATEMENT | ?wxPAPER_EXECUTIVE | ?wxPAPER_A3 | ?wxPAPER_A4SMALL | ?wxPAPER_A5 | ?wxPAPER_B4 | ?wxPAPER_B5 | ?wxPAPER_FOLIO | ?wxPAPER_QUARTO | ?wxPAPER_10X14 | ?wxPAPER_11X17 | ?wxPAPER_NOTE | ?wxPAPER_ENV_9 | ?wxPAPER_ENV_10 | ?wxPAPER_ENV_11 | ?wxPAPER_ENV_12 | ?wxPAPER_ENV_14 | ?wxPAPER_ENV_DL | ?wxPAPER_ENV_C5 | ?wxPAPER_ENV_C3 | ?wxPAPER_ENV_C4 | ?wxPAPER_ENV_C6 | ?wxPAPER_ENV_C65 | ?wxPAPER_ENV_B4 | ?wxPAPER_ENV_B5 | ?wxPAPER_ENV_B6 | ?wxPAPER_ENV_ITALY | ?wxPAPER_ENV_MONARCH | ?wxPAPER_ENV_PERSONAL | ?wxPAPER_FANFOLD_US | ?wxPAPER_FANFOLD_STD_GERMAN | ?wxPAPER_FANFOLD_LGL_GERMAN | ?wxPAPER_ISO_B4 | ?wxPAPER_JAPANESE_POSTCARD | ?wxPAPER_9X11 | ?wxPAPER_10X11 | ?wxPAPER_15X11 | ?wxPAPER_ENV_INVITE | ?wxPAPER_LETTER_EXTRA | ?wxPAPER_LEGAL_EXTRA | ?wxPAPER_TABLOID_EXTRA | ?wxPAPER_A4_EXTRA | ?wxPAPER_LETTER_TRANSVERSE | ?wxPAPER_A4_TRANSVERSE | ?wxPAPER_LETTER_EXTRA_TRANSVERSE | ?wxPAPER_A_PLUS | ?wxPAPER_B_PLUS | ?wxPAPER_LETTER_PLUS | ?wxPAPER_A4_PLUS | ?wxPAPER_A5_TRANSVERSE | ?wxPAPER_B5_TRANSVERSE | ?wxPAPER_A3_EXTRA | ?wxPAPER_A5_EXTRA | ?wxPAPER_B5_EXTRA | ?wxPAPER_A2 | ?wxPAPER_A3_TRANSVERSE | ?wxPAPER_A3_EXTRA_TRANSVERSE | ?wxPAPER_DBL_JAPANESE_POSTCARD | ?wxPAPER_A6 | ?wxPAPER_JENV_KAKU2 | ?wxPAPER_JENV_KAKU3 | ?wxPAPER_JENV_CHOU3 | ?wxPAPER_JENV_CHOU4 | ?wxPAPER_LETTER_ROTATED | ?wxPAPER_A3_ROTATED | ?wxPAPER_A4_ROTATED | ?wxPAPER_A5_ROTATED | ?wxPAPER_B4_JIS_ROTATED | ?wxPAPER_B5_JIS_ROTATED | ?wxPAPER_JAPANESE_POSTCARD_ROTATED | ?wxPAPER_DBL_JAPANESE_POSTCARD_ROTATED | ?wxPAPER_A6_ROTATED | ?wxPAPER_JENV_KAKU2_ROTATED | ?wxPAPER_JENV_KAKU3_ROTATED | ?wxPAPER_JENV_CHOU3_ROTATED | ?wxPAPER_JENV_CHOU4_ROTATED | ?wxPAPER_B6_JIS | ?wxPAPER_B6_JIS_ROTATED | ?wxPAPER_12X11 | ?wxPAPER_JENV_YOU4 | ?wxPAPER_JENV_YOU4_ROTATED | ?wxPAPER_P16K | ?wxPAPER_P32K | ?wxPAPER_P32KBIG | ?wxPAPER_PENV_1 | ?wxPAPER_PENV_2 | ?wxPAPER_PENV_3 | ?wxPAPER_PENV_4 | ?wxPAPER_PENV_5 | ?wxPAPER_PENV_6 | ?wxPAPER_PENV_7 | ?wxPAPER_PENV_8 | ?wxPAPER_PENV_9 | ?wxPAPER_PENV_10 | ?wxPAPER_P16K_ROTATED | ?wxPAPER_P32K_ROTATED | ?wxPAPER_P32KBIG_ROTATED | ?wxPAPER_PENV_1_ROTATED | ?wxPAPER_PENV_2_ROTATED | ?wxPAPER_PENV_3_ROTATED | ?wxPAPER_PENV_4_ROTATED | ?wxPAPER_PENV_5_ROTATED | ?wxPAPER_PENV_6_ROTATED | ?wxPAPER_PENV_7_ROTATED | ?wxPAPER_PENV_8_ROTATED | ?wxPAPER_PENV_9_ROTATED | ?wxPAPER_PENV_10_ROTATED
-spec setPaperId(This, Id) -> 'ok' when
	This::wxPageSetupDialogData(), Id::wx:wx_enum().
setPaperId(#wx_ref{type=ThisT,ref=ThisRef},Id)
 when is_integer(Id) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:cast(?wxPageSetupDialogData_SetPaperId,
  <<ThisRef:32/?UI,Id:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpagesetupdialogdata.html#wxpagesetupdialogdatasetpapersize">external documentation</a>.
%% <br /> Also:<br />
%% setPaperSize(This, Sz) -> 'ok' when<br />
%% 	This::wxPageSetupDialogData(), Sz::{W::integer(), H::integer()}.<br />
%% 
%%<br /> Id = ?wxPAPER_NONE | ?wxPAPER_LETTER | ?wxPAPER_LEGAL | ?wxPAPER_A4 | ?wxPAPER_CSHEET | ?wxPAPER_DSHEET | ?wxPAPER_ESHEET | ?wxPAPER_LETTERSMALL | ?wxPAPER_TABLOID | ?wxPAPER_LEDGER | ?wxPAPER_STATEMENT | ?wxPAPER_EXECUTIVE | ?wxPAPER_A3 | ?wxPAPER_A4SMALL | ?wxPAPER_A5 | ?wxPAPER_B4 | ?wxPAPER_B5 | ?wxPAPER_FOLIO | ?wxPAPER_QUARTO | ?wxPAPER_10X14 | ?wxPAPER_11X17 | ?wxPAPER_NOTE | ?wxPAPER_ENV_9 | ?wxPAPER_ENV_10 | ?wxPAPER_ENV_11 | ?wxPAPER_ENV_12 | ?wxPAPER_ENV_14 | ?wxPAPER_ENV_DL | ?wxPAPER_ENV_C5 | ?wxPAPER_ENV_C3 | ?wxPAPER_ENV_C4 | ?wxPAPER_ENV_C6 | ?wxPAPER_ENV_C65 | ?wxPAPER_ENV_B4 | ?wxPAPER_ENV_B5 | ?wxPAPER_ENV_B6 | ?wxPAPER_ENV_ITALY | ?wxPAPER_ENV_MONARCH | ?wxPAPER_ENV_PERSONAL | ?wxPAPER_FANFOLD_US | ?wxPAPER_FANFOLD_STD_GERMAN | ?wxPAPER_FANFOLD_LGL_GERMAN | ?wxPAPER_ISO_B4 | ?wxPAPER_JAPANESE_POSTCARD | ?wxPAPER_9X11 | ?wxPAPER_10X11 | ?wxPAPER_15X11 | ?wxPAPER_ENV_INVITE | ?wxPAPER_LETTER_EXTRA | ?wxPAPER_LEGAL_EXTRA | ?wxPAPER_TABLOID_EXTRA | ?wxPAPER_A4_EXTRA | ?wxPAPER_LETTER_TRANSVERSE | ?wxPAPER_A4_TRANSVERSE | ?wxPAPER_LETTER_EXTRA_TRANSVERSE | ?wxPAPER_A_PLUS | ?wxPAPER_B_PLUS | ?wxPAPER_LETTER_PLUS | ?wxPAPER_A4_PLUS | ?wxPAPER_A5_TRANSVERSE | ?wxPAPER_B5_TRANSVERSE | ?wxPAPER_A3_EXTRA | ?wxPAPER_A5_EXTRA | ?wxPAPER_B5_EXTRA | ?wxPAPER_A2 | ?wxPAPER_A3_TRANSVERSE | ?wxPAPER_A3_EXTRA_TRANSVERSE | ?wxPAPER_DBL_JAPANESE_POSTCARD | ?wxPAPER_A6 | ?wxPAPER_JENV_KAKU2 | ?wxPAPER_JENV_KAKU3 | ?wxPAPER_JENV_CHOU3 | ?wxPAPER_JENV_CHOU4 | ?wxPAPER_LETTER_ROTATED | ?wxPAPER_A3_ROTATED | ?wxPAPER_A4_ROTATED | ?wxPAPER_A5_ROTATED | ?wxPAPER_B4_JIS_ROTATED | ?wxPAPER_B5_JIS_ROTATED | ?wxPAPER_JAPANESE_POSTCARD_ROTATED | ?wxPAPER_DBL_JAPANESE_POSTCARD_ROTATED | ?wxPAPER_A6_ROTATED | ?wxPAPER_JENV_KAKU2_ROTATED | ?wxPAPER_JENV_KAKU3_ROTATED | ?wxPAPER_JENV_CHOU3_ROTATED | ?wxPAPER_JENV_CHOU4_ROTATED | ?wxPAPER_B6_JIS | ?wxPAPER_B6_JIS_ROTATED | ?wxPAPER_12X11 | ?wxPAPER_JENV_YOU4 | ?wxPAPER_JENV_YOU4_ROTATED | ?wxPAPER_P16K | ?wxPAPER_P32K | ?wxPAPER_P32KBIG | ?wxPAPER_PENV_1 | ?wxPAPER_PENV_2 | ?wxPAPER_PENV_3 | ?wxPAPER_PENV_4 | ?wxPAPER_PENV_5 | ?wxPAPER_PENV_6 | ?wxPAPER_PENV_7 | ?wxPAPER_PENV_8 | ?wxPAPER_PENV_9 | ?wxPAPER_PENV_10 | ?wxPAPER_P16K_ROTATED | ?wxPAPER_P32K_ROTATED | ?wxPAPER_P32KBIG_ROTATED | ?wxPAPER_PENV_1_ROTATED | ?wxPAPER_PENV_2_ROTATED | ?wxPAPER_PENV_3_ROTATED | ?wxPAPER_PENV_4_ROTATED | ?wxPAPER_PENV_5_ROTATED | ?wxPAPER_PENV_6_ROTATED | ?wxPAPER_PENV_7_ROTATED | ?wxPAPER_PENV_8_ROTATED | ?wxPAPER_PENV_9_ROTATED | ?wxPAPER_PENV_10_ROTATED
-spec setPaperSize(This, Id) -> 'ok' when
	This::wxPageSetupDialogData(), Id::wx:wx_enum();
      (This, Sz) -> 'ok' when
	This::wxPageSetupDialogData(), Sz::{W::integer(), H::integer()}.
setPaperSize(#wx_ref{type=ThisT,ref=ThisRef},Id)
 when is_integer(Id) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:cast(?wxPageSetupDialogData_SetPaperSize_1_0,
  <<ThisRef:32/?UI,Id:32/?UI>>);
setPaperSize(#wx_ref{type=ThisT,ref=ThisRef},{SzW,SzH})
 when is_integer(SzW),is_integer(SzH) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:cast(?wxPageSetupDialogData_SetPaperSize_1_1,
  <<ThisRef:32/?UI,SzW:32/?UI,SzH:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpagesetupdialogdata.html#wxpagesetupdialogdatasetprintdata">external documentation</a>.
-spec setPrintData(This, PrintData) -> 'ok' when
	This::wxPageSetupDialogData(), PrintData::wxPrintData:wxPrintData().
setPrintData(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=PrintDataT,ref=PrintDataRef}) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  ?CLASS(PrintDataT,wxPrintData),
  wxe_util:cast(?wxPageSetupDialogData_SetPrintData,
  <<ThisRef:32/?UI,PrintDataRef:32/?UI>>).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxPageSetupDialogData()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxPageSetupDialogData),
  wxe_util:destroy(?DESTROY_OBJECT,Obj),
  ok.
