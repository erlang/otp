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

-module(wxPrintData).
-include("wxe.hrl").
-export([destroy/1,getBin/1,getCollate/1,getColour/1,getDuplex/1,getNoCopies/1,
  getOrientation/1,getPaperId/1,getPrinterName/1,getQuality/1,isOk/1,
  new/0,new/1,setBin/2,setCollate/2,setColour/2,setDuplex/2,setNoCopies/2,
  setOrientation/2,setPaperId/2,setPrinterName/2,setQuality/2]).

%% inherited exports
-export([parent_class/1]).

-type wxPrintData() :: wx:wx_object().
-export_type([wxPrintData/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdata.html#wxprintdatawxprintdata">external documentation</a>.
-spec new() -> wxPrintData().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxPrintData_new_0),
  wxe_util:rec(?wxPrintData_new_0).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdata.html#wxprintdatawxprintdata">external documentation</a>.
-spec new(Data) -> wxPrintData() when
	Data::wxPrintData().
new(#wx_ref{type=DataT}=Data) ->
  ?CLASS(DataT,wxPrintData),
  wxe_util:queue_cmd(Data,?get_env(),?wxPrintData_new_1),
  wxe_util:rec(?wxPrintData_new_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdata.html#wxprintdatagetcollate">external documentation</a>.
-spec getCollate(This) -> boolean() when
	This::wxPrintData().
getCollate(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintData),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintData_GetCollate),
  wxe_util:rec(?wxPrintData_GetCollate).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdata.html#wxprintdatagetbin">external documentation</a>.
%%<br /> Res = ?wxPRINTBIN_DEFAULT | ?wxPRINTBIN_ONLYONE | ?wxPRINTBIN_LOWER | ?wxPRINTBIN_MIDDLE | ?wxPRINTBIN_MANUAL | ?wxPRINTBIN_ENVELOPE | ?wxPRINTBIN_ENVMANUAL | ?wxPRINTBIN_AUTO | ?wxPRINTBIN_TRACTOR | ?wxPRINTBIN_SMALLFMT | ?wxPRINTBIN_LARGEFMT | ?wxPRINTBIN_LARGECAPACITY | ?wxPRINTBIN_CASSETTE | ?wxPRINTBIN_FORMSOURCE | ?wxPRINTBIN_USER
-spec getBin(This) -> wx:wx_enum() when
	This::wxPrintData().
getBin(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintData),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintData_GetBin),
  wxe_util:rec(?wxPrintData_GetBin).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdata.html#wxprintdatagetcolour">external documentation</a>.
-spec getColour(This) -> boolean() when
	This::wxPrintData().
getColour(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintData),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintData_GetColour),
  wxe_util:rec(?wxPrintData_GetColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdata.html#wxprintdatagetduplex">external documentation</a>.
%%<br /> Res = ?wxDUPLEX_SIMPLEX | ?wxDUPLEX_HORIZONTAL | ?wxDUPLEX_VERTICAL
-spec getDuplex(This) -> wx:wx_enum() when
	This::wxPrintData().
getDuplex(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintData),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintData_GetDuplex),
  wxe_util:rec(?wxPrintData_GetDuplex).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdata.html#wxprintdatagetnocopies">external documentation</a>.
-spec getNoCopies(This) -> integer() when
	This::wxPrintData().
getNoCopies(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintData),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintData_GetNoCopies),
  wxe_util:rec(?wxPrintData_GetNoCopies).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdata.html#wxprintdatagetorientation">external documentation</a>.
%%<br /> Res = ?wxPORTRAIT | ?wxLANDSCAPE
-spec getOrientation(This) -> wx:wx_enum() when
	This::wxPrintData().
getOrientation(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintData),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintData_GetOrientation),
  wxe_util:rec(?wxPrintData_GetOrientation).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdata.html#wxprintdatagetpaperid">external documentation</a>.
%%<br /> Res = ?wxPAPER_NONE | ?wxPAPER_LETTER | ?wxPAPER_LEGAL | ?wxPAPER_A4 | ?wxPAPER_CSHEET | ?wxPAPER_DSHEET | ?wxPAPER_ESHEET | ?wxPAPER_LETTERSMALL | ?wxPAPER_TABLOID | ?wxPAPER_LEDGER | ?wxPAPER_STATEMENT | ?wxPAPER_EXECUTIVE | ?wxPAPER_A3 | ?wxPAPER_A4SMALL | ?wxPAPER_A5 | ?wxPAPER_B4 | ?wxPAPER_B5 | ?wxPAPER_FOLIO | ?wxPAPER_QUARTO | ?wxPAPER_10X14 | ?wxPAPER_11X17 | ?wxPAPER_NOTE | ?wxPAPER_ENV_9 | ?wxPAPER_ENV_10 | ?wxPAPER_ENV_11 | ?wxPAPER_ENV_12 | ?wxPAPER_ENV_14 | ?wxPAPER_ENV_DL | ?wxPAPER_ENV_C5 | ?wxPAPER_ENV_C3 | ?wxPAPER_ENV_C4 | ?wxPAPER_ENV_C6 | ?wxPAPER_ENV_C65 | ?wxPAPER_ENV_B4 | ?wxPAPER_ENV_B5 | ?wxPAPER_ENV_B6 | ?wxPAPER_ENV_ITALY | ?wxPAPER_ENV_MONARCH | ?wxPAPER_ENV_PERSONAL | ?wxPAPER_FANFOLD_US | ?wxPAPER_FANFOLD_STD_GERMAN | ?wxPAPER_FANFOLD_LGL_GERMAN | ?wxPAPER_ISO_B4 | ?wxPAPER_JAPANESE_POSTCARD | ?wxPAPER_9X11 | ?wxPAPER_10X11 | ?wxPAPER_15X11 | ?wxPAPER_ENV_INVITE | ?wxPAPER_LETTER_EXTRA | ?wxPAPER_LEGAL_EXTRA | ?wxPAPER_TABLOID_EXTRA | ?wxPAPER_A4_EXTRA | ?wxPAPER_LETTER_TRANSVERSE | ?wxPAPER_A4_TRANSVERSE | ?wxPAPER_LETTER_EXTRA_TRANSVERSE | ?wxPAPER_A_PLUS | ?wxPAPER_B_PLUS | ?wxPAPER_LETTER_PLUS | ?wxPAPER_A4_PLUS | ?wxPAPER_A5_TRANSVERSE | ?wxPAPER_B5_TRANSVERSE | ?wxPAPER_A3_EXTRA | ?wxPAPER_A5_EXTRA | ?wxPAPER_B5_EXTRA | ?wxPAPER_A2 | ?wxPAPER_A3_TRANSVERSE | ?wxPAPER_A3_EXTRA_TRANSVERSE | ?wxPAPER_DBL_JAPANESE_POSTCARD | ?wxPAPER_A6 | ?wxPAPER_JENV_KAKU2 | ?wxPAPER_JENV_KAKU3 | ?wxPAPER_JENV_CHOU3 | ?wxPAPER_JENV_CHOU4 | ?wxPAPER_LETTER_ROTATED | ?wxPAPER_A3_ROTATED | ?wxPAPER_A4_ROTATED | ?wxPAPER_A5_ROTATED | ?wxPAPER_B4_JIS_ROTATED | ?wxPAPER_B5_JIS_ROTATED | ?wxPAPER_JAPANESE_POSTCARD_ROTATED | ?wxPAPER_DBL_JAPANESE_POSTCARD_ROTATED | ?wxPAPER_A6_ROTATED | ?wxPAPER_JENV_KAKU2_ROTATED | ?wxPAPER_JENV_KAKU3_ROTATED | ?wxPAPER_JENV_CHOU3_ROTATED | ?wxPAPER_JENV_CHOU4_ROTATED | ?wxPAPER_B6_JIS | ?wxPAPER_B6_JIS_ROTATED | ?wxPAPER_12X11 | ?wxPAPER_JENV_YOU4 | ?wxPAPER_JENV_YOU4_ROTATED | ?wxPAPER_P16K | ?wxPAPER_P32K | ?wxPAPER_P32KBIG | ?wxPAPER_PENV_1 | ?wxPAPER_PENV_2 | ?wxPAPER_PENV_3 | ?wxPAPER_PENV_4 | ?wxPAPER_PENV_5 | ?wxPAPER_PENV_6 | ?wxPAPER_PENV_7 | ?wxPAPER_PENV_8 | ?wxPAPER_PENV_9 | ?wxPAPER_PENV_10 | ?wxPAPER_P16K_ROTATED | ?wxPAPER_P32K_ROTATED | ?wxPAPER_P32KBIG_ROTATED | ?wxPAPER_PENV_1_ROTATED | ?wxPAPER_PENV_2_ROTATED | ?wxPAPER_PENV_3_ROTATED | ?wxPAPER_PENV_4_ROTATED | ?wxPAPER_PENV_5_ROTATED | ?wxPAPER_PENV_6_ROTATED | ?wxPAPER_PENV_7_ROTATED | ?wxPAPER_PENV_8_ROTATED | ?wxPAPER_PENV_9_ROTATED | ?wxPAPER_PENV_10_ROTATED | ?wxPAPER_A0 | ?wxPAPER_A1
-spec getPaperId(This) -> wx:wx_enum() when
	This::wxPrintData().
getPaperId(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintData),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintData_GetPaperId),
  wxe_util:rec(?wxPrintData_GetPaperId).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdata.html#wxprintdatagetprintername">external documentation</a>.
-spec getPrinterName(This) -> unicode:charlist() when
	This::wxPrintData().
getPrinterName(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintData),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintData_GetPrinterName),
  wxe_util:rec(?wxPrintData_GetPrinterName).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdata.html#wxprintdatagetquality">external documentation</a>.
-spec getQuality(This) -> integer() when
	This::wxPrintData().
getQuality(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintData),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintData_GetQuality),
  wxe_util:rec(?wxPrintData_GetQuality).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdata.html#wxprintdataisok">external documentation</a>.
-spec isOk(This) -> boolean() when
	This::wxPrintData().
isOk(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintData),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintData_IsOk),
  wxe_util:rec(?wxPrintData_IsOk).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdata.html#wxprintdatasetbin">external documentation</a>.
%%<br /> Flag = ?wxPRINTBIN_DEFAULT | ?wxPRINTBIN_ONLYONE | ?wxPRINTBIN_LOWER | ?wxPRINTBIN_MIDDLE | ?wxPRINTBIN_MANUAL | ?wxPRINTBIN_ENVELOPE | ?wxPRINTBIN_ENVMANUAL | ?wxPRINTBIN_AUTO | ?wxPRINTBIN_TRACTOR | ?wxPRINTBIN_SMALLFMT | ?wxPRINTBIN_LARGEFMT | ?wxPRINTBIN_LARGECAPACITY | ?wxPRINTBIN_CASSETTE | ?wxPRINTBIN_FORMSOURCE | ?wxPRINTBIN_USER
-spec setBin(This, Flag) -> 'ok' when
	This::wxPrintData(), Flag::wx:wx_enum().
setBin(#wx_ref{type=ThisT}=This,Flag)
 when is_integer(Flag) ->
  ?CLASS(ThisT,wxPrintData),
  wxe_util:queue_cmd(This,Flag,?get_env(),?wxPrintData_SetBin).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdata.html#wxprintdatasetcollate">external documentation</a>.
-spec setCollate(This, Flag) -> 'ok' when
	This::wxPrintData(), Flag::boolean().
setCollate(#wx_ref{type=ThisT}=This,Flag)
 when is_boolean(Flag) ->
  ?CLASS(ThisT,wxPrintData),
  wxe_util:queue_cmd(This,Flag,?get_env(),?wxPrintData_SetCollate).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdata.html#wxprintdatasetcolour">external documentation</a>.
-spec setColour(This, Flag) -> 'ok' when
	This::wxPrintData(), Flag::boolean().
setColour(#wx_ref{type=ThisT}=This,Flag)
 when is_boolean(Flag) ->
  ?CLASS(ThisT,wxPrintData),
  wxe_util:queue_cmd(This,Flag,?get_env(),?wxPrintData_SetColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdata.html#wxprintdatasetduplex">external documentation</a>.
%%<br /> Mode = ?wxDUPLEX_SIMPLEX | ?wxDUPLEX_HORIZONTAL | ?wxDUPLEX_VERTICAL
-spec setDuplex(This, Mode) -> 'ok' when
	This::wxPrintData(), Mode::wx:wx_enum().
setDuplex(#wx_ref{type=ThisT}=This,Mode)
 when is_integer(Mode) ->
  ?CLASS(ThisT,wxPrintData),
  wxe_util:queue_cmd(This,Mode,?get_env(),?wxPrintData_SetDuplex).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdata.html#wxprintdatasetnocopies">external documentation</a>.
-spec setNoCopies(This, N) -> 'ok' when
	This::wxPrintData(), N::integer().
setNoCopies(#wx_ref{type=ThisT}=This,N)
 when is_integer(N) ->
  ?CLASS(ThisT,wxPrintData),
  wxe_util:queue_cmd(This,N,?get_env(),?wxPrintData_SetNoCopies).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdata.html#wxprintdatasetorientation">external documentation</a>.
%%<br /> Orientation = ?wxPORTRAIT | ?wxLANDSCAPE
-spec setOrientation(This, Orientation) -> 'ok' when
	This::wxPrintData(), Orientation::wx:wx_enum().
setOrientation(#wx_ref{type=ThisT}=This,Orientation)
 when is_integer(Orientation) ->
  ?CLASS(ThisT,wxPrintData),
  wxe_util:queue_cmd(This,Orientation,?get_env(),?wxPrintData_SetOrientation).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdata.html#wxprintdatasetpaperid">external documentation</a>.
%%<br /> PaperId = ?wxPAPER_NONE | ?wxPAPER_LETTER | ?wxPAPER_LEGAL | ?wxPAPER_A4 | ?wxPAPER_CSHEET | ?wxPAPER_DSHEET | ?wxPAPER_ESHEET | ?wxPAPER_LETTERSMALL | ?wxPAPER_TABLOID | ?wxPAPER_LEDGER | ?wxPAPER_STATEMENT | ?wxPAPER_EXECUTIVE | ?wxPAPER_A3 | ?wxPAPER_A4SMALL | ?wxPAPER_A5 | ?wxPAPER_B4 | ?wxPAPER_B5 | ?wxPAPER_FOLIO | ?wxPAPER_QUARTO | ?wxPAPER_10X14 | ?wxPAPER_11X17 | ?wxPAPER_NOTE | ?wxPAPER_ENV_9 | ?wxPAPER_ENV_10 | ?wxPAPER_ENV_11 | ?wxPAPER_ENV_12 | ?wxPAPER_ENV_14 | ?wxPAPER_ENV_DL | ?wxPAPER_ENV_C5 | ?wxPAPER_ENV_C3 | ?wxPAPER_ENV_C4 | ?wxPAPER_ENV_C6 | ?wxPAPER_ENV_C65 | ?wxPAPER_ENV_B4 | ?wxPAPER_ENV_B5 | ?wxPAPER_ENV_B6 | ?wxPAPER_ENV_ITALY | ?wxPAPER_ENV_MONARCH | ?wxPAPER_ENV_PERSONAL | ?wxPAPER_FANFOLD_US | ?wxPAPER_FANFOLD_STD_GERMAN | ?wxPAPER_FANFOLD_LGL_GERMAN | ?wxPAPER_ISO_B4 | ?wxPAPER_JAPANESE_POSTCARD | ?wxPAPER_9X11 | ?wxPAPER_10X11 | ?wxPAPER_15X11 | ?wxPAPER_ENV_INVITE | ?wxPAPER_LETTER_EXTRA | ?wxPAPER_LEGAL_EXTRA | ?wxPAPER_TABLOID_EXTRA | ?wxPAPER_A4_EXTRA | ?wxPAPER_LETTER_TRANSVERSE | ?wxPAPER_A4_TRANSVERSE | ?wxPAPER_LETTER_EXTRA_TRANSVERSE | ?wxPAPER_A_PLUS | ?wxPAPER_B_PLUS | ?wxPAPER_LETTER_PLUS | ?wxPAPER_A4_PLUS | ?wxPAPER_A5_TRANSVERSE | ?wxPAPER_B5_TRANSVERSE | ?wxPAPER_A3_EXTRA | ?wxPAPER_A5_EXTRA | ?wxPAPER_B5_EXTRA | ?wxPAPER_A2 | ?wxPAPER_A3_TRANSVERSE | ?wxPAPER_A3_EXTRA_TRANSVERSE | ?wxPAPER_DBL_JAPANESE_POSTCARD | ?wxPAPER_A6 | ?wxPAPER_JENV_KAKU2 | ?wxPAPER_JENV_KAKU3 | ?wxPAPER_JENV_CHOU3 | ?wxPAPER_JENV_CHOU4 | ?wxPAPER_LETTER_ROTATED | ?wxPAPER_A3_ROTATED | ?wxPAPER_A4_ROTATED | ?wxPAPER_A5_ROTATED | ?wxPAPER_B4_JIS_ROTATED | ?wxPAPER_B5_JIS_ROTATED | ?wxPAPER_JAPANESE_POSTCARD_ROTATED | ?wxPAPER_DBL_JAPANESE_POSTCARD_ROTATED | ?wxPAPER_A6_ROTATED | ?wxPAPER_JENV_KAKU2_ROTATED | ?wxPAPER_JENV_KAKU3_ROTATED | ?wxPAPER_JENV_CHOU3_ROTATED | ?wxPAPER_JENV_CHOU4_ROTATED | ?wxPAPER_B6_JIS | ?wxPAPER_B6_JIS_ROTATED | ?wxPAPER_12X11 | ?wxPAPER_JENV_YOU4 | ?wxPAPER_JENV_YOU4_ROTATED | ?wxPAPER_P16K | ?wxPAPER_P32K | ?wxPAPER_P32KBIG | ?wxPAPER_PENV_1 | ?wxPAPER_PENV_2 | ?wxPAPER_PENV_3 | ?wxPAPER_PENV_4 | ?wxPAPER_PENV_5 | ?wxPAPER_PENV_6 | ?wxPAPER_PENV_7 | ?wxPAPER_PENV_8 | ?wxPAPER_PENV_9 | ?wxPAPER_PENV_10 | ?wxPAPER_P16K_ROTATED | ?wxPAPER_P32K_ROTATED | ?wxPAPER_P32KBIG_ROTATED | ?wxPAPER_PENV_1_ROTATED | ?wxPAPER_PENV_2_ROTATED | ?wxPAPER_PENV_3_ROTATED | ?wxPAPER_PENV_4_ROTATED | ?wxPAPER_PENV_5_ROTATED | ?wxPAPER_PENV_6_ROTATED | ?wxPAPER_PENV_7_ROTATED | ?wxPAPER_PENV_8_ROTATED | ?wxPAPER_PENV_9_ROTATED | ?wxPAPER_PENV_10_ROTATED | ?wxPAPER_A0 | ?wxPAPER_A1
-spec setPaperId(This, PaperId) -> 'ok' when
	This::wxPrintData(), PaperId::wx:wx_enum().
setPaperId(#wx_ref{type=ThisT}=This,PaperId)
 when is_integer(PaperId) ->
  ?CLASS(ThisT,wxPrintData),
  wxe_util:queue_cmd(This,PaperId,?get_env(),?wxPrintData_SetPaperId).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdata.html#wxprintdatasetprintername">external documentation</a>.
-spec setPrinterName(This, PrinterName) -> 'ok' when
	This::wxPrintData(), PrinterName::unicode:chardata().
setPrinterName(#wx_ref{type=ThisT}=This,PrinterName)
 when ?is_chardata(PrinterName) ->
  ?CLASS(ThisT,wxPrintData),
  PrinterName_UC = unicode:characters_to_binary(PrinterName),
  wxe_util:queue_cmd(This,PrinterName_UC,?get_env(),?wxPrintData_SetPrinterName).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdata.html#wxprintdatasetquality">external documentation</a>.
-spec setQuality(This, Quality) -> 'ok' when
	This::wxPrintData(), Quality::integer().
setQuality(#wx_ref{type=ThisT}=This,Quality)
 when is_integer(Quality) ->
  ?CLASS(ThisT,wxPrintData),
  wxe_util:queue_cmd(This,Quality,?get_env(),?wxPrintData_SetQuality).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxPrintData()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxPrintData),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
