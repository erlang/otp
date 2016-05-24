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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdata.html">wxPrintData</a>.
%% @type wxPrintData().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxPrintData).
-include("wxe.hrl").
-export([destroy/1,getBin/1,getCollate/1,getColour/1,getDuplex/1,getNoCopies/1,
  getOrientation/1,getPaperId/1,getPrinterName/1,getQuality/1,isOk/1,
  new/0,new/1,setBin/2,setCollate/2,setColour/2,setDuplex/2,setNoCopies/2,
  setOrientation/2,setPaperId/2,setPrinterName/2,setQuality/2]).

%% inherited exports
-export([parent_class/1]).

-export_type([wxPrintData/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxPrintData() :: wx:wx_object().
%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdata.html#wxprintdatawxprintdata">external documentation</a>.
-spec new() -> wxPrintData().
new() ->
  wxe_util:construct(?wxPrintData_new_0,
  <<>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdata.html#wxprintdatawxprintdata">external documentation</a>.
-spec new(PrintData) -> wxPrintData() when
	PrintData::wxPrintData().
new(#wx_ref{type=PrintDataT,ref=PrintDataRef}) ->
  ?CLASS(PrintDataT,wxPrintData),
  wxe_util:construct(?wxPrintData_new_1,
  <<PrintDataRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdata.html#wxprintdatagetcollate">external documentation</a>.
-spec getCollate(This) -> boolean() when
	This::wxPrintData().
getCollate(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintData),
  wxe_util:call(?wxPrintData_GetCollate,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdata.html#wxprintdatagetbin">external documentation</a>.
%%<br /> Res = ?wxPRINTBIN_DEFAULT | ?wxPRINTBIN_ONLYONE | ?wxPRINTBIN_LOWER | ?wxPRINTBIN_MIDDLE | ?wxPRINTBIN_MANUAL | ?wxPRINTBIN_ENVELOPE | ?wxPRINTBIN_ENVMANUAL | ?wxPRINTBIN_AUTO | ?wxPRINTBIN_TRACTOR | ?wxPRINTBIN_SMALLFMT | ?wxPRINTBIN_LARGEFMT | ?wxPRINTBIN_LARGECAPACITY | ?wxPRINTBIN_CASSETTE | ?wxPRINTBIN_FORMSOURCE | ?wxPRINTBIN_USER
-spec getBin(This) -> wx:wx_enum() when
	This::wxPrintData().
getBin(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintData),
  wxe_util:call(?wxPrintData_GetBin,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdata.html#wxprintdatagetcolour">external documentation</a>.
-spec getColour(This) -> boolean() when
	This::wxPrintData().
getColour(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintData),
  wxe_util:call(?wxPrintData_GetColour,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdata.html#wxprintdatagetduplex">external documentation</a>.
%%<br /> Res = ?wxDUPLEX_SIMPLEX | ?wxDUPLEX_HORIZONTAL | ?wxDUPLEX_VERTICAL
-spec getDuplex(This) -> wx:wx_enum() when
	This::wxPrintData().
getDuplex(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintData),
  wxe_util:call(?wxPrintData_GetDuplex,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdata.html#wxprintdatagetnocopies">external documentation</a>.
-spec getNoCopies(This) -> integer() when
	This::wxPrintData().
getNoCopies(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintData),
  wxe_util:call(?wxPrintData_GetNoCopies,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdata.html#wxprintdatagetorientation">external documentation</a>.
-spec getOrientation(This) -> integer() when
	This::wxPrintData().
getOrientation(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintData),
  wxe_util:call(?wxPrintData_GetOrientation,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdata.html#wxprintdatagetpaperid">external documentation</a>.
%%<br /> Res = ?wxPAPER_NONE | ?wxPAPER_LETTER | ?wxPAPER_LEGAL | ?wxPAPER_A4 | ?wxPAPER_CSHEET | ?wxPAPER_DSHEET | ?wxPAPER_ESHEET | ?wxPAPER_LETTERSMALL | ?wxPAPER_TABLOID | ?wxPAPER_LEDGER | ?wxPAPER_STATEMENT | ?wxPAPER_EXECUTIVE | ?wxPAPER_A3 | ?wxPAPER_A4SMALL | ?wxPAPER_A5 | ?wxPAPER_B4 | ?wxPAPER_B5 | ?wxPAPER_FOLIO | ?wxPAPER_QUARTO | ?wxPAPER_10X14 | ?wxPAPER_11X17 | ?wxPAPER_NOTE | ?wxPAPER_ENV_9 | ?wxPAPER_ENV_10 | ?wxPAPER_ENV_11 | ?wxPAPER_ENV_12 | ?wxPAPER_ENV_14 | ?wxPAPER_ENV_DL | ?wxPAPER_ENV_C5 | ?wxPAPER_ENV_C3 | ?wxPAPER_ENV_C4 | ?wxPAPER_ENV_C6 | ?wxPAPER_ENV_C65 | ?wxPAPER_ENV_B4 | ?wxPAPER_ENV_B5 | ?wxPAPER_ENV_B6 | ?wxPAPER_ENV_ITALY | ?wxPAPER_ENV_MONARCH | ?wxPAPER_ENV_PERSONAL | ?wxPAPER_FANFOLD_US | ?wxPAPER_FANFOLD_STD_GERMAN | ?wxPAPER_FANFOLD_LGL_GERMAN | ?wxPAPER_ISO_B4 | ?wxPAPER_JAPANESE_POSTCARD | ?wxPAPER_9X11 | ?wxPAPER_10X11 | ?wxPAPER_15X11 | ?wxPAPER_ENV_INVITE | ?wxPAPER_LETTER_EXTRA | ?wxPAPER_LEGAL_EXTRA | ?wxPAPER_TABLOID_EXTRA | ?wxPAPER_A4_EXTRA | ?wxPAPER_LETTER_TRANSVERSE | ?wxPAPER_A4_TRANSVERSE | ?wxPAPER_LETTER_EXTRA_TRANSVERSE | ?wxPAPER_A_PLUS | ?wxPAPER_B_PLUS | ?wxPAPER_LETTER_PLUS | ?wxPAPER_A4_PLUS | ?wxPAPER_A5_TRANSVERSE | ?wxPAPER_B5_TRANSVERSE | ?wxPAPER_A3_EXTRA | ?wxPAPER_A5_EXTRA | ?wxPAPER_B5_EXTRA | ?wxPAPER_A2 | ?wxPAPER_A3_TRANSVERSE | ?wxPAPER_A3_EXTRA_TRANSVERSE | ?wxPAPER_DBL_JAPANESE_POSTCARD | ?wxPAPER_A6 | ?wxPAPER_JENV_KAKU2 | ?wxPAPER_JENV_KAKU3 | ?wxPAPER_JENV_CHOU3 | ?wxPAPER_JENV_CHOU4 | ?wxPAPER_LETTER_ROTATED | ?wxPAPER_A3_ROTATED | ?wxPAPER_A4_ROTATED | ?wxPAPER_A5_ROTATED | ?wxPAPER_B4_JIS_ROTATED | ?wxPAPER_B5_JIS_ROTATED | ?wxPAPER_JAPANESE_POSTCARD_ROTATED | ?wxPAPER_DBL_JAPANESE_POSTCARD_ROTATED | ?wxPAPER_A6_ROTATED | ?wxPAPER_JENV_KAKU2_ROTATED | ?wxPAPER_JENV_KAKU3_ROTATED | ?wxPAPER_JENV_CHOU3_ROTATED | ?wxPAPER_JENV_CHOU4_ROTATED | ?wxPAPER_B6_JIS | ?wxPAPER_B6_JIS_ROTATED | ?wxPAPER_12X11 | ?wxPAPER_JENV_YOU4 | ?wxPAPER_JENV_YOU4_ROTATED | ?wxPAPER_P16K | ?wxPAPER_P32K | ?wxPAPER_P32KBIG | ?wxPAPER_PENV_1 | ?wxPAPER_PENV_2 | ?wxPAPER_PENV_3 | ?wxPAPER_PENV_4 | ?wxPAPER_PENV_5 | ?wxPAPER_PENV_6 | ?wxPAPER_PENV_7 | ?wxPAPER_PENV_8 | ?wxPAPER_PENV_9 | ?wxPAPER_PENV_10 | ?wxPAPER_P16K_ROTATED | ?wxPAPER_P32K_ROTATED | ?wxPAPER_P32KBIG_ROTATED | ?wxPAPER_PENV_1_ROTATED | ?wxPAPER_PENV_2_ROTATED | ?wxPAPER_PENV_3_ROTATED | ?wxPAPER_PENV_4_ROTATED | ?wxPAPER_PENV_5_ROTATED | ?wxPAPER_PENV_6_ROTATED | ?wxPAPER_PENV_7_ROTATED | ?wxPAPER_PENV_8_ROTATED | ?wxPAPER_PENV_9_ROTATED | ?wxPAPER_PENV_10_ROTATED
-spec getPaperId(This) -> wx:wx_enum() when
	This::wxPrintData().
getPaperId(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintData),
  wxe_util:call(?wxPrintData_GetPaperId,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdata.html#wxprintdatagetprintername">external documentation</a>.
-spec getPrinterName(This) -> unicode:charlist() when
	This::wxPrintData().
getPrinterName(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintData),
  wxe_util:call(?wxPrintData_GetPrinterName,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdata.html#wxprintdatagetquality">external documentation</a>.
-spec getQuality(This) -> integer() when
	This::wxPrintData().
getQuality(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintData),
  wxe_util:call(?wxPrintData_GetQuality,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdata.html#wxprintdataisok">external documentation</a>.
-spec isOk(This) -> boolean() when
	This::wxPrintData().
isOk(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintData),
  wxe_util:call(?wxPrintData_IsOk,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdata.html#wxprintdatasetbin">external documentation</a>.
%%<br /> Bin = ?wxPRINTBIN_DEFAULT | ?wxPRINTBIN_ONLYONE | ?wxPRINTBIN_LOWER | ?wxPRINTBIN_MIDDLE | ?wxPRINTBIN_MANUAL | ?wxPRINTBIN_ENVELOPE | ?wxPRINTBIN_ENVMANUAL | ?wxPRINTBIN_AUTO | ?wxPRINTBIN_TRACTOR | ?wxPRINTBIN_SMALLFMT | ?wxPRINTBIN_LARGEFMT | ?wxPRINTBIN_LARGECAPACITY | ?wxPRINTBIN_CASSETTE | ?wxPRINTBIN_FORMSOURCE | ?wxPRINTBIN_USER
-spec setBin(This, Bin) -> 'ok' when
	This::wxPrintData(), Bin::wx:wx_enum().
setBin(#wx_ref{type=ThisT,ref=ThisRef},Bin)
 when is_integer(Bin) ->
  ?CLASS(ThisT,wxPrintData),
  wxe_util:cast(?wxPrintData_SetBin,
  <<ThisRef:32/?UI,Bin:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdata.html#wxprintdatasetcollate">external documentation</a>.
-spec setCollate(This, Flag) -> 'ok' when
	This::wxPrintData(), Flag::boolean().
setCollate(#wx_ref{type=ThisT,ref=ThisRef},Flag)
 when is_boolean(Flag) ->
  ?CLASS(ThisT,wxPrintData),
  wxe_util:cast(?wxPrintData_SetCollate,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Flag)):32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdata.html#wxprintdatasetcolour">external documentation</a>.
-spec setColour(This, Colour) -> 'ok' when
	This::wxPrintData(), Colour::boolean().
setColour(#wx_ref{type=ThisT,ref=ThisRef},Colour)
 when is_boolean(Colour) ->
  ?CLASS(ThisT,wxPrintData),
  wxe_util:cast(?wxPrintData_SetColour,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Colour)):32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdata.html#wxprintdatasetduplex">external documentation</a>.
%%<br /> Duplex = ?wxDUPLEX_SIMPLEX | ?wxDUPLEX_HORIZONTAL | ?wxDUPLEX_VERTICAL
-spec setDuplex(This, Duplex) -> 'ok' when
	This::wxPrintData(), Duplex::wx:wx_enum().
setDuplex(#wx_ref{type=ThisT,ref=ThisRef},Duplex)
 when is_integer(Duplex) ->
  ?CLASS(ThisT,wxPrintData),
  wxe_util:cast(?wxPrintData_SetDuplex,
  <<ThisRef:32/?UI,Duplex:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdata.html#wxprintdatasetnocopies">external documentation</a>.
-spec setNoCopies(This, V) -> 'ok' when
	This::wxPrintData(), V::integer().
setNoCopies(#wx_ref{type=ThisT,ref=ThisRef},V)
 when is_integer(V) ->
  ?CLASS(ThisT,wxPrintData),
  wxe_util:cast(?wxPrintData_SetNoCopies,
  <<ThisRef:32/?UI,V:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdata.html#wxprintdatasetorientation">external documentation</a>.
-spec setOrientation(This, Orient) -> 'ok' when
	This::wxPrintData(), Orient::integer().
setOrientation(#wx_ref{type=ThisT,ref=ThisRef},Orient)
 when is_integer(Orient) ->
  ?CLASS(ThisT,wxPrintData),
  wxe_util:cast(?wxPrintData_SetOrientation,
  <<ThisRef:32/?UI,Orient:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdata.html#wxprintdatasetpaperid">external documentation</a>.
%%<br /> SizeId = ?wxPAPER_NONE | ?wxPAPER_LETTER | ?wxPAPER_LEGAL | ?wxPAPER_A4 | ?wxPAPER_CSHEET | ?wxPAPER_DSHEET | ?wxPAPER_ESHEET | ?wxPAPER_LETTERSMALL | ?wxPAPER_TABLOID | ?wxPAPER_LEDGER | ?wxPAPER_STATEMENT | ?wxPAPER_EXECUTIVE | ?wxPAPER_A3 | ?wxPAPER_A4SMALL | ?wxPAPER_A5 | ?wxPAPER_B4 | ?wxPAPER_B5 | ?wxPAPER_FOLIO | ?wxPAPER_QUARTO | ?wxPAPER_10X14 | ?wxPAPER_11X17 | ?wxPAPER_NOTE | ?wxPAPER_ENV_9 | ?wxPAPER_ENV_10 | ?wxPAPER_ENV_11 | ?wxPAPER_ENV_12 | ?wxPAPER_ENV_14 | ?wxPAPER_ENV_DL | ?wxPAPER_ENV_C5 | ?wxPAPER_ENV_C3 | ?wxPAPER_ENV_C4 | ?wxPAPER_ENV_C6 | ?wxPAPER_ENV_C65 | ?wxPAPER_ENV_B4 | ?wxPAPER_ENV_B5 | ?wxPAPER_ENV_B6 | ?wxPAPER_ENV_ITALY | ?wxPAPER_ENV_MONARCH | ?wxPAPER_ENV_PERSONAL | ?wxPAPER_FANFOLD_US | ?wxPAPER_FANFOLD_STD_GERMAN | ?wxPAPER_FANFOLD_LGL_GERMAN | ?wxPAPER_ISO_B4 | ?wxPAPER_JAPANESE_POSTCARD | ?wxPAPER_9X11 | ?wxPAPER_10X11 | ?wxPAPER_15X11 | ?wxPAPER_ENV_INVITE | ?wxPAPER_LETTER_EXTRA | ?wxPAPER_LEGAL_EXTRA | ?wxPAPER_TABLOID_EXTRA | ?wxPAPER_A4_EXTRA | ?wxPAPER_LETTER_TRANSVERSE | ?wxPAPER_A4_TRANSVERSE | ?wxPAPER_LETTER_EXTRA_TRANSVERSE | ?wxPAPER_A_PLUS | ?wxPAPER_B_PLUS | ?wxPAPER_LETTER_PLUS | ?wxPAPER_A4_PLUS | ?wxPAPER_A5_TRANSVERSE | ?wxPAPER_B5_TRANSVERSE | ?wxPAPER_A3_EXTRA | ?wxPAPER_A5_EXTRA | ?wxPAPER_B5_EXTRA | ?wxPAPER_A2 | ?wxPAPER_A3_TRANSVERSE | ?wxPAPER_A3_EXTRA_TRANSVERSE | ?wxPAPER_DBL_JAPANESE_POSTCARD | ?wxPAPER_A6 | ?wxPAPER_JENV_KAKU2 | ?wxPAPER_JENV_KAKU3 | ?wxPAPER_JENV_CHOU3 | ?wxPAPER_JENV_CHOU4 | ?wxPAPER_LETTER_ROTATED | ?wxPAPER_A3_ROTATED | ?wxPAPER_A4_ROTATED | ?wxPAPER_A5_ROTATED | ?wxPAPER_B4_JIS_ROTATED | ?wxPAPER_B5_JIS_ROTATED | ?wxPAPER_JAPANESE_POSTCARD_ROTATED | ?wxPAPER_DBL_JAPANESE_POSTCARD_ROTATED | ?wxPAPER_A6_ROTATED | ?wxPAPER_JENV_KAKU2_ROTATED | ?wxPAPER_JENV_KAKU3_ROTATED | ?wxPAPER_JENV_CHOU3_ROTATED | ?wxPAPER_JENV_CHOU4_ROTATED | ?wxPAPER_B6_JIS | ?wxPAPER_B6_JIS_ROTATED | ?wxPAPER_12X11 | ?wxPAPER_JENV_YOU4 | ?wxPAPER_JENV_YOU4_ROTATED | ?wxPAPER_P16K | ?wxPAPER_P32K | ?wxPAPER_P32KBIG | ?wxPAPER_PENV_1 | ?wxPAPER_PENV_2 | ?wxPAPER_PENV_3 | ?wxPAPER_PENV_4 | ?wxPAPER_PENV_5 | ?wxPAPER_PENV_6 | ?wxPAPER_PENV_7 | ?wxPAPER_PENV_8 | ?wxPAPER_PENV_9 | ?wxPAPER_PENV_10 | ?wxPAPER_P16K_ROTATED | ?wxPAPER_P32K_ROTATED | ?wxPAPER_P32KBIG_ROTATED | ?wxPAPER_PENV_1_ROTATED | ?wxPAPER_PENV_2_ROTATED | ?wxPAPER_PENV_3_ROTATED | ?wxPAPER_PENV_4_ROTATED | ?wxPAPER_PENV_5_ROTATED | ?wxPAPER_PENV_6_ROTATED | ?wxPAPER_PENV_7_ROTATED | ?wxPAPER_PENV_8_ROTATED | ?wxPAPER_PENV_9_ROTATED | ?wxPAPER_PENV_10_ROTATED
-spec setPaperId(This, SizeId) -> 'ok' when
	This::wxPrintData(), SizeId::wx:wx_enum().
setPaperId(#wx_ref{type=ThisT,ref=ThisRef},SizeId)
 when is_integer(SizeId) ->
  ?CLASS(ThisT,wxPrintData),
  wxe_util:cast(?wxPrintData_SetPaperId,
  <<ThisRef:32/?UI,SizeId:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdata.html#wxprintdatasetprintername">external documentation</a>.
-spec setPrinterName(This, Name) -> 'ok' when
	This::wxPrintData(), Name::unicode:chardata().
setPrinterName(#wx_ref{type=ThisT,ref=ThisRef},Name)
 when is_list(Name) ->
  ?CLASS(ThisT,wxPrintData),
  Name_UC = unicode:characters_to_binary([Name,0]),
  wxe_util:cast(?wxPrintData_SetPrinterName,
  <<ThisRef:32/?UI,(byte_size(Name_UC)):32/?UI,(Name_UC)/binary, 0:(((8- ((0+byte_size(Name_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdata.html#wxprintdatasetquality">external documentation</a>.
-spec setQuality(This, Quality) -> 'ok' when
	This::wxPrintData(), Quality::integer().
setQuality(#wx_ref{type=ThisT,ref=ThisRef},Quality)
 when is_integer(Quality) ->
  ?CLASS(ThisT,wxPrintData),
  wxe_util:cast(?wxPrintData_SetQuality,
  <<ThisRef:32/?UI,Quality:32/?UI>>).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxPrintData()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxPrintData),
  wxe_util:destroy(?DESTROY_OBJECT,Obj),
  ok.
