%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2010. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%% This file is generated DO NOT EDIT

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintdata.html">wxPrintData</a>.
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

%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @spec () -> wxPrintData()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintdata.html#wxprintdatawxprintdata">external documentation</a>.
new() ->
  wxe_util:construct(?wxPrintData_new_0,
  <<>>).

%% @spec (PrintData::wxPrintData()) -> wxPrintData()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintdata.html#wxprintdatawxprintdata">external documentation</a>.
new(#wx_ref{type=PrintDataT,ref=PrintDataRef}) ->
  ?CLASS(PrintDataT,wxPrintData),
  wxe_util:construct(?wxPrintData_new_1,
  <<PrintDataRef:32/?UI>>).

%% @spec (This::wxPrintData()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintdata.html#wxprintdatagetcollate">external documentation</a>.
getCollate(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintData),
  wxe_util:call(?wxPrintData_GetCollate,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxPrintData()) -> WxPrintBin
%% WxPrintBin = integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintdata.html#wxprintdatagetbin">external documentation</a>.
%%<br /> WxPrintBin is one of ?wxPRINTBIN_DEFAULT | ?wxPRINTBIN_ONLYONE | ?wxPRINTBIN_LOWER | ?wxPRINTBIN_MIDDLE | ?wxPRINTBIN_MANUAL | ?wxPRINTBIN_ENVELOPE | ?wxPRINTBIN_ENVMANUAL | ?wxPRINTBIN_AUTO | ?wxPRINTBIN_TRACTOR | ?wxPRINTBIN_SMALLFMT | ?wxPRINTBIN_LARGEFMT | ?wxPRINTBIN_LARGECAPACITY | ?wxPRINTBIN_CASSETTE | ?wxPRINTBIN_FORMSOURCE | ?wxPRINTBIN_USER
getBin(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintData),
  wxe_util:call(?wxPrintData_GetBin,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxPrintData()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintdata.html#wxprintdatagetcolour">external documentation</a>.
getColour(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintData),
  wxe_util:call(?wxPrintData_GetColour,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxPrintData()) -> WxDuplexMode
%% WxDuplexMode = integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintdata.html#wxprintdatagetduplex">external documentation</a>.
%%<br /> WxDuplexMode is one of ?wxDUPLEX_SIMPLEX | ?wxDUPLEX_HORIZONTAL | ?wxDUPLEX_VERTICAL
getDuplex(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintData),
  wxe_util:call(?wxPrintData_GetDuplex,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxPrintData()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintdata.html#wxprintdatagetnocopies">external documentation</a>.
getNoCopies(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintData),
  wxe_util:call(?wxPrintData_GetNoCopies,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxPrintData()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintdata.html#wxprintdatagetorientation">external documentation</a>.
getOrientation(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintData),
  wxe_util:call(?wxPrintData_GetOrientation,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxPrintData()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintdata.html#wxprintdatagetpaperid">external documentation</a>.
getPaperId(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintData),
  wxe_util:call(?wxPrintData_GetPaperId,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxPrintData()) -> string()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintdata.html#wxprintdatagetprintername">external documentation</a>.
getPrinterName(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintData),
  wxe_util:call(?wxPrintData_GetPrinterName,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxPrintData()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintdata.html#wxprintdatagetquality">external documentation</a>.
getQuality(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintData),
  wxe_util:call(?wxPrintData_GetQuality,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxPrintData()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintdata.html#wxprintdataisok">external documentation</a>.
isOk(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintData),
  wxe_util:call(?wxPrintData_IsOk,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxPrintData(), Bin::WxPrintBin) -> ok
%% WxPrintBin = integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintdata.html#wxprintdatasetbin">external documentation</a>.
%%<br /> WxPrintBin is one of ?wxPRINTBIN_DEFAULT | ?wxPRINTBIN_ONLYONE | ?wxPRINTBIN_LOWER | ?wxPRINTBIN_MIDDLE | ?wxPRINTBIN_MANUAL | ?wxPRINTBIN_ENVELOPE | ?wxPRINTBIN_ENVMANUAL | ?wxPRINTBIN_AUTO | ?wxPRINTBIN_TRACTOR | ?wxPRINTBIN_SMALLFMT | ?wxPRINTBIN_LARGEFMT | ?wxPRINTBIN_LARGECAPACITY | ?wxPRINTBIN_CASSETTE | ?wxPRINTBIN_FORMSOURCE | ?wxPRINTBIN_USER
setBin(#wx_ref{type=ThisT,ref=ThisRef},Bin)
 when is_integer(Bin) ->
  ?CLASS(ThisT,wxPrintData),
  wxe_util:cast(?wxPrintData_SetBin,
  <<ThisRef:32/?UI,Bin:32/?UI>>).

%% @spec (This::wxPrintData(), Flag::bool()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintdata.html#wxprintdatasetcollate">external documentation</a>.
setCollate(#wx_ref{type=ThisT,ref=ThisRef},Flag)
 when is_boolean(Flag) ->
  ?CLASS(ThisT,wxPrintData),
  wxe_util:cast(?wxPrintData_SetCollate,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Flag)):32/?UI>>).

%% @spec (This::wxPrintData(), Colour::bool()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintdata.html#wxprintdatasetcolour">external documentation</a>.
setColour(#wx_ref{type=ThisT,ref=ThisRef},Colour)
 when is_boolean(Colour) ->
  ?CLASS(ThisT,wxPrintData),
  wxe_util:cast(?wxPrintData_SetColour,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Colour)):32/?UI>>).

%% @spec (This::wxPrintData(), Duplex::WxDuplexMode) -> ok
%% WxDuplexMode = integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintdata.html#wxprintdatasetduplex">external documentation</a>.
%%<br /> WxDuplexMode is one of ?wxDUPLEX_SIMPLEX | ?wxDUPLEX_HORIZONTAL | ?wxDUPLEX_VERTICAL
setDuplex(#wx_ref{type=ThisT,ref=ThisRef},Duplex)
 when is_integer(Duplex) ->
  ?CLASS(ThisT,wxPrintData),
  wxe_util:cast(?wxPrintData_SetDuplex,
  <<ThisRef:32/?UI,Duplex:32/?UI>>).

%% @spec (This::wxPrintData(), V::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintdata.html#wxprintdatasetnocopies">external documentation</a>.
setNoCopies(#wx_ref{type=ThisT,ref=ThisRef},V)
 when is_integer(V) ->
  ?CLASS(ThisT,wxPrintData),
  wxe_util:cast(?wxPrintData_SetNoCopies,
  <<ThisRef:32/?UI,V:32/?UI>>).

%% @spec (This::wxPrintData(), Orient::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintdata.html#wxprintdatasetorientation">external documentation</a>.
setOrientation(#wx_ref{type=ThisT,ref=ThisRef},Orient)
 when is_integer(Orient) ->
  ?CLASS(ThisT,wxPrintData),
  wxe_util:cast(?wxPrintData_SetOrientation,
  <<ThisRef:32/?UI,Orient:32/?UI>>).

%% @spec (This::wxPrintData(), SizeId::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintdata.html#wxprintdatasetpaperid">external documentation</a>.
setPaperId(#wx_ref{type=ThisT,ref=ThisRef},SizeId)
 when is_integer(SizeId) ->
  ?CLASS(ThisT,wxPrintData),
  wxe_util:cast(?wxPrintData_SetPaperId,
  <<ThisRef:32/?UI,SizeId:32/?UI>>).

%% @spec (This::wxPrintData(), Name::string()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintdata.html#wxprintdatasetprintername">external documentation</a>.
setPrinterName(#wx_ref{type=ThisT,ref=ThisRef},Name)
 when is_list(Name) ->
  ?CLASS(ThisT,wxPrintData),
  Name_UC = unicode:characters_to_binary([Name,0]),
  wxe_util:cast(?wxPrintData_SetPrinterName,
  <<ThisRef:32/?UI,(byte_size(Name_UC)):32/?UI,(Name_UC)/binary, 0:(((8- ((0+byte_size(Name_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxPrintData(), Quality::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintdata.html#wxprintdatasetquality">external documentation</a>.
setQuality(#wx_ref{type=ThisT,ref=ThisRef},Quality)
 when is_integer(Quality) ->
  ?CLASS(ThisT,wxPrintData),
  wxe_util:cast(?wxPrintData_SetQuality,
  <<ThisRef:32/?UI,Quality:32/?UI>>).

%% @spec (This::wxPrintData()) -> ok
%% @doc Destroys this object, do not use object again
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxPrintData),
  wxe_util:destroy(?DESTROY_OBJECT,Obj),
  ok.
