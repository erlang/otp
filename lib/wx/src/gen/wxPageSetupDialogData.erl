%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2012. All Rights Reserved.
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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/stable/wx_wxpagesetupdialogdata.html">wxPageSetupDialogData</a>.
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
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxpagesetupdialogdata.html#wxpagesetupdialogdatawxpagesetupdialogdata">external documentation</a>.
-spec new() -> wxPageSetupDialogData().
new() ->
  wxe_util:construct(?wxPageSetupDialogData_new_0,
  <<>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxpagesetupdialogdata.html#wxpagesetupdialogdatawxpagesetupdialogdata">external documentation</a>.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxpagesetupdialogdata.html#wxpagesetupdialogdataenablehelp">external documentation</a>.
-spec enableHelp(This, Flag) -> ok when
	This::wxPageSetupDialogData(), Flag::boolean().
enableHelp(#wx_ref{type=ThisT,ref=ThisRef},Flag)
 when is_boolean(Flag) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:cast(?wxPageSetupDialogData_EnableHelp,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Flag)):32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxpagesetupdialogdata.html#wxpagesetupdialogdataenablemargins">external documentation</a>.
-spec enableMargins(This, Flag) -> ok when
	This::wxPageSetupDialogData(), Flag::boolean().
enableMargins(#wx_ref{type=ThisT,ref=ThisRef},Flag)
 when is_boolean(Flag) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:cast(?wxPageSetupDialogData_EnableMargins,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Flag)):32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxpagesetupdialogdata.html#wxpagesetupdialogdataenableorientation">external documentation</a>.
-spec enableOrientation(This, Flag) -> ok when
	This::wxPageSetupDialogData(), Flag::boolean().
enableOrientation(#wx_ref{type=ThisT,ref=ThisRef},Flag)
 when is_boolean(Flag) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:cast(?wxPageSetupDialogData_EnableOrientation,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Flag)):32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxpagesetupdialogdata.html#wxpagesetupdialogdataenablepaper">external documentation</a>.
-spec enablePaper(This, Flag) -> ok when
	This::wxPageSetupDialogData(), Flag::boolean().
enablePaper(#wx_ref{type=ThisT,ref=ThisRef},Flag)
 when is_boolean(Flag) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:cast(?wxPageSetupDialogData_EnablePaper,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Flag)):32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxpagesetupdialogdata.html#wxpagesetupdialogdataenableprinter">external documentation</a>.
-spec enablePrinter(This, Flag) -> ok when
	This::wxPageSetupDialogData(), Flag::boolean().
enablePrinter(#wx_ref{type=ThisT,ref=ThisRef},Flag)
 when is_boolean(Flag) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:cast(?wxPageSetupDialogData_EnablePrinter,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Flag)):32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxpagesetupdialogdata.html#wxpagesetupdialogdatagetdefaultminmargins">external documentation</a>.
-spec getDefaultMinMargins(This) -> boolean() when
	This::wxPageSetupDialogData().
getDefaultMinMargins(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:call(?wxPageSetupDialogData_GetDefaultMinMargins,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxpagesetupdialogdata.html#wxpagesetupdialogdatagetenablemargins">external documentation</a>.
-spec getEnableMargins(This) -> boolean() when
	This::wxPageSetupDialogData().
getEnableMargins(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:call(?wxPageSetupDialogData_GetEnableMargins,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxpagesetupdialogdata.html#wxpagesetupdialogdatagetenableorientation">external documentation</a>.
-spec getEnableOrientation(This) -> boolean() when
	This::wxPageSetupDialogData().
getEnableOrientation(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:call(?wxPageSetupDialogData_GetEnableOrientation,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxpagesetupdialogdata.html#wxpagesetupdialogdatagetenablepaper">external documentation</a>.
-spec getEnablePaper(This) -> boolean() when
	This::wxPageSetupDialogData().
getEnablePaper(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:call(?wxPageSetupDialogData_GetEnablePaper,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxpagesetupdialogdata.html#wxpagesetupdialogdatagetenableprinter">external documentation</a>.
-spec getEnablePrinter(This) -> boolean() when
	This::wxPageSetupDialogData().
getEnablePrinter(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:call(?wxPageSetupDialogData_GetEnablePrinter,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxpagesetupdialogdata.html#wxpagesetupdialogdatagetenablehelp">external documentation</a>.
-spec getEnableHelp(This) -> boolean() when
	This::wxPageSetupDialogData().
getEnableHelp(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:call(?wxPageSetupDialogData_GetEnableHelp,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxpagesetupdialogdata.html#wxpagesetupdialogdatagetdefaultinfo">external documentation</a>.
-spec getDefaultInfo(This) -> boolean() when
	This::wxPageSetupDialogData().
getDefaultInfo(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:call(?wxPageSetupDialogData_GetDefaultInfo,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxpagesetupdialogdata.html#wxpagesetupdialogdatagetmargintopleft">external documentation</a>.
-spec getMarginTopLeft(This) -> {X::integer(), Y::integer()} when
	This::wxPageSetupDialogData().
getMarginTopLeft(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:call(?wxPageSetupDialogData_GetMarginTopLeft,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxpagesetupdialogdata.html#wxpagesetupdialogdatagetmarginbottomright">external documentation</a>.
-spec getMarginBottomRight(This) -> {X::integer(), Y::integer()} when
	This::wxPageSetupDialogData().
getMarginBottomRight(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:call(?wxPageSetupDialogData_GetMarginBottomRight,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxpagesetupdialogdata.html#wxpagesetupdialogdatagetminmargintopleft">external documentation</a>.
-spec getMinMarginTopLeft(This) -> {X::integer(), Y::integer()} when
	This::wxPageSetupDialogData().
getMinMarginTopLeft(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:call(?wxPageSetupDialogData_GetMinMarginTopLeft,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxpagesetupdialogdata.html#wxpagesetupdialogdatagetminmarginbottomright">external documentation</a>.
-spec getMinMarginBottomRight(This) -> {X::integer(), Y::integer()} when
	This::wxPageSetupDialogData().
getMinMarginBottomRight(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:call(?wxPageSetupDialogData_GetMinMarginBottomRight,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxpagesetupdialogdata.html#wxpagesetupdialogdatagetpaperid">external documentation</a>.
-spec getPaperId(This) -> integer() when
	This::wxPageSetupDialogData().
getPaperId(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:call(?wxPageSetupDialogData_GetPaperId,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxpagesetupdialogdata.html#wxpagesetupdialogdatagetpapersize">external documentation</a>.
-spec getPaperSize(This) -> {W::integer(), H::integer()} when
	This::wxPageSetupDialogData().
getPaperSize(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:call(?wxPageSetupDialogData_GetPaperSize,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxpagesetupdialogdata.html#wxpagesetupdialogdatagetprintdata">external documentation</a>.
-spec getPrintData(This) -> wxPrintData:wxPrintData() when
	This::wxPageSetupDialogData().
getPrintData(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:call(?wxPageSetupDialogData_GetPrintData,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxpagesetupdialogdata.html#wxpagesetupdialogdataisok">external documentation</a>.
-spec isOk(This) -> boolean() when
	This::wxPageSetupDialogData().
isOk(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:call(?wxPageSetupDialogData_IsOk,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxpagesetupdialogdata.html#wxpagesetupdialogdatasetdefaultinfo">external documentation</a>.
-spec setDefaultInfo(This, Flag) -> ok when
	This::wxPageSetupDialogData(), Flag::boolean().
setDefaultInfo(#wx_ref{type=ThisT,ref=ThisRef},Flag)
 when is_boolean(Flag) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:cast(?wxPageSetupDialogData_SetDefaultInfo,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Flag)):32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxpagesetupdialogdata.html#wxpagesetupdialogdatasetdefaultminmargins">external documentation</a>.
-spec setDefaultMinMargins(This, Flag) -> ok when
	This::wxPageSetupDialogData(), Flag::boolean().
setDefaultMinMargins(#wx_ref{type=ThisT,ref=ThisRef},Flag)
 when is_boolean(Flag) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:cast(?wxPageSetupDialogData_SetDefaultMinMargins,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Flag)):32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxpagesetupdialogdata.html#wxpagesetupdialogdatasetmargintopleft">external documentation</a>.
-spec setMarginTopLeft(This, Pt) -> ok when
	This::wxPageSetupDialogData(), Pt::{X::integer(), Y::integer()}.
setMarginTopLeft(#wx_ref{type=ThisT,ref=ThisRef},{PtX,PtY})
 when is_integer(PtX),is_integer(PtY) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:cast(?wxPageSetupDialogData_SetMarginTopLeft,
  <<ThisRef:32/?UI,PtX:32/?UI,PtY:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxpagesetupdialogdata.html#wxpagesetupdialogdatasetmarginbottomright">external documentation</a>.
-spec setMarginBottomRight(This, Pt) -> ok when
	This::wxPageSetupDialogData(), Pt::{X::integer(), Y::integer()}.
setMarginBottomRight(#wx_ref{type=ThisT,ref=ThisRef},{PtX,PtY})
 when is_integer(PtX),is_integer(PtY) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:cast(?wxPageSetupDialogData_SetMarginBottomRight,
  <<ThisRef:32/?UI,PtX:32/?UI,PtY:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxpagesetupdialogdata.html#wxpagesetupdialogdatasetminmargintopleft">external documentation</a>.
-spec setMinMarginTopLeft(This, Pt) -> ok when
	This::wxPageSetupDialogData(), Pt::{X::integer(), Y::integer()}.
setMinMarginTopLeft(#wx_ref{type=ThisT,ref=ThisRef},{PtX,PtY})
 when is_integer(PtX),is_integer(PtY) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:cast(?wxPageSetupDialogData_SetMinMarginTopLeft,
  <<ThisRef:32/?UI,PtX:32/?UI,PtY:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxpagesetupdialogdata.html#wxpagesetupdialogdatasetminmarginbottomright">external documentation</a>.
-spec setMinMarginBottomRight(This, Pt) -> ok when
	This::wxPageSetupDialogData(), Pt::{X::integer(), Y::integer()}.
setMinMarginBottomRight(#wx_ref{type=ThisT,ref=ThisRef},{PtX,PtY})
 when is_integer(PtX),is_integer(PtY) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:cast(?wxPageSetupDialogData_SetMinMarginBottomRight,
  <<ThisRef:32/?UI,PtX:32/?UI,PtY:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxpagesetupdialogdata.html#wxpagesetupdialogdatasetpaperid">external documentation</a>.
-spec setPaperId(This, Id) -> ok when
	This::wxPageSetupDialogData(), Id::integer().
setPaperId(#wx_ref{type=ThisT,ref=ThisRef},Id)
 when is_integer(Id) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  wxe_util:cast(?wxPageSetupDialogData_SetPaperId,
  <<ThisRef:32/?UI,Id:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxpagesetupdialogdata.html#wxpagesetupdialogdatasetpapersize">external documentation</a>.
%% <br /> Also:<br />
%% setPaperSize(This, Sz) -> ok when<br />
%% 	This::wxPageSetupDialogData(), Sz::{W::integer(), H::integer()}.<br />
%% 
-spec setPaperSize(This, Id) -> ok when
	This::wxPageSetupDialogData(), Id::integer();
      (This, Sz) -> ok when
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxpagesetupdialogdata.html#wxpagesetupdialogdatasetprintdata">external documentation</a>.
-spec setPrintData(This, PrintData) -> ok when
	This::wxPageSetupDialogData(), PrintData::wxPrintData:wxPrintData().
setPrintData(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=PrintDataT,ref=PrintDataRef}) ->
  ?CLASS(ThisT,wxPageSetupDialogData),
  ?CLASS(PrintDataT,wxPrintData),
  wxe_util:cast(?wxPageSetupDialogData_SetPrintData,
  <<ThisRef:32/?UI,PrintDataRef:32/?UI>>).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxPageSetupDialogData()) -> ok.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxPageSetupDialogData),
  wxe_util:destroy(?DESTROY_OBJECT,Obj),
  ok.
