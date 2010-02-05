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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintdialogdata.html">wxPrintDialogData</a>.
%% @type wxPrintDialogData().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxPrintDialogData).
-include("wxe.hrl").
-export([destroy/1,enableHelp/2,enablePageNumbers/2,enablePrintToFile/2,enableSelection/2,
  getAllPages/1,getCollate/1,getFromPage/1,getMaxPage/1,getMinPage/1,
  getNoCopies/1,getPrintData/1,getPrintToFile/1,getSelection/1,getToPage/1,
  isOk/1,new/0,new/1,setCollate/2,setFromPage/2,setMaxPage/2,setMinPage/2,
  setNoCopies/2,setPrintData/2,setPrintToFile/2,setSelection/2,setToPage/2]).

%% inherited exports
-export([parent_class/1]).

%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @spec () -> wxPrintDialogData()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintdialogdata.html#wxprintdialogdatawxprintdialogdata">external documentation</a>.
new() ->
  wxe_util:construct(?wxPrintDialogData_new_0,
  <<>>).

%% @spec (DialogData::wxPrintDialogData() | wxPrintData:wxPrintData()) -> wxPrintDialogData()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintdialogdata.html#wxprintdialogdatawxprintdialogdata">external documentation</a>.
new(#wx_ref{type=DialogDataT,ref=DialogDataRef}) ->
  DialogDataOP = case ?CLASS_T(DialogDataT,wxPrintDialogData) of
     true ->
       ?wxPrintDialogData_new_1_1;
     _ -> ?CLASS(DialogDataT,wxPrintData),
       ?wxPrintDialogData_new_1_0
     end,
  wxe_util:construct(DialogDataOP,
  <<DialogDataRef:32/?UI>>).

%% @spec (This::wxPrintDialogData(), Flag::bool()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintdialogdata.html#wxprintdialogdataenablehelp">external documentation</a>.
enableHelp(#wx_ref{type=ThisT,ref=ThisRef},Flag)
 when is_boolean(Flag) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:cast(?wxPrintDialogData_EnableHelp,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Flag)):32/?UI>>).

%% @spec (This::wxPrintDialogData(), Flag::bool()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintdialogdata.html#wxprintdialogdataenablepagenumbers">external documentation</a>.
enablePageNumbers(#wx_ref{type=ThisT,ref=ThisRef},Flag)
 when is_boolean(Flag) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:cast(?wxPrintDialogData_EnablePageNumbers,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Flag)):32/?UI>>).

%% @spec (This::wxPrintDialogData(), Flag::bool()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintdialogdata.html#wxprintdialogdataenableprinttofile">external documentation</a>.
enablePrintToFile(#wx_ref{type=ThisT,ref=ThisRef},Flag)
 when is_boolean(Flag) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:cast(?wxPrintDialogData_EnablePrintToFile,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Flag)):32/?UI>>).

%% @spec (This::wxPrintDialogData(), Flag::bool()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintdialogdata.html#wxprintdialogdataenableselection">external documentation</a>.
enableSelection(#wx_ref{type=ThisT,ref=ThisRef},Flag)
 when is_boolean(Flag) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:cast(?wxPrintDialogData_EnableSelection,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Flag)):32/?UI>>).

%% @spec (This::wxPrintDialogData()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintdialogdata.html#wxprintdialogdatagetallpages">external documentation</a>.
getAllPages(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:call(?wxPrintDialogData_GetAllPages,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxPrintDialogData()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintdialogdata.html#wxprintdialogdatagetcollate">external documentation</a>.
getCollate(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:call(?wxPrintDialogData_GetCollate,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxPrintDialogData()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintdialogdata.html#wxprintdialogdatagetfrompage">external documentation</a>.
getFromPage(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:call(?wxPrintDialogData_GetFromPage,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxPrintDialogData()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintdialogdata.html#wxprintdialogdatagetmaxpage">external documentation</a>.
getMaxPage(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:call(?wxPrintDialogData_GetMaxPage,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxPrintDialogData()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintdialogdata.html#wxprintdialogdatagetminpage">external documentation</a>.
getMinPage(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:call(?wxPrintDialogData_GetMinPage,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxPrintDialogData()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintdialogdata.html#wxprintdialogdatagetnocopies">external documentation</a>.
getNoCopies(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:call(?wxPrintDialogData_GetNoCopies,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxPrintDialogData()) -> wxPrintData:wxPrintData()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintdialogdata.html#wxprintdialogdatagetprintdata">external documentation</a>.
getPrintData(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:call(?wxPrintDialogData_GetPrintData,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxPrintDialogData()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintdialogdata.html#wxprintdialogdatagetprinttofile">external documentation</a>.
getPrintToFile(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:call(?wxPrintDialogData_GetPrintToFile,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxPrintDialogData()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintdialogdata.html#wxprintdialogdatagetselection">external documentation</a>.
getSelection(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:call(?wxPrintDialogData_GetSelection,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxPrintDialogData()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintdialogdata.html#wxprintdialogdatagettopage">external documentation</a>.
getToPage(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:call(?wxPrintDialogData_GetToPage,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxPrintDialogData()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintdialogdata.html#wxprintdialogdataisok">external documentation</a>.
isOk(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:call(?wxPrintDialogData_IsOk,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxPrintDialogData(), Flag::bool()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintdialogdata.html#wxprintdialogdatasetcollate">external documentation</a>.
setCollate(#wx_ref{type=ThisT,ref=ThisRef},Flag)
 when is_boolean(Flag) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:cast(?wxPrintDialogData_SetCollate,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Flag)):32/?UI>>).

%% @spec (This::wxPrintDialogData(), V::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintdialogdata.html#wxprintdialogdatasetfrompage">external documentation</a>.
setFromPage(#wx_ref{type=ThisT,ref=ThisRef},V)
 when is_integer(V) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:cast(?wxPrintDialogData_SetFromPage,
  <<ThisRef:32/?UI,V:32/?UI>>).

%% @spec (This::wxPrintDialogData(), V::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintdialogdata.html#wxprintdialogdatasetmaxpage">external documentation</a>.
setMaxPage(#wx_ref{type=ThisT,ref=ThisRef},V)
 when is_integer(V) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:cast(?wxPrintDialogData_SetMaxPage,
  <<ThisRef:32/?UI,V:32/?UI>>).

%% @spec (This::wxPrintDialogData(), V::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintdialogdata.html#wxprintdialogdatasetminpage">external documentation</a>.
setMinPage(#wx_ref{type=ThisT,ref=ThisRef},V)
 when is_integer(V) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:cast(?wxPrintDialogData_SetMinPage,
  <<ThisRef:32/?UI,V:32/?UI>>).

%% @spec (This::wxPrintDialogData(), V::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintdialogdata.html#wxprintdialogdatasetnocopies">external documentation</a>.
setNoCopies(#wx_ref{type=ThisT,ref=ThisRef},V)
 when is_integer(V) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:cast(?wxPrintDialogData_SetNoCopies,
  <<ThisRef:32/?UI,V:32/?UI>>).

%% @spec (This::wxPrintDialogData(), PrintData::wxPrintData:wxPrintData()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintdialogdata.html#wxprintdialogdatasetprintdata">external documentation</a>.
setPrintData(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=PrintDataT,ref=PrintDataRef}) ->
  ?CLASS(ThisT,wxPrintDialogData),
  ?CLASS(PrintDataT,wxPrintData),
  wxe_util:cast(?wxPrintDialogData_SetPrintData,
  <<ThisRef:32/?UI,PrintDataRef:32/?UI>>).

%% @spec (This::wxPrintDialogData(), Flag::bool()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintdialogdata.html#wxprintdialogdatasetprinttofile">external documentation</a>.
setPrintToFile(#wx_ref{type=ThisT,ref=ThisRef},Flag)
 when is_boolean(Flag) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:cast(?wxPrintDialogData_SetPrintToFile,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Flag)):32/?UI>>).

%% @spec (This::wxPrintDialogData(), Flag::bool()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintdialogdata.html#wxprintdialogdatasetselection">external documentation</a>.
setSelection(#wx_ref{type=ThisT,ref=ThisRef},Flag)
 when is_boolean(Flag) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:cast(?wxPrintDialogData_SetSelection,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Flag)):32/?UI>>).

%% @spec (This::wxPrintDialogData(), V::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintdialogdata.html#wxprintdialogdatasettopage">external documentation</a>.
setToPage(#wx_ref{type=ThisT,ref=ThisRef},V)
 when is_integer(V) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:cast(?wxPrintDialogData_SetToPage,
  <<ThisRef:32/?UI,V:32/?UI>>).

%% @spec (This::wxPrintDialogData()) -> ok
%% @doc Destroys this object, do not use object again
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxPrintDialogData),
  wxe_util:destroy(?DESTROY_OBJECT,Obj),
  ok.
