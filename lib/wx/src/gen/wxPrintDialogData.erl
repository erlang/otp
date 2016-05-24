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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdialogdata.html">wxPrintDialogData</a>.
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

-export_type([wxPrintDialogData/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxPrintDialogData() :: wx:wx_object().
%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdialogdata.html#wxprintdialogdatawxprintdialogdata">external documentation</a>.
-spec new() -> wxPrintDialogData().
new() ->
  wxe_util:construct(?wxPrintDialogData_new_0,
  <<>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdialogdata.html#wxprintdialogdatawxprintdialogdata">external documentation</a>.
-spec new(DialogData) -> wxPrintDialogData() when
	DialogData::wxPrintDialogData() | wxPrintData:wxPrintData().
new(#wx_ref{type=DialogDataT,ref=DialogDataRef}) ->
  DialogDataOP = case ?CLASS_T(DialogDataT,wxPrintDialogData) of
     true ->
       ?wxPrintDialogData_new_1_1;
     _ -> ?CLASS(DialogDataT,wxPrintData),
       ?wxPrintDialogData_new_1_0
     end,
  wxe_util:construct(DialogDataOP,
  <<DialogDataRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdialogdata.html#wxprintdialogdataenablehelp">external documentation</a>.
-spec enableHelp(This, Flag) -> 'ok' when
	This::wxPrintDialogData(), Flag::boolean().
enableHelp(#wx_ref{type=ThisT,ref=ThisRef},Flag)
 when is_boolean(Flag) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:cast(?wxPrintDialogData_EnableHelp,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Flag)):32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdialogdata.html#wxprintdialogdataenablepagenumbers">external documentation</a>.
-spec enablePageNumbers(This, Flag) -> 'ok' when
	This::wxPrintDialogData(), Flag::boolean().
enablePageNumbers(#wx_ref{type=ThisT,ref=ThisRef},Flag)
 when is_boolean(Flag) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:cast(?wxPrintDialogData_EnablePageNumbers,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Flag)):32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdialogdata.html#wxprintdialogdataenableprinttofile">external documentation</a>.
-spec enablePrintToFile(This, Flag) -> 'ok' when
	This::wxPrintDialogData(), Flag::boolean().
enablePrintToFile(#wx_ref{type=ThisT,ref=ThisRef},Flag)
 when is_boolean(Flag) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:cast(?wxPrintDialogData_EnablePrintToFile,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Flag)):32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdialogdata.html#wxprintdialogdataenableselection">external documentation</a>.
-spec enableSelection(This, Flag) -> 'ok' when
	This::wxPrintDialogData(), Flag::boolean().
enableSelection(#wx_ref{type=ThisT,ref=ThisRef},Flag)
 when is_boolean(Flag) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:cast(?wxPrintDialogData_EnableSelection,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Flag)):32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdialogdata.html#wxprintdialogdatagetallpages">external documentation</a>.
-spec getAllPages(This) -> boolean() when
	This::wxPrintDialogData().
getAllPages(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:call(?wxPrintDialogData_GetAllPages,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdialogdata.html#wxprintdialogdatagetcollate">external documentation</a>.
-spec getCollate(This) -> boolean() when
	This::wxPrintDialogData().
getCollate(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:call(?wxPrintDialogData_GetCollate,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdialogdata.html#wxprintdialogdatagetfrompage">external documentation</a>.
-spec getFromPage(This) -> integer() when
	This::wxPrintDialogData().
getFromPage(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:call(?wxPrintDialogData_GetFromPage,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdialogdata.html#wxprintdialogdatagetmaxpage">external documentation</a>.
-spec getMaxPage(This) -> integer() when
	This::wxPrintDialogData().
getMaxPage(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:call(?wxPrintDialogData_GetMaxPage,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdialogdata.html#wxprintdialogdatagetminpage">external documentation</a>.
-spec getMinPage(This) -> integer() when
	This::wxPrintDialogData().
getMinPage(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:call(?wxPrintDialogData_GetMinPage,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdialogdata.html#wxprintdialogdatagetnocopies">external documentation</a>.
-spec getNoCopies(This) -> integer() when
	This::wxPrintDialogData().
getNoCopies(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:call(?wxPrintDialogData_GetNoCopies,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdialogdata.html#wxprintdialogdatagetprintdata">external documentation</a>.
-spec getPrintData(This) -> wxPrintData:wxPrintData() when
	This::wxPrintDialogData().
getPrintData(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:call(?wxPrintDialogData_GetPrintData,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdialogdata.html#wxprintdialogdatagetprinttofile">external documentation</a>.
-spec getPrintToFile(This) -> boolean() when
	This::wxPrintDialogData().
getPrintToFile(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:call(?wxPrintDialogData_GetPrintToFile,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdialogdata.html#wxprintdialogdatagetselection">external documentation</a>.
-spec getSelection(This) -> boolean() when
	This::wxPrintDialogData().
getSelection(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:call(?wxPrintDialogData_GetSelection,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdialogdata.html#wxprintdialogdatagettopage">external documentation</a>.
-spec getToPage(This) -> integer() when
	This::wxPrintDialogData().
getToPage(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:call(?wxPrintDialogData_GetToPage,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdialogdata.html#wxprintdialogdataisok">external documentation</a>.
-spec isOk(This) -> boolean() when
	This::wxPrintDialogData().
isOk(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:call(?wxPrintDialogData_IsOk,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdialogdata.html#wxprintdialogdatasetcollate">external documentation</a>.
-spec setCollate(This, Flag) -> 'ok' when
	This::wxPrintDialogData(), Flag::boolean().
setCollate(#wx_ref{type=ThisT,ref=ThisRef},Flag)
 when is_boolean(Flag) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:cast(?wxPrintDialogData_SetCollate,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Flag)):32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdialogdata.html#wxprintdialogdatasetfrompage">external documentation</a>.
-spec setFromPage(This, V) -> 'ok' when
	This::wxPrintDialogData(), V::integer().
setFromPage(#wx_ref{type=ThisT,ref=ThisRef},V)
 when is_integer(V) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:cast(?wxPrintDialogData_SetFromPage,
  <<ThisRef:32/?UI,V:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdialogdata.html#wxprintdialogdatasetmaxpage">external documentation</a>.
-spec setMaxPage(This, V) -> 'ok' when
	This::wxPrintDialogData(), V::integer().
setMaxPage(#wx_ref{type=ThisT,ref=ThisRef},V)
 when is_integer(V) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:cast(?wxPrintDialogData_SetMaxPage,
  <<ThisRef:32/?UI,V:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdialogdata.html#wxprintdialogdatasetminpage">external documentation</a>.
-spec setMinPage(This, V) -> 'ok' when
	This::wxPrintDialogData(), V::integer().
setMinPage(#wx_ref{type=ThisT,ref=ThisRef},V)
 when is_integer(V) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:cast(?wxPrintDialogData_SetMinPage,
  <<ThisRef:32/?UI,V:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdialogdata.html#wxprintdialogdatasetnocopies">external documentation</a>.
-spec setNoCopies(This, V) -> 'ok' when
	This::wxPrintDialogData(), V::integer().
setNoCopies(#wx_ref{type=ThisT,ref=ThisRef},V)
 when is_integer(V) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:cast(?wxPrintDialogData_SetNoCopies,
  <<ThisRef:32/?UI,V:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdialogdata.html#wxprintdialogdatasetprintdata">external documentation</a>.
-spec setPrintData(This, PrintData) -> 'ok' when
	This::wxPrintDialogData(), PrintData::wxPrintData:wxPrintData().
setPrintData(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=PrintDataT,ref=PrintDataRef}) ->
  ?CLASS(ThisT,wxPrintDialogData),
  ?CLASS(PrintDataT,wxPrintData),
  wxe_util:cast(?wxPrintDialogData_SetPrintData,
  <<ThisRef:32/?UI,PrintDataRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdialogdata.html#wxprintdialogdatasetprinttofile">external documentation</a>.
-spec setPrintToFile(This, Flag) -> 'ok' when
	This::wxPrintDialogData(), Flag::boolean().
setPrintToFile(#wx_ref{type=ThisT,ref=ThisRef},Flag)
 when is_boolean(Flag) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:cast(?wxPrintDialogData_SetPrintToFile,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Flag)):32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdialogdata.html#wxprintdialogdatasetselection">external documentation</a>.
-spec setSelection(This, Flag) -> 'ok' when
	This::wxPrintDialogData(), Flag::boolean().
setSelection(#wx_ref{type=ThisT,ref=ThisRef},Flag)
 when is_boolean(Flag) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:cast(?wxPrintDialogData_SetSelection,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Flag)):32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdialogdata.html#wxprintdialogdatasettopage">external documentation</a>.
-spec setToPage(This, V) -> 'ok' when
	This::wxPrintDialogData(), V::integer().
setToPage(#wx_ref{type=ThisT,ref=ThisRef},V)
 when is_integer(V) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:cast(?wxPrintDialogData_SetToPage,
  <<ThisRef:32/?UI,V:32/?UI>>).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxPrintDialogData()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxPrintDialogData),
  wxe_util:destroy(?DESTROY_OBJECT,Obj),
  ok.
