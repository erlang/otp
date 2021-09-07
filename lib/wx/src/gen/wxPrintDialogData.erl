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

-module(wxPrintDialogData).
-include("wxe.hrl").
-export([destroy/1,enableHelp/2,enablePageNumbers/2,enablePrintToFile/2,enableSelection/2,
  getAllPages/1,getCollate/1,getFromPage/1,getMaxPage/1,getMinPage/1,
  getNoCopies/1,getPrintData/1,getPrintToFile/1,getSelection/1,getToPage/1,
  isOk/1,new/0,new/1,setCollate/2,setFromPage/2,setMaxPage/2,setMinPage/2,
  setNoCopies/2,setPrintData/2,setPrintToFile/2,setSelection/2,setToPage/2]).

%% inherited exports
-export([parent_class/1]).

-type wxPrintDialogData() :: wx:wx_object().
-export_type([wxPrintDialogData/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdialogdata.html#wxprintdialogdatawxprintdialogdata">external documentation</a>.
-spec new() -> wxPrintDialogData().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxPrintDialogData_new_0),
  wxe_util:rec(?wxPrintDialogData_new_0).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdialogdata.html#wxprintdialogdatawxprintdialogdata">external documentation</a>.
-spec new(DialogData) -> wxPrintDialogData() when
	DialogData::wxPrintDialogData:wxPrintDialogData() | wxPrintData:wxPrintData().
new(#wx_ref{type=DialogDataT}=DialogData) ->
  IswxPrintDialogData = ?CLASS_T(DialogDataT,wxPrintDialogData),
  IswxPrintData = ?CLASS_T(DialogDataT,wxPrintData),
  DialogDataType = if
    IswxPrintDialogData ->   wxPrintDialogData;
    IswxPrintData ->   wxPrintData;
    true -> error({badarg, DialogDataT})
  end,
  wxe_util:queue_cmd(wx:typeCast(DialogData, DialogDataType),?get_env(),?wxPrintDialogData_new_1),
  wxe_util:rec(?wxPrintDialogData_new_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdialogdata.html#wxprintdialogdataenablehelp">external documentation</a>.
-spec enableHelp(This, Flag) -> 'ok' when
	This::wxPrintDialogData(), Flag::boolean().
enableHelp(#wx_ref{type=ThisT}=This,Flag)
 when is_boolean(Flag) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:queue_cmd(This,Flag,?get_env(),?wxPrintDialogData_EnableHelp).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdialogdata.html#wxprintdialogdataenablepagenumbers">external documentation</a>.
-spec enablePageNumbers(This, Flag) -> 'ok' when
	This::wxPrintDialogData(), Flag::boolean().
enablePageNumbers(#wx_ref{type=ThisT}=This,Flag)
 when is_boolean(Flag) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:queue_cmd(This,Flag,?get_env(),?wxPrintDialogData_EnablePageNumbers).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdialogdata.html#wxprintdialogdataenableprinttofile">external documentation</a>.
-spec enablePrintToFile(This, Flag) -> 'ok' when
	This::wxPrintDialogData(), Flag::boolean().
enablePrintToFile(#wx_ref{type=ThisT}=This,Flag)
 when is_boolean(Flag) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:queue_cmd(This,Flag,?get_env(),?wxPrintDialogData_EnablePrintToFile).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdialogdata.html#wxprintdialogdataenableselection">external documentation</a>.
-spec enableSelection(This, Flag) -> 'ok' when
	This::wxPrintDialogData(), Flag::boolean().
enableSelection(#wx_ref{type=ThisT}=This,Flag)
 when is_boolean(Flag) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:queue_cmd(This,Flag,?get_env(),?wxPrintDialogData_EnableSelection).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdialogdata.html#wxprintdialogdatagetallpages">external documentation</a>.
-spec getAllPages(This) -> boolean() when
	This::wxPrintDialogData().
getAllPages(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintDialogData_GetAllPages),
  wxe_util:rec(?wxPrintDialogData_GetAllPages).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdialogdata.html#wxprintdialogdatagetcollate">external documentation</a>.
-spec getCollate(This) -> boolean() when
	This::wxPrintDialogData().
getCollate(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintDialogData_GetCollate),
  wxe_util:rec(?wxPrintDialogData_GetCollate).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdialogdata.html#wxprintdialogdatagetfrompage">external documentation</a>.
-spec getFromPage(This) -> integer() when
	This::wxPrintDialogData().
getFromPage(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintDialogData_GetFromPage),
  wxe_util:rec(?wxPrintDialogData_GetFromPage).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdialogdata.html#wxprintdialogdatagetmaxpage">external documentation</a>.
-spec getMaxPage(This) -> integer() when
	This::wxPrintDialogData().
getMaxPage(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintDialogData_GetMaxPage),
  wxe_util:rec(?wxPrintDialogData_GetMaxPage).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdialogdata.html#wxprintdialogdatagetminpage">external documentation</a>.
-spec getMinPage(This) -> integer() when
	This::wxPrintDialogData().
getMinPage(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintDialogData_GetMinPage),
  wxe_util:rec(?wxPrintDialogData_GetMinPage).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdialogdata.html#wxprintdialogdatagetnocopies">external documentation</a>.
-spec getNoCopies(This) -> integer() when
	This::wxPrintDialogData().
getNoCopies(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintDialogData_GetNoCopies),
  wxe_util:rec(?wxPrintDialogData_GetNoCopies).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdialogdata.html#wxprintdialogdatagetprintdata">external documentation</a>.
-spec getPrintData(This) -> wxPrintData:wxPrintData() when
	This::wxPrintDialogData().
getPrintData(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintDialogData_GetPrintData),
  wxe_util:rec(?wxPrintDialogData_GetPrintData).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdialogdata.html#wxprintdialogdatagetprinttofile">external documentation</a>.
-spec getPrintToFile(This) -> boolean() when
	This::wxPrintDialogData().
getPrintToFile(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintDialogData_GetPrintToFile),
  wxe_util:rec(?wxPrintDialogData_GetPrintToFile).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdialogdata.html#wxprintdialogdatagetselection">external documentation</a>.
-spec getSelection(This) -> boolean() when
	This::wxPrintDialogData().
getSelection(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintDialogData_GetSelection),
  wxe_util:rec(?wxPrintDialogData_GetSelection).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdialogdata.html#wxprintdialogdatagettopage">external documentation</a>.
-spec getToPage(This) -> integer() when
	This::wxPrintDialogData().
getToPage(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintDialogData_GetToPage),
  wxe_util:rec(?wxPrintDialogData_GetToPage).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdialogdata.html#wxprintdialogdataisok">external documentation</a>.
-spec isOk(This) -> boolean() when
	This::wxPrintDialogData().
isOk(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintDialogData_IsOk),
  wxe_util:rec(?wxPrintDialogData_IsOk).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdialogdata.html#wxprintdialogdatasetcollate">external documentation</a>.
-spec setCollate(This, Flag) -> 'ok' when
	This::wxPrintDialogData(), Flag::boolean().
setCollate(#wx_ref{type=ThisT}=This,Flag)
 when is_boolean(Flag) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:queue_cmd(This,Flag,?get_env(),?wxPrintDialogData_SetCollate).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdialogdata.html#wxprintdialogdatasetfrompage">external documentation</a>.
-spec setFromPage(This, Page) -> 'ok' when
	This::wxPrintDialogData(), Page::integer().
setFromPage(#wx_ref{type=ThisT}=This,Page)
 when is_integer(Page) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:queue_cmd(This,Page,?get_env(),?wxPrintDialogData_SetFromPage).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdialogdata.html#wxprintdialogdatasetmaxpage">external documentation</a>.
-spec setMaxPage(This, Page) -> 'ok' when
	This::wxPrintDialogData(), Page::integer().
setMaxPage(#wx_ref{type=ThisT}=This,Page)
 when is_integer(Page) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:queue_cmd(This,Page,?get_env(),?wxPrintDialogData_SetMaxPage).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdialogdata.html#wxprintdialogdatasetminpage">external documentation</a>.
-spec setMinPage(This, Page) -> 'ok' when
	This::wxPrintDialogData(), Page::integer().
setMinPage(#wx_ref{type=ThisT}=This,Page)
 when is_integer(Page) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:queue_cmd(This,Page,?get_env(),?wxPrintDialogData_SetMinPage).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdialogdata.html#wxprintdialogdatasetnocopies">external documentation</a>.
-spec setNoCopies(This, N) -> 'ok' when
	This::wxPrintDialogData(), N::integer().
setNoCopies(#wx_ref{type=ThisT}=This,N)
 when is_integer(N) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:queue_cmd(This,N,?get_env(),?wxPrintDialogData_SetNoCopies).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdialogdata.html#wxprintdialogdatasetprintdata">external documentation</a>.
-spec setPrintData(This, PrintData) -> 'ok' when
	This::wxPrintDialogData(), PrintData::wxPrintData:wxPrintData().
setPrintData(#wx_ref{type=ThisT}=This,#wx_ref{type=PrintDataT}=PrintData) ->
  ?CLASS(ThisT,wxPrintDialogData),
  ?CLASS(PrintDataT,wxPrintData),
  wxe_util:queue_cmd(This,PrintData,?get_env(),?wxPrintDialogData_SetPrintData).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdialogdata.html#wxprintdialogdatasetprinttofile">external documentation</a>.
-spec setPrintToFile(This, Flag) -> 'ok' when
	This::wxPrintDialogData(), Flag::boolean().
setPrintToFile(#wx_ref{type=ThisT}=This,Flag)
 when is_boolean(Flag) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:queue_cmd(This,Flag,?get_env(),?wxPrintDialogData_SetPrintToFile).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdialogdata.html#wxprintdialogdatasetselection">external documentation</a>.
-spec setSelection(This, Flag) -> 'ok' when
	This::wxPrintDialogData(), Flag::boolean().
setSelection(#wx_ref{type=ThisT}=This,Flag)
 when is_boolean(Flag) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:queue_cmd(This,Flag,?get_env(),?wxPrintDialogData_SetSelection).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintdialogdata.html#wxprintdialogdatasettopage">external documentation</a>.
-spec setToPage(This, Page) -> 'ok' when
	This::wxPrintDialogData(), Page::integer().
setToPage(#wx_ref{type=ThisT}=This,Page)
 when is_integer(Page) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:queue_cmd(This,Page,?get_env(),?wxPrintDialogData_SetToPage).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxPrintDialogData()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxPrintDialogData),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
