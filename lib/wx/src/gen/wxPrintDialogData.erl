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

-module(wxPrintDialogData).
-moduledoc """
This class holds information related to the visual characteristics of `m:wxPrintDialog`.

It contains a `m:wxPrintData` object with underlying printing settings.

See:
* [Overview printing](https://docs.wxwidgets.org/3.2/overview_printing.html#overview_printing)

* `m:wxPrintDialog`

* [Overview cmndlg](https://docs.wxwidgets.org/3.2/overview_cmndlg.html#overview_cmndlg_print)

wxWidgets docs: [wxPrintDialogData](https://docs.wxwidgets.org/3.2/classwx_print_dialog_data.html)
""".
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
-doc false.
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc "Default constructor.".
-spec new() -> wxPrintDialogData().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxPrintDialogData_new_0),
  wxe_util:rec(?wxPrintDialogData_new_0).

-doc "Copy constructor.".
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

-doc """
Enables or disables the "Help" button.
""".
-spec enableHelp(This, Flag) -> 'ok' when
	This::wxPrintDialogData(), Flag::boolean().
enableHelp(#wx_ref{type=ThisT}=This,Flag)
 when is_boolean(Flag) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:queue_cmd(This,Flag,?get_env(),?wxPrintDialogData_EnableHelp).

-doc """
Enables or disables the "Page numbers" controls.
""".
-spec enablePageNumbers(This, Flag) -> 'ok' when
	This::wxPrintDialogData(), Flag::boolean().
enablePageNumbers(#wx_ref{type=ThisT}=This,Flag)
 when is_boolean(Flag) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:queue_cmd(This,Flag,?get_env(),?wxPrintDialogData_EnablePageNumbers).

-doc """
Enables or disables the "Print to file" checkbox.
""".
-spec enablePrintToFile(This, Flag) -> 'ok' when
	This::wxPrintDialogData(), Flag::boolean().
enablePrintToFile(#wx_ref{type=ThisT}=This,Flag)
 when is_boolean(Flag) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:queue_cmd(This,Flag,?get_env(),?wxPrintDialogData_EnablePrintToFile).

-doc """
Enables or disables the "Selection" radio button.
""".
-spec enableSelection(This, Flag) -> 'ok' when
	This::wxPrintDialogData(), Flag::boolean().
enableSelection(#wx_ref{type=ThisT}=This,Flag)
 when is_boolean(Flag) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:queue_cmd(This,Flag,?get_env(),?wxPrintDialogData_EnableSelection).

-doc "Returns true if the user requested that all pages be printed.".
-spec getAllPages(This) -> boolean() when
	This::wxPrintDialogData().
getAllPages(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintDialogData_GetAllPages),
  wxe_util:rec(?wxPrintDialogData_GetAllPages).

-doc "Returns true if the user requested that the document(s) be collated.".
-spec getCollate(This) -> boolean() when
	This::wxPrintDialogData().
getCollate(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintDialogData_GetCollate),
  wxe_util:rec(?wxPrintDialogData_GetCollate).

-doc "Returns the `from` page number, as entered by the user.".
-spec getFromPage(This) -> integer() when
	This::wxPrintDialogData().
getFromPage(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintDialogData_GetFromPage),
  wxe_util:rec(?wxPrintDialogData_GetFromPage).

-doc "Returns the `maximum` page number.".
-spec getMaxPage(This) -> integer() when
	This::wxPrintDialogData().
getMaxPage(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintDialogData_GetMaxPage),
  wxe_util:rec(?wxPrintDialogData_GetMaxPage).

-doc "Returns the `minimum` page number.".
-spec getMinPage(This) -> integer() when
	This::wxPrintDialogData().
getMinPage(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintDialogData_GetMinPage),
  wxe_util:rec(?wxPrintDialogData_GetMinPage).

-doc "Returns the number of copies requested by the user.".
-spec getNoCopies(This) -> integer() when
	This::wxPrintDialogData().
getNoCopies(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintDialogData_GetNoCopies),
  wxe_util:rec(?wxPrintDialogData_GetNoCopies).

-doc "Returns a reference to the internal `m:wxPrintData` object.".
-spec getPrintData(This) -> wxPrintData:wxPrintData() when
	This::wxPrintDialogData().
getPrintData(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintDialogData_GetPrintData),
  wxe_util:rec(?wxPrintDialogData_GetPrintData).

-doc "Returns true if the user has selected printing to a file.".
-spec getPrintToFile(This) -> boolean() when
	This::wxPrintDialogData().
getPrintToFile(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintDialogData_GetPrintToFile),
  wxe_util:rec(?wxPrintDialogData_GetPrintToFile).

-doc """
Returns true if the user requested that the selection be printed (where "selection" is a
concept specific to the application).
""".
-spec getSelection(This) -> boolean() when
	This::wxPrintDialogData().
getSelection(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintDialogData_GetSelection),
  wxe_util:rec(?wxPrintDialogData_GetSelection).

-doc """
Returns the `"print to"` page number, as entered by the user.
""".
-spec getToPage(This) -> integer() when
	This::wxPrintDialogData().
getToPage(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintDialogData_GetToPage),
  wxe_util:rec(?wxPrintDialogData_GetToPage).

-doc """
Returns true if the print data is valid for using in print dialogs.

This can return false on Windows if the current printer is not set, for example. On all
other platforms, it returns true.
""".
-spec isOk(This) -> boolean() when
	This::wxPrintDialogData().
isOk(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintDialogData_IsOk),
  wxe_util:rec(?wxPrintDialogData_IsOk).

-doc """
Sets the "Collate" checkbox to true or false.
""".
-spec setCollate(This, Flag) -> 'ok' when
	This::wxPrintDialogData(), Flag::boolean().
setCollate(#wx_ref{type=ThisT}=This,Flag)
 when is_boolean(Flag) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:queue_cmd(This,Flag,?get_env(),?wxPrintDialogData_SetCollate).

-doc "Sets the `from` page number.".
-spec setFromPage(This, Page) -> 'ok' when
	This::wxPrintDialogData(), Page::integer().
setFromPage(#wx_ref{type=ThisT}=This,Page)
 when is_integer(Page) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:queue_cmd(This,Page,?get_env(),?wxPrintDialogData_SetFromPage).

-doc "Sets the `maximum` page number.".
-spec setMaxPage(This, Page) -> 'ok' when
	This::wxPrintDialogData(), Page::integer().
setMaxPage(#wx_ref{type=ThisT}=This,Page)
 when is_integer(Page) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:queue_cmd(This,Page,?get_env(),?wxPrintDialogData_SetMaxPage).

-doc "Sets the `minimum` page number.".
-spec setMinPage(This, Page) -> 'ok' when
	This::wxPrintDialogData(), Page::integer().
setMinPage(#wx_ref{type=ThisT}=This,Page)
 when is_integer(Page) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:queue_cmd(This,Page,?get_env(),?wxPrintDialogData_SetMinPage).

-doc "Sets the default number of copies the user has requested to be printed out.".
-spec setNoCopies(This, N) -> 'ok' when
	This::wxPrintDialogData(), N::integer().
setNoCopies(#wx_ref{type=ThisT}=This,N)
 when is_integer(N) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:queue_cmd(This,N,?get_env(),?wxPrintDialogData_SetNoCopies).

-doc "Sets the internal `m:wxPrintData`.".
-spec setPrintData(This, PrintData) -> 'ok' when
	This::wxPrintDialogData(), PrintData::wxPrintData:wxPrintData().
setPrintData(#wx_ref{type=ThisT}=This,#wx_ref{type=PrintDataT}=PrintData) ->
  ?CLASS(ThisT,wxPrintDialogData),
  ?CLASS(PrintDataT,wxPrintData),
  wxe_util:queue_cmd(This,PrintData,?get_env(),?wxPrintDialogData_SetPrintData).

-doc """
Sets the "Print to file" checkbox to true or false.
""".
-spec setPrintToFile(This, Flag) -> 'ok' when
	This::wxPrintDialogData(), Flag::boolean().
setPrintToFile(#wx_ref{type=ThisT}=This,Flag)
 when is_boolean(Flag) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:queue_cmd(This,Flag,?get_env(),?wxPrintDialogData_SetPrintToFile).

-doc """
Selects the "Selection" radio button.

The effect of printing the selection depends on how the application implements this
command, if at all.
""".
-spec setSelection(This, Flag) -> 'ok' when
	This::wxPrintDialogData(), Flag::boolean().
setSelection(#wx_ref{type=ThisT}=This,Flag)
 when is_boolean(Flag) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:queue_cmd(This,Flag,?get_env(),?wxPrintDialogData_SetSelection).

-doc """
Sets the `"print to"` page number.
""".
-spec setToPage(This, Page) -> 'ok' when
	This::wxPrintDialogData(), Page::integer().
setToPage(#wx_ref{type=ThisT}=This,Page)
 when is_integer(Page) ->
  ?CLASS(ThisT,wxPrintDialogData),
  wxe_util:queue_cmd(This,Page,?get_env(),?wxPrintDialogData_SetToPage).

-doc "Destroys the object".
-spec destroy(This::wxPrintDialogData()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxPrintDialogData),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
