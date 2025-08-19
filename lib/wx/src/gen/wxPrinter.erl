%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0 AND LicenseRef-scancode-wxwindows-free-doc-3
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
%% For documentation, wxWindow Free Documentation License, Version 3 applies.
%% wxWindows Free Documentation Licence, Version 3, as follows.
%% ===============================================
%%
%% Everyone is permitted to copy and distribute verbatim copies
%% of this licence document, but changing it is not allowed.
%%
%%                  WXWINDOWS FREE DOCUMENTATION LICENCE
%%    TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION
%%
%% 1. Permission is granted to make and distribute verbatim copies of this
%% manual or piece of documentation provided any copyright notice and this
%% permission notice are preserved on all copies.
%%
%% 2. Permission is granted to process this file or document through a
%% document processing system and, at your option and the option of any third
%% party, print the results, provided a printed document carries a copying
%% permission notice identical to this one.
%%
%% 3. Permission is granted to copy and distribute modified versions of this
%% manual or piece of documentation under the conditions for verbatim copying,
%% provided also that any sections describing licensing conditions for this
%% manual, such as, in particular, the GNU General Public Licence, the GNU
%% Library General Public Licence, and any wxWindows Licence are included
%% exactly as in the original, and provided that the entire resulting derived
%% work is distributed under the terms of a permission notice identical to
%% this one.
%%
%% 4. Permission is granted to copy and distribute translations of this manual
%% or piece of documentation into another language, under the above conditions
%% for modified versions, except that sections related to licensing, including
%% this paragraph, may also be included in translations approved by the
%% copyright holders of the respective licence documents in addition to the
%% original English.
%%
%% %CopyrightEnd%
%% This file is generated DO NOT EDIT

-module(wxPrinter).
-moduledoc """
This class represents the Windows or PostScript printer, and is the vehicle through which
printing may be launched by an application.

Printing can also be achieved through using of lower functions and classes, but this and
associated classes provide a more convenient and general method of printing.

See:
* [Overview printing](https://docs.wxwidgets.org/3.2/overview_printing.html#overview_printing)

* `m:wxPrintDialog`

* `m:wxPrintout`

* `m:wxPrintPreview`

wxWidgets docs: [wxPrinter](https://docs.wxwidgets.org/3.2/classwx_printer.html)
""".
-include("wxe.hrl").
-export([createAbortWindow/3,destroy/1,getAbort/1,getLastError/0,getPrintDialogData/1,
  new/0,new/1,print/3,print/4,printDialog/2,reportError/4,setup/2]).

%% inherited exports
-export([parent_class/1]).

-type wxPrinter() :: wx:wx_object().
-export_type([wxPrinter/0]).
-doc false.
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc(#{equiv => new([])}).
-spec new() -> wxPrinter().

new() ->
  new([]).

-doc """
Constructor.

Pass an optional pointer to a block of print dialog data, which will be copied to the
printer object's local data.

See:
* `m:wxPrintDialogData`

* `m:wxPrintData`
""".
-spec new([Option]) -> wxPrinter() when
	Option :: {'data', wxPrintDialogData:wxPrintDialogData()}.
new(Options)
 when is_list(Options) ->
  MOpts = fun({data, #wx_ref{type=DataT}} = Arg) ->   ?CLASS(DataT,wxPrintDialogData),Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Opts,?get_env(),?wxPrinter_new),
  wxe_util:rec(?wxPrinter_new).

-doc "Creates the default printing abort window, with a cancel button.".
-spec createAbortWindow(This, Parent, Printout) -> wxDialog:wxDialog() when
	This::wxPrinter(), Parent::wxWindow:wxWindow(), Printout::wxPrintout:wxPrintout().
createAbortWindow(#wx_ref{type=ThisT}=This,#wx_ref{type=ParentT}=Parent,#wx_ref{type=PrintoutT}=Printout) ->
  ?CLASS(ThisT,wxPrinter),
  ?CLASS(ParentT,wxWindow),
  ?CLASS(PrintoutT,wxPrintout),
  wxe_util:queue_cmd(This,Parent,Printout,?get_env(),?wxPrinter_CreateAbortWindow),
  wxe_util:rec(?wxPrinter_CreateAbortWindow).

-doc "Returns true if the user has aborted the print job.".
-spec getAbort(This) -> boolean() when
	This::wxPrinter().
getAbort(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrinter),
  wxe_util:queue_cmd(This,?get_env(),?wxPrinter_GetAbort),
  wxe_util:rec(?wxPrinter_GetAbort).

-doc """
Return last error.

Valid after calling `print/4`, `printDialog/2` or `wxPrintPreview:print/2`.

These functions set last error to `wxPRINTER_NO_ERROR` if no error happened.

Returned value is one of the following:
""".
%%  Res = ?wxPRINTER_NO_ERROR | ?wxPRINTER_CANCELLED | ?wxPRINTER_ERROR
-spec getLastError() -> wx:wx_enum().
getLastError() ->
  wxe_util:queue_cmd(?get_env(), ?wxPrinter_GetLastError),
  wxe_util:rec(?wxPrinter_GetLastError).

-doc "Returns the print data associated with the printer object.".
-spec getPrintDialogData(This) -> wxPrintDialogData:wxPrintDialogData() when
	This::wxPrinter().
getPrintDialogData(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrinter),
  wxe_util:queue_cmd(This,?get_env(),?wxPrinter_GetPrintDialogData),
  wxe_util:rec(?wxPrinter_GetPrintDialogData).

-doc(#{equiv => print(This,Parent,Printout, [])}).
-spec print(This, Parent, Printout) -> boolean() when
	This::wxPrinter(), Parent::wxWindow:wxWindow(), Printout::wxPrintout:wxPrintout().

print(This,Parent,Printout)
 when is_record(This, wx_ref),is_record(Parent, wx_ref),is_record(Printout, wx_ref) ->
  print(This,Parent,Printout, []).

-doc """
Starts the printing process.

Provide a parent window, a user-defined `m:wxPrintout` object which controls the printing
of a document, and whether the print dialog should be invoked first.

`print/4` could return false if there was a problem initializing the printer device context
(current printer not set, for example) or the user cancelled printing. Call `getLastError/0` to get
detailed information about the kind of the error.
""".
-spec print(This, Parent, Printout, [Option]) -> boolean() when
	This::wxPrinter(), Parent::wxWindow:wxWindow(), Printout::wxPrintout:wxPrintout(),
	Option :: {'prompt', boolean()}.
print(#wx_ref{type=ThisT}=This,#wx_ref{type=ParentT}=Parent,#wx_ref{type=PrintoutT}=Printout, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxPrinter),
  ?CLASS(ParentT,wxWindow),
  ?CLASS(PrintoutT,wxPrintout),
  MOpts = fun({prompt, _prompt} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Parent,Printout, Opts,?get_env(),?wxPrinter_Print),
  wxe_util:rec(?wxPrinter_Print).

-doc """
Invokes the print dialog.

If successful (the user did not press Cancel and no error occurred), a suitable device
context will be returned; otherwise NULL is returned; call `getLastError/0` to get detailed information
about the kind of the error.

Remark: The application must delete this device context to avoid a memory leak.
""".
-spec printDialog(This, Parent) -> wxDC:wxDC() when
	This::wxPrinter(), Parent::wxWindow:wxWindow().
printDialog(#wx_ref{type=ThisT}=This,#wx_ref{type=ParentT}=Parent) ->
  ?CLASS(ThisT,wxPrinter),
  ?CLASS(ParentT,wxWindow),
  wxe_util:queue_cmd(This,Parent,?get_env(),?wxPrinter_PrintDialog),
  wxe_util:rec(?wxPrinter_PrintDialog).

-doc "Default error-reporting function.".
-spec reportError(This, Parent, Printout, Message) -> 'ok' when
	This::wxPrinter(), Parent::wxWindow:wxWindow(), Printout::wxPrintout:wxPrintout(), Message::unicode:chardata().
reportError(#wx_ref{type=ThisT}=This,#wx_ref{type=ParentT}=Parent,#wx_ref{type=PrintoutT}=Printout,Message)
 when ?is_chardata(Message) ->
  ?CLASS(ThisT,wxPrinter),
  ?CLASS(ParentT,wxWindow),
  ?CLASS(PrintoutT,wxPrintout),
  Message_UC = unicode:characters_to_binary(Message),
  wxe_util:queue_cmd(This,Parent,Printout,Message_UC,?get_env(),?wxPrinter_ReportError).

-doc """
Invokes the print setup dialog.

Deprecated:

The setup dialog is obsolete, though retained for backward compatibility.
""".
-spec setup(This, Parent) -> boolean() when
	This::wxPrinter(), Parent::wxWindow:wxWindow().
setup(#wx_ref{type=ThisT}=This,#wx_ref{type=ParentT}=Parent) ->
  ?CLASS(ThisT,wxPrinter),
  ?CLASS(ParentT,wxWindow),
  wxe_util:queue_cmd(This,Parent,?get_env(),?wxPrinter_Setup),
  wxe_util:rec(?wxPrinter_Setup).

-doc "Destroys the object".
-spec destroy(This::wxPrinter()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxPrinter),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
