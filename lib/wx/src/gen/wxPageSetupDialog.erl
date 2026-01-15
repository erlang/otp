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

-module(wxPageSetupDialog).
-moduledoc """
This class represents the page setup common dialog.

The page setup dialog contains controls for paper size (letter, A4, A5 etc.), orientation
(landscape or portrait), and, only under Windows currently, controls for setting left,
top, right and bottom margin sizes in millimetres.

The exact appearance of this dialog varies among the platforms as a native dialog is used
when available (currently the case for all major platforms).

When the dialog has been closed, you need to query the `m:wxPageSetupDialogData` object
associated with the dialog.

Note that the OK and Cancel buttons do not destroy the dialog; this must be done by the application.

See:
* [Overview printing](https://docs.wxwidgets.org/3.2/overview_printing.html#overview_printing)

* `m:wxPrintDialog`

* `m:wxPageSetupDialogData`

wxWidgets docs: [wxPageSetupDialog](https://docs.wxwidgets.org/3.2/classwx_page_setup_dialog.html)
""".
-include("wxe.hrl").
-export([destroy/1,getPageSetupData/1,new/1,new/2,showModal/1]).

%% inherited exports
-export([parent_class/1]).

-type wxPageSetupDialog() :: wx:wx_object().
-export_type([wxPageSetupDialog/0]).
-doc false.
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc(#{equiv => new(Parent, [])}).
-spec new(Parent) -> wxPageSetupDialog() when
	Parent::wxWindow:wxWindow().

new(Parent)
 when is_record(Parent, wx_ref) ->
  new(Parent, []).

-doc """
Constructor.

Pass a parent window, and optionally a pointer to a block of page setup data, which will
be copied to the print dialog's internal data.
""".
-spec new(Parent, [Option]) -> wxPageSetupDialog() when
	Parent::wxWindow:wxWindow(),
	Option :: {'data', wxPageSetupDialogData:wxPageSetupDialogData()}.
new(#wx_ref{type=ParentT}=Parent, Options)
 when is_list(Options) ->
  ?CLASS(ParentT,wxWindow),
  MOpts = fun({data, #wx_ref{type=DataT}} = Arg) ->   ?CLASS(DataT,wxPageSetupDialogData),Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Parent, Opts,?get_env(),?wxPageSetupDialog_new),
  wxe_util:rec(?wxPageSetupDialog_new).

-doc "Returns the `m:wxPageSetupDialogData` object associated with the dialog.".
-spec getPageSetupData(This) -> wxPageSetupDialogData:wxPageSetupDialogData() when
	This::wxPageSetupDialog().
getPageSetupData(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPageSetupDialog),
  wxe_util:queue_cmd(This,?get_env(),?wxPageSetupDialog_GetPageSetupData),
  wxe_util:rec(?wxPageSetupDialog_GetPageSetupData).

-doc """
Shows the dialog, returning `wxID\_OK` if the user pressed OK, and `wxID\_CANCEL`
otherwise.
""".
-spec showModal(This) -> integer() when
	This::wxPageSetupDialog().
showModal(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPageSetupDialog),
  wxe_util:queue_cmd(This,?get_env(),?wxPageSetupDialog_ShowModal),
  wxe_util:rec(?wxPageSetupDialog_ShowModal).

-doc "Destroys the object".
-spec destroy(This::wxPageSetupDialog()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxPageSetupDialog),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
