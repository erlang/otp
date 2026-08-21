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

-module(wxAcceleratorTable).
-moduledoc """
An accelerator table allows the application to specify a table of keyboard shortcuts for
menu or button commands.

The object ?wxNullAcceleratorTable is defined to be a table with no data, and is the
initial accelerator table for a window.

Example:

Remark: An accelerator takes precedence over normal processing and can be a convenient
way to program some event handling. For example, you can use an accelerator table to
enable a dialog with a multi-line text control to accept CTRL-Enter as meaning 'OK'.

Predefined objects (include wx.hrl): ?wxNullAcceleratorTable

See:
* `m:wxAcceleratorEntry`

* `wxWindow:setAcceleratorTable/2`

wxWidgets docs: [wxAcceleratorTable](https://docs.wxwidgets.org/3.2/classwx_accelerator_table.html)
""".
-include("wxe.hrl").
-export([destroy/1,isOk/1,new/0,new/2,ok/1]).

%% inherited exports
-export([parent_class/1]).

-type wxAcceleratorTable() :: wx:wx_object().
-export_type([wxAcceleratorTable/0]).
-doc false.
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc "Default ctor.".
-spec new() -> wxAcceleratorTable().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxAcceleratorTable_new_0),
  wxe_util:rec(?wxAcceleratorTable_new_0).

-doc "Initializes the accelerator table from an array of `m:wxAcceleratorEntry`.".
-spec new(N, Entries) -> wxAcceleratorTable() when
	N::integer(), Entries::[wxAcceleratorEntry:wxAcceleratorEntry()].
new(N,Entries)
 when is_integer(N),is_list(Entries) ->
 _ = [?CLASS(EntriesT,wxAcceleratorEntry) || #wx_ref{type=EntriesT} <- Entries],
  wxe_util:queue_cmd(N,Entries,?get_env(),?wxAcceleratorTable_new_2),
  wxe_util:rec(?wxAcceleratorTable_new_2).

-doc "Equivalent to: `isOk/1`".
-spec ok(This) -> boolean() when
	This::wxAcceleratorTable().

ok(This)
 when is_record(This, wx_ref) ->
  isOk(This).

-doc "Returns true if the accelerator table is valid.".
-spec isOk(This) -> boolean() when
	This::wxAcceleratorTable().
isOk(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAcceleratorTable),
  wxe_util:queue_cmd(This,?get_env(),?wxAcceleratorTable_IsOk),
  wxe_util:rec(?wxAcceleratorTable_IsOk).

-doc "Destroys the object".
-spec destroy(This::wxAcceleratorTable()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxAcceleratorTable),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
