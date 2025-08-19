%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0 AND LicenseRef-scancode-wxwindows-free-doc-3
%%
%% Copyright Ericsson AB 2009-2025. All Rights Reserved.
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

-module(wxGridCellBoolEditor).
-moduledoc """
Grid cell editor for boolean data.

See:
* `m:wxGridCellEditor`

* `m:wxGridCellChoiceEditor`

* `m:wxGridCellFloatEditor`

* `m:wxGridCellNumberEditor`

* `m:wxGridCellTextEditor`

This class is derived, and can use functions, from:

* `m:wxGridCellEditor`

wxWidgets docs: [wxGridCellBoolEditor](https://docs.wxwidgets.org/3.2/classwx_grid_cell_bool_editor.html)
""".
-include("wxe.hrl").
-export([destroy/1,isTrueValue/1,new/0,useStringValues/0,useStringValues/1]).

%% inherited exports
-export([handleReturn/2,isCreated/1,parent_class/1,reset/1,setSize/2,show/2,
  show/3,startingClick/1,startingKey/2]).

-type wxGridCellBoolEditor() :: wx:wx_object().
-export_type([wxGridCellBoolEditor/0]).
-doc false.
parent_class(wxGridCellEditor) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc "Default constructor.".
-spec new() -> wxGridCellBoolEditor().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxGridCellBoolEditor_new),
  wxe_util:rec(?wxGridCellBoolEditor_new).

-doc """
Returns true if the given `value` is equal to the string representation of the truth
value we currently use (see `useStringValues/1`).
""".
-spec isTrueValue(Value) -> boolean() when
	Value::unicode:chardata().
isTrueValue(Value)
 when ?is_chardata(Value) ->
  Value_UC = unicode:characters_to_binary(Value),
  wxe_util:queue_cmd(Value_UC,?get_env(),?wxGridCellBoolEditor_IsTrueValue),
  wxe_util:rec(?wxGridCellBoolEditor_IsTrueValue).

-doc(#{equiv => useStringValues([])}).
-spec useStringValues() -> 'ok'.

useStringValues() ->
  useStringValues([]).

-doc """
This method allows you to customize the values returned by `wxGridCellNumberEditor:getValue/1`
for the cell using this editor.

By default, the default values of the arguments are used, i.e. `"1"` is returned if the
cell is checked and an empty string otherwise.
""".
-spec useStringValues([Option]) -> 'ok' when
	Option :: {'valueTrue', unicode:chardata()}
		 | {'valueFalse', unicode:chardata()}.
useStringValues(Options)
 when is_list(Options) ->
  MOpts = fun({valueTrue, ValueTrue}) ->   ValueTrue_UC = unicode:characters_to_binary(ValueTrue),{valueTrue,ValueTrue_UC};
          ({valueFalse, ValueFalse}) ->   ValueFalse_UC = unicode:characters_to_binary(ValueFalse),{valueFalse,ValueFalse_UC};
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Opts,?get_env(),?wxGridCellBoolEditor_UseStringValues).

-doc "Destroys the object".
-spec destroy(This::wxGridCellBoolEditor()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxGridCellBoolEditor),
  wxe_util:queue_cmd(Obj, ?get_env(), ?wxGridCellBoolEditor_destroy),
  ok.
 %% From wxGridCellEditor
-doc false.
handleReturn(This,Event) -> wxGridCellEditor:handleReturn(This,Event).
-doc false.
startingClick(This) -> wxGridCellEditor:startingClick(This).
-doc false.
startingKey(This,Event) -> wxGridCellEditor:startingKey(This,Event).
-doc false.
reset(This) -> wxGridCellEditor:reset(This).
-doc false.
show(This,Show, Options) -> wxGridCellEditor:show(This,Show, Options).
-doc false.
show(This,Show) -> wxGridCellEditor:show(This,Show).
-doc false.
setSize(This,Rect) -> wxGridCellEditor:setSize(This,Rect).
-doc false.
isCreated(This) -> wxGridCellEditor:isCreated(This).
