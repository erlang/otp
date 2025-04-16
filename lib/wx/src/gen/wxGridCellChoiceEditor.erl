%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
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
%% %CopyrightEnd%
%% This file is generated DO NOT EDIT

-module(wxGridCellChoiceEditor).
-moduledoc """
Grid cell editor for string data providing the user a choice from a list of strings.

See:
* `m:wxGridCellEditor`

* `m:wxGridCellBoolEditor`

* `m:wxGridCellFloatEditor`

* `m:wxGridCellNumberEditor`

* `m:wxGridCellTextEditor`

This class is derived, and can use functions, from:

* `m:wxGridCellEditor`

wxWidgets docs: [wxGridCellChoiceEditor](https://docs.wxwidgets.org/3.2/classwx_grid_cell_choice_editor.html)
""".
-include("wxe.hrl").
-export([destroy/1,new/1,new/2,setParameters/2]).

%% inherited exports
-export([handleReturn/2,isCreated/1,parent_class/1,reset/1,setSize/2,show/2,
  show/3,startingClick/1,startingKey/2]).

-type wxGridCellChoiceEditor() :: wx:wx_object().
-export_type([wxGridCellChoiceEditor/0]).
-doc false.
parent_class(wxGridCellEditor) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc(#{equiv => new(Choices, [])}).
-spec new(Choices) -> wxGridCellChoiceEditor() when
	Choices::[unicode:chardata()].

new(Choices)
 when is_list(Choices) ->
  new(Choices, []).

-doc "Choice cell renderer ctor.".
-spec new(Choices, [Option]) -> wxGridCellChoiceEditor() when
	Choices::[unicode:chardata()],
	Option :: {'allowOthers', boolean()}.
new(Choices, Options)
 when is_list(Choices),is_list(Options) ->
  Choices_UCA = [unicode:characters_to_binary(ChoicesTemp) ||
              ChoicesTemp <- Choices],
  MOpts = fun({allowOthers, _allowOthers} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Choices_UCA, Opts,?get_env(),?wxGridCellChoiceEditor_new),
  wxe_util:rec(?wxGridCellChoiceEditor_new).

-doc """
Parameters string format is "item1[,item2[...,itemN]]".

This method can be called before the editor is used for the first time, or later, in
which case it replaces the previously specified strings with the new ones.
""".
-spec setParameters(This, Params) -> 'ok' when
	This::wxGridCellChoiceEditor(), Params::unicode:chardata().
setParameters(#wx_ref{type=ThisT}=This,Params)
 when ?is_chardata(Params) ->
  ?CLASS(ThisT,wxGridCellChoiceEditor),
  Params_UC = unicode:characters_to_binary(Params),
  wxe_util:queue_cmd(This,Params_UC,?get_env(),?wxGridCellChoiceEditor_SetParameters).

-doc "Destroys the object".
-spec destroy(This::wxGridCellChoiceEditor()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxGridCellChoiceEditor),
  wxe_util:queue_cmd(Obj, ?get_env(), ?wxGridCellChoiceEditor_destroy),
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
