%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2024. All Rights Reserved.
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

-module(wxGridCellFloatEditor).
-moduledoc """
Functions for wxGridCellFloatEditor class

The editor for floating point numbers data.

See: `m:wxGridCellEditor`, `wxGridCellAutoWrapStringEditor` (not implemented in
wx), `m:wxGridCellBoolEditor`, `m:wxGridCellChoiceEditor`,
`wxGridCellEnumEditor` (not implemented in wx), `m:wxGridCellNumberEditor`,
`m:wxGridCellTextEditor`, `wxGridCellDateEditor` (not implemented in wx)

This class is derived (and can use functions) from: `m:wxGridCellEditor`

wxWidgets docs:
[wxGridCellFloatEditor](https://docs.wxwidgets.org/3.1/classwx_grid_cell_float_editor.html)
""".
-include("wxe.hrl").
-export([destroy/1,new/0,new/1,setParameters/2]).

%% inherited exports
-export([handleReturn/2,isCreated/1,parent_class/1,reset/1,setSize/2,show/2,
  show/3,startingClick/1,startingKey/2]).

-type wxGridCellFloatEditor() :: wx:wx_object().
-export_type([wxGridCellFloatEditor/0]).
%% @hidden
-doc false.
parent_class(wxGridCellEditor) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @equiv new([])
-spec new() -> wxGridCellFloatEditor().

new() ->
  new([]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellfloateditor.html#wxgridcellfloateditorwxgridcellfloateditor">external documentation</a>.
-doc "Float cell editor ctor.".
-spec new([Option]) -> wxGridCellFloatEditor() when
	Option :: {'width', integer()}
		 | {'precision', integer()}
		 | {'format', integer()}.
new(Options)
 when is_list(Options) ->
  MOpts = fun({width, _width} = Arg) -> Arg;
          ({precision, _precision} = Arg) -> Arg;
          ({format, _format} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Opts,?get_env(),?wxGridCellFloatEditor_new),
  wxe_util:rec(?wxGridCellFloatEditor_new).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellfloateditor.html#wxgridcellfloateditorsetparameters">external documentation</a>.
-doc """
The parameters string format is "width\[,precision[,format]]" where `format`
should be chosen between f|e|g|E|G (f is used by default)
""".
-spec setParameters(This, Params) -> 'ok' when
	This::wxGridCellFloatEditor(), Params::unicode:chardata().
setParameters(#wx_ref{type=ThisT}=This,Params)
 when ?is_chardata(Params) ->
  ?CLASS(ThisT,wxGridCellFloatEditor),
  Params_UC = unicode:characters_to_binary(Params),
  wxe_util:queue_cmd(This,Params_UC,?get_env(),?wxGridCellFloatEditor_SetParameters).

%% @doc Destroys this object, do not use object again
-doc "Destroys the object.".
-spec destroy(This::wxGridCellFloatEditor()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxGridCellFloatEditor),
  wxe_util:queue_cmd(Obj, ?get_env(), ?wxGridCellFloatEditor_destroy),
  ok.
 %% From wxGridCellEditor
%% @hidden
-doc false.
handleReturn(This,Event) -> wxGridCellEditor:handleReturn(This,Event).
%% @hidden
-doc false.
startingClick(This) -> wxGridCellEditor:startingClick(This).
%% @hidden
-doc false.
startingKey(This,Event) -> wxGridCellEditor:startingKey(This,Event).
%% @hidden
-doc false.
reset(This) -> wxGridCellEditor:reset(This).
%% @hidden
-doc false.
show(This,Show, Options) -> wxGridCellEditor:show(This,Show, Options).
%% @hidden
-doc false.
show(This,Show) -> wxGridCellEditor:show(This,Show).
%% @hidden
-doc false.
setSize(This,Rect) -> wxGridCellEditor:setSize(This,Rect).
%% @hidden
-doc false.
isCreated(This) -> wxGridCellEditor:isCreated(This).
