%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2020. All Rights Reserved.
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

-module(wxGridCellBoolEditor).
-include("wxe.hrl").
-export([destroy/1,isTrueValue/1,new/0,useStringValues/0,useStringValues/1]).

%% inherited exports
-export([handleReturn/2,isCreated/1,parent_class/1,reset/1,setSize/2,show/2,
  show/3,startingClick/1,startingKey/2]).

-type wxGridCellBoolEditor() :: wx:wx_object().
-export_type([wxGridCellBoolEditor/0]).
%% @hidden
parent_class(wxGridCellEditor) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellbooleditor.html#wxgridcellbooleditorwxgridcellbooleditor">external documentation</a>.
-spec new() -> wxGridCellBoolEditor().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxGridCellBoolEditor_new),
  wxe_util:rec(?wxGridCellBoolEditor_new).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellbooleditor.html#wxgridcellbooleditoristruevalue">external documentation</a>.
-spec isTrueValue(Value) -> boolean() when
	Value::unicode:chardata().
isTrueValue(Value)
 when ?is_chardata(Value) ->
  Value_UC = unicode:characters_to_binary(Value),
  wxe_util:queue_cmd(Value_UC,?get_env(),?wxGridCellBoolEditor_IsTrueValue),
  wxe_util:rec(?wxGridCellBoolEditor_IsTrueValue).

%% @equiv useStringValues([])
-spec useStringValues() -> 'ok'.

useStringValues() ->
  useStringValues([]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellbooleditor.html#wxgridcellbooleditorusestringvalues">external documentation</a>.
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

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxGridCellBoolEditor()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxGridCellBoolEditor),
  wxe_util:queue_cmd(Obj, ?get_env(), ?wxGridCellBoolEditor_destroy),
  ok.
 %% From wxGridCellEditor
%% @hidden
handleReturn(This,Event) -> wxGridCellEditor:handleReturn(This,Event).
%% @hidden
startingClick(This) -> wxGridCellEditor:startingClick(This).
%% @hidden
startingKey(This,Event) -> wxGridCellEditor:startingKey(This,Event).
%% @hidden
reset(This) -> wxGridCellEditor:reset(This).
%% @hidden
show(This,Show, Options) -> wxGridCellEditor:show(This,Show, Options).
%% @hidden
show(This,Show) -> wxGridCellEditor:show(This,Show).
%% @hidden
setSize(This,Rect) -> wxGridCellEditor:setSize(This,Rect).
%% @hidden
isCreated(This) -> wxGridCellEditor:isCreated(This).
