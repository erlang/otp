%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2016. All Rights Reserved.
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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellbooleditor.html">wxGridCellBoolEditor</a>.
%% <p>This class is derived (and can use functions) from:
%% <br />{@link wxGridCellEditor}
%% </p>
%% @type wxGridCellBoolEditor().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxGridCellBoolEditor).
-include("wxe.hrl").
-export([destroy/1,isTrueValue/1,new/0,useStringValues/0,useStringValues/1]).

%% inherited exports
-export([beginEdit/4,endEdit/4,handleReturn/2,isCreated/1,paintBackground/3,
  parent_class/1,reset/1,setSize/2,show/2,show/3,startingClick/1,startingKey/2]).

-export_type([wxGridCellBoolEditor/0]).
-compile([{nowarn_deprecated_function, {wxGridCellEditor,endEdit,4}},{nowarn_deprecated_function, {wxGridCellEditor,paintBackground,3}}]).

%% @hidden
parent_class(wxGridCellEditor) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxGridCellBoolEditor() :: wx:wx_object().
%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellbooleditor.html#wxgridcellbooleditorwxgridcellbooleditor">external documentation</a>.
-spec new() -> wxGridCellBoolEditor().
new() ->
  wxe_util:construct(?wxGridCellBoolEditor_new,
  <<>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellbooleditor.html#wxgridcellbooleditoristruevalue">external documentation</a>.
-spec isTrueValue(Value) -> boolean() when
	Value::unicode:chardata().
isTrueValue(Value)
 when ?is_chardata(Value) ->
  Value_UC = unicode:characters_to_binary([Value,0]),
  wxe_util:call(?wxGridCellBoolEditor_IsTrueValue,
  <<(byte_size(Value_UC)):32/?UI,(Value_UC)/binary, 0:(((8- ((4+byte_size(Value_UC)) band 16#7)) band 16#7))/unit:8>>).

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
  MOpts = fun({valueTrue, ValueTrue}, Acc) ->   ValueTrue_UC = unicode:characters_to_binary([ValueTrue,0]),[<<1:32/?UI,(byte_size(ValueTrue_UC)):32/?UI,(ValueTrue_UC)/binary, 0:(((8- ((0+byte_size(ValueTrue_UC)) band 16#7)) band 16#7))/unit:8>>|Acc];
          ({valueFalse, ValueFalse}, Acc) ->   ValueFalse_UC = unicode:characters_to_binary([ValueFalse,0]),[<<2:32/?UI,(byte_size(ValueFalse_UC)):32/?UI,(ValueFalse_UC)/binary, 0:(((8- ((0+byte_size(ValueFalse_UC)) band 16#7)) band 16#7))/unit:8>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxGridCellBoolEditor_UseStringValues,
  <<BinOpt/binary>>).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxGridCellBoolEditor()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxGridCellBoolEditor),
  wxe_util:destroy(?wxGridCellBoolEditor_destroy,Obj),
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
endEdit(This,Row,Col,Grid) -> wxGridCellEditor:endEdit(This,Row,Col,Grid).
%% @hidden
beginEdit(This,Row,Col,Grid) -> wxGridCellEditor:beginEdit(This,Row,Col,Grid).
%% @hidden
paintBackground(This,RectCell,Attr) -> wxGridCellEditor:paintBackground(This,RectCell,Attr).
%% @hidden
show(This,Show, Options) -> wxGridCellEditor:show(This,Show, Options).
%% @hidden
show(This,Show) -> wxGridCellEditor:show(This,Show).
%% @hidden
setSize(This,Rect) -> wxGridCellEditor:setSize(This,Rect).
%% @hidden
isCreated(This) -> wxGridCellEditor:isCreated(This).
