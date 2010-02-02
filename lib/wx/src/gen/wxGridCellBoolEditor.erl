%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2010. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%% This file is generated DO NOT EDIT

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgridcellbooleditor.html">wxGridCellBoolEditor</a>.
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

%% @hidden
parent_class(wxGridCellEditor) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @spec () -> wxGridCellBoolEditor()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgridcellbooleditor.html#wxgridcellbooleditorwxgridcellbooleditor">external documentation</a>.
new() ->
  wxe_util:construct(?wxGridCellBoolEditor_new,
  <<>>).

%% @spec (Value::string()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgridcellbooleditor.html#wxgridcellbooleditoristruevalue">external documentation</a>.
isTrueValue(Value)
 when is_list(Value) ->
  Value_UC = unicode:characters_to_binary([Value,0]),
  wxe_util:call(?wxGridCellBoolEditor_IsTrueValue,
  <<(byte_size(Value_UC)):32/?UI,(Value_UC)/binary, 0:(((8- ((4+byte_size(Value_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec () -> ok
%% @equiv useStringValues([])
useStringValues() ->
  useStringValues([]).

%% @spec ([Option]) -> ok
%% Option = {valueTrue, string()} | {valueFalse, string()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgridcellbooleditor.html#wxgridcellbooleditorusestringvalues">external documentation</a>.
useStringValues(Options)
 when is_list(Options) ->
  MOpts = fun({valueTrue, ValueTrue}, Acc) ->   ValueTrue_UC = unicode:characters_to_binary([ValueTrue,0]),[<<1:32/?UI,(byte_size(ValueTrue_UC)):32/?UI,(ValueTrue_UC)/binary, 0:(((8- ((0+byte_size(ValueTrue_UC)) band 16#7)) band 16#7))/unit:8>>|Acc];
          ({valueFalse, ValueFalse}, Acc) ->   ValueFalse_UC = unicode:characters_to_binary([ValueFalse,0]),[<<2:32/?UI,(byte_size(ValueFalse_UC)):32/?UI,(ValueFalse_UC)/binary, 0:(((8- ((0+byte_size(ValueFalse_UC)) band 16#7)) band 16#7))/unit:8>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxGridCellBoolEditor_UseStringValues,
  <<BinOpt/binary>>).

%% @spec (This::wxGridCellBoolEditor()) -> ok
%% @doc Destroys this object, do not use object again
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
