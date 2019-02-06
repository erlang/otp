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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellfloateditor.html">wxGridCellFloatEditor</a>.
%% <p>This class is derived (and can use functions) from:
%% <br />{@link wxGridCellEditor}
%% </p>
%% @type wxGridCellFloatEditor().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxGridCellFloatEditor).
-include("wxe.hrl").
-export([destroy/1,new/0,new/1,setParameters/2]).

%% inherited exports
-export([beginEdit/4,endEdit/4,handleReturn/2,isCreated/1,paintBackground/3,
  parent_class/1,reset/1,setSize/2,show/2,show/3,startingClick/1,startingKey/2]).

-export_type([wxGridCellFloatEditor/0]).
-compile([{nowarn_deprecated_function, {wxGridCellEditor,endEdit,4}},{nowarn_deprecated_function, {wxGridCellEditor,paintBackground,3}}]).

%% @hidden
parent_class(wxGridCellEditor) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxGridCellFloatEditor() :: wx:wx_object().
%% @equiv new([])
-spec new() -> wxGridCellFloatEditor().

new() ->
  new([]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellfloateditor.html#wxgridcellfloateditorwxgridcellfloateditor">external documentation</a>.
-spec new([Option]) -> wxGridCellFloatEditor() when
	Option :: {'width', integer()}
		 | {'precision', integer()}.
new(Options)
 when is_list(Options) ->
  MOpts = fun({width, Width}, Acc) -> [<<1:32/?UI,Width:32/?UI>>|Acc];
          ({precision, Precision}, Acc) -> [<<2:32/?UI,Precision:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:construct(?wxGridCellFloatEditor_new,
  <<BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellfloateditor.html#wxgridcellfloateditorsetparameters">external documentation</a>.
-spec setParameters(This, Params) -> 'ok' when
	This::wxGridCellFloatEditor(), Params::unicode:chardata().
setParameters(#wx_ref{type=ThisT,ref=ThisRef},Params)
 when ?is_chardata(Params) ->
  ?CLASS(ThisT,wxGridCellFloatEditor),
  Params_UC = unicode:characters_to_binary([Params,0]),
  wxe_util:cast(?wxGridCellFloatEditor_SetParameters,
  <<ThisRef:32/?UI,(byte_size(Params_UC)):32/?UI,(Params_UC)/binary, 0:(((8- ((0+byte_size(Params_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxGridCellFloatEditor()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxGridCellFloatEditor),
  wxe_util:destroy(?wxGridCellFloatEditor_destroy,Obj),
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
