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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellchoiceeditor.html">wxGridCellChoiceEditor</a>.
%% <p>This class is derived (and can use functions) from:
%% <br />{@link wxGridCellEditor}
%% </p>
%% @type wxGridCellChoiceEditor().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxGridCellChoiceEditor).
-include("wxe.hrl").
-export([destroy/1,new/1,new/2,setParameters/2]).

%% inherited exports
-export([beginEdit/4,endEdit/4,handleReturn/2,isCreated/1,paintBackground/3,
  parent_class/1,reset/1,setSize/2,show/2,show/3,startingClick/1,startingKey/2]).

-export_type([wxGridCellChoiceEditor/0]).
-compile([{nowarn_deprecated_function, {wxGridCellEditor,endEdit,4}},{nowarn_deprecated_function, {wxGridCellEditor,paintBackground,3}}]).

%% @hidden
parent_class(wxGridCellEditor) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxGridCellChoiceEditor() :: wx:wx_object().
%% @equiv new(Choices, [])
-spec new(Choices) -> wxGridCellChoiceEditor() when
	Choices::[unicode:chardata()].

new(Choices)
 when is_list(Choices) ->
  new(Choices, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellchoiceeditor.html#wxgridcellchoiceeditorwxgridcellchoiceeditor">external documentation</a>.
-spec new(Choices, [Option]) -> wxGridCellChoiceEditor() when
	Choices::[unicode:chardata()],
	Option :: {'allowOthers', boolean()}.
new(Choices, Options)
 when is_list(Choices),is_list(Options) ->
  Choices_UCA = [unicode:characters_to_binary([ChoicesTemp,0]) || 
              ChoicesTemp <- Choices],
  MOpts = fun({allowOthers, AllowOthers}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(AllowOthers)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:construct(?wxGridCellChoiceEditor_new,
  <<(length(Choices_UCA)):32/?UI, (<< <<(byte_size(UC_Str)):32/?UI, UC_Str/binary>>|| UC_Str <- Choices_UCA>>)/binary, 0:(((8- ((4 + lists:sum([byte_size(S)+4||S<-Choices_UCA])) band 16#7)) band 16#7))/unit:8, BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellchoiceeditor.html#wxgridcellchoiceeditorsetparameters">external documentation</a>.
-spec setParameters(This, Params) -> 'ok' when
	This::wxGridCellChoiceEditor(), Params::unicode:chardata().
setParameters(#wx_ref{type=ThisT,ref=ThisRef},Params)
 when is_list(Params) ->
  ?CLASS(ThisT,wxGridCellChoiceEditor),
  Params_UC = unicode:characters_to_binary([Params,0]),
  wxe_util:cast(?wxGridCellChoiceEditor_SetParameters,
  <<ThisRef:32/?UI,(byte_size(Params_UC)):32/?UI,(Params_UC)/binary, 0:(((8- ((0+byte_size(Params_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxGridCellChoiceEditor()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxGridCellChoiceEditor),
  wxe_util:destroy(?wxGridCellChoiceEditor_destroy,Obj),
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
