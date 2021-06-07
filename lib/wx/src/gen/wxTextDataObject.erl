%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2020. All Rights Reserved.
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

-module(wxTextDataObject).
-include("wxe.hrl").
-export([destroy/1,getText/1,getTextLength/1,new/0,new/1,setText/2]).

%% inherited exports
-export([parent_class/1]).

-type wxTextDataObject() :: wx:wx_object().
-export_type([wxTextDataObject/0]).
%% @hidden
parent_class(wxDataObject) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @equiv new([])
-spec new() -> wxTextDataObject().

new() ->
  new([]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextdataobject.html#wxtextdataobjectwxtextdataobject">external documentation</a>.
-spec new([Option]) -> wxTextDataObject() when
	Option :: {'text', unicode:chardata()}.
new(Options)
 when is_list(Options) ->
  MOpts = fun({text, Text}) ->   Text_UC = unicode:characters_to_binary(Text),{text,Text_UC};
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Opts,?get_env(),?wxTextDataObject_new),
  wxe_util:rec(?wxTextDataObject_new).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextdataobject.html#wxtextdataobjectgettextlength">external documentation</a>.
-spec getTextLength(This) -> integer() when
	This::wxTextDataObject().
getTextLength(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextDataObject),
  wxe_util:queue_cmd(This,?get_env(),?wxTextDataObject_GetTextLength),
  wxe_util:rec(?wxTextDataObject_GetTextLength).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextdataobject.html#wxtextdataobjectgettext">external documentation</a>.
-spec getText(This) -> unicode:charlist() when
	This::wxTextDataObject().
getText(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextDataObject),
  wxe_util:queue_cmd(This,?get_env(),?wxTextDataObject_GetText),
  wxe_util:rec(?wxTextDataObject_GetText).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextdataobject.html#wxtextdataobjectsettext">external documentation</a>.
-spec setText(This, StrText) -> 'ok' when
	This::wxTextDataObject(), StrText::unicode:chardata().
setText(#wx_ref{type=ThisT}=This,StrText)
 when ?is_chardata(StrText) ->
  ?CLASS(ThisT,wxTextDataObject),
  StrText_UC = unicode:characters_to_binary(StrText),
  wxe_util:queue_cmd(This,StrText_UC,?get_env(),?wxTextDataObject_SetText).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxTextDataObject()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxTextDataObject),
  wxe_util:queue_cmd(Obj, ?get_env(), ?wxTextDataObject_destroy),
  ok.
 %% From wxDataObject
