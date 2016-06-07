%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistitemattr.html">wxListItemAttr</a>.
%% @type wxListItemAttr().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxListItemAttr).
-include("wxe.hrl").
-export([destroy/1,getBackgroundColour/1,getFont/1,getTextColour/1,hasBackgroundColour/1,
  hasFont/1,hasTextColour/1,new/0,new/3,setBackgroundColour/2,setFont/2,
  setTextColour/2]).

%% inherited exports
-export([parent_class/1]).

-export_type([wxListItemAttr/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxListItemAttr() :: wx:wx_object().
%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistitemattr.html#wxlistitemattrwxlistitemattr">external documentation</a>.
-spec new() -> wxListItemAttr().
new() ->
  wxe_util:construct(?wxListItemAttr_new_0,
  <<>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistitemattr.html#wxlistitemattrwxlistitemattr">external documentation</a>.
-spec new(ColText, ColBack, Font) -> wxListItemAttr() when
	ColText::wx:wx_colour(), ColBack::wx:wx_colour(), Font::wxFont:wxFont().
new(ColText,ColBack,#wx_ref{type=FontT,ref=FontRef})
 when tuple_size(ColText) =:= 3; tuple_size(ColText) =:= 4,tuple_size(ColBack) =:= 3; tuple_size(ColBack) =:= 4 ->
  ?CLASS(FontT,wxFont),
  wxe_util:construct(?wxListItemAttr_new_3,
  <<(wxe_util:colour_bin(ColText)):16/binary,(wxe_util:colour_bin(ColBack)):16/binary,FontRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistitemattr.html#wxlistitemattrgetbackgroundcolour">external documentation</a>.
-spec getBackgroundColour(This) -> wx:wx_colour4() when
	This::wxListItemAttr().
getBackgroundColour(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxListItemAttr),
  wxe_util:call(?wxListItemAttr_GetBackgroundColour,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistitemattr.html#wxlistitemattrgetfont">external documentation</a>.
-spec getFont(This) -> wxFont:wxFont() when
	This::wxListItemAttr().
getFont(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxListItemAttr),
  wxe_util:call(?wxListItemAttr_GetFont,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistitemattr.html#wxlistitemattrgettextcolour">external documentation</a>.
-spec getTextColour(This) -> wx:wx_colour4() when
	This::wxListItemAttr().
getTextColour(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxListItemAttr),
  wxe_util:call(?wxListItemAttr_GetTextColour,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistitemattr.html#wxlistitemattrhasbackgroundcolour">external documentation</a>.
-spec hasBackgroundColour(This) -> boolean() when
	This::wxListItemAttr().
hasBackgroundColour(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxListItemAttr),
  wxe_util:call(?wxListItemAttr_HasBackgroundColour,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistitemattr.html#wxlistitemattrhasfont">external documentation</a>.
-spec hasFont(This) -> boolean() when
	This::wxListItemAttr().
hasFont(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxListItemAttr),
  wxe_util:call(?wxListItemAttr_HasFont,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistitemattr.html#wxlistitemattrhastextcolour">external documentation</a>.
-spec hasTextColour(This) -> boolean() when
	This::wxListItemAttr().
hasTextColour(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxListItemAttr),
  wxe_util:call(?wxListItemAttr_HasTextColour,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistitemattr.html#wxlistitemattrsetbackgroundcolour">external documentation</a>.
-spec setBackgroundColour(This, ColBack) -> 'ok' when
	This::wxListItemAttr(), ColBack::wx:wx_colour().
setBackgroundColour(#wx_ref{type=ThisT,ref=ThisRef},ColBack)
 when tuple_size(ColBack) =:= 3; tuple_size(ColBack) =:= 4 ->
  ?CLASS(ThisT,wxListItemAttr),
  wxe_util:cast(?wxListItemAttr_SetBackgroundColour,
  <<ThisRef:32/?UI,(wxe_util:colour_bin(ColBack)):16/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistitemattr.html#wxlistitemattrsetfont">external documentation</a>.
-spec setFont(This, Font) -> 'ok' when
	This::wxListItemAttr(), Font::wxFont:wxFont().
setFont(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=FontT,ref=FontRef}) ->
  ?CLASS(ThisT,wxListItemAttr),
  ?CLASS(FontT,wxFont),
  wxe_util:cast(?wxListItemAttr_SetFont,
  <<ThisRef:32/?UI,FontRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistitemattr.html#wxlistitemattrsettextcolour">external documentation</a>.
-spec setTextColour(This, ColText) -> 'ok' when
	This::wxListItemAttr(), ColText::wx:wx_colour().
setTextColour(#wx_ref{type=ThisT,ref=ThisRef},ColText)
 when tuple_size(ColText) =:= 3; tuple_size(ColText) =:= 4 ->
  ?CLASS(ThisT,wxListItemAttr),
  wxe_util:cast(?wxListItemAttr_SetTextColour,
  <<ThisRef:32/?UI,(wxe_util:colour_bin(ColText)):16/binary>>).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxListItemAttr()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxListItemAttr),
  wxe_util:destroy(?wxListItemAttr_destroy,Obj),
  ok.
