%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2011. All Rights Reserved.
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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/stable/wx_wxlistitemattr.html">wxListItemAttr</a>.
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

%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @spec () -> wxListItemAttr()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxlistitemattr.html#wxlistitemattrwxlistitemattr">external documentation</a>.
new() ->
  wxe_util:construct(?wxListItemAttr_new_0,
  <<>>).

%% @spec (ColText::wx:colour(), ColBack::wx:colour(), Font::wxFont:wxFont()) -> wxListItemAttr()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxlistitemattr.html#wxlistitemattrwxlistitemattr">external documentation</a>.
new(ColText,ColBack,#wx_ref{type=FontT,ref=FontRef})
 when tuple_size(ColText) =:= 3; tuple_size(ColText) =:= 4,tuple_size(ColBack) =:= 3; tuple_size(ColBack) =:= 4 ->
  ?CLASS(FontT,wxFont),
  wxe_util:construct(?wxListItemAttr_new_3,
  <<(wxe_util:colour_bin(ColText)):16/binary,(wxe_util:colour_bin(ColBack)):16/binary,FontRef:32/?UI>>).

%% @spec (This::wxListItemAttr()) -> wx:colour()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxlistitemattr.html#wxlistitemattrgetbackgroundcolour">external documentation</a>.
getBackgroundColour(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxListItemAttr),
  wxe_util:call(?wxListItemAttr_GetBackgroundColour,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxListItemAttr()) -> wxFont:wxFont()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxlistitemattr.html#wxlistitemattrgetfont">external documentation</a>.
getFont(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxListItemAttr),
  wxe_util:call(?wxListItemAttr_GetFont,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxListItemAttr()) -> wx:colour()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxlistitemattr.html#wxlistitemattrgettextcolour">external documentation</a>.
getTextColour(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxListItemAttr),
  wxe_util:call(?wxListItemAttr_GetTextColour,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxListItemAttr()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxlistitemattr.html#wxlistitemattrhasbackgroundcolour">external documentation</a>.
hasBackgroundColour(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxListItemAttr),
  wxe_util:call(?wxListItemAttr_HasBackgroundColour,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxListItemAttr()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxlistitemattr.html#wxlistitemattrhasfont">external documentation</a>.
hasFont(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxListItemAttr),
  wxe_util:call(?wxListItemAttr_HasFont,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxListItemAttr()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxlistitemattr.html#wxlistitemattrhastextcolour">external documentation</a>.
hasTextColour(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxListItemAttr),
  wxe_util:call(?wxListItemAttr_HasTextColour,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxListItemAttr(), ColBack::wx:colour()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxlistitemattr.html#wxlistitemattrsetbackgroundcolour">external documentation</a>.
setBackgroundColour(#wx_ref{type=ThisT,ref=ThisRef},ColBack)
 when tuple_size(ColBack) =:= 3; tuple_size(ColBack) =:= 4 ->
  ?CLASS(ThisT,wxListItemAttr),
  wxe_util:cast(?wxListItemAttr_SetBackgroundColour,
  <<ThisRef:32/?UI,(wxe_util:colour_bin(ColBack)):16/binary>>).

%% @spec (This::wxListItemAttr(), Font::wxFont:wxFont()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxlistitemattr.html#wxlistitemattrsetfont">external documentation</a>.
setFont(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=FontT,ref=FontRef}) ->
  ?CLASS(ThisT,wxListItemAttr),
  ?CLASS(FontT,wxFont),
  wxe_util:cast(?wxListItemAttr_SetFont,
  <<ThisRef:32/?UI,FontRef:32/?UI>>).

%% @spec (This::wxListItemAttr(), ColText::wx:colour()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxlistitemattr.html#wxlistitemattrsettextcolour">external documentation</a>.
setTextColour(#wx_ref{type=ThisT,ref=ThisRef},ColText)
 when tuple_size(ColText) =:= 3; tuple_size(ColText) =:= 4 ->
  ?CLASS(ThisT,wxListItemAttr),
  wxe_util:cast(?wxListItemAttr_SetTextColour,
  <<ThisRef:32/?UI,(wxe_util:colour_bin(ColText)):16/binary>>).

%% @spec (This::wxListItemAttr()) -> ok
%% @doc Destroys this object, do not use object again
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxListItemAttr),
  wxe_util:destroy(?wxListItemAttr_destroy,Obj),
  ok.
