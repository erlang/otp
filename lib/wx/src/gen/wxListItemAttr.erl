%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2021. All Rights Reserved.
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

-module(wxListItemAttr).
-include("wxe.hrl").
-export([destroy/1,getBackgroundColour/1,getFont/1,getTextColour/1,hasBackgroundColour/1,
  hasFont/1,hasTextColour/1,new/0,new/3,setBackgroundColour/2,setFont/2,
  setTextColour/2]).

%% inherited exports
-export([parent_class/1]).

-type wxListItemAttr() :: wx:wx_object().
-export_type([wxListItemAttr/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistitemattr.html#wxlistitemattrwxlistitemattr">external documentation</a>.
-spec new() -> wxListItemAttr().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxListItemAttr_new_0),
  wxe_util:rec(?wxListItemAttr_new_0).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistitemattr.html#wxlistitemattrwxlistitemattr">external documentation</a>.
-spec new(ColText, ColBack, Font) -> wxListItemAttr() when
	ColText::wx:wx_colour(), ColBack::wx:wx_colour(), Font::wxFont:wxFont().
new(ColText,ColBack,#wx_ref{type=FontT}=Font)
 when ?is_colordata(ColText),?is_colordata(ColBack) ->
  ?CLASS(FontT,wxFont),
  wxe_util:queue_cmd(wxe_util:color(ColText),wxe_util:color(ColBack),Font,?get_env(),?wxListItemAttr_new_3),
  wxe_util:rec(?wxListItemAttr_new_3).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistitemattr.html#wxlistitemattrgetbackgroundcolour">external documentation</a>.
-spec getBackgroundColour(This) -> wx:wx_colour4() when
	This::wxListItemAttr().
getBackgroundColour(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListItemAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxListItemAttr_GetBackgroundColour),
  wxe_util:rec(?wxListItemAttr_GetBackgroundColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistitemattr.html#wxlistitemattrgetfont">external documentation</a>.
-spec getFont(This) -> wxFont:wxFont() when
	This::wxListItemAttr().
getFont(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListItemAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxListItemAttr_GetFont),
  wxe_util:rec(?wxListItemAttr_GetFont).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistitemattr.html#wxlistitemattrgettextcolour">external documentation</a>.
-spec getTextColour(This) -> wx:wx_colour4() when
	This::wxListItemAttr().
getTextColour(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListItemAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxListItemAttr_GetTextColour),
  wxe_util:rec(?wxListItemAttr_GetTextColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistitemattr.html#wxlistitemattrhasbackgroundcolour">external documentation</a>.
-spec hasBackgroundColour(This) -> boolean() when
	This::wxListItemAttr().
hasBackgroundColour(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListItemAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxListItemAttr_HasBackgroundColour),
  wxe_util:rec(?wxListItemAttr_HasBackgroundColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistitemattr.html#wxlistitemattrhasfont">external documentation</a>.
-spec hasFont(This) -> boolean() when
	This::wxListItemAttr().
hasFont(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListItemAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxListItemAttr_HasFont),
  wxe_util:rec(?wxListItemAttr_HasFont).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistitemattr.html#wxlistitemattrhastextcolour">external documentation</a>.
-spec hasTextColour(This) -> boolean() when
	This::wxListItemAttr().
hasTextColour(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListItemAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxListItemAttr_HasTextColour),
  wxe_util:rec(?wxListItemAttr_HasTextColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistitemattr.html#wxlistitemattrsetbackgroundcolour">external documentation</a>.
-spec setBackgroundColour(This, ColBack) -> 'ok' when
	This::wxListItemAttr(), ColBack::wx:wx_colour().
setBackgroundColour(#wx_ref{type=ThisT}=This,ColBack)
 when ?is_colordata(ColBack) ->
  ?CLASS(ThisT,wxListItemAttr),
  wxe_util:queue_cmd(This,wxe_util:color(ColBack),?get_env(),?wxListItemAttr_SetBackgroundColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistitemattr.html#wxlistitemattrsetfont">external documentation</a>.
-spec setFont(This, Font) -> 'ok' when
	This::wxListItemAttr(), Font::wxFont:wxFont().
setFont(#wx_ref{type=ThisT}=This,#wx_ref{type=FontT}=Font) ->
  ?CLASS(ThisT,wxListItemAttr),
  ?CLASS(FontT,wxFont),
  wxe_util:queue_cmd(This,Font,?get_env(),?wxListItemAttr_SetFont).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistitemattr.html#wxlistitemattrsettextcolour">external documentation</a>.
-spec setTextColour(This, ColText) -> 'ok' when
	This::wxListItemAttr(), ColText::wx:wx_colour().
setTextColour(#wx_ref{type=ThisT}=This,ColText)
 when ?is_colordata(ColText) ->
  ?CLASS(ThisT,wxListItemAttr),
  wxe_util:queue_cmd(This,wxe_util:color(ColText),?get_env(),?wxListItemAttr_SetTextColour).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxListItemAttr()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxListItemAttr),
  wxe_util:queue_cmd(Obj, ?get_env(), ?wxListItemAttr_destroy),
  ok.
