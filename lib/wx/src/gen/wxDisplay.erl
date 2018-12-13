%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2018. All Rights Reserved.
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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdisplay.html">wxDisplay</a>.
%% @type wxDisplay().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxDisplay).
-include("wxe.hrl").
-export([destroy/1,getClientArea/1,getCount/0,getFromPoint/1,getFromWindow/1,
  getGeometry/1,getName/1,getPPI/1,isOk/1,isPrimary/1,new/0,new/1]).

%% inherited exports
-export([parent_class/1]).

-export_type([wxDisplay/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxDisplay() :: wx:wx_object().
%% @equiv new([])
-spec new() -> wxDisplay().

new() ->
  new([]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdisplay.html#wxdisplaywxdisplay">external documentation</a>.
-spec new([Option]) -> wxDisplay() when
	Option :: {'n', integer()}.
new(Options)
 when is_list(Options) ->
  MOpts = fun({n, N}, Acc) -> [<<1:32/?UI,N:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:construct(?wxDisplay_new,
  <<BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdisplay.html#wxdisplayisok">external documentation</a>.
-spec isOk(This) -> boolean() when
	This::wxDisplay().
isOk(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxDisplay),
  wxe_util:call(?wxDisplay_IsOk,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdisplay.html#wxdisplaygetclientarea">external documentation</a>.
-spec getClientArea(This) -> {X::integer(), Y::integer(), W::integer(), H::integer()} when
	This::wxDisplay().
getClientArea(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxDisplay),
  wxe_util:call(?wxDisplay_GetClientArea,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdisplay.html#wxdisplaygetgeometry">external documentation</a>.
-spec getGeometry(This) -> {X::integer(), Y::integer(), W::integer(), H::integer()} when
	This::wxDisplay().
getGeometry(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxDisplay),
  wxe_util:call(?wxDisplay_GetGeometry,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdisplay.html#wxdisplaygetname">external documentation</a>.
-spec getName(This) -> unicode:charlist() when
	This::wxDisplay().
getName(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxDisplay),
  wxe_util:call(?wxDisplay_GetName,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdisplay.html#wxdisplayisprimary">external documentation</a>.
-spec isPrimary(This) -> boolean() when
	This::wxDisplay().
isPrimary(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxDisplay),
  wxe_util:call(?wxDisplay_IsPrimary,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdisplay.html#wxdisplaygetcount">external documentation</a>.
-spec getCount() -> integer().
getCount() ->
  wxe_util:call(?wxDisplay_GetCount,
  <<>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdisplay.html#wxdisplaygetfrompoint">external documentation</a>.
-spec getFromPoint(Pt) -> integer() when
	Pt::{X::integer(), Y::integer()}.
getFromPoint({PtX,PtY})
 when is_integer(PtX),is_integer(PtY) ->
  wxe_util:call(?wxDisplay_GetFromPoint,
  <<PtX:32/?UI,PtY:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdisplay.html#wxdisplaygetfromwindow">external documentation</a>.
-spec getFromWindow(Window) -> integer() when
	Window::wxWindow:wxWindow().
getFromWindow(#wx_ref{type=WindowT,ref=WindowRef}) ->
  ?CLASS(WindowT,wxWindow),
  wxe_util:call(?wxDisplay_GetFromWindow,
  <<WindowRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdisplay.html#wxdisplaygetppi">external documentation</a>.
-spec getPPI(This) -> {W::integer(), H::integer()} when
	This::wxDisplay().
getPPI(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxDisplay),
  wxe_util:call(?wxDisplay_GetPPI,
  <<ThisRef:32/?UI>>).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxDisplay()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxDisplay),
  wxe_util:destroy(?wxDisplay_destruct,Obj),
  ok.
