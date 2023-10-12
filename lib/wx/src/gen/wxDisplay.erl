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

-module(wxDisplay).
-include("wxe.hrl").
-export([destroy/1,getClientArea/1,getCount/0,getFromPoint/1,getFromWindow/1,
  getGeometry/1,getName/1,getPPI/1,isOk/1,isPrimary/1,new/0,new/1]).

%% inherited exports
-export([parent_class/1]).

-type wxDisplay() :: wx:wx_object().
-export_type([wxDisplay/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdisplay.html#wxdisplaywxdisplay">external documentation</a>.
-spec new() -> wxDisplay().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxDisplay_new_0),
  wxe_util:rec(?wxDisplay_new_0).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdisplay.html#wxdisplaywxdisplay">external documentation</a>.
%% <br /> Also:<br />
%% new(Window) -> wxDisplay() when<br />
%% 	Window::wxWindow:wxWindow().<br />
%% 
-spec new(Index) -> wxDisplay() when
	Index::integer();
      (Window) -> wxDisplay() when
	Window::wxWindow:wxWindow().
new(Index)
 when is_integer(Index) ->
  wxe_util:queue_cmd(Index,?get_env(),?wxDisplay_new_1_0),
  wxe_util:rec(?wxDisplay_new_1_0);
new(#wx_ref{type=WindowT}=Window) ->
  ?CLASS(WindowT,wxWindow),
  wxe_util:queue_cmd(Window,?get_env(),?wxDisplay_new_1_1),
  wxe_util:rec(?wxDisplay_new_1_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdisplay.html#wxdisplayisok">external documentation</a>.
-spec isOk(This) -> boolean() when
	This::wxDisplay().
isOk(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxDisplay),
  wxe_util:queue_cmd(This,?get_env(),?wxDisplay_IsOk),
  wxe_util:rec(?wxDisplay_IsOk).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdisplay.html#wxdisplaygetclientarea">external documentation</a>.
-spec getClientArea(This) -> {X::integer(), Y::integer(), W::integer(), H::integer()} when
	This::wxDisplay().
getClientArea(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxDisplay),
  wxe_util:queue_cmd(This,?get_env(),?wxDisplay_GetClientArea),
  wxe_util:rec(?wxDisplay_GetClientArea).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdisplay.html#wxdisplaygetgeometry">external documentation</a>.
-spec getGeometry(This) -> {X::integer(), Y::integer(), W::integer(), H::integer()} when
	This::wxDisplay().
getGeometry(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxDisplay),
  wxe_util:queue_cmd(This,?get_env(),?wxDisplay_GetGeometry),
  wxe_util:rec(?wxDisplay_GetGeometry).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdisplay.html#wxdisplaygetname">external documentation</a>.
-spec getName(This) -> unicode:charlist() when
	This::wxDisplay().
getName(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxDisplay),
  wxe_util:queue_cmd(This,?get_env(),?wxDisplay_GetName),
  wxe_util:rec(?wxDisplay_GetName).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdisplay.html#wxdisplayisprimary">external documentation</a>.
-spec isPrimary(This) -> boolean() when
	This::wxDisplay().
isPrimary(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxDisplay),
  wxe_util:queue_cmd(This,?get_env(),?wxDisplay_IsPrimary),
  wxe_util:rec(?wxDisplay_IsPrimary).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdisplay.html#wxdisplaygetcount">external documentation</a>.
-spec getCount() -> integer().
getCount() ->
  wxe_util:queue_cmd(?get_env(), ?wxDisplay_GetCount),
  wxe_util:rec(?wxDisplay_GetCount).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdisplay.html#wxdisplaygetfrompoint">external documentation</a>.
-spec getFromPoint(Pt) -> integer() when
	Pt::{X::integer(), Y::integer()}.
getFromPoint({PtX,PtY} = Pt)
 when is_integer(PtX),is_integer(PtY) ->
  wxe_util:queue_cmd(Pt,?get_env(),?wxDisplay_GetFromPoint),
  wxe_util:rec(?wxDisplay_GetFromPoint).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdisplay.html#wxdisplaygetfromwindow">external documentation</a>.
-spec getFromWindow(Win) -> integer() when
	Win::wxWindow:wxWindow().
getFromWindow(#wx_ref{type=WinT}=Win) ->
  ?CLASS(WinT,wxWindow),
  wxe_util:queue_cmd(Win,?get_env(),?wxDisplay_GetFromWindow),
  wxe_util:rec(?wxDisplay_GetFromWindow).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdisplay.html#wxdisplaygetppi">external documentation</a>.
-spec getPPI(This) -> {W::integer(), H::integer()} when
	This::wxDisplay().
getPPI(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxDisplay),
  wxe_util:queue_cmd(This,?get_env(),?wxDisplay_GetPPI),
  wxe_util:rec(?wxDisplay_GetPPI).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxDisplay()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxDisplay),
  wxe_util:queue_cmd(Obj, ?get_env(), ?wxDisplay_destruct),
  ok.
