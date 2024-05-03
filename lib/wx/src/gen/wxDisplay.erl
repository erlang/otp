%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2024. All Rights Reserved.
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
-moduledoc """
Functions for wxDisplay class

Determines the sizes and locations of displays connected to the system.

wxWidgets docs: [wxDisplay](https://docs.wxwidgets.org/3.1/classwx_display.html)
""".
-include("wxe.hrl").
-export([destroy/1,getClientArea/1,getCount/0,getFromPoint/1,getFromWindow/1,
  getGeometry/1,getName/1,getPPI/1,isOk/1,isPrimary/1,new/0,new/1]).

%% inherited exports
-export([parent_class/1]).

-type wxDisplay() :: wx:wx_object().
-export_type([wxDisplay/0]).
%% @hidden
-doc false.
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdisplay.html#wxdisplaywxdisplay">external documentation</a>.
-doc """
Default constructor creating `m:wxDisplay` object representing the primary
display.
""".
-spec new() -> wxDisplay().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxDisplay_new_0),
  wxe_util:rec(?wxDisplay_new_0).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdisplay.html#wxdisplaywxdisplay">external documentation</a>.
%% <br /> Also:<br />
%% new(Window) -> wxDisplay() when<br />
%% 	Window::wxWindow:wxWindow().<br />
%% 
-doc """
Constructor creating the display object associated with the given window.

This is the most convenient way of finding the display on which the given window
is shown while falling back to the default display if it is not shown at all or
positioned outside of any display.

See: `getFromWindow/1`

Since: 3.1.2
""".
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
-doc "Returns true if the object was initialized successfully.".
-spec isOk(This) -> boolean() when
	This::wxDisplay().
isOk(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxDisplay),
  wxe_util:queue_cmd(This,?get_env(),?wxDisplay_IsOk),
  wxe_util:rec(?wxDisplay_IsOk).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdisplay.html#wxdisplaygetclientarea">external documentation</a>.
-doc """
Returns the client area of the display.

The client area is the part of the display available for the normal (non full
screen) windows, usually it is the same as `getGeometry/1` but it could be less
if there is a taskbar (or equivalent) on this display.
""".
-spec getClientArea(This) -> {X::integer(), Y::integer(), W::integer(), H::integer()} when
	This::wxDisplay().
getClientArea(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxDisplay),
  wxe_util:queue_cmd(This,?get_env(),?wxDisplay_GetClientArea),
  wxe_util:rec(?wxDisplay_GetClientArea).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdisplay.html#wxdisplaygetgeometry">external documentation</a>.
-doc """
Returns the bounding rectangle of the display whose index was passed to the
constructor.

See: `getClientArea/1`, `wx_misc:displaySize/0`
""".
-spec getGeometry(This) -> {X::integer(), Y::integer(), W::integer(), H::integer()} when
	This::wxDisplay().
getGeometry(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxDisplay),
  wxe_util:queue_cmd(This,?get_env(),?wxDisplay_GetGeometry),
  wxe_util:rec(?wxDisplay_GetGeometry).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdisplay.html#wxdisplaygetname">external documentation</a>.
-doc """
Returns the display's name.

The returned value is currently an empty string under all platforms except MSW.
""".
-spec getName(This) -> unicode:charlist() when
	This::wxDisplay().
getName(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxDisplay),
  wxe_util:queue_cmd(This,?get_env(),?wxDisplay_GetName),
  wxe_util:rec(?wxDisplay_GetName).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdisplay.html#wxdisplayisprimary">external documentation</a>.
-doc """
Returns true if the display is the primary display.

The primary display is the one whose index is 0.
""".
-spec isPrimary(This) -> boolean() when
	This::wxDisplay().
isPrimary(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxDisplay),
  wxe_util:queue_cmd(This,?get_env(),?wxDisplay_IsPrimary),
  wxe_util:rec(?wxDisplay_IsPrimary).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdisplay.html#wxdisplaygetcount">external documentation</a>.
-doc "Returns the number of connected displays.".
-spec getCount() -> integer().
getCount() ->
  wxe_util:queue_cmd(?get_env(), ?wxDisplay_GetCount),
  wxe_util:rec(?wxDisplay_GetCount).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdisplay.html#wxdisplaygetfrompoint">external documentation</a>.
-doc """
Returns the index of the display on which the given point lies, or `wxNOT_FOUND`
if the point is not on any connected display.
""".
-spec getFromPoint(Pt) -> integer() when
	Pt::{X::integer(), Y::integer()}.
getFromPoint({PtX,PtY} = Pt)
 when is_integer(PtX),is_integer(PtY) ->
  wxe_util:queue_cmd(Pt,?get_env(),?wxDisplay_GetFromPoint),
  wxe_util:rec(?wxDisplay_GetFromPoint).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdisplay.html#wxdisplaygetfromwindow">external documentation</a>.
-doc """
Returns the index of the display on which the given window lies.

If the window is on more than one display it gets the display that overlaps the
window the most.

Returns `wxNOT_FOUND` if the window is not on any connected display.
""".
-spec getFromWindow(Win) -> integer() when
	Win::wxWindow:wxWindow().
getFromWindow(#wx_ref{type=WinT}=Win) ->
  ?CLASS(WinT,wxWindow),
  wxe_util:queue_cmd(Win,?get_env(),?wxDisplay_GetFromWindow),
  wxe_util:rec(?wxDisplay_GetFromWindow).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdisplay.html#wxdisplaygetppi">external documentation</a>.
-doc """
Returns display resolution in pixels per inch.

Horizontal and vertical resolution are returned in `x` and `y` components of the
\{Width,Height\} object respectively.

If the resolution information is not available, returns.

Since: 3.1.2
""".
-spec getPPI(This) -> {W::integer(), H::integer()} when
	This::wxDisplay().
getPPI(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxDisplay),
  wxe_util:queue_cmd(This,?get_env(),?wxDisplay_GetPPI),
  wxe_util:rec(?wxDisplay_GetPPI).

%% @doc Destroys this object, do not use object again
-doc "Destructor.".
-spec destroy(This::wxDisplay()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxDisplay),
  wxe_util:queue_cmd(Obj, ?get_env(), ?wxDisplay_destruct),
  ok.
