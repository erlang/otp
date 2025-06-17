%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2008-2025. All Rights Reserved.
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

-module(wxCaret).
-moduledoc """
A caret is a blinking cursor showing the position where the typed text will appear.

Text controls usually have their own caret but `m:wxCaret` provides a way to use a caret
in other windows.

Currently, the caret appears as a rectangle of the given size. In the future, it will be
possible to specify a bitmap to be used for the caret shape.

A caret is always associated with a window and the current caret can be retrieved using `wxWindow:getCaret/1`.
The same caret can't be reused in two different windows.

wxWidgets docs: [wxCaret](https://docs.wxwidgets.org/3.2/classwx_caret.html)
""".
-include("wxe.hrl").
-export([create/3,create/4,destroy/1,getBlinkTime/0,getPosition/1,getSize/1,
  getWindow/1,hide/1,isOk/1,isVisible/1,move/2,move/3,new/2,new/3,setBlinkTime/1,
  setSize/2,setSize/3,show/1,show/2]).

%% inherited exports
-export([parent_class/1]).

-type wxCaret() :: wx:wx_object().
-export_type([wxCaret/0]).
-doc false.
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc "".
-spec new(Window, Size) -> wxCaret() when
	Window::wxWindow:wxWindow(), Size::{W::integer(), H::integer()}.
new(#wx_ref{type=WindowT}=Window,{SizeW,SizeH} = Size)
 when is_integer(SizeW),is_integer(SizeH) ->
  ?CLASS(WindowT,wxWindow),
  wxe_util:queue_cmd(Window,Size,?get_env(),?wxCaret_new_2),
  wxe_util:rec(?wxCaret_new_2).

-doc "Creates a caret with the given size (in pixels) and associates it with the `window`.".
-spec new(Window, Width, Height) -> wxCaret() when
	Window::wxWindow:wxWindow(), Width::integer(), Height::integer().
new(#wx_ref{type=WindowT}=Window,Width,Height)
 when is_integer(Width),is_integer(Height) ->
  ?CLASS(WindowT,wxWindow),
  wxe_util:queue_cmd(Window,Width,Height,?get_env(),?wxCaret_new_3),
  wxe_util:rec(?wxCaret_new_3).

-doc "".
-spec create(This, Window, Size) -> boolean() when
	This::wxCaret(), Window::wxWindow:wxWindow(), Size::{W::integer(), H::integer()}.
create(#wx_ref{type=ThisT}=This,#wx_ref{type=WindowT}=Window,{SizeW,SizeH} = Size)
 when is_integer(SizeW),is_integer(SizeH) ->
  ?CLASS(ThisT,wxCaret),
  ?CLASS(WindowT,wxWindow),
  wxe_util:queue_cmd(This,Window,Size,?get_env(),?wxCaret_Create_2),
  wxe_util:rec(?wxCaret_Create_2).

-doc """
Creates a caret with the given size (in pixels) and associates it with the `window` (same
as the equivalent constructors).
""".
-spec create(This, Window, Width, Height) -> boolean() when
	This::wxCaret(), Window::wxWindow:wxWindow(), Width::integer(), Height::integer().
create(#wx_ref{type=ThisT}=This,#wx_ref{type=WindowT}=Window,Width,Height)
 when is_integer(Width),is_integer(Height) ->
  ?CLASS(ThisT,wxCaret),
  ?CLASS(WindowT,wxWindow),
  wxe_util:queue_cmd(This,Window,Width,Height,?get_env(),?wxCaret_Create_3),
  wxe_util:rec(?wxCaret_Create_3).

-doc """
Returns the blink time which is measured in milliseconds and is the time elapsed between
2 inversions of the caret (blink time of the caret is the same for all carets, so this
functions is static).
""".
-spec getBlinkTime() -> integer().
getBlinkTime() ->
  wxe_util:queue_cmd(?get_env(), ?wxCaret_GetBlinkTime),
  wxe_util:rec(?wxCaret_GetBlinkTime).

-doc "".
-spec getPosition(This) -> {X::integer(), Y::integer()} when
	This::wxCaret().
getPosition(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxCaret),
  wxe_util:queue_cmd(This,?get_env(),?wxCaret_GetPosition),
  wxe_util:rec(?wxCaret_GetPosition).

-doc "".
-spec getSize(This) -> {W::integer(), H::integer()} when
	This::wxCaret().
getSize(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxCaret),
  wxe_util:queue_cmd(This,?get_env(),?wxCaret_GetSize),
  wxe_util:rec(?wxCaret_GetSize).

-doc "Get the window the caret is associated with.".
-spec getWindow(This) -> wxWindow:wxWindow() when
	This::wxCaret().
getWindow(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxCaret),
  wxe_util:queue_cmd(This,?get_env(),?wxCaret_GetWindow),
  wxe_util:rec(?wxCaret_GetWindow).

-doc "Hides the caret, same as Show(false).".
-spec hide(This) -> 'ok' when
	This::wxCaret().
hide(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxCaret),
  wxe_util:queue_cmd(This,?get_env(),?wxCaret_Hide).

-doc "Returns true if the caret was created successfully.".
-spec isOk(This) -> boolean() when
	This::wxCaret().
isOk(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxCaret),
  wxe_util:queue_cmd(This,?get_env(),?wxCaret_IsOk),
  wxe_util:rec(?wxCaret_IsOk).

-doc """
Returns true if the caret is visible and false if it is permanently hidden (if it is
blinking and not shown currently but will be after the next blink, this method still
returns true).
""".
-spec isVisible(This) -> boolean() when
	This::wxCaret().
isVisible(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxCaret),
  wxe_util:queue_cmd(This,?get_env(),?wxCaret_IsVisible),
  wxe_util:rec(?wxCaret_IsVisible).

-doc "".
-spec move(This, Pt) -> 'ok' when
	This::wxCaret(), Pt::{X::integer(), Y::integer()}.
move(#wx_ref{type=ThisT}=This,{PtX,PtY} = Pt)
 when is_integer(PtX),is_integer(PtY) ->
  ?CLASS(ThisT,wxCaret),
  wxe_util:queue_cmd(This,Pt,?get_env(),?wxCaret_Move_1).

-doc "Move the caret to given position (in logical coordinates).".
-spec move(This, X, Y) -> 'ok' when
	This::wxCaret(), X::integer(), Y::integer().
move(#wx_ref{type=ThisT}=This,X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxCaret),
  wxe_util:queue_cmd(This,X,Y,?get_env(),?wxCaret_Move_2).

-doc """
Sets the blink time for all the carets.

Warning:

Under Windows, this function will change the blink time for all carets permanently (until
the next time it is called), even for carets in other applications.

See: `getBlinkTime/0`
""".
-spec setBlinkTime(Milliseconds) -> 'ok' when
	Milliseconds::integer().
setBlinkTime(Milliseconds)
 when is_integer(Milliseconds) ->
  wxe_util:queue_cmd(Milliseconds,?get_env(),?wxCaret_SetBlinkTime).

-doc "".
-spec setSize(This, Size) -> 'ok' when
	This::wxCaret(), Size::{W::integer(), H::integer()}.
setSize(#wx_ref{type=ThisT}=This,{SizeW,SizeH} = Size)
 when is_integer(SizeW),is_integer(SizeH) ->
  ?CLASS(ThisT,wxCaret),
  wxe_util:queue_cmd(This,Size,?get_env(),?wxCaret_SetSize_1).

-doc "Changes the size of the caret.".
-spec setSize(This, Width, Height) -> 'ok' when
	This::wxCaret(), Width::integer(), Height::integer().
setSize(#wx_ref{type=ThisT}=This,Width,Height)
 when is_integer(Width),is_integer(Height) ->
  ?CLASS(ThisT,wxCaret),
  wxe_util:queue_cmd(This,Width,Height,?get_env(),?wxCaret_SetSize_2).

-doc(#{equiv => show(This, [])}).
-spec show(This) -> 'ok' when
	This::wxCaret().

show(This)
 when is_record(This, wx_ref) ->
  show(This, []).

-doc """
Shows or hides the caret.

Notice that if the caret was hidden N times, it must be shown N times as well to reappear
on the screen.
""".
-spec show(This, [Option]) -> 'ok' when
	This::wxCaret(),
	Option :: {'show', boolean()}.
show(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxCaret),
  MOpts = fun({show, _show} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxCaret_Show).

-doc "Destroys the object".
-spec destroy(This::wxCaret()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxCaret),
  wxe_util:queue_cmd(Obj, ?get_env(), ?wxCaret_destroy),
  ok.
