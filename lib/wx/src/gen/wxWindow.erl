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

-module(wxWindow).
-moduledoc """
Functions for wxWindow class

`m:wxWindow` is the base class for all windows and represents any visible object
on screen. All controls, top level windows and so on are windows. Sizers and
device contexts are not, however, as they don't appear on screen themselves.

Please note that all children of the window will be deleted automatically by the
destructor before the window itself is deleted which means that you don't have
to worry about deleting them manually. Please see the window deletion overview
for more information.

Also note that in this, and many others, wxWidgets classes some `GetXXX()`
methods may be overloaded (as, for example, `getSize/1` or `getClientSize/1`).
In this case, the overloads are non-virtual because having multiple virtual
functions with the same name results in a virtual function name hiding at the
derived class level (in English, this means that the derived class has to
override all overloaded variants if it overrides any of them). To allow
overriding them in the derived class, wxWidgets uses a unique protected virtual
`DoGetXXX()` method and all `GetXXX()` ones are forwarded to it, so overriding
the former changes the behaviour of the latter.

Styles

This class supports the following styles:

Extra Styles

This class supports the following extra styles:

See:
[Overview events](https://docs.wxwidgets.org/3.1/overview_events.html#overview_events),
[Overview windowsizing](https://docs.wxwidgets.org/3.1/overview_windowsizing.html#overview_windowsizing)

This class is derived (and can use functions) from: `m:wxEvtHandler`

wxWidgets docs: [wxWindow](https://docs.wxwidgets.org/3.1/classwx_window.html)

## Events

Event types emitted from this class: [`activate`](`m:wxActivateEvent`),
[`child_focus`](`m:wxChildFocusEvent`),
[`context_menu`](`m:wxContextMenuEvent`), [`help`](`m:wxHelpEvent`),
[`drop_files`](`m:wxDropFilesEvent`), [`erase_background`](`m:wxEraseEvent`),
[`set_focus`](`m:wxFocusEvent`), [`kill_focus`](`m:wxFocusEvent`),
[`idle`](`m:wxIdleEvent`), [`joy_button_down`](`m:wxJoystickEvent`),
[`joy_button_up`](`m:wxJoystickEvent`), [`joy_move`](`m:wxJoystickEvent`),
[`joy_zmove`](`m:wxJoystickEvent`), [`key_down`](`m:wxKeyEvent`),
[`key_up`](`m:wxKeyEvent`), [`char`](`m:wxKeyEvent`),
[`char_hook`](`m:wxKeyEvent`),
[`mouse_capture_lost`](`m:wxMouseCaptureLostEvent`),
[`mouse_capture_changed`](`m:wxMouseCaptureChangedEvent`),
[`left_down`](`m:wxMouseEvent`), [`left_up`](`m:wxMouseEvent`),
[`middle_down`](`m:wxMouseEvent`), [`middle_up`](`m:wxMouseEvent`),
[`right_down`](`m:wxMouseEvent`), [`right_up`](`m:wxMouseEvent`),
[`motion`](`m:wxMouseEvent`), [`enter_window`](`m:wxMouseEvent`),
[`leave_window`](`m:wxMouseEvent`), [`left_dclick`](`m:wxMouseEvent`),
[`middle_dclick`](`m:wxMouseEvent`), [`right_dclick`](`m:wxMouseEvent`),
[`mousewheel`](`m:wxMouseEvent`), [`aux1_down`](`m:wxMouseEvent`),
[`aux1_up`](`m:wxMouseEvent`), [`aux1_dclick`](`m:wxMouseEvent`),
[`aux2_down`](`m:wxMouseEvent`), [`aux2_up`](`m:wxMouseEvent`),
[`aux2_dclick`](`m:wxMouseEvent`), [`paint`](`m:wxPaintEvent`),
[`scrollwin_top`](`m:wxScrollWinEvent`),
[`scrollwin_bottom`](`m:wxScrollWinEvent`),
[`scrollwin_lineup`](`m:wxScrollWinEvent`),
[`scrollwin_linedown`](`m:wxScrollWinEvent`),
[`scrollwin_pageup`](`m:wxScrollWinEvent`),
[`scrollwin_pagedown`](`m:wxScrollWinEvent`),
[`scrollwin_thumbtrack`](`m:wxScrollWinEvent`),
[`scrollwin_thumbrelease`](`m:wxScrollWinEvent`),
[`set_cursor`](`m:wxSetCursorEvent`), [`size`](`m:wxSizeEvent`),
[`sys_colour_changed`](`m:wxSysColourChangedEvent`)
""".
-include("wxe.hrl").
-export(['Destroy'/1,cacheBestSize/2,canSetTransparent/1,captureMouse/1,center/1,
  center/2,centerOnParent/1,centerOnParent/2,centre/1,centre/2,centreOnParent/1,
  centreOnParent/2,clearBackground/1,clientToScreen/2,clientToScreen/3,
  close/1,close/2,convertDialogToPixels/2,convertPixelsToDialog/2,create/3,
  create/4,destroy/1,destroyChildren/1,disable/1,dragAcceptFiles/2,enable/1,
  enable/2,findFocus/0,findWindow/2,findWindowById/1,findWindowById/2,
  findWindowByLabel/1,findWindowByLabel/2,findWindowByName/1,findWindowByName/2,
  fit/1,fitInside/1,freeze/1,fromDIP/2,getAcceleratorTable/1,getBackgroundColour/1,
  getBackgroundStyle/1,getBestSize/1,getCapture/0,getCaret/1,getCharHeight/1,
  getCharWidth/1,getChildren/1,getClientSize/1,getContainingSizer/1,
  getContentScaleFactor/1,getCursor/1,getDPI/1,getDPIScaleFactor/1,
  getDropTarget/1,getExtraStyle/1,getFont/1,getForegroundColour/1,getGrandParent/1,
  getHandle/1,getHelpText/1,getId/1,getLabel/1,getMaxSize/1,getMinSize/1,
  getName/1,getParent/1,getPosition/1,getRect/1,getScreenPosition/1,
  getScreenRect/1,getScrollPos/2,getScrollRange/2,getScrollThumb/2,
  getSize/1,getSizer/1,getTextExtent/2,getTextExtent/3,getThemeEnabled/1,
  getToolTip/1,getUpdateRegion/1,getVirtualSize/1,getWindowStyleFlag/1,
  getWindowVariant/1,hasCapture/1,hasScrollbar/2,hasTransparentBackground/1,
  hide/1,inheritAttributes/1,initDialog/1,invalidateBestSize/1,isDoubleBuffered/1,
  isEnabled/1,isExposed/2,isExposed/3,isExposed/5,isFrozen/1,isRetained/1,
  isShown/1,isShownOnScreen/1,isTopLevel/1,layout/1,lineDown/1,lineUp/1,
  lower/1,move/2,move/3,move/4,moveAfterInTabOrder/2,moveBeforeInTabOrder/2,
  navigate/1,navigate/2,new/0,new/2,new/3,pageDown/1,pageUp/1,popupMenu/2,
  popupMenu/3,popupMenu/4,raise/1,refresh/1,refresh/2,refreshRect/2,refreshRect/3,
  releaseMouse/1,removeChild/2,reparent/2,screenToClient/1,screenToClient/2,
  scrollLines/2,scrollPages/2,scrollWindow/3,scrollWindow/4,setAcceleratorTable/2,
  setAutoLayout/2,setBackgroundColour/2,setBackgroundStyle/2,setCaret/2,
  setClientSize/2,setClientSize/3,setContainingSizer/2,setCursor/2,
  setDoubleBuffered/2,setDropTarget/2,setExtraStyle/2,setFocus/1,setFocusFromKbd/1,
  setFont/2,setForegroundColour/2,setHelpText/2,setId/2,setLabel/2,setMaxSize/2,
  setMinSize/2,setName/2,setOwnBackgroundColour/2,setOwnFont/2,setOwnForegroundColour/2,
  setPalette/2,setScrollPos/3,setScrollPos/4,setScrollbar/5,setScrollbar/6,
  setSize/2,setSize/3,setSize/5,setSize/6,setSizeHints/2,setSizeHints/3,
  setSizeHints/4,setSizer/2,setSizer/3,setSizerAndFit/2,setSizerAndFit/3,
  setThemeEnabled/2,setToolTip/2,setTransparent/2,setVirtualSize/2,
  setVirtualSize/3,setWindowStyle/2,setWindowStyleFlag/2,setWindowVariant/2,
  shouldInheritColours/1,show/1,show/2,thaw/1,toDIP/2,transferDataFromWindow/1,
  transferDataToWindow/1,update/1,updateWindowUI/1,updateWindowUI/2,
  validate/1,warpPointer/3]).

%% inherited exports
-export([connect/2,connect/3,disconnect/1,disconnect/2,disconnect/3,parent_class/1]).

-type wxWindow() :: wx:wx_object().
-export_type([wxWindow/0]).
%% @hidden
-doc false.
parent_class(wxEvtHandler) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowwxwindow">external documentation</a>.
-doc "Default constructor.".
-spec new() -> wxWindow().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxWindow_new_0),
  wxe_util:rec(?wxWindow_new_0).

%% @equiv new(Parent,Id, [])
-spec new(Parent, Id) -> wxWindow() when
	Parent::wxWindow(), Id::integer().

new(Parent,Id)
 when is_record(Parent, wx_ref),is_integer(Id) ->
  new(Parent,Id, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowwxwindow">external documentation</a>.
-doc """
Constructs a window, which can be a child of a frame, dialog or any other
non-control window.
""".
-spec new(Parent, Id, [Option]) -> wxWindow() when
	Parent::wxWindow(), Id::integer(),
	Option :: {'pos', {X::integer(), Y::integer()}}
		 | {'size', {W::integer(), H::integer()}}
		 | {'style', integer()}.
new(#wx_ref{type=ParentT}=Parent,Id, Options)
 when is_integer(Id),is_list(Options) ->
  ?CLASS(ParentT,wxWindow),
  MOpts = fun({pos, {_posX,_posY}} = Arg) -> Arg;
          ({size, {_sizeW,_sizeH}} = Arg) -> Arg;
          ({style, _style} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Parent,Id, Opts,?get_env(),?wxWindow_new_3),
  wxe_util:rec(?wxWindow_new_3).

%% @equiv create(This,Parent,Id, [])
-spec create(This, Parent, Id) -> boolean() when
	This::wxWindow(), Parent::wxWindow(), Id::integer().

create(This,Parent,Id)
 when is_record(This, wx_ref),is_record(Parent, wx_ref),is_integer(Id) ->
  create(This,Parent,Id, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowcreate">external documentation</a>.
-doc """
Construct the actual window object after creating the C++ object.

The non-default constructor of `m:wxWindow` class does two things: it
initializes the C++ object and it also creates the window object in the
underlying graphical toolkit. The `create/4` method can be used to perform the
second part later, while the default constructor can be used to perform the
first part only.

Please note that the underlying window must be created exactly once, i.e. if you
use the default constructor, which doesn't do this, you `must` call `create/4`
before using the window and if you use the non-default constructor, you can
`not` call `create/4`, as the underlying window is already created.

Note that it is possible and, in fact, useful, to call some methods on the
object between creating the C++ object itself and calling `create/4` on it, e.g.
a common pattern to avoid showing the contents of a window before it is fully
initialized is:

Also note that it is possible to create an object of a derived type and then
call `create/4` on it: This is notably used by overview_xrc.

The parameters of this method have exactly the same meaning as the non-default
constructor parameters, please refer to them for their description.

Return: true if window creation succeeded or false if it failed
""".
-spec create(This, Parent, Id, [Option]) -> boolean() when
	This::wxWindow(), Parent::wxWindow(), Id::integer(),
	Option :: {'pos', {X::integer(), Y::integer()}}
		 | {'size', {W::integer(), H::integer()}}
		 | {'style', integer()}.
create(#wx_ref{type=ThisT}=This,#wx_ref{type=ParentT}=Parent,Id, Options)
 when is_integer(Id),is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  ?CLASS(ParentT,wxWindow),
  MOpts = fun({pos, {_posX,_posY}} = Arg) -> Arg;
          ({size, {_sizeW,_sizeH}} = Arg) -> Arg;
          ({style, _style} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Parent,Id, Opts,?get_env(),?wxWindow_Create),
  wxe_util:rec(?wxWindow_Create).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowcachebestsize">external documentation</a>.
-doc """
Sets the cached best size value.

See: `getBestSize/1`
""".
-spec cacheBestSize(This, Size) -> 'ok' when
	This::wxWindow(), Size::{W::integer(), H::integer()}.
cacheBestSize(#wx_ref{type=ThisT}=This,{SizeW,SizeH} = Size)
 when is_integer(SizeW),is_integer(SizeH) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,Size,?get_env(),?wxWindow_CacheBestSize).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowcapturemouse">external documentation</a>.
-doc """
Directs all mouse input to this window.

Call `releaseMouse/1` to release the capture.

Note that wxWidgets maintains the stack of windows having captured the mouse and
when the mouse is released the capture returns to the window which had had
captured it previously and it is only really released if there were no previous
window. In particular, this means that you must release the mouse as many times
as you capture it, unless the window receives the `m:wxMouseCaptureLostEvent`
event.

Any application which captures the mouse in the beginning of some operation must
handle `m:wxMouseCaptureLostEvent` and cancel this operation when it receives
the event. The event handler must not recapture mouse.

See: `releaseMouse/1`, `m:wxMouseCaptureLostEvent`
""".
-spec captureMouse(This) -> 'ok' when
	This::wxWindow().
captureMouse(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_CaptureMouse).

%% @equiv center(This, [])
-spec center(This) -> 'ok' when
	This::wxWindow().

center(This)
 when is_record(This, wx_ref) ->
  center(This, []).

%% @equiv centre(This, [])
-spec centre(This) -> 'ok' when
	This::wxWindow().

centre(This)
 when is_record(This, wx_ref) ->
  centre(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowcentre">external documentation</a>.
-doc "See: `centre/2`.".
-spec center(This, [Option]) -> 'ok' when
	This::wxWindow(),
	Option :: {'dir', integer()}.

center(This, Options)
 when is_record(This, wx_ref),is_list(Options) ->
  centre(This, Options).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowcentre">external documentation</a>.
-doc """
Centres the window.

Remark: If the window is a top level one (i.e. doesn't have a parent), it will
be centred relative to the screen anyhow.

See: `center/2`
""".
-spec centre(This, [Option]) -> 'ok' when
	This::wxWindow(),
	Option :: {'dir', integer()}.
centre(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({dir, _dir} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxWindow_Centre).

%% @equiv centerOnParent(This, [])
-spec centerOnParent(This) -> 'ok' when
	This::wxWindow().

centerOnParent(This)
 when is_record(This, wx_ref) ->
  centerOnParent(This, []).

%% @equiv centreOnParent(This, [])
-spec centreOnParent(This) -> 'ok' when
	This::wxWindow().

centreOnParent(This)
 when is_record(This, wx_ref) ->
  centreOnParent(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowcentreonparent">external documentation</a>.
-doc "See: `centreOnParent/2`.".
-spec centerOnParent(This, [Option]) -> 'ok' when
	This::wxWindow(),
	Option :: {'dir', integer()}.

centerOnParent(This, Options)
 when is_record(This, wx_ref),is_list(Options) ->
  centreOnParent(This, Options).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowcentreonparent">external documentation</a>.
-doc """
Centres the window on its parent.

This is a more readable synonym for `centre/2`.

Remark: This methods provides for a way to centre top level windows over their
parents instead of the entire screen. If there is no parent or if the window is
not a top level window, then behaviour is the same as `centre/2`.

See: `wxTopLevelWindow:centreOnScreen/2`
""".
-spec centreOnParent(This, [Option]) -> 'ok' when
	This::wxWindow(),
	Option :: {'dir', integer()}.
centreOnParent(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({dir, _dir} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxWindow_CentreOnParent).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowclearbackground">external documentation</a>.
-doc """
Clears the window by filling it with the current background colour.

Does not cause an erase background event to be generated.

Notice that this uses `m:wxClientDC` to draw on the window and the results of
doing it while also drawing on `m:wxPaintDC` for this window are undefined.
Hence this method shouldn't be used from EVT_PAINT handlers, just use
`wxDC:clear/1` on the `m:wxPaintDC` you already use there instead.
""".
-spec clearBackground(This) -> 'ok' when
	This::wxWindow().
clearBackground(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_ClearBackground).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowclienttoscreen">external documentation</a>.
-doc "Converts to screen coordinates from coordinates relative to this window.".
-spec clientToScreen(This, Pt) -> {X::integer(), Y::integer()} when
	This::wxWindow(), Pt::{X::integer(), Y::integer()}.
clientToScreen(#wx_ref{type=ThisT}=This,{PtX,PtY} = Pt)
 when is_integer(PtX),is_integer(PtY) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,Pt,?get_env(),?wxWindow_ClientToScreen_1),
  wxe_util:rec(?wxWindow_ClientToScreen_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowclienttoscreen">external documentation</a>.
-doc "Converts to screen coordinates from coordinates relative to this window.".
-spec clientToScreen(This, X, Y) -> {X::integer(), Y::integer()} when
	This::wxWindow(), X::integer(), Y::integer().
clientToScreen(#wx_ref{type=ThisT}=This,X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,X,Y,?get_env(),?wxWindow_ClientToScreen_2),
  wxe_util:rec(?wxWindow_ClientToScreen_2).

%% @equiv close(This, [])
-spec close(This) -> boolean() when
	This::wxWindow().

close(This)
 when is_record(This, wx_ref) ->
  close(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowclose">external documentation</a>.
-doc """
This function simply generates a `m:wxCloseEvent` whose handler usually tries to
close the window.

It doesn't close the window itself, however.

Return: true if the event was handled and not vetoed, false otherwise.

Remark: Close calls the close handler for the window, providing an opportunity
for the window to choose whether to destroy the window. Usually it is only used
with the top level windows (`m:wxFrame` and `m:wxDialog` classes) as the others
are not supposed to have any special OnClose() logic. The close handler should
check whether the window is being deleted forcibly, using
`wxCloseEvent:canVeto/1`, in which case it should destroy the window using
`'Destroy'/1`. Note that calling Close does not guarantee that the window will
be destroyed; but it provides a way to simulate a manual close of a window,
which may or may not be implemented by destroying the window. The default
implementation of wxDialog::OnCloseWindow does not necessarily delete the
dialog, since it will simply simulate an wxID_CANCEL event which is handled by
the appropriate button event handler and may do anything at all. To guarantee
that the window will be destroyed, call `'Destroy'/1` instead

See: Window Deletion Overview, `'Destroy'/1`, `m:wxCloseEvent`
""".
-spec close(This, [Option]) -> boolean() when
	This::wxWindow(),
	Option :: {'force', boolean()}.
close(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({force, _force} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxWindow_Close),
  wxe_util:rec(?wxWindow_Close).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowconvertdialogtopixels">external documentation</a>.
-doc """
This is an overloaded member function, provided for convenience. It differs from
the above function only in what argument(s) it accepts.
""".
-spec convertDialogToPixels(This, Sz) -> {W::integer(), H::integer()} when
	This::wxWindow(), Sz::{W::integer(), H::integer()}.
convertDialogToPixels(#wx_ref{type=ThisT}=This,{SzW,SzH} = Sz)
 when is_integer(SzW),is_integer(SzH) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,Sz,?get_env(),?wxWindow_ConvertDialogToPixels),
  wxe_util:rec(?wxWindow_ConvertDialogToPixels).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowconvertpixelstodialog">external documentation</a>.
-doc """
This is an overloaded member function, provided for convenience. It differs from
the above function only in what argument(s) it accepts.
""".
-spec convertPixelsToDialog(This, Sz) -> {W::integer(), H::integer()} when
	This::wxWindow(), Sz::{W::integer(), H::integer()}.
convertPixelsToDialog(#wx_ref{type=ThisT}=This,{SzW,SzH} = Sz)
 when is_integer(SzW),is_integer(SzH) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,Sz,?get_env(),?wxWindow_ConvertPixelsToDialog),
  wxe_util:rec(?wxWindow_ConvertPixelsToDialog).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowdestroy">external documentation</a>.
-doc """
Destroys the window safely.

Use this function instead of the delete operator, since different window classes
can be destroyed differently. Frames and dialogs are not destroyed immediately
when this function is called - they are added to a list of windows to be deleted
on idle time, when all the window's events have been processed. This prevents
problems with events being sent to non-existent windows.

Return: true if the window has either been successfully deleted, or it has been
added to the list of windows pending real deletion.
""".
-spec 'Destroy'(This) -> boolean() when
	This::wxWindow().
'Destroy'(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_Destroy),
  wxe_util:rec(?wxWindow_Destroy).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowdestroychildren">external documentation</a>.
-doc """
Destroys all children of a window.

Called automatically by the destructor.
""".
-spec destroyChildren(This) -> boolean() when
	This::wxWindow().
destroyChildren(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_DestroyChildren),
  wxe_util:rec(?wxWindow_DestroyChildren).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowdisable">external documentation</a>.
-doc """
Disables the window.

Same as `enable/2` Enable(false).

Return: Returns true if the window has been disabled, false if it had been
already disabled before the call to this function.
""".
-spec disable(This) -> boolean() when
	This::wxWindow().
disable(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_Disable),
  wxe_util:rec(?wxWindow_Disable).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowdragacceptfiles">external documentation</a>.
-doc """
Enables or disables eligibility for drop file events (OnDropFiles).

Remark: Windows only until version 2.8.9, available on all platforms since
2.8.10. Cannot be used together with `setDropTarget/2` on non-Windows platforms.

See: `setDropTarget/2`
""".
-spec dragAcceptFiles(This, Accept) -> 'ok' when
	This::wxWindow(), Accept::boolean().
dragAcceptFiles(#wx_ref{type=ThisT}=This,Accept)
 when is_boolean(Accept) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,Accept,?get_env(),?wxWindow_DragAcceptFiles).

%% @equiv enable(This, [])
-spec enable(This) -> boolean() when
	This::wxWindow().

enable(This)
 when is_record(This, wx_ref) ->
  enable(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowenable">external documentation</a>.
-doc """
Enable or disable the window for user input.

Note that when a parent window is disabled, all of its children are disabled as
well and they are re-enabled again when the parent is.

A window can be created initially disabled by calling this method on it `before`
calling `create/4` to create the actual underlying window, e.g.

Return: Returns true if the window has been enabled or disabled, false if
nothing was done, i.e. if the window had already been in the specified state.

See: `isEnabled/1`, `disable/1`, `wxRadioBox:enable/3`
""".
-spec enable(This, [Option]) -> boolean() when
	This::wxWindow(),
	Option :: {'enable', boolean()}.
enable(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({enable, _enable} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxWindow_Enable),
  wxe_util:rec(?wxWindow_Enable).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowfindfocus">external documentation</a>.
-doc """
Finds the window or control which currently has the keyboard focus.

Remark: Note that this is a static function, so it can be called without needing
a `m:wxWindow` pointer.

See: `setFocus/1`, `HasFocus()` (not implemented in wx)
""".
-spec findFocus() -> wxWindow().
findFocus() ->
  wxe_util:queue_cmd(?get_env(), ?wxWindow_FindFocus),
  wxe_util:rec(?wxWindow_FindFocus).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowfindwindow">external documentation</a>.
%% <br /> Also:<br />
%% findWindow(This, Name) -> wxWindow() when<br />
%% 	This::wxWindow(), Name::unicode:chardata().<br />
%% 
-doc """
Find a child of this window, by name.

May return `this` if it matches itself.

Notice that only real children, not top level windows using this window as
parent, are searched by this function.
""".
-spec findWindow(This, Id) -> wxWindow() when
	This::wxWindow(), Id::integer();
      (This, Name) -> wxWindow() when
	This::wxWindow(), Name::unicode:chardata().
findWindow(#wx_ref{type=ThisT}=This,Id)
 when is_integer(Id) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,Id,?get_env(),?wxWindow_FindWindow_1_0),
  wxe_util:rec(?wxWindow_FindWindow_1_0);
findWindow(#wx_ref{type=ThisT}=This,Name)
 when ?is_chardata(Name) ->
  ?CLASS(ThisT,wxWindow),
  Name_UC = unicode:characters_to_binary(Name),
  wxe_util:queue_cmd(This,Name_UC,?get_env(),?wxWindow_FindWindow_1_1),
  wxe_util:rec(?wxWindow_FindWindow_1_1).

%% @equiv findWindowById(Id, [])
-spec findWindowById(Id) -> wxWindow() when
	Id::integer().

findWindowById(Id)
 when is_integer(Id) ->
  findWindowById(Id, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowfindwindowbyid">external documentation</a>.
-doc """
Find the first window with the given `id`.

If `parent` is NULL, the search will start from all top-level frames and dialog
boxes; if non-NULL, the search will be limited to the given window hierarchy.
The search is recursive in both cases.

See: `findWindow/2`

Return: Window with the given `id` or NULL if not found.
""".
-spec findWindowById(Id, [Option]) -> wxWindow() when
	Id::integer(),
	Option :: {'parent', wxWindow()}.
findWindowById(Id, Options)
 when is_integer(Id),is_list(Options) ->
  MOpts = fun({parent, #wx_ref{type=ParentT}} = Arg) ->   ?CLASS(ParentT,wxWindow),Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Id, Opts,?get_env(),?wxWindow_FindWindowById),
  wxe_util:rec(?wxWindow_FindWindowById).

%% @equiv findWindowByName(Name, [])
-spec findWindowByName(Name) -> wxWindow() when
	Name::unicode:chardata().

findWindowByName(Name)
 when ?is_chardata(Name) ->
  findWindowByName(Name, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowfindwindowbyname">external documentation</a>.
-doc """
Find a window by its name (as given in a window constructor or `create/4`
function call).

If `parent` is NULL, the search will start from all top-level frames and dialog
boxes; if non-NULL, the search will be limited to the given window hierarchy.

The search is recursive in both cases and, unlike `findWindow/2`, recurses into
top level child windows too.

If no window with such name is found, `findWindowByLabel/2` is called, i.e. the
name is interpreted as (internal) name first but if this fails, it's internal as
(user-visible) label. As this behaviour may be confusing, it is usually better
to use either the `findWindow/2` overload taking the name or
`findWindowByLabel/2` directly.

Return: Window with the given `name` or NULL if not found.
""".
-spec findWindowByName(Name, [Option]) -> wxWindow() when
	Name::unicode:chardata(),
	Option :: {'parent', wxWindow()}.
findWindowByName(Name, Options)
 when ?is_chardata(Name),is_list(Options) ->
  Name_UC = unicode:characters_to_binary(Name),
  MOpts = fun({parent, #wx_ref{type=ParentT}} = Arg) ->   ?CLASS(ParentT,wxWindow),Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Name_UC, Opts,?get_env(),?wxWindow_FindWindowByName),
  wxe_util:rec(?wxWindow_FindWindowByName).

%% @equiv findWindowByLabel(Label, [])
-spec findWindowByLabel(Label) -> wxWindow() when
	Label::unicode:chardata().

findWindowByLabel(Label)
 when ?is_chardata(Label) ->
  findWindowByLabel(Label, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowfindwindowbylabel">external documentation</a>.
-doc """
Find a window by its label.

Depending on the type of window, the label may be a window title or panel item
label. If `parent` is NULL, the search will start from all top-level frames and
dialog boxes; if non-NULL, the search will be limited to the given window
hierarchy.

The search is recursive in both cases and, unlike with `findWindow/2`, recurses
into top level child windows too.

See: `findWindow/2`

Return: Window with the given `label` or NULL if not found.
""".
-spec findWindowByLabel(Label, [Option]) -> wxWindow() when
	Label::unicode:chardata(),
	Option :: {'parent', wxWindow()}.
findWindowByLabel(Label, Options)
 when ?is_chardata(Label),is_list(Options) ->
  Label_UC = unicode:characters_to_binary(Label),
  MOpts = fun({parent, #wx_ref{type=ParentT}} = Arg) ->   ?CLASS(ParentT,wxWindow),Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Label_UC, Opts,?get_env(),?wxWindow_FindWindowByLabel),
  wxe_util:rec(?wxWindow_FindWindowByLabel).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowfit">external documentation</a>.
-doc """
Sizes the window to fit its best size.

Using this function is equivalent to setting window size to the return value of
`getBestSize/1`.

Note that, unlike `setSizerAndFit/3`, this function only changes the current
window size and doesn't change its minimal size.

See:
[Overview windowsizing](https://docs.wxwidgets.org/3.1/overview_windowsizing.html#overview_windowsizing)
""".
-spec fit(This) -> 'ok' when
	This::wxWindow().
fit(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_Fit).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowfitinside">external documentation</a>.
-doc """
Similar to `fit/1`, but sizes the interior (virtual) size of a window.

Mainly useful with scrolled windows to reset scrollbars after sizing changes
that do not trigger a size event, and/or scrolled windows without an interior
sizer. This function similarly won't do anything if there are no subwindows.
""".
-spec fitInside(This) -> 'ok' when
	This::wxWindow().
fitInside(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_FitInside).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowfreeze">external documentation</a>.
-doc """
Freezes the window or, in other words, prevents any updates from taking place on
screen, the window is not redrawn at all.

`thaw/1` must be called to re-enable window redrawing. Calls to these two
functions may be nested but to ensure that the window is properly repainted
again, you must thaw it exactly as many times as you froze it.

If the window has any children, they are recursively frozen too.

This method is useful for visual appearance optimization (for example, it is a
good idea to use it before doing many large text insertions in a row into a
`m:wxTextCtrl` under wxGTK) but is not implemented on all platforms nor for all
controls so it is mostly just a hint to wxWidgets and not a mandatory directive.

See: `wxWindowUpdateLocker` (not implemented in wx), `thaw/1`, `isFrozen/1`
""".
-spec freeze(This) -> 'ok' when
	This::wxWindow().
freeze(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_Freeze).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetacceleratortable">external documentation</a>.
-doc """
Gets the accelerator table for this window.

See `m:wxAcceleratorTable`.
""".
-spec getAcceleratorTable(This) -> wxAcceleratorTable:wxAcceleratorTable() when
	This::wxWindow().
getAcceleratorTable(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetAcceleratorTable),
  wxe_util:rec(?wxWindow_GetAcceleratorTable).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetbackgroundcolour">external documentation</a>.
-doc """
Returns the background colour of the window.

See: `setBackgroundColour/2`, `setForegroundColour/2`, `getForegroundColour/1`
""".
-spec getBackgroundColour(This) -> wx:wx_colour4() when
	This::wxWindow().
getBackgroundColour(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetBackgroundColour),
  wxe_util:rec(?wxWindow_GetBackgroundColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetbackgroundstyle">external documentation</a>.
%%<br /> Res = ?wxBG_STYLE_ERASE | ?wxBG_STYLE_SYSTEM | ?wxBG_STYLE_PAINT | ?wxBG_STYLE_COLOUR | ?wxBG_STYLE_TRANSPARENT
-doc """
Returns the background style of the window.

See: `setBackgroundColour/2`, `getForegroundColour/1`, `setBackgroundStyle/2`,
`setTransparent/2`
""".
-spec getBackgroundStyle(This) -> wx:wx_enum() when
	This::wxWindow().
getBackgroundStyle(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetBackgroundStyle),
  wxe_util:rec(?wxWindow_GetBackgroundStyle).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetbestsize">external documentation</a>.
-doc """
This functions returns the best acceptable minimal size for the window.

For example, for a static control, it will be the minimal size such that the
control label is not truncated. For windows containing subwindows (typically
`m:wxPanel`), the size returned by this function will be the same as the size
the window would have had after calling `fit/1`.

Override virtual `DoGetBestSize()` (not implemented in wx) or, better, because
it's usually more convenient, `DoGetBestClientSize()` (not implemented in wx)
when writing your own custom window class to change the value returned by this
public non-virtual method.

Notice that the best size respects the minimal and maximal size explicitly set
for the window, if any. So even if some window believes that it needs 200 pixels
horizontally, calling `setMaxSize/2` with a width of 100 would ensure that
`getBestSize/1` returns the width of at most 100 pixels.

See: `cacheBestSize/2`,
[Overview windowsizing](https://docs.wxwidgets.org/3.1/overview_windowsizing.html#overview_windowsizing)
""".
-spec getBestSize(This) -> {W::integer(), H::integer()} when
	This::wxWindow().
getBestSize(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetBestSize),
  wxe_util:rec(?wxWindow_GetBestSize).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetcaret">external documentation</a>.
-doc "Returns the caret() associated with the window.".
-spec getCaret(This) -> wxCaret:wxCaret() when
	This::wxWindow().
getCaret(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetCaret),
  wxe_util:rec(?wxWindow_GetCaret).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetcapture">external documentation</a>.
-doc """
Returns the currently captured window.

See: `hasCapture/1`, `captureMouse/1`, `releaseMouse/1`,
`m:wxMouseCaptureLostEvent`, `m:wxMouseCaptureChangedEvent`
""".
-spec getCapture() -> wxWindow().
getCapture() ->
  wxe_util:queue_cmd(?get_env(), ?wxWindow_GetCapture),
  wxe_util:rec(?wxWindow_GetCapture).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetcharheight">external documentation</a>.
-doc "Returns the character height for this window.".
-spec getCharHeight(This) -> integer() when
	This::wxWindow().
getCharHeight(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetCharHeight),
  wxe_util:rec(?wxWindow_GetCharHeight).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetcharwidth">external documentation</a>.
-doc "Returns the average character width for this window.".
-spec getCharWidth(This) -> integer() when
	This::wxWindow().
getCharWidth(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetCharWidth),
  wxe_util:rec(?wxWindow_GetCharWidth).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetchildren">external documentation</a>.
-doc """
Returns a const reference to the list of the window's children.

`wxWindowList` is a type-safe wxList-like class whose elements are of type
`wxWindow*`.
""".
-spec getChildren(This) -> [wxWindow()] when
	This::wxWindow().
getChildren(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetChildren),
  wxe_util:rec(?wxWindow_GetChildren).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetclientsize">external documentation</a>.
-doc """
This is an overloaded member function, provided for convenience. It differs from
the above function only in what argument(s) it accepts.
""".
-spec getClientSize(This) -> {W::integer(), H::integer()} when
	This::wxWindow().
getClientSize(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetClientSize),
  wxe_util:rec(?wxWindow_GetClientSize).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetcontainingsizer">external documentation</a>.
-doc "Returns the sizer of which this window is a member, if any, otherwise NULL.".
-spec getContainingSizer(This) -> wxSizer:wxSizer() when
	This::wxWindow().
getContainingSizer(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetContainingSizer),
  wxe_util:rec(?wxWindow_GetContainingSizer).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetcursor">external documentation</a>.
-doc """
Return the cursor associated with this window.

See: `setCursor/2`
""".
-spec getCursor(This) -> wxCursor:wxCursor() when
	This::wxWindow().
getCursor(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetCursor),
  wxe_util:rec(?wxWindow_GetCursor).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetdroptarget">external documentation</a>.
-doc """
Returns the associated drop target, which may be NULL.

See: `setDropTarget/2`,
[Overview dnd](https://docs.wxwidgets.org/3.1/overview_dnd.html#overview_dnd)
""".
-spec getDropTarget(This) -> wx:wx_object() when
	This::wxWindow().
getDropTarget(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetDropTarget),
  wxe_util:rec(?wxWindow_GetDropTarget).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetdpiscalefactor">external documentation</a>.
-doc """
Returns the ratio of the DPI used by this window to the standard DPI.

The returned value is 1 for standard DPI screens or 2 for "200% scaling" and,
unlike for `getContentScaleFactor/1`, is the same under all platforms.

This factor should be used to increase the size of icons and similar windows
whose best size is not based on text metrics when using DPI scaling.

E.g. the program may load a 32px bitmap if the content scale factor is 1.0 or
64px version of the same bitmap if it is 2.0 or bigger.

Notice that this method should `not` be used for window sizes expressed in
pixels, as they are already scaled by this factor by the underlying toolkit
under some platforms. Use `fromDIP/2` for anything window-related instead.

Since: 3.1.4
""".
-spec getDPIScaleFactor(This) -> number() when
	This::wxWindow().
getDPIScaleFactor(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetDPIScaleFactor),
  wxe_util:rec(?wxWindow_GetDPIScaleFactor).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetextrastyle">external documentation</a>.
-doc "Returns the extra style bits for the window.".
-spec getExtraStyle(This) -> integer() when
	This::wxWindow().
getExtraStyle(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetExtraStyle),
  wxe_util:rec(?wxWindow_GetExtraStyle).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetfont">external documentation</a>.
-doc """
Returns the font for this window.

See: `setFont/2`
""".
-spec getFont(This) -> wxFont:wxFont() when
	This::wxWindow().
getFont(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetFont),
  wxe_util:rec(?wxWindow_GetFont).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetforegroundcolour">external documentation</a>.
-doc """
Returns the foreground colour of the window.

Remark: The meaning of foreground colour varies according to the window class;
it may be the text colour or other colour, or it may not be used at all.

See: `setForegroundColour/2`, `setBackgroundColour/2`, `getBackgroundColour/1`
""".
-spec getForegroundColour(This) -> wx:wx_colour4() when
	This::wxWindow().
getForegroundColour(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetForegroundColour),
  wxe_util:rec(?wxWindow_GetForegroundColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetgrandparent">external documentation</a>.
-doc "Returns the grandparent of a window, or NULL if there isn't one.".
-spec getGrandParent(This) -> wxWindow() when
	This::wxWindow().
getGrandParent(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetGrandParent),
  wxe_util:rec(?wxWindow_GetGrandParent).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgethandle">external documentation</a>.
-doc """
Returns the platform-specific handle of the physical window.

Cast it to an appropriate handle, such as `HWND` for Windows, `Widget` for Motif
or `GtkWidget` for GTK.
""".
-spec getHandle(This) -> integer() when
	This::wxWindow().
getHandle(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetHandle),
  wxe_util:rec(?wxWindow_GetHandle).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgethelptext">external documentation</a>.
-doc """
Gets the help text to be used as context-sensitive help for this window.

Note that the text is actually stored by the current `wxHelpProvider` (not
implemented in wx) implementation, and not in the window object itself.

See: `setHelpText/2`, `GetHelpTextAtPoint()` (not implemented in wx),
`wxHelpProvider` (not implemented in wx)
""".
-spec getHelpText(This) -> unicode:charlist() when
	This::wxWindow().
getHelpText(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetHelpText),
  wxe_util:rec(?wxWindow_GetHelpText).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetid">external documentation</a>.
-doc """
Returns the identifier of the window.

Remark: Each window has an integer identifier. If the application has not
provided one (or the default wxID_ANY) a unique identifier with a negative value
will be generated.

See: `setId/2`,
[Overview windowids](https://docs.wxwidgets.org/3.1/overview_windowids.html#overview_windowids)
""".
-spec getId(This) -> integer() when
	This::wxWindow().
getId(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetId),
  wxe_util:rec(?wxWindow_GetId).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetlabel">external documentation</a>.
-doc """
Generic way of getting a label from any window, for identification purposes.

Remark: The interpretation of this function differs from class to class. For
frames and dialogs, the value returned is the title. For buttons or static text
controls, it is the button text. This function can be useful for meta-programs
(such as testing tools or special-needs access programs) which need to identify
windows by name.
""".
-spec getLabel(This) -> unicode:charlist() when
	This::wxWindow().
getLabel(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetLabel),
  wxe_util:rec(?wxWindow_GetLabel).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetmaxsize">external documentation</a>.
-doc """
Returns the maximum size of the window.

This is an indication to the sizer layout mechanism that this is the maximum
possible size as well as the upper bound on window's size settable using
`setSize/6`.

See: `GetMaxClientSize()` (not implemented in wx),
[Overview windowsizing](https://docs.wxwidgets.org/3.1/overview_windowsizing.html#overview_windowsizing)
""".
-spec getMaxSize(This) -> {W::integer(), H::integer()} when
	This::wxWindow().
getMaxSize(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetMaxSize),
  wxe_util:rec(?wxWindow_GetMaxSize).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetminsize">external documentation</a>.
-doc """
Returns the minimum size of the window, an indication to the sizer layout
mechanism that this is the minimum required size.

This method normally just returns the value set by `setMinSize/2`, but it can be
overridden to do the calculation on demand.

See: `GetMinClientSize()` (not implemented in wx),
[Overview windowsizing](https://docs.wxwidgets.org/3.1/overview_windowsizing.html#overview_windowsizing)
""".
-spec getMinSize(This) -> {W::integer(), H::integer()} when
	This::wxWindow().
getMinSize(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetMinSize),
  wxe_util:rec(?wxWindow_GetMinSize).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetname">external documentation</a>.
-doc """
Returns the window's name.

Remark: This name is not guaranteed to be unique; it is up to the programmer to
supply an appropriate name in the window constructor or via `setName/2`.

See: `setName/2`
""".
-spec getName(This) -> unicode:charlist() when
	This::wxWindow().
getName(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetName),
  wxe_util:rec(?wxWindow_GetName).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetparent">external documentation</a>.
-doc "Returns the parent of the window, or NULL if there is no parent.".
-spec getParent(This) -> wxWindow() when
	This::wxWindow().
getParent(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetParent),
  wxe_util:rec(?wxWindow_GetParent).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetposition">external documentation</a>.
-doc """
This gets the position of the window in pixels, relative to the parent window
for the child windows or relative to the display origin for the top level
windows.

See: `getScreenPosition/1`
""".
-spec getPosition(This) -> {X::integer(), Y::integer()} when
	This::wxWindow().
getPosition(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetPosition),
  wxe_util:rec(?wxWindow_GetPosition).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetrect">external documentation</a>.
-doc """
Returns the position and size of the window as a \{X,Y,W,H\} object.

See: `getScreenRect/1`
""".
-spec getRect(This) -> {X::integer(), Y::integer(), W::integer(), H::integer()} when
	This::wxWindow().
getRect(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetRect),
  wxe_util:rec(?wxWindow_GetRect).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetscreenposition">external documentation</a>.
-doc """
Returns the window position in screen coordinates, whether the window is a child
window or a top level one.

See: `getPosition/1`
""".
-spec getScreenPosition(This) -> {X::integer(), Y::integer()} when
	This::wxWindow().
getScreenPosition(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetScreenPosition),
  wxe_util:rec(?wxWindow_GetScreenPosition).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetscreenrect">external documentation</a>.
-doc """
Returns the position and size of the window on the screen as a \{X,Y,W,H\}
object.

See: `getRect/1`
""".
-spec getScreenRect(This) -> {X::integer(), Y::integer(), W::integer(), H::integer()} when
	This::wxWindow().
getScreenRect(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetScreenRect),
  wxe_util:rec(?wxWindow_GetScreenRect).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetscrollpos">external documentation</a>.
-doc """
Returns the built-in scrollbar position.

See: `setScrollbar/6`
""".
-spec getScrollPos(This, Orientation) -> integer() when
	This::wxWindow(), Orientation::integer().
getScrollPos(#wx_ref{type=ThisT}=This,Orientation)
 when is_integer(Orientation) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,Orientation,?get_env(),?wxWindow_GetScrollPos),
  wxe_util:rec(?wxWindow_GetScrollPos).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetscrollrange">external documentation</a>.
-doc """
Returns the built-in scrollbar range.

See: `setScrollbar/6`
""".
-spec getScrollRange(This, Orientation) -> integer() when
	This::wxWindow(), Orientation::integer().
getScrollRange(#wx_ref{type=ThisT}=This,Orientation)
 when is_integer(Orientation) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,Orientation,?get_env(),?wxWindow_GetScrollRange),
  wxe_util:rec(?wxWindow_GetScrollRange).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetscrollthumb">external documentation</a>.
-doc """
Returns the built-in scrollbar thumb size.

See: `setScrollbar/6`
""".
-spec getScrollThumb(This, Orientation) -> integer() when
	This::wxWindow(), Orientation::integer().
getScrollThumb(#wx_ref{type=ThisT}=This,Orientation)
 when is_integer(Orientation) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,Orientation,?get_env(),?wxWindow_GetScrollThumb),
  wxe_util:rec(?wxWindow_GetScrollThumb).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetsize">external documentation</a>.
-doc "See the GetSize(int*,int*) overload for more info.".
-spec getSize(This) -> {W::integer(), H::integer()} when
	This::wxWindow().
getSize(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetSize),
  wxe_util:rec(?wxWindow_GetSize).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetsizer">external documentation</a>.
-doc """
Returns the sizer associated with the window by a previous call to `setSizer/3`,
or NULL.
""".
-spec getSizer(This) -> wxSizer:wxSizer() when
	This::wxWindow().
getSizer(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetSizer),
  wxe_util:rec(?wxWindow_GetSizer).

%% @equiv getTextExtent(This,String, [])
-spec getTextExtent(This, String) -> Result when
	Result ::{W::integer(), H::integer(), Descent::integer(), ExternalLeading::integer()},
	This::wxWindow(), String::unicode:chardata().

getTextExtent(This,String)
 when is_record(This, wx_ref),?is_chardata(String) ->
  getTextExtent(This,String, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgettextextent">external documentation</a>.
-doc """
Gets the dimensions of the string as it would be drawn on the window with the
currently selected font.

The text extent is returned in the `w` and `h` pointers.
""".
-spec getTextExtent(This, String, [Option]) -> Result when
	Result :: {W::integer(), H::integer(), Descent::integer(), ExternalLeading::integer()},
	This::wxWindow(), String::unicode:chardata(),
	Option :: {'theFont', wxFont:wxFont()}.
getTextExtent(#wx_ref{type=ThisT}=This,String, Options)
 when ?is_chardata(String),is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  String_UC = unicode:characters_to_binary(String),
  MOpts = fun({theFont, #wx_ref{type=TheFontT}} = Arg) ->   ?CLASS(TheFontT,wxFont),Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,String_UC, Opts,?get_env(),?wxWindow_GetTextExtent),
  wxe_util:rec(?wxWindow_GetTextExtent).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetthemeenabled">external documentation</a>.
-doc """
Returns true if the window uses the system theme for drawing its background.

See: `setThemeEnabled/2`
""".
-spec getThemeEnabled(This) -> boolean() when
	This::wxWindow().
getThemeEnabled(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetThemeEnabled),
  wxe_util:rec(?wxWindow_GetThemeEnabled).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgettooltip">external documentation</a>.
-doc "Get the associated tooltip or NULL if none.".
-spec getToolTip(This) -> wxToolTip:wxToolTip() when
	This::wxWindow().
getToolTip(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetToolTip),
  wxe_util:rec(?wxWindow_GetToolTip).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetupdateregion">external documentation</a>.
-doc """
Gets the dimensions of the string as it would be drawn on the window with the
currently selected font.

Returns the region specifying which parts of the window have been damaged.
Should only be called within an `m:wxPaintEvent` handler.

See: `m:wxRegion`, `wxRegionIterator` (not implemented in wx)
""".
-spec getUpdateRegion(This) -> wxRegion:wxRegion() when
	This::wxWindow().
getUpdateRegion(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetUpdateRegion),
  wxe_util:rec(?wxWindow_GetUpdateRegion).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetvirtualsize">external documentation</a>.
-doc """
This gets the virtual size of the window in pixels.

By default it returns the client size of the window, but after a call to
`setVirtualSize/3` it will return the size set with that method.

See:
[Overview windowsizing](https://docs.wxwidgets.org/3.1/overview_windowsizing.html#overview_windowsizing)
""".
-spec getVirtualSize(This) -> {W::integer(), H::integer()} when
	This::wxWindow().
getVirtualSize(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetVirtualSize),
  wxe_util:rec(?wxWindow_GetVirtualSize).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetwindowstyleflag">external documentation</a>.
-doc """
Gets the window style that was passed to the constructor or `create/4` method.

`GetWindowStyle()` (not implemented in wx) is another name for the same
function.
""".
-spec getWindowStyleFlag(This) -> integer() when
	This::wxWindow().
getWindowStyleFlag(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetWindowStyleFlag),
  wxe_util:rec(?wxWindow_GetWindowStyleFlag).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetwindowvariant">external documentation</a>.
%%<br /> Res = ?wxWINDOW_VARIANT_NORMAL | ?wxWINDOW_VARIANT_SMALL | ?wxWINDOW_VARIANT_MINI | ?wxWINDOW_VARIANT_LARGE | ?wxWINDOW_VARIANT_MAX
-doc "Returns the value previously passed to `setWindowVariant/2`.".
-spec getWindowVariant(This) -> wx:wx_enum() when
	This::wxWindow().
getWindowVariant(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetWindowVariant),
  wxe_util:rec(?wxWindow_GetWindowVariant).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowhascapture">external documentation</a>.
-doc """
Returns true if this window has the current mouse capture.

See: `captureMouse/1`, `releaseMouse/1`, `m:wxMouseCaptureLostEvent`,
`m:wxMouseCaptureChangedEvent`
""".
-spec hasCapture(This) -> boolean() when
	This::wxWindow().
hasCapture(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_HasCapture),
  wxe_util:rec(?wxWindow_HasCapture).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowhasscrollbar">external documentation</a>.
-doc """
Returns true if this window currently has a scroll bar for this orientation.

This method may return false even when `CanScroll()` (not implemented in wx) for
the same orientation returns true, but if `CanScroll()` (not implemented in wx)
returns false, i.e. scrolling in this direction is not enabled at all,
`hasScrollbar/2` always returns false as well.
""".
-spec hasScrollbar(This, Orient) -> boolean() when
	This::wxWindow(), Orient::integer().
hasScrollbar(#wx_ref{type=ThisT}=This,Orient)
 when is_integer(Orient) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,Orient,?get_env(),?wxWindow_HasScrollbar),
  wxe_util:rec(?wxWindow_HasScrollbar).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowhastransparentbackground">external documentation</a>.
-doc """
Returns true if this window background is transparent (as, for example, for
`m:wxStaticText`) and should show the parent window background.

This method is mostly used internally by the library itself and you normally
shouldn't have to call it. You may, however, have to override it in your
wxWindow-derived class to ensure that background is painted correctly.
""".
-spec hasTransparentBackground(This) -> boolean() when
	This::wxWindow().
hasTransparentBackground(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_HasTransparentBackground),
  wxe_util:rec(?wxWindow_HasTransparentBackground).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowhide">external documentation</a>.
-doc "Equivalent to calling `show/2`(false).".
-spec hide(This) -> boolean() when
	This::wxWindow().
hide(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_Hide),
  wxe_util:rec(?wxWindow_Hide).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowinheritattributes">external documentation</a>.
-doc """
This function is (or should be, in case of custom controls) called during window
creation to intelligently set up the window visual attributes, that is the font
and the foreground and background colours.

By "intelligently" the following is meant: by default, all windows use their own
`GetClassDefaultAttributes()` (not implemented in wx) default attributes.
However if some of the parents attributes are explicitly (that is, using
`setFont/2` and not `setOwnFont/2`) changed and if the corresponding attribute
hadn't been explicitly set for this window itself, then this window takes the
same value as used by the parent. In addition, if the window overrides
`shouldInheritColours/1` to return false, the colours will not be changed no
matter what and only the font might.

This rather complicated logic is necessary in order to accommodate the different
usage scenarios. The most common one is when all default attributes are used and
in this case, nothing should be inherited as in modern GUIs different controls
use different fonts (and colours) than their siblings so they can't inherit the
same value from the parent. However it was also deemed desirable to allow to
simply change the attributes of all children at once by just changing the font
or colour of their common parent, hence in this case we do inherit the parents
attributes.
""".
-spec inheritAttributes(This) -> 'ok' when
	This::wxWindow().
inheritAttributes(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_InheritAttributes).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowinitdialog">external documentation</a>.
-doc """
Sends an `wxEVT_INIT_DIALOG` event, whose handler usually transfers data to the
dialog via validators.
""".
-spec initDialog(This) -> 'ok' when
	This::wxWindow().
initDialog(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_InitDialog).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowinvalidatebestsize">external documentation</a>.
-doc """
Resets the cached best size value so it will be recalculated the next time it is
needed.

See: `cacheBestSize/2`
""".
-spec invalidateBestSize(This) -> 'ok' when
	This::wxWindow().
invalidateBestSize(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_InvalidateBestSize).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowisfrozen">external documentation</a>.
-doc """
Returns true if the window is currently frozen by a call to `freeze/1`.

See: `freeze/1`, `thaw/1`
""".
-spec isFrozen(This) -> boolean() when
	This::wxWindow().
isFrozen(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_IsFrozen),
  wxe_util:rec(?wxWindow_IsFrozen).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowisenabled">external documentation</a>.
-doc """
Returns true if the window is enabled, i.e. if it accepts user input, false
otherwise.

Notice that this method can return false even if this window itself hadn't been
explicitly disabled when one of its parent windows is disabled. To get the
intrinsic status of this window, use `IsThisEnabled()` (not implemented in wx)

See: `enable/2`
""".
-spec isEnabled(This) -> boolean() when
	This::wxWindow().
isEnabled(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_IsEnabled),
  wxe_util:rec(?wxWindow_IsEnabled).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowisexposed">external documentation</a>.
%% <br /> Also:<br />
%% isExposed(This, Rect) -> boolean() when<br />
%% 	This::wxWindow(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()}.<br />
%% 
-doc """
This is an overloaded member function, provided for convenience. It differs from
the above function only in what argument(s) it accepts.
""".
-spec isExposed(This, Pt) -> boolean() when
	This::wxWindow(), Pt::{X::integer(), Y::integer()};
      (This, Rect) -> boolean() when
	This::wxWindow(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()}.
isExposed(#wx_ref{type=ThisT}=This,{PtX,PtY} = Pt)
 when is_integer(PtX),is_integer(PtY) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,Pt,?get_env(),?wxWindow_IsExposed_1_0),
  wxe_util:rec(?wxWindow_IsExposed_1_0);
isExposed(#wx_ref{type=ThisT}=This,{RectX,RectY,RectW,RectH} = Rect)
 when is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,Rect,?get_env(),?wxWindow_IsExposed_1_1),
  wxe_util:rec(?wxWindow_IsExposed_1_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowisexposed">external documentation</a>.
-doc """
Returns true if the given point or rectangle area has been exposed since the
last repaint.

Call this in an paint event handler to optimize redrawing by only redrawing
those areas, which have been exposed.
""".
-spec isExposed(This, X, Y) -> boolean() when
	This::wxWindow(), X::integer(), Y::integer().
isExposed(#wx_ref{type=ThisT}=This,X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,X,Y,?get_env(),?wxWindow_IsExposed_2),
  wxe_util:rec(?wxWindow_IsExposed_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowisexposed">external documentation</a>.
-doc """
This is an overloaded member function, provided for convenience. It differs from
the above function only in what argument(s) it accepts.
""".
-spec isExposed(This, X, Y, W, H) -> boolean() when
	This::wxWindow(), X::integer(), Y::integer(), W::integer(), H::integer().
isExposed(#wx_ref{type=ThisT}=This,X,Y,W,H)
 when is_integer(X),is_integer(Y),is_integer(W),is_integer(H) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,X,Y,W,H,?get_env(),?wxWindow_IsExposed_4),
  wxe_util:rec(?wxWindow_IsExposed_4).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowisretained">external documentation</a>.
-doc """
Returns true if the window is retained, false otherwise.

Remark: Retained windows are only available on X platforms.
""".
-spec isRetained(This) -> boolean() when
	This::wxWindow().
isRetained(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_IsRetained),
  wxe_util:rec(?wxWindow_IsRetained).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowisshown">external documentation</a>.
-doc """
Returns true if the window is shown, false if it has been hidden.

See: `isShownOnScreen/1`
""".
-spec isShown(This) -> boolean() when
	This::wxWindow().
isShown(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_IsShown),
  wxe_util:rec(?wxWindow_IsShown).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowistoplevel">external documentation</a>.
-doc """
Returns true if the given window is a top-level one.

Currently all frames and dialogs are considered to be top-level windows (even if
they have a parent window).
""".
-spec isTopLevel(This) -> boolean() when
	This::wxWindow().
isTopLevel(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_IsTopLevel),
  wxe_util:rec(?wxWindow_IsTopLevel).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowisshownonscreen">external documentation</a>.
-doc """
Returns true if the window is physically visible on the screen, i.e. it is shown
and all its parents up to the toplevel window are shown as well.

See: `isShown/1`
""".
-spec isShownOnScreen(This) -> boolean() when
	This::wxWindow().
isShownOnScreen(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_IsShownOnScreen),
  wxe_util:rec(?wxWindow_IsShownOnScreen).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowlayout">external documentation</a>.
-doc """
Lays out the children of this window using the associated sizer.

If a sizer hadn't been associated with this window (see `setSizer/3`), this
function doesn't do anything, unless this is a top level window (see
`layout/1`).

Note that this method is called automatically when the window size changes if it
has the associated sizer (or if `setAutoLayout/2` with true argument had been
explicitly called), ensuring that it is always laid out correctly.

See:
[Overview windowsizing](https://docs.wxwidgets.org/3.1/overview_windowsizing.html#overview_windowsizing)

Return: Always returns true, the return value is not useful.
""".
-spec layout(This) -> boolean() when
	This::wxWindow().
layout(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_Layout),
  wxe_util:rec(?wxWindow_Layout).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowlinedown">external documentation</a>.
-doc "Same as `scrollLines/2` (1).".
-spec lineDown(This) -> boolean() when
	This::wxWindow().
lineDown(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_LineDown),
  wxe_util:rec(?wxWindow_LineDown).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowlineup">external documentation</a>.
-doc "Same as `scrollLines/2` (-1).".
-spec lineUp(This) -> boolean() when
	This::wxWindow().
lineUp(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_LineUp),
  wxe_util:rec(?wxWindow_LineUp).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowlower">external documentation</a>.
-doc """
Lowers the window to the bottom of the window hierarchy (Z-order).

Remark: This function only works for wxTopLevelWindow-derived classes.

See: `raise/1`
""".
-spec lower(This) -> 'ok' when
	This::wxWindow().
lower(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_Lower).

%% @equiv move(This,Pt, [])
-spec move(This, Pt) -> 'ok' when
	This::wxWindow(), Pt::{X::integer(), Y::integer()}.

move(This,{PtX,PtY} = Pt)
 when is_record(This, wx_ref),is_integer(PtX),is_integer(PtY) ->
  move(This,Pt, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowmove">external documentation</a>.
%% <br /> Also:<br />
%% move(This, Pt, [Option]) -> 'ok' when<br />
%% 	This::wxWindow(), Pt::{X::integer(), Y::integer()},<br />
%% 	Option :: {'flags', integer()}.<br />
%% 
-doc """
Moves the window to the given position.

Remark: Implementations of `setSize/6` can also implicitly implement the
`move/4` function, which is defined in the base `m:wxWindow` class as the call:

See: `setSize/6`
""".
-spec move(This, X, Y) -> 'ok' when
	This::wxWindow(), X::integer(), Y::integer();
      (This, Pt, [Option]) -> 'ok' when
	This::wxWindow(), Pt::{X::integer(), Y::integer()},
	Option :: {'flags', integer()}.

move(This,X,Y)
 when is_record(This, wx_ref),is_integer(X),is_integer(Y) ->
  move(This,X,Y, []);
move(#wx_ref{type=ThisT}=This,{PtX,PtY} = Pt, Options)
 when is_integer(PtX),is_integer(PtY),is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({flags, _flags} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Pt, Opts,?get_env(),?wxWindow_Move_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowmove">external documentation</a>.
-doc """
Moves the window to the given position.

Remark: Implementations of SetSize can also implicitly implement the `move/4`
function, which is defined in the base `m:wxWindow` class as the call:

See: `setSize/6`
""".
-spec move(This, X, Y, [Option]) -> 'ok' when
	This::wxWindow(), X::integer(), Y::integer(),
	Option :: {'flags', integer()}.
move(#wx_ref{type=ThisT}=This,X,Y, Options)
 when is_integer(X),is_integer(Y),is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({flags, _flags} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,X,Y, Opts,?get_env(),?wxWindow_Move_3).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowmoveafterintaborder">external documentation</a>.
-doc """
Moves this window in the tab navigation order after the specified `win`.

This means that when the user presses `TAB` key on that other window, the focus
switches to this window.

Default tab order is the same as creation order, this function and
`moveBeforeInTabOrder/2` allow to change it after creating all the windows.
""".
-spec moveAfterInTabOrder(This, Win) -> 'ok' when
	This::wxWindow(), Win::wxWindow().
moveAfterInTabOrder(#wx_ref{type=ThisT}=This,#wx_ref{type=WinT}=Win) ->
  ?CLASS(ThisT,wxWindow),
  ?CLASS(WinT,wxWindow),
  wxe_util:queue_cmd(This,Win,?get_env(),?wxWindow_MoveAfterInTabOrder).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowmovebeforeintaborder">external documentation</a>.
-doc """
Same as `moveAfterInTabOrder/2` except that it inserts this window just before
`win` instead of putting it right after it.
""".
-spec moveBeforeInTabOrder(This, Win) -> 'ok' when
	This::wxWindow(), Win::wxWindow().
moveBeforeInTabOrder(#wx_ref{type=ThisT}=This,#wx_ref{type=WinT}=Win) ->
  ?CLASS(ThisT,wxWindow),
  ?CLASS(WinT,wxWindow),
  wxe_util:queue_cmd(This,Win,?get_env(),?wxWindow_MoveBeforeInTabOrder).

%% @equiv navigate(This, [])
-spec navigate(This) -> boolean() when
	This::wxWindow().

navigate(This)
 when is_record(This, wx_ref) ->
  navigate(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindownavigate">external documentation</a>.
-doc """
Performs a keyboard navigation action starting from this window.

This method is equivalent to calling `NavigateIn()` (not implemented in wx)
method on the parent window.

Return: Returns true if the focus was moved to another window or false if
nothing changed.

Remark: You may wish to call this from a text control custom keypress handler to
do the default navigation behaviour for the tab key, since the standard default
behaviour for a multiline text control with the wxTE_PROCESS_TAB style is to
insert a tab and not navigate to the next control. See also
`m:wxNavigationKeyEvent` and HandleAsNavigationKey.
""".
-spec navigate(This, [Option]) -> boolean() when
	This::wxWindow(),
	Option :: {'flags', integer()}.
navigate(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({flags, _flags} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxWindow_Navigate),
  wxe_util:rec(?wxWindow_Navigate).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowpagedown">external documentation</a>.
-doc "Same as `scrollPages/2` (1).".
-spec pageDown(This) -> boolean() when
	This::wxWindow().
pageDown(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_PageDown),
  wxe_util:rec(?wxWindow_PageDown).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowpageup">external documentation</a>.
-doc "Same as `scrollPages/2` (-1).".
-spec pageUp(This) -> boolean() when
	This::wxWindow().
pageUp(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_PageUp),
  wxe_util:rec(?wxWindow_PageUp).

%% @equiv popupMenu(This,Menu, [])
-spec popupMenu(This, Menu) -> boolean() when
	This::wxWindow(), Menu::wxMenu:wxMenu().

popupMenu(This,Menu)
 when is_record(This, wx_ref),is_record(Menu, wx_ref) ->
  popupMenu(This,Menu, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowpopupmenu">external documentation</a>.
-doc """
Pops up the given menu at the specified coordinates, relative to this window,
and returns control when the user has dismissed the menu.

If a menu item is selected, the corresponding menu event is generated and will
be processed as usual. If coordinates are not specified, the current mouse
cursor position is used.

`menu` is the menu to pop up.

The position where the menu will appear can be specified either as a \{X,Y\}
`pos` or by two integers (`x` and `y`).

Note that this function switches focus to this window before showing the menu.

Remark: Just before the menu is popped up, `wxMenu::UpdateUI` (not implemented
in wx) is called to ensure that the menu items are in the correct state. The
menu does not get deleted by the window. It is recommended to not explicitly
specify coordinates when calling PopupMenu in response to mouse click, because
some of the ports (namely, wxGTK) can do a better job of positioning the menu in
that case.

See: `m:wxMenu`
""".
-spec popupMenu(This, Menu, [Option]) -> boolean() when
	This::wxWindow(), Menu::wxMenu:wxMenu(),
	Option :: {'pos', {X::integer(), Y::integer()}}.
popupMenu(#wx_ref{type=ThisT}=This,#wx_ref{type=MenuT}=Menu, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  ?CLASS(MenuT,wxMenu),
  MOpts = fun({pos, {_posX,_posY}} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Menu, Opts,?get_env(),?wxWindow_PopupMenu_2),
  wxe_util:rec(?wxWindow_PopupMenu_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowpopupmenu">external documentation</a>.
-doc """
This is an overloaded member function, provided for convenience. It differs from
the above function only in what argument(s) it accepts.
""".
-spec popupMenu(This, Menu, X, Y) -> boolean() when
	This::wxWindow(), Menu::wxMenu:wxMenu(), X::integer(), Y::integer().
popupMenu(#wx_ref{type=ThisT}=This,#wx_ref{type=MenuT}=Menu,X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxWindow),
  ?CLASS(MenuT,wxMenu),
  wxe_util:queue_cmd(This,Menu,X,Y,?get_env(),?wxWindow_PopupMenu_3),
  wxe_util:rec(?wxWindow_PopupMenu_3).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowraise">external documentation</a>.
-doc """
Raises the window to the top of the window hierarchy (Z-order).

Notice that this function only requests the window manager to raise this window
to the top of Z-order. Depending on its configuration, the window manager may
raise the window, not do it at all or indicate that a window requested to be
raised in some other way, e.g. by flashing its icon if it is minimized.

Remark: This function only works for wxTopLevelWindow-derived classes.

See: `lower/1`
""".
-spec raise(This) -> 'ok' when
	This::wxWindow().
raise(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_Raise).

%% @equiv refresh(This, [])
-spec refresh(This) -> 'ok' when
	This::wxWindow().

refresh(This)
 when is_record(This, wx_ref) ->
  refresh(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowrefresh">external documentation</a>.
-doc """
Causes this window, and all of its children recursively (except under wxGTK1
where this is not implemented), to be repainted.

Note that repainting doesn't happen immediately but only during the next event
loop iteration, if you need to update the window immediately you should use
`update/1` instead.

See: `refreshRect/3`
""".
-spec refresh(This, [Option]) -> 'ok' when
	This::wxWindow(),
	Option :: {'eraseBackground', boolean()}
		 | {'rect', {X::integer(), Y::integer(), W::integer(), H::integer()}}.
refresh(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({eraseBackground, _eraseBackground} = Arg) -> Arg;
          ({rect, {_rectX,_rectY,_rectW,_rectH}} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxWindow_Refresh).

%% @equiv refreshRect(This,Rect, [])
-spec refreshRect(This, Rect) -> 'ok' when
	This::wxWindow(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()}.

refreshRect(This,{RectX,RectY,RectW,RectH} = Rect)
 when is_record(This, wx_ref),is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH) ->
  refreshRect(This,Rect, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowrefreshrect">external documentation</a>.
-doc """
Redraws the contents of the given rectangle: only the area inside it will be
repainted.

This is the same as `refresh/2` but has a nicer syntax as it can be called with
a temporary \{X,Y,W,H\} object as argument like this
`RefreshRect(wxRect(x, y, w, h))`.
""".
-spec refreshRect(This, Rect, [Option]) -> 'ok' when
	This::wxWindow(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()},
	Option :: {'eraseBackground', boolean()}.
refreshRect(#wx_ref{type=ThisT}=This,{RectX,RectY,RectW,RectH} = Rect, Options)
 when is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH),is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({eraseBackground, _eraseBackground} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Rect, Opts,?get_env(),?wxWindow_RefreshRect).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowreleasemouse">external documentation</a>.
-doc """
Releases mouse input captured with `captureMouse/1`.

See: `captureMouse/1`, `hasCapture/1`, `releaseMouse/1`,
`m:wxMouseCaptureLostEvent`, `m:wxMouseCaptureChangedEvent`
""".
-spec releaseMouse(This) -> 'ok' when
	This::wxWindow().
releaseMouse(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_ReleaseMouse).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowremovechild">external documentation</a>.
-doc """
Removes a child window.

This is called automatically by window deletion functions so should not be
required by the application programmer. Notice that this function is mostly
internal to wxWidgets and shouldn't be called by the user code.
""".
-spec removeChild(This, Child) -> 'ok' when
	This::wxWindow(), Child::wxWindow().
removeChild(#wx_ref{type=ThisT}=This,#wx_ref{type=ChildT}=Child) ->
  ?CLASS(ThisT,wxWindow),
  ?CLASS(ChildT,wxWindow),
  wxe_util:queue_cmd(This,Child,?get_env(),?wxWindow_RemoveChild).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowreparent">external documentation</a>.
-doc """
Reparents the window, i.e. the window will be removed from its current parent
window (e.g.

a non-standard toolbar in a `m:wxFrame`) and then re-inserted into another.

Notice that currently you need to explicitly call `wxBookCtrlBase:removePage/2`
before reparenting a notebook page.
""".
-spec reparent(This, NewParent) -> boolean() when
	This::wxWindow(), NewParent::wxWindow().
reparent(#wx_ref{type=ThisT}=This,#wx_ref{type=NewParentT}=NewParent) ->
  ?CLASS(ThisT,wxWindow),
  ?CLASS(NewParentT,wxWindow),
  wxe_util:queue_cmd(This,NewParent,?get_env(),?wxWindow_Reparent),
  wxe_util:rec(?wxWindow_Reparent).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowscreentoclient">external documentation</a>.
-doc "Converts from screen to client window coordinates.".
-spec screenToClient(This) -> {X::integer(), Y::integer()} when
	This::wxWindow().
screenToClient(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_ScreenToClient_2),
  wxe_util:rec(?wxWindow_ScreenToClient_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowscreentoclient">external documentation</a>.
-doc "Converts from screen to client window coordinates.".
-spec screenToClient(This, Pt) -> {X::integer(), Y::integer()} when
	This::wxWindow(), Pt::{X::integer(), Y::integer()}.
screenToClient(#wx_ref{type=ThisT}=This,{PtX,PtY} = Pt)
 when is_integer(PtX),is_integer(PtY) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,Pt,?get_env(),?wxWindow_ScreenToClient_1),
  wxe_util:rec(?wxWindow_ScreenToClient_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowscrolllines">external documentation</a>.
-doc """
Scrolls the window by the given number of lines down (if `lines` is positive) or
up.

Return: Returns true if the window was scrolled, false if it was already on
top/bottom and nothing was done.

Remark: This function is currently only implemented under MSW and `m:wxTextCtrl`
under wxGTK (it also works for `wxScrolled` (not implemented in wx) classes
under all platforms).

See: `scrollPages/2`
""".
-spec scrollLines(This, Lines) -> boolean() when
	This::wxWindow(), Lines::integer().
scrollLines(#wx_ref{type=ThisT}=This,Lines)
 when is_integer(Lines) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,Lines,?get_env(),?wxWindow_ScrollLines),
  wxe_util:rec(?wxWindow_ScrollLines).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowscrollpages">external documentation</a>.
-doc """
Scrolls the window by the given number of pages down (if `pages` is positive) or
up.

Return: Returns true if the window was scrolled, false if it was already on
top/bottom and nothing was done.

Remark: This function is currently only implemented under MSW and wxGTK.

See: `scrollLines/2`
""".
-spec scrollPages(This, Pages) -> boolean() when
	This::wxWindow(), Pages::integer().
scrollPages(#wx_ref{type=ThisT}=This,Pages)
 when is_integer(Pages) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,Pages,?get_env(),?wxWindow_ScrollPages),
  wxe_util:rec(?wxWindow_ScrollPages).

%% @equiv scrollWindow(This,Dx,Dy, [])
-spec scrollWindow(This, Dx, Dy) -> 'ok' when
	This::wxWindow(), Dx::integer(), Dy::integer().

scrollWindow(This,Dx,Dy)
 when is_record(This, wx_ref),is_integer(Dx),is_integer(Dy) ->
  scrollWindow(This,Dx,Dy, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowscrollwindow">external documentation</a>.
-doc """
Physically scrolls the pixels in the window and move child windows accordingly.

Remark: Note that you can often use `wxScrolled` (not implemented in wx) instead
of using this function directly.
""".
-spec scrollWindow(This, Dx, Dy, [Option]) -> 'ok' when
	This::wxWindow(), Dx::integer(), Dy::integer(),
	Option :: {'rect', {X::integer(), Y::integer(), W::integer(), H::integer()}}.
scrollWindow(#wx_ref{type=ThisT}=This,Dx,Dy, Options)
 when is_integer(Dx),is_integer(Dy),is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({rect, {_rectX,_rectY,_rectW,_rectH}} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Dx,Dy, Opts,?get_env(),?wxWindow_ScrollWindow).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetacceleratortable">external documentation</a>.
-doc """
Sets the accelerator table for this window.

See `m:wxAcceleratorTable`.
""".
-spec setAcceleratorTable(This, Accel) -> 'ok' when
	This::wxWindow(), Accel::wxAcceleratorTable:wxAcceleratorTable().
setAcceleratorTable(#wx_ref{type=ThisT}=This,#wx_ref{type=AccelT}=Accel) ->
  ?CLASS(ThisT,wxWindow),
  ?CLASS(AccelT,wxAcceleratorTable),
  wxe_util:queue_cmd(This,Accel,?get_env(),?wxWindow_SetAcceleratorTable).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetautolayout">external documentation</a>.
-doc """
Determines whether the `layout/1` function will be called automatically when the
window is resized.

This method is called implicitly by `setSizer/3` but if you use
`SetConstraints()` (not implemented in wx) you should call it manually or
otherwise the window layout won't be correctly updated when its size changes.

See: `setSizer/3`, `SetConstraints()` (not implemented in wx)
""".
-spec setAutoLayout(This, AutoLayout) -> 'ok' when
	This::wxWindow(), AutoLayout::boolean().
setAutoLayout(#wx_ref{type=ThisT}=This,AutoLayout)
 when is_boolean(AutoLayout) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,AutoLayout,?get_env(),?wxWindow_SetAutoLayout).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetbackgroundcolour">external documentation</a>.
-doc """
Sets the background colour of the window.

Notice that as with `setForegroundColour/2`, setting the background colour of a
native control may not affect the entire control and could be not supported at
all depending on the control and platform.

Please see `inheritAttributes/1` for explanation of the difference between this
method and `setOwnBackgroundColour/2`.

Remark: The background colour is usually painted by the default `m:wxEraseEvent`
event handler function under Windows and automatically under GTK. Note that
setting the background colour does not cause an immediate refresh, so you may
wish to call `clearBackground/1` or `refresh/2` after calling this function.
Using this function will disable attempts to use themes for this window, if the
system supports them. Use with care since usually the themes represent the
appearance chosen by the user to be used for all applications on the system.

Return: true if the colour was really changed, false if it was already set to
this colour and nothing was done.

See: `getBackgroundColour/1`, `setForegroundColour/2`, `getForegroundColour/1`,
`clearBackground/1`, `refresh/2`, `m:wxEraseEvent`, `m:wxSystemSettings`
""".
-spec setBackgroundColour(This, Colour) -> boolean() when
	This::wxWindow(), Colour::wx:wx_colour().
setBackgroundColour(#wx_ref{type=ThisT}=This,Colour)
 when ?is_colordata(Colour) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,wxe_util:color(Colour),?get_env(),?wxWindow_SetBackgroundColour),
  wxe_util:rec(?wxWindow_SetBackgroundColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetbackgroundstyle">external documentation</a>.
%%<br /> Style = ?wxBG_STYLE_ERASE | ?wxBG_STYLE_SYSTEM | ?wxBG_STYLE_PAINT | ?wxBG_STYLE_COLOUR | ?wxBG_STYLE_TRANSPARENT
-doc """
Sets the background style of the window.

The default background style is `wxBG_STYLE_ERASE` which indicates that the
window background may be erased in `EVT_ERASE_BACKGROUND` handler. This is a
safe, compatibility default; however you may want to change it to
`wxBG_STYLE_SYSTEM` if you don't define any erase background event handlers at
all, to avoid unnecessary generation of erase background events and always let
system erase the background. And you should change the background style to
`wxBG_STYLE_PAINT` if you define an `EVT_PAINT` handler which completely
overwrites the window background as in this case erasing it previously, either
in `EVT_ERASE_BACKGROUND` handler or in the system default handler, would result
in flicker as the background pixels will be repainted twice every time the
window is redrawn. Do ensure that the background is entirely erased by your
`EVT_PAINT` handler in this case however as otherwise garbage may be left on
screen.

Notice that in previous versions of wxWidgets a common way to work around the
above mentioned flickering problem was to define an empty `EVT_ERASE_BACKGROUND`
handler. Setting background style to `wxBG_STYLE_PAINT` is a simpler and more
efficient solution to the same problem.

Under wxGTK and wxOSX, you can use ?wxBG_STYLE_TRANSPARENT to obtain full
transparency of the window background. Note that wxGTK supports this only since
GTK 2.12 with a compositing manager enabled, call
`IsTransparentBackgroundSupported()` (not implemented in wx) to check whether
this is the case.

Also, in order for `SetBackgroundStyle(wxBG_STYLE_TRANSPARENT)` to work, it must
be called before `create/4`. If you're using your own wxWindow-derived class you
should write your code in the following way:

See: `setBackgroundColour/2`, `getForegroundColour/1`, `setTransparent/2`,
`IsTransparentBackgroundSupported()` (not implemented in wx)
""".
-spec setBackgroundStyle(This, Style) -> boolean() when
	This::wxWindow(), Style::wx:wx_enum().
setBackgroundStyle(#wx_ref{type=ThisT}=This,Style)
 when is_integer(Style) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,Style,?get_env(),?wxWindow_SetBackgroundStyle),
  wxe_util:rec(?wxWindow_SetBackgroundStyle).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetcaret">external documentation</a>.
-doc "Sets the caret() associated with the window.".
-spec setCaret(This, Caret) -> 'ok' when
	This::wxWindow(), Caret::wxCaret:wxCaret().
setCaret(#wx_ref{type=ThisT}=This,#wx_ref{type=CaretT}=Caret) ->
  ?CLASS(ThisT,wxWindow),
  ?CLASS(CaretT,wxCaret),
  wxe_util:queue_cmd(This,Caret,?get_env(),?wxWindow_SetCaret).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetclientsize">external documentation</a>.
%% <br /> Also:<br />
%% setClientSize(This, Rect) -> 'ok' when<br />
%% 	This::wxWindow(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()}.<br />
%% 
-doc """
This is an overloaded member function, provided for convenience. It differs from
the above function only in what argument(s) it accepts.
""".
-spec setClientSize(This, Size) -> 'ok' when
	This::wxWindow(), Size::{W::integer(), H::integer()};
      (This, Rect) -> 'ok' when
	This::wxWindow(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()}.
setClientSize(#wx_ref{type=ThisT}=This,{SizeW,SizeH} = Size)
 when is_integer(SizeW),is_integer(SizeH) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,Size,?get_env(),?wxWindow_SetClientSize_1_0);
setClientSize(#wx_ref{type=ThisT}=This,{RectX,RectY,RectW,RectH} = Rect)
 when is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,Rect,?get_env(),?wxWindow_SetClientSize_1_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetclientsize">external documentation</a>.
-doc """
This sets the size of the window client area in pixels.

Using this function to size a window tends to be more device-independent than
`setSize/6`, since the application need not worry about what dimensions the
border or title bar have when trying to fit the window around panel items, for
example.

See:
[Overview windowsizing](https://docs.wxwidgets.org/3.1/overview_windowsizing.html#overview_windowsizing)
""".
-spec setClientSize(This, Width, Height) -> 'ok' when
	This::wxWindow(), Width::integer(), Height::integer().
setClientSize(#wx_ref{type=ThisT}=This,Width,Height)
 when is_integer(Width),is_integer(Height) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,Width,Height,?get_env(),?wxWindow_SetClientSize_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetcontainingsizer">external documentation</a>.
-doc """
Used by `m:wxSizer` internally to notify the window about being managed by the
given sizer.

This method should not be called from outside the library, unless you're
implementing a custom sizer class - and in the latter case you must call this
method with the pointer to the sizer itself whenever a window is added to it and
with NULL argument when the window is removed from it.
""".
-spec setContainingSizer(This, Sizer) -> 'ok' when
	This::wxWindow(), Sizer::wxSizer:wxSizer().
setContainingSizer(#wx_ref{type=ThisT}=This,#wx_ref{type=SizerT}=Sizer) ->
  ?CLASS(ThisT,wxWindow),
  ?CLASS(SizerT,wxSizer),
  wxe_util:queue_cmd(This,Sizer,?get_env(),?wxWindow_SetContainingSizer).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetcursor">external documentation</a>.
-doc """
Sets the window's cursor.

Notice that the window cursor also sets it for the children of the window
implicitly.

The `cursor` may be `wxNullCursor` in which case the window cursor will be reset
back to default.

See: `wx_misc:setCursor/1`, `m:wxCursor`
""".
-spec setCursor(This, Cursor) -> boolean() when
	This::wxWindow(), Cursor::wxCursor:wxCursor().
setCursor(#wx_ref{type=ThisT}=This,#wx_ref{type=CursorT}=Cursor) ->
  ?CLASS(ThisT,wxWindow),
  ?CLASS(CursorT,wxCursor),
  wxe_util:queue_cmd(This,Cursor,?get_env(),?wxWindow_SetCursor),
  wxe_util:rec(?wxWindow_SetCursor).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetmaxsize">external documentation</a>.
-doc """
Sets the maximum size of the window, to indicate to the sizer layout mechanism
that this is the maximum possible size.

See: `SetMaxClientSize()` (not implemented in wx),
[Overview windowsizing](https://docs.wxwidgets.org/3.1/overview_windowsizing.html#overview_windowsizing)
""".
-spec setMaxSize(This, Size) -> 'ok' when
	This::wxWindow(), Size::{W::integer(), H::integer()}.
setMaxSize(#wx_ref{type=ThisT}=This,{SizeW,SizeH} = Size)
 when is_integer(SizeW),is_integer(SizeH) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,Size,?get_env(),?wxWindow_SetMaxSize).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetminsize">external documentation</a>.
-doc """
Sets the minimum size of the window, to indicate to the sizer layout mechanism
that this is the minimum required size.

You may need to call this if you change the window size after construction and
before adding to its parent sizer.

Notice that calling this method doesn't prevent the program from making the
window explicitly smaller than the specified size by calling `setSize/6`, it
just ensures that it won't become smaller than this size during the automatic
layout.

See: `SetMinClientSize()` (not implemented in wx),
[Overview windowsizing](https://docs.wxwidgets.org/3.1/overview_windowsizing.html#overview_windowsizing)
""".
-spec setMinSize(This, Size) -> 'ok' when
	This::wxWindow(), Size::{W::integer(), H::integer()}.
setMinSize(#wx_ref{type=ThisT}=This,{SizeW,SizeH} = Size)
 when is_integer(SizeW),is_integer(SizeH) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,Size,?get_env(),?wxWindow_SetMinSize).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetownbackgroundcolour">external documentation</a>.
-doc """
Sets the background colour of the window but prevents it from being inherited by
the children of this window.

See: `setBackgroundColour/2`, `inheritAttributes/1`
""".
-spec setOwnBackgroundColour(This, Colour) -> 'ok' when
	This::wxWindow(), Colour::wx:wx_colour().
setOwnBackgroundColour(#wx_ref{type=ThisT}=This,Colour)
 when ?is_colordata(Colour) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,wxe_util:color(Colour),?get_env(),?wxWindow_SetOwnBackgroundColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetownfont">external documentation</a>.
-doc """
Sets the font of the window but prevents it from being inherited by the children
of this window.

See: `setFont/2`, `inheritAttributes/1`
""".
-spec setOwnFont(This, Font) -> 'ok' when
	This::wxWindow(), Font::wxFont:wxFont().
setOwnFont(#wx_ref{type=ThisT}=This,#wx_ref{type=FontT}=Font) ->
  ?CLASS(ThisT,wxWindow),
  ?CLASS(FontT,wxFont),
  wxe_util:queue_cmd(This,Font,?get_env(),?wxWindow_SetOwnFont).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetownforegroundcolour">external documentation</a>.
-doc """
Sets the foreground colour of the window but prevents it from being inherited by
the children of this window.

See: `setForegroundColour/2`, `inheritAttributes/1`
""".
-spec setOwnForegroundColour(This, Colour) -> 'ok' when
	This::wxWindow(), Colour::wx:wx_colour().
setOwnForegroundColour(#wx_ref{type=ThisT}=This,Colour)
 when ?is_colordata(Colour) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,wxe_util:color(Colour),?get_env(),?wxWindow_SetOwnForegroundColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetdroptarget">external documentation</a>.
-doc """
Associates a drop target with this window.

If the window already has a drop target, it is deleted.

See: `getDropTarget/1`,
[Overview dnd](https://docs.wxwidgets.org/3.1/overview_dnd.html#overview_dnd)
""".
-spec setDropTarget(This, Target) -> 'ok' when
	This::wxWindow(), Target::wx:wx_object().
setDropTarget(#wx_ref{type=ThisT}=This,#wx_ref{type=TargetT}=Target) ->
  ?CLASS(ThisT,wxWindow),
  ?CLASS(TargetT,wxDropTarget),
  wxe_util:queue_cmd(This,Target,?get_env(),?wxWindow_SetDropTarget).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetextrastyle">external documentation</a>.
-doc """
Sets the extra style bits for the window.

The currently defined extra style bits are reported in the class description.
""".
-spec setExtraStyle(This, ExStyle) -> 'ok' when
	This::wxWindow(), ExStyle::integer().
setExtraStyle(#wx_ref{type=ThisT}=This,ExStyle)
 when is_integer(ExStyle) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,ExStyle,?get_env(),?wxWindow_SetExtraStyle).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetfocus">external documentation</a>.
-doc """
This sets the window to receive keyboard input.

See: `HasFocus()` (not implemented in wx), `m:wxFocusEvent`, `setFocus/1`,
`wxPanel:setFocusIgnoringChildren/1`
""".
-spec setFocus(This) -> 'ok' when
	This::wxWindow().
setFocus(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_SetFocus).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetfocusfromkbd">external documentation</a>.
-doc """
This function is called by wxWidgets keyboard navigation code when the user
gives the focus to this window from keyboard (e.g. using `TAB` key).

By default this method simply calls `setFocus/1` but can be overridden to do
something in addition to this in the derived classes.
""".
-spec setFocusFromKbd(This) -> 'ok' when
	This::wxWindow().
setFocusFromKbd(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_SetFocusFromKbd).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetfont">external documentation</a>.
-doc """
Sets the font for this window.

This function should not be called for the parent window if you don't want its
font to be inherited by its children, use `setOwnFont/2` instead in this case
and see `inheritAttributes/1` for more explanations.

Please notice that the given font is not automatically used for `m:wxPaintDC`
objects associated with this window, you need to call `wxDC:setFont/2` too.
However this font is used by any standard controls for drawing their text as
well as by `getTextExtent/3`.

Return: true if the font was really changed, false if it was already set to this
font and nothing was done.

See: `getFont/1`, `inheritAttributes/1`
""".
-spec setFont(This, Font) -> boolean() when
	This::wxWindow(), Font::wxFont:wxFont().
setFont(#wx_ref{type=ThisT}=This,#wx_ref{type=FontT}=Font) ->
  ?CLASS(ThisT,wxWindow),
  ?CLASS(FontT,wxFont),
  wxe_util:queue_cmd(This,Font,?get_env(),?wxWindow_SetFont),
  wxe_util:rec(?wxWindow_SetFont).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetforegroundcolour">external documentation</a>.
-doc """
Sets the foreground colour of the window.

The meaning of foreground colour varies according to the window class; it may be
the text colour or other colour, or it may not be used at all. Additionally, not
all native controls support changing their foreground colour so this method may
change their colour only partially or even not at all.

Please see `inheritAttributes/1` for explanation of the difference between this
method and `setOwnForegroundColour/2`.

Return: true if the colour was really changed, false if it was already set to
this colour and nothing was done.

See: `getForegroundColour/1`, `setBackgroundColour/2`, `getBackgroundColour/1`,
`shouldInheritColours/1`
""".
-spec setForegroundColour(This, Colour) -> boolean() when
	This::wxWindow(), Colour::wx:wx_colour().
setForegroundColour(#wx_ref{type=ThisT}=This,Colour)
 when ?is_colordata(Colour) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,wxe_util:color(Colour),?get_env(),?wxWindow_SetForegroundColour),
  wxe_util:rec(?wxWindow_SetForegroundColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsethelptext">external documentation</a>.
-doc """
Sets the help text to be used as context-sensitive help for this window.

Note that the text is actually stored by the current `wxHelpProvider` (not
implemented in wx) implementation, and not in the window object itself.

See: `getHelpText/1`, `wxHelpProvider::AddHelp()` (not implemented in wx)
""".
-spec setHelpText(This, HelpText) -> 'ok' when
	This::wxWindow(), HelpText::unicode:chardata().
setHelpText(#wx_ref{type=ThisT}=This,HelpText)
 when ?is_chardata(HelpText) ->
  ?CLASS(ThisT,wxWindow),
  HelpText_UC = unicode:characters_to_binary(HelpText),
  wxe_util:queue_cmd(This,HelpText_UC,?get_env(),?wxWindow_SetHelpText).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetid">external documentation</a>.
-doc """
Sets the identifier of the window.

Remark: Each window has an integer identifier. If the application has not
provided one, an identifier will be generated. Normally, the identifier should
be provided on creation and should not be modified subsequently.

See: `getId/1`,
[Overview windowids](https://docs.wxwidgets.org/3.1/overview_windowids.html#overview_windowids)
""".
-spec setId(This, Winid) -> 'ok' when
	This::wxWindow(), Winid::integer().
setId(#wx_ref{type=ThisT}=This,Winid)
 when is_integer(Winid) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,Winid,?get_env(),?wxWindow_SetId).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetlabel">external documentation</a>.
-doc """
Sets the window's label.

See: `getLabel/1`
""".
-spec setLabel(This, Label) -> 'ok' when
	This::wxWindow(), Label::unicode:chardata().
setLabel(#wx_ref{type=ThisT}=This,Label)
 when ?is_chardata(Label) ->
  ?CLASS(ThisT,wxWindow),
  Label_UC = unicode:characters_to_binary(Label),
  wxe_util:queue_cmd(This,Label_UC,?get_env(),?wxWindow_SetLabel).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetname">external documentation</a>.
-doc """
Sets the window's name.

See: `getName/1`
""".
-spec setName(This, Name) -> 'ok' when
	This::wxWindow(), Name::unicode:chardata().
setName(#wx_ref{type=ThisT}=This,Name)
 when ?is_chardata(Name) ->
  ?CLASS(ThisT,wxWindow),
  Name_UC = unicode:characters_to_binary(Name),
  wxe_util:queue_cmd(This,Name_UC,?get_env(),?wxWindow_SetName).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetpalette">external documentation</a>.
-doc "Deprecated: use `wxDC:setPalette/2` instead.".
-spec setPalette(This, Pal) -> 'ok' when
	This::wxWindow(), Pal::wxPalette:wxPalette().
setPalette(#wx_ref{type=ThisT}=This,#wx_ref{type=PalT}=Pal) ->
  ?CLASS(ThisT,wxWindow),
  ?CLASS(PalT,wxPalette),
  wxe_util:queue_cmd(This,Pal,?get_env(),?wxWindow_SetPalette).

%% @equiv setScrollbar(This,Orientation,Position,ThumbSize,Range, [])
-spec setScrollbar(This, Orientation, Position, ThumbSize, Range) -> 'ok' when
	This::wxWindow(), Orientation::integer(), Position::integer(), ThumbSize::integer(), Range::integer().

setScrollbar(This,Orientation,Position,ThumbSize,Range)
 when is_record(This, wx_ref),is_integer(Orientation),is_integer(Position),is_integer(ThumbSize),is_integer(Range) ->
  setScrollbar(This,Orientation,Position,ThumbSize,Range, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetscrollbar">external documentation</a>.
-doc """
Sets the scrollbar properties of a built-in scrollbar.

Remark: Let's say you wish to display 50 lines of text, using the same font. The
window is sized so that you can only see 16 lines at a time. You would use: Note
that with the window at this size, the thumb position can never go above 50
minus 16, or 34. You can determine how many lines are currently visible by
dividing the current view size by the character height in pixels. When defining
your own scrollbar behaviour, you will always need to recalculate the scrollbar
settings when the window size changes. You could therefore put your scrollbar
calculations and SetScrollbar call into a function named AdjustScrollbars, which
can be called initially and also from your `m:wxSizeEvent` handler function.

See:
[Overview scrolling](https://docs.wxwidgets.org/3.1/overview_scrolling.html#overview_scrolling),
`m:wxScrollBar`, `wxScrolled` (not implemented in wx), `m:wxScrollWinEvent`
""".
-spec setScrollbar(This, Orientation, Position, ThumbSize, Range, [Option]) -> 'ok' when
	This::wxWindow(), Orientation::integer(), Position::integer(), ThumbSize::integer(), Range::integer(),
	Option :: {'refresh', boolean()}.
setScrollbar(#wx_ref{type=ThisT}=This,Orientation,Position,ThumbSize,Range, Options)
 when is_integer(Orientation),is_integer(Position),is_integer(ThumbSize),is_integer(Range),is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({refresh, _refresh} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Orientation,Position,ThumbSize,Range, Opts,?get_env(),?wxWindow_SetScrollbar).

%% @equiv setScrollPos(This,Orientation,Pos, [])
-spec setScrollPos(This, Orientation, Pos) -> 'ok' when
	This::wxWindow(), Orientation::integer(), Pos::integer().

setScrollPos(This,Orientation,Pos)
 when is_record(This, wx_ref),is_integer(Orientation),is_integer(Pos) ->
  setScrollPos(This,Orientation,Pos, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetscrollpos">external documentation</a>.
-doc """
Sets the position of one of the built-in scrollbars.

Remark: This function does not directly affect the contents of the window: it is
up to the application to take note of scrollbar attributes and redraw contents
accordingly.

See: `setScrollbar/6`, `getScrollPos/2`, `getScrollThumb/2`, `m:wxScrollBar`,
`wxScrolled` (not implemented in wx)
""".
-spec setScrollPos(This, Orientation, Pos, [Option]) -> 'ok' when
	This::wxWindow(), Orientation::integer(), Pos::integer(),
	Option :: {'refresh', boolean()}.
setScrollPos(#wx_ref{type=ThisT}=This,Orientation,Pos, Options)
 when is_integer(Orientation),is_integer(Pos),is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({refresh, _refresh} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Orientation,Pos, Opts,?get_env(),?wxWindow_SetScrollPos).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetsize">external documentation</a>.
%% <br /> Also:<br />
%% setSize(This, Size) -> 'ok' when<br />
%% 	This::wxWindow(), Size::{W::integer(), H::integer()}.<br />
%% 
-doc """
This is an overloaded member function, provided for convenience. It differs from
the above function only in what argument(s) it accepts.
""".
-spec setSize(This, Rect) -> 'ok' when
	This::wxWindow(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()};
      (This, Size) -> 'ok' when
	This::wxWindow(), Size::{W::integer(), H::integer()}.

setSize(This,{RectX,RectY,RectW,RectH} = Rect)
 when is_record(This, wx_ref),is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH) ->
  setSize(This,Rect, []);
setSize(#wx_ref{type=ThisT}=This,{SizeW,SizeH} = Size)
 when is_integer(SizeW),is_integer(SizeH) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,Size,?get_env(),?wxWindow_SetSize_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetsize">external documentation</a>.
%% <br /> Also:<br />
%% setSize(This, Rect, [Option]) -> 'ok' when<br />
%% 	This::wxWindow(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()},<br />
%% 	Option :: {'sizeFlags', integer()}.<br />
%% 
-doc """
Sets the size of the window in pixels.

The size is specified using a \{X,Y,W,H\}, \{Width,Height\} or by a couple of
`int` objects.

Remark: This form must be used with non-default width and height values.

See: `move/4`,
[Overview windowsizing](https://docs.wxwidgets.org/3.1/overview_windowsizing.html#overview_windowsizing)
""".
-spec setSize(This, Width, Height) -> 'ok' when
	This::wxWindow(), Width::integer(), Height::integer();
      (This, Rect, [Option]) -> 'ok' when
	This::wxWindow(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()},
	Option :: {'sizeFlags', integer()}.
setSize(#wx_ref{type=ThisT}=This,Width,Height)
 when is_integer(Width),is_integer(Height) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,Width,Height,?get_env(),?wxWindow_SetSize_2_0);
setSize(#wx_ref{type=ThisT}=This,{RectX,RectY,RectW,RectH} = Rect, Options)
 when is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH),is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({sizeFlags, _sizeFlags} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Rect, Opts,?get_env(),?wxWindow_SetSize_2_1).

%% @equiv setSize(This,X,Y,Width,Height, [])
-spec setSize(This, X, Y, Width, Height) -> 'ok' when
	This::wxWindow(), X::integer(), Y::integer(), Width::integer(), Height::integer().

setSize(This,X,Y,Width,Height)
 when is_record(This, wx_ref),is_integer(X),is_integer(Y),is_integer(Width),is_integer(Height) ->
  setSize(This,X,Y,Width,Height, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetsize">external documentation</a>.
-doc """
Sets the size of the window in pixels.

Remark: This overload sets the position and optionally size, of the window.
Parameters may be wxDefaultCoord to indicate either that a default should be
supplied by wxWidgets, or that the current value of the dimension should be
used.

See: `move/4`,
[Overview windowsizing](https://docs.wxwidgets.org/3.1/overview_windowsizing.html#overview_windowsizing)
""".
-spec setSize(This, X, Y, Width, Height, [Option]) -> 'ok' when
	This::wxWindow(), X::integer(), Y::integer(), Width::integer(), Height::integer(),
	Option :: {'sizeFlags', integer()}.
setSize(#wx_ref{type=ThisT}=This,X,Y,Width,Height, Options)
 when is_integer(X),is_integer(Y),is_integer(Width),is_integer(Height),is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({sizeFlags, _sizeFlags} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,X,Y,Width,Height, Opts,?get_env(),?wxWindow_SetSize_5).

%% @equiv setSizeHints(This,MinSize, [])
-spec setSizeHints(This, MinSize) -> 'ok' when
	This::wxWindow(), MinSize::{W::integer(), H::integer()}.

setSizeHints(This,{MinSizeW,MinSizeH} = MinSize)
 when is_record(This, wx_ref),is_integer(MinSizeW),is_integer(MinSizeH) ->
  setSizeHints(This,MinSize, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetsizehints">external documentation</a>.
%% <br /> Also:<br />
%% setSizeHints(This, MinSize, [Option]) -> 'ok' when<br />
%% 	This::wxWindow(), MinSize::{W::integer(), H::integer()},<br />
%% 	Option :: {'maxSize', {W::integer(), H::integer()}}<br />
%% 		 | {'incSize', {W::integer(), H::integer()}}.<br />
%% 
-doc """
Use of this function for windows which are not toplevel windows (such as
`m:wxDialog` or `m:wxFrame`) is discouraged.

Please use `setMinSize/2` and `setMaxSize/2` instead.

See: `setSizeHints/4`,
[Overview windowsizing](https://docs.wxwidgets.org/3.1/overview_windowsizing.html#overview_windowsizing)
""".
-spec setSizeHints(This, MinW, MinH) -> 'ok' when
	This::wxWindow(), MinW::integer(), MinH::integer();
      (This, MinSize, [Option]) -> 'ok' when
	This::wxWindow(), MinSize::{W::integer(), H::integer()},
	Option :: {'maxSize', {W::integer(), H::integer()}}
		 | {'incSize', {W::integer(), H::integer()}}.

setSizeHints(This,MinW,MinH)
 when is_record(This, wx_ref),is_integer(MinW),is_integer(MinH) ->
  setSizeHints(This,MinW,MinH, []);
setSizeHints(#wx_ref{type=ThisT}=This,{MinSizeW,MinSizeH} = MinSize, Options)
 when is_integer(MinSizeW),is_integer(MinSizeH),is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({maxSize, {_maxSizeW,_maxSizeH}} = Arg) -> Arg;
          ({incSize, {_incSizeW,_incSizeH}} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,MinSize, Opts,?get_env(),?wxWindow_SetSizeHints_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetsizehints">external documentation</a>.
-doc """
This is an overloaded member function, provided for convenience. It differs from
the above function only in what argument(s) it accepts.
""".
-spec setSizeHints(This, MinW, MinH, [Option]) -> 'ok' when
	This::wxWindow(), MinW::integer(), MinH::integer(),
	Option :: {'maxW', integer()}
		 | {'maxH', integer()}
		 | {'incW', integer()}
		 | {'incH', integer()}.
setSizeHints(#wx_ref{type=ThisT}=This,MinW,MinH, Options)
 when is_integer(MinW),is_integer(MinH),is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({maxW, _maxW} = Arg) -> Arg;
          ({maxH, _maxH} = Arg) -> Arg;
          ({incW, _incW} = Arg) -> Arg;
          ({incH, _incH} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,MinW,MinH, Opts,?get_env(),?wxWindow_SetSizeHints_3).

%% @equiv setSizer(This,Sizer, [])
-spec setSizer(This, Sizer) -> 'ok' when
	This::wxWindow(), Sizer::wxSizer:wxSizer().

setSizer(This,Sizer)
 when is_record(This, wx_ref),is_record(Sizer, wx_ref) ->
  setSizer(This,Sizer, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetsizer">external documentation</a>.
-doc """
Sets the window to have the given layout sizer.

The window will then own the object, and will take care of its deletion. If an
existing layout constraints object is already owned by the window, it will be
deleted if the `deleteOld` parameter is true.

Note that this function will also call `setAutoLayout/2` implicitly with true
parameter if the `sizer` is non-NULL and false otherwise so that the sizer will
be effectively used to layout the window children whenever it is resized.

Remark: SetSizer enables and disables Layout automatically.
""".
-spec setSizer(This, Sizer, [Option]) -> 'ok' when
	This::wxWindow(), Sizer::wxSizer:wxSizer(),
	Option :: {'deleteOld', boolean()}.
setSizer(#wx_ref{type=ThisT}=This,#wx_ref{type=SizerT}=Sizer, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  ?CLASS(SizerT,wxSizer),
  MOpts = fun({deleteOld, _deleteOld} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Sizer, Opts,?get_env(),?wxWindow_SetSizer).

%% @equiv setSizerAndFit(This,Sizer, [])
-spec setSizerAndFit(This, Sizer) -> 'ok' when
	This::wxWindow(), Sizer::wxSizer:wxSizer().

setSizerAndFit(This,Sizer)
 when is_record(This, wx_ref),is_record(Sizer, wx_ref) ->
  setSizerAndFit(This,Sizer, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetsizerandfit">external documentation</a>.
-doc """
Associate the sizer with the window and set the window size and minimal size
accordingly.

This method calls `setSizer/3` and then `wxSizer:setSizeHints/2` which sets the
initial window size to the size needed to accommodate all sizer elements and
sets the minimal size to the same size, this preventing the user from resizing
this window to be less than this minimal size (if it's a top-level window which
can be directly resized by the user).
""".
-spec setSizerAndFit(This, Sizer, [Option]) -> 'ok' when
	This::wxWindow(), Sizer::wxSizer:wxSizer(),
	Option :: {'deleteOld', boolean()}.
setSizerAndFit(#wx_ref{type=ThisT}=This,#wx_ref{type=SizerT}=Sizer, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  ?CLASS(SizerT,wxSizer),
  MOpts = fun({deleteOld, _deleteOld} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Sizer, Opts,?get_env(),?wxWindow_SetSizerAndFit).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetthemeenabled">external documentation</a>.
-doc """
This function tells a window if it should use the system's "theme" code to draw
the windows' background instead of its own background drawing code.

This does not always have any effect since the underlying platform obviously
needs to support the notion of themes in user defined windows. One such platform
is GTK+ where windows can have (very colourful) backgrounds defined by a user's
selected theme.

Dialogs, notebook pages and the status bar have this flag set to true by default
so that the default look and feel is simulated best.

See: `getThemeEnabled/1`
""".
-spec setThemeEnabled(This, Enable) -> 'ok' when
	This::wxWindow(), Enable::boolean().
setThemeEnabled(#wx_ref{type=ThisT}=This,Enable)
 when is_boolean(Enable) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,Enable,?get_env(),?wxWindow_SetThemeEnabled).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsettooltip">external documentation</a>.
%% <br /> Also:<br />
%% setToolTip(This, Tip) -> 'ok' when<br />
%% 	This::wxWindow(), Tip::wxToolTip:wxToolTip().<br />
%% 
-doc """
This is an overloaded member function, provided for convenience. It differs from
the above function only in what argument(s) it accepts.
""".
-spec setToolTip(This, TipString) -> 'ok' when
	This::wxWindow(), TipString::unicode:chardata();
      (This, Tip) -> 'ok' when
	This::wxWindow(), Tip::wxToolTip:wxToolTip().
setToolTip(#wx_ref{type=ThisT}=This,TipString)
 when ?is_chardata(TipString) ->
  ?CLASS(ThisT,wxWindow),
  TipString_UC = unicode:characters_to_binary(TipString),
  wxe_util:queue_cmd(This,TipString_UC,?get_env(),?wxWindow_SetToolTip_1_0);
setToolTip(#wx_ref{type=ThisT}=This,#wx_ref{type=TipT}=Tip) ->
  ?CLASS(ThisT,wxWindow),
  ?CLASS(TipT,wxToolTip),
  wxe_util:queue_cmd(This,Tip,?get_env(),?wxWindow_SetToolTip_1_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetvirtualsize">external documentation</a>.
-doc """
This is an overloaded member function, provided for convenience. It differs from
the above function only in what argument(s) it accepts.
""".
-spec setVirtualSize(This, Size) -> 'ok' when
	This::wxWindow(), Size::{W::integer(), H::integer()}.
setVirtualSize(#wx_ref{type=ThisT}=This,{SizeW,SizeH} = Size)
 when is_integer(SizeW),is_integer(SizeH) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,Size,?get_env(),?wxWindow_SetVirtualSize_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetvirtualsize">external documentation</a>.
-doc """
Sets the virtual size of the window in pixels.

See:
[Overview windowsizing](https://docs.wxwidgets.org/3.1/overview_windowsizing.html#overview_windowsizing)
""".
-spec setVirtualSize(This, Width, Height) -> 'ok' when
	This::wxWindow(), Width::integer(), Height::integer().
setVirtualSize(#wx_ref{type=ThisT}=This,Width,Height)
 when is_integer(Width),is_integer(Height) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,Width,Height,?get_env(),?wxWindow_SetVirtualSize_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetwindowstyle">external documentation</a>.
-doc "See `setWindowStyleFlag/2` for more info.".
-spec setWindowStyle(This, Style) -> 'ok' when
	This::wxWindow(), Style::integer().
setWindowStyle(#wx_ref{type=ThisT}=This,Style)
 when is_integer(Style) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,Style,?get_env(),?wxWindow_SetWindowStyle).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetwindowstyleflag">external documentation</a>.
-doc """
Sets the style of the window.

Please note that some styles cannot be changed after the window creation and
that `refresh/2` might need to be called after changing the others for the
change to take place immediately.

See Window styles for more information about flags.

See: `getWindowStyleFlag/1`
""".
-spec setWindowStyleFlag(This, Style) -> 'ok' when
	This::wxWindow(), Style::integer().
setWindowStyleFlag(#wx_ref{type=ThisT}=This,Style)
 when is_integer(Style) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,Style,?get_env(),?wxWindow_SetWindowStyleFlag).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetwindowvariant">external documentation</a>.
%%<br /> Variant = ?wxWINDOW_VARIANT_NORMAL | ?wxWINDOW_VARIANT_SMALL | ?wxWINDOW_VARIANT_MINI | ?wxWINDOW_VARIANT_LARGE | ?wxWINDOW_VARIANT_MAX
-doc """
Chooses a different variant of the window display to use.

Window variants currently just differ in size, as can be seen from
?wxWindowVariant documentation. Under all platforms but macOS, this function
does nothing more than change the font used by the window. However under macOS
it is implemented natively and selects the appropriate variant of the native
widget, which has better appearance than just scaled down or up version of the
normal variant, so it should be preferred to directly tweaking the font size.

By default the controls naturally use the normal variant.
""".
-spec setWindowVariant(This, Variant) -> 'ok' when
	This::wxWindow(), Variant::wx:wx_enum().
setWindowVariant(#wx_ref{type=ThisT}=This,Variant)
 when is_integer(Variant) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,Variant,?get_env(),?wxWindow_SetWindowVariant).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowshouldinheritcolours">external documentation</a>.
-doc """
Return true from here to allow the colours of this window to be changed by
`inheritAttributes/1`.

Returning false forbids inheriting them from the parent window.

The base class version returns false, but this method is overridden in
`m:wxControl` where it returns true.
""".
-spec shouldInheritColours(This) -> boolean() when
	This::wxWindow().
shouldInheritColours(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_ShouldInheritColours),
  wxe_util:rec(?wxWindow_ShouldInheritColours).

%% @equiv show(This, [])
-spec show(This) -> boolean() when
	This::wxWindow().

show(This)
 when is_record(This, wx_ref) ->
  show(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowshow">external documentation</a>.
-doc """
Shows or hides the window.

You may need to call `raise/1` for a top level window if you want to bring it to
top, although this is not needed if `show/2` is called immediately after the
frame creation.

Notice that the default state of newly created top level windows is hidden (to
allow you to create their contents without flicker) unlike for all the other,
not derived from `m:wxTopLevelWindow`, windows that are by default created in
the shown state.

Return: true if the window has been shown or hidden or false if nothing was done
because it already was in the requested state.

See: `isShown/1`, `hide/1`, `wxRadioBox:show/3`, `m:wxShowEvent`
""".
-spec show(This, [Option]) -> boolean() when
	This::wxWindow(),
	Option :: {'show', boolean()}.
show(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({show, _show} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxWindow_Show),
  wxe_util:rec(?wxWindow_Show).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowthaw">external documentation</a>.
-doc """
Re-enables window updating after a previous call to `freeze/1`.

To really thaw the control, it must be called exactly the same number of times
as `freeze/1`.

If the window has any children, they are recursively thawed too.

See: `wxWindowUpdateLocker` (not implemented in wx), `freeze/1`, `isFrozen/1`
""".
-spec thaw(This) -> 'ok' when
	This::wxWindow().
thaw(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_Thaw).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowtransferdatafromwindow">external documentation</a>.
-doc """
Transfers values from child controls to data areas specified by their
validators.

Returns false if a transfer failed.

Notice that this also calls `transferDataFromWindow/1` for all children
recursively.

See: `transferDataToWindow/1`, `wxValidator` (not implemented in wx),
`validate/1`
""".
-spec transferDataFromWindow(This) -> boolean() when
	This::wxWindow().
transferDataFromWindow(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_TransferDataFromWindow),
  wxe_util:rec(?wxWindow_TransferDataFromWindow).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowtransferdatatowindow">external documentation</a>.
-doc """
Transfers values to child controls from data areas specified by their
validators.

Notice that this also calls `transferDataToWindow/1` for all children
recursively.

Return: Returns false if a transfer failed.

See: `transferDataFromWindow/1`, `wxValidator` (not implemented in wx),
`validate/1`
""".
-spec transferDataToWindow(This) -> boolean() when
	This::wxWindow().
transferDataToWindow(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_TransferDataToWindow),
  wxe_util:rec(?wxWindow_TransferDataToWindow).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowupdate">external documentation</a>.
-doc """
Calling this method immediately repaints the invalidated area of the window and
all of its children recursively (this normally only happens when the flow of
control returns to the event loop).

Notice that this function doesn't invalidate any area of the window so nothing
happens if nothing has been invalidated (i.e. marked as requiring a redraw). Use
`refresh/2` first if you want to immediately redraw the window unconditionally.
""".
-spec update(This) -> 'ok' when
	This::wxWindow().
update(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_Update).

%% @equiv updateWindowUI(This, [])
-spec updateWindowUI(This) -> 'ok' when
	This::wxWindow().

updateWindowUI(This)
 when is_record(This, wx_ref) ->
  updateWindowUI(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowupdatewindowui">external documentation</a>.
-doc """
This function sends one or more `m:wxUpdateUIEvent` to the window.

The particular implementation depends on the window; for example a `m:wxToolBar`
will send an update UI event for each toolbar button, and a `m:wxFrame` will
send an update UI event for each menubar menu item.

You can call this function from your application to ensure that your UI is
up-to-date at this point (as far as your `m:wxUpdateUIEvent` handlers are
concerned). This may be necessary if you have called `wxUpdateUIEvent:setMode/1`
or `wxUpdateUIEvent:setUpdateInterval/1` to limit the overhead that wxWidgets
incurs by sending update UI events in idle time. `flags` should be a bitlist of
one or more of the ?wxUpdateUI enumeration.

If you are calling this function from an OnInternalIdle or OnIdle function, make
sure you pass the wxUPDATE_UI_FROMIDLE flag, since this tells the window to only
update the UI elements that need to be updated in idle time. Some windows update
their elements only when necessary, for example when a menu is about to be
shown. The following is an example of how to call UpdateWindowUI from an idle
function.

See: `m:wxUpdateUIEvent`, `DoUpdateWindowUI()` (not implemented in wx),
`OnInternalIdle()` (not implemented in wx)
""".
-spec updateWindowUI(This, [Option]) -> 'ok' when
	This::wxWindow(),
	Option :: {'flags', integer()}.
updateWindowUI(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({flags, _flags} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxWindow_UpdateWindowUI).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowvalidate">external documentation</a>.
-doc """
Validates the current values of the child controls using their validators.

Notice that this also calls `validate/1` for all children recursively.

Return: Returns false if any of the validations failed.

See: `transferDataFromWindow/1`, `transferDataToWindow/1`, `wxValidator` (not
implemented in wx)
""".
-spec validate(This) -> boolean() when
	This::wxWindow().
validate(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_Validate),
  wxe_util:rec(?wxWindow_Validate).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowwarppointer">external documentation</a>.
-doc """
Moves the pointer to the given position on the window.

Note: Apple Human Interface Guidelines forbid moving the mouse cursor
programmatically so you should avoid using this function in Mac applications
(and probably avoid using it under the other platforms without good reason as
well).
""".
-spec warpPointer(This, X, Y) -> 'ok' when
	This::wxWindow(), X::integer(), Y::integer().
warpPointer(#wx_ref{type=ThisT}=This,X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,X,Y,?get_env(),?wxWindow_WarpPointer).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsettransparent">external documentation</a>.
-doc """
Set the transparency of the window.

If the system supports transparent windows, returns true, otherwise returns
false and the window remains fully opaque. See also `canSetTransparent/1`.

The parameter `alpha` is in the range 0..255 where 0 corresponds to a fully
transparent window and 255 to the fully opaque one. The constants
`wxIMAGE_ALPHA_TRANSPARENT` and `wxIMAGE_ALPHA_OPAQUE` can be used.
""".
-spec setTransparent(This, Alpha) -> boolean() when
	This::wxWindow(), Alpha::integer().
setTransparent(#wx_ref{type=ThisT}=This,Alpha)
 when is_integer(Alpha) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,Alpha,?get_env(),?wxWindow_SetTransparent),
  wxe_util:rec(?wxWindow_SetTransparent).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowcansettransparent">external documentation</a>.
-doc """
Returns true if the system supports transparent windows and calling
`setTransparent/2` may succeed.

If this function returns false, transparent windows are definitely not supported
by the current system.
""".
-spec canSetTransparent(This) -> boolean() when
	This::wxWindow().
canSetTransparent(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_CanSetTransparent),
  wxe_util:rec(?wxWindow_CanSetTransparent).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowisdoublebuffered">external documentation</a>.
-doc """
Returns true if the window contents is double-buffered by the system, i.e. if
any drawing done on the window is really done on a temporary backing surface and
transferred to the screen all at once later.

See: `m:wxBufferedDC`
""".
-spec isDoubleBuffered(This) -> boolean() when
	This::wxWindow().
isDoubleBuffered(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_IsDoubleBuffered),
  wxe_util:rec(?wxWindow_IsDoubleBuffered).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetdoublebuffered">external documentation</a>.
-doc "Turn on or off double buffering of the window if the system supports it.".
-spec setDoubleBuffered(This, On) -> 'ok' when
	This::wxWindow(), On::boolean().
setDoubleBuffered(#wx_ref{type=ThisT}=This,On)
 when is_boolean(On) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,On,?get_env(),?wxWindow_SetDoubleBuffered).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetcontentscalefactor">external documentation</a>.
-doc """
Returns the factor mapping logical pixels of this window to physical pixels.

This function can be used to portably determine the number of physical pixels in
a window of the given size, by multiplying the window size by the value returned
from it. I.e. it returns the factor converting window coordinates to "content
view" coordinates, where the view can be just a simple window displaying a
`m:wxBitmap` or `m:wxGLCanvas` or any other kind of window rendering arbitrary
"content" on screen.

For the platforms not doing any pixel mapping, i.e. where logical and physical
pixels are one and the same, this function always returns 1.0 and so using it
is, in principle, unnecessary and could be avoided by using preprocessor check
for `wxHAVE_DPI_INDEPENDENT_PIXELS` `not` being defined, however using this
function unconditionally under all platforms is usually simpler and so
preferable.

Note: Current behaviour of this function is compatible with wxWidgets 3.0, but
different from its behaviour in versions 3.1.0 to 3.1.3, where it returned the
same value as `getDPIScaleFactor/1`. Please use the other function if you need
to use a scaling factor greater than 1.0 even for the platforms without
`wxHAVE_DPI_INDEPENDENT_PIXELS`, such as wxMSW.

Since: 2.9.5
""".
-spec getContentScaleFactor(This) -> number() when
	This::wxWindow().
getContentScaleFactor(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetContentScaleFactor),
  wxe_util:rec(?wxWindow_GetContentScaleFactor).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetdpi">external documentation</a>.
-doc """
Return the DPI of the display used by this window.

The returned value can be different for different windows on systems with
support for per-monitor DPI values, such as Microsoft Windows 10.

If the DPI is not available, returns \{Width,Height\} object.

See: `wxDisplay:getPPI/1`, `wxDPIChangedEvent` (not implemented in wx)

Since: 3.1.3
""".
-spec getDPI(This) -> {W::integer(), H::integer()} when
	This::wxWindow().
getDPI(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetDPI),
  wxe_util:rec(?wxWindow_GetDPI).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowfromdip">external documentation</a>.
%% <br /> Also:<br />
%% fromDIP(Sz, W) -> {W::integer(), H::integer()} when<br />
%% 	Sz::{W::integer(), H::integer()}, W::wxWindow();<br />
%%       (This, D) -> integer() when<br />
%% 	This::wxWindow(), D::integer();<br />
%%       (This, Sz) -> {W::integer(), H::integer()} when<br />
%% 	This::wxWindow(), Sz::{W::integer(), H::integer()}.<br />
%% 
-doc """
Convert DPI-independent pixel values to the value in pixels appropriate for the
current toolkit.

A DPI-independent pixel is just a pixel at the standard 96 DPI resolution. To
keep the same physical size at higher resolution, the physical pixel value must
be scaled by `getDPIScaleFactor/1` but this scaling may be already done by the
underlying toolkit (GTK+, Cocoa, ...) automatically. This method performs the
conversion only if it is not already done by the lower level toolkit and so by
using it with pixel values you can guarantee that the physical size of the
corresponding elements will remain the same in all resolutions under all
platforms. For example, instead of creating a bitmap of the hard coded size of
32 pixels you should use to avoid using tiny bitmaps on high DPI screens.

Notice that this function is only needed when using hard coded pixel values. It
is not necessary if the sizes are already based on the DPI-independent units
such as dialog units or if you are relying on the controls automatic best size
determination and using sizers to lay out them.

Also note that if either component of `sz` has the special value of -1, it is
returned unchanged independently of the current DPI, to preserve the special
value of -1 in wxWidgets API (it is often used to mean "unspecified").

Since: 3.1.0
""".
-spec fromDIP(D, W) -> integer() when
	D::integer(), W::wxWindow();
      (Sz, W) -> {W::integer(), H::integer()} when
	Sz::{W::integer(), H::integer()}, W::wxWindow();
      (This, D) -> integer() when
	This::wxWindow(), D::integer();
      (This, Sz) -> {W::integer(), H::integer()} when
	This::wxWindow(), Sz::{W::integer(), H::integer()}.
fromDIP(D,#wx_ref{type=WT}=W)
 when is_integer(D) ->
  ?CLASS(WT,wxWindow),
  wxe_util:queue_cmd(D,W,?get_env(),?wxWindow_FromDIP_2_0),
  wxe_util:rec(?wxWindow_FromDIP_2_0);
fromDIP({SzW,SzH} = Sz,#wx_ref{type=WT}=W)
 when is_integer(SzW),is_integer(SzH) ->
  ?CLASS(WT,wxWindow),
  wxe_util:queue_cmd(Sz,W,?get_env(),?wxWindow_FromDIP_2_1),
  wxe_util:rec(?wxWindow_FromDIP_2_1);
fromDIP(#wx_ref{type=ThisT}=This,D)
 when is_integer(D) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,D,?get_env(),?wxWindow_FromDIP_1_0),
  wxe_util:rec(?wxWindow_FromDIP_1_0);
fromDIP(#wx_ref{type=ThisT}=This,{SzW,SzH} = Sz)
 when is_integer(SzW),is_integer(SzH) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,Sz,?get_env(),?wxWindow_FromDIP_1_1),
  wxe_util:rec(?wxWindow_FromDIP_1_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowtodip">external documentation</a>.
%% <br /> Also:<br />
%% toDIP(Sz, W) -> {W::integer(), H::integer()} when<br />
%% 	Sz::{W::integer(), H::integer()}, W::wxWindow();<br />
%%       (This, D) -> integer() when<br />
%% 	This::wxWindow(), D::integer();<br />
%%       (This, Sz) -> {W::integer(), H::integer()} when<br />
%% 	This::wxWindow(), Sz::{W::integer(), H::integer()}.<br />
%% 
-doc """
Convert pixel values of the current toolkit to DPI-independent pixel values.

A DPI-independent pixel is just a pixel at the standard 96 DPI resolution. To
keep the same physical size at higher resolution, the physical pixel value must
be scaled by `getDPIScaleFactor/1` but this scaling may be already done by the
underlying toolkit (GTK+, Cocoa, ...) automatically. This method performs the
conversion only if it is not already done by the lower level toolkit, For
example, you may want to use this to store window sizes and positions so that
they can be re-used regardless of the display DPI:

Also note that if either component of `sz` has the special value of -1, it is
returned unchanged independently of the current DPI, to preserve the special
value of -1 in wxWidgets API (it is often used to mean "unspecified").

Since: 3.1.0
""".
-spec toDIP(D, W) -> integer() when
	D::integer(), W::wxWindow();
      (Sz, W) -> {W::integer(), H::integer()} when
	Sz::{W::integer(), H::integer()}, W::wxWindow();
      (This, D) -> integer() when
	This::wxWindow(), D::integer();
      (This, Sz) -> {W::integer(), H::integer()} when
	This::wxWindow(), Sz::{W::integer(), H::integer()}.
toDIP(D,#wx_ref{type=WT}=W)
 when is_integer(D) ->
  ?CLASS(WT,wxWindow),
  wxe_util:queue_cmd(D,W,?get_env(),?wxWindow_ToDIP_2_0),
  wxe_util:rec(?wxWindow_ToDIP_2_0);
toDIP({SzW,SzH} = Sz,#wx_ref{type=WT}=W)
 when is_integer(SzW),is_integer(SzH) ->
  ?CLASS(WT,wxWindow),
  wxe_util:queue_cmd(Sz,W,?get_env(),?wxWindow_ToDIP_2_1),
  wxe_util:rec(?wxWindow_ToDIP_2_1);
toDIP(#wx_ref{type=ThisT}=This,D)
 when is_integer(D) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,D,?get_env(),?wxWindow_ToDIP_1_0),
  wxe_util:rec(?wxWindow_ToDIP_1_0);
toDIP(#wx_ref{type=ThisT}=This,{SzW,SzH} = Sz)
 when is_integer(SzW),is_integer(SzH) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,Sz,?get_env(),?wxWindow_ToDIP_1_1),
  wxe_util:rec(?wxWindow_ToDIP_1_1).

%% @doc Destroys this object, do not use object again
-doc """
Destructor.

Deletes all sub-windows, then deletes itself. Instead of using the `delete`
operator explicitly, you should normally use `'Destroy'/1` so that wxWidgets can
delete a window only when it is safe to do so, in idle time.

See: Window Deletion Overview, `'Destroy'/1`, `m:wxCloseEvent`
""".
-spec destroy(This::wxWindow()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxWindow),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
 %% From wxEvtHandler
%% @hidden
-doc false.
disconnect(This,EventType, Options) -> wxEvtHandler:disconnect(This,EventType, Options).
%% @hidden
-doc false.
disconnect(This,EventType) -> wxEvtHandler:disconnect(This,EventType).
%% @hidden
-doc false.
disconnect(This) -> wxEvtHandler:disconnect(This).
%% @hidden
-doc false.
connect(This,EventType, Options) -> wxEvtHandler:connect(This,EventType, Options).
%% @hidden
-doc false.
connect(This,EventType) -> wxEvtHandler:connect(This,EventType).
