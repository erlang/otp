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

-module(wxLayoutAlgorithm).
-moduledoc """
Functions for wxLayoutAlgorithm class

`m:wxLayoutAlgorithm` implements layout of subwindows in MDI or SDI frames. It
sends a `wxCalculateLayoutEvent` (not implemented in wx) event to children of
the frame, asking them for information about their size. For MDI parent frames,
the algorithm allocates the remaining space to the MDI client window (which
contains the MDI child frames).

For SDI (normal) frames, a 'main' window is specified as taking up the remaining
space.

Because the event system is used, this technique can be applied to any windows,
which are not necessarily 'aware' of the layout classes (no virtual functions in
`m:wxWindow` refer to `m:wxLayoutAlgorithm` or its events). However, you may
wish to use `m:wxSashLayoutWindow` for your subwindows since this class provides
handlers for the required events, and accessors to specify the desired size of
the window. The sash behaviour in the base class can be used, optionally, to
make the windows user-resizable.

`m:wxLayoutAlgorithm` is typically used in IDE (integrated development
environment) applications, where there are several resizable windows in addition
to the MDI client window, or other primary editing window. Resizable windows
might include toolbars, a project window, and a window for displaying error and
warning messages.

When a window receives an OnCalculateLayout event, it should call SetRect in the
given event object, to be the old supplied rectangle minus whatever space the
window takes up. It should also set its own size accordingly.
`wxSashLayoutWindow::OnCalculateLayout` (not implemented in wx) generates an
OnQueryLayoutInfo event which it sends to itself to determine the orientation,
alignment and size of the window, which it gets from internal member variables
set by the application.

The algorithm works by starting off with a rectangle equal to the whole frame
client area. It iterates through the frame children, generating
wxLayoutAlgorithm::OnCalculateLayout events which subtract the window size and
return the remaining rectangle for the next window to process. It is assumed (by
`wxSashLayoutWindow::OnCalculateLayout` (not implemented in wx)) that a window
stretches the full dimension of the frame client, according to the orientation
it specifies. For example, a horizontal window will stretch the full width of
the remaining portion of the frame client area. In the other orientation, the
window will be fixed to whatever size was specified by
wxLayoutAlgorithm::OnQueryLayoutInfo. An alignment setting will make the window
'stick' to the left, top, right or bottom of the remaining client area. This
scheme implies that order of window creation is important. Say you wish to have
an extra toolbar at the top of the frame, a project window to the left of the
MDI client window, and an output window above the status bar. You should
therefore create the windows in this order: toolbar, output window, project
window. This ensures that the toolbar and output window take up space at the top
and bottom, and then the remaining height in-between is used for the project
window.

`m:wxLayoutAlgorithm` is quite independent of the way in which
wxLayoutAlgorithm::OnCalculateLayout chooses to interpret a window's size and
alignment. Therefore you could implement a different window class with a new
wxLayoutAlgorithm::OnCalculateLayout event handler, that has a more
sophisticated way of laying out the windows. It might allow specification of
whether stretching occurs in the specified orientation, for example, rather than
always assuming stretching. (This could, and probably should, be added to the
existing implementation).

Note: `m:wxLayoutAlgorithm` has nothing to do with `wxLayoutConstraints` (not
implemented in wx). It is an alternative way of specifying layouts for which the
normal constraint system is unsuitable.

See: `m:wxSashEvent`, `m:wxSashLayoutWindow`,
[Overview events](https://docs.wxwidgets.org/3.1/overview_events.html#overview_events)

wxWidgets docs:
[wxLayoutAlgorithm](https://docs.wxwidgets.org/3.1/classwx_layout_algorithm.html)
""".
-include("wxe.hrl").
-export([destroy/1,layoutFrame/2,layoutFrame/3,layoutMDIFrame/2,layoutMDIFrame/3,
  layoutWindow/2,layoutWindow/3,new/0]).

%% inherited exports
-export([parent_class/1]).

-type wxLayoutAlgorithm() :: wx:wx_object().
-export_type([wxLayoutAlgorithm/0]).
%% @hidden
-doc false.
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlayoutalgorithm.html#wxlayoutalgorithmwxlayoutalgorithm">external documentation</a>.
-doc "Default constructor.".
-spec new() -> wxLayoutAlgorithm().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxLayoutAlgorithm_new),
  wxe_util:rec(?wxLayoutAlgorithm_new).

%% @equiv layoutFrame(This,Frame, [])
-spec layoutFrame(This, Frame) -> boolean() when
	This::wxLayoutAlgorithm(), Frame::wxFrame:wxFrame().

layoutFrame(This,Frame)
 when is_record(This, wx_ref),is_record(Frame, wx_ref) ->
  layoutFrame(This,Frame, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlayoutalgorithm.html#wxlayoutalgorithmlayoutframe">external documentation</a>.
-doc """
Lays out the children of a normal frame.

`mainWindow` is set to occupy the remaining space. This function simply calls
`layoutWindow/3`.
""".
-spec layoutFrame(This, Frame, [Option]) -> boolean() when
	This::wxLayoutAlgorithm(), Frame::wxFrame:wxFrame(),
	Option :: {'mainWindow', wxWindow:wxWindow()}.
layoutFrame(#wx_ref{type=ThisT}=This,#wx_ref{type=FrameT}=Frame, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxLayoutAlgorithm),
  ?CLASS(FrameT,wxFrame),
  MOpts = fun({mainWindow, #wx_ref{type=MainWindowT}} = Arg) ->   ?CLASS(MainWindowT,wxWindow),Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Frame, Opts,?get_env(),?wxLayoutAlgorithm_LayoutFrame),
  wxe_util:rec(?wxLayoutAlgorithm_LayoutFrame).

%% @equiv layoutMDIFrame(This,Frame, [])
-spec layoutMDIFrame(This, Frame) -> boolean() when
	This::wxLayoutAlgorithm(), Frame::wxMDIParentFrame:wxMDIParentFrame().

layoutMDIFrame(This,Frame)
 when is_record(This, wx_ref),is_record(Frame, wx_ref) ->
  layoutMDIFrame(This,Frame, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlayoutalgorithm.html#wxlayoutalgorithmlayoutmdiframe">external documentation</a>.
-doc """
Lays out the children of an MDI parent frame.

If `rect` is non-NULL, the given rectangle will be used as a starting point
instead of the frame's client area. The MDI client window is set to occupy the
remaining space.
""".
-spec layoutMDIFrame(This, Frame, [Option]) -> boolean() when
	This::wxLayoutAlgorithm(), Frame::wxMDIParentFrame:wxMDIParentFrame(),
	Option :: {'rect', {X::integer(), Y::integer(), W::integer(), H::integer()}}.
layoutMDIFrame(#wx_ref{type=ThisT}=This,#wx_ref{type=FrameT}=Frame, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxLayoutAlgorithm),
  ?CLASS(FrameT,wxMDIParentFrame),
  MOpts = fun({rect, {_rectX,_rectY,_rectW,_rectH}} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Frame, Opts,?get_env(),?wxLayoutAlgorithm_LayoutMDIFrame),
  wxe_util:rec(?wxLayoutAlgorithm_LayoutMDIFrame).

%% @equiv layoutWindow(This,Parent, [])
-spec layoutWindow(This, Parent) -> boolean() when
	This::wxLayoutAlgorithm(), Parent::wxWindow:wxWindow().

layoutWindow(This,Parent)
 when is_record(This, wx_ref),is_record(Parent, wx_ref) ->
  layoutWindow(This,Parent, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlayoutalgorithm.html#wxlayoutalgorithmlayoutwindow">external documentation</a>.
-doc """
Lays out the children of a normal frame or other window.

`mainWindow` is set to occupy the remaining space. If this is not specified,
then the last window that responds to a calculate layout event in query mode
will get the remaining space (that is, a non-query OnCalculateLayout event will
not be sent to this window and the window will be set to the remaining size).
""".
-spec layoutWindow(This, Parent, [Option]) -> boolean() when
	This::wxLayoutAlgorithm(), Parent::wxWindow:wxWindow(),
	Option :: {'mainWindow', wxWindow:wxWindow()}.
layoutWindow(#wx_ref{type=ThisT}=This,#wx_ref{type=ParentT}=Parent, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxLayoutAlgorithm),
  ?CLASS(ParentT,wxWindow),
  MOpts = fun({mainWindow, #wx_ref{type=MainWindowT}} = Arg) ->   ?CLASS(MainWindowT,wxWindow),Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Parent, Opts,?get_env(),?wxLayoutAlgorithm_LayoutWindow),
  wxe_util:rec(?wxLayoutAlgorithm_LayoutWindow).

%% @doc Destroys this object, do not use object again
-doc "Destructor.".
-spec destroy(This::wxLayoutAlgorithm()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxLayoutAlgorithm),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
