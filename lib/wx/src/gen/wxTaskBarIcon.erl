%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0 AND LicenseRef-scancode-wxwindows-free-doc-3
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
%% For documentation, wxWindow Free Documentation License, Version 3 applies.
%% wxWindows Free Documentation Licence, Version 3, as follows.
%% ===============================================
%%
%% Everyone is permitted to copy and distribute verbatim copies
%% of this licence document, but changing it is not allowed.
%%
%%                  WXWINDOWS FREE DOCUMENTATION LICENCE
%%    TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION
%%
%% 1. Permission is granted to make and distribute verbatim copies of this
%% manual or piece of documentation provided any copyright notice and this
%% permission notice are preserved on all copies.
%%
%% 2. Permission is granted to process this file or document through a
%% document processing system and, at your option and the option of any third
%% party, print the results, provided a printed document carries a copying
%% permission notice identical to this one.
%%
%% 3. Permission is granted to copy and distribute modified versions of this
%% manual or piece of documentation under the conditions for verbatim copying,
%% provided also that any sections describing licensing conditions for this
%% manual, such as, in particular, the GNU General Public Licence, the GNU
%% Library General Public Licence, and any wxWindows Licence are included
%% exactly as in the original, and provided that the entire resulting derived
%% work is distributed under the terms of a permission notice identical to
%% this one.
%%
%% 4. Permission is granted to copy and distribute translations of this manual
%% or piece of documentation into another language, under the above conditions
%% for modified versions, except that sections related to licensing, including
%% this paragraph, may also be included in translations approved by the
%% copyright holders of the respective licence documents in addition to the
%% original English.
%%
%% %CopyrightEnd%
%% This file is generated DO NOT EDIT

-module(wxTaskBarIcon).
-moduledoc """
This class represents a taskbar icon.

A taskbar icon is an icon that appears in the 'system tray' and responds to mouse clicks,
optionally with a tooltip above it to help provide information.

X Window System Note

Under X Window System, the window manager must support either the "System Tray Protocol"
(see [http://freedesktop.org/wiki/Specifications/systemtray-spec](http://freedesktop.org/wiki/Specifications/systemtray-spec))
by freedesktop.org (WMs used by modern desktop environments such as GNOME >= 2, KDE >= 3
and XFCE >= 4 all do) or the older methods used in GNOME 1.2 and KDE 1 and 2.

If it doesn't, the icon will appear as a toplevel window on user's desktop. Because not
all window managers have system tray, there's no guarantee that `m:wxTaskBarIcon` will
work correctly under X Window System and so the applications should use it only as an
optional component of their user interface. The user should be required to explicitly
enable the taskbar icon on Unix, it shouldn't be on by default.

This class is derived, and can use functions, from:

* `m:wxEvtHandler`

wxWidgets docs: [wxTaskBarIcon](https://docs.wxwidgets.org/3.2/classwx_task_bar_icon.html)

## Events

Event types emitted from this class:

* [`taskbar_move`](`m:wxTaskBarIconEvent`)

* [`taskbar_left_down`](`m:wxTaskBarIconEvent`)

* [`taskbar_left_up`](`m:wxTaskBarIconEvent`)

* [`taskbar_right_down`](`m:wxTaskBarIconEvent`)

* [`taskbar_right_up`](`m:wxTaskBarIconEvent`)

* [`taskbar_left_dclick`](`m:wxTaskBarIconEvent`)

* [`taskbar_right_dclick`](`m:wxTaskBarIconEvent`)
""".
-include("wxe.hrl").
-export([ new/0, new/1 ,destroy/1,popupMenu/2,removeIcon/1,setIcon/2,setIcon/3]).

%% inherited exports
-export([connect/2,connect/3,disconnect/1,disconnect/2,disconnect/3,parent_class/1]).

-type wxTaskBarIcon() :: wx:wx_object().
-export_type([wxTaskBarIcon/0]).
-doc false.
parent_class(wxEvtHandler) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).


-spec new() -> wxTaskBarIcon().
new() ->
    new([]).

%% @doc Creates a TaskBarIcon with a callback function for CreatePopupMenu:
%%   <pre>Callback() -> term()</pre>
%%
-spec new([Option]) -> wxTaskBarIcon() when
      Option :: {'iconType', wx:wx_enum()} |
                {'createPopupMenu', fun(() -> wxMenu:wxMenu())}.

new(Options) when is_list(Options) ->
    Op = ?wxTaskBarIcon_new,
    MOpts = fun({iconType, _iconType} = Arg) -> Arg;
               ({createPopupMenu, Fun}) when is_function(Fun) -> {createPopupMenu,  wxe_util:get_cbId(Fun)};
               (BadOpt) -> erlang:error({badoption, BadOpt}) end,
    Opts = lists:map(MOpts, Options),
    wxe_util:queue_cmd(Opts,?get_env(), Op),
    wxe_util:rec(Op).

-doc """
Pops up a menu at the current mouse position.

The events can be handled by a class derived from `m:wxTaskBarIcon`.

Note: It is recommended to override `CreatePopupMenu()` (not implemented in wx) callback
instead of calling this method from event handler, because some ports (e.g. wxCocoa) may
not implement `popupMenu/2` and mouse click events at all.
""".
-spec popupMenu(This, Menu) -> boolean() when
	This::wxTaskBarIcon(), Menu::wxMenu:wxMenu().
popupMenu(#wx_ref{type=ThisT}=This,#wx_ref{type=MenuT}=Menu) ->
  ?CLASS(ThisT,wxTaskBarIcon),
  ?CLASS(MenuT,wxMenu),
  wxe_util:queue_cmd(This,Menu,?get_env(),?wxTaskBarIcon_PopupMenu),
  wxe_util:rec(?wxTaskBarIcon_PopupMenu).

-doc "Removes the icon previously set with `setIcon/3`.".
-spec removeIcon(This) -> boolean() when
	This::wxTaskBarIcon().
removeIcon(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTaskBarIcon),
  wxe_util:queue_cmd(This,?get_env(),?wxTaskBarIcon_RemoveIcon),
  wxe_util:rec(?wxTaskBarIcon_RemoveIcon).

-doc(#{equiv => setIcon(This,Icon, [])}).
-spec setIcon(This, Icon) -> boolean() when
	This::wxTaskBarIcon(), Icon::wxIcon:wxIcon().

setIcon(This,Icon)
 when is_record(This, wx_ref),is_record(Icon, wx_ref) ->
  setIcon(This,Icon, []).

-doc "Sets the icon, and optional tooltip text.".
-spec setIcon(This, Icon, [Option]) -> boolean() when
	This::wxTaskBarIcon(), Icon::wxIcon:wxIcon(),
	Option :: {'tooltip', unicode:chardata()}.
setIcon(#wx_ref{type=ThisT}=This,#wx_ref{type=IconT}=Icon, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxTaskBarIcon),
  ?CLASS(IconT,wxIcon),
  MOpts = fun({tooltip, Tooltip}) ->   Tooltip_UC = unicode:characters_to_binary(Tooltip),{tooltip,Tooltip_UC};
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Icon, Opts,?get_env(),?wxTaskBarIcon_SetIcon),
  wxe_util:rec(?wxTaskBarIcon_SetIcon).

-doc "Destroys the object".
-spec destroy(This::wxTaskBarIcon()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxTaskBarIcon),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
 %% From wxEvtHandler
-doc false.
disconnect(This,EventType, Options) -> wxEvtHandler:disconnect(This,EventType, Options).
-doc false.
disconnect(This,EventType) -> wxEvtHandler:disconnect(This,EventType).
-doc false.
disconnect(This) -> wxEvtHandler:disconnect(This).
-doc false.
connect(This,EventType, Options) -> wxEvtHandler:connect(This,EventType, Options).
-doc false.
connect(This,EventType) -> wxEvtHandler:connect(This,EventType).
