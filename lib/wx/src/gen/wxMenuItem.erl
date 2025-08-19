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

-module(wxMenuItem).
-moduledoc """
A menu item represents an item in a menu.

Note that you usually don't have to deal with it directly as `m:wxMenu` methods usually
construct an object of this class for you.

Also please note that the methods related to fonts and bitmaps are currently only
implemented for Windows, Mac and GTK+.

See:
* `m:wxMenuBar`

* `m:wxMenu`

wxWidgets docs: [wxMenuItem](https://docs.wxwidgets.org/3.2/classwx_menu_item.html)

## Events

Event types emitted from this class:

* [`menu_open`](`m:wxMenuEvent`)

* [`menu_close`](`m:wxMenuEvent`)

* [`menu_highlight`](`m:wxMenuEvent`)
""".
-include("wxe.hrl").
-export([check/1,check/2,destroy/1,enable/1,enable/2,getBitmap/1,getHelp/1,getId/1,
  getItemLabel/1,getItemLabelText/1,getKind/1,getLabel/1,getLabelFromText/1,
  getLabelText/1,getMenu/1,getSubMenu/1,getText/1,isCheckable/1,isChecked/1,
  isEnabled/1,isSeparator/1,isSubMenu/1,new/0,new/1,setBitmap/2,setHelp/2,
  setItemLabel/2,setMenu/2,setSubMenu/2,setText/2]).

%% inherited exports
-export([parent_class/1]).

-type wxMenuItem() :: wx:wx_object().
-export_type([wxMenuItem/0]).
-doc false.
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc(#{equiv => new([])}).
-spec new() -> wxMenuItem().

new() ->
  new([]).

-doc """
Constructs a `m:wxMenuItem` object.

Menu items can be standard, or "stock menu items", or custom. For the standard menu items
(such as commands to open a file, exit the program and so on, see page_stockitems for the
full list) it is enough to specify just the stock ID and leave `text` and `help` string
empty. Some platforms (currently wxGTK only, and see the remark in `setBitmap/2` documentation) will
also show standard bitmaps for stock menu items.

Leaving at least `text` empty for the stock menu items is actually strongly recommended
as they will have appearance and keyboard interface (including standard accelerators)
familiar to the user.

For the custom (non-stock) menu items, `text` must be specified and while `help` string
may be left empty, it's recommended to pass the item description (which is automatically
shown by the library in the status bar when the menu item is selected) in this parameter.

Finally note that you can e.g. use a stock menu label without using its stock help string:

that is, stock properties are set independently one from the other.
""".
%%  Kind = ?wxITEM_SEPARATOR | ?wxITEM_NORMAL | ?wxITEM_CHECK | ?wxITEM_RADIO | ?wxITEM_DROPDOWN | ?wxITEM_MAX
-spec new([Option]) -> wxMenuItem() when
	Option :: {'parentMenu', wxMenu:wxMenu()}
		 | {'id', integer()}
		 | {'text', unicode:chardata()}
		 | {'help', unicode:chardata()}
		 | {'kind', wx:wx_enum()}
		 | {'subMenu', wxMenu:wxMenu()}.
new(Options)
 when is_list(Options) ->
  MOpts = fun({parentMenu, #wx_ref{type=ParentMenuT}} = Arg) ->   ?CLASS(ParentMenuT,wxMenu),Arg;
          ({id, _id} = Arg) -> Arg;
          ({text, Text}) ->   Text_UC = unicode:characters_to_binary(Text),{text,Text_UC};
          ({help, Help}) ->   Help_UC = unicode:characters_to_binary(Help),{help,Help_UC};
          ({kind, _kind} = Arg) -> Arg;
          ({subMenu, #wx_ref{type=SubMenuT}} = Arg) ->   ?CLASS(SubMenuT,wxMenu),Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Opts,?get_env(),?wxMenuItem_new),
  wxe_util:rec(?wxMenuItem_new).

-doc(#{equiv => check(This, [])}).
-spec check(This) -> 'ok' when
	This::wxMenuItem().

check(This)
 when is_record(This, wx_ref) ->
  check(This, []).

-doc """
Checks or unchecks the menu item.

Note that this only works when the item is already appended to a menu.
""".
-spec check(This, [Option]) -> 'ok' when
	This::wxMenuItem(),
	Option :: {'check', boolean()}.
check(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxMenuItem),
  MOpts = fun({check, _check} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxMenuItem_Check).

-doc(#{equiv => enable(This, [])}).
-spec enable(This) -> 'ok' when
	This::wxMenuItem().

enable(This)
 when is_record(This, wx_ref) ->
  enable(This, []).

-doc "Enables or disables the menu item.".
-spec enable(This, [Option]) -> 'ok' when
	This::wxMenuItem(),
	Option :: {'enable', boolean()}.
enable(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxMenuItem),
  MOpts = fun({enable, _enable} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxMenuItem_Enable).

-doc """
Returns the checked or unchecked bitmap.

Only for:wxmsw
""".
-spec getBitmap(This) -> wxBitmap:wxBitmap() when
	This::wxMenuItem().
getBitmap(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMenuItem),
  wxe_util:queue_cmd(This,?get_env(),?wxMenuItem_GetBitmap),
  wxe_util:rec(?wxMenuItem_GetBitmap).

-doc "Returns the help string associated with the menu item.".
-spec getHelp(This) -> unicode:charlist() when
	This::wxMenuItem().
getHelp(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMenuItem),
  wxe_util:queue_cmd(This,?get_env(),?wxMenuItem_GetHelp),
  wxe_util:rec(?wxMenuItem_GetHelp).

-doc "Returns the menu item identifier.".
-spec getId(This) -> integer() when
	This::wxMenuItem().
getId(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMenuItem),
  wxe_util:queue_cmd(This,?get_env(),?wxMenuItem_GetId),
  wxe_util:rec(?wxMenuItem_GetId).

-doc "Returns the item kind, one of `wxITEM\_SEPARATOR`, `wxITEM\_NORMAL`, `wxITEM\_CHECK` or `wxITEM\_RADIO`.".
%%  Res = ?wxITEM_SEPARATOR | ?wxITEM_NORMAL | ?wxITEM_CHECK | ?wxITEM_RADIO | ?wxITEM_DROPDOWN | ?wxITEM_MAX
-spec getKind(This) -> wx:wx_enum() when
	This::wxMenuItem().
getKind(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMenuItem),
  wxe_util:queue_cmd(This,?get_env(),?wxMenuItem_GetKind),
  wxe_util:rec(?wxMenuItem_GetKind).

-doc "Equivalent to: `getLabelText/1`".
-spec getLabelFromText(Text) -> unicode:charlist() when
	Text::unicode:chardata().

getLabelFromText(Text)
 when ?is_chardata(Text) ->
  getLabelText(Text).

-doc """
Strips all accelerator characters and mnemonics from the given `text`.

For example:

will return just `"Hello"`.

See:
* `getItemLabelText/1`

* `getItemLabel/1`
""".
-spec getLabelText(Text) -> unicode:charlist() when
	Text::unicode:chardata().
getLabelText(Text)
 when ?is_chardata(Text) ->
  Text_UC = unicode:characters_to_binary(Text),
  wxe_util:queue_cmd(Text_UC,?get_env(),?wxMenuItem_GetLabelText),
  wxe_util:rec(?wxMenuItem_GetLabelText).

-doc "Equivalent to: `getItemLabel/1`".
-spec getText(This) -> unicode:charlist() when
	This::wxMenuItem().

getText(This)
 when is_record(This, wx_ref) ->
  getItemLabel(This).

-doc """
Returns the text associated with the menu item including any accelerator characters that
were passed to the constructor or `setItemLabel/2`.

See:
* `getItemLabelText/1`

* `getLabelText/1`
""".
-spec getItemLabel(This) -> unicode:charlist() when
	This::wxMenuItem().
getItemLabel(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMenuItem),
  wxe_util:queue_cmd(This,?get_env(),?wxMenuItem_GetItemLabel),
  wxe_util:rec(?wxMenuItem_GetItemLabel).

-doc "Equivalent to: `getItemLabelText/1`".
-spec getLabel(This) -> unicode:charlist() when
	This::wxMenuItem().

getLabel(This)
 when is_record(This, wx_ref) ->
  getItemLabelText(This).

-doc """
Returns the text associated with the menu item, without any accelerator characters.

See:
* `getItemLabel/1`

* `getLabelText/1`
""".
-spec getItemLabelText(This) -> unicode:charlist() when
	This::wxMenuItem().
getItemLabelText(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMenuItem),
  wxe_util:queue_cmd(This,?get_env(),?wxMenuItem_GetItemLabelText),
  wxe_util:rec(?wxMenuItem_GetItemLabelText).

-doc "Returns the menu this menu item is in, or NULL if this menu item is not attached.".
-spec getMenu(This) -> wxMenu:wxMenu() when
	This::wxMenuItem().
getMenu(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMenuItem),
  wxe_util:queue_cmd(This,?get_env(),?wxMenuItem_GetMenu),
  wxe_util:rec(?wxMenuItem_GetMenu).

-doc "Returns the submenu associated with the menu item, or NULL if there isn't one.".
-spec getSubMenu(This) -> wxMenu:wxMenu() when
	This::wxMenuItem().
getSubMenu(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMenuItem),
  wxe_util:queue_cmd(This,?get_env(),?wxMenuItem_GetSubMenu),
  wxe_util:rec(?wxMenuItem_GetSubMenu).

-doc """
Returns true if the item is checkable.

Notice that the radio buttons are considered to be checkable as well, so this method
returns true for them too. Use `IsCheck()` (not implemented in wx) if you want to test for
the check items only.
""".
-spec isCheckable(This) -> boolean() when
	This::wxMenuItem().
isCheckable(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMenuItem),
  wxe_util:queue_cmd(This,?get_env(),?wxMenuItem_IsCheckable),
  wxe_util:rec(?wxMenuItem_IsCheckable).

-doc "Returns true if the item is checked.".
-spec isChecked(This) -> boolean() when
	This::wxMenuItem().
isChecked(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMenuItem),
  wxe_util:queue_cmd(This,?get_env(),?wxMenuItem_IsChecked),
  wxe_util:rec(?wxMenuItem_IsChecked).

-doc "Returns true if the item is enabled.".
-spec isEnabled(This) -> boolean() when
	This::wxMenuItem().
isEnabled(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMenuItem),
  wxe_util:queue_cmd(This,?get_env(),?wxMenuItem_IsEnabled),
  wxe_util:rec(?wxMenuItem_IsEnabled).

-doc "Returns true if the item is a separator.".
-spec isSeparator(This) -> boolean() when
	This::wxMenuItem().
isSeparator(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMenuItem),
  wxe_util:queue_cmd(This,?get_env(),?wxMenuItem_IsSeparator),
  wxe_util:rec(?wxMenuItem_IsSeparator).

-doc "Returns true if the item is a submenu.".
-spec isSubMenu(This) -> boolean() when
	This::wxMenuItem().
isSubMenu(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMenuItem),
  wxe_util:queue_cmd(This,?get_env(),?wxMenuItem_IsSubMenu),
  wxe_util:rec(?wxMenuItem_IsSubMenu).

-doc """
Sets the bitmap for the menu item.

It is equivalent to wxMenuItem::SetBitmaps(bmp, wxNullBitmap) if `checked` is true
(default value) or SetBitmaps(wxNullBitmap, bmp) otherwise.

`setBitmap/2` must be called before the item is appended to the menu, i.e. appending the item without
a bitmap and setting one later is not guaranteed to work. But the bitmap can be changed or
reset later if it had been set up initially.

Notice that GTK+ uses a global setting called `gtk-menu-images` to determine if the
images should be shown in the menus at all. If it is off (which is the case in e.g. Gnome
2.28 by default), no images will be shown, consistently with the native behaviour.

Only for:wxmsw,wxosx,wxgtk
""".
-spec setBitmap(This, Bmp) -> 'ok' when
	This::wxMenuItem(), Bmp::wxBitmap:wxBitmap().
setBitmap(#wx_ref{type=ThisT}=This,#wx_ref{type=BmpT}=Bmp) ->
  ?CLASS(ThisT,wxMenuItem),
  ?CLASS(BmpT,wxBitmap),
  wxe_util:queue_cmd(This,Bmp,?get_env(),?wxMenuItem_SetBitmap).

-doc "Sets the help string.".
-spec setHelp(This, HelpString) -> 'ok' when
	This::wxMenuItem(), HelpString::unicode:chardata().
setHelp(#wx_ref{type=ThisT}=This,HelpString)
 when ?is_chardata(HelpString) ->
  ?CLASS(ThisT,wxMenuItem),
  HelpString_UC = unicode:characters_to_binary(HelpString),
  wxe_util:queue_cmd(This,HelpString_UC,?get_env(),?wxMenuItem_SetHelp).

-doc "Sets the parent menu which will contain this menu item.".
-spec setMenu(This, Menu) -> 'ok' when
	This::wxMenuItem(), Menu::wxMenu:wxMenu().
setMenu(#wx_ref{type=ThisT}=This,#wx_ref{type=MenuT}=Menu) ->
  ?CLASS(ThisT,wxMenuItem),
  ?CLASS(MenuT,wxMenu),
  wxe_util:queue_cmd(This,Menu,?get_env(),?wxMenuItem_SetMenu).

-doc "Sets the submenu of this menu item.".
-spec setSubMenu(This, Menu) -> 'ok' when
	This::wxMenuItem(), Menu::wxMenu:wxMenu().
setSubMenu(#wx_ref{type=ThisT}=This,#wx_ref{type=MenuT}=Menu) ->
  ?CLASS(ThisT,wxMenuItem),
  ?CLASS(MenuT,wxMenu),
  wxe_util:queue_cmd(This,Menu,?get_env(),?wxMenuItem_SetSubMenu).

-doc "Equivalent to: `setItemLabel/2`".
-spec setText(This, Label) -> 'ok' when
	This::wxMenuItem(), Label::unicode:chardata().

setText(This,Label)
 when is_record(This, wx_ref),?is_chardata(Label) ->
  setItemLabel(This,Label).

-doc """
Sets the label associated with the menu item.

Note that if the ID of this menu item corresponds to a stock ID, then it is not necessary
to specify a label: wxWidgets will automatically use the stock item label associated with
that ID. See the `new/1` for more info.

The label string for the normal menu items (not separators) may include the accelerator
which can be used to activate the menu item from keyboard. An accelerator key can be
specified using the ampersand `&` character. In order to embed an ampersand character in
the menu item text, the ampersand must be doubled.

Optionally you can specify also an accelerator string appending a tab character `\t`
followed by a valid key combination (e.g. `CTRL+V`). Its general syntax is any combination
of `"CTRL"`, `"RAWCTRL"`, `"ALT"` and `"SHIFT"` strings (case doesn't matter) separated by
either `'-'` or `'+'` characters and followed by the accelerator itself. Notice that `CTRL`
corresponds to the "Ctrl" key on most platforms but not under macOS where it is mapped to
"Cmd" key on Mac keyboard. Usually this is exactly what you want in portable code but if
you really need to use the (rarely used for this purpose) "Ctrl" key even under Mac, you
may use `RAWCTRL` to prevent this mapping. Under the other platforms `RAWCTRL` is the same
as plain `CTRL`.

The accelerator may be any alphanumeric character, any function key (from `F1` to `F12`),
any numpad digit key using `KP_` prefix (i.e. from `KP_0` to `KP_9`) or one of the special
strings listed below (again, case doesn't matter) corresponding to the specified key code:

* `Del` or `Delete:` WXK_DELETE

* `Back:` WXK_BACK

* `Ins` or `Insert:` WXK_INSERT

* `Enter` or `Return:` WXK_RETURN

* `PgUp` or `PageUp:` WXK_PAGEUP

* `PgDn` or `PageDown:` WXK_PAGEDOWN

* `Left:` WXK_LEFT

* `Right:` WXK_RIGHT

* `Up:` WXK_UP

* `Down:` WXK_DOWN

* `Home:` WXK_HOME

* `End:` WXK_END

* `Space:` WXK_SPACE

* `Tab:` WXK_TAB

* `Esc` or `Escape:` WXK_ESCAPE

* `Cancel:` WXK_CANCEL

* `Clear:` WXK_CLEAR

* `Menu:` WXK_MENU

* `Pause:` WXK_PAUSE

* `Capital:` WXK_CAPITAL

* `Select:` WXK_SELECT

* `Print:` WXK_PRINT

* `Execute:` WXK_EXECUTE

* `Snapshot:` WXK_SNAPSHOT

* `Help:` WXK_HELP

* `Add:` WXK_ADD

* `Separator:` WXK_SEPARATOR

* `Subtract:` WXK_SUBTRACT

* `Decimal:` WXK_DECIMAL

* `Divide:` WXK_DIVIDE

* `Num_lock:` WXK_NUMLOCK

* `Scroll_lock:` WXK_SCROLL

* `KP_Space:` WXK_NUMPAD_SPACE

* `KP_Tab:` WXK_NUMPAD_TAB

* `KP_Enter:` WXK_NUMPAD_ENTER

* `KP_Home:` WXK_NUMPAD_HOME

* `KP_Left:` WXK_NUMPAD_LEFT

* `KP_Up:` WXK_NUMPAD_UP

* `KP_Right:` WXK_NUMPAD_RIGHT

* `KP_Down:` WXK_NUMPAD_DOWN

* `KP_PageUp:` WXK_NUMPAD_PAGEUP

* `KP_PageDown:` WXK_NUMPAD_PAGEDOWN

* `KP_Prior:` WXK_NUMPAD_PAGEUP

* `KP_Next:` WXK_NUMPAD_PAGEDOWN

* `KP_End:` WXK_NUMPAD_END

* `KP_Begin:` WXK_NUMPAD_BEGIN

* `KP_Insert:` WXK_NUMPAD_INSERT

* `KP_Delete:` WXK_NUMPAD_DELETE

* `KP_Equal:` WXK_NUMPAD_EQUAL

* `KP_Multiply:` WXK_NUMPAD_MULTIPLY

* `KP_Add:` WXK_NUMPAD_ADD

* `KP_Separator:` WXK_NUMPAD_SEPARATOR

* `KP_Subtract:` WXK_NUMPAD_SUBTRACT

* `KP_Decimal:` WXK_NUMPAD_DECIMAL

* `KP_Divide:` WXK_NUMPAD_DIVIDE

* `Windows_Left:` WXK_WINDOWS_LEFT

* `Windows_Right:` WXK_WINDOWS_RIGHT

* `Windows_Menu:` WXK_WINDOWS_MENU

* `Command:` WXK_COMMAND

Examples:

Note: In wxGTK using `"SHIFT"` with non-alphabetic characters currently doesn't work,
even in combination with other modifiers, due to GTK+ limitation. E.g. `Shift+Ctrl+A`
works but `Shift+Ctrl+1` or `Shift+/` do not, so avoid using accelerators of this form in
portable code.

Note: In wxGTk, the left/right/up/down arrow keys do not work as accelerator keys for a
menu item unless a modifier key is used. Additionally, the following keycodes are not
supported as menu accelerator keys:

* WXK_COMMAND/WXK_CONTROL

* WXK_SHIFT

* WXK_ALT

* WXK_SCROLL

* WXK_CAPITAL

* WXK_NUMLOCK

* WXK_NUMPAD_TAB

* WXK_TAB

* WXK_WINDOWS_LEFT

* WXK_WINDOWS_RIGHT

* WXK_ADD

* WXK_SEPARATOR

* WXK_SUBTRACT

* WXK_DECIMAL

* WXK_DIVIDE

* WXK_SNAPSHOT

See:
* `getItemLabel/1`

* `getItemLabelText/1`
""".
-spec setItemLabel(This, Label) -> 'ok' when
	This::wxMenuItem(), Label::unicode:chardata().
setItemLabel(#wx_ref{type=ThisT}=This,Label)
 when ?is_chardata(Label) ->
  ?CLASS(ThisT,wxMenuItem),
  Label_UC = unicode:characters_to_binary(Label),
  wxe_util:queue_cmd(This,Label_UC,?get_env(),?wxMenuItem_SetItemLabel).

-doc "Destroys the object".
-spec destroy(This::wxMenuItem()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxMenuItem),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
