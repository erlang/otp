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

-module(wxMenuItem).
-moduledoc """
Functions for wxMenuItem class

A menu item represents an item in a menu.

Note that you usually don't have to deal with it directly as `m:wxMenu` methods
usually construct an object of this class for you.

Also please note that the methods related to fonts and bitmaps are currently
only implemented for Windows, Mac and GTK+.

See: `m:wxMenuBar`, `m:wxMenu`

wxWidgets docs:
[wxMenuItem](https://docs.wxwidgets.org/3.1/classwx_menu_item.html)

## Events

Event types emitted from this class: [`menu_open`](`m:wxMenuEvent`),
[`menu_close`](`m:wxMenuEvent`), [`menu_highlight`](`m:wxMenuEvent`)
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
%% @hidden
-doc false.
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @equiv new([])
-spec new() -> wxMenuItem().

new() ->
  new([]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html#wxmenuitemwxmenuitem">external documentation</a>.
%%<br /> Kind = ?wxITEM_SEPARATOR | ?wxITEM_NORMAL | ?wxITEM_CHECK | ?wxITEM_RADIO | ?wxITEM_DROPDOWN | ?wxITEM_MAX
-doc """
Constructs a `m:wxMenuItem` object.

Menu items can be standard, or "stock menu items", or custom. For the standard
menu items (such as commands to open a file, exit the program and so on, see
page_stockitems for the full list) it is enough to specify just the stock ID and
leave `text` and `help` string empty. Some platforms (currently wxGTK only, and
see the remark in `setBitmap/2` documentation) will also show standard bitmaps
for stock menu items.

Leaving at least `text` empty for the stock menu items is actually strongly
recommended as they will have appearance and keyboard interface (including
standard accelerators) familiar to the user.

For the custom (non-stock) menu items, `text` must be specified and while `help`
string may be left empty, it's recommended to pass the item description (which
is automatically shown by the library in the status bar when the menu item is
selected) in this parameter.

Finally note that you can e.g. use a stock menu label without using its stock
help string:

that is, stock properties are set independently one from the other.
""".
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

%% @equiv check(This, [])
-spec check(This) -> 'ok' when
	This::wxMenuItem().

check(This)
 when is_record(This, wx_ref) ->
  check(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html#wxmenuitemcheck">external documentation</a>.
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

%% @equiv enable(This, [])
-spec enable(This) -> 'ok' when
	This::wxMenuItem().

enable(This)
 when is_record(This, wx_ref) ->
  enable(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html#wxmenuitemenable">external documentation</a>.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html#wxmenuitemgetbitmap">external documentation</a>.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html#wxmenuitemgethelp">external documentation</a>.
-doc "Returns the help string associated with the menu item.".
-spec getHelp(This) -> unicode:charlist() when
	This::wxMenuItem().
getHelp(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMenuItem),
  wxe_util:queue_cmd(This,?get_env(),?wxMenuItem_GetHelp),
  wxe_util:rec(?wxMenuItem_GetHelp).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html#wxmenuitemgetid">external documentation</a>.
-doc "Returns the menu item identifier.".
-spec getId(This) -> integer() when
	This::wxMenuItem().
getId(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMenuItem),
  wxe_util:queue_cmd(This,?get_env(),?wxMenuItem_GetId),
  wxe_util:rec(?wxMenuItem_GetId).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html#wxmenuitemgetkind">external documentation</a>.
%%<br /> Res = ?wxITEM_SEPARATOR | ?wxITEM_NORMAL | ?wxITEM_CHECK | ?wxITEM_RADIO | ?wxITEM_DROPDOWN | ?wxITEM_MAX
-doc """
Returns the item kind, one of `wxITEM_SEPARATOR`, `wxITEM_NORMAL`,
`wxITEM_CHECK` or `wxITEM_RADIO`.
""".
-spec getKind(This) -> wx:wx_enum() when
	This::wxMenuItem().
getKind(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMenuItem),
  wxe_util:queue_cmd(This,?get_env(),?wxMenuItem_GetKind),
  wxe_util:rec(?wxMenuItem_GetKind).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html#wxmenuitemgetlabeltext">external documentation</a>.
-doc "See: `getLabelText/1`.".
-spec getLabelFromText(Text) -> unicode:charlist() when
	Text::unicode:chardata().

getLabelFromText(Text)
 when ?is_chardata(Text) ->
  getLabelText(Text).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html#wxmenuitemgetlabeltext">external documentation</a>.
-doc """
Strips all accelerator characters and mnemonics from the given `text`.

For example:

will return just `"Hello"`.

See: `getItemLabelText/1`, `getItemLabel/1`
""".
-spec getLabelText(Text) -> unicode:charlist() when
	Text::unicode:chardata().
getLabelText(Text)
 when ?is_chardata(Text) ->
  Text_UC = unicode:characters_to_binary(Text),
  wxe_util:queue_cmd(Text_UC,?get_env(),?wxMenuItem_GetLabelText),
  wxe_util:rec(?wxMenuItem_GetLabelText).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html#wxmenuitemgetitemlabel">external documentation</a>.
-doc "See: `getItemLabel/1`.".
-spec getText(This) -> unicode:charlist() when
	This::wxMenuItem().

getText(This)
 when is_record(This, wx_ref) ->
  getItemLabel(This).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html#wxmenuitemgetitemlabel">external documentation</a>.
-doc """
Returns the text associated with the menu item including any accelerator
characters that were passed to the constructor or `setItemLabel/2`.

See: `getItemLabelText/1`, `getLabelText/1`
""".
-spec getItemLabel(This) -> unicode:charlist() when
	This::wxMenuItem().
getItemLabel(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMenuItem),
  wxe_util:queue_cmd(This,?get_env(),?wxMenuItem_GetItemLabel),
  wxe_util:rec(?wxMenuItem_GetItemLabel).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html#wxmenuitemgetitemlabeltext">external documentation</a>.
-doc "See: `getItemLabelText/1`.".
-spec getLabel(This) -> unicode:charlist() when
	This::wxMenuItem().

getLabel(This)
 when is_record(This, wx_ref) ->
  getItemLabelText(This).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html#wxmenuitemgetitemlabeltext">external documentation</a>.
-doc """
Returns the text associated with the menu item, without any accelerator
characters.

See: `getItemLabel/1`, `getLabelText/1`
""".
-spec getItemLabelText(This) -> unicode:charlist() when
	This::wxMenuItem().
getItemLabelText(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMenuItem),
  wxe_util:queue_cmd(This,?get_env(),?wxMenuItem_GetItemLabelText),
  wxe_util:rec(?wxMenuItem_GetItemLabelText).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html#wxmenuitemgetmenu">external documentation</a>.
-doc """
Returns the menu this menu item is in, or NULL if this menu item is not
attached.
""".
-spec getMenu(This) -> wxMenu:wxMenu() when
	This::wxMenuItem().
getMenu(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMenuItem),
  wxe_util:queue_cmd(This,?get_env(),?wxMenuItem_GetMenu),
  wxe_util:rec(?wxMenuItem_GetMenu).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html#wxmenuitemgetsubmenu">external documentation</a>.
-doc "Returns the submenu associated with the menu item, or NULL if there isn't one.".
-spec getSubMenu(This) -> wxMenu:wxMenu() when
	This::wxMenuItem().
getSubMenu(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMenuItem),
  wxe_util:queue_cmd(This,?get_env(),?wxMenuItem_GetSubMenu),
  wxe_util:rec(?wxMenuItem_GetSubMenu).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html#wxmenuitemischeckable">external documentation</a>.
-doc """
Returns true if the item is checkable.

Notice that the radio buttons are considered to be checkable as well, so this
method returns true for them too. Use `IsCheck()` (not implemented in wx) if you
want to test for the check items only.
""".
-spec isCheckable(This) -> boolean() when
	This::wxMenuItem().
isCheckable(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMenuItem),
  wxe_util:queue_cmd(This,?get_env(),?wxMenuItem_IsCheckable),
  wxe_util:rec(?wxMenuItem_IsCheckable).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html#wxmenuitemischecked">external documentation</a>.
-doc "Returns true if the item is checked.".
-spec isChecked(This) -> boolean() when
	This::wxMenuItem().
isChecked(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMenuItem),
  wxe_util:queue_cmd(This,?get_env(),?wxMenuItem_IsChecked),
  wxe_util:rec(?wxMenuItem_IsChecked).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html#wxmenuitemisenabled">external documentation</a>.
-doc "Returns true if the item is enabled.".
-spec isEnabled(This) -> boolean() when
	This::wxMenuItem().
isEnabled(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMenuItem),
  wxe_util:queue_cmd(This,?get_env(),?wxMenuItem_IsEnabled),
  wxe_util:rec(?wxMenuItem_IsEnabled).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html#wxmenuitemisseparator">external documentation</a>.
-doc "Returns true if the item is a separator.".
-spec isSeparator(This) -> boolean() when
	This::wxMenuItem().
isSeparator(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMenuItem),
  wxe_util:queue_cmd(This,?get_env(),?wxMenuItem_IsSeparator),
  wxe_util:rec(?wxMenuItem_IsSeparator).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html#wxmenuitemissubmenu">external documentation</a>.
-doc "Returns true if the item is a submenu.".
-spec isSubMenu(This) -> boolean() when
	This::wxMenuItem().
isSubMenu(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMenuItem),
  wxe_util:queue_cmd(This,?get_env(),?wxMenuItem_IsSubMenu),
  wxe_util:rec(?wxMenuItem_IsSubMenu).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html#wxmenuitemsetbitmap">external documentation</a>.
-doc """
Sets the bitmap for the menu item.

It is equivalent to wxMenuItem::SetBitmaps(bmp, wxNullBitmap) if `checked` is
true (default value) or SetBitmaps(wxNullBitmap, bmp) otherwise.

`setBitmap/2` must be called before the item is appended to the menu, i.e.
appending the item without a bitmap and setting one later is not guaranteed to
work. But the bitmap can be changed or reset later if it had been set up
initially.

Notice that GTK+ uses a global setting called `gtk-menu-images` to determine if
the images should be shown in the menus at all. If it is off (which is the case
in e.g. Gnome 2.28 by default), no images will be shown, consistently with the
native behaviour.

Only for:wxmsw,wxosx,wxgtk
""".
-spec setBitmap(This, Bmp) -> 'ok' when
	This::wxMenuItem(), Bmp::wxBitmap:wxBitmap().
setBitmap(#wx_ref{type=ThisT}=This,#wx_ref{type=BmpT}=Bmp) ->
  ?CLASS(ThisT,wxMenuItem),
  ?CLASS(BmpT,wxBitmap),
  wxe_util:queue_cmd(This,Bmp,?get_env(),?wxMenuItem_SetBitmap).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html#wxmenuitemsethelp">external documentation</a>.
-doc "Sets the help string.".
-spec setHelp(This, HelpString) -> 'ok' when
	This::wxMenuItem(), HelpString::unicode:chardata().
setHelp(#wx_ref{type=ThisT}=This,HelpString)
 when ?is_chardata(HelpString) ->
  ?CLASS(ThisT,wxMenuItem),
  HelpString_UC = unicode:characters_to_binary(HelpString),
  wxe_util:queue_cmd(This,HelpString_UC,?get_env(),?wxMenuItem_SetHelp).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html#wxmenuitemsetmenu">external documentation</a>.
-doc "Sets the parent menu which will contain this menu item.".
-spec setMenu(This, Menu) -> 'ok' when
	This::wxMenuItem(), Menu::wxMenu:wxMenu().
setMenu(#wx_ref{type=ThisT}=This,#wx_ref{type=MenuT}=Menu) ->
  ?CLASS(ThisT,wxMenuItem),
  ?CLASS(MenuT,wxMenu),
  wxe_util:queue_cmd(This,Menu,?get_env(),?wxMenuItem_SetMenu).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html#wxmenuitemsetsubmenu">external documentation</a>.
-doc "Sets the submenu of this menu item.".
-spec setSubMenu(This, Menu) -> 'ok' when
	This::wxMenuItem(), Menu::wxMenu:wxMenu().
setSubMenu(#wx_ref{type=ThisT}=This,#wx_ref{type=MenuT}=Menu) ->
  ?CLASS(ThisT,wxMenuItem),
  ?CLASS(MenuT,wxMenu),
  wxe_util:queue_cmd(This,Menu,?get_env(),?wxMenuItem_SetSubMenu).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html#wxmenuitemsetitemlabel">external documentation</a>.
-doc "See: `setItemLabel/2`.".
-spec setText(This, Label) -> 'ok' when
	This::wxMenuItem(), Label::unicode:chardata().

setText(This,Label)
 when is_record(This, wx_ref),?is_chardata(Label) ->
  setItemLabel(This,Label).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html#wxmenuitemsetitemlabel">external documentation</a>.
-doc """
Sets the label associated with the menu item.

Note that if the ID of this menu item corresponds to a stock ID, then it is not
necessary to specify a label: wxWidgets will automatically use the stock item
label associated with that ID. See the `new/1` for more info.

The label string for the normal menu items (not separators) may include the
accelerator which can be used to activate the menu item from keyboard. An
accelerator key can be specified using the ampersand `&` character. In order to
embed an ampersand character in the menu item text, the ampersand must be
doubled.

Optionally you can specify also an accelerator string appending a tab character
`\t` followed by a valid key combination (e.g. `CTRL+V`). Its general syntax is
any combination of `"CTRL"`, `"RAWCTRL"`, `"ALT"` and `"SHIFT"` strings (case
doesn't matter) separated by either `'-'` or `'+'` characters and followed by
the accelerator itself. Notice that `CTRL` corresponds to the "Ctrl" key on most
platforms but not under macOS where it is mapped to "Cmd" key on Mac keyboard.
Usually this is exactly what you want in portable code but if you really need to
use the (rarely used for this purpose) "Ctrl" key even under Mac, you may use
`RAWCTRL` to prevent this mapping. Under the other platforms `RAWCTRL` is the
same as plain `CTRL`.

The accelerator may be any alphanumeric character, any function key (from `F1`
to `F12`), any numpad digit key using `KP_` prefix (i.e. from `KP_0` to `KP_9`)
or one of the special strings listed below (again, case doesn't matter)
corresponding to the specified key code:

Examples:

Note: In wxGTK using `"SHIFT"` with non-alphabetic characters currently doesn't
work, even in combination with other modifiers, due to GTK+ limitation. E.g.
`Shift+Ctrl+A` works but `Shift+Ctrl+1` or `Shift+/` do not, so avoid using
accelerators of this form in portable code.

Note: In wxGTk, the left/right/up/down arrow keys do not work as accelerator
keys for a menu item unless a modifier key is used. Additionally, the following
keycodes are not supported as menu accelerator keys:

See: `getItemLabel/1`, `getItemLabelText/1`
""".
-spec setItemLabel(This, Label) -> 'ok' when
	This::wxMenuItem(), Label::unicode:chardata().
setItemLabel(#wx_ref{type=ThisT}=This,Label)
 when ?is_chardata(Label) ->
  ?CLASS(ThisT,wxMenuItem),
  Label_UC = unicode:characters_to_binary(Label),
  wxe_util:queue_cmd(This,Label_UC,?get_env(),?wxMenuItem_SetItemLabel).

%% @doc Destroys this object, do not use object again
-doc "Destructor.".
-spec destroy(This::wxMenuItem()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxMenuItem),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
